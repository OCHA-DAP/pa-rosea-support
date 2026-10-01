"""Backtest of the KHF RA2 (September 2026) rainfall triggers against IMERG, 1998 to present.

For each trigger as written: every date the observed indicator reached its threshold, how many
years that happened in, the Weibull return period, and how many recorded EM-DAT floods in the
trigger's counties it was reached for. Also the "wettest spot" reading of the 150 mm trigger, and the same thresholds applied to the
county average of each of the eight ASAL counties the KHF paper names.

Input  (data_dir): imerg_ken_adm1_daily.parquet  county means, from extract_imerg_counties.py
                   years/imerg_ken_grid_YYYY.npz  daily grids, same script (wettest spot only)
                   emdat_ken_floods.parquet       EM-DAT Kenya floods (see README)
Output (data_dir/out): summary.csv, activations.csv, overall.csv, floods.csv, wettest_spot.csv,
                       counties.csv, meta.json

Run: python flood/ken/backtest.py [data_dir]
"""
import calendar
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
OUT = DATA / "out"
COOLDOWN = 30     # days after an activation that belong to the same occasion
FLOOD_LEAD = 30   # a flood counts as reached if the threshold is reached from this many days before it starts to its end
MIN_COVER = 0.5   # wettest spot: pixels at least this share inside the county

UPPER_EWASO = ["Isiolo", "Samburu", "Nyeri", "Nyandarua", "Laikipia", "Meru"]
UPPER_TANA = ["Nyeri", "Kirinyaga", "Murang'a", "Embu", "Meru", "Tharaka-Nithi", "Nyandarua"]
# key: (counties averaged with equal weight, window in days, threshold in mm, EM-DAT location names, group)
TRIGGERS = {
    "krcs_mandera": (["Mandera"], 7, 150, ["Mandera"], "krcs_ne"),
    "krcs_wajir": (["Wajir"], 7, 150, ["Wajir"], "krcs_ne"),
    "krcs_marsabit": (["Marsabit"], 7, 150, ["Marsabit"], "krcs_ne"),
    "whh_70": (UPPER_EWASO, 7, 70, ["Isiolo", "Samburu"], "whh"),
    "whh_100": (UPPER_EWASO, 7, 100, ["Isiolo", "Samburu"], None),   # upper end of the 70-100 mm range
    "krcs_garissa": (UPPER_TANA, 1, 40, ["Garissa", "Tana River", "Dadaab"], "krcs_garissa"),
}


# the eight ASAL counties at Severity Level 4 in the KHF paper, and the thresholds in its triggers
COUNTIES = ["Garissa", "Isiolo", "Mandera", "Marsabit", "Samburu", "Tana River", "Turkana", "Wajir"]
THRESHOLDS = [(7, 150), (7, 100), (7, 70), (1, 40)]


def load_rain():
    c = pd.read_parquet(DATA / "imerg_ken_adm1_daily.parquet")
    c["date"] = pd.to_datetime(c["date"])
    w = c.pivot(index="date", columns="ADM1_EN", values="mean_mm").sort_index()
    assert w.index.to_series().diff().dropna().dt.days.eq(1).all(), "gap in the daily record"
    assert not w.isna().any().any(), "missing county-days"
    return w


def load_emdat():
    em = pd.read_parquet(DATA / "emdat_ken_floods.parquet").copy()

    def end(r):
        y = int(r["End Year"]) if pd.notna(r["End Year"]) else int(r["Start Year"])
        if pd.isna(r["End Month"]):  # month unknown: same year -> end of start month, later year -> end of January
            m = int(r["Start Month"]) if y == int(r["Start Year"]) else 1
        else:
            m = int(r["End Month"])
        d = int(r["End Day"]) if pd.notna(r["End Day"]) and pd.notna(r["End Month"]) else calendar.monthrange(y, m)[1]
        return pd.Timestamp(y, m, d)

    em["start"] = [pd.Timestamp(int(r["Start Year"]), int(r["Start Month"]),
                                int(r["Start Day"]) if pd.notna(r["Start Day"]) else 1) for _, r in em.iterrows()]
    em["end"] = [end(r) for _, r in em.iterrows()]
    em["loc"] = em["Location"].fillna("").str.lower().str.replace(" ", "")
    return em


def activations(s, thr):
    """First day at or above thr; later days within COOLDOWN of that day are the same occasion."""
    out, start = [], None
    for d, v in s[s >= thr].items():
        if start is None or (d - start).days > COOLDOWN:
            out.append(dict(date=d, peak_mm=float(v)))
            start = d
        else:
            out[-1]["peak_mm"] = max(out[-1]["peak_mm"], float(v))
    return out


def return_period(k, n):
    return round((n + 1) / k, 1) if k else None


def wettest_spot(last_full):
    """Years in which the 7-day total at the wettest pixel of each county reached 150 mm."""
    sys.path.insert(0, str(Path(__file__).parent))
    from extract_imerg_counties import grid_and_weights
    files = sorted((DATA / "years").glob("imerg_ken_grid_*.npz"))
    dates, grids = [], []
    for f in files:
        z = np.load(f)
        dates += list(z["dates"])
        grids.append(z["grid"].reshape(len(z["dates"]), -1))
    G = np.concatenate(grids)
    years = np.array([int(d[:4]) for d in dates])
    _, _, _, _, C, codes, names = grid_and_weights(str(dates[-1]))
    code = {v: k for k, v in names.items()}
    rows = []
    for county in ("Mandera", "Wajir", "Marsabit"):
        X = np.nan_to_num(G[:, C[codes.index(code[county])] >= MIN_COVER]).astype("float64")
        cs = np.cumsum(X, axis=0)
        r7 = np.full_like(X, np.nan)
        r7[6:] = cs[6:] - np.vstack([np.zeros((1, X.shape[1])), cs[:-7]])
        daily_max, yr = r7[6:].max(axis=1), years[6:]  # first 6 days have no full 7-day total
        n = sum(daily_max[yr == y].max() >= 150 for y in range(yr.min(), last_full + 1))
        rows.append(dict(county=county, pixels=int(X.shape[1]), years_reached=int(n)))
    return pd.DataFrame(rows)


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    rain, em = load_rain(), load_emdat()
    first_year, last_date = rain.index.min().year, rain.index.max()
    last_full = last_date.year - (0 if (last_date.month, last_date.day) == (12, 31) else 1)
    n = last_full - first_year + 1

    summary, acts, floods, years_by_group = [], [], [], {}
    for key, (counties, win, thr, flood_names, group) in TRIGGERS.items():
        s = rain[counties].mean(axis=1).rolling(win).sum()
        a = activations(s, thr)
        acts += [dict(trigger=key, date=x["date"].date(), peak_mm=round(x["peak_mm"])) for x in a]
        dates = pd.DatetimeIndex([x["date"] for x in a])
        names = [c.lower().replace(" ", "") for c in flood_names]
        ev = em[em["loc"].apply(lambda t: any(k in t for k in names))]
        reached = [bool(((dates >= e.start - pd.Timedelta(days=FLOOD_LEAD)) & (dates <= e.end)).any()) for e in ev.itertuples()]
        floods += [dict(trigger=key, disno=e["DisNo."], start=e["start"].date(), end=e["end"].date(),
                        affected=(None if pd.isna(e["Total Affected"]) else int(e["Total Affected"])), reached=r)
                   for (_, e), r in zip(ev.iterrows(), reached)]
        yrs = sorted({d.year for d in dates if d.year <= last_full})
        if group:
            years_by_group.setdefault(group, set()).update(yrs)
        summary.append(dict(trigger=key, threshold_mm=thr, window_days=win, activations=len(a),
                            years_reached=len(yrs), return_period=return_period(len(yrs), n),
                            floods=len(ev), floods_reached=int(sum(reached)), years=" ".join(map(str, yrs))))

    by_county = []
    for county in COUNTIES:
        ev = em[em["loc"].str.contains(county.lower().replace(" ", ""), regex=False)]
        for win, thr in THRESHOLDS:
            dates = pd.DatetimeIndex([x["date"] for x in activations(rain[county].rolling(win).sum(), thr)])
            yrs = {d.year for d in dates if d.year <= last_full}
            reached = sum(((dates >= e.start - pd.Timedelta(days=FLOOD_LEAD)) & (dates <= e.end)).any() for e in ev.itertuples())
            by_county.append(dict(county=county, window_days=win, threshold_mm=thr, years_reached=len(yrs),
                                  return_period=return_period(len(yrs), n), floods=len(ev), floods_reached=int(reached)))
    # consistency: where a county row and a trigger are the same indicator, they must agree
    bc = pd.DataFrame(by_county).set_index(["county", "window_days", "threshold_mm"])
    for r in summary:
        c = TRIGGERS[r["trigger"]][0]
        if len(c) == 1:
            assert bc.loc[(c[0], r["window_days"], r["threshold_mm"]), "years_reached"] == r["years_reached"]

    overall = []
    for g, members in [("krcs_ne", ["krcs_ne"]), ("all", ["krcs_ne", "whh", "krcs_garissa"])]:
        yrs = sorted(set().union(*[years_by_group[m] for m in members]))
        overall.append(dict(group=g, years_reached=len(yrs), return_period=return_period(len(yrs), n),
                            years=" ".join(map(str, yrs))))
    # overall frequency can never be lower than any member's
    smin = min(r["return_period"] for r in summary if r["return_period"])
    assert overall[1]["return_period"] <= smin + 1e-9

    pd.DataFrame(summary).to_csv(OUT / "summary.csv", index=False)
    pd.DataFrame(acts).to_csv(OUT / "activations.csv", index=False)
    pd.DataFrame(overall).to_csv(OUT / "overall.csv", index=False)
    pd.DataFrame(floods).to_csv(OUT / "floods.csv", index=False)
    wettest_spot(last_full).to_csv(OUT / "wettest_spot.csv", index=False)
    pd.DataFrame(by_county).to_csv(OUT / "counties.csv", index=False)
    (OUT / "meta.json").write_text(json.dumps(dict(
        first_date=str(rain.index.min().date()), last_date=str(last_date.date()), first_year=first_year,
        last_full_year=last_full, n_years=n, emdat_last=str(em["end"].max().date()))))
    print(pd.DataFrame(summary).drop(columns="years").to_string(index=False))
    print(pd.DataFrame(overall).to_string(index=False))
    print(bc["years_reached"].unstack(["window_days", "threshold_mm"]).to_string())


if __name__ == "__main__":
    main()

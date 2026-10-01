"""Backtest of the KHF RA2 (September 2026) rainfall triggers against IMERG, October to December
seasons 1998 to present (the allocation targets the October to December 2026 rains).

For each trigger as written: every date in October to December the observed indicator reached its
threshold, how many seasons that happened in, the Weibull return period, and how many recorded
EM-DAT floods that started in October to December in the trigger's counties it was reached for. Each is computed two ways: the rainfall averaged over the
area ("county") and the wettest single grid cell in the area ("cell"). The same thresholds are also
applied to each of the eight ASAL counties the KHF paper names.

Input  (data_dir): imerg_ken_adm1_daily.parquet  county means, from extract_imerg_counties.py
                   years/imerg_ken_grid_YYYY.npz  daily grids, same script (single-cell reading)
                   emdat_ken_floods.parquet       EM-DAT Kenya floods (see README)
Output (data_dir/out): summary.csv, activations.csv, overall.csv, floods.csv, counties.csv, meta.json
                       (every table has a "reading" column: county or cell)

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
MIN_COVER = 0.5   # single-cell reading: grid cells at least this share inside the area
SEASON = (10, 11, 12)  # October to December: a total counts when its last day falls in these months

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
    return em[em["start"].dt.month.isin(SEASON)]  # floods that started in October to December


def activations(s, thr):
    """First day in the season at or above thr; later days within COOLDOWN of that day are the same occasion."""
    s = s[s.index.month.isin(SEASON)]
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


class Cells:
    """Single-cell reading: the wettest grid cell (at least MIN_COVER inside the area) per day."""

    def __init__(self, index):
        sys.path.insert(0, str(Path(__file__).parent))
        from extract_imerg_counties import grid_and_weights
        dates, grids = [], []
        for f in sorted((DATA / "years").glob("imerg_ken_grid_*.npz")):
            z = np.load(f)
            dates += list(z["dates"])
            grids.append(z["grid"].reshape(len(z["dates"]), -1))
        self.index = pd.DatetimeIndex(pd.to_datetime(dates))
        assert self.index.equals(index), "grids and county file cover different days"
        G = np.nan_to_num(np.concatenate(grids)).astype("float32")
        cs = np.cumsum(G, axis=0, dtype="float64")
        r7 = np.full(G.shape, np.nan, dtype="float32")
        r7[6:] = (cs[6:] - np.vstack([np.zeros((1, G.shape[1])), cs[:-7]])).astype("float32")
        del cs
        self.run = {1: G, 7: r7}
        _, _, _, _, self.C, codes, names = grid_and_weights(dates[-1])
        self.row = {names[c]: i for i, c in enumerate(codes)}

    def series(self, counties, win):
        inside = self.C[[self.row[c] for c in counties]].sum(0) >= MIN_COVER
        return pd.Series(self.run[win][:, inside].max(axis=1), index=self.index)


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    rain, em = load_rain(), load_emdat()
    first_year, last_date = rain.index.min().year, rain.index.max()
    last_full = last_date.year - (0 if (last_date.month, last_date.day) == (12, 31) else 1)
    n = last_full - first_year + 1
    cells = Cells(rain.index)
    readings = {
        "county": lambda counties, win: rain[counties].mean(axis=1).rolling(win).sum(),  # area average
        "cell": lambda counties, win: cells.series(counties, win),                      # wettest single cell
    }

    def evaluate(series, thr, ev):
        a = activations(series, thr)
        dates = pd.DatetimeIndex([x["date"] for x in a])
        reached = [bool(((dates >= e.start - pd.Timedelta(days=FLOOD_LEAD)) & (dates <= e.end)).any()) for e in ev.itertuples()]
        yrs = sorted({d.year for d in dates if d.year <= last_full})
        return a, reached, yrs

    summary, acts, floods, overall, by_county = [], [], [], [], []
    for reading, make in readings.items():
        years_by_group = {}
        for key, (counties, win, thr, flood_names, group) in TRIGGERS.items():
            names = [c.lower().replace(" ", "") for c in flood_names]
            ev = em[em["loc"].apply(lambda t: any(k in t for k in names))]
            a, reached, yrs = evaluate(make(counties, win), thr, ev)
            acts += [dict(reading=reading, trigger=key, date=x["date"].date(), peak_mm=round(x["peak_mm"])) for x in a]
            floods += [dict(reading=reading, trigger=key, disno=e["DisNo."], start=e["start"].date(), end=e["end"].date(),
                            affected=(None if pd.isna(e["Total Affected"]) else int(e["Total Affected"])), reached=r)
                       for (_, e), r in zip(ev.iterrows(), reached)]
            if group:
                years_by_group.setdefault(group, set()).update(yrs)
            summary.append(dict(reading=reading, trigger=key, threshold_mm=thr, window_days=win, activations=len(a),
                                years_reached=len(yrs), return_period=return_period(len(yrs), n),
                                floods=len(ev), floods_reached=int(sum(reached)), years=" ".join(map(str, yrs))))
        for g, members in [("krcs_ne", ["krcs_ne"]), ("all", ["krcs_ne", "whh", "krcs_garissa"])]:
            yrs = sorted(set().union(*[years_by_group[m] for m in members]))
            overall.append(dict(reading=reading, group=g, years_reached=len(yrs),
                                return_period=return_period(len(yrs), n), years=" ".join(map(str, yrs))))
        for county in COUNTIES:
            ev = em[em["loc"].str.contains(county.lower().replace(" ", ""), regex=False)]
            for win, thr in THRESHOLDS:
                _, reached, yrs = evaluate(make([county], win), thr, ev)
                by_county.append(dict(reading=reading, county=county, window_days=win, threshold_mm=thr,
                                      years_reached=len(yrs), return_period=return_period(len(yrs), n),
                                      floods=len(ev), floods_reached=int(sum(reached))))

    sm, bc, ov = pd.DataFrame(summary), pd.DataFrame(by_county), pd.DataFrame(overall)
    # consistency checks
    for r in sm.itertuples():
        c = TRIGGERS[r.trigger][0]
        if len(c) == 1:  # a single-county trigger and its county-table cell are the same indicator
            q = bc[(bc.reading == r.reading) & (bc.county == c[0]) & (bc.window_days == r.window_days) & (bc.threshold_mm == r.threshold_mm)]
            assert int(q.years_reached.iloc[0]) == r.years_reached, r
    for reading in readings:  # overall frequency can never be lower than any member's
        rps = sm[(sm.reading == reading) & sm.return_period.notna()].return_period
        assert ov[(ov.reading == reading) & (ov.group == "all")].return_period.iloc[0] <= rps.min() + 1e-9
    piv = sm.pivot(index="trigger", columns="reading", values="years_reached")
    assert (piv["cell"] >= piv["county"]).all(), "the wettest cell is always at least as wet as the average"

    sm.to_csv(OUT / "summary.csv", index=False)
    pd.DataFrame(acts).to_csv(OUT / "activations.csv", index=False)
    ov.to_csv(OUT / "overall.csv", index=False)
    pd.DataFrame(floods).to_csv(OUT / "floods.csv", index=False)
    bc.to_csv(OUT / "counties.csv", index=False)
    (OUT / "wettest_spot.csv").unlink(missing_ok=True)  # replaced by the "cell" reading
    (OUT / "meta.json").write_text(json.dumps(dict(
        season="October to December", first_date=str(rain.index.min().date()), last_date=str(last_date.date()),
        first_year=first_year, last_full_year=last_full, n_years=n, emdat_last=str(em["end"].max().date()),
        min_cover=MIN_COVER)))
    print(sm.drop(columns="years").to_string(index=False))
    print(ov.to_string(index=False))
    print(bc.pivot_table(index="county", columns=["reading", "window_days", "threshold_mm"], values="years_reached").to_string())


if __name__ == "__main__":
    main()

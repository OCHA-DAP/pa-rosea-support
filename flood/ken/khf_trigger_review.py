"""Backtest of the rainfall thresholds in the KHF RA2 (Sept 2026) anticipatory
action triggers against IMERG daily rainfall, 1998 to present.

Input : data/years/imerg_ken_grid_*.npz  (clipped daily grids, from extract_imerg_counties.py)
        data/emdat_ken_floods.parquet     (EM-DAT, Kenya floods, from the team blob)
Output: data/out/*.csv tables used by build_review_page.py.

For each trigger area (single county or group of counties) and window (1, 3, 7 days)
three readings of "X mm in the area" are tested:
  mean   : area-weighted mean over the area  >= threshold
  pixel  : wettest 0.1 degree pixel in the area >= threshold (pixels at least half inside the area)
  q25/q50: at least 25% / 50% of the area's surface >= threshold
Return periods are Weibull on annual maxima over full years.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from extract_imerg_counties import MIN_COVER, grid_and_weights  # noqa: E402

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
OUT = DATA / "out"
OUT.mkdir(exist_ok=True)

# ---------------------------------------------------------------- load grids
files = sorted((DATA / "years").glob("imerg_ken_grid_*.npz"))
dates, grids = [], []
for f in files:
    z = np.load(f)
    dates.extend(z["dates"].tolist())
    grids.append(z["grid"])
G = np.concatenate(grids).astype("float32")  # (T, H, W)
dates = pd.to_datetime(pd.Index(dates))
order = np.argsort(dates)
G, dates = G[order], dates[order]
full = pd.date_range(dates.min(), dates.max(), freq="D")
missing = full.difference(dates)
if len(missing):  # reindex to a gap-free daily axis so rolling sums never span a gap silently
    Gf = np.full((len(full),) + G.shape[1:], np.nan, dtype="float32")
    Gf[full.get_indexer(dates)] = G
    G, dates = Gf, full
T, H, Wd = G.shape
P = G.reshape(T, -1)
LAST_FULL_YEAR = dates.max().year - (0 if dates.max().month == 12 and dates.max().day == 31 else 1)
print(f"{T} days {dates.min().date()} to {dates.max().date()}, {len(missing)} missing days, full years to {LAST_FULL_YEAR}")

win, shape, nodata, W, C, codes, names = grid_and_weights(dates.max().strftime("%Y-%m-%d"))
assert shape == (H, Wd), (shape, (H, Wd))
name_to_row = {names[c]: i for i, c in enumerate(codes)}

# ---------------------------------------------------------------- areas as written in the KHF document
AREAS = {
    # Kenya Red Cross Society: "KMSA 7 days rainfall forecast above 150mm"
    "Mandera": ["Mandera"],
    "Wajir": ["Wajir"],
    "Marsabit": ["Marsabit"],
    # Welthungerhilfe: 7-day forecast average 70-100mm or more over Isiolo, Samburu and the upper Ewaso Ng'iro
    "Isiolo": ["Isiolo"],
    "Samburu": ["Samburu"],
    "Ewaso Ng'iro area (6 counties)": ["Isiolo", "Samburu", "Nyeri", "Nyandarua", "Laikipia", "Meru"],
    # KRCS Garissa / Dadaab: Tana basin heavy rainfall (at least 40mm)
    "Garissa": ["Garissa"],
    "Tana River": ["Tana River"],
    "Upper Tana (7 counties)": ["Nyeri", "Kirinyaga", "Murang'a", "Embu", "Meru", "Tharaka-Nithi", "Nyandarua"],
}
area_w, area_in = {}, {}
for a, members in AREAS.items():
    rows_ = [name_to_row[m] for m in members]
    w = W[rows_].sum(0)  # each county sums to 1 -> equal county weighting
    area_w[a] = (w / w.sum()).astype("float32")
    area_in[a] = C[rows_].sum(0) >= MIN_COVER  # pixels at least half inside the area

WINDOWS = (1, 3, 7)
THRESHOLDS = (40, 70, 100, 150, 200)
READINGS = ("mean", "pixel", "q25", "q50")


def rolling_sum(P, k):
    if k == 1:
        return P
    c = np.cumsum(np.nan_to_num(P), axis=0, dtype="float64")
    r = np.full_like(P, np.nan, dtype="float32")
    r[k - 1:] = (c[k - 1:] - np.vstack([np.zeros((1, P.shape[1])), c[:-k]])).astype("float32")
    return r


def season(d):
    m = d.month
    return "OND" if m in (10, 11, 12) else ("MAM" if m in (3, 4, 5) else "other")


def weibull_rp(series, thr):
    g = series.groupby(series.index.year).max().dropna()
    g = g[g.index <= LAST_FULL_YEAR]
    n, k = len(g), int((g >= thr).sum())
    return ((n + 1) / k if k else np.inf), k, n


def thr_for_rp(series, rp):
    g = series.groupby(series.index.year).max().dropna()
    g = g[g.index <= LAST_FULL_YEAR]
    k = max(1, int(round((len(g) + 1) / rp)))
    return float(np.sort(g.values)[::-1][k - 1])


def episodes(series, thr, gap=14):
    ex = series[series >= thr]
    eps = []
    for d, v in ex.items():
        if eps and (d - eps[-1]["last"]).days <= gap:
            eps[-1]["last"] = d
            eps[-1]["peak"] = max(eps[-1]["peak"], float(v))
        else:
            eps.append(dict(first=d, last=d, peak=float(v)))
    return pd.DataFrame(eps)


# ---------------------------------------------------------------- series per area / window / reading
series = {}  # (area, window, reading[, thr]) -> pd.Series
for k in WINDOWS:
    R = rolling_sum(P, k)
    for a, w in area_w.items():
        sel = w > 0
        Rw = R[:, sel]
        ws = w[sel]
        valid = ~np.isnan(Rw)
        s_w = (valid * ws).sum(1)
        mean = np.where(s_w > 0, (np.nan_to_num(Rw) * ws).sum(1) / np.where(s_w > 0, s_w, 1), np.nan)
        series[(a, k, "mean")] = pd.Series(mean, index=dates)
        inside = area_in[a][sel]
        series[(a, k, "pixel")] = pd.Series(np.nanmax(np.where(inside, Rw, np.nan), axis=1), index=dates)
        for thr in THRESHOLDS:
            frac = ((Rw >= thr) * ws).sum(1) / np.where(s_w > 0, s_w, 1)
            series[(a, k, "frac", thr)] = pd.Series(frac, index=dates)
    del R

# ---------------------------------------------------------------- return-period table
rows = []
for a in AREAS:
    for k in WINDOWS:
        for thr in THRESHOLDS:
            for rd in READINGS:
                if rd in ("mean", "pixel"):
                    s = series[(a, k, rd)]
                    rp, n_ex, n = weibull_rp(s, thr)
                else:
                    q = 0.25 if rd == "q25" else 0.5
                    rp, n_ex, n = weibull_rp(series[(a, k, "frac", thr)], q)
                rows.append(dict(area=a, window_days=k, threshold_mm=thr, reading=rd, years_exceeded=n_ex, n_years=n,
                                 return_period_yr=(round(rp, 1) if np.isfinite(rp) else None)))
rp_tab = pd.DataFrame(rows)
rp_tab.to_csv(OUT / "threshold_return_periods.csv", index=False)

# thresholds equivalent to 1-in-3 and 1-in-5 for each area / window / reading
rows = []
for a in AREAS:
    for k in WINDOWS:
        for rd in ("mean", "pixel"):
            s = series[(a, k, rd)]
            rows.append(dict(area=a, window_days=k, reading=rd, thr_1in2_mm=round(thr_for_rp(s, 2)),
                             thr_1in3_mm=round(thr_for_rp(s, 3)), thr_1in5_mm=round(thr_for_rp(s, 5)),
                             thr_1in10_mm=round(thr_for_rp(s, 10))))
pd.DataFrame(rows).to_csv(OUT / "rp_equivalent_thresholds.csv", index=False)

# ---------------------------------------------------------------- EM-DAT impact record, north-east Kenya
em = pd.read_parquet(DATA / "emdat_ken_floods.parquet")
em["start"] = pd.to_datetime(dict(year=em["Start Year"], month=em["Start Month"].fillna(1), day=em["Start Day"].fillna(1)))
em["end"] = pd.to_datetime(dict(year=em["End Year"].fillna(em["Start Year"]),
                                month=em["End Month"].fillna(em["Start Month"]).fillna(12),
                                day=em["End Day"].fillna(28)))
NE = ["Mandera", "Wajir", "Marsabit", "Garissa", "Tana River", "Isiolo", "Samburu", "Dadaab", "Daua", "Meru", "Laikipia"]
em["ne_counties"] = em["Location"].fillna("").apply(
    lambda t: ", ".join(c for c in NE if c.lower().replace(" ", "") in t.lower().replace(" ", "")))
em_ne = em[(em["ne_counties"] != "") & (em["start"] >= dates.min())].copy()
em_ne[["DisNo.", "start", "end", "Total Affected", "Total Deaths", "ne_counties", "Location"]].to_csv(
    OUT / "emdat_ne_kenya_floods.csv", index=False)


def emdat_match(first, last, before=10, after=30):
    hits = em_ne[(em_ne["end"] >= first - pd.Timedelta(days=before)) & (em_ne["start"] <= last + pd.Timedelta(days=after))]
    return "; ".join(f"{r['DisNo.']}" for _, r in hits.iterrows())


# ---------------------------------------------------------------- the document's specific thresholds
SPEC = [
    ("KRCS: Mandera, Wajir, Marsabit", "Mandera", 7, 150),
    ("KRCS: Mandera, Wajir, Marsabit", "Wajir", 7, 150),
    ("KRCS: Mandera, Wajir, Marsabit", "Marsabit", 7, 150),
    ("WHH: Isiolo, Samburu (70 mm)", "Ewaso Ng'iro area (6 counties)", 7, 70),
    ("WHH: Isiolo, Samburu (100 mm)", "Ewaso Ng'iro area (6 counties)", 7, 100),
    ("WHH: Isiolo alone (70 mm)", "Isiolo", 7, 70),
    ("WHH: Samburu alone (70 mm)", "Samburu", 7, 70),
    ("KRCS: Garissa rainfall leg (40 mm)", "Garissa", 1, 40),
    ("KRCS: Garissa rainfall leg (40 mm)", "Tana River", 1, 40),
    ("KRCS: Garissa rainfall leg (40 mm)", "Upper Tana (7 counties)", 1, 40),
]
spec_rows, ep_rows = [], []
for label, a, k, thr in SPEC:
    for rd in ("mean", "pixel"):
        s = series[(a, k, rd)]
        rp, n_ex, n = weibull_rp(s, thr)
        eps = episodes(s, thr)
        n_match = 0
        if len(eps):
            eps["season"] = eps["first"].apply(season)
            eps["emdat"] = [emdat_match(f, l) for f, l in zip(eps["first"], eps["last"])]
            n_match = int((eps["emdat"] != "").sum())
            for c, v in [("trigger", label), ("area", a), ("window_days", k), ("threshold_mm", thr), ("reading", rd)]:
                eps.insert(0, c, v)
            ep_rows.append(eps)
        spec_rows.append(dict(trigger=label, area=a, window_days=k, threshold_mm=thr, reading=rd,
                              years_exceeded=n_ex, n_years=n, return_period_yr=(round(rp, 1) if np.isfinite(rp) else None),
                              episodes=len(eps), episodes_per_year=round(len(eps) / n, 2),
                              episodes_with_emdat_flood=n_match))
pd.DataFrame(spec_rows).to_csv(OUT / "khf_thresholds_backtest.csv", index=False)
(pd.concat(ep_rows) if ep_rows else pd.DataFrame()).to_csv(OUT / "khf_threshold_episodes.csv", index=False)

# ---------------------------------------------------------------- flood events vs rainfall (hit / miss)
rows = []
for _, e in em_ne.iterrows():
    row = dict(disno=e["DisNo."], start=e["start"].date(), end=e["end"].date(),
               affected=e["Total Affected"], deaths=e["Total Deaths"], counties=e["ne_counties"])
    lo, hi = e["start"] - pd.Timedelta(days=10), e["end"]
    for a in AREAS:
        for k in (1, 7):
            for rd in ("mean", "pixel"):
                s = series[(a, k, rd)]
                w = s[(s.index >= lo) & (s.index <= hi)].dropna()
                row[f"{a}|{k}d|{rd}"] = round(float(w.max())) if len(w) else None
    rows.append(row)
pd.DataFrame(rows).to_csv(OUT / "emdat_events_vs_rainfall.csv", index=False)

# ---------------------------------------------------------------- seasonal maxima and daily series for the page
sm = []
for a in AREAS:
    for k in (1, 7):
        for rd in ("mean", "pixel"):
            s = series[(a, k, rd)]
            g = s.groupby([s.index.year.rename("year"), s.index.map(season).rename("season")]).max().rename("max_mm").reset_index()
            g["area"], g["window_days"], g["reading"] = a, k, rd
            sm.append(g)
pd.concat(sm).to_csv(OUT / "seasonal_maxima.csv", index=False)

daily = pd.DataFrame({f"{a}|{k}|{rd}": series[(a, k, rd)] for a in AREAS for k in (1, 7) for rd in ("mean", "pixel")})
daily.index.name = "date"
daily.round(1).to_parquet(OUT / "daily_area_series.parquet")

pd.set_option("display.width", 250)
st = pd.DataFrame(spec_rows)
print(st.to_string(index=False))
print(rp_tab[(rp_tab.window_days == 7) & (rp_tab.reading == "mean") & (rp_tab.threshold_mm.isin([70, 100, 150]))]
      .pivot_table(index="area", columns="threshold_mm", values="return_period_yr"))

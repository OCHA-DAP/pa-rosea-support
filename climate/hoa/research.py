"""Ten research questions on heat, rain, ENSO/IOD and impacts in Ethiopia, Kenya and Somalia, 1981-2025.

Run: python climate/hoa/research.py [data_dir]
(after analyse.py, dry_spells.py, drivers.py, heat_impact.py, compare_chirts.py, extra_data.py, daily_metrics.py)
Writes data_dir/out/rq*.csv, one or more per question, and rq_summary.json with every number the paper quotes.

Shared definitions
- Seasons: March to May (MAM), June to September (JJAS), October to December (OND), over the cells where the season
  brings at least 25% of the annual rain. Main seasons: Kenya and Somalia MAM and OND; Ethiopia MAM, JJAS, OND.
- A failed season: rain in the lowest third of that country's seasons, 1981-2025 (a 1-in-3 rule).
- Afternoon maximum: ERA5, warmest of the 12-16 EAT hourly means, trend removed unless said otherwise.
- Impact records: EM-DAT flood starting in the season; EM-DAT drought starting in the season or 6 months after
  it; CERF drought allocation approved within 6 months after the season (seasons 2006-2025); FAOSTAT cereal
  production (national, tonnes; Ethiopia from 1993), as % departure from a quadratic trend on log production; October
  to December is matched to the next calendar year's production (harvest in January to February).
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr
from scipy import stats
from sklearn.metrics import roc_auc_score

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
NAMES = {"ETH": "Ethiopia", "KEN": "Kenya", "SOM": "Somalia"}
SEASONS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
MAIN = {"ETH": ["MAM", "JJAS", "OND"], "KEN": ["MAM", "OND"], "SOM": ["MAM", "OND"]}
rng = np.random.default_rng(7)
S = {}  # every quoted number


def auc(y, s):
    y, s = np.asarray(y), np.asarray(s)
    ok = ~np.isnan(s) & ~np.isnan(y)
    y, s = y[ok].astype(int), s[ok]
    return np.nan if y.min() == y.max() else float(roc_auc_score(y, s))


def auc_range(y, s, years, n=1000):
    y, s, years = np.asarray(y), np.asarray(s), np.asarray(years)
    ok = ~np.isnan(s) & ~np.isnan(y)
    y, s, years = y[ok].astype(int), s[ok], years[ok]
    rng_ = np.random.default_rng(7)  # same seed every call: the same inputs give the same range in every question
    uy, bs = np.unique(years), []
    for _ in range(n):
        idx = np.concatenate([np.where(years == u)[0] for u in rng_.choice(uy, len(uy))])
        if y[idx].min() != y[idx].max():
            bs.append(roc_auc_score(y[idx], s[idx]))
    return float(np.percentile(bs, 5)), float(np.percentile(bs, 95))


# ------------------------------------------------------------------ shared table
T = pd.read_csv(OUT / "impact_targets.csv")
dm = pd.read_csv(OUT / "daily_metrics.csv")[["iso3", "season", "year", "onset_anom_days", "no_onset_pct", "max3_pct", "max7_pct"]]
sea = pd.read_csv(OUT / "seasons.csv")[["iso3", "season", "year", "flood_affected"]]
T = T.merge(dm, on=["iso3", "season", "year"], how="left").merge(sea, on=["iso3", "season", "year"], how="left")
T["failed"] = T.groupby(["iso3", "season"]).rain_pct.transform(lambda v: v <= v.quantile(1 / 3))
T["big_flood"] = (T.flood_affected >= 100_000).astype(int)
fao = pd.read_csv(OUT / "faostat_cereals.csv")
fao["iso3"] = fao.Area.map({v: k for k, v in NAMES.items()})
fao = fao[fao.Year >= 1981]
fao["prod_dev_pct"] = np.nan
for iso, g in fao.groupby("iso3"):
    # quadratic trend on log production: a straight line leaves Ethiopia's curved growth since 1993 in the departures
    fit = np.polyval(np.polyfit(g.Year, np.log(g.Value), 2), g.Year)
    fao.loc[g.index, "prod_dev_pct"] = (np.exp(np.log(g.Value) - fit) - 1) * 100
fao = fao.rename(columns={"Year": "year"})[["iso3", "year", "prod_dev_pct"]]
# harvest year: March to May and June to September are harvested in the same calendar year, October to December
# mostly in January to February of the next, so October to December seasons take the next year's production
T["fao_year"] = T.year + (T.season == "OND").astype(int)
T = T.merge(fao.rename(columns={"year": "fao_year"}), on=["iso3", "fao_year"], how="left")
T.to_csv(OUT / "rq_table.csv", index=False)
S["fao_lag1"] = {i: float(g.sort_values("year").prod_dev_pct.autocorr(1)) for i, g in fao.groupby("iso3")}

# ------------------------------------------------------------------ Q1: does a hot season make a failed season worse?
rows = []
for iso in NAMES:
    for s in MAIN[iso]:
        g = T[(T.iso3 == iso) & (T.season == s) & T.failed]
        hot = g.tmax_detr_c > g.tmax_detr_c.median()
        for target in ["drought_emdat", "drought_cerf"]:
            h = g.dropna(subset=[target])
            hh = h.tmax_detr_c > g.tmax_detr_c.median()
            rows.append(dict(iso3=iso, season=s, target=target, n_failed=len(h),
                             hot_n=int(hh.sum()), hot_events=int(h[target][hh].sum()),
                             normal_n=int((~hh).sum()), normal_events=int(h[target][~hh].sum())))
        p = g.dropna(subset=["prod_dev_pct"])
        hp = p.tmax_detr_c > g.tmax_detr_c.median()
        rows.append(dict(iso3=iso, season=s, target="faostat", n_failed=len(p), hot_n=int(hp.sum()),
                         hot_mean=float(p.prod_dev_pct[hp].mean()) if hp.any() else np.nan,
                         normal_n=int((~hp).sum()), normal_mean=float(p.prod_dev_pct[~hp].mean()) if (~hp).any() else np.nan))
q1 = pd.DataFrame(rows)
q1.to_csv(OUT / "rq1_hot_dry.csv", index=False)
pool = {}
for target in ["drought_emdat", "drought_cerf"]:
    q = q1[q1.target == target]
    a, b, c, d = q.hot_events.sum(), q.hot_n.sum() - q.hot_events.sum(), q.normal_events.sum(), q.normal_n.sum() - q.normal_events.sum()
    pool[target] = dict(hot_events=int(a), hot_n=int(q.hot_n.sum()), normal_events=int(c), normal_n=int(q.normal_n.sum()),
                        fisher_p=float(stats.fisher_exact([[a, b], [c, d]])[1]))
# continuous version: within failed seasons, production departure against temperature
f1 = T[T.failed & T.prod_dev_pct.notna() & T.apply(lambda r: r.season in MAIN[r.iso3], axis=1)]
pool["faostat_r_failed"] = dict(r=float(stats.pearsonr(f1.tmax_detr_c, f1.prod_dev_pct)[0]),
                                p=float(stats.pearsonr(f1.tmax_detr_c, f1.prod_dev_pct)[1]), n=len(f1))
S["q1"] = pool

# ------------------------------------------------------------------ Q2: where and when is it warming?
cells = pd.read_parquet(OUT / "cells.parquet")
elev = pd.read_csv(OUT / "cell_elevation.csv")
cells = cells.merge(elev[["lat", "lon", "elevation_m"]], on=["lat", "lon"], how="left")
aft = xr.open_dataset(DATA / "era5_t2m_afternoon.nc")["t2m"] - 273.15
aft = aft.rename({"valid_time": "time", "latitude": "lat", "longitude": "lon"}).resample(time="MS").max()
pts = aft.interp(lat=xr.DataArray(cells["lat"].values, dims="cell"), lon=xr.DataArray(cells["lon"].values, dims="cell"))
tt = pd.to_datetime(pts["time"].values)
vals = pts.values  # (months, cells)
yrs = np.arange(1981, 2026)


def trend(series_by_year):
    return float(np.polyfit(yrs, series_by_year, 1)[0] * 10)


# per cell: trend of each calendar month, and of the rainy and the dry months
mon_tr = np.zeros((12, vals.shape[1]))
for m in range(12):
    sel = tt.month == m + 1
    x = vals[sel]
    mon_tr[m] = np.polyfit(yrs, x, 1)[0] * 10
chirps_files = sorted((DATA / "chirps").glob("*.npy"))
# rainy months per cell from the CHIRPS normals on the same cells (cells.parquet holds the wettest month only)
blk_a, _, x0, _, e, y0 = map(float, (DATA / "chirps" / "transform.txt").read_text().split())
months = np.array([int(f.stem[5:]) for f in chirps_files]); years_c = np.array([int(f.stem[:4]) for f in chirps_files])
norm = (years_c >= 1991) & (years_c <= 2020)
stack = np.stack([np.load(f) for f in chirps_files])
ny, nx = stack.shape[1] // 5, stack.shape[2] // 5
blk = np.nanmean(stack[:, :ny * 5, :nx * 5].reshape(len(chirps_files), ny, 5, nx, 5), axis=(2, 4))
blat = y0 + e * 5 * (np.arange(ny) + 0.5); blon = x0 + blk_a * 5 * (np.arange(nx) + 0.5)
ii = np.array([np.abs(blat - la).argmin() for la in cells.lat]); jj = np.array([np.abs(blon - lo).argmin() for lo in cells.lon])
rn_norm = np.stack([blk[norm & (months == m + 1)].mean(0)[ii, jj] for m in range(12)])  # (12, cells)
wet = rn_norm > rn_norm.sum(0) / 12
tr_wet = np.array([mon_tr[wet[:, c], c].mean() for c in range(len(cells))])
tr_dry = np.array([mon_tr[~wet[:, c], c].mean() if (~wet[:, c]).any() else np.nan for c in range(len(cells))])
cells["trend_annual"] = mon_tr.mean(0)
cells["trend_wet"] = tr_wet
cells["trend_dry"] = tr_dry
cells["band"] = pd.cut(cells.elevation_m, [-1000, 500, 1500, 10000], labels=["below 500 m", "500 to 1,500 m", "above 1,500 m"])
rows = []
for iso in NAMES:
    c = cells[cells.iso3 == iso]
    w = c.weight
    for m in range(12):
        rows.append(dict(iso3=iso, kind="month", key=m + 1, trend=float(np.average(mon_tr[m][(cells.iso3 == iso).values], weights=w))))
    for band, cb in c.groupby("band", observed=True):
        rows.append(dict(iso3=iso, kind="band", key=str(band), cells=len(cb), trend=float(np.average(cb.trend_annual, weights=cb.weight)),
                         trend_wet=float(np.average(cb.trend_wet, weights=cb.weight)),
                         trend_dry=float(np.nanmean(cb.trend_dry))))
    rows.append(dict(iso3=iso, kind="all", key="all", cells=len(c), trend=float(np.average(c.trend_annual, weights=w)),
                     trend_wet=float(np.average(c.trend_wet, weights=w)), trend_dry=float(np.nanmean(c.trend_dry))))
q2 = pd.DataFrame(rows)
q2.to_csv(OUT / "rq2_warming.csv", index=False)
cells[["lat", "lon", "iso3", "elevation_m", "band", "trend_annual", "trend_wet", "trend_dry"]].to_csv(OUT / "rq2_cells.csv", index=False)

# ------------------------------------------------------------------ Q3: CHIRTS against ERA5 (from compare_chirts.py)
q3 = pd.read_csv(OUT / "chirts_compare.csv")
q3.to_csv(OUT / "rq3_chirts.csv", index=False)

# ------------------------------------------------------------------ Q4: do two failed seasons in a row matter more?
ORDER = {"ETH": ["MAM", "JJAS", "OND"], "KEN": ["MAM", "OND"], "SOM": ["MAM", "OND"]}
seq = []
for iso, order in ORDER.items():
    g = T[(T.iso3 == iso) & T.season.isin(order)].copy()
    g["pos"] = g.year * 10 + g.season.map({s: i for i, s in enumerate(order)})
    g = g.sort_values("pos").reset_index(drop=True)
    z = g.groupby("season").rain_pct.transform(lambda v: (v - v.mean()) / v.std())
    g["prev_failed"] = g.failed.shift(1).fillna(False).astype(bool)
    g["two_failed"] = g.failed & g.prev_failed
    g["two_score"] = -(z + z.shift(1))  # higher = drier over this and the previous season
    g["one_score"] = -z
    seq.append(g)
Q = pd.concat(seq)
rows = []
for iso in list(NAMES) + ["all"]:
    g = Q if iso == "all" else Q[Q.iso3 == iso]
    for target in ["drought_emdat", "drought_cerf"]:
        h = g.dropna(subset=[target, "two_score"])
        for rule in ["failed", "two_failed"]:
            f, y = h[rule].astype(bool), h[target].astype(bool)
            rows.append(dict(iso3=iso, target=target, rule=rule, n=len(h), events=int(y.sum()), flagged=int(f.sum()),
                             hits=int((f & y).sum()), event_rate_flagged=float(y[f].mean()) if f.any() else np.nan,
                             event_rate_other=float(y[~f].mean())))
        for sc in ["one_score", "two_score"]:
            a = auc(h[target], h[sc]); lo, hi = auc_range(h[target], h[sc], h.year)
            rows.append(dict(iso3=iso, target=target, rule=sc, n=len(h), events=int(h[target].sum()), auc=a, auc_lo=lo, auc_hi=hi))
    h = g.dropna(subset=["prod_dev_pct", "two_score"])
    for rule in ["failed", "two_failed"]:
        f = h[rule].astype(bool)
        rows.append(dict(iso3=iso, target="faostat", rule=rule, n=len(h), flagged=int(f.sum()),
                         prod_flagged=float(h.prod_dev_pct[f].mean()) if f.any() else np.nan, prod_other=float(h.prod_dev_pct[~f].mean())))
# distinct CERF allocations behind the "two failed seasons" hits: overlapping 6-month windows can count one twice
cerf_all = pd.read_parquet(DATA / "cerf_all_raw.parquet")
cerf_all = cerf_all[cerf_all.countryCode.isin(list(NAMES)) & (cerf_all.emergencyTypeName == "Drought")].copy()
cerf_all["date"] = pd.to_datetime(cerf_all.firstProjectApprovedDate)
codes = set()
for r in Q[Q.two_failed & (Q.drought_cerf == 1)].itertuples():
    end = pd.Timestamp(r.year, SEASONS[r.season][-1], 1) + pd.offsets.MonthEnd(0)
    codes |= set(cerf_all[(cerf_all.countryCode == r.iso3) & (cerf_all.date > end) & (cerf_all.date <= end + pd.DateOffset(months=6))].applicationCode)
S["q4_cerf_allocations"] = len(codes)
S["q4_flagged_per_country"] = {i: int(Q[(Q.iso3 == i) & Q.two_failed & Q.drought_cerf.notna()].shape[0]) for i in NAMES}
q4 = pd.DataFrame(rows)
q4.to_csv(OUT / "rq4_two_failed.csv", index=False)
Q.to_csv(OUT / "rq4_sequence.csv", index=False)

# ------------------------------------------------------------------ Q5: onset and dry spells against the season total
rows = []
for iso in NAMES:
    for s in MAIN[iso]:
        g = T[(T.iso3 == iso) & (T.season == s)]
        for target in ["drought_emdat", "drought_cerf"]:
            h = g.dropna(subset=[target])
            if h[target].sum() < 4:
                continue
            for name, sc in [("rain total", -h.rain_pct), ("onset date", h.onset_anom_days), ("longest dry spell", h.longest_days)]:
                a = auc(h[target], sc); lo, hi = auc_range(h[target], sc, h.year)
                rows.append(dict(iso3=iso, season=s, target=target, indicator=name, n=len(h), events=int(h[target].sum()),
                                 auc=a, auc_lo=lo, auc_hi=hi))
        h = g.dropna(subset=["prod_dev_pct"])
        for name, col, sign in [("rain total", "rain_pct", 1), ("onset date", "onset_anom_days", -1), ("longest dry spell", "longest_days", -1)]:
            r, p = stats.pearsonr(h[col], h.prod_dev_pct)
            rows.append(dict(iso3=iso, season=s, target="faostat", indicator=name, n=len(h), r=float(r), p=float(p)))
q5 = pd.DataFrame(rows)
q5.to_csv(OUT / "rq5_onset.csv", index=False)
# how onset relates to the total, for context
S["q5_onset_rain_r"] = {f"{i}-{s}": float(stats.pearsonr(T[(T.iso3 == i) & (T.season == s)].onset_anom_days,
                                                          T[(T.iso3 == i) & (T.season == s)].rain_pct)[0])
                        for i in NAMES for s in MAIN[i]}

# ------------------------------------------------------------------ Q6: do ENSO and IOD before the season show how it will go?
n_ = pd.read_csv(DATA / "nino34_ersst5.txt", sep=r"\s+")
nino = pd.Series(n_.iloc[:, -1].values, index=pd.MultiIndex.from_arrays([n_.YR, n_.MON]))
rows_ = []
for line in (DATA / "dmi_had.txt").read_text().splitlines()[1:]:
    p_ = line.split()
    if len(p_) == 13 and p_[0].isdigit():
        rows_ += [((int(p_[0]), m + 1), float(v)) for m, v in enumerate(p_[1:]) if float(v) > -99]
dmi = pd.Series(dict(rows_))
BEFORE = {"OND": [(0, 8), (0, 9)], "MAM": [(0, 1), (0, 2)], "JJAS": [(0, 4), (0, 5)]}  # (year offset, month): Aug-Sep, Jan-Feb, Apr-May


def idx_mean(series, y, months):
    v = [series.get((y + dy, m), np.nan) for dy, m in months]
    return np.nan if np.any(np.isnan(v)) else float(np.mean(v))


rows, per_year = [], []
for iso in NAMES:
    for s in ["MAM", "JJAS", "OND"]:
        g = T[(T.iso3 == iso) & (T.season == s)].copy()
        if g.empty:
            continue
        g["nino_before"] = [idx_mean(nino, y, BEFORE[s]) for y in g.year]
        g["dmi_before"] = [idx_mean(dmi, y, BEFORE[s]) for y in g.year]
        per_year.append(g[["iso3", "season", "year", "rain_pct", "nino_before", "dmi_before", "nino34", "dmi"]])
        for idx, col_b, col_c in [("Niño3.4", "nino_before", "nino34"), ("DMI", "dmi_before", "dmi")]:
            h = g.dropna(subset=[col_b, col_c])
            rb, pb = stats.pearsonr(h[col_b], h.rain_pct)
            rc, pc = stats.pearsonr(h[col_c], h.rain_pct)
            rt, pt = stats.pearsonr(h[col_b], h.tmax_detr_c)
            thr = 0.5 if idx == "Niño3.4" else 0.4
            pos, neg = h[col_b] >= thr, h[col_b] <= -thr
            rows.append(dict(iso3=iso, season=s, index=idx, n=len(h), r_before=float(rb), p_before=float(pb), r_same=float(rc),
                             r_before_temp=float(rt), pos_n=int(pos.sum()), pos_above=int((h.rain_pct[pos] >= 100).sum()),
                             pos_median=float(h.rain_pct[pos].median()) if pos.any() else np.nan,
                             neg_n=int(neg.sum()), neg_below=int((h.rain_pct[neg] < 100).sum()),
                             neg_median=float(h.rain_pct[neg].median()) if neg.any() else np.nan,
                             pos_years=" ".join(map(str, h.year[pos])), neg_years=" ".join(map(str, h.year[neg]))))
q6 = pd.DataFrame(rows)
q6.to_csv(OUT / "rq6_lead.csv", index=False)
pd.concat(per_year).to_csv(OUT / "rq6_years.csv", index=False)

# ------------------------------------------------------------------ Q7: season total or heavy 3-7 day totals for floods?
rows = []
for iso in NAMES:
    for s in ["MAM", "JJAS", "OND"]:
        g = T[(T.iso3 == iso) & (T.season == s)]
        for target in ["flood", "big_flood"]:
            if g[target].sum() < 4:
                continue
            for name, col in [("season total", "rain_pct"), ("heaviest 3 days", "max3_pct"), ("heaviest 7 days", "max7_pct")]:
                a = auc(g[target], g[col]); lo, hi = auc_range(g[target], g[col], g.year)
                rows.append(dict(iso3=iso, season=s, target=target, indicator=name, n=len(g), events=int(g[target].sum()),
                                 auc=a, auc_lo=lo, auc_hi=hi))
q7 = pd.DataFrame(rows)
q7.to_csv(OUT / "rq7_floods.csv", index=False)
S["q7_named"] = {f"{i}-{y}": T[(T.iso3 == i) & (T.season == "OND") & (T.year == y)][["rain_pct", "max3_pct", "max7_pct"]].round(0).iloc[0].to_dict()
                 for i in NAMES for y in [1997, 2006, 2019, 2023]}

# ------------------------------------------------------------------ Q8: after a wet October to December
mon_c = {}
for iso in NAMES:
    m = (cells.iso3 == iso).values
    w = cells.weight.values[m]
    ser = pd.Series((vals[:, m] * w).sum(1) / w.sum(), index=tt)
    clim = ser["1991":"2020"].groupby(ser["1991":"2020"].index.month).mean()
    a = ser - clim.reindex(ser.index.month).values
    k = np.arange(len(a))
    mon_c[iso] = a - np.polyval(np.polyfit(k, a.values, 1), k)
mon_c = pd.DataFrame(mon_c)
rows = []
for iso in ["ETH", "KEN", "SOM"]:
    o = T[(T.iso3 == iso) & (T.season == "OND")].set_index("year")
    m_ = T[(T.iso3 == iso) & (T.season == "MAM")].set_index("year")
    for y in range(1981, 2025):
        fm = mon_c.loc[f"{y + 1}-02-01":f"{y + 1}-03-01", iso]
        rows.append(dict(iso3=iso, year=y, ond_rain=o.rain_pct.get(y), next_mam_rain=m_.rain_pct.get(y + 1),
                         next_febmar_tmax=float(fm.mean()) if len(fm) == 2 else np.nan,
                         ond_flood=o.flood.get(y), next_mam_drought=m_.drought_emdat.get(y + 1)))
q8 = pd.DataFrame(rows)
q8.to_csv(OUT / "rq8_after_wet.csv", index=False)
q8s = []
for iso, g in q8.groupby("iso3"):
    h = g.dropna(subset=["ond_rain", "next_mam_rain", "next_febmar_tmax"])
    wet_ = h.ond_rain >= 120
    q8s.append(dict(iso3=iso, n=len(h), r_mam=float(stats.pearsonr(h.ond_rain, h.next_mam_rain)[0]),
                    p_mam=float(stats.pearsonr(h.ond_rain, h.next_mam_rain)[1]),
                    r_febmar=float(stats.pearsonr(h.ond_rain, h.next_febmar_tmax)[0]),
                    p_febmar=float(stats.pearsonr(h.ond_rain, h.next_febmar_tmax)[1]),
                    wet_n=int(wet_.sum()), wet_years=" ".join(map(str, h.year[wet_])),
                    wet_next_mam_median=float(h.next_mam_rain[wet_].median()), other_next_mam_median=float(h.next_mam_rain[~wet_].median()),
                    wet_next_mam_below80=int((h.next_mam_rain[wet_] < 80).sum()),
                    wet_next_febmar=float(h.next_febmar_tmax[wet_].mean()), other_next_febmar=float(h.next_febmar_tmax[~wet_].mean())))
pd.DataFrame(q8s).to_csv(OUT / "rq8_summary.csv", index=False)

# ------------------------------------------------------------------ Q9: which impact record lines up with the climate record?
rows = []
for iso in NAMES:
    for s in MAIN[iso]:
        g = T[(T.iso3 == iso) & (T.season == s)]
        for target, label in [("drought_emdat", "EM-DAT drought start"), ("drought_cerf", "CERF drought allocation")]:
            h = g.dropna(subset=[target])
            if h[target].sum() < 4:
                rows.append(dict(iso3=iso, season=s, record=label, n=len(h), events=int(h[target].sum()), measure="AUC", value=np.nan))
                continue
            sc = -h.rain_pct
            a = auc(h[target], sc); lo, hi = auc_range(h[target], sc, h.year)
            rows.append(dict(iso3=iso, season=s, record=label, n=len(h), events=int(h[target].sum()), measure="AUC", value=a, lo=lo, hi=hi))
        h = g.dropna(subset=["prod_dev_pct"])
        r, p = stats.pearsonr(h.rain_pct, h.prod_dev_pct)
        # AUC of rain for the worst third of production years, to put FAOSTAT on the same scale as the event records
        worst = (h.prod_dev_pct <= h.prod_dev_pct.quantile(1 / 3)).astype(int)
        a = auc(worst, -h.rain_pct); lo, hi = auc_range(worst, -h.rain_pct, h.year)
        rows.append(dict(iso3=iso, season=s, record="FAOSTAT cereals, worst third", n=len(h), events=int(worst.sum()), measure="AUC",
                         value=a, lo=lo, hi=hi, r=float(r), p=float(p)))
q9 = pd.DataFrame(rows)
q9.to_csv(OUT / "rq9_records.csv", index=False)
S["q9_record_rates"] = {f"{i}": dict(emdat=int(T[T.iso3 == i].drought_emdat.sum()), cerf=int(T[T.iso3 == i].drought_cerf.sum()),
                                     cerf_seasons=int(T[T.iso3 == i].drought_cerf.notna().sum()),
                                     fao_years=int(fao[fao.iso3 == i].shape[0])) for i in NAMES}

# ------------------------------------------------------------------ Q10: has the ENSO and IOD link changed?
dr = pd.read_csv(OUT / "drivers_seasons.csv")
rows = []
for iso in NAMES:
    for s in ["MAM", "JJAS", "OND"]:
        g = dr[(dr.iso3 == iso) & (dr.season == s)].dropna(subset=["nino34", "dmi"])
        if g.empty:
            continue
        for idx in ["nino34", "dmi"]:
            a_, b_ = g[g.year <= 2002], g[g.year >= 2003]
            r1, r2 = stats.pearsonr(a_[idx], a_.rain_pct)[0], stats.pearsonr(b_[idx], b_.rain_pct)[0]
            z = (np.arctanh(r2) - np.arctanh(r1)) / np.sqrt(1 / (len(a_) - 3) + 1 / (len(b_) - 3))
            run = []
            for y0 in range(1981, 2026 - 20):
                h = g[g.year.between(y0, y0 + 20)]
                run.append(round(float(stats.pearsonr(h[idx], h.rain_pct)[0]), 3))
            rows.append(dict(iso3=iso, season=s, index=idx, n1=len(a_), n2=len(b_), r_1981_2002=float(r1), r_2003_2025=float(r2),
                             z=float(z), p=float(2 * stats.norm.sf(abs(z))), running_21yr=json.dumps(run)))
q10 = pd.DataFrame(rows)
q10.to_csv(OUT / "rq10_change.csv", index=False)

(OUT / "rq_summary.json").write_text(json.dumps(S, indent=1, default=float))
print("Q1", json.dumps(S["q1"], default=float))
print("Q2\n", q2[q2.kind != "month"].round(2).to_string())
print("Q4\n", q4.round(2).to_string())
print("Q5\n", q5.round(2).to_string())
print("Q6\n", q6.drop(columns=["pos_years", "neg_years"]).round(2).to_string())
print("Q7\n", q7.round(2).to_string())
print("Q8\n", pd.DataFrame(q8s).round(2).to_string())
print("Q9\n", q9.round(2).to_string())
print("Q10\n", q10.drop(columns=["running_21yr"]).round(2).to_string())

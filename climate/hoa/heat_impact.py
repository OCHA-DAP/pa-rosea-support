"""Two questions for Ethiopia, Kenya and Somalia, 1981-2025:
1. Is the afternoon maximum higher in El Nino years, in the same season and in the months after?
2. How well do season rainfall, afternoon maximum and the longest dry spell pick out flood and drought seasons?

Run: python climate/hoa/heat_impact.py [data_dir]   (after analyse.py, dry_spells.py, drivers.py, impacts.py)

Part 1. ENSO years run July to June and are classed by Nino3.4 averaged over November to January (NOAA CPC
ERSST v5): El Nino at +0.5 C or more, La Nina at -0.5 C or less (single-season threshold, as on the page).
For each country (all its cells), the monthly ERA5 afternoon maximum anomaly against 1991-2020, with the
linear trend removed, is averaged over El Nino and La Nina years from July of the onset year to December of
the year after. Writes out/enso_months.csv and out/enso_years.csv.

Part 2. Impact records, one row per country and rainy season:
- flood: an EM-DAT flood started in the season's months
- drought, two records scored separately: an EM-DAT drought started in the season or the 6 months after it
  (EM-DAT droughts with no start month cannot be placed and are left out); a CERF drought allocation (Rapid
  Response or Underfunded, emergency type Drought) was first approved within 6 months after the season ended
  (seasons 2006-2025 only).
Indicators per season: rain % of normal, afternoon maximum with its trend removed, longest dry spell.
Skill per country and season: ROC AUC of each indicator, and of equal-weight sums of standardised indicators
(nothing fitted, so no cross-validation is needed), with a bootstrap 90% range over years; and a 1-in-3 rule
(the worst third of seasons on that score) scored by hit rate and false alarm ratio.
Writes out/impact_targets.csv and out/impact_skill.csv.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr
from sklearn.metrics import roc_auc_score

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
SEASONS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
NAMES = {"ETH": "Ethiopia", "KEN": "Kenya", "SOM": "Somalia"}
rng = np.random.default_rng(1)

# ------------------------------------------------------------------ part 1: El Nino and the afternoon maximum
cells = pd.read_parquet(OUT / "cells.parquet")
aft = xr.open_dataset(DATA / "era5_t2m_afternoon.nc")["t2m"] - 273.15
aft = aft.rename({"valid_time": "time", "latitude": "lat", "longitude": "lon"}).resample(time="MS").max()
pts = aft.interp(lat=xr.DataArray(cells["lat"].values, dims="cell"), lon=xr.DataArray(cells["lon"].values, dims="cell"))
monthly = {}
for iso in NAMES:
    m = (cells["iso3"] == iso).values
    w = cells["weight"].values[m]
    s = pd.Series((pts.values[:, m] * w).sum(1) / w.sum(), index=pd.to_datetime(pts["time"].values))
    clim = s["1991":"2020"].groupby(s["1991":"2020"].index.month).mean()
    a = s - clim.reindex(s.index.month).values
    t = np.arange(len(a))
    monthly[iso] = a - np.polyval(np.polyfit(t, a.values, 1), t)  # trend removed
mon = pd.DataFrame(monthly)

n = pd.read_csv(DATA / "nino34_ersst5.txt", sep=r"\s+")
nino = pd.Series(n.iloc[:, -1].values, index=pd.to_datetime(dict(year=n.YR, month=n.MON, day=1)))
years = []
for y in range(1981, 2025):
    ndj = nino[f"{y}-11-01":f"{y + 1}-01-01"]
    if len(ndj) == 3:
        v = float(ndj.mean())
        years.append(dict(year=y, ndj=round(v, 2), phase="El Niño" if v >= 0.5 else "La Niña" if v <= -0.5 else "neutral"))
yrs = pd.DataFrame(years)
rows = []
for r in yrs.itertuples():
    for k in range(18):  # July of the onset year to December of the year after
        d = pd.Timestamp(r.year, 7, 1) + pd.DateOffset(months=k)
        if d in mon.index:
            rows.append(dict(year=r.year, phase=r.phase, k=k, month=d.strftime("%b"), **mon.loc[d].to_dict()))
em_ = pd.DataFrame(rows)
comp = em_.groupby(["phase", "k", "month"])[list(NAMES)].mean().reset_index()
comp["n_years"] = em_.groupby(["phase", "k"]).size().values
comp.round(3).to_csv(OUT / "enso_months.csv", index=False)
# July-June mean per ENSO year
ann = em_[em_.k < 12].groupby(["year", "phase"])[list(NAMES)].mean().reset_index().merge(yrs[["year", "ndj"]], on="year")
ann.round(3).to_csv(OUT / "enso_years.csv", index=False)

# ------------------------------------------------------------------ part 2: impact records
seas = pd.read_csv(OUT / "drivers_seasons.csv")
em = pd.read_parquet(DATA / "impacts_emdat.parquet")
cerf = pd.read_parquet(DATA / "cerf_all_raw.parquet")
cerf = cerf[cerf.countryCode.isin(list(NAMES)) & (cerf.emergencyTypeName == "Drought")].copy()
cerf["date"] = pd.to_datetime(cerf.firstProjectApprovedDate)
dr = em[(em["Disaster Type"] == "Drought") & em["Start Month"].notna()].copy()
dr["date"] = pd.to_datetime(dict(year=dr["Start Year"].astype(int), month=dr["Start Month"].astype(int), day=1))
no_month = int(((em["Disaster Type"] == "Drought") & em["Start Month"].isna()).sum())

t = []
for r in seas.itertuples():
    ms = SEASONS[r.season]
    start, end = pd.Timestamp(r.year, ms[0], 1), pd.Timestamp(r.year, ms[-1], 1) + pd.offsets.MonthEnd(0)
    em_hit = dr[(dr.ISO == r.iso3) & (dr.date >= start) & (dr.date <= end + pd.DateOffset(months=6))]
    cerf_hit = cerf[(cerf.countryCode == r.iso3) & (cerf.date > end) & (cerf.date <= end + pd.DateOffset(months=6))]
    t.append(dict(iso3=r.iso3, season=r.season, year=r.year, rain_pct=r.rain_pct, tmax_detr_c=r.tmax_detr_c,
                  longest_days=r.longest_days, nino34=r.nino34, dmi=r.dmi, flood=int(r.flood_events > 0),
                  drought_emdat=int(len(em_hit) > 0),
                  drought_cerf=int(len(cerf_hit) > 0) if 2006 <= r.year <= 2025 else np.nan))
T = pd.DataFrame(t)
T.to_csv(OUT / "impact_targets.csv", index=False)

# indicator scores: higher = more likely impact. Combinations add standardised scores with equal weight, so nothing is fitted.
SIGN = {"drought": {"rain_pct": -1, "tmax_detr_c": 1, "longest_days": 1},
        "flood": {"rain_pct": 1, "tmax_detr_c": -1, "longest_days": -1}}
IND = {"rain": ["rain_pct"], "temp": ["tmax_detr_c"], "dry spell": ["longest_days"],
       "rain + temp": ["rain_pct", "tmax_detr_c"], "rain + temp + dry spell": ["rain_pct", "tmax_detr_c", "longest_days"]}


def score(g, cols, hazard):
    return sum(SIGN[hazard][c] * (g[c] - g[c].mean()) / g[c].std() for c in cols)


def auc_ci(y, s, years):
    est = roc_auc_score(y, s)
    uy = np.unique(years)
    bs = []
    for _ in range(1000):
        pick = rng.choice(uy, len(uy))
        idx = np.concatenate([np.where(years == u)[0] for u in pick])
        if y[idx].min() != y[idx].max():
            bs.append(roc_auc_score(y[idx], s[idx]))
    return est, np.percentile(bs, 5), np.percentile(bs, 95)


res = []
for target, hazard in [("flood", "flood"), ("drought_emdat", "drought"), ("drought_cerf", "drought")]:
    for (iso, s_), g in T.groupby(["iso3", "season"]):
        g = g.dropna(subset=[target, "rain_pct", "tmax_detr_c", "longest_days"]).reset_index(drop=True)
        y = g[target].values.astype(int)
        if y.sum() < 4 or y.sum() > len(y) - 4:
            continue  # too few events (or non-events) to score
        for name, cols in IND.items():
            sc = score(g, cols, hazard).values
            a, lo, hi = auc_ci(y, sc, g.year.values)
            top = sc >= np.quantile(sc, 2 / 3)  # the worst third of seasons on this score: a 1-in-3 rule
            hits = int((top & (y == 1)).sum())
            res.append(dict(target=target, iso3=iso, season=s_, indicators=name, n=len(g), events=int(y.sum()),
                            auc=round(a, 3), auc_lo=round(lo, 3), auc_hi=round(hi, 3), flagged=int(top.sum()), hits=hits,
                            hit_rate=round(hits / y.sum(), 3), false_alarm_ratio=round((top.sum() - hits) / top.sum(), 3)))
R = pd.DataFrame(res)
R.to_csv(OUT / "impact_skill.csv", index=False)
print("EM-DAT droughts with no start month (left out):", no_month)
pd.set_option("display.width", 250)
print(R.pivot_table(index=["target", "iso3", "season", "n", "events"], columns="indicators", values="auc").round(2).to_string())

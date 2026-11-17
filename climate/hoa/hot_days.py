"""Very hot days per country and rainy season, Ethiopia, Kenya and Somalia, 1981-2025.

Run: python climate/hoa/hot_days.py [data_dir]   (after download_hourly.py, analyse.py, drivers.py)
Daily afternoon high: ERA5, the warmest of 12:00 to 16:00 East Africa Time each day, interpolated to the page's
0.25 deg cells. A hot day: hotter than 9 in 10 days of the same calendar month at that cell in 1991-2020 (the
cell's 90th percentile for that month), so a normal season has about 1 hot day in 10.
hot_days_detr repeats the count after taking out each cell's linear warming trend over 1981-2025 (fitted on the
daily departures from the cell's monthly means, centred on 2005.5), so seasons from different decades can be
compared by ENSO/IOD phase or by rainfall without the warming deciding the result.
Per season (March to May, June to September, October to December) the number of hot days, averaged over the
same cells as the season charts; also February to March, the hottest time of year, over the whole country.
Writes data_dir/out/hot_days.csv.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
WINDOWS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12], "FM": [2, 3]}

cells = pd.read_parquet(OUT / "cells.parquet")
files = sorted((DATA / "era5_daily").glob("[0-9][0-9][0-9][0-9].nc"))
assert len(files) == 45, f"{len(files)} years of daily data"
d = xr.open_mfdataset(files, combine="by_coords")["tmax"].sortby("lat")
pts = d.interp(lat=xr.DataArray(cells["lat"].values, dims="cell"), lon=xr.DataArray(cells["lon"].values, dims="cell")).load()
t = pd.to_datetime(pts["time"].values)
v = pts.values  # (days, cells)

# each cell's 90th percentile of the daily afternoon high, by calendar month, 1991-2020
norm = (t.year >= 1991) & (t.year <= 2020)
thr = np.stack([np.percentile(v[norm & (t.month == m)], 90, axis=0) for m in range(1, 13)])  # (12, cells)
hot = v > thr[t.month - 1]

# the same count with each cell's linear warming trend taken out
clim = np.stack([v[t.month == m].mean(0) for m in range(1, 13)])
yr = (t.year + (t.dayofyear - 0.5) / 365.25).values - 2005.5
slope = np.polyfit(yr, v - clim[t.month - 1], 1)[0]  # deg C per year, per cell
vd = v - np.outer(yr, slope)
thr_d = np.stack([np.percentile(vd[norm & (t.month == m)], 90, axis=0) for m in range(1, 13)])
hot_d = vd > thr_d[t.month - 1]

recs = []
for y in range(1981, 2026):
    for s, months in WINDOWS.items():
        sel_t = (t.year == y) & np.isin(t.month, months)
        n_days = int(sel_t.sum())
        count = hot[sel_t].sum(0)  # hot days per cell
        count_d = hot_d[sel_t].sum(0)
        hottest = v[sel_t].max(0)
        for iso in ["ETH", "KEN", "SOM"]:
            sel = (cells.iso3.values == iso) & ((cells[f"share_{s}"].values >= 0.25) if s != "FM" else True)
            if sel.sum() < 10:
                continue
            w = cells.weight.values[sel]
            recs.append(dict(iso3=iso, season=s, year=y, days=n_days, cells=int(sel.sum()),
                             hot_days=float(np.average(count[sel], weights=w)),
                             hot_days_detr=float(np.average(count_d[sel], weights=w)),
                             hottest_c=float(np.average(hottest[sel], weights=w))))
H = pd.DataFrame(recs)
H["hot_days_normal"] = H.groupby(["iso3", "season"]).hot_days.transform(lambda s_: s_[H.loc[s_.index, "year"].between(1991, 2020)].mean())
seas = pd.read_csv(OUT / "seasons.csv")[["iso3", "season", "year", "rain_pct", "tmax_anom_c", "flood_events", "drought_events"]]
H = H.merge(seas, on=["iso3", "season", "year"], how="left")
H.round(3).to_csv(OUT / "hot_days.csv", index=False)
pd.set_option("display.width", 200)
g = H.groupby(["iso3", "season"])
print(pd.DataFrame({"normal": g.hot_days_normal.first(), "1981-2000": g.apply(lambda q: q[q.year <= 2000].hot_days.mean()),
                    "2006-2025": g.apply(lambda q: q[q.year >= 2006].hot_days.mean()), "days": g.days.first()}).round(1).to_string())

"""How the afternoon maximum used on the page compares with the ERA5 mean daily maximum.

Run: python climate/hoa/check_tmax.py [data_dir] [year]
Downloads one year of ERA5 daily maximum 2 m temperature (CDS derived daily statistics, UTC+3 days)
for the Horn window and compares its monthly mean with the warmest 12-16 EAT monthly hourly mean.
Writes data_dir/out/tmax_check.json, which the page's method notes report.
"""
import json
import sys
from pathlib import Path

import cdsapi
import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
YEAR = sys.argv[2] if len(sys.argv) > 2 else "2020"
f = DATA / f"era5_tmax_daily_{YEAR}.nc"
if not f.exists():
    cdsapi.Client().retrieve("derived-era5-single-levels-daily-statistics", dict(
        product_type="reanalysis", variable=["2m_temperature"], year=YEAR, month=[f"{m:02d}" for m in range(1, 13)],
        day=[f"{d:02d}" for d in range(1, 32)], daily_statistic="daily_maximum", time_zone="utc+03:00",
        frequency="1_hourly", area=[16, 32, -5, 52]), str(f))

d = xr.open_dataset(f)
tm = d[[v for v in d.data_vars][0]] - 273.15
tm = tm.rename({k: v for k, v in {"valid_time": "time", "latitude": "lat", "longitude": "lon"}.items() if k in tm.dims})
daily_max = tm.resample(time="MS").mean()

a = xr.open_dataset(DATA / "era5_t2m_afternoon.nc")["t2m"] - 273.15
a = a.rename({"valid_time": "time", "latitude": "lat", "longitude": "lon"}).sel(time=YEAR).resample(time="MS").max()

cells = pd.read_parquet(DATA / "out" / "cells.parquet")
land = xr.DataArray(np.zeros((len(a.lat), len(a.lon)), bool), coords=dict(lat=a.lat, lon=a.lon))
for la, lo in zip(cells.lat, cells.lon):  # ERA5 points nearest to the page's land cells
    land.loc[dict(lat=land.lat.sel(lat=la, method="nearest"), lon=land.lon.sel(lon=lo, method="nearest"))] = True
diff = (daily_max - a).where(land)
w = np.cos(np.deg2rad(diff.lat))
by_month = diff.weighted(w).mean(("lat", "lon"))
out = dict(year=int(YEAR), mean_diff_c=round(float(by_month.mean()), 2),
           month_min_c=round(float(by_month.min()), 2), month_max_c=round(float(by_month.max()), 2),
           cell_p05_c=round(float(diff.quantile(0.05)), 2), cell_p95_c=round(float(diff.quantile(0.95)), 2),
           corr_monthly_cells=round(float(xr.corr(daily_max.where(land), a.where(land))), 3))
(DATA / "out" / "tmax_check.json").write_text(json.dumps(out))
print(out)

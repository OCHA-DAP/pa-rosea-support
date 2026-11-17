"""Download ERA5 hourly 2 m temperature for the afternoon hours, 1981-2025, for the Horn window.

Run: python climate/hoa/download_hourly.py [data_dir]
Hours 09-13 UTC (12:00 to 16:00 East Africa Time), every day, 5 S to 16 N, 32 to 52 E, one request per year
(reanalysis-era5-single-levels), four at a time. Each year is reduced to the daily afternoon high (warmest of the
five hours, deg C) and saved as data_dir/era5_daily/YYYY.nc; the hourly file is deleted. Needs ~/.cdsapirc.
"""
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import cdsapi
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "era5_daily"
OUT.mkdir(parents=True, exist_ok=True)


def year(y):
    out = OUT / f"{y}.nc"
    if out.exists():
        return
    tmp = OUT / f"{y}_hourly.nc"
    cdsapi.Client(quiet=True).retrieve("reanalysis-era5-single-levels", dict(
        product_type=["reanalysis"], variable=["2m_temperature"], year=[str(y)],
        month=[f"{m:02d}" for m in range(1, 13)], day=[f"{d:02d}" for d in range(1, 32)],
        time=[f"{h:02d}:00" for h in range(9, 14)], area=[16, 32, -5, 52],
        data_format="netcdf", download_format="unarchived"), str(tmp))
    with xr.open_dataset(tmp) as d:
        t = d["t2m"] - 273.15
        t = t.rename({k: v for k, v in {"valid_time": "time", "latitude": "lat", "longitude": "lon"}.items() if k in t.dims})
        t.resample(time="1D").max().rename("tmax").to_netcdf(out)
    tmp.unlink()
    print(y, flush=True)


with ThreadPoolExecutor(4) as ex:
    list(ex.map(year, range(1981, 2026)))
print("years:", len(list(OUT.glob("[0-9][0-9][0-9][0-9].nc"))))

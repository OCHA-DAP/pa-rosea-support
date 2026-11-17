"""Download CHIRTS-daily v1.0 Tmax (UCSB, Africa 0.25 deg yearly NetCDF) for the Horn window, 1983-2016.

Run: python climate/hoa/download_chirts.py [data_dir]
CHIRTS monthly Tmax is built from station observations and satellite infrared; the daily product is that
monthly record downscaled to days with ERA5 (file metadata), so monthly and season means stay
independent of ERA5 while day-to-day variation does not. Each yearly file
(about 145 MB) is cropped to 5 S to 16 N, 32 to 52 E, reduced to monthly means of daily maximum and
deleted, leaving data_dir/chirts/YYYY.nc. Used by compare_chirts.py.
"""
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "chirts"
OUT.mkdir(parents=True, exist_ok=True)
URL = "https://data.chc.ucsb.edu/products/CHIRTS/daily/v1.0/africa_netcdf_p25/Tmax.{}.nc"
N, W, S, E = 16, 32, -5, 52

for y in range(1983, 2017):
    out = OUT / f"{y}.nc"
    if out.exists():
        continue
    tmp = OUT / f"{y}_africa.nc"
    for attempt in range(8):
        try:
            urllib.request.urlretrieve(URL.format(y), tmp)
            break
        except urllib.error.HTTPError as err:  # UCSB answers 429 when pulled too fast
            if err.code != 429 or attempt == 7:
                raise
            time.sleep(30 * (attempt + 1))
    with xr.open_dataset(tmp) as d:
        v = d[[k for k in d.data_vars][0]]
        lat = "latitude" if "latitude" in v.dims else "lat"
        lon = "longitude" if "longitude" in v.dims else "lon"
        v = v.sel(time=v["time"].dt.year == y)  # each file ends with a padding step dated 1980-01-01
        v = v.sortby(lat).sel({lat: slice(S, N), lon: slice(W, E)})
        v = v.where(v > -100)  # missing values
        v.resample(time="MS").mean().rename("tmax").to_netcdf(out)
    tmp.unlink()
    print(y, flush=True)

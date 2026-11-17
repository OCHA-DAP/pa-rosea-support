"""Download CHIRPS v2.0 daily rainfall (0.25 deg yearly files, UCSB) for the Horn window, 1981-2025.

Run: python climate/hoa/download_daily.py [data_dir]
Each yearly global file (about 80 MB) is cropped to 5 S to 16 N, 32 to 52 E and deleted, leaving
data_dir/chirps_daily/YYYY.nc. Used by dry_spells.py.
"""
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "chirps_daily"
OUT.mkdir(parents=True, exist_ok=True)
URL = "https://data.chc.ucsb.edu/products/CHIRPS/v2.0/global_daily/netcdf/p25/chirps-v2.0.{}.days_p25.nc"
N, W, S, E = 16, 32, -5, 52

for y in range(1981, 2026):
    out = OUT / f"{y}.nc"
    if out.exists():
        continue
    tmp = OUT / f"{y}_global.nc"
    for attempt in range(8):
        try:
            urllib.request.urlretrieve(URL.format(y), tmp)
            break
        except urllib.error.HTTPError as err:  # UCSB answers 429 when pulled too fast
            if err.code != 429 or attempt == 7:
                raise
            time.sleep(30 * (attempt + 1))
    with xr.open_dataset(tmp) as d:
        d["precip"].sel(latitude=slice(S, N), longitude=slice(W, E)).load().to_netcdf(out)
    tmp.unlink()
    print(y, flush=True)

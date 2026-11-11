"""Download the current-season indices and temperature for the short-rains flood timing page (the rain, CHIRPS v3,
comes from download_chirps3.py).

Run: python flood/hoa/download_current.py [data_dir] [year]
Writes to data_dir/current/:
- oni.txt: NOAA CPC Oceanic Nino Index (ERSST v5)
- dmi_oisst_daily.csv: daily Dipole Mode Index from NOAA OISST v2.1 daily anomalies (PSL file,
  anomalies against 1971-2000 OI.v2 SST), 10 S-10 N, 50-70 E minus 10 S-0, 90-110 E, from 1 August
- era5_t2m_daily_{year}.nc: ERA5 daily mean 2 m temperature for the year so far, cropped to the Horn window
  (5 S to 16 N, 32 to 52 E), from
  the team's shared copy on the dev blob (raster container, computed from ARCO-ERA5 hourly by
  ds-heat-exposure and rewritten each month as a month completes; needs DSCI_AZ_BLOB_DEV_SAS)
Files already on disk are kept; delete them to refresh. The index files are always fetched again.
"""
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

import numpy as np
import ocha_stratus as stratus
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
YEAR = int(sys.argv[2]) if len(sys.argv) > 2 else 2026
OUT = DATA / "current"
OUT.mkdir(parents=True, exist_ok=True)
N, W, S, E = 16, 32, -5, 52
OISST = f"https://psl.noaa.gov/thredds/dodsC/Datasets/noaa.oisst.v2.highres/sst.day.anom.{YEAR}.nc"
CPC = {"oni.txt": "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"}


def fetch(url, dest):
    for attempt in range(8):
        try:
            urllib.request.urlretrieve(url, dest)
            return
        except urllib.error.HTTPError as err:  # retry when the server answers 429 (too many requests)
            if err.code != 429 or attempt == 7:
                raise
            time.sleep(30 * (attempt + 1))


# CPC Oceanic Nino Index
for name, url in CPC.items():
    fetch(url, OUT / name)

# DMI from OISST daily anomalies
ds = xr.open_dataset(OISST)
a = ds["anom"].sel(time=slice(f"{YEAR}-08-01", None))


def box(la0, la1, lo0, lo1):
    b = a.sel(lat=slice(la0, la1), lon=slice(lo0, lo1))
    return b.weighted(np.cos(np.deg2rad(b.lat))).mean(("lat", "lon")).load().to_series()


dmi = (box(-10, 10, 50, 70) - box(-10, 0, 90, 110)).rename("dmi")
dmi.to_csv(OUT / "dmi_oisst_daily.csv")
print("DMI to", dmi.index.max().date())

# ERA5 daily mean 2 m temperature (team blob copy), cropped
t2m = OUT / f"era5_t2m_daily_{YEAR}.nc"
if not t2m.exists():
    tmp = OUT / f"era5_t2m_daily_{YEAR}_global.nc"
    blob = f"era5/daily/processed/2m_temperature_daily_mean/era5_daily_2m_temperature_daily_mean_{YEAR}.nc"
    with open(tmp, "wb") as fh:
        stratus.get_container_client(container_name="raster", stage="dev").get_blob_client(blob)             .download_blob(max_concurrency=8).readinto(fh)
    with xr.open_dataset(tmp) as d:
        d["t2m"].sel(latitude=slice(N, S), longitude=slice(W, E)).load().to_netcdf(t2m)
    tmp.unlink()
with xr.open_dataarray(t2m) as d:
    print("ERA5 2 m temperature to", pd.Timestamp(d["valid_time"].values.max()).date())

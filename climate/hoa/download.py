"""Download the gridded inputs for the Horn of Africa review (Ethiopia, Kenya, Somalia).

Run: python climate/hoa/download.py [data_dir]
- ERA5 monthly means (CDS), 1981-2025: 2 m temperature (monthly mean), and 2 m temperature averaged by
  hour of day at 09-13 UTC (12-16 East Africa Time); the warmest of those hours is used as the
  afternoon maximum, because the monthly-means product has no daily maximum
- CHIRPS v2.0 Africa monthly rainfall (UCSB), 1981-2025, cropped to the window as each file arrives
Both cover 5 S to 16 N, 32 to 52 E. Needs a CDS key in ~/.cdsapirc.
"""
import gzip
import sys
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import cdsapi
import numpy as np
import rasterio
from rasterio.windows import from_bounds

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
DATA.mkdir(parents=True, exist_ok=True)
YEARS = [str(y) for y in range(1981, 2026)]
MONTHS = [f"{m:02d}" for m in range(1, 13)]
N, W, S, E = 16, 32, -5, 52


def era5(path, product, times):
    if not path.exists():
        cdsapi.Client().retrieve(
            "reanalysis-era5-single-levels-monthly-means",
            dict(product_type=[product], variable=["2m_temperature"], year=YEARS, month=MONTHS, time=times,
                 area=[N, W, S, E], data_format="netcdf", download_format="unarchived"),
            str(path))


era5(DATA / "era5_t2m_monthly.nc", "monthly_averaged_reanalysis", ["00:00"])
era5(DATA / "era5_t2m_afternoon.nc", "monthly_averaged_reanalysis_by_hour_of_day",
     [f"{h:02d}:00" for h in range(9, 14)])

CHIRPS = "https://data.chc.ucsb.edu/products/CHIRPS/v2.0/africa_monthly/tifs/chirps-v2.0.{}.{}.tif.gz"
cache = DATA / "chirps"
cache.mkdir(exist_ok=True)


def chirps(ym):
    out = cache / f"{ym[0]}-{ym[1]}.npy"
    if out.exists():
        return
    tmp = cache / f"{ym[0]}-{ym[1]}.tif"
    for attempt in range(8):
        try:
            with urllib.request.urlopen(CHIRPS.format(*ym), timeout=300) as r:
                tmp.write_bytes(gzip.decompress(r.read()))
            break
        except urllib.error.HTTPError as err:  # UCSB answers 429 when pulled too fast
            if err.code != 429 or attempt == 7:
                raise
            time.sleep(15 * (attempt + 1))
    with rasterio.open(tmp) as src:
        win = from_bounds(W, S, E, N, src.transform)
        arr = src.read(1, window=win).astype("float32")
        arr[arr < 0] = np.nan  # nodata is -9999
        if not (cache / "transform.txt").exists():
            (cache / "transform.txt").write_text(" ".join(map(str, src.window_transform(win)[:6])))
    np.save(out, arr)
    tmp.unlink()


with ThreadPoolExecutor(2) as ex:
    list(ex.map(chirps, [(y, m) for y in YEARS for m in MONTHS]))
print("CHIRPS months:", len(list(cache.glob("*.npy"))))

"""Download CHIRPS v3.0 rainfall for the short-rains flood timing page, cropped to the Horn window
(5 S to 16 N, 32 to 52 E) and averaged from 0.05 to 0.25 deg (mean of the land pixels in each 5 x 5 block,
so the cells line up with data_dir/out/cells.parquet).

Run: python flood/hoa/download_chirps3.py [data_dir] [first_year last_year | monthly]
(no years: 1981-2026 and then the monthly normals; a year range: those years only; "monthly": normals only)
CHC serves daily CHIRPS v3 as yearly global 0.05 deg NetCDF files (3-4 GB each, stored in blocks of 31 days
x 10 x 30 deg). This reads only the blocks that cover the window, by HTTP range requests, and backs off when
the server answers 429 (too many requests).

Writes
- data_dir/chirps3_daily/YYYY.nc: daily rain (mm), 1-31 January and 1 September-31 December of that year (the
  months the page uses). Final data use the 'rnl' daily disaggregation (each CHIRPS v3 pentad total split
  across its days with ERA5 daily rain; the only daily v3 series that starts in 1981). For last_year, final
  data end at CHC's last final day and the preliminary data follow; CHC makes preliminary daily v3 only
  with the 'sat' (IMERG) disaggregation. Variable 'prelim' flags those days.
- data_dir/chirps3_monthly_9120.nc: per 0.25 deg cell, mean monthly rain 1991-2020 by calendar month, from
  the CHIRPS v3 monthly file (for the share of annual rain that falls in October-December).
Years already on disk are kept; delete a file to fetch it again.
"""
import io
import sys
import time
from collections import OrderedDict
from pathlib import Path

import numpy as np
import pandas as pd
import requests
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
ARGS = sys.argv[2:]
Y0, Y1 = (1981, 2026) if not ARGS or ARGS[0] == "monthly" else (int(ARGS[0]), int(ARGS[1]))
LAST = 2026  # current season: final data, then preliminary
OUT = DATA / "chirps3_daily"
OUT.mkdir(parents=True, exist_ok=True)
N, W, S, E = 16, 32, -5, 52
CHC = "https://data.chc.ucsb.edu/products/CHIRPS/v3.0"
FINAL = CHC + "/daily/final/rnl/netcdf/byYear/chirps-v3.0.rnl.{}.days_p05.nc"
PRELIM = CHC + "/daily/prelim/sat/netcdf/byYear/chirps-v3.0_prelim.sat.{}.days_p05.nc"
MONTHLY = CHC + "/monthly/global/netcdf/chirps-v3.0.monthly.nc"
SESSION = requests.Session()
SESSION.headers["User-Agent"] = "chirps3-window-reader"


class RemoteFile(io.RawIOBase):
    """Read-only, seekable view of a remote file over HTTP range requests, for h5py. Small reads (HDF5
    metadata) go through a block cache; large reads (data chunks) are fetched in one request."""

    def __init__(self, url, block=1 << 18):
        self.url, self.block, self.pos, self.cache = url, block, 0, OrderedDict()
        self.size = int(self._get(0, 0, whole=True).headers["Content-Range"].split("/")[1])

    def _get(self, a, b, whole=False):
        for attempt in range(12):
            try:
                r = SESSION.get(self.url, headers={"Range": f"bytes={a}-{b}"}, timeout=300)
            except requests.RequestException:
                time.sleep(20 * (attempt + 1))
                continue
            if r.status_code == 206:
                time.sleep(0.3)  # stay under the rate limit
                return r if whole else r.content
            if r.status_code in (429, 500, 502, 503, 504):
                time.sleep(min(20 * (attempt + 1), 180))
                continue
            r.raise_for_status()
        raise RuntimeError(f"{self.url} bytes {a}-{b}: gave up after retries")

    def readable(self):
        return True

    def seekable(self):
        return True

    def tell(self):
        return self.pos

    def seek(self, off, whence=0):
        self.pos = {0: off, 1: self.pos + off, 2: self.size + off}[whence]
        return self.pos

    def _blk(self, i):
        if i not in self.cache:
            a = i * self.block
            self.cache[i] = self._get(a, min(a + self.block, self.size) - 1)
            if len(self.cache) > 64:
                self.cache.popitem(last=False)
        return self.cache[i]

    def read(self, n=-1):
        n = self.size - self.pos if n is None or n < 0 else min(n, self.size - self.pos)
        if n <= 0:
            return b""
        if n >= self.block:
            data = self._get(self.pos, self.pos + n - 1)
        else:
            i0, i1 = self.pos // self.block, (self.pos + n - 1) // self.block
            buf = b"".join(self._blk(i) for i in range(i0, i1 + 1))
            off = self.pos - i0 * self.block
            data = buf[off:off + n]
        self.pos += len(data)
        return data

    def readinto(self, b):
        data = self.read(len(b))
        b[:len(data)] = data
        return len(data)


def open_remote(url):
    return xr.open_dataset(RemoteFile(url), engine="h5netcdf", decode_times=True)


def window(da):
    """Crop to the window and average 5 x 5 pixel blocks to 0.25 deg (NaN pixels, i.e. sea, skipped)."""
    lat = da["latitude"].values
    da = da.sel(latitude=slice(S, N) if lat[0] < lat[-1] else slice(N, S), longitude=slice(W, E))
    da = da.sortby("latitude")
    v = da.values.astype("float32")
    v[v < 0] = np.nan
    t, ny, nx = v.shape
    assert ny % 5 == 0 and nx % 5 == 0, (ny, nx)
    with np.errstate(invalid="ignore"):
        b = np.nanmean(v.reshape(t, ny // 5, 5, nx // 5, 5), axis=(2, 4))
    la = da["latitude"].values.reshape(-1, 5).mean(1).round(3)
    lo = da["longitude"].values.reshape(-1, 5).mean(1).round(3)
    return xr.DataArray(b, dims=("time", "latitude", "longitude"),
                        coords=dict(time=da["time"].values, latitude=la, longitude=lo), name="precip")


def year_days(ds, y):
    tt = pd.to_datetime(ds["time"].values)
    keep = ((tt.month == 1) | (tt.month >= 9)) & (tt.year == y)
    return ds["precip"].isel(time=np.where(keep)[0])


for y in ([] if ARGS[:1] == ["monthly"] else range(Y0, Y1 + 1)):
    dest = OUT / f"{y}.nc"
    if dest.exists():
        continue
    t0 = time.time()
    with open_remote(FINAL.format(y)) as ds:
        fin = window(year_days(ds, y))
    fin = fin.dropna("time", how="all")
    parts, flag = [fin], [np.zeros(fin.sizes["time"], bool)]
    if y == LAST:
        last = pd.Timestamp(fin["time"].values.max())
        with open_remote(PRELIM.format(y)) as ds:
            tt = pd.to_datetime(ds["time"].values)
            pre = window(ds["precip"].isel(time=np.where((tt > last) & (tt.month >= 9))[0])).dropna("time", how="all")
        parts.append(pre)
        flag.append(np.ones(pre.sizes["time"], bool))
        print(y, "final to", last.date(), "| preliminary to", pd.Timestamp(pre["time"].values.max()).date())
    out = xr.concat(parts, "time").to_dataset()
    out["prelim"] = ("time", np.concatenate(flag))
    out.to_netcdf(dest)
    print(y, out.sizes["time"], "days", round(time.time() - t0), "s", flush=True)

# 1991-2020 mean monthly rain per cell, from the monthly file
mon = DATA / "chirps3_monthly_9120.nc"
if (not ARGS or ARGS[0] == "monthly") and not mon.exists():
    with open_remote(MONTHLY) as ds:
        da = ds["precip"].sel(time=slice("1991-01-01", "2020-12-31"))
        m = window(da)
    m.groupby("time.month").mean().to_netcdf(mon)
    print("monthly normals written")

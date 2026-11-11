"""CHIRTS against ERA5: does the page's temperature hold with an independent, station-based record?

Run: python climate/hoa/compare_chirts.py [data_dir]   (after download_chirts.py, analyse.py, drivers.py)
CHIRTS-daily v1.0 Tmax (UCSB; monthly level from stations and satellite infrared, days from ERA5), monthly
means of daily maximum, 1983-2016, against the page's ERA5 afternoon maximum on the same 0.25 deg cells.
For each country and season, over the same cells as the season charts and with 1983-2016 as the normal for
both: the level difference, the correlation of season anomalies (trend removed from each), and the page's
temperature links recomputed with each record on the same years (rain, Nino3.4, DMI, phase means).
Writes data_dir/out/chirts_compare.csv.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
SEASONS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}

cells = pd.read_parquet(OUT / "cells.parquet")
drv = pd.read_csv(OUT / "drivers_seasons.csv")

ch = xr.concat([xr.open_dataarray(f) for f in sorted((DATA / "chirts").glob("[0-9][0-9][0-9][0-9].nc"))], "time")
latn = "latitude" if "latitude" in ch.dims else "lat"
lonn = "longitude" if "longitude" in ch.dims else "lon"
ch_pts = ch.sel({latn: xr.DataArray(cells["lat"].values, dims="cell"), lonn: xr.DataArray(cells["lon"].values, dims="cell")},
                method="nearest", tolerance=0.01)
aft = xr.open_dataset(DATA / "era5_t2m_afternoon.nc")["t2m"] - 273.15
aft = aft.rename({"valid_time": "time", "latitude": "lat", "longitude": "lon"}).resample(time="MS").max()
er_pts = aft.interp(lat=xr.DataArray(cells["lat"].values, dims="cell"), lon=xr.DataArray(cells["lon"].values, dims="cell"))
er_pts = er_pts.sel(time=slice("1983-01-01", "2016-12-01"))
ch_t = pd.to_datetime(ch_pts["time"].values)
er_t = pd.to_datetime(er_pts["time"].values)
assert (ch_t == er_t).all() and len(ch_t) == 34 * 12


def detr(v, yrs):
    return v - np.polyval(np.polyfit(yrs, v, 1), yrs)


rows = []
for (iso, s), g in drv.groupby(["iso3", "season"]):
    months = SEASONS[s]
    sel = ((cells["iso3"] == iso) & (cells[f"share_{s}"] >= 0.25)).values
    chv, erv = ch_pts.values[:, sel], er_pts.values[:, sel]
    good = ~np.isnan(chv).any(0)  # cells CHIRTS covers in every month
    w = cells["weight"].values[sel][good]
    yrs = np.arange(1983, 2017)
    mm = np.isin(ch_t.month, months)
    ser = {}
    for name, arr in [("chirts", chv[:, good]), ("era5", erv[:, good])]:
        area = (arr * w).sum(1) / w.sum()
        ser[name] = np.array([area[mm & (ch_t.year == y)].mean() for y in yrs])
    g = g[g.year.between(1983, 2016)].sort_values("year")
    a_ch, a_er = detr(ser["chirts"] - ser["chirts"].mean(), yrs), detr(ser["era5"] - ser["era5"].mean(), yrs)
    rec = dict(iso3=iso, season=s, cells=int(good.sum()), cells_page=int(sel.sum()),
               chirts_mean_c=round(float(ser["chirts"].mean()), 2), era5_mean_c=round(float(ser["era5"].mean()), 2),
               level_diff_c=round(float(ser["chirts"].mean() - ser["era5"].mean()), 2),
               anom_corr=round(float(np.corrcoef(a_ch, a_er)[0, 1]), 3),
               trend_chirts=round(float(np.polyfit(yrs, ser["chirts"], 1)[0] * 10), 3),
               trend_era5=round(float(np.polyfit(yrs, ser["era5"], 1)[0] * 10), 3))
    for name, a in [("chirts", a_ch), ("era5", a_er)]:
        rec[f"r_rain_{name}"] = round(float(np.corrcoef(g.rain_pct.values, a)[0, 1]), 3)
        rec[f"r_nino_{name}"] = round(float(np.corrcoef(g.nino34.values, a)[0, 1]), 3)
        rec[f"r_dmi_{name}"] = round(float(np.corrcoef(g.dmi.values, a)[0, 1]), 3)
        for ph in ["El Niño", "La Niña"]:
            rec[f"{'nino' if ph == 'El Niño' else 'nina'}_{name}"] = round(float(a[(g.enso == ph).values].mean()), 3)
    rec["n_el_nino"] = int((g.enso == "El Niño").sum())
    rec["n_la_nina"] = int((g.enso == "La Niña").sum())
    rows.append(rec)
C = pd.DataFrame(rows)
C.to_csv(OUT / "chirts_compare.csv", index=False)
pd.set_option("display.width", 250)
print(C.to_string())

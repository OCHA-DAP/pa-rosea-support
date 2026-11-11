"""Temperature, rainfall and flood/drought impact years for Ethiopia, Kenya and Somalia.

Run: python climate/hoa/analyse.py [data_dir]   (after download.py and impacts.py)
Tmax here is the ERA5 afternoon maximum: the warmest of the 12-16 EAT monthly hourly means.
Writes data_dir/out/:
- cycle.csv        monthly 1991-2020 normals per country and rainfall regime: rain (CHIRPS), Tmean and Tmax (ERA5)
- cells.parquet    per 0.25 deg cell: wettest month, hottest month, Tmax in the rainy months minus the dry months
- seasons.csv      per country, season and year: rainfall % of normal and Tmax anomaly over the area where that
                   season is a main rainy season, with the EM-DAT droughts whose months overlap the season and the
                   floods that started in it
"""
import sys
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import xarray as xr
from rasterio.features import rasterize
from affine import Affine

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
OUT.mkdir(exist_ok=True)
NORMAL = slice("1991-01-01", "2020-12-31")
SEASONS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
MAIN_SHARE = 0.25  # a season is a main rainy season in a cell if it brings at least this share of the annual total

# ---- CHIRPS (0.05 deg, mm per month) averaged to 0.25 deg blocks; ERA5 interpolated to the block centres
files = sorted((DATA / "chirps").glob("*.npy"))
a, _, x0, _, e, y0 = map(float, (DATA / "chirps" / "transform.txt").read_text().split())
stack = np.stack([np.load(f) for f in files])
ny, nx = (stack.shape[1] // 5) * 5, (stack.shape[2] // 5) * 5
stack = stack[:, :ny, :nx].reshape(len(files), ny // 5, 5, nx // 5, 5)
blocks = np.nanmean(stack, axis=(2, 4))  # cells with no land pixels stay NaN
lat = y0 + e * 5 * (np.arange(ny // 5) + 0.5)
lon = x0 + a * 5 * (np.arange(nx // 5) + 0.5)
times = pd.to_datetime([f.stem + "-01" for f in files])
rain = xr.DataArray(blocks, dims=("time", "lat", "lon"), coords=dict(time=times, lat=lat, lon=lon)).sortby("lat")


def era5(name, var="t2m"):
    t = xr.open_dataset(DATA / name)
    t = t.rename({k: v for k, v in {"valid_time": "time", "latitude": "lat", "longitude": "lon"}.items() if k in t.variables})
    return t[var] - 273.15


t2m = era5("era5_t2m_monthly.nc")
aft = era5("era5_t2m_afternoon.nc")
tmax = aft.resample(time="MS").max()  # warmest of the 12-16 EAT hourly means: the afternoon maximum
t2m["time"] = pd.to_datetime(t2m["time"].values).to_period("M").to_timestamp()
t2m = t2m.interp(lat=rain.lat, lon=rain.lon)
tmax = tmax.interp(lat=rain.lat, lon=rain.lon)
common = np.intersect1d(np.intersect1d(rain.time.values, t2m.time.values), tmax.time.values)
rain, t2m, tmax = rain.sel(time=common), t2m.sel(time=common), tmax.sel(time=common)

# ---- country mask on the ERA5 grid
adm = gpd.read_parquet(DATA / "adm1.parquet")
lat, lon = rain.lat.values, rain.lon.values
res = float(lon[1] - lon[0])
tr = Affine(res, 0, lon[0] - res / 2, 0, res, lat[0] - res / 2)  # rows run south to north
ctry = adm.dissolve("iso3").reset_index()
codes = {i: k + 1 for k, i in enumerate(ctry["iso3"])}
mask = rasterize([(g, codes[i]) for g, i in zip(ctry.geometry, ctry["iso3"])], out_shape=(len(lat), len(lon)),
                 transform=tr, fill=0, all_touched=False)
iso_of = {v: k for k, v in codes.items()}
weight = np.cos(np.deg2rad(lat))[:, None] * np.ones((1, len(lon)))

# ---- per-cell normals and rainfall regime
rn = rain.sel(time=NORMAL).groupby("time.month").mean()           # mm per month
tn = tmax.sel(time=NORMAL).groupby("time.month").mean()
mn = t2m.sel(time=NORMAL).groupby("time.month").mean()
annual = rn.sum("month")
share = {s: rn.sel(month=m).sum("month") / annual for s, m in SEASONS.items()}
wet = rn > annual / 12                                              # rainy month: more than an average month's rain
dTmax = tn.where(wet).mean("month") - tn.where(~wet).mean("month")
regime = xr.where((share["MAM"] >= MAIN_SHARE) & (share["OND"] >= MAIN_SHARE), "bimodal (MAM and OND)",
                  xr.where(share["JJAS"] >= 0.5, "unimodal (JJAS)", "other"))

cells = pd.DataFrame(dict(
    lat=np.repeat(lat, len(lon)), lon=np.tile(lon, len(lat)), iso3=[iso_of.get(v, "") for v in mask.ravel()],
    weight=weight.ravel(), annual_mm=annual.values.ravel(),
    wettest_month=(rn.fillna(-1).argmax("month") + 1).values.ravel(),
    hottest_month=(tn.fillna(-99).argmax("month") + 1).values.ravel(),
    tmax_wet_minus_dry=dTmax.values.ravel(), regime=regime.values.ravel(),
    **{f"share_{s}": v.values.ravel() for s, v in share.items()}))
cells = cells[cells["iso3"] != ""]
cells.to_parquet(OUT / "cells.parquet")

# ---- monthly cycle per country and regime (area-weighted)
rows = []
for iso, g in cells.groupby("iso3"):
    for reg, gg in [("all", g)] + list(g[g["regime"] != "other"].groupby("regime")):
        ii = [(np.searchsorted(lat, a), np.searchsorted(lon, b)) for a, b in zip(gg["lat"], gg["lon"])]
        r_, c_ = map(np.array, zip(*ii))
        w = gg["weight"].values
        for m in range(1, 13):
            rows.append(dict(iso3=iso, regime=reg, month=m, cells=len(gg),
                             rain_mm=np.average(rn.sel(month=m).values[r_, c_], weights=w),
                             tmean_c=np.average(mn.sel(month=m).values[r_, c_], weights=w),
                             tmax_c=np.average(tn.sel(month=m).values[r_, c_], weights=w)))
pd.DataFrame(rows).round(2).to_csv(OUT / "cycle.csv", index=False)

# ---- seasonal anomalies per country over the area where the season is a main rainy season
em = pd.read_parquet(DATA / "impacts_emdat.parquet")
# a drought counts for a season when its start-to-end months overlap the season's months; a missing start
# month is taken as January and a missing end month as December, so those events cover the whole year
drought_start = em["Start Year"] * 12 + em["Start Month"].fillna(1)
drought_end = em["End Year"].fillna(em["Start Year"]) * 12 + em["End Month"].fillna(12)
recs = []
for iso in codes:
    for s, months in SEASONS.items():
        sel = (mask == codes[iso]) & (share[s].values >= MAIN_SHARE)
        if sel.sum() < 10:
            continue
        w = np.where(sel, weight, 0)
        rs = rain.where(rain.time.dt.month.isin(months), drop=True).groupby("time.year").sum()
        ts = tmax.where(tmax.time.dt.month.isin(months), drop=True).groupby("time.year").mean()
        rclim = rs.sel(year=slice(1991, 2020)).mean("year")
        tclim = ts.sel(year=slice(1991, 2020)).mean("year")
        for y in rs.year.values:
            rpct = float(np.nansum(rs.sel(year=y).values * w) / np.nansum(rclim.values * w) * 100)
            tano = float(np.nansum((ts.sel(year=y) - tclim).values * w) / w.sum())
            e = em[(em["ISO"] == iso)]
            fl = e[(e["Disaster Type"] == "Flood") & (e["Start Year"] == y) & e["Start Month"].isin(months)]
            dr = e[(e["Disaster Type"] == "Drought") & (drought_start <= y * 12 + months[-1]) & (drought_end >= y * 12 + months[0])]
            recs.append(dict(iso3=iso, season=s, year=int(y), cells=int(sel.sum()), rain_pct=round(rpct, 1),
                             tmax_anom_c=round(tano, 2),
                             drought_events=len(dr), drought_affected=dr["Total Affected"].sum(), drought_ids=";".join(dr["DisNo."]),
                             flood_events=len(fl), flood_affected=fl["Total Affected"].sum(), flood_ids=";".join(fl["DisNo."])))
pd.DataFrame(recs).to_csv(OUT / "seasons.csv", index=False)
print("cells", len(cells), "| cycle rows", len(rows), "| season rows", len(recs))

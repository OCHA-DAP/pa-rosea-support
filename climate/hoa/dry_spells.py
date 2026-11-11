"""Dry spells inside each rainy season, Ethiopia, Kenya and Somalia, 1981-2025 (CHIRPS daily, 0.25 deg).

Run: python climate/hoa/dry_spells.py [data_dir]   (after download_daily.py and analyse.py)
A dry day has less than 1 mm. A dry spell is SPELL or more dry days in a row. Seasons are fixed calendar
windows (March to May, June to September, October to December), so a late start of the rains counts as a
dry spell at the start of the season. Per cell and season: the longest run of dry days, the number of dry
days, and the number of dry spells; then averaged over the cells of the country where that season brings
at least 25% of the annual rain: the season charts' cells, less the few with missing daily values that year
(the cell count is kept per row). Writes data_dir/out/dryspells.csv.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
DRY_MM, SPELL, MAIN_SHARE = 1.0, 10, 0.25
SEASONS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}

cells = pd.read_parquet(OUT / "cells.parquet")
seas = pd.read_csv(OUT / "seasons.csv")


def spells(dry):
    """dry: (days, cells) bool -> longest run, dry days, number of runs of SPELL+ days, per cell."""
    run = np.zeros(dry.shape[1], int)
    longest = np.zeros_like(run)
    count = np.zeros_like(run)
    for d in dry:
        ended = ~d & (run >= SPELL)
        count += ended
        run = np.where(d, run + 1, 0)
        longest = np.maximum(longest, run)
    count += run >= SPELL  # a spell still running at the end of the season
    return longest, dry.sum(0), count


recs = []
for f in sorted((DATA / "chirps_daily").glob("[0-9][0-9][0-9][0-9].nc")):
    da = xr.open_dataarray(f)
    pick = da.sel(latitude=xr.DataArray(cells["lat"].values, dims="cell"),
                  longitude=xr.DataArray(cells["lon"].values, dims="cell"), method="nearest", tolerance=0.01)
    t = pd.to_datetime(pick["time"].values)
    vals = pick.values
    for s, months in SEASONS.items():
        m = np.isin(t.month, months)
        v = vals[m]
        ok = ~np.isnan(v).any(0)
        longest, ndry, nsp = spells(np.nan_to_num(v, nan=99) < DRY_MM)
        for iso in ["ETH", "KEN", "SOM"]:
            sel = (cells["iso3"].values == iso) & (cells[f"share_{s}"].values >= MAIN_SHARE) & ok
            if sel.sum() < 10:
                continue
            w = cells["weight"].values[sel]
            recs.append(dict(iso3=iso, season=s, year=int(f.stem), cells=int(sel.sum()), season_days=int(m.sum()),
                             longest_days=np.average(longest[sel], weights=w), dry_days=np.average(ndry[sel], weights=w),
                             spells=np.average(nsp[sel], weights=w),
                             share_with_spell=np.average(nsp[sel] > 0, weights=w) * 100,
                             share_longest_30=np.average(longest[sel] >= 30, weights=w) * 100))
ds = pd.DataFrame(recs)
norm = ds[ds.year.between(1991, 2020)].groupby(["iso3", "season"])[["longest_days", "dry_days", "spells"]].mean()
ds = ds.join(norm, on=["iso3", "season"], rsuffix="_normal")
ds["longest_anom_days"] = ds["longest_days"] - ds["longest_days_normal"]
ds = ds.merge(seas[["iso3", "season", "year", "rain_pct", "tmax_anom_c", "drought_events", "flood_events"]], on=["iso3", "season", "year"], how="left")
ds.round(2).to_csv(OUT / "dryspells.csv", index=False)
print(len(ds), "rows,", ds.year.nunique(), "years")

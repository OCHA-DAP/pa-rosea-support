"""Onset of the rains and heavy-rain totals per country and season, from CHIRPS daily (0.25 deg), 1981-2025.

Run: python climate/hoa/daily_metrics.py [data_dir]   (after download_daily.py and analyse.py)
Per cell and season (March to May, June to September, October to December):
- onset: anomalous accumulation (Liebmann et al. 2012; Dunning et al. 2016), which needs no rain threshold, so it
  works in dry and wet areas alike. Over a window from 30 days before the season to its end, the cumulative sum
  of daily rain minus that cell's 1991-2020 mean daily rain for the window; onset is the day after its minimum,
  in days from the season's first day (negative: before it). Where the minimum is the window's last day, the
  rains never rose above the cell's average: counted as no onset and given the season's last day.
- heaviest 3-day and 7-day totals inside the season
Then averaged over the same cells as the season charts (the season brings at least 25% of the annual rain),
less cells with missing daily values. Writes data_dir/out/daily_metrics.csv.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
SEASONS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
LEAD = 30  # days before the season's first day where the onset search starts

cells = pd.read_parquet(OUT / "cells.parquet")


def roll(v, k):
    c = np.cumsum(np.vstack([np.zeros((1, v.shape[1])), v]), 0)
    return c[k:] - c[:-k]  # totals of k consecutive days, starting on each day


def onset_day(v, qbar, lead):
    """v: (days, cells) rain over the window; qbar: (cells,) mean daily rain. Onset index relative to the season start."""
    acc = np.cumsum(v - qbar, 0)
    i = acc.argmin(0)
    found = i < len(v) - 1
    return np.where(found, i + 1 - lead, len(v) - 1 - lead), found


files = sorted((DATA / "chirps_daily").glob("[0-9][0-9][0-9][0-9].nc"))

years = {}
for f in files:  # every year once: cell series and the per-season windows (30 days before the season to its end)
    da = xr.open_dataarray(f)
    pick = da.sel(latitude=xr.DataArray(cells["lat"].values, dims="cell"),
                  longitude=xr.DataArray(cells["lon"].values, dims="cell"), method="nearest", tolerance=0.01)
    years[int(f.stem)] = pd.DataFrame(pick.values, index=pd.to_datetime(pick["time"].values))
allday = pd.concat(years.values()).sort_index()
qbar = {}
for s, months in SEASONS.items():
    wins = []
    for y in range(1991, 2021):
        st = pd.Timestamp(y, months[0], 1)
        en = pd.Timestamp(y, months[-1], 1) + pd.offsets.MonthEnd(0)
        wins.append(allday.loc[st - pd.Timedelta(days=LEAD):en])
    qbar[s] = pd.concat(wins).mean(0).values

recs = []
for y in sorted(years):
    for s, months in SEASONS.items():
        st = pd.Timestamp(y, months[0], 1)
        en = pd.Timestamp(y, months[-1], 1) + pd.offsets.MonthEnd(0)
        win = allday.loc[st - pd.Timedelta(days=LEAD):en].values
        lead = len(allday.loc[st - pd.Timedelta(days=LEAD):st - pd.Timedelta(days=1)])
        v = win[lead:]  # the season itself, for the heavy-rain totals
        ok = ~np.isnan(win).any(0)
        on, found = onset_day(np.nan_to_num(win), qbar[s], lead)
        v = np.nan_to_num(v)
        m3, m7 = roll(v, 3).max(0), roll(v, 7).max(0)
        for iso in ["ETH", "KEN", "SOM"]:
            sel = (cells["iso3"].values == iso) & (cells[f"share_{s}"].values >= 0.25) & ok
            if sel.sum() < 10:
                continue
            w = cells["weight"].values[sel]
            recs.append(dict(iso3=iso, season=s, year=y, cells=int(sel.sum()),
                             onset_day=np.average(on[sel], weights=w), no_onset_pct=np.average(~found[sel], weights=w) * 100,
                             max3_mm=np.average(m3[sel], weights=w), max7_mm=np.average(m7[sel], weights=w)))
d = pd.DataFrame(recs)
norm = d[d.year.between(1991, 2020)].groupby(["iso3", "season"])[["onset_day", "max3_mm", "max7_mm"]].mean()
d = d.join(norm, on=["iso3", "season"], rsuffix="_normal")
d["onset_anom_days"] = d.onset_day - d.onset_day_normal
d["max3_pct"] = d.max3_mm / d.max3_mm_normal * 100
d["max7_pct"] = d.max7_mm / d.max7_mm_normal * 100
d.round(2).to_csv(OUT / "daily_metrics.csv", index=False)
print(d.groupby(["iso3", "season"])[["onset_day", "no_onset_pct", "max3_mm", "max7_mm"]].mean().round(1).to_string())

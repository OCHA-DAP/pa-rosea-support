"""Short-rains flood timing: when EM-DAT floods start in October to December seasons with both an
El Nino and a positive IOD, against other seasons, and where the current season stands.

Run: python flood/hoa/flood_timing.py [data_dir] [year]
Reads, from download_chirps3.py:
  data_dir/chirps3_daily/YYYY.nc     CHIRPS v3.0 daily, 0.25 deg, January and September-December, 1981 to the
                                     current season (final 'rnl', then preliminary 'sat' for the current season)
  data_dir/chirps3_monthly_9120.nc   CHIRPS v3.0 mean monthly rain 1991-2020 (share of annual rain in OND)
from the climate/hoa build (see climate/hoa/README.md):
  data_dir/out/drivers_seasons.csv   season ENSO and IOD phase (drivers.py)
  data_dir/out/cells.parquet         0.25 deg cells: country and area weight (analyse.py)
  data_dir/impacts_emdat.parquet     EM-DAT events (impacts.py)
  data_dir/era5_t2m_monthly.nc       ERA5 monthly mean 2 m temperature, 1981-2025 (download.py)
  data_dir/nino34_ersst5.txt, data_dir/dmi_had.txt   monthly Nino3.4 and DMI (drivers.py)
and the current-season indices and temperature from download_current.py (data_dir/current/).
Rains start is computed here for every season with the method of climate/hoa/daily_metrics.py.
Writes data_dir/out/flood_timing.json (everything the page shows), flood_timing_seasons.csv,
flood_timing_events.csv and flood_timing_temperature.csv.
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
YEAR = int(sys.argv[2]) if len(sys.argv) > 2 else 2026
OUT, CUR = DATA / "out", DATA / "current"
ISOS = ["ETH", "KEN", "SOM"]
NORMAL = range(1991, 2021)
LEAD = 30  # days from 1 Sep to 1 Oct: the onset search starts on 1 Sep, as in climate/hoa/daily_metrics.py

cells = pd.read_parquet(OUT / "cells.parquet")
LAT, LON = cells["lat"].values, cells["lon"].values
WGT = cells["weight"].values


def pick(da):
    """Daily values at each cell centre: days x cells."""
    p = da.sel(latitude=xr.DataArray(LAT, dims="cell"), longitude=xr.DataArray(LON, dims="cell"),
               method="nearest", tolerance=0.01)
    return pd.DataFrame(p.values, index=pd.to_datetime(p["time"].values))


# OND rainfall areas: cells where October-December brings at least a quarter of the annual rain (CHIRPS v3, 1991-2020)
with xr.open_dataarray(DATA / "chirps3_monthly_9120.nc") as m:
    m = m.sel(latitude=xr.DataArray(LAT, dims="cell"), longitude=xr.DataArray(LON, dims="cell"),
              method="nearest", tolerance=0.01)
    with np.errstate(invalid="ignore"):
        SHARE = (m.sel(month=[10, 11, 12]).sum("month", skipna=False) / m.sum("month", skipna=False)).values
SEL = {iso: (cells["iso3"].values == iso) & (SHARE >= 0.25) for iso in ISOS}
print("OND rainfall-area cells:", {iso: int(v.sum()) for iso, v in SEL.items()})


def country_mean(v, iso):
    """Area-weighted mean over the country's OND-share cells, skipping missing cells day by day."""
    sel = SEL[iso]
    x, w = v[:, sel], WGT[sel]
    ok = ~np.isnan(x)
    ws = (ok * w).sum(1)
    with np.errstate(invalid="ignore", divide="ignore"):  # a day with no data at all stays NaN, not 0
        return np.where(ws > 0, np.nansum(np.where(ok, x, 0) * w, 1) / ws, np.nan)


# ---- 1. daily rain per cell and per country, 1981 to the current season; per-cell Sep-Dec normal for rains start
parts, prelim = [], pd.DatetimeIndex([])
for f in sorted((DATA / "chirps3_daily").glob("[0-9][0-9][0-9][0-9].nc")):
    with xr.open_dataset(f) as ds:
        parts.append(pick(ds["precip"]))
        prelim = prelim.append(pd.DatetimeIndex(ds["time"].values[ds["prelim"].values.astype(bool)]))
allday = pd.concat(parts).sort_index()
LAST = allday.index.max()
print(f"{YEAR}: final to {allday.index[~allday.index.isin(prelim)].max().date()}, preliminary to {LAST.date()}")
daily = {iso: pd.Series(country_mean(allday.values, iso), index=allday.index) for iso in ISOS}
pd.DataFrame(daily).to_parquet(OUT / "flood_timing_daily.parquet")
qbar = pd.concat([allday.loc[f"{y}-09-01":f"{y}-12-31"] for y in NORMAL]).mean(0).values  # mean daily rain, 1 Sep-31 Dec


def rains_start(y, end=None):
    """Per cell: rains start by anomalous accumulation over 1 Sep to `end` (default 31 Dec), in days from 1 Oct;
    whether it was found (else the window's last day); cells with complete data. As climate/hoa/daily_metrics.py."""
    win = allday.loc[f"{y}-09-01":end or f"{y}-12-31"].values
    ok = ~np.isnan(win).any(0)
    i = np.cumsum(np.nan_to_num(win) - qbar, 0).argmin(0)
    found = i < len(win) - 1
    return np.where(found, i + 1 - LEAD, len(win) - 1 - LEAD), found, ok


# ---- 2. season phase and EM-DAT floods starting September to December
s = pd.read_csv(OUT / "drivers_seasons.csv")
s = s[s.season == "OND"].drop_duplicates("year").set_index("year")
# only seasons EM-DAT covers to the end of December: its last event in the three countries starts in May 2025,
# so the 2025 season is left out
d = pd.read_parquet(DATA / "impacts_emdat.parquet")
st = d.dropna(subset=["Start Year", "Start Month"])
EM_LAST = max(pd.Timestamp(int(a), int(b), 1) for a, b in zip(st["Start Year"], st["Start Month"]))
s = s[[pd.Timestamp(y, 12, 1) < EM_LAST for y in s.index]]
print(f"EM-DAT last event starts {EM_LAST:%Y-%m}; seasons {s.index.min()}-{s.index.max()}")


def grp(y):
    e, i = s.loc[y, "enso"], s.loc[y, "iod"]
    if e == "El Niño" and i == "positive":
        return "both"
    return {"El Niño": "nino", "La Niña": "nina"}.get(e, "neutral")


s["grp"] = [grp(y) for y in s.index]
BOTH = s[s.grp == "both"].index.tolist()
print("seasons with both:", BOTH, "| positive IOD:", s[s.iod == "positive"].index.tolist())

f = d[(d["Disaster Type"] == "Flood") & d["Start Month"].isin([9, 10, 11, 12])
      & d["Start Year"].between(s.index.min(), s.index.max())].copy()
f["grp"] = f["Start Year"].map(s.grp)
f["riv"] = f["Disaster Subtype"] == "Riverine flood"
# each country-season once, at its earliest start month
first = (f.sort_values("Start Month").groupby(["ISO", "Start Year"])
         .agg(m=("Start Month", "first"), grp=("grp", "first")).reset_index())
firstr = (f[f.riv].sort_values("Start Month").groupby(["ISO", "Start Year"])
          .agg(m=("Start Month", "first"), grp=("grp", "first")).reset_index())
groups = {}
for g in ["both", "nino", "neutral", "nina"]:
    a, r = first[first.grp == g], firstr[firstr.grp == g]
    groups[g] = dict(
        years=s[s.grp == g].index.tolist(), country_seasons=int((s.grp == g).sum()) * 3, any=len(a),
        by_month={int(k): int(v) for k, v in a.m.value_counts().sort_index().items()},
        riv=len(r), riv_by_month={int(k): int(v) for k, v in r.m.value_counts().sort_index().items()},
        by_iso={i: int((a.ISO == i).sum()) for i in ISOS}, riv_by_iso={i: int((r.ISO == i).sum()) for i in ISOS})
ev = f[["DisNo.", "ISO", "Disaster Subtype", "Start Year", "Start Month", "End Year", "End Month",
        "Total Affected", "Location", "grp"]].sort_values(["Start Year", "Start Month", "ISO"])
ev.to_csv(OUT / "flood_timing_events.csv", index=False)

# ---- 3. per season: rains start, date the mean Sep-Dec total has fallen (from 1 Sep), Oct-Dec rain % of normal
RS = {y: rains_start(y) for y in s.index}
NT = {iso: np.mean([daily[iso][f"{y}-10-01":f"{y}-12-31"].sum() for y in NORMAL]) for iso in ISOS}
NSD = {iso: np.mean([daily[iso][f"{y}-09-01":f"{y}-12-31"].sum() for y in NORMAL]) for iso in ISOS}
rec = []
for iso in ISOS:
    dd, nt = daily[iso], NT[iso]
    for y in s.index:
        cum = dd[f"{y}-09-01":f"{y + 1}-01-31"].cumsum()
        full = cum[cum >= NSD[iso]]
        on, found, ok = RS[y]
        sel = SEL[iso] & ok
        od = np.average(on[sel], weights=WGT[sel])
        rec.append(dict(iso3=iso, year=y, grp=s.loc[y, "grp"],
                        onset=pd.Timestamp(y, 10, 1) + pd.Timedelta(days=round(od)),
                        no_onset_pct=float(np.average(~found[sel], weights=WGT[sel]) * 100),
                        full_normal=full.index[0] if len(full) else pd.NaT,
                        ond_pct=100 * dd[f"{y}-10-01":f"{y}-12-31"].sum() / nt))
t = pd.DataFrame(rec)
t.to_csv(OUT / "flood_timing_seasons.csv", index=False)

# ---- 4. current season: provisional rains start (anomalous accumulation per cell, from 1 Sep)
onset, found, ok = rains_start(YEAR, LAST)
md = LAST.strftime("%m-%d")
stats, curves, seasons = {}, {}, {}
for iso in ISOS:
    sel = SEL[iso] & ok
    od = np.average(onset[sel], weights=WGT[sel])
    dd, nt = daily[iso], NT[iso]
    per = lambda y: dd[f"{y}-09-01":f"{y}-{md}"].sum()  # rain from 1 Sep to the same day as YEAR's data end
    nper = np.mean([per(y) for y in NORMAL])
    lm = lambda y: dd[f"{y}-{LAST:%m}-01":f"{y}-{md}"].sum()  # rain from the 1st of the data's last month to its last day
    c = dd[f"{YEAR}-09-01":LAST].cumsum()
    stats[iso] = dict(
        onset_date=str((pd.Timestamp(YEAR, 10, 1) + pd.Timedelta(days=round(od))).date()),
        no_onset_pct=round(float(np.average(~found[sel], weights=WGT[sel]) * 100), 1),
        to_date_pct=round(float(100 * per(YEAR) / nper), 1),
        full_normal=str(c[c >= NSD[iso]].index[0].date()) if (c >= NSD[iso]).any() else None,
        month_mm=round(float(lm(YEAR)), 1), month_normal_mm=round(float(np.mean([lm(y) for y in NORMAL])), 1))
    # every past season: OND rain % of mean, any EM-DAT flood starting Sep-Dec, rain to date % of mean
    fl_years = set(first.loc[first.ISO == iso, "Start Year"])
    tt = t[t.iso3 == iso].set_index("year")
    seasons[iso] = [[int(y), round(float(tt.loc[y, "ond_pct"]), 1), int(y in fl_years), round(float(100 * per(y) / nper), 1)]
                    for y in s.index]

    def curve(y):
        return [round(float(v), 1) for v in dd[f"{y}-09-01":f"{y}-12-31"].cumsum().values]

    curves[iso] = dict(normal=[round(float(v), 1) for v in np.mean([curve(y) for y in NORMAL], 0)],
                       **{str(y): curve(y) for y in BOTH}, **{str(YEAR): curve(YEAR)})
    print(iso, stats[iso])

# ---- 5. current indices: CPC ONI (latest three-month season), DMI from OISST
oni = [ln.split() for ln in (CUR / "oni.txt").read_text().splitlines()[1:] if ln.strip()]
oni = [r for r in oni if int(r[1]) == YEAR]
dmi = pd.read_csv(CUR / "dmi_oisst_daily.csv", index_col=0, parse_dates=True)["dmi"]
dmi_m = dmi.resample("MS").mean()
now = dict(oni_aso=float(oni[-1][3]), dmi_oct=round(float(dmi_m[f"{YEAR}-10"].iloc[0]), 2),
           dmi_nov=round(float(dmi_m[f"{YEAR}-11"].iloc[0]), 2), dmi_nov_to=str(dmi.index.max().date()))
print("ONI", oni[-1][0], oni[-1][3], "|", now)

# ---- 6. monthly 2 m temperature, June to December, over the same cells: ERA5 monthly means 1981-2025 and the
#      current year's ERA5 daily means (complete months), as departures from each month's 1981-2025 linear trend
TMON = list(range(6, 13))


def at_centres(da):
    """ERA5 on whole quarter degrees to the cell centres: bilinear at a midpoint, the mean of the four corners."""
    return sum(da.sel(latitude=xr.DataArray(LAT + dy, dims="cell"), longitude=xr.DataArray(LON + dx, dims="cell"),
                      method="nearest", tolerance=0.01).values
               for dy in (-0.125, 0.125) for dx in (-0.125, 0.125)) / 4


with xr.open_dataset(DATA / "era5_t2m_monthly.nc") as d:
    tm = d["t2m"].load()
with xr.open_dataarray(CUR / f"era5_t2m_daily_{YEAR}.nc") as d:
    td = d.load()
tdays = pd.to_datetime(td["valid_time"].values)
tfull = [m for m in range(1, 13) if (tdays.month == m).sum() == pd.Timestamp(YEAR, m, 1).days_in_month]
tc = td.resample(valid_time="MS").mean().sel(valid_time=[pd.Timestamp(YEAR, m, 1) for m in tfull])
temp = pd.concat([pd.DataFrame({i: country_mean(at_centres(x), i) for i in ISOS},
                               index=pd.to_datetime(x["valid_time"].values)) for x in (tm, tc)]) - 273.15
temp = temp[~temp.index.duplicated(keep="last")]


def monthly(path):
    if path.name.startswith("nino"):
        n = pd.read_csv(path, sep=r"\s+")
        return pd.Series(n.iloc[:, -1].values, index=pd.MultiIndex.from_arrays([n["YR"], n["MON"]]))
    rows = []
    for line in path.read_text().splitlines()[1:]:
        p = line.split()
        if len(p) == 13 and p[0].isdigit():
            rows += [(int(p[0]), m + 1, float(v)) for m, v in enumerate(p[1:]) if float(v) > -99]
    return pd.DataFrame(rows, columns=["y", "m", "v"]).set_index(["y", "m"])["v"]


idx = {k: monthly(DATA / f) for k, f in [("nino", "nino34_ersst5.txt"), ("dmi", "dmi_had.txt")]}
YRS = list(s.index)
N3 = len(YRS) // 3  # thirds of the seasons, by August-September temperature and by OND rain


def partial(x, y, z):
    """Correlation of x and y with the columns of z held constant (both regressed on z first)."""
    Z = np.column_stack([np.ones(len(x))] + list(z))
    rx, ry = (v - Z @ np.linalg.lstsq(Z, v, rcond=None)[0] for v in (x, y))
    return float(np.corrcoef(rx, ry)[0, 1])


tstats, trows = {}, []
for iso in ISOS:
    tab = pd.DataFrame({m: temp.loc[temp.index.month == m, iso].set_axis(temp.index[temp.index.month == m].year)
                        for m in TMON})
    trend = {}
    for m in TMON:
        p = np.polyfit(YRS, tab.loc[YRS, m].values, 1)
        trend[m] = p[0] * 10
        tab[m] = tab[m] - np.polyval(p, tab.index)
    fl = set(first.loc[first.ISO == iso, "Start Year"])
    tt = t[t.iso3 == iso].set_index("year")
    g = pd.DataFrame(dict(dep_as=tab.loc[YRS, [8, 9]].mean(axis=1), flood=[int(y in fl) for y in YRS],
                          ond_pct=tt.loc[YRS, "ond_pct"],
                          nino_as=[np.mean([idx["nino"][(y, m)] for m in (8, 9)]) for y in YRS],
                          dmi_as=[np.mean([idx["dmi"][(y, m)] for m in (8, 9)]) for y in YRS]), index=YRS)
    # October rain as % of the 1991-2020 October mean, for the same-month comparison
    octr = pd.Series({y: daily[iso][f"{y}-10-01":f"{y}-10-31"].sum() for y in YRS + [YEAR]})
    octp = 100 * octr / octr.loc[list(NORMAL)].mean()
    g["oct_dep"], g["oct_pct"] = tab.loc[YRS, 10], octp.loc[YRS]
    nino = pd.Series([s.loc[y, "grp"] in ("both", "nino") for y in YRS], index=YRS)  # El Nino, with or without a positive IOD
    z = (g.nino_as.values, g.dmi_as.values)
    cur_as = float(tab.loc[YEAR, [8, 9]].mean())
    oct_done = LAST >= pd.Timestamp(YEAR, 10, 31) and not pd.isna(tab.loc[YEAR, 10])

    def cool_ranked(sub, n):
        """Of the n wettest and the n driest Octobers in sub: how many were cooler than the trend."""
        o = sub.sort_values("oct_pct")
        return [int((o.oct_dep.tail(n) < 0).sum()), int((o.oct_dep.head(n) < 0).sum()), n]

    def after(sub):
        """Seasons after a warm and after a cool Aug-Sep (warm: above the trend): count, OND rain above the
        1991-2020 mean, EM-DAT flood."""
        w = sub.dep_as > 0
        return {k: dict(n=int(m.sum()), wet=int((m & (sub.ond_pct > 100)).sum()), fl=int((m & (sub.flood == 1)).sum()))
                for k, m in (("warm", w), ("cool", ~w))}

    tstats[iso] = dict(
        pts=[[int(y), round(float(g.dep_as[y]), 2), round(float(g.ond_pct[y]), 1), round(float(g.oct_dep[y]), 2),
              round(float(g.oct_pct[y]), 1), int(g.flood[y]), int(nino[y])] for y in YRS],
        oct_cool=cool_ranked(g, N3), oct_cool_nn=cool_ranked(g[~nino], int((~nino).sum()) // 3),
        all=after(g), nn=after(g[~nino]), n_nn=int((~nino).sum()),
        pr_rain=round(partial(g.dep_as.values, g.ond_pct.values, z), 2),
        pr_flood=round(partial(g.dep_as.values, g.flood.values.astype(float), z), 2),
        r_dmi=round(float(np.corrcoef(g.dep_as, g.dmi_as)[0, 1]), 2),
        cur_as=round(cur_as, 2), cur_rank=int((g.dep_as < cur_as).sum()),
        cur_oct=round(float(tab.loc[YEAR, 10]), 2) if oct_done else None,
        cur_oct_pct=round(float(octp[YEAR]), 1) if oct_done else None,
        trend=[round(trend[m], 2) for m in TMON])
    for y in tab.index:
        trows.append(dict(iso3=iso, year=y, **{f"dep_{m:02d}": tab.loc[y, m] for m in TMON},
                          flood=g.flood.get(y), ond_pct=g.ond_pct.get(y), nino_as=g.nino_as.get(y), dmi_as=g.dmi_as.get(y)))
    print(iso, "temperature:", {k: v for k, v in tstats[iso].items() if k not in ("pts", "trend")})
pd.DataFrame(trows).round(3).to_csv(OUT / "flood_timing_temperature.csv", index=False)
temp_to = str((pd.Timestamp(YEAR, tfull[-1], 1) + pd.offsets.MonthEnd(0)).date())
print("temperature to", temp_to)


# ---- 7. page data
def dos(x, y):  # days since 1 Sep of the season year
    return None if pd.isna(x) else int((x - pd.Timestamp(y, 9, 1)).days)


timeline = {}
for iso in ["SOM", "KEN", "ETH"]:
    out = []
    for y in BOTH:
        r = t[(t.iso3 == iso) & (t.year == y)].iloc[0]
        e = ev[(ev.ISO == iso) & (ev["Start Year"] == y)]
        out.append(dict(year=y, onset=dos(r.onset, y), full=dos(r.full_normal, y), ond_pct=int(round(r.ond_pct)),
                        no_onset_pct=round(float(r.no_onset_pct), 1),
                        months=sorted(set(int(m) for m in e["Start Month"])),
                        events=[dict(id=a["DisNo."], sub=a["Disaster Subtype"], m=int(a["Start Month"]),
                                     aff=None if pd.isna(a["Total Affected"]) else int(a["Total Affected"]),
                                     loc=a["Location"]) for _, a in e.iterrows()]))
    c = stats[iso]
    out.append(dict(year=YEAR, onset=dos(pd.Timestamp(c["onset_date"]), YEAR),
                    full=dos(pd.Timestamp(c["full_normal"]), YEAR) if c["full_normal"] else None,
                    no_onset_pct=c["no_onset_pct"], months=[], events=[]))
    timeline[iso] = out

riv = ev[(ev["Disaster Subtype"] == "Riverine flood") & (ev.grp != "both")]
riverine = [dict(year=int(a["Start Year"]), grp=a.grp, iso=a.ISO, m=int(a["Start Month"]),
                 aff=None if pd.isna(a["Total Affected"]) else int(a["Total Affected"]), loc=a["Location"],
                 nino=round(float(s.loc[int(a["Start Year"]), "nino34"]), 2),
                 dmi=round(float(s.loc[int(a["Start Year"]), "dmi"]), 2)) for _, a in riv.iterrows()]
nfl = first.groupby("Start Year").size()
phase = [dict(year=int(y), nino=round(float(r.nino34), 2), dmi=round(float(r.dmi), 2), grp=r.grp,
              nflood=int(nfl.get(y, 0))) for y, r in s.iterrows()]

page = dict(seasons_span=[int(s.index.min()), int(s.index.max())], emdat_last=f"{EM_LAST:%Y-%m}", groups=groups, timeline=timeline, riverine=riverine, phase=phase, curves=curves, stats26=stats, now=now,
            seasons=seasons, temp=tstats, temp_to=temp_to)
(OUT / "flood_timing.json").write_text(json.dumps(page, separators=(",", ":"), ensure_ascii=False), encoding="utf-8")
print("wrote", OUT / "flood_timing.json", "| riverine events in other seasons:", len(riverine))

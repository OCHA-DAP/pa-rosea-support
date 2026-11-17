"""Second-way recalculation of the numbers on the Horn of Africa review page.

Run: python climate/hoa/independent_check.py [data_dir]   (after build_review_page.py)
Recomputes from the raw downloads by a different route (point-in-polygon country membership instead of
rasterize, nearest ERA5 point instead of linear interpolation, numpy instead of xarray groupby) and
compares with data_dir/out and the numbers printed in the page. Prints one line per check and
"PROBLEMS: none" when everything agrees within tolerance.
"""
import json
import re
import sys
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
PAGE = Path(__file__).parent / "hoa_climate_review.html"
problems = []


def check(name, ok, detail=""):
    print(("[PASS] " if ok else "[FAIL] ") + name + (f" | {detail}" if detail else ""))
    if not ok:
        problems.append(name)


# ---- raw CHIRPS blocks, built again with numpy
files = sorted((DATA / "chirps").glob("*.npy"))
check("CHIRPS has 540 months 1981-01 to 2025-12", len(files) == 540 and files[0].stem == "1981-01" and files[-1].stem == "2025-12",
      f"{len(files)} files {files[0].stem} to {files[-1].stem}")
a, _, x0, _, e, y0 = map(float, (DATA / "chirps" / "transform.txt").read_text().split())
check("CHIRPS window starts at 32 E, 16 N with 0.05 deg pixels", abs(x0 - 32) < 1e-6 and abs(y0 - 16) < 1e-6 and abs(a - 0.05) < 1e-6 and abs(e + 0.05) < 1e-6)
raw = np.stack([np.load(f) for f in files])  # (t, 420, 400), north to south
check("CHIRPS raw values are mm per month (no negatives, plausible maximum)", np.nanmin(raw) >= 0 and 300 < np.nanmax(raw) < 3000,
      f"min {np.nanmin(raw):.1f} max {np.nanmax(raw):.1f}")
nb_y, nb_x = raw.shape[1] // 5, raw.shape[2] // 5
blk = np.full((raw.shape[0], nb_y, nb_x), np.nan)
for i in range(nb_y):
    for j in range(nb_x):
        sub = raw[:, i * 5:(i + 1) * 5, j * 5:(j + 1) * 5].reshape(raw.shape[0], -1)
        good = ~np.isnan(sub).all(1)
        blk[good, i, j] = np.nanmean(sub[good], axis=1)
blat = 16 - 0.125 - 0.25 * np.arange(nb_y)
blon = 32 + 0.125 + 0.25 * np.arange(nb_x)

# ---- country membership by point in polygon
adm = gpd.read_parquet(DATA / "adm1.parquet").dissolve("iso3").reset_index()
LA, LO = np.meshgrid(blat, blon, indexing="ij")
pts = gpd.GeoDataFrame(geometry=gpd.points_from_xy(LO.ravel(), LA.ravel()), crs=4326)
iso = gpd.sjoin(pts, adm[["iso3", "geometry"]], predicate="within", how="left")["iso3"].groupby(level=0).first().reindex(range(len(pts)))
iso = np.array(iso.fillna("").tolist(), dtype=object).reshape(LA.shape)
cells = pd.read_parquet(OUT / "cells.parquet")
mine = {(round(r.lat, 3), round(r.lon, 3)): r.iso3 for r in cells.itertuples()}
theirs = {(round(LA[i, j], 3), round(LO[i, j], 3)): iso[i, j] for i in range(nb_y) for j in range(nb_x) if iso[i, j]}
agree = sum(mine.get(k) == v for k, v in theirs.items())
check("Country cells: rasterize and point-in-polygon agree on 99%+", agree / max(len(theirs), 1) > 0.99 and abs(len(mine) - len(theirs)) / len(theirs) < 0.01,
      f"{agree}/{len(theirs)} agree, {len(mine)} vs {len(theirs)} cells")

# ---- ERA5 nearest point, afternoon max as max over the 5 hours with numpy
t2 = xr.open_dataset(DATA / "era5_t2m_afternoon.nc")
hrs = pd.to_datetime(t2["valid_time"].values)
check("Afternoon file holds 09-13 UTC only, 5 per month, 540 months", set(hrs.hour) == {9, 10, 11, 12, 13} and len(hrs) == 2700)
arr = t2["t2m"].values.reshape(540, 5, *t2["t2m"].shape[1:]).max(1) - 273.15
elat, elon = t2["latitude"].values, t2["longitude"].values
ii = np.abs(elat[:, None] - blat[None, :]).argmin(0)
jj = np.abs(elon[:, None] - blon[None, :]).argmin(0)
tmax_blk = arr[:, ii][:, :, jj]  # nearest ERA5 point to each block centre

months = np.array([int(f.stem[5:]) for f in files])
years = np.array([int(f.stem[:4]) for f in files])
norm = (years >= 1991) & (years <= 2020)
w = np.cos(np.deg2rad(LA))
rn = np.stack([blk[norm & (months == m)].mean(0) for m in range(1, 13)])
tn = np.stack([tmax_blk[norm & (months == m)].mean(0) for m in range(1, 13)])
ann = rn.sum(0)
share = {s: rn[[m - 1 for m in ms]].sum(0) / ann for s, ms in {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}.items()}
bimodal = (share["MAM"] >= .25) & (share["OND"] >= .25)
uni = (share["JJAS"] >= .5) & ~bimodal

cyc = pd.read_csv(OUT / "cycle.csv")
for c, reg, msk in [("KEN", "bimodal (MAM and OND)", bimodal), ("SOM", "bimodal (MAM and OND)", bimodal), ("ETH", "unimodal (JJAS)", uni), ("ETH", "bimodal (MAM and OND)", bimodal)]:
    sel = (iso == c) & msk & ~np.isnan(ann)
    r2 = [np.average(rn[m][sel], weights=w[sel]) for m in range(12)]
    t2_ = [np.average(tn[m][sel], weights=w[sel]) for m in range(12)]
    g = cyc[(cyc.iso3 == c) & (cyc.regime == reg)].sort_values("month")
    dr, dt = np.max(np.abs(g.rain_mm.values - r2)), np.max(np.abs(g.tmax_c.values - t2_))
    tol = 0.5 if reg == "unimodal (JJAS)" else 0.3  # highlands: nearest point and linear interpolation differ more over steep terrain
    check(f"Cycle {c} {reg}: rain within 3 mm, afternoon max within {tol} C", dr < 3 and dt < tol, f"max diff {dr:.2f} mm, {dt:.2f} C, cells {sel.sum()} vs {int(g.cells.iloc[0])}")
    check(f"Cycle {c} {reg}: same hottest and wettest month", int(np.argmax(t2_)) + 1 == int(g.month.values[g.tmax_c.values.argmax()]) and int(np.argmax(r2)) + 1 == int(g.month.values[g.rain_mm.values.argmax()]))

# ---- season anomalies
seas = pd.read_csv(OUT / "seasons.csv")
SM = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
for (c, s) in [("ETH", "MAM"), ("ETH", "JJAS"), ("ETH", "OND"), ("KEN", "MAM"), ("KEN", "JJAS"), ("KEN", "OND"),
               ("SOM", "MAM"), ("SOM", "JJAS"), ("SOM", "OND")]:
    sel = (iso == c) & (share[s] >= .25) & ~np.isnan(ann)
    yrs = np.arange(1981, 2026)
    rs = np.stack([blk[(years == y) & np.isin(months, SM[s])].sum(0) for y in yrs])
    ts = np.stack([tmax_blk[(years == y) & np.isin(months, SM[s])].mean(0) for y in yrs])
    rc, tc = rs[(yrs >= 1991) & (yrs <= 2020)].mean(0), ts[(yrs >= 1991) & (yrs <= 2020)].mean(0)
    rp = np.array([np.sum(rs[k][sel] * w[sel]) / np.sum(rc[sel] * w[sel]) * 100 for k in range(len(yrs))])
    ta = np.array([np.average((ts[k] - tc)[sel], weights=w[sel]) for k in range(len(yrs))])
    g = seas[(seas.iso3 == c) & (seas.season == s)].sort_values("year")
    d1, d2 = np.max(np.abs(g.rain_pct.values - rp)), np.max(np.abs(g.tmax_anom_c.values - ta))
    check(f"Season {c} {s}: rain % within 2 points, anomaly within 0.1 C", d1 < 2 and d2 < 0.1, f"max diff {d1:.2f} pts, {d2:.3f} C")
    det = ta - np.polyval(np.polyfit(yrs, ta, 1), yrs)
    r = np.corrcoef(rp, det)[0, 1]
    r_page = re.search(rf"{c}-{s}.*?\"r\": (-?[0-9.]+)", PAGE.read_text(encoding="utf8"))
    rp_ = float(r_page.group(1)) if r_page else np.nan
    near_zero = abs(rp_) < 0.15 and abs(r) < 0.15  # a near-zero correlation moves with the ERA5 interpolation method
    check(f"Season {c} {s}: correlation on the page matches recalculation within 0.03 (or both near zero)",
          abs(rp_ - r) < 0.03 or near_zero, f"page {rp_} vs {r:.2f}" + (" (both near zero)" if near_zero and abs(rp_ - r) >= 0.03 else ""))

# ---- EM-DAT matching, by explicit listing
em = pd.read_parquet(DATA / "impacts_emdat.parquet")
check("EM-DAT: flood events with no start month (left off the season charts)", True,
      f"{int(em[(em['Disaster Type'] == 'Flood') & em['Start Month'].isna()].shape[0])} of {int((em['Disaster Type'] == 'Flood').sum())}")
for (c, s) in [("KEN", "OND"), ("SOM", "OND"), ("ETH", "JJAS")]:
    g = seas[(seas.iso3 == c) & (seas.season == s)]
    fl = em[(em.ISO == c) & (em["Disaster Type"] == "Flood") & em["Start Month"].isin(SM[s])]
    yrs_fl = sorted(set(fl["Start Year"].astype(int)))
    check(f"EM-DAT {c} {s}: flood years match", sorted(g[g.flood_events > 0].year) == yrs_fl, f"{yrs_fl}")

# ---- page text numbers
import html as _html
txt = _html.unescape(re.sub(r"<[^>]+>", " ", PAGE.read_text(encoding="utf8")))
for frag in ["0 of 22", "8 of 28", "Kenya's two-season areas 30.9 °C against 30.5 °C", "Somalia's 32.0 °C against 32.5 °C",
             "Ethiopia's two-season areas 29.8 °C against 31.0 °C"]:
    check(f"Page text contains '{frag}'", frag in txt)

# ---- drought matching by month: seasons the reviewer found wrongly tagged under year-only matching
for c, s, y in [("KEN", "MAM", 2018), ("KEN", "OND", 2019), ("ETH", "OND", 2023), ("KEN", "OND", 2015)]:
    q = seas[(seas.iso3 == c) & (seas.season == s) & (seas.year == y)].iloc[0]
    check(f"No EM-DAT drought overlaps {c} {s} {y}", q.drought_events == 0, f"{q.drought_events} matched")
for c, s, y in [("SOM", "OND", 2021), ("KEN", "OND", 2021), ("ETH", "MAM", 2022), ("SOM", "OND", 2019)]:  # SOM 2019 drought ends 2019-10
    q = seas[(seas.iso3 == c) & (seas.season == s) & (seas.year == y)].iloc[0]
    check(f"EM-DAT drought overlaps {c} {s} {y}", q.drought_events > 0, f"{q.drought_events} matched")
mo = seas[seas.season.isin(["MAM", "OND"]) & (seas.rain_pct >= 120)]
e_ = mo[mo.year <= 2000]; l_ = mo[mo.year >= 2011]
check("Wet-and-warm counts recomputed", (len(e_), int((e_.tmax_anom_c > 0).sum()), len(l_), int((l_.tmax_anom_c > 0).sum())) == (22, 0, 28, 8),
      f"{(e_.tmax_anom_c > 0).sum()} of {len(e_)}, {(l_.tmax_anom_c > 0).sum()} of {len(l_)}")

# ---- ENSO and IOD: indices parsed again line by line, correlations recomputed with numpy from this script's own rainfall
nino_raw = {}
for line in (DATA / "nino34_ersst5.txt").read_text().splitlines()[1:]:
    p = line.split()
    nino_raw[(int(p[0]), int(p[1]))] = float(p[9])  # YR MON then four (value, anomaly) pairs; NINO3.4 ANOM is the 10th field
dmi_raw = {}
for line in (DATA / "dmi_had.txt").read_text().splitlines():
    p = line.split()
    if len(p) == 13 and p[0].isdigit():
        for m in range(12):
            if float(p[m + 1]) > -99:
                dmi_raw[(int(p[0]), m + 1)] = float(p[m + 1])
drv = pd.read_csv(OUT / "drivers_seasons.csv")
for (c, s, y) in [("KEN", "OND", 1997), ("SOM", "OND", 2019), ("ETH", "JJAS", 2015), ("KEN", "MAM", 2010)]:
    q = drv[(drv.iso3 == c) & (drv.season == s) & (drv.year == y)].iloc[0]
    n_ = np.mean([nino_raw[(y, m)] for m in SM[s]]); d_ = np.mean([dmi_raw[(y, m)] for m in SM[s]])
    check(f"Indices {s} {y}: Niño3.4 and DMI season means match", abs(q.nino34 - n_) < 1e-6 and abs(q.dmi - d_) < 1e-6, f"{n_:+.2f}, {d_:+.2f}")
dc = pd.read_csv(OUT / "drivers_corr.csv")
for (c, s) in [("ETH", "OND"), ("KEN", "OND"), ("SOM", "OND"), ("ETH", "JJAS")]:
    sel = (iso == c) & (share[s] >= .25) & ~np.isnan(ann)
    yrs = np.arange(1981, 2026)
    rs = np.stack([blk[(years == y) & np.isin(months, SM[s])].sum(0) for y in yrs])
    rc = rs[(yrs >= 1991) & (yrs <= 2020)].mean(0)
    rp = np.array([np.sum(rs[k][sel] * w[sel]) / np.sum(rc[sel] * w[sel]) * 100 for k in range(len(yrs))])
    for idx, raw_ in [("nino34", nino_raw), ("dmi", dmi_raw)]:
        ix = np.array([np.mean([raw_[(y, m)] for m in SM[s]]) for y in yrs])
        r = np.corrcoef(ix, rp)[0, 1]
        page_r = float(dc[(dc.iso3 == c) & (dc.season == s) & (dc["index"] == idx) & (dc["var"] == "rain")].r.iloc[0])
        check(f"{idx} with rain {c} {s}: correlation matches within 0.02", abs(page_r - r) < 0.02, f"{page_r:+.3f} vs {r:+.3f}")

# ---- dry spells: longest run recomputed per cell with plain Python run lengths, for a few seasons
from itertools import groupby
dsp = pd.read_csv(OUT / "dryspells.csv")
for (c, s, y) in [("KEN", "OND", 2010), ("SOM", "OND", 2019), ("ETH", "JJAS", 2015)]:
    da = xr.open_dataarray(DATA / "chirps_daily" / f"{y}.nc")
    t = pd.to_datetime(da["time"].values)
    v = da.values[np.isin(t.month, SM[s])]
    clat, clon = da["latitude"].values, da["longitude"].values
    g = cells[(cells.iso3 == c) & (cells[f"share_{s}"] >= .25)]
    longest, wts = [], []
    for r_ in g.itertuples():
        col = v[:, np.abs(clat - r_.lat).argmin(), np.abs(clon - r_.lon).argmin()]
        if np.isnan(col).any():
            continue
        longest.append(max([sum(1 for _ in grp) for k, grp in groupby(col < 1.0) if k] or [0]))
        wts.append(r_.weight)
    mine_ = np.average(longest, weights=wts)
    theirs_ = float(dsp[(dsp.iso3 == c) & (dsp.season == s) & (dsp.year == y)].longest_days.iloc[0])
    check(f"Dry spell {c} {s} {y}: longest run matches within 0.1 day", abs(mine_ - theirs_) < 0.1, f"{theirs_:.2f} vs {mine_:.2f}")

# ---- driver findings text
for frag in ["correlate at +0.67", "+0.79 to +0.82", "6 of the 6 positive-IOD seasons"]:
    check(f"Page text contains '{frag}'", frag in txt)
# phase medians and the IOD / El Nino overlap, from this script's own index parse and rainfall
for c in ["ETH", "KEN", "SOM"]:
    sel = (iso == c) & (share["OND"] >= .25) & ~np.isnan(ann)
    yrs = np.arange(1981, 2026)
    rs = np.stack([blk[(years == y) & np.isin(months, SM["OND"])].sum(0) for y in yrs])
    rc = rs[(yrs >= 1991) & (yrs <= 2020)].mean(0)
    rp = np.array([np.sum(rs[k][sel] * w[sel]) / np.sum(rc[sel] * w[sel]) * 100 for k in range(len(yrs))])
    dmi_s = np.array([np.mean([dmi_raw[(y, m)] for m in SM["OND"]]) for y in yrs])
    nino_s = np.array([np.mean([nino_raw[(y, m)] for m in SM["OND"]]) for y in yrs])
    pos = dmi_s >= 0.4
    check(f"{c} OND: every positive-IOD season is also El Niño", bool(np.all(nino_s[pos] >= 0.5)), f"{pos.sum()} seasons")
    for name, msk in [("positive IOD", pos), ("La Niña", nino_s <= -0.5), ("ENSO neutral", np.abs(nino_s) < 0.5)]:
        med = float(np.median(rp[msk]))
        check(f"{c} OND {name}: median rain {med:.0f}% is on the page (within 1 point)", any(f"{med + d:.0f}%" in txt for d in (-1, 0, 1)))

print("PROBLEMS:", ", ".join(problems) if problems else "none")

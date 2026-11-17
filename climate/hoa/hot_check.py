"""Second-way recalculation of the very-hot-day and plain ENSO/IOD numbers on the first-version review page.

Run: python climate/hoa/hot_check.py [data_dir]   (after hot_days.py and build_review_page.py)
Recomputes from the ERA5 daily files by a different route (nearest ERA5 point instead of linear interpolation,
pandas quantiles instead of numpy percentiles, the warming trend fitted on yearly means instead of daily
values, ENSO/IOD phases from the raw NOAA text files) and compares with the numbers printed in
hoa_climate_review_v1.html. Counts must match exactly; day averages within 1 day (the nearest point differs
a little from the interpolated one). Prints "PROBLEMS: none" when everything agrees.
"""
import html
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
PAGE = Path(__file__).parent / "hoa_climate_review_v1.html"
NAMES = ["ETH", "KEN", "SOM"]
WIN = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12], "FM": [2, 3]}
problems = []


def check(name, ok, detail=""):
    print(("[PASS] " if ok else "[FAIL] ") + name + (f" | {detail}" if detail else ""))
    if not ok:
        problems.append(name)


def close(page, mine, tol=1.0):
    return len(page) == len(mine) and all(abs(a - b) <= tol for a, b in zip(page, mine))


# ---- daily afternoon highs at the nearest ERA5 point of each cell
cells = pd.read_parquet(OUT / "cells.parquet")
files = sorted((DATA / "era5_daily").glob("[0-9][0-9][0-9][0-9].nc"))
check("ERA5 daily files: 45 years 1981 to 2025", len(files) == 45 and files[0].stem == "1981" and files[-1].stem == "2025")
d = xr.concat([xr.open_dataset(f)["tmax"].load() for f in files], "time")
check("ERA5 daily: one value per day 1981-01-01 to 2025-12-31", d.sizes["time"] == len(pd.date_range("1981-01-01", "2025-12-31")),
      f"{d.sizes['time']} days")
near = d.sel(lat=xr.DataArray(cells.lat.values, dims="cell"), lon=xr.DataArray(cells.lon.values, dims="cell"), method="nearest")
v = pd.DataFrame(near.values, index=pd.to_datetime(near.time.values))
check("Afternoon highs are plausible (5 to 50 deg C)", 5 < np.nanmin(v.values) and np.nanmax(v.values) < 50,
      f"{np.nanmin(v.values):.1f} to {np.nanmax(v.values):.1f}")
mon = v.index.month


def hot_flags(x):
    base = x[(x.index.year >= 1991) & (x.index.year <= 2020)]
    q = base.groupby(base.index.month).quantile(0.9)  # (12, cells)
    return x.gt(q.loc[x.index.month].set_axis(x.index)).astype(np.int8)


hot = hot_flags(v)
share = hot[(hot.index.year >= 1991) & (hot.index.year <= 2020)].groupby(lambda t: t.month).mean().values.mean()
check("By construction about 1 day in 10 is very hot in 1991-2020", abs(share - 0.1) < 0.002, f"{share:.4f}")

# warming trend: slope of each cell's yearly mean departure from its monthly means
dep = v - v.groupby(mon).transform("mean")
ym = dep.groupby(dep.index.year).mean()
slope = np.polyfit(ym.index.values - 2005.5, ym.values, 1)[0]
frac = (v.index.year + (v.index.dayofyear - 0.5) / 365.25).values - 2005.5
hot_d = hot_flags(v - np.outer(frac, slope))

recs = []
for y in range(1981, 2026):
    for s, ms in WIN.items():
        sel_t = (v.index.year == y) & np.isin(v.index.month, ms)
        c_raw, c_det = hot[sel_t].sum().values, hot_d[sel_t].sum().values
        for iso in NAMES:
            sel = (cells.iso3.values == iso) & ((cells[f"share_{s}"].values >= 0.25) if s != "FM" else True)
            if sel.sum() < 10:
                continue
            w = cells.weight.values[sel]
            recs.append(dict(iso3=iso, season=s, year=y, hot=np.average(c_raw[sel], weights=w), hot_d=np.average(c_det[sel], weights=w)))
H = pd.DataFrame(recs)
mine_csv = pd.read_csv(OUT / "hot_days.csv").merge(H, on=["iso3", "season", "year"])
r_raw, r_det = mine_csv.hot.corr(mine_csv.hot_days), mine_csv.hot_d.corr(mine_csv.hot_days_detr)
check("Season counts: nearest point and interpolation agree (r > 0.97)", r_raw > 0.97 and r_det > 0.97, f"r {r_raw:.3f}, {r_det:.3f}")
tr = H.groupby(["iso3", "season"]).apply(lambda g: (np.polyfit(g.year, g.hot, 1)[0] * 10, np.polyfit(g.year, g.hot_d, 1)[0] * 10))
print("       trend in very hot days per decade, before -> after taking out the warming: "
      + "; ".join(f"{i} {s} {a:+.1f} -> {b:+.1f}" for (i, s), (a, b) in tr.items()))

# ---- phases from the raw NOAA files
nino, dmi = {}, {}
for line in (DATA / "nino34_ersst5.txt").read_text().splitlines()[1:]:
    p = line.split()
    nino[(int(p[0]), int(p[1]))] = float(p[9])
for line in (DATA / "dmi_had.txt").read_text().splitlines():
    p = line.split()
    if len(p) == 13 and p[0].isdigit():
        for m in range(12):
            if float(p[m + 1]) > -90:
                dmi[(int(p[0]), m + 1)] = float(p[m + 1])
H["n34"] = [np.mean([nino[(r.year, m)] for m in WIN[r.season]]) for r in H.itertuples()]
H["dmi"] = [np.mean([dmi[(r.year, m)] for m in WIN[r.season]]) for r in H.itertuples()]
seas = pd.read_csv(OUT / "seasons.csv")[["iso3", "season", "year", "rain_pct"]]
H = H.merge(seas, on=["iso3", "season", "year"], how="left")

# the eight driest and eight wettest of each country season, by rain
dry8 = {k_: set(q.nsmallest(8, "rain_pct").year) for k_, q in H[H.season != "FM"].dropna(subset=["rain_pct"]).groupby(["iso3", "season"])}
wet8 = {k_: set(q.nlargest(8, "rain_pct").year) for k_, q in H[H.season != "FM"].dropna(subset=["rain_pct"]).groupby(["iso3", "season"])}

# a small trend is left in the counts (fitting the warming per month or as a curve leaves the same); taking that
# out as well must not move any group average to the other side of usual, nor change whether the driest have more
shift, flips, dw_flip = [], [], []
for (i, s), q in H[H.season != "FM"].dropna(subset=["rain_pct"]).groupby(["iso3", "season"]):
    b = np.polyfit(q.year, q.hot_d, 1)[0]
    q2 = q.hot_d - b * (q.year - q.year.mean())
    usual = q[q.year.between(1991, 2020)].hot_d.mean()
    d_, w_ = q.year.isin(dry8[i, s]), q.year.isin(wet8[i, s])
    groups = [("pos IOD", q.dmi >= 0.4), ("neg IOD", q.dmi <= -0.4), ("El Niño", q.n34 >= 0.5), ("La Niña", q.n34 <= -0.5),
              ("neutral ENSO", q.n34.abs() < 0.5), ("eight driest", d_), ("eight wettest", w_)]
    for name, msk in groups:
        if msk.any():
            a_, b_ = q.hot_d[msk].mean(), q2[msk].mean()
            shift.append((abs(a_ - b_), f"{i} {s} {name}"))
            if abs(a_ - usual) >= 1 and np.sign(a_ - usual) != np.sign(b_ - usual):
                flips.append(f"{i} {s} {name}")
    if (q.hot_d[d_].mean() > q.hot_d[w_].mean()) != (q2[d_].mean() > q2[w_].mean()):
        dw_flip.append(f"{i} {s}")
w_ = max(shift)
check("Trend left in the counts flips no phase or dry/wet average across usual", not flips and not dw_flip,
      f"largest shift {w_[0]:.1f} days ({w_[1]}); flips: {flips + dw_flip or 'none'}")

# ---- the page text
t = html.unescape(re.sub(r"\s+", " ", re.sub(r"<[^>]+>", " ", PAGE.read_text(encoding="utf8"))))


def grab(pattern):
    m = re.search(pattern, t)
    if not m:
        check(f"page has: {pattern[:60]}", False)
        return None
    return [float(x) for x in m.groups()]


g = H.groupby(["iso3", "season"])
early = g.apply(lambda q: q[q.year <= 2000].hot.mean())
late = g.apply(lambda q: q[q.year >= 2006].hot.mean())
n_more = int((late > early).sum())
check("Very hot days: rows with more in 2006-2025 than 1981-2000", f"In all {len(early)} rows of the chart below" in t if n_more == len(early)
      else f"In {n_more} of the {len(early)} rows of the chart below" in t, f"{n_more} of {len(early)}")
p = grab(r"Kenya, October to December, for example: (\d+) a season on average in 1981 to 2000 and (\d+) in 2006 to 2025 \(usual: (\d+)\)")
if p:
    m_ = [early["KEN", "OND"], late["KEN", "OND"], H[(H.iso3 == "KEN") & (H.season == "OND") & H.year.between(1991, 2020)].hot.mean()]
    check("Kenya OND very hot days, then and now", close(p, m_), f"page {p}, mine {[round(x, 1) for x in m_]}")
p = grab(r"February to March, over the whole country \(usual: (\d+) a season\), very hot days on average in 1981 to 2000 and in 2006 to 2025: "
         r"Ethiopia (\d+) and (\d+), Kenya (\d+) and (\d+), Somalia (\d+) and (\d+)")
if p:
    m_ = [H[(H.iso3 == "KEN") & (H.season == "FM") & H.year.between(1991, 2020)].hot.mean()] + [x for i in NAMES for x in (early[i, "FM"], late[i, "FM"])]
    check("February to March very hot days, then and now", close(p, m_), f"page {p}, mine {[round(x, 1) for x in m_]}")

dw = {}
for (i, s), q in H[H.season != "FM"].dropna(subset=["rain_pct"]).groupby(["iso3", "season"]):
    dw[(i, s)] = (q[q.year.isin(dry8[i, s])].hot_d.mean(), q[q.year.isin(wet8[i, s])].hot_d.mean(), q[q.year.between(1991, 2020)].hot_d.mean())
n_dry = sum(a > b for a, b, _ in dw.values())
check("Eight driest against eight wettest: count of country seasons",
      f"had more very hot days than the eight wettest in {n_dry} of the {len(dw)} country seasons" in t, f"{n_dry} of {len(dw)}")
p = grab(r"Kenya, October to December: (\d+) on average in the eight driest, (\d+) in the eight wettest \(usual: (\d+)\)")
if p:
    check("Kenya OND very hot days, eight driest and eight wettest", close(p, list(dw["KEN", "OND"])), f"page {p}, mine {[round(x, 1) for x in dw['KEN', 'OND']]}")


def phase_mean(s, kind, i):
    q = H[(H.season == s) & (H.iso3 == i)]
    msk = {"pos_iod": q.dmi >= 0.4, "nino": q.n34 >= 0.5, "nina": q.n34 <= -0.5, "neutral": q.n34.abs() < 0.5}[kind]
    return q[msk].hot_d.mean()


def usual(s):
    return H[(H.season == s) & (H.iso3 == "KEN") & H.year.between(1991, 2020)].hot_d.mean()


for s, kinds in [("OND", [("El Niño", "nino"), ("neutral", "neutral"), ("La Niña", "nina"), ("positive IOD", "pos_iod")]),
                 ("JJAS", [("El Niño", "nino"), ("neutral", "neutral"), ("La Niña", "nina")])]:
    lead_ = "October to December \\(usual: (\\d+) a season\\): " if s == "OND" else "June to September \\(usual: (\\d+)\\): "
    pat = lead_ + " \\| ".join(f"{n}: " + ", ".join(f"{lab} (\\d+)" for lab, _ in kinds) for n in ["Ethiopia", "Kenya", "Somalia"])
    p = grab(pat)
    if p:
        m_ = [usual(s)] + [phase_mean(s, k_, i) for i in NAMES for _, k_ in kinds]
        check(f"Very hot days by ENSO and IOD phase, {s}", close(p, m_), f"page {p}, mine {[round(x, 1) for x in m_]}")

# ---- the plain ENSO/IOD sentences that are counts
o = H[(H.season == "OND") & (H.iso3 == "KEN")]
pos = o[o.dmi >= 0.4]
n_nino = int((o.n34 >= 0.5).sum())
both_txt = (f"All {len(pos)} positive-IOD seasons were also El Niño seasons ({', '.join(map(str, pos.year))}). The other {n_nino - len(pos)} "
            f"El Niño seasons had no positive IOD." if (pos.n34 >= 0.5).all() else f"{int((pos.n34 >= 0.5).sum())} of the {len(pos)}")
check("Positive-IOD seasons that were also El Niño", both_txt in t, both_txt)
p = grab(r"Most October to December seasons had less rain than usual: (\d+) of 45 in Ethiopia, (\d+) of 45 in Kenya, (\d+) of 45 in Somalia")
if p:
    m_ = [float(((H.iso3 == i) & (H.season == "OND") & (H.rain_pct < 100)).sum()) for i in NAMES]
    check("October to December seasons below usual", p == m_ and all(x > 22.5 for x in m_), f"page {p}, mine {m_}")
o = H[H.season == "OND"].copy()
o["as_"] = [np.mean([dmi[(y, 8)], dmi[(y, 9)]]) for y in o.year]
k = o[o.iso3 == "KEN"]
py, ny = k[k.as_ >= 0.4].year.tolist(), k[k.as_ <= -0.4].year.tolist()
above = [int(((o.iso3 == i) & (o.as_ >= 0.4) & (o.rain_pct > 100)).sum()) for i in NAMES]
below = [int(((o.iso3 == i) & (o.as_ <= -0.4) & (o.rain_pct < 100)).sum()) for i in NAMES]
want = (f"In the {len(py)} years when it was positive ({', '.join(map(str, py))}), October to December rain was above usual in "
        f"{above[0]} of them in Ethiopia, {above[1]} in Kenya and {above[2]} in Somalia. In the {len(ny)} years when it was negative "
        f"({', '.join(map(str, ny))}), rain was below usual in {below[0]} of them in Ethiopia, {below[1]} in Kenya and {below[2]} in Somalia.")
check("August-September IOD and the October to December rains", want in t, want)
j = H[(H.season == "JJAS") & (H.iso3 == "ETH") & (H.n34 >= 0.5)]
want = f"June to September, Ethiopia: in all {len(j)} El Niño seasons rain was below usual (median {j.rain_pct.median():.0f}%)"
check("Ethiopia JJAS El Niño seasons all below usual", (j.rain_pct < 100).all() and want in t, want)

print("PROBLEMS:", ", ".join(problems) if problems else "none")

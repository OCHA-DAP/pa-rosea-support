"""Recompute the page's counts a second way and compare with backtest.py.

backtest.py averages the county means and then sums over the window. This script works from the
saved pixel grids instead: running totals per pixel, then the area-weighted average (county
reading), and pandas rolling sums per pixel, then the maximum (single-cell reading). Season:
October to December totals and floods only, as in backtest.py. EM-DAT dates
and county matching are redone here from the raw file. Prints one OK/BAD line per trigger and
"PROBLEMS: none" when everything matches.

Run: python flood/ken/independent_check.py [data_dir]
"""
import calendar
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from extract_imerg_counties import grid_and_weights  # noqa: E402

D = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
EWASO = ["Isiolo", "Samburu", "Nyeri", "Nyandarua", "Laikipia", "Meru"]
TANA = ["Nyeri", "Kirinyaga", "Murang'a", "Embu", "Meru", "Tharaka-Nithi", "Nyandarua"]
TRIG = {"krcs_mandera": (["Mandera"], 7, 150, ["mandera"]), "krcs_wajir": (["Wajir"], 7, 150, ["wajir"]),
        "krcs_marsabit": (["Marsabit"], 7, 150, ["marsabit"]), "whh_70": (EWASO, 7, 70, ["isiolo", "samburu"]),
        "whh_100": (EWASO, 7, 100, ["isiolo", "samburu"]), "krcs_garissa": (TANA, 1, 40, ["garissa", "tanariver", "dadaab"]),
        "whh_isiolo_70": (["Isiolo"], 7, 70, ["isiolo"]), "whh_isiolo_100": (["Isiolo"], 7, 100, ["isiolo"]),
        "whh_samburu_70": (["Samburu"], 7, 70, ["samburu"]), "whh_samburu_100": (["Samburu"], 7, 100, ["samburu"])}

# pixel grids
dates, grids = [], []
for f in sorted((D / "years").glob("imerg_ken_grid_*.npz")):
    z = np.load(f)
    dates += list(z["dates"])
    grids.append(z["grid"].reshape(len(z["dates"]), -1))
G = np.nan_to_num(np.concatenate(grids)).astype("float64")
idx = pd.DatetimeIndex(pd.to_datetime(dates))
assert idx.to_series().diff().dropna().dt.days.eq(1).all(), "gap in the grids"
_, _, _, W, _, codes, names = grid_and_weights(dates[-1])
row = {names[c]: i for i, c in enumerate(codes)}
last_full = idx.max().year - 1
n = last_full - idx.min().year + 1

# EM-DAT, dated here
em = pd.read_parquet(D / "emdat_ken_floods.parquet")
em = em[em["Start Year"] >= idx.min().year].copy()
def _end(r):
    y = int(r["End Year"])
    m = int(r["End Month"]) if pd.notna(r["End Month"]) else (int(r["Start Month"]) if y == int(r["Start Year"]) else 1)
    return pd.Timestamp(y, m, int(r["End Day"]) if pd.notna(r["End Day"]) and pd.notna(r["End Month"]) else calendar.monthrange(y, m)[1])
em["s"] = [pd.Timestamp(int(r["Start Year"]), int(r["Start Month"]), int(r["Start Day"]) if pd.notna(r["Start Day"]) else 1) for _, r in em.iterrows()]
em["e"] = [_end(r) for _, r in em.iterrows()]
em["loc"] = em["Location"].fillna("").str.lower().str.replace(" ", "")
em = em[[d.month in (10, 11, 12) for d in em["s"]]]  # October to December floods only

CS = np.cumsum(G, axis=0)
RUN = {}
for win in (1, 7):  # running totals per pixel (county-average route)
    RUN[win] = CS.copy()
    RUN[win][win:] = CS[win:] - CS[:-win]
del CS
_, _, _, _, C, _, _ = grid_and_weights(dates[-1])


def series(reading, counties, win):
    if reading == "county":  # per-pixel running totals, then the area-weighted average, equal weight per county
        w = W[[row[c] for c in counties]].sum(0) / len(counties)
        return pd.Series(RUN[win] @ w, index=idx).iloc[win - 1:]
    inside = C[[row[c] for c in counties]].sum(0) >= 0.5  # single cell: pandas rolling sum per pixel, then the maximum
    return pd.DataFrame(G[:, inside], index=idx).rolling(win).sum().max(axis=1).iloc[win - 1:]


def count(s, thr, ev):
    acts, start = [], None
    for d in s.index[(s.values >= thr) & s.index.month.isin([10, 11, 12])]:
        if start is None or (d - start).days > 30:
            acts.append(d)
            start = d
    A = pd.DatetimeIndex(acts)
    yrs = {d.year for d in acts if d.year <= last_full}
    return acts, yrs, int(sum(((A >= r.s - pd.Timedelta(days=30)) & (A <= r.e)).any() for r in ev.itertuples()))


summ = pd.read_csv(D / "out" / "summary.csv").set_index(["reading", "trigger"])
ov = pd.read_csv(D / "out" / "overall.csv").set_index(["reading", "group"])
cty = pd.read_csv(D / "out" / "counties.csv").set_index(["reading", "county", "window_days", "threshold_mm"])
problems = []
for reading in ("county", "cell"):
    years_any, years_ne = set(), set()
    for k, (counties, win, thr, keys) in TRIG.items():
        ev = em[em["loc"].apply(lambda t: any(x in t for x in keys))]
        acts, yrs, reached = count(series(reading, counties, win), thr, ev)
        if k in ("krcs_mandera", "krcs_wajir", "krcs_marsabit", "whh_70", "krcs_garissa"):  # the triggers as written
            years_any |= yrs
        if k in ("krcs_mandera", "krcs_wajir", "krcs_marsabit"):
            years_ne |= yrs
        mine = (len(acts), len(yrs), len(ev), reached)
        p = summ.loc[(reading, k)]
        theirs = (int(p.activations), int(p.years_reached), int(p.floods), int(p.floods_reached))
        ok = mine == theirs
        print(f"{'OK ' if ok else 'BAD'} {reading:6s} {k:14s} activations, seasons, floods, floods reached: here {mine} | backtest {theirs}")
        problems += [] if ok else [f"{reading} {k}"]
    for g, yrs in [("krcs_ne", years_ne), ("all", years_any)]:
        ok = len(yrs) == int(ov.loc[(reading, g)].years_reached)
        print(f"{'OK ' if ok else 'BAD'} {reading:6s} {g:14s} seasons: here {len(yrs)} | backtest {int(ov.loc[(reading, g)].years_reached)}")
        problems += [] if ok else [f"{reading} {g}"]
    bad = []
    for (rd, county, win, thr), p in cty.loc[[reading]].iterrows():
        ev = em[em["loc"].str.contains(county.lower().replace(" ", ""), regex=False)]
        _, yrs, reached = count(series(reading, [county], win), thr, ev)
        mine = (len(yrs), len(ev), reached)
        if mine != (int(p.years_reached), int(p.floods), int(p.floods_reached)):
            bad.append((county, win, thr, mine))
    print(f"{'OK ' if not bad else 'BAD'} {reading:6s} counties       {len(cty.loc[[reading]])} cells checked, {len(bad)} differ {bad[:3]}")
    problems += [f"{reading} county {b[0]} {b[2]}mm" for b in bad]
# season-by-season table: highest October to December total per trigger and season
sx = pd.read_csv(D / "out" / "season_max.csv").set_index(["reading", "trigger", "year"])
worst, bad_met = 0.0, []
for reading in ("county", "cell"):
    for k, (counties, win, thr, _) in TRIG.items():
        s = series(reading, counties, win)
        s = s[s.index.month.isin([10, 11, 12]) & (s.index.year <= last_full)]
        for y, v in s.groupby(s.index.year).max().items():
            p = sx.loc[(reading, k, y)]
            worst = max(worst, abs(float(p.max_mm) - v))
            if bool(p.met) != bool(v >= thr):
                bad_met.append((reading, k, y))
ok = worst <= 0.06 and not bad_met
print(f"{'OK ' if ok else 'BAD'} season table   {len(sx)} values, largest difference {worst:.3f} mm (rounding), {len(bad_met)} highlight differences {bad_met[:3]}")
problems += [] if ok else ["season table"]
print(f"record {idx.min().date()} to {idx.max().date()}, {n} seasons | PROBLEMS: {problems or 'none'}")

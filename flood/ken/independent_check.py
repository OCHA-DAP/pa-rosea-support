"""Recompute the page's counts a second way and compare with backtest.py.

backtest.py averages the county means and then sums over the window. This script works from the
saved pixel grids instead: running totals per pixel, then the area-weighted average. EM-DAT dates
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
        "whh_100": (EWASO, 7, 100, ["isiolo", "samburu"]), "krcs_garissa": (TANA, 1, 40, ["garissa", "tanariver", "dadaab"])}

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

CS = np.cumsum(G, axis=0)
RUN = {}
for win in (1, 7):  # running totals per pixel
    RUN[win] = CS.copy()
    RUN[win][win:] = CS[win:] - CS[:-win]

summ = pd.read_csv(D / "out" / "summary.csv").set_index("trigger")
ov = pd.read_csv(D / "out" / "overall.csv").set_index("group")
problems, years_any, years_ne = [], set(), set()
for k, (counties, win, thr, keys) in TRIG.items():
    w = W[[row[c] for c in counties]].sum(0) / len(counties)          # equal weight per county
    R = RUN[win]
    s = pd.Series(R @ w, index=idx).iloc[win - 1:]                    # per-pixel running totals, then average
    acts, start = [], None
    for d in s.index[s.values >= thr]:
        if start is None or (d - start).days > 30:
            acts.append(d)
            start = d
    A = pd.DatetimeIndex(acts)
    yrs = {d.year for d in acts if d.year <= last_full}
    if k != "whh_100":
        years_any |= yrs
    if k.startswith("krcs_m") or k == "krcs_wajir":
        years_ne |= yrs
    ev = em[em["loc"].apply(lambda t: any(x in t for x in keys))]
    reached = sum(((A >= r.s - pd.Timedelta(days=30)) & (A <= r.e)).any() for r in ev.itertuples())
    mine = (len(acts), len(yrs), len(ev), int(reached))
    p = summ.loc[k]
    theirs = (int(p.activations), int(p.years_reached), int(p.floods), int(p.floods_reached))
    ok = mine == theirs
    print(f"{'OK ' if ok else 'BAD'} {k:14s} activations, years, floods, floods reached: here {mine} | backtest {theirs}")
    problems += [] if ok else [k]
for g, yrs in [("krcs_ne", years_ne), ("all", years_any)]:
    ok = len(yrs) == int(ov.loc[g].years_reached)
    print(f"{'OK ' if ok else 'BAD'} {g:14s} years: here {len(yrs)} | backtest {int(ov.loc[g].years_reached)}")
    problems += [] if ok else [g]
print(f"record {idx.min().date()} to {idx.max().date()}, {n} full years | PROBLEMS: {problems or 'none'}")

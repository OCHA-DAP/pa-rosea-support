"""Independent recomputation of the page's headline numbers.

Uses a different path from the pipeline: the per-county daily means (imerg_ken_adm1_daily.parquet),
averaged over counties, rolled, and compared with activation_summary.csv / overall_return_periods.csv.
EM-DAT is re-read from the raw extract and dated here without the pipeline's helper.
"""
import calendar
import sys
from pathlib import Path

import numpy as np
import pandas as pd

D = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
c = pd.read_parquet(D / "imerg_ken_adm1_daily.parquet")
c["date"] = pd.to_datetime(c["date"])
w = c.pivot(index="date", columns="ADM1_EN", values="mean_mm").sort_index()
assert w.index.is_monotonic_increasing and w.index.to_series().diff().dropna().dt.days.eq(1).all(), "gaps in daily record"
assert not w.isna().any().any(), "NaN county-days"

AREA = {"Mandera": ["Mandera"], "Wajir": ["Wajir"], "Marsabit": ["Marsabit"],
        "Ewaso": ["Isiolo", "Samburu", "Nyeri", "Nyandarua", "Laikipia", "Meru"],
        "UpperTana": ["Nyeri", "Kirinyaga", "Murang'a", "Embu", "Meru", "Tharaka-Nithi", "Nyandarua"]}
TRIG = {"krcs_mandera": ("Mandera", 7, 150, ["mandera"]), "krcs_wajir": ("Wajir", 7, 150, ["wajir"]),
        "krcs_marsabit": ("Marsabit", 7, 150, ["marsabit"]), "whh_70": ("Ewaso", 7, 70, ["isiolo", "samburu"]),
        "whh_100": ("Ewaso", 7, 100, ["isiolo", "samburu"]),
        "krcs_garissa": ("UpperTana", 1, 40, ["garissa", "tanariver", "dadaab"])}

em = pd.read_parquet(D / "emdat_ken_floods.parquet")
em = em[em["Start Year"] >= 1998].copy()
def d0(r): return pd.Timestamp(int(r["Start Year"]), int(r["Start Month"]), int(r["Start Day"]) if pd.notna(r["Start Day"]) else 1)
def d1(r):
    y = int(r["End Year"])
    m = int(r["End Month"]) if pd.notna(r["End Month"]) else (int(r["Start Month"]) if y == int(r["Start Year"]) else 1)
    d = int(r["End Day"]) if pd.notna(r["End Day"]) and pd.notna(r["End Month"]) else calendar.monthrange(y, m)[1]
    return pd.Timestamp(y, m, d)
em["s"] = em.apply(d0, axis=1); em["e"] = em.apply(d1, axis=1)
em["loc"] = em["Location"].fillna("").str.lower().str.replace(" ", "")

summ = pd.read_csv(D / "out" / "activation_summary.csv").set_index(["trigger", "level"])
problems = []
years_any = set()
for k, (area, win, thr, keys) in TRIG.items():
    s = w[AREA[area]].mean(axis=1).rolling(win).sum()
    # activations: first day >= thr, 30-day cooldown
    acts, last = [], None
    for d in s[s >= thr].index:
        if last is None or (d - last).days > 30:
            acts.append(d); last = d
    yrs = sorted({d.year for d in acts if d.year <= 2025})
    if k != "whh_100":
        years_any |= set(yrs)
    ev = em[em["loc"].apply(lambda t: any(x in t for x in keys))]
    A = pd.DatetimeIndex(acts)
    caught = sum(((A >= r.s - pd.Timedelta(days=30)) & (A <= r.e)).any() for r in ev.itertuples())
    p = summ.loc[(k, "as written")]
    mine = dict(activations=len(acts), years=len(yrs), rp=round(29 / len(yrs), 1) if yrs else None,
                floods=len(ev), caught=int(caught))
    theirs = dict(activations=int(p.activations), years=int(p.years_activated), rp=p.rp_years,
                  floods=int(p.floods_in_record), caught=int(p.floods_caught))
    ok = mine == theirs
    print(f"{'OK ' if ok else 'BAD'} {k:14s} independent {mine}  pipeline {theirs}")
    print(f"      years: {' '.join(map(str, yrs))}")
    if k in ("krcs_mandera", "krcs_wajir", "krcs_marsabit"):
        print("      dates:", ", ".join(f"{d.date()} ({s[d]:.0f} mm, peak {s[d:d + pd.Timedelta(days=30)].max():.0f})" for d in acts))
    if not ok:
        problems.append(k)

ov = pd.read_csv(D / "out" / "overall_return_periods.csv")
o = ov[(ov.group == "all") & (ov.level == "as written")].iloc[0]
print(f"{'OK ' if len(years_any) == o.years_activated else 'BAD'} any trigger: independent {len(years_any)} years, pipeline {o.years_activated}")
print("full years in record:", w.index.year.min(), "-", w.index.year.max() - 1, "| last day", w.index.max().date())
print("EM-DAT last end date:", em["e"].max().date())
print("PROBLEMS:", problems or "none")

"""Second-way recalculation of the numbers on the research page.

Run: python climate/hoa/research_check.py [data_dir]   (after build_research_page.py)
Recomputes from the raw inputs by different routes (plain loops instead of vectorised code, nearest ERA5 point
instead of interpolation, rank formula instead of scikit-learn) and checks the page's numbers.
Prints one line per check and "PROBLEMS: none" when all agree within tolerance.
"""
import html
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr
from scipy import stats

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
PAGE = Path(__file__).parent / "hoa_research.html"
txt = html.unescape(re.sub(r"<[^>]+>", " ", re.sub(r"<script.*?</script>|<style.*?</style>", "", PAGE.read_text(encoding="utf8"), flags=re.S)))
txt = re.sub(r"\s+", " ", txt)
problems = []
SM = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}


def check(name, ok, detail=""):
    print(("[PASS] " if ok else "[FAIL] ") + name + (f" | {detail}" if detail else ""))
    if not ok:
        problems.append(name)


def auc_rank(y, s):
    """Mann-Whitney form of ROC AUC."""
    y, s = np.asarray(y, bool), np.asarray(s, float)
    r = stats.rankdata(s)
    n1, n0 = y.sum(), (~y).sum()
    return (r[y].sum() - n1 * (n1 + 1) / 2) / (n1 * n0)


T = pd.read_csv(OUT / "rq_table.csv")
cells = pd.read_parquet(OUT / "cells.parquet")

# ---- daily metrics: onset and heaviest 3 days for a few seasons, plain loops on the raw daily files
dm = pd.read_csv(OUT / "daily_metrics.csv")
for c, s, y in [("KEN", "MAM", 2011), ("SOM", "OND", 2019), ("ETH", "JJAS", 2015)]:
    g = cells[(cells.iso3 == c) & (cells[f"share_{s}"] >= 0.25)].reset_index(drop=True)
    days = {}
    for yy in range(1991, 2021):
        days[yy] = xr.open_dataarray(DATA / "chirps_daily" / f"{yy}.nc")
    cur = xr.open_dataarray(DATA / "chirps_daily" / f"{y}.nc")
    st = pd.Timestamp(y, SM[s][0], 1); en = pd.Timestamp(y, SM[s][-1], 1) + pd.offsets.MonthEnd(0)
    m3s, ons, ws = [], [], []  # the 30-day lead stays inside the same year: every season starts in March or later
    for r in g.itertuples():
        col = cur.sel(latitude=r.lat, longitude=r.lon, method="nearest").to_series()
        win = col[st - pd.Timedelta(days=30):en]
        if win.isna().any():
            continue
        clim = pd.concat([days[yy].sel(latitude=r.lat, longitude=r.lon, method="nearest").to_series()
                          [pd.Timestamp(yy, SM[s][0], 1) - pd.Timedelta(days=30):pd.Timestamp(yy, SM[s][-1], 1) + pd.offsets.MonthEnd(0)] for yy in days])
        acc = (win - clim.mean()).cumsum().values
        i = int(np.argmin(acc))
        ons.append(i + 1 - 30 if i < len(acc) - 1 else len(acc) - 1 - 30)
        season = col[st:en].values
        m3s.append(max(season[k:k + 3].sum() for k in range(len(season) - 2)))
        ws.append(r.weight)
    q = dm[(dm.iso3 == c) & (dm.season == s) & (dm.year == y)].iloc[0]
    check(f"Onset {c} {s} {y}: recomputed within 0.5 day", abs(np.average(ons, weights=ws) - q.onset_day) < 0.5, f"{q.onset_day:.2f} vs {np.average(ons, weights=ws):.2f}")
    check(f"Heaviest 3 days {c} {s} {y}: within 0.1 mm", abs(np.average(m3s, weights=ws) - q.max3_mm) < 0.1, f"{q.max3_mm:.2f} vs {np.average(m3s, weights=ws):.2f}")

# ---- FAOSTAT departure from trend
fao = pd.read_csv(OUT / "faostat_cereals.csv")
k = fao[(fao.Area == "Kenya") & (fao.Year >= 1981)].sort_values("Year")
b = np.polyfit(k.Year.values, np.log(k.Value.values), 2)  # quadratic trend on log production
dev = pd.Series((np.exp(np.log(k.Value.values) - np.polyval(b, k.Year.values)) - 1) * 100, index=k.Year.values)
t = T[(T.iso3 == "KEN") & (T.season == "MAM")].set_index("year").prod_dev_pct
check("FAOSTAT Kenya departure from trend matches (March to May, same year)", np.allclose(dev.reindex(t.index).dropna().values, t.dropna().values, atol=1e-6))
t2 = T[(T.iso3 == "KEN") & (T.season == "OND")].set_index("year").prod_dev_pct
check("FAOSTAT October to December takes the next year's production", np.allclose(dev.reindex(t2.index + 1).dropna().values, t2.dropna().values, atol=1e-6))
check("FAOSTAT Ethiopia starts in 1993", int(fao[fao.Area == "Ethiopia"].Year.min()) == 1993)

# ---- Q1 pooled counts, recomputed
MAIN = {"ETH": ["MAM", "JJAS", "OND"], "KEN": ["MAM", "OND"], "SOM": ["MAM", "OND"]}
hot_e = hot_n = oth_e = oth_n = 0
for c, ss in MAIN.items():
    for s in ss:
        g = T[(T.iso3 == c) & (T.season == s)]
        f = g[g.rain_pct <= g.rain_pct.quantile(1 / 3)]
        hot = f.tmax_detr_c > f.tmax_detr_c.median()
        hot_e += int(f.drought_emdat[hot].sum()); hot_n += int(hot.sum()); oth_e += int(f.drought_emdat[~hot].sum()); oth_n += int((~hot).sum())
check("Q1 EM-DAT counts on the page", f"{hot_e} of {hot_n} were followed by an EM-DAT drought start, against {oth_e} of {oth_n}" in txt, f"{hot_e}/{hot_n} vs {oth_e}/{oth_n}")

# ---- Q2 Kenya whole-country trend, nearest ERA5 point, numpy
aft = xr.open_dataset(DATA / "era5_t2m_afternoon.nc")["t2m"]
arr = aft.values.reshape(540, 5, *aft.shape[1:]).max(1) - 273.15
la, lo = aft["latitude"].values, aft["longitude"].values
kc = cells[cells.iso3 == "KEN"]
ser = np.stack([arr[:, np.abs(la - r.lat).argmin(), np.abs(lo - r.lon).argmin()] for r in kc.itertuples()], 1)
area = (ser * kc.weight.values).sum(1) / kc.weight.sum()
yrs = np.arange(1981, 2026)
tr = np.mean([np.polyfit(yrs, area[m::12], 1)[0] * 10 for m in range(12)])
q2 = pd.read_csv(OUT / "rq2_warming.csv")
page_tr = float(q2[(q2.iso3 == "KEN") & (q2.kind == "all")].trend.iloc[0])
check("Q2 Kenya trend within 0.03 C per decade (nearest point)", abs(tr - page_tr) < 0.03, f"{page_tr:.3f} vs {tr:.3f}")

# ---- Q4 pooled CERF after two failed seasons
q = 0; n2 = 0
for c, order in {"ETH": ["MAM", "JJAS", "OND"], "KEN": ["MAM", "OND"], "SOM": ["MAM", "OND"]}.items():
    g = T[(T.iso3 == c) & T.season.isin(order)].copy()
    g["o"] = g.year * 10 + g.season.map({s: i for i, s in enumerate(order)})
    g = g.sort_values("o")
    fl = g.groupby("season").rain_pct.transform(lambda v: v <= v.quantile(1 / 3)).values
    two = fl & np.r_[False, fl[:-1]]
    h = g[two & g.drought_cerf.notna().values]
    q += int(h.drought_cerf.sum()); n2 += len(h)
check("Q4 CERF after two failed seasons on the page", f"{q} of {n2} seasons that ended a run of two failed seasons" in txt, f"{q} of {n2}")

# ---- Q6 August-September DMI years, parsed again
dmi = {}
for line in (DATA / "dmi_had.txt").read_text().splitlines():
    p = line.split()
    if len(p) == 13 and p[0].isdigit():
        dmi[int(p[0])] = [float(v) for v in p[1:]]
pos = [y for y in range(1981, 2026) if np.mean(dmi[y][7:9]) >= 0.4]
neg = [y for y in range(1981, 2026) if np.mean(dmi[y][7:9]) <= -0.4]
check("Q6 positive-DMI years on the page", " ".join(map(str, pos)) in txt, " ".join(map(str, pos)))
check("Q6 negative-DMI years on the page", " ".join(map(str, neg)) in txt, " ".join(map(str, neg)))
for c in ["ETH", "KEN", "SOM"]:
    o = T[(T.iso3 == c) & (T.season == "OND")].set_index("year").rain_pct
    r = np.corrcoef([np.mean(dmi[y][7:9]) for y in range(1981, 2026)], o.reindex(range(1981, 2026)).values)[0, 1]
    q6 = pd.read_csv(OUT / "rq6_lead.csv")
    page_r = float(q6[(q6.iso3 == c) & (q6.season == "OND") & (q6["index"] == "DMI")].r_before.iloc[0])
    check(f"Q6 {c} Aug-Sep DMI correlation matches", abs(page_r - r) < 0.005, f"{page_r:+.3f} vs {r:+.3f}")

# ---- Q7 AUCs with the rank formula
q7 = pd.read_csv(OUT / "rq7_floods.csv")
for c, s in [("KEN", "OND"), ("SOM", "OND"), ("ETH", "MAM")]:
    g = T[(T.iso3 == c) & (T.season == s)]
    for name, col in [("season total", "rain_pct"), ("heaviest 7 days", "max7_pct")]:
        a = auc_rank(g.flood, g[col])
        p = float(q7[(q7.iso3 == c) & (q7.season == s) & (q7.target == "flood") & (q7.indicator == name)].auc.iloc[0])
        check(f"Q7 {c} {s} {name} AUC matches", abs(a - p) < 1e-6, f"{p:.3f} vs {a:.3f}")

# ---- Q8 next March to May
for c in ["ETH", "KEN", "SOM"]:
    o = T[(T.iso3 == c) & (T.season == "OND")].set_index("year").rain_pct
    m = T[(T.iso3 == c) & (T.season == "MAM")].set_index("year").rain_pct
    yy = [y for y in range(1981, 2025)]
    r = np.corrcoef(o.reindex(yy).values, m.reindex([y + 1 for y in yy]).values)[0, 1]
    s8 = pd.read_csv(OUT / "rq8_summary.csv").set_index("iso3")
    check(f"Q8 {c} OND to next MAM correlation matches", abs(s8.r_mam[c] - r) < 1e-6, f"{s8.r_mam[c]:+.3f} vs {r:+.3f}")

# ---- Q10 Fisher z for Kenya OND DMI
dr = pd.read_csv(OUT / "drivers_seasons.csv")
g = dr[(dr.iso3 == "KEN") & (dr.season == "OND")]
r1 = np.corrcoef(g[g.year <= 2002].dmi, g[g.year <= 2002].rain_pct)[0, 1]; r2 = np.corrcoef(g[g.year >= 2003].dmi, g[g.year >= 2003].rain_pct)[0, 1]
z = (np.arctanh(r2) - np.arctanh(r1)) / np.sqrt(1 / 19 + 1 / 20)
q10 = pd.read_csv(OUT / "rq10_change.csv")
p = q10[(q10.iso3 == "KEN") & (q10.season == "OND") & (q10["index"] == "dmi")].iloc[0]
check("Q10 Kenya OND DMI periods and z match", abs(p.r_1981_2002 - r1) < 1e-6 and abs(p.r_2003_2025 - r2) < 1e-6 and abs(p.z - z) < 1e-6, f"{r1:+.2f}, {r2:+.2f}, z {z:.2f}")

# ---- no em dashes in the page text
check("No em dashes in the page", "—" not in PAGE.read_text(encoding="utf8"))
print("PROBLEMS:", ", ".join(problems) if problems else "none")

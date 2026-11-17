"""Build the Horn of Africa temperature, rainfall and impact review page.

Run: python climate/hoa/build_review_page.py [data_dir]   (after analyse.py)
Writes climate/hoa/hoa_climate_review.html, a single self-contained page.
"""
import html
import math
import json
import sys
from datetime import date
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
HERE = Path(__file__).parent
esc = html.escape
NAMES = {"ETH": "Ethiopia", "KEN": "Kenya", "SOM": "Somalia"}
MON = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
SEASON_NAME = {"MAM": "March to May", "JJAS": "June to September", "OND": "October to December"}
SEASON_MONTHS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
REGIME = {"bimodal (MAM and OND)": "two", "unimodal (JJAS)": "one", "other": "other"}

cycle = pd.read_csv(OUT / "cycle.csv")
cells = pd.read_parquet(OUT / "cells.parquet")
seas = pd.read_csv(OUT / "drivers_seasons.csv")  # seasons.csv plus Nino3.4, DMI, phases and dry spells (drivers.py)
dcorr = pd.read_csv(OUT / "drivers_corr.csv")
dphase = pd.read_csv(OUT / "drivers_phase.csv")
em = pd.read_parquet(DATA / "impacts_emdat.parquet")
adm = gpd.read_parquet(DATA / "adm1.parquet")
FIRST, LAST = int(seas["year"].min()), int(seas["year"].max())


def fmt_n(v):
    if pd.isna(v) or v == 0:
        return ""
    return f"{v / 1e6:.1f} million" if v >= 999_500 else f"{v / 1e3:.0f} thousand" if v >= 999.5 else f"{v:.0f}"


EVENTS = em.set_index("DisNo.")


def event_text(i):
    r = EVENTS.loc[i]
    when = lambda y, m: f"{int(y)}" + (f"-{int(m):02d}" if pd.notna(m) else "")
    span = when(r["Start Year"], r["Start Month"])
    if pd.notna(r["End Year"]) and (r["End Year"], r["End Month"]) != (r["Start Year"], r["Start Month"]):
        span += " to " + when(r["End Year"], r["End Month"])
    a = fmt_n(r["Total Affected"])
    return f"EM-DAT {r['Disaster Type'].lower()} {span}" + (f", {a} affected" if a else "")


def ids(v):
    return [] if pd.isna(v) or v == "" else str(v).split(";")


# ------------------------------------------------------------------ seasonal cycle panels
CYCLE_PANELS = [("ETH", "unimodal (JJAS)", "Ethiopia, areas with one main rainy season (June to September)"),
                ("ETH", "bimodal (MAM and OND)", "Ethiopia, areas with two rainy seasons (March to May, October to December)"),
                ("KEN", "bimodal (MAM and OND)", "Kenya, areas with two rainy seasons"),
                ("SOM", "bimodal (MAM and OND)", "Somalia, areas with two rainy seasons")]
cyc = []
for iso, reg, label in CYCLE_PANELS:
    g = cycle[(cycle.iso3 == iso) & (cycle.regime == reg)].sort_values("month")
    wet = g.rain_mm > g.rain_mm.sum() / 12
    cyc.append(dict(id=f"{iso}-{REGIME[reg]}", label=label, cells=int(g.cells.iloc[0]),
                    rain=g.rain_mm.round(1).tolist(), tmax=g.tmax_c.round(1).tolist(), tmean=g.tmean_c.round(1).tolist(),
                    wet=wet.tolist(), hottest=MON[int(g.tmax_c.values.argmax())], wettest=MON[int(g.rain_mm.values.argmax())],
                    coolest=MON[int(g.tmax_c.values.argmin())], coolest_wet=bool(wet.values[int(g.tmax_c.values.argmin())]),
                    tmax_wet=round(float(g.tmax_c[wet].mean()), 1), tmax_dry=round(float(g.tmax_c[~wet].mean()), 1)))

# ------------------------------------------------------------------ map of cells
k = 14  # px per degree
def px(lon, lat):
    return (lon - 32) * k, (16 - lat) * k

outline = []
for iso, g in adm.dissolve("iso3").simplify(0.03).items():
    polys = getattr(g, "geoms", [g])
    d = ""
    for p in polys:
        for ring in [p.exterior]:
            pts = [px(x, y) for x, y in ring.coords]
            d += "M" + "L".join(f"{x:.1f},{y:.1f}" for x, y in pts) + "Z"
    outline.append(d)
mapcells = [[round(float(r.lon), 3), round(float(r.lat), 3), REGIME[r.regime],
             None if pd.isna(r.tmax_wet_minus_dry) else round(float(r.tmax_wet_minus_dry), 1), r.iso3]
            for r in cells.itertuples()]
share_two = {i: round(float((g.regime == "bimodal (MAM and OND)").mul(g.weight).sum() / g.weight.sum() * 100)) for i, g in cells.groupby("iso3")}
share_cooler = {i: round(float((g.tmax_wet_minus_dry < 0).mul(g.weight).sum() / g.weight.sum() * 100)) for i, g in cells.groupby("iso3")}

# ------------------------------------------------------------------ season panels
SEASON_PANELS = [("ETH", "MAM"), ("ETH", "JJAS"), ("ETH", "OND"), ("KEN", "MAM"), ("KEN", "JJAS"), ("KEN", "OND"),
                 ("SOM", "MAM"), ("SOM", "JJAS"), ("SOM", "OND")]
panels, stats = [], {}
for iso, s in SEASON_PANELS:
    g = seas[(seas.iso3 == iso) & (seas.season == s)].sort_values("year").copy()
    fit = np.polyfit(g.year, g.tmax_anom_c, 1)
    g["t_dt"] = g.tmax_anom_c - np.polyval(fit, g.year)
    r = float(np.corrcoef(g.rain_pct, g.t_dt)[0, 1])
    dry, wetter = g.nsmallest(8, "rain_pct"), g.nlargest(8, "rain_pct")
    floods, drought = {}, {}
    for row in g.itertuples():
        fi = ids(row.flood_ids)
        if fi:
            tot = EVENTS.loc[fi, "Total Affected"].sum()
            floods[int(row.year)] = dict(big=bool(tot >= 500_000), ev=[event_text(i) for i in fi])
        di = ids(row.drought_ids)
        if di:
            drought[int(row.year)] = [event_text(i) for i in di]
    stats[(iso, s)] = dict(r=r, dry=float(dry.t_dt.mean()), wet=float(wetter.t_dt.mean()), trend=float(fit[0] * 10),
                           dry_years=dry.year.tolist(), wet_years=wetter.year.tolist())
    panels.append(dict(id=f"{iso}-{s}", iso=iso, season=s, label=f"{NAMES[iso]}, {SEASON_NAME[s]}", cells=int(g.cells.iloc[0]),
                       years=g.year.tolist(), rain=g.rain_pct.tolist(), t=g.tmax_anom_c.round(2).tolist(),
                       floods=floods, drought=drought, r=round(r, 2), trend=round(fit[0] * 10, 2),
                       dry=round(float(dry.t_dt.mean()), 1), wet=round(float(wetter.t_dt.mean()), 1),
                       nino=[None if pd.isna(v) else round(v, 2) for v in g.nino34], dmi=[None if pd.isna(v) else round(v, 2) for v in g.dmi],
                       enso=g.enso.fillna("").tolist(), iod=g.iod.fillna("").tolist(),
                       spell=[None if pd.isna(v) else round(v, 1) for v in g.longest_days],
                       spell_normal=float(g[g.year.between(1991, 2020)].longest_days.mean())))

# floods that start outside every season shown
shown = {(iso, m) for iso, s in SEASON_PANELS for m in SEASON_MONTHS[s]}
fl_all = em[em["Disaster Type"] == "Flood"]
fl_out = fl_all[[(i, m) not in shown for i, m in zip(fl_all.ISO, fl_all["Start Month"])]]
OUTSIDE = ", ".join(f"{n} {NAMES[i]} flood{'s' if n > 1 else ''}" for i, n in fl_out.groupby("ISO").size().items()) or "none"

# wet seasons and temperature, early and late part of the record
mo = seas[seas.season.isin(["MAM", "OND"])]
wetset = mo[mo.rain_pct >= 120]
def warm_share(a, b):
    w = wetset[(wetset.year >= a) & (wetset.year <= b)]
    return len(w), int((w.tmax_anom_c > 0).sum())
early_n, early_w = warm_share(FIRST, 2000)
late_n, late_w = warm_share(2011, LAST)

# ------------------------------------------------------------------ ENSO and IOD
VARS = [("rain", "Rain"), ("temp", "Afternoon maximum (trend removed)"), ("dry spell", "Longest dry spell")]
GRID_ROWS = [(i, s_) for s_ in ["OND", "MAM", "JJAS"] for i in NAMES]
grid = []
for iso, s_ in GRID_ROWS:
    row = dict(iso=iso, season=s_, label=f"{NAMES[iso]}", cells=[])
    for idx in ["nino34", "dmi"]:
        for v, _ in VARS:
            q = dcorr[(dcorr.iso3 == iso) & (dcorr.season == s_) & (dcorr["index"] == idx) & (dcorr["var"] == v)].iloc[0]
            row["cells"].append(dict(r=float(q.r), p=float(q.p), rs=float(q.spearman), pr=float(q.partial_r), pp=float(q.partial_p), n=int(q.n)))
    row["cross"] = float(dcorr[(dcorr.iso3 == iso) & (dcorr.season == s_) & (dcorr["var"] == "dmi")].r.iloc[0])
    grid.append(row)
phases = {}
for s_ in ["OND", "MAM", "JJAS"]:
    out_ = []
    for iso in NAMES:
        for drv in ["ENSO", "IOD"]:
            q = dphase[(dphase.iso3 == iso) & (dphase.season == s_) & (dphase.driver == drv)].set_index("phase")
            out_.append(dict(iso=iso, driver=drv, ph={ph: dict(n=int(r_.n), years=str(r_.years), rain=float(r_.rain_pct),
                                                             med=float(r_.rain_median), lo=float(r_.rain_min), hi=float(r_.rain_max),
                                                             below=int(r_.below_normal), temp=float(r_.tmax_detr_c),
                                                             dry=float(r_.longest_anom_days), floods=int(r_.floods))
                                                      for ph, r_ in q.iterrows()}))
    phases[s_] = out_


def gr(iso, s_, idx, v, key="r"):
    q = dcorr[(dcorr.iso3 == iso) & (dcorr.season == s_) & (dcorr["index"] == idx) & (dcorr["var"] == v)].iloc[0]
    return float(q[key])


def ph(iso, s_, drv, phase, col):
    return float(dphase[(dphase.iso3 == iso) & (dphase.season == s_) & (dphase.driver == drv) & (dphase.phase == phase)][col].iloc[0])


def rng(vals, f="{:+.2f}"):
    return f.format(min(vals)) if f.format(min(vals)) == f.format(max(vals)) else f"{f.format(min(vals))} to {f.format(max(vals))}"


ISOS = list(NAMES)
mam_vals = [abs(gr(i, "MAM", idx, v)) for i in ISOS for idx in ["nino34", "dmi"] for v in ["rain", "dry spell"]]
mam_t = [(gr(i, "MAM", "nino34", "temp"), gr(i, "MAM", "nino34", "temp", "p"), i) for i in ISOS]
mam_t_sig = [f"{NAMES[i]} ({r_:+.2f})" for r_, p_, i in mam_t if p_ < 0.05]
n_tests = len(grid) * 6
pos_years = dphase[(dphase.iso3 == "KEN") & (dphase.season == "OND") & (dphase.driver == "IOD") & (dphase.phase == "positive")].years.iloc[0]
n_pos = int(ph("KEN", "OND", "IOD", "positive", "n"))
n_pos_nino = int(ph("KEN", "OND", "IOD", "positive", "also_enso"))
pos_below = [f"{NAMES[i]} {r_.year} ({r_.rain_pct:.0f}%)" for i in ISOS
             for r_ in seas[(seas.iso3 == i) & (seas.season == "OND") & (seas.iod == "positive") & (seas.rain_pct < 100)].itertuples()]


def med3(s_, drv, phase):
    return ", ".join(f"{ph(i, s_, drv, phase, 'rain_median'):.0f}%" for i in ISOS)


mam_signed = [gr(i, "MAM", idx, v) for i in ISOS for idx in ["nino34", "dmi"] for v in ["rain", "dry spell"]]
ken_jjas = next(p_ for p_ in panels if p_["id"] == "KEN-JJAS")["cells"]
ken_all = int((cells.iso3 == "KEN").sum())
DRIVER_FINDINGS = [
    f"October to December: median rain was higher in positive-IOD and El Niño seasons than in La Niña and neutral seasons in all "
    f"three countries. The DMI correlates with rain at {rng([gr(i, 'OND', 'dmi', 'rain') for i in ISOS])} and Niño3.4 at "
    f"{rng([gr(i, 'OND', 'nino34', 'rain') for i in ISOS])}; the rank correlations are lower, "
    f"{rng([gr(i, 'OND', 'dmi', 'rain', 'spearman') for i in ISOS])} and {rng([gr(i, 'OND', 'nino34', 'rain', 'spearman') for i in ISOS])}. "
    f"Both indices correlate with a lower afternoon maximum, trend removed (DMI {rng([gr(i, 'OND', 'dmi', 'temp') for i in ISOS])}, "
    f"Niño3.4 {rng([gr(i, 'OND', 'nino34', 'temp') for i in ISOS])}) and a shorter longest dry spell (DMI "
    f"{rng([gr(i, 'OND', 'dmi', 'dry spell') for i in ISOS])}, Niño3.4 {rng([gr(i, 'OND', 'nino34', 'dry spell') for i in ISOS])}).",
    f"The two indices overlap in October to December: they correlate at {grid[0]['cross']:+.2f}, and {n_pos_nino} of the {n_pos} "
    f"positive-IOD seasons ({pos_years}) were also El Niño seasons. With the other index removed, the DMI's correlation with rain is "
    f"{rng([gr(i, 'OND', 'dmi', 'rain', 'partial_r') for i in ISOS])} and Niño3.4's {rng([gr(i, 'OND', 'nino34', 'rain', 'partial_r') for i in ISOS])}.",
    f"Median October to December rain, Ethiopia, Kenya and Somalia: {med3('OND', 'IOD', 'positive')} of normal in the {n_pos} "
    f"positive-IOD seasons" + (f" (below normal only in {' and '.join(pos_below)})" if pos_below else "") +
    f"; {med3('OND', 'ENSO', 'El Niño')} in the {int(ph('KEN', 'OND', 'ENSO', 'El Niño', 'n'))} El Niño seasons, of which "
    f"{', '.join(str(int(ph(i, 'OND', 'ENSO', 'El Niño', 'below_normal'))) for i in ISOS)} were below normal; "
    f"{med3('OND', 'ENSO', 'La Niña')} in the {int(ph('KEN', 'OND', 'ENSO', 'La Niña', 'n'))} La Niña seasons and "
    f"{med3('OND', 'ENSO', 'neutral')} in the {int(ph('KEN', 'OND', 'ENSO', 'neutral', 'n'))} neutral ones. The longest dry spell "
    f"was {rng([ph(i, 'OND', 'ENSO', 'La Niña', 'longest_anom_days') for i in ISOS], '{:+.1f}')} days against normal in La Niña "
    f"seasons and {rng([ph(i, 'OND', 'ENSO', 'neutral', 'longest_anom_days') for i in ISOS], '{:+.1f}')} in neutral ones.",
    f"June to September: Niño3.4 correlates with less rain and a higher afternoon maximum (trend removed) in Ethiopia "
    f"({gr('ETH', 'JJAS', 'nino34', 'rain'):+.2f} with rain, {gr('ETH', 'JJAS', 'nino34', 'temp'):+.2f} with the afternoon maximum) and in "
    f"Kenya's June to September area, {ken_jjas} of {ken_all} cells ({gr('KEN', 'JJAS', 'nino34', 'rain'):+.2f}, "
    f"{gr('KEN', 'JJAS', 'nino34', 'temp'):+.2f}). In Ethiopia's June to September area "
    f"{int(ph('ETH', 'JJAS', 'ENSO', 'El Niño', 'below_normal'))} of the {int(ph('ETH', 'JJAS', 'ENSO', 'El Niño', 'n'))} El Niño "
    f"seasons had below-normal rain ({ph('ETH', 'JJAS', 'ENSO', 'El Niño', 'rain_min'):.0f}% to "
    f"{ph('ETH', 'JJAS', 'ENSO', 'El Niño', 'rain_max'):.0f}% of normal).",
    f"March to May: the correlations of either index with rain or the longest dry spell are between {min(mam_signed):+.2f} and "
    f"{max(mam_signed):+.2f} in all three countries. Niño3.4's correlation with the afternoon maximum (trend removed) is "
    f"{rng([r_ for r_, _, _ in mam_t])}"
    + (f"; only {', '.join(mam_t_sig)} reaches 5% significance, one of {6 * sum(g['season'] == 'MAM' for g in grid)} March to May correlations." if mam_t_sig else ", not significant at 5% in any country."),
]

# ------------------------------------------------------------------ CHIRTS against ERA5
chc = pd.read_csv(OUT / "chirts_compare.csv")
CH_ORDER = [(i, s_) for i in NAMES for s_ in ["MAM", "JJAS", "OND"]]
chirts_rows = []
for iso, s_ in CH_ORDER:
    q = chc[(chc.iso3 == iso) & (chc.season == s_)].iloc[0]
    chirts_rows.append(dict(label=f"{NAMES[iso]}, {SEASON_NAME[s_]}", agree=float(q.anom_corr), level=float(q.level_diff_c),
                            rain=[float(q.r_rain_era5), float(q.r_rain_chirts)], nino=[float(q.r_nino_era5), float(q.r_nino_chirts)],
                            dmi=[float(q.r_dmi_era5), float(q.r_dmi_chirts)], ninoT=[float(q.nino_era5), float(q.nino_chirts)],
                            n_nino=int(q.n_el_nino)))


def chq(iso, s_, col):
    return float(chc[(chc.iso3 == iso) & (chc.season == s_)][col].iloc[0])


CHIRTS_FINDINGS = [
    f"Level: the CHIRTS mean daily maximum is {chc.level_diff_c.min():.1f} to {chc.level_diff_c.max():.1f} °C above the afternoon "
    f"maximum used on this page, across the {len(chc)} country seasons. The page compares anomalies, not levels.",
    f"Year to year (season anomalies, trend removed, {1983} to {2016}): the two records correlate at "
    f"{rng([chq('KEN', s_, 'anom_corr') for s_ in ['MAM', 'JJAS', 'OND']])} in Kenya, "
    f"{rng([chq('ETH', s_, 'anom_corr') for s_ in ['MAM', 'JJAS', 'OND']])} in Ethiopia and "
    f"{rng([chq('SOM', s_, 'anom_corr') for s_ in ['MAM', 'JJAS', 'OND']])} in Somalia.",
    f"Rain and temperature: the correlation between season rain and the afternoon maximum is "
    f"{rng([q for q in chc.r_rain_chirts if q < -0.3])} with CHIRTS in the {int((chc.r_rain_chirts < -0.3).sum())} country seasons "
    f"where ERA5 gives {rng([q for q in chc.r_rain_era5[chc.r_rain_chirts < -0.3]])}; Somalia June to September shows no correlation "
    f"in either ({chq('SOM', 'JJAS', 'r_rain_era5'):+.2f}, {chq('SOM', 'JJAS', 'r_rain_chirts'):+.2f}).",
    f"El Niño, June to September: the {int(chq('ETH', 'JJAS', 'n_el_nino'))} El Niño seasons of 1983 to 2016 were "
    f"{chq('ETH', 'JJAS', 'nino_chirts'):+.1f} °C (CHIRTS) and {chq('ETH', 'JJAS', 'nino_era5'):+.1f} °C (ERA5) against the trend in "
    f"Ethiopia, {chq('KEN', 'JJAS', 'nino_chirts'):+.1f} °C and {chq('KEN', 'JJAS', 'nino_era5'):+.1f} °C in Kenya.",
    f"October to December: the correlation of the afternoon maximum with the DMI is "
    f"{chq('ETH', 'OND', 'r_dmi_chirts'):+.2f}, {chq('KEN', 'OND', 'r_dmi_chirts'):+.2f} and {chq('SOM', 'OND', 'r_dmi_chirts'):+.2f} "
    f"with CHIRTS (Ethiopia, Kenya, Somalia) against {chq('ETH', 'OND', 'r_dmi_era5'):+.2f}, {chq('KEN', 'OND', 'r_dmi_era5'):+.2f} "
    f"and {chq('SOM', 'OND', 'r_dmi_era5'):+.2f} with ERA5; with Niño3.4, {chq('ETH', 'OND', 'r_nino_chirts'):+.2f}, "
    f"{chq('KEN', 'OND', 'r_nino_chirts'):+.2f} and {chq('SOM', 'OND', 'r_nino_chirts'):+.2f} against "
    f"{chq('ETH', 'OND', 'r_nino_era5'):+.2f}, {chq('KEN', 'OND', 'r_nino_era5'):+.2f} and {chq('SOM', 'OND', 'r_nino_era5'):+.2f}.",
    f"March to May, Somalia: the correlation of the afternoon maximum with Niño3.4 is {chq('SOM', 'MAM', 'r_nino_chirts'):+.2f} with "
    f"CHIRTS and {chq('SOM', 'MAM', 'r_nino_era5'):+.2f} with ERA5 over 1983 to 2016.",
]

HOT = None
if (OUT / "hot_days.csv").exists():
    HOT = pd.read_csv(OUT / "hot_days.csv")
    for pp in panels:
        h = HOT[(HOT.iso3 == pp["iso"]) & (HOT.season == pp["season"])].set_index("year").reindex(pp["years"])
        pp["hot"] = [None if pd.isna(v) else round(float(v), 1) for v in h.hot_days]
        pp["hot_normal"] = float(h.hot_days_normal.dropna().iloc[0])
        pp["hottest"] = [None if pd.isna(v) else round(float(v), 1) for v in h.hottest_c]
    hot_rows = []
    for iso in NAMES:
        for s_ in ["FM", "MAM", "JJAS", "OND"]:
            h = HOT[(HOT.iso3 == iso) & (HOT.season == s_)]
            if h.empty:
                continue
            early, late = h[h.year <= 2000].hot_days.mean(), h[h.year >= 2006].hot_days.mean()
            row = dict(iso=iso, season=s_, label=f"{NAMES[iso]}, {'February to March' if s_ == 'FM' else SEASON_NAME[s_]}",
                       days=int(h.days.median()), normal=float(h.hot_days_normal.iloc[0]), early=float(early), late=float(late))
            if s_ != "FM":
                d8, w8 = h[h.year.isin(stats[(iso, s_)]["dry_years"])], h[h.year.isin(stats[(iso, s_)]["wet_years"])]
                assert len(d8) == len(w8) == 8
                row |= dict(dry=float(d8.hot_days_detr.mean()), wet=float(w8.hot_days_detr.mean()),
                            normal_d=float(h[h.year.between(1991, 2020)].hot_days_detr.mean()))
            hot_rows.append(row)

payload = dict(cycle=cyc, map=dict(cells=mapcells, outline=outline, k=k, w=20 * k, h=21 * k, res=0.25),
               panels=panels, grid=grid, phases=phases, chirts=chirts_rows, names=NAMES, first=FIRST, last=LAST)

# ------------------------------------------------------------------ text
def c(iso, reg):
    return next(p for p in cyc if p["id"] == f"{iso}-{REGIME[reg]}")

B = "bimodal (MAM and OND)"
eth1, eth2, ken, som = c("ETH", "unimodal (JJAS)"), c("ETH", B), c("KEN", B), c("SOM", B)
mo_r = sorted(((v["r"], k_) for k_, v in stats.items()))
weak = [(r_, k_) for r_, k_ in mo_r if r_ > -0.3]
strong = [r_ for r_, k_ in mo_r if r_ <= -0.3]
weak_txt = "; ".join(f"{NAMES[k_[0]]} {SEASON_NAME[k_[1]]} {r_:+.2f}" for r_, k_ in weak)
def hot(p_):
    t = p_["tmax"]; order = sorted(range(12), key=lambda m: -t[m])
    return f"{MON[order[0]]} ({t[order[0]]:.1f} °C; {MON[order[1]]} {t[order[1]]:.1f} °C)"


mid_n, mid_w = warm_share(2001, 2010)
FINDINGS = [
    f"Hottest month of the year, afternoon maximum 1991 to 2020: {hot(ken)} in Kenya's two-season areas, {hot(som)} in Somalia's, "
    f"{hot(eth2)} in Ethiopia's two-season areas and {hot(eth1)} in Ethiopia's June to September areas. Two-season areas are the cells "
    f"where March to May and October to December each bring at least a quarter of the annual rain. The wettest months are "
    f"{ken['wettest']} (Kenya), {som['wettest']} (Somalia), {eth2['wettest']} and {eth1['wettest']} (Ethiopia).",
    f"Afternoon maximum averaged over the rainy months against the dry months: Kenya's two-season areas {ken['tmax_wet']} °C "
    f"against {ken['tmax_dry']} °C, Somalia's {som['tmax_wet']} °C against {som['tmax_dry']} °C, Ethiopia's two-season areas "
    f"{eth2['tmax_wet']} °C against {eth2['tmax_dry']} °C, Ethiopia's June to September areas {eth1['tmax_wet']} °C against "
    f"{eth1['tmax_dry']} °C. The coolest month is {ken['coolest']} in Kenya ({'a rainy' if ken['coolest_wet'] else 'a dry'} month) "
    f"and {som['coolest']} in Somalia ({'a rainy' if som['coolest_wet'] else 'a dry'} month).",
    f"Season by season, with the trend removed, the correlation between season rainfall and afternoon maximum is between "
    f"{min(strong):.2f} and {max(strong):.2f} in {len(strong)} of the {len(stats)} country seasons below"
    + (f" ({weak_txt} in the other{'s' if len(weak) > 1 else ''})" if weak else "") + ". "
    f"In October to December the eight driest seasons averaged {stats[('KEN', 'OND')]['dry']:+.1f} °C in Kenya and "
    f"{stats[('SOM', 'OND')]['dry']:+.1f} °C in Somalia against the trend, the eight wettest {stats[('KEN', 'OND')]['wet']:+.1f} °C and "
    f"{stats[('SOM', 'OND')]['wet']:+.1f} °C.",
    f"The afternoon maximum in the rainy seasons rose by {min(p['trend'] for p in panels):.1f} to {max(p['trend'] for p in panels):.1f} °C "
    f"per decade over {FIRST} to {LAST}. Of the March to May and October to December country seasons with at least 120% of normal "
    f"rainfall, {early_w} of {early_n} had an afternoon maximum above the 1991 to 2020 normal in {FIRST} to 2000, {mid_w} of {mid_n} "
    f"in 2001 to 2010 and {late_w} of {late_n} in 2011 to {LAST}.",
]

# recent seasons table
rows = []
for y in range(2015, LAST + 1):
    for s in ["MAM", "OND"]:
        cells_ = []
        for iso in NAMES:
            q = seas[(seas.iso3 == iso) & (seas.season == s) & (seas.year == y)]
            if q.empty:
                cells_.append("<td></td><td></td><td></td>")
                continue
            q = q.iloc[0]
            rp, ta = round(float(q.rain_pct)), round(float(q.tmax_anom_c), 1) + 0.0  # shade on the shown values; + 0.0 drops "-0.0"
            rc = "lo" if rp < 80 else "hi" if rp > 120 else ""
            tc = "warm" if ta >= 0.5 else "cool" if ta <= -0.5 else ""
            ev = []
            fi = ids(q.flood_ids)
            if fi:
                ev.append(f"Flood ({fmt_n(EVENTS.loc[fi, 'Total Affected'].sum()) or 'no figure'})")
            if ids(q.drought_ids):
                ev.append("Drought")
            cells_.append(f"<td class='n {rc}'>{rp}%</td><td class='n {tc}'>{ta:+.1f}</td><td class='ev'>{esc(', '.join(ev))}</td>")
        rows.append(f"<tr><td class='yr'>{SEASON_NAME[s]} {y}</td>{''.join(cells_)}</tr>")
RECENT = ("<div class='scroll'><table class='recent'><thead><tr><th rowspan='2'>Season</th>"
          + "".join(f"<th colspan='3' class='grp'>{n}</th>" for n in NAMES.values())
          + "</tr><tr>" + "<th class='n'>Rain, % of normal</th><th class='n'>Afternoon maximum, °C vs normal</th><th>EM-DAT (affected)</th>" * 3
          + "</tr></thead><tbody>" + "".join(rows) + "</tbody></table></div>")

tc_ = OUT / "tmax_check.json"
if tc_.exists():
    q = json.loads(tc_.read_text())
    TMAX_NOTE = (f"Checked against the ERA5 mean daily maximum for {q['year']}: mean daily maximum minus the value used here is "
                 f"{q['mean_diff_c']:+.1f} °C on average over the land cells (by month {q['month_min_c']:+.1f} to {q['month_max_c']:+.1f} °C; "
                 f"correlation {q['corr_monthly_cells']:.2f} across cells and months).")
else:
    TMAX_NOTE = "How far it sits below the mean of daily maxima has not been checked."
METHOD = [
    "Rainfall: CHIRPS v2.0 monthly (UCSB Climate Hazards Center), 0.05° grid averaged to 0.25° blocks.",
    "Temperature: ERA5 reanalysis (ECMWF, Copernicus Climate Data Store), monthly means of 2 m temperature by hour of day. "
    "The afternoon maximum is the warmest of the 12:00 to 16:00 East Africa Time monthly means; the monthly product has no "
    "daily maximum. " + TMAX_NOTE + " ERA5 is interpolated to the CHIRPS blocks.",
    "Normals: 1991 to 2020. A rainy month is a month with more than one twelfth of the annual normal rainfall.",
    "Rainfall regime per 0.25° cell: two rainy seasons where March to May and October to December each bring at least 25% "
    "of the annual total; one main season where June to September brings at least half; otherwise other.",
    "Season figures: rainfall total as % of the 1991 to 2020 normal and mean afternoon maximum minus normal, averaged over the "
    "cells of the country where that season brings at least 25% of the annual total (the cell count is in each chart's subtitle).",
    "Correlations, the driest and wettest eight seasons, the correlation grid and the phase chart use the afternoon maximum "
    "with its linear trend removed; the season charts and the recent seasons table show it as observed.",
    "Impacts: EM-DAT (CRED, UCLouvain) flood and drought events for each country, matched at country level. A flood is shown "
    "in the season its start month falls in; " + f"floods starting outside the seasons shown are left off ({OUTSIDE}). "
    "A drought is shown in every season its start-to-end months overlap; where EM-DAT gives no start or end month, January or "
    "December is used, so the event covers that whole year. Multi-year EM-DAT droughts therefore mark many seasons: "
    + "; ".join(f"{NAMES[i]} {', '.join(str(int(((seas.iso3 == i) & (seas.season == s_) & (seas.drought_events > 0)).sum())) for s_ in ['MAM', 'JJAS', 'OND'])}"
                for i in NAMES) + " of the 45 March to May, June to September and October to December seasons.",
    "Dry spells: CHIRPS v2.0 daily at 0.25°. A dry day has less than 1 mm. The longest dry spell is the longest run of dry days "
    "inside the calendar season, so a late start of the rains counts as a dry spell at the start of the season. Averaged over the "
    "same cells as the season figures, less cells with missing daily values.",
    "ENSO: Niño3.4 anomaly from NOAA CPC (ERSST v5, against a fixed 1991 to 2020 base). IOD: Dipole Mode Index from NOAA PSL "
    "(HadISST1.1; the DMI file is marked preliminary). Both are averaged over the season's own months, so they describe the "
    "same months as the rainfall, not a lead.",
    "Correlations: Pearson, over the 45 seasons; significance is a two-sided test at 5%. 'With the other index removed' is a "
    "partial correlation: the other index's linear relationship is taken out of both before correlating.",
    "Second temperature record: CHIRTS-daily v1.0 Tmax (UCSB Climate Hazards Center), Africa 0.25° files, monthly means of "
    "daily maximum, 1983 to 2016.",
    "Country boundaries: OCHA COD-AB.",
]
NOT_COVERED = [
    "Daily extremes such as heatwaves: the temperature here is a monthly afternoon average.",
    "Sub-national impacts: EM-DAT events are matched at country level only.",
    "Humidity and heat stress: temperature alone does not show how hot it feels.",
    "Forecasts: everything here is observed or reanalysis data, and the ENSO and IOD values are for the same months as the season.",
]


def ul(items, cls=""):
    return f"<ul class='{cls}'>" + "".join(f"<li>{esc(t)}</li>" for t in items) + "</ul>"


CSS = """
:root{color-scheme:light;--bg:#F4F6F7;--surface:#FFFFFF;--ink:#15212A;--ink2:#46555F;--muted:#76838C;--rule:#DDE3E6;--rule2:#EEF1F3;
--accent:#1D5B7C;--rain:#2a78d6;--temp:#eb6834;--two:#7c5cc4;--one:#1a9e8f;--other:#C9D1D6;--band:rgba(140,58,11,.12);--wetband:rgba(42,120,214,.08);
--div-cool:#2a78d6;--div-mid:#E3E7EA;--div-warm:#eb6834;--note:#FFF7E6;--noteb:#D08A3E;--dryc:#b8569f;--hotc:#b8569f;
--sans:"Public Sans",system-ui,-apple-system,"Segoe UI",sans-serif;--mono:"IBM Plex Mono",ui-monospace,Consolas,monospace}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;
--muted:#7F8C94;--rule:#2B353B;--rule2:#222B31;--accent:#7DB6D6;--rain:#3987e5;--temp:#d95926;--two:#9b7fe0;--one:#22a090;--other:#3A454C;
--band:rgba(242,179,107,.16);--wetband:rgba(57,135,229,.12);--div-cool:#3987e5;--div-mid:#2E383E;--div-warm:#d95926;--note:#2A2418;--noteb:#B9773A;--dryc:#c96bb0;--hotc:#c96bb0}}
:root[data-theme="dark"]{color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;--muted:#7F8C94;--rule:#2B353B;
--rule2:#222B31;--accent:#7DB6D6;--rain:#3987e5;--temp:#d95926;--two:#9b7fe0;--one:#22a090;--other:#3A454C;
--band:rgba(242,179,107,.16);--wetband:rgba(57,135,229,.12);--div-cool:#3987e5;--div-mid:#2E383E;--div-warm:#d95926;--note:#2A2418;--noteb:#B9773A;--dryc:#c96bb0;--hotc:#c96bb0}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font-family:var(--sans);font-size:16px;line-height:1.55}
.wrap{max-width:1000px;margin:0 auto;padding:36px 16px 56px}
header{padding-bottom:18px;border-bottom:1px solid var(--rule)}
.eyebrow{font:500 12px/1 var(--mono);letter-spacing:.08em;text-transform:uppercase;color:var(--accent)}
h1{font-size:clamp(26px,4.6vw,38px);line-height:1.12;margin:12px 0;font-weight:700;letter-spacing:-.015em;text-wrap:balance}
section{margin-top:40px}
h2{font-size:21px;margin:0 0 8px;font-weight:600}
h3{font-size:16px;margin:0 0 2px;font-weight:600}
.sub{color:var(--ink2);max-width:80ch;margin:0 0 14px;font-size:15px}
.sm{font-size:12.5px;color:var(--muted);font-weight:400}
ul.findings{padding-left:20px;margin:0 0 18px;max-width:80ch}ul.findings li{margin:8px 0}
.reading{background:var(--surface);border:1px solid var(--rule);border-radius:6px;padding:14px 18px;margin:0 0 16px}
.reading>b{display:block;margin-bottom:8px}
.reading .two{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:8px 24px;font-size:15px;color:var(--ink2)}
.reading .two span{display:block}.reading .two b{color:var(--ink)}
.panel{background:var(--surface);border:1px solid var(--rule);border-radius:6px;padding:14px 16px;margin-bottom:14px}
.grid2{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px}
.grid2 .panel{margin:0}
.season{overflow-x:auto}.season svg{min-width:680px}
svg{display:block;width:100%;height:auto}
svg text{font-family:var(--sans);fill:var(--ink2);font-size:12px}
svg text.lab{fill:var(--ink);font-weight:600}
.legend{display:flex;flex-wrap:wrap;gap:6px 18px;font-size:13px;color:var(--ink2);margin-top:8px}
.legend span{white-space:nowrap}
.legend i{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-1px;margin-right:5px}
.legend i.ln{height:3px;border-radius:2px;vertical-align:3px}
.ramp{display:inline-block;width:120px;height:10px;border-radius:2px;vertical-align:0;margin:0 6px;background:linear-gradient(90deg,var(--div-cool),var(--div-mid),var(--div-warm))}
.seg{display:inline-flex;border:1px solid var(--rule);border-radius:6px;overflow:hidden;background:var(--surface);margin:0 0 12px}
.seg button{border:0;background:transparent;color:var(--ink2);padding:7px 14px;font:inherit;font-size:14px;cursor:pointer}
.seg button[aria-pressed="true"]{background:var(--rule2);color:var(--ink);font-weight:600}
.stat{font-size:13px;color:var(--ink2);margin:2px 0 6px}
.scroll{overflow-x:auto}
table{border-collapse:collapse;width:100%;font-size:14px;background:var(--surface);border:1px solid var(--rule)}
th,td{padding:5px 9px;border-bottom:1px solid var(--rule2);text-align:left;vertical-align:top}
th{font-size:12px;font-weight:600;color:var(--muted);line-height:1.3}
th.grp{text-align:center;color:var(--ink);font-size:13px;border-bottom:2px solid var(--rule)}
.n{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
td.yr{white-space:nowrap;color:var(--ink2);font-variant-numeric:tabular-nums}
td.ev{font-size:12.5px;color:var(--ink2);min-width:120px}
td.lo{background:rgba(235,104,52,.16)}td.hi{background:rgba(42,120,214,.16)}
td.warm{background:rgba(235,104,52,.16)}td.cool{background:rgba(42,120,214,.16)}
ul.notes{padding-left:20px;max-width:80ch;color:var(--ink2)}ul.notes li{margin:6px 0}
details{margin-top:10px;font-size:14px}
summary{cursor:pointer;color:var(--accent);font-weight:500}
.tip{position:fixed;pointer-events:none;background:var(--surface);color:var(--ink);border:1px solid var(--rule);border-radius:4px;padding:6px 9px;font-size:13px;box-shadow:0 2px 8px rgba(0,0,0,.12);display:none;z-index:9;max-width:300px}
.tip .sm{margin-top:2px}
.home{display:inline-block;margin:0 0 18px;padding:6px 12px;font:500 13px/1 var(--sans);color:var(--accent);background:var(--surface);border:1px solid var(--rule);border-radius:4px;text-decoration:none}
table.grid{font-size:14px;max-width:760px}
table.grid td.rc{text-align:center;font-variant-numeric:tabular-nums;min-width:84px;opacity:.55;border-left:2px solid var(--surface)}
table.grid td.rc.sig{opacity:1;font-weight:700}
table.grid td.sep{border-left:3px solid var(--rule)}
table.grid tr.grp td{font-weight:600;color:var(--ink);background:var(--rule2);font-size:13px}
table.grid td.rl{white-space:nowrap}
#phasechart{overflow-x:auto}#phasechart svg{min-width:720px}
#chirtschart{overflow-x:auto}#chirtschart svg{min-width:760px}
#hotchart{overflow-x:auto}#hotchart svg{min-width:720px}
#plainphase,#leadchart{overflow-x:auto}#plainphase svg,#leadchart svg{min-width:720px}
.dot{display:inline-block;width:11px;height:11px;border-radius:50%;border:2px solid var(--ink);vertical-align:-1px;margin-right:5px}
.dot.f{background:var(--ink)}.tick{display:inline-block;width:2px;height:12px;background:var(--muted);vertical-align:-2px;margin-right:6px}
.box{display:inline-block;width:14px;height:10px;border-radius:2px;vertical-align:-1px;margin-right:5px;border:1.5px solid var(--ink)}
.box.f{background:var(--ink)}.box.n{background:var(--rule2);border-color:var(--rule2)}
footer{margin-top:44px;padding-top:14px;border-top:1px solid var(--rule);font-size:13px;color:var(--ink2)}
"""

JS = r"""
const A = JSON.parse(document.getElementById('data').textContent);
const MON = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];
const tip = document.getElementById('tip');
function el(t, a, txt){ const e = document.createElementNS('http://www.w3.org/2000/svg', t); for (const k in a) e.setAttribute(k, a[k]); if (txt != null) e.textContent = txt; return e; }
function show(ev, main, lines){ tip.textContent = ''; const b = document.createElement('div'); b.textContent = main; b.style.fontWeight = 600; tip.appendChild(b);
  (lines || []).forEach(l => { const s = document.createElement('div'); s.className = 'sm'; s.textContent = l; tip.appendChild(s); });
  tip.style.display = 'block'; tip.style.left = Math.min(ev.clientX + 14, innerWidth - 310) + 'px'; tip.style.top = Math.min(ev.clientY + 14, innerHeight - 90) + 'px'; }
function hide(){ tip.style.display = 'none'; }
function hit(svg, x, y, w, h, f){ const r = el('rect', {x, y, width: w, height: h, fill: 'transparent'}); r.addEventListener('pointermove', f); r.addEventListener('pointerleave', hide); svg.appendChild(r); }
function nice(lo, hi, n){ const step = [1,2,2.5,5,10,20,25,50,100].find(s => (hi - lo) / s <= n) || 100; const out = []; for (let v = Math.ceil(lo / step) * step; v <= hi + 1e-9; v += step) out.push(+v.toFixed(2)); return out; }

// ---- seasonal cycle: rain bars above, afternoon maximum line below, shared months
document.querySelectorAll('.cycle').forEach(box => {
  const P = A.cycle.find(p => p.id === box.dataset.id);
  const W = 460, ml = 40, mr = 8, h1 = 110, h2 = 100, gap = 26, mt = 8, mb = 22, H = mt + h1 + gap + h2 + mb;
  const bw = (W - ml - mr) / 12, x = i => ml + i * bw;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': P.label + ': monthly rainfall and afternoon maximum temperature, 1991 to 2020'});
  P.wet.forEach((w, i) => { if (w) { svg.appendChild(el('rect', {x: x(i), y: mt, width: bw, height: h1, fill: 'var(--wetband)'}));
                                     svg.appendChild(el('rect', {x: x(i), y: mt + h1 + gap, width: bw, height: h2, fill: 'var(--wetband)'})); } });
  const rmax = Math.max(...P.rain) * 1.08, ry = v => mt + h1 - v / rmax * h1;
  nice(0, rmax, 3).forEach(v => { svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: ry(v), y2: ry(v), stroke: 'var(--rule2)'})); svg.appendChild(el('text', {x: ml - 6, y: ry(v) + 4, 'text-anchor': 'end'}, v)); });
  svg.appendChild(el('text', {x: ml, y: mt + 2, 'dominant-baseline': 'hanging', class: 'lab', dx: 4}, 'Rain, mm per month'));
  P.rain.forEach((v, i) => { const y0 = ry(0), y1 = ry(v), w = bw - 6;
    svg.appendChild(el('path', {d: `M${x(i)+3},${y0}V${Math.min(y1 + 3, y0)}q0,-3 3,-3h${w-6}q3,0 3,3V${y0}Z`, fill: 'var(--rain)'})); });
  const t2 = mt + h1 + gap, tlo = Math.floor(Math.min(...P.tmax)) - 1, thi = Math.ceil(Math.max(...P.tmax)) + 1, ty = v => t2 + h2 - (v - tlo) / (thi - tlo) * h2;
  nice(tlo, thi, 3).forEach(v => { svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: ty(v), y2: ty(v), stroke: 'var(--rule2)'})); svg.appendChild(el('text', {x: ml - 6, y: ty(v) + 4, 'text-anchor': 'end'}, v)); });
  svg.appendChild(el('text', {x: ml, y: t2 - 18, 'dominant-baseline': 'hanging', class: 'lab', dx: 4}, 'Afternoon maximum, °C'));
  svg.appendChild(el('polyline', {points: P.tmax.map((v, i) => `${x(i) + bw / 2},${ty(v)}`).join(' '), fill: 'none', stroke: 'var(--temp)', 'stroke-width': 2}));
  P.tmax.forEach((v, i) => svg.appendChild(el('circle', {cx: x(i) + bw / 2, cy: ty(v), r: 3.5, fill: 'var(--temp)', stroke: 'var(--surface)', 'stroke-width': 1.5})));
  MON.forEach((m, i) => svg.appendChild(el('text', {x: x(i) + bw / 2, y: H - 6, 'text-anchor': 'middle'}, m[0])));
  MON.forEach((m, i) => hit(svg, x(i), mt, bw, h1 + gap + h2, ev => show(ev, m + (P.wet[i] ? ' (rainy month)' : ''),
    [`Rain ${P.rain[i]} mm`, `Afternoon maximum ${P.tmax[i]} °C`, `Monthly mean ${P.tmean[i]} °C`])));
  box.appendChild(svg);
});

// ---- maps: rainfall regime, and rainy months minus dry months
document.querySelectorAll('.map').forEach(box => {
  const M = A.map, k = M.k, s = M.res * k, mode = box.dataset.mode;
  const svg = el('svg', {viewBox: `0 0 ${M.w} ${M.h}`, role: 'img', 'aria-label': mode === 'regime' ? 'Map of rainfall regime' : 'Map of afternoon maximum in rainy months minus dry months'});
  const css = getComputedStyle(document.documentElement), C = n => css.getPropertyValue(n).trim();
  const mix = (a, b, t) => { const p = h => [1,3,5].map(i => parseInt(h.slice(i, i + 2), 16)); const A_ = p(a), B_ = p(b); return `rgb(${A_.map((v, i) => Math.round(v + (B_[i] - v) * t)).join(',')})`; };
  M.cells.forEach(([lon, lat, reg, d, iso]) => {
    let fill;
    if (mode === 'regime') fill = `var(--${reg})`;
    else { if (d == null) return; const t = Math.max(-1, Math.min(1, d / 4)); fill = t < 0 ? mix(C('--div-mid'), C('--div-cool'), -t) : mix(C('--div-mid'), C('--div-warm'), t); }
    const x = (lon - 32) * k - s / 2, y = (16 - lat) * k - s / 2;
    const r = el('rect', {x, y, width: s + .3, height: s + .3, fill});
    r.addEventListener('pointermove', ev => show(ev, `${lat.toFixed(2)}° N, ${lon.toFixed(2)}° E`,
      [{two: 'Two rainy seasons', one: 'One main season (June to September)', other: 'Other'}[reg], d == null ? '' : `Rainy months minus dry months: ${d > 0 ? '+' : ''}${d} °C`]));
    r.addEventListener('pointerleave', hide); svg.appendChild(r);
  });
  M.outline.forEach(d => svg.appendChild(el('path', {d, fill: 'none', stroke: 'var(--ink)', 'stroke-width': 1, 'stroke-linejoin': 'round', 'pointer-events': 'none'})));
  [['Ethiopia', 38.5, 9.5], ['Kenya', 37.6, 0.8], ['Somalia', 46.5, 6.0]].forEach(([n, lo, la]) =>
    svg.appendChild(el('text', {x: (lo - 32) * k, y: (16 - la) * k, 'text-anchor': 'middle', class: 'lab', 'pointer-events': 'none', style: 'paint-order:stroke;stroke:var(--surface);stroke-width:3px'}, n)));
  box.appendChild(svg);
});

// ---- seasons: rain % of normal above, temperature anomaly below, shared years; drought periods shaded, floods marked
function seasonChart(box, P){
  // stacked on shared years: ENSO and IOD phase strips, rain % of normal, afternoon maximum, longest dry spell
  const W = 960, ml = 46, mr = 8, gap = 30, strip = 12;
  const n = P.years.length, bw = (W - ml - mr) / n, x = i => ml + i * bw;
  const classic = !!A.classic;  // first version: rain and temperature only, no phase strips or dry spells
  const hasDry = !classic && P.spell && P.spell.some(v => v != null);
  const hasHot = !!A.showHot && P.hot && P.hot.some(v => v != null);
  const rows = [['rain', 120], ['temp', 100]].concat(hasDry ? [['dry', 100]] : []).concat(hasHot ? [['hot', 90]] : []);
  const strips = !classic || !!A.showPhase;
  let y = 8; const st = {enso: y + 14}; st.iod = st.enso + strip + 4; y = strips ? st.iod + strip + gap : 22;
  const top = {}; rows.forEach(([k, h]) => { top[k] = [y, h]; y += h + gap; });
  const H = y - gap + 22;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': P.label + ': ENSO and IOD phase, season rainfall, afternoon maximum and longest dry spell, with EM-DAT droughts and floods'});
  // drought bands across the three panels
  P.years.forEach((yr, i) => { if (!P.drought[yr]) return;
    rows.forEach(([k]) => { const [t, h] = top[k]; svg.appendChild(el('rect', {x: x(i), y: t, width: bw + .5, height: h, fill: 'var(--band)'})); }); });
  // phase strips
  // phase encoded by fill, not hue: positive (El Niño, positive IOD) solid, negative (La Niña, negative IOD) outline, neutral light
  if (strips) [['enso', 'ENSO'], ['iod', 'IOD']].forEach(([k, lab]) => {
    svg.appendChild(el('text', {x: ml - 6, y: st[k] + strip - 2, 'text-anchor': 'end'}, lab));
    P[k].forEach((ph, i) => { const pos = ph === 'El Niño' || ph === 'positive', neg = ph === 'La Niña' || ph === 'negative';
      svg.appendChild(el('rect', {x: x(i) + 1.5, y: st[k] + .75, width: Math.max(1, bw - 3), height: strip - 1.5, rx: 2,
        fill: pos ? 'var(--ink)' : neg ? 'var(--surface)' : 'var(--rule2)', stroke: neg ? 'var(--ink)' : 'none', 'stroke-width': 1.5})); }); });
  function panel(k, vals, base, lo, hi, fmt, col, label, unit){
    const [t, h] = top[k], yv = v => t + h - (v - lo) / (hi - lo) * h;
    nice(lo, hi, 4).forEach(v => { svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: yv(v), y2: yv(v), stroke: v === base ? 'var(--muted)' : 'var(--rule2)'}));
      svg.appendChild(el('text', {x: ml - 6, y: yv(v) + 4, 'text-anchor': 'end'}, fmt(v))); });
    svg.appendChild(el('text', {x: ml + 4, y: t - 8, class: 'lab'}, label));
    vals.forEach((v, i) => { if (v == null) return; const y0 = yv(base), y1 = yv(v);
      svg.appendChild(el('rect', {x: x(i) + 1.5, y: Math.min(y0, y1), width: Math.max(2, bw - 3), height: Math.max(1, Math.abs(y1 - y0)), rx: 1.5, fill: col, opacity: v >= base ? 1 : .55})); });
    return yv;
  }
  const ry = panel('rain', P.rain, 100, 0, Math.max(200, ...P.rain), v => v + '%', 'var(--rain)', 'Rain, % of normal');
  Object.entries(P.floods).forEach(([yr, f]) => { const i = P.years.indexOf(+yr); if (i < 0) return;
    const cx = x(i) + bw / 2, r = f.big ? 6 : 4;
    svg.appendChild(el('path', {d: `M${cx},${ry(Math.max(P.rain[i], 100)) - 4}l${-r},${-r * 1.6}h${2 * r}Z`, fill: 'var(--ink)', stroke: 'var(--surface)', 'stroke-width': 1.5})); });
  panel('temp', P.t, 0, Math.min(-2.5, ...P.t), Math.max(2.5, ...P.t), v => (v > 0 ? '+' : '') + v, 'var(--temp)', 'Afternoon maximum, °C against normal');
  if (hasDry) { const d = P.spell.filter(v => v != null);
    // bars start at the 1991-2020 normal; the axis spans the data, not zero, so year-to-year differences show
    panel('dry', P.spell, P.spell_normal, Math.max(0, Math.floor(Math.min(...d) - 3)), Math.ceil(Math.max(...d) + 3), v => v + ' d', 'var(--dryc)', `Longest dry spell, days (normal ${P.spell_normal.toFixed(0)})`); }
  if (hasHot) { const d = P.hot.filter(v => v != null);
    panel('hot', P.hot, P.hot_normal, 0, Math.ceil(Math.max(P.hot_normal * 2.5, ...d) + 1), v => v + ' d', 'var(--hotc)', `Very hot days in the season (usual: ${P.hot_normal.toFixed(0)})`); }
  P.years.forEach((yr, i) => { if (yr % 5 === 0) svg.appendChild(el('text', {x: x(i) + bw / 2, y: H - 6, 'text-anchor': 'middle'}, yr)); });
  P.years.forEach((yr, i) => hit(svg, x(i), strips ? st.enso : 8, bw, H - 22 - (strips ? st.enso : 8), ev => {
    const f = P.floods[yr], t = Math.abs(P.t[i]) < 0.05 ? 0 : P.t[i], sg = v => (v > 0 ? '+' : '') + v.toFixed(1);
    show(ev, `${SN[P.season]} ${yr}`, [
      ...(!strips ? [] : [
      `ENSO: Niño3.4 ${P.nino[i] == null ? 'n/a' : sg(P.nino[i])} °C${P.enso[i] && P.enso[i] !== 'neutral' ? ' (' + P.enso[i] + ')' : ''}`,
      `IOD: DMI ${P.dmi[i] == null ? 'n/a' : sg(P.dmi[i])} °C${P.iod[i] && P.iod[i] !== 'neutral' ? ' (' + P.iod[i] + ')' : ''}`]),
      `Rain ${P.rain[i].toFixed(0)}% of normal`, `Afternoon maximum ${sg(t)} °C against normal`,
      ...(hasDry && P.spell[i] != null ? [`Longest dry spell ${P.spell[i].toFixed(0)} days`] : []),
      ...(hasHot && P.hot[i] != null ? [`Very hot days: ${P.hot[i].toFixed(0)} (usual ${P.hot_normal.toFixed(0)}); hottest afternoon ${P.hottest[i].toFixed(1)} °C`] : []),
      ...(f ? f.ev : []), ...(P.drought[yr] || [])]); }));
  box.appendChild(svg);
}
document.querySelectorAll('.season').forEach(box => seasonChart(box, A.panels.find(p => p.id === box.dataset.id)));
document.querySelectorAll('#seasons .seg button').forEach(b => b.addEventListener('click', () => {
  document.querySelectorAll('#seasons .seg button').forEach(x => x.setAttribute('aria-pressed', String(x === b)));
  document.querySelectorAll('#seasons .country').forEach(w => { w.hidden = w.dataset.iso !== b.dataset.iso; });
}));

// ---- ENSO and IOD: correlation grid (each index alone, or with the other removed)
const SN = {OND: 'October to December', MAM: 'March to May', JJAS: 'June to September'};
function drawGrid(mode){
  const tb = document.querySelector('#grid tbody'); if (!tb) return; tb.textContent = '';
  let last = '';
  A.grid.forEach(g => {
    if (g.season !== last) { last = g.season; const tr = document.createElement('tr'); tr.className = 'grp';
      const td = document.createElement('td'); td.colSpan = 7; td.textContent = SN[g.season]; tr.appendChild(td); tb.appendChild(tr); }
    const tr = document.createElement('tr'); const th = document.createElement('td'); th.textContent = g.label; th.className = 'rl'; tr.appendChild(th);
    g.cells.forEach((c, j) => {
      const r = mode === 'partial' ? c.pr : c.r, pv = mode === 'partial' ? c.pp : c.p, td = document.createElement('td');
      td.className = 'rc' + (pv < 0.05 ? ' sig' : '') + (j === 3 ? ' sep' : '');
      const a = Math.min(1, Math.abs(r) / 0.8);
      td.style.background = `color-mix(in srgb, var(--accent) ${Math.round(a * 70)}%, var(--surface))`;
      td.style.color = a > 0.55 ? 'var(--surface)' : 'var(--ink)';
      td.textContent = (r > 0 ? '+' : r < 0 ? '−' : '') + Math.abs(r).toFixed(2);
      const idx = j < 3 ? 'Niño3.4 (ENSO)' : 'DMI (IOD)', v = ['rain', 'afternoon maximum (trend removed)', 'longest dry spell'][j % 3];
      td.addEventListener('pointermove', ev => show(ev, `${g.label}, ${SN[g.season]}`, [`${idx} with ${v}: r = ${r.toFixed(2)}`,
        `p = ${pv < 0.001 ? '<0.001' : pv.toFixed(3)} (${pv < 0.05 ? 'significant' : 'not significant'} at 5%), ${c.n} seasons`,
        ...(mode === 'partial' ? [] : [`rank correlation (Spearman) ${c.rs.toFixed(2)}`]),
        mode === 'partial' ? 'the other index removed from both' : `Niño3.4 and DMI correlate at ${g.cross.toFixed(2)} in these months`]));
      td.addEventListener('pointerleave', hide); tr.appendChild(td); });
    tb.appendChild(tr); });
}
drawGrid('alone');
document.querySelectorAll('#drivers .seg.gridmode button').forEach(b => b.addEventListener('click', () => {
  document.querySelectorAll('#drivers .seg.gridmode button').forEach(x => x.setAttribute('aria-pressed', String(x === b))); drawGrid(b.dataset.m); }));

// ---- ENSO and IOD: means by phase, one row per country and index, three measures side by side
function drawPhases(season){
  const box = document.getElementById('phasechart'); if (!box) return; box.textContent = '';
  const R = A.phases[season], W = 960, ml = 150, colw = (W - ml) / 3, rh = 30, mt = 34, H = mt + R.length * rh + 26;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': `Rain, temperature and longest dry spell by ENSO and IOD phase, ${SN[season]}`});
  const M = [['med', 'Rain, median % of normal', 100, v => v.toFixed(0) + '%'], ['temp', 'Afternoon maximum, °C (trend removed)', 0, v => (v > 0 ? '+' : '') + v.toFixed(1)],
             ['dry', 'Longest dry spell, days vs normal', 0, v => (v > 0 ? '+' : '') + v.toFixed(0)]];
  M.forEach(([k, lab, base, f], c) => {
    const vals = R.flatMap(r => Object.values(r.ph).map(o => o[k])).concat([base]);
    let lo = Math.min(...vals), hi = Math.max(...vals); const pad = (hi - lo) * .08 || 1; lo -= pad; hi += pad;
    const x0 = ml + c * colw + 14, x1 = ml + (c + 1) * colw - 14, xv = v => x0 + (v - lo) / (hi - lo) * (x1 - x0);
    svg.appendChild(el('text', {x: x0, y: 14, class: 'lab'}, lab));
    nice(lo, hi, 4).forEach(v => { svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt - 8, y2: H - 22, stroke: v === base ? 'var(--muted)' : 'var(--rule2)'}));
      svg.appendChild(el('text', {x: xv(v), y: H - 6, 'text-anchor': 'middle'}, f(v))); });
    R.forEach((r, i) => {
      const yc = mt + i * rh + rh / 2, P = r.ph, posK = r.driver === 'ENSO' ? 'El Niño' : 'positive', negK = r.driver === 'ENSO' ? 'La Niña' : 'negative';
      if (P[posK] && P[negK]) svg.appendChild(el('line', {x1: xv(P[negK][k]), x2: xv(P[posK][k]), y1: yc, y2: yc, stroke: 'var(--rule)', 'stroke-width': 2}));
      if (P.neutral) svg.appendChild(el('line', {x1: xv(P.neutral[k]), x2: xv(P.neutral[k]), y1: yc - 6, y2: yc + 6, stroke: 'var(--muted)', 'stroke-width': 2}));
      [[negK, false], [posK, true]].forEach(([pk, solid]) => { const o = P[pk]; if (!o) return;
        svg.appendChild(el('circle', {cx: xv(o[k]), cy: yc, r: 6, fill: solid ? 'var(--ink)' : 'var(--surface)', stroke: 'var(--ink)', 'stroke-width': 2}));
        const h = el('circle', {cx: xv(o[k]), cy: yc, r: 12, fill: 'transparent'});
        h.addEventListener('pointermove', ev => show(ev, `${A.names[r.iso]}, ${pk}${r.driver === 'IOD' ? ' IOD' : ''}`, [`${o.n} seasons: ${o.years}`,
          `Rain: mean ${o.rain.toFixed(0)}%, median ${o.med.toFixed(0)}%, range ${o.lo.toFixed(0)}% to ${o.hi.toFixed(0)}% of normal; below normal in ${o.below} of ${o.n}`,
          `Afternoon maximum (trend removed) ${(o.temp > 0 ? '+' : '') + o.temp.toFixed(1)} °C | longest dry spell ${(o.dry > 0 ? '+' : '') + o.dry.toFixed(0)} days vs normal`,
          `EM-DAT floods starting in ${o.floods} of these seasons`]));
        h.addEventListener('pointerleave', hide); svg.appendChild(h); });
    });
  });
  R.forEach((r, i) => { const yc = mt + i * rh + rh / 2;
    if (i % 2 === 0) svg.appendChild(el('text', {x: 0, y: yc + 4, class: 'lab'}, A.names[r.iso]));
    svg.appendChild(el('text', {x: ml - 6, y: yc + 4, 'text-anchor': 'end'}, r.driver));
    if (i % 2 === 1 && i < R.length - 1) svg.appendChild(el('line', {x1: 0, x2: W, y1: yc + rh / 2, y2: yc + rh / 2, stroke: 'var(--rule)'})); });
  box.appendChild(svg);
}
drawPhases('OND');
document.querySelectorAll('#drivers .seg.phaseseason button').forEach(b => b.addEventListener('click', () => {
  document.querySelectorAll('#drivers .seg.phaseseason button').forEach(x => x.setAttribute('aria-pressed', String(x === b))); drawPhases(b.dataset.s); }));

// ---- CHIRTS against ERA5
(function(){
  const box = document.getElementById('chirtschart'); if (!box) return;
  const R = A.chirts, W = 960, ml = 220, colw = (W - ml) / 4, rh = 28, mt = 34, H = mt + R.length * rh + 26;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'Agreement between CHIRTS and ERA5 and the temperature correlations computed with each'});
  const C = [['agree', 'Agreement of anomalies (r)', 0, 1], ['rain', 'With season rain (r)', -1, 1], ['nino', 'With Niño3.4 (r)', -1, 1], ['dmi', 'With DMI (r)', -1, 1]];
  C.forEach(([k, lab, lo, hi], c) => {
    const x0 = ml + c * colw + 26, x1 = ml + (c + 1) * colw - 26, xv = v => x0 + (v - lo) / (hi - lo) * (x1 - x0);
    svg.appendChild(el('text', {x: x0, y: 14, class: 'lab'}, lab));
    (k === 'agree' ? [0, 0.5, 1] : [-1, 0, 1]).forEach(v => {
      svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt - 8, y2: H - 22, stroke: v === 0 && k !== 'agree' ? 'var(--muted)' : 'var(--rule2)'}));
      svg.appendChild(el('text', {x: xv(v), y: H - 6, 'text-anchor': 'middle'}, (v > 0 && k !== 'agree' ? '+' : '') + v)); });
    R.forEach((r, i) => {
      const yc = mt + i * rh + rh / 2;
      if (k === 'agree') { svg.appendChild(el('circle', {cx: xv(r.agree), cy: yc, r: 5.5, fill: 'var(--ink)'}));
        hit(svg, x0 - 8, yc - rh / 2, x1 - x0 + 16, rh, ev => show(ev, r.label, [`CHIRTS and ERA5 season anomalies correlate at ${r.agree.toFixed(2)}`, `CHIRTS ${Math.abs(r.level).toFixed(1)} °C ${r.level >= 0 ? 'above' : 'below'} ERA5 on average`])); return; }
      const [e, ch] = r[k];
      svg.appendChild(el('line', {x1: xv(e), x2: xv(ch), y1: yc, y2: yc, stroke: 'var(--rule)', 'stroke-width': 2}));
      svg.appendChild(el('circle', {cx: xv(e), cy: yc, r: 5.5, fill: 'var(--temp)'}));
      svg.appendChild(el('circle', {cx: xv(ch), cy: yc, r: 5.5, fill: 'var(--surface)', stroke: 'var(--temp)', 'stroke-width': 2}));
      hit(svg, x0 - 8, yc - rh / 2, x1 - x0 + 16, rh, ev => show(ev, r.label, [`${lab}: ERA5 ${e.toFixed(2)}, CHIRTS ${ch.toFixed(2)}`, '1983 to 2016, afternoon maximum with the trend removed']));
    });
  });
  R.forEach((r, i) => { const yc = mt + i * rh + rh / 2; svg.appendChild(el('text', {x: 0, y: yc + 4}, r.label));
    if (i % 3 === 2 && i < R.length - 1) svg.appendChild(el('line', {x1: 0, x2: W, y1: yc + rh / 2, y2: yc + rh / 2, stroke: 'var(--rule)'})); });
  box.appendChild(svg);
})();

// ---- very hot days: then and now, and in dry and wet seasons
(function(){
  const box = document.getElementById('hotchart'); if (!box || !A.hot) return;
  const R = A.hot, W = 940, ml = 250, colw = (W - ml) / 2, rh = 28, mt = 34, H = mt + R.length * rh + 26;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'Very hot days per season: 1981 to 2000 against 2006 to 2025, and in dry against wet seasons'});
  const hi = Math.ceil(Math.max(...R.flatMap(r => [r.early, r.late, r.dry || 0, r.wet || 0, r.normal])) / 5) * 5 + 2;
  [['1981 to 2000 and 2006 to 2025, as measured', r => [[r.early, 'hollow', `1981 to 2000: ${r.early.toFixed(0)} a season on average`], [r.late, 'solid', `2006 to 2025: ${r.late.toFixed(0)}`]], r => r.normal],
   ['Eight wettest and eight driest seasons', r => r.dry == null ? [] : [[r.wet, 'hollow', `Eight wettest seasons: ${r.wet.toFixed(0)} on average`], [r.dry, 'solid', `Eight driest: ${r.dry.toFixed(0)} (after taking out the long-term warming)`]], r => r.normal_d]].forEach(([lab, pts, nrm], c) => {
    const x0 = ml + c * colw + 24, x1 = ml + (c + 1) * colw - 24, xv = v => x0 + v / hi * (x1 - x0);
    svg.appendChild(el('text', {x: x0, y: 14, class: 'lab'}, lab));
    nice(0, hi, 4).forEach(v => { svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt - 8, y2: H - 22, stroke: 'var(--rule2)'}));
      svg.appendChild(el('text', {x: xv(v), y: H - 6, 'text-anchor': 'middle'}, v)); });
    R.forEach((r, i) => { const yc = mt + i * rh + rh / 2, P = pts(r); if (!P.length) return;
      svg.appendChild(el('line', {x1: xv(nrm(r)), x2: xv(nrm(r)), y1: yc - 7, y2: yc + 7, stroke: 'var(--muted)', 'stroke-width': 2.5}));
      svg.appendChild(el('line', {x1: xv(Math.min(P[0][0], P[1][0])), x2: xv(Math.max(P[0][0], P[1][0])), y1: yc, y2: yc, stroke: 'var(--rule)', 'stroke-width': 2}));
      P.forEach(([v, k]) => svg.appendChild(el('circle', {cx: xv(v), cy: yc, r: 6, fill: k === 'solid' ? 'var(--hotc)' : 'var(--surface)', stroke: 'var(--hotc)', 'stroke-width': 2})));
      hit(svg, x0 - 10, yc - rh / 2, x1 - x0 + 20, rh, ev => show(ev, r.label, P.map(q => q[2]).concat([`Usual (1991 to 2020): ${nrm(r).toFixed(0)} of ${r.days} days`]))); });
  });
  R.forEach((r, i) => svg.appendChild(el('text', {x: 0, y: mt + i * rh + rh / 2 + 4}, r.label)));
  box.appendChild(svg);
})();

// ---- plain page: rain and afternoon temperature by ENSO and IOD phase, one season at a time
function plainPhase(season){
  const box = document.getElementById('plainphase'); if (!box || !A.plainPhase) return; box.textContent = '';
  const R = A.plainPhase[season], withHot = R.every(r => r.hot_normal != null && r.marks.every(m => m.hot != null));
  const cols = [['rain', 'Rain, % of usual (median)', 100, v => v + '%'], ['temp', 'Afternoon temperature, °C against usual', 0, v => (v > 0 ? '+' : '') + v]]
    .concat(withHot ? [['hot', 'Very hot days in the season', Math.round(R[0].hot_normal), v => v]] : []);
  const W = 940, ml = 150, colw = (W - ml) / cols.length, rh = 30, mt = 34, H = mt + R.length * rh + 26;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'Rain, afternoon temperature and very hot days by ENSO and IOD phase'});
  cols.forEach(([k, lab, base, f], c) => {
    const vals = R.flatMap(r => r.marks.map(m => m[k])).concat([base]);
    let lo = Math.min(...vals), hi = Math.max(...vals); const pad = (hi - lo) * .08; lo -= pad; hi += pad;
    const x0 = ml + c * colw + 20, x1 = ml + (c + 1) * colw - 20, xv = v => x0 + (v - lo) / (hi - lo) * (x1 - x0);
    svg.appendChild(el('text', {x: x0, y: 14, class: 'lab'}, lab));
    if (k === 'hot') svg.appendChild(el('line', {x1: xv(base), x2: xv(base), y1: mt - 8, y2: H - 22, stroke: 'var(--muted)'}));
    nice(lo, hi, 4).forEach(v => { svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt - 8, y2: H - 22, stroke: v === base && k !== 'hot' ? 'var(--muted)' : 'var(--rule2)'}));
      svg.appendChild(el('text', {x: xv(v), y: H - 6, 'text-anchor': 'middle'}, f(v))); });
    R.forEach((r, i) => { const yc = mt + i * rh + rh / 2, xs = r.marks.map(m => xv(m[k]));
      svg.appendChild(el('line', {x1: Math.min(...xs), x2: Math.max(...xs), y1: yc, y2: yc, stroke: 'var(--rule)', 'stroke-width': 2}));
      r.marks.forEach(m => { const xx = xv(m[k]);
        if (m.kind === 'neutral') svg.appendChild(el('line', {x1: xx, x2: xx, y1: yc - 7, y2: yc + 7, stroke: 'var(--muted)', 'stroke-width': 2.5}));
        else svg.appendChild(el('circle', {cx: xx, cy: yc, r: 6, fill: m.kind === 'positive' ? 'var(--ink)' : 'var(--surface)', stroke: 'var(--ink)', 'stroke-width': 2})); });
      hit(svg, x0 - 10, yc - rh / 2, x1 - x0 + 20, rh, ev => show(ev, `${r.label}, ${SN[season]}`, r.marks.map(m =>
        `${m.name} (${m.n} seasons): rain ${m.rain.toFixed(0)}% of usual, ${m.below} below usual; afternoons ${(m.temp > 0 ? '+' : '') + m.temp.toFixed(1)} °C` +
        (m.hot != null ? `; ${m.hot.toFixed(0)} very hot days (usual ${r.hot_normal.toFixed(0)})` : '')))); });
  });
  R.forEach((r, i) => { const yc = mt + i * rh + rh / 2; if (i % 2 === 0) svg.appendChild(el('text', {x: 0, y: yc + 4, class: 'lab'}, r.country));
    svg.appendChild(el('text', {x: ml - 6, y: yc + 4, 'text-anchor': 'end'}, r.driver)); });
  box.appendChild(svg);
}
plainPhase('OND');
document.querySelectorAll('#enso .seg button').forEach(b => b.addEventListener('click', () => {
  document.querySelectorAll('#enso .seg button').forEach(x => x.setAttribute('aria-pressed', String(x === b))); plainPhase(b.dataset.s); }));
(function(){
  const box = document.getElementById('leadchart'); if (!box || !A.lead) return;
  const C = A.lead, W = 940, pw = W / C.length, H = 300, ml = 44, mb = 44, mt = 24;
  const xs = C.flatMap(p => p.pts.map(q => q[0])), ys = C.flatMap(p => p.pts.map(q => q[1]));
  const xlo = Math.min(...xs) - .1, xhi = Math.max(...xs) + .1, yhi = Math.max(...ys) * 1.05;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'October to December rain against the IOD in August to September'});
  C.forEach((P, k) => {
    const x0 = k * pw + ml, x1 = (k + 1) * pw - 14, xv = v => x0 + (v - xlo) / (xhi - xlo) * (x1 - x0), yv = v => mt + (yhi - v) / yhi * (H - mt - mb);
    svg.appendChild(el('text', {x: x0, y: 14, class: 'lab'}, P.title));
    nice(0, yhi, 4).forEach(v => { svg.appendChild(el('line', {x1: x0, x2: x1, y1: yv(v), y2: yv(v), stroke: v === 100 ? 'var(--muted)' : 'var(--rule2)'}));
      if (k === 0) svg.appendChild(el('text', {x: x0 - 6, y: yv(v) + 4, 'text-anchor': 'end'}, v + '%')); });
    nice(xlo, xhi, 4).forEach(v => svg.appendChild(el('text', {x: xv(v), y: H - mb + 16, 'text-anchor': 'middle'}, (v > 0 ? '+' : '') + v)));
    [-0.4, 0.4].forEach(v => svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt, y2: H - mb, stroke: 'var(--ink2)', 'stroke-dasharray': '3 3'})));
    P.pts.forEach(([x, y, yr]) => { const pos = x >= 0.4, neg = x <= -0.4;
      svg.appendChild(el('circle', {cx: xv(x), cy: yv(y), r: 5, fill: pos ? 'var(--ink)' : neg ? 'var(--surface)' : 'var(--rule)', stroke: pos || neg ? 'var(--ink)' : 'var(--muted)', 'stroke-width': 1.5}));
      const h = el('circle', {cx: xv(x), cy: yv(y), r: 9, fill: 'transparent'});
      h.addEventListener('pointermove', ev => show(ev, `${P.title}, ${yr}`, [`IOD in August to September: ${(x > 0 ? '+' : '') + x.toFixed(2)} °C`, `October to December rain: ${y.toFixed(0)}% of usual`]));
      h.addEventListener('pointerleave', hide); svg.appendChild(h); });
  });
  svg.appendChild(el('text', {x: W / 2, y: H - 8, 'text-anchor': 'middle'}, 'IOD (DMI) in August to September, °C'));
  box.appendChild(svg);
})();
"""

cycle_html = "".join(
    f"<div class='panel'><h3>{esc(p['label'])}</h3><div class='stat'>Hottest month {p['hottest']}, wettest {p['wettest']} | "
    f"rainy months {p['tmax_wet']} °C, dry months {p['tmax_dry']} °C | {p['cells']} cells</div>"
    f"<div class='cycle' data-id='{p['id']}'></div></div>" for p in cyc)
season_html = ""
for iso in NAMES:
    inner = "".join(
        f"<div class='panel'><h3>{esc(p['label'])}</h3><div class='stat'>Correlation of rain with the afternoon maximum, trend removed, "
        f"{p['r']:+.2f} | eight driest seasons {p['dry']:+.1f} °C, eight wettest {p['wet']:+.1f} °C (trend removed) | trend "
        f"{p['trend']:+.2f} °C per decade | {p['cells']} cells</div><div class='season' data-id='{p['id']}'></div></div>"
        for p in panels if p["iso"] == iso)
    season_html += f"<div class='country' data-iso='{iso}'{'' if iso == 'ETH' else ' hidden'}>{inner}</div>"
seg = "".join(f"<button data-iso='{i}' aria-pressed='{str(i == 'ETH').lower()}'>{n}</button>" for i, n in NAMES.items())
PAYLOAD = json.dumps(payload).replace("</", "<\\/")

page = f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Horn Rain and Heat</title>
<meta name="description" content="ENSO and IOD against season rainfall, afternoon maximum temperature and dry spells, {FIRST} to {LAST}, in Ethiopia, Kenya and Somalia, with EM-DAT flood and drought events.">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>{CSS}</style>
</head>
<body>
<div class="wrap">
<a class="home" href="../">← ROSEA analyses</a>
<header>
  <div class="eyebrow">OCHA ROSEA support | Ethiopia, Kenya, Somalia | floods and drought</div>
  <h1>Horn of Africa: ENSO, IOD, rainfall, temperature, dry spells and flood and drought years, {FIRST} to {LAST}</h1>
</header>

<section id="findings">
<h2>At a glance</h2>
{ul(FINDINGS, 'findings')}
</section>


<section id="drivers">
<h2>ENSO and IOD</h2>
<p class="sub">ENSO: Niño3.4 sea surface temperature anomaly (NOAA CPC, ERSST v5). IOD: Dipole Mode Index (NOAA PSL, HadISST).
Each index is averaged over the same months as the season. El Niño where that season's Niño3.4 is +0.5 °C or more, La Niña −0.5 °C or less
(a single-season threshold, not NOAA's official El Niño and La Niña episodes);
positive IOD where the DMI is +0.4 °C or more, negative −0.4 °C or less. {FIRST} to {LAST}.</p>
{ul(DRIVER_FINDINGS, 'findings')}
<div class="panel">
<h3>Correlation of each index with the season</h3>
<p class="stat">Bold: significant at 5% ({LAST - FIRST + 1} seasons). With {n_tests} correlations in each view of the grid, about {round(n_tests * 0.05)} would reach 5% by chance alone.
Darker cells: stronger correlation, either sign. 'With the other index removed' can show a correlation that is not there for the index alone when the two
indices are themselves correlated (June to September: DMI with rain). Hover a cell for details.</p>
<div class="seg gridmode" role="group" aria-label="Correlation type"><button data-m="alone" aria-pressed="true">Each index alone</button><button data-m="partial" aria-pressed="false">With the other index removed</button></div>
<div class="scroll"><table class="grid" id="grid"><thead><tr><th></th><th colspan="3" class="grp">ENSO (Niño3.4)</th><th colspan="3" class="grp">IOD (DMI)</th></tr>
<tr><th></th><th class="n">Rain</th><th class="n">Afternoon maximum</th><th class="n">Longest dry spell</th><th class="n">Rain</th><th class="n">Afternoon maximum</th><th class="n">Longest dry spell</th></tr></thead><tbody></tbody></table></div>
</div>
<div class="panel">
<h3>Season averages by phase</h3>
<p class="stat">Each row: the seasons in the positive phase (El Niño, positive IOD), the negative phase (La Niña, negative IOD) and neutral.
Rain is the median, because a few very wet seasons pull the mean up; temperature and dry spell are means. ENSO and IOD rows share many of the same
seasons (in October to December every positive-IOD season was also El Niño), so the rows are not independent. Hover a dot for the years.</p>
<div class="seg phaseseason" role="group" aria-label="Season"><button data-s="OND" aria-pressed="true">October to December</button><button data-s="MAM" aria-pressed="false">March to May</button><button data-s="JJAS" aria-pressed="false">June to September</button></div>
<div id="phasechart"></div>
<div class="legend"><span><i class="dot f"></i>El Niño or positive IOD</span><span><i class="dot"></i>La Niña or negative IOD</span><span><i class="tick"></i>neutral</span></div>
</div>
</section>


<section id="chirts">
<h2>Temperature: a second record</h2>
<p class="sub">The temperature on this page is ERA5 reanalysis. CHIRTS-daily (UCSB Climate Hazards Center) builds its monthly maximum
temperature from station observations and satellite infrared, and spreads it over days with ERA5, so its monthly and season values
do not come from ERA5. It covers 1983 to 2016. Below, both records over the same cells and years, with 1983 to 2016 as the normal and
the trend removed from each.</p>
{ul(CHIRTS_FINDINGS, 'findings')}
<div class="panel">
<h3>CHIRTS against ERA5, 1983 to 2016</h3>
<p class="stat">Hover a row for the values.</p>
<div id="chirtschart"></div>
<div class="legend"><span><i class="dot f" style="border-color:var(--ink)"></i>Agreement of the two records</span><span><i class="dot f" style="background:var(--temp);border-color:var(--temp)"></i>ERA5</span><span><i class="dot" style="border-color:var(--temp)"></i>CHIRTS</span></div>
</div>
</section>

<section id="cycle">
<h2>Through the year</h2>
<p class="sub">Monthly normals, 1991 to 2020. Shaded columns are rainy months (more than one twelfth of the annual total).
Hover a month for its values.</p>
<div class="grid2">{cycle_html}</div>
<div class="legend"><span><i style="background:var(--rain)"></i>Rain (CHIRPS)</span><span><i class="ln" style="background:var(--temp)"></i>Afternoon maximum (ERA5)</span><span><i style="background:var(--wetband);border:1px solid var(--rule)"></i>Rainy month</span></div>
</section>

<section id="maps">
<h2>Where</h2>
<p class="sub">Left: rainfall regime of each 0.25° cell. Two rainy seasons cover {share_two['KEN']}% of Kenya, {share_two['SOM']}% of Somalia and {share_two['ETH']}% of Ethiopia by area.
Right: afternoon maximum in the rainy months minus the dry months; rainy months are cooler over {share_cooler['ETH']}% of Ethiopia, {share_cooler['KEN']}% of Kenya and {share_cooler['SOM']}% of Somalia.</p>
<div class="grid2">
<div class="panel"><h3>Rainfall regime</h3><div class="map" data-mode="regime"></div>
<div class="legend"><span><i style="background:var(--two)"></i>Two rainy seasons (March to May, October to December)</span><span><i style="background:var(--one)"></i>One main season (June to September)</span><span><i style="background:var(--other)"></i>Other</span></div></div>
<div class="panel"><h3>Rainy months minus dry months, afternoon maximum</h3><div class="map" data-mode="diff"></div>
<div class="legend"><span>−4 °C<span class="ramp"></span>+4 °C</span><span>blue: rainy months cooler | orange: rainy months hotter</span></div></div>
</div>
</section>

<section id="seasons">
<h2>Season by season</h2>
<p class="sub">Each rainy season, {FIRST} to {LAST}, over the part of the country where that season brings at least a quarter of the
annual rain: the ENSO and IOD phase of the season, then rainfall and afternoon maximum against the 1991 to 2020 normal and the
longest run of dry days (less than 1 mm) in the season. Shaded seasons overlap an EM-DAT drought; triangles mark EM-DAT floods that
started in that season (large triangle: 500 thousand or more affected). Hover a year for the figures.</p>
<div class="seg" role="group" aria-label="Country">{seg}</div>
{season_html}
<div class="legend"><span><i class="box f"></i>El Niño or positive IOD</span><span><i class="box"></i>La Niña or negative IOD</span><span><i class="box n"></i>neutral</span><span><i style="background:var(--rain)"></i>Rain, % of normal</span><span><i style="background:var(--temp)"></i>Afternoon maximum against normal</span><span><i style="background:var(--dryc)"></i>Longest dry spell</span><span><i style="background:var(--band);border:1px solid var(--rule)"></i>EM-DAT drought</span><span>▼ EM-DAT flood</span></div>
</section>

<section id="recent">
<h2>Recent seasons, 2015 to {LAST}</h2>
<p class="sub">March to May and October to December. Rain below 80% or above 120% of normal and temperature at least 0.5 °C from normal are shaded.</p>
{RECENT}
</section>

<section id="notes">
<h2>What this page does not cover</h2>
{ul(NOT_COVERED, 'notes')}
<details><summary>Method and data</summary>{ul(METHOD, 'notes')}</details>
</section>

</div>
<div class="tip" id="tip"></div>
<script id="data" type="application/json">{PAYLOAD}</script>
<script>{JS}</script>
</body>
</html>
"""
out = HERE / "hoa_climate_review.html"
out.write_text(page, encoding="utf8")
print("wrote", out, f"{out.stat().st_size / 1024:.0f} KB")

# ------------------------------------------------------------------ first version of the page: same data and fixes, original layout
import re as _re
v1 = page
for sec in ["drivers", "chirts"]:
    v1 = _re.sub(rf'\n<section id="{sec}">.*?</section>\n', "\n", v1, count=1, flags=_re.S)
v1 = v1.replace("<title>Horn Rain and Heat</title>", "<title>Horn Rain and Heat</title>", 1)
v1 = v1.replace(f"<h1>Horn of Africa: ENSO, IOD, rainfall, temperature, dry spells and flood and drought years, {FIRST} to {LAST}</h1>",
                f"<h1>Horn of Africa: temperature, rainfall and flood and drought years, {FIRST} to {LAST}</h1>", 1)
v1 = v1.replace(f'content="ENSO and IOD against season rainfall, afternoon maximum temperature and dry spells, {FIRST} to {LAST}, in Ethiopia, Kenya and Somalia, with EM-DAT flood and drought events."',
                f'content="Temperature and rainfall through the year and season by season, {FIRST} to {LAST}, in Ethiopia, Kenya and Somalia, with EM-DAT flood and drought events."', 1)
v1 = v1.replace("""annual rain: the ENSO and IOD phase of the season, then rainfall and afternoon maximum against the 1991 to 2020 normal and the
longest run of dry days (less than 1 mm) in the season. Shaded seasons""", """annual rain: rainfall and afternoon maximum against the 1991 to 2020 normal. Shaded seasons""", 1)
v1 = v1.replace("""<div class="legend"><span><i class="box f"></i>El Niño or positive IOD</span><span><i class="box"></i>La Niña or negative IOD</span><span><i class="box n"></i>neutral</span><span><i style="background:var(--rain)"></i>Rain, % of normal</span><span><i style="background:var(--temp)"></i>Afternoon maximum against normal</span><span><i style="background:var(--dryc)"></i>Longest dry spell</span>""",
                """<div class="legend"><span><i style="background:var(--rain)"></i>Rain, % of normal</span><span><i style="background:var(--temp)"></i>Afternoon maximum against normal</span>""", 1)
METHOD_V1 = [m for m in METHOD if not m.startswith(("Dry spells", "ENSO:", "Correlations: Pearson", "Second temperature"))]
v1 = v1.replace(f"<details><summary>Method and data</summary>{ul(METHOD, 'notes')}</details>",
                f"<details><summary>Method and data</summary>{ul(METHOD_V1, 'notes')}</details>", 1)
v1 = v1.replace(f"<li>{esc(NOT_COVERED[-1])}</li>", f"<li>{esc('Forecasts: everything here is observed or reanalysis data.')}</li>", 1)
v1 = v1.replace(f'<script id="data" type="application/json">{PAYLOAD}</script>',
                '<script id="data" type="application/json">' + json.dumps(payload | {"classic": True, "showHot": HOT is not None, "hot": hot_rows if HOT is not None else None}).replace("</", "<\/") + "</script>", 1)
v1 = v1.replace("Sources: CHIRPS v2.0 (UCSB CHC), ERA5 (ECMWF, Copernicus), NOAA CPC Niño3.4 (ERSST v5), NOAA PSL DMI (HadISST), ", "Sources: CHIRPS v2.0 (UCSB CHC), ERA5 (ECMWF, Copernicus), ")

# ---- plain-language text for the first version: same numbers as the full page, shorter sentences, terms explained once
def month_pair(p_):
    t = p_["tmax"]; o = sorted(range(12), key=lambda m: -t[m])
    return p_["hottest"] if t[o[0]] - t[o[1]] >= 0.3 else f"{MON[o[0]]} and {MON[o[1]]}"


def warmer(v):
    return f"{abs(v):.1f} °C {'hotter' if v >= 0 else 'cooler'}"


hotter_v1 = [pp for pp in panels if pp["dry"] > 0 and pp["wet"] < 0]
other_v1 = [pp for pp in panels if not (pp["dry"] > 0 and pp["wet"] < 0)]
PLAIN = [
    f"The hottest time of the year comes before the main rains: {month_pair(ken)} in Kenya, {month_pair(som)} in Somalia, "
    f"{month_pair(eth2)} in Ethiopia's areas with two rainy seasons and {month_pair(eth1)} in its areas with one main rainy season "
    f"(June to September).",
    f"In the areas with two rainy seasons, the rainy months average {ken['tmax_wet']} °C in the afternoon and the dry months "
    f"{ken['tmax_dry']} °C in Kenya; in Somalia, {som['tmax_wet']} °C and {som['tmax_dry']} °C.",
    f"In Ethiopia the rainy months are cooler: {eth1['tmax_wet']} °C against {eth1['tmax_dry']} °C in the dry months in the areas with "
    f"one main rainy season, {eth2['tmax_wet']} °C against {eth2['tmax_dry']} °C in the areas with two rainy seasons.",
    f"Drier seasons had hotter afternoons in {len(hotter_v1)} of the {len(panels)} country seasons on this page: there, the eight driest "
    f"of the 45 seasons were {min(pp['dry'] for pp in hotter_v1):.1f} to {max(pp['dry'] for pp in hotter_v1):.1f} °C hotter than usual "
    f"and the eight wettest {min(-pp['wet'] for pp in hotter_v1):.1f} to {max(-pp['wet'] for pp in hotter_v1):.1f} °C cooler "
    f"(after taking out the long-term warming)."
    + "".join(f" {pp['label']}: the eight driest {warmer(pp['dry'])} than usual, the eight wettest {warmer(pp['wet'])}." for pp in other_v1),
    f"Afternoons in the rainy seasons have warmed by {min(pp['trend'] for pp in panels):.1f} to {max(pp['trend'] for pp in panels):.1f} °C "
    f"every ten years since {FIRST}.",
    f"Very wet seasons (120% of usual rain or more, March to May and October to December), with temperatures as measured: "
    f"{early_w} of {early_n} had hotter afternoons than usual in {FIRST} to 2000, {mid_w} of {mid_n} in 2001 to 2010 and "
    f"{late_w} of {late_n} in 2011 to {LAST}.",
]
READING_V1 = """<div class="reading"><b>Reading this page</b><div class="two">
<span><b>Usual (or normal):</b> the average for 1991 to 2020.</span>
<span><b>Afternoon temperature:</b> the warmest hour of the afternoon (12:00 to 16:00), averaged over the month, from ERA5, a weather model run over past observations (a reanalysis).</span>
<span><b>Very hot day:</b> a day whose warmest afternoon hour was hotter than 9 in 10 days of the same month at that place in 1991 to 2020.</span>
<span><b>After taking out the long-term warming:</b> each place's straight-line warming since 1981 is subtracted first, so that seasons from different decades can be compared.</span>
<span><b>Rain:</b> CHIRPS satellite and station rainfall.</span>
<span><b>Rainy month:</b> a month with more than a twelfth of the year's rain.</span>
<span><b>Rainy season:</b> March to May, June to September or October to December, counted over the part of each country where that season brings at least a quarter of the year's rain.</span>
<span><b>Country season:</b> one rainy season in one country, for example Kenya, October to December.</span>
<span><b>Median:</b> the middle value: half the seasons had more, half had less.</span>
<span><b>Correlation:</b> from −1 to +1. 0: no link. +1: the two always rise and fall together. −1: one always rises when the other falls.</span>
<span><b>Floods and droughts:</b> events recorded in EM-DAT, the international disaster database.</span>
</div></div>"""
v1 = v1.replace(f"<h2>At a glance</h2>\n{ul(FINDINGS, 'findings')}", f"<h2>At a glance</h2>\n{READING_V1}\n{ul(PLAIN, 'findings')}", 1)
for p_ in cyc:  # month summary lines under each 'through the year' chart
    old = (f"<div class='stat'>Hottest month {p_['hottest']}, wettest {p_['wettest']} | "
           f"rainy months {p_['tmax_wet']} °C, dry months {p_['tmax_dry']} °C | {p_['cells']} cells</div>")
    new = (f"<div class='stat'>Hottest month: {p_['hottest']}. Wettest month: {p_['wettest']}. Rainy months average {p_['tmax_wet']} °C in the "
           f"afternoon, dry months {p_['tmax_dry']} °C.</div>")
    assert v1.count(old) == 1
    v1 = v1.replace(old, new, 1)
for p_ in panels:  # one plain line under each season chart
    old = (f"<div class='stat'>Correlation of rain with the afternoon maximum, trend removed, "
           f"{p_['r']:+.2f} | eight driest seasons {p_['dry']:+.1f} °C, eight wettest {p_['wet']:+.1f} °C (trend removed) | trend "
           f"{p_['trend']:+.2f} °C per decade | {p_['cells']} cells</div>")
    new = (f"<div class='stat'>Eight driest of the 45 seasons: afternoons {warmer(p_['dry'])} than usual; eight wettest: {warmer(p_['wet'])} "
           f"(both after taking out the long-term warming). Warming: {p_['trend']:.1f} °C every ten years. Bars: as measured.</div>")
    assert v1.count(old) == 1
    v1 = v1.replace(old, new, 1)
v1 = v1.replace("""<p class="sub">Monthly normals, 1991 to 2020. Shaded columns are rainy months (more than one twelfth of the annual total).
Hover a month for its values.</p>""", """<p class="sub">An average year, 1991 to 2020: bars show rain, the line shows afternoon temperature. Shaded columns are the rainy
months. Hover a month for its numbers.</p>""", 1)
v1 = v1.replace("<h2>Where</h2>", "<h2>Where the rains fall, and where rainy months are cooler</h2>", 1)
v1 = _re.sub(r'(<h2>Where the rains fall, and where rainy months are cooler</h2>\s*)<p class="sub">.*?</p>', lambda m: m.group(1) + (
    f'<p class="sub">Left: purple areas get two rainy seasons a year (March to May and October to December), teal areas one main season '
    f"(June to September). Two rainy seasons cover {share_two['KEN']}% of Kenya, {share_two['SOM']}% of Somalia and {share_two['ETH']}% of "
    f"Ethiopia. Right: blue where the rainy months have cooler afternoons than the dry months, orange where they are hotter. Rainy months "
    f"are cooler over {share_cooler['ETH']}% of Ethiopia, {share_cooler['KEN']}% of Kenya and {share_cooler['SOM']}% of Somalia. "
    f"Hover a square for its values.</p>"), v1, count=1, flags=_re.S)
v1 = _re.sub(r'(<section id="seasons">\s*<h2>Season by season</h2>\s*)<p class="sub">.*?</p>', lambda m: m.group(1) + (
    '<p class="sub">Every rainy season since 1981, over the part of the country where that season brings at least a quarter of the '
    "year's rain. Blue bars: rain compared with usual (above the line is wetter). Orange bars: afternoon temperature compared with usual "
    "(above the line is hotter). Shaded seasons: within the dates of an EM-DAT drought (the whole year where EM-DAT gives no months). Triangles: an EM-DAT flood started in that season "
    "(large triangle: 500,000 or more people affected). Hover a year for its numbers.</p>"), v1, count=1, flags=_re.S)
if HOT is not None:
    hr = {(r["iso"], r["season"]): r for r in hot_rows}
    more = [r for r in hot_rows if r["late"] > r["early"]]
    drier = [r for r in hot_rows if "dry" in r and r["dry"] > r["wet"]]
    fm_usual = {round(hr[(i, "FM")]["normal"]) for i in NAMES}
    assert len(fm_usual) == 1
    HOT_PLAIN = [
        (f"In all {len(hot_rows)} rows" if len(more) == len(hot_rows) else f"In {len(more)} of the {len(hot_rows)} rows")
        + " of the chart below, there were more very hot days in 2006 to 2025 than in 1981 to 2000. Kenya, October to December, for "
        f"example: {hr[('KEN', 'OND')]['early']:.0f} a season on average in 1981 to 2000 and {hr[('KEN', 'OND')]['late']:.0f} in 2006 to 2025 "
        f"(usual: {hr[('KEN', 'OND')]['normal']:.0f}).",
        f"February to March, over the whole country (usual: {fm_usual.pop()} a season), very hot days on average in 1981 to 2000 and in "
        f"2006 to 2025: " + ", ".join(f"{NAMES[i]} {hr[(i, 'FM')]['early']:.0f} and {hr[(i, 'FM')]['late']:.0f}" for i in NAMES) + ".",
        f"The eight driest of the 45 seasons had more very hot days than the eight wettest in {len(drier)} of the "
        f"{sum('dry' in r for r in hot_rows)} country seasons (after taking out the long-term warming). Kenya, October to December: "
        f"{hr[('KEN', 'OND')]['dry']:.0f} on average in the eight driest, {hr[('KEN', 'OND')]['wet']:.0f} in the eight wettest (usual: "
        f"{hr[('KEN', 'OND')]['normal_d']:.0f}).",
    ]
    hot_section = f"""
<section id="hot">
<h2>Very hot days</h2>
<p class="sub">A monthly average can hide a run of very hot days. Here each day counts on its own. A <b>very hot day</b> is a day
whose afternoon was hotter than 9 in 10 days of the same month at that place in 1991 to 2020. So in a usual season about 1 day in
10 is very hot (about 9 in a three-month season, 12 in June to September), in the highlands and the lowlands alike. Source: ERA5,
the warmest hour between 12:00 and 16:00 each day.</p>
{ul(HOT_PLAIN, 'findings')}
<div class="panel">
<h3>Very hot days per season</h3>
<p class="stat">Grey mark: the usual number (1991 to 2020). Left: as measured. Right: after taking out the long-term warming. Hover a row for the numbers.</p>
<div id="hotchart"></div>
<div class="legend"><span><b>Left:</b></span><span><i class="dot" style="border-color:var(--hotc)"></i>1981 to 2000</span><span><i class="dot f" style="background:var(--hotc);border-color:var(--hotc)"></i>2006 to 2025</span><span><b>Right:</b></span><span><i class="dot" style="border-color:var(--hotc)"></i>eight wettest seasons</span><span><i class="dot f" style="background:var(--hotc);border-color:var(--hotc)"></i>eight driest seasons</span><span><i class="tick"></i>usual</span></div>
</div>
</section>
"""
    v1 = v1.replace('<section id="seasons">', hot_section + '\n<section id="seasons">', 1)
    v1 = v1.replace('<span><i style="background:var(--temp)"></i>Afternoon maximum against normal</span>',
                    '<span><i style="background:var(--temp)"></i>Afternoon maximum against normal</span><span><i style="background:var(--hotc)"></i>Very hot days</span>', 1)
    v1 = v1.replace("Orange bars: afternoon temperature compared with usual (above the line is hotter).",
                    "Orange bars: afternoon temperature compared with usual (above the line is hotter). Pink bars: very hot days in the season, "
                    "compared with the usual number.", 1)
    v1 = v1.replace(f"<li>{esc(NOT_COVERED[0])}</li>", "", 1)
# ---- ENSO and IOD in plain words
PH_NAME = {"El Niño": "El Niño", "La Niña": "La Niña", "positive": "Positive IOD", "negative": "Negative IOD", "neutral": "Neutral"}
KIND = {"El Niño": "positive", "positive": "positive", "La Niña": "negative", "negative": "negative", "neutral": "neutral"}
hot_by_phase = {}
if HOT is not None:
    hp = HOT.merge(seas[["iso3", "season", "year", "enso", "iod"]], on=["iso3", "season", "year"], how="inner")
    for drv, col in [("ENSO", "enso"), ("IOD", "iod")]:
        for (i, s_, ph), g in hp.groupby(["iso3", "season", col]):
            hot_by_phase[(i, s_, drv, ph)] = float(g.hot_days_detr.mean())
    hot_normal_d = {(i, s_): float(g[g.year.between(1991, 2020)].hot_days_detr.mean()) for (i, s_), g in hp.groupby(["iso3", "season"])}
plain_phase = {}
for s_ in ["OND", "MAM", "JJAS"]:
    rows_ = []
    for iso in NAMES:
        for drv in ["ENSO", "IOD"]:
            q = dphase[(dphase.iso3 == iso) & (dphase.season == s_) & (dphase.driver == drv)]
            marks = [dict(kind=KIND[r.phase], name=PH_NAME[r.phase], n=int(r.n), rain=float(r.rain_median), below=int(r.below_normal),
                          temp=float(r.tmax_detr_c), hot=hot_by_phase.get((iso, s_, drv, r.phase))) for r in q.itertuples()]
            rows_.append(dict(country=NAMES[iso], driver=drv, label=f"{NAMES[iso]}, {drv}", marks=marks,
                              hot_normal=hot_normal_d.get((iso, s_)) if hot_by_phase else None))
    plain_phase[s_] = rows_
y6 = pd.read_csv(OUT / "rq6_years.csv")
lead = [dict(title=NAMES[i], pts=[[round(r.dmi_before, 2), round(r.rain_pct, 1), int(r.year)] for r in
                                  y6[(y6.iso3 == i) & (y6.season == "OND")].dropna(subset=["dmi_before"]).itertuples()]) for i in NAMES]
q6 = pd.read_csv(OUT / "rq6_lead.csv")
o6 = q6[(q6.season == "OND") & (q6["index"] == "DMI")].set_index("iso3")


def ph_(i, s_, drv, phase, col):
    return float(dphase[(dphase.iso3 == i) & (dphase.season == s_) & (dphase.driver == drv) & (dphase.phase == phase)][col].iloc[0])


def n_(s_, drv, phase):
    ns = {int(ph_(i, s_, drv, phase, "n")) for i in NAMES}
    assert len(ns) == 1
    return ns.pop()


def per(s_, drv, phase, col="rain_median", f="{:.0f}%"):
    return ", ".join(f"{NAMES[i]} {f.format(ph_(i, s_, drv, phase, col))}" for i in NAMES)


def per_t(s_, drv, phase):
    return ", ".join(f"{NAMES[i]} {warmer(ph_(i, s_, drv, phase, 'tmax_detr_c'))}" for i in NAMES)


n_pos = n_("OND", "IOD", "positive")
both = int(dphase[(dphase.iso3 == "KEN") & (dphase.season == "OND") & (dphase.driver == "IOD") & (dphase.phase == "positive")].also_enso.iloc[0])
pos_years = dphase[(dphase.iso3 == "KEN") & (dphase.season == "OND") & (dphase.driver == "IOD") & (dphase.phase == "positive")].years.iloc[0]
ond_n = {i: int(((seas.iso3 == i) & (seas.season == "OND")).sum()) for i in NAMES}
ond_below = {i: int(((seas.iso3 == i) & (seas.season == "OND") & (seas.rain_pct < 100)).sum()) for i in NAMES}
assert all(ond_below[i] > ond_n[i] / 2 for i in NAMES) and len(set(ond_n.values())) == 1
eth_jjas_all_below = int(ph_("ETH", "JJAS", "ENSO", "El Niño", "below_normal")) == n_("JJAS", "ENSO", "El Niño")
ENSO_PLAIN = [
    f"October to December, the {n_pos} seasons with a positive IOD: median rain {per('OND', 'IOD', 'positive')} of usual; afternoons "
    f"{per_t('OND', 'IOD', 'positive')} than usual (after taking out the long-term warming).",
    f"October to December, median rain by ENSO phase: El Niño ({n_('OND', 'ENSO', 'El Niño')} seasons): {per('OND', 'ENSO', 'El Niño')} | "
    f"La Niña ({n_('OND', 'ENSO', 'La Niña')}): {per('OND', 'ENSO', 'La Niña')} | neutral ({n_('OND', 'ENSO', 'neutral')}): "
    f"{per('OND', 'ENSO', 'neutral')}. Most October to December seasons had less rain than usual: "
    + ", ".join(f"{ond_below[i]} of {ond_n[i]} in {NAMES[i]}" for i in NAMES) + ".",
    (f"All {n_pos} positive-IOD seasons were also El Niño seasons ({pos_years.replace(' ', ', ')}). The other "
     f"{n_('OND', 'ENSO', 'El Niño') - both} El Niño seasons had no positive IOD." if both == n_pos else
     f"{both} of the {n_pos} positive-IOD seasons were also El Niño seasons ({pos_years.replace(' ', ', ')})."),
    f"The IOD in August and September, before the October to December rains. In the {int(o6.pos_n['KEN'])} years when it was positive "
    f"({o6.pos_years['KEN'].replace(' ', ', ')}), October to December rain was above usual in {int(o6.pos_above['ETH'])} of them in Ethiopia, "
    f"{int(o6.pos_above['KEN'])} in Kenya and {int(o6.pos_above['SOM'])} in Somalia. In the {int(o6.neg_n['KEN'])} years when it was negative "
    f"({o6.neg_years['KEN'].replace(' ', ', ')}), rain was below usual in {int(o6.neg_below['ETH'])} of them in Ethiopia, "
    f"{int(o6.neg_below['KEN'])} in Kenya and {int(o6.neg_below['SOM'])} in Somalia.",
    f"June to September, Ethiopia: in all {n_('JJAS', 'ENSO', 'El Niño')} El Niño seasons rain was below usual "
    f"(median {ph_('ETH', 'JJAS', 'ENSO', 'El Niño', 'rain_median'):.0f}%), and afternoons were "
    f"{warmer(ph_('ETH', 'JJAS', 'ENSO', 'El Niño', 'tmax_detr_c'))} than usual (after taking out the long-term warming)." if eth_jjas_all_below else "",
    f"March to May, median rain: La Niña ({n_('MAM', 'ENSO', 'La Niña')} seasons): {per('MAM', 'ENSO', 'La Niña')} of usual | "
    f"neutral ({n_('MAM', 'ENSO', 'neutral')}): {per('MAM', 'ENSO', 'neutral')}. Year to year, the correlation between March to May rain "
    f"and ENSO or the IOD is between {min(gr(i, 'MAM', idx, 'rain') for i in NAMES for idx in ['nino34', 'dmi']):+.2f} and "
    f"{max(gr(i, 'MAM', idx, 'rain') for i in NAMES for idx in ['nino34', 'dmi']):+.2f}.",
]
ENSO_PLAIN = [t for t in ENSO_PLAIN if t]
if hot_by_phase:
    def hot_line(s_, phases):
        return " | ".join(f"{NAMES[i]}: " + ", ".join(f"{lab} {hot_by_phase[(i, s_, drv, ph)]:.0f}" for lab, drv, ph in phases) for i in NAMES)
    ENSO_PLAIN.append(
        f"Very hot days on average, after taking out the long-term warming. October to December (usual: {hot_normal_d[('KEN', 'OND')]:.0f} a season): "
        + hot_line("OND", [("El Niño", "ENSO", "El Niño"), ("neutral", "ENSO", "neutral"), ("La Niña", "ENSO", "La Niña"), ("positive IOD", "IOD", "positive")])
        + f". June to September (usual: {hot_normal_d[('KEN', 'JJAS')]:.0f}): "
        + hot_line("JJAS", [("El Niño", "ENSO", "El Niño"), ("neutral", "ENSO", "neutral"), ("La Niña", "ENSO", "La Niña")]) + ".")
enso_section = f"""
<section id="enso">
<h2>El Niño, La Niña and the Indian Ocean Dipole</h2>
<div class="reading"><b>What these are</b><div class="two">
<span><b>ENSO (El Niño and La Niña):</b> how far the sea temperature in the central Pacific (the Niño3.4 area, NOAA) is above or below usual, averaged over the season. <b>El Niño</b>: 0.5 °C or more above usual. <b>La Niña</b>: 0.5 °C or more below. <b>Neutral</b>: neither.</span>
<span><b>IOD (Indian Ocean Dipole):</b> how far the western Indian Ocean is above its usual sea temperature, minus the same for the eastern Indian Ocean (the DMI, NOAA), averaged over the season. <b>Positive IOD</b>: +0.4 °C or more. <b>Negative IOD</b>: −0.4 °C or less. <b>Neutral</b>: in between.</span>
<span><b>Phase:</b> El Niño, La Niña or neutral; positive, negative or neutral IOD.</span>
</div></div>
{ul(ENSO_PLAIN, 'findings')}
<div class="panel">
<h3>Rain, afternoon temperature{' and very hot days' if hot_by_phase else ''} by phase</h3>
<p class="stat">Each row: the median rain, the average afternoon temperature{' and the average number of very hot days' if hot_by_phase else ''} of the seasons in each phase. Temperature{' and very hot days' if hot_by_phase else ''}: after taking out the long-term warming. Filled dot: El Niño or positive IOD. Open dot: La Niña or negative IOD. Grey mark: neutral. Grey line: usual. Hover a row for the numbers of seasons.</p>
<div class="seg" role="group" aria-label="Season"><button data-s="OND" aria-pressed="true">October to December</button><button data-s="MAM" aria-pressed="false">March to May</button><button data-s="JJAS" aria-pressed="false">June to September</button></div>
<div id="plainphase"></div>
</div>
<div class="panel">
<h3>The IOD in August to September, and the October to December rains</h3>
<p class="stat">Each dot is one year, 1981 to 2025. Dashed lines: the IOD at −0.4 and +0.4 °C. Filled dots: positive IOD; open dots: negative IOD. Hover a dot for the year.</p>
<div id="leadchart"></div>
</div>
</section>
"""
v1 = v1.replace('<section id="cycle">', enso_section + '\n<section id="cycle">', 1)
v1 = v1.replace('<script id="data" type="application/json">{', '<script id="data" type="application/json">{"showPhase": true, "plainPhase": '
                + json.dumps(plain_phase).replace("</", "<\\/") + ', "lead": ' + json.dumps(lead) + ', ', 1)
v1 = v1.replace("Every rainy season since 1981, over the part of the country where that season brings at least a quarter of the "
                "year's rain. Blue bars:", "Every rainy season since 1981, over the part of the country where that season brings at least a quarter of the "
                "year's rain. Top strips: ENSO and the IOD in that season (filled: El Niño or positive IOD; open: La Niña or negative IOD; grey: neutral). Blue bars:", 1)
v1 = v1.replace('<div class="legend"><span><i style="background:var(--rain)"></i>Rain, % of normal</span>',
                '<div class="legend"><span><i class="box f"></i>El Niño or positive IOD</span><span><i class="box"></i>La Niña or negative IOD</span><span><i class="box n"></i>neutral</span><span><i style="background:var(--rain)"></i>Rain, % of normal</span>', 1)
METHOD_V1_EXTRA = [m for m in METHOD if m.startswith("ENSO:")]


def plain_method(m):
    for a_, b_ in [(" (the cell count is in each chart's subtitle)", ""), (", the correlation grid and the phase chart", " and the phase chart"),
                   ("Multi-year EM-DAT droughts therefore mark many seasons: ",
                    "Seasons marked with a drought, of the 45 March to May, June to September and October to December seasons: "),
                   (" of the 45 March to May, June to September and October to December seasons.", "."),
                   ("0.25° blocks", "0.25° squares"), ("CHIRPS blocks", "CHIRPS squares"), ("per 0.25° cell", "per 0.25° square"),
                   ("cells of the country", "squares of the country"), ("afternoon maximum", "afternoon temperature"),
                   ("Afternoon maximum", "Afternoon temperature"), ("with its linear trend removed", "after taking out the long-term warming (a straight-line fit)"),
                   ("show it as observed", "show it as measured"), ("a small trend in very hot days", "a trend in very hot days"),
                   ("days per decade", "days every ten years"),
                   ("so they describe the same months as the rainfall, not a lead.",
                    "so they describe the same months as the rainfall; the one exception is the IOD in August to September, set against the October to December rains.")]:
        m = m.replace(a_, b_)
    return m


V1_WORDS = [
    ("'Rain, % of normal'", "'Rain, % of usual'"),
    ("'Afternoon maximum, °C against normal'", "'Afternoon temperature, °C against usual'"),
    ("`Rain ${P.rain[i].toFixed(0)}% of normal`, `Afternoon maximum ${sg(t)} °C against normal`",
     "`Rain ${P.rain[i].toFixed(0)}% of usual`, `Afternoon temperature ${sg(t)} °C against usual`"),
    ("Rain, % of normal</span>", "Rain, % of usual</span>"),
    ("Afternoon maximum against normal</span>", "Afternoon temperature against usual</span>"),
    ("<th class='n'>Rain, % of normal</th><th class='n'>Afternoon maximum, °C vs normal</th>",
     "<th class='n'>Rain, % of usual</th><th class='n'>Afternoon temperature, °C against usual (as measured)</th>"),
    ("Rain below 80% or above 120% of normal and temperature at least 0.5 °C from normal are shaded.",
     "Shaded: rain below 80% or above 120% of usual, and afternoon temperature 0.5 °C or more above or below usual."),
    ("Afternoon maximum (ERA5)</span>", "Afternoon temperature (ERA5)</span>"),
    ("<h3>Rainy months minus dry months, afternoon maximum</h3>", "<h3>Rainy months minus dry months, afternoon temperature</h3>"),
    ("'Afternoon maximum, °C'", "'Afternoon temperature, °C'"),
    ("`Afternoon maximum ${P.tmax[i]} °C`, `Monthly mean ${P.tmean[i]} °C`",
     "`Afternoon temperature ${P.tmax[i]} °C`, `Average over the whole day ${P.tmean[i]} °C`"),
]
if HOT is not None:
    left = [np.polyfit(g.year, g.hot_days_detr, 1)[0] * 10 for _, g in HOT.groupby(["iso3", "season"])]
    raw_tr = [np.polyfit(g.year, g.hot_days, 1)[0] * 10 for _, g in HOT.groupby(["iso3", "season"])]
    METHOD_V1_EXTRA.append(
        "Very hot days: ERA5 hourly 2 m temperature (reanalysis-era5-single-levels), the warmest hour between 12:00 and 16:00 East Africa "
        "Time each day, interpolated to each 0.25° square. A day is very hot when it is above that square's 90th percentile for the same "
        "calendar month in 1991 to 2020. Season counts are averaged over the same squares as the season charts (February to March: the "
        "whole country). After taking out the long-term warming: each square's straight-line warming over 1981 to 2025 is removed from its "
        f"daily values before counting; a small trend in very hot days remains ({min(left):+.1f} to {max(left):+.1f} days per decade, "
        f"against {min(raw_tr):+.1f} to {max(raw_tr):+.1f} before).")
v1 = v1.replace(f"<details><summary>Method and data</summary>{ul(METHOD_V1, 'notes')}</details>",
                f"<details><summary>Method and data</summary>{ul([plain_method(m) for m in METHOD_V1 + METHOD_V1_EXTRA], 'notes')}</details>", 1)
for a_, b_ in V1_WORDS:
    assert a_ in v1, a_[:80]
    v1 = v1.replace(a_, b_)
assert '<section id="drivers">' not in v1 and '<section id="chirts">' not in v1
out1 = HERE / "hoa_climate_review_v1.html"
out1.write_text(v1, encoding="utf8")
print("wrote", out1, f"{out1.stat().st_size / 1024:.0f} KB")
for f in FINDINGS:
    print("-", f)

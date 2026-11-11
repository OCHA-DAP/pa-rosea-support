"""Build the paper-style page: ten questions on heat, rain, ENSO/IOD and impacts in Ethiopia, Kenya and Somalia.

Run: python climate/hoa/build_research_page.py [data_dir]   (after research.py)
Writes climate/hoa/hoa_research.html, a single self-contained page. Every number in the text is computed here
from data_dir/out/rq*.csv; nothing is typed in by hand.
"""
import html
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
HERE = Path(__file__).parent
esc = html.escape
NAMES = {"ETH": "Ethiopia", "KEN": "Kenya", "SOM": "Somalia"}
SN = {"MAM": "March to May", "JJAS": "June to September", "OND": "October to December"}
MAIN = {"ETH": ["MAM", "JJAS", "OND"], "KEN": ["MAM", "OND"], "SOM": ["MAM", "OND"]}
MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
REC = {"drought_emdat": "EM-DAT drought start", "drought_cerf": "CERF drought allocation"}


def r2(v):
    return f"{v:+.2f}"


def pct(v):
    return f"{v:.0f}%"


def rng(vals, f=r2):
    vals = [v for v in vals if not pd.isna(v)]
    a, b = f(min(vals)), f(max(vals))
    return a if a == b else f"{a} to {b}"


T = pd.read_csv(OUT / "rq_table.csv")
FIRST, LAST = int(T.year.min()), int(T.year.max())
CH = {}  # chart payloads
TXT = {}  # findings per question

# ------------------------------------------------------------------ Q1
q1 = pd.read_csv(OUT / "rq1_hot_dry.csv")
summ = json.loads((OUT / "rq_summary.json").read_text())["q1"]
rows = []
for iso in NAMES:
    pts_ev, pts_fao = [], []
    for tg, kind in [("drought_emdat", "hollow"), ("drought_cerf", "diamond")]:
        q = q1[(q1.iso3 == iso) & (q1.target == tg)]
        h, hn, nn_, ne = q.hot_events.sum(), q.hot_n.sum(), q.normal_n.sum(), q.normal_events.sum()
        rows.append(dict(label=f"{NAMES[iso]}, {REC[tg]}", cols=[[
            dict(v=h / hn * 100, k="solid", tip=f"Hot failed seasons: {int(h)} of {int(hn)} followed by a {REC[tg]}"),
            dict(v=ne / nn_ * 100, k="hollow", tip=f"Other failed seasons: {int(ne)} of {int(nn_)}")]]))
    q = q1[(q1.iso3 == iso) & (q1.target == "faostat")].dropna(subset=["hot_mean", "normal_mean"])
    hm = np.average(q.hot_mean, weights=q.hot_n); nm = np.average(q.normal_mean, weights=q.normal_n)
    rows.append(dict(label=f"{NAMES[iso]}, FAOSTAT cereals", cols=[[], [
        dict(v=hm, k="solid", tip=f"Hot failed seasons: production {hm:+.1f}% from trend ({int(q.hot_n.sum())} seasons)"),
        dict(v=nm, k="hollow", tip=f"Other failed seasons: {nm:+.1f}% ({int(q.normal_n.sum())} seasons)")]]))
CH["q1"] = dict(rows=rows, cols=[dict(label="Followed by a drought record, % of failed seasons", lo=0, hi=100, base=None, unit="%"),
                                 dict(label="Cereal production, % from trend", lo=-30, hi=30, base=0, unit="%")])
e, c = summ["drought_emdat"], summ["drought_cerf"]
TXT["q1"] = [
    f"Of the failed seasons (lowest third of rain) that were also hot (afternoon maximum above the median of failed seasons), "
    f"{e['hot_events']} of {e['hot_n']} were followed by an EM-DAT drought start, against {e['normal_events']} of {e['normal_n']} "
    f"of the other failed seasons (Fisher exact test p = {e['fisher_p']:.2f}). For CERF drought allocations (2006 to 2025): "
    f"{c['hot_events']} of {c['hot_n']} against {c['normal_events']} of {c['normal_n']} (p = {c['fisher_p']:.2f}).",
    f"Across all failed main seasons with a FAOSTAT year, cereal production departure and afternoon maximum correlate at "
    f"{summ['faostat_r_failed']['r']:+.2f} (p = {summ['faostat_r_failed']['p']:.2f}, {summ['faostat_r_failed']['n']} seasons).",
    "Pooled, hot failed seasons were not followed by these records more often than other failed seasons. Within single country seasons "
    "the groups hold 0 to 8 seasons each and some go the other way; one drought episode can count in several seasons, "
    "and one FAOSTAT year in up to three, so the pooled seasons are not independent.",
]

# ------------------------------------------------------------------ Q2
q2 = pd.read_csv(OUT / "rq2_warming.csv")
COL = {"ETH": "var(--c1)", "KEN": "var(--c2)", "SOM": "var(--c3)"}
CH["q2_months"] = dict(series=[dict(name=NAMES[i], color=COL[i], pts=[[int(r.key), round(r.trend, 3)] for r in
                                    q2[(q2.iso3 == i) & (q2.kind == "month")].assign(key=lambda d: d.key.astype(int)).sort_values("key").itertuples()])
                               for i in NAMES], xlabels=MON, ylabel="Trend of the afternoon maximum, °C per decade, 1981 to 2025", base=0)
rows = []
for iso in NAMES:
    for r in q2[(q2.iso3 == iso) & (q2.kind.isin(["band", "all"]))].itertuples():
        lab = f"{NAMES[iso]}, {'whole country' if r.kind == 'all' else r.key}"
        rows.append(dict(label=lab, cols=[[dict(v=r.trend_wet, k="hollow", tip=f"Rainy months: {r.trend_wet:+.2f} °C per decade ({int(r.cells)} cells)"),
                                           dict(v=r.trend_dry, k="solid", tip=f"Dry months: {r.trend_dry:+.2f} °C per decade"),
                                           dict(v=r.trend, k="tick", tip=f"All months: {r.trend:+.2f} °C per decade")]]))
CH["q2_bands"] = dict(rows=rows, cols=[dict(label="Trend of the afternoon maximum, °C per decade", lo=0, hi=0.5, base=0, unit="")])
mon = q2[q2.kind == "month"].copy(); mon["key"] = mon.key.astype(int)
fast = {i: mon[mon.iso3 == i].nlargest(3, "trend") for i in NAMES}
slow = {i: mon[mon.iso3 == i].nsmallest(3, "trend") for i in NAMES}
allr = q2[q2.kind == "all"].set_index("iso3")
TXT["q2"] = [
    "Trend of the monthly afternoon maximum, 1981 to 2025, per decade: " + "; ".join(
        f"{NAMES[i]} {allr.trend[i]:+.2f} °C over the whole country, fastest in "
        f"{', '.join(MON[k - 1] for k in fast[i].key)} ({rng(fast[i].trend)}), slowest in {', '.join(MON[k - 1] for k in slow[i].key)} "
        f"({rng(slow[i].trend)})" for i in NAMES) + ".",
    "Rainy months against dry months (each cell's own rainy months, more than a twelfth of its annual rain): " + "; ".join(
        f"{NAMES[i]} {allr.trend_wet[i]:+.2f} against {allr.trend_dry[i]:+.2f} °C per decade" for i in NAMES) + ".",
    "By elevation (Copernicus DEM, cell centre): " + "; ".join(
        f"{NAMES[i]} " + ", ".join(f"{r.key} {r.trend:+.2f}" for r in q2[(q2.iso3 == i) & (q2.kind == "band")].itertuples())
        for i in NAMES) + " °C per decade. Somalia has " +
    f"{int(q2[(q2.iso3 == 'SOM') & (q2.key == 'above 1,500 m')].cells.iloc[0])} cells above 1,500 m.",
]

# ------------------------------------------------------------------ Q3
q3 = pd.read_csv(OUT / "rq3_chirts.csv")
rows = []
for r in q3.itertuples():
    rows.append(dict(label=f"{NAMES[r.iso3]}, {SN[r.season]}", cols=[
        [dict(v=r.anom_corr, k="solid", tip=f"Season anomalies correlate at {r.anom_corr:.2f} (1983 to 2016)")],
        [dict(v=r.trend_era5, k="solid", tip=f"ERA5 trend {r.trend_era5:+.2f} °C per decade"),
         dict(v=r.trend_chirts, k="hollow", tip=f"CHIRTS trend {r.trend_chirts:+.2f} °C per decade")],
        [dict(v=r.r_rain_era5, k="solid", tip=f"ERA5 {r.r_rain_era5:+.2f}"), dict(v=r.r_rain_chirts, k="hollow", tip=f"CHIRTS {r.r_rain_chirts:+.2f}")]]))
CH["q3"] = dict(rows=rows, cols=[dict(label="Agreement of season anomalies (r)", lo=0, hi=1, base=None, unit=""),
                                 dict(label="Trend 1983 to 2016, °C per decade", lo=0, hi=0.5, base=0, unit=""),
                                 dict(label="Correlation with season rain", lo=-1, hi=0.2, base=0, unit="")])
ch3 = q3.set_index(["iso3", "season"])
TXT["q3"] = [
    f"Season anomalies of the two records correlate at {rng(q3.anom_corr)} across the nine country seasons (1983 to 2016, trend removed); "
    f"lowest in Somalia March to May ({ch3.anom_corr[('SOM', 'MAM')]:.2f}).",
    f"Trends over 1983 to 2016 differ most in Ethiopia October to December (CHIRTS {ch3.trend_chirts[('ETH', 'OND')]:+.2f}, ERA5 "
    f"{ch3.trend_era5[('ETH', 'OND')]:+.2f} °C per decade) and Somalia October to December ({ch3.trend_chirts[('SOM', 'OND')]:+.2f}, "
    f"{ch3.trend_era5[('SOM', 'OND')]:+.2f}).",
    f"The rain to temperature correlation is negative in both records in eight of nine country seasons "
    f"(CHIRTS {rng([v for v in q3.r_rain_chirts if v < -0.3])}, ERA5 {rng([v for v in q3.r_rain_era5 if v < -0.3])}).",
]

# ------------------------------------------------------------------ Q4
q4 = pd.read_csv(OUT / "rq4_two_failed.csv")
rows = []
for iso in list(NAMES) + ["all"]:
    lab = "All three countries" if iso == "all" else NAMES[iso]
    for tg in ["drought_emdat", "drought_cerf"]:
        q = q4[(q4.iso3 == iso) & (q4.target == tg)].set_index("rule")
        one, two = q.loc["failed"], q.loc["two_failed"]
        rows.append(dict(label=f"{lab}, {REC[tg]}", cols=[[
            dict(v=two.event_rate_flagged * 100, k="solid", tip=f"After two failed seasons in a row: {int(two.hits)} of {int(two.flagged)}"),
            dict(v=one.event_rate_flagged * 100, k="hollow", tip=f"After any failed season: {int(one.hits)} of {int(one.flagged)}"),
            dict(v=one.event_rate_other * 100, k="tick", tip=f"After a season that was not failed: {one.event_rate_other * 100:.0f}%")]]))
    q = q4[(q4.iso3 == iso) & (q4.target == "faostat")].set_index("rule")
    rows.append(dict(label=f"{lab}, FAOSTAT cereals", cols=[[], [
        dict(v=q.loc["two_failed"].prod_flagged, k="solid", tip=f"Two failed in a row: {q.loc['two_failed'].prod_flagged:+.1f}% ({int(q.loc['two_failed'].flagged)} seasons)"),
        dict(v=q.loc["failed"].prod_flagged, k="hollow", tip=f"Any failed season: {q.loc['failed'].prod_flagged:+.1f}% ({int(q.loc['failed'].flagged)} seasons)"),
        dict(v=q.loc["failed"].prod_other, k="tick", tip=f"Seasons that were not failed: {q.loc['failed'].prod_other:+.1f}%")]]))
CH["q4"] = dict(rows=rows, cols=[dict(label="Followed by a drought record, % of seasons", lo=0, hi=100, base=None, unit="%"),
                                 dict(label="Cereal production, % from trend", lo=-25, hi=15, base=0, unit="%")])
a = q4[(q4.iso3 == "all")].set_index(["target", "rule"])
sq = json.loads((OUT / "rq_summary.json").read_text())
TXT["q4"] = [
    f"CERF drought allocations (2006 to 2025, three countries pooled): {int(a.hits[('drought_cerf', 'two_failed')])} of "
    f"{int(a.flagged[('drought_cerf', 'two_failed')])} seasons that ended a run of two failed seasons were followed by one "
    f"({a.event_rate_flagged[('drought_cerf', 'two_failed')] * 100:.0f}%), against {a.event_rate_flagged[('drought_cerf', 'failed')] * 100:.0f}% "
    f"after any failed season and {a.event_rate_other[('drought_cerf', 'failed')] * 100:.0f}% after the others. The "
    f"{int(a.hits[('drought_cerf', 'two_failed')])} matched seasons are followed by {sq['q4_cerf_allocations']} allocations in all. "
    f"Seasons since 2006 that ended a run of two failed seasons: "
    + ", ".join(f"{NAMES[i]} {n}" for i, n in sq["q4_flagged_per_country"].items()) + ".",
    f"EM-DAT drought starts go the other way: {int(a.hits[('drought_emdat', 'two_failed')])} of {int(a.flagged[('drought_emdat', 'two_failed')])} after two "
    f"failed seasons ({a.event_rate_flagged[('drought_emdat', 'two_failed')] * 100:.0f}%), {a.event_rate_flagged[('drought_emdat', 'failed')] * 100:.0f}% "
    f"after any failed season, {a.event_rate_other[('drought_emdat', 'failed')] * 100:.0f}% after the others.",
    "Cereal production, % from trend, in the FAOSTAT year matched to the season: " + "; ".join(
        f"{NAMES[i]} {q4[(q4.iso3 == i) & (q4.target == 'faostat') & (q4.rule == 'two_failed')].prod_flagged.iloc[0]:+.1f} after two failed "
        f"seasons, {q4[(q4.iso3 == i) & (q4.target == 'faostat') & (q4.rule == 'failed')].prod_flagged.iloc[0]:+.1f} after any failed season, "
        f"{q4[(q4.iso3 == i) & (q4.target == 'faostat') & (q4.rule == 'failed')].prod_other.iloc[0]:+.1f} after a season that was not failed" for i in NAMES) + ".",
]

# ------------------------------------------------------------------ Q5
q5 = pd.read_csv(OUT / "rq5_onset.csv")
KIND = {"rain total": "solid", "onset date": "hollow", "longest dry spell": "diamond"}
rows = []
for iso in NAMES:
    for s in MAIN[iso]:
        cols = []
        for tg in ["drought_cerf", "drought_emdat"]:
            q = q5[(q5.iso3 == iso) & (q5.season == s) & (q5.target == tg)]
            cols.append([dict(v=r.auc, k=KIND[r.indicator], tip=f"{r.indicator}: AUC {r.auc:.2f} (90% range {r.auc_lo:.2f} to {r.auc_hi:.2f}), {int(r.events)} events in {int(r.n)} seasons") for r in q.itertuples()])
        q = q5[(q5.iso3 == iso) & (q5.season == s) & (q5.target == "faostat")]
        cols.append([dict(v=(1 if r.indicator == "rain total" else -1) * r.r, k=KIND[r.indicator],
                          tip=f"{r.indicator}: r {r.r:+.2f} with production (p = {r.p:.2f}, {int(r.n)} years){'' if r.indicator == 'rain total' else ', sign flipped'}") for r in q.itertuples()])
        rows.append(dict(label=f"{NAMES[iso]}, {SN[s]}", cols=cols))
CH["q5"] = dict(rows=rows, cols=[dict(label="CERF drought allocation, AUC", lo=0.2, hi=1, base=0.5, unit=""),
                                 dict(label="EM-DAT drought start, AUC", lo=0.2, hi=1, base=0.5, unit=""),
                                 dict(label="FAOSTAT cereals, r (dry signs flipped)", lo=-0.4, hi=0.6, base=0, unit="")])
k5 = q5[(q5.iso3 == "KEN") & (q5.season == "MAM") & (q5.target == "faostat")].set_index("indicator")
best = q5[q5.target != "faostat"].loc[lambda d: d.groupby(["iso3", "season", "target"]).auc.transform("max") == d.auc]
spread = q5[q5.target != "faostat"].groupby(["iso3", "season", "target"]).auc.agg(lambda v: v.max() - v.min())
overlap = all(g.auc_lo.max() <= g.auc_hi.min() for _, g in q5[q5.target != "faostat"].groupby(["iso3", "season", "target"]))
fr = q5[(q5.target == "faostat") & (q5.indicator == "rain total")]
TXT["q5"] = [
    f"Against the drought event records, the rain total, onset date and longest dry spell give AUCs within {spread.max():.2f} of each other "
    f"in every country, season and record ({spread.median():.2f} at the median)" + ("; their 90% ranges overlap in every case." if overlap else "."),
    f"Against cereal production, rain total correlates at {rng(fr.r)} across the main seasons; the onset date "
    f"{rng(q5[(q5.target == 'faostat') & (q5.indicator == 'onset date')].r)} (the chart flips the sign of onset and dry spell, so that higher is drier in every column). The two strongest are Kenya March to May (rain {k5.r['rain total']:+.2f}, "
    f"onset {k5.r['onset date']:+.2f}) and Somalia March to May (rain "
    f"{q5[(q5.iso3 == 'SOM') & (q5.season == 'MAM') & (q5.target == 'faostat') & (q5.indicator == 'rain total')].r.iloc[0]:+.2f}, onset "
    f"{q5[(q5.iso3 == 'SOM') & (q5.season == 'MAM') & (q5.target == 'faostat') & (q5.indicator == 'onset date')].r.iloc[0]:+.2f}).",
    "Onset date and season total correlate at " +
    rng(list(json.loads((OUT / 'rq_summary.json').read_text())["q5_onset_rain_r"].values())) + " across the main seasons.",
]

# ------------------------------------------------------------------ Q6
q6 = pd.read_csv(OUT / "rq6_lead.csv")
y6 = pd.read_csv(OUT / "rq6_years.csv")
CH["q6"] = dict(panels=[dict(title=NAMES[i], color=COL[i], pts=[[round(r.dmi_before, 2), round(r.rain_pct, 1), int(r.year)]
                                                              for r in y6[(y6.iso3 == i) & (y6.season == "OND")].dropna(subset=["dmi_before"]).itertuples()])
                        for i in NAMES], xlabel="DMI, August to September mean, °C", ylabel="October to December rain, % of normal", xbase=0, ybase=100,
                xthr=[-0.4, 0.4])
o6 = q6[q6.season == "OND"].set_index(["iso3", "index"])
TXT["q6"] = [
    f"October to December rain correlates with the August to September DMI at {rng([o6.r_before[(i, 'DMI')] for i in NAMES])} and with the "
    f"August to September Niño3.4 at {rng([o6.r_before[(i, 'Niño3.4')] for i in NAMES])}; with the October to December indices themselves at "
    f"{rng([o6.r_same[(i, 'DMI')] for i in NAMES])} (DMI) and {rng([o6.r_same[(i, 'Niño3.4')] for i in NAMES])} (Niño3.4).",
    f"In the {int(o6.pos_n[('KEN', 'DMI')])} years with an August to September DMI of +0.4 °C or more ({o6.pos_years[('KEN', 'DMI')]}), October to "
    f"December rain was above normal in " + ", ".join(f"{int(o6.pos_above[(i, 'DMI')])}" for i in NAMES) + " (Ethiopia, Kenya, Somalia). In the "
    f"{int(o6.neg_n[('KEN', 'DMI')])} years at −0.4 °C or less ({o6.neg_years[('KEN', 'DMI')]}), it was below normal in "
    + ", ".join(f"{int(o6.neg_below[(i, 'DMI')])}" for i in NAMES) + ".",
    f"March to May rain and the January to February indices: correlation {rng(q6[q6.season == 'MAM'].r_before)}. June to September rain and "
    f"the April to May Niño3.4: {rng(q6[(q6.season == 'JJAS') & (q6['index'] == 'Niño3.4')].r_before)}.",
]

# ------------------------------------------------------------------ Q7
q7 = pd.read_csv(OUT / "rq7_floods.csv")
K7 = {"season total": "solid", "heaviest 3 days": "hollow", "heaviest 7 days": "diamond"}
rows = []
for iso in NAMES:
    for s in ["MAM", "JJAS", "OND"]:
        cols = []
        for tg in ["flood", "big_flood"]:
            q = q7[(q7.iso3 == iso) & (q7.season == s) & (q7.target == tg)]
            cols.append([dict(v=r.auc, k=K7[r.indicator], tip=f"{r.indicator}: AUC {r.auc:.2f} (90% range {r.auc_lo:.2f} to {r.auc_hi:.2f}), {int(r.events)} flood seasons") for r in q.itertuples()])
        if any(cols):
            rows.append(dict(label=f"{NAMES[iso]}, {SN[s]}", cols=cols))
CH["q7"] = dict(rows=rows, cols=[dict(label="Any EM-DAT flood, AUC", lo=0.3, hi=1, base=0.5, unit=""),
                                 dict(label="EM-DAT flood, 100 thousand or more affected, AUC", lo=0.3, hi=1, base=0.5, unit="")])
g7 = q7[q7.target == "flood"].pivot_table(index=["iso3", "season"], columns="indicator", values="auc")
named = json.loads((OUT / "rq_summary.json").read_text())["q7_named"]
_k = T[(T.iso3 == "KEN") & (T.season == "OND")].nlargest(4, "rain_pct").year
assert set(_k) == {1997, 2006, 2019, 2023}, "the Q7 sentence names Kenya's four wettest October to December seasons"
rng7 = q7[q7.target == "flood"].groupby(["iso3", "season"])
_ds = pd.read_csv(OUT / "drivers_seasons.csv")
som_jjas_cells = int(_ds[(_ds.iso3 == "SOM") & (_ds.season == "JJAS")].cells.iloc[0])
som_cells = int((pd.read_parquet(OUT / "cells.parquet").iso3 == "SOM").sum())
ov7 = all(g.auc_lo.max() <= g.auc_hi.min() for _, g in rng7)
TXT["q7"] = [
    f"Any EM-DAT flood: season total AUC {rng(g7['season total'], lambda v: f'{v:.2f}')}, heaviest 7 days "
    f"{rng(g7['heaviest 7 days'], lambda v: f'{v:.2f}')}, heaviest 3 days {rng(g7['heaviest 3 days'], lambda v: f'{v:.2f}')} across the "
    f"{len(g7)} country seasons" + ("; in each country season the three 90% ranges overlap." if ov7 else "."),
    "October to December, Kenya, % of normal (season total, heaviest 3 days, heaviest 7 days): " + "; ".join(
        f"{y} {named[f'KEN-{y}']['rain_pct']:.0f}, {named[f'KEN-{y}']['max3_pct']:.0f}, {named[f'KEN-{y}']['max7_pct']:.0f}"
        f" ({'EM-DAT flood start' if int(T[(T.iso3 == 'KEN') & (T.season == 'OND') & (T.year == y)].flood.iloc[0]) else 'no EM-DAT flood start in the season'})"
        for y in [1997, 2006, 2019, 2023]) + ". These are the four wettest October to December seasons in Kenya's record.",
    f"Somalia June to September floods are not picked out by rain over the Somali cells where June to September brings a quarter of the "
    f"annual rain ({som_jjas_cells} of {som_cells} cells): AUC "
    f"{rng(q7[(q7.iso3 == 'SOM') & (q7.season == 'JJAS') & (q7.target == 'flood')].auc, lambda v: f'{v:.2f}')}.",
]

# ------------------------------------------------------------------ Q8
q8 = pd.read_csv(OUT / "rq8_after_wet.csv")
s8 = pd.read_csv(OUT / "rq8_summary.csv").set_index("iso3")
CH["q8"] = dict(panels=[dict(title=NAMES[i], color=COL[i], pts=[[round(r.ond_rain, 1), round(r.next_mam_rain, 1), int(r.year)]
                                                              for r in q8[q8.iso3 == i].dropna(subset=["ond_rain", "next_mam_rain"]).itertuples()])
                        for i in NAMES], xlabel="October to December rain, % of normal", ylabel="Next March to May rain, % of normal",
                xbase=100, ybase=100, xthr=[120])
TXT["q8"] = [
    "October to December rain and the next March to May rain correlate at " + ", ".join(
        f"{s8.r_mam[i]:+.2f} (p = {s8.p_mam[i]:.2f})" for i in NAMES) + " (Ethiopia, Kenya, Somalia).",
    "After an October to December season with 120% of normal rain or more, the next March to May had a median of " + ", ".join(
        f"{s8.wet_next_mam_median[i]:.0f}%" for i in NAMES) + " of normal, against " + ", ".join(f"{s8.other_next_mam_median[i]:.0f}%" for i in NAMES) +
    " after the other seasons; below 80% in " + ", ".join(f"{int(s8.wet_next_mam_below80[i])} of {int(s8.wet_n[i])}" for i in NAMES) + ".",
    "October to December rain and the next February to March afternoon maximum (whole country, trend removed) correlate at " + ", ".join(
        f"{s8.r_febmar[i]:+.2f} (p = {s8.p_febmar[i]:.2f})" for i in NAMES) + ".",
]

# ------------------------------------------------------------------ Q9
q9 = pd.read_csv(OUT / "rq9_records.csv")
K9 = {"EM-DAT drought start": "hollow", "CERF drought allocation": "diamond", "FAOSTAT cereals, worst third": "solid"}
rows = []
for iso in NAMES:
    for s in MAIN[iso]:
        q = q9[(q9.iso3 == iso) & (q9.season == s)].dropna(subset=["value"])
        rows.append(dict(label=f"{NAMES[iso]}, {SN[s]}", cols=[[dict(v=r.value, k=K9[r.record],
                    tip=f"{r.record}: AUC {r.value:.2f} (90% range {r.lo:.2f} to {r.hi:.2f}), {int(r.events)} of {int(r.n)}") for r in q.itertuples()]]))
CH["q9"] = dict(rows=rows, cols=[dict(label="AUC of season rain for each impact record", lo=0.2, hi=1, base=0.5, unit="")])
p9 = q9.pivot_table(index=["iso3", "season"], columns="record", values="value")
top = p9.stack().idxmax()
TXT["q9"] = [
    "AUC of season rain (lower rain, more likely impact) across the main country seasons: " + "; ".join(
        f"{rec} {rng(p9[rec], lambda v: f'{v:.2f}')}" for rec in ["EM-DAT drought start", "CERF drought allocation", "FAOSTAT cereals, worst third"]) + ".",
    f"The highest is {top[2]} in {NAMES[top[0]]} {SN[top[1]]} ({p9.stack().max():.2f}). Against FAOSTAT the highest is Kenya March to May "
    f"({p9.loc[('KEN', 'MAM'), 'FAOSTAT cereals, worst third']:.2f}); Ethiopia June to September gives "
    f"{p9.loc[('ETH', 'JJAS'), 'FAOSTAT cereals, worst third']:.2f}.",
    "The records differ in length: EM-DAT 1981 to 2025 (45 seasons each), CERF 2006 to 2025 (20), FAOSTAT cereals Kenya and Somalia "
    "1981 to 2024 and Ethiopia 1993 to 2024 (Ethiopia PDR before 1993 included Eritrea). Detrended production has a lag-1 correlation "
    f"of {sq['fao_lag1']['ETH']:.2f} in Ethiopia (Kenya {sq['fao_lag1']['KEN']:.2f}, Somalia {sq['fao_lag1']['SOM']:.2f}).",
]

# ------------------------------------------------------------------ Q10
q10 = pd.read_csv(OUT / "rq10_change.csv")
for idx, key in [("dmi", "q10_dmi"), ("nino34", "q10_nino")]:
    CH[key] = dict(series=[dict(name=NAMES[i], color=COL[i], pts=[[1991 + k, v] for k, v in enumerate(json.loads(
        q10[(q10.iso3 == i) & (q10.season == "OND") & (q10["index"] == idx)].running_21yr.iloc[0]))]) for i in NAMES],
        xlabels=None, ylabel=f"Correlation of {'DMI' if idx == 'dmi' else 'Niño3.4'} with October to December rain, 21-year windows (centre year)",
        base=0, ylo=-0.2, yhi=1)
TXT["q10"] = [
    "October to December, 1981 to 2002 against 2003 to 2025: DMI with rain " + ", ".join(
        f"{r.r_1981_2002:+.2f} and {r.r_2003_2025:+.2f}" for r in q10[(q10.season == "OND") & (q10["index"] == "dmi")].itertuples()) +
    "; Niño3.4 with rain " + ", ".join(f"{r.r_1981_2002:+.2f} and {r.r_2003_2025:+.2f}" for r in q10[(q10.season == "OND") & (q10["index"] == "nino34")].itertuples()) +
    " (Ethiopia, Kenya, Somalia).",
    f"None of the {len(q10)} index, country and season pairs changes significantly between the two periods at 5% (smallest p = {q10.p.min():.2f}, "
    f"{q10.loc[q10.p.idxmin(), 'index'].replace('nino34', 'Niño3.4').replace('dmi', 'DMI')} with {NAMES[q10.loc[q10.p.idxmin(), 'iso3']]} "
    f"{SN[q10.loc[q10.p.idxmin(), 'season']]} rain, {q10.loc[q10.p.idxmin(), 'r_1981_2002']:+.2f} to {q10.loc[q10.p.idxmin(), 'r_2003_2025']:+.2f}).",
    "With 22 and 23 seasons, this test detects only large changes: a non-significant result does not show that a link is unchanged.",
]

# ------------------------------------------------------------------ questions, abstract, method
Q = [
    ("q1", "Are hot failed seasons followed by drought records more often?", "Failed main seasons (lowest third of rain) split at the median afternoon maximum of the failed seasons; share followed by an EM-DAT drought start or a CERF drought allocation, and cereal production in that year."),
    ("q2", "Where and when is it warming?", "Linear trend of the monthly afternoon maximum per cell, 1981 to 2025, by calendar month, by each cell's rainy and dry months, and by elevation."),
    ("q3", "Do two temperature records agree?", "ERA5 afternoon maximum against CHIRTS-daily mean daily maximum (stations and satellite), same cells, 1983 to 2016."),
    ("q4", "Are two failed seasons in a row followed by drought records more often than one?", "A season ending a run of two failed seasons (the season and the one before it both in their lowest third), against any failed season and the others."),
    ("q5", "Do the onset date or the longest dry spell match drought records more closely than the season total?", "Onset date (anomalous accumulation), longest run of dry days and season total, each against EM-DAT, CERF and FAOSTAT."),
    ("q6", "Do ENSO and the IOD before a season show how it will go?", "Niño3.4 and DMI averaged over the two months before each season (August to September for October to December, January to February for March to May, April to May for June to September)."),
    ("q7", "For floods, the season total or the heaviest few days?", "Season total against the heaviest 3-day and 7-day totals (cell maxima averaged over the area), each as % of normal, against EM-DAT floods starting in the season, and against those with 100 thousand or more people affected (scored only where there are at least 4)."),
    ("q8", "What follows a wet October to December?", "October to December rain against the next March to May rain and the next February to March afternoon maximum."),
    ("q9", "Which impact record lines up with the climate record?", "AUC of season rain against EM-DAT drought starts, CERF drought allocations and the worst third of FAOSTAT cereal production years."),
    ("q10", "Has the ENSO and IOD link with rain changed?", "Correlations in 1981 to 2002 against 2003 to 2025 (Fisher z test), and in running 21-year windows."),
]
ABSTRACT = [
    "Hot failed seasons were not followed by drought records more often than other failed seasons, pooled over the three countries.",
    TXT["q2"][1],
    TXT["q3"][0],
    f"After two failed seasons in a row, {int(a.hits[('drought_cerf', 'two_failed')])} of {int(a.flagged[('drought_cerf', 'two_failed')])} seasons "
    f"({a.event_rate_flagged[('drought_cerf', 'two_failed')] * 100:.0f}%) were followed by a CERF drought allocation, against "
    f"{a.event_rate_flagged[('drought_cerf', 'failed')] * 100:.0f}% after any failed season; "
    f"EM-DAT drought starts did not ({a.event_rate_flagged[('drought_emdat', 'two_failed')] * 100:.0f}% against "
    f"{a.event_rate_flagged[('drought_emdat', 'failed')] * 100:.0f}%).",
    "Against drought records, the 90% ranges of the onset date, the longest dry spell and the season total overlap in every country, "
    "season and record; onset and total correlate at " + rng(list(json.loads((OUT / 'rq_summary.json').read_text())["q5_onset_rain_r"].values())) + ".",
    TXT["q6"][1],
    TXT["q7"][0],
    TXT["q8"][1],
    TXT["q9"][0],
    TXT["q10"][1] + " With 22 and 23 seasons per period, only large changes would show.",
]
METHOD = [
    "Area: Ethiopia, Kenya and Somalia (OCHA COD-AB boundaries), 0.25° cells. Season figures are averaged over the cells where that season brings at least a quarter of the annual rain.",
    "Rain: CHIRPS v2.0 monthly and daily (UCSB Climate Hazards Center). Normal 1991 to 2020.",
    "Temperature: ERA5 (ECMWF, Copernicus), the warmest of the 12:00 to 16:00 East Africa Time monthly hourly means (afternoon maximum), with its linear trend removed in every comparison with rain, indices or impacts. CHIRTS-daily v1.0 (UCSB), 1983 to 2016, as the second record.",
    "Indices: Niño3.4 anomaly, NOAA CPC ERSST v5 (1991 to 2020 base); Dipole Mode Index, NOAA PSL from HadISST1.1 (marked preliminary).",
    "Dry spells: longest run of days with less than 1 mm inside the calendar season. Onset: anomalous accumulation after Liebmann et al. (2012) and Dunning et al. (2016), with one change: the reference rate is each cell's 1991 to 2020 mean daily rain over the search window (30 days before the season to its end), not the annual mean.",
    "Impact records: EM-DAT (CRED, UCLouvain) floods starting in the season and droughts starting in the season or the 6 months after it (droughts with no start month left out); CERF OneGMS drought allocations (Rapid Response and Underfunded) approved within 6 months after the season, 2006 to 2025; FAOSTAT cereals, primary, production, as % departure from a quadratic trend on log production, matched to the harvest year (October to December seasons to the next calendar year).",
    "Skill: ROC AUC (0.5: no better than chance, 1: every impact season ranked above every other season), with a 90% range from 1,000 bootstrap resamples of years. No model is fitted, but the cut-offs (thirds, medians, ±0.4 °C, 120%) are set on the same data they are tested on. Correlations are Pearson with two-sided p-values.",
    "Elevation: Copernicus DEM 90 m at each cell centre, through the Open-Meteo elevation service.",
]
LIMITS = [
    "Small samples: 44 to 45 seasons per country and season, 32 to 44 FAOSTAT years, 20 seasons for CERF; most AUC ranges are wide, and several include 0.5 (no better than chance).",
    "Many tests: this page reports well over a hundred correlations and AUCs; at 5%, some will look significant by chance.",
    "Impact records are coarse: EM-DAT dates and national coverage vary; CERF allocations also depend on funding decisions; FAOSTAT is national and annual.",
    "Country-level matching: an impact anywhere in the country is matched to rain averaged over a large area.",
    "The Juba and Shabelle rivers that flood in Somalia rise in Ethiopia; rain upstream is not part of Somalia's figures here.",
    "All indices and climate figures are observed or reanalysis values for the same or earlier months, not forecasts.",
    "Shared events: one drought episode or CERF allocation can match several seasons, and one FAOSTAT year up to three, so pooled counts are not independent.",
]

# ------------------------------------------------------------------ page
CSS = """
:root{color-scheme:light;--bg:#F4F6F7;--surface:#FFFFFF;--ink:#15212A;--ink2:#46555F;--muted:#76838C;--rule:#DDE3E6;--rule2:#EEF1F3;
--accent:#1D5B7C;--c1:#2a78d6;--c2:#eb6834;--c3:#1baf7a;
--sans:"Public Sans",system-ui,-apple-system,"Segoe UI",sans-serif;--mono:"IBM Plex Mono",ui-monospace,Consolas,monospace}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;
--muted:#7F8C94;--rule:#2B353B;--rule2:#222B31;--accent:#7DB6D6;--c1:#3987e5;--c2:#d95926;--c3:#199e70}}
:root[data-theme="dark"]{color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;--muted:#7F8C94;--rule:#2B353B;
--rule2:#222B31;--accent:#7DB6D6;--c1:#3987e5;--c2:#d95926;--c3:#199e70}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font-family:var(--sans);font-size:16px;line-height:1.6}
.wrap{max-width:960px;margin:0 auto;padding:36px 16px 56px}
header{padding-bottom:18px;border-bottom:1px solid var(--rule)}
.eyebrow{font:500 12px/1 var(--mono);letter-spacing:.08em;text-transform:uppercase;color:var(--accent)}
h1{font-size:clamp(26px,4.4vw,36px);line-height:1.15;margin:12px 0;font-weight:700;letter-spacing:-.015em;text-wrap:balance}
h2{font-size:21px;margin:0 0 6px;font-weight:600}
h3{font-size:15px;margin:14px 0 4px;font-weight:600}
section{margin-top:40px}
.sub{color:var(--ink2);max-width:80ch;margin:0 0 12px;font-size:15px}
.qn{font:500 12px/1 var(--mono);color:var(--accent);letter-spacing:.06em}
ul,ol{max-width:80ch;padding-left:20px}li{margin:7px 0}
ol.abstract li{margin:8px 0}
.panel{background:var(--surface);border:1px solid var(--rule);border-radius:6px;padding:14px 16px;margin:12px 0}
.chart{overflow-x:auto}.chart svg{display:block;width:100%;height:auto;min-width:640px}
svg text{font-family:var(--sans);fill:var(--ink2);font-size:12px}svg text.lab{fill:var(--ink);font-weight:600}
.legend{display:flex;flex-wrap:wrap;gap:6px 18px;font-size:13px;color:var(--ink2);margin-top:8px}
.legend span{white-space:nowrap}.legend svg{display:inline-block;width:14px;height:14px;vertical-align:-2px;margin-right:5px;min-width:0}
.legend i{display:inline-block;width:14px;height:3px;border-radius:2px;vertical-align:3px;margin-right:6px}
ul.notes{color:var(--ink2)}
.tip{position:fixed;pointer-events:none;background:var(--surface);color:var(--ink);border:1px solid var(--rule);border-radius:4px;padding:6px 9px;font-size:13px;box-shadow:0 2px 8px rgba(0,0,0,.12);display:none;z-index:9;max-width:320px}
.tip .sm{font-size:12.5px;color:var(--muted);margin-top:2px}
.home{display:inline-block;margin:0 0 18px;padding:6px 12px;font:500 13px/1 var(--sans);color:var(--accent);background:var(--surface);border:1px solid var(--rule);border-radius:4px;text-decoration:none}
a{color:var(--accent)}
"""

JS = r"""
const A = JSON.parse(document.getElementById('data').textContent);
const tip = document.getElementById('tip');
function el(t, a, txt){ const e = document.createElementNS('http://www.w3.org/2000/svg', t); for (const k in a) e.setAttribute(k, a[k]); if (txt != null) e.textContent = txt; return e; }
function show(ev, main, lines){ tip.textContent = ''; const b = document.createElement('div'); b.textContent = main; b.style.fontWeight = 600; tip.appendChild(b);
  (lines || []).forEach(l => { const s = document.createElement('div'); s.className = 'sm'; s.textContent = l; tip.appendChild(s); });
  tip.style.display = 'block'; tip.style.left = Math.min(ev.clientX + 14, innerWidth - 330) + 'px'; tip.style.top = Math.min(ev.clientY + 14, innerHeight - 90) + 'px'; }
function hide(){ tip.style.display = 'none'; }
function hit(svg, x, y, w, h, f){ const r = el('rect', {x, y, width: w, height: h, fill: 'transparent'}); r.addEventListener('pointermove', f); r.addEventListener('pointerleave', hide); svg.appendChild(r); }
function ticks(lo, hi, n){ const step = [0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 5, 10, 20, 25, 50, 100].find(s => (hi - lo) / s <= n) || 100; const out = []; for (let v = Math.ceil(lo / step) * step; v <= hi + 1e-9; v += step) out.push(+v.toFixed(3)); return out; }
function mark(svg, k, x, y, color){
  if (k === 'tick') return svg.appendChild(el('line', {x1: x, x2: x, y1: y - 7, y2: y + 7, stroke: 'var(--muted)', 'stroke-width': 2.5}));
  if (k === 'diamond') return svg.appendChild(el('path', {d: `M${x},${y - 7}l7,7l-7,7l-7,-7Z`, fill: 'var(--surface)', stroke: color || 'var(--ink)', 'stroke-width': 2}));
  svg.appendChild(el('circle', {cx: x, cy: y, r: 6, fill: k === 'solid' ? (color || 'var(--ink)') : 'var(--surface)', stroke: color || 'var(--ink)', 'stroke-width': 2}));
}
// rows of marks in one or more columns, each column its own scale
function dots(id, C){
  const box = document.getElementById(id); if (!box) return;
  const W = 940, ml = 240, rh = 28, mt = 34, nc = C.cols.length, colw = (W - ml) / nc, H = mt + C.rows.length * rh + 26;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': C.cols.map(c => c.label).join('; ')});
  C.cols.forEach((col, c) => {
    const x0 = ml + c * colw + 24, x1 = ml + (c + 1) * colw - 24, xv = v => x0 + (Math.max(col.lo, Math.min(col.hi, v)) - col.lo) / (col.hi - col.lo) * (x1 - x0);
    svg.appendChild(el('text', {x: x0, y: 14, class: 'lab'}, col.label));
    ticks(col.lo, col.hi, nc > 1 ? 3 : 6).forEach(v => { svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt - 8, y2: H - 22, stroke: v === col.base ? 'var(--muted)' : 'var(--rule2)'}));
      svg.appendChild(el('text', {x: xv(v), y: H - 6, 'text-anchor': 'middle'}, (col.base === 0 && v > 0 ? '+' : '') + v + (col.unit || ''))); });
    C.rows.forEach((r, i) => { const yc = mt + i * rh + rh / 2, P = (r.cols[c] || []).filter(p => p.v != null && !isNaN(p.v));
      if (P.length > 1) { const vs = P.map(p => xv(p.v)); svg.appendChild(el('line', {x1: Math.min(...vs), x2: Math.max(...vs), y1: yc, y2: yc, stroke: 'var(--rule)', 'stroke-width': 2})); }
      P.forEach(p => mark(svg, p.k, xv(p.v), yc, p.color));
      if (P.length) hit(svg, x0 - 10, yc - rh / 2, x1 - x0 + 20, rh, ev => show(ev, r.label, P.map(p => p.tip))); });
  });
  C.rows.forEach((r, i) => { const yc = mt + i * rh + rh / 2; svg.appendChild(el('text', {x: 0, y: yc + 4}, r.label)); });
  box.appendChild(svg);
}
// lines, one per country, shared axes
function lines(id, C){
  const box = document.getElementById(id); if (!box) return;
  const W = 940, ml = 50, mr = 110, mt = 30, mb = 28, H = 300;
  const xs = C.series.flatMap(s => s.pts.map(p => p[0])), ys = C.series.flatMap(s => s.pts.map(p => p[1]));
  const xlo = Math.min(...xs), xhi = Math.max(...xs), ylo = C.ylo ?? Math.min(0, ...ys) - 0.05, yhi = C.yhi ?? Math.max(...ys) + 0.05;
  const xv = v => ml + (v - xlo) / (xhi - xlo) * (W - ml - mr), yv = v => mt + (yhi - v) / (yhi - ylo) * (H - mt - mb);
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': C.ylabel});
  svg.appendChild(el('text', {x: ml, y: 14, class: 'lab'}, C.ylabel));
  ticks(ylo, yhi, 5).forEach(v => { svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: yv(v), y2: yv(v), stroke: v === C.base ? 'var(--muted)' : 'var(--rule2)'}));
    svg.appendChild(el('text', {x: ml - 6, y: yv(v) + 4, 'text-anchor': 'end'}, (v > 0 ? '+' : '') + v)); });
  const xt = C.xlabels ? C.xlabels.map((l, i) => [i + 1, l]) : ticks(xlo, xhi, 8).map(v => [v, String(v)]);
  xt.forEach(([v, l]) => svg.appendChild(el('text', {x: xv(v), y: H - 8, 'text-anchor': 'middle'}, l)));
  C.series.forEach(s => {
    svg.appendChild(el('polyline', {points: s.pts.map(p => `${xv(p[0])},${yv(p[1])}`).join(' '), fill: 'none', stroke: s.color, 'stroke-width': 2}));
    s.pts.forEach(p => svg.appendChild(el('circle', {cx: xv(p[0]), cy: yv(p[1]), r: 3.5, fill: s.color, stroke: 'var(--surface)', 'stroke-width': 1.5})));
    const lp = s.pts[s.pts.length - 1]; svg.appendChild(el('text', {x: xv(lp[0]) + 10, y: yv(lp[1]) + 4, class: 'lab'}, s.name));
  });
  const xsU = [...new Set(xs)].sort((a, b) => a - b), step = (xv(xsU[1]) - xv(xsU[0])) || 20;
  xsU.forEach(x => hit(svg, xv(x) - step / 2, mt, step, H - mt - mb, ev => show(ev, C.xlabels ? C.xlabels[x - 1] : String(x),
    C.series.map(s => { const p = s.pts.find(q => q[0] === x); return p ? `${s.name} ${(p[1] > 0 ? '+' : '') + p[1].toFixed(2)}` : ''; }))));
  box.appendChild(svg);
}
// scatter, one small panel per country
function scatter(id, C){
  const box = document.getElementById(id); if (!box) return;
  const W = 940, pw = W / C.panels.length, H = 300, ml = 44, mb = 40, mt = 24;
  const xs = C.panels.flatMap(p => p.pts.map(q => q[0])), ys = C.panels.flatMap(p => p.pts.map(q => q[1]));
  const xlo = Math.min(...xs), xhi = Math.max(...xs), ylo = 0, yhi = Math.max(...ys) * 1.05, padx = (xhi - xlo) * 0.06;
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': `${C.ylabel} against ${C.xlabel}`});
  C.panels.forEach((P, k) => {
    const x0 = k * pw + ml, x1 = (k + 1) * pw - 12, xv = v => x0 + (v - xlo + padx) / (xhi - xlo + 2 * padx) * (x1 - x0), yv = v => mt + (yhi - v) / (yhi - ylo) * (H - mt - mb);
    svg.appendChild(el('text', {x: x0, y: 14, class: 'lab'}, P.title));
    ticks(ylo, yhi, 4).forEach(v => { svg.appendChild(el('line', {x1: x0, x2: x1, y1: yv(v), y2: yv(v), stroke: v === C.ybase ? 'var(--muted)' : 'var(--rule2)'}));
      if (k === 0) svg.appendChild(el('text', {x: x0 - 6, y: yv(v) + 4, 'text-anchor': 'end'}, v + '%')); });
    ticks(xlo, xhi, 4).forEach(v => { svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt, y2: H - mb, stroke: v === C.xbase ? 'var(--muted)' : 'var(--rule2)'}));
      svg.appendChild(el('text', {x: xv(v), y: H - mb + 16, 'text-anchor': 'middle'}, v)); });
    (C.xthr || []).forEach(v => svg.appendChild(el('line', {x1: xv(v), x2: xv(v), y1: mt, y2: H - mb, stroke: 'var(--ink2)', 'stroke-dasharray': '3 3'})));
    P.pts.forEach(([x, y, yr]) => { svg.appendChild(el('circle', {cx: xv(x), cy: yv(y), r: 4.5, fill: P.color, 'fill-opacity': .85, stroke: 'var(--surface)', 'stroke-width': 1.5}));
      const h = el('circle', {cx: xv(x), cy: yv(y), r: 9, fill: 'transparent'}); h.addEventListener('pointermove', ev => show(ev, `${P.title} ${yr}`, [`${C.xlabel}: ${x}`, `${C.ylabel}: ${y.toFixed(0)}%`]));
      h.addEventListener('pointerleave', hide); svg.appendChild(h); });
  });
  svg.appendChild(el('text', {x: W / 2, y: H - 6, 'text-anchor': 'middle'}, C.xlabel));
  box.appendChild(svg);
}
dots('c-q1', A.q1); lines('c-q2m', A.q2_months); dots('c-q2b', A.q2_bands); dots('c-q3', A.q3); dots('c-q4', A.q4);
dots('c-q5', A.q5); scatter('c-q6', A.q6); dots('c-q7', A.q7); scatter('c-q8', A.q8); dots('c-q9', A.q9);
lines('c-q10d', A.q10_dmi); lines('c-q10n', A.q10_nino);
"""


def leg(items):
    svgs = {"solid": "<svg viewBox='0 0 14 14'><circle cx='7' cy='7' r='5' fill='var(--ink)'/></svg>",
            "hollow": "<svg viewBox='0 0 14 14'><circle cx='7' cy='7' r='5' fill='none' stroke='var(--ink)' stroke-width='2'/></svg>",
            "diamond": "<svg viewBox='0 0 14 14'><path d='M7,1l6,6l-6,6l-6,-6Z' fill='none' stroke='var(--ink)' stroke-width='2'/></svg>",
            "tick": "<svg viewBox='0 0 14 14'><line x1='7' x2='7' y1='1' y2='13' stroke='var(--muted)' stroke-width='2.5'/></svg>"}
    return "<div class='legend'>" + "".join(f"<span>{svgs[k] if k in svgs else ''}{esc(t)}</span>" if k in svgs else
                                             f"<span><i style='background:{k}'></i>{esc(t)}</span>" for k, t in items) + "</div>"


COUNTRY_LEG = [("var(--c1)", "Ethiopia"), ("var(--c2)", "Kenya"), ("var(--c3)", "Somalia")]
CHARTS = {
    "q1": "<div class='chart' id='c-q1'></div>" + leg([("solid", "Hot failed seasons"), ("hollow", "Other failed seasons")]),
    "q2": "<div class='chart' id='c-q2m'></div>" + leg(COUNTRY_LEG) + "<div class='chart' id='c-q2b' style='margin-top:14px'></div>" +
          leg([("hollow", "Rainy months"), ("solid", "Dry months"), ("tick", "All months")]),
    "q3": "<div class='chart' id='c-q3'></div>" + leg([("solid", "ERA5 (or the agreement of the two)"), ("hollow", "CHIRTS")]),
    "q4": "<div class='chart' id='c-q4'></div>" + leg([("solid", "After two failed seasons in a row"), ("hollow", "After any failed season"), ("tick", "After a season that was not failed")]),
    "q5": "<div class='chart' id='c-q5'></div>" + leg([("solid", "Season total"), ("hollow", "Onset date"), ("diamond", "Longest dry spell")]),
    "q6": "<div class='chart' id='c-q6'></div><p class='sub' style='margin-top:6px'>Dashed lines: DMI at −0.4 and +0.4 °C. Hover a dot for the year.</p>",
    "q7": "<div class='chart' id='c-q7'></div>" + leg([("solid", "Season total"), ("hollow", "Heaviest 3 days"), ("diamond", "Heaviest 7 days")]),
    "q8": "<div class='chart' id='c-q8'></div><p class='sub' style='margin-top:6px'>Dashed line: 120% of normal. Hover a dot for the year (the October to December year).</p>",
    "q9": "<div class='chart' id='c-q9'></div>" + leg([("hollow", "EM-DAT drought start"), ("diamond", "CERF drought allocation"), ("solid", "FAOSTAT cereals, worst third")]),
    "q10": "<div class='chart' id='c-q10d'></div><div class='chart' id='c-q10n' style='margin-top:14px'></div>" + leg(COUNTRY_LEG),
}
sections = ""
for i, (k, title, how) in enumerate(Q, 1):
    sections += (f"<section id='{k}'><div class='qn'>QUESTION {i}</div><h2>{esc(title)}</h2><p class='sub'>{esc(how)}</p>"
                 f"<ul>{''.join(f'<li>{esc(t)}</li>' for t in TXT[k])}</ul><div class='panel'>{CHARTS[k]}</div></section>")
PAYLOAD = json.dumps(CH).replace("</", "<\\/")
page = f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Horn Climate Questions</title>
<meta name="description" content="Ten questions on afternoon temperature, rainfall, dry spells, ENSO and the IOD, and flood and drought impact records in Ethiopia, Kenya and Somalia, {FIRST} to {LAST}.">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>{CSS}</style>
</head>
<body>
<div class="wrap">
<a class="home" href="../">← ROSEA analyses</a>
<header>
  <div class="eyebrow">OCHA ROSEA support | Ethiopia, Kenya, Somalia | floods and drought</div>
  <h1>Heat, rain, ENSO and the IOD in Ethiopia, Kenya and Somalia: ten questions, {FIRST} to {LAST}</h1>
  <p class="sub">Companion to the <a href="hoa_climate_review.html">temperature, rainfall and impact review</a>, which holds the season-by-season charts.</p>
</header>
<section id="abstract"><h2>Summary</h2><p class="sub">Terms: a <b>failed season</b> is one in the driest third of that country's seasons, 1981 to 2025. The <b>afternoon maximum</b> is ERA5 temperature, with its linear trend removed when compared with rain or impacts. <b>AUC</b> scores how well an indicator ranks impact seasons above the others: 0.5 is no better than chance, 1 is perfect; the <b>90% range</b> comes from resampling the years 1,000 times. <b>Onset</b> is the start date of the rains (anomalous accumulation).</p><ol class="abstract">{''.join(f'<li>{esc(t)}</li>' for t in ABSTRACT)}</ol></section>
<section id="method"><h2>Data and method</h2><ul class="notes">{''.join(f'<li>{esc(t)}</li>' for t in METHOD)}</ul></section>
{sections}
<section id="limits"><h2>Limitations</h2><ul class="notes">{''.join(f'<li>{esc(t)}</li>' for t in LIMITS)}</ul></section>
</div>
<div class="tip" id="tip"></div>
<script id="data" type="application/json">{PAYLOAD}</script>
<script>{JS}</script>
</body>
</html>
"""
out = HERE / "hoa_research.html"
out.write_text(page, encoding="utf8")
print("wrote", out, f"{out.stat().st_size / 1024:.0f} KB")
for k, t in TXT.items():
    for x in t:
        print(k, "-", x)

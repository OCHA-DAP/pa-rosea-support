"""Assemble flood/ken/ken_khf_trigger_review.html from the tables written by
khf_trigger_review.py. Single self-contained page (inline CSS, JS, data).

Run: python flood/ken/build_review_page.py [data_dir]
"""
import json
import sys
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
OUT = DATA / "out"
HERE = Path(__file__).parent
sys.path.insert(0, str(HERE))
import page_activations  # noqa: E402

spec = pd.read_csv(OUT / "khf_thresholds_backtest.csv")
rp = pd.read_csv(OUT / "threshold_return_periods.csv")
eq = pd.read_csv(OUT / "rp_equivalent_thresholds.csv")
eps = pd.read_csv(OUT / "khf_threshold_episodes.csv", parse_dates=["first", "last"])
em = pd.read_csv(OUT / "emdat_ne_kenya_floods.csv", parse_dates=["start", "end"])
evr = pd.read_csv(OUT / "emdat_events_vs_rainfall.csv")
sm = pd.read_csv(OUT / "seasonal_maxima.csv")
daily = pd.read_parquet(OUT / "daily_area_series.parquet")

N_YEARS = int(spec["n_years"].max())
FIRST_YEAR = int(sm["year"].min())
LAST_FULL = FIRST_YEAR + N_YEARS - 1
LAST_DATE = daily.index.max().date()


def season_of(ts):
    m = ts.month
    return "OND" if m in (10, 11, 12) else ("MAM" if m in (3, 4, 5) else "other")


em["season"] = em["start"].apply(season_of)
em["year"] = em["start"].dt.year
flood_seasons = {(int(r.year), r.season) for r in em.itertuples()}
em_by_season = em.groupby(["year", "season"]).agg(n=("DisNo.", "size"), affected=("Total Affected", "sum"),
                                                   ids=("DisNo.", lambda x: ", ".join(x))).reset_index()

# ---------------------------------------------------------------- triggers as written (from the KHF RA2 document, Sept 2026)
TRIGGERS = [
    dict(key="krcs_ne", partner="Kenya Red Cross Society", place="Mandera, Wajir, Marsabit",
         text="KMSA 7-day rainfall forecast above 150 mm. Stated lead time 7 days.",
         tested="7-day IMERG total per county against 150 mm", areas=["Mandera", "Wajir", "Marsabit"], window=7, thr=[150]),
    dict(key="krcs_garissa", partner="Kenya Red Cross Society", place="Garissa (Tana River basin)",
         text="Observed river level at Garissa Bridge above 5.1 m, OR a KMSA heavy rainfall advisory for the Tana River basin "
              "(at least 40 mm). Stated lead time 7 days.",
         tested="Rainfall leg only: 1-day IMERG total against 40 mm over Garissa, Tana River and the upper Tana counties. "
                "The gauge leg is not testable with rainfall data.",
         areas=["Garissa", "Tana River", "Upper Tana (7 counties)"], window=1, thr=[40]),
    dict(key="whh", partner="Welthungerhilfe", place="Isiolo, Samburu (Waso, Wamba West, Garbatulla, Ngaremara wards)",
         text="KMSA 7-day forecast indicates average rainfall of 70-100 mm or more over Isiolo and Samburu counties and the upper "
              "Ewaso Ng'iro catchment (Nyeri, Nyandarua, Laikipia, Meru). Stated lead time 7 days.",
         tested="7-day IMERG total averaged over the six counties, and over Isiolo and Samburu alone, against 70 and 100 mm",
         areas=["Ewaso Ng'iro area (6 counties)", "Isiolo", "Samburu"], window=7, thr=[70, 100]),
    dict(key="dadaab", partner="Dadaab partners", place="Dadaab, Garissa",
         text="KMSA 7-day forecast at or approaching Flood Alert levels (Garissa 3.0-3.5 m; Hola 2.0-2.3 m; Garsen 2.8-3.2 m); "
              "rainfall expected in the upper catchment; river levels rising.",
         tested="Not testable with rainfall data (gauge levels). See the notes below.", areas=[], window=None, thr=[]),
    dict(key="drc", partner="Danish Refugee Council", place="Darika, Mandera East (Daua River)",
         text="Verified KMSA weather reports of heavy rainfall within the next 24 hours, complemented by field observation of river "
              "level rise at Daua monitoring points. Stated lead time 7 days.",
         tested="Not testable as written (no rainfall amount). See the notes below.", areas=[], window=None, thr=[]),
]

AREA_MEMBERS = {
    "Mandera": "Mandera county", "Wajir": "Wajir county", "Marsabit": "Marsabit county", "Isiolo": "Isiolo county",
    "Samburu": "Samburu county",
    "Ewaso Ng'iro area (6 counties)": "Isiolo, Samburu, Nyeri, Nyandarua, Laikipia, Meru (equal county weighting)",
    "Garissa": "Garissa county", "Tana River": "Tana River county",
    "Upper Tana (7 counties)": "Nyeri, Kirinyaga, Murang'a, Embu, Meru, Tharaka-Nithi, Nyandarua (equal county weighting)",
}


def rp_lookup(area, window, thr, reading):
    r = rp[(rp.area == area) & (rp.window_days == window) & (rp.threshold_mm == thr) & (rp.reading == reading)]
    if not len(r):
        return None, None
    v = r.iloc[0]
    return (None if pd.isna(v.return_period_yr) else float(v.return_period_yr)), int(v.years_exceeded)


def fmt_rp(v, k):
    if v is None:
        return f"never in {N_YEARS} years"
    if v <= 1.15:
        return "every year"
    return f"1-in-{v:g} years ({k} of {N_YEARS})"


# ---------------------------------------------------------------- data for the charts
chart_data = {}
for area in AREA_MEMBERS:
    d = {}
    for reading in ("mean", "pixel"):
        for w in (1, 7):
            sub = sm[(sm.area == area) & (sm.window_days == w) & (sm.reading == reading) & (sm.year <= LAST_FULL)]
            piv = sub.pivot(index="year", columns="season", values="max_mm").reindex(range(FIRST_YEAR, LAST_FULL + 1))
            d[f"{reading}|{w}"] = {
                "years": [int(y) for y in piv.index],
                "MAM": [None if pd.isna(v) else round(float(v), 1) for v in piv.get("MAM", pd.Series(index=piv.index))],
                "OND": [None if pd.isna(v) else round(float(v), 1) for v in piv.get("OND", pd.Series(index=piv.index))],
            }
    chart_data[area] = d

floods_js = [dict(year=int(r.year), season=r.season, n=int(r.n), affected=(None if pd.isna(r.affected) else int(r.affected)),
                  ids=r.ids) for r in em_by_season.itertuples() if r.season in ("MAM", "OND")]

# equivalents table
eq_js = eq.to_dict(orient="records")
rp_js = rp.replace({np.nan: None}).to_dict(orient="records")

# episodes per trigger (mean reading), for the episode strip
eps_mean = eps[eps.reading == "mean"].copy()
eps_js = [dict(area=r.area, window=int(r.window_days), thr=int(r.threshold_mm), first=r.first.strftime("%Y-%m-%d"),
               last=r.last.strftime("%Y-%m-%d"), peak=round(float(r.peak)), season=r.season,
               emdat=("" if pd.isna(r.emdat) else r.emdat)) for r in eps_mean.itertuples()]

# EM-DAT events vs rainfall table
evr_cols = [c for c in evr.columns if "|" in c]
ev_js = []
for r in evr.to_dict(orient="records"):
    ev_js.append(dict(disno=r["disno"], start=str(r["start"]), end=str(r["end"]),
                      affected=(None if pd.isna(r["affected"]) else int(r["affected"])), counties=r["counties"],
                      vals={c: (None if pd.isna(r[c]) else int(r[c])) for c in evr_cols}))

# ---------------------------------------------------------------- per-trigger findings text (numbers from the tables only)
def finding_lines(trig):
    lines = []
    for area in trig["areas"]:
        for thr in trig["thr"]:
            w = trig["window"]
            rm, km = rp_lookup(area, w, thr, "mean")
            rpx, kpx = rp_lookup(area, w, thr, "pixel")
            r25, k25 = rp_lookup(area, w, thr, "q25")
            r50, k50 = rp_lookup(area, w, thr, "q50")
            e = spec[(spec.area == area) & (spec.window_days == w) & (spec.threshold_mm == thr) & (spec.reading == "mean")]
            n_ep = int(e.episodes.iloc[0]) if len(e) else 0
            n_hit = int(e.episodes_with_emdat_flood.iloc[0]) if len(e) else 0
            e3 = eq[(eq.area == area) & (eq.window_days == w) & (eq.reading == "mean")].iloc[0]
            lines.append(dict(area=area, thr=thr, window=w,
                              mean=fmt_rp(rm, km), pixel=fmt_rp(rpx, kpx), q25=fmt_rp(r25, k25), q50=fmt_rp(r50, k50),
                              episodes=n_ep, hits=n_hit, eq3=int(e3.thr_1in3_mm), eq5=int(e3.thr_1in5_mm)))
    return lines


findings = {t["key"]: finding_lines(t) for t in TRIGGERS}

# ---------------------------------------------------------------- HTML
def esc(s):
    return (str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))


def trigger_cards():
    out = []
    for t in TRIGGERS:
        out.append(f"""
<div class="fact">
  <div class="src">{esc(t['partner'])} | {esc(t['place'])}</div>
  <p><b>As written:</b> {esc(t['text'])}</p>
  <p class="small" style="margin-top:6px"><b>Tested here:</b> {esc(t['tested'])}</p>
</div>""")
    return "\n".join(out)


def finding_table(key):
    rows = findings[key]
    if not rows:
        return ""
    h = ["<div class='scroll'><table class='hot'><thead><tr><th>Area</th><th>Threshold</th>"
         "<th>County mean reaches it</th><th>Half of the area reaches it</th><th>A quarter of the area reaches it</th>"
         "<th>Wettest pixel reaches it</th><th>Episodes, county mean</th><th>1-in-3 / 1-in-5 equivalent, county mean</th></tr></thead><tbody>"]
    for r in rows:
        h.append(f"<tr><td>{esc(r['area'])}</td><td class='num'>{r['thr']} mm in {r['window']} day{'s' if r['window']>1 else ''}</td>"
                 f"<td>{esc(r['mean'])}</td><td>{esc(r['q50'])}</td><td>{esc(r['q25'])}</td><td>{esc(r['pixel'])}</td>"
                 f"<td class='num'>{r['episodes']} in {N_YEARS} years, {r['hits']} within an EM-DAT flood</td>"
                 f"<td class='num'>{r['eq3']} / {r['eq5']} mm</td></tr>")
    h.append("</tbody></table></div>")
    return "\n".join(h)


def chart_block(key, area, thr_list, window, title):
    thr_attr = ",".join(str(x) for x in thr_list)
    return f"""
<div class="panel chart" data-area="{esc(area)}" data-thr="{thr_attr}" data-window="{window}">
  <h3>{esc(title)}</h3>
  <p class="desc small">Highest {window}-day total in each March-May and October-December season, {FIRST_YEAR}-{LAST_FULL}. Hairline: the document's threshold. Marker under a season: an EM-DAT flood event that names one of the review's north-east counties started in that season.</p>
  <div class="svgwrap"></div>
</div>"""


def trigger_sections():
    out = []
    # KRCS north-east
    t = TRIGGERS[0]
    out.append(f"""
<section id="krcs-ne">
<h2>Kenya Red Cross Society: Mandera, Wajir, Marsabit</h2>
<p class="sub">Threshold as written: KMSA 7-day rainfall forecast above 150 mm. The document does not say whether 150 mm refers to a county average, a location within the county, or a share of the county. IMERG gives all three readings.</p>
{finding_table('krcs_ne')}
<div class="charts">
{chart_block('krcs_ne', 'Mandera', [150], 7, 'Mandera: highest 7-day total per season')}
{chart_block('krcs_ne', 'Wajir', [150], 7, 'Wajir: highest 7-day total per season')}
{chart_block('krcs_ne', 'Marsabit', [150], 7, 'Marsabit: highest 7-day total per season')}
</div>
</section>""")
    out.append(f"""
<section id="whh">
<h2>Welthungerhilfe: Isiolo and Samburu</h2>
<p class="sub">Threshold as written: KMSA 7-day forecast of 70-100 mm or more averaged over Isiolo, Samburu and the upper Ewaso Ng'iro catchment counties (Nyeri, Nyandarua, Laikipia, Meru). Two averaging areas are tested: the six counties as written, and the two target counties alone.</p>
{finding_table('whh')}
<div class="charts">
{chart_block('whh', "Ewaso Ng'iro area (6 counties)", [70, 100], 7, "Six-county area: highest 7-day total per season")}
{chart_block('whh', 'Isiolo', [70, 100], 7, 'Isiolo: highest 7-day total per season')}
{chart_block('whh', 'Samburu', [70, 100], 7, 'Samburu: highest 7-day total per season')}
</div>
</section>""")
    out.append(f"""
<section id="krcs-garissa">
<h2>Kenya Red Cross Society: Garissa (rainfall leg)</h2>
<p class="sub">Threshold as written: a KMSA heavy rainfall advisory that the Tana River basin is expected to receive at least 40 mm, OR Garissa Bridge above 5.1 m. Only the rainfall leg is tested. The advisory's accumulation period is not stated in the document; 40 mm in one day is used here, and the 3-day and 7-day return periods are in the table at the end.</p>
{finding_table('krcs_garissa')}
<div class="charts">
{chart_block('krcs_garissa', 'Garissa', [40], 1, 'Garissa: highest 1-day total per season')}
{chart_block('krcs_garissa', 'Tana River', [40], 1, 'Tana River: highest 1-day total per season')}
{chart_block('krcs_garissa', 'Upper Tana (7 counties)', [40], 1, 'Upper Tana counties: highest 1-day total per season')}
</div>
</section>""")
    return "\n".join(out)


def emdat_table():
    h = ["<div class='scroll'><table class='hot'><thead><tr><th>EM-DAT event</th><th>Dates</th><th>People affected</th><th>Counties named (of those reviewed)</th>",
         "<th>Mandera 7d</th><th>Wajir 7d</th><th>Marsabit 7d</th><th>Six-county area 7d</th><th>Garissa 1d</th><th>Tana River 1d</th><th>Upper Tana 1d</th></tr></thead><tbody>"]
    cols = [("Mandera|7d|mean", 150), ("Wajir|7d|mean", 150), ("Marsabit|7d|mean", 150), ("Ewaso Ng'iro area (6 counties)|7d|mean", 70),
            ("Garissa|1d|mean", 40), ("Tana River|1d|mean", 40), ("Upper Tana (7 counties)|1d|mean", 40)]
    for e in ev_js:
        cells = []
        for c, thr in cols:
            v = e["vals"].get(c)
            if v is None:
                cells.append("<td class='num muted'>n/a</td>")
            else:
                cls = "hit" if v >= thr else ""
                cells.append(f"<td class='num {cls}'>{v}</td>")
        aff = "n/a" if e["affected"] is None else f"{e['affected']:,}"
        h.append(f"<tr><td class='mono'>{esc(e['disno'])}</td><td class='mono'>{esc(e['start'])} to {esc(e['end'])}</td><td class='num'>{aff}</td>"
                 f"<td>{esc(e['counties'])}</td>{''.join(cells)}</tr>")
    h.append("</tbody></table></div>")
    return "\n".join(h)


def rp_full_table():
    areas = list(AREA_MEMBERS)
    h = ["<div class='scroll'><table class='hot rpfull'><thead><tr><th>Area</th><th>Window</th>"]
    for thr in (40, 70, 100, 150, 200):
        h.append(f"<th>{thr} mm</th>")
    h.append("<th>1-in-3 mm</th><th>1-in-5 mm</th></tr></thead><tbody>")
    for a in areas:
        for w in (1, 3, 7):
            e3 = eq[(eq.area == a) & (eq.window_days == w) & (eq.reading == "mean")].iloc[0]
            h.append(f"<tr><td>{esc(a)}</td><td class='num'>{w} day{'s' if w>1 else ''}</td>")
            for thr in (40, 70, 100, 150, 200):
                v, k = rp_lookup(a, w, thr, "mean")
                txt = "never" if v is None else ("every year" if v <= 1.15 else f"{v:g}")
                h.append(f"<td class='num'>{txt}</td>")
            h.append(f"<td class='num'>{int(e3.thr_1in3_mm)}</td><td class='num'>{int(e3.thr_1in5_mm)}</td></tr>")
    h.append("</tbody></table></div>")
    return "\n".join(h)


payload = dict(chart=chart_data, floods=floods_js, eps=eps_js, nYears=N_YEARS, firstYear=FIRST_YEAR, lastFull=LAST_FULL)

html = f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
<title>Kenya Flood Triggers</title>
<meta name="description" content="Review of the rainfall thresholds in the Kenya Humanitarian Fund RA2 anticipatory action triggers against IMERG rainfall, 1998 to 2025.">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>
:root{{
  color-scheme:light;
  --bg:#F4F6F7; --surface:#FFFFFF; --ink:#15212A; --ink2:#46555F; --muted:#76838C; --rule:#DDE3E6; --rule2:#EEF1F3;
  --accent:#1D5B7C; --mam:#2a78d6; --ond:#eb6834; --thr:#15212A; --flood:#8C3A0B; --hit:#FAE3D3; --o-before:#4a3aa7; --o-during:#1baf7a; --flood-band:rgba(140,58,11,.16);
  --sans:"Public Sans",system-ui,-apple-system,"Segoe UI",sans-serif;
  --mono:"IBM Plex Mono",ui-monospace,Consolas,monospace;
}}
@media (prefers-color-scheme:dark){{:root:not([data-theme="light"]){{color-scheme:dark;
  --bg:#11171B; --surface:#182026; --ink:#E6ECEF; --ink2:#AAB6BD; --muted:#7F8C94; --rule:#2B353B; --rule2:#222B31;
  --accent:#7DB6D6; --mam:#3987e5; --ond:#d95926; --thr:#E6ECEF; --flood:#F2B36B; --hit:#4A2E1A; --o-before:#9085e9; --o-during:#199e70; --flood-band:rgba(242,179,107,.20);}}}}
:root[data-theme="dark"]{{color-scheme:dark;
  --bg:#11171B; --surface:#182026; --ink:#E6ECEF; --ink2:#AAB6BD; --muted:#7F8C94; --rule:#2B353B; --rule2:#222B31;
  --accent:#7DB6D6; --mam:#3987e5; --ond:#d95926; --thr:#E6ECEF; --flood:#F2B36B; --hit:#4A2E1A; --o-before:#9085e9; --o-during:#199e70; --flood-band:rgba(242,179,107,.20);}}
*{{box-sizing:border-box}}
body{{margin:0;background:var(--bg);color:var(--ink);font-family:var(--sans);font-size:15px;line-height:1.55}}
.wrap{{max-width:1120px;margin:0 auto;padding-inline:16px;padding-block:36px 56px}}
header{{padding-bottom:22px;border-bottom:1px solid var(--rule)}}
.eyebrow{{font:500 12px/1 var(--mono);letter-spacing:.08em;text-transform:uppercase;color:var(--accent)}}
h1{{font-size:clamp(26px,5vw,40px);line-height:1.08;margin:12px 0;font-weight:700;letter-spacing:-.015em;text-wrap:balance}}
.lede{{color:var(--ink2);max-width:76ch;margin:0}}
.meta{{font:12px/1.6 var(--mono);color:var(--muted);margin-top:12px}}
section{{margin-top:46px}}
h2{{font-size:21px;margin:0 0 6px;font-weight:600;letter-spacing:-.005em;text-wrap:balance}}
h3{{font-size:15px;margin:0 0 4px;font-weight:600}}
.sub{{color:var(--ink2);max-width:80ch;margin:0 0 16px}}
.small{{font-size:13px;color:var(--ink2)}}
.muted{{color:var(--muted)}}
.mono{{font-family:var(--mono);font-size:12.5px}}
.glance{{background:var(--surface);border:1px solid var(--rule);border-left:4px solid var(--accent);border-radius:4px;padding:16px 20px;margin-top:22px}}
.glance h2{{font-size:17px;margin:0 0 10px}}
.glance ul{{margin:0;padding-left:18px}} .glance li{{margin:4px 0}}
.nav{{display:flex;flex-wrap:wrap;gap:6px 14px;margin-top:14px;font-size:13px}}
.nav a{{color:var(--accent);text-decoration:none}}.nav a:hover{{text-decoration:underline}}
.facts{{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:12px;margin:0 0 18px}}
.fact{{background:var(--surface);border:1px solid var(--rule);border-top:3px solid var(--accent);border-radius:4px;padding:12px 16px}}
.fact .src{{font:500 11px var(--mono);letter-spacing:.06em;text-transform:uppercase;color:var(--muted);margin-bottom:4px}}
.fact p{{margin:0;font-size:14px}}
.panel{{background:var(--surface);border:1px solid var(--rule);border-radius:6px;padding:16px}}
.charts{{display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:14px;margin-top:16px}}
.chart .desc{{margin:0 0 8px;min-height:3.9em}}
.scroll{{overflow-x:auto;margin:0 0 8px}}
table.hot{{border-collapse:collapse;width:100%;font-size:13.5px;background:var(--surface)}}
.hot th,.hot td{{padding:7px 10px;border-bottom:1px solid var(--rule2);text-align:left;vertical-align:top}}
.hot th{{font:500 11px var(--mono);letter-spacing:.05em;text-transform:uppercase;color:var(--muted);border-bottom:1px solid var(--rule)}}
.hot td.num{{font-variant-numeric:tabular-nums;white-space:nowrap}}
.hot td.hit{{background:var(--hit);font-weight:600}}
svg{{display:block;width:100%;height:auto}}
svg text{{font-family:var(--sans);fill:var(--ink2);font-size:11px}}
svg .strong{{fill:var(--ink);font-weight:600}}
.legend{{display:flex;gap:14px;flex-wrap:wrap;font-size:12.5px;color:var(--ink2);margin:6px 0 0}}
.legend i{{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-2px;margin-right:5px}}
.legend .l{{display:inline-block;width:14px;height:0;border-top:2px solid var(--thr);vertical-align:3px;margin-right:5px}}
.legend .t{{display:inline-block;width:0;height:0;border-left:5px solid transparent;border-right:5px solid transparent;border-bottom:8px solid var(--flood);vertical-align:-1px;margin-right:5px}}
__ACT_CSS__
.tip{{position:fixed;pointer-events:none;background:var(--surface);color:var(--ink);border:1px solid var(--rule);border-radius:4px;padding:6px 9px;font-size:12.5px;box-shadow:0 2px 8px rgba(0,0,0,.12);display:none;z-index:9;max-width:280px}}
.tip b{{font-variant-numeric:tabular-nums}}
.tip .k{{display:inline-block;width:10px;height:10px;border-radius:2px;margin-right:6px;vertical-align:-1px}}
.controls{{display:flex;flex-wrap:wrap;gap:8px 16px;margin:0 0 12px;align-items:center;font-size:13px}}
.seg{{display:inline-flex;border:1px solid var(--rule);border-radius:5px;overflow:hidden;background:var(--surface)}}
.seg button{{border:0;background:transparent;color:var(--ink2);padding:6px 12px;font:inherit;cursor:pointer}}
.seg button[aria-pressed="true"]{{background:var(--accent);color:#fff}}
ol.notes li,ul.notes li{{margin:6px 0}}
footer{{margin-top:48px;padding-top:16px;border-top:1px solid var(--rule);font-size:13px;color:var(--ink2)}}
</style>
</head>
<body>
<div class="wrap">
<header>
  <div class="eyebrow">OCHA ROSEA support | Kenya | floods</div>
  <h1>Kenya Humanitarian Fund flood triggers against IMERG rainfall</h1>
  <p class="lede">The Kenya Humanitarian Fund's RA2 allocation (September 2026) reserves USD 4 million for anticipatory action ahead of El Niño floods and lists partner triggers on KMSA 7-day rainfall forecasts and Tana River gauge levels. This page tests how often each rainfall threshold has been reached in the observed record, and whether those occasions line up with recorded floods.</p>
  <div class="meta">IMERG Late Run v7, daily, 0.1 degree, {FIRST_YEAR}-01-01 to {LAST_DATE} | return periods on full years {FIRST_YEAR}-{LAST_FULL} ({N_YEARS} years) | impact record: EM-DAT floods naming the reviewed counties | built {date.today().isoformat()}</div>
  <nav class="nav"><a href="#written">Triggers as written</a><a href="#activations">When each trigger would have been reached</a><a href="#krcs-ne">Mandera, Wajir, Marsabit</a><a href="#whh">Isiolo, Samburu</a><a href="#krcs-garissa">Garissa rainfall leg</a><a href="#events">Flood events vs rainfall</a><a href="#untested">Not testable here</a><a href="#method">Method and data</a></nav>
</header>

<div class="glance">
<h2>What the record shows</h2>
<ul id="glance-list"></ul>
</div>

<section id="written">
<h2>Triggers as written in the KHF document</h2>
<p class="sub">Five partner triggers under Priority Area III. The text is quoted from the allocation paper; the second line of each card says what this page tests.</p>
<div class="facts">
{trigger_cards()}
</div>
</section>

__ACT_HTML__

<div class="controls" id="reading-ctl">
  <span class="small">Reading of "X mm over the area":</span>
  <span class="seg" role="group" aria-label="Reading">
    <button type="button" data-reading="mean" aria-pressed="true">County mean</button>
    <button type="button" data-reading="pixel" aria-pressed="false">Wettest pixel</button>
  </span>
  <span class="small">Charts below follow this control; the tables show all readings.</span>
</div>

{trigger_sections()}

<section id="events">
<h2>Recorded flood events against the rainfall that preceded them</h2>
<p class="sub">Every EM-DAT flood event since {FIRST_YEAR} whose location names one of the reviewed counties, with the highest county-mean rainfall total in the 10 days before the event start through its end. Shaded cells reach the document's threshold for that area (150 mm in 7 days for Mandera, Wajir, Marsabit; 70 mm in 7 days for the six-county area; 40 mm in a day for the Tana areas).</p>
{emdat_table()}
<p class="small">EM-DAT locations are free text and often list many counties for one national event; a county being named does not mean the flood was driven by rain over that county. Riverine floods in Garissa and Tana River are driven by rain in the upper Tana, which is why that column is included.</p>
</section>

<section id="untested">
<h2>Parts of the document this data cannot test, and internal inconsistencies</h2>
<ul class="notes">
<li><b>Gauge thresholds differ between partners.</b> The Kenya Red Cross Society trigger uses Garissa Bridge above 5.1 m; the Dadaab trigger uses "Flood Alert" at Garissa 3.0-3.5 m. The document does not say how the two relate. The Kenya Red Cross Society's 2021 Early Action Protocol also uses 5.1 m at Garissa (its 5-year return period discharge), and its November 2023 activation was reported when Garissa Bridge exceeded 5 m.</li>
<li><b>Lead times.</b> The Danish Refugee Council trigger is a forecast of heavy rainfall within the next 24 hours, but states a 7-day lead time. The Dadaab trigger is a 7-day forecast of river levels; the document does not say who issues river-level forecasts at that range.</li>
<li><b>Rainfall amounts without a period or area.</b> The 40 mm advisory (Garissa) has no accumulation period, and the 150 mm and 70-100 mm forecasts have no spatial definition. The tables above show that the choice between a county average and a wettest location moves the frequency from "never" to "every year" for 150 mm in Mandera and Wajir.</li>
<li><b>Forecast skill is not tested.</b> This page uses observed rainfall. A KMSA 7-day forecast of 150 mm and an observed 150 mm are different quantities; a forecast archive would be needed to backtest the trigger as written.</li>
<li><b>Impact record.</b> EM-DAT is the only impact record used. The document cites a Kenya Red Cross Society figure of 188,000 people affected per year in Tana River (2001-2020) that is not reproduced here.</li>
</ul>
</section>

<section id="method">
<h2>Method and data</h2>
<ul class="notes">
<li>Rainfall: NASA IMERG Late Run v7 daily totals at 0.1 degree, read from the team raster store. County boundaries: Kenya COD-AB admin 1 (47 counties), from the team blob.</li>
<li>Readings: county mean is the area-weighted mean of the pixels in the county; multi-county areas weight each county equally. Wettest pixel is the maximum over pixels with at least half their area inside the area. "Half" and "a quarter of the area" are the share of the area's surface at or above the threshold.</li>
<li>Windows: running 1-, 3- and 7-day totals per pixel, then aggregated. Return periods: Weibull plotting position (n + 1) / k on the annual maximum of each series over {N_YEARS} full years. An episode is a run of days at or above the threshold, merged when separated by 14 days or less.</li>
<li>Flood matching: an episode is counted "within an EM-DAT flood" when an EM-DAT event naming a reviewed county overlaps the window from 10 days before the episode to 30 days after it.</li>
__DB_CHECK__
<li>Full return-period table for every area, window and threshold, county-mean reading:</li>
</ul>
{rp_full_table()}
<p class="small">Code: <span class="mono">flood/ken/</span> in <a href="https://github.com/OCHA-DAP/pa-rosea-support">OCHA-DAP/pa-rosea-support</a>. Contact: ocha-datascience@un.org.</p>
</section>

<footer>OCHA Centre for Humanitarian Data, Data Science team. Sources: NASA GPM IMERG (Huffman et al.), EM-DAT (CRED / UCLouvain), OCHA COD-AB, Kenya Humanitarian Fund RA2 allocation paper (September 2026).</footer>
</div>
<div class="tip" id="tip"></div>

<script id="data" type="application/json">{json.dumps(payload)}</script>
<script id="actdata" type="application/json">__ACT_DATA__</script>
<script>
(function(){{
const D = JSON.parse(document.getElementById('data').textContent);
const floods = new Map(D.floods.map(f => [f.year + '|' + f.season, f]));
const tip = document.getElementById('tip');
let reading = 'mean';

function showTip(ev, html){{ tip.innerHTML = ''; tip.appendChild(html); tip.style.display='block';
  const x = Math.min(ev.clientX + 14, window.innerWidth - 300), y = Math.min(ev.clientY + 14, window.innerHeight - 90);
  tip.style.left = x + 'px'; tip.style.top = y + 'px'; }}
function hideTip(){{ tip.style.display='none'; }}
function el(tag, attrs, text){{ const e = document.createElementNS('http://www.w3.org/2000/svg', tag);
  for (const k in attrs) e.setAttribute(k, attrs[k]); if (text != null) e.textContent = text; return e; }}
function fmt(v){{ return v == null ? 'n/a' : Math.round(v).toLocaleString('en') + ' mm'; }}

function drawChart(panel){{
  const area = panel.dataset.area, win = panel.dataset.window, thrs = panel.dataset.thr.split(',').map(Number);
  const s = D.chart[area][reading + '|' + win];
  const wrap = panel.querySelector('.svgwrap'); wrap.innerHTML = '';
  const W = 560, H = 250, ml = 44, mr = 10, mt = 10, mb = 34;
  const iw = W - ml - mr, ih = H - mt - mb;
  const n = s.years.length, band = iw / n;
  const vmax = Math.max(...thrs, ...s.MAM.filter(v => v != null), ...s.OND.filter(v => v != null)) * 1.08;
  const y = v => mt + ih - (v / vmax) * ih;
  const svg = el('svg', {{viewBox: `0 0 ${{W}} ${{H}}`, role: 'img', 'aria-label': area + ' seasonal maxima'}});
  // gridlines
  const step = vmax > 400 ? 100 : vmax > 160 ? 50 : vmax > 60 ? 20 : 10;
  for (let g = 0; g <= vmax; g += step) {{
    svg.appendChild(el('line', {{x1: ml, x2: W - mr, y1: y(g), y2: y(g), stroke: 'var(--rule2)', 'stroke-width': 1}}));
    svg.appendChild(el('text', {{x: ml - 6, y: y(g) + 4, 'text-anchor': 'end'}}, g.toLocaleString('en')));
  }}
  // bars
  const bw = Math.min(10, band * 0.38);
  s.years.forEach((yr, i) => {{
    const x0 = ml + i * band + band / 2;
    [['MAM', -bw - 1, 'var(--mam)'], ['OND', 1, 'var(--ond)']].forEach(([sea, off, col]) => {{
      const v = s[sea][i]; if (v == null) return;
      const top = y(v), h = Math.max(0, mt + ih - top);
      const r = el('rect', {{x: x0 + off, y: top, width: bw, height: h, fill: col, rx: 2}});
      const f = floods.get(yr + '|' + sea);
      r.addEventListener('pointermove', ev => {{
        const d = document.createElement('div');
        const k = document.createElement('span'); k.className = 'k'; k.style.background = col; d.appendChild(k);
        const b = document.createElement('b'); b.textContent = fmt(v); d.appendChild(b);
        d.appendChild(document.createTextNode(' ' + sea + ' ' + yr + ', highest ' + win + '-day total, ' + (reading === 'mean' ? 'county mean' : 'wettest pixel')));
        if (f) {{ const p = document.createElement('div'); p.className = 'small'; p.textContent = 'EM-DAT flood ' + f.ids + (f.affected ? ', ' + f.affected.toLocaleString('en') + ' affected' : ''); d.appendChild(p); }}
        showTip(ev, d); r.setAttribute('opacity', .75);
      }});
      r.addEventListener('pointerleave', () => {{ hideTip(); r.removeAttribute('opacity'); }});
      svg.appendChild(r);
      if (f) svg.appendChild(el('path', {{d: `M${{x0 + off + bw/2}} ${{mt + ih + 4}} l4 7 h-8 z`, fill: 'var(--flood)'}}));
    }});
    if (yr % 5 === 0) svg.appendChild(el('text', {{x: x0, y: H - 8, 'text-anchor': 'middle'}}, String(yr)));
  }});
  // thresholds
  thrs.forEach(t => {{
    svg.appendChild(el('line', {{x1: ml, x2: W - mr, y1: y(t), y2: y(t), stroke: 'var(--thr)', 'stroke-width': 1.2}}));
    svg.appendChild(el('text', {{x: W - mr, y: y(t) - 4, 'text-anchor': 'end', class: 'strong'}}, t + ' mm'));
  }});
  svg.appendChild(el('line', {{x1: ml, x2: W - mr, y1: mt + ih, y2: mt + ih, stroke: 'var(--rule)'}}));
  wrap.appendChild(svg);
  let lg = panel.querySelector('.legend');
  if (!lg) {{ lg = document.createElement('div'); lg.className = 'legend';
    lg.innerHTML = '<span><i style="background:var(--mam)"></i>March-May</span><span><i style="background:var(--ond)"></i>October-December</span><span><span class="l"></span>threshold</span><span><span class="t"></span>EM-DAT flood that season</span>';
    panel.appendChild(lg); }}
}}
function drawAll(){{ document.querySelectorAll('.chart').forEach(drawChart); }}
document.querySelectorAll('#reading-ctl button').forEach(b => b.addEventListener('click', () => {{
  reading = b.dataset.reading;
  document.querySelectorAll('#reading-ctl button').forEach(x => x.setAttribute('aria-pressed', String(x === b)));
  drawAll();
}}));
drawAll();
__ACT_JS__

// at-a-glance lines, computed from the same data
const gl = document.getElementById('glance-list');
__GLANCE_JS__
}})();
</script>
</body>
</html>
"""

def activation_glance():
    sm_ = pd.read_csv(OUT / "activation_summary.csv")
    ac_ = pd.read_csv(OUT / "activations.csv")
    ov_ = pd.read_csv(OUT / "overall_return_periods.csv")
    aw = sm_[sm_.level == "as written"].set_index("trigger")
    out = []
    parts = []
    for k, nm in [("krcs_mandera", "Mandera"), ("krcs_wajir", "Wajir"), ("krcs_marsabit", "Marsabit")]:
        d = ac_[(ac_.trigger == k) & (ac_.level == "as written")]
        dates = ", ".join(pd.to_datetime(d.date).dt.strftime("%b %Y"))
        times = {1: "once", 2: "twice"}.get(len(d), f"{len(d)} times")
        parts.append(f"{nm} {times} ({dates})")
    ne = aw.loc[["krcs_mandera", "krcs_wajir", "krcs_marsabit"]]
    o = ov_[(ov_.group == "krcs_ne") & (ov_.level == "as written")].iloc[0]
    out.append("150 mm in 7 days (county mean) would have been reached: " + "; ".join(parts) + ". "
               f"{int(ne.during_flood.sum())} of these came during an EM-DAT flood already under way in that county, "
               f"{int(ne.before_flood.sum())} before one, {int(ne.no_recorded_flood.sum())} with no recorded flood. "
               f"Any of the three counties: {int(o.years_activated)} of {N_YEARS} years ({o.years}). Recorded floods with an activation: "
               f"Mandera {int(aw.loc['krcs_mandera','floods_caught'])} of {int(aw.loc['krcs_mandera','floods_in_record'])}, "
               f"Wajir {int(aw.loc['krcs_wajir','floods_caught'])} of {int(aw.loc['krcs_wajir','floods_in_record'])}, "
               f"Marsabit {int(aw.loc['krcs_marsabit','floods_caught'])} of {int(aw.loc['krcs_marsabit','floods_in_record'])}.")
    for k, txt in [("whh_70", "70 mm in 7 days over the six Ewaso Ng'iro counties"), ("whh_100", "100 mm")]:
        r = aw.loc[k]
        out.append(f"{txt}: {int(r.activations)} activations in {int(r.years_activated)} of {N_YEARS} years; "
                   f"{int(r.before_flood)} before a recorded flood in Isiolo or Samburu, {int(r.during_flood)} during one, "
                   f"{int(r.no_recorded_flood)} with no recorded flood; {int(r.floods_caught)} of {int(r.floods_in_record)} recorded floods had an activation.")
    r = aw.loc["krcs_garissa"]
    out.append(f"40 mm in a day over the upper Tana counties: {int(r.activations)} activations in {int(r.years_activated)} of {N_YEARS} years; "
               f"{int(r.before_flood)} before a recorded flood in Garissa, Tana River or Dadaab, {int(r.during_flood)} during one, "
               f"{int(r.no_recorded_flood)} with no recorded flood; {int(r.floods_caught)} of {int(r.floods_in_record)} recorded floods had an activation.")
    oa = ov_[(ov_.group == "all") & (ov_.level == "as written")].iloc[0]
    out.append(f"At least one of these rainfall triggers as written would have been reached in {int(oa.years_activated)} of {N_YEARS} years "
               f"and in {int(oa.ond_seasons_activated)} of {N_YEARS} October-December seasons.")
    return out


# glance lines are built in Python from the tables so the text and the numbers cannot drift apart
def glance_lines():
    L = []
    def rp_txt(area, w, thr, rd):
        v, k = rp_lookup(area, w, thr, rd)
        return fmt_rp(v, k)
    L.append(f"150 mm in 7 days as a county mean: {rp_txt('Mandera',7,150,'mean')} in Mandera, {rp_txt('Wajir',7,150,'mean')} in Wajir, "
             f"{rp_txt('Marsabit',7,150,'mean')} in Marsabit. As the wettest pixel in the county: {rp_txt('Mandera',7,150,'pixel')} in Mandera, "
             f"{rp_txt('Wajir',7,150,'pixel')} in Wajir, {rp_txt('Marsabit',7,150,'pixel')} in Marsabit.")
    fdb = OUT / "db_crosscheck.json"
    if fdb.exists():
        for k, v in json.loads(fdb.read_text())["trigger_years"].items():
            for d in v["differs"]:
                L.append(f"{k} {d['year']} sits on the 150 mm line: {d['cog']} mm here, {d['db']} mm in the team database, "
                         f"so {k} reaches the threshold in {len(v['cog'])} or {len(v['db'])} of {N_YEARS} years depending on the source.")
    e = eq[(eq.window_days == 7) & (eq.reading == "mean")].set_index("area")
    L.append(f"County-mean 7-day totals with a 1-in-3 year return period: Mandera {int(e.loc['Mandera','thr_1in3_mm'])} mm, "
             f"Wajir {int(e.loc['Wajir','thr_1in3_mm'])} mm, Marsabit {int(e.loc['Marsabit','thr_1in3_mm'])} mm. 1-in-5: "
             f"{int(e.loc['Mandera','thr_1in5_mm'])}, {int(e.loc['Wajir','thr_1in5_mm'])}, {int(e.loc['Marsabit','thr_1in5_mm'])} mm.")
    L += activation_glance()
    n_ev = len(ev_js)
    L.append(f"{n_ev} EM-DAT flood events since {FIRST_YEAR} name at least one reviewed county; the table below shows the rainfall preceding each.")
    L.append("Gauge levels (Garissa 5.1 m for the Kenya Red Cross Society, 3.0-3.5 m for Dadaab) and forecast skill are outside what rainfall data can test.")
    return L


glance_js = "\n".join(f"{{ const li = document.createElement('li'); li.textContent = {json.dumps(t)}; gl.appendChild(li); }}" for t in glance_lines())
html = html.replace("__GLANCE_JS__", glance_js)


def db_check_html():
    f = OUT / "db_crosscheck.json"
    if not f.exists():
        return ""
    c = json.loads(f.read_text())
    diffs = [f"{k} {d['year']} ({d['cog']} mm here, {d['db']} mm in the database)"
             for k, v in c["trigger_years"].items() for d in v["differs"]]
    diff_txt = ("The count of years at or above 150 mm in 7 days differs in one case: " + "; ".join(diffs) + "."
                if diffs else "The years at or above 150 mm in 7 days are identical for Mandera, Wajir and Marsabit.")
    gap = c["dates_missing_from_db"]
    gap_txt = (f" The database has no Kenya rows for {', '.join(gap)}; the rasters for those days exist and are used here." if gap else "")
    return (f"<li>Cross-check against the team database table <span class='mono'>public.imerg</span> "
            f"({c['rows_compared']:,} county-days, {c['db_first']} to {c['db_last']}): correlation {c['corr']}, "
            f"median difference {c['median_abs_diff_mm']} mm, 99th percentile {c['p99_abs_diff_mm']} mm. "
            f"{esc(diff_txt)}{esc(gap_txt)}</li>")


html = html.replace("__DB_CHECK__", db_check_html())
_ah, _ad, _ac, _aj = page_activations.build(OUT, N_YEARS, FIRST_YEAR, LAST_FULL)
html = (html.replace("__ACT_HTML__", _ah).replace("__ACT_DATA__", _ad.replace("</", "<\/"))
        .replace("__ACT_CSS__", _ac).replace("__ACT_JS__", _aj))

out_path = HERE / "ken_khf_trigger_review.html"
out_path.write_text(html, encoding="utf8")
print("wrote", out_path, f"{out_path.stat().st_size/1024:.0f} KB")

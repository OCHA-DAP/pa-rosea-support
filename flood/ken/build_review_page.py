"""Build flood/ken/ken_khf_trigger_review.html: how often each KHF RA2 rainfall trigger would
have been reached since 1998, against recorded floods. Single self-contained page, written
for a non-technical reader.

Reads data_dir/out/ from backtest.py and (optional) crosscheck_db.py.
Run: python flood/ken/build_review_page.py [data_dir]
"""
import json
import sys
from datetime import date
from pathlib import Path

import pandas as pd

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
OUT = DATA / "out"
HERE = Path(__file__).parent

summ = pd.read_csv(OUT / "summary.csv").set_index("trigger")
acts = pd.read_csv(OUT / "activations.csv", parse_dates=["date"])
ov = pd.read_csv(OUT / "overall.csv").set_index("group")
fl = pd.read_csv(OUT / "floods.csv")
wet = pd.read_csv(OUT / "wettest_spot.csv")
meta = json.loads((OUT / "meta.json").read_text())
dbc = json.loads((OUT / "db_crosscheck.json").read_text()) if (OUT / "db_crosscheck.json").exists() else None

FIRST_YEAR, LAST_FULL, N = meta["first_year"], meta["last_full_year"], meta["n_years"]
LAST_DATE = meta["last_date"]
EM_LAST_YEAR = int(meta["emdat_last"][:4])

# one row per trigger as written: (key, area shown, partner, threshold text, lane label)
TRIGGERS = [
    ("krcs_mandera", "Mandera", "Kenya Red Cross Society", "Mandera"),
    ("krcs_wajir", "Wajir", "Kenya Red Cross Society", "Wajir"),
    ("krcs_marsabit", "Marsabit", "Kenya Red Cross Society", "Marsabit"),
    ("whh_70", "Isiolo and Samburu", "Welthungerhilfe", "70 mm"),
    ("whh_100", "Isiolo and Samburu", "Welthungerhilfe", "100 mm"),
    ("krcs_garissa", "Garissa (rainfall part)", "Kenya Red Cross Society", "40 mm"),
]
SECTIONS = [
    dict(id="north-east", title="Mandera, Wajir and Marsabit", partner="Kenya Red Cross Society",
         trigger="KMSA 7-day rainfall forecast above 150 mm.",
         shown="Rainfall over 7 days, averaged over each county.",
         keys=["krcs_mandera", "krcs_wajir", "krcs_marsabit"]),
    dict(id="isiolo-samburu", title="Isiolo and Samburu", partner="Welthungerhilfe",
         trigger="KMSA 7-day forecast of 70 to 100 mm or more over Isiolo, Samburu and the upper Ewaso Ng'iro catchment.",
         shown="Rainfall over 7 days, averaged over Isiolo, Samburu, Nyeri, Nyandarua, Laikipia and Meru. Both ends of the range are shown.",
         keys=["whh_70", "whh_100"]),
    dict(id="garissa", title="Garissa", partner="Kenya Red Cross Society",
         trigger="Garissa Bridge river level above 5.1 m, or a KMSA heavy rainfall advisory of at least 40 mm for the Tana basin.",
         shown="The rainfall part only: rainfall in one day, averaged over the upper Tana counties upstream of Garissa (Nyeri, Kirinyaga, Murang'a, Embu, Meru, Tharaka-Nithi, Nyandarua).",
         keys=["krcs_garissa"]),
]
LANE = {k: lab for k, _, _, lab in TRIGGERS}


def esc(s):
    return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def srow(k):
    return summ.loc[k]


def rp_txt(v):
    if v is None or pd.isna(v):
        return "never"
    return "every year" if v <= 1.15 else f"1 in {v:g} years"


def thr_txt(r):
    return f"{int(r.threshold_mm)} mm in {int(r.window_days)} day{'s' if r.window_days > 1 else ''}"


# ------------------------------------------------------------------ chart data
lanes = {s["id"]: [dict(key=k, label=LANE[k],
                        acts=[dict(d=a.date.strftime("%Y-%m-%d"), p=int(a.peak_mm))
                              for a in acts[acts.trigger == k].itertuples()])
                   for k in s["keys"]] for s in SECTIONS}
floods = {k: [dict(id=r.disno, s=r.start, e=r.end, a=(None if pd.isna(r.affected) else int(r.affected)))
              for r in g.itertuples()] for k, g in fl.groupby("trigger")}
payload = dict(lanes=lanes, floods=floods, first=f"{FIRST_YEAR}-01-01", last=f"{LAST_DATE[:4]}-12-31",
               emLast=meta["emdat_last"])

# ------------------------------------------------------------------ numbers used in the text (all from the tables)
ne, alltr = ov.loc["krcs_ne"], ov.loc["all"]
R = {k: srow(k) for k, *_ in TRIGGERS}

FINDINGS = [
    ("Mandera, Wajir and Marsabit (150 mm)",
     f"Reached in {int(ne.years_reached)} of the last {N} years in at least one of the three counties. "
     f"It was reached for {int(R['krcs_wajir'].floods_reached)} of {int(R['krcs_wajir'].floods)} recorded floods in Wajir, "
     f"{int(R['krcs_mandera'].floods_reached)} of {int(R['krcs_mandera'].floods)} in Mandera and "
     f"{int(R['krcs_marsabit'].floods_reached)} of {int(R['krcs_marsabit'].floods)} in Marsabit."),
    ("Isiolo and Samburu (70 mm)",
     f"Reached in {int(R['whh_70'].years_reached)} of {N} years, and for {int(R['whh_70'].floods_reached)} of "
     f"{int(R['whh_70'].floods)} recorded floods. At 100 mm: {int(R['whh_100'].years_reached)} of {N} years."),
    ("Garissa (40 mm rainfall part)",
     f"Reached in {int(R['krcs_garissa'].years_reached)} of {N} years, and for {int(R['krcs_garissa'].floods_reached)} of "
     f"{int(R['krcs_garissa'].floods)} recorded floods in Garissa, Tana River or Dadaab."),
    ("All triggers together",
     f"At least one would have been reached in {int(alltr.years_reached)} of {N} years."),
]


def findings_html():
    return "<ul class='findings'>" + "".join(f"<li><b>{esc(h)}:</b> {esc(t)}</li>" for h, t in FINDINGS) + "</ul>"


def summary_table():
    h = ["<div class='scroll'><table><thead><tr><th>Trigger</th><th>Threshold</th>"
         f"<th class='n'>Years reached<br>({FIRST_YEAR}-{LAST_FULL})</th><th class='n'>How often</th>"
         "<th class='n'>Recorded floods<br>it was reached for</th></tr></thead><tbody>"]
    for k, area, partner, _ in TRIGGERS:
        r = R[k]
        h.append(f"<tr><td>{esc(area)}<div class='sm'>{esc(partner)}</div></td><td>{thr_txt(r)}</td>"
                 f"<td class='n'>{int(r.years_reached)} of {N}</td><td class='n'>{rp_txt(r.return_period)}</td>"
                 f"<td class='n'>{int(r.floods_reached)} of {int(r.floods)}</td></tr>")
    h.append("</tbody></table></div>")
    return "".join(h)


def dates_list(s):
    parts = []
    for k in s["keys"]:
        a = acts[acts.trigger == k].sort_values("date")
        items = "".join(f"<li><span class='mono'>{r.date.strftime('%d %b %Y')}</span> | {int(r.peak_mm)} mm</li>" for r in a.itertuples())
        head = f"<h4>{esc(LANE[k])}</h4>" if len(s["keys"]) > 1 else ""
        parts.append(f"{head}<ul class='dates'>{items}</ul>")
    return f"<details><summary>List of dates</summary><p class='sm'>Date the threshold was first reached | highest total on that occasion</p>{''.join(parts)}</details>"


LEGEND = ("<div class='legend'>"
          "<span><svg width='14' height='14'><circle cx='7' cy='7' r='4.5' fill='var(--dot)'/></svg>threshold reached</span>"
          "<span><i style='background:var(--band)'></i>recorded flood</span>"
          f"<span><i style='background:var(--rule2)'></i>no flood records after {EM_LAST_YEAR}</span></div>")


def section_html(s):
    return f"""
<section id="{s['id']}">
  <h2>{esc(s['title'])}</h2>
  <p class="sub"><b>Trigger ({esc(s['partner'])}):</b> {esc(s['trigger'])}<br><b>Shown here:</b> {esc(s['shown'])}</p>
  <div class="panel timeline" data-group="{s['id']}"><div class="svgwrap"></div>{LEGEND}</div>
  {dates_list(s)}
</section>"""


NOT_COVERED = [
    "Whether KMSA forecasts would have predicted these totals. This page uses observed rainfall.",
    "The river-level triggers: Garissa Bridge above 5.1 m (Kenya Red Cross Society), Flood Alert levels at Garissa, Hola and Garsen (Dadaab partners), and the Danish Refugee Council trigger for Darika, which has no rainfall amount.",
    f"Where in a county the rainfall must fall. The triggers do not say, so this page uses the county average. Measured at the wettest spot in each county instead, 150 mm in 7 days is reached in {int(wet.years_reached.min())} or more of {N} years.",
]

METHOD = [
    f"Rainfall: NASA IMERG Late Run version 7, daily, about 11 km grid, {FIRST_YEAR}-01-01 to {LAST_DATE}, averaged over Kenya county boundaries (COD-AB).",
    "A threshold counts as reached on the first day the running total gets to it. Further days in the next 30 days count as the same occasion.",
    f"Floods: events in EM-DAT, the international disaster database, whose location names the trigger's counties, {FIRST_YEAR} to {EM_LAST_YEAR}. "
    "EM-DAT only includes events that meet its criteria (for example 100 or more people affected), often records one long event for a whole season, "
    "and can list many counties for one event. A flood counts as reached when the threshold was reached between 30 days before it began and its end.",
    f"How often: (number of years + 1) divided by the years reached, over the {N} full years {FIRST_YEAR}-{LAST_FULL} (Weibull return period).",
    "Checks: every count on this page was recalculated a second way, from the county rainfall series and the raw EM-DAT file, and matched. Each flood matched to a county was checked against the EM-DAT location text.",
]
if dbc:
    METHOD.append(f"The county rainfall matches the team's database copy of IMERG (correlation {dbc['corr']}). "
                  + " ".join(f"The one difference that changes a count: {k} in {d['year']} is {d['cog']} mm here and {d['db']} mm in the database, "
                             f"so {k} reaches 150 mm in {len(v['cog'])} or {len(v['db'])} years depending on the copy used."
                             for k, v in dbc["trigger_years"].items() for d in v["differs"]))


def ul(items, cls=""):
    return f"<ul class='{cls}'>" + "".join(f"<li>{esc(t)}</li>" for t in items) + "</ul>"


# ------------------------------------------------------------------ page
CSS = """
:root{color-scheme:light;--bg:#F4F6F7;--surface:#FFFFFF;--ink:#15212A;--ink2:#46555F;--muted:#76838C;--rule:#DDE3E6;--rule2:#EEF1F3;
--accent:#1D5B7C;--dot:#1D5B7C;--band:rgba(140,58,11,.13);--note:#FFF7E6;--noteb:#D08A3E;
--sans:"Public Sans",system-ui,-apple-system,"Segoe UI",sans-serif;--mono:"IBM Plex Mono",ui-monospace,Consolas,monospace}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;
--muted:#7F8C94;--rule:#2B353B;--rule2:#222B31;--accent:#7DB6D6;--dot:#7DB6D6;--band:rgba(242,179,107,.18);--note:#2A2418;--noteb:#B9773A}}
:root[data-theme="dark"]{color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;--muted:#7F8C94;--rule:#2B353B;
--rule2:#222B31;--accent:#7DB6D6;--dot:#7DB6D6;--band:rgba(242,179,107,.18);--note:#2A2418;--noteb:#B9773A}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font-family:var(--sans);font-size:16px;line-height:1.55}
.wrap{max-width:980px;margin:0 auto;padding:36px 16px 56px}
header{padding-bottom:18px;border-bottom:1px solid var(--rule)}
.eyebrow{font:500 12px/1 var(--mono);letter-spacing:.08em;text-transform:uppercase;color:var(--accent)}
h1{font-size:clamp(26px,4.6vw,38px);line-height:1.12;margin:12px 0;font-weight:700;letter-spacing:-.015em;text-wrap:balance}
.lede{color:var(--ink2);max-width:66ch;margin:0}
.note{background:var(--note);border-left:4px solid var(--noteb);border-radius:4px;padding:12px 18px;margin-top:22px;max-width:80ch}
.note b{display:block;margin-bottom:2px}
section{margin-top:40px}
h2{font-size:21px;margin:0 0 8px;font-weight:600}
h4{font-size:13px;margin:10px 0 4px}
.sub{color:var(--ink2);max-width:80ch;margin:0 0 14px;font-size:15px}
ul.findings{padding-left:20px;margin:0 0 18px;max-width:80ch}ul.findings li{margin:8px 0}
.panel{background:var(--surface);border:1px solid var(--rule);border-radius:6px;padding:14px 16px}
.scroll{overflow-x:auto}
table{border-collapse:collapse;width:100%;font-size:15px;background:var(--surface);border:1px solid var(--rule)}
th,td{padding:9px 12px;border-bottom:1px solid var(--rule2);text-align:left;vertical-align:top}
th{font-size:12px;font-weight:600;color:var(--muted);line-height:1.3}
.n{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
.sm{font-size:12.5px;color:var(--muted)}
.mono{font-family:var(--mono);font-size:13px}
.timeline .svgwrap{overflow-x:auto}
.timeline .svgwrap svg{display:block;width:100%;height:auto;min-width:680px}
.timeline .svgwrap svg text{font-family:var(--sans);fill:var(--ink2);font-size:13px}
.timeline .svgwrap svg text.strong{fill:var(--ink);font-weight:600}
.legend{display:flex;flex-wrap:wrap;gap:6px 18px;font-size:13px;color:var(--ink2);margin-top:8px}
.legend span{white-space:nowrap}
.legend svg{display:inline-block;width:14px;height:14px;vertical-align:-2px;margin-right:5px}
.legend i{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-1px;margin-right:5px}
details{margin-top:10px;font-size:14px}
summary{cursor:pointer;color:var(--accent);font-weight:500}
ul.dates{list-style:none;padding:0;margin:4px 0 6px;columns:3 200px;column-gap:24px}
ul.dates li{padding:2px 0;break-inside:avoid;color:var(--ink2)}
ul.notes{padding-left:20px;max-width:80ch;color:var(--ink2)}ul.notes li{margin:6px 0}
.tip{position:fixed;pointer-events:none;background:var(--surface);color:var(--ink);border:1px solid var(--rule);border-radius:4px;padding:6px 9px;font-size:13px;box-shadow:0 2px 8px rgba(0,0,0,.12);display:none;z-index:9;max-width:300px}
.tip .sm{margin-top:2px}
footer{margin-top:44px;padding-top:14px;border-top:1px solid var(--rule);font-size:13px;color:var(--ink2)}
"""

JS = r"""
const A = JSON.parse(document.getElementById('data').textContent);
const tip = document.getElementById('tip');
function el(t, a, txt){ const e = document.createElementNS('http://www.w3.org/2000/svg', t); for (const k in a) e.setAttribute(k, a[k]); if (txt != null) e.textContent = txt; return e; }
function show(ev, main, sub){ tip.textContent = ''; const b = document.createElement('div'); b.textContent = main; b.style.fontWeight = 600; tip.appendChild(b);
  if (sub) { const s = document.createElement('div'); s.className = 'sm'; s.textContent = sub; tip.appendChild(s); }
  tip.style.display = 'block'; tip.style.left = Math.min(ev.clientX + 14, innerWidth - 310) + 'px'; tip.style.top = Math.min(ev.clientY + 14, innerHeight - 70) + 'px'; }
function hide(){ tip.style.display = 'none'; }
document.querySelectorAll('.timeline').forEach(panel => {
  const L = A.lanes[panel.dataset.group];
  const W = 1000, lh = 34, gap = 8, ml = 100, mr = 8, mt = 4, mb = 26;
  let yy = mt; const pos = L.map((ln, i) => { if (i > 0 && L[i - 1].key !== ln.key && !(ln.key.startsWith('whh') && L[i - 1].key.startsWith('whh'))) yy += gap; const t = yy; yy += lh; return t; });
  const H = yy + mb;
  const t0 = Date.parse(A.first), t1 = Date.parse(A.last);
  const x = d => ml + (Date.parse(d) - t0) / (t1 - t0) * (W - ml - mr);
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'Dates each threshold was reached, with recorded floods'});
  const ex = x(A.emLast);
  svg.appendChild(el('rect', {x: ex, y: mt, width: W - mr - ex, height: yy - mt, fill: 'var(--rule2)'}));
  // flood shading behind the rows of the same county group
  const groups = [];
  L.forEach((ln, i) => { const fk = ln.key.startsWith('whh') ? 'whh_70' : ln.key; const g = groups[groups.length - 1];
    if (g && g.key === fk) g.bot = pos[i] + lh; else groups.push({key: fk, top: pos[i], bot: pos[i] + lh}); });
  groups.forEach(g => (A.floods[g.key] || []).forEach(f => {
    const r = el('rect', {x: x(f.s), y: g.top, width: Math.max(3, x(f.e) - x(f.s)), height: g.bot - g.top, fill: 'var(--band)'});
    r.addEventListener('pointermove', ev => show(ev, 'Recorded flood', f.s + ' to ' + f.e + (f.a ? ' | ' + f.a.toLocaleString('en') + ' people affected' : '') + ' | EM-DAT ' + f.id));
    r.addEventListener('pointerleave', hide); svg.appendChild(r);
  }));
  const ya = +A.first.slice(0, 4), yb = +A.last.slice(0, 4);
  for (let y = ya; y <= yb; y += 2) svg.appendChild(el('text', {x: x(y + '-01-01'), y: H - 6, 'text-anchor': 'middle'}, String(y)));
  L.forEach((ln, i) => {
    const yc = pos[i] + lh / 2;
    svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: yc, y2: yc, stroke: 'var(--rule)'}));
    svg.appendChild(el('text', {x: ml - 12, y: yc + 4, 'text-anchor': 'end', class: 'strong'}, ln.label));
    ln.acts.forEach(a => {
      const xx = x(a.d);
      svg.appendChild(el('circle', {cx: xx, cy: yc, r: 6, fill: 'var(--dot)', stroke: 'var(--surface)', 'stroke-width': 2}));
      const hit = el('circle', {cx: xx, cy: yc, r: 12, fill: 'transparent'});
      hit.addEventListener('pointermove', ev => show(ev, a.p + ' mm', ln.label + ' | reached ' + a.d));
      hit.addEventListener('pointerleave', hide);
      svg.appendChild(hit);
    });
  });
  panel.querySelector('.svgwrap').appendChild(svg);
});
"""

PAYLOAD_JSON = json.dumps(payload).replace("</", "<\\/")
SECTIONS_HTML = "".join(section_html(s) for s in SECTIONS)
html = f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Kenya Flood Triggers</title>
<meta name="description" content="How often the Kenya Humanitarian Fund RA2 rainfall triggers would have been reached since {FIRST_YEAR}, against recorded floods.">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>{CSS}</style>
</head>
<body>
<div class="wrap">
<header>
  <div class="eyebrow">OCHA ROSEA support | Kenya | floods</div>
  <h1>Kenya flood triggers: how often they would have been reached</h1>
  <p class="lede">The Kenya Humanitarian Fund's RA2 allocation (September 2026) sets aside USD 4 million for anticipatory action ahead of El Niño floods. This page checks each of its rainfall triggers against rainfall and recorded floods since {FIRST_YEAR}.</p>
</header>

<div class="note"><b>Different data source: results may vary</b>The triggers are written on forecasts from KMSA (Kenya Meteorological Service Authority). This page uses NASA's IMERG satellite rainfall estimates instead. Totals from the two sources differ, so the dates and counts here may not match what KMSA data would give.</div>

<section id="findings">
<h2>Key findings</h2>
{findings_html()}
{summary_table()}
</section>

{SECTIONS_HTML}

<section id="notes">
<h2>What this page does not cover</h2>
{ul(NOT_COVERED, 'notes')}
<details><summary>Method and data</summary>{ul(METHOD, 'notes')}</details>
</section>

<footer>OCHA Centre for Humanitarian Data, Data Science team | ocha-datascience@un.org | Built {date.today().isoformat()} | Sources: NASA GPM IMERG, EM-DAT (CRED / UCLouvain), OCHA COD-AB, Kenya Humanitarian Fund RA2 allocation paper</footer>
</div>
<div class="tip" id="tip"></div>
<script id="data" type="application/json">{PAYLOAD_JSON}</script>
<script>{JS}</script>
</body>
</html>
"""

out_path = HERE / "ken_khf_trigger_review.html"
out_path.write_text(html, encoding="utf8")
print("wrote", out_path, f"{out_path.stat().st_size / 1024:.0f} KB")

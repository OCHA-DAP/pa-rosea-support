"""Build flood/ken/ken_khf_trigger_review.html: when each KHF RA2 rainfall trigger would
have been reached since 1998, against recorded floods. Single self-contained page.

Reads the tables from khf_trigger_review.py, khf_activations.py and (optional) crosscheck_db.py.
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

summ = pd.read_csv(OUT / "activation_summary.csv")
acts = pd.read_csv(OUT / "activations.csv", parse_dates=["date"])
ov = pd.read_csv(OUT / "overall_return_periods.csv")
em = pd.read_csv(OUT / "emdat_events_clean.csv", parse_dates=["start", "end"])
rp = pd.read_csv(OUT / "threshold_return_periods.csv")
daily = pd.read_parquet(OUT / "daily_area_series.parquet")
dbc = json.loads((OUT / "db_crosscheck.json").read_text()) if (OUT / "db_crosscheck.json").exists() else None

LAST_DATE = pd.Timestamp(daily.index.max()).date()
FIRST_YEAR = pd.Timestamp(daily.index.min()).year
LAST_FULL = LAST_DATE.year - 1
N = LAST_FULL - FIRST_YEAR + 1
EM_LAST = em["end"].max()

NAME = {"krcs_mandera": "Mandera", "krcs_wajir": "Wajir", "krcs_marsabit": "Marsabit",
        "whh_70": "Ewaso Ng'iro area", "whh_100": "Ewaso Ng'iro area", "krcs_garissa": "Upper Tana"}
FLOOD_COUNTIES = {"krcs_mandera": ["Mandera"], "krcs_wajir": ["Wajir"], "krcs_marsabit": ["Marsabit"],
                  "whh_70": ["Isiolo", "Samburu"], "whh_100": ["Isiolo", "Samburu"],
                  "krcs_garissa": ["Garissa", "Tana River", "Dadaab"]}
SECTIONS = [
    dict(id="krcs-ne", title="Mandera, Wajir, Marsabit",
         partner="Kenya Red Cross Society",
         written="KMSA 7-day rainfall forecast above 150 mm.",
         tested="7-day rainfall, county average. Floods: EM-DAT events naming the county.",
         rows=[(k, lv) for k in ("krcs_mandera", "krcs_wajir", "krcs_marsabit") for lv in ("as written", "1-in-3", "1-in-5")]),
    dict(id="whh", title="Isiolo and Samburu",
         partner="Welthungerhilfe",
         written="KMSA 7-day forecast of 70-100 mm or more over Isiolo, Samburu and the upper Ewaso Ng'iro (Nyeri, Nyandarua, Laikipia, Meru).",
         tested="7-day rainfall averaged over the six counties. Floods: EM-DAT events naming Isiolo or Samburu.",
         rows=[("whh_70", "as written"), ("whh_100", "as written"), ("whh_70", "1-in-3"), ("whh_70", "1-in-5")]),
    dict(id="garissa", title="Garissa",
         partner="Kenya Red Cross Society",
         written="Garissa Bridge above 5.1 m, or a KMSA heavy rainfall advisory of at least 40 mm for the Tana basin.",
         tested="Rainfall leg only: 1-day rainfall averaged over the upper Tana counties. Floods: EM-DAT events naming Garissa, Tana River or Dadaab.",
         rows=[("krcs_garissa", lv) for lv in ("as written", "1-in-3", "1-in-5")]),
]


def esc(s):
    return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def rp_txt(v):
    if v is None or pd.isna(v):
        return "never"
    return "every year" if v <= 1.15 else f"1-in-{v:g}"


def srow(k, lv):
    return summ[(summ.trigger == k) & (summ.level == lv)].iloc[0]


def lane_label(k, lv):
    r = srow(k, lv)
    return f"{NAME[k]} {int(r.threshold_mm)} mm" + ("" if lv == "as written" else f" ({lv})")


# ------------------------------------------------------------------ timeline data
lanes = {}
for s in SECTIONS:
    lanes[s["id"]] = [dict(key=k, aw=(lv == "as written"), label=lane_label(k, lv),
                           acts=[dict(d=a.date.strftime("%Y-%m-%d"), p=int(a.peak_mm), o=a.outcome,
                                      e=("" if pd.isna(a.emdat) else a.emdat),
                                      l=(None if pd.isna(a.lead_days) else int(a.lead_days)))
                                 for a in acts[(acts.trigger == k) & (acts.level == lv)].itertuples()])
                      for k, lv in s["rows"]]
floods = {}
for k, cs in FLOOD_COUNTIES.items():
    keys = [c.lower().replace(" ", "") for c in cs]
    sub = em[em["Location"].fillna("").str.lower().str.replace(" ", "").apply(lambda t: any(x in t for x in keys))]
    floods[k] = [dict(id=r["DisNo."], s=r["start"].strftime("%Y-%m-%d"), e=r["end"].strftime("%Y-%m-%d"),
                      a=(None if pd.isna(r["Total Affected"]) else int(r["Total Affected"]))) for _, r in sub.iterrows()]
payload = dict(lanes=lanes, floods=floods, first=f"{FIRST_YEAR}-01-01", last=f"{LAST_DATE.year}-12-31",
               emLast=EM_LAST.strftime("%Y-%m-%d"))

# ------------------------------------------------------------------ html pieces
def summary_table():
    rows = [("krcs_mandera", "Kenya Red Cross Society"), ("krcs_wajir", "Kenya Red Cross Society"),
            ("krcs_marsabit", "Kenya Red Cross Society"), ("whh_70", "Welthungerhilfe"), ("whh_100", "Welthungerhilfe"),
            ("krcs_garissa", "Kenya Red Cross Society")]
    h = ["<div class='scroll'><table><thead><tr><th>Trigger</th><th>Threshold</th>"
         f"<th class='n'>Years reached (of {N})</th><th class='n'>Return period</th><th class='n'>Recorded floods it caught</th></tr></thead><tbody>"]
    for k, partner in rows:
        r = srow(k, "as written")
        h.append(f"<tr><td>{esc(NAME[k])}<div class='sm'>{esc(partner)}</div></td>"
                 f"<td>{int(r.threshold_mm)} mm in {int(r.window_days)} day{'s' if r.window_days > 1 else ''}</td>"
                 f"<td class='n'>{int(r.years_activated)}</td><td class='n'>{rp_txt(r.rp_years)}</td>"
                 f"<td class='n'>{int(r.floods_caught)} of {int(r.floods_in_record)}</td></tr>")
    h.append("</tbody></table></div>")
    return "".join(h)


def section_table(s):
    h = ["<div class='scroll'><table><thead><tr><th>Threshold</th>"
         f"<th class='n'>Years reached</th><th class='n'>Return period</th><th class='n'>Floods caught</th>"
         "<th class='n'>Reached with no recorded flood</th></tr></thead><tbody>"]
    for k, lv in s["rows"]:
        r = srow(k, lv)
        h.append(f"<tr class='{'aw' if lv == 'as written' else ''}'><td>{esc(lane_label(k, lv))}</td>"
                 f"<td class='n'>{int(r.years_activated)}</td><td class='n'>{rp_txt(r.rp_years)}</td>"
                 f"<td class='n'>{int(r.floods_caught)} of {int(r.floods_in_record)}</td>"
                 f"<td class='n'>{int(r.no_recorded_flood)} of {int(r.evaluable)}</td></tr>")
    h.append("</tbody></table></div>")
    return "".join(h)


def dates_list(s):
    keys = [k for k, lv in s["rows"] if lv == "as written"]
    a = acts[(acts.trigger.isin(keys)) & (acts.level == "as written")].copy()
    a["_o"] = a["trigger"].map({k: i for i, k in enumerate(keys)})
    lines = []
    for k in keys:
        sub = a[a.trigger == k].sort_values("date")
        items = []
        for r in sub.itertuples():
            ev = f" | {esc(r.emdat)}" if isinstance(r.emdat, str) and r.emdat else ""
            items.append(f"<li><span class='mono'>{r.date.strftime('%d %b %Y')}</span> | {int(r.peak_mm)} mm | {esc(r.outcome)}{ev}</li>")
        lines.append(f"<h4>{esc(lane_label(k, 'as written'))}</h4><ul class='dates'>{''.join(items)}</ul>")
    return f"<details><summary>Dates reached at the threshold as written</summary>{''.join(lines)}</details>"


def section_html(s):
    return f"""
<section id="{s['id']}">
  <h2>{esc(s['title'])}</h2>
  <p class="sub"><b>{esc(s['partner'])}, as written:</b> {esc(s['written'])}<br><b>Tested:</b> {esc(s['tested'])}</p>
  <div class="panel timeline" data-group="{s['id']}"><div class="svgwrap"></div>{LEGEND}</div>
  {section_table(s)}
  {dates_list(s)}
</section>"""


LEGEND = ("<div class='legend'>"
          "<span><svg width='14' height='14'><circle cx='7' cy='7' r='5' fill='var(--before)'/></svg>before a recorded flood</span>"
          "<span><svg width='14' height='14'><rect x='2.5' y='2.5' width='9' height='9' transform='rotate(45 7 7)' fill='var(--during)'/></svg>during a recorded flood</span>"
          "<span><svg width='14' height='14'><circle cx='7' cy='7' r='4.5' fill='none' stroke='var(--muted)' stroke-width='1.6'/></svg>no recorded flood</span>"
          "<span><i style='background:var(--band)'></i>recorded flood period</span>"
          f"<span><i style='background:var(--rule2)'></i>no flood record after {EM_LAST.year}</span></div>")

# ------------------------------------------------------------------ headline numbers (from the tables)
ne = summ[(summ.level == "as written") & (summ.trigger.isin(["krcs_mandera", "krcs_wajir", "krcs_marsabit"]))]
n150, d150, b150 = int(ne.activations.sum()), int(ne.during_flood.sum()), int(ne.before_flood.sum())
all_aw = ov[(ov.group == "all") & (ov.level == "as written")].iloc[0]
w70 = srow("whh_70", "as written")
ut = srow("krcs_garissa", "as written")
px = rp[(rp.window_days == 7) & (rp.threshold_mm == 150) & (rp.reading == "pixel") & rp.area.isin(["Mandera", "Wajir", "Marsabit"])]
px_min = int(px.years_exceeded.min())

KEY_POINTS = [
    f"150 mm in 7 days was reached {n150} times in Mandera, Wajir and Marsabit. {d150} were during a flood EM-DAT had already recorded in that county; {b150} came before one.",
    f"70 mm in 7 days over the Ewaso Ng'iro area was reached in {int(w70.years_activated)} of {N} years. {int(w70.no_recorded_flood)} of its {int(w70.evaluable)} activations had no recorded flood in Isiolo or Samburu.",
    f"40 mm in a day over the upper Tana was reached in {int(ut.years_activated)} of {N} years and caught {int(ut.floods_caught)} of {int(ut.floods_in_record)} recorded floods downstream.",
    f"At least one trigger as written would have been reached in {int(all_aw.years_activated)} of {N} years.",
]

NOT_COVERED = [
    "Forecast skill. Observed rainfall stands in for the KMSA forecast; a forecast of the same total would be issued up to 7 days earlier.",
    "The gauge triggers: Garissa Bridge 5.1 m (Kenya Red Cross Society), Flood Alert at Garissa 3.0-3.5 m (Dadaab partners), and the Danish Refugee Council 24-hour trigger for Darika, which gives no rainfall amount.",
    f"How the thresholds apply over space. The document does not say. This page uses the county average; read at the wettest point in the county, 150 mm is reached in {px_min} or more of {N} years.",
]

METHOD = [
    f"Rainfall: NASA IMERG Late Run v7, daily, 0.1 degree, {FIRST_YEAR}-01-01 to {LAST_DATE}. County boundaries: Kenya COD-AB.",
    "Activation: the first day the running total reaches the threshold; the next 30 days count as the same activation. 1-in-3 and 1-in-5 thresholds are set on the annual maximum of the same indicator.",
    f"Floods: EM-DAT events whose location names the trigger's counties, {FIRST_YEAR} to {EM_LAST.year}. \"Before\": an event starts within 30 days after the activation. \"During\": an event was already under way. EM-DAT dates are often season-long and one event can list many counties.",
    f"Return periods: Weibull, (n + 1) / years reached, over {N} full years ({FIRST_YEAR}-{LAST_FULL}).",
]
if dbc:
    METHOD.append(f"County averages match the team database table public.imerg (correlation {dbc['corr']}, median difference {dbc['median_abs_diff_mm']} mm). "
                  + " ".join(f"{k} {d['year']} is {d['cog']} mm here and {d['db']} mm in the database." for k, v in dbc["trigger_years"].items() for d in v["differs"]))


def ul(items, cls=""):
    return f"<ul class='{cls}'>" + "".join(f"<li>{esc(t)}</li>" for t in items) + "</ul>"


# ------------------------------------------------------------------ page
CSS = """
:root{color-scheme:light;--bg:#F4F6F7;--surface:#FFFFFF;--ink:#15212A;--ink2:#46555F;--muted:#76838C;--rule:#DDE3E6;--rule2:#EEF1F3;
--accent:#1D5B7C;--before:#4a3aa7;--during:#1baf7a;--band:rgba(140,58,11,.18);
--sans:"Public Sans",system-ui,-apple-system,"Segoe UI",sans-serif;--mono:"IBM Plex Mono",ui-monospace,Consolas,monospace}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;
--muted:#7F8C94;--rule:#2B353B;--rule2:#222B31;--accent:#7DB6D6;--before:#9085e9;--during:#199e70;--band:rgba(242,179,107,.22)}}
:root[data-theme="dark"]{color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;--muted:#7F8C94;--rule:#2B353B;
--rule2:#222B31;--accent:#7DB6D6;--before:#9085e9;--during:#199e70;--band:rgba(242,179,107,.22)}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font-family:var(--sans);font-size:15px;line-height:1.55}
.wrap{max-width:1040px;margin:0 auto;padding:36px 16px 56px}
header{padding-bottom:20px;border-bottom:1px solid var(--rule)}
.eyebrow{font:500 12px/1 var(--mono);letter-spacing:.08em;text-transform:uppercase;color:var(--accent)}
h1{font-size:clamp(26px,4.6vw,38px);line-height:1.1;margin:12px 0;font-weight:700;letter-spacing:-.015em;text-wrap:balance}
.lede{color:var(--ink2);max-width:70ch;margin:0}
.meta{font:12px/1.6 var(--mono);color:var(--muted);margin-top:10px}
section{margin-top:44px}
h2{font-size:21px;margin:0 0 6px;font-weight:600}
h4{font-size:13px;margin:12px 0 4px}
.sub{color:var(--ink2);max-width:80ch;margin:0 0 14px;font-size:14px}
.key{background:var(--surface);border:1px solid var(--rule);border-left:4px solid var(--accent);border-radius:4px;padding:14px 20px;margin-top:22px}
.key ul{margin:0;padding-left:18px}.key li{margin:4px 0}
.panel{background:var(--surface);border:1px solid var(--rule);border-radius:6px;padding:14px 16px;margin-bottom:12px}
.scroll{overflow-x:auto}
table{border-collapse:collapse;width:100%;font-size:14px;background:var(--surface);border:1px solid var(--rule);border-radius:6px}
th,td{padding:7px 12px;border-bottom:1px solid var(--rule2);text-align:left;vertical-align:top}
th{font:500 11px var(--mono);letter-spacing:.05em;text-transform:uppercase;color:var(--muted)}
.n{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
tr.aw td{font-weight:600}
.sm{font-size:12px;color:var(--muted);font-weight:400}
.mono{font-family:var(--mono);font-size:12.5px}
.timeline .svgwrap{overflow-x:auto}
.timeline .svgwrap svg{display:block;width:100%;height:auto;min-width:720px}
.timeline .svgwrap svg text{font-family:var(--sans);fill:var(--ink2);font-size:12px}
.timeline .svgwrap svg text.strong{fill:var(--ink);font-weight:600}
.legend{display:flex;flex-wrap:wrap;gap:6px 16px;font-size:12.5px;color:var(--ink2);margin-top:6px}
.legend span{white-space:nowrap}
.legend svg{display:inline-block;width:14px;height:14px;vertical-align:-2px;margin-right:5px}
.legend i{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-1px;margin-right:5px}
details{margin-top:10px;font-size:14px}
summary{cursor:pointer;color:var(--accent);font-weight:500}
ul.dates{list-style:none;padding:0;margin:0 0 6px;columns:2 320px;column-gap:24px}
ul.dates li{padding:2px 0;break-inside:avoid;color:var(--ink2)}
ul.notes{padding-left:18px;max-width:80ch;color:var(--ink2)}ul.notes li{margin:6px 0}
.tip{position:fixed;pointer-events:none;background:var(--surface);color:var(--ink);border:1px solid var(--rule);border-radius:4px;padding:6px 9px;font-size:12.5px;box-shadow:0 2px 8px rgba(0,0,0,.12);display:none;z-index:9;max-width:300px}
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
  const W = 1000, lh = 28, ml = 200, mr = 8, mt = 6, mb = 24, H = mt + L.length * lh + mb;
  const t0 = Date.parse(A.first), t1 = Date.parse(A.last);
  const x = d => ml + (Date.parse(d) - t0) / (t1 - t0) * (W - ml - mr);
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'Dates each threshold was reached, with recorded flood periods'});
  const y0 = +A.first.slice(0, 4), y1 = +A.last.slice(0, 4);
  for (let y = y0; y <= y1; y++) {
    const xx = x(y + '-01-01');
    if (y % 2 === 0) { svg.appendChild(el('line', {x1: xx, x2: xx, y1: mt, y2: H - mb, stroke: 'var(--rule2)'})); svg.appendChild(el('text', {x: xx, y: H - 6, 'text-anchor': 'middle'}, String(y))); }
  }
  const ex = x(A.emLast);
  svg.appendChild(el('rect', {x: ex, y: mt, width: W - mr - ex, height: L.length * lh, fill: 'var(--rule2)'}));
  L.forEach((ln, i) => {
    const top = mt + i * lh, yc = top + lh / 2;
    if (i > 0 && L[i - 1].key !== ln.key) svg.appendChild(el('line', {x1: 0, x2: W - mr, y1: top, y2: top, stroke: 'var(--rule)'}));
    svg.appendChild(el('text', {x: ml - 10, y: yc + 4, 'text-anchor': 'end', class: ln.aw ? 'strong' : ''}, ln.label));
    (A.floods[ln.key] || []).forEach(f => {
      const r = el('rect', {x: x(f.s), y: top + 5, width: Math.max(4, x(f.e) - x(f.s)), height: lh - 10, fill: 'var(--band)', rx: 2});
      r.addEventListener('pointermove', ev => show(ev, 'Recorded flood ' + f.id, f.s + ' to ' + f.e + (f.a ? ' | ' + f.a.toLocaleString('en') + ' people affected' : '')));
      r.addEventListener('pointerleave', hide); svg.appendChild(r);
    });
    svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: yc, y2: yc, stroke: 'var(--rule)', 'stroke-width': .6}));
    ln.acts.forEach(a => {
      const xx = x(a.d); let m;
      if (a.o === 'before a recorded flood') m = el('circle', {cx: xx, cy: yc, r: 5.5, fill: 'var(--before)', stroke: 'var(--surface)', 'stroke-width': 2});
      else if (a.o === 'during a recorded flood') m = el('rect', {x: xx - 5, y: yc - 5, width: 10, height: 10, transform: `rotate(45 ${xx} ${yc})`, fill: 'var(--during)', stroke: 'var(--surface)', 'stroke-width': 2});
      else if (a.o === 'no recorded flood') m = el('circle', {cx: xx, cy: yc, r: 4.5, fill: 'var(--surface)', stroke: 'var(--muted)', 'stroke-width': 1.6});
      else m = el('circle', {cx: xx, cy: yc, r: 3.5, fill: 'var(--muted)'});
      const hit = el('circle', {cx: xx, cy: yc, r: 12, fill: 'transparent'});
      const out = a.o === 'after impact record' ? 'no flood record for this date' : a.o;
      hit.addEventListener('pointermove', ev => show(ev, a.p + ' mm | ' + a.d, ln.label + ' | ' + out + (a.e ? ' ' + a.e : '') + (a.l != null ? ', ' + a.l + ' days before it started' : '')));
      hit.addEventListener('pointerleave', hide);
      svg.appendChild(m); svg.appendChild(hit);
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
<meta name="description" content="When the Kenya Humanitarian Fund RA2 rainfall triggers would have been reached since {FIRST_YEAR}, against recorded floods.">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>{CSS}</style>
</head>
<body>
<div class="wrap">
<header>
  <div class="eyebrow">OCHA ROSEA support | Kenya | floods</div>
  <h1>Kenya Humanitarian Fund flood triggers: when they would have been reached</h1>
  <p class="lede">The Kenya Humanitarian Fund's RA2 allocation (September 2026) sets aside USD 4 million for anticipatory action ahead of El Niño floods, with partner triggers on KMSA rainfall forecasts. This page shows every time since {FIRST_YEAR} that observed rainfall reached each threshold, and whether a flood was recorded in that county at the time.</p>
  <div class="meta">IMERG rainfall {FIRST_YEAR}-{LAST_DATE} | EM-DAT floods {FIRST_YEAR}-{EM_LAST.year} | return periods over {N} years | built {date.today().isoformat()}</div>
</header>

<div class="key">
{ul(KEY_POINTS)}
</div>

<section id="summary">
<h2>Triggers as written</h2>
{summary_table()}
</section>

{SECTIONS_HTML}

<section id="notes">
<h2>Not covered</h2>
{ul(NOT_COVERED, 'notes')}
<details><summary>Method and data</summary>{ul(METHOD, 'notes')}</details>
</section>

<footer>OCHA Centre for Humanitarian Data, Data Science team | ocha-datascience@un.org | Sources: NASA GPM IMERG, EM-DAT (CRED / UCLouvain), OCHA COD-AB, Kenya Humanitarian Fund RA2 allocation paper | Code: flood/ken/ in OCHA-DAP/pa-rosea-support</footer>
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

"""Build flood/ken/ken_khf_trigger_review.html: how often the KHF RA2 rainfall thresholds are reached
in October to December seasons since 1998, read two ways (county average and single grid cell),
against recorded floods. Single self-contained page, written for a non-technical reader.

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

summ = pd.read_csv(OUT / "summary.csv").set_index(["reading", "trigger"])
acts = pd.read_csv(OUT / "activations.csv", parse_dates=["date"])
ov = pd.read_csv(OUT / "overall.csv").set_index(["reading", "group"])
fl = pd.read_csv(OUT / "floods.csv")
cty = pd.read_csv(OUT / "counties.csv")
sx = pd.read_csv(OUT / "season_max.csv")
meta = json.loads((OUT / "meta.json").read_text())
dbc = json.loads((OUT / "db_crosscheck.json").read_text()) if (OUT / "db_crosscheck.json").exists() else None

FIRST_YEAR, LAST_FULL, N = meta["first_year"], meta["last_full_year"], meta["n_years"]
LAST_DATE = meta["last_date"]
EM_LAST_YEAR = int(meta["emdat_last"][:4])
READINGS = [("county", "County average"), ("cell", "Single cell")]

# one row per trigger as written: (key, area shown, partner, lane label)
TRIGGERS = [
    ("krcs_mandera", "Mandera", "Kenya Red Cross Society", "Mandera"),
    ("krcs_wajir", "Wajir", "Kenya Red Cross Society", "Wajir"),
    ("krcs_marsabit", "Marsabit", "Kenya Red Cross Society", "Marsabit"),
    ("whh_isiolo_70", "Isiolo", "Welthungerhilfe", "Isiolo 70 mm"),
    ("whh_isiolo_100", "Isiolo", "Welthungerhilfe", "Isiolo 100 mm"),
    ("whh_samburu_70", "Samburu", "Welthungerhilfe", "Samburu 70 mm"),
    ("whh_samburu_100", "Samburu", "Welthungerhilfe", "Samburu 100 mm"),
    ("krcs_garissa", "Garissa (rainfall part)", "Kenya Red Cross Society", "40 mm"),
]
SECTIONS = [
    dict(id="north-east", title="Mandera, Wajir and Marsabit", partner="Kenya Red Cross Society",
         trigger="KMSA 7-day rainfall forecast above 150 mm. The document does not say whether this is a county average or any one place.",
         shown="Rainfall over 7 days in each county, October to December.",
         keys=["krcs_mandera", "krcs_wajir", "krcs_marsabit"]),
    dict(id="isiolo-samburu", title="Isiolo and Samburu", partner="Welthungerhilfe",
         trigger="KMSA 7-day forecast of \"average rainfall of 70-100mm or more\" over Isiolo, Samburu and the upper Ewaso Ng'iro catchment (Nyeri, Nyandarua, Laikipia, Meru). This is the only trigger that says average.",
         shown="Rainfall over 7 days in Isiolo and in Samburu, each on its own, at both ends of the range, October to December.",
         keys=["whh_isiolo_70", "whh_isiolo_100", "whh_samburu_70", "whh_samburu_100"]),
    dict(id="garissa", title="Garissa", partner="Kenya Red Cross Society",
         trigger="Garissa Bridge river level above 5.1 m, or a KMSA heavy rainfall advisory of at least 40 mm for the Tana basin. The document does not say whether this is a basin average or any one place.",
         shown="The rainfall part only: rainfall in one day over the upper Tana counties upstream of Garissa (Nyeri, Kirinyaga, Murang'a, Embu, Meru, Tharaka-Nithi, Nyandarua), October to December.",
         keys=["krcs_garissa"]),
]
LANE = {k: lab for k, _, _, lab in TRIGGERS}


def esc(s):
    return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def S(reading, k):
    return summ.loc[(reading, k)]


def rp_txt(r):
    """How often, from a summary row: 'every season' only when reached in all N seasons."""
    if int(r.years_reached) == 0:
        return "never"
    if int(r.years_reached) == N:
        return "every season"
    return f"1 in {r.return_period:g} seasons"


def thr_txt(r):
    return f"{int(r.threshold_mm)} mm in {int(r.window_days)} day{'s' if r.window_days > 1 else ''}"


# ------------------------------------------------------------------ chart data
FLOOD_KEY = {"whh_isiolo_100": "whh_isiolo_70", "whh_samburu_100": "whh_samburu_70"}
lanes = {s["id"]: [dict(key=k, fk=FLOOD_KEY.get(k, k), reading=rd, label=f"{LANE[k]}, {'average' if rd == 'county' else 'one cell'}",
                        acts=[dict(d=a.date.strftime("%Y-%m-%d"), p=int(a.peak_mm))
                              for a in acts[(acts.trigger == k) & (acts.reading == rd)].itertuples()])
                   for k in s["keys"] for rd, _ in READINGS] for s in SECTIONS}
fl1 = fl[fl.reading == "county"]  # the flood list is the same for both readings
# flood periods only (start, end): EM-DAT's terms do not allow republishing its records, so no IDs or impact figures
floods = {k: [dict(s=r.start, e=r.end)
              for r in g.itertuples()] for k, g in fl1.groupby("trigger")}
payload = dict(lanes=lanes, floods=floods, first=f"{FIRST_YEAR}-01-01", last=f"{LAST_DATE[:4]}-12-31",
               emLast=meta["emdat_last"])

# ------------------------------------------------------------------ numbers used in the text (all from the tables)
def both(k, field="years_reached"):
    return int(S("county", k)[field]), int(S("cell", k)[field])


def CR(reading, w, t):
    g = cty[(cty.reading == reading) & (cty.window_days == w) & (cty.threshold_mm == t)]
    return int(g.years_reached.min()), int(g.years_reached.max())


ne = [int(ov.loc[(rd, "krcs_ne")].years_reached) for rd, _ in READINGS]
alltr = [int(ov.loc[(rd, "all")].years_reached) for rd, _ in READINGS]
FINDINGS = [
    ("Mandera, Wajir and Marsabit (150 mm in 7 days)",
     f"Reached in at least one of the three counties in {ne[0]} of {N} seasons on the county average, and {ne[1]} of {N} at a single cell."),
    ("Isiolo (70 mm in 7 days)",
     f"Reached in {both('whh_isiolo_70')[0]} of {N} seasons on the county average, and {both('whh_isiolo_70')[1]} of {N} at a single cell. "
     f"At 100 mm: {both('whh_isiolo_100')[0]} and {both('whh_isiolo_100')[1]}."),
    ("Samburu (70 mm in 7 days)",
     f"Reached in {both('whh_samburu_70')[0]} of {N} seasons on the county average, and {both('whh_samburu_70')[1]} of {N} at a single cell. "
     f"At 100 mm: {both('whh_samburu_100')[0]} and {both('whh_samburu_100')[1]}."),
    ("Garissa (40 mm in a day, rainfall part)",
     f"Reached in {both('krcs_garissa')[0]} of {N} seasons on the upper Tana average, and {both('krcs_garissa')[1]} of {N} at a single cell."),
    ("All triggers together, as written",
     f"At least one was reached in {alltr[0]} of {N} seasons on the average, and {alltr[1]} of {N} at a single cell. "
     f"This uses the Welthungerhilfe trigger as written, averaged over six counties."),
    ("Other counties",
     f"In the eight ASAL counties named in the allocation, 150 mm in 7 days was reached in {CR('county', 7, 150)[0]} to {CR('county', 7, 150)[1]} "
     f"of {N} seasons on the county average, and {CR('cell', 7, 150)[0]} to {CR('cell', 7, 150)[1]} at a single cell."),
]


def findings_html():
    return "<ul class='findings'>" + "".join(f"<li><b>{esc(h)}:</b> {esc(t)}</li>" for h, t in FINDINGS) + "</ul>"


def summary_table():
    h = ["<div class='scroll'><table class='sum'><thead>"
         "<tr><th rowspan='2'>Trigger</th><th rowspan='2'>Threshold</th>"
         "<th colspan='2' class='grp g1'>County average</th><th colspan='2' class='grp g2'>Single cell</th></tr>"
         f"<tr><th class='n g1'>Seasons reached<br>(of {N})</th><th class='n g1'>Oct-Dec floods<br>it was reached for</th>"
         f"<th class='n g2'>Seasons reached<br>(of {N})</th><th class='n g2'>Oct-Dec floods<br>it was reached for</th></tr></thead><tbody>"]
    for k, area, partner, _ in TRIGGERS:
        c, x = S("county", k), S("cell", k)
        h.append(f"<tr><td>{esc(area)}<div class='sm'>{esc(partner)}</div></td><td>{thr_txt(c)}</td>"
                 f"<td class='n g1'>{int(c.years_reached)}<div class='sm'>{rp_txt(c)}</div></td>"
                 f"<td class='n g1'>{int(c.floods_reached)} of {int(c.floods)}</td>"
                 f"<td class='n g2'>{int(x.years_reached)}<div class='sm'>{rp_txt(x)}</div></td>"
                 f"<td class='n g2'>{int(x.floods_reached)} of {int(x.floods)}</td></tr>")
    h.append("</tbody></table></div>")
    return "".join(h)


# season-by-season table: (column header, threshold note, trigger key for the value, higher-threshold key or None)
SEASON_COLS = [
    ("Mandera", "150 mm, 7 days", "krcs_mandera", None),
    ("Wajir", "150 mm, 7 days", "krcs_wajir", None),
    ("Marsabit", "150 mm, 7 days", "krcs_marsabit", None),
    ("Isiolo", "70 / 100 mm, 7 days", "whh_isiolo_70", "whh_isiolo_100"),
    ("Samburu", "70 / 100 mm, 7 days", "whh_samburu_70", "whh_samburu_100"),
    ("Garissa (upper Tana)", "40 mm, 1 day", "krcs_garissa", None),
]


def season_table(reading):
    t = sx[sx.reading == reading].set_index(["trigger", "year"])
    years = sorted(sx.year.unique())
    h = [f"<div class='scroll'><table class='season {reading}'><thead><tr><th>Oct-Dec</th>"]
    for name, note, *_ in SEASON_COLS:
        h.append(f"<th class='n'>{esc(name)}<div class='sm'>{esc(note)}</div></th>")
    h.append("</tr></thead><tbody>")
    for y in years:
        h.append(f"<tr><td class='yr'>{y}</td>")
        for _, _, k, k_hi in SEASON_COLS:
            r = t.loc[(k, y)]
            assert (r.max_mm // 1 >= r.threshold_mm) == bool(r.met), (k, y, r.max_mm)  # shown value and highlight agree
            cls = ""
            if k_hi is not None and bool(t.loc[(k_hi, y)].met):
                cls = "met hi"
            elif bool(r.met):
                cls = "met"
            h.append(f"<td class='n {cls}'>{int(r.max_mm // 1)}</td>")  # rounded down, so a shown value at the threshold is always highlighted
        h.append("</tr>")
    h.append(f"<tr class='tot'><td>Seasons reached</td>")
    for _, _, k, k_hi in SEASON_COLS:
        lo = int(S(reading, k).years_reached)
        txt = f"{lo}" if k_hi is None else f"{lo} / {int(S(reading, k_hi).years_reached)}"
        h.append(f"<td class='n'>{txt} of {N}</td>")
    h.append("</tr></tbody></table></div>")
    return "".join(h)


SEASON_SECTION = f"""
<section id="seasons">
  <h2>Season by season</h2>
  <p class="sub">The highest rainfall total in each October to December season, in whole mm rounded down, for each trigger's indicator. Highlighted cells reached the threshold. For Isiolo and Samburu, light means 70 mm was reached and dark means 100 mm was reached.</p>
  <div class="seg" role="group" aria-label="Reading">
    <button type="button" data-r="county" aria-pressed="true"><span class="key k1"></span>County average</button>
    <button type="button" data-r="cell" aria-pressed="false"><span class="key k2"></span>Single cell</button>
  </div>
  <div class="season-wrap" data-r="county">{season_table("county")}</div>
  <div class="season-wrap" data-r="cell" hidden>{season_table("cell")}</div>
</section>"""


def dates_list(s):
    parts = []
    for rd, rname in READINGS:
        for k in s["keys"]:
            a = acts[(acts.trigger == k) & (acts.reading == rd)].sort_values("date")
            items = "".join(f"<li><span class='mono'>{r.date.strftime('%d %b %Y')}</span> | {int(r.peak_mm)} mm</li>" for r in a.itertuples())
            parts.append(f"<h4>{esc(LANE[k])}, {esc(rname.lower())}</h4><ul class='dates'>{items}</ul>")
    return (f"<details><summary>List of dates</summary><p class='sm'>Date the threshold was first reached | highest total on that occasion</p>"
            f"{''.join(parts)}</details>")


LEGEND = ("<div class='legend'>"
          "<span><svg width='14' height='14'><circle cx='7' cy='7' r='4.5' fill='var(--dot)'/></svg>reached, county average</span>"
          "<span><svg width='14' height='14'><circle cx='7' cy='7' r='4.5' fill='var(--dot2)'/></svg>reached, single cell</span>"
          "<span><i style='background:var(--band)'></i>recorded flood starting October to December</span>"
          f"<span><i style='background:var(--rule2)'></i>no flood records after {EM_LAST_YEAR}</span></div>")


def as_written_note(s):
    if s["id"] != "isiolo-samburu":
        return ""
    a70, a100 = S("county", "whh_70"), S("county", "whh_100")
    return (f"<br><b>As written, over the six-county area:</b> reached in {int(a70.years_reached)} of {N} seasons at 70 mm and "
            f"{int(a100.years_reached)} at 100 mm on the average, for {int(a70.floods_reached)} and {int(a100.floods_reached)} of "
            f"{int(a70.floods)} recorded floods in Isiolo or Samburu.")


def section_html(s):
    return f"""
<section id="{s['id']}">
  <h2>{esc(s['title'])}</h2>
  <p class="sub"><b>Trigger ({esc(s['partner'])}):</b> {esc(s['trigger'])}<br><b>Shown here:</b> {esc(s['shown'])}{as_written_note(s)}</p>
  <div class="panel timeline" data-group="{s['id']}"><div class="svgwrap"></div>{LEGEND}</div>
  {dates_list(s)}
</section>"""


COUNTY_COLS = [(7, 150), (7, 100), (7, 70), (1, 40)]
AS_WRITTEN = {("Mandera", 7, 150), ("Wajir", 7, 150), ("Marsabit", 7, 150)}


def county_table():
    t = cty.set_index(["reading", "county", "window_days", "threshold_mm"])
    h = ["<div class='scroll'><table class='heat'><thead><tr><th>County</th>"]
    for w, thr in COUNTY_COLS:
        h.append(f"<th class='n'>{thr} mm in {w} day{'s' if w > 1 else ''}<div class='sm'>average | one cell</div></th>")
    h.append("</tr></thead><tbody>")
    for c in sorted(cty.county.unique()):
        h.append(f"<tr><td>{esc(c)}<div class='sm'>{int(t.loc[('county', c, 7, 150), 'floods'])} Oct-Dec floods</div></td>")
        for w, thr in COUNTY_COLS:
            a, x = t.loc[("county", c, w, thr)], t.loc[("cell", c, w, thr)]
            mark = " aw" if (c, w, thr) in AS_WRITTEN else ""
            h.append(f"<td class='pair{mark}'>"
                     f"<span class='v' style='background:rgba(42,120,214,{0.06 + 0.44 * int(a.years_reached) / N:.2f})'><b>{int(a.years_reached)}</b><i>{int(a.floods_reached)}/{int(a.floods)} floods</i></span>"
                     f"<span class='v' style='background:rgba(235,104,52,{0.06 + 0.44 * int(x.years_reached) / N:.2f})'><b>{int(x.years_reached)}</b><i>{int(x.floods_reached)}/{int(x.floods)} floods</i></span></td>")
        h.append("</tr>")
    h.append("</tbody></table></div>")
    return "".join(h)


COUNTY_SECTION = f"""
<section id="counties">
  <h2>The same thresholds in other counties</h2>
  <p class="sub">Each threshold applied in the eight ASAL counties the allocation names at Severity Level 4. In each cell, the left number is the October to December seasons it was reached ({FIRST_YEAR}-{LAST_FULL}, of {N}) on the county average, the right number at a single cell, each with the recorded floods it was reached for. Outlined cells are the triggers as written. The Garissa trigger covers the upper Tana counties, so its single-county values here differ from the chart above.</p>
  {county_table()}
</section>"""

READING_BOX = f"""<div class="reading"><b>Two ways to read a rainfall threshold</b>
<div class="two"><div><span class="key k1"></span><b>County average</b>: rainfall averaged over the whole county or area. The Welthungerhilfe trigger says "average"; the others do not say.</div>
<div><span class="key k2"></span><b>Single cell</b>: at least one satellite grid cell, about 11 km across, reaches the threshold somewhere in the county or area.</div></div></div>"""

NOT_COVERED = [
    "Whether KMSA forecasts would have predicted these totals. This page uses observed rainfall.",
    "The river-level triggers: Garissa Bridge above 5.1 m (Kenya Red Cross Society), Flood Alert levels at Garissa, Hola and Garsen (Dadaab partners), and the Danish Refugee Council trigger for Darika, which has no rainfall amount.",
    "Which reading KMSA forecasts or the partners use. Both are shown because the document only specifies it for one trigger.",
]

METHOD = [
    f"Season: only rainfall totals whose last day falls between 1 October and 31 December count, and only EM-DAT floods that started in those months. "
    f"The {N} seasons are {FIRST_YEAR}-{LAST_FULL}; the October to December {int(LAST_DATE[:4])} season is not yet in the record.",
    f"Rainfall: NASA IMERG Late Run version 7, daily, about 11 km grid, {FIRST_YEAR}-01-01 to {LAST_DATE}, with Kenya county boundaries (COD-AB).",
    "County average: each grid cell weighted by the share of its area inside the county; areas of several counties weight each county equally. "
    f"Single cell: the wettest grid cell with at least {int(meta.get('min_cover', 0.5) * 100)}% of its area inside the county or area.",
    "A threshold counts as reached on the first day the running total gets to it. Further days in the next 30 days count as the same occasion.",
    f"Floods: events in EM-DAT, the international disaster database, that started in October to December and whose location names the trigger's counties, {FIRST_YEAR} to {EM_LAST_YEAR}. "
    "EM-DAT only includes events that meet its criteria (for example 100 or more people affected), often records one long event for a whole season, "
    "and can list many counties for one event. A flood counts as reached when the threshold was reached between 30 days before it began and its end.",
    f"How often: (number of seasons + 1) divided by the seasons reached, over the {N} seasons {FIRST_YEAR}-{LAST_FULL} (Weibull return period).",
    "Checks: every count on this page, for both readings, was recalculated a second way from the saved rainfall grids and the raw EM-DAT file, and matched. Each flood matched to a county was checked against the EM-DAT location text.",
]
if dbc:
    note = f"The county rainfall matches the team's database copy of IMERG (correlation {dbc['corr']})."
    if dbc.get("season") == meta.get("season"):
        note += "".join(f" One difference changes a count: {k} in {d['year']} is {d['cog']} mm here and {d['db']} mm in the database."
                        for k, v in dbc["trigger_years"].items() for d in v["differs"])
    METHOD.append(note)


def ul(items, cls=""):
    return f"<ul class='{cls}'>" + "".join(f"<li>{esc(t)}</li>" for t in items) + "</ul>"


# ------------------------------------------------------------------ page
CSS = """
:root{color-scheme:light;--bg:#F4F6F7;--surface:#FFFFFF;--ink:#15212A;--ink2:#46555F;--muted:#76838C;--rule:#DDE3E6;--rule2:#EEF1F3;
--accent:#1D5B7C;--dot:#2a78d6;--dot2:#eb6834;--band:rgba(140,58,11,.13);--note:#FFF7E6;--noteb:#D08A3E;
--sans:"Public Sans",system-ui,-apple-system,"Segoe UI",sans-serif;--mono:"IBM Plex Mono",ui-monospace,Consolas,monospace}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;
--muted:#7F8C94;--rule:#2B353B;--rule2:#222B31;--accent:#7DB6D6;--dot:#3987e5;--dot2:#d95926;--band:rgba(242,179,107,.18);--note:#2A2418;--noteb:#B9773A}}
:root[data-theme="dark"]{color-scheme:dark;--bg:#11171B;--surface:#182026;--ink:#E6ECEF;--ink2:#AAB6BD;--muted:#7F8C94;--rule:#2B353B;
--rule2:#222B31;--accent:#7DB6D6;--dot:#3987e5;--dot2:#d95926;--band:rgba(242,179,107,.18);--note:#2A2418;--noteb:#B9773A}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font-family:var(--sans);font-size:16px;line-height:1.55}
.wrap{max-width:1000px;margin:0 auto;padding:36px 16px 56px}
header{padding-bottom:18px;border-bottom:1px solid var(--rule)}
.eyebrow{font:500 12px/1 var(--mono);letter-spacing:.08em;text-transform:uppercase;color:var(--accent)}
h1{font-size:clamp(26px,4.6vw,38px);line-height:1.12;margin:12px 0;font-weight:700;letter-spacing:-.015em;text-wrap:balance}
.note{background:var(--note);border-left:4px solid var(--noteb);border-radius:4px;padding:12px 18px;margin-top:22px;max-width:80ch}
.note b{display:block;margin-bottom:2px}
.reading{background:var(--surface);border:1px solid var(--rule);border-radius:6px;padding:14px 18px;margin-top:14px}
.reading>b{display:block;margin-bottom:6px}
.reading .two{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:8px 24px;font-size:15px;color:var(--ink2)}
.reading .two b{color:var(--ink)}
.key{display:inline-block;width:11px;height:11px;border-radius:50%;margin-right:7px;vertical-align:0}
.key.k1{background:var(--dot)}.key.k2{background:var(--dot2)}
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
table.sum th.grp{text-align:center;color:var(--ink);font-size:13px;border-bottom:2px solid}
table.sum th.grp.g1{border-bottom-color:var(--dot)}table.sum th.grp.g2{border-bottom-color:var(--dot2)}
table.sum td.g1{background:rgba(42,120,214,.05)}table.sum td.g2{background:rgba(235,104,52,.05)}
.n{text-align:right;font-variant-numeric:tabular-nums;white-space:nowrap}
.sm{font-size:12.5px;color:var(--muted);font-weight:400}
.mono{font-family:var(--mono);font-size:13px}
table.heat th.n{text-align:center}
table.heat td.pair{white-space:nowrap;padding:6px}
table.heat td.pair .v{display:inline-flex;flex-direction:column;align-items:center;min-width:64px;padding:4px 6px;border-radius:4px;margin:0 2px}
table.heat td.pair .v b{font-size:17px;font-variant-numeric:tabular-nums}
table.heat td.pair .v i{font-style:normal;font-size:11.5px;color:var(--ink2)}
table.heat td.aw{outline:2px solid var(--ink);outline-offset:-3px}
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
ul.dates{list-style:none;padding:0;margin:4px 0 6px;columns:4 170px;column-gap:24px}
ul.dates li{padding:2px 0;break-inside:avoid;color:var(--ink2)}
ul.notes{padding-left:20px;max-width:80ch;color:var(--ink2)}ul.notes li{margin:6px 0}
.tip{position:fixed;pointer-events:none;background:var(--surface);color:var(--ink);border:1px solid var(--rule);border-radius:4px;padding:6px 9px;font-size:13px;box-shadow:0 2px 8px rgba(0,0,0,.12);display:none;z-index:9;max-width:300px}
.tip .sm{margin-top:2px}
.seg{display:inline-flex;border:1px solid var(--rule);border-radius:6px;overflow:hidden;background:var(--surface);margin:0 0 10px}
.seg button{border:0;background:transparent;color:var(--ink2);padding:7px 14px;font:inherit;font-size:14px;cursor:pointer}
.seg button[aria-pressed="true"]{background:var(--rule2);color:var(--ink);font-weight:600}
table.season{font-size:14px}
table.season th,table.season td{padding:5px 10px}
table.season td.yr{font-variant-numeric:tabular-nums;color:var(--ink2)}
table.season td.n{text-align:center}
table.season th.n{text-align:center}
table.season.county td.met{background:rgba(42,120,214,.28);font-weight:700}
table.season.county td.met.hi{background:rgba(42,120,214,.62)}
table.season.cell td.met{background:rgba(235,104,52,.28);font-weight:700}
table.season.cell td.met.hi{background:rgba(235,104,52,.62)}
table.season tr.tot td{border-top:2px solid var(--rule);font-weight:600;background:var(--rule2)}
.home{display:inline-block;margin:0 0 18px;padding:6px 12px;font:500 13px/1 var(--sans);color:var(--accent);background:var(--surface);border:1px solid var(--rule);border-radius:4px;text-decoration:none}
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
const fkey = ln => ln.fk;   // flood list per county
document.querySelectorAll('.timeline').forEach(panel => {
  const L = A.lanes[panel.dataset.group];
  const W = 1000, lh = 26, gap = 12, ml = 200, mr = 8, mt = 4, mb = 26;
  let yy = mt; const pos = L.map((ln, i) => { if (i > 0 && fkey(L[i - 1]) !== fkey(ln)) yy += gap; const t = yy; yy += lh; return t; });
  const H = yy + mb;
  const t0 = Date.parse(A.first), t1 = Date.parse(A.last);
  const x = d => ml + (Date.parse(d) - t0) / (t1 - t0) * (W - ml - mr);
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'Dates each threshold was reached, county average and single cell, with recorded floods'});
  const ex = x(A.emLast);
  svg.appendChild(el('rect', {x: ex, y: mt, width: W - mr - ex, height: yy - mt, fill: 'var(--rule2)'}));
  const groups = [];
  L.forEach((ln, i) => { const g = groups[groups.length - 1];
    if (g && g.key === fkey(ln)) g.bot = pos[i] + lh; else groups.push({key: fkey(ln), top: pos[i], bot: pos[i] + lh}); });
  groups.forEach(g => (A.floods[g.key] || []).forEach(f => {
    const r = el('rect', {x: x(f.s), y: g.top, width: Math.max(3, x(f.e) - x(f.s)), height: g.bot - g.top, fill: 'var(--band)'});
    r.addEventListener('pointermove', ev => show(ev, 'Recorded flood', f.s + ' to ' + f.e));
    r.addEventListener('pointerleave', hide); svg.appendChild(r);
  }));
  const ya = +A.first.slice(0, 4), yb = +A.last.slice(0, 4);
  for (let y = ya; y <= yb; y += 2) svg.appendChild(el('text', {x: x(y + '-01-01'), y: H - 6, 'text-anchor': 'middle'}, String(y)));
  L.forEach((ln, i) => {
    const yc = pos[i] + lh / 2, col = ln.reading === 'cell' ? 'var(--dot2)' : 'var(--dot)';
    svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: yc, y2: yc, stroke: 'var(--rule)'}));
    svg.appendChild(el('text', {x: ml - 12, y: yc + 4, 'text-anchor': 'end', class: ln.reading === 'county' ? 'strong' : ''}, ln.label));
    ln.acts.forEach(a => {
      const xx = x(a.d);
      svg.appendChild(el('circle', {cx: xx, cy: yc, r: 5.5, fill: col, stroke: 'var(--surface)', 'stroke-width': 1.5}));
      const hit = el('circle', {cx: xx, cy: yc, r: 11, fill: 'transparent'});
      hit.addEventListener('pointermove', ev => show(ev, a.p + ' mm', ln.label + ' | reached ' + a.d));
      hit.addEventListener('pointerleave', hide);
      svg.appendChild(hit);
    });
  });
  panel.querySelector('.svgwrap').appendChild(svg);
});
document.querySelectorAll('#seasons .seg button').forEach(b => b.addEventListener('click', () => {
  document.querySelectorAll('#seasons .seg button').forEach(x => x.setAttribute('aria-pressed', String(x === b)));
  document.querySelectorAll('#seasons .season-wrap').forEach(w => { w.hidden = w.dataset.r !== b.dataset.r; });
}));
"""

PAYLOAD_JSON = json.dumps(payload).replace("</", "<\\/")
SECTIONS_HTML = "".join(section_html(s) for s in SECTIONS)
html = f"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Kenya Flood Triggers</title>
<meta name="description" content="How often the Kenya Humanitarian Fund RA2 rainfall thresholds are reached in October to December seasons since {FIRST_YEAR}, as a county average and at a single grid cell, against recorded floods.">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Public+Sans:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>{CSS}</style>
</head>
<body>
<div class="wrap">
<a class="home" href="../">← ROSEA analyses</a>
<header>
  <div class="eyebrow">OCHA ROSEA support | Kenya | floods</div>
  <h1>Kenya flood triggers: how often the rainfall thresholds are reached in October to December</h1>
</header>

<div class="note"><b>Different data source: results may vary</b>The triggers are written on forecasts from KMSA (Kenya Meteorological Service Authority). This page uses NASA's IMERG satellite rainfall estimates instead. Totals from the two sources differ, so the dates and counts here may not match what KMSA data would give.</div>
{READING_BOX}

<section id="findings">
<h2>At a glance</h2>
<p class="sub">October to December seasons, {FIRST_YEAR} to {LAST_FULL}.</p>
{findings_html()}
{summary_table()}
</section>

{SEASON_SECTION}

{SECTIONS_HTML}

{COUNTY_SECTION}

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

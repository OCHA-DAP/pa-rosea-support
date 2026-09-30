"""Page section: when each KHF rainfall trigger would have been reached (used by build_review_page.py).

Reads the tables written by khf_activations.py and returns (html, js_data, css, js_code).
"""
import json

import pandas as pd

GROUPS = [
    dict(id="act-krcs-ne", title="Kenya Red Cross Society: Mandera, Wajir, Marsabit (150 mm in 7 days)",
         keys=["krcs_mandera", "krcs_wajir", "krcs_marsabit"],
         note="County-mean 7-day IMERG total against 150 mm, one lane per county, with the 1-in-3 and 1-in-5 "
              "year levels of the same indicator below each. Flood periods are EM-DAT events whose location names that county."),
    dict(id="act-whh", title="Welthungerhilfe: Isiolo and Samburu (70-100 mm in 7 days)",
         keys=["whh_70", "whh_100"],
         note="7-day IMERG total averaged over Isiolo, Samburu, Nyeri, Nyandarua, Laikipia and Meru, against 70 mm and 100 mm, "
              "and the 1-in-3 and 1-in-5 year levels of the same indicator. Flood periods are EM-DAT events naming Isiolo or Samburu."),
    dict(id="act-garissa", title="Kenya Red Cross Society: Garissa, rainfall leg (40 mm)",
         keys=["krcs_garissa"],
         note="1-day IMERG total averaged over the upper Tana counties (Nyeri, Kirinyaga, Murang'a, Embu, Meru, Tharaka-Nithi, Nyandarua) "
              "against 40 mm. The gauge leg (Garissa Bridge above 5.1 m) is not included. Flood periods are EM-DAT events naming "
              "Garissa, Tana River or Dadaab."),
]
FLOOD_COUNTIES = {"krcs_mandera": ["Mandera"], "krcs_wajir": ["Wajir"], "krcs_marsabit": ["Marsabit"],
                  "whh_70": ["Isiolo", "Samburu"], "whh_100": ["Isiolo", "Samburu"],
                  "krcs_garissa": ["Garissa", "Tana River", "Dadaab"]}
SHORT = {"krcs_mandera": "Mandera", "krcs_wajir": "Wajir", "krcs_marsabit": "Marsabit",
         "whh_70": "Six-county area", "whh_100": "Six-county area", "krcs_garissa": "Upper Tana"}


def esc(s):
    return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def rows_for(keys):
    """(trigger, level) rows in display order: each threshold as written, then the shared 1-in-3 / 1-in-5 levels."""
    if keys == ["whh_70", "whh_100"]:
        return [("whh_70", "as written"), ("whh_100", "as written"), ("whh_70", "1-in-3"), ("whh_70", "1-in-5")]
    return [(k, lv) for k in keys for lv in ("as written", "1-in-3", "1-in-5")]


def _rp(v):
    if v is None or pd.isna(v):
        return "never"
    return "every year" if v <= 1.15 else f"1-in-{v:g}"


def build(OUT, n_years, first_year, last_full):
    acts = pd.read_csv(OUT / "activations.csv", parse_dates=["date"])
    summ = pd.read_csv(OUT / "activation_summary.csv")
    miss = pd.read_csv(OUT / "missed_floods.csv", parse_dates=["start", "end"])
    ov = pd.read_csv(OUT / "overall_return_periods.csv")
    em = pd.read_csv(OUT / "emdat_events_clean.csv", parse_dates=["start", "end"])
    em_last = em["end"].max()

    # ------------------------------------------------ lanes for the timelines
    lanes = {}
    for g in GROUPS:
        L = []
        for k, lv in rows_for(g["keys"]):
            if True:
                srow = summ[(summ.trigger == k) & (summ.level == lv)].iloc[0]
                a = acts[(acts.trigger == k) & (acts.level == lv)]
                lab = f"{SHORT[k]} {int(srow.threshold_mm)} mm" + ("" if lv == "as written" else f" ({lv})")
                L.append(dict(key=k, level=lv, label=lab, thr=int(srow.threshold_mm),
                              acts=[dict(d=r.date.strftime("%Y-%m-%d"), p=int(r.peak_mm), o=r.outcome,
                                         e=("" if pd.isna(r.emdat) else r.emdat),
                                         l=(None if pd.isna(r.lead_days) else int(r.lead_days))) for r in a.itertuples()]))
        lanes[g["id"]] = L
    floods = {}
    for k, cs in FLOOD_COUNTIES.items():
        keys = [c.lower().replace(" ", "") for c in cs]
        sub = em[em["Location"].fillna("").str.lower().str.replace(" ", "").apply(lambda t: any(x in t for x in keys))]
        floods[k] = [dict(id=r["DisNo."], s=r["start"].strftime("%Y-%m-%d"), e=r["end"].strftime("%Y-%m-%d"),
                          a=(None if pd.isna(r["Total Affected"]) else int(r["Total Affected"]))) for _, r in sub.iterrows()]
    data = dict(lanes=lanes, floods=floods, first=f"{first_year}-01-01", last=f"{last_full + 1}-12-31",
                emLast=em_last.strftime("%Y-%m-%d"))

    # ------------------------------------------------ html
    def summary_table(keys):
        h = ["<div class='scroll'><table class='hot'><thead><tr><th>Trigger</th><th>Threshold</th><th>Activations</th>"
             f"<th>Years with one ({n_years})</th><th>Return period</th><th>Oct-Dec seasons with one</th><th>Oct-Dec return period</th>"
             "<th>Before a recorded flood</th><th>During a recorded flood</th><th>No recorded flood</th>"
             "<th>Recorded floods with an activation</th></tr></thead><tbody>"]
        for k, lv in rows_for(keys):
            if True:
                r = summ[(summ.trigger == k) & (summ.level == lv)].iloc[0]
                name = r.label.split(": ", 1)[1] if ": " in r.label else r.label
                if lv != "as written":
                    name = name.split(" (")[0]
                lvl = "as written" if lv == "as written" else f"{lv} level"
                h.append(f"<tr{' class=' + chr(39) + 'aw' + chr(39) if lv == 'as written' else ''}><td>{esc(name)}<br><span class='small'>{lvl}</span></td>"
                         f"<td class='num'>{int(r.threshold_mm)} mm in {int(r.window_days)} day{'s' if r.window_days > 1 else ''}</td>"
                         f"<td class='num'>{int(r.activations)}</td><td class='num'>{int(r.years_activated)}</td><td class='num'>{_rp(r.rp_years)}</td>"
                         f"<td class='num'>{int(r.ond_seasons_activated)}</td><td class='num'>{_rp(r.rp_ond)}</td>"
                         f"<td class='num'>{int(r.before_flood)}</td><td class='num'>{int(r.during_flood)}</td><td class='num'>{int(r.no_recorded_flood)}</td>"
                         f"<td class='num'>{int(r.floods_caught)} of {int(r.floods_in_record)}</td></tr>")
        h.append("</tbody></table></div>")
        return "\n".join(h)

    def act_list(keys):
        a = acts[(acts.trigger.isin(keys)) & (acts.level == "as written")].copy()
        a["_o"] = a["trigger"].map({k: i for i, k in enumerate(keys)})
        a = a.sort_values(["_o", "date"])
        h = ["<div class='scroll tall'><table class='hot'><thead><tr><th>Date reached</th><th>Where</th><th>Threshold</th><th>Season</th>"
             "<th>Peak</th><th>Days at or above</th><th>Outcome</th><th>EM-DAT event</th><th>People affected</th><th>Days to flood start</th></tr></thead><tbody>"]
        for r in a.itertuples():
            cls = {"before a recorded flood": "o-before", "during a recorded flood": "o-during"}.get(r.outcome, "o-none")
            h.append(f"<tr><td class='mono'>{r.date.strftime('%Y-%m-%d')}</td><td>{esc(SHORT[r.trigger])}</td>"
                     f"<td class='num'>{int(r.threshold_mm)} mm</td><td>{esc(r.season)}</td><td class='num'>{int(r.peak_mm)} mm</td>"
                     f"<td class='num'>{int(r.days_at_or_above)}</td><td><span class='dot {cls}'></span>{esc(r.outcome)}</td>"
                     f"<td class='mono'>{esc('' if pd.isna(r.emdat) else r.emdat)}</td>"
                     f"<td class='num'>{'' if pd.isna(r.affected) else f'{int(r.affected):,}'}</td>"
                     f"<td class='num'>{'' if pd.isna(r.lead_days) else int(r.lead_days)}</td></tr>")
        h.append("</tbody></table></div>")
        return "\n".join(h)

    def miss_list(keys):
        m = miss[(miss.trigger.isin(keys)) & (miss.level == "as written")].copy()
        m["_o"] = m["trigger"].map({k: i for i, k in enumerate(keys)})
        m = m.sort_values(["_o", "start"])
        if not len(m):
            return "<p class='small'>No recorded flood without an activation.</p>"
        h = ["<div class='scroll tall'><table class='hot'><thead><tr><th>EM-DAT event</th><th>Dates</th><th>Trigger</th>"
             "<th>People affected</th><th>Highest indicator value, 30 days before start to end</th></tr></thead><tbody>"]
        for r in m.itertuples():
            note = f" <span class='small'>({esc(r.date_note)})</span>" if isinstance(r.date_note, str) and r.date_note else ""
            h.append(f"<tr><td class='mono'>{esc(r.disno)}</td><td class='mono'>{r.start.strftime('%Y-%m-%d')} to {r.end.strftime('%Y-%m-%d')}{note}</td>"
                     f"<td>{esc(SHORT[r.trigger])} {int(r.threshold_mm)} mm</td>"
                     f"<td class='num'>{'' if pd.isna(r.affected) else f'{int(r.affected):,}'}</td><td class='num'>{int(r.max_indicator_mm)} mm</td></tr>")
        h.append("</tbody></table></div>")
        return "\n".join(h)

    def overall_table():
        h = ["<div class='scroll'><table class='hot'><thead><tr><th>Combination</th><th>Level</th><th>Years with any activation</th>"
             "<th>Overall return period</th><th>Oct-Dec seasons with any activation</th><th>Oct-Dec return period</th><th>Years</th></tr></thead><tbody>"]
        for r in ov.itertuples():
            h.append(f"<tr><td>{esc(r.label)}</td><td>{esc(r.level)}</td><td class='num'>{int(r.years_activated)} of {n_years}</td>"
                     f"<td class='num'>{_rp(r.rp_years)}</td><td class='num'>{int(r.ond_seasons_activated)}</td><td class='num'>{_rp(r.rp_ond)}</td>"
                     f"<td class='mono small'>{esc(r.years)}</td></tr>")
        h.append("</tbody></table></div>")
        return "\n".join(h)

    legend = ("<div class='legend'><span><svg width='14' height='14'><circle cx='7' cy='7' r='5' fill='var(--o-before)'/></svg>before a recorded flood (starts within 30 days)</span>"
              "<span><svg width='14' height='14'><rect x='2.5' y='2.5' width='9' height='9' transform='rotate(45 7 7)' fill='var(--o-during)'/></svg>during a recorded flood</span>"
              "<span><svg width='14' height='14'><circle cx='7' cy='7' r='4.5' fill='none' stroke='var(--muted)' stroke-width='1.6'/></svg>no recorded flood</span>"
              "<span><svg width='14' height='14'><circle cx='7' cy='7' r='3' fill='var(--rule)'/></svg>after the impact record ends</span>"
              "<span><i style='background:var(--flood-band)'></i>EM-DAT flood period</span></div>")

    parts = [f"""
<section id="activations">
<h2>When each trigger would have been reached, {first_year} to {last_full + 1}</h2>
<p class="sub">Every date the observed IMERG indicator for each trigger reached its threshold, set against EM-DAT flood events in the trigger's counties. Rows marked "1-in-3" and "1-in-5" use the same indicator with the threshold set at that return period, for comparison. Observed rainfall stands in for the KMSA forecast: a 7-day forecast of the same total would be issued up to 7 days before the dates listed.</p>
<ul class="notes small">
<li>An activation is the first day at or above the threshold; days in the following 30 days belong to the same activation.</li>
<li><b>Before a recorded flood</b>: an EM-DAT event naming the trigger's counties starts 0 to 30 days after the activation. <b>During a recorded flood</b>: such an event had already started and was still under way. <b>No recorded flood</b>: neither. EM-DAT ends in {em_last.year}; later activations are shown without an outcome.</li>
<li>EM-DAT start dates are often the start of a season-long event, and one event can list many counties.</li>
</ul>
{overall_table()}
</section>"""]
    for g in GROUPS:
        parts.append(f"""
<section id="{g['id']}">
<h3 class="h3big">{esc(g['title'])}</h3>
<p class="sub small">{esc(g['note'])}</p>
<div class="panel timeline" data-group="{g['id']}"><div class="svgwrap"></div>{legend}</div>
{summary_table(g['keys'])}
<details open><summary>Every activation at the threshold as written</summary>
{act_list(g['keys'])}
</details>
<details><summary>Recorded floods with no activation at the threshold as written</summary>
{miss_list(g['keys'])}
</details>
</section>""")
    html = "\n".join(parts)

    css = """
details{margin:10px 0}
details summary{cursor:pointer;font-weight:600;font-size:14px;margin:6px 0}
.scroll.tall{max-height:420px;overflow:auto}
.hot tr.aw td{background:var(--rule2)}
.h3big{font-size:18px;margin:28px 0 4px}
.dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:6px;vertical-align:0}
.dot.o-before{background:var(--o-before)}
.dot.o-during{background:var(--o-during);border-radius:1px;transform:rotate(45deg)}
.dot.o-none{border:1.5px solid var(--muted)}
.timeline{margin:10px 0 12px}
.timeline .svgwrap{overflow-x:auto}
.timeline .svgwrap svg{min-width:760px}
.legend svg{display:inline-block;width:14px;height:14px;vertical-align:-2px;margin-right:5px}
.legend span{white-space:nowrap}
"""
    js = r"""
const A = JSON.parse(document.getElementById('actdata').textContent);
function tlDraw(panel){
  const L = A.lanes[panel.dataset.group];
  const wrap = panel.querySelector('.svgwrap'); wrap.innerHTML = '';
  const W = 1000, lh = 26, ml = 190, mr = 10, mt = 8, mb = 26, H = mt + L.length * lh + mb;
  const t0 = Date.parse(A.first), t1 = Date.parse(A.last);
  const x = d => ml + (Date.parse(d) - t0) / (t1 - t0) * (W - ml - mr);
  const svg = el('svg', {viewBox: `0 0 ${W} ${H}`, role: 'img', 'aria-label': 'Activation timeline'});
  for (let y = +A.first.slice(0,4); y <= +A.last.slice(0,4); y++) {
    const xx = x(y + '-01-01');
    svg.appendChild(el('line', {x1: xx, x2: xx, y1: mt, y2: H - mb, stroke: 'var(--rule2)', 'stroke-width': 1}));
    if (y % 2 === 0) svg.appendChild(el('text', {x: xx + 2, y: H - 8}, String(y)));
  }
  const emx = x(A.emLast);
  svg.appendChild(el('rect', {x: emx, y: mt, width: W - mr - emx, height: L.length * lh, fill: 'var(--rule2)', opacity: .6}));
  L.forEach((ln, i) => {
    const y0 = mt + i * lh, yc = y0 + lh / 2;
    if (i > 0 && L[i-1].key !== ln.key) svg.appendChild(el('line', {x1: 0, x2: W - mr, y1: y0, y2: y0, stroke: 'var(--rule)'}));
    svg.appendChild(el('text', {x: ml - 8, y: yc + 4, 'text-anchor': 'end', class: ln.level === 'as written' ? 'strong' : ''}, ln.label));
    (A.floods[ln.key] || []).forEach(f => {
      const r = el('rect', {x: x(f.s), y: y0 + 4, width: Math.max(4, x(f.e) - x(f.s)), height: lh - 8, fill: 'var(--flood-band)', rx: 2});
      r.addEventListener('pointermove', ev => { const d = document.createElement('div'); const b = document.createElement('b'); b.textContent = f.id; d.appendChild(b);
        d.appendChild(document.createTextNode(' ' + f.s + ' to ' + f.e + (f.a ? ', ' + f.a.toLocaleString('en') + ' affected' : ''))); showTip(ev, d); });
      r.addEventListener('pointerleave', hideTip);
      svg.appendChild(r);
    });
    svg.appendChild(el('line', {x1: ml, x2: W - mr, y1: yc, y2: yc, stroke: 'var(--rule)', 'stroke-width': .6}));
    ln.acts.forEach(a => {
      const xx = x(a.d); let m;
      if (a.o === 'before a recorded flood') m = el('circle', {cx: xx, cy: yc, r: 5, fill: 'var(--o-before)', stroke: 'var(--surface)', 'stroke-width': 2});
      else if (a.o === 'during a recorded flood') m = el('rect', {x: xx - 4.5, y: yc - 4.5, width: 9, height: 9, transform: `rotate(45 ${xx} ${yc})`, fill: 'var(--o-during)', stroke: 'var(--surface)', 'stroke-width': 2});
      else if (a.o === 'no recorded flood') m = el('circle', {cx: xx, cy: yc, r: 4.5, fill: 'var(--surface)', stroke: 'var(--muted)', 'stroke-width': 1.6});
      else m = el('circle', {cx: xx, cy: yc, r: 3.5, fill: 'var(--muted)', opacity: .6});
      const hit = el('circle', {cx: xx, cy: yc, r: 11, fill: 'transparent'});
      const show = ev => { const d = document.createElement('div'); const b = document.createElement('b'); b.textContent = a.p + ' mm'; d.appendChild(b);
        d.appendChild(document.createTextNode(' ' + ln.label + ', reached ' + a.d));
        const p = document.createElement('div'); p.className = 'small';
        p.textContent = a.o + (a.e ? ': ' + a.e : '') + (a.l != null ? ', flood started ' + a.l + ' days later' : ''); d.appendChild(p); showTip(ev, d); };
      hit.addEventListener('pointermove', show); hit.addEventListener('pointerleave', hideTip);
      svg.appendChild(m); svg.appendChild(hit);
    });
  });
  wrap.appendChild(svg);
}
document.querySelectorAll('.timeline').forEach(tlDraw);
"""
    return html, json.dumps(data), css, js

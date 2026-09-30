"""Historical activations of each KHF RA2 rainfall trigger, 1998 to present.

For each trigger (as written, and at the 1-in-3 and 1-in-5 year levels of the same
indicator) this lists every date the observed IMERG indicator would have reached the
threshold, whether an EM-DAT flood naming the trigger's counties followed, the floods
it missed, and individual / overall return periods.

Input : data/out/daily_area_series.parquet, data/out/rp_equivalent_thresholds.csv
        (from khf_trigger_review.py), data/emdat_ken_floods.parquet
Output: data/out/activations.csv, activation_summary.csv, missed_floods.csv,
        overall_return_periods.csv, emdat_events_clean.csv

Definitions
- Activation: the first day the indicator is at or above the threshold. Further days
  within the following 30 days belong to the same activation.
- Outcome, using EM-DAT flood events whose location names one of the trigger's counties:
  "before a recorded flood"  an event starts 0 to 30 days after the activation;
  "during a recorded flood"  an event had already started and was still under way
                             (its period overlaps [activation - 14 days, activation]);
  "no recorded flood"        neither.
- Lead: days from the activation to the start of the next event (outcome "before").
- Miss: an EM-DAT flood naming one of the trigger's counties with no activation in
  [flood start - 30 days, flood end].
- Return periods: Weibull (n + 1) / k over full calendar years, k = years with at
  least one activation (all months) and October-December seasons with one (OND only).
"""
import calendar
import sys
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
OUT = DATA / "out"

daily = pd.read_parquet(OUT / "daily_area_series.parquet")
daily.index = pd.to_datetime(daily.index)
eq = pd.read_csv(OUT / "rp_equivalent_thresholds.csv")
LAST_FULL = daily.index.max().year - 1
FIRST = daily.index.min().year
N_YEARS = LAST_FULL - FIRST + 1
COOLDOWN, BEFORE, AFTER, MISS_BEFORE = 30, 14, 30, 30

# ---------------------------------------------------------------- EM-DAT, dates cleaned
em = pd.read_parquet(DATA / "emdat_ken_floods.parquet").copy()


def _end(r):
    y = int(r["End Year"]) if pd.notna(r["End Year"]) else int(r["Start Year"])
    if pd.isna(r["End Month"]):
        # month unknown: same year -> end of start month; later year -> end of January of that year
        m = int(r["Start Month"]) if y == int(r["Start Year"]) else 1
    else:
        m = int(r["End Month"])
    d = int(r["End Day"]) if pd.notna(r["End Day"]) and pd.notna(r["End Month"]) else calendar.monthrange(y, m)[1]
    return pd.Timestamp(y, m, d)


em["start"] = [pd.Timestamp(int(r["Start Year"]), int(r["Start Month"]), int(r["Start Day"]) if pd.notna(r["Start Day"]) else 1)
               for _, r in em.iterrows()]
em["end"] = [_end(r) for _, r in em.iterrows()]
em["date_note"] = np.where(em["Start Day"].isna() | em["End Day"].isna() | em["End Month"].isna(), "approximate dates", "")
em["loc"] = em["Location"].fillna("").str.lower().str.replace(" ", "")
em = em[em["start"] >= daily.index.min()]
EM_LAST = em["end"].max()
em[["DisNo.", "start", "end", "Total Affected", "Total Deaths", "Location", "date_note"]].to_csv(OUT / "emdat_events_clean.csv", index=False)


def events_for(counties):
    keys = [c.lower().replace(" ", "") for c in counties]
    return em[em["loc"].apply(lambda t: any(k in t for k in keys))]


# ---------------------------------------------------------------- triggers as written
TRIGGERS = [
    dict(key="krcs_mandera", group="krcs_ne", label="Kenya Red Cross Society: Mandera", series="Mandera|7|mean",
         window=7, thr=150, flood_counties=["Mandera"]),
    dict(key="krcs_wajir", group="krcs_ne", label="Kenya Red Cross Society: Wajir", series="Wajir|7|mean",
         window=7, thr=150, flood_counties=["Wajir"]),
    dict(key="krcs_marsabit", group="krcs_ne", label="Kenya Red Cross Society: Marsabit", series="Marsabit|7|mean",
         window=7, thr=150, flood_counties=["Marsabit"]),
    dict(key="whh_70", group="whh", label="Welthungerhilfe: Isiolo and Samburu (70 mm)",
         series="Ewaso Ng'iro area (6 counties)|7|mean", window=7, thr=70, flood_counties=["Isiolo", "Samburu"]),
    dict(key="whh_100", group="whh_100", label="Welthungerhilfe: Isiolo and Samburu (100 mm)",
         series="Ewaso Ng'iro area (6 counties)|7|mean", window=7, thr=100, flood_counties=["Isiolo", "Samburu"]),
    dict(key="krcs_garissa", group="krcs_garissa", label="Kenya Red Cross Society: Garissa, rainfall leg (upper Tana)",
         series="Upper Tana (7 counties)|1|mean", window=1, thr=40, flood_counties=["Garissa", "Tana River", "Dadaab"]),
]


def eq_thr(series, rp):
    area, win, rd = series.split("|")
    r = eq[(eq.area == area) & (eq.window_days == int(win)) & (eq.reading == rd)].iloc[0]
    return int(r[f"thr_1in{rp}_mm"])


def season(d):
    return "OND" if d.month in (10, 11, 12) else ("MAM" if d.month in (3, 4, 5) else "other")


def activations(s, thr):
    acts, last = [], None
    hits = s[s >= thr]
    for d, v in hits.items():
        if last is None or (d - last).days > COOLDOWN:
            acts.append(dict(date=d, peak=float(v), days_at_or_above=1))
            last = d
        else:
            acts[-1]["peak"] = max(acts[-1]["peak"], float(v))
            acts[-1]["days_at_or_above"] += 1
    return acts


def rp(k, n):
    return round((n + 1) / k, 1) if k else None


rows, summ, misses, act_years = [], [], [], {}
for t in TRIGGERS:
    s = daily[t["series"]]
    evs = events_for(t["flood_counties"])
    for level, thr in [("as written", t["thr"]), ("1-in-3", eq_thr(t["series"], 3)), ("1-in-5", eq_thr(t["series"], 5))]:
        acts = activations(s, thr)
        matched_ids = set()
        for a in acts:
            d = a["date"]
            nxt = evs[(evs["start"] >= d) & (evs["start"] <= d + pd.Timedelta(days=AFTER))]
            ongoing = evs[(evs["start"] < d) & (evs["end"] >= d - pd.Timedelta(days=BEFORE))]
            m = pd.concat([nxt, ongoing]).drop_duplicates("DisNo.")
            evaluable = d <= EM_LAST
            if not evaluable:
                outcome = "after impact record"
            elif len(nxt):
                outcome = "before a recorded flood"
            elif len(ongoing):
                outcome = "during a recorded flood"
            else:
                outcome = "no recorded flood"
            matched_ids |= set(m["DisNo."])
            first = nxt.sort_values("start").iloc[0] if len(nxt) else None
            rows.append(dict(trigger=t["key"], label=t["label"], level=level, threshold_mm=thr, window_days=t["window"],
                             date=d.date(), season=season(d), year=d.year, peak_mm=round(a["peak"]),
                             days_at_or_above=a["days_at_or_above"], outcome=outcome,
                             emdat=", ".join(m["DisNo."]),
                             affected=(int(m["Total Affected"].sum()) if len(m) and m["Total Affected"].notna().any() else None),
                             lead_days=(int((first["start"] - d).days) if first is not None else None)))
        # misses: floods with no activation from 30 days before start to end
        act_dates = pd.to_datetime([a["date"] for a in acts])
        n_miss = 0
        for _, e in evs.iterrows():
            ok = ((act_dates >= e["start"] - pd.Timedelta(days=MISS_BEFORE)) & (act_dates <= e["end"])).any() if len(act_dates) else False
            if not ok:
                n_miss += 1
                misses.append(dict(trigger=t["key"], label=t["label"], level=level, threshold_mm=thr, disno=e["DisNo."],
                                   start=e["start"].date(), end=e["end"].date(), season=season(e["start"]),
                                   affected=(None if pd.isna(e["Total Affected"]) else int(e["Total Affected"])),
                                   max_indicator_mm=round(float(s[(s.index >= e["start"] - pd.Timedelta(days=MISS_BEFORE)) & (s.index <= e["end"])].max())),
                                   date_note=e["date_note"]))
        ev_acts = [r for r in rows if r["trigger"] == t["key"] and r["level"] == level and r["outcome"] != "after impact record"]
        n_before = sum(r["outcome"] == "before a recorded flood" for r in ev_acts)
        n_during = sum(r["outcome"] == "during a recorded flood" for r in ev_acts)
        yrs = sorted({a["date"].year for a in acts if a["date"].year <= LAST_FULL})
        ond = sorted({a["date"].year for a in acts if a["date"].year <= LAST_FULL and a["date"].month >= 10})
        act_years[(t["group"], level, t["key"])] = (set(yrs), set(ond))
        leads = [r["lead_days"] for r in ev_acts if r["lead_days"] is not None]
        summ.append(dict(trigger=t["key"], label=t["label"], group=t["group"], level=level, threshold_mm=thr,
                         window_days=t["window"], activations=len(acts),
                         activations_ond=sum(a["date"].month >= 10 for a in acts),
                         evaluable=len(ev_acts), before_flood=n_before, during_flood=n_during,
                         no_recorded_flood=len(ev_acts) - n_before - n_during,
                         floods_in_record=len(evs), floods_caught=len(evs) - n_miss, floods_missed=n_miss,
                         years_activated=len(yrs), rp_years=rp(len(yrs), N_YEARS),
                         ond_seasons_activated=len(ond), rp_ond=rp(len(ond), N_YEARS),
                         median_lead_days=(int(np.median(leads)) if leads else None),
                         activation_years=" ".join(str(y) for y in yrs)))

acts_df = pd.DataFrame(rows)
acts_df.to_csv(OUT / "activations.csv", index=False)
summ_df = pd.DataFrame(summ)
summ_df.to_csv(OUT / "activation_summary.csv", index=False)
pd.DataFrame(misses).to_csv(OUT / "missed_floods.csv", index=False)

# ---------------------------------------------------------------- overall return periods (any trigger in a group)
ov = []
groups = {"krcs_ne": "Kenya Red Cross Society: any of Mandera, Wajir, Marsabit",
          "all": "Any KHF rainfall trigger (Mandera, Wajir, Marsabit, Ewaso Ng'iro area at its lower threshold, upper Tana)"}
for level in ("as written", "1-in-3", "1-in-5"):
    for g, name in groups.items():
        keys = [k for k in act_years if k[1] == level and (k[0] == g or (g == "all" and k[0] != "whh_100"))]
        yrs = set().union(*[act_years[k][0] for k in keys])
        ond = set().union(*[act_years[k][1] for k in keys])
        indiv = [rp(len(act_years[k][0]), N_YEARS) for k in keys]
        ov.append(dict(group=g, label=name, level=level, years_activated=len(yrs), rp_years=rp(len(yrs), N_YEARS),
                       ond_seasons_activated=len(ond), rp_ond=rp(len(ond), N_YEARS),
                       min_individual_rp=min([x for x in indiv if x] or [None]) if any(indiv) else None,
                       years=" ".join(str(y) for y in sorted(yrs))))
ov_df = pd.DataFrame(ov)
# overall RP can never exceed the smallest individual RP
chk = ov_df.dropna(subset=["rp_years", "min_individual_rp"])
assert (chk["rp_years"] <= chk["min_individual_rp"] + 1e-9).all(), chk
ov_df.to_csv(OUT / "overall_return_periods.csv", index=False)

pd.set_option("display.width", 250)
print(f"record {FIRST}-{LAST_FULL} ({N_YEARS} full years); EM-DAT to {EM_LAST.date()}")
print(summ_df.drop(columns=["label", "group"]).to_string(index=False))
print(ov_df.to_string(index=False))
print(acts_df[acts_df.level == "as written"][["trigger", "date", "peak_mm", "outcome", "emdat", "lead_days"]].to_string(index=False))

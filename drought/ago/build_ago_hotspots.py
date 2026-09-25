"""Build the Angola drought hotspots page (drought/ago/ago_hotspots.html).

Pulls every source fresh, cross-checks them, and writes a self-contained HTML
page from ago_hotspots_template.html.

Sources
- JRC ASAP national hotspots (hotspots_ts.zip) and admin-1 warnings (warnings_ts.zip)
- FEWS NET Data Warehouse: ipcphase.csv + ipcphase JSON (cross-check) + ipcpackage shapefiles
- IPC API (analyses, areas; needs IPC_API_KEY) + HDX IPC area file (cross-check)
- HDX COD-AB Angola boundaries (maps and area-to-province lookup)
- WFP on HDX: dekadal CHIRPS rainfall and MODIS NDVI by province (ago-rainfall-subnational, ago-ndvi-subnational)

Run from the repo root:
    uv run --with pandas --with geopandas --with requests python drought/ago/build_ago_hotspots.py
"""

import io
import json
import os
import re
import tempfile
import unicodedata
import zipfile
from pathlib import Path

import geopandas as gpd
import pandas as pd
import requests

HERE = Path(__file__).parent
CACHE = Path(os.environ.get("AGO_CACHE", Path(tempfile.gettempdir()) / "ago_hotspots_cache"))
CACHE.mkdir(parents=True, exist_ok=True)

ASAP = "https://agricultural-production-hotspots.ec.europa.eu/files/"
FDW = "https://fdw.fews.net/api/"
IPC_API = "https://api.ipcinfo.org/"
HDX_IPC_AREA = (
    "https://data.humdata.org/dataset/b57c8667-6403-4af4-9465-5a9f013ae8fc/resource/"
    "10c5b38d-1ac1-48e5-bdf4-3ee1853d437b/download/ipc_ago_area_long.csv"
)
HDX_IPC_L1 = (
    "https://data.humdata.org/dataset/b57c8667-6403-4af4-9465-5a9f013ae8fc/resource/"
    "8d8fa344-3643-48aa-a123-7d82d254bba3/download/ipc_ago_level1_long.csv"
)
HDX_IPC_NAT = (
    "https://data.humdata.org/dataset/b57c8667-6403-4af4-9465-5a9f013ae8fc/resource/"
    "0b392ef6-db95-4e7e-8871-b2e99721eaa5/download/ipc_ago_national_long.csv"
)
COD = (
    "https://data.humdata.org/dataset/a62b2816-7d2e-4e07-a794-0a1dcb507092/resource/"
    "1076fc26-8c42-4196-bb6b-2353487fd7e0/download/ago_admin_boundaries.geojson.zip"
)

FIRST_SEASON = 2001  # warnings start 2001-05; season 2001 = Oct 2001 - Sep 2002
RECENT = [2021, 2022, 2023, 2024, 2025]
CHECKS = []


def check(cond, msg):
    """Record a verification; stop the build if it fails."""
    CHECKS.append(("PASS" if cond else "FAIL", msg))
    if not cond:
        raise AssertionError(msg)


def fetch(url, name, params=None):
    """Download to the cache; set AGO_USE_CACHE=1 to reuse earlier downloads."""
    refresh = not os.environ.get("AGO_USE_CACHE")
    path = CACHE / name
    if refresh or not path.exists():
        r = requests.get(url, params=params, timeout=600)
        r.raise_for_status()
        path.write_bytes(r.content)
    return path


def key(s):
    """Accent/case-insensitive name key; tolerates the IPC API's broken 'í'."""
    s = s.replace("�\xad", "i").replace("�", "")
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    return re.sub(r"[^a-z]", "", s.lower())


def decode_lines(raw):
    """ASAP warnings mix UTF-8 and Latin-1 rows; decode each line on its own."""
    out = []
    for line in raw.split(b"\n"):
        try:
            out.append(line.decode("utf-8"))
        except UnicodeDecodeError:
            out.append(line.decode("latin-1"))
    return "\n".join(out)


# ---------------------------------------------------------------- boundaries
def load_cod():
    z = zipfile.ZipFile(fetch(COD, "ago_cod.zip"))
    adm = {}
    for lvl in (0, 1, 2, 3):
        with z.open(f"ago_admin{lvl}.geojson") as f:
            adm[lvl] = gpd.read_file(f)
    check(len(adm[1]) == 18, f"COD admin1 has 18 provinces (got {len(adm[1])})")
    return adm


# ---------------------------------------------------------------- ASAP
def load_asap(provinces):
    hs_zip = zipfile.ZipFile(fetch(ASAP + "hotspots_ts.zip", "hotspots_ts.zip"))
    hs = pd.read_csv(hs_zip.open("hotspots_ts.csv"), sep=";")
    hs = hs[hs["asap0_name"] == "Angola"].copy()
    hs["date"] = pd.to_datetime(hs["date"])
    hs = hs.sort_values("date")
    check(hs["date"].is_unique, "ASAP hotspots: one assessment per date")
    check(set(hs["hs_code"]) <= {0, 1, 2}, f"ASAP hs_code values {sorted(set(hs['hs_code']))}")
    names = dict(zip(hs["hs_code"], hs["hs_name"]))
    check(names.get(0) == "No hotspot" and names.get(1) == "Hotspot", f"hs_code names {names}")

    w_zip = zipfile.ZipFile(fetch(ASAP + "warnings_ts.zip", "warnings_ts.zip"))
    raw = w_zip.open("warnings_ts.csv").read()
    header = raw.split(b"\n", 1)[0]
    ago = b"\n".join([header] + [ln for ln in raw.split(b"\n") if b';"Angola";' in ln])
    w = pd.read_csv(io.StringIO(decode_lines(ago)), sep=";")
    w["date"] = pd.to_datetime(w["date"])
    w["prov"] = w["asap1_name"]
    cod_by_key = {key(p): p for p in provinces}
    w["prov"] = w["prov"].map(lambda s: cod_by_key.get(key(s)))
    check(w["prov"].notna().all(), "ASAP warning provinces all match COD admin1 names")
    check(w["prov"].nunique() == 18, "ASAP warnings cover all 18 provinces")
    check(w.groupby("prov").size().nunique() == 1, "ASAP warnings: same dekad count per province")

    def group(txt):
        if txt == "No warning":
            return 0
        m = re.fullmatch(r"Warning group (\d)", txt)
        if m:
            return int(m.group(1))
        if txt in ("Off season", "No crop/rangeland"):
            return None
        raise ValueError(txt)

    for lc in ("crop", "range"):
        seen = set(w[f"w_{lc}_gr"])
        check(
            seen <= {"No warning", "Off season", "No crop/rangeland"} | {f"Warning group {i}" for i in range(1, 5)},
            f"ASAP {lc} warning groups recognised: {sorted(seen)}",
        )
        w[f"g_{lc}"] = w[f"w_{lc}_gr"].map(group)
    never = sorted(w.groupby("prov")["w_crop_gr"].apply(lambda s: (s == "No crop/rangeland").all()).pipe(lambda s: s[s].index))
    w["season"] = w["date"].map(lambda d: d.year if d.month >= 10 else d.year - 1)
    w = w[w["season"] >= FIRST_SEASON]

    out = {}
    for lc in ("crop", "range"):
        s = w.dropna(subset=[f"g_{lc}"])
        t = (
            s.groupby(["prov", "season"])[f"g_{lc}"]
            .agg(n="size", w1=lambda x: (x >= 1).mean(), w2=lambda x: (x >= 2).mean(), mx="max")
            .reset_index()
        )
        out[lc] = [[r.prov, int(r.season), int(r.n), round(r.w1, 4), round(r.w2, 4), int(r.mx)] for r in t.itertuples()]
    check(
        set(r[0] for r in out["crop"]) == set(provinces) - set(never),
        f"provinces without warnings are exactly the never-assessed ones: {never}",
    )

    # independent recount of the hotspot series straight from the zip text
    txt = hs_zip.open("hotspots_ts.csv").read().decode("utf-8")
    recount = len(re.findall(r'^2;"Angola";\d{4}-\d\d-\d\d;1;', txt, flags=re.M))
    check(recount == int((hs["hs_code"] == 1).sum()), f"hotspot months recount {recount}")

    return {
        "hs": [[d.strftime("%Y-%m-%d"), int(c)] for d, c in zip(hs["date"], hs["hs_code"])],
        "crop": out["crop"],
        "range": out["range"],
        "never": never,
        "warn_last": w["date"].max().strftime("%Y-%m-%d"),
    }


# ---------------------------------------------------------------- FEWS NET
FEWS_ADMIN1 = {"Bie": "Bié", "Huila": "Huíla", "Uige": "Uíge", "Kuando Kubango": "Cuando Cubango",
               "Kuanza Norte": "Cuanza Norte", "Kuanza Sul": "Cuanza Sul"}


def load_fews(provinces):
    csv = pd.read_csv(fetch(FDW + "ipcphase.csv", "fews_ao.csv", params={"country_code": "AO"}))
    js = pd.DataFrame(requests.get(FDW + "ipcphase/", params={"country_code": "AO", "format": "json"}, timeout=600).json())
    k = ["fnid", "scenario_name", "projection_start", "reporting_date"]
    m = csv[k + ["value"]].merge(js[k + ["value"]], on=k, how="outer", suffixes=("_csv", "_json"), indicator=True)
    check((m["_merge"] == "both").all(), f"FEWS NET CSV and JSON have the same {len(csv)} records")
    check(((m["value_csv"] == m["value_json"]) | (m["value_csv"].isna() & m["value_json"].isna())).all(),
          "FEWS NET CSV and JSON phases identical")
    check((csv["data_usage_policy"] == "Public").all(), "FEWS NET rows all public")
    csv["sc"] = csv["scenario_name"].str.strip()

    # national near-term series: remote-monitoring national unit (2022-24), max over zones (2026)
    near = csv[(csv["sc"] == "Near Term Projection") & csv["unit_type"].isin(["fsc_rm_admin", "fsc_admin_lhz"])]
    rm = near[near["unit_type"] == "fsc_rm_admin"]
    check(rm.groupby("reporting_date").size().max() == 1, "one remote-monitoring national phase per report")
    tl = near.groupby("reporting_date")["value"].max().astype(int)
    srcdoc = near.groupby("reporting_date")["source_document"].first()
    kind = near.groupby("reporting_date")["unit_type"].agg(lambda u: "national" if (u == "fsc_rm_admin").all() else "zones")
    check(set(kind) == {"national", "zones"}, "each FEWS NET report is either national-only or zone-level")
    timeline = [[d, int(v), srcdoc[d], kind[d]] for d, v in tl.items()]

    # zone-level: latest package shapefiles, cross-checked against the CSV record
    lhz = csv[csv["unit_type"] == "fsc_admin_lhz"]
    rounds = sorted(lhz["reporting_date"].unique())
    pk_latest = zipfile.ZipFile(fetch(FDW + "ipcpackage/", "fews_pkg_latest.zip", params={"country_code": "AO"}))
    shp_round = sorted({re.match(r"AO_(\d{6})_", n).group(1) for n in pk_latest.namelist() if n.endswith(".shp")})
    check(len(shp_round) == 1, f"latest FEWS NET package is one round: {shp_round}")
    ml_round = shp_round[0]
    cs_rounds = sorted(lhz[lhz["sc"] == "Current Situation"]["reporting_date"].unique())
    cs_round = cs_rounds[-1]
    pk_cs = zipfile.ZipFile(fetch(FDW + "ipcpackage/", f"fews_pkg_{cs_round[:7]}.zip",
                                  params={"country_code": "AO", "collection_date": cs_round}))

    def read_shp(z, stem):
        d = Path(tempfile.mkdtemp(dir=CACHE))
        z.extractall(d)
        return gpd.read_file(d / f"{stem}.shp")

    cs = read_shp(pk_cs, f"AO_{cs_round[:4]}{cs_round[5:7]}_CS")
    ml1 = read_shp(pk_latest, f"AO_{ml_round}_ML1")
    ml2 = read_shp(pk_latest, f"AO_{ml_round}_ML2")
    ml_date = f"{ml_round[:4]}-{ml_round[4:]}-01"

    def rec(date, sc):
        r = lhz[(lhz["reporting_date"] == date) & (lhz["sc"] == sc)]
        return dict(zip(r["fnid"], r["value"]))

    for g, col, date, sc in ((cs, "CS", cs_round, "Current Situation"),
                             (ml1, "ML1", ml_date, "Near Term Projection"),
                             (ml2, "ML2", ml_date, "Medium Term Projection")):
        r = rec(date, sc)
        check(set(g["fnid"]) == set(r), f"FEWS NET {col} {date}: package units match the record ({len(r)})")
        check(all(int(v) == int(r[f]) for f, v in zip(g["fnid"], g[col])), f"FEWS NET {col} {date}: package phases match the record")

    # every zone-level round from the current-situation round on must carry the package projections
    later = [d for d in rounds if d > ml_date]
    for d in [x for x in rounds if x >= cs_round]:
        for sc, g, col in (("Near Term Projection", ml1, "ML1"), ("Medium Term Projection", ml2, "ML2")):
            r = rec(d, sc)
            same = all(int(r[f]) == int(v) for f, v in zip(g["fnid"], g[col]))
            check(same, f"FEWS NET {d} {sc} phases equal the {ml_date} package {col}")
    docs = lhz.groupby("reporting_date")["source_document"].agg(lambda x: sorted(set(x)))
    check(docs.map(len).eq(1).all(), "one FEWS NET source document per zone-level round")
    docs = [[d, v[0].replace(", Angola", "")] for d, v in docs.items()]

    g = cs[["fnid", "ADMIN1", "LZNAME", "CS", "geometry"]].merge(ml1[["fnid", "ML1"]], on="fnid").merge(ml2[["fnid", "ML2"]], on="fnid")
    g["prov"] = g["ADMIN1"].map(lambda s: FEWS_ADMIN1.get(s, s))
    check(set(g["prov"]) == set(provinces), "FEWS NET zone provinces match all 18 COD provinces")
    cov = {c: f"{x['cov_start'].iloc[0]} to {x['cov_end'].iloc[0]}" for c, x in (("CS", cs), ("ML1", ml1), ("ML2", ml2))}
    return {
        "timeline": timeline,
        "zones": g,
        "cs_round": cs_round,
        "ml_round": ml_date,
        "later_rounds": later,
        "docs": docs,
        "cov": cov,
        "first": min(csv["reporting_date"]),
        "last": max(csv["reporting_date"]),
    }


# ---------------------------------------------------------------- IPC
IPC_ALIAS_2021 = {  # IPC municipality -> COD admin2, where names differ
    "municipiodosgambosexchiange": "gambosexchiange",
    "mocamedes": "namibe",  # COD Namibe province municipalities: Bibala, Camucuio, Namibe, Tombwa, Virei
}
IPC_ALIAS_2019 = {  # IPC commune -> COD admin3: the seat communes COD spells with K
    "quilengues": "kilengue",  # Quilengues municipality: COD communes Dinde, Impulo, Kilengue
    "cuchi": "kuchi",  # Cuchi municipality: COD communes Chinguanja, Kuchi, Kutato
}


def load_ipc(adm):
    api_key = os.environ["IPC_API_KEY"]
    an = requests.get(IPC_API + "analyses", params={"key": api_key, "country": "AO", "format": "json"}, timeout=120).json()
    check(len(an) == 2, f"IPC API lists 2 Angola analyses: {[a['title'] for a in an]}")
    rows = []
    for year in (2019, 2021):
        for per in ("C", "P"):
            areas = requests.get(IPC_API + "areas", params={"key": api_key, "country": "AO", "year": year,
                                                          "type": "A", "period": per, "format": "json"}, timeout=120).json()
            for a in areas:
                ph = {p["phase"]: p for p in a["phases"]}
                rows.append(dict(year=year, per=per, area=a["title"], frm=a["from"], to=a["to"], phase=a["overall_phase"],
                                 pop=a["estimated_population"], p3=a["phase3_worse_population"],
                                 p3pct=a["phase3_worse_percentage"], p4=ph[4]["population"], p4pct=ph[4]["percent"],
                                 p5=ph[5]["population"]))
    d = pd.DataFrame(rows)
    check(set(d.groupby(["year", "per"]).size().items()) == {((2019, "C"), 23), ((2019, "P"), 23), ((2021, "C"), 17), ((2021, "P"), 17)},
          "IPC API: 23 areas in 2019, 17 in 2021, both periods")
    check((d["p5"] == 0).all(), "IPC: no population in Phase 5 in any area")

    # map areas to COD: 2019 areas are communes (admin3), 2021 areas are municipalities (admin2)
    south = {"Cunene", "Huíla", "Namibe", "Cuando Cubango"}
    a2 = adm[2][adm[2]["adm1_name"].isin(south)]
    a3 = adm[3][adm[3]["adm1_name"].isin(south)]
    k2 = {key(n): (n, p, pc) for n, p, pc in zip(a2["adm2_name"], a2["adm1_name"], a2["adm2_pcode"])}
    check(len(k2) == len(a2), "COD southern municipality names unique after normalising")
    k3 = {}
    for n, m, p, pc in zip(a3["adm3_name"], a3["adm2_name"], a3["adm1_name"], a3["adm3_pcode"]):
        k3.setdefault(key(n), []).append((n, m, p, pc))

    # HDX IPC file: the 2019 round lists each commune's parent municipality ("Level 1")
    h = pd.read_csv(fetch(HDX_IPC_AREA, "ipc_ago_area_long.csv"), skiprows=[1], encoding="utf-8", encoding_errors="replace")
    parent19 = {key(a): key(l1) for a, l1, dt in zip(h["Area"], h["Level 1"], h["Date of analysis"]) if dt.endswith("2019")}

    def locate(row):
        k = key(row.area)
        if row.year == 2021:
            n, p, pc = k2[IPC_ALIAS_2021.get(k, k)]
            return pd.Series(dict(cod=n, muni=n, prov=p, pcode=pc))
        cands = [c for c in k3.get(IPC_ALIAS_2019.get(k, k), []) if key(c[1]).startswith(parent19[k][:6])]
        if len(cands) != 1:
            raise AssertionError(f"IPC 2019 commune {row.area}: {len(cands)} COD matches under {parent19[k]}")
        n, m, p, pc = cands[0]
        return pd.Series(dict(cod=n, muni=m, prov=p, pcode=pc))

    d = pd.concat([d, d.apply(locate, axis=1)], axis=1)
    d["name"] = d["area"].str.replace("�­", "í", regex=False)
    check(not d["name"].str.contains("�").any(), "IPC area names free of broken characters")
    check(d["pcode"].notna().all(), "every IPC area matched to exactly one COD unit")

    # cross-check with the HDX IPC file (same consensus numbers, different publisher)
    h = h[h["Phase"] == "3+"].copy()
    h["year"] = h["Date of analysis"].str[-4:].astype(int)
    h["per"] = h["Validity period"].map({"current": "C", "first projection": "P"})
    h["k"] = h["Area"].map(key)
    d["k"] = d["area"].map(key)
    j = d.merge(h[["year", "per", "k", "Number"]], on=["year", "per", "k"], how="left")
    missing = j[j["Number"].isna()][["year", "per", "area"]].values.tolist()
    diff = j[j["Number"].notna() & (j["Number"] != j["p3"])][["year", "per", "area", "p3", "Number"]].values.tolist()
    check(not diff, f"IPC API and HDX Phase 3+ numbers agree where both exist (mismatches: {diff})")
    CHECKS.append(("NOTE", f"IPC areas missing from HDX area file: {missing}"))

    # province figures as published (HDX level-1 file). In 2021 level 1 is the province; in 2019 it is the
    # municipality, so 2019 province figures are sums of the published municipality figures.
    l1 = pd.read_csv(fetch(HDX_IPC_L1, "ipc_ago_level1_long.csv"), skiprows=[1], encoding="utf-8", encoding_errors="replace")
    l1["year"] = l1["Date of analysis"].str[-4:].astype(int)
    l1["per"] = l1["Validity period"].map({"current": "C", "first projection": "P"})
    k2all = {key(n): p for n, p in zip(a2["adm2_name"], a2["adm1_name"])}
    kprov = {key(p): p for p in south}
    prov_rows = []
    for (y, per, lv), g in l1.groupby(["year", "per", "Level 1"]):
        v = dict(zip(g["Phase"], g["Number"]))
        pc = dict(zip(g["Phase"], g["Percentage"]))
        pv = kprov.get(key(lv)) if y == 2021 else k2all.get(key(lv))
        check(pv is not None, f"IPC level-1 unit {lv} ({y}) placed in a province")
        prov_rows.append(dict(year=y, per=per, lvl=lv, prov=pv, p3=v["3+"], pop=v.get("all"), pct=pc["3+"]))
    l1d = pd.DataFrame(prov_rows)
    # published level-1 figures vs the area figures they aggregate
    for (y, per), g in l1d.groupby(["year", "per"]):
        a = d[(d["year"] == y) & (d["per"] == per)]
        grp = a.groupby("prov" if y == 2021 else "muni")["p3"].sum()
        for r in g.itertuples():
            name = r.prov if y == 2021 else [m for m in grp.index if key(m).startswith(key(r.lvl)[:6])][0]
            check(abs(grp[name] - r.p3) <= 10, f"IPC {y}{per} {r.lvl}: published Phase 3+ {int(r.p3):,} vs area sum {int(grp[name]):,}")
    prov = []
    for (y, per, pv), g in l1d.groupby(["year", "per", "prov"]):
        if y == 2021:
            r = g.iloc[0]
            prov.append(dict(year=y, per=per, prov=pv, p3=int(r.p3), pop=None if pd.isna(r["pop"]) else int(r["pop"]),
                             share=float(r.pct), basis="published province figure"))
        else:
            prov.append(dict(year=y, per=per, prov=pv, p3=int(g.p3.sum()), pop=int(g["pop"].sum()),
                             share=float(g.p3.sum() / g["pop"].sum()), basis=f"sum of published figures for {', '.join(sorted(g.lvl))}"))
    prov = pd.DataFrame(prov)
    periods = d.groupby(["year", "per"])[["frm", "to"]].first().to_dict("index")
    sums = d.groupby(["year", "per"])[["p3", "p4", "pop"]].sum()
    # published national totals (HDX national file), cross-checked against the IPC API population endpoint
    hn = pd.read_csv(fetch(HDX_IPC_NAT, "ipc_ago_national_long.csv"), skiprows=[1])
    hn["year"] = hn["Date of analysis"].str[-4:].astype(int)
    hn["per"] = hn["Validity period"].map({"current": "C", "first projection": "P"})
    nat = {}
    for (y, per), g in hn.groupby(["year", "per"]):
        v = dict(zip(g["Phase"], g["Number"]))
        nat[(y, per)] = {"p3": int(v["3+"]), "p4": int(v["4"])}
        gap = abs(nat[(y, per)]["p3"] - sums.loc[(y, per), "p3"])
        check(gap <= 10, f"IPC {y}{per}: area Phase 3+ sum {int(sums.loc[(y, per), 'p3']):,} vs published {nat[(y, per)]['p3']:,} (rounding gap {gap})")
        nat[(y, per)]["pop"] = int(sums.loc[(y, per), "pop"])
    pop21 = requests.get(IPC_API + "population", params={"key": api_key, "country": "AO", "format": "json"}, timeout=120).json()
    p21 = [a for a in pop21 if a["analysis_date"] == "Jun 2021"][0]
    check(p21["p3plus"] == nat[(2021, "C")]["p3"] and p21["p3plus_projected"] == nat[(2021, "P")]["p3"],
          "IPC API national Phase 3+ (Jun 2021) equals the HDX national file")
    return d, prov, periods, nat, an


# ---------------------------------------------------------------- rainfall and NDVI (Oct-Mar), WFP on HDX
WFP_RAIN = (
    "https://data.humdata.org/dataset/84dbf78f-3d9c-43d4-8a69-64debbd9e582/resource/"
    "fac00c49-12ca-4a13-9bed-75e829c879ea/download/ago-rainfall-subnat-full.csv"
)
WFP_NDVI = (
    "https://data.humdata.org/dataset/3ca138a8-0832-47be-8908-5f9a95c330d9/resource/"
    "2284c6e2-d80c-49ab-9931-b8a04236df46/download/ago-ndvi-subnat-full.csv"
)
ONDJFM = (10, 11, 12, 1, 2, 3)  # 18 dekads, dated by their start day


def load_wfp(adm):
    """Oct-Mar rainfall (CHIRPS, WFP) and NDVI (MODIS, WFP) per province, plus an Angola pixel-weighted mean."""
    names = dict(zip(adm[1]["adm1_pcode"], adm[1]["adm1_name"]))
    area = dict(zip(adm[1]["adm1_pcode"], adm[1]["area_sqkm"]))
    out, meta = [], {}
    for var, url, fname, val, avg, base in (
        ("rain", WFP_RAIN, "wfp_ago_rain.csv", "rfh", "rfh_avg", ("1989-01-01", "2018-12-31")),
        ("ndvi", WFP_NDVI, "wfp_ago_ndvi.csv", "vim", "vim_avg", ("2002-07-01", "2018-07-01")),
    ):
        d = pd.read_csv(fetch(url, fname), parse_dates=["date"])  # no HXL row in these files
        d = d[d["adm_level"] == 1].copy()
        check(set(d["PCODE"]) == set(names), f"WFP {var}: the 18 province PCODEs match COD admin1 PCODEs")
        check(d.groupby("PCODE")["n_pixels"].nunique().eq(1).all(), f"WFP {var}: one pixel count per province")
        d["md"] = d["date"].dt.strftime("%m-%d")
        # the long-term average column is the per-dekad mean over the documented reference period (inclusive)
        lta = d[d["date"].between(*base)].groupby(["PCODE", "md"])[val].mean()
        got = d.groupby(["PCODE", "md"])[avg].agg(["min", "max"])
        check((got["max"] - got["min"]).abs().max() == 0, f"WFP {var}: {avg} constant per province and dekad")
        gap = (got["min"] - lta.reindex(got.index)).abs().max()
        check(gap < 1e-3 * max(1, lta.abs().max()), f"WFP {var}: {avg} equals the {base[0]} to {base[1]} dekadal mean (max diff {gap:.2g})")
        d = d[d["date"].dt.month.isin(ONDJFM)].copy()
        d["season"] = d["date"].map(lambda t: t.year if t.month >= 10 else t.year - 1)
        full = d.groupby(["PCODE", "season"]).size()
        seasons = sorted(s for s in full.index.get_level_values(1).unique() if (full.xs(s, level=1) == 18).all())
        check(seasons == list(range(seasons[0], seasons[-1] + 1)), f"WFP {var}: complete Oct-Mar seasons {seasons[0]}/{seasons[0] + 1} to {seasons[-1]}/{seasons[-1] + 1}, none missing")
        d = d[d["season"].isin(seasons)]
        if "version" in d:
            check((d["version"] == "final").all(), f"WFP {var}: every Oct-Mar dekad used is final (not prelim or forecast)")
        agg = "sum" if var == "rain" else "mean"
        s = d.groupby(["PCODE", "season"]).agg(v=(val, agg), n=(avg, agg), px=("n_pixels", "first")).reset_index()
        nat = s.assign(wv=s.v * s.px, wn=s.n * s.px).groupby("season").agg(wv=("wv", "sum"), wn=("wn", "sum"), px=("px", "sum")).reset_index()
        nat = nat.assign(PCODE="AO", v=nat.wv / nat.px, n=nat.wn / nat.px)[["PCODE", "season", "v", "n", "px"]]
        s = pd.concat([s, nat], ignore_index=True)
        s["area"] = s["PCODE"].map(lambda p: names.get(p, "Angola"))
        s["pct"] = s["v"] / s["n"]
        s["rank"] = s.groupby("PCODE")["v"].rank(method="min").astype(int)  # 1 = lowest
        s["var"] = var
        out.append(s)
        px = d.groupby("PCODE")["n_pixels"].first()
        km2 = {p: area[p] / px[p] for p in px.index}
        meta[var] = {"first": seasons[0], "last": seasons[-1], "base": base, "n": len(seasons),
                     "km2_per_px": {names[p]: round(v, 1) for p, v in km2.items()}}
    # WFP's own units vs COD provinces: area per CHIRPS pixel should be about constant if the units are the same
    k = meta["rain"]["km2_per_px"]
    typical = float(pd.Series(k).median())
    odd = sorted(p for p, v in k.items() if abs(v / typical - 1) > 0.15)
    CHECKS.append(("NOTE", f"WFP province units whose pixel count does not fit the COD area (km2/pixel, median {typical:.1f}): "
                           + ", ".join(f"{p} {k[p]}" for p in odd)))
    both = sum(adm[1].set_index("adm1_name").loc[odd, "area_sqkm"]) / sum(
        int(round(adm[1].set_index("adm1_name").loc[p, "area_sqkm"] / k[p])) for p in odd)
    check(abs(both / typical - 1) < 0.1, f"{' + '.join(odd)} together fit the COD area ({both:.1f} km2/pixel)")
    meta["odd_units"] = odd
    return pd.concat(out, ignore_index=True), meta


# ---------------------------------------------------------------- map paths
class Proj:
    def __init__(self, bounds, width=560):
        x0, y0, x1, y1 = bounds
        import math
        self.k = math.cos(math.radians((y0 + y1) / 2))
        self.x0, self.y1 = x0, y1
        self.s = width / ((x1 - x0) * self.k)
        self.w = width
        self.h = (y1 - y0) * self.s

    def path(self, geom, tol):
        geom = geom.simplify(tol, preserve_topology=True)
        polys = [geom] if geom.geom_type == "Polygon" else list(geom.geoms)
        out = []
        for p in polys:
            for ring in [p.exterior, *p.interiors]:
                pts = [f"{(x - self.x0) * self.k * self.s:.1f} {(self.y1 - y) * self.s:.1f}" for x, y in ring.coords]
                if len(pts) > 3:
                    out.append("M" + "L".join(pts) + "Z")
        return "".join(out)


def build():
    adm = load_cod()
    provinces = sorted(adm[1]["adm1_name"])
    asap = load_asap(provinces)
    fews = load_fews(provinces)
    ipc, ipc_prov, ipc_periods, ipc_nat, ipc_an = load_ipc(adm)
    wfp, wfp_meta = load_wfp(adm)

    proj = Proj(adm[0].total_bounds)
    maps = {
        "w": proj.w, "h": round(proj.h, 1),
        "outline": proj.path(adm[0].geometry.iloc[0], 0.01),
        "adm1": [[r.adm1_name, proj.path(r.geometry, 0.01)] for r in adm[1].itertuples()],
        "fews": [[r.fnid, r.prov, r.LZNAME, int(r.CS), int(r.ML1), int(r.ML2), proj.path(r.geometry, 0.01)]
                 for r in fews["zones"].itertuples()],
    }
    lab = adm[1][["adm1_name", "center_lon", "center_lat"]]
    maps["labels"] = [[n, round((x - proj.x0) * proj.k * proj.s, 1), round((proj.y1 - y) * proj.s, 1)]
                      for n, x, y in lab.itertuples(index=False)]
    a2 = adm[2].set_index("adm2_pcode")
    a3 = adm[3].set_index("adm3_pcode")
    ipc_rows = []
    for r in ipc.itertuples():
        geom = (a3 if r.year == 2019 else a2).loc[r.pcode].geometry
        ipc_rows.append([r.year, r.per, r.name, r.muni, r.prov, int(r.phase), int(r.pop), int(r.p3), r.p3pct,
                         int(r.p4), r.p4pct, proj.path(geom, 0.004)])

    # summary facts derived from the data (the page text uses these, nothing hardcoded)
    z = fews["zones"]
    crisis = z[z["ML2"] >= 3].groupby("LZNAME")["prov"].apply(lambda s: sorted(set(s))).to_dict()
    crisis_cs = sorted(set(z[z["ML2"] >= 3]["CS"]))
    crisis_ph = sorted(set(z[z["ML2"] >= 3]["ML2"]))
    hs = asap["hs"]
    runs, cur = [], None
    for d, c in hs:
        if c >= 1:
            if cur is None:
                cur = [d, d, 0]
                runs.append(cur)
            cur[1], cur[2] = d, cur[2] + 1
        else:
            cur = None
    check(len(runs) >= 1, "at least one hotspot spell")

    data = {
        "asap": asap, "fews_tl": fews["timeline"], "maps": maps, "ipc": ipc_rows,
        "ipc_prov": ipc_prov.to_dict("records"),
        "ipc_periods": {f"{y}{p}": v for (y, p), v in ipc_periods.items()},
        "ipc_nat": {f"{y}{p}": v for (y, p), v in ipc_nat.items()},
        "ipc_titles": sorted(a["title"] for a in ipc_an),
        "wfp": [[r.var, r.area, int(r.season), round(float(r.v), 4 if r.var == "ndvi" else 1), round(float(r.n), 4 if r.var == "ndvi" else 1),
                 round(float(r.pct), 4), int(r.rank)] for r in wfp.itertuples()],
        "wfp_meta": wfp_meta,
        "facts": {
            "crisis_zones": crisis, "crisis_zones_cs": [int(x) for x in crisis_cs], "crisis_ph": [int(x) for x in crisis_ph],
            "fews_cs_round": fews["cs_round"], "fews_ml_round": fews["ml_round"],
            "fews_later": fews["later_rounds"], "fews_docs": fews["docs"], "fews_cov": fews["cov"],
            "fews_first": fews["first"], "fews_last": fews["last"],
            "hs_runs": runs, "recent": RECENT,
        },
    }
    html = (HERE / "ago_hotspots_template.html").read_text(encoding="utf-8")
    js = json.dumps(data, ensure_ascii=False, separators=(",", ":"), default=float)
    (HERE / "ago_hotspots.html").write_text(html.replace("/*DATA*/", "const D=" + js + ";"), encoding="utf-8")

    for status, msg in CHECKS:
        print(f"[{status}] {msg}")
    print(f"wrote {HERE / 'ago_hotspots.html'} ({len(js) / 1e3:.0f} kB data)")


if __name__ == "__main__":
    build()

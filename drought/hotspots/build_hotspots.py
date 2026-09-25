"""Build a country drought hotspots page: drought/<iso3>/<iso3>_hotspots.html.

Pulls every source fresh, cross-checks them, and fills template.html.

Sources
- JRC ASAP national hotspots (hotspots_ts.zip) and admin-1 warnings (warnings_ts.zip)
- FEWS NET Data Warehouse: ipcphase.csv, ipcphase JSON (cross-check), ipcpackage shapefiles
- IPC API (analyses, areas, population; needs IPC_API_KEY) and the HDX IPC country files (cross-check)
- WFP on HDX: dekadal CHIRPS rainfall and MODIS NDVI by admin 1
- HDX COD-AB boundaries (maps, area-to-province lookup)

Run from the repo root:
    uv run --with pandas --with geopandas --with requests python drought/hotspots/build_hotspots.py AGO ZMB
"""

import io
import json
import math
import os
import re
import sys
import tempfile
import unicodedata
import zipfile
from pathlib import Path

import geopandas as gpd
import pandas as pd
import requests
from shapely.ops import unary_union

HERE = Path(__file__).parent
CACHE = Path(os.environ.get("HOTSPOTS_CACHE", Path(tempfile.gettempdir()) / "hotspots_cache"))
CACHE.mkdir(parents=True, exist_ok=True)

ASAP = "https://agricultural-production-hotspots.ec.europa.eu/files/"
FDW = "https://fdw.fews.net/api/"
IPC_API = "https://api.ipcinfo.org/"
HDX = "https://data.humdata.org/api/3/action/package_show"
ASAP_FIRST_SEASON = 2001  # warnings start 2001-05; season 2001 = Oct 2001 to Sep 2002
ONDJFM = (10, 11, 12, 1, 2, 3)

# Per-country settings. Aliases map a normalised source name to a normalised COD name (see key()),
# and each one was checked by hand against the COD list for that level.
COUNTRIES = {
    "AGO": {
        "name": "Angola", "iso2": "AO", "asap": "Angola",
        "admin1_alias": {"kuandokubango": "cuandocubango", "kuanzanorte": "cuanzanorte", "kuanzasul": "cuanzasul"},
        "ipc_adm2_alias": {
            "municipiodosgambosexchiange": "gambosexchiange",
            "mocamedes": "namibe",  # COD Namibe province municipalities: Bibala, Camucuio, Namibe, Tombwa, Virei
        },
        "ipc_adm3_alias": {
            "quilengues": "kilengue",  # Quilengues municipality: COD communes Dinde, Impulo, Kilengue
            "cuchi": "kuchi",  # Cuchi municipality: COD communes Chinguanja, Kuchi, Kutato
        },
        "map_labels": ["Namibe", "Huíla", "Cunene", "Cuando Cubango", "Benguela", "Huambo", "Bié", "Luanda", "Malanje", "Moxico"],
    },
    "ZMB": {
        "name": "Zambia", "iso2": "ZM", "asap": "Zambia",
        "admin1_alias": {"muchiga": "muchinga", "machinga": "muchinga"},
        "ipc_adm2_alias": {
            "chikankanta": "chikankata", "milengi": "milenge", "chiengi": "chienge",
            "luntedistrict": "lunte", "mushindano": "mushindamo",
            "kalabosikongo": ["kalabo", "sikongo"],  # one IPC area covering two COD districts (Jun 2022)
        },
        "ipc_adm3_alias": {},
        "map_labels": None,  # label every province
    },
}

CHECKS = []


def check(cond, msg):
    """Record a verification; stop the build if it fails."""
    CHECKS.append(("PASS" if cond else "FAIL", msg))
    if not cond:
        raise AssertionError(msg)


def note(msg):
    CHECKS.append(("NOTE", msg))


def fetch(url, name, params=None):
    """Download to the cache; set HOTSPOTS_USE_CACHE=1 to reuse earlier downloads."""
    path = CACHE / name
    if os.environ.get("HOTSPOTS_USE_CACHE") and path.exists():
        return path
    r = requests.get(url, params=params, timeout=900)
    r.raise_for_status()
    path.write_bytes(r.content)
    return path


def get_json(url, name, params=None):
    return json.loads(fetch(url, name, params).read_text(encoding="utf-8"))


def key(s):
    """Accent/case-insensitive name key; tolerates the IPC API's broken 'í' and a trailing 'Province'."""
    s = str(s).replace("�\xad", "i").replace("�", "")
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    s = re.sub(r"[^a-z]", "", s.lower())
    return s[: -len("province")] if s.endswith("province") and len(s) > len("province") else s


def fix_name(s):
    return str(s).replace("�\xad", "í")


def decode_lines(raw):
    """ASAP warnings mix UTF-8 and Latin-1 rows; decode each line on its own."""
    out = []
    for line in raw.split(b"\n"):
        try:
            out.append(line.decode("utf-8"))
        except UnicodeDecodeError:
            out.append(line.decode("latin-1"))
    return "\n".join(out)


def hdx_resource(dataset, suffix):
    meta = get_json(HDX, f"hdx_{dataset}.json", {"id": dataset})
    check(meta.get("success"), f"HDX dataset {dataset} found")
    urls = [r["url"] for r in meta["result"]["resources"] if r["name"].endswith(suffix)]
    check(len(urls) == 1, f"HDX {dataset}: one resource ending {suffix}")
    return urls[0]


def month_start(label):
    """'Jul 2024' -> '2024-07-01'."""
    return pd.to_datetime(label, format="%b %Y").strftime("%Y-%m-%d")


class Country:
    def __init__(self, iso3):
        self.iso3 = iso3
        self.c = COUNTRIES[iso3]
        self.name = self.c["name"]
        self.low = iso3.lower()

    def prov_of(self, s):
        k = key(s)
        k = self.c["admin1_alias"].get(k, k)
        return self.adm1_by_key.get(k)

    # ------------------------------------------------------------ boundaries
    def load_cod(self):
        z = zipfile.ZipFile(fetch(hdx_resource(f"cod-ab-{self.low}", "geojson.zip"), f"{self.low}_cod.zip"))
        self.adm = {}
        for lvl in (0, 1, 2, 3):
            with z.open(f"{self.low}_admin{lvl}.geojson") as f:
                self.adm[lvl] = gpd.read_file(f)
        self.provinces = sorted(self.adm[1]["adm1_name"])
        self.adm1_by_key = {key(p): p for p in self.provinces}
        check(len(self.adm1_by_key) == len(self.provinces), f"{self.name} COD: {len(self.provinces)} province names distinct after normalising")

    # ------------------------------------------------------------ ASAP
    def load_asap(self):
        hs_zip = zipfile.ZipFile(fetch(ASAP + "hotspots_ts.zip", "hotspots_ts.zip"))
        hs = pd.read_csv(hs_zip.open("hotspots_ts.csv"), sep=";")
        hs = hs[hs["asap0_name"] == self.c["asap"]].copy()
        hs["date"] = pd.to_datetime(hs["date"])
        hs = hs.sort_values("date")
        check(len(hs) > 0 and hs["date"].is_unique, f"ASAP {self.name}: {len(hs)} hotspot assessments, one per date")
        check(set(hs["hs_code"]) <= {0, 1, 2}, f"ASAP {self.name}: hs_code values {sorted(set(hs['hs_code']))}")
        names = dict(zip(hs["hs_code"], hs["hs_name"]))
        known = {0: "No hotspot", 1: "Hotspot", 2: "Major hotspot"}
        check(all(known[k] == v for k, v in names.items()), f"ASAP {self.name}: hs_code names {names}")
        txt = hs_zip.open("hotspots_ts.csv").read().decode("utf-8")
        for code in (1, 2):
            n = len(re.findall(rf'^\d+;"{self.c["asap"]}";\d{{4}}-\d\d-\d\d;{code};', txt, flags=re.M))
            check(n == int((hs["hs_code"] == code).sum()), f"ASAP {self.name}: code-{code} months recounted from the raw text ({n})")

        w_zip = zipfile.ZipFile(fetch(ASAP + "warnings_ts.zip", "warnings_ts.zip"))
        raw = w_zip.open("warnings_ts.csv").read()
        header = raw.split(b"\n", 1)[0]
        tag = f';"{self.c["asap"]}";'.encode()
        w = pd.read_csv(io.StringIO(decode_lines(b"\n".join([header] + [ln for ln in raw.split(b"\n") if tag in ln]))), sep=";")
        w["date"] = pd.to_datetime(w["date"])
        w["prov"] = w["asap1_name"].map(self.prov_of)
        check(w["prov"].notna().all(), f"ASAP {self.name}: warning units all match COD provinces ({sorted(set(w['asap1_name']))})")
        check(set(w["prov"]) == set(self.provinces), f"ASAP {self.name}: warnings cover all {len(self.provinces)} provinces")
        check(w.groupby("prov").size().nunique() == 1, f"ASAP {self.name}: same dekad count per province")

        def group(t):
            if t == "No warning":
                return 0
            m = re.fullmatch(r"Warning group (\d)", t)
            if m:
                return int(m.group(1))
            if t in ("Off season", "No crop/rangeland"):
                return None
            raise ValueError(t)

        for lc in ("crop", "range"):
            seen = set(w[f"w_{lc}_gr"])
            check(seen <= {"No warning", "Off season", "No crop/rangeland"} | {f"Warning group {i}" for i in range(1, 5)},
                  f"ASAP {self.name} {lc}: warning groups recognised")
            w[f"g_{lc}"] = w[f"w_{lc}_gr"].map(group)
        never = sorted(p for p, g in w.groupby("prov") if (g["w_crop_gr"] == "No crop/rangeland").all())
        w["season"] = w["date"].map(lambda d: d.year if d.month >= 10 else d.year - 1)
        w = w[w["season"] >= ASAP_FIRST_SEASON]
        out = {}
        for lc in ("crop", "range"):
            s = w.dropna(subset=[f"g_{lc}"])
            t = s.groupby(["prov", "season"])[f"g_{lc}"].agg(n="size", w1=lambda x: (x >= 1).mean(), w2=lambda x: (x >= 2).mean(), mx="max").reset_index()
            out[lc] = [[r.prov, int(r.season), int(r.n), round(r.w1, 4), round(r.w2, 4), int(r.mx)] for r in t.itertuples()]
        check({r[0] for r in out["crop"]} == set(self.provinces) - set(never), f"ASAP {self.name}: provinces without warnings are the never-assessed ones {never}")
        runs, cur = [], None
        for d, code in zip(hs["date"], hs["hs_code"]):
            if code >= 1:
                if cur is None:
                    cur = [d.strftime("%Y-%m-%d"), None, 0]
                    runs.append(cur)
                cur[1], cur[2] = d.strftime("%Y-%m-%d"), cur[2] + 1
            else:
                cur = None
        self.asap = {"hs": [[d.strftime("%Y-%m-%d"), int(c)] for d, c in zip(hs["date"], hs["hs_code"])],
                     "crop": out["crop"], "range": out["range"], "never": never, "runs": runs,
                     "warn_last": w["date"].max().strftime("%Y-%m-%d")}

    # ------------------------------------------------------------ FEWS NET
    def load_fews(self):
        iso2 = self.c["iso2"]
        csv = pd.read_csv(fetch(FDW + "ipcphase.csv", f"fews_{iso2}.csv", {"country_code": iso2}))
        rows, url, params = [], FDW + "ipcphase/", {"country_code": iso2, "format": "json"}
        while url:
            r = requests.get(url, params=params, timeout=900).json()
            if isinstance(r, list):
                rows += r
                url = None
            else:
                rows += r["results"]
                url, params = r.get("next"), None
        js = pd.DataFrame(rows)
        k = ["fnid", "scenario_name", "projection_start", "reporting_date"]
        m = csv[k + ["value"]].merge(js[k + ["value"]], on=k, how="outer", suffixes=("_csv", "_json"), indicator=True)
        check((m["_merge"] == "both").all(), f"FEWS NET {self.name}: CSV and JSON have the same {len(csv)} records")
        check(((m["value_csv"] == m["value_json"]) | (m["value_csv"].isna() & m["value_json"].isna())).all(), f"FEWS NET {self.name}: CSV and JSON phases identical")
        check((csv["data_usage_policy"] == "Public").all(), f"FEWS NET {self.name}: all rows public")
        csv["sc"] = csv["scenario_name"].str.strip()
        csv["doc"] = csv["source_document"]
        sub = csv[csv["unit_type"].isin(["fsc_admin", "fsc_admin_lhz"])].copy()
        sub["prov"] = sub["geographic_unit_full_name"].str.split(", ").str[-2].map(self.prov_of)
        check(sub["prov"].notna().all(), f"FEWS NET {self.name}: every subnational unit placed in a COD province")
        nulls = int(csv["value"].isna().sum())
        if nulls:
            note(f"FEWS NET {self.name}: {nulls} rows without a phase ({sorted(set(csv[csv['value'].isna()]['status']))}), left out")

        # near-term phase by report month, for the country and each province
        near = csv[(csv["sc"] == "Near Term Projection") & csv["value"].notna()]
        monthly = []
        for d, g in near.groupby("reporting_date"):
            gs = g[g["unit_type"].isin(["fsc_admin", "fsc_admin_lhz"])]
            if len(gs):
                kind = "zones" if (gs["unit_type"] == "fsc_admin_lhz").all() else "districts"
                gs = gs.assign(prov=gs["geographic_unit_full_name"].str.split(", ").str[-2].map(self.prov_of))
                docs = sorted(set(gs["doc"]))
                check(len(docs) == 1, f"FEWS NET {self.name} {d}: one source document ({docs})")
                monthly.append([d, "_national", int(gs["value"].max()), docs[0], kind])
                for p, gp in gs.groupby("prov"):
                    monthly.append([d, p, int(gp["value"].max()), docs[0], kind])
            else:
                gr = g[g["unit_type"] == "fsc_rm_admin"]
                if len(gr):
                    check(len(gr) == 1, f"FEWS NET {self.name} {d}: one remote-monitoring national phase")
                    monthly.append([d, "_national", int(gr["value"].iloc[0]), gr["doc"].iloc[0], "national"])
        # zone map: geometry from the latest package, phases from the latest zone-level rounds in the record
        lhz = sub[sub["unit_type"] == "fsc_admin_lhz"]
        check(len(lhz) > 0, f"FEWS NET {self.name}: livelihood-zone classifications exist")
        pk = zipfile.ZipFile(fetch(FDW + "ipcpackage/", f"fews_pkg_{iso2}.zip", {"country_code": iso2}))
        stems = sorted({n[:-4] for n in pk.namelist() if n.endswith(".shp")})
        rnd = sorted({re.match(rf"{iso2}_(\d{{6}})_", s).group(1) for s in stems})
        check(len(rnd) == 1, f"FEWS NET {self.name}: latest package is one round {rnd}")
        d = Path(tempfile.mkdtemp(dir=CACHE))
        pk.extractall(d)
        geo = None
        pdate = f"{rnd[0][:4]}-{rnd[0][4:]}-01"
        for stem in stems:
            col = stem.split("_")[-1]
            g = gpd.read_file(d / f"{stem}.shp")
            sc = {"CS": "Current Situation", "ML1": "Near Term Projection", "ML2": "Medium Term Projection"}[col]
            r = lhz[(lhz["reporting_date"] == pdate) & (lhz["sc"] == sc)]
            rec = dict(zip(r["fnid"], r["value"]))
            check(set(g["fnid"]) == set(rec), f"FEWS NET {self.name} {col} {pdate}: package units match the record ({len(rec)})")
            check(all(int(v) == int(rec[f]) for f, v in zip(g["fnid"], g[col])), f"FEWS NET {self.name} {col} {pdate}: package phases match the record")
            if geo is None:
                geo = g[["fnid", "ADMIN1", "LZNAME", "geometry"]]
        geo = geo.assign(prov=geo["ADMIN1"].map(self.prov_of))
        check(geo["prov"].notna().all(), f"FEWS NET {self.name}: package zones placed in COD provinces")
        zones = {}
        for sc, col in (("Current Situation", "CS"), ("Near Term Projection", "ML1"), ("Medium Term Projection", "ML2")):
            r = lhz[lhz["sc"] == sc]
            last = r["reporting_date"].max()
            r = r[r["reporting_date"] == last]
            check(set(r["fnid"]) == set(geo["fnid"]), f"FEWS NET {self.name} {col}: latest round {last} covers the package zones")
            zones[col] = {"round": last, "doc": sorted(set(r["doc"]))[0].rsplit(", ", 1)[0],
                          "from": r["projection_start"].min(), "to": r["projection_end"].max(),
                          "phase": dict(zip(r["fnid"], r["value"].astype(int)))}
        self.fews = {"monthly": monthly, "geo": geo, "zones": zones,
                     "first": csv["reporting_date"].min(), "last": csv["reporting_date"].max()}

    # ------------------------------------------------------------ IPC
    def load_ipc(self):
        iso2, api_key = self.c["iso2"], os.environ["IPC_API_KEY"]
        an = requests.get(IPC_API + "analyses", params={"key": api_key, "country": iso2, "format": "json"}, timeout=120).json()
        an = [a for a in an if a.get("condition", "A") == "A"]
        pop = requests.get(IPC_API + "population", params={"key": api_key, "country": iso2, "start": 2000, "end": 2100, "format": "json"}, timeout=120).json()
        pop = {a["id"]: a for a in pop}
        check(set(pop) == {a["id"] for a in an}, f"IPC {self.name}: population figures for all {len(an)} analyses")
        years = sorted({int(a["analysis_date"][-4:]) for a in pop.values()} | {int(a["analysis_date"][-4:]) + 1 for a in pop.values()})
        rows = []
        for y in years:
            for per in ("C", "P"):
                for a in requests.get(IPC_API + "areas", params={"key": api_key, "country": iso2, "year": y, "type": "A",
                                                                "period": per, "format": "json"}, timeout=120).json():
                    if a["anl_id"] not in pop:
                        continue
                    ph = {p["phase"]: p for p in a["phases"]}
                    rows.append(dict(anl=a["anl_id"], per=a["ipc_period"], area=a["title"], frm=a["from"], to=a["to"],
                                     phase=a["overall_phase"], pop=a["estimated_population"], p3=a["phase3_worse_population"],
                                     p3pct=a["phase3_worse_percentage"], p4=ph[4]["population"], p4pct=ph[4]["percent"],
                                     p5=ph[5]["population"], aid=a["id"]))
        d = pd.DataFrame(rows).drop_duplicates(["anl", "per", "aid"])
        check(set(d["anl"]) == set(pop), f"IPC {self.name}: areas returned for every analysis")
        check(d.groupby(["anl", "per"])["area"].apply(lambda s: s.map(key).is_unique).all(), f"IPC {self.name}: area names unique within each analysis and period")
        # overall phase 0 = no area classification. Areas with no population figures either are placeholders: drop them.
        # Areas with population figures but no area phase (e.g. Lusaka 2020) stay in the totals as "not classified".
        empty = (d["phase"] == 0) & (d["p3"].fillna(0) == 0) & (d["p4"].fillna(0) == 0)
        if empty.any():
            note(f"IPC {self.name}: {int(empty.sum())} area-periods listed without any figures, left out: "
                 + "; ".join(f"{pop[a]['analysis_date']} {per}: {n}" for (a, per), n in d[empty].groupby(["anl", "per"]).size().items()))
        d = d[~empty].copy()
        nc = d[d["phase"] == 0]
        if len(nc):
            note(f"IPC {self.name}: areas with figures but no area phase, left out of every total (as IPC's published totals do) and mapped as not classified: "
                 + "; ".join(f"{pop[r.anl]['analysis_date']} {r.per} {r.area}" for r in nc.itertuples()))
        check(d["phase"].between(0, 5).all() and d["p3"].notna().all(), f"IPC {self.name}: every area kept has Phase 3+ population")

        # HDX files (same consensus figures, second publisher); rounds matched by period start month
        def hdx(level):
            url = hdx_resource(f"{self.name.lower()}-acute-food-insecurity-country-data", f"_{level}_long.csv")
            h = pd.read_csv(fetch(url, f"ipc_{self.low}_{level}_long.csv"), skiprows=[1], encoding="utf-8", encoding_errors="replace")
            h["per"] = h["Validity period"].map({"current": "C", "first projection": "P", "second projection": "P2"})
            h["frm"] = pd.to_datetime(h["From"]).dt.strftime("%Y-%m-01")
            return h[h["per"].isin(["C", "P"])]
        h_area, h_l1, h_nat = hdx("area"), hdx("level1"), hdx("national")
        d["frm_d"] = d["frm"].map(month_start)

        # map areas to COD units: municipalities/districts (admin 2), or communes (admin 3) when the analysis used them
        a2, a3 = self.adm[2], self.adm[3]
        k2 = {}
        for n, p, pc in zip(a2["adm2_name"], a2["adm1_name"], a2["adm2_pcode"]):
            k2.setdefault(key(n), []).append((n, p, pc))
        k3 = {}
        for n, m, p, pc in zip(a3["adm3_name"], a3["adm2_name"], a3["adm1_name"], a3["adm3_pcode"]):
            k3.setdefault(key(n), []).append((n, m, p, pc))
        al2, al3 = self.c["ipc_adm2_alias"], self.c["ipc_adm3_alias"]
        parent = {(f, key(a)): key(l1) for f, a, l1 in zip(h_area["frm"], h_area["Area"], h_area["Level 1"])}

        def adm2_hits(k):
            t = al2.get(k, k)
            return [k2[x] for x in (t if isinstance(t, list) else [t]) if x in k2]

        places = []
        for anl, g in d.groupby("anl"):
            share2 = g["area"].map(lambda s: len(adm2_hits(key(s))) > 0).mean()
            level = 2 if share2 >= 0.9 else 3
            for r in g.itertuples():
                k = key(r.area)
                if level == 2:
                    hits = adm2_hits(k)
                    check(len(hits) >= 1 and all(len(h) == 1 for h in hits), f"IPC {self.name} {r.area}: exactly one COD district per name")
                    units = [h[0] for h in hits]
                    provs = {u[1] for u in units}
                    check(len(provs) == 1, f"IPC {self.name} {r.area}: in one province")
                    places.append((r.Index, " + ".join(u[0] for u in units), provs.pop(), 2, [u[2] for u in units], None))
                else:
                    par = parent.get((r.frm_d, k))
                    check(par is not None, f"IPC {self.name} {r.area}: parent municipality found in HDX")
                    cands = [c for c in k3.get(al3.get(k, k), []) if key(c[1]).startswith(par[:6])]
                    check(len(cands) == 1, f"IPC {self.name} {r.area}: one COD commune under {par}")
                    n, mname, p, pc = cands[0]
                    places.append((r.Index, n, p, 3, [pc], mname))
        pl = pd.DataFrame(places, columns=["i", "cod", "prov", "level", "pcodes", "muni"]).set_index("i")
        d = d.join(pl)
        check(d["prov"].notna().all(), f"IPC {self.name}: every area matched to COD units ({len(d)} area-periods)")
        # group each area under the province IPC itself used (HDX "Level 1"), where that is a province;
        # COD only supplies the geometry. Differences are listed on the page.
        l1 = {(f, per, key(a)): self.prov_of(l) for f, per, a, l in zip(h_area["frm"], h_area["per"], h_area["Area"], h_area["Level 1"])}
        d["prov_cod"] = d["prov"]
        d["prov"] = [l1.get((f, per, key(a))) or pc for f, per, a, pc in zip(d["frm_d"], d["per"], d["area"], d["prov_cod"])]
        moved = d[d["prov"] != d["prov_cod"]]
        self.ipc_moved = sorted({(fix_name(r.area), r.prov, r.prov_cod) for r in moved.itertuples()})
        if len(moved):
            note(f"IPC {self.name}: areas IPC places in a different province from COD: "
                 + "; ".join(f"{a}: IPC {p}, COD {c}" for a, p, c in self.ipc_moved))
        d["name"] = d["area"].map(fix_name)
        check(not d["name"].str.contains("�").any(), f"IPC {self.name}: area names free of broken characters")

        # cross-check area Phase 3+ against HDX where HDX has the round
        ha = h_area[h_area["Phase"] == "3+"].assign(k=lambda t: t["Area"].map(key))
        j = d.assign(k=d["area"].map(key)).merge(ha[["frm", "per", "k", "Number"]], left_on=["frm_d", "per", "k"], right_on=["frm", "per", "k"], how="left")
        covered = j.groupby("anl")["Number"].apply(lambda s: s.notna().any())
        diff = j[j["Number"].notna() & (j["Number"] != j["p3"])][["anl", "per", "area", "p3", "Number"]].values.tolist()
        check(not diff, f"IPC {self.name}: API and HDX area Phase 3+ agree where both exist (mismatches {diff[:5]})")
        miss = j[j["anl"].isin(covered[covered].index) & j["Number"].isna()][["anl", "per", "area"]].values.tolist()
        if miss:
            note(f"IPC {self.name}: areas in the API but not the HDX area file: {miss}")
        not_hdx = sorted(pop[a]["analysis_date"] for a in covered[~covered].index)
        if not_hdx:
            note(f"IPC {self.name}: analyses not in the HDX area file (API only): {not_hdx}")

        # national totals: IPC API population endpoint; checked against HDX and the area sums
        rounds = []
        for a in sorted(pop.values(), key=lambda a: month_start(a["analysis_date"])):
            g = d[d["anl"] == a["id"]]
            periods, nat = {}, {}
            for per, fld in (("C", ""), ("P", "_projected")):
                gp = g[(g["per"] == per) & (g["phase"] > 0)]
                if not len(gp):
                    continue
                periods[per] = {"frm": gp["frm"].iloc[0], "to": gp["to"].iloc[0]}
                p3, est = a.get("p3plus" + fld), a.get("estimated_population" + fld)
                check(p3 is not None, f"IPC {self.name} {a['analysis_date']} {per}: national Phase 3+ published")
                gap = abs(p3 - gp["p3"].sum())
                hn = h_nat[(h_nat["per"] == per) & (h_nat["frm"] == month_start(gp["frm"].iloc[0])) & (h_nat["Phase"] == "3+")]
                if len(hn):
                    check(int(hn["Number"].iloc[0]) == p3, f"IPC {self.name} {a['analysis_date']} {per}: HDX national Phase 3+ equals the API ({p3:,})")
                if gap <= max(10, 0.001 * p3):
                    check(True, f"IPC {self.name} {a['analysis_date']} {per}: area sum {int(gp['p3'].sum()):,} vs national {p3:,} (gap {gap})")
                else:
                    check(len(hn) > 0, f"IPC {self.name} {a['analysis_date']} {per}: national total differs from the area sum and HDX has the round to confirm it")
                    note(f"IPC {self.name} {a['analysis_date']} {per}: published national Phase 3+ {p3:,} (API and HDX agree) is {int(p3 - gp['p3'].sum()):+,} "
                         f"against the sum of the {len(gp)} areas ({int(gp['p3'].sum()):,}); the page uses the published figure")
                nat[per] = {"p3": int(p3), "pop": int(est or gp["pop"].sum()), "p4": int(gp["p4"].sum())}
            unit = {2: "municipalities" if self.iso3 == "AGO" else "districts", 3: "communes"}[int(g["level"].iloc[0])]
            rounds.append({"id": a["id"], "label": a["analysis_date"], "title": a["title"], "periods": periods, "nat": nat,
                           "unit": unit, "update": "C" not in periods, "provs": sorted(set(g["prov"]))})

        # province figures: published level-1 figures where HDX has them, otherwise the sum of the areas
        prov = []
        for rd in rounds:
            for per, pr in rd["periods"].items():
                g = d[(d["anl"] == rd["id"]) & (d["per"] == per) & (d["phase"] > 0)]
                hl = h_l1[(h_l1["per"] == per) & (h_l1["frm"] == month_start(pr["frm"]))]
                pub = {}
                if len(hl):
                    for lv, gl in hl.groupby("Level 1"):
                        v, pc = dict(zip(gl["Phase"], gl["Number"])), dict(zip(gl["Phase"], gl["Percentage"]))
                        pv = self.prov_of(lv)
                        if pv is None:  # level 1 is a municipality (Angola 2019): add it to its province
                            m = [p for n, p, _ in sum((k2.get(x, []) for x in [key(lv)]), [])]
                            check(len(m) == 1, f"IPC {self.name} {rd['label']}: level-1 unit {lv} placed in a province")
                            pub.setdefault(m[0], []).append((lv, v["3+"], v.get("all"), None))
                        else:
                            pub.setdefault(pv, []).append((lv, v["3+"], v.get("all"), pc["3+"]))
                for pv, gp in g.groupby("prov"):
                    area_p3, area_pop = int(gp["p3"].sum()), int(gp["pop"].sum())
                    if pv in pub and len(pub[pv]) == 1 and pub[pv][0][3] is not None:
                        lv, p3, allp, pct = pub[pv][0]
                        gap = abs(p3 - area_p3)
                        if gap <= max(10, 0.001 * p3):
                            check(True, f"IPC {self.name} {rd['label']} {per} {pv}: published {int(p3):,} vs area sum {area_p3:,}")
                        else:
                            note(f"IPC {self.name} {rd['label']} {per} {pv}: published province Phase 3+ {int(p3):,} is {int(p3 - area_p3):+,} against the sum of its areas ({area_p3:,}); the page uses the published figure")
                        prov.append(dict(round=rd["id"], per=per, prov=pv, p3=int(p3), pop=None if pd.isna(allp) else int(allp),
                                         share=float(pct), basis="Published province figure"))
                    elif pv in pub:
                        check(all(x[3] is None for x in pub[pv]), f"IPC {self.name} {rd['label']} {per} {pv}: level-1 rows are all municipalities")
                        p3, allp = sum(x[1] for x in pub[pv]), sum(x[2] for x in pub[pv])
                        check(abs(p3 - area_p3) <= max(10, 0.001 * p3), f"IPC {self.name} {rd['label']} {per} {pv}: municipality sum {int(p3):,} vs area sum {area_p3:,}")
                        prov.append(dict(round=rd["id"], per=per, prov=pv, p3=int(p3), pop=int(allp), share=float(p3 / allp),
                                         basis="Total of the municipality figures for " + ", ".join(sorted(x[0] for x in pub[pv]))))
                    else:
                        prov.append(dict(round=rd["id"], per=per, prov=pv, p3=area_p3, pop=area_pop, share=area_p3 / area_pop,
                                         basis=f"Total of the {rd['unit']} analysed"))
        self.ipc, self.ipc_rounds, self.ipc_prov = d, rounds, prov

    # ------------------------------------------------------------ WFP rainfall and NDVI
    def load_wfp(self):
        names = dict(zip(self.adm[1]["adm1_pcode"], self.adm[1]["adm1_name"]))
        area = dict(zip(self.adm[1]["adm1_pcode"], self.adm[1]["area_sqkm"]))
        out, meta = [], {}
        for var, val, avg, base in (("rain", "rfh", "rfh_avg", ("1989-01-01", "2018-12-31")),
                                    ("ndvi", "vim", "vim_avg", ("2002-07-01", "2018-07-01"))):
            slug = f"{self.low}-{'rainfall' if var == 'rain' else 'ndvi'}-subnational"
            url = hdx_resource(slug, "subnat-full.csv")
            d = pd.read_csv(fetch(url, f"wfp_{self.low}_{var}.csv"), parse_dates=["date"])  # no HXL row in these files
            d = d[d["adm_level"] == 1].copy()
            check(set(d["PCODE"]) == set(names), f"WFP {self.name} {var}: province PCODEs match COD")
            check(d.groupby("PCODE")["n_pixels"].nunique().eq(1).all(), f"WFP {self.name} {var}: one pixel count per province")
            d["md"] = d["date"].dt.strftime("%m-%d")
            lta = d[d["date"].between(*base)].groupby(["PCODE", "md"])[val].mean()
            got = d.groupby(["PCODE", "md"])[avg].agg(["min", "max"])
            check((got["max"] - got["min"]).abs().max() == 0, f"WFP {self.name} {var}: {avg} constant per province and dekad")
            gap = (got["min"] - lta.reindex(got.index)).abs().max()
            check(gap < 1e-3 * max(1, lta.abs().max()), f"WFP {self.name} {var}: {avg} equals the {base[0]} to {base[1]} dekadal mean (max diff {gap:.2g})")
            d = d[d["date"].dt.month.isin(ONDJFM)].copy()
            d["season"] = d["date"].map(lambda t: t.year if t.month >= 10 else t.year - 1)
            full = d.groupby(["PCODE", "season"]).size()
            seasons = sorted(s for s in full.index.get_level_values(1).unique() if (full.xs(s, level=1) == 18).all())
            check(seasons == list(range(seasons[0], seasons[-1] + 1)), f"WFP {self.name} {var}: complete Oct-Mar seasons {seasons[0]}/{seasons[0] + 1} to {seasons[-1]}/{seasons[-1] + 1}")
            d = d[d["season"].isin(seasons)]
            if "version" in d:
                check((d["version"] == "final").all(), f"WFP {self.name} {var}: every Oct-Mar dekad used is final")
            agg = "sum" if var == "rain" else "mean"
            s = d.groupby(["PCODE", "season"]).agg(v=(val, agg), n=(avg, agg), px=("n_pixels", "first")).reset_index()
            nat = s.assign(wv=s.v * s.px, wn=s.n * s.px).groupby("season").agg(wv=("wv", "sum"), wn=("wn", "sum"), px=("px", "sum")).reset_index()
            nat = nat.assign(PCODE="_national", v=nat.wv / nat.px, n=nat.wn / nat.px)[["PCODE", "season", "v", "n", "px"]]
            s = pd.concat([s, nat], ignore_index=True)
            s["area"] = s["PCODE"].map(lambda p: names.get(p, "_national"))
            s["pct"] = s["v"] / s["n"]
            s["rank"] = s.groupby("PCODE")["v"].rank(method="min").astype(int)  # 1 = lowest
            s["var"] = var
            out.append(s)
            px = d.groupby("PCODE")["n_pixels"].first()
            meta[var] = {"first": seasons[0], "last": seasons[-1], "base": base, "n": len(seasons),
                         "km2_per_px": {names[p]: round(area[p] / px[p], 1) for p in px.index}}
        k = meta["rain"]["km2_per_px"]
        typical = float(pd.Series(k).median())
        odd = sorted(p for p, v in k.items() if abs(v / typical - 1) > 0.15)
        if odd:
            note(f"WFP {self.name}: units whose pixel count does not fit the COD area (km2/pixel, median {typical:.1f}): " + ", ".join(f"{p} {k[p]}" for p in odd))
            a = self.adm[1].set_index("adm1_name")["area_sqkm"]
            both = sum(a[p] for p in odd) / sum(a[p] / k[p] for p in odd)
            check(abs(both / typical - 1) < 0.1, f"WFP {self.name}: {' + '.join(odd)} together fit the COD area ({both:.1f} km2/pixel)")
        meta["odd_units"] = odd
        self.wfp = pd.concat(out, ignore_index=True)
        self.wfp_meta = meta

    # ------------------------------------------------------------ page
    def write(self, out_dir):
        proj = Proj(self.adm[0].total_bounds)
        geo = self.fews["geo"]
        z = self.fews["zones"]
        fews_map = [[r.fnid, r.prov, r.LZNAME, int(z["CS"]["phase"][r.fnid]), int(z["ML1"]["phase"][r.fnid]),
                     int(z["ML2"]["phase"][r.fnid]), proj.path(r.geometry, 0.01)] for r in geo.itertuples()]
        labels = self.c["map_labels"] or self.provinces
        a1 = self.adm[1].set_index("adm1_name")
        maps = {"w": proj.w, "h": round(proj.h, 1), "outline": proj.path(unary_union(self.adm[0].geometry), 0.01),
                "adm1": [[r.adm1_name, proj.path(r.geometry, 0.01)] for r in self.adm[1].itertuples()],
                "labels": [[n, *proj.xy(a1.loc[n, "center_lon"], a1.loc[n, "center_lat"])] for n in labels],
                "fews": fews_map}
        geoms = {2: self.adm[2].set_index("adm2_pcode").geometry, 3: self.adm[3].set_index("adm3_pcode").geometry}
        shapes = {}
        for r in self.ipc.itertuples():
            k = "+".join(r.pcodes)
            if k not in shapes:
                shapes[k] = proj.path(unary_union([geoms[r.level][p] for p in r.pcodes]), 0.004)
        maps["ipc"] = shapes
        ipc_rows = [[r.anl, r.per, r.name, r.muni if r.level == 3 else None, r.prov, int(r.phase), int(r.pop), int(r.p3), r.p3pct,
                     int(r.p4), r.p4pct, "+".join(r.pcodes)] for r in self.ipc.itertuples()]
        crisis = {}
        for f, prov, lz, cs, ml1, ml2, _ in fews_map:
            if ml2 >= 3:
                crisis.setdefault(lz, set()).add(prov)
        data = {
            "country": {"iso3": self.iso3, "name": self.name}, "built": pd.Timestamp.today().strftime("%Y-%m-%d"),
            "provinces": self.provinces, "asap": self.asap, "maps": maps,
            "fews": {"monthly": self.fews["monthly"], "first": self.fews["first"], "last": self.fews["last"],
                     "zones": {k: {x: v[x] for x in ("round", "doc", "from", "to")} for k, v in z.items()},
                     "crisis": {k: sorted(v) for k, v in crisis.items()}},
            "ipc": ipc_rows, "ipc_rounds": self.ipc_rounds, "ipc_prov": self.ipc_prov, "ipc_moved": self.ipc_moved,
            "wfp": [[r.var, r.area, int(r.season), round(float(r.v), 4 if r.var == "ndvi" else 1),
                     round(float(r.n), 4 if r.var == "ndvi" else 1), round(float(r.pct), 4), int(r.rank)] for r in self.wfp.itertuples()],
            "wfp_meta": self.wfp_meta,
        }
        html = (HERE / "template.html").read_text(encoding="utf-8")
        js = json.dumps(data, ensure_ascii=False, separators=(",", ":"), default=float)
        out = out_dir / self.low / f"{self.low}_hotspots.html"
        out.parent.mkdir(exist_ok=True)
        out.write_text(html.replace("/*DATA*/", "const D=" + js + ";").replace("<title>Drought Hotspots</title>", f"<title>{self.name} Drought Hotspots</title>"), encoding="utf-8")
        return out, len(js)


class Proj:
    """Equirectangular projection scaled to a fixed width, for inline SVG paths."""

    def __init__(self, bounds, width=560):
        x0, y0, x1, y1 = bounds
        self.k = math.cos(math.radians((y0 + y1) / 2))
        self.x0, self.y1 = x0, y1
        self.s = width / ((x1 - x0) * self.k)
        self.w, self.h = width, (y1 - y0) * self.s

    def xy(self, x, y):
        return round((x - self.x0) * self.k * self.s, 1), round((self.y1 - y) * self.s, 1)

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


def build(iso3):
    CHECKS.clear()
    c = Country(iso3)
    c.load_cod()
    c.load_asap()
    c.load_fews()
    c.load_ipc()
    c.load_wfp()
    out, size = c.write(HERE.parent)
    for status, msg in CHECKS:
        print(f"[{status}] {msg}")
    print(f"{iso3}: {sum(s == 'PASS' for s, _ in CHECKS)} checks passed; wrote {out} ({size / 1e3:.0f} kB data)")


if __name__ == "__main__":
    for iso3 in sys.argv[1:] or COUNTRIES:
        build(iso3.upper())

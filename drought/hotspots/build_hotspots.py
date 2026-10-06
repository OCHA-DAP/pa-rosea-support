"""Build a country drought hotspots page: drought/<iso3>/<iso3>_hotspots.html.

Pulls every source fresh, cross-checks them, and fills template.html.

Sources
- FEWS NET Data Warehouse: ipcphase.csv, ipcphase JSON (cross-check), ipcpackage shapefiles
- IPC API (analyses, areas, population; needs IPC_API_KEY) and the HDX IPC country files (cross-check)
- WFP on HDX: dekadal CHIRPS rainfall and MODIS NDVI by admin 1 (monthly; seasonal = Oct-Mar rainfall total, March NDVI)
- HDX COD-AB boundaries (maps, area-to-province lookup)
- SEAS5 seasonal rainfall forecast, ensemble-mean monthly COGs on the team raster store (needs DSCI_AZ_BLOB_PROD_SAS)

Run from the repo root:
    uv run --with pandas --with geopandas --with requests --with rasterio python drought/hotspots/build_hotspots.py AGO ZMB
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

FDW = "https://fdw.fews.net/api/"
IPC_API = "https://api.ipcinfo.org/"
HDX = "https://data.humdata.org/api/3/action/package_show"
ONDJFM = (10, 11, 12, 1, 2, 3)
FIRST_SEASON = 2010  # first October to March season shown on the pages

# Per-country settings. Aliases map a normalised source name to a normalised COD name (see key()),
# and each one was checked by hand against the COD list for that level.
# "focus": provinces the page is about (COD names) with the hazards they were listed for; the page
# shows them first and gives their month-by-month rainfall and NDVI. "renamed": COD provinces that
# have since been divided, with the current provinces they cover.
COUNTRIES = {
    "AGO": {
        "name": "Angola", "iso2": "AO", "capital": "Luanda",
        "focus": {"Cunene": ["drought"], "Huíla": ["drought"], "Namibe": ["drought", "flood"], "Cuando Cubango": ["drought"],
                  "Moxico": ["drought"], "Benguela": ["flood"]},
        # Law of 5 September 2024: 21 provinces. The sources used here still report the former 18.
        "renamed": {"Cuando Cubango": ["Cuando", "Cubango"], "Moxico": ["Moxico", "Moxico Leste"], "Luanda": ["Luanda", "Icolo e Bengo"]},
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
        "name": "Zambia", "iso2": "ZM", "capital": "Lusaka", "focus": {}, "renamed": {},
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


def hdx_resources(dataset, suffix):
    meta = get_json(HDX, f"hdx_{dataset}.json", {"id": dataset})
    check(meta.get("success"), f"HDX dataset {dataset} found")
    return [r["url"] for r in meta["result"]["resources"] if r["name"].endswith(suffix)]


def hdx_resource(dataset, suffix):
    urls = hdx_resources(dataset, suffix)
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
        for k in ("focus", "renamed"):
            check(set(self.c[k]) <= set(self.provinces), f"{self.name}: {k} provinces are COD provinces ({sorted(self.c[k])})")

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
        # While a report is being published, one endpoint can carry its rows before the other. Rows in only one
        # source are accepted when they belong to the latest report; the record used is the union of the two.
        latest = max(csv["reporting_date"].max(), js["reporting_date"].max())
        one = m[m["_merge"] != "both"]
        check((one["reporting_date"] == latest).all(), f"FEWS NET {self.name}: CSV and JSON differ only for the latest report ({latest})")
        both = m[m["_merge"] == "both"]
        check(((both["value_csv"] == both["value_json"]) | (both["value_csv"].isna() & both["value_json"].isna())).all(), f"FEWS NET {self.name}: CSV and JSON phases identical for the {len(both)} shared records")
        if len(one):
            src = {"left_only": "CSV", "right_only": "JSON"}
            note(f"FEWS NET {self.name}: {latest} report partly published: " + "; ".join(f"{n} records only in the {src[w]} endpoint" for w, n in one["_merge"].value_counts().items() if n) + "; the union is used")
            extra = js.merge(one[one["_merge"] == "right_only"][k], on=k)
            csv = pd.concat([csv, extra[csv.columns]], ignore_index=True)
        check(not csv.duplicated(k).any(), f"FEWS NET {self.name}: {len(csv)} records, one per unit, scenario, period and report")
        check((csv["data_usage_policy"] == "Public").all(), f"FEWS NET {self.name}: all rows public")
        csv["sc"] = csv["scenario_name"].str.strip()
        # document labels: keep the type, drop the country suffix; FDW files some reports under another country's name
        parts = csv["source_document"].str.rsplit(", ", n=1, expand=True)
        csv["doc"] = parts[0]
        odd_docs = sorted(set(parts[1].dropna()) - {self.name, "Highest FIC"})
        if odd_docs:
            note(f"FEWS NET {self.name}: {int(parts[1].isin(odd_docs).sum())} records whose document label names another country ({odd_docs}); "
                 "the unit names and IDs are this country's, so the records are used and the label is shown without the country")
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
            zones[col] = {"round": last, "doc": sorted(set(r["doc"]))[0],
                          "from": r["projection_start"].min(), "to": r["projection_end"].max(),
                          "phase": dict(zip(r["fnid"], r["value"].astype(int)))}
        self.fews = {"monthly": monthly, "geo": geo, "zones": zones, "odd_docs": odd_docs,
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
            # HDX publishes a full history file (_long.csv) and a latest-analysis file (_long_latest.csv). Since
            # 2026-10-05 the Angola dataset has only the latest file; earlier analyses come from the copy of the
            # full file taken on 2026-10-02 (hdx_snapshots/), so the cross-checks still cover them.
            ds = f"{self.name.lower()}-acute-food-insecurity-country-data"
            urls = hdx_resources(ds, f"_{level}_long.csv")
            def read(path):  # skip the HXL tag row only when there is one
                hxl = Path(path).read_text(encoding="utf-8", errors="replace").splitlines()[1].startswith("#")
                return pd.read_csv(path, skiprows=[1] if hxl else None, encoding="utf-8", encoding_errors="replace")
            if urls:
                check(len(urls) == 1, f"HDX {ds}: one resource ending _{level}_long.csv")
                h = read(fetch(urls[0], f"ipc_{self.low}_{level}_long.csv"))
            else:
                h = read(fetch(hdx_resource(ds, f"_{level}_long_latest.csv"), f"ipc_{self.low}_{level}_long_latest.csv"))
                snap = HERE / "hdx_snapshots" / f"ipc_{self.low}_{level}_long.csv"
                check(snap.exists(), f"HDX {ds}: full history file missing on HDX and a snapshot exists for {level}")
                old = read(snap)
                old = old[~old["Date of analysis"].isin(set(h["Date of analysis"]))]
                check(set(h.columns) == set(old.columns), f"HDX {ds} {level}: snapshot columns match the current file")
                note(f"HDX {ds} {level}: HDX now carries only {sorted(set(h['Date of analysis']))}; "
                     f"{sorted(set(old['Date of analysis']))} taken from the 2026-10-02 snapshot of the full HDX file")
                h = pd.concat([h, old], ignore_index=True)
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
        for _anl, g in d.groupby("anl"):
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
        l1 = {(f, per, key(a)): self.prov_of(lv) for f, per, a, lv in zip(h_area["frm"], h_area["per"], h_area["Area"], h_area["Level 1"])}
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
        out, monthly, meta = [], [], {}
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
            d["month"] = d["date"].dt.month
            check((d.groupby(["PCODE", "season", "month"]).size() == 3).all(), f"WFP {self.name} {var}: three dekads in every month used")
            mo = d[d["season"] >= FIRST_SEASON].groupby(["PCODE", "season", "month"]).agg(v=(val, agg), n=(avg, agg), px=("n_pixels", "first")).reset_index()
            mnat = mo.assign(wv=mo.v * mo.px, wn=mo.n * mo.px).groupby(["season", "month"]).agg(wv=("wv", "sum"), wn=("wn", "sum"), px=("px", "sum")).reset_index()
            mo = pd.concat([mo, mnat.assign(PCODE="_national", v=mnat.wv / mnat.px, n=mnat.wn / mnat.px)[["PCODE", "season", "month", "v", "n"]]], ignore_index=True)
            mo["area"] = mo["PCODE"].map(lambda p: names.get(p, "_national"))
            mo["var"] = var
            monthly.append(mo)
            # seasonal indicator: rainfall total over October to March; NDVI at the end of the season (March mean)
            ds = d if var == "rain" else d[d["month"] == 3]
            s = ds.groupby(["PCODE", "season"]).agg(v=(val, agg), n=(avg, agg), px=("n_pixels", "first")).reset_index()
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
        mo = pd.concat(monthly, ignore_index=True)
        # the six monthly rainfall values add up to the seasonal total; the March NDVI value is the seasonal NDVI value
        check((mo.groupby(["var", "area", "season"]).size() == 6).all(), f"WFP {self.name}: six monthly values for every area and season since {FIRST_SEASON}")
        chk = mo[mo["var"] == "rain"].groupby(["area", "season"]).agg(v=("v", "sum"), n=("n", "sum")).reset_index().assign(var="rain")
        chk = pd.concat([chk, mo[(mo["var"] == "ndvi") & (mo["month"] == 3)][["var", "area", "season", "v", "n"]]], ignore_index=True)
        chk = chk.merge(self.wfp[["var", "area", "season", "v", "n"]], on=["var", "area", "season"], suffixes=("_m", ""))
        check(len(chk) == len(mo) / 6 and ((chk["v_m"] - chk["v"]).abs() < 1e-6).all() and ((chk["n_m"] - chk["n"]).abs() < 1e-6).all(),
              f"WFP {self.name}: monthly rainfall adds up to the seasonal total and March NDVI equals the seasonal NDVI value, every area and season since {FIRST_SEASON}")
        self.wfp_monthly = mo

    # ------------------------------------------------------------ SEAS5 rainfall forecast (Oct-Mar)
    def load_seas5(self):
        """Oct-Mar rainfall forecast from the latest September SEAS5 issue, per province and national, against
        the same forecasts issued every September since 1981 (team raster store, ensemble-mean monthly COGs)."""
        import calendar

        import numpy as np
        import rasterio
        from rasterio.features import rasterize
        from rasterio.warp import Resampling, reproject
        from rasterio.windows import from_bounds

        os.environ.setdefault("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
        sas = os.environ["DSCI_AZ_BLOB_PROD_SAS"]
        blob = "https://imb0chd0prod.blob.core.windows.net/raster"
        listing = requests.get(f"{blob}?restype=container&comp=list&prefix=seas5/monthly/processed/precip_em_i&maxresults=5000&{sas}", timeout=300).text
        check("<NextMarker>" not in listing.replace("<NextMarker />", ""), "SEAS5: blob listing complete in one page")
        files = set(re.findall(r"precip_em_i(\d{4}-\d\d-\d\d)_lt(\d)", listing))
        issues = sorted({d for d, lt in files if d.endswith("-09-01")})
        check(len(issues) > 0, f"SEAS5: {len(issues)} September issues on blob")
        latest = issues[-1]
        years = [int(d[:4]) for d in issues]
        check(years == list(range(years[0], years[-1] + 1)), f"SEAS5: September issues every year {years[0]} to {years[-1]}")
        check(all((d, str(lt)) in files for d in issues for lt in range(1, 7)), "SEAS5: lead times 1 to 6 present for every September issue")
        out_csv = CACHE / f"seas5_{self.low}_{latest}.csv"

        x0, y0, x1, y1 = self.adm[0].total_bounds
        bounds = (np.floor(x0 * 20) / 20 - 0.5, np.floor(y0 * 20) / 20 - 0.5, np.ceil(x1 * 20) / 20 + 0.5, np.ceil(y1 * 20) / 20 + 0.5)
        res = 0.05
        shape = (int(round((bounds[3] - bounds[1]) / res)), int(round((bounds[2] - bounds[0]) / res)))
        tr = rasterio.transform.from_origin(bounds[0], bounds[3], res, res)
        lat = bounds[3] - res * (np.arange(shape[0]) + 0.5)
        wlat = np.repeat(np.cos(np.radians(lat))[:, None], shape[1], axis=1)
        areas = {"_national": unary_union(self.adm[0].geometry), **dict(zip(self.adm[1]["adm1_name"], self.adm[1].geometry))}
        masks = {k: rasterize([(g, 1)], out_shape=shape, transform=tr, fill=0).astype(bool) for k, g in areas.items()}
        check(all(m.sum() > 0 for m in masks.values()), "SEAS5: every province covers grid cells")

        if os.environ.get("HOTSPOTS_USE_CACHE") and out_csv.exists():
            f = pd.read_csv(out_csv)
        else:
            rows = []
            for d in issues:
                y = int(d[:4])
                total = np.zeros(shape)
                for lt in range(1, 7):
                    vm = 9 + lt
                    vy, vm = (y, vm) if vm <= 12 else (y + 1, vm - 12)
                    with rasterio.open(f"/vsicurl/{blob}/seas5/monthly/processed/precip_em_i{d}_lt{lt}.tif?{sas}") as r:
                        tags = r.tags()
                        check(tags.get("units") == "mm/day" and int(tags.get("month_valid", -1)) == vm and int(tags.get("year_valid", -1)) == vy,
                              f"SEAS5 {d} lt{lt}: mm/day, valid {vy}-{vm:02d}") if lt == 1 or d == latest else None
                        w = from_bounds(*bounds, r.transform).round_offsets().round_lengths()
                        src = r.read(1, window=w).astype("float64")
                        dst = np.full(shape, np.nan)
                        reproject(src, dst, src_transform=r.window_transform(w), src_crs=r.crs, dst_transform=tr, dst_crs=r.crs, resampling=Resampling.nearest)
                        total += dst * calendar.monthrange(vy, vm)[1]
                for k, m in masks.items():
                    ok = m & np.isfinite(total)
                    rows.append(dict(area=k, year=y, mm=float((total[ok] * wlat[ok]).sum() / wlat[ok].sum())))
                print(f"  SEAS5 {self.iso3} {d} done", flush=True)
            f = pd.DataFrame(rows)
            f.to_csv(out_csv, index=False)
        check(f["mm"].notna().all() and (f["mm"] > 0).all() and f.groupby("area").size().nunique() == 1, "SEAS5: a total for every area and year")
        ref = (1991, 2020)
        base = f[f["year"].between(*ref)].groupby("area")["mm"].mean().rename("normal")
        f = f.join(base, on="area")
        f["pct"] = f["mm"] / f["normal"]
        f["rank"] = f.groupby("area")["mm"].rank(method="min").astype(int)
        self.seas5 = f
        self.seas5_meta = {"issued": latest, "season": years[-1], "first": years[0], "n": len(years), "ref": ref, "res": "0.4"}

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
        for _f, prov, lz, _cs, _ml1, ml2, _path in fews_map:
            if ml2 >= 3:
                crisis.setdefault(lz, set()).add(prov)
        data = {
            "country": {"iso3": self.iso3, "name": self.name, "capital": self.c["capital"]}, "built": pd.Timestamp.today().strftime("%Y-%m-%d"),
            "provinces": self.provinces, "first_season": FIRST_SEASON, "focus": self.c["focus"], "renamed": self.c["renamed"], "maps": maps,
            "fews": {"monthly": self.fews["monthly"], "first": self.fews["first"], "last": self.fews["last"], "odd_docs": self.fews["odd_docs"],
                     "zones": {k: {x: v[x] for x in ("round", "doc", "from", "to")} for k, v in z.items()},
                     "crisis": {k: sorted(v) for k, v in crisis.items()}},
            "ipc": ipc_rows, "ipc_rounds": self.ipc_rounds, "ipc_prov": self.ipc_prov, "ipc_moved": self.ipc_moved,
            "wfp": [[r.var, r.area, int(r.season), round(float(r.v), 4 if r.var == "ndvi" else 1),
                     round(float(r.n), 4 if r.var == "ndvi" else 1), round(float(r.pct), 4), int(r.rank)] for r in self.wfp.itertuples()],
            "wfp_meta": self.wfp_meta,
            # [var, area, season, [[value, normal] for Oct..Mar]]
            "wfp_monthly": [[v, a, int(se), [[round(float(r.v), 4 if v == "ndvi" else 1), round(float(r.n), 4 if v == "ndvi" else 1)]
                                            for r in g.sort_values("month", key=lambda m: m.map(ONDJFM.index)).itertuples()]]
                            for (v, a, se), g in self.wfp_monthly.groupby(["var", "area", "season"])],
            "seas5": [[r.area, int(r.year), round(float(r.mm), 1), round(float(r.normal), 1), round(float(r.pct), 4), int(r.rank)] for r in self.seas5.itertuples()],
            "seas5_meta": self.seas5_meta,
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
    c.load_fews()
    c.load_ipc()
    c.load_wfp()
    c.load_seas5()
    out, size = c.write(HERE.parent)
    for status, msg in CHECKS:
        print(f"[{status}] {msg}")
    print(f"{iso3}: {sum(s == 'PASS' for s, _ in CHECKS)} checks passed; wrote {out} ({size / 1e3:.0f} kB data)")


if __name__ == "__main__":
    for iso3 in sys.argv[1:] or COUNTRIES:
        build(iso3.upper())

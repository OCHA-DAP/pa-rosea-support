"""Flood and drought impact records for Ethiopia, Kenya and Somalia, 1981-2025.

Run: python climate/hoa/impacts.py [data_dir]
- EM-DAT (team copy on dev blob): flood and drought events with start/end and people affected
- CERF OneGMS (public feed): Rapid Response and Underfunded allocations with emergency type flood or drought
Writes impacts_emdat.parquet and impacts_cerf.parquet, and the admin 1 boundaries (COD-AB) as adm1.parquet.
"""
import sys
from pathlib import Path

import geopandas as gpd
import pandas as pd
import requests
import ocha_stratus as stratus

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
DATA.mkdir(parents=True, exist_ok=True)
ISO3 = ["ETH", "KEN", "SOM"]

em = stratus.load_parquet_from_blob("emdat/processed/emdat_all.parquet", stage="dev", container_name="global")
em = em[em["ISO"].isin(ISO3) & em["Disaster Type"].isin(["Flood", "Drought"]) & (em["Start Year"] >= 1981)]
keep = ["DisNo.", "ISO", "Disaster Type", "Disaster Subtype", "Event Name", "Location",
        "Start Year", "Start Month", "End Year", "End Month", "Total Deaths", "Total Affected"]
em[[c for c in keep if c in em.columns]].to_parquet(DATA / "impacts_emdat.parquet")

r = requests.get("https://cerfgms-webapi.unocha.org/v1/application/All.json", timeout=300)
r.raise_for_status()
cerf = pd.json_normalize(r.json())
cerf.to_parquet(DATA / "cerf_all_raw.parquet")

adm = pd.concat([stratus.codab.load_codab_from_blob(i, admin_level=1, stage="prod").to_crs(4326)
                 .assign(iso3=i)[["iso3", "ADM1_PCODE", "ADM1_EN", "geometry"]] for i in ISO3])
gpd.GeoDataFrame(adm).to_parquet(DATA / "adm1.parquet")
print(len(em), "EM-DAT events;", len(cerf), "CERF applications;", len(adm), "admin 1 units")

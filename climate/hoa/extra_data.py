"""Extra inputs for the research questions: cell elevation and national cereal production.

Run: python climate/hoa/extra_data.py [data_dir]   (after analyse.py)
- Elevation of each 0.25 deg cell centre: Open-Meteo elevation API (Copernicus DEM 90 m), 100 points a request
- FAOSTAT crops and livestock, Africa bulk file (wide, no flags): cereals, primary (item 1717), production in
  tonnes (element 5510), Ethiopia, Kenya, Somalia
Writes data_dir/out/cell_elevation.csv and data_dir/out/faostat_cereals.csv.
"""
import io
import json
import sys
import time
import urllib.request
import zipfile
from pathlib import Path

import pandas as pd

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
cells = pd.read_parquet(OUT / "cells.parquet")

f = OUT / "cell_elevation.csv"
if not f.exists():
    elev = []
    for i in range(0, len(cells), 100):
        c = cells.iloc[i:i + 100]
        url = ("https://api.open-meteo.com/v1/elevation?latitude=" + ",".join(f"{v:.3f}" for v in c.lat)
               + "&longitude=" + ",".join(f"{v:.3f}" for v in c.lon))
        for attempt in range(6):
            try:
                elev += json.loads(urllib.request.urlopen(url, timeout=60).read())["elevation"]
                break
            except OSError:  # the API sometimes drops the connection
                if attempt == 5:
                    raise
                time.sleep(5 * (attempt + 1))
        time.sleep(1)
    cells.assign(elevation_m=elev)[["lat", "lon", "iso3", "elevation_m"]].to_csv(f, index=False)

f = OUT / "faostat_cereals.csv"
if not f.exists():
    z = urllib.request.urlopen("https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_Africa.zip", timeout=300).read()
    zf = zipfile.ZipFile(io.BytesIO(z))
    # the Africa bulk file is wide: one column per year (Y1961 ... Y2024)
    d = pd.read_csv(zf.open("Production_Crops_Livestock_E_Africa_NOFLAG.csv"), encoding="latin-1", low_memory=False)
    d = d[d["Area"].isin(["Ethiopia", "Kenya", "Somalia"]) & (d["Item Code"] == 1717) & (d["Element Code"] == 5510)]
    d = d.melt(id_vars=["Area", "Unit"], value_vars=[c for c in d.columns if c.startswith("Y") and c[1:].isdigit()],
               var_name="Year", value_name="Value")
    d["Year"] = d["Year"].str[1:].astype(int)
    d.dropna(subset=["Value"]).to_csv(f, index=False)
print(pd.read_csv(OUT / "cell_elevation.csv").groupby("iso3").elevation_m.describe().round(0))
print(pd.read_csv(f).groupby("Area").Year.agg(["min", "max", "count"]))

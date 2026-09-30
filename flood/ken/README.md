# Kenya floods: KHF RA2 anticipatory action triggers against IMERG

Review of the rainfall thresholds in the Kenya Humanitarian Fund's RA2 allocation paper
(September 2026, Priority Area III: anticipatory action ahead of El Niño floods). The partner
triggers are on KMSA 7-day rainfall forecasts (150 mm for Mandera, Wajir and Marsabit; 70-100 mm
over Isiolo, Samburu and the upper Ewaso Ng'iro; a 40 mm advisory for the Tana basin) and on
Tana River gauge levels. This folder tests how often each rainfall threshold has been reached in
observed IMERG rainfall since 1998 and whether those occasions coincide with EM-DAT flood events.

Output: `ken_khf_trigger_review.html`, a single self-contained page.

## Build

```bash
# 1. daily IMERG grids and county statistics, 1998 to present (about 40 minutes; resumable, one year at a time)
python flood/ken/extract_imerg_counties.py data/ken            # optional third argument: 1998-2012
# 2. EM-DAT Kenya floods from the team blob -> data/ken/emdat_ken_floods.parquet (see below)
# 3. backtest tables
python flood/ken/khf_trigger_review.py data/ken
# 4. page
python flood/ken/build_review_page.py data/ken
```

Needs `DSCI_AZ_BLOB_PROD_SAS` (IMERG COGs and COD-AB on the prod raster / projects containers)
and `DSCI_AZ_BLOB_DEV_SAS` (EM-DAT on the dev `global` container). Python with `ocha-stratus`,
`rasterio`, `geopandas`, `pandas`, `pyarrow`, `numpy`. The build reads the COGs rather than the
Postgres `public.imerg` table because it needs the pixel grids (wettest pixel, share of area, and
running totals per pixel), which the table does not hold.

Optional step 3b, a cross-check of the county means against `public.imerg`, shown in the page's
method notes. From a laptop the database is reached through the team Databricks SSH tunnel
(`ds-knowledge-base-internal/infrastructure/local-db-access.md`); in Git Bash:

```bash
db-tunnel up
DSCI_AZ_DB_PROD_HOST=127.0.0.1:15433 python flood/ken/crosscheck_db.py data/ken
```

`db-tunnel` is a Git Bash script, so it does not run from PowerShell.

EM-DAT extract used:

```python
import ocha_stratus as stratus
df = stratus.load_parquet_from_blob("emdat/processed/emdat_all.parquet", stage="dev", container_name="global")
k = df[(df["ISO"] == "KEN") & (df["Disaster Type"].str.contains("Flood", na=False)) & (df["Start Year"] >= 1998)]
k.to_parquet("data/ken/emdat_ken_floods.parquet")
```

## Files

| file | what |
|---|---|
| `extract_imerg_counties.py` | reads a Kenya window from each daily IMERG Late v7 COG, keeps the grids (`years/imerg_ken_grid_YYYY.npz`) and writes county mean, wettest pixel (pixels at least half inside the county) and area-share statistics (`imerg_ken_adm1_daily.parquet`) |
| `crosscheck_db.py` | compares the county means with `public.imerg` (prod) and writes `out/db_crosscheck.json` |
| `khf_trigger_review.py` | rolling 1/3/7-day totals per pixel; county-mean, wettest-pixel and area-share readings per trigger area; Weibull return periods on annual maxima; exceedance episodes matched to EM-DAT; tables in `data/ken/out/` |
| `build_review_page.py` | assembles the HTML page from those tables |
| `ken_khf_trigger_review.html` | the page |

Data produced by the build is not committed. The daily county parquet is on the dev `projects`
container at `pa-rosea-support/processed/imerg/imerg_ken_adm1_daily.parquet`.

## What the page does not test

KMSA forecast skill (observed rainfall stands in for the forecast), the Garissa, Hola and Garsen
gauge thresholds, and the Danish Refugee Council 24-hour trigger, which has no rainfall amount.

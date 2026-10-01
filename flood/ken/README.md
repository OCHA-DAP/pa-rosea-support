# Kenya floods: KHF RA2 rainfall triggers against IMERG

How often the rainfall triggers in the Kenya Humanitarian Fund's RA2 allocation paper
(September 2026, Priority Area III: anticipatory action ahead of El Niño floods) would have been
reached since 1998, and for how many recorded floods, in the counties they were written for and,
applied to the county average, in the eight ASAL counties the allocation names at Severity Level 4
(Garissa, Isiolo, Mandera, Marsabit, Samburu, Tana River, Turkana, Wajir). The triggers are written on KMSA forecasts;
this analysis uses observed NASA IMERG satellite rainfall instead, so results may differ from what
KMSA data would give.

Season: October to December only, because the allocation targets the October to December 2026
rains. A rainfall total counts when its last day falls between 1 October and 31 December, and only
EM-DAT floods that started in those months are used. Frequencies are per season, 1998 to 2025.

Output: `ken_khf_trigger_review.html`, a single self-contained page.

| Trigger (partner) | Tested as |
|---|---|
| Mandera, Wajir, Marsabit: 150 mm in 7 days (Kenya Red Cross Society) | 7-day total of the county average |
| Isiolo and Samburu: 70-100 mm in 7 days (Welthungerhilfe) | 7-day total averaged over Isiolo, Samburu, Nyeri, Nyandarua, Laikipia, Meru, at 70 and 100 mm |
| Garissa: 40 mm advisory for the Tana basin (Kenya Red Cross Society) | 1-day total averaged over the upper Tana counties; the 5.1 m gauge part is not tested |

## Build

```bash
# 1. county rainfall from the daily IMERG COGs (about 40 minutes the first time; resumable)
python flood/ken/extract_imerg_counties.py data/ken
# 2. EM-DAT Kenya floods to data/ken/emdat_ken_floods.parquet (snippet below)
# 3. backtest
python flood/ken/backtest.py data/ken
# 4. second-way recalculation of every count; must print "PROBLEMS: none"
python flood/ken/independent_check.py data/ken
# 5. page
python flood/ken/build_review_page.py data/ken
```

Needs `DSCI_AZ_BLOB_PROD_SAS` (IMERG COGs and COD-AB) and `DSCI_AZ_BLOB_DEV_SAS` (EM-DAT), and
Python with `ocha-stratus`, `rasterio`, `geopandas`, `pandas`, `pyarrow`, `numpy`. The COGs are
used rather than the `public.imerg` table because the wettest-spot figure needs the pixel grids.

```python
import ocha_stratus as stratus
df = stratus.load_parquet_from_blob("emdat/processed/emdat_all.parquet", stage="dev", container_name="global")
k = df[(df["ISO"] == "KEN") & (df["Disaster Type"].str.contains("Flood", na=False)) & (df["Start Year"] >= 1998)]
k.to_parquet("data/ken/emdat_ken_floods.parquet")
```

Optional: `crosscheck_db.py` compares the county averages with `public.imerg` and writes
`out/db_crosscheck.json`, which the page's method notes report. From a laptop the database is
reached through the team Databricks SSH tunnel
(`ds-knowledge-base-internal/infrastructure/local-db-access.md`), in Git Bash:

```bash
db-tunnel up
DSCI_AZ_DB_PROD_HOST=127.0.0.1:15433 python flood/ken/crosscheck_db.py data/ken
```

## Files

| file | what |
|---|---|
| `extract_imerg_counties.py` | reads a Kenya window from each daily IMERG Late v7 COG; writes county `mean_mm` and `max_mm` (wettest pixel at least half inside the county) to `imerg_ken_adm1_daily.parquet` and keeps the grids in `years/`; `--from-grids` recomputes without downloading |
| `backtest.py` | every date each trigger was reached (first day at the threshold, 30-day cooldown), years reached, Weibull return period, EM-DAT floods reached (threshold reached from 30 days before the start to the end), wettest-spot years for 150 mm, and each threshold in the eight ASAL counties (`out/counties.csv`); writes `out/` |
| `independent_check.py` | recomputes every count, including the county table, from the pixel grids and the raw EM-DAT file and compares with `out/` |
| `crosscheck_db.py` | optional comparison with `public.imerg` |
| `build_review_page.py` | the page, from `out/` |

Data are not committed. The county file is on the dev `projects` container at
`pa-rosea-support/processed/imerg/imerg_ken_adm1_daily.parquet`.

## Not tested

KMSA forecast skill, the river-level triggers (Garissa Bridge 5.1 m; Flood Alert levels at
Garissa, Hola and Garsen), and the Danish Refugee Council 24-hour trigger for Darika, which has
no rainfall amount.

# Horn of Africa: ENSO, IOD, rainfall, temperature, dry spells and flood and drought years

Ethiopia, Kenya and Somalia, 1981 to 2025: how ENSO and the Indian Ocean Dipole line up with season
rainfall, afternoon temperature and the longest dry spell, how temperature moves through the year
against the rainy seasons, and each season against EM-DAT flood and drought events.

Output: `hoa_climate_review.html`, a single self-contained page, and `hoa_climate_review_v1.html`, the same
numbers in plain language (no drivers or CHIRTS sections; adds very hot days and a plain ENSO and IOD section).

- Rainfall: CHIRPS v2.0 Africa monthly (UCSB), averaged to 0.25° blocks; CHIRPS v2.0 daily at 0.25°
  for dry spells (a dry day has less than 1 mm)
- Temperature: ERA5 monthly means by hour of day (Copernicus CDS); the afternoon maximum is the
  warmest of the 12:00 to 16:00 East Africa Time means (the monthly-means product has no daily maximum).
  `check_tmax.py` compares it with the ERA5 mean daily maximum for one year. Very hot days (first-version
  page): ERA5 hourly 2 m temperature, the warmest of 12:00 to 16:00 East Africa Time each day; a day is very
  hot above the cell's 90th percentile for that calendar month in 1991 to 2020, also counted with each cell's
  linear warming trend taken out (used for the ENSO, IOD and dry/wet comparisons)
- ENSO: Niño3.4 anomaly, NOAA CPC ERSST v5 (1991 to 2020 base); IOD: DMI, NOAA PSL HadISST. Both
  averaged over the season's own months. The same index sources as `ds-teleconnections`
- Impacts: EM-DAT floods and droughts (team copy on the dev blob); droughts matched to every season
  their months overlap, floods to the season they start in. CERF allocations are fetched but not used yet
- Normals 1991 to 2020; boundaries OCHA COD-AB

## Build

```bash
R="uv run --python 3.11 --with xarray --with netcdf4 --with scipy --with geopandas --with pyarrow --with rasterio --with cdsapi --with ocha-stratus --with requests python"
# 1. ERA5 (needs ~/.cdsapirc) and CHIRPS monthly, about 2.4 GB of downloads, cropped as they arrive
$R climate/hoa/download.py data/hoa
# 2. CHIRPS daily, 45 yearly files of about 80 MB, cropped as they arrive
$R climate/hoa/download_daily.py data/hoa
# 3. EM-DAT, CERF and admin 1 boundaries (DSCI_AZ_BLOB_DEV_SAS, DSCI_AZ_BLOB_PROD_SAS)
$R climate/hoa/impacts.py data/hoa
# 4. normals, regimes and season anomalies; dry spells; ENSO and IOD
$R climate/hoa/analyse.py data/hoa
$R climate/hoa/dry_spells.py data/hoa
$R climate/hoa/drivers.py data/hoa
# 5. second temperature record: CHIRTS-daily Tmax 1983-2016 (34 files of about 145 MB, cropped as they arrive)
$R climate/hoa/download_chirts.py data/hoa
$R climate/hoa/compare_chirts.py data/hoa
# 6. El Nino and the afternoon maximum by month, and how well rain, temperature and dry spells pick out
#    EM-DAT floods and EM-DAT / CERF droughts (needs scikit-learn; not on the page yet)
$R climate/hoa/heat_impact.py data/hoa
# 7. optional: afternoon maximum against the ERA5 daily maximum for one year (reported in the method notes)
$R climate/hoa/check_tmax.py data/hoa 2020
# 8. daily afternoon highs: ERA5 hourly, 45 yearly requests reduced to a daily maximum (about 430 MB kept), then very hot days
$R climate/hoa/download_hourly.py data/hoa
$R --with dask climate/hoa/hot_days.py data/hoa
# 9. research questions: elevation and FAOSTAT, onset and heavy-rain totals (the first-version page uses rq6_*.csv)
$R climate/hoa/extra_data.py data/hoa
$R climate/hoa/daily_metrics.py data/hoa
$R --with scikit-learn climate/hoa/research.py data/hoa
# 10. pages, each followed by a second-way recalculation of its numbers; each must print "PROBLEMS: none"
$R climate/hoa/build_review_page.py data/hoa
$R climate/hoa/independent_check.py data/hoa
$R climate/hoa/hot_check.py data/hoa
$R climate/hoa/build_research_page.py data/hoa
$R climate/hoa/research_check.py data/hoa
```

`hoa_research.html` is the companion page: ten questions on heat, rain, onset, dry spells, ENSO and the IOD
against EM-DAT, CERF and FAOSTAT, written as a short paper (summary, data and method, one section per
question, limitations). Every sentence is generated from `data/hoa/out/rq*.csv`.

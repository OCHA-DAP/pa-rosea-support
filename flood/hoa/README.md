# Horn of Africa: short-rains flood timing

When EM-DAT floods start in Ethiopia, Kenya and Somalia in October to December seasons with both an
El Niño and a positive Indian Ocean Dipole (1994, 1997, 2006, 2018, 2019, 2023), how that compares
with El Niño-only, ENSO-neutral and La Niña seasons, the riverine floods in those other seasons,
October to December rainfall against recorded floods for every season, August-September and October temperature
against rainfall and floods, and
where the 2026 season stands (indices, rain to date, provisional rains start, temperature, reported flooding).

Output: `hoa_flood_timing.html`, a single self-contained page, served at
<https://ocha-dap.github.io/pa-rosea-support/hoa-flood-timing/>.

- Season type: Niño3.4 (NOAA CPC, ERSST v5, 1991-2020 base) and DMI (NOAA PSL, HadISST 1.1), October
  to December averages. El Niño at Niño3.4 ≥ +0.5, La Niña at ≤ -0.5, positive IOD at DMI ≥ +0.4
  (`climate/hoa/drivers.py`)
- Floods: EM-DAT floods starting September to December, 1981-2025; each country-season counts once,
  at its earliest start month. Riverine is the EM-DAT subtype "Riverine flood". The EM-DAT copy used
  (team copy on the dev blob, read 3 November 2026) has no event starting after May 2025
- Rain: CHIRPS v3.0 (`download_chirps3.py`). Daily final data use the 'rnl' split of each pentad total
  across its days (ERA5 daily rain; the only daily v3 version that starts in 1981); the current season's
  preliminary data exist only with the 'sat' (IMERG) split. CHC serves daily v3 as yearly global 0.05°
  NetCDF files, so the script reads only the blocks over the Horn and averages them to the 0.25° cells of
  `cells.parquet` (equal to CHC's own 0.25° files wherever both have a value; CHC's leave 96 part-sea coast
  cells empty). Averages are over the cells where October to December brings at least a quarter of the
  1991-2020 annual rain in CHIRPS v3 monthly data. Rains start is the per-cell anomalous accumulation from
  1 September, computed in `flood_timing.py` with the method of `climate/hoa/daily_metrics.py` (on CHIRPS
  v2 it reproduces that script's values to rounding); cells where the rains never rise above their average
  get the season's last day (for 2026, the last data day) before averaging. "Rain since 1 Sep reaches the normal Oct-Dec total" is the date the running total from
  1 September passes the 1991-2020 mean October to December total
- Temperature: ERA5 2 m air temperature, monthly means 1981-2025 (`data/hoa/era5_t2m_monthly.nc` from
  `climate/hoa/download.py`, CDS), interpolated to the cell centres and averaged over the same cells, as
  departures from each month's 1981-2025 linear trend. The page compares August-September temperature with
  October-December rain and floods (all seasons, and the 30 without El Niño), and October temperature with October
  rain (% of the 1991-2020 October mean). Partial correlations hold the August-September Nino3.4 and DMI constant. 2026: ERA5 daily means from the team's shared copy on the dev blob
  (`raster/era5/daily/processed/2m_temperature_daily_mean/`, produced by ds-heat-exposure from ARCO-ERA5),
  complete months only (for 2025, the country monthly means from these files equal those from the CDS
  monthly means to 0.001 °C)
- 2026: CHIRPS v3 final to 30 September and preliminary from 1 October; NOAA CPC Oceanic Niño Index;
  DMI from NOAA OISST v2.1 daily anomalies (PSL file, anomalies against
  1971-2000), because the HadISST DMI ends in May 2026. Reported flooding is a hand-picked selection
  of ReliefWeb reports, in `template.html`

## Build

Needs the `climate/hoa` outputs first: `download.py` (ERA5 monthly temperature), `impacts.py`, `analyse.py`
(the 0.25° cells) and `drivers.py` (steps 1, 3 and 4 in `climate/hoa/README.md`). `impacts.py` reads EM-DAT from the team dev blob (`DSCI_AZ_BLOB_DEV_SAS`). `climate/hoa` is not committed yet on this branch;
the page cannot be rebuilt from the repo until it is.

```bash
R="uv run --python 3.11 --with ocha-stratus --with xarray --with netcdf4 --with h5netcdf --with h5py --with requests --with pyarrow --with pandas python"
# 0. CHIRPS v3 daily 1981-2026 and monthly normals: slow (CHC rate-limits; several hours in one process).
#    Year ranges can run side by side, e.g. "$R flood/hoa/download_chirps3.py data/hoa 1981 1992", plus
#    "... data/hoa monthly" for the normals; years already on disk are skipped
$R flood/hoa/download_chirps3.py data/hoa
# 1. current season: CPC ONI, OISST DMI, ERA5 daily temperature (needs DSCI_AZ_BLOB_DEV_SAS)
$R flood/hoa/download_current.py data/hoa 2026
# 2. analysis to data/hoa/out/flood_timing.json
$R flood/hoa/flood_timing.py data/hoa 2026
# 3. page
python flood/hoa/build_page.py data/hoa
```

The prose, the report log and the dates written in `template.html` describe the data as of
9 November 2026 (CHIRPS to 5 November, DMI to 7 November, ERA5 temperature to 31 October). After a data refresh, update them before
rebuilding.

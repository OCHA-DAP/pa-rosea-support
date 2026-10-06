# Drought hotspot pages

One build for the country pages `drought/ago/ago_hotspots.html` (Angola) and
`drought/zmb/zmb_hotspots.html` (Zambia). Each page is a single self-contained HTML file showing:

- Summary: one chart per province (provinces of interest where set, otherwise every province) of every
  October to March season since 2010/11 against normal, with the SEAS5 forecast for the coming season and a
  rainfall/NDVI toggle; three maps (seasons below normal in the last six, the latest IPC projection by area,
  the FEWS NET projection by livelihood zone); the ranking of all provinces (average rainfall and NDVI
  deviation over the last six seasons, IPC Phase 3+ share, FEWS NET Crisis months, the SEAS5 forecast and
  FEWS NET projection); and the latest season, forecast and classifications in four lines
- Within the season: WFP's rolling three-month rainfall total through the season (Oct to Dec, then each month
  to the end of March) and the monthly mean NDVI, each against normal, for the last six seasons or every
  season since 2010/11
- IPC and FEWS NET by province: recent IPC analyses and the current and projected FEWS NET phase
- IPC by municipality: share in Crisis or worse for each analysis
- Sources and methods

No fixed thresholds are used: the pages show each season's deviation from normal and average it over the last
six seasons where a single figure per province is needed.

## Build

```bash
uv run --with pandas --with geopandas --with requests --with rasterio python drought/hotspots/build_hotspots.py AGO ZMB
```

Needs `IPC_API_KEY` and `DSCI_AZ_BLOB_PROD_SAS` (for the SEAS5 forecast COGs). Downloads go to a temp cache (`HOTSPOTS_CACHE` to override;
`HOTSPOTS_USE_CACHE=1` reuses earlier downloads). The script prints one `[PASS]` line per check,
`[NOTE]` lines for source issues it works around, and stops on the first failure. To add a country,
add an entry to `COUNTRIES` in `build_hotspots.py`; `focus` lists the provinces of interest (COD names) with
the hazards they were listed for, and `renamed` maps COD provinces that have since been divided to the
current provinces (Angola has had 21 provinces since September 2024; the sources still report the former 18).

## Sources

| Source | Endpoint | Used for |
|---|---|---|
| FEWS NET | `fdw.fews.net/api/ipcphase.csv`, `ipcphase/` (JSON), `ipcpackage/` | classifications, zone geometry |
| IPC | `api.ipcinfo.org/analyses`, `areas`, `population` | area phases and populations, national totals |
| HDX IPC | `<country>-acute-food-insecurity-country-data` (area, level-1, national files; `hdx_snapshots/` holds the 2026-10-02 copy of the Angola history files, which HDX replaced with latest-analysis files on 2026-10-05) | published province totals, cross-check |
| WFP on HDX | `<iso3>-rainfall-subnational` (CHIRPS), `<iso3>-ndvi-subnational` (MODIS) | dekadal rainfall and NDVI by province, by season and by month |
| HDX COD-AB | `cod-ab-<iso3>` | map boundaries, area-to-province lookup |
| SEAS5 | team raster store `seas5/monthly/processed/precip_em_i<issue>_lt<n>.tif` (prod blob) | October to March rainfall forecast from the latest September issue, against the same forecasts issued every September since 1981 |

## What the checks cover, and the source issues found

- FEWS NET CSV and JSON return identical records (the CSV may carry a newer report not yet in the JSON endpoint;
  such rows are noted and kept); the latest shapefile package matches the record.
- IPC API area figures match the HDX area file. Where HDX only publishes the latest analysis (Angola since
  2026-10-05), earlier analyses are read from `hdx_snapshots/` and noted. National totals come from the IPC API and match HDX;
  area sums match them within rounding, except the Zambia Jul 2024 projection, where IPC's published
  national and Eastern totals are 54,214 above the sum of the districts (API and HDX agree). The pages use
  the published figures.
- IPC area names are matched to COD units, with checked aliases per country (see `COUNTRIES`). IPC figures
  are grouped by the province IPC used: in Zambia IPC counts Chirundu in Southern, Chama in Muchinga and
  Itezhi-tezhi in Central (COD: Lusaka, Eastern, Southern).
- IPC areas with an overall phase of 0: those without figures are left out; Lusaka in 2020 has figures
  but no area phase, is outside IPC's own totals, and is mapped as not classified.
- Angola 2019 IPC areas are communes; they are placed with the parent municipality from HDX.
- WFP: province PCODEs match COD, the long-term averages equal the dekadal mean over the documented
  reference periods (rainfall 1989-01-01 to 2018-12-31, NDVI 2002-07-01 to 2018-07-01), and every season
  used is complete and final, WFP's three-month rainfall total (`r3h`) equals the nine-dekad sum, the Oct to Dec
  and Jan to Mar three-month totals add up to the seasonal total, and the March NDVI value equals the seasonal
  NDVI value. WFP's province units differ from COD for Bengo and Luanda (Angola) and for
  Eastern, Muchinga and Southern (Zambia); together they match.
- SEAS5: every September issue since 1981 has lead times 1 to 6; each COG's tags say mm/day and the expected
  valid month. The forecast is compared with the model's own 1991 to 2020 September forecasts, not with
  observed rainfall, so it shows whether the model expects a drier or wetter season than it usually does.

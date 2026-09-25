# Drought hotspot pages

One build for the country pages `drought/ago/ago_hotspots.html` (Angola) and
`drought/zmb/zmb_hotspots.html` (Zambia). Each page is a single self-contained HTML file showing:

- October to March rainfall and NDVI for every season since 2010/11, national and by province, as % of
  WFP's long-term average, lined up with the IPC analyses and FEWS NET's monthly phase
- maps of ASAP cropland warnings, IPC area phases (any analysis, current or projection) and FEWS NET
  livelihood-zone phases
- a province-by-province table combining the three sources
- ASAP national hotspot status by month, and ASAP warnings by province and season since 2001
- IPC Phase 3+ shares by area for each analysis

## Build

```bash
uv run --with pandas --with geopandas --with requests python drought/hotspots/build_hotspots.py AGO ZMB
```

Needs `IPC_API_KEY`. Downloads go to a temp cache (`HOTSPOTS_CACHE` to override;
`HOTSPOTS_USE_CACHE=1` reuses earlier downloads). The script prints one `[PASS]` line per check,
`[NOTE]` lines for source issues it works around, and stops on the first failure. To add a country,
add an entry to `COUNTRIES` in `build_hotspots.py`.

## Sources

| Source | Endpoint | Used for |
|---|---|---|
| JRC ASAP | `agricultural-production-hotspots.ec.europa.eu/files/hotspots_ts.zip`, `warnings_ts.zip` | national hotspots, province warnings |
| FEWS NET | `fdw.fews.net/api/ipcphase.csv`, `ipcphase/` (JSON), `ipcpackage/` | classifications, zone geometry |
| IPC | `api.ipcinfo.org/analyses`, `areas`, `population` | area phases and populations, national totals |
| HDX IPC | `<country>-acute-food-insecurity-country-data` (area, level-1, national files) | published province totals, cross-check |
| WFP on HDX | `<iso3>-rainfall-subnational` (CHIRPS), `<iso3>-ndvi-subnational` (MODIS) | dekadal rainfall and NDVI by province |
| HDX COD-AB | `cod-ab-<iso3>` | map boundaries, area-to-province lookup |

## What the checks cover, and the source issues found

- FEWS NET CSV and JSON return identical records; the latest shapefile package matches the record.
- IPC API area figures match the HDX area file. National totals come from the IPC API and match HDX;
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
  used is complete and final. WFP's province units differ from COD for Bengo and Luanda (Angola) and for
  Eastern, Muchinga and Southern (Zambia); together they match.
- ASAP warning groups are read from the file's own group column; hotspot counts are recounted from the
  raw text.

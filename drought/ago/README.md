# Angola drought hotspots

`ago_hotspots.html` is a self-contained page showing which areas of Angola JRC ASAP, IPC and
FEWS NET have flagged for drought and acute food insecurity:

- October–March rainfall and NDVI for each season since 2010/11 (national and by province, % of WFP's
  long-term average), lined up with the IPC analyses and FEWS NET's monthly near-term phase
- maps of ASAP cropland warnings by province, IPC area phases (Aug 2019 and Jun 2021 analyses)
  and FEWS NET livelihood-zone phases (current and projected)
- a province-by-province table combining the three sources
- the FEWS NET national classification by report month
- ASAP national hotspot status by month, and ASAP warnings by province and season
- IPC Phase 3+ shares by municipality (2021) and commune (2019)

## Build

```bash
uv run --with pandas --with geopandas --with requests python drought/ago/build_ago_hotspots.py
```

Needs `IPC_API_KEY` in the environment. Downloads go to a temp cache
(`AGO_CACHE` to override; `AGO_USE_CACHE=1` reuses earlier downloads). The script prints a
`[PASS]` line per check and stops on the first failure.

## Sources

| Source | Endpoint | Used for |
|---|---|---|
| JRC ASAP | `agricultural-production-hotspots.ec.europa.eu/files/hotspots_ts.zip`, `warnings_ts.zip` | national hotspots, admin-1 warnings |
| FEWS NET | `fdw.fews.net/api/ipcphase.csv?country_code=AO`, `ipcphase/` (JSON), `ipcpackage/` | classifications, zone geometry |
| IPC | `api.ipcinfo.org/analyses`, `areas`, `population` | area phases and populations |
| HDX IPC | `angola-acute-food-insecurity-country-data` (area, level-1, national files) | published province/national totals, cross-check |
| HDX COD-AB | `cod-ab-ago` | map boundaries, IPC area to province lookup |
| WFP on HDX | `ago-rainfall-subnational` (CHIRPS v2), `ago-ndvi-subnational` (MODIS) | dekadal rainfall and NDVI by province |

## Checks the build runs

- FEWS NET CSV and JSON endpoints return identical records; package shapefile phases match the
  record for every zone and round; later update rounds repeat the package projections.
- IPC API area figures match the HDX area file; area sums reproduce the published municipality,
  province and national Phase 3+ totals within rounding (at most 7 people).
- Every IPC area maps to exactly one COD unit. Aliases where the names differ:
  Município dos Gambos = COD Gambos (ex-Chiange); Mocamedes = COD Namibe municipality;
  2019 communes Quilengues = COD Kilengue and Cuchi = COD Kuchi (the seat communes of those
  municipalities).
- WFP rainfall and NDVI: province PCODEs match COD; the long-term average columns equal the
  dekadal mean over the documented reference periods (rainfall 1989-01-01 to 2018-12-31, NDVI
  2002-07-01 to 2018-07-01); every Oct–Mar season used is complete and final. WFP's Bengo and
  Luanda units do not match the COD provinces of the same name (pixel counts), though together
  they match the combined area.
- ASAP warning groups are read from the file's own `w_*_gr` column; provinces match COD names;
  the hotspot count is recounted from the raw text.

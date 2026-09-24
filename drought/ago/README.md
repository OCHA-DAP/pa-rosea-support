# Angola drought hotspots

`ago_hotspots.html` is a self-contained page showing which areas of Angola JRC ASAP, IPC and
FEWS NET have flagged for drought and acute food insecurity:

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

## Checks the build runs

- FEWS NET CSV and JSON endpoints return identical records; package shapefile phases match the
  record for every zone and round; later update rounds repeat the package projections.
- IPC API area figures match the HDX area file; area sums reproduce the published municipality,
  province and national Phase 3+ totals within rounding (at most 7 people).
- Every IPC area maps to exactly one COD unit. Aliases where the names differ:
  Município dos Gambos = COD Gambos (ex-Chiange); Mocamedes = COD Namibe municipality;
  2019 communes Quilengues = COD Kilengue and Cuchi = COD Kuchi (the seat communes of those
  municipalities).
- ASAP warning groups are read from the file's own `w_*_gr` column; provinces match COD names;
  the hotspot count is recounted from the raw text.

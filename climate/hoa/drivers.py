"""ENSO and IOD against season rainfall, temperature and dry spells, Ethiopia, Kenya and Somalia, 1981-2025.

Run: python climate/hoa/drivers.py [data_dir]   (after analyse.py and dry_spells.py)
Indices, averaged over the same months as each season (concurrent):
- ENSO: Nino3.4 anomaly, NOAA CPC ERSST v5, 1991-2020 base (www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii)
- IOD: Dipole Mode Index, NOAA PSL from HadISST1.1 (psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data)
Phases: El Nino where Nino3.4 >= +0.5 C, La Nina <= -0.5 C; positive IOD where DMI >= +0.4 C, negative <= -0.4 C.
Writes data_dir/out/drivers_seasons.csv (one row per country, season, year), drivers_corr.csv (correlations)
and drivers_phase.csv (means by phase).
"""
import sys
import urllib.request
from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
OUT = DATA / "out"
SEASONS = {"MAM": [3, 4, 5], "JJAS": [6, 7, 8, 9], "OND": [10, 11, 12]}
ENSO_T, IOD_T = 0.5, 0.4

nino_f, dmi_f = DATA / "nino34_ersst5.txt", DATA / "dmi_had.txt"
if not nino_f.exists():
    urllib.request.urlretrieve("https://www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii", nino_f)
if not dmi_f.exists():
    urllib.request.urlretrieve("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data", dmi_f)

n = pd.read_csv(nino_f, sep=r"\s+")
nino = pd.Series(n.iloc[:, -1].values, index=pd.MultiIndex.from_arrays([n["YR"], n["MON"]]))  # last column: NINO3.4 ANOM
rows = []
for line in dmi_f.read_text().splitlines()[1:]:
    p = line.split()
    if len(p) == 13 and p[0].isdigit():
        rows += [(int(p[0]), m + 1, float(v)) for m, v in enumerate(p[1:]) if float(v) > -99]
dmi = pd.DataFrame(rows, columns=["y", "m", "v"]).set_index(["y", "m"])["v"]

seas = pd.read_csv(OUT / "seasons.csv")
ds_f = OUT / "dryspells.csv"
dry = pd.read_csv(ds_f)[["iso3", "season", "year", "longest_days", "longest_anom_days", "dry_days", "spells"]] if ds_f.exists() else None


def season_mean(series, y, months):
    vals = [series.get((y, m), np.nan) for m in months]
    return np.nan if any(np.isnan(vals)) else float(np.mean(vals))


seas["nino34"] = [season_mean(nino, y, SEASONS[s]) for y, s in zip(seas.year, seas.season)]
seas["dmi"] = [season_mean(dmi, y, SEASONS[s]) for y, s in zip(seas.year, seas.season)]
seas["enso"] = np.select([seas.nino34 >= ENSO_T, seas.nino34 <= -ENSO_T], ["El Niño", "La Niña"], "neutral")
seas["iod"] = np.select([seas.dmi >= IOD_T, seas.dmi <= -IOD_T], ["positive", "negative"], "neutral")
seas.loc[seas.nino34.isna(), "enso"] = ""
seas.loc[seas.dmi.isna(), "iod"] = ""
if dry is not None:
    seas = seas.merge(dry, on=["iso3", "season", "year"], how="left")
# temperature with its linear trend removed per country and season, so warming does not mix into the index link
seas["tmax_detr_c"] = np.nan
for (i, s), g in seas.groupby(["iso3", "season"]):
    fit = np.polyfit(g.year, g.tmax_anom_c, 1)
    seas.loc[g.index, "tmax_detr_c"] = g.tmax_anom_c - np.polyval(fit, g.year)
seas.to_csv(OUT / "drivers_seasons.csv", index=False)

VARS = {"rain_pct": "rain", "tmax_detr_c": "temp"} | ({"longest_days": "dry spell"} if dry is not None else {})
corr, phase = [], []
for (i, s), g in seas.groupby(["iso3", "season"]):
    for idx in ["nino34", "dmi"]:
        other = "dmi" if idx == "nino34" else "nino34"
        for v, label in VARS.items():
            h = g[[idx, other, v]].dropna()
            r, p = stats.pearsonr(h[idx], h[v])
            rs, ps = stats.spearmanr(h[idx], h[v])  # rank correlation: less pulled by extreme seasons such as 1997
            # partial correlation: the index's link with the other index removed from both; one more parameter, so n-3 df
            ra = h[idx] - np.polyval(np.polyfit(h[other], h[idx], 1), h[other])
            rb = h[v] - np.polyval(np.polyfit(h[other], h[v], 1), h[other])
            pr = np.corrcoef(ra, rb)[0, 1]
            df = len(h) - 3
            pp = 2 * stats.t.sf(abs(pr) * np.sqrt(df / (1 - pr ** 2)), df)
            corr.append(dict(iso3=i, season=s, index=idx, var=label, n=len(h), r=round(r, 3), p=round(p, 4),
                             spearman=round(rs, 3), spearman_p=round(ps, 4), partial_r=round(pr, 3), partial_p=round(pp, 4)))
    h = g[["nino34", "dmi"]].dropna()
    corr.append(dict(iso3=i, season=s, index="nino34", var="dmi", n=len(h), r=round(stats.pearsonr(h.nino34, h.dmi)[0], 3)))
    for drv, col in [("ENSO", "enso"), ("IOD", "iod")]:
        for ph, q in g[g[col] != ""].groupby(col):
            rec = dict(iso3=i, season=s, driver=drv, phase=ph, n=len(q), years=" ".join(map(str, q.year)),
                       rain_pct=q.rain_pct.mean(), rain_median=q.rain_pct.median(), rain_min=q.rain_pct.min(),
                       rain_max=q.rain_pct.max(), below_normal=int((q.rain_pct < 100).sum()), tmax_detr_c=q.tmax_detr_c.mean(),
                       floods=int((q.flood_events > 0).sum()))
            # seasons in this phase that are also in the other index's same-sign phase (El Nino with positive IOD, etc.)
            if drv == "IOD" and ph != "neutral":
                both = "El Niño" if ph == "positive" else "La Niña"
                rec["also_enso"] = int((q.enso == both).sum())
            if dry is not None:
                rec["longest_days"] = q.longest_days.mean()
                rec["longest_anom_days"] = q.longest_anom_days.mean()
            phase.append(rec)
pd.DataFrame(corr).to_csv(OUT / "drivers_corr.csv", index=False)
pd.DataFrame(phase).round(2).to_csv(OUT / "drivers_phase.csv", index=False)
print(pd.DataFrame(corr).query("var != 'dmi'").pivot_table(index=["iso3", "season"], columns=["index", "var"], values="r").round(2).to_string())

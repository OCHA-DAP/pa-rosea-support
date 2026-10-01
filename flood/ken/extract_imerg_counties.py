"""Daily IMERG (Late Run v7) rainfall by Kenya county, 1998 to present.

Reads a Kenya window from each daily COG in the team raster store (prod blob,
raster/imerg/daily/late/v7/processed/) and writes, per county (COD-AB admin 1) and day:
mean_mm (area-weighted mean) and max_mm (wettest pixel with at least half its area inside
the county). The clipped daily grids are kept, one .npz per year, so statistics can be
recomputed without downloading again.

Run:  python flood/ken/extract_imerg_counties.py data/ken                 # all years, resumable
      python flood/ken/extract_imerg_counties.py data/ken --years 1998-2012
      python flood/ken/extract_imerg_counties.py data/ken --from-grids    # recompute from saved grids
Needs DSCI_AZ_BLOB_PROD_SAS.
Output: data/ken/imerg_ken_adm1_daily.parquet, data/ken/years/imerg_ken_{adm1,grid}_YYYY.*
"""
import argparse
import os
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import geopandas as gpd
import numpy as np
import ocha_stratus as stratus
import pandas as pd
import rasterio
from rasterio.windows import from_bounds
from shapely.geometry import box

HOST = "https://imb0chd0prod.blob.core.windows.net/raster"
PREFIX = "imerg/daily/late/v7/processed/imerg-daily-late-"
BOUNDS = (33.5, -5.0, 42.2, 5.3)  # Kenya, lon/lat
MIN_COVER = 0.5                   # share of a pixel inside the county for it to count as the wettest pixel
COLS = ["date", "ADM1_PCODE", "ADM1_EN", "mean_mm", "max_mm"]

for k, v in {"GDAL_DISABLE_READDIR_ON_OPEN": "EMPTY_DIR", "CPL_VSIL_CURL_ALLOWED_EXTENSIONS": ".tif",
             "GDAL_HTTP_MULTIRANGE": "YES", "GDAL_HTTP_MERGE_CONSECUTIVE_RANGES": "YES"}.items():
    os.environ.setdefault(k, v)


def url(date):
    return f"{HOST}/{PREFIX}{date}.tif?{os.environ['DSCI_AZ_BLOB_PROD_SAS']}"


def list_dates():
    cc = stratus.get_container_client(container_name="raster", stage="prod")
    return sorted(b.name[len(PREFIX):-4] for b in cc.list_blobs(name_starts_with=PREFIX) if b.name.endswith(".tif"))


def grid_and_weights(sample_date):
    """Kenya window of the IMERG grid, county x pixel area weights W (rows sum to 1) and
    county x pixel coverage C (share of each pixel's own area inside the county)."""
    with rasterio.open(url(sample_date)) as src:
        win = from_bounds(*BOUNDS, transform=src.transform).round_offsets().round_lengths()
        tr = src.window_transform(win)
        h, w = int(win.height), int(win.width)
        nodata = src.nodata
    gdf = stratus.codab.load_codab_from_blob("KEN", admin_level=1, stage="prod").to_crs(4326)
    rows, cols = np.meshgrid(np.arange(h), np.arange(w), indexing="ij")
    xs0, ys0 = tr * (cols.ravel(), rows.ravel())
    xs1, ys1 = tr * (cols.ravel() + 1, rows.ravel() + 1)
    cells = gpd.GeoDataFrame({"pix": np.arange(h * w)}, crs=4326, geometry=[
        box(min(a, c), min(b, d), max(a, c), max(b, d)) for a, b, c, d in zip(xs0, ys0, xs1, ys1)])
    inter = gpd.overlay(cells, gdf[["ADM1_PCODE", "ADM1_EN", "geometry"]], how="intersection", keep_geom_type=False)
    inter["a"] = inter.geometry.to_crs("+proj=cea").area
    inter["cov"] = inter["a"] / inter["pix"].map(cells.set_index("pix").geometry.to_crs("+proj=cea").area)
    inter["w"] = inter["a"] / inter.groupby("ADM1_PCODE")["a"].transform("sum")
    codes = list(gdf["ADM1_PCODE"])
    idx = {c: i for i, c in enumerate(codes)}
    W = np.zeros((len(codes), h * w))
    C = np.zeros((len(codes), h * w))
    for c, p, wt, cv in zip(inter["ADM1_PCODE"], inter["pix"], inter["w"], inter["cov"]):
        W[idx[c], p] += wt
        C[idx[c], p] += cv
    return win, (h, w), nodata, W, C, codes, dict(zip(gdf["ADM1_PCODE"], gdf["ADM1_EN"]))


def day_stats(d, arr, W, inside, codes, names):
    """County rows for one day: weighted mean over valid pixels, and the wettest inside pixel."""
    valid = ~np.isnan(arr)
    Wv = W * valid
    s = Wv.sum(1)
    a0 = np.nan_to_num(arr)
    mean = np.where(s > 0, (Wv @ a0) / np.where(s > 0, s, 1), np.nan)
    mx = np.where(inside & valid, a0, -np.inf).max(1)
    return [(d, c, names[c], float(mean[i]), float(mx[i]) if np.isfinite(mx[i]) else np.nan) for i, c in enumerate(codes)]


def read_day(date, win, shape):
    err = None
    for attempt in range(4):
        try:
            with rasterio.open(url(date)) as src:
                arr = src.read(1, window=win, out_shape=shape).astype("float64")
                nod = src.nodata
            if nod is not None:
                arr[arr == nod] = np.nan
            arr[arr < 0] = np.nan
            return date, arr.ravel()
        except Exception as e:  # noqa: BLE001  network errors: retry
            err = e
            time.sleep(2 * (attempt + 1))
    return date, err


def extract(out, years_arg):
    dates = list_dates()
    print(f"{len(dates)} daily COGs, {dates[0]} to {dates[-1]}", flush=True)
    win, shape, _, W, C, codes, names = grid_and_weights(dates[-1])
    inside = C >= MIN_COVER
    years = sorted({d[:4] for d in dates})
    if years_arg:
        y0, y1 = years_arg.split("-")
        years = [y for y in years if y0 <= y <= y1]
    for y in years:
        fp, fz = out / "years" / f"imerg_ken_adm1_{y}.parquet", out / "years" / f"imerg_ken_grid_{y}.npz"
        yd = [d for d in dates if d.startswith(y)]
        if fp.exists() and fz.exists() and pd.read_parquet(fp)["date"].nunique() == len(yd):
            continue
        t, recs, fails, grids = time.time(), [], [], {}
        with ThreadPoolExecutor(max_workers=12) as ex:
            for fu in as_completed([ex.submit(read_day, d, win, shape) for d in yd]):
                d, arr = fu.result()
                if isinstance(arr, Exception):
                    fails.append((d, repr(arr)[:120]))
                    continue
                grids[d] = arr.astype("float32")
                recs += day_stats(d, arr, W, inside, codes, names)
        pd.DataFrame(recs, columns=COLS).sort_values(["ADM1_PCODE", "date"]).to_parquet(fp, index=False)
        ks = sorted(grids)
        np.savez_compressed(fz, dates=np.array(ks), grid=np.stack([grids[k] for k in ks]).reshape(len(ks), *shape))
        print(f"{y}: {len(yd)} days in {time.time() - t:.0f}s, {len(fails)} failed {fails[:3]}", flush=True)


def from_grids(out):
    files = sorted((out / "years").glob("imerg_ken_grid_*.npz"))
    _, _, _, W, C, codes, names = grid_and_weights(str(np.load(files[-1])["dates"][-1]))
    inside = C >= MIN_COVER
    for fz in files:
        z = np.load(fz)
        recs = []
        for d, g in zip(z["dates"], z["grid"]):
            recs += day_stats(str(d), g.reshape(-1).astype("float64"), W, inside, codes, names)
        pd.DataFrame(recs, columns=COLS).sort_values(["ADM1_PCODE", "date"]).to_parquet(
            out / "years" / f"imerg_ken_adm1_{fz.stem.split('_')[-1]}.parquet", index=False)
    print(f"recomputed {len(files)} years from grids", flush=True)


def combine(out):
    df = pd.concat(pd.read_parquet(f) for f in sorted((out / "years").glob("imerg_ken_adm1_*.parquet")))
    df["date"] = pd.to_datetime(df["date"])
    df[COLS].to_parquet(out / "imerg_ken_adm1_daily.parquet", index=False)
    print("done", len(df), df["date"].min().date(), df["date"].max().date(), flush=True)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("out_dir", nargs="?", default="data")
    ap.add_argument("--years", help="year range to extract, e.g. 1998-2012 (skips the combine step)")
    ap.add_argument("--from-grids", action="store_true", help="recompute county stats from saved grids")
    a = ap.parse_args()
    out = Path(a.out_dir)
    (out / "years").mkdir(parents=True, exist_ok=True)
    if a.from_grids:
        from_grids(out)
    else:
        extract(out, a.years)
        if a.years:
            return
    combine(out)


if __name__ == "__main__":
    main()

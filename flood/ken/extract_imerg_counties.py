"""Daily IMERG (Late v7) rainfall by Kenya county, 1998 to present.

Reads a Kenya window from each daily COG in the team raster store
(prod blob, raster/imerg/daily/late/v7/processed/) and computes an
area-weighted mean per county (COD-AB admin 1). Output: one parquet with
columns date, ADM1_PCODE, ADM1_EN, mean_mm, max_mm (wettest pixel in the
county) and frac_ge_40/70/100/150 (share of the county's area at or above
that daily total). The clipped daily grids are also kept (one .npz per year)
so other statistics can be computed without re-downloading.

Run:  python flood/ken/extract_imerg_counties.py [out_dir] [YYYY-YYYY]   (optional year range)
Needs DSCI_AZ_BLOB_PROD_SAS. Resumable: one parquet per year in out_dir/years/.
"""
import os, sys, time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.windows import from_bounds
from shapely.geometry import box
import ocha_stratus as stratus

OUT = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
(OUT / "years").mkdir(parents=True, exist_ok=True)
SAS = os.environ["DSCI_AZ_BLOB_PROD_SAS"]
HOST = "https://imb0chd0prod.blob.core.windows.net/raster"
PREFIX = "imerg/daily/late/v7/processed/imerg-daily-late-"
BOUNDS = (33.5, -5.0, 42.2, 5.3)  # Kenya bbox, lon/lat

os.environ.setdefault("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
os.environ.setdefault("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif")
os.environ.setdefault("GDAL_HTTP_MULTIRANGE", "YES")
os.environ.setdefault("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES", "YES")


def url(date):
    return f"{HOST}/{PREFIX}{date}.tif?{SAS}"


def list_dates():
    cc = stratus.get_container_client(container_name="raster", stage="prod")
    names = [b.name for b in cc.list_blobs(name_starts_with=PREFIX)]
    return sorted(n[len(PREFIX):-4] for n in names if n.endswith(".tif"))


def grid_and_weights(sample_date):
    """Pixel grid for the Kenya window and county x pixel area weights."""
    with rasterio.open(url(sample_date)) as src:
        win = from_bounds(*BOUNDS, transform=src.transform).round_offsets().round_lengths()
        tr = src.window_transform(win)
        h, w = int(win.height), int(win.width)
        nodata = src.nodata
    gdf = stratus.codab.load_codab_from_blob("KEN", admin_level=1, stage="prod").to_crs(4326)
    rows, cols = np.meshgrid(np.arange(h), np.arange(w), indexing="ij")
    xs0, ys0 = tr * (cols.ravel(), rows.ravel())
    xs1, ys1 = tr * (cols.ravel() + 1, rows.ravel() + 1)
    cells = gpd.GeoDataFrame(
        {"pix": np.arange(h * w)},
        geometry=[box(min(a, c), min(b, d), max(a, c), max(b, d)) for a, b, c, d in zip(xs0, ys0, xs1, ys1)],
        crs=4326,
    )
    inter = gpd.overlay(cells, gdf[["ADM1_PCODE", "ADM1_EN", "geometry"]], how="intersection", keep_geom_type=False)
    inter["w"] = inter.geometry.to_crs("+proj=cea").area
    inter["w"] = inter["w"] / inter.groupby("ADM1_PCODE")["w"].transform("sum")
    W = np.zeros((len(gdf), h * w), dtype=np.float64)
    codes = list(gdf["ADM1_PCODE"])
    idx = {c: i for i, c in enumerate(codes)}
    for c, p, wt in zip(inter["ADM1_PCODE"], inter["pix"], inter["w"]):
        W[idx[c], p] += wt
    names = dict(zip(gdf["ADM1_PCODE"], gdf["ADM1_EN"]))
    return win, (h, w), nodata, W, codes, names


def read_day(date, win, shape):
    for attempt in range(4):
        try:
            with rasterio.open(url(date)) as src:
                arr = src.read(1, window=win, out_shape=shape).astype("float64")
                nod = src.nodata
            if nod is not None:
                arr[arr == nod] = np.nan
            arr[arr < 0] = np.nan
            return date, arr.ravel()
        except Exception as e:  # noqa: BLE001
            err = e
            time.sleep(2 * (attempt + 1))
    return date, err


def main():
    dates = list_dates()
    print(f"{len(dates)} daily COGs, {dates[0]} to {dates[-1]}", flush=True)
    win, shape, nodata, W, codes, names = grid_and_weights(dates[-1])
    print(f"window {shape}, {len(codes)} counties, weights ok (row sums {W.sum(1).min():.3f}-{W.sum(1).max():.3f})", flush=True)
    years = sorted({d[:4] for d in dates})
    if len(sys.argv) > 2:
        y0, y1 = sys.argv[2].split("-")
        years = [y for y in years if y0 <= y <= y1]
    for y in years:
        f = OUT / "years" / f"imerg_ken_adm1_{y}.parquet"
        fz = OUT / "years" / f"imerg_ken_grid_{y}.npz"
        yd = [d for d in dates if d.startswith(y)]
        if f.exists() and fz.exists() and len(pd.read_parquet(f)["date"].unique()) == len(yd):
            continue
        t = time.time()
        recs, fails, grids = [], [], {}
        Wb = W > 0  # pixel touches county
        with ThreadPoolExecutor(max_workers=12) as ex:
            futs = [ex.submit(read_day, d, win, shape) for d in yd]
            for fu in as_completed(futs):
                d, arr = fu.result()
                if isinstance(arr, Exception):
                    fails.append((d, repr(arr)[:120]))
                    continue
                grids[d] = arr.astype("float32")
                # weights renormalised over valid pixels per county
                valid = ~np.isnan(arr)
                Wv = W * valid
                s = Wv.sum(1)
                a0 = np.nan_to_num(arr)
                m = np.where(s > 0, (Wv @ a0) / np.where(s > 0, s, 1), np.nan)
                mx = np.where(Wb & valid, a0, -np.inf).max(1)
                fr = {t: (Wv * (a0 >= t)).sum(1) / np.where(s > 0, s, 1) for t in (40, 70, 100, 150)}
                for i, c in enumerate(codes):
                    recs.append((d, c, names[c], float(m[i]), float(mx[i]) if np.isfinite(mx[i]) else np.nan,
                                 *(float(fr[t][i]) for t in (40, 70, 100, 150))))
        df = pd.DataFrame(recs, columns=["date", "ADM1_PCODE", "ADM1_EN", "mean_mm", "max_mm",
                                         "frac_ge_40", "frac_ge_70", "frac_ge_100", "frac_ge_150"]).sort_values(["ADM1_PCODE", "date"])
        df.to_parquet(f, index=False)
        ks = sorted(grids)
        np.savez_compressed(fz, dates=np.array(ks), grid=np.stack([grids[k] for k in ks]).reshape(len(ks), *shape))
        print(f"{y}: {len(yd)} days in {time.time()-t:.0f}s, {len(fails)} failed {fails[:3]}", flush=True)
    if len(sys.argv) > 2:
        return
    allf = sorted((OUT / "years").glob("*.parquet"))
    out = pd.concat(pd.read_parquet(f) for f in allf)
    out["date"] = pd.to_datetime(out["date"])
    out.to_parquet(OUT / "imerg_ken_adm1_daily.parquet", index=False)
    print("done", len(out), out["date"].min(), out["date"].max(), flush=True)


if __name__ == "__main__":
    main()

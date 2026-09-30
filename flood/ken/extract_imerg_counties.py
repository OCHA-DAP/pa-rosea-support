"""Daily IMERG (Late v7) rainfall by Kenya county, 1998 to present.

Reads a Kenya window from each daily COG in the team raster store
(prod blob, raster/imerg/daily/late/v7/processed/) and computes an
area-weighted mean per county (COD-AB admin 1). Output: one parquet with
columns date, ADM1_PCODE, ADM1_EN, mean_mm, max_mm (wettest pixel with at
least half its area inside the county) and frac_ge_40/70/100/150 (share of the county's area at or above
that daily total). The clipped daily grids are also kept (one .npz per year)
so other statistics can be computed without re-downloading.

Run:  python flood/ken/extract_imerg_counties.py [out_dir] [YYYY-YYYY]   (optional year range)
      python flood/ken/extract_imerg_counties.py [out_dir] --from-grids    (recompute stats from the kept grids)
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
MIN_COVER = 0.5  # a pixel counts for the wettest-pixel statistic when at least half of it is inside the area

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
    """Pixel grid for the Kenya window, county x pixel area weights W (each row sums to 1)
    and county x pixel coverage C (share of the pixel's own area inside the county)."""
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
    inter["a"] = inter.geometry.to_crs("+proj=cea").area
    cell_area = cells.set_index("pix").geometry.to_crs("+proj=cea").area
    inter["cov"] = inter["a"] / inter["pix"].map(cell_area)
    inter["w"] = inter["a"] / inter.groupby("ADM1_PCODE")["a"].transform("sum")
    W = np.zeros((len(gdf), h * w), dtype=np.float64)
    C = np.zeros((len(gdf), h * w), dtype=np.float64)
    codes = list(gdf["ADM1_PCODE"])
    idx = {c: i for i, c in enumerate(codes)}
    for c, p, wt, cv in zip(inter["ADM1_PCODE"], inter["pix"], inter["w"], inter["cov"]):
        W[idx[c], p] += wt
        C[idx[c], p] += cv
    names = dict(zip(gdf["ADM1_PCODE"], gdf["ADM1_EN"]))
    return win, (h, w), nodata, W, C, codes, names


def day_stats(d, arr, W, Wb, codes, names):
    """County rows for one day: area-weighted mean (renormalised over valid pixels),
    wettest pixel among pixels at least MIN_COVER inside, and area shares above thresholds."""
    valid = ~np.isnan(arr)
    Wv = W * valid
    s = Wv.sum(1)
    a0 = np.nan_to_num(arr)
    m = np.where(s > 0, (Wv @ a0) / np.where(s > 0, s, 1), np.nan)
    mx = np.where(Wb & valid, a0, -np.inf).max(1)
    fr = {t: (Wv * (a0 >= t)).sum(1) / np.where(s > 0, s, 1) for t in (40, 70, 100, 150)}
    return [(d, c, names[c], float(m[i]), float(mx[i]) if np.isfinite(mx[i]) else np.nan,
             *(float(fr[t][i]) for t in (40, 70, 100, 150))) for i, c in enumerate(codes)]


COLS = ["date", "ADM1_PCODE", "ADM1_EN", "mean_mm", "max_mm", "frac_ge_40", "frac_ge_70", "frac_ge_100", "frac_ge_150"]


def main_from_grids():
    files = sorted((OUT / "years").glob("imerg_ken_grid_*.npz"))
    last = str(np.load(files[-1])["dates"][-1])
    win, shape, nodata, W, C, codes, names = grid_and_weights(last)
    Wb = C >= MIN_COVER
    for fz in files:
        z = np.load(fz)
        recs = []
        for d, g in zip(z["dates"], z["grid"]):
            recs += day_stats(str(d), g.reshape(-1).astype("float64"), W, Wb, codes, names)
        y = fz.stem.split("_")[-1]
        pd.DataFrame(recs, columns=COLS).sort_values(["ADM1_PCODE", "date"]).to_parquet(
            OUT / "years" / f"imerg_ken_adm1_{y}.parquet", index=False)
        print(y, "recomputed", flush=True)


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
    if len(sys.argv) > 2 and sys.argv[2] == "--from-grids":
        main_from_grids()
        combine()
        return
    dates = list_dates()
    print(f"{len(dates)} daily COGs, {dates[0]} to {dates[-1]}", flush=True)
    win, shape, nodata, W, C, codes, names = grid_and_weights(dates[-1])
    print(f"window {shape}, {len(codes)} counties, weights ok (row sums {W.sum(1).min():.3f}-{W.sum(1).max():.3f})", flush=True)
    years = sorted({d[:4] for d in dates})
    if len(sys.argv) > 2 and sys.argv[2] != "--from-grids":
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
        Wb = C >= MIN_COVER
        with ThreadPoolExecutor(max_workers=12) as ex:
            futs = [ex.submit(read_day, d, win, shape) for d in yd]
            for fu in as_completed(futs):
                d, arr = fu.result()
                if isinstance(arr, Exception):
                    fails.append((d, repr(arr)[:120]))
                    continue
                grids[d] = arr.astype("float32")
                recs += day_stats(d, arr, W, Wb, codes, names)
        df = pd.DataFrame(recs, columns=COLS).sort_values(["ADM1_PCODE", "date"])
        df.to_parquet(f, index=False)
        ks = sorted(grids)
        np.savez_compressed(fz, dates=np.array(ks), grid=np.stack([grids[k] for k in ks]).reshape(len(ks), *shape))
        print(f"{y}: {len(yd)} days in {time.time()-t:.0f}s, {len(fails)} failed {fails[:3]}", flush=True)
    if len(sys.argv) > 2:
        return
    combine()


def combine():
    allf = sorted((OUT / "years").glob("*.parquet"))
    out = pd.concat(pd.read_parquet(f) for f in allf)
    out["date"] = pd.to_datetime(out["date"])
    out.to_parquet(OUT / "imerg_ken_adm1_daily.parquet", index=False)
    print("done", len(out), out["date"].min(), out["date"].max(), flush=True)


if __name__ == "__main__":
    main()

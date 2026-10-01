"""Cross-check the COG-derived county means against public.imerg (prod Postgres).

Run: python flood/ken/crosscheck_db.py [data_dir]
From a laptop, the database is reached through the team Databricks SSH tunnel
(ds-knowledge-base-internal/infrastructure/local-db-access.md): set
DSCI_AZ_DB_PROD_HOST=127.0.0.1:15433 in the shell for local runs only.
Writes data_dir/out/db_crosscheck.json, which build_review_page.py shows in the method notes.
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import ocha_stratus as stratus

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data")
(DATA / "out").mkdir(exist_ok=True)

mine = pd.read_parquet(DATA / "imerg_ken_adm1_daily.parquet")
mine["date"] = pd.to_datetime(mine["date"])
db = pd.read_sql("select pcode, valid_date, mean from public.imerg where iso3='KEN' and adm_level=1",
                 stratus.get_engine(stage="prod"))
db["valid_date"] = pd.to_datetime(db["valid_date"])

m = mine.merge(db, left_on=["ADM1_PCODE", "date"], right_on=["pcode", "valid_date"], how="left")
missing = sorted(m.loc[m["mean"].isna(), "date"].dt.strftime("%Y-%m-%d").unique())
b = m.dropna(subset=["mean"])
d = (b["mean_mm"] - b["mean"]).abs()

out = dict(
    db_first=str(db["valid_date"].min().date()), db_last=str(db["valid_date"].max().date()),
    counties=int(db["pcode"].nunique()), rows_compared=int(len(b)),
    corr=round(float(np.corrcoef(b["mean_mm"], b["mean"])[0, 1]), 4),
    median_abs_diff_mm=round(float(d.median()), 2), p99_abs_diff_mm=round(float(d.quantile(.99)), 2),
    dates_missing_from_db=missing, trigger_years={},
)
last_full = mine["date"].max().year - 1
out["season"] = "October to December"
for c in ["Mandera", "Wajir", "Marsabit"]:
    x = b[b["ADM1_EN"] == c].set_index("date").sort_index()
    am = {}
    for k, col in [("cog", "mean_mm"), ("db", "mean")]:
        r = x[col].rolling(7).sum()
        r = r[r.index.month.isin([10, 11, 12])]  # October to December totals, as in backtest.py
        am[k] = r.groupby(r.index.year).max()
    yrs = [y for y in am["cog"].index if y <= last_full]
    out["trigger_years"][c] = {
        k: [int(y) for y in yrs if am[k][y] >= 150] for k in am
    } | {"differs": [dict(year=int(y), cog=round(float(am["cog"][y]), 1), db=round(float(am["db"][y]), 1))
                     for y in yrs if (am["cog"][y] >= 150) != (am["db"][y] >= 150)]}

(DATA / "out" / "db_crosscheck.json").write_text(json.dumps(out, indent=1))
print(json.dumps(out, indent=1))

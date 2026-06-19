# https://github.com/utopia-group/pushdown/blob/main/benchmarks/pandas/a_udafs/relevant/Dunai-epfl/ada-exam-repo/ADA%20CopyPaste.ipynb.py

from __future__ import annotations
import os
import time
import pandas as pd
from datasynth import DistSpec, generate
import warnings
import numpy as np
warnings.filterwarnings("ignore")

schema = {
    "BASKET_ID": "int",
    "PRODUCT_ID": "int",
    "household_key": "int",
    "DAY": "int",
}

product_list = ['0', '1', '2', '3']

predicate = lambda r: r['PRODUCT_ID'].isin(product_list)

parquet_path = "Basket_1.parquet"
if os.path.exists(parquet_path):
    # print(f"⏩  Using cached data at {parquet_path}")
    df = pd.read_parquet(parquet_path)
else:
    # print(f"🔄  Generating fresh input and writing to {parquet_path}")
    df = generate(
        schema,
        rows=100_000_000,
        distributions={
            "BASKET_ID": DistSpec.Sequential(1, 1_000, 1),
            "PRODUCT_ID": DistSpec.Zipf(5_000, 1.15),
            "household_key": DistSpec.Zipf(1_000, 1.50),
            "DAY": DistSpec.Zipf(7, 1.50)},
        seed=42)
    df["BASKET_ID"] = df["BASKET_ID"].map(lambda i: f"basket{i}")
    df["PRODUCT_ID"] = (df["PRODUCT_ID"] - 1).astype(str)
    df.to_parquet(parquet_path, index=False)

t0 = time.perf_counter()

out = df.groupby(['household_key', 'BASKET_ID', 'DAY'])
out = out.apply(lambda x : len(set(x.PRODUCT_ID.tolist()) & set(product_list))).reset_index().rename(columns={0:"label"})
filtered = out[np.where(out['label'] > 0, 1, 0) == 1]

runtime = time.perf_counter() - t0
# print(f"Before: {runtime:.3f}s")


t0_pre = time.perf_counter()

df_pre = df[predicate(df)]
out_pre = df_pre.groupby(['household_key', 'BASKET_ID', 'DAY'])
out_pre = out_pre.apply(lambda x : len(set(x.PRODUCT_ID.tolist()) & set(product_list))).reset_index().rename(columns={0:"label"})

runtime_pre = time.perf_counter() - t0_pre
# print(f"After: {runtime_pre:.3f}s")
print(f"{(runtime - runtime_pre) / runtime * 100:.2f}")

# https://github.com/utopia-group/pushdown/blob/main/benchmarks/pandas/a_udafs/relevant/bananachip/JData/user_statistic.ipynb.py

from __future__ import annotations
import os
import time
import pandas as pd
from datasynth import DistSpec, generate
import warnings
import numpy as np
warnings.filterwarnings("ignore")

schema = {
    "user_id": "int",
    "type": "int"
}

predicate = lambda r: r['type'] == 4

parquet_path = "Jdata_1.parquet"
if os.path.exists(parquet_path):
    # print(f"⏩  Using cached data at {parquet_path}")
    df = pd.read_parquet(parquet_path)
else:
    # print(f"🔄  Generating fresh input and writing to {parquet_path}")
    df = generate(
        schema,
        # rows=100_000,
        rows=100_000_000,
        distributions={
            "user_id": DistSpec.Zipf(5_000, 1.50),
            "type": DistSpec.Zipf(100, 1.50)},
        seed=42)
    df["user_id"] = df["user_id"].map(lambda i: f"user{i}")
    df.to_parquet(parquet_path, index=False)

t0 = time.perf_counter()

def ifbuy(lst):
    if 4 in pd.unique(lst.type):
        return 1
    else:
        return 0
out = df[['user_id','type']].groupby('user_id').apply(ifbuy).reset_index(name='ifbuy')
filtered = out[out['ifbuy'] > 0]

runtime = time.perf_counter() - t0
# print(f"Before: {runtime:.3f}s")


t0_pre = time.perf_counter()

df_pre = df[predicate(df)]
def ifbuy(lst):
    if 4 in pd.unique(lst.type):
        return 1
    else:
        return 0
out_pre = df_pre[['user_id','type']].groupby('user_id').apply(ifbuy).reset_index(name='ifbuy')

runtime_pre = time.perf_counter() - t0_pre
# print(f"After: {runtime_pre:.3f}s")
print(f"{(runtime - runtime_pre) / runtime * 100:.2f}")

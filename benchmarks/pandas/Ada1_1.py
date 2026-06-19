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
    "sciper": "int",
    "semester": "int"
}

predicate = lambda r: r['semester'].notnull()

parquet_path  = "Ada1_1.parquet"
if os.path.exists(parquet_path):
    # print(f"⏩  Using cached data at {parquet_path}")
    df = pd.read_parquet(parquet_path)
else:
    # print(f"🔄  Generating fresh input and writing to {parquet_path}")
    df = generate(
        schema,
        rows=100_000_000,
        distributions={
            "sciper": DistSpec.Zipf(10_000, 1.25),
            "semester": DistSpec.Zipf(15, 1.15)},
        seed=42)
    df.loc[np.random.rand(len(df)) < 0.95, "semester"] = None
    df["semester"] = df["semester"].map(lambda i: f"{i:.1f}" if pd.notnull(i) else None)
    df.to_parquet(parquet_path, index=False)

t0 = time.perf_counter()

out = df.groupby('sciper', group_keys=False).apply(
    lambda g: pd.Series({
        'semester_count': g['semester'].count(),
        'in_1_or_2_count': g['semester'].isin(['1.0', '2.0']).sum()})
).reset_index()
filtered = out[
    (out['semester_count'] >= 1) & (out['semester_count'] < 1_300) &
    (out['in_1_or_2_count'] >= 0) & (out['in_1_or_2_count'] <= 200)]

runtime = time.perf_counter() - t0
# # print(f"Before: {runtime:.3f}s")


t0_pre = time.perf_counter()

df_pre = df[predicate(df)]
out_pre = df_pre.groupby('sciper', group_keys=False).apply(
    lambda g: pd.Series({
        'semester_count': g['semester'].count(),
        'in_1_or_2_count': g['semester'].isin(['1.0', '2.0']).sum()})
).reset_index()
filtered_pre = out_pre[
    (out_pre['semester_count'] < 1300) &
    (out_pre['in_1_or_2_count'] <= 200) &
    (out_pre['semester_count'] != 0)]

runtime_pre = time.perf_counter() - t0_pre
# print(f"After: {runtime_pre:.3f}s")
print(f"{(runtime - runtime_pre) / runtime * 100:.2f}")

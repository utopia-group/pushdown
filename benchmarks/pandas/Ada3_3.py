from __future__ import annotations
import os
import time
import pandas as pd
from datasynth import DistSpec, generate
import warnings
import numpy as np
warnings.filterwarnings("ignore")

schema = {
    "team": "int",
    "rank": "int",
    "score1": "int",
    "score2": "int",
}

predicate = lambda r: (r['score1'] <= 95) | (r['score2'] <= 40)

parquet_path  = "Ada3_1.parquet"
if os.path.exists(parquet_path):
    # print(f"⏩  Using cached data at {parquet_path}")
    df = pd.read_parquet(parquet_path)
else:
    # print(f"🔄  Generating fresh input and writing to {parquet_path}")
    df = generate(
        schema,
        rows=100_000_000,
        distributions={
            "team": DistSpec.Sequential(1, 1_000, 1),
            "rank": DistSpec.Zipf(5, 1.50),
            "score1": DistSpec.Zipf(500, 1.01),
            "score2": DistSpec.Zipf(500, 1.01)},
        seed=42)
    df["team"] = df["team"].map(lambda i: f"team{i}")
    df["rank"] = df["rank"].astype(str)
    # df["score1"] = df["score1"] + 40
    # df["score2"] = df["score2"] + 40
    df.to_parquet(parquet_path, index=False)

t0 = time.perf_counter()

out = df.groupby(['team', 'rank']).apply(
    lambda g: pd.Series({
        'min1': g['score1'].min(),
        'min2': g['score2'].min(),
    })
).reset_index()
filtered = out[
    (out['min1'] <= 95) &
    ((out['min2'] == 40) | (out['min2'] < 15))]

runtime = time.perf_counter() - t0
# print(f"Before: {runtime:.3f}s")


t0_pre = time.perf_counter()

df_pre = df[predicate(df)]
out_pre = df_pre.groupby(['team', 'rank']).apply(
    lambda g: pd.Series({
        'min1': g['score1'].min(),
        'min2': g['score2'].min(),
    })
).reset_index()
filtered_pre = out_pre[
    (out_pre['min1'] <= 95) &
    ((out_pre['min2'] == 40) | (out_pre['min2'] < 15))]

runtime_pre = time.perf_counter() - t0_pre
# print(f"After: {runtime_pre:.3f}s")
print(f"{(runtime - runtime_pre) / runtime * 100:.2f}")

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
    "trackable_name": "int"
}

predicate = lambda r: (r['trackable_name'] == 'Headache') | (r['trackable_name'] == 'Nausea')

trackable_names = ["Pain", "Fatigue", "Headache", "Nausea", "Medication", "Mood"]

parquet_path = "Flaredown_1.parquet"
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
            "trackable_name": DistSpec.Zipf(len(trackable_names), 1.40)},
        seed=42)
    df["user_id"] = df["user_id"].map(lambda i: f"user{i}")
    df["trackable_name"] = df["trackable_name"].map(lambda i: trackable_names[i - 1])
    df.to_parquet(parquet_path, index=False)

t0 = time.perf_counter()

out = df.groupby('user_id').apply(
    lambda g: pd.Series({
        'Headache': np.any(g['trackable_name'] == 'Headache'),
        'Nausea': np.any(g['trackable_name'] == 'Nausea'),
    })
).reset_index()
filtered = out[out['Headache'] | ~out['Nausea']]

runtime = time.perf_counter() - t0
# print(f"Before: {runtime:.3f}s")


t0_pre = time.perf_counter()

df_pre = df[predicate(df)]
out_pre = df_pre.groupby('user_id').apply(
    lambda g: pd.Series({
        'Headache': np.any(g['trackable_name'] == 'Headache'),
        'Nausea': np.any(g['trackable_name'] == 'Nausea'),
    })
).reset_index()
filtered_pre = out_pre[out_pre['Headache'] | ~out_pre['Nausea']]

runtime_pre = time.perf_counter() - t0_pre
# print(f"After: {runtime_pre:.3f}s")
print(f"{(runtime - runtime_pre) / runtime * 100:.2f}")

from __future__ import annotations
import os
import time
import pandas as pd
from datasynth import DistSpec, generate
import warnings
import numpy as np
warnings.filterwarnings("ignore")

schema = {
    "Titles": "int",
    "Gender": "int",
    "Salary": "float64",
}

predicate = lambda r: (r['Gender'] == 'M') | (r['Gender'] == 'F')

titles_lbls = [
    "Police Officer", "Firefighter", "Laborer", "Engineer", "Manager",
    "Analyst", "Administrative Assistant", "Nurse", "Teacher", "Clerk",
    "Driver", "Specialist", "Coordinator", "Director", "Inspector",
    "Foreman", "Mechanic", "Electrician", "Carpenter", "Plumber",
    "Guard", "Case Worker", "Attorney", "Developer", "Planner",
    "Technician", "Operator", "Architect", "Scientist", "Consultant"]

gender_lbls = ["X", "U", "F", "M"]

parquet_path = "Chicago_1.parquet"
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
            "Titles": DistSpec.Zipf(len(titles_lbls), 1.15),
            "Gender": DistSpec.Zipf(len(gender_lbls), 2.6),
            "Salary": DistSpec.Gaussian(70_000, 18_000)},
        seed=42)
    df["Titles"] = df["Titles"].map(lambda i: titles_lbls[i - 1])
    df["Gender"] = df["Gender"].map(lambda i: gender_lbls[i - 1])
    df["Salary"] = df["Salary"].clip(lower=25_000).round(0).astype("int64")
    df.to_parquet(parquet_path, index=False)

t0 = time.perf_counter()

out = df.groupby('Titles').apply(
    lambda g: pd.Series({
        'M': np.any(g['Gender'] == 'M'),
        'F': np.any(g['Gender'] == 'F'),
    })
).reset_index()
filtered = out[~out['M'] | out['F']]

runtime = time.perf_counter() - t0
# print(f"Before: {runtime:.3f}s")


t0_pre = time.perf_counter()

df_pre = df[predicate(df)]
out_pre = df_pre.groupby('Titles').apply(
    lambda g: pd.Series({
        'M': np.any(g['Gender'] == 'M'),
        'F': np.any(g['Gender'] == 'F'),
    })
).reset_index()
filtered_pre = out_pre[~out_pre['M'] | out_pre['F']]

runtime_pre = time.perf_counter() - t0_pre
# print(f"After: {runtime_pre:.3f}s")
print(f"{(runtime - runtime_pre) / runtime * 100:.2f}")

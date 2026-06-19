from __future__ import annotations
import os
import time
import pandas as pd
from datasynth import DistSpec, generate
import warnings
warnings.filterwarnings("ignore")

schema = {
    "Nombre": "int",
    "Estado": "int"
}

udf = lambda x: int('Adjudicada' in x['Estado'].values)

predicate = lambda r: r['Estado'] == 'Adjudicada'

estado_lbls = ["Adjudicada", "Pendiente", "Rechazada", "Cancelada"]

parquet_path  = "20Nov_1.parquet"
if os.path.exists(parquet_path):
    # print(f"⏩  Using cached data at {parquet_path}")
    df = pd.read_parquet(parquet_path)
else:
    # print(f"🔄  Generating fresh input and writing to {parquet_path}")
    df = generate(
        schema,
        rows=100_000_000,
        distributions={
            "Nombre": DistSpec.Zipf(1_000, 1.25),
            "Estado": DistSpec.Zipf(len(estado_lbls), 1.05)},
        seed=42)
    df["Nombre"] = df["Nombre"].map(lambda i: f"Empresa{i}")
    df["Estado"] = df["Estado"].map(lambda i: estado_lbls[i - 1]).astype("string")
    df.to_parquet(parquet_path, index=False)

t0 = time.perf_counter()

out = df.groupby('Nombre').apply(lambda x: int('Adjudicada' in x['Estado'].values))
out = out.to_frame().reset_index()
out.columns = ['Nombre', 'class_sold']
out = df.merge(out, on='Nombre')
filtered = out[out["class_sold"] == 1]

runtime = time.perf_counter() - t0
# print(f"Before: {runtime:.3f}s")


t0_pre = time.perf_counter()

df_pre = df[predicate(df)]
out_pre = df_pre.groupby('Nombre').apply(lambda x: int('Adjudicada' in x['Estado'].values))
out_pre = out_pre.to_frame().reset_index()
out_pre.columns = ['Nombre', 'class_sold']
out_pre = df_pre.merge(out_pre, on='Nombre')

runtime_pre = time.perf_counter() - t0_pre
# print(f"After: {runtime_pre:.3f}s")
print(f"{(runtime - runtime_pre) / runtime * 100:.2f}")

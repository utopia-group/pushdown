# datasynth.py
"""
Vectorised data generator that mirrors the behaviour of the Scala `DataSynth`
utility for Spark.  It returns a Pandas `DataFrame`, so the resulting data
works equally well with Pandas, PySpark-on-Pandas, Polars, DuckDB, etc.

Example
-------
>>> from datasynth import DistSpec, generate
>>> import pandas as pd
>>> schema = {"months": "int32", "days": "int32", "micros": "int32"}
>>> df = generate(schema, rows=1_000,
...               distributions={
...                   "months": DistSpec.Zipf(100, 1.10),
...                   "days"  : DistSpec.Zipf(1_000, 1.10),
...                   "micros": DistSpec.Zipf(10_000, 1.05)},
...               seed=42)
"""

from __future__ import annotations

import math
import hashlib
import random
from dataclasses import dataclass
from typing import Dict, List, Mapping, MutableMapping, Sequence

import numpy as np
import pandas as pd


# ──────────────────────────────────────────────────────────────────────────
#   1.  Distribution algebra  (faithful port of the Scala ADT)
# ──────────────────────────────────────────────────────────────────────────
class DistSpec:  # sealed “root” type

    # 1.1 Numeric / temporal ------------------------------------------------
    @dataclass(frozen=True)
    class Gaussian:
        mu: float
        sigma: float

    @dataclass(frozen=True)
    class Sequential:
        start: int = 0
        end: int | None = None      # end is mandatory in constructor below
        step: int = 1

        def __post_init__(self):
            if self.step == 0:
                raise ValueError("step must be non-zero")
            if self.end is None or self.end < self.start:
                raise ValueError("end must be ≥ start")
            if (self.end - self.start) % self.step != 0:
                raise ValueError("end − start must be divisible by step")

    @dataclass(frozen=True)
    class Zipf:
        max: int
        exponent: float = 1.2           # s > 1

    # 1.2 String ------------------------------------------------------------
    @dataclass(frozen=True)
    class CategoricalString:
        pmf: Sequence[tuple[str, float]]

        # Factory for a uniform categorical distribution
        @staticmethod
        def uniform(*values: str) -> "DistSpec.CategoricalString":
            if not values:
                raise ValueError("At least one category required")
            p = 1.0 / len(values)
            return DistSpec.CategoricalString([(v, p) for v in values])


# ──────────────────────────────────────────────────────────────────────────
#   2.  Generator
# ──────────────────────────────────────────────────────────────────────────
def _categorical_string(col_size: int, pmf: Sequence[tuple[str, float]]) -> np.ndarray:
    values, probs = zip(*pmf)
    return np.random.choice(values, size=col_size, p=probs)


def _zipf_bounded(size: int, max_value: int, s: float) -> np.ndarray:
    """
    Draw from Zipf(s) but truncate to ≤ max_value, exactly like the Scala
    implementation that used `pow(rand, -1/(s−1))`.
    """
    # Draw until all are ≤ max_value (vectorised rejection sampling)
    out = np.empty(size, dtype=np.int64)
    filled = 0
    while filled < size:
        # numpy Zipf starts at 1
        batch = np.random.zipf(a=s, size=size - filled)
        batch = np.minimum(batch, max_value)
        out[filled:filled + len(batch)] = batch
        filled += len(batch)
    return out


def _random_bytes_hex(n_rows: int, n_hex: int = 32) -> np.ndarray:
    """
    SHA-256 over a random float, truncated – mirrors the Scala SHA2/MD5 trick.
    """
    rnd = np.random.random(n_rows).astype("float64").tobytes()
    digest = hashlib.sha256(rnd).hexdigest()[:n_hex]
    return np.repeat(digest, n_rows)


# --------------------------------------------------------------------------
def generate(
    schema: Mapping[str, str] | Sequence[tuple[str, str]],
    /,
    *,
    rows: int,
    distributions: Mapping[str, object] | None = None,
    overrides: Mapping[str, Sequence] | None = None,
    seed: int | None = None,
) -> pd.DataFrame:
    """
    Parameters
    ----------
    schema : dict[str, str] or list[tuple[str,str]]
        Column names mapped to pandas / numpy dtypes (e.g. ``"int32"``,
        ``"float64"``, ``"datetime64[ns]"`` …).
    rows : int
        Number of rows to generate (must be > 0).
    distributions, overrides
        Same semantics as the Scala version.
        * **distributions** - declarative sampling instructions.
        * **overrides** - explicit arrays / Series that win over everything.
    seed : int, optional
        When given, fixes the RNG seed exactly like
        `spark.sparkContext.setLocalProperty("spark.random.seed", …)`.
    """
    if rows <= 0:
        raise ValueError("rows must be positive")

    if seed is not None:
        np.random.seed(seed)
        random.seed(seed)

    # Pandas-friendly “schema” normalisation
    if isinstance(schema, Mapping):
        schema_items: List[tuple[str, str]] = list(schema.items())
    else:
        schema_items = list(schema)

    distributions = distributions or {}
    overrides = overrides or {}

    # ----------------------------------------------------------------------
    cols: MutableMapping[str, Sequence] = {}

    # Row index for Sequential() – avoid allocating the whole range when we
    # merely need modulo arithmetic
    row_index = np.arange(rows, dtype=np.int64)

    for name, dtype in schema_items:
        # 1.  Explicit override?
        if name in overrides:
            col = overrides[name]
            if len(col) != rows:
                raise ValueError(f"Override for {name} has length != rows")
            cols[name] = pd.Series(col, dtype=dtype)
            continue

        dist = distributions.get(name, None)

        # 2.  Distribution-driven generation --------------------------------
        if isinstance(dist, DistSpec.Gaussian):
            mu, sigma = dist.mu, dist.sigma
            col = np.random.randn(rows) * sigma + mu

        elif isinstance(dist, DistSpec.Sequential):
            steps_per_cycle = ((dist.end - dist.start) // dist.step) + 1
            col = dist.start + (row_index % steps_per_cycle) * dist.step

        elif isinstance(dist, DistSpec.Zipf):
            col = _zipf_bounded(rows, dist.max, dist.exponent)

        elif isinstance(dist, DistSpec.CategoricalString):
            col = _categorical_string(rows, dist.pmf)

        # 3.  Fallback default for dtype ------------------------------------
        else:
            if np.issubdtype(np.dtype(dtype), np.bool_):
                col = np.random.rand(rows) < 0.5
            elif np.issubdtype(np.dtype(dtype), np.number):
                col = np.random.randn(rows)
            elif dtype.startswith("datetime64"):
                # approx: Gaussian around epoch, scaled
                col = pd.to_datetime(np.random.randn(rows).astype("int64"), unit="D")
            elif dtype.startswith("timedelta64"):
                col = pd.to_timedelta(np.random.randn(rows).astype("int64"), unit="us")
            elif dtype == "object" or dtype.startswith("str"):
                col = _random_bytes_hex(rows, 12)
            else:
                raise ValueError(f"No default generator for type {dtype!r}")

        cols[name] = pd.Series(col, dtype=dtype)

    return pd.DataFrame(cols)

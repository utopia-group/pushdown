#!/usr/bin/env python3
"""
Master script for reproducing paper tables and figures.

Usage:
  python3 experiments.py                       # Reproduce all tables and figures
  python3 experiments.py --task results        # Same as above
  python3 experiments.py --task rerun          # Re-run experiments from scratch (hours)

Individual tables/figures:
  python3 experiments.py --task table1     # Table 1: UDF Statistics
  python3 experiments.py --task table2     # Table 2: Pushdown Outcomes
  python3 experiments.py --task table3     # Table 3: Types of Pushdowns
  python3 experiments.py --task table4     # Table 4: Pipeline Runtime Reduction
  python3 experiments.py --task table5     # Table 5: Search Space Reduction
  python3 experiments.py --task table6     # Table 6: Solution Optimality
  python3 experiments.py --task figure5    # Figure 5: CDF Plot
"""

import csv
import os
import re
import subprocess
import sys
import pandas as pd
import numpy as np
from pathlib import Path
from statistics import mean as _mean, median as _median
from typing import Dict, List, Tuple, Optional
import argparse

import matplotlib.pyplot as plt
from matplotlib import rcParams
import matplotlib.lines as mlines


CDF_COLORS = dict(
    push="C0",
    nob="#f28e00",
    sp="green",
    eld="red",
    noa="purple",
    two="brown",
    noba="darkslategray",
)


def _make_cdf(rts: np.ndarray, tot: int) -> Tuple[np.ndarray, np.ndarray]:
    idx = np.arange(1, len(rts) + 1)
    y = idx / tot * 100.0
    x = np.concatenate(([0.0], rts))
    y = np.concatenate(([0.0], y))
    return x, y


def _styled_scatter(ax, xs, ys, marker, colour, edge_w=0.6, size=30):
    ax.scatter(
        xs,
        ys,
        marker=marker,
        s=size,
        facecolors=colour,
        edgecolors="white",
        linewidths=edge_w,
        zorder=3,
    )


def _halo_handle(colour: str, marker: str, size: int = 5, edge_w: float = 0.6):
    return mlines.Line2D(
        [],
        [],
        lw=1,
        color=colour,
        marker=marker,
        markersize=size,
        markerfacecolor=colour,
        markeredgecolor="white",
        markeredgewidth=edge_w,
    )


# ---------------------------------------------------------------------------
# Benchmark runner constants and helpers
# ---------------------------------------------------------------------------

FLOAT_RE = re.compile(r"^(\d+\.\d{2})s?$", re.MULTILINE)
STEM_RE = re.compile(r"^(.+)_([0-9]+)$")
CLASS_STEM_RE = re.compile(r"^(.+)_([0-9]+)$")

DEFAULT_MASTER = "local[4]"
DEFAULT_DRIVER_MEMORY = "6g"
DEFAULT_SPARK_CONF = {
    "spark.driver.memoryOverhead": "1g",
    "spark.driver.host": "127.0.0.1",
    "spark.driver.bindAddress": "127.0.0.1",
    "spark.default.parallelism": "8",
    "spark.sql.shuffle.partitions": "8",
    "spark.sql.files.maxPartitionBytes": "64m",
    "spark.sql.adaptive.enabled": "false",
    "spark.speculation": "false",
    "spark.dynamicAllocation.enabled": "false",
    "spark.executor.memory": "6g",
    "spark.sql.codegen.wholeStage": "false",
    "spark.ui.enabled": "false",
    "spark.eventLog.enabled": "false",
    "spark.driver.extraJavaOptions": "-XX:+AlwaysPreTouch",
    "spark.executor.extraJavaOptions": "-XX:+AlwaysPreTouch -XX:+UseG1GC -XX:ParallelGCThreads=2 -XX:ConcGCThreads=1",
    "spark.local.dir": "/tmp/spark_tmp",
}


def parse_runtime(text: str) -> Optional[float]:
    """Extract a float runtime (e.g. '12.34' or '12.34s') from text."""
    m = FLOAT_RE.search(text)
    return float(m.group(1)) if m else None


def _append_speedup_row(benchmark: str, speedup: str, results_path: Path) -> None:
    """Append a single speedup row to the CSV, writing the header if needed."""
    write_header = not results_path.exists() or results_path.stat().st_size == 0
    with results_path.open("a", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        if write_header:
            w.writerow(["benchmark", "speedup"])
        w.writerow([benchmark, speedup])


def _load_completed_speedups(results_path: Path) -> set:
    """Return set of benchmark names already present in the speedups CSV."""
    if not results_path.exists():
        return set()
    completed = set()
    with results_path.open(newline="", encoding="utf-8") as f:
        reader = csv.reader(f)
        for row in reader:
            if row and row[0] != "benchmark":
                completed.add(row[0])
    return completed


def _print_speedup_summary(results_path: Path) -> None:
    """Read the speedups CSV and print summary statistics."""
    if not results_path.exists():
        print(f"No results file found at {results_path}")
        return
    rows = []
    with results_path.open(newline="", encoding="utf-8") as f:
        reader = csv.reader(f)
        for row in reader:
            if row and row[0] != "benchmark":
                rows.append(row)

    valid = [float(r[1].rstrip("%")) for r in rows if r[1] != "NA"]
    if valid:
        neg = sum(1 for s in valid if s < 0)
        print(f"\n{'=' * 60}")
        print(f"Results: {results_path}")
        print(f"Benchmarks: {len(valid)}")
        print(f"Mean speedup:   {sum(valid) / len(valid):.1f}%")
        print(f"Median speedup: {sorted(valid)[len(valid) // 2]:.1f}%")
        print(f"Negative:       {neg}/{len(valid)}")
        print(f"Range:          {min(valid):+.1f}% .. {max(valid):+.1f}%")

    print(f"\nResults written to {results_path}")


# -- pandas runner helpers --------------------------------------------------


def _run_pandas_script(
    python_cmd: str, script: Path, extra_env: Optional[dict] = None
) -> Optional[float]:
    env = {**os.environ, **(extra_env or {})}
    try:
        p = subprocess.run(
            [python_cmd, script.name],
            text=True,
            capture_output=True,
            check=False,
            cwd=str(script.resolve().parent),
            env=env,
        )
    except FileNotFoundError:
        print(f"[error] Python not found: {python_cmd}", file=sys.stderr)
        raise
    if p.returncode != 0:
        print(f"[warn] {script.stem}: exit {p.returncode}", file=sys.stderr)
        if p.stderr.strip():
            print(f"  stderr: {p.stderr.strip()[-200:]}", file=sys.stderr)
    result = parse_runtime(p.stdout)
    if result is None:
        result = parse_runtime(p.stderr)
    return result


def find_benchmark_scripts(d: Path) -> List[str]:
    """Return sorted list of Name_N stems for .py files in a single directory."""
    stems = sorted(
        p.stem
        for p in d.iterdir()
        if p.suffix == ".py" and p.is_file() and STEM_RE.match(p.stem)
    )
    if not stems:
        raise SystemExit(f"No benchmark scripts found in {d}")
    return stems


def run_pandas(args: argparse.Namespace) -> None:
    """Run pandas benchmarks and collect percentage speedups from stdout."""
    stems = find_benchmark_scripts(args.pandas_dir)
    if args.file_regex:
        rx = re.compile(args.file_regex)
        stems = [s for s in stems if rx.search(s)]
    if not stems:
        print("No matching scripts after filtering.", file=sys.stderr)
        sys.exit(1)

    completed = _load_completed_speedups(args.results)
    remaining = len([s for s in stems if s not in completed])
    print(
        f"Writing results incrementally to {args.results} ({len(completed)}/{len(stems)} already completed, {remaining} remaining) ..."
    )
    n_runs = max(args.runs, 2)  # at least 1 warmup + 1 measured
    print(
        f"Pandas benchmark: {len(stems)} scripts x {n_runs} rounds "
        f"(1 warmup + {n_runs - 1} measured)\n"
    )

    for stem in stems:
        if stem in completed:
            print(f"  Skipping {stem} (already completed)")
            continue

        script = args.pandas_dir / f"{stem}.py"
        pct_values: List[float] = []

        for rnd in range(n_runs):
            is_warmup = rnd == 0
            pct = _run_pandas_script(args.python, script)

            tag = " (warmup)" if is_warmup else ""
            disp = f"{pct:.2f}%" if pct is not None else "NA"
            print(f"  {stem} [round {rnd + 1}/{n_runs}]{tag}: {disp}")

            if not is_warmup and pct is not None:
                pct_values.append(pct)

        if not pct_values:
            print(f"[warn] {stem}: missing data, recording NA", file=sys.stderr)
            _append_speedup_row(stem, "NA", args.results)
        else:
            med_pct = _median(pct_values)
            _append_speedup_row(stem, f"{med_pct:.2f}%", args.results)
            print(f"  {stem} => speedup={med_pct:.2f}%\n")

    _print_speedup_summary(args.results)


# -- Spark runner helpers ---------------------------------------------------


def _build_jar(project_dir: Path, skip: bool) -> Path:
    jar_dir = project_dir / "target" / "scala-2.13"
    jar_pattern = (
        "*enchmarks*.jar"  # case-insensitive match for SparkBenchmarks/sparkbenchmarks
    )

    def _find_jar():
        jars = list(jar_dir.glob(jar_pattern))
        return jars[0] if jars else None

    if skip:
        jar = _find_jar()
        if not jar:
            raise SystemExit(f"JAR not found (--skip-build) in {jar_dir}")
        return jar
    print(f"  Building JAR in {project_dir.name}/ ...")
    ret = subprocess.call(
        ["sbt", "-batch", "-error", "--supershell=false", "package"],
        cwd=project_dir,
    )
    if ret != 0:
        raise SystemExit(f"sbt package failed in {project_dir}")
    jar = _find_jar()
    if not jar:
        raise SystemExit(f"JAR not found after build in {jar_dir}")
    return jar


def _spark_submit(
    class_name: str,
    jar: Path,
    master: str,
    driver_memory: str,
    conf: dict,
    cwd: Optional[Path] = None,
) -> Optional[float]:
    cmd = [
        "spark-submit",
        "--class",
        class_name,
        "--master",
        master,
        "--driver-memory",
        driver_memory,
    ]
    for k, v in conf.items():
        cmd += ["--conf", f"{k}={v}"]
    cmd.append(str(jar))
    env = {**os.environ, "SPARK_LOCAL_IP": "127.0.0.1"}
    try:
        p = subprocess.run(
            cmd,
            text=True,
            capture_output=True,
            check=False,
            env=env,
            cwd=str(cwd) if cwd else None,
        )
    except FileNotFoundError:
        print("[error] spark-submit not found on PATH", file=sys.stderr)
        raise
    if p.returncode != 0:
        print(f"[warn] {class_name}: spark-submit exit {p.returncode}", file=sys.stderr)
        if p.stderr.strip():
            # Print last 300 chars of stderr for diagnostics
            print(f"  stderr: ...{p.stderr.strip()[-300:]}", file=sys.stderr)
    result = parse_runtime(p.stdout)
    if result is None:
        result = parse_runtime(p.stderr)
    return result


def find_benchmark_classes(d: Path) -> List[str]:
    """Return sorted list of Name_N stems for .scala files in a single directory."""
    # Look in src/main/scala/ subdirectory (standard sbt layout)
    src_dir = d / "src" / "main" / "scala"
    search_dir = src_dir if src_dir.is_dir() else d
    classes = sorted(
        p.stem
        for p in search_dir.iterdir()
        if p.suffix == ".scala" and p.is_file() and CLASS_STEM_RE.match(p.stem)
    )
    if not classes:
        raise SystemExit(f"No benchmark classes found in {search_dir}")
    return classes


def run_spark(args: argparse.Namespace) -> None:
    """Run Spark benchmarks and collect percentage speedups from stdout."""
    # Build JAR
    print("Building JAR ...")
    jar = _build_jar(args.spark_dir, args.skip_build)

    # Discover classes
    classes = find_benchmark_classes(args.spark_dir)
    if args.class_regex:
        rx = re.compile(args.class_regex)
        classes = [c for c in classes if rx.search(c)]
    if not classes:
        print("No matching classes after filtering.", file=sys.stderr)
        sys.exit(1)

    # Merge conf
    conf = dict(DEFAULT_SPARK_CONF)
    for kv in args.extra_conf:
        if "=" not in kv:
            raise SystemExit(f"--extra-conf must be key=value, got: {kv}")
        k, v = kv.split("=", 1)
        conf[k.strip()] = v.strip()

    # Run benchmarks
    completed = _load_completed_speedups(args.results)
    remaining = len([c for c in classes if c not in completed])
    print(
        f"Writing results incrementally to {args.results} ({len(completed)}/{len(classes)} already completed, {remaining} remaining) ..."
    )
    n_runs = max(args.runs, 2)  # at least 1 warmup + 1 measured
    print(
        f"\nSpark benchmark: {len(classes)} classes x {n_runs} rounds "
        f"(1 warmup + {n_runs - 1} measured)\n"
    )

    for cls in classes:
        if cls in completed:
            print(f"  Skipping {cls} (already completed)")
            continue

        pct_values: List[float] = []

        for rnd in range(n_runs):
            is_warmup = rnd == 0
            pct = _spark_submit(
                cls, jar, args.master, args.driver_memory, conf, cwd=args.spark_dir
            )

            tag = " (warmup)" if is_warmup else ""
            disp = f"{pct:.2f}%" if pct is not None else "NA"
            print(f"  {cls} [round {rnd + 1}/{n_runs}]{tag}: {disp}")

            if not is_warmup and pct is not None:
                pct_values.append(pct)

        if not pct_values:
            print(f"[warn] {cls}: missing data, recording NA", file=sys.stderr)
            _append_speedup_row(cls, "NA", args.results)
        else:
            med_pct = _median(pct_values)
            _append_speedup_row(cls, f"{med_pct:.2f}%", args.results)
            print(f"  {cls} => speedup={med_pct:.2f}%\n")

    _print_speedup_summary(args.results)


# ---------------------------------------------------------------------------
# run-synth: run pusharoo.exe on every benchmark in a directory (from bench.py)
# ---------------------------------------------------------------------------


_SYNTH_COLUMNS = [
    "benchmark",
    "status",
    "kind",
    "runtime",
    "|Q*|",
    "|P*|",
    "|BI|",
    "|P|",
    "|DP|",
    "num_pairs",
    "magicpush_exact",
    "magicpush_partial",
]


def _load_completed_synth(output_csv: str) -> set:
    """Return set of benchmark names already present in the synth results CSV."""
    if not os.path.exists(output_csv):
        return set()
    completed = set()
    try:
        df = pd.read_csv(output_csv)
        completed = set(df["benchmark"].tolist())
    except Exception:
        pass
    return completed


def _append_synth_row(rec: dict, output_csv: str) -> None:
    """Append a single result row to the synth CSV, writing the header if needed."""
    write_header = not os.path.exists(output_csv) or os.path.getsize(output_csv) == 0
    with open(output_csv, "a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=_SYNTH_COLUMNS)
        if write_header:
            w.writeheader()
        w.writerow(rec)


def run_pusharoo_on_directory(
    bench_dir, timeout, mode, results_dir, out, expand=None, save_qp=None
):
    """Run pusharoo.exe on every .py file in *bench_dir* and write a results CSV."""
    exe = str(
        (Path(__file__).resolve().parent / "synthesizer" / "pusharoo.exe").resolve()
    )
    output_csv = str(Path(results_dir) / f"{out}.csv")

    # Load already-completed benchmarks for resumability
    completed = _load_completed_synth(output_csv)
    total_files = sum(
        1 for e in os.listdir(bench_dir) if os.path.isfile(os.path.join(bench_dir, e))
    )
    remaining = total_files - len(completed)
    print(
        f"Writing results incrementally to {output_csv} ({len(completed)}/{total_files} already completed, {remaining} remaining) ..."
    )

    for entry in sorted(os.listdir(bench_dir)):
        file_path = os.path.join(bench_dir, entry)
        if not os.path.isfile(file_path):
            continue

        # Check the benchmark name (strip .py extension to match CSV format)
        bench_name = os.path.splitext(entry)[0] if entry.endswith(".py") else entry
        if bench_name in completed or entry in completed:
            print(f"  Skipping {entry} (already completed)")
            continue

        cmd = [exe, file_path, "--timeout", timeout, "--mode", mode]
        if expand is not None:
            cmd += ["--expand", expand]
        if save_qp is not None:
            cmd += ["--save-qp", save_qp]

        try:
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        except subprocess.CalledProcessError as e:
            print(f"Error running pusharoo on {entry}: {e.stderr.strip()}")
            continue

        line = result.stdout.strip()

        if not line:
            print(f"No output for {entry}, marking as none.")
            status = "none"
            parts = [entry, status]
        else:
            parts = [p.strip() for p in line.split(",")]

            if parts[1] in ("unknown", "none", "timeout"):
                status = parts[1]
            elif len(parts) == 11:
                status = "ok"
            else:
                print(f"Unexpected output format for {entry}: {line}")
                continue

        rec = {
            "benchmark": parts[0],
            "status": status,
            "kind": None,
            "runtime": None,
            "|Q*|": None,
            "|P*|": None,
            "|BI|": None,
            "|P|": None,
            "|DP|": None,
            "num_pairs": None,
            "magicpush_exact": None,
            "magicpush_partial": None,
        }

        if status == "ok":
            rec.update(
                {
                    "kind": int(parts[1]),
                    "runtime": float(parts[2]),
                    "|Q*|": int(parts[3]),
                    "|P*|": int(parts[4]),
                    "|BI|": int(parts[5]),
                    "|P|": int(parts[6]),
                    "|DP|": int(parts[7]),
                    "num_pairs": int(parts[8]),
                    "magicpush_exact": parts[9],
                    "magicpush_partial": parts[10],
                }
            )

        print(line)
        _append_synth_row(rec, output_csv)

    # Read back the full CSV for summary statistics
    df = pd.read_csv(output_csv)
    print(f"Results file: {output_csv}")

    num_unknown = (df["status"] == "unknown").sum()
    num_none = (df["status"] == "none").sum()
    num_timeout = (df["status"] == "timeout").sum()
    num_ok = (df["status"] == "ok").sum()
    print(f"# benchmarks: {len(df)}")
    print(f"# successful: {num_ok}")
    print(f"# timeouts: {num_timeout}")
    print(f"# no solution: {num_none}")
    print(f"# unknown: {num_unknown}")

    df_ok = df[df["status"] == "ok"]

    if mode == "pusharoo":
        k4 = df_ok[df_ok["kind"] == 4]
        assert len(k4) == 0, (
            f"Unexpected kind=4 in Pusharoo mode for: "
            f"{', '.join(k4['benchmark'].tolist())}"
        )
        df.loc[df["kind"] == 5, "kind"] = 3
        # Normalize by subtracting the early-halting check pair from num_pairs,
        # as it is irrelevant to the search space reduction experiment.
        df.loc[df["num_pairs"].notna(), "num_pairs"] = (
            df.loc[df["num_pairs"].notna(), "num_pairs"] - 1
        ).clip(lower=0)

    runtimes = df_ok["runtime"]
    print("\n=== Runtime statistics (ok only) ===")
    print(f"Avg runtime:   {runtimes.mean():.2f}s")
    print(f"Min runtime:   {runtimes.min():.2f}s")
    print(f"Max runtime:   {runtimes.max():.2f}s")
    print(f"Total runtime: {runtimes.sum():.2f}s")

    avg_qstar = df_ok["|Q*|"].mean()
    avg_pstar = df_ok["|P*|"].mean()
    avg_bi = df_ok["|BI|"].mean()

    df_k3 = df_ok[df_ok["kind"].isin([3, 4, 5])]
    avg_d_p = df_k3["|P|"].mean()
    avg_dp = df_k3["|DP|"].mean()

    print(f"Avg |Q*|: {avg_qstar:.2f}")
    print(f"Avg |P*|: {avg_pstar:.2f}")
    print(f"Avg |BI|: {avg_bi:.2f}")
    print(f"Avg |P| (weaker): {avg_d_p:.2f}")
    print(f"Avg |DP| (weaker): {avg_dp:.2f}")


# ---------------------------------------------------------------------------
# ablcomp: compare solution quality between Pusharoo and CHC solvers
# ---------------------------------------------------------------------------


def _ablcomp_parse_row(row: List[str]) -> Tuple[str, Optional[str], Optional[str]]:
    """Return (benchmark, p_val, sentinel).

    benchmark : COL1 stripped.
    p_val     : COL3 (stripped) if row is *normal*, else None.
    sentinel  : 'none' | 'timeout' if present, else None.
    """
    if len(row) < 2:
        raise ValueError(f"Row {row!r} has fewer than two columns.")

    benchmark = row[0].strip()
    col2 = row[1].strip().lower()

    if col2 in {"none", "timeout"}:
        return benchmark, None, col2

    if len(row) < 3:
        raise ValueError(
            f"Row for benchmark {benchmark!r} lacks a third column (COL3)."
        )

    return benchmark, row[2].strip(), None  # normal row


def _ablcomp_run(exe: Path, bench_py: Path, p1: str, p2: str, chc_mode: str) -> str:
    """Return ablcomp.exe's single-line stdout (stripped)."""
    cmd: List[str] = [
        str(exe),
        "--chc-mode",
        chc_mode,
        "-p1",
        p1,
        "-p2",
        p2,
        "-i",
        str(bench_py),
    ]
    completed = subprocess.run(cmd, check=True, text=True, capture_output=True)
    return completed.stdout.strip()


def _ablcomp_load_csv(path: Path) -> List[List[str]]:
    """Read a header-less CSV into a list of raw rows."""
    return list(csv.reader(open(path, newline="", encoding="utf-8")))


def _ablcomp_process_mode(
    exe: Path,
    bench_dir: Path,
    csv1_rows: List[List[str]],
    csv2_rows: List[List[str]],
    chc_mode: str,
) -> List[str]:
    """Execute one mode (spacer or eld) and return comp values aligned with csv1_rows."""
    comps: List[str] = []

    for idx, (row1, row2) in enumerate(zip(csv1_rows, csv2_rows), 1):
        b1, p1, s1 = _ablcomp_parse_row(row1)
        b2, p2, s2 = _ablcomp_parse_row(row2)

        if b1 != b2:
            sys.exit(f"Error: benchmark mismatch at row {idx}: {b1!r} vs {b2!r}")
        benchmark = b1

        # Sentinel handling
        if s1 or s2:
            comps.append("timeout" if "timeout" in (s1, s2) else "none")
            continue

        # Normal row: run ablcomp
        bench_file = bench_dir / f"{benchmark}.py"
        try:
            full_line = _ablcomp_run(exe, bench_file, p1, p2, chc_mode)
            try:
                _, comp_field = full_line.split(",", 1)
            except ValueError:
                raise RuntimeError(
                    f"Unexpected ablcomp output for {benchmark!r}: {full_line!r}"
                )
            comps.append(comp_field)
        except Exception as e:
            sys.stderr.write(f"{chc_mode}: failure on {benchmark}: {e}\n")
            comps.append("error")

    return comps


def run_ablcomp(results_dir: Path, bench_dir: Path) -> None:
    """Compare solution quality between Pusharoo and CHC solvers."""
    base_dir = Path(__file__).resolve().parent
    exe = (base_dir / "synthesizer" / "ablcomp.exe").resolve()

    push_path = results_dir / "comp_pusharoo_QP.csv"
    spacer_path = results_dir / "comp_spacer_QP.csv"
    eld_path = results_dir / "comp_eld_QP.csv"
    out_path = results_dir / "ablcomp.csv"
    print(f"Writing results to {out_path} ...")

    # Load CSVs
    push_rows = _ablcomp_load_csv(push_path)
    spacer_rows = _ablcomp_load_csv(spacer_path)
    eld_rows = _ablcomp_load_csv(eld_path)

    n_rows = len(push_rows)
    if len(spacer_rows) != n_rows or len(eld_rows) != n_rows:
        sys.exit(
            "Error: input CSVs do not have the same number of rows:\n"
            f"  pusharoo: {n_rows}\n"
            f"  spacer  : {len(spacer_rows)}\n"
            f"  eld     : {len(eld_rows)}"
        )

    # Run both modes
    comp_spacer = _ablcomp_process_mode(
        exe, bench_dir, push_rows, spacer_rows, "spacer"
    )
    comp_eld = _ablcomp_process_mode(exe, bench_dir, push_rows, eld_rows, "eld")

    # Write merged output
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", encoding="utf-8", newline="") as fp_out:
        fp_out.write("benchmark,comp_spacer,comp_eld\n")
        for row_push, cs, ce in zip(push_rows, comp_spacer, comp_eld):
            benchmark = row_push[0].strip()
            fp_out.write(f"{benchmark},{cs},{ce}\n")

    # Count results
    spacer_twos = sum(1 for x in comp_spacer if x == "2")
    eld_twos = sum(1 for x in comp_eld if x == "2")

    spacer_valid = sum(1 for x in comp_spacer if x not in {"none", "timeout"})
    eld_valid = sum(1 for x in comp_eld if x not in {"none", "timeout"})

    print(
        f"comp_spacer: {spacer_twos} occurrences of 2, "
        f"{spacer_valid} non-sentinel values"
    )
    print(f"comp_eld   : {eld_twos} occurrences of 2, {eld_valid} non-sentinel values")


# ---------------------------------------------------------------------------
# udf-props: collect UDF boolean properties for Table 1 (from props.py)
# ---------------------------------------------------------------------------


def compute_udf_props(bench_dir: str) -> None:
    """For each unique prefix, run pusharoo.exe --props and write udf_props.csv."""
    exe_path = str(
        (Path(__file__).resolve().parent / "synthesizer" / "pusharoo.exe").resolve()
    )
    results_dir = Path(__file__).resolve().parent / "results"
    output_csv = results_dir / "udf_props.csv"

    # from manual inspection of the original source code
    _HAS_COLLECTION = {
        "Ada1",
        "Basket",
        "Flaredown1",
        "MinPooling",
        "PreprocCSVTable",
        "RoaringBitmapUDAF",
        "Top2",
        "UDAF2",
    }

    bool_names = [
        "has_conditional",
        "has_tuple_accum",
        "has_indep",
        "has_crossdep",
        "has_collection",
    ]

    rows = []  # (prefix, b1, b2, b3, b4, b5)
    for fname in sorted(os.listdir(bench_dir)):
        full = os.path.join(bench_dir, fname)
        if not os.path.isfile(full) or "_" not in fname:
            continue

        prefix = fname.split("_", 1)[0]
        if any(r[0] == prefix for r in rows):
            continue  # already processed

        try:
            proc = subprocess.run(
                [exe_path, "--udf-props", full],
                capture_output=True,
                text=True,
                check=True,
            )
        except subprocess.CalledProcessError as e:
            print(f"Error running on {fname}: {e.stderr.strip()}")
            continue

        parts = proc.stdout.strip().split(",")
        if len(parts) < 5:
            print(f"Unexpected output for {fname}: {proc.stdout.strip()!r}")
            continue

        bools = [parts[i].strip().lower() == "true" for i in range(1, 5)]
        bools.append(prefix in _HAS_COLLECTION)
        rows.append((prefix, *bools))

    # Write udf_props.csv
    print(f"Writing results to {output_csv} ...")
    with open(output_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["prefix"] + bool_names)
        for row in rows:
            w.writerow([row[0]] + [str(b).lower() for b in row[1:]])

    print(f"Processed {len(rows)} UDFs; results written to {output_csv}")
    print("Statistics:")
    for name in bool_names:
        count = sum(1 for row in rows if row[1 + bool_names.index(name)])
        print(f"  {name}: {count}")


# ---------------------------------------------------------------------------
# ast-sizes: compute AST size statistics for Table 1 (from size.py)
# ---------------------------------------------------------------------------


def compute_ast_sizes(bench_dir: str, results_dir: Path) -> None:
    """For each benchmark .py file, run pusharoo.exe --ast-size and collect sizes."""
    exe_path = str(
        (Path(__file__).resolve().parent / "synthesizer" / "pusharoo.exe").resolve()
    )
    output_csv = str(results_dir / "ast_sizes.csv")
    print(f"Writing results to {output_csv} ...")

    sizes = []  # list of integer AST sizes, one per benchmark file

    for fname in sorted(os.listdir(bench_dir)):
        full_path = os.path.join(bench_dir, fname)
        if not os.path.isfile(full_path) or "_" not in fname:
            continue

        try:
            proc = subprocess.run(
                [exe_path, "--ast-size", full_path],
                capture_output=True,
                text=True,
                check=True,
            )
        except subprocess.CalledProcessError as e:
            print(f"Error on {fname}: {e.stderr.strip()}")
            continue

        line = proc.stdout.strip()
        parts = line.split(",")
        if len(parts) < 2:
            print(f"Unexpected output for {fname}: {line!r}")
            continue

        try:
            value = int(parts[1])
        except ValueError:
            print(f"Non-integer value for {fname}: {parts[1]!r}")
            continue

        sizes.append(value)

    # ensure output directory exists
    os.makedirs(os.path.dirname(output_csv), exist_ok=True)

    # write to CSV: single column "size", one row per benchmark
    with open(output_csv, "w", newline="") as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(["size"])
        for size in sizes:
            writer.writerow([size])

    if not sizes:
        print("No valid records collected.")
        return

    mn = min(sizes)
    mx = max(sizes)
    avg = _mean(sizes)
    med = _median(sizes)

    print(f"Processed {len(sizes)} benchmarks; results written to {output_csv}")
    print("Statistics for 'size':")
    print(f"  Min   : {mn}")
    print(f"  Max   : {mx}")
    print(f"  Avg   : {avg:.2f}")
    print(f"  Median: {med}")


class ResultsAnalyzer:
    """Analyzer for benchmark results.

    This class provides extensible analysis capabilities for benchmark results.
    To add new analyses, simply add new methods and call them from main().

    Example extension:
        def analyze_timeout_rate(self, files: List[str]) -> pd.DataFrame:
            '''Compute timeout rates across different configurations.'''
            stats = []
            for filename in files:
                df = self.load_csv(filename)
                total = len(df)
                timeouts = len(df[df['status'] == 'timeout'])
                stats.append({
                    'File': filename,
                    'Timeout Rate': timeouts / total * 100
                })
            return pd.DataFrame(stats)
    """

    def __init__(self, results_dir: Path):
        """Initialize the analyzer with a results directory.

        Args:
            results_dir: Path to the directory containing results CSV files
        """
        self.results_dir = Path(results_dir)

    def load_csv(self, filename: str) -> pd.DataFrame:
        """Load a CSV file from the results directory.

        Args:
            filename: Name of the CSV file to load

        Returns:
            DataFrame containing the CSV data
        """
        filepath = self.results_dir / filename
        if not filepath.exists():
            raise FileNotFoundError(f"File not found: {filepath}")
        return pd.read_csv(filepath)

    def compute_speedup_stats_by_type(
        self,
        speedups_file: str = "results_speedups.csv",
        status_file: str = "results.csv",
    ) -> pd.DataFrame:
        """Compute median, average, and max speedups by pushdown type.

        Args:
            speedups_file: Name of the speedups CSV file
            status_files: Dictionary mapping status file labels to filenames.
                         If None, uses default files.

        Returns:
            DataFrame with speedup statistics by type
        """

        speedups = self.load_csv(speedups_file)
        status_df = self.load_csv(status_file)

        merged = speedups.merge(
            status_df[["benchmark", "status", "kind"]], on="benchmark"
        )

        # Convert speedup percentage to float (remove % and convert)
        merged["speedup_val"] = merged["speedup"].str.rstrip("%").astype(float)

        # Group by pushdown type
        type_mapping = {1: "Exact", 2: "Partial", 3: "Split", 5: "Split"}
        merged["type_category"] = merged["kind"].map(type_mapping)

        # Compute statistics for each type
        stats = []
        for type_name in ["Exact", "Partial", "Split"]:
            type_data = merged[merged["type_category"] == type_name]["speedup_val"]
            if len(type_data) > 0:
                stats.append(
                    {
                        "Type": type_name,
                        "Min": type_data.min(),
                        "Median": type_data.median(),
                        "Avg": type_data.mean(),
                        "Max": type_data.max(),
                    }
                )
            else:
                stats.append(
                    {
                        "Type": type_name,
                        "Min": np.nan,
                        "Median": np.nan,
                        "Avg": np.nan,
                        "Max": np.nan,
                    }
                )

        # Compute overall statistics across all types
        if len(merged) > 0:
            stats.append(
                {
                    "Type": "Overall",
                    "Min": merged["speedup_val"].min(),
                    "Median": merged["speedup_val"].median(),
                    "Avg": merged["speedup_val"].mean(),
                    "Max": merged["speedup_val"].max(),
                }
            )
        else:
            stats.append(
                {
                    "Type": "Overall",
                    "Min": np.nan,
                    "Median": np.nan,
                    "Avg": np.nan,
                    "Max": np.nan,
                }
            )

        return pd.DataFrame(stats)

    def generate_latex_table(self, stats_df: pd.DataFrame) -> str:
        """Generate LaTeX table from statistics DataFrame.

        Args:
            stats_df: DataFrame with columns Type, Median, Avg, Max

        Returns:
            LaTeX table as string
        """
        lines = []
        lines.append("\\begin{tabular}{lcccc}")
        lines.append("\\toprule")
        lines.append(
            "\\textbf{Type} & \\textbf{Min} & \\textbf{Avg} & \\textbf{Median} & \\textbf{Max} \\\\"
        )
        lines.append("\\midrule")

        for _, row in stats_df.iterrows():
            type_name = row["Type"]

            # Add midrule before Overall row
            if type_name == "Overall":
                lines.append("\\midrule")

            min = f"{row['Min']:.1f}\\%" if not pd.isna(row["Min"]) else "--"
            avg = f"{row['Avg']:.1f}\\%" if not pd.isna(row["Avg"]) else "--"
            median = f"{row['Median']:.1f}\\%" if not pd.isna(row["Median"]) else "--"
            max_val = f"{row['Max']:.1f}\\%" if not pd.isna(row["Max"]) else "--"

            lines.append(
                f"{type_name:7s} & {min:>6s} & {avg:>6s} & {median:>6s} & {max_val:>6s} \\\\"
            )

        lines.append("\\bottomrule")
        lines.append("\\end{tabular}")

        return "\n".join(lines)

    def compute_search_space_reduction_by_type(
        self, baseline_file: str, comparison_file: str
    ) -> Dict[str, Tuple[float, float, float]]:
        """Compute search space reduction statistics by pushdown type.

        Args:
            baseline_file: Baseline results file
            comparison_file: Comparison results file
            num_pairs_col: Column name for number of pairs

        Returns:
            Dictionary mapping type names to (median, avg, max) tuples
        """
        num_pairs_col = "num_pairs"

        baseline = self.load_csv(baseline_file)
        comparison = self.load_csv(comparison_file)

        baseline = baseline[baseline["status"] == "ok"]
        comparison = comparison[comparison["status"] == "ok"]

        merged = baseline[["benchmark", "kind", num_pairs_col]].merge(
            comparison[["benchmark", "kind", num_pairs_col]],
            on=["benchmark", "kind"],
            suffixes=("_baseline", "_comparison"),
        )

        # Exclude rows where baseline has 0 pairs (division by zero)
        merged = merged[merged[f"{num_pairs_col}_baseline"] != 0]

        print(f"Analyzing {len(merged)} benchmarks for search space reduction")

        merged["reduction_pct"] = (
            (
                merged[f"{num_pairs_col}_baseline"]
                - merged[f"{num_pairs_col}_comparison"]
            )
            / merged[f"{num_pairs_col}_baseline"]
            * 100
        )

        type_mapping = {1: "Exact", 2: "Partial", 3: "Split", 5: "Split"}
        merged["type_category"] = merged["kind"].map(type_mapping)

        # Compute statistics for each type
        results = {}
        for type_name in ["Exact", "Partial", "Split"]:
            type_data = merged[merged["type_category"] == type_name]["reduction_pct"]
            if len(type_data) > 0:
                results[type_name] = (
                    type_data.median(),
                    type_data.mean(),
                    type_data.max(),
                )
            else:
                results[type_name] = (np.nan, np.nan, np.nan)

        # Compute overall statistics
        if len(merged) > 0:
            results["Overall"] = (
                merged["reduction_pct"].median(),
                merged["reduction_pct"].mean(),
                merged["reduction_pct"].max(),
            )
        else:
            results["Overall"] = (np.nan, np.nan, np.nan)

        return results

    def analyze_search_space_reductions(
        self, target_file: str = "results.csv", baselines: Dict[str, str] = None
    ) -> pd.DataFrame:
        """Analyze search space reductions against multiple baselines.

        Args:
            target_file: Target results file to compare against baselines
            baselines: Dictionary mapping baseline names to filenames

        Returns:
            DataFrame with reduction statistics for each baseline and type
        """
        if baselines is None:
            baselines = {
                "NoBounds": "results_nobounds.csv",
                "NoRepair": "results_noanalysis.csv",
                "NoBoundsNoRepair": "results_nobounds_noanalysis.csv",
                "TwoPhase": "results_twophase.csv",
            }

        # Collect statistics for each baseline
        all_stats = {}
        for baseline_name, baseline_file in baselines.items():
            try:
                stats = self.compute_search_space_reduction_by_type(
                    baseline_file, target_file
                )
                all_stats[baseline_name] = stats
            except (FileNotFoundError, ValueError) as e:
                print(f"Warning: Could not process {baseline_name}: {e}")
                # Note: NoBounds baseline doesn't track num_pairs, so this is expected
                all_stats[baseline_name] = {
                    "Exact": (np.nan, np.nan, np.nan),
                    "Partial": (np.nan, np.nan, np.nan),
                    "Split": (np.nan, np.nan, np.nan),
                    "Overall": (np.nan, np.nan, np.nan),
                }

        # Convert to DataFrame format
        rows = []
        for type_name in ["Exact", "Partial", "Split", "Overall"]:
            row = {"Type": type_name}
            for baseline_name in baselines.keys():
                if baseline_name in all_stats:
                    median, avg, max_val = all_stats[baseline_name][type_name]
                    row[f"{baseline_name}_Median"] = median
                    row[f"{baseline_name}_Avg"] = avg
                    row[f"{baseline_name}_Max"] = max_val
                else:
                    row[f"{baseline_name}_Median"] = np.nan
                    row[f"{baseline_name}_Avg"] = np.nan
                    row[f"{baseline_name}_Max"] = np.nan
            rows.append(row)

        return pd.DataFrame(rows)

    def generate_search_space_latex_table(self, stats_df: pd.DataFrame) -> str:
        """Generate LaTeX table for search space reduction statistics.

        Args:
            stats_df: DataFrame with columns Type, and for each baseline: Median, Avg, Max

        Returns:
            LaTeX table as string
        """
        # Get baseline names (extract from column names)
        baseline_cols = [
            col.replace("_Median", "")
            for col in stats_df.columns
            if col.endswith("_Median")
        ]

        lines = []

        # Build header with baseline names
        header_parts = ["\\textbf{Type}"]
        for baseline in baseline_cols:
            header_parts.append(f"\\textsc{{{baseline}}}")

        num_cols = len(header_parts)
        lines.append(f"\\begin{{tabular}}{{{'l' + 'c' * (num_cols - 1)}}}")
        lines.append("\\toprule")
        lines.append(" & ".join(header_parts) + " \\\\")
        lines.append("\\midrule")

        # Add data rows (Exact, Partial, Split)
        for i, row in stats_df.iterrows():
            type_name = row["Type"]

            # Skip Overall for now, will add after midrule
            if type_name == "Overall":
                continue

            parts = [f"{type_name:7s}"]
            for baseline in baseline_cols:
                median = row[f"{baseline}_Median"]
                avg = row[f"{baseline}_Avg"]
                max_val = row[f"{baseline}_Max"]

                if pd.isna(median):
                    parts.append("-- / -- / --")
                else:
                    parts.append(f"{median:.1f} / {avg:.1f} / {max_val:.1f}")

            lines.append(" & ".join(parts) + " \\\\")

        # Add midrule before Overall
        lines.append("\\midrule")

        # Add Overall row
        overall_row = stats_df[stats_df["Type"] == "Overall"].iloc[0]
        parts = ["Overall"]
        for baseline in baseline_cols:
            median = overall_row[f"{baseline}_Median"]
            avg = overall_row[f"{baseline}_Avg"]
            max_val = overall_row[f"{baseline}_Max"]

            if pd.isna(median):
                parts.append("-- / -- / --")
            else:
                parts.append(f"{median:.1f} / {avg:.1f} / {max_val:.1f}")

        lines.append(" & ".join(parts) + " \\\\")

        lines.append("\\bottomrule")
        lines.append("\\end{tabular}")

        return "\n".join(lines)

    def compute_benchmark_classification(
        self, results_file: str = "results.csv"
    ) -> pd.DataFrame:
        """Compute benchmark classification statistics.

        Classifies benchmarks by origin (pandas vs Spark) and computes statistics
        for Pusharoo (by kind) and MagicPush (by magicpush_exact/partial flags).

        Args:
            results_file: Results file to analyze

        Returns:
            DataFrame with classification statistics
        """
        df = self.load_csv(results_file)

        # Filter only successful runs
        df = df[df["status"] == "ok"]

        # Define pandas benchmark prefixes
        pandas_prefixes = [
            "20Nov",
            "Ada1",
            "Ada3",
            "Basket",
            "Chicago",
            "Flaredown",
            "Jdata",
        ]

        # Classify benchmarks
        def is_pandas(benchmark_name):
            return any(benchmark_name.startswith(prefix) for prefix in pandas_prefixes)

        df["origin"] = df["benchmark"].apply(
            lambda x: "pandas" if is_pandas(x) else "Spark"
        )

        results = []

        for origin in ["pandas", "Spark"]:
            origin_df = df[df["origin"] == origin]
            total = len(origin_df)

            if total == 0:
                continue

            # Pusharoo statistics (by kind)
            exact_count = len(origin_df[origin_df["kind"] == 1])
            partial_count = len(origin_df[origin_df["kind"] == 2])
            split_count = len(origin_df[origin_df["kind"].isin([3, 5])])
            any_pusharoo = exact_count + partial_count + split_count

            # MagicPush statistics
            # Exact: both true AND kind=1 (only true exact pushdown)
            mp_exact = origin_df[
                (origin_df["magicpush_exact"] == True)
                & (origin_df["magicpush_partial"] == True)
                & (origin_df["kind"] == 1)
            ]
            mp_exact_count = len(mp_exact)

            # Partial: (false, true) OR (true, true with kind=2 or 3), excluding MinPooling,
            # which is unsupported by MagicPush's DSL
            mp_partial = origin_df[
                (
                    (
                        (origin_df["magicpush_exact"] == False)
                        & (origin_df["magicpush_partial"] == True)
                    )
                    | (
                        (origin_df["magicpush_exact"] == True)
                        & (origin_df["magicpush_partial"] == True)
                        & (origin_df["kind"].isin([2, 3]))
                    )
                )
                & (~origin_df["benchmark"].str.startswith("MinPooling"))
            ]
            mp_partial_count = len(mp_partial)

            # None: false, false
            mp_none = origin_df[
                (origin_df["magicpush_exact"] == False)
                & (origin_df["magicpush_partial"] == False)
            ]
            mp_none_count = len(mp_none)

            # Any MagicPush (exact or partial)
            mp_any_count = mp_exact_count + mp_partial_count

            results.append(
                {
                    "Origin": origin,
                    "Total": total,
                    "Pusharoo_Any": any_pusharoo,
                    "Pusharoo_Exact": exact_count,
                    "Pusharoo_Partial": partial_count,
                    "Pusharoo_Split": split_count,
                    "MagicPush_Any": mp_any_count,
                    "MagicPush_Exact": mp_exact_count,
                    "MagicPush_Partial": mp_partial_count,
                    "MagicPush_None": mp_none_count,
                }
            )

        # Add overall row
        total_all = len(df)
        exact_all = len(df[df["kind"] == 1])
        partial_all = len(df[df["kind"] == 2])
        split_all = len(df[df["kind"].isin([3, 5])])
        any_all = exact_all + partial_all + split_all

        # Count rows with true,true and kind=2 or 3 (to be moved from exact to partial)
        true_true_partial_split = df[
            (df["magicpush_exact"] == True)
            & (df["magicpush_partial"] == True)
            & (df["kind"].isin([2, 3]))
        ]
        true_true_partial_split_count = len(true_true_partial_split)

        mp_exact_all = len(
            df[
                (df["magicpush_exact"] == True)
                & (df["magicpush_partial"] == True)
                & (df["kind"] == 1)
            ]
        )
        mp_partial_all = len(
            df[
                (
                    (
                        (df["magicpush_exact"] == False)
                        & (df["magicpush_partial"] == True)
                    )
                    | (
                        (df["magicpush_exact"] == True)
                        & (df["magicpush_partial"] == True)
                        & (df["kind"].isin([2, 3]))
                    )
                )
                & (~df["benchmark"].str.startswith("MinPooling"))
            ]
        )
        mp_none_all = len(
            df[(df["magicpush_exact"] == False) & (df["magicpush_partial"] == False)]
        )
        mp_any_all = mp_exact_all + mp_partial_all

        # Compute additional statistic: percentage of MagicPush Partial cases with kind=1 or 3
        mp_partial_df = df[
            (
                ((df["magicpush_exact"] == False) & (df["magicpush_partial"] == True))
                | (
                    (df["magicpush_exact"] == True)
                    & (df["magicpush_partial"] == True)
                    & (df["kind"].isin([2, 3]))
                )
            )
            & (~df["benchmark"].str.startswith("MinPooling"))
        ]
        mp_partial_exact_or_split = len(
            mp_partial_df[mp_partial_df["kind"].isin([1, 3])]
        )
        mp_partial_exact_or_split_pct = (
            (mp_partial_exact_or_split / mp_partial_all * 100)
            if mp_partial_all > 0
            else 0
        )

        results.append(
            {
                "Origin": "Overall",
                "Total": total_all,
                "Pusharoo_Any": any_all,
                "Pusharoo_Exact": exact_all,
                "Pusharoo_Partial": partial_all,
                "Pusharoo_Split": split_all,
                "MagicPush_Any": mp_any_all,
                "MagicPush_Exact": mp_exact_all,
                "MagicPush_Partial": mp_partial_all,
                "MagicPush_None": mp_none_all,
            }
        )

        # Return both the DataFrame and the additional statistics
        additional_stats = {
            "mp_partial_total": mp_partial_all,
            "mp_partial_exact_or_split": mp_partial_exact_or_split,
            "mp_partial_exact_or_split_pct": mp_partial_exact_or_split_pct,
            "true_true_partial_split_count": true_true_partial_split_count,
            "true_true_partial_split_benchmarks": true_true_partial_split[
                "benchmark"
            ].tolist(),
        }

        return pd.DataFrame(results), additional_stats

    def generate_benchmark_classification_latex_table(
        self, stats_df: pd.DataFrame
    ) -> str:
        """Generate LaTeX table for benchmark classification.

        Args:
            stats_df: DataFrame with classification statistics

        Returns:
            LaTeX table as string
        """
        lines = []

        # Add midrule before first row
        lines.append("\\midrule")

        for idx, row in stats_df.iterrows():
            origin = row["Origin"]
            total = int(row["Total"])

            # Format origin name
            if origin == "pandas":
                origin_str = "\\texttt{pandas}"
            elif origin == "Overall":
                origin_str = "\\textbf{Overall}"
                lines.append("\\midrule")
            else:
                origin_str = origin

            # Pusharoo columns
            pa_any = int(row["Pusharoo_Any"])
            pa_any_pct = pa_any / total * 100 if total > 0 else 0

            pa_exact = int(row["Pusharoo_Exact"])
            pa_exact_pct = pa_exact / total * 100 if total > 0 else 0

            pa_partial = int(row["Pusharoo_Partial"])
            pa_partial_pct = pa_partial / total * 100 if total > 0 else 0

            pa_split = int(row["Pusharoo_Split"])
            pa_split_pct = pa_split / total * 100 if total > 0 else 0

            # MagicPush columns
            mp_any = int(row["MagicPush_Any"])
            mp_any_pct = mp_any / total * 100 if total > 0 else 0

            mp_exact = int(row["MagicPush_Exact"])
            mp_exact_pct = mp_exact / total * 100 if total > 0 else 0

            mp_partial = int(row["MagicPush_Partial"])
            mp_partial_pct = mp_partial / total * 100 if total > 0 else 0

            # Build row with bold percentages for Any, Exact, Partial columns
            line_parts = [
                origin_str,
                f"{total}",
                f"{pa_any} (\\textbf{{{pa_any_pct:.1f}\\%}})",
                f"{pa_exact} (\\textbf{{{pa_exact_pct:.1f}\\%}})",
                f"{pa_partial} (\\textbf{{{pa_partial_pct:.1f}\\%}})",
                f"{pa_split} ({pa_split_pct:.1f}\\%)",
                f"{mp_any} ({mp_any_pct:.1f}\\%)",
                f"{mp_exact} ({mp_exact_pct:.1f}\\%)",
                f"{mp_partial} ({mp_partial_pct:.1f}\\%)",
            ]

            lines.append(" & ".join(line_parts) + " \\\\")

        return "\n".join(lines)

    def compute_pushdown_type_stats(
        self, results_file: str = "results.csv"
    ) -> pd.DataFrame:
        """Compute statistics by pushdown type.

        Computes counts and averages for runtime and various metrics by pushdown type.

        Args:
            results_file: Results file to analyze

        Returns:
            DataFrame with pushdown type statistics
        """
        df = self.load_csv(results_file)

        # Filter only successful runs
        df = df[df["status"] == "ok"]

        # Map kind to type category
        type_mapping = {1: "Exact", 2: "Partial", 3: "Split", 5: "Split"}
        df["type_category"] = df["kind"].map(type_mapping)

        results = []

        # Compute stats for each type
        for type_name in ["Exact", "Partial", "Split"]:
            type_df = df[df["type_category"] == type_name]

            if len(type_df) == 0:
                continue

            # For |P*|, use N/A for Exact type (kind=1) since they don't have |P*|
            avg_p_star = np.nan if type_name == "Exact" else type_df["|P*|"].mean()

            results.append(
                {
                    "Type": type_name,
                    "Count": len(type_df),
                    "Avg_Runtime": type_df["runtime"].mean(),
                    "Avg_Q_star": type_df["|Q*|"].mean(),
                    "Avg_P_star": avg_p_star,
                    "Avg_BI": type_df["|BI|"].mean(),
                    "Avg_P": type_df["|P|"].mean(),
                }
            )

        # Compute overall stats
        results.append(
            {
                "Type": "Overall",
                "Count": len(df),
                "Avg_Runtime": df["runtime"].mean(),
                "Avg_Q_star": df["|Q*|"].mean(),
                "Avg_P_star": df["|P*|"].mean(),
                "Avg_BI": df["|BI|"].mean(),
                "Avg_P": df["|P|"].mean(),
            }
        )

        return pd.DataFrame(results)

    def generate_pushdown_type_latex_table(self, stats_df: pd.DataFrame) -> str:
        """Generate LaTeX table for pushdown type statistics.

        Args:
            stats_df: DataFrame with pushdown type statistics

        Returns:
            LaTeX table as string
        """
        lines = []
        lines.append("\\begin{tabular}{l c c c c c c}")
        lines.append("\\toprule")
        lines.append("\\textbf{Type}")
        lines.append("  & \\textbf{\\# Benchmarks}")
        lines.append("    & \\textbf{Avg Runtime (s)}")
        lines.append("      & $\\mathbf{Avg~|Q^*|}$")
        lines.append("        & $\\mathbf{Avg~|P^*|}$")
        lines.append("          & $\\mathbf{Avg~|\\psi|}$")
        lines.append("            & $\\mathbf{Avg~|P|}$\\\\")
        lines.append("\\midrule")

        for idx, row in stats_df.iterrows():
            type_name = row["Type"]

            # Add midrule before Overall row
            if type_name == "Overall":
                lines.append("\\midrule")
                type_str = "\\textbf{Overall}"
            else:
                type_str = type_name

            count = int(row["Count"])
            runtime = row["Avg_Runtime"]
            q_star = row["Avg_Q_star"]
            p_star = row["Avg_P_star"]
            bi = row["Avg_BI"]
            p = row["Avg_P"]

            # Format P* as N/A for Exact type
            if pd.isna(p_star):
                p_star_str = "N/A"
            else:
                p_star_str = f"{p_star:.2f}"

            parts = [
                type_str,
                f"{count}",
                f"{runtime:.2f}",
                f"{q_star:.2f}",
                p_star_str,
                f"{bi:.2f}",
                f"{p:.2f}",
            ]

            lines.append(" & ".join(parts) + " \\\\")

        lines.append("\\bottomrule")
        lines.append("\\end{tabular}")

        return "\n".join(lines)

    def compute_runtime_quartiles(
        self, results_file: str = "results.csv"
    ) -> pd.DataFrame:
        """Compute runtime quartile statistics by pushdown type.

        Computes Q1, Median, Q3, and Max runtime for each pushdown type.

        Args:
            results_file: Results file to analyze

        Returns:
            DataFrame with runtime quartile statistics
        """
        df = self.load_csv(results_file)

        # Filter only successful runs
        df = df[df["status"] == "ok"]

        # Map kind to type category
        type_mapping = {1: "Exact", 2: "Partial", 3: "Split", 5: "Split"}
        df["type_category"] = df["kind"].map(type_mapping)

        results = []

        # Compute stats for each type
        for type_name in ["Exact", "Partial", "Split"]:
            type_df = df[df["type_category"] == type_name]

            if len(type_df) == 0:
                continue

            runtime_data = type_df["runtime"]

            results.append(
                {
                    "Type": type_name,
                    "Q1": runtime_data.quantile(0.25),
                    "Median": runtime_data.median(),
                    "Q3": runtime_data.quantile(0.75),
                    "Max": runtime_data.max(),
                }
            )

        # Compute overall stats
        runtime_all = df["runtime"]
        results.append(
            {
                "Type": "Overall",
                "Q1": runtime_all.quantile(0.25),
                "Median": runtime_all.median(),
                "Q3": runtime_all.quantile(0.75),
                "Max": runtime_all.max(),
            }
        )

        return pd.DataFrame(results)

    def generate_runtime_quartiles_latex_table(self, stats_df: pd.DataFrame) -> str:
        """Generate LaTeX table for runtime quartile statistics.

        Args:
            stats_df: DataFrame with runtime quartile statistics

        Returns:
            LaTeX table as string
        """
        lines = []
        lines.append("\\begin{tabular}{lcccc}")
        lines.append("\\toprule")
        lines.append(
            "\\textbf{Type} & \\textbf{Q1 (s)} & \\textbf{Median (s)} & \\textbf{Q3 (s)} & \\textbf{Max (s)} \\\\"
        )
        lines.append("\\midrule")

        for idx, row in stats_df.iterrows():
            type_name = row["Type"]

            # Add midrule before Overall row
            if type_name == "Overall":
                lines.append("\\midrule")
                type_str = "Overall"
            else:
                type_str = type_name

            q1 = row["Q1"]
            median = row["Median"]
            q3 = row["Q3"]
            max_val = row["Max"]

            parts = [
                type_str,
                f"{q1:.2f}",
                f"{median:.2f}",
                f"{q3:.2f}",
                f"{max_val:.2f}",
            ]

            lines.append(" & ".join(parts) + " \\\\")

        lines.append("\\bottomrule")
        lines.append("\\end{tabular}")

        return "\n".join(lines)

    def compare_solvers(
        self,
        pusharoo_file: str = "results.csv",
        spacer_file: str = "results_spacer.csv",
        eldarica_file: str = "results_eld.csv",
    ) -> pd.DataFrame:
        """Compare Pusharoo, Spacer, and Eldarica solvers.

        Computes solved benchmarks and suboptimal solutions for each solver.
        Suboptimality is determined by comparing the ``kind`` column between
        Pusharoo and each CHC solver on benchmarks solved by both.  A CHC
        solver result is *suboptimal* when its kind differs from Pusharoo's,
        **excluding** the case where the CHC solver has kind=3 (split) and
        Pusharoo has kind=2 (partial).

        Args:
            pusharoo_file: Pusharoo results file
            spacer_file: Spacer results file
            eldarica_file: Eldarica results file

        Returns:
            DataFrame with solver comparison statistics
        """
        pusharoo_df = self.load_csv(pusharoo_file)
        spacer_df = self.load_csv(spacer_file)
        eldarica_df = self.load_csv(eldarica_file)

        pusharoo_ok = pusharoo_df[pusharoo_df["status"] == "ok"]
        pusharoo_count = len(pusharoo_ok)
        spacer_ok = spacer_df[spacer_df["status"] == "ok"]
        spacer_count = len(spacer_ok)
        eldarica_ok = eldarica_df[eldarica_df["status"] == "ok"]
        eldarica_count = len(eldarica_ok)

        def _count_suboptimal(chc_ok: pd.DataFrame) -> int:
            """Count suboptimal CHC results via kind-based comparison."""
            merged = pusharoo_ok[["benchmark", "kind"]].merge(
                chc_ok[["benchmark", "kind"]],
                on="benchmark",
                suffixes=("_push", "_chc"),
            )
            differs = merged[merged["kind_push"] != merged["kind_chc"]]
            excludable = (differs["kind_chc"] == 3) & (differs["kind_push"] == 2)
            return int((~excludable).sum())

        spacer_suboptimal = _count_suboptimal(spacer_ok)
        eldarica_suboptimal = _count_suboptimal(eldarica_ok)

        results = [
            {"Solver": "Pusharoo", "Solved": pusharoo_count, "Suboptimal": "N/A"},
            {
                "Solver": "Spacer",
                "Solved": spacer_count,
                "Suboptimal": spacer_suboptimal,
            },
            {
                "Solver": "Eldarica",
                "Solved": eldarica_count,
                "Suboptimal": eldarica_suboptimal,
            },
        ]

        spacer_subopt_pct = (
            (spacer_suboptimal / spacer_count * 100) if spacer_count > 0 else 0
        )
        eldarica_subopt_pct = (
            (eldarica_suboptimal / eldarica_count * 100) if eldarica_count > 0 else 0
        )

        additional_stats = {
            "spacer_subopt_pct": spacer_subopt_pct,
            "eldarica_subopt_pct": eldarica_subopt_pct,
        }

        return pd.DataFrame(results), additional_stats

    def generate_solver_comparison_latex_table(self, stats_df: pd.DataFrame) -> str:
        """Generate LaTeX table for solver comparison.

        Args:
            stats_df: DataFrame with solver comparison statistics

        Returns:
            LaTeX table as string
        """
        lines = []
        lines.append("\\begin{tabular}{lcc}")
        lines.append("\\toprule")
        lines.append("\\textbf{Solver} & \\textbf{Solved} & \\textbf{Suboptimal} \\\\")
        lines.append("\\midrule")

        for idx, row in stats_df.iterrows():
            solver = row["Solver"]
            solved = int(row["Solved"])
            suboptimal = row["Suboptimal"]

            if suboptimal == "N/A":
                suboptimal_str = "0"
            else:
                suboptimal_str = str(int(suboptimal))

            parts = [f"\\textsc{{{solver}}}", f"{solved}", suboptimal_str]

            lines.append(" & ".join(parts) + " \\\\")

        lines.append("\\bottomrule")
        lines.append("\\end{tabular}")

        return "\n".join(lines)

    def compute_ablation_stats(
        self,
        pusharoo_file: str = "results.csv",
        nobounds_file: str = "results_nobounds.csv",
        noanalysis_file: str = "results_noanalysis.csv",
        nobounds_noanalysis_file: str = "results_nobounds_noanalysis.csv",
        twophase_file: str = "results_twophase.csv",
        spacer_file: str = "results_spacer.csv",
        eldarica_file: str = "results_eld.csv",
    ) -> Dict[str, Dict[str, float]]:
        """Compute total solved and total time for Pusharoo and ablations.

        Args:
            pusharoo_file: Pusharoo results file
            nobounds_file: NoBounds ablation results file
            noanalysis_file: NoRepair ablation results file
            nobounds_noanalysis_file: NoBoundsNoRepair ablation results file
            twophase_file: TwoPhase ablation results file
            spacer_file: Spacer results file
            eldarica_file: Eldarica results file

        Returns:
            Dictionary mapping configuration name to stats
        """
        configs = {
            "Pusharoo": pusharoo_file,
            "NoBounds": nobounds_file,
            "NoRepair": noanalysis_file,
            "NoBoundsNoRepair": nobounds_noanalysis_file,
            "TwoPhase": twophase_file,
            "Spacer": spacer_file,
            "Eldarica": eldarica_file,
        }

        results = {}

        for name, filename in configs.items():
            df = self.load_csv(filename)

            # Filter only successful runs
            ok_df = df[df["status"] == "ok"]

            total_solved = len(ok_df)
            total_time = ok_df["runtime"].sum()

            results[name] = {"solved": total_solved, "total_time": total_time}

        return results

    def compute_slowdown(
        self,
        baseline_file: str,
        comparison_file: str,
    ) -> pd.DataFrame:
        """Compute slowdown statistics of comparison relative to baseline.

        For each benchmark solved by both, computes
        slowdown = comparison_runtime / baseline_runtime.

        Args:
            baseline_file: Baseline results CSV (denominator)
            comparison_file: Comparison results CSV (numerator)

        Returns:
            DataFrame with one row containing mean, median, min, max, Q1, Q3
        """
        baseline = self.load_csv(baseline_file)
        comparison = self.load_csv(comparison_file)

        baseline_ok = baseline[baseline["status"] == "ok"].set_index("benchmark")
        comparison_ok = comparison[comparison["status"] == "ok"].set_index("benchmark")

        common = baseline_ok.index.intersection(comparison_ok.index)
        base_rt = baseline_ok.loc[common, "runtime"].astype(float)
        comp_rt = comparison_ok.loc[common, "runtime"].astype(float)

        slowdown = comp_rt / base_rt

        return pd.DataFrame(
            [
                {
                    "Benchmarks": len(common),
                    "Mean": slowdown.mean(),
                    "Median": slowdown.median(),
                    "Min": slowdown.min(),
                    "Max": slowdown.max(),
                    "Q1": slowdown.quantile(0.25),
                    "Q3": slowdown.quantile(0.75),
                }
            ]
        )

    def plot_cdf(
        self,
        pusharoo_file: str = "results.csv",
        nobounds_file: str = "results_nobounds.csv",
        noanalysis_file: str = "results_noanalysis.csv",
        nobounds_noanalysis_file: Optional[str] = "results_nobounds_noanalysis.csv",
        twophase_file: str = "results_twophase.csv",
        spacer_file: str = "results_spacer.csv",
        eldarica_file: str = "results_eld.csv",
        output_path: Optional[Path] = None,
    ) -> None:
        """Plot CDF curves for all solver/ablation configurations.

        Args:
            pusharoo_file: Pusharoo results file
            nobounds_file: NoBounds ablation results file
            noanalysis_file: NoRepair ablation results file
            nobounds_noanalysis_file: NoBoundsNoRepair ablation results file
                (set to None to omit this curve, e.g. for Figure 5)
            twophase_file: TwoPhase ablation results file
            spacer_file: Spacer results file
            eldarica_file: Eldarica results file
            output_path: Where to save the PDF (default: results_dir/cdf.pdf)
        """
        rcParams.update(
            {
                "font.family": "sans-serif",
                "mathtext.fontset": "dejavusans",
                "text.usetex": False,
                "axes.labelsize": 12,
                "xtick.labelsize": 12,
                "ytick.labelsize": 12,
            }
        )

        # Load data
        configs = [
            ("push", pusharoo_file, "o", 30, "Pusharoo", 7, 0.8),
            ("nob", nobounds_file, "s", 30, "NoBounds", 6.5, 0.75),
            ("noa", noanalysis_file, "p", 45, "NoRepair", 7, 0.8),
        ]
        if nobounds_noanalysis_file is not None:
            configs.append(
                (
                    "noba",
                    nobounds_noanalysis_file,
                    "P",
                    45,
                    "NoPrune",
                    7,
                    0.8,
                ),
            )
        configs += [
            ("two", twophase_file, "v", 45, "TwoPhase", 7, 0.8),
            ("sp", spacer_file, "^", 45, "Spacer", 7, 0.8),
            ("eld", eldarica_file, "D", 30, "Eldarica", 5, 0.6),
        ]

        # Load ok rows and compute sorted runtimes
        loaded = []
        for tag, filename, marker, msize, label, hsize, hedge_w in configs:
            df = self.load_csv(filename)
            ok = df[df["status"] == "ok"]
            rts = np.sort(ok["runtime"].astype(float).values)
            loaded.append((tag, rts, marker, msize, label, hsize, hedge_w))

        # Use Pusharoo total as common denominator
        tot = len(loaded[0][1])

        # Build CDF curves
        curves = []
        for tag, rts, marker, msize, label, hsize, hedge_w in loaded:
            x, y = _make_cdf(rts, tot)
            curves.append((tag, x, y, rts, marker, msize, label, hsize, hedge_w))

        # Plot
        fig, ax = plt.subplots(figsize=(8.4, 4.8), constrained_layout=True)
        ax.tick_params(axis="both", labelsize=12)

        for tag, x, y, _rts, _m, _ms, label, _hs, _hw in curves:
            ax.step(x, y, lw=1, color=CDF_COLORS[tag], label=label)

        for tag, x, y, _rts, marker, msize, _label, _hs, _hw in curves:
            _styled_scatter(ax, x, y, marker, CDF_COLORS[tag], size=msize)

        # Dashed max-runtime lines + labels
        label_nudge = {"two": 12, "noba": -12}
        r_max = [(tag, rts[-1]) for tag, rts, *_ in curves if len(rts) > 0]
        for tag, rt in r_max:
            ax.axvline(rt, color=CDF_COLORS[tag], ls="--", lw=1, alpha=0.6)
            nudge = label_nudge.get(tag, 0)
            ax.text(
                rt + nudge,
                1.005,
                f"{rt:.0f}s",
                transform=ax.get_xaxis_transform(),
                ha="center",
                va="bottom",
                color=CDF_COLORS[tag],
                fontsize=12,
            )

        # X-ticks: 0 + multiples of 100
        max_rt = max(rt for _, rt in r_max)
        raw_ticks = np.arange(0, int(np.ceil(max_rt / 100)) * 100 + 1, 100)
        ax.set_xticks(raw_ticks)
        ax.set_xticklabels([f"{int(t)}" for t in raw_ticks], fontsize=12)

        # Legend
        handles = []
        labels = []
        for tag, _x, _y, _rts, marker, _ms, label, hsize, hedge_w in curves:
            handles.append(_halo_handle(CDF_COLORS[tag], marker, hsize, hedge_w))
            labels.append(label)
        ax.legend(handles, labels, loc="lower right", fontsize=12, framealpha=0.9)

        ax.set_xlim(0, max_rt)
        ax.set_ylim(0, 100)
        ax.set_xlabel("Runtime (s)", fontsize=14)
        ax.set_ylabel("Benchmarks Solved (%)", fontsize=14)

        if output_path is None:
            output_path = self.results_dir / "cdf.pdf"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_path, dpi=2400, bbox_inches="tight")
        print(f"Saved CDF plot -> {output_path}")
        plt.close(fig)


def main():
    """Main entry point for the analysis script."""
    parser = argparse.ArgumentParser(
        description="Analyze benchmark results and generate LaTeX tables"
    )
    parser.add_argument(
        "--results-dir",
        type=Path,
        default=Path(__file__).parent / "results",
        help="Directory containing results CSV files",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=None,
        help="Directory to save output files (default: results_dir)",
    )
    parser.add_argument(
        "--task",
        choices=[
            "results",
            "rerun",
            "table1",
            "table2",
            "table3",
            "table4",
            "table5",
            "table6",
            "figure5",
            "all-tables",
            # Legacy aliases (kept for backward compatibility)
            "speedup",
            "search-space",
            "classification",
            "type-stats",
            "runtime-quartiles",
            "solver-comparison",
            "ablation-stats",
            "cdf",
            "slowdown",
            "run-pandas",
            "run-spark",
            "run-synth",
            "ablcomp",
            "udf-props",
            "ast-sizes",
            "all",
        ],
        default="results",
        help="Which task to perform (see --help for workflow descriptions)",
    )

    # -- run-pandas arguments -----------------------------------------------
    parser.add_argument(
        "--pandas-dir",
        type=Path,
        default=Path(__file__).resolve().parent / "benchmarks" / "pandas",
        help="Directory with pandas benchmark .py files",
    )
    parser.add_argument(
        "--python",
        default=sys.executable,
        help="Python interpreter for pandas benchmarks",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=4,
        help="Total rounds per benchmark (first is warmup)",
    )
    parser.add_argument(
        "--file-regex",
        default=None,
        help="Only run pandas scripts matching this regex",
    )

    # -- run-spark arguments ------------------------------------------------
    parser.add_argument(
        "--spark-dir",
        type=Path,
        default=Path(__file__).resolve().parent / "benchmarks" / "spark",
        help="Directory with Spark benchmark .scala files",
    )
    parser.add_argument("--master", default=DEFAULT_MASTER)
    parser.add_argument("--driver-memory", default=DEFAULT_DRIVER_MEMORY)
    parser.add_argument(
        "--class-regex",
        default=None,
        help="Only run Spark classes matching this regex",
    )
    parser.add_argument(
        "--skip-build",
        action="store_true",
        help="Skip sbt build and reuse existing JARs",
    )
    parser.add_argument(
        "--extra-conf",
        action="append",
        default=[],
        help="Additional Spark conf (key=value)",
    )

    # -- run-synth / udf-props / ast-sizes arguments ---------------------------
    parser.add_argument(
        "--bench-dir",
        type=str,
        default=str((Path(__file__).resolve().parent / "benchmarks" / "dsl").resolve()),
        help="Directory of benchmark .py files (default: benchmarks/dsl)",
    )
    parser.add_argument(
        "--timeout",
        type=str,
        default="600",
        help="Timeout in seconds for pusharoo.exe (default: 600)",
    )
    parser.add_argument(
        "--mode",
        type=str,
        default="pusharoo",
        help="Mode for pusharoo.exe (default: pusharoo)",
    )
    parser.add_argument(
        "--out",
        type=str,
        default=None,
        help="Output CSV base name for run-synth (without .csv extension)",
    )
    parser.add_argument(
        "--expand",
        type=str,
        default=None,
        help="Expand predicate universes by given fraction (e.g. 0.1 for 10%%)",
    )
    parser.add_argument(
        "--save-qp",
        type=str,
        default=None,
        help="Append name,Q*,P'* to given CSV file",
    )

    args = parser.parse_args()

    # Default --results for runners: both pandas and spark write to the same file
    if not hasattr(args, "results") or args.task in ("run-pandas", "run-spark"):
        args.results = args.results_dir / "results_speedups.csv"

    if args.output_dir is None:
        args.output_dir = args.results_dir

    args.output_dir.mkdir(parents=True, exist_ok=True)

    # -- Composite workflow: rerun (re-run experiments + regenerate tables) ----
    if args.task == "rerun":
        bench_dir_dsl_bench = str(
            (Path(__file__).resolve().parent / "benchmarks" / "dsl-bench").resolve()
        )

        synth_steps = [
            # (mode,       out_name,              bench_dir)
            ("pusharoo", "results", bench_dir_dsl_bench),
            ("nobounds", "results_nobounds", bench_dir_dsl_bench),
            ("noanalysis", "results_noanalysis", bench_dir_dsl_bench),
            ("twophase", "results_twophase", bench_dir_dsl_bench),
            ("spacer", "results_spacer", bench_dir_dsl_bench),
            ("eldarica", "results_eld", bench_dir_dsl_bench),
        ]

        # Delete result files so experiments start fresh
        for _, out_name, _ in synth_steps:
            p = args.results_dir / f"{out_name}.csv"
            if p.exists():
                p.unlink()
                print(f"[rerun] Deleted {p.name}")
        for extra in ["results_speedups.csv", "udf_props.csv", "ast_sizes.csv"]:
            p = args.results_dir / extra
            if p.exists():
                p.unlink()
                print(f"[rerun] Deleted {p.name}")

        # Step 0: Regenerate Table 1 data (udf_props.csv, ast_sizes.csv)
        print(f"\n{'=' * 70}")
        print("[rerun] Step 0: Generating UDF properties and AST sizes")
        print(f"{'=' * 70}")
        try:
            compute_udf_props(bench_dir_dsl_bench)
            compute_ast_sizes(bench_dir_dsl_bench, args.results_dir)
        except Exception as e:
            print(f"[warn] Step 0 failed: {e} -- continuing")

        total_steps = len(synth_steps) + 2  # +2 for pandas and spark
        for i, (mode, out_name, bdir) in enumerate(synth_steps, 1):
            print(f"\n{'=' * 70}")
            print(
                f"[rerun] Step {i}/{total_steps}: run-synth --mode {mode} --out {out_name}"
            )
            print(f"{'=' * 70}")
            if not os.path.isdir(bdir):
                print(f"[warn] Benchmark directory not found: {bdir} -- skipping")
                continue
            try:
                run_pusharoo_on_directory(
                    bdir,
                    args.timeout,
                    mode,
                    str(args.results_dir),
                    out_name,
                )
            except Exception as e:
                print(f"[warn] Step {i} failed ({mode}): {e} -- continuing")

        # Step 7: run-pandas
        step_num = len(synth_steps) + 1
        print(f"\n{'=' * 70}")
        print(f"[rerun] Step {step_num}/{total_steps}: run-pandas")
        print(f"{'=' * 70}")
        try:
            args.results = args.results_dir / "results_speedups.csv"
            run_pandas(args)
        except Exception as e:
            print(f"[warn] run-pandas failed: {e} -- continuing")

        # Step 8: run-spark
        step_num = len(synth_steps) + 2
        print(f"\n{'=' * 70}")
        print(f"[rerun] Step {step_num}/{total_steps}: run-spark")
        print(f"{'=' * 70}")
        try:
            args.results = args.results_dir / "results_speedups.csv"
            run_spark(args)
        except Exception as e:
            print(f"[warn] run-spark failed: {e} -- continuing")

        # Fall through to analysis
        print(f"\n{'=' * 70}")
        print("[rerun] Data generation complete. Now running analysis...")
        print(f"{'=' * 70}")
        args.task = "all-tables"

    # -- Composite workflow: results (alias for all-tables) --------------------
    if args.task == "results":
        args.task = "all-tables"

    analyzer = ResultsAnalyzer(args.results_dir)

    # Convenience set for the "all-tables" meta-task
    _ALL_TABLES = {
        "table1",
        "table2",
        "table3",
        "table4",
        "table5",
        "table6",
        "figure5",
    }

    # Table 1: UDF statistics (combines udf-props + ast-sizes)
    if args.task in ["table1", "all-tables"]:
        print("=" * 70)
        print("TABLE 1: UDF Statistics")
        print("=" * 70)

        # Read pre-computed results (udf_props.csv and ast_sizes.csv)
        props_path = args.results_dir / "udf_props.csv"
        size_path = args.results_dir / "ast_sizes.csv"

        # Pandas UDF prefixes (rest are Spark)
        _PANDAS_PREFIXES = {
            "20Nov",
            "Ada1",
            "Ada3",
            "Basket",
            "Chicago",
            "Flaredown1",
            "Jdata",
        }

        if props_path.exists():
            props_df = pd.read_csv(props_path)
            # Convert string "true"/"false" to bool
            for c in props_df.columns[1:]:
                props_df[c] = props_df[c].astype(str).str.lower() == "true"
            total = len(props_df)
            pandas_count = sum(1 for p in props_df.iloc[:, 0] if p in _PANDAS_PREFIXES)
            spark_count = total - pandas_count

            print(f"\n  Total UDFs:         {total}")
            print(f"  pandas UDFs:        {pandas_count}")
            print(f"  Spark UDFs:         {spark_count}")

            col_map = {
                "has_conditional": "# w/ conditionals",
                "has_collection": "# w/ collections",
                "has_tuple_accum": "# w/ tuple-accumulator",
                "has_crossdep": "# w/ cross-dependent",
            }
            for col, label in col_map.items():
                if col in props_df.columns:
                    print(f"  {label}: {int(props_df[col].sum())}")
        else:
            print(
                f"\nWarning: {props_path} not found. Run --task udf-props to generate."
            )

        if size_path.exists():
            size_df = pd.read_csv(size_path)
            values = size_df.iloc[:, 0]
            print(f"\n  Avg AST size:       {values.mean():.0f}")
            print(f"  Max AST size:       {int(values.max())}")
        else:
            print(
                f"\nWarning: {size_path} not found. Run --task ast-sizes to generate."
            )

        # Predicates per UDF: count benchmarks per prefix
        results_path = args.results_dir / "results.csv"
        if results_path.exists():
            res_df = pd.read_csv(results_path)
            res_df["prefix"] = res_df["benchmark"].str.rsplit("_", n=1).str[0]
            preds_per_udf = res_df.groupby("prefix").size()
            print(
                f"\n  Predicates per UDF: {preds_per_udf.min()}-{preds_per_udf.max()} (avg {preds_per_udf.mean():.1f})"
            )

    # Table 4 / speedup: Pipeline runtime reduction
    if args.task in ["table4", "speedup", "all", "all-tables"]:
        print("=" * 70)
        print("TABLE 4: Pipeline Runtime Reduction")
        print("=" * 70)

        speedup_stats = analyzer.compute_speedup_stats_by_type()
        print("\nStatistics:")
        formatted = speedup_stats.copy()
        for col in ["Min", "Median", "Avg", "Max"]:
            formatted[col] = formatted[col].apply(
                lambda x: "N/A" if pd.isna(x) else f"{x:.1f}"
            )
        print(formatted.to_string(index=False))

        # Average speedup as multiplier
        overall = speedup_stats[speedup_stats["Type"] == "Overall"].iloc[0]
        avg_pct = overall["Avg"]
        max_pct = overall["Max"]
        if not pd.isna(avg_pct) and avg_pct < 100:
            avg_mult = 1 / (1 - avg_pct / 100)
            max_mult = 1 / (1 - max_pct / 100) if max_pct < 100 else float("inf")
            print(f"\nAverage speedup: {avg_mult:.1f}x (up to {max_mult:.1f}x)")

    if args.task == "all-tables":
        try:
            _ss = analyzer.compute_speedup_stats_by_type()
            _ov = _ss[_ss["Type"] == "Overall"].iloc[0]
            _avg_p = _ov["Avg"]
            _med_p = _ov["Median"]
            _max_p = _ov["Max"]
            _avg_x = 1 / (1 - _avg_p / 100) if _avg_p < 100 else float("inf")
            print(
                f"\nRQ3: Average runtime reduction: {_avg_p:.1f}% (median {_med_p:.1f}%, up to {_max_p:.1f}%)."
            )
            print(f"     Average speedup: {_avg_x:.1f}x.")
        except Exception:
            pass

    # Table 5 / search-space: Search space reduction
    if args.task in ["table5", "search-space", "all", "all-tables"]:
        print("\n" + "=" * 70)
        print("TABLE 5: Search Space Reduction")
        print("=" * 70)

        try:
            # Table 5 shows NoBounds, NoRepair, TwoPhase (no NoBoundsNoRepair)
            table5_baselines = {
                "NoBounds": "results_nobounds.csv",
                "NoRepair": "results_noanalysis.csv",
                "TwoPhase": "results_twophase.csv",
            }
            reduction_stats = analyzer.analyze_search_space_reductions(
                baselines=table5_baselines
            )

            # Display statistics in a more readable format
            print("\nStatistics (Median / Avg / Max):")
            print("-" * 70)

            # Get baseline names
            baseline_cols = [
                col.replace("_Median", "")
                for col in reduction_stats.columns
                if col.endswith("_Median")
            ]

            # Print header
            header = f"{'Type':<10}"
            for baseline in baseline_cols:
                header += f"{baseline:>25}"
            print(header)
            print("-" * 70)

            # Print each row
            for _, row in reduction_stats.iterrows():
                line = f"{row['Type']:<10}"
                for baseline in baseline_cols:
                    median = row[f"{baseline}_Median"]
                    avg = row[f"{baseline}_Avg"]
                    max_val = row[f"{baseline}_Max"]

                    if pd.isna(median):
                        line += f"{'-- / -- / --':>25}"
                    else:
                        line += f"{median:>6.1f} / {avg:>5.1f} / {max_val:>5.1f}   "
                print(line)

        except Exception as e:
            print(f"\nError analyzing search space reduction: {e}")
            import traceback

            traceback.print_exc()

    # Table 2 / classification: Pushdown outcomes (Pusharoo vs MagicPush)
    if args.task in ["table2", "classification", "all", "all-tables"]:
        print("\n" + "=" * 70)
        print("TABLE 2: Pushdown Outcomes (Pusharoo vs. MagicPush)")
        print("=" * 70)

        try:
            classification_stats, additional_stats = (
                analyzer.compute_benchmark_classification()
            )

            # Display statistics with percentages
            print("\nStatistics:")
            formatted = classification_stats.drop(
                columns=["MagicPush_None"], errors="ignore"
            ).copy()
            count_cols = [
                "Pusharoo_Any",
                "Pusharoo_Exact",
                "Pusharoo_Partial",
                "Pusharoo_Split",
                "MagicPush_Any",
                "MagicPush_Exact",
                "MagicPush_Partial",
            ]
            for col in count_cols:
                formatted[col] = formatted.apply(
                    lambda row: (
                        f"{row[col]} ({row[col] / row['Total'] * 100:.1f}%)"
                        if row["Total"] > 0
                        else str(row[col])
                    ),
                    axis=1,
                )
            print(formatted.to_string(index=False))

            print(f"\nMagicPush Partial cases: {additional_stats['mp_partial_total']}")
            print(
                f"  - Pusharoo finds a weaker residual for: {additional_stats['mp_partial_exact_or_split']} ({additional_stats['mp_partial_exact_or_split_pct']:.1f}%)"
            )

        except Exception as e:
            print(f"\nError analyzing benchmark classification: {e}")
            import traceback

            traceback.print_exc()

    if args.task == "all-tables":
        try:
            _cls, _add = analyzer.compute_benchmark_classification()
            overall = _cls[_cls["Origin"] == "Overall"].iloc[0]
            total = overall["Total"]
            mp_any = overall["MagicPush_Any"]
            print("\nRQ1: Pusharoo achieves valid pushdown on 100% of benchmarks.")
            print(
                f"     MagicPush applies to only {mp_any}/{total} ({mp_any / total * 100:.1f}%)."
            )
        except Exception:
            pass

    # Table 3 / type-stats: Types of pushdowns synthesized
    if args.task in ["table3", "type-stats", "all", "all-tables"]:
        print("\n" + "=" * 70)
        print("TABLE 3: Types of Pushdowns Synthesized")
        print("=" * 70)

        try:
            type_stats = analyzer.compute_pushdown_type_stats()

            # Display statistics (2 decimal places, NaN shown as N/A)
            print("\nStatistics:")
            formatted = type_stats.copy()
            float_cols = ["Avg_Runtime", "Avg_Q_star", "Avg_P_star", "Avg_BI", "Avg_P"]
            for col in float_cols:
                formatted[col] = formatted[col].apply(
                    lambda x: "N/A" if pd.isna(x) else f"{x:.2f}"
                )
            print(formatted.to_string(index=False))

            # Overall median synthesis time
            results_df = analyzer.load_csv("results.csv")
            ok_df = results_df[results_df["status"] == "ok"]
            median_rt = ok_df["runtime"].median()
            print(f"\nMedian synthesis time: {median_rt:.2f}s")

        except Exception as e:
            print(f"\nError analyzing pushdown type statistics: {e}")
            import traceback

            traceback.print_exc()

    if args.task == "all-tables":
        try:
            _ts = analyzer.compute_pushdown_type_stats()
            _ov = _ts[_ts["Type"] == "Overall"].iloc[0]
            _rdf = analyzer.load_csv("results.csv")
            _ok = _rdf[_rdf["status"] == "ok"]
            _med = _ok["runtime"].median()
            _avg = _ov["Avg_Runtime"]
            _avg_bi = _ov["Avg_BI"]
            print(f"\nRQ2: Median synthesis time: {_med:.2f}s (avg {_avg:.2f}s).")
            print(f"     Bisimulation invariants average {_avg_bi:.1f} in size.")
        except Exception:
            pass

    # Task 5: Runtime quartile statistics
    if args.task in ["runtime-quartiles", "all"]:
        print("\n" + "=" * 70)
        print("RUNTIME QUARTILE STATISTICS")
        print("=" * 70)

        try:
            quartile_stats = analyzer.compute_runtime_quartiles()

            # Display statistics
            print("\nStatistics:")
            print(quartile_stats.to_string(index=False))

        except Exception as e:
            print(f"\nError analyzing runtime quartiles: {e}")
            import traceback

            traceback.print_exc()

    # Table 6 / solver-comparison: Solution optimality
    if args.task in ["table6", "solver-comparison", "all", "all-tables"]:
        print("\n" + "=" * 70)
        print("TABLE 6: Solution Optimality")
        print("=" * 70)

        try:
            comparison_stats, additional_stats = analyzer.compare_solvers()

            # Display statistics
            print("\nStatistics:")
            print(comparison_stats.to_string(index=False))

            # Display additional percentages
            print("\nSuboptimal Solution Percentages:")
            print(
                f"  Spacer: {additional_stats['spacer_subopt_pct']:.1f}% of solved benchmarks"
            )
            print(
                f"  Eldarica: {additional_stats['eldarica_subopt_pct']:.1f}% of solved benchmarks"
            )

        except Exception as e:
            print(f"\nError comparing solvers: {e}")
            import traceback

            traceback.print_exc()

    if args.task == "all-tables":
        try:
            _cs, _as = analyzer.compare_solvers()
            spacer_row = _cs[_cs["Solver"] == "Spacer"].iloc[0]
            eldarica_row = _cs[_cs["Solver"] == "Eldarica"].iloc[0]
            sp_solved = int(spacer_row["Solved"])
            sp_sub = int(spacer_row["Suboptimal"])
            el_solved = int(eldarica_row["Solved"])
            el_sub = int(eldarica_row["Suboptimal"])
            print(
                f"\nRQ4: Spacer produces suboptimal solutions for {sp_sub}/{sp_solved} benchmarks ({_as['spacer_subopt_pct']:.0f}%)."
            )
            print(
                f"     Eldarica produces suboptimal solutions for {el_sub}/{el_solved} benchmarks ({_as['eldarica_subopt_pct']:.0f}%)."
            )
        except Exception:
            pass

    # Task 7: Ablation statistics
    if args.task in ["ablation-stats", "all"]:
        print("\n" + "=" * 70)
        print("ABLATION STATISTICS")
        print("=" * 70)

        try:
            ablation_stats = analyzer.compute_ablation_stats()

            # Get Pusharoo's total time for ratio computation
            pusharoo_time = ablation_stats["Pusharoo"]["total_time"]
            total_benchmarks = 150

            # Display statistics
            print("\nTotal Solved Benchmarks and Total Time:\n")
            sum_pct = 0
            sum_time = 0
            cnt_abls = 0
            for config_name, stats in ablation_stats.items():
                solved = stats["solved"]
                total_time = stats["total_time"]
                solved_pct = solved / total_benchmarks * 100

                if config_name == "Pusharoo":
                    print(
                        f"{config_name:18s}: {solved:3d} solved ({solved_pct:5.1f}%), {total_time:8.2f}s total time"
                    )
                else:
                    if config_name in [
                        "NoBounds",
                        "NoRepair",
                        "NoBoundsNoRepair",
                        "TwoPhase",
                    ]:
                        cnt_abls += 1
                        sum_pct += solved_pct
                        sum_time += total_time
                    ratio = total_time / pusharoo_time if pusharoo_time > 0 else 0
                    print(
                        f"{config_name:18s}: {solved:3d} solved ({solved_pct:5.1f}%), {total_time:8.2f}s total time ({ratio:5.2f}x)"
                    )

            print(f"Average solve rate: {(sum_pct / cnt_abls):.1f}%")
            print(f"Average solve time: {sum_time / cnt_abls / pusharoo_time:.1f}x")
        except Exception as e:
            print(f"\nError analyzing ablation statistics: {e}")
            import traceback

            traceback.print_exc()

    # Figure 5 / cdf: CDF plot of synthesis runtimes
    if args.task in ["figure5", "cdf", "all", "all-tables"]:
        print("\n" + "=" * 70)
        print("FIGURE 5: CDF Plot")
        print("=" * 70)

        try:
            output_path = args.output_dir / "cdf.pdf"
            print(f"Writing results to {output_path} ...")
            # Figure 5 shows 6 curves: Pusharoo, NoBounds, NoRepair,
            # TwoPhase, Spacer, Eldarica (no NoBoundsNoRepair / NoPrune).
            analyzer.plot_cdf(
                pusharoo_file="results.csv",
                nobounds_file="results_nobounds.csv",
                noanalysis_file="results_noanalysis.csv",
                nobounds_noanalysis_file=None,
                twophase_file="results_twophase.csv",
                spacer_file="results_spacer.csv",
                eldarica_file="results_eld.csv",
                output_path=output_path,
            )
        except Exception as e:
            print(f"\nError generating CDF plot: {e}")
            import traceback

            traceback.print_exc()

    # Task 9: Slowdown statistics
    if args.task in ["slowdown", "all"]:
        print("\n" + "=" * 70)
        print("SLOWDOWN: NoBoundsNoRepair vs Pusharoo")
        print("=" * 70)

        try:
            slowdown_df = analyzer.compute_slowdown(
                baseline_file="results.csv",
                comparison_file="results_nobounds_noanalysis.csv",
            )
            print(f"\n{int(slowdown_df.iloc[0]['Benchmarks'])} common benchmarks\n")
            print(f"  Mean:   {slowdown_df.iloc[0]['Mean']:.2f}x")
            print(f"  Median: {slowdown_df.iloc[0]['Median']:.2f}x")
            print(f"  Min:    {slowdown_df.iloc[0]['Min']:.2f}x")
            print(f"  Max:    {slowdown_df.iloc[0]['Max']:.2f}x")
            print(f"  Q1:     {slowdown_df.iloc[0]['Q1']:.2f}x")
            print(f"  Q3:     {slowdown_df.iloc[0]['Q3']:.2f}x")
        except Exception as e:
            print(f"\nError computing slowdown: {e}")
            import traceback

            traceback.print_exc()

    # Task 10: Run pandas benchmarks
    if args.task == "run-pandas":
        print("=" * 70)
        print("PANDAS BENCHMARK")
        print("=" * 70)
        run_pandas(args)

    # Task 11: Run Spark benchmarks
    if args.task == "run-spark":
        print("=" * 70)
        print("SPARK BENCHMARK")
        print("=" * 70)
        run_spark(args)

    # Task 12: Run Pusharoo/ablations
    if args.task == "run-synth":
        print("=" * 70)
        print("RUN PUSHAROO SYNTHESIS")
        print("=" * 70)

        if not os.path.isdir(args.bench_dir):
            print(f"Error: {args.bench_dir} is not a directory or does not exist.")
            sys.exit(1)

        out_name = args.out
        if out_name is None:
            print("Error: --out is required for run-synth task.")
            sys.exit(1)

        save_qp = args.save_qp

        run_pusharoo_on_directory(
            args.bench_dir,
            args.timeout,
            args.mode,
            str(args.results_dir),
            out_name,
            expand=args.expand,
            save_qp=save_qp,
        )

    # Task 13: Compare solution quality (ablcomp)
    if args.task == "ablcomp":
        print("=" * 70)
        print("ABLATION COMPARISON (SOLUTION QUALITY)")
        print("=" * 70)

        bench_dir_chc = (
            Path(__file__).resolve().parent / "benchmarks" / "dsl-bench"
        ).resolve()

        run_ablcomp(args.results_dir, bench_dir_chc)

    # Task 14: UDF boolean properties
    if args.task == "udf-props":
        print("=" * 70)
        print("UDF BOOLEAN PROPERTIES")
        print("=" * 70)

        if not os.path.isdir(args.bench_dir):
            print(f"Error: {args.bench_dir} is not a directory or does not exist.")
            sys.exit(1)

        compute_udf_props(args.bench_dir)

    # Task 15: AST size statistics
    if args.task == "ast-sizes":
        print("=" * 70)
        print("AST SIZE STATISTICS")
        print("=" * 70)

        if not os.path.isdir(args.bench_dir):
            print(f"Error: {args.bench_dir} is not a directory or does not exist.")
            sys.exit(1)

        compute_ast_sizes(args.bench_dir, args.results_dir)


if __name__ == "__main__":
    main()

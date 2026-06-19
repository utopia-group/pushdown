# Artifact: Optimal Predicate Pushdown Synthesis

This artifact accompanies the paper *Optimal Predicate Pushdown Synthesis*
(PLDI 2026). It contains Pusharoo, our tool for automatically synthesizing
optimal predicate pushdown transformations for stateful UDFs, along with
150 benchmarks, pre-computed results, and scripts to reproduce all
experimental results reported in the paper.

This artifact is also available on [Zenodo](https://doi.org/10.5281/zenodo.19084858)
with pre-generated Parquet input data files for the benchmarks.

Please cite our work with:

```bibtex
@article{10.1145/3808312,
author = {Zhang, Robert and Campbell, Eric Hayden and Tang, Dixin and Dillig, I\c{s}\i{}l},
title = {Optimal Predicate Pushdown Synthesis},
journal = {Proc. ACM Program. Lang.},
volume = {10},
number = {PLDI},
articleno = {234},
year = {2026},
doi = {10.1145/3808312}
}
```

## Getting Started (< 30 minutes)

### Setup (Docker, recommended)

Build and enter the container (all dependencies are pre-installed and Pusharoo is built).
**IMPORTANT**: Allocate at least **16 GB of memory** to Docker (Docker Desktop > Settings >
Resources).

```bash
docker build -t pusharoo .
docker run -it pusharoo
```

The build takes ~20 minutes and automatically
verifies the artifact by reproducing all tables, Figure 5, and stats.

If the build fails fetching Z3/CVC5/Eldarica/sbt/opam packages on a restricted
network, pass a proxy or host networking to `docker build`:
`--build-arg HTTP_PROXY=... --build-arg HTTPS_PROXY=...` or `--network=host`.
Transient timeouts usually clear on a re-run.

Once inside:

```bash
python3 experiments.py              # Reproduce all tables, Figure 5, and stats
cd synthesizer
./pusharoo.exe ../benchmarks/dsl-bench/Top2_1.py   # Quick synthesis check
```

Expected output for the synthesis check:
`Top2_1,3,<runtime>,1,1,25,4,3,3,false,false`
(kind=3 indicates a split pushdown decomposition).

### Setup (manual)

Requirements:
- OCaml 4.14.1, Z3 4.15.3, Eldarica 2.1, CVC5 1.2.1, Python 3.9+, Apache Spark 4.0.0, sbt 1.11.2
- See the `Dockerfile` for exact installation steps

```bash
pip3 install pandas==2.3.3 numpy==2.0.2 matplotlib==3.9.4 pyarrow==21.0.0
cd synthesizer && opam install . --deps-only && dune build && cd ..
python3 experiments.py              # Reproduce all tables, Figure 5, and stats
```

## Step-by-Step Instructions

### Reproducing Tables, Figure 5, and Stats

All experimental results can be reproduced:

```bash
python3 experiments.py                         # All tables, Figure 5, and stats
python3 experiments.py --task results          # Same as above
python3 experiments.py --task table1           # Table 1: UDF Statistics
python3 experiments.py --task table2           # Table 2: Pushdown Outcomes
python3 experiments.py --task table3           # Table 3: Types of Pushdowns
python3 experiments.py --task table4           # Table 4: Pipeline Runtime Reduction
python3 experiments.py --task table5           # Table 5: Search Space Reduction
python3 experiments.py --task table6           # Table 6: Solution Optimality
python3 experiments.py --task figure5          # Figure 5: CDF Plot
```

Figure 5 is saved to `results/cdf.pdf`. To view it from a Docker container,
copy it to the host with `docker cp <id>:/home/reviewer/pusharoo/results/cdf.pdf .`
(get `<id>` via `docker ps -a`).

### Re-running Experiments from Scratch

To regenerate all experimental data and reproduce the tables/Figure 5/stats:

```bash
python3 experiments.py --task rerun
```

This re-runs all synthesis experiments and end-to-end pipeline benchmarks,
then reproduces the tables, Figure 5, and stats. Expect approximately 28 hours
for the full sequential run on a machine with at least 14 cores and 36 GB RAM
(the majority of time is spent on ablation variants with many per-benchmark
timeouts; see [detailed breakdown](#re-running-individual-experiments) below).
Synthesis runtimes are
hardware-dependent and may differ from the paper (which used an Apple M3
Max); structural results (pushdown types, predicate sizes) are deterministic
up to solver non-determinism in rare edge cases. End-to-end pipeline
speedups (Table 4) require Apache Spark 4.0 and are sensitive to system
load, JVM warm-up, and hardware differences.

All experiments are **incremental**: results are written to disk after each
benchmark completes, so runs can be interrupted and resumed without losing
progress. Delete the result file of an experiment to restart afresh (using
the same result file name).

## Claims Supported by the Artifact

| Claim (Section) | Supported | Reproduced by |
|-----------------|-----------|---------------|
| UDF statistics (6) | Yes | `--task table1` |
| Pusharoo synthesizes valid pushdowns for all 150 benchmarks (6.1) | Yes | `--task table2` |
| MagicPush applies to only 22/150 benchmarks (6.1) | Yes | `--task table2` |
| Median synthesis time of 1.6s (6.2) | Yes | `--task table3` |
| Average runtime reduction of 58.8% (6.3) | Yes | `--task table4` |
| Search space reduction of 41-62% (6.4) | Yes | `--task table5` |
| CHC solvers produce suboptimal solutions (6.4) | Yes | `--task table6` |
| CDF comparison with ablations (6.4) | Yes | `--task figure5` |

Table 1's "# w/ conditionals" reports 23 vs. 22 in the paper due to a
corrected edge case in the UDF categorization logic.

## Artifact Structure

```
Dockerfile               Reproducible environment with all dependencies
experiments.py           Unified experiment and analysis script
results/                 Pre-computed experimental results (CSV, PDF)
synthesizer/
  src/                   OCaml source code for Pusharoo
  dune-project           Build configuration
benchmarks/
  dsl/                   150 DSL benchmark programs (with native types)
  dsl-bench/             150 DSL benchmarks (integer-encoded, used for evaluation)
  pandas/                7 pandas pipeline implementations (27 benchmarks)
  spark/                 19 Spark pipeline implementations (123 benchmarks)
baselines/               SyGuS baseline specifications
theorization/            Coq and Dafny formal proofs
.devcontainer/           Development container configuration
```

## Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| OCaml | 4.14 | Synthesizer implementation |
| Z3 | 4.15 | SMT solver backend |
| Eldarica | 2.1 | CHC solver (ablation study) |
| CVC5 | 1.2 | SMT solver (MagicPush feasibility check) |
| Python | 3.9 | Experiment scripts and analysis |
| pandas | 2.3 | Data analysis |
| numpy | 2.0 | Numerical computation |
| matplotlib | 3.9 | Figure generation |
| Apache Spark | 4.0 | End-to-end pipeline benchmarks |
| sbt | 1.11 | Scala build tool |

All dependencies are installed automatically by the `Dockerfile`.

## Re-running Individual Experiments

The `--task rerun` command runs everything sequentially (~28 hours total).
Experiments are run sequentially in order to avoid interference and ensure
accurate reproduction. Individual experiments can be re-run separately
(after deleting the respective result file):

| Experiment | Command | Est. Time |
|------------|---------|-----------|
| UDF statistics | `python3 experiments.py --task udf-props && python3 experiments.py --task ast-sizes` | <1m |
| Main synthesis | `python3 experiments.py --task run-synth --bench-dir benchmarks/dsl-bench --out results` | ~10m |
| NoBounds ablation | `python3 experiments.py --task run-synth --bench-dir benchmarks/dsl-bench --mode nobounds --out results_nobounds` | ~3h |
| NoRepair ablation | `python3 experiments.py --task run-synth --bench-dir benchmarks/dsl-bench --mode noanalysis --out results_noanalysis` | ~4h |
| TwoPhase ablation | `python3 experiments.py --task run-synth --bench-dir benchmarks/dsl-bench --mode twophase --out results_twophase` | ~13h |
| Spacer ablation | `python3 experiments.py --task run-synth --bench-dir benchmarks/dsl-bench --mode spacer --out results_spacer` | ~30m |
| Eldarica ablation | `python3 experiments.py --task run-synth --bench-dir benchmarks/dsl-bench --mode eldarica --out results_eld` | ~2.5h |
| Pandas pipelines | `python3 experiments.py --task run-pandas` | ~2h |
| Spark pipelines | `python3 experiments.py --task run-spark` | ~3h |

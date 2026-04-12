# RL Training KPIs

Code RL systems use compilation success as the reward signal. For Rust projects,
compile time is 70–90% of the rollout phase — the main bottleneck of the
training loop. `cargo-slicer rl-bench` translates compile speedup into the KPI
language used by MLOps teams.

## Usage

```bash
# Measure current project (2 cold builds per mode)
cargo-slicer rl-bench

# Custom options
cargo-slicer rl-bench --runs 3 --rollout-fraction 0.85 \
  --gpus 16 --project /tmp/your-project

# Persist to bench-results.db
cargo-slicer rl-bench --db bench-results.db
```

## KPIs reported

**KPI 1 — Cold-build throughput (samples/hour)**

```
samples/hour = 3600 / compile_time_seconds
```

**KPI 2 — Incremental feedback latency**

Time from a one-line edit to the first `cargo check` result.

**KPI 3 — Compute cost per valid sample**

```
cost = compile_time / pass_rate
```

**KPI 4 — Cluster-hour equivalent**

How many RL samples fit in one GPU-cluster-hour at a given rollout fraction.

## Example output (nushell, 5.1× speedup)

Numbers below are nushell — a workspace that benefits from vslice-cc with a
clear ~5× win. A previous version of this example used zeroclaw and claimed
15.8×; that number was invalid (see `benchmarks.md` for the retraction).
zeroclaw's most recent measurement (2026-04-11, interleaved 3 rounds) is
**1.31× faster** (686 s → 522 s). Only 1.6% of zeroclaw's mono items are
stubbable overall, but 4.4% of its binary-target mono items are — those are
the heavy-LLVM ones, and seed-guided codegen filtering of them produces the
~160 s win.

```
  KPI 1 — Cold-Build Throughput (samples/hour)
    Baseline   :   596.6s  →       6 samples/hr
    cargo-slicer:  117.0s  →      30 samples/hr  (5.1× faster)

  KPI 2 — Incremental Feedback Latency (cargo check)
    Baseline   :    12.4s  →     290 feedback-loops/hr
    cargo-slicer:    4.1s  →     878 feedback-loops/hr  (3.0× faster)

  Cluster-Hour Equivalent (8 GPUs, 80% rollout fraction)
    Baseline   :      48 samples / cluster-hour
    cargo-slicer:     240 samples / cluster-hour  (5× more data)
```

## Persisting results

Results are written to the `rl_kpi` table in `bench-results.db`:

```sql
SELECT project, baseline_cold_secs, slicer_cold_secs, speedup,
       slicer_throughput_per_hr, ts
FROM rl_kpi ORDER BY ts DESC LIMIT 10;
```

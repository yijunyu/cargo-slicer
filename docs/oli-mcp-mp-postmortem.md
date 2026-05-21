# Postmortem — why "MIR-precise" stubs more functions but builds slower

*Companion to [oli-mcp-blog-draft.md](oli-mcp-blog-draft.md), 2026-05-21.*

cargo-slicer has two modes for deciding which function bodies to stub:

- **vslice-cc** (syn-based): parse every workspace crate's source, BFS
  from `main` to compute the reachable set, write per-crate `.seeds`.
- **vslice-cc-mp** (MIR-precise): additionally deserialise every
  workspace crate's optimized MIR from `.rmeta` and refine the seed
  set with monomorphisation-aware reachability.

Mp stubs more items. The intuition is that it should therefore save
more codegen time. Internal notes from February 2026 recorded
zeroclaw at **1,060 monomorphic items stubbed (MP) vs 799 (syn)** and
attached "27% vs 9% speedup" to the comparison. The 27% figure had
since been informally cited as MP's speedup over syn.

When we re-measured on wall time with the current toolchain — 36
single-shot timings across 4 projects × 3 modes × 3 rounds — that
interpretation did not survive.

## Numbers

| project | baseline | vslice-cc | vslice-cc-mp | mp-gap |
|---|---|---|---|---|
| ripgrep | 11 s | 8 s (1.38×) | 11 s (1.00×) | -37.5% |
| helix | 74 s | 47 s (1.57×) | 69 s (1.07×) | -46.8% |
| nushell | 112 s | 86 s (1.30×) | 121 s (0.93×) | -40.7% |
| zeroclaw | 685 s | 525 s (1.30×) | 756 s (0.91×) | -44.0% |

On wall time, MIR-precise is **30–47% slower than syn-based** on every
project measured. On ripgrep it matches baseline; on nushell and
zeroclaw it loses to baseline.

## Why the original "27%" reading was wrong

The February notes recorded a *count* (1,060 vs 799 mono items) and a
*speedup* (27% vs 9%) in the same paragraph. The implicit claim — more
items stubbed means a bigger wall-time win — is the kind of claim
that's easy to type and hard to verify. The two columns were
attached to different projects (an early run on a different workload),
not the same project measured both ways.

The actual wall-time gap is the opposite sign.

## What we think is happening

We don't know which factor dominates, but three are plausible:

- **Harvest cost.** Deserialising every workspace crate's optimized
  MIR on each binary compile is not free. MP pays this cost on every
  build; syn pays nothing comparable (its `.seeds` files are already
  on disk from the pre-pass).
- **Codegen-unit deduplication.** Many of the "extra" items MP stubs
  may already be deduplicated by rustc's CGU partitioning. Stubbing
  them removes nothing from LLVM's input.
- **Parallel-frontend contention.** MP exercises `optimized_mir`
  queries on extern crates from inside the binary compile. Recent
  nightly's parallel frontend may serialise on this query in a way
  that wasn't visible when the original notes were taken.

Distinguishing these would need more instrumentation. We didn't, because:

## The decision

The MCP proposal ships with the syn-level oracle only.

MIR-precise was a research path. On the workloads we care about, it
doesn't pay its own cost — and once that's true, the upstream
maintenance burden of carrying an MP code path in rustc isn't
justified by the perf data we have.

If a future workload shows MP actually winning on wall time, the
oracle format is flexible enough to extend later. For now, simpler is
better.

## Lesson

A count is not a speedup. The February notes were not wrong about
1,060 vs 799; they were wrong to let that comparison stand in for a
wall-time claim. The fix is mechanical — when measuring an
optimisation, the unit is wall time, never an intermediate count —
but it's worth writing down because it's the kind of mistake that
compounds: a half-remembered number becomes a citation, becomes a
design assumption, becomes a maintenance burden.

The 36-data-point re-measurement took an afternoon. The five months
of "MP wins by 27%" floating around in design conversations did not.

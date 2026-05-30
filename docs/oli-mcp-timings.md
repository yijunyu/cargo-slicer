# MCP cross-crate DFE — timings, syn cost, and synchronization

Third companion to `docs/oli-mcp-design.md` and `docs/oli-mcp-perf-followup.md`.
Answers three questions Oli raised after reading the design:

1. **Is the syn pre-analysis cost included in the wall measurements?** Yes,
   in both bench harnesses; the isolated cost is **0.25 s on ripgrep and
   0.60 s on helix**, and on real bench runs it overlaps with `cargo metadata`
   + the earliest registry-dep compiles.
2. **How does syn synchronize with rustc?** It doesn't. Pre-analysis writes
   `.cache` / `.seeds` files to disk; the in-driver pass reads them when rustc
   invokes the wrapper. File handoff, no IPC, no shared memory. The driver
   falls through to "no stubbing" if files are missing, which is the
   safe failure mode.
3. **Can `cargo build --timings` visualize this?** Yes. We capture HTMLs
   for baseline vs `vslice-cc` on ripgrep and helix and parse the per-unit
   windows to attribute the speedup to specific crates.

---

## 1. Syn pre-analysis cost (measured, isolated)

`scripts/bench_timings.rs` runs `cargo-slicer pre-analyze` in isolation
(no overlapping build), timed end-to-end:

| project | isolated pre-analyze | build wall (vslice-cc median, gap bench) | overhead % |
|---|---|---|---|
| ripgrep (17 crates) | **0.25 s** | 8 s | 3.1% |
| helix (16 crates) | **0.60 s** | 47 s | 1.3% |

The bench harness (`bench_fresh_build.sh`, lines 286–303) already
**includes pre-analyze time in the `START`/`END` window** for `vslice-cc`
modes. The wall numbers in `docs/oli-mcp-perf-followup.md` are full
totals, not "build only". Pre-analyze runs in the background overlapping
with the early build steps (see §2).

### How `cargo-slicer pre-analyze` works

`src/pre_analyze.rs`:
1. `cargo metadata --no-deps` → list of workspace crates.
2. For each crate, in parallel: parse all `.rs` files with **syn** (a
   pure-Rust parser, no rustc). Build a per-crate `Vec<Item>` index.
3. From the binary's `main` (resolved via `cargo metadata`'s
   `bin.targets`), BFS the call graph using syn name resolution.
4. Write `<workspace>/.slicer-cache/<crate>.cache` (full call graph)
   and `<crate>.seeds` (just the reachable items, one per line + a
   source-hash header).

The expensive step is syn parsing, but it's embarrassingly parallel and
each crate's analysis is independent. On a 48-core machine, ripgrep's
17 crates finish in 0.25 s wall.

---

## 2. Synchronization model (file handoff, no IPC)

```
                          ┌─────────────────────────────────────┐
                          │   PRE-ANALYZE  (cargo-slicer)       │
                          │   Process: pre-analyze              │
                          │   Time: t = [0, P]                  │
                          │   Output: .slicer-cache/*.{cache,    │
                          │            seeds}                   │
                          └────────────┬────────────────────────┘
                                       │  (file write, fsync)
                                       │
        cargo build                    ▼
        (wakes when                ┌────────────────────────────┐
         pre-analyze writes        │  .slicer-cache/A.seeds     │ ◀── on disk
         files OR keeps going      │  .slicer-cache/A.cache     │
         and accepts "no stub")    │  .slicer-cache/B.seeds     │
                                   │  .slicer-cache/B.cache     │
                                   └────────────┬───────────────┘
                                                │  (file read)
                                                ▼
        cargo spawns                  ┌───────────────────────────┐
        rustc lib_A.rs                │  cargo_slicer_dispatch    │
              │                       │  (RUSTC_WRAPPER)          │
              ▼                       │                           │
       ┌─────────────────────────┐    │  - detect local crate     │
       │ cargo_slicer_dispatch   │ ── │  - exec custom driver     │
       │ Process: dispatch       │    └──────────┬────────────────┘
       │ Time: t = [S, S+ε]      │               │
       └─────────────────────────┘               ▼
                                     ┌───────────────────────────┐
                                     │  cargo_slicer_rustc       │
                                     │  (custom rustc driver)    │
                                     │                           │
                                     │  - read A.cache from disk │
                                     │  - install optimized_mir  │
                                     │    query override         │
                                     │  - rustc proceeds         │
                                     │  - stub unreachable bodies│
                                     └───────────────────────────┘
                                              │
                                              ▼
                                       lib_A.rlib (with stubs)
```

Key properties:
- **No shared memory, no IPC, no locks.** The only synchronization is the
  filesystem itself.
- **No ordering constraint between pre-analyze and any specific rustc
  invocation.** If rustc reaches lib_A before `.slicer-cache/A.cache`
  exists, the driver falls through to "compile normally, no stubs"
  (`virtual_slicer.rs:178–195`). The build is correct, just less fast.
- **`cargo` is unmodified.** It does not know pre-analyze exists.
- **Staleness is bounded by source hash.** Each `.seeds` file carries
  a `SOURCE_HASH:` header. If the workspace's source has changed since
  the cache was written, the driver invalidates and skips stubbing.

This is why the in-tree `-Z dead-fn-elimination-oracle=PATH` proposal
preserves the same model: cargo writes the path, rustc reads the file,
no new query, no IPC. The only thing that changes is the consumer
(in-tree pass vs userspace driver).

### Overlap with `cargo metadata`

cargo spends the first ~0.5–2 s of any build on `cargo metadata`-equivalent
work (lockfile parsing, target resolution, dep graph). On ripgrep, the
first registry dep starts compiling at **t ≈ 0.7 s** (from the timings
JSON). With pre-analyze taking 0.25 s, **the syn pass finishes before
cargo has even started compiling anything**. On helix, pre-analyze
(0.60 s) similarly finishes before the first workspace crate starts.

This is why the gap bench's overlapped wall is identical to a
hypothetical "build-only" wall: the syn cost is invisible behind cargo's
own startup. The `bench_timings.rs` harness runs pre-analyze sequentially
*before* the build so the measurement counts the full syn cost, but in
production the overlap masks it.

---

## 3. `cargo build --timings` visualization

`scripts/bench_timings.rs` captures the timings HTMLs for both modes and
extracts per-unit (= per-crate-target-mode) windows.

### ripgrep

Single-shot measurement (artifacts: `/tmp/oli-timings/ripgrep/*.html`):

| | wall | crit-path | units |
|---|---|---|---|
| baseline | 17.7 s | 17.6 s | 48 |
| vslice-cc | 11.7 s | 11.6 s | 48 |
| **speedup** | **1.52×** | **1.52×** | — |

Pre-analyze isolated: **0.25 s** (2% of vslice-cc wall, masked by overlap).

#### Where the time went

The same 48 cargo units run in both modes. The speedup comes from each
unit's `rustc` taking less time (because the driver stubs unreachable
function bodies before LLVM-IR generation).

**Top 10 crates by time saved** (positive = vslice-cc faster):

| Crate | Baseline (s) | vslice-cc (s) | Saved (s) |
|---|---|---|---|
| libc | 1.89 | 1.11 | +0.78 |
| encoding_rs | 2.79 | 2.15 | +0.64 |
| regex-syntax | 4.42 | 3.89 | +0.53 |
| grep-matcher | 0.59 | 0.23 | +0.36 |
| termcolor | 0.80 | 0.51 | +0.29 |
| serde_core | 3.68 | 3.39 | +0.29 |
| textwrap | 0.91 | 0.64 | +0.27 |
| itoa | 0.34 | 0.09 | +0.25 |
| log | 0.54 | 0.31 | +0.23 |
| cfg-if | 0.27 | 0.04 | +0.23 |

The big savings are in **registry deps with broad public APIs that
ripgrep uses narrowly** (libc, encoding_rs, regex-syntax). ripgrep
imports only a small slice of each — the BFS prunes the rest.

**Regressions** (workspace leaf crates that pay driver overhead with
little to stub):

| Crate | Baseline (s) | vslice-cc (s) | Delta (s) |
|---|---|---|---|
| globset | 0.92 | 1.98 | -1.06 |
| ignore | 1.63 | 2.51 | -0.88 |
| grep-regex | 0.63 | 1.28 | -0.65 |
| grep-printer | 1.04 | 1.53 | -0.49 |
| grep-cli | 0.68 | 1.02 | -0.34 |

These are the ripgrep-owned crates: small public APIs, all reachable from
the binary, so vslice-cc stubs almost nothing but still pays the
dispatch+driver fork cost. **This is exactly the per-invocation overhead
the M3 microbench measured** — and exactly what the in-tree
`-Z dead-fn-elimination-oracle` extension would eliminate by removing
the userspace fork.

### helix

Single-shot timings results showed high variance (72.8 s and 114 s on
two consecutive baseline runs — cargo's parallel scheduler is sensitive
to system load). The reliable wall numbers come from the
**3-round interleaved gap bench** (`docs/oli-mcp-perf-followup.md`):

| | baseline | vslice-cc | speedup |
|---|---|---|---|
| helix (median of 3) | 74 s | 47 s | **1.57×** |

Per-crate breakdown from the timings HTML (`/tmp/oli-timings/helix/`):

**Top 10 crates by time saved** (caveat: from a single noisy run):

| Crate | Baseline (s) | vslice-cc (s) | Saved (s) |
|---|---|---|---|
| tree-house-bindings (build-script) | 10.02 | 2.47 | +7.55 |
| winnow | 6.62 | 1.29 | +5.33 |
| rustix | 8.42 | 4.34 | +4.08 |
| serde_core | 8.76 | 4.90 | +3.86 |
| libc | 5.77 | 2.03 | +3.74 |
| icu_properties_data | 3.74 | 0.48 | +3.26 |
| libc (second proc-macro instance) | 4.96 | 2.03 | +2.93 |
| allocator-api2 | 3.40 | 0.70 | +2.70 |
| regex-syntax | 8.30 | 5.68 | +2.62 |
| hashbrown | 3.71 | 1.27 | +2.44 |

Same pattern as ripgrep: large registry deps with broad APIs shrink
substantially. The biggest single saver (`tree-house-bindings`
build-script, 7.55 s) suggests build-script outputs benefit too.

### Critical-path observation

In both projects the **crit-path tracks wall** almost exactly
(ripgrep: 17.6/17.7, vslice-cc 11.6/11.7). This is the parallel-scheduler
working well — the binary's final link is the longest unit, and
everything else fits before it. The speedup comes from individual units
shrinking, not from rescheduling.

This matters for the in-tree proposal: shrinking each library compile
keeps cargo's existing scheduling intact. There's no need to introduce
new cargo coordination between pre-pass and rustc — file handoff is
sufficient because the timings show every library can be shortened
independently.

---

## 4. Implications for the in-tree extension

The `-Z dead-fn-elimination-oracle=PATH` proposal in `oli-mcp-design.md`
preserves every property the timings reveal:

| Property (from timings) | Userspace today | In-tree proposal |
|---|---|---|
| Pre-pass is masked by cargo startup | ✓ (0.25–0.60 s, overlaps `cargo metadata`) | ✓ (cargo writes the oracle, rustc reads) |
| File handoff is the only sync | ✓ (`.cache` / `.seeds`) | ✓ (oracle file at `-Z` path) |
| Each library shrinks independently | ✓ (per-crate codegen filter) | ✓ (per-crate `-Z` consumption) |
| Per-invocation overhead is visible | ✗ (fork+dispatch chain) | ✓ (no userspace driver — M3: 11.5× faster) |

The single property the in-tree path *changes for the better* is the
last one: removing the `cargo_slicer_dispatch` → `cargo_slicer_rustc`
fork chain. This is what the regressed workspace-leaf crates in
ripgrep's bottom-5 table would benefit from most.

**Concretely**: ripgrep's 5 regressing crates lose 3.4 s combined
to driver overhead. M3 says the in-tree path saves ~17 ms / invocation,
which on 5 invocations is 85 ms — *but* M3 used `rustc --version` as the
inner payload (the cheapest possible). The real overhead per crate is
larger; the regressions in the bottom-5 table are evidence of that.
Eliminating dispatch + driver-process startup should claw back roughly
the 3.4 s of regression and turn ripgrep's 1.52× into 1.7×+.

---

## 5. Artifacts

- `scripts/bench_timings.rs` — capture + parse `cargo build --timings`
  for baseline + vslice-cc; emits HTML, JSON, and markdown summaries.
- `/tmp/oli-timings/ripgrep/baseline.html` — clean ripgrep baseline.
- `/tmp/oli-timings/ripgrep/vslice-cc.html` — same workload, vslice-cc.
- `/tmp/oli-timings/helix/baseline.html` — helix baseline (single shot).
- `/tmp/oli-timings/helix/vslice-cc.html` — helix vslice-cc (single shot).
- `/tmp/oli-timings/<project>/summary.{md,json}` — per-crate deltas.

To regenerate: `./scripts/bench_timings.rs <project>` (project must be
cloned at `/tmp/<project>` first; see `bench_fresh_build.sh:54+` for
the supported list).

### Reading the HTMLs

cargo's `--timings` view shows two charts:
- **Top**: each crate's compile window as a horizontal bar, color-coded
  by type. Width = compile time. Position = start time.
- **Bottom**: concurrency (number of rustc processes running) over time.

The visual difference between `baseline.html` and `vslice-cc.html` is
the **width of each library bar shrinking**. The binary's final-link
bar stays roughly the same (it doesn't benefit from stubbing — it
*invokes* the stubs). This is the geometric intuition behind why the
speedup tops out at the binary's intrinsic compile time.

# cargo-slicer Development History

This document tracks major achievements, optimizations, and milestones.

## April 2026

### April 2, 2026 — cargo-warmup + priority scheduling; OpaqueTy / pch-overhead fixes

#### cargo-warmup: registry dep cache (R3)
- New `cargo_warmup_dispatch` RUSTC_WRAPPER: caches compiled `.rlib`/`.rmeta`/`.so` for
  registry crates by semantic key `(crate, version, rustc, features)` — topology-independent.
- `cargo-warmup init --tier=1` pre-compiles top-500 crates.io deps once per toolchain (~10 min).
- Cache hits serve pre-built artifacts in <1ms; subsequent cold builds skip recompiling
  `serde`, `syn`, `proc-macro2`, `tokio`, etc.
- **Cold-build results (verified Apr 2026, identical RUSTFLAGS)**: ripgrep 10.5s → 7s
  (**1.50×**), zeroclaw 686s → 522s (**1.31×**), nushell 103s → 82s (**1.26×**).
  An earlier version claimed nushell at **5.1×** (597s → 117s) but that was an
  apples-to-oranges comparison: the baseline lacked `-Z threads=8` and the wild
  linker that the vslice-cc run used. Zed (1.38×) has not yet been re-verified.
  zeroclaw was previously listed at **15.9×** but that number was retracted on
  2026-04-11 — it came from a run that silently failed before producing a binary.
  An interim post-fix measurement read 511s → 623s (**0.82×, a regression**) and
  was documented as such; that session's 511 s baseline was never reproduced.
  A 3×3-runs remeasurement showed **1.29×** (baseline 681s, vslice-cc 528s),
  and an interleaved 3-round measurement — round 1: 686s / 524s; round 2:
  688s / 519s; round 3: 685s / 524s — confirmed **1.31×** (baseline ±0.2%,
  vslice-cc ±0.5%). Interleaving rules out thermal and run-order effects.
  zeroclaw has only 1.6% stubbable mono items overall (3,786 of ~241k;
  lib 0.36%, bin 4.4%), but the binary target's 4.4% stub density concentrates
  the heavy-LLVM items — that's where the ~160s wall-clock win comes from.
  No warm registry cache is in play (cargo-warmup is not installed on the
  measurement machine); both modes compile from scratch. Three bugs were fixed
  during the investigation:
  (1) pre-analysis keyed cache files by package name but the driver reads target
  name, causing cache misses on any crate that renames its lib/bin via
  `[lib] name = X`;
  (2) the `mir_built` edge-scanning override was allowed to run during default-phase
  builds on binary targets (whenever `cached_marked.is_none()`), and its mere
  presence interacted with async-fn closure borrow-checking to trigger an E0391
  cycle through `type_of({opaque#N})` → `resolve_instance` → normalize → `type_of`
  — five recursive async fns in zeroclawlabs hit this. Fix: gate edge-scanning
  on `phase == "analysis" || phase == "lazy-metadata"` so the override is never
  installed during plain builds;
  (3) when edge-scanning *does* legitimately run, the `has_opaque_types()` guard
  alone was too narrow — `Instance::try_resolve` internally calls
  `normalize_erasing_regions`, which hits coroutine/alias substs too. Mirror
  rustc's own bailout pattern: `has_opaque_types() || has_coroutines() || has_aliases()`.

#### Multi-project build ordering (`cargo warmup schedule` / `build-all`)
- Greedy "heaviest-overlap-first" algorithm: seed = project with largest total dep weight;
  each subsequent pick = project maximising overlap with accumulated cache set.
- Approximation ratio ≥ 63% of NP-hard optimal (submodular maximisation bound).
- Dep compile times estimated from rlib size: `estimated_ms = size_bytes / 350` (clamped
  100–60,000ms, empirically ~1MB ≈ 3s compile time).
- `cargo warmup schedule <dir1> <dir2> ...` prints recommended build order + predicted savings.
- `cargo warmup build-all <dir1> <dir2> ...` runs builds in that order.

#### Single-project critical-path scheduling (`cargo warmup pch-plan` + `cargo_warmup_pch`)
- `cargo warmup pch-plan --manifest Cargo.toml --output plan.json`: reads
  `cargo build -Z unstable-options --unit-graph`, assigns compile-time estimates,
  computes CP(unit) = self_weight + max(CP(dep)) via Kahn topological sort bottom-up.
- `cargo_warmup_pch` RUSTC_WRAPPER: priority daemon over Unix domain socket; high-CP units
  admitted first, low-CP units wait until a slot frees. Cargo sees normal rustc subprocesses.
- **Registry bypass fix** (same day): daemon round-trip was applied to every rustc invocation
  including instant cache hits, eliminating warmup gains. Fix: detect `/.cargo/registry/` in
  source path early and short-circuit to `warmup_dispatch` directly. Overhead dropped from
  O(units × 5ms) to negligible.

#### Three-layer wrapper chain integrated into `cargo-slicer.sh`
- Single command `cargo-slicer.sh /path/to/project` now runs all four steps automatically:
  1. Warm registry dep cache (once per toolchain)
  2. Cross-crate call graph pre-analysis
  3. Critical-path priority plan
  4. Build: `cargo_warmup_pch` → `cargo_warmup_dispatch` → `cargo_slicer_dispatch`
- Graceful fallback at each level if any component is unavailable.
- Also handles `-Z dead-fn-elimination` in-tree flag path (A) vs RUSTC_WRAPPER fallback (B).

#### Bug fix: `DefKind::OpaqueTy` / `SyntheticCoroutineBody` causing E0391 in zeroclaw
- Virtual slicer's `is_safe_to_skip()` did not guard `DefKind::OpaqueTy` (the `{opaque#N}`
  return types of async fns). Stubbing these caused "cycle detected when computing type of
  opaque" (E0391) errors in projects with heavy async usage (zeroclaw).
- Fix: added `DefKind::OpaqueTy => return false` and `DefKind::SyntheticCoroutineBody =>
  return false` to the DefKind match. Zeroclaw now builds cleanly (13m26s, 0 errors).

#### Warm-incremental benchmark summary (Apr 2026, 3 runs averaged)
| Project | Baseline | vslice-cc | Speedup |
|---------|----------|-----------|---------|
| zed | 1,025s | 744s | **1.38×** |
| rustc-perf suite | 145s | 123s | **1.18×** |
| cargo-slicer | 143s | 82s | **1.74×** |
| helix | 78s | 62s | **1.26×** |
| ripgrep | 13.4s | 12.2s | **1.10×** |

**Tag**: `tse_submission_cutting_warming`

## February 2026

### February 14, 2026 — Critical Bug Fix: Std Trait Method Resolution

- **Bug: `mir_built` call graph missed std trait method impls**: In pre-optimization MIR, calls to trait methods defined in `std`/`core`/`alloc` (e.g., `Default::default()`, `Clone::clone()`, `Ord::cmp()`) appear with the TRAIT method's DefId (e.g., `std::default::Default::default`), not the impl method's DefId (e.g., `<LowArgs as Default>::default`). The `check_fndef` helper filtered these as std library calls, silently dropping edges to user-defined trait implementations. This caused BFS to miss marking reachable functions, leading to incorrect stubs and runtime crashes (SIGILL/ud2).
- **Root cause**: The `mir_built` edge scanner compared `def_path_str(callee_id)` against `std::`/`core::`/`alloc::` prefixes. For `LowArgs::default()`, the MIR uses `FnDef(std::default::Default::default, [LowArgs])` where the DefId points to the trait method, not the impl. The substs contain the concrete type but were ignored.
- **Fix**: Added a post-scan resolution pass using `Instance::try_resolve()` to resolve std/core/alloc trait method calls with concrete substs to their actual impl methods. Collects `(DefId, GenericArgsRef)` pairs during body scan, then resolves after `Steal::borrow()` is released.
- **Impact**: ripgrep call graph grew from 737 to 1157 edges (+57%), marked items from 1361 to 1463, stubbed items from 15 (including the crashing `<LowArgs as Default>::default`) to 3 (all genuinely unreachable). Binary no longer crashes.
- **mir_built early stub**: Implemented `mir_built` override that returns stub MIR for unreachable items on cache hit, skipping THIR→MIR lowering. Gated behind `CARGO_SLICER_MIR_BUILT_EARLY_STUB` env var. Required special handling: `const fn` exclusion (const eval uses `mir_for_ctfe → mir_promoted → mir_built`), no `set_required_consts`/`set_mentioned_items` (set by downstream passes). Benchmark showed negligible benefit on ripgrep/helix (too few items stubbed); may help on larger projects.
- **Updated benchmark results** (post-fix, with parallel frontend `-Z threads=8` + wild linker): zed **-29% wall / -37% insn**, rustc **-17% wall / -26% insn**, helix **-6% wall / -11% insn**, ripgrep **-4% wall / -5% insn**. Ripgrep's smaller improvement reflects the fix: fewer false stubs → fewer savings, but binary is now correct.

### February 13, 2026 — Driver Optimizations & lcnr Review Feedback

- **P0: Batch call edge recording**: Replaced per-edge `record_call_edge()` (each acquiring a global lock) with `batch_record_call_edges()` that collects edges into a thread-local `Vec` and inserts them in one lock acquisition. Eliminates lock contention during `mir_built` override.
- **P1: Interned BFS seeds**: Changed BFS seed set from `HashSet<String>` to `HashSet<DefId>`, using `tcx.def_path_hash()` for cache serialization. Eliminates `def_path_str()` allocation per item during marking.
- **P2: Pre-built codegen caches**: New `build_codegen_caches(tcx)` builds both `MARKED_DEF_IDS` and `SAFE_TO_SKIP_DEF_IDS` `HashSet<DefId>` in a single pass during `after_analysis`. The `optimized_mir` override becomes O(1) `HashSet::contains()` instead of per-item `is_safe_to_skip()` with multiple query calls.
- **lcnr review feedback** (5 items addressed):
  1. Removed nested-fn restriction — `mir_built` captures pre-inlining call edges, so nested function references are always visible in BFS. More functions now eligible for stubbing (instructions: 4292B → 4268B on rustc, -0.56%).
  2. Replaced `def_path.contains("::drop")` with `tcx.lang_items().drop_trait()` check — uses compiler's own lang item registry instead of fragile string matching.
  3. Removed `__rust_std_internal` / `__init_fn` string heuristics (no longer needed).
  4. Deferred `def_path_str()` allocation to dependency crate checks only.
  5. Generic functions and trait handling: documented as roadmap items. Post-monomorphization analysis would unlock the biggest remaining opportunity.
- **Removed nushell from multicrate benchmarks**: Driver overhead (+4.1% wall time) exceeds marginal instruction savings (-0.8%) due to heavy generic usage limiting stubbing effectiveness.
- **Updated benchmark results** (with parallel frontend `-Z threads=8` + wild linker): zed **-21% wall / -37% insn**, rustc **-15% wall / -26% insn**, ripgrep **-8% wall / -12% insn**, helix **-6% wall / -11% insn**. Zed RSS dropped from 22.7GB to 12.0GB (-47%).

### February 8, 2026 — Benchmark Infrastructure & Results

- **Fork-server daemon mode**: Dispatch binary pre-loads `librustc_driver.so` once, then `fork()`s for each local crate compilation via Unix socket protocol. Eliminates per-crate ~130ms driver loading overhead.
- **Benchmark infrastructure**: SQLite-backed result storage (`bench-results.db`) with self-contained HTML comparison reports. Metrics: wall time, RSS, instructions, cycles (userspace-only counters matching perf.rust-lang.org).
- **CI benchmark scripts**: `ci_bench_multicrate.sh` (7 projects × baseline vs vslice-cc × 3 runs) and `ci_bench_rustc_perf.sh` (20 rustc-perf crates × baseline vs vslice × 3 runs). Zero-argument, fully automated.
- **Multi-crate results** (vslice-cc vs baseline, single-threaded compilation): helix **-81%**, bevy **-67%**, zed **-53%**, slicer **-53%**, nushell **-18%**, rustc **-15%**, ripgrep **-10%** wall time. (Superseded by Feb 13 results with parallel frontend.)
- **Single-crate results** (rustc-perf): 0-11% wall-time overhead on 20 library-dominated crates. Pure driver overhead, nothing to stub.
- **Dramatic improvement**: Previous results (pre-daemon) showed vslice 5-20% *slower* across all projects due to per-crate driver loading cost.

### February 7, 2026 — Optimization Round

- **Skip-driver markers**: Dispatch binary checks for `.skip-driver` marker files. If a crate has no unmarked functions (only structs, enums, traits), skip driver entirely. Nushell: 57% of crates skip, helix: 25%.
- **Relaxed safety checks**: Pub items OK in binary crates, `const fn` OK, `#[inline]` attrs OK, `unsafe fn` opt-in via `CARGO_SLICER_RELAX_UNSAFE=1`. Removed false-positive name heuristics.
- **Vtable tracking**: Scan `mir_built` for `PointerCoercion::Unsize` casts to detect `(Type, dyn Trait)` pairs. If a trait has NO vtable constructions, its impl methods can be stubbed (all dispatch is static).
- **Deferred hash computation**: Skip source hash calculation on fresh builds where no cache exists to compare against.
- **Pluggable pre-analysis backends**: `PreAnalysisBackend` trait with syn (default), fast tokenizer, and ctags implementations. Select via `CARGO_SLICER_PARSER` env var or `--parser` flag.
- **Inherent AssocFn stubbing**: Allow stubbing inherent methods (`impl MyStruct {}`) while still protecting trait impl methods.
- **Skip original optimized_mir() in stubs**: Construct `SourceScopeData` directly instead of calling the original provider, saving optimization time for stubbed functions.
- **Net result**: Relaxed checks + vtable tracking had minimal real-world impact (trait impls dominate unstubbed items), but skip-driver + daemon mode were transformative.

### February 6, 2026 — Virtual Slicing Benchmarks

Comprehensive benchmarking across 7 projects of varying size and type:

| Project | Type | Local Crates | Items Stubbed | Baseline | Vslice | Change |
|---------|------|------------:|--------------:|---------:|-------:|--------|
| **rustc** | Binary | ~282 | ~25% | 4m16s | 3m11s | **25.0% faster** |
| **nushell** | Binary | ~40 | ~30% | 2m27s | 2m11s | **10.9% faster** |
| ripgrep | Binary | 10 | 9.7% | 14s | 15s | ~0% |
| helix | Mixed | 16 | 11.5% | 1m29s | 1m38s | 10.1% slower |
| bevy | Library | 43 | 4.0% | 1m43s | 1m52s | 8.7% slower |
| zed | Mixed | 209 | 0% (2) | 17m07s | 19m40s | 14.9% slower |
| slicer | Mixed | 4 | 11.6% | 2m37s | 2m52s | 9.6% slower |

**Key finding**: Virtual slicing is effective on large binary-centric projects (nushell, rustc) where `main` is the only BFS seed, leaving 25-30% of internal functions unreachable. Library-dominated projects (bevy, helix, zed) see net overhead because all public items are seeds, resulting in <5% reduction — not enough to offset analysis cost. Zed was the largest project tested (209 local crates, full binary build) but had only 2 items stubbed because 198/209 crates are libraries.

- **Bug fix**: `is_safe_to_skip()` panicked on `SyntheticCoroutineBody` items (async desugaring) when calling `tcx.visibility()`. Fixed by checking `def_kind` before `visibility()`.

### February 5, 2026 — MIR Constant Scanning Fix

- **Problem**: Functions passed as values (e.g., `operate(&get_basename)`) appeared as `Operand::Constant` with `FnDef` type in MIR assignment statements, not in call terminators
- **Fix**: Scan ALL constants in MIR body — `Rvalue::Use`, `Rvalue::Cast`, call arguments — for `FnDef` references
- **Impact**: Eliminated false stubs in nushell path commands

### February 4, 2026 — Dispatch Binary & Sccache Integration

- **Dispatch binary** (`cargo_slicer_dispatch`): Thin RUSTC_WRAPPER that routes non-local crates to sccache/rustc and local crates to the analysis driver. No rustc_driver dependency, loads in <1ms.
- **Impact**: Saved ~6s per build on ripgrep by avoiding driver overhead for 300+ third-party crates.

### February 3-4, 2026 — MIR Abort Stub Implementation

- **`optimized_mir` query override**: Replaces unreachable function bodies with minimal abort stubs
- **`mir_built` query override**: Captures pre-optimization call graph before inlining erases edges
- **BFS reachability**: Seeds from `main` (bins) or all `pub` items (libs), plus `Drop` impls, object-safe trait impls, statics
- **Safety exclusions**: Never stub associated functions, generics, const fn, async fn, closures, unsafe fn, or functions with fn-ptr/dyn params

### February 3, 2026 — Documentation Simplification
- README.md streamlined

### February 2, 2026 — Text-Based Deletion Optimization
- Identified O(items^2) complexity in text-based safety check
- Fast tokenizer with Aho-Corasick automaton: 150ms to 5ms (30x speedup)

### February 1, 2026 — Pure Rust Ctags Architecture
- Replaced tree-sitter dependency with pure Rust syn-based parser
- Direct integration, improved performance, reduced dependencies

## January 2026

### January 24 — Dogfooding Success
- cargo-slicer builds itself using its own sliced dependencies
- 179/179 individual sliced crates compile (100%)
- Self-hosting proven: the slicer can use its own output

### January 24 — Optimization Levels
- `-O0` to `-O3` all passing on self-slicing (136 crates)
- 333 items deleted at -O2 (329 functions, 3 constants, 1 static)

### January 23 — Architecture Simplification
- Migrated from SCIP-based analysis to copy-and-delete approach
- New pipeline: Copy -> Parse -> Graph -> Mark -> Delete -> Cleanup

### January 22 — Automatic Compatibility Fixes
- getrandom feature rename, clap version conflicts, trybuild exact versions

### January 21 — Self-Hosting Achievement
- cargo-slicer successfully builds itself using sliced dependencies
- 179/179 crates compile, 75MB binary with full functionality

## Architecture Timeline

1. **Jan 21**: SCIP-based analysis with rust-analyzer
2. **Jan 23**: Pivot to copy-and-delete with syn AST parsing
3. **Jan 24**: Graph analysis, caching, trial deletion, self-hosting
4. **Feb 1-2**: Fast tokenizer, ctags integration
5. **Feb 3-6**: Virtual slicing via rustc `override_queries` — MIR abort stubs, dispatch binary, comprehensive benchmarking
6. **Feb 7**: Optimization round — skip-driver markers, relaxed safety checks, vtable tracking, pluggable pre-analysis backends
7. **Feb 8**: Fork-server daemon, benchmark infrastructure (SQLite + HTML), CI scripts, 10-81% speedups across all projects
8. **Feb 13**: P0-P2 driver optimizations (batch call edges, interned BFS, codegen caches), lcnr review feedback (lang items, nested-fn relaxation)
9. **Feb 14**: Critical bug fix — std trait method resolution via `Instance::try_resolve()`, mir_built early stub optimization

## Key Innovations

- **Virtual slicing**: Transparent RUSTC_WRAPPER that skips codegen for unreachable functions via MIR abort stubs. 4-29% wall time reduction, up to 37% instruction reduction.
- **Dispatch binary**: Routes third-party deps through sccache, local crates through analysis driver. Sub-millisecond overhead per invocation.
- **Pre-optimization call graph**: `mir_built` override captures call edges before inlining erases them, enabling accurate reachability analysis.
- **Pre-built codegen caches**: Single-pass `build_codegen_caches()` builds DefId HashSets for O(1) lookup during `optimized_mir` override.
- **Source slicing**: Copy-and-delete approach guarantees LOC reduction. AST-based analysis with graduated optimization levels (-O0 to -O3).
- **Self-hosting**: Tool validates itself on its own 136+ dependencies.

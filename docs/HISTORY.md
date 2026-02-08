# cargo-slicer Development History

This document tracks major achievements, optimizations, and milestones.

## February 2026

### February 8, 2026 — Benchmark Infrastructure & Results

- **Fork-server daemon mode**: Dispatch binary pre-loads `librustc_driver.so` once, then `fork()`s for each local crate compilation via Unix socket protocol. Eliminates per-crate ~130ms driver loading overhead.
- **Benchmark infrastructure**: SQLite-backed result storage (`bench-results.db`) with self-contained HTML comparison reports. Metrics: wall time, RSS, instructions, cycles (userspace-only counters matching perf.rust-lang.org).
- **CI benchmark scripts**: `ci_bench_multicrate.sh` (7 projects × baseline vs vslice-cc × 3 runs) and `ci_bench_rustc_perf.sh` (20 rustc-perf crates × baseline vs vslice × 3 runs). Zero-argument, fully automated.
- **Multi-crate results** (vslice-cc vs baseline): helix **-81%**, bevy **-67%**, zed **-53%**, slicer **-53%**, nushell **-18%**, rustc **-15%**, ripgrep **-10%** wall time. All projects now faster with virtual slicing.
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

## Key Innovations

- **Virtual slicing**: Transparent RUSTC_WRAPPER that skips codegen for unreachable functions via MIR abort stubs. 10-81% faster release builds across projects.
- **Dispatch binary**: Routes third-party deps through sccache, local crates through analysis driver. Sub-millisecond overhead per invocation.
- **Pre-optimization call graph**: `mir_built` override captures call edges before inlining erases them, enabling accurate reachability analysis.
- **Source slicing**: Copy-and-delete approach guarantees LOC reduction. AST-based analysis with graduated optimization levels (-O0 to -O3).
- **Self-hosting**: Tool validates itself on its own 136+ dependencies.

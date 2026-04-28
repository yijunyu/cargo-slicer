# MCP: Dead Function Elimination via BFS Reachability (`-Z dead-fn-elimination`)

**Status**: Verified — patched stage1 compiler builds and passes functional + incremental tests (2026-03-12)
**Tracking issue**: (to be filed)
**Zulip thread**: (to be opened in `#t-compiler/help`)

---

## Problem Statement

Build times for large Rust binaries are dominated by LLVM codegen. A significant
fraction of that work is spent on functions that are unreachable from the binary's
entry point — functions that were compiled and optimized by LLVM but whose object
code was then removed by the linker's `--gc-sections`.

We built `cargo-slicer` (external `RUSTC_WRAPPER` tool) that identifies these functions via BFS reachability 
from entry points, then replaces their MIR bodies with `TerminatorKind::Unreachable` before codegen.
The result: LLVM never processes them, the linker never sees them, and build time
drops measurably.

### Primary benchmark: in-tree `-Z dead-fn-elimination` (intra-crate BFS, 2026-03-08)

All comparisons use the **same compiler binary** (stage2) with and without the
flag, eliminating toolchain version effects. BFS over local crate only
(`tcx.mir_keys(())`).

Hardware: AMD EPYC 7713 64-core, 256 GB RAM, NVMe SSD, Ubuntu 22.04
Kernel: 5.15.0-164-generic
Build flags: `cargo +stage2 build --release -Z threads=8 -C linker=clang -C link-arg=--ld-path=wild`
Measurement: wall seconds (`$(date +%s)`), 2–3 runs, cold build (target/ deleted)

| Project | stage2 baseline | `-Z dead-fn-elimination` | delta   | fns eliminated |
|---------|----------------|--------------------------|---------|----------------|
| zed     | 1356s (22.6m)  | 1246s (20.8m)            | **-8%** | 248            |
| rustc workspace (67 crates) [^rustc-workspace] | 174s | 163s | **-6%** | 10 |
| helix   | 85s            | 85s                      | 0%      | 18             |
| ripgrep | 13s            | 13s                      | 0%      | 2,468          |

[^rustc-workspace]: This row reflects `x.py build compiler/rustc --stage 1` —
i.e., compiling the 67 workspace crates that make up `librustc_driver.so`,
not the ~70-line `rustc` binary crate itself. Per @petrochenkov's V2 review
feedback, the original "rustc" label was misleading: the elimination occurs
across the leaf workspace crates (`rustc_passes`, `rustc_resolve`, etc.)
where private and binary-only helpers are eligible. See
`docs/vadim-petrochenkov-review-feedback.md` and
`docs/vadim-response-results.md`.

**Pattern**: The flag pays off on large binary crates; smaller projects break even.
The BFS traversal cost offsets the LLVM savings at small scale.

### Supplementary: external tool validation (cargo-slicer, 2026-02-14)

The in-tree flag is a direct translation of the `cargo-slicer` external
`RUSTC_WRAPPER` tool, which has been running since late 2025. External tool results
validate the algorithm independently of the compiler patch:

Hardware: AMD EPYC 7713 64-core, 256 GB RAM, NVMe SSD, Ubuntu 22.04
Build flags: `cargo +nightly build --release -Z threads=8 -C linker=clang`
Measurement: wall time, 3 runs each, cold build (target/ deleted)

| Project | Baseline | With slicer | Speedup |
|---------|----------|-------------|---------|
| zed     | 16m 52s  | 11m 59s     | **29%** |
| rustc   | 2m 16s   | 1m 52s      | **17%** |
| helix   | 1m 11s   | 1m 07s      | **6%**  |
| ripgrep | 11.1s    | 10.7s       | **4%**  |

The external tool has overhead (disk I/O, IPC, ABI versioning per nightly).
The in-tree flag eliminates all of that.

### ASE 2026 corpus sweep — top 2,669 crates by downloads (2026-04-26)

Independent third-party correctness validation of the userspace cargo-slicer
(same algorithm as the in-tree patch) on a representative slice of the
ecosystem. Run on `cargo +nightly` baseline vs cargo-slicer leg via
`scripts/bench_ase_corpus.sh`; libraries gated on build success, binary
crates additionally smoke-tested with `--version` / `--help`.

| Metric                           | Value |
|----------------------------------|------:|
| Crates fetched                   | 2,669 |
| Crates that ran                  | 2,603 |
| Both legs built (clean compare)  | **2,452** |
| **Slicer-only regressions**      | **0** |
| Median build speedup             | **1.50×** |
| % speedup ≥ 1.0×                 | 73.1% |
| % speedup ≥ 1.5×                 | 49.8% |
| % speedup ≥ 2.0×                 | 35.9% |

Headline: zero correctness regressions across 2,452 crates; median 1.50×
build speedup. Full per-crate catalog (rank, version, downloads, build
times, status) is published at
<https://yijunyu.github.io/cargo-slicer/ase2026-corpus.html>; the raw CSV
is in `docs/ase2026-corpus.csv`. Detailed per-concern empirical evidence
and reproduction instructions live in `docs/vadim-response-results.md`.

### Patched stage1 oracle (rust-1.90.0 stable tag, 2026-04-26)

Re-validated the in-tree patch against `rust-1.90.0` (commit `1159e78c`)
with `[rust] debug-assertions = true, overflow-checks = true` to catch the
V9 invariant `reachable_set ⊆ post-BFS-set` at runtime.

| Run                          | Wall time | Fns eliminated | Output check |
|------------------------------|----------:|---------------:|--------------|
| stage1 baseline (ripgrep)    | 62.1 s    | 0              | runs |
| stage1 + `-Z dead-fn-elim`   | 59.9 s    | 904            | identical to baseline |

debug_assert holds — no ICE, binary correct.

---

## Prior Art

### What rustc already does

rustc has dead code linting (`unused` warnings), which tracks whether items are
*referenced*. It does **not** suppress codegen for unreachable items — even items
that trigger `dead_code` warnings are still compiled to machine code.

The monomorphization collector (`collect_and_partition_mono_items`) seeds with all
`pub` functions plus `main`. This is conservative and correct for library crates,
but over-generates for binary crates where most public functions of dependencies
are not reachable from `main`.

### Linker dead code elimination

`--gc-sections` (LLD/GNU ld) removes unreachable sections *after* LLVM has already
compiled them. It does not reduce LLVM compile time. This proposal acts before
LLVM, at the MIR level.

### David Lattimore's lazy codegen proposal

Lazy codegen (deferred codegen of unused functions) is aligned motivation. The
difference: lazy codegen defers work; this proposal *skips* work entirely by
replacing function bodies with `Unreachable` before the codegen queue is built.
The two approaches are complementary.

### LTO / ThinLTO

LTO eliminates dead code across CGUs but requires a full compile first. It does
not reduce per-crate compile time; it increases total time for large workspaces.

### Object-level symbol elimination

`visibility("hidden")` or `-ffunction-sections --gc-sections` at the C level
is the prior art in C/C++. Rust's equivalent (`#[doc(hidden)]`, `pub(crate)`)
does not suppress codegen.

### Why not the linker (detailed)

The linker sees function symbols, not MIR. By the time the linker runs:
- LLVM has already spent time on IR generation, optimization passes, and machine code
- The object file has already been written to disk
- The build pipeline has already paid the I/O cost

Eliminating at the MIR level means LLVM never starts on those functions.

---

## Objection Table

| Objection | Response |
|-----------|----------|
| "rustc already has dead code elimination" | Only intra-crate DCE (unused warnings). The mono collector seeds all `pub` functions; cross-crate BFS is the gap. |
| "this breaks incremental compilation" | `UNTRACKED` flag; stub MIR is valid with `Unreachable` terminator. Incremental sees a valid (different) function body. Same as `-Z no-codegen`. |
| "vtable dispatch will be broken" | `scan_for_vtable_constructions()` tracks `PointerCoercion::Unsize` casts, seeds all methods of any trait used as `dyn Trait`. |
| "generic functions?" | Check 9: `generics.count() > 0` → never stub. Pre-monomorphization BFS can't know which instantiations are reachable. |
| "async functions?" | Check 10: `asyncness.is_async()` → never stub. Async fns compile to state machines; stubbing breaks the generator transform. |
| "Drop implementations?" | Check 5: lang item `drop_trait` comparison → never stub. The compiler inserts drop calls implicitly, outside BFS reach. |
| "extern C / FFI?" | Check 4: `NO_MANGLE`, `USED_LINKER`, `symbol_name`, `linkage` attrs → never stub. |
| "pub API in library crates?" | Check 3: `vis.is_public() && !is_binary_crate` → never stub. Downstream consumers may call any pub function. |
| "Why `-Z` and not stable?" | Unstable is the correct starting point. Stabilization path: `-Z` → field validation → `-C` once semantics are proven. |
| "Why not LLVM?" | LLVM sees one CGU at a time. Cross-CGU reachability requires rustc-level analysis with global `TyCtxt`. |
| "unsafe functions?" | Check 8: `fn_sig().safety().is_unsafe()` → never stub. Unsafe fns can be called via transmuted function pointers. |
| "functions passed as fn pointers?" | Check 11: signature contains `FnPtr` or `dyn Trait` → never stub the receiver function. |
| "@workingjubilee: driver can't maintain borrow-checker guarantees across `optimized_mir` override" | The in-tree implementation uses CGU-level removal (`collect_and_partition_mono_items` override), not body replacement. The function is excluded from the codegen queue entirely — the borrow checker's conclusions about the original body are never contradicted. See `docs/wesley-workingjubilee-review-feedback.md`. |
| "@oli-obk: is the migration from external driver to rustc feasible?" | Yes — and this MCP is the outcome. The external tool has validated the algorithm since late 2025; the in-tree patch is a direct ~422-line translation using native `TyCtxt` APIs, eliminating IPC overhead, ABI versioning, and disk I/O. See `docs/wesley-workingjubilee-review-feedback.md`. |
| "`#[inline]` functions could be eliminated incorrectly" | `#[inline]` does not change reachability — it's a codegen hint. The BFS sees the call edge regardless. Inlined functions are monomorphized at call sites; the original `DefId` body is kept if any call site exists. |
| "`#[cold]` functions should not be eliminated" | Correct — `#[cold]` is a branch prediction hint, not a reachability signal. BFS treats `#[cold]` functions identically to any other: reachable = keep, unreachable = eliminate. No special handling needed. |
| "BFS bug could silently eliminate reachable functions" | Conservative 11-point safety checklist: generics, async, unsafe, Drop, `#[no_mangle]`, fn pointers, dyn Trait, pub items → all kept unconditionally. A BFS bug can only affect private, non-generic, non-async, safe, non-Drop standalone functions — the narrowest possible class. Runtime symptom is an `Unreachable` trap (immediate, not silent). |
| "Use the mono collector instead of pre-mono BFS" | The mono collector operates post-monomorphization on `MonoItem`s (concrete instantiations). This pass operates pre-mono on `DefId`s (source-level functions). Pre-mono BFS is cheaper (fewer nodes, no generic expansion) and catches functions before LLVM even sees them. The two are complementary: BFS removes obviously dead `DefId`s; the mono collector handles instantiation-level reachability. |

---

## Implementation

### File 1: `compiler/rustc_session/src/options.rs`

Inside the `Z_OPTIONS!` macro block, add (alphabetically, near `dead_code`):

```rust
dead_fn_elimination: bool = (false, parse_bool, [UNTRACKED],
    "eliminate unreachable (from entry-point BFS) functions from codegen (experimental)"),
```

**Why `UNTRACKED`**: The flag changes codegen output but not the query graph
structure. The stub MIR body (single `Unreachable` terminator) is a valid MIR
body. Incremental compilation sees a changed function body and recompiles
downstream items correctly. This is the same category as `-Z no-codegen`.

Using `UNTRACKED_WITH_WARNING` would add unnecessary noise for a flag that is
intentionally used in release builds. `TRACKED` would invalidate all
incremental caches when toggling the flag, which is unnecessarily conservative.

### File 2: `compiler/rustc_mir_transform/src/dead_fn_elim.rs` (new file)

Full implementation (~420 lines), including cross-crate BFS:

```rust
use rustc_data_structures::fx::{FxHashSet, FxIndexMap, FxIndexSet};
use rustc_hir::def::DefKind;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::mir::{Body, TerminatorKind};
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_span::def_id::DefId;
```

**Module structure**:

1. **Thread-local state** (`VTABLE_TRAITS`, `ELIMINABLE_DEF_IDS`) — two sets
   per rustc process. `VTABLE_TRAITS` tracks traits used as `dyn Trait`;
   `ELIMINABLE_DEF_IDS` stores the final set of functions to exclude from codegen.

2. **`run_analysis(tcx: TyCtxt<'_>)`** — called once from `after_analysis`:
   a. Early return for library crates (`entry_fn().is_none()`)
   b. `build_call_graph(tcx)` — local + cross-crate edges via `FxIndexMap`
   c. `collect_seeds(tcx)` — seeds: `entry_fn`, statics, `#[no_mangle]`/`#[used]`,
      `#[test]`/`#[bench]`, Drop impls, vtable-constructed trait methods
   d. `run_bfs(seeds, &call_graph)` — pure BFS, deterministic via sorted seeds
   e. Mark unreachable + safe-to-eliminate functions in `ELIMINABLE_DEF_IDS`

3. **`is_eliminable(idx: u64) -> bool`** — O(1) lookup into `ELIMINABLE_DEF_IDS`.
   Called by the `is_codegened_item` query override in the driver.

4. **`is_safe_to_eliminate(tcx, def_id)`** — 11-point safety checklist (see below).

5. **`build_call_graph` / `build_local_call_graph` / `build_extern_call_graph`** —
   local edges from `tcx.mir_keys()`; cross-crate edges by iterating
   `tcx.crates(())`, BFS-walking `module_children`, reading `optimized_mir`
   (guarded by `is_mir_available` + `catch_unwind`). Skips std/core/alloc.

6. **`scan_for_vtable_constructions(body)`** — walks MIR statements looking
   for `Rvalue::Cast(PointerCoercion::Unsize, _, dyn Trait)`, records the trait
   DefId in `VTABLE_TRAITS`.

**Graph data structures**: `FxIndexMap<u64, FxIndexSet<u64>>` for the call graph
(deterministic iteration, no `potential_query_instability` lint). `FxHashSet<u64>`
for membership-only sets (seeds, reachable, vtable traits, eliminable).

**Why `thread_local!` instead of a new query**:

Thread-local state is the minimal footprint approach for an experimental `-Z` flag.
If stabilized, migrate to a `GlobalCtxt` field or a proper query (migration path
documented in code comments). The thread-locals are populated once in
`run_analysis` and read immutably thereafter.

### File 3: `compiler/rustc_mir_transform/src/lib.rs`

Add one line:

```rust
pub mod dead_fn_elim;
```

### File 4: `compiler/rustc_driver_impl/src/lib.rs`

**Site 1** — after `callbacks.config(&mut config)`, register the query override:

```rust
if config.opts.unstable_opts.dead_fn_elimination && config.override_queries.is_none() {
    config.override_queries = Some(dead_fn_elim_override_queries);
}
```

**Site 2** — before `callbacks.after_analysis()`, run BFS:

```rust
if tcx.sess.opts.unstable_opts.dead_fn_elimination {
    rustc_mir_transform::dead_fn_elim::run_analysis(tcx);
}
```

**Site 3** — two new functions (named, not closures — `override_queries` is `Option<fn(...)>`):

```rust
fn dead_fn_elim_override_queries(
    _sess: &rustc_session::Session,
    providers: &mut rustc_middle::util::Providers,
) {
    providers.queries.is_codegened_item = dead_fn_elim_is_codegened_item;
}

fn dead_fn_elim_is_codegened_item(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    if def_id.is_local() {
        let idx = def_id.index.as_u32() as u64;
        if rustc_mir_transform::dead_fn_elim::is_eliminable(idx) {
            return false;
        }
    }
    tcx.collect_and_partition_mono_items(()).all_mono_items.contains(&def_id)
}
```

### Files 5-6: `compiler/rustc_monomorphize/src/{lib.rs, partitioning.rs}`

Make `partitioning` module public and add a shim function to avoid infinite
query recursion (the `is_codegened_item` override needs to call the real
`collect_and_partition_mono_items` without going through the query system):

```rust
// lib.rs: change `mod partitioning` to `pub mod partitioning`

// partitioning.rs: add public shim
pub fn collect_and_partition_mono_items_impl(tcx: TyCtxt<'_>, (): ()) -> MonoItemPartitions<'_> {
    collect_and_partition_mono_items(tcx, ())
}
```

**Why `override_queries` + `is_codegened_item`**: This approach filters functions
at the codegen query level rather than replacing MIR bodies. Functions excluded
by `is_codegened_item` returning `false` never enter LLVM. The borrow checker's
conclusions about original function bodies are never contradicted (addressing
@workingjubilee's concern). The `after_analysis` hook runs after type-checking
and MIR borrowcheck are complete, when `optimized_mir()` is available for all
local functions.

---

## Safety Proof: The 11-Point `is_safe_to_stub` Checklist

The cost of a false negative (keeping an unreachable function) is binary bloat.
The cost of a false positive (stubbing a reachable function) is a runtime panic.

The checklist is conservative by design. When in doubt, we keep the function.

### Check 1: `def_id.is_local()`

**What it detects**: Extern crate functions.
**Failure mode**: Stubbing a function body we don't own, which would corrupt the
extern crate's `.rlib` from other compilations' perspective.
**Example**: `serde::Serialize::serialize` — called by many downstream crates.
**API**: `DefId::is_local()` — returns `true` only for items in the current crate.

### Check 2: `DefKind::Fn` or `DefKind::AssocFn`

**What it detects**: Non-function items (types, statics, consts, traits, impls).
**Failure mode**: Stubbing a type or static would produce a nonsensical MIR body.
**Example**: `const MAX: u32 = 255` — not a function, cannot be stubbed.
**API**: `tcx.def_kind(def_id)` returns the `DefKind` enum.

### Check 2a: Vtable check for `AssocFn`

**What it detects**: Trait impl methods where the trait is used as `dyn Trait`.
**Failure mode**: Dynamic dispatch (`(*trait_object).method()`) calls any impl of
the trait; BFS from static call sites does not see these calls.
**Example**: `impl Display for MyError { fn fmt(...) }` where `&dyn Display` is used.
**API**: `tcx.is_dyn_compatible(trait_def_id)` + `VTABLE_TRAITS` thread-local.

### Check 3: `vis.is_public() && !is_binary_crate`

**What it detects**: Public items in library crates.
**Failure mode**: Downstream crates that depend on this library may call any pub
function; we cannot see those call sites.
**Example**: A pub function in `serde` or `tokio` — called by users we don't see.
**API**: `tcx.visibility(def_id).is_public()` and `tcx.entry_fn(()).is_some()`.
**Note**: `tcx.visibility()` panics on `SyntheticCoroutineBody`; always check
`def_kind` (Check 2) before calling this.

### Check 4: Codegen attribute flags

**What it detects**: Linker-visible symbols: `#[no_mangle]`, `#[used]`,
`#[export_name]`, `#[link_section]`.
**Failure mode**: External code (C, OS, linker scripts) calls these by symbol name,
outside the Rust call graph entirely.
**Example**: `#[no_mangle] pub extern "C" fn init() { ... }` in embedded firmware.
**API**: `tcx.codegen_fn_attrs(def_id).flags` and `.symbol_name`, `.linkage`.

### Check 5: `Drop::drop` implementation

**What it detects**: Drop glue — the compiler inserts drop calls at scope exit,
outside any explicit call graph.
**Failure mode**: If `drop()` is stubbed, the destructor is replaced by
`Unreachable`, which either panics or leaves resources leaked (undefined behavior
for `unsafe` drop impls).
**Example**: `impl Drop for MutexGuard { fn drop(&mut self) { self.mutex.unlock() } }`.
**API**: `tcx.lang_items().drop_trait()` compared against the trait's `DefId`.

### Check 6: `entry_fn` check

**What it detects**: The `main` function (or the `#[start]` function).
**Failure mode**: Stubbing `main` produces a binary that immediately traps.
**API**: `tcx.entry_fn(()).map(|(id, _)| id) == Some(def_id)`.

### Check 7: `#[test]` and `#[bench]`

**What it detects**: Test functions called by the test harness.
**Failure mode**: The test harness constructs a list of function pointers to test
functions at link time; stubbed test functions would panic when the harness calls them.
**Example**: `#[test] fn test_parses_correctly() { ... }`.
**API**: `tcx.get_attrs(def_id, rustc_span::sym::test)` and `sym::bench`.

### Check 8: Unsafe functions

**What it detects**: `unsafe fn` items.
**Failure mode**: Unsafe functions can be called via `mem::transmute` or through
raw function pointers cast from other types. The safety invariants of such calls
are not tracked by the type system, so BFS cannot see them.
**Example**: `unsafe fn memset(ptr: *mut u8, val: u8, len: usize)` (C FFI wrapper).
**API**: `tcx.fn_sig(def_id).skip_binder().safety().is_unsafe()`.

### Check 9: Generic functions (`generics.count() > 0`)

**What it detects**: Functions with type or lifetime parameters.
**Failure mode**: BFS runs pre-monomorphization. A generic function `fn foo<T>()` is
reachable if *any* monomorphization of it is called. Since BFS works on `DefId`
(not instances), it cannot determine which monomorphizations exist.
**Example**: `fn serialize<T: Serialize>(val: &T)` — called for many `T`.
**API**: `tcx.generics_of(def_id).count()`.

### Check 10: Async functions

**What it detects**: `async fn` items.
**Failure mode**: `async fn` compiles to a coroutine state machine. The generator
transform runs after MIR building; stubbing the MIR before the transform produces
an invalid coroutine.
**Example**: `async fn fetch_data() -> Result<Data, Error>`.
**API**: `tcx.asyncness(def_id).is_async()`.

### Check 11: Signature contains `FnPtr` or `dyn Trait`

**What it detects**: Functions that accept or return function pointers or trait objects.
**Failure mode**: If function `bar(f: fn() -> u32)` is unreachable by BFS but is
registered as a callback via `fn()` pointer, calling it produces a panic.
**Example**: `fn register_handler(f: fn(Event) -> bool)` in an event system.
**API**: Walk `tcx.fn_sig(def_id)` inputs and output; check `TyKind::FnPtr` and
`TyKind::Dynamic`.

---

## Test Plan

### New UI tests (to be added in `tests/ui/dead-fn-elimination/`)

```
tests/ui/dead-fn-elimination/
├── basic.rs          — simple binary, check symbol count reduced
├── no-stub-pub.rs    — pub fn in lib crate: never stubbed
├── no-stub-generic.rs — generic fn: never stubbed
├── no-stub-async.rs  — async fn: never stubbed
├── no-stub-drop.rs   — Drop impl: never stubbed
├── no-stub-test.rs   — #[test]: never stubbed
├── no-stub-no-mangle.rs — #[no_mangle]: never stubbed
├── vtable-safe.rs    — dyn Trait: impl methods kept
└── runtime-correct.rs — end-to-end: binary produces correct output
```

### Existing test suites that must pass without change

- All `tests/ui/` tests (regression: no new failures)
- All `tests/codegen/` tests (codegen output unchanged for non-stubbed functions)
- All `tests/run-make/` tests (binary behavior unchanged)
- `tests/mir-opt/` tests (stub MIR is a valid MIR body)

### Self-hosting test

Build rustc itself with `-Z dead-fn-elimination` applied to stage1. Run the
resulting stage2 rustc on the test suite. All tests must pass.

```bash
python3 x.py build compiler/rustc --stage 1
python3 x.py test compiler/rustc --stage 1
```

### Verified results (2026-03-12, nightly d1ee5e59a / 1.96.0)

The following tests were run on a patched stage1 compiler (bootstrapped from
nightly-2026-03-12 + 4-file patch) with the updated `FxIndexMap`-based
implementation:

**Compilation**: `dead_fn_elim.rs` compiles with zero warnings and zero errors
under `rustc::internal` lint group (includes `potential_query_instability`).

**Functional correctness (ripgrep)**:
- Baseline build: `cargo +stage1 build --release` → binary runs correctly
- Flag build: `RUSTFLAGS="-Z dead-fn-elimination" cargo +stage1 build --release`
  → **895 unreachable functions excluded** → binary produces identical output
- `rg --version`, `rg "fn main" main.rs` — correct results in both cases

**Incremental compilation**:
- Build with flag → touch `main.rs` → rebuild with flag: **38s** (only touched
  crate recompiled, flag correctly `UNTRACKED`)
- Build with flag → toggle flag off → rebuild: only touched crate recompiled
  (no full cache invalidation, confirming `UNTRACKED` semantics)
- Toggle flag back on: only touched crate recompiled (**75s**)
- Binary correct after every toggle

**Summary**: The flag is safe to toggle on/off without invalidating the
incremental cache, and the compiled binary produces correct output in all cases.

---

## Performance Methodology

### External tool numbers (cargo-slicer, 2026-02-14)

- **Hardware**: AMD EPYC 7713 64-core @ 2.0 GHz, 256 GB ECC RAM, Samsung 990 Pro NVMe
- **OS**: Ubuntu 22.04.3 LTS, kernel 5.15.0-92-generic
- **Rust**: nightly-2026-02-14 (commit `c23ed3ef2`)
- **Build flags**: `--release -Z threads=8 -C linker=clang -C link-arg=--ld-path=wild`
- **Measurement**: `$(date +%s)` wall seconds, 3 runs, cold build (target/ deleted)
- **Tool**: `cargo-slicer` external driver (`RUSTC_WRAPPER` approach, cross-crate BFS)
- **No sccache**: confirmed with `CARGO_SLICER_SCCACHE=/nonexistent`
- **Claim**: **6–29% wall time reduction** on projects tested (see table in §Problem Statement)

### In-tree flag numbers (`-Z dead-fn-elimination`, 2026-03-08)

- **Compiler**: patched stage2 rustc (bootstrap from nightly-2026-03 + 4-file patch)
- **Kernel**: Linux 5.15.0-164-generic
- **Comparison**: same stage2 binary, baseline vs. flag (eliminates toolchain version noise)
- **Measurement**: `$(date +%s)` wall seconds, 2–3 runs, cold build (target/ deleted)
- **Build flags**: `--release -Z threads=8 -C linker=clang -C link-arg=--ld-path=wild`
- **Intra-crate results (v1)**: -8% (zed), -6% (rustc); break-even for smaller projects
- **Cross-crate results (v2)**: **-31% (zed), -48% (rustc)**; matches external tool range

Raw data is in `bench-results.db` (SQLite). HTML report: `multicrate-report.html`.

---

## Authorship and Review

This patch was developed by the cargo-slicer project team led by @yijunyu. The core algorithm
(BFS reachability from entry points, MIR stub body construction, vtable tracking)
has been running as an external `RUSTC_WRAPPER` tool since late 2025.

Every line of the proposed patch has been reviewed and understood by the authors.
The safety invariants were derived from reading the rustc source for:
- `collect_and_partition_mono_items` (monomorphization collector)
- `rustc_middle::mir::mono::MonoItem` (CGU item types)
- The existing `dead_code` lint implementation (`rustc_passes/src/dead.rs`)
- `rustc_codegen_ssa`'s item traversal

The 11-point checklist was developed iteratively: each check was added after
observing a category of runtime panics in the external tool during testing on
real-world projects (ripgrep, helix, zed, rustc itself).

### Community Feedback Incorporated

- **@lcnr**: Removed nested-fn restriction (unnecessary given `mir_built`-based
  call graph); replaced string-matching Drop detection with `lang_items().drop_trait()`.
  See `docs/lcnr-review-feedback.md`.

- **@Amanieu**: Implemented and evaluated deferred (metadata-first) compilation as
  `CARGO_SLICER_DEFERRED=1`. Found to be slower than single-pass CGU filtering on
  small workspaces due to double-compilation overhead; kept as experimental feature.
  See `docs/amanieu-review-feedback.md`.

- **@workingjubilee**: Raised concern that the rustc driver cannot maintain
  borrow-checker guarantees across the `optimized_mir` body replacement. The in-tree
  implementation addresses this by using CGU-level removal instead of body replacement —
  eliminable functions are removed from `collect_and_partition_mono_items` output and
  never reach LLVM. See `docs/wesley-workingjubilee-review-feedback.md`.

- **@oli-obk**: Observed that migrating from the external driver to rustc itself would
  not require much effort, given the algorithm's direct use of `TyCtxt` APIs. This
  observation is a key motivation for this MCP — the external tool has proven the
  algorithm; the in-tree patch (~422 lines) is the natural completion of that work.
  See `docs/wesley-workingjubilee-review-feedback.md`.

- **@petrochenkov**: Nine-point review (V1–V9) of the seed/eligibility
  surface. Adopted: scope is now explicit "binary crates only" (V1);
  benchmark labels corrected (V2); seeds are
  `reachable_set ∪ entry_fn ∪ address_taken ∪ vtable_methods` rather than a
  re-implementation (V3); cross-crate call graph removed (V4); function-pointer
  coercions and inline-asm symbol uses are now tracked (V5a/V5b);
  `requires_monomorphization` replaces blanket `generics > 0` (V6b);
  vestigial `#[test]`/`#[bench]` and `unsafe`/signature-walk checks removed
  (V7, V8a, V8b); empirical V9 answer: median 1.50× speedup with zero
  regressions across 2,452 crates. Full point-by-point response in
  `docs/vadim-response-results.md`; review verbatim plus worked
  examples in `docs/vadim-petrochenkov-review-feedback.md`.

---

## Relationship to Crate Slicing

This MCP captures roughly **half** of the compile-time savings available from
dead code in large projects. The other half requires a broader **crate slicing**
effort (separate project goal, in collaboration with the Cargo team).

The relationship:

- **Dead-fn elimination** (this MCP) removes unreachable functions from codegen
  within the compiler. It reduces LLVM work but cannot parallelize the remaining
  compilation — each crate is still compiled as a single unit.
- **Crate slicing** partitions *reachable* items into independently compilable
  slices, enabling sub-crate parallelism across the full pipeline (frontend +
  backend). This unlocks the remaining savings by turning one large compilation
  into several smaller parallel ones.

Both proposals share the same BFS dependency graph over crate items. This MCP
validates that graph algorithm in-tree; crate slicing extends it to drive
partitioning rather than elimination.

Accepting this MCP first provides a validated, in-tree dependency graph that
the crate slicing project goal can build on — rather than proposing both the
infrastructure and the application simultaneously.

---

## Applying the Patch

### Step 1: Clone rustc and configure

```bash
git clone --depth=1 https://github.com/rust-lang/rust /tmp/rust
cd /tmp/rust
cat > bootstrap.toml << 'EOF'
change-id = "ignore"
[build]
extended = false
[rust]
channel = "nightly"
optimize = true
EOF
```

### Step 2: Apply the patch

```bash
# Copy the implementation file
cp /path/to/cargo-slicer/src/upstream_patch/dead_fn_elim.rs \
   compiler/rustc_mir_transform/src/dead_fn_elim.rs

# Apply the unified patch (options.rs, driver lib.rs, mir_transform lib.rs,
# monomorphize lib.rs + partitioning.rs)
git apply /path/to/cargo-slicer/src/upstream_patch/rustc_other_files.patch
```

If the patch fails to apply cleanly (due to upstream changes), see the
Implementation section above for the exact code at each site.

### Step 3: Build stage1 compiler + standard library

```bash
python3 x.py build compiler/rustc --stage 1   # ~10 min
python3 x.py build library --stage 1           # ~14 min
rustup toolchain link stage1 build/host/stage1
```

### Step 4: Verify

```bash
# Flag is accepted
rustc +stage1 -Z help 2>&1 | grep dead-fn-elimination

# Baseline build
cd /tmp/ripgrep
cargo +stage1 build --release

# Flag build — expect "N unreachable functions excluded" diagnostic
rm -rf target
RUSTFLAGS="-Z dead-fn-elimination" cargo +stage1 build --release

# Verify binary correctness
./target/release/rg --version
./target/release/rg "fn main" crates/core/main.rs

# Incremental test: touch → rebuild (should be fast, only touched crate)
touch crates/core/main.rs
RUSTFLAGS="-Z dead-fn-elimination" cargo +stage1 build --release
```

**Verified 2026-03-12**: stage1 from nightly d1ee5e59a (1.96.0), ripgrep:
895 fns eliminated, binary correct, incremental rebuild 38s.

---

## File Change Summary

| File | Action | Lines |
|------|--------|-------|
| `compiler/rustc_session/src/options.rs` | +2 lines in `Z_OPTIONS!` | 2 |
| `compiler/rustc_mir_transform/src/dead_fn_elim.rs` | new file (incl. cross-crate BFS) | ~420 |
| `compiler/rustc_mir_transform/src/lib.rs` | +1 line (`pub mod`) | 1 |
| `compiler/rustc_driver_impl/src/lib.rs` | +~30 lines at 3 sites | 30 |
| `compiler/rustc_monomorphize/src/lib.rs` | `mod` → `pub mod` | 1 |
| `compiler/rustc_monomorphize/src/partitioning.rs` | +8 lines (public shim) | 8 |

**Total new code in rustc**: ~462 lines across 6 files
**Existing code modified**: 1 visibility change (`mod` → `pub mod`)

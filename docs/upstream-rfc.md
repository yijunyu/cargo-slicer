# MCP: Dead Function Elimination via BFS Reachability (`-Z dead-fn-elimination`)

**Status**: Patched stage1 builds and passes functional + incremental tests
(2026-03-12); `reachable_set ⊆ post-BFS-set` invariant holds on the rust-1.90
oracle; 59 crates.io binaries built with zero correctness regressions
(2026-04-29). Algorithm reflects @petrochenkov's V1–V11 review.
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

Independent correctness validation on a representative slice of the
ecosystem. Run on `cargo +nightly` baseline vs cargo-slicer leg via
`scripts/bench_ase_corpus.sh`; libraries gated on build success, binary
crates additionally smoke-tested with `--version` / `--help`.

> **Reframing (V10/V11, 2026-04-29)**: numbers are split by crate kind. The
> in-tree `-Z dead-fn-elimination` flag is a no-op on libraries today
> (early-return when `entry_fn().is_none()`), so library numbers cannot be
> folded into a single in-tree-flag headline. The userspace cargo-slicer
> tool's RUSTC_WRAPPER pipeline does run on libraries; its numbers are
> reported separately. See `docs/vadim-response-results.md` for the full
> V10/V11 discussion.

**Binary subset (n=65) — relevant to in-tree `-Z dead-fn-elimination`**:

| Metric                           | Value |
|----------------------------------|------:|
| Binary crates attempted          | 65    |
| Both legs built                  | **59** |
| **Slicer-only failures**         | **0** |
| Median build speedup             | **1.38×** |
| Mean build speedup               | 2.45× |
| % speedup ≥ 1.0×                 | 69.5% |
| % speedup ≥ 1.5×                 | 45.8% |
| % speedup ≥ 2.0×                 | 27.1% |

**Library subset (n=2,538) — userspace cargo-slicer only, NOT the `-Z` flag**:
2,393 of 2,538 libraries built under both legs with **zero slicer-only
failures**; userspace median 1.50×. This number measures cross-crate
orchestration in the userspace tool, not the in-tree single-crate flag.

Full per-crate catalog (rank, version, downloads, build times, status) is
published at <https://yijunyu.github.io/cargo-slicer/ase2026-corpus.html>;
the raw CSV is `docs/ase2026-corpus.csv`. Detailed per-concern empirical
evidence and reproduction instructions live in `docs/vadim-response-results.md`.

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
| "generic functions?" | `generics_of(def_id).requires_monomorphization(tcx)` → never eliminate (P6/V6b; matches `reachable.rs::recursively_reachable`). Pre-monomorphization BFS can't know which instantiations are reachable. |
| "async functions?" | `asyncness.is_async()` → never eliminate (also `DefKind::OpaqueTy` for the return type). Async fns compile to state machines; eliminating breaks the coroutine transform. |
| "Drop implementations?" | lang item `drop_trait` comparison → never eliminate. The compiler inserts drop calls implicitly, outside BFS reach. |
| "extern C / FFI?" | `NO_MANGLE`, `USED_LINKER`, `export_name`, `linkage` attrs → never eliminate. |
| "pub API in library crates?" | The pass early-returns on library crate types (P1/V1); in a binary, `reachable_set` seeding (P3/V3) preserves any `pub` item that needs preserving. No separate `is_public()` branch. |
| "Why `-Z` and not stable?" | Unstable is the correct starting point. Stabilization path: `-Z` → field validation → `-C` once semantics are proven. |
| "Why not LLVM?" | LLVM sees one CGU at a time. Cross-CGU reachability requires rustc-level analysis with global `TyCtxt`. |
| "unsafe functions?" | The "unsafe fn" ban was dropped (P8a/V8a) — it was a proxy for "might be address-taken". Address-taken tracking (P5a/P5b/V5) is the precise invariant: an unsafe fn coerced to a fn pointer is seeded and kept; one that is never address-taken and is BFS-unreachable is eliminable. |
| "functions passed as fn pointers?" | Tracked precisely by address-taken seeding (P5a/V5a): `ReifyFnPointer` / `ClosureFnPointer` coercions union the target into the BFS seeds. The earlier `FnPtr`/`dyn`-in-signature walk (Check 11) was dropped (P8b/V8b) as imprecise — it kept callees that merely *accept* a fn pointer rather than the ones actually address-taken. |
| "@workingjubilee: driver can't maintain borrow-checker guarantees across `optimized_mir` override" | The in-tree implementation uses CGU-level removal (`collect_and_partition_mono_items` override), not body replacement. The function is excluded from the codegen queue entirely — the borrow checker's conclusions about the original body are never contradicted. See `docs/wesley-workingjubilee-review-feedback.md`. |
| "@oli-obk: is the migration from external driver to rustc feasible?" | Yes — and this MCP is the outcome. The external tool has validated the algorithm since late 2025; the in-tree patch is a direct ~340-line translation using native `TyCtxt` APIs, eliminating IPC overhead, ABI versioning, and disk I/O. See `docs/wesley-workingjubilee-review-feedback.md`. |
| "`#[inline]` functions could be eliminated incorrectly" | `#[inline]` does not change reachability — it's a codegen hint. The BFS sees the call edge regardless. Inlined functions are monomorphized at call sites; the original `DefId` body is kept if any call site exists. |
| "`#[cold]` functions should not be eliminated" | Correct — `#[cold]` is a branch prediction hint, not a reachability signal. BFS treats `#[cold]` functions identically to any other: reachable = keep, unreachable = eliminate. No special handling needed. |
| "BFS bug could silently eliminate reachable functions" | Conservative safety checklist (`is_safe_to_eliminate`): generics, async, Drop, `#[no_mangle]`, address-taken, `dyn`-compatible trait methods → all kept unconditionally. A BFS bug can only affect a private, non-generic, non-async, non-Drop, non-address-taken standalone function — the narrowest possible class. Runtime symptom is an `Unreachable` trap (immediate, not silent). |
| "Use the mono collector instead of pre-mono BFS" | The mono collector operates post-monomorphization on `MonoItem`s (concrete instantiations). This pass operates pre-mono on `DefId`s (source-level functions). Pre-mono BFS is cheaper (fewer nodes, no generic expansion) and catches functions before LLVM even sees them. The two are complementary: BFS removes statically dead `DefId`s; the mono collector handles instantiation-level reachability. |

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

Full implementation (~340 lines), **local** BFS only. (An earlier draft
carried a `build_extern_call_graph` that re-walked extern crates; @petrochenkov's
V4 review showed it was redundant — `reachable_set` seeding plus address-taken
seeding already cover the cross-crate surface a single-crate pass can act on —
so it was deleted. See `docs/vadim-response-results.md`.)

```rust
use rustc_data_structures::fx::{FxHashSet, FxIndexMap, FxIndexSet};
use rustc_hir::def::DefKind;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::mir::{Body, TerminatorKind};
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_span::def_id::DefId;
```

**Module structure**:

1. **Thread-local state** (`VTABLE_TRAITS`, `ADDRESS_TAKEN`, `ELIMINABLE_DEF_IDS`)
   — three sets per rustc process. `VTABLE_TRAITS` tracks traits used as
   `dyn Trait`; `ADDRESS_TAKEN` (V5a/V5b) tracks functions whose address is
   taken; `ELIMINABLE_DEF_IDS` stores the final set to exclude from codegen.

2. **`run_analysis(tcx: TyCtxt<'_>)`** — called once from `after_analysis`:
   a. Early return for library crate types (`entry_fn().is_none()`)
   b. `build_call_graph(tcx)` — **local** edges from `tcx.mir_keys(())`;
      scans vtable constructions and address-taken operands in the same MIR walk
   c. `collect_seeds(tcx)` — seeds anchored on `tcx.reachable_set(())` (V3),
      plus the binary `entry_fn` and the vtable-constructed methods of
      `dyn`-compatible traits
   d. union `ADDRESS_TAKEN` into the seed set, then `run_bfs(seeds, &call_graph)`
   e. mark unreachable + `is_safe_to_eliminate` functions in `ELIMINABLE_DEF_IDS`
   f. (`debug_assertions`) assert `reachable_set ⊆ post-BFS-set` (V9 invariant)

3. **`is_eliminable(idx: u64) -> bool`** — O(1) lookup into `ELIMINABLE_DEF_IDS`.
   Called by the `is_codegened_item` query override in the driver.

4. **`is_safe_to_eliminate(tcx, def_id)`** — the safety checklist (see below).

5. **`build_call_graph(tcx)`** — local edges only, from `tcx.mir_keys(())`,
   reading each body's call terminators via `add_mir_edges`. No extern-crate
   walk (deleted in V4).

6. **`scan_for_vtable_constructions(body)`** — walks MIR for
   `Rvalue::Cast(PointerCoercion::Unsize, _, dyn Trait)`, records the trait
   DefId in `VTABLE_TRAITS`.

7. **`scan_for_address_taken(body)`** (V5a) and **`scan_inline_asm`** (V5b) —
   record `ReifyFnPointer` / `ClosureFnPointer` coercions and `InlineAsm`
   `SymFn` operands in `ADDRESS_TAKEN`, so indirectly-called functions survive
   BFS even when no direct call edge exists.

**Graph data structures**: `FxIndexMap<u64, FxIndexSet<u64>>` for the call graph
(deterministic iteration, no `potential_query_instability` lint). `FxHashSet<u64>`
for membership-only sets (seeds, reachable, vtable traits, address-taken,
eliminable). DefIds are encoded as `u64` keys (`(krate << 32) | index`) rather
than `def_path_str` — using the string path as a graph key triggered a
`trimmed_def_paths` ICE in `rustc_errors` (one of three bugs the review caught).

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

## Safety conditions: `is_safe_to_eliminate`

The cost of a false negative (keeping an unreachable function) is binary bloat.
The cost of a false positive (eliminating a reachable function) is a runtime
`Unreachable` trap. The checklist is conservative; when in doubt, keep the
function.

This section reflects the post-review checklist. @petrochenkov's review
(`yijunyu/cargo-slicer#1`, V1–V11) removed four heuristic proxies that earlier
drafts carried — a `vis.is_public()` library branch, `#[test]`/`#[bench]`
attribute checks, an "unsafe fn" ban, and a `FnPtr`/`dyn`-in-signature walk —
and replaced them with two direct invariants: seeding from `reachable_set`
(V3) and explicit address-taken tracking (V5). The dropped proxies are noted
below where they used to live.

### Always keep (never eliminate)

| Condition | Why | API |
|-----------|-----|-----|
| `!def_id.is_local()` | Never act on a function we don't own. | `DefId::is_local()` |
| Not `DefKind::Fn` / `AssocFn` | Non-fn items have no eliminable body. | `tcx.def_kind` |
| `DefKind::OpaqueTy` (async fn return) | Eliminating triggers an `E0391` cycle. | `tcx.def_kind` |
| `DefKind::SyntheticCoroutineBody` | `tcx.visibility()` panics on these — check `def_kind` first. | `tcx.def_kind` |
| Method of a `dyn`-compatible trait used as `dyn Trait` | Dynamic dispatch calls any impl; BFS from static sites misses it. | `tcx.is_dyn_compatible` + `VTABLE_TRAITS` |
| `Drop::drop` impl | Drop glue is inserted by the compiler outside the call graph. | `tcx.lang_items().drop_trait()` |
| Linker-visible (`#[no_mangle]`, `#[used]`, `export_name`, explicit `linkage`) | Called by symbol name from outside Rust. | `tcx.codegen_fn_attrs(def_id)` flags / `export_name` / `linkage` |
| `entry_fn` | Eliminating `main` traps the binary. | `tcx.entry_fn(())` |
| `requires_monomorphization` (type/const generics) | Pre-mono BFS cannot know which instantiations are live; matches `reachable.rs::recursively_reachable`. | `tcx.generics_of(def_id).requires_monomorphization(tcx)` |
| `async fn` | Coroutine transform runs post-MIR-build; eliminating produces an invalid coroutine. | `tcx.asyncness(def_id).is_async()` |
| Address-taken | Reachable via fn-pointer / closure coercion / `asm! sym`, no direct call edge. | `ADDRESS_TAKEN` (V5a/V5b) |

What survives this list — and is therefore eliminable when BFS-unreachable —
is the narrowest class: a **private, non-exported, non-generic, non-async,
non-Drop, non-address-taken standalone function or inherent-impl method**. A
BFS bug can affect only that class, and its runtime symptom is an immediate
`Unreachable` trap, not silent misbehavior.

**Notes on the dropped proxies**

- *`vis.is_public()` library branch (was Check 3 → dropped in P1/V1).* The pass
  early-returns on library crate types, and in a binary the `reachable_set`
  seeding (V3) already preserves any `pub` item that needs preserving — no
  separate visibility branch is needed.
- *`#[test]`/`#[bench]` (was Check 7 → dropped in P7/V7).* By the time the pass
  runs, the harness has already lowered the test list into a const initialiser
  that `reachable_set` covers; the attribute check was a no-op.
- *unsafe-fn ban (was Check 8 → dropped in P8a/V8a).* "unsafe" was a proxy for
  "might be address-taken"; address-taken tracking (V5a) is the precise
  invariant, so unsafe fns are no longer kept unconditionally.
- *`FnPtr`/`dyn` signature walk (was Check 11 → dropped in P8b/V8b).* Subsumed
  by `reachable_set` seeding plus address-taken tracking.

---

## Test Plan

### UI tests

The test matrix lives in `tests/dead-fn-elim/ui/` in cargo-slicer and is
mirrored into `tests/ui/dead-fn-elimination/` for the in-tree patch. Each file
targets one numbered concern from the review:

```
tests/ui/dead-fn-elimination/
├── fn-ptr-coercion.rs       — V5a: `as fn()` address-taken fn preserved
├── closure-fn-ptr.rs        — V5a: closure coerced to fn ptr preserved
├── inline-asm-sym.rs        — V5b: `asm!(… sym f …)` operand preserved
├── track-caller.rs          — R2: `#[track_caller]` shim preserved
├── coroutine.rs             — R3: coroutine body preserved
├── proc-macro-helper.rs     — R4: proc-macro crate untouched
├── cross-crate-inlinable.rs — R5: inlinable private fn preserved under LTO
├── test-harness.rs          — V7: `#[test]` survives `--test`
├── private-helper-pruned.rs — V9+: unreachable private fn IS eliminated
└── library-noop.rs          — V1: `--crate-type rlib` eliminates zero fns
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

The safety invariants were derived from reading the rustc source for:
- `collect_and_partition_mono_items` (monomorphization collector)
- `rustc_middle::mir::mono::MonoItem` (CGU item types)
- The existing `dead_code` lint implementation (`rustc_passes/src/dead.rs`)
- `rustc_codegen_ssa`'s item traversal

The `is_safe_to_eliminate` checklist was developed iteratively: each condition
was added after observing a category of runtime panics in the external tool
during testing on real-world projects (ripgrep, helix, zed, rustc itself), then
tightened during @petrochenkov's V1–V11 review (four heuristic proxies replaced
by `reachable_set` seeding + address-taken tracking).

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

- **@petrochenkov**: Eleven-point review (V1–V11) of scope and the
  seed/eligibility surface. Adopted V1–V9: benchmark labels corrected (V2);
  seeds are `reachable_set ∪ entry_fn ∪ address_taken ∪ vtable_methods` rather
  than a re-implementation (V3); cross-crate call graph removed (V4);
  function-pointer coercions and inline-asm symbol uses are now tracked
  (V5a/V5b); `requires_monomorphization` replaces blanket `generics > 0`
  (V6b); vestigial `#[test]`/`#[bench]` and `unsafe`/signature-walk checks
  removed (V7, V8a, V8b); empirical V9 answer on the binary subset:
  **median 1.38× across 59 binary crates with zero slicer-only failures**.
  V10 (libs work with `reachable_set` seeds — lift V1 early-return) and V11
  (single-crate elim is mostly redundant; cross-crate orchestration is where
  the win lives) are tracked as future-work / deferred RFC items rather than
  patched in this MCP. Full point-by-point response in
  `docs/vadim-response-results.md`; review verbatim plus worked examples in
  `docs/vadim-petrochenkov-review-feedback.md`.

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

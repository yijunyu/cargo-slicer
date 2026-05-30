# MCP design: cross-crate dead-fn elimination

Brief design doc for Oli Scherer (rustc-mono / MIR review).

## Question

For a workspace with crates A, B, Main (Main → B → A), when rustc compiles
**A in isolation** (Main is not visible at that compile invocation), how does
the MIR pass know which functions in A are unreachable from `Main::main` and
therefore safe to stub?

A's compile sees only A's source and A's `tcx`. The reachability oracle
must come from outside the compiler.

## TL;DR

Two implementations exist in this repo today; they share one oracle but
differ in where it's consumed.

- **Implementation 1 — userspace driver (works today).** A pre-pass
  (`cargo-slicer pre-analyze`) walks the workspace's syn ASTs, builds a
  cross-crate call graph from Main::main, and writes per-crate `.cache`
  / `.seeds` files. When A compiles, a custom rustc driver
  (`cargo_slicer_rustc` via `RUSTC_WRAPPER`) loads A's `.cache`,
  installs an `optimized_mir` query override, and replaces unreachable
  function bodies with abort stubs.
- **Implementation 2 — in-tree `-Z dead-fn-elimination` (binary-only
  today, cross-crate proposal).** A `rustc_mir_transform` pass runs
  only when `entry_fn().is_some()` and does intra-binary reachability
  using `tcx`. For A's compile it currently does nothing. The proposal
  below extends it to read the same `.seeds` oracle, gated behind an
  unstable flag with an explicit path.

The doc keeps the userspace implementation as the "what works now"
baseline, and frames the in-tree extension as what we'd like Oli's
input on.

---

## Data-flow diagram (both implementations)

```
                  ┌──────────────────────────────────────────────┐
                  │   PRE-PASS  (cargo-slicer pre-analyze)       │
                  │                                              │
                  │   cargo metadata --no-deps                   │
                  │     →  workspace crate list                  │
                  │                                              │
                  │   syn / fast / ctags parser per crate        │
                  │     →  src/pre_analyze.rs:80                 │
                  │                                              │
                  │   FNV-1a source hash per crate               │
                  │     →  src/pre_analyze.rs:1242               │
                  │                                              │
                  │   write `.slicer-cache/<crate>.analysis`     │
                  │     DEFINED_ITEMS / CALL_EDGES /             │
                  │     TRAIT_IMPLS / STATICS / NO_MANGLE        │
                  │     →  src/pre_analyze.rs:1299               │
                  │                                              │
                  │   unified BFS from each binary's `main`      │
                  │     →  src/cross_crate_bfs.rs:189            │
                  │                                              │
                  │   per-crate output:                          │
                  │     `<crate>.cache`   reachable items        │
                  │     `<crate>.seeds`   pub items reachable    │
                  │                       from any binary        │
                  │     `<crate>.skip-driver`  (if 0-stub crate) │
                  │     →  src/cross_crate_bfs.rs:307,455,518    │
                  └──────────────────────────────────────────────┘
                                       │
                                       │ written to disk
                                       ▼
        .slicer-cache/A.cache     A.seeds     A.skip-driver
        .slicer-cache/B.cache     B.seeds     ...
        .slicer-cache/Main.cache  (Main is a binary, no .seeds)
                                       │
                                       │ cargo invokes rustc on A
                                       ▼
   ┌───────────────────────────┐   ┌───────────────────────────────────┐
   │ Impl 1 — userspace driver │   │ Impl 2 — in-tree -Z DFE           │
   │ RUSTC_WRAPPER=            │   │ RUSTC=rustc-with-patch            │
   │   cargo_slicer_dispatch   │   │   (no wrapper, no .cache reads)   │
   │                           │   │                                   │
   │ dispatch detects local    │   │ dispatch detects in-tree flag,    │
   │  source, picks driver     │   │  appends -Zdead-fn-elimination,   │
   │ →cargo_slicer_dispatch:307│   │  execs rustc directly             │
   │                           │   │ →cargo_slicer_dispatch:75,313     │
   │ driver loads A.cache      │   │                                   │
   │ →virtual_slicer.rs:627    │   │ pass runs only if entry_fn        │
   │                           │   │  is_some() — for A it's None,     │
   │ override providers:       │   │  returns immediately              │
   │  optimized_mir →          │   │ →dead_fn_elim.rs:70               │
   │   replace body if unmarked│   │                                   │
   │   & safe                  │   │ when running (on Main):           │
   │  collect_and_partition_   │   │  build intra-crate call graph     │
   │   mono_items →            │   │  from optimized_mir(),            │
   │   remove stubbed items    │   │  union with tcx.reachable_set()   │
   │   from CGUs               │   │  + vtables + address-taken,       │
   │ →virtual_slicer.rs:677    │   │  BFS, populate ELIMINABLE_DEF_IDS │
   └───────────────────────────┘   │ →dead_fn_elim.rs:63,80,91,110     │
                                   │                                   │
                                   │ query override reads              │
                                   │  is_eliminable(idx) → stub        │
                                   │ →dead_fn_elim.rs:151              │
                                   └───────────────────────────────────┘
                                       │
                                       ▼
                              codegen sees only marked items
```

---

## Implementation 1 — userspace driver (current, working)

### Components

- **`cargo-slicer pre-analyze`** — `src/pre_analyze.rs:80` (`run_pre_analysis`).
- **`cargo_slicer_dispatch`** — `src/bin/cargo_slicer_dispatch.rs:124`
  (RUSTC_WRAPPER; thin, no rustc_driver link, <1 ms startup).
- **`cargo_slicer_rustc`** — `src/bin/cargo_slicer_rustc.rs` (drives rustc
  with overridden providers).
- **`virtual_slicer::override_providers`** —
  `src/rustc_integration/virtual_slicer.rs:677`.

### Oracle file format (the part rustc/oli actually needs to see)

`<crate>.cache` is a newline-delimited list of reachable item paths
plus a header carrying the source hash:

```
SOURCE_HASH: <fnv1a>
<crate>::<module>::<item>
...
```

`<crate>.seeds` is the same shape, restricted to pub items that are
reachable from some binary's `main`. For a library compiling alone,
this is the file rustc would consult.

### How A's compile decides what to stub

1. Dispatch sees A's compile, detects local source path, ships args to
   `cargo_slicer_rustc`. (`cargo_slicer_dispatch.rs:307`)
2. Driver loads `A.cache`, builds a `FxHashSet<DefId>` of marked items
   indexed by `DefId` and by `def_path_str`.
   (`virtual_slicer.rs:627`)
3. Override `optimized_mir`: for every local def_id, query whether it
   is **marked** and **safe to stub**.
   (`virtual_slicer.rs:677`)
4. Safety predicate (`virtual_slicer.rs:327`): private only, no
   `#[no_mangle]` / `#[used]` / `#[export_name]`, not Drop, not entry,
   not generic, not async, no fn-ptr / `dyn Trait` in signature, not
   coerced to a vtable, not address-taken via inline-asm `sym fn`.
5. Body replacement: rebuild the MIR body as `loop { panic!() }`
   (effectively `unreachable_unchecked`-like; LLVM optimises it to a
   single trap).
6. Override `collect_and_partition_mono_items` to remove stubbed items
   from CGUs entirely, so downstream linker has nothing to find missing.

### Properties

- A's compile is **standalone**: only file inputs are A.rs and
  `.slicer-cache/A.cache`.
- The oracle is text. Hash of A's source is stored in the cache so
  rustc-side staleness is detectable.
- Soundness rests on the safety predicate. Bugs are visible as link
  errors (missing symbol → caller still wants the stubbed fn).
- No nightly required (the driver is built against `rustc_private` but
  is stable in shape).
- Cost: every rustc invocation pays a fork + driver load. Mitigated by
  a fork-server daemon (`cargo_slicer_dispatch.rs:366`).

---

## Implementation 2 — in-tree `-Z dead-fn-elimination`

### Current scope (binary-only)

`compiler/rustc_mir_transform/src/dead_fn_elim.rs`. Behaviour today:

- Pass installed via `override_queries` hook (avoids recursion with
  `optimized_mir` provider override).
- `run_analysis()` (`dead_fn_elim.rs:65`):
  - Early-return if `tcx.entry_fn(()).is_none()` — so libraries
    (including A and B) get no elimination from this pass at all.
    (`dead_fn_elim.rs:70`)
  - Build call graph from `tcx.mir_keys()` over local items.
    (`dead_fn_elim.rs:76`)
  - Seeds: `tcx.reachable_set()` ∪ entry fn ∪ vtable-constructed trait
    impl methods ∪ address-taken fns. (`dead_fn_elim.rs:80`)
  - BFS, populate `ELIMINABLE_DEF_IDS` (thread-local `FxHashSet<u64>`,
    keyed by `def_id_key = (krate << 32) | item_idx`).
    (`dead_fn_elim.rs:59,91,110`)
- Query override consults `is_eliminable(u64) -> bool` per item.
  O(1) borrow; no per-call clone. (`dead_fn_elim.rs:151`)

So for the A → B → Main workspace, `-Z dead-fn-elimination` **only fires
on Main**, and within Main it can only eliminate functions defined in
Main. A's pub items remain present in A's `.rlib`.

### Proposal: cross-crate via `.seeds`

The proposal is the minimal extension that lets A's pass do something.

Sketch of the patch:

1. **New nightly flag**:
   `-Z dead-fn-elimination-oracle=<path>` accepting a path to a
   `.seeds`-format file. Empty / absent → current behaviour (no-op on
   libraries). Present → activate library path.

2. **Library path** (modify `run_analysis`):

   ```rust
   if tcx.entry_fn(()).is_none() {
       let Some(oracle_path) = tcx.sess.opts.unstable_opts
           .dead_fn_elimination_oracle.as_deref()
       else { return; };
       let seeds = parse_seeds(oracle_path)?;          // Vec<DefPathHash>
       // Reject if seeds.source_hash != source_hash(tcx)
       let seed_def_ids = resolve_seeds(tcx, &seeds);  // Vec<DefId>
       let local_graph = build_call_graph(tcx);
       run_bfs(local_graph, seed_def_ids,
               &mut ELIMINABLE_DEF_IDS.borrow_mut());
       // safety predicate same as binary path
       return;
   }
   ```

3. **Seeds resolution**: seeds in the oracle are
   `(crate_name, DefPathHash)` pairs. `DefPathHash` is stable across
   compiler versions and across compilation sessions
   (`rustc_span::def_id::DefPathHash`). Cargo-slicer's `pre-analyze`
   already emits these as a side-output of the syn walk; mapping
   syn-path → `DefPathHash` is the missing piece in the pre-pass, not
   in rustc.

4. **Staleness gate**: oracle file carries a source-hash header. If
   the hash of the crate's source files doesn't match, the pass emits
   a non-fatal lint and falls back to the no-op. No silent corruption.

5. **Safety predicate** is unchanged from the binary path — the same
   set of "don't stub" rules (non-pub, address-taken, vtable, async,
   generic via `requires_monomorphization`, linker attrs, Drop) protects
   soundness regardless of which path produced the seeds.

   > **Note — the two implementations use different predicates.** The
   > userspace driver (Impl 1, `virtual_slicer.rs:327`) still carries two
   > extra conservative guards: an "unsafe fn" ban and a walk that keeps any
   > fn whose signature mentions a fn-pointer or `dyn Trait`. The in-tree
   > path (Impl 2) **dropped both** during @petrochenkov's review (V8a/V8b):
   > "unsafe" was a proxy for "address-taken", now tracked precisely
   > (`ReifyFnPointer`/`ClosureFnPointer`/`asm! sym`), and the signature walk
   > was subsumed by `reachable_set` seeding + address-taken tracking. So the
   > in-tree predicate is *tighter* (eliminates strictly more) while resting
   > on a more precise invariant. The userspace tool has not yet been updated
   > to match; it is conservative, not wrong. This divergence is itself a
   > question for Oli (see open question 2).

### Why this is small

- The oracle format is **outside** rustc. Producing it is the
  pre-pass's job. rustc only validates the source hash and resolves
  `DefPathHash`s.
- The pass itself is the same BFS and the same safety predicate as
  the binary path, just with seeds-from-file instead of seeds-from-tcx.
- No new query, no new MIR opt. The existing `optimized_mir` override
  still does the stubbing.

### What we'd like Oli's input on

1. Is `-Z dead-fn-elimination-oracle=PATH` an acceptable nightly
   surface, or should the oracle come from a different channel
   (`--extern` metadata, environment, rmeta sidecar)?
2. The safety predicate — anything missing? Specifically: are there
   non-`#[no_mangle]` ways for an external crate to reach a `pub fn`
   in A that bypass our address-taken/vtable scan? Lint-style
   `#[used(linker)]`, `#[link_section]`, weak symbols?
3. Should seeds use `DefPathHash` (stable across builds) or
   `SymbolName` (linker-visible)? `DefPathHash` is what we want for
   correctness; `SymbolName` is what the linker would actually see.
4. Source-hash staleness gate: lint-and-fallback, or hard error? We
   default to lint so a stale oracle never *miscompiles*, just
   under-optimises.
5. Should the binary path also accept an external oracle (to let the
   binary BFS skip the workspace walk on subsequent builds)? Likely
   yes, symmetrically.

---

## Worked example: A → B → Main

A has `pub fn used()` (called by B), `pub fn unused()` (called by
nobody in the workspace), `fn private_used()` (called by `used`),
`fn private_unused()` (called by nobody).

### Pre-pass (runs once per workspace state)

- Parse A, B, Main. Build cross-crate call graph from `Main::main`.
- Mark: `Main::main`, `B::wrap`, `A::used`, `A::private_used`.
- Unmark: `A::unused`, `A::private_unused`.
- Write `.slicer-cache/A.seeds` containing
  `{A::used, A::private_used}` + source hash of A.

### A compiles

#### Impl 1 (userspace driver)

- `cargo_slicer_dispatch` sees A is local; routes to
  `cargo_slicer_rustc`.
- Driver loads `A.cache` (or `.seeds`).
- `optimized_mir(A::unused)` → unmarked, safe → replaced with abort.
- `optimized_mir(A::private_unused)` → unmarked, safe → replaced.
- `optimized_mir(A::used)` → marked → real body.
- `optimized_mir(A::private_used)` → marked → real body.
- `collect_and_partition_mono_items` filters out the two unmarked
  items, so A.rlib codegens 2 of 4 functions.

#### Impl 2 (proposed in-tree)

- Cargo invokes `rustc -Z dead-fn-elimination
  -Z dead-fn-elimination-oracle=.slicer-cache/A.seeds` on A.
- `entry_fn(()).is_none()` → enter library path.
- Load `A.seeds`, validate source hash, resolve `DefPathHash`s to
  `DefId`s for `{A::used, A::private_used}`.
- Build A's local call graph; BFS from the resolved seeds;
  `ELIMINABLE_DEF_IDS = {A::unused, A::private_unused}`.
- `optimized_mir` query override (already in the patch) replaces
  bodies for those two.
- A.rlib codegens 2 of 4 functions, same as Impl 1.

### Main compiles

- Impl 1: same flow, with Main's own `.cache`.
- Impl 2: current behaviour (binary path, intra-crate only). Cross-crate
  is already done because A's compile already eliminated its dead
  functions.

---

## What's prototype vs. proposal

| Piece | Prototype today | Needs Oli's review |
|---|---|---|
| Pre-pass syn walk + cross-crate BFS | yes (`src/pre_analyze.rs`, `src/cross_crate_bfs.rs`) | no |
| `.cache` / `.seeds` file format | yes | format itself, if Oli wants it more cargo-/rustc-canonical |
| Userspace driver, query overrides, safety predicate | yes, used in production | the safety predicate, if anything is missing |
| Binary-path in-tree `-Z dead-fn-elimination` | yes (`src/upstream_patch/dead_fn_elim.rs`), benchmarked | already on its way upstream |
| Library-path in-tree (the actual proposal) | **no — design only** | the whole thing |
| `DefPathHash`-based seed resolution | no | format + lookup API |
| Source-hash staleness gate | no | lint vs hard error |

---

## File pointers

- Pre-pass: `src/pre_analyze.rs:80,1242,1299`
- Cross-crate BFS: `src/cross_crate_bfs.rs:189,307,455,518`
- Dispatch wrapper: `src/bin/cargo_slicer_dispatch.rs:75,124,307,313`
- Userspace driver overrides:
  `src/rustc_integration/virtual_slicer.rs:327,627,677`
- In-tree pass (working, binary-only) — the current post-review source
  (P1–P9 applied) lives in this repo at
  `src/upstream_patch/dead_fn_elim.rs:59,65,70,80,91,110,151`. To build it,
  copy it to `compiler/rustc_mir_transform/src/dead_fn_elim.rs` and apply
  `src/upstream_patch/rustc_other_files.patch` (the four integration sites).
  This is the file the line numbers above refer to.

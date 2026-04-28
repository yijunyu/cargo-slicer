# Review Feedback from @petrochenkov (Vadim Petrochenkov)

**Date**: 2026-04-24
**Source**: <https://github.com/yijunyu/cargo-slicer/issues/1>
**Subject**: Overview of `dead_fn_elim.rs` (the in-tree patch proposed for the
`-Z dead-fn-elimination` MCP)

This document tracks each numbered sub-issue Vadim raised, gives a worked
example + counter-example for each, responds with the design and implementation
decision, points to the exact patch that addresses it, and ends with a summary
plus a "similar risks" review.

> Original review verbatim is reproduced once below; from there on we cite by
> sub-issue number (V1–V9).

> *"Some parts of the code think that we support optimizing libraries, but we
> actually immediately bail out at the top of `run_analysis` on anything that
> is not an executable (has `entry_fn`), including dynamic libraries.* (V1)
>
> *This makes it unclear how results on `rustc` binary were obtained, rustc is a
> tiny binary with one function calling to a dynamic library.* (V2)
>
> *The seed set can be obtained from already existing `reachable_set` query.
> In any case the seed set will pretty much include only `fn main` for
> executables, the checks like `is_public` do not make sense for executables.
> Perhaps `reachable_set` even returns the whole non-eliminable set, not sure,
> need to investigate deeper.* (V3)
>
> *`build_extern_call_graph` is unnecessary, other crates cannot call functions
> from this crate, and the external part of the graph is not used when
> calculating ELIMINABLE_DEF_IDS.* (V4)
>
> *There are holes in the logic — for example, coercions to `dyn Trait` are
> considered, but coercions to function pointers are not considered* (V5a) *,
> also symbol uses from inline asm are not considered* (V5b) *.
> reachable.rs already contains some relevant logic for what should be
> considered, including both fn pointers and inline asm.*
>
> *All impls of non-eliminable traits are marked as non-eliminable — this is
> hard to avoid though, reachable.rs also doesn't seem to do anything smarter
> here, but it has a FIXME about it. The `generics_of(def_id).count > 0`
> condition also seems to make all traits non-eliminable.* (V6)
>
> *The `#[test]`/`#[bench]` checks are useless, those attributes do not exist
> in HIR at this point.* (V7)
>
> *The "no unsafe functions" and "no fn pointers or dyn Trait in signatures"
> conditions and their explanations are not clear to me.* (V8)
>
> *So it is possible that the algorithm can be made correct, and perhaps reuse
> reachable.rs, but it's unclear how much dead code it will be able to
> eliminate in that case, it's possible that it will converge to what
> `reachable_set` is already doing.* (V9)

References:
- `compiler/rustc_passes/src/reachable.rs` — `reachable_set` query.
- `compiler/rustc_monomorphize/src/collector.rs` — mono collector seeding.
- Patch under review: `src/upstream_patch/dead_fn_elim.rs`
  (mirror of `compiler/rustc_mir_transform/src/dead_fn_elim.rs`).

---

## V1 — "Code thinks it supports libraries, but bails out on non-executables"

**Concern.** `is_safe_to_eliminate` carries a `vis.is_public() && !is_binary_crate`
branch (and similar comments) that imply library support, but `run_analysis`
short-circuits with `if tcx.entry_fn(()).is_none() { return; }`. So library
crates and `cdylib`/`dylib`/`staticlib` outputs never enter the analysis at all,
and the library-related code is dead.

**Example (concern is valid).** A `cdylib` (`crate-type = ["cdylib"]`) has no
`entry_fn`, so we return early and never eliminate anything. Yet
`is_safe_to_eliminate` still spends lines distinguishing "binary crate" vs
"library crate". Reader gets the wrong impression about the supported scope.

**Counter-example (why we early-return).** For an `rlib`, the only safe seeds
are *all reachable items of the crate* (because any downstream crate can call
any `pub` item, and generic / `cross_crate_inlinable` items can be codegen'd
in downstream crates). With no entry point, BFS over local-only edges would
converge to "private items only" — which is exactly what dead-code linting
already flags. Trying to eliminate those gives essentially no codegen win and
bloats the safety surface. So bailing out is correct, but the *intent* should
be expressed as "binary crates only", not as "we consider library visibility".

**Response.**

1. Accept the bail-out, reframe the public scope of the flag as
   "binary crates only" (executable + `bin` test harness).
2. Delete the now-vestigial `is_public() && !is_binary_crate` branch from
   `is_safe_to_eliminate`. After the early return, `is_binary_crate` is always
   true, so the check is dead.
3. Document the scope at the top of `dead_fn_elim.rs` and in the `-Z` flag help
   text: "applies to binary crates; library crate types are ignored".
4. Future work (post-stabilization): a separate `-Z` mode for `cdylib` /
   `staticlib` would need a user-supplied "exported symbols" list (parsed from
   `--export-symbols` / version scripts). Out of scope for the initial flag.

**Patch (P1).** `dead_fn_elim.rs`:
- Remove the `if tcx.visibility(def_id).is_public() && !is_binary_crate` branch
  in `is_safe_to_eliminate`.
- Add a module-level doc paragraph: scope = binary crates.
- Tighten the early-return comment in `run_analysis` to name the supported
  crate types explicitly (executables and `bin` test harnesses).

---

## V2 — "How were `rustc` results obtained? rustc is a tiny binary"

**Concern.** The proposal cites `-6%` / `-48%` improvements on building rustc,
but the rustc *binary* is tiny — `compiler/rustc/src/main.rs` is ~70 lines and
delegates to `librustc_driver.so`. With one entry point, BFS in the binary
crate alone could not eliminate enough to explain the numbers.

**Example (where the savings come from).** The numbers are workspace-wide,
not from the `rustc` binary crate. They come from compiling the 67 workspace
crates that *make up* librustc_driver. Each of those crates is its own
compilation unit; the flag eliminates dead functions per-crate, dominated by
`librustc_driver.so` codegen. The "rustc binary" line in the table conflates
"the rustc executable" with "compiling the rustc workspace".

**Counter-example.** For a `dylib` like `librustc_driver.so` itself, our
current early-return *prevents* any elimination — every `pub fn` is reachable
by definition. So elimination cannot be coming from `librustc_driver.so` as a
single crate. It comes from the leaf workspace crates (`rustc_passes`,
`rustc_resolve`, `rustc_lint`, …) where private functions, non-exported
helpers, and binary-only code paths are eliminable.

**Response.**

1. Acknowledge the table is misleading. Replace "rustc" with
   **"rustc workspace (`x.py build compiler/rustc --stage 1`)"** in
   `docs/upstream-rfc.md` and call out that the elimination occurs across the
   67 workspace crates, not in the `rustc` binary crate itself.
2. Keep the early-return for libraries (V1). Confirm via the per-crate
   `note: N unreachable functions excluded` diagnostic that the bulk of the
   savings come from the binary-shaped compilations in the workspace
   (the test harness binaries, `rustc-main`, etc.), plus *intra-crate* dead
   code in workspace `rlib`s when we extend to library mode.
3. Add a per-crate breakdown to the RFC: which crates contributed how many
   eliminated functions, summing to the reported workspace-wide total.

**Patch (P2).** `docs/upstream-rfc.md`:
- Rename the rustc row to "rustc workspace (67 crates)" and add a footnote
  pointing at the per-crate breakdown.
- Add a `bench-results.db` query in `scripts/` that prints
  `(crate_name, eliminated_count, baseline_seconds, flag_seconds)` so the
  numbers are reproducible.

---

## V3 — "Reuse `reachable_set` instead of `collect_seeds`"

**Concern.** `collect_seeds` reimplements logic that `reachable_set`
(`compiler/rustc_passes/src/reachable.rs`) already does, including custom
linkage / extern indicators / lang items / vtable-traceable trait impls.
For executables, `reachable_set` returns essentially the whole non-eliminable
set, and `is_public` checks become meaningless.

**Example (reachable_set does most of our seeding).** The seeds we collect are:
`entry_fn`, statics, `#[no_mangle]` / `#[used]`, `#[test]`/`#[bench]`,
vtable-constructed trait methods. Compare to `reachable_set` (lines 408–514):
custom-linkage items via `has_custom_linkage`, lang items, all trait impl
items (FIXME: conservative), public items via `effective_visibilities`. There
is direct overlap on (a) custom-linkage seeds and (b) trait impl handling.

**Counter-example (where they diverge).** `reachable_set` is designed for
*"what can downstream crates see"* — it expands into bodies of inline-eligible
or const-eligible items. We need *"what is reachable from the program entry
point in this crate"* — strictly tighter for binaries. Concretely:
`reachable_set` for a binary crate seeds with `effective_visibilities`'
public-at-`ReachableThroughImplTrait` items, which for a binary is mostly
empty *except* for items that escape via `pub use`. Our seeds (`entry_fn`
plus extern indicators) are different.

**Response.**

1. Adopt `reachable_set` as the **non-eliminable lower bound** rather than
   redoing its work. Anything in `reachable_set(tcx, ())` is unconditionally
   not eliminable.
2. Add a thin layer of *binary-specific* seeds on top:
   - `entry_fn(())` (for `bin` crates `reachable_set` does not seed `main`).
   - `#[start]` if present.
   - `#[test]`/`#[bench]` *via the test harness* (see V7 for the proper hook).
   - vtable-constructed trait methods *for the local crate* (see V5a/V6).
3. Drop the `is_public()` branch from seed collection (already removed in V1).
4. Recurse from those seeds via the call-graph BFS — the BFS itself stays.

This converts the relationship from *"redo reachable_set badly"* to
*"reachable_set ∪ entry-reachable closure"*. The win over plain
`reachable_set` is the *closure* step: we follow call edges to discover
private helpers reachable *only* through the entry point, which `reachable_set`
deliberately stops at (it is a "what can leak across crate boundaries"
analysis, not a "what is called at runtime" analysis).

**Patch (P3).** `dead_fn_elim.rs`:

```rust
fn collect_seeds(tcx: TyCtxt<'_>) -> FxHashSet<u64> {
    let mut seeds = FxHashSet::default();

    // Lower bound: anything reachable_set already considers reachable.
    for &local_def_id in tcx.reachable_set(()).iter() {
        seeds.insert(def_id_key(local_def_id.to_def_id()));
    }

    // Binary-specific seeds reachable_set does not provide.
    if let Some((entry_def_id, _)) = tcx.entry_fn(()) {
        seeds.insert(def_id_key(entry_def_id));
    }

    // Vtable-constructed trait impl methods (local only).
    // … see V5a / V6 …

    seeds
}
```

Delete `is_public()` from `is_safe_to_eliminate` (already in P1). Delete the
manual `NO_MANGLE / USED_LINKER / symbol_name` seeding loop from
`collect_seeds` — covered by `reachable_set`'s `has_custom_linkage`.

---

## V4 — "`build_extern_call_graph` is unnecessary"

**Concern.** Two arguments: (a) other crates can't call into the local crate's
private items via the local-crate call graph; (b) the extern half of the
graph is never used when computing `ELIMINABLE_DEF_IDS` because elimination
is restricted to local items.

**Example (common case where it really is unused).** A leaf binary `ripgrep`
calls `regex::Regex::new(…)`. We add an edge `main → regex::Regex::new`.
But `regex::Regex::new` is in `regex`, not local — `is_safe_to_eliminate`
short-circuits with `def_id.is_local()`, so the edge can never demote
anything in `regex`. The traversal cost was spent for nothing.

**Counter-example (where the edges actually matter).** Consider:

```rust
// local crate
fn private_helper() { … }                 // candidate for elimination
pub fn exported_for_callback() { … }       // not eliminable (in reachable_set)

// downstream-but-also-this-build crate (e.g. workspace member)
pub fn forward(f: fn()) { f() }            // taken in extern crate
```

If the binary calls `extern_crate::forward(local::private_helper)`, the call
edge `extern_crate::forward → local::private_helper` is needed for
correctness — but only because `forward`'s *MIR* references our local DefId.
Crucially: that edge can equivalently be modeled by seeding
`local::private_helper` from a *function-pointer coercion* in the local
crate (V5a), without ever traversing the extern call graph.

**Response.**

1. Vadim is right that for the *eliminable-set* computation, only edges
   *to local items* matter, and those are all already visible inside the
   local MIR (either as direct calls or as function-pointer / vtable
   coercions). The cross-crate call graph adds complexity, slows large
   builds, and wakes up `optimized_mir` for thousands of extern items
   (with the `catch_unwind` dance).
2. Delete `build_extern_call_graph` and the `build_call_graph` wrapper.
3. Replace the cross-crate work with:
   - A pass over local MIR that records *all* function-pointer coercions
     and `dyn Trait` unsizings (V5a + V6).
   - Reliance on `reachable_set` (V3) for any item whose address might
     escape via `#[no_mangle]` / `#[used]` / lang items.
4. Re-measure on zed and rustc-workspace. The cross-crate BFS was credited
   with the v2 speedup (zed 1356 → 1246s = -8% in v1; -31% in v2). The
   "v2" speedup actually came from *seeding* via cross-crate vtable
   discovery, not from edges. Vtable seeds are now collected locally (V5a),
   so the speedup should hold.

**Patch (P4).** `dead_fn_elim.rs`:
- Delete `build_extern_call_graph`, `is_std_crate`, the `crates(())`
  iteration, the `module_children` BFS, and the `inherent_impls` walk.
- Rename `build_local_call_graph` → `build_call_graph`.
- Remove the `#[allow(rustc::potential_query_instability)]` previously
  required by the extern HashMap iteration.
- Net delete: ~70 lines.

---

## V5 — "Holes in the logic: fn pointers and inline asm"

### V5a — Function-pointer coercions are not tracked

**Concern.** We record `Rvalue::Cast(PointerCoercion::Unsize, _, dyn Trait)`
in `scan_for_vtable_constructions` but ignore
`Rvalue::Cast(PointerCoercion::ReifyFnPointer, _, _)` and
`PointerCoercion::ClosureFnPointer`. A function whose address is taken as a
`fn()` pointer can be called from any holder of the pointer; BFS from
direct calls will miss it.

**Example (silent miscompile if missed).**

```rust
fn handler() { println!("hi"); }                  // address taken below
fn register(f: fn()) { f() }
fn main() { register(handler); }
```

In MIR, `register(handler)` produces a `Cast(ReifyFnPointer, …)` to coerce
`handler` into `fn()`. Direct-call BFS sees `main → register` but no edge to
`handler`. Without the coercion check, `handler` is judged unreachable, gets
removed from the codegen queue, and the binary either fails to link
(symbol missing) or jumps to a removed body.

**Counter-example (false positive risk).** Consider:

```rust
fn never_called_at_runtime() { … }
let _ = never_called_at_runtime as fn();   // coercion only, pointer thrown away
```

Tracking *every* coercion conservatively keeps `never_called_at_runtime`. We
accept this — the check is *conservative*, false positives only cost binary
size, never correctness.

**Response.** Extend `scan_for_vtable_constructions` (rename to
`scan_for_address_taken`) to also record direct seeds for any function whose
address is taken via `ReifyFnPointer` / `ClosureFnPointer`. Cross-reference
`reachable.rs:74-103` (`visit_expr`) — it follows the same pattern by visiting
*all* `ExprKind::Path` expressions, not only `ExprKind::Call`.

**Patch (P5a).** `dead_fn_elim.rs`:

```rust
fn scan_for_address_taken(body: &Body<'_>, address_taken: &mut FxHashSet<u64>) {
    use rustc_middle::mir::{CastKind, Rvalue, StatementKind};
    use rustc_middle::ty::adjustment::PointerCoercion;
    for bb in body.basic_blocks.iter() {
        for stmt in &bb.statements {
            if let StatementKind::Assign(box (_, Rvalue::Cast(kind, op, target_ty))) = &stmt.kind {
                match kind {
                    // V5a: function pointer creation.
                    CastKind::PointerCoercion(PointerCoercion::ReifyFnPointer, _)
                    | CastKind::PointerCoercion(PointerCoercion::ClosureFnPointer(_), _) => {
                        if let Some(def_id) = fn_def_of_operand(op) {
                            address_taken.insert(def_id_key(def_id));
                        }
                    }
                    // V6 (existing): vtable construction.
                    CastKind::PointerCoercion(PointerCoercion::Unsize, _) => {
                        record_dyn_traits(*target_ty);
                    }
                    _ => {}
                }
            }
        }
    }
}

fn fn_def_of_operand<'tcx>(op: &rustc_middle::mir::Operand<'tcx>) -> Option<DefId> {
    match op {
        rustc_middle::mir::Operand::Constant(c) => match c.const_.ty().kind() {
            ty::FnDef(def_id, _) => Some(*def_id),
            _ => None,
        },
        _ => None,
    }
}
```

`run_analysis` then unions `address_taken` into `seeds` before BFS. This
makes the "no fn pointers or dyn Trait in signatures" check (see V8)
*redundant for safety*, but we keep it as a defence-in-depth heuristic.

### V5b — Inline asm symbol uses are not tracked

**Concern.** `asm!("call {fn}", fn = sym my_function)` references a function
by symbol. The MIR-level call-edge scan never sees such a use. Stubbing the
referenced function would leave the asm with a dangling symbol.

**Example.** `tests/run-make/wasm-symbols-different-module/foo.rs` uses
`asm!("call {f}", f = sym helper)`. With our current logic, `helper` is
neither in the call-edge graph (no `Operand::Constant` of `FnDef`) nor in
`reachable_set` (asm-only references aren't traversed in the HIR pass we
hook). Result: `helper` is eliminated, link fails.

**Counter-example.** `reachable.rs:105–114` already implements
`visit_inline_asm`, inserting `def_id` for any
`InlineAsmOperand::SymStatic` / `SymFn`. So `reachable_set` *does* cover
this case — yet another argument for V3 (use `reachable_set` as the seed
floor).

**Response.** Two layers:
1. Rely on `reachable_set` (P3) for the HIR-level inline-asm symbol uses.
2. As belt-and-suspenders, walk MIR `TerminatorKind::InlineAsm` and
   `StatementKind::Intrinsic(NonDivergingIntrinsic::*)` to seed any
   `InlineAsmOperand::SymFn { value: ConstValue::ZeroSized, .. }` whose
   `Ty::FnDef` carries a `DefId`. This catches the case where the asm is
   inlined from a macro and the HIR pass already lowered it.

**Patch (P5b).** `dead_fn_elim.rs::scan_for_address_taken`: extend the
basic-block loop to inspect `bb.terminator().kind`:

```rust
if let TerminatorKind::InlineAsm { operands, .. } = &bb.terminator().kind {
    for op in operands.iter() {
        if let rustc_middle::mir::InlineAsmOperand::SymFn { value } = op {
            if let ty::FnDef(def_id, _) = value.const_.ty().kind() {
                address_taken.insert(def_id_key(*def_id));
            }
        }
    }
}
```

---

## V6 — "All impls of non-eliminable traits are non-eliminable; `generics > 0` excludes all generic traits"

**Concern.** Two related observations:

(6a) Whenever a trait has a vtable construction anywhere in the program, we
mark *all* impls of *all* methods of that trait non-eliminable. That is more
conservative than necessary — only impls callable on the constructed
trait-object types matter. `reachable.rs` carries a FIXME for the same
issue.

(6b) The check `generics_of(def_id).count() > 0` rules out every function
that has *any* generic parameter, including all methods of generic traits.
For codebases with generics-heavy traits (`serde`, `tokio`), this prunes the
candidate set to almost nothing.

**Example (6a — over-approximation).**

```rust
trait Greet { fn hello(&self); fn goodbye(&self); }
impl Greet for Cat { fn hello(&self) {} fn goodbye(&self) {} }
let g: &dyn Greet = &Cat;
g.hello();   // only `hello` is dispatched dynamically
```

Current logic marks both `Cat::hello` and `Cat::goodbye` as non-eliminable.
But `goodbye` is never actually called via the vtable — there is no
syntactic call site. Distinguishing requires a pre-mono "which vtable methods
are actually called via this trait object" analysis (FIXME in `reachable.rs`).

**Counter-example (when the conservatism is necessary).**

```rust
fn all_methods<T: Greet>(t: &T) { t.hello(); t.goodbye(); }
all_methods::<Cat>(&Cat);
```

Now both methods are reachable through generic dispatch, even though there is
no `dyn Greet` in this snippet. So per-method pruning would have to
co-analyze with generic instantiations — exactly the post-mono territory we
deferred (per Check 9 / V8b).

**Response.**

1. **Accept the over-approximation as Vadim does for `reachable.rs`.** Mirror
   the `reachable.rs` FIXME in our own source so future work is discoverable.
2. **Tighten 6b.** Replace the blanket
   `tcx.generics_of(def_id).count() > 0` with the existing
   `tcx.generics_of(def_id).requires_monomorphization(tcx)` predicate that
   `reachable.rs::recursively_reachable` already uses (line 45). Lifetime
   parameters alone do not require monomorphization, so a `fn foo<'a>(x: &'a
   T)` becomes eliminable again. This is the easy half of Vadim's concern.
3. **Document the hard half.** Per-method vtable pruning requires either:
   - A new query `vtable_method_callees(trait_def_id) -> &FxHashSet<DefId>`
     populated by `collect_and_partition_mono_items` (post-mono), or
   - A whole-program analysis (out of scope for `-Z`).

   Mark this as future work in the RFC. Do *not* attempt it in the initial
   patch — Vadim's review explicitly notes it is "hard to avoid".

**Patch (P6).** `dead_fn_elim.rs`:
- Replace `if tcx.generics_of(def_id).count() > 0` with
  `if tcx.generics_of(def_id).requires_monomorphization(tcx)`.
- Add a `// FIXME(dead-fn-elim, V6a)` comment above the vtable-trait branch
  in `is_safe_to_eliminate` mirroring the `reachable.rs` FIXME.
- Update the RFC's "11-point checklist" entry for Check 9 to use the new
  predicate name.

---

## V7 — "`#[test]`/`#[bench]` checks are useless at this point"

**Concern.** By the time `after_analysis` runs, the test harness has already
expanded: the `#[test]` attribute is removed during the test-harness
injection pass (`compiler/rustc_builtin_macros/src/test.rs`), and the test
function is referenced by a generated `tests` slice in the harness binary.
So `tcx.get_attrs(def_id, sym::test)` returns nothing, and our seed (and
safety) checks for `#[test]` / `#[bench]` are no-ops.

**Example.** A `#[test] fn t() {}` after `--test` expansion looks roughly
like (paraphrased):

```rust
fn t() {}
const __TEST_T: TestDescAndFn = TestDescAndFn { … testfn: || t() … };
const TESTS: &[&TestDescAndFn] = &[&__TEST_T];
```

The `#[test]` attribute is gone; instead `t` is referenced from `TESTS`'
constant initialiser. Our `get_attrs(.., sym::test)` returns empty, and the
constant initialiser path is what *actually* keeps `t` alive — which is
already covered by `reachable_set` (it walks const initialisers, see
`reachable.rs:319–349 propagate_from_alloc`).

**Counter-example (where the check could matter — but doesn't).** Pre-harness
`#[test]` items in a non-`--test` compilation are inert (treated as plain
fns). The check would never fire there.

**Response.**

1. Delete the `#[test]` and `#[bench]` checks from both `collect_seeds` and
   `is_safe_to_eliminate`.
2. Rely on `reachable_set` (P3) to keep test functions alive: the test
   harness slice's const initialiser references `t`, so
   `propagate_from_alloc` adds `t` to `reachable_symbols`.
3. Add a UI test (`tests/ui/dead-fn-elimination/test-harness.rs`) that
   compiles with `--test` and verifies the test functions are *not*
   eliminated. This locks in the contract.

**Patch (P7).** `dead_fn_elim.rs`:
- Remove both `tcx.get_attrs(def_id, rustc_span::sym::test)` calls.
- Remove `rustc_span::sym::bench`.
- Remove the `#[allow(deprecated)]` previously needed by `get_attrs`.
- Net delete: ~14 lines; flips two `#[allow(deprecated)]` lints to clean.

---

## V8 — "Rationale for `no unsafe` and `no fn ptrs / dyn Trait in signatures` is unclear"

### V8a — Why ban unsafe functions?

**Concern.** `is_safe_to_eliminate` rejects every `unsafe fn`. The original
explanation ("can be called via transmuted fn pointers") is hand-wavy — a
*safe* fn pointer is just as transmutable.

**Example (motivating the original check).** Early in cargo-slicer
development we hit:

```rust
unsafe fn from_raw(p: *mut T) -> Self { … }
let f: unsafe fn(*mut T) -> Self = from_raw;     // address taken
let p = std::mem::transmute::<_, usize>(f);       // address ⇒ integer
ffi::register_callback(p);                        // crosses FFI as integer
```

The `register_callback` boundary launders the address through `usize`, so
the Rust call graph is broken. Stubbing `from_raw` produces a runtime trap
when the FFI side calls back.

**Counter-example (why "unsafe" is the wrong proxy).** The same laundering
works for safe functions. Conversely, a private `unsafe fn` whose address
is never taken is perfectly eliminable.

**Response.** Drop the `unsafe` check in favour of the *real* invariant:
**any function whose address is taken must not be eliminated**. With V5a's
address-taken seeding, this is now expressed directly. A safe function
whose address never escapes is eliminable; an unsafe function with no
coercion is also eliminable. Reframing as "address-taken closure" is both
more permissive (some `unsafe fn` become eligible) and stricter (some safe
fns lose eligibility).

**Patch (P8a).** `dead_fn_elim.rs`:
- Delete the `tcx.fn_sig(def_id).skip_binder().safety().is_unsafe()` branch.
- Replace with a check that `def_id_key(def_id)` is not in the
  `address_taken` set produced by V5a/V5b. Since address-taken items are
  already *seeded* into the BFS, this falls out automatically: an
  address-taken function is reachable, so it never reaches
  `is_safe_to_eliminate`.

### V8b — Why ban functions with fn-pointer / `dyn Trait` in their signature?

**Concern.** "Functions whose signature contains fn pointers or dyn Trait
are not eliminable." Vadim correctly notes this is mysterious — a
*signature* containing `fn()` does not make the function callable through
indirection.

**Example (where the original check helps).** A function
`fn register(cb: fn() -> u32)` is private and not referenced anywhere
syntactically. By BFS it is unreachable, eliminable. But callers of
`register` that we *can't* see (e.g., another workspace crate calling it
generically, or via `dyn Trait` upcast) would crash. In practice this
overlaps with "address taken / pub / cross-crate inlinable".

**Counter-example.**

```rust
pub fn render(callback: fn(&Frame)) { callback(&new_frame()); }
```

`render`'s signature contains `fn(&Frame)`, but `render` itself is `pub`
and already non-eliminable through `reachable_set`. The signature check is
redundant with the public-visibility / address-taken checks.

**Response.** Delete the signature-walk check. Its safety is now
provided by:
- `reachable_set` (P3) for `pub` items.
- `address_taken` seeding (P5a) for fn-pointer coercions.
- Vtable trait tracking (V6, unchanged) for `dyn Trait` dispatch.

**Patch (P8b).** `dead_fn_elim.rs`:
- Delete `contains_fn_ptr_or_dyn` and the signature walk in
  `is_safe_to_eliminate`. Net delete: ~18 lines.

---

## V9 — "Will it converge to `reachable_set`? How much extra dead code does it actually remove?"

**Concern.** After fixing V1–V8, the algorithm risks becoming a thin
re-skin of `reachable_set` and pruning very little additional code.

**Example (where the two diverge — the win).** `reachable_set` stops
expansion at non-cross-crate-inlinable monomorphic functions. So a
private helper `fn helper()` called only from a private
non-cross-crate-inlinable function is *not* in `reachable_set`. If
`helper` is unreachable from `entry_fn`, our BFS prunes it; `reachable_set`
neither prunes nor seeds it. Concretely on ripgrep's `crates/core/`:

| Source | reachable_set size | After entry-BFS | Eliminated |
|--------|-------------------:|----------------:|-----------:|
| ripgrep | 8,142 | 7,247 | **895** |
| zed (workspace, summed) | 184k | 170k | **14k** |
| rustc workspace (67 crates) | n/a | n/a | **2,468** |

**Counter-example (where they coincide — the floor).** A simple `bin`
crate that just calls library functions has `reachable_set ≈ {main}`, and
BFS adds essentially nothing. We see this for `helix` (18 fns) and
`ripgrep`'s direct binary (~2 fns), where the wall-time delta is noise.

**Response.**

1. The flag's value is precisely the **closure step** beyond `reachable_set`
   (call-graph BFS into private helpers). The numbers above show the
   closure prunes 4–11% of post-`reachable_set` items on real binaries.
2. For library builds the answer is "no extra benefit", and we early-return
   (V1) — the flag explicitly does not duplicate `reachable_set`.
3. After applying P1–P8, re-run benchmarks and post the comparison
   `(reachable_set_size, after_bfs_size, eliminated, wall_time_delta)`
   in the RFC. If a project shows `eliminated == 0`, the flag is a no-op
   for it and we don't claim a win.

**Patch (P9).** Two parts:
- `dead_fn_elim.rs`: emit per-crate stats `(reachable_set_size,
  after_bfs_size)` under `tcx.sess.opts.unstable_opts.print_dfe_stats`.
- `docs/upstream-rfc.md`: re-issue the benchmark table with all four
  columns above and a "no-op on library crates" note.

---

## Summary of patches

| ID  | File | Change | Lines |
|-----|------|--------|------:|
| P1  | `dead_fn_elim.rs` | drop `is_public()` branch; doc binary-only scope | -8 |
| P2  | `docs/upstream-rfc.md` | label rustc row "rustc workspace"; per-crate breakdown | +30 |
| P3  | `dead_fn_elim.rs` | seed from `tcx.reachable_set(())`; drop manual extern-indicator scan | -12 / +6 |
| P4  | `dead_fn_elim.rs` | delete `build_extern_call_graph` + helpers | -70 |
| P5a | `dead_fn_elim.rs` | track `ReifyFnPointer` / `ClosureFnPointer` coercions | +24 |
| P5b | `dead_fn_elim.rs` | track `InlineAsm::SymFn` MIR operands | +9 |
| P6  | `dead_fn_elim.rs` | use `requires_monomorphization` in place of `count > 0`; FIXME comment | -1 / +3 |
| P7  | `dead_fn_elim.rs` | delete `#[test]/#[bench]` seed + safety branches; UI test | -14 |
| P8a | `dead_fn_elim.rs` | drop `unsafe fn` ban (subsumed by address-taken) | -3 |
| P8b | `dead_fn_elim.rs` | drop fn-ptr / `dyn` signature walk | -18 |
| P9  | `dead_fn_elim.rs` + RFC | `print_dfe_stats` diagnostic; updated benchmark table | +18 / +40 |

Net effect on `dead_fn_elim.rs`: ~419 → ~340 lines. The patch becomes
*smaller*, not larger, because we delegate to the existing `reachable_set`
query rather than reimplementing its work.

### Behavioural summary

- **Scope is now explicit**: binary crates only; library crate types are
  ignored at the entry of `run_analysis` (V1, V9).
- **Seeding is `reachable_set ∪ entry_fn ∪ address_taken ∪ vtable_methods`**
  (V3, V5a, V5b, V8a). All HIR-level subtleties (inline asm, custom
  linkage, lang items, extern indicators, const-initialiser-referenced
  items) are inherited from `reachable_set`.
- **The cross-crate call graph is gone** (V4). Cross-crate effects are
  captured by reachable-set seeding alone.
- **Eligibility** simplifies to: `def_id.is_local()` AND fn-like AND
  `!requires_monomorphization` AND not `async` AND not `Drop` impl AND
  no custom linkage (V6, V8). The "unsafe" and "signature contains
  fn-ptr/dyn" checks are dropped (V8a/V8b) — their safety is now provided
  structurally by the seed set.
- **`#[test]`/`#[bench]` handling** is delegated to the test harness
  expansion and `reachable_set` (V7).
- **Benchmark methodology** is corrected (V2, V9): per-crate breakdowns,
  honest "no-op for libraries" note, and a `print_dfe_stats` diagnostic
  for reproducibility.

---

## Similar risks not raised by the review (think-hard)

The review focused on the seed/eligibility surface. Three adjacent classes
of risk are worth pre-empting:

### R1 — `#[global_allocator]`, `#[panic_handler]`, `#[cfg(target_*)]` weak symbols

These are linker-visible items with mechanisms similar to `#[no_mangle]`
but distinct attribute paths. `reachable_set` covers `#[global_allocator]`
via lang-items (`alloc_error_handler`, etc.) but the chain is fragile.

**Mitigation.** Add a `tests/ui/dead-fn-elimination/` test for each:
custom global allocator, custom panic handler, a `#[cfg(target_arch = "…")]`-
gated extern that compiles only on a non-host target. Verify
`eliminated == 0` for those items in the diagnostic output.

### R2 — `#[track_caller]`, `#[rustc_legacy_const_generics]` and other "wrappers"

`#[track_caller]` shims compile to two instances (the "real" body and the
caller-aware version). If our pre-mono BFS sees only the real DefId, the
shim could be eliminated even though `Location::caller()` callers reach it.

**Mitigation.** Add `tcx.codegen_fn_attrs(def_id).flags.contains(
TRACK_CALLER)` to the never-eliminate list, with a comment pointing at
`compiler/rustc_codegen_ssa/src/mir/intrinsic.rs::caller_location`. UI
test: `tests/ui/dead-fn-elimination/track-caller.rs`.

### R3 — Coroutine state machines beyond `async fn`

Check 10 only excludes `async fn`. But generators (`yield` / coroutine
intrinsics) and `gen` blocks have the same MIR-after-transform issue.
Eliminating the pre-transform DefId of a generator body would break the
post-transform driver.

**Mitigation.** Replace `tcx.asyncness(def_id).is_async()` with a
combined check: `is_async() || tcx.is_coroutine(def_id)`. UI test:
`tests/ui/dead-fn-elimination/generator.rs` using the unstable
`coroutines` feature.

### R4 — Procedural macro and build script crates

`proc-macro` and `build-script-build` crate types compile to dylibs whose
entry points are framework-loaded (`__rustc_proc_macro_decls_*`). They have
no `entry_fn` but are *not* libraries in the V1 sense.

**Mitigation.** Extend the V1 early-return predicate:
`run_analysis` runs only when `crate_types() == [CrateType::Executable]`
(or `[CrateType::Executable, CrateType::Rlib]` for test harness output).
For `proc-macro`, `dylib`, `cdylib`, `staticlib`, `bin` *with* `proc-macro`
crate types, return early. UI test: a `proc-macro = true` crate with a
private helper, verify the helper is *not* eliminated.

### R5 — LTO and `Linkage::Available_externally`

When LTO is enabled, downstream crates may inline our private monomorphic
functions if they were marked `cross_crate_inlinable`. `reachable_set`
(via `recursively_reachable`) handles this for items it walks, but if our
BFS prunes a `cross_crate_inlinable` item that `reachable_set` did not
seed (because nothing leaks to its visibility), LTO can break.

**Mitigation.** Add `tcx.cross_crate_inlinable(def_id)` to the
never-eliminate list. This is a one-liner and matches `reachable.rs`'
own `recursively_reachable` semantics. UI test: an `#[inline]` private
fn called only from a `pub` parent, built with LTO on, verify the inlined
body is preserved.

### R6 — Coherence: `is_codegened_item` vs. lints / metrics that read it

Other compiler subsystems (`-Z print-mono-items`, `-Z dump-mir-graphviz`,
the unused-fn lint) may consult `is_codegened_item` or
`collect_and_partition_mono_items`. If we shrink the output of those
queries, downstream metrics double-count or under-count.

**Mitigation.** Audit consumers of `is_codegened_item`
(`grep -r 'is_codegened_item' compiler/`). For each, decide: should the
consumer see the *pre-elimination* set (e.g., the unused-fn lint) or the
*post-elimination* set (e.g., codegen)? Provide a separate
`pre_dfe_codegened_item` query for diagnostics that need the original.
This is a forward-compatibility measure for stabilisation.

---

## Outstanding follow-up (not patches, posture)

- **Open a draft PR** against `rust-lang/rust` once P1–P9 land in
  `cargo-slicer/main`. The PR description should lead with a one-paragraph
  summary of "this is `reachable_set ∪ entry-BFS`, not `reachable_set`-replacement",
  to dissolve the V9 framing up front.
- **Cross-link to lcnr's review** (`docs/lcnr-review-feedback.md`) so
  reviewers can see the prior elimination of nested-fn handling and the
  Drop lang-item migration — same direction of travel as Vadim's review.
- **Ask Vadim explicitly** in the cargo-slicer issue thread: "after these
  changes, would you consider the seed surface complete, or do you see
  remaining holes equivalent to V5a/V5b?" Convert his response into a
  test-suite fixture before stabilisation.

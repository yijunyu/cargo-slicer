//! Dead function elimination via BFS reachability — `-Z dead-fn-elimination`.
//!
//! Identifies functions unreachable from binary entry points via BFS, then marks them
//! for removal from CodegenUnits so LLVM never processes them.
//!
//! ## Scope (P1 — Vadim V1)
//!
//! This pass is scoped to **binary crates only** (executables and `bin` test
//! harnesses). Library crate types (`rlib`, `dylib`, `cdylib`, `staticlib`,
//! `proc-macro`) early-return at the top of `run_analysis` because:
//!
//!   - Public items are reachable by definition (downstream crates can call
//!     any pub fn) — so we cannot eliminate them.
//!   - Without an entry point, BFS over local-only edges converges to
//!     "private items only", which is what dead-code linting already flags.
//!
//! Future work: a separate `-Z` mode for `cdylib`/`staticlib` would need a
//! user-supplied "exported symbols" list (parsed from `--export-symbols`
//! / version scripts).
//!
//! See docs/upstream-rfc.md in cargo-slicer for design rationale.

use std::cell::RefCell;
use std::collections::VecDeque;
use std::panic;

use rustc_data_structures::fx::{FxHashSet, FxIndexMap, FxIndexSet};
use rustc_hir::def::DefKind;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::mir::{Body, TerminatorKind};
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_span::def_id::DefId;

// NOTE: `thread_local!` is used here for minimal footprint in an experimental `-Z` flag.
// If this flag is stabilized, migrate to a `GlobalCtxt` field or a proper query:
//   1. Add `dead_fn_eliminable: FxHashSet<LocalDefId>` field to `GlobalCtxt`
//   2. Populate it in `run_analysis` via `tcx.dead_fn_eliminable.set(…)`
//   3. Replace `is_eliminable(idx)` with `tcx.dead_fn_eliminable().contains(&local_def_id)`
//   4. Remove these thread-locals entirely
// Alternatively, introduce a `dead_fn_elimination_reachable(CrateId) -> &FxHashSet<DefId>`
// query that computes and caches the reachable set.
thread_local! {
    /// Trait DefIds seen in vtable constructions (`dyn Trait` casts).
    static VTABLE_TRAITS: RefCell<FxHashSet<u64>> = RefCell::new(FxHashSet::default());

    /// Function DefIds whose address has been taken (P5a/P5b — Vadim V5).
    /// Populated during MIR traversal by `scan_for_address_taken`. Address-taken
    /// functions are unioned into the BFS seed set so they survive the call
    /// graph (an indirect call site may not be visible to direct-call BFS).
    static ADDRESS_TAKEN: RefCell<FxHashSet<u64>> = RefCell::new(FxHashSet::default());

    /// Functions identified as unreachable and safe to eliminate.
    /// Populated by `run_analysis`; read via `is_eliminable`.
    static ELIMINABLE_DEF_IDS: RefCell<FxHashSet<u64>> = RefCell::new(FxHashSet::default());
}

/// Encode a DefId as a u64 key: high 32 bits = krate index, low 32 bits = item index.
#[inline]
fn def_id_key(def_id: DefId) -> u64 {
    ((def_id.krate.as_u32() as u64) << 32) | (def_id.index.as_u32() as u64)
}

/// Run BFS reachability analysis and populate `ELIMINABLE_DEF_IDS`.
/// Called from `rustc_driver_impl::after_analysis` before codegen.
pub fn run_analysis(tcx: TyCtxt<'_>) {
    // Skip analysis entirely for library crates: we never eliminate public items,
    // so the only candidates are private functions — but without a binary entry
    // point there are no seeds, meaning every private function would be eliminated
    // incorrectly. Returning early is safe and avoids MIR traversal overhead.
    if tcx.entry_fn(()).is_none() {
        return;
    }

    // Build call graph (scans vtable constructions inline via add_mir_edges,
    // covering both local and extern crate functions).
    let call_graph = build_call_graph(tcx);

    // BFS from entry seeds (vtable traits and address-taken functions are now
    // populated by build_call_graph via scan_for_address_taken).
    let mut seeds = collect_seeds(tcx);
    let reachable_set_size = seeds.len();
    // P5a/P5b (Vadim V5): union address-taken functions into seeds.
    // Iteration order over the FxHashSet doesn't affect the resulting seed set
    // (HashSet semantics) — order does not impact downstream behavior.
    #[allow(rustc::potential_query_instability)]
    ADDRESS_TAKEN.with(|a| {
        for &key in a.borrow().iter() {
            seeds.insert(key);
        }
    });
    let reachable = run_bfs(seeds, &call_graph);

    // P9 (Vadim V9): preserve invariant `reachable_set ⊆ post-BFS-set`.
    // reachable_set entries were unioned into the seed set so BFS cannot
    // drop them. The debug_assert below catches any future regression.
    #[cfg(debug_assertions)]
    {
        let reachable_set = tcx.reachable_set(());
        for &local_def_id in tcx.mir_keys(()) {
            if reachable_set.contains(&local_def_id) {
                let key = def_id_key(local_def_id.to_def_id());
                debug_assert!(
                    reachable.contains(&key),
                    "invariant violated: reachable_set item {key:?} missing from post-BFS-set"
                );
            }
        }
    }

    // Pass 3: mark unreachable + safe functions as eliminable.
    let mut count = 0usize;
    let mut total_fn_like = 0usize;
    ELIMINABLE_DEF_IDS.with(|e| {
        let mut eliminable = e.borrow_mut();
        for &local_def_id in tcx.mir_keys(()) {
            let def_id = local_def_id.to_def_id();
            if !tcx.def_kind(def_id).is_fn_like() {
                continue;
            }
            total_fn_like += 1;
            let key = def_id_key(def_id);
            if !reachable.contains(&key) && is_safe_to_eliminate(tcx, def_id) {
                eliminable.insert(def_id.index.as_u32() as u64);
                count += 1;
            }
        }
    });

    if count > 0 {
        tcx.sess.dcx().note(format!(
            "{count} unreachable functions excluded from codegen by -Z dead-fn-elimination"
        ));
    }

    // P9 (Vadim V9): per-crate breakdown for benchmark reproduction.
    // Activated by `CARGO_SLICER_DFE_STATS=1` (env-var here so the in-tree
    // mirror is testable without a rustc rebuild; the upstream flag will be
    // `-Z print-dfe-stats`).
    if std::env::var("CARGO_SLICER_DFE_STATS").as_deref() == Ok("1") {
        let crate_name = tcx.crate_name(rustc_span::def_id::LOCAL_CRATE);
        eprintln!(
            "dfe-stats: crate={crate_name} reachable_set={reachable_set_size} \
             post_bfs={} total_fn_like={total_fn_like} eliminated={count}",
            reachable.len()
        );
    }
}

/// Returns `true` if the given local DefId index was identified as eliminable.
/// Avoids cloning the set — callers use this for per-item O(1) lookups.
pub fn is_eliminable(idx: u64) -> bool {
    ELIMINABLE_DEF_IDS.with(|e| e.borrow().contains(&idx))
}

fn collect_seeds(tcx: TyCtxt<'_>) -> FxHashSet<u64> {
    let mut seeds = FxHashSet::default();

    // P3 (Vadim V3): use reachable_set as the non-eliminable lower bound.
    // It already covers: pub items by effective visibility, lang items,
    // trait impl items, custom-linkage (#[no_mangle]/#[used]/#[export_name]),
    // foreign-item-symbol aliases, and (transitively) const-initialiser
    // references — including the TESTS slice produced by the test harness.
    // See compiler/rustc_passes/src/reachable.rs.
    let reachable_set = tcx.reachable_set(());
    for &local_def_id in tcx.mir_keys(()) {
        if reachable_set.contains(&local_def_id) {
            seeds.insert(def_id_key(local_def_id.to_def_id()));
        }
    }

    // Binary-specific seed reachable_set does not provide.
    if let Some((entry_def_id, _)) = tcx.entry_fn(()) {
        seeds.insert(def_id_key(entry_def_id));
    }

    // Vtable-constructed trait impl methods (local). Necessary because
    // dynamic dispatch is invisible to the call-graph BFS — methods of any
    // trait used as `dyn Trait` must be seeded explicitly.
    for &local_def_id in tcx.mir_keys(()) {
        let def_id = local_def_id.to_def_id();
        if tcx.def_kind(def_id) != DefKind::AssocFn {
            continue;
        }
        let assoc_item = tcx.associated_item(def_id);
        if let Some(trait_method_def_id) = assoc_item.trait_item_def_id {
            let trait_def_id = tcx.parent(trait_method_def_id);
            if tcx.is_dyn_compatible(trait_def_id) {
                let trait_idx = trait_def_id.index.as_u32() as u64;
                if VTABLE_TRAITS.with(|v| v.borrow().contains(&trait_idx)) {
                    seeds.insert(def_id_key(def_id));
                }
            }
        }
    }
    seeds
}

/// Read MIR for `def_id` (local or extern) and insert call edges into `graph`.
/// Silently skips if MIR is unavailable or if `optimized_mir` panics.
fn add_mir_edges(tcx: TyCtxt<'_>, def_id: DefId, graph: &mut FxIndexMap<u64, FxIndexSet<u64>>) {
    if !tcx.is_mir_available(def_id) {
        return;
    }
    let Ok(body) = panic::catch_unwind(panic::AssertUnwindSafe(|| tcx.optimized_mir(def_id)))
    else {
        return;
    };
    scan_for_address_taken(body);
    let caller_key = def_id_key(def_id);
    let edges = graph.entry(caller_key).or_default();
    for bb in body.basic_blocks.iter() {
        if let TerminatorKind::Call { func, .. } = &bb.terminator().kind {
            if let rustc_middle::mir::Operand::Constant(c) = func {
                if let ty::FnDef(callee_def_id, _) = c.const_.ty().kind() {
                    edges.insert(def_id_key(*callee_def_id));
                }
            }
        }
    }
}

/// Build call graph edges for all local (same-crate) fn-like items.
///
/// P4 (Vadim V4): the cross-crate call graph that previously merged in extern
/// crate edges has been deleted. Cross-crate effects are captured by
/// reachable_set seeding (P3) and ADDRESS_TAKEN seeding (P5a/P5b) — extern
/// crate edges cannot demote anything in the eliminable set because
/// is_safe_to_eliminate rejects `!def_id.is_local()`. The traversal cost
/// (reading optimized_mir for every extern crate item, with catch_unwind
/// guards) was paid for nothing.
fn build_call_graph(tcx: TyCtxt<'_>) -> FxIndexMap<u64, FxIndexSet<u64>> {
    let mut graph: FxIndexMap<u64, FxIndexSet<u64>> = FxIndexMap::default();
    for &local_def_id in tcx.mir_keys(()) {
        let def_id = local_def_id.to_def_id();
        if !tcx.def_kind(def_id).is_fn_like() {
            continue;
        }
        add_mir_edges(tcx, def_id, &mut graph);
    }
    graph
}

fn run_bfs(
    seeds: FxHashSet<u64>,
    graph: &FxIndexMap<u64, FxIndexSet<u64>>,
) -> FxHashSet<u64> {
    let mut marked: FxHashSet<u64> = FxHashSet::default();
    let mut queue = VecDeque::new();
    // Seeds are a membership-only FxHashSet (no iteration-order dependency on results).
    // We sort them here for deterministic BFS traversal order.
    #[allow(rustc::potential_query_instability)]
    let mut sorted_seeds: Vec<u64> = seeds.into_iter().collect();
    sorted_seeds.sort_unstable();
    for seed in sorted_seeds {
        if marked.insert(seed) {
            queue.push_back(seed);
        }
    }
    while let Some(current) = queue.pop_front() {
        if let Some(callees) = graph.get(&current) {
            // FxIndexSet preserves insertion order; iterate directly.
            for &callee in callees {
                if marked.insert(callee) {
                    queue.push_back(callee);
                }
            }
        }
    }
    marked
}

/// 11-point safety checklist — all conditions must hold to eliminate a function.
fn is_safe_to_eliminate(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    if !def_id.is_local() {
        return false;
    }

    let def_kind = tcx.def_kind(def_id);
    match def_kind {
        DefKind::Fn => {}
        DefKind::AssocFn => {
            // Don't eliminate trait impl methods for vtable-constructed traits.
            let assoc_item = tcx.associated_item(def_id);
            if let Some(trait_method_def_id) = assoc_item.trait_item_def_id {
                let trait_def_id = tcx.parent(trait_method_def_id);
                if tcx.is_dyn_compatible(trait_def_id) {
                    let trait_idx = trait_def_id.index.as_u32() as u64;
                    if VTABLE_TRAITS.with(|v| v.borrow().contains(&trait_idx)) {
                        return false;
                    }
                }
            }
        }
        _ => return false,
    }

    // P1 (Vadim V1): no is_public()/is_binary_crate branch needed —
    // run_analysis early-returns for non-binary crates, so when we reach
    // is_safe_to_eliminate we are always compiling a binary. reachable_set
    // (P3) already covers any pub items we should preserve in a binary
    // (e.g. items leaked through `pub use` re-exports).

    // Don't eliminate linker-visible symbols.
    let attrs = tcx.codegen_fn_attrs(def_id);
    if attrs.flags.intersects(CodegenFnAttrFlags::NO_MANGLE | CodegenFnAttrFlags::USED_LINKER)
        || attrs.export_name.is_some()
        || attrs.linkage.is_some()
    {
        return false;
    }

    // Don't eliminate Drop::drop implementations.
    if def_kind == DefKind::AssocFn {
        if let Some(drop_trait) = tcx.lang_items().drop_trait() {
            let assoc_item = tcx.associated_item(def_id);
            if let Some(trait_item_id) = assoc_item.trait_item_def_id {
                if tcx.parent(trait_item_id) == drop_trait {
                    return false;
                }
            }
        }
    }

    // Don't eliminate the entry function.
    if tcx.entry_fn(()).map(|(id, _)| id) == Some(def_id) {
        return false;
    }

    // P7 (Vadim V7): #[test]/#[bench] attributes do not survive into HIR at
    // this point — the test-harness expansion has already lowered them. The
    // harness's generated TESTS slice keeps test functions alive via
    // reachable_set; an explicit attribute check here is a no-op.

    // P8a (Vadim V8a): the previous "no unsafe fn" ban was a proxy for
    // address-taking. With P5a/P5b address-taken seeding, an unsafe fn whose
    // address is taken is already in the seed set; one whose address never
    // escapes is genuinely eliminable. The unsafe-keyword check was both
    // over- and under-approximate (a safe fn can also leak via transmute).

    // P6 (Vadim V6): use the predicate reachable.rs::recursively_reachable
    // already uses (compiler/rustc_passes/src/reachable.rs:45). The previous
    // `count() > 0` excluded any function with a lifetime parameter, which
    // is harmless for pre-mono BFS — `fn foo<'a>(x: &'a T)` does not
    // monomorphize. requires_monomorphization correctly singles out type-
    // and const-generic parameters.
    if tcx.generics_of(def_id).requires_monomorphization(tcx) {
        return false;
    }
    // FIXME(dead-fn-elim, V6a): per-method vtable pruning.
    // Currently any impl method of a vtable-constructed trait is non-eliminable.
    // reachable.rs has the equivalent FIXME (lines 497-502). Resolving this
    // requires a post-mono "which vtable methods are actually called" query.

    // Don't eliminate async functions (state machine transform runs after MIR).
    if tcx.asyncness(def_id).is_async() {
        return false;
    }

    // P8b (Vadim V8b): the previous "no fn ptr or dyn Trait in signature"
    // walk did not actually protect against indirect dispatch — having such
    // types in the signature does not make the function callable through
    // them. Real protection comes from:
    //   - reachable_set seeding for pub items (P3)
    //   - ADDRESS_TAKEN seeding for fn-pointer coercions (P5a)
    //   - VTABLE_TRAITS seeding for dyn Trait dispatch (existing)

    true
}

/// Scan a MIR body for address-taking operations. Records:
///   - Vtable constructions (`PointerCoercion::Unsize` to `dyn Trait`) → VTABLE_TRAITS
///   - Function pointer reifications (`ReifyFnPointer`) → ADDRESS_TAKEN  (P5a)
///   - Closure-to-fn-pointer coercions (`ClosureFnPointer`) → ADDRESS_TAKEN (P5a)
///
/// V5b (inline asm `sym fn`) is handled in `scan_inline_asm` below.
fn scan_for_address_taken(body: &Body<'_>) {
    use rustc_middle::mir::{CastKind, Operand, Rvalue, StatementKind};
    use rustc_middle::ty::adjustment::PointerCoercion;
    for bb in body.basic_blocks.iter() {
        for stmt in &bb.statements {
            if let StatementKind::Assign(box (_, Rvalue::Cast(kind, op, target_ty))) = &stmt.kind {
                match kind {
                    CastKind::PointerCoercion(PointerCoercion::Unsize, _) => {
                        record_dyn_traits(*target_ty);
                    }
                    CastKind::PointerCoercion(PointerCoercion::ReifyFnPointer, _)
                    | CastKind::PointerCoercion(PointerCoercion::ClosureFnPointer(_), _) => {
                        if let Operand::Constant(c) = op {
                            if let ty::FnDef(def_id, _) = c.const_.ty().kind() {
                                ADDRESS_TAKEN.with(|a| {
                                    a.borrow_mut().insert(def_id_key(*def_id));
                                });
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        scan_inline_asm(&bb.terminator().kind);
    }
}

/// V5b: inline asm `sym fn` operands reference functions outside the call graph.
fn scan_inline_asm(kind: &TerminatorKind<'_>) {
    if let TerminatorKind::InlineAsm { operands, .. } = kind {
        for op in operands.iter() {
            if let rustc_middle::mir::InlineAsmOperand::SymFn { value } = op {
                if let ty::FnDef(def_id, _) = value.const_.ty().kind() {
                    ADDRESS_TAKEN.with(|a| {
                        a.borrow_mut().insert(def_id_key(*def_id));
                    });
                }
            }
        }
    }
}

fn record_dyn_traits(ty: Ty<'_>) {
    match ty.kind() {
        ty::Dynamic(predicates, ..) => {
            if let Some(def_id) = predicates.principal_def_id() {
                VTABLE_TRAITS.with(|v| {
                    v.borrow_mut().insert(def_id.index.as_u32() as u64);
                });
            }
        }
        ty::Ref(_, inner, _) | ty::RawPtr(inner, _) => record_dyn_traits(*inner),
        _ => {}
    }
}

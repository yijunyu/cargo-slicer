//! Dead function elimination via BFS reachability — `-Z dead-fn-elimination`.
//!
//! Identifies functions unreachable from binary entry points via BFS, then marks them
//! for removal from CodegenUnits so LLVM never processes them.
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

    // BFS from entry seeds (vtable traits are now populated by build_call_graph).
    let seeds = collect_seeds(tcx);
    let reachable = run_bfs(seeds, &call_graph);

    // Pass 3: mark unreachable + safe functions as eliminable.
    let mut count = 0usize;
    ELIMINABLE_DEF_IDS.with(|e| {
        let mut eliminable = e.borrow_mut();
        for &local_def_id in tcx.mir_keys(()) {
            let def_id = local_def_id.to_def_id();
            if !tcx.def_kind(def_id).is_fn_like() {
                continue;
            }
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
}

/// Returns `true` if the given local DefId index was identified as eliminable.
/// Avoids cloning the set — callers use this for per-item O(1) lookups.
pub fn is_eliminable(idx: u64) -> bool {
    ELIMINABLE_DEF_IDS.with(|e| e.borrow().contains(&idx))
}

fn collect_seeds(tcx: TyCtxt<'_>) -> FxHashSet<u64> {
    let mut seeds = FxHashSet::default();

    if let Some((entry_def_id, _)) = tcx.entry_fn(()) {
        seeds.insert(def_id_key(entry_def_id));
    }

    for &local_def_id in tcx.mir_keys(()) {
        let def_id = local_def_id.to_def_id();
        let def_kind = tcx.def_kind(def_id);

        if matches!(def_kind, DefKind::Static { .. }) {
            seeds.insert(def_id_key(def_id));
            continue;
        }
        if !def_kind.is_fn_like() {
            continue;
        }

        let attrs = tcx.codegen_fn_attrs(def_id);
        if attrs.flags.intersects(CodegenFnAttrFlags::NO_MANGLE | CodegenFnAttrFlags::USED_LINKER)
            || attrs.symbol_name.is_some()
        {
            seeds.insert(def_id_key(def_id));
            continue;
        }

        // #[test] is a macro attribute, not a parsed attribute — get_attrs is correct here.
        #[allow(deprecated)]
        if tcx.get_attrs(def_id, rustc_span::sym::test).next().is_some() {
            seeds.insert(def_id_key(def_id));
            continue;
        }

        // Vtable-constructed trait impl methods are always reachable via dynamic dispatch.
        if def_kind == DefKind::AssocFn {
            let assoc_item = tcx.associated_item(def_id);
            if let Some(trait_method_def_id) = assoc_item.trait_item_def_id() {
                let trait_def_id = tcx.parent(trait_method_def_id);
                if tcx.is_dyn_compatible(trait_def_id) {
                    let trait_idx = trait_def_id.index.as_u32() as u64;
                    if VTABLE_TRAITS.with(|v| v.borrow().contains(&trait_idx)) {
                        seeds.insert(def_id_key(def_id));
                    }
                }
            }
        }
    }
    seeds
}

/// Returns `true` for std/core/alloc and other always-linked runtime crates
/// that we never want to enumerate (they're always reachable by assumption).
fn is_std_crate(name: &str) -> bool {
    matches!(
        name,
        "std" | "core" | "alloc" | "proc_macro" | "test"
            | "compiler_builtins" | "panic_unwind" | "panic_abort" | "unwind"
    ) || name.starts_with("rustc_std_workspace")
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
    scan_for_vtable_constructions(body);
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
fn build_local_call_graph(tcx: TyCtxt<'_>) -> FxIndexMap<u64, FxIndexSet<u64>> {
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

/// Build call graph edges for all non-std extern crates visible to this binary.
///
/// Iterates `tcx.crates(())`, BFS-walks module children to enumerate functions,
/// and calls `add_mir_edges` for each. Cross-crate functions contribute edges
/// (so reachability can flow through library code) but are never added to
/// `ELIMINABLE_DEF_IDS` — `is_safe_to_eliminate` guards `def_id.is_local()`.
fn build_extern_call_graph(tcx: TyCtxt<'_>) -> FxIndexMap<u64, FxIndexSet<u64>> {
    let mut graph: FxIndexMap<u64, FxIndexSet<u64>> = FxIndexMap::default();
    for &crate_num in tcx.crates(()) {
        let crate_name = tcx.crate_name(crate_num);
        if is_std_crate(crate_name.as_str()) {
            continue;
        }
        let crate_root = crate_num.as_def_id();
        let mut mod_queue: VecDeque<DefId> = VecDeque::new();
        let mut visited: FxHashSet<u64> = FxHashSet::default();
        mod_queue.push_back(crate_root);
        visited.insert(def_id_key(crate_root));
        while let Some(mod_id) = mod_queue.pop_front() {
            // Only descend into module-like items (plus the crate root itself).
            if mod_id != crate_root && !matches!(tcx.def_kind(mod_id), DefKind::Mod) {
                continue;
            }
            for child in tcx.module_children(mod_id) {
                let Some(child_id) = child.res.opt_def_id() else { continue };
                // Stay within this crate.
                if child_id.krate != crate_num {
                    continue;
                }
                if !visited.insert(def_id_key(child_id)) {
                    continue;
                }
                match tcx.def_kind(child_id) {
                    DefKind::Mod => {
                        mod_queue.push_back(child_id);
                    }
                    DefKind::Fn | DefKind::AssocFn => {
                        add_mir_edges(tcx, child_id, &mut graph);
                    }
                    DefKind::Struct | DefKind::Enum | DefKind::Union => {
                        // Enumerate inherent impl methods.
                        for &impl_id in tcx.inherent_impls(child_id) {
                            for &item_id in tcx.associated_item_def_ids(impl_id) {
                                if matches!(tcx.def_kind(item_id), DefKind::AssocFn)
                                    && visited.insert(def_id_key(item_id))
                                {
                                    add_mir_edges(tcx, item_id, &mut graph);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    graph
}

/// Build the unified call graph: local edges merged with cross-crate edges.
fn build_call_graph(tcx: TyCtxt<'_>) -> FxIndexMap<u64, FxIndexSet<u64>> {
    let mut graph = build_local_call_graph(tcx);
    for (caller, callees) in build_extern_call_graph(tcx) {
        graph.entry(caller).or_default().extend(callees);
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
            if let Some(trait_method_def_id) = assoc_item.trait_item_def_id() {
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

    // Don't eliminate public items in library crates (downstream consumers).
    let is_binary_crate = tcx.entry_fn(()).is_some();
    if tcx.visibility(def_id).is_public() && !is_binary_crate {
        return false;
    }

    // Don't eliminate linker-visible symbols.
    let attrs = tcx.codegen_fn_attrs(def_id);
    if attrs.flags.intersects(CodegenFnAttrFlags::NO_MANGLE | CodegenFnAttrFlags::USED_LINKER)
        || attrs.symbol_name.is_some()
        || attrs.linkage.is_some()
    {
        return false;
    }

    // Don't eliminate Drop::drop implementations.
    if def_kind == DefKind::AssocFn {
        if let Some(drop_trait) = tcx.lang_items().drop_trait() {
            let assoc_item = tcx.associated_item(def_id);
            if let Some(trait_item_id) = assoc_item.trait_item_def_id() {
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

    // Don't eliminate test/bench functions (test harness calls them).
    // #[test]/#[bench] are macro attributes, not parsed attributes — get_attrs is correct here.
    #[allow(deprecated)]
    if tcx.get_attrs(def_id, rustc_span::sym::test).next().is_some()
        || tcx.get_attrs(def_id, rustc_span::sym::bench).next().is_some()
    {
        return false;
    }

    // Don't eliminate unsafe functions (can be called via transmuted fn pointers).
    if tcx.fn_sig(def_id).skip_binder().safety().is_unsafe() {
        return false;
    }

    // Don't eliminate generic functions (pre-monomorphization BFS is incomplete).
    if tcx.generics_of(def_id).count() > 0 {
        return false;
    }

    // Don't eliminate async functions (state machine transform runs after MIR).
    if tcx.asyncness(def_id).is_async() {
        return false;
    }

    // Don't eliminate functions whose signature contains fn pointers or dyn Trait.
    let sig = tcx.instantiate_bound_regions_with_erased(tcx.fn_sig(def_id).skip_binder());
    for ty in sig.inputs().iter().chain(std::iter::once(&sig.output())) {
        if contains_fn_ptr_or_dyn(*ty) {
            return false;
        }
    }

    true
}

fn contains_fn_ptr_or_dyn(ty: Ty<'_>) -> bool {
    match ty.kind() {
        ty::FnPtr(..) => true,
        ty::Dynamic(..) => true,
        ty::Ref(_, inner, _) | ty::RawPtr(inner, _) => contains_fn_ptr_or_dyn(*inner),
        _ => false,
    }
}

fn scan_for_vtable_constructions(body: &Body<'_>) {
    use rustc_middle::mir::{CastKind, Rvalue, StatementKind};
    use rustc_middle::ty::adjustment::PointerCoercion;
    for bb in body.basic_blocks.iter() {
        for stmt in &bb.statements {
            if let StatementKind::Assign(box (
                _,
                Rvalue::Cast(
                    CastKind::PointerCoercion(PointerCoercion::Unsize, _),
                    _,
                    target_ty,
                ),
            )) = &stmt.kind
            {
                record_dyn_traits(*target_ty);
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

//! Virtual slicing support - filter codegen without modifying source
//!
//! This module provides thread-safe storage for marked items and safety checks
//! for determining which items can be skipped during codegen.

#![cfg(feature = "rustc-driver")]

extern crate rustc_hir;
extern crate rustc_middle;
extern crate rustc_span;

use std::collections::{HashMap, HashSet};
use std::sync::{Mutex, OnceLock};
use std::sync::atomic::{AtomicUsize, Ordering};

use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;

/// Global pre-optimization call graph collected from mir_built() override.
/// Maps function path → set of called function paths.
/// This captures call edges BEFORE inlining, giving accurate reference data.
static PRE_OPT_CALL_GRAPH: OnceLock<Mutex<HashMap<String, HashSet<String>>>> = OnceLock::new();

/// Vtable constructions detected from mir_built() override.
/// Stores trait DefId values (as u64) for which ANY unsizing coercion was found.
/// If a trait appears here, its impl methods may be called via vtable dispatch.
/// If a trait does NOT appear here, all dispatch is static and BFS is reliable.
///
/// We track at trait granularity (not per-type) for safety: if ANY type is cast
/// to `dyn Trait`, we conservatively protect ALL impls of that trait. This avoids
/// the pre-monomorphization problem where generic unsizing (`&T as &dyn Trait`)
/// doesn't reveal the concrete type.
static VTABLE_TRAITS: OnceLock<Mutex<HashSet<u64>>> = OnceLock::new();

/// Initialize the pre-optimization call graph storage
pub fn init_call_graph() {
    PRE_OPT_CALL_GRAPH.get_or_init(|| Mutex::new(HashMap::new()));
    VTABLE_TRAITS.get_or_init(|| Mutex::new(HashSet::new()));
}

/// Record a call edge from caller to callee (discovered from mir_built)
pub fn record_call_edge(caller: String, callee: String) {
    if let Some(graph) = PRE_OPT_CALL_GRAPH.get() {
        if let Ok(mut g) = graph.lock() {
            g.entry(caller).or_default().insert(callee);
        }
    }
}

/// Get all call edges (for BFS traversal)
pub fn get_call_graph() -> Option<HashMap<String, HashSet<String>>> {
    PRE_OPT_CALL_GRAPH.get()
        .and_then(|g| g.lock().ok())
        .map(|g| g.clone())
}

/// Record that a trait has vtable constructions (unsizing coercion detected).
/// Uses the trait's DefId index as a u64 key for cross-function storage.
pub fn record_vtable_trait(trait_def_id_index: u64) {
    if let Some(set) = VTABLE_TRAITS.get() {
        if let Ok(mut s) = set.lock() {
            s.insert(trait_def_id_index);
        }
    }
}

/// Check if a trait has any vtable constructions detected.
/// Returns true if unsizing coercions to `dyn Trait` were found in mir_built.
/// Returns true (conservative) if vtable tracking is not initialized.
pub fn has_vtable_constructions(trait_def_id_index: u64) -> bool {
    match VTABLE_TRAITS.get() {
        Some(set) => {
            match set.lock() {
                Ok(s) => s.contains(&trait_def_id_index),
                Err(_) => true, // Lock poisoned, be conservative
            }
        }
        None => true, // Not initialized, be conservative
    }
}

/// Global counter for actually-stubbed functions (via optimized_mir override).
/// Used to write skip-driver markers: if stub_count == 0 after compilation,
/// the next incremental build can skip loading the driver for this crate.
static STUB_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Increment the stub counter (called from optimized_mir override)
pub fn increment_stub_count() {
    STUB_COUNT.fetch_add(1, Ordering::Relaxed);
}

/// Get the current stub count
pub fn get_stub_count() -> usize {
    STUB_COUNT.load(Ordering::Relaxed)
}

/// Get count of traits with vtable constructions (for debug logging)
pub fn vtable_trait_count() -> usize {
    VTABLE_TRAITS.get()
        .and_then(|s| s.lock().ok())
        .map(|s| s.len())
        .unwrap_or(0)
}

/// Information about marked items for virtual slicing
#[derive(Debug, Clone)]
pub struct MarkedItemsInfo {
    /// Set of item paths that are marked as "used" and should be codegen'd
    pub marked_paths: HashSet<String>,
    /// Whether virtual slicing is enabled
    pub enabled: bool,
}

impl Default for MarkedItemsInfo {
    fn default() -> Self {
        Self {
            marked_paths: HashSet::new(),
            enabled: false,
        }
    }
}

/// Global storage for marked items (thread-safe, set once per compilation)
static MARKED_ITEMS: OnceLock<MarkedItemsInfo> = OnceLock::new();

/// Store the marked items from BFS marking for the query override to access
///
/// This should be called from `after_analysis` before codegen begins.
/// Pre-computes suffix lookup sets for O(1) `is_item_marked()` queries.
/// Returns true if successfully stored, false if already set.
pub fn store_marked_items(items: HashSet<String>) -> bool {
    let info = MarkedItemsInfo {
        marked_paths: items,
        enabled: true,
    };
    MARKED_ITEMS.set(info).is_ok()
}

/// Check if an item path is marked as used
///
/// Returns true if:
/// - Virtual slicing is not enabled (conservative: keep everything)
/// - The item is in the marked set (exact match)
/// - No marked items have been stored yet (conservative: keep everything)
pub fn is_item_marked(path: &str) -> bool {
    match MARKED_ITEMS.get() {
        Some(info) if info.enabled => {
            info.marked_paths.contains(path)
        }
        _ => true, // Conservative: if not enabled or not set, keep everything
    }
}

/// Clear the marked items (for testing or re-compilation)
///
/// Note: OnceLock doesn't support clearing, so this is a no-op in production.
/// In tests, you'd need to use a different storage mechanism.
pub fn clear_marked_items() {
    // OnceLock can't be cleared; this is intentional for thread safety.
    // Each rustc invocation is a fresh process, so the static is reset.
}

/// Check if it's safe to skip codegen for an item
///
/// We must NOT skip codegen for:
/// - Public items (they may be used by downstream crates)
/// - Trait implementations (may be used via trait objects)
/// - Items with special attributes (#[no_mangle], #[export_name], #[used])
/// - FFI functions (extern "C")
/// - Drop implementations (compiler-generated calls)
/// - Items that are entry points (main, tests, etc.)
///
/// For dependency crates, we're very conservative (almost never skip).
/// For local crates, we can be more aggressive since we have full analysis data.
pub fn is_safe_to_skip<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    // 1. Never skip non-local items
    if !def_id.is_local() {
        return false;
    }

    // 2. Check def kind FIRST — some DefKinds (e.g. SyntheticCoroutineBody)
    // don't have a visibility provider and calling tcx.visibility() on them panics.
    let def_kind = tcx.def_kind(def_id);
    match def_kind {
        rustc_hir::def::DefKind::Fn => {
            // Standalone functions - continue with more checks
            // But never skip nested functions (defined inside another function).
            // Their call references are invisible after the parent is inlined.
            if let Some(parent) = tcx.opt_parent(def_id) {
                let parent_kind = tcx.def_kind(parent);
                if parent_kind.is_fn_like() {
                    return false;
                }
            }
        }
        rustc_hir::def::DefKind::AssocFn => {
            let assoc_item = tcx.associated_item(def_id);
            if assoc_item.trait_item_def_id().is_some() {
                // Trait impl method — check dispatch safety.
                let trait_method_def_id = assoc_item.trait_item_def_id().unwrap();
                let trait_def_id = tcx.parent(trait_method_def_id);

                if !tcx.is_dyn_compatible(trait_def_id) {
                    // Non-dyn-compatible: all dispatch is static, BFS is reliable.
                } else if !has_vtable_constructions(trait_def_id.index.as_u32() as u64) {
                    // Dyn-compatible BUT no unsizing coercions to `dyn Trait` found
                    // in any mir_built body. All dispatch is static in practice.
                } else {
                    return false; // Vtable exists → dispatch may be dynamic
                }
            }
            // Inherent method or safe-to-stub trait impl — continue checks
        }
        _ => return false,
    }

    // 3. Must be private (not pub) — UNLESS this is a binary crate.
    // In binary crates, pub items have no downstream consumers, so if they're
    // not reachable from main via BFS, they're dead code.
    let is_binary_crate = tcx.entry_fn(()).is_some();
    let vis = tcx.visibility(def_id);
    if vis.is_public() && !is_binary_crate {
        return false;
    }

    // Get def path for various checks below
    let def_path = tcx.def_path_str(def_id);

    // 4. Check if this is a dependency crate (very conservative)
    // We detect this by checking if the source is from .cargo/registry
    let is_dependency = is_likely_dependency_source(tcx, def_id);

    // 5. Check codegen-related attributes
    if def_kind.is_fn_like() {
        let attrs = tcx.codegen_fn_attrs(def_id);

        use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;

        let special_flags = CodegenFnAttrFlags::NO_MANGLE
            | CodegenFnAttrFlags::USED_LINKER;

        if attrs.flags.intersects(special_flags) {
            return false;
        }

        if attrs.symbol_name.is_some() {
            return false;
        }

        if attrs.linkage.is_some() {
            return false;
        }

        // Note: #[inline(never)] and #[inline(always)] are no longer blocked.
        // Our MIR-based scanning captures function-pointer usage (scanning all
        // constants for FnDef types). If neither mir_built nor optimized_mir
        // found a reference, the function is unreachable regardless of inline hints.
    }

    // 6. Check if this is a Drop implementation (by path or by trait)
    if def_path.contains("::drop") {
        return false;
    }

    // 6b. Don't skip compiler-generated init functions for statics/thread-locals
    if def_path.contains("__rust_std_internal") || def_path.contains("__init_fn") {
        return false;
    }

    // 7. Check if this is an entry point
    if tcx.entry_fn(()).map(|(id, _)| id) == Some(def_id) {
        return false;
    }

    // 8. Check for test/bench attributes
    for attr in tcx.get_attrs(def_id, rustc_span::symbol::sym::test) {
        let _ = attr;
        return false;
    }
    for attr in tcx.get_attrs(def_id, rustc_span::symbol::sym::bench) {
        let _ = attr;
        return false;
    }

    // 9. Check for unsafe functions (opt-in relaxation via CARGO_SLICER_RELAX_UNSAFE=1)
    // Unsafe functions are called like normal functions — their call edges are captured
    // by mir_built. The concern is hidden fn-ptr dispatch via transmute/raw-ptr casts.
    if def_kind.is_fn_like() {
        let relax_unsafe = std::env::var("CARGO_SLICER_RELAX_UNSAFE").map_or(false, |v| v == "1");
        if !relax_unsafe && tcx.fn_sig(def_id).skip_binder().safety().is_unsafe() {
            return false;
        }
    }

    // 10. Don't skip generic functions - monomorphization creates different call targets
    // that our pre-monomorphization MIR analysis can't see
    if def_kind.is_fn_like() {
        let generics = tcx.generics_of(def_id);
        if generics.count() > 0 {
            return false;
        }
    }

    // 11. Don't skip async functions - they are state machines with complex codegen
    if def_kind.is_fn_like() {
        if tcx.asyncness(def_id).is_async() {
            return false;
        }
    }

    // Note: const fn check (11b) removed — mir_built captures const fn calls before
    // evaluation, so our BFS-based marking is reliable for const fn.

    // Note: name-based heuristics (11c: __/callback/handler) removed — BFS-based
    // marking supersedes name patterns. If BFS didn't mark it, the name doesn't matter.

    // 12. Don't skip functions whose signature involves fn pointers or dyn Trait
    // These often participate in dynamic dispatch
    if def_kind.is_fn_like() {
        let sig = tcx.fn_sig(def_id).skip_binder();
        let sig = tcx.instantiate_bound_regions_with_erased(sig);
        for ty in sig.inputs().iter().chain(std::iter::once(&sig.output())) {
            if has_fn_ptr_or_dyn(*ty) {
                return false;
            }
        }
    }

    // 13. For dependencies only: extra conservative checks
    if is_dependency {
        let name = def_path.rsplit("::").next().unwrap_or("");
        // Don't skip functions with names suggesting indirect calls
        if name.contains("dispatch") ||
           name.contains("handler") ||
           name.contains("callback") ||
           name.contains("vtable") ||
           name.contains("detect") ||
           name.contains("find_") ||
           name.starts_with("_") {
            return false;
        }
        // Don't skip arch-specific functions (often use function pointers)
        if def_path.contains("::arch::") ||
           def_path.contains("::x86") ||
           def_path.contains("::aarch64") ||
           def_path.contains("::simd") {
            return false;
        }
    }

    // Passed all checks - safe to skip if not marked
    true
}

/// Diagnostic: return reason why an item can't be stubbed.
/// Only used for debug logging — mirrors is_safe_to_skip logic.
#[cfg(feature = "rustc-driver")]
pub fn skip_reason<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> &'static str {
    if !def_id.is_local() { return "non-local"; }

    let def_kind = tcx.def_kind(def_id);
    match def_kind {
        rustc_hir::def::DefKind::Fn => {
            if let Some(parent) = tcx.opt_parent(def_id) {
                if tcx.def_kind(parent).is_fn_like() {
                    return "nested-fn";
                }
            }
        }
        rustc_hir::def::DefKind::AssocFn => {
            let assoc_item = tcx.associated_item(def_id);
            if assoc_item.trait_item_def_id().is_some() {
                let trait_method_def_id = assoc_item.trait_item_def_id().unwrap();
                let trait_def_id = tcx.parent(trait_method_def_id);
                if tcx.is_dyn_compatible(trait_def_id) {
                    if has_vtable_constructions(trait_def_id.index.as_u32() as u64) {
                        return "vtable-constructed-trait-impl";
                    }
                    // dyn-compatible but no vtable found — allowed
                }
            }
        }
        _ => return "not-fn",
    }

    let is_binary_crate = tcx.entry_fn(()).is_some();
    let vis = tcx.visibility(def_id);
    if vis.is_public() && !is_binary_crate { return "pub-in-lib"; }

    if def_kind.is_fn_like() {
        let attrs = tcx.codegen_fn_attrs(def_id);
        use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
        let special_flags = CodegenFnAttrFlags::NO_MANGLE | CodegenFnAttrFlags::USED_LINKER;
        if attrs.flags.intersects(special_flags) { return "special-attr"; }
        if attrs.symbol_name.is_some() { return "symbol-name"; }
        if attrs.linkage.is_some() { return "linkage"; }
    }

    let def_path = tcx.def_path_str(def_id);
    if def_path.contains("::drop") { return "drop"; }

    if tcx.entry_fn(()).map(|(id, _)| id) == Some(def_id) { return "entry-point"; }

    if def_kind.is_fn_like() {
        if tcx.fn_sig(def_id).skip_binder().safety().is_unsafe() {
            return "unsafe";
        }
    }

    if def_kind.is_fn_like() {
        let generics = tcx.generics_of(def_id);
        if generics.count() > 0 { return "generic"; }
    }

    if def_kind.is_fn_like() {
        if tcx.asyncness(def_id).is_async() { return "async"; }
    }

    if def_kind.is_fn_like() {
        let sig = tcx.fn_sig(def_id).skip_binder();
        let sig = tcx.instantiate_bound_regions_with_erased(sig);
        for ty in sig.inputs().iter().chain(std::iter::once(&sig.output())) {
            if has_fn_ptr_or_dyn(*ty) { return "fn-ptr-or-dyn-in-sig"; }
        }
    }

    "unknown"
}

/// Check if a type contains function pointers or dyn Trait objects
/// (indicating potential dynamic dispatch that our analysis can't track)
fn has_fn_ptr_or_dyn(ty: rustc_middle::ty::Ty<'_>) -> bool {
    match ty.kind() {
        rustc_middle::ty::TyKind::FnPtr(..) => true,
        rustc_middle::ty::TyKind::Dynamic(..) => true,
        rustc_middle::ty::TyKind::Ref(_, inner, _) => has_fn_ptr_or_dyn(*inner),
        rustc_middle::ty::TyKind::RawPtr(inner, _) => has_fn_ptr_or_dyn(*inner),
        _ => false,
    }
}

/// Check if a def_id is likely from a dependency crate source
/// (as opposed to local project source)
fn is_likely_dependency_source<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    // Get the source file for this definition
    let span = tcx.def_span(def_id);
    let source_map = tcx.sess.source_map();

    if let Some(filename) = source_map.span_to_filename(span).into_local_path() {
        let path_str = filename.to_string_lossy();
        // Check for cargo registry paths
        if path_str.contains(".cargo/registry") || path_str.contains(".cargo/git") {
            return true;
        }
        // Check for vendored dependencies
        if path_str.contains("/vendor/") {
            return true;
        }
    }

    false
}

/// Check if an item is marked as used by looking up its def_id path.
///
/// This is used by the optimized_mir query override to decide whether to
/// replace MIR bodies with abort stubs.
pub fn is_item_marked_by_def_id<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    let path = tcx.def_path_str(def_id);
    is_item_marked(&path)
}

/// Determine if an item should be codegen'd based on marking and safety
///
/// Returns true if the item should be included in codegen.
pub fn should_codegen_item<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    let path = tcx.def_path_str(def_id);

    // If item is marked, always codegen it
    if is_item_marked(&path) {
        return true;
    }

    // If not marked but not safe to skip, still codegen it
    if !is_safe_to_skip(tcx, def_id) {
        return true;
    }

    // Not marked and safe to skip
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_item_marked_default() {
        // Before any items are stored, everything should be marked (conservative)
        assert!(is_item_marked("anything::goes"));
    }

    #[test]
    fn test_marked_items_info_default() {
        let info = MarkedItemsInfo::default();
        assert!(!info.enabled);
        assert!(info.marked_paths.is_empty());
    }
}

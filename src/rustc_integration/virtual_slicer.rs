//! Virtual slicing support - filter codegen without modifying source
//!
//! This module provides thread-safe storage for marked items and safety checks
//! for determining which items can be skipped during codegen.

#![cfg(feature = "rustc-driver")]

extern crate rustc_hir;
extern crate rustc_middle;
extern crate rustc_span;

use std::collections::{HashMap, HashSet};
use std::sync::{Mutex, RwLock, OnceLock};
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
static VTABLE_TRAITS: OnceLock<RwLock<HashSet<u64>>> = OnceLock::new();

/// Initialize the pre-optimization call graph storage
pub fn init_call_graph() {
    PRE_OPT_CALL_GRAPH.get_or_init(|| Mutex::new(HashMap::new()));
    VTABLE_TRAITS.get_or_init(|| RwLock::new(HashSet::new()));
}

/// Record a call edge from caller to callee (discovered from mir_built)
pub fn record_call_edge(caller: String, callee: String) {
    if let Some(graph) = PRE_OPT_CALL_GRAPH.get() {
        if let Ok(mut g) = graph.lock() {
            g.entry(caller).or_default().insert(callee);
        }
    }
}

/// Record multiple call edges from a single caller in one Mutex acquisition.
/// This is ~100x fewer lock operations than per-edge record_call_edge calls.
pub fn batch_record_call_edges(caller: String, callees: Vec<String>) {
    if let Some(graph) = PRE_OPT_CALL_GRAPH.get() {
        if let Ok(mut g) = graph.lock() {
            let entry = g.entry(caller).or_default();
            for callee in callees {
                entry.insert(callee);
            }
        }
    }
}

/// Get all call edges (for BFS traversal)
///
/// This takes ownership of the graph data (via mem::take) instead of cloning,
/// avoiding a full deep-clone of all strings. Safe because the graph is only
/// read once per compilation (in after_analysis).
pub fn take_call_graph() -> Option<HashMap<String, HashSet<String>>> {
    PRE_OPT_CALL_GRAPH.get()
        .and_then(|g| g.lock().ok())
        .map(|mut g| std::mem::take(&mut *g))
}

/// Record that a trait has vtable constructions (unsizing coercion detected).
/// Uses the trait's DefId index as a u64 key for cross-function storage.
pub fn record_vtable_trait(trait_def_id_index: u64) {
    if let Some(set) = VTABLE_TRAITS.get() {
        if let Ok(mut s) = set.write() {
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
            match set.read() {
                Ok(s) => s.contains(&trait_def_id_index),
                Err(_) => true, // Lock poisoned, be conservative
            }
        }
        None => true, // Not initialized, be conservative
    }
}

/// Global set of DefId indices that were actually stubbed by the optimized_mir override.
/// The collect_and_partition_mono_items override reads this to remove stubbed items
/// from CodegenUnits entirely, preventing LLVM from generating any IR for them.
static STUBBED_DEF_IDS: OnceLock<RwLock<HashSet<u64>>> = OnceLock::new();

/// Initialize the stubbed DefId set (call alongside init_call_graph)
pub fn init_stubbed_def_ids() {
    STUBBED_DEF_IDS.get_or_init(|| RwLock::new(HashSet::new()));
}

/// Record a DefId as stubbed (called from optimized_mir override)
pub fn record_stubbed_def_id(def_id_index: u64) {
    if let Some(set) = STUBBED_DEF_IDS.get() {
        if let Ok(mut s) = set.write() {
            s.insert(def_id_index);
        }
    }
}

/// Check if a DefId was stubbed by the optimized_mir override
pub fn is_def_id_stubbed(def_id_index: u64) -> bool {
    match STUBBED_DEF_IDS.get() {
        Some(set) => set.read().map_or(false, |s| s.contains(&def_id_index)),
        None => false,
    }
}

/// Get the count of stubbed DefIds
pub fn stubbed_def_id_count() -> usize {
    STUBBED_DEF_IDS.get()
        .and_then(|s| s.read().ok())
        .map(|s| s.len())
        .unwrap_or(0)
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
        .and_then(|s| s.read().ok())
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

/// Global storage for marked items (thread-safe, resettable for in-process daemon reuse).
/// Uses Mutex<Option<T>> instead of OnceLock so Windows named-pipe daemon can reset
/// between compilations (Unix fork() gives fresh statics, Windows in-process does not).
static MARKED_ITEMS: Mutex<Option<MarkedItemsInfo>> = Mutex::new(None);

/// Fast DefId-indexed lookup for the codegen hot path.
/// Built by `build_codegen_caches()` in a single pass over mir_keys.
static MARKED_DEF_IDS: Mutex<Option<HashSet<u64>>> = Mutex::new(None);

/// Pre-computed set of DefIds that are safe to skip (stubbable).
/// Built alongside MARKED_DEF_IDS by `build_codegen_caches()`, replacing
/// per-item `is_safe_to_skip()` calls during the hot `optimized_mir` path
/// with O(1) HashSet lookups.
static SAFE_TO_SKIP_DEF_IDS: Mutex<Option<HashSet<u64>>> = Mutex::new(None);

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
    if let Ok(mut lock) = MARKED_ITEMS.lock() {
        *lock = Some(info);
        true
    } else {
        false
    }
}

/// Check if an item path is marked as used
///
/// Returns true if:
/// - Virtual slicing is not enabled (conservative: keep everything)
/// - The item is in the marked set (exact match or normalized match)
/// - No marked items have been stored yet (conservative: keep everything)
///
/// When the marked set contains syn-format paths (from cross_crate_bfs cache),
/// trait impl paths in rustc format (`<Type as Trait>::method`) are also checked
/// against their normalized syn-format equivalent (`Type::method`).
pub fn is_item_marked(path: &str) -> bool {
    match MARKED_ITEMS.lock() {
        Ok(guard) => match guard.as_ref() {
            Some(info) if info.enabled => {
                if info.marked_paths.contains(path) {
                    return true;
                }
                // Normalize trait impl paths: "<Type as Trait>::method" → "Type::method"
                // This handles the syn-format vs rustc-format mismatch in cross-crate caches.
                if path.starts_with('<') {
                    if let Some(normalized) = normalize_trait_impl_path(path) {
                        return info.marked_paths.contains(&normalized);
                    }
                }
                false
            }
            _ => true, // Conservative: if not enabled or not set, keep everything
        },
        Err(_) => true, // Lock poisoned, be conservative
    }
}

/// Normalize a rustc-format trait impl path to syn-format.
///
/// `<Type as some::Trait>::method` → `Type::method`
fn normalize_trait_impl_path(path: &str) -> Option<String> {
    let inner = path.strip_prefix('<')?;
    let (type_part, rest) = inner.split_once(" as ")?;
    let (_trait_part, after_bracket) = rest.split_once('>')?;
    let method = after_bracket.strip_prefix("::")?;
    Some(format!("{}::{}", type_part, method))
}

/// Clear the marked items (for testing or re-compilation)
///
/// Note: OnceLock doesn't support clearing, so this is a no-op in production.
/// In tests, you'd need to use a different storage mechanism.
pub fn clear_marked_items() {
    if let Ok(mut lock) = MARKED_ITEMS.lock() {
        *lock = None;
    }
    if let Ok(mut lock) = MARKED_DEF_IDS.lock() {
        *lock = None;
    }
    if let Ok(mut lock) = SAFE_TO_SKIP_DEF_IDS.lock() {
        *lock = None;
    }
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
            // Standalone functions — continue with more checks.
            // Nested functions (defined inside another function) are also
            // eligible: our mir_built override captures call edges before
            // inlining, so parent→nested-fn edges are always visible in BFS.
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

    // 6. Check if this is a Drop::drop implementation (via lang item, not string matching).
    // The trait-impl case for Drop is already handled in the AssocFn match arm above
    // via trait_item_def_id + is_dyn_compatible. This catches standalone fns named "drop"
    // that are actually Drop impls, using the proper lang item.
    if def_kind == rustc_hir::def::DefKind::AssocFn {
        if let Some(drop_trait) = tcx.lang_items().drop_trait() {
            let assoc_item = tcx.associated_item(def_id);
            if let Some(trait_item_id) = assoc_item.trait_item_def_id() {
                if tcx.parent(trait_item_id) == drop_trait {
                    return false;
                }
            }
        }
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
        let def_path = tcx.def_path_str(def_id);
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
            // Nested functions are now eligible — mir_built captures
            // parent→nested-fn edges before inlining.
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

    if def_kind == rustc_hir::def::DefKind::AssocFn {
        if let Some(drop_trait) = tcx.lang_items().drop_trait() {
            let assoc_item = tcx.associated_item(def_id);
            if let Some(trait_item_id) = assoc_item.trait_item_def_id() {
                if tcx.parent(trait_item_id) == drop_trait {
                    return "drop-impl";
                }
            }
        }
    }

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
        // Normalize backslashes for Windows path compatibility
        let path_str = filename.to_string_lossy().replace('\\', "/");
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

/// Build a fast DefId-indexed cache from the marked paths.
///
/// Call this once after `store_marked_items()` with access to `tcx`.
/// Iterates all local DefIds, checks `is_item_marked(def_path_str(...))`,
/// and stores matching DefId indices in a HashSet for O(1) lookups.
pub fn build_marked_def_id_cache<'tcx>(tcx: TyCtxt<'tcx>) {
    let mut cache = HashSet::new();
    for &local_def_id in tcx.mir_keys(()) {
        let def_id = local_def_id.to_def_id();
        let path = tcx.def_path_str(def_id);
        if is_item_marked(&path) {
            cache.insert(def_id.index.as_u32() as u64);
        }
    }
    if let Ok(mut lock) = MARKED_DEF_IDS.lock() {
        *lock = Some(cache);
    }
}

/// Build both marked-DefId and safe-to-skip caches in a single pass over mir_keys.
///
/// Replaces separate `build_marked_def_id_cache()` + per-item `is_safe_to_skip()` calls.
/// After this, `is_item_marked_by_def_id()` and `is_safe_to_skip_by_def_id()` use O(1) lookups.
pub fn build_codegen_caches<'tcx>(tcx: TyCtxt<'tcx>) {
    let mut marked_cache = HashSet::new();
    let mut stubbable_cache = HashSet::new();
    for &local_def_id in tcx.mir_keys(()) {
        let def_id = local_def_id.to_def_id();
        let idx = def_id.index.as_u32() as u64;
        let path = tcx.def_path_str(def_id);
        if is_item_marked(&path) {
            marked_cache.insert(idx);
        } else if is_safe_to_skip(tcx, def_id) {
            stubbable_cache.insert(idx);
        }
    }
    if let Ok(mut lock) = MARKED_DEF_IDS.lock() {
        *lock = Some(marked_cache);
    }
    if let Ok(mut lock) = SAFE_TO_SKIP_DEF_IDS.lock() {
        *lock = Some(stubbable_cache);
    }
}

/// Check if an item is marked as used by looking up its DefId index.
///
/// This is used by the optimized_mir query override to decide whether to
/// replace MIR bodies with abort stubs.
/// Uses the pre-built DefId cache for O(1) lookups when available,
/// falling back to the path-based lookup.
pub fn is_item_marked_by_def_id<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    // Fast path: use pre-built DefId cache (avoids def_path_str allocation)
    if let Ok(guard) = MARKED_DEF_IDS.lock() {
        if let Some(cache) = guard.as_ref() {
            return cache.contains(&(def_id.index.as_u32() as u64));
        }
    }
    // Fallback: string-based lookup
    let path = tcx.def_path_str(def_id);
    is_item_marked(&path)
}

/// Check if an item is safe to skip by looking up its DefId index.
///
/// Uses the pre-built stubbable cache for O(1) lookups when available,
/// falling back to the full `is_safe_to_skip()` check.
pub fn is_safe_to_skip_by_def_id<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    if let Ok(guard) = SAFE_TO_SKIP_DEF_IDS.lock() {
        if let Some(cache) = guard.as_ref() {
            return cache.contains(&(def_id.index.as_u32() as u64));
        }
    }
    is_safe_to_skip(tcx, def_id)
}

/// Determine if an item should be codegen'd based on marking and safety
///
/// Returns true if the item should be included in codegen.
/// Uses pre-built caches when available for O(1) lookups.
pub fn should_codegen_item<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    // Fast path: use pre-built caches
    let marked_ok = MARKED_DEF_IDS.lock().ok().and_then(|g| g.as_ref().map(|_| ()));
    let skip_ok = SAFE_TO_SKIP_DEF_IDS.lock().ok().and_then(|g| g.as_ref().map(|_| ()));
    if marked_ok.is_some() && skip_ok.is_some() {
        let idx = def_id.index.as_u32() as u64;
        if let Ok(guard) = MARKED_DEF_IDS.lock() {
            if let Some(cache) = guard.as_ref() {
                if cache.contains(&idx) {
                    return true;
                }
            }
        }
        if let Ok(guard) = SAFE_TO_SKIP_DEF_IDS.lock() {
            if let Some(cache) = guard.as_ref() {
                if cache.contains(&idx) {
                    return false;
                }
            }
        }
        // Not in either cache — not a mir_key or not local, codegen it
        return true;
    }

    // Fallback: string-based lookup
    let path = tcx.def_path_str(def_id);

    if is_item_marked(&path) {
        return true;
    }

    if !is_safe_to_skip(tcx, def_id) {
        return true;
    }

    false
}

/// Reset all compilation state for in-process reuse (Windows named-pipe daemon).
///
/// On Unix, fork() gives each compilation fresh statics. On Windows, the daemon
/// runs compilations in-process sequentially, so statics persist and must be
/// explicitly reset between compilations.
pub fn reset_compilation_state() {
    // Clear OnceLock-wrapped collections (initialized once, contents cleared)
    if let Some(graph) = PRE_OPT_CALL_GRAPH.get() {
        if let Ok(mut g) = graph.lock() {
            g.clear();
        }
    }
    if let Some(set) = VTABLE_TRAITS.get() {
        if let Ok(mut s) = set.write() {
            s.clear();
        }
    }
    if let Some(set) = STUBBED_DEF_IDS.get() {
        if let Ok(mut s) = set.write() {
            s.clear();
        }
    }

    // Reset atomic counter
    STUB_COUNT.store(0, Ordering::Relaxed);

    // Clear Mutex<Option<T>> statics
    if let Ok(mut lock) = MARKED_ITEMS.lock() {
        *lock = None;
    }
    if let Ok(mut lock) = MARKED_DEF_IDS.lock() {
        *lock = None;
    }
    if let Ok(mut lock) = SAFE_TO_SKIP_DEF_IDS.lock() {
        *lock = None;
    }
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

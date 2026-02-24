//! MIR-precise whole-program analysis for cross-crate virtual slicing.
//!
//! When the binary crate compiles, all library crate `.rmeta` files are available.
//! This module reads MIR from external crates via `tcx.optimized_mir()` to build
//! a precise cross-crate call graph with full type information. This replaces the
//! approximate syn-based pre-analysis with compiler-precise analysis.
//!
//! Benefits over syn-based pre-analysis:
//! - Resolves all method calls (full type info)
//! - Sees macro-expanded code
//! - Tracks concrete generic instantiations
//! - Identifies all Drop impls precisely
//! - Enables stubbing of PUBLIC library functions (the biggest untapped opportunity)

#![cfg(feature = "rustc-driver")]

extern crate rustc_hir;
extern crate rustc_middle;
extern crate rustc_span;

use std::collections::{HashMap, HashSet, VecDeque};
use rustc_hir::def::DefKind;
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;

use super::debug_log::{debug_log, is_debug_enabled};

/// Result of MIR-precise whole-program analysis.
#[derive(Debug)]
pub struct MirAnalysisResult {
    /// Cross-crate call graph: caller_path → set of callee_paths
    pub call_graph: HashMap<String, HashSet<String>>,
    /// All function DefIds found in workspace crates (path → crate_name)
    pub extern_functions: HashMap<String, String>,
    /// Per-crate marked items from MIR-precise BFS
    pub crate_marked: HashMap<String, HashSet<String>>,
    /// Number of extern crates analyzed
    pub crates_analyzed: usize,
    /// Number of functions whose MIR was read
    pub functions_analyzed: usize,
}

/// Standard library crate prefixes to skip during MIR harvesting.
/// These crates are too large and their items are never stubbed.
const STD_CRATE_PREFIXES: &[&str] = &[
    "std", "core", "alloc", "proc_macro", "test",
    "compiler_builtins", "rustc_std_workspace",
    "panic_unwind", "panic_abort", "unwind",
    "hashbrown", "cfg_if", "unicode_ident",
    "libc", "getrandom", "memchr",
];

/// Harvest MIR from all workspace extern crates and build a precise call graph.
///
/// Called from the binary crate's `after_analysis` callback. At this point,
/// all library crate `.rmeta` files are loaded and their MIR is accessible
/// via `tcx.optimized_mir()`.
///
/// The `workspace_crates` parameter lists crate names that are part of the
/// workspace (as opposed to external dependencies from crates.io).
pub fn harvest_extern_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    workspace_crates: &HashSet<String>,
) -> MirAnalysisResult {
    let debug = is_debug_enabled();
    let mut call_graph: HashMap<String, HashSet<String>> = HashMap::new();
    let mut extern_functions: HashMap<String, String> = HashMap::new();
    let mut crates_analyzed = 0usize;
    let mut functions_analyzed = 0usize;

    if debug {
        debug_log(&format!("[mir-harvest] Starting MIR-precise analysis, workspace crates: {:?}",
            workspace_crates));
    }

    // Iterate all external crates
    for &crate_num in tcx.crates(()) {
        let crate_name = tcx.crate_name(crate_num).to_string();

        // Skip standard library and common non-workspace crates
        if STD_CRATE_PREFIXES.iter().any(|p| crate_name == *p || crate_name.starts_with(&format!("{}_", p))) {
            continue;
        }

        // Only analyze workspace crates (if list is provided)
        if !workspace_crates.is_empty() && !workspace_crates.contains(&crate_name) {
            if debug {
                debug_log(&format!("[mir-harvest] Skipping non-workspace crate: {}", crate_name));
            }
            continue;
        }

        if debug {
            debug_log(&format!("[mir-harvest] Analyzing extern crate: {}", crate_name));
        }

        crates_analyzed += 1;

        // BFS through the crate's module tree to find all functions
        let crate_root = crate_num.as_def_id();
        let mut module_queue: VecDeque<DefId> = VecDeque::new();
        let mut visited: HashSet<DefId> = HashSet::new();
        module_queue.push_back(crate_root);
        visited.insert(crate_root);

        while let Some(module_def_id) = module_queue.pop_front() {
            // module_children may panic on non-module items; guard with def_kind check
            let def_kind = tcx.def_kind(module_def_id);
            if !matches!(def_kind, DefKind::Mod) && module_def_id != crate_root {
                continue;
            }

            let children = tcx.module_children(module_def_id);
            for child in children.iter() {
                let Some(child_def_id) = child.res.opt_def_id() else {
                    continue;
                };

                if !visited.insert(child_def_id) {
                    continue;
                }

                // Only process items from the same crate
                if child_def_id.krate != crate_num {
                    continue;
                }

                let child_kind = tcx.def_kind(child_def_id);
                match child_kind {
                    DefKind::Mod => {
                        module_queue.push_back(child_def_id);
                    }
                    DefKind::Fn | DefKind::AssocFn => {
                        let path = tcx.def_path_str(child_def_id);
                        extern_functions.insert(path.clone(), crate_name.clone());

                        // Read and analyze MIR
                        if let Some(edges) = analyze_function_mir(tcx, child_def_id, &path) {
                            if !edges.is_empty() {
                                call_graph.entry(path).or_default().extend(edges);
                            }
                            functions_analyzed += 1;
                        }
                    }
                    DefKind::Struct | DefKind::Enum | DefKind::Union | DefKind::Trait => {
                        // These can have impl blocks — check associated items
                        analyze_associated_items(
                            tcx, child_def_id, &crate_name,
                            &mut call_graph, &mut extern_functions,
                            &mut functions_analyzed,
                        );
                    }
                    _ => {}
                }
            }
        }
    }

    // Also analyze the local (binary) crate's call edges into extern crates
    analyze_local_mir(tcx, &mut call_graph, &mut functions_analyzed);

    if debug {
        let total_edges: usize = call_graph.values().map(|s| s.len()).sum();
        debug_log(&format!("[mir-harvest] Complete: {} crates, {} functions, {} callers, {} edges",
            crates_analyzed, functions_analyzed, call_graph.len(), total_edges));
    }

    // Compute MIR-precise marked items per crate
    let crate_marked = compute_mir_precise_marked(
        tcx, &call_graph, &extern_functions, workspace_crates,
    );

    MirAnalysisResult {
        call_graph,
        extern_functions,
        crate_marked,
        crates_analyzed,
        functions_analyzed,
    }
}

/// Analyze a single function's MIR to extract call edges.
fn analyze_function_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    caller_path: &str,
) -> Option<HashSet<String>> {
    // Guard against items that don't have MIR (e.g., foreign functions, intrinsics)
    let def_kind = tcx.def_kind(def_id);
    if !matches!(def_kind, DefKind::Fn | DefKind::AssocFn) {
        return None;
    }

    // Check if MIR is available (some items don't have it)
    if !tcx.is_mir_available(def_id) {
        return None;
    }

    // Read optimized MIR (this works for external DefIds via .rmeta)
    let body = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        tcx.optimized_mir(def_id)
    })) {
        Ok(body) => body,
        Err(_) => {
            if is_debug_enabled() {
                debug_log(&format!("[mir-harvest] WARNING: panic reading MIR for {}", caller_path));
            }
            return None;
        }
    };

    let mut edges = HashSet::new();

    // Helper: extract callee path from FnDef type
    let extract_fndef = |ty: rustc_middle::ty::Ty<'tcx>| -> Option<String> {
        if let rustc_middle::ty::TyKind::FnDef(callee_id, _) = ty.kind() {
            let callee = tcx.def_path_str(*callee_id);
            // Skip std/core/alloc — we never stub these
            if !callee.starts_with("std::") && !callee.starts_with("core::") && !callee.starts_with("alloc::") {
                return Some(callee);
            }
        }
        None
    };

    // Walk MIR body: scan all basic blocks for call edges
    for bb in body.basic_blocks.iter() {
        // Scan statements for function references (fn items as constants)
        for stmt in &bb.statements {
            if let rustc_middle::mir::StatementKind::Assign(box (_, ref rvalue)) = stmt.kind {
                match rvalue {
                    rustc_middle::mir::Rvalue::Use(rustc_middle::mir::Operand::Constant(c)) |
                    rustc_middle::mir::Rvalue::Cast(_, rustc_middle::mir::Operand::Constant(c), _) |
                    rustc_middle::mir::Rvalue::UnaryOp(_, rustc_middle::mir::Operand::Constant(c)) |
                    rustc_middle::mir::Rvalue::Repeat(rustc_middle::mir::Operand::Constant(c), _) => {
                        let ty = match c.const_ {
                            rustc_middle::mir::Const::Val(_, ty) => ty,
                            rustc_middle::mir::Const::Ty(ty, _) => ty,
                            rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                        };
                        if let Some(callee) = extract_fndef(ty) {
                            edges.insert(callee);
                        }
                    }
                    _ => {}
                }
            }
        }

        // Scan terminators for call targets and arguments
        if let Some(ref term) = bb.terminator {
            if let rustc_middle::mir::TerminatorKind::Call { ref func, ref args, .. } = term.kind {
                // Direct call target
                if let rustc_middle::mir::Operand::Constant(constant) = func {
                    let ty = match constant.const_ {
                        rustc_middle::mir::Const::Val(_, ty) => ty,
                        rustc_middle::mir::Const::Ty(ty, _) => ty,
                        rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                    };
                    if let Some(callee) = extract_fndef(ty) {
                        edges.insert(callee);
                    }
                }
                // Function items passed as arguments
                for arg in args {
                    if let rustc_middle::mir::Operand::Constant(c) = &arg.node {
                        let ty = match c.const_ {
                            rustc_middle::mir::Const::Val(_, ty) => ty,
                            rustc_middle::mir::Const::Ty(ty, _) => ty,
                            rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                        };
                        if let Some(callee) = extract_fndef(ty) {
                            edges.insert(callee);
                        }
                    }
                }
            }
        }
    }

    Some(edges)
}

/// Analyze associated items (methods) of a struct/enum/trait.
fn analyze_associated_items<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    crate_name: &str,
    call_graph: &mut HashMap<String, HashSet<String>>,
    extern_functions: &mut HashMap<String, String>,
    functions_analyzed: &mut usize,
) {
    // Get all impl blocks for this type
    let impls = tcx.inherent_impls(def_id);
    for &impl_def_id in impls.iter() {
        let items = tcx.associated_item_def_ids(impl_def_id);
        for &item_def_id in items {
            let item_kind = tcx.def_kind(item_def_id);
            if matches!(item_kind, DefKind::AssocFn) {
                let path = tcx.def_path_str(item_def_id);
                extern_functions.insert(path.clone(), crate_name.to_string());

                if let Some(edges) = analyze_function_mir(tcx, item_def_id, &path) {
                    if !edges.is_empty() {
                        call_graph.entry(path).or_default().extend(edges);
                    }
                    *functions_analyzed += 1;
                }
            }
        }
    }
}

/// Analyze the local (binary) crate's MIR for call edges into extern crates.
fn analyze_local_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    call_graph: &mut HashMap<String, HashSet<String>>,
    functions_analyzed: &mut usize,
) {
    use rustc_span::def_id::LOCAL_CRATE;

    let hir = tcx.hir_crate_items(());
    for def_id in hir.definitions() {
        let def_kind = tcx.def_kind(def_id.to_def_id());
        if !matches!(def_kind, DefKind::Fn | DefKind::AssocFn) {
            continue;
        }

        let full_def_id = def_id.to_def_id();
        let path = tcx.def_path_str(full_def_id);

        if let Some(edges) = analyze_function_mir(tcx, full_def_id, &path) {
            if !edges.is_empty() {
                call_graph.entry(path).or_default().extend(edges);
            }
            *functions_analyzed += 1;
        }
    }

    if is_debug_enabled() {
        let crate_name = tcx.crate_name(LOCAL_CRATE).to_string();
        debug_log(&format!("[mir-harvest] Analyzed local crate: {}", crate_name));
    }
}

/// Compute MIR-precise marked items per workspace crate.
///
/// Runs BFS from binary crate entry points over the MIR-precise call graph
/// and writes per-crate marked item sets.
fn compute_mir_precise_marked<'tcx>(
    tcx: TyCtxt<'tcx>,
    call_graph: &HashMap<String, HashSet<String>>,
    extern_functions: &HashMap<String, String>,
    _workspace_crates: &HashSet<String>,
) -> HashMap<String, HashSet<String>> {
    use rustc_span::def_id::LOCAL_CRATE;

    // Compute seeds from binary crate
    let mut seeds: HashSet<String> = HashSet::new();

    // Main entry point
    if let Some((entry_id, _)) = tcx.entry_fn(()) {
        let path = tcx.def_path_str(entry_id);
        seeds.insert(path);
    }

    // Also seed #[test], #[no_mangle], statics from binary crate
    let hir = tcx.hir_crate_items(());
    for def_id in hir.definitions() {
        let full_def_id = def_id.to_def_id();
        let def_kind = tcx.def_kind(full_def_id);

        // Statics
        if matches!(def_kind, DefKind::Static { .. }) {
            seeds.insert(tcx.def_path_str(full_def_id));
        }

        // #[no_mangle]
        if def_kind.is_fn_like() {
            let attrs = tcx.codegen_fn_attrs(full_def_id);
            use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
            if attrs.flags.contains(CodegenFnAttrFlags::NO_MANGLE) {
                seeds.insert(tcx.def_path_str(full_def_id));
            }
        }
    }

    if is_debug_enabled() {
        debug_log(&format!("[mir-harvest] BFS seeds: {} from binary crate", seeds.len()));
    }

    // BFS from seeds over the MIR-precise call graph
    let mut marked: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = seeds.into_iter().collect();

    while let Some(item) = queue.pop_front() {
        if !marked.insert(item.clone()) {
            continue;
        }
        if let Some(callees) = call_graph.get(&item) {
            for callee in callees {
                if !marked.contains(callee) {
                    queue.push_back(callee.clone());
                }
            }
        }
    }

    if is_debug_enabled() {
        debug_log(&format!("[mir-harvest] MIR-precise BFS marked {} items total", marked.len()));
    }

    // Partition marked items by crate
    let mut crate_marked: HashMap<String, HashSet<String>> = HashMap::new();

    for item in &marked {
        if let Some(crate_name) = extern_functions.get(item) {
            crate_marked.entry(crate_name.clone()).or_default().insert(item.clone());
        }
    }

    // Also add the local binary crate's marked items
    let local_name = tcx.crate_name(LOCAL_CRATE).to_string();
    let local_marked: HashSet<String> = marked.iter()
        .filter(|item| !extern_functions.contains_key(*item))
        .cloned()
        .collect();
    if !local_marked.is_empty() {
        crate_marked.insert(local_name, local_marked);
    }

    if is_debug_enabled() {
        for (crate_name, items) in &crate_marked {
            debug_log(&format!("[mir-harvest] Crate {}: {} MIR-precise marked items", crate_name, items.len()));
        }
    }

    crate_marked
}

/// Write MIR-precise cache files for use by the lazy-codegen phase.
///
/// For each workspace crate, writes a `.mir-cache` file containing the
/// MIR-precise marked items. The driver's lazy-codegen phase reads these
/// instead of the syn-based `.cache` files.
pub fn write_mir_precise_caches(
    result: &MirAnalysisResult,
    cache_dir: &std::path::Path,
) -> Result<(), String> {
    let _ = std::fs::create_dir_all(cache_dir);

    for (crate_name, marked_items) in &result.crate_marked {
        let cache_path = cache_dir.join(format!("{}.mir-cache", crate_name));
        let mut content = String::new();
        content.push_str("mir-precise\n"); // Header marker
        for item in marked_items {
            content.push_str(item);
            content.push('\n');
        }

        std::fs::write(&cache_path, content)
            .map_err(|e| format!("Failed to write MIR cache for {}: {}", crate_name, e))?;
    }

    if is_debug_enabled() {
        debug_log(&format!("[mir-harvest] Wrote {} MIR-precise cache files", result.crate_marked.len()));
    }

    Ok(())
}

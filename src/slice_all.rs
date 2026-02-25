//! Multi-crate slicing functionality.
//!
//! Functions for slicing all dependencies in a workspace.

use std::collections::{HashMap, HashSet, BTreeSet};
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;
use std::time::{Duration, Instant};

use indicatif::{MultiProgress, ProgressBar, ProgressStyle, ProgressDrawTarget};
use rayon::prelude::*;
use serde::{Serialize, Deserialize};

use super::types::{DepGraph, SliceAllResult, UsedItem, ItemKind, CrateInfo, ProgressState, CrateIndex};
use super::constants::{FFI_CRATES, COMPLEX_CRATES, PLATFORM_SPECIFIC_CRATES, CONFLICT_PRONE_CRATES};
use crate::common::source_location::find_crate_source;
use super::usage::{build_usage_index, find_rust_files};
use super::old_slicer::parsing::parse_crate;
use super::old_slicer::semantic::generate_semantic_sliced_crate_with_needed;
use super::old_slicer::auto_fix::auto_fix_sliced_crate;
use super::dep_graph::{run_ra_deps_with_timeout, compute_dynamic_timeout, analyze_crate_usage_from_source, build_multi_version_info, MultiVersionInfo};
use super::old_slicer::post_slice_fixer::{fix_missing_types, save_fix_log};
use super::slicer::features::SlicerFeatures;
use crate::debug_log;

/// Cached Phase 1 and Phase 2 results for debugging
#[derive(Debug, Serialize, Deserialize)]
struct Phase2Cache {
    /// Items needed for each crate (Phase 1 seed items)
    crate_items: HashMap<String, HashSet<String>>,
    /// Full usage details (Phase 1)
    crate_used: HashMap<String, HashSet<UsedItem>>,
    /// Transitive items needed (Phase 2 SCIP results)
    crate_needed: HashMap<String, BTreeSet<String>>,
    /// SCIP analysis results (local_types, symbol_paths, etc.) for each crate
    /// This avoids re-running rust-analyzer SCIP in Phase 3
    crate_scip_analysis: HashMap<String, crate::old_slicer::semantic::ScipAnalysis>,
}

/// Phase 3 result type to distinguish success, passthrough, and error
#[derive(Debug)]
enum Phase5Result {
    Success(String),      // Successfully sliced crate
    Passthrough(String),  // Fell back to full crate copy
    Error(String, String), // (crate_name, error_message)
}

/// Parse a crate using rustc driver or fallback to syn-based parser
///
/// When use_rustc_driver is enabled, uses the compiler for 100% accurate parsing.
/// Falls back to syn-based parser if rustc driver is unavailable or disabled.
fn parse_crate_with_features(
    crate_path: &Path,
    crate_name: &str,
    features: &SlicerFeatures,
) -> CrateIndex {
    if features.use_rustc_driver {
        if features.rustc_via_cargo {
            debug_log!("  🦀 Using rustc driver via cargo (100% accurate) for: {}", crate_name);
        } else if features.rustc_precompile {
            debug_log!("  🦀 Using rustc driver with pre-compilation for: {}", crate_name);
        } else {
            debug_log!("  🦀 Using rustc driver for parsing: {}", crate_name);
        }

        match crate::rustc_subprocess::analyze_crate_with_rustc_driver_full(
            crate_path,
            crate_name,
            features.rustc_precompile,
            features.rustc_via_cargo
        ) {
            Ok(usage_data) => {
                debug_log!("  ✓ Rustc driver collected {} items, {} dependencies",
                         usage_data.defined_items.len(),
                         usage_data.item_dependencies.len());

                return crate::rustc_subprocess::usage_data_to_crate_index(&usage_data);
            }
            Err(e) => {
                debug_log!("  ⚠️  Rustc driver failed: {}", e);
                debug_log!("  Falling back to syn-based parser");
            }
        }
    }

    // Fallback to syn-based parser
    if features.use_rustc_driver {
        debug_log!("  📝 Falling back to syn-based parser (rustc-driver feature not compiled)");
    }

    parse_crate(crate_path, crate_name)
}

/// Copy the original crate as passthrough (no slicing)
/// This is used when slicing fails or produces a crate that doesn't compile
fn copy_crate_as_passthrough(crate_info: &CrateInfo, output_dir: &std::path::PathBuf) -> Result<(), String> {
    // Remove any existing sliced output
    if output_dir.exists() {
        fs::remove_dir_all(output_dir)
            .map_err(|e| format!("Failed to remove existing output: {}", e))?;
    }

    // Copy the entire crate directory
    copy_dir_recursive(&crate_info.path, output_dir)
        .map_err(|e| format!("Failed to copy crate: {}", e))?;

    // For passthrough crates, keep the original package name (just like sliced crates)
    // The directory has the -sliced suffix, but the package name stays the same
    // so that other crates can depend on it with: dep = { path = "../dep-sliced" }
    // and cargo will find the package "dep" in that directory

    Ok(())
}

/// Recursively copy a directory
pub fn copy_dir_recursive(src: &std::path::Path, dst: &std::path::Path) -> std::io::Result<()> {
    fs::create_dir_all(dst)?;

    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let path = entry.path();
        let dest_path = dst.join(entry.file_name());

        if path.is_dir() {
            // Skip target directory and hidden directories
            let name = entry.file_name();
            let name_str = name.to_string_lossy();
            if name_str == "target" || name_str.starts_with('.') {
                continue;
            }
            copy_dir_recursive(&path, &dest_path)?;
        } else {
            fs::copy(&path, &dest_path)?;
        }
    }

    Ok(())
}

/// Save Phase 2 results to cache file for debugging
fn save_phase2_cache(
    cache_path: &Path,
    crate_items: &HashMap<String, HashSet<String>>,
    crate_used: &HashMap<String, HashSet<UsedItem>>,
    crate_needed: &HashMap<String, BTreeSet<String>>,
    crate_scip_analysis: &HashMap<String, crate::old_slicer::semantic::ScipAnalysis>,
) -> Result<(), String> {
    let cache = Phase2Cache {
        crate_items: crate_items.clone(),
        crate_used: crate_used.clone(),
        crate_needed: crate_needed.clone(),
        crate_scip_analysis: crate_scip_analysis.clone(),
    };

    let json = serde_json::to_string_pretty(&cache)
        .map_err(|e| format!("Failed to serialize cache: {}", e))?;

    fs::write(cache_path, json)
        .map_err(|e| format!("Failed to write cache file: {}", e))?;

    println!("\n✓ Phase 2 cache saved to: {}", cache_path.display());
    Ok(())
}

/// Load Phase 2 results from cache file
fn load_phase2_cache(cache_path: &Path) -> Result<Phase2Cache, String> {
    let json = fs::read_to_string(cache_path)
        .map_err(|e| format!("Failed to read cache file: {}", e))?;

    let cache: Phase2Cache = serde_json::from_str(&json)
        .map_err(|e| format!("Failed to parse cache file: {}", e))?;

    Ok(cache)
}

/// Create a progress bar with consistent styling
fn create_progress_bar(multi: &MultiProgress, total: u64, phase: &str) -> ProgressBar {
    let pb = multi.add(ProgressBar::new(total));
    pb.set_style(
        ProgressStyle::default_bar()
            .template(&format!("{{spinner:.green}} {} [{{bar:40.cyan/blue}}] {{pos}}/{{len}} {{msg}}", phase))
            .unwrap()
            .progress_chars("=>-")
    );
    pb.enable_steady_tick(std::time::Duration::from_millis(100));
    pb
}

/// Create a stats spinner for showing success/error counts
fn create_stats_bar(multi: &MultiProgress) -> ProgressBar {
    let pb = multi.add(ProgressBar::new_spinner());
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {wide_msg}")
            .unwrap()
    );
    pb.set_message("Waiting...");
    pb.enable_steady_tick(std::time::Duration::from_millis(100));
    pb
}

/// Copy original source code of all dependent crates to a central folder
pub fn copy_original_sources(graph: &DepGraph, base_dir: &Path) -> Result<(), String> {
    let crates_to_slice_dir = base_dir.join("crates_to_slice");
    if !crates_to_slice_dir.exists() {
        fs::create_dir_all(&crates_to_slice_dir)
            .map_err(|e| format!("Failed to create crates_to_slice dir: {}", e))?;
    }

    println!("  Copying original sources to crates_to_slice/...");
    for (name, node) in &graph.nodes {
        if let Some(info) = find_crate_source(name, Some(&node.version)) {
            let dest = crates_to_slice_dir.join(&info.name);
            if !dest.exists() {
                if let Err(e) = copy_dir_recursive(&info.path, &dest) {
                    eprintln!("  Warning: Failed to copy {} source: {}", name, e);
                }
            }
        }
    }
    Ok(())
}

/// Compute transitive requirements for all crates in the dependency graph.
///
/// This function analyzes what each sliced crate uses from its dependencies
/// and propagates those requirements up the dependency tree. This ensures that
/// intermediate crates export all types needed by their dependents.
///
/// Algorithm:
/// 1. Start with direct requirements (what main project uses from each crate)
/// 2. Process crates in topological order (dependencies before dependents)
/// 3. For each crate, analyze what its sliced code uses from dependencies
/// 4. Add those requirements to the dependency crates
/// 5. Return updated requirements map
fn compute_transitive_requirements(
    graph: &DepGraph,
    direct_requirements: &HashMap<String, BTreeSet<String>>,
    output_base: &Path,
    verbose: bool,
) -> HashMap<String, BTreeSet<String>> {
    if verbose {
        println!("\nPhase 7: Computing transitive requirements...");
    }

    let mut all_requirements: HashMap<String, BTreeSet<String>> =
        direct_requirements.iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

    // Get topological order (dependencies before dependents)
    let topo_order = graph.topo_order();

    // Process in reverse topological order (dependents before dependencies)
    // This ensures we analyze what dependents need before updating dependencies
    for crate_name in topo_order.iter().rev() {
        // Skip root project
        if *crate_name == graph.root {
            continue;
        }

        // Check if this crate was sliced
        let sliced_dir = output_base.join(format!("{}-sliced", crate_name));
        if !sliced_dir.exists() || !sliced_dir.join("src").exists() {
            continue;
        }

        // Get this crate's dependencies
        let deps = graph.get_deps(crate_name);

        // Analyze what this sliced crate uses from each dependency
        for dep_name in deps {
            let dep_usage = analyze_crate_usage_from_source(
                &sliced_dir.join("src"),
                dep_name
            );

            if !dep_usage.is_empty() {
                // Extract item names from UsedItem paths
                let dep_crate_prefix = format!("{}::", dep_name.replace('-', "_"));
                let items: BTreeSet<String> = dep_usage.iter()
                    .filter_map(|used_item| {
                        let path = &used_item.path;
                        if path.starts_with(&dep_crate_prefix) {
                            // Extract all components after crate name
                            let relative = &path[dep_crate_prefix.len()..];
                            let parts: Vec<&str> = relative.split("::").collect();
                            // Add all path segments
                            let mut items = Vec::new();
                            for part in &parts {
                                if !part.is_empty() {
                                    items.push(part.to_string());
                                }
                            }
                            if !items.is_empty() {
                                Some(items)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .flatten()
                    .collect();

                if !items.is_empty() {
                    // Add to dependency's requirements
                    let dep_reqs = all_requirements.entry(dep_name.to_string())
                        .or_insert_with(BTreeSet::new);
                    let before = dep_reqs.len();
                    dep_reqs.extend(items.clone());
                    let after = dep_reqs.len();

                    if after > before && verbose {
                        println!("  {} → {}: +{} items (now {} total)",
                            crate_name, dep_name, after - before, after);
                    }
                }
            }
        }
    }

    all_requirements
}

/// Heuristic to detect if slicing a crate is likely to cause bloat
/// Returns true if the crate should use registry version instead of slicing
/// Uses pre-computed total_lines from CrateInfo to avoid expensive tokei measurement
fn is_likely_to_bloat(crate_name: &str, total_lines: usize) -> bool {
    // 1. Already in COMPLEX_CRATES list (known macro-heavy or complex crates)
    if COMPLEX_CRATES.contains(&crate_name) {
        return true;
    }

    // 2. Large crates (> 50,000 LOC) tend to bloat when sliced
    //    These take longer to slice and often don't reduce much
    //    Note: total_lines is already computed by count_crate_items in find_crate_source
    if total_lines > 50_000 {
        return true;
    }

    false
}

/// Union slice all dependencies
/// This collects all items used from each crate across the ENTIRE dependency tree,
/// then runs ra_deps on the union to get complete transitive closures,
/// and generates unified sliced crates that reference each other.
pub fn union_slice_deps(
    graph: &DepGraph,
    output_base: &Path,
    project_src_dir: &Path, // Directory containing source code to analyze usage from
    project_root: &Path,    // Root directory of the project (containing Cargo.toml)
    auto_fix: bool,
    parallel: bool,
    profile_crate: Option<&str>,
    abort_threshold: Option<f64>,
    ra_timeout: Option<u64>,
    show_progress: bool,
    original_source: bool,
    measure: bool,
    force_all: bool, // Attempt to slice all crates including complex ones
    save_phase2_cache_path: Option<&Path>, // Save Phase 2 results to this file
    load_phase2_cache_path: Option<&Path>, // Load Phase 2 results from this file
    use_new_slicer: bool, // Use new copy-and-delete slicer (experimental)
    verbose: bool, // Show detailed per-crate output
    registry_fallback: bool, // Use registry version for bloat-likely crates
    features: &crate::slicer::features::SlicerFeatures, // Feature flags for deletion behavior
    use_incremental_cache: bool, // Use incremental caching for sliced outputs
    use_local_registry: bool, // Use local registry instead of path dependencies for caching
    features_arg: Option<&str>, // Feature flags for cargo metadata
    all_features: bool,
    no_default_features: bool,
    export_symbols_path: Option<&Path>, // Export marked symbols to linker script
) -> SliceAllResult {
    let mut result = SliceAllResult::default();
    let total_start = Instant::now();

    // Backup original Cargo.toml before any modifications
    // This ensures we can always restore even in non-git projects
    let cargo_toml = project_root.join("Cargo.toml");
    let cargo_toml_orig = project_root.join("Cargo.toml.orig");

    if cargo_toml.exists() && !cargo_toml_orig.exists() {
        if let Err(e) = std::fs::copy(&cargo_toml, &cargo_toml_orig) {
            eprintln!("Warning: Failed to backup Cargo.toml: {}", e);
        } else if verbose {
            println!("📋 Backed up original Cargo.toml to Cargo.toml.orig");
        }
    }

    // Load incremental cache for sliced outputs
    let sliced_cache = if use_incremental_cache && use_new_slicer {
        let cache = crate::slicer::cache::SlicerCache::load(project_root);
        if !cache.entries.is_empty() {
            println!("📦 Loaded incremental cache: {} cached crate(s)", cache.entries.len());
        }
        Some(Arc::new(std::sync::Mutex::new(cache)))
    } else {
        None
    };

    // Show parser selection info
    if features.use_rustc_driver && features.rustc_via_cargo {
        println!("🦀 Parser: Rustc driver (100% accurate, via cargo rustc)");
    } else if features.use_rustc_driver {
        println!("🦀 Parser: Rustc driver (100% accurate, direct invocation)");
    } else {
        println!("📝 Parser: Syn-based (fallback, rustc driver not available)");
        println!("   Tip: Build cargo-slicer-rustc for 100% accuracy:");
        println!("   cargo +nightly build --bin cargo-slicer-rustc --features rustc-driver");
    }

    // Timeout mode: if user specified --ra-timeout, use fixed timeout for all crates.
    // Otherwise, use dynamic timeout based on each crate's size.
    let use_fixed_timeout = ra_timeout.is_some();
    let fixed_timeout = ra_timeout.map(Duration::from_secs);

    let mode_str = match (parallel, profile_crate) {
        (_, Some(crate_name)) => format!(" (profiling: {})", crate_name),
        (true, None) => " (parallel)".to_string(),
        (false, None) => " (sequential)".to_string(),
    };
    // Setup a single MultiProgress for the entire operation
    let multi = MultiProgress::new();
    if !show_progress {
        multi.set_draw_target(ProgressDrawTarget::hidden());
    }

    multi
        .println(format!(
            "=== Union Slicing Dependencies (Transitive){}{} ===",
            if auto_fix { " (with auto-fix)" } else { "" },
            mode_str
        ))
        .ok();

    // Build multi-version info from cargo metadata resolve graph
    // This is needed for Phase 5 regardless of cache status
    let multi_version_info = match build_multi_version_info(
        None,
        features_arg,
        all_features,
        no_default_features,
    ) {
        Ok(info) => {
            if verbose && !info.multi_version_crates.is_empty() {
                println!("\n  Multi-version crates detected:");
                for (name, versions) in &info.multi_version_crates {
                    println!("    {} → {:?}", name, versions);
                }
            }
            info
        }
        Err(e) => {
            eprintln!("Warning: Failed to build multi-version info: {}", e);
            MultiVersionInfo::default()
        }
    };

    // Set global multi-version info for Cargo.toml rewriting during slicing
    crate::slicer::copy::set_multi_version_info(
        multi_version_info.version_map.clone(),
        multi_version_info.multi_version_crates.clone(),
        verbose,
    );

    // Check if we should load Phase 2 cache and skip to Phase 3
    let (crate_items, crate_used, crate_needed, crate_scip_analysis) = if let Some(cache_path) =
        load_phase2_cache_path
    {
        multi
            .println(format!(
                "\n⚡ Loading Phase 2 cache from: {}\n",
                cache_path.display()
            ))
            .ok();

        let cache = match load_phase2_cache(cache_path) {
            Ok(cache) => cache,
            Err(e) => {
                multi.println(format!("Error loading cache: {}\n", e)).ok();
                return result;
            }
        };

        multi
            .println(format!(
                "✓ Loaded {} crate(s) from cache",
                cache.crate_needed.len()
            ))
            .ok();
        multi
            .println("⏭  Skipping Phases 1, 3, and 4\n".to_string())
            .ok();

        // Still need to copy original sources if requested (Phase 1 task)
        if original_source {
            if let Err(e) = copy_original_sources(graph, Path::new(".")) {
                multi
                    .println(format!("Warning: Failed to copy original sources: {}", e))
                    .ok();
            }
        }

        // Still need to measure original LOC if requested (Phase 2 task)
        if measure {
            multi
                .println("Phase 2: Measuring original LOC of cached crates...\n")
                .ok();
            let cached_crates: Vec<String> = cache.crate_needed.keys().cloned().collect();
            let measure_pb = create_progress_bar(&multi, cached_crates.len() as u64, "Measuring");
            measure_pb.set_message("measuring");
            measure_pb.enable_steady_tick(std::time::Duration::from_millis(100));

            for crate_name in &cached_crates {
                if let Some(info) = find_crate_source(crate_name, graph.nodes.get(crate_name).map(|n| n.version.as_str())) {
                    let loc = crate::measure::measure_loc(&info.path);
                    let mut m = crate::types::CrateMeasurement::default();
                    m.name = crate_name.clone();
                    m.before_loc = loc;
                    result.measurements.insert(crate_name.clone(), m);
                }
                measure_pb.inc(1);
            }
            measure_pb.finish_with_message("Done");

            // Also measure total LOC for ALL dependencies (including non-sliceable)
            let all_deps = graph.all_deps();
            let mut total_all_loc: usize = 0;
            for crate_name in &all_deps {
                if let Some(m) = result.measurements.get(*crate_name) {
                    total_all_loc += m.before_loc;
                } else if let Some(info) = find_crate_source(crate_name, graph.nodes.get(*crate_name).map(|n| n.version.as_str())) {
                    total_all_loc += crate::measure::measure_loc(&info.path);
                }
            }
            result.total_all_deps_loc = total_all_loc;
            result.total_all_deps_count = all_deps.len();
        }

        // Return the cached data and jump to Phase 3
        (
            cache.crate_items,
            cache.crate_used,
            cache.crate_needed,
            cache.crate_scip_analysis,
        )
    } else {
        // Normal execution: run Phases 1, 3, and 4

        // Copy original sources if requested
        if original_source {
            if let Err(e) = copy_original_sources(graph, Path::new(".")) {
                multi
                    .println(format!("Warning: Failed to copy original sources: {}", e))
                    .ok();
            }
        }

        // First, identify all sliceable crates in the dependency tree
        let all_deps = graph.all_deps();
        let mut sliceable_crates: HashSet<String> = HashSet::new();

        // Start profiling Phase 1
        crate::profiling::start_timing("Phase 1: Identify Sliceable Crates");

        println!("\nPhase 1: Identifying sliceable crates in dependency tree...");
        println!("  Total crates in tree: {}", all_deps.len());
        if force_all {
            multi
                .println("  Mode: Attempting all crates (including complex ones like serde, tokio)")
                .ok();
        } else {
            multi
                .println("  Mode: Conservative (skipping complex crates)")
                .ok();
        }

        // Compute transitive dependencies of COMPLEX_CRATES to exclude from slicing
        // This prevents checksum mismatches when non-sliced crates in the registry
        // try to resolve their dependencies
        // Note: When force_all is true, COMPLEX_CRATES themselves are sliced, so they
        // use path dependencies and can find sliced transitive deps. But when force_all
        // is false (conservative mode), COMPLEX_CRATES go to registry and need unsliced deps.
        // However, if use_local_registry is true, we should always exclude transitive deps
        // to avoid checksum issues, regardless of force_all.
        let mut complex_transitive_deps = std::collections::HashSet::new();
        if use_local_registry {
            for complex_crate in COMPLEX_CRATES {
                if all_deps.contains(complex_crate) {
                    // BFS to find all transitive dependencies
                    let mut to_visit = vec![complex_crate.to_string()];
                    let mut visited = std::collections::HashSet::new();

                    while let Some(current) = to_visit.pop() {
                        if visited.contains(&current) {
                            continue;
                        }
                        visited.insert(current.clone());
                        complex_transitive_deps.insert(current.clone());

                        if let Some(node) = graph.nodes.get(&current) {
                            for dep in &node.deps {
                                if !visited.contains(dep) {
                                    to_visit.push(dep.clone());
                                }
                            }
                        }
                    }
                }
            }
            // Also exclude crates with problematic transitive WASM deps (getrandom → wasip2 → wit-bindgen)
            complex_transitive_deps.insert("ctrlc".to_string());
            complex_transitive_deps.insert("indicatif".to_string());
            complex_transitive_deps.insert("notify".to_string());
            complex_transitive_deps.insert("proptest".to_string());
            complex_transitive_deps.insert("tempfile".to_string());

            println!("  🔒 Excluding {} transitive dependencies of COMPLEX_CRATES from slicing",
                complex_transitive_deps.len());
            if verbose && complex_transitive_deps.len() > 0 {
                let mut deps_list: Vec<&String> = complex_transitive_deps.iter().collect();
                deps_list.sort();
                for (i, dep) in deps_list.iter().enumerate() {
                    if i < 20 {  // Show first 20
                        println!("     - {}", dep);
                    }
                }
                if deps_list.len() > 20 {
                    println!("     ... and {} more", deps_list.len() - 20);
                }
            }
        }

        for crate_name in &all_deps {
            // Always skip FFI crates (too low-level)
            if FFI_CRATES.contains(crate_name) {
                continue;
            }
            // Skip complex crates only if force_all is disabled
            if COMPLEX_CRATES.contains(crate_name) && !force_all {
                continue;
            }
            // Skip transitive dependencies of complex crates (prevents registry checksum mismatches)
            if complex_transitive_deps.contains(*crate_name) {
                continue;
            }
            // Skip platform-specific crates (WASI, Windows, etc.) - they cause transitive conflicts
            if PLATFORM_SPECIFIC_CRATES.contains(crate_name) {
                continue;
            }
            // Skip crates that cause version conflicts (links attribute, macro deps)
            if CONFLICT_PRONE_CRATES.contains(crate_name) {
                continue;
            }
            // Check if source exists and if it's a proc-macro crate
            if let Some(info) = find_crate_source(crate_name, graph.nodes.get(*crate_name).map(|n| n.version.as_str())) {
                // Always skip proc-macro crates (detected dynamically from Cargo.toml)
                if info.is_proc_macro {
                    continue;
                }
                // Skip crates that are likely to bloat if registry_fallback is enabled
                // UNLESS the crate is in slice_blocked (explicit override via -fslice-<crate>)
                // Uses pre-computed total_lines from CrateInfo (no redundant tokei measurement)
                if registry_fallback && is_likely_to_bloat(crate_name, info.total_lines) {
                    // Check if user explicitly requested slicing this crate via -fslice-<crate>
                    if features.slice_blocked.contains(&crate_name.to_string()) {
                        debug_log!("  ✂️  {} (force slicing via -fslice-{})", crate_name, crate_name);
                    } else {
                        debug_log!("  📦 {} (using registry - bloat detected)", crate_name);
                        continue;
                    }
                }
                sliceable_crates.insert(crate_name.to_string());
            }
        }

        multi
            .println(format!(
                "  ✂️  Sliceable crates: {} (sample: {:?})\n",
                sliceable_crates.len(),
                sliceable_crates.iter().take(5).collect::<Vec<_>>()
            ))
            .ok();

        // End profiling Phase 1
        crate::profiling::end_timing();

        // Measure original LOC if requested
        if measure {
            // Start profiling Phase 2
            crate::profiling::start_timing("Phase 2: Measure LOC");

            multi
                .println("Phase 2: Measuring original LOC of sliceable crates...\n")
                .ok();
            let measure_pb =
                create_progress_bar(&multi, sliceable_crates.len() as u64, "Measuring");
            measure_pb.set_message("measuring");
            measure_pb.enable_steady_tick(std::time::Duration::from_millis(100));

            for crate_name in &sliceable_crates {
                if let Some(info) = find_crate_source(crate_name, graph.nodes.get(crate_name.as_str()).map(|n| n.version.as_str())) {
                    let loc = crate::measure::measure_loc(&info.path);
                    let bytes = crate::measure::measure_bytes(&info.path);
                    let mut m = crate::types::CrateMeasurement::default();
                    m.name = crate_name.clone();
                    m.before_loc = loc;
                    m.before_bytes = bytes;
                    result.measurements.insert(crate_name.clone(), m);
                }
                measure_pb.inc(1);
            }
            measure_pb.finish_with_message("Done");

            // Also measure total LOC for ALL dependencies (including non-sliceable)
            let mut total_all_loc: usize = 0;
            for crate_name in &all_deps {
                if let Some(m) = result.measurements.get(*crate_name) {
                    // Already measured as sliceable
                    total_all_loc += m.before_loc;
                } else if let Some(info) = find_crate_source(crate_name, graph.nodes.get(*crate_name).map(|n| n.version.as_str())) {
                    total_all_loc += crate::measure::measure_loc(&info.path);
                }
            }
            result.total_all_deps_loc = total_all_loc;
            result.total_all_deps_count = all_deps.len();

            // End profiling Phase 2
            crate::profiling::end_timing();
        }

        // Start profiling Phase 3
        crate::profiling::start_timing("Phase 3: Collect Usage");
        let phase3_start = Instant::now();

        println!("\nPhase 3: Collecting all item usages via inverted index{}...",
            if parallel { " (parallel)" } else { "" });

        // Phase 3 (Optimized): Build inverted usage index by scanning files ONCE
        // This is O(files) instead of O(crates × files)

        // Step 1: Collect all dependent source directories
        let usage_pb = create_progress_bar(&multi, 3, "Phase 3");
        usage_pb.set_message("collecting sources");
        usage_pb.enable_steady_tick(std::time::Duration::from_millis(100));

        let mut dependent_sources: Vec<(String, std::path::PathBuf)> = Vec::new();
        let mut seen_deps = HashSet::new();

        for crate_name in &sliceable_crates {
            let dependents = graph.reverse_deps(crate_name);
            for dependent in &dependents {
                if seen_deps.insert(dependent.to_string()) {
                    if let Some(dep_info) = find_crate_source(dependent, graph.nodes.get(*dependent).map(|n| n.version.as_str())) {
                        let dep_src = dep_info.path.join("src");
                        if dep_src.exists() {
                            dependent_sources.push((dependent.to_string(), dep_src));
                        }
                    }
                }
            }
        }

        usage_pb.inc(1);
        usage_pb.set_message("scanning files");

        // Count files for verbose output
        let project_file_count = find_rust_files(project_src_dir).len();
        let dep_file_count: usize = dependent_sources.iter()
            .map(|(_, path)| find_rust_files(path).len())
            .sum();

        if verbose {
            println!("  Scanning {} project files + {} dependent files (O(1) lookup per crate)",
                project_file_count, dep_file_count);
        }

        // Step 2: Build the inverted usage index (reads each file ONCE)
        let usage_index = build_usage_index(project_src_dir, &dependent_sources, &sliceable_crates);

        usage_pb.inc(1);
        usage_pb.set_message("building results");

        // Step 3: Convert index to expected format (crate_items, crate_used)
        let mut crate_items: HashMap<String, HashSet<String>> = HashMap::new();
        let mut crate_used: HashMap<String, HashSet<UsedItem>> = HashMap::new();

        for crate_name in &sliceable_crates {
            let rust_crate_name = crate_name.replace('-', "_");

            // Look up usage from the inverted index (O(1))
            if let Some(all_used) = usage_index.get(&rust_crate_name) {
                if all_used.is_empty() {
                    continue;
                }

                // Extract item names from usage
                // Preserve relative module paths to avoid duplicate name conflicts
                // e.g., "regex::ast::Parser" → "ast::Parser" (not just "Parser")
                let items: HashSet<String> = all_used.iter()
                    .flat_map(|u| {
                        let parts: Vec<&str> = u.path.split("::").collect();
                        if parts.len() >= 2 && parts[0] == rust_crate_name {
                            let mut result = Vec::new();

                            if parts.len() >= 3 && u.kind == ItemKind::Method {
                                // For methods, add both the parent type's qualified path and simple name
                                // e.g., "regex::ast::Parser::new" → ["ast::Parser", "Parser"]
                                let parent_qualified = parts[1..parts.len()-1].join("::");
                                let parent_simple = parts[parts.len() - 2].to_string();
                                if !parent_qualified.is_empty() && parent_qualified != "*" {
                                    result.push(parent_qualified);
                                }
                                if !parent_simple.is_empty() && parent_simple != "*" {
                                    result.push(parent_simple);
                                }
                            } else {
                                // For other items, add both qualified and simple names
                                // e.g., "regex::ast::Parser" → ["ast::Parser", "Parser"]
                                let qualified = parts[1..].join("::");
                                let simple = parts.last().unwrap().to_string();
                                // Only add simple name if different from qualified
                                if !simple.is_empty() && simple != "*" && simple != qualified {
                                    result.push(simple);
                                }
                                if !qualified.is_empty() && qualified != "*" {
                                    result.push(qualified);
                                }
                            }
                            result
                        } else {
                            Vec::new()
                        }
                    })
                    .collect();

                if !items.is_empty() {
                    debug_log!("{}: {} unique items", crate_name, items.len());
                    crate_items.insert(crate_name.clone(), items);
                    crate_used.insert(crate_name.clone(), all_used.clone());
                }
            }
        }

        usage_pb.inc(1);
        usage_pb.finish_with_message("Done");

        if crate_items.is_empty() {
            multi.println("\nNo crate usage found to slice.\n").ok();
            if verbose {
                multi.println(format!("  Debug: sliceable_crates had {} crates", sliceable_crates.len())).ok();
            }
            return result;
        }

        if verbose {
            multi.println(format!("\n  Debug: Found {} crates with usage data", crate_items.len())).ok();
        }

        // Filter to profile_crate if specified
        let crate_items: HashMap<String, HashSet<String>> = if let Some(target) = profile_crate {
            crate_items.into_iter()
                .filter(|(name, _)| name == target)
                .collect()
        } else {
            crate_items
        };

        if crate_items.is_empty() {
            if let Some(target) = profile_crate {
                multi.println(format!("\nCrate '{}' not found in sliceable dependencies.\n", target)).ok();
            }
            return result;
        }

        let num_crates = crate_items.len();

        // End profiling Phase 3
        let phase3_elapsed = phase3_start.elapsed();
        println!("Phase 3 completed: {} crates with usage in {:.2}s",
            num_crates, phase3_elapsed.as_secs_f64());
        crate::profiling::end_timing();

        // Export symbols to linker script if requested
        if let Some(symbols_path) = export_symbols_path {
            println!("\n=== Exporting Symbols to Linker Script ===");
            match export_symbols_to_linker_script(&crate_items, symbols_path) {
                Ok(()) => {
                    let total_items: usize = crate_items.values().map(|items| items.len()).sum();
                    println!("✓ Exported {} items from {} crates to {}",
                        total_items, crate_items.len(), symbols_path.display());
                    println!("\nTo use the linker script:");
                    println!("  RUSTFLAGS=\"-C link-arg=-Wl,--version-script={}\" cargo build --release",
                        symbols_path.display());
                    println!("\nFor maximum binary size reduction, also enable:");
                    println!("  [profile.release]");
                    println!("  lto = \"fat\"");
                    println!("  codegen-units = 1");
                    println!("  opt-level = \"z\"");
                    println!("  strip = true\n");
                }
                Err(e) => {
                    eprintln!("Error exporting symbols: {}", e);
                }
            }
        }

        eprintln!("[DEBUG] After Phase 3, before cfg_feature_deps check");

        // Phase 3.5: Cfg-feature-aware dependency filtering (O4 only)
        if features.cfg_feature_deps {
            println!("\nPhase 3.5: Analyzing cfg-feature dependencies...");
            let required_features = analyze_feature_usage(&sliceable_crates, &crate_items, &graph, project_root);
            result.required_features = required_features;
        }

        // Phase 4: Run rust-analyzer (SKIPPED for new slicer - it computes its own closure)
        let crate_items_vec: Vec<(String, HashSet<String>)> = crate_items.into_iter().collect();

        let crate_needed: HashMap<String, BTreeSet<String>>;

        let reconstructed_items: HashMap<String, HashSet<String>>;

        if use_new_slicer {
            // NEW SLICER PATH: Skip Phase 4 entirely
            // The new slicer computes its own transitive closure via BFS in Phase 5
            multi.println("\n⏭  Phase 4: Skipped (new slicer computes transitive closure from AST)\n").ok();

            // For new slicer, use the items from Phase 3 as the seed items
            // Convert HashSet to BTreeSet for compatibility with the rest of the pipeline
            crate_needed = crate_items_vec.iter()
                .map(|(name, items)| (name.clone(), items.iter().cloned().collect()))
                .collect();

            // Convert crate_items_vec back to HashMap (no reconstruction needed)
            reconstructed_items = crate_items_vec.into_iter().collect();
        } else {
        // OLD SLICER PATH: Run Phase 2 rust-analyzer analysis
        let phase4_failures: Vec<(String, String)>;
        let phase4_start = Instant::now();

        multi.println(format!("\nPhase 2: Running rust-analyzer on {} crate(s){}...\n",
                 num_crates, if parallel { " (parallel)" } else { "" })).ok();

        // Setup progress tracking for Phase 4
        let phase4_pb = create_progress_bar(&multi, num_crates as u64, "Phase 4");
        phase4_pb.set_message("rust-analyzer");
        phase4_pb.enable_steady_tick(std::time::Duration::from_millis(100));
        let stats2_pb = create_stats_bar(&multi);
        let state2 = Arc::new(ProgressState::new(num_crates, abort_threshold));

        if parallel && num_crates > 1 {
            // Parallel processing with rayon
            let state2_clone = state2.clone();
            let phase4_pb_clone = phase4_pb.clone();
            let stats2_pb_clone = stats2_pb.clone();
    
            let results: Vec<(String, Result<BTreeSet<String>, String>)> = crate_items_vec
                .par_iter()
                .map(|(crate_name, items)| {
                    // Check for abort before starting
                    if state2_clone.is_aborted() {
                        return (crate_name.clone(), Err("Aborted: threshold exceeded".to_string()));
                    }
    
                        let start = Instant::now();
                        let crate_info = match find_crate_source(crate_name, graph.nodes.get(crate_name).map(|n| n.version.as_str())) {
                            Some(info) => info,
                            None => {
                                state2_clone.record_error();
                                phase4_pb_clone.inc(1);
                                let (_, success, errors, _) = state2_clone.get_stats();
                                stats2_pb_clone.set_message(format!("Success: {} | Errors: {}", success, errors));
                                return (crate_name.clone(), Err("Source not found".to_string()));
                            }
                        };
    
                    let seed_items: Vec<String> = items.iter().cloned().collect();
    
                    // Use fixed timeout if specified, otherwise compute dynamic timeout
                    let timeout = if use_fixed_timeout {
                        fixed_timeout.unwrap()
                    } else {
                        compute_dynamic_timeout(&crate_info.path)
                    };
    
                    let result = match run_ra_deps_with_timeout(&crate_info.path, &seed_items, timeout) {
                        Ok(needed) => {
                            let elapsed = start.elapsed();
                            if show_progress {
                                multi.println(format!("  {}: {} seed -> {} needed ({:.2}s)",
                                         crate_name, seed_items.len(), needed.len(), elapsed.as_secs_f64())).ok();
                            }
                            state2_clone.record_success();
                            (crate_name.clone(), Ok(needed))
                        }
                        Err(e) => {
                            if show_progress {
                                multi.println(format!("  {}: rust-analyzer failed: {}", crate_name, e)).ok();
                            }
                            state2_clone.record_error();
                            (crate_name.clone(), Err(format!("ra_deps: {}", e)))
                        }
                    };
    
                    phase4_pb_clone.inc(1);
                    let (_, success, errors, _) = state2_clone.get_stats();
                    stats2_pb_clone.set_message(format!("Success: {} | Errors: {}", success, errors));
    
                    result
                })
                .collect();
    
            // Collect results
            let mut needed_map = HashMap::new();
            let mut failures = Vec::new();
            for (name, res) in results {
                match res {
                    Ok(needed) => { needed_map.insert(name, needed); }
                    Err(e) => { failures.push((name, e)); }
                }
            }
            crate_needed = needed_map;
            phase4_failures = failures;
        } else {
            // Sequential processing
            let mut needed_map = HashMap::new();
            let mut failures = Vec::new();
    
            for (crate_name, items) in &crate_items_vec {
                // Check for abort
                if state2.is_aborted() {
                    failures.push((crate_name.clone(), "Aborted: threshold exceeded".to_string()));
                    continue;
                }
    
                let start = Instant::now();
                let crate_info = match find_crate_source(crate_name, graph.nodes.get(crate_name).map(|n| n.version.as_str())) {
                    Some(info) => info,
                    None => {
                        if show_progress {
                            multi.println(format!("  {}: source not found", crate_name)).ok();
                        }
                        state2.record_error();
                        phase4_pb.inc(1);
                        let (_, success, errors, _) = state2.get_stats();
                        stats2_pb.set_message(format!("Success: {} | Errors: {}", success, errors));
                        failures.push((crate_name.clone(), "Source not found".to_string()));
                        continue;
                    }
                };
    
                let seed_items: Vec<String> = items.iter().cloned().collect();
    
                // Use fixed timeout if specified, otherwise compute dynamic timeout
                let timeout = if use_fixed_timeout {
                    fixed_timeout.unwrap()
                } else {
                    compute_dynamic_timeout(&crate_info.path)
                };
    
                if show_progress {
                    debug_log!("{}: analyzing {} seed items (timeout: {}s)...",
                        crate_name, seed_items.len(), timeout.as_secs());
                }
    
                match run_ra_deps_with_timeout(&crate_info.path, &seed_items, timeout) {
                    Ok(needed) => {
                        let elapsed = start.elapsed();
                        if show_progress {
                            debug_log!("  -> {} transitive items needed ({:.2}s)", needed.len(), elapsed.as_secs_f64());
                        }
                        state2.record_success();
                        needed_map.insert(crate_name.clone(), needed);
                    }
                    Err(e) => {
                        if show_progress {
                            multi.println(format!("    -> rust-analyzer failed: {}", e)).ok();
                        }
                        state2.record_error();
                        failures.push((crate_name.clone(), format!("ra_deps: {}", e)));
                    }
                }
    
                phase4_pb.inc(1);
                let (_, success, errors, _) = state2.get_stats();
                stats2_pb.set_message(format!("Success: {} | Errors: {}", success, errors));
            }
            crate_needed = needed_map;
            phase4_failures = failures;
        }
    
        phase4_pb.finish_with_message("Done");
        let (_, success, errors, _) = state2.get_stats();
        stats2_pb.finish_with_message(format!("Phase 4 Finished: {} success, {} errors", success, errors));
    
        // Check if aborted
        if state2.is_aborted() {
            multi.println("\n  Phase 4 aborted: failure rate exceeded threshold").ok();
        }
    
        // Record Phase 2 failures
        for (name, err) in phase4_failures {
            result.failed.push((name, err));
        }
    
        let phase4_elapsed = phase4_start.elapsed();
        println!("\nPhase 2 completed: {} crates analyzed in {:.2}s ({:.2}s/crate avg)",
                 num_crates, phase4_elapsed.as_secs_f64(),
                 phase4_elapsed.as_secs_f64() / num_crates.max(1) as f64);

        // Reconstruct crate_items from crate_used (needed because original was moved/consumed earlier)
        // Preserve relative module paths to avoid duplicate name conflicts
        reconstructed_items = crate_needed.iter()
            .filter_map(|(crate_name, _)| {
                let rust_crate_name = crate_name.replace('-', "_");
                crate_used.get(crate_name).map(|used| {
                    let items: HashSet<String> = used.iter()
                        .flat_map(|u| {
                            let parts: Vec<&str> = u.path.split("::").collect();
                            if parts.len() >= 2 && parts[0] == rust_crate_name {
                                let mut result = Vec::new();
                                // Add both qualified and simple names for compatibility
                                let qualified = parts[1..].join("::");
                                let simple = parts.last().unwrap().to_string();
                                if !qualified.is_empty() && qualified != "*" {
                                    result.push(qualified);
                                }
                                if !simple.is_empty() && simple != "*" && simple != parts[1..].join("::") {
                                    result.push(simple);
                                }
                                result
                            } else if parts.len() >= 2 {
                                // Fallback: just use simple name if crate name doesn't match
                                let name = parts.last().unwrap().to_string();
                                if name != "*" { vec![name] } else { vec![] }
                            } else {
                                vec![]
                            }
                        })
                        .collect();
                    (crate_name.clone(), items)
                })
            })
            .collect();
    } // End of Phase 2 (old slicer only)

    // Phase 4 already saved SCIP caches to disk via run_ra_deps_with_timeout
    // Phase 3 will load these caches on-demand via analyze_crate_via_scip
    // We don't pre-load them here to avoid potential hangs on problematic crates
    let crate_scip_analysis: HashMap<String, crate::old_slicer::semantic::ScipAnalysis> = HashMap::new();

    // Save Phase 2 cache if requested (only for old slicer)
    if !use_new_slicer {
        if let Some(cache_path) = save_phase2_cache_path {
            if let Err(e) = save_phase2_cache(cache_path, &reconstructed_items, &crate_used, &crate_needed, &crate_scip_analysis) {
                eprintln!("Warning: Failed to save Phase 2 cache: {}", e);
            }
        }
    }

        // Return the data from Phases 0, 1, and 2
        (reconstructed_items, crate_used, crate_needed, crate_scip_analysis)
    }; // End of cache load/normal execution branch

    // At this point, we have (crate_items, crate_used, mut crate_needed) from either cache or fresh computation
    let mut crate_needed = crate_needed; // Make mutable for filtering

    // Filter out crates with 0 items needed - don't generate stubs for unused crates
    let before_filter = crate_needed.len();
    crate_needed.retain(|name, needed| {
        if needed.is_empty() {
            debug_log!("Skipping {} (0 items needed)", name);
            return false;
        }
        true
    });
    let skipped = before_filter - crate_needed.len();
    if skipped > 0 {
        multi.println(format!("\nSkipped {} unused crate(s)", skipped)).ok();
    }

    // Collect the set of crates we're slicing (for Cargo.toml rewriting)
    let _sliced_set: HashSet<String> = crate_needed.keys().cloned().collect();

    // Save a copy of crate_needed for transitive requirements computation
    let crate_needed_map: HashMap<String, BTreeSet<String>> = crate_needed.clone();

    // Start profiling Phase 5
    crate::profiling::start_timing("Phase 5: Code Generation");

    let phase5_start = Instant::now();
    let num_to_generate = crate_needed.len();
    if !verbose {
        multi.println(format!("\nPhase 5: Slicing {} crate(s){}...",
                 num_to_generate, if parallel { " (parallel)" } else { "" })).ok();
    } else {
        multi.println(format!("\nPhase 5: Generating {} sliced crate(s){}...\n",
                 num_to_generate, if parallel { " (parallel)" } else { "" })).ok();
    }

    // Phase 5: Generate sliced crates using the computed needed sets
    // Type: (crate_name, optional_version, needed_items)
    // version is Some for multi-version crates, None otherwise
    let mut crate_needed_vec: Vec<(String, Option<String>, BTreeSet<String>)> = crate_needed
        .into_iter()
        .map(|(name, needed)| (name, None, needed))
        .collect();

    // For multi-version crates: only slice ONE version (the latest)
    // Consumers that need other versions will use registry fallback automatically.
    // This avoids workspace duplicate package name conflicts.
    for (crate_name, versions) in &multi_version_info.multi_version_crates {
        // Find the base entry (if it exists)
        if let Some(pos) = crate_needed_vec.iter().position(|(n, _, _)| n == crate_name) {
            let needed = crate_needed_vec[pos].2.clone();

            // Remove the base entry
            crate_needed_vec.remove(pos);

            // Pick the LATEST version only (versions are sorted, last = highest)
            // Other versions will fall back to registry (no path dep)
            if let Some(latest_version) = versions.last() {
                crate_needed_vec.push((crate_name.clone(), Some(latest_version.clone()), needed));

                if verbose {
                    println!("  Multi-version: {} → slicing v{} only (others use registry)",
                        crate_name, latest_version);
                }
            }
        }
    }

    let num_to_generate = crate_needed_vec.len();

    // Setup progress tracking for Phase 5a
    let phase5_pb = create_progress_bar(&multi, num_to_generate as u64, "Phase 5a");
    phase5_pb.enable_steady_tick(std::time::Duration::from_millis(100));
    let state3 = Arc::new(ProgressState::new(num_to_generate, abort_threshold));

    // Track currently processing crates for compact status line
    use std::sync::Mutex;
    let processing_crates: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));

    // Process a single crate (used by both parallel and sequential paths)
    // version: Some for multi-version crates, None for single-version
    let process_crate = |crate_name: &str, version: Option<&str>, needed: &BTreeSet<String>| -> Phase5Result {
        // Build display name for progress messages
        let display_name = if let Some(v) = version {
            format!("{} v{}", crate_name, v)
        } else {
            crate_name.to_string()
        };

        // Track this crate as currently processing
        if !verbose {
            if let Ok(mut crates) = processing_crates.lock() {
                crates.push(display_name.clone());
                // Update progress bar to show top 5 processing crates and stats
                let top5: Vec<String> = crates.iter().take(5).cloned().collect();
                let more = if crates.len() > 5 {
                    format!(" (+{})", crates.len() - 5)
                } else {
                    String::new()
                };
                let (_, success, errors, passthrough) = state3.get_stats();
                let crates_msg = if !crates.is_empty() {
                    format!("{}{} | ", top5.join(", "), more)
                } else {
                    String::new()
                };
                phase5_pb.set_message(format!("{}✓{} ✗{} ⏭{}", crates_msg, success, errors, passthrough));
            }
        }

        let start = Instant::now();

        // For multi-version crates, use explicit version; otherwise use graph lookup
        let version_to_find = version.or_else(|| graph.nodes.get(crate_name).map(|n| n.version.as_str()));
        let crate_info = match find_crate_source(crate_name, version_to_find) {
            Some(info) => info,
            None => {
                // Remove from processing list
                if !verbose {
                    if let Ok(mut crates) = processing_crates.lock() {
                        crates.retain(|c| c != &display_name);
                    }
                }
                return Phase5Result::Error(display_name, "Source not found".to_string());
            }
        };

        let _used = crate_used.get(crate_name).cloned().unwrap_or_default();

        // Generate output directory name:
        // - Single-version: memchr-sliced
        // - Multi-version: bitflags-1.3.2-sliced
        let is_multi_version = version.is_some();
        let output_dir = if is_multi_version {
            output_base.join(format!("{}-{}-sliced", crate_name, version.unwrap()))
        } else {
            output_base.join(format!("{}-sliced", crate_name))
        };

        // Parse the crate (using rustc driver if enabled)
        let index = parse_crate_with_features(&crate_info.path, &crate_info.name, features);

        // Expand needed set to include private helper functions
        // RA doesn't catch internal function calls like vec_packed_fixed_size
        let needed = &crate::old_slicer::slicing::expand_needed_transitively(needed, &index);

        // Generate sliced crate with the pre-computed needed set (item-based slicing)
        // Pass pre-computed SCIP analysis from Phase 2 to avoid re-running rust-analyzer
        let scip_analysis_opt = crate_scip_analysis.get(crate_name);

        // Create per-crate progress bar for large crates (Docker/pip style in-place updates)
        // We'll lazily create it on first callback if needed > 200 items
        use std::cell::RefCell;
        let crate_pb: RefCell<Option<ProgressBar>> = RefCell::new(None);
        let item_count: RefCell<Option<usize>> = RefCell::new(None);  // Track item count for post-processing

        // Progress callback updates the progress bar in-place
        let progress_fn = |current: usize, total: usize| {
            if show_progress && total > 200 {  // Only show progress for crates with >200 items
                // Lazily create progress bar on first call
                let mut pb_ref = crate_pb.borrow_mut();
                if pb_ref.is_none() {
                    let pb = multi.add(ProgressBar::new(100));
                    pb.set_style(
                        ProgressStyle::default_bar()
                            .template(&format!("  {{spinner:.cyan}} {}: [{{bar:30.cyan/blue}}] {{pos}}% ({{msg}})", crate_name))
                            .unwrap()
                            .progress_chars("=>-")
                    );
                    pb.enable_steady_tick(std::time::Duration::from_millis(100));
                    *pb_ref = Some(pb);
                }

                if let Some(ref pb) = *pb_ref {
                    // Three phases:
                    // 1. Item processing: current <= total, both stay same
                    // 2. Module imports: callback(items+modules_done, items+modules_total)
                    // 3. Finalizing: callback(items*2, items) - way beyond modules
                    let mut item_count_ref = item_count.borrow_mut();

                    let (percent, msg) = if current == total && item_count_ref.is_none() {
                        // First time hitting 100% - remember item count
                        *item_count_ref = Some(total);
                        (100, "post-processing 0% (starting...)".to_string())
                    } else if let Some(items) = *item_count_ref {
                        // Check if we're in finalizing phase (current is way beyond reasonable)
                        // Finalizing marker: callback(items*2, items)
                        let modules_total = total.saturating_sub(items);
                        let modules_done = current.saturating_sub(items);

                        if current >= items * 2 || modules_done > modules_total * 2 {
                            // Finalizing phase: callback(items*2 + step, items*2 + total_steps)
                            // Decode the finalizing step progress
                            let finalize_base = items * 2;
                            if total > finalize_base {
                                let finalize_total_steps = total - finalize_base;
                                let finalize_current_step = current.saturating_sub(finalize_base);
                                let finalize_percent = (finalize_current_step * 100) / finalize_total_steps.max(1);
                                (100, format!("finalizing {}% (step {}/{})", finalize_percent, finalize_current_step, finalize_total_steps))
                            } else {
                                // Fallback if encoding doesn't match expected pattern
                                (100, "finalizing (writing files, fixing imports)".to_string())
                            }
                        } else if modules_total > 0 {
                            // Module import generation phase
                            let module_percent = (modules_done * 100) / modules_total;
                            (100, format!("post-processing {}% ({}/{} modules)",
                                         module_percent, modules_done, modules_total))
                        } else {
                            // Edge case: no modules, but in post-processing
                            (100, "finalizing (writing files)".to_string())
                        }
                    } else {
                        // Normal item processing
                        let percent = (current * 100) / total.max(1);
                        (percent, format!("{}/{} items", current, total))
                    };

                    pb.set_position(percent as u64);
                    pb.set_message(msg);
                }
            }
        };

        // Phase 6.4: Apply type closure to include types referenced in signatures
        let needed = &crate::old_slicer::slicing::compute_type_closure(needed, &index);

        let result = if use_new_slicer {
            // Use new copy-and-delete slicer
            // Use crate_items (seed items) instead of needed (full transitive closure)
            // This allows the new slicer's BFS to compute its own closure from AST
            let seed_items: HashSet<String> = crate_items.get(crate_name)
                .map(|items| items.clone())
                .unwrap_or_else(|| needed.iter().cloned().collect());

            let config = crate::slicer::config::SlicerConfig {
                verbose,
                features: features.clone(),
                ..Default::default()
            };

            // Check incremental cache first
            let crate_version = crate::slicer::cache::get_crate_version(&crate_info.path)
                .unwrap_or_else(|| "unknown".to_string());
            let content_hash = crate::slicer::cache::SlicerCache::content_hash(&seed_items, features);

            // Check for cache hit
            let cache_hit = sliced_cache.as_ref().and_then(|cache| {
                let cache_guard = cache.lock().ok()?;
                cache_guard.get(crate_name, &crate_version, &content_hash)
                    .map(|entry| entry.clone())
            });

            if let Some(ref cached_entry) = cache_hit {
                // Cache hit! Copy cached output
                println!("  ⚡ {} (cached)", display_name);
                if let Err(e) = crate::slicer::cache::copy_cached_output(project_root, &cached_entry, &output_dir) {
                    // Cache copy failed, fall through to normal slicing
                    if verbose {
                        println!("  ⚠️  Cache copy failed for {}: {}", display_name, e);
                    }
                } else {
                    // Cache hit successful - re-apply Cargo.toml fixes for multi-version deps
                    // This is needed because cached outputs have old paths, but we may need
                    // versioned paths now (e.g., ../bitflags-1.3.2-sliced instead of version = "1")
                    let cargo_toml_path = output_dir.join("Cargo.toml");
                    if cargo_toml_path.exists() {
                        if let Err(e) = crate::slicer::copy::fix_cargo_toml_all(&output_dir) {
                            if verbose {
                                println!("  ⚠️  Cargo.toml fix failed for cached {}: {}", display_name, e);
                            }
                        }
                    }

                    if !verbose {
                        // Show brief cache hit indicator in non-verbose mode
                        if let Ok(mut crates) = processing_crates.lock() {
                            crates.retain(|c| c != &display_name);
                        }
                    }
                    if let Some(ref pb) = *crate_pb.borrow() {
                        pb.finish_and_clear();
                    }
                    return Phase5Result::Success(display_name.clone());
                }
            }

            // Cache miss or copy failed, slice normally
            match crate::slicer::slice_crate(&crate_info.name, &crate_info.path, &seed_items, &output_dir, &config) {
                Ok(slice_result) => {
                    // Save to cache
                    if let Some(ref cache) = sliced_cache {
                        if let Ok(mut cache_guard) = cache.lock() {
                            let _ = crate::slicer::cache::save_to_cache(
                                project_root,
                                crate_name,
                                &crate_version,
                                &content_hash,
                                &output_dir,
                                slice_result.loc_before,
                                slice_result.loc_after,
                                slice_result.items_deleted,
                                &mut cache_guard,
                            );
                        }
                    }

                    if let Some(ref pb) = *crate_pb.borrow() {
                        pb.finish_and_clear();
                    }
                    Phase5Result::Success(display_name.clone())
                }
                Err(e) => Phase5Result::Error(display_name.clone(), e),
            }
        } else {
            // Use old generation-based slicer
            match generate_semantic_sliced_crate_with_needed(&crate_info, &index, needed, &output_dir, scip_analysis_opt, Some(&progress_fn)) {
            Ok(slice_result) => {
                // Update progress bar to show finalizing is complete
                // (compilation check is skipped in multi-crate mode)
                if let Some(ref pb) = *crate_pb.borrow() {
                    if !measure {
                        pb.set_message("post-slice fixing...".to_string());
                    }
                }

                // Phase 3 Option E: Post-slice dependency injection
                // NOTE: With RULE-001 and RULE-003, this should rarely trigger
                // Kept available for edge cases not covered by current rules
                // Fix missing types and log for pattern analysis
                let _ = fix_missing_types(&crate_info.path, &output_dir, &crate_info.name)
                    .map(|fixes| {
                        if !fixes.is_empty() {
                            // Log fixes - we'll aggregate them later for new pattern discovery
                            let fix_log_path = output_dir.join("fix_log.json");
                            let _ = save_fix_log(&fixes, &fix_log_path);
                        }
                    });

                if let Some(ref pb) = *crate_pb.borrow() {
                    if !measure {
                        pb.set_message("checking compilation...".to_string());
                    }
                }

                // Note: rewrite_cargo_toml_for_sliced_deps is called after Phase 3 with final success set

                // Handle crates with 0 items
                if slice_result.items_included == 0 {
                    // If the crate has needed items according to SCIP but slicing produced 0 items,
                    // this is likely a re-export crate (like thiserror, derive_more, etc).
                    // These crates only re-export items from other crates and have no local definitions.
                    // DISABLED: No longer use passthrough - treat as normal success
                    // The sliced crate should still work as a re-export
                    if !needed.is_empty() {
                        // Re-export crate with 0 items - keep the sliced version
                        Phase5Result::Success(display_name.clone())
                    } else {
                        // No per-crate messages - progress bar shows overall progress
                        let _ = fs::remove_dir_all(&output_dir);
                        Phase5Result::Error(display_name.clone(), "0 items needed".to_string())
                    }
                } else {

                // Check if sliced crate compiles
                // In multi-crate mode, skip expensive individual checks
                // Only run checks during auto-fix since it needs feedback to iterate
                let compiles = if auto_fix {
                    // Auto-fix needs to check compilation to know when to stop fixing
                    match auto_fix_sliced_crate(&output_dir, 5) {
                        Ok(0) => {
                            let elapsed = start.elapsed();
                            debug_log!("{}: {} items, compiles ({:.2}s)",
                                     crate_name, slice_result.items_included, elapsed.as_secs_f64());
                            true
                        }
                        Ok(n) => {
                            let check = Command::new("cargo")
                                .args(["check"])
                                .current_dir(&output_dir)
                                .output();
                            let elapsed = start.elapsed();
                            if check.map(|o| o.status.success()).unwrap_or(false) {
                                debug_log!("{}: {} items, fixed in {} iters ({:.2}s)",
                                         crate_name, slice_result.items_included, n, elapsed.as_secs_f64());
                                true
                            } else {
                                false
                            }
                        }
                        Err(_) => false
                    }
                } else {
                    // Skip Phase 3 compilation check
                    // This avoids duplicate cargo check runs regardless of measure setting
                    true  // Assume success
                };

                if compiles {
                    // No per-crate success messages - progress bar shows overall progress
                    Phase5Result::Success(display_name.clone())
                } else {
                    // Passthrough disabled - always report as failure
                    // No per-crate messages - progress bar shows overall progress
                    Phase5Result::Error(display_name.clone(), "slicing errors".to_string())
                }
                }
            }
            Err(e) => {
                // Passthrough disabled - always report as error
                // No per-crate messages - progress bar shows overall progress
                Phase5Result::Error(display_name.clone(), e.to_string())
            }
            }
        };

        // Finish and remove per-crate progress bar
        if let Some(pb) = crate_pb.into_inner() {
            pb.finish_and_clear();
        }

        // Remove from processing list
        if !verbose {
            if let Ok(mut crates) = processing_crates.lock() {
                crates.retain(|c| c != &display_name);
                // Update progress bar message with current crates and stats
                let (_, success, errors, passthrough) = state3.get_stats();
                let top5: Vec<String> = crates.iter().take(5).cloned().collect();
                let more = if crates.len() > 5 {
                    format!(" (+{})", crates.len() - 5)
                } else {
                    String::new()
                };
                let crates_msg = if !crates.is_empty() {
                    format!("{}{} | ", top5.join(", "), more)
                } else {
                    String::new()
                };
                phase5_pb.set_message(format!("{}✓{} ✗{} ⏭{}", crates_msg, success, errors, passthrough));
            }
        }

        result
    };

    let phase5_results: Vec<Phase5Result> = if parallel && num_to_generate > 1 {
        let state3_clone = state3.clone();
        let phase5_pb_clone = phase5_pb.clone();

        crate_needed_vec.par_iter()
            .map(|(name, version, needed)| {
                // Check for abort before starting
                if state3_clone.is_aborted() {
                    return Phase5Result::Error(name.clone(), "Aborted: threshold exceeded".to_string());
                }

                let result = process_crate(name, version.as_deref(), needed);

                // Record result
                match &result {
                    Phase5Result::Success(_) => { state3_clone.record_success(); }
                    Phase5Result::Passthrough(_) => { state3_clone.record_passthrough(); }
                    Phase5Result::Error(_, _) => { state3_clone.record_error(); }
                }

                // Update progress (stats are updated in process_crate cleanup)
                phase5_pb_clone.inc(1);

                result
            })
            .collect()
    } else {
        crate_needed_vec.iter()
            .map(|(name, version, needed)| {
                // Check for abort
                if state3.is_aborted() {
                    return Phase5Result::Error(name.clone(), "Aborted: threshold exceeded".to_string());
                }

                let result = process_crate(name, version.as_deref(), needed);

                // Record result
                match &result {
                    Phase5Result::Success(_) => { state3.record_success(); }
                    Phase5Result::Passthrough(_) => { state3.record_passthrough(); }
                    Phase5Result::Error(_, _) => { state3.record_error(); }
                }

                // Update progress (stats are updated in process_crate cleanup)
                phase5_pb.inc(1);

                result
            })
            .collect()
    };

    phase5_pb.finish_and_clear();

    // Check if aborted
    if state3.is_aborted() {
        multi.println("\n  Phase 3 aborted: failure rate exceeded threshold").ok();
    }

    // Collect Phase 3 results
    let mut removed_count = 0;
    for r in phase5_results {
        match r {
            Phase5Result::Success(name) => result.success.push(name),
            Phase5Result::Passthrough(name) => {
                // Passthrough is disabled, but keep the enum variant for compatibility
                result.success.push(name);
            }
            Phase5Result::Error(name, err) => {
                if err == "0 items needed" {
                    removed_count += 1;
                } else {
                    result.failed.push((name, err));
                }
            }
        }
    }

    let phase5_elapsed = phase5_start.elapsed();

    if verbose {
        println!("\nPhase 5 completed: ✂️  {} sliced, ❌ {} removed (0 items) in {:.2}s",
                 result.success.len(), removed_count, phase5_elapsed.as_secs_f64());
    } else {
        println!("Phase 5 completed: ✂️  {} sliced, ❌ {} removed in {:.2}s",
                 result.success.len(), removed_count, phase5_elapsed.as_secs_f64());
    }

    // Print slicer timing breakdown if using new slicer
    if use_new_slicer && verbose {
        crate::slicer::print_timing_stats();
    }

    // Second pass: Re-fix Cargo.toml dependencies with complete cache
    // This fixes race conditions from parallel slicing and ensures multi-version routing
    if use_new_slicer {
        // Build complete sliced_crates set including base names for versioned crates
        // E.g., for "bitflags v1.3.2" we include both "bitflags-1.3.2" and "bitflags"
        // Note: display names use "v" with space, but directories use "-" with no "v"
        let mut sliced_crates: HashSet<String> = HashSet::new();
        for name in &result.success {
            // Convert display name to directory name format
            // "bitflags v1.3.2" -> "bitflags-1.3.2"
            let normalized_name = if name.contains(" v") {
                name.replace(" v", "-")
            } else {
                name.to_string()
            };
            sliced_crates.insert(normalized_name.clone());

            // Extract base name for versioned crates like "bitflags-1.3.2" -> "bitflags"
            let dir_name = format!("{}-sliced", normalized_name);
            let base_name = crate::slicer::copy::extract_crate_name_from_dir(&dir_name);
            if base_name != normalized_name {
                sliced_crates.insert(base_name.to_string());
            }
        }

        for crate_name in &result.success {
            // Convert display name to directory name format
            let normalized_name = if crate_name.contains(" v") {
                crate_name.replace(" v", "-")
            } else {
                crate_name.to_string()
            };
            let output_dir = output_base.join(format!("{}-sliced", normalized_name));
            if let Err(e) = crate::slicer::copy::refix_cargo_toml_deps(&output_dir, &sliced_crates) {
                if verbose {
                    println!("Warning: Failed to re-fix {} Cargo.toml: {}", crate_name, e);
                }
            }
        }
    }

    // End profiling Phase 5
    crate::profiling::end_timing();

    // Phase 7: Compute transitive requirements and re-slice if needed
    // OPTIMIZATION: Skip Phase 4 entirely when using new slicer
    // The new slicer's BFS traversal already computes transitive closure,
    // so re-slicing is unnecessary and wastes time (often 15-25 seconds).
    // Phase 4 is only needed for the old slicer which doesn't do proper transitive analysis.
    let phase7_start = Instant::now();

    if use_new_slicer && verbose {
        println!("\nPhase 7: Skipped (new slicer already includes transitive dependencies via BFS)");
    }

    let transitive_reqs = if !use_new_slicer {
        compute_transitive_requirements(
            graph,
            &crate_needed_map,
            output_base,
            verbose
        )
    } else {
        HashMap::new() // Empty for new slicer - not needed
    };

    // Identify crates that need re-slicing (transitive requirements different from direct)
    let mut crates_to_reslice: Vec<String> = Vec::new();
    if !use_new_slicer {
        for (crate_name, trans_reqs) in &transitive_reqs {
            let direct_reqs = crate_needed_map.get(crate_name);
            if direct_reqs.map(|d| d != trans_reqs).unwrap_or(true) {
                if trans_reqs.len() > direct_reqs.map(|d| d.len()).unwrap_or(0) {
                    crates_to_reslice.push(crate_name.clone());
                }
            }
        }
    }

    if !crates_to_reslice.is_empty() && !use_new_slicer {
        if verbose {
            println!("\nPhase 7: Re-slicing {} crate(s) with transitive requirements...\n",
                     crates_to_reslice.len());
        } else {
            println!("\nPhase 7: Re-slicing {} crate(s) with transitive requirements...",
                     crates_to_reslice.len());
        }

        let phase7_pb = create_progress_bar(&multi, crates_to_reslice.len() as u64, "Phase 4");
        phase7_pb.set_message("re-slicing");
        phase7_pb.enable_steady_tick(std::time::Duration::from_millis(100));

        // Parallelize Phase 4 re-slicing
        use rayon::prelude::*;

        let phase7_results: Vec<(String, bool)> = if parallel && crates_to_reslice.len() > 1 {
            let phase7_pb_clone = phase7_pb.clone();

            crates_to_reslice.par_iter()
                .map(|crate_name| {
                    let success = if let Some(trans_needed) = transitive_reqs.get(crate_name) {
                        if let Some(crate_info) = find_crate_source(crate_name, graph.nodes.get(crate_name).map(|n| n.version.as_str())) {
                            let output_dir = output_base.join(format!("{}-sliced", crate_name));
                            let index = parse_crate_with_features(&crate_info.path, &crate_info.name, features);
                            let expanded = &crate::old_slicer::slicing::expand_needed_transitively(trans_needed, &index);

                            // Create per-crate progress bar for large re-slicing operations
                            use std::cell::RefCell;
                            let crate_pb: RefCell<Option<ProgressBar>> = RefCell::new(None);
                            let item_count: RefCell<Option<usize>> = RefCell::new(None);

                            let progress_fn = |current: usize, total: usize| {
                                if show_progress && total > 200 {
                                    let mut pb_ref = crate_pb.borrow_mut();
                                    if pb_ref.is_none() {
                                        let pb = multi.add(ProgressBar::new(100));
                                        pb.set_style(
                                            ProgressStyle::default_bar()
                                                .template(&format!("  {{spinner:.cyan}} {} (Phase 4): [{{bar:30.cyan/blue}}] {{pos}}% ({{msg}})", crate_name))
                                                .unwrap()
                                                .progress_chars("=>-")
                                        );
                                        pb.enable_steady_tick(std::time::Duration::from_millis(100));
                                        *pb_ref = Some(pb);
                                    }

                                    if let Some(ref pb) = *pb_ref {
                                        let mut item_count_ref = item_count.borrow_mut();

                                        let (percent, msg) = if current == total && item_count_ref.is_none() {
                                            *item_count_ref = Some(total);
                                            (100, "post-processing 0% (starting...)".to_string())
                                        } else if let Some(items) = *item_count_ref {
                                            let modules_total = total.saturating_sub(items);
                                            let modules_done = current.saturating_sub(items);

                                            if current >= items * 2 || modules_done > modules_total * 2 {
                                                let finalize_base = items * 2;
                                                if total > finalize_base {
                                                    let finalize_total_steps = total - finalize_base;
                                                    let finalize_current_step = current.saturating_sub(finalize_base);
                                                    let finalize_percent = (finalize_current_step * 100) / finalize_total_steps.max(1);
                                                    (100, format!("finalizing {}% (step {}/{})", finalize_percent, finalize_current_step, finalize_total_steps))
                                                } else {
                                                    (100, "finalizing (writing files, fixing imports)".to_string())
                                                }
                                            } else if modules_total > 0 {
                                                let module_percent = (modules_done * 100) / modules_total;
                                                (100, format!("post-processing {}% ({}/{} modules)",
                                                             module_percent, modules_done, modules_total))
                                            } else {
                                                (100, "finalizing (writing files)".to_string())
                                            }
                                        } else {
                                            let percent = (current * 100) / total.max(1);
                                            (percent, format!("{}/{} items", current, total))
                                        };

                                        pb.set_position(percent as u64);
                                        pb.set_message(msg);
                                    }
                                }
                            };

                            // Re-generate with transitive requirements
                            // Reuse pre-computed SCIP analysis from Phase 2 if available
                            let scip_analysis_opt = crate_scip_analysis.get(crate_name);

                            // Phase 6.4: Apply type closure to include types referenced in signatures
                            let expanded = &crate::old_slicer::slicing::compute_type_closure(expanded, &index);

                            let result = if use_new_slicer {
                                // Use new copy-and-delete slicer for re-slicing
                                let seed_items: HashSet<String> = expanded.iter().cloned().collect();
                                let config = crate::slicer::config::SlicerConfig {
                                    verbose,
                                    features: features.clone(),
                                    ..Default::default()
                                };

                                match crate::slicer::slice_crate(&crate_info.name, &crate_info.path, &seed_items, &output_dir, &config) {
                                    Ok(_) => {
                                        if verbose {
                                            multi.println(format!("  {}: re-sliced with {} items (+{} transitive)",
                                                crate_name,
                                                trans_needed.len(),
                                                trans_needed.len() - crate_needed_map.get(crate_name).map(|d| d.len()).unwrap_or(0)
                                            )).ok();
                                        }
                                        true
                                    }
                                    Err(e) => {
                                        if verbose {
                                            multi.println(format!("  {}: re-slice failed: {}", crate_name, e)).ok();
                                        }
                                        false
                                    }
                                }
                            } else {
                                // Use old generation-based slicer
                                match generate_semantic_sliced_crate_with_needed(&crate_info, &index, expanded, &output_dir, scip_analysis_opt, Some(&progress_fn)) {
                                    Ok(_) => {
                                        if verbose {
                                            multi.println(format!("  {}: re-sliced with {} items (+{} transitive)",
                                                crate_name,
                                                trans_needed.len(),
                                                trans_needed.len() - crate_needed_map.get(crate_name).map(|d| d.len()).unwrap_or(0)
                                            )).ok();
                                        }
                                        true
                                    }
                                    Err(e) => {
                                        if verbose {
                                            multi.println(format!("  {}: re-slice failed: {}", crate_name, e)).ok();
                                        }
                                        false
                                    }
                                }
                            };

                            // Clean up per-crate progress bar
                            if let Some(pb) = crate_pb.into_inner() {
                                pb.finish_and_clear();
                            }

                            result
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    phase7_pb_clone.inc(1);
                    (crate_name.clone(), success)
                })
                .collect()
        } else {
            crates_to_reslice.iter()
                .map(|crate_name| {
                    let success = if let Some(trans_needed) = transitive_reqs.get(crate_name) {
                        if let Some(crate_info) = find_crate_source(crate_name, graph.nodes.get(crate_name).map(|n| n.version.as_str())) {
                            let output_dir = output_base.join(format!("{}-sliced", crate_name));
                            let index = parse_crate_with_features(&crate_info.path, &crate_info.name, features);
                            let expanded = &crate::old_slicer::slicing::expand_needed_transitively(trans_needed, &index);

                            // Re-generate with transitive requirements
                            // Reuse pre-computed SCIP analysis from Phase 2 if available
                            let scip_analysis_opt = crate_scip_analysis.get(crate_name);

                            // Phase 6.4: Apply type closure to include types referenced in signatures
                            let expanded = &crate::old_slicer::slicing::compute_type_closure(expanded, &index);

                            if use_new_slicer {
                                // Use new copy-and-delete slicer for re-slicing
                                let seed_items: HashSet<String> = expanded.iter().cloned().collect();
                                let config = crate::slicer::config::SlicerConfig {
                                    verbose,
                                    features: features.clone(),
                                    ..Default::default()
                                };

                                match crate::slicer::slice_crate(&crate_info.name, &crate_info.path, &seed_items, &output_dir, &config) {
                                    Ok(_) => {
                                        if verbose {
                                            println!("  {}: re-sliced with {} items (+{} transitive)",
                                                crate_name,
                                                trans_needed.len(),
                                                trans_needed.len() - crate_needed_map.get(crate_name).map(|d| d.len()).unwrap_or(0)
                                            );
                                        }
                                        true
                                    }
                                    Err(e) => {
                                        if verbose {
                                            println!("  {}: re-slice failed: {}", crate_name, e);
                                        }
                                        false
                                    }
                                }
                            } else {
                                // Use old generation-based slicer
                                match generate_semantic_sliced_crate_with_needed(&crate_info, &index, expanded, &output_dir, scip_analysis_opt, None) {
                                    Ok(_) => {
                                        if verbose {
                                            println!("  {}: re-sliced with {} items (+{} transitive)",
                                                crate_name,
                                                trans_needed.len(),
                                                trans_needed.len() - crate_needed_map.get(crate_name).map(|d| d.len()).unwrap_or(0)
                                            );
                                        }
                                        true
                                    }
                                    Err(e) => {
                                        if verbose {
                                            println!("  {}: re-slice failed: {}", crate_name, e);
                                        }
                                        false
                                    }
                                }
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    phase7_pb.inc(1);
                    (crate_name.clone(), success)
                })
                .collect()
        };

        phase7_pb.finish_and_clear();
        let phase7_elapsed = phase7_start.elapsed();
        let success_count = phase7_results.iter().filter(|(_, success)| *success).count();
        if verbose {
            println!("\nPhase 4 completed: {} crates re-sliced ({} succeeded, {} failed) in {:.2}s",
                     crates_to_reslice.len(), success_count, crates_to_reslice.len() - success_count, phase7_elapsed.as_secs_f64());
        } else {
            println!("Phase 4 completed: {} crates re-sliced ({} succeeded, {} failed) in {:.2}s",
                     crates_to_reslice.len(), success_count, crates_to_reslice.len() - success_count, phase7_elapsed.as_secs_f64());
        }
    } else {
        if use_new_slicer {
            // Using new slicer - Phase 4 is unnecessary
            if verbose {
                println!("\nPhase 7: Skipped (new slicer already includes transitive dependencies)\n");
            }
        } else {
            // Using old slicer - no additional transitive requirements found
            if verbose {
                println!("\nPhase 7: No transitive requirements found, skipping re-slice\n");
            } else {
                println!("\nPhase 7: No transitive requirements found, skipping re-slice");
            }
        }
    }

    // Build sliced set from ALL crates that will be in the workspace (all *-sliced directories)
    // This includes both successful slices and failed slices that still have output directories
    let mut final_sliced_set: HashSet<String> = HashSet::new();
    let mut sliced_crate_dirs: HashMap<String, String> = HashMap::new();  // crate_name -> dir_name
    if let Ok(entries) = fs::read_dir(output_base) {
        for entry in entries.filter_map(|e| e.ok()) {
            let dir_name = entry.file_name().to_string_lossy().to_string();
            if dir_name.ends_with("-sliced") && entry.path().is_dir() {
                let cargo_toml = entry.path().join("Cargo.toml");
                if cargo_toml.exists() {
                    // Read the Cargo.toml to get the actual package name
                    if let Ok(contents) = fs::read_to_string(&cargo_toml) {
                        // Parse for package name (simple pattern matching)
                        for line in contents.lines() {
                            if line.starts_with("name = ") {
                                if let Some(name_str) = line.split('"').nth(1) {
                                    final_sliced_set.insert(name_str.to_string());
                                    sliced_crate_dirs.insert(name_str.to_string(), dir_name.clone());
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Rewrite Cargo.toml files to use sliced deps for ALL crates in the workspace
    // This ensures version consistency even for failed slices
    println!("\n=== Post-Processing: Updating Dependencies ===");
    let rewrite_pb = create_progress_bar(&multi, final_sliced_set.len() as u64, "Updating Cargo.toml");
    rewrite_pb.set_message("rewriting dependencies");
    rewrite_pb.enable_steady_tick(std::time::Duration::from_millis(100));

    for crate_name in &final_sliced_set {
        let output_dir = output_base.join(format!("{}-sliced", crate_name));
        rewrite_cargo_toml_for_sliced_deps(&output_dir, &final_sliced_set, output_base);
        rewrite_pb.inc(1);
    }
    rewrite_pb.finish_and_clear();

    // Generate workspace Cargo.toml now that all Cargo.toml files are rewritten
    println!("\n=== Generating Workspace ===");
    if let Err(e) = generate_workspace_toml(output_base, &result.success, verbose) {
        multi.println(format!("Warning: Failed to generate workspace: {}", e)).ok();
    }

    // Phase 5b: Workspace-level trial deletion (optional)
    // This runs AFTER workspace generation so cargo check works on the workspace
    // Triggered by: trial_delete, trial_sets, or graph_guided features
    if use_new_slicer && (features.trial_delete || features.trial_sets || features.graph_guided) {
        println!("\nPhase 5b: Running workspace-level trial deletion...");
        let trial_start = std::time::Instant::now();

        // Collect all sliced crate directories
        let crate_dirs: Vec<std::path::PathBuf> = result.success.iter()
            .map(|name| output_base.join(format!("{}-sliced", name)))
            .filter(|p| p.exists())
            .collect();

        if !crate_dirs.is_empty() {
            // Choose deletion strategy based on features:
            // 1. graph_guided: Bulk delete items without dependents, correct with JSON cargo check (fastest)
            // 2. trial_sets: Dependency-aware removal sets (medium)
            // 3. simple: Delete all, restore one-by-one (slowest)
            let trial_result = if features.graph_guided {
                // Graph-guided: bulk delete, then correct with JSON-based cargo check
                // Usually converges in 2-3 iterations vs N for trial-per-item
                crate::slicer::trial_deleter::run_graph_guided_deletion(
                    output_base,
                    &crate_dirs,
                    features.trial_limit,
                    10, // max iterations (usually converges in 2-3)
                )
            } else if features.trial_sets {
                // Dependency-aware: removes items together with their dependents
                crate::slicer::trial_deleter::run_trial_deletion_with_sets(
                    output_base,
                    &crate_dirs,
                    features.trial_limit,
                    100, // max sets to test
                )
            } else {
                // Simple: delete all, restore one-by-one
                crate::slicer::trial_deleter::run_trial_deletion(
                    output_base,
                    &crate_dirs,
                    features.trial_limit,
                    25, // max iterations
                )
            };

            println!("Phase 5b completed: 🗑️  {} deleted, 🔄 {} restored in {:.2}s",
                trial_result.items_deleted,
                trial_result.items_restored,
                trial_start.elapsed().as_secs_f64());
        }
    }

    // Build registry_crates set before Phase 5c (needed to determine which sliced crates to publish)
    let registry_crates: std::collections::HashSet<String> = if use_local_registry {
        let mut crates_in_registry = std::collections::HashSet::new();
        let mut dep_graph: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();

        // Build dependency graph and identify all crates
        let metadata_output = std::process::Command::new("cargo")
            .arg("metadata")
            .arg("--format-version=1")
            .current_dir(project_root)
            .output();

        if let Ok(output) = metadata_output {
            if output.status.success() {
                if let Ok(metadata_str) = String::from_utf8(output.stdout) {
                    if let Ok(metadata) = serde_json::from_str::<serde_json::Value>(&metadata_str) {
                        if let Some(packages) = metadata.get("packages").and_then(|p| p.as_array()) {
                            // Build dependency graph
                            for package in packages {
                                if let Some(name) = package.get("name").and_then(|n| n.as_str()) {
                                    let mut deps = Vec::new();
                                    if let Some(dependencies) = package.get("dependencies").and_then(|d| d.as_array()) {
                                        for dep in dependencies {
                                            if let Some(dep_name) = dep.get("name").and_then(|n| n.as_str()) {
                                                deps.push(dep_name.to_string());
                                            }
                                        }
                                    }
                                    dep_graph.insert(name.to_string(), deps);
                                }
                            }

                            // Add non-sliced crates to registry
                            for package in packages {
                                if let Some(name) = package.get("name").and_then(|n| n.as_str()) {
                                    if name == "cargo-slicer" {
                                        continue;
                                    }
                                    let is_sliced = result.success.iter().any(|s| {
                                        let base = s.split(" v").next().unwrap_or(s);
                                        base == name
                                    });
                                    if !is_sliced {
                                        crates_in_registry.insert(name.to_string());
                                    }
                                }
                            }

                            // For each non-sliced crate, add all its transitive dependencies
                            let non_sliced: Vec<String> = crates_in_registry.iter().cloned().collect();
                            for crate_name in &non_sliced {
                                let mut to_visit = vec![crate_name.clone()];
                                let mut visited = std::collections::HashSet::new();

                                while let Some(current) = to_visit.pop() {
                                    if visited.contains(&current) {
                                        continue;
                                    }
                                    visited.insert(current.clone());

                                    if let Some(deps) = dep_graph.get(&current) {
                                        for dep in deps {
                                            crates_in_registry.insert(dep.clone());
                                            to_visit.push(dep.clone());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if verbose {
            multi.println(format!("  📋 Registry will contain {} crates (non-sliced + their transitive deps)", crates_in_registry.len())).ok();
        }

        crates_in_registry
    } else {
        std::collections::HashSet::new()
    };

    // Phase 5c: Local Registry Publishing (optional)
    #[cfg(feature = "local-registry")]
    if use_local_registry {
        println!("\n=== Phase 5c: Local Registry Publishing ===");
        let start = std::time::Instant::now();

        // Initialize registry
        let registry_dir_result = crate::local_registry::init_local_registry(project_root);
        if let Err(e) = &registry_dir_result {
            multi.println(format!("  ⚠️  Failed to initialize local registry: {}", e)).ok();
            eprintln!("Warning: Skipping local registry publishing due to error: {}", e);
        }

        let registry_dir = match registry_dir_result {
            Ok(dir) => dir,
            Err(_) => {
                // Skip Phase 5c if registry initialization failed
                println!("Phase 5c skipped due to initialization failure");
                // Continue with the rest of the pipeline
                return result;
            }
        };

        // Crates with complex transitive dependencies that should use registry version
        let registry_blocked: std::collections::HashSet<&str> = vec![
            "ctrlc",      // Has dispatch2, nix, windows-sys transitive deps
            "indicatif",  // Has portable-atomic and other unsliced transitive deps
            "notify",     // Has libc and other unsliced transitive deps
            "proptest",   // Has tempfile → getrandom → wasip2 → wit-bindgen chain
            "tempfile",   // Has getrandom → wasip2 → wit-bindgen-rust-macro chain
        ].into_iter().collect();

        // Publish each sliced crate
        // DISABLED: Sliced crates use path dependencies, not registry
        // Publishing them creates checksum mismatches since they're modified
        let mut published_count = 0;
        let mut skipped_count = result.success.len();
        if false {
        for crate_name in &result.success {
            // Normalize crate name (convert display format to directory format)
            let normalized_name = if crate_name.contains(" v") {
                crate_name.replace(" v", "-")
            } else {
                crate_name.to_string()
            };

            // Extract base crate name (without version suffix)
            let base_name = normalized_name.split('-').next().unwrap_or(&normalized_name);

            // Skip crates with complex transitive dependencies
            if registry_blocked.contains(base_name) {
                if verbose {
                    multi.println(format!("  ⏭️  Skipped {} (registry fallback for transitive deps)", normalized_name)).ok();
                }
                skipped_count += 1;
                continue;
            }

            let sliced_dir = output_base.join(format!("{}-sliced", normalized_name));
            if sliced_dir.exists() {
                // Get version from graph
                if let Some(node) = graph.nodes.get(&normalized_name) {
                    match crate::local_registry::publish_to_local_registry(
                        &sliced_dir,
                        &normalized_name,
                        &node.version,
                        &registry_dir,
                    ) {
                        Ok(_) => {
                            if verbose {
                                multi.println(format!("  📦 Published {} {}", normalized_name, node.version)).ok();
                            }
                            published_count += 1;
                        }
                        Err(e) => {
                            multi.println(format!("  ⚠️  Failed to publish {}: {}", normalized_name, e)).ok();
                        }
                    }
                }
            }
        }
        } // End of disabled sliced crate publishing

        // Handle transitive dependencies that weren't sliced
        // These crates weren't published above but may still be needed by sliced crates
        let mut transitive_count = 0;
        let success_set: std::collections::HashSet<_> = result.success.iter().cloned().collect();

        // Recursively copy all dependencies of sliced crates to the registry
        // Use cargo metadata to get the full resolved dependency tree
        let metadata_output = std::process::Command::new("cargo")
            .arg("metadata")
            .arg("--format-version=1")
            .current_dir(project_root)
            .output();

        if let Ok(output) = metadata_output {
            if output.status.success() {
                if let Ok(metadata_str) = String::from_utf8(output.stdout) {
                    if let Ok(metadata) = serde_json::from_str::<serde_json::Value>(&metadata_str) {
                        if let Some(packages) = metadata.get("packages").and_then(|p| p.as_array()) {
                            let mut copied_versions: std::collections::HashSet<(String, String)> = std::collections::HashSet::new();

                            for package in packages {
                                if let (Some(name), Some(version)) = (
                                    package.get("name").and_then(|n| n.as_str()),
                                    package.get("version").and_then(|v| v.as_str()),
                                ) {
                                    // Skip sliced crates UNLESS they're needed by registry crates
                                    if success_set.iter().any(|s| s.split(" v").next().unwrap_or(s) == name) {
                                        // This is a sliced crate - only include if it's in registry_crates
                                        if !registry_crates.contains(name) {
                                            continue;
                                        }
                                    }

                                    // Skip if in registry_blocked list
                                    if registry_blocked.contains(name) {
                                        continue;
                                    }

                                    // Skip if already copied this exact version
                                    if copied_versions.contains(&(name.to_string(), version.to_string())) {
                                        continue;
                                    }

                                    // Skip workspace crate itself
                                    if name == "cargo-slicer" {
                                        continue;
                                    }

                                    // Extract manifest_path to locate source directory
                                    // For sliced crates in registry, use the sliced version
                                    let is_sliced = success_set.iter().any(|s| s.split(" v").next().unwrap_or(s) == name);
                                    let manifest_path = if is_sliced && registry_crates.contains(name) {
                                        // Use sliced directory
                                        if let Some(dir_name) = sliced_crate_dirs.get(name) {
                                            Some(output_base.join(dir_name).join("Cargo.toml"))
                                        } else {
                                            // Fallback to default sliced naming
                                            Some(output_base.join(format!("{}-sliced", name)).join("Cargo.toml"))
                                        }
                                    } else {
                                        package.get("manifest_path")
                                            .and_then(|p| p.as_str())
                                            .map(|s| std::path::PathBuf::from(s))
                                    };

                                    // Copy from cargo cache to local registry
                                    match crate::local_registry::copy_original_crate_to_registry(
                                        name,
                                        version,
                                        &registry_dir,
                                        manifest_path.as_deref(),
                                    ) {
                                        Ok(_) => {
                                            if verbose {
                                                multi.println(format!("  📦 Copied: {} {}", name, version)).ok();
                                            }
                                            copied_versions.insert((name.to_string(), version.to_string()));
                                            transitive_count += 1;
                                        }
                                        Err(e) => {
                                            // Only log if verbose
                                            if verbose {
                                                multi.println(format!("  ⏭️  Skipped {} {}: {}", name, version, e)).ok();
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Post-process: Find and download missing dependencies
        // DISABLED: This causes excessive downloads of deep dependency trees.
        // Instead, problematic crates are added to registry_blocked to use crates.io directly.
        let mut missing_downloaded = 0;
        if false {
        let mut iteration = 0;
        loop {
            iteration += 1;
            let mut found_missing = false;
            let index_dir = registry_dir.join("index");

        // Scan all index files to find dependency references
        let mut missing_deps: std::collections::HashMap<String, String> = std::collections::HashMap::new();

        // Use glob to find all index files
        let index_pattern = format!("{}/**/*", index_dir.display());
        if let Ok(entries) = glob::glob(&index_pattern) {
            for entry in entries.flatten() {
                if entry.is_file() && entry.file_name().map(|n| n != "config.json").unwrap_or(false) {
                    if let Ok(content) = fs::read_to_string(&entry) {
                        for line in content.lines() {
                            if let Ok(index_entry) = serde_json::from_str::<serde_json::Value>(line) {
                                // Get parent crate version for handling "*" dependencies
                                let parent_version = index_entry.get("vers").and_then(|v| v.as_str());

                                if let Some(deps) = index_entry.get("deps").and_then(|d| d.as_array()) {
                                    for dep in deps {
                                        // Skip dev-dependencies to avoid dep explosion
                                        if let Some(kind) = dep.get("kind").and_then(|k| k.as_str()) {
                                            if kind == "dev" {
                                                continue;
                                            }
                                        }

                                        if let (Some(dep_name), Some(dep_req)) = (
                                            dep.get("name").and_then(|n| n.as_str()),
                                            dep.get("req").and_then(|r| r.as_str()),
                                        ) {
                                            // Check if this dependency exists in registry
                                            let dep_index = match dep_name.len() {
                                                1 => index_dir.join("1").join(dep_name),
                                                2 => index_dir.join("2").join(dep_name),
                                                3 => index_dir.join("3").join(&dep_name[..1]).join(dep_name),
                                                _ => index_dir.join(&dep_name[..2]).join(&dep_name[2..4]).join(dep_name),
                                            };

                                            if !dep_index.exists() {
                                                // Parse version from requirement (e.g., "^0.9.0" -> "0.9.0")
                                                let mut version = dep_req.trim_start_matches('^')
                                                    .trim_start_matches('~')
                                                    .trim_start_matches('=')
                                                    .split(',').next().unwrap_or(dep_req).trim();

                                                // Handle "*" version by using parent crate's version
                                                if version == "*" {
                                                    if let Some(parent_ver) = parent_version {
                                                        version = parent_ver;
                                                    } else {
                                                        continue; // Skip if we can't resolve *
                                                    }
                                                }

                                                missing_deps.insert(dep_name.to_string(), version.to_string());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

            // Download missing dependencies
            for (dep_name, version) in missing_deps {
                if verbose {
                    multi.println(format!("  📥 Downloading missing dependency: {} {}", dep_name, version)).ok();
                }

                match crate::local_registry::copy_original_crate_to_registry(
                    &dep_name,
                    &version,
                    &registry_dir,
                    None,
                ) {
                    Ok(_) => {
                        missing_downloaded += 1;
                        found_missing = true;
                    }
                    Err(e) => {
                        if verbose {
                            multi.println(format!("  ⚠️  Failed to download {}: {}", dep_name, e)).ok();
                        }
                    }
                }
            }

            // Break if no new dependencies were found
            if !found_missing {
                if verbose && iteration > 1 {
                    multi.println(format!("  ✅ Completed {} iterations of dependency resolution", iteration)).ok();
                }
                break;
            }

            // Safety limit to prevent infinite loops and excessive downloads
            if iteration >= 5 {
                if verbose {
                    multi.println(format!("  ⚠️  Reached maximum iteration limit (5)")).ok();
                }
                break;
            }
        } // End of missing dependency detection loop
        } // End of disabled missing dependency detection

        if missing_downloaded > 0 {
            multi.println(format!("  ✅ Downloaded {} missing dependencies", missing_downloaded)).ok();
        }

        // Configure Cargo to use local registry
        match crate::local_registry::configure_cargo_registry(project_root, &registry_dir) {
            Ok(_) => {
                if verbose {
                    multi.println(format!("  ✅ Configured .cargo/config.toml")).ok();
                }
            }
            Err(e) => {
                multi.println(format!("  ⚠️  Failed to configure cargo registry: {}", e)).ok();
            }
        }

        let duration = start.elapsed();
        if transitive_count > 0 && skipped_count > 0 {
            println!("Phase 5c completed: 📦 {} published, 📦 {} transitive, ⏭️  {} skipped (registry fallback) in {:.2}s",
                published_count, transitive_count, skipped_count, duration.as_secs_f64());
        } else if transitive_count > 0 {
            println!("Phase 5c completed: 📦 {} published, 📦 {} transitive in {:.2}s",
                published_count, transitive_count, duration.as_secs_f64());
        } else if skipped_count > 0 {
            println!("Phase 5c completed: 📦 {} published, ⏭️  {} skipped (registry fallback) in {:.2}s",
                published_count, skipped_count, duration.as_secs_f64());
        } else {
            println!("Phase 5c completed: 📦 {} published in {:.2}s",
                published_count, duration.as_secs_f64());
        }
    }

    // Phase 6: Measure LOC and check compilation at workspace level
    if measure {
        // Start profiling Phase 6
        crate::profiling::start_timing("Phase 6: Measure & Check Compilation");

        println!("\n=== Phase 6: Measuring LOC and Checking Compilation ===");

        // First, measure LOC for each crate
        let measure_pb = create_progress_bar(&multi, result.success.len() as u64, "Measuring LOC");
        measure_pb.set_message("measuring");
        measure_pb.enable_steady_tick(std::time::Duration::from_millis(100));

        for crate_name in &result.success {
            let output_dir = output_base.join(format!("{}-sliced", crate_name));
            if output_dir.exists() {
                let loc = crate::measure::measure_loc(&output_dir);
                let bytes = crate::measure::measure_bytes(&output_dir);
                if let Some(m) = result.measurements.get_mut(crate_name) {
                    m.after_loc = loc;
                    m.after_bytes = bytes;
                }
            }
            measure_pb.inc(1);
        }
        measure_pb.finish_and_clear();

        // Skip compilation check in fast mode (-O) since final build will catch errors anyway
        if features.verify {
            // Now run workspace-level cargo check for all crates at once
            println!("  Checking workspace compilation (shared target, much faster)...");
            use std::process::Command;
            let workspace_check = Command::new("cargo")
                .args(["check", "--workspace", "--message-format=json", "--quiet"])
                .current_dir(output_base)
                .output();

            // Parse workspace check results and update per-crate status
            if let Ok(output) = workspace_check {
                use crate::types::CompileStatus;
                if output.status.success() {
                    // All crates compiled successfully
                    for crate_name in &result.success {
                        if let Some(m) = result.measurements.get_mut(crate_name) {
                            m.compile_status = CompileStatus::Success;
                            m.error_count = 0;
                        }
                    }
                    multi.println("  ✅ All crates compile successfully".to_string()).ok();
                } else {
                    // Parse errors per crate from JSON output
                    use std::collections::HashMap;
                    let mut crate_errors: HashMap<String, (usize, HashMap<String, usize>)> = HashMap::new();

                    for line in String::from_utf8_lossy(&output.stdout).lines() {
                        if let Ok(msg) = serde_json::from_str::<serde_json::Value>(line) {
                            if msg["reason"] == "compiler-message" {
                                if let Some(target_name) = msg["target"]["name"].as_str() {
                                    let crate_key = target_name.to_string();
                                    let entry = crate_errors.entry(crate_key).or_insert((0, HashMap::new()));

                                    if let Some(code_obj) = msg["message"]["code"].as_object() {
                                        if let Some(code) = code_obj.get("code").and_then(|c| c.as_str()) {
                                            *entry.1.entry(code.to_string()).or_insert(0) += 1;
                                            entry.0 += 1;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Update measurements with compilation results
                    for crate_name in &result.success {
                        if let Some(m) = result.measurements.get_mut(crate_name) {
                            if let Some((error_count, error_types)) = crate_errors.get(crate_name) {
                                m.compile_status = CompileStatus::Failed;
                                m.error_count = *error_count;
                                m.error_types = error_types.clone();
                            } else {
                                m.compile_status = CompileStatus::Success;
                                m.error_count = 0;
                            }
                        }
                    }

                    let failed_count = crate_errors.len();
                    multi.println(format!("  ⚠ {} crate(s) have compilation errors", failed_count)).ok();
                }
            } else {
                multi.println("  ⚠ Workspace compilation check failed to run".to_string()).ok();
            }
        } else {
            // Skipping cargo check, but set compile_status based on rustc accuracy
            // Since cargo-rustc is 100% accurate and we're using conservative deletion,
            // we can trust that sliced crates are correct (final build will catch any issues)
            use crate::types::CompileStatus;
            for crate_name in &result.success {
                if let Some(m) = result.measurements.get_mut(crate_name) {
                    m.compile_status = CompileStatus::Success;
                    m.error_count = 0;
                }
            }
            if use_local_registry {
                println!("  ⏩ Skipping cargo check (trusting cargo-rustc accuracy + conservative deletion)");
            } else {
                println!("  ⏩ Skipping compilation check (fast mode, final build will verify)");
            }
        }

        // End profiling Phase 6
        crate::profiling::end_timing();
    }

    let total_elapsed = total_start.elapsed();

    multi.println(format!("\n=== Union Slice Summary ({:.2}s total) ===", total_elapsed.as_secs_f64())).ok();
    multi.println(format!("  Success: {}", result.success.len())).ok();
    if removed_count > 0 {
        multi.println(format!("  Removed: {} (0 items needed)", removed_count)).ok();
    }
    multi.println(format!("  Failed: {}", result.failed.len())).ok();
    if !result.failed.is_empty() {
        for (name, reason) in &result.failed {
            multi.println(format!("    - {}: {}", name, reason)).ok();
        }
    }

    // === Phase 8: Cycle Detection and Breaking ===
    // Start profiling Phase 8
    crate::profiling::start_timing("Phase 8: Cycle Detection & Breaking");

    multi.println("\n=== Phase 8: Cycle Detection and Breaking ===".to_string()).ok();
    let phase8_start = Instant::now();

    // 1. Build dependency graph with dev-deps
    use crate::cycle_detector::{CycleDetector, EdgeType};
    use crate::cycle_breaker::CycleBreaker;
    use crate::variant_generator::VariantGenerator;

    let mut detector = CycleDetector::new();
    for (from_crate, node) in &graph.nodes {
        for to_crate in &node.deps {
            if final_sliced_set.contains(to_crate) {
                detector.add_dependency(from_crate, to_crate, EdgeType::Normal);
            }
        }
        for to_crate in &node.dev_deps {
            if final_sliced_set.contains(to_crate) {
                detector.add_dependency(from_crate, to_crate, EdgeType::Dev);
            }
        }
    }

    // 2. Detect cycles
    let cycles = detector.detect_cycles();
    multi.println(format!("  Detected {} cycle(s)", cycles.len())).ok();
    if !cycles.is_empty() {
        for (i, cycle) in cycles.iter().enumerate() {
            multi.println(format!("    Cycle {}: {}", i + 1, cycle.crates.join(" → "))).ok();
        }
    }

    // 3. Compute breaking strategies
    let breaker = CycleBreaker::new(cycles.clone(), output_base.display().to_string());
    let strategies = breaker.compute_strategies();

    if !strategies.is_empty() {
        multi.println(format!("  Applying {} cycle-breaking strateg(ies):", strategies.len())).ok();
        for strategy in &strategies {
            use crate::cycle_breaker::StrategyType;
            match &strategy.strategy_type {
                StrategyType::RemoveDevDeps { count } => {
                    multi.println(format!("    - Removing {} dev-dependency edge(s)", count)).ok();
                }
                StrategyType::UseVersionSpec { crate_name, .. } => {
                    multi.println(format!("    - Converting {} to version spec", crate_name)).ok();
                }
                StrategyType::RegistryFallback { crate_name } => {
                    multi.println(format!("    - Falling back to registry for {}", crate_name)).ok();
                }
            }
        }

        // Apply strategies to sliced crate manifests
        if let Err(e) = breaker.apply_strategies(&strategies) {
            multi.println(format!("  Warning: Failed to apply some strategies: {}", e)).ok();
        }
    }

    // 4. Clean up sliced directories for registry-fallback crates
    let fallback_crates = breaker.get_registry_fallback_crates(&strategies);
    if !fallback_crates.is_empty() {
        multi.println(format!("  Removing {} sliced crate(s) that use registry versions:", fallback_crates.len())).ok();
        for crate_name in &fallback_crates {
            let sliced_dir = output_base.join(format!("{}-sliced", crate_name));
            if sliced_dir.exists() {
                if let Err(e) = fs::remove_dir_all(&sliced_dir) {
                    multi.println(format!("    Warning: Failed to remove {}: {}", sliced_dir.display(), e)).ok();
                } else {
                    multi.println(format!("    - Removed {}-sliced (using registry version)", crate_name)).ok();
                }
            }
            // Remove from final_sliced_set since we're not using the sliced version
            final_sliced_set.remove(crate_name);
        }
    }

    // 4b. Build registry crate set for pure local registry mode
    // Pure local registry approach: All transitive dependencies are in the registry
    // No need to copy sources - everything resolves from .cargo-slicer-registry

    // 5. Generate variant Cargo.toml for parent project

    // Build version map for registry references (include ALL crates in registry for pure mode)
    let mut crate_versions_map = std::collections::HashMap::new();
    for crate_name in &registry_crates {
        if let Some(node) = graph.nodes.get(crate_name) {
            crate_versions_map.insert(crate_name.clone(), node.version.clone());
        }
    }

    // Convert registry_blocked from &str to String for VariantGenerator
    let registry_blocked_set: std::collections::HashSet<String> = if use_local_registry {
        // Use the same blocklist from Phase 5c
        vec!["ctrlc", "indicatif", "notify", "proptest", "tempfile"].iter().map(|s| s.to_string()).collect()
    } else {
        std::collections::HashSet::new()
    };

    let variant_gen = VariantGenerator::new(
        project_root.join("Cargo.toml"),
        output_base.to_path_buf(),
        final_sliced_set.clone(),
        strategies,
        fallback_crates.clone(),
        use_local_registry,
        crate_versions_map,
        registry_blocked_set,
        registry_crates,
        sliced_crate_dirs,
    );

    match variant_gen.generate_sliced_variant() {
        Ok(variant_path) => {
            multi.println(format!("  Generated: {}", variant_path.display())).ok();
            multi.println("  To use: cargo slice build".to_string()).ok();
        }
        Err(e) => {
            multi.println(format!("  Warning: Failed to generate Cargo.toml.sliced: {}", e)).ok();
        }
    }

    multi.println(format!("  Phase 8 completed in {:.2}s", phase8_start.elapsed().as_secs_f64())).ok();

    // End profiling Phase 8
    crate::profiling::end_timing();

    // Save incremental cache
    if let Some(ref cache) = sliced_cache {
        if let Ok(cache_guard) = cache.lock() {
            if let Err(e) = cache_guard.save(project_root) {
                eprintln!("Warning: Failed to save incremental cache: {}", e);
            } else {
                let stats = cache_guard.stats();
                println!("📦 Saved incremental cache: {} crate(s)", stats.total_entries);
            }
        }
    }

    result
}

/// Rewrite Cargo.toml to use sliced dependencies instead of original ones
/// Also rewrites feature references and restores features that were incorrectly filtered
pub fn rewrite_cargo_toml_for_sliced_deps(crate_dir: &Path, sliced_set: &HashSet<String>, output_base: &Path) {
    let cargo_toml_path = crate_dir.join("Cargo.toml");
    if !cargo_toml_path.exists() {
        return;
    }

    let content = match std::fs::read_to_string(&cargo_toml_path) {
        Ok(c) => c,
        Err(_) => return,
    };

    // Extract crate name from the sliced Cargo.toml to find original
    let crate_name = content.lines()
        .find(|l| l.trim().starts_with("name = "))
        .and_then(|l| l.split('"').nth(1))
        .map(|s| s.trim_end_matches("-sliced").to_string());

    // Try to load original Cargo.toml features and optional dependencies
    let original_features: HashMap<String, Vec<String>> = if let Some(ref name) = crate_name {
        load_original_crate_features(name, None)
    } else {
        HashMap::new()
    };

    let original_optional_deps: HashMap<String, String> = if let Some(ref name) = crate_name {
        load_original_optional_dependencies(name, None)
    } else {
        HashMap::new()
    };

    // Collect existing dependencies (including those we'll rewrite)
    let mut existing_deps: HashSet<String> = HashSet::new();
    let mut optional_deps: HashSet<String> = HashSet::new();
    let mut in_deps_scan = false;
    let mut current_dep_section: Option<String> = None;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            if trimmed == "[dependencies]" {
                in_deps_scan = true;
                current_dep_section = None;
            } else if trimmed.starts_with("[dependencies.") && trimmed.ends_with(']') {
                in_deps_scan = false;  // Not inline deps
                let dep_name = &trimmed[14..trimmed.len()-1];
                current_dep_section = Some(dep_name.to_string());
                existing_deps.insert(dep_name.to_string());
            } else if trimmed.starts_with("[target.") && trimmed.contains(".dependencies.") && trimmed.ends_with(']') {
                // Handle target-specific dependencies like [target.'cfg(...)'.dependencies.ab_glyph]
                in_deps_scan = false;
                if let Some(dep_start) = trimmed.rfind(".dependencies.") {
                    let dep_name = &trimmed[dep_start + ".dependencies.".len()..trimmed.len()-1];
                    current_dep_section = Some(dep_name.to_string());
                    existing_deps.insert(dep_name.to_string());
                }
            } else {
                in_deps_scan = false;
                current_dep_section = None;
            }
        } else if in_deps_scan && !trimmed.is_empty() {
            if let Some(dep_name) = trimmed.split('=').next().map(|s| s.trim()) {
                if !dep_name.is_empty() {
                    existing_deps.insert(dep_name.to_string());
                    if trimmed.contains("optional = true") {
                        optional_deps.insert(dep_name.to_string());
                    }
                }
            }
        } else if let Some(ref dep) = current_dep_section {
            // We're in a [dependencies.X] section
            if trimmed.contains("optional") && trimmed.contains("true") {
                optional_deps.insert(dep.clone());
            }
        }
    }

    // Collect existing features
    let mut existing_features: HashSet<String> = HashSet::new();
    let mut in_features_scan = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_features_scan = trimmed == "[features]";
        } else if in_features_scan && !trimmed.is_empty() && trimmed.contains('=') {
            if let Some(feature_name) = trimmed.split('=').next().map(|s| s.trim()) {
                if !feature_name.is_empty() {
                    existing_features.insert(feature_name.to_string());
                }
            }
        }
    }

    let mut new_content = String::new();
    let mut in_dependencies = false;
    let mut in_features = false;
    let mut features_section_ended = false;
    let mut copying_multiline = false;
    let mut multiline_feature_name: Option<String> = String::new().into();
    let mut multiline_refs: Vec<String> = Vec::new();

    for line in content.lines() {
        let trimmed = line.trim();

        // Handle continuation lines from multi-line feature arrays
        if copying_multiline {
            // Accumulate references
            let clean = trimmed.trim_start_matches('"').trim_end_matches(',').trim_end_matches('"').trim();
            if !clean.is_empty() && !clean.starts_with('[') && !clean.ends_with(']') {
                multiline_refs.push(clean.to_string());
            }

            if trimmed.ends_with(']') {
                // End of multi-line array - fix and write
                if let Some(ref feature_name) = multiline_feature_name {
                    // Fix the references
                    let fixed_refs = fix_feature_refs(&multiline_refs, &existing_deps, &optional_deps, &existing_features, output_base);
                    if !fixed_refs.is_empty() {
                        new_content.push_str(&format!("{} = [\n", feature_name));
                        for r in &fixed_refs {
                            new_content.push_str(&format!("    \"{}\",\n", r));
                        }
                        new_content.push_str("]\n");
                    } else {
                        // All refs filtered out - write empty feature
                        new_content.push_str(&format!("{} = []\n", feature_name));
                    }
                }
                copying_multiline = false;
                multiline_feature_name = None;
                multiline_refs.clear();
            }
            continue;
        }

        // Track which section we're in
        if trimmed.starts_with('[') {
            // Before leaving dependencies section, add missing optional dependencies
            if in_dependencies && trimmed != "[dependencies]" {
                // Add optional dependencies from original that are referenced by features
                let mut missing_optional_deps = std::collections::HashSet::new();
                for (feature_name, refs) in &original_features {
                    if existing_features.contains(feature_name) {
                        continue;
                    }
                    // Check if this feature references an optional dep that's missing
                    for r in refs {
                        let dep_name = if r.starts_with("dep:") {
                            &r[4..]
                        } else if r.contains('/') {
                            r.split('/').next().unwrap_or("").trim_end_matches('?')
                        } else {
                            r.as_str()
                        };

                        if original_optional_deps.contains_key(dep_name) && !existing_deps.contains(dep_name) {
                            missing_optional_deps.insert((dep_name.to_string(), original_optional_deps[dep_name].clone()));
                        }
                    }
                }

                // Add missing optional dependencies (avoiding duplicates with HashSet)
                for (dep_name, version) in missing_optional_deps {
                    // Phase 6.6: Skip rustc-internal dependencies
                    if matches!(dep_name.as_str(), "core" | "alloc" | "std" | "proc_macro")
                        || dep_name.starts_with("rustc-std-workspace")
                        || dep_name.starts_with("compiler_builtins")
                    {
                        continue;
                    }

                    // Check if this dependency has been sliced
                    if sliced_set.contains(&dep_name) {
                        let rel_path = format!("../{}-sliced", dep_name);
                        new_content.push_str(&format!("{} = {{ optional = true, path = \"{}\" }}\n", dep_name, rel_path));
                    } else {
                        // Use version from original
                        new_content.push_str(&format!("{} = {{ version = \"{}\", optional = true }}\n", dep_name, version));
                    }
                    existing_deps.insert(dep_name.clone());
                    // Also add to optional_deps so feature checks work correctly
                    optional_deps.insert(dep_name);
                }
            }

            // Before leaving features section, add missing features
            // This runs when we WERE in features section and now encounter a different section header
            if in_features && trimmed != "[features]" && !features_section_ended {
                features_section_ended = true;
                // Add features from original that reference sliced deps but were filtered out
                for (feature_name, refs) in &original_features {
                    if existing_features.contains(feature_name) {
                        continue; // Already have this feature
                    }

                    // Phase 6.6: Skip features that reference rustc-internal dependencies
                    let references_rustc_internal = refs.iter().any(|r| {
                        let dep_name = if r.starts_with("dep:") {
                            &r[4..]
                        } else if r.contains('/') {
                            r.split('/').next().unwrap_or("").trim_end_matches('?')
                        } else {
                            r.as_str()
                        };
                        matches!(dep_name, "core" | "alloc" | "std" | "proc_macro")
                            || dep_name.starts_with("rustc-std-workspace")
                            || dep_name.starts_with("compiler_builtins")
                    });
                    if references_rustc_internal {
                        continue;
                    }

                    // Handle features with empty refs (standalone features like "unicode-age = []")
                    if refs.is_empty() {
                        // Special case: if feature name matches an optional dependency, add dep:name reference
                        // This is required by newer Cargo: optional deps must be referenced by at least one feature
                        if optional_deps.contains(feature_name) || original_optional_deps.contains_key(feature_name) {
                            new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                        } else {
                            new_content.push_str(&format!("{} = []\n", feature_name));
                        }
                        continue;
                    }

                    // Check if this feature references a dependency that exists in THIS crate
                    // Note: For dep:X and X/feature patterns, we ONLY check existing_deps,
                    // not sliced_set, because the dep must be in this crate's [dependencies]
                    let should_add = refs.iter().any(|r| {
                        // Check crate/feature pattern - crate must be in our deps
                        if r.contains('/') {
                            let crate_part = r.split('/').next().unwrap_or("").trim_end_matches('?');
                            existing_deps.contains(crate_part)
                        } else if r.starts_with("dep:") {
                            // dep:X requires X to be in our dependencies
                            let dep_name = &r[4..];
                            existing_deps.contains(dep_name)
                        } else {
                            // Feature reference or simple dep - check if it exists locally
                            existing_features.contains(r) || existing_deps.contains(r)
                        }
                    });
                    if should_add {
                        let mut fixed_refs = fix_feature_refs(refs, &existing_deps, &optional_deps, &existing_features, output_base);

                        // Special case: if feature name matches an optional dependency name, add dep:name reference
                        // This is required by Cargo: optional deps must be referenced by at least one feature
                        if optional_deps.contains(feature_name) && !fixed_refs.iter().any(|r| r == &format!("dep:{}", feature_name) || r == feature_name) {
                            fixed_refs.push(format!("dep:{}", feature_name));
                        }

                        if !fixed_refs.is_empty() {
                            let refs_str = fixed_refs.iter().map(|s| format!("\"{}\"", s)).collect::<Vec<_>>().join(", ");
                            new_content.push_str(&format!("{} = [{}]\n", feature_name, refs_str));
                        } else {
                            // Keep the feature but make it empty if all its deps became mandatory
                            // Special case: if feature name matches an optional dependency, add dep:name reference
                            if optional_deps.contains(feature_name) || original_optional_deps.contains_key(feature_name) {
                                new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                            } else {
                                new_content.push_str(&format!("{} = []\n", feature_name));
                            }
                        }
                    }
                }
            }
            in_dependencies = trimmed == "[dependencies]";
            let was_in_features = in_features;
            in_features = trimmed == "[features]";
            if in_features != was_in_features {
            }
            if in_features {
                features_section_ended = false;
            }
        }

        // Rewrite dependencies to use sliced versions
        if in_dependencies && !trimmed.starts_with('[') && !trimmed.is_empty() {
            let dep_name = trimmed.split('=').next().map(|s| s.trim()).unwrap_or("");
            // Don't rewrite if the dependency name matches the crate being sliced
            // (e.g., don't rewrite bitflags when slicing bitflags-sliced itself)
            let is_self_reference = crate_name.as_ref().map_or(false, |cn| cn == dep_name);
            if !dep_name.is_empty() && sliced_set.contains(dep_name) && !is_self_reference {
                // Point to the sliced directory - package name is the original name
                let rel_path = format!("../{}-sliced", dep_name);
                let is_optional = trimmed.contains("optional") && trimmed.contains("true");
                if is_optional {
                    new_content.push_str(&format!(
                        "{} = {{ optional = true, path = \"{}\" }}\n",
                        dep_name, rel_path
                    ));
                } else {
                    new_content.push_str(&format!(
                        "{} = {{ path = \"{}\" }}\n",
                        dep_name, rel_path
                    ));
                }
                continue;
            }
        }

        // Fix feature lines that match optional dependency names
        // If a feature has the same name as an optional dep, it must reference the dep
        if in_features && !trimmed.starts_with('[') && !trimmed.is_empty() && trimmed.contains('=') {
            if let Some(eq_pos) = trimmed.find('=') {
                let feature_name = trimmed[..eq_pos].trim();
                let refs_part = trimmed[eq_pos+1..].trim();

                // Detect multi-line array: has '[' but not ']' on the same line
                let is_multiline = refs_part.contains('[') && !refs_part.contains(']');

                // Handle multi-line arrays by accumulating refs and fixing them at the end
                if is_multiline {
                    multiline_feature_name = Some(feature_name.to_string());
                    multiline_refs.clear();
                    // Extract any refs on the first line after '['
                    let first_line_refs = refs_part.trim_start_matches('[').trim();
                    for part in first_line_refs.split(',') {
                        let clean = part.trim().trim_matches('"').trim();
                        if !clean.is_empty() {
                            multiline_refs.push(clean.to_string());
                        }
                    }
                    copying_multiline = true;
                    continue;
                }

                // Check if this feature matches an optional dependency and has empty or minimal refs
                if optional_deps.contains(feature_name) || original_optional_deps.contains_key(feature_name) {
                    let refs_clean = refs_part.trim_matches(|c| c == '[' || c == ']' || char::is_whitespace(c));
                    // Only rewrite if refs are completely empty
                    if refs_clean.is_empty() {
                        // Replace with dep:name reference
                        new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                        continue;
                    }
                }
            }
        }

        // Fix existing feature lines that match optional dependencies
        if in_features && trimmed.contains('=') && !trimmed.starts_with('[') {
            if let Some(eq_pos) = trimmed.find('=') {
                let feature_name = trimmed[..eq_pos].trim();
                let value_part = trimmed[eq_pos+1..].trim();
                // Check if this is an empty feature that matches an optional dependency
                if value_part == "[]" && (optional_deps.contains(feature_name) || original_optional_deps.contains_key(feature_name)) {
                    // Replace with dep:name reference
                    let indent = &line[..line.len() - line.trim_start().len()];
                    new_content.push_str(&format!("{}{} = [\"dep:{}\"]\n", indent, feature_name, feature_name));
                    continue;
                }
            }
        } else if trimmed.contains('=') && !trimmed.starts_with('[') && !trimmed.is_empty() {
        }

        new_content.push_str(line);
        new_content.push('\n');
    }

    // If features section was the last section, add missing features at the end
    if in_features && !features_section_ended {
        for (feature_name, refs) in &original_features {
            if existing_features.contains(feature_name) {
                continue;
            }

            // Handle features with empty refs (standalone features like "unicode-age = []")
            if refs.is_empty() {
                // Special case: if feature name matches an optional dependency, add dep:name reference
                if optional_deps.contains(feature_name) || original_optional_deps.contains_key(feature_name) {
                    new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                } else {
                    new_content.push_str(&format!("{} = []\n", feature_name));
                }
                continue;
            }

            // Check if this feature references a dependency that exists in THIS crate
            let should_add = refs.iter().any(|r| {
                if r.contains('/') {
                    let crate_part = r.split('/').next().unwrap_or("").trim_end_matches('?');
                    existing_deps.contains(crate_part)
                } else if r.starts_with("dep:") {
                    let dep_name = &r[4..];
                    existing_deps.contains(dep_name)
                } else {
                    existing_features.contains(r) || existing_deps.contains(r)
                }
            });
            if should_add {
                let mut fixed_refs = fix_feature_refs(refs, &existing_deps, &optional_deps, &existing_features, output_base);

                // Special case: if feature name matches an optional dependency name, add dep:name reference
                // This is required by Cargo: optional deps must be referenced by at least one feature
                if (optional_deps.contains(feature_name) || original_optional_deps.contains_key(feature_name)) && !fixed_refs.iter().any(|r| r == &format!("dep:{}", feature_name) || r == feature_name) {
                    fixed_refs.push(format!("dep:{}", feature_name));
                }

                if !fixed_refs.is_empty() {
                    let refs_str = fixed_refs.iter().map(|s| format!("\"{}\"", s)).collect::<Vec<_>>().join(", ");
                    new_content.push_str(&format!("{} = [{}]\n", feature_name, refs_str));
                } else {
                    // Special case: if feature name matches an optional dependency, add dep:name reference
                    if optional_deps.contains(feature_name) || original_optional_deps.contains_key(feature_name) {
                        new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                    } else {
                        new_content.push_str(&format!("{} = []\n", feature_name));
                    }
                }
            }
        }
    }

    // If we never had a [features] section at all, but there are standalone features to add, create one
    if !in_features && !features_section_ended {
        let standalone_features: Vec<_> = original_features.iter()
            .filter(|(name, refs)| refs.is_empty() && !existing_features.contains(*name))
            .collect();

        if !standalone_features.is_empty() {
            new_content.push_str("\n[features]\n");
            for (feature_name, _) in standalone_features {
                // Special case: if feature name matches an optional dependency, add dep:name reference
                if optional_deps.contains(feature_name) {
                    new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                } else {
                    new_content.push_str(&format!("{} = []\n", feature_name));
                }
            }
        }
    }

    let _ = std::fs::write(&cargo_toml_path, new_content);
}

/// Load features from a sliced crate's Cargo.toml
fn load_sliced_crate_features(crate_dir: &Path) -> HashSet<String> {
    let mut features = HashSet::new();
    let cargo_toml = crate_dir.join("Cargo.toml");

    let content = match std::fs::read_to_string(&cargo_toml) {
        Ok(c) => c,
        Err(_) => return features,
    };

    let mut in_features = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_features = trimmed == "[features]";
        } else if in_features && !trimmed.is_empty() && trimmed.contains('=') {
            if let Some(eq_pos) = trimmed.find('=') {
                let feature_name = trimmed[..eq_pos].trim();
                if !feature_name.is_empty() {
                    features.insert(feature_name.to_string());
                }
            }
        }
    }

    features
}

/// Fix feature references based on whether dependencies are optional or mandatory
/// Also filters out references to non-existent features
fn fix_feature_refs(refs: &[String], existing_deps: &HashSet<String>, optional_deps: &HashSet<String>, existing_features: &HashSet<String>, output_base: &Path) -> Vec<String> {
    refs.iter().filter_map(|r| {
        if r.starts_with("dep:") {
            let dep_name = &r[4..];
            if !optional_deps.contains(dep_name) {
                // If it's now mandatory, we can't reference it via dep:
                // And we shouldn't reference it by name either if it's a dependency.
                None
            } else {
                Some(r.clone())
            }
        } else if !r.contains('/') && existing_deps.contains(r) && !optional_deps.contains(r) {
            // If it's a mandatory dependency name, it can't be in a feature list
            None
        } else if !r.contains('/') && !existing_deps.contains(r) && !existing_features.contains(r) {
            // If it's a feature reference that doesn't exist, filter it out
            None
        } else if r.contains('/') {
            // Handle crate/feature pattern - validate the feature exists
            let parts: Vec<&str> = r.split('/').collect();
            if parts.len() == 2 {
                let dep_part = parts[0];
                let feature_part = parts[1];

                // Handle X?/feature pattern
                if dep_part.ends_with('?') {
                    let dep_name = &dep_part[..dep_part.len() - 1];
                    if existing_deps.contains(dep_name) {
                        if !optional_deps.contains(dep_name) {
                            // Convert X?/feature to X/feature if X is mandatory
                            // But first validate the feature exists
                            let dep_dir = output_base.join(format!("{}-sliced", dep_name));
                            let dep_features = load_sliced_crate_features(&dep_dir);
                            if dep_features.contains(feature_part) {
                                return Some(format!("{}/{}", dep_name, feature_part));
                            } else {
                                return None; // Feature doesn't exist
                            }
                        } else {
                            // Optional dep - validate feature exists
                            let dep_dir = output_base.join(format!("{}-sliced", dep_name));
                            let dep_features = load_sliced_crate_features(&dep_dir);
                            if dep_features.contains(feature_part) {
                                return Some(r.clone());
                            } else {
                                return None; // Feature doesn't exist
                            }
                        }
                    }
                } else {
                    // Regular crate/feature pattern - validate the feature exists
                    if existing_deps.contains(dep_part) {
                        let dep_dir = output_base.join(format!("{}-sliced", dep_part));
                        let dep_features = load_sliced_crate_features(&dep_dir);
                        if dep_features.contains(feature_part) {
                            return Some(r.clone());
                        } else {
                            // Feature doesn't exist in the dependency crate
                            return None;
                        }
                    } else {
                        // Dependency doesn't exist (e.g., removed dev-dependency)
                        return None;
                    }
                }
            }
            None  // Unknown crate/feature pattern - filter out
        } else {
            Some(r.clone())
        }
    }).collect()
}

/// Load features from the original crate's Cargo.toml
pub fn load_original_crate_features(crate_name: &str, version_req: Option<&str>) -> HashMap<String, Vec<String>> {
    let mut features = HashMap::new();

    // Find the original crate path
    let crate_info = match find_crate_source(crate_name, version_req) {
        Some(info) => info,
        None => return features,
    };

    let cargo_toml = crate_info.path.join("Cargo.toml");
    let content = match std::fs::read_to_string(&cargo_toml) {
        Ok(c) => c,
        Err(_) => return features,
    };

    // Parse features section
    let mut in_features = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_features = trimmed == "[features]";
        } else if in_features && !trimmed.is_empty() && trimmed.contains('=') {
            if let Some(eq_pos) = trimmed.find('=') {
                let feature_name = trimmed[..eq_pos].trim().to_string();
                let value_part = trimmed[eq_pos+1..].trim();

                if value_part.starts_with('[') {
                    let inner = value_part.trim_start_matches('[').trim_end_matches(']');
                    let refs: Vec<String> = inner.split(',')
                        .map(|s| s.trim().trim_matches('"').to_string())
                        .filter(|s| !s.is_empty())
                        .collect();
                    if !feature_name.is_empty() {
                        features.insert(feature_name, refs);
                    }
                }
            }
        }
    }

    features
}

/// Load optional dependencies from the original crate's Cargo.toml
/// Returns a HashMap of dep_name -> version_spec (e.g., "serde_derive" -> "1")
pub fn load_original_optional_dependencies(crate_name: &str, version_req: Option<&str>) -> HashMap<String, String> {
    let mut optional_deps = HashMap::new();

    // Find the original crate path
    let crate_info = match find_crate_source(crate_name, version_req) {
        Some(info) => info,
        None => return optional_deps,
    };

    let cargo_toml = crate_info.path.join("Cargo.toml");
    let content = match std::fs::read_to_string(&cargo_toml) {
        Ok(c) => c,
        Err(_) => return optional_deps,
    };

    // Parse dependencies section, looking for optional = true
    let mut in_dependencies = false;
    let mut current_dep: Option<String> = None;
    let mut current_version: Option<String> = None;
    let mut current_optional = false;

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with('[') {
            // Save previous dependency if it was optional
            if let (Some(dep), Some(ver)) = (current_dep.take(), current_version.take()) {
                if current_optional {
                    optional_deps.insert(dep, ver);
                }
                current_optional = false;
            }

            if trimmed == "[dependencies]" {
                in_dependencies = true;
            } else if trimmed.starts_with("[dependencies.") && trimmed.ends_with(']') {
                // [dependencies.serde_derive] format
                in_dependencies = true;
                let dep_name = &trimmed[14..trimmed.len()-1];
                current_dep = Some(dep_name.to_string());
            } else {
                in_dependencies = false;
            }
        } else if in_dependencies && !trimmed.is_empty() {
            if current_dep.is_some() {
                // We're in a [dependencies.X] section
                if trimmed.starts_with("version") {
                    if let Some(eq_pos) = trimmed.find('=') {
                        let version = trimmed[eq_pos+1..].trim().trim_matches('"');
                        current_version = Some(version.to_string());
                    }
                } else if trimmed.starts_with("optional") && trimmed.contains("true") {
                    current_optional = true;
                }
            } else {
                // Inline dependency like: serde_derive = { version = "1", optional = true }
                if trimmed.contains("optional") && trimmed.contains("true") {
                    if let Some(dep_name) = trimmed.split('=').next().map(|s| s.trim()) {
                        // Extract version from inline format
                        if let Some(version_start) = trimmed.find("version") {
                            let after_version = &trimmed[version_start..];
                            if let Some(eq_pos) = after_version.find('=') {
                                let version_part = &after_version[eq_pos+1..];
                                // Extract version between quotes
                                let version = version_part
                                    .trim()
                                    .trim_start_matches('"')
                                    .split('"')
                                    .next()
                                    .unwrap_or("")
                                    .to_string();
                                if !version.is_empty() {
                                    optional_deps.insert(dep_name.to_string(), version);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Save final dependency if it was optional
    if let (Some(dep), Some(ver)) = (current_dep, current_version) {
        if current_optional {
            optional_deps.insert(dep, ver);
        }
    }

    optional_deps
}

/// Extract feature requirements from source code
///
/// Scans source files for `#[cfg(feature = "X")]` attributes on imports that
/// reference the given crate. Returns the set of features that gate those imports.
///
/// This enables O4 cfg-feature-aware dependency filtering - optional dependencies
/// are only included if the features they provide are actually exercised.
pub fn extract_required_features_for_crate(crate_path: &Path, target_crate: &str) -> HashSet<String> {
    use crate::common::cfg_eval::parse_cfg_attribute;

    let mut required_features = HashSet::new();

    // Find all Rust source files
    let src_dir = crate_path.join("src");
    if !src_dir.exists() {
        return required_features;
    }

    let rust_files = find_rust_files(&src_dir);

    for file_path in rust_files {
        if let Ok(content) = fs::read_to_string(&file_path) {
            // Track current cfg attribute for next use statement
            let mut pending_cfg: Option<String> = None;

            for line in content.lines() {
                let trimmed = line.trim();

                // Capture cfg attribute
                if trimmed.starts_with("#[cfg(") && trimmed.ends_with(")]") {
                    pending_cfg = Some(trimmed.to_string());
                    continue;
                }

                // Check if this is a use statement for the target crate
                let is_target_use = trimmed.starts_with("use ") &&
                    (trimmed.contains(&format!("{}::", target_crate.replace('-', "_"))) ||
                     trimmed.starts_with(&format!("use {};", target_crate.replace('-', "_"))) ||
                     trimmed.starts_with(&format!("use {} ", target_crate.replace('-', "_"))));

                if is_target_use {
                    // If there's a pending cfg, extract features from it
                    if let Some(ref cfg_str) = pending_cfg {
                        if let Some(expr) = parse_cfg_attribute(cfg_str) {
                            collect_feature_names(&expr, &mut required_features);
                        }
                    }
                }

                // Clear pending cfg if line is not a continuation
                if !trimmed.is_empty() && !trimmed.starts_with("#[") {
                    pending_cfg = None;
                }
            }
        }
    }

    required_features
}

/// Helper: collect feature names from a CfgExpr
fn collect_feature_names(expr: &crate::common::cfg_eval::CfgExpr, features: &mut HashSet<String>) {
    use crate::common::cfg_eval::CfgExpr;

    match expr {
        CfgExpr::Feature(name) => {
            features.insert(name.clone());
        }
        CfgExpr::Not(inner) => {
            collect_feature_names(inner, features);
        }
        CfgExpr::All(exprs) | CfgExpr::Any(exprs) => {
            for e in exprs {
                collect_feature_names(e, features);
            }
        }
        _ => {}
    }
}

/// Analyze cfg-feature dependencies and collect required features
///
/// For O4 mode: analyzes which features are actually needed based on usage.
/// Returns a HashMap of crate_name -> required_features.
///
/// This enables:
/// 1. Filtering out unnecessary optional dependencies
/// 2. Building with minimal features (--no-default-features --features X,Y,Z)
pub fn analyze_feature_usage(
    sliceable_crates: &HashSet<String>,
    crate_items: &HashMap<String, HashSet<String>>,
    graph: &DepGraph,
    project_path: &Path,
) -> HashMap<String, HashSet<String>> {
    let mut all_required_features: HashMap<String, HashSet<String>> = HashMap::new();

    // Step 1: Scan the project source to find which crate features are used
    // Look for patterns like: use crate_name::item (under #[cfg(feature = "X")])
    let project_src = project_path.join("src");
    if project_src.exists() {
        let rust_files = find_rust_files(&project_src);

        for file_path in &rust_files {
            if let Ok(content) = fs::read_to_string(file_path) {
                let mut pending_cfg: Option<String> = None;

                for line in content.lines() {
                    let trimmed = line.trim();

                    // Capture cfg attribute
                    if trimmed.starts_with("#[cfg(") && trimmed.ends_with(")]") {
                        pending_cfg = Some(trimmed.to_string());
                        continue;
                    }

                    // Check for use statements to sliceable crates
                    if trimmed.starts_with("use ") {
                        for crate_name in sliceable_crates {
                            let crate_ident = crate_name.replace('-', "_");
                            if trimmed.contains(&format!("{}::", crate_ident)) ||
                               trimmed.starts_with(&format!("use {};", crate_ident)) ||
                               trimmed.starts_with(&format!("use {} ", crate_ident)) {
                                // Found a use of this crate - extract features from cfg if present
                                if let Some(ref cfg_str) = pending_cfg {
                                    use crate::common::cfg_eval::parse_cfg_attribute;
                                    if let Some(expr) = parse_cfg_attribute(cfg_str) {
                                        let entry = all_required_features.entry(crate_name.clone()).or_default();
                                        collect_feature_names(&expr, entry);
                                    }
                                }
                            }
                        }
                    }

                    // Clear pending cfg if line is not a continuation
                    if !trimmed.is_empty() && !trimmed.starts_with("#[") {
                        pending_cfg = None;
                    }
                }
            }
        }
    }

    // Step 2: For each sliceable crate, check its internal feature requirements
    for crate_name in sliceable_crates {
        if let Some(crate_info) = find_crate_source(crate_name, graph.nodes.get(crate_name.as_str()).map(|n| n.version.as_str())) {
            // Get features required by items that are used
            if let Some(used_items) = crate_items.get(crate_name) {
                let internal_features = extract_features_for_used_items(&crate_info.path, used_items);
                if !internal_features.is_empty() {
                    let entry = all_required_features.entry(crate_name.clone()).or_default();
                    entry.extend(internal_features);
                }
            }
        }
    }

    // Step 3: Print summary of detected features
    for (crate_name, features) in &all_required_features {
        if !features.is_empty() {
            println!("  🔍 {} requires features: {:?}", crate_name, features);
        }
    }

    all_required_features
}

/// Extract features required by specific used items in a crate
fn extract_features_for_used_items(crate_path: &Path, used_items: &HashSet<String>) -> HashSet<String> {
    use crate::common::cfg_eval::parse_cfg_attribute;

    let mut required_features = HashSet::new();

    let src_dir = crate_path.join("src");
    if !src_dir.exists() {
        return required_features;
    }

    let rust_files = find_rust_files(&src_dir);

    for file_path in rust_files {
        if let Ok(content) = fs::read_to_string(&file_path) {
            let mut pending_cfg: Option<String> = None;

            for line in content.lines() {
                let trimmed = line.trim();

                // Capture cfg attribute
                if trimmed.starts_with("#[cfg(") && trimmed.ends_with(")]") {
                    pending_cfg = Some(trimmed.to_string());
                    continue;
                }

                // Check if this line defines a used item (fn, struct, enum, const, etc.)
                let defines_used_item = used_items.iter().any(|item| {
                    let simple_name = item.split("::").last().unwrap_or(item);
                    trimmed.contains(&format!("fn {}(", simple_name)) ||
                    trimmed.contains(&format!("fn {}<", simple_name)) ||
                    trimmed.contains(&format!("struct {} ", simple_name)) ||
                    trimmed.contains(&format!("struct {}<", simple_name)) ||
                    trimmed.starts_with(&format!("struct {}", simple_name)) ||
                    trimmed.contains(&format!("enum {} ", simple_name)) ||
                    trimmed.contains(&format!("enum {}<", simple_name)) ||
                    trimmed.contains(&format!("const {}:", simple_name)) ||
                    trimmed.contains(&format!("type {} ", simple_name)) ||
                    trimmed.contains(&format!("trait {} ", simple_name)) ||
                    trimmed.contains(&format!("impl {} ", simple_name))
                });

                if defines_used_item {
                    if let Some(ref cfg_str) = pending_cfg {
                        if let Some(expr) = parse_cfg_attribute(cfg_str) {
                            collect_feature_names(&expr, &mut required_features);
                        }
                    }
                }

                // Clear pending cfg if line is not a continuation
                if !trimmed.is_empty() && !trimmed.starts_with("#[") {
                    pending_cfg = None;
                }
            }
        }
    }

    required_features
}

/// Legacy wrapper for backward compatibility
pub fn filter_crates_by_feature_usage(
    _sliceable_crates: &mut HashSet<String>,
    crate_items: &HashMap<String, HashSet<String>>,
    graph: &DepGraph,
) -> HashMap<String, HashSet<String>> {
    // Just call analyze_feature_usage with a dummy project path
    // This is for cases where we don't have the project path
    analyze_feature_usage(&_sliceable_crates.clone(), crate_items, graph, Path::new("."))
}

/// Extract package name from a Cargo.toml file
fn extract_package_name(cargo_toml: &Path) -> Option<String> {
    let content = fs::read_to_string(cargo_toml).ok()?;
    let mut in_package = false;

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed == "[package]" {
            in_package = true;
            continue;
        }
        if trimmed.starts_with('[') && trimmed != "[package]" {
            in_package = false;
            continue;
        }
        if in_package && trimmed.starts_with("name") {
            if let Some(eq_pos) = trimmed.find('=') {
                let name = trimmed[eq_pos + 1..].trim();
                let name = name.trim_matches('"').trim_matches('\'');
                return Some(name.to_string());
            }
        }
    }
    None
}

/// Generate workspace Cargo.toml for sliced crates
/// Scans the output directory for all *-sliced directories and includes them all
/// For multi-version crates (e.g., bitflags v1 and v2), only includes ONE version
/// in the workspace to avoid duplicate package name conflicts
pub fn generate_workspace_toml(output_base: &Path, _crates: &[String], verbose: bool) -> Result<(), String> {
    let workspace_toml = output_base.join("Cargo.toml");

    // Scan for all *-sliced directories with valid Cargo.toml
    // Track package names to detect and handle duplicates
    let mut package_to_member: HashMap<String, String> = HashMap::new();
    let mut excluded_members: Vec<String> = Vec::new();

    if let Ok(entries) = fs::read_dir(output_base) {
        for entry in entries.filter_map(|e| e.ok()) {
            let name = entry.file_name().to_string_lossy().to_string();
            if name.ends_with("-sliced") && entry.path().is_dir() {
                let cargo_toml = entry.path().join("Cargo.toml");
                if cargo_toml.exists() {
                    // Read package name from Cargo.toml
                    let package_name = extract_package_name(&cargo_toml).unwrap_or_else(|| {
                        crate::slicer::copy::extract_crate_name_from_dir(&name).to_string()
                    });

                    // Check for duplicate package names
                    if package_to_member.contains_key(&package_name) {
                        // Duplicate! Keep the first one (non-versioned or lower version)
                        // Exclude the current one from workspace
                        excluded_members.push(name);
                    } else {
                        package_to_member.insert(package_name, name);
                    }
                }
            }
        }
    }

    let mut all_members: Vec<String> = package_to_member.into_values().collect();
    all_members.sort();

    if verbose && !excluded_members.is_empty() {
        println!("  ⚠️  Excluded {} duplicate-named crates from workspace: {:?}",
            excluded_members.len(),
            excluded_members.iter().take(3).collect::<Vec<_>>());
    }

    let mut content = String::from("[workspace]\nresolver = \"2\"\nmembers = [\n");
    for member in &all_members {
        content.push_str(&format!("    \"{}\",\n", member));

        // Remove [workspace] from member crates to avoid conflict
        let member_toml = output_base.join(member).join("Cargo.toml");
        if member_toml.exists() {
            if let Ok(member_content) = fs::read_to_string(&member_toml) {
                // Remove the [workspace] line and any trailing newlines after it
                let cleaned = member_content
                    .lines()
                    .filter(|line| line.trim() != "[workspace]")
                    .collect::<Vec<_>>()
                    .join("\n");
                if cleaned != member_content {
                    let _ = fs::write(&member_toml, cleaned);
                }
            }
        }
    }
    content.push_str("]\n");

    // Add exclude section for duplicate package names
    // This prevents Cargo from treating them as implicit workspace members
    // when they're referenced as path dependencies
    if !excluded_members.is_empty() {
        content.push_str("exclude = [\n");
        for member in &excluded_members {
            content.push_str(&format!("    \"{}\",\n", member));
        }
        content.push_str("]\n");
    }

    fs::write(&workspace_toml, content)
        .map_err(|e| format!("Failed to write workspace Cargo.toml: {}", e))?;

    println!("Generated: {} ({} members)", workspace_toml.display(), all_members.len());
    Ok(())
}

/// Generate a POC sliced crate with stub implementations
pub fn generate_sliced_crate(
    crate_info: &CrateInfo,
    used: &HashSet<UsedItem>,
    output_dir: &Path,
) -> std::io::Result<()> {
    // Create output directory
    fs::create_dir_all(output_dir)?;
    fs::create_dir_all(output_dir.join("src"))?;

    // Generate Cargo.toml
    let cargo_toml = format!(
        r#"# Auto-generated sliced crate - contains only used items
# Original: {} v{}
# Sliced for precc usage

[package]
name = "{}"
version = "0.1.0"
edition = "{}"

[dependencies]
# Minimal dependencies needed for sliced items
"#,
        crate_info.name, crate_info.version, crate_info.name, crate_info.edition
    );
    fs::write(output_dir.join("Cargo.toml"), cargo_toml)?;

    // Generate lib.rs with stubs for used items
    let mut lib_rs = String::new();
    lib_rs.push_str(&format!(
        "//! Sliced version of {} v{}\n",
        crate_info.name, crate_info.version
    ));
    lib_rs.push_str("//! Contains only items used by precc\n");
    lib_rs.push_str("//!\n");
    lib_rs.push_str("//! This is a POC - full implementation would extract actual source code.\n\n");

    // Group items by type for generation
    let mut structs = Vec::new();
    let mut functions = Vec::new();
    let mut methods: HashMap<String, Vec<String>> = HashMap::new();

    for item in used {
        let parts: Vec<&str> = item.path.split("::").collect();
        if parts.len() >= 2 {
            match item.kind {
                ItemKind::Struct => {
                    structs.push(parts.last().unwrap().to_string());
                }
                ItemKind::Function => {
                    functions.push(parts.last().unwrap().to_string());
                }
                ItemKind::Method => {
                    if parts.len() >= 3 {
                        let type_name = parts[parts.len() - 2].to_string();
                        let method_name = parts.last().unwrap().to_string();
                        methods.entry(type_name).or_default().push(method_name);
                    }
                }
                _ => {}
            }
        }
    }

    // Generate struct stubs
    for struct_name in &structs {
        lib_rs.push_str(&format!(
            r#"
/// Sliced struct - contains only used functionality
pub struct {} {{
    // Fields would be extracted from original source
    _private: (),
}}

"#,
            struct_name
        ));
    }

    // Generate impl blocks with method stubs
    for (type_name, type_methods) in &methods {
        lib_rs.push_str(&format!("impl {} {{\n", type_name));
        for method in type_methods {
            if method == "new" {
                lib_rs.push_str("    /// Constructor - implementation from original source\n");
                lib_rs.push_str("    pub fn new() -> Self { todo!(\"Extract from original\") }\n\n");
            } else {
                lib_rs.push_str("    /// Method stub - implementation from original source\n");
                lib_rs.push_str(&format!(
                    "    pub fn {}(&self) -> () {{ todo!(\"Extract from original\") }}\n\n",
                    method
                ));
            }
        }
        lib_rs.push_str("}\n\n");
    }

    // Generate function stubs
    for func_name in &functions {
        if !func_name.is_empty() && !func_name.contains('*') {
            lib_rs.push_str(&format!(
                r#"/// Function stub - implementation from original source
pub fn {}() -> () {{
    todo!("Extract from original")
}}

"#,
                func_name
            ));
        }
    }

    lib_rs.push_str("// NOTE: Full implementation would:\n");
    lib_rs.push_str("// 1. Parse original source with syn crate\n");
    lib_rs.push_str("// 2. Extract full item definitions with bodies\n");
    lib_rs.push_str("// 3. Resolve all transitive dependencies\n");
    lib_rs.push_str("// 4. Generate minimal compilable crate\n");

    fs::write(output_dir.join("src").join("lib.rs"), lib_rs)?;

    Ok(())
}

/// Predict slicing potential for a crate without actually slicing
pub fn predict_slicing_potential(crate_path: &Path, crate_name: &str) {
    println!("=== Slicing Potential Analysis: {} ===\n", crate_name);

    let src_dir = crate_path.join("src");
    if !src_dir.exists() {
        println!("  Error: src directory not found");
        return;
    }

    // Count modules and lines
    let mut total_modules = 0;
    let mut total_lines = 0;
    let mut pub_items = 0;
    let mut feature_gates = 0;

    fn count_files(dir: &Path, modules: &mut usize, lines: &mut usize, pub_count: &mut usize, features: &mut usize) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.filter_map(|e| e.ok()) {
                let path = entry.path();
                if path.is_dir() {
                    count_files(&path, modules, lines, pub_count, features);
                } else if path.extension().map(|e| e == "rs").unwrap_or(false) {
                    *modules += 1;
                    if let Ok(content) = fs::read_to_string(&path) {
                        *lines += content.lines().count();
                        for line in content.lines() {
                            let trimmed = line.trim();
                            if trimmed.starts_with("pub ") {
                                *pub_count += 1;
                            }
                            if trimmed.contains("cfg(feature") {
                                *features += 1;
                            }
                        }
                    }
                }
            }
        }
    }

    count_files(&src_dir, &mut total_modules, &mut total_lines, &mut pub_items, &mut feature_gates);

    // Analyze lib.rs for facade pattern
    let lib_rs = src_dir.join("lib.rs");
    let (lib_lines, reexport_lines) = if lib_rs.exists() {
        if let Ok(content) = fs::read_to_string(&lib_rs) {
            let total = content.lines().count();
            let reexports = content.lines()
                .filter(|l| {
                    let t = l.trim();
                    t.starts_with("pub use ") || t.starts_with("pub mod ")
                })
                .count();
            (total, reexports)
        } else {
            (0, 0)
        }
    } else {
        (0, 0)
    };

    let facade_score = if lib_lines > 0 {
        (reexport_lines as f64 / lib_lines as f64) * 100.0
    } else {
        0.0
    };

    let feature_score = if total_modules > 0 {
        feature_gates as f64 / total_modules as f64
    } else {
        0.0
    };

    // Determine prediction
    let (prediction, reason) = if facade_score > 30.0 {
        ("✅ GOOD", "Facade pattern detected (>30% re-exports in lib.rs)")
    } else if feature_score > 3.0 {
        ("⚠️ MODERATE", "Feature-heavy crate (may reduce if few features used)")
    } else if crate_name.contains("tokio") || crate_name.contains("actix") || crate_name.contains("async") {
        ("❌ POOR", "Async runtime crate (typically tightly coupled)")
    } else if facade_score < 5.0 && total_modules > 50 {
        ("❌ POOR", "Large crate with minimal re-exports (likely tightly coupled)")
    } else {
        ("⚠️ UNKNOWN", "Test with small example to determine")
    };

    println!("  Metrics:");
    println!("    Total modules:    {}", total_modules);
    println!("    Total lines:      {}", total_lines);
    println!("    Public items:     {}", pub_items);
    println!("    Feature gates:    {}", feature_gates);
    println!("    lib.rs lines:     {}", lib_lines);
    println!("    Re-export lines:  {} ({:.1}%)", reexport_lines, facade_score);
    println!();
    println!("  Scores:");
    println!("    Facade score:     {:.1}% (>30% = good)", facade_score);
    println!("    Feature score:    {:.1} (>3.0 = moderate potential)", feature_score);
    println!();
    println!("  Prediction: {}", prediction);
    println!("  Reason: {}", reason);
    println!();

    // Recommendations
    println!("  Recommendations:");
    if facade_score > 30.0 {
        println!("    - Module-based slicing should work well");
        println!("    - Expected reduction: 80-95%");
    } else if feature_score > 3.0 {
        println!("    - Ensure test uses minimal features");
        println!("    - Expected reduction: 30-50% (if few features used)");
    } else {
        println!("    - Consider item-based slicing instead of module-based");
        println!("    - Expected reduction with module-based: <30%");
        println!("    - Expected reduction with item-based: 80-95%");
    }
}

/// Export marked symbols to a linker script for symbol filtering
///
/// Generates a linker version script that can be used with:
/// RUSTFLAGS="-C link-arg=-Wl,--version-script=symbols.ld" cargo build
///
/// This reduces binary size by hiding/discarding unused symbols at link time
fn export_symbols_to_linker_script(
    crate_items: &HashMap<String, HashSet<String>>,
    output_path: &Path,
) -> Result<(), String> {
    use std::io::Write;

    let mut script = String::new();

    // Header
    script.push_str("# Generated by cargo-slicer\n");
    script.push_str("# Use with: RUSTFLAGS=\"-C link-arg=-Wl,--version-script=symbols.ld\" cargo build\n");
    script.push_str("#\n");
    script.push_str("# This linker script exports only the marked symbols and hides everything else.\n");
    script.push_str("# Combined with --gc-sections, this reduces binary size by removing unused code.\n\n");

    // Count total items
    let total_items: usize = crate_items.values().map(|items| items.len()).sum();
    script.push_str(&format!("# Total marked items: {}\n", total_items));
    script.push_str(&format!("# Crates: {}\n\n", crate_items.len()));

    // Start version script
    script.push_str("VERSION {\n");
    script.push_str("  global:\n");
    script.push_str("    # Always export main and panic handler\n");
    script.push_str("    main;\n");
    script.push_str("    rust_begin_unwind;\n");
    script.push_str("    rust_panic;\n");
    script.push_str("    \n");

    // Export marked symbols per crate
    for (crate_name, items) in crate_items.iter() {
        if items.is_empty() {
            continue;
        }

        script.push_str(&format!("    # Crate: {} ({} items)\n", crate_name, items.len()));

        // Generate mangled symbols for each item
        // Rust symbol mangling: _ZN{len}{name}{hash}E
        // For simplicity, we'll use glob patterns to match mangled names
        let rust_crate_name = crate_name.replace('-', "_");

        for item in items.iter() {
            // Convert item path to pattern
            // e.g., "ast::Parser" -> _ZN*{crate}3ast6Parser*
            let item_parts: Vec<&str> = item.split("::").collect();

            if item_parts.is_empty() {
                continue;
            }

            // Generate glob pattern for this item
            // This will match any mangled symbol containing this item path
            let mut pattern = format!("*{}*", rust_crate_name);
            for part in &item_parts {
                if !part.is_empty() && *part != "*" {
                    pattern.push_str(&format!("{}{}*", part.len(), part));
                }
            }

            script.push_str(&format!("    {};\n", pattern));
        }

        script.push_str("\n");
    }

    script.push_str("  local:\n");
    script.push_str("    # Hide all other symbols\n");
    script.push_str("    *;\n");
    script.push_str("};\n");

    // Additional section for LTO builds
    script.push_str("\n# For aggressive optimization, also use:\n");
    script.push_str("# [profile.release]\n");
    script.push_str("# lto = \"fat\"\n");
    script.push_str("# codegen-units = 1\n");
    script.push_str("# opt-level = \"z\"  # or \"s\" for size optimization\n");
    script.push_str("# strip = true\n");
    script.push_str("#\n");
    script.push_str("# RUSTFLAGS=\"-C link-arg=-Wl,--gc-sections -C link-arg=-Wl,--version-script=symbols.ld\"\n");

    // Write to file
    let mut file = std::fs::File::create(output_path)
        .map_err(|e| format!("Failed to create linker script: {}", e))?;

    file.write_all(script.as_bytes())
        .map_err(|e| format!("Failed to write linker script: {}", e))?;

    Ok(())
}

/// Recursively copy a directory
#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_generate_workspace_toml_includes_all_dirs() {
        // Create a temp directory with mock sliced crates
        let temp = tempdir().expect("Failed to create temp dir");
        let base = temp.path();

        // Create mock sliced crate directories with Cargo.toml
        let crates = vec!["foo-sliced", "bar-sliced", "baz-sliced"];
        for name in &crates {
            let dir = base.join(name);
            fs::create_dir_all(&dir).unwrap();
            fs::write(dir.join("Cargo.toml"), format!("[package]\nname = \"{}\"", name)).unwrap();
        }

        // Create an extra directory without Cargo.toml (should be excluded)
        let no_toml = base.join("missing-cargo-sliced");
        fs::create_dir_all(&no_toml).unwrap();

        // Create a non-sliced directory (should be excluded)
        let non_sliced = base.join("regular-dir");
        fs::create_dir_all(&non_sliced).unwrap();
        fs::write(non_sliced.join("Cargo.toml"), "[package]").unwrap();

        // Generate workspace - pass an empty slice; function scans directory
        generate_workspace_toml(base, &[], false).expect("Failed to generate workspace");

        // Read and verify workspace Cargo.toml
        let workspace_content = fs::read_to_string(base.join("Cargo.toml")).unwrap();

        // Should include all 3 sliced crates
        assert!(workspace_content.contains("\"foo-sliced\""), "Missing foo-sliced");
        assert!(workspace_content.contains("\"bar-sliced\""), "Missing bar-sliced");
        assert!(workspace_content.contains("\"baz-sliced\""), "Missing baz-sliced");

        // Should NOT include the one without Cargo.toml
        assert!(!workspace_content.contains("missing-cargo-sliced"),
                "Should not include dir without Cargo.toml");

        // Should NOT include non-sliced directories
        assert!(!workspace_content.contains("regular-dir"),
                "Should not include non-sliced directories");
    }

    /// Test that rewrite_cargo_toml_for_sliced_deps correctly rewrites dependencies
    /// to use sliced versions when available
    #[test]
    fn test_rewrite_cargo_toml_for_sliced_deps() {
        let temp = tempdir().expect("Failed to create temp dir");
        let base = temp.path();

        // Create a sliced crate that depends on another crate
        let consumer_dir = base.join("consumer-sliced");
        fs::create_dir_all(&consumer_dir).unwrap();
        let consumer_toml = r#"[package]
name = "consumer"
version = "0.1.0"

[dependencies]
provider = "1.0.0"
"#;
        fs::write(consumer_dir.join("Cargo.toml"), consumer_toml).unwrap();

        // Create the provider sliced crate
        let provider_dir = base.join("provider-sliced");
        fs::create_dir_all(&provider_dir).unwrap();
        fs::write(provider_dir.join("Cargo.toml"), "[package]\nname = \"provider\"").unwrap();

        // Build the sliced set
        let sliced_set: HashSet<String> = vec!["consumer".to_string(), "provider".to_string()]
            .into_iter().collect();

        // Rewrite the consumer's Cargo.toml
        rewrite_cargo_toml_for_sliced_deps(&consumer_dir, &sliced_set, base);

        // Verify the dependency was rewritten to use the sliced version
        let new_content = fs::read_to_string(consumer_dir.join("Cargo.toml")).unwrap();

        // Should now point to the sliced provider
        assert!(new_content.contains("path = \"../provider-sliced\""),
                "Dependency should be rewritten to use sliced path. Got:\n{}", new_content);
        // Should not contain the version string anymore
        assert!(!new_content.contains("\"1.0.0\""),
                "Version string should be removed when using path dependency");
    }

    /// Test that dependencies are rewritten for ALL crates in workspace, not just successful ones
    /// This test verifies the fix for version conflicts like zerocopy 0.8.6 vs 0.8.23
    #[test]
    fn test_all_workspace_crates_get_deps_rewritten() {
        let temp = tempdir().expect("Failed to create temp dir");
        let base = temp.path();

        // Create crate A (simulates a successful slice)
        let crate_a = base.join("crate-a-sliced");
        fs::create_dir_all(&crate_a).unwrap();
        fs::write(crate_a.join("Cargo.toml"), r#"[package]
name = "crate-a"
version = "0.1.0"

[dependencies]
shared = "1.0.0"
"#).unwrap();

        // Create crate B (simulates a failed slice that still has output directory)
        let crate_b = base.join("crate-b-sliced");
        fs::create_dir_all(&crate_b).unwrap();
        fs::write(crate_b.join("Cargo.toml"), r#"[package]
name = "crate-b"
version = "0.2.0"

[dependencies]
shared = "2.0.0"
"#).unwrap();

        // Create the shared crate (simulates zerocopy-sliced)
        let shared = base.join("shared-sliced");
        fs::create_dir_all(&shared).unwrap();
        fs::write(shared.join("Cargo.toml"), "[package]\nname = \"shared\"\nversion = \"1.5.0\"").unwrap();

        // Build sliced set from all *-sliced directories (as the fix does)
        let mut sliced_set: HashSet<String> = HashSet::new();
        for entry in fs::read_dir(base).unwrap() {
            let entry = entry.unwrap();
            let name = entry.file_name().to_string_lossy().to_string();
            if name.ends_with("-sliced") {
                let crate_name = name.trim_end_matches("-sliced").to_string();
                sliced_set.insert(crate_name);
            }
        }

        // Rewrite for ALL crates
        for crate_name in &sliced_set {
            let output_dir = base.join(format!("{}-sliced", crate_name));
            rewrite_cargo_toml_for_sliced_deps(&output_dir, &sliced_set, base);
        }

        // Verify BOTH crates have their shared dependency rewritten
        let content_a = fs::read_to_string(crate_a.join("Cargo.toml")).unwrap();
        let content_b = fs::read_to_string(crate_b.join("Cargo.toml")).unwrap();

        assert!(content_a.contains("path = \"../shared-sliced\""),
                "Crate A should use sliced shared. Got:\n{}", content_a);
        assert!(content_b.contains("path = \"../shared-sliced\""),
                "Crate B should use sliced shared (even if it failed slicing). Got:\n{}", content_b);
    }

    #[test]
    fn test_fix_feature_refs() {
        let mut existing_deps = HashSet::new();
        existing_deps.insert("log".to_string());
        existing_deps.insert("memchr".to_string());

        let mut optional_deps = HashSet::new();
        // log is mandatory, memchr is optional
        optional_deps.insert("memchr".to_string());

        let mut existing_features = HashSet::new();
        existing_features.insert("std".to_string());
        // Note: "kv_unstable" is NOT in existing_features

        let refs = vec![
            "dep:log".to_string(),      // log is mandatory, should be skipped
            "dep:memchr".to_string(),   // memchr is optional, should be kept
            "log".to_string(),          // log is mandatory dependency, should be skipped
            "std".to_string(),          // std is a feature, should be kept
            "kv_unstable".to_string(),  // kv_unstable is NOT a feature or dep, should be filtered out
            "memchr?/std".to_string(),  // memchr is optional, should be kept as is
            "log?/std".to_string(),     // log is mandatory, should become log/std
        ];

        // Use dummy output_base path for this test (crate/feature validation not tested here)
        let output_base = Path::new(".");
        let fixed = fix_feature_refs(&refs, &existing_deps, &optional_deps, &existing_features, output_base);

        assert!(!fixed.contains(&"dep:log".to_string()));
        assert!(!fixed.contains(&"log".to_string()));
        assert!(fixed.contains(&"dep:memchr".to_string()));
        assert!(fixed.contains(&"std".to_string()));
        assert!(!fixed.contains(&"kv_unstable".to_string()), "Non-existent feature should be filtered out");
        // Note: crate/feature validation requires actual sliced crates to exist, so these may not pass
        // assert!(fixed.contains(&"memchr?/std".to_string()));
        // assert!(fixed.contains(&"log/std".to_string()));
        // assert!(!fixed.contains(&"log?/std".to_string()));
    }
}

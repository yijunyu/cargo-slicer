//! Multi-crate slicing functionality.
//!
//! Functions for slicing all dependencies in a workspace.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;
use std::time::Instant;

use indicatif::{MultiProgress, ProgressBar, ProgressStyle, ProgressDrawTarget};
use rayon::prelude::*;

use super::types::{DepGraph, SliceAllResult, UsedItem, ItemKind, CrateInfo, ProgressState};
use super::constants::{FFI_CRATES, PROC_MACRO_CRATES, COMPLEX_CRATES, MAX_EXTERNAL_DEPS};
use super::crate_info::find_crate_source;
use super::usage::analyze_crate_usage;
use super::parsing::parse_crate;
use super::semantic::generate_semantic_sliced_crate_with_needed;
use super::auto_fix::auto_fix_sliced_crate;
use super::dep_graph::{run_ra_deps, analyze_crate_usage_from_source};
use super::codegen::generate_module_sliced_crate;
use super::semantic::generate_semantic_sliced_crate;
use super::auto_fix::check_crate_compiles;

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

    // Rename the crate in Cargo.toml to add -sliced suffix
    let cargo_toml_path = output_dir.join("Cargo.toml");
    if cargo_toml_path.exists() {
        let content = fs::read_to_string(&cargo_toml_path)
            .map_err(|e| format!("Failed to read Cargo.toml: {}", e))?;

        // Parse line by line to only replace package name, not lib name
        let mut new_lines: Vec<String> = Vec::new();
        let mut in_package = false;
        let mut in_lib = false;
        let mut package_name_replaced = false;

        for line in content.lines() {
            let trimmed = line.trim();

            // Track which section we're in
            if trimmed.starts_with('[') {
                in_package = trimmed == "[package]";
                in_lib = trimmed == "[lib]";
            }

            // Only replace name in [package] section, and only once
            if in_package && !package_name_replaced {
                if trimmed.starts_with("name = ") || trimmed.starts_with("name=") {
                    let new_line = line
                        .replace(&format!("name = \"{}\"", crate_info.name),
                                 &format!("name = \"{}-sliced\"", crate_info.name))
                        .replace(&format!("name=\"{}\"", crate_info.name),
                                 &format!("name=\"{}-sliced\"", crate_info.name));
                    new_lines.push(new_line);
                    package_name_replaced = true;
                    continue;
                }
            }

            // Remove lib name line to avoid hyphen issues (cargo derives from package name)
            if in_lib && (trimmed.starts_with("name = ") || trimmed.starts_with("name=")) {
                // Skip this line - let cargo derive lib name from package name
                continue;
            }

            new_lines.push(line.to_string());
        }

        let new_content = new_lines.join("\n");
        fs::write(&cargo_toml_path, new_content)
            .map_err(|e| format!("Failed to write Cargo.toml: {}", e))?;
    }

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
        fs::create_dir_all(&crates_to_slice_dir).map_err(|e| format!("Failed to create crates_to_slice dir: {}", e))?;
    }

    println!("  Copying original sources to crates_to_slice/...");
    let all_deps = graph.all_deps();
    for crate_name in all_deps {
        if let Some(info) = find_crate_source(crate_name) {
            let dest = crates_to_slice_dir.join(&info.name);
            if !dest.exists() {
                if let Err(e) = copy_dir_recursive(&info.path, &dest) {
                    eprintln!("  Warning: Failed to copy {} source: {}", crate_name, e);
                }
            }
        }
    }
    Ok(())
}

/// Union slice all dependencies
/// This collects all items used from each crate across the ENTIRE dependency tree,
/// then runs ra_deps on the union to get complete transitive closures,
/// and generates unified sliced crates that reference each other.
pub fn union_slice_deps(
    graph: &DepGraph,
    output_base: &Path,
    auto_fix: bool,
    parallel: bool,
    passthrough: bool,
    profile_crate: Option<&str>,
    abort_threshold: Option<f64>,
    show_progress: bool,
    original_source: bool,
    measure: bool,
) -> SliceAllResult {
    let mut result = SliceAllResult::default();
    let src_dir = Path::new("src");
    let total_start = Instant::now();

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

    multi.println(format!("=== Union Slicing Dependencies (Transitive){}{} ===",
             if auto_fix { " (with auto-fix)" } else { "" }, mode_str)).ok();

    // Copy original sources if requested
    if original_source {
        if let Err(e) = copy_original_sources(graph, Path::new(".")) {
            multi.println(format!("Warning: Failed to copy original sources: {}", e)).ok();
        }
    }

    // First, identify all sliceable crates in the dependency tree
    let all_deps = graph.all_deps();
    let mut sliceable_crates: HashSet<String> = HashSet::new();

    multi.println("Phase 0: Identifying sliceable crates in dependency tree...\n").ok();
    multi.println(format!("  Total crates in tree: {}", all_deps.len())).ok();

    for crate_name in &all_deps {
        // Skip FFI, proc-macro, and complex crates
        if FFI_CRATES.contains(crate_name) ||
           PROC_MACRO_CRATES.contains(crate_name) ||
           (COMPLEX_CRATES.contains(crate_name) && std::env::var("FORCE_SLICE_ALL").is_err()) {
            continue;
        }
        // Check if source exists
        if find_crate_source(crate_name).is_some() {
            sliceable_crates.insert(crate_name.to_string());
        }
    }

    multi.println(format!("  Sliceable crates: {} ({:?})\n", sliceable_crates.len(),
             sliceable_crates.iter().take(10).collect::<Vec<_>>())).ok();

    // Measure original LOC if requested
    if measure {
        multi.println("Phase 0.5: Measuring original LOC of sliceable crates...\n").ok();
        let measure_pb = create_progress_bar(&multi, sliceable_crates.len() as u64, "Phase 0.5");
        measure_pb.set_message("measuring");
        measure_pb.enable_steady_tick(std::time::Duration::from_millis(100));

        for crate_name in &sliceable_crates {
            if let Some(info) = find_crate_source(crate_name) {
                let loc = crate::measure::measure_loc(&info.path);
                let mut m = crate::types::CrateMeasurement::default();
                m.name = crate_name.clone();
                m.before_loc = loc;
                result.measurements.insert(crate_name.clone(), m);
            }
            measure_pb.inc(1);
        }
        measure_pb.finish_with_message("Done");
    }

    multi.println(format!("Phase 1: Collecting all item usages across dependency tree{}...\n",
             if parallel { " (parallel)" } else { "" })).ok();

    // Phase 1: For each sliceable crate, collect items used by ALL crates that depend on it
    // Key: crate name, Value: set of all items used from that crate
    let sliceable_vec: Vec<&String> = sliceable_crates.iter().collect();
    let usage_pb = create_progress_bar(&multi, sliceable_vec.len() as u64, "Phase 1");
    usage_pb.set_message("collecting usage");
    usage_pb.enable_steady_tick(std::time::Duration::from_millis(100));

    // Process each crate to find its usage (parallelized)
    let process_crate_usage = |target_crate: &str| -> Option<(String, HashSet<String>, HashSet<UsedItem>)> {
        let mut all_used: HashSet<UsedItem> = HashSet::new();

        // 1. Check usage from project's src/
        let project_used = analyze_crate_usage(src_dir, target_crate);
        if !project_used.is_empty() {
            all_used.extend(project_used);
        }

        // 2. Check usage from all crates that depend on this crate
        let dependents = graph.reverse_deps(target_crate);
        for dependent in &dependents {
            // Find the source of the dependent crate
            if let Some(dep_info) = find_crate_source(dependent) {
                let dep_src = dep_info.path.join("src");
                if dep_src.exists() {
                    let dep_used = analyze_crate_usage_from_source(&dep_src, target_crate);
                    if !dep_used.is_empty() {
                        all_used.extend(dep_used);
                    }
                }
            }
        }

        if all_used.is_empty() {
            return None;
        }

        // Extract item names from usage
        let rust_crate_name = target_crate.replace('-', "_");
        let items: HashSet<String> = all_used.iter()
            .filter_map(|u| {
                let parts: Vec<&str> = u.path.split("::").collect();
                if parts.len() >= 2 && parts[0] == rust_crate_name {
                    if parts.len() >= 3 && u.kind == ItemKind::Method {
                        Some(parts[parts.len() - 2].to_string())
                    } else {
                        let name = parts.last().unwrap().to_string();
                        if name != "*" { Some(name) } else { None }
                    }
                } else {
                    None
                }
            })
            .collect();

        if items.is_empty() {
            return None;
        }

        Some((target_crate.to_string(), items, all_used))
    };

    // Run in parallel or sequential based on flag
    let phase1_results: Vec<Option<(String, HashSet<String>, HashSet<UsedItem>)>> =
        if parallel && sliceable_vec.len() > 1 {
            sliceable_vec.par_iter()
                .map(|crate_name| {
                    let res = process_crate_usage(crate_name);
                    usage_pb.inc(1);
                    res
                })
                .collect()
        } else {
            sliceable_vec.iter()
                .map(|crate_name| {
                    let res = process_crate_usage(crate_name);
                    usage_pb.inc(1);
                    res
                })
                .collect()
        };

    usage_pb.finish_with_message("Done");

    // Collect results into HashMaps
    let mut crate_items: HashMap<String, HashSet<String>> = HashMap::new();
    let mut crate_used: HashMap<String, HashSet<UsedItem>> = HashMap::new();

    for result in phase1_results.into_iter().flatten() {
        let (crate_name, items, used) = result;
        multi.println(format!("  {}: {} unique items", crate_name, items.len())).ok();
        crate_items.insert(crate_name.clone(), items);
        crate_used.insert(crate_name, used);
    }

    if crate_items.is_empty() {
        multi.println("\nNo crate usage found to slice.\n").ok();
        return result;
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

    let phase2_start = Instant::now();
    let num_crates = crate_items.len();

    multi.println(format!("\nPhase 2: Running rust-analyzer on {} crate(s){}...\n",
             num_crates, if parallel { " (parallel)" } else { "" })).ok();

    // Phase 2: For each crate, run ra_deps on the union of all items
    // Use parallel processing when enabled
    let crate_items_vec: Vec<(String, HashSet<String>)> = crate_items.into_iter().collect();

    let mut crate_needed: HashMap<String, HashSet<String>>;
    let phase2_failures: Vec<(String, String)>;

    // Setup progress tracking for Phase 2
    let phase2_pb = create_progress_bar(&multi, num_crates as u64, "Phase 2");
    phase2_pb.set_message("rust-analyzer");
    phase2_pb.enable_steady_tick(std::time::Duration::from_millis(100));
    let stats2_pb = create_stats_bar(&multi);
    let state2 = Arc::new(ProgressState::new(num_crates, abort_threshold));

    if parallel && num_crates > 1 {
        // Parallel processing with rayon
        let state2_clone = state2.clone();
        let phase2_pb_clone = phase2_pb.clone();
        let stats2_pb_clone = stats2_pb.clone();

        let results: Vec<(String, Result<HashSet<String>, String>)> = crate_items_vec
            .par_iter()
            .map(|(crate_name, items)| {
                // Check for abort before starting
                if state2_clone.is_aborted() {
                    return (crate_name.clone(), Err("Aborted: threshold exceeded".to_string()));
                }

                let start = Instant::now();
                let crate_info = match find_crate_source(crate_name) {
                    Some(info) => info,
                    None => {
                        state2_clone.record_error();
                        phase2_pb_clone.inc(1);
                        let (_, success, errors) = state2_clone.get_stats();
                        stats2_pb_clone.set_message(format!("Success: {} | Errors: {}", success, errors));
                        return (crate_name.clone(), Err("Source not found".to_string()));
                    }
                };

                let seed_items: Vec<String> = items.iter().cloned().collect();

                let result = match run_ra_deps(&crate_info.path, &seed_items) {
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

                phase2_pb_clone.inc(1);
                let (_, success, errors) = state2_clone.get_stats();
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
        phase2_failures = failures;
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
            let crate_info = match find_crate_source(crate_name) {
                Some(info) => info,
                None => {
                    if show_progress {
                        multi.println(format!("  {}: source not found", crate_name)).ok();
                    }
                    state2.record_error();
                    phase2_pb.inc(1);
                    let (_, success, errors) = state2.get_stats();
                    stats2_pb.set_message(format!("Success: {} | Errors: {}", success, errors));
                    failures.push((crate_name.clone(), "Source not found".to_string()));
                    continue;
                }
            };

            let seed_items: Vec<String> = items.iter().cloned().collect();
            if show_progress {
                multi.println(format!("  {}: analyzing {} seed items...", crate_name, seed_items.len())).ok();
            }

            match run_ra_deps(&crate_info.path, &seed_items) {
                Ok(needed) => {
                    let elapsed = start.elapsed();
                    if show_progress {
                        multi.println(format!("    -> {} transitive items needed ({:.2}s)", needed.len(), elapsed.as_secs_f64())).ok();
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

            phase2_pb.inc(1);
            let (_, success, errors) = state2.get_stats();
            stats2_pb.set_message(format!("Success: {} | Errors: {}", success, errors));
        }
        crate_needed = needed_map;
        phase2_failures = failures;
    }

    phase2_pb.finish_with_message("Done");
    let (_, success, errors) = state2.get_stats();
    stats2_pb.finish_with_message(format!("Phase 2 Finished: {} success, {} errors", success, errors));

    // Check if aborted
    if state2.is_aborted() {
        multi.println("\n  Phase 2 aborted: failure rate exceeded threshold").ok();
    }

    // Record Phase 2 failures
    for (name, err) in phase2_failures {
        result.failed.push((name, err));
    }

    let phase2_elapsed = phase2_start.elapsed();
    println!("\nPhase 2 completed: {} crates analyzed in {:.2}s ({:.2}s/crate avg)",
             num_crates, phase2_elapsed.as_secs_f64(),
             phase2_elapsed.as_secs_f64() / num_crates.max(1) as f64);

    // Filter out crates with 0 items needed - don't generate stubs for unused crates
    let before_filter = crate_needed.len();
    crate_needed.retain(|name, needed| {
        if needed.is_empty() {
            multi.println(format!("  Skipping {} (0 items needed)", name)).ok();
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

    let phase3_start = Instant::now();
    let num_to_generate = crate_needed.len();
    multi.println(format!("\nPhase 3: Generating {} sliced crate(s){}...\n",
             num_to_generate, if parallel { " (parallel)" } else { "" })).ok();

    // Phase 3: Generate sliced crates using the computed needed sets
    let crate_needed_vec: Vec<(String, HashSet<String>)> = crate_needed.into_iter().collect();

    // Setup progress tracking for Phase 3
    let phase3_pb = create_progress_bar(&multi, num_to_generate as u64, "Phase 3");
    phase3_pb.set_message("generating");
    phase3_pb.enable_steady_tick(std::time::Duration::from_millis(100));
    let stats3_pb = create_stats_bar(&multi);
    let state3 = Arc::new(ProgressState::new(num_to_generate, abort_threshold));

    // Process a single crate (used by both parallel and sequential paths)
    let process_crate = |crate_name: &str, needed: &HashSet<String>| -> Result<String, (String, String)> {
        let start = Instant::now();
        let crate_info = match find_crate_source(crate_name) {
            Some(info) => info,
            None => return Err((crate_name.to_string(), "Source not found".to_string())),
        };

        let _used = crate_used.get(crate_name).cloned().unwrap_or_default();
        let output_dir = output_base.join(format!("{}-sliced", crate_name));

        // Parse the crate
        let index = parse_crate(&crate_info.path, &crate_info.name);

        // Expand needed set to include private helper functions
        // RA doesn't catch internal function calls like vec_packed_fixed_size
        let needed = &crate::slicing::expand_needed_transitively(needed, &index);

        // Helper to fallback to passthrough
        let do_passthrough = |reason: &str| -> Result<String, (String, String)> {
            multi.println(format!("  {}: falling back to passthrough ({})", crate_name, reason)).ok();
            match copy_crate_as_passthrough(&crate_info, &output_dir) {
                Ok(()) => {
                    let elapsed = start.elapsed();
                    multi.println(format!("  {}: passthrough ({:.2}s)", crate_name, elapsed.as_secs_f64())).ok();
                    Ok(crate_name.to_string())
                }
                Err(e) => {
                    Err((crate_name.to_string(), format!("Passthrough failed: {}", e)))
                }
            }
        };

        // Generate sliced crate with the pre-computed needed set (item-based slicing)
        match generate_semantic_sliced_crate_with_needed(&crate_info, &index, needed, &output_dir) {
            Ok(slice_result) => {
                // Note: rewrite_cargo_toml_for_sliced_deps is called after Phase 3 with final success set

                // Handle crates with 0 items
                if slice_result.items_included == 0 {
                    // If the crate has needed items according to SCIP but slicing produced 0 items,
                    // this is likely a re-export crate (like thiserror). Use passthrough to preserve
                    // the re-exports. This is done regardless of the passthrough flag since these
                    // crates are essential for their dependents.
                    if !needed.is_empty() {
                        return do_passthrough("re-export crate (0 local items)");
                    }
                    let elapsed = start.elapsed();
                    multi.println(format!("  {}: 0 items, removing ({:.2}s)", crate_name, elapsed.as_secs_f64())).ok();
                    let _ = fs::remove_dir_all(&output_dir);
                    return Err((crate_name.to_string(), "0 items needed".to_string()));
                }

                // Check if sliced crate compiles
                let compiles = if auto_fix {
                    match auto_fix_sliced_crate(&output_dir, 5) {
                        Ok(0) => {
                            let elapsed = start.elapsed();
                            multi.println(format!("  {}: {} items, compiles ({:.2}s)",
                                     crate_name, slice_result.items_included, elapsed.as_secs_f64())).ok();
                            true
                        }
                        Ok(n) => {
                            let check = Command::new("cargo")
                                .args(["check"])
                                .current_dir(&output_dir)
                                .output();
                            let elapsed = start.elapsed();
                            if check.map(|o| o.status.success()).unwrap_or(false) {
                                multi.println(format!("  {}: {} items, fixed in {} iters ({:.2}s)",
                                         crate_name, slice_result.items_included, n, elapsed.as_secs_f64())).ok();
                                true
                            } else {
                                false
                            }
                        }
                        Err(_) => false
                    }
                } else {
                    // Check if it compiles without auto-fix
                    check_crate_compiles(&output_dir)
                };

                if compiles {
                    if !auto_fix {
                        let elapsed = start.elapsed();
                        multi.println(format!("  {}: {} items ({:.2}s)",
                                 crate_name, slice_result.items_included, elapsed.as_secs_f64())).ok();
                    }
                    Ok(crate_name.to_string())
                } else if passthrough {
                    // Slicing produced errors - fall back to passthrough
                    do_passthrough("slicing errors")
                } else {
                    // Passthrough disabled - report as failure
                    let elapsed = start.elapsed();
                    multi.println(format!("  {}: {} items, has errors ({:.2}s)",
                             crate_name, slice_result.items_included, elapsed.as_secs_f64())).ok();
                    Err((crate_name.to_string(), "slicing errors".to_string()))
                }
            }
            Err(e) => {
                if passthrough {
                    // Slicing failed entirely - fall back to passthrough
                    do_passthrough(&format!("generation failed: {}", e))
                } else {
                    multi.println(format!("  {}: generation failed: {}", crate_name, e)).ok();
                    Err((crate_name.to_string(), e.to_string()))
                }
            }
        }
    };

    let phase3_results: Vec<Result<String, (String, String)>> = if parallel && num_to_generate > 1 {
        let state3_clone = state3.clone();
        let phase3_pb_clone = phase3_pb.clone();
        let stats3_pb_clone = stats3_pb.clone();

        crate_needed_vec.par_iter()
            .map(|(name, needed)| {
                // Check for abort before starting
                if state3_clone.is_aborted() {
                    return Err((name.clone(), "Aborted: threshold exceeded".to_string()));
                }

                let result = process_crate(name, needed);

                // Record result
                match &result {
                    Ok(_) => { state3_clone.record_success(); }
                    Err(_) => { state3_clone.record_error(); }
                }

                // Update progress
                phase3_pb_clone.inc(1);
                let (_, success, errors) = state3_clone.get_stats();
                stats3_pb_clone.set_message(format!("Success: {} | Errors: {}", success, errors));

                result
            })
            .collect()
    } else {
        crate_needed_vec.iter()
            .map(|(name, needed)| {
                // Check for abort
                if state3.is_aborted() {
                    return Err((name.clone(), "Aborted: threshold exceeded".to_string()));
                }

                let result = process_crate(name, needed);

                // Record result
                match &result {
                    Ok(_) => { state3.record_success(); }
                    Err(_) => { state3.record_error(); }
                }

                // Update progress
                phase3_pb.inc(1);
                let (_, success, errors) = state3.get_stats();
                stats3_pb.set_message(format!("Success: {} | Errors: {}", success, errors));

                result
            })
            .collect()
    };

    phase3_pb.finish_with_message("Done");
    let (_, success, errors) = state3.get_stats();
    stats3_pb.finish_with_message(format!("Phase 3 Finished: {} success, {} errors", success, errors));

    // Check if aborted
    if state3.is_aborted() {
        multi.println("\n  Phase 3 aborted: failure rate exceeded threshold").ok();
    }

    // Collect Phase 3 results
    let mut removed_count = 0;
    for r in phase3_results {
        match r {
            Ok(name) => result.success.push(name),
            Err((name, err)) => {
                if err == "0 items needed" {
                    removed_count += 1;
                } else {
                    result.failed.push((name, err));
                }
            }
        }
    }

    let phase3_elapsed = phase3_start.elapsed();

    multi.println(format!("\nPhase 3 completed: {} crates generated, {} removed (0 items) in {:.2}s",
             result.success.len(), removed_count, phase3_elapsed.as_secs_f64())).ok();

    // Measure after LOC if requested
    if measure {
        println!("\nPhase 3.5: Measuring sliced LOC...");
        let measure_pb = create_progress_bar(&multi, result.success.len() as u64, "Phase 3.5");
        measure_pb.set_message("measuring");
        measure_pb.enable_steady_tick(std::time::Duration::from_millis(100));

        for crate_name in &result.success {
            let output_dir = output_base.join(format!("{}-sliced", crate_name));
            if output_dir.exists() {
                let loc = crate::measure::measure_loc(&output_dir);
                if let Some(m) = result.measurements.get_mut(crate_name) {
                    m.after_loc = loc;
                }
            }
            measure_pb.inc(1);
        }
        measure_pb.finish_with_message("Done");
    }

    // Build sliced set from ALL crates that will be in the workspace (all *-sliced directories)
    // This includes both successful slices and failed slices that still have output directories
    let mut final_sliced_set: HashSet<String> = HashSet::new();
    if let Ok(entries) = fs::read_dir(output_base) {
        for entry in entries.filter_map(|e| e.ok()) {
            let name = entry.file_name().to_string_lossy().to_string();
            if name.ends_with("-sliced") && entry.path().is_dir() {
                let cargo_toml = entry.path().join("Cargo.toml");
                if cargo_toml.exists() {
                    // Extract the original crate name (without -sliced suffix)
                    let crate_name = name.trim_end_matches("-sliced").to_string();
                    final_sliced_set.insert(crate_name);
                }
            }
        }
    }

    // Rewrite Cargo.toml files to use sliced deps for ALL crates in the workspace
    // This ensures version consistency even for failed slices
    for crate_name in &final_sliced_set {
        let output_dir = output_base.join(format!("{}-sliced", crate_name));
        rewrite_cargo_toml_for_sliced_deps(&output_dir, &final_sliced_set, output_base);
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

    result
}

/// Rewrite Cargo.toml to use sliced dependencies instead of original ones
/// Also rewrites feature references and restores features that were incorrectly filtered
pub fn rewrite_cargo_toml_for_sliced_deps(crate_dir: &Path, sliced_set: &HashSet<String>, _output_base: &Path) {
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

    // Try to load original Cargo.toml features
    let original_features: HashMap<String, Vec<String>> = if let Some(ref name) = crate_name {
        load_original_crate_features(name)
    } else {
        HashMap::new()
    };

    // Collect existing dependencies (including those we'll rewrite)
    let mut existing_deps: HashSet<String> = HashSet::new();
    let mut optional_deps: HashSet<String> = HashSet::new();
    let mut in_deps_scan = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_deps_scan = trimmed == "[dependencies]";
        } else if in_deps_scan && !trimmed.is_empty() {
            if let Some(dep_name) = trimmed.split('=').next().map(|s| s.trim()) {
                if !dep_name.is_empty() {
                    existing_deps.insert(dep_name.to_string());
                    if trimmed.contains("optional = true") {
                        optional_deps.insert(dep_name.to_string());
                    }
                }
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

    for line in content.lines() {
        let trimmed = line.trim();

        // Track which section we're in
        if trimmed.starts_with('[') {
            // Before leaving features section, add missing features
            if in_features && !features_section_ended {
                features_section_ended = true;
                // Add features from original that reference sliced deps but were filtered out
                for (feature_name, refs) in &original_features {
                    if existing_features.contains(feature_name) {
                        continue; // Already have this feature
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
                    if should_add && !refs.is_empty() {
                        let fixed_refs = fix_feature_refs(refs, &existing_deps, &optional_deps);
                        if !fixed_refs.is_empty() {
                            let refs_str = fixed_refs.iter().map(|s| format!("\"{}\"", s)).collect::<Vec<_>>().join(", ");
                            new_content.push_str(&format!("{} = [{}]\n", feature_name, refs_str));
                        } else {
                            // Keep the feature but make it empty if all its deps became mandatory
                            new_content.push_str(&format!("{} = []\n", feature_name));
                        }
                    }
                }
            }
            in_dependencies = trimmed == "[dependencies]";
            in_features = trimmed == "[features]";
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

        new_content.push_str(line);
        new_content.push('\n');
    }

    // If features section was the last section, add missing features at the end
    if in_features && !features_section_ended {
        for (feature_name, refs) in &original_features {
            if existing_features.contains(feature_name) {
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
            if should_add && !refs.is_empty() {
                let fixed_refs = fix_feature_refs(refs, &existing_deps, &optional_deps);
                if !fixed_refs.is_empty() {
                    let refs_str = fixed_refs.iter().map(|s| format!("\"{}\"", s)).collect::<Vec<_>>().join(", ");
                    new_content.push_str(&format!("{} = [{}]\n", feature_name, refs_str));
                } else {
                    new_content.push_str(&format!("{} = []\n", feature_name));
                }
            }
        }
    }

    let _ = std::fs::write(&cargo_toml_path, new_content);
}

/// Fix feature references based on whether dependencies are optional or mandatory
fn fix_feature_refs(refs: &[String], existing_deps: &HashSet<String>, optional_deps: &HashSet<String>) -> Vec<String> {
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
        } else if r.contains('/') && r.contains('?') {
            // Check for X?/feature pattern
            let parts: Vec<&str> = r.split('/').collect();
            if parts.len() == 2 {
                let dep_part = parts[0];
                if dep_part.ends_with('?') {
                    let dep_name = &dep_part[..dep_part.len() - 1];
                    if existing_deps.contains(dep_name) && !optional_deps.contains(dep_name) {
                        // Convert X?/feature to X/feature if X is mandatory
                        return Some(format!("{}/{}", dep_name, parts[1]));
                    }
                }
            }
            Some(r.clone())
        } else {
            Some(r.clone())
        }
    }).collect()
}

/// Load features from the original crate's Cargo.toml
pub fn load_original_crate_features(crate_name: &str) -> HashMap<String, Vec<String>> {
    let mut features = HashMap::new();

    // Find the original crate path
    let crate_info = match find_crate_source(crate_name) {
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

/// Slice all direct dependencies
pub fn slice_all_deps(
    graph: &DepGraph,
    output_base: &Path,
    use_semantic: bool,
    auto_fix: bool,
    use_module_slice: bool,
    use_adaptive: bool,
    original_source: bool,
    measure: bool,
) -> SliceAllResult {
    let mut result = SliceAllResult::default();
    let src_dir = Path::new("src");

    // Setup MultiProgress
    let multi = MultiProgress::new();

    multi.println(format!("=== Slicing All Dependencies{} ===",
             if auto_fix { " (with auto-fix)" } else { "" })).ok();

    // Copy original sources if requested
    if original_source {
        if let Err(e) = copy_original_sources(graph, Path::new(".")) {
            multi.println(format!("Warning: Failed to copy original sources: {}", e)).ok();
        }
    }

    multi.println(format!("Max external deps: {}, Using original crate editions\n", MAX_EXTERNAL_DEPS)).ok();

    let direct_deps = graph.direct_deps();
    let pb = create_progress_bar(&multi, direct_deps.len() as u64, "Slicing");
    pb.set_message("crates");

    for crate_name in direct_deps {
        pb.set_message(format!("processing {}", crate_name));
        
        // Measure before LOC
        if measure {
            if let Some(info) = find_crate_source(crate_name) {
                let loc = crate::measure::measure_loc(&info.path);
                let mut m = crate::types::CrateMeasurement::default();
                m.name = crate_name.to_string();
                m.before_loc = loc;
                result.measurements.insert(crate_name.to_string(), m);
            }
        }

        // Get crate metadata from graph
        let dep_node = graph.nodes.get(crate_name);

        // Passthrough heuristic 1: Known FFI crates
        if FFI_CRATES.contains(&crate_name) {
            multi.println(format!("  {}: Passthrough: FFI crate (C bindings)", crate_name)).ok();
            result.passthrough.push((crate_name.to_string(), "FFI".to_string()));
            pb.inc(1);
            continue;
        }

        // Passthrough heuristic 2: Known proc-macro crates
        if PROC_MACRO_CRATES.contains(&crate_name) {
            multi.println(format!("  {}: Passthrough: Proc-macro crate", crate_name)).ok();
            result.passthrough.push((crate_name.to_string(), "proc-macro".to_string()));
            pb.inc(1);
            continue;
        }

        // Passthrough heuristic 3: Known complex crates (can bypass with FORCE_SLICE_ALL=1)
        if COMPLEX_CRATES.contains(&crate_name) && std::env::var("FORCE_SLICE_ALL").is_err() {
            multi.println(format!("  {}: Passthrough: Complex crate (deep dependencies)", crate_name)).ok();
            result.passthrough.push((crate_name.to_string(), "complex".to_string()));
            pb.inc(1);
            continue;
        }

        // Passthrough heuristic 4: Too many external dependencies
        if let Some(node) = dep_node {
            if node.deps.len() > MAX_EXTERNAL_DEPS {
                let reason = format!("{} deps > {}", node.deps.len(), MAX_EXTERNAL_DEPS);
                multi.println(format!("  {}: Passthrough: Too many deps ({})", crate_name, reason)).ok();
                result.passthrough.push((crate_name.to_string(), reason));
                pb.inc(1);
                continue;
            }
        }

        // Analyze usage
        let used = analyze_crate_usage(src_dir, crate_name);
        if used.is_empty() {
            multi.println(format!("  {}: Skipped: No usage found", crate_name)).ok();
            result.skipped.push(crate_name.to_string());
            pb.inc(1);
            continue;
        }

        // Find crate source
        let crate_info = match find_crate_source(crate_name) {
            Some(info) => info,
            None => {
                multi.println(format!("  {}: Failed: Could not locate source", crate_name)).ok();
                result.failed.push((crate_name.to_string(), "Source not found".to_string()));
                pb.inc(1);
                continue;
            }
        };

        // Generate sliced crate
        let output_dir = output_base.join(format!("{}-sliced", crate_name));

        let gen_result = if use_adaptive {
            // Adaptive mode: try semantic first, fallback to module if compilation fails
            match generate_semantic_sliced_crate(&crate_info, &used, &output_dir) {
                Ok(semantic_result) => {
                    if check_crate_compiles(&output_dir) {
                        Ok(format!("{} items (semantic)", semantic_result.items_included))
                    } else {
                        // Semantic failed, try module slicing
                        let _ = fs::remove_dir_all(&output_dir);
                        generate_module_sliced_crate(&crate_info, &used, &output_dir)
                            .map(|r| format!("{} files (module fallback)", r.items_included))
                    }
                }
                Err(_) => {
                    // Semantic slicing failed entirely, try module
                    generate_module_sliced_crate(&crate_info, &used, &output_dir)
                        .map(|r| format!("{} files (module fallback)", r.items_included))
                }
            }
        } else if use_module_slice {
            generate_module_sliced_crate(&crate_info, &used, &output_dir)
                .map(|r| format!("{} files", r.items_included))
        } else if use_semantic {
            generate_semantic_sliced_crate(&crate_info, &used, &output_dir)
                .map(|r| format!("{} items", r.items_included))
        } else {
            generate_sliced_crate(&crate_info, &used, &output_dir)
                .map(|_| "stub".to_string())
        };

        match gen_result {
            Ok(info) => {
                multi.println(format!("  {}: Generated ({})", crate_name, info)).ok();

                // Run auto-fix if enabled
                if auto_fix {
                    multi.println(format!("  {}: Auto-fixing...", crate_name)).ok();
                    match auto_fix_sliced_crate(&output_dir, 5) {
                        Ok(0) => {
                            multi.println(format!("  {}: already compiles!", crate_name)).ok();
                            result.success.push(crate_name.to_string());
                        }
                        Ok(n) => {
                            // Verify it now compiles
                            let check = Command::new("cargo")
                                .args(["check"])
                                .current_dir(&output_dir)
                                .output();
                            if check.map(|o| o.status.success()).unwrap_or(false) {
                                multi.println(format!("  {}: fixed in {} iterations!", crate_name, n)).ok();
                                result.success.push(crate_name.to_string());
                            } else {
                                multi.println(format!("  {}: partial fix ({} iterations), still has errors", crate_name, n)).ok();
                                result.failed.push((crate_name.to_string(), "Partial fix".to_string()));
                            }
                        }
                        Err(e) => {
                            multi.println(format!("  {}: error: {}", crate_name, e)).ok();
                            result.failed.push((crate_name.to_string(), e));
                        }
                    }
                } else {
                    result.success.push(crate_name.to_string());
                }

                // Measure after LOC
                if measure {
                    if output_dir.exists() {
                        let loc = crate::measure::measure_loc(&output_dir);
                        if let Some(m) = result.measurements.get_mut(crate_name) {
                            m.after_loc = loc;
                        }
                    }
                }
            }
            Err(e) => {
                multi.println(format!("  {}: Failed: {}", crate_name, e)).ok();
                result.failed.push((crate_name.to_string(), e.to_string()));
            }
        }
        pb.inc(1);
    }

    pb.finish_with_message("Done");

    multi.println("\n=== Summary ===").ok();
    multi.println(format!("  Success: {}", result.success.len())).ok();
    multi.println(format!("  Failed: {}", result.failed.len())).ok();
    multi.println(format!("  Passthrough: {} (complexity heuristics)", result.passthrough.len())).ok();
    if !result.passthrough.is_empty() {
        for (name, reason) in &result.passthrough {
            multi.println(format!("    - {}: {}", name, reason)).ok();
        }
    }
    multi.println(format!("  Skipped: {} (no usage)", result.skipped.len())).ok();

    result
}

/// Generate workspace Cargo.toml for sliced crates
/// Scans the output directory for all *-sliced directories and includes them all
pub fn generate_workspace_toml(output_base: &Path, _crates: &[String]) -> Result<(), String> {
    let workspace_toml = output_base.join("Cargo.toml");

    // Scan for all *-sliced directories with valid Cargo.toml
    let mut all_members: Vec<String> = Vec::new();
    if let Ok(entries) = fs::read_dir(output_base) {
        for entry in entries.filter_map(|e| e.ok()) {
            let name = entry.file_name().to_string_lossy().to_string();
            if name.ends_with("-sliced") && entry.path().is_dir() {
                let cargo_toml = entry.path().join("Cargo.toml");
                if cargo_toml.exists() {
                    all_members.push(name);
                }
            }
        }
    }
    all_members.sort();

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
        generate_workspace_toml(base, &[]).expect("Failed to generate workspace");

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
        
        let refs = vec![
            "dep:log".to_string(),      // log is mandatory, should be skipped
            "dep:memchr".to_string(),   // memchr is optional, should be kept
            "log".to_string(),          // log is mandatory dependency, should be skipped
            "std".to_string(),          // std is likely a feature, should be kept
            "memchr?/std".to_string(),  // memchr is optional, should be kept as is
            "log?/std".to_string(),     // log is mandatory, should become log/std
        ];
        
        let fixed = fix_feature_refs(&refs, &existing_deps, &optional_deps);
        
        assert!(!fixed.contains(&"dep:log".to_string()));
        assert!(!fixed.contains(&"log".to_string()));
        assert!(fixed.contains(&"dep:memchr".to_string()));
        assert!(fixed.contains(&"std".to_string()));
        assert!(fixed.contains(&"memchr?/std".to_string()));
        assert!(fixed.contains(&"log/std".to_string()));
        assert!(!fixed.contains(&"log?/std".to_string()));
    }
}

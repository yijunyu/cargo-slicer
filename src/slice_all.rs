//! Multi-crate slicing functionality.
//!
//! Functions for slicing all dependencies in a workspace.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use rayon::prelude::*;

use super::types::{DepGraph, SliceAllResult, UsedItem, ItemKind, CrateInfo};
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
fn copy_dir_recursive(src: &std::path::Path, dst: &std::path::Path) -> std::io::Result<()> {
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
) -> SliceAllResult {
    let mut result = SliceAllResult::default();
    let src_dir = Path::new("src");
    let total_start = Instant::now();

    let mode_str = match (parallel, profile_crate) {
        (_, Some(crate_name)) => format!(" (profiling: {})", crate_name),
        (true, None) => " (parallel)".to_string(),
        (false, None) => " (sequential)".to_string(),
    };
    println!("=== Union Slicing Dependencies (Transitive){}{} ===",
             if auto_fix { " (with auto-fix)" } else { "" }, mode_str);

    // First, identify all sliceable crates in the dependency tree
    let all_deps = graph.all_deps();
    let mut sliceable_crates: HashSet<String> = HashSet::new();

    println!("Phase 0: Identifying sliceable crates in dependency tree...\n");
    println!("  Total crates in tree: {}", all_deps.len());

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

    println!("  Sliceable crates: {} ({:?})\n", sliceable_crates.len(),
             sliceable_crates.iter().take(10).collect::<Vec<_>>());

    println!("Phase 1: Collecting all item usages across dependency tree...\n");

    // Phase 1: For each sliceable crate, collect items used by ALL crates that depend on it
    // Key: crate name, Value: set of all items used from that crate
    let mut crate_items: HashMap<String, HashSet<String>> = HashMap::new();
    let mut crate_used: HashMap<String, HashSet<UsedItem>> = HashMap::new();

    for target_crate in &sliceable_crates {
        let mut all_used: HashSet<UsedItem> = HashSet::new();

        // 1. Check usage from project's src/
        let project_used = analyze_crate_usage(src_dir, target_crate);
        if !project_used.is_empty() {
            println!("  {}: {} items from project src/", target_crate, project_used.len());
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
                        println!("  {}: {} items from {} (dependent)", target_crate, dep_used.len(), dependent);
                        all_used.extend(dep_used);
                    }
                }
            }
        }

        if all_used.is_empty() {
            continue;
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

        if !items.is_empty() {
            println!("  {}: {} unique items total", target_crate, items.len());
            crate_items.entry(target_crate.to_string())
                .or_insert_with(HashSet::new)
                .extend(items);
            crate_used.entry(target_crate.to_string())
                .or_insert_with(HashSet::new)
                .extend(all_used);
        }
    }

    if crate_items.is_empty() {
        println!("\nNo crate usage found to slice.\n");
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
            println!("\nCrate '{}' not found in sliceable dependencies.\n", target);
        }
        return result;
    }

    let phase2_start = Instant::now();
    let num_crates = crate_items.len();

    println!("\nPhase 2: Running rust-analyzer on {} crate(s){}...\n",
             num_crates, if parallel { " (parallel)" } else { "" });

    // Phase 2: For each crate, run ra_deps on the union of all items
    // Use parallel processing when enabled
    let crate_items_vec: Vec<(String, HashSet<String>)> = crate_items.into_iter().collect();

    let mut crate_needed: HashMap<String, HashSet<String>>;
    let phase2_failures: Vec<(String, String)>;

    if parallel && num_crates > 1 {
        // Parallel processing with rayon
        let results: Vec<(String, Result<HashSet<String>, String>)> = crate_items_vec
            .par_iter()
            .map(|(crate_name, items)| {
                let start = Instant::now();
                let crate_info = match find_crate_source(crate_name) {
                    Some(info) => info,
                    None => {
                        return (crate_name.clone(), Err("Source not found".to_string()));
                    }
                };

                let seed_items: Vec<String> = items.iter().cloned().collect();

                match run_ra_deps(&crate_info.path, &seed_items) {
                    Ok(needed) => {
                        let elapsed = start.elapsed();
                        println!("  {}: {} seed -> {} needed ({:.2}s)",
                                 crate_name, seed_items.len(), needed.len(), elapsed.as_secs_f64());
                        (crate_name.clone(), Ok(needed))
                    }
                    Err(e) => {
                        println!("  {}: rust-analyzer failed: {}", crate_name, e);
                        (crate_name.clone(), Err(format!("ra_deps: {}", e)))
                    }
                }
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
            let start = Instant::now();
            let crate_info = match find_crate_source(crate_name) {
                Some(info) => info,
                None => {
                    println!("  {}: source not found", crate_name);
                    failures.push((crate_name.clone(), "Source not found".to_string()));
                    continue;
                }
            };

            let seed_items: Vec<String> = items.iter().cloned().collect();
            println!("  {}: analyzing {} seed items...", crate_name, seed_items.len());

            match run_ra_deps(&crate_info.path, &seed_items) {
                Ok(needed) => {
                    let elapsed = start.elapsed();
                    println!("    -> {} transitive items needed ({:.2}s)", needed.len(), elapsed.as_secs_f64());
                    needed_map.insert(crate_name.clone(), needed);
                }
                Err(e) => {
                    println!("    -> rust-analyzer failed: {}", e);
                    failures.push((crate_name.clone(), format!("ra_deps: {}", e)));
                }
            }
        }
        crate_needed = needed_map;
        phase2_failures = failures;
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
            println!("  Skipping {} (0 items needed)", name);
            return false;
        }
        true
    });
    let skipped = before_filter - crate_needed.len();
    if skipped > 0 {
        println!("\nSkipped {} unused crate(s)", skipped);
    }

    // Collect the set of crates we're slicing (for Cargo.toml rewriting)
    let _sliced_set: HashSet<String> = crate_needed.keys().cloned().collect();

    let phase3_start = Instant::now();
    let num_to_generate = crate_needed.len();
    println!("\nPhase 3: Generating {} sliced crate(s){}...\n",
             num_to_generate, if parallel { " (parallel)" } else { "" });

    // Phase 3: Generate sliced crates using the computed needed sets
    let crate_needed_vec: Vec<(String, HashSet<String>)> = crate_needed.into_iter().collect();

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

        // Helper to fallback to passthrough
        let do_passthrough = |reason: &str| -> Result<String, (String, String)> {
            println!("  {}: falling back to passthrough ({})", crate_name, reason);
            match copy_crate_as_passthrough(&crate_info, &output_dir) {
                Ok(()) => {
                    let elapsed = start.elapsed();
                    println!("  {}: passthrough ({:.2}s)", crate_name, elapsed.as_secs_f64());
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

                // Delete crates with 0 items - they're unused stubs
                if slice_result.items_included == 0 {
                    let elapsed = start.elapsed();
                    println!("  {}: 0 items, removing ({:.2}s)", crate_name, elapsed.as_secs_f64());
                    let _ = fs::remove_dir_all(&output_dir);
                    return Err((crate_name.to_string(), "0 items needed".to_string()));
                }

                // Check if sliced crate compiles
                let compiles = if auto_fix {
                    match auto_fix_sliced_crate(&output_dir, 5) {
                        Ok(0) => {
                            let elapsed = start.elapsed();
                            println!("  {}: {} items, compiles ({:.2}s)",
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
                                println!("  {}: {} items, fixed in {} iters ({:.2}s)",
                                         crate_name, slice_result.items_included, n, elapsed.as_secs_f64());
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
                        println!("  {}: {} items ({:.2}s)",
                                 crate_name, slice_result.items_included, elapsed.as_secs_f64());
                    }
                    Ok(crate_name.to_string())
                } else if passthrough {
                    // Slicing produced errors - fall back to passthrough
                    do_passthrough("slicing errors")
                } else {
                    // Passthrough disabled - report as failure
                    let elapsed = start.elapsed();
                    println!("  {}: {} items, has errors ({:.2}s)",
                             crate_name, slice_result.items_included, elapsed.as_secs_f64());
                    Err((crate_name.to_string(), "slicing errors".to_string()))
                }
            }
            Err(e) => {
                if passthrough {
                    // Slicing failed entirely - fall back to passthrough
                    do_passthrough(&format!("generation failed: {}", e))
                } else {
                    println!("  {}: generation failed: {}", crate_name, e);
                    Err((crate_name.to_string(), e.to_string()))
                }
            }
        }
    };

    let phase3_results: Vec<Result<String, (String, String)>> = if parallel && num_to_generate > 1 {
        crate_needed_vec.par_iter()
            .map(|(name, needed)| process_crate(name, needed))
            .collect()
    } else {
        crate_needed_vec.iter()
            .map(|(name, needed)| process_crate(name, needed))
            .collect()
    };

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

    println!("\nPhase 3 completed: {} crates generated, {} removed (0 items) in {:.2}s",
             result.success.len(), removed_count, phase3_elapsed.as_secs_f64());

    // Rewrite Cargo.toml files to use sliced deps (only for successful crates)
    let final_sliced_set: HashSet<String> = result.success.iter().cloned().collect();
    for crate_name in &result.success {
        let output_dir = output_base.join(format!("{}-sliced", crate_name));
        rewrite_cargo_toml_for_sliced_deps(&output_dir, &final_sliced_set, output_base);
    }

    let total_elapsed = total_start.elapsed();

    println!("\n=== Union Slice Summary ({:.2}s total) ===", total_elapsed.as_secs_f64());
    println!("  Success: {}", result.success.len());
    if removed_count > 0 {
        println!("  Removed: {} (0 items needed)", removed_count);
    }
    println!("  Failed: {}", result.failed.len());
    if !result.failed.is_empty() {
        for (name, reason) in &result.failed {
            println!("    - {}: {}", name, reason);
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
    let mut in_deps_scan = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_deps_scan = trimmed == "[dependencies]";
        } else if in_deps_scan && !trimmed.is_empty() {
            if let Some(dep_name) = trimmed.split('=').next().map(|s| s.trim()) {
                if !dep_name.is_empty() {
                    existing_deps.insert(dep_name.to_string());
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
                        let refs_str = refs.iter().map(|s| format!("\"{}\"", s)).collect::<Vec<_>>().join(", ");
                        new_content.push_str(&format!("{} = [{}]\n", feature_name, refs_str));
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
                let refs_str = refs.iter().map(|s| format!("\"{}\"", s)).collect::<Vec<_>>().join(", ");
                new_content.push_str(&format!("{} = [{}]\n", feature_name, refs_str));
            }
        }
    }

    let _ = std::fs::write(&cargo_toml_path, new_content);
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
) -> SliceAllResult {
    let mut result = SliceAllResult::default();
    let src_dir = Path::new("src");

    println!("=== Slicing All Dependencies{} ===",
             if auto_fix { " (with auto-fix)" } else { "" });
    println!("Max external deps: {}, Using original crate editions\n", MAX_EXTERNAL_DEPS);

    for crate_name in graph.direct_deps() {
        println!("Processing: {}", crate_name);

        // Get crate metadata from graph
        let dep_node = graph.nodes.get(crate_name);

        // Passthrough heuristic 1: Known FFI crates
        if FFI_CRATES.contains(&crate_name) {
            println!("  Passthrough: FFI crate (C bindings)\n");
            result.passthrough.push((crate_name.to_string(), "FFI".to_string()));
            continue;
        }

        // Passthrough heuristic 2: Known proc-macro crates
        if PROC_MACRO_CRATES.contains(&crate_name) {
            println!("  Passthrough: Proc-macro crate\n");
            result.passthrough.push((crate_name.to_string(), "proc-macro".to_string()));
            continue;
        }

        // Passthrough heuristic 3: Known complex crates (can bypass with FORCE_SLICE_ALL=1)
        if COMPLEX_CRATES.contains(&crate_name) && std::env::var("FORCE_SLICE_ALL").is_err() {
            println!("  Passthrough: Complex crate (deep dependencies)\n");
            result.passthrough.push((crate_name.to_string(), "complex".to_string()));
            continue;
        }

        // Passthrough heuristic 4: Too many external dependencies
        if let Some(node) = dep_node {
            if node.deps.len() > MAX_EXTERNAL_DEPS {
                let reason = format!("{} deps > {}", node.deps.len(), MAX_EXTERNAL_DEPS);
                println!("  Passthrough: Too many deps ({})\n", reason);
                result.passthrough.push((crate_name.to_string(), reason));
                continue;
            }
        }

        // Note: Edition mismatch no longer triggers passthrough - we use original edition

        // Analyze usage
        let used = analyze_crate_usage(src_dir, crate_name);
        if used.is_empty() {
            println!("  Skipped: No usage found\n");
            result.skipped.push(crate_name.to_string());
            continue;
        }

        // Find crate source
        let crate_info = match find_crate_source(crate_name) {
            Some(info) => info,
            None => {
                println!("  Failed: Could not locate source\n");
                result.failed.push((crate_name.to_string(), "Source not found".to_string()));
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
                println!("  Generated: {} ({})", output_dir.display(), info);

                // Run auto-fix if enabled
                if auto_fix {
                    print!("  Auto-fixing...");
                    match auto_fix_sliced_crate(&output_dir, 5) {
                        Ok(0) => {
                            println!(" already compiles!");
                            result.success.push(crate_name.to_string());
                        }
                        Ok(n) => {
                            // Verify it now compiles
                            let check = Command::new("cargo")
                                .args(["check"])
                                .current_dir(&output_dir)
                                .output();
                            if check.map(|o| o.status.success()).unwrap_or(false) {
                                println!(" fixed in {} iterations!", n);
                                result.success.push(crate_name.to_string());
                            } else {
                                println!(" partial fix ({} iterations), still has errors", n);
                                result.failed.push((crate_name.to_string(), "Partial fix".to_string()));
                            }
                        }
                        Err(e) => {
                            println!(" {}", e);
                            result.failed.push((crate_name.to_string(), e));
                        }
                    }
                } else {
                    result.success.push(crate_name.to_string());
                }
                println!();
            }
            Err(e) => {
                println!("  Failed: {}\n", e);
                result.failed.push((crate_name.to_string(), e.to_string()));
            }
        }
    }

    println!("\n=== Summary ===");
    println!("  Success: {}", result.success.len());
    println!("  Failed: {}", result.failed.len());
    println!("  Passthrough: {} (complexity heuristics)", result.passthrough.len());
    if !result.passthrough.is_empty() {
        for (name, reason) in &result.passthrough {
            println!("    - {}: {}", name, reason);
        }
    }
    println!("  Skipped: {} (no usage)", result.skipped.len());

    result
}

/// Generate workspace Cargo.toml for sliced crates
pub fn generate_workspace_toml(output_base: &Path, crates: &[String]) -> Result<(), String> {
    let workspace_toml = output_base.join("Cargo.toml");

    let mut content = String::from("[workspace]\nresolver = \"2\"\nmembers = [\n");
    for name in crates {
        content.push_str(&format!("    \"{}-sliced\",\n", name));

        // Remove [workspace] from member crates to avoid conflict
        let member_toml = output_base.join(format!("{}-sliced", name)).join("Cargo.toml");
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

    println!("Generated: {}", workspace_toml.display());
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

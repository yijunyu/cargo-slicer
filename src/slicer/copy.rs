//! Copy original source code to output directory

use std::path::{Path, PathBuf};
use std::fs;
use std::collections::HashSet;
use rayon::prelude::*;
use crate::constants::PLATFORM_SPECIFIC_CRATES;
use crate::debug_log;

use std::collections::HashMap;
use std::sync::RwLock;

/// Version mapping for multi-version dependencies.
/// Key: (consumer_name, consumer_version, dep_name)
/// Value: dep_version
/// This is populated from cargo metadata resolve graph.
pub type VersionMap = HashMap<(String, String, String), String>;

/// Map of crate names to their available versions in the dependency tree.
pub type MultiVersionCrates = HashMap<String, Vec<String>>;

/// Global storage for multi-version info (set once at start, read during slicing)
static MULTI_VERSION_INFO: RwLock<Option<MultiVersionData>> = RwLock::new(None);

/// Data needed for multi-version routing during Cargo.toml rewriting
#[derive(Clone, Default)]
pub struct MultiVersionData {
    pub version_map: VersionMap,
    pub multi_version_crates: MultiVersionCrates,
}

/// Initialize the global multi-version info (call once before slicing)
pub fn set_multi_version_info(version_map: VersionMap, multi_version_crates: MultiVersionCrates, verbose: bool) {
    if verbose {
        println!("  Setting multi-version info: {} crates, {} mappings",
            multi_version_crates.len(), version_map.len());
        if !multi_version_crates.is_empty() {
            for (name, versions) in multi_version_crates.iter().take(5) {
                println!("    {} → versions: {:?}", name, versions);
            }
        }
    }
    let data = MultiVersionData { version_map, multi_version_crates };
    if let Ok(mut guard) = MULTI_VERSION_INFO.write() {
        *guard = Some(data);
    }
}

/// Get the multi-version data for routing lookups
fn get_multi_version_info() -> Option<MultiVersionData> {
    MULTI_VERSION_INFO.read().ok().and_then(|g| g.clone())
}

/// Extract crate name from a sliced directory name.
///
/// Handles both regular and versioned names:
/// - `memchr-sliced` → `memchr`
/// - `bitflags-1.3.2-sliced` → `bitflags`
pub fn extract_crate_name_from_dir(dir_name: &str) -> &str {
    // Strip -sliced suffix first
    let name = dir_name.strip_suffix("-sliced").unwrap_or(dir_name);

    // Check if it ends with a version pattern (e.g., -1.3.2)
    // Version pattern: last segment starts with a digit
    if let Some(last_dash) = name.rfind('-') {
        let potential_version = &name[last_dash + 1..];
        if potential_version.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            return &name[..last_dash];
        }
    }

    name
}

/// Generate the sliced output directory name for a crate.
///
/// For single-version crates: `memchr-sliced`
/// For multi-version crates: `bitflags-1.3.2-sliced`
pub fn sliced_dir_name(crate_name: &str, version: Option<&str>, is_multi_version: bool) -> String {
    if is_multi_version {
        if let Some(v) = version {
            format!("{}-{}-sliced", crate_name, v)
        } else {
            format!("{}-sliced", crate_name)
        }
    } else {
        format!("{}-sliced", crate_name)
    }
}

/// Re-fix Cargo.toml dependencies with a complete cache of sliced crates.
/// Called after Phase 5 completes to fix race condition issues.
pub fn refix_cargo_toml_deps(output_dir: &Path, sliced_crates: &HashSet<String>) -> Result<(), String> {
    let cargo_toml_path = output_dir.join("Cargo.toml");
    if !cargo_toml_path.exists() {
        return Ok(());
    }

    let content = fs::read_to_string(&cargo_toml_path)
        .map_err(|e| format!("Failed to read Cargo.toml: {}", e))?;

    let dir_name = output_dir.file_name().unwrap().to_str().unwrap();

    // Pass 1: Collect metadata
    let (optional_deps, deps_needing_implicit_features, existing_features, uses_rand_core_getrandom) =
        analyze_cargo_toml_metadata(&content, dir_name);

    // Pass 2: Apply all fixes with the complete cache
    let new_content = apply_cargo_toml_fixes(
        &content,
        output_dir,
        sliced_crates,
        &optional_deps,
        &deps_needing_implicit_features,
        &existing_features,
        uses_rand_core_getrandom,
    )?;

    // Pass 3: Suppress unexpected_cfgs warnings (build.rs stripped by slicer)
    let new_content = ensure_lint_suppressions(&new_content);

    // Only write if content changed
    if new_content != content {
        fs::write(&cargo_toml_path, &new_content)
            .map_err(|e| format!("Failed to write fixed Cargo.toml: {}", e))?;
    }

    Ok(())
}

pub fn copy_source(source_dir: &Path, output_dir: &Path) -> Result<(), String> {
    fs::create_dir_all(output_dir)
        .map_err(|e| format!("Failed to create output dir: {}", e))?;

    // Recursively copy all files
    copy_dir_recursive(source_dir, output_dir)?;

    // Strip include_str! for README files (they often don't exist in sliced crates)
    strip_readme_includes(output_dir)?;

    // Fix bare extern blocks (deprecated in Rust 2024 edition)
    fix_bare_extern_blocks(output_dir)?;

    // Apply all Cargo.toml fixes in a single pass (read once, write once)
    fix_cargo_toml_all(output_dir)?;

    Ok(())
}

/// Strip include_str! for README files from lib.rs files
/// These often reference files that don't exist in sliced crates and cause build errors
fn strip_readme_includes(output_dir: &Path) -> Result<(), String> {
    let lib_rs = output_dir.join("src/lib.rs");
    if !lib_rs.exists() {
        return Ok(());
    }

    let content = match fs::read_to_string(&lib_rs) {
        Ok(c) => c,
        Err(_) => return Ok(()), // Skip if we can't read
    };

    // Check if file contains README include
    if !content.contains("include_str!") ||
       (!content.contains("README") && !content.contains("readme")) {
        return Ok(());
    }

    // Remove lines containing #![doc = include_str!("../README.md")] or similar
    let mut new_lines = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        // Skip lines that are README doc includes
        if trimmed.starts_with("#![doc") &&
           trimmed.contains("include_str!") &&
           (trimmed.contains("README") || trimmed.contains("readme")) {
            continue;
        }
        new_lines.push(line);
    }

    let new_content = new_lines.join("\n");
    if new_content != content {
        fs::write(&lib_rs, new_content)
            .map_err(|e| format!("Failed to write stripped lib.rs: {}", e))?;
    }

    Ok(())
}

/// All lint suppressions needed for sliced third-party code.
/// These suppress warnings from code style issues in upstream crates
/// and from artifacts of the slicing process (e.g., stripped build.rs).
const SLICED_LINT_SUPPRESSIONS: &[(&str, &str)] = &[
    ("unexpected_cfgs", "{ level = \"allow\" }"),
    ("dead_code", "\"allow\""),
    ("unused_must_use", "\"allow\""),
    ("mismatched_lifetime_syntaxes", "\"allow\""),
    ("unpredictable_function_pointer_comparisons", "\"allow\""),
];

/// Suppress warnings in sliced Cargo.toml by adding `[lints.rust]` entries.
/// Handles: unexpected_cfgs, dead_code, unused_must_use,
/// mismatched_lifetime_syntaxes, unpredictable_function_pointer_comparisons.
fn ensure_lint_suppressions(content: &str) -> String {
    // Collect which lints still need to be added
    let mut missing_lints: Vec<(&str, &str)> = Vec::new();
    for &(key, value) in SLICED_LINT_SUPPRESSIONS {
        let pattern = format!("{} = ", key);
        if !content.contains(&pattern) {
            missing_lints.push((key, value));
        }
    }

    // All lints already present - nothing to do
    if missing_lints.is_empty() {
        return content.to_string();
    }

    // Case 1: Already has [lints.rust.unexpected_cfgs] sub-table - change level to "allow"
    // and add missing lints to [lints.rust] (or create it)
    let mut result = content.to_string();
    if result.contains("[lints.rust.unexpected_cfgs]") {
        let mut new_result = String::new();
        let mut in_section = false;
        for line in result.lines() {
            let trimmed = line.trim();
            if trimmed == "[lints.rust.unexpected_cfgs]" {
                in_section = true;
            } else if trimmed.starts_with('[') {
                in_section = false;
            }
            if in_section && trimmed.starts_with("level") {
                new_result.push_str("level = \"allow\"\n");
                continue;
            }
            new_result.push_str(line);
            new_result.push('\n');
        }
        result = new_result;
        // Remove unexpected_cfgs from missing list since we handled it
        missing_lints.retain(|&(k, _)| k != "unexpected_cfgs");
    }

    if missing_lints.is_empty() {
        return result;
    }

    // Case 2: Has [lints.rust] section - add missing lints after the header
    if result.contains("[lints.rust]") {
        let mut new_result = String::new();
        for line in result.lines() {
            new_result.push_str(line);
            new_result.push('\n');
            if line.trim() == "[lints.rust]" {
                for &(key, value) in &missing_lints {
                    new_result.push_str(&format!("{} = {}\n", key, value));
                }
            }
        }
        return new_result;
    }

    // Case 3: No [lints.rust] section - append one with all lints
    if !result.ends_with('\n') {
        result.push('\n');
    }
    result.push_str("\n[lints.rust]\n");
    for &(key, value) in &missing_lints {
        result.push_str(&format!("{} = {}\n", key, value));
    }
    result
}

/// Fix bare `extern { }` blocks by adding explicit "C" ABI.
/// Bare extern blocks are deprecated in Rust 2024 edition.
/// Since `extern { }` ≡ `extern "C" { }`, this is semantically safe.
fn fix_bare_extern_blocks(output_dir: &Path) -> Result<(), String> {
    let src_dir = output_dir.join("src");
    let search_dir = if src_dir.exists() { &src_dir } else { output_dir };

    fn visit_and_fix(dir: &Path) -> Result<(), String> {
        for entry in fs::read_dir(dir).map_err(|e| format!("{}: {}", dir.display(), e))?.flatten() {
            let path = entry.path();
            if path.is_dir() {
                visit_and_fix(&path)?;
            } else if path.extension().map_or(false, |e| e == "rs") {
                let content = match fs::read_to_string(&path) {
                    Ok(c) => c,
                    Err(_) => continue,
                };
                if !content.contains("extern {") && !content.contains("extern{") {
                    continue;
                }
                let new_content = content
                    .replace("extern {", "extern \"C\" {")
                    .replace("extern{", "extern \"C\" {");
                if new_content != content {
                    fs::write(&path, &new_content)
                        .map_err(|e| format!("write {}: {}", path.display(), e))?;
                }
            }
        }
        Ok(())
    }

    visit_and_fix(search_dir)
}

/// Unified Cargo.toml fixer - applies all fixes in a single read/write pass
/// Combines: optional deps, sliced deps, and compatibility fixes
pub fn fix_cargo_toml_all(output_dir: &Path) -> Result<(), String> {
    let cargo_toml_path = output_dir.join("Cargo.toml");
    if !cargo_toml_path.exists() {
        return Ok(());
    }

    let content = fs::read_to_string(&cargo_toml_path)
        .map_err(|e| format!("Failed to read Cargo.toml: {}", e))?;

    // Find the sliced crates directory (parent of output_dir)
    let sliced_root = output_dir
        .parent()
        .ok_or_else(|| "Cannot find parent directory".to_string())?;

    // Cache existence checks for sliced dependencies
    // For versioned directories (e.g., bitflags-1.3.2-sliced), insert BOTH
    // the full name (bitflags-1.3.2) and the base name (bitflags) so that
    // dependency lookups can find a match.
    let mut sliced_deps_cache: HashSet<String> = HashSet::new();
    if let Ok(entries) = fs::read_dir(sliced_root) {
        for entry in entries.flatten() {
            if let Some(name) = entry.file_name().to_str() {
                if name.ends_with("-sliced") {
                    let dep_name = name.trim_end_matches("-sliced");
                    sliced_deps_cache.insert(dep_name.to_string());

                    // For versioned directories, also insert base crate name
                    let base_name = extract_crate_name_from_dir(name);
                    if base_name != dep_name {
                        sliced_deps_cache.insert(base_name.to_string());
                    }
                }
            }
        }
    }

    // Also check for sliced deps in the same directory (for single-crate slicing)
    if let Ok(entries) = fs::read_dir(output_dir) {
        for entry in entries.flatten() {
            if let Some(name) = entry.file_name().to_str() {
                if name.ends_with("-sliced") {
                    let dep_name = name.trim_end_matches("-sliced");
                    sliced_deps_cache.insert(format!("same:{}", dep_name));

                    // For versioned directories, also insert base crate name
                    let base_name = extract_crate_name_from_dir(name);
                    if base_name != dep_name {
                        sliced_deps_cache.insert(format!("same:{}", base_name));
                    }
                }
            }
        }
    }

    let dir_name = output_dir.file_name().unwrap().to_str().unwrap();

    // Pass 1: Collect metadata (optional deps, features info)
    let (optional_deps, deps_needing_implicit_features, existing_features, uses_rand_core_getrandom) =
        analyze_cargo_toml_metadata(&content, dir_name);

    // Pass 2: Apply all fixes
    let new_content = apply_cargo_toml_fixes(
        &content,
        output_dir,
        &sliced_deps_cache,
        &optional_deps,
        &deps_needing_implicit_features,
        &existing_features,
        uses_rand_core_getrandom,
    )?;

    // Pass 3: Suppress unexpected_cfgs warnings (build.rs stripped by slicer)
    let new_content = ensure_lint_suppressions(&new_content);

    // NOTE: We do NOT rename package names for versioned crates because:
    // 1. Path dependencies require the package name to match the dependency name
    // 2. Renaming breaks cargo's ability to resolve path deps
    // Instead, we exclude duplicate package names from the workspace in generate_workspace_toml

    // Only write if content changed
    if new_content != content {
        fs::write(&cargo_toml_path, &new_content)
            .map_err(|e| format!("Failed to write fixed Cargo.toml: {}", e))?;
    }

    Ok(())
}

/// Analyze Cargo.toml to collect metadata needed for fixes
/// Returns: (optional_deps, deps_needing_implicit_features, existing_features, uses_rand_core_getrandom)
fn analyze_cargo_toml_metadata(content: &str, dir_name: &str) -> (HashSet<String>, HashSet<String>, HashSet<String>, bool) {
    let mut optional_deps = HashSet::new();
    let mut existing_features = HashSet::new();
    let mut deps_needing_implicit_features = HashSet::new();
    let mut deps_with_explicit_dep_prefix = HashSet::new();
    // Detect if features reference rand_core/getrandom (needs registry 0.6, not sliced 0.9)
    let mut uses_rand_core_getrandom = false;

    // Find optional dependencies (including target-specific ones)
    let mut current_dep: Option<String> = None;
    for line in content.lines() {
        let trimmed = line.trim();
        // Check for regular dependencies
        if trimmed.starts_with("[dependencies.") {
            let dep_name = trimmed
                .trim_start_matches("[dependencies.")
                .trim_end_matches(']')
                .to_string();
            current_dep = Some(dep_name);
        }
        // Check for target-specific dependencies like [target.'cfg(...)'.dependencies.name]
        else if trimmed.starts_with("[target.") && trimmed.contains(".dependencies.") {
            if let Some(dep_start) = trimmed.rfind(".dependencies.") {
                let dep_name = trimmed[dep_start + ".dependencies.".len()..].trim_end_matches(']').to_string();
                current_dep = Some(dep_name);
            }
        }
        else if trimmed.starts_with('[') {
            current_dep = None;
        } else if let Some(ref dep) = current_dep {
            if trimmed == "optional = true" {
                optional_deps.insert(dep.clone());
            }
        }
    }

    // Analyze features section
    let mut in_features_section = false;
    let mut current_feature_name: Option<String> = None;
    let mut current_feature_value = String::new();

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with("[features]") {
            in_features_section = true;
            continue;
        } else if trimmed.starts_with('[') {
            // Process last feature before leaving section
            if let Some(feature_name) = current_feature_name.take() {
                existing_features.insert(feature_name.clone());
                check_feature_references(
                    &feature_name,
                    &current_feature_value,
                    &optional_deps,
                    &mut deps_needing_implicit_features,
                    &mut deps_with_explicit_dep_prefix,
                    dir_name,
                );
                current_feature_value.clear();
            }
            in_features_section = false;
            continue;
        }

        if in_features_section && !trimmed.is_empty() && !trimmed.starts_with('#') {
            // Detect rand_core/getrandom reference (sliced rand_core 0.9 renamed this to os_rng)
            if trimmed.contains("rand_core/getrandom") {
                uses_rand_core_getrandom = true;
            }

            if let Some(eq_pos) = trimmed.find('=') {
                // New feature - process previous one first
                if let Some(feature_name) = current_feature_name.take() {
                    existing_features.insert(feature_name.clone());
                    check_feature_references(
                        &feature_name,
                        &current_feature_value,
                        &optional_deps,
                        &mut deps_needing_implicit_features,
                        &mut deps_with_explicit_dep_prefix,
                        dir_name,
                    );
                    current_feature_value.clear();
                }

                let feature_name = trimmed[..eq_pos].trim();
                current_feature_name = Some(feature_name.to_string());
                current_feature_value.push_str(&trimmed[eq_pos + 1..]);
            } else if current_feature_name.is_some() {
                // Multi-line array continuation
                current_feature_value.push(' ');
                current_feature_value.push_str(trimmed);
            }
        }
    }

    // Process last feature (EOF case)
    if let Some(feature_name) = current_feature_name.take() {
        existing_features.insert(feature_name.clone());
        check_feature_references(
            &feature_name,
            &current_feature_value,
            &optional_deps,
            &mut deps_needing_implicit_features,
            &mut deps_with_explicit_dep_prefix,
            dir_name,
        );
    }

    (optional_deps, deps_needing_implicit_features, existing_features, uses_rand_core_getrandom)
}

/// Check if a feature has bare references to optional dependencies
fn check_feature_references(
    feature_name: &str,
    feature_value: &str,
    optional_deps: &HashSet<String>,
    deps_needing_implicit: &mut HashSet<String>,
    deps_with_explicit: &mut HashSet<String>,
    _dir_name: &str,
) {
    for dep in optional_deps {
        let dep_prefix = format!("\"dep:{}", dep);
        if feature_value.contains(&dep_prefix) {
            deps_with_explicit.insert(dep.clone());
        }

        // Check for bare references
        for entry in feature_value.split(',') {
            let entry = entry.trim();
            let bare_pattern = format!("\"{}\"", dep);
            if entry.contains(&bare_pattern) && !entry.contains("dep:") && !entry.contains('/') && feature_name != *dep {
                deps_needing_implicit.insert(dep.clone());
                break;
            }
        }
    }
}

/// Apply all Cargo.toml fixes in a single pass
fn apply_cargo_toml_fixes(
    content: &str,
    output_dir: &Path,
    sliced_deps_cache: &HashSet<String>,
    optional_deps: &HashSet<String>,
    deps_needing_implicit_features: &HashSet<String>,
    existing_features: &HashSet<String>,
    uses_rand_core_getrandom: bool,
) -> Result<String, String> {
    let mut new_content = String::new();
    let mut in_features_section = false;
    let mut added_implicit_features = false;
    let mut skip_until_next_section = false;
    let mut current_dep_optional = false;
    let mut current_dep_features: Vec<String> = Vec::new();
    let mut collecting_dep_info = false;
    let mut in_features_array = false;
    let mut in_getrandom_dep = false;
    let mut in_clap_dep = false;
    let mut current_dep_is_sliced_clap = false;
    let mut clap_features_buffer: Vec<String> = Vec::new();
    let mut collecting_clap_features = false;
    let mut skip_bench_section = false;
    let mut in_package_section = false;
    let mut seen_edition = false;
    let mut regular_deps: HashSet<String> = HashSet::new();

    // Pre-scan: collect dep names from [dependencies.X] (non-target-specific)
    for line in content.lines() {
        let t = line.trim();
        if t.starts_with("[dependencies.") && !t.starts_with("[dependencies.\"") {
            let name = t.trim_start_matches("[dependencies.")
                .trim_end_matches(']')
                .to_string();
            regular_deps.insert(name);
        }
    }

    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];
        let trimmed = line.trim();

        // === Skip [[bench]], [[test]], and dev-dependencies sections ===
        // We don't copy benches/tests directories and dev-dependencies cause version conflicts
        let is_skip_section = trimmed == "[[bench]]"
            || trimmed == "[[test]]"
            || trimmed == "[dev-dependencies]"
            || trimmed.starts_with("[dev-dependencies.")
            || trimmed.starts_with("[profile.")
            || (trimmed.starts_with("[target.") && trimmed.contains(".dev-dependencies"));
        if is_skip_section {
            skip_bench_section = true;
            i += 1;
            continue;
        }
        if skip_bench_section {
            if trimmed.starts_with('[') && !is_skip_section {
                skip_bench_section = false;
            } else {
                i += 1;
                continue;
            }
        }

        // === Track [package] section for resolver/edition fixes ===
        if trimmed == "[package]" {
            in_package_section = true;
            seen_edition = false;
        } else if in_package_section && trimmed.starts_with('[') {
            if !seen_edition {
                new_content.push_str("edition = \"2015\"\n");
            }
            in_package_section = false;
        }
        if in_package_section && trimmed.starts_with("edition") {
            seen_edition = true;
        }
        if in_package_section && trimmed.starts_with("resolver") {
            i += 1;
            continue;
        }

        // === Handle features section (for implicit features) ===
        if trimmed.starts_with("[features]") {
            // Write any pending dependency info before starting features section
            if collecting_dep_info {
                write_sliced_dep_path(&mut new_content, output_dir, sliced_deps_cache,
                    current_dep_optional, &current_dep_features)?;
                collecting_dep_info = false;
                skip_until_next_section = false;
                current_dep_optional = false;
                current_dep_features.clear();
            }
            in_features_section = true;
            new_content.push_str(line);
            new_content.push('\n');
            i += 1;
            continue;
        }

        // Skip features that reference missing or dev-dependencies
        if in_features_section && !trimmed.starts_with('[') && trimmed.contains('/') {
            // Check if feature references a test-related crate (likely dev-dependency)
            if trimmed.contains("-test-suite") || trimmed.contains("-test/") {
                i += 1;
                continue;
            }
            // Skip features that reference common non-sliced optional dependencies
            // These are typically used for nightly/unstable features that we don't need
            if trimmed.contains("bumpalo/") || trimmed.contains("rkyv/") {
                i += 1;
                continue;
            }
        }

        // Fix features for optional deps that are missing dep: prefix
        // This is required for Rust 2021 edition where optional deps need explicit dep: prefix
        // Handles both empty features (`ab_glyph = []`) and non-empty ones (`ab_glyph = ["once_cell"]`)
        if in_features_section && !trimmed.starts_with('[') && !trimmed.is_empty() && !trimmed.starts_with('#') {
            if let Some(eq_pos) = trimmed.find('=') {
                let feature_name = trimmed[..eq_pos].trim();
                let feature_value = trimmed[eq_pos + 1..].trim();

                // Check if feature matches optional dep name
                if optional_deps.contains(feature_name) {
                    let dep_ref = format!("dep:{}", feature_name);

                    // Check if dep: reference is missing
                    if !feature_value.contains(&dep_ref) {
                        if feature_value == "[]" {
                            // Empty feature: just add dep:
                            new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                        } else if feature_value.starts_with('[') && feature_value.ends_with(']') {
                            // Non-empty feature: prepend dep: to existing items
                            // e.g., `ab_glyph = ["once_cell"]` -> `ab_glyph = ["dep:ab_glyph", "once_cell"]`
                            let inner = &feature_value[1..feature_value.len()-1].trim();
                            if inner.is_empty() {
                                new_content.push_str(&format!("{} = [\"dep:{}\"]\n", feature_name, feature_name));
                            } else {
                                new_content.push_str(&format!("{} = [\"dep:{}\", {}]\n", feature_name, feature_name, inner));
                            }
                        } else {
                            // Multi-line array start - handle by adding dep: at the start
                            // e.g., `ab_glyph = [` followed by items on next lines
                            new_content.push_str(&format!("{} = [\n    \"dep:{}\",\n", feature_name, feature_name));
                        }
                        i += 1;
                        continue;
                    }
                }
            }
        }

        // Add implicit features before leaving features section
        if in_features_section && trimmed.starts_with('[') && !trimmed.starts_with("[features") {
            if !added_implicit_features {
                for dep in deps_needing_implicit_features {
                    if !existing_features.contains(dep) {
                        new_content.push_str(&format!("{} = [\"dep:{}\"]\n", dep, dep));
                    }
                }
                added_implicit_features = true;
            }
            in_features_section = false;
        }

        // === Handle dependency sections (for sliced deps) ===
        let is_dep_section = trimmed.starts_with("[dependencies.")
            || trimmed.starts_with("[dev-dependencies.")
            || trimmed.starts_with("[build-dependencies.")
            || (trimmed.starts_with("[target.") && (
                trimmed.contains(".dependencies.") ||
                trimmed.contains(".dev-dependencies.") ||
                trimmed.contains(".build-dependencies.")
            ));

        if is_dep_section {
            // Write any pending dependency info
            if collecting_dep_info {
                write_sliced_dep_path(&mut new_content, output_dir, sliced_deps_cache,
                    current_dep_optional, &current_dep_features)?;
            }

            // Reset state
            skip_until_next_section = false;
            current_dep_optional = false;
            current_dep_features.clear();
            collecting_dep_info = false;
            in_features_array = false;

            // Extract dependency name
            let (section_prefix, dep_name) = extract_dep_section_info(trimmed);

            // Skip target-specific deps that duplicate a regular [dependencies.X] entry.
            // Cargo rejects the same dep with different source paths per target.
            let is_target_dep = trimmed.starts_with("[target.") && trimmed.contains(".dependencies.");
            let is_target_non_dev = is_target_dep
                && !trimmed.contains(".dev-dependencies.")
                && !trimmed.contains(".build-dependencies.");
            if is_target_non_dev && regular_deps.contains(&dep_name) {
                skip_bench_section = true;
                i += 1;
                continue;
            }

            // Check for compatibility fixes (getrandom, clap, rand_core, trybuild)
            in_getrandom_dep = dep_name == "getrandom";
            in_clap_dep = trimmed.starts_with("[dev-dependencies.clap]");
            let is_rand_core_dep = dep_name == "rand_core";

            if in_clap_dep {
                current_dep_is_sliced_clap = false;
                collecting_clap_features = false;
                clap_features_buffer.clear();
            }

            // Check if sliced version exists
            let has_sliced = sliced_deps_cache.contains(&dep_name) ||
                             sliced_deps_cache.contains(&format!("same:{}", dep_name));



            // Special case: rand_core with getrandom feature needs registry v0.6
            // (sliced rand_core v0.9 renamed 'getrandom' to 'os_rng')
            if is_rand_core_dep && uses_rand_core_getrandom && has_sliced {
                // Write registry version instead of sliced path
                new_content.push_str(&format!("{}{}]\n", section_prefix, dep_name));
                new_content.push_str("version = \"0.6\"\n");
                // Look ahead for optional = true and skip the rest
                i += 1;
                while i < lines.len() {
                    let peek_line = lines[i];
                    let peek_trimmed = peek_line.trim();
                    if peek_trimmed.starts_with('[') {
                        break;
                    }
                    if peek_trimmed == "optional = true" {
                        new_content.push_str("optional = true\n");
                    }
                    i += 1;
                }
                let crate_name = output_dir.file_name().unwrap().to_str().unwrap();
                debug_log!("    📦 {} → rand_core (registry v0.6 for 'getrandom' feature)", crate_name);
                continue;
            }

            if has_sliced {
                collecting_dep_info = true;
                new_content.push_str(&format!("{}{}]\n", section_prefix, dep_name));
                skip_until_next_section = true;
                i += 1;
                continue;
            }
        } else if trimmed.starts_with('[') {
            // End of dependency section
            if collecting_dep_info {
                write_sliced_dep_path(&mut new_content, output_dir, sliced_deps_cache,
                    current_dep_optional, &current_dep_features)?;
                collecting_dep_info = false;
            }

            // Handle clap EOF case
            if in_clap_dep && current_dep_is_sliced_clap && !clap_features_buffer.is_empty() {
                handle_clap_dep(&mut new_content, &clap_features_buffer, output_dir);
                clap_features_buffer.clear();
            }

            skip_until_next_section = false;
            current_dep_optional = false;
            current_dep_features.clear();
            in_features_array = false;
            in_getrandom_dep = false;
            in_clap_dep = false;
            collecting_clap_features = false;
        }

        // === Collect dependency info for sliced deps ===
        if skip_until_next_section {
            if trimmed == "optional = true" {
                current_dep_optional = true;
            } else if trimmed.starts_with("features") {
                current_dep_features.push(line.to_string());
                if line.contains('[') && !line.contains(']') {
                    in_features_array = true;
                }
            } else if in_features_array {
                current_dep_features.push(line.to_string());
                if trimmed.ends_with(']') {
                    in_features_array = false;
                }
            } else if trimmed.starts_with("default-features") {
                current_dep_features.push(line.to_string());
            }
            i += 1;
            continue;
        }

        // === Compatibility fixes ===

        // Fix getrandom js -> wasm_js
        if in_getrandom_dep && trimmed.starts_with("features") && trimmed.contains("[\"js\"]") {
            let fixed_line = line.replace("[\"js\"]", "[\"wasm_js\"]");
            new_content.push_str(&fixed_line);
            new_content.push('\n');
            i += 1;
            continue;
        }

        // Handle clap sliced path detection
        if in_clap_dep && trimmed.starts_with("path") && trimmed.contains("clap-sliced") {
            current_dep_is_sliced_clap = true;
            collecting_clap_features = true;
            i += 1;
            continue;
        }

        // Buffer clap features
        if in_clap_dep && collecting_clap_features {
            clap_features_buffer.push(line.to_string());
            i += 1;
            continue;
        }

        // Fix any exact version constraint (e.g., version = "=0.2.163" -> version = "^0.2")
        // These cause dependency resolution conflicts
        // Check for both "=\"" and "= \"=" patterns (with or without space)
        if trimmed.starts_with("version") && (trimmed.contains("\"=") || trimmed.contains("=\"")) {
            if let Some(fixed) = fix_exact_version(trimmed) {
                new_content.push_str(&fixed);
                new_content.push('\n');
                i += 1;
                continue;
            }
        }

        // Default: copy line as-is
        new_content.push_str(line);
        new_content.push('\n');
        i += 1;
    }

    // Handle EOF cases
    if in_package_section && !seen_edition {
        new_content.push_str("edition = \"2015\"\n");
    }

    if collecting_dep_info {
        write_sliced_dep_path(&mut new_content, output_dir, sliced_deps_cache,
            current_dep_optional, &current_dep_features)?;
    }

    if !added_implicit_features && in_features_section {
        for dep in deps_needing_implicit_features {
            if !existing_features.contains(dep) {
                new_content.push_str(&format!("{} = [\"dep:{}\"]\n", dep, dep));
            }
        }
    }

    if in_clap_dep && current_dep_is_sliced_clap && !clap_features_buffer.is_empty() {
        handle_clap_dep(&mut new_content, &clap_features_buffer, output_dir);
    }

    Ok(new_content)
}

/// Extract section prefix and dependency name from a dependency section header
fn extract_dep_section_info(trimmed: &str) -> (String, String) {
    if trimmed.starts_with("[target.") {
        if let Some(pos) = trimmed.find(".dev-dependencies.") {
            let prefix = &trimmed[..pos + ".dev-dependencies.".len()];
            let name = trimmed[pos + ".dev-dependencies.".len()..].trim_end_matches(']');
            return (prefix.to_string(), name.to_string());
        } else if let Some(pos) = trimmed.find(".build-dependencies.") {
            let prefix = &trimmed[..pos + ".build-dependencies.".len()];
            let name = trimmed[pos + ".build-dependencies.".len()..].trim_end_matches(']');
            return (prefix.to_string(), name.to_string());
        } else if let Some(pos) = trimmed.find(".dependencies.") {
            let prefix = &trimmed[..pos + ".dependencies.".len()];
            let name = trimmed[pos + ".dependencies.".len()..].trim_end_matches(']');
            return (prefix.to_string(), name.to_string());
        }
    }

    let section_prefix = if trimmed.starts_with("[dev-dependencies.") {
        "[dev-dependencies."
    } else if trimmed.starts_with("[build-dependencies.") {
        "[build-dependencies."
    } else {
        "[dependencies."
    };
    let dep_name = trimmed
        .trim_start_matches(section_prefix)
        .trim_end_matches(']')
        .to_string();
    (section_prefix.to_string(), dep_name)
}

/// Write path dependency info for a sliced dependency
fn write_sliced_dep_path(
    content: &mut String,
    output_dir: &Path,
    sliced_deps_cache: &HashSet<String>,
    optional: bool,
    features: &[String],
) -> Result<(), String> {
    // Get global multi-version info
    let mv_info = get_multi_version_info().unwrap_or_default();

    // Try to get consumer version from:
    // 1. Versioned directory name (e.g., bitflags-1.3.2-sliced)
    // 2. Cargo.toml in the output directory
    let dir_name = output_dir.file_name().unwrap().to_str().unwrap();
    let consumer_version = extract_version_from_dir(dir_name)
        .or_else(|| get_version_from_cargo_toml(output_dir));

    write_sliced_dep_path_versioned(
        content,
        output_dir,
        sliced_deps_cache,
        optional,
        features,
        consumer_version.as_deref(),
        &mv_info.version_map,
        &mv_info.multi_version_crates,
    )
}

/// Extract version from a versioned directory name.
/// `bitflags-1.3.2-sliced` → Some("1.3.2")
/// `memchr-sliced` → None
fn extract_version_from_dir(dir_name: &str) -> Option<String> {
    let name = dir_name.strip_suffix("-sliced").unwrap_or(dir_name);

    // Check if it ends with a version pattern (e.g., -1.3.2)
    if let Some(last_dash) = name.rfind('-') {
        let potential_version = &name[last_dash + 1..];
        if potential_version.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            return Some(potential_version.to_string());
        }
    }

    None
}

/// Get version from Cargo.toml in the output directory
fn get_version_from_cargo_toml(output_dir: &Path) -> Option<String> {
    let cargo_toml = output_dir.join("Cargo.toml");
    let content = fs::read_to_string(cargo_toml).ok()?;

    // Look for version = "x.y.z" in [package] section
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
        if in_package && trimmed.starts_with("version") {
            if let Some(v) = trimmed.split('=').nth(1) {
                let version = v.trim().trim_matches('"').trim_matches('\'');
                return Some(version.to_string());
            }
        }
    }
    None
}

/// Write path dependency info for a sliced dependency with multi-version support.
///
/// For multi-version crates, routes to versioned directories like `bitflags-1.3.2-sliced/`.
fn write_sliced_dep_path_versioned(
    content: &mut String,
    output_dir: &Path,
    sliced_deps_cache: &HashSet<String>,
    optional: bool,
    features: &[String],
    consumer_version: Option<&str>,
    version_map: &VersionMap,
    multi_version_crates: &MultiVersionCrates,
) -> Result<(), String> {
    // Get the last section header to determine dep name
    let last_section = content.lines().rev()
        .find(|l| l.starts_with("[dependencies.")
            || l.starts_with("[dev-dependencies.")
            || l.starts_with("[build-dependencies.")
            || (l.starts_with("[target.") && (
                l.contains(".dependencies.") ||
                l.contains(".dev-dependencies.") ||
                l.contains(".build-dependencies.")
            )))
        .unwrap_or("");

    let (_, dep_name) = extract_dep_section_info(last_section);


    // Get consumer crate info (strip -sliced and version suffix if present)
    let crate_name = output_dir.file_name().unwrap().to_str().unwrap();
    let consumer_name = extract_crate_name_from_dir(crate_name);

    // Special case: clap with derive feature should use registry v4
    let has_derive = features.iter().any(|f| f.contains("\"derive\""));
    if dep_name == "clap" && has_derive {
        content.push_str("version = \"4\"\n");
        for feat_line in features {
            content.push_str(feat_line);
            content.push('\n');
        }
        println!("    📦 {} → clap (registry v4 for 'derive' feature)", crate_name);
        return Ok(());
    }

    // Special case: rand_core with getrandom feature should use registry v0.6
    // (sliced rand_core v0.9 renamed 'getrandom' to 'os_rng')
    let has_getrandom = features.iter().any(|f| f.contains("\"getrandom\""));
    if dep_name == "rand_core" && has_getrandom {
        content.push_str("version = \"0.6\"\n");
        for feat_line in features {
            content.push_str(feat_line);
            content.push('\n');
        }
        debug_log!("    📦 {} → rand_core (registry v0.6 for 'getrandom' feature)", crate_name);
        return Ok(());
    }

    // Multi-version routing: if this dependency is a multi-version crate,
    // check if we have a sliced version for it. We only slice the LATEST version,
    // so consumers that need older versions will use registry fallback.
    if let Some(available_versions) = multi_version_crates.get(&dep_name) {
        // The latest version is the one we slice (last in sorted list)
        let latest_version = available_versions.last().cloned();

        // Check which version this consumer needs
        let needed_version = if let Some(cv) = consumer_version {
            let key = (consumer_name.to_string(), cv.to_string(), dep_name.clone());
            version_map.get(&key).cloned()
        } else {
            None
        };

        // If consumer needs the latest version (or we don't know), try sliced path
        // Otherwise fall through to registry (the sliced version doesn't match)
        if let Some(ref latest) = latest_version {
            let versioned_dir_name = format!("{}-{}-sliced", dep_name, latest);
            let versioned_base = format!("{}-{}", dep_name, latest);
            let sliced_exists = sliced_deps_cache.contains(&versioned_base);

            // Route to sliced version only if:
            // 1. The sliced version exists, AND
            // 2. Either the consumer needs the latest version, or we don't know what it needs
            let should_use_sliced = sliced_exists &&
                (needed_version.is_none() || needed_version.as_ref() == Some(latest));

            if should_use_sliced {
                let versioned_path = format!("../{}", versioned_dir_name);
                content.push_str(&format!("path = \"{}\"\n", versioned_path));
                if optional {
                    content.push_str("optional = true\n");
                }
                for feat_line in features {
                    content.push_str(feat_line);
                    content.push('\n');
                }
                return Ok(());
            } else if needed_version.is_some() && needed_version != latest_version {
                // Consumer needs an OLDER version - use registry (multi-version fallback)
                let version = needed_version.unwrap();
                content.push_str(&format!("version = \"{}\"\n", version));
                if optional {
                    content.push_str("optional = true\n");
                }
                for feat_line in features {
                    content.push_str(feat_line);
                    content.push('\n');
                }
                debug_log!("    📦 {} → {} v{} (registry - multi-version fallback)",
                    crate_name, dep_name, version);
                return Ok(());
            }
        }
        // Fall through to normal sliced/registry logic below
    }

    // Special case: platform-specific crates that aren't sliced should use registry versions
    // These include macOS (objc2, dispatch2, block2, fsevent-sys), BSD (kqueue, kqueue-sys), etc.
    let is_platform_specific = PLATFORM_SPECIFIC_CRATES.contains(&dep_name.as_str());
    let sliced_exists = sliced_deps_cache.contains(&dep_name) ||
                        sliced_deps_cache.contains(&format!("same:{}", dep_name));
    if is_platform_specific && !sliced_exists {
        // Use registry version for platform-specific crates that aren't sliced
        let version = match dep_name.as_str() {
            "dispatch2" => "0.3",
            "block2" => "0.5",
            "objc2" | "objc2-exception-helper" => "0.5",
            "fsevent-sys" => "4",
            "kqueue" => "1.0",
            "kqueue-sys" => "1.0",
            "mio" => "0.8",
            _ => "1", // fallback to any recent version
        };
        content.push_str(&format!("version = \"{}\"\n", version));
        if optional {
            content.push_str("optional = true\n");
        }
        for feat_line in features {
            content.push_str(feat_line);
            content.push('\n');
        }
        return Ok(());
    }

    // Determine relative path
    let relative_path = if sliced_deps_cache.contains(&format!("same:{}", dep_name)) {
        format!("{}-sliced", dep_name)
    } else {
        format!("../{}-sliced", dep_name)
    };

    content.push_str(&format!("path = \"{}\"\n", relative_path));
    if optional {
        content.push_str("optional = true\n");
    }
    for feat_line in features {
        content.push_str(feat_line);
        content.push('\n');
    }

    Ok(())
}

/// Handle clap dependency with derive feature - switch to registry v4
fn handle_clap_dep(content: &mut String, features_buffer: &[String], output_dir: &Path) {
    let has_derive = features_buffer.iter().any(|f| f.contains("\"derive\""));
    if has_derive {
        content.push_str("version = \"4\"\n");
        for feat_line in features_buffer {
            content.push_str(feat_line);
            content.push('\n');
        }
        let crate_name = output_dir.file_name().unwrap().to_str().unwrap();
        println!("    📦 {} → clap (registry v4 for 'derive' feature)", crate_name);
    } else {
        content.push_str("path = \"../clap-sliced\"\n");
        for feat_line in features_buffer {
            content.push_str(feat_line);
            content.push('\n');
        }
    }
}

/// Fix exact version constraint (e.g., version = "=0.2.163" -> version = "^0.2")
fn fix_exact_version(trimmed: &str) -> Option<String> {
    if let Some(start) = trimmed.find('"') {
        let version_str = &trimmed[start+1..];
        if let Some(end) = version_str.find('"') {
            let version = &version_str[..end];
            let clean_version = version.trim_start_matches('=');
            let parts: Vec<&str> = clean_version.split('.').collect();
            if parts.len() >= 2 {
                return Some(format!("version = \"^{}.{}\"", parts[0], parts[1]));
            }
        }
    }
    None
}

/// Directories to skip when copying (not needed for compilation)
/// Note: We keep "examples" because some crates reference examples in Cargo.toml
const SKIP_DIRS: &[&str] = &[
    "benches", "tests", "test", "target", ".git", ".github",
    "docs", "doc", "fuzz", "ci", "scripts", ".cargo", "fixtures",
];

/// Copy directory with optimizations:
/// 1. Skip unnecessary directories (tests, examples, benches, etc.)
/// 2. Collect all files first, then copy in parallel
fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<(), String> {
    // Phase 1: Collect all files and directories to copy
    let mut files_to_copy: Vec<(PathBuf, PathBuf)> = Vec::new();
    let mut dirs_to_create: Vec<PathBuf> = Vec::new();

    collect_files_recursive(src, dst, &mut files_to_copy, &mut dirs_to_create)?;

    // Phase 2: Create all directories (must be sequential)
    for dir in &dirs_to_create {
        fs::create_dir_all(dir)
            .map_err(|e| format!("Failed to create dir {:?}: {}", dir, e))?;
    }

    // Phase 3: Copy all files in parallel
    let errors: Vec<String> = files_to_copy
        .par_iter()
        .filter_map(|(src_path, dst_path)| {
            match fs::copy(src_path, dst_path) {
                Ok(_) => None,
                Err(e) => Some(format!("Failed to copy {:?}: {}", src_path, e)),
            }
        })
        .collect();

    if !errors.is_empty() {
        return Err(errors.join("; "));
    }

    Ok(())
}

/// Recursively collect files to copy, skipping unnecessary directories
fn collect_files_recursive(
    src: &Path,
    dst: &Path,
    files: &mut Vec<(PathBuf, PathBuf)>,
    dirs: &mut Vec<PathBuf>,
) -> Result<(), String> {
    // Add destination directory to create list
    dirs.push(dst.to_path_buf());

    // Check if we're in a src/ directory (keep data files there)
    let is_src_dir = src.ends_with("src") ||
        src.to_string_lossy().contains("/src/") ||
        src.to_string_lossy().contains("\\src\\");

    let entries = fs::read_dir(src)
        .map_err(|e| format!("Failed to read dir {:?}: {}", src, e))?;

    for entry in entries {
        let entry = entry.map_err(|e| format!("Failed to read entry: {}", e))?;
        let path = entry.path();
        let file_name = entry.file_name();
        let file_name_str = file_name.to_string_lossy();
        let dst_path = dst.join(&file_name);

        if path.is_dir() {
            // Skip unnecessary directories
            if should_skip_dir(&file_name_str) {
                continue;
            }
            collect_files_recursive(&path, &dst_path, files, dirs)?;
        } else {
            // Skip unnecessary files (but keep data files in src/)
            if should_skip_file(&file_name_str, is_src_dir) {
                continue;
            }
            files.push((path, dst_path));
        }
    }

    Ok(())
}

/// Check if a directory should be skipped
#[inline]
fn should_skip_dir(name: &str) -> bool {
    SKIP_DIRS.contains(&name)
}

/// Check if a file should be skipped
/// `in_src_dir`: if true, keep data files like .txt (they might be used by include_str!)
#[inline]
fn should_skip_file(name: &str, in_src_dir: bool) -> bool {
    // Hidden files are always skipped
    if name.starts_with('.') {
        return true;
    }

    // In src/ directories, keep data files (they might be include_str! targets)
    if in_src_dir {
        // Keep .txt, .json, .toml (data), .csv, etc in src/
        // Only skip truly unnecessary files like .md, .yml
        return name.ends_with(".md")  // Docs
            || name.ends_with(".yml") || name.ends_with(".yaml");  // CI configs
    }

    // Outside src/, skip common non-essential files
    name.ends_with(".md")  // Markdown docs
        || name.ends_with(".txt")  // Text files
        || name.ends_with(".yml") || name.ends_with(".yaml")  // CI configs
        || name == "LICENSE" || name == "LICENSE-MIT" || name == "LICENSE-APACHE"
        || name == "CHANGELOG" || name.starts_with("CHANGELOG.")
        || name == "README" || name.starts_with("README.")
        || name == "CONTRIBUTING" || name.starts_with("CONTRIBUTING.")
        || name == "rustfmt.toml" || name == ".rustfmt.toml"
        || name == "clippy.toml" || name == ".clippy.toml"
        || name == "deny.toml"
        || name == "Makefile"
        || name.ends_with(".lock") && name != "Cargo.lock"  // Keep Cargo.lock
}

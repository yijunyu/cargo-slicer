//! External import injection.
//!
//! Functions for detecting and injecting external imports.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

/// Get type-to-crate mappings for common external types
pub fn get_type_to_crate_mappings() -> Vec<(&'static str, &'static str)> {
    vec![
        ("RngCore", "rand_core"),
        ("SeedableRng", "rand_core"),
        ("CryptoRng", "rand_core"),
        ("RawMutex", "lock_api"),
        ("RawRwLock", "lock_api"),
        ("SmallVec", "smallvec"),
        ("FromPrimitive", "num-traits"),
        ("ToPrimitive", "num-traits"),
        // parking_lot types
        ("parking_lot::", "parking_lot"),
    ]
}

/// Get external import paths for common types
pub fn get_external_import_paths() -> HashMap<&'static str, &'static str> {
    let mut map = HashMap::new();
    map.insert("RngCore", "rand_core::RngCore");
    map.insert("SeedableRng", "rand_core::SeedableRng");
    map.insert("CryptoRng", "rand_core::CryptoRng");
    map.insert("RawMutex", "lock_api::RawMutex");
    map.insert("SmallVec", "smallvec::SmallVec");
    map
}

/// Inject external imports into content if needed
pub fn inject_external_imports(content: &str, available_deps: &HashSet<String>) -> String {
    let mut result = content.to_string();
    let import_paths = get_external_import_paths();

    for (type_name, import_path) in &import_paths {
        // Check if type is used but not imported
        if content.contains(type_name) && !content.contains(&format!("use {}", import_path)) {
            // Extract crate name from path
            if let Some(crate_name) = import_path.split("::").next() {
                if available_deps.contains(crate_name) {
                    // Add import at the top
                    result = format!("use {};\n{}", import_path, result);
                }
            }
        }
    }

    result
}

/// Inject imports in all files in a directory
pub fn inject_imports_in_directory(dir: &Path, available_deps: &HashSet<String>) -> std::io::Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().map(|e| e == "rs").unwrap_or(false) {
            let content = fs::read_to_string(&path)?;
            let new_content = inject_external_imports(&content, available_deps);
            if new_content != content {
                fs::write(&path, new_content)?;
            }
        } else if path.is_dir() {
            inject_imports_in_directory(&path, available_deps)?;
        }
    }

    Ok(())
}

/// Scan code for external dependencies
pub fn scan_code_for_external_deps(output_dir: &Path) -> HashSet<String> {
    let mut deps = HashSet::new();
    let mappings = get_type_to_crate_mappings();

    if let Ok(entries) = fs::read_dir(output_dir.join("src")) {
        for entry in entries.flatten() {
            if let Ok(content) = fs::read_to_string(entry.path()) {
                for (type_name, crate_name) in &mappings {
                    if content.contains(type_name) {
                        deps.insert(crate_name.to_string());
                    }
                }
            }
        }
    }

    deps
}

/// Detect required features based on code usage
pub fn detect_required_features(output_dir: &Path, _crate_name: &str) -> Vec<String> {
    let mut features = Vec::new();

    // Scan source files for feature indicators
    if let Ok(content) = fs::read_to_string(output_dir.join("src").join("lib.rs")) {
        // Simple heuristics for common features
        if content.contains("std::") {
            features.push("std".to_string());
        }
        if content.contains("alloc::") {
            features.push("alloc".to_string());
        }
    }

    features
}

/// Add features to a dependency value
pub fn add_features_to_dep(dep_value: &str, new_features: &[String]) -> String {
    if dep_value.starts_with('{') {
        // Already has features, append
        if dep_value.contains("features") {
            // Parse and add
            dep_value.to_string() // Simplified - full impl would merge
        } else {
            // Add features key
            let features = new_features.iter()
                .map(|f| format!("\"{}\"", f))
                .collect::<Vec<_>>()
                .join(", ");
            dep_value.replace("}", &format!(", features = [{}] }}", features))
        }
    } else {
        // Simple version string, convert to table
        let features = new_features.iter()
            .map(|f| format!("\"{}\"", f))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{{ version = {}, features = [{}] }}", dep_value, features)
    }
}

/// Remove optional flag from a dependency
pub fn remove_optional_from_dep(dep_value: &str) -> String {
    dep_value
        .replace(", optional = true", "")
        .replace("optional = true, ", "")
        .replace("optional = true", "")
}

/// Filter dependencies to only include needed ones
pub fn filter_dependencies(
    deps: &HashMap<String, String>,
    needed_crates: &HashSet<String>,
) -> HashMap<String, String> {
    deps.iter()
        .filter(|(name, _)| {
            let normalized = name.replace('-', "_");
            needed_crates.contains(&normalized) || needed_crates.contains(*name)
        })
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect()
}

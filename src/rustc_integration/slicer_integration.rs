//! Integration between rustc driver and cargo-slicer workflow
//!
//! This module provides functions to run the rustc driver on crates
//! and convert the collected UsageData into formats the slicer can use.

#![cfg(feature = "rustc-driver")]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::collections::{HashMap, HashSet};
use super::UsageData;

/// Run rustc driver on a crate to collect usage data
///
/// Returns UsageData with:
/// - defined_items: All items in the crate with their metadata
/// - item_dependencies: Dependency graph (what calls what)
/// - referenced_items: Reverse index (what is called by whom)
pub fn analyze_crate_with_rustc_driver(
    crate_path: &Path,
    crate_name: &str,
) -> Result<UsageData, String> {
    // Find the rustc driver binary
    let driver_path = find_rustc_driver_binary()?;

    // Create temporary output file
    let output_path = std::env::temp_dir().join(format!("{}-rustc-data.json", crate_name));

    // Set up environment
    let rustc_sysroot = get_rustc_sysroot()?;
    let ld_library_path = format!("{}/lib", rustc_sysroot);

    // Find the main library file
    let lib_file = find_lib_file(crate_path)?;

    // Run the rustc driver
    let output = Command::new(&driver_path)
        .env("LD_LIBRARY_PATH", &ld_library_path)
        .env("CARGO_SLICER_COLLECT_DATA", "1")
        .env("CARGO_SLICER_DATA_OUT", &output_path)
        .arg("--edition=2021")
        .arg("--crate-type=lib")
        .arg(&lib_file)
        .output()
        .map_err(|e| format!("Failed to run rustc driver: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Only error if it's not just warnings
        if !stderr.contains("warning:") || stderr.contains("error:") {
            return Err(format!("Rustc driver failed: {}", stderr));
        }
    }

    // Read the collected data
    UsageData::read_from_file(&output_path)
        .map_err(|e| format!("Failed to read rustc driver output: {}", e))
}

/// Find the cargo-slicer-rustc binary
fn find_rustc_driver_binary() -> Result<PathBuf, String> {
    // Check in target/debug first
    let debug_path = PathBuf::from("target/debug/cargo-slicer-rustc");
    if debug_path.exists() {
        return Ok(debug_path);
    }

    // Check in target/release
    let release_path = PathBuf::from("target/release/cargo-slicer-rustc");
    if release_path.exists() {
        return Ok(release_path);
    }

    // Check if it's in PATH
    if let Ok(output) = Command::new("which").arg("cargo-slicer-rustc").output() {
        if output.status.success() {
            let path_str = String::from_utf8_lossy(&output.stdout);
            return Ok(PathBuf::from(path_str.trim()));
        }
    }

    Err("cargo-slicer-rustc binary not found. Build with: cargo build --features rustc-driver --bin cargo-slicer-rustc".to_string())
}

/// Get rustc sysroot path
fn get_rustc_sysroot() -> Result<String, String> {
    let output = Command::new("rustc")
        .arg("--print")
        .arg("sysroot")
        .output()
        .map_err(|e| format!("Failed to run rustc: {}", e))?;

    if !output.status.success() {
        return Err("Failed to get rustc sysroot".to_string());
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Find the main library file (lib.rs or main.rs)
fn find_lib_file(crate_path: &Path) -> Result<PathBuf, String> {
    let src_dir = crate_path.join("src");

    // Try lib.rs first
    let lib_rs = src_dir.join("lib.rs");
    if lib_rs.exists() {
        return Ok(lib_rs);
    }

    // Try main.rs
    let main_rs = src_dir.join("main.rs");
    if main_rs.exists() {
        return Ok(main_rs);
    }

    Err(format!("No lib.rs or main.rs found in {}", src_dir.display()))
}

/// Convert UsageData to the slicer's dependency graph format
///
/// Maps item_dependencies to a simpler format for the BFS marker
pub fn usage_data_to_dep_graph(
    usage_data: &UsageData,
) -> HashMap<String, HashSet<String>> {
    // The item_dependencies field already has the right format:
    // {"caller" -> ["callee1", "callee2", ...]}
    usage_data.item_dependencies.clone()
}

/// Get all defined items from UsageData
pub fn get_defined_items(usage_data: &UsageData) -> HashSet<String> {
    usage_data.defined_items.keys().cloned().collect()
}

/// Get public items only (for entry point detection)
pub fn get_public_items(usage_data: &UsageData) -> HashSet<String> {
    usage_data
        .defined_items
        .iter()
        .filter(|(_, info)| matches!(info.visibility, super::Visibility::Public))
        .map(|(path, _)| path.clone())
        .collect()
}

/// Convert UsageData to CrateIndex format for compatibility with existing slicer
///
/// This allows the rustc driver to be used as a drop-in replacement for parse_crate()
pub fn usage_data_to_crate_index(usage_data: &UsageData) -> crate::types::CrateIndex {
    use crate::types::{CrateIndex, ParsedItem, ParsedItemKind, ItemVisibility};
    use std::collections::HashSet;

    let mut index = CrateIndex::new();

    for (name, item_info) in &usage_data.defined_items {
        // Convert ItemKind to ParsedItemKind
        let kind = match item_info.kind {
            super::ItemKind::Function => ParsedItemKind::Function,
            super::ItemKind::Type => ParsedItemKind::Struct, // Could be struct/enum/type alias
            super::ItemKind::Trait => ParsedItemKind::Trait,
            super::ItemKind::Impl => ParsedItemKind::Impl,
            super::ItemKind::Const => ParsedItemKind::Const,
            super::ItemKind::Static => ParsedItemKind::Static,
            super::ItemKind::Macro => ParsedItemKind::Macro,
            super::ItemKind::Mod => ParsedItemKind::Mod,
        };

        // Convert visibility
        let (is_pub, visibility) = match item_info.visibility {
            super::Visibility::Public => (true, ItemVisibility::Public),
            super::Visibility::PublicCrate => (true, ItemVisibility::Crate),
            super::Visibility::Private => (false, ItemVisibility::Private),
        };

        // Parse location (format: "file:line:col")
        let location_parts: Vec<&str> = item_info.location.split(':').collect();
        let file_path = location_parts.get(0).map(|s| s.to_string()).unwrap_or_default();
        let line = location_parts.get(1).and_then(|s| s.parse().ok());

        // Get dependencies from item_dependencies map
        let dependencies = usage_data.item_dependencies
            .get(name)
            .cloned()
            .unwrap_or_default();

        let parsed_item = ParsedItem {
            name: name.clone(),
            path: name.clone(),
            kind,
            source: String::new(), // Not available from rustc driver
            dependencies,
            typed_dependencies: Vec::new(), // Not collected by rustc driver yet
            external_crates: HashSet::new(), // Not tracked separately yet
            module_path: name.clone(), // Simplified - use full path
            is_pub,
            visibility,
            file: std::path::PathBuf::from(file_path),
            line,
            cfg_attr: None, // Not collected yet
            generics: Vec::new(), // Not collected yet
        };

        index.add_item(parsed_item);
    }

    index
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_rustc_driver_binary() {
        // This will fail in CI but helps during development
        let _ = find_rustc_driver_binary();
    }

    #[test]
    fn test_get_rustc_sysroot() {
        let sysroot = get_rustc_sysroot();
        assert!(sysroot.is_ok());
    }
}

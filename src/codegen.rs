//! Code generation for sliced crates.
//!
//! Functions for generating module content and crate structure.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use super::types::{ParsedItem, CrateInfo, SemanticSliceResult, UsedItem};
use super::slicing::compute_needed_items;
use super::parsing::parse_crate;
use super::source_fix::format_source;
use super::arch::escape_keyword;

/// Generate a sliced crate in the output directory (module-based)
pub fn generate_module_sliced_crate(
    crate_info: &CrateInfo,
    used: &HashSet<UsedItem>,
    output_dir: &Path,
) -> std::io::Result<SemanticSliceResult> {
    // Parse the crate
    let index = parse_crate(&crate_info.path, &crate_info.name);

    // Compute needed items
    let needed = compute_needed_items(used, &index, &crate_info.name);

    // Create output directory structure
    fs::create_dir_all(output_dir.join("src"))?;

    // Collect items to include
    let mut included_items: Vec<&ParsedItem> = Vec::new();
    for name in &needed {
        for item in index.get_all(name) {
            included_items.push(item);
        }
        for impl_item in index.get_impls(name) {
            included_items.push(impl_item);
        }
    }

    // Generate lib.rs content
    let mut lib_content = String::new();
    lib_content.push_str("//! Sliced crate - auto-generated\n\n");

    for item in &included_items {
        lib_content.push_str(&item.source);
        lib_content.push_str("\n\n");
    }

    // Format and write
    let lib_content = format_source(&lib_content);
    let lib_path = output_dir.join("src").join("lib.rs");
    fs::write(&lib_path, &lib_content)?;

    // Generate Cargo.toml
    let cargo_content = format!(
        r#"[package]
name = "{}"
version = "{}"
edition = "{}"

[lib]
path = "src/lib.rs"

[dependencies]
"#,
        crate_info.name, crate_info.version, crate_info.edition
    );

    fs::write(output_dir.join("Cargo.toml"), cargo_content)?;

    Ok(SemanticSliceResult {
        total_parsed: index.all_items.len(),
        items_needed: needed.len(),
        items_included: included_items.len(),
        lines_generated: lib_content.lines().count(),
    })
}

/// Create module file path from module path
pub fn create_module_file_path(
    module_path: &str,
    output_dir: &Path,
) -> PathBuf {
    let parts: Vec<&str> = module_path.split("::").collect();
    let mut path = output_dir.join("src");

    for (i, part) in parts.iter().enumerate() {
        if *part == "crate" {
            continue;
        }
        if i == parts.len() - 1 {
            path = path.join(format!("{}.rs", escape_keyword(part)));
        } else {
            path = path.join(escape_keyword(part));
        }
    }

    path
}

/// Collect top-level modules from a module map
pub fn collect_top_level_modules(modules: &HashMap<String, Vec<&ParsedItem>>) -> Vec<String> {
    let mut top_level = HashSet::new();

    for module_path in modules.keys() {
        let parts: Vec<&str> = module_path.split("::").collect();
        if parts.len() >= 2 && parts[0] == "crate" {
            top_level.insert(parts[1].to_string());
        }
    }

    top_level.into_iter().collect()
}

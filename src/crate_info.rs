//! Phase 2: Crate source location and metadata.

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use super::types::CrateInfo;
use super::usage::find_rust_files;

/// Parse the Rust edition from a crate's Cargo.toml
pub fn parse_cargo_edition(crate_path: &Path) -> Option<String> {
    let cargo_toml = crate_path.join("Cargo.toml");
    if let Ok(content) = fs::read_to_string(&cargo_toml) {
        for line in content.lines() {
            let line = line.trim();
            if line.starts_with("edition") {
                if let Some(eq_pos) = line.find('=') {
                    let edition = line[eq_pos + 1..].trim().trim_matches('"').to_string();
                    return Some(edition);
                }
            }
        }
    }
    None
}

/// Find the cargo home directory
pub fn cargo_home() -> Option<PathBuf> {
    if let Ok(home) = env::var("CARGO_HOME") {
        return Some(PathBuf::from(home));
    }

    if let Ok(home) = env::var("HOME") {
        let cargo_path = PathBuf::from(home).join(".cargo");
        if cargo_path.exists() {
            return Some(cargo_path);
        }
    }

    None
}

/// Find the source directory for a crate in the cargo registry
pub fn find_crate_source(crate_name: &str) -> Option<CrateInfo> {
    let cargo = cargo_home()?;
    let registry_src = cargo.join("registry").join("src");

    if !registry_src.exists() {
        return None;
    }

    let mut best_match: Option<(PathBuf, String)> = None;

    if let Ok(entries) = fs::read_dir(&registry_src) {
        for entry in entries.flatten() {
            let mirror_path = entry.path();
            if mirror_path.is_dir() {
                if let Ok(crate_entries) = fs::read_dir(&mirror_path) {
                    for crate_entry in crate_entries.flatten() {
                        let crate_path = crate_entry.path();
                        let dir_name = crate_path.file_name()?.to_str()?;

                        if dir_name.starts_with(&format!("{}-", crate_name)) {
                            let version = dir_name.strip_prefix(&format!("{}-", crate_name))?;
                            if version.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                                if best_match.is_none() || version > best_match.as_ref()?.1.as_str() {
                                    best_match = Some((crate_path.clone(), version.to_string()));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    let (crate_path, version) = best_match?;

    let lib_path = if crate_path.join("src").join("lib.rs").exists() {
        Some(crate_path.join("src").join("lib.rs"))
    } else if crate_path.join("lib.rs").exists() {
        Some(crate_path.join("lib.rs"))
    } else {
        None
    };

    let edition = parse_cargo_edition(&crate_path).unwrap_or_else(|| "2015".to_string());
    let (total_items, total_lines) = count_crate_items(&crate_path);

    Some(CrateInfo {
        name: crate_name.to_string(),
        version,
        edition,
        path: crate_path,
        lib_path,
        total_items,
        total_lines,
    })
}

/// Count approximate items and lines in a crate
pub fn count_crate_items(crate_path: &Path) -> (usize, usize) {
    let mut total_items = 0;
    let mut total_lines = 0;

    let src_path = crate_path.join("src");
    let search_path = if src_path.exists() { src_path } else { crate_path.to_path_buf() };

    for file in find_rust_files(&search_path) {
        if let Ok(content) = fs::read_to_string(&file) {
            total_lines += content.lines().count();

            for line in content.lines() {
                let trimmed = line.trim();
                if trimmed.starts_with("pub fn ") || trimmed.starts_with("fn ") {
                    total_items += 1;
                } else if trimmed.starts_with("pub struct ") || trimmed.starts_with("struct ") {
                    total_items += 1;
                } else if trimmed.starts_with("pub enum ") || trimmed.starts_with("enum ") {
                    total_items += 1;
                } else if trimmed.starts_with("pub trait ") || trimmed.starts_with("trait ") {
                    total_items += 1;
                } else if trimmed.starts_with("pub type ") || trimmed.starts_with("type ") {
                    total_items += 1;
                } else if trimmed.starts_with("pub const ") || trimmed.starts_with("const ") {
                    total_items += 1;
                } else if trimmed.starts_with("pub static ") || trimmed.starts_with("static ") {
                    total_items += 1;
                } else if trimmed.starts_with("impl ") || trimmed.starts_with("impl<") {
                    total_items += 1;
                } else if trimmed.starts_with("pub mod ") || trimmed.starts_with("mod ") {
                    total_items += 1;
                } else if trimmed.starts_with("macro_rules!") {
                    total_items += 1;
                }
            }
        }
    }

    (total_items, total_lines)
}

/// Format a number with thousands separators
pub fn format_number(n: usize) -> String {
    let s = n.to_string();
    let mut result = String::new();
    for (i, c) in s.chars().rev().enumerate() {
        if i > 0 && i % 3 == 0 {
            result.push(',');
        }
        result.push(c);
    }
    result.chars().rev().collect()
}

/// Generate crate location report
pub fn generate_crate_report(crate_info: &CrateInfo, used_count: usize) {
    println!("\n=== Crate Source Information ===");
    println!("  Name: {}", crate_info.name);
    println!("  Version: {}", crate_info.version);
    println!("  Path: {}", crate_info.path.display());
    if let Some(lib) = &crate_info.lib_path {
        println!("  Entry: {}", lib.display());
    }
    println!("  Total lines: {}", format_number(crate_info.total_lines));
    println!("  Total items: {} (approximate)", format_number(crate_info.total_items));

    if crate_info.total_items > 0 {
        let usage_percent = (used_count as f64 / crate_info.total_items as f64) * 100.0;
        println!("\n=== Actual Savings Estimate ===");
        println!("  Items used: {} / {} ({:.2}%)", used_count, crate_info.total_items, usage_percent);
        println!("  Items unused: {} ({:.2}%)",
                 crate_info.total_items - used_count.min(crate_info.total_items),
                 100.0 - usage_percent);
        println!("  Potential lines saved: ~{}",
                 format_number((crate_info.total_lines as f64 * (1.0 - usage_percent / 100.0)) as usize));
    }
}

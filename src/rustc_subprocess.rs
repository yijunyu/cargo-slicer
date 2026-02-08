//! Rustc driver subprocess invocation for cargo-slicer
//!
//! This module provides integration with the cargo-slicer-rustc driver binary
//! WITHOUT linking rustc_private. It invokes the driver as a subprocess.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};

/// Information collected about items defined in a crate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItemInfo {
    /// Fully qualified name of the item
    pub path: String,
    /// Kind of item (function, type, const, etc.)
    pub kind: ItemKind,
    /// Visibility (pub, pub(crate), private)
    pub visibility: Visibility,
    /// Source location (file:line:col)
    pub location: String,
}

/// Kind of Rust item
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ItemKind {
    Function,
    Type,
    Trait,
    Impl,
    Const,
    Static,
    Macro,
    Mod,
}

/// Visibility level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    PublicCrate,
    Private,
}

/// Usage data collected from compilation
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct UsageData {
    /// Crate name being analyzed
    pub crate_name: String,

    /// All items defined in this crate
    pub defined_items: HashMap<String, ItemInfo>,

    /// Items referenced (used) from this crate
    /// Maps item path -> set of locations where it's referenced
    pub referenced_items: HashMap<String, HashSet<String>>,

    /// Dependencies between items within the crate
    /// Maps item -> set of items it depends on
    pub item_dependencies: HashMap<String, HashSet<String>>,
}

impl UsageData {
    /// Read usage data from a JSON file
    pub fn read_from_file(path: &Path) -> std::io::Result<Self> {
        let json = std::fs::read_to_string(path)?;
        serde_json::from_str(&json)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
    }
}

/// Use cargo rustc to compile with our custom driver
///
/// This is the CORRECT way to use rustc driver - let cargo handle:
/// - Dependency resolution (no sysroot conflicts)
/// - Edition handling (correct prelude imports)
/// - Build scripts (OUT_DIR, etc.)
/// - Feature flags
///
/// Returns 100% accurate compilation environment.
fn compile_with_cargo_rustc(crate_path: &Path, output_path: &Path) -> Result<(), String> {
    if std::env::var("CARGO_SLICER_RUSTC_DEBUG").is_ok() {
        eprintln!("[rustc_subprocess] Using cargo rustc for {}", crate_path.display());
    }

    // Find our rustc driver binary (must be absolute path for RUSTC_WRAPPER)
    let driver_path = find_rustc_driver_binary()?.canonicalize()
        .map_err(|e| format!("Failed to get absolute path for rustc driver: {}", e))?;

    // Set up environment for cargo rustc
    let rustc_sysroot = get_rustc_sysroot()?;
    let ld_library_path = format!("{}/lib", rustc_sysroot);

    let output = Command::new("cargo")
        .current_dir(crate_path)
        .env("RUSTC_WRAPPER", driver_path.to_str().unwrap())
        .env("LD_LIBRARY_PATH", &ld_library_path)
        .env("CARGO_SLICER_COLLECT_DATA", "1")
        .env("CARGO_SLICER_DATA_OUT", output_path)
        .arg("rustc")
        .arg("--lib")
        .arg("--quiet")
        .output()
        .map_err(|e| format!("Failed to run cargo rustc: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Cargo rustc failed: {}", stderr));
    }

    if std::env::var("CARGO_SLICER_RUSTC_DEBUG").is_ok() {
        eprintln!("[rustc_subprocess] Cargo rustc successful");
    }

    Ok(())
}

/// Pre-compile a crate to generate .rlib files for dependencies
///
/// Runs `cargo build` to compile the crate and all its dependencies,
/// making .rlib files available in target/debug/deps for rustc driver.
fn precompile_crate_dependencies(crate_path: &Path) -> Result<(), String> {
    if std::env::var("CARGO_SLICER_RUSTC_DEBUG").is_ok() {
        eprintln!("[rustc_subprocess] Pre-compiling dependencies for {}", crate_path.display());
    }

    let output = Command::new("cargo")
        .current_dir(crate_path)
        .arg("build")
        .arg("--lib")
        .arg("--quiet")
        .output()
        .map_err(|e| format!("Failed to run cargo build: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Cargo build failed: {}", stderr));
    }

    if std::env::var("CARGO_SLICER_RUSTC_DEBUG").is_ok() {
        eprintln!("[rustc_subprocess] Pre-compilation successful");
    }

    Ok(())
}

/// Run rustc driver on a crate to collect usage data
///
/// Returns UsageData with:
/// - defined_items: All items in the crate with their metadata
/// - item_dependencies: Dependency graph (what calls what)
/// - referenced_items: Reverse index (what is called by whom)
///
/// If `precompile` is true, runs `cargo build` first to ensure dependencies
/// are available as .rlib files, avoiding fallback to syn parser.
pub fn analyze_crate_with_rustc_driver(
    crate_path: &Path,
    crate_name: &str,
) -> Result<UsageData, String> {
    analyze_crate_with_rustc_driver_impl(crate_path, crate_name, false, false)
}

/// Run rustc driver with optional pre-compilation
pub fn analyze_crate_with_rustc_driver_precompile(
    crate_path: &Path,
    crate_name: &str,
    precompile: bool,
) -> Result<UsageData, String> {
    analyze_crate_with_rustc_driver_impl(crate_path, crate_name, precompile, false)
}

/// Run rustc driver with full control over compilation method
pub fn analyze_crate_with_rustc_driver_full(
    crate_path: &Path,
    crate_name: &str,
    precompile: bool,
    via_cargo: bool,
) -> Result<UsageData, String> {
    analyze_crate_with_rustc_driver_impl(crate_path, crate_name, precompile, via_cargo)
}

fn analyze_crate_with_rustc_driver_impl(
    crate_path: &Path,
    crate_name: &str,
    precompile: bool,
    via_cargo: bool,
) -> Result<UsageData, String> {
    // Create output path
    let output_path = std::env::temp_dir().join(format!("{}-rustc-data.json", crate_name));

    // If using cargo rustc, delegate everything to cargo
    if via_cargo {
        compile_with_cargo_rustc(crate_path, &output_path)?;
        return UsageData::read_from_file(&output_path)
            .map_err(|e| format!("Failed to read rustc driver output: {}", e));
    }

    // Otherwise use direct rustc invocation (legacy path)

    // Pre-compile dependencies if requested
    if precompile {
        precompile_crate_dependencies(crate_path)?;
    }
    // Find the rustc driver binary
    let driver_path = find_rustc_driver_binary()?;

    // Set up environment
    let rustc_sysroot = get_rustc_sysroot()?;
    let ld_library_path = format!("{}/lib", rustc_sysroot);

    // Find the main library file
    let lib_file = find_lib_file(crate_path)?;

    // Detect edition from Cargo.toml
    let edition = detect_edition(crate_path);

    // Find dependencies and build --extern flags
    let extern_flags = find_dependency_externs(crate_path, crate_name)?;

    // Debug: print extern flags if enabled
    if std::env::var("CARGO_SLICER_RUSTC_DEBUG").is_ok() && !extern_flags.is_empty() {
        eprintln!("[rustc_subprocess] Found {} dependencies for {}:", extern_flags.len(), crate_name);
        for (dep_name, dep_path) in &extern_flags {
            eprintln!("  --extern {}={}", dep_name, dep_path.display());
        }
    }

    // Build rustc command
    let mut cmd = Command::new(&driver_path);
    cmd.env("LD_LIBRARY_PATH", &ld_library_path)
        .env("CARGO_SLICER_COLLECT_DATA", "1")
        .env("CARGO_SLICER_DATA_OUT", &output_path)
        .arg(format!("--edition={}", edition))
        .arg("--crate-type=lib");

    // Add --extern flags for dependencies
    for (dep_name, dep_path) in extern_flags {
        cmd.arg("--extern");
        cmd.arg(format!("{}={}", dep_name, dep_path.display()));
    }

    cmd.arg(&lib_file);

    // Run the rustc driver
    let output = cmd.output()
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

/// Detect Rust edition from Cargo.toml
///
/// Returns the edition string ("2015", "2018", "2021") from the Cargo.toml,
/// defaulting to "2021" if not specified.
fn detect_edition(crate_path: &Path) -> String {
    let cargo_toml_path = crate_path.join("Cargo.toml");
    if let Ok(content) = std::fs::read_to_string(&cargo_toml_path) {
        // Simple regex-free parsing for edition = "..."
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("edition") {
                if let Some(eq_pos) = trimmed.find('=') {
                    let value = trimmed[eq_pos + 1..].trim();
                    // Remove quotes
                    let edition = value.trim_matches(|c| c == '"' || c == '\'');
                    if edition.starts_with("20") && edition.len() == 4 {
                        return edition.to_string();
                    }
                }
            }
        }
    }

    // Default to 2021
    "2021".to_string()
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

/// Find dependency .rlib files and build --extern flags
///
/// Searches for compiled dependencies in workspace target directories.
///
/// **Note**: This works best when dependencies are already compiled from a previous
/// build or in an incremental workflow. For fresh parsing, dependencies may not be
/// available yet, and the rustc driver will fall back to syn parser.
///
/// Search locations:
/// 1. Sliced workspace target directories (e.g., crate_name-sliced/target/*/deps/)
/// 2. Current project target directories
/// 3. Cargo registry cache (source only, no .rlib files)
fn find_dependency_externs(
    crate_path: &Path,
    _crate_name: &str,
) -> Result<Vec<(String, PathBuf)>, String> {
    let mut externs = Vec::new();

    // Parse Cargo.toml to get dependencies
    let cargo_toml_path = crate_path.join("Cargo.toml");
    if !cargo_toml_path.exists() {
        // No Cargo.toml, no dependencies
        return Ok(externs);
    }

    let cargo_toml = std::fs::read_to_string(&cargo_toml_path)
        .map_err(|e| format!("Failed to read Cargo.toml: {}", e))?;

    // Simple TOML parsing - look for [dependencies] section
    let deps = parse_dependencies_simple(&cargo_toml);

    if std::env::var("CARGO_SLICER_RUSTC_DEBUG").is_ok() {
        eprintln!("[rustc_subprocess] Parsed {} dependencies from Cargo.toml", deps.len());
    }

    // Search paths for .rlib files
    // Note: We look for pre-compiled dependencies from previous builds or workspace
    let mut search_paths = vec![
        // Workspace target directories (from sliced output)
        crate_path.parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("target/debug/deps"))
            .unwrap_or_else(|| PathBuf::from("target/debug/deps")),
        crate_path.parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("target/release/deps"))
            .unwrap_or_else(|| PathBuf::from("target/release/deps")),
        // Current project target directories
        PathBuf::from("target/debug/deps"),
        PathBuf::from("target/release/deps"),
    ];

    // Look for compiled crates in the sliced workspace
    if let Some(sliced_dir) = crate_path.parent().and_then(|p| p.parent()) {
        if let Ok(entries) = std::fs::read_dir(sliced_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    search_paths.push(path.join("target/debug/deps"));
                    search_paths.push(path.join("target/release/deps"));
                }
            }
        }
    }

    // Also check cargo registry
    if let Ok(home) = std::env::var("CARGO_HOME") {
        let registry_target = PathBuf::from(&home).join("registry/cache");
        search_paths.push(registry_target);
    }

    for dep_name in deps {
        // Try to find the .rlib or .rmeta file for this dependency
        if let Some(lib_path) = find_dependency_lib(&dep_name, &search_paths) {
            externs.push((dep_name.clone(), lib_path));
        } else if std::env::var("CARGO_SLICER_RUSTC_DEBUG").is_ok() {
            eprintln!("[rustc_subprocess] Could not find library for dependency: {}", dep_name);
        }
    }

    Ok(externs)
}

/// Simple dependency parser for Cargo.toml
///
/// Extracts dependency names from [dependencies] section
fn parse_dependencies_simple(cargo_toml: &str) -> Vec<String> {
    let mut deps = Vec::new();
    let mut in_dependencies = false;

    for line in cargo_toml.lines() {
        let trimmed = line.trim();

        // Check for section headers
        if trimmed.starts_with('[') {
            in_dependencies = trimmed == "[dependencies]";
            continue;
        }

        // Parse dependency lines
        if in_dependencies && !trimmed.is_empty() && !trimmed.starts_with('#') {
            if let Some(eq_pos) = trimmed.find('=') {
                let dep_name = trimmed[..eq_pos].trim().to_string();
                // Convert hyphens to underscores for crate names
                let dep_name_normalized = dep_name.replace('-', "_");
                deps.push(dep_name_normalized);
            }
        }
    }

    deps
}

/// Find the compiled library file for a dependency
///
/// Searches through search_paths for lib<dep_name>-*.rlib or lib<dep_name>-*.rmeta
fn find_dependency_lib(dep_name: &str, search_paths: &[PathBuf]) -> Option<PathBuf> {
    let lib_prefix = format!("lib{}-", dep_name);
    let lib_exact = format!("lib{}.rlib", dep_name);

    for search_path in search_paths {
        if !search_path.exists() {
            continue;
        }

        // Search this directory
        if let Some(lib) = search_in_dir(search_path, &lib_prefix, &lib_exact) {
            return Some(lib);
        }

        // Also search in subdirectories for sliced crates (crates_to_slice/<dep>-sliced/target/*/deps/)
        if search_path.ends_with("crates_to_slice") {
            if let Ok(entries) = std::fs::read_dir(search_path) {
                for entry in entries.flatten() {
                    let subdir = entry.path();
                    if subdir.is_dir() {
                        // Check target/debug/deps and target/release/deps
                        for target_dir in &["target/debug/deps", "target/release/deps", "target/debug", "target/release"] {
                            let deps_dir = subdir.join(target_dir);
                            if let Some(lib) = search_in_dir(&deps_dir, &lib_prefix, &lib_exact) {
                                return Some(lib);
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

/// Helper to search a directory for a matching library file
fn search_in_dir(dir: &Path, lib_prefix: &str, lib_exact: &str) -> Option<PathBuf> {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(filename) = path.file_name().and_then(|n| n.to_str()) {
                // Match lib<name>-<hash>.rlib or lib<name>.rlib
                if (filename.starts_with(lib_prefix) || filename == lib_exact)
                    && (filename.ends_with(".rlib") || filename.ends_with(".rmeta"))
                {
                    return Some(path);
                }
            }
        }
    }
    None
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
            ItemKind::Function => ParsedItemKind::Function,
            ItemKind::Type => ParsedItemKind::Struct, // Could be struct/enum/type alias
            ItemKind::Trait => ParsedItemKind::Trait,
            ItemKind::Impl => ParsedItemKind::Impl,
            ItemKind::Const => ParsedItemKind::Const,
            ItemKind::Static => ParsedItemKind::Static,
            ItemKind::Macro => ParsedItemKind::Macro,
            ItemKind::Mod => ParsedItemKind::Mod,
        };

        // Convert visibility
        let (is_pub, visibility) = match item_info.visibility {
            Visibility::Public => (true, ItemVisibility::Public),
            Visibility::PublicCrate => (true, ItemVisibility::Crate),
            Visibility::Private => (false, ItemVisibility::Private),
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

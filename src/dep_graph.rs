//! Dependency graph analysis.
//!
//! Functions for analyzing dependency graphs using cargo metadata and rust-analyzer.

use std::collections::{HashSet, BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;
use std::io::Read;
use std::sync::Mutex;

use once_cell::sync::Lazy;
use regex::Regex;

use super::types::{DepGraph, DepNode, UsedItem, ItemKind};

/// Tracks which version of a dependency each consumer crate needs.
/// Key: (consumer_name, consumer_version, dep_name)
/// Value: dep_version
pub type VersionMap = HashMap<(String, String, String), String>;

/// Information about multi-version crates in the dependency tree.
#[derive(Debug, Default)]
pub struct MultiVersionInfo {
    /// Crates that have multiple versions in the tree: crate_name -> [versions]
    pub multi_version_crates: HashMap<String, Vec<String>>,
    /// Exact version mapping: (consumer_name, consumer_version, dep_name) -> dep_version
    pub version_map: VersionMap,
}

/// Default timeout for rust-analyzer operations (300 seconds = 5 minutes)
/// This is set high to accommodate large/complex crates like tokio, syn, etc.
/// that may take several minutes to fully index with rust-analyzer SCIP.
///
/// Empirical measurements (direct rust-analyzer scip execution):
/// - tokio (95k LOC): 40s
/// - syn (50k LOC): 50s
/// - serde (15k LOC): ~20s
/// - regex (11k LOC): ~15s
///
/// However, when running through ra_deps subprocess, actual times can be 2-5x longer
/// due to process overhead, compilation, and dependency analysis.
pub const RA_TIMEOUT_SECS: u64 = 300;

/// Compute dynamic timeout based on crate size
/// Formula: base_timeout + (LOC / 1000) * seconds_per_kloc
/// This provides safety margin while avoiding excessive waits for small crates.
pub fn compute_dynamic_timeout(crate_path: &Path) -> Duration {
    // Base timeout: 120 seconds (2 minutes) for any crate
    let base_timeout = 120;

    // Additional time: 2 seconds per 1000 lines of code
    let seconds_per_kloc = 2;

    // Maximum timeout: 15 minutes (for extremely large crates)
    let max_timeout = 900;

    // Count LOC in the crate
    let loc = count_rust_loc(crate_path);

    // Calculate dynamic timeout
    let calculated = base_timeout + (loc / 1000) * seconds_per_kloc;
    let timeout = calculated.min(max_timeout);

    Duration::from_secs(timeout)
}

/// Count lines of Rust code in a crate
fn count_rust_loc(crate_path: &Path) -> u64 {
    let src_dir = crate_path.join("src");
    if !src_dir.exists() {
        return 0;
    }

    let mut total_lines = 0u64;

    // Use walkdir if available, otherwise simple recursive count
    if let Ok(entries) = std::fs::read_dir(&src_dir) {
        for entry in entries.flatten() {
            if let Ok(metadata) = entry.metadata() {
                if metadata.is_file() {
                    if let Some(ext) = entry.path().extension() {
                        if ext == "rs" {
                            if let Ok(content) = std::fs::read_to_string(entry.path()) {
                                total_lines += content.lines().count() as u64;
                            }
                        }
                    }
                } else if metadata.is_dir() {
                    // Recursively count subdirectories
                    total_lines += count_rust_loc(&entry.path());
                }
            }
        }
    }

    total_lines
}

/// Call ra_deps tool to get accurate dependency analysis via rust-analyzer
/// Uses dynamic timeout based on crate size to avoid hanging on large crates
pub fn run_ra_deps(crate_path: &Path, seed_items: &[String]) -> Result<BTreeSet<String>, String> {
    let timeout = compute_dynamic_timeout(crate_path);
    run_ra_deps_with_timeout(crate_path, seed_items, timeout)
}

/// Call ra_deps with a custom timeout
pub fn run_ra_deps_with_timeout(
    crate_path: &Path,
    seed_items: &[String],
    timeout: Duration,
) -> Result<BTreeSet<String>, String> {
    if seed_items.is_empty() {
        return Ok(BTreeSet::new());
    }

    // Build ra_deps command
    let ra_deps_path = std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|p| p.join("ra_deps")))
        .unwrap_or_else(|| PathBuf::from("ra_deps"));

    if !ra_deps_path.exists() {
        return Err(format!(
            "ra_deps binary not found at {}. Build with: cargo build --release --bin ra_deps",
            ra_deps_path.display()
        ));
    }

    let mut cmd = Command::new(&ra_deps_path);
    cmd.arg("--scip")  // Use SCIP batch mode for ~8x speedup
       .arg("--json")
       .arg(crate_path)
       .stdout(Stdio::piped())
       .stderr(Stdio::piped());

    for item in seed_items {
        cmd.arg(item);
    }

    // Spawn the process
    let mut child = cmd.spawn()
        .map_err(|e| format!("Failed to spawn ra_deps: {}", e))?;

    // Wait with timeout using a simple polling approach
    let start = std::time::Instant::now();
    let poll_interval = Duration::from_millis(100);

    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                // Process finished
                let mut stdout = String::new();
                let mut stderr = String::new();

                if let Some(mut out) = child.stdout.take() {
                    out.read_to_string(&mut stdout).ok();
                }
                if let Some(mut err) = child.stderr.take() {
                    err.read_to_string(&mut stderr).ok();
                }

                if !status.success() {
                    return Err(format!("ra_deps failed: {}", stderr));
                }

                // Parse the output
                return parse_ra_deps_output(&stdout);
            }
            Ok(None) => {
                // Still running, check timeout
                if start.elapsed() > timeout {
                    // Kill the process
                    let _ = child.kill();
                    let _ = child.wait();
                    return Err(format!(
                        "ra_deps timed out after {}s for {}",
                        timeout.as_secs(),
                        crate_path.display()
                    ));
                }
                std::thread::sleep(poll_interval);
            }
            Err(e) => {
                return Err(format!("Error waiting for ra_deps: {}", e));
            }
        }
    }
}

/// Parse ra_deps JSON output
fn parse_ra_deps_output(stdout: &str) -> Result<BTreeSet<String>, String> {
    // Find the JSON output (skip stderr lines that go to stdout)
    let json_start = stdout.find('{')
        .ok_or("No JSON output from ra_deps")?;
    let json_str = &stdout[json_start..];

    // Parse JSON output
    let json: serde_json::Value = serde_json::from_str(json_str)
        .map_err(|e| format!("Failed to parse ra_deps output: {}", e))?;

    let mut needed = BTreeSet::new();

    if let Some(items) = json.get("items").and_then(|v| v.as_array()) {
        for item in items {
            if let Some(name) = item.get("name").and_then(|v| v.as_str()) {
                needed.insert(name.to_string());
            }
        }
    }

    Ok(needed)
}

/// Analyze dependency graph using cargo metadata
pub fn analyze_dependency_graph(
    manifest_path: Option<&Path>,
    features: Option<&str>,
    all_features: bool,
    no_default_features: bool,
) -> Result<DepGraph, String> {
    let mut cmd = Command::new("cargo");
    cmd.args(["metadata", "--format-version", "1"]);
    
    if let Some(path) = manifest_path {
        cmd.arg("--manifest-path").arg(path);
    }

    if let Some(f) = features {
        cmd.arg("--features").arg(f);
    }
    if all_features {
        cmd.arg("--all-features");
    }
    if no_default_features {
        cmd.arg("--no-default-features");
    }

    let output = cmd.output()
        .map_err(|e| format!("Failed to run cargo metadata: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    let metadata: serde_json::Value = serde_json::from_slice(&output.stdout)
        .map_err(|e| format!("Failed to parse metadata: {}", e))?;

    let mut graph = DepGraph::default();

    // Get root package
    if let Some(resolve) = metadata.get("resolve") {
        if let Some(root) = resolve.get("root").and_then(|r| r.as_str()) {
            // Root format can be:
            // - "package_name version (path)" (older cargo)
            // - "path+file:///path#name@version" (newer cargo with name)
            // - "path+file:///path#version" (newer cargo without explicit name)
            let root_name = if root.contains('#') {
                let after_hash = root.split('#').last().unwrap_or("");
                if after_hash.contains('@') {
                    // New format with name: "path+file:///path#name@version"
                    after_hash.split('@').next().unwrap_or("precc")
                } else {
                    // New format without name: "path+file:///path#version"
                    // Extract path and find matching package
                    let path = root.strip_prefix("path+file://").unwrap_or(root);
                    let path = path.split('#').next().unwrap_or(path);
                    // Find package with matching manifest path (exact match or closest)
                    if let Some(packages) = metadata.get("packages").and_then(|p| p.as_array()) {
                        // Clean the path for comparison
                        let clean_path = path.trim_end_matches('/');
                        let cargo_toml = format!("{}/Cargo.toml", clean_path);

                        // First try exact manifest_path match
                        packages.iter()
                            .find(|p| p.get("manifest_path")
                                .and_then(|m| m.as_str())
                                .map(|m| m == &cargo_toml)
                                .unwrap_or(false))
                            .or_else(|| {
                                // Fallback: find the package whose manifest is in the root path
                                // (not in a subdirectory like sliced_crates/)
                                packages.iter()
                                    .find(|p| p.get("manifest_path")
                                        .and_then(|m| m.as_str())
                                        .map(|m| m.starts_with(clean_path) && !m[clean_path.len()..].contains('/'))
                                        .unwrap_or(false))
                            })
                            .and_then(|p| p.get("name").and_then(|n| n.as_str()))
                            .unwrap_or("precc")
                    } else {
                        "precc"
                    }
                }
            } else {
                root.split_whitespace().next().unwrap_or(root)
            };
            graph.root = root_name.to_string();
        }
    }

    // Build nodes from packages
    if let Some(packages) = metadata.get("packages").and_then(|p| p.as_array()) {
        for pkg in packages {
            let name = pkg.get("name").and_then(|n| n.as_str()).unwrap_or("");
            let version = pkg.get("version").and_then(|v| v.as_str()).unwrap_or("");
            let edition = pkg.get("edition").and_then(|e| e.as_str()).unwrap_or("2018");
            let manifest = pkg.get("manifest_path").and_then(|m| m.as_str());

            // Get dependencies (normal, dev, build)
            let mut deps = Vec::new();
            let mut dev_deps = Vec::new();
            let mut build_deps = Vec::new();

            if let Some(dep_array) = pkg.get("dependencies").and_then(|d| d.as_array()) {
                for dep in dep_array {
                    if let Some(dep_name) = dep.get("name").and_then(|n| n.as_str()) {
                        let kind = dep.get("kind").and_then(|k| k.as_str());
                        match kind {
                            None | Some("normal") => deps.push(dep_name.to_string()),
                            Some("dev") => dev_deps.push(dep_name.to_string()),
                            Some("build") => build_deps.push(dep_name.to_string()),
                            _ => {} // Ignore other kinds
                        }
                    }
                }
            }

            graph.nodes.insert(
                name.to_string(),
                DepNode {
                    name: name.to_string(),
                    version: version.to_string(),
                    edition: edition.to_string(),
                    deps,
                    dev_deps,
                    build_deps,
                    manifest_path: manifest.map(PathBuf::from),
                },
            );
        }
    }

    Ok(graph)
}

/// Build multi-version dependency information from cargo metadata.
///
/// This parses the "resolve" section of cargo metadata to:
/// 1. Identify crates with multiple versions in the dependency tree
/// 2. Build exact version mappings: (consumer, dep_name) -> dep_version
pub fn build_multi_version_info(
    manifest_path: Option<&Path>,
    features: Option<&str>,
    all_features: bool,
    no_default_features: bool,
) -> Result<MultiVersionInfo, String> {
    let mut cmd = Command::new("cargo");
    cmd.args(["metadata", "--format-version", "1"]);

    if let Some(path) = manifest_path {
        cmd.arg("--manifest-path").arg(path);
    }
    
    if let Some(f) = features {
        cmd.arg("--features").arg(f);
    }
    if all_features {
        cmd.arg("--all-features");
    }
    if no_default_features {
        cmd.arg("--no-default-features");
    }

    let output = cmd.output()
        .map_err(|e| format!("Failed to run cargo metadata: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    let metadata: serde_json::Value = serde_json::from_slice(&output.stdout)
        .map_err(|e| format!("Failed to parse metadata: {}", e))?;

    let mut info = MultiVersionInfo::default();

    // First pass: collect all versions of each crate
    let mut crate_versions: HashMap<String, HashSet<String>> = HashMap::new();

    if let Some(resolve) = metadata.get("resolve") {
        if let Some(nodes) = resolve.get("nodes").and_then(|n| n.as_array()) {
            for node in nodes {
                if let Some(id) = node.get("id").and_then(|i| i.as_str()) {
                    // Parse id format: "registry+.../index#name@version" or "path+..."
                    if let Some((name, version)) = parse_pkg_id(id) {
                        crate_versions.entry(name).or_default().insert(version);
                    }
                }
            }
        }
    }

    // Identify multi-version crates
    for (name, versions) in &crate_versions {
        if versions.len() > 1 {
            let mut v: Vec<String> = versions.iter().cloned().collect();
            v.sort_by(|a, b| version_cmp(a, b));
            info.multi_version_crates.insert(name.clone(), v);
        }
    }

    // Second pass: build version mapping for consumers of multi-version crates
    if let Some(resolve) = metadata.get("resolve") {
        if let Some(nodes) = resolve.get("nodes").and_then(|n| n.as_array()) {
            for node in nodes {
                let consumer_id = node.get("id").and_then(|i| i.as_str()).unwrap_or("");
                let (consumer_name, consumer_version) = parse_pkg_id(consumer_id)
                    .unwrap_or(("".to_string(), "".to_string()));

                if let Some(deps) = node.get("deps").and_then(|d| d.as_array()) {
                    for dep in deps {
                        let dep_pkg = dep.get("pkg").and_then(|p| p.as_str()).unwrap_or("");
                        if let Some((dep_name, dep_version)) = parse_pkg_id(dep_pkg) {
                            // Only record if this is a multi-version crate
                            if info.multi_version_crates.contains_key(&dep_name) {
                                info.version_map.insert(
                                    (consumer_name.clone(), consumer_version.clone(), dep_name),
                                    dep_version,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(info)
}

/// Parse a cargo package id to extract name and version.
/// Handles formats:
/// - "registry+https://github.com/rust-lang/crates.io-index#name@version"
/// - "path+file:///path/to/crate#version" or "#name@version"
fn parse_pkg_id(id: &str) -> Option<(String, String)> {
    // Find the # which separates the source from the name@version
    let after_hash = id.split('#').last()?;

    if after_hash.contains('@') {
        // Format: name@version
        let mut parts = after_hash.splitn(2, '@');
        let name = parts.next()?.to_string();
        let version = parts.next()?.to_string();
        Some((name, version))
    } else {
        // Format: just version (path dependencies)
        // Try to extract name from the path
        None
    }
}

/// Compare version strings semantically (major.minor.patch).
fn version_cmp(a: &str, b: &str) -> std::cmp::Ordering {
    let parse = |s: &str| -> Vec<u32> {
        s.split(|c: char| !c.is_ascii_digit())
            .filter_map(|p| p.parse().ok())
            .collect()
    };
    let av = parse(a);
    let bv = parse(b);
    av.cmp(&bv)
}

/// Print dependency analysis
pub fn print_dep_analysis(graph: &DepGraph) {
    println!("=== Dependency Graph Analysis ===\n");
    println!("Root package: {}", graph.root);
    println!("Total packages: {}\n", graph.nodes.len());

    println!("Direct dependencies:");
    for dep in graph.direct_deps() {
        if let Some(node) = graph.nodes.get(dep) {
            println!("  {} v{} ({} deps)", node.name, node.version, node.deps.len());
        }
    }

    println!("\nTopological order (leaf-first):");
    for (i, name) in graph.topo_order().iter().enumerate() {
        if *name != graph.root {
            println!("  {}. {}", i + 1, name);
        }
    }
}

/// Regex cache for qualified path patterns
/// Caches regexes for crate_name::Type patterns to avoid recompilation
static QUALIFIED_PATH_REGEX_CACHE: Lazy<Mutex<HashMap<String, Regex>>> = Lazy::new(|| {
    Mutex::new(HashMap::new())
});

/// Analyze usage of a target crate from another crate's source directory
pub fn analyze_crate_usage_from_source(src_path: &Path, target_crate: &str) -> HashSet<UsedItem> {
    let mut used = HashSet::new();
    let rust_crate_name = target_crate.replace('-', "_");

    // Find all .rs files in the source directory
    let pattern = format!("{}/**/*.rs", src_path.display());
    if let Ok(entries) = glob::glob(&pattern) {
        for entry in entries.flatten() {
            if let Ok(content) = std::fs::read_to_string(&entry) {
                // Look for use statements
                for line in content.lines() {
                    let trimmed = line.trim();
                    if trimmed.starts_with("use ") && trimmed.contains(&rust_crate_name) {
                        // Parse the use statement
                        if let Some(path) = trimmed.strip_prefix("use ") {
                            let path = path.trim_end_matches(';').trim();
                            if path.starts_with(&rust_crate_name) {
                                // Handle use crate::{A, B} syntax
                                if path.contains('{') {
                                    if let Some(brace_start) = path.find('{') {
                                        let prefix = &path[..brace_start];
                                        if let Some(brace_end) = path.find('}') {
                                            let items = &path[brace_start+1..brace_end];
                                            for item in items.split(',') {
                                                let item = item.trim();
                                                if !item.is_empty() && item != "*" {
                                                    used.insert(UsedItem {
                                                        path: format!("{}{}", prefix, item),
                                                        kind: ItemKind::Type,
                                                    });
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    used.insert(UsedItem {
                                        path: path.to_string(),
                                        kind: ItemKind::Type,
                                    });
                                }
                            }
                        }
                    }

                    // Also look for qualified paths like crate_name::Type
                    if trimmed.contains(&format!("{}::", rust_crate_name)) {
                        let re_pattern = format!(r"{}::(\w+)", regex::escape(&rust_crate_name));

                        // Try to get cached regex
                        let re_opt = {
                            let cache = QUALIFIED_PATH_REGEX_CACHE.lock().unwrap();
                            cache.get(&re_pattern).cloned()
                        };

                        let re = match re_opt {
                            Some(cached_re) => cached_re,
                            None => {
                                // Compile and cache
                                if let Ok(new_re) = Regex::new(&re_pattern) {
                                    let mut cache = QUALIFIED_PATH_REGEX_CACHE.lock().unwrap();
                                    cache.insert(re_pattern.clone(), new_re.clone());
                                    new_re
                                } else {
                                    continue;
                                }
                            }
                        };

                        for cap in re.captures_iter(trimmed) {
                            if let Some(name) = cap.get(1) {
                                used.insert(UsedItem {
                                    path: format!("{}::{}", rust_crate_name, name.as_str()),
                                    kind: ItemKind::Type,
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    used
}

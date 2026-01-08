//! Dependency graph analysis.
//!
//! Functions for analyzing dependency graphs using cargo metadata and rust-analyzer.

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;
use std::io::Read;

use super::types::{DepGraph, DepNode, UsedItem, ItemKind};

/// Default timeout for rust-analyzer operations (60 seconds)
pub const RA_TIMEOUT_SECS: u64 = 60;

/// Call ra_deps tool to get accurate dependency analysis via rust-analyzer
/// Times out after RA_TIMEOUT_SECS seconds to avoid hanging on large crates
pub fn run_ra_deps(crate_path: &Path, seed_items: &[String]) -> Result<HashSet<String>, String> {
    run_ra_deps_with_timeout(crate_path, seed_items, Duration::from_secs(RA_TIMEOUT_SECS))
}

/// Call ra_deps with a custom timeout
pub fn run_ra_deps_with_timeout(
    crate_path: &Path,
    seed_items: &[String],
    timeout: Duration,
) -> Result<HashSet<String>, String> {
    if seed_items.is_empty() {
        return Ok(HashSet::new());
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
fn parse_ra_deps_output(stdout: &str) -> Result<HashSet<String>, String> {
    // Find the JSON output (skip stderr lines that go to stdout)
    let json_start = stdout.find('{')
        .ok_or("No JSON output from ra_deps")?;
    let json_str = &stdout[json_start..];

    // Parse JSON output
    let json: serde_json::Value = serde_json::from_str(json_str)
        .map_err(|e| format!("Failed to parse ra_deps output: {}", e))?;

    let mut needed = HashSet::new();

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
pub fn analyze_dependency_graph() -> Result<DepGraph, String> {
    let output = Command::new("cargo")
        .args(["metadata", "--format-version", "1"])
        .output()
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

            // Get dependencies
            let mut deps = Vec::new();
            if let Some(dep_array) = pkg.get("dependencies").and_then(|d| d.as_array()) {
                for dep in dep_array {
                    if let Some(dep_name) = dep.get("name").and_then(|n| n.as_str()) {
                        // Only include non-dev, non-build dependencies
                        let kind = dep.get("kind").and_then(|k| k.as_str());
                        if kind.is_none() || kind == Some("normal") {
                            deps.push(dep_name.to_string());
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
                    manifest_path: manifest.map(PathBuf::from),
                },
            );
        }
    }

    Ok(graph)
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
                        if let Ok(re) = regex::Regex::new(&re_pattern) {
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
    }

    used
}

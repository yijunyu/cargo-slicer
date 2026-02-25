//! Non-workspace dependency analysis and pruning.
//!
//! Analyzes external (registry/git) dependency crates to determine which
//! functions are reachable from the workspace, allowing unreachable private
//! functions to be stubbed during compilation.
//!
//! ## Architecture
//!
//! 1. **Discovery**: `cargo metadata` to find all non-workspace deps with source paths
//! 2. **Reference scanning**: Text scan of workspace source for `use dep::...` and `dep::path()` patterns
//! 3. **Classification**: Tiered heuristics to skip deps where pruning won't help
//!    - T0: proc-macro, build-only, or <500 LOC → skip
//!    - T1: 0 workspace references → skip
//!    - T2: >50% generic functions → skip (generics can't be stubbed)
//!    - T3: Analyze with FastTokenizer + BFS
//! 4. **Analysis**: For T3 deps, extract items + internal call edges, BFS from
//!    workspace entry points, write `.dep-seeds` files
//!
//! The driver loads `.dep-seeds` and stubs unreachable private functions.

use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::slicer::fast_tokenizer::{FastTokenizer, scan_call_edges};
use crate::types::ParsedItemKind;

/// A non-workspace dependency crate discovered via cargo metadata.
#[derive(Debug)]
pub struct DepCrate {
    pub name: String,
    pub rust_name: String,
    pub version: String,
    pub source_path: PathBuf,
    pub is_proc_macro: bool,
}

/// Classification tier for dependency pruning.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DepTier {
    /// Proc-macro, build-only, or too small to matter
    T0Skip,
    /// No workspace references found
    T1Skip,
    /// Too generic or trait-heavy to benefit from pruning
    T2Skip,
    /// Worth analyzing
    T3Analyze,
}

/// Run dependency analysis as part of the pre-analysis pipeline.
///
/// Called after workspace analysis and cross-crate BFS are complete.
/// Discovers deps, classifies them, analyzes promising ones, and writes
/// `.dep-seeds` files for the driver to use.
pub fn run_dep_analysis(workspace_root: &Path, cache_dir: &Path) -> Result<(), String> {
    let start = std::time::Instant::now();

    // 1. Discover all non-workspace deps
    let t0 = std::time::Instant::now();
    let (deps, workspace_source_dirs) = discover_dep_crates(workspace_root)?;
    if deps.is_empty() {
        eprintln!("[dep-analyze] No non-workspace dependencies found");
        return Ok(());
    }
    let discover_ms = t0.elapsed().as_millis();

    // Build set of dep rust names for reference scanning
    let dep_names: HashSet<String> = deps.iter().map(|d| d.rust_name.clone()).collect();

    // 2. Collect all workspace .rs files
    let workspace_rs_files = collect_workspace_rs_files(&workspace_source_dirs);

    // 3. Scan workspace source for dep references
    let t1 = std::time::Instant::now();
    let dep_refs = scan_workspace_dep_refs(&workspace_rs_files, &dep_names);
    let scan_ms = t1.elapsed().as_millis();

    // Also collect wildcard method names from .analysis files
    let wildcard_methods = collect_wildcard_methods(cache_dir);

    eprintln!("[dep-analyze] {} deps, {} workspace files, {} wildcard methods (discover={}ms, scan={}ms)",
        deps.len(), workspace_rs_files.len(), wildcard_methods.len(), discover_ms, scan_ms);

    // 4. Classify deps
    let mut t0 = 0usize;
    let mut t1 = 0usize;
    let mut t2 = 0usize;
    let mut t3_deps: Vec<(&DepCrate, HashSet<String>)> = Vec::new();

    for dep in &deps {
        let refs = dep_refs.get(&dep.rust_name).cloned().unwrap_or_default();
        let tier = classify_dep(dep, &refs);
        match tier {
            DepTier::T0Skip => t0 += 1,
            DepTier::T1Skip => t1 += 1,
            DepTier::T2Skip => t2 += 1,
            DepTier::T3Analyze => {
                t3_deps.push((dep, refs));
            }
        }
    }
    eprintln!("[dep-analyze] Classification: T0(skip)={}, T1(no-refs)={}, T2(generic)={}, T3(analyze)={}",
        t0, t1, t2, t3_deps.len());

    if t3_deps.is_empty() {
        eprintln!("[dep-analyze] No deps worth analyzing, skipping");
        return Ok(());
    }

    // 5. Analyze T3 deps (in parallel)
    use rayon::prelude::*;
    let seeds_written = AtomicUsize::new(0);
    t3_deps.par_iter().for_each(|(dep, refs)| {
        match analyze_dep(dep, refs, &wildcard_methods, cache_dir) {
            Ok(count) => {
                if count > 0 {
                    seeds_written.fetch_add(1, Ordering::Relaxed);
                    eprintln!("[dep-analyze]   {} → {} reachable items, .dep-seeds written",
                        dep.rust_name, count);
                } else {
                    eprintln!("[dep-analyze]   {} → all items reachable, skipping", dep.rust_name);
                }
            }
            Err(e) => {
                eprintln!("[dep-analyze]   {} → error: {}", dep.rust_name, e);
            }
        }
    });

    eprintln!("[dep-analyze] Wrote {} .dep-seeds files in {:.1}s",
        seeds_written.load(Ordering::Relaxed), start.elapsed().as_secs_f64());

    Ok(())
}

/// Discover non-workspace dependency crates and workspace source directories.
fn discover_dep_crates(workspace_root: &Path) -> Result<(Vec<DepCrate>, Vec<PathBuf>), String> {
    let output = std::process::Command::new("cargo")
        .args(["+nightly", "metadata", "--format-version=1"])
        .current_dir(workspace_root)
        .output()
        .map_err(|e| format!("cargo metadata: {}", e))?;

    if !output.status.success() {
        return Err(format!("cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr)));
    }

    let json: serde_json::Value = serde_json::from_slice(&output.stdout)
        .map_err(|e| format!("parse cargo metadata: {}", e))?;

    let workspace_members: HashSet<String> = json["workspace_members"]
        .as_array()
        .unwrap_or(&vec![])
        .iter()
        .filter_map(|v| v.as_str().map(String::from))
        .collect();

    let packages = json["packages"].as_array()
        .ok_or("No packages in cargo metadata")?;

    let mut workspace_source_dirs: Vec<PathBuf> = Vec::new();
    let mut deps = Vec::new();

    for pkg in packages {
        let pkg_id = pkg["id"].as_str().unwrap_or("");
        let is_workspace = workspace_members.contains(pkg_id);

        let targets = pkg["targets"].as_array();
        if targets.is_none() { continue; }

        for target in targets.unwrap() {
            let empty_vec = vec![];
            let kinds: Vec<&str> = target["kind"].as_array().unwrap_or(&empty_vec)
                .iter().filter_map(|k| k.as_str()).collect();
            let src_path = target["src_path"].as_str().unwrap_or("");
            if src_path.is_empty() { continue; }

            if is_workspace {
                // Collect workspace source directories for reference scanning
                if kinds.iter().any(|k| *k == "lib" || *k == "bin") {
                    if let Some(parent) = Path::new(src_path).parent() {
                        workspace_source_dirs.push(parent.to_path_buf());
                    }
                }
                continue;
            }

            // Non-workspace: only library targets
            let is_lib = kinds.iter().any(|k| *k == "lib");
            let is_proc_macro = kinds.contains(&"proc-macro");

            // Skip non-library targets (bin, test, example, bench, custom-build)
            if !is_lib && !is_proc_macro { continue; }

            let name = pkg["name"].as_str().unwrap_or("").to_string();
            let version = pkg["version"].as_str().unwrap_or("").to_string();

            deps.push(DepCrate {
                rust_name: name.replace('-', "_"),
                name,
                version,
                source_path: PathBuf::from(src_path),
                is_proc_macro,
            });
        }
    }

    Ok((deps, workspace_source_dirs))
}

/// Recursively collect all .rs files from workspace source directories.
fn collect_workspace_rs_files(source_dirs: &[PathBuf]) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut seen_dirs = HashSet::new();

    for dir in source_dirs {
        // Walk up to find the crate source root (src/ directory or crate root)
        // source_dirs contains the parent of lib.rs/main.rs (typically src/)
        if seen_dirs.contains(dir) { continue; }
        seen_dirs.insert(dir.clone());
        collect_rs_files_recursive(dir, &mut files);
    }

    files
}

fn collect_rs_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files_recursive(&path, files);
        } else if path.extension().map_or(false, |e| e == "rs") {
            files.push(path);
        }
    }
}

/// Scan workspace source files for references to dependency crate names.
///
/// Returns: dep_rust_name → set of referenced qualified paths.
/// E.g., "encoding_rs" → {"Encoding::for_label", "Decoder::decode_to_string"}
fn scan_workspace_dep_refs(
    workspace_files: &[PathBuf],
    dep_names: &HashSet<String>,
) -> HashMap<String, HashSet<String>> {
    use regex::Regex;

    let mut refs: HashMap<String, HashSet<String>> = HashMap::new();

    // Build regex patterns for each dep name
    // Match: `use dep_name::path;` and `dep_name::path::item(`
    // We compile one big alternation regex for efficiency
    if dep_names.is_empty() { return refs; }

    // Pattern 1: `use dep_name::path` — captures the full path after the dep name
    // Pattern 2: `dep_name::path(` — direct qualified calls
    // Both use word boundary to avoid matching substrings (e.g., "serde" in "serde_json")
    let dep_names_sorted: Vec<&String> = {
        let mut v: Vec<&String> = dep_names.iter().collect();
        // Sort longest first to avoid prefix matching issues
        v.sort_by(|a, b| b.len().cmp(&a.len()));
        v
    };

    // Build per-dep regex for use statements
    let use_pattern = Regex::new(
        &format!(r"use\s+((?:{})::[\w::*]+)", dep_names_sorted.iter()
            .map(|n| regex::escape(n))
            .collect::<Vec<_>>()
            .join("|"))
    ).unwrap();

    // Build per-dep regex for qualified calls
    let call_pattern = Regex::new(
        &format!(r"\b((?:{})::[\w::]+)\s*[(\[]", dep_names_sorted.iter()
            .map(|n| regex::escape(n))
            .collect::<Vec<_>>()
            .join("|"))
    ).unwrap();

    for file_path in workspace_files {
        let source = match std::fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(_) => continue,
        };

        // Scan for use statements
        for cap in use_pattern.captures_iter(&source) {
            if let Some(full_path) = cap.get(1) {
                let path_str = full_path.as_str();
                if let Some(dep_name) = extract_dep_name(path_str, dep_names) {
                    // Store path without dep prefix for matching
                    let remainder = &path_str[dep_name.len() + 2..]; // skip "dep_name::"
                    refs.entry(dep_name.clone())
                        .or_default()
                        .insert(remainder.to_string());
                }
            }
        }

        // Scan for qualified calls
        for cap in call_pattern.captures_iter(&source) {
            if let Some(full_path) = cap.get(1) {
                let path_str = full_path.as_str();
                if let Some(dep_name) = extract_dep_name(path_str, dep_names) {
                    let remainder = &path_str[dep_name.len() + 2..];
                    refs.entry(dep_name.clone())
                        .or_default()
                        .insert(remainder.to_string());
                }
            }
        }
    }

    refs
}

/// Extract the dep name from a qualified path like "encoding_rs::Encoding::for_label".
fn extract_dep_name<'a>(path: &str, dep_names: &'a HashSet<String>) -> Option<&'a String> {
    let first_segment = path.split("::").next()?;
    dep_names.get(first_segment)
}

/// Collect wildcard method names from workspace .analysis files.
///
/// These are method calls like `obj.method()` that the pre-analysis couldn't
/// resolve to a specific crate. Format: `*::method_name` in call_edges.
fn collect_wildcard_methods(cache_dir: &Path) -> HashSet<String> {
    let mut methods = HashSet::new();

    let entries = match std::fs::read_dir(cache_dir) {
        Ok(e) => e,
        Err(_) => return methods,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map_or(true, |e| e != "analysis") { continue; }

        let content = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => continue,
        };

        let mut in_call_edges = false;
        for line in content.lines() {
            if line == "CALL_EDGES:" {
                in_call_edges = true;
                continue;
            }
            if line.ends_with(':') && !line.contains('\t') {
                in_call_edges = false;
                continue;
            }
            if !in_call_edges { continue; }

            // Format: caller\tcallee
            if let Some(callee) = line.split('\t').nth(1) {
                if let Some(method) = callee.strip_prefix("*::") {
                    methods.insert(method.to_string());
                }
            }
        }
    }

    methods
}

/// Classify a dependency crate using tiered heuristics.
///
/// Check order is optimized: free checks first, file I/O last.
fn classify_dep(dep: &DepCrate, workspace_refs: &HashSet<String>) -> DepTier {
    // T0: proc-macro (runs at compile time, not in binary) — free check
    if dep.is_proc_macro {
        return DepTier::T0Skip;
    }

    // T0: source path doesn't exist or is not in .cargo/registry — free check
    let src_str = dep.source_path.to_string_lossy();
    if !src_str.contains(".cargo/registry") && !src_str.contains(".cargo/git") {
        return DepTier::T0Skip;
    }

    // T1: no workspace references — free check, eliminates most deps
    if workspace_refs.is_empty() {
        return DepTier::T1Skip;
    }

    // T0: count LOC — skip tiny deps (<500 LOC)
    // Only reached for deps with workspace refs (~60 deps for helix)
    let source_dir = dep.source_path.parent().unwrap_or(Path::new("."));
    let loc = count_loc(source_dir);
    if loc < 500 {
        return DepTier::T0Skip;
    }

    // T2: high generic ratio — quick scan for generic markers
    let generic_pct = estimate_generic_ratio(source_dir);
    if generic_pct > 50 {
        return DepTier::T2Skip;
    }

    DepTier::T3Analyze
}

/// Count lines of Rust code in a directory (recursive).
fn count_loc(dir: &Path) -> usize {
    let mut total = 0;
    let mut files = Vec::new();
    collect_rs_files_recursive(dir, &mut files);

    for file in &files {
        if let Ok(content) = std::fs::read_to_string(file) {
            total += content.lines()
                .filter(|l| {
                    let trimmed = l.trim();
                    !trimmed.is_empty() && !trimmed.starts_with("//")
                })
                .count();
        }
    }
    total
}

/// Estimate the percentage of functions that are generic.
///
/// Quick heuristic: count lines containing generic markers (<T, <T:, impl<, where)
/// vs total function definitions.
fn estimate_generic_ratio(dir: &Path) -> usize {
    let mut files = Vec::new();
    collect_rs_files_recursive(dir, &mut files);

    let mut fn_count = 0usize;
    let mut generic_fn_count = 0usize;

    for file in &files {
        let content = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(_) => continue,
        };

        let lines: Vec<&str> = content.lines().collect();
        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            // Count function definitions
            if (trimmed.starts_with("pub fn ") || trimmed.starts_with("fn ")
                || trimmed.starts_with("pub(crate) fn ") || trimmed.starts_with("pub(super) fn ")
                || trimmed.starts_with("pub async fn ") || trimmed.starts_with("async fn ")
                || trimmed.starts_with("pub const fn ") || trimmed.starts_with("const fn ")
                || trimmed.starts_with("pub unsafe fn ") || trimmed.starts_with("unsafe fn "))
                && !trimmed.starts_with("//")
            {
                fn_count += 1;
                // Check if this function or its context has generic markers
                if trimmed.contains('<') || trimmed.contains("where") {
                    generic_fn_count += 1;
                } else if i + 1 < lines.len() {
                    let next = lines[i + 1].trim();
                    if next.starts_with("where") {
                        generic_fn_count += 1;
                    }
                }
            }
        }
    }

    if fn_count == 0 { return 0; }
    (generic_fn_count * 100) / fn_count
}

/// Analyze a T3 dependency: extract items, run BFS, write .dep-seeds.
///
/// Returns the number of reachable items written, or 0 if all items are reachable.
fn analyze_dep(
    dep: &DepCrate,
    workspace_refs: &HashSet<String>,
    wildcard_methods: &HashSet<String>,
    cache_dir: &Path,
) -> Result<usize, String> {
    // 1. Build module tree for the dep
    let modules = crate::pre_analyze::build_module_tree(&dep.source_path, &dep.rust_name)?;

    // 2. Collect imports for call edge resolution
    let mut imports: HashMap<String, String> = HashMap::new();

    for (mod_path, file_path) in &modules {
        let source = std::fs::read_to_string(file_path)
            .map_err(|e| format!("read {}: {}", file_path.display(), e))?;
        // Lightweight import collection (parse only use statements)
        collect_imports_lightweight(&source, mod_path, &dep.rust_name, &mut imports);
    }

    // 3. Run FastTokenizer on all dep source files → extract items
    let mut all_items = Vec::new();
    let mut all_edges: Vec<(String, String)> = Vec::new();

    for (mod_path, file_path) in &modules {
        let source = std::fs::read_to_string(file_path)
            .map_err(|e| format!("read {}: {}", file_path.display(), e))?;
        let file_str = file_path.to_string_lossy().to_string();

        let items = FastTokenizer::parse_file(&source, &file_str);

        // Extract call edges for function items
        let edges = scan_call_edges(&source, &items, &imports, &dep.rust_name, mod_path);
        all_edges.extend(edges);

        // Collect items with qualified paths
        for item in &items {
            let path = if mod_path == &dep.rust_name {
                format!("{}::{}", dep.rust_name, item.name)
            } else {
                // mod_path already includes the crate name prefix
                format!("{}::{}", mod_path, item.name)
            };
            all_items.push((path, item.kind.clone(), item.name.clone()));
        }
    }

    if all_items.is_empty() {
        return Ok(0);
    }

    // 4. Build internal call graph: caller_path → set of callee_paths
    let mut call_graph: HashMap<String, HashSet<String>> = HashMap::new();
    for (caller, callee) in &all_edges {
        call_graph.entry(caller.clone())
            .or_default()
            .insert(callee.clone());
    }

    // Build item name → qualified paths index for wildcard resolution
    let mut name_to_paths: HashMap<&str, Vec<&str>> = HashMap::new();
    for (path, _kind, name) in &all_items {
        name_to_paths.entry(name.as_str())
            .or_default()
            .push(path.as_str());
    }

    // 5. Determine BFS seeds from workspace references
    let all_item_paths: HashSet<&str> = all_items.iter().map(|(p, _, _)| p.as_str()).collect();
    let mut seeds: HashSet<String> = HashSet::new();

    // a. Direct workspace references (e.g., "Encoding::for_label" from `use encoding_rs::Encoding::for_label`)
    for ref_path in workspace_refs {
        // ref_path is without dep prefix (e.g., "Encoding::for_label")
        // Try matching as dep_name::ref_path
        let full_path = format!("{}::{}", dep.rust_name, ref_path);
        if all_item_paths.contains(full_path.as_str()) {
            seeds.insert(full_path);
        }

        // Also try matching just the last segment as a type/function name
        // (handles `use encoding_rs::Encoding;` followed by `Encoding::method()`)
        let last_segment = ref_path.rsplit("::").next().unwrap_or(ref_path);
        if let Some(paths) = name_to_paths.get(last_segment) {
            for path in paths {
                seeds.insert(path.to_string());
            }
        }

        // Try glob imports: `use encoding_rs::*` → all public items
        if ref_path == "*" || ref_path.ends_with("::*") {
            // Mark all items under the referenced module as seeds
            let prefix = if ref_path == "*" {
                format!("{}::", dep.rust_name)
            } else {
                format!("{}::{}",  dep.rust_name, &ref_path[..ref_path.len() - 2])
            };
            for (path, _, _) in &all_items {
                if path.starts_with(&prefix) {
                    seeds.insert(path.clone());
                }
            }
        }
    }

    // b. Wildcard methods from workspace (e.g., `*::decode` from `obj.decode()`)
    for method in wildcard_methods {
        if let Some(paths) = name_to_paths.get(method.as_str()) {
            for path in paths {
                seeds.insert(path.to_string());
            }
        }
    }

    // c. Always include: Drop impls, #[no_mangle], statics
    //    (These are detected by name heuristic — not perfect but conservative)
    for (path, kind, name) in &all_items {
        if name == "drop" && path.contains("::drop") {
            seeds.insert(path.clone());
        }
        if matches!(kind, ParsedItemKind::Static) {
            seeds.insert(path.clone());
        }
    }

    // 6. BFS from seeds through internal call graph
    let mut marked: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    for seed in &seeds {
        if marked.insert(seed.clone()) {
            queue.push_back(seed.clone());
        }
    }

    while let Some(item) = queue.pop_front() {
        if let Some(callees) = call_graph.get(&item) {
            for callee in callees {
                // Handle wildcard edges from scan_call_edges
                if callee.starts_with("*::") {
                    let method = &callee[3..];
                    if let Some(paths) = name_to_paths.get(method) {
                        for path in paths {
                            if marked.insert(path.to_string()) {
                                queue.push_back(path.to_string());
                            }
                        }
                    }
                } else if all_item_paths.contains(callee.as_str()) {
                    if marked.insert(callee.clone()) {
                        queue.push_back(callee.clone());
                    }
                }
            }
        }
    }

    // 7. Check if pruning helps: if all function items are marked, skip
    let total_fn_items = all_items.iter()
        .filter(|(_, kind, _)| matches!(kind, ParsedItemKind::Function))
        .count();
    let marked_fn_items = all_items.iter()
        .filter(|(path, kind, _)| matches!(kind, ParsedItemKind::Function) && marked.contains(path))
        .count();

    if marked_fn_items >= total_fn_items {
        // All functions reachable — no benefit from dep pruning
        return Ok(0);
    }

    // 8. Write .dep-seeds file
    // Format: version on first line, then reachable item paths (WITHOUT dep prefix)
    let seeds_path = cache_dir.join(format!("{}-lib.dep-seeds", dep.rust_name));
    let mut lines = Vec::new();
    lines.push(dep.version.clone()); // version as cache key

    let prefix = format!("{}::", dep.rust_name);
    let mut sorted_marked: Vec<&str> = marked.iter()
        .map(|s| s.as_str())
        .collect();
    sorted_marked.sort();

    for path in sorted_marked {
        // Strip dep prefix for driver compatibility (def_path_str omits crate prefix)
        if let Some(stripped) = path.strip_prefix(&prefix) {
            lines.push(stripped.to_string());
        } else {
            lines.push(path.to_string());
        }
    }

    std::fs::write(&seeds_path, lines.join("\n"))
        .map_err(|e| format!("write dep-seeds: {}", e))?;

    let prunable = total_fn_items - marked_fn_items;
    eprintln!("[dep-analyze]   {} → {}/{} functions reachable ({} prunable)",
        dep.rust_name, marked_fn_items, total_fn_items, prunable);

    Ok(marked.len())
}

/// Lightweight import collection from source text.
///
/// Extracts `use crate_name::path::item;` statements and builds an
/// import resolution table. Only resolves internal (same-crate) imports.
fn collect_imports_lightweight(
    source: &str,
    mod_path: &str,
    crate_name: &str,
    imports: &mut HashMap<String, String>,
) {
    use regex::Regex;
    use once_cell::sync::Lazy;

    static USE_RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"use\s+((?:crate|self|super)::[\w::]+(?:\s+as\s+\w+)?)\s*;").unwrap()
    });

    for cap in USE_RE.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let use_path = m.as_str();

            // Handle `as` alias
            let (path_part, alias) = if let Some(idx) = use_path.find(" as ") {
                (&use_path[..idx], &use_path[idx + 4..])
            } else {
                (use_path, use_path.rsplit("::").next().unwrap_or(use_path))
            };

            // Resolve crate:: prefix
            let resolved = if let Some(rest) = path_part.strip_prefix("crate::") {
                format!("{}::{}", crate_name, rest)
            } else if let Some(rest) = path_part.strip_prefix("self::") {
                format!("{}::{}", mod_path, rest)
            } else {
                continue; // super:: and others — skip for now
            };

            // Store: short name → fully qualified path
            imports.insert(alias.to_string(), resolved.clone());
            // Also store with module prefix
            imports.insert(format!("{}::{}", mod_path, alias), resolved);
        }
    }
}

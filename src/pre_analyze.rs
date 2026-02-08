//! Pre-build static analysis for cross-crate virtual slicing.
//!
//! Performs lightweight syn-based analysis of all workspace crates BEFORE
//! `cargo build`, extracting defined items, call edges, trait impls, statics,
//! and #[no_mangle] functions. Writes per-crate `.analysis` files that
//! `cross_crate_bfs::run_cross_crate_bfs()` uses to produce `.seeds` files.
//!
//! This replaces the expensive `cargo check` + driver analysis phase,
//! reducing overhead from ~25% to ~5-10 seconds.

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use syn::visit::Visit;

/// Trait for pre-analysis parser backends.
/// Each backend must produce a PreAnalysis struct from a workspace crate.
pub trait PreAnalysisBackend {
    fn name(&self) -> &'static str;
    fn analyze_crate(
        &self,
        krate: &WorkspaceCrate,
        local_crate_names: &HashSet<String>,
    ) -> Result<PreAnalysis, String>;
}

/// A workspace crate discovered via `cargo metadata`.
#[derive(Debug)]
pub struct WorkspaceCrate {
    /// Package name from Cargo.toml (may have hyphens)
    package_name: String,
    /// Normalized name (underscores), matches def_path_str()
    rust_name: String,
    /// Crate root source path (src/lib.rs or src/main.rs)
    source_path: PathBuf,
    /// Directory containing Cargo.toml
    manifest_dir: PathBuf,
    /// Whether this is a binary target
    is_binary: bool,
    /// "lib" or "bin"
    crate_type: String,
    /// Local workspace dependency names (rust-normalized)
    local_deps: Vec<String>,
}

/// Pre-analysis result for a single crate.
#[derive(Debug)]
pub struct PreAnalysis {
    pub crate_name: String,
    pub crate_type: String,
    pub source_hash: String,
    pub is_binary: bool,
    /// (path, is_pub, is_function)
    pub defined_items: Vec<(String, bool, bool)>,
    /// (caller, callee)
    pub call_edges: Vec<(String, String)>,
    /// (trait_path, is_drop, is_object_safe, impl_method_path)
    pub trait_impls: Vec<(String, bool, bool, String)>,
    pub statics: Vec<String>,
    pub no_mangle: Vec<String>,
}

/// Select a pre-analysis backend by name.
///
/// Valid names: "syn" (default), "fast", "ctags"
pub fn select_backend(name: &str) -> Box<dyn PreAnalysisBackend> {
    match name {
        "fast" => Box::new(FastTokenizerBackend),
        "ctags" => Box::new(CtagsBackend),
        _ => Box::new(SynBackend),
    }
}

/// Entry point: run pre-analysis on the workspace.
///
/// `backend_name` selects the parser: "syn" (default), "fast", "ctags".
/// Also respects `CARGO_SLICER_PARSER` env var.
pub fn run_pre_analysis(workspace_root: &Path) -> Result<(), String> {
    let backend_name = std::env::var("CARGO_SLICER_PARSER")
        .unwrap_or_else(|_| "syn".to_string());
    run_pre_analysis_with_backend(workspace_root, &backend_name)
}

/// Entry point with explicit backend selection.
pub fn run_pre_analysis_with_backend(workspace_root: &Path, backend_name: &str) -> Result<(), String> {
    let backend = select_backend(backend_name);
    let start = std::time::Instant::now();

    eprintln!("[pre-analyze] Using {} backend", backend.name());

    // 1. Discover workspace crates
    let crates = discover_workspace_crates(workspace_root)?;
    eprintln!("[pre-analyze] Found {} workspace crate targets", crates.len());

    if crates.is_empty() {
        return Err("No workspace crates found".to_string());
    }

    // Build set of known local crate names for cross-crate import resolution
    let local_crate_names: HashSet<String> = crates.iter()
        .map(|c| c.rust_name.clone())
        .collect();

    // Determine cache dir
    let cache_dir = std::env::var("CARGO_SLICER_CACHE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| workspace_root.join(".slicer-cache"));
    std::fs::create_dir_all(&cache_dir)
        .map_err(|e| format!("Failed to create cache dir: {}", e))?;

    // 2. Analyze each crate
    let mut analyses_written = 0;
    for krate in &crates {
        match backend.analyze_crate(krate, &local_crate_names) {
            Ok(analysis) => {
                write_analysis_file(&cache_dir, &analysis)?;
                analyses_written += 1;
            }
            Err(e) => {
                eprintln!("[pre-analyze] Warning: skipping {} ({}): {}",
                    krate.package_name, krate.crate_type, e);
            }
        }
    }

    eprintln!("[pre-analyze] Wrote {} .analysis files in {:.1}s",
        analyses_written, start.elapsed().as_secs_f64());

    // 3. Run cross-crate BFS to produce .seeds files
    crate::cross_crate_bfs::run_cross_crate_bfs(&cache_dir)?;

    eprintln!("[pre-analyze] Total pre-analysis time: {:.1}s", start.elapsed().as_secs_f64());

    Ok(())
}

// ===== Backend Implementations =====

/// Syn-based backend (default). Full AST parsing with accurate call edges.
pub struct SynBackend;

impl PreAnalysisBackend for SynBackend {
    fn name(&self) -> &'static str { "syn" }

    fn analyze_crate(
        &self,
        krate: &WorkspaceCrate,
        local_crate_names: &HashSet<String>,
    ) -> Result<PreAnalysis, String> {
        syn_analyze_crate(krate, local_crate_names)
    }
}

/// Fast tokenizer backend. Byte-level item scanning + lightweight call edge detection.
/// 5-10x faster than syn, but with lower accuracy for call edges.
pub struct FastTokenizerBackend;

impl PreAnalysisBackend for FastTokenizerBackend {
    fn name(&self) -> &'static str { "fast" }

    fn analyze_crate(
        &self,
        krate: &WorkspaceCrate,
        local_crate_names: &HashSet<String>,
    ) -> Result<PreAnalysis, String> {
        use crate::slicer::fast_tokenizer::{FastTokenizer, scan_call_edges};

        // Build module tree (same as syn backend)
        let modules = build_module_tree(&krate.source_path, &krate.rust_name)?;
        let source_hash = compute_source_hash(&krate.source_path)?;

        // Two passes: first collect imports, then extract items + call edges
        let mut global_imports: HashMap<String, String> = HashMap::new();

        // Pass 1: Collect imports via lightweight source scanning
        for (mod_path, file_path) in &modules {
            let source = std::fs::read_to_string(file_path)
                .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;
            collect_use_items_from_source(&source, mod_path, &krate.rust_name, local_crate_names, &mut global_imports);
        }

        let mut defined_items: Vec<(String, bool, bool)> = Vec::new();
        let mut call_edges: Vec<(String, String)> = Vec::new();
        let mut statics: Vec<String> = Vec::new();
        let no_mangle: Vec<String> = Vec::new();

        // Pass 2: Extract items and call edges using fast tokenizer
        for (mod_path, file_path) in &modules {
            let source = std::fs::read_to_string(file_path)
                .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;
            let file_str = file_path.to_string_lossy().to_string();

            let items = FastTokenizer::parse_file(&source, &file_str);

            // Map fast tokenizer items to PreAnalysis format
            for item in &items {
                let path = format!("{}::{}", mod_path, item.name);
                let is_pub = item.visibility == crate::types::ItemVisibility::Public;
                let is_fn = item.kind == crate::types::ParsedItemKind::Function;

                defined_items.push((path.clone(), is_pub, is_fn));

                if item.kind == crate::types::ParsedItemKind::Static {
                    statics.push(path);
                }
            }

            // Scan call edges from function bodies
            let edges = scan_call_edges(&source, &items, &global_imports, &krate.rust_name, mod_path);
            call_edges.extend(edges);
        }

        // Note: fast tokenizer doesn't detect trait impls or #[no_mangle]
        // accurately — those are left empty (conservative: cross_crate_bfs
        // will treat all trait impls as potentially reachable).
        Ok(PreAnalysis {
            crate_name: krate.rust_name.clone(),
            crate_type: krate.crate_type.clone(),
            source_hash,
            is_binary: krate.is_binary,
            defined_items,
            call_edges,
            trait_impls: Vec::new(),
            statics,
            no_mangle,
        })
    }
}

/// Ctags-based backend. Uses external ctags binary for item extraction.
/// No call edges — cross-crate BFS treats all pub items as reachable.
pub struct CtagsBackend;

impl PreAnalysisBackend for CtagsBackend {
    fn name(&self) -> &'static str { "ctags" }

    fn analyze_crate(
        &self,
        krate: &WorkspaceCrate,
        _local_crate_names: &HashSet<String>,
    ) -> Result<PreAnalysis, String> {
        let modules = build_module_tree(&krate.source_path, &krate.rust_name)?;
        let source_hash = compute_source_hash(&krate.source_path)?;

        let mut defined_items: Vec<(String, bool, bool)> = Vec::new();
        let mut statics: Vec<String> = Vec::new();

        for (mod_path, file_path) in &modules {
            let file_str = file_path.to_string_lossy().to_string();
            let tags = crate::ctags_collector::parse_rust_file(&file_str)
                .map_err(|e| format!("ctags failed for {}: {}", file_str, e))?;

            for tag in &tags {
                let path = if let Some(ref scope) = tag.scope {
                    // Try to extract the scope type name for qualified paths
                    let scope_name = scope.split(':').last().unwrap_or(scope);
                    format!("{}::{}::{}", mod_path, scope_name, tag.name)
                } else {
                    format!("{}::{}", mod_path, tag.name)
                };

                // ctags doesn't reliably report visibility, assume pub for
                // functions without scope (top-level) and priv for scoped items
                let is_pub = tag.scope.is_none();

                // ctags kind: "function" or "method" → is_function
                let is_fn = tag.kind_name == "function" || tag.kind_name == "method";

                defined_items.push((path.clone(), is_pub, is_fn));

                if tag.kind_name == "constant" && tag.name.chars().all(|c| c.is_uppercase() || c == '_') {
                    statics.push(path);
                }
            }
        }

        // No call edges, no trait impl info — strictly conservative
        Ok(PreAnalysis {
            crate_name: krate.rust_name.clone(),
            crate_type: krate.crate_type.clone(),
            source_hash,
            is_binary: krate.is_binary,
            defined_items,
            call_edges: Vec::new(),
            trait_impls: Vec::new(),
            statics,
            no_mangle: Vec::new(),
        })
    }
}

/// Collect `use` statements from raw source text (for backends that don't use syn).
/// This is a lightweight regex-free scanner that finds `use foo::bar::baz;` patterns.
fn collect_use_items_from_source(
    source: &str,
    mod_path: &str,
    crate_name: &str,
    local_crate_names: &HashSet<String>,
    imports: &mut HashMap<String, String>,
) {
    // Parse with syn just for use items — this is fast since we skip function bodies
    if let Ok(parsed) = syn::parse_file(source) {
        collect_use_items(&parsed, mod_path, crate_name, local_crate_names, imports);
    }
}

/// Discover workspace crates via `cargo metadata --no-deps`.
fn discover_workspace_crates(workspace_root: &Path) -> Result<Vec<WorkspaceCrate>, String> {
    let output = std::process::Command::new("cargo")
        .args(["metadata", "--no-deps", "--format-version=1"])
        .current_dir(workspace_root)
        .output()
        .map_err(|e| format!("Failed to run cargo metadata: {}", e))?;

    if !output.status.success() {
        return Err(format!("cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr)));
    }

    let json: serde_json::Value = serde_json::from_slice(&output.stdout)
        .map_err(|e| format!("Failed to parse cargo metadata JSON: {}", e))?;

    let workspace_members: HashSet<String> = json["workspace_members"]
        .as_array()
        .unwrap_or(&vec![])
        .iter()
        .filter_map(|v| v.as_str().map(String::from))
        .collect();

    let packages = json["packages"].as_array()
        .ok_or("No packages in cargo metadata")?;

    // Build a set of workspace package names for local dep detection
    let workspace_pkg_names: HashSet<String> = packages.iter()
        .filter(|pkg| {
            let id = pkg["id"].as_str().unwrap_or("");
            workspace_members.contains(id)
        })
        .filter_map(|pkg| pkg["name"].as_str().map(String::from))
        .collect();

    let mut result = Vec::new();

    for pkg in packages {
        let pkg_id = pkg["id"].as_str().unwrap_or("");
        if !workspace_members.contains(pkg_id) {
            continue;
        }

        let package_name = pkg["name"].as_str().unwrap_or("").to_string();
        let manifest_path = pkg["manifest_path"].as_str().unwrap_or("");
        let manifest_dir = Path::new(manifest_path)
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();

        // Collect local workspace dependencies
        let local_deps: Vec<String> = pkg["dependencies"]
            .as_array()
            .unwrap_or(&vec![])
            .iter()
            .filter(|dep| {
                // A dependency is local if its name is in our workspace
                let dep_name = dep["name"].as_str().unwrap_or("");
                workspace_pkg_names.contains(dep_name)
            })
            .filter_map(|dep| dep["name"].as_str())
            .map(|name| name.replace('-', "_"))
            .collect();

        let targets = pkg["targets"].as_array()
            .ok_or_else(|| format!("No targets for {}", package_name))?;

        for target in targets {
            let empty_vec = vec![];
            let kinds = target["kind"].as_array().unwrap_or(&empty_vec);
            let kind_strs: Vec<&str> = kinds.iter()
                .filter_map(|k| k.as_str())
                .collect();

            // Skip proc-macro, build script, example, test, bench targets
            if kind_strs.iter().any(|k| *k == "proc-macro" || *k == "custom-build"
                || *k == "example" || *k == "test" || *k == "bench") {
                continue;
            }

            let is_binary = kind_strs.contains(&"bin");
            let is_lib = kind_strs.iter().any(|k| *k == "lib" || *k == "rlib" || *k == "dylib" || *k == "staticlib");

            if !is_binary && !is_lib {
                continue;
            }

            let src_path = target["src_path"].as_str().unwrap_or("").to_string();
            if src_path.is_empty() {
                continue;
            }

            let crate_type = if is_binary { "bin" } else { "lib" };
            let rust_name = package_name.replace('-', "_");

            result.push(WorkspaceCrate {
                package_name: package_name.clone(),
                rust_name,
                source_path: PathBuf::from(&src_path),
                manifest_dir: manifest_dir.clone(),
                is_binary,
                crate_type: crate_type.to_string(),
                local_deps: local_deps.clone(),
            });
        }
    }

    Ok(result)
}

/// Analyze a single crate using syn (internal implementation for SynBackend).
fn syn_analyze_crate(krate: &WorkspaceCrate, local_crate_names: &HashSet<String>) -> Result<PreAnalysis, String> {
    // Build module tree: discover all .rs files
    let modules = build_module_tree(&krate.source_path, &krate.rust_name)?;

    // Compute source hash matching driver's compute_source_hash()
    let source_hash = compute_source_hash(&krate.source_path)?;

    // Two-pass analysis:
    // Pass 1: Parse all files, collect use statements
    let mut parsed_files: Vec<(String, syn::File)> = Vec::new();
    let mut global_imports: HashMap<String, String> = HashMap::new();

    for (mod_path, file_path) in &modules {
        let source = std::fs::read_to_string(file_path)
            .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;

        let parsed = syn::parse_file(&source)
            .map_err(|e| format!("Failed to parse {}: {}", file_path.display(), e))?;

        // Collect use statements
        collect_use_items(&parsed, mod_path, &krate.rust_name, local_crate_names, &mut global_imports);

        parsed_files.push((mod_path.clone(), parsed));
    }

    // Pass 2: Visit all files with complete imports for call edge resolution
    let mut defined_items: Vec<(String, bool, bool)> = Vec::new();
    let mut call_edges: Vec<(String, String)> = Vec::new();
    let mut trait_impls: Vec<(String, bool, bool, String)> = Vec::new();
    let mut statics: Vec<String> = Vec::new();
    let mut no_mangle: Vec<String> = Vec::new();

    for (mod_path, parsed) in &parsed_files {
        let mut visitor = PreAnalysisVisitor {
            crate_name: &krate.rust_name,
            current_module: mod_path.clone(),
            current_fn: None,
            inline_mod_stack: Vec::new(),
            imports: &global_imports,
            local_crate_names,
            defined_items: &mut defined_items,
            call_edges: &mut call_edges,
            trait_impls: &mut trait_impls,
            statics: &mut statics,
            no_mangle: &mut no_mangle,
        };
        visitor.visit_file(parsed);
    }

    Ok(PreAnalysis {
        crate_name: krate.rust_name.clone(),
        crate_type: krate.crate_type.clone(),
        source_hash,
        is_binary: krate.is_binary,
        defined_items,
        call_edges,
        trait_impls,
        statics,
        no_mangle,
    })
}

/// Build the module tree by following `mod foo;` declarations recursively.
///
/// Returns `Vec<(module_path, file_path)>` where module_path is like `crate_name::mod_a::mod_b`.
fn build_module_tree(crate_root: &Path, crate_name: &str) -> Result<Vec<(String, PathBuf)>, String> {
    let mut result = Vec::new();
    let root_module = crate_name.to_string();

    if !crate_root.exists() {
        return Err(format!("Crate root does not exist: {}", crate_root.display()));
    }

    result.push((root_module.clone(), crate_root.to_path_buf()));

    // Parse root file to find mod declarations
    let source = std::fs::read_to_string(crate_root)
        .map_err(|e| format!("Failed to read {}: {}", crate_root.display(), e))?;

    let parsed = match syn::parse_file(&source) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("[pre-analyze] Warning: parse error in {}: {}", crate_root.display(), e);
            return Ok(result);
        }
    };

    let root_dir = crate_root.parent().unwrap_or(Path::new("."));
    discover_submodules(&parsed, &root_module, root_dir, &mut result);

    Ok(result)
}

/// Recursively discover submodules from `mod foo;` declarations.
fn discover_submodules(
    file: &syn::File,
    parent_module: &str,
    parent_dir: &Path,
    result: &mut Vec<(String, PathBuf)>,
) {
    for item in &file.items {
        if let syn::Item::Mod(item_mod) = item {
            let mod_name = item_mod.ident.to_string();
            let mod_path = format!("{}::{}", parent_module, mod_name);

            if item_mod.content.is_some() {
                // Inline module — no file to discover, items will be visited
                // via inline_mod_stack in the visitor
                continue;
            }

            // Check for #[path = "..."] attribute
            let explicit_path = item_mod.attrs.iter().find_map(|attr| {
                if attr.path().is_ident("path") {
                    if let syn::Meta::NameValue(nv) = &attr.meta {
                        if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) = &nv.value {
                            return Some(s.value());
                        }
                    }
                }
                None
            });

            // Resolve file path
            let file_path = if let Some(explicit) = explicit_path {
                parent_dir.join(explicit)
            } else {
                // Try foo.rs first, then foo/mod.rs
                let direct = parent_dir.join(format!("{}.rs", mod_name));
                if direct.exists() {
                    direct
                } else {
                    let dir_mod = parent_dir.join(&mod_name).join("mod.rs");
                    dir_mod
                }
            };

            if !file_path.exists() {
                // Module might be cfg-gated; skip silently
                continue;
            }

            result.push((mod_path.clone(), file_path.clone()));

            // Determine the child directory for nested modules
            let child_dir = if file_path.file_name().map_or(false, |f| f == "mod.rs") {
                file_path.parent().unwrap_or(parent_dir).to_path_buf()
            } else {
                // foo.rs → children in foo/
                parent_dir.join(&mod_name)
            };

            // Parse and recurse
            if let Ok(source) = std::fs::read_to_string(&file_path) {
                if let Ok(parsed) = syn::parse_file(&source) {
                    discover_submodules(&parsed, &mod_path, &child_dir, result);
                }
            }
        }
    }
}

/// Collect `use` statements from a parsed file into the global imports table.
fn collect_use_items(
    file: &syn::File,
    mod_path: &str,
    crate_name: &str,
    local_crate_names: &HashSet<String>,
    imports: &mut HashMap<String, String>,
) {
    for item in &file.items {
        if let syn::Item::Use(item_use) = item {
            collect_use_paths(&item_use.tree, &mut Vec::new(), mod_path, crate_name, local_crate_names, imports);
        }
    }
}

/// Walk a `UseTree` recursively and populate the imports table.
///
/// Only tracks imports from known local crates (for cross-crate edge detection).
fn collect_use_paths(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    mod_path: &str,
    crate_name: &str,
    local_crate_names: &HashSet<String>,
    imports: &mut HashMap<String, String>,
) {
    match tree {
        syn::UseTree::Path(use_path) => {
            let seg = use_path.ident.to_string();
            prefix.push(seg);
            collect_use_paths(&use_path.tree, prefix, mod_path, crate_name, local_crate_names, imports);
            prefix.pop();
        }
        syn::UseTree::Name(use_name) => {
            let name = use_name.ident.to_string();
            prefix.push(name.clone());
            let full_path = resolve_use_prefix(prefix, crate_name, mod_path);
            // Only record if it resolves to a known local crate
            if is_local_path(&full_path, local_crate_names) {
                imports.insert(format!("{}::{}", mod_path, name), full_path.clone());
                imports.insert(name, full_path);
            }
            prefix.pop();
        }
        syn::UseTree::Rename(use_rename) => {
            let original = use_rename.ident.to_string();
            let alias = use_rename.rename.to_string();
            prefix.push(original);
            let full_path = resolve_use_prefix(prefix, crate_name, mod_path);
            if is_local_path(&full_path, local_crate_names) {
                imports.insert(format!("{}::{}", mod_path, alias), full_path.clone());
                imports.insert(alias, full_path);
            }
            prefix.pop();
        }
        syn::UseTree::Group(use_group) => {
            for subtree in &use_group.items {
                collect_use_paths(subtree, prefix, mod_path, crate_name, local_crate_names, imports);
            }
        }
        syn::UseTree::Glob(_) => {
            // Can't resolve glob imports without type info; skip (conservative)
        }
    }
}

/// Resolve a use-path prefix to a fully-qualified path.
fn resolve_use_prefix(segments: &[String], crate_name: &str, mod_path: &str) -> String {
    if segments.is_empty() {
        return String::new();
    }

    let first = &segments[0];
    let rest = &segments[1..];

    let resolved_first = if first == "crate" || first == "$crate" {
        crate_name.to_string()
    } else if first == "self" {
        mod_path.to_string()
    } else if first == "super" {
        // Go up one module level
        if let Some(pos) = mod_path.rfind("::") {
            mod_path[..pos].to_string()
        } else {
            mod_path.to_string()
        }
    } else {
        // External crate or absolute path
        first.clone()
    };

    if rest.is_empty() {
        resolved_first
    } else {
        format!("{}::{}", resolved_first, rest.join("::"))
    }
}

/// Check if a fully-qualified path belongs to a local workspace crate.
fn is_local_path(path: &str, local_crate_names: &HashSet<String>) -> bool {
    let first_segment = path.split("::").next().unwrap_or("");
    local_crate_names.contains(first_segment)
}

/// syn visitor that extracts defined items, call edges, trait impls, etc.
struct PreAnalysisVisitor<'a> {
    crate_name: &'a str,
    current_module: String,
    current_fn: Option<String>,
    inline_mod_stack: Vec<String>,
    imports: &'a HashMap<String, String>,
    local_crate_names: &'a HashSet<String>,
    defined_items: &'a mut Vec<(String, bool, bool)>,
    call_edges: &'a mut Vec<(String, String)>,
    trait_impls: &'a mut Vec<(String, bool, bool, String)>,
    statics: &'a mut Vec<String>,
    no_mangle: &'a mut Vec<String>,
}

impl<'a> PreAnalysisVisitor<'a> {
    /// Get the current effective module path (accounting for inline mods).
    fn effective_module(&self) -> String {
        if self.inline_mod_stack.is_empty() {
            self.current_module.clone()
        } else {
            format!("{}::{}", self.current_module, self.inline_mod_stack.join("::"))
        }
    }

    /// Build a fully-qualified path for an item in the current scope.
    fn item_path(&self, name: &str) -> String {
        format!("{}::{}", self.effective_module(), name)
    }

    /// Check if an item is public.
    fn is_pub(vis: &syn::Visibility) -> bool {
        matches!(vis, syn::Visibility::Public(_))
    }

    /// Check if an item has #[no_mangle].
    fn has_no_mangle(attrs: &[syn::Attribute]) -> bool {
        attrs.iter().any(|attr| attr.path().is_ident("no_mangle"))
    }

    /// Resolve a call path to a fully-qualified path.
    fn resolve_path(&self, path: &syn::Path) -> Option<String> {
        let segments: Vec<String> = path.segments.iter()
            .map(|s| s.ident.to_string())
            .collect();

        if segments.is_empty() {
            return None;
        }

        let raw_path = segments.join("::");

        // Check if it starts with a known local crate name → already qualified
        let first = &segments[0];
        if self.local_crate_names.contains(first) {
            return Some(raw_path);
        }

        // Check imports table (with module prefix first, then bare name)
        let with_mod = format!("{}::{}", self.effective_module(), first);
        if let Some(resolved) = self.imports.get(&with_mod) {
            if segments.len() == 1 {
                return Some(resolved.clone());
            } else {
                return Some(format!("{}::{}", resolved, segments[1..].join("::")));
            }
        }
        if let Some(resolved) = self.imports.get(first) {
            if segments.len() == 1 {
                return Some(resolved.clone());
            } else {
                return Some(format!("{}::{}", resolved, segments[1..].join("::")));
            }
        }

        // crate:: prefix
        if first == "crate" || first == "$crate" {
            let rest = &segments[1..];
            return Some(format!("{}::{}", self.crate_name, rest.join("::")));
        }

        // self:: prefix
        if first == "self" {
            let rest = &segments[1..];
            return Some(format!("{}::{}", self.effective_module(), rest.join("::")));
        }

        // super:: prefix
        if first == "super" {
            let mod_path = self.effective_module();
            let parent = if let Some(pos) = mod_path.rfind("::") {
                &mod_path[..pos]
            } else {
                &mod_path
            };
            let rest = &segments[1..];
            return Some(format!("{}::{}", parent, rest.join("::")));
        }

        // Single segment → local call within current module
        if segments.len() == 1 {
            return Some(self.item_path(first));
        }

        // Multi-segment but not recognized → could be Type::method or external
        // Prepend current module as best effort
        Some(format!("{}::{}", self.effective_module(), raw_path))
    }
}

impl<'a> Visit<'a> for PreAnalysisVisitor<'_> {
    fn visit_item_fn(&mut self, node: &'a syn::ItemFn) {
        let name = node.sig.ident.to_string();
        let path = self.item_path(&name);
        let is_pub = Self::is_pub(&node.vis);

        self.defined_items.push((path.clone(), is_pub, true));

        if Self::has_no_mangle(&node.attrs) {
            self.no_mangle.push(path.clone());
        }

        // Set current_fn for call edge recording
        let prev_fn = self.current_fn.take();
        self.current_fn = Some(path);

        // Visit the function body
        syn::visit::visit_item_fn(self, node);

        self.current_fn = prev_fn;
    }

    fn visit_item_struct(&mut self, node: &'a syn::ItemStruct) {
        let name = node.ident.to_string();
        let path = self.item_path(&name);
        self.defined_items.push((path, Self::is_pub(&node.vis), false));

        syn::visit::visit_item_struct(self, node);
    }

    fn visit_item_enum(&mut self, node: &'a syn::ItemEnum) {
        let name = node.ident.to_string();
        let path = self.item_path(&name);
        self.defined_items.push((path, Self::is_pub(&node.vis), false));

        syn::visit::visit_item_enum(self, node);
    }

    fn visit_item_trait(&mut self, node: &'a syn::ItemTrait) {
        let name = node.ident.to_string();
        let path = self.item_path(&name);
        self.defined_items.push((path, Self::is_pub(&node.vis), false));

        syn::visit::visit_item_trait(self, node);
    }

    fn visit_item_type(&mut self, node: &'a syn::ItemType) {
        let name = node.ident.to_string();
        let path = self.item_path(&name);
        self.defined_items.push((path, Self::is_pub(&node.vis), false));

        syn::visit::visit_item_type(self, node);
    }

    fn visit_item_const(&mut self, node: &'a syn::ItemConst) {
        let name = node.ident.to_string();
        let path = self.item_path(&name);
        self.defined_items.push((path, Self::is_pub(&node.vis), false));

        syn::visit::visit_item_const(self, node);
    }

    fn visit_item_static(&mut self, node: &'a syn::ItemStatic) {
        let name = node.ident.to_string();
        let path = self.item_path(&name);
        let is_pub = Self::is_pub(&node.vis);

        self.defined_items.push((path.clone(), is_pub, false));
        self.statics.push(path);

        syn::visit::visit_item_static(self, node);
    }

    fn visit_item_mod(&mut self, node: &'a syn::ItemMod) {
        // Only handle inline modules here; file-based modules
        // are handled by build_module_tree()
        if node.content.is_some() {
            let mod_name = node.ident.to_string();
            self.inline_mod_stack.push(mod_name);
            syn::visit::visit_item_mod(self, node);
            self.inline_mod_stack.pop();
        }
        // Non-inline modules: don't recurse (visited separately via module tree)
    }

    fn visit_item_impl(&mut self, node: &'a syn::ItemImpl) {
        // Extract self type name
        let self_type_name = extract_type_name(&node.self_ty);

        if let Some(ref trait_ref) = node.trait_ {
            // Trait impl
            let trait_name = path_to_string(&trait_ref.1);
            let is_drop = trait_name == "Drop"
                || trait_name.ends_with("::Drop")
                || trait_name == "std::ops::Drop"
                || trait_name == "core::ops::Drop";
            // Conservatively mark all trait impls as potentially object-safe
            // (we can't determine object safety from syn alone)
            let is_object_safe = !is_drop;

            for item in &node.items {
                if let syn::ImplItem::Fn(method) = item {
                    let method_name = method.sig.ident.to_string();
                    let method_path = format!("{}::{}::{}",
                        self.effective_module(), self_type_name, method_name);

                    self.defined_items.push((method_path.clone(), Self::is_pub(&method.vis), true));
                    self.trait_impls.push((trait_name.clone(), is_drop, is_object_safe, method_path.clone()));

                    // Visit method body for call edges
                    let prev_fn = self.current_fn.take();
                    self.current_fn = Some(method_path);
                    syn::visit::visit_impl_item_fn(self, method);
                    self.current_fn = prev_fn;
                }
            }
        } else {
            // Inherent impl
            for item in &node.items {
                if let syn::ImplItem::Fn(method) = item {
                    let method_name = method.sig.ident.to_string();
                    let method_path = format!("{}::{}::{}",
                        self.effective_module(), self_type_name, method_name);

                    self.defined_items.push((method_path.clone(), Self::is_pub(&method.vis), true));

                    if Self::has_no_mangle(&method.attrs) {
                        self.no_mangle.push(method_path.clone());
                    }

                    // Visit method body for call edges
                    let prev_fn = self.current_fn.take();
                    self.current_fn = Some(method_path);
                    syn::visit::visit_impl_item_fn(self, method);
                    self.current_fn = prev_fn;
                }
            }
        }

        // Don't call the default visit — we already visited methods manually
    }

    fn visit_expr_call(&mut self, node: &'a syn::ExprCall) {
        // Extract callee from function calls like `foo()`, `bar::baz()`, `Type::method()`
        if let Some(ref caller) = self.current_fn {
            if let syn::Expr::Path(expr_path) = &*node.func {
                if let Some(callee) = self.resolve_path(&expr_path.path) {
                    self.call_edges.push((caller.clone(), callee));
                }
            }
        }

        // Continue visiting sub-expressions (arguments, nested calls)
        syn::visit::visit_expr_call(self, node);
    }

    // Skip visit_expr_method_call: no type info for receiver resolution
}

/// Extract a simple type name from a syn::Type (best effort).
fn extract_type_name(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Path(type_path) => {
            path_to_string(&type_path.path)
        }
        syn::Type::Reference(type_ref) => {
            extract_type_name(&type_ref.elem)
        }
        _ => "_UnknownType".to_string(),
    }
}

/// Convert a syn::Path to a string (segments joined by "::").
fn path_to_string(path: &syn::Path) -> String {
    path.segments.iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

/// Compute source hash matching the driver's `compute_source_hash()`.
///
/// Uses DefaultHasher, hashes `path + content` for each sorted .rs file,
/// then `file_count`.
fn compute_source_hash(source_path: &Path) -> Result<String, String> {
    use std::collections::hash_map::DefaultHasher;

    let source_dir = source_path.parent()
        .ok_or_else(|| format!("No parent dir for {}", source_path.display()))?;

    let mut rs_files = Vec::new();
    collect_rs_files(source_dir, &mut rs_files);
    rs_files.sort();

    let mut hasher = DefaultHasher::new();
    let mut file_count = 0u64;

    if rs_files.is_empty() {
        // Fallback: hash just the root file
        let content = std::fs::read(source_path)
            .map_err(|e| format!("Failed to read {}: {}", source_path.display(), e))?;
        content.hash(&mut hasher);
        return Ok(format!("{:016x}", hasher.finish()));
    }

    for path in &rs_files {
        if let Ok(content) = std::fs::read(path) {
            path.hash(&mut hasher);
            content.hash(&mut hasher);
            file_count += 1;
        }
    }

    file_count.hash(&mut hasher);

    Ok(format!("{:016x}", hasher.finish()))
}

/// Recursively collect .rs files from a directory.
fn collect_rs_files(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_rs_files(&path, files);
            } else if path.extension().map_or(false, |ext| ext == "rs") {
                files.push(path);
            }
        }
    }
}

/// Write a `.analysis` file in the format expected by `cross_crate_bfs::parse_analysis_file()`.
fn write_analysis_file(cache_dir: &Path, analysis: &PreAnalysis) -> Result<(), String> {
    let filename = format!("{}-{}.analysis", analysis.crate_name, analysis.crate_type);
    let path = cache_dir.join(filename);

    let mut content = String::new();

    // Header
    content.push_str(&format!("CRATE_NAME:{}\n", analysis.crate_name));
    content.push_str(&format!("CRATE_TYPE:{}\n", analysis.crate_type));
    content.push_str(&format!("SOURCE_HASH:{}\n", analysis.source_hash));
    content.push_str(&format!("IS_BINARY:{}\n", analysis.is_binary));

    // Defined items
    content.push_str("DEFINED_ITEMS:\n");
    for (path, is_pub, is_fn) in &analysis.defined_items {
        let vis = if *is_pub { "pub" } else { "priv" };
        let kind = if *is_fn { "fn" } else { "type" };
        content.push_str(&format!("{}\t{}\t{}\n", path, vis, kind));
    }

    // Call edges
    content.push_str("CALL_EDGES:\n");
    for (caller, callee) in &analysis.call_edges {
        content.push_str(&format!("{}\t{}\n", caller, callee));
    }

    // Trait impls
    content.push_str("TRAIT_IMPLS:\n");
    for (trait_path, is_drop, is_obj_safe, method) in &analysis.trait_impls {
        content.push_str(&format!("{}\t{}\t{}\t{}\n",
            trait_path, is_drop, is_obj_safe, method));
    }

    // Statics
    content.push_str("STATICS:\n");
    for s in &analysis.statics {
        content.push_str(s);
        content.push('\n');
    }

    // No mangle
    content.push_str("NO_MANGLE:\n");
    for nm in &analysis.no_mangle {
        content.push_str(nm);
        content.push('\n');
    }

    std::fs::write(&path, content)
        .map_err(|e| format!("Failed to write {}: {}", path.display(), e))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_resolve_use_prefix_crate() {
        let segs = vec!["crate".to_string(), "foo".to_string(), "bar".to_string()];
        assert_eq!(resolve_use_prefix(&segs, "mylib", "mylib::mod_a"), "mylib::foo::bar");
    }

    #[test]
    fn test_resolve_use_prefix_self() {
        let segs = vec!["self".to_string(), "helper".to_string()];
        assert_eq!(resolve_use_prefix(&segs, "mylib", "mylib::mod_a"), "mylib::mod_a::helper");
    }

    #[test]
    fn test_resolve_use_prefix_super() {
        let segs = vec!["super".to_string(), "sibling".to_string()];
        assert_eq!(resolve_use_prefix(&segs, "mylib", "mylib::mod_a::sub"), "mylib::mod_a::sibling");
    }

    #[test]
    fn test_resolve_use_prefix_external() {
        let segs = vec!["other_crate".to_string(), "Thing".to_string()];
        assert_eq!(resolve_use_prefix(&segs, "mylib", "mylib::mod_a"), "other_crate::Thing");
    }

    #[test]
    fn test_is_local_path() {
        let mut locals = HashSet::new();
        locals.insert("mylib".to_string());
        locals.insert("myutil".to_string());

        assert!(is_local_path("mylib::foo::bar", &locals));
        assert!(is_local_path("myutil::Thing", &locals));
        assert!(!is_local_path("serde::Serialize", &locals));
        assert!(!is_local_path("std::collections::HashMap", &locals));
    }

    #[test]
    fn test_extract_type_name() {
        let ty: syn::Type = syn::parse_str("MyStruct").unwrap();
        assert_eq!(extract_type_name(&ty), "MyStruct");
    }

    #[test]
    fn test_write_and_parse_analysis() {
        let dir = std::env::temp_dir().join("pre_analyze_test");
        let _ = std::fs::create_dir_all(&dir);

        let analysis = PreAnalysis {
            crate_name: "testcrate".to_string(),
            crate_type: "lib".to_string(),
            source_hash: "abc123".to_string(),
            is_binary: false,
            defined_items: vec![
                ("testcrate::foo".to_string(), true, true),
                ("testcrate::bar".to_string(), false, true),
            ],
            call_edges: vec![
                ("testcrate::foo".to_string(), "testcrate::bar".to_string()),
            ],
            trait_impls: vec![
                ("Drop".to_string(), true, false, "testcrate::MyType::drop".to_string()),
            ],
            statics: vec!["testcrate::GLOBAL".to_string()],
            no_mangle: vec!["testcrate::exported_fn".to_string()],
        };

        write_analysis_file(&dir, &analysis).unwrap();

        // Verify the file can be parsed by cross_crate_bfs
        let analysis_path = dir.join("testcrate-lib.analysis");
        assert!(analysis_path.exists());

        // Read and verify contents manually
        let content = std::fs::read_to_string(&analysis_path).unwrap();
        assert!(content.contains("CRATE_NAME:testcrate"));
        assert!(content.contains("CRATE_TYPE:lib"));
        assert!(content.contains("SOURCE_HASH:abc123"));
        assert!(content.contains("IS_BINARY:false"));
        assert!(content.contains("testcrate::foo\tpub\tfn"));
        assert!(content.contains("testcrate::bar\tpriv\tfn"));
        assert!(content.contains("testcrate::foo\ttestcrate::bar"));
        assert!(content.contains("Drop\ttrue\tfalse\ttestcrate::MyType::drop"));
        assert!(content.contains("testcrate::GLOBAL"));
        assert!(content.contains("testcrate::exported_fn"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_analyze_simple_source() {
        let dir = std::env::temp_dir().join("pre_analyze_simple_test");
        let _ = std::fs::remove_dir_all(&dir);
        let _ = std::fs::create_dir_all(&dir);

        // Create a simple Rust source file
        let src_path = dir.join("lib.rs");
        let mut f = std::fs::File::create(&src_path).unwrap();
        writeln!(f, "pub fn hello() {{ helper(); }}").unwrap();
        writeln!(f, "fn helper() {{}}").unwrap();
        writeln!(f, "pub static COUNTER: u32 = 0;").unwrap();
        drop(f);

        let local_names: HashSet<String> = ["testcrate".to_string()].into_iter().collect();
        let krate = WorkspaceCrate {
            package_name: "testcrate".to_string(),
            rust_name: "testcrate".to_string(),
            source_path: src_path,
            manifest_dir: dir.clone(),
            is_binary: false,
            crate_type: "lib".to_string(),
            local_deps: vec![],
        };

        let analysis = syn_analyze_crate(&krate, &local_names).unwrap();

        assert_eq!(analysis.crate_name, "testcrate");
        assert!(!analysis.is_binary);

        // Should have: hello (pub), helper (priv), COUNTER (pub/static)
        let item_names: Vec<&str> = analysis.defined_items.iter()
            .map(|(p, _, _)| p.as_str())
            .collect();
        assert!(item_names.contains(&"testcrate::hello"), "missing hello: {:?}", item_names);
        assert!(item_names.contains(&"testcrate::helper"), "missing helper: {:?}", item_names);
        assert!(item_names.contains(&"testcrate::COUNTER"), "missing COUNTER: {:?}", item_names);

        // hello → helper call edge
        assert!(analysis.call_edges.iter().any(|(c, e)| c == "testcrate::hello" && e == "testcrate::helper"),
            "missing hello→helper edge: {:?}", analysis.call_edges);

        // COUNTER should be in statics
        assert!(analysis.statics.contains(&"testcrate::COUNTER".to_string()));

        let _ = std::fs::remove_dir_all(&dir);
    }
}

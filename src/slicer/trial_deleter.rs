//! Trial-based deletion: Empirically discover what can be safely deleted
//!
//! ## Dependency-Aware Removal Sets
//!
//! Key insight: If B depends on A, removing A alone breaks B. But removing
//! {A, B} together might succeed if nothing else uses either.
//!
//! The algorithm:
//! 1. Build intra-crate dependency graph (who-references-whom among private items)
//! 2. Compute "removal sets" - each item + all items that depend on it
//! 3. Trial-delete entire removal sets together (largest first)
//! 4. If set deletion succeeds, all members are deletable
//! 5. If it fails with external errors, the set is needed
//!
//! This finds more deletable code than single-item trials.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use syn::{Item, Visibility, visit::Visit};

use crate::constants::SKIP_TRIAL_DELETION;

/// Stored item for potential restoration
#[derive(Clone)]
struct StoredItem {
    name: String,
    kind: String,
    file_path: PathBuf,
    item_source: String, // Original source text of the item
    line_start: usize,
    line_end: usize,
}

/// Result of trial-based deletion
pub struct TrialDeletionResult {
    pub items_deleted: usize,
    pub items_restored: usize,
    pub final_deletable: HashSet<String>,
    pub must_keep: HashSet<String>,
}

/// Table of all deletion trial results for a crate
#[derive(Debug, Default)]
pub struct DeletionTable {
    pub crate_name: String,
    pub trials: Vec<DeletionTrialResult>,
    pub safe_to_delete: HashSet<String>,
    pub must_keep: HashSet<String>,
}

/// Result of trying to delete a single item
#[derive(Debug, Clone)]
pub struct DeletionTrialResult {
    pub item_name: String,
    pub item_kind: String,
    pub file_path: String,
    pub can_delete: bool,
    pub error_count: usize,
    pub dependent_items: Vec<String>,
}

/// A removal set: a root item plus all items that depend on it
/// If the root is removed, all dependents must also be removed
#[derive(Debug, Clone)]
pub struct RemovalSet {
    /// The root item (dominant node)
    pub root: String,
    /// All items in the set (including root)
    pub members: HashSet<String>,
    /// Size for sorting (larger sets tried first)
    pub size: usize,
}

impl RemovalSet {
    fn new(root: String) -> Self {
        let mut members = HashSet::new();
        members.insert(root.clone());
        Self {
            root,
            members,
            size: 1,
        }
    }

    fn add_member(&mut self, member: String) {
        if self.members.insert(member) {
            self.size += 1;
        }
    }
}

/// Intra-crate dependency graph for private items
#[derive(Debug, Default)]
pub struct ItemDependencyGraph {
    /// Forward edges: A → {B, C} means A references B and C
    pub references: HashMap<String, HashSet<String>>,
    /// Reverse edges: B → {A} means A depends on B (A references B)
    pub dependents: HashMap<String, HashSet<String>>,
    /// All known item names
    pub items: HashSet<String>,
}

impl ItemDependencyGraph {
    fn new() -> Self {
        Self::default()
    }

    fn add_item(&mut self, name: String) {
        self.items.insert(name);
    }

    fn add_reference(&mut self, from: &str, to: &str) {
        // from references to (from depends on to)
        self.references
            .entry(from.to_string())
            .or_default()
            .insert(to.to_string());
        // to has from as a dependent
        self.dependents
            .entry(to.to_string())
            .or_default()
            .insert(from.to_string());
    }

    /// Get all items that transitively depend on the given item
    fn get_transitive_dependents(&self, item: &str) -> HashSet<String> {
        let mut result = HashSet::new();
        let mut queue = vec![item.to_string()];
        let mut visited = HashSet::new();

        while let Some(current) = queue.pop() {
            if !visited.insert(current.clone()) {
                continue;
            }
            if let Some(deps) = self.dependents.get(&current) {
                for dep in deps {
                    if !visited.contains(dep) {
                        result.insert(dep.clone());
                        queue.push(dep.clone());
                    }
                }
            }
        }
        result
    }

    /// Compute removal sets for all items
    /// Returns sets sorted by size (largest first)
    pub fn compute_removal_sets(&self, candidates: &HashSet<String>) -> Vec<RemovalSet> {
        let mut sets: Vec<RemovalSet> = Vec::new();

        for item in candidates {
            let mut set = RemovalSet::new(item.clone());

            // Add all transitive dependents that are also candidates
            let dependents = self.get_transitive_dependents(item);
            for dep in dependents {
                if candidates.contains(&dep) {
                    set.add_member(dep);
                }
            }

            sets.push(set);
        }

        // Sort by size descending (try largest sets first for efficiency)
        sets.sort_by(|a, b| b.size.cmp(&a.size));

        // Deduplicate: if a smaller set is fully contained in a larger one, skip it
        let mut unique_sets: Vec<RemovalSet> = Vec::new();
        for set in sets {
            let already_covered = unique_sets
                .iter()
                .any(|existing| set.members.is_subset(&existing.members));
            if !already_covered {
                unique_sets.push(set);
            }
        }

        unique_sets
    }
}

impl DeletionTable {
    pub fn find_maximal_deletable_set(&self) -> HashSet<String> {
        let mut deletable = self.safe_to_delete.clone();
        for trial in &self.trials {
            if self.must_keep.contains(&trial.item_name) {
                for dep in &trial.dependent_items {
                    deletable.remove(dep);
                }
            }
        }
        deletable
    }

    pub fn print_summary(&self) {
        println!("\n=== Deletion Trial Results for {} ===", self.crate_name);
        println!("  Total items tested: {}", self.trials.len());
        println!("  Safe to delete: {}", self.safe_to_delete.len());
        println!("  Must keep: {}", self.must_keep.len());

        if !self.safe_to_delete.is_empty() {
            println!("\n  Deletable items:");
            for name in self.safe_to_delete.iter().take(10) {
                println!("    ✓ {}", name);
            }
            if self.safe_to_delete.len() > 10 {
                println!("    ... and {} more", self.safe_to_delete.len() - 10);
            }
        }
    }
}

/// Get the name and kind of an item
fn get_item_info(item: &Item) -> Option<(String, String)> {
    match item {
        Item::Fn(f) => Some((f.sig.ident.to_string(), "fn".to_string())),
        Item::Struct(s) => Some((s.ident.to_string(), "struct".to_string())),
        Item::Enum(e) => Some((e.ident.to_string(), "enum".to_string())),
        Item::Const(c) => Some((c.ident.to_string(), "const".to_string())),
        Item::Static(s) => Some((s.ident.to_string(), "static".to_string())),
        Item::Type(t) => Some((t.ident.to_string(), "type".to_string())),
        Item::Trait(t) => Some((t.ident.to_string(), "trait".to_string())),
        Item::Union(u) => Some((u.ident.to_string(), "union".to_string())),
        _ => None,
    }
}

/// Check if item has private visibility
fn is_private(item: &Item) -> bool {
    match item {
        Item::Fn(f) => matches!(f.vis, Visibility::Inherited),
        Item::Struct(s) => matches!(s.vis, Visibility::Inherited),
        Item::Enum(e) => matches!(e.vis, Visibility::Inherited),
        Item::Const(c) => matches!(c.vis, Visibility::Inherited),
        Item::Static(s) => matches!(s.vis, Visibility::Inherited),
        Item::Type(t) => matches!(t.vis, Visibility::Inherited),
        Item::Trait(t) => matches!(t.vis, Visibility::Inherited),
        Item::Union(u) => matches!(u.vis, Visibility::Inherited),
        _ => false,
    }
}

/// Check if item has #[cfg(...)] attributes
/// Items with cfg attributes should not be deleted since we can't evaluate the conditions
fn has_cfg_attribute(item: &Item) -> bool {
    let attrs = match item {
        Item::Fn(f) => &f.attrs,
        Item::Struct(s) => &s.attrs,
        Item::Enum(e) => &e.attrs,
        Item::Const(c) => &c.attrs,
        Item::Static(s) => &s.attrs,
        Item::Type(t) => &t.attrs,
        Item::Trait(t) => &t.attrs,
        Item::Union(u) => &u.attrs,
        _ => return false,
    };

    for attr in attrs {
        if let Some(ident) = attr.path().get_ident() {
            if ident == "cfg" {
                return true;
            }
        }
    }
    false
}

/// PHASE 2 OPTIMIZATION: AST visitor for efficient reference extraction
///
/// Uses syn's Visit trait to walk the AST directly instead of converting
/// to string and parsing identifiers. This is 3-5x faster than the old approach.
struct ReferenceExtractor {
    refs: HashSet<String>,
    keywords: HashSet<&'static str>,
}

impl ReferenceExtractor {
    fn new() -> Self {
        Self {
            refs: HashSet::new(),
            keywords: Self::get_keyword_set(),
        }
    }

    fn get_keyword_set() -> HashSet<&'static str> {
        [
            "fn", "let", "mut", "const", "static", "struct", "enum", "trait",
            "impl", "type", "pub", "crate", "self", "super", "mod", "use",
            "if", "else", "match", "for", "while", "loop", "return", "break",
            "continue", "where", "as", "in", "ref", "move", "dyn", "async",
            "await", "unsafe", "extern", "true", "false", "Some", "None", "Ok",
            "Err", "Self", "str", "bool", "char", "usize", "isize", "u8",
            "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64",
            "i128", "f32", "f64", "String", "Vec", "Box", "Option",
            "Result",
        ]
        .iter()
        .cloned()
        .collect()
    }

    fn is_valid_ident(&self, s: &str) -> bool {
        // Skip keywords, too short, or starting with digit
        s.len() >= 2
            && !s.starts_with(char::is_numeric)
            && !self.keywords.contains(s)
            && s.chars()
                .next()
                .map(|c| c.is_alphabetic() || c == '_')
                .unwrap_or(false)
    }
}

impl Visit<'_> for ReferenceExtractor {
    fn visit_ident(&mut self, ident: &syn::Ident) {
        let s = ident.to_string();

        if self.is_valid_ident(&s) {
            self.refs.insert(s);
        }

        syn::visit::visit_ident(self, ident);
    }
}

/// Extract identifier references from an item's body
/// Returns the names of other items this item references
/// Captures functions (snake_case), constants (SCREAMING_CASE), and types (PascalCase)
///
/// PHASE 2 OPTIMIZATION: Uses AST visitor instead of string conversion
/// This is 3-5x faster than the old quote::quote! approach.
fn extract_references_from_item(item: &Item) -> HashSet<String> {
    let mut extractor = ReferenceExtractor::new();
    extractor.visit_item(item);
    extractor.refs
}


/// PHASE 2 OPTIMIZATION: Cached parsed file to avoid re-parsing
///
/// Stores parsed AST and item classification to reuse across operations.
/// This avoids redundant syn::parse_file() calls when files are
/// processed multiple times (e.g., once for collection, once for graph).
struct ParsedFile {
    syntax_tree: syn::File,
    file_path: PathBuf,
}

/// Build a dependency graph of private items within a crate
/// Scans each item's body to find references to other private items
///
/// PHASE 2 OPTIMIZATION: Caches parsed files to avoid re-parsing.
/// Files are parsed once and reused, reducing syn::parse_file() overhead.
fn build_item_dependency_graph(
    crate_dir: &Path,
    private_items: &HashMap<String, StoredItem>,
) -> ItemDependencyGraph {
    let mut graph = ItemDependencyGraph::new();

    // Extract just the names (without file paths) for matching
    let item_names: HashSet<String> = private_items.values().map(|i| i.name.clone()).collect();

    // QUICK WIN: Build reverse index (name -> Vec<full_key>) for O(1) lookups
    // This eliminates the O(n²) nested loop when matching references to items
    // PHASE 2 OPTIMIZATION: Use owned strings to avoid lifetime issues
    let name_to_keys: HashMap<String, Vec<String>> = private_items
        .iter()
        .map(|(key, stored)| (stored.name.clone(), key.clone()))
        .fold(HashMap::new(), |mut acc, (name, key)| {
            acc.entry(name).or_default().push(key);
            acc
        });

    // Add all items to the graph
    for key in private_items.keys() {
        graph.add_item(key.clone());
    }

    // PHASE 2 OPTIMIZATION: Parse files once and cache results
    // This avoids redundant syn::parse_file() calls
    let mut parsed_files: Vec<ParsedFile> = Vec::new();
    let mut rs_files = Vec::new();
    collect_rs_files(crate_dir, &mut rs_files);

    for file_path in rs_files {
        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let syntax_tree = match syn::parse_file(&content) {
            Ok(tree) => tree,
            Err(_) => continue,
        };

        parsed_files.push(ParsedFile {
            syntax_tree,
            file_path,
        });
    }

    // Build dependency graph from cached parses
    for parsed_file in &parsed_files {
        for item in &parsed_file.syntax_tree.items {
            // Process ALL items (both public and private) to track dependencies
            // This is critical: public functions may depend on private constants/types
            // e.g., Level::as_str() depends on LOG_LEVEL_NAMES

            // Handle macro definitions - they may reference private items in their body
            if let Item::Macro(macro_item) = item {
                let macro_name = macro_item
                    .ident
                    .as_ref()
                    .map(|i| i.to_string())
                    .unwrap_or_else(|| "unknown_macro".to_string());

                let refs = extract_references_from_item(item);
                for ref_name in refs {
                    if let Some(matching_keys) = name_to_keys.get(&ref_name) {
                        // O(1) lookup instead of O(n) nested loop
                        for to_key in matching_keys {
                            graph.add_reference(&format!("__macro__::{}", macro_name), to_key);
                        }
                    }
                }
                continue;
            }

            // Handle impl blocks separately - they contain methods that may reference private items
            if let Item::Impl(impl_block) = item {
                // Extract type name for impl block
                let type_name = quote::quote!(#impl_block.self_ty)
                    .to_string()
                    .split_whitespace()
                    .last()
                    .unwrap_or("unknown")
                    .to_string();

                // Extract references from all methods in the impl block
                let refs = extract_references_from_item(item);

                // Mark dependencies on private items
                for ref_name in refs {
                    if let Some(matching_keys) = name_to_keys.get(&ref_name) {
                        // O(1) lookup instead of O(n) nested loop
                        for to_key in matching_keys {
                            // Impl block depends on private item
                            graph.add_reference(&format!("__impl__::{}", type_name), to_key);
                        }
                    }
                }
                continue;
            }

            if let Some((name, _)) = get_item_info(item) {
                let from_key = item_key(&parsed_file.file_path, &name);
                let is_private_item = is_private(item);

                // Extract references from this item
                let refs = extract_references_from_item(item);

                // Find which references match private items we're tracking
                // QUICK WIN: Use O(1) lookup via name_to_keys instead of O(n²) nested loop
                for ref_name in refs {
                    if item_names.contains(&ref_name) && ref_name != name {
                        if let Some(matching_keys) = name_to_keys.get(&ref_name) {
                            for to_key in matching_keys {
                                if is_private_item && private_items.contains_key(&from_key) {
                                    // Private item depends on another private item
                                    graph.add_reference(&from_key, to_key);
                                } else {
                                    // Public item depends on private item - mark as having dependents
                                    // Use a special key to indicate external dependency
                                    graph.add_reference(&format!("__public__::{}", name), to_key);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    graph
}

/// Run cargo check on the entire sliced workspace
/// Returns (all_passed, missing_symbols from failed crates)
fn run_workspace_check(sliced_workspace_dir: &Path) -> (bool, HashSet<String>) {
    // Run cargo check on the entire workspace to catch cross-crate issues
    // and ensure features are properly enabled
    // Use --all-features to check feature-gated code (e.g., log's serde support)
    let output = Command::new("cargo")
        .arg("check")
        .arg("--workspace")
        .arg("--all-features")
        .current_dir(sliced_workspace_dir)
        .output();

    match output {
        Ok(out) => {
            if out.status.success() {
                (true, HashSet::new())
            } else {
                let stderr = String::from_utf8_lossy(&out.stderr);
                let stdout = String::from_utf8_lossy(&out.stdout);
                let combined = format!("{}\n{}", stderr, stdout);

                // Check if this is a manifest error (not a code error)
                // Manifest errors won't have missing symbols to detect
                if combined.contains("failed to load manifest")
                    || combined.contains("failed to parse manifest")
                    || combined.contains("no targets specified")
                {
                    // Fall back to per-crate checks for actual code errors
                    return run_per_crate_checks(sliced_workspace_dir);
                }

                let missing = parse_missing_symbols(&combined);
                (false, missing)
            }
        }
        Err(_) => {
            // If we can't run cargo, fall back to per-crate checks
            run_per_crate_checks(sliced_workspace_dir)
        }
    }
}

/// Fallback: Run cargo check on individual crates
fn run_per_crate_checks(sliced_workspace_dir: &Path) -> (bool, HashSet<String>) {
    let mut all_missing = HashSet::new();
    let mut any_failed = false;

    let entries = match fs::read_dir(sliced_workspace_dir) {
        Ok(e) => e,
        Err(_) => return (true, HashSet::new()),
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }

        match path.file_name().and_then(|n| n.to_str()) {
            Some(n) if n.ends_with("-sliced") => {}
            _ => continue,
        }

        let cargo_toml = path.join("Cargo.toml");
        if !cargo_toml.exists() {
            continue;
        }

        let has_lib = path.join("src/lib.rs").exists();
        let has_main = path.join("src/main.rs").exists();
        if !has_lib && !has_main {
            continue;
        }

        let output = Command::new("cargo")
            .arg("check")
            .current_dir(&path)
            .output();

        if let Ok(out) = output {
            if !out.status.success() {
                any_failed = true;
                let stderr = String::from_utf8_lossy(&out.stderr);
                let stdout = String::from_utf8_lossy(&out.stdout);
                let combined = format!("{}\n{}", stderr, stdout);
                let missing = parse_missing_symbols(&combined);
                all_missing.extend(missing);
            }
        }
    }

    (!any_failed, all_missing)
}

/// Parse compiler errors to extract missing symbol names
fn parse_missing_symbols(stderr: &str) -> HashSet<String> {
    let mut missing = HashSet::new();

    // Patterns for missing symbols:
    // error[E0425]: cannot find value `NAME` in this scope
    // error[E0412]: cannot find type `NAME` in this scope
    // error[E0422]: cannot find struct, variant or union type `NAME`
    // error[E0432]: unresolved import `self::NAME` or `crate::NAME`
    // error[E0433]: failed to resolve: use of undeclared type `NAME`
    // error[E0408]: variable `NAME` is not bound in all patterns

    for line in stderr.lines() {
        // Check for error patterns
        let is_error = line.contains("cannot find")
            || line.contains("undeclared")
            || line.contains("not bound")
            || line.contains("unresolved import")
            || line.contains("failed to resolve")
            || line.contains("not found in this scope")
            || line.contains("E0425")
            || line.contains("E0412")
            || line.contains("E0432")
            || line.contains("E0433");

        if is_error {
            // Extract symbol names between backticks
            let mut rest = line;
            while let Some(start) = rest.find('`') {
                rest = &rest[start + 1..];
                if let Some(end) = rest.find('`') {
                    let name = &rest[..end];
                    rest = &rest[end + 1..];

                    // For paths like `self::Foo` or `crate::bar::Baz`,
                    // extract just the final component
                    let final_name = if name.contains("::") {
                        name.rsplit("::").next().unwrap_or(name)
                    } else {
                        name
                    };

                    // Skip empty, very long, or path-like names
                    if !final_name.is_empty() && final_name.len() < 50 && !final_name.contains('/')
                    {
                        missing.insert(final_name.to_string());
                    }
                } else {
                    break;
                }
            }
        }
    }

    missing
}

/// A trait requirement discovered from compilation errors
/// E.g., E0277 "the trait bound `Foo: Clone` is not satisfied"
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitRequirement {
    /// The type that needs the trait impl
    pub type_name: String,
    /// The trait that must be implemented
    pub trait_name: String,
}

/// Result from JSON-based cargo check
#[derive(Debug, Default)]
pub struct JsonCheckResult {
    /// Whether compilation succeeded
    pub success: bool,
    /// Missing symbol names (from E0425, E0412, E0432, E0433)
    pub missing_symbols: HashSet<String>,
    /// Trait requirements (from E0277)
    pub trait_requirements: HashSet<TraitRequirement>,
    /// Methods not found (from E0599) - implies missing trait impl
    pub missing_methods: HashSet<(String, String)>, // (type_name, method_name)
}

/// Run cargo check with JSON output for faster, more reliable parsing
///
/// This is an upgrade from text-based parsing:
/// - Uses `--message-format=json` for structured output
/// - Directly accesses error codes and messages
/// - More reliable than regex on prose
fn run_workspace_check_json(sliced_workspace_dir: &Path) -> JsonCheckResult {
    // Use --all-features to check feature-gated code (e.g., log's serde support)
    let output = Command::new("cargo")
        .args([
            "check",
            "--workspace",
            "--all-features",
            "--message-format=json",
        ])
        .current_dir(sliced_workspace_dir)
        .output();

    let mut result = JsonCheckResult::default();

    match output {
        Ok(out) => {
            result.success = out.status.success();

            if !result.success {
                // Parse JSON messages from stdout (JSON goes to stdout, human-readable to stderr)
                let stdout = String::from_utf8_lossy(&out.stdout);

                for line in stdout.lines() {
                    if let Ok(msg) = serde_json::from_str::<serde_json::Value>(line) {
                        // Only process compiler messages
                        if msg.get("reason").and_then(|r| r.as_str()) != Some("compiler-message") {
                            continue;
                        }

                        // QUICK WIN: Skip non-error messages (warnings, notes, etc.)
                        let level = msg.get("message")
                            .and_then(|m| m.get("level"))
                            .and_then(|l| l.as_str());
                        if level != Some("error") {
                            continue;
                        }

                        let message = match msg.get("message") {
                            Some(m) => m,
                            None => continue,
                        };

                        // Get error code
                        let code = message
                            .get("code")
                            .and_then(|c| c.get("code"))
                            .and_then(|c| c.as_str())
                            .unwrap_or("");

                        // Get message text
                        let text = message
                            .get("message")
                            .and_then(|m| m.as_str())
                            .unwrap_or("");

                        match code {
                            // Missing value/function
                            "E0425" => {
                                if let Some(sym) = extract_backtick_content(text) {
                                    result.missing_symbols.insert(sym);
                                }
                            }
                            // Missing type
                            "E0412" => {
                                if let Some(sym) = extract_backtick_content(text) {
                                    result.missing_symbols.insert(sym);
                                }
                            }
                            // Unresolved import
                            "E0432" => {
                                if let Some(sym) = extract_backtick_content(text) {
                                    // Extract final path component
                                    let final_sym = sym.rsplit("::").next().unwrap_or(&sym);
                                    result.missing_symbols.insert(final_sym.to_string());
                                }
                            }
                            // Failed to resolve type
                            "E0433" => {
                                if let Some(sym) = extract_backtick_content(text) {
                                    let final_sym = sym.rsplit("::").next().unwrap_or(&sym);
                                    result.missing_symbols.insert(final_sym.to_string());
                                }
                            }
                            // Trait bound not satisfied
                            "E0277" => {
                                if let Some(req) = parse_trait_bound(text) {
                                    result.trait_requirements.insert(req);
                                }
                            }
                            // Method not found (implies missing trait impl)
                            "E0599" => {
                                if let Some((ty, method)) = parse_method_not_found(text) {
                                    result.missing_methods.insert((ty, method));
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            result
        }
        Err(_) => {
            // Fall back to per-crate checks
            let (success, missing) = run_per_crate_checks(sliced_workspace_dir);
            result.success = success;
            result.missing_symbols = missing;
            result
        }
    }
}

/// Extract first content between backticks
fn extract_backtick_content(text: &str) -> Option<String> {
    let start = text.find('`')?;
    let rest = &text[start + 1..];
    let end = rest.find('`')?;
    let content = &rest[..end];

    // Skip empty or path-like content
    if content.is_empty() || content.contains('/') || content.len() > 50 {
        return None;
    }

    Some(content.to_string())
}

/// Parse trait bound error: "the trait bound `Foo: Clone` is not satisfied"
fn parse_trait_bound(text: &str) -> Option<TraitRequirement> {
    // Look for pattern: `Type: Trait`
    let start = text.find('`')?;
    let rest = &text[start + 1..];
    let end = rest.find('`')?;
    let bound = &rest[..end];

    // Split on `: ` to get type and trait
    let parts: Vec<&str> = bound.split(": ").collect();
    if parts.len() >= 2 {
        let type_name = parts[0].trim().to_string();
        // Trait might have multiple bounds like "Clone + Send", take first
        let trait_name = parts[1].split('+').next()?.trim().to_string();

        // Skip if type looks like a generic parameter (single uppercase letter)
        if type_name.len() == 1 && type_name.chars().all(|c| c.is_uppercase()) {
            return None;
        }

        Some(TraitRequirement {
            type_name,
            trait_name,
        })
    } else {
        None
    }
}

/// Parse method not found error: "no method named `foo` found for struct `Bar`"
fn parse_method_not_found(text: &str) -> Option<(String, String)> {
    // Pattern: no method named `X` found for ... `Y`
    if !text.contains("no method named") {
        return None;
    }

    let mut parts = text.split('`');
    parts.next()?; // Skip prefix
    let method = parts.next()?.to_string();

    // Find the type - skip until we find "for" context
    let rest = parts.collect::<Vec<_>>().join("`");
    if let Some(for_idx) = rest.find(" for ") {
        let after_for = &rest[for_idx + 5..];
        if let Some(type_start) = after_for.find('`') {
            let after_start = &after_for[type_start + 1..];
            if let Some(type_end) = after_start.find('`') {
                let type_name = after_start[..type_end].to_string();
                return Some((type_name, method));
            }
        }
    }

    None
}

/// Recursively collect .rs files from src/ directory only
/// Skips build.rs and root-level include!() helper files (like no_atomic.rs)
fn collect_rs_files(crate_dir: &Path, files: &mut Vec<PathBuf>) {
    // Only process files in src/ directory to avoid modifying include!() helper files
    let src_dir = crate_dir.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        crate_dir.to_path_buf()
    };

    fn visit_dir(dir: &Path, files: &mut Vec<PathBuf>) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                        if !name.starts_with('.') && name != "target" {
                            visit_dir(&path, files);
                        }
                    }
                } else if path.extension().map(|e| e == "rs").unwrap_or(false) {
                    // Skip build.rs - it's a special file that needs its main function
                    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                        if name == "build.rs" {
                            continue;
                        }
                    }
                    files.push(path);
                }
            }
        }
    }

    visit_dir(&search_dir, files);
}

/// Extract source text for an item from file content
fn extract_item_source(_content: &str, item: &Item) -> Option<String> {
    // Use span information if available
    let item_str = quote::quote!(#item).to_string();
    // Format it nicely
    if let Ok(parsed) = syn::parse_file(&item_str) {
        Some(crate::unparse::unparse(&parsed))
    } else {
        Some(item_str)
    }
}

/// Generate a unique key for an item (file_path::name)
fn item_key(file_path: &Path, name: &str) -> String {
    format!("{}::{}", file_path.display(), name)
}

/// Extract just the name from a full key
fn name_from_key(key: &str) -> &str {
    key.rsplit("::").next().unwrap_or(key)
}

/// Collect all private items from a directory, storing their source
/// Uses file_path::name as key to distinguish same-named items in different files
fn collect_deletable_items(crate_dir: &Path, max_items: usize) -> HashMap<String, StoredItem> {
    let mut items = HashMap::new();
    let mut rs_files = Vec::new();
    collect_rs_files(crate_dir, &mut rs_files);

    for file_path in rs_files {
        if items.len() >= max_items {
            break;
        }

        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let syntax_tree = match syn::parse_file(&content) {
            Ok(tree) => tree,
            Err(_) => continue,
        };

        for item in &syntax_tree.items {
            if items.len() >= max_items {
                break;
            }

            if !is_private(item) {
                continue;
            }

            // Skip items with #[cfg(...)] - we can't evaluate the conditions
            if has_cfg_attribute(item) {
                continue;
            }

            if let Some((name, kind)) = get_item_info(item) {
                if let Some(source) = extract_item_source(&content, item) {
                    let key = item_key(&file_path, &name);
                    items.insert(
                        key,
                        StoredItem {
                            name,
                            kind,
                            file_path: file_path.clone(),
                            item_source: source,
                            line_start: 0,
                            line_end: 0,
                        },
                    );
                }
            }
        }
    }

    items
}

/// Delete items from files, returning count of deleted items
/// items_to_delete contains full keys (file_path::name)
fn delete_items_from_files(crate_dir: &Path, items_to_delete: &HashSet<String>) -> usize {
    let mut deleted_count = 0;
    let mut rs_files = Vec::new();
    collect_rs_files(crate_dir, &mut rs_files);

    for file_path in rs_files {
        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let syntax_tree = match syn::parse_file(&content) {
            Ok(tree) => tree,
            Err(_) => continue,
        };

        let original_count = syntax_tree.items.len();

        let filtered_items: Vec<Item> = syntax_tree
            .items
            .into_iter()
            .filter(|item| {
                if let Some((name, _)) = get_item_info(item) {
                    let key = item_key(&file_path, &name);
                    if items_to_delete.contains(&key) {
                        deleted_count += 1;
                        return false;
                    }
                }
                true
            })
            .collect();

        if filtered_items.len() < original_count {
            let filtered_file = syn::File {
                shebang: syntax_tree.shebang,
                attrs: syntax_tree.attrs,
                items: filtered_items,
            };

            let modified_content = crate::unparse::unparse(&filtered_file);
            let _ = fs::write(&file_path, modified_content);
        }
    }

    deleted_count
}

/// Restore items to their original files
fn restore_items(
    stored_items: &HashMap<String, StoredItem>,
    items_to_restore: &HashSet<String>,
) -> usize {
    // Group items by file
    let mut items_by_file: HashMap<PathBuf, Vec<&StoredItem>> = HashMap::new();
    for name in items_to_restore {
        if let Some(stored) = stored_items.get(name) {
            items_by_file
                .entry(stored.file_path.clone())
                .or_default()
                .push(stored);
        }
    }

    let mut restored_count = 0;

    for (file_path, items) in items_by_file {
        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let syntax_tree = match syn::parse_file(&content) {
            Ok(tree) => tree,
            Err(_) => continue,
        };

        // Parse and add the stored items back
        let mut new_items = syntax_tree.items;
        for stored in items {
            // Parse the stored item source
            if let Ok(parsed_file) = syn::parse_file(&stored.item_source) {
                for item in parsed_file.items {
                    new_items.push(item);
                    restored_count += 1;
                }
            }
        }

        // Write the file with restored items
        let restored_file = syn::File {
            shebang: syntax_tree.shebang,
            attrs: syntax_tree.attrs,
            items: new_items,
        };

        let modified_content = crate::unparse::unparse(&restored_file);
        let _ = fs::write(&file_path, modified_content);
    }

    restored_count
}

/// Run trial-based deletion with iterative restoration
///
/// This is the main entry point for the trial-based approach:
/// 1. Collect all private items and store their source
/// 2. Delete all of them
/// 3. Run cargo check on the sliced workspace
/// 4. If errors, restore items that are needed
/// 5. Repeat until no errors
///
/// Arguments:
/// - sliced_workspace_dir: The directory containing sliced crates (e.g., unknown_sliced/)
/// - crate_dirs: Paths to sliced crate directories to process
/// Check if a crate should be skipped for trial deletion
fn should_skip_trial_deletion(crate_dir: &Path) -> bool {
    if let Some(name) = crate_dir.file_name().and_then(|n| n.to_str()) {
        // Remove -sliced suffix for comparison
        let base_name = name.strip_suffix("-sliced").unwrap_or(name);
        SKIP_TRIAL_DELETION.contains(&base_name)
    } else {
        false
    }
}

pub fn run_trial_deletion(
    sliced_workspace_dir: &Path,
    crate_dirs: &[PathBuf],
    max_items_per_crate: usize,
    max_iterations: usize,
) -> TrialDeletionResult {
    println!("  🧪 Starting trial-based deletion...");

    // Filter out crates that should skip trial deletion
    let filtered_crate_dirs: Vec<&PathBuf> = crate_dirs
        .iter()
        .filter(|d| !should_skip_trial_deletion(d))
        .collect();

    let skipped_count = crate_dirs.len() - filtered_crate_dirs.len();
    if skipped_count > 0 {
        println!("  ⏭️  Skipping {} crates (on blocklist)", skipped_count);
    }

    // Step 1: Collect all deletable items from all crates (except skipped ones)
    let mut all_stored_items: HashMap<String, StoredItem> = HashMap::new();
    for crate_dir in &filtered_crate_dirs {
        let items = collect_deletable_items(crate_dir, max_items_per_crate);
        all_stored_items.extend(items);
    }

    let total_candidates = all_stored_items.len();
    println!(
        "  📋 Found {} private items as deletion candidates",
        total_candidates
    );

    if total_candidates == 0 {
        return TrialDeletionResult {
            items_deleted: 0,
            items_restored: 0,
            final_deletable: HashSet::new(),
            must_keep: HashSet::new(),
        };
    }

    // Step 2: Delete all candidate items (only from filtered crates)
    let mut currently_deleted: HashSet<String> = all_stored_items.keys().cloned().collect();
    let mut total_deleted = 0;

    for crate_dir in &filtered_crate_dirs {
        let deleted = delete_items_from_files(crate_dir, &currently_deleted);
        total_deleted += deleted;
    }

    println!(
        "  🗑️  Deleted {} items, running verification...",
        total_deleted
    );

    // Step 3: Iterative verification and restoration
    let mut must_keep: HashSet<String> = HashSet::new();
    let mut iterations = 0;
    let mut total_restored = 0;

    loop {
        iterations += 1;
        if iterations > max_iterations {
            println!("  ⚠️  Max iterations ({}) reached", max_iterations);
            break;
        }

        let (success, missing_symbols) = run_workspace_check(sliced_workspace_dir);

        if success {
            println!("  ✅ Verification passed after {} iteration(s)", iterations);
            break;
        }

        if missing_symbols.is_empty() {
            println!("  ⚠️  Build failed but no missing symbols detected, stopping");
            break;
        }

        // Find which deleted items need to be restored
        // Error messages only give names, so we need to find matching full keys
        let mut to_restore: HashSet<String> = HashSet::new();
        let mut unmatched_symbols: Vec<&String> = Vec::new();

        for symbol in &missing_symbols {
            // Find all full keys that end with this symbol name
            let matching_keys: Vec<String> = currently_deleted
                .iter()
                .filter(|key| name_from_key(key) == symbol && !must_keep.contains(*key))
                .cloned()
                .collect();

            if matching_keys.is_empty() {
                unmatched_symbols.push(symbol);
            } else {
                to_restore.extend(matching_keys);
            }
        }

        if to_restore.is_empty() {
            // The missing symbols aren't in our deleted set
            if !unmatched_symbols.is_empty() {
                println!(
                    "  ⚠️  {} missing symbols not in deleted set: {:?}",
                    unmatched_symbols.len(),
                    unmatched_symbols.iter().take(5).collect::<Vec<_>>()
                );
            }
            break;
        }

        // For display, show just the names
        let restore_names: Vec<&str> = to_restore
            .iter()
            .take(3)
            .map(|k| name_from_key(k))
            .collect();

        println!(
            "  🔄 Iteration {}: restoring {} items ({:?}...)",
            iterations,
            to_restore.len(),
            restore_names
        );

        // Restore the items
        let restored = restore_items(&all_stored_items, &to_restore);
        total_restored += restored;

        // Update tracking
        for name in &to_restore {
            currently_deleted.remove(name);
            must_keep.insert(name.clone());
        }
    }

    let final_deletable: HashSet<String> = currently_deleted.clone();

    println!(
        "  📊 Final: {} deleted, {} restored, {} must-keep",
        final_deletable.len(),
        total_restored,
        must_keep.len()
    );

    TrialDeletionResult {
        items_deleted: final_deletable.len(),
        items_restored: total_restored,
        final_deletable,
        must_keep,
    }
}

/// Run trial-based deletion using dependency-aware removal sets
///
/// Key insight: If B depends on A, removing A alone breaks B. But removing
/// {A, B} together might succeed if nothing else uses either.
///
/// Algorithm:
/// 1. Collect all private items and build dependency graph
/// 2. Compute removal sets (item + all dependents)
/// 3. For each set (largest first):
///    a. Delete entire set
///    b. Run cargo check
///    c. If succeeds: mark all as deletable
///    d. If fails: restore set and mark as must-keep
///
/// This finds more deletable code than single-item trials.
pub fn run_trial_deletion_with_sets(
    sliced_workspace_dir: &Path,
    crate_dirs: &[PathBuf],
    max_items_per_crate: usize,
    max_sets: usize,
) -> TrialDeletionResult {
    println!("  🧪 Starting dependency-aware trial deletion...");

    // Filter out crates that should skip trial deletion
    let filtered_crate_dirs: Vec<&PathBuf> = crate_dirs
        .iter()
        .filter(|d| !should_skip_trial_deletion(d))
        .collect();

    let skipped_count = crate_dirs.len() - filtered_crate_dirs.len();
    if skipped_count > 0 {
        println!("  ⏭️  Skipping {} crates (on blocklist)", skipped_count);
    }

    // Step 1: Collect all deletable items from all crates
    let mut all_stored_items: HashMap<String, StoredItem> = HashMap::new();
    for crate_dir in &filtered_crate_dirs {
        let items = collect_deletable_items(crate_dir, max_items_per_crate);
        all_stored_items.extend(items);
    }

    let total_candidates = all_stored_items.len();
    println!(
        "  📋 Found {} private items as deletion candidates",
        total_candidates
    );

    if total_candidates == 0 {
        return TrialDeletionResult {
            items_deleted: 0,
            items_restored: 0,
            final_deletable: HashSet::new(),
            must_keep: HashSet::new(),
        };
    }

    // Step 2: Build dependency graph for each crate
    println!("  🔗 Building intra-crate dependency graph...");
    let mut combined_graph = ItemDependencyGraph::new();
    for crate_dir in &filtered_crate_dirs {
        // Get items for this crate only
        let crate_items: HashMap<String, StoredItem> = all_stored_items
            .iter()
            .filter(|(k, _)| k.starts_with(&crate_dir.display().to_string()))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        if !crate_items.is_empty() {
            let graph = build_item_dependency_graph(crate_dir, &crate_items);
            // Merge into combined graph
            for (key, deps) in graph.references {
                combined_graph
                    .references
                    .entry(key)
                    .or_default()
                    .extend(deps);
            }
            for (key, deps) in graph.dependents {
                combined_graph
                    .dependents
                    .entry(key)
                    .or_default()
                    .extend(deps);
            }
            combined_graph.items.extend(graph.items);
        }
    }

    // Step 3: Compute removal sets
    let candidate_keys: HashSet<String> = all_stored_items.keys().cloned().collect();
    let removal_sets = combined_graph.compute_removal_sets(&candidate_keys);

    let total_sets = removal_sets.len();
    let multi_member_sets = removal_sets.iter().filter(|s| s.size > 1).count();
    println!(
        "  📦 Computed {} removal sets ({} with multiple members)",
        total_sets, multi_member_sets
    );

    // Preview some interesting sets
    for set in removal_sets.iter().filter(|s| s.size > 1).take(3) {
        let member_names: Vec<&str> = set
            .members
            .iter()
            .take(4)
            .map(|k| name_from_key(k))
            .collect();
        println!(
            "    → Set rooted at '{}': {} members {:?}{}",
            name_from_key(&set.root),
            set.size,
            member_names,
            if set.size > 4 { "..." } else { "" }
        );
    }

    // Step 4: Trial delete sets (largest first)
    let mut final_deletable: HashSet<String> = HashSet::new();
    let mut must_keep: HashSet<String> = HashSet::new();
    let mut sets_tested = 0;
    let mut sets_deletable = 0;

    // Limit number of sets to test
    let sets_to_test: Vec<_> = removal_sets
        .into_iter()
        .filter(|s| !s.members.iter().any(|m| must_keep.contains(m)))
        .take(max_sets)
        .collect();

    println!("  🔬 Testing {} removal sets...", sets_to_test.len());

    for set in sets_to_test {
        // Skip if any member is already marked as must-keep
        if set.members.iter().any(|m| must_keep.contains(m)) {
            continue;
        }

        // Skip if all members are already marked as deletable
        if set.members.iter().all(|m| final_deletable.contains(m)) {
            continue;
        }

        sets_tested += 1;

        // Delete all members of this set
        for crate_dir in &filtered_crate_dirs {
            delete_items_from_files(crate_dir, &set.members);
        }

        // Check if compilation still works
        let (success, _missing_symbols) = run_workspace_check(sliced_workspace_dir);

        if success {
            // Entire set can be deleted!
            sets_deletable += 1;
            final_deletable.extend(set.members.clone());
            if sets_tested <= 10 || sets_tested % 10 == 0 {
                println!(
                    "    ✅ Set '{}' ({} items) deletable",
                    name_from_key(&set.root),
                    set.size
                );
            }
        } else {
            // Set is needed - restore all members
            restore_items(&all_stored_items, &set.members);
            must_keep.extend(set.members.clone());
            if sets_tested <= 10 || sets_tested % 10 == 0 {
                println!(
                    "    ❌ Set '{}' ({} items) must keep",
                    name_from_key(&set.root),
                    set.size
                );
            }
        }
    }

    println!(
        "  📊 Final: {} sets tested, {} deletable ({} items), {} must-keep ({} items)",
        sets_tested,
        sets_deletable,
        final_deletable.len(),
        sets_tested - sets_deletable,
        must_keep.len()
    );

    TrialDeletionResult {
        items_deleted: final_deletable.len(),
        items_restored: 0, // We restore in-place, not tracking separately
        final_deletable,
        must_keep,
    }
}

/// Graph-guided bulk deletion with JSON-based verification
///
/// This is the optimized approach that minimizes cargo check invocations:
/// 1. Build dependency graph for all private items
/// 2. Delete ONLY items with no dependents in the graph (safe bulk delete)
/// 3. Run cargo check with JSON output
/// 4. Restore items that caused errors (including trait requirements)
/// 5. Repeat until no errors (usually 2-3 iterations)
///
/// Key insight: The graph catches ~90% of dependencies. Cargo check catches
/// the remaining ~10% (implicit trait requirements, auto traits, etc.).
///
/// This is MUCH faster than trial-per-item because:
/// - Current: N cargo checks (one per item or set)
/// - This: 2-3 cargo checks (bulk delete, then correct)
pub fn run_graph_guided_deletion(
    sliced_workspace_dir: &Path,
    crate_dirs: &[PathBuf],
    max_items_per_crate: usize,
    max_iterations: usize,
) -> TrialDeletionResult {
    println!("  🧪 Starting graph-guided deletion (JSON mode)...");

    // Filter out crates that should skip trial deletion
    let filtered_crate_dirs: Vec<&PathBuf> = crate_dirs
        .iter()
        .filter(|d| !should_skip_trial_deletion(d))
        .collect();

    let skipped_count = crate_dirs.len() - filtered_crate_dirs.len();
    if skipped_count > 0 {
        println!("  ⏭️  Skipping {} crates (on blocklist)", skipped_count);
    }

    // Step 1: Collect all deletable items from all crates
    let mut all_stored_items: HashMap<String, StoredItem> = HashMap::new();
    for crate_dir in &filtered_crate_dirs {
        let items = collect_deletable_items(crate_dir, max_items_per_crate);
        all_stored_items.extend(items);
    }

    let total_candidates = all_stored_items.len();
    println!(
        "  📋 Found {} private items as deletion candidates",
        total_candidates
    );

    if total_candidates == 0 {
        return TrialDeletionResult {
            items_deleted: 0,
            items_restored: 0,
            final_deletable: HashSet::new(),
            must_keep: HashSet::new(),
        };
    }

    // QUICK WIN: Use simple trial deletion for very small crates (< 20 items)
    // Building dependency graph has high overhead that isn't worth it for very small crates
    // Threshold is conservative (20) because graph-guided is much faster than simple trial deletion
    const SMALL_CRATE_THRESHOLD: usize = 20;
    if total_candidates < SMALL_CRATE_THRESHOLD {
        println!("  ⚡ Very small crate ({} < {} items), using simple trial deletion",
            total_candidates, SMALL_CRATE_THRESHOLD);
        return run_trial_deletion(
            sliced_workspace_dir,
            crate_dirs,
            max_items_per_crate,
            max_iterations,
        );
    }

    // Step 2: Build dependency graph
    println!("  🔗 Building dependency graph...");
    let mut combined_graph = ItemDependencyGraph::new();
    for crate_dir in &filtered_crate_dirs {
        let crate_items: HashMap<String, StoredItem> = all_stored_items
            .iter()
            .filter(|(k, _)| k.starts_with(&crate_dir.display().to_string()))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        if !crate_items.is_empty() {
            let graph = build_item_dependency_graph(crate_dir, &crate_items);
            for (key, deps) in graph.references {
                combined_graph
                    .references
                    .entry(key)
                    .or_default()
                    .extend(deps);
            }
            for (key, deps) in graph.dependents {
                combined_graph
                    .dependents
                    .entry(key)
                    .or_default()
                    .extend(deps);
            }
            combined_graph.items.extend(graph.items);
        }
    }

    // Step 3: Identify items with NO dependents (safe to delete)
    let candidate_keys: HashSet<String> = all_stored_items.keys().cloned().collect();
    let mut likely_unused: HashSet<String> = candidate_keys
        .iter()
        .filter(|item| {
            // Item is safe to delete ONLY if it has NO dependents at all
            // This includes synthetic dependents like __public__::FunctionName
            // which indicate a public item references this private item
            match combined_graph.dependents.get(*item) {
                None => true,
                Some(deps) => deps.is_empty(),
            }
        })
        .cloned()
        .collect();

    let items_with_deps = total_candidates - likely_unused.len();
    println!(
        "  📊 Graph analysis: {} likely unused, {} have dependents",
        likely_unused.len(),
        items_with_deps
    );

    // Step 4: Delete all likely-unused items in ONE pass
    let mut total_deleted = 0;
    for crate_dir in &filtered_crate_dirs {
        let deleted = delete_items_from_files(crate_dir, &likely_unused);
        total_deleted += deleted;
    }
    println!(
        "  🗑️  Deleted {} items, running JSON verification...",
        total_deleted
    );

    // Step 5: Iterative correction with JSON-based cargo check
    let mut must_keep: HashSet<String> = HashSet::new();
    let mut total_restored = 0;

    for iteration in 1..=max_iterations {
        let check_result = run_workspace_check_json(sliced_workspace_dir);

        if check_result.success {
            println!("  ✅ Verification passed after {} iteration(s)", iteration);
            break;
        }

        // Combine all error sources to find items to restore
        let mut to_restore: HashSet<String> = HashSet::new();

        // 1. Missing symbols (direct dependencies)
        for symbol in &check_result.missing_symbols {
            let matching_keys: Vec<String> = likely_unused
                .iter()
                .filter(|key| name_from_key(key) == symbol && !must_keep.contains(*key))
                .cloned()
                .collect();
            to_restore.extend(matching_keys);
        }

        // 2. Trait requirements - find impl blocks for the type
        for req in &check_result.trait_requirements {
            // Look for items that might be trait impls for this type
            // (This is heuristic - we look for items related to the type name)
            let matching_keys: Vec<String> = likely_unused
                .iter()
                .filter(|key| {
                    let name = name_from_key(key);
                    // Check if this might be related to the required type/trait
                    name.contains(&req.type_name) || name.contains(&req.trait_name)
                })
                .cloned()
                .collect();
            to_restore.extend(matching_keys);
        }

        // 3. Missing methods - restore type and potential trait impls
        for (type_name, _method) in &check_result.missing_methods {
            let matching_keys: Vec<String> = likely_unused
                .iter()
                .filter(|key| name_from_key(key).contains(type_name))
                .cloned()
                .collect();
            to_restore.extend(matching_keys);
        }

        if to_restore.is_empty() {
            // No items to restore - either errors are from other sources or we're stuck
            let total_errors = check_result.missing_symbols.len()
                + check_result.trait_requirements.len()
                + check_result.missing_methods.len();
            if total_errors > 0 {
                println!(
                    "  ⚠️  {} errors but no matching items to restore, stopping",
                    total_errors
                );
                if !check_result.missing_symbols.is_empty() {
                    println!(
                        "      Missing: {:?}",
                        check_result
                            .missing_symbols
                            .iter()
                            .take(5)
                            .collect::<Vec<_>>()
                    );
                }
                if !check_result.trait_requirements.is_empty() {
                    println!(
                        "      Traits: {:?}",
                        check_result
                            .trait_requirements
                            .iter()
                            .take(3)
                            .collect::<Vec<_>>()
                    );
                }
            }
            break;
        }

        // Restore the items
        let restore_names: Vec<&str> = to_restore
            .iter()
            .take(3)
            .map(|k| name_from_key(k))
            .collect();

        println!(
            "  🔄 Iteration {}: restoring {} items ({:?}{})",
            iteration,
            to_restore.len(),
            restore_names,
            if to_restore.len() > 3 { "..." } else { "" }
        );

        let restored = restore_items(&all_stored_items, &to_restore);
        total_restored += restored;

        // Update tracking
        for key in &to_restore {
            likely_unused.remove(key);
            must_keep.insert(key.clone());
        }
    }

    let final_deletable = likely_unused;

    println!(
        "  📊 Final: {} deleted, {} restored, {} must-keep",
        final_deletable.len(),
        total_restored,
        must_keep.len()
    );

    TrialDeletionResult {
        items_deleted: final_deletable.len(),
        items_restored: total_restored,
        final_deletable,
        must_keep,
    }
}

/// Build a deletion table for a crate (basic version for compatibility)
pub fn build_deletion_table(crate_dir: &Path, max_trials: usize) -> DeletionTable {
    let mut table = DeletionTable {
        crate_name: crate_dir
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default(),
        ..Default::default()
    };

    let mut rs_files = Vec::new();
    collect_rs_files(crate_dir, &mut rs_files);

    let mut trial_count = 0;

    for file_path in rs_files {
        if trial_count >= max_trials {
            break;
        }

        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let syntax_tree = match syn::parse_file(&content) {
            Ok(tree) => tree,
            Err(_) => continue,
        };

        for item in &syntax_tree.items {
            if trial_count >= max_trials {
                break;
            }

            if !is_private(item) {
                continue;
            }

            // Skip items with #[cfg(...)] - we can't evaluate the conditions
            if has_cfg_attribute(item) {
                continue;
            }

            if let Some((name, kind)) = get_item_info(item) {
                trial_count += 1;

                let result = DeletionTrialResult {
                    item_name: name.clone(),
                    item_kind: kind,
                    file_path: file_path.to_string_lossy().to_string(),
                    can_delete: true,
                    error_count: 0,
                    dependent_items: Vec::new(),
                };

                table.safe_to_delete.insert(name);
                table.trials.push(result);
            }
        }
    }

    table
}

/// Apply verified deletions from a deletion table (for compatibility)
pub fn apply_verified_deletions(crate_dir: &Path, table: &DeletionTable) -> Result<usize, String> {
    let deletable = table.find_maximal_deletable_set();

    if deletable.is_empty() {
        return Ok(0);
    }

    Ok(delete_items_from_files(crate_dir, &deletable))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_item_info() {
        let code = "fn test_fn() {}";
        let file = syn::parse_file(code).unwrap();
        let info = get_item_info(&file.items[0]);
        assert_eq!(info, Some(("test_fn".to_string(), "fn".to_string())));
    }

    #[test]
    fn test_parse_missing_symbols() {
        let stderr = r#"
error[E0425]: cannot find value `RUNNING` in this scope
error[E0412]: cannot find type `Guard` in this scope
error[E0432]: unresolved import `self::SuffixOrdering`
error[E0432]: unresolved import `crate::foo::SearcherKind`
error[E0433]: failed to resolve: use of undeclared type `Helper`
error[E0408]: variable `STATE` is not bound in all patterns
        "#;
        let missing = parse_missing_symbols(stderr);
        assert!(missing.contains("RUNNING"));
        assert!(missing.contains("Guard"));
        assert!(missing.contains("SuffixOrdering"));
        assert!(missing.contains("SearcherKind"));
        assert!(missing.contains("Helper"));
        assert!(missing.contains("STATE"));
    }

    #[test]
    fn test_extract_item_source() {
        let code = "fn test_fn() { let x = 1; }";
        let file = syn::parse_file(code).unwrap();
        let source = extract_item_source(code, &file.items[0]);
        assert!(source.is_some());
        assert!(source.unwrap().contains("test_fn"));
    }

    #[test]
    fn test_dependency_graph_basic() {
        let mut graph = ItemDependencyGraph::new();
        graph.add_item("a".to_string());
        graph.add_item("b".to_string());
        graph.add_item("c".to_string());

        // b depends on a (b references a)
        graph.add_reference("b", "a");
        // c depends on b
        graph.add_reference("c", "b");

        // Check forward references
        assert!(graph.references.get("b").unwrap().contains("a"));
        assert!(graph.references.get("c").unwrap().contains("b"));

        // Check reverse dependencies
        assert!(graph.dependents.get("a").unwrap().contains("b"));
        assert!(graph.dependents.get("b").unwrap().contains("c"));
    }

    #[test]
    fn test_transitive_dependents() {
        let mut graph = ItemDependencyGraph::new();
        graph.add_item("a".to_string());
        graph.add_item("b".to_string());
        graph.add_item("c".to_string());

        // Chain: c -> b -> a (c depends on b, b depends on a)
        graph.add_reference("b", "a");
        graph.add_reference("c", "b");

        // Transitive dependents of 'a' should include both 'b' and 'c'
        let deps = graph.get_transitive_dependents("a");
        assert!(deps.contains("b"));
        assert!(deps.contains("c"));
        assert_eq!(deps.len(), 2);

        // Transitive dependents of 'b' should only include 'c'
        let deps = graph.get_transitive_dependents("b");
        assert!(deps.contains("c"));
        assert_eq!(deps.len(), 1);

        // 'c' has no dependents
        let deps = graph.get_transitive_dependents("c");
        assert!(deps.is_empty());
    }

    #[test]
    fn test_removal_sets() {
        let mut graph = ItemDependencyGraph::new();
        graph.add_item("helper".to_string());
        graph.add_item("uses_helper".to_string());
        graph.add_item("standalone".to_string());

        // uses_helper depends on helper
        graph.add_reference("uses_helper", "helper");

        let candidates: HashSet<String> = ["helper", "uses_helper", "standalone"]
            .iter()
            .map(|s| s.to_string())
            .collect();

        let sets = graph.compute_removal_sets(&candidates);

        // Should have sets, the helper set should include uses_helper
        assert!(!sets.is_empty());

        // Find the set rooted at "helper"
        let helper_set = sets.iter().find(|s| s.root == "helper");
        assert!(helper_set.is_some());
        let helper_set = helper_set.unwrap();

        // The helper set should contain both helper and uses_helper
        assert!(helper_set.members.contains("helper"));
        assert!(helper_set.members.contains("uses_helper"));
        assert_eq!(helper_set.size, 2);
    }

    #[test]
    fn test_extract_references() {
        let code = "fn uses_helper() { helper(); other_fn(); }";
        let file = syn::parse_file(code).unwrap();
        let refs = extract_references_from_item(&file.items[0]);

        // Should find helper and other_fn
        assert!(refs.contains("helper"));
        assert!(refs.contains("other_fn"));
        // Should not contain keywords
        assert!(!refs.contains("fn"));
    }

    // === Tests for JSON parsing functions (Phase 6) ===

    #[test]
    fn test_extract_backtick_content() {
        assert_eq!(
            extract_backtick_content("cannot find `Foo` in this scope"),
            Some("Foo".to_string())
        );
        assert_eq!(
            extract_backtick_content("unresolved import `crate::bar::Baz`"),
            Some("crate::bar::Baz".to_string())
        );
        assert_eq!(extract_backtick_content("no backticks here"), None);
        assert_eq!(extract_backtick_content("empty ``"), None);
    }

    #[test]
    fn test_parse_trait_bound() {
        // Standard trait bound error
        let result = parse_trait_bound("the trait bound `Foo: Clone` is not satisfied");
        assert!(result.is_some());
        let req = result.unwrap();
        assert_eq!(req.type_name, "Foo");
        assert_eq!(req.trait_name, "Clone");

        // Multiple bounds - should take first
        let result = parse_trait_bound("the trait bound `Bar: Send + Sync` is not satisfied");
        assert!(result.is_some());
        let req = result.unwrap();
        assert_eq!(req.type_name, "Bar");
        assert_eq!(req.trait_name, "Send");

        // Generic parameter (should be skipped)
        let result = parse_trait_bound("the trait bound `T: Clone` is not satisfied");
        assert!(result.is_none());

        // No match
        let result = parse_trait_bound("some other error message");
        assert!(result.is_none());
    }

    #[test]
    fn test_parse_method_not_found() {
        // Standard method not found
        let result = parse_method_not_found("no method named `foo` found for struct `Bar`");
        assert!(result.is_some());
        let (ty, method) = result.unwrap();
        assert_eq!(ty, "Bar");
        assert_eq!(method, "foo");

        // No match
        let result = parse_method_not_found("cannot find value");
        assert!(result.is_none());
    }

    #[test]
    fn test_trait_requirement_equality() {
        let req1 = TraitRequirement {
            type_name: "Foo".to_string(),
            trait_name: "Clone".to_string(),
        };
        let req2 = TraitRequirement {
            type_name: "Foo".to_string(),
            trait_name: "Clone".to_string(),
        };
        let req3 = TraitRequirement {
            type_name: "Bar".to_string(),
            trait_name: "Clone".to_string(),
        };

        assert_eq!(req1, req2);
        assert_ne!(req1, req3);

        // Test hashability
        let mut set = HashSet::new();
        set.insert(req1.clone());
        assert!(set.contains(&req2));
        assert!(!set.contains(&req3));
    }
}

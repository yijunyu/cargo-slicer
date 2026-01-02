//! Core data structures for the cargo slicer.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// Represents a used item from a dependency crate
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct UsedItem {
    /// Full path like "regex::Regex::new"
    pub path: String,
    /// Kind: struct, fn, trait, impl, etc.
    pub kind: ItemKind,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[allow(dead_code)]
pub enum ItemKind {
    Struct,
    Function,
    Method,
    Trait,
    Impl,
    Type,
    Const,
    Static,
    Module,
    Unknown,
}

/// Tracks variable types for method call resolution
#[derive(Debug, Default)]
pub struct TypeTracker {
    /// Maps variable name to its type (e.g., "re" -> "regex::Regex")
    pub var_types: HashMap<String, String>,
    /// Maps short type name to full path (e.g., "Regex" -> "regex::Regex")
    pub type_aliases: HashMap<String, String>,
    /// Maps module alias to full path (e.g., "mpsc" -> "tokio::sync::mpsc")
    pub module_aliases: HashMap<String, String>,
}

/// Represents a module declaration with cfg attributes and optional path redirect
#[derive(Debug, Clone)]
pub struct ModuleDecl {
    pub name: String,
    pub is_pub: bool,
    pub cfg_attr: Option<String>,  // Raw cfg attribute string
    pub path_redirect: Option<String>,
}

/// Represents a parsed item from the crate source
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ParsedItem {
    /// Item name (e.g., "Regex", "new", "escape")
    pub name: String,
    /// Full module path (e.g., "regex::Regex")
    pub path: String,
    /// Kind of item
    pub kind: ParsedItemKind,
    /// The actual source code of this item
    pub source: String,
    /// Types this item depends on (internal items)
    pub dependencies: HashSet<String>,
    /// External crates this item depends on (e.g., "serde", "rand_core")
    pub external_crates: HashSet<String>,
    /// Module path where this item is defined (e.g., "crate::seq::index")
    pub module_path: String,
    /// Is this item public?
    pub is_pub: bool,
    /// File where this item is defined
    pub file: PathBuf,
    /// Any #[cfg(...)] attribute on this item
    pub cfg_attr: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParsedItemKind {
    Struct,
    Enum,
    Function,
    Trait,
    Impl,
    TypeAlias,
    Const,
    Static,
    Macro,
    Mod,
    ExternBlock,
}

/// Result of dependency extraction: (internal_deps, external_crates)
pub struct ExtractedDeps {
    pub internal: HashSet<String>,
    pub external_crates: HashSet<String>,
}

/// Information about a located crate
#[derive(Debug)]
pub struct CrateInfo {
    pub name: String,
    pub version: String,
    pub edition: String,
    pub path: PathBuf,
    pub lib_path: Option<PathBuf>,
    pub total_items: usize,
    pub total_lines: usize,
}

/// Index of all items in a crate
#[derive(Debug, Default)]
#[allow(dead_code)]
pub struct CrateIndex {
    /// All parsed items, indexed by name
    pub items: HashMap<String, Vec<ParsedItem>>,
    /// Impl blocks for each type
    pub impls: HashMap<String, Vec<ParsedItem>>,
    /// All items in order of appearance
    pub all_items: Vec<ParsedItem>,
    /// Re-export mapping: public module path -> source module path
    pub reexports: HashMap<String, String>,
    /// Original use statements from source files: (source_code, symbols_imported)
    /// e.g., ("use std::sync::atomic::Ordering;", ["Ordering"])
    pub use_statements: Vec<UseStatement>,
}

/// A use statement from the original source code
#[derive(Debug, Clone)]
pub struct UseStatement {
    /// The full source code of the use statement (e.g., "use std::collections::HashMap;")
    pub source: String,
    /// Symbols imported by this statement (e.g., ["HashMap"])
    pub symbols: Vec<String>,
    /// Is this an external import (std, core, alloc, or external crate)?
    pub is_external: bool,
}

#[allow(dead_code)]
impl CrateIndex {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_item(&mut self, item: ParsedItem) {
        let name = item.name.clone();

        // Track impl blocks separately
        if item.kind == ParsedItemKind::Impl {
            self.impls.entry(name.clone()).or_default().push(item.clone());
        }

        // For extern blocks, also index by each function name they contain
        if item.kind == ParsedItemKind::ExternBlock {
            // Parse the source to find all fn declarations
            for fn_name in extract_extern_fn_names(&item.source) {
                self.items.entry(fn_name).or_default().push(item.clone());
            }
        }

        self.items.entry(name).or_default().push(item.clone());
        self.all_items.push(item);
    }

    /// Add a use statement
    pub fn add_use_statement(&mut self, stmt: UseStatement) {
        // Avoid duplicates
        if !self.use_statements.iter().any(|s| s.source == stmt.source) {
            self.use_statements.push(stmt);
        }
    }

    /// Get an item by name
    pub fn get(&self, name: &str) -> Option<&ParsedItem> {
        self.items.get(name).and_then(|items| items.first())
    }

    /// Get all items with a given name
    pub fn get_all(&self, name: &str) -> Vec<&ParsedItem> {
        self.items.get(name).map(|v| v.iter().collect()).unwrap_or_default()
    }

    /// Find inline modules that contain a specific item definition
    pub fn find_containing_modules(&self, item_name: &str) -> Vec<&ParsedItem> {
        self.all_items.iter()
            .filter(|item| {
                if item.kind != ParsedItemKind::Mod {
                    return false;
                }
                let patterns = [
                    format!("trait {} ", item_name),
                    format!("trait {}<", item_name),
                    format!("trait {}{{", item_name),
                    format!("trait {}:", item_name),  // trait bounds like `trait Sealed: Copy`
                    format!("struct {} ", item_name),
                    format!("struct {}<", item_name),
                    format!("struct {}{{", item_name),
                    format!("struct {}(", item_name),
                    format!("struct {};", item_name),
                    format!("struct {}:", item_name),  // struct with generics bounds
                    format!("enum {} ", item_name),
                    format!("enum {}<", item_name),
                    format!("enum {}{{", item_name),
                    format!("type {} ", item_name),
                    format!("type {}=", item_name),
                    format!("type {}<", item_name),
                    format!("fn {}(", item_name),
                    format!("fn {}<", item_name),
                    format!("const {} ", item_name),
                    format!("const {}:", item_name),
                    format!("static {} ", item_name),
                    format!("static {}:", item_name),
                    format!("mod {} ", item_name),  // nested modules
                    format!("mod {}{{", item_name),
                ];
                patterns.iter().any(|p| item.source.contains(p))
            })
            .collect()
    }

    /// Get all impl blocks for a type
    pub fn get_impls(&self, type_name: &str) -> Vec<&ParsedItem> {
        self.impls.get(type_name).map(|v| v.iter().collect()).unwrap_or_default()
    }

    /// Add a re-export mapping
    pub fn add_reexport(&mut self, public_path: String, source_path: String) {
        self.reexports.insert(public_path, source_path);
    }

    /// Resolve a module path through re-exports
    pub fn resolve_reexport(&self, module_path: &str) -> Option<&str> {
        self.reexports.get(module_path).map(|s| s.as_str())
    }

    /// Get all items by name, including those accessible via re-exports
    pub fn get_all_with_reexports(&self, name: &str, module_hint: Option<&str>) -> Vec<&ParsedItem> {
        let mut results = self.get_all(name);

        if let Some(module) = module_hint {
            if let Some(source_module) = self.resolve_reexport(module) {
                for item in &self.all_items {
                    if item.name == name && item.module_path.contains(source_module) {
                        if !results.iter().any(|r| std::ptr::eq(*r, item)) {
                            results.push(item);
                        }
                    }
                }
            }
        }

        results
    }
}

/// Parse Cargo.toml and extract dependency information
#[derive(Debug, Default)]
#[allow(dead_code)]
pub struct CrateDependencies {
    /// Direct dependencies: name -> version or details
    pub dependencies: HashMap<String, String>,
    /// Dev dependencies
    pub dev_dependencies: HashMap<String, String>,
    /// Features used
    pub features: Vec<String>,
    /// Raw [features] section from original Cargo.toml
    pub features_section: String,
    /// Raw target-specific dependency sections
    pub target_deps_sections: Vec<String>,
    /// Names of target-specific dependencies
    pub target_dep_names: HashSet<String>,
}

#[derive(Debug)]
pub struct SemanticSliceResult {
    pub total_parsed: usize,
    pub items_needed: usize,
    pub items_included: usize,
    pub lines_generated: usize,
}

/// Represents a missing item that needs a stub
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum MissingItem {
    Type(String),
    Trait(String),
    Function(String),
    Macro(String),
    Module(String),
}

/// Dependency graph node
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct DepNode {
    pub name: String,
    pub version: String,
    pub edition: String,
    pub deps: Vec<String>,
    pub manifest_path: Option<PathBuf>,
}

/// Full dependency graph
#[derive(Debug, Default)]
pub struct DepGraph {
    pub nodes: HashMap<String, DepNode>,
    pub root: String,
}

impl DepGraph {
    /// Get topological order (leaf crates first)
    pub fn topo_order(&self) -> Vec<&str> {
        let mut visited = HashSet::new();
        let mut order = Vec::new();

        fn visit<'a>(
            graph: &'a DepGraph,
            name: &'a str,
            visited: &mut HashSet<&'a str>,
            order: &mut Vec<&'a str>,
        ) {
            if visited.contains(name) {
                return;
            }
            visited.insert(name);

            if let Some(node) = graph.nodes.get(name) {
                for dep in &node.deps {
                    visit(graph, dep, visited, order);
                }
            }
            order.push(name);
        }

        for name in self.nodes.keys() {
            visit(self, name, &mut visited, &mut order);
        }

        order
    }

    /// Get direct dependencies (excluding root)
    pub fn direct_deps(&self) -> Vec<&str> {
        if let Some(root) = self.nodes.get(&self.root) {
            root.deps.iter().map(|s| s.as_str()).collect()
        } else {
            Vec::new()
        }
    }

    /// Get all transitive dependencies (excludes root package)
    pub fn all_deps(&self) -> Vec<&str> {
        self.nodes.keys()
            .filter(|name| *name != &self.root)
            .map(|s| s.as_str())
            .collect()
    }

    /// Get all crates that depend on a given crate (reverse dependencies)
    pub fn reverse_deps(&self, crate_name: &str) -> Vec<&str> {
        self.nodes.iter()
            .filter(|(_, node)| node.deps.iter().any(|d| d == crate_name))
            .map(|(name, _)| name.as_str())
            .collect()
    }
}

/// Result of slicing all crates
#[derive(Debug, Default)]
pub struct SliceAllResult {
    pub success: Vec<String>,
    pub failed: Vec<(String, String)>,
    pub passthrough: Vec<(String, String)>,
    pub skipped: Vec<String>,
}

/// Extract function names from an extern block source code
/// Looks for patterns like `fn function_name(` or `fn function_name <`
pub fn extract_extern_fn_names(source: &str) -> Vec<String> {
    let mut names = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(c) = chars.next() {
        // Look for "fn " pattern
        if c == 'f' && chars.peek() == Some(&'n') {
            chars.next(); // consume 'n'
            // Must be followed by whitespace
            if let Some(&ws) = chars.peek() {
                if ws.is_whitespace() {
                    // Skip whitespace
                    while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                        chars.next();
                    }
                    // Collect identifier
                    let mut name = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            name.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if !name.is_empty() {
                        names.push(name);
                    }
                }
            }
        }
    }
    names
}

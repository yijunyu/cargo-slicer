//! Symbol Resolution System
//!
//! This module provides a unified approach to symbol resolution for sliced code.
//! The goal is to bridge the gap between SCIP semantic information and Rust compiler
//! requirements for successful compilation.
//!
//! ## Architecture
//!
//! 1. **SymbolTable**: Collects all symbols referenced in sliced code
//! 2. **SymbolResolver**: Determines the source of each symbol
//! 3. **ImportGenerator**: Generates import statements from resolutions
//!
//! ## Design Principles
//!
//! - **Additive only**: Never filter imports, only add
//! - **Conservative**: When uncertain, generate import anyway
//! - **Monotonic**: Each change should reduce or maintain error count

use std::collections::{HashMap, HashSet};
use regex::Regex;
use once_cell::sync::Lazy;
use crate::debug_log;

/// The kind of symbol being referenced
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    /// Struct, enum, union, or type alias
    Type,
    /// Trait definition
    Trait,
    /// Function or method
    Function,
    /// Constant or static
    Constant,
    /// Module
    Module,
    /// Macro (macro_rules! or proc-macro)
    Macro,
    /// Unknown - could be any of the above
    Unknown,
}

impl SymbolKind {
    /// Infer symbol kind from naming conventions
    pub fn infer_from_name(name: &str) -> Self {
        if name.is_empty() {
            return SymbolKind::Unknown;
        }

        // Check for macro first (ends with !)
        if name.ends_with('!') {
            return SymbolKind::Macro;
        }

        // Check for ALL_CAPS constant (e.g., MAX_SIZE, BUFFER_LEN)
        // Must have at least one underscore or be all uppercase letters
        if name.len() > 1 && name.chars().all(|c| c.is_uppercase() || c == '_' || c.is_ascii_digit()) {
            // Ensure it's not just a single word like "OK"
            if name.contains('_') || name.len() > 2 {
                return SymbolKind::Constant;
            }
        }

        let first_char = name.chars().next().unwrap();

        // Uppercase typically indicates Type or Trait
        if first_char.is_uppercase() {
            // Could be Type or Trait - we'll resolve this later
            SymbolKind::Type
        } else {
            // lowercase typically indicates function or module
            SymbolKind::Function
        }
    }
}

/// Location where a symbol is used
#[derive(Debug, Clone)]
pub struct UsageLocation {
    /// Module path where the symbol is used (e.g., "foo::bar")
    pub module_path: String,
    /// Whether the usage is qualified (e.g., `std::path::Path` vs just `Path`)
    pub is_qualified: bool,
    /// The qualifier prefix if any (e.g., "std::path" for `std::path::Path`)
    pub qualifier: Option<String>,
}

/// Location where a symbol is defined
#[derive(Debug, Clone)]
pub struct DefinitionLocation {
    /// Full path to the definition (e.g., "crate::util::helper")
    pub path: String,
    /// Whether the definition is public
    pub is_public: bool,
}

/// Information about a symbol
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    /// The symbol name
    pub name: String,
    /// Kind of symbol
    pub kind: SymbolKind,
    /// All places where this symbol is used
    pub usages: Vec<UsageLocation>,
    /// Where this symbol is defined (if known and in-crate)
    pub definition: Option<DefinitionLocation>,
}

impl SymbolInfo {
    pub fn new(name: String, kind: SymbolKind) -> Self {
        Self {
            name,
            kind,
            usages: Vec::new(),
            definition: None,
        }
    }

    pub fn add_usage(&mut self, location: UsageLocation) {
        self.usages.push(location);
    }

    pub fn set_definition(&mut self, location: DefinitionLocation) {
        self.definition = Some(location);
    }
}

/// How a symbol should be resolved/imported
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolution {
    /// Symbol is defined locally in the current module - no import needed
    Local,
    /// Symbol is defined elsewhere in the crate
    Crate { path: String },
    /// Symbol is from the standard library
    Std { path: String },
    /// Symbol is from an external crate
    External { path: String },
    /// Cannot determine source - will try conservative import
    Unknown,
}

impl Resolution {
    /// Generate the import statement for this resolution
    pub fn to_import_statement(&self, symbol: &str) -> Option<String> {
        match self {
            Resolution::Local => None,
            Resolution::Crate { path } => {
                // Priority 7: Skip incorrect Visitor trait imports (E0407 fix)
                // Don't generate imports for hir::visitor::Visitor - this causes trait mismatch errors
                // Check both exact match and path containing hir::visitor
                let is_hir_visitor = symbol == "Visitor" && (path == "hir::visitor" || path.contains("hir::visitor"));

                // Priority 7 Debug: Trace Visitor import attempts in symbol.rs
                let debug_visitor = std::env::var("DEBUG_VISITOR").is_ok();
                if debug_visitor && symbol.contains("Visitor") {
                    eprintln!("[DEBUG_VISITOR] symbol.rs to_import_statement:");
                    eprintln!("  Symbol: {}", symbol);
                    eprintln!("  Path: {}", path);
                    eprintln!("  Is hir::visitor: {}", is_hir_visitor);
                }

                if is_hir_visitor {
                    if debug_visitor {
                        eprintln!("[DEBUG_VISITOR] FILTERED by is_hir_visitor check in symbol.rs");
                    }
                    return None;
                }

                if path.is_empty() {
                    Some(format!("use crate::{};", symbol))
                } else {
                    Some(format!("use crate::{}::{};", path, symbol))
                }
            }
            Resolution::Std { path } => {
                // Skip unstable std imports
                if crate::old_slicer::semantic::is_unstable_std_import(path) {
                    return None;
                }

                // Skip Rust prelude types - they're always available without importing
                // https://doc.rust-lang.org/std/prelude/index.html
                const PRELUDE_TYPES: &[&str] = &[
                    // Comparison traits
                    "std::cmp::Eq",
                    "std::cmp::Ord",
                    "std::cmp::PartialEq",
                    "std::cmp::PartialOrd",
                    // Conversion traits
                    "std::convert::AsRef",
                    "std::convert::AsMut",
                    "std::convert::From",
                    "std::convert::Into",
                    // Default/Clone
                    "std::default::Default",
                    "std::clone::Clone",
                    // Marker traits
                    "std::marker::Copy",
                    "std::marker::Send",
                    "std::marker::Sized",
                    "std::marker::Sync",
                    // Ops traits
                    "std::ops::Drop",
                    "std::ops::Fn",
                    "std::ops::FnMut",
                    "std::ops::FnOnce",
                    // Iterator traits
                    "std::iter::DoubleEndedIterator",
                    "std::iter::ExactSizeIterator",
                    "std::iter::Extend",
                    "std::iter::IntoIterator",
                    "std::iter::Iterator",
                    // Borrow
                    "std::borrow::ToOwned",
                    // Option/Result
                    "std::option::Option",
                    "std::result::Result",
                    // Common types
                    "std::string::String",
                    "std::string::ToString",
                    "std::vec::Vec",
                    "std::boxed::Box",
                    // mem functions
                    "std::mem::drop",
                ];

                if PRELUDE_TYPES.contains(&path.as_str()) {
                    // Debug: Log when we skip a prelude type
                    if std::env::var("CARGO_SLICER_DEBUG_LOG").is_ok() {
                        crate::debug_log!("[PRELUDE FILTER] Skipping import for {}", path);
                    }
                    return None;
                }

                Some(format!("use {};", path))
            }
            Resolution::External { path } => {
                if path.contains("use ") {
                    Some(format!("{};", path))
                } else {
                    Some(format!("use {};", path))
                }
            }
            Resolution::Unknown => None, // Cannot generate import for unknown
        }
    }

    /// Priority for resolution (lower is better)
    /// Used when multiple resolutions are possible
    pub fn priority(&self) -> u8 {
        match self {
            Resolution::Local => 0,      // Best: no import needed
            Resolution::Crate { .. } => 1, // Good: same crate
            Resolution::Std { .. } => 2,  // OK: standard library
            Resolution::External { .. } => 3, // Acceptable: external
            Resolution::Unknown => 4,    // Worst: unknown
        }
    }
}

/// Collects all symbols from sliced code
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// All symbols found, keyed by name
    symbols: HashMap<String, SymbolInfo>,
    /// Symbols defined in each module
    module_definitions: HashMap<String, HashSet<String>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a symbol usage
    pub fn add_usage(&mut self, name: &str, kind: SymbolKind, location: UsageLocation) {
        let info = self
            .symbols
            .entry(name.to_string())
            .or_insert_with(|| SymbolInfo::new(name.to_string(), kind));
        info.add_usage(location);
    }

    /// Add a symbol definition
    pub fn add_definition(&mut self, name: &str, kind: SymbolKind, location: DefinitionLocation) {
        let module = location.path.rsplit("::").skip(1).collect::<Vec<_>>().into_iter().rev().collect::<Vec<_>>().join("::");

        self.module_definitions
            .entry(module)
            .or_default()
            .insert(name.to_string());

        let info = self
            .symbols
            .entry(name.to_string())
            .or_insert_with(|| SymbolInfo::new(name.to_string(), kind));
        info.set_definition(location);
    }

    /// Get all symbols
    pub fn symbols(&self) -> &HashMap<String, SymbolInfo> {
        &self.symbols
    }

    /// Check if a symbol is defined in a specific module
    pub fn is_defined_in_module(&self, symbol: &str, module: &str) -> bool {
        self.module_definitions
            .get(module)
            .map(|defs| defs.contains(symbol))
            .unwrap_or(false)
    }

    /// Get all symbol names
    pub fn symbol_names(&self) -> impl Iterator<Item = &str> {
        self.symbols.keys().map(|s| s.as_str())
    }

    /// Collect symbols from code content
    /// This is a simplified collector - will be enhanced incrementally
    pub fn collect_from_code(&mut self, code: &str, module_path: &str) {
        // Collect uppercase identifiers (potential types/traits)
        let mut current_word = String::new();
        let code_bytes = code.as_bytes();
        let mut word_start = 0;

        for (i, c) in code.chars().enumerate() {
            if c.is_alphanumeric() || c == '_' {
                if current_word.is_empty() {
                    word_start = i;
                }
                current_word.push(c);
            } else {
                if !current_word.is_empty() && current_word.len() > 1 {
                    // Check if this is part of a qualified path (preceded by ::)
                    let is_qualified = if word_start >= 2 {
                        &code_bytes[word_start - 2..word_start] == b"::"
                    } else {
                        false
                    };

                    if !is_qualified {
                        let first_char = current_word.chars().next().unwrap();

                        // Skip common keywords
                        if is_rust_keyword(&current_word) {
                            current_word.clear();
                            continue;
                        }

                        let kind = SymbolKind::infer_from_name(&current_word);

                        // Only track uppercase (types/traits) for now
                        if first_char.is_uppercase() {
                            self.add_usage(
                                &current_word,
                                kind,
                                UsageLocation {
                                    module_path: module_path.to_string(),
                                    is_qualified: false,
                                    qualifier: None,
                                },
                            );
                        }
                    }
                }
                current_word.clear();
            }
        }
    }

    /// Comprehensive symbol collection from code
    /// Extracts types, traits, functions, and module paths
    pub fn collect_comprehensive(&mut self, code: &str, module_path: &str) {
        // First pass: collect simple identifiers
        self.collect_from_code(code, module_path);

        // Second pass: collect qualified paths (e.g., std::path::Path, crate::util::Helper)
        self.collect_qualified_paths(code, module_path);
    }

    /// Collect qualified path references (e.g., std::path::Path)
    fn collect_qualified_paths(&mut self, code: &str, module_path: &str) {
        // Pattern to match qualified paths like std::path::Path or crate::util::Helper
        static QUALIFIED_PATH: Lazy<Regex> = Lazy::new(|| {
            Regex::new(r"\b(std|core|alloc|crate|self|super)(\s*::\s*[a-zA-Z_][a-zA-Z0-9_]*)+").unwrap()
        });

        for cap in QUALIFIED_PATH.find_iter(code) {
            let path = cap.as_str();
            // Normalize spaces around ::
            let normalized: String = path.split("::").map(|s| s.trim()).collect::<Vec<_>>().join("::");

            // Extract the final component (the actual symbol)
            if let Some(last_part) = normalized.rsplit("::").next() {
                let first_char = last_part.chars().next().unwrap_or('a');
                if first_char.is_uppercase() {
                    // This is a type/trait reference
                    let qualifier = normalized.rsplit("::").skip(1).collect::<Vec<_>>().into_iter().rev().collect::<Vec<_>>().join("::");

                    self.add_usage(
                        last_part,
                        SymbolKind::Type,
                        UsageLocation {
                            module_path: module_path.to_string(),
                            is_qualified: true,
                            qualifier: Some(qualifier),
                        },
                    );
                }
            }
        }
    }

    /// Get all unique type symbols that need imports
    pub fn get_unresolved_types(&self, local_definitions: &HashSet<String>) -> HashSet<String> {
        self.symbols
            .iter()
            .filter(|(name, info)| {
                // Only types/traits
                matches!(info.kind, SymbolKind::Type | SymbolKind::Trait)
                    // Not defined locally
                    && !local_definitions.contains(*name)
                    // Has unqualified usages (needs import)
                    && info.usages.iter().any(|u| !u.is_qualified)
            })
            .map(|(name, _)| name.clone())
            .collect()
    }
}

/// Check if a word is a Rust keyword
fn is_rust_keyword(word: &str) -> bool {
    matches!(
        word,
        "Self" | "self" | "super" | "crate" | "pub" | "fn" | "let" | "mut" | "const"
            | "static" | "struct" | "enum" | "trait" | "impl" | "type" | "where"
            | "for" | "loop" | "while" | "if" | "else" | "match" | "return"
            | "break" | "continue" | "move" | "ref" | "as" | "in" | "mod" | "use"
            | "true" | "false" | "async" | "await" | "dyn" | "unsafe" | "extern"
    )
}

/// Types that are commonly redefined by crates and should not be force-imported from std
const COMMONLY_REDEFINED: &[&str] = &[
    "Result", "Ok", "Err",
    "Range", "RangeFrom", "RangeTo", "RangeFull", "RangeInclusive",
    "Error", "Token", "Ordering", "CachePadded",
    "OsStringExt", "OsStrExt", "Block",
    "Context", "Builder", "Config", "State",
];

/// Resolves symbols to their sources
pub struct SymbolResolver {
    /// Types defined in the standard library (name -> full path)
    std_types: HashMap<String, String>,
    /// Types defined in the crate (name -> crate path)
    crate_types: HashMap<String, String>,
    /// External imports preserved from original code
    external_imports: HashMap<String, String>,
    /// Types defined anywhere in the crate (for skipping std imports)
    crate_local_types: HashSet<String>,
}

impl SymbolResolver {
    pub fn new(
        std_types: HashMap<String, String>,
        crate_types: HashMap<String, String>,
        external_imports: HashMap<String, String>,
    ) -> Self {
        Self {
            std_types,
            crate_types,
            external_imports,
            crate_local_types: HashSet::new(),
        }
    }

    pub fn with_crate_local_types(mut self, types: HashSet<String>) -> Self {
        self.crate_local_types = types;
        self
    }

    /// Resolve a symbol to its source
    ///
    /// Priority order:
    /// 1. Local definition (same module)
    /// 2. Crate-local type (defined somewhere in crate) - skip std import
    /// 3. Crate definition (elsewhere in crate)
    /// 4. Standard library
    /// 5. External crate
    /// 6. Unknown (conservative)
    pub fn resolve(
        &self,
        symbol: &str,
        current_module: &str,
        local_definitions: &HashSet<String>,
    ) -> Resolution {
        // 1. Check if defined locally in this module
        if local_definitions.contains(symbol) {
            return Resolution::Local;
        }

        // 2. Priority 5: Check if symbol is commonly redefined (Result, Error, etc.)
        // These should use std prelude types, not crate-local type aliases
        // This prevents importing incompatible type aliases like ast::parse::Result<T>
        // when code needs std::result::Result<T, E> with 2 generic parameters
        if COMMONLY_REDEFINED.contains(&symbol) {
            // Even if the crate defines this type, don't import it - use std prelude
            return Resolution::Unknown;
        }

        // 3. Check if this is a crate-local type (don't add std import for it)
        // This prevents E0252 (name defined multiple times) when the crate
        // defines a type with the same name as a std type
        if self.crate_local_types.contains(symbol) {
            // Check if we have a specific crate path for it
            if let Some(path) = self.crate_types.get(symbol) {
                if path != current_module && !path.starts_with(&format!("{}::", current_module)) {
                    return Resolution::Crate { path: path.clone() };
                }
            }
            // It's crate-local but we don't know where - don't add import
            return Resolution::Unknown;
        }

        // 4. Check if defined elsewhere in the crate
        if let Some(path) = self.crate_types.get(symbol) {
            // Don't import from current module
            if path != current_module && !path.starts_with(&format!("{}::", current_module)) {
                return Resolution::Crate { path: path.clone() };
            }
        }

        // 5. Check standard library
        if let Some(path) = self.std_types.get(symbol) {
            // Note: COMMONLY_REDEFINED check is now at step 2, before crate checks
            return Resolution::Std { path: path.clone() };
        }

        // 6. Check external imports
        if let Some(path) = self.external_imports.get(symbol) {
            return Resolution::External { path: path.clone() };
        }

        // 7. Unknown - cannot resolve
        Resolution::Unknown
    }

    /// Resolve all symbols in a table for a specific module
    pub fn resolve_all(
        &self,
        table: &SymbolTable,
        current_module: &str,
        local_definitions: &HashSet<String>,
    ) -> HashMap<String, Resolution> {
        let mut resolutions = HashMap::new();

        for (name, _info) in table.symbols() {
            let resolution = self.resolve(name, current_module, local_definitions);
            resolutions.insert(name.clone(), resolution);
        }

        resolutions
    }
}

/// Generates import statements from resolutions
pub struct ImportGenerator;

impl ImportGenerator {
    /// Generate import statements for a module
    pub fn generate(
        resolutions: &HashMap<String, Resolution>,
        _current_module: &str,
    ) -> Vec<String> {
        let mut imports: HashSet<String> = HashSet::new();

        for (symbol, resolution) in resolutions {
            if let Some(import) = resolution.to_import_statement(symbol) {
                imports.insert(import);
            }
        }

        let mut sorted: Vec<String> = imports.into_iter().collect();
        sorted.sort();
        sorted
    }

    /// Generate all imports including deduplication and conflict resolution
    pub fn generate_with_dedup(
        resolutions: &HashMap<String, Resolution>,
        existing_imports: &HashSet<String>,
    ) -> Vec<String> {
        let mut imports: HashSet<String> = HashSet::new();

        for (symbol, resolution) in resolutions {
            // Skip if already imported
            if existing_imports.contains(symbol) {
                continue;
            }

            if let Some(import) = resolution.to_import_statement(symbol) {
                imports.insert(import);
            }
        }

        let mut sorted: Vec<String> = imports.into_iter().collect();
        sorted.sort();
        sorted
    }
}

/// Unified API for symbol resolution workflow
///
/// This provides a single entry point that combines:
/// 1. Symbol collection from code
/// 2. Resolution against std/crate/external sources
/// 3. Import generation
pub struct SymbolWorkflow {
    resolver: SymbolResolver,
}

impl SymbolWorkflow {
    /// Create a new workflow with the given type sources
    pub fn new(
        std_types: HashMap<String, String>,
        crate_types: HashMap<String, String>,
        external_imports: HashMap<String, String>,
    ) -> Self {
        Self {
            resolver: SymbolResolver::new(std_types, crate_types, external_imports),
        }
    }

    /// Add crate-local types to avoid std import conflicts
    pub fn with_crate_local_types(mut self, types: HashSet<String>) -> Self {
        self.resolver = self.resolver.with_crate_local_types(types);
        self
    }

    /// Generate imports for the given code
    ///
    /// This is the main entry point that:
    /// 1. Collects all symbols from the code
    /// 2. Resolves each symbol to its source
    /// 3. Generates import statements
    ///
    /// Returns a sorted, deduplicated list of import statements
    pub fn generate_imports(
        &self,
        code: &str,
        current_module: &str,
        local_definitions: &HashSet<String>,
        existing_imports: &HashSet<String>,
    ) -> Vec<String> {
        // Step 1: Collect symbols
        let mut table = SymbolTable::new();
        table.collect_comprehensive(code, current_module);

        // Step 2: Resolve symbols
        let resolutions = self.resolver.resolve_all(&table, current_module, local_definitions);

        // Step 3: Generate imports (with dedup against existing)
        ImportGenerator::generate_with_dedup(&resolutions, existing_imports)
    }

    /// Generate imports and return detailed resolution info
    ///
    /// Useful for debugging and understanding what was resolved
    pub fn generate_imports_verbose(
        &self,
        code: &str,
        current_module: &str,
        local_definitions: &HashSet<String>,
        existing_imports: &HashSet<String>,
    ) -> (Vec<String>, HashMap<String, Resolution>) {
        let mut table = SymbolTable::new();
        table.collect_comprehensive(code, current_module);

        let resolutions = self.resolver.resolve_all(&table, current_module, local_definitions);
        let imports = ImportGenerator::generate_with_dedup(&resolutions, existing_imports);

        (imports, resolutions)
    }

    /// Get the underlying resolver for direct access
    pub fn resolver(&self) -> &SymbolResolver {
        &self.resolver
    }
}

/// Helper to create a workflow from the cached std types
pub fn create_workflow_from_cache(
    crate_types: HashMap<String, String>,
    external_imports: HashMap<String, String>,
) -> SymbolWorkflow {
    // Load std types from cache (same as semantic.rs does)
    let std_types = crate::old_slicer::semantic::load_std_types_cache();
    SymbolWorkflow::new(std_types, crate_types, external_imports)
}

/// Build crate types map from SCIP index
/// This maps type names to their module paths within the crate
pub fn build_crate_types_from_index(index: &crate::CrateIndex) -> HashMap<String, String> {
    let mut crate_types = HashMap::new();

    // Priority 7 Debug: Check for Visitor trait paths
    let debug_visitor = std::env::var("DEBUG_VISITOR").is_ok();

    for (name, items) in &index.items {
        // Priority 7 Debug: Trace Visitor trait resolution
        if debug_visitor && name == "Visitor" {
            eprintln!("[DEBUG_VISITOR] build_crate_types_from_index:");
            eprintln!("  Symbol: {}", name);
            eprintln!("  All items for this symbol:");
            for item in items {
                eprintln!("    - module_path: {}, is_pub: {}", item.module_path, item.is_pub);
            }
        }

        // Find the best public definition by preferring shorter module paths
        // This helps when a crate has multiple items with the same name
        // (e.g., `enums::Enum` trait vs `well_known_types::type_::Enum` struct)
        let best_item = items.iter()
            .filter(|item| item.is_pub && !item.module_path.is_empty())
            .min_by_key(|item| item.module_path.matches("::").count())
            .or_else(|| items.iter()
                .filter(|item| !item.module_path.is_empty())
                .min_by_key(|item| item.module_path.matches("::").count()))
            .or_else(|| items.first());

        if let Some(item) = best_item {
            // The module_path contains the module path within the crate
            // It may or may not start with "crate::" depending on the source
            let module_path = if item.module_path.starts_with("crate::") {
                // Strip "crate::" prefix
                item.module_path.strip_prefix("crate::").unwrap_or(&item.module_path).to_string()
            } else {
                // Use as-is (it's already relative to the crate root)
                item.module_path.clone()
            };

            if debug_visitor && name == "Visitor" {
                eprintln!("  Selected best_item module_path: {}", module_path);
            }

            // Priority 7: Filter out hir::visitor::Visitor to prevent wrong trait imports
            // Files should import ast::visitor::Visitor which has different method signatures
            if name == "Visitor" {
                debug_log!("Visitor found - module_path: '{}'", module_path);
                if module_path == "hir::visitor" {
                    eprintln!("[VISITOR FIX] Skipping hir::visitor::Visitor - will use ast::visitor::Visitor instead");
                    continue;
                } else {
                    eprintln!("[VISITOR FIX] Keeping {} Visitor", module_path);
                }
            }

            crate_types.insert(name.clone(), module_path);
        }
    }

    crate_types
}

/// Build external imports map from preserved use statements
pub fn build_external_imports(
    use_statements: &[crate::UseStatement],
) -> HashMap<String, String> {
    let mut external_imports = HashMap::new();

    for stmt in use_statements {
        if stmt.is_external {
            for symbol in &stmt.symbols {
                if symbol != "*" && symbol != "self" {
                    // Extract the import path for this symbol
                    // e.g., "use foo::bar::Baz;" -> symbol="Baz", path="foo::bar::Baz"
                    let path = if stmt.source.contains(&format!("::{}", symbol)) {
                        if stmt.source.trim().starts_with("use ") {
                            // Simple use statement - strip "use " and ";"
                            stmt.source
                                .trim()
                                .trim_start_matches("use ")
                                .trim_end_matches(';')
                                .to_string()
                        } else {
                            // Complex use statement (with attributes) - keep as is (strip ;)
                            stmt.source.trim_end_matches(';').to_string()
                        }
                    } else {
                        // Glob or complex import - use source as-is (strip ;)
                        stmt.source.trim_end_matches(';').to_string()
                    };
                    external_imports.insert(symbol.clone(), path);
                }
            }
        }
    }

    external_imports
}

/// Compare old and new import generation (for validation)
/// Returns (old_only, new_only, common)
pub fn compare_imports(
    old_imports: &[String],
    new_imports: &[String],
) -> (Vec<String>, Vec<String>, Vec<String>) {
    let old_set: HashSet<_> = old_imports.iter().cloned().collect();
    let new_set: HashSet<_> = new_imports.iter().cloned().collect();

    let common: Vec<_> = old_set.intersection(&new_set).cloned().collect();
    let old_only: Vec<_> = old_set.difference(&new_set).cloned().collect();
    let new_only: Vec<_> = new_set.difference(&old_set).cloned().collect();

    (old_only, new_only, common)
}

/// Merge imports from old and new systems (conservative approach)
/// Takes the union of both to ensure we don't miss anything
pub fn merge_imports(old_imports: &str, new_imports: &[String]) -> String {
    let mut all_imports: HashSet<String> = HashSet::new();

    // Add old imports
    for line in old_imports.lines() {
        let trimmed = line.trim();
        if !trimmed.is_empty() && trimmed.starts_with("use ") {
            all_imports.insert(trimmed.to_string());
        }
    }

    // Add new imports
    for import in new_imports {
        all_imports.insert(import.clone());
    }

    // Sort and join
    let mut sorted: Vec<_> = all_imports.into_iter().collect();
    sorted.sort();
    sorted.join("\n")
}

/// Extract symbol names from existing imports
/// Used to avoid generating duplicate imports
pub fn extract_imported_symbols(imports: &str) -> HashSet<String> {
    static USE_PATTERN: Lazy<Regex> = Lazy::new(|| {
        // Match: use path::to::symbol; or use path::to::{Symbol, other};
        // Include both uppercase (types) and lowercase (functions) identifiers
        // Use .+ (greedy) which backtracks to find the last :: before the symbol
        Regex::new(r"use\s+.+::([a-zA-Z_][a-zA-Z0-9_]*)\s*;").unwrap()
    });
    static BRACE_PATTERN: Lazy<Regex> = Lazy::new(|| {
        // Match symbols inside braces: {Symbol, Other, Another}
        Regex::new(r"\{([^}]+)\}").unwrap()
    });

    let mut symbols = HashSet::new();

    for cap in USE_PATTERN.captures_iter(imports) {
        if let Some(m) = cap.get(1) {
            symbols.insert(m.as_str().to_string());
        }
    }

    // Also extract from brace patterns
    for cap in BRACE_PATTERN.captures_iter(imports) {
        if let Some(m) = cap.get(1) {
            for part in m.as_str().split(',') {
                let name = part.trim().split_whitespace().next().unwrap_or("");
                // Include all valid identifiers (both types and functions)
                if !name.is_empty() && name.chars().next().map(|c| c.is_alphanumeric() || c == '_').unwrap_or(false) {
                    symbols.insert(name.to_string());
                }
            }
        }
    }

    symbols
}

/// Generate only the imports that are missing from existing imports
/// This is the gap-filling approach - don't duplicate, just add what's missing
pub fn fill_import_gaps(
    existing_imports: &str,
    new_imports: &[String],
) -> String {
    // Extract symbols already imported
    let existing_symbols = extract_imported_symbols(existing_imports);

    // Filter new imports to only those not already imported
    let mut gap_imports: Vec<String> = Vec::new();
    for import in new_imports {
        // Extract symbol name from import statement
        if let Some(symbol) = import.rsplit("::").next().map(|s| s.trim_end_matches(';').trim()) {
            if !existing_symbols.contains(symbol) {
                gap_imports.push(import.clone());
            }
        }
    }

    if gap_imports.is_empty() {
        existing_imports.to_string()
    } else {
        // Append new imports
        format!("{}\n{}", existing_imports, gap_imports.join("\n"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_kind_inference() {
        assert_eq!(SymbolKind::infer_from_name("PathBuf"), SymbolKind::Type);
        assert_eq!(SymbolKind::infer_from_name("Iterator"), SymbolKind::Type);
        assert_eq!(SymbolKind::infer_from_name("println!"), SymbolKind::Macro);
        assert_eq!(SymbolKind::infer_from_name("MAX_SIZE"), SymbolKind::Constant);
        assert_eq!(SymbolKind::infer_from_name("foo_bar"), SymbolKind::Function);
    }

    #[test]
    fn test_resolution_priority() {
        assert!(Resolution::Local.priority() < Resolution::Crate { path: "".to_string() }.priority());
        assert!(Resolution::Crate { path: "".to_string() }.priority() < Resolution::Std { path: "".to_string() }.priority());
        assert!(Resolution::Std { path: "".to_string() }.priority() < Resolution::External { path: "".to_string() }.priority());
    }

    #[test]
    fn test_resolution_to_import() {
        assert_eq!(Resolution::Local.to_import_statement("Foo"), None);

        let crate_res = Resolution::Crate { path: "util".to_string() };
        assert_eq!(crate_res.to_import_statement("Helper"), Some("use crate::util::Helper;".to_string()));

        let std_res = Resolution::Std { path: "std::path::PathBuf".to_string() };
        assert_eq!(std_res.to_import_statement("PathBuf"), Some("use std::path::PathBuf;".to_string()));
    }

    #[test]
    fn test_symbol_table_collection() {
        let mut table = SymbolTable::new();
        table.collect_from_code("fn foo(x: PathBuf) -> Option<String> {}", "");

        assert!(table.symbols().contains_key("PathBuf"));
        assert!(table.symbols().contains_key("Option"));
        assert!(table.symbols().contains_key("String"));
    }

    #[test]
    fn test_symbol_resolver() {
        let mut std_types = HashMap::new();
        std_types.insert("PathBuf".to_string(), "std::path::PathBuf".to_string());
        std_types.insert("String".to_string(), "std::string::String".to_string());

        let resolver = SymbolResolver::new(std_types, HashMap::new(), HashMap::new());

        let local_defs = HashSet::new();

        let res = resolver.resolve("PathBuf", "", &local_defs);
        assert_eq!(res, Resolution::Std { path: "std::path::PathBuf".to_string() });

        let res = resolver.resolve("UnknownType", "", &local_defs);
        assert_eq!(res, Resolution::Unknown);
    }

    #[test]
    fn test_to_import_statement_with_attributes() {
        let res = Resolution::External { 
            path: "#[cfg(not(crossbeam_no_atomic))] use core::sync::atomic::Ordering".to_string() 
        };
        let import = res.to_import_statement("Ordering").unwrap();
        // Currently this fails and returns "use #[cfg...;"
        assert_eq!(import, "#[cfg(not(crossbeam_no_atomic))] use core::sync::atomic::Ordering;");
    }
}

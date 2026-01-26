//! Build dependency graph (what items depend on what)
//!
//! This module reuses the existing parsing infrastructure to build a graph
//! of item dependencies, which can then be used for marking and deletion.

use std::path::Path;
use std::collections::{HashMap, HashSet};
use crate::types::{CrateIndex, ParsedItem};
use crate::old_slicer::parsing::parse_crate;
use crate::constants::RUST_KEYWORDS;

/// Dependency graph: item_name -> list of items it depends on
pub type Graph = HashMap<String, Vec<String>>;

/// Build dependency graph by parsing the crate
pub fn build_graph(output_dir: &Path, crate_name: &str) -> Result<(Graph, CrateIndex, HashSet<String>), String> {
    // Parse the crate using existing infrastructure
    let index = parse_crate(output_dir, crate_name);

    // Build the dependency graph
    let graph = build_graph_from_index(&index);

    // Extract public re-exports from lib.rs - these are part of the crate's public API
    // and should always be kept even if not directly used
    let public_reexports = extract_public_reexports(&index);

    Ok((graph, index, public_reexports))
}

/// Extract items that are publicly re-exported in lib.rs
/// These items are part of the crate's public API and should be kept
fn extract_public_reexports(index: &CrateIndex) -> HashSet<String> {
    use std::collections::HashSet;
    let mut reexports = HashSet::new();

    // Find use statements in the root module (lib.rs or main.rs)
    for use_stmt in &index.use_statements {
        if use_stmt.module_path.is_empty() {
            // This is a use statement in lib.rs
            // Check if it's a crate-internal re-export (use crate::, use self::)
            if use_stmt.source.contains("use crate::") || use_stmt.source.contains("use self::") {
                // Check if it's public (pub use)
                if use_stmt.source.trim().starts_with("pub use") {
                    // Add all symbols being re-exported
                    reexports.extend(use_stmt.symbols.iter().cloned());
                }
            }
        }
    }

    reexports
}

/// Extract dependencies from a parsed crate index
fn build_graph_from_index(index: &CrateIndex) -> Graph {
    let mut graph = HashMap::new();

    // Build a map of module_path -> imported names from use statements
    let mut module_imports: HashMap<String, Vec<String>> = HashMap::new();

    for use_stmt in &index.use_statements {
        // Use the symbols field which already contains the imported names
        // e.g., for "use crate::either::Either;" symbols = ["Either"]
        let module_path = use_stmt.module_path.clone();
        module_imports.entry(module_path).or_default().extend(use_stmt.symbols.iter().cloned());
    }

    for item in &index.all_items {
        // Use qualified names (module::name) to avoid collisions when multiple items
        // have the same name in different modules (e.g., multiple "Parser" items)
        let qualified_name = if item.module_path.is_empty() {
            item.name.clone()
        } else {
            format!("{}::{}", item.module_path, item.name)
        };

        let mut deps = extract_item_dependencies(item);

        // Add imports from use statements in the same module
        // This ensures that imported items become dependencies in the graph
        if let Some(imports) = module_imports.get(&item.module_path) {
            deps.extend(imports.iter().cloned());
        }

        graph.insert(qualified_name, deps);
    }

    graph
}

/// Extract what an item depends on (enhanced to include body-level references)
fn extract_item_dependencies(item: &ParsedItem) -> Vec<String> {
    let mut deps = HashSet::new();

    // Extract from item.dependencies (already computed by old parser - type-level deps)
    for dep in &item.dependencies {
        deps.insert(dep.clone());
    }

    // Also scan the item source for body-level references (constants, functions, types)
    // This is critical for catching private items used inside function bodies
    // e.g., LOG_LEVEL_NAMES used inside Level::from_str()
    let body_refs = extract_body_references(&item.source);
    for ref_name in body_refs {
        deps.insert(ref_name);
    }

    deps.into_iter().collect()
}

/// Extract identifier references from item source code
/// Catches both types (uppercase) and values (constants, functions)
fn extract_body_references(source: &str) -> HashSet<String> {
    let mut refs = HashSet::new();

    // Tokenize by non-identifier characters
    for word in source.split(|c: char| !c.is_alphanumeric() && c != '_') {
        let word = word.trim();

        // Skip empty, too short, keywords, or starting with digit
        if word.len() < 2 || word.starts_with(char::is_numeric) {
            continue;
        }

        // Skip Rust keywords
        if RUST_KEYWORDS.contains(&word) {
            continue;
        }

        // Skip primitive types
        if is_primitive_type(word) {
            continue;
        }

        // Include identifiers that look like:
        // - Constants: ALL_CAPS or MIXED_CAPS (e.g., LOG_LEVEL_NAMES, HEX_CHARS_LOWER)
        // - Types: PascalCase (e.g., BytesToHexChars, MaybeStaticStr)
        // - Functions: snake_case (e.g., set_times, encode_to_iter)
        let first_char = word.chars().next().unwrap();
        if first_char.is_alphabetic() {
            refs.insert(word.to_string());
        }
    }

    refs
}

/// Check if a type name is a primitive
fn is_primitive_type(name: &str) -> bool {
    matches!(name,
        "bool" | "char" | "str" | "String" |
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" |
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
        "f32" | "f64" |
        "Vec" | "Box" | "Option" | "Result" | "Some" | "None" | "Ok" | "Err" |
        "Self" | "self" | "true" | "false"
    )
}

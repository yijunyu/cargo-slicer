//! Core slicing logic - dependency computation.
//!
//! TODO: Migrate from original cargo_slicer.rs:
//! - group_items_by_file() (line 2846)
//! - compute_needed_items() (line 3616)
//! - expand_needed_transitively() (line 3701)

use std::collections::{HashMap, HashSet, BTreeSet};

use regex::Regex;
use once_cell::sync::Lazy;

use crate::types::{ParsedItem, CrateIndex, UsedItem};
use super::parsing::is_primitive_type;

/// Extract items referenced via `use crate::*` imports from source code.
/// Returns a set of module/item names that are imported from the crate root.
/// This includes both individual segments and full paths for transitive inclusion.
fn extract_crate_imports(source: &str) -> HashSet<String> {
    // Pattern matches full paths: use crate::a::b::c::Item;
    static CRATE_IMPORT_FULL: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"use\s+crate\s*::\s*([a-zA-Z_][a-zA-Z0-9_:]*[a-zA-Z0-9_])").unwrap()
    });

    // Also match items in braces: use crate::{A, B}
    static CRATE_BRACE_IMPORT: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"use\s+crate\s*::\s*\{([^}]+)\}").unwrap()
    });

    // Match module path before braces: use crate::a::b::{X, Y}
    static CRATE_PATH_BRACE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"use\s+crate\s*::\s*([a-zA-Z_][a-zA-Z0-9_:]*)\s*::\s*\{").unwrap()
    });

    let mut imports = HashSet::with_capacity(32);

    // Match full path imports: use crate::module::submodule::Item
    for cap in CRATE_IMPORT_FULL.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let full_path = m.as_str();
            // Skip if this is followed by ::{ (handled by CRATE_PATH_BRACE)
            if full_path.ends_with(':') {
                continue;
            }
            // Add all path segments and intermediate paths
            let parts: Vec<&str> = full_path.split("::").collect();
            for i in 1..=parts.len() {
                let path = parts[..i].join("::");
                // Only add lowercase segments (modules) and uppercase (types)
                let last = parts[i-1];
                if !last.is_empty() && last.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false) {
                    imports.insert(path);
                }
            }
        }
    }

    // Match path before braces: use crate::a::b::{X, Y}
    for cap in CRATE_PATH_BRACE.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let base_path = m.as_str();
            let parts: Vec<&str> = base_path.split("::").collect();
            for i in 1..=parts.len() {
                let path = parts[..i].join("::");
                imports.insert(path);
            }
        }
    }

    // Match brace imports: use crate::{A, B, C}
    for cap in CRATE_BRACE_IMPORT.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            for item in m.as_str().split(',') {
                let item = item.trim();
                // Handle nested paths like `module::Item`
                let parts: Vec<&str> = item.split("::").collect();
                for i in 1..=parts.len() {
                    let part = parts[i-1].trim();
                    if !part.is_empty() && part.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false) {
                        imports.insert(parts[..i].join("::"));
                    }
                }
            }
        }
    }

    imports
}

/// Extract type names used in source code (uppercase identifiers).
/// This helps detect types used in macros that don't have explicit imports.
pub fn extract_type_usages(source: &str) -> HashSet<String> {
    static TYPE_PATTERN: Lazy<Regex> = Lazy::new(|| {
        // Match uppercase identifiers that look like types
        // Skip common generic parameters (T, U, V, etc.) and lifetimes
        Regex::new(r"\b([A-Z][a-zA-Z0-9_]*)\b").unwrap()
    });

    // Common type parameters and keywords to skip
    let skip: HashSet<&str> = [
        "Self", "T", "U", "V", "W", "E", "F", "R", "S", "A", "B", "C", "D", "K",
        "Ok", "Err", "Some", "None", "Result", "Option", "Vec", "Box", "String",
        "Ref", "RefMut", "Cell", "RefCell", "Rc", "Arc", "Mutex", "RwLock",
        "Send", "Sync", "Sized", "Copy", "Clone", "Debug", "Display", "Default",
        "Iterator", "IntoIterator", "From", "Into", "AsRef", "AsMut", "Borrow",
        "Deref", "DerefMut", "Drop", "Fn", "FnMut", "FnOnce", "Future", "Stream",
        "Read", "Write", "Seek", "BufRead", "Ord", "PartialOrd", "Eq", "PartialEq",
        "Hash", "Hasher", "Add", "Sub", "Mul", "Div", "Rem", "Neg", "Not",
        "Index", "IndexMut", "Range", "RangeInclusive", "Bound",
        "CB", "CB1", "CB2", "Item", "Output", "Error", "Target", "Iter", "IntoIter",
    ].into_iter().collect();

    let mut types = HashSet::with_capacity(32);

    for cap in TYPE_PATTERN.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let type_name = m.as_str();
            // Skip common types and single letters
            if type_name.len() > 1 && !skip.contains(type_name) {
                types.insert(type_name.to_string());
            }
        }
    }

    types
}

/// Extract function call names from source code (lowercase identifiers followed by `(`).
/// This catches function calls that TypeRefVisitor might miss (e.g., inside closures in macros).
pub fn extract_function_calls(source: &str) -> HashSet<String> {
    static FUNC_CALL_PATTERN: Lazy<Regex> = Lazy::new(|| {
        // Match lowercase identifiers followed by ( that look like function calls
        // Exclude common patterns like `if (`, `for (`, etc.
        Regex::new(r"\b([a-z_][a-z0-9_]*)\s*\(").unwrap()
    });

    // Keywords and common patterns to skip
    let skip: HashSet<&str> = [
        "if", "for", "while", "match", "loop", "fn", "let", "mut", "ref", "pub",
        "use", "mod", "struct", "enum", "trait", "impl", "type", "const", "static",
        "where", "as", "in", "return", "break", "continue", "move", "async", "await",
        "unsafe", "dyn", "self", "super", "crate", "extern", "true", "false",
        // Common std/prelude functions
        "vec", "format", "println", "eprintln", "print", "eprint", "write", "writeln",
        "panic", "unreachable", "todo", "unimplemented", "assert", "assert_eq", "assert_ne",
        "debug_assert", "debug_assert_eq", "debug_assert_ne", "cfg", "cfg_attr",
        "include", "include_str", "include_bytes", "env", "option_env", "concat",
        "stringify", "line", "column", "file", "module_path", "compile_error", "matches",
        "dbg", "try", "ok", "err", "some", "none", "default", "clone", "drop",
        "new", "with_capacity", "push", "pop", "get", "set", "insert", "remove",
        "len", "is_empty", "iter", "into_iter", "map", "filter", "collect", "unwrap",
        "expect", "ok_or", "ok_or_else", "map_err", "and_then", "or_else",
    ].into_iter().collect();

    let mut functions = HashSet::with_capacity(32);

    for cap in FUNC_CALL_PATTERN.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let func_name = m.as_str();
            // Skip keywords and common functions
            if func_name.len() > 2 && !skip.contains(func_name) {
                functions.insert(func_name.to_string());
            }
        }
    }

    functions
}

/// Extract constant usages from source code (UPPER_CASE identifiers).
/// This catches constants like `LITTLE_ENDIAN`, `MAX_SIZE`, etc.
pub fn extract_constant_usages(source: &str) -> HashSet<String> {
    static CONST_PATTERN: Lazy<Regex> = Lazy::new(|| {
        // Match UPPER_CASE identifiers (at least 2 chars with underscore or all caps)
        Regex::new(r"\b([A-Z][A-Z0-9_]+)\b").unwrap()
    });

    // Common constants and keywords to skip
    let skip: HashSet<&str> = [
        // Common Rust constants
        "OK", "ERR", "SOME", "NONE", "TRUE", "FALSE",
        // Common type-like patterns
        "ID", "OK", "IO", "OS", "UI", "DB", "API", "URL", "URI", "XML", "JSON",
        "HTML", "CSS", "SQL", "TCP", "UDP", "HTTP", "HTTPS", "FTP", "SSH",
        // Single letters and very short
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
        "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    ].into_iter().collect();

    let mut constants = HashSet::with_capacity(16);

    for cap in CONST_PATTERN.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let const_name = m.as_str();
            // Must have at least one underscore or be 3+ chars
            if !skip.contains(const_name) &&
               (const_name.contains('_') || const_name.len() >= 3) {
                constants.insert(const_name.to_string());
            }
        }
    }

    constants
}

/// Group items by the file they came from
pub fn group_items_by_file<'a>(
    items: impl Iterator<Item = &'a ParsedItem>,
) -> HashMap<String, Vec<&'a ParsedItem>> {
    let mut by_file: HashMap<String, Vec<&ParsedItem>> = HashMap::new();
    for item in items {
        let key = item.file.to_string_lossy().to_string();
        by_file.entry(key).or_default().push(item);
    }
    by_file
}

/// Compute which items are needed based on usage
pub fn compute_needed_items(
    used: &HashSet<UsedItem>,
    index: &CrateIndex,
    crate_name: &str,
) -> BTreeSet<String> {
    let mut needed = BTreeSet::new();
    let crate_prefix = format!("{}::", crate_name.replace('-', "_"));

    for item in used {
        let path = &item.path;
        if path.starts_with(&crate_prefix) {
            let relative = &path[crate_prefix.len()..];
            let parts: Vec<&str> = relative.split("::").collect();

            for part in &parts {
                if !is_primitive_type(part) {
                    needed.insert(part.to_string());
                }
            }
        }
    }

    let needed = expand_needed_transitively(&needed, index);

    // Phase 6.3: Apply type closure to include types referenced in signatures
    compute_type_closure(&needed, index)
}

/// Sort items topologically by dependency count.
///
/// Strategy: Process items with fewest dependencies first (leaf-first ordering).
/// This minimizes the transitive closure by preferring minimal definitions.
///
/// For example, if we need type `Error`:
/// - Option A: `pub struct Error { ... }` (0 dependencies)
/// - Option B: `pub use other_crate::Error;` (pulls in entire crate)
/// Processing A first means we get the minimal definition.
fn topological_sort(items: &BTreeSet<String>, index: &CrateIndex) -> Vec<String> {
    // Build dependency count map
    let mut dep_counts: HashMap<String, usize> = HashMap::with_capacity(items.len());

    for name in items {
        let all_items = index.get_all(name);
        if all_items.is_empty() {
            // If not found, assume it's a module with 0 deps (prioritize)
            dep_counts.insert(name.clone(), 0);
        } else {
            // Count total dependencies across all definitions of this item
            let total_deps: usize = all_items.iter()
                .map(|item| item.dependencies.len())
                .min()  // Use minimum (prefer definition with fewest deps)
                .unwrap_or(0);
            dep_counts.insert(name.clone(), total_deps);
        }
    }

    // Sort by dependency count (ascending), then by name for determinism
    let mut sorted: Vec<String> = items.iter().cloned().collect();
    sorted.sort_by(|a, b| {
        let count_a = dep_counts.get(a).unwrap_or(&999999);
        let count_b = dep_counts.get(b).unwrap_or(&999999);
        count_a.cmp(count_b)
            .then_with(|| a.cmp(b))  // Alphabetical tiebreaker for determinism
    });

    sorted
}

/// Expand needed items to include all transitive dependencies.
///
/// This function performs two types of expansion:
/// 1. SCIP-based dependencies (item.dependencies)
/// 2. Import-driven extraction: scan source for `use crate::*` imports
///
/// Uses topological ordering (fewest dependencies first) to minimize
/// the transitive closure and produce smaller, more optimal slices.
pub fn expand_needed_transitively(
    initial: &BTreeSet<String>,
    index: &CrateIndex,
) -> BTreeSet<String> {
    // Use BTreeSet for deterministic iteration
    let mut needed: BTreeSet<String> = initial.iter().cloned().collect();

    // Use topological ordering: process items with fewest dependencies first
    // This minimizes transitive closure and produces smaller slices
    let mut worklist: Vec<String> = topological_sort(initial, index);

    while let Some(name) = worklist.pop() {
        let items = index.get_all(&name);

        // If item not found directly, look for containing modules
        if items.is_empty() {
            for containing_mod in index.find_containing_modules(&name) {
                let mod_name = &containing_mod.name;
                if !needed.contains(mod_name) {
                    needed.insert(mod_name.clone());
                    worklist.push(mod_name.clone());
                }
            }
        }

        for item in items {
            // Process SCIP-based dependencies
            for dep in &item.dependencies {
                if !is_primitive_type(dep) && !needed.contains(dep) {
                    needed.insert(dep.clone());
                    worklist.push(dep.clone());
                }
            }

            // Import-driven extraction: scan source for `use crate::*` imports
            // This catches items that SCIP might miss (e.g., inside macros, private helpers)
            for imported in extract_crate_imports(&item.source) {
                if !is_primitive_type(&imported) && !needed.contains(&imported) {
                    // For paths like "reflect::repeated::vec_downcast", check the last segment
                    let check_name = imported.rsplit("::").next().unwrap_or(&imported);
                    // Check if this import target exists in the index
                    if !index.get_all(check_name).is_empty() || !index.find_containing_modules(check_name).is_empty() {
                        // Also add intermediate modules to needed set
                        let parts: Vec<&str> = imported.split("::").collect();
                        for i in 1..parts.len() {
                            let intermediate = parts[..i].join("::");
                            if !needed.contains(&intermediate) {
                                needed.insert(intermediate.clone());
                                worklist.push(intermediate);
                            }
                        }
                        needed.insert(imported.clone());
                        worklist.push(imported);
                    }
                }
            }

            // Type-usage extraction: scan source for type names (uppercase identifiers)
            // This catches types used in macros that don't have explicit imports
            for type_name in extract_type_usages(&item.source) {
                if !needed.contains(&type_name) {
                    // Check if this type exists in the index
                    if !index.get_all(&type_name).is_empty() {
                        needed.insert(type_name.clone());
                        worklist.push(type_name);
                    }
                }
            }

            // Function-call extraction: scan source for function calls (lowercase identifiers + `(`)
            // This catches function calls that TypeRefVisitor might miss (e.g., in closures)
            for func_name in extract_function_calls(&item.source) {
                if !needed.contains(&func_name) {
                    // Check if this function exists in the index
                    if !index.get_all(&func_name).is_empty() {
                        needed.insert(func_name.clone());
                        worklist.push(func_name);
                    }
                }
            }

            // Constant-usage extraction: scan source for UPPER_CASE identifiers
            // This catches constants like LITTLE_ENDIAN, MAX_SIZE, etc.
            for const_name in extract_constant_usages(&item.source) {
                if !needed.contains(&const_name) {
                    // Check if this constant exists in the index
                    if !index.get_all(&const_name).is_empty() {
                        needed.insert(const_name.clone());
                        worklist.push(const_name);
                    }
                }
            }
        }

        // Also include impl blocks
        for impl_item in index.get_impls(&name) {
            for dep in &impl_item.dependencies {
                if !is_primitive_type(dep) && !needed.contains(dep) {
                    needed.insert(dep.clone());
                    worklist.push(dep.clone());
                }
            }

            // Import-driven extraction for impl blocks too
            for imported in extract_crate_imports(&impl_item.source) {
                if !is_primitive_type(&imported) && !needed.contains(&imported) {
                    let check_name = imported.rsplit("::").next().unwrap_or(&imported);
                    if !index.get_all(check_name).is_empty() || !index.find_containing_modules(check_name).is_empty() {
                        // Also add intermediate modules
                        let parts: Vec<&str> = imported.split("::").collect();
                        for i in 1..parts.len() {
                            let intermediate = parts[..i].join("::");
                            if !needed.contains(&intermediate) {
                                needed.insert(intermediate.clone());
                                worklist.push(intermediate);
                            }
                        }
                        needed.insert(imported.clone());
                        worklist.push(imported);
                    }
                }
            }

            // Type-usage extraction for impl blocks
            for type_name in extract_type_usages(&impl_item.source) {
                if !needed.contains(&type_name) {
                    if !index.get_all(&type_name).is_empty() {
                        needed.insert(type_name.clone());
                        worklist.push(type_name);
                    }
                }
            }

            // Function-call extraction for impl blocks
            for func_name in extract_function_calls(&impl_item.source) {
                if !needed.contains(&func_name) {
                    if !index.get_all(&func_name).is_empty() {
                        needed.insert(func_name.clone());
                        worklist.push(func_name);
                    }
                }
            }

            // Constant-usage extraction for impl blocks
            for const_name in extract_constant_usages(&impl_item.source) {
                if !needed.contains(&const_name) {
                    if !index.get_all(&const_name).is_empty() {
                        needed.insert(const_name.clone());
                        worklist.push(const_name);
                    }
                }
            }
        }
    }

    // CRITICAL FIX: Recursive final pass to ensure ALL dependencies are included
    // This fixes cases where items added through source scanning might not have had
    // their SCIP-based dependencies fully followed. Keep adding until no new deps found.
    let mut iteration = 0;
    loop {
        iteration += 1;
        let mut final_additions = HashSet::new();

        for name in &needed {
            let items = index.get_all(name);
            for item in items {
                for dep in &item.dependencies {
                    if !is_primitive_type(dep) && !needed.contains(dep) && !final_additions.contains(dep) {
                        final_additions.insert(dep.clone());
                    }
                }
            }
            for impl_item in index.get_impls(name) {
                for dep in &impl_item.dependencies {
                    if !is_primitive_type(dep) && !needed.contains(dep) && !final_additions.contains(dep) {
                        final_additions.insert(dep.clone());
                    }
                }
            }
        }

        // If no new dependencies found, we're done
        if final_additions.is_empty() {
            break;
        }

        needed.extend(final_additions);

        // Safety limit to prevent infinite loops
        if iteration >= 10 {
            break;
        }
    }

    needed
}

/// Phase 6.3: Compute transitive type closure from typed dependencies
///
/// This function expands the set of needed items by following required type dependencies.
/// Unlike expand_needed_transitively which follows all dependencies, this specifically
/// follows type dependencies that are marked as required (e.g., function return types,
/// parameters, struct fields) but not optional ones (e.g., usage-only dependencies).
///
/// # Arguments
/// * `initial` - The initial set of needed item names
/// * `index` - The crate index containing all parsed items
///
/// # Returns
/// An expanded set of item names including all transitively required types
pub fn compute_type_closure(
    initial: &BTreeSet<String>,
    index: &CrateIndex,
) -> BTreeSet<String> {
    let _initial_count = initial.len();
    let mut closure: BTreeSet<String> = initial.iter().cloned().collect();
    let mut worklist: Vec<String> = initial.iter().cloned().collect();

    let mut iterations = 0;
    let mut _total_typed_deps_found = 0;
    let mut _already_in_closure_count = 0;
    let mut example_types_in_closure: Vec<String> = Vec::new();

    while let Some(item_name) = worklist.pop() {
        // Safety limit to prevent infinite loops
        iterations += 1;
        if iterations > 10000 {
            eprintln!("Warning: compute_type_closure reached iteration limit");
            break;
        }

        // Get all variants of this item (different cfg variants)
        let items = index.get_all(&item_name);

        for item in items {
            // Process typed dependencies with context
            _total_typed_deps_found += item.typed_dependencies.len();
            for type_dep in &item.typed_dependencies {
                // Only follow required dependencies
                if !type_dep.required {
                    continue;
                }

                let typename = &type_dep.typename;

                // Skip if already in closure or is primitive
                if closure.contains(typename) {
                    _already_in_closure_count += 1;
                    if example_types_in_closure.len() < 10 {
                        example_types_in_closure.push(typename.clone());
                    }
                    continue;
                }

                if is_primitive_type(typename) {
                    continue;
                }

                // Check if this type exists in the index
                if !index.get_all(typename).is_empty() {
                    closure.insert(typename.clone());
                    worklist.push(typename.clone());
                }
            }
        }

        // Also process impl blocks for this item
        for impl_item in index.get_impls(&item_name) {
            for type_dep in &impl_item.typed_dependencies {
                if !type_dep.required {
                    continue;
                }

                let typename = &type_dep.typename;

                if closure.contains(typename) || is_primitive_type(typename) {
                    continue;
                }

                if !index.get_all(typename).is_empty() {
                    closure.insert(typename.clone());
                    worklist.push(typename.clone());
                }
            }
        }
    }

    closure
}

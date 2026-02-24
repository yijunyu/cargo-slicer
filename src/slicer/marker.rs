//! Mark items as used (graph traversal + heuristic rules)
//!
//! Uses BFS traversal to mark all transitively used items starting from
//! entry points (items directly used by the project).
//!
//! Three-phase marking:
//! 1. Mark items reachable from project entry points
//! 2. Mark dependencies of ALL items (since public items are always kept)
//! 3. Mark module siblings and parent modules (module cohesion)
//!
//! This ensures private helpers called by public functions aren't deleted,
//! and that module-level dependencies are preserved.

use std::collections::{HashSet, HashMap, VecDeque};
use super::config::SlicerConfig;
use super::dependency_graph::Graph;

/// Mark all transitively used items via BFS traversal
///
/// This marks items as "used" if they are:
/// 1. Directly used by the project (entry_points)
/// 2. Transitively depended on by any entry point
/// 3. A DEPENDENCY of any item in the graph (since public items are always kept)
///
/// The key insight is that public items are always kept by the deleter,
/// so we must also mark their dependencies as used.
pub fn mark_used_items(
    graph: &Graph,
    entry_points: &HashSet<String>,
    public_reexports: &HashSet<String>,
    config: &SlicerConfig,
) -> Result<HashSet<String>, String> {
    // Phase 1: Mark all items that are DEPENDENCIES of at least one other item.
    // These items cannot be deleted because something depends on them.
    // Note: We don't mark items themselves as used - only their dependencies.
    // This way, truly orphaned items (those that nothing depends on) are NOT in `used`.
    let mut used = HashSet::new();

    // Collect all dependencies from all items in the graph
    for deps in graph.values() {
        for dep in deps {
            used.insert(dep.clone());
            // Also add resolved qualified names
            for key in graph.keys() {
                if key.ends_with(&format!("::{}", dep)) {
                    used.insert(key.clone());
                }
            }
        }
    }

    // Phase 2: Also mark items reachable from entry points via BFS
    let mut queue = VecDeque::new();

    // Combine entry points with public re-exports
    let mut all_entry_points = entry_points.clone();
    all_entry_points.extend(public_reexports.iter().cloned());

    // Initialize with entry points - expand simple names to qualified names
    // Prefer exact matches to avoid false positives with duplicate names
    for entry_point in &all_entry_points {
        // First, check for an exact match (highest priority)
        if graph.contains_key(entry_point) {
            used.insert(entry_point.clone());
            queue.push_back(entry_point.clone());
            continue;
        }

        // Second, check if entry_point is a qualified path that matches a graph key
        // e.g., "ast::Parser" matches graph key "ast::Parser"
        let mut found_exact = false;
        for key in graph.keys() {
            if key == entry_point {
                used.insert(key.clone());
                queue.push_back(key.clone());
                found_exact = true;
            }
        }

        // Only fall back to suffix matching if no exact match found
        // This prevents "Parser" from matching both "ast::Parser" and "hir::Parser"
        // when we have the qualified "ast::Parser" in entry_points
        if !found_exact {
            for key in graph.keys() {
                if key.ends_with(&format!("::{}", entry_point)) {
                    used.insert(key.clone());
                    queue.push_back(key.clone());
                }
            }
        }
    }

    // BFS traversal to find all transitively used items
    while let Some(current) = queue.pop_front() {
        // Get dependencies of current item
        if let Some(deps) = graph.get(&current) {
            for dep in deps {
                // Try to resolve dependency - it might be a simple name that needs qualification
                let mut resolved_deps = Vec::new();

                // Check if dep is already in used (might be qualified)
                if used.contains(dep) {
                    continue;
                }

                // Find all graph keys that match this dependency
                for key in graph.keys() {
                    if key == dep || key.ends_with(&format!("::{}", dep)) {
                        resolved_deps.push(key.clone());
                    }
                }

                // If no qualified match found, use the dependency as-is
                if resolved_deps.is_empty() {
                    resolved_deps.push(dep.clone());
                }

                for resolved in resolved_deps {
                    if !used.contains(&resolved) {
                        // Apply heuristic rules to determine if we should include this dependency
                        if should_include_dependency(&current, &resolved, config) {
                            used.insert(resolved.clone());
                            queue.push_back(resolved);
                        }
                    }
                }
            }
        }
    }

    // Phase 3: Module cohesion - when an item in a module is used, also mark
    // sibling items and parent modules. This ensures module-level dependencies
    // are preserved (e.g., shared helpers used by items in the same module).
    let used_with_module_cohesion = apply_module_cohesion(graph, &used, config);

    // Return both qualified names and simple names for compatibility with deleter
    let mut result = used_with_module_cohesion.clone();
    for qualified in &used_with_module_cohesion {
        // Extract simple name from qualified name (last component after ::)
        if let Some(simple_name) = qualified.split("::").last() {
            result.insert(simple_name.to_string());
        }
    }

    Ok(result)
}

/// Apply module cohesion: when items from a module are used, also mark sibling items
/// This handles crates with deep module dependencies where internal modules depend on each other
fn apply_module_cohesion(
    graph: &Graph,
    used: &HashSet<String>,
    _config: &SlicerConfig,
) -> HashSet<String> {
    let mut result = used.clone();

    // Build a map of module_path -> items in that module
    let mut module_items: HashMap<String, Vec<String>> = HashMap::new();
    for key in graph.keys() {
        let module_path = extract_module_path(key);
        module_items.entry(module_path).or_default().push(key.clone());
    }

    // Find all modules that have at least one used item
    let mut used_modules: HashSet<String> = HashSet::new();
    for item in used {
        let module_path = extract_module_path(item);
        if !module_path.is_empty() {
            used_modules.insert(module_path.clone());
            // Also mark all parent modules
            let mut parent = module_path.as_str();
            while let Some(pos) = parent.rfind("::") {
                parent = &parent[..pos];
                used_modules.insert(parent.to_string());
            }
        }
    }

    // For each used module, mark all items in that module
    // This ensures sibling items (internal helpers) are preserved
    for module_path in &used_modules {
        if let Some(items) = module_items.get(module_path) {
            for item in items {
                result.insert(item.clone());
            }
        }
    }

    // Also mark items whose module is a prefix of a used module
    // This handles cases like: validation::shared is used because validation::owned::X needs it
    for (module_path, items) in &module_items {
        for used_mod in &used_modules {
            // If used_mod starts with module_path (e.g., "validation::owned" starts with "validation")
            // then items in module_path might be needed
            if used_mod.starts_with(&format!("{}::", module_path)) || used_mod == module_path {
                for item in items {
                    result.insert(item.clone());
                }
            }
        }
    }

    result
}

/// Extract the module path from a qualified item name
/// e.g., "validation::owned::Parser" -> "validation::owned"
/// e.g., "Parser" -> ""
fn extract_module_path(qualified_name: &str) -> String {
    if let Some(pos) = qualified_name.rfind("::") {
        qualified_name[..pos].to_string()
    } else {
        String::new()
    }
}

/// Heuristic rules for whether to include a dependency
fn should_include_dependency(
    _from_item: &str,
    _to_item: &str,
    config: &SlicerConfig,
) -> bool {
    // For now, be conservative and include all dependencies
    // The rules in config.rules can control this later

    // Always include type dependencies if configured
    if config.rules.keep_type_dependencies {
        return true;
    }

    // Default: include everything (conservative approach)
    true
}

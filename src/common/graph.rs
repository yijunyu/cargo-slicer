//! Common dependency graph structures

use std::collections::HashMap;

/// Dependency graph: maps item name to its dependencies
pub type DependencyGraph = HashMap<String, Vec<String>>;

/// Build empty graph
pub fn new_graph() -> DependencyGraph {
    HashMap::new()
}

/// Add dependency: item depends on dep
pub fn add_dependency(graph: &mut DependencyGraph, item: String, dep: String) {
    graph.entry(item).or_insert_with(Vec::new).push(dep);
}

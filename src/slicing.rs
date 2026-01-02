//! Core slicing logic - dependency computation.
//!
//! TODO: Migrate from original cargo_slicer.rs:
//! - group_items_by_file() (line 2846)
//! - compute_needed_items() (line 3616)
//! - expand_needed_transitively() (line 3701)

use std::collections::{BTreeMap, HashSet};

use super::types::{ParsedItem, CrateIndex, UsedItem};
use super::parsing::is_primitive_type;

/// Group items by the file they came from
pub fn group_items_by_file<'a>(
    items: impl Iterator<Item = &'a ParsedItem>,
) -> BTreeMap<String, Vec<&'a ParsedItem>> {
    let mut by_file: BTreeMap<String, Vec<&ParsedItem>> = BTreeMap::new();
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
) -> HashSet<String> {
    let mut needed = HashSet::new();
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

    expand_needed_transitively(&needed, index)
}

/// Expand needed items to include all transitive dependencies
pub fn expand_needed_transitively(
    initial: &HashSet<String>,
    index: &CrateIndex,
) -> HashSet<String> {
    let mut needed = initial.clone();
    let mut worklist: Vec<String> = initial.iter().cloned().collect();

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
            for dep in &item.dependencies {
                if !is_primitive_type(dep) && !needed.contains(dep) {
                    needed.insert(dep.clone());
                    worklist.push(dep.clone());
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
        }
    }

    needed
}

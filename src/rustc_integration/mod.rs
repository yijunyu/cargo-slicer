//! Rustc driver integration for cargo-slicer
//!
//! This module provides integration with rustc's compiler driver to collect
//! usage information directly during compilation.

#![cfg(feature = "rustc-driver")]
#![allow(unused_imports)]

pub mod usage_collector;
pub mod ast_visitor;
pub mod hir_visitor;
pub mod slicer_integration;
pub mod virtual_slicer;
pub mod debug_log;

// Re-export for convenience
pub use hir_visitor::collect_from_hir;
pub use slicer_integration::analyze_crate_with_rustc_driver;
pub use virtual_slicer::{store_marked_items, is_item_marked, is_item_marked_by_def_id, clear_marked_items, MarkedItemsInfo, should_codegen_item, is_safe_to_skip, skip_reason, init_call_graph, record_call_edge, get_call_graph, record_vtable_trait, has_vtable_constructions, vtable_trait_count, increment_stub_count, get_stub_count};

use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};

/// Information collected about items defined in a crate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItemInfo {
    /// Fully qualified name of the item
    pub path: String,
    /// Kind of item (function, type, const, etc.)
    pub kind: ItemKind,
    /// Visibility (pub, pub(crate), private)
    pub visibility: Visibility,
    /// Source location (file:line:col)
    pub location: String,
}

/// Kind of Rust item
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ItemKind {
    Function,
    Type,
    Trait,
    Impl,
    Const,
    Static,
    Macro,
    Mod,
}

/// Visibility level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    PublicCrate,
    Private,
}

/// Usage data collected from compilation
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct UsageData {
    /// Crate name being analyzed
    pub crate_name: String,

    /// All items defined in this crate
    pub defined_items: HashMap<String, ItemInfo>,

    /// Items referenced (used) from this crate
    /// Maps item path -> set of locations where it's referenced
    pub referenced_items: HashMap<String, HashSet<String>>,

    /// Dependencies between items within the crate
    /// Maps item -> set of items it depends on
    pub item_dependencies: HashMap<String, HashSet<String>>,
}

impl UsageData {
    pub fn new(crate_name: String) -> Self {
        Self {
            crate_name,
            defined_items: HashMap::new(),
            referenced_items: HashMap::new(),
            item_dependencies: HashMap::new(),
        }
    }

    /// Add a defined item
    pub fn add_item(&mut self, path: String, info: ItemInfo) {
        self.defined_items.insert(path, info);
    }

    /// Record a reference to an item
    pub fn add_reference(&mut self, item_path: String, ref_location: String) {
        self.referenced_items
            .entry(item_path)
            .or_insert_with(HashSet::new)
            .insert(ref_location);
    }

    /// Record a dependency between two items
    pub fn add_dependency(&mut self, from_item: String, to_item: String) {
        self.item_dependencies
            .entry(from_item)
            .or_insert_with(HashSet::new)
            .insert(to_item);
    }

    /// Write usage data to a JSON file
    pub fn write_to_file(&self, path: &std::path::Path) -> std::io::Result<()> {
        let json = serde_json::to_string_pretty(self)?;
        std::fs::write(path, json)
    }

    /// Read usage data from a JSON file
    pub fn read_from_file(path: &std::path::Path) -> std::io::Result<Self> {
        let json = std::fs::read_to_string(path)?;
        serde_json::from_str(&json)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
    }
}

//! Cargo Slicer - Extract minimal subsets of Rust crates.
//!
//! This module provides functionality to analyze crate usage and generate
//! sliced versions containing only the items actually used.

#![allow(dead_code)]  // Many helper functions may be useful later

// Module declarations
pub mod types;
pub mod constants;
pub mod cfg;
pub mod arch;
pub mod usage;
pub mod crate_info;
pub mod parsing;
pub mod slicing;
pub mod codegen;
pub mod semantic;
pub mod source_fix;
pub mod auto_fix;
pub mod cargo_toml;
pub mod imports;
pub mod dep_graph;
pub mod slice_all;
pub mod symbol;
pub mod measure;

// Re-exports for public API
pub use types::*;
pub use constants::*;
pub use crate_info::find_crate_source;
pub use usage::analyze_crate_usage;
pub use parsing::parse_crate;
pub use semantic::{generate_semantic_sliced_crate, generate_standalone_sliced_crate};
pub use dep_graph::analyze_dependency_graph;
pub use slice_all::{union_slice_deps, slice_all_deps, generate_workspace_toml, copy_original_sources, copy_dir_recursive};
pub use measure::*;

//! Cargo Slicer - Extract minimal subsets of Rust crates.
//!
//! This module provides functionality to analyze crate usage and generate
//! sliced versions containing only the items actually used.

#![allow(dead_code)]  // Many helper functions may be useful later

// Common utilities (shared by both old and new slicer)
pub mod common;

// Core modules (used by both slicers)
pub mod types;
pub mod constants;
pub mod usage;
pub mod cargo_toml;
pub mod dep_graph;
pub mod slice_all;
pub mod measure;
pub mod cycle_detector;
pub mod cycle_breaker;
pub mod variant_generator;
pub mod build_handler;
pub mod debug_log;
pub mod profiling;
pub mod watch;

// Old slicer implementation (generation-based, causes LOC bloat)
// Note: parsing.rs is always compiled (used by new slicer for AST parsing)
// SCIP-dependent functions in semantic.rs are gated by scip-analysis feature
pub mod old_slicer;

// New slicer implementation (copy-and-delete, guarantees reduction)
pub mod slicer;

// Re-exports for public API
pub use types::*;
pub use constants::*;
pub use usage::analyze_crate_usage;
pub use dep_graph::analyze_dependency_graph;
pub use slice_all::{union_slice_deps, generate_workspace_toml, copy_original_sources, copy_dir_recursive};
pub use measure::*;

// Common utilities re-exports
pub use common::source_location::find_crate_source;
pub use common::cfg_eval;
pub use common::arch;

// Old slicer re-exports (for backwards compatibility)
pub use old_slicer::parsing::parse_crate;
#[cfg(feature = "scip-analysis")]
pub use old_slicer::semantic::{generate_semantic_sliced_crate, generate_standalone_sliced_crate};

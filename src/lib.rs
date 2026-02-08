//! Cargo Slicer - Extract minimal subsets of Rust crates.
//!
//! This module provides functionality to analyze crate usage and generate
//! sliced versions containing only the items actually used.

#![allow(dead_code)]  // Many helper functions may be useful later
#![cfg_attr(feature = "rustc-driver", feature(rustc_private))]

// Rustc crates for driver integration
#[cfg(feature = "rustc-driver")]
extern crate rustc_driver;
#[cfg(feature = "rustc-driver")]
extern crate rustc_hir;
#[cfg(feature = "rustc-driver")]
extern crate rustc_interface;
#[cfg(feature = "rustc-driver")]
extern crate rustc_middle;
#[cfg(feature = "rustc-driver")]
extern crate rustc_span;

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
#[cfg(feature = "local-registry")]
pub mod local_registry;

// ctags FFI bindings and tag collection (experimental)
// Commented out: requires libc dependency which is not declared
// #[allow(dead_code)]
// pub mod ctags_ffi;
pub mod ctags_collector;

// Pure-Rust ctags-style parser (Phase 1 MVP)
pub mod ctags_rs;

pub mod unparse;

// Old slicer implementation (generation-based, causes LOC bloat)
// Note: parsing.rs is always compiled (used by new slicer for AST parsing)
// SCIP-dependent functions in semantic.rs are gated by scip-analysis feature
pub mod old_slicer;

// New slicer implementation (copy-and-delete, guarantees reduction)
pub mod slicer;

// Rustc driver integration (experimental, nightly-only)
#[cfg(feature = "rustc-driver")]
pub mod rustc_integration;

// Cross-crate BFS for workspace-wide virtual slicing
pub mod cross_crate_bfs;

// Pre-build static analysis for cross-crate virtual slicing
pub mod pre_analyze;

// Rustc driver subprocess invocation (no rustc_private linking)
pub mod rustc_subprocess;

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

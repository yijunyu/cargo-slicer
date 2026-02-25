//! Old slicer implementation (generation-based approach)
//!
//! This module contains the original slicer implementation that:
//! - Parses crates with syn
//! - Analyzes with SCIP
//! - Generates code from scratch
//! - Adds imports (often too many, causing LOC bloat)
//!
//! **Problem**: This approach tends to INCREASE code size instead of reducing it.
//! The new slicer in `src/slicer/` uses a copy-and-delete approach instead.
//!
//! This is kept for:
//! - Comparison testing
//! - Fallback if new method has issues
//! - Reference for rule development

// Re-export old implementation modules
pub mod semantic;
pub mod codegen;
pub mod parsing;
pub mod slicing;
pub mod symbol;
pub mod imports;
pub mod import_analyzer;
pub mod import_decision_log;
pub mod dependency_decision_log;
pub mod post_slice_fixer;
pub mod source_fix;
pub mod auto_fix;

// Re-export main entry points
pub use codegen::*;
pub use parsing::*;
pub use slicing::*;

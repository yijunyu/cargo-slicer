//! Common utilities shared between old_slicer and new slicer
//!
//! This module contains code that both implementations need:
//! - AST parsing with syn
//! - Cfg evaluation
//! - Architecture detection
//! - File operations
//! - Dependency graph structures
//! - Item visitor patterns
//!
//! Benefits:
//! - Reduce code duplication
//! - Easier to maintain both implementations
//! - Clear separation: shared utilities vs implementation logic
//! - Enable gradual refactoring

// Shared configuration and architecture
pub mod cfg_eval;        // Cfg attribute evaluation
pub mod arch;            // Architecture/platform detection
pub mod source_location; // Find crate sources in cargo registry

// Shared utilities
pub mod file_ops;        // File I/O operations
pub mod ast_parser;      // AST parsing with syn
pub mod item_visitor;    // Visit items in AST
pub mod graph;           // Dependency graph structures

// Re-export commonly used items
pub use cfg_eval::*;
pub use arch::*;
pub use source_location::*;

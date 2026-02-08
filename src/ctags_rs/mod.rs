//! Pure-Rust ctags-style parser for extracting tags from source code
//!
//! This module provides a lightweight alternative to full AST parsing for dependency tracking.
//! Currently supports Rust language parsing with plans to expand to other languages.

pub mod rust_parser;

// Re-export main types for convenience
pub use rust_parser::{parse_rust_file, RustTag, TagKind, Visibility};

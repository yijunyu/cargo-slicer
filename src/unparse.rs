//! Simple unparse implementation that avoids prettyplease reformatting
//!
//! This module provides a minimal alternative to prettyplease that preserves
//! the original line count by using quote's token stream directly.

use quote::ToTokens;
use syn::File;

/// Unparse a syn::File to string without reformatting
/// This avoids the line count increases that prettyplease causes
pub fn unparse(file: &File) -> String {
    file.into_token_stream().to_string()
}

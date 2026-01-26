//! Delete unused items from source files
//!
//! This module removes items that were not marked as used, reducing the codebase
//! to only what's actually needed.
//!
//! ## Feature-Aware Deletion
//!
//! The deletion behavior is controlled by `SlicerFeatures`:
//! - `-fprivate-fn`: Delete unused private functions
//! - `-fprivate-const`: Delete unused private constants/statics
//! - `-fprivate-type`: Delete unused private type aliases
//! - `-ftrust-graph`: Trust the dependency graph for all items
//!
//! By default (no features enabled), all items are kept (conservative mode).
//!
//! ## Cfg-Aware Deletion
//!
//! Items with `#[cfg(...)]` attributes are handled conservatively:
//! - Feature-gated items (`#[cfg(feature = "...")]`) are always kept
//! - Platform-specific items are evaluated against the current target
//! - This prevents deleting items that might be needed with different features

use std::path::Path;
use std::collections::{HashSet, HashMap};
use std::fs;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::types::CrateIndex;
use crate::slicer::config::SlicerConfig;
use crate::common::cfg_eval::{parse_cfg_expr, CfgExpr};
use syn::{Item, Type, TypePath, Visibility, Attribute};
use syn::spanned::Spanned;
use rayon::prelude::*;
use regex::Regex;

/// Convert line/column position to byte offset in source
/// Lines are 1-indexed, columns are 0-indexed (from proc-macro2)
fn line_col_to_byte_offset(source: &str, line: usize, col: usize) -> usize {
    let mut current_line = 1;
    let mut byte_offset = 0;

    for (i, c) in source.char_indices() {
        if current_line == line {
            // Found the target line, now count columns
            let mut current_col = 0;
            for (j, ch) in source[i..].char_indices() {
                if current_col == col {
                    return i + j;
                }
                if ch == '\n' {
                    break;
                }
                current_col += 1;
            }
            return i + col.min(source[i..].find('\n').unwrap_or(source.len() - i));
        }
        if c == '\n' {
            current_line += 1;
        }
        byte_offset = i + c.len_utf8();
    }

    byte_offset
}

/// Get the byte range of an item in source code
/// Returns (start_byte, end_byte) inclusive of any leading attributes and doc comments
fn get_item_byte_range(item: &Item, source: &str) -> Option<(usize, usize)> {
    let span = item.span();
    let start = span.start();
    let end = span.end();

    // proc-macro2 lines are 1-indexed, columns are 0-indexed
    if start.line == 0 || end.line == 0 {
        // Span not available (happens in some cases)
        return None;
    }

    let start_byte = line_col_to_byte_offset(source, start.line, start.column);
    let end_byte = line_col_to_byte_offset(source, end.line, end.column);

    // Extend start backwards to include any preceding whitespace/newlines
    // This helps clean up blank lines left after deletion
    let mut actual_start = start_byte;
    while actual_start > 0 {
        let prev_char = source[..actual_start].chars().last().unwrap_or(' ');
        if prev_char == '\n' {
            // Include the newline
            actual_start -= 1;
            break;
        } else if prev_char.is_whitespace() {
            actual_start -= prev_char.len_utf8();
        } else {
            break;
        }
    }

    Some((actual_start, end_byte))
}

/// Get the name of an item (for comparison purposes)
fn get_item_name(item: &Item) -> Option<String> {
    match item {
        Item::Const(c) => Some(c.ident.to_string()),
        Item::Enum(e) => Some(e.ident.to_string()),
        Item::Fn(f) => Some(f.sig.ident.to_string()),
        Item::Mod(m) => Some(m.ident.to_string()),
        Item::Static(s) => Some(s.ident.to_string()),
        Item::Struct(s) => Some(s.ident.to_string()),
        Item::Trait(t) => Some(t.ident.to_string()),
        Item::TraitAlias(t) => Some(t.ident.to_string()),
        Item::Type(t) => Some(t.ident.to_string()),
        Item::Union(u) => Some(u.ident.to_string()),
        Item::Macro(m) => m.ident.as_ref().map(|i| i.to_string()),
        // Items without names (use, impl, etc.) - use a hash of their span
        Item::Impl(_) | Item::Use(_) | Item::ExternCrate(_) | Item::ForeignMod(_) => {
            // These are always kept (no name to track)
            None
        }
        _ => None,
    }
}

/// Clean up multiple consecutive blank lines after deletion
fn cleanup_blank_lines(content: &str) -> String {
    let mut result = String::with_capacity(content.len());
    let mut blank_count = 0;

    for line in content.lines() {
        if line.trim().is_empty() {
            blank_count += 1;
            // Keep at most 2 consecutive blank lines
            if blank_count <= 2 {
                result.push_str(line);
                result.push('\n');
            }
        } else {
            blank_count = 0;
            result.push_str(line);
            result.push('\n');
        }
    }

    // Remove trailing newlines (keep at most 1)
    while result.ends_with("\n\n") {
        result.pop();
    }

    result
}

/// Read all .rs files in a directory and concatenate their content
/// This is used for crate-wide usage checks
fn read_all_rs_content(dir: &Path) -> String {
    let mut all_content = String::new();
    read_rs_files_recursive(dir, &mut all_content);
    all_content
}

fn read_rs_files_recursive(dir: &Path, content: &mut String) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                read_rs_files_recursive(&path, content);
            } else if path.extension().map(|e| e == "rs").unwrap_or(false) {
                if let Ok(file_content) = fs::read_to_string(&path) {
                    content.push_str(&file_content);
                    content.push('\n');
                }
            }
        }
    }
}

/// Count whole-word occurrences of a name in content
/// Uses word boundaries to avoid false positives (e.g., "Hash" in "HashMap")
fn count_word_occurrences(name: &str, content: &str) -> usize {
    // Build a regex pattern with word boundaries
    // \b matches word boundary (start/end of word)
    let pattern = format!(r"\b{}\b", regex::escape(name));
    match Regex::new(&pattern) {
        Ok(re) => re.find_iter(content).count(),
        Err(_) => {
            // Fallback to simple contains if regex fails
            content.matches(name).count()
        }
    }
}

pub struct DeleteStats {
    pub items_deleted: usize,
    pub loc_before: usize,
    pub loc_after: usize,
    pub files_skipped: usize,  // Files where nothing was deleted (prettyplease skipped)
    pub files_modified: usize, // Files that were actually modified
}

/// Check if an item has private (inherited) visibility
/// Returns true for inherited visibility (no pub keyword), false for any pub variant
fn is_private_visibility(vis: &Visibility) -> bool {
    matches!(vis, Visibility::Inherited)
}

/// Convert a file path relative to src/ into a module path
/// e.g., "src/ast.rs" → "ast", "src/ast/parser.rs" → "ast::parser", "src/lib.rs" → ""
fn file_path_to_module_path(rel_path: &Path) -> String {
    let path_str = rel_path.to_string_lossy();

    // Remove src/ prefix if present
    let path_str = path_str.strip_prefix("src/")
        .or_else(|| path_str.strip_prefix("src\\"))
        .unwrap_or(&path_str);

    // Remove .rs extension
    let path_str = path_str.strip_suffix(".rs")
        .unwrap_or(path_str);

    // Handle lib.rs and main.rs (root module)
    if path_str == "lib" || path_str == "main" {
        return String::new();
    }

    // Handle mod.rs files - use parent directory
    let path_str = if path_str.ends_with("/mod") || path_str.ends_with("\\mod") {
        &path_str[..path_str.len() - 4]
    } else {
        path_str
    };

    // Convert path separators to ::
    path_str.replace(['/', '\\'], "::")
}

/// Check if an item name (either qualified or simple) is in the used set
/// Returns true if item should be kept
fn is_item_used(name: &str, module_path: &str, used: &HashSet<String>) -> bool {
    // 1. Check if the simple name is directly in the used set
    if used.contains(name) {
        return true;
    }

    // 2. Build qualified name and check that
    let qualified = if module_path.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", module_path, name)
    };

    if used.contains(&qualified) {
        return true;
    }

    // 3. Check if any used entry ends with the qualified name
    // This handles cases where entry_points have different module path formats
    for used_item in used {
        if used_item.ends_with(&format!("::{}", name)) {
            // Check if this is a more specific match that includes our module path
            if module_path.is_empty() || used_item.contains(module_path) {
                return true;
            }
        }
    }

    false
}

/// Get the visibility of an item
fn get_item_visibility(item: &Item) -> Option<&Visibility> {
    match item {
        Item::Const(i) => Some(&i.vis),
        Item::Enum(i) => Some(&i.vis),
        Item::Fn(i) => Some(&i.vis),
        Item::Static(i) => Some(&i.vis),
        Item::Struct(i) => Some(&i.vis),
        Item::Trait(i) => Some(&i.vis),
        Item::TraitAlias(i) => Some(&i.vis),
        Item::Type(i) => Some(&i.vis),
        Item::Union(i) => Some(&i.vis),
        Item::Mod(i) => Some(&i.vis),
        _ => None,
    }
}

/// Extract type names from an impl block (e.g., "impl Clone for Level" -> "Level")
fn extract_impl_types(impl_item: &syn::ItemImpl) -> Vec<String> {
    let mut types = Vec::new();

    // Extract the self type (the type being implemented for)
    if let Type::Path(TypePath { path, .. }) = &*impl_item.self_ty {
        if let Some(segment) = path.segments.last() {
            types.push(segment.ident.to_string());
        }
    }

    // Extract trait type if this is a trait impl
    if let Some((_, trait_path, _)) = &impl_item.trait_ {
        if let Some(segment) = trait_path.segments.last() {
            types.push(segment.ident.to_string());
        }
    }

    types
}

/// Common derive traits that should always be preserved
/// These are typically generated by macros like bitflags!, derive macros, etc.
const PRESERVED_DERIVE_TRAITS: &[&str] = &[
    // Core traits
    "Clone", "Copy", "Debug", "Default",
    // Comparison traits
    "PartialEq", "Eq", "PartialOrd", "Ord", "Hash",
    // Formatting traits
    "Display", "Binary", "Octal", "LowerHex", "UpperHex",
    // Conversion traits
    "From", "Into", "TryFrom", "TryInto", "AsRef", "AsMut",
    // Iterator traits
    "Iterator", "IntoIterator", "Extend",
    // Bitwise operation traits (used by bitflags!)
    "BitAnd", "BitOr", "BitXor", "Not", "BitAndAssign", "BitOrAssign", "BitXorAssign",
    // Arithmetic traits
    "Add", "Sub", "Mul", "Div", "Rem", "Neg",
    "AddAssign", "SubAssign", "MulAssign", "DivAssign", "RemAssign",
    // Deref traits
    "Deref", "DerefMut",
    // Send/Sync (marker traits)
    "Send", "Sync",
];

/// Check if an impl block implements a common derive trait that should be preserved
fn is_preserved_derive_impl(impl_item: &syn::ItemImpl) -> bool {
    // Only applies to trait impls
    if let Some((_, trait_path, _)) = &impl_item.trait_ {
        if let Some(segment) = trait_path.segments.last() {
            let trait_name = segment.ident.to_string();
            return PRESERVED_DERIVE_TRAITS.contains(&trait_name.as_str());
        }
    }
    false
}

/// Get the attributes of an item
fn get_item_attrs(item: &Item) -> Option<&[Attribute]> {
    match item {
        Item::Const(i) => Some(&i.attrs),
        Item::Enum(i) => Some(&i.attrs),
        Item::Fn(i) => Some(&i.attrs),
        Item::Static(i) => Some(&i.attrs),
        Item::Struct(i) => Some(&i.attrs),
        Item::Trait(i) => Some(&i.attrs),
        Item::TraitAlias(i) => Some(&i.attrs),
        Item::Type(i) => Some(&i.attrs),
        Item::Union(i) => Some(&i.attrs),
        Item::Mod(i) => Some(&i.attrs),
        Item::Impl(i) => Some(&i.attrs),
        Item::Use(i) => Some(&i.attrs),
        Item::Macro(i) => Some(&i.attrs),
        _ => None,
    }
}

/// Check if an item has a cfg attribute that should prevent deletion
/// Returns true if the item should be KEPT (not deleted) due to cfg attributes
fn should_keep_due_to_cfg(item: &Item) -> bool {
    let attrs = match get_item_attrs(item) {
        Some(a) => a,
        None => return false,
    };

    for attr in attrs {
        // Check for #[cfg(...)] attributes
        if attr.path().is_ident("cfg") {
            // Convert attribute to string and parse the cfg expression
            let attr_str = quote::quote!(#attr).to_string();

            // Extract the cfg content from #[cfg(...)]
            if let Some(start) = attr_str.find("cfg(") {
                let rest = &attr_str[start + 4..];
                if let Some(end) = find_matching_paren(rest) {
                    let cfg_content = &rest[..end];

                    // Parse and evaluate the cfg expression
                    if let Some(expr) = parse_cfg_expr(cfg_content) {
                        // Check if this is a feature-gated item
                        if contains_feature_gate(&expr) {
                            // Feature-gated items should always be kept
                            // Let Cargo decide at build time
                            return true;
                        }

                        // For platform-specific cfgs, evaluate against current target
                        let empty_features = std::collections::HashSet::new();
                        if !expr.evaluate(&empty_features) {
                            // Cfg evaluates to false on current platform
                            // This item is for a different platform - can be deleted
                            // (but we're being asked if we should KEEP it, so return false)
                            return false;
                        }
                    }
                }
            }
        }
    }

    false
}

/// Find the position of the matching closing parenthesis
fn find_matching_paren(s: &str) -> Option<usize> {
    let mut depth = 1;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Check if a cfg expression contains a feature gate
fn contains_feature_gate(expr: &CfgExpr) -> bool {
    match expr {
        CfgExpr::Feature(_) => true,
        CfgExpr::Not(inner) => contains_feature_gate(inner),
        CfgExpr::All(exprs) | CfgExpr::Any(exprs) => {
            exprs.iter().any(contains_feature_gate)
        }
        CfgExpr::Target(_, _) | CfgExpr::Other(_) => false,
    }
}

/// Delete unused items from the copied source
///
/// The deletion behavior depends on the features enabled in `config.features`:
/// - No features: Conservative mode (keep everything)
/// - `-fprivate-fn`: Delete unused private functions
/// - `-fprivate-const`: Delete unused private constants/statics
/// - `-fprivate-type`: Delete unused private type aliases
/// - `-ftrust-graph`: Trust the graph for all items (most aggressive)
pub fn delete_unused(
    index: &CrateIndex,
    used: &HashSet<String>,
    output_dir: &Path,
    config: &SlicerConfig,
) -> Result<DeleteStats, String> {
    // Group items by file
    let mut items_by_file: HashMap<std::path::PathBuf, Vec<&crate::types::ParsedItem>> = HashMap::new();

    for item in &index.all_items {
        items_by_file.entry(item.file.clone()).or_default().push(item);
    }

    // Collect files to process
    let files: Vec<_> = items_by_file.into_keys().collect();

    // Use atomic counters for thread-safe aggregation
    let total_deleted = AtomicUsize::new(0);
    let total_loc_before = AtomicUsize::new(0);
    let total_loc_after = AtomicUsize::new(0);
    let files_skipped = AtomicUsize::new(0);  // Files where nothing was deleted
    let files_modified = AtomicUsize::new(0); // Files that were actually modified

    // OPTIMIZATION: Single-pass processing (previously two passes)
    // The filter rules are very conservative (keep all functions, structs, enums,
    // traits, types, modules, etc.) so we don't need a separate pass to collect
    // impl types. The extended_used set is just the original used set.

    // Extract features for use in closures
    let features = &config.features;

    // Read all .rs content in the crate for crate-wide usage checks
    // This ensures we don't delete items used in other files
    let crate_wide_content = read_all_rs_content(output_dir);

    // Single pass: parse each file once, collect impl types AND filter items
    let results: Vec<Result<(), String>> = files.par_iter().map(|file_path| {
        // Convert absolute path to relative path within output_dir
        let rel_path = file_path.strip_prefix(output_dir)
            .unwrap_or(file_path);
        let output_file = output_dir.join(rel_path);

        // Compute module path from file path for qualified name checking
        let module_path = file_path_to_module_path(rel_path);

        // Read original file
        let content = fs::read_to_string(&output_file)
            .map_err(|e| format!("Failed to read {}: {}", output_file.display(), e))?;

        let before_lines = content.lines().count();
        total_loc_before.fetch_add(before_lines, Ordering::Relaxed);

        // Parse file with syn
        let syntax_tree = match syn::parse_file(&content) {
            Ok(tree) => tree,
            Err(_) => {
                // If parsing fails, keep the file unchanged
                total_loc_after.fetch_add(before_lines, Ordering::Relaxed);
                return Ok(());
            }
        };

        // Filter items based on features and used set
        let mut deleted_count = 0;
        let original_item_count = syntax_tree.items.len();
        let items_to_keep: Vec<Item> = syntax_tree.items.into_iter()
            .filter(|item| {
                // === CFG-AWARE DELETION ===
                // Items with #[cfg(feature = "...")] should always be kept
                // Let Cargo decide at build time whether to include them
                if should_keep_due_to_cfg(item) {
                    return true;
                }

                // === FEATURE-AWARE DELETION ===
                // The deletion behavior depends on which features are enabled.
                // By default (no features), we keep everything (conservative mode).

                // Helper: check if item should be deleted based on visibility and used set
                // Uses qualified names (module_path::name) to handle duplicate names correctly
                let should_delete_private = |vis: &Visibility, name: &str| -> bool {
                    // Only delete if:
                    // 1. Item is private (inherited visibility)
                    // 2. Item is NOT in the used set (checking both simple and qualified names)
                    is_private_visibility(vis) && !is_item_used(name, &module_path, used)
                };

                match item {
                    // === CONSTANTS AND STATICS ===
                    Item::Const(c) => {
                        if features.delete_private_const {
                            let name = c.ident.to_string();
                            // Only delete if name doesn't appear elsewhere in the ENTIRE crate
                            let is_used_in_crate = {
                                let occurrences = count_word_occurrences(&name, &crate_wide_content);
                                occurrences > 1
                            };
                            if should_delete_private(&c.vis, &name) && !is_used_in_crate {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }
                    Item::Static(s) => {
                        if features.delete_private_const {
                            let name = s.ident.to_string();
                            // Only delete if name doesn't appear elsewhere in the ENTIRE crate
                            let is_used_in_crate = {
                                let occurrences = count_word_occurrences(&name, &crate_wide_content);
                                occurrences > 1
                            };
                            if should_delete_private(&s.vis, &name) && !is_used_in_crate {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }

                    // === FUNCTIONS ===
                    Item::Fn(f) => {
                        if features.delete_private_fn {
                            let name = f.sig.ident.to_string();
                            // Only delete if:
                            // 1. Private visibility
                            // 2. Not in used set
                            // 3. Not called anywhere in the ENTIRE crate
                            let is_called_in_crate = {
                                // Check if "funcname(" appears in the crate (indicates a call)
                                let call_pattern = format!("{}(", name);
                                // Count occurrences - 1 for definition, any more means it's called
                                let occurrences = crate_wide_content.matches(&call_pattern).count();
                                occurrences > 1 // More than just the definition
                            };
                            if should_delete_private(&f.vis, &name) && !is_called_in_crate {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }

                    // === TYPE ALIASES ===
                    Item::Type(t) => {
                        if features.delete_private_type {
                            let name = t.ident.to_string();
                            // Only delete if type name doesn't appear elsewhere in file
                            let is_used_in_file = {
                                // Count occurrences of the type name
                                let occurrences = count_word_occurrences(&name, &crate_wide_content);
                                occurrences > 1 // More than just the definition
                            };
                            if should_delete_private(&t.vis, &name) && !is_used_in_file {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }

                    // === ENUMS, STRUCTS, UNIONS ===
                    // With delete_private_struct: delete private structs/enums/unions
                    // using the same text-based heuristic as functions (safe)
                    // With trust_graph: delete based on used set only (aggressive)
                    Item::Enum(e) => {
                        if features.delete_private_struct {
                            let name = e.ident.to_string();
                            // Only delete if type name doesn't appear elsewhere in file
                            let is_used_in_file = {
                                let occurrences = count_word_occurrences(&name, &crate_wide_content);
                                occurrences > 1 // More than just the definition
                            };
                            if should_delete_private(&e.vis, &name) && !is_used_in_file {
                                deleted_count += 1;
                                return false;
                            }
                        } else if features.trust_graph {
                            let name = e.ident.to_string();
                            if !is_item_used(&name, &module_path, used) {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }
                    Item::Struct(s) => {
                        if features.delete_private_struct {
                            let name = s.ident.to_string();
                            // Only delete if type name doesn't appear elsewhere in file
                            let is_used_in_file = {
                                let occurrences = count_word_occurrences(&name, &crate_wide_content);
                                occurrences > 1 // More than just the definition
                            };
                            if should_delete_private(&s.vis, &name) && !is_used_in_file {
                                deleted_count += 1;
                                return false;
                            }
                        } else if features.trust_graph {
                            let name = s.ident.to_string();
                            if !is_item_used(&name, &module_path, used) {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }
                    Item::Union(u) => {
                        if features.delete_private_struct {
                            let name = u.ident.to_string();
                            // Only delete if type name doesn't appear elsewhere in file
                            let is_used_in_file = {
                                let occurrences = count_word_occurrences(&name, &crate_wide_content);
                                occurrences > 1 // More than just the definition
                            };
                            if should_delete_private(&u.vis, &name) && !is_used_in_file {
                                deleted_count += 1;
                                return false;
                            }
                        } else if features.trust_graph {
                            let name = u.ident.to_string();
                            if !is_item_used(&name, &module_path, used) {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }

                    // === TRAITS ===
                    // Traits are heavily used in bounds - only delete with trust_graph
                    Item::Trait(t) => {
                        if features.trust_graph {
                            let name = t.ident.to_string();
                            if !is_item_used(&name, &module_path, used) {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true // Keep by default
                    }

                    // === MODULES ===
                    // Always keep module declarations - they're structural
                    // Cleanup phase will remove empty modules later
                    Item::Mod(_) => true,

                    // === IMPL BLOCKS ===
                    // Impl blocks for common derive traits should always be preserved
                    // This handles macro-generated impls (bitflags!, derive macros, etc.)
                    Item::Impl(impl_item) => {
                        // Always keep impl blocks for common derive traits
                        if is_preserved_derive_impl(impl_item) {
                            return true;
                        }
                        // For other impl blocks, keep by default
                        // Cleanup phase can remove orphaned impls later
                        true
                    }

                    // === OTHER ITEMS ===
                    // Use statements, macros, etc.
                    // Keep them - cleanup phase handles these
                    _ => {
                        if let Some(name) = get_item_name(item) {
                            // For named items not handled above, check the used set
                            // Uses qualified names to handle duplicate name collisions
                            if features.trust_graph && !is_item_used(&name, &module_path, used) {
                                deleted_count += 1;
                                return false;
                            }
                        }
                        true
                    }
                }
            })
            .collect();

        total_deleted.fetch_add(deleted_count, Ordering::Relaxed);

        // OPTIMIZATION: Skip if nothing was deleted
        if items_to_keep.len() == original_item_count {
            // Nothing deleted, keep original file as-is
            total_loc_after.fetch_add(before_lines, Ordering::Relaxed);
            files_skipped.fetch_add(1, Ordering::Relaxed);
            return Ok(());
        }

        files_modified.fetch_add(1, Ordering::Relaxed);

        // SPAN-BASED DELETION: Remove deleted items from original source
        // This preserves original formatting (comments, whitespace, style)
        // Collect byte ranges of items to DELETE (not keep)
        let mut ranges_to_delete: Vec<(usize, usize)> = Vec::new();

        // Re-parse to get items with spans (items_to_keep lost the deleted ones)
        // Compare by checking if item is in items_to_keep
        let items_to_keep_set: HashSet<String> = items_to_keep.iter()
            .filter_map(|item| get_item_name(item))
            .collect();

        if let Ok(reparsed) = syn::parse_file(&content) {
            for item in &reparsed.items {
                if let Some(name) = get_item_name(item) {
                    if !items_to_keep_set.contains(&name) {
                        // This item should be deleted - get its byte range
                        if let Some((start, end)) = get_item_byte_range(item, &content) {
                            ranges_to_delete.push((start, end));
                        }
                    }
                }
            }
        }

        // Sort ranges by start position (descending) to delete from end to start
        // This prevents invalidating byte offsets as we delete
        ranges_to_delete.sort_by(|a, b| b.0.cmp(&a.0));

        // Delete byte ranges from original content
        let mut new_content = content.clone();
        for (start, end) in ranges_to_delete {
            if start < new_content.len() && end <= new_content.len() && start <= end {
                new_content.replace_range(start..end, "");
            }
        }

        // Clean up multiple consecutive blank lines (artifact of deletion)
        let new_content = cleanup_blank_lines(&new_content);

        // Write modified file (preserves original formatting!)
        fs::write(&output_file, &new_content)
            .map_err(|e| format!("Failed to write {}: {}", output_file.display(), e))?;

        let after_lines = new_content.lines().count();
        total_loc_after.fetch_add(after_lines, Ordering::Relaxed);

        Ok(())
    }).collect();

    // Check for errors
    for result in results {
        result?;
    }

    Ok(DeleteStats {
        items_deleted: total_deleted.load(Ordering::Relaxed),
        loc_before: total_loc_before.load(Ordering::Relaxed),
        loc_after: total_loc_after.load(Ordering::Relaxed),
        files_skipped: files_skipped.load(Ordering::Relaxed),
        files_modified: files_modified.load(Ordering::Relaxed),
    })
}

//! Text-based deleter for ctags mode
//!
//! This module implements item deletion using text manipulation instead of AST parsing.
//! It works with ctags tag information to identify item locations and uses the
//! TextExtractor to find precise item boundaries.
//!
//! Key features:
//! - No syn parsing required (works with ctags tags)
//! - Item-wise restoration for cargo check verification
//! - Preserves original formatting (no prettyplease needed)

use std::path::{Path, PathBuf};
use std::collections::{HashMap, HashSet};
use std::fs;
use crate::types::ParsedItem;
use crate::slicer::text_extractor::TextExtractor;
use crate::slicer::config::SlicerConfig;

/// Metadata for a potentially deletable item
#[derive(Debug, Clone)]
pub struct DeletableItem {
    /// Item name
    pub name: String,
    /// File path
    pub file: PathBuf,
    /// Byte range in the file (start, end)
    pub byte_range: (usize, usize),
    /// Original content (for restoration)
    pub original_content: String,
    /// Whether this item is currently deleted
    pub deleted: bool,
    /// Module path (for qualified name checking)
    pub module_path: String,
}

/// Result of text-based deletion
pub struct TextDeleteStats {
    pub items_deleted: usize,
    pub items_restored: usize,
    pub files_modified: usize,
}

/// Text-based deleter using ctags information
pub struct TextDeleter {
    /// All items that can potentially be deleted
    deletable_items: Vec<DeletableItem>,
    /// Index: file_path -> list of item indices
    items_by_file: HashMap<PathBuf, Vec<usize>>,
}

impl TextDeleter {
    /// Create a new text deleter from parsed items
    ///
    /// This analyzes all items and identifies which ones are candidates for deletion
    /// based on visibility and usage.
    pub fn new(
        all_items: &[ParsedItem],
        used: &HashSet<String>,
        output_dir: &Path,
        config: &SlicerConfig,
    ) -> Result<Self, String> {
        let mut deletable_items = Vec::new();
        let mut items_by_file: HashMap<PathBuf, Vec<usize>> = HashMap::new();

        // Create extractors once per file (not per item!)
        let mut extractors: HashMap<PathBuf, TextExtractor> = HashMap::new();

        for item in all_items {
            // Determine if this item is a deletion candidate
            if !should_consider_for_deletion(item, config) {
                continue;
            }

            // Check if item is in the used set
            let module_path = file_path_to_module_path(&item.file, output_dir);
            if is_item_used(&item.name, &module_path, used) {
                continue;
            }

            // Load file content and create extractor if not already done
            let file_path = output_dir.join(&item.file);
            if !extractors.contains_key(&file_path) {
                let content = fs::read_to_string(&file_path)
                    .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;
                extractors.insert(file_path.clone(), TextExtractor::new(content));
            }

            // Reuse extractor (no clone!)
            let extractor = &extractors[&file_path];

            // Get item byte range using line number from ctags
            let byte_range = match extract_item_range(extractor, item) {
                Some(range) => range,
                None => {
                    eprintln!("Warning: Could not extract range for {} in {}",
                             item.name, file_path.display());
                    continue;
                }
            };

            let original_content = extractor.source_ref()[byte_range.0..byte_range.1].to_string();

            let idx = deletable_items.len();
            deletable_items.push(DeletableItem {
                name: item.name.clone(),
                file: file_path.clone(),
                byte_range,
                original_content,
                deleted: false,
                module_path,
            });

            items_by_file.entry(file_path).or_default().push(idx);
        }

        Ok(Self {
            deletable_items,
            items_by_file,
        })
    }

    /// Delete all items that are not used
    ///
    /// Returns the number of items deleted.
    pub fn delete_unused(&mut self) -> Result<usize, String> {
        let mut deleted_count = 0;

        for item in &mut self.deletable_items {
            if !item.deleted {
                item.deleted = true;
                deleted_count += 1;
            }
        }

        Ok(deleted_count)
    }

    /// Apply deletions to files
    ///
    /// This writes the modified content back to disk.
    pub fn apply_deletions(&self) -> Result<usize, String> {
        let mut files_modified = 0;

        for (file_path, item_indices) in &self.items_by_file {
            // Check if any items in this file are deleted
            let has_deletions = item_indices.iter()
                .any(|&idx| self.deletable_items[idx].deleted);

            if !has_deletions {
                continue;
            }

            // Load original file content
            let original_content = fs::read_to_string(file_path)
                .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;

            // Collect byte ranges to delete (sorted by start position, descending)
            let mut ranges_to_delete: Vec<(usize, usize)> = item_indices.iter()
                .filter(|&&idx| self.deletable_items[idx].deleted)
                .map(|&idx| self.deletable_items[idx].byte_range)
                .collect();

            ranges_to_delete.sort_by(|a, b| b.0.cmp(&a.0)); // Sort descending

            // Apply deletions from end to start (to preserve byte offsets)
            let mut new_content = original_content.clone();
            for (start, end) in ranges_to_delete {
                if start < new_content.len() && end <= new_content.len() && start <= end {
                    // Delete the range
                    new_content.replace_range(start..end, "");
                }
            }

            // Clean up multiple consecutive blank lines
            new_content = cleanup_blank_lines(&new_content);

            // Write back
            fs::write(file_path, &new_content)
                .map_err(|e| format!("Failed to write {}: {}", file_path.display(), e))?;

            files_modified += 1;
        }

        Ok(files_modified)
    }

    /// Restore specific items by name
    ///
    /// This is used after cargo check identifies errors.
    pub fn restore_items(&mut self, item_names: &[String]) -> usize {
        let mut restored_count = 0;

        for item in &mut self.deletable_items {
            if item.deleted && item_names.contains(&item.name) {
                item.deleted = false;
                restored_count += 1;
            }
        }

        restored_count
    }

    /// Get statistics
    pub fn stats(&self) -> TextDeleteStats {
        let items_deleted = self.deletable_items.iter()
            .filter(|item| item.deleted)
            .count();

        let files_modified = self.items_by_file.keys()
            .filter(|file| {
                self.items_by_file[*file].iter()
                    .any(|&idx| self.deletable_items[idx].deleted)
            })
            .count();

        TextDeleteStats {
            items_deleted,
            items_restored: 0, // Updated after restoration
            files_modified,
        }
    }
}

/// Extract byte range for an item using TextExtractor
fn extract_item_range(extractor: &TextExtractor, item: &ParsedItem) -> Option<(usize, usize)> {
    // Get starting line from ctags (1-indexed)
    let start_line = item.line?; // Unwrap Option<usize>
    if start_line == 0 {
        return None;
    }

    // Get the byte offset for the start line
    let start_offset = extractor.line_to_offset(start_line)?;

    // Use TextExtractor to find the item end based on kind
    use crate::types::ParsedItemKind;
    let kind = match &item.kind {
        ParsedItemKind::Function => "function",
        ParsedItemKind::Struct => "struct",
        ParsedItemKind::Enum => "enum",
        ParsedItemKind::Trait => "trait",
        ParsedItemKind::Impl => "impl",
        ParsedItemKind::Mod => "module",
        ParsedItemKind::Const => "constant",
        ParsedItemKind::Static => "constant",
        ParsedItemKind::TypeAlias => "type",
        _ => "function", // Default fallback
    };

    // Find the end offset using brace counting or semicolon detection
    let end_offset = find_item_end(extractor, start_offset, kind)?;

    // Extend start backwards to include doc comments and attributes
    let actual_start = find_item_start(extractor, start_offset);

    Some((actual_start, end_offset))
}

/// Find the start of an item by scanning backwards for doc comments and attributes
fn find_item_start(extractor: &TextExtractor, offset: usize) -> usize {
    let source = extractor.source_ref();
    let mut pos = offset;

    // Scan backwards to find doc comments (#[...], ///, etc.)
    while pos > 0 {
        // Skip whitespace backwards
        let mut ws_pos = pos;
        while ws_pos > 0 {
            let ch = source[..ws_pos].chars().last().unwrap_or(' ');
            if ch.is_whitespace() && ch != '\n' {
                ws_pos -= ch.len_utf8();
            } else {
                break;
            }
        }

        // Check for attribute or doc comment on previous line
        let line_start = source[..ws_pos].rfind('\n').map(|p| p + 1).unwrap_or(0);
        let line = &source[line_start..ws_pos];
        let trimmed = line.trim();

        if trimmed.starts_with("#[") || trimmed.starts_with("///") || trimmed.starts_with("//!") {
            // Found an attribute or doc comment, continue scanning backwards
            pos = line_start;
        } else {
            // No more attributes/doc comments
            break;
        }
    }

    pos
}

/// Find the end of an item
fn find_item_end(extractor: &TextExtractor, start: usize, kind: &str) -> Option<usize> {
    let source = extractor.source_ref();

    match kind {
        "constant" | "type" => {
            // These end with semicolon
            find_semicolon_end(source, start)
        }
        _ => {
            // These use braces
            find_brace_end(source, start)
        }
    }
}

/// Find the end of an item by looking for semicolon
fn find_semicolon_end(source: &str, start: usize) -> Option<usize> {
    let mut in_string = false;
    let mut escape = false;

    for (i, ch) in source[start..].char_indices() {
        if escape {
            escape = false;
            continue;
        }

        match ch {
            '\\' if in_string => escape = true,
            '"' => in_string = !in_string,
            ';' if !in_string => return Some(start + i + 1),
            _ => {}
        }
    }

    None
}

/// Find the end of an item by counting braces
fn find_brace_end(source: &str, start: usize) -> Option<usize> {
    let mut brace_depth = 0;
    let mut in_string = false;
    let mut in_char = false;
    let mut escape = false;
    let mut found_opening = false;

    for (i, ch) in source[start..].char_indices() {
        if escape {
            escape = false;
            continue;
        }

        match ch {
            '\\' if in_string || in_char => escape = true,
            '"' if !in_char => in_string = !in_string,
            '\'' if !in_string => in_char = !in_char,
            '{' if !in_string && !in_char => {
                brace_depth += 1;
                found_opening = true;
            }
            '}' if !in_string && !in_char => {
                brace_depth -= 1;
                if found_opening && brace_depth == 0 {
                    // Found matching closing brace
                    return Some(start + i + 1);
                }
            }
            _ => {}
        }
    }

    None
}

/// Clean up multiple consecutive blank lines
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

/// Check if an item should be considered for deletion
fn should_consider_for_deletion(item: &ParsedItem, config: &SlicerConfig) -> bool {
    use crate::types::{ParsedItemKind, ItemVisibility};
    let features = &config.features;

    // Only consider private items
    if item.visibility != ItemVisibility::Private {
        return false;
    }

    // Check feature flags
    match &item.kind {
        ParsedItemKind::Function if features.delete_private_fn => true,
        ParsedItemKind::Const | ParsedItemKind::Static if features.delete_private_const => true,
        ParsedItemKind::TypeAlias if features.delete_private_type => true,
        ParsedItemKind::Struct | ParsedItemKind::Enum if features.delete_private_struct => true,
        _ if features.trust_graph => true,
        _ => false,
    }
}

/// Convert a file path relative to src/ into a module path
fn file_path_to_module_path(rel_path: &Path, output_dir: &Path) -> String {
    let path_str = rel_path.strip_prefix(output_dir)
        .unwrap_or(rel_path)
        .to_string_lossy();

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
    for used_item in used {
        if used_item.ends_with(&format!("::{}", name)) {
            if module_path.is_empty() || used_item.contains(module_path) {
                return true;
            }
        }
    }

    false
}

// No extension trait needed - TextExtractor now has source_ref() method

/// Delete unused items using text-based deletion (wrapper to match deleter interface)
///
/// This function provides the same interface as `deleter::delete_unused()` but uses
/// text-based deletion instead of AST manipulation.
pub fn delete_unused_text_based(
    index: &crate::types::CrateIndex,
    used: &HashSet<String>,
    output_dir: &Path,
    config: &SlicerConfig,
) -> Result<super::deleter::DeleteStats, String> {
    use crate::slicer::deleter::DeleteStats;

    // Create text deleter from parsed items
    let mut deleter = TextDeleter::new(&index.all_items, used, output_dir, config)?;

    // Count LOC before deletion
    let loc_before = count_loc(output_dir)?;

    // Delete unused items
    let deleted_count = deleter.delete_unused()?;

    // Apply deletions to files
    let files_modified = deleter.apply_deletions()?;

    // Count LOC after deletion
    let loc_after = count_loc(output_dir)?;

    Ok(DeleteStats {
        items_deleted: deleted_count,
        loc_before,
        loc_after,
        files_skipped: 0,  // Text deleter doesn't skip files
        files_modified,
    })
}

/// Count lines of code in a directory
fn count_loc(dir: &Path) -> Result<usize, String> {
    fn count_loc_recursive(dir: &Path, total: &mut usize) -> Result<(), String> {
        let entries = fs::read_dir(dir)
            .map_err(|e| format!("Failed to read directory {}: {}", dir.display(), e))?;

        for entry in entries {
            let entry = entry.map_err(|e| format!("Failed to read entry: {}", e))?;
            let path = entry.path();

            if path.is_dir() {
                count_loc_recursive(&path, total)?;
            } else if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "rs" {
                        let content = fs::read_to_string(&path)
                            .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
                        *total += content.lines().count();
                    }
                }
            }
        }

        Ok(())
    }

    let mut total = 0;
    count_loc_recursive(dir, &mut total)?;
    Ok(total)
}

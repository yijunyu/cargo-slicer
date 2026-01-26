//! Clean up imports and empty modules
//!
//! After deleting unused items, we need to clean up:
//! - Unused import statements
//! - Empty module files
//! - Empty mod declarations

use std::path::Path;
use std::collections::HashSet;
use std::fs;
use std::sync::atomic::{AtomicUsize, Ordering};
use syn::Item;
use prettyplease;
use rayon::prelude::*;

/// Clean up unused imports and empty modules
pub fn cleanup(
    output_dir: &Path,
    _used: &HashSet<String>,  // Will be used in Phase 4c
) -> Result<(), String> {
    // Find all .rs files
    let rust_files = find_rust_files(output_dir)?;

    // Use atomic counter for thread-safe aggregation
    let cleaned_files = AtomicUsize::new(0);

    // Process each file in parallel
    let results: Vec<Result<(), String>> = rust_files.par_iter().map(|file_path| {
        // Read the file
        let content = fs::read_to_string(&file_path)
            .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;

        // Check if file is empty (only whitespace/comments)
        let is_empty = content.trim().is_empty() ||
                      content.lines().all(|line| {
                          let trimmed = line.trim();
                          trimmed.is_empty() || trimmed.starts_with("//")
                      });

        if is_empty {
            // Remove empty files
            fs::remove_file(&file_path)
                .map_err(|e| format!("Failed to remove {}: {}", file_path.display(), e))?;
            cleaned_files.fetch_add(1, Ordering::Relaxed);
            return Ok(());
        }

        // Parse and clean up empty mod declarations
        let syntax_tree = match syn::parse_file(&content) {
            Ok(tree) => tree,
            Err(_) => return Ok(()), // Skip files that don't parse
        };

        // Filter out empty mod declarations (mod foo;) where foo.rs doesn't exist
        let items_before = syntax_tree.items.len();
        let filtered_items: Vec<Item> = syntax_tree.items.into_iter()
            .filter(|item| {
                if let Item::Mod(ref mod_item) = item {
                    // Check if this is a mod declaration (not inline module)
                    if mod_item.content.is_none() {
                        let mod_name = mod_item.ident.to_string();

                        // Collect ALL paths from #[path = "..."] and #[cfg_attr(..., path = "...")] attributes
                        let custom_paths: Vec<String> = mod_item.attrs.iter()
                            .filter_map(|attr| {
                                // Direct #[path = "value"] attribute
                                if attr.path().is_ident("path") {
                                    if let syn::Meta::NameValue(ref nv) = attr.meta {
                                        if let syn::Expr::Lit(ref lit) = nv.value {
                                            if let syn::Lit::Str(ref s) = lit.lit {
                                                return Some(s.value());
                                            }
                                        }
                                    }
                                }
                                // #[cfg_attr(condition, path = "value")] attribute
                                if attr.path().is_ident("cfg_attr") {
                                    if let syn::Meta::List(ref list) = attr.meta {
                                        // Parse the tokens to find path = "..."
                                        let tokens_str = list.tokens.to_string();
                                        // Look for `path = "..."` pattern in the cfg_attr
                                        // Handle both "path =" and "path=" formats
                                        let path_pattern = if tokens_str.contains("path =") {
                                            Some(("path =", 6))
                                        } else if tokens_str.contains("path=") {
                                            Some(("path=", 5))
                                        } else {
                                            None
                                        };

                                        if let Some((pattern, skip)) = path_pattern {
                                            if let Some(path_pos) = tokens_str.find(pattern) {
                                                let after_path = &tokens_str[path_pos + skip..];
                                                // Extract the string value
                                                if let Some(start) = after_path.find('"') {
                                                    let after_start = &after_path[start + 1..];
                                                    if let Some(end) = after_start.find('"') {
                                                        return Some(after_start[..end].to_string());
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                None
                            })
                            .collect();

                        // Check if the module file exists
                        let parent_dir = file_path.parent().unwrap_or(output_dir);

                        if !custom_paths.is_empty() {
                            // Check if ANY of the custom paths exist
                            // (with cfg_attr, different paths are used for different features)
                            let exists = custom_paths.iter().any(|custom_file| {
                                let mod_file = parent_dir.join(custom_file);
                                mod_file.exists()
                            });
                            return exists;
                        } else {
                            // Determine the correct directory to look in
                            // For probe.rs -> look in probe/ subdirectory
                            // For mod.rs -> look in parent directory
                            let search_dir: std::path::PathBuf = if file_path.file_name().and_then(|n| n.to_str()) == Some("mod.rs") {
                                // This is a mod.rs, submodules are in parent directory
                                parent_dir.to_path_buf()
                            } else if let Some(file_stem) = file_path.file_stem().and_then(|s| s.to_str()) {
                                // Check if there's a subdirectory with same name as this file (e.g., probe/ for probe.rs)
                                let subdir = parent_dir.join(file_stem);
                                if subdir.is_dir() {
                                    subdir
                                } else {
                                    parent_dir.to_path_buf()
                                }
                            } else {
                                parent_dir.to_path_buf()
                            };

                            // Use default naming (mod_name.rs or mod_name/mod.rs)
                            let mod_file = search_dir.join(format!("{}.rs", mod_name));
                            let mod_dir = search_dir.join(&mod_name).join("mod.rs");

                            // Keep the mod declaration if either file exists
                            return mod_file.exists() || mod_dir.exists();
                        }
                    }
                }
                true // Keep all other items
            })
            .collect();

        // Only rewrite if we removed something
        if filtered_items.len() < items_before {
            let filtered_file = syn::File {
                shebang: syntax_tree.shebang,
                attrs: syntax_tree.attrs,
                items: filtered_items,
            };

            let new_content = prettyplease::unparse(&filtered_file);
            fs::write(&file_path, new_content)
                .map_err(|e| format!("Failed to write {}: {}", file_path.display(), e))?;

            cleaned_files.fetch_add(1, Ordering::Relaxed);
        }

        Ok(())
    }).collect();

    // Check for errors
    for result in results {
        result?;
    }

    // Return the count for potential verbose logging at caller
    Ok(())
}

/// Find all .rs files in the src/ directory recursively
/// Skips root-level .rs files (like no_atomic.rs, build-common.rs) which are
/// included via include!() macros and should not be processed by the slicer
fn find_rust_files(dir: &Path) -> Result<Vec<std::path::PathBuf>, String> {
    let mut files = Vec::new();

    // Only process files in src/ directory to avoid modifying include!() helper files
    let src_dir = dir.join("src");
    let search_dir = if src_dir.exists() { src_dir } else { dir.to_path_buf() };

    fn visit_dir(dir: &Path, files: &mut Vec<std::path::PathBuf>) -> Result<(), String> {
        for entry in fs::read_dir(dir)
            .map_err(|e| format!("Failed to read dir: {}", e))?
        {
            let entry = entry.map_err(|e| format!("Failed to read entry: {}", e))?;
            let path = entry.path();

            if path.is_dir() {
                visit_dir(&path, files)?;
            } else if path.extension().map_or(false, |ext| ext == "rs") {
                files.push(path);
            }
        }
        Ok(())
    }

    visit_dir(&search_dir, &mut files)?;
    Ok(files)
}

//! Source code fixing utilities.
//!
//! Functions for fixing common issues in generated/extracted source code.

use std::collections::HashSet;
use std::io::Write;
use std::process::{Command, Stdio};

/// Format source code with rustfmt
/// Set RUSTFMT=1 to enable formatting (disabled by default for faster slicing)
pub fn format_source(source: &str) -> String {
    // Skip rustfmt by default for faster slicing
    // Set RUSTFMT=1 to enable formatting
    if std::env::var("RUSTFMT").map(|v| v == "1").unwrap_or(false) {
        // First try rustfmt, fall back to original if it fails
        if let Ok(formatted) = run_rustfmt(source, "2021") {
            return formatted;
        }
    }
    source.to_string()
}

/// Run rustfmt on source code
pub fn run_rustfmt(source: &str, edition: &str) -> Result<String, String> {
    let mut child = Command::new("rustfmt")
        .args(["--edition", edition])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn rustfmt: {}", e))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(source.as_bytes())
            .map_err(|e| format!("Failed to write to rustfmt: {}", e))?;
    }

    let output = child.wait_with_output()
        .map_err(|e| format!("Failed to wait for rustfmt: {}", e))?;

    if output.status.success() {
        String::from_utf8(output.stdout)
            .map_err(|e| format!("rustfmt output not UTF-8: {}", e))
    } else {
        Err(format!("rustfmt failed: {}", String::from_utf8_lossy(&output.stderr)))
    }
}

/// Fix bare trait objects (add `dyn`)
pub fn fix_bare_trait_objects(source: &str) -> String {
    let common_traits = [
        "Error", "Read", "Write", "Seek", "BufRead",
        "Iterator", "Display", "Debug",
    ];

    let mut result = source.to_string();

    for trait_name in &common_traits {
        // &Trait -> &dyn Trait
        let pattern = format!("&{}", trait_name);
        let replacement = format!("&dyn {}", trait_name);
        result = fix_trait_reference(&result, &pattern, &replacement, trait_name);

        // Box<Trait> -> Box<dyn Trait>
        let pattern = format!("Box<{}>", trait_name);
        let replacement = format!("Box<dyn {}>", trait_name);
        result = result.replace(&pattern, &replacement);
    }

    result
}

fn fix_trait_reference(source: &str, pattern: &str, replacement: &str, _trait_name: &str) -> String {
    let mut result = String::new();
    let mut last_end = 0;

    for (start, _) in source.match_indices(pattern) {
        let after_pos = start + pattern.len();
        let char_after = source[after_pos..].chars().next();

        let is_word_boundary = match char_after {
            Some(c) => !c.is_alphanumeric() && c != '_',
            None => true,
        };

        let already_dyn = {
            let before_start = if start >= 4 { start - 4 } else { 0 };
            source[before_start..start].contains("dyn ")
        };

        if is_word_boundary && !already_dyn {
            result.push_str(&source[last_end..start]);
            result.push_str(replacement);
            last_end = after_pos;
        }
    }

    result.push_str(&source[last_end..]);
    result
}

/// Fix visibility issues when items are flattened to lib.rs.
/// Converts `pub(super)` to `pub(crate)` and fixes excessive super:: paths.
pub fn fix_flattened_visibility(source: &str) -> String {
    let mut result = source.to_string();

    // Replace pub(super) with pub(crate) - it's invalid at lib.rs root level
    result = result.replace("pub(super)", "pub(crate)");
    result = result.replace("pub (super)", "pub(crate)");

    // Fix super:: paths that go too far up - replace with crate::
    // Pattern: super::super:: or more
    while result.contains("super::super::") {
        result = result.replace("super::super::", "crate::");
    }

    result
}

/// Extract identifiers from source code
pub fn extract_identifiers_from_source(source: &str) -> HashSet<String> {
    let mut identifiers = HashSet::new();
    let mut current = String::new();

    for c in source.chars() {
        if c.is_alphanumeric() || c == '_' {
            current.push(c);
        } else {
            if !current.is_empty() {
                identifiers.insert(current.clone());
                current.clear();
            }
        }
    }

    if !current.is_empty() {
        identifiers.insert(current);
    }

    identifiers
}

/// Check if a string is a Rust keyword or primitive
pub fn is_rust_keyword_or_primitive(s: &str) -> bool {
    matches!(s,
        "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" |
        "extern" | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" |
        "loop" | "match" | "mod" | "move" | "mut" | "pub" | "ref" | "return" |
        "self" | "Self" | "static" | "struct" | "super" | "trait" | "true" |
        "type" | "unsafe" | "use" | "where" | "while" | "async" | "await" |
        "dyn" | "bool" | "char" | "str" | "i8" | "i16" | "i32" | "i64" |
        "i128" | "isize" | "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
        "f32" | "f64"
    )
}

/// Fix broken crate-internal imports.
/// When slicing reorganizes items, imports like `use crate::foo::Bar` may break
/// if Bar is now at `crate::Bar` instead. This function rewrites such imports.
///
/// `item_locations` maps item names to their module paths in the sliced crate.
/// For items at root, the path is empty string.
pub fn fix_crate_imports(
    source: &str,
    item_locations: &std::collections::HashMap<String, String>,
) -> String {
    let mut result = String::new();
    let mut lines = source.lines().peekable();

    while let Some(line) = lines.next() {
        let trimmed = line.trim();

        // Check if this is a crate-internal import
        if trimmed.starts_with("use crate::") && trimmed.ends_with(';') {
            if let Some(fixed) = fix_single_crate_import(trimmed, item_locations) {
                result.push_str(&fixed);
                result.push('\n');
                continue;
            }
        }

        result.push_str(line);
        result.push('\n');
    }

    // Remove trailing newline if original didn't have one
    if !source.ends_with('\n') && result.ends_with('\n') {
        result.pop();
    }

    result
}

/// Fix a single crate import line
fn fix_single_crate_import(
    line: &str,
    item_locations: &std::collections::HashMap<String, String>,
) -> Option<String> {
    // Parse: use crate::foo::bar::Item;
    let import_path = line
        .trim_start_matches("use crate::")
        .trim_end_matches(';')
        .trim();

    // Handle grouped imports: use crate::foo::{A, B};
    if import_path.contains('{') {
        return fix_grouped_import(line, item_locations);
    }

    // Get the item name (last component)
    let parts: Vec<&str> = import_path.split("::").collect();
    if parts.is_empty() {
        return None;
    }

    let item_name = *parts.last().unwrap();

    // Check if we know where this item actually is
    if let Some(actual_module) = item_locations.get(item_name) {
        let current_module = if parts.len() > 1 {
            parts[..parts.len()-1].join("::")
        } else {
            String::new()
        };

        // Only fix if the paths don't match
        if &current_module != actual_module {
            let new_import = if actual_module.is_empty() {
                format!("use crate::{};", item_name)
            } else {
                format!("use crate::{}::{};", actual_module, item_name)
            };
            return Some(new_import);
        }
    }

    None
}

/// Fix grouped imports like `use crate::foo::{A, B};`
fn fix_grouped_import(
    line: &str,
    item_locations: &std::collections::HashMap<String, String>,
) -> Option<String> {
    // Parse: use crate::foo::{A, B, C};
    let after_crate = line.trim_start_matches("use crate::").trim_end_matches(';');

    // Find the opening brace
    let brace_pos = after_crate.find('{')?;
    let base_path = after_crate[..brace_pos].trim_end_matches("::");
    let items_str = after_crate[brace_pos+1..].trim_end_matches('}');

    // Parse items
    let items: Vec<&str> = items_str.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()).collect();

    // Check each item and potentially regroup
    let mut by_module: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();

    for item in items {
        // Handle `self` specially
        if item == "self" {
            by_module.entry(base_path.to_string())
                .or_default()
                .push("self".to_string());
            continue;
        }

        // Get the actual location of this item
        let actual_module = item_locations.get(item)
            .map(|s| s.as_str())
            .unwrap_or(base_path);

        by_module.entry(actual_module.to_string())
            .or_default()
            .push(item.to_string());
    }

    // If all items are in the expected module, no change needed
    if by_module.len() == 1 && by_module.contains_key(base_path) {
        return None;
    }

    // Generate new import statements
    let mut new_imports: Vec<String> = Vec::new();
    for (module, items) in by_module {
        if items.len() == 1 && items[0] != "self" {
            if module.is_empty() {
                new_imports.push(format!("use crate::{};", items[0]));
            } else {
                new_imports.push(format!("use crate::{}::{};", module, items[0]));
            }
        } else if !items.is_empty() {
            let items_joined = items.join(", ");
            if module.is_empty() {
                new_imports.push(format!("use crate::{{{}}};", items_joined));
            } else {
                new_imports.push(format!("use crate::{}::{{{}}};", module, items_joined));
            }
        }
    }

    Some(new_imports.join("\n"))
}

/// Build item location map by scanning all module files in a sliced crate
pub fn build_item_location_map(output_dir: &std::path::Path) -> std::collections::HashMap<String, String> {
    let mut locations: std::collections::HashMap<String, String> = std::collections::HashMap::new();
    let src_dir = output_dir.join("src");

    if let Err(_) = scan_module_for_items(&src_dir, "", &mut locations) {
        // Ignore errors - best effort
    }

    locations
}

/// Scan a module file/directory for item definitions
fn scan_module_for_items(
    path: &std::path::Path,
    module_path: &str,
    locations: &mut std::collections::HashMap<String, String>,
) -> std::io::Result<()> {
    use std::fs;

    if path.is_file() {
        let content = fs::read_to_string(path)?;
        extract_defined_items(&content, module_path, locations);
    } else if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let file_path = entry.path();

            if file_path.is_file() && file_path.extension().map(|e| e == "rs").unwrap_or(false) {
                let file_name = file_path.file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("");

                let this_module = if file_name == "lib" || file_name == "mod" {
                    module_path.to_string()
                } else if module_path.is_empty() {
                    file_name.to_string()
                } else {
                    format!("{}::{}", module_path, file_name)
                };

                let content = fs::read_to_string(&file_path)?;
                extract_defined_items(&content, &this_module, locations);
            } else if file_path.is_dir() {
                let dir_name = file_path.file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or("");
                let sub_module = if module_path.is_empty() {
                    dir_name.to_string()
                } else {
                    format!("{}::{}", module_path, dir_name)
                };
                scan_module_for_items(&file_path, &sub_module, locations)?;
            }
        }
    }

    Ok(())
}

/// Extract item definitions from source content
fn extract_defined_items(
    content: &str,
    module_path: &str,
    locations: &mut std::collections::HashMap<String, String>,
) {
    // Simple regex-free parsing for pub struct/enum/fn/type/const/static/trait/mod
    let patterns = [
        "pub struct ",
        "pub enum ",
        "pub fn ",
        "pub type ",
        "pub const ",
        "pub static ",
        "pub trait ",
        "pub mod ",
        "pub union ",
        "struct ",
        "enum ",
        "type ",
        "const ",
        "static ",
        "trait ",
        "union ",
    ];

    for line in content.lines() {
        let trimmed = line.trim();

        // Skip impl blocks, they define methods not items
        if trimmed.starts_with("impl ") {
            continue;
        }

        for pattern in &patterns {
            if trimmed.starts_with(pattern) || trimmed.contains(&format!(" {}", pattern)) {
                let after_pattern = if let Some(pos) = trimmed.find(pattern) {
                    &trimmed[pos + pattern.len()..]
                } else {
                    continue;
                };

                // Extract the identifier (first word before < { ( :)
                let mut name = String::new();
                for c in after_pattern.chars() {
                    if c.is_alphanumeric() || c == '_' {
                        name.push(c);
                    } else {
                        break;
                    }
                }

                if !name.is_empty() && name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    // Only track type-like items (start with uppercase)
                    locations.insert(name, module_path.to_string());
                }
                break;
            }
        }
    }
}

/// Extract macro invocations from source code
pub fn extract_macro_invocations(source: &str) -> HashSet<String> {
    let mut macros = HashSet::new();
    let mut chars = source.chars().peekable();

    while let Some(c) = chars.next() {
        if c.is_alphabetic() || c == '_' {
            let mut name = String::new();
            name.push(c);

            while let Some(&nc) = chars.peek() {
                if nc.is_alphanumeric() || nc == '_' {
                    name.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            if let Some(&'!') = chars.peek() {
                macros.insert(name);
            }
        }
    }

    macros
}

/// Extract file paths from include_bytes! and include_str! macros
pub fn extract_included_files(source: &str) -> HashSet<String> {
    let mut files = HashSet::new();

    // Pattern: include_bytes!("path") or include_str!("path")
    // Also handle syn-generated: include_bytes ! ("path")
    for macro_name in &["include_bytes", "include_str"] {
        // Try both with and without space before !
        for pattern in &[format!("{}!", macro_name), format!("{} !", macro_name)] {
            let mut start = 0;

            while let Some(pos) = source[start..].find(pattern.as_str()) {
                let abs_pos = start + pos + pattern.len();
                start = abs_pos;

                // Find the opening delimiter (usually '(' or '[')
                let rest = &source[abs_pos..];
                let trimmed = rest.trim_start();

                // Skip whitespace and find opening paren/bracket
                let _open_char = match trimmed.chars().next() {
                    Some('(') | Some('[') => trimmed.chars().next().unwrap(),
                    _ => continue,
                };

                // Find the string literal inside
                if let Some(quote_start) = trimmed.find('"') {
                    let after_quote = &trimmed[quote_start + 1..];
                    if let Some(quote_end) = after_quote.find('"') {
                        let path = &after_quote[..quote_end];
                        files.insert(path.to_string());
                    }
                }
            }
        }
    }

    files
}

/// Copy included files from source crate to output directory
pub fn copy_included_files(
    source_dir: &std::path::Path,
    output_dir: &std::path::Path,
) -> std::io::Result<usize> {
    let src_dir = output_dir.join("src");
    copy_included_files_recursive(source_dir, &src_dir)
}

/// Recursively scan directories for .rs files and copy included files
fn copy_included_files_recursive(
    source_dir: &std::path::Path,
    scan_dir: &std::path::Path,
) -> std::io::Result<usize> {
    use std::fs;

    let mut copied = 0;

    if !scan_dir.exists() {
        return Ok(0);
    }

    // Scan all entries in the directory
    for entry in fs::read_dir(scan_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().map(|e| e == "rs").unwrap_or(false) {
            let content = fs::read_to_string(&path)?;
            let included = extract_included_files(&content);

            for rel_path in included {
                // The path is relative to the .rs file's location
                let rs_dir = path.parent().unwrap_or(scan_dir);

                // Source file location
                let source_file = source_dir.join("src").join(&rel_path);

                // Destination in sliced crate
                let dest_file = rs_dir.join(&rel_path);

                if source_file.exists() && !dest_file.exists() {
                    // Create parent directories if needed
                    if let Some(parent) = dest_file.parent() {
                        fs::create_dir_all(parent)?;
                    }

                    fs::copy(&source_file, &dest_file)?;
                    copied += 1;
                }
            }
        } else if path.is_dir() {
            // Recursively process subdirectories
            copied += copy_included_files_recursive(source_dir, &path)?;
        }
    }

    Ok(copied)
}

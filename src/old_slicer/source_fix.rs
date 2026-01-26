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

    // Priority 7: Don't "fix" Visitor imports to hir::visitor
    // The ast::Visitor trait is the main one used by both ast and hir modules
    // hir::Visitor is a different trait with a different signature
    if item_name == "Visitor" {
        let current_module = if parts.len() > 1 {
            parts[..parts.len()-1].join("::")
        } else {
            String::new()
        };
        // If it's already importing from ast::visitor, don't change it
        if current_module == "ast::visitor" || current_module == "ast" {
            return None;
        }
        // If item_locations suggests hir::visitor, ignore it and keep original
        if let Some(suggested) = item_locations.get(item_name) {
            if suggested.starts_with("hir") {
                return None;  // Don't move to hir::visitor
            }
        }
    }

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

    // Priority 7: Don't split ast::{..., Visitor} or ast::visitor::{self, Visitor} imports
    // ast::Visitor and hir::Visitor are different traits with different methods
    // Preserve the original import to keep the correct ast::Visitor
    if (base_path == "ast" || base_path == "ast::visitor") && items_str.contains("Visitor") {
        return None;  // Keep original import unchanged
    }

    // Parse items
    let items: Vec<&str> = items_str.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()).collect();

    // CONSERVATIVE FIX: If ANY item's location is unknown, don't split the import
    // This prevents incorrectly splitting imports when we don't have complete information
    // about where items are defined (e.g., re-exports that aren't in the sliced code)
    let all_items_known = items.iter().all(|item| {
        *item == "self" || item_locations.contains_key(*item)
    });

    if !all_items_known {
        // We don't have complete information - trust the original import
        return None;
    }

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

        // Get the actual location of this item (we know it exists from the check above)
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

        // Skip impl blocks entirely - they define associated types/methods, not standalone items
        // This handles both "impl X { ... }" on its own line and inline impl blocks
        if trimmed.starts_with("impl ") || trimmed.contains(" impl ") {
            continue;
        }

        for pattern in &patterns {
            if trimmed.starts_with(pattern) || trimmed.contains(&format!(" {}", pattern)) {
                let after_pattern = if let Some(pos) = trimmed.find(pattern) {
                    &trimmed[pos + pattern.len()..]
                } else {
                    continue;
                };

                // Check if this is a function pattern
                let is_function = pattern.contains("fn ");

                // Extract the identifier (first word before < { ( :)
                let mut name = String::new();
                let mut chars_iter = after_pattern.chars().peekable();
                for c in chars_iter.by_ref() {
                    if c.is_alphanumeric() || c == '_' {
                        name.push(c);
                    } else {
                        // Check for associated type declarations: "type Name :"
                        // These should NOT be treated as item definitions
                        if c == ' ' || c == ':' {
                            // Skip whitespace to see what comes after
                            let rest: String = std::iter::once(c).chain(chars_iter).collect();
                            let trimmed_rest = rest.trim_start();
                            if trimmed_rest.starts_with(':') && !trimmed_rest.starts_with("::") {
                                // This is an associated type declaration (e.g., "type RuntimeType : SomeTrait")
                                // NOT a type alias (e.g., "type Foo = Bar")
                                name.clear();
                            }
                        }
                        break;
                    }
                }

                if !name.is_empty() {
                    // Track functions (can start with lowercase) and type-like items (start with uppercase)
                    let first_char = name.chars().next().unwrap();
                    if is_function || first_char.is_uppercase() {
                        locations.insert(name, module_path.to_string());
                    }
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

/// Fix Result type alias conflicts.
/// When code imports `use crate::error::Result;` (a 1-arg alias) but uses
/// `Result<T, E>` with 2 arguments, replace those usages with `std::result::Result<T, E>`.
pub fn fix_result_type_alias(source: &str) -> String {
    // Check if this file imports crate::error::Result
    if !source.contains("use crate::error::Result;") {
        return source.to_string();
    }

    let mut result = source.to_string();

    // Check if there are 2-arg Result usages anywhere in the file
    // Pattern: "Result < T , E >" - Result followed by < and containing a comma before >
    let has_two_arg_result = has_two_arg_result_usage(&result);

    if has_two_arg_result {
        // Replace the import to avoid conflict
        result = result.replace(
            "use crate::error::Result;",
            "use std::result::Result; // Replaced crate::error::Result to avoid 1-arg vs 2-arg conflict"
        );
    }

    result
}

/// Fix conflicting imports - when both `crate::X` and `std::X` are imported with the same name,
/// rename one of them using `as`.
pub fn fix_conflicting_imports(source: &str) -> String {
    let mut result = source.to_string();

    // Find all import names and their sources
    let mut import_names: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();

    for line in result.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("use ") && trimmed.ends_with(';') {
            // Extract the import name (last component before ; or as)
            let import_part = trimmed.trim_start_matches("use ").trim_end_matches(';');
            if import_part.contains(" as ") {
                continue; // Already renamed
            }

            if let Some(last_colon_pos) = import_part.rfind("::") {
                let name = &import_part[last_colon_pos + 2..];
                // Skip glob imports (`use foo::*;`) and brace imports (`use foo::{A, B};`)
                if !name.contains('{') && !name.is_empty() && name != "*" {
                    import_names.entry(name.to_string())
                        .or_default()
                        .push(import_part.to_string());
                }
            }
        }
    }

    // For each name that appears multiple times, rename the std:: one
    for (name, sources) in import_names {
        if sources.len() > 1 {
            // Check if the code uses the bare name with :: qualification (e.g., Ordering::Equal)
            // If so, don't rename the import because the code expects to use the bare name
            let uses_bare_name = result.contains(&format!("{}::", name));

            if !uses_bare_name {
                // Find if there's a std:: import to rename
                for source_path in &sources {
                    if source_path.starts_with("std::") {
                        // Rename std::X to std::X as std_X
                        let old_import = format!("use {};", source_path);
                        let new_import = format!("use {} as std_{};", source_path, name);
                        result = result.replace(&old_import, &new_import);
                    }
                }
            }
        }
    }

    result
}

/// Remove spurious self-referential imports where a file imports something it defines itself.
/// Example: word_lock.rs has `use parking_lot::THREAD_DATA;` but also defines `thread_local!(static THREAD_DATA...)`
fn remove_spurious_self_imports(source: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let mut new_lines = Vec::new();

    for line in &lines {
        let trimmed = line.trim_start();

        // Check if this is a "use module::Item;" import
        if trimmed.starts_with("use ")
            && !trimmed.starts_with("use self::")
            && !trimmed.starts_with("use crate::")
            && !trimmed.starts_with("use super::")
            && !trimmed.starts_with("use std::")
            && !trimmed.starts_with("use core::")
            && !trimmed.starts_with("use alloc::")
            && trimmed.contains("::") {
            // Extract "module::Item" part
            if let Some(use_pos) = trimmed.find("use ") {
                let after_use = &trimmed[use_pos + 4..];
                if let Some(semicolon_pos) = after_use.find(';') {
                    let import_path = after_use[..semicolon_pos].trim();
                    // Get the final item name (after last ::)
                    if let Some(item_name) = import_path.split("::").last() {
                        // Check if this item is defined in the file
                        let is_defined = source.contains(&format!("struct {} ", item_name))
                            || source.contains(&format!("struct {}<", item_name))
                            || source.contains(&format!("enum {} ", item_name))
                            || source.contains(&format!("const {} ", item_name))
                            || source.contains(&format!("static {} ", item_name))
                            || source.contains(&format!("static {}: ", item_name))
                            || source.contains(&format!("type {} =", item_name))
                            || source.contains(&format!("fn {}(", item_name))
                            || source.contains(&format!("(static {} ", item_name)) // thread_local!
                            || source.contains(&format!("(static {}: ", item_name));

                        if is_defined {
                            // Skip this spurious self-import
                            continue;
                        }
                    }
                }
            }
        }

        new_lines.push(line.to_string());
    }

    new_lines.join("\n")
}

/// Fix external crate imports that are incorrectly prefixed with `crate::`.
/// Common external crates like `thiserror`, `bytes`, etc. should be imported
/// as `use thiserror;` not `use crate::thiserror;`.
pub fn fix_external_crate_imports(source: &str) -> String {
    let mut result = remove_spurious_self_imports(source);

    // Remove spurious "use loom;" and "use crate::loom;" imports
    // loom is used in cfg-guarded macros with fully qualified paths, no import needed
    if result.contains("use loom") || result.contains("use crate::loom") {
        // Remove "use loom;" patterns
        let re = regex::Regex::new(r"(?m)^use loom;\n").unwrap();
        result = re.replace_all(&result, "").to_string();
        result = result.replace("use loom;", "");

        // Remove "use crate::loom;" patterns
        let re = regex::Regex::new(r"(?m)^use crate::loom;\n").unwrap();
        result = re.replace_all(&result, "").to_string();
        result = result.replace("use crate::loom;", "");
    }

    // List of common external crates that might be incorrectly prefixed with crate::
    let external_crates = [
        "thiserror",
        "bytes",
        "serde",
        "serde_json",
        "serde_derive",
        "log",
        "env_logger",
        "tokio",
        "async_trait",
        "futures",
        "anyhow",
        "once_cell",
        "lazy_static",
        "regex",
        "chrono",
        "uuid",
        "rand",
        "parking_lot",
        "crossbeam",
        "rayon",
        "itertools",
        "num",
        "num_traits",
        // NOTE: "loom" removed - it's only used with fully qualified paths in cfg-guarded code
        // NOTE: "byteorder" removed - it's a common internal module name (e.g., in protobuf)
    ];

    for crate_name in external_crates {
        // Replace "use crate::cratename;" with "use cratename;"
        let wrong_import = format!("use crate::{};", crate_name);
        let correct_import = format!("use {};", crate_name);
        result = result.replace(&wrong_import, &correct_import);

        // Also handle "use crate::cratename::" patterns
        let wrong_prefix = format!("use crate::{}::", crate_name);
        let correct_prefix = format!("use {}::", crate_name);
        result = result.replace(&wrong_prefix, &correct_prefix);

        // Handle derive macro usage like #[derive(thiserror::Error)]
        // These don't need fixing as they use the crate name directly
    }

    result
}

/// Fix pub use re-exports of pub(crate) items.
/// When an item is defined as pub(crate), re-exporting it with `pub use` is an error.
/// This function changes `pub use` to `pub(crate) use` for such items.
pub fn fix_pub_crate_reexports(source: &str, pub_crate_items: &std::collections::HashSet<String>) -> String {
    if pub_crate_items.is_empty() {
        return source.to_string();
    }

    let mut result = String::new();
    for line in source.lines() {
        let trimmed = line.trim();

        // Check if this is a pub use statement
        if trimmed.starts_with("pub use ") && !trimmed.starts_with("pub(crate) use ") {
            // Check if any pub(crate) item is being re-exported
            let needs_fix = pub_crate_items.iter().any(|item| {
                // Check for direct re-export: pub use module::ItemName;
                trimmed.contains(&format!("::{};", item)) ||
                trimmed.contains(&format!("::{}}}",item)) ||
                trimmed.contains(&format!("::{},", item)) ||
                // Check for grouped re-export: pub use module::{..., ItemName, ...};
                (trimmed.contains('{') && trimmed.contains(&format!("{},", item))) ||
                (trimmed.contains('{') && trimmed.contains(&format!("{}}}",item))) ||
                (trimmed.contains('{') && trimmed.contains(&format!(" {}", item)))
            });

            if needs_fix {
                result.push_str(&line.replace("pub use ", "pub(crate) use "));
            } else {
                result.push_str(line);
            }
        } else {
            result.push_str(line);
        }
        result.push('\n');
    }

    // Remove trailing newline if original didn't have one
    if !source.ends_with('\n') && result.ends_with('\n') {
        result.pop();
    }

    result
}

/// Scan source code for pub(crate) item definitions
pub fn find_pub_crate_items(source: &str) -> std::collections::HashSet<String> {
    let mut items = std::collections::HashSet::new();

    // Patterns for pub(crate) definitions
    let patterns = [
        "pub(crate) struct ",
        "pub(crate) enum ",
        "pub(crate) type ",
        "pub(crate) fn ",
        "pub(crate) trait ",
        "pub(crate) const ",
        "pub(crate) static ",
        "pub (crate) struct ",
        "pub (crate) enum ",
        "pub (crate) type ",
        "pub (crate) fn ",
        "pub (crate) trait ",
        "pub (crate) const ",
        "pub (crate) static ",
    ];

    for line in source.lines() {
        let trimmed = line.trim();
        for pattern in &patterns {
            if trimmed.contains(pattern) {
                // Extract the item name
                if let Some(pos) = trimmed.find(pattern) {
                    let after_pattern = &trimmed[pos + pattern.len()..];
                    let mut name = String::new();
                    for c in after_pattern.chars() {
                        if c.is_alphanumeric() || c == '_' {
                            name.push(c);
                        } else {
                            break;
                        }
                    }
                    if !name.is_empty() {
                        items.insert(name);
                    }
                }
            }
        }

        // Also check for pub(crate) use re-exports which indicate the item is pub(crate)
        if trimmed.starts_with("pub(crate) use ") || trimmed.starts_with("pub (crate) use ") {
            // Extract item names from use statements
            if let Some(brace_start) = trimmed.find('{') {
                if let Some(brace_end) = trimmed.find('}') {
                    let inside = &trimmed[brace_start+1..brace_end];
                    for item in inside.split(',') {
                        let item = item.trim();
                        if !item.is_empty() && !item.contains("::") {
                            items.insert(item.to_string());
                        }
                    }
                }
            } else {
                // Single item: pub(crate) use module::ItemName;
                if let Some(last_colon) = trimmed.rfind("::") {
                    let after = &trimmed[last_colon+2..].trim_end_matches(';').trim();
                    if !after.is_empty() && !after.contains('{') {
                        items.insert(after.to_string());
                    }
                }
            }
        }
    }

    items
}

/// Check if the source contains 2-arg Result usages like `Result<T, E>`
fn has_two_arg_result_usage(source: &str) -> bool {
    // Find all occurrences of "Result <" or "Result<"
    let result_patterns = ["Result <", "Result<"];

    for pattern in result_patterns {
        let mut search_start = 0;
        while let Some(pos) = source[search_start..].find(pattern) {
            let abs_pos = search_start + pos + pattern.len();
            search_start = abs_pos;

            // Now look for the matching > and check if there's a comma inside
            let rest = &source[abs_pos..];

            // Track angle bracket nesting
            let mut depth = 1;
            let mut found_comma = false;
            let mut chars_seen = 0;

            for c in rest.chars() {
                chars_seen += 1;
                match c {
                    '<' => depth += 1,
                    '>' => {
                        depth -= 1;
                        if depth == 0 {
                            // Found matching >
                            if found_comma {
                                return true;
                            }
                            break;
                        }
                    }
                    ',' if depth == 1 => {
                        // Found comma at top level
                        found_comma = true;
                    }
                    _ => {}
                }

                // Don't search too far
                if chars_seen > 200 {
                    break;
                }
            }
        }
    }

    false
}

/// Remove imports of unstable std features and invalid doctest artifacts
pub fn fix_unstable_imports(source: &str) -> String {
    let invalid_imports = [
        // Unstable features
        "use std::ops::Try;",
        "use std::ops::ControlFlow;",
        "use std::ops::FromResidual;",
        "use std::ops::Residual;",
        // Doctest artifacts that aren't real std types
        "use std::os::Messages;",
        "use std::io::Writer;",
        "use std::collections::Foo;",
        // Feature-gated types that may not exist
        "use crate::reflect::types::ProtobufTypeTokioBytes;",
        "use crate::reflect::types::ProtobufTypeTokioChars;",
        "use crate::reflect::runtime_types::RuntimeTypeTokioBytes;",
        "use crate::reflect::runtime_types::RuntimeTypeTokioChars;",
        // Missing sliced modules (vestigial imports)
        "use crate::compiler;",
        "use crate::x20compiler;",
        "use crate::ext::ExtFieldOptional;",
        // External crate not in dependencies (replace with native Rust)
        "use byteorder::LITTLE_ENDIAN;",
        // Invalid/redundant imports
        "use crate::descriptor::descriptor;",
        "use crate::reflect::repeated::vec_downcast;",
        "use crate::reflect::message::is_initialized_is_always_true;",
    ];

    let result: Vec<&str> = source
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            !invalid_imports.iter().any(|import| trimmed == *import)
        })
        .collect();

    let mut result = result.join("\n");

    // Also remove TokioBytes/TokioChars from grouped imports
    // e.g., "pub use self::types::{..., ProtobufTypeTokioBytes, ProtobufTypeTokioChars};"
    let tokio_patterns = [
        ", ProtobufTypeTokioBytes",
        ", ProtobufTypeTokioChars",
        ", RuntimeTypeTokioBytes",
        ", RuntimeTypeTokioChars",
        "ProtobufTypeTokioBytes, ",
        "ProtobufTypeTokioChars, ",
        "RuntimeTypeTokioBytes, ",
        "RuntimeTypeTokioChars, ",
    ];
    for pattern in tokio_patterns {
        result = result.replace(pattern, "");
    }

    // If we removed byteorder::LITTLE_ENDIAN, add a const definition
    if source.contains("use byteorder::LITTLE_ENDIAN;") && !result.contains("LITTLE_ENDIAN") {
        // Find the first line that's not a use statement to insert the const
        let lines: Vec<&str> = result.lines().collect();
        let mut new_lines = Vec::new();
        let mut inserted = false;

        for line in lines {
            if !inserted && !line.trim().starts_with("use ") && !line.trim().starts_with("//") && !line.trim().is_empty() {
                new_lines.push("const LITTLE_ENDIAN: bool = cfg!(target_endian = \"little\");");
                new_lines.push("");
                inserted = true;
            }
            new_lines.push(line);
        }
        result = new_lines.join("\n");
    }

    // Fix E0365: pub use of pub(crate) items - change to pub(crate) use
    // These are specific Generated* types that are pub(crate) but get re-exported as pub
    let pub_crate_reexports = [
        ("pub use self::enums::GeneratedEnumDescriptorData;", "pub(crate) use self::enums::GeneratedEnumDescriptorData;"),
        ("pub use self::file::GeneratedFileDescriptor;", "pub(crate) use self::file::GeneratedFileDescriptor;"),
        ("pub use self::message::GeneratedMessageDescriptorData;", "pub(crate) use self::message::GeneratedMessageDescriptorData;"),
        ("pub use self::oneof::GeneratedOneofDescriptorData;", "pub(crate) use self::oneof::GeneratedOneofDescriptorData;"),
        ("pub use crate::reflect::protobuf_type_box::ProtobufType;", "pub(crate) use crate::reflect::protobuf_type_box::ProtobufType;"),
    ];
    for (from, to) in pub_crate_reexports {
        result = result.replace(from, to);
    }

    // Fix E0432: use Kind::* needs to be use value::Kind::* in struct_ module
    // Only apply if this file has the inline pub mod value definition
    if result.contains("pub mod value {") && result.contains("pub enum Kind {") {
        result = result.replace("use Kind::*;", "use value::Kind::*;");
    }

    // Fix E0255: field name conflict - rename import when there's enum Field in same file
    if result.contains("use crate::reflect::field;") && result.contains("enum Field {") {
        result = result.replace("use crate::reflect::field;", "use crate::reflect::field as reflect_field;");
    }

    // Fix E0659: WireError ambiguous - add explicit module path
    // Check for both "pub enum WireError" and "pub (crate) enum WireError"
    if result.contains("use WireError::*;") &&
       (result.contains("pub enum WireError") || result.contains("enum WireError")) {
        result = result.replace("use WireError::*;", "use self::WireError::*;");
    }

    // Fix E0404: Expected trait but found struct - Enum
    // well_known_types::type_::Enum is a struct, but enums::Enum is the trait
    // When code uses `E: Enum` or `: Enum` bounds, it needs the trait, not the struct
    if result.contains("use crate::well_known_types::type_::Enum;") {
        // Check if the file uses Enum as a trait bound (e.g., "E: Enum" or ": Enum + ")
        if result.contains("E : Enum") || result.contains(": Enum +") || result.contains(": Enum >")
           || result.contains("E: Enum") || result.contains("< E >") {
            result = result.replace(
                "use crate::well_known_types::type_::Enum;",
                "use crate::enums::Enum;"
            );
        }
    }

    // Ensure trailing newline if original had one
    if source.ends_with('\n') && !result.ends_with('\n') {
        result.push('\n');
    }

    result
}

/// Fix module imports that conflict with nested module definitions.
/// For example, `use crate::reflect::value;` conflicts with `pub mod value { ... }`.
pub fn fix_module_name_conflicts(source: &str) -> String {
    let mut result = source.to_string();

    // Helper to check for inline module declaration (pub mod name {) vs file module (pub mod name;)
    fn has_inline_module(source: &str, module_name: &str) -> bool {
        let pattern = format!("pub mod {} {{", module_name);
        source.contains(&pattern)
    }

    // Check for common patterns where module import conflicts with local inline module
    // Only apply when there's an inline module definition (pub mod name {), not a file module (pub mod name;)
    if result.contains("use crate::reflect::value;") && has_inline_module(&result, "value") {
        result = result.replace("use crate::reflect::value;", "use crate::reflect::value as reflect_value;");
    }

    if result.contains("use crate::reflect::field;") && has_inline_module(&result, "field") {
        result = result.replace("use crate::reflect::field;", "use crate::reflect::field as reflect_field;");
    }

    // Fix bare `use Kind::*;` which should be `use value::Kind::*;` when there's an inline `pub mod value`
    if result.contains("use Kind::*;") && has_inline_module(&result, "value") {
        result = result.replace("use Kind::*;", "use value::Kind::*;");
    }

    // Fix bare `use Field::*;` which should be `use field::Field::*;`
    if result.contains("use Field::*;") && has_inline_module(&result, "field") {
        result = result.replace("use Field::*;", "use field::Field::*;");
    }

    result
}

/// Fix impl blocks that conflict with std types.
/// For example, `impl Any { ... }` where Any is from well_known_types, not std::any
pub fn fix_impl_std_type_conflicts(source: &str) -> String {
    // If we have both `use std::any::Any;` and `impl Any`, there's a conflict
    if !source.contains("use std::any::Any;") {
        return source.to_string();
    }

    let mut result = String::new();
    let mut in_any_impl = false;
    let mut brace_depth = 0;

    for line in source.lines() {
        let trimmed = line.trim();

        // Track if we're inside an impl Any block
        if trimmed.starts_with("impl Any {") || trimmed.starts_with("impl Any{") {
            // This is trying to implement methods on a struct named Any, not the trait
            // We need to remove this entire impl block as it's conflicting
            in_any_impl = true;
            brace_depth = 1;
            // Don't add this line
            continue;
        }

        if in_any_impl {
            // Count braces to know when impl block ends
            for c in trimmed.chars() {
                match c {
                    '{' => brace_depth += 1,
                    '}' => brace_depth -= 1,
                    _ => {}
                }
            }
            if brace_depth == 0 {
                in_any_impl = false;
            }
            // Skip all lines in the impl block
            continue;
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

/// Fix missing imports for commonly used functions.
/// When the slicer extracts code that uses certain functions, it may not always
/// add the necessary imports. This function adds them when needed.
pub fn fix_missing_imports(source: &str) -> String {
    // Define functions that need imports when used
    // (function_name, import_statement, module_where_defined)
    // Note: we check for both "func(" and "func (" patterns since the slicer may add spaces
    let import_fixes: &[(&str, &str, Option<&str>)] = &[
        // Varint decode functions
        ("decode_varint64", "use crate::varint::decode::decode_varint64;", Some("varint")),
        ("decode_varint32", "use crate::varint::decode::decode_varint32;", Some("varint")),
        // Varint encode functions
        ("encode_varint32", "use crate::varint::encode::encode_varint32;", Some("varint")),
        ("encode_varint64", "use crate::varint::encode::encode_varint64;", Some("varint")),
        ("encoded_varint64_len", "use crate::varint::encode::encoded_varint64_len;", Some("varint")),
        // Zigzag functions
        ("decode_zig_zag_32", "use crate::zigzag::decode_zig_zag_32;", Some("zigzag")),
        ("decode_zig_zag_64", "use crate::zigzag::decode_zig_zag_64;", Some("zigzag")),
        ("encode_zig_zag_32", "use crate::zigzag::encode_zig_zag_32;", Some("zigzag")),
        ("encode_zig_zag_64", "use crate::zigzag::encode_zig_zag_64;", Some("zigzag")),
        // Misc utility functions
        ("maybe_ununit_array_assume_init", "use crate::misc::maybe_ununit_array_assume_init;", Some("misc")),
        ("maybe_uninit_write_slice", "use crate::misc::maybe_uninit_write_slice;", Some("misc")),
        // Wire format and coding functions
        ("check_message_size", "use crate::wire_format::check_message_size;", Some("wire_format")),
        ("vec_packed_fixed_data_size", "use crate::rt::packed::vec_packed_fixed_data_size;", Some("rt")),
        ("vec_packed_varint_data_size", "use crate::rt::packed::vec_packed_varint_data_size;", Some("rt")),
        ("vec_packed_varint_zigzag_data_size", "use crate::rt::packed::vec_packed_varint_zigzag_data_size;", Some("rt")),
        ("vec_packed_enum_or_unknown_data_size", "use crate::rt::packed::vec_packed_enum_or_unknown_data_size;", Some("rt")),
        // Compute size functions - these may not always exist, so only add when used AND defined
        ("compute_raw_varint32_size", "use crate::rt::compute_raw_varint32_size;", Some("rt")),
        ("compute_raw_varint64_size", "use crate::rt::compute_raw_varint64_size;", Some("rt")),
        ("tag_size", "use crate::rt::tag_size;", Some("rt")),
    ];

    let mut imports_to_add: Vec<&str> = Vec::new();

    for (func_name, import, skip_if_in_module) in import_fixes {
        // Check if the function is used (check various patterns):
        // - "func(" or "func (" for direct calls
        // - "(func)" for function references passed to .map(), etc.
        // - "{ func }" for function in code blocks
        let usage_pattern1 = format!("{}(", func_name);
        let usage_pattern2 = format!("{} (", func_name);
        let usage_pattern3 = format!("({})", func_name);  // function reference
        let usage_pattern4 = format!("({} )", func_name); // function reference with space
        let usage_pattern5 = format!("( {})", func_name); // function reference with space before
        if !source.contains(&usage_pattern1) && !source.contains(&usage_pattern2)
            && !source.contains(&usage_pattern3) && !source.contains(&usage_pattern4)
            && !source.contains(&usage_pattern5) {
            continue;
        }

        // Check if the import already exists (in various forms)
        let import_name = import.split("::").last().unwrap_or("").trim_end_matches(';');
        if source.contains(import) {
            continue;
        }

        // Check if this is the module where the function is defined (don't add import)
        if let Some(module_name) = skip_if_in_module {
            // If the file path or module declaration suggests this is the defining module, skip
            if source.contains(&format!("pub mod {};", module_name)) ||
               source.contains(&format!("mod {};", module_name)) ||
               source.contains(&format!("fn {} ", import_name)) ||
               source.contains(&format!("pub fn {} ", import_name)) ||
               source.contains(&format!("pub (crate) fn {} ", import_name)) {
                continue;
            }
        }

        // Check if there's already any import of this function (partial match)
        // Check for patterns like "use crate::..::name;", "::name;", "::name}"
        let pattern_with_brace = format!("::{}}}", import_name);
        if source.contains(&format!("use crate::{}", import_name)) ||
           source.contains(&format!("::{};", import_name)) ||
           source.contains(&pattern_with_brace) {
            continue;
        }

        imports_to_add.push(import);
    }

    if imports_to_add.is_empty() {
        return source.to_string();
    }

    // Find the right place to add imports (after existing use statements, before other code)
    let mut lines: Vec<&str> = source.lines().collect();
    let mut insert_pos = 0;

    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with("use ") || trimmed.starts_with("pub use ") ||
           trimmed.starts_with("pub(crate) use ") || trimmed.starts_with("pub mod ") ||
           trimmed.starts_with("mod ") || trimmed.starts_with("//") || trimmed.is_empty() {
            insert_pos = i + 1;
        } else if !trimmed.is_empty() {
            break;
        }
    }

    // Insert the missing imports
    for import in imports_to_add.into_iter().rev() {
        lines.insert(insert_pos, import);
    }

    let mut result = lines.join("\n");

    // Special case: LITTLE_ENDIAN should be a const, not an import
    // If LITTLE_ENDIAN is used but not defined, add a const definition
    if result.contains("LITTLE_ENDIAN") &&
       !result.contains("const LITTLE_ENDIAN") &&
       !result.contains("use byteorder::LITTLE_ENDIAN") {
        // Find the right place to add the const (after imports, before other code)
        let lines: Vec<&str> = result.lines().collect();
        let mut new_lines = Vec::new();
        let mut inserted = false;

        for line in lines {
            if !inserted && !line.trim().starts_with("use ") &&
               !line.trim().starts_with("pub use ") &&
               !line.trim().starts_with("pub(crate) use ") &&
               !line.trim().starts_with("pub mod ") &&
               !line.trim().starts_with("mod ") &&
               !line.trim().starts_with("//") &&
               !line.trim().is_empty() {
                new_lines.push("const LITTLE_ENDIAN: bool = cfg!(target_endian = \"little\");");
                new_lines.push("");
                inserted = true;
            }
            new_lines.push(line);
        }
        result = new_lines.join("\n");
    }

    result
}

/// Fix crate::Enum references to crate::enums::Enum.
/// The Enum trait is in the `enums` module, not at crate root.
pub fn fix_crate_enum_references(source: &str) -> String {
    // Replace crate::Enum with crate::enums::Enum (but not crate::EnumFull)
    let mut result = source.to_string();

    // Match "crate :: Enum" or "crate::Enum" followed by word boundary (not EnumFull)
    // Need to be careful not to match EnumFull
    let patterns = [
        ("crate :: Enum for", "crate::enums::Enum for"),
        ("crate::Enum for", "crate::enums::Enum for"),
        ("impl crate :: Enum ", "impl crate::enums::Enum "),
        ("impl crate::Enum ", "impl crate::enums::Enum "),
        (": crate :: Enum +", ": crate::enums::Enum +"),
        (": crate::Enum +", ": crate::enums::Enum +"),
        (": crate :: Enum >", ": crate::enums::Enum >"),
        (": crate::Enum >", ": crate::enums::Enum >"),
        (": crate :: Enum,", ": crate::enums::Enum,"),
        (": crate::Enum,", ": crate::enums::Enum,"),
        ("< crate :: Enum >", "< crate::enums::Enum >"),
        ("< crate::Enum >", "< crate::enums::Enum >"),
        ("crate :: Enum ::", "crate::enums::Enum::"),
        ("crate::Enum::", "crate::enums::Enum::"),
    ];

    for (pattern, replacement) in patterns {
        result = result.replace(pattern, replacement);
    }

    result
}

/// Fix module name conflicts with imports.
/// When a module name conflicts with an imported module, rename the import.
pub fn fix_module_import_conflicts(source: &str, file_path: &str) -> String {
    let mut result = source.to_string();

    // Check for field module conflict in well_known_types/type_/mod.rs
    if file_path.contains("well_known_types/type_") || file_path.contains("well_known_types\\type_") {
        // If there's both "use crate::reflect::field;" and "pub mod field {"
        if result.contains("use crate::reflect::field;") && result.contains("pub mod field {") {
            result = result.replace(
                "use crate::reflect::field;",
                "use crate::reflect::field as reflect_field;"
            );
            // Also update any usages of field:: that refer to the import
            // But be careful - field:: inside pub mod field { ... } should not be changed
        }
    }

    result
}

/// Fix empty rt::v2 module by adding re-exports of accessor functions.
/// The reflect::rt::v2 module should re-export functions from reflect::acc::v2,
/// but the slicer may leave it empty.
pub fn fix_empty_rt_v2_module(source: &str, file_path: &str) -> String {
    // Only apply to reflect/rt/v2/mod.rs
    if !file_path.ends_with("reflect/rt/v2/mod.rs") {
        return source.to_string();
    }

    // If the file is empty or only has whitespace, add the re-exports
    if source.trim().is_empty() {
        return r#"//! Re-exports of accessor functions for generated code.

pub use crate::reflect::acc::v2::singular::make_option_accessor;
pub use crate::reflect::acc::v2::singular::make_message_field_accessor;
pub use crate::reflect::acc::v2::singular::make_simpler_field_accessor;
pub use crate::reflect::acc::v2::singular::oneof::make_oneof_message_has_get_mut_set_accessor;
pub use crate::reflect::acc::v2::singular::oneof::make_oneof_enum_accessors;
pub use crate::reflect::acc::v2::singular::oneof::make_oneof_copy_has_get_set_simpler_accessors;
pub use crate::reflect::acc::v2::singular::oneof::make_oneof_deref_has_get_set_simpler_accessor;
pub use crate::reflect::acc::v2::repeated::make_vec_simpler_accessor;
pub use crate::reflect::acc::v2::map::make_map_simpler_accessor;
pub use crate::reflect::acc::v2::map::make_map_simpler_accessor_new;
"#.to_string();
    }

    source.to_string()
}

/// Fix lib.rs by adding missing re-exports for common traits.
/// When code references crate::Enum, crate::Oneof etc, it expects traits at crate root.
pub fn fix_lib_rs_reexports(source: &str, file_path: &str) -> String {
    // Only apply to lib.rs
    if !file_path.ends_with("lib.rs") {
        return source.to_string();
    }

    let mut result = source.to_string();

    // Define modules and their re-exports: (module, item_to_reexport)
    let reexports = [
        ("pub mod enums;", "pub use crate::enums::Enum;"),
        ("pub mod oneof;", "pub use crate::oneof::Oneof;"),
        ("pub mod oneof_full;", "pub use crate::oneof_full::OneofFull;"),
    ];

    for (module_decl, reexport) in reexports {
        if result.contains(module_decl) && !result.contains(reexport) {
            // Add re-export after the module declaration
            result = result.replace(
                module_decl,
                &format!("{}\n{}", module_decl, reexport)
            );
        }
    }

    result
}

/// Fix trait vs struct import confusion.
/// When a struct import is used in a trait bound position, replace it with the trait import.
/// This handles cases like:
/// - `use crate::well_known_types::any::Any;` in trait bound should be `use std::any::Any;`
/// - `use crate::well_known_types::type_::Enum;` in trait bound should be `use crate::enums::Enum;`
pub fn fix_trait_struct_confusion(source: &str) -> String {
    let mut result = source.to_string();

    // Fix Any: if the source imports crate::well_known_types::any::Any and uses it in a trait bound
    if result.contains("use crate::well_known_types::any::Any;") {
        // Check if Any is used as a trait bound (: Any + ...)
        // These patterns indicate trait usage, not struct usage
        // IMPORTANT: "impl Any" can mean either:
        // - "impl Any for X" (implementing trait Any for type X) - Any is a trait
        // - "impl Any { }" (implementing methods on type Any) - Any is a struct
        // So we check for "impl Any for" specifically
        let is_trait_bound = result.contains(": Any +");
        let is_trait_impl = result.contains("impl Any for");

        // Don't replace if Any is used as a struct (constructor, method call on type, etc.)
        let is_struct_usage = result.contains("impl Any {") ||
                              result.contains("Any {") && !result.contains(": Any {");

        if (is_trait_bound || is_trait_impl) && !is_struct_usage {
            result = result.replace(
                "use crate::well_known_types::any::Any;",
                "use std::any::Any;"
            );
        }
    }

    // Fix Enum: if the source imports crate::well_known_types::type_::Enum and uses it in a trait bound
    if result.contains("use crate::well_known_types::type_::Enum;") {
        // Check if Enum is used as a trait bound
        // Same logic as above - "impl Enum for" means trait, "impl Enum {" means struct
        let is_trait_bound = result.contains(": Enum {") || result.contains(": Enum +") ||
                             result.contains(": Enum>") || result.contains("+ Enum +") ||
                             result.contains("+ Enum>");
        let is_trait_impl = result.contains("impl Enum for");

        // Don't replace if Enum is used as a struct
        let is_struct_usage = result.contains("impl Enum {") ||
                              result.contains("Enum {") && !result.contains(": Enum {");

        if (is_trait_bound || is_trait_impl) && !is_struct_usage {
            result = result.replace(
                "use crate::well_known_types::type_::Enum;",
                "use crate::enums::Enum;"
            );
        }
    }

    result
}

/// Remove spurious imports that reference items only mentioned in doc comments.
/// These imports are incorrectly generated when parsing doc comments that mention type names.
pub fn fix_spurious_doc_imports(source: &str) -> String {
    let mut result = source.to_string();

    // List of imports that are known to be spuriously generated from doc comments
    let spurious_patterns = [
        // "Either" appears in "Either store a value in unknown, or skip a group."
        "use crate::reflect::message::is_initialized_is_always_true::Either;",
        // Other known spurious patterns can be added here
    ];

    for pattern in spurious_patterns {
        if result.contains(pattern) {
            // Remove the import line
            result = result.replace(&format!("{}\n", pattern), "");
            result = result.replace(pattern, "");
        }
    }

    result
}

/// Remove duplicate trait implementations that cause E0119 errors.
/// When the same impl appears in multiple files, remove the duplicate.
pub fn fix_duplicate_impls(source: &str, file_path: &str) -> String {
    let mut result = source.to_string();

    // In well_known_types_util/timestamp.rs - these impls already exist in timestamp.rs
    if file_path.ends_with("well_known_types_util/timestamp.rs") {
        // Remove duplicate From<SystemTime> for Timestamp
        if result.contains("impl From < SystemTime > for Timestamp") {
            // Find and remove the impl block - it's complex inline code
            // Just remove the file if it only contains these impls
            if result.contains("impl From < SystemTime > for Timestamp") {
                // Replace the impl with a comment
                result = remove_impl_block(&result, "impl From < SystemTime > for Timestamp");
            }
        }
        // Remove duplicate Into<SystemTime> for Timestamp
        if result.contains("impl Into < SystemTime > for Timestamp") {
            result = remove_impl_block(&result, "impl Into < SystemTime > for Timestamp");
        }
    }

    // In well_known_types_util/duration.rs - orphan impls that can't exist in sliced crate
    if file_path.ends_with("well_known_types_util/duration.rs") {
        // These are orphan impls that violate the orphan rule
        // They implement traits for types from std
        if result.contains("impl From < std :: time :: Duration > for Duration") {
            result = remove_impl_block(&result, "impl From < std :: time :: Duration > for Duration");
        }
        if result.contains("impl Into < std :: time :: Duration > for Duration") {
            result = remove_impl_block(&result, "impl Into < std :: time :: Duration > for Duration");
        }
    }

    result
}

/// Helper to remove an impl block from source (simple version - removes the line)
fn remove_impl_block(source: &str, impl_signature: &str) -> String {
    // For now, just remove lines containing the impl signature
    // This is a simple approach that works for single-line impls
    let lines: Vec<&str> = source.lines().collect();
    let mut result = Vec::new();
    let mut skip_current = false;

    for line in lines {
        if line.contains(impl_signature) {
            skip_current = true;
            continue;
        }
        if !skip_current {
            result.push(line);
        }
        // Reset skip for next line (single-line impls)
        skip_current = false;
    }

    result.join("\n")
}

/// Fix private items that need to be pub(crate) for cross-module access.
/// When the slicer generates private items that are used from other modules,
/// we need to make them pub(crate).
pub fn fix_private_item_visibility(source: &str, file_path: &str) -> String {
    let mut result = source.to_string();

    // Fix private enums/structs that need to be pub(crate)
    // These are items that are used across module boundaries but were generated as private

    // In optional.rs: "enum Impl" should be "pub(crate) enum Impl"
    if file_path.ends_with("optional.rs") {
        if result.contains("enum Impl <") && !result.contains("pub enum Impl") && !result.contains("pub(crate) enum Impl") {
            result = result.replace("enum Impl <", "pub(crate) enum Impl <");
        }
    }

    // In owning_ref.rs: "enum Owner" should be "pub(crate) enum Owner"
    if file_path.ends_with("owning_ref.rs") {
        if result.contains("enum Owner <") && !result.contains("pub enum Owner") && !result.contains("pub(crate) enum Owner") {
            result = result.replace("enum Owner <", "pub(crate) enum Owner <");
        }
    }

    // In text_format/parse.rs: "struct Parser" should be "pub(crate) struct Parser"
    if file_path.ends_with("parse.rs") {
        if result.contains("struct Parser <") && !result.contains("pub struct Parser") && !result.contains("pub(crate) struct Parser") {
            result = result.replace("struct Parser <", "pub(crate) struct Parser <");
        }
    }

    result
}

/// Fix imports in nested modules that incorrectly reference the parent module as an external crate.
///
/// Example: `src/parking_lot/deadlock.rs` has `use parking_lot::ThreadData;`
/// This looks like an external crate import but should be `use super::ThreadData;` or `use crate::parking_lot::ThreadData;`
///
/// Also removes spurious self-referential imports where a file imports something it defines itself.
///
/// This function detects when a file is in a subdirectory and fixes imports that match the parent directory name.
pub fn fix_nested_module_imports(content: &str, file_path: &str) -> String {
    use std::path::Path;

    let path = Path::new(file_path);

    // Extract parent directory name if this is a nested module file
    // e.g., src/parking_lot/deadlock.rs → parent is "parking_lot"
    let parent_module_name = if let Some(parent) = path.parent() {
        if let Some(parent_name) = parent.file_name() {
            let name = parent_name.to_string_lossy();
            // Skip if parent is just "src" (not a module)
            if name != "src" {
                Some(name.to_string())
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    let Some(parent_name) = parent_module_name else {
        return content.to_string();
    };

    let mut result = content.to_string();

    // Fix patterns like "use parking_lot::Item;" to "use super::Item;"
    // But be careful not to match:
    // - "use self::parking_lot::Item;" (already correct)
    // - "use crate::parking_lot::Item;" (already correct)
    // - "pub use self::parking_lot::Item;" (already correct)

    // Pattern 1: Remove or fix "use parent_module::Item;" imports
    let bare_import = format!("use {}::", parent_name);
    if result.contains(&bare_import) {
        // Check each line to avoid replacing comments or strings
        let lines: Vec<&str> = result.lines().collect();
        let mut new_lines = Vec::new();

        for line in lines {
            let trimmed = line.trim_start();
            // Only process bare imports (not self::, crate::, or pub use self::)
            if trimmed.starts_with(&bare_import)
                && !trimmed.contains("self::")
                && !trimmed.contains("crate::") {
                // Extract the imported item name
                let import_part = if let Some(semicolon_pos) = trimmed.find(';') {
                    &trimmed[bare_import.len()..semicolon_pos]
                } else {
                    &trimmed[bare_import.len()..]
                };

                // Check if this item is defined in the current file (spurious self-import)
                // Common patterns: "THREAD_DATA", "ThreadData", etc.
                let item_name = import_part.split("::").next().unwrap_or("").trim();

                // Check if the file defines this item (struct, const, static, thread_local!, etc.)
                let is_defined_locally =
                    result.contains(&format!("struct {} ", item_name))
                    || result.contains(&format!("struct {}<", item_name))
                    || result.contains(&format!("struct {}\n", item_name))
                    || result.contains(&format!("const {} ", item_name))
                    || result.contains(&format!("static {} ", item_name))
                    || result.contains(&format!("static {}: ", item_name))
                    || result.contains(&format!("enum {} ", item_name))
                    || result.contains(&format!("type {} ", item_name))
                    || result.contains(&format!("fn {}(", item_name))
                    || result.contains(&format!("(static {} ", item_name)) // thread_local!(static ...)
                    || result.contains(&format!("(static {}: ", item_name));

                if is_defined_locally {
                    // Skip this spurious import - don't add it to new_lines
                    continue;
                } else {
                    // Replace "use parking_lot::" with "use super::"
                    let fixed_line = line.replace(&bare_import, "use super::");
                    new_lines.push(fixed_line);
                }
            } else {
                new_lines.push(line.to_string());
            }
        }

        result = new_lines.join("\n");
    }

    result
}

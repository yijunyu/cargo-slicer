// ctags_parser.rs - Convert ctags output to ParsedItem structures
//
// This module bridges the gap between Universal Ctags (fast text-based parsing)
// and the slicer's internal representation (ParsedItem).

use crate::ctags_collector::RustTag;
use crate::types::{ParsedItem, ParsedItemKind, ItemVisibility};
use crate::slicer::text_extractor::TextExtractor;
use std::path::Path;
use std::collections::HashSet;

/// Convert ctags RustTag to ParsedItem
///
/// This function:
/// 1. Maps ctags kind characters to ParsedItemKind
/// 2. Extracts source text using TextExtractor
/// 3. Computes full path from scope information
/// 4. Infers visibility from tag attributes (TODO: improve this)
/// 5. Extracts generic parameters (TODO: implement)
pub fn convert_tag_to_item(
    tag: &RustTag,
    extractor: &TextExtractor,
    crate_name: &str,
    file_path: &Path,
) -> Option<ParsedItem> {
    // Map ctags kind to ParsedItemKind
    let kind = map_ctags_kind(&tag.kind_name, tag.kind_char)?;

    // Extract source text for this item
    let source = extractor.extract_item(tag.line_number, &tag.kind_name)?;

    // Compute full path from scope information
    let (full_path, module_path) = compute_paths(tag, crate_name);

    // Infer visibility (ctags doesn't provide this, so we parse from source)
    let (is_pub, visibility) = infer_visibility(&source);

    Some(ParsedItem {
        name: tag.name.clone(),
        path: full_path.clone(),
        kind,
        source: source.clone(),
        dependencies: HashSet::new(), // Will be filled by text_dependencies.rs
        typed_dependencies: Vec::new(), // Will be filled by text_dependencies.rs
        external_crates: HashSet::new(), // Will be filled by text_dependencies.rs
        module_path,
        is_pub,
        visibility,
        file: file_path.to_path_buf(),
        line: Some(tag.line_number), // Line number from ctags (1-indexed)
        cfg_attr: None, // TODO: Extract from source text
        generics: Vec::new(), // TODO: Extract from source text
    })
}

/// Map ctags kind character/name to ParsedItemKind
///
/// Ctags uses single-character kinds plus kind names:
/// - f/function: Free function
/// - P/method: Method (function in impl block) - currently filtered out
/// - s/struct: Struct definition
/// - e/enum: Enum definition
/// - t/trait: Trait definition
/// - c/impl: Implementation block
/// - T/type: Type alias
/// - v/constant: Constant or static
/// - M/macro: Macro definition
/// - m/field: Struct field - currently filtered out
/// - g/module: Module (also enum in some ctags versions - needs disambiguation)
fn map_ctags_kind(kind_name: &str, kind_char: char) -> Option<ParsedItemKind> {
    match (kind_name, kind_char) {
        ("function", 'f') => Some(ParsedItemKind::Function),
        ("method", 'P') => None, // Skip methods - not top-level items
        ("struct", 's') => Some(ParsedItemKind::Struct),
        ("enum", 'e') => Some(ParsedItemKind::Enum),
        ("trait", 't') => Some(ParsedItemKind::Trait),
        ("impl", 'c') => Some(ParsedItemKind::Impl),
        ("type", 'T') => Some(ParsedItemKind::TypeAlias),
        ("constant", 'v') => Some(ParsedItemKind::Const),
        ("macro", 'M') => Some(ParsedItemKind::Macro),
        ("field", 'm') => None, // Skip struct fields - not top-level items
        ("module", 'g') => Some(ParsedItemKind::Mod),
        _ => None, // Unknown kind - skip
    }
}

/// Compute full path and module path from ctags tag
///
/// Ctags provides scope information in formats like:
/// - "implementation:TypeName" for methods
/// - "struct:TypeName" for associated items
/// - "module:mod_name::sub_mod" for nested items
///
/// We need to reconstruct the full path like "crate_name::module::TypeName::item_name"
fn compute_paths(tag: &RustTag, crate_name: &str) -> (String, String) {
    let mut path_parts = vec![crate_name];

    // Parse scope information if present
    if let Some(ref scope) = tag.scope {
        // Example scope: "implementation:regex::Regex"
        // or "module:regex::internal"
        let parts: Vec<&str> = scope.split(':').collect();
        if parts.len() >= 2 {
            let scope_type = parts[0];
            let scope_path = parts[1];

            match scope_type {
                "implementation" | "struct" | "enum" | "trait" => {
                    // For impl blocks, the scope is the type being implemented
                    path_parts.extend(scope_path.split("::"));
                }
                "module" => {
                    // For module-scoped items, add the module path
                    path_parts.extend(scope_path.split("::"));
                }
                _ => {
                    // Unknown scope type - just add the path
                    path_parts.extend(scope_path.split("::"));
                }
            }
        }
    }

    // Add the item name itself
    path_parts.push(&tag.name);

    // Full path: "crate_name::module::Item::item_name"
    let full_path = path_parts.join("::");

    // Module path: "crate::module" (without the item name)
    let module_path = if path_parts.len() > 1 {
        let mut mod_parts = path_parts;
        mod_parts.pop(); // Remove item name
        mod_parts.join("::")
    } else {
        format!("crate")
    };

    (full_path, module_path)
}

/// Infer visibility from source text
///
/// Ctags doesn't provide visibility information, so we parse it from the source.
/// This is a simple heuristic - check if source starts with "pub".
///
/// More sophisticated parsing would handle:
/// - pub(crate)
/// - pub(super)
/// - pub(in path::to::module)
fn infer_visibility(source: &str) -> (bool, ItemVisibility) {
    let trimmed = source.trim_start();

    // Skip attributes and doc comments
    let code_start = trimmed
        .lines()
        .skip_while(|line| {
            let l = line.trim();
            l.starts_with("///") || l.starts_with("#[") || l.starts_with("//!")
        })
        .next()
        .unwrap_or("");

    // Check for pub keywords
    if code_start.starts_with("pub(crate)") {
        (true, ItemVisibility::Crate)
    } else if code_start.starts_with("pub(super)") {
        (true, ItemVisibility::Super)
    } else if code_start.starts_with("pub(in ") {
        // pub(in path::to::module) - restricted visibility
        // For now, treat as crate visibility (conservative)
        (true, ItemVisibility::Crate)
    } else if code_start.starts_with("pub ") || code_start.starts_with("pub(") {
        (true, ItemVisibility::Public)
    } else {
        (false, ItemVisibility::Private)
    }
}

/// Parse all items from a file using ctags
///
/// This is the main entry point for ctags-based parsing.
/// It calls ctags, extracts source text, and converts to ParsedItem.
pub fn parse_file_with_ctags(
    file_path: &Path,
    crate_name: &str,
) -> Result<Vec<ParsedItem>, String> {
    use crate::ctags_collector::parse_rust_file;
    use std::fs;

    // Read file content for text extraction
    let source = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;

    // Create text extractor
    let extractor = TextExtractor::new(source);

    // Parse with ctags
    let tags = parse_rust_file(file_path.to_str().ok_or("Invalid UTF-8 path")?)?;

    // Convert tags to ParsedItems
    let items: Vec<ParsedItem> = tags
        .into_iter()
        .filter_map(|tag| convert_tag_to_item(&tag, &extractor, crate_name, file_path))
        .collect();

    Ok(items)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_ctags_kind() {
        assert_eq!(
            map_ctags_kind("function", 'f'),
            Some(ParsedItemKind::Function)
        );
        assert_eq!(
            map_ctags_kind("struct", 's'),
            Some(ParsedItemKind::Struct)
        );
        assert_eq!(
            map_ctags_kind("enum", 'e'),
            Some(ParsedItemKind::Enum)
        );
        assert_eq!(map_ctags_kind("method", 'P'), None); // Methods filtered out
        assert_eq!(map_ctags_kind("field", 'm'), None); // Fields filtered out
    }

    #[test]
    fn test_compute_paths_simple() {
        let tag = RustTag {
            name: "hello".to_string(),
            kind_name: "function".to_string(),
            kind_char: 'f',
            line_number: 1,
            scope: None,
        };

        let (full_path, module_path) = compute_paths(&tag, "mycrate");
        assert_eq!(full_path, "mycrate::hello");
        assert_eq!(module_path, "crate");
    }

    #[test]
    fn test_compute_paths_with_scope() {
        let tag = RustTag {
            name: "new".to_string(),
            kind_name: "function".to_string(),
            kind_char: 'f',
            line_number: 10,
            scope: Some("implementation:mycrate::Regex".to_string()),
        };

        let (full_path, module_path) = compute_paths(&tag, "mycrate");
        assert_eq!(full_path, "mycrate::mycrate::Regex::new");
        assert_eq!(module_path, "mycrate::mycrate::Regex");
    }

    #[test]
    fn test_infer_visibility_public() {
        assert_eq!(
            infer_visibility("pub fn hello() {}"),
            (true, ItemVisibility::Public)
        );
    }

    #[test]
    fn test_infer_visibility_crate() {
        assert_eq!(
            infer_visibility("pub(crate) fn hello() {}"),
            (true, ItemVisibility::Crate)
        );
    }

    #[test]
    fn test_infer_visibility_private() {
        assert_eq!(
            infer_visibility("fn hello() {}"),
            (false, ItemVisibility::Private)
        );
    }

    #[test]
    fn test_infer_visibility_with_attributes() {
        let source = "#[inline]\n#[cfg(test)]\npub fn hello() {}";
        assert_eq!(infer_visibility(source), (true, ItemVisibility::Public));
    }
}

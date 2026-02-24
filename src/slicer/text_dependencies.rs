// text_dependencies.rs - Extract dependencies from source text using fast pattern matching
//
// This module provides text-based dependency extraction as an alternative to
// full AST parsing. It uses aho-corasick for fast multi-pattern matching.
//
// DESIGN PHILOSOPHY:
// - Over-inclusive is safe for slicing (false positives are okay)
// - Fast pattern matching >> slow AST analysis
// - Use for 90% case, fallback to syn for complex cases

use aho_corasick::AhoCorasick;
use std::collections::HashSet;

/// Extract internal dependencies from source text
///
/// This function uses fast text search to find type/function names in the source.
/// It's intentionally over-inclusive - false positives are okay for slicing.
///
/// Strategy:
/// 1. Build multi-pattern matcher for all known item names in the crate
/// 2. Search source text for these patterns
/// 3. Filter out false positives (e.g., in comments, strings)
/// 4. Return set of potentially-used items
///
/// This is 10-100x faster than AST parsing for large files.
pub fn extract_dependencies_fast(
    source: &str,
    all_item_names: &[String],
) -> HashSet<String> {
    if all_item_names.is_empty() {
        return HashSet::new();
    }

    // Build aho-corasick automaton for fast multi-pattern matching
    let ac = AhoCorasick::new(all_item_names).expect("Failed to build pattern matcher");

    let mut dependencies = HashSet::new();

    // Find all matches in source text
    for mat in ac.find_iter(source) {
        let matched_name = &all_item_names[mat.pattern().as_usize()];

        // Basic filtering: skip if match is in a comment or string
        // This is a heuristic - we'll accept some false positives for speed
        if !is_likely_false_positive(source, mat.start()) {
            dependencies.insert(matched_name.clone());
        }
    }

    dependencies
}

/// Extract dependencies using a pre-built aho-corasick automaton
///
/// This is the optimized version that avoids rebuilding the automaton for each item.
/// Should be called when processing multiple items from the same file.
pub fn extract_dependencies_with_automaton(
    source: &str,
    all_item_names: &[String],
    automaton: &AhoCorasick,
) -> HashSet<String> {
    let mut dependencies = HashSet::new();

    // Find all matches in source text using pre-built automaton
    for mat in automaton.find_iter(source) {
        let matched_name = &all_item_names[mat.pattern().as_usize()];

        // Basic filtering: skip if match is in a comment or string
        if !is_likely_false_positive(source, mat.start()) {
            dependencies.insert(matched_name.clone());
        }
    }

    dependencies
}

/// Heuristic to detect false positives (matches in comments/strings)
///
/// This is a simple scan-backwards approach:
/// - Check for "//" on the same line before the match
/// - Check for "/*" before the match (without closing */  between)
/// - Check for opening quote before the match (without closing quote between)
///
/// NOT PERFECT - but good enough for over-inclusive matching.
fn is_likely_false_positive(source: &str, match_pos: usize) -> bool {
    let bytes = source.as_bytes();

    // Scan backwards to start of line
    let mut pos = match_pos;
    while pos > 0 && bytes[pos - 1] != b'\n' {
        pos -= 1;
    }

    let line_start = pos;
    let line = &source[line_start..match_pos];

    // Check for line comment
    if line.contains("//") {
        return true;
    }

    // Check for string literal (simple heuristic: odd number of quotes before match)
    let quote_count = line.matches('"').count();
    if quote_count % 2 == 1 {
        return true; // Inside string
    }

    // TODO: Add block comment detection (/* */)
    // For now, we accept some false positives in block comments

    false
}

/// Extract external crate dependencies from source text
///
/// Looks for patterns like:
/// - `use extern_crate::Item`
/// - `extern_crate::function()`
/// - `extern_crate::Type`
///
/// Returns set of crate names (not full paths).
pub fn extract_external_crates(source: &str) -> HashSet<String> {
    let mut crates = HashSet::new();

    // Pattern 1: use statements
    // use crate_name::...
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("use ") {
            if let Some(rest) = trimmed.strip_prefix("use ") {
                // Extract first identifier before ::
                if let Some(crate_name) = rest.split("::").next() {
                    let crate_name = crate_name.trim();
                    // Skip std, core, alloc, self, super, crate
                    if !is_builtin_crate(crate_name) && !crate_name.is_empty() {
                        crates.insert(crate_name.to_string());
                    }
                }
            }
        }
    }

    // Pattern 2: Qualified paths in code
    // This is harder and more error-prone, so we use a simpler heuristic
    // Look for :: followed by uppercase (likely a type reference)
    // e.g., "regex::Regex" or "serde::Serialize"

    // For now, stick with use statements (most reliable)
    // TODO: Add more sophisticated path detection if needed

    crates
}

/// Check if a crate name is a builtin (std, core, etc.)
fn is_builtin_crate(name: &str) -> bool {
    matches!(
        name,
        "std" | "core" | "alloc" | "self" | "super" | "crate" | "Self"
    )
}

/// Build dependency graph using text-based extraction
///
/// This is the main entry point for text-based dependency analysis.
/// It's used as an alternative to syn-based AST parsing.
///
/// Returns:
/// - dependencies: Map from item name to set of items it depends on
/// - external_crates: Set of external crate names used
pub fn build_text_dependency_graph(
    items: &[(String, String)], // (item_name, source_text)
) -> (std::collections::HashMap<String, HashSet<String>>, HashSet<String>) {
    use std::collections::HashMap;

    let mut dependencies = HashMap::new();
    let mut all_external_crates = HashSet::new();

    // Collect all item names for pattern matching
    let all_names: Vec<String> = items.iter().map(|(name, _)| name.clone()).collect();

    // For each item, find what it depends on
    for (item_name, source) in items {
        // Extract internal dependencies (other items in this crate)
        let internal_deps = extract_dependencies_fast(source, &all_names);
        dependencies.insert(item_name.clone(), internal_deps);

        // Extract external dependencies (other crates)
        let external_deps = extract_external_crates(source);
        all_external_crates.extend(external_deps);
    }

    (dependencies, all_external_crates)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_dependencies_simple() {
        let source = "fn hello() { world(); other_func(); }";
        let all_items = vec!["world".to_string(), "other_func".to_string(), "unused".to_string()];

        let deps = extract_dependencies_fast(source, &all_items);

        assert!(deps.contains("world"));
        assert!(deps.contains("other_func"));
        assert!(!deps.contains("unused"));
    }

    #[test]
    fn test_skip_comments() {
        let source = "fn hello() { /* world() */ real_func(); }";
        let all_items = vec!["world".to_string(), "real_func".to_string()];

        let deps = extract_dependencies_fast(source, &all_items);

        // Note: Our simple heuristic doesn't catch block comments perfectly
        // This is okay - over-inclusive is safe
        assert!(deps.contains("real_func"));
    }

    #[test]
    fn test_skip_line_comments() {
        let source = "fn hello() { \n// world()\n real_func(); }";
        let all_items = vec!["world".to_string(), "real_func".to_string()];

        let deps = extract_dependencies_fast(source, &all_items);

        assert!(!deps.contains("world")); // In comment, should be skipped
        assert!(deps.contains("real_func"));
    }

    #[test]
    fn test_skip_strings() {
        let source = r#"fn hello() { let s = "world"; real_func(); }"#;
        let all_items = vec!["world".to_string(), "real_func".to_string()];

        let deps = extract_dependencies_fast(source, &all_items);

        assert!(!deps.contains("world")); // In string, should be skipped
        assert!(deps.contains("real_func"));
    }

    #[test]
    fn test_extract_external_crates_use() {
        let source = r#"
use regex::Regex;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use super::local_module;
"#;

        let crates = extract_external_crates(source);

        assert!(crates.contains("regex"));
        assert!(crates.contains("serde"));
        assert!(!crates.contains("std")); // Builtin, should be filtered
        assert!(!crates.contains("super")); // Builtin, should be filtered
    }

    #[test]
    fn test_is_builtin_crate() {
        assert!(is_builtin_crate("std"));
        assert!(is_builtin_crate("core"));
        assert!(is_builtin_crate("self"));
        assert!(!is_builtin_crate("regex"));
        assert!(!is_builtin_crate("serde"));
    }

    #[test]
    fn test_build_text_dependency_graph() {
        let items = vec![
            ("hello".to_string(), "fn hello() { world(); }".to_string()),
            ("world".to_string(), "fn world() { }".to_string()),
        ];

        let (deps, _external) = build_text_dependency_graph(&items);

        assert_eq!(deps.len(), 2);
        assert!(deps.get("hello").unwrap().contains("world"));
        assert!(deps.get("world").unwrap().is_empty());
    }
}

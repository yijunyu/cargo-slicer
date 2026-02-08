//! Phase 1: Usage analysis - finds which items from a crate are used.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use rayon::prelude::*;

use super::types::{UsedItem, ItemKind, TypeTracker};

/// Find all Rust source files in a directory
pub fn find_rust_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                files.extend(find_rust_files(&path));
            } else if path.extension().map(|e| e == "rs").unwrap_or(false) {
                files.push(path);
            }
        }
    }

    files
}

/// Main analysis function
pub fn analyze_crate_usage(project_src: &Path, crate_name: &str) -> HashSet<UsedItem> {
    let mut all_used = HashSet::new();

    for file in find_rust_files(project_src) {
        let used = find_used_items(&file, crate_name);
        all_used.extend(used);
    }

    all_used
}

/// Analyzes a Rust source file to find used items from a specific crate
pub fn find_used_items(source_path: &Path, crate_name: &str) -> HashSet<UsedItem> {
    let mut used = HashSet::new();
    let mut tracker = TypeTracker::default();

    let content = match fs::read_to_string(source_path) {
        Ok(c) => c,
        Err(_) => return used,
    };

    let rust_crate_name = crate_name.replace('-', "_");
    let crate_prefix = format!("{}::", rust_crate_name);

    // First pass: collect use statements and type aliases
    for line in content.lines() {
        let line = line.trim();
        // Match both "use crate::" and "use crate::" patterns (with :: or : for brace imports)
        if line.starts_with("use ") && (line.contains(&crate_prefix) || line.contains(&format!("{}:", rust_crate_name))) {
            if let Some(items) = parse_use_statement(line, &rust_crate_name) {
                for item in &items {
                    if let Some(short_name) = item.path.split("::").last() {
                        if item.kind == ItemKind::Struct || item.kind == ItemKind::Type {
                            tracker.type_aliases.insert(
                                short_name.to_string(),
                                item.path.clone(),
                            );
                        }
                        if item.kind == ItemKind::Module {
                            tracker.module_aliases.insert(
                                short_name.to_string(),
                                item.path.clone(),
                            );
                        }
                    }
                    used.insert(item.clone());
                }
            }
        }
    }

    // Second pass: find direct path usage and variable declarations
    for line in content.lines() {
        let line = line.trim();

        if line.starts_with("//") || line.starts_with("/*") || line.starts_with("*") {
            continue;
        }

        // Pattern 1: Direct path usage
        let mut search_pos = 0;
        while let Some(pos) = line[search_pos..].find(&crate_prefix) {
            let abs_pos = search_pos + pos;
            let rest = &line[abs_pos + crate_prefix.len()..];

            let mut path = String::new();
            let mut chars = rest.chars().peekable();

            while let Some(&c) = chars.peek() {
                if c.is_alphanumeric() || c == '_' || c == ':' {
                    path.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            if !path.is_empty() {
                let kind = infer_item_kind(&path, line);
                used.insert(UsedItem {
                    path: format!("{}{}", crate_prefix, path),
                    kind,
                });
            }

            search_pos = abs_pos + crate_prefix.len();
        }

        // Pattern 2: Track variable declarations with type annotations
        if let Some((var_name, type_name)) = extract_variable_type(line, &rust_crate_name, &tracker) {
            tracker.var_types.insert(var_name, type_name);
        }

        // Pattern 3: Track static/const declarations with Lazy<Type>
        if let Some((var_name, type_name)) = extract_static_type(line, &rust_crate_name, &tracker) {
            tracker.var_types.insert(var_name, type_name);
        }

        // Pattern 4: Detect module alias usage
        for (alias, full_path) in &tracker.module_aliases {
            let alias_prefix = format!("{}::", alias);
            let mut search_pos = 0;
            while let Some(pos) = line[search_pos..].find(&alias_prefix) {
                let abs_pos = search_pos + pos;
                let is_word_boundary = abs_pos == 0 || {
                    let prev_char = line[..abs_pos].chars().last().unwrap_or(' ');
                    !prev_char.is_alphanumeric() && prev_char != '_'
                };
                if is_word_boundary {
                    let rest = &line[abs_pos + alias_prefix.len()..];
                    let item_name: String = rest.chars()
                        .take_while(|c| c.is_alphanumeric() || *c == '_')
                        .collect();
                    if !item_name.is_empty() {
                        let kind = infer_item_kind(&item_name, line);
                        used.insert(UsedItem {
                            path: format!("{}::{}", full_path, item_name),
                            kind,
                        });
                    }
                }
                search_pos = abs_pos + alias_prefix.len();
            }
        }
    }

    // Third pass: find method calls on tracked variables
    for line in content.lines() {
        let line = line.trim();

        if line.starts_with("//") || line.starts_with("/*") || line.starts_with("*") {
            continue;
        }

        for (var_name, type_path) in &tracker.var_types {
            if type_path.starts_with(&format!("{}::", crate_name)) {
                let methods = find_method_calls(line, var_name);
                for method in methods {
                    used.insert(UsedItem {
                        path: format!("{}::{}", type_path, method),
                        kind: ItemKind::Method,
                    });
                }
            }
        }
    }

    used
}

/// Extract variable name and type from a let binding
fn extract_variable_type(line: &str, crate_name: &str, tracker: &TypeTracker) -> Option<(String, String)> {
    let crate_prefix = format!("{}::", crate_name);

    if line.starts_with("let ") || line.starts_with("let mut ") {
        let after_let = if line.starts_with("let mut ") {
            &line[8..]
        } else {
            &line[4..]
        };

        let var_end = after_let.find(|c| c == ':' || c == '=').unwrap_or(after_let.len());
        let var_name = after_let[..var_end].trim().to_string();

        if !var_name.is_empty() {
            if let Some(colon_pos) = after_let.find(':') {
                let after_colon = &after_let[colon_pos + 1..];
                if let Some(eq_pos) = after_colon.find('=') {
                    let type_str = after_colon[..eq_pos].trim();
                    if type_str.starts_with(&crate_prefix) {
                        return Some((var_name, type_str.to_string()));
                    }
                    if let Some(full_path) = tracker.type_aliases.get(type_str) {
                        return Some((var_name, full_path.clone()));
                    }
                }
            }

            if let Some(pos) = line.find(&crate_prefix) {
                let rest = &line[pos + crate_prefix.len()..];
                let type_end = rest.find("::").unwrap_or_else(|| {
                    rest.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(rest.len())
                });
                let type_name = &rest[..type_end];
                if !type_name.is_empty() && type_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    return Some((var_name, format!("{}{}", crate_prefix, type_name)));
                }
            }

            for (short_name, full_path) in &tracker.type_aliases {
                let pattern = format!("{}::", short_name);
                if line.contains(&pattern) {
                    return Some((var_name, full_path.clone()));
                }
            }
        }
    }

    if line.contains("if let ") || line.contains("while let ") {
        for pattern in &["Ok(", "Some(", "Err("] {
            if let Some(start) = line.find(pattern) {
                let after = &line[start + pattern.len()..];
                if let Some(end) = after.find(')') {
                    let var_name = after[..end].trim().to_string();
                    if !var_name.is_empty() && var_name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        if let Some(pos) = line.find(&crate_prefix) {
                            let rest = &line[pos + crate_prefix.len()..];
                            let type_end = rest.find("::").unwrap_or_else(|| {
                                rest.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(rest.len())
                            });
                            let type_name = &rest[..type_end];
                            if !type_name.is_empty() && type_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                                return Some((var_name, format!("{}{}", crate_prefix, type_name)));
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

/// Extract type from static/const declarations with Lazy<Type>
fn extract_static_type(line: &str, crate_name: &str, tracker: &TypeTracker) -> Option<(String, String)> {
    if !line.starts_with("static ") && !line.starts_with("const ") {
        return None;
    }

    let after_keyword = if line.starts_with("static ") { &line[7..] } else { &line[6..] };
    let name_end = after_keyword.find(':').unwrap_or(after_keyword.len());
    let var_name = after_keyword[..name_end].trim().trim_start_matches("mut ").to_string();

    let crate_prefix = format!("{}::", crate_name);

    if let Some(start) = line.find('<') {
        if let Some(end) = line.find('>') {
            if end > start + 1 {
                let inner = &line[start + 1..end];
                if inner.starts_with(&crate_prefix) {
                    let type_end = inner.find("::").map(|p| {
                        let rest = &inner[p + 2..];
                        p + 2 + rest.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(rest.len())
                    }).unwrap_or(inner.len());
                    return Some((var_name, inner[..type_end].to_string()));
                }
                if let Some(full_path) = tracker.type_aliases.get(inner.trim()) {
                    return Some((var_name, full_path.clone()));
                }
            }
        }
    }

    None
}

/// Find method calls on a variable in a line
fn find_method_calls(line: &str, var_name: &str) -> Vec<String> {
    let mut methods = Vec::new();

    let pattern = format!("{}.", var_name);
    let mut search_pos = 0;

    while let Some(pos) = line[search_pos..].find(&pattern) {
        let abs_pos = search_pos + pos;
        let after_dot = &line[abs_pos + pattern.len()..];

        let method_end = after_dot.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after_dot.len());
        let method_name = &after_dot[..method_end];

        if !method_name.is_empty() && method_name.chars().next().map(|c| c.is_lowercase()).unwrap_or(false) {
            methods.push(method_name.to_string());
        }

        search_pos = abs_pos + pattern.len();
    }

    methods
}

/// Infer the kind of an item based on naming conventions
pub fn infer_item_kind(path: &str, context: &str) -> ItemKind {
    let parts: Vec<&str> = path.split("::").collect();

    if parts.is_empty() {
        return ItemKind::Unknown;
    }

    let last = parts.last().unwrap();

    if context.contains(&format!("{}(", last)) {
        if parts.len() > 1 {
            return ItemKind::Method;
        }
        return ItemKind::Function;
    }

    if context.starts_with("use ") && context.ends_with(";") {
        let last_char = last.chars().next().unwrap_or(' ');
        if last_char.is_lowercase() && !last.contains('(') {
            return ItemKind::Module;
        }
    }

    if last.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
        ItemKind::Struct
    } else if *last == "new" || *last == "default" || last.starts_with("from_") {
        ItemKind::Method
    } else {
        ItemKind::Function
    }
}

/// Parse a use statement to extract used items
pub fn parse_use_statement(line: &str, crate_name: &str) -> Option<Vec<UsedItem>> {
    let line = line.trim_start_matches("use ").trim_end_matches(';').trim();

    if !line.starts_with(crate_name) {
        return None;
    }

    let mut items = Vec::new();
    let rest = &line[crate_name.len()..];

    if rest.starts_with("::") {
        let rest = &rest[2..];

        if let Some(brace_pos) = rest.find('{') {
            let prefix = &rest[..brace_pos].trim_end_matches("::");
            let mut depth = 0;
            let mut end_pos = brace_pos;
            for (i, ch) in rest[brace_pos..].char_indices() {
                match ch {
                    '{' => depth += 1,
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            end_pos = brace_pos + i;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            // Skip if no matching closing brace found (multi-line or truncated)
            if end_pos <= brace_pos {
                return None;
            }
            let inner = &rest[brace_pos + 1..end_pos];
            let mut item_depth = 0;
            let mut current = String::new();
            for ch in inner.chars() {
                match ch {
                    '{' => {
                        item_depth += 1;
                        current.push(ch);
                    }
                    '}' => {
                        item_depth -= 1;
                        current.push(ch);
                    }
                    ',' if item_depth == 0 => {
                        let part = current.trim();
                        if !part.is_empty() {
                            let full_path = if prefix.is_empty() {
                                format!("{}::{}", crate_name, part)
                            } else {
                                format!("{}::{}::{}", crate_name, prefix, part)
                            };
                            items.push(UsedItem {
                                path: full_path,
                                kind: infer_item_kind(part, line),
                            });
                        }
                        current = String::new();
                    }
                    _ => current.push(ch),
                }
            }
            let part = current.trim();
            if !part.is_empty() {
                let full_path = if prefix.is_empty() {
                    format!("{}::{}", crate_name, part)
                } else {
                    format!("{}::{}::{}", crate_name, prefix, part)
                };
                items.push(UsedItem {
                    path: full_path,
                    kind: infer_item_kind(part, line),
                });
            }
        } else {
            items.push(UsedItem {
                path: format!("{}::{}", crate_name, rest),
                kind: infer_item_kind(rest, line),
            });
        }
    }

    if items.is_empty() {
        None
    } else {
        Some(items)
    }
}

/// Compute transitive dependencies for a set of used items
#[allow(dead_code)]
pub fn compute_transitive_deps(used: &HashSet<UsedItem>) -> HashSet<String> {
    let mut all_needed = HashSet::new();

    for item in used {
        all_needed.insert(item.path.clone());

        match item.kind {
            ItemKind::Method => {
                if let Some(parent) = get_parent_type(&item.path) {
                    all_needed.insert(parent);
                }
            }
            _ => {}
        }
    }

    all_needed
}

/// Get parent type from a method path
pub fn get_parent_type(path: &str) -> Option<String> {
    let parts: Vec<&str> = path.split("::").collect();
    if parts.len() >= 2 {
        Some(parts[..parts.len()-1].join("::"))
    } else {
        None
    }
}

/// Generate a report of used items
pub fn generate_report(used: &HashSet<UsedItem>, crate_name: &str) {
    println!("=== Crate Usage Analysis for '{}' ===\n", crate_name);

    let mut by_kind: HashMap<&ItemKind, Vec<&UsedItem>> = HashMap::new();
    for item in used {
        by_kind.entry(&item.kind).or_default().push(item);
    }

    println!("Summary:");
    println!("  Total items used: {}", used.len());
    for (kind, items) in &by_kind {
        println!("  {:?}: {}", kind, items.len());
    }

    println!("\nDetailed usage:");
    for (kind, items) in &by_kind {
        println!("\n{:?}:", kind);
        let mut sorted_items: Vec<_> = items.iter().collect();
        sorted_items.sort_by(|a, b| a.path.cmp(&b.path));
        for item in sorted_items {
            println!("  - {}", item.path);
        }
    }

    let transitive = compute_transitive_deps(used);
    if transitive.len() > used.len() {
        println!("\nTransitive dependencies (computed):");
        let mut sorted: Vec<_> = transitive.iter().collect();
        sorted.sort();
        for dep in sorted {
            if !used.iter().any(|u| &u.path == dep) {
                println!("  + {}", dep);
            }
        }
    }

    println!("\n=== Potential Savings ===");
    println!("If {} has 10,000 items and you use {}, that's {:.2}% of the API.",
             crate_name, used.len(), (used.len() as f64 / 10000.0) * 100.0);
    println!("Source-level slicing could eliminate ~{:.0}% of compile time for this dependency.",
             100.0 - (used.len() as f64 / 10000.0) * 100.0);

    println!("\n=== POC Limitations ===");
    println!("  - Method calls detected via string-based variable tracking (may have false positives)");
    println!("  - Glob imports (use foo::*) not expanded to specific items");
    println!("  - Trait method calls not yet detected (e.g., .par_iter())");
    println!("  - Type parameters and generics not analyzed");
    println!("  - Full implementation would use rust-analyzer or rustc query system");
}

// ============================================================================
// Inverted Usage Index - O(files) instead of O(crates × files)
// ============================================================================

/// Extract ALL crate usages from a file content (not just one target crate).
/// Returns a vector of (crate_name, UsedItem) pairs.
pub fn extract_all_crate_usages(content: &str) -> Vec<(String, UsedItem)> {
    let mut results = Vec::new();
    let mut seen_items = HashSet::new();

    // Track use statements with their full structure for proper parsing
    for line in content.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with("//") || trimmed.starts_with("/*") || trimmed.starts_with("*") {
            continue;
        }

        // Pattern 1: Parse use statements
        if trimmed.starts_with("use ") {
            // Simple extraction without regex - much faster
            let after_use = &trimmed[4..].trim();
            if let Some(double_colon) = after_use.find("::") {
                let crate_name = &after_use[..double_colon];
                // Skip std/core/alloc - these aren't sliceable
                if is_sliceable_crate_name(crate_name) {
                    if let Some(items) = parse_use_statement(trimmed, crate_name) {
                        for item in items {
                            // Use seen_items to prevent duplicates
                            let key = format!("{}:{}", crate_name, &item.path);
                            if seen_items.insert(key) {
                                results.push((crate_name.to_string(), item));
                            }
                        }
                    }
                }
            }
        }

        // Pattern 2: Direct path usage (e.g., regex::Regex::new())
        // Only look for obvious patterns, not every possible :: occurrence
        // This prevents false positives from things like "foo.bar::baz()"
        let mut search_pos = 0;
        while search_pos < trimmed.len() {
            if let Some(double_colon) = trimmed[search_pos..].find("::") {
                let abs_pos = search_pos + double_colon;

                // Look backwards to find the crate name
                let before = &trimmed[..abs_pos];
                let crate_start = before.rfind(|c: char| !c.is_alphanumeric() && c != '_')
                    .map(|i| i + 1)
                    .unwrap_or(0);

                if crate_start < abs_pos {
                    let crate_name = &before[crate_start..];

                    // Only process if this looks like a crate name and not a method call
                    if is_sliceable_crate_name(crate_name) && !before[..crate_start].trim_end().ends_with('.') {
                        // Extract the path after ::
                        let after = &trimmed[abs_pos + 2..];
                        let path_end = after.find(|c: char| !c.is_alphanumeric() && c != '_' && c != ':')
                            .unwrap_or(after.len());

                        if path_end > 0 {
                            let path_rest = &after[..path_end];
                            let full_path = format!("{}::{}", crate_name, path_rest);
                            let kind = infer_item_kind(path_rest, trimmed);

                            // Use seen_items to prevent duplicates
                            let key = format!("{}:{}", crate_name, &full_path);
                            if seen_items.insert(key) {
                                results.push((crate_name.to_string(), UsedItem {
                                    path: full_path,
                                    kind,
                                }));
                            }
                        }
                    }
                }

                search_pos = abs_pos + 2;
            } else {
                break;
            }
        }
    }

    results
}

/// Check if a crate name looks like a sliceable third-party crate
fn is_sliceable_crate_name(name: &str) -> bool {
    // Skip Rust built-in crates and common keywords
    !matches!(name,
        "std" | "core" | "alloc" | "proc_macro" |
        "self" | "super" | "crate" |
        "Self" | "r#" |
        "test" | "tests"
    )
}

/// Build an inverted usage index from all source directories.
/// Reads each file once and extracts all crate usages.
///
/// Returns: HashMap<normalized_crate_name, HashSet<UsedItem>>
pub fn build_usage_index(
    project_src: &Path,
    dependent_sources: &[(String, PathBuf)],
    sliceable_crate_names: &HashSet<String>,
) -> HashMap<String, HashSet<UsedItem>> {
    // Collect all source directories and their files
    let mut all_files: Vec<PathBuf> = Vec::new();

    // Add project src files
    all_files.extend(find_rust_files(project_src));

    // Add dependent crate src files
    for (_crate_name, src_path) in dependent_sources {
        if src_path.exists() {
            all_files.extend(find_rust_files(src_path));
        }
    }

    // Normalize crate names (replace - with _) for lookup
    let normalized_sliceable: HashSet<String> = sliceable_crate_names
        .iter()
        .map(|name| name.replace('-', "_"))
        .collect();

    // Process files in parallel and collect all usages
    let file_usages: Vec<Vec<(String, UsedItem)>> = all_files
        .par_iter()
        .filter_map(|file_path| {
            match fs::read_to_string(file_path) {
                Ok(content) => {
                    let usages = extract_all_crate_usages(&content);
                    if usages.is_empty() {
                        None
                    } else {
                        Some(usages)
                    }
                }
                Err(_) => None,
            }
        })
        .collect();

    // Build the inverted index
    let mut index: HashMap<String, HashSet<UsedItem>> = HashMap::new();

    for usages in file_usages {
        for (crate_name, item) in usages {
            // Only include usages for sliceable crates
            if normalized_sliceable.contains(&crate_name) {
                index.entry(crate_name)
                    .or_default()
                    .insert(item);
            }
        }
    }

    index
}

/// Collect all dependent source directories for sliceable crates.
/// Returns: Vec<(crate_name, src_path)>
pub fn collect_dependent_sources(
    sliceable_crates: &HashSet<String>,
    get_reverse_deps: impl Fn(&str) -> Vec<&str>,
    find_crate_source: impl Fn(&str) -> Option<PathBuf>,
) -> Vec<(String, PathBuf)> {
    let mut sources = Vec::new();
    let mut seen = HashSet::new();

    for crate_name in sliceable_crates {
        let dependents = get_reverse_deps(crate_name);
        for dependent in dependents {
            if seen.insert(dependent.to_string()) {
                if let Some(src_path) = find_crate_source(dependent) {
                    sources.push((dependent.to_string(), src_path));
                }
            }
        }
    }

    sources
}

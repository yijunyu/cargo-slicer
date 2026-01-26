//! Post-slice dependency injection with rule learning.
//!
//! Phase 1: Reactively fix missing types and log context for pattern analysis.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use regex::Regex;
use serde::{Serialize, Deserialize};

/// Log entry for each reactive fix
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FixLogEntry {
    pub typename: String,
    pub usage_context: String,  // "extern fn param", "struct field", etc.
    pub definition_location: String,
    pub module_path: String,
}

/// Extract all crate-root type references (:: typename) from generated code
pub fn extract_crate_root_type_refs(code: &str) -> HashSet<String> {
    let mut types = HashSet::new();

    // Pattern: :: typename (with word boundary after typename)
    // Matches: :: c_int, :: timespec, etc.
    // Doesn't match: ::std::, ::crate::
    let re = Regex::new(r"::\s*([A-Z_][a-zA-Z0-9_]*)\b").unwrap();

    for cap in re.captures_iter(code) {
        if let Some(typename) = cap.get(1) {
            let name = typename.as_str();
            // Skip known stdlib modules and keywords
            if !is_stdlib_module(name) && !is_rust_keyword(name) {
                types.insert(name.to_string());
            }
        }
    }

    types
}

/// Check if a name is a stdlib module that shouldn't be treated as a type
fn is_stdlib_module(name: &str) -> bool {
    matches!(name,
        "std" | "core" | "alloc" | "Option" | "Some" | "None" |
        "Result" | "Ok" | "Err" | "Box" | "Vec" | "String"
    )
}

/// Check if a name is a Rust keyword
fn is_rust_keyword(name: &str) -> bool {
    crate::constants::RUST_KEYWORDS.contains(&name)
}

/// Check if a type exists in the generated code
pub fn type_exists_in_code(code: &str, typename: &str) -> bool {
    // Check for struct, enum, type alias, or use statement
    let patterns = [
        format!(r"\bpub\s+struct\s+{}\b", regex::escape(typename)),
        format!(r"\bpub\s+enum\s+{}\b", regex::escape(typename)),
        format!(r"\bpub\s+type\s+{}\s*=", regex::escape(typename)),
        format!(r"\bpub\s+use\s+.*\b{}\b", regex::escape(typename)),
        // Also check for re-exports at crate root
        format!(r"\bpub\s+use\s+.*::{}\s*;", regex::escape(typename)),
    ];

    for pattern in &patterns {
        if let Ok(re) = Regex::new(pattern) {
            if re.is_match(code) {
                return true;
            }
        }
    }

    false
}

/// Find type definition in original crate source
pub fn find_type_definition(
    crate_path: &Path,
    typename: &str,
) -> Option<(String, String, String)> {
    // Search all .rs files in the crate
    let src_dir = crate_path.join("src");
    if !src_dir.exists() {
        return None;
    }

    find_type_in_directory(&src_dir, typename, "")
}

/// Recursively search directory for type definition
fn find_type_in_directory(
    dir: &Path,
    typename: &str,
    module_prefix: &str,
) -> Option<(String, String, String)> {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_file() && path.extension().map_or(false, |e| e == "rs") {
                if let Ok(content) = fs::read_to_string(&path) {
                    if let Some((context, def)) = extract_type_definition(&content, typename) {
                        // Determine module path
                        let module_path = if path.file_name() == Some("lib.rs".as_ref())
                            || path.file_name() == Some("mod.rs".as_ref()) {
                            module_prefix.to_string()
                        } else if let Some(stem) = path.file_stem() {
                            if module_prefix.is_empty() {
                                stem.to_string_lossy().to_string()
                            } else {
                                format!("{}::{}", module_prefix, stem.to_string_lossy())
                            }
                        } else {
                            module_prefix.to_string()
                        };

                        return Some((module_path, def, context));
                    }
                }
            } else if path.is_dir() {
                // Recursively search subdirectories
                let dir_name = path.file_name()?.to_string_lossy();
                let new_prefix = if module_prefix.is_empty() {
                    dir_name.to_string()
                } else {
                    format!("{}::{}", module_prefix, dir_name)
                };

                if let Some(result) = find_type_in_directory(&path, typename, &new_prefix) {
                    return Some(result);
                }
            }
        }
    }

    None
}

/// Extract type definition from source code
fn extract_type_definition(code: &str, typename: &str) -> Option<(String, String)> {
    let lines: Vec<&str> = code.lines().collect();

    // Patterns to match different type definitions
    let struct_re = Regex::new(&format!(r"^\s*pub\s+struct\s+{}\b", regex::escape(typename))).ok()?;
    let enum_re = Regex::new(&format!(r"^\s*pub\s+enum\s+{}\b", regex::escape(typename))).ok()?;
    let type_re = Regex::new(&format!(r"^\s*pub\s+type\s+{}\s*=", regex::escape(typename))).ok()?;

    for (i, line) in lines.iter().enumerate() {
        // Check for struct
        if struct_re.is_match(line) {
            let def = extract_complete_definition(&lines, i, "struct")?;
            return Some(("struct definition".to_string(), def));
        }

        // Check for enum
        if enum_re.is_match(line) {
            let def = extract_complete_definition(&lines, i, "enum")?;
            return Some(("enum definition".to_string(), def));
        }

        // Check for type alias
        if type_re.is_match(line) {
            let def = extract_complete_definition(&lines, i, "type")?;
            return Some(("type alias".to_string(), def));
        }
    }

    None
}

/// Extract complete definition (handling multi-line definitions)
fn extract_complete_definition(lines: &[&str], start_idx: usize, def_type: &str) -> Option<String> {
    let mut result = String::new();
    let mut brace_count = 0;
    let mut started = false;

    for line in &lines[start_idx..] {
        result.push_str(line);
        result.push('\n');

        // For type aliases, stop at semicolon
        if def_type == "type" {
            if line.contains(';') {
                return Some(result.trim().to_string());
            }
        } else {
            // For struct/enum, track braces
            for ch in line.chars() {
                match ch {
                    '{' => {
                        brace_count += 1;
                        started = true;
                    }
                    '}' => {
                        brace_count -= 1;
                        if started && brace_count == 0 {
                            return Some(result.trim().to_string());
                        }
                    }
                    _ => {}
                }
            }
        }

        // Safety limit
        if result.len() > 10000 {
            break;
        }
    }

    Some(result.trim().to_string())
}

/// Determine usage context by analyzing where the type appears
pub fn determine_usage_context(code: &str, typename: &str) -> String {
    let lines: Vec<&str> = code.lines().collect();

    // Search for typical usage patterns
    for (i, line) in lines.iter().enumerate() {
        if line.contains(&format!("::{}", typename)) || line.contains(&format!(":: {}", typename)) {
            // Check surrounding context
            let context_start = i.saturating_sub(5);
            let context_end = (i + 5).min(lines.len());
            let context: String = lines[context_start..context_end].join("\n");

            // Determine context type
            if context.contains("extern \"C\"") || context.contains("extern \"system\"") {
                if context.contains("fn ") {
                    return "extern fn signature".to_string();
                }
            }

            if context.contains("pub struct") {
                return "struct field".to_string();
            }

            if context.contains("pub enum") {
                return "enum variant".to_string();
            }

            if context.contains("pub fn") || context.contains("fn ") {
                return "function signature".to_string();
            }

            if context.contains("impl ") {
                return "impl block".to_string();
            }

            return "type reference".to_string();
        }
    }

    "unknown context".to_string()
}

/// Fix missing types in sliced crate and log fixes
pub fn fix_missing_types(
    original_crate_path: &Path,
    sliced_crate_path: &Path,
    crate_name: &str,
) -> Result<Vec<FixLogEntry>, String> {
    let mut log_entries = Vec::new();

    // Read the generated lib.rs
    let lib_rs_path = sliced_crate_path.join("src/lib.rs");
    let code = fs::read_to_string(&lib_rs_path)
        .map_err(|e| format!("Failed to read lib.rs: {}", e))?;

    // Extract all :: typename references
    let type_refs = extract_crate_root_type_refs(&code);

    // Check which types are missing
    let mut missing_types = Vec::new();
    for typename in &type_refs {
        if !type_exists_in_code(&code, typename) {
            missing_types.push(typename.clone());
        }
    }

    if missing_types.is_empty() {
        return Ok(log_entries);
    }

    // For each missing type, find definition and inject
    let mut fixes_to_inject = HashMap::new();

    for typename in &missing_types {
        if let Some((module_path, definition, _context)) = find_type_definition(original_crate_path, typename) {
            // Determine usage context
            let usage_context = determine_usage_context(&code, typename);

            // Log this fix
            let log_entry = FixLogEntry {
                typename: typename.clone(),
                usage_context,
                definition_location: format!("{}::{}", crate_name, module_path),
                module_path: module_path.clone(),
            };
            log_entries.push(log_entry);

            // Store for injection
            fixes_to_inject.insert(typename.clone(), (module_path, definition));
        }
    }

    // Inject missing types into lib.rs
    if !fixes_to_inject.is_empty() {
        inject_types_into_lib_rs(&lib_rs_path, &code, fixes_to_inject)?;
    }

    Ok(log_entries)
}

/// Inject missing type definitions into lib.rs
fn inject_types_into_lib_rs(
    lib_rs_path: &Path,
    original_code: &str,
    fixes: HashMap<String, (String, String)>,
) -> Result<(), String> {
    let mut new_code = original_code.to_string();

    // Add a comment section for injected types
    new_code.push_str("\n\n// === Types injected by post-slice fixer ===\n");
    new_code.push_str("// These types were referenced but missing from the slice\n\n");

    for (typename, (_module_path, definition)) in fixes {
        new_code.push_str(&format!("// Injected: {}\n", typename));
        new_code.push_str(&definition);
        new_code.push_str("\n\n");
    }

    // Write back to file
    fs::write(lib_rs_path, new_code)
        .map_err(|e| format!("Failed to write lib.rs: {}", e))?;

    Ok(())
}

/// Save fix log to JSON file
pub fn save_fix_log(log_entries: &[FixLogEntry], output_path: &Path) -> Result<(), String> {
    let json = serde_json::to_string_pretty(log_entries)
        .map_err(|e| format!("Failed to serialize log: {}", e))?;

    fs::write(output_path, json)
        .map_err(|e| format!("Failed to write log file: {}", e))?;

    Ok(())
}

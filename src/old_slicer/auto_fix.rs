//! Auto-fix for compilation errors.
//!
//! Functions for parsing compilation errors and generating stubs.

use std::collections::HashSet;
use std::fs;
use std::path::Path;
use std::process::Command;

use crate::types::MissingItem;

/// Check if a crate compiles successfully
pub fn check_crate_compiles(crate_dir: &Path) -> bool {
    let output = Command::new("cargo")
        .args(["check"])
        .current_dir(crate_dir)
        .output();

    output.map(|o| o.status.success()).unwrap_or(false)
}

/// Parse cargo check errors to find missing items
pub fn parse_compilation_errors(crate_dir: &Path) -> Vec<MissingItem> {
    let output = Command::new("cargo")
        .args(["check", "--message-format=short"])
        .current_dir(crate_dir)
        .output();

    let output = match output {
        Ok(o) => o,
        Err(_) => return Vec::new(),
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    let mut missing = HashSet::new();

    for line in stderr.lines() {
        if line.contains("cannot find type") {
            if let Some(name) = extract_quoted_name(line) {
                if !name.contains("::") && is_valid_ident(&name) {
                    missing.insert(MissingItem::Type(name));
                }
            }
        } else if line.contains("cannot find trait") {
            if let Some(name) = extract_quoted_name(line) {
                if !name.contains("::") && is_valid_ident(&name) {
                    missing.insert(MissingItem::Trait(name));
                }
            }
        } else if line.contains("cannot find function") {
            if let Some(name) = extract_quoted_name(line) {
                if !name.contains("::") && is_valid_ident(&name) {
                    let is_pascal_case = name.chars().next()
                        .map(|c| c.is_uppercase())
                        .unwrap_or(false);
                    if !is_pascal_case {
                        missing.insert(MissingItem::Function(name));
                    }
                }
            }
        } else if line.contains("cannot find macro") {
            if let Some(name) = extract_quoted_name(line) {
                if is_valid_ident(&name) {
                    missing.insert(MissingItem::Macro(name));
                }
            }
        }
    }

    missing.into_iter().collect()
}

/// Extract a quoted name like `Foo` from an error message
pub fn extract_quoted_name(line: &str) -> Option<String> {
    let start = line.find('`')? + 1;
    let rest = &line[start..];
    let end = rest.find('`')?;
    Some(rest[..end].to_string())
}

/// Check if a string is a valid Rust identifier
pub fn is_valid_ident(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !first.is_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// Generate stub code for missing items
pub fn generate_stubs(missing: &[MissingItem], existing_content: &str, external_items: &HashSet<String>) -> String {
    let mut stubs = String::new();

    for item in missing {
        match item {
            MissingItem::Type(name) => {
                if !existing_content.contains(&format!("struct {} ", name))
                    && !existing_content.contains(&format!("enum {} ", name))
                    && !external_items.contains(name)
                {
                    stubs.push_str(&format!("pub struct {} {{}}\n", name));
                }
            }
            MissingItem::Trait(name) => {
                if !existing_content.contains(&format!("trait {} ", name))
                    && !external_items.contains(name)
                {
                    stubs.push_str(&format!("pub trait {} {{}}\n", name));
                }
            }
            MissingItem::Function(name) => {
                if !existing_content.contains(&format!("fn {}(", name))
                    && !external_items.contains(name)
                {
                    stubs.push_str(&format!("pub fn {}() {{}}\n", name));
                }
            }
            MissingItem::Macro(name) => {
                if !existing_content.contains(&format!("macro_rules! {}", name))
                    && !external_items.contains(name)
                {
                    stubs.push_str(&format!("macro_rules! {} {{ () => {{}} }}\n", name));
                }
            }
            MissingItem::Module(name) => {
                if !existing_content.contains(&format!("mod {} ", name))
                    && !external_items.contains(name)
                {
                    stubs.push_str(&format!("pub mod {} {{}}\n", name));
                }
            }
        }
    }

    stubs
}

/// Auto-fix a sliced crate by iteratively adding stubs
pub fn auto_fix_sliced_crate(crate_dir: &Path, max_iterations: usize) -> Result<usize, String> {
    for iteration in 0..max_iterations {
        if check_crate_compiles(crate_dir) {
            return Ok(iteration);
        }

        let missing = parse_compilation_errors(crate_dir);
        if missing.is_empty() {
            return Err("Compilation errors but no missing items detected".to_string());
        }

        let lib_path = crate_dir.join("src").join("lib.rs");
        let content = fs::read_to_string(&lib_path)
            .map_err(|e| format!("Failed to read lib.rs: {}", e))?;

        let stubs = generate_stubs(&missing, &content, &HashSet::new());
        if stubs.is_empty() {
            return Err("No stubs generated but still have errors".to_string());
        }

        let new_content = format!("{}\n// Auto-generated stubs\n{}", content, stubs);
        fs::write(&lib_path, new_content)
            .map_err(|e| format!("Failed to write lib.rs: {}", e))?;
    }

    Err(format!("Max iterations ({}) reached", max_iterations))
}

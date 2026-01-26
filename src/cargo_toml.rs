//! Cargo.toml handling.
//!
//! Functions for parsing and generating Cargo.toml files.

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use super::types::CrateDependencies;

/// Helper enum to track current dependency section
enum DependencyType {
    None,
    Normal,
    Dev,
    Build,
}

/// Parse dependencies from a Cargo.toml file
pub fn parse_cargo_toml(crate_path: &Path) -> Option<CrateDependencies> {
    let cargo_path = crate_path.join("Cargo.toml");
    let content = fs::read_to_string(&cargo_path).ok()?;

    let mut deps = CrateDependencies::default();
    let mut current_section = "";
    let mut current_dep_name: Option<String> = None;
    let mut current_dep_version: Option<String> = None;
    let mut current_dep_package: Option<String> = None;
    let mut current_dep_optional = false;
    let mut current_dep_type = DependencyType::None;

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            // Save previous sub-table dependency if any
            if let (Some(name), Some(version)) = (current_dep_name.take(), current_dep_version.take()) {
                // If this dependency has a package rename, use the actual package name
                // e.g., [dependencies.foo_] with package = "foo" should use "foo" as the key
                let actual_name = current_dep_package.take().unwrap_or(name);
                // Format as table if optional, otherwise just version
                let value = if current_dep_optional {
                    format!("{{ version = \"{}\", optional = true }}", version)
                } else {
                    format!("\"{}\"", version)
                };
                match current_dep_type {
                    DependencyType::Normal => {
                        deps.dependencies.insert(actual_name, value);
                    }
                    DependencyType::Dev => {
                        deps.dev_dependencies.insert(actual_name, value);
                    }
                    DependencyType::Build => {
                        deps.build_dependencies.insert(actual_name, value);
                    }
                    DependencyType::None => {} // Should not happen
                }
                current_dep_optional = false;
            }

            current_section = trimmed;

            // Check for sub-table dependencies like [dependencies.bytes]
            if current_section.starts_with("[dependencies.") && current_section.ends_with(']') {
                let dep_name = &current_section[14..current_section.len() - 1];
                current_dep_name = Some(dep_name.to_string());
                current_dep_package = None; // Reset for new dependency
                current_dep_type = DependencyType::Normal;
            } else if current_section.starts_with("[dev-dependencies.") && current_section.ends_with(']') {
                let dep_name = &current_section[18..current_section.len() - 1];
                current_dep_name = Some(dep_name.to_string());
                current_dep_package = None;
                current_dep_type = DependencyType::Dev;
            } else if current_section.starts_with("[build-dependencies.") && current_section.ends_with(']') {
                let dep_name = &current_section[19..current_section.len() - 1];
                current_dep_name = Some(dep_name.to_string());
                current_dep_package = None;
                current_dep_type = DependencyType::Build;
            } else {
                current_dep_name = None;
                current_dep_package = None;
                current_dep_type = DependencyType::None; // Reset type
            }
            continue;
        }

        // Handle sub-table dependency properties
        if current_dep_name.is_some() {
            if trimmed.starts_with("version") {
                if let Some(eq_pos) = trimmed.find('=') {
                    let version = trimmed[eq_pos + 1..].trim().trim_matches('"');
                    current_dep_version = Some(version.to_string());
                }
            } else if trimmed.starts_with("package") {
                // Handle renamed dependencies like [dependencies.foo_] with package = "foo"
                if let Some(eq_pos) = trimmed.find('=') {
                    let package = trimmed[eq_pos + 1..].trim().trim_matches('"');
                    current_dep_package = Some(package.to_string());
                }
            } else if trimmed.starts_with("optional") && trimmed.contains("true") {
                current_dep_optional = true;
            }
            continue;
        }

        if current_section == "[dependencies]" {
            if let Some((name, value)) = parse_dependency_line_with_value(trimmed) {
                deps.dependencies.insert(name.to_string(), value.to_string());
            }
        } else if current_section == "[dev-dependencies]" {
            if let Some((name, value)) = parse_dependency_line_with_value(trimmed) {
                deps.dev_dependencies.insert(name.to_string(), value.to_string());
            }
        } else if current_section == "[build-dependencies]" {
            if let Some((name, value)) = parse_dependency_line_with_value(trimmed) {
                deps.build_dependencies.insert(name.to_string(), value.to_string());
            }
        } else if current_section == "[features]" {
            deps.features_section.push_str(line);
            deps.features_section.push('\n');
        }
    }

    // Save any pending sub-table dependency at the end of the file
    if let Some(name) = current_dep_name.take() {
        if let Some(version) = current_dep_version.take() {
            let actual_name = current_dep_package.unwrap_or(name);
            let value = if current_dep_optional {
                format!("{{ version = \"{}\", optional = true }}", version)
            } else {
                format!("\"{}\"", version)
            };
            match current_dep_type {
                DependencyType::Normal => {
                    deps.dependencies.insert(actual_name, value);
                }
                DependencyType::Dev => {
                    deps.dev_dependencies.insert(actual_name, value);
                }
                DependencyType::Build => {
                    deps.build_dependencies.insert(actual_name, value);
                }
                DependencyType::None => {} // Should not happen
            }
        }
    }

    Some(deps)
}

/// Parse a single dependency line, returning (name, value) where value can be "version" or "{ path = ... }"
fn parse_dependency_line_with_value(line: &str) -> Option<(&str, &str)> {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return None;
    }

    let parts: Vec<&str> = trimmed.splitn(2, '=').collect();
    if parts.len() != 2 {
        return None;
    }

    let name = parts[0].trim();
    let mut value = parts[1].trim(); // Keep the full value string (e.g., "1.0" or { path = "..." })

    // Strip trailing comments (# ...) but be careful not to strip # inside strings or tables
    if let Some(comment_pos) = value.find('#') {
        // Only strip if the # is outside quotes and braces
        let before_comment = &value[..comment_pos].trim_end();
        // Simple heuristic: if we have balanced quotes/braces before #, it's a comment
        if before_comment.ends_with('"') || before_comment.ends_with('}') {
            value = before_comment;
        }
    }

    Some((name, value))
}

/// Convert a dependency to a TOML string
pub fn dependency_to_string(name: &str, value: &str) -> String {
    // If value already contains { or is a quoted string, use it directly
    if value.starts_with('{') || (value.starts_with('"') && value.ends_with('"')) {
        format!("{} = {}", name, value)
    } else {
        format!("{} = \"{}\"", name, value)
    }
}

/// Augment default features with additional features
pub fn augment_default_features(features_section: &str, enabled_features: &HashSet<String>) -> String {
    let mut result = features_section.to_string();

    for feature in enabled_features {
        if !result.contains(&format!("\"{}\"", feature)) {
            // Add to default if it exists, otherwise create it
            if result.contains("default = [") {
                result = result.replace("default = [", &format!("default = [\"{}\", ", feature));
            }
        }
    }

    result
}

/// Validate features section against available dependencies
pub fn validate_features_section(features_section: &str, _available_deps: &HashSet<String>) -> String {
    // For now, just return the original section
    // A full implementation would filter out references to unavailable deps
    features_section.to_string()
}

/// Remove spurious dependencies from Cargo.toml that match local module names or self-dependencies.
/// This fixes cases where spurious imports like "use parking_lot::THREAD_DATA;" in parking_lot_core
/// cause the dependency detector to add parking_lot as a dependency even though it's a local module.
/// Also removes self-dependencies like proc-macro2 depending on proc-macro2.
/// Also removes mutual circular dependencies like serde ← serde_core → serde.
pub fn remove_spurious_dependencies(cargo_toml_path: &Path, local_modules: &HashSet<String>) -> Result<(), std::io::Error> {
    let content = fs::read_to_string(cargo_toml_path)?;

    // Extract the crate name from [package] section
    let crate_name = extract_crate_name(&content).unwrap_or_default();

    // Known mutual circular dependencies to break
    // Format: (crate_a, crate_b) means if we're in crate_a, remove dependency on crate_b
    let circular_pairs = vec![
        ("serde", "serde_core"),  // serde ← serde_core → serde (core is internal module of serde)
        ("serde_core", "serde"),  // Break both directions
        ("serde_derive", "serde"), // serde_derive ← serde (proc-macro shouldn't depend on runtime lib)
    ];

    let mut lines = Vec::new();
    let mut in_dependencies = false;
    let mut skip_next_lines = 0;

    for line in content.lines() {
        // Check if we're entering or leaving [dependencies] section
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            if trimmed == "[dependencies]" {
                in_dependencies = true;
                lines.push(line.to_string());
                continue;
            } else {
                in_dependencies = false;
            }
        }

        // Skip lines if we're in a multi-line dependency we want to remove
        if skip_next_lines > 0 {
            skip_next_lines -= 1;
            continue;
        }

        // If we're in [dependencies] section, check if this line is a spurious dependency
        if in_dependencies && !trimmed.is_empty() && !trimmed.starts_with('#') {
            // Check if this is a dependency line
            if let Some(dep_name) = extract_dependency_name(trimmed) {
                // Check for self-dependency: crate depending on itself
                // Normalize both names for comparison (handle hyphen vs underscore)
                let normalized_dep = dep_name.replace('-', "_");
                let normalized_crate = crate_name.replace('-', "_");

                if normalized_dep == normalized_crate {
                    // Skip self-dependency
                    continue;
                }

                // Check if this is a known circular dependency to break
                let is_circular = circular_pairs.iter().any(|(a, b)| {
                    let norm_a = a.replace('-', "_");
                    let norm_b = b.replace('-', "_");
                    normalized_crate == norm_a && normalized_dep == norm_b
                });

                if is_circular {
                    // Skip this circular dependency
                    continue;
                }

                // Check if any local module name matches this dependency
                let is_local_module = local_modules.iter().any(|m| {
                    let first_segment = m.split("::").next().unwrap_or(m);
                    first_segment == normalized_dep || first_segment.replace('-', "_") == normalized_dep
                });

                if is_local_module {
                    // Skip this spurious dependency
                    // If it's a multi-line table dependency like:
                    // parking_lot = { path = "..." }
                    // We only skip this one line since it's inline
                    continue;
                }
            }
        }

        lines.push(line.to_string());
    }

    // Write back the cleaned content
    let cleaned = lines.join("\n");
    fs::write(cargo_toml_path, cleaned)?;

    Ok(())
}

/// Extract crate name from [package] section of Cargo.toml
fn extract_crate_name(content: &str) -> Option<String> {
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("name = ") {
            if let Some(eq_pos) = trimmed.find('=') {
                let name = trimmed[eq_pos + 1..].trim().trim_matches('"');
                return Some(name.to_string());
            }
        }
    }
    None
}

/// Extract dependency name from a Cargo.toml dependency line
fn extract_dependency_name(line: &str) -> Option<&str> {
    let trimmed = line.trim();

    // Handle simple format: name = "version"
    // Handle table format: name = { ... }
    // Handle path format: name = { path = "..." }
    if let Some(eq_pos) = trimmed.find('=') {
        let name = trimmed[..eq_pos].trim();
        if !name.is_empty() && !name.contains('"') && !name.contains('{') {
            return Some(name);
        }
    }

    None
}

/// Check if a crate is a proc-macro crate by reading its Cargo.toml
///
/// Proc-macro crates have `proc-macro = true` in their [lib] section.
/// These crates generate code that other crates compile, so they should
/// only depend on what's in their original Cargo.toml, not on dependencies
/// detected from ::qualified::paths in quote! blocks (which are generated code).
pub fn is_proc_macro_crate(cargo_toml_path: &Path) -> bool {
    if let Ok(content) = fs::read_to_string(cargo_toml_path) {
        let mut in_lib_section = false;

        for line in content.lines() {
            let trimmed = line.trim();

            // Check for [lib] section start
            if trimmed == "[lib]" {
                in_lib_section = true;
            }
            // Check for other section start (ends [lib] section)
            else if trimmed.starts_with('[') && trimmed.ends_with(']') {
                in_lib_section = false;
            }
            // Check for proc-macro = true in [lib] section
            else if in_lib_section && trimmed.starts_with("proc-macro") {
                // Look for "proc-macro = true" or "proc-macro=true"
                if let Some(eq_pos) = trimmed.find('=') {
                    let value = trimmed[eq_pos + 1..].trim();
                    if value == "true" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    /// Test that parse_cargo_toml handles normalized format with [dependencies.crate_name]
    /// This test would FAIL on an implementation that only parsed simple format
    #[test]
    fn test_parse_cargo_toml_normalized_format() {
        let temp_dir = TempDir::new().unwrap();
        let cargo_toml = temp_dir.path().join("Cargo.toml");

        // Normalized format as produced by cargo publish
        let content = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies.once_cell]
version = "1.9.0"

[dependencies.bytes]
version = "1.1"
optional = true

[dependencies.protobuf-support]
version = "=3.7.2"
"#;
        std::fs::write(&cargo_toml, content).unwrap();

        let deps = parse_cargo_toml(temp_dir.path()).expect("Should parse cargo toml");

        assert!(deps.dependencies.contains_key("once_cell"), "once_cell should be parsed from normalized format");
        assert!(deps.dependencies.contains_key("bytes"), "bytes should be parsed from normalized format");
        assert!(deps.dependencies.contains_key("protobuf-support"), "protobuf-support should be parsed from normalized format");

        // Check values
        assert_eq!(deps.dependencies.get("once_cell").unwrap(), "\"1.9.0\"");
        // bytes is optional, should preserve that
        assert!(deps.dependencies.get("bytes").unwrap().contains("optional = true"));
    }

    /// Test parse_cargo_toml with simple format
    #[test]
    fn test_parse_cargo_toml_simple_format() {
        let temp_dir = TempDir::new().unwrap();
        let cargo_toml = temp_dir.path().join("Cargo.toml");

        let content = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies]
once_cell = "1.9.0"
serde = "1.0"
"#;
        std::fs::write(&cargo_toml, content).unwrap();

        let deps = parse_cargo_toml(temp_dir.path()).expect("Should parse cargo toml");

        assert!(deps.dependencies.contains_key("once_cell"), "once_cell should be parsed");
        assert!(deps.dependencies.contains_key("serde"), "serde should be parsed");
    }
}

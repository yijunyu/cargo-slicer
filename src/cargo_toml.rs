//! Cargo.toml handling.
//!
//! Functions for parsing and generating Cargo.toml files.

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use super::types::CrateDependencies;

/// Parse dependencies from a Cargo.toml file
pub fn parse_cargo_toml(crate_path: &Path) -> Option<CrateDependencies> {
    let cargo_path = crate_path.join("Cargo.toml");
    let content = fs::read_to_string(&cargo_path).ok()?;

    let mut deps = CrateDependencies::default();
    let mut current_section = "";
    let mut current_dep_name: Option<String> = None;
    let mut current_dep_version: Option<String> = None;
    let mut current_dep_optional = false;

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            // Save previous sub-table dependency if any
            if let (Some(name), Some(version)) = (current_dep_name.take(), current_dep_version.take()) {
                // Format as table if optional, otherwise just version
                let value = if current_dep_optional {
                    format!("{{ version = \"{}\", optional = true }}", version)
                } else {
                    format!("\"{}\"", version)
                };
                deps.dependencies.insert(name, value);
                current_dep_optional = false;
            }

            current_section = trimmed;

            // Check for sub-table dependencies like [dependencies.bytes]
            if current_section.starts_with("[dependencies.") && current_section.ends_with(']') {
                let dep_name = &current_section[14..current_section.len()-1];
                current_dep_name = Some(dep_name.to_string());
            } else if current_section.starts_with("[dev-dependencies.") && current_section.ends_with(']') {
                // Skip dev-dependencies sub-tables for now
                current_dep_name = None;
            } else {
                current_dep_name = None;
            }
            continue;
        }

        // Handle sub-table dependency properties
        if current_dep_name.is_some() {
            if trimmed.starts_with("version") {
                if let Some(eq_pos) = trimmed.find('=') {
                    let version = trimmed[eq_pos+1..].trim().trim_matches('"');
                    current_dep_version = Some(version.to_string());
                }
            } else if trimmed.starts_with("optional") && trimmed.contains("true") {
                current_dep_optional = true;
            }
            continue;
        }

        if current_section == "[dependencies]" {
            if let Some((name, version)) = parse_dependency_line(trimmed) {
                deps.dependencies.insert(name.to_string(), version.to_string());
            }
        } else if current_section == "[dev-dependencies]" {
            if let Some((name, version)) = parse_dependency_line(trimmed) {
                deps.dev_dependencies.insert(name.to_string(), version.to_string());
            }
        } else if current_section == "[features]" {
            deps.features_section.push_str(line);
            deps.features_section.push('\n');
        }
    }

    // Save final sub-table dependency if any
    if let (Some(name), Some(version)) = (current_dep_name, current_dep_version) {
        let value = if current_dep_optional {
            format!("{{ version = \"{}\", optional = true }}", version)
        } else {
            format!("\"{}\"", version)
        };
        deps.dependencies.insert(name, value);
    }

    Some(deps)
}

/// Parse a single dependency line
fn parse_dependency_line(line: &str) -> Option<(&str, &str)> {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return None;
    }

    let parts: Vec<&str> = trimmed.splitn(2, '=').collect();
    if parts.len() != 2 {
        return None;
    }

    let name = parts[0].trim();
    let version = parts[1].trim().trim_matches('"');

    Some((name, version))
}

/// Convert a dependency to a TOML string
pub fn dependency_to_string(name: &str, value: &str) -> String {
    if value.starts_with('{') || value.contains("version") {
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

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

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            current_section = trimmed;
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

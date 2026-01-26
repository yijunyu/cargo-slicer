/// Dependency decision logging for systematic error analysis
///
/// This module logs every decision point where a dependency is added to the
/// dependency graph, tracking:
/// - What level: crate, module, or type
/// - Why: the condition/reason for inclusion
/// - Where: the code path that triggered the decision
/// - Context: full details for error matching
///
/// Similar to import decision logging, this enables:
/// 1. Matching compilation errors to specific dependency decisions
/// 2. Identifying spurious/incorrect dependencies
/// 3. Understanding why certain dependencies are missing
/// 4. Systematic fixes based on error patterns

use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::Write;
use std::sync::{Mutex, OnceLock};
use std::time::{SystemTime, UNIX_EPOCH};

/// Global dependency decision logger instance
static DEPENDENCY_LOGGER: OnceLock<DependencyDecisionLogger> = OnceLock::new();

/// Get a simple timestamp string
fn get_timestamp() -> String {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(duration) => format!("{}", duration.as_secs()),
        Err(_) => String::from("0"),
    }
}

/// Access the global logger
pub fn logger() -> &'static DependencyDecisionLogger {
    DEPENDENCY_LOGGER.get_or_init(|| {
        DependencyDecisionLogger::new()
    })
}

/// Level at which the dependency was added
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum DependencyLevel {
    /// Dependency added at crate level (affects entire crate)
    Crate,
    /// Dependency added at module level (affects specific module)
    Module,
    /// Dependency added at type level (affects specific type)
    Type,
}

/// Source/reason for the dependency decision
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum DependencySource {
    /// Direct usage found in source code
    DirectUsage,
    /// Transitive dependency (required by another item)
    Transitive,
    /// Type field dependency (struct field type)
    TypeField,
    /// Function parameter dependency
    FunctionParameter,
    /// Function return type dependency
    ReturnType,
    /// Trait bound dependency
    TraitBound,
    /// Impl block dependency
    ImplBlock,
    /// Generic type parameter
    GenericParameter,
    /// Associated type
    AssociatedType,
    /// Macro expansion
    MacroExpansion,
    /// Static/const item type
    StaticType,
    /// Cfg-conditional dependency
    CfgConditional,
    /// External crate dependency (e.g., libc, SIMD crates)
    ExternalCrate,
    /// Module included because trait methods used
    ModuleForTrait,
    /// Union type field dependency
    UnionField,
    /// External crate dependency from Cargo.toml
    CargoTomlDependency,
    /// Re-export chain detection
    ReexportChain,
    /// Other/unknown reason
    Other(String),
}

/// Context about where and why the dependency was added
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyContext {
    /// Crate being sliced
    pub crate_name: String,
    /// Module path where dependency was triggered
    pub module_path: String,
    /// Source file path
    pub file: String,
    /// Line number (if available)
    pub line: Option<usize>,
    /// Code snippet (if available)
    pub snippet: Option<String>,
    /// Cfg conditions active at this point
    pub cfg_conditions: Vec<String>,
    /// Whether this is a test-only dependency
    pub is_test: bool,
    /// Whether this is a platform-specific dependency
    pub platform_specific: Option<String>,
}

/// A single dependency decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyDecision {
    /// Unique ID for this decision
    pub decision_id: String,

    /// What was included (item name)
    pub item_name: String,

    /// What type of item (function, type, module, etc.)
    pub item_type: String,

    /// Full path to the item
    pub item_path: String,

    /// Level at which dependency was added
    pub level: DependencyLevel,

    /// Why this dependency was added
    pub source: DependencySource,

    /// Context about the decision
    pub context: DependencyContext,

    /// Alternative items that could have been used
    pub alternatives: Vec<String>,

    /// What triggered this dependency (the "parent" item)
    pub triggered_by: Option<String>,

    /// Timestamp of the decision
    pub timestamp: String,
}

/// Thread-safe dependency decision logger
pub struct DependencyDecisionLogger {
    log_file: Mutex<Option<File>>,
    enabled: bool,
}

impl DependencyDecisionLogger {
    /// Create a new logger
    pub fn new() -> Self {
        let enabled = std::env::var("CARGO_SLICER_LOG_DEPENDENCIES")
            .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
            .unwrap_or(false);

        let log_file = if enabled {
            let log_path = std::env::var("DEPENDENCY_LOG_PATH")
                .unwrap_or_else(|_| {
                    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
                    format!("{}/precc-slicer/dependency_analysis/dependency_decisions.jsonl", home)
                });

            // Create directory if needed
            if let Some(parent) = std::path::Path::new(&log_path).parent() {
                let _ = std::fs::create_dir_all(parent);
            }

            // Open in append mode
            File::options()
                .create(true)
                .append(true)
                .open(&log_path)
                .ok()
        } else {
            None
        };

        Self {
            log_file: Mutex::new(log_file),
            enabled,
        }
    }

    /// Check if logging is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled || self.log_file.lock().unwrap().is_some()
    }

    /// Log a dependency decision
    pub fn log_decision(&self, decision: DependencyDecision) {
        if !self.is_enabled() {
            return;
        }

        let mut log_file_guard = self.log_file.lock().unwrap();
        if let Some(file) = log_file_guard.as_mut() {
            // Serialize to JSON
            if let Ok(json) = serde_json::to_string(&decision) {
                let _ = writeln!(file, "{}", json);
                let _ = file.flush();
            }
        }
    }

    /// Clear the log file
    pub fn clear(&self) {
        let mut log_file_guard = self.log_file.lock().unwrap();
        if let Some(_) = log_file_guard.take() {
            // Re-open in truncate mode
            let log_path = std::env::var("DEPENDENCY_LOG_PATH")
                .unwrap_or_else(|_| {
                    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
                    format!("{}/precc-slicer/dependency_analysis/dependency_decisions.jsonl", home)
                });

            if let Ok(file) = File::options()
                .create(true)
                .write(true)
                .truncate(true)
                .open(&log_path)
            {
                *log_file_guard = Some(file);
            }
        }
    }
}

/// Helper to generate unique decision ID
pub fn generate_decision_id(
    item_name: &str,
    item_path: &str,
    crate_name: &str,
    triggered_by: Option<&str>,
) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    item_name.hash(&mut hasher);
    item_path.hash(&mut hasher);
    crate_name.hash(&mut hasher);
    if let Some(trigger) = triggered_by {
        trigger.hash(&mut hasher);
    }
    let hash = hasher.finish();

    format!("{}:{}:{:x}", crate_name, item_name, hash)
}

/// Helper to log a crate-level dependency
pub fn log_crate_dependency(
    crate_name: &str,
    item_name: &str,
    item_type: &str,
    item_path: &str,
    source: DependencySource,
    file: &str,
    cfg_conditions: Vec<String>,
    triggered_by: Option<String>,
) {
    if !logger().is_enabled() {
        return;
    }

    let decision = DependencyDecision {
        decision_id: generate_decision_id(item_name, item_path, crate_name, triggered_by.as_deref()),
        item_name: item_name.to_string(),
        item_type: item_type.to_string(),
        item_path: item_path.to_string(),
        level: DependencyLevel::Crate,
        source,
        context: DependencyContext {
            crate_name: crate_name.to_string(),
            module_path: String::new(),
            file: file.to_string(),
            line: None,
            snippet: None,
            cfg_conditions,
            is_test: false,
            platform_specific: None,
        },
        alternatives: vec![],
        triggered_by,
        timestamp: get_timestamp(),
    };

    logger().log_decision(decision);
}

/// Helper to log a module-level dependency
pub fn log_module_dependency(
    crate_name: &str,
    module_path: &str,
    item_name: &str,
    item_type: &str,
    item_path: &str,
    source: DependencySource,
    file: &str,
    line: Option<usize>,
    cfg_conditions: Vec<String>,
    triggered_by: Option<String>,
) {
    if !logger().is_enabled() {
        return;
    }

    let decision = DependencyDecision {
        decision_id: generate_decision_id(item_name, item_path, crate_name, triggered_by.as_deref()),
        item_name: item_name.to_string(),
        item_type: item_type.to_string(),
        item_path: item_path.to_string(),
        level: DependencyLevel::Module,
        source,
        context: DependencyContext {
            crate_name: crate_name.to_string(),
            module_path: module_path.to_string(),
            file: file.to_string(),
            line,
            snippet: None,
            cfg_conditions,
            is_test: false,
            platform_specific: None,
        },
        alternatives: vec![],
        triggered_by,
        timestamp: get_timestamp(),
    };

    logger().log_decision(decision);
}

/// Helper to log a type-level dependency
pub fn log_type_dependency(
    crate_name: &str,
    module_path: &str,
    item_name: &str,
    item_type: &str,
    item_path: &str,
    source: DependencySource,
    file: &str,
    line: Option<usize>,
    snippet: Option<String>,
    cfg_conditions: Vec<String>,
    triggered_by: Option<String>,
    alternatives: Vec<String>,
) {
    if !logger().is_enabled() {
        return;
    }

    let decision = DependencyDecision {
        decision_id: generate_decision_id(item_name, item_path, crate_name, triggered_by.as_deref()),
        item_name: item_name.to_string(),
        item_type: item_type.to_string(),
        item_path: item_path.to_string(),
        level: DependencyLevel::Type,
        source,
        context: DependencyContext {
            crate_name: crate_name.to_string(),
            module_path: module_path.to_string(),
            file: file.to_string(),
            line,
            snippet,
            cfg_conditions,
            is_test: false,
            platform_specific: None,
        },
        alternatives,
        triggered_by,
        timestamp: get_timestamp(),
    };

    logger().log_decision(decision);
}

/// Module inclusion/exclusion decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleDecision {
    pub crate_name: String,
    pub module_path: String,
    pub included: bool,
    pub reason: String,
    pub triggered_by: Option<String>,
    pub timestamp: String,
}

/// Cargo.toml dependency addition decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CargoDecision {
    pub crate_name: String,
    pub dep_name: String,
    pub dep_version: String,
    pub source: String, // "direct_usage", "transitive", "external_crate"
    pub timestamp: String,
}

/// Visibility validation decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisibilityDecision {
    pub crate_name: String,
    pub item_name: String,
    pub actual_visibility: String, // "pub", "pub(crate)", "pub(super)", "private"
    pub export_visibility: String, // "pub", "pub(crate)"
    pub allowed: bool,
    pub reason: String,
    pub timestamp: String,
}

/// Log a module inclusion/exclusion decision
pub fn log_module_decision(
    crate_name: &str,
    module_path: &str,
    included: bool,
    reason: &str,
    triggered_by: Option<String>,
) {
    if !logger().is_enabled() {
        return;
    }

    let decision = ModuleDecision {
        crate_name: crate_name.to_string(),
        module_path: module_path.to_string(),
        included,
        reason: reason.to_string(),
        triggered_by,
        timestamp: get_timestamp(),
    };

    // Write to module_decisions.jsonl
    if let Ok(json) = serde_json::to_string(&decision) {
        if let Some(log_dir) = std::env::var_os("HOME") {
            let log_path = std::path::PathBuf::from(log_dir)
                .join("precc-slicer")
                .join("dependency_analysis")
                .join("module_decisions.jsonl");

            if let Some(parent) = log_path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }

            let _ = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&log_path)
                .and_then(|mut file| {
                    use std::io::Write;
                    writeln!(file, "{}", json)
                });
        }
    }
}

/// Log a Cargo.toml dependency addition
pub fn log_cargo_dependency(
    crate_name: &str,
    dep_name: &str,
    dep_version: &str,
    source: &str,
) {
    if !logger().is_enabled() {
        return;
    }

    let decision = CargoDecision {
        crate_name: crate_name.to_string(),
        dep_name: dep_name.to_string(),
        dep_version: dep_version.to_string(),
        source: source.to_string(),
        timestamp: get_timestamp(),
    };

    // Write to cargo_decisions.jsonl
    if let Ok(json) = serde_json::to_string(&decision) {
        if let Some(log_dir) = std::env::var_os("HOME") {
            let log_path = std::path::PathBuf::from(log_dir)
                .join("precc-slicer")
                .join("dependency_analysis")
                .join("cargo_decisions.jsonl");

            if let Some(parent) = log_path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }

            let _ = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&log_path)
                .and_then(|mut file| {
                    use std::io::Write;
                    writeln!(file, "{}", json)
                });
        }
    }
}

/// Log a visibility validation decision
pub fn log_visibility_decision(
    crate_name: &str,
    item_name: &str,
    actual_visibility: &str,
    export_visibility: &str,
    allowed: bool,
    reason: &str,
) {
    if !logger().is_enabled() {
        return;
    }

    let decision = VisibilityDecision {
        crate_name: crate_name.to_string(),
        item_name: item_name.to_string(),
        actual_visibility: actual_visibility.to_string(),
        export_visibility: export_visibility.to_string(),
        allowed,
        reason: reason.to_string(),
        timestamp: get_timestamp(),
    };

    // Write to visibility_decisions.jsonl
    if let Ok(json) = serde_json::to_string(&decision) {
        if let Some(log_dir) = std::env::var_os("HOME") {
            let log_path = std::path::PathBuf::from(log_dir)
                .join("precc-slicer")
                .join("dependency_analysis")
                .join("visibility_decisions.jsonl");

            if let Some(parent) = log_path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }

            let _ = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&log_path)
                .and_then(|mut file| {
                    use std::io::Write;
                    writeln!(file, "{}", json)
                });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decision_id_generation() {
        let id1 = generate_decision_id("Foo", "crate::foo::Foo", "test_crate", None);
        let id2 = generate_decision_id("Foo", "crate::foo::Foo", "test_crate", None);
        let id3 = generate_decision_id("Foo", "crate::foo::Foo", "test_crate", Some("Bar"));

        assert_eq!(id1, id2, "Same inputs should produce same ID");
        assert_ne!(id1, id3, "Different triggered_by should produce different ID");
    }

    #[test]
    fn test_logger_enabled() {
        std::env::set_var("CARGO_SLICER_LOG_DEPENDENCIES", "1");
        let logger = DependencyDecisionLogger::new();
        assert!(logger.is_enabled());

        std::env::remove_var("CARGO_SLICER_LOG_DEPENDENCIES");
    }
}

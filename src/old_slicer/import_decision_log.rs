//! Import Decision Logging System
//!
//! Logs all import decisions made during code generation to enable
//! data-driven analysis and systematic rule induction.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write as IoWrite};
use std::path::Path;
use std::sync::Mutex;

/// A single import decision made during code generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportDecision {
    /// Symbol being imported (e.g., "Union", "AtomicUsize")
    pub symbol: String,

    /// Import path chosen (e.g., "std::collections::hash_set::Union")
    pub import_path: String,

    /// Source of the decision
    pub source: DecisionSource,

    /// Context where the symbol is used
    pub usage_context: UsageContext,

    /// Alternative paths that were considered but not chosen
    pub alternatives: Vec<String>,

    /// Unique ID for this decision
    pub decision_id: String,
}

/// Source that made the import decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DecisionSource {
    /// From SCIP analysis
    Scip { symbol_path: String },
    /// From std_types.json cache
    Cache,
    /// From crate index
    CrateIndex { item_path: String },
    /// From preserved imports
    Preserved,
    /// Pattern-based detection in code
    PatternDetection,
    /// Fallback/heuristic
    Heuristic { reason: String },
}

/// Context where the symbol appears in code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UsageContext {
    /// Crate being sliced
    pub crate_name: String,
    /// Module path (e.g., "crate::ast::parse")
    pub module_path: String,
    /// File where used
    pub file: String,
    /// Approximate line number (if known)
    pub line: Option<usize>,
    /// Surrounding code snippet (for context)
    pub snippet: Option<String>,
    /// cfg conditions active in this context
    pub cfg_conditions: Vec<String>,
    /// Whether this is in a qualified path (e.g., "std::fmt::Debug")
    pub is_qualified: bool,
    /// Whether symbol is defined locally in the crate
    pub has_local_definition: bool,
}

/// Result of using an import (determined by compilation)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportResult {
    /// Decision ID this result is for
    pub decision_id: String,
    /// Whether the import compiled successfully
    pub success: bool,
    /// Error code if failed (e.g., "E0432")
    pub error_code: Option<String>,
    /// Error message if failed
    pub error_message: Option<String>,
    /// File and line where error occurred
    pub error_location: Option<(String, usize)>,
}

/// Complete decision table: decisions + results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportDecisionTable {
    pub decisions: Vec<ImportDecision>,
    pub results: HashMap<String, ImportResult>,
}

/// Thread-safe logger for import decisions
pub struct ImportDecisionLogger {
    decisions: Mutex<Vec<ImportDecision>>,
    log_file: Mutex<Option<BufWriter<File>>>,
}

impl ImportDecisionLogger {
    /// Create a new logger
    pub fn new() -> Self {
        Self {
            decisions: Mutex::new(Vec::new()),
            log_file: Mutex::new(None),
        }
    }

    /// Initialize logging to a file
    pub fn init_file(&self, path: &Path) -> std::io::Result<()> {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)?;
        let writer = BufWriter::new(file);
        *self.log_file.lock().unwrap() = Some(writer);
        Ok(())
    }

    /// Check if logging is enabled
    pub fn is_enabled(&self) -> bool {
        // Check if log file is initialized or if environment variable is set
        self.log_file.lock().unwrap().is_some() ||
            std::env::var("CARGO_SLICER_LOG_IMPORTS").is_ok()
    }

    /// Log an import decision
    pub fn log_decision(&self, decision: ImportDecision) {
        let mut decisions = self.decisions.lock().unwrap();

        // Write to file if initialized
        if let Some(writer) = &mut *self.log_file.lock().unwrap() {
            if let Ok(json) = serde_json::to_string(&decision) {
                let _ = writeln!(writer, "{}", json);
                let _ = writer.flush();
            }
        }

        decisions.push(decision);
    }

    /// Get all logged decisions
    pub fn get_decisions(&self) -> Vec<ImportDecision> {
        self.decisions.lock().unwrap().clone()
    }

    /// Save decisions to JSON file
    pub fn save_to_json(&self, path: &Path) -> std::io::Result<()> {
        let decisions = self.get_decisions();
        let json = serde_json::to_string_pretty(&decisions)?;
        std::fs::write(path, json)
    }

    /// Clear all logged decisions
    pub fn clear(&self) {
        self.decisions.lock().unwrap().clear();
    }
}

/// Global logger instance
static LOGGER: once_cell::sync::Lazy<ImportDecisionLogger> =
    once_cell::sync::Lazy::new(|| {
        let logger = ImportDecisionLogger::new();

        // Initialize from environment variable if set
        if let Ok(log_path) = std::env::var("CARGO_SLICER_LOG_IMPORTS") {
            let path = std::path::Path::new(&log_path);
            let _ = logger.init_file(path);
        }

        logger
    });

/// Get the global logger
pub fn logger() -> &'static ImportDecisionLogger {
    &LOGGER
}

/// Generate a unique decision ID
pub fn generate_decision_id(symbol: &str, module: &str, crate_name: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    symbol.hash(&mut hasher);
    module.hash(&mut hasher);
    crate_name.hash(&mut hasher);
    let hash = hasher.finish();

    format!("{}:{}:{:016x}", crate_name, symbol, hash)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decision_logging() {
        let logger = ImportDecisionLogger::new();

        let decision = ImportDecision {
            symbol: "Union".to_string(),
            import_path: "std::collections::hash_set::Union".to_string(),
            source: DecisionSource::Cache,
            usage_context: UsageContext {
                crate_name: "regex-syntax".to_string(),
                module_path: "ast::parse".to_string(),
                file: "src/ast/parse.rs".to_string(),
                line: Some(334),
                snippet: Some("union: ast::ClassSetUnion".to_string()),
                cfg_conditions: vec![],
                is_qualified: false,
                has_local_definition: false,
            },
            alternatives: vec!["std::collections::Union".to_string()],
            decision_id: "regex-syntax:Union:abc123".to_string(),
        };

        logger.log_decision(decision.clone());

        let decisions = logger.get_decisions();
        assert_eq!(decisions.len(), 1);
        assert_eq!(decisions[0].symbol, "Union");
    }
}

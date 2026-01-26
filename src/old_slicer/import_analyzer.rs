//! Import Decision Analysis and Rule Induction
//!
//! Analyzes import decisions and compilation results to learn patterns
//! and induce systematic rules for correct import resolution.

use crate::old_slicer::import_decision_log::{ImportDecision, ImportDecisionTable};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// Pattern learned from successful/failed imports
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportPattern {
    /// Symbol or symbol pattern (e.g., "Union", "*Difference", "Atomic*")
    pub symbol_pattern: String,

    /// Conditions that must be true for this pattern to apply
    pub conditions: Vec<Condition>,

    /// Import path template to use when conditions match
    pub import_path_template: String,

    /// Confidence score (success_rate)
    pub confidence: f64,

    /// Number of times this pattern was seen
    pub sample_count: usize,
}

/// Condition that affects import resolution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Condition {
    /// Symbol name matches pattern
    SymbolMatches(String),

    /// Module path matches pattern
    ModulePathMatches(String),

    /// Crate name matches
    CrateNameMatches(String),

    /// cfg condition present
    CfgCondition(String),

    /// Symbol NOT qualified (appears standalone)
    IsUnqualified,

    /// Symbol IS qualified (appears as path::Type)
    IsQualified,

    /// Has local definition in crate
    HasLocalDefinition,

    /// Does NOT have local definition
    NoLocalDefinition,

    /// Source of decision
    DecisionSource(String),
}

/// Statistics about import decisions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportStatistics {
    pub total_decisions: usize,
    pub successful_imports: usize,
    pub failed_imports: usize,
    pub pending_results: usize,
    pub success_rate: f64,

    /// Breakdown by symbol
    pub by_symbol: HashMap<String, SymbolStats>,

    /// Breakdown by import path
    pub by_path: HashMap<String, PathStats>,

    /// Breakdown by error code
    pub by_error: HashMap<String, usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolStats {
    pub symbol: String,
    pub total: usize,
    pub successful: usize,
    pub failed: usize,
    pub success_rate: f64,
    pub common_paths: Vec<(String, usize)>, // (path, count)
    pub common_errors: Vec<(String, usize)>, // (error_code, count)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathStats {
    pub path: String,
    pub total: usize,
    pub successful: usize,
    pub failed: usize,
    pub success_rate: f64,
    pub common_symbols: Vec<(String, usize)>, // (symbol, count)
}

/// Analyze import decisions and induce patterns
pub fn analyze_decisions(table: &ImportDecisionTable) -> (ImportStatistics, Vec<ImportPattern>) {
    let stats = compute_statistics(table);
    let patterns = induce_patterns(table);

    (stats, patterns)
}

/// Compute statistics from decision table
fn compute_statistics(table: &ImportDecisionTable) -> ImportStatistics {
    let total_decisions = table.decisions.len();
    let mut successful = 0;
    let mut failed = 0;
    let pending = total_decisions - table.results.len();

    let mut by_symbol: HashMap<String, SymbolStats> = HashMap::new();
    let mut by_path: HashMap<String, PathStats> = HashMap::new();
    let mut by_error: HashMap<String, usize> = HashMap::new();

    for decision in &table.decisions {
        // Update by_symbol
        let symbol_entry = by_symbol.entry(decision.symbol.clone()).or_insert(SymbolStats {
            symbol: decision.symbol.clone(),
            total: 0,
            successful: 0,
            failed: 0,
            success_rate: 0.0,
            common_paths: vec![],
            common_errors: vec![],
        });
        symbol_entry.total += 1;

        // Update by_path
        let path_entry = by_path.entry(decision.import_path.clone()).or_insert(PathStats {
            path: decision.import_path.clone(),
            total: 0,
            successful: 0,
            failed: 0,
            success_rate: 0.0,
            common_symbols: vec![],
        });
        path_entry.total += 1;

        // Check result
        if let Some(result) = table.results.get(&decision.decision_id) {
            if result.success {
                successful += 1;
                symbol_entry.successful += 1;
                path_entry.successful += 1;
            } else {
                failed += 1;
                symbol_entry.failed += 1;
                path_entry.failed += 1;

                if let Some(err_code) = &result.error_code {
                    *by_error.entry(err_code.clone()).or_insert(0) += 1;
                }
            }
        }
    }

    // Calculate rates
    for stats in by_symbol.values_mut() {
        if stats.total > 0 {
            stats.success_rate = stats.successful as f64 / stats.total as f64;
        }
    }

    for stats in by_path.values_mut() {
        if stats.total > 0 {
            stats.success_rate = stats.successful as f64 / stats.total as f64;
        }
    }

    let success_rate = if total_decisions > 0 {
        successful as f64 / total_decisions as f64
    } else {
        0.0
    };

    ImportStatistics {
        total_decisions,
        successful_imports: successful,
        failed_imports: failed,
        pending_results: pending,
        success_rate,
        by_symbol,
        by_path,
        by_error,
    }
}

/// Induce patterns from successful decisions
fn induce_patterns(table: &ImportDecisionTable) -> Vec<ImportPattern> {
    let mut patterns = Vec::new();

    // Group decisions by symbol
    let mut by_symbol: HashMap<String, Vec<&ImportDecision>> = HashMap::new();
    for decision in &table.decisions {
        by_symbol.entry(decision.symbol.clone())
            .or_insert_with(Vec::new)
            .push(decision);
    }

    // For each symbol, find the best path
    for (symbol, decisions) in by_symbol {
        let mut path_results: HashMap<String, (usize, usize)> = HashMap::new(); // (success, total)

        for decision in &decisions {
            if let Some(result) = table.results.get(&decision.decision_id) {
                let entry = path_results.entry(decision.import_path.clone()).or_insert((0, 0));
                entry.1 += 1; // total
                if result.success {
                    entry.0 += 1; // success
                }
            }
        }

        // Find best path for this symbol
        if let Some((best_path, (success, total))) = path_results.iter()
            .max_by_key(|(_, (s, t))| (*s as f64 / *t as f64 * 1000.0) as i32)
        {
            if *total > 0 {
                let confidence = *success as f64 / *total as f64;

                // Extract common conditions
                let mut conditions = vec![Condition::SymbolMatches(symbol.clone())];

                // Check if symbol has consistent qualification requirement
                let qualified_count = decisions.iter()
                    .filter(|d| d.usage_context.is_qualified)
                    .count();

                if qualified_count == 0 {
                    conditions.push(Condition::IsUnqualified);
                } else if qualified_count == decisions.len() {
                    conditions.push(Condition::IsQualified);
                }

                patterns.push(ImportPattern {
                    symbol_pattern: symbol,
                    conditions,
                    import_path_template: best_path.clone(),
                    confidence,
                    sample_count: *total,
                });
            }
        }
    }

    // Sort by confidence and sample count
    patterns.sort_by(|a, b| {
        b.confidence.partial_cmp(&a.confidence).unwrap()
            .then(b.sample_count.cmp(&a.sample_count))
    });

    patterns
}

/// Load decision table from files
pub fn load_decision_table(decisions_path: &Path, results_path: &Path) -> std::io::Result<ImportDecisionTable> {
    let decisions_json = std::fs::read_to_string(decisions_path)?;
    let decisions: Vec<ImportDecision> = serde_json::from_str(&decisions_json)?;

    let results = if results_path.exists() {
        let results_json = std::fs::read_to_string(results_path)?;
        serde_json::from_str(&results_json)?
    } else {
        HashMap::new()
    };

    Ok(ImportDecisionTable { decisions, results })
}

/// Save analysis results
pub fn save_analysis(
    stats: &ImportStatistics,
    patterns: &Vec<ImportPattern>,
    output_dir: &Path,
) -> std::io::Result<()> {
    let stats_json = serde_json::to_string_pretty(stats)?;
    std::fs::write(output_dir.join("import_statistics.json"), stats_json)?;

    let patterns_json = serde_json::to_string_pretty(patterns)?;
    std::fs::write(output_dir.join("import_patterns.json"), patterns_json)?;

    // Generate human-readable report
    let mut report = String::new();
    report.push_str("# Import Decision Analysis Report\n\n");
    report.push_str(&format!("## Statistics\n\n"));
    report.push_str(&format!("- Total Decisions: {}\n", stats.total_decisions));
    report.push_str(&format!("- Successful: {} ({:.1}%)\n",
        stats.successful_imports,
        stats.success_rate * 100.0));
    report.push_str(&format!("- Failed: {} ({:.1}%)\n",
        stats.failed_imports,
        (stats.failed_imports as f64 / stats.total_decisions as f64) * 100.0));
    report.push_str(&format!("- Pending: {}\n\n", stats.pending_results));

    report.push_str("## Top Failing Symbols\n\n");
    let mut failing_symbols: Vec<_> = stats.by_symbol.values()
        .filter(|s| s.failed > 0)
        .collect();
    failing_symbols.sort_by_key(|s| std::cmp::Reverse(s.failed));

    for (i, stats) in failing_symbols.iter().take(20).enumerate() {
        report.push_str(&format!("{}. **{}**: {} failed / {} total ({:.1}% failure)\n",
            i + 1, stats.symbol, stats.failed, stats.total,
            (stats.failed as f64 / stats.total as f64) * 100.0));
    }

    report.push_str("\n## Induced Patterns (Top 20)\n\n");
    for (i, pattern) in patterns.iter().take(20).enumerate() {
        report.push_str(&format!("{}. **{}** → `{}`\n",
            i + 1, pattern.symbol_pattern, pattern.import_path_template));
        report.push_str(&format!("   - Confidence: {:.1}%\n", pattern.confidence * 100.0));
        report.push_str(&format!("   - Samples: {}\n", pattern.sample_count));
        report.push_str(&format!("   - Conditions: {:?}\n\n", pattern.conditions));
    }

    std::fs::write(output_dir.join("import_analysis_report.md"), report)?;

    Ok(())
}

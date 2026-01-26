use crate::cycle_detector::{Cycle, DependencyEdge, EdgeType};
use std::collections::HashSet;
use std::fs;
use std::path::Path;

#[derive(Debug, Clone)]
pub struct BreakingStrategy {
    pub cycle: Cycle,
    pub edges_to_remove: Vec<DependencyEdge>,  // Dev-deps to exclude
    pub edges_to_convert: Vec<DependencyEdge>, // Convert to version spec
    pub strategy_type: StrategyType,
}

#[derive(Debug, Clone)]
pub enum StrategyType {
    RemoveDevDeps { count: usize },
    UseVersionSpec { crate_name: String, original_version: String },
    RegistryFallback { crate_name: String },
}

pub struct CycleBreaker {
    cycles: Vec<Cycle>,
    sliced_crates_dir: String,
}

impl CycleBreaker {
    pub fn new(cycles: Vec<Cycle>, sliced_crates_dir: String) -> Self {
        CycleBreaker {
            cycles,
            sliced_crates_dir,
        }
    }

    /// Compute breaking strategies for all cycles
    pub fn compute_strategies(&self) -> Vec<BreakingStrategy> {
        let mut strategies = Vec::new();

        for cycle in &self.cycles {
            let strategy = self.break_single_cycle(cycle);
            strategies.push(strategy);
        }

        strategies
    }

    /// Compute strategy for a single cycle
    fn break_single_cycle(&self, cycle: &Cycle) -> BreakingStrategy {
        // Step 1: Try removing all dev-dep edges (safest)
        let dev_edges: Vec<_> = cycle.edges.iter()
            .filter(|e| e.edge_type == EdgeType::Dev)
            .cloned()
            .collect();

        if !dev_edges.is_empty() {
            return BreakingStrategy {
                cycle: cycle.clone(),
                edges_to_remove: dev_edges.clone(),
                edges_to_convert: vec![],
                strategy_type: StrategyType::RemoveDevDeps {
                    count: dev_edges.len()
                },
            };
        }

        // Step 2: Score all normal edges, pick lowest
        let mut scored: Vec<_> = cycle.edges.iter()
            .map(|e| (e, self.score_edge(e)))
            .collect();
        scored.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

        if let Some((best_edge, _score)) = scored.first() {
            // Get version from original dependency
            let original_version = self.get_original_version(&best_edge.to)
                .unwrap_or_else(|| "1.0".to_string());

            return BreakingStrategy {
                cycle: cycle.clone(),
                edges_to_remove: vec![],
                edges_to_convert: vec![(*best_edge).clone()],
                strategy_type: StrategyType::UseVersionSpec {
                    crate_name: best_edge.to.clone(),
                    original_version,
                },
            };
        }

        // Step 3: Last resort - registry fallback for smallest crate
        let smallest_crate = cycle.crates.first()
            .cloned()
            .unwrap_or_else(|| "unknown".to_string());

        BreakingStrategy {
            cycle: cycle.clone(),
            edges_to_remove: vec![],
            edges_to_convert: vec![],
            strategy_type: StrategyType::RegistryFallback {
                crate_name: smallest_crate,
            },
        }
    }

    /// Score an edge for conversion priority
    /// Lower score = more preferred for conversion
    fn score_edge(&self, edge: &DependencyEdge) -> f64 {
        let mut score = 0.0;

        // Prefer dev-deps over normal deps
        if edge.edge_type == EdgeType::Dev {
            score -= 100.0;
        }

        // Prefer proc-macro crates (they're usually small)
        if edge.to.contains("proc-macro") || edge.to.contains("derive") {
            score -= 50.0;
        }

        // Add a small random component to break ties
        score += (edge.to.len() % 10) as f64;

        score
    }

    /// Get original version from sliced crate's Cargo.toml
    fn get_original_version(&self, crate_name: &str) -> Option<String> {
        let manifest_path = Path::new(&self.sliced_crates_dir)
            .join(format!("{}-sliced", crate_name))
            .join("Cargo.toml");

        if let Ok(content) = fs::read_to_string(&manifest_path) {
            // Parse version from [package] section
            for line in content.lines() {
                if line.trim().starts_with("version") {
                    if let Some(version) = line.split('=').nth(1) {
                        let version = version.trim().trim_matches('"').to_string();
                        return Some(version);
                    }
                }
            }
        }

        None
    }

    /// Apply computed strategies to sliced crate manifests
    pub fn apply_strategies(&self, strategies: &[BreakingStrategy]) -> Result<(), String> {
        for strategy in strategies {
            match &strategy.strategy_type {
                StrategyType::RemoveDevDeps { .. } => {
                    // Remove dev-dependencies from affected crates
                    for edge in &strategy.edges_to_remove {
                        self.remove_dev_dependency(&edge.from)?;
                    }
                }
                StrategyType::UseVersionSpec { crate_name, original_version } => {
                    // Convert path dependencies to version specs
                    self.convert_to_version_spec(crate_name, original_version)?;
                }
                StrategyType::RegistryFallback { crate_name } => {
                    println!("    Strategy: Registry fallback for {}", crate_name);
                }
            }
        }

        Ok(())
    }

    /// Remove dev-dependencies section from a crate's Cargo.toml
    fn remove_dev_dependency(&self, crate_name: &str) -> Result<(), String> {
        let manifest_path = Path::new(&self.sliced_crates_dir)
            .join(format!("{}-sliced", crate_name))
            .join("Cargo.toml");

        if !manifest_path.exists() {
            return Ok(()); // Silently skip if doesn't exist
        }

        let content = fs::read_to_string(&manifest_path)
            .map_err(|e| format!("Failed to read {}: {}", manifest_path.display(), e))?;

        // Remove [dev-dependencies] section
        let mut new_content = String::new();
        let mut in_dev_deps = false;
        let mut skip_section = false;

        for line in content.lines() {
            let trimmed = line.trim();

            // Check if entering [dev-dependencies] section
            if trimmed == "[dev-dependencies]" {
                in_dev_deps = true;
                skip_section = true;
                continue;
            }

            // Check if entering a new section
            if trimmed.starts_with('[') && in_dev_deps {
                in_dev_deps = false;
                skip_section = false;
            }

            // Skip lines in [dev-dependencies] section
            if skip_section && (trimmed.is_empty() || !trimmed.starts_with('[')) {
                continue;
            }

            new_content.push_str(line);
            new_content.push('\n');
        }

        // Write back
        fs::write(&manifest_path, new_content)
            .map_err(|e| format!("Failed to write {}: {}", manifest_path.display(), e))?;

        Ok(())
    }

    /// Convert path dependencies to version specs in sliced crate manifests
    /// This breaks circular path dependencies by falling back to registry versions
    fn convert_to_version_spec(&self, crate_name: &str, version: &str) -> Result<(), String> {
        let sliced_dir = Path::new(&self.sliced_crates_dir);

        if !sliced_dir.exists() {
            return Err(format!("Sliced crates directory not found: {}", sliced_dir.display()));
        }

        // Iterate through all sliced crate directories
        let entries = fs::read_dir(sliced_dir)
            .map_err(|e| format!("Failed to read directory {}: {}", sliced_dir.display(), e))?;

        for entry in entries {
            let entry = entry.map_err(|e| e.to_string())?;
            let path = entry.path();

            if !path.is_dir() {
                continue;
            }

            let manifest_path = path.join("Cargo.toml");
            if !manifest_path.exists() {
                continue;
            }

            // Read the manifest
            let content = fs::read_to_string(&manifest_path)
                .map_err(|e| format!("Failed to read {}: {}", manifest_path.display(), e))?;

            let mut modified = false;
            let mut new_content = String::new();
            let mut in_dependencies = false;
            let mut in_dev_dependencies = false;

            for line in content.lines() {
                let trimmed = line.trim();

                // Track which section we're in
                if trimmed == "[dependencies]" {
                    in_dependencies = true;
                    in_dev_dependencies = false;
                } else if trimmed == "[dev-dependencies]" {
                    in_dependencies = false;
                    in_dev_dependencies = true;
                } else if trimmed.starts_with('[') && trimmed.ends_with(']') {
                    in_dependencies = false;
                    in_dev_dependencies = false;
                }

                // Check if this line is a path dependency for our target crate
                // Handle both crate-name and crate_name (with underscores)
                let normalized_crate = crate_name.replace('_', "-");
                let is_target_dep = (trimmed.starts_with(&format!("{} =", crate_name)) ||
                                     trimmed.starts_with(&format!("{} =", normalized_crate))) &&
                                    trimmed.contains("{ path =");

                if (in_dependencies || in_dev_dependencies) && is_target_dep {
                    // Extract features if present
                    let features = if let Some(features_start) = trimmed.find("features = [") {
                        if let Some(features_end) = trimmed[features_start..].find(']') {
                            Some(trimmed[features_start..features_start + features_end + 1].to_string())
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    // Replace with version spec
                    let indent = line.len() - line.trim_start().len();
                    let spaces = " ".repeat(indent);

                    let new_line = if let Some(features) = features {
                        format!("{}{} = {{ version = \"{}\", {} }}", spaces, crate_name, version, features)
                    } else {
                        format!("{}{} = \"{}\"", spaces, crate_name, version)
                    };

                    new_content.push_str(&new_line);
                    new_content.push('\n');
                    modified = true;
                } else {
                    new_content.push_str(line);
                    new_content.push('\n');
                }
            }

            // Write back if modified
            if modified {
                fs::write(&manifest_path, new_content)
                    .map_err(|e| format!("Failed to write {}: {}", manifest_path.display(), e))?;
            }
        }

        Ok(())
    }

    /// Collect crates that need registry fallback
    pub fn get_registry_fallback_crates(&self, strategies: &[BreakingStrategy]) -> HashSet<String> {
        let mut fallback_crates = HashSet::new();

        for strategy in strategies {
            if let StrategyType::RegistryFallback { crate_name } = &strategy.strategy_type {
                fallback_crates.insert(crate_name.clone());
            }
            if let StrategyType::UseVersionSpec { crate_name, .. } = &strategy.strategy_type {
                fallback_crates.insert(crate_name.clone());
            }
        }

        fallback_crates
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_score_edge_dev_dep() {
        let breaker = CycleBreaker::new(vec![], "test".to_string());

        let edge_dev = DependencyEdge {
            from: "A".to_string(),
            to: "B".to_string(),
            edge_type: EdgeType::Dev,
        };

        let edge_normal = DependencyEdge {
            from: "A".to_string(),
            to: "B".to_string(),
            edge_type: EdgeType::Normal,
        };

        let score_dev = breaker.score_edge(&edge_dev);
        let score_normal = breaker.score_edge(&edge_normal);

        // Dev-dep should have lower (more negative) score
        assert!(score_dev < score_normal);
    }

    #[test]
    fn test_break_cycle_prefers_dev_deps() {
        let cycle = Cycle {
            crates: vec!["A".to_string(), "B".to_string()],
            edges: vec![
                DependencyEdge {
                    from: "A".to_string(),
                    to: "B".to_string(),
                    edge_type: EdgeType::Dev,
                },
                DependencyEdge {
                    from: "B".to_string(),
                    to: "A".to_string(),
                    edge_type: EdgeType::Normal,
                },
            ],
        };

        let breaker = CycleBreaker::new(vec![cycle.clone()], "test".to_string());
        let strategy = breaker.break_single_cycle(&cycle);

        // Should prefer removing dev-deps
        match strategy.strategy_type {
            StrategyType::RemoveDevDeps { count } => {
                assert_eq!(count, 1);
            }
            _ => panic!("Expected RemoveDevDeps strategy"),
        }
    }
}

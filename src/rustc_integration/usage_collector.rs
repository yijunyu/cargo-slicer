//! Main usage collection orchestrator
//!
//! This module coordinates the collection of usage information from rustc.

#![cfg(feature = "rustc-driver")]

use super::UsageData;
use std::path::Path;

/// Collection strategy - determines which compiler phase to use
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionStrategy {
    /// Parse only - fastest but may miss macro-generated code
    AfterParsing,
    /// After macro expansion - good balance of speed and completeness
    AfterExpansion,
    /// After type checking - slowest but most accurate
    AfterAnalysis,
}

/// Configuration for usage collection
#[derive(Debug, Clone)]
pub struct CollectionConfig {
    /// Which compilation phase to collect at
    pub strategy: CollectionStrategy,
    /// Output path for collected data
    pub output_path: std::path::PathBuf,
    /// Whether to include private items
    pub include_private: bool,
    /// Whether to collect detailed location information
    pub detailed_locations: bool,
}

impl Default for CollectionConfig {
    fn default() -> Self {
        Self {
            strategy: CollectionStrategy::AfterExpansion,
            output_path: std::path::PathBuf::from(".cargo-slicer-data.json"),
            include_private: true,
            detailed_locations: false,
        }
    }
}

/// Collect usage data from a crate
pub fn collect_usage(
    crate_name: &str,
    _crate_path: &Path,
    _config: &CollectionConfig,
) -> Result<UsageData, String> {
    let usage_data = UsageData::new(crate_name.to_string());

    // TODO: Implement actual collection
    // This would:
    // 1. Invoke rustc with our custom driver
    // 2. Collect data via callbacks
    // 3. Return the collected data

    Ok(usage_data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collection_config_default() {
        let config = CollectionConfig::default();
        assert_eq!(config.strategy, CollectionStrategy::AfterExpansion);
        assert!(config.include_private);
        assert!(!config.detailed_locations);
    }

    #[test]
    fn test_collect_usage_placeholder() {
        let config = CollectionConfig::default();
        let result = collect_usage("test_crate", Path::new("."), &config);
        assert!(result.is_ok());
    }
}

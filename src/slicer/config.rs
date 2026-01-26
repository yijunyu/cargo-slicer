//! Configuration for slicer behavior and rules

use serde::{Deserialize, Serialize};
use super::features::SlicerFeatures;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SlicerConfig {
    /// Enable/disable specific rules
    pub rules: RuleConfig,

    /// Output folder pattern (e.g., "{crate_name}_sliced")
    pub output_pattern: String,

    /// Verbose output
    pub verbose: bool,

    /// Feature configuration (optimization levels, deletion flags)
    #[serde(skip)]
    pub features: SlicerFeatures,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleConfig {
    /// Keep all methods in impl blocks if type is used
    pub keep_full_impl_blocks: bool,

    /// Keep all trait definitions if implemented by used type
    pub keep_trait_definitions: bool,

    /// Keep type dependencies (field types, return types)
    pub keep_type_dependencies: bool,

    /// Keep generic bounds
    pub keep_generic_bounds: bool,

    /// Keep macro definitions if macro is used
    pub keep_macro_definitions: bool,
}

impl Default for SlicerConfig {
    fn default() -> Self {
        Self {
            rules: RuleConfig::default(),
            output_pattern: "{crate_name}_sliced".to_string(),
            verbose: false,
            features: SlicerFeatures::default(),
        }
    }
}

impl Default for RuleConfig {
    fn default() -> Self {
        Self {
            keep_full_impl_blocks: true,  // Conservative: keep all methods
            keep_trait_definitions: true,  // Needed for trait bounds
            keep_type_dependencies: true,  // Needed for type checking
            keep_generic_bounds: true,     // Part of API contract
            keep_macro_definitions: true,  // Macros expand to unknown code
        }
    }
}

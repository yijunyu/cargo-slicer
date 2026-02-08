//! Feature configuration for the slicer
//!
//! This module provides a unified way to configure slicer behavior through
//! feature flags (like GCC -f flags) and optimization levels (like GCC -O).

use std::collections::HashSet;
use std::path::Path;

/// Slicer feature configuration
///
/// Controls which optimizations and analyses are enabled during slicing.
/// Features can be set individually via `-f<feature>` flags or as presets
/// via `-O<level>` optimization levels.
#[derive(Debug, Clone, Default)]
pub struct SlicerFeatures {
    // === Deletion Features ===
    /// Delete unused private functions (-fprivate-fn)
    pub delete_private_fn: bool,
    /// Delete unused private constants and statics (-fprivate-const)
    pub delete_private_const: bool,
    /// Delete unused private type aliases (-fprivate-type)
    pub delete_private_type: bool,
    /// Delete unused private structs and enums (-fprivate-struct)
    pub delete_private_struct: bool,
    /// Trust the dependency graph for all items, not just private (-ftrust-graph)
    pub trust_graph: bool,
    /// Use trial-based deletion (slower but accurate) (-ftrial-delete)
    pub trial_delete: bool,
    /// Use dependency-aware removal sets for trial deletion (-ftrial-sets)
    /// This finds more deletable code by removing dependent items together
    pub trial_sets: bool,
    /// Use graph-guided deletion with JSON verification (-fgraph-guided)
    /// This is the fastest mode: bulk delete items without dependents, then correct
    /// with cargo check. Usually converges in 2-3 iterations vs N for trial-per-item.
    pub graph_guided: bool,
    /// Maximum items to trial per crate (-ftrial-limit=N)
    pub trial_limit: usize,

    // === Analysis Features (from old slicer) ===
    /// Enable cycle detection and breaking using Tarjan SCC (-fcycle-breaking)
    pub cycle_breaking: bool,
    /// Evaluate #[cfg(...)] expressions for target platform (-fcfg-eval)
    pub cfg_eval: bool,
    /// Filter architecture-specific code (-farch-filter)
    pub arch_filter: bool,
    /// Apply Cargo.toml compatibility fixes (-fauto-fixes)
    pub auto_fixes: bool,

    // === Dependency Analysis Features ===
    /// Track cfg(feature = "X") gates and filter unused optional deps (-fcfg-feature-deps)
    pub cfg_feature_deps: bool,

    // === Marking Rule Features ===
    /// Keep full impl blocks if type is used (-fmark-impl-blocks)
    pub mark_impl_blocks: bool,
    /// Include trait implementations (-fmark-trait-deps)
    pub mark_trait_deps: bool,
    /// Include types used in signatures (-fmark-type-deps)
    pub mark_type_deps: bool,
    /// Include trait bounds on generics (-fmark-generic-bounds)
    pub mark_generic_bounds: bool,

    // === Verification Features ===
    /// Run cargo check after deletion to verify correctness (-fverify)
    pub verify: bool,
    /// Detailed timing analysis per phase (-fprofiling)
    pub profiling: bool,

    // === Optimization Features ===
    /// Use incremental graph building to skip parsing dead code (-fincremental-graph)
    /// This builds the dependency graph incrementally via BFS from entry points,
    /// avoiding the need to parse files containing only dead code.
    /// Expected speedup: 2.5-5x faster graph building (parse 20-40% of files instead of 100%)
    pub incremental_graph: bool,

    // === Parser Selection Features ===
    /// Use ctags for parsing instead of syn (-fuse-ctags)
    /// Expected speedup: 4-8x faster parsing (text-based vs AST)
    pub use_ctags: bool,

    /// Use fast tokenizer for parsing instead of syn (-fuse-fast-tokenizer)
    /// Expected speedup: 10-50x faster parsing (single-pass lightweight tokenizer)
    pub use_fast_tokenizer: bool,

    /// Use hybrid mode: ctags for fast path, syn for validation (-fctags-hybrid)
    /// This uses ctags for parsing but falls back to syn for complex dependency analysis
    pub ctags_hybrid: bool,

    /// Use rustc driver for parsing and reference tracking (-fuse-rustc-driver)
    /// Uses actual compiler for 100% accurate parsing and dependency tracking.
    /// Expected accuracy: ~95% vs ~70% for ctags. Slower than ctags but faster than rust-analyzer.
    /// Requires nightly Rust and rustc-driver feature.
    pub use_rustc_driver: bool,

    /// Pre-compile dependencies before rustc driver parsing (-frustc-precompile)
    /// Runs `cargo build` before parsing to ensure all dependencies are compiled.
    /// Eliminates fallback to syn parser but adds compilation time.
    /// Only effective when use_rustc_driver is also enabled.
    pub rustc_precompile: bool,

    /// Use cargo rustc instead of direct rustc (-frustc-via-cargo)
    /// Invokes rustc driver through `cargo rustc` wrapper.
    /// Provides 100% accurate compilation environment by letting cargo handle:
    /// - Dependency resolution (eliminates sysroot conflicts)
    /// - Edition handling (correct prelude imports)
    /// - Build scripts (OUT_DIR, etc.)
    /// - Feature flags
    /// This is the recommended approach for maximum accuracy.
    pub rustc_via_cargo: bool,

    /// Skip formatting after text-based slicing (-fskip-format)
    /// Text-based slicing doesn't preserve rustfmt, this skips reformatting
    pub skip_format: bool,

    // === Blocked Crate Overrides ===
    /// Force slice normally-blocked crates (-fslice-<crate>)
    pub slice_blocked: HashSet<String>,
}

impl SlicerFeatures {
    /// Create a new SlicerFeatures with optimized defaults
    pub fn new() -> Self {
        // Auto-detect if rustc driver binary is available
        let has_rustc_driver = Self::detect_rustc_driver_binary();

        Self {
            trial_limit: 50,  // Default: test up to 50 items per crate
            use_rustc_driver: has_rustc_driver,  // Smart default: enable only if binary exists
            rustc_via_cargo: has_rustc_driver,   // Smart default: enable only if binary exists
            ..Default::default()
        }
    }

    /// Detect if cargo-slicer-rustc binary is available
    ///
    /// Checks the same locations as rustc_subprocess::find_rustc_driver_binary()
    /// to ensure consistency. Returns true if the binary exists.
    fn detect_rustc_driver_binary() -> bool {
        use std::path::PathBuf;

        // Check in target/debug (dev build)
        if PathBuf::from("target/debug/cargo-slicer-rustc").exists() {
            return true;
        }

        // Check in target/release (release build)
        if PathBuf::from("target/release/cargo-slicer-rustc").exists() {
            return true;
        }

        // Check in PATH (installed via cargo install)
        if let Ok(output) = std::process::Command::new("which")
            .arg("cargo-slicer-rustc")
            .output()
        {
            if output.status.success() && !output.stdout.is_empty() {
                return true;
            }
        }

        false
    }

    /// Create fast production mode features (-O flag)
    ///
    /// Deletes private items WITHOUT trial verification for speed.
    /// Skips Phase 6 compilation check (final build catches errors).
    /// Use this for production; use -O2 for development to find minimal.
    pub fn fast() -> Self {
        let mut features = Self::new();
        // Enable all deletion features
        features.delete_private_fn = true;
        features.delete_private_const = true;
        features.delete_private_type = true;
        features.delete_private_struct = true;
        // Enable analysis features
        features.cfg_eval = true;
        features.arch_filter = true;
        features.auto_fixes = true;
        features.cycle_breaking = true;
        // Enable marking rules
        features.mark_impl_blocks = true;
        features.mark_trait_deps = true;
        features.mark_type_deps = true;
        features.mark_generic_bounds = true;
        // NO trial_delete - that's the key difference for speed
        // NO verify - final build will catch errors
        features
    }

    /// Create features from an optimization level (0-4)
    ///
    /// Development modes with trial verification for finding minimal code.
    /// - O0: Conservative (no deletion, current behavior)
    /// - O1: Private function deletion (highest impact/risk ratio)
    /// - O2: All private item deletion + trial verification
    /// - O3: + Slice selected blocked crates
    /// - O4: Full aggressive mode (trust graph)
    pub fn from_level(level: u8) -> Self {
        let mut features = Self::new();

        match level {
            0 => {
                // O0: Conservative - no deletion features, just safe defaults
            }
            1 => {
                // O1: Private function deletion (highest impact/risk ratio)
                features.delete_private_fn = true;
                features.cfg_eval = true;
                features.arch_filter = true;
                features.auto_fixes = true;
                features.verify = true;
                // Enable conservative marking rules
                features.mark_impl_blocks = true;
                features.mark_trait_deps = true;
                features.mark_type_deps = true;
                features.mark_generic_bounds = true;
            }
            2 => {
                // O2: All private item deletion + dependency-aware trial deletion + graph-guided
                features = Self::from_level(1);
                features.delete_private_const = true;
                features.delete_private_type = true;
                features.cycle_breaking = true;
                features.trial_delete = true;
                features.trial_sets = true;  // Use dependency-aware removal sets
                features.graph_guided = true;  // Use optimized graph-guided deletion (moved from O3)
                features.incremental_graph = true;  // Skip parsing dead code
            }
            3 => {
                // O3: + Private struct/enum deletion + slice selected blocked crates
                // Note: We don't force-slice aho-corasick or other optional deps here
                // because the decision should be based on actual usage analysis,
                // not hardcoded overrides. Users can still use -fslice-<crate> if needed.
                features = Self::from_level(2);
                features.delete_private_struct = true;
                // graph_guided is already enabled from O2
                features.slice_blocked.insert("parking_lot".to_string());
            }
            4 | _ => {
                // O4: Full aggressive mode + cfg-aware dependency filtering
                features = Self::from_level(3);
                features.trust_graph = true;
                features.cfg_feature_deps = true;  // Filter unused optional deps via cfg tracking
                features.slice_blocked.insert("env_logger".to_string());
            }
        }

        features
    }

    /// Enable a feature by name
    ///
    /// Returns Ok(()) if the feature was recognized and enabled,
    /// or Err with the unknown feature name.
    pub fn enable(&mut self, feature: &str) -> Result<(), String> {
        match feature {
            // Deletion features
            "private-fn" => self.delete_private_fn = true,
            "private-const" => self.delete_private_const = true,
            "private-type" => self.delete_private_type = true,
            "private-struct" => self.delete_private_struct = true,
            "trust-graph" => self.trust_graph = true,
            "trial-delete" => self.trial_delete = true,
            "trial-sets" => self.trial_sets = true,
            "graph-guided" => self.graph_guided = true,
            s if s.starts_with("trial-limit=") => {
                if let Some(num_str) = s.strip_prefix("trial-limit=") {
                    if let Ok(n) = num_str.parse::<usize>() {
                        self.trial_limit = n;
                    }
                }
            }

            // Analysis features
            "cycle-breaking" => self.cycle_breaking = true,
            "cfg-eval" => self.cfg_eval = true,
            "arch-filter" => self.arch_filter = true,
            "auto-fixes" => self.auto_fixes = true,
            "cfg-feature-deps" => self.cfg_feature_deps = true,

            // Marking rule features
            "mark-impl-blocks" => self.mark_impl_blocks = true,
            "mark-trait-deps" => self.mark_trait_deps = true,
            "mark-type-deps" => self.mark_type_deps = true,
            "mark-generic-bounds" => self.mark_generic_bounds = true,

            // Verification features
            "verify" => self.verify = true,
            "profiling" => self.profiling = true,

            // Optimization features
            "incremental-graph" => self.incremental_graph = true,

            // Parser selection features
            "use-ctags" => self.use_ctags = true,
            "use-fast-tokenizer" => self.use_fast_tokenizer = true,
            "ctags-hybrid" => self.ctags_hybrid = true,
            "use-rustc-driver" => self.use_rustc_driver = true,
            "rustc-precompile" => self.rustc_precompile = true,
            "rustc-via-cargo" => self.rustc_via_cargo = true,
            "skip-format" => self.skip_format = true,

            // Blocked crate overrides (slice-<crate>)
            s if s.starts_with("slice-") => {
                let crate_name = s.strip_prefix("slice-").unwrap();
                self.slice_blocked.insert(crate_name.to_string());
            }

            _ => return Err(format!("Unknown feature: {}", feature)),
        }
        Ok(())
    }

    /// Disable a feature by name (for -fno-<feature> flags)
    ///
    /// Returns Ok(()) if the feature was recognized and disabled,
    /// or Err with the unknown feature name.
    pub fn disable(&mut self, feature: &str) -> Result<(), String> {
        match feature {
            // Deletion features
            "private-fn" => self.delete_private_fn = false,
            "private-const" => self.delete_private_const = false,
            "private-type" => self.delete_private_type = false,
            "private-struct" => self.delete_private_struct = false,
            "trust-graph" => self.trust_graph = false,
            "trial-delete" => self.trial_delete = false,
            "trial-sets" => self.trial_sets = false,
            "graph-guided" => self.graph_guided = false,

            // Analysis features
            "cycle-breaking" => self.cycle_breaking = false,
            "cfg-eval" => self.cfg_eval = false,
            "arch-filter" => self.arch_filter = false,
            "auto-fixes" => self.auto_fixes = false,
            "cfg-feature-deps" => self.cfg_feature_deps = false,

            // Marking rule features
            "mark-impl-blocks" => self.mark_impl_blocks = false,
            "mark-trait-deps" => self.mark_trait_deps = false,
            "mark-type-deps" => self.mark_type_deps = false,
            "mark-generic-bounds" => self.mark_generic_bounds = false,

            // Verification features
            "verify" => self.verify = false,
            "profiling" => self.profiling = false,

            // Optimization features
            "incremental-graph" => self.incremental_graph = false,

            // Parser selection features
            "use-ctags" => self.use_ctags = false,
            "use-fast-tokenizer" => self.use_fast_tokenizer = false,
            "ctags-hybrid" => self.ctags_hybrid = false,
            "use-rustc-driver" => self.use_rustc_driver = false,
            "rustc-precompile" => self.rustc_precompile = false,
            "rustc-via-cargo" => self.rustc_via_cargo = false,
            "skip-format" => self.skip_format = false,

            // Blocked crate overrides
            s if s.starts_with("slice-") => {
                let crate_name = s.strip_prefix("slice-").unwrap();
                self.slice_blocked.remove(crate_name);
            }

            _ => return Err(format!("Unknown feature: {}", feature)),
        }
        Ok(())
    }

    /// Load features from a TOML config file
    ///
    /// Config format:
    /// ```toml
    /// [features]
    /// delete_private_fn = true
    /// verify = true
    /// slice_blocked = ["parking_lot", "aho-corasick"]
    /// ```
    pub fn load_config(path: &Path) -> Result<Self, String> {
        use std::fs;

        let content = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read config file: {}", e))?;

        Self::parse_toml(&content)
    }

    /// Parse features from TOML content
    fn parse_toml(content: &str) -> Result<Self, String> {
        let mut features = Self::new();

        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with('[') {
                continue;
            }

            if let Some((key, value)) = line.split_once('=') {
                let key = key.trim().replace('_', "-");
                let value = value.trim().trim_matches('"');

                if value == "true" {
                    let _ = features.enable(&key);
                } else if value == "false" {
                    let _ = features.disable(&key);
                } else if value.starts_with('[') {
                    // Array value for slice_blocked
                    if key == "slice-blocked" {
                        let items = value
                            .trim_start_matches('[')
                            .trim_end_matches(']')
                            .split(',')
                            .map(|s| s.trim().trim_matches('"').to_string())
                            .filter(|s| !s.is_empty());
                        features.slice_blocked.extend(items);
                    }
                }
            }
        }

        Ok(features)
    }

    /// Check if any deletion features are enabled
    pub fn has_deletion_features(&self) -> bool {
        self.delete_private_fn
            || self.delete_private_const
            || self.delete_private_type
            || self.delete_private_struct
            || self.trust_graph
    }

    /// Get a summary of enabled features for display
    pub fn summary(&self) -> String {
        let mut enabled = Vec::new();

        if self.delete_private_fn { enabled.push("private-fn"); }
        if self.delete_private_const { enabled.push("private-const"); }
        if self.delete_private_type { enabled.push("private-type"); }
        if self.delete_private_struct { enabled.push("private-struct"); }
        if self.trust_graph { enabled.push("trust-graph"); }
        if self.trial_delete { enabled.push("trial-delete"); }
        if self.trial_sets { enabled.push("trial-sets"); }
        if self.graph_guided { enabled.push("graph-guided"); }
        if self.cycle_breaking { enabled.push("cycle-breaking"); }
        if self.cfg_eval { enabled.push("cfg-eval"); }
        if self.arch_filter { enabled.push("arch-filter"); }
        if self.auto_fixes { enabled.push("auto-fixes"); }
        if self.verify { enabled.push("verify"); }
        if self.profiling { enabled.push("profiling"); }

        for crate_name in &self.slice_blocked {
            enabled.push(Box::leak(format!("slice-{}", crate_name).into_boxed_str()));
        }

        if enabled.is_empty() {
            "none (conservative mode)".to_string()
        } else {
            enabled.join(", ")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_level_0() {
        let features = SlicerFeatures::from_level(0);
        assert!(!features.delete_private_fn);
        assert!(!features.verify);
    }

    #[test]
    fn test_from_level_1() {
        let features = SlicerFeatures::from_level(1);
        assert!(features.delete_private_fn);
        assert!(features.cfg_eval);
        assert!(features.verify);
        assert!(!features.delete_private_const);
    }

    #[test]
    fn test_from_level_2() {
        let features = SlicerFeatures::from_level(2);
        assert!(features.delete_private_fn);
        assert!(features.delete_private_const);
        assert!(features.delete_private_type);
        assert!(features.cycle_breaking);
    }

    #[test]
    fn test_from_level_3() {
        let features = SlicerFeatures::from_level(3);
        assert!(features.slice_blocked.contains("parking_lot"));
        assert!(features.delete_private_struct);
        // Note: aho-corasick is NOT force-sliced at O3 per design decision
        // Users can use -fslice-aho-corasick if needed
    }

    #[test]
    fn test_enable_disable() {
        let mut features = SlicerFeatures::new();

        assert!(features.enable("private-fn").is_ok());
        assert!(features.delete_private_fn);

        assert!(features.disable("private-fn").is_ok());
        assert!(!features.delete_private_fn);

        assert!(features.enable("unknown-feature").is_err());
    }

    #[test]
    fn test_slice_blocked() {
        let mut features = SlicerFeatures::new();

        assert!(features.enable("slice-parking-lot").is_ok());
        assert!(features.slice_blocked.contains("parking-lot"));

        assert!(features.disable("slice-parking-lot").is_ok());
        assert!(!features.slice_blocked.contains("parking-lot"));
    }

    #[test]
    fn test_parse_toml() {
        // TOML keys should match the feature names expected by enable()
        // e.g., "private_fn" -> "private-fn", "verify" -> "verify"
        let toml = r#"
            [features]
            private_fn = true
            verify = true
            # This is a comment
            slice_blocked = ["parking_lot", "aho-corasick"]
        "#;

        let features = SlicerFeatures::parse_toml(toml).unwrap();
        assert!(features.delete_private_fn);
        assert!(features.verify);
    }
}

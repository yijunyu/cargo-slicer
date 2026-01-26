//! Watch mode: monitors source files and intelligently re-slices when needed.
//!
//! This module provides a "smart watch mode" that:
//! - Monitors source file changes in the project
//! - Computes usage fingerprints to detect when dependencies change
//! - Only re-slices crates that have new items used (not just any code change)
//! - Automatically builds after detecting changes

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::{Duration, Instant};

use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};

use crate::slicer::cache::SlicerCache;
use crate::slicer::features::SlicerFeatures;
use crate::types::UsedItem;
use crate::usage::build_usage_index;

/// Usage fingerprint for a crate - hash of used items
#[derive(Debug, Clone)]
pub struct UsageFingerprint {
    /// Crate name (normalized with underscores)
    pub crate_name: String,
    /// Hash of the used items set
    pub usage_hash: String,
    /// The actual used items (for diffing)
    pub items: HashSet<UsedItem>,
}

/// Result of a watch cycle
#[derive(Debug)]
pub enum WatchAction {
    /// No changes detected
    NoChange,
    /// Changes detected, but no new dependencies used - just rebuild
    RebuildOnly,
    /// Changes detected with new dependency usage - need to re-slice specific crates
    Reslice(Vec<String>),
}

/// Compute usage fingerprints for all sliceable crates
///
/// This scans the project source files and computes what items are used
/// from each sliceable dependency crate.
pub fn compute_usage_fingerprints(
    project_src: &Path,
    sliceable_crates: &HashSet<String>,
    features: &SlicerFeatures,
) -> HashMap<String, UsageFingerprint> {
    // Use existing build_usage_index but wrap in fingerprints
    // Note: We pass empty dependent_sources since we only care about project usage
    let usage_index = build_usage_index(project_src, &[], sliceable_crates);

    usage_index
        .into_iter()
        .map(|(crate_name, items)| {
            let items_as_strings: HashSet<String> = items.iter().map(|i| i.path.clone()).collect();
            let usage_hash = SlicerCache::content_hash(&items_as_strings, features);
            (
                crate_name.clone(),
                UsageFingerprint {
                    crate_name,
                    usage_hash,
                    items,
                },
            )
        })
        .collect()
}

/// Determine which crates need re-slicing based on usage changes
///
/// Returns the list of crate names that have NEW items being used
/// (items removed don't matter - the sliced crate will just have extra code)
pub fn crates_needing_reslice(
    old_fingerprints: &HashMap<String, UsageFingerprint>,
    new_fingerprints: &HashMap<String, UsageFingerprint>,
) -> Vec<String> {
    let mut needs_reslice = Vec::new();

    for (crate_name, new_fp) in new_fingerprints {
        match old_fingerprints.get(crate_name) {
            Some(old_fp) => {
                // Check if NEW items were added (items removed don't matter)
                let new_items: HashSet<_> = new_fp.items.difference(&old_fp.items).collect();
                if !new_items.is_empty() {
                    needs_reslice.push(crate_name.clone());
                }
            }
            None => {
                // New crate usage - need to slice it
                if !new_fp.items.is_empty() {
                    needs_reslice.push(crate_name.clone());
                }
            }
        }
    }

    needs_reslice
}

/// Watch mode configuration
#[derive(Debug, Clone)]
pub struct WatchConfig {
    /// Directory containing project source files
    pub project_src: PathBuf,
    /// Project root directory
    pub project_root: PathBuf,
    /// Set of sliceable crate names
    pub sliceable_crates: HashSet<String>,
    /// Whether to build in release mode
    pub build_release: bool,
    /// Slicer feature configuration
    pub features: SlicerFeatures,
    /// Debounce duration for file changes
    pub debounce: Duration,
}

/// Watch mode state
pub struct WatchState {
    /// Current usage fingerprints
    pub fingerprints: HashMap<String, UsageFingerprint>,
    /// Last modification time of any source file
    pub last_change: Instant,
    /// Whether changes are pending processing
    pub pending_changes: bool,
}

impl WatchState {
    pub fn new(config: &WatchConfig) -> Self {
        let fingerprints =
            compute_usage_fingerprints(&config.project_src, &config.sliceable_crates, &config.features);
        Self {
            fingerprints,
            last_change: Instant::now(),
            pending_changes: false,
        }
    }

    /// Process pending changes and determine action
    pub fn process_changes(&mut self, config: &WatchConfig) -> WatchAction {
        if !self.pending_changes {
            return WatchAction::NoChange;
        }

        self.pending_changes = false;

        // Re-compute fingerprints
        let new_fingerprints =
            compute_usage_fingerprints(&config.project_src, &config.sliceable_crates, &config.features);
        let needs_reslice = crates_needing_reslice(&self.fingerprints, &new_fingerprints);

        // Update fingerprints
        self.fingerprints = new_fingerprints;

        if needs_reslice.is_empty() {
            WatchAction::RebuildOnly
        } else {
            WatchAction::Reslice(needs_reslice)
        }
    }
}

/// Run the cargo build command
fn run_cargo_build(project_root: &Path, release: bool) -> Result<(), String> {
    let mut cmd = std::process::Command::new("cargo");
    cmd.arg("build");
    if release {
        cmd.arg("--release");
    }
    cmd.arg("--manifest-path")
        .arg("Cargo.toml.sliced")
        .current_dir(project_root);

    let status = cmd.status().map_err(|e| format!("Failed to run cargo: {}", e))?;

    if status.success() {
        Ok(())
    } else {
        Err(format!("Build failed with exit code: {:?}", status.code()))
    }
}

/// Run the watch loop
///
/// This monitors the project source directory for changes and:
/// - If changes don't affect dependency usage: just rebuilds
/// - If changes add new dependency items: re-slices affected crates, then rebuilds
pub fn run_watch_mode(config: WatchConfig) -> Result<(), Box<dyn std::error::Error>> {
    println!("Watch mode started. Monitoring {} for changes...", config.project_src.display());
    println!("   Press Ctrl+C to stop.\n");

    // Check if sliced manifest exists
    let sliced_manifest = config.project_root.join("Cargo.toml.sliced");
    if !sliced_manifest.exists() {
        return Err("Cargo.toml.sliced not found. Run the slicer first to generate sliced dependencies.".into());
    }

    // Initial state
    let mut state = WatchState::new(&config);
    println!(
        "Computed initial fingerprints for {} crate(s)",
        state.fingerprints.len()
    );
    println!("Watching for changes...\n");

    // Set up file watcher
    let (tx, rx) = mpsc::channel();

    let mut watcher = RecommendedWatcher::new(
        move |res: Result<Event, notify::Error>| {
            if let Ok(event) = res {
                let _ = tx.send(event);
            }
        },
        Config::default(),
    )?;

    watcher.watch(&config.project_src, RecursiveMode::Recursive)?;

    loop {
        match rx.recv_timeout(Duration::from_millis(100)) {
            Ok(event) => {
                // Only care about modifications and creations of .rs files
                if matches!(
                    event.kind,
                    EventKind::Modify(_) | EventKind::Create(_)
                ) {
                    let is_rust_file = event
                        .paths
                        .iter()
                        .any(|p| p.extension().map(|e| e == "rs").unwrap_or(false));

                    if is_rust_file {
                        state.pending_changes = true;
                        state.last_change = Instant::now();
                    }
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {
                // Check if we have pending changes and debounce time has passed
                if state.pending_changes && state.last_change.elapsed() > config.debounce {
                    match state.process_changes(&config) {
                        WatchAction::NoChange => {
                            // Shouldn't happen since pending_changes was true
                        }
                        WatchAction::RebuildOnly => {
                            println!("Changes detected. No new dependencies used. Running cargo build...");
                            match run_cargo_build(&config.project_root, config.build_release) {
                                Ok(_) => println!("Build succeeded.\n"),
                                Err(e) => println!("Build failed: {}\n", e),
                            }
                        }
                        WatchAction::Reslice(crates) => {
                            println!(
                                "Changes detected. Re-slicing {} crate(s): {:?}",
                                crates.len(),
                                crates
                            );

                            // For now, suggest running the full slicer
                            // TODO: Implement selective re-slicing
                            println!("  Note: Selective re-slicing not yet implemented.");
                            println!("  Run './target/release/cargo-slicer' to update sliced crates.\n");

                            // Still try to build - it might work if the new items are already included
                            println!("Attempting build anyway...");
                            match run_cargo_build(&config.project_root, config.build_release) {
                                Ok(_) => println!("Build succeeded.\n"),
                                Err(e) => {
                                    println!("Build failed: {}", e);
                                    println!("  Hint: Run './target/release/cargo-slicer' to re-slice dependencies.\n");
                                }
                            }
                        }
                    }
                }
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                println!("Watch channel disconnected. Exiting.");
                break;
            }
        }
    }

    Ok(())
}

/// Check if watch mode prerequisites are met
pub fn check_watch_prerequisites(project_root: &Path) -> Result<(), String> {
    let sliced_manifest = project_root.join("Cargo.toml.sliced");
    if !sliced_manifest.exists() {
        return Err(format!(
            "Cargo.toml.sliced not found at {}.\n\
             Run './target/release/cargo-slicer' first to generate sliced dependencies.",
            sliced_manifest.display()
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::ItemKind;

    #[test]
    fn test_crates_needing_reslice_no_change() {
        let items: HashSet<UsedItem> = vec![UsedItem {
            path: "regex::Regex".to_string(),
            kind: ItemKind::Struct,
        }]
        .into_iter()
        .collect();

        let old = HashMap::from([(
            "regex".to_string(),
            UsageFingerprint {
                crate_name: "regex".to_string(),
                usage_hash: "abc123".to_string(),
                items: items.clone(),
            },
        )]);

        let new = HashMap::from([(
            "regex".to_string(),
            UsageFingerprint {
                crate_name: "regex".to_string(),
                usage_hash: "abc123".to_string(),
                items,
            },
        )]);

        let result = crates_needing_reslice(&old, &new);
        assert!(result.is_empty());
    }

    #[test]
    fn test_crates_needing_reslice_new_item() {
        let old_items: HashSet<UsedItem> = vec![UsedItem {
            path: "regex::Regex".to_string(),
            kind: ItemKind::Struct,
        }]
        .into_iter()
        .collect();

        let mut new_items = old_items.clone();
        new_items.insert(UsedItem {
            path: "regex::Regex::new".to_string(),
            kind: ItemKind::Method,
        });

        let old = HashMap::from([(
            "regex".to_string(),
            UsageFingerprint {
                crate_name: "regex".to_string(),
                usage_hash: "abc123".to_string(),
                items: old_items,
            },
        )]);

        let new = HashMap::from([(
            "regex".to_string(),
            UsageFingerprint {
                crate_name: "regex".to_string(),
                usage_hash: "def456".to_string(),
                items: new_items,
            },
        )]);

        let result = crates_needing_reslice(&old, &new);
        assert_eq!(result, vec!["regex".to_string()]);
    }

    #[test]
    fn test_crates_needing_reslice_item_removed() {
        // Removing items should NOT trigger re-slicing
        let mut old_items: HashSet<UsedItem> = HashSet::new();
        old_items.insert(UsedItem {
            path: "regex::Regex".to_string(),
            kind: ItemKind::Struct,
        });
        old_items.insert(UsedItem {
            path: "regex::Regex::new".to_string(),
            kind: ItemKind::Method,
        });

        let new_items: HashSet<UsedItem> = vec![UsedItem {
            path: "regex::Regex".to_string(),
            kind: ItemKind::Struct,
        }]
        .into_iter()
        .collect();

        let old = HashMap::from([(
            "regex".to_string(),
            UsageFingerprint {
                crate_name: "regex".to_string(),
                usage_hash: "abc123".to_string(),
                items: old_items,
            },
        )]);

        let new = HashMap::from([(
            "regex".to_string(),
            UsageFingerprint {
                crate_name: "regex".to_string(),
                usage_hash: "def456".to_string(),
                items: new_items,
            },
        )]);

        let result = crates_needing_reslice(&old, &new);
        assert!(result.is_empty());
    }
}

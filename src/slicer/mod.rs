//! New slicer implementation (copy-and-delete approach)
//!
//! This module implements a fundamentally different slicing strategy:
//! 1. Copy original source code (guaranteed to compile)
//! 2. Delete unused items based on heuristic rules
//! 3. Clean up imports and empty modules
//!
//! **Advantages over old slicer:**
//! - Guarantees LOC reduction (can only delete, never add)
//! - No import bloat (original imports are correct)
//! - Fast (no compilation during slicing)
//! - Preserves original structure
//!
//! **Architecture:**
//! - `copy.rs`: Copy original source to output directory
//! - `dependency_graph.rs`: Build what-uses-what graph
//! - `marker.rs`: Mark items as used (graph traversal)
//! - `deleter.rs`: Delete unmarked items
//! - `cleanup.rs`: Remove unused imports and empty modules
//! - `rules/`: Pluggable heuristic rules

// Enforce strict code quality: treat all warnings as errors in new slicer
#![deny(warnings)]

pub mod cache;
pub mod config;
pub mod copy;
pub mod dependency_graph;
pub mod marker;
pub mod deleter;
pub mod cleanup;
pub mod rules;
pub mod features;
pub mod trial_deleter;

use std::path::Path;
use std::collections::HashSet;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

// Global timing accumulators for profiling (thread-safe)
static COPY_TIME_MS: AtomicU64 = AtomicU64::new(0);
static GRAPH_TIME_MS: AtomicU64 = AtomicU64::new(0);
static MARK_TIME_MS: AtomicU64 = AtomicU64::new(0);
static DELETE_TIME_MS: AtomicU64 = AtomicU64::new(0);
static CLEANUP_TIME_MS: AtomicU64 = AtomicU64::new(0);
static VERIFY_TIME_MS: AtomicU64 = AtomicU64::new(0);
static CRATE_COUNT: AtomicU64 = AtomicU64::new(0);
static VERIFY_PASS_COUNT: AtomicU64 = AtomicU64::new(0);
static VERIFY_FAIL_COUNT: AtomicU64 = AtomicU64::new(0);

/// Print accumulated timing stats
pub fn print_timing_stats() {
    let count = CRATE_COUNT.load(Ordering::Relaxed);
    if count == 0 {
        return;
    }
    let copy_ms = COPY_TIME_MS.load(Ordering::Relaxed);
    let graph_ms = GRAPH_TIME_MS.load(Ordering::Relaxed);
    let mark_ms = MARK_TIME_MS.load(Ordering::Relaxed);
    let delete_ms = DELETE_TIME_MS.load(Ordering::Relaxed);
    let cleanup_ms = CLEANUP_TIME_MS.load(Ordering::Relaxed);
    let verify_ms = VERIFY_TIME_MS.load(Ordering::Relaxed);
    let total_ms = copy_ms + graph_ms + mark_ms + delete_ms + cleanup_ms + verify_ms;

    println!("\n=== Slicer Timing Breakdown ({} crates) ===", count);
    println!("  Copy source:       {:>8}ms ({:>5.1}%)", copy_ms, copy_ms as f64 / total_ms as f64 * 100.0);
    println!("  Build graph:       {:>8}ms ({:>5.1}%)", graph_ms, graph_ms as f64 / total_ms as f64 * 100.0);
    println!("  Mark used:         {:>8}ms ({:>5.1}%)", mark_ms, mark_ms as f64 / total_ms as f64 * 100.0);
    println!("  Delete unused:     {:>8}ms ({:>5.1}%)", delete_ms, delete_ms as f64 / total_ms as f64 * 100.0);
    println!("  Cleanup:           {:>8}ms ({:>5.1}%)", cleanup_ms, cleanup_ms as f64 / total_ms as f64 * 100.0);
    if verify_ms > 0 {
        println!("  Verify:            {:>8}ms ({:>5.1}%)", verify_ms, verify_ms as f64 / total_ms as f64 * 100.0);
        let pass = VERIFY_PASS_COUNT.load(Ordering::Relaxed);
        let fail = VERIFY_FAIL_COUNT.load(Ordering::Relaxed);
        println!("    Pass: {}, Fail: {}", pass, fail);
    }
    println!("  Total:             {:>8}ms", total_ms);
    println!("  Avg per crate:     {:>8.1}ms", total_ms as f64 / count as f64);
}

/// Reset timing stats
pub fn reset_timing_stats() {
    COPY_TIME_MS.store(0, Ordering::Relaxed);
    GRAPH_TIME_MS.store(0, Ordering::Relaxed);
    MARK_TIME_MS.store(0, Ordering::Relaxed);
    DELETE_TIME_MS.store(0, Ordering::Relaxed);
    CLEANUP_TIME_MS.store(0, Ordering::Relaxed);
    VERIFY_TIME_MS.store(0, Ordering::Relaxed);
    CRATE_COUNT.store(0, Ordering::Relaxed);
    VERIFY_PASS_COUNT.store(0, Ordering::Relaxed);
    VERIFY_FAIL_COUNT.store(0, Ordering::Relaxed);
}

/// Main entry point for new slicer
pub fn slice_crate(
    crate_name: &str,
    source_dir: &Path,
    used_items: &HashSet<String>,
    output_dir: &Path,
    config: &config::SlicerConfig,
) -> Result<SliceResult, String> {
    if config.verbose {
        println!("🔪 New slicer: {}", crate_name);
    }

    // Step 1: Copy original source
    if config.verbose {
        println!("  📋 Copying original source...");
    }
    let t1 = Instant::now();
    copy::copy_source(source_dir, output_dir)?;
    COPY_TIME_MS.fetch_add(t1.elapsed().as_millis() as u64, Ordering::Relaxed);

    // Step 2: Build dependency graph
    if config.verbose {
        println!("  📊 Building dependency graph...");
    }
    let t2 = Instant::now();
    let (graph, index, public_reexports) = dependency_graph::build_graph(output_dir, crate_name)?;
    GRAPH_TIME_MS.fetch_add(t2.elapsed().as_millis() as u64, Ordering::Relaxed);

    // Step 3: Mark used items (including public re-exports from lib.rs)
    if config.verbose {
        println!("  ✅ Marking used items...");
    }
    let t3 = Instant::now();
    let used = marker::mark_used_items(&graph, used_items, &public_reexports, config)?;
    MARK_TIME_MS.fetch_add(t3.elapsed().as_millis() as u64, Ordering::Relaxed);

    // Step 4: Delete unused items
    if config.verbose {
        println!("  🗑️  Deleting unused items...");
    }
    let t4 = Instant::now();
    let stats = deleter::delete_unused(&index, &used, output_dir, config)?;
    DELETE_TIME_MS.fetch_add(t4.elapsed().as_millis() as u64, Ordering::Relaxed);

    // Step 4b: Trial-based deletion is now done at workspace level in slice_all.rs (Phase 5b)
    // The workspace-level approach is more accurate because it verifies against
    // the entire workspace, catching cross-crate dependencies that per-crate
    // verification would miss.

    // Step 5: Cleanup
    if config.verbose {
        println!("  🧹 Cleaning up imports and modules...");
    }
    let t5 = Instant::now();
    cleanup::cleanup(output_dir, &used)?;
    CLEANUP_TIME_MS.fetch_add(t5.elapsed().as_millis() as u64, Ordering::Relaxed);

    CRATE_COUNT.fetch_add(1, Ordering::Relaxed);

    // Step 6: Verification (optional, when -fverify is enabled)
    let verified = if config.features.verify && stats.items_deleted > 0 {
        if config.verbose {
            println!("  🔍 Verifying with cargo check...");
        }
        let t6 = Instant::now();
        let check_result = verify_crate(output_dir);
        VERIFY_TIME_MS.fetch_add(t6.elapsed().as_millis() as u64, Ordering::Relaxed);

        match check_result {
            Ok(()) => {
                VERIFY_PASS_COUNT.fetch_add(1, Ordering::Relaxed);
                if config.verbose {
                    println!("  ✅ Verification passed");
                }
                Some(true)
            }
            Err(e) => {
                VERIFY_FAIL_COUNT.fetch_add(1, Ordering::Relaxed);
                if config.verbose {
                    println!("  ❌ Verification failed: {}", e);
                }
                Some(false)
            }
        }
    } else {
        None // Verification not requested or nothing deleted
    };

    if config.verbose {
        println!("  ✨ Done! Removed {} items", stats.items_deleted);
    }

    Ok(SliceResult {
        items_kept: used.len(),
        items_deleted: stats.items_deleted,
        loc_before: stats.loc_before,
        loc_after: stats.loc_after,
        verified,
    })
}

/// Verify a sliced crate compiles with `cargo check`
fn verify_crate(crate_dir: &Path) -> Result<(), String> {
    use std::process::Command;

    // Check if this crate is part of a workspace (has [workspace] or is referenced by parent)
    // If so, skip individual verification - workspace will be verified later
    let cargo_toml = crate_dir.join("Cargo.toml");
    if cargo_toml.exists() {
        if let Ok(content) = std::fs::read_to_string(&cargo_toml) {
            // Check for workspace member reference pattern
            if content.contains("workspace = ") && !content.contains("[workspace]") {
                // This crate inherits from a workspace, skip individual check
                return Ok(());
            }
        }
    }

    let output = Command::new("cargo")
        .arg("check")
        .arg("--lib")
        .arg("--quiet")
        .current_dir(crate_dir)
        .output()
        .map_err(|e| format!("Failed to run cargo check: {}", e))?;

    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);

        // If the error is about workspace membership, consider it a pass
        // The workspace verification will catch real errors later
        if stderr.contains("believes it's in a workspace") ||
           stderr.contains("is a member of workspace") {
            return Ok(());
        }

        // Extract first error line for concise reporting
        let first_error = stderr
            .lines()
            .find(|l| l.contains("error"))
            .unwrap_or("compilation failed");
        Err(first_error.to_string())
    }
}

#[derive(Debug)]
pub struct SliceResult {
    pub items_kept: usize,
    pub items_deleted: usize,
    pub loc_before: usize,
    pub loc_after: usize,
    /// Verification result: None if not verified, Some(true) if passed, Some(false) if failed
    pub verified: Option<bool>,
}

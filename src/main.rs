//! Cargo Slicer - Extract minimal subsets of Rust crates.
//!
//! This module provides functionality to analyze crate usage and generate
//! sliced versions containing only the items actually used.

#![allow(dead_code)]  // Many helper functions may be useful later

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

// Helper to access submodules since main.rs is no longer the parent
// We need to use full paths like cargo_slicer::module
#[cfg(feature = "scip-analysis")]
use cargo_slicer::old_slicer::semantic;
use cargo_slicer::common::source_location as crate_info;
use cargo_slicer::slice_all;
use cargo_slicer::dep_graph;
#[cfg(feature = "scip-analysis")]
use cargo_slicer::old_slicer::auto_fix;
use cargo_slicer::types::CrateInfo;
use cargo_slicer::slicer::features::SlicerFeatures;

/// Benchmark result for a single build configuration
#[derive(Debug)]
struct BenchResult {
    name: String,
    duration_secs: f64,
    success: bool,
}

/// Run build benchmarks comparing sliced vs normal, debug vs release
/// slicing_time_secs: time taken by the slicer (to include in total overhead)
fn run_benchmarks(project_root: &Path, slicing_time_secs: f64) -> Vec<BenchResult> {
    use std::time::Instant;

    let mut results = Vec::new();

    // Store slicing overhead for the summary
    results.push(BenchResult {
        name: "Slicing Overhead".to_string(),
        duration_secs: slicing_time_secs,
        success: true,
    });
    let sliced_manifest = project_root.join("Cargo.toml.sliced");
    let original_manifest = project_root.join("Cargo.toml");
    let backup_manifest = project_root.join("Cargo.toml.bak");

    // Helper to run a build and measure time
    let run_build = |release: bool, clean_first: bool| -> (f64, bool) {
        // Clean if requested
        if clean_first {
            let _ = std::process::Command::new("cargo")
                .arg("clean")
                .current_dir(project_root)
                .output();
        }

        let start = Instant::now();
        let mut cmd = std::process::Command::new("cargo");
        cmd.arg("build");
        if release {
            cmd.arg("--release");
        }
        cmd.current_dir(project_root);

        let output = cmd.output();
        let duration = start.elapsed().as_secs_f64();

        let success = output.map(|o| o.status.success()).unwrap_or(false);
        (duration, success)
    };

    println!("\n=== Build Benchmarks ===");
    println!("Running 4 builds: sliced debug/release, normal debug/release");
    println!("(Each build starts fresh after cargo clean)\n");

    // 1. Sliced debug build
    if sliced_manifest.exists() {
        let _ = fs::rename(&original_manifest, &backup_manifest);
        let _ = fs::copy(&sliced_manifest, &original_manifest);

        print!("  Building sliced (debug)...");
        std::io::Write::flush(&mut std::io::stdout()).ok();
        let (duration, success) = run_build(false, true);
        println!(" {:.2}s {}", duration, if success { "✓" } else { "✗" });
        results.push(BenchResult {
            name: "Sliced Debug".to_string(),
            duration_secs: duration,
            success,
        });

        // 2. Sliced release build
        print!("  Building sliced (release)...");
        std::io::Write::flush(&mut std::io::stdout()).ok();
        let (duration, success) = run_build(true, true);
        println!(" {:.2}s {}", duration, if success { "✓" } else { "✗" });
        results.push(BenchResult {
            name: "Sliced Release".to_string(),
            duration_secs: duration,
            success,
        });

        // Restore original manifest
        let _ = fs::rename(&backup_manifest, &original_manifest);
    }

    // 3. Normal debug build
    print!("  Building normal (debug)...");
    std::io::Write::flush(&mut std::io::stdout()).ok();
    let (duration, success) = run_build(false, true);
    println!(" {:.2}s {}", duration, if success { "✓" } else { "✗" });
    results.push(BenchResult {
        name: "Normal Debug".to_string(),
        duration_secs: duration,
        success,
    });

    // 4. Normal release build
    print!("  Building normal (release)...");
    std::io::Write::flush(&mut std::io::stdout()).ok();
    let (duration, success) = run_build(true, true);
    println!(" {:.2}s {}", duration, if success { "✓" } else { "✗" });
    results.push(BenchResult {
        name: "Normal Release".to_string(),
        duration_secs: duration,
        success,
    });

    results
}

/// Print benchmark summary table
fn print_benchmark_summary(results: &[BenchResult]) {
    println!("\n=== Benchmark Summary ===\n");
    println!("  {:<20} {:>12} {:>10}", "Build Type", "Time", "Status");
    println!("  {}", "-".repeat(44));

    for result in results {
        let status = if result.success { "✓ OK" } else { "✗ FAIL" };
        println!("  {:<20} {:>10.2}s {:>10}", result.name, result.duration_secs, status);
    }

    // Calculate speedups if we have all results (5 with slicing overhead)
    let slicing_overhead = results.iter().find(|r| r.name == "Slicing Overhead");
    let sliced_debug = results.iter().find(|r| r.name == "Sliced Debug");
    let sliced_release = results.iter().find(|r| r.name == "Sliced Release");
    let normal_debug = results.iter().find(|r| r.name == "Normal Debug");
    let normal_release = results.iter().find(|r| r.name == "Normal Release");

    if let Some(overhead) = slicing_overhead {
        println!("\n  Comparison (build time only):");
        if let (Some(sd), Some(nd)) = (sliced_debug, normal_debug) {
            let speedup = nd.duration_secs / sd.duration_secs;
            let saved = nd.duration_secs - sd.duration_secs;
            println!("    Debug:   {:.1}x faster with slicing ({:.1}s saved)",
                     speedup, saved);
        }
        if let (Some(sr), Some(nr)) = (sliced_release, normal_release) {
            let speedup = nr.duration_secs / sr.duration_secs;
            let saved = nr.duration_secs - sr.duration_secs;
            println!("    Release: {:.1}x faster with slicing ({:.1}s saved)",
                     speedup, saved);
        }

        println!("\n  End-to-end (including {:.1}s slicing overhead):", overhead.duration_secs);
        if let (Some(sd), Some(nd)) = (sliced_debug, normal_debug) {
            let total_sliced = sd.duration_secs + overhead.duration_secs;
            let speedup = nd.duration_secs / total_sliced;
            let saved = nd.duration_secs - total_sliced;
            println!("    Debug:   {:.1}x ({:.1}s sliced+slicer vs {:.1}s normal, {:.1}s {})",
                     speedup, total_sliced, nd.duration_secs, saved.abs(),
                     if saved >= 0.0 { "saved" } else { "overhead" });
        }
        if let (Some(sr), Some(nr)) = (sliced_release, normal_release) {
            let total_sliced = sr.duration_secs + overhead.duration_secs;
            let speedup = nr.duration_secs / total_sliced;
            let saved = nr.duration_secs - total_sliced;
            println!("    Release: {:.1}x ({:.1}s sliced+slicer vs {:.1}s normal, {:.1}s {})",
                     speedup, total_sliced, nr.duration_secs, saved.abs(),
                     if saved >= 0.0 { "saved" } else { "overhead" });
        }
    }
    println!();
}

fn print_usage() {
    eprintln!("Usage: cargo slicer [OPTIONS] [<target>] [-- CARGO_ARGS]");
    eprintln!();
    eprintln!("Slice Rust crates to include only used code.");
    eprintln!();
    eprintln!("Targets:");
    eprintln!("  (none)             Slice all dependencies of the current project");
    eprintln!("  <crate_name>       Slice a specific crate from the registry");
    eprintln!("  <folder>           Slice all dependencies of the project in <folder>");
    eprintln!();
    eprintln!("Common Options:");
    eprintln!("      --no-clean     Skip cleaning (default: clean before slicing)");
    eprintln!("      --no-bench     Skip benchmarks (default: run benchmarks)");
    eprintln!("  -v, --verbose      Show detailed output and debug information");
    eprintln!("  -o, --output DIR   Custom output directory (default: <project>_sliced/)");
    eprintln!("  -w, --watch        Watch mode: monitor files and auto-rebuild");
    eprintln!("      --old          Use old generation-based slicer (may cause bloat)");
    eprintln!("      --self-upgrade Build slicer with its own sliced dependencies");
    eprintln!("  -h, --help         Show this help message");
    eprintln!("      --help-advanced  Show all advanced options");
    eprintln!();
    eprintln!("Optimization Levels (default: -O3):");
    eprintln!("  -O                 Fast production mode - delete without verification");
    eprintln!("  -O0                Conservative - no deletion (safe baseline)");
    eprintln!("  -O1                Delete private functions (with verification)");
    eprintln!("  -O2                Delete all private items + trial deletion (slow, dev mode)");
    eprintln!("  -O3                + Graph-guided deletion (default)");
    eprintln!("  -O4                Full aggressive mode (trust dependency graph)");
    eprintln!();
    eprintln!("Feature Flags (like GCC -f):");
    eprintln!("  -f<feature>        Enable a feature (e.g., -fprivate-fn)");
    eprintln!("  -fno-<feature>     Disable a feature (e.g., -fno-verify)");
    eprintln!();
    eprintln!("Note: Use -O for fastest slicing in production. Use -O2 during development");
    eprintln!("      to discover minimal code (trial deletion verifies each deletion).");
    eprintln!();
    eprintln!("Build Command:");
    eprintln!("  build [ARGS]       Slice dependencies and build with sliced crates");
    eprintln!("                     Example: cargo-slicer build --release");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  cargo-slicer -O              # Fast production slicing (recommended)");
    eprintln!("  cargo-slicer -O2             # Development mode with trial deletion");
    eprintln!("  cargo-slicer -O -b           # Fast slicing + benchmarks");
    eprintln!("  cargo-slicer --clean -O      # Clean and slice");
    eprintln!("  cargo-slicer regex -O        # Slice regex crate fast");
}

fn print_advanced_help() {
    print_usage();
    eprintln!();
    eprintln!("Feature Flags (detailed):");
    eprintln!();
    eprintln!("  Deletion Features:");
    eprintln!("    -fprivate-fn       Delete unused private functions");
    eprintln!("    -fprivate-const    Delete unused private constants/statics");
    eprintln!("    -fprivate-type     Delete unused private type aliases");
    eprintln!("    -ftrust-graph      Trust graph for all items (risky)");
    eprintln!();
    eprintln!("  Analysis Features:");
    eprintln!("    -fcycle-breaking   Enable cycle detection & breaking");
    eprintln!("    -fcfg-eval         Evaluate #[cfg(...)] for target platform");
    eprintln!("    -farch-filter      Filter architecture-specific code");
    eprintln!("    -fauto-fixes       Apply Cargo.toml compatibility fixes");
    eprintln!();
    eprintln!("  Marking Rules:");
    eprintln!("    -fmark-impl-blocks Keep full impl blocks if type is used");
    eprintln!("    -fmark-trait-deps  Include trait implementations");
    eprintln!("    -fmark-type-deps   Include types in signatures");
    eprintln!("    -fmark-generic-bounds Include trait bounds on generics");
    eprintln!();
    eprintln!("  Verification:");
    eprintln!("    -fverify           Run cargo check after deletion");
    eprintln!("    -fprofiling        Detailed timing analysis per phase");
    eprintln!();
    eprintln!("  Blocked Crate Override:");
    eprintln!("    -fslice-<crate>    Force slice a normally-blocked crate");
    eprintln!("                       e.g., -fslice-parking-lot");
    eprintln!();
    eprintln!("Advanced Options:");
    eprintln!();
    eprintln!("Analysis Mode:");
    eprintln!("  --analyze-deps     Analyze and display dependency graph");
    eprintln!("  --topo-order       Show dependencies in topological order");
    eprintln!();
    eprintln!("Slicing Strategy (for development/debugging):");
    eprintln!("  -l, --locate       Show crate source location and statistics");
    eprintln!("  -g, --generate     Generate sliced crate POC (stub implementations)");
    eprintln!("  -s, --semantic     Use syn-based semantic analysis (extracts real code)");
    eprintln!("  -m, --module-slice Use whole-module slicing (includes entire files)");
    eprintln!("  -a, --adaptive     Adaptive mode: try semantic first, fallback to module");
    eprintln!("  -r, --ra-deps      Use rust-analyzer for accurate dependency analysis");
    eprintln!("  -p, --predict      Predict slicing potential without actually slicing");
    eprintln!();
    eprintln!("Standalone Mode:");
    eprintln!("  --path DIR         Crate source directory (requires --items)");
    eprintln!("  --items LIST       Comma-separated seed items (requires --path)");
    eprintln!();
    eprintln!("Performance & Debugging:");
    eprintln!("  --auto-fix         Auto-generate stubs for missing types");
    eprintln!("  --profile-crate X  Profile timing on single crate X");
    eprintln!("  --bench            Run build benchmarks (sliced vs normal, debug vs release)");
    eprintln!("  --no-parallel      Disable parallel processing (sequential mode)");
    eprintln!("  --abort-threshold N  Abort if failure rate exceeds N (0.0-1.0)");
    eprintln!("  --ra-timeout N     rust-analyzer timeout in seconds (default: 300)");
    eprintln!("  --log-decisions    Enable comprehensive decision logging");
    eprintln!();
    eprintln!("Output Control:");
    eprintln!("  --no-progress      Disable progress bars (for CI/non-interactive)");
    eprintln!("  --measure          Measure LOC reduction (default: enabled)");
    eprintln!("  --no-measure       Disable LOC measurement");
    eprintln!("  --unicode          Use unicode characters in output (default)");
    eprintln!("  --ascii            Use only ASCII characters in output");
    eprintln!();
    eprintln!("Advanced Control:");
    eprintln!("  --original-source  Copy original sources to crates_to_slice/ (default)");
    eprintln!("  --no-original-source  Disable copying original sources");
    eprintln!("  --force-all        Slice all crates including complex ones (default)");
    eprintln!("  --no-force-all     Conservative: skip complex crates (serde, tokio, etc.)");
    eprintln!("  --registry-fallback  Use registry version for bloat-likely crates (default)");
    eprintln!("  --no-registry-fallback  Disable registry fallback (slice everything)");
    eprintln!("  --passthrough      Enable passthrough for compilation failures");
    eprintln!();
    eprintln!("Caching:");
    eprintln!("  --cache FILE       Phase 2 cache file (default: .phase2_cache_<hash>.json)");
    eprintln!("  --no-cache         Disable automatic Phase 2 caching");
    eprintln!("  --save-phase2 FILE [Deprecated] Use --cache instead");
    eprintln!("  --load-phase2 FILE [Deprecated] Use --cache instead");
    eprintln!();
    eprintln!("Slicer Selection:");
    eprintln!("  --use-new-slicer, --new  Use new copy-and-delete slicer (default)");
    eprintln!("  --use-old-slicer, --old  Use old generation-based slicer");
    eprintln!();
    eprintln!("Development:");
    eprintln!("  --generate-std-types PATH  Generate std type mappings from SCIP index");
    eprintln!("  --debug-scip PATH  Debug SCIP analysis for a crate");
    eprintln!("  --debug-impl PATH  Debug impl block parsing for a crate");
}

/// Load features defined in a project's Cargo.toml
fn load_project_features(project_root: &Path) -> std::collections::HashSet<String> {
    let mut features = std::collections::HashSet::new();

    let cargo_toml = project_root.join("Cargo.toml");
    if let Ok(content) = fs::read_to_string(&cargo_toml) {
        let mut in_features = false;
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with('[') {
                in_features = trimmed == "[features]";
            } else if in_features && !trimmed.is_empty() && trimmed.contains('=') {
                if let Some(eq_pos) = trimmed.find('=') {
                    let feature_name = trimmed[..eq_pos].trim();
                    if !feature_name.is_empty() {
                        features.insert(feature_name.to_string());
                    }
                }
            }
        }
    }

    features
}

pub fn main() {
    let args: Vec<String> = env::args().collect();

    let mut target_arg: Option<String> = None;
    let mut _locate_crate = false;
    let mut _generate_crate = false;
    let mut _use_semantic = false;
    let mut _use_module_slice = false;
    let mut _use_adaptive = false;
    let mut _use_ra_deps = false;
    let mut output_dir: Option<PathBuf> = None;
    let mut analyze_deps = false;
    let mut topo_order = false;
    let mut auto_fix = false;
    let mut _predict_mode = false;
    let mut profile_crate: Option<String> = None;
    let mut parallel = true;
    let mut abort_threshold: Option<f64> = None;
    let mut ra_timeout: Option<u64> = None;
    let mut show_progress = true;
    let mut measure = true;
    let mut use_unicode = true;
    let mut original_source = true;
    let mut force_all = true;
    let mut verbose = false;
    let mut log_decisions = false;
    let mut clean_mode = true;  // Default: clean before slicing
    let mut generate_std_types_path: Option<PathBuf> = None;
    let mut use_cache = true;
    let mut cache_file = PathBuf::from(".phase2_cache.json");
    let mut save_phase2_cache: Option<PathBuf> = None;
    let mut load_phase2_cache: Option<PathBuf> = None;
    let mut use_new_slicer = true;  // Default: copy-and-delete approach (guarantees reduction)
    let mut registry_fallback = true;  // Use registry version for bloat-likely crates (default)
    let mut enable_profiling = false;  // Enable detailed timing profiling
    let mut profile_output_path: Option<PathBuf> = None;  // Path for profile JSON output
    let mut bench_mode = true;  // Default: run build benchmarks comparing sliced vs normal
    let mut features = SlicerFeatures::new();  // Feature configuration for aggressive slicing
    let mut optimization_level: Option<u8> = Some(3);  // Default: -O3 (graph-guided deletion)
    let mut self_upgrade_mode = false;  // Self-slicing flywheel: build slicer with sliced deps
    let mut watch_mode = false;  // Watch mode: monitor files and auto-rebuild

    // Standalone mode args
    let mut crate_path: Option<PathBuf> = None;
    let mut seed_items: Vec<String> = Vec::new();

    // Build command capture
    let mut explicit_build_cmd = false;
    let mut cargo_args: Vec<String> = Vec::new();

    // Passthrough args for cargo build (e.g., --release, --features foo)
    let mut passthrough_args: Vec<String> = Vec::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--locate" | "-l" => _locate_crate = true,
            "--generate" | "-g" => _generate_crate = true,
            "--semantic" | "-s" => {
                _use_semantic = true;
                _generate_crate = true;
            }
            "--module-slice" | "-m" => {
                _use_module_slice = true;
                _use_semantic = true;
                _generate_crate = true;
            }
            "--adaptive" | "-a" => {
                _use_adaptive = true;
                _use_semantic = true;
                _generate_crate = true;
            }
            "--ra-deps" | "-r" => {
                _use_ra_deps = true;
                _use_semantic = true;
                _generate_crate = true;
            }
            "--output" | "-o" => {
                i += 1;
                if i < args.len() {
                    output_dir = Some(PathBuf::from(&args[i]));
                } else {
                    eprintln!("Error: --output requires a directory argument");
                    std::process::exit(1);
                }
            }
            "--path" => {
                i += 1;
                if i < args.len() {
                    crate_path = Some(PathBuf::from(&args[i]));
                } else {
                    eprintln!("Error: --path requires a directory argument");
                    std::process::exit(1);
                }
            }
            "--items" => {
                i += 1;
                if i < args.len() {
                    seed_items = args[i].split(',').map(|s| s.trim().to_string()).collect();
                } else {
                    eprintln!("Error: --items requires a comma-separated list");
                    std::process::exit(1);
                }
            }
            "--analyze-deps" => analyze_deps = true,
            "--auto-fix" => auto_fix = true,
            "--topo-order" => topo_order = true,
            "--predict" | "-p" => _predict_mode = true,
            "--profile-crate" => {
                i += 1;
                if i < args.len() {
                    profile_crate = Some(args[i].clone());
                    _use_ra_deps = true;
                    _use_semantic = true;
                } else {
                    eprintln!("Error: --profile-crate requires a crate name");
                    std::process::exit(1);
                }
            }
            "--no-parallel" => parallel = false,
            "--no-progress" => show_progress = false,
            "--measure" => measure = true,
            "--no-measure" => measure = false,
            "--unicode" => use_unicode = true,
            "--ascii" => use_unicode = false,
            "--original-source" => original_source = true,
            "--no-original-source" => original_source = false,
            "--force-all" => force_all = true,
            "--no-force-all" => force_all = false,
            "--registry-fallback" => registry_fallback = true,
            "--no-registry-fallback" => registry_fallback = false,
            "--verbose" | "-v" => verbose = true,
            "--log-decisions" => log_decisions = true,
            "--profile" => enable_profiling = true,
            "--bench" | "-b" => bench_mode = true,
            "--no-bench" => bench_mode = false,
            "--self-upgrade" => self_upgrade_mode = true,
            "--watch" | "-w" => watch_mode = true,
            // Optimization levels: -O (fast), -O0 through -O4
            "-O" => {
                // Fast production mode: delete without verification
                features = SlicerFeatures::fast();
            }
            "-O0" => optimization_level = Some(0),
            "-O1" => optimization_level = Some(1),
            "-O2" => optimization_level = Some(2),
            "-O3" => optimization_level = Some(3),
            "-O4" => optimization_level = Some(4),
            // Feature flags: -f<feature> to enable, -fno-<feature> to disable
            s if s.starts_with("-fno-") => {
                let feature = &s[5..];
                if let Err(e) = features.disable(feature) {
                    eprintln!("Warning: {}", e);
                }
            }
            s if s.starts_with("-f") && s.len() > 2 => {
                let feature = &s[2..];
                if let Err(e) = features.enable(feature) {
                    eprintln!("Warning: {}", e);
                }
            }
            "--profile-output" => {
                i += 1;
                if i < args.len() {
                    profile_output_path = Some(PathBuf::from(&args[i]));
                } else {
                    eprintln!("Error: --profile-output requires a file path");
                    std::process::exit(1);
                }
            }
            "--clean" | "-c" => clean_mode = true,
            "--no-clean" => clean_mode = false,
            "--use-new-slicer" | "--new" => use_new_slicer = true,
            "--use-old-slicer" | "--old" => use_new_slicer = false,
            "--abort-threshold" => {
                i += 1;
                if i < args.len() {
                    match args[i].parse::<f64>() {
                        Ok(t) if t >= 0.0 && t <= 1.0 => {
                            abort_threshold = Some(t);
                        }
                        Ok(t) => {
                            eprintln!("Error: --abort-threshold must be between 0.0 and 1.0, got {}", t);
                            std::process::exit(1);
                        }
                        Err(_) => {
                            eprintln!("Error: --abort-threshold requires a number (e.g., 0.3 for 30%)");
                            std::process::exit(1);
                        }
                    }
                } else {
                    eprintln!("Error: --abort-threshold requires a threshold value");
                    std::process::exit(1);
                }
            }
            "--ra-timeout" => {
                i += 1;
                if i < args.len() {
                    match args[i].parse::<u64>() {
                        Ok(t) if t > 0 => {
                            ra_timeout = Some(t);
                        }
                        Ok(t) => {
                            eprintln!("Error: --ra-timeout must be > 0, got {}", t);
                            std::process::exit(1);
                        }
                        Err(_) => {
                            eprintln!("Error: --ra-timeout requires a number in seconds (e.g., 300 for 5 minutes)");
                            std::process::exit(1);
                        }
                    }
                } else {
                    eprintln!("Error: --ra-timeout requires a timeout value in seconds");
                    std::process::exit(1);
                }
            }
            "--generate-std-types" => {
                i += 1;
                if i < args.len() {
                    generate_std_types_path = Some(PathBuf::from(&args[i]));
                } else {
                    eprintln!("Error: --generate-std-types requires a SCIP index path");
                    std::process::exit(1);
                }
            }
            "--cache" => {
                i += 1;
                if i < args.len() {
                    cache_file = PathBuf::from(&args[i]);
                    use_cache = true;
                } else {
                    eprintln!("Error: --cache requires a file path");
                    std::process::exit(1);
                }
            }
            "--no-cache" => {
                use_cache = false;
            }
            "--save-phase2" => {
                eprintln!("Warning: --save-phase2 is deprecated, use --cache instead");
                i += 1;
                if i < args.len() {
                    save_phase2_cache = Some(PathBuf::from(&args[i]));
                } else {
                    eprintln!("Error: --save-phase2 requires a file path");
                    std::process::exit(1);
                }
            }
            "--load-phase2" => {
                eprintln!("Warning: --load-phase2 is deprecated, use --cache instead");
                i += 1;
                if i < args.len() {
                    load_phase2_cache = Some(PathBuf::from(&args[i]));
                } else {
                    eprintln!("Error: --load-phase2 requires a file path");
                    std::process::exit(1);
                }
            }
            #[cfg(feature = "scip-analysis")]
            "--debug-scip" => {
                i += 1;
                if i < args.len() {
                    semantic::debug_scip_visibility(Path::new(&args[i]));
                    return;
                } else {
                    eprintln!("Error: --debug-scip requires a SCIP index path");
                    std::process::exit(1);
                }
            }
            #[cfg(feature = "scip-analysis")]
            "--debug-impl" => {
                i += 1;
                if i < args.len() {
                    semantic::debug_scip_impl_blocks(Path::new(&args[i]));
                    return;
                } else {
                    eprintln!("Error: --debug-impl requires a crate path");
                    std::process::exit(1);
                }
            }
            "--help-advanced" => {
                print_advanced_help();
                return;
            }
            "--help" | "-h" => {
                print_usage();
                return;
            }
            "build" => {
                explicit_build_cmd = true;
                // Collect remaining args for cargo
                cargo_args = args[i+1..].to_vec();
                break;
            }
            s if !s.starts_with('-') => target_arg = Some(s.to_string()),
            _ => {
                // Collect unknown args as passthrough args for cargo build
                passthrough_args.push(args[i].clone());
            }
        }
        i += 1;
    }

    // Apply optimization level to features (CLI -f flags can override)
    if let Some(level) = optimization_level {
        // Start with level preset, then apply individual flag overrides
        let preset = SlicerFeatures::from_level(level);
        // Merge: preset first, then any CLI overrides already in features
        features.delete_private_fn = features.delete_private_fn || preset.delete_private_fn;
        features.delete_private_const = features.delete_private_const || preset.delete_private_const;
        features.delete_private_type = features.delete_private_type || preset.delete_private_type;
        features.delete_private_struct = features.delete_private_struct || preset.delete_private_struct;
        features.trust_graph = features.trust_graph || preset.trust_graph;
        features.trial_delete = features.trial_delete || preset.trial_delete;
        features.trial_sets = features.trial_sets || preset.trial_sets;
        features.graph_guided = features.graph_guided || preset.graph_guided;
        if features.trial_limit == 0 && preset.trial_limit > 0 {
            features.trial_limit = preset.trial_limit;
        }
        features.cycle_breaking = features.cycle_breaking || preset.cycle_breaking;
        features.cfg_eval = features.cfg_eval || preset.cfg_eval;
        features.arch_filter = features.arch_filter || preset.arch_filter;
        features.auto_fixes = features.auto_fixes || preset.auto_fixes;
        features.cfg_feature_deps = features.cfg_feature_deps || preset.cfg_feature_deps;
        features.mark_impl_blocks = features.mark_impl_blocks || preset.mark_impl_blocks;
        features.mark_trait_deps = features.mark_trait_deps || preset.mark_trait_deps;
        features.mark_type_deps = features.mark_type_deps || preset.mark_type_deps;
        features.mark_generic_bounds = features.mark_generic_bounds || preset.mark_generic_bounds;
        features.verify = features.verify || preset.verify;
        features.profiling = features.profiling || preset.profiling;
        features.slice_blocked.extend(preset.slice_blocked);
    }

    // Show features if verbose or if any features are enabled
    if verbose && features.has_deletion_features() {
        println!("Slicer features: {}", features.summary());
    }

    if verbose {
        cargo_slicer::old_slicer::parsing::set_verbose(true);
        // Enable debug logging to cargo-slicer-debug.log
        std::env::set_var("CARGO_SLICER_DEBUG_LOG", "cargo-slicer-debug.log");
        cargo_slicer::debug_log::init();
    }

    // Handle build command
    if explicit_build_cmd {
        use cargo_slicer::build_handler::BuildHandler;
        let handler = BuildHandler::new();
        let exit_code = handler.handle_build_command(&cargo_args);
        std::process::exit(exit_code);
    }

    // Handle --generate-std-types mode (requires scip-analysis feature)
    #[cfg(feature = "scip-analysis")]
    if let Some(scip_path) = generate_std_types_path {
        println!("Generating std type mappings from SCIP index: {}", scip_path.display());
        match semantic::generate_std_from_scip(&scip_path) {
            Some(result) => {
                println!("Found {} new types and {} modules", result.types.len(), result.modules.len());

                // Load existing cache and merge (accumulate from multiple SCIP files)
                let mut merged_types = semantic::load_std_types_cache();
                let mut merged_modules = semantic::load_std_modules_cache();
                let prev_types = merged_types.len();
                let prev_modules = merged_modules.len();

                merged_types.extend(result.types);
                merged_modules.extend(result.modules);

                println!("Merged: {} -> {} types, {} -> {} modules",
                         prev_types, merged_types.len(), prev_modules, merged_modules.len());

                // Save merged cache
                if let Err(e) = semantic::save_std_types_cache(&merged_types) {
                    eprintln!("Warning: Failed to save types cache: {}", e);
                }
                if let Err(e) = semantic::save_std_modules_cache(&merged_modules) {
                    eprintln!("Warning: Failed to save modules cache: {}", e);
                }

                // Print some statistics
                println!("\nTop 20 types:");
                let mut types: Vec<(&String, &String)> = merged_types.iter().collect();
                types.sort_by_key(|(k, _)| k.as_str());
                for (name, path) in types.iter().take(20) {
                    println!("  {} -> {}", name, path);
                }
            }
            None => {
                eprintln!("Failed to parse SCIP index or no std types found");
                std::process::exit(1);
            }
        }
        return;
    }

    // Stub when scip-analysis is disabled
    #[cfg(not(feature = "scip-analysis"))]
    if generate_std_types_path.is_some() {
        eprintln!("Error: --generate-std-types requires the 'scip-analysis' feature");
        eprintln!("Rebuild with: cargo build --release --features scip-analysis");
        std::process::exit(1);
    }

    // Determine target project root and manifest path
    let mut project_root = PathBuf::from(".");
    let manifest_path;

    if let Some(target) = target_arg {
        let path = PathBuf::from(&target);
        if path.is_dir() && path.join("Cargo.toml").exists() {
            // Case 2: Target is a folder
            project_root = path;
            manifest_path = Some(project_root.join("Cargo.toml"));
            println!("Target: Folder '{}'", project_root.display());
        } else {
            // Case 3: Target is a package name
            println!("Target: Package '{}'. Copying from registry...", target);
            if let Some(info) = crate_info::find_crate_source(&target, None) {
                let dest_dir = PathBuf::from(format!("{}_to_slice", target));

                // Preserve slicer cache when re-copying from registry
                let cache_dir = dest_dir.join(".slicer-cache");
                let temp_cache_dir = PathBuf::from(format!("{}_to_slice.slicer-cache-backup", target));
                let had_cache = if cache_dir.exists() {
                    // Move cache to temp location
                    let _ = fs::rename(&cache_dir, &temp_cache_dir);
                    true
                } else {
                    false
                };

                if dest_dir.exists() {
                    let _ = fs::remove_dir_all(&dest_dir);
                }
                if let Err(e) = slice_all::copy_dir_recursive(&info.path, &dest_dir) {
                    eprintln!("Error: Failed to copy crate source: {}", e);
                    std::process::exit(1);
                }

                // Restore cache
                if had_cache && temp_cache_dir.exists() {
                    let _ = fs::rename(&temp_cache_dir, &cache_dir);
                }
                
                // Add [workspace] to the copied Cargo.toml if missing to make it a workspace root
                let copied_toml_path = dest_dir.join("Cargo.toml");
                if let Ok(mut content) = fs::read_to_string(&copied_toml_path) {
                    if !content.contains("[workspace]") {
                        content.push_str("\n[workspace]\n");
                        let _ = fs::write(&copied_toml_path, content);
                    }
                }

                project_root = dest_dir;
                manifest_path = Some(project_root.join("Cargo.toml"));
                println!("Fell back to slicing on copied subfolder '{}'", project_root.display());
            } else {
                eprintln!("Error: Could not find package '{}' in registry", target);
                std::process::exit(1);
            }
        }
    } else {
        // Case 1: Default (current project)
        if Path::new("Cargo.toml").exists() {
            manifest_path = Some(PathBuf::from("Cargo.toml"));
            println!("Target: Current Project");
        } else {
            eprintln!("Error: No Cargo.toml found in current directory and no target specified.");
            std::process::exit(1);
        }
    }

    // Handle --self-upgrade mode
    if self_upgrade_mode {
        println!("=== Self-Slicing Flywheel ===");
        println!("Building cargo-slicer with its own sliced dependencies...\n");

        let sliced_manifest = project_root.join("Cargo.toml.sliced");
        if !sliced_manifest.exists() {
            eprintln!("Error: Cargo.toml.sliced not found.");
            eprintln!("Run './target/release/cargo-slicer' first to generate sliced dependencies.");
            std::process::exit(1);
        }

        // Step 1: Swap manifests and build with sliced deps
        println!("Step 1: Building with sliced dependencies...");
        let original_manifest = project_root.join("Cargo.toml");
        let backup_manifest = project_root.join("Cargo.toml.original");

        // Backup original and swap to sliced
        if let Err(e) = std::fs::copy(&original_manifest, &backup_manifest) {
            eprintln!("  ❌ Failed to backup Cargo.toml: {}", e);
            std::process::exit(1);
        }
        if let Err(e) = std::fs::copy(&sliced_manifest, &original_manifest) {
            eprintln!("  ❌ Failed to swap to sliced manifest: {}", e);
            let _ = std::fs::copy(&backup_manifest, &original_manifest);
            std::process::exit(1);
        }

        let build_output = std::process::Command::new("cargo")
            .arg("build")
            .arg("--release")
            .output();

        // Always restore original manifest after build
        let restore_result = std::fs::copy(&backup_manifest, &original_manifest);
        let _ = std::fs::remove_file(&backup_manifest);

        match build_output {
            Ok(output) if output.status.success() => {
                println!("  ✅ Build successful!");
            }
            Ok(output) => {
                eprintln!("  ❌ Build failed!");
                eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                if restore_result.is_err() {
                    eprintln!("  ⚠️  Warning: Could not restore Cargo.toml. Check Cargo.toml.original");
                }
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("  ❌ Failed to run cargo build: {}", e);
                std::process::exit(1);
            }
        }

        // Step 2: Run a quick test (slice regex)
        println!("\nStep 2: Testing sliced slicer...");
        let test_output = std::process::Command::new("./target/release/cargo-slicer")
            .arg("regex")
            .arg("-c")
            .output();

        match test_output {
            Ok(output) if output.status.success() => {
                println!("  ✅ Test passed!");
                println!("\n🎉 Self-slicing flywheel active!");
                println!("   The slicer is now built with its own sliced dependencies.");
            }
            Ok(output) => {
                eprintln!("  ❌ Test failed!");
                eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                eprintln!("\nRolling back: rebuild with original dependencies...");
                let _ = std::process::Command::new("cargo")
                    .arg("build")
                    .arg("--release")
                    .output();
                std::process::exit(1);
            }
            Err(e) => {
                eprintln!("  ❌ Failed to run test: {}", e);
                std::process::exit(1);
            }
        }

        std::process::exit(0);
    }

    // Handle --clean mode
    if clean_mode {
        let project_name = project_root.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");

        println!("=== Cleaning project: {} ===", project_name);

        // Clean Phase 2 cache files
        let cache_dir = PathBuf::from(".");
        if let Ok(entries) = fs::read_dir(&cache_dir) {
            for entry in entries.flatten() {
                if let Some(name) = entry.file_name().to_str() {
                    if name.starts_with(".phase2_cache_") && name.ends_with(".json") {
                        println!("  Removing cache file: {}", name);
                        let _ = fs::remove_file(entry.path());
                    }
                }
            }
        }

        // Clean source folders
        let crates_to_slice = PathBuf::from("crates_to_slice");
        if crates_to_slice.exists() {
            println!("  Removing directory: crates_to_slice/");
            if let Err(e) = fs::remove_dir_all(&crates_to_slice) {
                eprintln!("    Warning: Failed to remove crates_to_slice/: {}", e);
            }
        }

        // Clean sliced folder (both old generic name and project-specific name)
        let sliced_crates = PathBuf::from("sliced_crates");
        if sliced_crates.exists() {
            println!("  Removing directory: sliced_crates/");
            if let Err(e) = fs::remove_dir_all(&sliced_crates) {
                eprintln!("    Warning: Failed to remove sliced_crates/: {}", e);
            }
        }

        // Clean incremental slicer cache
        let slicer_cache = PathBuf::from(".slicer-cache");
        if slicer_cache.exists() {
            println!("  Removing directory: .slicer-cache/");
            if let Err(e) = fs::remove_dir_all(&slicer_cache) {
                eprintln!("    Warning: Failed to remove .slicer-cache/: {}", e);
            }
        }

        // Also clean project-specific sliced directory
        let project_sliced = PathBuf::from(format!("{}_sliced", project_name));
        if project_sliced.exists() {
            println!("  Removing directory: {}/", project_sliced.display());
            if let Err(e) = fs::remove_dir_all(&project_sliced) {
                eprintln!("    Warning: Failed to remove {}: {}", project_sliced.display(), e);
            }
        }

        // Clean log files
        let debug_log = PathBuf::from("cargo-slicer-debug.log");
        if debug_log.exists() {
            println!("  Removing log file: cargo-slicer-debug.log");
            let _ = fs::remove_file(&debug_log);
        }

        let error_log = PathBuf::from(format!("{}-build-errors.log", project_name));
        if error_log.exists() {
            println!("  Removing log file: {}", error_log.display());
            let _ = fs::remove_file(&error_log);
        }

        // Also clean the copied project folder if it was created
        let copied_project = PathBuf::from(format!("{}_to_slice", project_name));
        if copied_project.exists() && copied_project != project_root {
            println!("  Removing directory: {}/", copied_project.display());
            if let Err(e) = fs::remove_dir_all(&copied_project) {
                eprintln!("    Warning: Failed to remove {}: {}", copied_project.display(), e);
            }
        }

        println!("✓ Clean complete! Continuing with slicing...\n");
    }

    let project_src_dir = project_root.join("src");

    // Handle --watch mode
    if watch_mode {
        use cargo_slicer::watch::{check_watch_prerequisites, run_watch_mode, WatchConfig};

        // Check prerequisites
        if let Err(e) = check_watch_prerequisites(&project_root) {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }

        // Get sliceable crates from dependency graph
        let graph = match dep_graph::analyze_dependency_graph(manifest_path.as_deref()) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Error analyzing dependencies: {}", e);
                std::process::exit(1);
            }
        };

        // Collect sliceable crate names (normalized with underscores)
        let sliceable_crates: std::collections::HashSet<String> = graph.nodes.keys()
            .filter(|name| *name != &graph.root)
            .map(|name| name.replace('-', "_"))
            .collect();

        // Determine if release mode
        let build_release = passthrough_args.contains(&"--release".to_string());

        let config = WatchConfig {
            project_src: project_src_dir.clone(),
            project_root: project_root.clone(),
            sliceable_crates,
            build_release,
            features: features.clone(),
            debounce: std::time::Duration::from_millis(500),
        };

        println!("=== Watch Mode ===");
        if let Err(e) = run_watch_mode(config) {
            eprintln!("Watch mode error: {}", e);
            std::process::exit(1);
        }
        return;
    }

    // Standard mode: multi-crate slicing
    if crate_path.is_none() {
        let graph = match dep_graph::analyze_dependency_graph(manifest_path.as_deref()) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Error analyzing dependencies: {}", e);
                std::process::exit(1);
            }
        };

        if analyze_deps {
            dep_graph::print_dep_analysis(&graph);
            return;
        }

        if topo_order {
            println!("=== Topological Order (leaf-first) ===\n");
            for (i, name) in graph.topo_order().iter().enumerate() {
                if *name != graph.root {
                    if let Some(node) = graph.nodes.get(*name) {
                        println!("  {:3}. {} v{}", i + 1, name, node.version);
                    }
                }
            }
            return;
        }

        // Default to semantic mode for multi-crate if not specified
        if !_generate_crate && !_locate_crate && !_predict_mode {
            _use_ra_deps = true;
            _use_semantic = true;
            _generate_crate = true;
        }

        // Derive output directory name from project being sliced
        let project_name = project_root.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");
        let default_output = PathBuf::from(format!("{}_sliced", project_name));
        let output_base = output_dir.unwrap_or(default_output);

        // Clear output directory if it exists to avoid mixing results from different projects
        if output_base.exists() {
            println!("Cleaning output directory: {}...", output_base.display());
            let _ = fs::remove_dir_all(&output_base);
        }
        let _ = fs::create_dir_all(&output_base);

        // Cache logic - make default cache project-specific
        let cache_file = if cache_file == PathBuf::from(".phase2_cache.json") {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            project_root.hash(&mut hasher);
            let hash = hasher.finish();
            PathBuf::from(format!(".phase2_cache_{:x}.json", hash))
        } else {
            cache_file
        };

        // Determine cache behavior
        let (save_cache, load_cache) = if save_phase2_cache.is_some() || load_phase2_cache.is_some() {
            (save_phase2_cache.as_deref(), load_phase2_cache.as_deref())
        } else if use_cache {
            let load = if cache_file.exists() { Some(cache_file.as_path()) } else { None };
            (Some(cache_file.as_path()), load)
        } else {
            (None, None)
        };

        if log_decisions {
            std::env::set_var("CARGO_SLICER_LOG_DEPENDENCIES", "1");
        }

        // Enable profiling if requested
        if enable_profiling {
            cargo_slicer::profiling::set_profiling_enabled(true);
            println!("Profiling enabled");
        }

        // Track slicing time for benchmarks
        let slicing_start = std::time::Instant::now();

        let result = slice_all::union_slice_deps(
            &graph, &output_base, &project_src_dir, &project_root,
            auto_fix, parallel, profile_crate.as_deref(), abort_threshold,
            ra_timeout, show_progress, original_source, measure, force_all,
            save_cache, load_cache, use_new_slicer, verbose, registry_fallback,
            &features, use_cache
        );

        let slicing_time_secs = slicing_start.elapsed().as_secs_f64();

        if measure {
            cargo_slicer::measure::print_measurement_report(
                &graph, &result.measurements, use_unicode, Some(&output_base),
                result.total_all_deps_loc, result.total_all_deps_count,
            );
        }

        // Export profiling data if enabled
        if enable_profiling {
            let timing_data = cargo_slicer::profiling::get_timing_data();
            let report = cargo_slicer::profiling::ProfileReport::from_timing_data(timing_data);

            // Print summary to console
            report.print_summary();

            // Write to file
            let output_path = profile_output_path
                .unwrap_or_else(|| PathBuf::from("profile.json"));
            match report.write_to_file(&output_path) {
                Ok(_) => println!("\nProfile data written to: {}", output_path.display()),
                Err(e) => eprintln!("Error writing profile data: {}", e),
            }
        }

        // Auto-build the sliced variant
        println!("\n=== Building Sliced Variant ===");
        let sliced_manifest = project_root.join("Cargo.toml.sliced");
        if sliced_manifest.exists() {
            let original_manifest = project_root.join("Cargo.toml");
            let backup_manifest = project_root.join("Cargo.toml.bak");

            // Backup original and put sliced in place
            let _ = fs::rename(&original_manifest, &backup_manifest);
            let _ = fs::copy(&sliced_manifest, &original_manifest);

            // Collect all required features from cfg-feature analysis (O4)
            let all_features: std::collections::HashSet<String> = result.required_features
                .values()
                .flat_map(|v| v.iter().cloned())
                .collect();

            // Load the project's defined features from Cargo.toml
            let project_features = load_project_features(&project_root);

            // Filter to only features that the project actually supports
            let supported_features: Vec<String> = all_features.iter()
                .filter(|f| project_features.contains(*f))
                .cloned()
                .collect();

            // Build minimal feature flag if we have feature analysis data
            // Note: We ADD the detected features without disabling defaults
            // This is safer - user can pass --no-default-features explicitly if needed
            let extra_features = if !supported_features.is_empty() && features.cfg_feature_deps {
                // Only use minimal features if cfg-feature analysis is enabled
                let features_str = supported_features.join(",");
                println!("  🔧 Adding features from cfg analysis: {}", features_str);
                if !all_features.is_empty() {
                    let unsupported: Vec<_> = all_features.iter()
                        .filter(|f| !project_features.contains(*f))
                        .collect();
                    if !unsupported.is_empty() {
                        println!("  ℹ️  Skipped {} features not defined in project: {:?}",
                            unsupported.len(), unsupported.iter().take(5).collect::<Vec<_>>());
                    }
                }
                Some(features_str)
            } else {
                None
            };

            // Add passthrough args to the build command
            let display_args = if let Some(ref feat) = extra_features {
                if passthrough_args.is_empty() {
                    format!("build --features {}", feat)
                } else {
                    format!("build {} --features {}", passthrough_args.join(" "), feat)
                }
            } else if !passthrough_args.is_empty() {
                format!("build {}", passthrough_args.join(" "))
            } else {
                "build".to_string()
            };
            println!("Running: cargo {} in {}", display_args, project_root.display());

            let mut build_cmd = std::process::Command::new("cargo");
            build_cmd.arg("build");

            // Add detected features (without disabling defaults)
            if let Some(ref feat) = extra_features {
                build_cmd.arg("--features");
                build_cmd.arg(feat);
            }

            for arg in &passthrough_args {
                build_cmd.arg(arg);
            }
            build_cmd.current_dir(&project_root);

            let output = build_cmd.output();

            // Restore original manifest
            let _ = fs::rename(&backup_manifest, &original_manifest);

            match output {
                Ok(output) => {
                    // Extract project name from project_root for log filename
                    let project_name = project_root.file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("unknown");
                    let error_log_path = format!("{}-build-errors.log", project_name);

                    // Combine stdout and stderr
                    let full_output = String::from_utf8_lossy(&output.stdout).to_string()
                        + &String::from_utf8_lossy(&output.stderr);

                    // Filter for warnings and errors only
                    let mut diagnostics = Vec::new();
                    let mut in_diagnostic = false;
                    for line in full_output.lines() {
                        if line.contains("warning:") || line.contains("error:") || line.contains("error[E") {
                            in_diagnostic = true;
                            diagnostics.push(line);
                        } else if in_diagnostic {
                            // Include context lines that are indented or continuation lines
                            if line.starts_with("  ") || line.starts_with(" -->") || line.starts_with("   |") || line.starts_with("  =") {
                                diagnostics.push(line);
                            } else if line.trim().is_empty() {
                                // Keep empty lines within diagnostics
                                diagnostics.push(line);
                            } else if line.starts_with("   Compiling") || line.starts_with("    Finished") {
                                // Stop at next compilation message
                                in_diagnostic = false;
                            }
                        }
                    }

                    // Count warnings and errors
                    let warning_count = diagnostics.iter()
                        .filter(|l| l.starts_with("warning:") && !l.contains("generated"))
                        .count();
                    let error_count = diagnostics.iter()
                        .filter(|l| l.starts_with("error:") || l.starts_with("error[E"))
                        .count();

                    // Write to error log if there are any diagnostics
                    if !diagnostics.is_empty() {
                        if let Err(e) = fs::write(&error_log_path, diagnostics.join("\n")) {
                            eprintln!("Warning: Failed to write error log: {}", e);
                        } else {
                            println!("Build diagnostics written to: {}", error_log_path);
                            if warning_count > 0 || error_count > 0 {
                                println!("  ({} warnings, {} errors)", warning_count, error_count);
                            }
                        }
                    }

                    if output.status.success() {
                        println!("✓ Sliced build successful!");

                        // Run benchmarks if requested
                        if bench_mode {
                            let results = run_benchmarks(&project_root, slicing_time_secs);
                            print_benchmark_summary(&results);
                        }
                    } else {
                        println!("✗ Sliced build failed with exit code: {:?}", output.status.code());
                        std::process::exit(output.status.code().unwrap_or(1));
                    }
                }
                Err(e) => {
                    println!("✗ Failed to execute cargo build: {}", e);
                    std::process::exit(1);
                }
            }
        } else {
            println!("Warning: Cargo.toml.sliced not found at {}, skipping build.", sliced_manifest.display());
        }

        return;
    }

    // Standalone mode: direct crate path and items specified
    if let Some(path) = crate_path {
        if !seed_items.is_empty() {
            if !path.exists() {
                eprintln!("Error: crate path does not exist: {}", path.display());
                std::process::exit(1);
            }

            // Extract crate name from path
            let crate_name_from_path = path.file_name()
                .and_then(|n| n.to_str())
                .map(|s| {
                    if let Some(idx) = s.rfind('-').and_then(|i| {
                        if s[i+1..].chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                            Some(i)
                        } else {
                            None
                        }
                    }) {
                        s[..idx].to_string()
                    } else {
                        s.to_string()
                    }
                })
                .unwrap_or_else(|| "unknown".to_string());

            println!("Rust Crate Slicer (Standalone Mode)");
            println!("  Crate path: {}", path.display());
            println!("  Crate name: {}", crate_name_from_path);
            println!("  Seed items: {:?}", seed_items);

            let out_dir = output_dir.unwrap_or_else(|| PathBuf::from(format!("{}-sliced", crate_name_from_path)));
            println!("  Output dir: {}\n", out_dir.display());

            let edition = crate_info::parse_cargo_edition(&path).unwrap_or_else(|| "2021".to_string());

            let lib_path_opt = if path.join("src/lib.rs").exists() {
                Some(path.join("src/lib.rs"))
            } else if path.join("lib.rs").exists() {
                Some(path.join("lib.rs"))
            } else {
                None
            };
            let is_proc_macro = crate_info::is_proc_macro_crate(&path);
            let _crate_info = CrateInfo {
                name: crate_name_from_path.clone(),
                version: "0.0.0".to_string(),
                edition,
                path: path.clone(),
                lib_path: lib_path_opt,
                total_lines: 0,
                total_items: 0,
                is_proc_macro,
            };

            #[cfg(feature = "scip-analysis")]
            if _use_ra_deps {
                let result = semantic::generate_standalone_sliced_crate(&_crate_info, &seed_items, &out_dir, _use_ra_deps);
                match result {
                    Ok(r) => {
                        println!("\n=== Standalone Slice Complete ===");
                        println!("  Output: {}/", out_dir.display());
                        println!("  Items needed: {}", r.items_needed);
                        println!("  Items included: {}", r.items_included);
                        println!("  Lines generated: {}", r.lines_generated);

                        print!("  Checking compilation... ");
                        if auto_fix::check_crate_compiles(&out_dir) {
                            println!("SUCCESS!");
                        } else {
                            println!("FAILED (see errors above)");
                        }
                    }
                    Err(e) => {
                        eprintln!("Error generating sliced crate: {}", e);
                        std::process::exit(1);
                    }
                }
            } else {
                eprintln!("Standalone mode currently requires --ra-deps flag");
            }

            #[cfg(not(feature = "scip-analysis"))]
            {
                let _ = _use_ra_deps;  // suppress unused variable warning
                eprintln!("Standalone mode requires the 'scip-analysis' feature");
                eprintln!("Rebuild with: cargo build --release --features scip-analysis");
                std::process::exit(1);
            }
        }
    }
}

//! Cargo Slicer - Extract minimal subsets of Rust crates.
//!
//! This module provides functionality to analyze crate usage and generate
//! sliced versions containing only the items actually used.

#![allow(dead_code)]  // Many helper functions may be useful later

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

// Module declarations
pub mod types;
pub mod constants;
pub mod cfg;
pub mod arch;
pub mod usage;
pub mod crate_info;
pub mod parsing;
pub mod slicing;
pub mod codegen;
pub mod semantic;
pub mod source_fix;
pub mod auto_fix;
pub mod cargo_toml;
pub mod imports;
pub mod dep_graph;
pub mod slice_all;

// Re-exports for public API
pub use types::*;
pub use constants::*;
pub use crate_info::find_crate_source;
pub use usage::analyze_crate_usage;
pub use parsing::parse_crate;
pub use semantic::{generate_semantic_sliced_crate, generate_standalone_sliced_crate};
pub use dep_graph::analyze_dependency_graph;
pub use slice_all::{union_slice_deps, slice_all_deps, generate_workspace_toml};

fn print_usage() {
    eprintln!("Usage: cargo slicer [OPTIONS] <crate_name>");
    eprintln!("       cargo slicer --ra-deps --path <crate_path> --items <Item1,Item2,...> [-o <output_dir>]");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -l, --locate       Show crate source location and statistics");
    eprintln!("  -g, --generate     Generate sliced crate POC (stub implementations)");
    eprintln!("  -s, --semantic     Use syn-based semantic analysis (extracts real code)");
    eprintln!("  -m, --module-slice Use whole-module slicing (includes entire files)");
    eprintln!("  -a, --adaptive     Adaptive mode: try semantic first, fallback to module");
    eprintln!("  -p, --predict      Predict slicing potential without slicing");
    eprintln!("  -r, --ra-deps      Use rust-analyzer for accurate dependency analysis");
    eprintln!("  -o, --output DIR   Specify output directory for generated crate");
    eprintln!("  --path DIR         Standalone mode: crate source directory (requires --items)");
    eprintln!("  --items LIST       Standalone mode: comma-separated seed items (requires --path)");
    eprintln!("  --analyze-deps     Analyze full dependency graph");
    eprintln!("  --slice-all        Slice all direct dependencies (one-by-one)");
    eprintln!("  --union-slice      Union-slice mode (default): collect all items, run ra_deps, then slice");
    eprintln!("  --no-union-slice   Disable union-slice mode (for single-crate analysis)");
    eprintln!("  --auto-fix         Auto-generate stubs for missing types");
    eprintln!("  --profile-crate X  Profile timing on single crate X (uses union-slice mode)");
    eprintln!("  --no-parallel      Disable parallel processing (sequential mode)");
    eprintln!("  --passthrough      Enable passthrough fallback for failed slices");
    eprintln!("  --topo-order       Show dependencies in topological order");
    eprintln!("  --generate-std-types PATH  Generate std type mappings from SCIP index");
    eprintln!("  -h, --help         Show this help message");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  cargo slicer regex              # Analyze regex usage");
    eprintln!("  cargo slicer regex --locate     # Also show source statistics");
    eprintln!("  cargo slicer regex --generate   # Generate sliced crate (stubs)");
    eprintln!("  cargo slicer regex --semantic   # Generate with real source extraction");
    eprintln!("  cargo slicer rand --adaptive    # Try semantic, fallback to module if needed");
    eprintln!("  cargo slicer rusqlite --ra-deps # Use rust-analyzer for accurate deps");
    eprintln!();
    eprintln!("Standalone mode examples:");
    eprintln!("  cargo slicer --ra-deps --path ~/.cargo/registry/src/*/rustc-hash-2.1.1 --items FxHashMap,FxHashSet");
    eprintln!("  cargo slicer --ra-deps --path /path/to/crate --items Type1,Type2 -o /tmp/sliced");
    eprintln!("  cargo slicer --analyze-deps     # Show full dependency graph");
    eprintln!("  cargo slicer --slice-all -o sliced_crates  # Slice all deps (one-by-one)");
    eprintln!("  cargo slicer -o sliced           # Union-slice (default) to output dir");
    eprintln!("  cargo slicer --auto-fix -o sliced  # Union-slice + auto-fix");
    eprintln!("  cargo slicer --profile-crate regex  # Profile timing on 'regex' crate only");
    eprintln!("  cargo slicer --no-parallel       # Sequential processing");
}

pub fn main() {
    let args: Vec<String> = env::args().collect();

    let mut crate_name: Option<String> = None;
    let mut locate_crate = false;
    let mut generate_crate = false;
    let mut use_semantic = false;
    let mut use_module_slice = false;
    let mut use_adaptive = false;
    let mut use_ra_deps = false;
    let mut output_dir: Option<PathBuf> = None;
    let mut analyze_deps = false;
    let mut slice_all = false;
    let mut union_slice = true;  // Union-slice enabled by default
    let mut topo_order = false;
    let mut auto_fix = false;
    let mut predict_mode = false;
    let mut profile_crate: Option<String> = None;
    let mut parallel = true;  // Parallelization enabled by default
    let mut passthrough = false;  // Passthrough fallback disabled by default
    let mut generate_std_types_path: Option<PathBuf> = None;
    // Standalone mode: specify crate path and items directly
    let mut crate_path: Option<PathBuf> = None;
    let mut seed_items: Vec<String> = Vec::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--locate" | "-l" => locate_crate = true,
            "--generate" | "-g" => generate_crate = true,
            "--semantic" | "-s" => {
                use_semantic = true;
                generate_crate = true;
            }
            "--module-slice" | "-m" => {
                use_module_slice = true;
                use_semantic = true;
                generate_crate = true;
            }
            "--adaptive" | "-a" => {
                use_adaptive = true;
                use_semantic = true;
                generate_crate = true;
            }
            "--ra-deps" | "-r" => {
                use_ra_deps = true;
                use_semantic = true;
                generate_crate = true;
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
            "--slice-all" => {
                slice_all = true;
                use_semantic = true;  // Default to semantic for slice-all
            }
            "--union-slice" => {
                union_slice = true;
            }
            "--no-union-slice" => {
                union_slice = false;
            }
            "--auto-fix" => auto_fix = true,
            "--passthrough" => passthrough = true,
            "--no-passthrough" => passthrough = false,
            "--topo-order" => topo_order = true,
            "--predict" | "-p" => predict_mode = true,
            "--profile-crate" => {
                i += 1;
                if i < args.len() {
                    profile_crate = Some(args[i].clone());
                    union_slice = true;  // Profiling requires union-slice mode
                    use_ra_deps = true;
                    use_semantic = true;
                } else {
                    eprintln!("Error: --profile-crate requires a crate name");
                    std::process::exit(1);
                }
            }
            "--no-parallel" => parallel = false,
            "--generate-std-types" => {
                i += 1;
                if i < args.len() {
                    generate_std_types_path = Some(PathBuf::from(&args[i]));
                } else {
                    eprintln!("Error: --generate-std-types requires a SCIP index path");
                    std::process::exit(1);
                }
            }
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
            "--help" | "-h" => {
                print_usage();
                return;
            }
            s if !s.starts_with('-') => crate_name = Some(s.to_string()),
            _ => {
                eprintln!("Unknown option: {}", args[i]);
                print_usage();
                std::process::exit(1);
            }
        }
        i += 1;
    }

    // Handle --generate-std-types mode
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
                let mut types: Vec<_> = merged_types.iter().collect();
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

    // Handle dependency graph analysis modes
    if analyze_deps || topo_order || slice_all || union_slice {
        println!("Rust Crate Slicer - Multi-Crate Mode\n");

        let graph = match dep_graph::analyze_dependency_graph() {
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

        if slice_all {
            let output_base = output_dir.unwrap_or_else(|| PathBuf::from("sliced_crates"));
            fs::create_dir_all(&output_base).expect("Failed to create output directory");

            let result = slice_all::slice_all_deps(&graph, &output_base, use_semantic, auto_fix, use_module_slice, use_adaptive);

            // Generate workspace if any succeeded
            if !result.success.is_empty() {
                println!("\n=== Generating Workspace ===");
                if let Err(e) = slice_all::generate_workspace_toml(&output_base, &result.success) {
                    eprintln!("Warning: {}", e);
                }
            }

            return;
        }

        if union_slice {
            let output_base = output_dir.unwrap_or_else(|| PathBuf::from("sliced_crates"));
            fs::create_dir_all(&output_base).expect("Failed to create output directory");

            let result = slice_all::union_slice_deps(&graph, &output_base, auto_fix, parallel, passthrough, profile_crate.as_deref());

            // Generate workspace if any succeeded
            if !result.success.is_empty() {
                println!("\n=== Generating Workspace ===");
                if let Err(e) = slice_all::generate_workspace_toml(&output_base, &result.success) {
                    eprintln!("Warning: {}", e);
                }
            }

            return;
        }
    }

    // Standalone mode: direct crate path and items specified
    if crate_path.is_some() && !seed_items.is_empty() {
        let path = crate_path.unwrap();
        if !path.exists() {
            eprintln!("Error: crate path does not exist: {}", path.display());
            std::process::exit(1);
        }

        // Extract crate name from path (last component, or from Cargo.toml)
        let crate_name_from_path = path.file_name()
            .and_then(|n| n.to_str())
            .map(|s| {
                // Handle versioned directories like "rustc-hash-2.1.1"
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

        // Parse edition from Cargo.toml
        let edition = crate_info::parse_cargo_edition(&path).unwrap_or_else(|| "2021".to_string());

        // Create CrateInfo from path
        let lib_path_opt = if path.join("src/lib.rs").exists() {
            Some(path.join("src/lib.rs"))
        } else {
            None
        };
        let crate_info = CrateInfo {
            name: crate_name_from_path.clone(),
            version: "0.0.0".to_string(),  // Will be updated from Cargo.toml during slicing
            edition,
            path: path.clone(),
            lib_path: lib_path_opt,
            total_lines: 0,  // Will be counted during analysis
            total_items: 0,
        };

        // Use the semantic slicer with ra_deps
        if use_ra_deps {
            let result = semantic::generate_standalone_sliced_crate(&crate_info, &seed_items, &out_dir, use_ra_deps);
            match result {
                Ok(r) => {
                    println!("\n=== Standalone Slice Complete ===");
                    println!("  Output: {}/", out_dir.display());
                    println!("  Items needed: {}", r.items_needed);
                    println!("  Items included: {}", r.items_included);
                    println!("  Lines generated: {}", r.lines_generated);

                    // Check compilation
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
            std::process::exit(1);
        }

        return;
    }

    // Single crate mode (analyze project usage)
    let crate_name = crate_name.unwrap_or_else(|| "regex".to_string());

    println!("Rust Crate Slicer{}", if use_semantic { " (Semantic Mode)" } else { " - POC" });
    println!("Analyzing usage of '{}' crate in precc...\n", crate_name);

    let src_dir = Path::new("src");

    if !src_dir.exists() {
        eprintln!("Error: src directory not found");
        std::process::exit(1);
    }

    let used = usage::analyze_crate_usage(src_dir, &crate_name);

    if used.is_empty() {
        println!("No usage of '{}' crate found.", crate_name);
        return;
    }

    usage::generate_report(&used, &crate_name);

    // Phase 2: Locate crate source
    let crate_info_opt = crate_info::find_crate_source(&crate_name);

    if locate_crate || generate_crate {
        if let Some(ref info) = crate_info_opt {
            crate_info::generate_crate_report(info, used.len());
        } else {
            println!("\n=== Crate Source ===");
            println!("  Could not locate '{}' in cargo registry", crate_name);
            println!("  (Is it a dependency in Cargo.toml?)");
        }
    }

    // Predict mode: analyze slicing potential without slicing
    if predict_mode {
        if let Some(ref info) = crate_info_opt {
            slice_all::predict_slicing_potential(&info.path, &crate_name);
        } else {
            println!("\n  Cannot predict: crate source not found");
        }
        return;
    }

    // Phase 3/4: Generate sliced crate
    if generate_crate {
        if let Some(ref info) = crate_info_opt {
            let out_dir = output_dir.unwrap_or_else(|| PathBuf::from(format!("sliced_{}", crate_name)));

            if use_adaptive {
                // Adaptive mode: try semantic first, fallback to module if compilation fails
                println!("\n=== Adaptive Slicing ===");
                println!("  Trying semantic slicing first...");

                match semantic::generate_semantic_sliced_crate(info, &used, &out_dir) {
                    Ok(semantic_result) => {
                        println!("  Semantic slice generated ({} items)", semantic_result.items_included);
                        print!("  Checking compilation... ");

                        if auto_fix::check_crate_compiles(&out_dir) {
                            println!("SUCCESS!");
                            println!("\n=== Adaptive Slice Complete (Semantic) ===");
                            println!("  Output: {}/", out_dir.display());
                            println!("  Total parsed: {} items", semantic_result.total_parsed);
                            println!("  Items needed: {} (with transitive deps)", semantic_result.items_needed);
                            println!("  Items included: {}", semantic_result.items_included);
                            println!("  Lines generated: {}", semantic_result.lines_generated);
                            println!("\n  Created: {}/Cargo.toml", out_dir.display());
                            println!("  Created: {}/src/lib.rs", out_dir.display());
                        } else {
                            println!("FAILED");
                            println!("  Falling back to module slicing...");

                            // Remove the failed semantic output
                            let _ = fs::remove_dir_all(&out_dir);

                            match codegen::generate_module_sliced_crate(info, &used, &out_dir) {
                                Ok(module_result) => {
                                    print!("  Module slice generated ({} files), checking... ", module_result.items_included);
                                    if auto_fix::check_crate_compiles(&out_dir) {
                                        println!("SUCCESS!");
                                    } else {
                                        println!("still has errors");
                                    }
                                    println!("\n=== Adaptive Slice Complete (Module) ===");
                                    println!("  Output: {}/", out_dir.display());
                                    println!("  Total parsed: {} items", module_result.total_parsed);
                                    println!("  Items needed: {} (with transitive deps)", module_result.items_needed);
                                    println!("  Module files included: {}", module_result.items_included);
                                    println!("  Lines generated: {}", module_result.lines_generated);
                                    println!("\n  Created: {}/Cargo.toml", out_dir.display());
                                    println!("  Created: {}/src/lib.rs", out_dir.display());
                                }
                                Err(e) => {
                                    eprintln!("  Error: {}", e);
                                }
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("  Semantic slicing failed: {}", e);
                        println!("  Falling back to module slicing...");

                        match codegen::generate_module_sliced_crate(info, &used, &out_dir) {
                            Ok(module_result) => {
                                print!("  Module slice generated ({} files), checking... ", module_result.items_included);
                                if auto_fix::check_crate_compiles(&out_dir) {
                                    println!("SUCCESS!");
                                } else {
                                    println!("still has errors");
                                }
                                println!("\n=== Adaptive Slice Complete (Module) ===");
                                println!("  Output: {}/", out_dir.display());
                                println!("  Total parsed: {} items", module_result.total_parsed);
                                println!("  Items needed: {} (with transitive deps)", module_result.items_needed);
                                println!("  Module files included: {}", module_result.items_included);
                                println!("  Lines generated: {}", module_result.lines_generated);
                                println!("\n  Created: {}/Cargo.toml", out_dir.display());
                                println!("  Created: {}/src/lib.rs", out_dir.display());
                            }
                            Err(e) => {
                                eprintln!("  Error: {}", e);
                            }
                        }
                    }
                }
            } else if use_module_slice {
                // Use module-based slicing (whole files)
                match codegen::generate_module_sliced_crate(info, &used, &out_dir) {
                    Ok(result) => {
                        println!("\n=== Module-Based Slice Complete ===");
                        println!("  Output: {}/", out_dir.display());
                        println!("  Total parsed: {} items", result.total_parsed);
                        println!("  Items needed: {} (with transitive deps)", result.items_needed);
                        println!("  Module files included: {}", result.items_included);
                        println!("  Lines generated: {}", result.lines_generated);
                        println!("\n  Created: {}/Cargo.toml", out_dir.display());
                        println!("  Created: {}/src/lib.rs", out_dir.display());
                    }
                    Err(e) => {
                        eprintln!("  Error: {}", e);
                    }
                }
            } else if use_semantic {
                // Use syn-based semantic analysis
                let result = if use_ra_deps {
                    // Use rust-analyzer for accurate dependency analysis
                    semantic::generate_semantic_sliced_crate_with_ra_deps(info, &used, &out_dir)
                } else {
                    semantic::generate_semantic_sliced_crate(info, &used, &out_dir)
                };
                match result {
                    Ok(result) => {
                        println!("\n=== Semantic Slice Complete{} ===",
                            if use_ra_deps { " (rust-analyzer)" } else { "" });
                        println!("  Output: {}/", out_dir.display());
                        println!("  Total parsed: {} items", result.total_parsed);
                        println!("  Items needed: {} (with transitive deps)", result.items_needed);
                        println!("  Items included: {}", result.items_included);
                        println!("  Lines generated: {}", result.lines_generated);
                        println!("\n  Created: {}/Cargo.toml", out_dir.display());
                        println!("  Created: {}/src/lib.rs", out_dir.display());
                    }
                    Err(e) => {
                        eprintln!("  Error: {}", e);
                    }
                }
            } else {
                // Use stub-based generation
                println!("\n=== Generating Sliced Crate ===");
                println!("  Output: {}/", out_dir.display());

                match slice_all::generate_sliced_crate(info, &used, &out_dir) {
                    Ok(()) => {
                        println!("  Created: {}/Cargo.toml", out_dir.display());
                        println!("  Created: {}/src/lib.rs", out_dir.display());
                        println!("\n  Note: This is a POC with stub implementations.");
                        println!("  Use --semantic for real source extraction.");
                    }
                    Err(e) => {
                        eprintln!("  Error generating sliced crate: {}", e);
                    }
                }
            }
        }
    }
}

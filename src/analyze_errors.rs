#!/usr/bin/env rust-script
//! Analyze compilation errors in sliced crates and rank by frequency
//!
//! Usage:
//!   cargo build --release --bin analyze-errors
//!   ./target/release/analyze-errors [sliced_crates_dir]
//!
//! Or with rust-script:
//!   rust-script scripts/analyze_errors.rs [sliced_crates_dir]

use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

#[derive(Debug, Clone, Default)]
struct ErrorStats {
    code: String,
    description: String,
    count: usize,
    crates: Vec<String>,
    examples: Vec<String>,
}

#[derive(Debug, Default)]
struct CrateResult {
    name: String,
    total_errors: usize,
    error_codes: HashMap<String, usize>,
    success: bool,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let sliced_dir = args.get(1).map(|s| s.as_str()).unwrap_or("sliced_crates");

    println!("=== Sliced Crate Error Analyzer ===\n");
    println!("Analyzing: {}\n", sliced_dir);

    let sliced_path = Path::new(sliced_dir);
    if !sliced_path.exists() {
        eprintln!("Error: Directory '{}' does not exist", sliced_dir);
        eprintln!("Run the slicer first: cargo run -- <crate> -o {}", sliced_dir);
        std::process::exit(1);
    }

    // Find all sliced crate directories
    let mut crate_dirs: Vec<_> = fs::read_dir(sliced_path)
        .expect("Failed to read directory")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir() && e.file_name().to_string_lossy().ends_with("-sliced"))
        .collect();

    crate_dirs.sort_by_key(|e| e.file_name());

    if crate_dirs.is_empty() {
        eprintln!("No sliced crates found in '{}'", sliced_dir);
        std::process::exit(1);
    }

    println!("Found {} sliced crates\n", crate_dirs.len());

    // Analyze each crate
    let mut all_errors: HashMap<String, ErrorStats> = HashMap::new();
    let mut crate_results: Vec<CrateResult> = Vec::new();
    let mut success_count = 0;
    let mut fail_count = 0;

    for entry in &crate_dirs {
        let crate_path = entry.path();
        let crate_name = entry.file_name().to_string_lossy()
            .trim_end_matches("-sliced")
            .to_string();

        print!("  Building {}... ", crate_name);

        let output = Command::new("cargo")
            .args(["build", "--message-format=short"])
            .current_dir(&crate_path)
            .output()
            .expect("Failed to run cargo build");

        let stderr = String::from_utf8_lossy(&output.stderr);
        let mut result = CrateResult {
            name: crate_name.clone(),
            ..Default::default()
        };

        if output.status.success() {
            println!("OK");
            result.success = true;
            success_count += 1;
        } else {
            // Parse errors
            let errors = parse_errors(&stderr);
            result.total_errors = errors.len();

            for (code, desc, example) in errors {
                *result.error_codes.entry(code.clone()).or_insert(0) += 1;

                let stats = all_errors.entry(code.clone()).or_insert_with(|| ErrorStats {
                    code: code.clone(),
                    description: desc.clone(),
                    ..Default::default()
                });
                stats.count += 1;
                if !stats.crates.contains(&crate_name) {
                    stats.crates.push(crate_name.clone());
                }
                if stats.examples.len() < 3 && !stats.examples.contains(&example) {
                    stats.examples.push(example);
                }
            }

            println!("{} errors", result.total_errors);
            fail_count += 1;
        }

        crate_results.push(result);
    }

    // Print summary
    println!("\n{}", "=".repeat(70));
    println!("SUMMARY");
    println!("{}", "=".repeat(70));
    println!("\nCrates: {} total, {} success, {} failed ({:.1}% success rate)\n",
        crate_dirs.len(), success_count, fail_count,
        100.0 * success_count as f64 / crate_dirs.len() as f64);

    // Sort errors by frequency
    let mut error_list: Vec<_> = all_errors.values().collect();
    error_list.sort_by(|a, b| b.count.cmp(&a.count));

    println!("{}", "=".repeat(70));
    println!("ERROR RANKING BY FREQUENCY");
    println!("{}", "=".repeat(70));
    println!("\n{:<10} {:<8} {:<10} {}", "Code", "Count", "Crates", "Description");
    println!("{}", "-".repeat(70));

    for stats in &error_list {
        println!("{:<10} {:<8} {:<10} {}",
            stats.code,
            stats.count,
            stats.crates.len(),
            truncate(&stats.description, 40));
    }

    // Print detailed breakdown for top errors
    println!("\n{}", "=".repeat(70));
    println!("TOP 10 ERRORS - DETAILED");
    println!("{}", "=".repeat(70));

    for (i, stats) in error_list.iter().take(10).enumerate() {
        println!("\n{}. {} ({} occurrences in {} crates)",
            i + 1, stats.code, stats.count, stats.crates.len());
        println!("   Description: {}", stats.description);
        println!("   Affected crates: {}",
            stats.crates.iter().take(5).cloned().collect::<Vec<_>>().join(", "));
        if stats.crates.len() > 5 {
            println!("   ... and {} more", stats.crates.len() - 5);
        }
        if !stats.examples.is_empty() {
            println!("   Examples:");
            for example in stats.examples.iter().take(2) {
                println!("     - {}", truncate(example, 60));
            }
        }
    }

    // Print crates with most errors
    println!("\n{}", "=".repeat(70));
    println!("CRATES WITH MOST ERRORS");
    println!("{}", "=".repeat(70));

    let mut failed_crates: Vec<_> = crate_results.iter()
        .filter(|r| !r.success)
        .collect();
    failed_crates.sort_by(|a, b| b.total_errors.cmp(&a.total_errors));

    println!("\n{:<30} {:<10} {}", "Crate", "Errors", "Top Error Codes");
    println!("{}", "-".repeat(70));

    for result in failed_crates.iter().take(15) {
        let mut codes: Vec<_> = result.error_codes.iter().collect();
        codes.sort_by(|a, b| b.1.cmp(a.1));
        let top_codes: Vec<_> = codes.iter().take(3)
            .map(|(c, n)| format!("{}({})", c, n))
            .collect();
        println!("{:<30} {:<10} {}",
            result.name,
            result.total_errors,
            top_codes.join(", "));
    }

    // Generate actionable recommendations
    println!("\n{}", "=".repeat(70));
    println!("RECOMMENDATIONS");
    println!("{}", "=".repeat(70));

    let recommendations = generate_recommendations(&error_list);
    for (i, rec) in recommendations.iter().enumerate() {
        println!("\n{}. {}", i + 1, rec);
    }

    println!("\n");
}

fn parse_errors(stderr: &str) -> Vec<(String, String, String)> {
    let mut errors = Vec::new();

    for line in stderr.lines() {
        // Match error[E0XXX]: description
        if let Some(start) = line.find("error[E") {
            let after_error = &line[start + 7..]; // Skip "error[E" to get "0433]..."
            if let Some(end) = after_error.find(']') {
                let code = format!("E{}", &after_error[..end]);
                let desc_start = after_error.find("]: ").map(|i| i + 3).unwrap_or(end + 2);
                let description = after_error.get(desc_start..).unwrap_or("").to_string();
                let example = line.to_string();
                errors.push((code, description, example));
            }
        }
        // Match "error: cannot find macro" etc.
        else if line.starts_with("error: ") && !line.contains("could not compile") {
            let description = line.trim_start_matches("error: ").to_string();
            let code = if description.contains("cannot find macro") {
                "macro".to_string()
            } else if description.contains("aborting") {
                continue;
            } else {
                "other".to_string()
            };
            errors.push((code, description.clone(), line.to_string()));
        }
    }

    errors
}

fn truncate(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len - 3])
    }
}

fn generate_recommendations(errors: &[&ErrorStats]) -> Vec<String> {
    let mut recs = Vec::new();

    for stats in errors.iter().take(5) {
        let rec = match stats.code.as_str() {
            "E0433" => format!(
                "**E0433 (Unresolved module)**: {} occurrences\n   \
                 Fix: Add cfg-gated module detection. Check if modules like 'generic', \n   \
                 'packedpair' are conditionally compiled and include them based on target.",
                stats.count
            ),
            "E0599" => {
                if stats.description.contains("Ordering") || stats.description.contains("variant") {
                    format!(
                        "**E0599 (Wrong type/Missing method)**: {} occurrences\n   \
                         Fix: Improve SCIP-based import resolution to distinguish between\n   \
                         similarly-named types (e.g., std::cmp::Ordering vs std::sync::atomic::Ordering).",
                        stats.count
                    )
                } else {
                    format!(
                        "**E0599 (Missing method)**: {} occurrences\n   \
                         Fix: Include trait impl blocks that provide the missing methods.\n   \
                         Use SCIP to detect which impl blocks are needed.",
                        stats.count
                    )
                }
            }
            "E0425" | "E0531" => format!(
                "**{} (Missing enum variant/value)**: {} occurrences\n   \
                 Fix: When including an enum, include ALL its variants.\n   \
                 Parse enum definitions completely, not just referenced variants.",
                stats.code, stats.count
            ),
            "E0277" => format!(
                "**E0277 (Trait bound not satisfied)**: {} occurrences\n   \
                 Fix: Include derive macro implementations (Debug, Clone, etc.)\n   \
                 or the manual trait impl blocks for types that need them.",
                stats.count
            ),
            "E0432" => format!(
                "**E0432 (Unresolved import)**: {} occurrences\n   \
                 Fix: Check if imported items exist in the sliced output.\n   \
                 May need to include more items from re-export chains.",
                stats.count
            ),
            "E0412" => format!(
                "**E0412 (Cannot find type)**: {} occurrences\n   \
                 Fix: Add missing type definitions. Check if types are\n   \
                 defined in cfg-gated modules or via type aliases.",
                stats.count
            ),
            "E0254" => format!(
                "**E0254 (Duplicate definition)**: {} occurrences\n   \
                 Fix: Improve import deduplication in code generation.\n   \
                 Check for conflicting `use` statements.",
                stats.count
            ),
            "macro" => format!(
                "**Missing macros**: {} occurrences\n   \
                 Fix: Generate stub macros for common logging/debug macros\n   \
                 (trace!, debug!, info!, warn!, error!, debug_unreachable!).",
                stats.count
            ),
            _ => format!(
                "**{}**: {} occurrences in {} crates\n   \
                 Investigate: {}",
                stats.code, stats.count, stats.crates.len(),
                truncate(&stats.description, 50)
            ),
        };
        recs.push(rec);
    }

    recs
}

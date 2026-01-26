use indicatif::{ProgressBar, ProgressStyle};
use rayon::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
struct CrateResult {
    name: String,
    success: bool,
    error_count: usize,
    errors: Vec<String>,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let sliced_dir = std::env::current_dir()
        .expect("Failed to get current directory");

    println!("Checking compilation status of sliced crates...");
    println!("==================================================\n");

    // Find all *-sliced directories with Cargo.toml
    let mut crates: Vec<PathBuf> = fs::read_dir(&sliced_dir)
        .expect("Failed to read directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.is_dir()
                && path.file_name()?.to_str()?.ends_with("-sliced")
                && path.join("Cargo.toml").exists()
            {
                Some(path)
            } else {
                None
            }
        })
        .collect();

    // Sort for consistent ordering
    crates.sort_by(|a, b| a.file_name().cmp(&b.file_name()));

    // Filter by command-line arguments if provided
    if args.len() > 1 {
        if args[1] == "--help" || args[1] == "-h" {
            println!("Usage: check-all-crates [OPTIONS] [CRATE_NAMES...]");
            println!();
            println!("Options:");
            println!("  --limit N     Check only the first N crates");
            println!("  --help, -h    Show this help message");
            println!();
            println!("Examples:");
            println!("  check-all-crates                    # Check all crates");
            println!("  check-all-crates --limit 10         # Check first 10 crates");
            println!("  check-all-crates regex tokio        # Check only regex and tokio crates");
            return;
        } else if args[1] == "--limit" {
            if let Some(limit_str) = args.get(2) {
                if let Ok(limit) = limit_str.parse::<usize>() {
                    crates.truncate(limit);
                    println!("Limiting to first {} crates\n", limit);
                }
            }
        } else {
            // Filter to specified crate names
            let requested: Vec<String> = args[1..].iter().map(|s| s.to_string()).collect();
            crates.retain(|path| {
                let name = path.file_name().unwrap().to_str().unwrap();
                requested.iter().any(|req| {
                    name.contains(req) || name == &format!("{}-sliced", req)
                })
            });
            println!("Checking {} specified crate(s)\n", crates.len());
        }
    }

    let total = crates.len();
    if total == 0 {
        println!("No sliced crates found.");
        return;
    }

    // Setup progress bar
    let main_pb = ProgressBar::new(total as u64);
    main_pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} ({per_sec}) {msg}")
            .unwrap()
            .progress_chars("#>-"),
    );

    let results = Arc::new(Mutex::new(Vec::new()));
    let pb = Arc::new(main_pb);

    // Process crates in parallel
    crates.par_iter().for_each(|crate_path| {
        let crate_name = crate_path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let result = check_crate(crate_path);

        // Update progress with result
        let status = if result.success {
            "✓"
        } else {
            &format!("✗({})", result.error_count)
        };

        {
            let mut res = results.lock().unwrap();
            res.push(result.clone());
            pb.set_message(format!("{} {} | {}/{} done", status, crate_name, res.len(), total));
        }

        pb.inc(1);
    });

    pb.finish_with_message(format!("All {} crates checked", total));
    println!();

    // Sort results by name for consistent output
    let mut results = results.lock().unwrap().clone();
    results.sort_by(|a, b| a.name.cmp(&b.name));

    // Display individual results
    println!("Individual Results:");
    println!("==================");
    for (idx, result) in results.iter().enumerate() {
        if result.success {
            println!("[{}] {} ... ✓ SUCCESS", idx + 1, result.name);
        } else {
            println!(
                "[{}] {} ... ✗ FAILED ({} errors)",
                idx + 1,
                result.name,
                result.error_count
            );

            // Show top error types for this crate
            if !result.errors.is_empty() {
                let error_counts = count_error_types(&result.errors);
                let mut error_vec: Vec<_> = error_counts.iter().collect();
                error_vec.sort_by(|a, b| b.1.cmp(a.1));

                for (error_type, count) in error_vec.iter().take(5) {
                    println!("  {} {}", count, error_type);
                }
            }
        }
    }

    // Generate summary
    println!("\n==================================================");
    println!("Summary:");

    let success_count = results.iter().filter(|r| r.success).count();
    let failed_count = results.iter().filter(|r| !r.success).count();
    let total_errors: usize = results.iter().map(|r| r.error_count).sum();

    println!("  Total crates: {}", total);
    println!("  Success: {}", success_count);
    println!("  Failed: {}", failed_count);
    println!("  Total errors: {}", total_errors);

    if total > 0 {
        let success_rate = (success_count as f64 / total as f64) * 100.0;
        println!("  Success rate: {:.1}%", success_rate);
    }

    // Show failed crates
    if failed_count > 0 {
        println!("\nFailed Crates:");
        for result in results.iter().filter(|r| !r.success) {
            println!("  {}: {} errors", result.name, result.error_count);
        }

        // Aggregate all errors across crates
        let all_errors: Vec<String> = results
            .iter()
            .flat_map(|r| r.errors.clone())
            .collect();

        if !all_errors.is_empty() {
            println!("\nTop 10 Error Types (across all crates):");
            let error_counts = count_error_types(&all_errors);
            let mut error_vec: Vec<_> = error_counts.iter().collect();
            error_vec.sort_by(|a, b| b.1.cmp(a.1));

            for (error_type, count) in error_vec.iter().take(10) {
                println!("  {} {}", count, error_type);
            }
        }
    }
}

fn check_crate(crate_path: &PathBuf) -> CrateResult {
    let crate_name = crate_path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    let manifest_path = crate_path.join("Cargo.toml");

    let output = Command::new("cargo")
        .arg("build")
        .arg("--manifest-path")
        .arg(&manifest_path)
        .output();

    match output {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            let combined = format!("{}{}", stdout, stderr);

            if combined.contains("Finished") || output.status.success() {
                CrateResult {
                    name: crate_name,
                    success: true,
                    error_count: 0,
                    errors: Vec::new(),
                }
            } else {
                let errors: Vec<String> = combined
                    .lines()
                    .filter(|line| line.starts_with("error[E"))
                    .map(|s| s.to_string())
                    .collect();

                let error_count = errors.len();

                CrateResult {
                    name: crate_name,
                    success: false,
                    error_count,
                    errors,
                }
            }
        }
        Err(e) => {
            eprintln!("Failed to execute cargo for {}: {}", crate_name, e);
            CrateResult {
                name: crate_name,
                success: false,
                error_count: 0,
                errors: Vec::new(),
            }
        }
    }
}

fn count_error_types(errors: &[String]) -> HashMap<String, usize> {
    let mut counts = HashMap::new();

    for error in errors {
        // Extract error message after "error[EXXX]: "
        if let Some(msg_start) = error.find("]: ") {
            let error_msg = &error[msg_start + 3..];
            *counts.entry(error_msg.to_string()).or_insert(0) += 1;
        }
    }

    counts
}

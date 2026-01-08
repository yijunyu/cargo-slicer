//! check-slicer: Build, slice, and analyze workflow
//!
//! This command performs the full test workflow:
//! 1. Build cargo-slicer in release mode
//! 2. Clean sliced_crates directory
//! 3. Run cargo-slicer on a test crate (default: aho-corasick)
//! 4. Run analyze-errors to check the status
//!
//! Usage: check-slicer [CRATE_NAME]

use std::env;
use std::fs;
use std::path::Path;
use std::process::{Command, ExitCode};
use std::time::Instant;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let crate_name = args.get(1).map(|s| s.as_str()).unwrap_or("aho-corasick");

    println!("=== Cargo Slicer Check Workflow ===\n");

    let start = Instant::now();

    // Step 1: Build cargo-slicer (dev profile with opt-level=2 for fast builds + good runtime)
    println!("Step 1: Building cargo-slicer (dev)...");
    let build_start = Instant::now();
    let status = Command::new("cargo")
        .args(["build"])
        .status();

    match status {
        Ok(s) if s.success() => {
            println!("  Build completed in {:.2}s\n", build_start.elapsed().as_secs_f64());
        }
        Ok(s) => {
            eprintln!("  Build failed with exit code: {:?}", s.code());
            return ExitCode::FAILURE;
        }
        Err(e) => {
            eprintln!("  Failed to run cargo build: {}", e);
            return ExitCode::FAILURE;
        }
    }

    // Step 2: Clean sliced_crates directory
    println!("Step 2: Cleaning sliced_crates directory...");
    let sliced_crates_dir = Path::new("sliced_crates");
    if sliced_crates_dir.exists() {
        match fs::remove_dir_all(sliced_crates_dir) {
            Ok(_) => println!("  Removed sliced_crates/\n"),
            Err(e) => {
                eprintln!("  Warning: Failed to remove sliced_crates/: {}", e);
            }
        }
    } else {
        println!("  sliced_crates/ does not exist, skipping\n");
    }

    // Step 3: Run cargo-slicer
    println!("Step 3: Running cargo-slicer on '{}'...", crate_name);
    let slice_start = Instant::now();
    let status = Command::new("./target/debug/cargo-slicer")
        .arg(crate_name)
        .status();

    match status {
        Ok(s) if s.success() => {
            println!("  Slicing completed in {:.2}s\n", slice_start.elapsed().as_secs_f64());
        }
        Ok(s) => {
            eprintln!("  Slicing failed with exit code: {:?}", s.code());
            // Continue to analyze-errors anyway to see what happened
        }
        Err(e) => {
            eprintln!("  Failed to run cargo-slicer: {}", e);
            return ExitCode::FAILURE;
        }
    }

    // Step 4: Check if sliced_crates exists before running analyze-errors
    if !sliced_crates_dir.exists() {
        eprintln!("  Error: sliced_crates/ was not created");
        return ExitCode::FAILURE;
    }

    // Step 5: Run analyze-errors
    println!("Step 4: Running analyze-errors...");
    let analyze_start = Instant::now();
    let output = Command::new("./target/debug/analyze-errors")
        .arg("sliced_crates")
        .output();

    match output {
        Ok(out) => {
            // Print stdout
            let stdout = String::from_utf8_lossy(&out.stdout);
            print!("{}", stdout);

            // Print stderr if any
            if !out.stderr.is_empty() {
                let stderr = String::from_utf8_lossy(&out.stderr);
                eprint!("{}", stderr);
            }

            println!("\n  Analysis completed in {:.2}s", analyze_start.elapsed().as_secs_f64());
        }
        Err(e) => {
            eprintln!("  Failed to run analyze-errors: {}", e);
            return ExitCode::FAILURE;
        }
    }

    // Step 6: Also run cargo check on the workspace to verify it builds
    println!("\nStep 5: Verifying workspace builds...");
    let check_start = Instant::now();
    let output = Command::new("cargo")
        .args(["check"])
        .current_dir(sliced_crates_dir)
        .output();

    match output {
        Ok(out) => {
            if out.status.success() {
                println!("  Workspace builds successfully!");

                // Count warnings
                let stderr = String::from_utf8_lossy(&out.stderr);
                let warning_count = stderr.matches("warning:").count();
                if warning_count > 0 {
                    println!("  ({} warnings)", warning_count);
                }
            } else {
                let stderr = String::from_utf8_lossy(&out.stderr);
                let error_count = stderr.matches("error[E").count();
                println!("  Workspace has {} compilation errors", error_count);

                // Show first few errors
                for line in stderr.lines().filter(|l| l.contains("error[E")).take(5) {
                    println!("    {}", line);
                }
            }
            println!("  Check completed in {:.2}s", check_start.elapsed().as_secs_f64());
        }
        Err(e) => {
            eprintln!("  Failed to run cargo check: {}", e);
        }
    }

    println!("\n=== Total time: {:.2}s ===", start.elapsed().as_secs_f64());

    ExitCode::SUCCESS
}

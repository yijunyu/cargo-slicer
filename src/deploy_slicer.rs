//! deploy-slicer:
//! Deployment workflow for cargo-slicer
//!
//! This command performs the deployment workflow:
//! 1. Remove old binaries from ~/.cargo/bin
//! 2. Run deploy-demo from precc
//! 3. Run generate-demo-gifs
//! 4. Commit and push assets
//!
//! Usage: deploy-slicer

use std::env;
use std::fs;
use std::process::{Command, ExitCode};
use std::path::Path;

fn main() -> ExitCode {
    println!("=== Cargo Slicer Deployment Workflow ===\n");

    let home = env::var("HOME").expect("HOME environment variable not set");
    let cargo_bin = Path::new(&home).join(".cargo/bin");

    // Step 1: Remove old binaries
    println!("Step 1: Removing old binaries...");
    for bin in &["cargo-slicer", "ra-deps", "ra_deps"] {
        let bin_path = cargo_bin.join(bin);
        if bin_path.exists() {
            match fs::remove_file(&bin_path) {
                Ok(_) => println!("  Removed {}", bin_path.display()),
                Err(e) => eprintln!("  Warning: Failed to remove {}: {}", bin_path.display(), e),
            }
        }
    }
    println!();

    // Step 2: Run deploy-demo
    println!("Step 2: Running deploy-demo...");
    let status = Command::new("../precc/target/release/deploy-demo")
        .args(["--config", "deploy-cargo-slicer.toml"])
        .status();

    match status {
        Ok(s) if s.success() => println!("  deploy-demo completed successfully\n"),
        _ => {
            eprintln!("  Warning: deploy-demo failed or skipped\n");
            // Continue anyway
        }
    }

    // Step 3: Generate demo gifs
    println!("Step 3: Generating demo gifs...");
    let status = Command::new("../precc/target/release/generate-demo-gifs")
        .arg("README.md")
        .status();

    match status {
        Ok(s) if s.success() => println!("  generate-demo-gifs completed successfully\n"),
        _ => eprintln!("  Warning: generate-demo-gifs failed or skipped\n"),
    }

    // Step 4: Git commit and push
    println!("Step 4: Committing assets...");
    
    // git add assets
    let _ = Command::new("git").args(["add", "assets"]).status();
    
    // git commit -am "cargo slicer deployed"
    let status = Command::new("git")
        .args(["commit", "-am", "cargo slicer deployed"])
        .status();

    if let Ok(s) = status {
        if s.success() {
            println!("  Committed changes");
            
            // git push
            println!("  Pushing to remote...");
            let status = Command::new("git").arg("push").status();
            match status {
                Ok(s) if s.success() => println!("  Push successful\n"),
                _ => eprintln!("  Warning: Push failed\n"),
            }
        } else {
            println!("  No changes to commit or commit failed\n");
        }
    }

    // Step 5: Run deploy-demo again (to update the demo repo with pushed changes)
    println!("Step 5: Final deployment check...");
    let _ = Command::new("../precc/target/release/deploy-demo")
        .args(["--config", "deploy-cargo-slicer.toml"])
        .status();

    println!("\n=== Deployment workflow completed ===");
    ExitCode::SUCCESS
}

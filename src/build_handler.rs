use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

pub struct BuildHandler {
    manifest: &'static str,
    variant: &'static str,
    backup: &'static str,
    sliced_dir: &'static str,
}

impl BuildHandler {
    pub fn new() -> Self {
        // Auto-detect sliced directory: prefer {project_name}_sliced, fallback to sliced_crates
        let sliced_dir = Self::detect_sliced_dir().unwrap_or("sliced_crates");

        BuildHandler {
            manifest: "Cargo.toml",
            variant: "Cargo.toml.sliced",
            backup: ".Cargo.toml.backup",
            sliced_dir,
        }
    }

    /// Detect the sliced directory by looking for *_sliced directories
    fn detect_sliced_dir() -> Option<&'static str> {
        // Check if there's a {current_dir}_sliced directory
        if let Ok(current_dir) = std::env::current_dir() {
            if let Some(dir_name) = current_dir.file_name().and_then(|n| n.to_str()) {
                let project_sliced = format!("{}_sliced", dir_name);
                if Path::new(&project_sliced).exists() {
                    // Leak the string to get a 'static reference
                    // This is OK since we only call this once at startup
                    return Some(Box::leak(project_sliced.into_boxed_str()));
                }
            }
        }

        // Fallback: check if sliced_crates exists
        if Path::new("sliced_crates").exists() {
            return Some("sliced_crates");
        }

        None
    }

    /// Check if slicing results (Phase 4 and Phase 5) are cached
    fn check_slicing_cached(&self) -> bool {
        let variant_path = Path::new(self.variant);
        let sliced_dir = Path::new(self.sliced_dir);

        // Check if both Cargo.toml.sliced and sliced_crates/ exist
        variant_path.exists() && sliced_dir.exists() && sliced_dir.is_dir()
    }

    /// Run the full slicing process (Phase 0-5)
    fn run_slicing(&self) -> i32 {
        println!("Slicing results not found or outdated. Running full slicing process (Phase 0-5)...\n");

        // Use the local cargo-slicer binary if available, otherwise fall back to installed version
        // This ensures we use the latest code when running from the project directory
        let slicer_bin = std::env::current_exe()
            .ok()
            .unwrap_or_else(|| std::path::PathBuf::from("cargo-slicer"));

        let mut cmd = Command::new(&slicer_bin);
        // Use default flags: auto-fix, parallel, progress, etc.

        println!("Running: {} (union slice mode)\n", slicer_bin.display());

        let status = cmd.status();

        match status {
            Ok(s) => {
                let exit_code = s.code().unwrap_or(1);
                if exit_code != 0 {
                    eprintln!("\nError: Slicing process failed with exit code {}", exit_code);
                    return exit_code;
                }
                println!("\nSlicing complete. Proceeding with build...\n");
                0
            }
            Err(e) => {
                eprintln!("Error running cargo slicer: {}", e);
                1
            }
        }
    }

    pub fn handle_build_command(&self, cargo_args: &[String]) -> i32 {
        let manifest_path = Path::new(self.manifest);
        let variant_path = Path::new(self.variant);
        let backup_path = Path::new(self.backup);

        // 1. Validate
        if !manifest_path.exists() {
            eprintln!("Error: Cargo.toml not found in current directory");
            return 1;
        }

        // 2. Check if slicing results are cached, if not run slicing
        if !self.check_slicing_cached() {
            let slice_result = self.run_slicing();
            if slice_result != 0 {
                return slice_result;
            }

            // Verify slicing was successful
            if !variant_path.exists() {
                eprintln!("Error: Cargo.toml.sliced not generated after slicing.");
                return 1;
            }
        } else {
            println!("Using cached slicing results ({}/ and Cargo.toml.sliced)", self.sliced_dir);
        }

        // 3. Set up Ctrl-C handler for safe restore
        let restore_flag = Arc::new(AtomicBool::new(false));
        let restore_flag_clone = restore_flag.clone();

        let backup_str = self.backup.to_string();
        let manifest_str = self.manifest.to_string();

        if let Err(e) = ctrlc::set_handler(move || {
            if restore_flag_clone.load(Ordering::SeqCst) {
                eprintln!("\n\nInterrupted! Restoring Cargo.toml...");
                if let Err(e) = fs::copy(&backup_str, &manifest_str) {
                    eprintln!("ERROR restoring: {}", e);
                    eprintln!("Original saved at: {}", backup_str);
                } else {
                    let _ = fs::remove_file(&backup_str);
                    eprintln!("Cargo.toml restored.");
                }
            }
            std::process::exit(130); // SIGINT exit code
        }) {
            eprintln!("Warning: Could not set Ctrl-C handler: {}", e);
            eprintln!("Build will continue, but may not restore Cargo.toml on interruption.");
        }

        // 4. Backup original
        if let Err(e) = fs::copy(manifest_path, backup_path) {
            eprintln!("Error backing up Cargo.toml: {}", e);
            return 1;
        }

        // 5. Swap to variant
        if let Err(e) = fs::copy(variant_path, manifest_path) {
            eprintln!("Error installing Cargo.toml.sliced: {}", e);
            let _ = fs::copy(backup_path, manifest_path);
            let _ = fs::remove_file(backup_path);
            return 1;
        }

        restore_flag.store(true, Ordering::SeqCst);

        // 6. Run cargo build
        let mut cmd = Command::new("cargo");
        cmd.arg("build");
        cmd.args(cargo_args);

        println!("Building with sliced dependencies...");
        println!("Running: cargo build {}", cargo_args.join(" "));

        let status = cmd.status();

        // 7. Restore (ALWAYS)
        restore_flag.store(false, Ordering::SeqCst);

        if let Err(e) = fs::copy(backup_path, manifest_path) {
            eprintln!("\nCRITICAL: Failed to restore Cargo.toml: {}", e);
            eprintln!("Original saved at: {}", backup_path.display());
            return 1;
        }

        let _ = fs::remove_file(backup_path);
        println!("Cargo.toml restored.");

        // 8. Exit with cargo's exit code
        match status {
            Ok(s) => s.code().unwrap_or(1),
            Err(e) => {
                eprintln!("Error running cargo: {}", e);
                1
            }
        }
    }
}

impl Default for BuildHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_handler_creation() {
        let handler = BuildHandler::new();
        assert_eq!(handler.manifest, "Cargo.toml");
        assert_eq!(handler.variant, "Cargo.toml.sliced");
        assert_eq!(handler.backup, ".Cargo.toml.backup");
        // sliced_dir is now auto-detected, so just verify it's not empty
        assert!(!handler.sliced_dir.is_empty());
    }
}

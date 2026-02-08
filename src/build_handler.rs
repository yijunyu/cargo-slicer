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
    virtual_mode: bool,
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
            virtual_mode: false,
        }
    }

    /// Enable virtual slicing mode (no source modification)
    pub fn with_virtual_mode(mut self, enabled: bool) -> Self {
        self.virtual_mode = enabled;
        self
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

    /// Build with virtual slicing (no source modification)
    ///
    /// This uses RUSTC_WRAPPER to inject cargo-slicer-rustc, which skips
    /// unused items at codegen time without modifying any source files.
    fn handle_virtual_build(&self, cargo_args: &[String]) -> i32 {
        // Check if cross-crate mode is requested
        let cross_crate = std::env::var("CARGO_SLICER_CROSS_CRATE").is_ok();

        if cross_crate {
            return self.handle_cross_crate_build(cargo_args);
        }

        // Find cargo-slicer-rustc binary
        let rustc_driver = match self.find_rustc_driver() {
            Ok(driver) => driver,
            Err(e) => {
                eprintln!("Error: {}", e);
                eprintln!("Hint: Build with 'cargo +nightly build --profile release-rustc --bin cargo-slicer-rustc --features rustc-driver'");
                return 1;
            }
        };

        println!("Virtual slicing enabled (no source modification)");
        println!("Using rustc driver: {}", rustc_driver.display());

        // The rustc driver is built against nightly, so we need to use nightly toolchain
        // Check if user already specified a toolchain
        let has_toolchain = cargo_args.iter().any(|a| a.starts_with('+'));

        let (cargo_cmd, extra_args): (&str, Vec<&str>) = if has_toolchain {
            ("cargo", vec![])
        } else {
            // Use nightly by default for virtual slicing
            ("cargo", vec!["+nightly"])
        };

        // Build display string for logging
        let mut args_display = extra_args.to_vec();
        args_display.push("build");
        for arg in cargo_args {
            args_display.push(arg);
        }
        println!("Running: {} {}", cargo_cmd, args_display.join(" "));

        // Run cargo build with RUSTC_WRAPPER
        let mut cmd = Command::new(cargo_cmd);
        for arg in &extra_args {
            cmd.arg(arg);
        }
        cmd.arg("build");
        cmd.args(cargo_args);
        cmd.env("RUSTC_WRAPPER", &rustc_driver);
        cmd.env("CARGO_SLICER_VIRTUAL", "1");

        // Inject linker flags for dead code elimination
        let mut rustflags = std::env::var("RUSTFLAGS").unwrap_or_default();
        if !rustflags.contains("--gc-sections") {
            rustflags.push_str(" -C link-arg=-Wl,--gc-sections");
        }
        if !rustflags.contains("--icf") {
            rustflags.push_str(" -C link-arg=-Wl,--icf=all");
        }
        if !rustflags.contains("function-sections") {
            rustflags.push_str(" -C link-arg=-ffunction-sections -C link-arg=-fdata-sections");
        }

        // Set up version script directory for per-crate linker scripts
        let vscript_dir = std::env::temp_dir().join("cargo-slicer-vscripts");
        let _ = fs::create_dir_all(&vscript_dir);
        cmd.env("CARGO_SLICER_VSCRIPT_DIR", &vscript_dir);

        // If a combined version script exists from a previous run, use it
        let combined_vscript = vscript_dir.join("combined.ld");
        if combined_vscript.exists() && !rustflags.contains("--version-script") {
            rustflags.push_str(&format!(
                " -C link-arg=-Wl,--version-script={}",
                combined_vscript.display()
            ));
        }

        cmd.env("RUSTFLAGS", rustflags.trim());

        let status = cmd.status();

        match status {
            Ok(s) => {
                let exit_code = s.code().unwrap_or(1);
                if exit_code == 0 {
                    println!("\nVirtual slicing build complete.");
                }
                exit_code
            }
            Err(e) => {
                eprintln!("Error running cargo: {}", e);
                1
            }
        }
    }

    /// Cross-crate virtual slicing: two-phase build.
    ///
    /// Phase 1: Pre-analysis with syn → writes .analysis files, runs cross-crate BFS → .seeds files
    /// Phase 2: `cargo build` with driver → loads .seeds files for restricted BFS seeds
    fn handle_cross_crate_build(&self, cargo_args: &[String]) -> i32 {
        // Find driver and dispatch binaries
        let rustc_driver = match self.find_rustc_driver() {
            Ok(driver) => driver,
            Err(e) => {
                eprintln!("Error: {}", e);
                eprintln!("Hint: Build with 'cargo +nightly build --profile release-rustc --bin cargo-slicer-rustc --bin cargo_slicer_dispatch --features rustc-driver'");
                return 1;
            }
        };

        let dispatch_binary = match self.find_dispatch_binary() {
            Ok(dispatch) => dispatch,
            Err(e) => {
                eprintln!("Error: {}", e);
                eprintln!("Hint: Build with 'cargo +nightly build --profile release-rustc --bin cargo_slicer_dispatch --features rustc-driver'");
                return 1;
            }
        };

        println!("Cross-crate virtual slicing enabled");
        println!("Using driver: {}", rustc_driver.display());
        println!("Using dispatch: {}", dispatch_binary.display());

        let has_toolchain = cargo_args.iter().any(|a| a.starts_with('+'));
        let extra_args: Vec<&str> = if has_toolchain { vec![] } else { vec!["+nightly"] };

        // ── Phase 1: Pre-analysis (syn parsing + cross-crate BFS) ──
        println!("\n[Phase 1/2] Running pre-analysis...");
        let workspace_root = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."));
        match crate::pre_analyze::run_pre_analysis(&workspace_root) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("Pre-analysis failed: {}", e);
                return 1;
            }
        }

        // ── Phase 2: Build with driver (loads .seeds files for restricted seeds) ──
        println!("[Phase 2/2] Building with cross-crate seeds...");
        let mut build_cmd = Command::new("cargo");
        for arg in &extra_args {
            build_cmd.arg(arg);
        }
        build_cmd.arg("build");
        build_cmd.args(cargo_args);
        build_cmd.env("RUSTC_WRAPPER", &dispatch_binary);
        build_cmd.env("CARGO_SLICER_DRIVER", &rustc_driver);
        build_cmd.env("CARGO_SLICER_VIRTUAL", "1");
        build_cmd.env("CARGO_SLICER_CODEGEN_FILTER", "1");
        if let Ok(cache) = std::env::var("CARGO_SLICER_CACHE_DIR") {
            build_cmd.env("CARGO_SLICER_CACHE_DIR", cache);
        }

        // Inject linker flags
        let mut rustflags = std::env::var("RUSTFLAGS").unwrap_or_default();
        if !rustflags.contains("--gc-sections") {
            rustflags.push_str(" -C link-arg=-Wl,--gc-sections");
        }
        if !rustflags.contains("--icf") {
            rustflags.push_str(" -C link-arg=-Wl,--icf=all");
        }
        if !rustflags.contains("function-sections") {
            rustflags.push_str(" -C link-arg=-ffunction-sections -C link-arg=-fdata-sections");
        }
        build_cmd.env("RUSTFLAGS", rustflags.trim());

        match build_cmd.status() {
            Ok(s) => {
                let exit_code = s.code().unwrap_or(1);
                if exit_code == 0 {
                    println!("\nCross-crate virtual slicing build complete.");
                }
                exit_code
            }
            Err(e) => {
                eprintln!("Error running cargo build: {}", e);
                1
            }
        }
    }

    /// Find the cargo-slicer-rustc binary
    fn find_rustc_driver(&self) -> Result<std::path::PathBuf, String> {
        // 1. Check same directory as current exe (handles installed case)
        if let Ok(exe) = std::env::current_exe() {
            if let Some(dir) = exe.parent() {
                let driver = dir.join("cargo-slicer-rustc");
                if driver.exists() {
                    return Ok(driver);
                }
            }
        }

        // 2. Check relative to exe's parent directory (handles cargo workspace builds)
        // Do this BEFORE PATH check to prefer local builds over installed versions
        if let Ok(exe) = std::env::current_exe() {
            if let Some(dir) = exe.parent() {
                // If exe is in target/release, check target/release-rustc first, then target/debug
                if dir.ends_with("release") {
                    if let Some(target_dir) = dir.parent() {
                        // Prefer release-rustc (optimized) over debug
                        let release_rustc = target_dir.join("release-rustc").join("cargo-slicer-rustc");
                        if release_rustc.exists() {
                            return Ok(release_rustc);
                        }

                        let debug_driver = target_dir.join("debug").join("cargo-slicer-rustc");
                        if debug_driver.exists() {
                            return Ok(debug_driver);
                        }
                    }
                }
            }
        }

        // 3. Check common locations relative to current directory
        let common_paths = [
            "./target/release-rustc/cargo-slicer-rustc",  // Custom profile for rustc driver
            "./target/release/cargo-slicer-rustc",
            "./target/debug/cargo-slicer-rustc",
        ];

        for path in &common_paths {
            let expanded = if path.starts_with("~/") {
                if let Some(home) = std::env::var_os("HOME") {
                    let mut p = std::path::PathBuf::from(home);
                    p.push(&path[2..]);
                    p
                } else {
                    continue;
                }
            } else {
                std::path::PathBuf::from(path)
            };

            if expanded.exists() {
                return Ok(expanded);
            }
        }

        // 4. Last resort: check PATH (may find older installed version)
        if let Ok(output) = Command::new("which")
            .arg("cargo-slicer-rustc")
            .output()
        {
            if output.status.success() {
                let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
                if !path.is_empty() {
                    return Ok(std::path::PathBuf::from(path));
                }
            }
        }

        Err("cargo-slicer-rustc not found. Ensure it is built and in PATH or same directory as cargo-slicer.".to_string())
    }

    /// Find the cargo_slicer_dispatch binary (thin RUSTC_WRAPPER)
    fn find_dispatch_binary(&self) -> Result<std::path::PathBuf, String> {
        // Check env var first (explicit override)
        if let Ok(path) = std::env::var("CARGO_SLICER_DISPATCH") {
            let p = std::path::PathBuf::from(&path);
            if p.exists() {
                return Ok(p);
            }
        }

        // Check same directory as current exe
        if let Ok(exe) = std::env::current_exe() {
            if let Some(dir) = exe.parent() {
                let dispatch = dir.join("cargo_slicer_dispatch");
                if dispatch.exists() {
                    return Ok(dispatch);
                }
            }
        }

        // Check common locations
        let common_paths = [
            "./target/release-rustc/cargo_slicer_dispatch",
            "./target/release/cargo_slicer_dispatch",
            "./target/debug/cargo_slicer_dispatch",
        ];

        for path in &common_paths {
            let p = std::path::PathBuf::from(path);
            if p.exists() {
                return Ok(p);
            }
        }

        Err("cargo_slicer_dispatch not found. Build with: cargo +nightly build --profile release-rustc --bin cargo_slicer_dispatch --features rustc-driver".to_string())
    }

    pub fn handle_build_command(&self, cargo_args: &[String]) -> i32 {
        // If virtual mode is enabled, use virtual slicing path
        if self.virtual_mode {
            return self.handle_virtual_build(cargo_args);
        }

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
        assert!(!handler.virtual_mode);
    }

    #[test]
    fn test_build_handler_virtual_mode() {
        let handler = BuildHandler::new().with_virtual_mode(true);
        assert!(handler.virtual_mode);
    }
}

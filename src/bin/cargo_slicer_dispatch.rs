// When built with --features rustc-driver, the library crate transitively
// links rustc_driver. The rustc_private feature must be declared in every
// crate root in the link graph to enable -Z prefer_deps_of_dynamic, which
// prevents duplicate std linking errors (especially on Windows MSVC).
#![cfg_attr(feature = "rustc-driver", feature(rustc_private))]

//! Thin dispatch wrapper for cargo-slicer virtual slicing.
//!
//! This binary is used as RUSTC_WRAPPER instead of cargo-slicer-rustc.
//! It does NOT link rustc_driver, so it loads in <1ms. It inspects the
//! compiler args to decide:
//!   - Non-local crates (from .cargo/registry): exec via sccache or real rustc
//!   - Probe commands (-vV, --print): exec real rustc directly
//!   - Local crates with CARGO_SLICER_VIRTUAL: exec cargo-slicer-rustc driver
//!
//! This eliminates the ~300ms dynamic linking overhead of loading
//! librustc_driver.so for every non-local crate compilation.

use std::env;
use std::path::PathBuf;
use std::process::Command;

use cargo_slicer::process_ext::exec_command;

/// Find sccache binary without spawning `which` (fast path).
fn find_sccache() -> Option<PathBuf> {
    let sccache_name = format!("sccache{}", std::env::consts::EXE_SUFFIX);

    // 1. Explicit env var (set to empty or /nonexistent to disable)
    if let Ok(path) = env::var("CARGO_SLICER_SCCACHE") {
        let p = PathBuf::from(&path);
        if !p.exists() {
            return None; // Disabled or invalid path
        }
        return Some(p);
    }

    // 2. Check common paths directly (no process spawn)
    #[cfg(unix)]
    {
        let candidates = [
            "/usr/bin/sccache",
            "/usr/local/bin/sccache",
        ];
        for path in candidates {
            if std::fs::metadata(path).is_ok() {
                return Some(PathBuf::from(path));
            }
        }
    }

    // 3. ~/.cargo/bin/sccache (HOME on Unix, USERPROFILE on Windows)
    let home = env::var("HOME").or_else(|_| env::var("USERPROFILE")).ok();
    if let Some(home) = home {
        let p = PathBuf::from(home).join(".cargo").join("bin").join(&sccache_name);
        if p.exists() {
            return Some(p);
        }
    }

    None
}

fn exec_with_sccache(sccache: &PathBuf, rustc: &str, args: &[String]) {
    exec_command(Command::new(sccache).arg(rustc).args(args));
}

/// Fast path for non-local crates: exec rustc directly with minimal overhead.
/// Skips sccache lookup, env var checks, and all local-crate logic.
/// Only checks sccache if explicitly configured (not the default disabled path).
fn exec_passthrough(real_rustc: &str, compiler_args: &[String]) -> ! {
    // Check sccache only if explicitly enabled (env var set to a real path).
    // When CARGO_SLICER_SCCACHE is unset or set to /nonexistent, skip entirely.
    if let Ok(path) = env::var("CARGO_SLICER_SCCACHE") {
        if !path.is_empty() && path != "/nonexistent" {
            let p = PathBuf::from(&path);
            if p.exists() {
                exec_command(Command::new(&p).arg(real_rustc).args(compiler_args));
            }
        }
        // Sccache disabled or not found → fall through to direct rustc
    } else {
        // No CARGO_SLICER_SCCACHE env var → check common paths only if
        // sccache might be useful (not in benchmark mode)
        if let Some(sccache) = find_sccache() {
            exec_with_sccache(&sccache, real_rustc, compiler_args);
        }
    }
    exec_command(Command::new(real_rustc).args(compiler_args));
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // RUSTC_WRAPPER protocol: args[0]=us, args[1]=real_rustc, args[2..]=compiler_args
    if args.len() < 2 {
        eprintln!("cargo-slicer-dispatch: not enough arguments");
        std::process::exit(1);
    }

    let real_rustc = &args[1];
    let compiler_args = &args[2..];

    // Single-pass arg scan: detect probe, non-local, opt-level, crate-name/type.
    // For non-local crates (~94% of invocations), we exit immediately without
    // any env var lookups or sccache checks.
    let mut is_probe = false;
    let mut is_non_local = false;
    let mut has_explicit_opt_level = false;
    let mut crate_name_idx: Option<usize> = None;
    let mut crate_type_idx: Option<usize> = None;
    let mut prev_was_c = false;
    let mut prev_was_crate_name = false;
    let mut prev_was_crate_type = false;

    for (i, arg) in compiler_args.iter().enumerate() {
        // Track two-arg flags from previous iteration
        if prev_was_c {
            prev_was_c = false;
            if arg.starts_with("opt-level=") {
                has_explicit_opt_level = true;
            }
        }
        if prev_was_crate_name {
            prev_was_crate_name = false;
            crate_name_idx = Some(i);
        }
        if prev_was_crate_type {
            prev_was_crate_type = false;
            crate_type_idx = Some(i);
        }

        // Probe commands: exec rustc immediately
        if arg == "-vV" || arg == "-V" || arg == "--version" || arg.starts_with("--print") {
            is_probe = true;
            break;
        }

        // Non-local detection: source file path contains registry/git/vendor
        // Normalize backslashes for Windows path compatibility
        if !arg.starts_with('-') && arg.len() > 3 && arg.ends_with(".rs") {
            let normalized = arg.replace('\\', "/");
            if normalized.contains(".cargo/registry") || normalized.contains(".cargo/git") || normalized.contains("/vendor/") {
                is_non_local = true;
                // Don't break - we still need opt-level for trace mode
                // But if trace is unlikely, we could break early.
                // We'll check after the loop.
            }
        }

        // Two-arg flags
        if arg == "-C" {
            prev_was_c = true;
        } else if arg.starts_with("-Copt-level=") {
            has_explicit_opt_level = true;
        } else if arg == "--crate-name" {
            prev_was_crate_name = true;
        } else if arg == "--crate-type" {
            prev_was_crate_type = true;
        }
    }

    // Fast path 1: probe commands → exec rustc immediately
    if is_probe {
        exec_command(Command::new(real_rustc).args(compiler_args));
    }

    // Fast path 2: non-local crates → exec rustc directly.
    // Skip ALL env var checks, sccache lookups, skip-driver logic, etc.
    // This saves ~0.5-1ms per invocation × 600+ non-local crates.
    if is_non_local {
        // Only check sccache/trace if env vars are set (rare in benchmarks)
        exec_passthrough(real_rustc, compiler_args);
    }

    // CARGO_SLICER_DEFERRED=1 is the unified entry point for deferred compilation.
    // It implies VIRTUAL + CODEGEN_FILTER + LAZY (and optionally MIR_PRECISE).
    // Pass 1 emits metadata only (--emit=dep-info,metadata, no LLVM codegen).
    // Pass 2 runs selective codegen before binary compilation (sequential).
    let deferred = env::var("CARGO_SLICER_DEFERRED").is_ok();
    if deferred {
        // Set implied env vars so downstream code (driver, Pass 2) sees them
        env::set_var("CARGO_SLICER_VIRTUAL", "1");
        env::set_var("CARGO_SLICER_CODEGEN_FILTER", "1");
        env::set_var("CARGO_SLICER_LAZY", "1");
    }

    let virtual_enabled = env::var("CARGO_SLICER_VIRTUAL").is_ok();
    let lazy_enabled = env::var("CARGO_SLICER_LAZY").is_ok();

    // Detect build-dependency compilations: Cargo compiles crates used by build scripts
    // with default opt-level (0, no explicit flag) while target deps always get an explicit
    // -C opt-level=N (where N is 1/2/3/s/z depending on the profile).
    // Build deps must NOT be stubbed because their code actually runs during the build.
    let is_build_dep = !has_explicit_opt_level;

    // Helper closures for extracted fields
    let crate_name_str = || -> Option<String> {
        crate_name_idx.map(|i| compiler_args[i].clone())
    };
    let crate_type_str = || -> String {
        crate_type_idx.map(|i| compiler_args[i].clone()).unwrap_or_else(|| "unknown".to_string())
    };

    // Trace logging (only if CARGO_SLICER_TRACE is set)
    if env::var("CARGO_SLICER_TRACE").is_ok() {
        let cn = crate_name_str().unwrap_or_default();
        let ct = crate_type_str();
        eprintln!("[dispatch] crate={} type={} local={} virtual={} build_dep={} lazy={} deferred={} opt_level={}",
            cn, ct, true, virtual_enabled, is_build_dep, lazy_enabled, deferred, has_explicit_opt_level);
    }

    // Non-virtual or build deps: use sccache or direct rustc
    if !virtual_enabled || is_build_dep {
        if let Some(sccache) = find_sccache() {
            exec_with_sccache(&sccache, real_rustc, compiler_args);
        }
        exec_command(Command::new(real_rustc).args(compiler_args));
    }

    // Skip-driver optimization: if pre-analysis determined this crate needs no stubs,
    // skip the driver entirely and use plain rustc (or sccache).
    // Check .skip-driver marker first (single stat() syscall), then fall back to
    // the more expensive all-items-marked-in-cache check for build phase.
    // Uses crate_name/type from the single-pass scan above.
    if let Some(crate_name) = crate_name_str() {
        let crate_type = crate_type_str();
        let cache_dir = env::var("CARGO_SLICER_CACHE_DIR")
            .unwrap_or_else(|_| ".slicer-cache".to_string());
        let skip_path = format!("{}/{}-{}.skip-driver", cache_dir, crate_name, crate_type);

        if std::fs::metadata(&skip_path).is_ok() {
            // All items reachable → no stubs needed → skip driver
            if let Some(sccache) = find_sccache() {
                exec_with_sccache(&sccache, real_rustc, compiler_args);
            }
            exec_command(Command::new(real_rustc).args(compiler_args));
        }

        // Legacy build-phase check: if a local library crate has no items to stub,
        // skip the driver entirely and use plain rustc (or sccache).
        let phase = env::var("CARGO_SLICER_PHASE").unwrap_or_default();
        if phase == "build" && (crate_type == "lib" || crate_type == "rlib") {
            let cache_path = format!("{}/{}-{}.cache", cache_dir, crate_name, crate_type);

            if let Some(analysis_path_str) = find_analysis_file(&cache_dir, &crate_name) {
                if all_items_marked_in_cache(&cache_path, &analysis_path_str) {
                    if let Some(sccache) = find_sccache() {
                        exec_with_sccache(&sccache, real_rustc, compiler_args);
                    }
                    exec_command(Command::new(real_rustc).args(compiler_args));
                }
            }
        }
    }

    // Try fork-server daemon for faster driver invocation (Unix only)
    #[cfg(unix)]
    if env::var("CARGO_SLICER_DAEMON").is_ok() {
        let cache_dir = env::var("CARGO_SLICER_CACHE_DIR")
            .unwrap_or_else(|_| ".slicer-cache".to_string());
        let sock_path = format!("{}/driver.sock", cache_dir);

        // Start daemon if not running
        if std::fs::metadata(&sock_path).is_err() {
            let driver_path = env::var("CARGO_SLICER_DRIVER")
                .unwrap_or_else(|_| {
                    env::current_exe().ok()
                        .and_then(|p| p.parent().map(|d| d.join("cargo-slicer-rustc").to_string_lossy().to_string()))
                        .unwrap_or_else(|| "cargo-slicer-rustc".to_string())
                });
            start_daemon(&driver_path, &sock_path, &cache_dir);
        }

        // Try connecting to daemon
        if let Ok(exit_code) = try_daemon_compile(&sock_path, &args[1..]) {
            std::process::exit(exit_code);
        }
        // Fall through to direct exec on failure
    }

    // Try named-pipe daemon for faster driver invocation (Windows only)
    #[cfg(windows)]
    if env::var("CARGO_SLICER_DAEMON").is_ok() {
        let cache_dir = env::var("CARGO_SLICER_CACHE_DIR")
            .unwrap_or_else(|_| ".slicer-cache".to_string());
        let pipe_name = get_pipe_path(&cache_dir);

        // Start daemon if not running (check PID + pipe liveness)
        ensure_daemon_running_windows(&pipe_name, &cache_dir);

        // Try connecting to daemon
        if let Ok(exit_code) = try_daemon_compile_windows(&pipe_name, &args[1..]) {
            std::process::exit(exit_code);
        }
        // Fall through to direct exec on failure
    }

    // Lazy/deferred compilation mode: split lib crate compilation into two passes.
    // Pass 1: metadata-only (--emit=metadata + Compilation::Stop) → downstream can start immediately
    // Pass 2: selective codegen (after binary crate knows what's reachable)
    if lazy_enabled {
        let crate_type = crate_type_str();
        let crate_name = crate_name_str();
        let is_bin = crate_type == "bin";
        let is_lib = crate_type == "lib" || crate_type == "rlib" || crate_type == "dylib"
            || crate_type == "cdylib" || crate_type == "staticlib" || crate_type == "proc-macro";

        let cache_dir = env::var("CARGO_SLICER_CACHE_DIR")
            .unwrap_or_else(|_| ".slicer-cache".to_string());

        if is_lib {
            // Pass 1: metadata-only compilation for library crates.
            // The driver will run analysis + Compilation::Stop (no codegen).
            // We then create a placeholder .rlib so Cargo can pipeline.
            let driver = find_driver();
            let exit_code = run_driver_lazy_metadata(
                &driver, real_rustc, compiler_args, &cache_dir,
                crate_name.as_deref(), &crate_type,
            );
            std::process::exit(exit_code);
        }

        if is_bin {
            let driver = find_driver();
            let overlap = env::var("CARGO_SLICER_OVERLAP").is_ok();

            if overlap {
                // Overlapped mode: start binary compilation concurrently with Pass 2.
                //
                // WARNING: This only works when Pass 1 uses stub-all codegen (old mode),
                // NOT with metadata-only Pass 1 (--emit=dep-info,metadata). The metadata-only
                // .rmeta does not contain optimized MIR, so the binary's codegen will fail
                // with "missing optimized MIR" errors. Use CARGO_SLICER_OVERLAP=1 only
                // with the old stub-all LAZY mode (not DEFERRED).
                //
                // When it works, binary does analysis+MIR+LLVM while Pass 2 writes real
                // .rlib files. Pass 2 typically finishes before the binary's linker runs.
                let bin_child = Command::new(&driver)
                    .args(&args[1..])
                    .spawn();

                // Run Pass 2 synchronously (uses threads internally for per-crate parallelism)
                run_lazy_codegen_pass(&driver, &cache_dir);

                // Wait for binary compilation to finish
                match bin_child {
                    Ok(mut child) => {
                        match child.wait() {
                            Ok(status) => std::process::exit(status.code().unwrap_or(1)),
                            Err(e) => {
                                eprintln!("cargo-slicer-dispatch: binary compilation wait failed: {}", e);
                                std::process::exit(1);
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("cargo-slicer-dispatch: failed to spawn binary compilation: {}", e);
                        std::process::exit(1);
                    }
                }
            } else {
                // Sequential mode: complete Pass 2 before binary compilation
                run_lazy_codegen_pass(&driver, &cache_dir);
                // Fall through to normal binary compilation with the driver
            }
        }
    }

    // Local crate + virtual slicing: exec the full cargo-slicer-rustc driver
    let driver = find_driver();

    if !driver.exists() {
        eprintln!("cargo-slicer-dispatch: driver not found at {}", driver.display());
        // Fallback: try sccache or direct rustc
        if let Some(sccache) = find_sccache() {
            exec_with_sccache(&sccache, real_rustc, compiler_args);
        }
        exec_command(Command::new(real_rustc).args(compiler_args));
    }

    // Pass ALL original args (driver expects wrapper protocol: rustc_path + compiler_args)
    exec_command(Command::new(&driver).args(&args[1..]));
}

/// Find the driver binary path
fn find_driver() -> PathBuf {
    let driver_name = format!("cargo-slicer-rustc{}", std::env::consts::EXE_SUFFIX);
    env::var("CARGO_SLICER_DRIVER")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            env::current_exe().ok()
                .and_then(|p| p.parent().map(|d| d.join(&driver_name)))
                .unwrap_or_else(|| driver_name.into())
        })
}

/// Pass 1 of lazy/deferred compilation: run driver in metadata-only mode for a library crate.
///
/// Replaces --emit with --emit=dep-info,metadata so rustc writes .rmeta and .d but skips
/// LLVM codegen entirely. The driver runs analysis + BFS + cache-saving, then returns
/// Compilation::Continue (codegen phase is a no-op with metadata-only emit).
/// We create a placeholder .rlib from the .rmeta so Cargo can pipeline downstream crates.
///
/// Records the compilation args in a .lazy-pending file for Pass 2.
fn run_driver_lazy_metadata(
    driver: &PathBuf,
    real_rustc: &str,
    compiler_args: &[String],
    cache_dir: &str,
    crate_name: Option<&str>,
    crate_type: &str,
) -> i32 {
    let _ = std::fs::create_dir_all(cache_dir);

    // Replace --emit=dep-info,metadata,link with --emit=dep-info,metadata
    // so rustc writes .rmeta (and .d) but skips LLVM codegen and linking entirely.
    // With only metadata in the emit set, rustc's codegen phase is a no-op.
    let metadata_args: Vec<String> = compiler_args.iter()
        .map(|arg| {
            if arg.starts_with("--emit=") {
                "--emit=dep-info,metadata".to_string()
            } else {
                arg.clone()
            }
        })
        .collect();

    // Run driver with lazy-metadata phase (Compilation::Stop after analysis)
    let status = Command::new(driver)
        .arg(real_rustc)
        .args(&metadata_args)
        .env("CARGO_SLICER_PHASE", "lazy-metadata")
        .status();

    let exit_code = match status {
        Ok(s) => s.code().unwrap_or(1),
        Err(e) => {
            eprintln!("cargo-slicer-dispatch: lazy-metadata failed: {}", e);
            return 1;
        }
    };

    if exit_code != 0 {
        return exit_code;
    }

    // Create placeholder .rlib: extract --out-dir and -C extra-filename to find the output path.
    // The .rmeta file should already be written by rustc's metadata emission (happens before codegen).
    // We need to create a minimal .rlib (ar archive containing lib.rmeta) for Cargo to proceed.
    if let (Some(out_dir), Some(name)) = (extract_out_dir(compiler_args), crate_name) {
        let extra = extract_extra_filename(compiler_args);
        let rlib_name = format!("lib{}{}.rlib", name, extra);
        let rlib_path = PathBuf::from(&out_dir).join(&rlib_name);
        let rmeta_name = format!("lib{}{}.rmeta", name, extra);
        let rmeta_path = PathBuf::from(&out_dir).join(&rmeta_name);

        // Create placeholder .rlib by wrapping .rmeta in an ar archive.
        // IMPORTANT: rustc expects the member name to be "lib.rmeta" inside the archive.
        // We must rename the .rmeta to lib.rmeta in a temp dir before archiving.
        if rmeta_path.exists() && !rlib_path.exists() {
            let temp_dir = std::env::temp_dir().join(format!("slicer-placeholder-{}", std::process::id()));
            let _ = std::fs::create_dir_all(&temp_dir);
            let temp_rmeta = temp_dir.join("lib.rmeta");
            let ar_ok = if std::fs::copy(&rmeta_path, &temp_rmeta).is_ok() {
                #[cfg(unix)]
                let ar_status = Command::new("ar")
                    .arg("rcs")
                    .arg(&rlib_path)
                    .arg(&temp_rmeta)
                    .status();
                #[cfg(windows)]
                let ar_status = Command::new("lib")
                    .arg(format!("/OUT:{}", rlib_path.display()))
                    .arg(&temp_rmeta)
                    .status();
                let ok = matches!(ar_status, Ok(s) if s.success());
                let _ = std::fs::remove_dir_all(&temp_dir);
                ok
            } else {
                false
            };
            if !ar_ok {
                // Fallback: copy rmeta as rlib (Cargo may accept this for pipelining)
                let _ = std::fs::copy(&rmeta_path, &rlib_path);
            }
        }
    }

    // Record pending compilation args + env vars for Pass 2
    if let Some(name) = crate_name {
        let pending_path = format!("{}/{}-{}.lazy-pending", cache_dir, name, crate_type);
        // Serialize: real_rustc + compiler_args, then "---ENV---" separator,
        // then KEY=VALUE lines for Cargo-set environment variables.
        let mut content = String::new();
        content.push_str(real_rustc);
        content.push('\n');
        for arg in compiler_args {
            content.push_str(arg);
            content.push('\n');
        }
        // Capture Cargo-set env vars that build scripts may have set.
        // These are needed for env!() macros during Pass 2 recompilation.
        content.push_str("---ENV---\n");
        for (key, value) in std::env::vars() {
            // Capture build-script env vars and Cargo metadata vars.
            // Skip internal Cargo/slicer vars and common system vars.
            if key.starts_with("DEP_")
                || key.starts_with("CARGO_PKG_")
                || key.starts_with("CARGO_MANIFEST_")
                || key.starts_with("OUT_DIR")
                || key == "BUILD_TARGET"
                || key == "TARGET"
                || key == "HOST"
                || key == "OPT_LEVEL"
                || key == "PROFILE"
                // Capture any custom env vars from build scripts
                // (they often use uppercase names with underscores)
                || (key.chars().all(|c| c.is_ascii_uppercase() || c == '_' || c.is_ascii_digit())
                    && !key.starts_with("CARGO_SLICER_")
                    && !key.starts_with("RUSTC")
                    && !key.starts_with("RUST")
                    && key != "PATH"
                    && key != "HOME"
                    && key != "USER"
                    && key != "SHELL"
                    && key != "TERM"
                    && key != "LANG"
                    && key != "PWD"
                    && key != "SHLVL"
                    && key != "OLDPWD"
                    && key != "HOSTNAME"
                    && key != "LOGNAME"
                    && key != "MAIL"
                    && key != "EDITOR"
                    && key != "VISUAL"
                    && key != "XDG_RUNTIME_DIR"
                    && key != "_")
            {
                content.push_str(&key);
                content.push('=');
                content.push_str(&value);
                content.push('\n');
            }
        }
        let _ = std::fs::write(&pending_path, content);
    }

    0
}

/// Pass 2 of lazy compilation: re-compile all pending library crates with codegen filtering.
///
/// Called when the binary crate is about to be compiled. Reads all .lazy-pending files
/// and re-invokes the driver with CARGO_SLICER_PHASE=lazy-codegen.
fn run_lazy_codegen_pass(driver: &PathBuf, cache_dir: &str) {
    let entries = match std::fs::read_dir(cache_dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    let mut pending_files: Vec<PathBuf> = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().map_or(false, |e| e == "lazy-pending") {
            pending_files.push(path);
        }
    }

    if pending_files.is_empty() {
        return;
    }

    eprintln!("cargo-slicer-dispatch: lazy Pass 2: re-compiling {} library crates with codegen filtering", pending_files.len());

    // Process pending crates in parallel using threads
    let handles: Vec<_> = pending_files.into_iter().map(|pending_path| {
        let driver = driver.clone();
        std::thread::spawn(move || {
            let content = match std::fs::read_to_string(&pending_path) {
                Ok(c) => c,
                Err(_) => return,
            };

            // Parse .lazy-pending: compiler args before "---ENV---", env vars after
            let mut compiler_lines: Vec<&str> = Vec::new();
            let mut env_vars: Vec<(&str, &str)> = Vec::new();
            let mut in_env_section = false;
            for line in content.lines() {
                if line == "---ENV---" {
                    in_env_section = true;
                    continue;
                }
                if in_env_section {
                    if let Some((key, value)) = line.split_once('=') {
                        env_vars.push((key, value));
                    }
                } else {
                    compiler_lines.push(line);
                }
            }

            if compiler_lines.len() < 2 {
                return;
            }

            let real_rustc = compiler_lines[0];
            let compiler_args: Vec<String> = compiler_lines[1..].iter()
                .filter(|l| !l.is_empty())
                .map(|l| l.to_string())
                .collect();

            // Run driver with lazy-codegen phase (full compile with filtering)
            let mut cmd = Command::new(&driver);
            cmd.arg(real_rustc)
                .args(&compiler_args)
                .env("CARGO_SLICER_PHASE", "lazy-codegen")
                .env("CARGO_SLICER_VIRTUAL", "1")
                .env("CARGO_SLICER_CODEGEN_FILTER", "1");
            // Restore env vars captured from Pass 1 (build-script env vars etc.)
            for (key, value) in &env_vars {
                cmd.env(key, value);
            }
            let status = cmd.status();

            match status {
                Ok(s) if s.success() => {
                    // Clean up pending file on success
                    let _ = std::fs::remove_file(&pending_path);
                }
                Ok(s) => {
                    eprintln!("cargo-slicer-dispatch: lazy-codegen failed for {:?} (exit {})",
                        pending_path.file_name().unwrap_or_default(),
                        s.code().unwrap_or(-1));
                }
                Err(e) => {
                    eprintln!("cargo-slicer-dispatch: lazy-codegen exec failed: {}", e);
                }
            }
        })
    }).collect();

    // Wait for all Pass 2 compilations to complete
    for handle in handles {
        let _ = handle.join();
    }
}

/// Extract --out-dir from compiler args
fn extract_out_dir(args: &[String]) -> Option<String> {
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--out-dir" && i + 1 < args.len() {
            return Some(args[i + 1].clone());
        }
        i += 1;
    }
    None
}

/// Extract -C extra-filename=VALUE from compiler args
fn extract_extra_filename(args: &[String]) -> String {
    for arg in args {
        if let Some(rest) = arg.strip_prefix("extra-filename=") {
            return rest.to_string();
        }
    }
    // Also check -C extra-filename=VALUE form
    let mut i = 0;
    while i < args.len() {
        if args[i] == "-C" && i + 1 < args.len() {
            if let Some(rest) = args[i + 1].strip_prefix("extra-filename=") {
                return rest.to_string();
            }
        }
        i += 1;
    }
    String::new()
}

/// Find the .analysis file for a crate (may be -lib or -rlib)
fn find_analysis_file(cache_dir: &str, crate_name: &str) -> Option<String> {
    for suffix in &["lib", "rlib"] {
        let path = format!("{}/{}-{}.analysis", cache_dir, crate_name, suffix);
        if std::fs::metadata(&path).is_ok() {
            return Some(path);
        }
    }
    None
}

/// Check if all defined items in the analysis file are present in the cache file.
/// If so, nothing needs to be stubbed and we can skip the driver.
fn all_items_marked_in_cache(cache_path: &str, analysis_path: &str) -> bool {
    use std::io::{BufRead, BufReader};

    // Read cache file: first line is hash, rest are marked items
    let cache_file = match std::fs::File::open(cache_path) {
        Ok(f) => f,
        Err(_) => return false, // No cache → need driver
    };
    let cache_reader = BufReader::new(cache_file);
    let mut cache_items = std::collections::HashSet::new();
    for (i, line) in cache_reader.lines().enumerate() {
        if let Ok(line) = line {
            if i == 0 { continue; } // Skip hash line
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                cache_items.insert(trimmed.to_string());
            }
        }
    }

    // Read analysis file: count defined items not in cache
    let analysis_file = match std::fs::File::open(analysis_path) {
        Ok(f) => f,
        Err(_) => return false,
    };
    let analysis_reader = BufReader::new(analysis_file);
    let mut in_defined = false;

    for line in analysis_reader.lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => return false,
        };
        let trimmed = line.trim();

        if trimmed == "DEFINED_ITEMS:" {
            in_defined = true;
            continue;
        }
        if trimmed.ends_with(':') && trimmed != "DEFINED_ITEMS:" {
            if in_defined { break; } // Past the defined items section
            continue;
        }

        if in_defined {
            // Format: path\tpub|priv[\tfn|type] (3rd column optional)
            let parts: Vec<&str> = trimmed.split('\t').collect();
            if parts.len() >= 2 {
                let path = parts[0];
                let is_fn = parts.get(2).map_or(false, |k| *k == "fn");
                // Only check function items — non-functions can't be stubbed
                if is_fn && !cache_items.contains(path) {
                    return false; // Found an unmarked function → need driver
                }
            }
        }
    }

    true // All defined items are in cache → nothing to stub
}

/// Start the fork-server daemon in the background
#[cfg(unix)]
fn start_daemon(driver_path: &str, socket_path: &str, cache_dir: &str) {
    // Ensure cache dir exists
    let _ = std::fs::create_dir_all(cache_dir);

    // Check for stale PID file
    let pid_path = format!("{}/driver.pid", cache_dir);
    if let Ok(pid_str) = std::fs::read_to_string(&pid_path) {
        if let Ok(pid) = pid_str.trim().parse::<u32>() {
            // Check if process is alive via /proc
            let proc_path = format!("/proc/{}", pid);
            if std::fs::metadata(&proc_path).is_err() {
                // Process is dead, clean up stale files
                let _ = std::fs::remove_file(socket_path);
                let _ = std::fs::remove_file(&pid_path);
            } else {
                // Daemon is running, wait for socket
                wait_for_socket(socket_path);
                return;
            }
        }
    }

    // Spawn daemon via double-fork so it's never a child of this dispatch
    // process. If the daemon is a direct child, its lifetime is tied to
    // the dispatch — when the dispatch exits, Cargo's process tracking
    // can get confused by the orphaned child.
    let result = Command::new(driver_path)
        .arg(format!("--fork-server={}", socket_path))
        .arg("--daemonize")
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr({
            // Redirect daemon stderr to a file to avoid sharing Cargo's
            // stderr pipe with the daemon's fork children.
            let log_path = format!("{}/daemon.log", cache_dir);
            std::fs::File::create(&log_path)
                .map(std::process::Stdio::from)
                .unwrap_or_else(|_| std::process::Stdio::null())
        })
        .env("CARGO_SLICER_VIRTUAL", "1")
        .env("CARGO_SLICER_CODEGEN_FILTER", "1")
        .spawn();

    match result {
        Ok(mut child) => {
            // Wait for the intermediate process to exit (it double-forks
            // and exits immediately, leaving the real daemon as a child of init)
            let _ = child.wait();
            wait_for_socket(socket_path);
        }
        Err(e) => {
            eprintln!("cargo-slicer-dispatch: failed to start daemon: {}", e);
        }
    }
}

/// Wait up to 2 seconds for the daemon socket to appear
#[cfg(unix)]
fn wait_for_socket(socket_path: &str) {
    for _ in 0..40 {
        if std::fs::metadata(socket_path).is_ok() {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
}

/// Try to compile via the fork-server daemon.
/// Returns Ok(exit_code) on success, Err on connection failure.
#[cfg(unix)]
fn try_daemon_compile(socket_path: &str, args: &[String]) -> Result<i32, String> {
    use std::os::unix::net::UnixStream;
    use std::io::{Read as IoRead, Write as IoWrite};

    let mut stream = UnixStream::connect(socket_path)
        .map_err(|e| format!("connect: {}", e))?;

    // Set a generous timeout for compilation
    stream.set_read_timeout(Some(std::time::Duration::from_secs(300))).ok();
    stream.set_write_timeout(Some(std::time::Duration::from_secs(10))).ok();

    // Collect env vars that the compiler needs.
    // The daemon child inherits the daemon's env (not Cargo's), so we
    // transmit relevant env vars for the child to set after fork.
    let env_vars = cargo_slicer::process_ext::collect_daemon_env_vars();

    // Build entire message in one buffer to reduce write syscalls (~125 → 1).
    let mut buf = Vec::with_capacity(16384);

    // Args: u32 count, then (u32 len + bytes) per arg
    buf.extend_from_slice(&(args.len() as u32).to_le_bytes());
    for arg in args {
        buf.extend_from_slice(&(arg.len() as u32).to_le_bytes());
        buf.extend_from_slice(arg.as_bytes());
    }

    // Env vars: u32 count, then (u32 len + "KEY=VALUE" bytes) per var
    buf.extend_from_slice(&(env_vars.len() as u32).to_le_bytes());
    for (key, value) in &env_vars {
        let pair = format!("{}={}", key, value);
        buf.extend_from_slice(&(pair.len() as u32).to_le_bytes());
        buf.extend_from_slice(pair.as_bytes());
    }

    // Single write for the entire message
    stream.write_all(&buf).map_err(|e| format!("write request: {}", e))?;

    // Read result
    let mut buf4 = [0u8; 4];

    stream.read_exact(&mut buf4)
        .map_err(|e| format!("read stderr_len: {}", e))?;
    let stderr_len = u32::from_le_bytes(buf4) as usize;

    if stderr_len > 0 {
        let mut stderr_data = vec![0u8; stderr_len];
        stream.read_exact(&mut stderr_data)
            .map_err(|e| format!("read stderr: {}", e))?;
        // Write stderr to our stderr
        let _ = std::io::stderr().write_all(&stderr_data);
    }

    stream.read_exact(&mut buf4)
        .map_err(|e| format!("read exit_code: {}", e))?;
    let exit_code = i32::from_le_bytes(buf4);

    Ok(exit_code)
}

// ═══════════════════════════════════════════════════════════════════════════
// Windows Named-Pipe Client
// ═══════════════════════════════════════════════════════════════════════════

/// Generate a deterministic pipe name from the cache directory.
///
/// Uses a hash of the absolute cache dir path so different projects get
/// different pipe names. Format: `\\.\pipe\cargo_slicer_{hash:016x}`
#[cfg(windows)]
fn get_pipe_path(cache_dir: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let abs_path = std::path::Path::new(cache_dir)
        .canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(cache_dir));
    let mut hasher = DefaultHasher::new();
    abs_path.hash(&mut hasher);
    format!(r"\\.\pipe\cargo_slicer_{:016x}", hasher.finish())
}

/// Check if a process with the given PID is alive on Windows.
#[cfg(windows)]
fn is_process_alive_windows(pid: u32) -> bool {
    use windows_sys::Win32::System::Threading::{OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION};
    use windows_sys::Win32::Foundation::CloseHandle;

    let handle = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid) };
    if handle.is_null() {
        return false;
    }
    unsafe { CloseHandle(handle); }
    true
}

/// Ensure the daemon is running, starting it if necessary.
#[cfg(windows)]
fn ensure_daemon_running_windows(pipe_name: &str, cache_dir: &str) {
    let _ = std::fs::create_dir_all(cache_dir);

    let pid_path = format!("{}/driver.pid", cache_dir);

    // Check if daemon is already running via PID file
    if let Ok(pid_str) = std::fs::read_to_string(&pid_path) {
        if let Ok(pid) = pid_str.trim().parse::<u32>() {
            if is_process_alive_windows(pid) {
                // Process alive — try connecting to pipe to verify it's responsive
                if try_pipe_connect(pipe_name) {
                    return; // Daemon is running and responsive
                }
                // Pipe not ready yet, wait a bit
                wait_for_pipe(pipe_name);
                return;
            } else {
                // Process is dead, clean up stale PID file
                let _ = std::fs::remove_file(&pid_path);
            }
        }
    }

    // Start daemon
    start_daemon_windows(pipe_name, cache_dir);
}

/// Start the daemon process on Windows.
#[cfg(windows)]
fn start_daemon_windows(pipe_name: &str, cache_dir: &str) {
    use std::os::windows::process::CommandExt;

    let driver_path = env::var("CARGO_SLICER_DRIVER")
        .unwrap_or_else(|_| {
            env::current_exe().ok()
                .and_then(|p| p.parent().map(|d| d.join("cargo-slicer-rustc.exe").to_string_lossy().to_string()))
                .unwrap_or_else(|| "cargo-slicer-rustc.exe".to_string())
        });

    // DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP
    const DETACHED_PROCESS: u32 = 0x00000008;
    const CREATE_NEW_PROCESS_GROUP: u32 = 0x00000200;

    let log_path = format!("{}/daemon.log", cache_dir);
    let stderr_file = std::fs::File::create(&log_path)
        .map(std::process::Stdio::from)
        .unwrap_or_else(|_| std::process::Stdio::null());

    let result = Command::new(&driver_path)
        .arg(format!("--pipe-server={}", pipe_name))
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(stderr_file)
        .env("CARGO_SLICER_VIRTUAL", "1")
        .env("CARGO_SLICER_CODEGEN_FILTER", "1")
        .env("CARGO_SLICER_CACHE_DIR", cache_dir)
        .creation_flags(DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP)
        .spawn();

    match result {
        Ok(_child) => {
            // Don't wait — daemon runs independently
            wait_for_pipe(pipe_name);
        }
        Err(e) => {
            eprintln!("cargo-slicer-dispatch: failed to start Windows daemon: {}", e);
        }
    }
}

/// Try to connect to the named pipe (non-blocking check).
#[cfg(windows)]
fn try_pipe_connect(pipe_name: &str) -> bool {
    use windows_sys::Win32::Storage::FileSystem::{CreateFileW, OPEN_EXISTING};
    use windows_sys::Win32::Foundation::{CloseHandle, INVALID_HANDLE_VALUE, GENERIC_READ, GENERIC_WRITE};

    let wide_name: Vec<u16> = pipe_name.encode_utf16().chain(std::iter::once(0)).collect();
    let handle = unsafe {
        CreateFileW(
            wide_name.as_ptr(),
            GENERIC_READ | GENERIC_WRITE,
            0,
            std::ptr::null(),
            OPEN_EXISTING,
            0,
            std::ptr::null_mut(),
        )
    };
    if handle == INVALID_HANDLE_VALUE {
        return false;
    }
    unsafe { CloseHandle(handle); }
    true
}

/// Wait up to 4 seconds for the named pipe to become available.
#[cfg(windows)]
fn wait_for_pipe(pipe_name: &str) {
    for _ in 0..80 {
        if try_pipe_connect(pipe_name) {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
}

/// Try to compile via the Windows named-pipe daemon.
/// Returns Ok(exit_code) on success, Err on connection failure.
#[cfg(windows)]
fn try_daemon_compile_windows(pipe_name: &str, args: &[String]) -> Result<i32, String> {
    use windows_sys::Win32::Storage::FileSystem::{CreateFileW, OPEN_EXISTING};
    use windows_sys::Win32::Foundation::{CloseHandle, INVALID_HANDLE_VALUE, GENERIC_READ, GENERIC_WRITE, GetLastError};

    let wide_name: Vec<u16> = pipe_name.encode_utf16().chain(std::iter::once(0)).collect();
    let handle = unsafe {
        CreateFileW(
            wide_name.as_ptr(),
            GENERIC_READ | GENERIC_WRITE,
            0,
            std::ptr::null(),
            OPEN_EXISTING,
            0,
            std::ptr::null_mut(),
        )
    };
    if handle == INVALID_HANDLE_VALUE {
        let err = unsafe { GetLastError() };
        return Err(format!("connect to pipe: error {}", err));
    }

    // Collect env vars using the shared function
    let env_vars = cargo_slicer::process_ext::collect_daemon_env_vars();

    // Build entire message in one buffer to reduce write syscalls
    let mut buf = Vec::with_capacity(16384);

    // Args: u32 count, then (u32 len + bytes) per arg
    buf.extend_from_slice(&(args.len() as u32).to_le_bytes());
    for arg in args {
        buf.extend_from_slice(&(arg.len() as u32).to_le_bytes());
        buf.extend_from_slice(arg.as_bytes());
    }

    // Env vars: u32 count, then (u32 len + "KEY=VALUE" bytes) per var
    buf.extend_from_slice(&(env_vars.len() as u32).to_le_bytes());
    for (key, value) in &env_vars {
        let pair = format!("{}={}", key, value);
        buf.extend_from_slice(&(pair.len() as u32).to_le_bytes());
        buf.extend_from_slice(pair.as_bytes());
    }

    // Single write for the entire message
    pipe_write_all(handle, &buf).map_err(|e| {
        unsafe { CloseHandle(handle); }
        format!("write request: {}", e)
    })?;

    // Read result
    let mut buf4 = [0u8; 4];

    pipe_read_exact(handle, &mut buf4).map_err(|e| {
        unsafe { CloseHandle(handle); }
        format!("read stderr_len: {}", e)
    })?;
    let stderr_len = u32::from_le_bytes(buf4) as usize;

    if stderr_len > 0 {
        let mut stderr_data = vec![0u8; stderr_len];
        pipe_read_exact(handle, &mut stderr_data).map_err(|e| {
            unsafe { CloseHandle(handle); }
            format!("read stderr: {}", e)
        })?;
        let _ = std::io::Write::write_all(&mut std::io::stderr(), &stderr_data);
    }

    pipe_read_exact(handle, &mut buf4).map_err(|e| {
        unsafe { CloseHandle(handle); }
        format!("read exit_code: {}", e)
    })?;
    let exit_code = i32::from_le_bytes(buf4);

    unsafe { CloseHandle(handle); }
    Ok(exit_code)
}

/// Write all bytes to a pipe handle.
#[cfg(windows)]
fn pipe_write_all(handle: windows_sys::Win32::Foundation::HANDLE, data: &[u8]) -> std::io::Result<()> {
    use windows_sys::Win32::Storage::FileSystem::WriteFile;
    use windows_sys::Win32::Foundation::{FALSE, GetLastError};

    let mut offset = 0;
    while offset < data.len() {
        let mut bytes_written: u32 = 0;
        let ok = unsafe {
            WriteFile(
                handle,
                data[offset..].as_ptr() as _,
                (data.len() - offset) as u32,
                &mut bytes_written,
                std::ptr::null_mut(),
            )
        };
        if ok == FALSE {
            let err = unsafe { GetLastError() };
            return Err(std::io::Error::from_raw_os_error(err as i32));
        }
        offset += bytes_written as usize;
    }
    Ok(())
}

/// Read exactly `buf.len()` bytes from a pipe handle.
#[cfg(windows)]
fn pipe_read_exact(handle: windows_sys::Win32::Foundation::HANDLE, buf: &mut [u8]) -> std::io::Result<()> {
    use windows_sys::Win32::Storage::FileSystem::ReadFile;
    use windows_sys::Win32::Foundation::{FALSE, GetLastError};

    let mut offset = 0;
    while offset < buf.len() {
        let mut bytes_read: u32 = 0;
        let ok = unsafe {
            ReadFile(
                handle,
                buf[offset..].as_mut_ptr() as _,
                (buf.len() - offset) as u32,
                &mut bytes_read,
                std::ptr::null_mut(),
            )
        };
        if ok == FALSE {
            let err = unsafe { GetLastError() };
            return Err(std::io::Error::from_raw_os_error(err as i32));
        }
        if bytes_read == 0 {
            return Err(std::io::Error::new(std::io::ErrorKind::UnexpectedEof, "pipe closed"));
        }
        offset += bytes_read as usize;
    }
    Ok(())
}

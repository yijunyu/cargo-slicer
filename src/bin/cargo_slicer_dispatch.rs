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
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::Command;

/// Find sccache binary without spawning `which` (fast path).
fn find_sccache() -> Option<PathBuf> {
    // 1. Explicit env var
    if let Ok(path) = env::var("CARGO_SLICER_SCCACHE") {
        return Some(PathBuf::from(path));
    }

    // 2. Check common paths directly (no process spawn)
    let candidates = [
        "/usr/bin/sccache",
        "/usr/local/bin/sccache",
    ];
    for path in candidates {
        if std::fs::metadata(path).is_ok() {
            return Some(PathBuf::from(path));
        }
    }

    // 3. ~/.cargo/bin/sccache
    if let Ok(home) = env::var("HOME") {
        let p = PathBuf::from(home).join(".cargo/bin/sccache");
        if p.exists() {
            return Some(p);
        }
    }

    None
}

fn exec_with_sccache(sccache: &PathBuf, rustc: &str, args: &[String]) {
    let _err = Command::new(sccache).arg(rustc).args(args).exec();
    // exec only returns on error; fall through to direct rustc
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

    // Fast path 1: probe commands
    if compiler_args.iter().any(|a| a == "-vV" || a == "-V" || a == "--version" || a.starts_with("--print")) {
        let err = Command::new(real_rustc).args(compiler_args).exec();
        eprintln!("cargo-slicer-dispatch: probe exec failed: {}", err);
        std::process::exit(1);
    }

    // Check if this is a non-local (registry/git/vendor) crate
    let is_non_local = compiler_args.iter().any(|arg| {
        !arg.starts_with('-') && arg.ends_with(".rs") && (
            arg.contains(".cargo/registry") ||
            arg.contains(".cargo/git") ||
            arg.contains("/vendor/")
        )
    });

    let virtual_enabled = env::var("CARGO_SLICER_VIRTUAL").is_ok();

    // Non-local crates or no virtual slicing: use sccache (if available) or direct rustc
    if is_non_local || !virtual_enabled {
        if let Some(sccache) = find_sccache() {
            exec_with_sccache(&sccache, real_rustc, compiler_args);
        }
        let err = Command::new(real_rustc).args(compiler_args).exec();
        eprintln!("cargo-slicer-dispatch: exec failed: {}", err);
        std::process::exit(1);
    }

    // Skip-driver optimization: if pre-analysis determined this crate needs no stubs,
    // skip the driver entirely and use plain rustc (or sccache).
    // Check .skip-driver marker first (single stat() syscall), then fall back to
    // the more expensive all-items-marked-in-cache check for build phase.
    if let Some(crate_name) = extract_crate_name(compiler_args) {
        let crate_type = extract_crate_type(compiler_args);
        let cache_dir = env::var("CARGO_SLICER_CACHE_DIR")
            .unwrap_or_else(|_| ".slicer-cache".to_string());
        let skip_path = format!("{}/{}-{}.skip-driver", cache_dir, crate_name, crate_type);

        if std::fs::metadata(&skip_path).is_ok() {
            // All items reachable → no stubs needed → skip driver
            if let Some(sccache) = find_sccache() {
                exec_with_sccache(&sccache, real_rustc, compiler_args);
            }
            let err = Command::new(real_rustc).args(compiler_args).exec();
            eprintln!("cargo-slicer-dispatch: exec failed: {}", err);
            std::process::exit(1);
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
                    let err = Command::new(real_rustc).args(compiler_args).exec();
                    eprintln!("cargo-slicer-dispatch: exec failed: {}", err);
                    std::process::exit(1);
                }
            }
        }
    }

    // Try fork-server daemon for faster driver invocation
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

    // Local crate + virtual slicing: exec the full cargo-slicer-rustc driver
    let driver = env::var("CARGO_SLICER_DRIVER")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            env::current_exe().ok()
                .and_then(|p| p.parent().map(|d| d.join("cargo-slicer-rustc")))
                .unwrap_or_else(|| "cargo-slicer-rustc".into())
        });

    if !driver.exists() {
        eprintln!("cargo-slicer-dispatch: driver not found at {}", driver.display());
        // Fallback: try sccache or direct rustc
        if let Some(sccache) = find_sccache() {
            exec_with_sccache(&sccache, real_rustc, compiler_args);
        }
        let err = Command::new(real_rustc).args(compiler_args).exec();
        eprintln!("cargo-slicer-dispatch: exec failed: {}", err);
        std::process::exit(1);
    }

    // Pass ALL original args (driver expects wrapper protocol: rustc_path + compiler_args)
    let err = Command::new(&driver).args(&args[1..]).exec();
    eprintln!("cargo-slicer-dispatch: driver exec failed: {}", err);
    std::process::exit(1);
}

/// Extract --crate-type from compiler args
fn extract_crate_type(args: &[String]) -> String {
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--crate-type" && i + 1 < args.len() {
            return args[i + 1].clone();
        }
        i += 1;
    }
    "unknown".to_string()
}

/// Extract --crate-name from compiler args
fn extract_crate_name(args: &[String]) -> Option<String> {
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--crate-name" && i + 1 < args.len() {
            return Some(args[i + 1].clone());
        }
        i += 1;
    }
    None
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

    // Spawn daemon process in background
    let result = Command::new(driver_path)
        .arg(format!("--fork-server={}", socket_path))
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::inherit())
        .env("CARGO_SLICER_VIRTUAL", "1")
        .env("CARGO_SLICER_CODEGEN_FILTER", "1")
        .spawn();

    match result {
        Ok(_child) => {
            wait_for_socket(socket_path);
        }
        Err(e) => {
            eprintln!("cargo-slicer-dispatch: failed to start daemon: {}", e);
        }
    }
}

/// Wait up to 2 seconds for the daemon socket to appear
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
fn try_daemon_compile(socket_path: &str, args: &[String]) -> Result<i32, String> {
    use std::os::unix::net::UnixStream;
    use std::io::{Read as IoRead, Write as IoWrite};

    let mut stream = UnixStream::connect(socket_path)
        .map_err(|e| format!("connect: {}", e))?;

    // Set a generous timeout for compilation
    stream.set_read_timeout(Some(std::time::Duration::from_secs(300))).ok();
    stream.set_write_timeout(Some(std::time::Duration::from_secs(10))).ok();

    // Send args
    let arg_count = args.len() as u32;
    stream.write_all(&arg_count.to_le_bytes())
        .map_err(|e| format!("write arg_count: {}", e))?;

    for arg in args {
        let len = arg.len() as u32;
        stream.write_all(&len.to_le_bytes())
            .map_err(|e| format!("write arg_len: {}", e))?;
        stream.write_all(arg.as_bytes())
            .map_err(|e| format!("write arg: {}", e))?;
    }
    stream.flush().map_err(|e| format!("flush: {}", e))?;

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

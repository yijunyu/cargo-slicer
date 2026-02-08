//! Custom rustc driver for cargo-slicer
//!
//! This binary acts as a drop-in replacement for rustc, using the direct
//! rustc_interface approach to collect HIR data and optionally perform
//! virtual slicing (filtering codegen without modifying source).
//!
//! By default, virtual slicing analyzes ALL crates (including dependencies)
//! to maximize unused code elimination. Use CARGO_SLICER_SKIP_DEPS=1 to
//! skip analysis for registry dependencies (faster but less thorough).
//!
//! Debug logging: Set CARGO_SLICER_DEBUG=1 to enable file-based logging
//! to .cargo-slicer-debug.log (or path specified by CARGO_SLICER_DEBUG_LOG).

#![feature(rustc_private)]
#![feature(box_patterns)]

extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;

use rustc_interface::interface;
use std::collections::{HashSet, BTreeSet};
use std::env;
use std::path::{Path, PathBuf};
use std::io::{BufRead, BufReader};

use cargo_slicer::rustc_integration::debug_log::{debug_log, is_debug_enabled};

/// Cache entry format for incremental virtual slicing
#[derive(Debug)]
struct CacheEntry {
    source_hash: String,
    marked_items: HashSet<String>,
}

/// Compute a hash of all .rs source files in the crate's source tree.
///
/// Given the crate root file (e.g., src/lib.rs), hashes all .rs files in
/// the same directory tree. This ensures cache invalidation when any source
/// file changes, not just the crate root.
fn compute_source_hash(source_path: &Path) -> Option<String> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    let mut file_count = 0u64;

    // Get the directory containing the source file
    let source_dir = source_path.parent()?;

    // Collect all .rs files in the source tree
    let mut rs_files = Vec::new();
    collect_rs_files(source_dir, &mut rs_files);
    rs_files.sort();

    if rs_files.is_empty() {
        // Fallback: just hash the root file
        let content = std::fs::read(source_path).ok()?;
        content.hash(&mut hasher);
        return Some(format!("{:016x}", hasher.finish()));
    }

    for path in &rs_files {
        if let Ok(content) = std::fs::read(path) {
            path.hash(&mut hasher);
            content.hash(&mut hasher);
            file_count += 1;
        }
    }

    file_count.hash(&mut hasher);
    Some(format!("{:016x}", hasher.finish()))
}

/// Recursively collect .rs files from a directory
fn collect_rs_files(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_rs_files(&path, files);
            } else if path.extension().map_or(false, |ext| ext == "rs") {
                files.push(path);
            }
        }
    }
}

/// Compute a combined hash of ALL source files from the source map.
///
/// Uses the source map from `tcx` to find every file that was actually parsed,
/// ensuring cache invalidation when any contributing file changes.
fn compute_all_sources_hash(tcx: rustc_middle::ty::TyCtxt<'_>) -> Option<String> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let source_map = tcx.sess.source_map();
    let files = source_map.files();
    let mut hasher = DefaultHasher::new();
    let mut file_count = 0u64;

    // Sort files by name for deterministic hashing
    let mut local_files: Vec<_> = files.iter()
        .filter_map(|f| {
            match &f.name {
                rustc_span::FileName::Real(real_name) => {
                    real_name.local_path().map(|p| p.to_path_buf())
                }
                _ => None,
            }
        })
        .collect();

    local_files.sort();

    for path in &local_files {
        if let Ok(content) = std::fs::read(path) {
            path.hash(&mut hasher);
            content.hash(&mut hasher);
            file_count += 1;
        }
    }

    if file_count == 0 {
        return None;
    }

    file_count.hash(&mut hasher);

    if is_debug_enabled() {
        debug_log(&format!("[cache] Hashed {} source files for cache key", file_count));
    }

    Some(format!("{:016x}", hasher.finish()))
}

/// Get the cache directory, creating it if necessary
fn get_cache_dir() -> Option<PathBuf> {
    // Use CARGO_SLICER_CACHE_DIR if set, otherwise use .slicer-cache in current dir
    let cache_dir = env::var("CARGO_SLICER_CACHE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(".slicer-cache"));

    // Create directory if it doesn't exist
    if !cache_dir.exists() {
        std::fs::create_dir_all(&cache_dir).ok()?;
    }

    Some(cache_dir)
}

/// Get cache file path for a crate, including crate type to avoid lib/bin collisions
fn get_cache_path(crate_name: &str, crate_type: &str) -> Option<PathBuf> {
    let cache_dir = get_cache_dir()?;
    Some(cache_dir.join(format!("{}-{}.cache", crate_name, crate_type)))
}

/// Extract crate type from rustc args (lib, bin, proc-macro, etc.)
fn extract_crate_type_from_args(args: &[String]) -> String {
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--crate-type" && i + 1 < args.len() {
            return args[i + 1].clone();
        }
        i += 1;
    }
    "unknown".to_string()
}

/// Load cache entry from file
fn load_cache_entry(cache_path: &Path) -> Option<CacheEntry> {
    let file = std::fs::File::open(cache_path).ok()?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    // First line is the hash
    let source_hash = lines.next()?.ok()?;

    // Remaining lines are marked items
    let marked_items: HashSet<String> = lines
        .filter_map(|line| line.ok())
        .filter(|s| !s.is_empty())
        .collect();

    Some(CacheEntry { source_hash, marked_items })
}

/// Save cache entry to file
fn save_cache_entry(cache_path: &Path, hash: &str, marked_items: &HashSet<String>) -> bool {
    let mut content = String::new();
    content.push_str(hash);
    content.push('\n');

    // Sort items for deterministic output
    let sorted: BTreeSet<_> = marked_items.iter().collect();
    for item in sorted {
        content.push_str(item);
        content.push('\n');
    }

    std::fs::write(cache_path, content).is_ok()
}

/// Load cross-crate seeds from a .seeds file.
///
/// Returns true if seeds were loaded, false if no seeds file exists.
/// The seeds file is written by `cross_crate_bfs::run_cross_crate_bfs()` after
/// a build, and contains only the pub items reachable from binary entry points.
fn load_seeds_file(crate_name: &str, crate_type: &str, seeds: &mut HashSet<String>) -> bool {
    let cache_dir = match get_cache_dir() {
        Some(d) => d,
        None => return false,
    };
    let seeds_path = cache_dir.join(format!("{}-{}.seeds", crate_name, crate_type));

    let file = match std::fs::File::open(&seeds_path) {
        Ok(f) => f,
        Err(_) => return false,
    };

    let reader = BufReader::new(file);
    let mut line_count = 0;

    for (i, line) in reader.lines().enumerate() {
        if let Ok(line) = line {
            if i == 0 { continue; } // Skip hash line
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                seeds.insert(trimmed.to_string());
                line_count += 1;
            }
        }
    }

    if line_count > 0 {
        if is_debug_enabled() {
            debug_log(&format!(
                "[cargo-slicer-rustc] Loaded {} cross-crate seeds from {}",
                line_count, seeds_path.display()
            ));
        }
        true
    } else {
        false
    }
}

/// Extract source file path from rustc args
fn extract_source_path(args: &[String]) -> Option<PathBuf> {
    for arg in args {
        if arg.starts_with('-') {
            continue;
        }
        if arg.ends_with(".rs") {
            return Some(PathBuf::from(arg));
        }
    }
    None
}

/// Extract crate name from rustc args
fn extract_crate_name_from_args(args: &[String]) -> Option<String> {
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--crate-name" && i + 1 < args.len() {
            return Some(args[i + 1].clone());
        }
        i += 1;
    }
    None
}

/// Check if args indicate this is a dependency crate (from registry/git/vendor)
fn is_dependency_crate_from_args(args: &[String]) -> bool {
    for arg in args {
        if arg.starts_with('-') {
            continue;
        }
        if arg.ends_with(".rs") {
            if arg.contains(".cargo/registry") || arg.contains(".cargo/git") {
                return true;
            }
            if arg.contains("/vendor/") {
                return true;
            }
        }
    }
    false
}

/// Check if this compilation is for a dependency crate (from cargo registry/git)
/// rather than a local crate.
///
/// By default, virtual slicing analyzes ALL crates (including dependencies) since
/// this provides the most benefit - unused code in dependencies is also skipped.
///
/// Set CARGO_SLICER_SKIP_DEPS=1 to skip virtual slicing for registry dependencies
/// (faster compilation but less code elimination).
fn is_dependency_crate(args: &[String]) -> bool {
    // By default, treat all crates as local (apply virtual slicing to all)
    // Only skip dependencies if explicitly requested
    if env::var("CARGO_SLICER_SKIP_DEPS").is_err() {
        return false;  // Slice all crates (default)
    }

    // CARGO_SLICER_SKIP_DEPS=1: Skip registry dependencies for faster builds
    for arg in args {
        // Skip flags
        if arg.starts_with('-') {
            continue;
        }

        // Check if it's a .rs file path
        if arg.ends_with(".rs") {
            // Dependency paths contain .cargo/registry or .cargo/git
            if arg.contains(".cargo/registry") || arg.contains(".cargo/git") {
                return true;
            }
            // Also check for common sliced crate patterns
            if arg.contains("_sliced/") || arg.contains("/sliced_crates/") {
                return true;
            }
        }
    }

    false
}

/// Fast path: exec the real rustc (or sccache) directly for dependency crates
/// This avoids loading rustc_driver for crates that don't need analysis.
/// If sccache is available, use it for caching; otherwise call rustc directly.
fn exec_real_rustc(rustc_path: &str, args: &[String]) -> ! {
    use std::os::unix::process::CommandExt;
    use std::process::Command;

    // Check if sccache should be used (set CARGO_SLICER_USE_SCCACHE=1 to enable)
    let use_sccache = env::var("CARGO_SLICER_USE_SCCACHE").is_ok();

    if use_sccache {
        // Try to find sccache
        if let Some(sccache) = find_sccache() {
            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Using sccache: {}", sccache));
            }
            // sccache expects: sccache rustc [args...]
            let err = Command::new(&sccache)
                .arg(rustc_path)
                .args(args)
                .exec();
            eprintln!("Failed to exec sccache: {}", err);
            std::process::exit(1);
        }
    }

    // Fallback: call rustc directly
    let err = Command::new(rustc_path)
        .args(args)
        .exec();

    // exec() only returns if it fails
    eprintln!("Failed to exec rustc: {}", err);
    std::process::exit(1);
}

/// Find sccache binary
fn find_sccache() -> Option<String> {
    use std::process::Command;

    // Check common locations
    let paths = [
        // Check PATH first
        "sccache",
        // Common install locations
        "/usr/bin/sccache",
        "/usr/local/bin/sccache",
    ];

    for path in paths {
        if let Ok(output) = Command::new("which").arg(path).output() {
            if output.status.success() {
                let found = String::from_utf8_lossy(&output.stdout).trim().to_string();
                if !found.is_empty() {
                    return Some(found);
                }
            }
        }
        // Also check if the path exists directly
        if std::path::Path::new(path).exists() {
            return Some(path.to_string());
        }
    }

    // Check ~/.cargo/bin/sccache
    if let Ok(home) = env::var("HOME") {
        let cargo_sccache = format!("{}/.cargo/bin/sccache", home);
        if std::path::Path::new(&cargo_sccache).exists() {
            return Some(cargo_sccache);
        }
    }

    None
}

/// Run the fork-server daemon: pre-load librustc_driver.so once, then fork()
/// for each compilation request received over a unix socket.
///
/// Protocol (binary, over unix socket):
///   Client → Server: u32 LE arg_count, then for each arg: u32 LE len + UTF-8 bytes
///   Server → Client: u32 LE stderr_len + stderr bytes + i32 LE exit_code
fn run_fork_server(socket_path: &str) -> ! {
    use std::os::unix::net::UnixListener;
    use std::io::Read as IoRead;

    // Remove stale socket
    let _ = std::fs::remove_file(socket_path);

    let listener = match UnixListener::bind(socket_path) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("cargo-slicer-rustc: fork-server bind failed: {}", e);
            std::process::exit(1);
        }
    };

    // Write PID file
    if let Some(parent) = Path::new(socket_path).parent() {
        let pid_path = parent.join("driver.pid");
        let _ = std::fs::write(&pid_path, format!("{}", std::process::id()));
    }

    // Set non-blocking with timeout for idle shutdown
    listener.set_nonblocking(false).ok();

    eprintln!("cargo-slicer-rustc: fork-server listening on {}", socket_path);

    let idle_timeout = std::time::Duration::from_secs(300); // 5 minutes
    let mut last_activity = std::time::Instant::now();

    loop {
        // Check idle timeout
        if last_activity.elapsed() > idle_timeout {
            eprintln!("cargo-slicer-rustc: fork-server idle timeout, exiting");
            cleanup_fork_server(socket_path);
            std::process::exit(0);
        }

        // Use SO_RCVTIMEO to periodically check idle timeout
        use std::os::unix::io::AsRawFd;
        let fd = listener.as_raw_fd();
        let tv = libc::timeval {
            tv_sec: 10,
            tv_usec: 0,
        };
        unsafe {
            libc::setsockopt(
                fd,
                libc::SOL_SOCKET,
                libc::SO_RCVTIMEO,
                &tv as *const _ as *const libc::c_void,
                std::mem::size_of::<libc::timeval>() as libc::socklen_t,
            );
        }

        let (mut stream, _) = match listener.accept() {
            Ok(s) => s,
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock
                || e.raw_os_error() == Some(libc::EAGAIN) => {
                continue; // timeout, check idle
            }
            Err(e) => {
                eprintln!("cargo-slicer-rustc: accept error: {}", e);
                continue;
            }
        };

        last_activity = std::time::Instant::now();

        // Read compilation args from client
        let args = match read_compilation_request(&mut stream) {
            Ok(a) => a,
            Err(e) => {
                eprintln!("cargo-slicer-rustc: read request error: {}", e);
                continue;
            }
        };

        // Create pipes for capturing child stderr
        let mut stderr_pipe = [0i32; 2];
        if unsafe { libc::pipe(stderr_pipe.as_mut_ptr()) } != 0 {
            eprintln!("cargo-slicer-rustc: pipe() failed");
            let _ = send_compilation_result(&mut stream, b"pipe failed", 1);
            continue;
        }

        let pid = unsafe { libc::fork() };
        match pid {
            -1 => {
                eprintln!("cargo-slicer-rustc: fork() failed");
                let _ = send_compilation_result(&mut stream, b"fork failed", 1);
                unsafe {
                    libc::close(stderr_pipe[0]);
                    libc::close(stderr_pipe[1]);
                }
            }
            0 => {
                // ═══ Child process ═══
                // Close read end of pipe, redirect stderr to write end
                unsafe {
                    libc::close(stderr_pipe[0]);
                    libc::dup2(stderr_pipe[1], 2); // redirect stderr
                    libc::close(stderr_pipe[1]);
                }

                // Close the listener socket (child doesn't need it)
                drop(listener);

                // Run the actual driver logic
                let exit_code = run_driver_main(&args);

                // Read captured stderr from the pipe and send result
                // (can't do this since stderr is the pipe; child just exits)
                unsafe { libc::_exit(exit_code) };
            }
            child_pid => {
                // ═══ Parent process ═══
                // Close write end of pipe
                unsafe { libc::close(stderr_pipe[1]); }

                // Read stderr from child
                let mut stderr_data = Vec::new();
                let mut read_file = unsafe {
                    use std::os::unix::io::FromRawFd;
                    std::fs::File::from_raw_fd(stderr_pipe[0])
                };
                let _ = read_file.read_to_end(&mut stderr_data);
                // read_file drops and closes stderr_pipe[0]

                // Wait for child
                let mut status: libc::c_int = 0;
                unsafe { libc::waitpid(child_pid, &mut status, 0); }

                let exit_code = if libc::WIFEXITED(status) {
                    libc::WEXITSTATUS(status)
                } else {
                    1
                };

                let _ = send_compilation_result(&mut stream, &stderr_data, exit_code);
            }
        }
    }
}

fn cleanup_fork_server(socket_path: &str) {
    let _ = std::fs::remove_file(socket_path);
    if let Some(parent) = Path::new(socket_path).parent() {
        let _ = std::fs::remove_file(parent.join("driver.pid"));
    }
}

fn read_compilation_request(stream: &mut impl std::io::Read) -> Result<Vec<String>, String> {
    let mut buf4 = [0u8; 4];
    stream.read_exact(&mut buf4).map_err(|e| format!("read arg_count: {}", e))?;
    let arg_count = u32::from_le_bytes(buf4) as usize;

    if arg_count > 10000 {
        return Err(format!("too many args: {}", arg_count));
    }

    let mut args = Vec::with_capacity(arg_count);
    for _ in 0..arg_count {
        stream.read_exact(&mut buf4).map_err(|e| format!("read arg len: {}", e))?;
        let len = u32::from_le_bytes(buf4) as usize;
        if len > 1_000_000 {
            return Err(format!("arg too long: {}", len));
        }
        let mut arg_bytes = vec![0u8; len];
        stream.read_exact(&mut arg_bytes).map_err(|e| format!("read arg: {}", e))?;
        let arg = String::from_utf8(arg_bytes).map_err(|e| format!("invalid utf8: {}", e))?;
        args.push(arg);
    }

    Ok(args)
}

fn send_compilation_result(stream: &mut impl std::io::Write, stderr_data: &[u8], exit_code: i32) -> Result<(), String> {
    let stderr_len = stderr_data.len() as u32;
    stream.write_all(&stderr_len.to_le_bytes()).map_err(|e| format!("write stderr_len: {}", e))?;
    stream.write_all(stderr_data).map_err(|e| format!("write stderr: {}", e))?;
    stream.write_all(&exit_code.to_le_bytes()).map_err(|e| format!("write exit_code: {}", e))?;
    stream.flush().map_err(|e| format!("flush: {}", e))?;
    Ok(())
}

/// Run the main driver logic with the given args (extracted for fork-server use).
/// Returns exit code.
fn run_driver_main(raw_args: &[String]) -> i32 {
    let collect_data = env::var("CARGO_SLICER_COLLECT_DATA").is_ok();
    let virtual_slice = env::var("CARGO_SLICER_VIRTUAL").is_ok();

    // When used via fork-server, args are the full original args
    let is_wrapper_mode = raw_args.get(1).map(|arg| {
        arg.ends_with("rustc") || arg.contains("/rustc")
    }).unwrap_or(false);

    let mut args: Vec<String> = vec!["rustc".to_string()];
    if is_wrapper_mode {
        args.extend(raw_args.iter().skip(2).cloned());
    } else {
        args.extend(raw_args.iter().skip(1).cloned());
    }

    if virtual_slice {
        return run_with_virtual_slicing(&args);
    }

    if collect_data {
        return run_with_data_collection(&args);
    }

    let mut callbacks = PassthroughCallbacks;
    rustc_driver::catch_with_exit_code(|| {
        rustc_driver::run_compiler(&args, &mut callbacks)
    })
}

fn main() {
    // Install panic hook
    rustc_driver::install_ice_hook("https://github.com/yijunyu/cargo-slicer/issues", |_| ());

    // Fork-server mode: stay resident, fork for each compilation
    let raw_args: Vec<String> = env::args().collect();
    if let Some(fork_server_arg) = raw_args.iter().find_map(|a| a.strip_prefix("--fork-server=")) {
        run_fork_server(fork_server_arg);
    }

    let collect_data = env::var("CARGO_SLICER_COLLECT_DATA").is_ok();
    let virtual_slice = env::var("CARGO_SLICER_VIRTUAL").is_ok();

    if is_debug_enabled() {
        debug_log(&format!("[cargo-slicer-rustc] Starting with {} args", raw_args.len()));
        if collect_data {
            debug_log("[cargo-slicer-rustc] Data collection ENABLED");
        }
        if virtual_slice {
            debug_log("[cargo-slicer-rustc] Virtual slicing ENABLED");
        }
    }

    // When used as RUSTC_WRAPPER, cargo invokes: wrapper /path/to/rustc [args...]
    // We need to detect this and skip the rustc path argument.
    // Detection: if arg[1] exists and looks like a path to rustc binary, we're in wrapper mode.
    let is_wrapper_mode = raw_args.get(1).map(|arg| {
        arg.ends_with("rustc") || arg.contains("/rustc")
    }).unwrap_or(false);

    let real_rustc = if is_wrapper_mode {
        raw_args.get(1).cloned()
    } else {
        None
    };

    // Build the args for rustc_driver - replace arg[0] with "rustc"
    let mut args: Vec<String> = vec!["rustc".to_string()];
    if is_wrapper_mode {
        // Skip arg[0] (our binary) and arg[1] (real rustc path)
        args.extend(raw_args.iter().skip(2).cloned());
        if is_debug_enabled() {
            debug_log("[cargo-slicer-rustc] RUSTC_WRAPPER mode detected, skipping rustc path");
        }
    } else {
        // Direct invocation: skip only arg[0]
        args.extend(raw_args.iter().skip(1).cloned());
    }

    // Fast path: for non-local (registry/git) crates, exec real rustc directly.
    // Virtual slicing only analyzes local workspace crates, so there's no benefit
    // to loading rustc_driver for third-party dependencies. This eliminates the
    // ~40ms overhead of loading our driver binary per dependency crate.
    if let Some(ref rustc) = real_rustc {
        let is_registry = is_dependency_crate_from_args(&args);
        if is_registry || is_dependency_crate(&args) {
            if is_debug_enabled() {
                debug_log("[cargo-slicer-rustc] Non-local crate - exec'ing real rustc directly");
            }
            let rustc_args: Vec<String> = raw_args.iter().skip(2).cloned().collect();
            exec_real_rustc(rustc, &rustc_args);
        }
    }

    // Check if this is a "probe" command from cargo (e.g., -vV, --version, --print)
    // These must always pass through to regular rustc behavior
    let is_probe = args.iter().any(|arg| {
        arg == "-vV" || arg == "-V" || arg == "--version" ||
        arg.starts_with("--print") || arg == "-vvV"
    });

    // For probe commands, exec real rustc directly (fast path)
    if is_probe {
        if let Some(ref rustc) = real_rustc {
            let rustc_args: Vec<String> = raw_args.iter().skip(2).cloned().collect();
            exec_real_rustc(rustc, &rustc_args);
        }
        // Fallback if not in wrapper mode
        let mut callbacks = PassthroughCallbacks;
        let exit_code = rustc_driver::catch_with_exit_code(|| {
            rustc_driver::run_compiler(&args, &mut callbacks)
        });
        std::process::exit(exit_code);
    }

    // If virtual slicing is enabled, use the virtual slicing path (only reaches here for local crates)
    if virtual_slice {
        let exit_code = run_with_virtual_slicing(&args);
        std::process::exit(exit_code);
    }

    // If data collection is enabled, use our custom compilation path
    if collect_data {
        let exit_code = run_with_data_collection(&args);
        std::process::exit(exit_code);
    }

    // Otherwise, just pass through to normal rustc
    let mut callbacks = PassthroughCallbacks;
    let exit_code = rustc_driver::catch_with_exit_code(|| {
        rustc_driver::run_compiler(&args, &mut callbacks)
    });

    if is_debug_enabled() {
        debug_log(&format!("[cargo-slicer-rustc] Finished with exit code: {}", exit_code));
    }

    std::process::exit(exit_code);
}

/// Passthrough callbacks that do nothing special
struct PassthroughCallbacks;

impl rustc_driver::Callbacks for PassthroughCallbacks {
    fn config(&mut self, _config: &mut interface::Config) {
        if is_debug_enabled() {
            debug_log("[cargo-slicer-rustc] Passthrough mode");
        }
    }
}

/// Run compilation with data collection using direct interface access
fn run_with_data_collection(args: &[String]) -> i32 {
    use rustc_driver::Callbacks;

    struct DataCollectionCallbacks {
        output_path: PathBuf,
    }

    impl DataCollectionCallbacks {
        fn new() -> Self {
            let output_path = env::var("CARGO_SLICER_DATA_OUT")
                .map(PathBuf::from)
                .unwrap_or_else(|_| PathBuf::from(".cargo-slicer-data.json"));

            Self { output_path }
        }
    }

    impl Callbacks for DataCollectionCallbacks {
        fn config(&mut self, _config: &mut interface::Config) {
            if is_debug_enabled() {
                debug_log("[cargo-slicer-rustc] Data collection config");
            }
        }

        fn after_analysis<'tcx>(
            &mut self,
            _compiler: &interface::Compiler,
            tcx: rustc_middle::ty::TyCtxt<'tcx>,
        ) -> rustc_driver::Compilation {
            use rustc_span::def_id::LOCAL_CRATE;

            if is_debug_enabled() {
                debug_log("[cargo-slicer-rustc] After analysis - collecting data");
            }

            let crate_name = tcx.crate_name(LOCAL_CRATE);

            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Analyzing crate: {}", crate_name));
            }

            // Call our HIR visitor (which now also does MIR-based reference tracking)
            let usage_data = cargo_slicer::rustc_integration::hir_visitor::collect_from_hir(tcx);

            // Write to file
            if let Err(e) = usage_data.write_to_file(&self.output_path) {
                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] ERROR: Failed to write data: {}", e));
                }
            } else if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] SUCCESS: Wrote {} items to {}",
                         usage_data.defined_items.len(),
                         self.output_path.display()));
                debug_log(&format!("[cargo-slicer-rustc] Collected {} references, {} dependencies",
                         usage_data.referenced_items.len(),
                         usage_data.item_dependencies.len()));
            }

            // Continue compilation
            rustc_driver::Compilation::Continue
        }
    }

    let mut callbacks = DataCollectionCallbacks::new();

    rustc_driver::catch_with_exit_code(|| {
        rustc_driver::run_compiler(args, &mut callbacks)
    })
}

/// Run compilation with virtual slicing (filter codegen without modifying source)
fn run_with_virtual_slicing(args: &[String]) -> i32 {
    use rustc_driver::Callbacks;
    use cargo_slicer::rustc_integration::store_marked_items;

    struct VirtualSlicingCallbacks {
        /// Marked items loaded from file (if provided via CARGO_SLICER_MARKED_ITEMS)
        preloaded_marked: Option<HashSet<String>>,
        /// Cached marked items (loaded from incremental cache if source unchanged)
        cached_marked: Option<HashSet<String>>,
        /// Source file path for this crate (used during construction for hashing)
        #[allow(dead_code)]
        source_path: Option<PathBuf>,
        /// Crate name (for cache key)
        crate_name: Option<String>,
        /// Crate type (lib, bin, etc. - for cache key to avoid lib/bin collisions)
        crate_type: String,
        /// Source hash (for cache validation)
        source_hash: Option<String>,
        /// Whether incremental cache is enabled
        incremental_enabled: bool,
        /// Whether this is a dependency crate (from registry/git/vendor)
        is_dependency: bool,
        /// Cross-crate phase: "" (default/legacy), "analysis", or "build"
        phase: String,
    }

    impl VirtualSlicingCallbacks {
        fn new(args: &[String]) -> Self {
            // Check cross-crate phase
            let phase = env::var("CARGO_SLICER_PHASE").unwrap_or_default();

            // Check if incremental caching is enabled (default: on)
            let incremental_enabled = env::var("CARGO_SLICER_NO_CACHE").is_err();

            // Check if this is a dependency crate
            let is_dependency = is_dependency_crate_from_args(args);

            // Extract crate type (lib, bin, etc.) for cache key to avoid lib/bin collisions
            let crate_type = extract_crate_type_from_args(args);

            // Check if marked items are provided via file
            let preloaded_marked = env::var("CARGO_SLICER_MARKED_ITEMS")
                .ok()
                .and_then(|path| {
                    std::fs::read_to_string(&path)
                        .ok()
                        .map(|content| {
                            content.lines()
                                .map(|s| s.trim().to_string())
                                .filter(|s| !s.is_empty() && !s.starts_with('#'))
                                .collect()
                        })
                });

            if is_debug_enabled() && preloaded_marked.is_some() {
                debug_log("[cargo-slicer-rustc] Loaded marked items from file");
            }

            // Extract source path and crate name for caching
            let source_path = extract_source_path(args);
            let crate_name = extract_crate_name_from_args(args);

            // In "analysis" phase, we only need source hash for the .analysis file.
            // No cache loading needed — we always collect fresh data.
            if phase == "analysis" {
                let source_hash = source_path.as_ref()
                    .and_then(|p| compute_source_hash(p));

                if is_debug_enabled() {
                    debug_log(&format!(
                        "[cargo-slicer-rustc] Analysis phase for {:?}/{}", crate_name, crate_type
                    ));
                }

                return Self {
                    preloaded_marked: None,
                    cached_marked: None,
                    source_path,
                    crate_name,
                    crate_type,
                    source_hash,
                    incremental_enabled: false,
                    is_dependency,
                    phase,
                };
            }

            // In "build" phase, load from cache (written by cross-crate BFS).
            // Skip source hash computation — trust the cache from phase 2.
            if phase == "build" {
                if let Some(ref crate_nm) = crate_name {
                    if let Some(cache_path) = get_cache_path(crate_nm, &crate_type) {
                        if let Some(entry) = load_cache_entry(&cache_path) {
                            if is_debug_enabled() {
                                debug_log(&format!(
                                    "[cargo-slicer-rustc] Build phase: loaded {} cached items for {}/{}",
                                    entry.marked_items.len(), crate_nm, crate_type
                                ));
                            }
                            return Self {
                                preloaded_marked: None,
                                cached_marked: Some(entry.marked_items),
                                source_path,
                                crate_name,
                                crate_type,
                                source_hash: None,
                                incremental_enabled: false,
                                is_dependency,
                                phase,
                            };
                        } else if is_debug_enabled() {
                            debug_log(&format!(
                                "[cargo-slicer-rustc] Build phase: no cache for {}/{}, keeping all items",
                                crate_nm, crate_type
                            ));
                        }
                    }
                }

                return Self {
                    preloaded_marked: None,
                    cached_marked: None,
                    source_path,
                    crate_name,
                    crate_type,
                    source_hash: None,
                    incremental_enabled: false,
                    is_dependency,
                    phase,
                };
            }

            // Auto-lean mode: if both .cache and .seeds exist (from cross_crate_bfs),
            // use pre-computed marked set. The presence of .seeds indicates
            // pre-analysis ran, so we trust the cache without hash validation.
            if let Some(ref crate_nm) = crate_name {
                if let Some(cache_path) = get_cache_path(crate_nm, &crate_type) {
                    if let Some(entry) = load_cache_entry(&cache_path) {
                        let seeds_exists = get_cache_dir()
                            .map(|d| d.join(format!("{}-{}.seeds", crate_nm, crate_type)).exists())
                            .unwrap_or(false);
                        if seeds_exists {
                            if is_debug_enabled() {
                                debug_log(&format!(
                                    "[cargo-slicer-rustc] Auto-lean: loaded {} cached items for {}/{} (seeds+cache present)",
                                    entry.marked_items.len(), crate_nm, crate_type
                                ));
                            }
                            return Self {
                                preloaded_marked: preloaded_marked,
                                cached_marked: Some(entry.marked_items),
                                source_path,
                                crate_name,
                                crate_type,
                                source_hash: None,
                                incremental_enabled: false,
                                is_dependency,
                                phase,
                            };
                        }
                    }
                }
            }

            // Default phase (legacy mode): check cache existence FIRST, then
            // compute source hash only if a cache file exists. This avoids
            // expensive hash computation on fresh builds where there's no cache.
            let (source_hash, cached_marked) = if incremental_enabled {
                if let (Some(ref src_path), Some(ref crate_nm)) = (&source_path, &crate_name) {
                    if let Some(cache_path) = get_cache_path(crate_nm, &crate_type) {
                        if let Some(entry) = load_cache_entry(&cache_path) {
                            // Cache file exists — now compute hash to validate
                            if let Some(hash) = compute_source_hash(src_path) {
                                if entry.source_hash == hash {
                                    if is_debug_enabled() {
                                        debug_log(&format!(
                                            "[cargo-slicer-rustc] Cache HIT for {}/{} (hash: {})",
                                            crate_nm, crate_type, hash
                                        ));
                                    }
                                    return Self {
                                        preloaded_marked,
                                        cached_marked: Some(entry.marked_items),
                                        source_path,
                                        crate_name,
                                        crate_type,
                                        source_hash: Some(hash),
                                        incremental_enabled,
                                        is_dependency,
                                        phase,
                                    };
                                } else if is_debug_enabled() {
                                    debug_log(&format!(
                                        "[cargo-slicer-rustc] Cache MISS for {} (hash changed: {} -> {})",
                                        crate_nm, entry.source_hash, hash
                                    ));
                                }
                                (Some(hash), None)
                            } else {
                                (None, None)
                            }
                        } else {
                            // No cache file — skip hash computation entirely
                            if is_debug_enabled() {
                                debug_log(&format!(
                                    "[cargo-slicer-rustc] Cache MISS for {} (no cache file, skipping hash)",
                                    crate_nm
                                ));
                            }
                            (None, None)
                        }
                    } else {
                        (None, None)
                    }
                } else {
                    (None, None)
                }
            } else {
                (None, None)
            };

            Self {
                preloaded_marked,
                cached_marked,
                source_path,
                crate_name,
                crate_type,
                source_hash,
                incremental_enabled,
                is_dependency,
                phase,
            }
        }
    }

    impl Callbacks for VirtualSlicingCallbacks {
        fn config(&mut self, config: &mut interface::Config) {
            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Virtual slicing config (phase={:?})", self.phase));
            }

            // In "analysis" phase: skip pre-storing marked items and skip optimized_mir override.
            // We only need mir_built override (to collect pre-opt call edges).
            // In "build" phase: skip mir_built override, only need optimized_mir override with cached items.

            // Pre-store marked items BEFORE queries run, so the optimized_mir override
            // can see them. Skip in analysis phase (no codegen filtering).
            if self.phase != "analysis" {
                if let Some(ref preloaded) = self.preloaded_marked {
                    if store_marked_items(preloaded.clone()) {
                        if is_debug_enabled() {
                            debug_log(&format!("[cargo-slicer-rustc] Pre-stored {} preloaded marked items in config()",
                                     preloaded.len()));
                        }
                    }
                } else if let Some(ref cached) = self.cached_marked {
                    if store_marked_items(cached.clone()) {
                        if is_debug_enabled() {
                            debug_log(&format!("[cargo-slicer-rustc] Pre-stored {} cached marked items in config()",
                                     cached.len()));
                        }
                    }
                }
            }

            // MIR body replacement: replace unmarked functions' MIR with abort stubs.
            // Only enabled when CARGO_SLICER_CODEGEN_FILTER=1 is set.
            // In "analysis" phase: skip (no codegen filtering, just data collection).
            let enable_codegen_filter = env::var("CARGO_SLICER_CODEGEN_FILTER").is_ok()
                && !self.is_dependency
                && self.phase != "analysis";

            // Initialize the call graph for pre-optimization reference tracking.
            // Skip in "build" phase (no data collection needed).
            if self.phase != "build" {
                cargo_slicer::rustc_integration::init_call_graph();
            }

            // Determine which query overrides to install based on phase:
            // - "analysis": mir_built only (collect pre-opt call edges), no optimized_mir override
            // - "build": optimized_mir only (stub unmarked items), no mir_built override
            // - "" (default): both overrides (legacy single-pass mode)
            // Skip mir_built override when we have cached marked items —
            // call edges are only needed for BFS, which is skipped on cache hit.
            let need_mir_built = self.phase != "build"
                && self.cached_marked.is_none()
                && (enable_codegen_filter || self.phase == "analysis");
            let need_optimized_mir = enable_codegen_filter;

            if need_mir_built || need_optimized_mir {
                use std::sync::OnceLock;
                type OptimizedMirFn = for<'tcx> fn(
                    rustc_middle::ty::TyCtxt<'tcx>,
                    rustc_hir::def_id::LocalDefId,
                ) -> &'tcx rustc_middle::mir::Body<'tcx>;
                static ORIGINAL_OPTIMIZED_MIR: OnceLock<OptimizedMirFn> = OnceLock::new();

                type MirBuiltFn = for<'tcx> fn(
                    rustc_middle::ty::TyCtxt<'tcx>,
                    rustc_hir::def_id::LocalDefId,
                ) -> &'tcx rustc_data_structures::steal::Steal<rustc_middle::mir::Body<'tcx>>;
                static ORIGINAL_MIR_BUILT: OnceLock<MirBuiltFn> = OnceLock::new();

                // Use static flags to communicate phase to query overrides (fn pointers can't capture)
                static ENABLE_MIR_BUILT: OnceLock<bool> = OnceLock::new();
                static ENABLE_OPTIMIZED_MIR: OnceLock<bool> = OnceLock::new();
                ENABLE_MIR_BUILT.set(need_mir_built).ok();
                ENABLE_OPTIMIZED_MIR.set(need_optimized_mir).ok();

                config.override_queries = Some(|_session, providers| {
                    ORIGINAL_OPTIMIZED_MIR.set(providers.optimized_mir).ok();
                    ORIGINAL_MIR_BUILT.set(providers.mir_built).ok();

                    let mir_built_enabled = ENABLE_MIR_BUILT.get().copied().unwrap_or(false);
                    let optimized_mir_enabled = ENABLE_OPTIMIZED_MIR.get().copied().unwrap_or(false);

                    if mir_built_enabled {
                        // Override mir_built to collect call edges from pre-optimization MIR.
                        // This captures references BEFORE inlining removes them.
                        providers.mir_built = |tcx, def_id| {
                            let original = ORIGINAL_MIR_BUILT.get()
                                .expect("original mir_built not stored");
                            let result = original(tcx, def_id);

                            // Collect call edges from the pre-optimization MIR body
                            let body = result.borrow();
                            let caller = tcx.def_path_str(def_id.to_def_id());

                            // Helper: record a FnDef reference from a constant type
                            let record_fndef = |ty: rustc_middle::ty::Ty<'_>, caller: &str| {
                                if let rustc_middle::ty::TyKind::FnDef(callee_id, _) = ty.kind() {
                                    let callee = tcx.def_path_str(*callee_id);
                                    if !callee.starts_with("std::") &&
                                       !callee.starts_with("core::") &&
                                       !callee.starts_with("alloc::") {
                                        cargo_slicer::rustc_integration::record_call_edge(
                                            caller.to_string(), callee);
                                    }
                                }
                            };

                            // Scan ALL constants in the MIR body for FnDef references
                            // AND detect unsizing coercions (vtable construction).
                            for bb in body.basic_blocks.iter() {
                                for stmt in &bb.statements {
                                    if let rustc_middle::mir::StatementKind::Assign(box (_, ref rvalue)) = stmt.kind {
                                        match rvalue {
                                            rustc_middle::mir::Rvalue::Use(rustc_middle::mir::Operand::Constant(c)) |
                                            rustc_middle::mir::Rvalue::Cast(_, rustc_middle::mir::Operand::Constant(c), _) |
                                            rustc_middle::mir::Rvalue::UnaryOp(_, rustc_middle::mir::Operand::Constant(c)) |
                                            rustc_middle::mir::Rvalue::Repeat(rustc_middle::mir::Operand::Constant(c), _) => {
                                                let ty = match c.const_ {
                                                    rustc_middle::mir::Const::Val(_, ty) => ty,
                                                    rustc_middle::mir::Const::Ty(ty, _) => ty,
                                                    rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                                                };
                                                record_fndef(ty, &caller);
                                            }
                                            // Detect unsizing coercions: &T -> &dyn Trait
                                            // This records which traits have vtables constructed.
                                            rustc_middle::mir::Rvalue::Cast(
                                                rustc_middle::mir::CastKind::PointerCoercion(
                                                    rustc_middle::ty::adjustment::PointerCoercion::Unsize, _
                                                ),
                                                _,
                                                target_ty,
                                            ) => {
                                                // Peel through references/pointers to find dyn Trait
                                                let inner_ty = target_ty.builtin_deref(true)
                                                    .unwrap_or(*target_ty);
                                                if let rustc_middle::ty::TyKind::Dynamic(data, _) = inner_ty.kind() {
                                                    if let Some(principal) = data.principal() {
                                                        let trait_def_id: rustc_span::def_id::DefId = principal.def_id();
                                                        cargo_slicer::rustc_integration::record_vtable_trait(
                                                            trait_def_id.index.as_u32() as u64
                                                        );
                                                        if is_debug_enabled() {
                                                            let trait_path = tcx.def_path_str(trait_def_id);
                                                            debug_log(&format!(
                                                                "[vtable] {} creates dyn {}",
                                                                caller, trait_path
                                                            ));
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }

                                if let Some(ref term) = bb.terminator {
                                    if let rustc_middle::mir::TerminatorKind::Call { ref func, ref args, .. } = term.kind {
                                        if let rustc_middle::mir::Operand::Constant(constant) = func {
                                            let ty = match constant.const_ {
                                                rustc_middle::mir::Const::Val(_, ty) => ty,
                                                rustc_middle::mir::Const::Ty(ty, _) => ty,
                                                rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                                            };
                                            record_fndef(ty, &caller);
                                        }
                                        for arg in args {
                                            if let rustc_middle::mir::Operand::Constant(c) = &arg.node {
                                                let ty = match c.const_ {
                                                    rustc_middle::mir::Const::Val(_, ty) => ty,
                                                    rustc_middle::mir::Const::Ty(ty, _) => ty,
                                                    rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                                                };
                                                record_fndef(ty, &caller);
                                            }
                                        }
                                    }
                                }
                            }

                            result
                        };
                    }

                    if optimized_mir_enabled {
                        providers.optimized_mir = |tcx, def_id| {
                            use cargo_slicer::rustc_integration::{is_item_marked_by_def_id, is_safe_to_skip};
                            use rustc_middle::mir::*;
                            use rustc_index::IndexVec;

                            let original = ORIGINAL_OPTIMIZED_MIR.get()
                                .expect("original optimized_mir not stored");

                            let full_def_id = def_id.to_def_id();
                            let is_marked = is_item_marked_by_def_id(tcx, full_def_id);
                            let safe_to_skip = if !is_marked { is_safe_to_skip(tcx, full_def_id) } else { false };

                            if !is_marked && safe_to_skip {
                                cargo_slicer::rustc_integration::increment_stub_count();
                                if is_debug_enabled() {
                                    let path = tcx.def_path_str(full_def_id);
                                    debug_log(&format!("[codegen-filter] STUB: {}", path));
                                }
                            }

                            if !is_marked && safe_to_skip {
                                let span = tcx.def_span(full_def_id);
                                let source_info = SourceInfo::outermost(span);

                                let block = BasicBlockData::new(
                                    Some(Terminator {
                                        source_info,
                                        kind: TerminatorKind::Unreachable,
                                    }),
                                    false,
                                );
                                let blocks = IndexVec::from_raw(vec![block]);

                                let sig = tcx.fn_sig(full_def_id).instantiate_identity();
                                let sig = tcx.instantiate_bound_regions_with_erased(sig);

                                let mut local_decls = IndexVec::new();
                                local_decls.push(LocalDecl::new(sig.output(), span));
                                for &arg_ty in sig.inputs() {
                                    local_decls.push(LocalDecl::new(arg_ty, span).immutable());
                                }

                                // Construct minimal source_scopes directly instead of
                                // calling original(tcx, def_id) which runs the full MIR
                                // optimization pipeline only to discard everything.
                                let source_scopes = IndexVec::from_raw(vec![SourceScopeData {
                                    span,
                                    parent_scope: None,
                                    inlined: None,
                                    inlined_parent_scope: None,
                                    local_data: ClearCrossCrate::Clear,
                                }]);

                                let source = MirSource::item(full_def_id);
                                let mut body = Body::new(
                                    source,
                                    blocks,
                                    source_scopes,
                                    local_decls,
                                    IndexVec::new(),
                                    sig.inputs().len(),
                                    vec![],
                                    span,
                                    None,
                                    None,
                                );
                                body.set_required_consts(Vec::new());
                                body.set_mentioned_items(Vec::new());

                                return tcx.arena.alloc(body);
                            }
                            original(tcx, def_id)
                        };
                    }
                });

                if is_debug_enabled() {
                    if need_mir_built {
                        debug_log("[cargo-slicer-rustc] mir_built override ENABLED (call edge collection)");
                    }
                    if need_optimized_mir {
                        debug_log("[cargo-slicer-rustc] optimized_mir override ENABLED (codegen filtering)");
                    }
                }
            }

            if is_debug_enabled() {
                if self.is_dependency {
                    debug_log("[cargo-slicer-rustc] Virtual slicing analysis mode (dependency crate)");
                } else {
                    debug_log("[cargo-slicer-rustc] Virtual slicing analysis mode (local crate)");
                }
            }
        }

        fn after_analysis<'tcx>(
            &mut self,
            _compiler: &interface::Compiler,
            tcx: rustc_middle::ty::TyCtxt<'tcx>,
        ) -> rustc_driver::Compilation {
            use rustc_span::def_id::LOCAL_CRATE;

            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] After analysis (phase={:?})", self.phase));
            }

            let crate_name = tcx.crate_name(LOCAL_CRATE);

            // ── Analysis phase: collect data, write .analysis file, return ──
            if self.phase == "analysis" {
                return self.after_analysis_phase(tcx, &crate_name.to_string());
            }

            // ── Build phase: use cached marked items, skip data collection ──
            if self.phase == "build" {
                return self.after_build_phase(tcx, &crate_name.to_string());
            }

            // ── Default phase (legacy): full analysis + BFS + cache ──
            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Analyzing crate for virtual slicing: {}", crate_name));
            }

            // Priority: preloaded > cached > compute fresh
            let (marked_items, needs_cache_update) = if let Some(ref preloaded) = self.preloaded_marked {
                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Using {} preloaded marked items", preloaded.len()));
                }
                (preloaded.clone(), false)
            } else if let Some(ref cached) = self.cached_marked {
                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Using {} cached marked items (skipping analysis)", cached.len()));
                }
                (cached.clone(), false)
            } else {
                // Collect usage data and compute marked items via BFS reachability
                let usage_data = cargo_slicer::rustc_integration::hir_visitor::collect_from_hir(tcx);

                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Collected {} defined items, {} dependencies",
                             usage_data.defined_items.len(),
                             usage_data.item_dependencies.len()));
                }

                // BFS reachability from seeds
                let mut seeds: HashSet<String> = HashSet::new();
                let is_binary = tcx.entry_fn(()).is_some();

                // Check for cross-crate .seeds file (written by post-build BFS)
                // If present, use restricted seeds instead of all-pub
                let seeds_loaded = if !is_binary {
                    if let Some(ref crate_nm) = self.crate_name {
                        load_seeds_file(crate_nm, &self.crate_type, &mut seeds)
                    } else {
                        false
                    }
                } else {
                    false
                };

                if !seeds_loaded {
                    // No .seeds file — fall back to default seed computation
                    for (path, info) in &usage_data.defined_items {
                        if is_binary {
                            if path == "main" || path.ends_with("::main") {
                                seeds.insert(path.clone());
                            }
                        } else {
                            if info.visibility != cargo_slicer::rustc_integration::Visibility::Private {
                                seeds.insert(path.clone());
                            }
                        }
                    }
                }

                // Always add unconditional seeds: #[test], #[no_mangle], Drop impls, statics
                collect_seeds_from_hir(tcx, is_binary, &mut seeds);

                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] BFS seeds: {} (binary: {}, cross-crate seeds: {})",
                             seeds.len(), is_binary, seeds_loaded));
                }

                // BFS from seeds
                let pre_opt_graph = cargo_slicer::rustc_integration::get_call_graph();
                if is_debug_enabled() {
                    if let Some(ref graph) = pre_opt_graph {
                        let total_edges: usize = graph.values().map(|s| s.len()).sum();
                        debug_log(&format!("[cargo-slicer-rustc] Call graph: {} callers, {} total edges",
                            graph.len(), total_edges));
                    }
                }
                let seed_count = seeds.len();
                let mut marked: HashSet<String> = HashSet::new();
                let mut queue: std::collections::VecDeque<String> = seeds.into_iter().collect();

                while let Some(item) = queue.pop_front() {
                    if !marked.insert(item.clone()) {
                        continue;
                    }

                    if let Some(deps) = usage_data.item_dependencies.get(&item) {
                        for dep in deps {
                            if !marked.contains(dep) {
                                queue.push_back(dep.clone());
                            }
                        }
                    }

                    if let Some(ref graph) = pre_opt_graph {
                        if let Some(deps) = graph.get(&item) {
                            for dep in deps {
                                if !marked.contains(dep) {
                                    queue.push_back(dep.clone());
                                }
                            }
                        }
                    }
                }

                for (path, _refs) in &usage_data.referenced_items {
                    if !usage_data.defined_items.contains_key(path) {
                        marked.insert(path.clone());
                    }
                }

                let total_defined = usage_data.defined_items.len();
                let unmarked_count = usage_data.defined_items.keys()
                    .filter(|path| !marked.contains(*path))
                    .count();

                if is_debug_enabled() {
                    let vtable_count = cargo_slicer::rustc_integration::vtable_trait_count();
                    debug_log(&format!("[cargo-slicer-rustc] BFS: {} seeds, {} defined, {} marked, {} defined-unmarked ({:.1}% reduction), {} vtable traits",
                             seed_count, total_defined, marked.len(), unmarked_count,
                             if total_defined > 0 { unmarked_count as f64 / total_defined as f64 * 100.0 } else { 0.0 },
                             vtable_count));
                    if unmarked_count > 0 && unmarked_count <= 50 {
                        for path in usage_data.defined_items.keys().filter(|p| !marked.contains(*p)) {
                            debug_log(&format!("[cargo-slicer-rustc]   UNMARKED defined: {}", path));
                        }
                    }
                }

                // Write skip-driver marker if no FUNCTION items are unmarked.
                // Non-function items (structs, enums, mods, traits) never go through
                // optimized_mir so they can't be stubbed — ignore them for skip-driver.
                // On subsequent incremental builds, dispatch will skip loading the driver.
                let unmarked_fn_count = usage_data.defined_items.iter()
                    .filter(|(path, info)| {
                        info.kind == cargo_slicer::rustc_integration::ItemKind::Function
                            && !marked.contains(*path)
                    })
                    .count();
                if unmarked_fn_count == 0 {
                    if let Some(ref crate_nm) = self.crate_name {
                        if let Some(cache_dir) = get_cache_dir() {
                            let skip_path = cache_dir.join(format!("{}-{}.skip-driver", crate_nm, self.crate_type));
                            let _ = std::fs::write(&skip_path, "no unmarked functions\n");
                            if is_debug_enabled() {
                                debug_log(&format!("[cargo-slicer-rustc] Wrote skip-driver marker for {}/{} (0 unmarked functions)", crate_nm, self.crate_type));
                            }
                        }
                    }
                }

                // Write .analysis file for cross-crate BFS (when enabled).
                // Piggyback on the already-collected data (no redundant traversals).
                if env::var("CARGO_SLICER_CROSS_CRATE").is_ok() {
                    self.write_analysis_file(tcx, &usage_data, &pre_opt_graph);
                }

                (marked, self.incremental_enabled)
            };

            // Save to cache if we computed fresh results
            if needs_cache_update {
                if let Some(ref crate_nm) = self.crate_name {
                    // Compute hash lazily if not already computed (happens on
                    // fresh builds where we skipped hash to avoid wasted I/O)
                    let hash = self.source_hash.clone().or_else(|| {
                        self.source_path.as_ref().and_then(|p| compute_source_hash(p))
                    });
                    if let Some(ref hash) = hash {
                        if let Some(cache_path) = get_cache_path(crate_nm, &self.crate_type) {
                            if save_cache_entry(&cache_path, hash, &marked_items) {
                                if is_debug_enabled() {
                                    debug_log(&format!(
                                        "[cargo-slicer-rustc] Saved cache for {}/{} ({} items, hash: {})",
                                        crate_nm, self.crate_type, marked_items.len(), hash
                                    ));
                                }
                            } else if is_debug_enabled() {
                                debug_log(&format!(
                                    "[cargo-slicer-rustc] Failed to save cache for {}",
                                    crate_nm
                                ));
                            }
                        }
                    }
                }
            }

            // Store marked items for the query override to access
            if !store_marked_items(marked_items.clone()) {
                if is_debug_enabled() {
                    debug_log("[cargo-slicer-rustc] WARNING: Marked items were already set");
                }
            }

            // Optionally write marked items to file for debugging
            if let Ok(output_path) = env::var("CARGO_SLICER_MARKED_OUT") {
                let content: String = marked_items.iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join("\n");
                if let Err(e) = std::fs::write(&output_path, content) {
                    if is_debug_enabled() {
                        debug_log(&format!("[cargo-slicer-rustc] ERROR: Failed to write marked items: {}", e));
                    }
                } else if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Wrote marked items to {}", output_path));
                }
            }

            // Generate linker version script from marked items
            if let Ok(script_dir) = env::var("CARGO_SLICER_VSCRIPT_DIR") {
                let crate_nm = self.crate_name.as_deref().unwrap_or("unknown");
                let script_path = PathBuf::from(&script_dir)
                    .join(format!("{}.ld", crate_nm));

                if let Err(e) = generate_version_script(&marked_items, crate_nm, &script_path) {
                    if is_debug_enabled() {
                        debug_log(&format!("[cargo-slicer-rustc] Failed to write version script: {}", e));
                    }
                } else if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Wrote version script to {}", script_path.display()));
                }
            }

            rustc_driver::Compilation::Continue
        }
    }

    impl VirtualSlicingCallbacks {
        /// Analysis phase: collect data from HIR/MIR and write .analysis file.
        /// Does NOT compute BFS or store marked items.
        fn after_analysis_phase<'tcx>(
            &mut self,
            tcx: rustc_middle::ty::TyCtxt<'tcx>,
            crate_name: &str,
        ) -> rustc_driver::Compilation {
            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Analysis phase for crate: {}", crate_name));
            }

            // Collect usage data (defined items + MIR dependencies)
            let usage_data = cargo_slicer::rustc_integration::hir_visitor::collect_from_hir(tcx);

            // Get pre-optimization call graph (collected by mir_built override)
            let pre_opt_graph = cargo_slicer::rustc_integration::get_call_graph();

            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Analysis: {} defined items, {} deps, {} call graph callers",
                    usage_data.defined_items.len(),
                    usage_data.item_dependencies.len(),
                    pre_opt_graph.as_ref().map_or(0, |g| g.len())));
            }

            // Write .analysis file using shared method
            self.write_analysis_file(tcx, &usage_data, &pre_opt_graph);

            // Continue compilation (cargo check will finish without codegen)
            rustc_driver::Compilation::Continue
        }

        /// Build phase: use pre-computed cache from cross-crate BFS.
        /// Skip data collection entirely.
        fn after_build_phase<'tcx>(
            &mut self,
            _tcx: rustc_middle::ty::TyCtxt<'tcx>,
            crate_name: &str,
        ) -> rustc_driver::Compilation {
            // Marked items should already be pre-stored in config() from cache.
            // If not (no cache file), everything will be kept (conservative).
            if is_debug_enabled() {
                if self.cached_marked.is_some() {
                    debug_log(&format!("[cargo-slicer-rustc] Build phase for {}: using cached marked items", crate_name));
                } else {
                    debug_log(&format!("[cargo-slicer-rustc] Build phase for {}: no cache, keeping all items", crate_name));
                }
            }

            // If marked items weren't pre-stored in config() (because cached_marked was None),
            // store an empty marker so the optimized_mir override keeps everything.
            if self.cached_marked.is_none() && self.preloaded_marked.is_none() {
                // Don't call store_marked_items — without calling it, MARKED_ITEMS stays unset,
                // and is_item_marked() returns true (conservative: keep everything).
            }

            rustc_driver::Compilation::Continue
        }

        /// Write .analysis file for cross-crate BFS.
        /// Called from both analysis phase and default phase (when CARGO_SLICER_CROSS_CRATE=1).
        ///
        /// Takes pre-collected data to avoid redundant HIR/MIR traversals.
        fn write_analysis_file<'tcx>(
            &self,
            tcx: rustc_middle::ty::TyCtxt<'tcx>,
            usage_data: &cargo_slicer::rustc_integration::UsageData,
            pre_opt_graph: &Option<std::collections::HashMap<String, HashSet<String>>>,
        ) {
            use rustc_span::def_id::LOCAL_CRATE;

            let cache_dir = match get_cache_dir() {
                Some(d) => d,
                None => return,
            };

            let crate_name_sym = tcx.crate_name(LOCAL_CRATE);
            let crate_nm = self.crate_name.as_deref().unwrap_or(&crate_name_sym.as_str());
            let analysis_path = cache_dir.join(format!("{}-{}.analysis", crate_nm, self.crate_type));

            let is_binary = tcx.entry_fn(()).is_some();

            // Collect trait impls, statics, no_mangle from HIR
            let mut trait_impls: Vec<(String, bool, bool, String)> = Vec::new();
            let mut statics: Vec<String> = Vec::new();
            let mut no_mangle: Vec<String> = Vec::new();

            for id in tcx.hir_free_items() {
                let item = tcx.hir_item(id);
                let def_id = item.owner_id.def_id;

                let def_kind = tcx.def_kind(def_id.to_def_id());
                if def_kind.is_fn_like() {
                    let attrs = tcx.codegen_fn_attrs(def_id.to_def_id());
                    use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
                    if attrs.flags.contains(CodegenFnAttrFlags::NO_MANGLE) {
                        no_mangle.push(tcx.def_path_str(def_id.to_def_id()));
                    }
                }

                if let rustc_hir::ItemKind::Impl(impl_item) = &item.kind {
                    if let Some(trait_ref) = impl_item.of_trait {
                        if let rustc_hir::def::Res::Def(_, trait_def_id) = trait_ref.trait_ref.path.res {
                            let trait_path = tcx.def_path_str(trait_def_id);
                            let is_drop = tcx.lang_items().drop_trait() == Some(trait_def_id);
                            let is_object_safe = tcx.is_dyn_compatible(trait_def_id);

                            for &impl_item_id in impl_item.items {
                                let impl_def_id = impl_item_id.owner_id.def_id;
                                let method_path = tcx.def_path_str(impl_def_id.to_def_id());
                                trait_impls.push((trait_path.clone(), is_drop, is_object_safe, method_path));
                            }
                        }
                    }
                }

                if matches!(&item.kind, rustc_hir::ItemKind::Static(..)) {
                    statics.push(tcx.def_path_str(def_id.to_def_id()));
                }
            }

            // Write .analysis file
            let mut content = String::new();
            content.push_str(&format!("CRATE_NAME:{}\n", crate_nm));
            content.push_str(&format!("CRATE_TYPE:{}\n", self.crate_type));
            content.push_str(&format!("SOURCE_HASH:{}\n", self.source_hash.as_deref().unwrap_or("")));
            content.push_str(&format!("IS_BINARY:{}\n", is_binary));

            content.push_str("DEFINED_ITEMS:\n");
            let mut sorted_items: Vec<_> = usage_data.defined_items.iter().collect();
            sorted_items.sort_by_key(|(path, _)| path.as_str());
            for (path, info) in &sorted_items {
                let vis = if info.visibility != cargo_slicer::rustc_integration::Visibility::Private { "pub" } else { "priv" };
                content.push_str(&format!("{}\t{}\n", path, vis));
            }

            content.push_str("CALL_EDGES:\n");
            let mut all_edges: std::collections::BTreeSet<(String, String)> = std::collections::BTreeSet::new();
            for (caller, deps) in &usage_data.item_dependencies {
                for callee in deps {
                    all_edges.insert((caller.clone(), callee.clone()));
                }
            }
            if let Some(ref graph) = pre_opt_graph {
                for (caller, deps) in graph {
                    for callee in deps {
                        all_edges.insert((caller.clone(), callee.clone()));
                    }
                }
            }
            for (caller, callee) in &all_edges {
                content.push_str(&format!("{}\t{}\n", caller, callee));
            }

            content.push_str("TRAIT_IMPLS:\n");
            for (tp, is_drop, is_obj_safe, mp) in &trait_impls {
                content.push_str(&format!("{}\t{}\t{}\t{}\n", tp, is_drop, is_obj_safe, mp));
            }
            content.push_str("STATICS:\n");
            for s in &statics { content.push_str(&format!("{}\n", s)); }
            content.push_str("NO_MANGLE:\n");
            for nm in &no_mangle { content.push_str(&format!("{}\n", nm)); }

            if let Err(e) = std::fs::write(&analysis_path, &content) {
                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Failed to write analysis file: {}", e));
                }
            } else if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Wrote .analysis: {} ({} items, {} edges)",
                    analysis_path.display(), usage_data.defined_items.len(), all_edges.len()));
            }
        }
    }

    let mut callbacks = VirtualSlicingCallbacks::new(args);

    rustc_driver::catch_with_exit_code(|| {
        rustc_driver::run_compiler(args, &mut callbacks)
    })
}

/// Collect seed items from HIR: #[test], #[no_mangle], Drop impls, object-safe trait impls, statics.
/// Shared by both default phase and analysis phase.
fn collect_seeds_from_hir<'tcx>(
    tcx: rustc_middle::ty::TyCtxt<'tcx>,
    is_binary: bool,
    seeds: &mut HashSet<String>,
) {
    for id in tcx.hir_free_items() {
        let item = tcx.hir_item(id);
        let def_id = item.owner_id.def_id;

        // Seed #[test] and #[no_mangle] functions
        let is_test = tcx.get_attrs(def_id.to_def_id(), rustc_span::symbol::sym::test).next().is_some();
        let is_no_mangle = {
            let def_kind = tcx.def_kind(def_id.to_def_id());
            if def_kind.is_fn_like() {
                let attrs = tcx.codegen_fn_attrs(def_id.to_def_id());
                use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
                attrs.flags.contains(CodegenFnAttrFlags::NO_MANGLE)
            } else {
                false
            }
        };

        if is_test || is_no_mangle {
            let path = tcx.def_path_str(def_id.to_def_id());
            seeds.insert(path);
        }

        if let rustc_hir::ItemKind::Impl(impl_item) = &item.kind {
            let is_trait_impl = impl_item.of_trait.is_some();

            if is_binary {
                if is_trait_impl {
                    if let Some(trait_ref) = impl_item.of_trait {
                        if let rustc_hir::def::Res::Def(_, trait_def_id) = trait_ref.trait_ref.path.res {
                            let is_object_safe = tcx.is_dyn_compatible(trait_def_id);
                            let is_drop = tcx.lang_items().drop_trait() == Some(trait_def_id);

                            if is_drop || is_object_safe {
                                for &impl_item_id in impl_item.items {
                                    let impl_def_id = impl_item_id.owner_id.def_id;
                                    let path = tcx.def_path_str(impl_def_id.to_def_id());
                                    seeds.insert(path);
                                }
                            }
                        }
                    }
                }
            } else {
                if is_trait_impl {
                    for &impl_item_id in impl_item.items {
                        let impl_def_id = impl_item_id.owner_id.def_id;
                        let path = tcx.def_path_str(impl_def_id.to_def_id());
                        seeds.insert(path);
                    }
                }
            }
        }

        // Seed statics
        if matches!(&item.kind, rustc_hir::ItemKind::Static(..)) {
            let path = tcx.def_path_str(def_id.to_def_id());
            seeds.insert(path);
        }
    }
}

/// Generate a linker version script that exports only marked symbols.
///
/// The version script tells the linker to export marked symbols and hide
/// everything else. Combined with `--gc-sections`, this gives additional
/// binary size reduction beyond what MIR stub replacement provides.
fn generate_version_script(
    marked_items: &HashSet<String>,
    crate_name: &str,
    output_path: &Path,
) -> Result<(), String> {
    let mut script = String::with_capacity(marked_items.len() * 64);

    script.push_str("/* Generated by cargo-slicer virtual slicing */\n");
    script.push_str(&format!("/* Crate: {} - {} marked items */\n\n", crate_name, marked_items.len()));

    script.push_str("{\n");
    script.push_str("  global:\n");

    // Always export essential symbols
    script.push_str("    main;\n");
    script.push_str("    rust_begin_unwind;\n");
    script.push_str("    rust_panic;\n");
    script.push_str("    __rust_*;\n");
    script.push_str("    DW.ref.rust_eh_personality;\n");

    // Export marked symbols using glob patterns matching Rust mangling
    let rust_crate_name = crate_name.replace('-', "_");

    // Sort for deterministic output
    let mut sorted_items: Vec<_> = marked_items.iter().collect();
    sorted_items.sort();

    for item in &sorted_items {
        // Generate glob pattern for mangled Rust symbols
        // Legacy mangling pattern: _ZN{len}{name}...E
        let parts: Vec<&str> = item.split("::").collect();
        if parts.is_empty() {
            continue;
        }

        let mut pattern = format!("*{}*", rust_crate_name);
        for part in &parts {
            if !part.is_empty() {
                pattern.push_str(&format!("{}{}*", part.len(), part));
            }
        }
        script.push_str(&format!("    {};\n", pattern));
    }

    script.push_str("\n  local:\n");
    script.push_str("    *;\n");
    script.push_str("};\n");

    // Create parent directory if needed
    if let Some(parent) = output_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }

    std::fs::write(output_path, script)
        .map_err(|e| format!("Failed to write version script: {}", e))
}

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

/// Get MIR-precise cache file path for a crate.
/// These are written by mir_harvester and contain more accurate marked items.
fn get_mir_cache_path(crate_name: &str) -> Option<PathBuf> {
    let cache_dir = get_cache_dir()?;
    Some(cache_dir.join(format!("{}.mir-cache", crate_name)))
}

/// Load MIR-precise cache entry, stripping the crate prefix from paths.
///
/// MIR-precise caches use `tcx.def_path_str()` which includes the crate prefix
/// (e.g., "zeroclaw::some::func"), but when the driver runs on that crate locally,
/// `tcx.def_path_str()` returns paths WITHOUT the prefix (e.g., "some::func").
fn load_mir_cache(cache_path: &Path, crate_name: &str) -> Option<HashSet<String>> {
    let content = std::fs::read_to_string(cache_path).ok()?;
    let mut lines = content.lines();

    // First line must be "mir-precise" header
    let header = lines.next()?;
    if header != "mir-precise" {
        return None;
    }

    let prefix = format!("{}::", crate_name);
    let marked_items: HashSet<String> = lines
        .filter(|s| !s.is_empty())
        .map(|path| {
            path.strip_prefix(&prefix).unwrap_or(path).to_string()
        })
        .collect();

    Some(marked_items)
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

/// Normalize a rustc-format trait impl path to syn-format for seed matching.
///
/// Rustc format: `<Type as some::Trait>::method`
/// Syn format:   `Type::method`
///
/// Returns the normalized syn-format path, or None if not a trait impl path.
fn normalize_trait_impl_path(path: &str) -> Option<String> {
    let inner = path.strip_prefix('<')?;
    let (type_part, rest) = inner.split_once(" as ")?;
    let (_trait_part, after_bracket) = rest.split_once('>')?;
    let method = after_bracket.strip_prefix("::")?;
    Some(format!("{}::{}", type_part, method))
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

/// Run BFS reachability from seeds, following both item_dependencies and pre-opt call graph edges.
/// Includes closure propagation (async fn closures) and external reference marking.
///
/// Shared by the default phase and lazy-metadata phase.
/// BFS reachability from seeds using interned string indices.
///
/// Interns all unique path strings into a u32 index table, builds adjacency
/// lists on indices, and runs BFS with zero string cloning. Only allocates
/// strings for the final result conversion.
fn run_bfs_from_seeds(
    seeds: HashSet<String>,
    usage_data: &cargo_slicer::rustc_integration::UsageData,
    pre_opt_graph: &Option<std::collections::HashMap<String, HashSet<String>>>,
) -> HashSet<String> {
    // Step 1: Collect all unique strings from all sources
    let mut all_strings: HashSet<&str> = HashSet::new();
    for seed in &seeds {
        all_strings.insert(seed.as_str());
    }
    for (k, deps) in &usage_data.item_dependencies {
        all_strings.insert(k.as_str());
        for dep in deps {
            all_strings.insert(dep.as_str());
        }
    }
    if let Some(ref graph) = pre_opt_graph {
        for (k, deps) in graph {
            all_strings.insert(k.as_str());
            for dep in deps {
                all_strings.insert(dep.as_str());
            }
        }
    }
    for (path, _) in &usage_data.referenced_items {
        all_strings.insert(path.as_str());
    }
    for (path, _) in &usage_data.defined_items {
        all_strings.insert(path.as_str());
    }

    // Step 2: Build index mapping
    let idx_to_str: Vec<&str> = all_strings.into_iter().collect();
    let n = idx_to_str.len();
    let str_to_idx: std::collections::HashMap<&str, u32> = idx_to_str.iter().enumerate()
        .map(|(i, &s)| (s, i as u32))
        .collect();

    // Step 3: Build adjacency lists (merged from both graphs)
    let mut adj: Vec<Vec<u32>> = vec![Vec::new(); n];
    for (k, deps) in &usage_data.item_dependencies {
        if let Some(&ki) = str_to_idx.get(k.as_str()) {
            for dep in deps {
                if let Some(&di) = str_to_idx.get(dep.as_str()) {
                    adj[ki as usize].push(di);
                }
            }
        }
    }
    if let Some(ref graph) = pre_opt_graph {
        for (k, deps) in graph {
            if let Some(&ki) = str_to_idx.get(k.as_str()) {
                for dep in deps {
                    if let Some(&di) = str_to_idx.get(dep.as_str()) {
                        adj[ki as usize].push(di);
                    }
                }
            }
        }
    }

    // Step 4: BFS from seeds (Phase 1)
    let mut marked = vec![false; n];
    let mut queue: std::collections::VecDeque<u32> = std::collections::VecDeque::new();
    for seed in &seeds {
        if let Some(&idx) = str_to_idx.get(seed.as_str()) {
            if !marked[idx as usize] {
                marked[idx as usize] = true;
                queue.push_back(idx);
            }
        }
    }
    while let Some(item) = queue.pop_front() {
        for &dep in &adj[item as usize] {
            if !marked[dep as usize] {
                marked[dep as usize] = true;
                queue.push_back(dep);
            }
        }
    }

    // Phase 2: Mark external references (items referenced but not defined locally)
    for (path, _refs) in &usage_data.referenced_items {
        if !usage_data.defined_items.contains_key(path) {
            if let Some(&idx) = str_to_idx.get(path.as_str()) {
                marked[idx as usize] = true;
            }
        }
    }

    // Phase 3: Propagate marks to closures of marked functions.
    // Only check nodes that have outgoing edges (keys in dependency/call graphs).
    {
        let mut closure_queue: std::collections::VecDeque<u32> = std::collections::VecDeque::new();
        for (i, edges) in adj.iter().enumerate() {
            if !edges.is_empty() && !marked[i] {
                let s = idx_to_str[i];
                if let Some(parent_end) = s.find("::{closure") {
                    let parent = &s[..parent_end];
                    if let Some(&pi) = str_to_idx.get(parent) {
                        if marked[pi as usize] {
                            closure_queue.push_back(i as u32);
                        }
                    }
                }
            }
        }
        while let Some(item) = closure_queue.pop_front() {
            if marked[item as usize] {
                continue;
            }
            marked[item as usize] = true;
            for &dep in &adj[item as usize] {
                if !marked[dep as usize] {
                    closure_queue.push_back(dep);
                }
            }
        }
    }

    // Step 5: Convert back to strings
    let mut result: HashSet<String> = HashSet::with_capacity(
        marked.iter().filter(|&&m| m).count()
    );
    for (i, &s) in idx_to_str.iter().enumerate() {
        if marked[i] {
            result.insert(s.to_string());
        }
    }
    result
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
            // Normalize backslashes for Windows path compatibility
            let normalized = arg.replace('\\', "/");
            if normalized.contains(".cargo/registry") || normalized.contains(".cargo/git") {
                return true;
            }
            if normalized.contains("/vendor/") {
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
            // Normalize backslashes for Windows path compatibility
            let normalized = arg.replace('\\', "/");
            // Dependency paths contain .cargo/registry or .cargo/git
            if normalized.contains(".cargo/registry") || normalized.contains(".cargo/git") {
                return true;
            }
            // Also check for common sliced crate patterns
            if normalized.contains("_sliced/") || normalized.contains("/sliced_crates/") {
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
    use std::process::Command;
    use cargo_slicer::process_ext::exec_command;

    // Check if sccache should be used (set CARGO_SLICER_USE_SCCACHE=1 to enable)
    let use_sccache = env::var("CARGO_SLICER_USE_SCCACHE").is_ok();

    if use_sccache {
        // Try to find sccache
        if let Some(sccache) = find_sccache() {
            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Using sccache: {}", sccache));
            }
            // sccache expects: sccache rustc [args...]
            exec_command(Command::new(&sccache).arg(rustc_path).args(args));
        }
    }

    // Fallback: call rustc directly
    exec_command(Command::new(rustc_path).args(args));
}

/// Find sccache binary
fn find_sccache() -> Option<String> {
    use std::process::Command;

    let sccache_name = format!("sccache{}", std::env::consts::EXE_SUFFIX);
    let which_cmd = if cfg!(windows) { "where" } else { "which" };

    // Check PATH via which/where
    if let Ok(output) = Command::new(which_cmd).arg("sccache").output() {
        if output.status.success() {
            let found = String::from_utf8_lossy(&output.stdout).trim().to_string();
            // `where` on Windows may return multiple lines; take the first
            if let Some(first) = found.lines().next() {
                if !first.is_empty() {
                    return Some(first.to_string());
                }
            }
        }
    }

    // Check common Unix locations directly
    #[cfg(unix)]
    {
        let paths = ["/usr/bin/sccache", "/usr/local/bin/sccache"];
        for path in paths {
            if std::path::Path::new(path).exists() {
                return Some(path.to_string());
            }
        }
    }

    // Check ~/.cargo/bin/sccache (HOME on Unix, USERPROFILE on Windows)
    let home = env::var("HOME").or_else(|_| env::var("USERPROFILE")).ok();
    if let Some(home) = home {
        let cargo_sccache = PathBuf::from(&home).join(".cargo").join("bin").join(&sccache_name);
        if cargo_sccache.exists() {
            return Some(cargo_sccache.to_string_lossy().to_string());
        }
    }

    None
}

#[cfg(unix)]
/// Run the fork-server daemon: pre-load librustc_driver.so once, then fork()
/// for each compilation request received over a unix socket.
///
/// Protocol (binary, over unix socket):
///   Client → Server: u32 LE arg_count, then for each arg: u32 LE len + UTF-8 bytes
///   Server → Client: u32 LE stderr_len + stderr bytes + i32 LE exit_code
fn run_fork_server(socket_path: &str) -> ! {
    use std::os::unix::net::UnixListener;
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

    // Set SO_RCVTIMEO once (not per-iteration) for periodic idle timeout checks
    {
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
    }

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

        // Fork IMMEDIATELY after accept — let child read the request.
        // This minimizes the accept→fork critical path in the parent,
        // allowing it to accept the next connection sooner.
        let pid = unsafe { libc::fork() };
        match pid {
            -1 => {
                eprintln!("cargo-slicer-rustc: fork() failed");
                let _ = send_compilation_result(&mut stream, b"fork failed", 1);
            }
            0 => {
                // ═══ Child process ═══
                // Close the listener socket (child doesn't need it)
                drop(listener);

                // Read compilation args and env vars from client.
                // Wrap in BufReader to reduce read syscalls (~125 → ~5-10).
                let mut buf_reader = std::io::BufReader::with_capacity(16384, &stream);
                let (args, env_vars) = match read_compilation_request(&mut buf_reader) {
                    Ok(r) => r,
                    Err(e) => {
                        eprintln!("cargo-slicer-rustc: child read request error: {}", e);
                        let _ = send_compilation_result(&mut stream, e.as_bytes(), 1);
                        unsafe { libc::_exit(1) };
                    }
                };

                // Set env vars from the dispatch (Cargo's per-crate env)
                for (key, value) in &env_vars {
                    std::env::set_var(key, value);
                }

                // Run the actual driver logic
                let exit_code = run_driver_main(&args);

                // Send result to dispatch: empty stderr (already on daemon stderr) + exit code
                let _ = send_compilation_result(&mut stream, &[], exit_code);

                unsafe { libc::_exit(exit_code) };
            }
            _child_pid => {
                // ═══ Parent process ═══
                // Leak the stream fd — do NOT close it. Closing triggers
                // the dispatch to see a connection state change.
                // The child will send results and close on _exit.
                // Fd leak is bounded (~15 per build, well within limits).
                std::mem::forget(stream);

                // Non-blocking reap of any finished children (avoid zombies)
                loop {
                    let w = unsafe { libc::waitpid(-1, std::ptr::null_mut(), libc::WNOHANG) };
                    if w <= 0 { break; }
                }
            }
        }
    }
}

#[cfg(unix)]
fn cleanup_fork_server(socket_path: &str) {
    let _ = std::fs::remove_file(socket_path);
    if let Some(parent) = Path::new(socket_path).parent() {
        let _ = std::fs::remove_file(parent.join("driver.pid"));
    }
}

#[cfg(any(unix, windows))]
fn read_compilation_request(stream: &mut impl std::io::Read) -> Result<(Vec<String>, Vec<(String, String)>), String> {
    let mut buf4 = [0u8; 4];

    // Read args
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

    // Read env vars (KEY=VALUE pairs)
    let env_vars = match stream.read_exact(&mut buf4) {
        Ok(()) => {
            let env_count = u32::from_le_bytes(buf4) as usize;
            if env_count > 10000 {
                return Err(format!("too many env vars: {}", env_count));
            }
            let mut vars = Vec::with_capacity(env_count);
            for _ in 0..env_count {
                stream.read_exact(&mut buf4).map_err(|e| format!("read env len: {}", e))?;
                let len = u32::from_le_bytes(buf4) as usize;
                if len > 1_000_000 {
                    return Err(format!("env too long: {}", len));
                }
                let mut env_bytes = vec![0u8; len];
                stream.read_exact(&mut env_bytes).map_err(|e| format!("read env: {}", e))?;
                let pair = String::from_utf8(env_bytes).map_err(|e| format!("invalid utf8: {}", e))?;
                if let Some((key, value)) = pair.split_once('=') {
                    vars.push((key.to_string(), value.to_string()));
                }
            }
            vars
        }
        Err(_) => Vec::new(), // Backwards-compatible: old dispatch without env vars
    };

    Ok((args, env_vars))
}

#[cfg(any(unix, windows))]
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
        let exe_suffix = std::env::consts::EXE_SUFFIX;
        let ends_with_rustc = arg.ends_with("rustc") ||
            (!exe_suffix.is_empty() && arg.ends_with(&format!("rustc{}", exe_suffix)));
        ends_with_rustc || arg.contains("/rustc") || arg.contains("\\rustc")
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

    // Fork-server daemon mode (Unix only)
    #[cfg(unix)]
    if let Some(fork_server_arg) = raw_args.iter().find_map(|a| a.strip_prefix("--fork-server=")) {
        let daemonize = raw_args.iter().any(|a| a == "--daemonize");
        if daemonize {
            // Double-fork to detach from the dispatch process.
            // The intermediate process exits immediately, and the real daemon
            // is reparented to init — preventing Cargo from seeing it as a child.
            let pid = unsafe { libc::fork() };
            match pid {
                -1 => {
                    eprintln!("cargo-slicer-rustc: daemonize fork failed");
                    std::process::exit(1);
                }
                0 => {
                    // Grandchild: become the daemon
                    unsafe { libc::setsid(); } // new session
                    run_fork_server(fork_server_arg);
                }
                _ => {
                    // Intermediate process: exit immediately
                    // The grandchild (daemon) is reparented to init.
                    std::process::exit(0);
                }
            }
        } else {
            run_fork_server(fork_server_arg);
        }
    }

    // Windows named-pipe daemon mode
    #[cfg(windows)]
    if let Some(pipe_name) = raw_args.iter().find_map(|a| a.strip_prefix("--pipe-server=")) {
        run_pipe_server(pipe_name);
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
        let exe_suffix = std::env::consts::EXE_SUFFIX;
        let ends_with_rustc = arg.ends_with("rustc") ||
            (!exe_suffix.is_empty() && arg.ends_with(&format!("rustc{}", exe_suffix)));
        ends_with_rustc || arg.contains("/rustc") || arg.contains("\\rustc")
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

            // Call our HIR visitor (no mir_built override in data-collection mode)
            let (usage_data, _) = cargo_slicer::rustc_integration::hir_visitor::collect_from_hir(tcx, false, None);

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

            // In "analysis" or "lazy-metadata" phase, we need source hash.
            // No cache loading needed — we always collect fresh data.
            if phase == "analysis" || phase == "lazy-metadata" {
                let source_hash = source_path.as_ref()
                    .and_then(|p| compute_source_hash(p));

                if is_debug_enabled() {
                    debug_log(&format!(
                        "[cargo-slicer-rustc] {} phase for {:?}/{}", phase, crate_name, crate_type
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

            // In "build" or "lazy-codegen" phase, load from cache.
            // Prefer MIR-precise cache (.mir-cache) over syn-based (.cache).
            // Skip source hash computation — trust the cache.
            if phase == "build" || phase == "lazy-codegen" {
                if let Some(ref crate_nm) = crate_name {
                    // Try MIR-precise cache first (more accurate, written by mir_harvester)
                    if let Some(mir_path) = get_mir_cache_path(crate_nm) {
                        if let Some(mir_items) = load_mir_cache(&mir_path, crate_nm) {
                            if is_debug_enabled() {
                                debug_log(&format!(
                                    "[cargo-slicer-rustc] Build phase: loaded {} MIR-precise items for {}/{}",
                                    mir_items.len(), crate_nm, crate_type
                                ));
                            }
                            return Self {
                                preloaded_marked: None,
                                cached_marked: Some(mir_items),
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

                    // Fall back to syn-based cache
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

            // Auto-lean mode: if a MIR-precise cache or both .cache and .seeds exist,
            // use pre-computed marked set instead of running full driver BFS.
            //
            // MIR-precise cache (from mir_harvester) is preferred — it uses actual
            // MIR call graph data and is more accurate than syn-based pre-analysis.
            //
            // The .cache contains syn-format paths with crate prefix (e.g.,
            // "helix_stdx::rope::foo") but tcx.def_path_str() for LOCAL items
            // returns paths WITHOUT the crate prefix (e.g., "rope::foo").
            // We strip the crate prefix here so is_item_marked() queries match.
            if let Some(ref crate_nm) = crate_name {
                // Try MIR-precise cache first (already has prefix stripped in load_mir_cache)
                if let Some(mir_path) = get_mir_cache_path(crate_nm) {
                    if let Some(mir_items) = load_mir_cache(&mir_path, crate_nm) {
                        if is_debug_enabled() {
                            debug_log(&format!(
                                "[cargo-slicer-rustc] Auto-lean: loaded {} MIR-precise items for {}/{} (preferred over syn cache)",
                                mir_items.len(), crate_nm, crate_type
                            ));
                        }
                        return Self {
                            preloaded_marked: preloaded_marked,
                            cached_marked: Some(mir_items),
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

                // Fall back to syn-based cache + seeds
                if let Some(cache_path) = get_cache_path(crate_nm, &crate_type) {
                    if let Some(entry) = load_cache_entry(&cache_path) {
                        let seeds_exists = get_cache_dir()
                            .map(|d| d.join(format!("{}-{}.seeds", crate_nm, crate_type)).exists())
                            .unwrap_or(false);
                        if seeds_exists {
                            // Strip crate prefix from syn-format paths to match
                            // tcx.def_path_str() output for local items.
                            let prefix = format!("{}::", crate_nm);
                            let stripped: HashSet<String> = entry.marked_items.iter()
                                .map(|path| {
                                    if let Some(rest) = path.strip_prefix(&prefix) {
                                        rest.to_string()
                                    } else {
                                        path.clone()
                                    }
                                })
                                .collect();
                            if is_debug_enabled() {
                                debug_log(&format!(
                                    "[cargo-slicer-rustc] Auto-lean: loaded {} cached items for {}/{} (stripped crate prefix)",
                                    stripped.len(), crate_nm, crate_type
                                ));
                            }
                            return Self {
                                preloaded_marked: preloaded_marked,
                                cached_marked: Some(stripped),
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

            // Phase behavior:
            // - "analysis": mir_built only (collect data), no codegen filter
            // - "build": optimized_mir only (stub with cached items), no mir_built
            // - "lazy-metadata": analysis + BFS + cache, then Continue (--emit=metadata, no codegen)
            // - "lazy-codegen": like build (load cache, filter codegen)
            // - "" (default): both overrides (legacy single-pass mode)

            // Pre-store marked items BEFORE queries run, so the optimized_mir override
            // can see them. Skip in analysis/lazy-metadata phase (no codegen filtering).
            if self.phase != "analysis" && self.phase != "lazy-metadata" {
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
            // In "analysis" phase: skip (no codegen, just data collection).
            // In "lazy-metadata" phase: skip — dispatch replaced --emit with
            // dep-info,metadata so no LLVM codegen runs. Installing codegen overrides
            // would interfere with metadata serialization.
            let enable_codegen_filter = env::var("CARGO_SLICER_CODEGEN_FILTER").is_ok()
                && !self.is_dependency
                && self.phase != "analysis"
                && self.phase != "lazy-metadata";

            // Initialize the call graph for pre-optimization reference tracking.
            // Skip in "build"/"lazy-codegen" phase (no data collection needed).
            if self.phase != "build" && self.phase != "lazy-codegen" {
                cargo_slicer::rustc_integration::init_call_graph();
            }

            // Initialize stubbed DefId tracking for collect_and_partition filtering.
            if enable_codegen_filter {
                cargo_slicer::rustc_integration::init_stubbed_def_ids();
            }

            // Determine which query overrides to install based on phase:
            // - "analysis": mir_built only (collect pre-opt call edges), no optimized_mir override
            // - "build"/"lazy-codegen": optimized_mir + optional mir_built early stub
            // - "" (default): both overrides (legacy single-pass mode)
            //
            // Two modes for mir_built override:
            // A. Edge scanning (no cache): capture pre-inlining call edges for BFS
            // B. Early stubbing (cache hit + codegen filter): skip THIR→MIR lowering
            //    for unreachable items by returning stub MIR directly. This avoids
            //    the cost of building full MIR bodies that will be discarded anyway.
            let need_mir_built_edges = self.phase != "build" && self.phase != "lazy-codegen"
                && self.cached_marked.is_none()
                && (enable_codegen_filter || self.phase == "analysis" || self.phase == "lazy-metadata");
            let need_mir_built_early_stub = enable_codegen_filter
                && (self.cached_marked.is_some() || self.preloaded_marked.is_some())
                && env::var("CARGO_SLICER_MIR_BUILT_EARLY_STUB").is_ok();
            let need_mir_built = need_mir_built_edges || need_mir_built_early_stub;
            let need_optimized_mir = enable_codegen_filter;

            if need_mir_built || need_optimized_mir {
                use std::sync::OnceLock;
                use std::sync::atomic::{AtomicBool, Ordering as AtomicOrdering};

                // Cache trace flag to avoid env::var calls in query overrides
                // AtomicBool: resettable for Windows in-process daemon reuse
                static TRACE_ENABLED: AtomicBool = AtomicBool::new(false);
                TRACE_ENABLED.store(env::var("CARGO_SLICER_TRACE").is_ok(), AtomicOrdering::Relaxed);

                // Cache crate name/type for stub-count feedback in collect_and_partition
                // Mutex<String>: resettable for Windows in-process daemon reuse
                static CRATE_NAME_FOR_FEEDBACK: std::sync::Mutex<String> = std::sync::Mutex::new(String::new());
                static CRATE_TYPE_FOR_FEEDBACK: std::sync::Mutex<String> = std::sync::Mutex::new(String::new());
                if let Some(ref cn) = self.crate_name {
                    if let Ok(mut lock) = CRATE_NAME_FOR_FEEDBACK.lock() {
                        *lock = cn.clone();
                    }
                }
                if let Ok(mut lock) = CRATE_TYPE_FOR_FEEDBACK.lock() {
                    *lock = self.crate_type.clone();
                }

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

                type CollectPartitionFn = for<'tcx> fn(
                    rustc_middle::ty::TyCtxt<'tcx>,
                    (),
                ) -> rustc_middle::mir::mono::MonoItemPartitions<'tcx>;
                static ORIGINAL_COLLECT_PARTITION: OnceLock<CollectPartitionFn> = OnceLock::new();

                // Use static flags to communicate phase to query overrides (fn pointers can't capture)
                // AtomicBool: resettable for Windows in-process daemon reuse
                static ENABLE_MIR_BUILT: AtomicBool = AtomicBool::new(false);
                static ENABLE_OPTIMIZED_MIR: AtomicBool = AtomicBool::new(false);
                // Early stub mode: on cache hit, mir_built returns stubs for unreachable
                // items instead of calling the original provider (skips THIR→MIR lowering).
                static ENABLE_MIR_BUILT_EARLY_STUB: AtomicBool = AtomicBool::new(false);
                ENABLE_MIR_BUILT.store(need_mir_built, AtomicOrdering::Relaxed);
                ENABLE_OPTIMIZED_MIR.store(need_optimized_mir, AtomicOrdering::Relaxed);
                ENABLE_MIR_BUILT_EARLY_STUB.store(need_mir_built_early_stub, AtomicOrdering::Relaxed);

                config.override_queries = Some(|_session, providers| {
                    ORIGINAL_OPTIMIZED_MIR.set(providers.optimized_mir).ok();
                    ORIGINAL_MIR_BUILT.set(providers.mir_built).ok();
                    ORIGINAL_COLLECT_PARTITION.set(providers.collect_and_partition_mono_items).ok();

                    let mir_built_enabled = ENABLE_MIR_BUILT.load(AtomicOrdering::Relaxed);
                    let optimized_mir_enabled = ENABLE_OPTIMIZED_MIR.load(AtomicOrdering::Relaxed);

                    if mir_built_enabled {
                        // Override mir_built for two purposes:
                        // A. Early stub mode (cache hit): return stub MIR for unreachable
                        //    items, skipping THIR→MIR lowering entirely. Borrowck on stub
                        //    MIR is trivial (no borrows, no moves).
                        // B. Edge scanning mode (no cache): call original provider, then
                        //    scan the resulting MIR for call edges and vtable coercions.
                        providers.mir_built = |tcx, def_id| {
                            let original = ORIGINAL_MIR_BUILT.get()
                                .expect("original mir_built not stored");

                            // Early stub mode: skip THIR→MIR lowering for unreachable items
                            let early_stub = ENABLE_MIR_BUILT_EARLY_STUB.load(AtomicOrdering::Relaxed);
                            if early_stub {
                                use cargo_slicer::rustc_integration::{is_item_marked, is_safe_to_skip};
                                use rustc_middle::mir::*;
                                use rustc_index::IndexVec;

                                let full_def_id = def_id.to_def_id();

                                // At mir_built time, const evaluation hasn't happened yet.
                                // Const fns may be called during const eval which uses
                                // mir_for_ctfe → mir_promoted → mir_built. A stub body
                                // would cause "entering unreachable code" errors.
                                // (This check is safe to skip at optimized_mir time because
                                // const eval is already complete by then.)
                                if tcx.is_const_fn(full_def_id) {
                                    return original(tcx, def_id);
                                }

                                let path = tcx.def_path_str(full_def_id);
                                if !is_item_marked(&path) && is_safe_to_skip(tcx, full_def_id) {
                                    if is_debug_enabled() {
                                        debug_log(&format!("[mir_built-early-stub] STUB: {}", path));
                                    }
                                    // Construct minimal stub MIR wrapped in Steal
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

                                    let source_scopes = IndexVec::from_raw(vec![SourceScopeData {
                                        span,
                                        parent_scope: None,
                                        inlined: None,
                                        inlined_parent_scope: None,
                                        local_data: ClearCrossCrate::Clear,
                                    }]);

                                    let source = MirSource::item(full_def_id);
                                    // Don't set required_consts/mentioned_items here —
                                    // unlike optimized_mir (final body), mir_built produces
                                    // an intermediate body. mir_promoted will compute and
                                    // set required_consts; mentioned_items are set by later
                                    // MIR transform passes.
                                    let body = Body::new(
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

                                    return tcx.arena.alloc(
                                        rustc_data_structures::steal::Steal::new(body)
                                    );
                                }
                                // Marked or not safe to skip → fall through to original provider
                                return original(tcx, def_id);
                            }

                            // Edge scanning mode: call original first, then scan MIR
                            let result = original(tcx, def_id);

                            // Collect call edges from the pre-optimization MIR body
                            let body = result.borrow();
                            let caller = tcx.def_path_str(def_id.to_def_id());

                            // Accumulate callees locally, then flush with a single
                            // Mutex lock (batch_record_call_edges). This is ~100x
                            // fewer lock operations than per-edge record_call_edge.
                            let mut local_callees: Vec<String> = Vec::new();

                            use rustc_middle::ty::TypeVisitableExt;

                            // Helper: check if a type is a FnDef and return the callee path.
                            // Returns None for std/core/alloc items (resolved separately).
                            let check_fndef = |ty: rustc_middle::ty::Ty<'_>| -> Option<String> {
                                if let rustc_middle::ty::TyKind::FnDef(callee_id, _) = ty.kind() {
                                    let callee = tcx.def_path_str(*callee_id);
                                    if !callee.starts_with("std::") &&
                                       !callee.starts_with("core::") &&
                                       !callee.starts_with("alloc::") {
                                        return Some(callee);
                                    }
                                }
                                None
                            };

                            // Collect std/core/alloc FnDef with concrete substs
                            // for post-borrow trait resolution.
                            let mut std_fndefs: Vec<(rustc_span::def_id::DefId, rustc_middle::ty::GenericArgsRef<'_>)> = Vec::new();

                            // Single-pass scan: collect call edges, vtable constructions,
                            // and std trait method references from all basic blocks.
                            for bb in body.basic_blocks.iter() {
                                for stmt in &bb.statements {
                                    if let rustc_middle::mir::StatementKind::Assign(box (_, ref rvalue)) = stmt.kind {
                                        match rvalue {
                                            rustc_middle::mir::Rvalue::Use(rustc_middle::mir::Operand::Constant(c)) |
                                            rustc_middle::mir::Rvalue::UnaryOp(_, rustc_middle::mir::Operand::Constant(c)) |
                                            rustc_middle::mir::Rvalue::Repeat(rustc_middle::mir::Operand::Constant(c), _) => {
                                                let ty = match c.const_ {
                                                    rustc_middle::mir::Const::Val(_, ty) => ty,
                                                    rustc_middle::mir::Const::Ty(ty, _) => ty,
                                                    rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                                                };
                                                if let Some(callee) = check_fndef(ty) {
                                                    local_callees.push(callee);
                                                } else {
                                                    if let rustc_middle::ty::TyKind::FnDef(cid, subs) = ty.kind() {
                                                        if !subs.is_empty() && !subs.has_param() {
                                                            std_fndefs.push((*cid, subs));
                                                        }
                                                    }
                                                }
                                            }
                                            rustc_middle::mir::Rvalue::Cast(kind, operand, target_ty) => {
                                                // Check constant operand for FnDef
                                                if let rustc_middle::mir::Operand::Constant(c) = operand {
                                                    let ty = match c.const_ {
                                                    rustc_middle::mir::Const::Val(_, ty) => ty,
                                                    rustc_middle::mir::Const::Ty(ty, _) => ty,
                                                    rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                                                };
                                                    if let Some(callee) = check_fndef(ty) {
                                                        local_callees.push(callee);
                                                    } else {
                                                        if let rustc_middle::ty::TyKind::FnDef(cid, subs) = ty.kind() {
                                                        if !subs.is_empty() && !subs.has_param() {
                                                            std_fndefs.push((*cid, subs));
                                                        }
                                                    }
                                                    }
                                                }
                                                // Detect unsizing coercions: &T -> &dyn Trait
                                                if let rustc_middle::mir::CastKind::PointerCoercion(
                                                    rustc_middle::ty::adjustment::PointerCoercion::Unsize, _
                                                ) = kind {
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
                                            if let Some(callee) = check_fndef(ty) {
                                                local_callees.push(callee);
                                            } else {
                                                if let rustc_middle::ty::TyKind::FnDef(cid, subs) = ty.kind() {
                                                        if !subs.is_empty() && !subs.has_param() {
                                                            std_fndefs.push((*cid, subs));
                                                        }
                                                    }
                                            }
                                        }
                                        for arg in args {
                                            if let rustc_middle::mir::Operand::Constant(c) = &arg.node {
                                                let ty = match c.const_ {
                                                    rustc_middle::mir::Const::Val(_, ty) => ty,
                                                    rustc_middle::mir::Const::Ty(ty, _) => ty,
                                                    rustc_middle::mir::Const::Unevaluated(_, ty) => ty,
                                                };
                                                if let Some(callee) = check_fndef(ty) {
                                                    local_callees.push(callee);
                                                } else {
                                                    if let rustc_middle::ty::TyKind::FnDef(cid, subs) = ty.kind() {
                                                        if !subs.is_empty() && !subs.has_param() {
                                                            std_fndefs.push((*cid, subs));
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            // Release body borrow, then resolve std trait calls.
                            // In MIR, calls like LowArgs::default() appear as
                            // std::default::Default::default with substs=[LowArgs].
                            // Resolve to the concrete impl (e.g., <LowArgs as Default>::default)
                            // so BFS can mark user-defined trait implementations.
                            drop(body);
                            for (callee_id, substs) in std_fndefs {
                                if let Ok(Some(inst)) = rustc_middle::ty::Instance::try_resolve(
                                    tcx, rustc_middle::ty::TypingEnv::fully_monomorphized(), callee_id, substs,
                                ) {
                                    let resolved = tcx.def_path_str(inst.def_id());
                                    if !resolved.starts_with("std::") && !resolved.starts_with("core::") && !resolved.starts_with("alloc::") {
                                        local_callees.push(resolved);
                                    }
                                }
                            }

                            // Batch flush: single Mutex lock for all edges from this function
                            if !local_callees.is_empty() {
                                cargo_slicer::rustc_integration::batch_record_call_edges(
                                    caller, local_callees);
                            }

                            result
                        };
                    }

                    if optimized_mir_enabled {
                        providers.optimized_mir = |tcx, def_id| {
                            use cargo_slicer::rustc_integration::{is_item_marked_by_def_id, is_safe_to_skip_by_def_id};
                            use rustc_middle::mir::*;
                            use rustc_index::IndexVec;

                            let original = ORIGINAL_OPTIMIZED_MIR.get()
                                .expect("original optimized_mir not stored");

                            let full_def_id = def_id.to_def_id();
                            let is_marked = is_item_marked_by_def_id(tcx, full_def_id);
                            let safe_to_skip = if !is_marked { is_safe_to_skip_by_def_id(tcx, full_def_id) } else { false };

                            if !is_marked && safe_to_skip {
                                cargo_slicer::rustc_integration::increment_stub_count();
                                cargo_slicer::rustc_integration::record_stubbed_def_id(
                                    full_def_id.index.as_u32() as u64
                                );
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

                    // Override collect_and_partition_mono_items for CGU rebalancing
                    // and diagnostics logging.
                    //
                    // When CARGO_SLICER_CGU_REBALANCE=1: moves stubbed MonoItems into a
                    // dedicated "stubs" CGU. Benefits:
                    //   1. Stubs CGU compiles instantly (all ud2 bodies)
                    //   2. Real CGUs have fewer items → faster LLVM compilation
                    //   3. Better thread utilization during parallel codegen
                    //
                    // NOTE: Items are MOVED, not removed. The stub symbols still exist
                    // in the stubs CGU, so non-stubbed callers can still reference them.
                    // Only non-inlined (canonical) definitions are moved; inlined copies
                    // stay in their original CGUs to avoid cross-CGU reference issues.
                    if optimized_mir_enabled {
                        providers.collect_and_partition_mono_items = |tcx, ()| {
                            use rustc_middle::mir::mono::{MonoItem, CodegenUnit};
                            use rustc_span::def_id::LOCAL_CRATE;

                            let original = ORIGINAL_COLLECT_PARTITION.get()
                                .expect("original collect_and_partition_mono_items not stored");
                            let result = original(tcx, ());

                            let stub_count = cargo_slicer::rustc_integration::get_stub_count();

                            // Only count stubbed items in CGUs when needed for diagnostics
                            // or CGU rebalancing. Skip the expensive iteration otherwise.
                            let need_cgu_rebalance = std::env::var("CARGO_SLICER_CGU_REBALANCE").is_ok()
                                && stub_count > 0;
                            let need_diagnostics = is_debug_enabled()
                                || TRACE_ENABLED.load(AtomicOrdering::Relaxed);

                            if need_diagnostics {
                                let total_items: usize = result.codegen_units.iter()
                                    .map(|cgu| cgu.items().len())
                                    .sum();
                                let crate_name = tcx.crate_name(LOCAL_CRATE);
                                let msg = format!(
                                    "[codegen-filter] {} collect_and_partition: {} total mono items, {} stubbed",
                                    crate_name, total_items, stub_count
                                );
                                if is_debug_enabled() { debug_log(&msg); }
                                eprintln!("{}", msg);
                            }

                            // Write stub-count feedback for future builds.
                            // If stub_count is 0, write a .skip-driver marker so the
                            // dispatch skips loading the driver on subsequent builds.
                            if let (Ok(cn_lock), Ok(ct_lock)) = (CRATE_NAME_FOR_FEEDBACK.lock(), CRATE_TYPE_FOR_FEEDBACK.lock()) {
                                let cn = &*cn_lock;
                                let ct = &*ct_lock;
                                if !cn.is_empty() {
                                    if let Some(cache_dir) = get_cache_dir() {
                                        let stub_count_path = cache_dir.join(
                                            format!("{}-{}.stub-count", cn, ct));
                                        let _ = std::fs::write(&stub_count_path,
                                            format!("{}\n", stub_count));
                                        if stub_count < 10 {
                                            let skip_path = cache_dir.join(
                                                format!("{}-{}.skip-driver", cn, ct));
                                            let _ = std::fs::write(&skip_path,
                                                format!("stub-count-feedback:{}\n", stub_count));
                                        }
                                    }
                                }
                            }

                            // CGU rebalancing: move stubbed items into a dedicated CGU
                            if need_cgu_rebalance {
                                let crate_name = tcx.crate_name(LOCAL_CRATE);
                                let stubs_name = rustc_span::Symbol::intern(
                                    &format!("{}.cargo_slicer_stubs", crate_name)
                                );
                                let mut stubs_cgu = CodegenUnit::new(stubs_name);
                                let mut new_cgus: Vec<CodegenUnit<'_>> = Vec::with_capacity(
                                    result.codegen_units.len() + 1
                                );
                                let mut moved_count = 0usize;

                                for orig_cgu in result.codegen_units {
                                    let mut keep_cgu = CodegenUnit::new(orig_cgu.name());
                                    if orig_cgu.is_primary() {
                                        keep_cgu.make_primary();
                                    }

                                    for (item, data) in orig_cgu.items() {
                                        // Check local FIRST to avoid DefIndex collisions
                                        // between local and external crate items.
                                        let is_stubbed_fn = matches!(item, MonoItem::Fn(instance) if
                                            instance.def_id().is_local() &&
                                            cargo_slicer::rustc_integration::is_def_id_stubbed(
                                                instance.def_id().index.as_u32() as u64
                                            )
                                        );

                                        if is_stubbed_fn && !data.inlined {
                                            // Move canonical stub definitions to dedicated CGU
                                            stubs_cgu.items_mut().entry(*item).or_insert(*data);
                                            moved_count += 1;
                                        } else {
                                            keep_cgu.items_mut().insert(*item, *data);
                                        }
                                    }

                                    keep_cgu.compute_size_estimate();
                                    new_cgus.push(keep_cgu);
                                }

                                if moved_count > 0 {
                                    stubs_cgu.compute_size_estimate();
                                    new_cgus.push(stubs_cgu);

                                    if is_debug_enabled() {
                                        debug_log(&format!(
                                            "[codegen-filter] CGU rebalance: moved {} stubbed items to dedicated stubs CGU ({} CGUs total)",
                                            moved_count, new_cgus.len()
                                        ));
                                    }

                                    let new_units = &*tcx.arena.alloc_from_iter(new_cgus);
                                    return rustc_middle::mir::mono::MonoItemPartitions {
                                        codegen_units: new_units,
                                        all_mono_items: result.all_mono_items,
                                    };
                                }
                            }

                            result
                        };
                    }
                });

                if is_debug_enabled() {
                    if need_mir_built {
                        debug_log("[cargo-slicer-rustc] mir_built override ENABLED (call edge collection)");
                    }
                    if need_optimized_mir {
                        debug_log("[cargo-slicer-rustc] optimized_mir override ENABLED (codegen filtering)");
                        debug_log("[cargo-slicer-rustc] collect_and_partition override ENABLED (CGU filtering)");
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

            let phase_start = std::time::Instant::now();
            let trace_timing = std::env::var("CARGO_SLICER_TRACE_TIMING").is_ok();

            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] After analysis (phase={:?})", self.phase));
            }

            let crate_name = tcx.crate_name(LOCAL_CRATE);

            // ── Analysis phase: collect data, write .analysis file, return ──
            if self.phase == "analysis" {
                return self.after_analysis_phase(tcx, &crate_name.to_string());
            }

            // ── Lazy-metadata phase: like analysis but stop compilation (no codegen) ──
            if self.phase == "lazy-metadata" {
                return self.after_lazy_metadata_phase(tcx, &crate_name.to_string());
            }

            // ── Lazy-codegen phase: like build, use cached items for selective codegen ──
            if self.phase == "lazy-codegen" {
                return self.after_build_phase(tcx, &crate_name.to_string());
            }

            // ── Build phase: use cached marked items, skip data collection ──
            if self.phase == "build" {
                return self.after_build_phase(tcx, &crate_name.to_string());
            }

            // ── Dep-seeds phase: non-workspace dep with pre-computed seeds ──
            // NOTE: Dep pruning was found to be ineffective because `is_safe_to_skip()`
            // blocks public items in lib crates, and most dep functions are public.
            // Only private non-generic non-trait-impl functions could be stubbed,
            // which are very few in practice. The driver loading overhead (~130ms)
            // exceeds any savings. Keeping code for reference but disabled.
            if let Ok(dep_seeds_path) = env::var("CARGO_SLICER_DEP_SEEDS") {
                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Dep-seeds found but dep pruning disabled: {}",
                        dep_seeds_path));
                }
                // Fall through to normal compilation — no codegen filtering for deps
                return rustc_driver::Compilation::Continue;
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
                // Collect usage data and compute marked items via BFS reachability.
                // mir_built override is active here — skip redundant MIR body visits
                // (call edges are already in the pre-opt call graph).
                // Also collect unconditional seeds in the same HIR pass.
                let is_binary = tcx.entry_fn(()).is_some();
                let t0 = std::time::Instant::now();
                let (usage_data, unconditional_seeds) = cargo_slicer::rustc_integration::hir_visitor::collect_from_hir(tcx, true, Some(is_binary));
                let hir_elapsed = t0.elapsed();

                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Collected {} defined items, {} dependencies",
                             usage_data.defined_items.len(),
                             usage_data.item_dependencies.len()));
                }

                // BFS reachability from seeds
                let mut seeds: HashSet<String> = unconditional_seeds;

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

                if seeds_loaded {
                    // Seeds are in syn-format paths with crate prefix
                    // (e.g., "helix_core::movement::move_next_word_end").
                    // The driver's defined_items use rustc-format paths WITHOUT
                    // crate prefix (e.g., "movement::move_next_word_end").
                    // Strip crate prefix from seeds, then normalize trait impl
                    // paths to match rustc-format.
                    let raw_seeds = std::mem::take(&mut seeds);
                    let prefix = self.crate_name.as_ref()
                        .map(|n| format!("{}::", n))
                        .unwrap_or_default();
                    let syn_seeds: HashSet<String> = raw_seeds.into_iter()
                        .map(|path| {
                            path.strip_prefix(&prefix)
                                .unwrap_or(&path)
                                .to_string()
                        })
                        .collect();
                    for (rustc_path, _info) in &usage_data.defined_items {
                        if syn_seeds.contains(rustc_path) {
                            // Direct match (non-trait-impl paths)
                            seeds.insert(rustc_path.clone());
                        } else if rustc_path.starts_with('<') {
                            // Trait impl: "<Type as Trait>::method" → "Type::method"
                            if let Some(normalized) = normalize_trait_impl_path(rustc_path) {
                                if syn_seeds.contains(&normalized) {
                                    seeds.insert(rustc_path.clone());
                                }
                            }
                        }
                    }
                    if is_debug_enabled() {
                        debug_log(&format!(
                            "[cargo-slicer-rustc] Normalized {} syn-format seeds → {} rustc-format seeds",
                            syn_seeds.len(), seeds.len()
                        ));
                    }
                } else {
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

                // Unconditional seeds (#[test], #[no_mangle], trait impls, statics)
                // were already collected during collect_from_hir above.

                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] BFS seeds: {} (binary: {}, cross-crate seeds: {})",
                             seeds.len(), is_binary, seeds_loaded));
                }

                // BFS from seeds
                let pre_opt_graph = cargo_slicer::rustc_integration::take_call_graph();
                if is_debug_enabled() {
                    if let Some(ref graph) = pre_opt_graph {
                        let total_edges: usize = graph.values().map(|s| s.len()).sum();
                        debug_log(&format!("[cargo-slicer-rustc] Call graph: {} callers, {} total edges",
                            graph.len(), total_edges));
                    }
                }
                let seed_count = seeds.len();
                let t1 = std::time::Instant::now();
                let marked = run_bfs_from_seeds(seeds, &usage_data, &pre_opt_graph);
                let bfs_elapsed = t1.elapsed();

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

                if trace_timing {
                    eprintln!("[timing] {} hir_mir={:.1}ms bfs={:.1}ms",
                        crate_name,
                        hir_elapsed.as_secs_f64() * 1000.0,
                        bfs_elapsed.as_secs_f64() * 1000.0);
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

            // MIR-precise whole-program analysis (Stage 3).
            // When CARGO_SLICER_MIR_PRECISE=1 and this is a binary crate, harvest MIR
            // from all workspace extern crates to build a precise cross-crate call graph.
            // This writes per-crate .mir-cache files for use in subsequent lazy-codegen passes.
            if env::var("CARGO_SLICER_MIR_PRECISE").is_ok() && tcx.entry_fn(()).is_some() {
                let workspace_crates: HashSet<String> = env::var("CARGO_SLICER_WORKSPACE_CRATES")
                    .unwrap_or_default()
                    .split(',')
                    .filter(|s| !s.is_empty())
                    .map(|s| s.to_string())
                    .collect();

                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] Running MIR-precise analysis for binary crate"));
                }

                let mir_result = cargo_slicer::rustc_integration::mir_harvester::harvest_extern_mir(
                    tcx, &workspace_crates,
                );

                if is_debug_enabled() {
                    debug_log(&format!("[cargo-slicer-rustc] MIR-precise: {} crates, {} functions, {} per-crate marked sets",
                        mir_result.crates_analyzed, mir_result.functions_analyzed,
                        mir_result.crate_marked.len()));
                }

                // Write per-crate MIR-precise cache files
                if let Some(cache_dir) = get_cache_dir() {
                    if let Err(e) = cargo_slicer::rustc_integration::mir_harvester::write_mir_precise_caches(
                        &mir_result, &cache_dir,
                    ) {
                        if is_debug_enabled() {
                            debug_log(&format!("[cargo-slicer-rustc] MIR cache write error: {}", e));
                        }
                    }
                }
            }

            // Store marked items for the query override to access
            let t2 = std::time::Instant::now();
            let marked_count = marked_items.len();
            if !store_marked_items(marked_items.clone()) {
                if is_debug_enabled() {
                    debug_log("[cargo-slicer-rustc] WARNING: Marked items were already set");
                }
            }

            // Build both marked-DefId and safe-to-skip caches in a single pass
            // over mir_keys. Replaces per-item is_safe_to_skip() calls during codegen
            // with O(1) HashSet lookups.
            cargo_slicer::rustc_integration::build_codegen_caches(tcx);

            let store_elapsed = t2.elapsed();

            if trace_timing {
                let total = phase_start.elapsed();
                eprintln!("[timing] {} after_analysis_total={:.1}ms store+cache={:.1}ms marked={}",
                    crate_name,
                    total.as_secs_f64() * 1000.0,
                    store_elapsed.as_secs_f64() * 1000.0,
                    marked_count);
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

            // Collect usage data (definitions only — MIR edges are in pre-opt graph)
            let (usage_data, _) = cargo_slicer::rustc_integration::hir_visitor::collect_from_hir(tcx, true, None);

            // Get pre-optimization call graph (collected by mir_built override)
            let pre_opt_graph = cargo_slicer::rustc_integration::take_call_graph();

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

        /// Lazy-metadata phase: collect analysis + BFS, emit metadata only.
        /// Returns Compilation::Continue — the dispatch replaces --emit with
        /// --emit=dep-info,metadata so rustc's codegen phase is a no-op (no LLVM).
        /// The dispatch then wraps the .rmeta into a placeholder .rlib for Cargo pipelining.
        /// The REAL marked items are saved to cache for Pass 2 (lazy-codegen).
        fn after_lazy_metadata_phase<'tcx>(
            &mut self,
            tcx: rustc_middle::ty::TyCtxt<'tcx>,
            crate_name: &str,
        ) -> rustc_driver::Compilation {
            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Lazy-metadata phase for crate: {}", crate_name));
            }

            // Collect usage data + unconditional seeds in one HIR pass
            let is_binary = tcx.entry_fn(()).is_some();
            let (usage_data, unconditional_seeds) = cargo_slicer::rustc_integration::hir_visitor::collect_from_hir(tcx, true, Some(is_binary));
            let pre_opt_graph = cargo_slicer::rustc_integration::take_call_graph();

            if is_debug_enabled() {
                debug_log(&format!("[cargo-slicer-rustc] Lazy-metadata: {} defined items, {} deps, {} call graph callers",
                    usage_data.defined_items.len(),
                    usage_data.item_dependencies.len(),
                    pre_opt_graph.as_ref().map_or(0, |g| g.len())));
            }

            // Write .analysis file for cross-crate BFS
            self.write_analysis_file(tcx, &usage_data, &pre_opt_graph);

            // Compute and cache marked items (BFS) so lazy-codegen can use them.
            // For lazy-metadata, always use conservative all-pub seeds (not .seeds
            // restriction) because the syn-based pre-analysis is incomplete —
            // it misses method calls, so .seeds omits items like DiffHandle::new.
            // The optimization cost is minimal: lib crates mark all pub items +
            // their transitive deps. Private unreachable items still get stubbed.
            let mut seeds: HashSet<String> = unconditional_seeds;

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

            // BFS from seeds (shared function handles BFS + closure propagation)
            let marked = run_bfs_from_seeds(seeds, &usage_data, &pre_opt_graph);

            // Save REAL marked items to cache for lazy-codegen Pass 2
            if let Some(ref crate_nm) = self.crate_name {
                let hash = self.source_hash.clone().or_else(|| {
                    self.source_path.as_ref().and_then(|p| compute_source_hash(p))
                });
                if let Some(ref hash) = hash {
                    if let Some(cache_path) = get_cache_path(crate_nm, &self.crate_type) {
                        let _ = save_cache_entry(&cache_path, hash, &marked);
                        if is_debug_enabled() {
                            debug_log(&format!(
                                "[cargo-slicer-rustc] Lazy-metadata: saved {} marked items to cache for {}",
                                marked.len(), crate_nm
                            ));
                        }
                    }
                }
            }

            if is_debug_enabled() {
                debug_log("[cargo-slicer-rustc] Lazy-metadata: continuing with metadata-only emit (no LLVM codegen)");
            }

            // Continue compilation — the dispatch replaced --emit with --emit=dep-info,metadata,
            // so rustc's codegen phase sees no link/asm/llvm-ir outputs and becomes a no-op.
            // The .rmeta is written during the normal pipeline. No LLVM work at all.
            // The dispatch creates a placeholder .rlib from the .rmeta for Cargo pipelining.
            rustc_driver::Compilation::Continue
        }

        /// Build phase: use pre-computed cache from cross-crate BFS.
        /// Skip data collection entirely.
        fn after_build_phase<'tcx>(
            &mut self,
            tcx: rustc_middle::ty::TyCtxt<'tcx>,
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

            // Build codegen caches for O(1) lookups in optimized_mir override.
            // Only useful when marked items were pre-stored; otherwise is_item_marked()
            // returns true conservatively and nothing gets stubbed.
            if self.cached_marked.is_some() || self.preloaded_marked.is_some() {
                cargo_slicer::rustc_integration::build_codegen_caches(tcx);
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

// ═══════════════════════════════════════════════════════════════════════════
// Windows Named-Pipe Daemon
// ═══════════════════════════════════════════════════════════════════════════
//
// On Windows there's no fork(), so the daemon keeps rustc_driver.dll loaded
// and runs compilations in-process sequentially. Dispatch communicates via
// Windows named pipes using the same binary IPC protocol as the Unix daemon.

#[cfg(windows)]
mod pipe_server {
    use std::io::{Read, Write};
    use windows_sys::Win32::Foundation::{
        CloseHandle, HANDLE, INVALID_HANDLE_VALUE, FALSE,
        GetLastError, ERROR_PIPE_CONNECTED,
    };
    use windows_sys::Win32::Storage::FileSystem::{
        ReadFile, WriteFile, FlushFileBuffers, PIPE_ACCESS_DUPLEX,
    };
    use windows_sys::Win32::System::Pipes::{
        CreateNamedPipeW, ConnectNamedPipe, DisconnectNamedPipe,
        PIPE_TYPE_BYTE, PIPE_READMODE_BYTE, PIPE_WAIT,
    };

    /// Wrapper around a Windows HANDLE that implements Read.
    pub struct PipeReader {
        pub handle: HANDLE,
    }

    impl Read for PipeReader {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            let mut bytes_read: u32 = 0;
            let ok = unsafe {
                ReadFile(
                    self.handle,
                    buf.as_mut_ptr() as _,
                    buf.len() as u32,
                    &mut bytes_read,
                    std::ptr::null_mut(),
                )
            };
            if ok == FALSE {
                let err = unsafe { GetLastError() };
                return Err(std::io::Error::from_raw_os_error(err as i32));
            }
            Ok(bytes_read as usize)
        }
    }

    /// Wrapper around a Windows HANDLE that implements Write.
    pub struct PipeWriter {
        pub handle: HANDLE,
    }

    impl Write for PipeWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            let mut bytes_written: u32 = 0;
            let ok = unsafe {
                WriteFile(
                    self.handle,
                    buf.as_ptr() as _,
                    buf.len() as u32,
                    &mut bytes_written,
                    std::ptr::null_mut(),
                )
            };
            if ok == FALSE {
                let err = unsafe { GetLastError() };
                return Err(std::io::Error::from_raw_os_error(err as i32));
            }
            Ok(bytes_written as usize)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            let ok = unsafe { FlushFileBuffers(self.handle) };
            if ok == FALSE {
                let err = unsafe { GetLastError() };
                return Err(std::io::Error::from_raw_os_error(err as i32));
            }
            Ok(())
        }
    }

    /// Encode a Rust &str as a null-terminated wide string for Win32 APIs.
    pub fn to_wide(s: &str) -> Vec<u16> {
        s.encode_utf16().chain(std::iter::once(0)).collect()
    }

    /// Create a named pipe instance and return its handle.
    pub fn create_pipe_instance(pipe_name: &str) -> Result<HANDLE, String> {
        let wide_name = to_wide(pipe_name);
        let handle = unsafe {
            CreateNamedPipeW(
                wide_name.as_ptr(),
                PIPE_ACCESS_DUPLEX,
                PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
                1,      // max_instances = 1 enforces sequential
                65536,  // out buffer
                65536,  // in buffer
                5000,   // default timeout ms
                std::ptr::null(),
            )
        };
        if handle == INVALID_HANDLE_VALUE {
            let err = unsafe { GetLastError() };
            return Err(format!("CreateNamedPipeW failed: error {}", err));
        }
        Ok(handle)
    }

    /// Wait for a client to connect to the pipe.
    pub fn wait_for_client(handle: HANDLE) -> Result<(), String> {
        let ok = unsafe { ConnectNamedPipe(handle, std::ptr::null_mut()) };
        if ok == FALSE {
            let err = unsafe { GetLastError() };
            // ERROR_PIPE_CONNECTED means client connected between Create and Connect
            if err != ERROR_PIPE_CONNECTED {
                return Err(format!("ConnectNamedPipe failed: error {}", err));
            }
        }
        Ok(())
    }

    /// Disconnect and close a pipe handle.
    pub fn disconnect_and_close(handle: HANDLE) {
        unsafe {
            FlushFileBuffers(handle);
            DisconnectNamedPipe(handle);
            CloseHandle(handle);
        }
    }
}

#[cfg(windows)]
fn run_pipe_server(pipe_name: &str) -> ! {
    let cache_dir = std::env::var("CARGO_SLICER_CACHE_DIR")
        .unwrap_or_else(|_| ".slicer-cache".to_string());
    let _ = std::fs::create_dir_all(&cache_dir);

    // Write PID file
    let pid_path = format!("{}/driver.pid", cache_dir);
    let _ = std::fs::write(&pid_path, format!("{}", std::process::id()));

    eprintln!("cargo-slicer-rustc: pipe-server listening on {}", pipe_name);

    let idle_timeout = std::time::Duration::from_secs(300); // 5 minutes
    let mut last_activity = std::time::Instant::now();

    // Store pipe_name for the idle-timeout thread
    let pipe_name_owned = pipe_name.to_string();

    loop {
        // Check idle timeout
        if last_activity.elapsed() > idle_timeout {
            eprintln!("cargo-slicer-rustc: pipe-server idle timeout, exiting");
            let _ = std::fs::remove_file(&pid_path);
            std::process::exit(0);
        }

        // Create a new pipe instance for each connection
        let handle = match pipe_server::create_pipe_instance(&pipe_name_owned) {
            Ok(h) => h,
            Err(e) => {
                eprintln!("cargo-slicer-rustc: pipe create failed: {}", e);
                std::thread::sleep(std::time::Duration::from_millis(100));
                continue;
            }
        };

        // Spawn a timer thread to cancel the blocking ConnectNamedPipe after idle timeout.
        // This lets us exit if no client connects within the timeout.
        // HANDLE is *mut c_void (not Send), so transmit as usize across thread boundary.
        let timer_handle_raw = handle as usize;
        let timeout_remaining = idle_timeout.saturating_sub(last_activity.elapsed());
        let timer = std::thread::spawn(move || {
            std::thread::sleep(timeout_remaining);
            // Cancel the blocking I/O on the pipe handle to unblock ConnectNamedPipe
            let h = timer_handle_raw as windows_sys::Win32::Foundation::HANDLE;
            unsafe {
                windows_sys::Win32::System::IO::CancelIoEx(h, std::ptr::null());
            }
        });

        // Block until a client connects (or timer cancels us)
        match pipe_server::wait_for_client(handle) {
            Ok(()) => {
                // Client connected — cancel the timer (best effort)
                drop(timer);
                last_activity = std::time::Instant::now();
            }
            Err(e) => {
                // Likely cancelled by timer or other error
                unsafe { windows_sys::Win32::Foundation::CloseHandle(handle); }
                if last_activity.elapsed() > idle_timeout {
                    eprintln!("cargo-slicer-rustc: pipe-server idle timeout, exiting");
                    let _ = std::fs::remove_file(&pid_path);
                    std::process::exit(0);
                }
                eprintln!("cargo-slicer-rustc: pipe connect error: {}", e);
                continue;
            }
        }

        // Handle the client in-process (no fork on Windows)
        let result = handle_pipe_client(handle);
        if let Err(e) = result {
            eprintln!("cargo-slicer-rustc: pipe client error: {}", e);
        }

        pipe_server::disconnect_and_close(handle);
    }
}

#[cfg(windows)]
fn handle_pipe_client(handle: windows_sys::Win32::Foundation::HANDLE) -> Result<(), String> {
    let mut reader = pipe_server::PipeReader { handle };
    let mut writer = pipe_server::PipeWriter { handle };

    // Read compilation request (same binary protocol as Unix)
    let mut buf_reader = std::io::BufReader::with_capacity(16384, &mut reader);
    let (args, env_vars) = read_compilation_request(&mut buf_reader)?;
    drop(buf_reader);

    // Save current env vars that we'll override, so we can restore after
    let saved_env: Vec<(String, Option<String>)> = env_vars.iter()
        .map(|(key, _)| (key.clone(), std::env::var(key).ok()))
        .collect();

    // Set per-crate env vars from request
    for (key, value) in &env_vars {
        std::env::set_var(key, value);
    }

    // Reset compilation state for in-process reuse
    cargo_slicer::rustc_integration::reset_compilation_state();

    // Run the actual driver logic, catching panics
    let exit_code = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        run_driver_main(&args)
    })) {
        Ok(code) => code,
        Err(_) => {
            eprintln!("cargo-slicer-rustc: driver panicked during compilation");
            101 // Rust panic exit code
        }
    };

    // Restore env vars
    for (key, old_value) in saved_env {
        match old_value {
            Some(val) => std::env::set_var(&key, &val),
            None => std::env::remove_var(&key),
        }
    }

    // Send result: empty stderr (already on daemon stderr) + exit code
    send_compilation_result(&mut writer, &[], exit_code)
        .map_err(|e| format!("send result: {}", e))
}

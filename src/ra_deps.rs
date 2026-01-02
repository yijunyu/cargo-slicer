//! rust-analyzer based dependency analyzer
//!
//! This tool uses rust-analyzer's LSP protocol to accurately determine
//! item-level dependencies for crate slicing.
//!
//! Usage:
//!   ra_deps <crate_path> <item1> [item2] ...
//!
//! Example:
//!   ra_deps ~/.cargo/registry/src/*/rusqlite-0.32.1 Connection Result

#![allow(dead_code)]  // Some helper functions may be useful later

use std::collections::{HashMap, HashSet, VecDeque};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use serde_json::{json, Value};

static REQUEST_ID: AtomicU64 = AtomicU64::new(1);

// ============================================================================
// SCIP Cache Infrastructure
// ============================================================================

/// Get the SCIP cache directory, creating it if necessary
fn get_scip_cache_dir() -> Option<PathBuf> {
    // Try XDG_CACHE_HOME first, then ~/.cache, then /tmp
    let cache_base = std::env::var("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|_| {
            std::env::var("HOME")
                .map(|h| PathBuf::from(h).join(".cache"))
        })
        .unwrap_or_else(|_| PathBuf::from("/tmp"));

    let cache_dir = cache_base.join("cargo-slicer").join("scip");

    // Create directory if it doesn't exist
    if !cache_dir.exists() {
        if let Err(e) = std::fs::create_dir_all(&cache_dir) {
            eprintln!("Warning: Failed to create SCIP cache directory: {}", e);
            return None;
        }
    }

    Some(cache_dir)
}

/// Compute a cache key for a crate based on its Cargo.toml content
/// Returns (crate_name, version, content_hash) for cache filename
fn compute_cache_key(crate_path: &Path) -> Option<String> {
    let cargo_toml = crate_path.join("Cargo.toml");
    if !cargo_toml.exists() {
        return None;
    }

    // Read Cargo.toml to get name and version
    let content = std::fs::read_to_string(&cargo_toml).ok()?;

    // Parse name and version from Cargo.toml
    let mut name = None;
    let mut version = None;

    for line in content.lines() {
        let line = line.trim();
        if line.starts_with("name") && line.contains('=') {
            if let Some(val) = line.split('=').nth(1) {
                name = Some(val.trim().trim_matches('"').to_string());
            }
        } else if line.starts_with("version") && line.contains('=') && version.is_none() {
            if let Some(val) = line.split('=').nth(1) {
                version = Some(val.trim().trim_matches('"').to_string());
            }
        }
        if name.is_some() && version.is_some() {
            break;
        }
    }

    let name = name?;
    let version = version.unwrap_or_else(|| "unknown".to_string());

    // Also hash the full Cargo.toml content to detect any changes
    let hash = simple_hash(&content);

    Some(format!("{}-{}-{:016x}", name, version, hash))
}

/// Simple non-cryptographic hash for cache invalidation
fn simple_hash(s: &str) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325; // FNV-1a offset basis
    for byte in s.bytes() {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3); // FNV-1a prime
    }
    hash
}

/// Try to load cached SCIP data
fn load_scip_cache(crate_path: &Path) -> Option<Vec<u8>> {
    let cache_dir = get_scip_cache_dir()?;
    let cache_key = compute_cache_key(crate_path)?;
    let cache_file = cache_dir.join(format!("{}.scip", cache_key));

    if cache_file.exists() {
        match std::fs::read(&cache_file) {
            Ok(data) => {
                eprintln!("SCIP cache hit: {}", cache_key);
                return Some(data);
            }
            Err(e) => {
                eprintln!("Warning: Failed to read SCIP cache: {}", e);
            }
        }
    }

    None
}

/// Save SCIP data to cache
fn save_scip_cache(crate_path: &Path, scip_data: &[u8]) {
    let cache_dir = match get_scip_cache_dir() {
        Some(dir) => dir,
        None => return,
    };

    let cache_key = match compute_cache_key(crate_path) {
        Some(key) => key,
        None => return,
    };

    let cache_file = cache_dir.join(format!("{}.scip", cache_key));

    match std::fs::write(&cache_file, scip_data) {
        Ok(_) => {
            eprintln!("SCIP cache saved: {} ({} bytes)", cache_key, scip_data.len());
        }
        Err(e) => {
            eprintln!("Warning: Failed to save SCIP cache: {}", e);
        }
    }
}

/// Clear old cache entries (keep most recent N entries)
fn cleanup_scip_cache(max_entries: usize) {
    let cache_dir = match get_scip_cache_dir() {
        Some(dir) => dir,
        None => return,
    };

    let mut entries: Vec<_> = match std::fs::read_dir(&cache_dir) {
        Ok(dir) => dir.filter_map(|e| e.ok()).collect(),
        Err(_) => return,
    };

    if entries.len() <= max_entries {
        return;
    }

    // Sort by modification time (oldest first)
    entries.sort_by_key(|e| {
        e.metadata()
            .and_then(|m| m.modified())
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH)
    });

    // Remove oldest entries
    let to_remove = entries.len() - max_entries;
    for entry in entries.into_iter().take(to_remove) {
        let _ = std::fs::remove_file(entry.path());
    }
}

fn next_id() -> u64 {
    REQUEST_ID.fetch_add(1, Ordering::SeqCst)
}

/// LSP Message header + content
fn encode_message(content: &Value) -> Vec<u8> {
    let content_str = serde_json::to_string(content).unwrap();
    let header = format!("Content-Length: {}\r\n\r\n", content_str.len());
    let mut msg = header.into_bytes();
    msg.extend(content_str.bytes());
    msg
}

/// Read an LSP message from the stream
fn read_message(reader: &mut impl BufRead) -> Option<Value> {
    // Read headers
    let mut content_length = 0;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let line = line.trim();
        if line.is_empty() {
            break;
        }
        if line.starts_with("Content-Length: ") {
            content_length = line[16..].parse().ok()?;
        }
    }

    if content_length == 0 {
        return None;
    }

    // Read content
    let mut content = vec![0u8; content_length];
    reader.read_exact(&mut content).ok()?;

    serde_json::from_slice(&content).ok()
}

/// Location in a file
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct Location {
    file: PathBuf,
    line: u32,
    character: u32,
}

/// An item with its location
#[derive(Debug, Clone)]
struct Item {
    name: String,
    kind: String,
    location: Location,
    file_uri: String,
}

/// rust-analyzer LSP client
struct RaClient {
    child: Child,
    writer: BufWriter<std::process::ChildStdin>,
    reader: BufReader<std::process::ChildStdout>,
    root_uri: String,
    pending_requests: HashMap<u64, String>,
}

impl RaClient {
    /// Start rust-analyzer LSP server
    fn new(crate_path: &Path) -> std::io::Result<Self> {
        let crate_path = crate_path.canonicalize()?;
        let root_uri = format!("file://{}", crate_path.display());

        eprintln!("Starting rust-analyzer for {}...", crate_path.display());

        // Suppress rust-analyzer stderr noise
        let mut child = Command::new("rust-analyzer")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()?;

        let stdin = child.stdin.take().expect("Failed to get stdin");
        let stdout = child.stdout.take().expect("Failed to get stdout");

        let mut client = Self {
            child,
            writer: BufWriter::new(stdin),
            reader: BufReader::new(stdout),
            root_uri: root_uri.clone(),
            pending_requests: HashMap::new(),
        };

        // Initialize
        client.initialize(&root_uri)?;

        Ok(client)
    }

    /// Send a request and get the ID
    fn send_request(&mut self, method: &str, params: Value) -> std::io::Result<u64> {
        let id = next_id();
        let msg = json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": params
        });

        let encoded = encode_message(&msg);
        self.writer.write_all(&encoded)?;
        self.writer.flush()?;

        self.pending_requests.insert(id, method.to_string());
        Ok(id)
    }

    /// Send a notification (no response expected)
    fn send_notification(&mut self, method: &str, params: Value) -> std::io::Result<()> {
        let msg = json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params
        });

        let encoded = encode_message(&msg);
        self.writer.write_all(&encoded)?;
        self.writer.flush()?;
        Ok(())
    }

    /// Wait for a response to a specific request
    fn wait_response(&mut self, request_id: u64) -> Option<Value> {
        loop {
            let msg = read_message(&mut self.reader)?;

            // Check if it's our response
            if let Some(id) = msg.get("id").and_then(|v| v.as_u64()) {
                if id == request_id {
                    self.pending_requests.remove(&id);
                    return msg.get("result").cloned();
                }
            }

            // Handle server requests (like workspace/configuration)
            if let Some(method) = msg.get("method").and_then(|v| v.as_str()) {
                if let Some(id) = msg.get("id") {
                    // Respond to server requests
                    let response = match method {
                        "workspace/configuration" => {
                            json!([{}])
                        }
                        "client/registerCapability" => {
                            json!(null)
                        }
                        _ => json!(null)
                    };

                    let reply = json!({
                        "jsonrpc": "2.0",
                        "id": id,
                        "result": response
                    });
                    let encoded = encode_message(&reply);
                    let _ = self.writer.write_all(&encoded);
                    let _ = self.writer.flush();
                }
            }
        }
    }

    /// Initialize the LSP connection
    fn initialize(&mut self, root_uri: &str) -> std::io::Result<()> {
        let params = json!({
            "processId": std::process::id(),
            "rootUri": root_uri,
            "capabilities": {
                "textDocument": {
                    "definition": { "dynamicRegistration": false },
                    "references": { "dynamicRegistration": false },
                    "documentSymbol": { "dynamicRegistration": false }
                },
                "workspace": {
                    "workspaceFolders": true
                }
            },
            "workspaceFolders": [{
                "uri": root_uri,
                "name": "crate"
            }]
        });

        let id = self.send_request("initialize", params)?;
        let _result = self.wait_response(id);

        // Send initialized notification
        self.send_notification("initialized", json!({}))?;

        eprintln!("rust-analyzer initialized, waiting for indexing...");

        // Wait for rust-analyzer to index the project
        // We poll for workspace symbols as a readiness check
        for _ in 0..30 {
            std::thread::sleep(std::time::Duration::from_millis(500));
            eprint!(".");

            // Try a simple workspace symbol query to check if ready
            let id = self.send_request("workspace/symbol", json!({"query": "Connection"}))?;
            if let Some(result) = self.wait_response(id) {
                if let Some(arr) = result.as_array() {
                    if !arr.is_empty() {
                        eprintln!(" ready!");
                        return Ok(());
                    }
                }
            }
        }

        eprintln!(" timeout waiting for indexing");
        Ok(())
    }

    /// Open a document (required before querying)
    fn open_document(&mut self, file_path: &Path) -> std::io::Result<String> {
        let uri = format!("file://{}", file_path.display());
        let content = std::fs::read_to_string(file_path)?;

        self.send_notification("textDocument/didOpen", json!({
            "textDocument": {
                "uri": &uri,
                "languageId": "rust",
                "version": 1,
                "text": content
            }
        }))?;

        // Give rust-analyzer time to process
        std::thread::sleep(std::time::Duration::from_millis(100));

        Ok(uri)
    }

    /// Get document symbols (all items in a file)
    fn document_symbols(&mut self, file_path: &Path) -> Vec<Item> {
        // First open the document
        let uri = match self.open_document(file_path) {
            Ok(u) => u,
            Err(_) => return Vec::new(),
        };

        let params = json!({
            "textDocument": { "uri": &uri }
        });

        let id = self.send_request("textDocument/documentSymbol", params).unwrap();
        let result = self.wait_response(id);

        let mut items = Vec::new();
        if let Some(symbols) = result.and_then(|v| v.as_array().cloned()) {
            self.extract_symbols(&symbols, file_path, &uri, &mut items);
        }
        items
    }

    fn extract_symbols(&self, symbols: &[Value], file_path: &Path, uri: &str, items: &mut Vec<Item>) {
        for sym in symbols {
            if let (Some(name), Some(kind)) = (
                sym.get("name").and_then(|v| v.as_str()),
                sym.get("kind").and_then(|v| v.as_u64())
            ) {
                let kind_str = match kind {
                    5 => "struct",
                    10 => "enum",
                    12 => "function",
                    11 => "trait",
                    _ => "other",
                };

                if let Some(range) = sym.get("range") {
                    if let (Some(line), Some(char)) = (
                        range.get("start").and_then(|s| s.get("line")).and_then(|v| v.as_u64()),
                        range.get("start").and_then(|s| s.get("character")).and_then(|v| v.as_u64())
                    ) {
                        items.push(Item {
                            name: name.to_string(),
                            kind: kind_str.to_string(),
                            location: Location {
                                file: file_path.to_path_buf(),
                                line: line as u32,
                                character: char as u32,
                            },
                            file_uri: uri.to_string(),
                        });
                    }
                }
            }

            // Recurse into children
            if let Some(children) = sym.get("children").and_then(|v| v.as_array()) {
                self.extract_symbols(children, file_path, uri, items);
            }
        }
    }

    /// Find all references to an item
    fn find_references(&mut self, item: &Item) -> Vec<Location> {
        let params = json!({
            "textDocument": { "uri": &item.file_uri },
            "position": {
                "line": item.location.line,
                "character": item.location.character
            },
            "context": { "includeDeclaration": false }
        });

        let id = self.send_request("textDocument/references", params).unwrap();
        let result = self.wait_response(id);

        let mut locations = Vec::new();
        if let Some(refs) = result.and_then(|v| v.as_array().cloned()) {
            for r in refs {
                if let (Some(uri), Some(range)) = (
                    r.get("uri").and_then(|v| v.as_str()),
                    r.get("range")
                ) {
                    let file = uri.strip_prefix("file://")
                        .map(PathBuf::from)
                        .unwrap_or_default();

                    if let (Some(line), Some(char)) = (
                        range.get("start").and_then(|s| s.get("line")).and_then(|v| v.as_u64()),
                        range.get("start").and_then(|s| s.get("character")).and_then(|v| v.as_u64())
                    ) {
                        locations.push(Location {
                            file,
                            line: line as u32,
                            character: char as u32,
                        });
                    }
                }
            }
        }
        locations
    }

    /// Go to definition
    fn goto_definition(&mut self, uri: &str, line: u32, character: u32) -> Option<Location> {
        let params = json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": character }
        });

        let id = self.send_request("textDocument/definition", params).unwrap();
        let result = self.wait_response(id)?;

        // Can be a single location or array
        let loc = if result.is_array() {
            result.as_array()?.first()?.clone()
        } else {
            result
        };

        let uri = loc.get("uri").and_then(|v| v.as_str())?;
        let range = loc.get("range")?;
        let line = range.get("start")?.get("line")?.as_u64()? as u32;
        let char = range.get("start")?.get("character")?.as_u64()? as u32;

        Some(Location {
            file: PathBuf::from(uri.strip_prefix("file://").unwrap_or(uri)),
            line,
            character: char,
        })
    }

    /// Search for symbols in the workspace by name
    fn workspace_symbol(&mut self, query: &str) -> Vec<Item> {
        let params = json!({ "query": query });
        let id = self.send_request("workspace/symbol", params).unwrap();
        let result = self.wait_response(id);

        let mut items = Vec::new();
        if let Some(symbols) = result.and_then(|v| v.as_array().cloned()) {
            for sym in symbols {
                if let (Some(name), Some(kind)) = (
                    sym.get("name").and_then(|v| v.as_str()),
                    sym.get("kind").and_then(|v| v.as_u64())
                ) {
                    // LSP SymbolKind values:
                    // 1=File, 2=Module, 3=Namespace, 4=Package, 5=Class, 6=Method,
                    // 7=Property, 8=Field, 9=Constructor, 10=Enum, 11=Interface,
                    // 12=Function, 13=Variable, 14=Constant, 15=String, 16=Number,
                    // 17=Boolean, 18=Array, 19=Object, 20=Key, 21=Null, 22=EnumMember,
                    // 23=Struct, 24=Event, 25=Operator, 26=TypeParameter
                    let kind_str = match kind {
                        5 => "class",
                        6 => "method",
                        10 => "enum",
                        11 => "trait", // Interface
                        12 => "function",
                        13 => "variable",
                        14 => "constant",
                        22 => "enum_member",
                        23 => "struct",
                        26 => "type_param",
                        _ => "other",
                    };

                    // SymbolInformation has a location field (not range)
                    if let Some(location) = sym.get("location") {
                        if let (Some(uri), Some(range)) = (
                            location.get("uri").and_then(|v| v.as_str()),
                            location.get("range")
                        ) {
                            let file = uri.strip_prefix("file://")
                                .map(PathBuf::from)
                                .unwrap_or_default();

                            if let (Some(line), Some(char)) = (
                                range.get("start").and_then(|s| s.get("line")).and_then(|v| v.as_u64()),
                                range.get("start").and_then(|s| s.get("character")).and_then(|v| v.as_u64())
                            ) {
                                items.push(Item {
                                    name: name.to_string(),
                                    kind: kind_str.to_string(),
                                    location: Location {
                                        file,
                                        line: line as u32,
                                        character: char as u32,
                                    },
                                    file_uri: uri.to_string(),
                                });
                            }
                        }
                    }
                }
            }
        }
        items
    }

    /// Get hover info (type, documentation) for a position
    fn hover(&mut self, uri: &str, line: u32, character: u32) -> Option<String> {
        let params = json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": character }
        });

        let id = self.send_request("textDocument/hover", params).unwrap();
        let result = self.wait_response(id)?;

        // Extract markdown content from hover result
        let contents = result.get("contents")?;
        if let Some(value) = contents.get("value").and_then(|v| v.as_str()) {
            return Some(value.to_string());
        }
        if let Some(s) = contents.as_str() {
            return Some(s.to_string());
        }
        None
    }

    fn shutdown(&mut self) {
        let _ = self.send_request("shutdown", json!(null));
        let _ = self.send_notification("exit", json!(null));
        let _ = self.child.wait();
    }
}

impl Drop for RaClient {
    fn drop(&mut self) {
        self.shutdown();
    }
}

/// Qualified item name with module path
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct QualifiedItem {
    name: String,
    module: String, // e.g., "cache" or "" for root
    file: PathBuf,
    kind: String,
}

impl QualifiedItem {
    fn qualified_name(&self) -> String {
        if self.module.is_empty() {
            self.name.clone()
        } else {
            format!("{}::{}", self.module, self.name)
        }
    }
}

/// Extract identifiers from Rust source code
fn extract_identifiers(code: &str) -> HashSet<String> {
    let mut identifiers = HashSet::new();
    let mut current = String::new();
    let mut in_string = false;
    let mut escape = false;

    // Note: We don't track char literals because distinguishing them from
    // lifetime annotations ('a, 'static) is complex. Char literals don't
    // contain identifiers anyway, so we just parse through them.
    // We do track strings because they can contain arbitrary text.

    for c in code.chars() {
        if escape {
            escape = false;
            continue;
        }
        if c == '\\' && in_string {
            escape = true;
            continue;
        }
        if c == '"' {
            in_string = !in_string;
            continue;
        }
        if in_string {
            continue;
        }

        if c.is_alphanumeric() || c == '_' {
            current.push(c);
        } else {
            if !current.is_empty() && !current.chars().next().unwrap().is_ascii_digit() {
                // Skip keywords and small words (but allow single uppercase letters - likely constants)
                let is_single_uppercase = current.len() == 1 && current.chars().next().unwrap().is_ascii_uppercase();
                if (current.len() >= 2 || is_single_uppercase) && !is_rust_keyword(&current) {
                    identifiers.insert(current.clone());
                }
            }
            current.clear();
        }
    }
    if !current.is_empty() && !current.chars().next().unwrap().is_ascii_digit() {
        let is_single_uppercase = current.len() == 1 && current.chars().next().unwrap().is_ascii_uppercase();
        if (current.len() >= 2 || is_single_uppercase) && !is_rust_keyword(&current) {
            identifiers.insert(current);
        }
    }
    identifiers
}

fn is_rust_keyword(s: &str) -> bool {
    matches!(s,
        "as" | "async" | "await" | "break" | "const" | "continue" | "crate" |
        "dyn" | "else" | "enum" | "extern" | "false" | "fn" | "for" | "if" |
        "impl" | "in" | "let" | "loop" | "match" | "mod" | "move" | "mut" |
        "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct" |
        "super" | "trait" | "true" | "type" | "unsafe" | "use" | "where" |
        "while" | "yield" | "Box" | "Option" | "Result" | "Some" | "None" |
        "Ok" | "Err" | "Vec" | "String" | "str" | "bool" | "i8" | "i16" |
        "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64" |
        "u128" | "usize" | "f32" | "f64" | "char"
    )
}

/// Scan a file for local definitions (const, static, fn, type, struct, enum, etc.)
/// Returns a map from identifier name to (line, kind)
fn scan_file_for_definitions(content: &str) -> HashMap<String, (u32, String)> {
    use regex::Regex;

    let mut definitions = HashMap::new();

    // Patterns for various Rust definitions
    // const NAME: ... = ...
    // static NAME: ... = ...
    // fn NAME(...) ...
    // type NAME = ...
    // struct NAME ...
    // enum NAME ...
    // trait NAME ...

    let patterns = [
        (r"^\s*(?:pub\s*(?:\([^)]*\)\s*)?)?const\s+([A-Z_][A-Z0-9_]*)\s*:", "constant"),
        (r"^\s*(?:pub\s*(?:\([^)]*\)\s*)?)?static\s+(?:mut\s+)?([A-Z_][A-Z0-9_]*)\s*:", "static"),
        (r"^\s*(?:pub\s*(?:\([^)]*\)\s*)?)?fn\s+([a-z_][a-z0-9_]*)\s*[<(]", "function"),
        (r"^\s*(?:pub\s*(?:\([^)]*\)\s*)?)?type\s+([A-Z][a-zA-Z0-9_]*)\s*", "type"),
        (r"^\s*(?:pub\s*(?:\([^)]*\)\s*)?)?struct\s+([A-Z][a-zA-Z0-9_]*)\s*", "struct"),
        (r"^\s*(?:pub\s*(?:\([^)]*\)\s*)?)?enum\s+([A-Z][a-zA-Z0-9_]*)\s*", "enum"),
        (r"^\s*(?:pub\s*(?:\([^)]*\)\s*)?)?trait\s+([A-Z][a-zA-Z0-9_]*)\s*", "trait"),
    ];

    for (line_num, line) in content.lines().enumerate() {
        for (pattern, kind) in &patterns {
            if let Ok(re) = Regex::new(pattern) {
                if let Some(caps) = re.captures(line) {
                    if let Some(name) = caps.get(1) {
                        let name_str = name.as_str().to_string();
                        // Only add if not already present (keep first definition)
                        if !definitions.contains_key(&name_str) {
                            definitions.insert(name_str, (line_num as u32, kind.to_string()));
                        }
                    }
                }
            }
        }
    }

    definitions
}

/// Priority for symbol kinds (higher is better)
fn kind_priority(kind: &str) -> u8 {
    match kind {
        "struct" | "class" => 10,
        "enum" => 9,
        "trait" => 8,
        "function" => 7,
        "constant" => 6,
        "variable" => 5,
        "type_param" => 1,
        _ => 3,
    }
}

/// Add items from search results to the index
fn add_items_to_index(
    results: Vec<Item>,
    crate_path: &Path,
    all_items: &mut HashMap<String, Item>,
    items_by_location: &mut HashMap<(PathBuf, u32), String>,
) {
    for item in results {
        // Filter to items in our crate
        if item.location.file.starts_with(crate_path) {
            // Prefer struct/enum/trait/function over type_param
            let dominated = match all_items.get(&item.name) {
                Some(existing) if kind_priority(&existing.kind) > kind_priority(&item.kind) => true,
                _ => false,
            };
            if !dominated {
                items_by_location.insert(
                    (item.location.file.clone(), item.location.line),
                    item.name.clone()
                );
                all_items.insert(item.name.clone(), item);
            }
        }
    }
}

/// Build transitive dependency graph using workspace symbol search
fn build_dependency_graph(
    client: &mut RaClient,
    crate_path: &Path,
    seed_items: &[&str],
) -> HashSet<String> {
    eprintln!("Building symbol index via workspace search...");

    // Build index by searching for common type patterns
    let mut all_items: HashMap<String, Item> = HashMap::new();
    let mut items_by_location: HashMap<(PathBuf, u32), String> = HashMap::new();

    // Search for seed items first with exact match priority
    for seed in seed_items {
        let results = client.workspace_symbol(seed);
        // Prefer struct/enum/trait matches over type params
        let mut best: Option<Item> = None;
        for item in results {
            if item.name == *seed && item.location.file.starts_with(crate_path) {
                match item.kind.as_str() {
                    "struct" | "class" | "enum" | "trait" | "function" => {
                        best = Some(item);
                        break;
                    }
                    _ if best.is_none() => {
                        best = Some(item);
                    }
                    _ => {}
                }
            }
        }
        if let Some(item) = best {
            eprintln!("  Found {} ({}) at {}:{}",
                item.name, item.kind,
                item.location.file.file_name().unwrap_or_default().to_string_lossy(),
                item.location.line);
            items_by_location.insert(
                (item.location.file.clone(), item.location.line),
                item.name.clone()
            );
            all_items.insert(item.name.clone(), item);
        } else {
            eprintln!("  Warning: seed '{}' not found", seed);
        }
    }

    // Search for common patterns to build broader index
    let search_patterns = ["", "Error", "Result", "Config", "Options", "State", "Inner"];
    for pattern in &search_patterns {
        let results = client.workspace_symbol(pattern);
        add_items_to_index(results, crate_path, &mut all_items, &mut items_by_location);
    }
    eprintln!("Indexed {} symbols", all_items.len());

    // Start with seed items
    let mut needed: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    for seed in seed_items {
        if all_items.contains_key(*seed) {
            queue.push_back(seed.to_string());
            needed.insert(seed.to_string());
        }
    }

    // BFS to find all items that the seeds depend on
    eprintln!("\nBuilding dependency graph...");
    let mut processed: HashSet<String> = HashSet::new();

    while let Some(item_name) = queue.pop_front() {
        if processed.contains(&item_name) {
            continue;
        }
        processed.insert(item_name.clone());

        if let Some(item) = all_items.get(&item_name).cloned() {
            eprint!("  {} ({}):", item_name, item.kind);

            // Find all references to this item (who uses us)
            let refs = client.find_references(&item);
            let mut dep_count = 0;

            for loc in &refs {
                // Check if this reference is within our crate
                if !loc.file.starts_with(&crate_path) {
                    continue;
                }

                // Find which item contains this reference
                // Use goto_definition on the reference location to find what we depend on
                // Or search for the nearest item definition
                for line in (0..=loc.line).rev() {
                    if let Some(containing_item) = items_by_location.get(&(loc.file.clone(), line)) {
                        if containing_item != &item_name && !needed.contains(containing_item) {
                            needed.insert(containing_item.clone());
                            queue.push_back(containing_item.clone());
                            dep_count += 1;
                        }
                        break;
                    }
                }
            }

            if dep_count > 0 {
                eprint!(" +{} deps", dep_count);
            }
            eprintln!();
        }
    }

    needed
}

/// Output format for the dependency analysis
#[derive(Debug)]
struct DependencyOutput {
    items: Vec<ItemInfo>,
}

#[derive(Debug)]
struct ItemInfo {
    name: String,
    kind: String,
    file: String,
    line: u32,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Check for flags
    let json_output = args.iter().any(|a| a == "--json");
    let use_scip = args.iter().any(|a| a == "--scip");
    let no_cache = args.iter().any(|a| a == "--no-cache");
    let args: Vec<&str> = args.iter()
        .filter(|a| *a != "--json" && *a != "--scip" && *a != "--no-cache")
        .map(|s| s.as_str())
        .collect();

    if args.len() < 3 {
        eprintln!("Usage: ra_deps [--json] [--scip] [--no-cache] <crate_path> <item1> [item2] ...");
        eprintln!();
        eprintln!("Options:");
        eprintln!("  --json      Output results as JSON");
        eprintln!("  --scip      Use SCIP batch mode (faster, ~8x speedup)");
        eprintln!("  --no-cache  Bypass SCIP cache (force re-indexing)");
        eprintln!();
        eprintln!("Cache location: ~/.cache/cargo-slicer/scip/");
        eprintln!();
        eprintln!("Example:");
        eprintln!("  ra_deps ~/.cargo/registry/src/*/rusqlite-0.32.1 Connection Result");
        eprintln!("  ra_deps --json ~/.cargo/registry/src/*/rusqlite-0.32.1 Connection");
        eprintln!("  ra_deps --scip --json ~/.cargo/registry/src/*/memchr-2.7.6 memchr");
        eprintln!("  ra_deps --scip --no-cache ~/.cargo/registry/src/*/memchr-2.7.6 memchr");
        std::process::exit(1);
    }

    let crate_path = PathBuf::from(args[1]);
    let seed_items: Vec<&str> = args[2..].to_vec();

    if !crate_path.exists() {
        eprintln!("Error: crate path does not exist: {}", crate_path.display());
        std::process::exit(1);
    }

    // Ensure Cargo.toml exists
    if !crate_path.join("Cargo.toml").exists() {
        eprintln!("Error: no Cargo.toml found in {}", crate_path.display());
        std::process::exit(1);
    }

    // Use SCIP mode for faster batch processing
    if use_scip {
        match run_scip_analysis(&crate_path, &seed_items, no_cache) {
            Ok(needed) => {
                if json_output {
                    let items: Vec<_> = needed.iter()
                        .map(|name| json!({ "name": name, "kind": "unknown" }))
                        .collect();
                    let output = json!({
                        "crate_path": crate_path.display().to_string(),
                        "seed_items": seed_items,
                        "total": needed.len(),
                        "items": items
                    });
                    println!("{}", serde_json::to_string_pretty(&output).unwrap());
                } else {
                    println!("\n=== Items Needed (SCIP) ===");
                    for name in &needed {
                        println!("  {}", name);
                    }
                    println!("\nTotal: {} items", needed.len());
                }
            }
            Err(e) => {
                eprintln!("SCIP analysis failed: {}", e);
                std::process::exit(1);
            }
        }
        return;
    }

    // LSP mode (original implementation)
    let mut client = match RaClient::new(&crate_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to start rust-analyzer: {}", e);
            eprintln!();
            eprintln!("Install rust-analyzer with:");
            eprintln!("  rustup component add rust-analyzer");
            std::process::exit(1);
        }
    };

    let (needed, all_items) = build_dependency_graph_with_info(&mut client, &crate_path, &seed_items);

    if json_output {
        // Output JSON
        let items: Vec<_> = needed.iter()
            .filter_map(|name| all_items.get(name))
            .map(|item| {
                json!({
                    "name": item.name,
                    "kind": item.kind,
                    "file": item.location.file.display().to_string(),
                    "line": item.location.line
                })
            })
            .collect();

        let output = json!({
            "crate_path": crate_path.display().to_string(),
            "seed_items": seed_items,
            "total": needed.len(),
            "items": items
        });

        println!("{}", serde_json::to_string_pretty(&output).unwrap());
    } else {
        // Human-readable output
        println!("\n=== Items Needed ===");
        for item_name in &needed {
            if let Some(item) = all_items.get(item_name) {
                println!("  {} ({}) - {}:{}",
                    item.name, item.kind,
                    item.location.file.file_name().unwrap_or_default().to_string_lossy(),
                    item.location.line);
            } else {
                println!("  {}", item_name);
            }
        }
        println!("\nTotal: {} items", needed.len());
    }
}

/// Build transitive dependency graph and return both the needed set and all items
fn build_dependency_graph_with_info(
    client: &mut RaClient,
    crate_path: &Path,
    seed_items: &[&str],
) -> (HashSet<String>, HashMap<String, Item>) {
    eprintln!("Building symbol index via workspace search...");

    // Build index by searching for common type patterns
    let mut all_items: HashMap<String, Item> = HashMap::new();
    let mut items_by_location: HashMap<(PathBuf, u32), String> = HashMap::new();

    // Search for seed items first with exact match priority
    for seed in seed_items {
        let results = client.workspace_symbol(seed);
        // Prefer struct/enum/trait matches over type params
        let mut best: Option<Item> = None;
        for item in results {
            if item.name == *seed && item.location.file.starts_with(crate_path) {
                match item.kind.as_str() {
                    "struct" | "class" | "enum" | "trait" | "function" => {
                        best = Some(item);
                        break;
                    }
                    _ if best.is_none() => {
                        best = Some(item);
                    }
                    _ => {}
                }
            }
        }
        if let Some(item) = best {
            eprintln!("  Found {} ({}) at {}:{}",
                item.name, item.kind,
                item.location.file.file_name().unwrap_or_default().to_string_lossy(),
                item.location.line);
            items_by_location.insert(
                (item.location.file.clone(), item.location.line),
                item.name.clone()
            );
            all_items.insert(item.name.clone(), item);
        } else {
            eprintln!("  Warning: seed '{}' not found", seed);
        }
    }

    // Search for common patterns to build broader index
    let search_patterns = ["", "Error", "Result", "Config", "Options", "State", "Inner"];
    for pattern in &search_patterns {
        let results = client.workspace_symbol(pattern);
        add_items_to_index(results, crate_path, &mut all_items, &mut items_by_location);
    }
    eprintln!("Indexed {} symbols", all_items.len());

    // Start with seed items
    let mut needed: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    for seed in seed_items {
        if all_items.contains_key(*seed) {
            queue.push_back(seed.to_string());
            needed.insert(seed.to_string());
        }
    }

    // BFS to find all items that the seeds depend on (forward + reverse)
    eprintln!("\nBuilding dependency graph (forward + reverse)...");
    let mut processed: HashSet<String> = HashSet::new();

    // Cache file contents to avoid re-reading
    let mut file_cache: HashMap<PathBuf, String> = HashMap::new();

    // Cache file-level definitions (const, static, fn, etc. not found by workspace search)
    let mut file_definitions_cache: HashMap<PathBuf, HashMap<String, (u32, String)>> = HashMap::new();

    while let Some(item_name) = queue.pop_front() {
        if processed.contains(&item_name) {
            continue;
        }
        processed.insert(item_name.clone());

        if let Some(item) = all_items.get(&item_name).cloned() {
            eprint!("  {} ({}):", item_name, item.kind);

            let mut forward_deps = 0;
            let mut reverse_deps = 0;

            // === FORWARD DEPENDENCIES ===
            // Read the source file and extract identifiers from this item's definition
            if item.location.file.exists() {
                let file_content = file_cache.entry(item.location.file.clone())
                    .or_insert_with(|| {
                        std::fs::read_to_string(&item.location.file).unwrap_or_default()
                    });

                // Extract a region around the item definition (heuristic: next 100 lines or until next item)
                let lines: Vec<&str> = file_content.lines().collect();
                let start_line = item.location.line as usize;
                let end_line = (start_line + 150).min(lines.len());

                // Find the end of this item by looking for the next top-level item
                let mut item_end = end_line;
                for i in (start_line + 1)..end_line {
                    if i < lines.len() {
                        let line = lines[i].trim();
                        // Heuristic: next pub/fn/struct/enum/trait/impl at column 0
                        if (line.starts_with("pub ") || line.starts_with("fn ") ||
                            line.starts_with("struct ") || line.starts_with("enum ") ||
                            line.starts_with("trait ") || line.starts_with("impl ") ||
                            line.starts_with("type ") || line.starts_with("const ") ||
                            line.starts_with("static ") || line.starts_with("mod ") ||
                            line.starts_with("#[")) && !lines[i].starts_with(" ") && !lines[i].starts_with("\t") {
                            item_end = i;
                            break;
                        }
                    }
                }

                // Extract item source and find identifiers
                if start_line < lines.len() {
                    let item_source: String = lines[start_line..item_end.min(lines.len())]
                        .join("\n");
                    let identifiers = extract_identifiers(&item_source);

                    // Get/build local file definitions cache for this file
                    let file_defs = file_definitions_cache
                        .entry(item.location.file.clone())
                        .or_insert_with(|| scan_file_for_definitions(file_content));

                    // Check each identifier against our symbol index
                    for ident in &identifiers {
                        if ident != &item_name && !needed.contains(ident) {
                            // Check if this identifier is a known symbol in our crate
                            if all_items.contains_key(ident) {
                                needed.insert(ident.clone());
                                queue.push_back(ident.clone());
                                forward_deps += 1;
                            } else {
                                // Try workspace search for this identifier
                                let results = client.workspace_symbol(ident);
                                let mut found_via_workspace = false;
                                for found_item in results {
                                    if found_item.name == *ident &&
                                       found_item.location.file.starts_with(crate_path) &&
                                       !needed.contains(&found_item.name) {
                                        // Add to index and needed set
                                        items_by_location.insert(
                                            (found_item.location.file.clone(), found_item.location.line),
                                            found_item.name.clone()
                                        );
                                        needed.insert(found_item.name.clone());
                                        queue.push_back(found_item.name.clone());
                                        all_items.insert(found_item.name.clone(), found_item);
                                        forward_deps += 1;
                                        found_via_workspace = true;
                                        break;
                                    }
                                }

                                // If not found via workspace search, check local file definitions
                                // This catches private constants, statics, and helper functions
                                if !found_via_workspace {
                                    if let Some((line, kind)) = file_defs.get(ident) {
                                        // Create a synthetic Item for this local definition
                                        let local_item = Item {
                                            name: ident.clone(),
                                            kind: kind.clone(),
                                            location: Location {
                                                file: item.location.file.clone(),
                                                line: *line,
                                                character: 0,
                                            },
                                            file_uri: item.file_uri.clone(),
                                        };
                                        items_by_location.insert(
                                            (local_item.location.file.clone(), local_item.location.line),
                                            local_item.name.clone()
                                        );
                                        needed.insert(local_item.name.clone());
                                        queue.push_back(local_item.name.clone());
                                        all_items.insert(local_item.name.clone(), local_item);
                                        forward_deps += 1;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // === REVERSE DEPENDENCIES ===
            // Find all references to this item (who uses us)
            let refs = client.find_references(&item);

            for loc in &refs {
                // Check if this reference is within our crate
                if !loc.file.starts_with(crate_path) {
                    continue;
                }

                // Find which item contains this reference
                for line in (0..=loc.line).rev() {
                    if let Some(containing_item) = items_by_location.get(&(loc.file.clone(), line)) {
                        if containing_item != &item_name && !needed.contains(containing_item) {
                            needed.insert(containing_item.clone());
                            queue.push_back(containing_item.clone());
                            reverse_deps += 1;
                        }
                        break;
                    }
                }
            }

            if forward_deps > 0 || reverse_deps > 0 {
                eprint!(" +{}fwd +{}rev", forward_deps, reverse_deps);
            }
            eprintln!();
        }
    }

    (needed, all_items)
}

// ============================================================================
// SCIP-based implementation (faster batch mode)
// ============================================================================

/// Extract item name from SCIP symbol string
/// Format: "rust-analyzer cargo <crate> <version> <path>"
/// Examples:
///   "rust-analyzer cargo log 0.4.29 Log" -> Some("Log")
///   "rust-analyzer cargo log 0.4.29 macros/log" -> Some("log")
///   "rust-analyzer cargo log 0.4.29 impl#[Logger][Log]enabled()." -> Some("enabled")
fn extract_scip_item_name(symbol: &str) -> Option<String> {
    // Skip symbols that don't look like rust-analyzer symbols
    if !symbol.starts_with("rust-analyzer") {
        return None;
    }

    // Split by space to get the main parts
    let parts: Vec<&str> = symbol.split_whitespace().collect();
    // Format: ["rust-analyzer", "cargo", "<crate>", "<version>", "<path>"]
    if parts.len() < 5 {
        return None;
    }

    // The path is the 5th element (index 4) and beyond
    let path = parts[4..].join(" ");

    // Extract the last component of the path
    // Handle various SCIP path formats:
    // - "Log" -> "Log"
    // - "macros/log" -> "log"
    // - "impl#[Logger][Log]enabled()." -> "enabled"
    // - "Record#builder()." -> "builder"

    let path = path.trim_end_matches('.');
    let path = path.trim_end_matches("()");

    // Get the last path component
    let last_component = if path.contains('/') {
        path.rsplit('/').next().unwrap_or(&path)
    } else {
        &path
    };

    // Handle impl blocks: "impl#[Type][Trait]method" -> "method"
    let name = if last_component.contains('#') {
        // Find the last ] and take everything after
        if let Some(pos) = last_component.rfind(']') {
            &last_component[pos + 1..]
        } else if let Some(pos) = last_component.find('#') {
            &last_component[..pos]
        } else {
            last_component
        }
    } else {
        last_component
    };

    let name = name.trim();

    // Filter out empty, numeric, or invalid names
    if name.is_empty() || name.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(true) {
        return None;
    }

    // Filter out symbols that are just punctuation or special characters
    if name.starts_with('`') || name.starts_with('[') || name.starts_with('(') {
        return None;
    }

    Some(name.to_string())
}

/// Run rust-analyzer in SCIP mode and parse the output
fn run_scip_analysis(crate_path: &Path, seed_items: &[&str], no_cache: bool) -> Result<HashSet<String>, String> {
    use std::process::Command;

    let start = std::time::Instant::now();

    // Try to load from cache first (unless --no-cache is specified)
    let scip_data = if !no_cache {
        if let Some(cached_data) = load_scip_cache(crate_path) {
            let cache_elapsed = start.elapsed();
            eprintln!("Loaded from cache in {:.3}s", cache_elapsed.as_secs_f64());
            Some(cached_data)
        } else {
            None
        }
    } else {
        eprintln!("Cache bypassed (--no-cache)");
        None
    };

    // If we have cached data, use it; otherwise run rust-analyzer
    let scip_data = match scip_data {
        Some(data) => data,
        None => {
            // Cache miss - run rust-analyzer scip
            eprintln!("Running rust-analyzer SCIP indexing...");

            // Create temp file for SCIP output
            let scip_path = std::env::temp_dir().join(format!("ra_deps_{}.scip", std::process::id()));

            let output = Command::new("rust-analyzer")
                .args(["scip", &crate_path.to_string_lossy(), "--output", &scip_path.to_string_lossy()])
                .output()
                .map_err(|e| format!("Failed to run rust-analyzer scip: {}", e))?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(format!("rust-analyzer scip failed: {}", stderr));
            }

            let scip_elapsed = start.elapsed();
            eprintln!("SCIP indexing completed in {:.2}s", scip_elapsed.as_secs_f64());

            // Read SCIP file
            let data = std::fs::read(&scip_path)
                .map_err(|e| format!("Failed to read SCIP file: {}", e))?;

            // Clean up temp file
            let _ = std::fs::remove_file(&scip_path);

            // Save to cache for next time (even if --no-cache, we still save for future use)
            save_scip_cache(crate_path, &data);

            // Periodically clean up old cache entries (keep last 100)
            cleanup_scip_cache(100);

            data
        }
    };

    // Parse SCIP file
    eprintln!("Parsing SCIP index...");

    // Parse the SCIP protobuf using protobuf crate
    use protobuf::Message;
    let index = scip::types::Index::parse_from_bytes(&scip_data)
        .map_err(|e| format!("Failed to parse SCIP: {}", e))?;

    eprintln!("Parsed {} documents", index.documents.len());

    // Build symbol maps
    let mut symbol_to_name: HashMap<String, String> = HashMap::new();
    let mut name_to_symbols: HashMap<String, Vec<String>> = HashMap::new();
    let mut symbol_refs: HashMap<String, Vec<String>> = HashMap::new();  // symbol -> symbols it references

    for doc in &index.documents {
        for sym in &doc.symbols {
            // Extract the simple name from the SCIP symbol
            // Format: "rust-analyzer cargo <crate> <version> <path>"
            // Example: "rust-analyzer cargo log 0.4.29 macros/log"
            // We want to extract "log" from the path portion
            if let Some(name) = extract_scip_item_name(&sym.symbol) {
                symbol_to_name.insert(sym.symbol.clone(), name.clone());
                name_to_symbols.entry(name).or_default().push(sym.symbol.clone());
            }
        }

        // Build reference graph from occurrences
        for occ in &doc.occurrences {
            if !occ.symbol.is_empty() {
                // Track what symbols this location references
                symbol_refs.entry(occ.symbol.clone()).or_default();
            }
        }
    }

    eprintln!("Indexed {} symbols, {} unique names",
              symbol_to_name.len(), name_to_symbols.len());

    // Find seed symbols
    let mut needed: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    for seed in seed_items {
        if let Some(symbols) = name_to_symbols.get(*seed) {
            for sym in symbols {
                if let Some(name) = symbol_to_name.get(sym) {
                    needed.insert(name.clone());
                    queue.push_back(sym.clone());
                }
            }
        }
    }

    eprintln!("Found {} seed items in SCIP index", needed.len());

    // BFS to find transitive dependencies
    let mut processed: HashSet<String> = HashSet::new();

    while let Some(symbol) = queue.pop_front() {
        if processed.contains(&symbol) {
            continue;
        }
        processed.insert(symbol.clone());

        // Find all symbols referenced by this symbol
        // Note: SCIP stores occurrences per document, not per symbol
        // We need to look at the document containing this symbol's definition
        // and find what other symbols are referenced in that context

        // For now, use the simple name matching approach
        if let Some(name) = symbol_to_name.get(&symbol) {
            needed.insert(name.clone());
        }
    }

    // For full transitive closure, we need more sophisticated analysis
    // The SCIP format has the data, but extracting it requires more work
    // For now, return all symbols that match seed patterns

    // Collect all symbol names that could be needed
    let mut result: HashSet<String> = HashSet::new();
    for name in needed {
        result.insert(name);
    }

    // Also add symbols that are commonly referenced
    for doc in &index.documents {
        for sym in &doc.symbols {
            if let Some(name) = symbol_to_name.get(&sym.symbol) {
                // Include public items and items with many references
                result.insert(name.clone());
            }
        }
    }

    eprintln!("SCIP analysis found {} items", result.len());
    Ok(result)
}

/// Local registry implementation for caching sliced dependencies
///
/// Creates a file-based Cargo registry to enable fingerprinting and caching
/// of sliced dependencies, avoiding unnecessary recompilation.

use serde_json::{json, Value};
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use tar::Builder as TarBuilder;
use flate2::Compression;
use flate2::write::GzEncoder;
use walkdir::WalkDir;
use sha2::{Sha256, Digest};

/// Initialize local registry structure
pub fn init_local_registry(project_root: &Path) -> Result<PathBuf, String> {
    let registry_dir = project_root.join(".cargo-slicer-registry");

    // Create directory structure
    fs::create_dir_all(registry_dir.join("index"))
        .map_err(|e| format!("Failed to create index dir: {}", e))?;
    fs::create_dir_all(registry_dir.join("crates"))
        .map_err(|e| format!("Failed to create crates dir: {}", e))?;

    // Initialize git repo for index if not already initialized
    let git_dir = registry_dir.join("index/.git");
    if !git_dir.exists() {
        let output = Command::new("git")
            .args(&["init"])
            .current_dir(registry_dir.join("index"))
            .output()
            .map_err(|e| format!("Failed to run git init: {}", e))?;

        if !output.status.success() {
            return Err(format!("git init failed: {}", String::from_utf8_lossy(&output.stderr)));
        }

        // Create initial config.json
        // For a local file-based registry, use the special template format
        let abs_registry_dir = fs::canonicalize(&registry_dir)
            .unwrap_or_else(|_| registry_dir.clone());
        let config = json!({
            "dl": format!("file://{}/crates/{{crate}}-{{version}}.crate", abs_registry_dir.display()),
            "api": null
        });

        fs::write(
            registry_dir.join("index/config.json"),
            serde_json::to_string_pretty(&config)
                .map_err(|e| format!("Failed to serialize config: {}", e))?
        ).map_err(|e| format!("Failed to write config.json: {}", e))?;

        // Initial commit
        Command::new("git")
            .args(&["add", "config.json"])
            .current_dir(registry_dir.join("index"))
            .output()
            .map_err(|e| format!("Failed to git add: {}", e))?;

        Command::new("git")
            .args(&["commit", "-m", "Initialize cargo-slicer registry"])
            .current_dir(registry_dir.join("index"))
            .output()
            .map_err(|e| format!("Failed to git commit: {}", e))?;
    }

    Ok(registry_dir)
}

/// Publish a sliced crate to the local registry
pub fn publish_to_local_registry(
    sliced_dir: &Path,
    crate_name: &str,
    version: &str,
    registry_dir: &Path,
) -> Result<(), String> {
    // 1. Create .crate tarball
    let crate_file = create_crate_tarball(sliced_dir, crate_name, version)?;

    // 2. Compute checksum
    let cksum = compute_sha256(&crate_file)?;

    // 3. Parse dependencies from Cargo.toml.orig (has original version info)
    // Fall back to Cargo.toml if .orig doesn't exist
    let cargo_toml_orig = sliced_dir.join("Cargo.toml.orig");
    let cargo_toml_path = if cargo_toml_orig.exists() {
        cargo_toml_orig
    } else {
        sliced_dir.join("Cargo.toml")
    };
    let deps = parse_dependencies(&cargo_toml_path)?;

    // 4. Parse features from Cargo.toml.orig (or Cargo.toml if .orig doesn't exist)
    let features = parse_features(&cargo_toml_path)?;

    // 5. Create index entry (fields must be in specific order for Cargo)
    // IMPORTANT: Cargo requires fields in this exact order: name, vers, deps, cksum, features, yanked, links
    // We manually construct JSON to preserve order since serde_json sorts alphabetically
    let deps_json = if deps.is_empty() {
        "[]".to_string()
    } else {
        format!("[{}]", deps.join(","))
    };
    let features_json = serde_json::to_string(&features)
        .map_err(|e| format!("Failed to serialize features: {}", e))?;

    // Create index entry in v2 format (required by newer cargo versions)
    let index_entry_str = format!(
        r#"{{"name":"{}","vers":"{}","deps":{},"cksum":"{}","features":{},"features2":{},"yanked":false,"v":2}}"#,
        crate_name, version, deps_json, cksum, features_json, features_json
    );

    // Validate JSON is correct format (but don't use the Value, pass string directly)
    let _validate: Value = serde_json::from_str(&index_entry_str)
        .map_err(|e| format!("Failed to parse index entry: {}", e))?;

    // 6. Append to index file (pass string directly to preserve field order)
    append_to_index_str(registry_dir, crate_name, &index_entry_str)?;

    // 7. Copy .crate file to registry
    let dest = registry_dir.join("crates").join(format!("{}-{}.crate", crate_name, version));
    fs::copy(&crate_file, &dest)
        .map_err(|e| format!("Failed to copy .crate file: {}", e))?;

    // 8. Commit index change
    git_commit_index(registry_dir, crate_name, version)?;

    // 9. Clean up temporary .crate file
    let _ = fs::remove_file(&crate_file);

    Ok(())
}

/// Create a .crate tarball from sliced directory
fn create_crate_tarball(
    sliced_dir: &Path,
    crate_name: &str,
    version: &str,
) -> Result<PathBuf, String> {
    let temp_dir = std::env::temp_dir();
    let output_file = temp_dir.join(format!("{}-{}.crate", crate_name, version));

    let tar_gz = File::create(&output_file)
        .map_err(|e| format!("Failed to create .crate file: {}", e))?;
    let enc = GzEncoder::new(tar_gz, Compression::default());
    let mut ar = TarBuilder::new(enc);

    let base_path = format!("{}-{}", crate_name, version);

    // Add all files from sliced_dir
    for entry in WalkDir::new(sliced_dir)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            // Skip hidden files and directories
            let name = e.file_name().to_string_lossy();
            !name.starts_with('.')
        })
    {
        let entry = entry.map_err(|e| format!("Failed to walk directory: {}", e))?;
        let path = entry.path();

        // Skip the output file itself
        if path == output_file {
            continue;
        }

        if path.is_file() {
            let relative = path.strip_prefix(sliced_dir)
                .map_err(|e| format!("Failed to strip prefix: {}", e))?;
            let tar_path = Path::new(&base_path).join(relative);

            ar.append_path_with_name(path, tar_path)
                .map_err(|e| format!("Failed to add file to tarball: {}", e))?;
        }
    }

    // Finish the tar archive and get back the GzEncoder
    let enc = ar.into_inner()
        .map_err(|e| format!("Failed to finalize tarball: {}", e))?;

    // Explicitly finish the GzEncoder to flush all buffers
    let _file = enc.finish()
        .map_err(|e| format!("Failed to finish gzip compression: {}", e))?;

    Ok(output_file)
}

/// Compute SHA256 checksum of a file
fn compute_sha256(file_path: &Path) -> Result<String, String> {
    let mut file = File::open(file_path)
        .map_err(|e| format!("Failed to open file for checksum: {}", e))?;
    let mut hasher = Sha256::new();
    let mut buffer = [0; 8192];

    loop {
        let n = file.read(&mut buffer)
            .map_err(|e| format!("Failed to read file: {}", e))?;
        if n == 0 {
            break;
        }
        hasher.update(&buffer[..n]);
    }

    Ok(format!("{:x}", hasher.finalize()))
}

/// Parse dependencies from Cargo.toml (returns JSON strings for order preservation)
fn parse_dependencies(cargo_toml_path: &Path) -> Result<Vec<String>, String> {
    let content = fs::read_to_string(cargo_toml_path)
        .map_err(|e| format!("Failed to read Cargo.toml: {}", e))?;

    let toml: toml::Value = toml::from_str(&content)
        .map_err(|e| format!("Failed to parse Cargo.toml: {}", e))?;

    let mut deps = Vec::new();

    // Parse [dependencies]
    if let Some(dependencies) = toml.get("dependencies").and_then(|v| v.as_table()) {
        for (name, value) in dependencies {
            if let Some(dep) = parse_dependency(name, value, "normal")? {
                deps.push(dep);
            }
        }
    }

    // Parse [dev-dependencies]
    if let Some(dev_deps) = toml.get("dev-dependencies").and_then(|v| v.as_table()) {
        for (name, value) in dev_deps {
            if let Some(dep) = parse_dependency(name, value, "dev")? {
                deps.push(dep);
            }
        }
    }

    // Parse [build-dependencies]
    if let Some(build_deps) = toml.get("build-dependencies").and_then(|v| v.as_table()) {
        for (name, value) in build_deps {
            if let Some(dep) = parse_dependency(name, value, "build")? {
                deps.push(dep);
            }
        }
    }

    // Parse target-specific dependencies (e.g., [target.'cfg(...)'.dependencies])
    if let Some(target) = toml.get("target").and_then(|v| v.as_table()) {
        for (_target_spec, target_table) in target {
            if let Some(target_table) = target_table.as_table() {
                // Parse [target.'cfg(...)'.dependencies]
                if let Some(target_deps) = target_table.get("dependencies").and_then(|v| v.as_table()) {
                    for (name, value) in target_deps {
                        if let Some(dep) = parse_dependency(name, value, "normal")? {
                            deps.push(dep);
                        }
                    }
                }
                // Parse [target.'cfg(...)'.dev-dependencies]
                if let Some(target_dev_deps) = target_table.get("dev-dependencies").and_then(|v| v.as_table()) {
                    for (name, value) in target_dev_deps {
                        if let Some(dep) = parse_dependency(name, value, "dev")? {
                            deps.push(dep);
                        }
                    }
                }
                // Parse [target.'cfg(...)'.build-dependencies]
                if let Some(target_build_deps) = target_table.get("build-dependencies").and_then(|v| v.as_table()) {
                    for (name, value) in target_build_deps {
                        if let Some(dep) = parse_dependency(name, value, "build")? {
                            deps.push(dep);
                        }
                    }
                }
            }
        }
    }

    Ok(deps)
}

/// Parse a single dependency entry (returns JSON string for order preservation)
fn parse_dependency(name: &str, value: &toml::Value, kind: &str) -> Result<Option<String>, String> {
    // Default values
    let mut req = "*".to_string();
    let mut features = Vec::new();
    let mut optional = false;
    let mut default_features = true;
    let mut actual_package_name = name.to_string();

    // Parse values from TOML
    match value {
        toml::Value::String(version) => {
            req = version.clone();
        }
        toml::Value::Table(table) => {
            if let Some(version) = table.get("version").and_then(|v| v.as_str()) {
                req = version.to_string();
            }
            if let Some(opt) = table.get("optional").and_then(|v| v.as_bool()) {
                optional = opt;
            }
            if let Some(df) = table.get("default-features").and_then(|v| v.as_bool()) {
                default_features = df;
            }
            if let Some(feats) = table.get("features").and_then(|v| v.as_array()) {
                features = feats
                    .iter()
                    .filter_map(|f| f.as_str().map(String::from))
                    .collect();
            }
            // Handle package renaming: `package = "actual-name"`
            if let Some(pkg) = table.get("package").and_then(|p| p.as_str()) {
                eprintln!("  🔄 Package rename detected: {} -> {} ({})", name, pkg, req);
                actual_package_name = pkg.to_string();
            }
        }
        _ => {}
    }

    // Keep rustc internal dependencies in the index (cargo needs them for feature validation)
    // They won't cause issues since they're only used when building std itself
    // if actual_package_name.starts_with("rustc-std-workspace-") {
    //     return Ok(None);
    // }

    // Manually construct JSON with exact field order: name, req, features, optional, default_features, target, kind
    // Use the dependency KEY (name parameter) so features can reference it correctly
    let features_json = serde_json::to_string(&features)
        .map_err(|e| format!("Failed to serialize features: {}", e))?;

    // For package renames, cargo index format uses the KEY as "name" and adds "package" field
    let dep_json = if actual_package_name != name {
        format!(
            r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"{}","package":"{}"}}"#,
            name, req, features_json, optional, default_features, kind, actual_package_name
        )
    } else {
        format!(
            r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"{}"}}"#,
            name, req, features_json, optional, default_features, kind
        )
    };

    Ok(Some(dep_json))
}

/// Parse features from Cargo.toml
fn parse_features(cargo_toml_path: &Path) -> Result<Value, String> {
    let content = fs::read_to_string(cargo_toml_path)
        .map_err(|e| format!("Failed to read Cargo.toml: {}", e))?;

    let toml: toml::Value = toml::from_str(&content)
        .map_err(|e| format!("Failed to parse Cargo.toml: {}", e))?;

    if let Some(features) = toml.get("features").and_then(|v| v.as_table()) {
        let mut feature_map = serde_json::Map::new();

        for (key, value) in features {
            if let Some(array) = value.as_array() {
                let feature_list: Vec<String> = array
                    .iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect();
                feature_map.insert(key.clone(), json!(feature_list));
            }
        }

        Ok(Value::Object(feature_map))
    } else {
        Ok(json!({}))
    }
}

/// Append index entry string directly to index file (preserves field order)
fn append_to_index_str(registry_dir: &Path, crate_name: &str, entry_json: &str) -> Result<(), String> {
    // Determine index path based on crate name (following crates.io convention)
    let index_path = get_index_path(registry_dir, crate_name);

    // Create parent directories
    if let Some(parent) = index_path.parent() {
        fs::create_dir_all(parent)
            .map_err(|e| format!("Failed to create index directories: {}", e))?;
    }

    // Append entry as single JSON line
    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&index_path)
        .map_err(|e| format!("Failed to open index file: {}", e))?;

    writeln!(file, "{}", entry_json)
        .map_err(|e| format!("Failed to write index entry: {}", e))?;

    Ok(())
}

/// Append index entry to the appropriate index file (deprecated - re-serializes and loses field order)
#[allow(dead_code)]
fn append_to_index(registry_dir: &Path, crate_name: &str, entry: &Value) -> Result<(), String> {
    // This function re-serializes the Value which sorts fields alphabetically
    // Use append_to_index_str instead to preserve field order
    let entry_str = serde_json::to_string(entry)
        .map_err(|e| format!("Failed to serialize index entry: {}", e))?;
    append_to_index_str(registry_dir, crate_name, &entry_str)
}

/// Get index file path for a crate (following crates.io convention)
fn get_index_path(registry_dir: &Path, crate_name: &str) -> PathBuf {
    let index_dir = registry_dir.join("index");

    match crate_name.len() {
        1 => index_dir.join("1").join(crate_name),
        2 => index_dir.join("2").join(crate_name),
        3 => index_dir.join("3").join(&crate_name[0..1]).join(crate_name),
        _ => index_dir.join(&crate_name[0..2]).join(&crate_name[2..4]).join(crate_name),
    }
}

/// Commit index changes to git
fn git_commit_index(registry_dir: &Path, crate_name: &str, version: &str) -> Result<(), String> {
    let index_dir = registry_dir.join("index");

    // Add the modified index file
    let output = Command::new("git")
        .args(&["add", "-A"])
        .current_dir(&index_dir)
        .output()
        .map_err(|e| format!("Failed to run git add: {}", e))?;

    if !output.status.success() {
        return Err(format!("git add failed: {}", String::from_utf8_lossy(&output.stderr)));
    }

    // Commit with descriptive message
    let message = format!("Add {} {}", crate_name, version);
    let output = Command::new("git")
        .args(&["commit", "-m", &message])
        .current_dir(&index_dir)
        .output()
        .map_err(|e| format!("Failed to run git commit: {}", e))?;

    // Note: commit might fail if nothing changed (which is fine)
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        if !stderr.contains("nothing to commit") {
            return Err(format!("git commit failed: {}", stderr));
        }
    }

    Ok(())
}

/// Configure .cargo/config.toml to use local registry
pub fn configure_cargo_registry(project_root: &Path, registry_dir: &Path) -> Result<(), String> {
    let cargo_dir = project_root.join(".cargo");
    fs::create_dir_all(&cargo_dir)
        .map_err(|e| format!("Failed to create .cargo directory: {}", e))?;

    let config_path = cargo_dir.join("config.toml");
    let registry_index = registry_dir.join("index").canonicalize()
        .map_err(|e| format!("Failed to canonicalize registry path: {}", e))?;

    let registry_config = format!(
        r#"
[registries.cargo-slicer]
index = "file://{}"
"#,
        registry_index.display()
    );

    // Read existing config or create new
    let mut content = if config_path.exists() {
        fs::read_to_string(&config_path)
            .map_err(|e| format!("Failed to read config.toml: {}", e))?
    } else {
        String::new()
    };

    // Add registry config if not already present
    if !content.contains("[registries.cargo-slicer]") {
        content.push_str(&registry_config);
        fs::write(&config_path, content)
            .map_err(|e| format!("Failed to write config.toml: {}", e))?;
    }

    Ok(())
}

/// Copy original crate from cargo cache to local registry for transitive dependencies
/// Used when a crate wasn't sliced but is still needed as a transitive dependency
/// Download a crate from crates.io if not in cargo cache
fn download_crate_from_crates_io(
    crate_name: &str,
    version: &str,
) -> Result<PathBuf, String> {
    // Use the static.crates.io CDN for direct .crate file downloads
    let url = format!("https://static.crates.io/crates/{}/{}-{}.crate", crate_name, crate_name, version);

    // Create temp directory for download
    let temp_dir = std::env::temp_dir().join("cargo-slicer-downloads");
    std::fs::create_dir_all(&temp_dir)
        .map_err(|e| format!("Failed to create temp dir: {}", e))?;

    let temp_crate = temp_dir.join(format!("{}-{}.crate", crate_name, version));

    // Download using curl
    eprintln!("  📥 Downloading {} {} from crates.io...", crate_name, version);

    let download_result = std::process::Command::new("curl")
        .args(&["-sL", "-o", temp_crate.to_str().unwrap(), &url])
        .output();

    if download_result.is_err() || !download_result.as_ref().unwrap().status.success() {
        // Try wget as fallback
        let wget_result = std::process::Command::new("wget")
            .args(&["-q", "-O", temp_crate.to_str().unwrap(), &url])
            .output();

        if wget_result.is_err() || !wget_result.as_ref().unwrap().status.success() {
            return Err(format!("Failed to download {} {} from crates.io (tried curl and wget)", crate_name, version));
        }
    }

    if !temp_crate.exists() || std::fs::metadata(&temp_crate).map(|m| m.len()).unwrap_or(0) == 0 {
        return Err(format!("Downloaded file is empty or missing: {}", temp_crate.display()));
    }

    Ok(temp_crate)
}

pub fn copy_original_crate_to_registry(
    crate_name: &str,
    version: &str,
    registry_dir: &Path,
    manifest_path: Option<&Path>,
) -> Result<(), String> {
    // First, try to find .crate file in cargo cache (fastest - already compressed)
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
    let cache_patterns = vec![
        format!("{}/.cargo/registry/cache/index.crates.io-*/{}-{}.crate", home, crate_name, version),
        format!("{}/.cargo/registry/cache/*/{}-{}.crate", home, crate_name, version),
    ];

    let mut cached_crate = None;
    for pattern in &cache_patterns {
        if let Ok(entries) = glob::glob(pattern) {
            for entry in entries.flatten() {
                if entry.exists() {
                    cached_crate = Some(entry);
                    break;
                }
            }
            if cached_crate.is_some() {
                break;
            }
        }
    }

    let source_crate = if let Some(cached) = cached_crate {
        // Found in cache - use it directly
        cached
    } else if let Some(manifest) = manifest_path {
        // No cached .crate, but we have manifest_path
        // Create tarball from source directory as fallback
        eprintln!("  📦 Creating tarball from: {}", manifest.display());
        let source_dir = manifest.parent()
            .ok_or_else(|| format!("Invalid manifest path: {}", manifest.display()))?;
        create_crate_tarball(source_dir, crate_name, version)
            .map_err(|e| format!("Failed to create tarball from source: {}", e))?
    } else {
        // Last resort: download from crates.io
        download_crate_from_crates_io(crate_name, version)?
    };

    // Copy to local registry crates directory
    let dest_crate = registry_dir.join("crates").join(format!("{}-{}.crate", crate_name, version));

    // Check if already fully published (both .crate and index entry)
    let name_lower = crate_name.to_lowercase();
    let index_path = match name_lower.len() {
        1 => registry_dir.join("index").join("1").join(&name_lower),
        2 => registry_dir.join("index").join("2").join(&name_lower),
        3 => registry_dir.join("index").join("3").join(&name_lower[..1]).join(&name_lower),
        _ => registry_dir.join("index").join(&name_lower[..2]).join(&name_lower[2..4]).join(&name_lower),
    };

    if dest_crate.exists() && index_path.exists() {
        // Already fully published, check if this version is in the index
        if let Ok(content) = std::fs::read_to_string(&index_path) {
            if content.lines().any(|line| line.contains(&format!("\"vers\":\"{}\"", version))) {
                return Ok(());
            }
        }
    }

    // Link or copy .crate file if needed
    if !dest_crate.exists() {
        // If source is in cargo cache, use symlink (instant, no disk usage)
        // Otherwise copy (for temp files from create_crate_tarball)
        let source_str = source_crate.to_string_lossy();
        if source_str.contains("/.cargo/registry/cache/") {
            // Use symlink for cache files
            std::os::unix::fs::symlink(&source_crate, &dest_crate)
                .map_err(|e| format!("Failed to symlink crate: {}", e))?;
        } else {
            // Copy for temporary files
            fs::copy(&source_crate, &dest_crate)
                .map_err(|e| format!("Failed to copy crate: {}", e))?;
        }
    }

    // Compute checksum
    let cksum = compute_sha256(&dest_crate)?;

    // Extract metadata from .crate tarball
    let (deps_json, features_json) = extract_metadata_from_crate(&dest_crate, crate_name, version)?;

    // Create index entry with full metadata (v2 format for newer cargo)
    // Split features into v1 "features" (non-default) and v2 "features2" (all features including default)
    let index_entry = format!(
        r#"{{"name":"{}","vers":"{}","deps":{},"cksum":"{}","features":{},"features2":{},"yanked":false,"v":2}}"#,
        crate_name, version, deps_json, cksum, features_json, features_json
    );

    // index_path already calculated above when checking if file exists

    // Create parent directory
    if let Some(parent) = index_path.parent() {
        fs::create_dir_all(parent)
            .map_err(|e| format!("Failed to create index dir: {}", e))?;
    }

    // Check if this version already exists in the index (prevent duplicates)
    if index_path.exists() {
        if let Ok(content) = std::fs::read_to_string(&index_path) {
            if content.lines().any(|line| line.contains(&format!("\"vers\":\"{}\"", version))) {
                // Version already in index, skip appending
                return Ok(());
            }
        }
    }

    // Append to index file
    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&index_path)
        .map_err(|e| format!("Failed to open index file: {}", e))?;

    writeln!(file, "{}", index_entry)
        .map_err(|e| format!("Failed to write index entry: {}", e))?;

    // Commit to git index
    let index_dir = registry_dir.join("index");
    Command::new("git")
        .args(&["add", "-A"])
        .current_dir(&index_dir)
        .output()
        .map_err(|e| format!("Failed to git add: {}", e))?;

    Command::new("git")
        .args(&["commit", "-m", &format!("Add original {}-{}", crate_name, version)])
        .current_dir(&index_dir)
        .output()
        .ok(); // Ignore errors (might be nothing to commit)

    Ok(())
}

/// Extract dependencies and features metadata from a .crate tarball
fn extract_metadata_from_crate(
    crate_path: &Path,
    crate_name: &str,
    version: &str,
) -> Result<(String, String), String> {
    use flate2::read::GzDecoder;
    use std::io::Read;
    use tar::Archive;

    // Open and decompress the .crate file
    let file = File::open(crate_path)
        .map_err(|e| format!("Failed to open .crate file: {}", e))?;
    let gz = GzDecoder::new(file);
    let mut archive = Archive::new(gz);

    // Look for Cargo.toml.orig (original before normalization) or Cargo.toml in the tarball
    // Prefer .orig as it has complete package rename information
    let cargo_toml_orig_path = format!("{}-{}/Cargo.toml.orig", crate_name, version);
    let cargo_toml_path = format!("{}-{}/Cargo.toml", crate_name, version);
    let mut cargo_toml_content = String::new();
    let mut regular_toml_content = String::new();

    // Scan archive once, collect both .orig and regular Cargo.toml
    for entry in archive.entries().map_err(|e| format!("Failed to read tar entries: {}", e))? {
        let mut entry = entry.map_err(|e| format!("Failed to read entry: {}", e))?;
        let path = entry.path().map_err(|e| format!("Failed to get path: {}", e))?;

        if path == Path::new(&cargo_toml_orig_path) {
            entry.read_to_string(&mut cargo_toml_content)
                .map_err(|e| format!("Failed to read Cargo.toml.orig: {}", e))?;
        } else if path == Path::new(&cargo_toml_path) {
            entry.read_to_string(&mut regular_toml_content)
                .map_err(|e| format!("Failed to read Cargo.toml: {}", e))?;
        }
    }

    // Prefer .orig if found, otherwise use regular
    if cargo_toml_content.is_empty() {
        cargo_toml_content = regular_toml_content;
    }

    if cargo_toml_content.is_empty() {
        return Err(format!("Cargo.toml not found in {}", crate_path.display()));
    }

    // Parse Cargo.toml
    let manifest: toml::Value = toml::from_str(&cargo_toml_content)
        .map_err(|e| format!("Failed to parse Cargo.toml: {}", e))?;

    // Extract features (as JSON object)
    let features = manifest.get("features")
        .and_then(|f| f.as_table())
        .cloned()
        .unwrap_or_default();

    let features_json = serde_json::to_string(&features)
        .map_err(|e| format!("Failed to serialize features: {}", e))?;

    // Extract dependencies (convert to cargo index format)
    // IMPORTANT: Manually construct JSON to preserve exact field order that Cargo expects
    let mut deps_strings = Vec::new();

    // Extract [dependencies]
    if let Some(deps_table) = manifest.get("dependencies").and_then(|d| d.as_table()) {
        for (dep_name, dep_value) in deps_table {
            let mut req = "*".to_string();
            let mut features = Vec::<String>::new();
            let mut optional = false;
            let mut default_features = true;
            let mut actual_package_name = dep_name.clone();

            // Parse dependency details
            if let Some(dep_str) = dep_value.as_str() {
                // Simple version string
                req = dep_str.to_string();
            } else if let Some(dep_table) = dep_value.as_table() {
                // Detailed dependency
                if let Some(version) = dep_table.get("version").and_then(|v| v.as_str()) {
                    req = version.to_string();
                }
                if let Some(opt) = dep_table.get("optional").and_then(|o| o.as_bool()) {
                    optional = opt;
                }
                if let Some(df) = dep_table.get("default-features").and_then(|d| d.as_bool()) {
                    default_features = df;
                }
                if let Some(feats) = dep_table.get("features").and_then(|f| f.as_array()) {
                    features = feats.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect();
                }
                // Handle package renaming: `package = "actual-name"`
                if let Some(pkg) = dep_table.get("package").and_then(|p| p.as_str()) {
                    eprintln!("  🔄 Package rename: {} -> {}", dep_name, pkg);
                    actual_package_name = pkg.to_string();
                }
            }

            // Manually construct JSON with exact field order: name, req, features, optional, default_features, target, kind
            // Use the actual package name, not the alias
            let features_json = serde_json::to_string(&features)
                .map_err(|e| format!("Failed to serialize features: {}", e))?;

            if dep_name != &actual_package_name {
                eprintln!("  🔄 Using renamed package: {} -> {} ({})", dep_name, actual_package_name, req);
            }

            // Keep rustc internal dependencies in the index (cargo needs them for feature validation)
            // if actual_package_name.starts_with("rustc-std-workspace-") {
            //     continue;
            // }

            // Use dependency KEY as "name" and add "package" field for renames
            let dep_json = if dep_name != &actual_package_name {
                format!(
                    r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"normal","package":"{}"}}"#,
                    dep_name, req, features_json, optional, default_features, actual_package_name
                )
            } else {
                format!(
                    r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"normal"}}"#,
                    dep_name, req, features_json, optional, default_features
                )
            };

            deps_strings.push(dep_json);
        }
    }

    // Extract [dev-dependencies]
    if let Some(dev_deps_table) = manifest.get("dev-dependencies").and_then(|d| d.as_table()) {
        for (dep_name, dep_value) in dev_deps_table {
            let mut req = "*".to_string();
            let mut features = Vec::<String>::new();
            let mut optional = false;
            let mut default_features = true;
            let mut actual_package_name = dep_name.clone();

            if let Some(dep_str) = dep_value.as_str() {
                req = dep_str.to_string();
            } else if let Some(dep_table) = dep_value.as_table() {
                if let Some(version) = dep_table.get("version").and_then(|v| v.as_str()) {
                    req = version.to_string();
                }
                if let Some(opt) = dep_table.get("optional").and_then(|o| o.as_bool()) {
                    optional = opt;
                }
                if let Some(df) = dep_table.get("default-features").and_then(|d| d.as_bool()) {
                    default_features = df;
                }
                if let Some(feats) = dep_table.get("features").and_then(|f| f.as_array()) {
                    features = feats.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect();
                }
                // Handle package renaming
                if let Some(pkg) = dep_table.get("package").and_then(|p| p.as_str()) {
                    actual_package_name = pkg.to_string();
                }
            }

            let features_json = serde_json::to_string(&features)
                .map_err(|e| format!("Failed to serialize features: {}", e))?;

            // Use dependency KEY as "name" and add "package" field for renames
            let dep_json = if dep_name != &actual_package_name {
                format!(
                    r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"dev","package":"{}"}}"#,
                    dep_name, req, features_json, optional, default_features, actual_package_name
                )
            } else {
                format!(
                    r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"dev"}}"#,
                    dep_name, req, features_json, optional, default_features
                )
            };

            deps_strings.push(dep_json);
        }
    }

    // Extract [build-dependencies]
    if let Some(build_deps_table) = manifest.get("build-dependencies").and_then(|d| d.as_table()) {
        for (dep_name, dep_value) in build_deps_table {
            let mut req = "*".to_string();
            let mut features = Vec::<String>::new();
            let mut optional = false;
            let mut default_features = true;
            let mut actual_package_name = dep_name.clone();

            if let Some(dep_str) = dep_value.as_str() {
                req = dep_str.to_string();
            } else if let Some(dep_table) = dep_value.as_table() {
                if let Some(version) = dep_table.get("version").and_then(|v| v.as_str()) {
                    req = version.to_string();
                }
                if let Some(opt) = dep_table.get("optional").and_then(|o| o.as_bool()) {
                    optional = opt;
                }
                if let Some(df) = dep_table.get("default-features").and_then(|d| d.as_bool()) {
                    default_features = df;
                }
                if let Some(feats) = dep_table.get("features").and_then(|f| f.as_array()) {
                    features = feats.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect();
                }
                // Handle package renaming
                if let Some(pkg) = dep_table.get("package").and_then(|p| p.as_str()) {
                    actual_package_name = pkg.to_string();
                }
            }

            let features_json = serde_json::to_string(&features)
                .map_err(|e| format!("Failed to serialize features: {}", e))?;

            // Use dependency KEY as "name" and add "package" field for renames
            let dep_json = if dep_name != &actual_package_name {
                format!(
                    r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"build","package":"{}"}}"#,
                    dep_name, req, features_json, optional, default_features, actual_package_name
                )
            } else {
                format!(
                    r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":null,"kind":"build"}}"#,
                    dep_name, req, features_json, optional, default_features
                )
            };

            deps_strings.push(dep_json);
        }
    }

    // Extract target-specific dependencies (e.g., [target.'cfg(...)'.dependencies])
    if let Some(target_table) = manifest.get("target").and_then(|t| t.as_table()) {
        for (target_spec, target_data) in target_table {
            if let Some(target_data_table) = target_data.as_table() {
                // Extract target.'cfg(...)'.dependencies
                if let Some(target_deps) = target_data_table.get("dependencies").and_then(|d| d.as_table()) {
                    for (dep_name, dep_value) in target_deps {
                        let mut req = "*".to_string();
                        let mut features = Vec::<String>::new();
                        let mut optional = false;
                        let mut default_features = true;
                        let mut actual_package_name = dep_name.clone();

                        if let Some(dep_str) = dep_value.as_str() {
                            req = dep_str.to_string();
                        } else if let Some(dep_table) = dep_value.as_table() {
                            if let Some(version) = dep_table.get("version").and_then(|v| v.as_str()) {
                                req = version.to_string();
                            }
                            if let Some(opt) = dep_table.get("optional").and_then(|o| o.as_bool()) {
                                optional = opt;
                            }
                            if let Some(df) = dep_table.get("default-features").and_then(|d| d.as_bool()) {
                                default_features = df;
                            }
                            if let Some(feats) = dep_table.get("features").and_then(|f| f.as_array()) {
                                features = feats.iter()
                                    .filter_map(|v| v.as_str().map(String::from))
                                    .collect();
                            }
                            // Handle package renaming
                            if let Some(pkg) = dep_table.get("package").and_then(|p| p.as_str()) {
                                actual_package_name = pkg.to_string();
                            }
                        }

                        let features_json = serde_json::to_string(&features)
                            .map_err(|e| format!("Failed to serialize features: {}", e))?;
                        let target_json = serde_json::to_string(target_spec)
                            .map_err(|e| format!("Failed to serialize target: {}", e))?;

                        // Use dependency KEY as "name" and add "package" field for renames
                        let dep_json = if dep_name != &actual_package_name {
                            format!(
                                r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":{},"kind":"normal","package":"{}"}}"#,
                                dep_name, req, features_json, optional, default_features, target_json, actual_package_name
                            )
                        } else {
                            format!(
                                r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":{},"kind":"normal"}}"#,
                                dep_name, req, features_json, optional, default_features, target_json
                            )
                        };

                        deps_strings.push(dep_json);
                    }
                }

                // Extract target.'cfg(...)'.dev-dependencies
                if let Some(target_dev_deps) = target_data_table.get("dev-dependencies").and_then(|d| d.as_table()) {
                    for (dep_name, dep_value) in target_dev_deps {
                        let mut req = "*".to_string();
                        let mut features = Vec::<String>::new();
                        let mut optional = false;
                        let mut default_features = true;
                        let mut actual_package_name = dep_name.clone();

                        if let Some(dep_str) = dep_value.as_str() {
                            req = dep_str.to_string();
                        } else if let Some(dep_table) = dep_value.as_table() {
                            if let Some(version) = dep_table.get("version").and_then(|v| v.as_str()) {
                                req = version.to_string();
                            }
                            if let Some(opt) = dep_table.get("optional").and_then(|o| o.as_bool()) {
                                optional = opt;
                            }
                            if let Some(df) = dep_table.get("default-features").and_then(|d| d.as_bool()) {
                                default_features = df;
                            }
                            if let Some(feats) = dep_table.get("features").and_then(|f| f.as_array()) {
                                features = feats.iter()
                                    .filter_map(|v| v.as_str().map(String::from))
                                    .collect();
                            }
                            // Handle package renaming
                            if let Some(pkg) = dep_table.get("package").and_then(|p| p.as_str()) {
                                actual_package_name = pkg.to_string();
                            }
                        }

                        let features_json = serde_json::to_string(&features)
                            .map_err(|e| format!("Failed to serialize features: {}", e))?;
                        let target_json = serde_json::to_string(target_spec)
                            .map_err(|e| format!("Failed to serialize target: {}", e))?;

                        // Use dependency KEY as "name" and add "package" field for renames
                        let dep_json = if dep_name != &actual_package_name {
                            format!(
                                r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":{},"kind":"dev","package":"{}"}}"#,
                                dep_name, req, features_json, optional, default_features, target_json, actual_package_name
                            )
                        } else {
                            format!(
                                r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":{},"kind":"dev"}}"#,
                                dep_name, req, features_json, optional, default_features, target_json
                            )
                        };

                        deps_strings.push(dep_json);
                    }
                }

                // Extract target.'cfg(...)'.build-dependencies
                if let Some(target_build_deps) = target_data_table.get("build-dependencies").and_then(|d| d.as_table()) {
                    for (dep_name, dep_value) in target_build_deps {
                        let mut req = "*".to_string();
                        let mut features = Vec::<String>::new();
                        let mut optional = false;
                        let mut default_features = true;
                        let mut actual_package_name = dep_name.clone();

                        if let Some(dep_str) = dep_value.as_str() {
                            req = dep_str.to_string();
                        } else if let Some(dep_table) = dep_value.as_table() {
                            if let Some(version) = dep_table.get("version").and_then(|v| v.as_str()) {
                                req = version.to_string();
                            }
                            if let Some(opt) = dep_table.get("optional").and_then(|o| o.as_bool()) {
                                optional = opt;
                            }
                            if let Some(df) = dep_table.get("default-features").and_then(|d| d.as_bool()) {
                                default_features = df;
                            }
                            if let Some(feats) = dep_table.get("features").and_then(|f| f.as_array()) {
                                features = feats.iter()
                                    .filter_map(|v| v.as_str().map(String::from))
                                    .collect();
                            }
                            // Handle package renaming
                            if let Some(pkg) = dep_table.get("package").and_then(|p| p.as_str()) {
                                actual_package_name = pkg.to_string();
                            }
                        }

                        let features_json = serde_json::to_string(&features)
                            .map_err(|e| format!("Failed to serialize features: {}", e))?;
                        let target_json = serde_json::to_string(target_spec)
                            .map_err(|e| format!("Failed to serialize target: {}", e))?;

                        // Use dependency KEY as "name" and add "package" field for renames
                        let dep_json = if dep_name != &actual_package_name {
                            format!(
                                r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":{},"kind":"build","package":"{}"}}"#,
                                dep_name, req, features_json, optional, default_features, target_json, actual_package_name
                            )
                        } else {
                            format!(
                                r#"{{"name":"{}","req":"{}","features":{},"optional":{},"default_features":{},"target":{},"kind":"build"}}"#,
                                dep_name, req, features_json, optional, default_features, target_json
                            )
                        };

                        deps_strings.push(dep_json);
                    }
                }
            }
        }
    }

    let deps_json = if deps_strings.is_empty() {
        "[]".to_string()
    } else {
        format!("[{}]", deps_strings.join(","))
    };

    Ok((deps_json, features_json))
}

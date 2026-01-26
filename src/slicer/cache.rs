//! Incremental caching for sliced crate outputs
//!
//! Caches sliced crate outputs to avoid re-slicing unchanged crates.
//! Cache key = hash(crate_name + crate_version + used_items + features)

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::slice_all::copy_dir_recursive;
use super::features::SlicerFeatures;

/// Cache directory name
const CACHE_DIR: &str = ".slicer-cache";

/// Cache metadata file
const CACHE_META: &str = "cache.json";

/// Individual crate cache entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrateCacheEntry {
    /// Crate name
    pub crate_name: String,
    /// Crate version
    pub crate_version: String,
    /// Hash of used_items + features
    pub content_hash: String,
    /// Path to cached sliced output (relative to cache dir)
    pub cached_path: PathBuf,
    /// Timestamp when cached
    pub cached_at: u64,
    /// LOC stats from slicing
    pub loc_before: usize,
    pub loc_after: usize,
    pub items_deleted: usize,
}

/// Cache metadata tracking all cached crates
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct SlicerCache {
    /// Map from cache key (crate_name@version) to entry
    pub entries: HashMap<String, CrateCacheEntry>,
    /// Cache format version for compatibility
    pub version: u32,
}

impl SlicerCache {
    /// Current cache format version
    const CURRENT_VERSION: u32 = 1;

    /// Load cache from disk
    pub fn load(project_root: &Path) -> Self {
        let cache_dir = project_root.join(CACHE_DIR);
        let meta_path = cache_dir.join(CACHE_META);

        if !meta_path.exists() {
            return Self::default();
        }

        match fs::read_to_string(&meta_path) {
            Ok(content) => {
                match serde_json::from_str::<SlicerCache>(&content) {
                    Ok(cache) if cache.version == Self::CURRENT_VERSION => cache,
                    Ok(_) => {
                        // Version mismatch, invalidate cache
                        eprintln!("Cache version mismatch, clearing cache");
                        let _ = fs::remove_dir_all(&cache_dir);
                        Self::default()
                    }
                    Err(e) => {
                        eprintln!("Failed to parse cache: {}", e);
                        Self::default()
                    }
                }
            }
            Err(_) => Self::default(),
        }
    }

    /// Save cache to disk
    pub fn save(&self, project_root: &Path) -> Result<(), String> {
        let cache_dir = project_root.join(CACHE_DIR);
        fs::create_dir_all(&cache_dir)
            .map_err(|e| format!("Failed to create cache dir: {}", e))?;

        let meta_path = cache_dir.join(CACHE_META);
        let content = serde_json::to_string_pretty(self)
            .map_err(|e| format!("Failed to serialize cache: {}", e))?;

        fs::write(&meta_path, content)
            .map_err(|e| format!("Failed to write cache: {}", e))?;

        Ok(())
    }

    /// Generate cache key for a crate
    pub fn cache_key(crate_name: &str, crate_version: &str) -> String {
        format!("{}@{}", crate_name, crate_version)
    }

    /// Generate content hash from used_items and features
    pub fn content_hash(used_items: &HashSet<String>, features: &SlicerFeatures) -> String {
        let mut hasher = Sha256::new();

        // Sort used_items for deterministic hashing
        let mut sorted_items: Vec<_> = used_items.iter().collect();
        sorted_items.sort();

        for item in sorted_items {
            hasher.update(item.as_bytes());
            hasher.update(b"\n");
        }

        // Hash relevant feature flags
        hasher.update(b"features:");
        hasher.update(format!("{:?}", features.delete_private_fn).as_bytes());
        hasher.update(format!("{:?}", features.delete_private_const).as_bytes());
        hasher.update(format!("{:?}", features.delete_private_type).as_bytes());
        hasher.update(format!("{:?}", features.delete_private_struct).as_bytes());
        hasher.update(format!("{:?}", features.trust_graph).as_bytes());
        hasher.update(format!("{:?}", features.trial_delete).as_bytes());

        let result = hasher.finalize();
        hex::encode(&result[..8]) // Use first 8 bytes (16 hex chars)
    }

    /// Generate content hash from a slice of path strings (for watch mode fingerprinting)
    pub fn content_hash_from_strings(items: &[&String]) -> String {
        let mut hasher = Sha256::new();

        // Items should already be sorted by caller
        for item in items {
            hasher.update(item.as_bytes());
            hasher.update(b"\n");
        }

        let result = hasher.finalize();
        hex::encode(&result[..8]) // Use first 8 bytes (16 hex chars)
    }

    /// Check if a crate has a valid cache hit
    pub fn get(&self, crate_name: &str, crate_version: &str, content_hash: &str) -> Option<&CrateCacheEntry> {
        let key = Self::cache_key(crate_name, crate_version);
        self.entries.get(&key).filter(|e| e.content_hash == content_hash)
    }

    /// Insert or update a cache entry
    pub fn insert(&mut self, entry: CrateCacheEntry) {
        let key = Self::cache_key(&entry.crate_name, &entry.crate_version);
        self.entries.insert(key, entry);
        self.version = Self::CURRENT_VERSION;
    }

    /// Remove stale entries (crates not in current dependency set)
    pub fn prune(&mut self, current_crates: &HashSet<String>) {
        self.entries.retain(|_, entry| current_crates.contains(&entry.crate_name));
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        CacheStats {
            total_entries: self.entries.len(),
            total_loc_saved: self.entries.values()
                .map(|e| e.loc_before.saturating_sub(e.loc_after))
                .sum(),
        }
    }
}

/// Cache statistics
#[derive(Debug)]
pub struct CacheStats {
    pub total_entries: usize,
    pub total_loc_saved: usize,
}

/// Copy cached sliced output to target directory
pub fn copy_cached_output(
    project_root: &Path,
    entry: &CrateCacheEntry,
    output_dir: &Path,
) -> Result<(), String> {
    let cache_dir = project_root.join(CACHE_DIR);
    let cached_path = cache_dir.join(&entry.cached_path);

    if !cached_path.exists() {
        return Err(format!("Cached output not found: {}", cached_path.display()));
    }

    // Remove existing output if any
    if output_dir.exists() {
        fs::remove_dir_all(output_dir)
            .map_err(|e| format!("Failed to remove existing output: {}", e))?;
    }

    // Copy cached output to target
    copy_dir_recursive(&cached_path, output_dir)
        .map_err(|e| format!("Failed to copy cached output: {}", e))?;

    Ok(())
}

/// Save sliced output to cache
pub fn save_to_cache(
    project_root: &Path,
    crate_name: &str,
    crate_version: &str,
    content_hash: &str,
    output_dir: &Path,
    loc_before: usize,
    loc_after: usize,
    items_deleted: usize,
    cache: &mut SlicerCache,
) -> Result<(), String> {
    let cache_dir = project_root.join(CACHE_DIR);
    let cached_name = format!("{}-{}", crate_name, crate_version);
    let cached_path = PathBuf::from(&cached_name);
    let full_cached_path = cache_dir.join(&cached_path);

    // Remove existing cached version if any
    if full_cached_path.exists() {
        fs::remove_dir_all(&full_cached_path)
            .map_err(|e| format!("Failed to remove old cache: {}", e))?;
    }

    // Copy sliced output to cache
    fs::create_dir_all(&cache_dir)
        .map_err(|e| format!("Failed to create cache dir: {}", e))?;

    copy_dir_recursive(output_dir, &full_cached_path)
        .map_err(|e| format!("Failed to copy to cache: {}", e))?;

    // Create cache entry
    let entry = CrateCacheEntry {
        crate_name: crate_name.to_string(),
        crate_version: crate_version.to_string(),
        content_hash: content_hash.to_string(),
        cached_path,
        cached_at: SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0),
        loc_before,
        loc_after,
        items_deleted,
    };

    cache.insert(entry);
    Ok(())
}

/// Get crate version from Cargo.toml
pub fn get_crate_version(source_dir: &Path) -> Option<String> {
    let cargo_toml = source_dir.join("Cargo.toml");
    if let Ok(content) = fs::read_to_string(&cargo_toml) {
        // Simple version extraction - look for version = "x.y.z"
        for line in content.lines() {
            let line = line.trim();
            if line.starts_with("version") && line.contains('=') {
                if let Some(version) = line.split('=').nth(1) {
                    return Some(version.trim().trim_matches('"').to_string());
                }
            }
        }
    }
    None
}

/// Clean the entire cache
pub fn clean_cache(project_root: &Path) -> Result<(), String> {
    let cache_dir = project_root.join(CACHE_DIR);
    if cache_dir.exists() {
        fs::remove_dir_all(&cache_dir)
            .map_err(|e| format!("Failed to clean cache: {}", e))?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_content_hash_deterministic() {
        let items1: HashSet<String> = ["foo", "bar", "baz"].iter().map(|s| s.to_string()).collect();
        let items2: HashSet<String> = ["baz", "foo", "bar"].iter().map(|s| s.to_string()).collect();
        let features = SlicerFeatures::new();

        let hash1 = SlicerCache::content_hash(&items1, &features);
        let hash2 = SlicerCache::content_hash(&items2, &features);

        assert_eq!(hash1, hash2, "Same items should produce same hash regardless of order");
    }

    #[test]
    fn test_content_hash_different_features() {
        let items: HashSet<String> = ["foo"].iter().map(|s| s.to_string()).collect();
        let features1 = SlicerFeatures::new();
        let mut features2 = SlicerFeatures::new();
        features2.delete_private_fn = true;

        let hash1 = SlicerCache::content_hash(&items, &features1);
        let hash2 = SlicerCache::content_hash(&items, &features2);

        assert_ne!(hash1, hash2, "Different features should produce different hash");
    }

    #[test]
    fn test_cache_key() {
        assert_eq!(SlicerCache::cache_key("regex", "1.10.3"), "regex@1.10.3");
    }
}

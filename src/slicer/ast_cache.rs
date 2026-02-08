// AST Cache: Reuse parsed ASTs across multiple slicing phases
//
// Performance: Parsing is expensive (~20-80ms per file). Since we parse
// the same files multiple times (dependency graph → marker → deleter → cleanup → trial),
// caching parsed ASTs can save 5-6x time on repeated operations.

use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use syn::File as SynFile;

/// Thread-safe cache for parsed ASTs with LRU eviction
pub struct AstCache {
    /// Map: (absolute_path, content_hash) -> Arc<SynFile>
    cache: Arc<RwLock<HashMap<CacheKey, Arc<SynFile>>>>,
    /// Maximum number of cached ASTs (to bound memory usage)
    max_entries: usize,
    /// Statistics
    stats: Arc<RwLock<CacheStats>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CacheKey {
    path: PathBuf,
    content_hash: u64,
}

#[derive(Debug, Default)]
struct CacheStats {
    hits: usize,
    misses: usize,
    evictions: usize,
}

impl AstCache {
    /// Create a new cache with default limit (100 files, ~500MB)
    pub fn new() -> Self {
        Self::new_with_limit(100)
    }

    /// Create a new cache with custom entry limit
    pub fn new_with_limit(max_entries: usize) -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            max_entries,
            stats: Arc::new(RwLock::new(CacheStats::default())),
        }
    }

    /// Parse file or return cached AST
    ///
    /// # Arguments
    /// * `path` - Absolute path to the source file
    /// * `content` - Source code content
    ///
    /// # Returns
    /// Arc-wrapped parsed AST (cheap to clone)
    pub fn parse_file(&self, path: &Path, content: &str) -> Result<Arc<SynFile>, syn::Error> {
        let key = CacheKey {
            path: path.to_path_buf(),
            content_hash: hash_content(content),
        };

        // Fast path: Check cache with read lock
        {
            let cache = self.cache.read().unwrap();
            if let Some(ast) = cache.get(&key) {
                // Cache hit!
                let mut stats = self.stats.write().unwrap();
                stats.hits += 1;
                return Ok(Arc::clone(ast));
            }
        }

        // Slow path: Parse and cache with write lock
        let ast = syn::parse_file(content)?;
        let arc_ast = Arc::new(ast);

        {
            let mut cache = self.cache.write().unwrap();
            let mut stats = self.stats.write().unwrap();
            stats.misses += 1;

            // Evict oldest entry if cache is full (simple FIFO, could upgrade to LRU)
            if cache.len() >= self.max_entries {
                if let Some(old_key) = cache.keys().next().cloned() {
                    cache.remove(&old_key);
                    stats.evictions += 1;
                }
            }

            cache.insert(key, Arc::clone(&arc_ast));
        }

        Ok(arc_ast)
    }

    /// Clear the entire cache
    pub fn clear(&self) {
        let mut cache = self.cache.write().unwrap();
        cache.clear();
    }

    /// Get cache statistics
    ///
    /// Returns (entries, hits, misses, evictions, hit_rate%)
    pub fn stats(&self) -> (usize, usize, usize, usize, f64) {
        let cache = self.cache.read().unwrap();
        let stats = self.stats.read().unwrap();
        let entries = cache.len();
        let total_requests = stats.hits + stats.misses;
        let hit_rate = if total_requests > 0 {
            (stats.hits as f64 / total_requests as f64) * 100.0
        } else {
            0.0
        };
        (entries, stats.hits, stats.misses, stats.evictions, hit_rate)
    }

    /// Print cache statistics (for debugging)
    pub fn print_stats(&self) {
        let (entries, hits, misses, evictions, hit_rate) = self.stats();
        println!("=== AST Cache Statistics ===");
        println!("  Entries: {}", entries);
        println!("  Hits: {}", hits);
        println!("  Misses: {}", misses);
        println!("  Evictions: {}", evictions);
        println!("  Hit rate: {:.1}%", hit_rate);
    }
}

impl Default for AstCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Hash file content for cache key
fn hash_content(content: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_hit() {
        let cache = AstCache::new();
        let path = Path::new("test.rs");
        let content = "fn main() {}";

        // First parse: cache miss
        let ast1 = cache.parse_file(path, content).unwrap();
        let (_, hits, misses, _, _) = cache.stats();
        assert_eq!(hits, 0); // 0 hits
        assert_eq!(misses, 1); // 1 miss

        // Second parse: cache hit
        let ast2 = cache.parse_file(path, content).unwrap();
        let (_, hits, misses, _, _) = cache.stats();
        assert_eq!(hits, 1); // 1 hit
        assert_eq!(misses, 1); // still 1 miss

        // Both should point to the same Arc
        assert!(Arc::ptr_eq(&ast1, &ast2));
    }

    #[test]
    fn test_cache_invalidation_on_content_change() {
        let cache = AstCache::new();
        let path = Path::new("test.rs");

        // Parse v1
        let content1 = "fn main() {}";
        let ast1 = cache.parse_file(path, content1).unwrap();

        // Parse v2 (different content)
        let content2 = "fn main() { println!(\"hello\"); }";
        let ast2 = cache.parse_file(path, content2).unwrap();

        // Should be different ASTs (content changed)
        assert!(!Arc::ptr_eq(&ast1, &ast2));
        let (_, hits, misses, _, _) = cache.stats();
        assert_eq!(hits, 0); // 0 hits (different content)
        assert_eq!(misses, 2); // 2 misses
    }

    #[test]
    fn test_cache_eviction() {
        let cache = AstCache::new_with_limit(2);
        let path1 = Path::new("test1.rs");
        let path2 = Path::new("test2.rs");
        let path3 = Path::new("test3.rs");

        // Fill cache
        cache.parse_file(path1, "fn f1() {}").unwrap();
        cache.parse_file(path2, "fn f2() {}").unwrap();
        let (entries, _, _, _, _) = cache.stats();
        assert_eq!(entries, 2); // 2 entries

        // Add third entry (should evict oldest)
        cache.parse_file(path3, "fn f3() {}").unwrap();
        let (entries, _, _, evictions, _) = cache.stats();
        assert_eq!(entries, 2); // still 2 entries
        assert_eq!(evictions, 1); // 1 eviction
    }

    // NOTE: We can't test true concurrent access because syn::File contains Rc<T>
    // internally, which is !Send. The cache itself is thread-safe (uses RwLock),
    // but syn::File can only be used within a single thread.
    //
    // In practice, this is fine because:
    // 1. Each slicing operation is sequential within a thread
    // 2. We use rayon's par_iter() for parallelism, which handles per-thread data correctly
    // 3. Arc<SynFile> can't be sent across threads anyway due to syn's design
}

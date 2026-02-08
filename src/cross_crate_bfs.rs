//! Cross-crate BFS for workspace-wide virtual slicing.
//!
//! Reads per-crate `.analysis` files, builds a unified call graph across
//! all crates, runs BFS from binary entry points, and writes per-crate
//! `.cache` files with the marked items.
//!
//! This enables library crates to benefit from virtual slicing: instead of
//! seeding all pub items (which marks everything), we only seed items
//! transitively reachable from binary crate entry points.

use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::io::{BufRead, BufReader};
use std::path::Path;

/// Parsed analysis data for a single crate.
#[derive(Debug)]
struct CrateAnalysis {
    crate_name: String,
    crate_type: String,
    source_hash: String,
    is_binary: bool,
    /// All items defined in this crate: path → (is_public, is_function)
    defined_items: HashMap<String, (bool, bool)>,
    /// Call edges: caller_path → callee_path
    call_edges: Vec<(String, String)>,
    /// Trait impls: (trait_path, is_drop, is_object_safe, impl_method_path)
    trait_impls: Vec<(String, bool, bool, String)>,
    /// Static item paths
    statics: Vec<String>,
    /// #[no_mangle] function paths
    no_mangle: Vec<String>,
}

/// Parse a `.analysis` file into CrateAnalysis.
fn parse_analysis_file(path: &Path) -> Result<CrateAnalysis, String> {
    let file = std::fs::File::open(path)
        .map_err(|e| format!("Failed to open {}: {}", path.display(), e))?;
    let reader = BufReader::new(file);

    let mut crate_name = String::new();
    let mut crate_type = String::new();
    let mut source_hash = String::new();
    let mut is_binary = false;
    let mut defined_items = HashMap::new();
    let mut call_edges = Vec::new();
    let mut trait_impls = Vec::new();
    let mut statics = Vec::new();
    let mut no_mangle = Vec::new();

    #[derive(PartialEq)]
    enum Section {
        Header,
        DefinedItems,
        CallEdges,
        TraitImpls,
        Statics,
        NoMangle,
    }

    let mut section = Section::Header;

    for line in reader.lines() {
        let line = line.map_err(|e| format!("Read error: {}", e))?;
        let line = line.trim_end();

        if line.is_empty() {
            continue;
        }

        // Section headers
        match line {
            "DEFINED_ITEMS:" => { section = Section::DefinedItems; continue; }
            "CALL_EDGES:" => { section = Section::CallEdges; continue; }
            "TRAIT_IMPLS:" => { section = Section::TraitImpls; continue; }
            "STATICS:" => { section = Section::Statics; continue; }
            "NO_MANGLE:" => { section = Section::NoMangle; continue; }
            _ => {}
        }

        match section {
            Section::Header => {
                if let Some(val) = line.strip_prefix("SOURCE_HASH:") {
                    source_hash = val.to_string();
                } else if let Some(val) = line.strip_prefix("CRATE_NAME:") {
                    crate_name = val.to_string();
                } else if let Some(val) = line.strip_prefix("CRATE_TYPE:") {
                    crate_type = val.to_string();
                } else if let Some(val) = line.strip_prefix("IS_BINARY:") {
                    is_binary = val == "true";
                }
            }
            Section::DefinedItems => {
                // Format: path\tpub|priv[\tfn|type] (3rd column optional for backward compat)
                let parts: Vec<&str> = line.split('\t').collect();
                if parts.len() >= 2 {
                    let is_pub = parts[1] == "pub";
                    let is_fn = parts.get(2).map_or(false, |k| *k == "fn");
                    defined_items.insert(parts[0].to_string(), (is_pub, is_fn));
                }
            }
            Section::CallEdges => {
                // Format: caller_path\tcallee_path
                if let Some((caller, callee)) = line.split_once('\t') {
                    call_edges.push((caller.to_string(), callee.to_string()));
                }
            }
            Section::TraitImpls => {
                // Format: trait_path\tis_drop\tis_object_safe\timpl_method_path
                let parts: Vec<&str> = line.split('\t').collect();
                if parts.len() >= 4 {
                    trait_impls.push((
                        parts[0].to_string(),
                        parts[1] == "true",
                        parts[2] == "true",
                        parts[3].to_string(),
                    ));
                }
            }
            Section::Statics => {
                statics.push(line.to_string());
            }
            Section::NoMangle => {
                no_mangle.push(line.to_string());
            }
        }
    }

    if crate_name.is_empty() {
        return Err(format!("Missing CRATE_NAME in {}", path.display()));
    }

    Ok(CrateAnalysis {
        crate_name,
        crate_type,
        source_hash,
        is_binary,
        defined_items,
        call_edges,
        trait_impls,
        statics,
        no_mangle,
    })
}

/// Run workspace-wide BFS using analysis data from all crates.
///
/// Reads `.analysis` files, builds a unified call graph, runs BFS from
/// binary entry points, and writes per-crate `.cache` files.
pub fn run_cross_crate_bfs(cache_dir: &Path) -> Result<(), String> {
    // 1. Find and parse all .analysis files
    let mut analyses = Vec::new();

    let entries = std::fs::read_dir(cache_dir)
        .map_err(|e| format!("Failed to read cache dir {}: {}", cache_dir.display(), e))?;

    for entry in entries {
        let entry = entry.map_err(|e| format!("Read dir error: {}", e))?;
        let path = entry.path();
        if path.extension().map_or(false, |ext| ext == "analysis") {
            match parse_analysis_file(&path) {
                Ok(analysis) => analyses.push(analysis),
                Err(e) => eprintln!("Warning: skipping {}: {}", path.display(), e),
            }
        }
    }

    if analyses.is_empty() {
        return Err("No .analysis files found in cache directory".to_string());
    }

    eprintln!("[cross-crate] Loaded {} crate analyses", analyses.len());

    // 2. Build unified call graph: caller → set of callees
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();

    for analysis in &analyses {
        for (caller, callee) in &analysis.call_edges {
            graph.entry(caller.clone()).or_default().insert(callee.clone());
        }
    }

    let total_edges: usize = graph.values().map(|s| s.len()).sum();
    eprintln!("[cross-crate] Unified call graph: {} callers, {} edges", graph.len(), total_edges);

    // 3. Compute seeds: binary entry points + special items
    let mut seeds: HashSet<String> = HashSet::new();

    for analysis in &analyses {
        if analysis.is_binary {
            // Binary crate: only main is a seed
            for path in analysis.defined_items.keys() {
                if path == "main" || path.ends_with("::main") {
                    seeds.insert(path.clone());
                }
            }
        }
        // NO automatic pub seeds for library crates (the whole point!)

        // Seed Drop impls and object-safe trait impls from all crates
        for (_trait_path, is_drop, is_object_safe, impl_method_path) in &analysis.trait_impls {
            if *is_drop || *is_object_safe {
                seeds.insert(impl_method_path.clone());
            }
        }

        // Seed statics from all crates
        for static_path in &analysis.statics {
            seeds.insert(static_path.clone());
        }

        // Seed #[no_mangle] functions from all crates
        for nm_path in &analysis.no_mangle {
            seeds.insert(nm_path.clone());
        }
    }

    eprintln!("[cross-crate] BFS seeds: {}", seeds.len());

    // 4. BFS from seeds across unified graph
    let mut marked: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = seeds.into_iter().collect();

    while let Some(item) = queue.pop_front() {
        if !marked.insert(item.clone()) {
            continue;
        }

        if let Some(deps) = graph.get(&item) {
            for dep in deps {
                if !marked.contains(dep) {
                    queue.push_back(dep.clone());
                }
            }
        }
    }

    eprintln!("[cross-crate] BFS marked {} items total", marked.len());

    // 5. Write per-crate .seeds files
    // Instead of writing full marked-items caches, we write just the SEEDS
    // for each library crate. The driver will do its own crate-local BFS
    // from these seeds during the next build.
    let mut total_seeds_reduced = 0usize;

    for analysis in &analyses {
        let seeds_path = cache_dir.join(format!("{}-{}.seeds", analysis.crate_name, analysis.crate_type));

        if analysis.is_binary {
            // Binary crates don't need seed files — they always use main
            continue;
        }

        // For library crates: seeds = pub items that are reachable in global BFS
        // This replaces the default "all pub items" with a much smaller set.
        let all_pub: Vec<&String> = analysis.defined_items.iter()
            .filter(|(_, (is_pub, _))| *is_pub)
            .map(|(path, _)| path)
            .collect();

        let reachable_pub: BTreeSet<&String> = all_pub.iter()
            .copied()
            .filter(|path| marked.contains(*path))
            .collect();

        let pub_count = all_pub.len();
        let seed_count = reachable_pub.len();

        if seed_count < pub_count {
            total_seeds_reduced += pub_count - seed_count;
            eprintln!("[cross-crate]   {} ({}): seeds {}/{} pub items ({:.1}% reduction in seeds)",
                analysis.crate_name, analysis.crate_type,
                seed_count, pub_count,
                if pub_count > 0 { (pub_count - seed_count) as f64 / pub_count as f64 * 100.0 } else { 0.0 });
        }

        // Write seeds file: hash on first line, then seed paths
        let mut content = String::new();
        content.push_str(&analysis.source_hash);
        content.push('\n');
        for item in &reachable_pub {
            content.push_str(item);
            content.push('\n');
        }
        // Also include unconditional seeds (Drop, object-safe trait impls, statics, no_mangle)
        // so the driver doesn't need to re-derive them
        for (_trait_path, is_drop, is_object_safe, method_path) in &analysis.trait_impls {
            if *is_drop || *is_object_safe {
                if !reachable_pub.contains(&method_path) {
                    content.push_str(method_path);
                    content.push('\n');
                }
            }
        }
        for s in &analysis.statics {
            if !reachable_pub.contains(s) {
                content.push_str(s);
                content.push('\n');
            }
        }
        for nm in &analysis.no_mangle {
            if !reachable_pub.contains(nm) {
                content.push_str(nm);
                content.push('\n');
            }
        }

        std::fs::write(&seeds_path, content)
            .map_err(|e| format!("Failed to write seeds {}: {}", seeds_path.display(), e))?;
    }

    eprintln!("[cross-crate] Total: {} pub seeds eliminated across all library crates",
        total_seeds_reduced);

    // 6. Write .cache files and .skip-driver markers per crate.
    // For each library crate, compute the full marked set via intra-crate BFS
    // expansion, then write a .cache file in save_cache_entry format (hash + items).
    // Also write .skip-driver if ALL function items are reachable.
    let mut skipped_count = 0usize;
    let mut cache_count = 0usize;
    for analysis in &analyses {
        if analysis.is_binary {
            continue; // Binary crates always need driver (for optimized_mir override)
        }

        // Collect globally-reachable items that belong to this crate
        let mut crate_marked: HashSet<String> = HashSet::new();
        for item in &marked {
            if analysis.defined_items.contains_key(item) {
                crate_marked.insert(item.clone());
            }
        }

        // Add unconditional seeds (Drop impls, object-safe trait impls, statics, no_mangle)
        for (_, is_drop, is_obj_safe, method) in &analysis.trait_impls {
            if *is_drop || *is_obj_safe {
                crate_marked.insert(method.clone());
            }
        }
        for s in &analysis.statics { crate_marked.insert(s.clone()); }
        for nm in &analysis.no_mangle { crate_marked.insert(nm.clone()); }

        // Expand via intra-crate call edges (BFS)
        let mut queue: VecDeque<String> = crate_marked.iter().cloned().collect();
        let intra_edges: HashMap<&str, Vec<&str>> = {
            let mut map: HashMap<&str, Vec<&str>> = HashMap::new();
            for (caller, callee) in &analysis.call_edges {
                if analysis.defined_items.contains_key(callee) {
                    map.entry(caller.as_str()).or_default().push(callee.as_str());
                }
            }
            map
        };
        while let Some(item) = queue.pop_front() {
            if let Some(callees) = intra_edges.get(item.as_str()) {
                for callee in callees {
                    if crate_marked.insert(callee.to_string()) {
                        queue.push_back(callee.to_string());
                    }
                }
            }
        }

        // Write .cache file with full marked set (hash + items)
        // This allows the driver to skip HIR/BFS analysis entirely.
        let cache_path = cache_dir.join(
            format!("{}-{}.cache", analysis.crate_name, analysis.crate_type));
        let mut cache_content = String::new();
        cache_content.push_str(&analysis.source_hash);
        cache_content.push('\n');
        let sorted_marked: BTreeSet<_> = crate_marked.iter().collect();
        for item in &sorted_marked {
            cache_content.push_str(item);
            cache_content.push('\n');
        }
        if std::fs::write(&cache_path, &cache_content).is_ok() {
            cache_count += 1;
        }

        // Check if all FUNCTION items are reachable (only functions can be stubbed).
        // Non-function items (structs, enums, mods, traits) never go through
        // optimized_mir so they don't affect skip-driver decision.
        let all_fn_marked = analysis.defined_items.iter()
            .filter(|(_, (_, is_fn))| *is_fn)
            .all(|(item, _)| crate_marked.contains(item));

        if all_fn_marked {
            let skip_path = cache_dir.join(
                format!("{}-{}.skip-driver", analysis.crate_name, analysis.crate_type));
            let _ = std::fs::write(&skip_path, &analysis.source_hash);
            skipped_count += 1;
        }
    }
    eprintln!("[cross-crate] Wrote {} .cache files", cache_count);
    eprintln!("[cross-crate] {} crates can skip driver (all function items reachable)", skipped_count);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_parse_analysis_file() {
        let dir = std::env::temp_dir().join("cross_crate_bfs_test");
        let _ = std::fs::create_dir_all(&dir);

        let analysis_path = dir.join("mylib-lib.analysis");
        let mut f = std::fs::File::create(&analysis_path).unwrap();
        writeln!(f, "CRATE_NAME:mylib").unwrap();
        writeln!(f, "CRATE_TYPE:lib").unwrap();
        writeln!(f, "SOURCE_HASH:abc123").unwrap();
        writeln!(f, "IS_BINARY:false").unwrap();
        writeln!(f, "DEFINED_ITEMS:").unwrap();
        writeln!(f, "mylib::foo\tpub").unwrap();
        writeln!(f, "mylib::bar\tpriv").unwrap();
        writeln!(f, "CALL_EDGES:").unwrap();
        writeln!(f, "mylib::foo\tmylib::bar").unwrap();
        writeln!(f, "TRAIT_IMPLS:").unwrap();
        writeln!(f, "STATICS:").unwrap();
        writeln!(f, "NO_MANGLE:").unwrap();
        drop(f);

        let analysis = parse_analysis_file(&analysis_path).unwrap();
        assert_eq!(analysis.crate_name, "mylib");
        assert_eq!(analysis.crate_type, "lib");
        assert_eq!(analysis.source_hash, "abc123");
        assert!(!analysis.is_binary);
        assert_eq!(analysis.defined_items.len(), 2);
        assert_eq!(analysis.call_edges.len(), 1);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_cross_crate_bfs_basic() {
        let dir = std::env::temp_dir().join("cross_crate_bfs_test2");
        let _ = std::fs::remove_dir_all(&dir);
        let _ = std::fs::create_dir_all(&dir);

        // Create a binary crate analysis
        let bin_path = dir.join("mybin-bin.analysis");
        let mut f = std::fs::File::create(&bin_path).unwrap();
        writeln!(f, "CRATE_NAME:mybin").unwrap();
        writeln!(f, "CRATE_TYPE:bin").unwrap();
        writeln!(f, "SOURCE_HASH:hash1").unwrap();
        writeln!(f, "IS_BINARY:true").unwrap();
        writeln!(f, "DEFINED_ITEMS:").unwrap();
        writeln!(f, "mybin::main\tpriv\tfn").unwrap();
        writeln!(f, "CALL_EDGES:").unwrap();
        writeln!(f, "mybin::main\tmylib::foo").unwrap();
        writeln!(f, "TRAIT_IMPLS:").unwrap();
        writeln!(f, "STATICS:").unwrap();
        writeln!(f, "NO_MANGLE:").unwrap();
        drop(f);

        // Create a library crate analysis with an unreachable function
        let lib_path = dir.join("mylib-lib.analysis");
        let mut f = std::fs::File::create(&lib_path).unwrap();
        writeln!(f, "CRATE_NAME:mylib").unwrap();
        writeln!(f, "CRATE_TYPE:lib").unwrap();
        writeln!(f, "SOURCE_HASH:hash2").unwrap();
        writeln!(f, "IS_BINARY:false").unwrap();
        writeln!(f, "DEFINED_ITEMS:").unwrap();
        writeln!(f, "mylib::foo\tpub\tfn").unwrap();
        writeln!(f, "mylib::bar\tpub\tfn").unwrap();
        writeln!(f, "mylib::unused\tpub\tfn").unwrap();
        writeln!(f, "mylib::MyStruct\tpub\ttype").unwrap();
        writeln!(f, "CALL_EDGES:").unwrap();
        writeln!(f, "mylib::foo\tmylib::bar").unwrap();
        writeln!(f, "TRAIT_IMPLS:").unwrap();
        writeln!(f, "STATICS:").unwrap();
        writeln!(f, "NO_MANGLE:").unwrap();
        drop(f);

        // Run cross-crate BFS
        run_cross_crate_bfs(&dir).unwrap();

        // Verify: mylib seeds should contain foo (called from binary) but NOT unused
        // bar is private so it won't appear in seeds (it's found by crate-local BFS)
        let seeds_path = dir.join("mylib-lib.seeds");
        let content = std::fs::read_to_string(&seeds_path).unwrap();
        assert!(content.contains("mylib::foo"), "foo should be a seed (called from binary)");
        assert!(content.contains("mylib::bar"), "bar should be a seed (reachable via foo, and it's pub)");
        assert!(!content.contains("mylib::unused"), "unused should NOT be a seed");

        // Verify: .cache file should be written with marked items
        let cache_path = dir.join("mylib-lib.cache");
        assert!(cache_path.exists(), ".cache file should exist");
        let cache_content = std::fs::read_to_string(&cache_path).unwrap();
        assert!(cache_content.starts_with("hash2\n"), "cache should start with source hash");
        assert!(cache_content.contains("mylib::foo"), "cache should contain foo");
        assert!(cache_content.contains("mylib::bar"), "cache should contain bar");

        // Verify: skip-driver should NOT exist (unused fn is unreachable)
        let skip_path = dir.join("mylib-lib.skip-driver");
        assert!(!skip_path.exists(), "skip-driver should NOT exist when function items are unreachable");

        let _ = std::fs::remove_dir_all(&dir);
    }
}

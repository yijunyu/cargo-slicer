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

/// Controls when library crates skip the rustc driver (no MIR stubbing).
enum SkipMode {
    /// Never skip — all crates go through driver. (`CARGO_SLICER_SKIP_THRESHOLD=never`)
    Never,
    /// Skip when predicted stubs < N. (`CARGO_SLICER_SKIP_THRESHOLD=N`)
    Absolute(usize),
    /// Adaptive heuristic based on stub count and ratio. (default, or `=auto`)
    Auto,
}

fn parse_skip_threshold(s: &str) -> SkipMode {
    let s = s.trim();
    match s {
        "auto" | "Auto" | "AUTO" => SkipMode::Auto,
        "never" | "Never" | "NEVER" => SkipMode::Never,
        _ => match s.parse::<usize>() {
            Ok(0) => SkipMode::Never,
            Ok(n) => SkipMode::Absolute(n),
            Err(_) => {
                eprintln!("[cross-crate] Warning: invalid CARGO_SLICER_SKIP_THRESHOLD={:?}, using auto", s);
                SkipMode::Auto
            }
        },
    }
}

/// Auto heuristic: skip driver when it can't meaningfully help.
/// Returns true (skip) when predicted stubs are zero or negligibly small.
fn should_skip_auto(potential_stubs: usize, total_fns: usize) -> bool {
    if potential_stubs == 0 { return true; }
    if total_fns == 0 { return true; }
    // Driver overhead (~100ms loading + per-fn analysis) exceeds stub savings
    // when predicted stubs are few. With typical 10-30% prediction accuracy,
    // <10 predicted stubs means <1-3 actual stubs = <2-6ms codegen savings.
    potential_stubs < 10
}

/// Parsed analysis data for a single crate.
#[derive(Debug)]
struct CrateAnalysis {
    crate_name: String,
    crate_type: String,
    source_hash: String,
    is_binary: bool,
    /// All items defined in this crate: path → (is_public, is_function, is_stubbable_hint)
    defined_items: HashMap<String, (bool, bool, bool)>,
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
                // Format: path\tpub|priv[\tfn|type[\tstubbable]]
                let mut fields = line.split('\t');
                if let (Some(path), Some(vis)) = (fields.next(), fields.next()) {
                    let is_pub = vis == "pub";
                    let is_fn = fields.next().map_or(false, |k| k == "fn");
                    let is_stubbable = fields.next().map_or(false, |k| k == "stubbable");
                    defined_items.insert(path.to_string(), (is_pub, is_fn, is_stubbable));
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
                let mut fields = line.split('\t');
                if let (Some(trait_path), Some(is_drop), Some(is_obj_safe), Some(impl_method)) =
                    (fields.next(), fields.next(), fields.next(), fields.next())
                {
                    trait_impls.push((
                        trait_path.to_string(),
                        is_drop == "true",
                        is_obj_safe == "true",
                        impl_method.to_string(),
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

    // Build method-name index for resolving wildcard edges (*::method_name).
    // Maps last path segment → list of full paths with that segment.
    let mut method_index: HashMap<&str, Vec<&str>> = HashMap::new();
    for analysis in &analyses {
        for (path, _) in &analysis.defined_items {
            if let Some(last_seg) = path.rsplit("::").next() {
                method_index.entry(last_seg).or_default().push(path.as_str());
            }
        }
    }

    // 4. BFS from seeds across unified graph
    let mut marked: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = seeds.into_iter().collect();

    while let Some(item) = queue.pop_front() {
        if !marked.insert(item.clone()) {
            continue;
        }

        if let Some(deps) = graph.get(&item) {
            for dep in deps {
                if dep.starts_with("*::") {
                    // Wildcard method edge: resolve against all items with matching last segment
                    let method_name = &dep[3..];
                    if let Some(targets) = method_index.get(method_name) {
                        for target in targets {
                            if !marked.contains(*target) {
                                queue.push_back(target.to_string());
                            }
                        }
                    }
                } else if !marked.contains(dep) {
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
            .filter(|(_, (is_pub, _, _))| *is_pub)
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

        // Atomic write: .tmp then rename to prevent partial reads during concurrent access
        let tmp_path = seeds_path.with_extension("seeds.tmp");
        std::fs::write(&tmp_path, content)
            .map_err(|e| format!("Failed to write seeds {}: {}", tmp_path.display(), e))?;
        std::fs::rename(&tmp_path, &seeds_path)
            .map_err(|e| format!("Failed to rename {}: {}", seeds_path.display(), e))?;
    }

    eprintln!("[cross-crate] Total: {} pub seeds eliminated across all library crates",
        total_seeds_reduced);

    // 6. Pre-build per-crate intra-crate edge maps (method index + resolved edges).
    // Done once upfront instead of rebuilding per crate in the write loop.
    let intra_edges_per_crate: Vec<HashMap<&str, Vec<&str>>> = analyses.iter().map(|analysis| {
        let mut map: HashMap<&str, Vec<&str>> = HashMap::new();
        // Build a crate-local method index for wildcard resolution
        let mut local_method_idx: HashMap<&str, Vec<&str>> = HashMap::new();
        for (path, _) in &analysis.defined_items {
            if let Some(last_seg) = path.rsplit("::").next() {
                local_method_idx.entry(last_seg).or_default().push(path.as_str());
            }
        }
        for (caller, callee) in &analysis.call_edges {
            if callee.starts_with("*::") {
                let method_name = &callee[3..];
                if let Some(targets) = local_method_idx.get(method_name) {
                    for target in targets {
                        map.entry(caller.as_str()).or_default().push(target);
                    }
                }
            } else if analysis.defined_items.contains_key(callee) {
                map.entry(caller.as_str()).or_default().push(callee.as_str());
            }
        }
        map
    }).collect();

    // Write .cache files and .skip-driver markers per crate.
    // NOTE: .cache files use syn-format paths. The driver's is_item_marked()
    // normalizes trait impl paths (e.g., "<T as Trait>::m" → "T::m") so
    // lookups against syn-format caches still match correctly.
    let skip_mode = match std::env::var("CARGO_SLICER_SKIP_THRESHOLD") {
        Ok(val) => parse_skip_threshold(&val),
        Err(_) => SkipMode::Auto,
    };
    let mut skipped_count = 0usize;
    let mut driver_count = 0usize;
    let mut cache_count = 0usize;
    for (idx, analysis) in analyses.iter().enumerate() {
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

        // Expand via intra-crate call edges (BFS) using pre-built edge map
        let mut queue: VecDeque<String> = crate_marked.iter().cloned().collect();
        let intra_edges = &intra_edges_per_crate[idx];
        while let Some(item) = queue.pop_front() {
            if let Some(callees) = intra_edges.get(item.as_str()) {
                for callee in callees {
                    if crate_marked.insert(callee.to_string()) {
                        queue.push_back(callee.to_string());
                    }
                }
            }
        }

        // Write .cache file with full marked set (syn-format paths).
        // The driver's is_item_marked() normalizes trait impl paths so these
        // syn-format paths are matched correctly during codegen.
        let cache_path = cache_dir.join(
            format!("{}-{}.cache", analysis.crate_name, analysis.crate_type));
        let mut cache_content = String::new();
        cache_content.push_str(&analysis.source_hash);
        cache_content.push('\n');
        let mut sorted_marked: Vec<&String> = crate_marked.iter().collect();
        sorted_marked.sort_unstable();
        for item in &sorted_marked {
            cache_content.push_str(item);
            cache_content.push('\n');
        }
        // Atomic write: .tmp then rename
        let cache_tmp = cache_path.with_extension("cache.tmp");
        if std::fs::write(&cache_tmp, &cache_content).is_ok() {
            if std::fs::rename(&cache_tmp, &cache_path).is_ok() {
                cache_count += 1;
            }
        }

        // Categorize functions by reachability and stubbability.
        // This reveals the upper bound on stubs per crate for threshold tuning.
        let is_lib = !analysis.is_binary;
        let mut reachable_stubbable = 0usize;
        let mut reachable_unstubbable = 0usize;
        let mut unreachable_stubbable = 0usize;
        let mut unreachable_unstubbable = 0usize;
        for (item, (_, is_fn, is_stubbable)) in &analysis.defined_items {
            if !*is_fn { continue; }
            let reachable = crate_marked.contains(item.as_str());
            match (reachable, *is_stubbable) {
                (true, true) => reachable_stubbable += 1,
                (true, false) => reachable_unstubbable += 1,
                (false, true) => unreachable_stubbable += 1,
                (false, false) => unreachable_unstubbable += 1,
            }
        }
        let total_fns = reachable_stubbable + reachable_unstubbable
            + unreachable_stubbable + unreachable_unstubbable;

        // potential_stubs = unreachable stubbable fns, excluding pub items in lib crates
        // (driver refuses to stub pub items in lib crates to preserve public API)
        let potential_stubs: usize = analysis.defined_items.iter()
            .filter(|(_, (_, is_fn, _))| *is_fn)
            .filter(|(item, (is_pub, _, is_stubbable))| {
                !crate_marked.contains(item.as_str()) && *is_stubbable && !(is_lib && *is_pub)
            })
            .count();

        let should_skip = match &skip_mode {
            SkipMode::Never => false,
            SkipMode::Absolute(t) => potential_stubs < *t,
            SkipMode::Auto => should_skip_auto(potential_stubs, total_fns),
        };

        let skip_label = if should_skip {
            match &skip_mode {
                SkipMode::Auto => " (SKIP: auto)".to_string(),
                SkipMode::Absolute(t) => format!(" (SKIP: < {})", t),
                SkipMode::Never => unreachable!(),
            }
        } else {
            " (DRIVER)".to_string()
        };

        if should_skip {
            let skip_path = cache_dir.join(
                format!("{}-{}.skip-driver", analysis.crate_name, analysis.crate_type));
            // Atomic write: .tmp then rename
            let skip_tmp = skip_path.with_extension("skip-driver.tmp");
            let _ = std::fs::write(&skip_tmp, &analysis.source_hash);
            let _ = std::fs::rename(&skip_tmp, &skip_path);
            skipped_count += 1;
        } else {
            driver_count += 1;
        }

        if total_fns > 0 {
            eprintln!("[cross-crate]   {} ({}): {} fns = {} reachable ({} stubbable + {} unstubbable) \
                + {} unreachable ({} stubbable + {} unstubbable) → {} potential stubs{}",
                analysis.crate_name, analysis.crate_type, total_fns,
                reachable_stubbable + reachable_unstubbable,
                reachable_stubbable, reachable_unstubbable,
                unreachable_stubbable + unreachable_unstubbable,
                unreachable_stubbable, unreachable_unstubbable,
                potential_stubs, skip_label);
        }
    }
    let mode_desc = match &skip_mode {
        SkipMode::Never => "never".to_string(),
        SkipMode::Absolute(t) => format!("< {}", t),
        SkipMode::Auto => "auto".to_string(),
    };
    eprintln!("[cross-crate] Wrote {} .cache files", cache_count);
    eprintln!("[cross-crate] Skip threshold: {} → {} DRIVER, {} SKIP", mode_desc, driver_count, skipped_count);

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
        writeln!(f, "mybin::main\tpriv\tfn\tstubbable").unwrap();
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
        writeln!(f, "mylib::foo\tpub\tfn\tstubbable").unwrap();
        writeln!(f, "mylib::bar\tpub\tfn\tstubbable").unwrap();
        writeln!(f, "mylib::unused\tpub\tfn\tstubbable").unwrap();
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

        // Verify: skip-driver SHOULD exist — all items are pub so potential_stubs == 0,
        // and auto heuristic skips crates with zero predicted stubs
        let skip_path = dir.join("mylib-lib.skip-driver");
        assert!(skip_path.exists(), "skip-driver should exist when potential stubs are below threshold");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_skip_threshold_parsing() {
        // "0" and "never" → Never
        assert!(matches!(parse_skip_threshold("0"), SkipMode::Never));
        assert!(matches!(parse_skip_threshold("never"), SkipMode::Never));
        assert!(matches!(parse_skip_threshold("NEVER"), SkipMode::Never));

        // Positive integers → Absolute
        assert!(matches!(parse_skip_threshold("5"), SkipMode::Absolute(5)));
        assert!(matches!(parse_skip_threshold("200"), SkipMode::Absolute(200)));

        // "auto" → Auto
        assert!(matches!(parse_skip_threshold("auto"), SkipMode::Auto));
        assert!(matches!(parse_skip_threshold("AUTO"), SkipMode::Auto));

        // Invalid → Auto (with warning)
        assert!(matches!(parse_skip_threshold("abc"), SkipMode::Auto));
        assert!(matches!(parse_skip_threshold("-1"), SkipMode::Auto));
    }

    #[test]
    fn test_auto_heuristic() {
        // 0 stubs → always skip (bevy all-pub crates)
        assert!(should_skip_auto(0, 100));
        assert!(should_skip_auto(0, 0));

        // 0 total fns → skip
        assert!(should_skip_auto(5, 0));

        // < 10 stubs → skip (driver overhead exceeds savings)
        assert!(should_skip_auto(2, 300));
        assert!(should_skip_auto(5, 100));
        assert!(should_skip_auto(9, 500));

        // ≥10 stubs → don't skip
        assert!(!should_skip_auto(10, 100));
        assert!(!should_skip_auto(50, 1000));
        assert!(!should_skip_auto(100, 500));
    }
}

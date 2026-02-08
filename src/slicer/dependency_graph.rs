//! Build dependency graph (what items depend on what)
//!
//! This module reuses the existing parsing infrastructure to build a graph
//! of item dependencies, which can then be used for marking and deletion.
//!
//! Supports two modes:
//! - Full graph building: Parse all files (default)
//! - Incremental graph building: Parse only reachable items via BFS (with -fincremental-graph)
//!
//! Supports three parsers:
//! - Syn-based (default): Accurate AST parsing
//! - Ctags-based (with -fuse-ctags): Fast text-based parsing (10x faster)
//! - Fast tokenizer (with -fuse-fast-tokenizer): Single-pass lightweight parsing (10-50x faster)

use std::path::Path;
use std::collections::{HashMap, HashSet};
use crate::types::{CrateIndex, ParsedItem, ParsedItemKind, ItemVisibility};
use crate::old_slicer::parsing::parse_crate;
use crate::constants::RUST_KEYWORDS;
use crate::usage::find_rust_files;
use crate::ctags_rs::rust_parser::{parse_rust_file_with_content, RustTag, TagKind, Visibility};
use super::incremental_graph;
use super::config::SlicerConfig;
// use super::ctags_parser;  // Unused - kept for reference
// use super::text_dependencies::{extract_dependencies_fast, extract_dependencies_with_automaton};  // Now using automaton directly
// use walkdir::WalkDir;  // Unused - only needed for build_graph_ctags (experimental, not called)

/// Dependency graph: item_name -> list of items it depends on
pub type Graph = HashMap<String, Vec<String>>;

/// Build dependency graph by parsing the crate
///
/// Supports two modes:
/// - Full mode (default): Parse all files in the crate
/// - Incremental mode (config.features.incremental_graph): Parse only reachable items
pub fn build_graph(
    output_dir: &Path,
    crate_name: &str,
    config: &SlicerConfig,
    entry_points: Option<&HashSet<String>>,
) -> Result<(Graph, CrateIndex, HashSet<String>), String> {
    // If incremental graph building is enabled AND we have entry points, use it
    if config.features.incremental_graph && entry_points.is_some() {
        let entry_points = entry_points.unwrap();

        // First, extract public re-exports (lightweight scan of lib.rs only)
        let public_reexports = extract_public_reexports_lightweight(output_dir)?;

        // Then build graph incrementally
        incremental_graph::build_graph_incremental(
            output_dir,
            crate_name,
            entry_points,
            &public_reexports,
        )
    } else {
        // Fall back to full graph building
        build_graph_full(output_dir, crate_name, config)
    }
}

/// Build full dependency graph (parse all files)
///
/// Dispatches to syn-based, ctags-based, or fast-tokenizer-based parsing depending on configuration
pub fn build_graph_full(output_dir: &Path, crate_name: &str, config: &SlicerConfig) -> Result<(Graph, CrateIndex, HashSet<String>), String> {
    // Dispatch based on parser selection
    if config.features.use_fast_tokenizer {
        // Use fast tokenizer (10-50x faster, single-pass)
        build_graph_fast_tokenizer(output_dir, crate_name)
    } else if config.features.use_ctags {
        // Use ctags-based parsing (10.6x faster)
        build_graph_pure_rust_ctags(output_dir, crate_name)
    } else {
        // Use syn-based parsing (default, accurate AST)
        let index = parse_crate(output_dir, crate_name);

        // Build the dependency graph
        let graph = build_graph_from_index(&index);

        // Extract public re-exports from lib.rs - these are part of the crate's public API
        // and should always be kept even if not directly used
        let public_reexports = extract_public_reexports(&index);

        Ok((graph, index, public_reexports))
    }
}

/// Build dependency graph using pure-Rust ctags parser (10.6x faster than syn)
///
/// This function uses the validated pure-Rust ctags parser to extract tags
/// from Rust files. It's significantly faster than syn-based parsing while
/// maintaining accuracy for Phase 1 item extraction (top-level items).
///
/// Current limitations (Phase 1 MVP):
/// - Only extracts top-level items (functions, structs, enums, traits, modules, etc.)
/// - Does not parse impl blocks or methods yet (Phase 2)
/// - Does not extract generic parameters yet (Phase 3)
pub fn build_graph_pure_rust_ctags(output_dir: &Path, crate_name: &str) -> Result<(Graph, CrateIndex, HashSet<String>), String> {
    use crate::slicer::text_dependencies::extract_dependencies_fast;

    let src_path = output_dir.join("src");
    let search_path = if src_path.exists() { src_path.as_path() } else { output_dir };

    let mut all_items = Vec::new();
    let mut graph = HashMap::new();

    // SINGLE PASS: Parse and extract dependencies in one loop
    for file_path in find_rust_files(search_path) {
        // Parse ctags and get file content (single read!)
        let (tags, content) = match parse_rust_file_with_content(&file_path) {
            Ok((tags, content)) => (tags, content),
            Err(e) => {
                eprintln!("Warning: Failed to parse {}: {}", file_path.display(), e);
                continue;
            }
        };

        // Convert tags to ParsedItems
        let file_items: Vec<ParsedItem> = tags.iter()
            .filter_map(|tag| convert_tag_to_parsed_item(tag, crate_name))
            .collect();

        // Collect all item names for dependency matching
        let all_item_names: Vec<String> = file_items.iter()
            .map(|item| item.name.clone())
            .collect();

        // Extract dependencies from file content
        for item in &file_items {
            let deps = extract_dependencies_fast(&content, &all_item_names);

            // Convert item names to full paths for graph edges
            let dep_paths: Vec<String> = deps.iter()
                .filter_map(|dep_name| {
                    // Find the item with this name
                    file_items.iter()
                        .chain(all_items.iter())  // Also check previously parsed items
                        .find(|i| &i.name == dep_name)
                        .map(|i| i.path.clone())
                })
                .collect();

            graph.insert(item.path.clone(), dep_paths);
        }

        // Accumulate all items
        all_items.extend(file_items);
    }

    // Create CrateIndex
    // CRITICAL: Move all_items instead of cloning to avoid O(items) memory allocation
    let index = CrateIndex {
        items: HashMap::new(),
        impls: HashMap::new(),
        all_items,  // Move instead of clone to prevent OOM
        reexports: HashMap::new(),
        use_statements: Vec::new(),
        internal_aliases: HashMap::new(),
        macro_use_crates: HashSet::new(),
    };

    // Extract public re-exports (lightweight)
    let public_reexports = extract_public_reexports_lightweight(output_dir)?;

    Ok((graph, index, public_reexports))
}

/// Build dependency graph using fast tokenizer (10-50x faster than syn, two-pass)
///
/// This function uses a two-pass architecture to build the aho-corasick automaton
/// once per crate instead of once per file, eliminating O(files²) complexity:
///
/// Pass 1: Parse all files, collect all items (no dependency extraction)
/// Pass 2: Build automaton ONCE, extract dependencies for all items
pub fn build_graph_fast_tokenizer(output_dir: &Path, crate_name: &str) -> Result<(Graph, CrateIndex, HashSet<String>), String> {
    use super::fast_tokenizer::FastTokenizer;
    use crate::slicer::text_dependencies::extract_dependencies_with_automaton;
    use std::fs;
    // use std::path::PathBuf;  // PathBuf not needed - using String keys

    let src_path = output_dir.join("src");
    let search_path = if src_path.exists() { src_path.as_path() } else { output_dir };

    let mut all_items = Vec::new();
    let mut graph = HashMap::new();

    // PASS 1: Parse all files, collect all items (no dependency extraction yet)
    // Memory optimization: Store file contents separately instead of duplicating source in every ParsedItem
    let mut file_contents: HashMap<String, String> = HashMap::new();
    let mut items_with_ranges: HashMap<String, Vec<super::fast_tokenizer::ItemWithRange>> = HashMap::new();

    for file_path in find_rust_files(search_path) {
        // Read file content
        let content = match fs::read_to_string(&file_path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Warning: Failed to read {}: {}", file_path.display(), e);
                continue;
            }
        };

        // Parse with fast tokenizer (single pass, returns byte ranges)
        let file_path_str = file_path.to_string_lossy().to_string();
        let parsed_items = FastTokenizer::parse_file(&content, &file_path_str);

        // Convert to ParsedItems WITHOUT extracting source (deferred until dependency extraction)
        let file_items: Vec<ParsedItem> = parsed_items.iter()
            .filter_map(|item| {
                convert_fast_tokenizer_item(item, crate_name, &file_path_str, "")
            })
            .collect();

        // Store file content and raw items for later dependency extraction
        // Use String as key to match item.file type
        file_contents.insert(file_path_str.clone(), content);
        items_with_ranges.insert(file_path_str.clone(), parsed_items);

        // Accumulate all items from this file
        all_items.extend(file_items);
    }

    // PASS 2: Build aho-corasick automaton ONCE with all item names
    // This is the key optimization - build automaton once per crate, not per file
    let all_item_names: Vec<String> = all_items.iter()
        .map(|item| item.name.clone())
        .collect();

    // Build name-to-path index to avoid O(items²) lookups
    // This eliminates linear search for each dependency
    let name_to_path: HashMap<String, Vec<String>> = {
        let mut map = HashMap::new();
        for item in &all_items {
            map.entry(item.name.clone())
                .or_insert_with(Vec::new)
                .push(item.path.clone());
        }
        map
    };

    use aho_corasick::AhoCorasick;
    let ac = if !all_item_names.is_empty() {
        Some(AhoCorasick::new(&all_item_names).expect("Failed to build pattern matcher"))
    } else {
        None
    };

    // PASS 2 (continued): Extract dependencies for all items using pre-built automaton
    // Memory optimization: Extract source on-demand from file_contents HashMap using byte ranges
    for item in &all_items {
        // Extract dependencies from the item's source code (reuse single automaton)
        let deps = if let Some(ref automaton) = ac {
            // Get file content and item range for this specific item
            // Note: item.file is a PathBuf, HashMap keys are String
            let file_str = item.file.to_string_lossy().to_string();
            let source = if let Some(content) = file_contents.get(&file_str) {
                // Find the corresponding ItemWithRange to get byte offsets
                if let Some(items) = items_with_ranges.get(&file_str) {
                    // Find matching item by name and kind
                    if let Some(item_with_range) = items.iter().find(|i: &&super::fast_tokenizer::ItemWithRange| {
                        i.name == item.name && i.kind == item.kind
                    }) {
                        // Extract source using byte range (avoids storing in ParsedItem)
                        content.get(item_with_range.start_byte..item_with_range.end_byte)
                            .unwrap_or("")
                    } else {
                        ""
                    }
                } else {
                    ""
                }
            } else {
                ""
            };

            extract_dependencies_with_automaton(source, &all_item_names, automaton)
        } else {
            HashSet::new()
        };

        // Convert item names to full paths for graph edges using index
        // O(deps) instead of O(deps × items)
        let dep_paths: Vec<String> = deps.iter()
            .flat_map(|dep_name| {
                name_to_path.get(dep_name)
                    .map(|paths| paths.iter().cloned())
                    .into_iter()
                    .flatten()
            })
            .collect();

        graph.insert(item.path.clone(), dep_paths);
    }

    // Create CrateIndex
    // CRITICAL: Move all_items instead of cloning to avoid O(items) memory allocation
    let index = CrateIndex {
        items: HashMap::new(),
        impls: HashMap::new(),
        all_items,  // Move instead of clone to prevent OOM
        reexports: HashMap::new(),
        use_statements: Vec::new(),
        internal_aliases: HashMap::new(),
        macro_use_crates: HashSet::new(),
    };

    // Extract public re-exports
    let public_reexports = extract_public_reexports_lightweight(output_dir)?;

    Ok((graph, index, public_reexports))
}

/// Convert FastTokenizer ItemWithRange to ParsedItem
fn convert_fast_tokenizer_item(item: &super::fast_tokenizer::ItemWithRange, crate_name: &str, file_path: &str, source: &str) -> Option<ParsedItem> {
    use std::collections::HashSet;
    use std::path::PathBuf;

    // Skip impl blocks (same as ctags)
    if matches!(item.kind, ParsedItemKind::Impl) {
        return None;
    }

    // Compute module path (e.g., "src/seq/index.rs" -> "seq::index")
    let module_path = compute_module_path_from_file(file_path);

    // Compute qualified path (crate::module::item)
    let path = if module_path.is_empty() {
        format!("{}::{}", crate_name, item.name)
    } else {
        format!("{}::{}::{}", crate_name, module_path, item.name)
    };

    // Determine if item is public
    let is_pub = matches!(item.visibility, ItemVisibility::Public | ItemVisibility::Crate);

    Some(ParsedItem {
        name: item.name.clone(),
        path,
        kind: item.kind.clone(),
        source: source.to_string(),
        dependencies: HashSet::new(), // Will be filled by dependency extraction
        typed_dependencies: Vec::new(), // Will be filled by dependency extraction
        external_crates: HashSet::new(), // Will be filled by dependency extraction
        module_path: module_path.clone(),
        is_pub,
        visibility: item.visibility.clone(),
        file: PathBuf::from(file_path),
        line: None, // Fast tokenizer uses byte ranges, not line numbers
        cfg_attr: None, // Not extracted by fast tokenizer
        generics: Vec::new(), // Not extracted by fast tokenizer
    })
}

/// Compute module path from file path (e.g., "src/seq/index.rs" -> "seq::index")
fn compute_module_path_from_file(file_path: &str) -> String {
    use std::path::Path;

    let path = Path::new(file_path);

    // Remove "src/" prefix and .rs extension
    let relative = path.strip_prefix("src").unwrap_or(path);
    let no_ext = relative.with_extension("");

    // Convert path separators to ::
    let module_path = no_ext.to_string_lossy()
        .replace(std::path::MAIN_SEPARATOR, "::")
        .replace('/', "::"); // Also handle forward slashes on Windows

    // Remove "lib" or "mod" suffixes (lib.rs, mod.rs)
    if module_path == "lib" || module_path == "mod" {
        String::new()
    } else {
        module_path
    }
}

/// Convert RustTag to ParsedItem
fn convert_tag_to_parsed_item(tag: &RustTag, crate_name: &str) -> Option<ParsedItem> {
    // Map TagKind to ParsedItemKind
    let kind = match tag.kind {
        TagKind::Function => ParsedItemKind::Function,
        TagKind::Struct => ParsedItemKind::Struct,
        TagKind::Enum => ParsedItemKind::Enum,
        TagKind::Trait => ParsedItemKind::Trait,
        TagKind::Constant => ParsedItemKind::Const,
        TagKind::Static => ParsedItemKind::Static,
        TagKind::TypeAlias => ParsedItemKind::TypeAlias,
        TagKind::Module => ParsedItemKind::Mod,
        TagKind::Impl => return None, // Skip impl blocks for Phase 1
        TagKind::Macro => ParsedItemKind::Macro,
    };

    // Compute full path
    let path = format!("{}::{}", crate_name, tag.name);

    // Map Visibility to ItemVisibility
    let (is_pub, visibility) = match &tag.visibility {
        Visibility::Public => (true, ItemVisibility::Public),
        Visibility::Private => (false, ItemVisibility::Private),
        Visibility::PublicCrate => (true, ItemVisibility::Crate),
        Visibility::PublicSuper => (true, ItemVisibility::Super),
        Visibility::PublicIn(path) => (true, ItemVisibility::InPath(path.clone())),
    };

    Some(ParsedItem {
        name: tag.name.clone(),
        path: path.clone(),
        kind,
        source: String::new(), // Not needed for Phase 1
        dependencies: HashSet::new(), // Will be filled in Phase 2
        typed_dependencies: Vec::new(),
        external_crates: HashSet::new(),
        module_path: String::new(),
        is_pub,
        visibility,
        file: std::path::PathBuf::from(&tag.file_path),
        line: None, // Not available from incremental graph tags
        cfg_attr: None,
        generics: Vec::new(),
    })
}

/// Build dependency graph from ctags-parsed items using text-based dependency extraction
fn build_graph_from_tags(items: &[ParsedItem]) -> Graph {
    use crate::slicer::text_dependencies::extract_dependencies_fast;
    use std::fs;

    let mut graph = HashMap::new();

    // Group items by file for efficient processing
    let mut items_by_file: HashMap<&std::path::Path, Vec<&ParsedItem>> = HashMap::new();
    for item in items {
        items_by_file.entry(item.file.as_path())
            .or_insert_with(Vec::new)
            .push(item);
    }

    // For each file, extract dependencies using text-based matching
    for (file_path, file_items) in items_by_file.iter() {
        // Read file contents
        let source = match fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Warning: Failed to read {}: {}", file_path.display(), e);
                // Fall back to empty dependencies for this file
                for item in file_items {
                    graph.insert(item.path.clone(), Vec::new());
                }
                continue;
            }
        };

        // Collect all item names for pattern matching
        let all_item_names: Vec<String> = items.iter()
            .map(|item| item.name.clone())
            .collect();

        // For each item in this file, extract its dependencies
        for item in file_items {
            // Use fast text-based dependency extraction
            let deps = extract_dependencies_fast(&source, &all_item_names);

            // Convert item names to full paths for graph
            let dep_paths: Vec<String> = deps.iter()
                .filter_map(|dep_name| {
                    // Find the item with this name
                    items.iter()
                        .find(|i| &i.name == dep_name)
                        .map(|i| i.path.clone())
                })
                .collect();

            graph.insert(item.path.clone(), dep_paths);
        }
    }

    graph
}

/// Build dependency graph using ctags (fast text-based parsing)
///
/// This is an alternative to build_graph_full() that uses Universal Ctags
/// for fast parsing instead of syn. It's 4-8x faster but may be less accurate.
///
/// COMMENTED OUT: requires walkdir dependency which is not declared in Cargo.toml
/*
#[allow(dead_code)]
pub fn build_graph_ctags(output_dir: &Path, crate_name: &str) -> Result<(Graph, CrateIndex, HashSet<String>), String> {
    // Find all .rs files in src/
    let src_dir = output_dir.join("src");
    let mut all_items = Vec::new();

    // Parse each file with ctags
    for entry in walkdir::WalkDir::new(&src_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "rs"))
    {
        let file_path = entry.path();
        let items = ctags_parser::parse_file_with_ctags(file_path, crate_name)?;
        all_items.extend(items);
    }

    // Build dependency graph using text-based extraction
    let item_sources: Vec<(String, String)> = all_items
        .iter()
        .map(|item| (item.name.clone(), item.source.clone()))
        .collect();

    let (internal_deps, external_crates) = text_dependencies::build_text_dependency_graph(&item_sources);

    // Update items with dependencies
    for item in &mut all_items {
        if let Some(deps) = internal_deps.get(&item.name) {
            item.dependencies = deps.clone();
        }
        item.external_crates = external_crates.clone();
    }

    // Convert to CrateIndex format
    let index = CrateIndex {
        items: HashMap::new(), // Will be filled if needed
        impls: HashMap::new(), // Will be filled if needed
        all_items: all_items.clone(),
        reexports: HashMap::new(),
        use_statements: Vec::new(), // Not needed for ctags-based approach
        internal_aliases: HashMap::new(),
        macro_use_crates: HashSet::new(),
    };

    // Build graph from items
    let graph = build_graph_from_ctags_items(&all_items);

    // Extract public re-exports (lightweight)
    let public_reexports = extract_public_reexports_lightweight(output_dir)?;

    Ok((graph, index, public_reexports))
}
*/

/// Build dependency graph from ctags-parsed items
/*
#[allow(dead_code)]
fn build_graph_from_ctags_items(items: &[ParsedItem]) -> Graph {
    let mut graph = HashMap::new();

    for item in items {
        let qualified_name = if item.module_path.is_empty() {
            item.name.clone()
        } else {
            format!("{}::{}", item.module_path, item.name)
        };

        let deps: Vec<String> = item.dependencies.iter().cloned().collect();
        graph.insert(qualified_name, deps);
    }

    graph
}
*/

/// Extract public re-exports from lib.rs (lightweight, no full crate parse)
///
/// This is used by incremental graph building to identify entry points
/// before parsing the entire crate.
fn extract_public_reexports_lightweight(output_dir: &Path) -> Result<HashSet<String>, String> {
    use std::fs;

    let mut reexports = HashSet::new();

    // Find lib.rs or main.rs
    let lib_rs = output_dir.join("src/lib.rs");
    let main_rs = output_dir.join("src/main.rs");

    let entry_file = if lib_rs.exists() {
        lib_rs
    } else if main_rs.exists() {
        main_rs
    } else {
        // No entry point found, return empty set
        return Ok(reexports);
    };

    // Read and parse lib.rs only
    let content = fs::read_to_string(&entry_file)
        .map_err(|e| format!("Failed to read {}: {}", entry_file.display(), e))?;

    let syntax = syn::parse_file(&content)
        .map_err(|e| format!("Failed to parse {}: {}", entry_file.display(), e))?;

    // Extract pub use statements
    for item in &syntax.items {
        if let syn::Item::Use(use_item) = item {
            // Check if it's public
            let is_public = matches!(use_item.vis, syn::Visibility::Public(_));

            if is_public {
                // Extract symbols from the use tree
                collect_use_symbols(&use_item.tree, &mut reexports);
            }
        }
    }

    Ok(reexports)
}

/// Recursively collect symbols from a use tree
fn collect_use_symbols(tree: &syn::UseTree, symbols: &mut HashSet<String>) {
    match tree {
        syn::UseTree::Path(path) => {
            collect_use_symbols(&path.tree, symbols);
        }
        syn::UseTree::Name(name) => {
            symbols.insert(name.ident.to_string());
        }
        syn::UseTree::Rename(rename) => {
            symbols.insert(rename.rename.to_string());
        }
        syn::UseTree::Glob(_) => {
            // pub use foo::*; - can't know what symbols are re-exported
            // This is handled conservatively by marking all items in the module
        }
        syn::UseTree::Group(group) => {
            for tree in &group.items {
                collect_use_symbols(tree, symbols);
            }
        }
    }
}

/// Extract items that are publicly re-exported in lib.rs
/// These items are part of the crate's public API and should be kept
fn extract_public_reexports(index: &CrateIndex) -> HashSet<String> {
    use std::collections::HashSet;
    let mut reexports = HashSet::new();

    // Find use statements in the root module (lib.rs or main.rs)
    for use_stmt in &index.use_statements {
        if use_stmt.module_path.is_empty() {
            // This is a use statement in lib.rs
            // Check if it's a crate-internal re-export (use crate::, use self::)
            if use_stmt.source.contains("use crate::") || use_stmt.source.contains("use self::") {
                // Check if it's public (pub use)
                if use_stmt.source.trim().starts_with("pub use") {
                    // Add all symbols being re-exported
                    reexports.extend(use_stmt.symbols.iter().cloned());
                }
            }
        }
    }

    reexports
}

/// Extract dependencies from a parsed crate index
fn build_graph_from_index(index: &CrateIndex) -> Graph {
    let mut graph = HashMap::new();

    // Build a map of module_path -> imported names from use statements
    let mut module_imports: HashMap<String, Vec<String>> = HashMap::new();

    for use_stmt in &index.use_statements {
        // Use the symbols field which already contains the imported names
        // e.g., for "use crate::either::Either;" symbols = ["Either"]
        let module_path = use_stmt.module_path.clone();
        module_imports.entry(module_path).or_default().extend(use_stmt.symbols.iter().cloned());
    }

    for item in &index.all_items {
        // Use qualified names (module::name) to avoid collisions when multiple items
        // have the same name in different modules (e.g., multiple "Parser" items)
        let qualified_name = if item.module_path.is_empty() {
            item.name.clone()
        } else {
            format!("{}::{}", item.module_path, item.name)
        };

        let mut deps = extract_item_dependencies(item);

        // Add imports from use statements in the same module
        // This ensures that imported items become dependencies in the graph
        if let Some(imports) = module_imports.get(&item.module_path) {
            deps.extend(imports.iter().cloned());
        }

        graph.insert(qualified_name, deps);
    }

    graph
}

/// Extract what an item depends on (enhanced to include body-level references)
fn extract_item_dependencies(item: &ParsedItem) -> Vec<String> {
    let mut deps = HashSet::new();

    // Extract from item.dependencies (already computed by old parser - type-level deps)
    for dep in &item.dependencies {
        deps.insert(dep.clone());
    }

    // Also scan the item source for body-level references (constants, functions, types)
    // This is critical for catching private items used inside function bodies
    // e.g., LOG_LEVEL_NAMES used inside Level::from_str()
    let body_refs = extract_body_references(&item.source);
    for ref_name in body_refs {
        deps.insert(ref_name);
    }

    deps.into_iter().collect()
}

/// Extract identifier references from item source code
/// Catches both types (uppercase) and values (constants, functions)
fn extract_body_references(source: &str) -> HashSet<String> {
    let mut refs = HashSet::new();

    // Tokenize by non-identifier characters
    for word in source.split(|c: char| !c.is_alphanumeric() && c != '_') {
        let word = word.trim();

        // Skip empty, too short, keywords, or starting with digit
        if word.len() < 2 || word.starts_with(char::is_numeric) {
            continue;
        }

        // Skip Rust keywords
        if RUST_KEYWORDS.contains(&word) {
            continue;
        }

        // Skip primitive types
        if is_primitive_type(word) {
            continue;
        }

        // Include identifiers that look like:
        // - Constants: ALL_CAPS or MIXED_CAPS (e.g., LOG_LEVEL_NAMES, HEX_CHARS_LOWER)
        // - Types: PascalCase (e.g., BytesToHexChars, MaybeStaticStr)
        // - Functions: snake_case (e.g., set_times, encode_to_iter)
        let first_char = word.chars().next().unwrap();
        if first_char.is_alphabetic() {
            refs.insert(word.to_string());
        }
    }

    refs
}

/// Check if a type name is a primitive
fn is_primitive_type(name: &str) -> bool {
    matches!(name,
        "bool" | "char" | "str" | "String" |
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" |
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
        "f32" | "f64" |
        "Vec" | "Box" | "Option" | "Result" | "Some" | "None" | "Ok" | "Err" |
        "Self" | "self" | "true" | "false"
    )
}

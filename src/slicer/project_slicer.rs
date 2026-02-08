//! Project slicing orchestration
//!
//! This module coordinates slicing the project itself (not just dependencies).
//! It uses root set detection to determine necessary items, then applies the
//! same dependency graph + marking + deletion logic used for dependencies.

use std::path::{Path, PathBuf};
use std::collections::{HashSet, HashMap};
use std::fs;
use syn::Item;
use super::root_set::{self, ProjectType};
use super::config::SlicerConfig;
use crate::types::{ParsedItem, ItemVisibility, ParsedItemKind};
use crate::constants::RUST_KEYWORDS;

/// Result of project slicing
#[derive(Debug)]
pub struct ProjectSliceResult {
    /// Number of root items (entry points or public items)
    pub root_items: usize,
    /// Number of items marked as necessary
    pub necessary_items: usize,
    /// Number of items deleted
    pub items_deleted: usize,
    /// Lines of code before slicing
    pub loc_before: usize,
    /// Lines of code after slicing
    pub loc_after: usize,
}

/// Slice the project itself based on root set analysis
///
/// This implementation:
/// 1. Builds a dependency graph of all items in the project
/// 2. Marks items reachable from the root set (BFS traversal)
/// 3. Deletes unmarked items from the AST
/// 4. Writes modified files to output directory
pub fn slice_project(
    project_root: &Path,
    config: &SlicerConfig,
) -> Result<ProjectSliceResult, Box<dyn std::error::Error>> {
    if config.verbose {
        println!("\n=== Project Slicing ===");
        println!("  Project: {}", project_root.display());
    }

    // Step 1: Determine project type
    let project_type = root_set::determine_project_type(project_root)?;

    if config.verbose {
        match &project_type {
            ProjectType::Binary(paths) => {
                println!("  Type: Binary ({} entry point(s))", paths.len());
            }
            ProjectType::Library(_) => {
                println!("  Type: Library");
            }
            ProjectType::Both { binaries, .. } => {
                println!("  Type: Both (library + {} binary/binaries)", binaries.len());
            }
        }
    }

    // Step 2: Extract root items
    let root_items = root_set::extract_root_items(&project_type)?;

    if config.verbose {
        println!("  Root items found: {}", root_items.len());
        if root_items.len() <= 10 {
            for item in &root_items {
                println!("    - {} {} ({})", item.kind, item.name, item.file_path.display());
            }
        }
    }

    // Step 3: Find all source files
    let src_dir = project_root.join("src");
    let all_files = root_set::find_all_source_files(&src_dir)?;

    if config.verbose {
        println!("  Source files: {}", all_files.len());
    }

    // Step 4: Parse all files and build dependency graph
    if config.verbose {
        println!("  Building dependency graph...");
    }

    let mut all_items = Vec::new();
    let mut file_contents: HashMap<PathBuf, String> = HashMap::new();

    for file_path in &all_files {
        let content = fs::read_to_string(file_path)?;
        let parsed_items = parse_file(&content, file_path)?;
        all_items.extend(parsed_items);
        file_contents.insert(file_path.clone(), content);
    }

    if config.verbose {
        println!("    Parsed {} items from {} files", all_items.len(), all_files.len());
    }

    // Build dependency graph
    let graph = build_dependency_graph(&all_items);

    // Step 5: Mark necessary items via BFS from root set
    if config.verbose {
        println!("  Marking necessary items...");
    }

    let necessary_items = mark_necessary_items(&root_items, &graph, &all_items, &project_type)?;

    if config.verbose {
        println!("    Marked {} items as necessary", necessary_items.len());
    }

    // Step 6: Count LOC before
    let loc_before: usize = file_contents.values()
        .map(|content| content.lines().filter(|l| !l.trim().is_empty()).count())
        .sum();

    // Step 7: Delete unnecessary items from each file
    if config.verbose {
        println!("  Deleting unnecessary items...");
    }

    let mut deleted_count = 0;
    let output_dir = project_root.join("src_sliced");

    // Create output directory
    if output_dir.exists() {
        fs::remove_dir_all(&output_dir)?;
    }
    fs::create_dir_all(&output_dir)?;

    for file_path in &all_files {
        let content = file_contents.get(file_path).unwrap();
        let file_items: Vec<_> = all_items.iter()
            .filter(|item| &item.file == file_path)
            .collect();

        let (new_content, deleted) = delete_unnecessary_items(
            content,
            &file_items,
            &necessary_items,
        )?;

        deleted_count += deleted;

        // Write to output directory, preserving structure relative to src/
        let relative_path = file_path.strip_prefix(&src_dir)?;
        let output_path = output_dir.join(relative_path);

        // Create parent directories if needed
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        fs::write(&output_path, new_content)?;
    }

    // Step 8: Count LOC after
    let loc_after = root_set::find_all_source_files(&output_dir)?
        .iter()
        .filter_map(|f| fs::read_to_string(f).ok())
        .map(|content| content.lines().filter(|l| !l.trim().is_empty()).count())
        .sum();

    // Print results
    println!("\n✅ Project slicing completed!");
    println!("  Output directory: {}", output_dir.display());
    println!("  Root items: {}", root_items.len());
    println!("  Necessary items: {}", necessary_items.len());
    println!("  Deleted items: {}", deleted_count);
    println!("  LOC: {} → {} ({:.1}% reduction)",
        loc_before,
        loc_after,
        if loc_before > 0 {
            ((loc_before - loc_after) as f64 / loc_before as f64) * 100.0
        } else {
            0.0
        }
    );

    Ok(ProjectSliceResult {
        root_items: root_items.len(),
        necessary_items: necessary_items.len(),
        items_deleted: deleted_count,
        loc_before,
        loc_after,
    })
}

/// Parse a single file and extract items
fn parse_file(content: &str, file_path: &Path) -> Result<Vec<ParsedItem>, Box<dyn std::error::Error>> {
    let syntax = syn::parse_file(content)?;
    let mut items = Vec::new();

    for item in &syntax.items {
        if let Some(parsed_item) = parse_item(item, file_path, content) {
            items.push(parsed_item);
        }
    }

    Ok(items)
}

/// Parse a single syn::Item into a ParsedItem
fn parse_item(item: &Item, file_path: &Path, _source: &str) -> Option<ParsedItem> {
    match item {
        Item::Fn(func) => {
            Some(ParsedItem {
                name: func.sig.ident.to_string(),
                path: func.sig.ident.to_string(),
                kind: ParsedItemKind::Function,
                source: quote::quote!(#func).to_string(),
                dependencies: extract_dependencies(&func.block.stmts),
                typed_dependencies: vec![],
                external_crates: HashSet::new(),
                module_path: String::new(),
                is_pub: matches!(func.vis, syn::Visibility::Public(_)),
                visibility: match &func.vis {
                    syn::Visibility::Public(_) => ItemVisibility::Public,
                    _ => ItemVisibility::Private,
                },
                file: file_path.to_path_buf(),
                line: None, // Not available from syn parser
                cfg_attr: None,
                generics: vec![],
            })
        }
        Item::Struct(s) => {
            Some(ParsedItem {
                name: s.ident.to_string(),
                path: s.ident.to_string(),
                kind: ParsedItemKind::Struct,
                source: quote::quote!(#s).to_string(),
                dependencies: HashSet::new(),
                typed_dependencies: vec![],
                external_crates: HashSet::new(),
                module_path: String::new(),
                is_pub: matches!(s.vis, syn::Visibility::Public(_)),
                visibility: match &s.vis {
                    syn::Visibility::Public(_) => ItemVisibility::Public,
                    _ => ItemVisibility::Private,
                },
                file: file_path.to_path_buf(),
                line: None, // Not available from syn parser
                cfg_attr: None,
                generics: vec![],
            })
        }
        Item::Enum(e) => {
            Some(ParsedItem {
                name: e.ident.to_string(),
                path: e.ident.to_string(),
                kind: ParsedItemKind::Enum,
                source: quote::quote!(#e).to_string(),
                dependencies: HashSet::new(),
                typed_dependencies: vec![],
                external_crates: HashSet::new(),
                module_path: String::new(),
                is_pub: matches!(e.vis, syn::Visibility::Public(_)),
                visibility: match &e.vis {
                    syn::Visibility::Public(_) => ItemVisibility::Public,
                    _ => ItemVisibility::Private,
                },
                file: file_path.to_path_buf(),
                line: None, // Not available from syn parser
                cfg_attr: None,
                generics: vec![],
            })
        }
        Item::Trait(t) => {
            Some(ParsedItem {
                name: t.ident.to_string(),
                path: t.ident.to_string(),
                kind: ParsedItemKind::Trait,
                source: quote::quote!(#t).to_string(),
                dependencies: HashSet::new(),
                typed_dependencies: vec![],
                external_crates: HashSet::new(),
                module_path: String::new(),
                is_pub: matches!(t.vis, syn::Visibility::Public(_)),
                visibility: match &t.vis {
                    syn::Visibility::Public(_) => ItemVisibility::Public,
                    _ => ItemVisibility::Private,
                },
                file: file_path.to_path_buf(),
                line: None, // Not available from syn parser
                cfg_attr: None,
                generics: vec![],
            })
        }
        Item::Const(c) => {
            Some(ParsedItem {
                name: c.ident.to_string(),
                path: c.ident.to_string(),
                kind: ParsedItemKind::Const,
                source: quote::quote!(#c).to_string(),
                dependencies: HashSet::new(),
                typed_dependencies: vec![],
                external_crates: HashSet::new(),
                module_path: String::new(),
                is_pub: matches!(c.vis, syn::Visibility::Public(_)),
                visibility: match &c.vis {
                    syn::Visibility::Public(_) => ItemVisibility::Public,
                    _ => ItemVisibility::Private,
                },
                file: file_path.to_path_buf(),
                line: None, // Not available from syn parser
                cfg_attr: None,
                generics: vec![],
            })
        }
        _ => None,
    }
}

/// Extract dependencies from statements using proper AST walking
fn extract_dependencies(stmts: &[syn::Stmt]) -> HashSet<String> {
    let mut deps = HashSet::new();

    for stmt in stmts {
        extract_deps_from_stmt(stmt, &mut deps);
    }

    deps
}

/// Extract dependencies from a statement
fn extract_deps_from_stmt(stmt: &syn::Stmt, deps: &mut HashSet<String>) {
    match stmt {
        syn::Stmt::Local(local) => {
            if let Some(init) = &local.init {
                extract_deps_from_expr(&init.expr, deps);
            }
        }
        syn::Stmt::Expr(expr, _) => {
            extract_deps_from_expr(expr, deps);
        }
        syn::Stmt::Item(_) => {}
        syn::Stmt::Macro(m) => {
            // Extract from macro arguments
            let tokens = m.mac.tokens.to_string();
            for word in tokens.split(|c: char| !c.is_alphanumeric() && c != '_') {
                let word = word.trim();
                if word.len() >= 2 && !RUST_KEYWORDS.contains(&word) {
                    deps.insert(word.to_string());
                }
            }
        }
    }
}

/// Extract dependencies from an expression
fn extract_deps_from_expr(expr: &syn::Expr, deps: &mut HashSet<String>) {
    match expr {
        syn::Expr::Path(path) => {
            // Extract the last segment of the path
            if let Some(segment) = path.path.segments.last() {
                deps.insert(segment.ident.to_string());
            }
        }
        syn::Expr::Call(call) => {
            extract_deps_from_expr(&call.func, deps);
            for arg in &call.args {
                extract_deps_from_expr(arg, deps);
            }
        }
        syn::Expr::MethodCall(method) => {
            deps.insert(method.method.to_string());
            extract_deps_from_expr(&method.receiver, deps);
            for arg in &method.args {
                extract_deps_from_expr(arg, deps);
            }
        }
        syn::Expr::Binary(binary) => {
            extract_deps_from_expr(&binary.left, deps);
            extract_deps_from_expr(&binary.right, deps);
        }
        syn::Expr::Unary(unary) => {
            extract_deps_from_expr(&unary.expr, deps);
        }
        syn::Expr::Field(field) => {
            extract_deps_from_expr(&field.base, deps);
        }
        syn::Expr::Index(index) => {
            extract_deps_from_expr(&index.expr, deps);
            extract_deps_from_expr(&index.index, deps);
        }
        syn::Expr::Tuple(tuple) => {
            for elem in &tuple.elems {
                extract_deps_from_expr(elem, deps);
            }
        }
        syn::Expr::Array(array) => {
            for elem in &array.elems {
                extract_deps_from_expr(elem, deps);
            }
        }
        syn::Expr::Struct(s) => {
            if let Some(segment) = s.path.segments.last() {
                deps.insert(segment.ident.to_string());
            }
            for field in &s.fields {
                extract_deps_from_expr(&field.expr, deps);
            }
        }
        syn::Expr::Block(block) => {
            for stmt in &block.block.stmts {
                extract_deps_from_stmt(stmt, deps);
            }
        }
        syn::Expr::If(if_expr) => {
            extract_deps_from_expr(&if_expr.cond, deps);
            for stmt in &if_expr.then_branch.stmts {
                extract_deps_from_stmt(stmt, deps);
            }
            if let Some((_, else_branch)) = &if_expr.else_branch {
                extract_deps_from_expr(else_branch, deps);
            }
        }
        syn::Expr::Match(match_expr) => {
            extract_deps_from_expr(&match_expr.expr, deps);
            for arm in &match_expr.arms {
                extract_deps_from_expr(&arm.body, deps);
            }
        }
        syn::Expr::Loop(loop_expr) => {
            for stmt in &loop_expr.body.stmts {
                extract_deps_from_stmt(stmt, deps);
            }
        }
        syn::Expr::While(while_expr) => {
            extract_deps_from_expr(&while_expr.cond, deps);
            for stmt in &while_expr.body.stmts {
                extract_deps_from_stmt(stmt, deps);
            }
        }
        syn::Expr::ForLoop(for_loop) => {
            extract_deps_from_expr(&for_loop.expr, deps);
            for stmt in &for_loop.body.stmts {
                extract_deps_from_stmt(stmt, deps);
            }
        }
        syn::Expr::Return(ret) => {
            if let Some(expr) = &ret.expr {
                extract_deps_from_expr(expr, deps);
            }
        }
        syn::Expr::Try(try_expr) => {
            extract_deps_from_expr(&try_expr.expr, deps);
        }
        syn::Expr::Await(await_expr) => {
            extract_deps_from_expr(&await_expr.base, deps);
        }
        syn::Expr::Assign(assign) => {
            extract_deps_from_expr(&assign.left, deps);
            extract_deps_from_expr(&assign.right, deps);
        }
        syn::Expr::Reference(reference) => {
            extract_deps_from_expr(&reference.expr, deps);
        }
        syn::Expr::Paren(paren) => {
            extract_deps_from_expr(&paren.expr, deps);
        }
        syn::Expr::Cast(cast) => {
            extract_deps_from_expr(&cast.expr, deps);
        }
        syn::Expr::Macro(m) => {
            // Extract from macro tokens
            let tokens = m.mac.tokens.to_string();
            for word in tokens.split(|c: char| !c.is_alphanumeric() && c != '_') {
                let word = word.trim();
                if word.len() >= 2 && !RUST_KEYWORDS.contains(&word) {
                    deps.insert(word.to_string());
                }
            }
        }
        _ => {
            // For other expression types, fall back to string tokenization
            let source = quote::quote!(#expr).to_string();
            for word in source.split(|c: char| !c.is_alphanumeric() && c != '_') {
                let word = word.trim();
                if word.len() >= 2 && !RUST_KEYWORDS.contains(&word) {
                    deps.insert(word.to_string());
                }
            }
        }
    }
}

/// Build dependency graph from parsed items
fn build_dependency_graph(items: &[ParsedItem]) -> HashMap<String, Vec<String>> {
    let mut graph = HashMap::new();

    for item in items {
        let deps: Vec<String> = item.dependencies.iter().cloned().collect();
        graph.insert(item.name.clone(), deps);
    }

    graph
}

/// Mark necessary items using BFS from root set
fn mark_necessary_items(
    root_items: &HashSet<root_set::RootItem>,
    graph: &HashMap<String, Vec<String>>,
    all_items: &[ParsedItem],
    project_type: &ProjectType,
) -> Result<HashSet<String>, Box<dyn std::error::Error>> {
    use std::collections::VecDeque;

    let mut necessary = HashSet::new();
    let mut queue = VecDeque::new();

    // Add all root items to queue
    for root in root_items {
        necessary.insert(root.name.clone());
        queue.push_back(root.name.clone());
    }

    // For library projects, also mark all public items as necessary (they're part of the public API)
    // For binary projects, only mark items reachable from main()
    let is_library = matches!(project_type, ProjectType::Library(_) | ProjectType::Both { .. });

    if is_library {
        for item in all_items {
            if item.is_pub {
                necessary.insert(item.name.clone());
                queue.push_back(item.name.clone());
            }
        }
    }

    // BFS traversal
    while let Some(current) = queue.pop_front() {
        if let Some(deps) = graph.get(&current) {
            for dep in deps {
                if !necessary.contains(dep) {
                    necessary.insert(dep.clone());
                    queue.push_back(dep.clone());
                }
            }
        }
    }
    Ok(necessary)
}

/// Delete unnecessary items from file content
fn delete_unnecessary_items(
    content: &str,
    file_items: &[&ParsedItem],
    necessary: &HashSet<String>,
) -> Result<(String, usize), Box<dyn std::error::Error>> {
    let syntax = syn::parse_file(content)?;
    let mut kept_items = Vec::new();
    let mut deleted_count = 0;

    for item in &syntax.items {
        if should_keep_item(item, file_items, necessary) {
            kept_items.push(item.clone());
        } else {
            deleted_count += 1;
        }
    }

    // Rebuild file with only kept items
    let file = syn::File {
        shebang: syntax.shebang.clone(),
        attrs: syntax.attrs.clone(),
        items: kept_items,
    };

    let new_content = prettyplease::unparse(&file);

    Ok((new_content, deleted_count))
}

/// Check if an item should be kept
fn should_keep_item(
    item: &Item,
    _file_items: &[&ParsedItem],
    necessary: &HashSet<String>,
) -> bool {
    let item_name = match item {
        Item::Fn(f) => f.sig.ident.to_string(),
        Item::Struct(s) => s.ident.to_string(),
        Item::Enum(e) => e.ident.to_string(),
        Item::Trait(t) => t.ident.to_string(),
        Item::Const(c) => c.ident.to_string(),
        Item::Type(t) => t.ident.to_string(),
        Item::Static(s) => s.ident.to_string(),
        // Keep use statements, modules, and other items by default
        _ => return true,
    };

    // Only keep items in the necessary set
    // (mark_necessary_items already handles public items for libraries)
    necessary.contains(&item_name)
}

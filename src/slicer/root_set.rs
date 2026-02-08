//! Root set detection for project slicing
//!
//! This module determines the "root set" of necessary items for a Rust project:
//! - For binaries: main() entry points
//! - For libraries: all pub (public) items
//!
//! The root set is used as the starting point for reachability analysis to
//! determine which items are actually necessary vs. can be safely deleted.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

/// Type of Rust project
#[derive(Debug, Clone)]
pub enum ProjectType {
    /// Binary with entry point paths
    Binary(Vec<PathBuf>),
    /// Library with lib.rs path
    Library(PathBuf),
    /// Both binary and library
    Both {
        binaries: Vec<PathBuf>,
        library: PathBuf,
    },
}

/// A root item that must be kept
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RootItem {
    /// File where this item is defined
    pub file_path: PathBuf,
    /// Name of the item (e.g., "main", "MyStruct")
    pub name: String,
    /// Kind of item (e.g., "fn", "struct", "enum", "trait")
    pub kind: String,
    /// Whether this is an entry point (main function)
    pub is_entry_point: bool,
}

/// Determine the type of Rust project
pub fn determine_project_type(project_path: &Path) -> Result<ProjectType, Box<dyn std::error::Error>> {
    let mut binaries = Vec::new();
    let mut library = None;

    // Check for src/main.rs (binary)
    let main_path = project_path.join("src/main.rs");
    if main_path.exists() {
        binaries.push(main_path);
    }

    // Check for src/lib.rs (library)
    let lib_path = project_path.join("src/lib.rs");
    if lib_path.exists() {
        library = Some(lib_path);
    }

    // Check for additional binaries in src/bin/
    let bin_dir = project_path.join("src/bin");
    if bin_dir.exists() && bin_dir.is_dir() {
        for entry in fs::read_dir(&bin_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                binaries.push(path);
            }
        }
    }

    // Determine project type
    match (binaries.is_empty(), library) {
        (true, None) => Err("No src/main.rs or src/lib.rs found".into()),
        (true, Some(lib)) => Ok(ProjectType::Library(lib)),
        (false, None) => Ok(ProjectType::Binary(binaries)),
        (false, Some(lib)) => Ok(ProjectType::Both {
            binaries,
            library: lib,
        }),
    }
}

/// Extract root items based on project type
pub fn extract_root_items(
    project_type: &ProjectType,
) -> Result<HashSet<RootItem>, Box<dyn std::error::Error>> {
    match project_type {
        ProjectType::Binary(paths) => {
            // For binaries, root set is main() functions
            let mut roots = HashSet::new();
            for path in paths {
                roots.extend(find_main_functions(path)?);
            }
            Ok(roots)
        }
        ProjectType::Library(path) => {
            // For libraries, root set is all public items
            find_public_items(path)
        }
        ProjectType::Both { binaries, library } => {
            // For both, union of main() and public items
            let mut roots = HashSet::new();
            for path in binaries {
                roots.extend(find_main_functions(path)?);
            }
            roots.extend(find_public_items(library)?);
            Ok(roots)
        }
    }
}

/// Find all main() functions in a file
fn find_main_functions(file_path: &Path) -> Result<HashSet<RootItem>, Box<dyn std::error::Error>> {
    let content = fs::read_to_string(file_path)?;
    let syntax = syn::parse_file(&content)?;

    let mut mains = HashSet::new();

    for item in &syntax.items {
        if let syn::Item::Fn(func) = item {
            if func.sig.ident == "main" {
                mains.insert(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: "main".to_string(),
                    kind: "fn".to_string(),
                    is_entry_point: true,
                });
            }
        }
    }

    Ok(mains)
}

/// Find all public items in a file (for library root set)
fn find_public_items(file_path: &Path) -> Result<HashSet<RootItem>, Box<dyn std::error::Error>> {
    let content = fs::read_to_string(file_path)?;
    let syntax = syn::parse_file(&content)?;

    let mut public_items = HashSet::new();

    for item in &syntax.items {
        if let Some(root_item) = extract_public_item(item, file_path) {
            public_items.insert(root_item);
        }
    }

    Ok(public_items)
}

/// Extract a public item from syn::Item
fn extract_public_item(item: &syn::Item, file_path: &Path) -> Option<RootItem> {
    match item {
        syn::Item::Fn(func) => {
            if matches!(func.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: func.sig.ident.to_string(),
                    kind: "fn".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        syn::Item::Struct(s) => {
            if matches!(s.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: s.ident.to_string(),
                    kind: "struct".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        syn::Item::Enum(e) => {
            if matches!(e.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: e.ident.to_string(),
                    kind: "enum".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        syn::Item::Trait(t) => {
            if matches!(t.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: t.ident.to_string(),
                    kind: "trait".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        syn::Item::Type(t) => {
            if matches!(t.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: t.ident.to_string(),
                    kind: "type".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        syn::Item::Const(c) => {
            if matches!(c.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: c.ident.to_string(),
                    kind: "const".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        syn::Item::Static(s) => {
            if matches!(s.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: s.ident.to_string(),
                    kind: "static".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        syn::Item::Mod(m) => {
            if matches!(m.vis, syn::Visibility::Public(_)) {
                Some(RootItem {
                    file_path: file_path.to_path_buf(),
                    name: m.ident.to_string(),
                    kind: "mod".to_string(),
                    is_entry_point: false,
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Find all Rust source files in a directory recursively
pub fn find_all_source_files(dir: &Path) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let mut files = Vec::new();

    if !dir.exists() || !dir.is_dir() {
        return Ok(files);
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            // Recursively search subdirectories
            files.extend(find_all_source_files(&path)?);
        } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            files.push(path);
        }
    }

    Ok(files)
}

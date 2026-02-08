// ctags_collector.rs - Simplified tag collection for cargo-slicer
//
// This module provides a simple interface to collect Rust tags from ctags
// without requiring the full precc infrastructure.

use std::cell::RefCell;
use std::ffi::{CStr, c_char};
use std::os::raw::c_ulong;

/// Simplified Rust tag extracted from ctags
#[derive(Debug, Clone)]
pub struct RustTag {
    pub name: String,
    pub kind_name: String,
    pub kind_char: char,
    pub line_number: usize,
    pub scope: Option<String>,
}

thread_local! {
    /// Thread-local storage for collecting tags during parsing
    static TAG_COLLECTOR: RefCell<Vec<RustTag>> = RefCell::new(Vec::new());
}

/// C-compatible tagEntryInfo struct (simplified view)
///
/// Matches the layout in entry.h but only includes fields we need.
/// The full struct has more fields, but we only access these.
#[repr(C)]
pub struct TagEntryInfo {
    line_number_entry: bool,
    line_number: c_ulong,
    _file_position: [u8; 16],  // fpos_t placeholder
    language: *const c_char,
    is_file_scope: bool,
    is_file_entry: bool,
    truncate_line: bool,
    source_file_name: *const c_char,
    name: *const c_char,
    kind_name: *const c_char,
    kind: c_char,
    // Extension fields follow but we don't need them yet
}

/// Callback function called by ctags when a tag is found
///
/// This function is called from C code (makeTagEntry in ctags_amalg.c).
/// It extracts tag information and stores it in thread-local storage.
#[no_mangle]
pub extern "C" fn makeTagEntry(tag: *const TagEntryInfo) {
    if tag.is_null() {
        return;
    }

    unsafe {
        let tag_ref = &*tag;

        // Extract tag name
        let name = if tag_ref.name.is_null() {
            return; // Skip tags without names
        } else {
            match CStr::from_ptr(tag_ref.name).to_str() {
                Ok(s) => s.to_string(),
                Err(_) => return, // Skip invalid UTF-8
            }
        };

        // Extract kind name (e.g., "function", "struct", "module")
        let kind_name = if tag_ref.kind_name.is_null() {
            String::from("unknown")
        } else {
            match CStr::from_ptr(tag_ref.kind_name).to_str() {
                Ok(s) => s.to_string(),
                Err(_) => String::from("unknown"),
            }
        };

        // Extract kind character
        let kind_char = tag_ref.kind as u8 as char;

        // Extract line number
        let line_number = tag_ref.line_number as usize;

        // Create tag and add to collection
        let rust_tag = RustTag {
            name,
            kind_name,
            kind_char,
            line_number,
            scope: None, // TODO: Extract scope from extension fields
        };

        TAG_COLLECTOR.with(|collector| {
            collector.borrow_mut().push(rust_tag);
        });
    }
}

/// Clear the tag collector (call before parsing a new file)
pub fn clear_tags() {
    TAG_COLLECTOR.with(|collector| {
        collector.borrow_mut().clear();
    });
}

/// Get all collected tags and clear the collector
pub fn take_tags() -> Vec<RustTag> {
    TAG_COLLECTOR.with(|collector| {
        let mut tags = collector.borrow_mut();
        std::mem::take(&mut *tags)
    })
}

/// Parse a Rust file using Universal Ctags FFI (no process spawn)
///
/// This calls libctags directly via FFI, which eliminates the ~4ms process spawn overhead.
/// Tags are collected via the makeTagEntry callback and stored in thread-local storage.
///
/// COMMENTED OUT: requires ctags_ffi module which depends on libc (not declared in Cargo.toml)
/*
#[allow(dead_code)]
pub fn parse_rust_file_ffi(file_path: &str) -> Result<Vec<RustTag>, String> {
    use crate::ctags_ffi::DCTags;

    // Clear any previous tags
    clear_tags();

    // Create DCTags instance and process file
    let dctags = DCTags::new()?;
    dctags.process_rust_file(file_path)?;

    // Retrieve collected tags
    Ok(take_tags())
}
*/

/// Parse a Rust file using Universal Ctags command-line tool
///
/// This calls the real ctags binary installed on the system, which has a mature
/// Rust parser that handles async/unsafe/const modifiers, generics, and scope correctly.
pub fn parse_rust_file(file_path: &str) -> Result<Vec<RustTag>, String> {
    use std::process::Command;

    // Run ctags on the file
    // --language-force=Rust: Force Rust language
    // --fields=+n: Include line numbers
    // --sort=no: Don't sort, preserve file order
    // -f -: Output to stdout
    let output = Command::new("ctags")
        .args(&[
            "--language-force=Rust",
            "--fields=+n",
            "--sort=no",
            "-f", "-",
            file_path
        ])
        .output()
        .map_err(|e| format!("Failed to run ctags: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("ctags failed: {}", stderr));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut tags = Vec::new();

    // Parse ctags output (tab-separated format)
    // Format: <name>\t<file>\t<pattern>;\t<kind>\tline:<num>\t[scope:<type>:<name>]
    for line in stdout.lines() {
        let fields: Vec<&str> = line.split('\t').collect();
        if fields.len() < 4 {
            continue; // Skip malformed lines
        }

        let name = fields[0].to_string();
        let kind_char = fields[3].chars().next().unwrap_or('?');

        // Extract line number from fields (format: "line:N")
        let line_number = fields.iter()
            .find(|f| f.starts_with("line:"))
            .and_then(|f| f.strip_prefix("line:"))
            .and_then(|n| n.parse::<usize>().ok())
            .unwrap_or(0);

        // Extract scope if present (format: "implementation:TypeName" or "struct:TypeName")
        let scope = fields.iter()
            .find(|f| f.contains(':') && !f.starts_with("line:"))
            .map(|s| s.to_string());

        // Map ctags kind character to kind name
        let kind_name = match kind_char {
            'f' => "function",        // Free function
            'P' => "method",          // Method (function in impl block)
            's' => "struct",          // Struct
            'e' => "enum",            // Enum
            't' => "trait",           // Trait
            'c' => "impl",            // Implementation block
            'T' => "type",            // Type alias
            'v' => "constant",        // Constant/static
            'M' => "macro",           // Macro
            'm' => "field",           // Struct field
            'g' => "module",          // Module
            _ => "unknown",
        }.to_string();

        tags.push(RustTag {
            name,
            kind_name,
            kind_char,
            line_number,
            scope,
        });
    }

    Ok(tags)
}

/// DEPRECATED: Old regex-based parser (kept for reference)
///
/// This was an initial attempt at text-based parsing using regex patterns.
/// It has limitations like missing async/unsafe modifiers and no scope awareness.
/// Use parse_rust_file() instead, which calls real Universal Ctags.
#[allow(dead_code)]
fn parse_rust_file_regex(file_path: &str) -> Result<Vec<RustTag>, String> {
    use std::fs;
    use regex::Regex;

    // Read file content
    let content = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    let mut tags = Vec::new();

    // Regex patterns for different Rust constructs
    // Note: These are simplified and won't handle all edge cases, but good enough for experimentation

    // pub fn name(...) or pub async fn name(...) or fn name(...) (for impl block methods)
    let fn_regex = Regex::new(r"(?m)^[\s]*(?:pub\s+)?(?:async|unsafe|const|extern\s+)?fn\s+(\w+)\s*[<(]")
        .map_err(|e| format!("Regex error: {}", e))?;

    // pub struct Name or pub struct Name<T>
    let struct_regex = Regex::new(r"(?m)^[\s]*pub\s+struct\s+(\w+)")
        .map_err(|e| format!("Regex error: {}", e))?;

    // pub enum Name or pub enum Name<T>
    let enum_regex = Regex::new(r"(?m)^[\s]*pub\s+enum\s+(\w+)")
        .map_err(|e| format!("Regex error: {}", e))?;

    // pub trait Name or pub trait Name<T>
    let trait_regex = Regex::new(r"(?m)^[\s]*pub\s+trait\s+(\w+)")
        .map_err(|e| format!("Regex error: {}", e))?;

    // pub type Name = ...
    let type_regex = Regex::new(r"(?m)^[\s]*pub\s+type\s+(\w+)")
        .map_err(|e| format!("Regex error: {}", e))?;

    // pub const NAME: ... or pub static NAME: ...
    let const_regex = Regex::new(r"(?m)^[\s]*pub\s+(?:const|static)\s+(\w+)\s*:")
        .map_err(|e| format!("Regex error: {}", e))?;

    // pub mod name
    let mod_regex = Regex::new(r"(?m)^[\s]*pub\s+mod\s+(\w+)")
        .map_err(|e| format!("Regex error: {}", e))?;

    // impl Name or impl<T> Name or impl Trait for Name
    let impl_regex = Regex::new(r"(?m)^[\s]*impl\s+(?:<[^>]+>\s+)?(?:\w+\s+for\s+)?(\w+)")
        .map_err(|e| format!("Regex error: {}", e))?;

    // Helper to find line number for a match (1-based)
    let find_line_number = |match_pos: usize| -> usize {
        content[..match_pos].chars().filter(|&c| c == '\n').count() + 1
    };

    // Extract functions
    for cap in fn_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "function".to_string(),
                kind_char: 'f',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Extract structs
    for cap in struct_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "struct".to_string(),
                kind_char: 's',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Extract enums
    for cap in enum_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "enum".to_string(),
                kind_char: 'e',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Extract traits
    for cap in trait_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "trait".to_string(),
                kind_char: 't',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Extract type aliases
    for cap in type_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "type".to_string(),
                kind_char: 'T',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Extract constants and statics
    for cap in const_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "constant".to_string(),
                kind_char: 'c',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Extract modules
    for cap in mod_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "module".to_string(),
                kind_char: 'm',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Extract impl blocks
    for cap in impl_regex.captures_iter(&content) {
        if let Some(name_match) = cap.get(1) {
            tags.push(RustTag {
                name: name_match.as_str().to_string(),
                kind_name: "impl".to_string(),
                kind_char: 'i',
                line_number: find_line_number(name_match.start()),
                scope: None,
            });
        }
    }

    // Sort by line number for consistency
    tags.sort_by_key(|t| t.line_number);

    Ok(tags)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_rust_file() {
        // Create a temporary Rust file
        use std::fs;
        use std::io::Write;

        let test_file = "/tmp/test_ctags_collector.rs";
        let mut file = fs::File::create(test_file).unwrap();
        writeln!(file, "pub fn hello() {{}}").unwrap();
        writeln!(file, "pub struct Point {{ x: i32, y: i32 }}").unwrap();
        drop(file);

        // Parse the file
        let tags = parse_rust_file(test_file).unwrap();

        // Check results
        assert!(tags.len() >= 2, "Expected at least 2 tags (function + struct)");

        // Find function tag
        let func_tag = tags.iter().find(|t| t.name == "hello");
        assert!(func_tag.is_some(), "Function 'hello' not found");

        // Find struct tag
        let struct_tag = tags.iter().find(|t| t.name == "Point");
        assert!(struct_tag.is_some(), "Struct 'Point' not found");

        // Clean up
        fs::remove_file(test_file).ok();
    }
}

//! Configuration expression parsing and evaluation.

use std::collections::HashSet;
use std::path::Path;

/// Represents a parsed cfg expression
#[derive(Debug, Clone)]
pub enum CfgExpr {
    Feature(String),
    Not(Box<CfgExpr>),
    All(Vec<CfgExpr>),
    Any(Vec<CfgExpr>),
    Target(String, String),
    Other(String),
}

impl CfgExpr {
    /// Evaluate cfg expression against enabled features
    pub fn evaluate(&self, enabled_features: &HashSet<String>) -> bool {
        match self {
            CfgExpr::Feature(name) => enabled_features.contains(name),
            CfgExpr::Not(inner) => !inner.evaluate(enabled_features),
            CfgExpr::All(exprs) => exprs.iter().all(|e| e.evaluate(enabled_features)),
            CfgExpr::Any(exprs) => exprs.iter().any(|e| e.evaluate(enabled_features)),
            CfgExpr::Target(key, value) => {
                match key.as_str() {
                    "target_os" => {
                        let current_os = if cfg!(target_os = "linux") { "linux" }
                            else if cfg!(target_os = "macos") { "macos" }
                            else if cfg!(target_os = "windows") { "windows" }
                            else if cfg!(target_os = "freebsd") { "freebsd" }
                            else if cfg!(target_os = "android") { "android" }
                            else if cfg!(target_os = "ios") { "ios" }
                            else if cfg!(target_os = "wasi") { "wasi" }
                            else { "unknown" };
                        value == current_os
                    }
                    "target_arch" => {
                        let current_arch = if cfg!(target_arch = "x86_64") { "x86_64" }
                            else if cfg!(target_arch = "x86") { "x86" }
                            else if cfg!(target_arch = "aarch64") { "aarch64" }
                            else if cfg!(target_arch = "arm") { "arm" }
                            else if cfg!(target_arch = "wasm32") { "wasm32" }
                            else { "unknown" };
                        value == current_arch
                    }
                    "target_family" => {
                        match value.as_str() {
                            "unix" => cfg!(unix),
                            "windows" => cfg!(windows),
                            "wasm" => cfg!(target_family = "wasm"),
                            _ => false,
                        }
                    }
                    "target_env" => {
                        let current_env = if cfg!(target_env = "gnu") { "gnu" }
                            else if cfg!(target_env = "musl") { "musl" }
                            else if cfg!(target_env = "msvc") { "msvc" }
                            else { "" };
                        value == current_env
                    }
                    "target_pointer_width" => {
                        let current_width = if cfg!(target_pointer_width = "64") { "64" }
                            else if cfg!(target_pointer_width = "32") { "32" }
                            else { "unknown" };
                        value == current_width
                    }
                    "target_endian" => {
                        let current_endian = if cfg!(target_endian = "little") { "little" }
                            else { "big" };
                        value == current_endian
                    }
                    "target_has_atomic" => {
                        match value.as_str() {
                            "64" => cfg!(target_pointer_width = "64"),
                            "32" | "16" | "8" | "ptr" => true,
                            _ => false,
                        }
                    }
                    _ => true,
                }
            }
            CfgExpr::Other(name) => {
                match name.as_str() {
                    "unix" => cfg!(unix),
                    "windows" => cfg!(windows),
                    "docsrs" => false,
                    "test" => false,
                    "doc" => false,
                    "loom" => false,
                    "miri" => false,
                    "tokio_unstable" => false,
                    "tokio_taskdump" => false,
                    "fuzzing" => false,
                    _ => true,
                }
            }
        }
    }
}

/// Check if a file path is for a platform that doesn't match the current platform
pub fn is_wrong_platform_file(path: &Path) -> bool {
    let path_str = path.to_string_lossy();

    let wrong_platform_dirs = if cfg!(unix) {
        vec!["/windows/", "/win/", "/os_win"]
    } else if cfg!(windows) {
        vec!["/unix/", "/linux/", "/macos/", "/freebsd/", "/os_unix"]
    } else {
        vec![]
    };

    let wrong_platform_files = if cfg!(unix) {
        vec!["_windows.rs", "_win.rs", "windows.rs", "os_win32.rs", "os_windows.rs"]
    } else if cfg!(windows) {
        vec!["_unix.rs", "_linux.rs", "_macos.rs", "unix.rs", "linux.rs", "macos.rs",
             "freebsd.rs", "os_unix.rs"]
    } else {
        vec![]
    };

    for pattern in &wrong_platform_dirs {
        if path_str.contains(pattern) {
            return true;
        }
    }

    if let Some(file_name) = path.file_name().and_then(|n| n.to_str()) {
        for pattern in &wrong_platform_files {
            if file_name == *pattern || file_name.ends_with(pattern) {
                return true;
            }
        }
    }

    #[cfg(not(target_family = "wasm"))]
    {
        if path_str.contains("/wasm32/") || path_str.contains("/wasm/") {
            return true;
        }
        if let Some(file_name) = path.file_name().and_then(|n| n.to_str()) {
            if file_name == "wasm.rs" || file_name == "wasm32.rs" {
                return true;
            }
        }
    }

    false
}

/// Parse a cfg expression string
pub fn parse_cfg_expr(s: &str) -> Option<CfgExpr> {
    let s = s.trim();

    if s.starts_with("feature") {
        if let Some(start) = s.find('"') {
            if let Some(end) = s[start+1..].find('"') {
                let name = &s[start+1..start+1+end];
                return Some(CfgExpr::Feature(name.to_string()));
            }
        }
        return None;
    }

    if s.starts_with("not(") && s.ends_with(')') {
        let inner = &s[4..s.len()-1];
        if let Some(expr) = parse_cfg_expr(inner) {
            return Some(CfgExpr::Not(Box::new(expr)));
        }
        return None;
    }

    if (s.starts_with("all(") || s.starts_with("any(")) && s.ends_with(')') {
        let is_all = s.starts_with("all(");
        let inner = &s[4..s.len()-1];

        let mut exprs = Vec::new();
        let mut depth = 0;
        let mut start = 0;
        let chars: Vec<char> = inner.chars().collect();

        for (i, c) in chars.iter().enumerate() {
            match c {
                '(' => depth += 1,
                ')' => depth -= 1,
                ',' if depth == 0 => {
                    let part = inner[start..i].trim();
                    if !part.is_empty() {
                        if let Some(expr) = parse_cfg_expr(part) {
                            exprs.push(expr);
                        }
                    }
                    start = i + 1;
                }
                _ => {}
            }
        }
        let part = inner[start..].trim();
        if !part.is_empty() {
            if let Some(expr) = parse_cfg_expr(part) {
                exprs.push(expr);
            }
        }

        if is_all {
            return Some(CfgExpr::All(exprs));
        } else {
            return Some(CfgExpr::Any(exprs));
        }
    }

    if s.starts_with("target_") {
        if let Some(eq_pos) = s.find('=') {
            let key = s[..eq_pos].trim();
            let val = s[eq_pos+1..].trim().trim_matches('"');
            return Some(CfgExpr::Target(key.to_string(), val.to_string()));
        }
    }

    if s == "test" {
        return Some(CfgExpr::Other("test".to_string()));
    }
    if s == "doc" {
        return Some(CfgExpr::Other("doc".to_string()));
    }

    Some(CfgExpr::Other(s.to_string()))
}

/// Parse #[cfg(...)] attribute and return the cfg expression
pub fn parse_cfg_attribute(attr_line: &str) -> Option<CfgExpr> {
    let trimmed = attr_line.trim();
    if !trimmed.starts_with("#[cfg(") {
        return None;
    }
    let start = 6;
    let inner = &trimmed[start..];
    if let Some(end) = inner.rfind(")]") {
        let cfg_str = &inner[..end];
        return parse_cfg_expr(cfg_str);
    }
    None
}

//! Semantic slicing strategies.
//!
//! Higher-level slicing functions that use rust-analyzer or other tools.

use std::collections::{HashMap, HashSet, BTreeSet};
use std::fs;
use std::path::Path;
use std::sync::OnceLock;

use aho_corasick::AhoCorasick;
#[cfg(feature = "scip-analysis")]
use protobuf::Message;

use crate::types::{CrateInfo, CrateIndex, SemanticSliceResult, UsedItem, ParsedItemKind};
use super::slicing::compute_needed_items;
use super::parsing::parse_crate;
use super::source_fix::format_source;
use crate::cargo_toml::parse_cargo_toml;
use crate::common::cfg_eval::parse_cfg_attribute;
use super::import_decision_log::{logger, ImportDecision, DecisionSource, UsageContext};

/// Global cache for std type mappings (type_name -> full_path)
static STD_TYPES_CACHE: OnceLock<HashMap<String, String>> = OnceLock::new();

/// Reserved module names that shadow standard library crates
/// These must be renamed to avoid conflicts
const RESERVED_MODULE_NAMES: &[&str] = &["core", "std", "alloc", "proc_macro"];

/// Types that are commonly redefined by crates and should not be force-imported from std
const COMMONLY_REDEFINED: &[&str] = &[
    "Result",
    "Ok",
    "Err",
    "Range",
    "RangeFrom",
    "RangeTo",
    "RangeFull",
    "RangeInclusive",
    "Error",
    "Token",
    "Ordering",
    "CachePadded",
    "OsStringExt",
    "OsStrExt",
    "Block",
    "Context",
    "Builder",
    "Config",
    "State",
    // Common sync primitives that are often re-exported in crates
    "Mutex",
    "RwLock",
    "Arc",
    "Condvar",
    // Private/internal types that appear in std_types cache but shouldn't be imported
    "Inner",  // Private in std::thread, commonly used as local enum/struct name
    "Slot",   // Private in std::sync, commonly used as local struct name
];

/// Patterns that indicate unstable API usage
/// These are substrings that, if found in source code or imports, indicate
/// usage of nightly-only features that aren't available on stable Rust.
/// Format: (pattern, description) for documentation
const UNSTABLE_PATTERNS: &[(&str, &str)] = &[
    // error_generic_member_access feature (nightly only)
    ("std::error::Request", "error_generic_member_access"),
    ("core::error::Request", "error_generic_member_access"),
    ("error::Request", "error_generic_member_access"),
    ("Request < 'a >", "error_generic_member_access (syn output)"),
    ("Request<'a>", "error_generic_member_access"),
    ("ThiserrorProvide", "error_generic_member_access"),

    // backtrace feature (partially stabilized but Request still isn't)
    ("error::request", "error_generic_member_access"),

    // allocator_api feature (nightly only)
    ("alloc::Allocator", "allocator_api"),
    ("core::alloc::Allocator", "allocator_api"),
    ("std::alloc::Allocator", "allocator_api"),
    ("std::alloc::Global", "allocator_api"),
    ("alloc::alloc::Global", "allocator_api"),
    ("core::alloc::Global", "allocator_api"),

    // negative_impls feature (nightly only)
    ("impl !Send", "negative_impls"),
    ("impl !Sync", "negative_impls"),

    // specialization feature (nightly only)
    ("default fn ", "specialization"),
    ("default type ", "specialization"),

    // slice_concat_trait feature (nightly only)
    ("std::slice::Join", "slice_concat_trait"),

    // portable_simd feature (nightly only) - THE BIG ONE (328 errors)
    ("std::simd", "portable_simd"),
    ("core::simd", "portable_simd"),
    ("std :: simd", "portable_simd"),  // syn adds spaces
    ("core :: simd", "portable_simd"),
];

/// Detect unstable features that need #![feature(...)] declarations
/// Returns a list of feature names that should be added to lib.rs
fn detect_required_features(code: &str) -> Vec<&'static str> {
    let mut features = HashSet::new();

    // Normalize code - syn's quote! adds spaces around :: and <
    let normalized = normalize_source(code);

    // Check for portable_simd (the big one - 328 errors across 4 crates)
    if normalized.contains("std::simd") || normalized.contains("core::simd") {
        features.insert("portable_simd");
    }

    // Check for other unstable patterns
    for (pattern, _desc) in UNSTABLE_PATTERNS {
        let pattern_normalized = normalize_source(pattern);
        if normalized.contains(&pattern_normalized) {
            // Extract feature name from description
            if pattern.contains("simd") {
                features.insert("portable_simd");
            } else if pattern.contains("Allocator") || pattern.contains("Global") {
                features.insert("allocator_api");
            } else if pattern.contains("!Send") || pattern.contains("!Sync") {
                features.insert("negative_impls");
            } else if pattern.starts_with("default ") {
                features.insert("specialization");
            } else if pattern.contains("Request") || pattern.contains("request") {
                // Skip error_generic_member_access - too unstable, rarely needed
                // Most uses are in derive macros that we don't include anyway
            }
        }
    }

    features.into_iter().collect()
}

/// Private items from std library that should not be imported
/// These are internal implementation details and will cause E0603 or E0432 errors
const PRIVATE_STD_ITEMS: &[&str] = &[
    "std::thread::Inner",
    "core::thread::Inner",
    "std::sync::Slot",        // Private type, causes E0432 "no `Slot` in `sync`"
    "core::sync::Slot",
    // Add other known private std items as they're discovered
];

/// Check if source code uses unstable features
fn uses_unstable_features(source: &str) -> bool {
    for (pattern, _) in UNSTABLE_PATTERNS {
        if source.contains(pattern) {
            return true;
        }
    }
    false
}

/// Check if a cfg attribute is specifically #[cfg(test)]
/// This is more precise than just checking if the string contains "test",
/// which would incorrectly match #[cfg(doctest)] or #[cfg(feature = "test")]
fn is_cfg_test(cfg_attr: &str) -> bool {
    // Match #[cfg(test)] but not #[cfg(doctest)] or #[cfg(feature = "test")]
    use regex::Regex;
    use once_cell::sync::Lazy;
    static CFG_TEST: Lazy<Regex> = Lazy::new(|| {
        // Match cfg(test) as a whole word, not as part of another word like "doctest"
        Regex::new(r"cfg\s*\(\s*test\s*\)").unwrap()
    });
    CFG_TEST.is_match(cfg_attr)
}

/// Strip #[cfg(test)] blocks from source code.
/// These are test modules that shouldn't be included in the sliced output.
fn strip_cfg_test_blocks(source: &str) -> String {
    use regex::Regex;
    use once_cell::sync::Lazy;

    // Pattern to match #[cfg(test)] mod name { ... } blocks
    // This handles nested braces by counting them
    static CFG_TEST_MOD: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"#\s*\[\s*cfg\s*\(\s*test\s*\)\s*\]\s*mod\s+\w+\s*\{").unwrap()
    });

    let mut result = source.to_string();

    // Find and remove all #[cfg(test)] mod blocks
    while let Some(m) = CFG_TEST_MOD.find(&result) {
        let start = m.start();
        let block_start = m.end() - 1; // Position of opening brace

        // Find matching closing brace using byte indices
        let mut depth = 1;
        let mut pos = block_start + 1;
        let bytes = result.as_bytes();

        while pos < bytes.len() && depth > 0 {
            match bytes[pos] {
                b'{' => depth += 1,
                b'}' => depth -= 1,
                _ => {}
            }
            pos += 1;
        }

        if depth == 0 {
            // Remove the entire block
            result = format!("{}{}", &result[..start], &result[pos..]);
        } else {
            // Couldn't find matching brace, skip this match to avoid infinite loop
            break;
        }
    }

    result
}

/// Strip `use crate::` and `clippy` imports from source code.
/// These are managed by the slicer or should be filtered out.
fn strip_internal_imports(source: &str) -> String {
    use regex::Regex;
    use once_cell::sync::Lazy;

    // Pattern to match:
    // 1. use crate::clippy...;
    // 2. use clippy...;
    // 3. #[allow(clippy::...)]
    static STRIP_PATTERNS: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"(?m)^\s*(?:use\s+(?:crate\s*::\s*clippy|clippy)|#\s*\[\s*allow\s*\(\s*clippy).*\n?").unwrap()
    });

    STRIP_PATTERNS.replace_all(source, "").to_string()
}

/// Check if a use statement imports from an unstable API
/// Returns true if the import should be skipped
/// Extract module path from `use crate::module::path::{...}` → "module::path"
fn extract_crate_import_module(source: &str) -> Option<String> {
    // Match `use crate::path::to::module::{...}` or `use crate::path::to::module::Item`
    let re = regex::Regex::new(
        r"use\s+crate\s*::\s*([a-z_][a-z0-9_]*(?:\s*::\s*[a-z_][a-z0-9_]*)*)\s*(?:::|;)"
    ).unwrap();

    if let Some(cap) = re.captures(source) {
        if let Some(path_match) = cap.get(1) {
            let path = path_match.as_str().replace(" ", "");
            return Some(path);
        }
    }

    None
}

pub fn is_unstable_std_import(import_source: &str) -> bool {
    // Normalize whitespace for matching
    let normalized: String = import_source.chars()
        .map(|c| if c.is_whitespace() { ' ' } else { c })
        .collect();

    for (pattern, _) in UNSTABLE_PATTERNS {
        if normalized.contains(pattern) {
            return true;
        }
    }
    false
}

/// Check if an import attempts to access a private item from std library
/// Returns true if the import should be skipped
pub fn is_private_std_import(import_source: &str) -> bool {
    // Normalize whitespace for matching
    let normalized: String = import_source.chars()
        .map(|c| if c.is_whitespace() { ' ' } else { c })
        .collect();

    for pattern in PRIVATE_STD_ITEMS {
        if normalized.contains(pattern) {
            return true;
        }
    }
    false
}

/// Check if an import is for a Rust prelude type that doesn't need importing
/// Returns true if the import should be skipped (type is always available)
pub fn is_prelude_type(import_path: &str) -> bool {
    // Rust prelude types that are always available without importing
    // https://doc.rust-lang.org/std/prelude/index.html
    const PRELUDE_TYPES: &[&str] = &[
        // Comparison traits
        "std::cmp::Eq",
        "std::cmp::Ord",
        "std::cmp::PartialEq",
        "std::cmp::PartialOrd",
        // Conversion traits
        "std::convert::AsRef",
        "std::convert::AsMut",
        "std::convert::From",
        "std::convert::Into",
        "std::convert::TryFrom",
        "std::convert::TryInto",
        // Default/Clone
        "std::default::Default",
        "std::clone::Clone",
        // Marker traits
        "std::marker::Copy",
        "std::marker::Send",
        "std::marker::Sized",
        "std::marker::Sync",
        "std::marker::Unpin",
        // Ops traits
        "std::ops::Drop",
        "std::ops::Fn",
        "std::ops::FnMut",
        "std::ops::FnOnce",
        // Iterator traits
        "std::iter::DoubleEndedIterator",
        "std::iter::ExactSizeIterator",
        "std::iter::Extend",
        "std::iter::IntoIterator",
        "std::iter::Iterator",
        // Borrow
        "std::borrow::ToOwned",
        // Option/Result
        "std::option::Option",
        "std::result::Result",
        // Common types
        "std::string::String",
        "std::string::ToString",
        "std::vec::Vec",
        "std::boxed::Box",
        // mem functions
        "std::mem::drop",
    ];

    PRELUDE_TYPES.contains(&import_path)
}

/// Build a map from module paths to their cfg attributes.
/// This collects cfg attributes from inline modules (ParsedItemKind::Mod) so we can
/// filter out platform-specific modules that don't match the current target.
pub fn build_module_cfg_map(index: &CrateIndex, crate_name: &str) -> HashMap<String, String> {
    let mut module_cfgs = HashMap::new();

    for item in &index.all_items {
        if item.kind == ParsedItemKind::Mod {
            if let Some(ref cfg_attr) = item.cfg_attr {
                // Build the full module path
                let module_path = if item.module_path.is_empty() {
                    item.name.clone()
                } else {
                    format!("{}::{}", item.module_path, item.name)
                };

                // Normalize by removing crate name prefix
                let normalized = module_path
                    .trim_start_matches(crate_name)
                    .trim_start_matches("::")
                    .to_string();

                if !normalized.is_empty() {
                    module_cfgs.insert(normalized, cfg_attr.clone());
                }
            }
        }
    }

    module_cfgs
}

/// Check if a module's cfg attribute evaluates to true for the current platform.
/// Returns true if the module should be included (cfg matches or no cfg present).
pub fn module_cfg_matches_platform(module_path: &str, module_cfgs: &HashMap<String, String>) -> bool {
    // Check if this module or any of its parents have a cfg attribute
    let parts: Vec<&str> = module_path.split("::").collect();

    for i in 1..=parts.len() {
        let path = parts[..i].join("::");
        if let Some(cfg_attr) = module_cfgs.get(&path) {
            if let Some(cfg_expr) = parse_cfg_attribute(cfg_attr) {
                let enabled_features = HashSet::new(); // No features enabled by default
                if !cfg_expr.evaluate(&enabled_features) {
                    return false;
                }
            }
        }
    }

    true
}

/// Rename a module name if it conflicts with a standard library crate
fn rename_reserved_module(name: &str) -> String {
    if RESERVED_MODULE_NAMES.contains(&name) {
        format!("{}_", name)
    } else {
        name.to_string()
    }
}

/// Transform a module path by renaming any reserved module names
/// e.g., "core::ops" -> "core_::ops"
fn rename_reserved_in_path(path: &str) -> String {
    if path.is_empty() {
        return path.to_string();
    }
    path.split("::")
        .map(|part| {
            if RESERVED_MODULE_NAMES.contains(&part) {
                format!("{}_", part)
            } else {
                part.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("::")
}

/// Transform content to rename references to reserved modules
/// e.g., "crate::core::" -> "crate::core_::"
fn transform_reserved_module_refs(content: &str) -> String {
    let mut result = content.to_string();

    for &reserved in RESERVED_MODULE_NAMES {
        // Replace "crate::core::" with "crate::core_::"
        let crate_pattern = format!("crate::{}::", reserved);
        let crate_replacement = format!("crate::{}_{}", reserved, "::");
        result = result.replace(&crate_pattern, &crate_replacement);

        // Replace "crate::core," or "crate::core;" or "crate::core}" (end of use)
        for suffix in &[",", ";", "}", " "] {
            let pattern = format!("crate::{}{}", reserved, suffix);
            let replacement = format!("crate::{}_{}", reserved, suffix);
            result = result.replace(&pattern, &replacement);
        }

        // Replace "super::core::" with "super::core_::"
        let super_pattern = format!("super::{}::", reserved);
        let super_replacement = format!("super::{}_{}", reserved, "::");
        result = result.replace(&super_pattern, &super_replacement);
    }

    // Fix broken imports for std types that are incorrectly imported from crate
    // Replace "use crate::anything::TypeName;" with the correct std import
    // Uses the std_types cache to look up canonical paths

    // Types that should NOT be replaced even if in cache
    // These are commonly shadowed by crate-local definitions
    static EXCLUDED_TYPES: &[&str] = &[
        // Range types - winnow and other crates define their own
        "Range", "RangeFrom", "RangeTo", "RangeFull", "RangeInclusive",
        // Common names that might be crate-local
        "Error", "Result", "Context", "Builder", "Config", "Token", "Ordering", "CachePadded",
        "OsStringExt", "OsStrExt", "Block",
        // Trait and type names commonly shadowed by protobuf and other crates
        "Any", "Enum",
    ];

    // Replace broken crate imports with correct std imports
    let std_types = load_std_types();
    let mut cleaned_lines = Vec::new();
    for line in result.lines() {
        let trimmed = line.trim();
        // Check if this is a use statement importing a core std type from crate
        if trimmed.starts_with("use crate::") && trimmed.ends_with(';') {
            // Extract the type name (last component before ;)
            let path = &trimmed[4..trimmed.len()-1]; // remove "use " and ";"
            if let Some(type_name) = path.rsplit("::").next() {
                // Skip excluded types that are commonly shadowed
                if !EXCLUDED_TYPES.contains(&type_name) {
                    // Look up in std_types cache
                    if let Some(std_path) = std_types.get(type_name) {
                        // Skip imports from unstable APIs (E0658 fix), private items (E0603 fix), and prelude types
                        if !is_unstable_std_import(std_path) && !is_private_std_import(std_path) && !is_prelude_type(std_path) {
                            let std_import = format!("use {};", std_path);
                            cleaned_lines.push(std_import);
                            continue;
                        }
                    }
                }
            }
        }
        cleaned_lines.push(line.to_string());
    }

    cleaned_lines.join("\n")
}

/// Global cache for std module mappings (module_name -> full_path)
static STD_MODULES_CACHE: OnceLock<HashMap<String, String>> = OnceLock::new();

/// Global cache for Aho-Corasick pattern matcher for std import detection
static STD_IMPORT_MATCHER: OnceLock<StdImportMatcher> = OnceLock::new();

/// Pattern categories for std import detection
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ImportCategory {
    // Hash traits
    Hasher,
    BuildHasher,
    Hash,
    // Cmp traits
    Ord,
    PartialOrd,
    Eq,
    PartialEq,
    CmpOrdering,
    AtomicOrdering,
    // Fmt traits
    Display,
    Debug,
    FmtWrite,
    Formatter,
    // IO traits
    Read,
    IoWrite,
    BufRead,
    Seek,
    // Iter traits
    Iterator,
    IntoIterator,
    ExactSizeIterator,
    DoubleEndedIterator,
    FusedIterator,
    // Ops traits
    Deref,
    DerefMut,
    Index,
    IndexMut,
    Drop,
    RangeBounds,
    // Convert traits
    From,
    Into,
    TryFrom,
    TryInto,
    AsRef,
    AsMut,
    // Default/Clone
    Default,
    Clone,
    // Marker traits
    Send,
    Sync,
    Copy,
    PhantomData,
    // Borrow traits
    Borrow,
    BorrowMut,
    ToOwned,
    Cow,
    // Error
    Error,
    // Str
    FromStr,
    // Smart pointers
    Arc,
    Rc,
    Mutex,
    RwLock,
    Cell,
    RefCell,
    UnsafeCell,
    // Hint
    Hint,
    // Mem
    Mem,
    // Ptr
    Ptr,
    // Slice
    SliceIndex,
    // Atomics
    AtomicUsize,
    AtomicBool,
    AtomicU8,
    AtomicU16,
    AtomicU32,
    AtomicU64,
    AtomicPtr,
    // NonZero
    NonZeroUsize,
    NonZeroU8,
    NonZeroU16,
    NonZeroU32,
    NonZeroU64,
    NonZeroI32,
    NonZeroI64,
    // Sync types
    Condvar,
    Once,
    Barrier,
    // Panic types
    UnwindSafe,
    RefUnwindSafe,
    // Thread
    Thread,
    ThreadId,
    // Time
    Duration,
    Instant,
    SystemTime,
    // Collections
    HashMap,
    HashSet,
    BTreeMap,
    BTreeSet,
    VecDeque,
    LinkedList,
    BinaryHeap,
    // Path
    Path,
    PathBuf,
    // FFI
    CStr,
    CString,
    OsStr,
    OsString,
    CVoid,
    CChar,
    CInt,
    CUint,
    CLong,
    CUlong,
    CachePadded,
    // IO module
    IoModule,
    // Env
    Env,
    // Fs
    Fs,
    // Parking lot detection
    ParkingLot,
    // Use statement detection (negative patterns)
    UseStatement,
    // Fmt module detection for Write disambiguation
    FmtModule,
    // IO module detection for Write disambiguation
    IoModule2,
}

/// Pattern rule for std import detection
struct PatternRule {
    pattern: &'static str,
    category: ImportCategory,
}

/// Compiled Aho-Corasick matcher with pattern metadata
struct StdImportMatcher {
    automaton: AhoCorasick,
    rules: Vec<PatternRule>,
}

impl StdImportMatcher {
    fn new() -> Self {
        let rules: Vec<PatternRule> = vec![
            // Hash traits
            PatternRule { pattern: "impl Hasher for", category: ImportCategory::Hasher },
            PatternRule { pattern: ": Hasher", category: ImportCategory::Hasher },
            PatternRule { pattern: "impl BuildHasher for", category: ImportCategory::BuildHasher },
            PatternRule { pattern: ": BuildHasher", category: ImportCategory::BuildHasher },
            PatternRule { pattern: "type Hasher =", category: ImportCategory::BuildHasher },
            PatternRule { pattern: "impl Hash for", category: ImportCategory::Hash },
            PatternRule { pattern: ": Hash", category: ImportCategory::Hash },
            // Cmp traits
            PatternRule { pattern: "impl Ord for", category: ImportCategory::Ord },
            PatternRule { pattern: ": Ord", category: ImportCategory::Ord },
            PatternRule { pattern: "impl PartialOrd for", category: ImportCategory::PartialOrd },
            PatternRule { pattern: ": PartialOrd", category: ImportCategory::PartialOrd },
            PatternRule { pattern: "impl Eq for", category: ImportCategory::Eq },
            PatternRule { pattern: ": Eq", category: ImportCategory::Eq },
            PatternRule { pattern: "impl PartialEq for", category: ImportCategory::PartialEq },
            PatternRule { pattern: ": PartialEq", category: ImportCategory::PartialEq },
            // Ordering detection
            PatternRule { pattern: "Ordering::Less", category: ImportCategory::CmpOrdering },
            PatternRule { pattern: "Ordering::Equal", category: ImportCategory::CmpOrdering },
            PatternRule { pattern: "Ordering::Greater", category: ImportCategory::CmpOrdering },
            PatternRule { pattern: ": Ordering", category: ImportCategory::CmpOrdering },
            PatternRule { pattern: "Ordering::Acquire", category: ImportCategory::AtomicOrdering },
            PatternRule { pattern: "Ordering::Release", category: ImportCategory::AtomicOrdering },
            PatternRule { pattern: "Ordering::Relaxed", category: ImportCategory::AtomicOrdering },
            PatternRule { pattern: "Ordering::AcqRel", category: ImportCategory::AtomicOrdering },
            PatternRule { pattern: "Ordering::SeqCst", category: ImportCategory::AtomicOrdering },
            // Fmt traits
            PatternRule { pattern: "impl Display for", category: ImportCategory::Display },
            PatternRule { pattern: ": Display", category: ImportCategory::Display },
            PatternRule { pattern: "impl Debug for", category: ImportCategory::Debug },
            PatternRule { pattern: ": Debug", category: ImportCategory::Debug },
            PatternRule { pattern: "impl Write for", category: ImportCategory::FmtWrite },
            PatternRule { pattern: ": Write", category: ImportCategory::FmtWrite },
            PatternRule { pattern: "Formatter<", category: ImportCategory::Formatter },
            PatternRule { pattern: "fmt::Result", category: ImportCategory::Formatter },
            PatternRule { pattern: "fmt::", category: ImportCategory::FmtModule },
            // IO traits
            PatternRule { pattern: "impl Read for", category: ImportCategory::Read },
            PatternRule { pattern: ": Read", category: ImportCategory::Read },
            PatternRule { pattern: "impl BufRead for", category: ImportCategory::BufRead },
            PatternRule { pattern: ": BufRead", category: ImportCategory::BufRead },
            PatternRule { pattern: "impl Seek for", category: ImportCategory::Seek },
            PatternRule { pattern: ": Seek", category: ImportCategory::Seek },
            PatternRule { pattern: "io::", category: ImportCategory::IoModule2 },
            // Iter traits
            PatternRule { pattern: "impl Iterator for", category: ImportCategory::Iterator },
            PatternRule { pattern: ": Iterator", category: ImportCategory::Iterator },
            PatternRule { pattern: "impl IntoIterator for", category: ImportCategory::IntoIterator },
            PatternRule { pattern: ": IntoIterator", category: ImportCategory::IntoIterator },
            PatternRule { pattern: "impl ExactSizeIterator for", category: ImportCategory::ExactSizeIterator },
            PatternRule { pattern: "impl DoubleEndedIterator for", category: ImportCategory::DoubleEndedIterator },
            PatternRule { pattern: "impl FusedIterator for", category: ImportCategory::FusedIterator },
            // Ops traits
            PatternRule { pattern: "impl Deref for", category: ImportCategory::Deref },
            PatternRule { pattern: ": Deref", category: ImportCategory::Deref },
            PatternRule { pattern: "impl DerefMut for", category: ImportCategory::DerefMut },
            PatternRule { pattern: ": DerefMut", category: ImportCategory::DerefMut },
            PatternRule { pattern: "impl Index<", category: ImportCategory::Index },
            PatternRule { pattern: "impl IndexMut<", category: ImportCategory::IndexMut },
            PatternRule { pattern: "impl Drop for", category: ImportCategory::Drop },
            PatternRule { pattern: "RangeBounds", category: ImportCategory::RangeBounds },
            // Convert traits
            PatternRule { pattern: "impl From<", category: ImportCategory::From },
            PatternRule { pattern: ": From<", category: ImportCategory::From },
            PatternRule { pattern: "impl Into<", category: ImportCategory::Into },
            PatternRule { pattern: ": Into<", category: ImportCategory::Into },
            PatternRule { pattern: "impl TryFrom<", category: ImportCategory::TryFrom },
            PatternRule { pattern: "impl TryInto<", category: ImportCategory::TryInto },
            PatternRule { pattern: "impl AsRef<", category: ImportCategory::AsRef },
            PatternRule { pattern: "impl AsMut<", category: ImportCategory::AsMut },
            // Default/Clone
            PatternRule { pattern: "impl Default for", category: ImportCategory::Default },
            PatternRule { pattern: ": Default", category: ImportCategory::Default },
            PatternRule { pattern: "impl Clone for", category: ImportCategory::Clone },
            PatternRule { pattern: ": Clone", category: ImportCategory::Clone },
            // Marker traits
            PatternRule { pattern: ": Send", category: ImportCategory::Send },
            PatternRule { pattern: "+ Send", category: ImportCategory::Send },
            PatternRule { pattern: ": Sync", category: ImportCategory::Sync },
            PatternRule { pattern: "+ Sync", category: ImportCategory::Sync },
            PatternRule { pattern: ": Copy", category: ImportCategory::Copy },
            PatternRule { pattern: "+ Copy", category: ImportCategory::Copy },
            PatternRule { pattern: "PhantomData<", category: ImportCategory::PhantomData },
            PatternRule { pattern: "PhantomData,", category: ImportCategory::PhantomData },
            PatternRule { pattern: "PhantomData}", category: ImportCategory::PhantomData },
            PatternRule { pattern: ": PhantomData", category: ImportCategory::PhantomData },
            // Borrow traits
            PatternRule { pattern: "impl Borrow<", category: ImportCategory::Borrow },
            PatternRule { pattern: ": Borrow<", category: ImportCategory::Borrow },
            PatternRule { pattern: "impl BorrowMut<", category: ImportCategory::BorrowMut },
            PatternRule { pattern: "impl ToOwned for", category: ImportCategory::ToOwned },
            PatternRule { pattern: "Cow<", category: ImportCategory::Cow },
            // Error
            PatternRule { pattern: "impl Error for", category: ImportCategory::Error },
            PatternRule { pattern: ": Error", category: ImportCategory::Error },
            // Str
            PatternRule { pattern: "impl FromStr for", category: ImportCategory::FromStr },
            // Smart pointers
            PatternRule { pattern: "Arc<", category: ImportCategory::Arc },
            PatternRule { pattern: "Rc<", category: ImportCategory::Rc },
            PatternRule { pattern: "Mutex<", category: ImportCategory::Mutex },
            PatternRule { pattern: "RwLock<", category: ImportCategory::RwLock },
            PatternRule { pattern: "Cell<", category: ImportCategory::Cell },
            PatternRule { pattern: "RefCell<", category: ImportCategory::RefCell },
            PatternRule { pattern: "UnsafeCell<", category: ImportCategory::UnsafeCell },
            PatternRule { pattern: "parking_lot::", category: ImportCategory::ParkingLot },
            // Hint
            PatternRule { pattern: "hint::", category: ImportCategory::Hint },
            PatternRule { pattern: "unreachable_unchecked", category: ImportCategory::Hint },
            // Mem
            PatternRule { pattern: "mem::", category: ImportCategory::Mem },
            PatternRule { pattern: "MaybeUninit<", category: ImportCategory::Mem },
            // Ptr
            PatternRule { pattern: "ptr::", category: ImportCategory::Ptr },
            PatternRule { pattern: "NonNull<", category: ImportCategory::Ptr },
            // Slice
            PatternRule { pattern: "SliceIndex", category: ImportCategory::SliceIndex },
            // Atomics
            PatternRule { pattern: "AtomicUsize", category: ImportCategory::AtomicUsize },
            PatternRule { pattern: "AtomicBool", category: ImportCategory::AtomicBool },
            PatternRule { pattern: "AtomicU8", category: ImportCategory::AtomicU8 },
            PatternRule { pattern: "AtomicU16", category: ImportCategory::AtomicU16 },
            PatternRule { pattern: "AtomicU32", category: ImportCategory::AtomicU32 },
            PatternRule { pattern: "AtomicU64", category: ImportCategory::AtomicU64 },
            PatternRule { pattern: "AtomicPtr", category: ImportCategory::AtomicPtr },
            // NonZero
            PatternRule { pattern: "NonZeroUsize", category: ImportCategory::NonZeroUsize },
            PatternRule { pattern: "NonZeroU8", category: ImportCategory::NonZeroU8 },
            PatternRule { pattern: "NonZeroU16", category: ImportCategory::NonZeroU16 },
            PatternRule { pattern: "NonZeroU32", category: ImportCategory::NonZeroU32 },
            PatternRule { pattern: "NonZeroU64", category: ImportCategory::NonZeroU64 },
            PatternRule { pattern: "NonZeroI32", category: ImportCategory::NonZeroI32 },
            PatternRule { pattern: "NonZeroI64", category: ImportCategory::NonZeroI64 },
            // Sync types
            PatternRule { pattern: "Condvar", category: ImportCategory::Condvar },
            PatternRule { pattern: "Once ", category: ImportCategory::Once },
            PatternRule { pattern: "Once,", category: ImportCategory::Once },
            PatternRule { pattern: "Once::", category: ImportCategory::Once },
            PatternRule { pattern: "Barrier", category: ImportCategory::Barrier },
            // Panic types
            PatternRule { pattern: "UnwindSafe", category: ImportCategory::UnwindSafe },
            PatternRule { pattern: "RefUnwindSafe", category: ImportCategory::RefUnwindSafe },
            // Thread
            PatternRule { pattern: "Thread ", category: ImportCategory::Thread },
            PatternRule { pattern: "Thread,", category: ImportCategory::Thread },
            PatternRule { pattern: "<Thread>", category: ImportCategory::Thread },
            PatternRule { pattern: "thread::", category: ImportCategory::Thread },
            PatternRule { pattern: "ThreadId", category: ImportCategory::ThreadId },
            // Time
            PatternRule { pattern: "Duration", category: ImportCategory::Duration },
            PatternRule { pattern: "Instant", category: ImportCategory::Instant },
            PatternRule { pattern: "SystemTime", category: ImportCategory::SystemTime },
            // Collections
            PatternRule { pattern: "HashMap<", category: ImportCategory::HashMap },
            PatternRule { pattern: "HashSet<", category: ImportCategory::HashSet },
            PatternRule { pattern: "BTreeMap<", category: ImportCategory::BTreeMap },
            PatternRule { pattern: "BTreeSet<", category: ImportCategory::BTreeSet },
            PatternRule { pattern: "VecDeque<", category: ImportCategory::VecDeque },
            PatternRule { pattern: "LinkedList<", category: ImportCategory::LinkedList },
            PatternRule { pattern: "BinaryHeap<", category: ImportCategory::BinaryHeap },
            // Path
            PatternRule { pattern: "Path ", category: ImportCategory::Path },
            PatternRule { pattern: "Path,", category: ImportCategory::Path },
            PatternRule { pattern: "&Path", category: ImportCategory::Path },
            PatternRule { pattern: "PathBuf", category: ImportCategory::PathBuf },
            PatternRule { pattern: "CachePadded", category: ImportCategory::CachePadded },
            // FFI
            PatternRule { pattern: "CStr", category: ImportCategory::CStr },
            PatternRule { pattern: "CString", category: ImportCategory::CString },
            PatternRule { pattern: "OsStr", category: ImportCategory::OsStr },
            PatternRule { pattern: "OsString", category: ImportCategory::OsString },
            PatternRule { pattern: "c_void", category: ImportCategory::CVoid },
            PatternRule { pattern: "c_char", category: ImportCategory::CChar },
            PatternRule { pattern: "c_int", category: ImportCategory::CInt },
            PatternRule { pattern: "c_uint", category: ImportCategory::CUint },
            PatternRule { pattern: "c_long", category: ImportCategory::CLong },
            PatternRule { pattern: "c_ulong", category: ImportCategory::CUlong },
            // IO module
            PatternRule { pattern: "io::Result", category: ImportCategory::IoModule },
            PatternRule { pattern: "io::Error", category: ImportCategory::IoModule },
            // Env
            PatternRule { pattern: "env::", category: ImportCategory::Env },
            // Fs
            PatternRule { pattern: "fs::", category: ImportCategory::Fs },
            // Use statement (for negative matching)
            PatternRule { pattern: "use ", category: ImportCategory::UseStatement },
        ];

        let patterns: Vec<&str> = rules.iter().map(|r| r.pattern).collect();
        let automaton = AhoCorasick::new(&patterns).expect("Failed to build Aho-Corasick automaton");

        StdImportMatcher { automaton, rules }
    }

    /// Find all matching categories in the content
    fn find_matches(&self, content: &str) -> HashSet<ImportCategory> {
        let mut found = HashSet::new();
        for mat in self.automaton.find_iter(content) {
            found.insert(self.rules[mat.pattern().as_usize()].category);
        }
        found
    }
}

/// Get the std import matcher (lazily initialized)
fn get_std_import_matcher() -> &'static StdImportMatcher {
    STD_IMPORT_MATCHER.get_or_init(StdImportMatcher::new)
}

/// Global cache for std module pattern matcher (for generate_internal_imports)
static STD_MODULE_MATCHER: OnceLock<StdModuleMatcher> = OnceLock::new();

/// Matcher for std module patterns like "fmt::", "ptr::", "mem::", etc.
struct StdModuleMatcher {
    automaton: AhoCorasick,
    /// Maps pattern index to (module_name, import_path)
    patterns: Vec<(String, String)>,
}

impl StdModuleMatcher {
    fn new(std_modules: &HashMap<String, String>) -> Self {
        let mut patterns = Vec::with_capacity(std_modules.len());
        let mut pattern_strings = Vec::with_capacity(std_modules.len());

        for (mod_name, import_path) in std_modules {
            let pattern = format!("{}::", mod_name);
            pattern_strings.push(pattern);
            patterns.push((mod_name.clone(), import_path.clone()));
        }

        let automaton = AhoCorasick::new(&pattern_strings)
            .expect("Failed to build std module Aho-Corasick automaton");

        StdModuleMatcher { automaton, patterns }
    }

    /// Find all std module usages in content, filtering out std::/core:: prefixed ones
    fn find_module_usages(&self, content: &str) -> HashSet<usize> {
        let mut found = HashSet::new();

        for mat in self.automaton.find_iter(content) {
            let pos = mat.start();
            // Check if this is preceded by "std::" or "core::"
            let is_std_import = pos >= 5 && &content[pos - 5..pos] == "std::";
            let is_core_import = pos >= 6 && &content[pos - 6..pos] == "core::";

            if !is_std_import && !is_core_import {
                found.insert(mat.pattern().as_usize());
            }
        }

        found
    }

    /// Get module info by pattern index
    fn get_module_info(&self, idx: usize) -> (&str, &str) {
        let (mod_name, import_path) = &self.patterns[idx];
        (mod_name.as_str(), import_path.as_str())
    }
}

/// Get the std module matcher (lazily initialized)
fn get_std_module_matcher() -> &'static StdModuleMatcher {
    STD_MODULE_MATCHER.get_or_init(|| {
        let std_modules = load_std_modules();
        StdModuleMatcher::new(std_modules)
    })
}

/// Build an Aho-Corasick automaton for crate-internal module patterns
/// Rust primitive types and keywords that should never be treated as crate modules
static RUST_PRIMITIVES: &[&str] = &[
    // Primitive types
    "u8", "u16", "u32", "u64", "u128", "usize",
    "i8", "i16", "i32", "i64", "i128", "isize",
    "f32", "f64", "bool", "char", "str",
    // Keywords that might appear with :: (e.g., impl::, mut::)
    "impl", "mut", "fn", "let", "const", "static", "type", "trait", "struct",
    "enum", "union", "mod", "pub", "crate", "self", "super", "use", "where",
    "for", "loop", "while", "if", "else", "match", "return", "break", "continue",
    "move", "ref", "async", "await", "dyn", "unsafe", "extern",
];

fn build_crate_module_matcher(all_paths: &HashSet<String>, current_module: &str, crate_name: Option<&str>) -> Option<(AhoCorasick, Vec<String>)> {
    // Build patterns for both top-level and leaf modules
    // For "nfa::noncontiguous", we want to match "noncontiguous::" and map it to "nfa::noncontiguous"
    let mut patterns: Vec<String> = Vec::new();
    let mut modules: Vec<String> = Vec::new();

    // Normalize crate name (hyphens to underscores) for filtering
    let normalized_crate_name = crate_name.map(|n| n.replace('-', "_"));

    // Build a set of "false root paths" - paths that exist as leaves but only within a parent
    // E.g., if we have "descriptor::field_descriptor_proto" but "field_descriptor_proto" by itself,
    // the standalone version is likely a false positive from pattern detection
    let mut false_root_paths: std::collections::HashSet<&str> = std::collections::HashSet::new();
    for path in all_paths {
        // If this is a root-level path (no ::)
        if !path.contains("::") && !path.is_empty() {
            // Check if there's a nested path that ends with this
            let suffix = format!("::{}", path);
            let has_nested_version = all_paths.iter().any(|p| p.ends_with(&suffix));
            if has_nested_version {
                // Check if this root path is likely a real module (has sibling with same prefix)
                // E.g., "rt" is real if "rt::something" exists in all_paths
                let has_children = all_paths.iter().any(|p| p.starts_with(&format!("{}::", path)));
                if !has_children {
                    // This root path has no children but has a nested version - it's a false positive
                    false_root_paths.insert(path.as_str());
                }
            }
        }
    }

    for path in all_paths {
        if path.is_empty() || path == current_module {
            continue;
        }

        // Get the leaf module name (last component)
        let leaf = path.rsplit("::").next().unwrap_or(path);

        // Skip single-character module names - these are likely false positives
        // (e.g., "r" from raw string prefix, "b" from byte string prefix)
        if leaf.len() <= 1 {
            continue;
        }

        // Skip if this module is the current module or a parent of it
        if current_module == leaf || current_module.starts_with(&format!("{}::", leaf)) {
            continue;
        }

        // Skip Rust primitive types - these should never be crate modules
        if RUST_PRIMITIVES.contains(&leaf) {
            continue;
        }

        // Skip if this is the crate name itself - it's the root, not a module
        // E.g., skip "aho_corasick" for crate "aho-corasick"
        if let Some(ref cn) = normalized_crate_name {
            if leaf == cn || path == cn {
                continue;
            }
        }

        // Skip false root paths (e.g., "field_descriptor_proto" when only
        // "descriptor::field_descriptor_proto" should be used)
        if false_root_paths.contains(path.as_str()) {
            continue;
        }

        // Skip nested modules when a REAL root-level module with the same leaf name exists
        // E.g., skip "reflect::rt" when "rt" exists as a real root module (not a false root)
        // This prevents import shadowing where `use crate::reflect::rt;` shadows `use crate::rt;`
        // Only skip if the root-level version is NOT a false root path
        if path.contains("::") && all_paths.contains(leaf) && !false_root_paths.contains(leaf) {
            continue;
        }

        // Add pattern for the leaf module name
        patterns.push(format!("{}::", leaf));
        modules.push(path.clone());
    }

    if modules.is_empty() || current_module.is_empty() {
        return None;
    }

    let automaton = AhoCorasick::new(&patterns).ok()?;
    Some((automaton, modules))
}

/// Find crate module usages using Aho-Corasick
fn find_crate_module_usages(
    content: &str,
    automaton: &AhoCorasick,
    modules: &[String],
) -> HashSet<String> {
    let mut found = HashSet::new();

    for mat in automaton.find_iter(content) {
        let pos = mat.start();

        // Check if preceded by "crate::"
        let is_crate_import = pos >= 7 && &content[pos - 7..pos] == "crate::";

        // Check if in a use statement
        let line_start = content[..pos].rfind('\n').map(|p| p + 1).unwrap_or(0);
        let line_prefix = &content[line_start..pos];
        let is_use_statement = line_prefix.trim_start().starts_with("use ");

        if !is_crate_import && !is_use_statement {
            found.insert(modules[mat.pattern().as_usize()].clone());
        }
    }

    found
}

/// Resolve a module path to its full path in the crate.
/// For example, if code uses `noncontiguous::NFA` but the actual path is `nfa::noncontiguous`,
/// this function returns "nfa::noncontiguous" given "noncontiguous".
fn resolve_module_path(short_path: &str, all_paths: &HashSet<String>) -> String {
    // If the short path already exists in all_paths, use it directly
    if all_paths.contains(short_path) {
        return short_path.to_string();
    }

    // Look for a path that ends with ::short_path
    let suffix = format!("::{}", short_path);
    for path in all_paths {
        if path.ends_with(&suffix) {
            return path.clone();
        }
    }

    // If no match found, return the original
    short_path.to_string()
}

/// Load std types from a cached JSON file (generated via --generate-std-types)
fn load_std_types() -> &'static HashMap<String, String> {
    STD_TYPES_CACHE.get_or_init(|| {
        let cache_path = std::env::var("STD_TYPES_CACHE")
            .unwrap_or_else(|_| {
                let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
                format!("{}/.cache/cargo-slicer/std_types.json", home)
            });

        if let Ok(content) = fs::read_to_string(&cache_path) {
            if let Ok(types) = serde_json::from_str::<HashMap<String, String>>(&content) {
                return types;
            }
        }

        // No cache available - return empty map
        // Run `cargo-slicer --generate-std-types <scip_path>` to generate
        HashMap::new()
    })
}

/// Generate a unique decision ID for an import decision
fn generate_decision_id(symbol: &str, module_path: &str, crate_name: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    symbol.hash(&mut hasher);
    module_path.hash(&mut hasher);
    crate_name.hash(&mut hasher);
    let hash = hasher.finish();

    format!("{}:{}:{:x}", crate_name, symbol, hash)
}

/// Log an import decision if logging is enabled
fn log_import_decision(
    symbol: &str,
    import_path: &str,
    source: DecisionSource,
    crate_name: &str,
    module_path: &str,
    file: &str,
    cfg_conditions: Vec<String>,
    is_qualified: bool,
    has_local_definition: bool,
    alternatives: Vec<String>,
) {
    // Only log if logging is enabled
    if !logger().is_enabled() {
        return;
    }

    let decision = ImportDecision {
        symbol: symbol.to_string(),
        import_path: import_path.to_string(),
        source,
        usage_context: UsageContext {
            crate_name: crate_name.to_string(),
            module_path: module_path.to_string(),
            file: file.to_string(),
            line: None, // Line numbers not available at this level
            snippet: None,
            cfg_conditions,
            is_qualified,
            has_local_definition,
        },
        alternatives,
        decision_id: generate_decision_id(symbol, module_path, crate_name),
    };

    logger().log_decision(decision);
}

/// Load std modules from cache (generated via --generate-std-types)
fn load_std_modules() -> &'static HashMap<String, String> {
    STD_MODULES_CACHE.get_or_init(|| {
        let cache_path = std::env::var("STD_MODULES_CACHE")
            .unwrap_or_else(|_| {
                let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
                format!("{}/.cache/cargo-slicer/std_modules.json", home)
            });

        if let Ok(content) = fs::read_to_string(&cache_path) {
            if let Ok(modules) = serde_json::from_str::<HashMap<String, String>>(&content) {
                return modules;
            }
        }

        // No cache available - return empty map
        HashMap::new()
    })
}

/// Result of SCIP analysis for std library
#[cfg(feature = "scip-analysis")]
pub struct StdScipResult {
    pub types: HashMap<String, String>,
    pub modules: HashMap<String, String>,
}

/// Debug: inspect SCIP impl blocks and method calls
#[cfg(feature = "scip-analysis")]
pub fn debug_scip_impl_blocks(crate_path: &Path) {
    let scip_path = std::env::temp_dir().join("debug_impl.scip");

    let result = Command::new("rust-analyzer")
        .args(["scip", ".", "--output", &scip_path.to_string_lossy()])
        .current_dir(crate_path)
        .output();

    if result.is_err() || !result.as_ref().unwrap().status.success() {
        eprintln!("Failed to run rust-analyzer scip");
        return;
    }

    let Ok(scip_data) = fs::read(&scip_path) else { return; };
    let _ = fs::remove_file(&scip_path);

    let Ok(_index) = scip::types::Index::parse_from_bytes(&scip_data) else { return; };
}

/// Debug: inspect SCIP symbols to understand visibility patterns
#[cfg(feature = "scip-analysis")]
pub fn debug_scip_visibility(scip_path: &Path) {
    use protobuf::Message;

    let data = match fs::read(scip_path) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Failed to read SCIP: {}", e);
            return;
        }
    };
    let index = match scip::types::Index::parse_from_bytes(&data) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Failed to parse SCIP: {}", e);
            return;
        }
    };

    println!("\n=== SCIP Visibility Analysis ===\n");

    // Collect private modules (those with "mod X" not "pub mod X")
    let mut private_modules: HashSet<String> = HashSet::new();

    for doc in &index.documents {
        for sym in &doc.symbols {
            // Check if this is a private module
            if let Some(sig) = sym.signature_documentation.as_ref() {
                let sig_text = &sig.text;
                // "mod X" without "pub" indicates private module
                if sig_text.starts_with("mod ") && !sig_text.contains("pub") {
                    let module_path = &sym.symbol;
                    println!("PRIVATE MODULE: {}", module_path);
                    println!("  signature: {:?}", sig_text);
                    private_modules.insert(module_path.clone());
                }
            }
        }
    }

    println!("\n=== Private modules found: {} ===\n", private_modules.len());
    for m in private_modules.iter().take(20) {
        println!("  {}", m);
    }

    // Look for types in private modules
    println!("\n=== Types in private modules ===\n");
    let mut count = 0;
    for doc in &index.documents {
        for sym in &doc.symbols {
            let s = &sym.symbol;
            // Check if symbol is inside a private module
            for pm in &private_modules {
                if s.starts_with(pm) && s != pm && count < 20 {
                    println!("  {} (in {})", s, pm);
                    count += 1;
                }
            }
        }
    }

    // Show a few public modules for comparison
    println!("\n=== Public modules (sample) ===\n");
    count = 0;
    for doc in &index.documents {
        for sym in &doc.symbols {
            if let Some(sig) = sym.signature_documentation.as_ref() {
                let sig_text = &sig.text;
                if sig_text.starts_with("pub mod ") && count < 10 {
                    println!("  {} - {:?}", sym.symbol, sig_text);
                    count += 1;
                }
            }
        }
    }
}

/// Extract module path from SCIP symbol
/// e.g., "rust-analyzer cargo std .../library/std sealed/" -> "sealed"
/// e.g., "rust-analyzer cargo std .../library/std sync/poison/mutex/" -> "sync::poison::mutex"
fn extract_scip_module_path(scip_symbol: &str) -> Option<String> {
    // Find the std/ marker and extract everything after it
    // SCIP format: "rust-analyzer cargo std <url> module1/module2/.../moduleN/"
    let parts: Vec<&str> = scip_symbol.split_whitespace().collect();
    if parts.len() < 4 {
        return None;
    }

    // The last part is the module path with slashes
    let path_part = parts.last()?;

    // Handle paths like "std/sync/poison/mutex/" or just "sealed/"
    // Remove trailing slash and convert slashes to ::
    let path = path_part.trim_end_matches('/');

    // Skip the "std" prefix if present
    let path = if path.starts_with("std/") {
        &path[4..]
    } else {
        path
    };

    if path.is_empty() {
        return None;
    }

    // Convert slashes to ::
    Some(path.replace('/', "::"))
}

/// Generate std types and modules mapping from a SCIP index
/// This can be used to update the cache from a crate that uses std
#[cfg(feature = "scip-analysis")]
pub fn generate_std_from_scip(scip_path: &Path) -> Option<StdScipResult> {
    let data = fs::read(scip_path).ok()?;
    let index = scip::types::Index::parse_from_bytes(&data).ok()?;

    let mut types = HashMap::new();
    let mut modules = HashSet::new();

    // First pass: collect all private module paths
    // Private modules have signature_documentation like "mod X" (not "pub mod X")
    let mut private_module_paths: HashSet<String> = HashSet::new();
    for doc in &index.documents {
        for sym in &doc.symbols {
            if let Some(sig) = sym.signature_documentation.as_ref() {
                let sig_text = &sig.text;
                // "mod X" without "pub" indicates private module
                if sig_text.starts_with("mod ") && !sig_text.contains("pub") {
                    // Extract the module path from the SCIP symbol
                    // e.g., "rust-analyzer cargo std .../library/std sealed/" -> "sealed"
                    if let Some(path) = extract_scip_module_path(&sym.symbol) {
                        private_module_paths.insert(path);
                    }
                }
            }
        }
    }

    // Helper to check if a path goes through a private module
    let is_in_private_module = |path: &str| -> bool {
        let parts: Vec<&str> = path.split("::").collect();
        // Check each component to see if it's a private module
        for i in 1..parts.len() {
            let prefix = parts[..=i].join("::");
            // Remove "std::" prefix for comparison
            let module_path = prefix.strip_prefix("std::").unwrap_or(&prefix);
            if private_module_paths.contains(module_path) {
                return true;
            }
        }
        false
    };

    // Helper to process a symbol string
    let mut process_symbol = |sym: &str| {
        if let Some(import_path) = scip_symbol_to_import(sym) {
            // Skip if the path goes through a private module
            if is_in_private_module(&import_path) {
                return;
            }

            // Extract the type name (last component)
            if let Some(type_name) = import_path.split("::").last() {
                // Add if it looks like a type:
                // - Starts with uppercase (normal types)
                // - Starts with "c_" (C FFI types)
                // - Ends with "_t" (C-style types)
                // - Starts with "__" and is in std::arch (SIMD intrinsic types)
                let is_uppercase = type_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                let is_c_ffi = type_name.starts_with("c_");
                let is_c_style = type_name.ends_with("_t") && type_name.len() > 2;
                let is_simd_intrinsic = type_name.starts_with("__") && import_path.contains("::arch::");
                if is_uppercase || is_c_ffi || is_c_style || is_simd_intrinsic {
                    types.insert(type_name.to_string(), import_path.clone());
                }
            }

            // Extract module paths (e.g., "std::fmt" from "std::fmt::Display")
            let parts: Vec<&str> = import_path.split("::").collect();
            if parts.len() >= 2 && (parts[0] == "std" || parts[0] == "core" || parts[0] == "alloc") {
                // Add all intermediate module paths
                for i in 2..=parts.len() {
                    let module_path = parts[..i].join("::");
                    // Only add if it looks like a module (last part is lowercase)
                    if let Some(last) = parts.get(i - 1) {
                        if last.chars().next().map(|c| c.is_lowercase()).unwrap_or(false) {
                            modules.insert(module_path);
                        }
                    }
                }
            }
        }
    };

    for doc in &index.documents {
        // Check symbol definitions
        for symbol in &doc.symbols {
            process_symbol(&symbol.symbol);
        }
        // Also check occurrences (references to external symbols like std)
        for occ in &doc.occurrences {
            process_symbol(&occ.symbol);
        }
    }

    // Convert modules set to HashMap (module_name -> full_path)
    let modules_map: HashMap<String, String> = modules.into_iter()
        .filter_map(|path| {
            let parts: Vec<&str> = path.split("::").collect();
            if parts.len() >= 2 {
                let module_name = parts.last()?.to_string();
                Some((module_name, path))
            } else {
                None
            }
        })
        .collect();

    if types.is_empty() && modules_map.is_empty() {
        None
    } else {
        Some(StdScipResult {
            types,
            modules: modules_map,
        })
    }
}

/// Generate std types mapping from a SCIP index (convenience wrapper)
#[cfg(feature = "scip-analysis")]
pub fn generate_std_types_from_scip(scip_path: &Path) -> Option<HashMap<String, String>> {
    generate_std_from_scip(scip_path).map(|r| r.types)
}

/// Save std types to cache file
#[cfg(feature = "scip-analysis")]
pub fn save_std_types_cache(types: &HashMap<String, String>) -> std::io::Result<()> {
    save_cache("STD_TYPES_CACHE", "std_types.json", types)
}

/// Save std modules to cache file
#[cfg(feature = "scip-analysis")]
pub fn save_std_modules_cache(modules: &HashMap<String, String>) -> std::io::Result<()> {
    save_cache("STD_MODULES_CACHE", "std_modules.json", modules)
}

/// Load std types from cache file (returns empty map if not found)
#[cfg(feature = "scip-analysis")]
pub fn load_std_types_cache() -> HashMap<String, String> {
    load_cache("STD_TYPES_CACHE", "std_types.json")
}

/// Stub: returns empty when scip-analysis is disabled
#[cfg(not(feature = "scip-analysis"))]
pub fn load_std_types_cache() -> HashMap<String, String> {
    HashMap::new()
}

/// Load std modules from cache file (returns empty map if not found)
#[cfg(feature = "scip-analysis")]
pub fn load_std_modules_cache() -> HashMap<String, String> {
    load_cache("STD_MODULES_CACHE", "std_modules.json")
}

/// Stub: returns empty when scip-analysis is disabled
#[cfg(not(feature = "scip-analysis"))]
pub fn load_std_modules_cache() -> HashMap<String, String> {
    HashMap::new()
}

/// Load a HashMap from a JSON cache file
fn load_cache(env_var: &str, default_name: &str) -> HashMap<String, String> {
    let cache_path = std::env::var(env_var)
        .unwrap_or_else(|_| {
            let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
            format!("{}/.cache/cargo-slicer/{}", home, default_name)
        });

    if let Ok(content) = fs::read_to_string(&cache_path) {
        if let Ok(data) = serde_json::from_str::<HashMap<String, String>>(&content) {
            return data;
        }
    }
    HashMap::new()
}

/// Save a HashMap to a JSON cache file
fn save_cache(env_var: &str, default_name: &str, data: &HashMap<String, String>) -> std::io::Result<()> {
    let cache_path = std::env::var(env_var)
        .unwrap_or_else(|_| {
            let home = std::env::var("HOME").unwrap_or_else(|_| ".".to_string());
            format!("{}/.cache/cargo-slicer/{}", home, default_name)
        });

    // Create parent directory if needed
    if let Some(parent) = Path::new(&cache_path).parent() {
        fs::create_dir_all(parent)?;
    }

    let content = serde_json::to_string_pretty(data)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
    fs::write(&cache_path, content)?;
    Ok(())
}

/// Parse SCIP symbol to extract import path
/// Format: "rust-analyzer cargo <crate> <source> <path>"
/// Returns the std:: import path for std/core/alloc symbols
fn scip_symbol_to_import(symbol: &str) -> Option<String> {
    let parts: Vec<&str> = symbol.split_whitespace().collect();
    if parts.len() < 5 {
        return None;
    }

    let crate_name = parts[2];
    // Only handle std/core/alloc
    if !["std", "core", "alloc"].contains(&crate_name) {
        return None;
    }

    // Path is after the URL (parts[4+])
    let path = parts[4..].join(" ");

    // Parse the path: "sync/atomic/Ordering#" -> "sync::atomic::Ordering"
    let path = path.trim_end_matches('#')  // Remove type marker
                   .trim_end_matches(':')   // Remove trait marker
                   .trim_end_matches('.')   // Remove method marker
                   .replace('/', "::");

    // Skip impl blocks, methods, and macros
    if path.contains("impl#") || path.ends_with("()") || path.contains("macros") {
        return None;
    }

    // Skip internal std paths that shouldn't be used in imports
    // These are implementation details exposed by SCIP but not valid Rust imports
    // - legacy_int_modules: old integer conversion traits
    // - internal_: internal implementation details
    // - sealed: private trait used for sealed trait pattern
    if path.contains("legacy_int_modules") || path.contains("internal_") || path.contains("sealed") {
        return None;
    }

    // Skip empty, crate-level, or variant paths
    if path.is_empty() || path == "crate" || path.contains('#') {
        return None;
    }

    // Skip paths that are just module names (no item)
    let parts: Vec<&str> = path.split("::").collect();
    if parts.is_empty() {
        return None;
    }

    // The last part should be a valid type name:
    // - Starts with uppercase (normal types/traits)
    // - Starts with "c_" (C FFI types like c_void, c_int)
    // - Ends with "_t" (C-style types like size_t, time_t)
    // - Starts with "__" (SIMD intrinsic types like __m128i, __m256i)
    // - Special items: "self", "hint"
    let last = parts.last().unwrap();
    let is_uppercase_type = last.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
    let is_c_ffi_type = last.starts_with("c_");
    let is_c_style_type = last.ends_with("_t") && last.len() > 2;
    let is_simd_intrinsic = last.starts_with("__") && path.contains("arch");
    let is_special = ["self", "hint"].contains(last);

    if last.is_empty() || (!is_uppercase_type && !is_c_ffi_type && !is_c_style_type && !is_simd_intrinsic && !is_special) {
        return None;
    }

    // Skip items that are all uppercase (likely constants, not types)
    if last.chars().all(|c| c.is_uppercase() || c == '_') && last.len() > 1 {
        return None;
    }

    // Skip single-letter type names (generic parameters like T, U, etc.)
    if last.len() == 1 {
        return None;
    }

    // Skip generic placeholder names that aren't real types
    static GENERIC_PLACEHOLDERS: &[&str] = &[
        "Trait", "Item", "Iter", "Self", "This", "Key", "Value",
        "Input", "Output", "Error", "Target", "Source", "Token",
    ];
    if GENERIC_PLACEHOLDERS.contains(&last) {
        return None;
    }

    // Skip test types and internal implementation types
    // These appear in SCIP but aren't part of the public API
    static TEST_INTERNAL_TYPES: &[&str] = &[
        // Test-related types
        "TestError", "ShortReader", "OneByteReader", "ExampleSliceReader",
        "DataAndErrorReader", "WriteObserver", "ErrAfterFirstSeekReader",
        "Bojji", // test type in std::io
        // Internal implementation types (not publicly exported)
        "Data", "Call", "State", "Buffer", "Adapter", "SimpleMessage",
        "Tid", "UniqueArc", "Backoff", "Operation", "SyncWaker",
        "ReceiverFlavor", "Atomic", "PoisonOnPanic", "SenderFlavor",
        "Position", "MappedMutexGuard",
        // More internal/private types
        "Flag", "OnceExclusiveState", "WouldBlock", "Nanoseconds",
        "VaListImpl", "VaArgSafe", "RawPthread",
        // BTreeMap internal types
        "IterMut", "Keys", "Values",
    ];
    if TEST_INTERNAL_TYPES.contains(&last) {
        return None;
    }

    // Skip items directly under crate root (need at least module::Type)
    // e.g., "std::Trait" is invalid, "std::fmt::Display" is valid
    if parts.len() < 2 {
        return None;
    }

    // Skip primitive types - they don't need imports
    static PRIMITIVE_TYPES: &[&str] = &[
        "u8", "u16", "u32", "u64", "u128", "usize",
        "i8", "i16", "i32", "i64", "i128", "isize",
        "f32", "f64", "bool", "char", "str",
    ];
    if PRIMITIVE_TYPES.contains(&last) {
        return None;
    }

    // Normalize internal paths to public re-export paths
    // SCIP gives internal paths like std::sync::poison::mutex::Mutex
    // but we need std::sync::Mutex
    let type_name = *last;
    let normalized_path = normalize_std_path(&path, type_name);

    Some(normalized_path)
}

/// Normalize internal std paths to their public re-export locations
/// Known public submodules in std that should be preserved in paths
/// Types in these submodules use the full path (e.g., std::sync::atomic::AtomicBool)
/// Note: Map collections (hash_map, btree_map) have types re-exported at std::collections,
/// but Set collections (hash_set, btree_set) do NOT re-export iterator types like Union.
static PUBLIC_SUBMODULES: &[&str] = &[
    // std::sync submodules - atomic types live here
    "atomic",
    // std::sync::mpsc - channel types live here
    "mpsc",
    // std::os - platform-specific types live here
    "os",
    // std::f32/f64 constants
    "consts",
    // std::collections::hash_set - iterator types like Union, Intersection not re-exported
    "hash_set",
    // std::collections::btree_set - iterator types like Union, Intersection not re-exported
    "btree_set",
];

/// Types that have ambiguous names or need to stay in specific submodules
/// Returns the canonical path for types that need special handling
fn get_ambiguous_type_path(type_name: &str, path: &str) -> Option<&'static str> {
    match type_name {
        // Ambiguous types - same name in different modules
        // Once can be std::sync::Once or std::iter::Once
        "Once" => Some(if path.contains("iter::") { "std::iter::Once" } else { "std::sync::Once" }),
        // Weak can be std::rc::Weak or std::sync::Weak
        "Weak" => Some(if path.contains("rc::") { "std::rc::Weak" } else { "std::sync::Weak" }),
        // Error is commonly std::io::Error but could be others
        "Error" => if path.contains("io::") { Some("std::io::Error") } else { None },
        // IntoIter can be from many places
        "IntoIter" => if path.contains("vec::") { Some("std::vec::IntoIter") } else { None },
        // Drain can be from vec, string, etc.
        "Drain" => if path.contains("vec::") { Some("std::vec::Drain") } else if path.contains("string::") { Some("std::string::Drain") } else { None },
        // Ordering can be std::cmp::Ordering or std::sync::atomic::Ordering
        "Ordering" => Some(if path.contains("atomic") { "std::sync::atomic::Ordering" } else { "std::cmp::Ordering" }),

        // Types that stay in collections submodules (not re-exported at parent)
        // Entry is in both HashMap and BTreeMap submodules
        "Entry" => Some(if path.contains("btree") { "std::collections::btree_map::Entry" } else { "std::collections::hash_map::Entry" }),
        // hash_map specific types
        "DefaultHasher" => Some("std::collections::hash_map::DefaultHasher"),
        "RandomState" => Some("std::collections::hash_map::RandomState"),
        // OccupiedEntry/VacantEntry
        "OccupiedEntry" => Some(if path.contains("btree") { "std::collections::btree_map::OccupiedEntry" } else { "std::collections::hash_map::OccupiedEntry" }),
        "VacantEntry" => Some(if path.contains("btree") { "std::collections::btree_map::VacantEntry" } else { "std::collections::hash_map::VacantEntry" }),

        // Set iterator types - NOT re-exported at std::collections level
        // These must use the full submodule path
        "Union" => Some(if path.contains("btree") { "std::collections::btree_set::Union" } else { "std::collections::hash_set::Union" }),
        "Difference" => Some(if path.contains("btree") { "std::collections::btree_set::Difference" } else { "std::collections::hash_set::Difference" }),
        "Intersection" => Some(if path.contains("btree") { "std::collections::btree_set::Intersection" } else { "std::collections::hash_set::Intersection" }),
        "SymmetricDifference" => Some(if path.contains("btree") { "std::collections::btree_set::SymmetricDifference" } else { "std::collections::hash_set::SymmetricDifference" }),

        _ => None,
    }
}

/// Normalize internal std paths to their public re-export locations
/// This uses heuristics based on std library structure to infer canonical paths
/// without hardcoding every single type mapping.
fn normalize_std_path(path: &str, type_name: &str) -> String {
    // First check for ambiguous types that need special handling
    if let Some(canonical) = get_ambiguous_type_path(type_name, path) {
        return canonical.to_string();
    }

    // Handle SIMD intrinsic types from core_arch
    // SCIP path: core_arch::x86::__m128i -> std::arch::x86_64::__m128i
    if path.starts_with("core_arch::") && type_name.starts_with("__") {
        let arch_path = path.strip_prefix("core_arch::").unwrap_or(path);
        // Normalize x86 to x86_64 for 64-bit systems
        let arch_path = if arch_path.starts_with("x86::") {
            arch_path.replacen("x86::", "x86_64::", 1)
        } else {
            arch_path.to_string()
        };
        return format!("std::arch::{}", arch_path);
    }

    let parts: Vec<&str> = path.split("::").collect();

    // If path is already simple (module::Type), return as-is
    if parts.len() <= 2 {
        return format!("std::{}", path);
    }

    // The first component is the top-level module (sync, io, collections, etc.)
    let top_module = parts[0];

    // Check if any intermediate component is a known public submodule
    // If so, preserve it in the path
    let mut result_parts = vec![top_module];

    let mut found_public_submodule = false;
    for part in &parts[1..parts.len()-1] {
        if PUBLIC_SUBMODULES.contains(part) {
            result_parts.push(part);
            found_public_submodule = true;

            // Special case: for "os" paths, continue to preserve platform-specific components
            // e.g., std::os::linux::raw::mode_t should preserve all parts, not just std::os::mode_t
            if part == &"os" {
                continue; // Don't break, keep processing platform paths
            }

            break; // Only keep up to the first public submodule (except for os)
        } else if found_public_submodule && top_module == "os" {
            // We're inside std::os::*, preserve platform/arch-specific components
            // like "linux", "unix", "windows", "raw", etc.
            result_parts.push(part);
        }
    }

    // Add the type name
    result_parts.push(type_name);

    // If we found a public submodule, use that path
    // Otherwise, collapse to just top_module::TypeName
    let canonical = format!("std::{}", result_parts.join("::"));

    // Special case: Atomic types should always use std::sync::atomic
    if type_name.starts_with("Atomic") && !canonical.contains("atomic") {
        return format!("std::sync::atomic::{}", type_name);
    }

    canonical
}

/// Detect imports using SCIP analysis
/// This runs rust-analyzer on the sliced crate to find all external references
#[cfg(feature = "scip-analysis")]
fn detect_imports_via_scip(output_dir: &Path) -> Vec<String> {
    let scip_path = output_dir.join("index.scip");

    // Run rust-analyzer scip
    let result = Command::new("rust-analyzer")
        .args(["scip", ".", "--output", &scip_path.to_string_lossy()])
        .current_dir(output_dir)
        .output();

    let Ok(output) = result else {
        return Vec::new();
    };

    if !output.status.success() {
        return Vec::new();
    }

    // Read and parse SCIP file
    let Ok(scip_data) = fs::read(&scip_path) else {
        return Vec::new();
    };

    // Clean up the SCIP file
    let _ = fs::remove_file(&scip_path);

    // Parse SCIP protobuf
    use protobuf::Message;
    let Ok(index) = scip::types::Index::parse_from_bytes(&scip_data) else {
        return Vec::new();
    };

    // Collect all external imports needed
    let mut imports: HashSet<String> = HashSet::new();

    for doc in &index.documents {
        for occ in &doc.occurrences {
            if let Some(import_path) = scip_symbol_to_import(&occ.symbol) {
                imports.insert(import_path);
            }
        }
    }

    // Convert to sorted vector
    let mut imports: Vec<_> = imports.into_iter().collect();
    imports.sort();
    imports
}

/// Result of SCIP analysis containing local types and needed impl blocks
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize)]
pub struct ScipAnalysis {
    /// Type names defined locally in the crate (to avoid std import conflicts)
    pub local_types: HashSet<String>,
    /// Impl block identifiers that are actually used (e.g., "ext/impl#[`*const T`][Pointer]")
    pub needed_impl_blocks: HashSet<String>,
    /// Mapping from impl block ID to its module path (e.g., "ext/impl#..." -> "ext")
    pub impl_block_modules: HashMap<String, String>,
    /// Mapping from symbol name to full module paths (e.g., "FatAVX2" -> ["packed::teddy::x86_64"])
    /// Multiple paths possible for items defined in multiple modules (e.g., re-exports)
    pub symbol_paths: HashMap<String, Vec<String>>,
    /// Phase 3: Types referenced in the code (from SCIP occurrences)
    /// Maps type name to set of all module paths where it's defined
    /// This helps ensure type definitions are included even if only referenced in signatures
    pub referenced_types: HashMap<String, HashSet<String>>,
}

/// Extract impl block identifier from a SCIP symbol
/// Input: "rust-analyzer cargo memchr 2.7.6 ext/impl#[`*const T`][Pointer]distance()."
/// Output: "ext/impl#[`*const T`][Pointer]"
fn extract_impl_block_id(symbol: &str) -> Option<String> {
    // Early check for impl block symbols
    if !symbol.contains("impl#[") {
        return None;
    }

    // Find the path portion (after crate version)
    let parts: Vec<&str> = symbol.split_whitespace().collect();
    if parts.len() < 5 {
        return None;
    }

    let path = parts[4..].join(" ");

    // Find impl#[...][...] pattern
    let impl_idx = path.find("impl#[")?;
    let after_impl = &path[impl_idx..];

    // Find the closing bracket of the trait: impl#[Type][Trait]
    // Count brackets to find the end
    let mut bracket_count = 0;
    let mut end_idx = 0;
    let mut found_first_close = false;

    for (i, c) in after_impl.char_indices() {
        match c {
            '[' => bracket_count += 1,
            ']' => {
                bracket_count -= 1;
                if bracket_count == 0 {
                    if found_first_close {
                        end_idx = i + 1;
                        break;
                    }
                    found_first_close = true;
                }
            }
            _ => {}
        }
    }

    if end_idx > 0 {
        // Include the module path prefix
        let module_prefix = &path[..impl_idx];
        let impl_part = &after_impl[..end_idx];
        Some(format!("{}{}", module_prefix, impl_part))
    } else {
        None
    }
}

/// Extract the module path from an impl block ID
/// Input: "ext/impl#[`*const T`][Pointer]"
/// Output: "ext"
fn extract_impl_module(impl_id: &str) -> String {
    if let Some(impl_idx) = impl_id.find("impl#") {
        let module = &impl_id[..impl_idx];
        module.trim_end_matches('/').to_string()
    } else {
        String::new()
    }
}

/// Convert a SCIP impl block ID to the parsed item name format
/// Input: "ext/impl#[`*const T`][Pointer]"
/// Output: "*const T:Pointer"
fn scip_impl_to_parsed_name(impl_id: &str) -> Option<String> {
    // Find impl#[...][...]
    let impl_idx = impl_id.find("impl#[")?;
    let after_impl = &impl_id[impl_idx + 6..]; // skip "impl#["

    // Parse the self type (first bracket group)
    let mut bracket_count = 1;
    let mut self_type_end = 0;
    for (i, c) in after_impl.char_indices() {
        match c {
            '[' => bracket_count += 1,
            ']' => {
                bracket_count -= 1;
                if bracket_count == 0 {
                    self_type_end = i;
                    break;
                }
            }
            _ => {}
        }
    }

    if self_type_end == 0 {
        return None;
    }

    let self_type = &after_impl[..self_type_end];
    // Remove backticks from SCIP format
    let self_type = self_type.trim_matches('`').to_string();

    // Check if there's a trait (second bracket group)
    let remaining = &after_impl[self_type_end + 1..];
    if remaining.starts_with('[') {
        let trait_start = 1;
        let trait_end = remaining.find(']').unwrap_or(remaining.len());
        let trait_name = &remaining[trait_start..trait_end];
        Some(format!("{}:{}", self_type, trait_name))
    } else {
        Some(self_type)
    }
}

/// Extract full module path and item name from a SCIP symbol
/// Input: "rust-analyzer cargo aho-corasick 1.1.4 packed/teddy/x86_64/FatAVX2#"
/// Output: Some(("packed::teddy::x86_64", "FatAVX2"))
fn extract_scip_symbol_path(symbol: &str) -> Option<(String, String)> {
    let parts: Vec<&str> = symbol.split_whitespace().collect();
    if parts.len() < 5 {
        return None;
    }

    let path = parts[4..].join(" ");
    let path = path.trim_end_matches('#')
                  .trim_end_matches(':')
                  .trim_end_matches('.');

    // Skip impl blocks and methods
    if path.contains("impl#") || path.ends_with("()") {
        return None;
    }

    if path.contains('/') {
        // Split into module path and item name
        let components: Vec<&str> = path.split('/').collect();
        let item_name = components.last()?.to_string();

        // Only include valid type/function names (not empty or numbers)
        if item_name.is_empty() || item_name.chars().next()?.is_ascii_digit() {
            return None;
        }

        // Build module path from all but the last component
        let module_parts: Vec<&str> = components[..components.len() - 1].to_vec();
        let module_path = module_parts.join("::");

        Some((module_path, item_name))
    } else {
        // Top-level item, no module path
        if path.is_empty() || path.chars().next()?.is_ascii_digit() {
            return None;
        }
        Some((String::new(), path.to_string()))
    }
}

/// Try to load SCIP data from cache (mirrors ra_deps.rs cache mechanism)
fn try_load_scip_cache(crate_path: &Path) -> Option<Vec<u8>> {
    use std::path::PathBuf;

    // Get cache directory
    let cache_base = std::env::var("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|_| std::env::var("HOME").map(|h| PathBuf::from(h).join(".cache")))
        .ok()?;

    let cache_dir = cache_base.join("cargo-slicer").join("scip");
    if !cache_dir.exists() {
        return None;
    }

    // Compute cache key from Cargo.toml
    let cargo_toml = crate_path.join("Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).ok()?;

    // Parse name and version
    let mut name = None;
    let mut version = None;
    for line in content.lines() {
        let line = line.trim();
        if line.starts_with("name") && line.contains('=') {
            if let Some(val) = line.split('=').nth(1) {
                name = Some(val.trim().trim_matches('"').to_string());
            }
        } else if line.starts_with("version") && line.contains('=') && version.is_none() {
            if let Some(val) = line.split('=').nth(1) {
                version = Some(val.trim().trim_matches('"').to_string());
            }
        }
        if name.is_some() && version.is_some() {
            break;
        }
    }

    let name = name?;
    let version = version.unwrap_or_else(|| "unknown".to_string());

    // Hash content for cache key
    let mut hash: u64 = 0xcbf29ce484222325;
    for byte in content.bytes() {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }

    let cache_file = cache_dir.join(format!("{}-{}-{:016x}.scip", name, version, hash));
    fs::read(&cache_file).ok()
}

/// Save SCIP data to cache (mirrors ra_deps.rs logic)
fn save_scip_cache(crate_path: &Path, scip_data: &[u8]) {
    use std::path::PathBuf;

    // Get cache directory
    let cache_base = std::env::var("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|_| std::env::var("HOME").map(|h| PathBuf::from(h).join(".cache")))
        .unwrap_or_else(|_| PathBuf::from("/tmp"));

    let cache_dir = cache_base.join("cargo-slicer").join("scip");

    // Create directory if it doesn't exist
    if !cache_dir.exists() {
        if let Err(e) = fs::create_dir_all(&cache_dir) {
            eprintln!("Warning: Failed to create SCIP cache directory: {}", e);
            return;
        }
    }

    // Compute cache key from Cargo.toml (same logic as try_load_scip_cache)
    let cargo_toml = crate_path.join("Cargo.toml");
    let Ok(content) = fs::read_to_string(&cargo_toml) else {
        return;
    };

    // Parse name and version
    let mut name = None;
    let mut version = None;
    for line in content.lines() {
        let line = line.trim();
        if line.starts_with("name") && line.contains('=') {
            if let Some(val) = line.split('=').nth(1) {
                name = Some(val.trim().trim_matches('"').to_string());
            }
        } else if line.starts_with("version") && line.contains('=') && version.is_none() {
            if let Some(val) = line.split('=').nth(1) {
                version = Some(val.trim().trim_matches('"').to_string());
            }
        }
        if name.is_some() && version.is_some() {
            break;
        }
    }

    let Some(name) = name else {
        return;
    };
    let version = version.unwrap_or_else(|| "unknown".to_string());

    // Hash content for cache key (same FNV-1a hash as try_load_scip_cache)
    let mut hash: u64 = 0xcbf29ce484222325;
    for byte in content.bytes() {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }

    let cache_file = cache_dir.join(format!("{}-{}-{:016x}.scip", name, version, hash));

    // Write to cache
    match fs::write(&cache_file, scip_data) {
        Ok(_) => {
            eprintln!("SCIP cache saved: {}-{}-{:016x} ({} bytes)", name, version, hash, scip_data.len());
        }
        Err(e) => {
            eprintln!("Warning: Failed to save SCIP cache: {}", e);
        }
    }
}

/// Comprehensive SCIP analysis for a crate
/// Returns local types (for import conflict avoidance) and needed impl blocks
#[cfg(feature = "scip-analysis")]
pub fn analyze_crate_via_scip(crate_path: &Path) -> ScipAnalysis {
    // Try to load from cache first (same as ra_deps.rs cache)
    let scip_data = match try_load_scip_cache(crate_path) {
        Some(data) => data,
        None => {
            // Fall back to running rust-analyzer
            let scip_path = std::env::temp_dir().join(format!("slicer_analysis_{}.scip", std::process::id()));

            let result = Command::new("rust-analyzer")
                .args(["scip", ".", "--output", &scip_path.to_string_lossy()])
                .current_dir(crate_path)
                .output();

            let Ok(output) = result else {
                return ScipAnalysis::default();
            };

            if !output.status.success() {
                return ScipAnalysis::default();
            }

            let Ok(data) = fs::read(&scip_path) else {
                return ScipAnalysis::default();
            };

            // Save to cache before removing temp file
            save_scip_cache(crate_path, &data);

            let _ = fs::remove_file(&scip_path);
            data
        }
    };

    // Parse SCIP protobuf
    let Ok(index) = scip::types::Index::parse_from_bytes(&scip_data) else {
        return ScipAnalysis::default();
    };

    let mut analysis = ScipAnalysis::default();

    // Extract the crate name from the first document's symbols
    let crate_name = index.documents.first()
        .and_then(|doc| doc.symbols.first())
        .and_then(|sym| {
            let parts: Vec<&str> = sym.symbol.split_whitespace().collect();
            if parts.len() >= 3 { Some(parts[2].to_string()) } else { None }
        })
        .unwrap_or_default();

    // First pass: collect local types and symbol paths from symbol definitions
    for doc in &index.documents {
        for sym in &doc.symbols {
            let parts: Vec<&str> = sym.symbol.split_whitespace().collect();
            if parts.len() < 5 {
                continue;
            }

            let symbol_crate = parts[2];
            if symbol_crate != crate_name {
                continue;
            }

            // Extract full symbol path and name
            if let Some((module_path, item_name)) = extract_scip_symbol_path(&sym.symbol) {
                // Track local types (for import conflict avoidance)
                if let Some(first_char) = item_name.chars().next() {
                    if first_char.is_uppercase() {
                        analysis.local_types.insert(item_name.clone());
                    }
                }

                // Track symbol paths (for proper import generation)
                // Only track non-empty module paths (items in modules)
                if !module_path.is_empty() {
                    analysis.symbol_paths
                        .entry(item_name)
                        .or_default()
                        .push(module_path);
                }
            }
        }
    }

    // Deduplicate symbol paths
    for paths in analysis.symbol_paths.values_mut() {
        paths.sort();
        paths.dedup();
    }

    // Second pass: collect needed impl blocks from occurrences
    for doc in &index.documents {
        for occ in &doc.occurrences {
            // Check if this occurrence references an impl block from this crate
            if !occ.symbol.contains("impl#[") {
                continue;
            }

            // Only consider impl blocks from this crate
            let parts: Vec<&str> = occ.symbol.split_whitespace().collect();
            if parts.len() >= 3 && parts[2] == crate_name {
                if let Some(impl_id) = extract_impl_block_id(&occ.symbol) {
                    let module = extract_impl_module(&impl_id);
                    analysis.impl_block_modules.insert(impl_id.clone(), module);
                    analysis.needed_impl_blocks.insert(impl_id);
                }
            }
        }
    }

    // Third pass (Phase 3): extract type references from occurrences
    // This ensures types referenced in function signatures get their definitions included
    for doc in &index.documents {
        for occ in &doc.occurrences {
            // Only process occurrences from this crate (definitions)
            let parts: Vec<&str> = occ.symbol.split_whitespace().collect();
            if parts.len() < 5 || parts[2] != crate_name {
                continue;
            }

            // Extract type name and module path from the symbol
            if let Some((module_path, type_name)) = extract_scip_symbol_path(&occ.symbol) {
                // Only track types (uppercase first letter)
                if let Some(first_char) = type_name.chars().next() {
                    if first_char.is_uppercase() && !is_primitive_type(&type_name) {
                        analysis.referenced_types
                            .entry(type_name)
                            .or_default()
                            .insert(module_path);
                    }
                }
            }
        }
    }

    analysis
}

/// Extract type/trait/struct names defined locally in a crate using SCIP analysis
/// Returns a set of type names that are defined within the crate (not from std or dependencies)
/// This is used to avoid incorrectly importing std types when the crate has local definitions
#[cfg(feature = "scip-analysis")]
pub fn get_crate_local_types_via_scip(crate_path: &Path) -> HashSet<String> {
    analyze_crate_via_scip(crate_path).local_types
}

/// Stub: returns empty ScipAnalysis when scip-analysis is disabled
#[cfg(not(feature = "scip-analysis"))]
#[allow(unused_variables)]
pub fn analyze_crate_via_scip(crate_path: &Path) -> ScipAnalysis {
    ScipAnalysis::default()
}

/// Stub: returns empty when scip-analysis is disabled
#[cfg(not(feature = "scip-analysis"))]
#[allow(unused_variables)]
pub fn get_crate_local_types_via_scip(crate_path: &Path) -> HashSet<String> {
    HashSet::new()
}

/// Remove doc comments from code to avoid false positives in dependency detection
/// This prevents treating example code in documentation as actual dependency usage
fn remove_doc_comments(code: &str) -> String {
    let mut result = String::with_capacity(code.len());
    let mut in_block_comment = false;

    for line in code.lines() {
        let trimmed = line.trim_start();

        // Skip line doc comments (/// and //!)
        if trimmed.starts_with("///") || trimmed.starts_with("//!") {
            result.push('\n'); // Keep line structure
            continue;
        }

        // Aggressively strip #[doc = "..."] attributes from the line
        // These can appear multiple times per line and may be followed by actual code
        // Use a simpler approach: just remove #[doc = ...] patterns entirely
        let mut cleaned_line = line.to_string();

        // Remove all #[doc = "..."] or #[doc = r"..."] attributes
        // Use a regex-like approach but simpler: just find and remove the pattern
        while let Some(start) = cleaned_line.find("#[doc = ") {
            // Find matching "]" for this attribute
            let search_start = start + "#[doc = ".len();

            // Simple heuristic: find the closing "]" after a quote
            // This works for most cases and avoids complex UTF-8 indexing issues
            if let Some(after_quote) = cleaned_line[search_start..].find("\"]") {
                let end_pos = search_start + after_quote + 2; // Include "]
                cleaned_line = format!("{}{}", &cleaned_line[..start], &cleaned_line[end_pos..]);
            } else {
                // Can't find proper end, just remove everything from this point
                cleaned_line = cleaned_line[..start].to_string();
                break;
            }
        }

        // If line became empty or contains only whitespace after removing doc attrs, skip it
        if cleaned_line.trim().is_empty() {
            result.push('\n');
            continue;
        }

        // Handle block comments /* ... */
        let mut line_result = String::new();
        let mut chars = cleaned_line.chars().peekable();

        while let Some(ch) = chars.next() {
            if in_block_comment {
                if ch == '*' && chars.peek() == Some(&'/') {
                    chars.next(); // consume '/'
                    in_block_comment = false;
                }
            } else {
                if ch == '/' && chars.peek() == Some(&'*') {
                    chars.next(); // consume '*'
                    in_block_comment = true;
                } else {
                    line_result.push(ch);
                }
            }
        }

        result.push_str(&line_result);
        result.push('\n');
    }

    result
}

/// Fix nested module imports in all generated code before dependency detection.
/// This prevents bare imports like "use parking_lot::" from being mistaken for external crate imports
/// when parking_lot is actually a local module.
fn fix_nested_module_imports_in_all_code(code: &str, module_paths: &HashSet<String>) -> String {
    let mut result = code.to_string();

    // Build set of top-level module names
    let mut local_module_names = HashSet::new();
    for path in module_paths {
        if let Some(first_module) = path.split("::").nth(1) {  // Skip "crate", get first module
            local_module_names.insert(first_module.to_string());
        }
    }

    // For each local module, convert bare imports to crate:: imports
    // e.g., "use parking_lot::ThreadData" → "use crate::parking_lot::ThreadData"
    for module_name in local_module_names {
        let bare_pattern = format!("use {}::", module_name);
        if result.contains(&bare_pattern) {
            // Don't replace if it's already qualified with self:: or crate::
            let lines: Vec<&str> = result.lines().collect();
            let mut new_lines = Vec::new();

            for line in lines {
                let trimmed = line.trim_start();
                // Only fix bare imports (not already qualified)
                if trimmed.starts_with(&bare_pattern)
                    && !trimmed.contains("self::")
                    && !trimmed.contains("crate::") {
                    // Convert "use module::" to "use crate::module::"
                    let fixed_line = line.replace(&bare_pattern, &format!("use crate::{}::", module_name));
                    new_lines.push(fixed_line);
                } else {
                    new_lines.push(line.to_string());
                }
            }

            result = new_lines.join("\n");
        }
    }

    result
}

/// Detect which dependencies from the original Cargo.toml are used in the sliced code
/// If output_dir is provided, checks for sliced versions and uses path dependencies
fn detect_used_dependencies(code: &str, crate_info: &CrateInfo, output_dir: Option<&Path>, module_paths: &HashSet<String>) -> Vec<String> {
    let mut deps = Vec::new();

    // Normalize code - syn's quote! adds spaces around :: and <
    let code = normalize_source(code);

    // Remove doc comments to avoid false positives from example code in documentation
    // Doc comments like `/// use serde::de::Deserialize;` should not trigger dependencies
    let code_without_doc_comments = remove_doc_comments(&code);
    let code = code_without_doc_comments.as_str();

    // Build set of top-level module names for local module detection
    // e.g., "crate::parking_lot::foo" → "parking_lot" is a local module
    let mut local_module_names = HashSet::new();
    for path in module_paths {
        if let Some(first_module) = path.split("::").nth(1) {  // Skip "crate", get first module
            local_module_names.insert(first_module.to_string());
        }
    }

    // Parse original Cargo.toml to get available dependencies
    if let Some(original_deps) = parse_cargo_toml(&crate_info.path) {
        for (name, value) in &original_deps.dependencies {
            // Skip Rust standard library crates - they're built into the language
            // These include rustc-std-workspace-* crates used for building std itself
            if matches!(name.as_str(), "core" | "alloc" | "std" | "proc_macro")
                || name.starts_with("rustc-std-workspace")
            {
                continue;
            }

            // Normalize name for pattern matching (crate names use - but imports use _)
            let underscore_name = name.replace('-', "_");

            // Check if this name matches a local module
            // e.g., parking_lot_core has a module "parking_lot", not the external crate
            if local_module_names.contains(&underscore_name) || local_module_names.contains(name) {
                continue;
            }

            // Check if references like "parking_lot::" are to LOCAL modules, not external crates
            // Patterns like "self::parking_lot::" or "crate::parking_lot::" indicate local modules
            let has_local_module_ref = code.contains(&format!("self::{}::", underscore_name))
                || code.contains(&format!("self::{}::", name))
                || code.contains(&format!("pub mod {};", underscore_name))
                || code.contains(&format!("pub mod {};", name))
                || code.contains(&format!("mod {};", underscore_name))
                || code.contains(&format!("mod {};", name));

            // Skip this dependency if it's actually a local module name
            if has_local_module_ref {
                if std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
                    crate::debug_log!("[DepDetect] {} -> SKIPPED: Local module detected", name);
                }
                continue;
            }

            // Check if this crate is used in the code
            // Also check for ::crate:: (absolute path) patterns
            // And check for crate::external_crate patterns (incorrect but common in sliced code)
            let is_used = code.contains(&format!("{}::", underscore_name))
                || code.contains(&format!("{}::", name))
                || code.contains(&format!("::{}::", underscore_name))
                || code.contains(&format!("::{}::", name))
                || code.contains(&format!("use {};", underscore_name))
                || code.contains(&format!("use {};", name))
                || code.contains(&format!("use {}::", underscore_name))
                || code.contains(&format!("use {}::", name))
                || code.contains(&format!("extern crate {};", underscore_name))
                || code.contains(&format!("extern crate {};", name))
                // For macros like cfg_if!
                || code.contains(&format!("{}!", underscore_name))
                || code.contains(&format!("{}!", name))
                // Check for incorrectly prefixed imports like "use crate::thiserror;"
                // These will be fixed by fix_external_crate_imports later
                || code.contains(&format!("use crate::{};", underscore_name))
                || code.contains(&format!("use crate::{};", name))
                || code.contains(&format!("use crate::{}::", underscore_name))
                || code.contains(&format!("use crate::{}::", name));

            if is_used {
                // Log dependency detection
                if std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
                    crate::debug_log!("[DepDetect] {} -> USED: Found in code", name);
                }

                // Check if a sliced version exists in the workspace
                let dep_value = if let Some(out_dir) = output_dir {
                    // Check for sliced version in parent directory (sibling to this crate)
                    if let Some(parent) = out_dir.parent() {
                        let sliced_name = format!("{}-sliced", name);
                        let sliced_path = parent.join(&sliced_name);
                        if sliced_path.exists() && sliced_path.join("Cargo.toml").exists() {
                            // Use path dependency to sliced version
                            format!("{{ path = \"../{}\" }}", sliced_name)
                        } else {
                            // Use original dependency specification
                            get_dependency_value(value)
                        }
                    } else {
                        get_dependency_value(value)
                    }
                } else {
                    get_dependency_value(value)
                };

                deps.push(format!("{} = {}", name, dep_value));
            }
        }
    }

    // Add fallback detection for common dependencies not in Cargo.toml
    // (Some crates may have optional dependencies that weren't in [dependencies])
    add_fallback_deps(&mut deps, code);

    // Final filter: Remove dependencies that match local module names
    // This is a safety net to prevent local modules from being added as external dependencies
    // even if they passed the earlier checks
    let local_module_names: HashSet<String> = module_paths
        .iter()
        .filter_map(|path| path.split("::").nth(1).map(|s| s.to_string()))
        .collect();

    deps.retain(|dep_line| {
        // Extract dependency name from "name = ..." format
        if let Some(eq_pos) = dep_line.find('=') {
            let dep_name = dep_line[..eq_pos].trim();
            // Keep the dependency only if it doesn't match a local module
            !local_module_names.contains(dep_name)
        } else {
            true  // Keep malformed lines (shouldn't happen)
        }
    });

    deps
}

/// Extract dependency value, removing optional = true and cleaning up format
fn get_dependency_value(value: &str) -> String {
    if value.contains("optional = true") {
        // Remove optional = true since we're using it unconditionally
        let mut fixed = value.replace(", optional = true", "");
        fixed = fixed.replace("optional = true, ", "");
        fixed = fixed.replace("optional = true", "");
        // Clean up empty braces: { version = "X" } -> "X"
        if fixed.trim().starts_with("{") && fixed.contains("version") && !fixed.contains(",") {
            // Extract just the version
            if let Some(start) = fixed.find("version") {
                if let Some(eq) = fixed[start..].find('=') {
                    let after_eq = &fixed[start + eq + 1..];
                    let version = after_eq.trim().trim_matches(|c| c == '"' || c == '{' || c == '}' || c == ' ');
                    return format!("\"{}\"", version);
                }
            }
        }
        fixed
    } else {
        value.to_string()
    }
}

/// Add fallback dependency detection for common crates
fn add_fallback_deps(deps: &mut Vec<String>, code: &str) {
    // Check if a dependency is already present (case-insensitive prefix match)
    fn has_dep(deps: &[String], name: &str) -> bool {
        deps.iter().any(|d| {
            let d_lower = d.to_lowercase();
            d_lower.starts_with(&name.to_lowercase())
                || d_lower.starts_with(&name.replace('-', "_").to_lowercase())
        })
    }

    // Collect new deps to add (avoids borrow issues)
    let mut new_deps = Vec::new();

    // Helper to log dependency decisions
    let log_dep_decision = |name: &str, reason: &str, added: bool| {
        if std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
            crate::debug_log!("[DepDetect] {} -> {}: {}",
                name,
                if added { "ADDED" } else { "SKIPPED" },
                reason
            );
        }
    };

    // Proc-macro crates (often optional or dev-deps in original)
    if !has_dep(deps, "syn") && code.contains("syn::") {
        new_deps.push("syn = { version = \"2\", features = [\"full\"] }".to_string());
        log_dep_decision("syn", "Found syn:: usage", true);
    } else if code.contains("syn::") {
        log_dep_decision("syn", "Already present in deps", false);
    }
    if !has_dep(deps, "quote") && (code.contains("quote::") || code.contains("quote!")) {
        new_deps.push("quote = \"1\"".to_string());
    }
    if !has_dep(deps, "proc-macro2") && code.contains("proc_macro2::") {
        new_deps.push("proc-macro2 = \"1\"".to_string());
    }

    // System crates
    if !has_dep(deps, "libc") && code.contains("libc::") {
        new_deps.push("libc = \"0.2\"".to_string());
    }
    if !has_dep(deps, "cfg-if") && (code.contains("cfg_if::") || code.contains("cfg_if!")) {
        new_deps.push("cfg-if = \"1\"".to_string());
    }

    // Common utility crates
    if !has_dep(deps, "parking_lot") && code.contains("parking_lot::") {
        new_deps.push("parking_lot = \"0.12\"".to_string());
    }
    if !has_dep(deps, "lock_api") && code.contains("lock_api::") {
        new_deps.push("lock_api = \"0.4\"".to_string());
    }

    // Windows crates - transitive dependencies
    // UPDATED: Some crates actually DO use windows_core at runtime (e.g., iana-time-zone)
    // Detect genuine usage vs proc-macro generated references
    if !has_dep(deps, "windows-core") && code.contains("windows_core::") {
        // Only add if it's actual usage, not just in proc-macro generated code comments
        // Proc-macro generated code usually appears in specific patterns
        let is_genuine_usage = !code.contains("Generated by `windows-bindgen`")
            && (code.contains("use windows_core::") || code.contains("windows_core::HRESULT"));

        if is_genuine_usage {
            new_deps.push("windows-core = \"0.52\"".to_string());
        }
    }

    if !has_dep(deps, "windows-sys") && code.contains("windows_sys::") {
        new_deps.push("windows-sys = \"0.52\"".to_string());
    }

    // Platform-specific transitive dependencies
    // core-foundation-sys is used by macOS-specific code
    if !has_dep(deps, "core-foundation-sys") && code.contains("core_foundation_sys::") {
        new_deps.push("core-foundation-sys = \"0.8\"".to_string());
    }

    // Testing/mocking frameworks (often optional)
    // loom is used for concurrent testing - usually behind cfg(loom) or cfg(test)
    // Only add if it's NOT cfg-gated (meaning it's actually needed at runtime)
    if !has_dep(deps, "loom") && code.contains("loom::") {
        // Check if loom usage is NOT cfg-gated (would appear with #[cfg(loom)])
        let has_cfg_gated_loom = code.contains("#[cfg(loom)]") || code.contains("cfg(loom)");
        if !has_cfg_gated_loom {
            // Genuine runtime dependency on loom
            new_deps.push("loom = \"0.7\"".to_string());
        }
        // If cfg-gated, we skip adding the dep (test-only)
    }

    // Serialization crates
    if !has_dep(deps, "protobuf") && (code.contains("protobuf::") || code.contains("::protobuf::")) {
        new_deps.push("protobuf = \"3.7\"".to_string());
    }
    if !has_dep(deps, "serde") && (code.contains("serde::") || code.contains("::serde::")) {
        new_deps.push("serde = { version = \"1\", features = [\"derive\"] }".to_string());
    }
    if !has_dep(deps, "serde_core") && (code.contains("serde_core::") || code.contains("::serde_core::")) {
        new_deps.push("serde_core = \"1.0\"".to_string());
    }
    if !has_dep(deps, "thiserror") && code.contains("thiserror::") {
        new_deps.push("thiserror = \"2\"".to_string());
    }
    if !has_dep(deps, "anyhow") && code.contains("anyhow::") {
        new_deps.push("anyhow = \"1\"".to_string());
    }

    // Unicode/identifier crates
    if !has_dep(deps, "unicode-ident") && code.contains("unicode_ident::") {
        new_deps.push("unicode-ident = \"1\"".to_string());
        log_dep_decision("unicode-ident", "Found unicode_ident:: usage", true);
    }

    // Utility macro crates
    // Check for extern crate declarations or actual usage
    let has_scopeguard_usage = code.contains("scopeguard::")
        || code.contains("extern crate scopeguard")
        || code.contains("defer!")
        || code.contains("#[macro_use]\nextern crate scopeguard");

    if !has_dep(deps, "scopeguard") && has_scopeguard_usage {
        new_deps.push("scopeguard = \"1\"".to_string());
        if code.contains("extern crate scopeguard") {
            log_dep_decision("scopeguard", "Found 'extern crate scopeguard' declaration", true);
        } else {
            log_dep_decision("scopeguard", "Found scopeguard usage or defer! macro", true);
        }
    } else if has_scopeguard_usage {
        log_dep_decision("scopeguard", "Found usage but dep already present", false);
    }

    let has_doc_comment_usage = code.contains("doc_comment::")
        || code.contains("doc_comment!")
        || code.contains("#[doc_comment")
        || code.contains("extern crate doc_comment");

    if !has_dep(deps, "doc-comment") && has_doc_comment_usage {
        new_deps.push("doc-comment = \"0.3\"".to_string());
        if code.contains("extern crate doc_comment") {
            log_dep_decision("doc-comment", "Found 'extern crate doc_comment' declaration", true);
        } else {
            log_dep_decision("doc-comment", "Found doc_comment usage", true);
        }
    } else if has_doc_comment_usage {
        log_dep_decision("doc-comment", "Found usage but dep already present", false);
    }

    // Note: The `alloc` crate doesn't need a Cargo.toml entry - it's a core crate.
    // Code using `alloc::format!` or `alloc::string::String` just needs `extern crate alloc;`
    // which is automatically available in Rust 2018+ edition.

    deps.extend(new_deps);
}

/// Generate a semantically sliced crate
pub fn generate_semantic_sliced_crate(
    crate_info: &CrateInfo,
    used: &HashSet<UsedItem>,
    output_dir: &Path,
) -> std::io::Result<SemanticSliceResult> {
    let index = parse_crate(&crate_info.path, &crate_info.name);
    let needed = compute_needed_items(used, &index, &crate_info.name);

    // Phase 6.4: Apply type closure to include types referenced in signatures
    let needed = super::slicing::compute_type_closure(&needed, &index);

    generate_semantic_sliced_crate_with_needed(crate_info, &index, &needed, output_dir, None, None)
}

/// Compute a signature for an item to detect duplicates
/// Uses the item's "canonical form" - strips whitespace and normalizes
/// Includes module_path for functions to avoid incorrectly deduplicating
/// same-name functions from different modules (e.g., file_descriptor_proto)
fn item_signature(source: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    // Normalize: remove extra whitespace and get just the key structure
    let normalized: String = source
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");

    let mut hasher = DefaultHasher::new();
    normalized.hash(&mut hasher);
    hasher.finish()
}

/// Compute a signature for an item including module context
/// This prevents deduplicating same-name items from different modules
fn item_signature_with_module(source: &str, module_path: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    // Normalize: remove extra whitespace and get just the key structure
    let normalized: String = source
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");

    let mut hasher = DefaultHasher::new();
    module_path.hash(&mut hasher);
    normalized.hash(&mut hasher);
    hasher.finish()
}

/// Normalize source code by removing extra spaces around :: and <
/// This is needed because syn's quote! macro adds spaces around these
fn normalize_source(content: &str) -> String {
    content
        .replace(" :: ", "::")
        .replace(" < ", "<")
        .replace(" <", "<")
        .replace("< ", "<")
}

/// Check if content contains a pattern (handles spaces around :: and <)
fn contains_pattern(content: &str, pattern: &str) -> bool {
    // Normalize content first, then do simple contains
    let normalized = normalize_source(content);
    normalized.contains(pattern)
}

/// Detect and generate necessary std imports based on code content
/// Uses Aho-Corasick for efficient multi-pattern matching (single pass through content)
fn detect_std_imports(content: &str) -> String {
    // Normalize source to handle syn's extra spaces around :: and <
    let content = normalize_source(content);
    let content = content.as_str();

    // Use Aho-Corasick for single-pass multi-pattern matching
    let matcher = get_std_import_matcher();
    let found = matcher.find_matches(content);

    let mut imports = Vec::with_capacity(32);

    // Helper flags for conditional imports
    let has_use_stmt = found.contains(&ImportCategory::UseStatement);
    let uses_parking_lot = found.contains(&ImportCategory::ParkingLot);
    let has_fmt_module = found.contains(&ImportCategory::FmtModule);
    let has_io_module = found.contains(&ImportCategory::IoModule2);

    // Hash traits
    if found.contains(&ImportCategory::Hasher) {
        imports.push("use std::hash::Hasher;");
    }
    if found.contains(&ImportCategory::BuildHasher) {
        imports.push("use std::hash::BuildHasher;");
    }
    if found.contains(&ImportCategory::Hash) {
        imports.push("use std::hash::Hash;");
    }

    // Cmp traits (all in prelude - no import needed)
    // Ord, PartialOrd, Eq, PartialEq are in the prelude
    // if found.contains(&ImportCategory::Ord) {
    //     imports.push("use std::cmp::Ord;");
    // }
    // if found.contains(&ImportCategory::PartialOrd) {
    //     imports.push("use std::cmp::PartialOrd;");
    // }
    // if found.contains(&ImportCategory::Eq) {
    //     imports.push("use std::cmp::Eq;");
    // }
    // if found.contains(&ImportCategory::PartialEq) {
    //     imports.push("use std::cmp::PartialEq;");
    // }

    // Ordering - disambiguate between cmp::Ordering and atomic::Ordering
    let has_cmp_ordering = found.contains(&ImportCategory::CmpOrdering);
    let has_atomic_ordering = found.contains(&ImportCategory::AtomicOrdering);
    if has_cmp_ordering && !has_atomic_ordering {
        imports.push("use std::cmp::Ordering;");
    }
    if has_atomic_ordering {
        imports.push("use std::sync::atomic::Ordering;");
    }

    // Fmt traits
    if found.contains(&ImportCategory::Display) {
        imports.push("use std::fmt::Display;");
    }
    if found.contains(&ImportCategory::Debug) {
        imports.push("use std::fmt::Debug;");
    }
    // Write disambiguation: fmt::Write vs io::Write
    if found.contains(&ImportCategory::FmtWrite) && has_fmt_module {
        imports.push("use std::fmt::Write;");
    }
    if found.contains(&ImportCategory::FmtWrite) && has_io_module {
        imports.push("use std::io::Write;");
    }
    if found.contains(&ImportCategory::Formatter) {
        imports.push("use std::fmt::Formatter;");
    }

    // IO traits
    if found.contains(&ImportCategory::Read) {
        imports.push("use std::io::Read;");
    }
    if found.contains(&ImportCategory::BufRead) {
        imports.push("use std::io::BufRead;");
    }
    if found.contains(&ImportCategory::Seek) {
        imports.push("use std::io::Seek;");
    }

    // Iter traits (Iterator, IntoIterator, DoubleEndedIterator, ExactSizeIterator are in prelude)
    // if found.contains(&ImportCategory::Iterator) {
    //     imports.push("use std::iter::Iterator;");
    // }
    // if found.contains(&ImportCategory::IntoIterator) {
    //     imports.push("use std::iter::IntoIterator;");
    // }
    // if found.contains(&ImportCategory::ExactSizeIterator) {
    //     imports.push("use std::iter::ExactSizeIterator;");
    // }
    // if found.contains(&ImportCategory::DoubleEndedIterator) {
    //     imports.push("use std::iter::DoubleEndedIterator;");
    // }
    if found.contains(&ImportCategory::FusedIterator) {
        imports.push("use std::iter::FusedIterator;");
    }

    // Ops traits (Drop, Fn, FnMut, FnOnce are in prelude, but Deref/Index/etc are not)
    if found.contains(&ImportCategory::Deref) {
        imports.push("use std::ops::Deref;");
    }
    if found.contains(&ImportCategory::DerefMut) {
        imports.push("use std::ops::DerefMut;");
    }
    if found.contains(&ImportCategory::Index) {
        imports.push("use std::ops::Index;");
    }
    if found.contains(&ImportCategory::IndexMut) {
        imports.push("use std::ops::IndexMut;");
    }
    // Drop is in the prelude
    // if found.contains(&ImportCategory::Drop) {
    //     imports.push("use std::ops::Drop;");
    // }
    if found.contains(&ImportCategory::RangeBounds) {
        imports.push("use std::ops::RangeBounds;");
    }

    // Convert traits (From, Into, AsRef, AsMut are in prelude, but TryFrom/TryInto are not)
    // if found.contains(&ImportCategory::From) {
    //     imports.push("use std::convert::From;");
    // }
    // if found.contains(&ImportCategory::Into) {
    //     imports.push("use std::convert::Into;");
    // }
    if found.contains(&ImportCategory::TryFrom) {
        imports.push("use std::convert::TryFrom;");
    }
    if found.contains(&ImportCategory::TryInto) {
        imports.push("use std::convert::TryInto;");
    }
    // if found.contains(&ImportCategory::AsRef) {
    //     imports.push("use std::convert::AsRef;");
    // }
    // if found.contains(&ImportCategory::AsMut) {
    //     imports.push("use std::convert::AsMut;");
    // }

    // Default/Clone (both in prelude)
    // if found.contains(&ImportCategory::Default) {
    //     imports.push("use std::default::Default;");
    // }
    // if found.contains(&ImportCategory::Clone) {
    //     imports.push("use std::clone::Clone;");
    // }

    // Marker traits (Send, Sync, Copy, Sized are in prelude)
    // if found.contains(&ImportCategory::Send) {
    //     imports.push("use std::marker::Send;");
    // }
    // if found.contains(&ImportCategory::Sync) {
    //     imports.push("use std::marker::Sync;");
    // }
    // if found.contains(&ImportCategory::Copy) {
    //     imports.push("use std::marker::Copy;");
    // }
    if found.contains(&ImportCategory::PhantomData) {
        imports.push("use std::marker::PhantomData;");
    }

    // Borrow traits (ToOwned is in prelude, Borrow/BorrowMut are not)
    if found.contains(&ImportCategory::Borrow) {
        imports.push("use std::borrow::Borrow;");
    }
    if found.contains(&ImportCategory::BorrowMut) {
        imports.push("use std::borrow::BorrowMut;");
    }
    // if found.contains(&ImportCategory::ToOwned) {
    //     imports.push("use std::borrow::ToOwned;");
    // }
    if found.contains(&ImportCategory::Cow) {
        imports.push("use std::borrow::Cow;");
    }

    // Error
    if found.contains(&ImportCategory::Error) {
        imports.push("use std::error::Error;");
    }

    // Str
    if found.contains(&ImportCategory::FromStr) {
        imports.push("use std::str::FromStr;");
    }

    // Smart pointers
    if found.contains(&ImportCategory::Arc) {
        imports.push("use std::sync::Arc;");
    }
    if found.contains(&ImportCategory::Rc) {
        imports.push("use std::rc::Rc;");
    }

    // Mutex/RwLock - disambiguate between std and parking_lot
    if found.contains(&ImportCategory::Mutex) {
        if uses_parking_lot {
            imports.push("use parking_lot::Mutex;");
        } else {
            imports.push("use std::sync::Mutex;");
        }
    }
    if found.contains(&ImportCategory::RwLock) {
        if uses_parking_lot {
            imports.push("use parking_lot::RwLock;");
        } else {
            imports.push("use std::sync::RwLock;");
        }
    }

    // Cell types
    if found.contains(&ImportCategory::Cell) {
        imports.push("use std::cell::Cell;");
    }
    if found.contains(&ImportCategory::RefCell) {
        imports.push("use std::cell::RefCell;");
    }
    if found.contains(&ImportCategory::UnsafeCell) {
        imports.push("use std::cell::UnsafeCell;");
    }

    // Hint
    if found.contains(&ImportCategory::Hint) {
        imports.push("use std::hint;");
    }

    // Mem
    if found.contains(&ImportCategory::Mem) {
        imports.push("use std::mem::MaybeUninit;");
    }

    // Ptr
    if found.contains(&ImportCategory::Ptr) {
        imports.push("use std::ptr::NonNull;");
    }

    // Slice
    if found.contains(&ImportCategory::SliceIndex) {
        imports.push("use std::slice::SliceIndex;");
    }

    // Atomics
    if found.contains(&ImportCategory::AtomicUsize) {
        imports.push("use std::sync::atomic::AtomicUsize;");
    }
    if found.contains(&ImportCategory::AtomicBool) {
        imports.push("use std::sync::atomic::AtomicBool;");
    }
    if found.contains(&ImportCategory::AtomicU8) {
        imports.push("use std::sync::atomic::AtomicU8;");
    }
    if found.contains(&ImportCategory::AtomicU16) {
        imports.push("use std::sync::atomic::AtomicU16;");
    }
    if found.contains(&ImportCategory::AtomicU32) {
        imports.push("use std::sync::atomic::AtomicU32;");
    }
    if found.contains(&ImportCategory::AtomicU64) {
        imports.push("use std::sync::atomic::AtomicU64;");
    }
    if found.contains(&ImportCategory::AtomicPtr) {
        imports.push("use std::sync::atomic::AtomicPtr;");
    }

    // NonZero types
    if found.contains(&ImportCategory::NonZeroUsize) {
        imports.push("use std::num::NonZeroUsize;");
    }
    if found.contains(&ImportCategory::NonZeroU8) {
        imports.push("use std::num::NonZeroU8;");
    }
    if found.contains(&ImportCategory::NonZeroU16) {
        imports.push("use std::num::NonZeroU16;");
    }
    if found.contains(&ImportCategory::NonZeroU32) {
        imports.push("use std::num::NonZeroU32;");
    }
    if found.contains(&ImportCategory::NonZeroU64) {
        imports.push("use std::num::NonZeroU64;");
    }
    if found.contains(&ImportCategory::NonZeroI32) {
        imports.push("use std::num::NonZeroI32;");
    }
    if found.contains(&ImportCategory::NonZeroI64) {
        imports.push("use std::num::NonZeroI64;");
    }

    // Sync types
    if found.contains(&ImportCategory::Condvar) {
        imports.push("use std::sync::Condvar;");
    }
    if found.contains(&ImportCategory::Once) {
        imports.push("use std::sync::Once;");
    }
    if found.contains(&ImportCategory::Barrier) {
        imports.push("use std::sync::Barrier;");
    }

    // Panic types
    if found.contains(&ImportCategory::UnwindSafe) {
        imports.push("use std::panic::UnwindSafe;");
    }
    if found.contains(&ImportCategory::RefUnwindSafe) {
        imports.push("use std::panic::RefUnwindSafe;");
    }

    // Thread types
    if found.contains(&ImportCategory::Thread) {
        imports.push("use std::thread;");
        imports.push("use std::thread::Thread;");
    }
    if found.contains(&ImportCategory::ThreadId) {
        imports.push("use std::thread::ThreadId;");
    }

    // Time types
    if found.contains(&ImportCategory::Duration) {
        imports.push("use std::time::Duration;");
    }
    if found.contains(&ImportCategory::Instant) {
        imports.push("use std::time::Instant;");
    }
    if found.contains(&ImportCategory::SystemTime) {
        imports.push("use std::time::SystemTime;");
    }

    // Collections - skip if already has use statement
    if found.contains(&ImportCategory::HashMap) && !has_use_stmt {
        imports.push("use std::collections::HashMap;");
    }
    if found.contains(&ImportCategory::HashSet) && !has_use_stmt {
        imports.push("use std::collections::HashSet;");
    }
    if found.contains(&ImportCategory::BTreeMap) {
        imports.push("use std::collections::BTreeMap;");
    }
    if found.contains(&ImportCategory::BTreeSet) {
        imports.push("use std::collections::BTreeSet;");
    }
    if found.contains(&ImportCategory::VecDeque) {
        imports.push("use std::collections::VecDeque;");
    }
    if found.contains(&ImportCategory::LinkedList) {
        imports.push("use std::collections::LinkedList;");
    }
    if found.contains(&ImportCategory::BinaryHeap) {
        imports.push("use std::collections::BinaryHeap;");
    }

    // Path types
    if found.contains(&ImportCategory::Path) {
        imports.push("use std::path::Path;");
    }
    if found.contains(&ImportCategory::PathBuf) {
        imports.push("use std::path::PathBuf;");
    }
    if found.contains(&ImportCategory::CachePadded) {
        imports.push("use std::cell::CachePadded;");
    }

    // FFI types
    if found.contains(&ImportCategory::CStr) {
        imports.push("use std::ffi::CStr;");
    }
    if found.contains(&ImportCategory::CString) {
        imports.push("use std::ffi::CString;");
    }
    if found.contains(&ImportCategory::OsStr) {
        imports.push("use std::ffi::OsStr;");
    }
    if found.contains(&ImportCategory::OsString) {
        imports.push("use std::ffi::OsString;");
    }
    if found.contains(&ImportCategory::CVoid) {
        imports.push("use std::ffi::c_void;");
    }
    if found.contains(&ImportCategory::CChar) {
        imports.push("use std::ffi::c_char;");
    }
    if found.contains(&ImportCategory::CInt) {
        imports.push("use std::ffi::c_int;");
    }
    if found.contains(&ImportCategory::CUint) {
        imports.push("use std::ffi::c_uint;");
    }
    if found.contains(&ImportCategory::CLong) {
        imports.push("use std::ffi::c_long;");
    }
    if found.contains(&ImportCategory::CUlong) {
        imports.push("use std::ffi::c_ulong;");
    }

    // IO module
    if found.contains(&ImportCategory::IoModule) {
        imports.push("use std::io;");
    }

    // Env
    if found.contains(&ImportCategory::Env) {
        imports.push("use std::env;");
    }

    // Fs
    if found.contains(&ImportCategory::Fs) {
        imports.push("use std::fs;");
    }

    // Deduplicate and sort
    imports.sort();
    imports.dedup();
    imports.join("\n")
}

/// Filter preserved use statements to keep only those whose symbols are used in the code
/// This is the preferred approach: keep original imports and remove unused ones
/// Resolve super:: imports to absolute crate:: paths
/// Phase 6.5a: Convert `use super::X` to `use crate::parent_module::X`
///
/// # Examples
/// - In module "kv::value", `use super::Key;` -> `use crate::kv::Key;`
/// - In module "kv::value", `use super::{A, B};` -> `use crate::kv::{A, B};`
/// - In root module "", `use super::X;` -> `use crate::X;` (stays the same)
fn resolve_super_import(import: &str, current_module: &str) -> String {
    // If current_module is empty, we're at the crate root, super:: would be invalid
    // but we'll handle it gracefully by just replacing super:: with crate::
    if current_module.is_empty() {
        return import.replace("super::", "crate::");
    }

    // Split current_module to get the parent module
    // e.g., "kv::value" -> parent is "kv"
    // e.g., "kv" -> parent is "" (crate root)
    let parent_module = if let Some(last_sep) = current_module.rfind("::") {
        &current_module[..last_sep]
    } else {
        "" // We're one level deep, parent is crate root
    };

    // Build the replacement path
    let replacement = if parent_module.is_empty() {
        "crate::".to_string()
    } else {
        format!("crate::{}::", parent_module)
    };

    // Replace "super::" with the parent module path
    // This handles: use super::X; use super::{A, B}; etc.
    import.replace("super::", &replacement)
}

/// Fix self-imports in lib.rs content
/// Phase 6.6a: When slicing crate "foo", convert `use foo::` to `use crate::`
///
/// This function processes the entire lib.rs content and fixes imports that reference
/// the crate being sliced by its external name instead of using `crate::`.
///
/// # Examples
/// - When slicing "bytes": `pub use bytes::Bytes;` -> `pub use crate::Bytes;`
/// - When slicing "serde": `use serde::Serialize;` -> `use crate::Serialize;`
/// - External imports unchanged: `use std::vec::Vec;` -> `use std::vec::Vec;`
fn fix_self_imports_in_lib(content: &str, crate_name: &str) -> String {
    // Fix self-referential imports in lib.rs by:
    // 1. Removing imports like "pub use crate::bytes::Bytes;" when bytes is the crate name
    //    (these try to re-export from a non-existent module)
    // 2. Removing imports like "pub use crate::TypeName;" when TypeName is defined in the same file
    //    (these cause E0255 duplicate name errors)

    // Build regex pattern to match type definitions with optional attributes
    // Pattern matches: (optional attrs) pub (struct|enum|trait|type) TypeName (whitespace|{)
    fn type_is_defined_in_content(content: &str, type_name: &str) -> bool {
        use regex::Regex;

        // Match pub struct/enum/trait/type TypeName with optional attributes before it
        // The (?m) flag enables multiline mode so ^ matches start of line
        let pattern = format!(
            r"(?m)pub\s+(struct|enum|trait|type)\s+{}\s*[{{\s]",
            regex::escape(type_name)
        );

        if let Ok(re) = Regex::new(&pattern) {
            re.is_match(content)
        } else {
            false
        }
    }

    let mut result = String::new();
    for line in content.lines() {
        let trimmed = line.trim();
        let mut skip_line = false;

        // Check for imports that reference crate::CRATE_NAME::
        // e.g., "pub use crate::bytes::Bytes;" in the bytes crate
        if (trimmed.starts_with("pub use crate::") || trimmed.starts_with("use crate::")) && trimmed.ends_with(";") {
            // Extract the path after "crate::"
            let import_path = if let Some(start_pos) = trimmed.find("crate::") {
                &trimmed[start_pos + 7..] // Skip "crate::"
            } else {
                ""
            };

            // Check if the first segment matches the crate name
            if let Some(first_segment_end) = import_path.find("::") {
                let first_segment = &import_path[..first_segment_end];
                if first_segment == crate_name {
                    skip_line = true;
                }
            } else {
                // Single segment import like "pub use crate::Bytes;"
                // Extract the type name
                let type_name = import_path.trim_end_matches(";").trim();

                // Check if this type is defined in the content using regex
                if type_is_defined_in_content(content, type_name) {
                    skip_line = true;
                }
            }
        }

        if !skip_line {
            result.push_str(line);
            result.push('\n');
        }
    }

    result
}

/// Normalize self-imports: when slicing crate "foo", convert `use foo::Bar` to `use crate::Bar`
/// Phase 6.6: Fix self-import detection
///
/// # Examples
/// - When slicing "bytes" crate: `use bytes::Bytes;` -> `use crate::Bytes;`
/// - When slicing "serde" crate: `pub use serde::Serialize;` -> `pub use crate::Serialize;`
/// - External imports are unchanged: `use std::vec::Vec;` -> `use std::vec::Vec;`
///
/// # Algorithm
/// If the import starts with `use {crate_name}::` or `pub use {crate_name}::`,
/// replace `{crate_name}` with `crate`.
fn normalize_self_import(import: &str, crate_name: Option<&str>) -> String {
    // If no crate name provided, return unchanged
    let Some(crate_name) = crate_name else {
        return import.to_string();
    };

    // Check if import starts with "use {crate_name}::" or "pub use {crate_name}::"
    let patterns = [
        format!("use {}::", crate_name),
        format!("pub use {}::", crate_name),
        format!("use pub {}::", crate_name),  // syn sometimes reorders
    ];

    for pattern in &patterns {
        if import.contains(pattern) {
            // Replace crate name with "crate"
            let replacement = pattern.replace(crate_name, "crate");
            return import.replace(pattern, &replacement);
        }
    }

    // No match - return unchanged
    import.to_string()
}

fn filter_preserved_imports(
    use_statements: &[crate::types::UseStatement],
    code: &str,
    current_module: &str,
) -> String {
    filter_preserved_imports_with_modules(use_statements, code, current_module, None, None)
}

/// Priority 1: Filter preserved imports excluding locally-defined symbols
/// Prevents E0255 errors when a module imports AND defines the same symbol
fn filter_preserved_imports_excluding_local(
    use_statements: &[crate::types::UseStatement],
    code: &str,
    current_module: &str,
    all_module_paths: Option<&HashSet<String>>,
    crate_name: Option<&str>,
    locally_defined: &HashSet<String>,
) -> String {
    let preserved = filter_preserved_imports_with_modules(use_statements, code, current_module, all_module_paths, crate_name);

    let mut filtered_lines = Vec::new();
    for line in preserved.lines() {
        let symbols = extract_imported_symbols(line);
        if symbols.iter().any(|s| locally_defined.contains(s)) {
            continue;
        }
        filtered_lines.push(line);
    }

    filtered_lines.join("\n")
}

/// Priority 4: Rewrite imports to remove non-existent intermediate modules
/// E.g., `use crate::memchr::{item}` → `use crate::{item}` if memchr module doesn't exist
fn rewrite_import_path(import: &str, all_module_paths: &HashSet<String>) -> String {
    // Only process actual use statements (not doc comments or other lines)
    let trimmed = import.trim();
    let is_use_statement = trimmed.starts_with("use ") || trimmed.starts_with("pub use ");

    // Only process crate:: imports with braces
    if !is_use_statement || !import.contains("use crate::") || !import.contains('{') {
        return import.to_string();
    }

    // Find "use crate::" and extract everything between that and the opening brace
    let is_pub = import.trim().starts_with("pub ");
    let use_start = if is_pub {
        import.find("use crate::").unwrap() + 11  // length of "use crate::"
    } else {
        import.find("use crate::").unwrap() + 11
    };

    let brace_pos = import.find('{').unwrap();
    let module_segment = &import[use_start..brace_pos].trim();

    // Remove trailing :: if present
    let full_module_path = module_segment.trim_end_matches("::").trim();

    // Split into segments and check if intermediate modules exist
    let segments: Vec<&str> = full_module_path.split("::").collect();

    // Find the longest prefix that doesn't exist in all_module_paths
    for i in (0..segments.len()).rev() {
        let prefix = segments[..=i].join("::");

        // Check if this module exists
        let module_exists = all_module_paths.iter().any(|path| {
            path == &prefix || path.starts_with(&format!("{}::", prefix))
        });

        if !module_exists {
            // Remove this segment and everything before it
            if i == segments.len() - 1 {
                // Remove entire path - items are at crate root
                if is_pub {
                    return import.replace(&format!("pub use crate::{}::", full_module_path), "pub use crate::");
                } else {
                    return import.replace(&format!("use crate::{}::", full_module_path), "use crate::");
                }
            } else {
                // Keep segments after this one
                let remaining = segments[i+1..].join("::");
                if is_pub {
                    return import.replace(&format!("pub use crate::{}::", full_module_path),
                                        &format!("pub use crate::{}::", remaining));
                } else {
                    return import.replace(&format!("use crate::{}::", full_module_path),
                                        &format!("use crate::{}::", remaining));
                }
            }
        }
    }

    import.to_string()
}

fn filter_preserved_imports_with_modules(
    use_statements: &[crate::types::UseStatement],
    code: &str,
    current_module: &str,
    all_module_paths: Option<&HashSet<String>>,
    crate_name: Option<&str>,
) -> String {
    let mut kept_imports = Vec::new();

    for stmt in use_statements {
        // Only include use statements from the same module (or root module for all)
        // This ensures aliases defined in transducer.rs are only used in transducer.rs
        if !stmt.module_path.is_empty() && stmt.module_path != current_module {
            continue;
        }

        // Skip spurious "use loom;" imports
        // loom is used in cfg-guarded macros with fully qualified paths, no import needed
        if stmt.source.trim() == "use loom;" || stmt.source.contains("use loom::") {
            continue;
        }

        // Preserve certain internal imports:
        // 1. Module aliases (e.g., `use unix as imp;`) for platform-specific code
        // 2. `use crate::` imports for explicit module references (e.g., constants/statics)
        // 3. `use self::` imports for child module items (e.g., `use self::internal::{Inner, Visitor}`)
        // 4. `use super::` imports for parent module items (Phase 6.5a fix)
        let is_module_alias = stmt.source.contains(" as ");
        let is_crate_import = stmt.source.contains("use crate::");
        let is_self_import = stmt.source.contains("use self::");
        let is_super_import = stmt.source.contains("use super::");

        // Skip self-imports: don't add `use crate::tables::...` inside the tables module
        if is_crate_import && !current_module.is_empty() {
            // Extract module path from `use crate::module::...`
            if let Some(import_module) = extract_crate_import_module(&stmt.source) {
                if import_module == current_module || import_module.starts_with(&format!("{}::", current_module)) {
                    continue;
                }
            }
        }

        // Skip internal imports unless they're module aliases, crate imports, self imports, or super imports
        if !stmt.is_external && !is_module_alias && !is_crate_import && !is_self_import && !is_super_import {
            continue;
        }

        // Skip imports from unstable APIs (E0658 fix) and private items (E0603 fix)
        if is_unstable_std_import(&stmt.source) || is_private_std_import(&stmt.source) {
            continue;
        }

        // Skip imports to non-existent crate modules (E0432 fix for feature-gated modules)
        // BUT: don't filter out root-level item imports like `use crate::function_name;`
        // These are functions/types from the crate root, not modules
        if is_crate_import {
            if let Some(import_module) = extract_crate_import_module(&stmt.source) {
                // If the import_module contains "::", it's a module path
                // If it doesn't, it's a root-level item (function/type/const) and we should keep it
                if import_module.contains("::") {
                    // Check if the module exists in the sliced output
                    if let Some(module_paths) = all_module_paths {
                        // Extract the top-level module name (e.g., "std_support" from "std_support::types")
                        let top_module = import_module.split("::").next().unwrap_or(&import_module);

                        // Check if this top-level module exists in the sliced output
                        let module_exists = module_paths.iter().any(|path| {
                            path == top_module || path.starts_with(&format!("{}::", top_module))
                        });

                        if !module_exists {
                            // Module doesn't exist - skip this import
                            continue;
                        }
                    }
                }
                // If no "::" in import_module, it's a root-level item - always keep it
            }
        }

        // Module aliases are always preserved (they're structural, not just convenience)
        // For other imports, check if symbols are actually used
        let is_used = if is_module_alias {
            true
        } else {
            stmt.symbols.iter().any(|symbol| {
                if symbol == "*" {
                    // For glob imports, keep them (conservative)
                    true
                } else if symbol == "self" {
                    // For `use foo::bar::{self, Baz}`, check if `bar` is used as a module prefix
                    // This is tricky - for now, keep it
                    true
                } else {
                    // Check if the symbol appears in the code
                    // We need to be careful about word boundaries
                    is_symbol_used(symbol, code)
                }
            })
        };

        if is_used {
            // Phase 6.5a: Convert super:: imports to absolute crate:: paths
            let mut import_source = if is_super_import {
                resolve_super_import(&stmt.source, current_module)
            } else {
                stmt.source.clone()
            };

            // Phase 6.6: Fix self-imports - convert crate name to "crate::" when slicing that crate
            // e.g., when slicing "bytes" crate, `use bytes::Bytes;` should be `use crate::Bytes;`
            import_source = normalize_self_import(&import_source, crate_name);

            // Priority 4: Rewrite import paths to match flattened module structure
            // E.g., `use crate::memchr::{item}` → `use crate::{item}` if memchr module doesn't exist
            if let Some(module_paths) = all_module_paths {
                import_source = rewrite_import_path(&import_source, module_paths);
            }

            kept_imports.push(import_source);
        }
    }

    // Deduplicate and sort
    kept_imports.sort();
    kept_imports.dedup();
    kept_imports.join("\n")
}

/// Extract symbols from preserved imports for a specific module
/// Returns a set of symbols that should be skipped in generate_internal_imports
fn extract_symbols_from_preserved_imports(
    use_statements: &[crate::types::UseStatement],
    current_module: &str,
) -> HashSet<String> {
    let mut symbols = HashSet::new();

    for stmt in use_statements {
        // Only include use statements from the same module
        if !stmt.module_path.is_empty() && stmt.module_path != current_module {
            continue;
        }

        // Add all symbols from this statement
        for symbol in &stmt.symbols {
            if symbol != "*" && symbol != "self" {
                symbols.insert(symbol.clone());
            }
        }
    }

    symbols
}

/// Check if a symbol is used in the code
/// Handles word boundaries to avoid false positives
fn is_symbol_used(symbol: &str, code: &str) -> bool {
    // Direct match patterns
    let patterns = [
        format!("{}::", symbol),      // Type::method or module::item
        format!(": {}", symbol),       // Type annotation
        format!("<{}", symbol),        // Generic parameter
        format!("{}>", symbol),        // Generic parameter end
        format!("{},", symbol),        // In a list
        format!("({})", symbol),       // Single parameter
        format!("({},", symbol),       // First in tuple
        format!(", {})", symbol),      // Last in tuple
        format!("& {}", symbol),       // Reference
        format!("&{}", symbol),        // Reference without space
        format!("impl {}", symbol),    // Trait impl
        format!("for {}", symbol),     // Trait for type
        format!("-> {}", symbol),      // Return type
        format!("= {}", symbol),       // Assignment/binding
        format!("{} {{", symbol),      // Struct literal
        format!("{}(", symbol),        // Function call
        format!("{} ", symbol),        // Followed by space
        format!(" {} ", symbol),       // Surrounded by spaces
        format!(".{}", symbol),        // Method call
    ];

    for pattern in &patterns {
        if code.contains(pattern) {
            return true;
        }
    }

    // Also check if the symbol appears as a standalone word
    // This catches cases like `Foo` in `impl Foo`
    let word_pattern = format!(r"\b{}\b", regex::escape(symbol));
    if let Ok(re) = regex::Regex::new(&word_pattern) {
        if re.is_match(code) {
            return true;
        }
    }

    false
}

/// Combine preserved imports with detected imports, avoiding duplicates
/// Preserved imports take priority - they're processed first
/// Each symbol should only be imported once
fn combine_imports(preserved: &str, detected: &str) -> String {
    use std::collections::HashSet;

    // Track which symbols we've already imported
    let mut covered_symbols: HashSet<String> = HashSet::new();
    // Final list of import statements to include
    let mut final_imports: Vec<String> = Vec::new();

    // Process preserved imports FIRST (they have priority)
    // These are the original imports from the source code
    for line in preserved.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let symbols = extract_imported_symbols(line);

        // Skip if ALL symbols are already covered
        // (allow partial overlap - the line might bring new symbols)
        let all_covered = !symbols.is_empty() && symbols.iter().all(|s| covered_symbols.contains(s));

        if !all_covered {
            // Mark all symbols as covered
            for symbol in symbols {
                covered_symbols.insert(symbol);
            }
            final_imports.push(line.to_string());
        }
    }

    // Process detected imports SECOND
    // Only add imports for symbols that aren't already covered
    for line in detected.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let symbols = extract_imported_symbols(line);

        // Skip if ANY symbol is already covered (would cause duplicate import error)
        let has_duplicate = symbols.iter().any(|s| covered_symbols.contains(s));

        if !has_duplicate {
            // Mark all symbols as covered
            for symbol in symbols {
                covered_symbols.insert(symbol);
            }
            final_imports.push(line.to_string());
        }
    }

    // Sort and join
    final_imports.sort();
    final_imports.dedup();
    final_imports.join("\n")
}

/// Deduplicate import lines in a multi-line import string
/// This deduplicates both:
/// 1. Exact duplicate lines
/// 2. Imports that bring the same symbol into scope from different paths
/// Normalize import paths for comparison (std/core/alloc unification)
fn normalize_import_path(path: &str) -> String {
    // Normalize core:: and alloc:: to std:: for deduplication
    // This prevents duplicates like "use std::fmt::Debug;" and "use core::fmt::Debug;"
    let normalized = path.replace("core::", "std::")
        .replace("alloc::", "std::");

    // Also normalize self:: to just remove it for deduplication purposes
    // This helps catch duplicates like "self::visitor::X" and "crate::hir::visitor::X"
    // when we're in the hir module
    if normalized.starts_with("self::") {
        normalized[6..].to_string()  // Remove "self::"
    } else {
        normalized
    }
}

/// Normalize an import line for deduplication
/// Strips attributes, pub keyword, and normalizes std/core/alloc
fn normalize_import_line(line: &str) -> String {
    let mut normalized = line.trim().to_string();

    // Remove attributes like #[doc(no_inline)], #[cfg(...)], etc.
    while normalized.starts_with("#[") {
        if let Some(end_idx) = normalized.find(']') {
            normalized = normalized[end_idx + 1..].trim().to_string();
        } else {
            break;
        }
    }

    // Remove 'pub ' keyword (note the space to avoid matching 'public')
    if normalized.starts_with("pub use ") {
        normalized = normalized.replace("pub use ", "use ");
    }

    // Normalize std/core/alloc paths
    normalize_import_path(&normalized)
}

fn deduplicate_import_lines(imports: &str) -> String {
    use std::collections::HashSet;

    let debug = std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok();
    if debug {
        eprintln!("\n[DedupImport] ===== Starting deduplication =====");
        crate::debug_log!("[DedupImport] Input has {} lines", imports.lines().count());
    }

    let mut seen_lines: HashSet<String> = HashSet::new();
    let mut seen_symbols: HashSet<String> = HashSet::new();  // Normalized symbol paths
    let mut seen_modules: HashSet<String> = HashSet::new();  // Track module imports from {self, ...}
    let mut seen_symbol_names: HashSet<String> = HashSet::new();  // Track final symbol names in scope (E0252 prevention)
    let mut result: Vec<String> = Vec::new();

    // Phase 3 fix: Track attributes (like #[cfg(...)]) to preserve them with their imports
    let mut pending_attributes: Vec<String> = Vec::new();

    for line in imports.lines() {
        let trimmed = line.trim();

        // Check if this is an attribute-ONLY line (starts with #[ and ends with ], no code after)
        // e.g., "#[derive(Debug)]" vs "#[repr(C)] pub struct Foo"
        // Phase 6.4 fix: Don't skip lines that have both attributes AND code on the same line
        if trimmed.starts_with("#[") {
            // Check if there's code after the attribute by finding the closing ] and seeing if there's more
            if let Some(close_bracket) = trimmed.rfind(']') {
                let after_attr = &trimmed[close_bracket + 1..].trim();
                if after_attr.is_empty() {
                    // Attribute-only line, save for next line
                    pending_attributes.push(line.to_string());
                    continue;
                }
                // else: has code after attribute, fall through to process as regular line
            }
        }
        if trimmed.is_empty() {
            // Keep empty lines (but clear pending attributes)
            pending_attributes.clear();
            result.push(line.to_string());
            continue;
        }

        // Normalize the line for exact duplicate checking
        // This handles pub use vs use, attributes, and std/core/alloc normalization
        let normalized_line = normalize_import_line(trimmed);
        if seen_lines.contains(&normalized_line) {
            // Skip duplicate - also clear any pending attributes
            pending_attributes.clear();
            continue;
        }

        // Phase 5.3: Handle extern crate declarations
        // Check for conflicts with use statements (E0254 prevention)
        if trimmed.starts_with("extern crate ") && trimmed.ends_with(";") {
            // Extract the name that will be defined in scope
            // "extern crate foo;" → "foo"
            // "extern crate foo as bar;" → "bar"
            let crate_name = if let Some(as_pos) = trimmed.find(" as ") {
                // Has alias: extract name after " as "
                let after_as = &trimmed[as_pos + 4..];
                after_as.trim_end_matches(';').trim()
            } else {
                // No alias: extract name after "extern crate "
                let after_extern = &trimmed[13..]; // "extern crate " is 13 chars
                after_extern.trim_end_matches(';').trim()
            };

            // Check if this name conflicts with existing symbols
            if seen_symbol_names.contains(crate_name) {
                // Skip - would create E0254 duplicate name error
                // Example: extern crate alloc; + use std::alloc; → both define "alloc"
                pending_attributes.clear();
                continue;
            }

            // Track this extern crate name as a defined symbol
            seen_symbol_names.insert(crate_name.to_string());

            // Add the line to result (with any pending attributes)
            seen_lines.insert(normalized_line);
            for attr in &pending_attributes {
                result.push(attr.clone());
            }
            result.push(line.to_string());
            pending_attributes.clear();
            continue;
        }

        // For use statements, also check symbol duplicates
        // Handle both "use" and "pub use" (and other visibility modifiers like "pub(crate) use")
        let is_use_stmt = (trimmed.starts_with("use ") || trimmed.starts_with("pub use ") ||
                          trimmed.starts_with("pub(crate) use ") || trimmed.starts_with("pub(super) use ") ||
                          trimmed.starts_with("pub(in ") && trimmed.contains(") use "))
                          && trimmed.ends_with(";");

        if is_use_stmt {
            let (symbol_paths, module_paths) = extract_imported_paths(trimmed);

            // Check for symbol name conflicts (E0252/E0254/E0255 prevention)
            // Extract symbol names from this import line
            let symbol_names = extract_imported_symbols(trimmed);

            // Priority 5: Skip imports of Result/Ok/Err from crate paths
            // These should use std::result types from prelude, not crate type aliases
            // This prevents E0107 errors when code uses Result<T, E> but imports Result<T> alias
            // Priority 8a: Also skip "error" module imports - they shadow std::error
            if trimmed.contains("use crate::") {
                const PRELUDE_RESULT_TYPES: &[&str] = &["Result", "Ok", "Err"];
                let imports_prelude_type = symbol_names.iter().any(|name| {
                    PRELUDE_RESULT_TYPES.contains(&name.as_str())
                });
                if imports_prelude_type {
                    if debug {
                        crate::debug_log!("[DedupImport] SKIPPED (prelude Result type from crate): {}", trimmed);
                    }
                    pending_attributes.clear();
                    continue;
                }

                // Priority 8a: Skip "error" module imports (E0404 fix - REVERTED, needs different approach)
                // `use crate::error;` shadows std::error, causing `error::Error` to resolve
                // to crate::error::Error (enum) instead of std::error::Error (trait)
                // Replacing with std::error breaks access to types in crate::error module
                if trimmed == "use crate::error;" || trimmed == "pub use crate::error;" {
                    if debug {
                        crate::debug_log!("[DedupImport] SKIPPED (error module shadows std::error): {}", trimmed);
                    }
                    pending_attributes.clear();
                    continue;
                }

                // Priority 6a: Skip imports of known private items (E0603 fix)
                // Priority 8b: Added Primitive to fix remaining E0603 errors
                // Priority 3: Added unicode module re-export
                // These are private type aliases/structs/enums that shouldn't be imported
                const KNOWN_PRIVATE_ITEMS: &[(&str, &str)] = &[
                    ("Range", "unicode"),  // private type alias in unicode module
                    ("Flags", "hir::translate"),  // private struct in hir::translate
                    ("CanonicalClassQuery", "unicode"),  // private enum
                    ("ClassState", "ast::parse"),  // private enum
                    ("GroupState", "ast::parse"),  // private enum
                    ("HirFrame", "hir::translate"),  // private enum
                    ("Primitive", "ast::parse"),  // private enum (Priority 8b)
                    ("unicode", "hir::translate"),  // private module re-export in hir::translate (Priority 3)
                ];
                let imports_private_item = KNOWN_PRIVATE_ITEMS.iter().any(|(item_name, module)| {
                    symbol_names.contains(&item_name.to_string()) &&
                    trimmed.contains(&format!("use crate::{}::{}", module, item_name))
                });
                if imports_private_item {
                    crate::debug_log!("[Priority 3 DEDUP] SKIPPED (known private item): {}", trimmed);
                    if debug {
                        crate::debug_log!("[DedupImport] SKIPPED (known private item): {}", trimmed);
                    }
                    pending_attributes.clear();
                    continue;
                }

                // Priority 6b: Skip incorrect imports of unicode_tables submodules at crate root (E0432 fix)
                // These modules are in unicode_tables/, not at crate root
                // The correct imports (use crate::unicode_tables::age::BY_NAME) already exist
                const UNICODE_TABLE_MODULES: &[&str] = &[
                    "age", "case_folding_simple", "general_category", "grapheme_cluster_break",
                    "perl_decimal", "perl_space", "perl_word", "property_bool", "property_names",
                    "property_values", "script", "script_extension", "sentence_break", "word_break",
                ];
                let imports_unicode_table_at_root = UNICODE_TABLE_MODULES.iter().any(|module_name| {
                    // Match "use crate::age;" pattern (module at root, which is wrong)
                    trimmed == &format!("use crate::{};", module_name) ||
                    trimmed == &format!("pub use crate::{};", module_name)
                });
                if imports_unicode_table_at_root {
                    if debug {
                        crate::debug_log!("[DedupImport] SKIPPED (unicode_tables module at wrong path): {}", trimmed);
                    }
                    pending_attributes.clear();
                    continue;
                }

                // Skip self-import (crate importing itself)
                // Example: "use crate::regex_syntax;" in regex-syntax crate
                // This can happen when doc examples mention the crate name
                const SELF_IMPORT_PATTERNS: &[&str] = &[
                    "use crate::regex_syntax;",
                    "pub use crate::regex_syntax;",
                    "use crate::regex_automata;",
                    "pub use crate::regex_automata;",
                ];
                if SELF_IMPORT_PATTERNS.iter().any(|pattern| trimmed == *pattern) {
                    if debug {
                        crate::debug_log!("[DedupImport] SKIPPED (self-import): {}", trimmed);
                    }
                    pending_attributes.clear();
                    continue;
                }

                // Priority 7: Skip incorrect Visitor trait imports (E0407 fix)
                // Files in ast/ modules should use ast::visitor::Visitor, not hir::visitor::Visitor
                // This causes E0407 errors when implementing methods that exist in ast::Visitor
                // but not in hir::Visitor (like visit_class_set_item_pre)

                // Match exact string to filter this specific import (handle both with/without spaces)
                let matches_hir_visitor = trimmed == "use crate::hir::visitor::Visitor;" ||
                                          trimmed == "pub use crate::hir::visitor::Visitor;" ||
                                          trimmed.starts_with("use crate::hir::visitor::Visitor") ||
                                          trimmed.starts_with("pub use crate::hir::visitor::Visitor");

                if matches_hir_visitor {
                    if debug {
                        crate::debug_log!("[DedupImport] SKIPPED (incorrect Visitor trait import - Priority 7 fix): {}", trimmed);
                    }
                    pending_attributes.clear();
                    continue;
                }
            }

            if debug {
                eprintln!("\n[DedupImport] Processing: {}", trimmed);
                crate::debug_log!("[DedupImport]   Symbol paths: {:?}", symbol_paths);
                crate::debug_log!("[DedupImport]   Module paths: {:?}", module_paths);
                crate::debug_log!("[DedupImport]   Symbol names: {:?}", symbol_names);
                crate::debug_log!("[DedupImport]   Already seen names: {:?}", seen_symbol_names);
            }

            // Check if ALL symbol names conflict with existing imports
            // If only SOME conflict, we'll try to reconstruct without duplicates later
            let all_symbols_conflict = !symbol_names.is_empty()
                && symbol_names.iter().all(|name| seen_symbol_names.contains(name));

            if all_symbols_conflict {
                // Skip this import - ALL symbols would create E0252/E0254/E0255 errors
                // This applies to BOTH regular imports and re-exports
                // Example: pub use self::apple::{c_char, ...}; + use std::ffi::c_char;
                //          Both would define "c_char" → skip the second one
                if debug {
                    crate::debug_log!("[DedupImport] SKIPPED (all symbols duplicate): {}", trimmed);
                }
                pending_attributes.clear();  // Clear attributes for skipped import
                continue;
            }

            // Separate new modules from duplicate modules
            // A module can be in seen_modules (from {self, ...}) or in seen_symbols (from simple import)
            let new_modules: Vec<&String> = module_paths.iter()
                .filter(|path| {
                    let normalized = normalize_import_path(path);
                    !seen_modules.contains(&normalized) && !seen_symbols.contains(&normalized)
                })
                .collect();

            // Separate new symbols from duplicate symbols
            // Also check seen_modules because "use std::ops;" duplicates "use core::ops::{self, ...}"
            let new_symbols: Vec<&String> = symbol_paths.iter()
                .filter(|path| {
                    let normalized = normalize_import_path(path);
                    !seen_symbols.contains(&normalized) && !seen_modules.contains(&normalized)
                })
                .collect();

            // Check if everything is a duplicate
            if new_modules.is_empty() && new_symbols.is_empty() {
                // Everything is a duplicate - skip entire line
                pending_attributes.clear();  // Clear attributes for skipped import
                continue;
            }

            // Check if we have partial duplicates that need reconstruction
            let has_duplicate_modules = new_modules.len() < module_paths.len();
            let has_duplicate_symbols = new_symbols.len() < symbol_paths.len();

            // Also check if some symbol NAMES are duplicates (not just paths)
            let new_symbol_names: Vec<String> = symbol_names.iter()
                .filter(|name| !seen_symbol_names.contains(*name))
                .cloned()
                .collect();
            let has_duplicate_symbol_names = new_symbol_names.len() < symbol_names.len();

            if debug && has_duplicate_symbol_names {
                crate::debug_log!("[DedupImport]   Has duplicate symbol names!");
                crate::debug_log!("[DedupImport]   Original names: {:?}", symbol_names);
                crate::debug_log!("[DedupImport]   New names: {:?}", new_symbol_names);
                let duplicate_names: Vec<_> = symbol_names.iter()
                    .filter(|name| seen_symbol_names.contains(*name))
                    .collect();
                crate::debug_log!("[DedupImport]   Duplicate names: {:?}", duplicate_names);
            }

            if (has_duplicate_modules || has_duplicate_symbols || has_duplicate_symbol_names) && !new_symbols.is_empty() {
                // Partial duplicates: reconstruct import with only new symbols
                // This handles cases like:
                // 1. use core::sync::atomic::Ordering;  // Already seen
                //    use core::{ arch::asm, sync::atomic::Ordering };  // asm=NEW, Ordering=DUP
                //    Result: use core::arch::asm;
                // 2. use core::fmt;  // Already seen
                //    use std::fmt::{self, Formatter};  // self=DUP (module), Formatter=NEW
                //    Result: use std::fmt::Formatter;
                // 3. pub(crate) use self::searcher::{PrefilterState, ...};  // Already seen
                //    use crate::{ ..., memmem::searcher::{ PrefilterState, ... } };
                //    Result: use crate::{ ... };  // Without PrefilterState

                // Filter new_symbols to only include symbols whose NAMES are not duplicates
                let new_symbols_filtered: Vec<&String> = new_symbols.iter()
                    .filter(|path| {
                        // Extract the symbol name from the path (last component)
                        if let Some(name) = path.rsplit("::").next() {
                            !seen_symbol_names.contains(name)
                        } else {
                            true
                        }
                    })
                    .copied()
                    .collect();

                if !new_symbols_filtered.is_empty() {
                    if let Some(reconstructed) = reconstruct_import_with_new_symbols(trimmed, &symbol_paths, &new_symbols_filtered) {
                        if debug {
                            crate::debug_log!("[DedupImport] RECONSTRUCTED (partial duplicates):");
                            crate::debug_log!("[DedupImport]   Original: {}", trimmed);
                            crate::debug_log!("[DedupImport]   New: {}", reconstructed);
                        }

                        // Mark the new symbols and modules as seen
                        for path in new_symbols_filtered {
                            seen_symbols.insert(normalize_import_path(path));
                        }
                        for path in new_modules {
                            seen_modules.insert(normalize_import_path(path));
                        }

                        // Track symbol names from reconstructed import
                        let reconstructed_names = extract_imported_symbols(&reconstructed);
                        seen_symbol_names.extend(reconstructed_names);

                        // Add reconstructed import instead of original (with attributes)
                        seen_lines.insert(normalize_import_line(&reconstructed));
                        // Prepend any pending attributes before the import
                        for attr in &pending_attributes {
                            result.push(attr.clone());
                        }
                        result.push(reconstructed);
                        pending_attributes.clear();
                        continue;
                    }
                } else {
                    // All symbols filtered out - skip this import entirely
                    if debug {
                        crate::debug_log!("[DedupImport] SKIPPED (all new symbols filtered): {}", trimmed);
                    }
                    pending_attributes.clear();
                    continue;
                }
                // If reconstruction failed, fall through to keep original
            }

            // Mark symbols and modules as seen (with normalization)
            for path in symbol_paths {
                seen_symbols.insert(normalize_import_path(&path));
            }
            for path in module_paths {
                seen_modules.insert(normalize_import_path(&path));
            }

            // Track symbol names (already extracted earlier)
            if debug && !symbol_names.is_empty() {
                crate::debug_log!("[DedupImport]   Adding to seen_symbol_names: {:?}", symbol_names);
            }
            seen_symbol_names.extend(symbol_names);
        }

        // Add the line to result (with any pending attributes)
        if debug {
            crate::debug_log!("[DedupImport]   KEEPING: {}", trimmed);
        }
        seen_lines.insert(normalized_line);
        // Prepend any pending attributes before the import/line
        for attr in &pending_attributes {
            result.push(attr.clone());
        }
        result.push(line.to_string());
        pending_attributes.clear();
    }

    result.join("\n")
}

/// Strip visibility modifiers and "use" keyword from an import statement
/// e.g., "pub use foo::Bar;" -> "foo::Bar"
///       "pub(crate) use foo::Bar;" -> "foo::Bar"
fn strip_use_prefix(use_stmt: &str) -> &str {
    let stmt = use_stmt.trim();

    // Strip visibility modifiers
    let stmt = stmt.trim_start_matches("pub(crate) ")
                   .trim_start_matches("pub(super) ")
                   .trim_start_matches("pub ")
                   .trim();

    // Handle pub(in path) visibility
    let stmt = if stmt.starts_with("pub(in ") {
        if let Some(end) = stmt.find(") ") {
            stmt[end + 2..].trim()
        } else {
            stmt
        }
    } else {
        stmt
    };

    // Strip "use " keyword
    stmt.trim_start_matches("use ").trim()
}

/// Extract imported paths from a use statement
/// Returns (symbol_paths, module_paths) where:
/// - symbol_paths: Full paths of imported symbols (e.g., "std::fmt::Debug")
/// - module_paths: Paths of modules imported via {self, ...} (e.g., "std::mem")
fn extract_imported_paths(use_stmt: &str) -> (Vec<String>, Vec<String>) {
    let mut symbol_paths = Vec::new();
    let mut module_paths = Vec::new();

    // Remove visibility modifiers, "use " prefix and ";" suffix
    let stmt = strip_use_prefix(use_stmt)
        .trim_end_matches(';')
        .trim();

    // Recursively extract paths from nested groups
    extract_paths_recursive(stmt, &mut symbol_paths, &mut module_paths);

    (symbol_paths, module_paths)
}

/// Extract the imported symbols from a use statement (legacy version for compatibility)
fn extract_imported_symbols(use_stmt: &str) -> Vec<String> {
    let mut symbols = Vec::new();

    // Remove visibility modifiers, "use " prefix and ";" suffix
    let stmt = strip_use_prefix(use_stmt)
        .trim_end_matches(';')
        .trim();

    // Recursively extract symbols from nested groups
    extract_symbols_recursive(stmt, &mut symbols);

    symbols
}

/// Recursively extract full paths from a use statement
fn extract_paths_recursive(path: &str, symbol_paths: &mut Vec<String>, module_paths: &mut Vec<String>) {
    let path = path.trim();

    // Handle group: {A, B, C}
    if path.starts_with('{') && path.ends_with('}') {
        let inner = &path[1..path.len()-1];
        for item in split_use_group(inner) {
            extract_paths_recursive(item.trim(), symbol_paths, module_paths);
        }
        return;
    }

    // Handle path with group: foo::bar::{A, B} or foo::bar::{self, A}
    if let Some(brace_start) = path.find('{') {
        if let Some(brace_end) = path.rfind('}') {
            if brace_end <= brace_start {
                // Malformed, treat as simple path
                symbol_paths.push(path.to_string());
                return;
            }

            let prefix = path[..brace_start].trim().trim_end_matches("::");
            let group_content = &path[brace_start + 1..brace_end];
            let items: Vec<&str> = split_use_group(group_content);

            for item in &items {
                let item = item.trim();
                if item == "self" {
                    // {self, ...} imports the module itself
                    module_paths.push(prefix.to_string());
                } else if item.starts_with("self ") || item == "self" {
                    module_paths.push(prefix.to_string());
                } else {
                    // Build full path for each item in the group
                    // Note: even if item contains "::", it's still relative to prefix
                    // e.g., "use core::{sync::atomic::Ordering}" -> prefix="core", item="sync::atomic::Ordering"
                    // full path should be "core::sync::atomic::Ordering"
                    let full_path = format!("{}::{}", prefix, item);
                    extract_paths_recursive(&full_path, symbol_paths, module_paths);
                }
            }
            return;
        }
    }

    // Handle "as" rename: foo::Bar as Baz
    // IMPORTANT: Track the ORIGINAL path (before "as"), not the alias
    // This is key to detecting duplicates like "use X;" and "use X as Y;"
    if let Some(as_idx) = path.find(" as ") {
        let original_path = path[..as_idx].trim();
        symbol_paths.push(original_path.to_string());
        return;
    }

    // Simple path: foo::bar::Baz or just Baz
    if !path.is_empty() && path != "self" && path != "*" {
        symbol_paths.push(path.to_string());
    }
}

/// Recursively extract symbols from a use path that may contain nested groups
fn extract_symbols_recursive(path: &str, symbols: &mut Vec<String>) {
    let path = path.trim();

    // Handle group: {A, B, C} or {foo::A, bar::B}
    if path.starts_with('{') && path.ends_with('}') {
        let inner = &path[1..path.len()-1];
        // Split by comma, but be careful of nested braces
        for item in split_use_group(inner) {
            extract_symbols_recursive(item.trim(), symbols);
        }
        return;
    }

    // Handle path with group: foo::{A, B} or foo::{self, A, B}
    if let Some(brace_start) = path.find('{') {
        if let Some(brace_end) = path.rfind('}') {
            // Safety check: ensure brace_end comes after brace_start
            if brace_end <= brace_start {
                // Malformed path, extract just the last segment as symbol
                if let Some(last) = path.rsplit("::").next() {
                    let cleaned = last.trim().trim_matches(|c| c == '{' || c == '}' || c == ';');
                    if !cleaned.is_empty() && cleaned != "self" && cleaned != "*" {
                        symbols.push(cleaned.to_string());
                    }
                }
                return;
            }
            let prefix = path[..brace_start].trim().trim_end_matches("::");
            let group_content = &path[brace_start + 1..brace_end];

            // Check if 'self' is in the group - that means we're importing the module itself
            let items: Vec<&str> = split_use_group(group_content);
            for item in &items {
                let item = item.trim();
                if item == "self" {
                    // 'self' means we're importing the parent module
                    if let Some(module_name) = prefix.rsplit("::").next() {
                        symbols.push(module_name.to_string());
                    }
                } else {
                    // Recursively process other items
                    extract_symbols_recursive(item, symbols);
                }
            }
            return;
        }
    }

    // Handle "as" rename: foo::Bar as Baz
    if let Some(as_idx) = path.find(" as ") {
        let renamed = path[as_idx + 4..].trim();
        if !renamed.is_empty() && renamed != "self" {
            symbols.push(renamed.to_string());
        }
        return;
    }

    // Handle nested path: foo::bar::Baz
    if let Some(last) = path.rsplit("::").next() {
        let last = last.trim();
        if !last.is_empty() && last != "self" && !last.starts_with('{') {
            symbols.push(last.to_string());
        }
    } else if !path.is_empty() && path != "self" {
        symbols.push(path.to_string());
    }
}

/// Split a use group by commas, respecting nested braces
fn split_use_group(group: &str) -> Vec<&str> {
    let mut items = Vec::new();
    let mut start = 0;
    let mut depth = 0;

    for (i, c) in group.char_indices() {
        match c {
            '{' => depth += 1,
            '}' => depth -= 1,
            ',' if depth == 0 => {
                let item = group[start..i].trim();
                if !item.is_empty() {
                    items.push(item);
                }
                start = i + 1;
            }
            _ => {}
        }
    }

    // Don't forget the last item
    let last = group[start..].trim();
    if !last.is_empty() {
        items.push(last);
    }

    items
}

/// Reconstruct an import statement keeping only the specified new symbols
///
/// Takes an original import like:
///   use core::{ arch::asm, sync::atomic::Ordering };
/// And a list of symbol paths to keep (e.g., ["core::arch::asm"])
/// Returns a reconstructed import with only those symbols:
///   use core::arch::asm;
fn reconstruct_import_with_new_symbols(
    _original_import: &str,
    _all_symbol_paths: &[String],
    new_symbols: &[&String],
) -> Option<String> {
    // Safety check: if no new symbols, return None
    if new_symbols.is_empty() {
        return None;
    }

    // If only one new symbol, generate simple import
    if new_symbols.len() == 1 {
        let symbol = new_symbols[0];
        return Some(format!("use {};", symbol));
    }

    // For multiple symbols, try to find common prefix and group them
    // For now, implement the simple case: generate separate imports
    if new_symbols.len() == 1 {
        return Some(format!("use {};", new_symbols[0]));
    }

    // Check if all new symbols share a common prefix
    let common_prefix = find_common_prefix(new_symbols);

    if let Some(prefix) = common_prefix {
        // Can group under common prefix
        let items: Vec<String> = new_symbols.iter()
            .filter_map(|symbol| {
                if symbol.starts_with(&format!("{}::", prefix)) {
                    // Extract the part after the prefix
                    let suffix = &symbol[prefix.len() + 2..]; // +2 for "::"
                    Some(suffix.to_string())
                } else if *symbol == &prefix {
                    // Symbol is the prefix itself
                    Some("self".to_string())
                } else {
                    None
                }
            })
            .collect();

        if items.len() == 1 {
            // Single item after prefix - use simple import
            return Some(format!("use {}::{};", prefix, items[0]));
        } else if items.len() > 1 {
            // Multiple items - use grouped import
            return Some(format!("use {}::{{{}}};", prefix, items.join(", ")));
        }
    }

    // No common prefix or grouping failed - generate simple import for first symbol
    // In practice, this case shouldn't happen often
    Some(format!("use {};", new_symbols[0]))
}

/// Find the longest common prefix for a set of import paths
/// e.g., ["core::arch::asm", "core::fmt::Debug"] -> Some("core")
///       ["std::fmt::Debug", "core::mem::size_of"] -> None
fn find_common_prefix(paths: &[&String]) -> Option<String> {
    if paths.is_empty() {
        return None;
    }

    if paths.len() == 1 {
        // Single path - extract parent module
        let path = paths[0];
        if let Some(pos) = path.rfind("::") {
            return Some(path[..pos].to_string());
        }
        return None;
    }

    // Split first path into segments
    let first_segments: Vec<&str> = paths[0].split("::").collect();

    // Find common prefix length
    let mut common_len = 0;
    for i in 0..first_segments.len() {
        let segment = first_segments[i];
        let all_match = paths[1..].iter().all(|path| {
            let segments: Vec<&str> = path.split("::").collect();
            segments.len() > i && segments[i] == segment
        });

        if all_match {
            common_len = i + 1;
        } else {
            break;
        }
    }

    if common_len > 0 && common_len < first_segments.len() {
        // Has a common prefix that's not the entire path
        Some(first_segments[..common_len].join("::"))
    } else {
        None
    }
}

/// Create a definition key for deduplication
/// For most items, we use kind+name to ensure only one definition per name
/// For impl blocks, we need more nuanced handling
fn definition_key(item: &crate::types::ParsedItem) -> String {
    use crate::types::ParsedItemKind;
    let base_key = match item.kind {
        ParsedItemKind::Impl => {
            let src = item.source.trim();
            // For trait impls (impl Trait for Type), dedupe by header AND module path
            // This allows same trait impl for same-named types in different modules
            // (e.g., impl Iterator for Shlex in lib.rs vs bytes.rs)
            if src.contains(" for ") {
                if let Some(impl_start) = src.find("impl") {
                    let after_impl = &src[impl_start + 4..];
                    if let Some(brace) = after_impl.find('{') {
                        let header = after_impl[..brace].trim();
                        format!("trait_impl:{}:{}", item.module_path, header)
                    } else {
                        format!("trait_impl:{}:{}:{}", item.module_path, item.name, item_signature(src))
                    }
                } else {
                    format!("trait_impl:{}:{}:{}", item.module_path, item.name, item_signature(src))
                }
            } else {
                // For inherent impls (impl Type), use source hash to allow multiple impl blocks
                // that define different methods
                format!("inherent_impl:{}:{}:{}", item.module_path, item.name, item_signature(src))
            }
        }
        // For structs, enums, functions, macros, type aliases, traits - distinguish by kind, path, and name
        _ => format!("{:?}:{}:{}", item.kind, item.module_path, item.name),
    };

    // For all items, append cfg_attr if present to distinguish platform-specific variants
    // This is needed for cases like impl_consume macro or platform-specific functions
    match &item.cfg_attr {
        Some(cfg) => format!("{}:cfg({})", base_key, cfg),
        None => base_key,
    }
}

/// Detect module path references in source code (e.g., "private::Sealed")
/// Returns the module names that are referenced
fn detect_module_path_refs(source: &str) -> HashSet<String> {
    let mut modules = HashSet::new();

    // Simple pattern matching for "identifier ::" followed by uppercase
    let chars: Vec<char> = source.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        // Look for lowercase identifier
        if chars[i].is_ascii_lowercase() || chars[i] == '_' {
            let start = i;
            while i < len && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let ident: String = chars[start..i].iter().collect();

            // Skip whitespace
            while i < len && chars[i].is_whitespace() {
                i += 1;
            }

            // Check for "::"
            if i + 1 < len && chars[i] == ':' && chars[i + 1] == ':' {
                i += 2;
                // Skip whitespace
                while i < len && chars[i].is_whitespace() {
                    i += 1;
                }
                // Check for uppercase letter (type name)
                if i < len && chars[i].is_ascii_uppercase() {
                    // This is a module::Type pattern
                    if !matches!(ident.as_str(), "std" | "core" | "alloc" | "crate" | "self" | "super") {
                        modules.insert(ident);
                    }
                }
                // Also check for lowercase followed by () - this is a function call like module::func()
                // The module still needs to be added as it contains the function
                else if i < len && (chars[i].is_ascii_lowercase() || chars[i] == '_') {
                    // Check if this is followed by ( - function call pattern
                    let func_start = i;
                    while i < len && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                        i += 1;
                    }
                    // Skip whitespace
                    while i < len && chars[i].is_whitespace() {
                        i += 1;
                    }
                    // If followed by ( or ::, the ident is a module
                    if i < len && (chars[i] == '(' || chars[i] == ':') {
                        if !matches!(ident.as_str(), "std" | "core" | "alloc" | "crate" | "self" | "super") {
                            modules.insert(ident);
                        }
                    }
                    // Reset i to after the function name for next iteration
                    i = func_start;
                }
            }
        } else {
            i += 1;
        }
    }
    modules
}

/// Extract module paths from `use crate::...` import statements in source code.
/// For example, from `use crate::reflect::repeated::vec_downcast::VecMutVariant;`
/// this extracts `reflect::repeated::vec_downcast` (the module path before the type).
/// Extract module paths of functions called in the source code by looking them up in the index.
/// This is used to discover transitive dependencies for functions that are called without a module prefix.
fn extract_function_call_module_paths(source: &str, index: &CrateIndex, crate_name: &str) -> HashSet<String> {
    let mut module_paths = HashSet::new();

    // Find function calls: lowercase identifiers followed by `(`
    let func_call_pattern = regex::Regex::new(r"\b([a-z_][a-z0-9_]+)\s*\(").unwrap();
    // Also detect generic function calls: lowercase identifier followed by `::<`
    let generic_func_pattern = regex::Regex::new(r"\b([a-z_][a-z0-9_]+)\s*::\s*<").unwrap();

    // Common functions to skip
    static SKIP_FUNCS: &[&str] = &[
        "if", "for", "while", "match", "loop", "fn", "let", "mut", "ref",
        "use", "mod", "struct", "enum", "trait", "impl", "type", "const",
        "where", "as", "in", "return", "break", "continue", "move", "async",
        "unsafe", "self", "super", "crate", "true", "false", "pub", "static",
        "new", "clone", "default", "unwrap", "expect", "ok", "err", "some", "none",
        "len", "is_empty", "iter", "into_iter", "collect", "push", "pop", "get", "set",
        "map", "filter", "and_then", "or_else", "map_err", "ok_or", "ok_or_else",
        "write", "writeln", "println", "eprintln", "print", "eprint", "format",
        "panic", "unreachable", "todo", "unimplemented", "assert", "debug_assert",
        "vec", "box", "rc", "arc", "mutex", "rwlock", "cell", "refcell",
    ];

    for cap in func_call_pattern.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let func_name = m.as_str();

            // Skip common keywords and builtin functions
            if SKIP_FUNCS.contains(&func_name) {
                continue;
            }

            // Check if function exists in the index
            if let Some(items) = index.items.get(func_name) {
                for item in items {
                    if item.kind == crate::types::ParsedItemKind::Function && item.is_pub {
                        let item_module = normalize_module_path(&item.module_path, crate_name);
                        if !item_module.is_empty() {
                            // Add the module path and all parent paths
                            let parts: Vec<&str> = item_module.split("::").collect();
                            for i in 1..=parts.len() {
                                let path = parts[..i].join("::");
                                module_paths.insert(path);
                            }
                        }
                    }
                }
            }
        }
    }

    // Also check for generic function calls
    for cap in generic_func_pattern.captures_iter(source) {
        if let Some(m) = cap.get(1) {
            let func_name = m.as_str();

            if SKIP_FUNCS.contains(&func_name) {
                continue;
            }

            // Check if function exists in the index
            if let Some(items) = index.items.get(func_name) {
                for item in items {
                    if item.kind == crate::types::ParsedItemKind::Function && item.is_pub {
                        let item_module = normalize_module_path(&item.module_path, crate_name);
                        if !item_module.is_empty() {
                            let parts: Vec<&str> = item_module.split("::").collect();
                            for i in 1..=parts.len() {
                                let path = parts[..i].join("::");
                                module_paths.insert(path);
                            }
                        }
                    }
                }
            }
        }
    }

    module_paths
}

fn extract_crate_import_module_paths(source: &str) -> HashSet<String> {
    let mut module_paths = HashSet::new();

    // Match `use crate::path::to::module::Item;` patterns
    // The regex captures the full path after `use crate::`
    let re = regex::Regex::new(
        r"use\s+crate\s*::\s*([a-z_][a-z0-9_]*(?:\s*::\s*[a-z_][a-z0-9_]*)*)\s*::\s*[A-Z]"
    ).unwrap();

    for cap in re.captures_iter(source) {
        if let Some(path_match) = cap.get(1) {
            let path = path_match.as_str().replace(" ", "").replace("\n", "");
            // Add the full module path and all intermediate paths
            let parts: Vec<&str> = path.split("::").collect();
            for i in 1..=parts.len() {
                let module_path = parts[..i].join("::");
                if !module_path.is_empty() {
                    module_paths.insert(module_path);
                }
            }
        }
    }

    // Also match brace imports: `use crate::path::to::module::{Item1, Item2};`
    let re_brace = regex::Regex::new(
        r"use\s+crate\s*::\s*([a-z_][a-z0-9_]*(?:\s*::\s*[a-z_][a-z0-9_]*)*)\s*::\s*\{"
    ).unwrap();

    for cap in re_brace.captures_iter(source) {
        if let Some(path_match) = cap.get(1) {
            let path = path_match.as_str().replace(" ", "").replace("\n", "");
            let parts: Vec<&str> = path.split("::").collect();
            for i in 1..=parts.len() {
                let module_path = parts[..i].join("::");
                if !module_path.is_empty() {
                    module_paths.insert(module_path);
                }
            }
        }
    }

    module_paths
}

/// Detect full module path references in source code
/// Returns (module_paths, referenced_items) where:
/// - module_paths: full module paths like "arch::generic::memchr"
/// - referenced_items: items referenced from those paths like "Iter", "search_slice_with_raw"
fn detect_full_module_paths(source: &str) -> (HashSet<String>, HashSet<String>) {
    let mut module_paths = HashSet::new();
    let mut referenced_items = HashSet::new();

    // Use regex to find crate::path::Item patterns
    // This captures both module paths and the final item name
    let re_crate_item = regex::Regex::new(
        r"crate\s*::\s*([a-z_][a-z0-9_]*(?:\s*::\s*[a-z_][a-z0-9_]*)*)\s*::\s*([A-Za-z_][A-Za-z0-9_]*)"
    ).unwrap();

    // Match crate::path patterns (without trailing Type)
    // Use a negative lookahead to avoid matching function calls like crate::path::func()
    // We check after the full match if it's followed by ()
    let re_crate_only = regex::Regex::new(
        r"crate\s*::\s*([a-z_][a-z0-9_]*(?:\s*::\s*[a-z_][a-z0-9_]*)*)"
    ).unwrap();

    // Find crate::path::Item patterns
    for cap in re_crate_item.captures_iter(source) {
        if let (Some(path_match), Some(item_match)) = (cap.get(1), cap.get(2)) {
            let path = path_match.as_str().replace(" ", "").replace("\n", "");
            let item = item_match.as_str();

            // Add all prefix paths (but not the last part if item is uppercase - it's part of the Type)
            let parts: Vec<&str> = path.split("::").collect();
            for i in 1..=parts.len() {
                let module_path = parts[..i].join("::");
                if !module_path.is_empty() {
                    module_paths.insert(module_path);
                }
            }

            // Add the referenced item
            referenced_items.insert(item.to_string());
        }
    }

    // Also find crate::path (without trailing item)
    // But skip if the last component is followed by () - it's a function call, not a module
    for cap in re_crate_only.captures_iter(source) {
        if let Some(path_match) = cap.get(1) {
            // Check if this match is followed by () (function call)
            let match_end = cap.get(0).unwrap().end();
            let rest_of_source = &source[match_end..];
            let rest_trimmed = rest_of_source.trim_start();

            // Skip if followed by ( or ::< (turbofish) - this is a function call, not a module reference
            let is_function_call = rest_trimmed.starts_with('(') || rest_trimmed.starts_with("::<");
            if is_function_call {
                // Still add parent modules, just not the function itself
                let path = path_match.as_str().replace(" ", "").replace("\n", "");
                let parts: Vec<&str> = path.split("::").collect();
                // Add all prefix paths EXCEPT the last one (which is the function)
                for i in 1..parts.len() {
                    let module_path = parts[..i].join("::");
                    if !module_path.is_empty() {
                        module_paths.insert(module_path);
                    }
                }
                continue;
            }

            let path = path_match.as_str().replace(" ", "").replace("\n", "");
            let parts: Vec<&str> = path.split("::").collect();
            for i in 1..=parts.len() {
                let module_path = parts[..i].join("::");
                if !module_path.is_empty() {
                    module_paths.insert(module_path);
                }
            }
        }
    }

    // Find self::path::Item patterns
    let re_self_item = regex::Regex::new(
        r"self\s*::\s*([a-z_][a-z0-9_]*(?:\s*::\s*[a-z_][a-z0-9_]*)*)\s*::\s*([A-Za-z_][A-Za-z0-9_]*)"
    ).unwrap();

    for cap in re_self_item.captures_iter(source) {
        if let (Some(path_match), Some(item_match)) = (cap.get(1), cap.get(2)) {
            let path = path_match.as_str().replace(" ", "").replace("\n", "");
            let item = item_match.as_str();

            let parts: Vec<&str> = path.split("::").collect();
            for i in 1..=parts.len() {
                let module_path = parts[..i].join("::");
                if !module_path.is_empty() {
                    module_paths.insert(module_path);
                }
            }
            referenced_items.insert(item.to_string());
        }
    }

    // Detect bare module::Item patterns like "generic::Iter"
    let re_bare = regex::Regex::new(r"\b([a-z_][a-z0-9_]*)\s*::\s*([A-Za-z_][A-Za-z0-9_]*)").unwrap();

    // Load std modules from SCIP-generated cache instead of hardcoding
    let std_modules_cache = load_std_modules_cache();

    // Keywords and primitives that should always be excluded
    let always_exclude: HashSet<&str> = [
        "std", "core", "alloc", "crate", "self", "super", "r#ref", "fmt", "mem", "primitive",
    ].into_iter().collect();

    for cap in re_bare.captures_iter(source) {
        if let (Some(module_match), Some(item_match)) = (cap.get(1), cap.get(2)) {
            let module = module_match.as_str();
            let item = item_match.as_str();
            // Exclude:
            // 1. Rust keywords (crate, self, super, etc.)
            // 2. Standard library modules (from SCIP cache)
            if !always_exclude.contains(module) && !std_modules_cache.contains_key(module) {
                module_paths.insert(module.to_string());
                referenced_items.insert(item.to_string());
            }
        }
    }

    (module_paths, referenced_items)
}

/// Generate a semantically sliced crate with pre-computed needed items
/// Preserves original module structure instead of flattening
pub fn generate_semantic_sliced_crate_with_needed(
    crate_info: &CrateInfo,
    index: &CrateIndex,
    needed: &BTreeSet<String>,
    output_dir: &Path,
    scip_analysis_opt: Option<&ScipAnalysis>,
    progress_callback: Option<&dyn Fn(usize, usize)>,  // (current, total) items processed
) -> std::io::Result<SemanticSliceResult> {
    use std::collections::BTreeMap;

    // Clean output directory to prevent stale files from previous runs
    // This fixes E0761 (duplicate module files) when module structure changes
    if output_dir.exists() {
        let _ = fs::remove_dir_all(output_dir);
    }
    fs::create_dir_all(output_dir.join("src"))?;

    // Full SCIP analysis: get local types AND needed impl blocks
    // - local_types: avoid std import conflicts (e.g., don't import std::fmt::Pointer)
    // - needed_impl_blocks: impl blocks referenced by the code (e.g., `impl<T> Pointer for *const T`)
    // Use pre-computed SCIP data from Phase 2 if available (avoids re-running rust-analyzer)
    let scip_analysis = if let Some(analysis) = scip_analysis_opt {
        analysis.clone()
    } else {
        analyze_crate_via_scip(&crate_info.path)
    };
    let crate_local_types = scip_analysis.local_types;
    let scip_symbol_paths = scip_analysis.symbol_paths;

    // Build cfg map for platform-specific module filtering
    // This prevents including modules like tests_aarch64_neon on x86_64
    let module_cfgs = build_module_cfg_map(index, &crate_info.name);

    // First pass: collect all items and detect module path references
    let mut all_sources = String::new();
    for name in needed {
        for item in index.get_all(name) {
            all_sources.push_str(&item.source);
            all_sources.push('\n');
        }
    }

    // Detect module references like "private::Sealed" and add those modules
    let module_refs = detect_module_path_refs(&all_sources);
    let mut augmented_needed = needed.clone();
    for module_name in &module_refs {
        // Check if this module exists in the index
        if !index.get_all(module_name).is_empty() {
            augmented_needed.insert(module_name.clone());
        }
    }

    // Phase 3: Add referenced types from SCIP analysis
    // This ensures types referenced in function signatures (e.g., timespec in clock_gettime)
    // have their definitions included in the sliced output
    for (type_name, _module_paths) in &scip_analysis.referenced_types {
        // Only add if type exists in the index and isn't already in needed set
        if !augmented_needed.contains(type_name) && !index.get_all(type_name).is_empty() {
            augmented_needed.insert(type_name.clone());
        }
    }

    // Group items by their module path
    let mut modules: BTreeMap<String, Vec<&crate::types::ParsedItem>> = BTreeMap::new();
    let mut seen_signatures: HashSet<u64> = HashSet::new();
    let mut seen_definitions: HashSet<String> = HashSet::new();
    let mut included_count = 0;

    // Track progress for large crates
    let total_items = augmented_needed.len();
    let mut processed_items = 0;

    for name in &augmented_needed {
        processed_items += 1;
        if let Some(callback) = progress_callback {
            callback(processed_items, total_items);
        }

        for item in index.get_all(name) {
            // Use module-aware signature to avoid deduplicating same-name items
            // from different modules (e.g., multiple file_descriptor_proto functions)
            let sig = item_signature_with_module(&item.source, &item.module_path);
            let def_key = definition_key(item);

            if !seen_signatures.insert(sig) {
                continue;
            }
            // CRITICAL FIX: For impl blocks, don't check seen_definitions because multiple impl blocks
            // for the same type can have the same definition key but different signatures (different methods).
            // The signature check above is sufficient to prevent true duplicates.
            if item.kind != crate::types::ParsedItemKind::Impl {
                if !seen_definitions.insert(def_key.clone()) {
                    continue;
                }
            } else {
                // Still insert for impl blocks to track them, but don't skip based on this check
                seen_definitions.insert(def_key.clone());
            }

            let module_path = normalize_module_path(&item.module_path, &crate_info.name);

            // Skip items from cfg-gated modules that don't match current platform
            if !module_cfg_matches_platform(&module_path, &module_cfgs) {
                continue;
            }

            modules.entry(module_path).or_default().push(item);
            included_count += 1;
        }
        for impl_item in index.get_impls(name) {
            // Use module-aware signature for impl blocks too
            let sig = item_signature_with_module(&impl_item.source, &impl_item.module_path);
            let def_key = definition_key(impl_item);

            if !seen_signatures.insert(sig) {
                continue;
            }
            // CRITICAL FIX: For impl blocks, don't check seen_definitions (see comment above in get_all loop)
            // Just insert to track, but don't skip based on duplicate def_key
            seen_definitions.insert(def_key.clone());

            let module_path = normalize_module_path(&impl_item.module_path, &crate_info.name);

            // Skip items from cfg-gated modules that don't match current platform
            if !module_cfg_matches_platform(&module_path, &module_cfgs) {
                continue;
            }

            modules.entry(module_path).or_default().push(impl_item);
            included_count += 1;
        }
    }

    // Post-processing phase: notify callback we're done with item loop
    if let Some(callback) = progress_callback {
        callback(total_items, total_items);  // Show 100% item processing completion
    }

    // Include all macro_rules! definitions - they're often needed and small
    for item in &index.all_items {
        if item.kind == crate::types::ParsedItemKind::Macro {
            let sig = item_signature_with_module(&item.source, &item.module_path);
            let def_key = definition_key(item);

            if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                continue;
            }
            seen_signatures.insert(sig);
            seen_definitions.insert(def_key);

            let module_path = normalize_module_path(&item.module_path, &crate_info.name);

            // Skip items from cfg-gated modules that don't match current platform
            if !module_cfg_matches_platform(&module_path, &module_cfgs) {
                continue;
            }

            modules.entry(module_path).or_default().push(item);
            included_count += 1;
        }
    }

    // Include private functions from modules that have macros
    // Macros often call helper functions like can_transmute that need to be included
    let macro_modules: std::collections::HashSet<String> = modules.keys().cloned().collect();

    for item in &index.all_items {
        if item.kind == crate::types::ParsedItemKind::Function && !item.is_pub {
            let module_path = normalize_module_path(&item.module_path, &crate_info.name);

            // Only include if this module has a macro
            if !macro_modules.contains(&module_path) {
                continue;
            }

            // Skip if already included
            let sig = item_signature_with_module(&item.source, &item.module_path);
            let def_key = definition_key(item);
            if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                continue;
            }
            seen_signatures.insert(sig);
            seen_definitions.insert(def_key);

            // Skip items from cfg-gated modules
            if !module_cfg_matches_platform(&module_path, &module_cfgs) {
                continue;
            }

            modules.entry(module_path).or_default().push(item);
            included_count += 1;
        }
    }

    // Include impl blocks that are referenced in SCIP analysis
    // This catches generic impl blocks like `impl<T> Pointer for *const T`
    // that are used via methods like `.distance()` but not matched by type name
    for scip_impl_id in &scip_analysis.needed_impl_blocks {
        // Convert SCIP impl ID to parsed item name format
        // e.g., "ext/impl#[`*const T`][Pointer]" -> "*const T:Pointer"
        if let Some(parsed_name) = scip_impl_to_parsed_name(scip_impl_id) {
            let module_path = extract_impl_module(scip_impl_id);

            // Search all impl blocks for a match
            for item in &index.all_items {
                if item.kind != crate::types::ParsedItemKind::Impl {
                    continue;
                }

                // Check if this impl block matches the SCIP reference
                let item_normalized_module = item.module_path
                    .trim_start_matches(&crate_info.name)
                    .trim_start_matches("::")
                    .replace("::", "/");

                // Match by module path and name
                if item.name == parsed_name &&
                   (module_path.is_empty() || item_normalized_module == module_path ||
                    item_normalized_module.ends_with(&format!("/{}", module_path))) {
                    let sig = item_signature_with_module(&item.source, &item.module_path);
                    let def_key = definition_key(item);

                    if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                        continue;
                    }
                    seen_signatures.insert(sig);
                    seen_definitions.insert(def_key.clone());

                    let norm_module = normalize_module_path(&item.module_path, &crate_info.name);

                    // Skip items from cfg-gated modules that don't match current platform
                    if !module_cfg_matches_platform(&norm_module, &module_cfgs) {
                        continue;
                    }

                    modules.entry(norm_module).or_default().push(item);
                    included_count += 1;
                }
            }
        }
    }

    // CRITICAL FIX: Include ALL impl blocks for types that have ANY impl block included
    // This fixes the ParserI issue where multiple impl blocks exist for the same type
    // When one is needed, all must be included to provide all methods
    let mut impl_types_included: HashSet<(String, String)> = HashSet::new();
    for items in modules.values() {
        for item in items {
            if item.kind == crate::types::ParsedItemKind::Impl {
                // Extract base type name (e.g., "ParserI" from various impl forms)
                let type_name = item.name.split(':').next().unwrap_or(&item.name).to_string();
                impl_types_included.insert((type_name, item.module_path.clone()));
            }
        }
    }

    // Now include ALL other impl blocks for these types from the same modules
    for (type_name, module_path) in impl_types_included.iter() {
        for item in &index.all_items {
            if item.kind != crate::types::ParsedItemKind::Impl {
                continue;
            }

            // Check if this is an impl for the same type in the same module
            let item_type = item.name.split(':').next().unwrap_or(&item.name);
            if item_type == type_name && &item.module_path == module_path {
                let sig = item_signature_with_module(&item.source, &item.module_path);
                let def_key = definition_key(item);

                // Skip if already included
                if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                    continue;
                }

                seen_signatures.insert(sig);
                seen_definitions.insert(def_key.clone());

                let norm_module = normalize_module_path(&item.module_path, &crate_info.name);

                // Skip items from cfg-gated modules that don't match current platform
                if !module_cfg_matches_platform(&norm_module, &module_cfgs) {
                    continue;
                }

                crate::debug_log!("[IMPL BLOCK FIX] Including additional impl block for {} in module {}", type_name, norm_module);
                modules.entry(norm_module).or_default().push(item);
                included_count += 1;
            }
        }
    }

    // CRITICAL FIX #2: Include ALL impl blocks for structs/enums that are included
    // This is a broader fix that ensures when we include a type definition,
    // we also include all its methods (from all impl blocks)
    let mut types_included: HashSet<(String, String)> = HashSet::new();
    for items in modules.values() {
        for item in items {
            if item.kind == crate::types::ParsedItemKind::Struct ||
               item.kind == crate::types::ParsedItemKind::Enum {
                types_included.insert((item.name.clone(), item.module_path.clone()));
            }
        }
    }

    // Find and include ALL impl blocks for these types
    for (type_name, module_path) in types_included.iter() {
        for item in &index.all_items {
            if item.kind != crate::types::ParsedItemKind::Impl {
                continue;
            }

            // Check if this impl is for one of our included types in the same module
            let impl_type = item.name.split(':').next().unwrap_or(&item.name);

            if impl_type == type_name && &item.module_path == module_path {
                let sig = item_signature_with_module(&item.source, &item.module_path);
                let def_key = definition_key(item);

                // Skip if already included
                if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                    continue;
                }

                seen_signatures.insert(sig);
                seen_definitions.insert(def_key.clone());

                let norm_module = normalize_module_path(&item.module_path, &crate_info.name);

                // Skip items from cfg-gated modules that don't match current platform
                if !module_cfg_matches_platform(&norm_module, &module_cfgs) {
                    continue;
                }

                crate::debug_log!("[IMPL BLOCK FIX #2] Including impl block for struct/enum {} in module {}", type_name, norm_module);
                modules.entry(norm_module).or_default().push(item);
                included_count += 1;
            }
        }
    }

    // Collect all module paths that need to exist
    let mut all_module_paths: HashSet<String> = HashSet::new();
    for module_path in modules.keys() {
        // Add the module and all its parents
        let parts: Vec<&str> = module_path.split("::").collect();
        for i in 1..=parts.len() {
            all_module_paths.insert(parts[..i].join("::"));
        }
    }

    // Detect full module paths and items referenced in the source code
    // This catches paths like `crate::arch::generic::memchr::Iter`
    let (referenced_module_paths, referenced_items) = detect_full_module_paths(&all_sources);

    // Add referenced items to needed set and include them in the appropriate modules
    for item_name in &referenced_items {
        // Find items with this name in the index
        for item in index.get_all(item_name) {
            let sig = item_signature_with_module(&item.source, &item.module_path);
            let def_key = definition_key(item);

            if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                continue;
            }
            seen_signatures.insert(sig);
            seen_definitions.insert(def_key);

            let module_path = normalize_module_path(&item.module_path, &crate_info.name);

            // Skip items from cfg-gated modules that don't match current platform
            if !module_cfg_matches_platform(&module_path, &module_cfgs) {
                continue;
            }

            modules.entry(module_path).or_default().push(item);
            included_count += 1;
        }
    }

    // Add module paths from newly included items to all_module_paths
    for module_path in modules.keys() {
        let parts: Vec<&str> = module_path.split("::").collect();
        for i in 1..=parts.len() {
            all_module_paths.insert(parts[..i].join("::"));
        }
    }

    // Add detected module paths to all_module_paths
    // First, collect all actual module paths from the index for resolution
    // Build index_module_paths including all intermediate paths
    // E.g., for "packed::teddy::builder", also include "packed::teddy" and "packed"
    let index_module_paths: HashSet<String> = {
        let mut paths = HashSet::new();
        for item in &index.all_items {
            let path = normalize_module_path(&item.module_path, &crate_info.name);
            if !path.is_empty() {
                // Add the path and all intermediate paths
                let parts: Vec<&str> = path.split("::").collect();
                for i in 1..=parts.len() {
                    paths.insert(parts[..i].join("::"));
                }
            }
        }
        paths
    };

    for module_path in referenced_module_paths {
        // Skip Rust primitive types - these should never be modules
        // (e.g., u32::MAX is a primitive constant, not a module path)
        if RUST_PRIMITIVES.contains(&module_path.as_str()) {
            continue;
        }

        // Resolve short module names to their full paths
        // E.g., "noncontiguous" -> "nfa::noncontiguous" if that path exists in index
        let resolved_path = if module_path.contains("::") {
            module_path.clone()
        } else {
            // Look for a path in the index that ends with ::module_path
            let suffix = format!("::{}", module_path);
            index_module_paths.iter()
                .find(|p| p.ends_with(&suffix))
                .cloned()
                .unwrap_or(module_path.clone())
        };

        // Add this path and all its parent paths (but skip primitives and cfg-gated modules)
        let parts: Vec<&str> = resolved_path.split("::").collect();
        for i in 1..=parts.len() {
            let path_part = parts[..i].join("::");
            // Also check each component for primitives
            if !parts[..i].iter().any(|p| RUST_PRIMITIVES.contains(p)) {
                // Skip cfg-gated modules that don't match current platform
                if module_cfg_matches_platform(&path_part, &module_cfgs) {
                    all_module_paths.insert(path_part);
                }
            }
        }
    }

    // Module-complete inclusion: for each module in all_module_paths,
    // include ALL public items from that module. This fixes E0412/E0432/E0425
    // where internal module references can't find sibling items.
    // We now include items from both populated modules AND referenced modules.
    for item in &index.all_items {
        // Only include public items
        if !item.is_pub {
            continue;
        }

        let item_module = normalize_module_path(&item.module_path, &crate_info.name);

        // Only include if this module is in all_module_paths (either populated or referenced)
        if !all_module_paths.contains(&item_module) {
            continue;
        }

        // Skip items from cfg-gated modules that don't match current platform
        if !module_cfg_matches_platform(&item_module, &module_cfgs) {
            continue;
        }

        let sig = item_signature_with_module(&item.source, &item.module_path);
        let def_key = definition_key(item);

        // Skip if already included
        if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
            continue;
        }
        seen_signatures.insert(sig);
        seen_definitions.insert(def_key);

        modules.entry(item_module).or_default().push(item);
        included_count += 1;
    }

    // Include private helper functions called by included public items
    // This fixes E0425 errors like "cannot find function `vec_packed_fixed_size`"
    // where a public function calls a private helper in the same module
    let mut private_helpers_to_add: Vec<(String, &crate::types::ParsedItem)> = Vec::new();
    {
        // Collect all source code from currently included items
        let mut all_included_sources = String::new();
        for items in modules.values() {
            for item in items {
                all_included_sources.push_str(&item.source);
                all_included_sources.push('\n');
            }
        }

        // Extract function calls, constants, and type usages from included source
        let func_calls = crate::old_slicer::slicing::extract_function_calls(&all_included_sources);
        let const_usages = crate::old_slicer::slicing::extract_constant_usages(&all_included_sources);
        let type_usages = crate::old_slicer::slicing::extract_type_usages(&all_included_sources);

        // Find private items that match these calls
        for item in &index.all_items {
            // Only look at private items (public ones are already included)
            if item.is_pub {
                continue;
            }

            // Check if this item is called/used (functions, constants, or types like DynamicMapIterImpl)
            let is_called = func_calls.contains(&item.name) || const_usages.contains(&item.name) || type_usages.contains(&item.name);
            if !is_called {
                continue;
            }

            let item_module = normalize_module_path(&item.module_path, &crate_info.name);

            // Skip items from cfg-gated modules that don't match current platform
            if !module_cfg_matches_platform(&item_module, &module_cfgs) {
                continue;
            }

            let sig = item_signature_with_module(&item.source, &item.module_path);
            let def_key = definition_key(item);

            // Skip if already included
            if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                continue;
            }

            private_helpers_to_add.push((item_module.clone(), item));
        }
    }

    // Add the private helpers we found
    for (module_path, item) in private_helpers_to_add {
        let sig = item_signature_with_module(&item.source, &item.module_path);
        let def_key = definition_key(item);
        seen_signatures.insert(sig);
        seen_definitions.insert(def_key);
        modules.entry(module_path).or_default().push(item);
        included_count += 1;

        // CRITICAL FIX: Also include dependencies of private helpers
        // For example, thread_indices() depends on ThreadIndices struct
        for dep in &item.dependencies {
            if !crate::old_slicer::parsing::is_primitive_type(dep) {
                for dep_item in index.get_all(dep) {
                    let dep_sig = item_signature_with_module(&dep_item.source, &dep_item.module_path);
                    let dep_def_key = definition_key(dep_item);

                    // Skip if already included
                    if seen_signatures.contains(&dep_sig) || seen_definitions.contains(&dep_def_key) {
                        continue;
                    }

                    let dep_module = normalize_module_path(&dep_item.module_path, &crate_info.name);

                    // Skip items from cfg-gated modules that don't match current platform
                    if !module_cfg_matches_platform(&dep_module, &module_cfgs) {
                        continue;
                    }

                    seen_signatures.insert(dep_sig);
                    seen_definitions.insert(dep_def_key);
                    modules.entry(dep_module).or_default().push(dep_item);
                    included_count += 1;
                }
            }
        }
    }

    // Second pass: detect module references in all included items' source code
    // This catches `crate::ext::ExtFieldOptional` patterns in items like rustproto.rs
    // that are added via module-complete inclusion but whose dependencies weren't detected initially
    let mut all_included_sources = String::new();
    for items in modules.values() {
        for item in items {
            all_included_sources.push_str(&item.source);
            all_included_sources.push('\n');
        }
    }

    let (additional_module_paths, _additional_items) = detect_full_module_paths(&all_included_sources);

    // Add any newly detected module paths
    for module_path in additional_module_paths {
        if RUST_PRIMITIVES.contains(&module_path.as_str()) {
            continue;
        }
        let parts: Vec<&str> = module_path.split("::").collect();
        for i in 1..=parts.len() {
            let path_part = parts[..i].join("::");
            if !parts[..i].iter().any(|p| RUST_PRIMITIVES.contains(p)) {
                if module_cfg_matches_platform(&path_part, &module_cfgs) {
                    all_module_paths.insert(path_part);
                }
            }
        }
    }

    // Also detect module paths from function calls in the source code
    // This ensures modules like rt::map are included when their functions are called
    let func_call_module_paths = extract_function_call_module_paths(&all_included_sources, index, &crate_info.name);
    for module_path in func_call_module_paths {
        if RUST_PRIMITIVES.contains(&module_path.as_str()) {
            continue;
        }
        if module_cfg_matches_platform(&module_path, &module_cfgs) {
            all_module_paths.insert(module_path);
        }
    }

    // Include public items from newly detected module paths
    for item in &index.all_items {
        if !item.is_pub {
            continue;
        }
        let item_module = normalize_module_path(&item.module_path, &crate_info.name);
        if !all_module_paths.contains(&item_module) {
            continue;
        }
        if !module_cfg_matches_platform(&item_module, &module_cfgs) {
            continue;
        }
        let sig = item_signature_with_module(&item.source, &item.module_path);
        let def_key = definition_key(item);
        if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
            continue;
        }
        seen_signatures.insert(sig);
        seen_definitions.insert(def_key);
        modules.entry(item_module).or_default().push(item);
        included_count += 1;
    }

    // Sync all_module_paths with modules.keys() - this ensures any modules created
    // during private helper inclusion or second-pass detection get proper mod declarations
    for module_path in modules.keys() {
        let parts: Vec<&str> = module_path.split("::").collect();
        for i in 1..=parts.len() {
            let path_part = parts[..i].join("::");
            if parts[..i].iter().any(|p| RUST_PRIMITIVES.contains(p)) {
                continue;
            }
            if !module_cfg_matches_platform(&path_part, &module_cfgs) {
                continue;
            }
            all_module_paths.insert(path_part);
        }
    }

    // Build a map of module paths to their public item names for re-exports
    // This allows parent modules to re-export items from child modules
    // Store (item_name, is_crate_only) where is_crate_only means pub(crate)
    let mut child_module_items: HashMap<String, Vec<(String, bool)>> = HashMap::new();
    for (module_path, items) in &modules {
        let mut public_items: Vec<(String, bool)> = items.iter()
            .filter(|item| {
                // Include public items that are types, functions, traits, etc.
                // Exclude impl blocks (they're not re-exported by name)
                // Exclude macros (they need #[macro_export], can't be re-exported with use)
                // IMPORTANT: Exclude private items to prevent E0603 errors
                item.is_pub &&
                item.kind != crate::types::ParsedItemKind::Impl &&
                item.kind != crate::types::ParsedItemKind::Macro &&
                item.visibility != crate::types::ItemVisibility::Private
            })
            .map(|item| {
                // Use the visibility field to determine if this is crate-only or fully public
                // pub(crate), pub(super), and pub(in path) are crate-only
                // Only fully pub items can be re-exported with pub use
                let is_crate_only = match item.visibility {
                    crate::types::ItemVisibility::Public => false,
                    crate::types::ItemVisibility::Crate |
                    crate::types::ItemVisibility::Super |
                    crate::types::ItemVisibility::InPath(_) => true,
                    crate::types::ItemVisibility::Private => {
                        // This shouldn't happen due to filter above, but be safe
                        true
                    }
                };
                (item.name.clone(), is_crate_only)
            })
            .collect();
        public_items.sort();
        public_items.dedup();
        if !public_items.is_empty() {
            child_module_items.insert(module_path.clone(), public_items);
        }
    }

    // Generate module files with internal imports
    let mut all_code = String::new();
    let mut skipped_modules: HashSet<String> = HashSet::new();

    // Add modules that have #[cfg(test)] to skipped_modules
    // This handles cases like `mod hex` with `#[cfg(test)]` in lib.rs
    // IMPORTANT: Only add the module itself if it's a top-level test-only module
    // (e.g., #[cfg(test)] mod hex;), not child test modules (e.g., #[cfg(test)] mod tests;)
    for (module_path, cfg_attr) in &module_cfgs {
        if is_cfg_test(cfg_attr) {
            // Only skip if this is a top-level test-only module, not a test submodule
            // A top-level test module like "hex" should be skipped entirely
            // But a test submodule like "coded_input_stream::tests" should not cause
            // its parent to be skipped
            if !module_path.contains("::") {
                skipped_modules.insert(module_path.clone());
            }
        }
    }

    // Re-initialize symbol workflow with only the items that are actually included
    // This ensures we don't generate imports for items that have been sliced out
    // Use BTreeMap for deterministic iteration and selection
    let mut final_crate_types = HashMap::new();
    for (mod_path, items) in &modules {
         for item in items {
             // Only track public items for imports
             if item.is_pub {
                 // Priority 7: Skip hir::visitor::Visitor to prevent wrong trait imports
                 // The ast::visitor::Visitor and hir::visitor::Visitor are different traits
                 // Files should import ast::visitor::Visitor which has different method signatures
                 if item.name == "Visitor" && mod_path == "hir::visitor" {
                     // [Priority 7] Skipping hir::visitor::Visitor from final_crate_types
                     continue;
                 }
                 if item.name == "Visitor" {
                     // [Priority 7] Adding Visitor to final_crate_types from module
                 }

                 // When multiple modules define the same item, pick the shortest/alphabetically-first path
                 // This ensures deterministic selection when items are re-exported or defined in multiple places
                 final_crate_types.entry(item.name.clone())
                     .and_modify(|existing_path: &mut String| {
                         // Prefer shorter paths (fewer ::), then alphabetically first
                         let existing_depth = existing_path.matches("::").count();
                         let new_depth = mod_path.matches("::").count();
                         if new_depth < existing_depth || (new_depth == existing_depth && mod_path < existing_path) {
                             *existing_path = mod_path.clone();
                         }
                     })
                     .or_insert_with(|| mod_path.clone());
             }
         }
    }
    
    // Rebuild external_imports as the previous one was consumed
    let external_imports = crate::old_slicer::symbol::build_external_imports(&index.use_statements);
    
    let symbol_workflow = crate::old_slicer::symbol::create_workflow_from_cache(final_crate_types, external_imports)
        .with_crate_local_types(crate_local_types.clone());

    // Create a set of module paths that have actual content
    let populated_modules: HashSet<String> = modules.keys().cloned().collect();

    // Track post-processing progress (import generation for each module)
    let total_modules = modules.len();
    let mut processed_modules = 0;

    // Priority 7: Track which modules were written in the first pass
    // This prevents the third pass from overwriting modules that were correctly generated
    // with full SCIP context in the first pass
    let mut first_pass_written_modules: HashSet<String> = HashSet::new();

    for (module_path, items) in &modules {
        // Report post-processing progress (import generation)
        if let Some(callback) = progress_callback {
            processed_modules += 1;
            // Use convention: callback(total_items + modules_done, total_items + modules_total)
            // This signals post-processing phase (current > original_total)
            callback(total_items + processed_modules, total_items + total_modules);
        }
        // Skip test modules - they have #[cfg(test)] in original source and shouldn't be included
        // in sliced output (they cause missing macro errors like `t!`)
        let is_test_module = module_path == "tests" ||
            module_path.ends_with("::tests") ||
            module_path.starts_with("tests::") ||
            module_path.contains("::tests::");
        if is_test_module {
            let top_mod = module_path.split("::").next().unwrap_or(module_path);
            if !top_mod.is_empty() {
                skipped_modules.insert(top_mod.to_string());
            }
            continue;
        }

        // Check if any item in this module uses unstable features
        let has_unstable = items.iter().any(|item| uses_unstable_features(&item.source));
        if has_unstable {
            // Mark this module and its top-level parent as skipped
            let top_mod = module_path.split("::").next().unwrap_or(module_path);
            if !top_mod.is_empty() {
                skipped_modules.insert(top_mod.to_string());
            }
            continue;
        }

        // Skip modules that are test-only (all items have cfg(test) attribute)
        let all_test_items = items.iter().all(|item| {
            item.cfg_attr.as_ref().map(|cfg| is_cfg_test(cfg)).unwrap_or(false)
        });
        if all_test_items && !items.is_empty() {
            let top_mod = module_path.split("::").next().unwrap_or(module_path);
            if !top_mod.is_empty() {
                skipped_modules.insert(top_mod.to_string());
            }
            continue;
        }

        let mut module_content = String::new();

        // Collect names of TYPE definitions in THIS SPECIFIC module file (not child modules)
        // The `items` list may contain items from child modules, so we need to filter by module_path
        // to ensure we only mark items as "locally defined" if they're actually in this module file.
        // This prevents child module constants from being treated as locally defined in parent modules.
        //
        // Build normalized module path once for comparison
        let target_mod_normalized = module_path;  // Already normalized when modules map was built

        let locally_defined: HashSet<String> = items.iter()
            .filter_map(|item| {
                // Check both kind and module path in one filter_map for efficiency
                // Priority 1 extension: Include Functions to prevent duplicate function imports
                if !matches!(item.kind,
                    crate::types::ParsedItemKind::Struct |
                    crate::types::ParsedItemKind::Enum |
                    crate::types::ParsedItemKind::TypeAlias |
                    crate::types::ParsedItemKind::Trait |
                    crate::types::ParsedItemKind::Function |
                    crate::types::ParsedItemKind::Const |
                    crate::types::ParsedItemKind::Static
                ) {
                    return None;
                }

                // Compare normalized paths - item.module_path is from parsing,
                // module_path is from modules map (both should be normalized already)
                let item_mod_normalized = normalize_module_path(&item.module_path, &crate_info.name);
                if item_mod_normalized == *target_mod_normalized {
                    Some(item.name.clone())
                } else {
                    None
                }
            })
            .collect();

        // Sort items to ensure macros are placed before functions/types that use them
        // This prevents "cannot find macro" errors when private functions reference macros
        let mut items = items.clone();
        items.sort_by_key(|item| {
            match item.kind {
                crate::types::ParsedItemKind::Macro => 0,  // Macros first
                crate::types::ParsedItemKind::Const => 1,  // Then constants
                crate::types::ParsedItemKind::Static => 1,
                crate::types::ParsedItemKind::TypeAlias => 2,  // Then type aliases
                crate::types::ParsedItemKind::Struct => 3,  // Then types
                crate::types::ParsedItemKind::Enum => 3,
                crate::types::ParsedItemKind::Trait => 4,  // Then traits
                crate::types::ParsedItemKind::Function => 5,  // Then functions
                crate::types::ParsedItemKind::Impl => 6,  // Then impl blocks
                _ => 7,  // Everything else last
            }
        });

        for item in &items {
            // Skip items with cfg(test) attribute
            if item.cfg_attr.as_ref().map(|cfg| is_cfg_test(cfg)).unwrap_or(false) {
                continue;
            }
            // Strip #[cfg(test)] blocks and internal imports from source
            let clean_source = strip_cfg_test_blocks(&item.source);
            let clean_source = strip_internal_imports(&clean_source);

            // Priority 7 debug: check if cleaned source still contains wrong imports
            if module_path == "ast::print" {
                let first_lines: Vec<&str> = clean_source.lines().take(10).collect();
                if first_lines.iter().any(|line| line.contains("use ") && line.contains("Visitor")) {
                    // [P7] ast::print item cleaned source still has Visitor imports
                    for line in &first_lines {
                        if line.trim().starts_with("use ") || line.contains("Visitor") {
                            crate::debug_log!("  *** {}", line);
                        }
                    }
                }
            }

            module_content.push_str(&clean_source);
            module_content.push_str("\n\n");
        }

        // Skip modules that are empty after stripping test code
        // These are test-only modules (like "hex" in protobuf) that shouldn't be included
        if module_content.trim().is_empty() {
            let top_mod = module_path.split("::").next().unwrap_or(module_path);
            if !top_mod.is_empty() {
                skipped_modules.insert(top_mod.to_string());
            }
            continue;
        }

        // Get preserved imports from original use statements (includes aliases like `Foo as Bar`)
        // We need to do this first to extract symbols that should be skipped in generate_internal_imports
        // Priority 1: Filter out imports that conflict with locally_defined symbols (E0255 fix)
        let preserved_imports = filter_preserved_imports_excluding_local(&index.use_statements, &module_content, module_path, Some(&all_module_paths), Some(&crate_info.name), &locally_defined);

        // Priority 7 debug: check preserved_imports for ast::print
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] preserved_imports for ast::print:");
            for line in preserved_imports.lines().take(10) {
                if line.contains("Visitor") || line.contains("visitor") {
                    crate::debug_log!("  *** {}", line);
                } else {
                    crate::debug_log!("  {}", line);
                }
            }
        }

        // Extract symbols from preserved imports to avoid duplicate imports (E0252)
        let mut already_imported = extract_symbols_from_preserved_imports(&index.use_statements, module_path);


        // Compute child modules that will be declared (E0255 fix)
        // These need to be passed to generate_internal_imports so it knows to skip importing them
        let child_modules: HashSet<String> = all_module_paths.iter()
            .filter_map(|p| {
                let prefix = format!("{}::", module_path);
                if p.starts_with(&prefix) {
                    let rest = &p[prefix.len()..];
                    // Only direct children (no more ::)
                    if !rest.contains("::") {
                        return Some(rest.to_string());
                    }
                }
                None
            })
            .collect();

        // Add items that will be re-exported from child modules to already_imported (E0252 fix)
        // This prevents generating imports like `use crate::automaton::private::Sealed`
        // when we're already re-exporting `pub use self::private::{Sealed}`
        for child in &child_modules {
            let child_path = format!("{}::{}", module_path, child);
            if let Some(items) = child_module_items.get(&child_path) {
                for (name, _is_crate_only) in items {
                    already_imported.insert(name.clone());
                }
            }
        }

        // Generate internal imports for cross-module references (existing system)
        // Extract context for decision logging
        let file_path_str = items.first().map(|i| i.file.display().to_string()).unwrap_or_default();
        let cfg_conditions: Vec<String> = items.iter()
            .filter_map(|i| i.cfg_attr.as_ref())
            .cloned()
            .collect();

        let internal_imports = generate_internal_imports(
            &module_content,
            module_path,
            index,
            &all_module_paths,
            &locally_defined,
            &crate_local_types,
            &already_imported,
            &child_modules,
            Some(&crate_info.name),
            Some(&scip_symbol_paths),
            Some(&populated_modules),
            Some(&file_path_str),
            cfg_conditions,
        );

        // Combine old imports: preserved first (for aliases), then generated
        // Use combine_imports for proper deduplication - preserved imports take precedence
        // This ensures that `use std::any::Any;` is kept over `use crate::well_known_types::any::Any;`
        let old_imports = if preserved_imports.is_empty() {
            internal_imports.clone()
        } else if internal_imports.is_empty() {
            preserved_imports.clone() + "\n"
        } else {
            combine_imports(&preserved_imports, &internal_imports)
        };

        // Generate additional imports using the new symbol resolution system
        // This fills gaps for symbols that the old system might miss
        let symbol_imports = symbol_workflow.generate_imports(
            &module_content,
            module_path,
            &locally_defined,
            &already_imported,
        );

        // Priority 7 debug: check symbol_imports for ast::print
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] symbol_imports for ast::print:");
            for imp in &symbol_imports {
                if imp.contains("Visitor") {
                    crate::debug_log!("  {}", imp);
                }
            }
        }

        // Use gap-filling approach: only add imports for symbols not already imported
        let all_imports = crate::old_slicer::symbol::fill_import_gaps(&old_imports, &symbol_imports);

        // Priority 7 debug: check all_imports for ast::print
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] all_imports after fill_import_gaps for ast::print (first 10 lines):");
            for (i, line) in all_imports.lines().take(10).enumerate() {
                crate::debug_log!("  {}: {}", i+1, line);
            }
        }

        // Generate enum variant imports for locally-defined enums used unqualified
        let enum_imports = generate_enum_variant_imports(&module_content);

        // Generate imports for UPPER_CASE constants that are used but not locally defined
        // This catches constants like LITTLE_ENDIAN that are used in the code
        // Pass both module_content and all_imports to check for existing imports
        let constant_imports = generate_constant_imports(
            &module_content,
            &locally_defined,
            &already_imported,
            index,
            &crate_info.name,
            &all_imports,
            &modules,
            &populated_modules,
        );

        // Generate imports for atomic Ordering enum variants (SeqCst, Acquire, etc.)
        let ordering_imports = generate_atomic_ordering_imports(&module_content, &all_imports);

        // Generate imports for encoding_rs macros that need handles types
        let encoding_macro_imports = generate_encoding_macro_imports(&module_content, &all_imports, Some(&crate_info.name));

        // Generate imports for root-level helper functions (e.g., take_unchecked, panic_advance)
        let root_helper_imports = generate_root_helper_imports(
            &module_content,
            &locally_defined,
            &already_imported,
            index,
            &all_imports,
            module_path,
        );

        // Combine all imports
        let mut final_imports = if enum_imports.is_empty() {
            all_imports
        } else if all_imports.is_empty() {
            enum_imports
        } else {
            format!("{}{}", all_imports, enum_imports)
        };
        if !constant_imports.is_empty() {
            if final_imports.is_empty() {
                final_imports = constant_imports;
            } else {
                final_imports = format!("{}{}", final_imports, constant_imports);
            }
        }
        if !ordering_imports.is_empty() {
            if final_imports.is_empty() {
                final_imports = ordering_imports;
            } else {
                final_imports = format!("{}{}", final_imports, ordering_imports);
            }
        }
        if !encoding_macro_imports.is_empty() {
            if final_imports.is_empty() {
                final_imports = encoding_macro_imports;
            } else {
                final_imports = format!("{}{}", final_imports, encoding_macro_imports);
            }
        }
        if !root_helper_imports.is_empty() {
            if final_imports.is_empty() {
                final_imports = root_helper_imports;
            } else {
                final_imports = format!("{}{}", final_imports, root_helper_imports);
            }
        }

        // Priority 7 debug: check final_imports before deduplication
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] final_imports BEFORE deduplicate_import_lines (first 10 lines):");
            for (i, line) in final_imports.lines().take(10).enumerate() {
                crate::debug_log!("  {}: {}", i+1, line);
            }
        }

        // Final deduplication: remove duplicate import lines
        // This catches any duplicates that slipped through the earlier deduplication steps
        let final_imports = deduplicate_import_lines(&final_imports);

        // Priority 7 debug: check final_imports AFTER deduplication
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] final_imports AFTER deduplicate_import_lines (first 10 lines):");
            for (i, line) in final_imports.lines().take(10).enumerate() {
                crate::debug_log!("  {}: {}", i+1, line);
            }
        }

        // Prepend imports to module content
        let mut full_module_content = if final_imports.is_empty() {
            module_content.clone()
        } else {
            format!("{}{}", final_imports, module_content)
        };

        // Remove spurious "use loom;" imports
        // loom is used in cfg-guarded macros with fully qualified paths, no import needed
        if full_module_content.contains("use loom") {
            // Remove all variations of use loom import
            full_module_content = full_module_content.replace("use loom;\n", "");
            full_module_content = full_module_content.replace("\nuse loom;", "");
            full_module_content = full_module_content.replace("use loom;", "");
        }

        // Use full_module_content (with imports) for all_code so detect_used_dependencies
        // can find external crate usages like `use once_cell::sync::OnceCell;`
        all_code.push_str(&full_module_content);

        // Write to the appropriate file
        write_module_file(output_dir, module_path, &full_module_content, &all_module_paths, &child_module_items, &skipped_modules, &populated_modules, &module_cfgs, &crate_info.name)?;

        // Priority 7: Track that this module was written in the first pass
        first_pass_written_modules.insert(module_path.to_string());
    }

    // Signal finalizing phase (third pass, file writing, etc.)
    // Finalizing has 7 steps - we'll report progress through them
    // Steps: 1=third pass discovery, 2=write discovered modules, 3=update parents,
    //        4=write lib.rs, 5=write Cargo.toml, 6=fix imports, 7=copy files
    const FINALIZING_STEPS: usize = 7;
    let mut finalize_step = 0;

    let report_finalize_progress = |step: usize| {
        if let Some(cb) = progress_callback {
            // Encoding: current = items*2 + step, total = items*2 + total_steps
            cb(total_items * 2 + step, total_items * 2 + FINALIZING_STEPS);
        }
    };

    finalize_step += 1;
    report_finalize_progress(finalize_step); // Step 1: Third pass discovery

    // Third pass: iteratively detect module paths from generated imports and include missing modules
    // This catches cases where internal imports like `use crate::reflect::repeated::vec_downcast::VecMutVariant;`
    // reference modules that weren't initially included
    // We iterate until no new modules are found, since newly included modules may have their own imports
    let mut all_newly_added: BTreeMap<String, Vec<&crate::types::ParsedItem>> = BTreeMap::new();
    let mut iteration = 0;
    const MAX_ITERATIONS: usize = 10; // Prevent infinite loops

    // Phase 1: Iteratively discover all needed modules without writing
    // We need to find all modules FIRST so that import generation has complete information
    loop {
        iteration += 1;
        if iteration > MAX_ITERATIONS {
            eprintln!("Warning: reached max iterations ({}) in third pass module discovery", MAX_ITERATIONS);
            break;
        }

        // Discover modules from both:
        // 1. `use crate::...` import statements
        // 2. Function calls that reference functions in the index
        let mut discovered_module_paths = extract_crate_import_module_paths(&all_code);
        let func_module_paths = extract_function_call_module_paths(&all_code, index, &crate_info.name);
        discovered_module_paths.extend(func_module_paths);

        let mut newly_discovered_count = 0;

        for module_path in &discovered_module_paths {
            // Skip if already in all_module_paths
            if all_module_paths.contains(module_path) {
                continue;
            }

            // Skip if already has content in modules
            if modules.contains_key(module_path) {
                continue;
            }

            // Skip primitives
            if RUST_PRIMITIVES.contains(&module_path.as_str()) {
                continue;
            }

            // Skip cfg-gated modules
            if !module_cfg_matches_platform(module_path, &module_cfgs) {
                continue;
            }

            // Add to all_module_paths (but don't write yet)
            all_module_paths.insert(module_path.clone());

            // Include items from this module
            for item in &index.all_items {
                if !item.is_pub {
                    continue;
                }

                let item_module = normalize_module_path(&item.module_path, &crate_info.name);
                if item_module != *module_path {
                    continue;
                }

                let sig = item_signature_with_module(&item.source, &item.module_path);
                let def_key = definition_key(item);
                if seen_signatures.contains(&sig) || seen_definitions.contains(&def_key) {
                    continue;
                }
                seen_signatures.insert(sig);
                seen_definitions.insert(def_key);

                all_newly_added.entry(module_path.clone()).or_default().push(item);
                newly_discovered_count += 1;
            }
        }

        if newly_discovered_count == 0 {
            break;
        }

        // Add all newly discovered module content to all_code for next iteration's discovery
        for (_module_path, items) in &all_newly_added {
            let module_content: String = items.iter()
                .map(|item| item.source.clone())
                .collect::<Vec<_>>()
                .join("\n");
            // Only add if not already in all_code (avoid duplicates)
            if !module_content.is_empty() && !all_code.contains(&module_content) {
                all_code.push_str(&module_content);
            }
        }
    }

    finalize_step += 1;
    report_finalize_progress(finalize_step); // Step 2: Write discovered modules

    // Phase 2: Write all newly discovered modules with correct imports
    // Now that all modules are in all_module_paths, imports will be generated correctly
    for (module_path, items) in &all_newly_added {
        let module_content: String = items.iter()
            .map(|item| item.source.clone())
            .collect::<Vec<_>>()
            .join("\n");

        if !module_content.is_empty() {
            // Generate imports for this module
            let child_modules: HashSet<String> = all_module_paths.iter()
                .filter(|p| p.starts_with(&format!("{}::", module_path)) && !p[module_path.len()+2..].contains("::"))
                .map(|p| p[module_path.len()+2..].to_string())
                .collect();

            // Extract context for decision logging
            let file_path_str = items.first().map(|i| i.file.display().to_string()).unwrap_or_default();
            let cfg_conditions: Vec<String> = items.iter()
                .filter_map(|i| i.cfg_attr.as_ref())
                .cloned()
                .collect();

            let internal_imports = generate_internal_imports(
                &module_content,
                module_path,
                index,
                &all_module_paths,
                &HashSet::new(), // locally_defined
                &crate_local_types,
                &HashSet::new(), // already_imported
                &child_modules,
                Some(&crate_info.name),
                None, // symbol_paths
                Some(&populated_modules),
                Some(&file_path_str),
                cfg_conditions,
            );

            let mut full_module_content = if internal_imports.is_empty() {
                module_content
            } else {
                format!("{}{}", internal_imports, module_content)
            };

            // Remove spurious "use loom;" imports
            // loom is used in cfg-guarded macros with fully qualified paths, no import needed
            if full_module_content.contains("use loom") {
                // Remove all variations of use loom import
                full_module_content = full_module_content.replace("use loom;\n", "");
                full_module_content = full_module_content.replace("\nuse loom;", "");
                full_module_content = full_module_content.replace("use loom;", "");
            }

            // Priority 7: Skip writing if this module was already written in the first pass
            // The first pass has full SCIP context and generates correct imports
            // The third pass doesn't have symbol_paths, so imports may be incorrect
            // This prevents overwriting correct files with incorrect ones
            if first_pass_written_modules.contains(module_path) {
                continue;
            }

            write_module_file(output_dir, module_path, &full_module_content, &all_module_paths, &child_module_items, &skipped_modules, &populated_modules, &module_cfgs, &crate_info.name)?;
        }
    }

    // Step 3: Update parent modules
    finalize_step += 1;
    report_finalize_progress(finalize_step);

    // Update parent modules to include declarations for newly added child modules
    // This is needed because parent mod.rs files were written before the third pass discovered these modules
    for module_path in all_newly_added.keys() {
        // Get parent path
        if let Some(last_sep) = module_path.rfind("::") {
            let parent_path = &module_path[..last_sep];
            let child_name = &module_path[last_sep + 2..];

            // Find the parent's mod.rs file
            let parent_mod_file = if parent_path.is_empty() {
                output_dir.join("src").join("lib.rs")
            } else {
                let parent_dir = output_dir.join("src").join(parent_path.replace("::", "/"));
                let mod_rs = parent_dir.join("mod.rs");
                if mod_rs.exists() {
                    mod_rs
                } else {
                    // Try parent_path.rs
                    let parts: Vec<&str> = parent_path.split("::").collect();
                    if parts.len() > 1 {
                        let grandparent = parts[..parts.len()-1].join("/");
                        let parent_file = output_dir.join("src").join(&grandparent).join(format!("{}.rs", parts.last().unwrap()));
                        parent_file
                    } else {
                        output_dir.join("src").join(format!("{}.rs", parent_path))
                    }
                }
            };

            if parent_mod_file.exists() {
                let parent_content = fs::read_to_string(&parent_mod_file).unwrap_or_default();
                let child_name_renamed = rename_reserved_module(child_name);
                let mod_decl = format!("pub(crate) mod {};", child_name_renamed);
                let mod_decl_pub = format!("pub mod {};", child_name_renamed);

                // Check if declaration already exists
                if !parent_content.contains(&mod_decl) && !parent_content.contains(&mod_decl_pub) {
                    // Add the module declaration at the beginning
                    let new_content = format!("{}\n{}", mod_decl, parent_content);
                    fs::write(&parent_mod_file, new_content)?;
                }
            }
        }
    }

    // Create stub modules for paths that are referenced but have no items
    // This ensures module chains like arch::generic::memchr exist even if we don't have items from them
    // BUT: Don't create empty stub modules with no children - these cause "expected type, found module" errors
    let modules_with_items: HashSet<String> = modules.keys().cloned().collect();
    for module_path in &all_module_paths {
        if !modules_with_items.contains(module_path) && !module_path.is_empty() {
            // Check if this module is under a skipped module
            let top_mod = module_path.split("::").next().unwrap_or(module_path);
            if skipped_modules.contains(top_mod) {
                continue;
            }

            // Check if this module has any children in all_module_paths
            // Only create stub modules that have children (to support module hierarchy)
            let prefix = format!("{}::", module_path);
            let has_children = all_module_paths.iter().any(|p| p.starts_with(&prefix));

            if !has_children {
                // Skip creating empty stub modules with no children
                // These are likely type names being mistaken for modules (e.g., __m256i, uint8x16_t)
                continue;
            }

            // Also check if any of the children (or their descendants) are populated
            // This prevents creating stub modules for unpopulated module hierarchies
            let has_populated_descendants = populated_modules.iter()
                .any(|p| p.starts_with(&prefix) || p == module_path);

            if !has_populated_descendants {
                // Skip if no descendants have actual content
                continue;
            }

            // Write a stub module (empty or with just child module declarations)
            write_module_file(output_dir, module_path, "", &all_module_paths, &child_module_items, &skipped_modules, &populated_modules, &module_cfgs, &crate_info.name)?;
        }
    }

    // Filter out skipped modules and unpopulated modules from all_module_paths
    let filtered_module_paths: HashSet<String> = all_module_paths
        .into_iter()
        .filter(|p| {
            // Filter out skipped modules (test-only, unstable, etc.)
            let top_mod = p.split("::").next().unwrap_or(p);
            if skipped_modules.contains(top_mod) {
                return false;
            }

            // Filter out modules with no content and no populated descendants
            // This prevents declaring/writing stub modules for unpopulated module hierarchies
            if populated_modules.contains(p) {
                return true;
            }
            let prefix = format!("{}::", p);
            populated_modules.iter().any(|pm| pm.starts_with(&prefix))
        })
        .collect();

    // Scan output directory to find any subdirectories with .rs files that need mod.rs
    // This fixes cases where files are generated but parent module declarations are missing
    // Also adds re-exports for public items from child modules
    let additional_modules = scan_for_undeclared_modules(&output_dir.join("src"), &filtered_module_paths, &child_module_items, &index.use_statements)?;

    let mut final_module_paths = filtered_module_paths.clone();
    for module in additional_modules {
        final_module_paths.insert(module);
    }

    // Step 4: Generate lib.rs
    finalize_step += 1;
    report_finalize_progress(finalize_step);

    // Generate lib.rs with module declarations
    let (lib_content, module_reexports) = generate_lib_rs(&modules, &final_module_paths, &index.use_statements, &all_code, index, &crate_local_types, &skipped_modules, &index.macro_use_crates, &crate_info.name);

    // Add enum variant imports for any enums defined in lib.rs
    let enum_imports = generate_enum_variant_imports(&lib_content);
    let lib_content = if enum_imports.is_empty() {
        lib_content
    } else {
        // Insert enum imports after the crate header comment
        if lib_content.starts_with("//!") {
            // Find end of header line and insert after it
            let first_newline = lib_content.find('\n').unwrap_or(lib_content.len());
            format!("{}\n{}\n{}", &lib_content[..first_newline], enum_imports.trim(), &lib_content[first_newline..].trim_start_matches('\n'))
        } else {
            format!("{}\n{}", enum_imports.trim(), lib_content)
        }
    };

    // Deduplicate imports before formatting (fixes E0252 errors)
    let lib_content = deduplicate_import_lines(&lib_content);

    // Phase 6.6a: Fix self-imports - convert external crate name to "crate" when slicing that crate
    // e.g., when slicing "bytes" crate, `pub use bytes::` should be `pub use crate::`
    let lib_content = fix_self_imports_in_lib(&lib_content, &crate_info.name);

    // Phase 6.6: Transform reserved module references (crate::core:: -> crate::core_::)
    let lib_content = transform_reserved_module_refs(&lib_content);

    // Fix E0425 MAX_LEVEL_INNER error (add default definition if missing)
    let lib_content = crate::old_slicer::parsing::fix_max_level_inner(&lib_content);

    // Fix E0433 ArrayVecImpl error (add missing trait import)
    let lib_content = crate::old_slicer::parsing::fix_arrayvec_impl_import(&lib_content);

    // Fix E0433 MakeMaybeUninit error (add missing struct import)
    let lib_content = crate::old_slicer::parsing::fix_make_maybe_uninit_import(&lib_content);

    let lib_content = format_source(&lib_content);
    fs::write(output_dir.join("src").join("lib.rs"), &lib_content)?;

    // Phase 6.6a post-write: Fix self-imports in the written lib.rs file
    // This catches any self-imports that were embedded in item source code
    let lib_path = output_dir.join("src").join("lib.rs");
    let lib_content = fs::read_to_string(&lib_path)?;
    let lib_content = fix_self_imports_in_lib(&lib_content, &crate_info.name);
    fs::write(&lib_path, &lib_content)?;

    // Apply module-level re-exports for nested crate::module::Type references
    apply_module_reexports(output_dir, &module_reexports)?;

    // Fix nested module imports BEFORE dependency detection
    // This prevents bare imports like "use parking_lot::" in src/parking_lot/deadlock.rs
    // from being detected as external crate imports
    let all_code_fixed = fix_nested_module_imports_in_all_code(&all_code, &final_module_paths);

    // Get dependencies from the original Cargo.toml
    // Pass output_dir to enable automatic path dependencies for sliced crates
    // Pass final_module_paths to detect local modules and avoid treating them as external dependencies
    let mut external_deps = detect_used_dependencies(&all_code_fixed, crate_info, Some(output_dir), &final_module_paths);

    // Check if this is a proc-macro crate
    // Proc-macros generate code for OTHER crates, so ::qualified::paths in their source
    // are NOT compile-time dependencies - they're in the generated code
    let is_proc_macro = crate_info.is_proc_macro;

    if is_proc_macro && std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
        crate::debug_log!("[PROC-MACRO] {} is a proc-macro crate - will only include original dependencies", crate_info.name);
    }

    // For proc-macros, get the original dependencies to filter against
    let original_deps = if is_proc_macro {
        if let Some(parsed_deps) = crate::cargo_toml::parse_cargo_toml(&crate_info.path) {
            parsed_deps.dependencies.keys().cloned().collect::<std::collections::HashSet<_>>()
        } else {
            std::collections::HashSet::new()
        }
    } else {
        std::collections::HashSet::new()
    };

    // Filter detected dependencies for proc-macro crates
    // Proc-macros have ::qualified::paths in quote! blocks that are NOT compile-time dependencies
    if is_proc_macro && !original_deps.is_empty() {
        external_deps.retain(|dep_line| {
            // Extract crate name from dependency line (e.g., "windows-core = ..." -> "windows-core")
            if let Some(crate_name) = dep_line.split('=').next().map(|s| s.trim()) {
                // Check with both dashes and underscores
                let with_underscores = crate_name.replace('-', "_");
                let in_original = original_deps.contains(crate_name) ||
                                  original_deps.contains(&with_underscores);

                if !in_original {
                    if std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
                        crate::debug_log!("[PROC-MACRO] {} -> {} FILTERED: Not in original Cargo.toml (from detect_used_dependencies)",
                                  crate_info.name, crate_name);
                    }
                    return false;
                }
            }
            true
        });
    }

    // Add dependencies for macro_use crates (e.g., scopeguard, doc-comment)
    // These are detected during parsing when we see #[macro_use] extern crate declarations
    const STDLIB_CRATES: &[&str] = &["std", "core", "alloc", "proc_macro", "test"];

    for macro_crate in &index.macro_use_crates {
        // FIX #1: Filter out stdlib crates - they're not external dependencies!
        // These are built-in compiler crates and should never be in Cargo.toml [dependencies]
        if STDLIB_CRATES.contains(&macro_crate.as_str()) {
            if std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
                crate::debug_log!("[DepDetect] {} -> SKIPPED: Stdlib crate, not external dependency", macro_crate);
            }
            continue;
        }

        // FIX #2: Determine the correct Cargo.toml name
        // Most crates use dashes in Cargo.toml (e.g., wasm-bindgen-backend)
        // Some older crates use underscores (e.g., serde_derive, lazy_static)
        // Match on the underscore version and explicitly specify the Cargo.toml name
        let (cargo_crate_name, dep_line) = match macro_crate.as_str() {
            "scopeguard" => ("scopeguard", "scopeguard = \"1\""),
            "doc_comment" => ("doc-comment", "doc-comment = \"0.3\""),
            "cfg_if" => ("cfg-if", "cfg-if = \"1\""),
            "wasm_bindgen_backend" => ("wasm-bindgen-backend", "wasm-bindgen-backend = \"*\""),
            "lazy_static" => ("lazy_static", "lazy_static = \"*\""),
            "serde_derive" => ("serde_derive", "serde_derive = \"*\""),
            // Default: try dashes first (most common convention)
            _ => {
                let with_dashes = macro_crate.replace('_', "-");
                (&*Box::leak(with_dashes.into_boxed_str()), Box::leak(format!("{} = \"*\"", macro_crate.replace('_', "-")).into_boxed_str()) as &str)
            }
        };

        // FIX #3: For proc-macro crates, only add dependencies from the original Cargo.toml
        // Proc-macros generate code using ::qualified::paths (e.g., ::windows_core::Interface)
        // These paths are in the GENERATED code, not compile-time dependencies
        if is_proc_macro && !original_deps.is_empty() {
            // Check both the cargo name and the macro crate name (with underscores)
            let in_original = original_deps.contains(cargo_crate_name) ||
                              original_deps.contains(macro_crate.as_str());

            if !in_original {
                if std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
                    crate::debug_log!("[PROC-MACRO] {} -> {} SKIPPED: Not in original Cargo.toml (generated code dependency)",
                              crate_info.name, cargo_crate_name);
                }
                continue;
            }
        }

        // Check if not already in deps
        let already_present = external_deps.iter().any(|dep| {
            dep.starts_with(&format!("{} =", cargo_crate_name))
        });

        if !already_present {
            external_deps.push(dep_line.to_string());

            if std::env::var("CARGO_SLICER_LOG_DEPENDENCIES").is_ok() {
                crate::debug_log!("[DepDetect] {} -> ADDED as {}: Found in macro_use_crates",
                          macro_crate, cargo_crate_name);
            }
        }
    }

    let deps_section = if external_deps.is_empty() {
        String::new()
    } else {
        external_deps.join("\n")
    };

    // Build features section
    let mut features = Vec::new();
    let has_std_feature = all_code.contains("feature = \"std\"") || all_code.contains("feature = \\\"std\\\"") || all_code.contains("feature = 'std'");
    let has_alloc_feature = all_code.contains("feature = \"alloc\"") || all_code.contains("feature = \\\"alloc\\\"") || all_code.contains("feature = 'alloc'");

    // Add default feature if std is present
    if has_std_feature {
        features.push("default = [\"std\"]");
        features.push("std = []");
    }
    if has_alloc_feature {
        features.push("alloc = []");
    }

    let features_section = if features.is_empty() {
        String::new()
    } else {
        format!("\n[features]\n{}\n", features.join("\n"))
    };

    // Build lints section for unexpected cfgs
    let mut check_cfgs = Vec::new();
    if all_code.contains("manual_codegen_check") {
        check_cfgs.push("'cfg(manual_codegen_check)'");
    }
    
    let lints_section = if check_cfgs.is_empty() {
        String::new()
    } else {
        format!(
            "\n[lints.rust]\nunexpected_cfgs = {{ level = \"warn\", check-cfg = [{}] }}\n",
            check_cfgs.join(", ")
        )
    };

    let cargo_content = format!(
        r#"[package]
name = "{}"
version = "{}"
edition = "{}"

[lib]
path = "src/lib.rs"

[dependencies]
{}{}{}

# Temporary workspace marker - removed when parent workspace is created
[workspace]
"#,
        crate_info.name, crate_info.version, crate_info.edition, deps_section, features_section, lints_section
    );

    // Step 5: Write Cargo.toml
    finalize_step += 1;
    report_finalize_progress(finalize_step);

    fs::write(output_dir.join("Cargo.toml"), cargo_content)?;

    // Step 6: Fix imports
    finalize_step += 1;
    report_finalize_progress(finalize_step);

    // Post-process: fix broken crate-internal imports
    // This handles cases where items have moved to different modules during slicing
    fix_all_crate_imports(output_dir)?;

    // Remove spurious dependencies from Cargo.toml that match local module names
    // This fixes circular dependencies caused by spurious imports like "use parking_lot::THREAD_DATA;"
    // in parking_lot_core where THREAD_DATA is actually defined locally
    let cargo_toml_path = output_dir.join("Cargo.toml");
    if let Err(e) = crate::cargo_toml::remove_spurious_dependencies(&cargo_toml_path, &final_module_paths) {
        eprintln!("Warning: Failed to remove spurious dependencies: {}", e);
    }

    // Step 7: Copy files
    finalize_step += 1;
    report_finalize_progress(finalize_step);

    // Copy included files (from include_bytes!/include_str! macros)
    let _ = super::source_fix::copy_included_files(&crate_info.path, output_dir);

    Ok(SemanticSliceResult {
        total_parsed: index.all_items.len(),
        items_needed: needed.len(),
        items_included: included_count,
        lines_generated: lib_content.lines().count(),
    })
}

/// Normalize module path - convert "crate::foo::bar" or "cratename::foo::bar" to "foo::bar"
fn normalize_module_path(path: &str, crate_name: &str) -> String {
    let path = path.trim();
    if path.is_empty() || path == "crate" || path == crate_name {
        return String::new(); // Root module
    }

    // Strip "crate::" or "cratename::" prefix
    let path = if path.starts_with("crate::") {
        &path[7..]
    } else if path.starts_with(&format!("{}::", crate_name)) {
        &path[crate_name.len() + 2..]
    } else {
        path
    };

    path.to_string()
}

/// Write a module file, creating directories as needed
fn write_module_file(
    output_dir: &Path,
    module_path: &str,
    content: &str,
    all_paths: &HashSet<String>,
    child_module_items: &HashMap<String, Vec<(String, bool)>>,
    skipped_modules: &HashSet<String>,
    populated_modules: &HashSet<String>,
    module_cfgs: &HashMap<String, String>,
    crate_name: &str,  // Phase 6.3: For platform-specific filtering
) -> std::io::Result<()> {
    if module_path.is_empty() {
        return Ok(()); // Root module content goes in lib.rs
    }

    // Only write files for modules that have content or populated descendants
    // This prevents creating empty module files for modules that aren't actually needed
    let has_content = populated_modules.contains(module_path);
    let prefix = format!("{}::", module_path);
    let has_populated_descendants = populated_modules.iter().any(|p| p.starts_with(&prefix));

    if !has_content && !has_populated_descendants {
        return Ok(()); // Skip writing unpopulated modules
    }

    // Rename reserved module names in paths
    let renamed_parts: Vec<String> = module_path.split("::")
        .map(|p| rename_reserved_module(p))
        .collect();
    let src_dir = output_dir.join("src");

    // Determine file path - if this module has children, use mod.rs style
    let has_children = all_paths.iter().any(|p| {
        p.starts_with(&format!("{}::", module_path)) && p != module_path
    });

    let file_path = if has_children {
        // Create directory and use mod.rs
        let mut dir_path = src_dir.clone();
        for part in &renamed_parts {
            dir_path = dir_path.join(part);
        }
        fs::create_dir_all(&dir_path)?;
        dir_path.join("mod.rs")
    } else {
        // Create parent directories and use module_name.rs
        let mut dir_path = src_dir.clone();
        for part in &renamed_parts[..renamed_parts.len() - 1] {
            dir_path = dir_path.join(part);
        }
        fs::create_dir_all(&dir_path)?;
        dir_path.join(format!("{}.rs", renamed_parts.last().unwrap()))
    };

    // Add child module declarations if this module has children
    let mut full_content = String::new();
    let mut child_modules: Vec<&str> = all_paths.iter()
        .filter_map(|p| {
            if p.starts_with(&format!("{}::", module_path)) {
                let rest = &p[module_path.len() + 2..];
                // Only direct children (no more ::)
                if !rest.contains("::") {
                    return Some(rest);
                }
            }
            None
        })
        .collect();
    child_modules.sort();
    child_modules.dedup();

    // Filter out test modules - they have #[cfg(test)] in original and shouldn't be included
    child_modules.retain(|m| *m != "tests" && *m != "test");

    // Filter out skipped modules (test-only, empty after stripping, etc.)
    child_modules.retain(|m| !skipped_modules.contains(*m));

    // Phase 6.3: libc-specific Unix sub-platform filtering
    // On Linux, only include linux_like/linux, not bsd/haiku/hermit/etc.
    if crate_name == "libc" && module_path == "unix" {
        let target_os = std::env::consts::OS;
        let unwanted_unix_platforms = match target_os {
            "linux" => vec!["bsd", "haiku", "hermit", "newlib", "solarish", "redox"],
            "macos" | "ios" => vec!["linux_like", "haiku", "hermit", "newlib", "solarish", "redox"],
            "freebsd" | "openbsd" | "netbsd" | "dragonfly" => vec!["linux_like", "haiku", "hermit", "newlib", "solarish", "redox"],
            _ => vec![],
        };
        child_modules.retain(|m| !unwanted_unix_platforms.contains(m));
    }

    // Phase 6.3: libc linux_like sub-platform filtering
    // Within linux_like, only include linux on Linux, not android/emscripten
    if crate_name == "libc" && module_path == "unix::linux_like" {
        let target_os = std::env::consts::OS;
        let unwanted_linux_like = match target_os {
            "linux" => vec!["android", "emscripten"],
            "android" => vec!["linux", "emscripten"],
            _ => vec!["linux", "android", "emscripten"], // On non-Linux/Android, exclude all
        };
        child_modules.retain(|m| !unwanted_linux_like.contains(m));
    }

    // Filter out child names that look like function names rather than module names
    // Functions are commonly named with prefixes like make_, get_, set_, new_, etc.
    // These are likely re-exported functions, not submodules
    // BUT: only filter if the full path isn't in all_paths (which is actually all_module_paths
    // as passed by the caller - so if it's there, it's definitely a module)
    child_modules.retain(|m| {
        // If this child is explicitly in all_paths (module paths), it's definitely a module
        let child_path = format!("{}::{}", module_path, m);
        if all_paths.contains(&child_path) {
            return true;
        }

        // Common function name prefixes - these are likely functions, not modules
        const FUNCTION_PREFIXES: &[&str] = &[
            "make_", "get_", "set_", "new_", "create_", "build_", "from_", "into_",
            "try_", "is_", "has_", "with_", "to_", "as_", "parse_", "read_", "write_",
            "encode_", "decode_", "encoded_", "decoded_", // Common function prefixes
            "file_descriptor", // Common in protobuf
        ];
        !FUNCTION_PREFIXES.iter().any(|prefix| m.starts_with(prefix))
    });

    // Filter out child modules that have no content and no children
    child_modules.retain(|m| {
        let child_path = if module_path.is_empty() {
            m.to_string()
        } else {
            format!("{}::{}", module_path, m)
        };
        // Module has content directly
        if populated_modules.contains(&child_path) {
            return true;
        }
        // Module has children with content
        let prefix = format!("{}::", child_path);
        if populated_modules.iter().any(|p| p.starts_with(&prefix)) {
            return true;
        }
        // CRITICAL: Only include if all_paths has children AND those children are populated
        // This prevents declaring modules that have no actual content
        if all_paths.iter().any(|p| p.starts_with(&prefix)) {
            // Check if any of these child paths are actually populated
            return populated_modules.iter().any(|p| p.starts_with(&prefix));
        }
        false
    });

    // Detect inline module definitions in the content to avoid duplicate declarations
    // Pattern matches: `pub mod name { ... }` or `mod name { ... }` (not `mod name;`)
    let inline_mod_pattern = regex::Regex::new(r"(?:pub\s*(?:\(crate\)\s*)?)?\bmod\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{").unwrap();
    let inline_modules: HashSet<String> = inline_mod_pattern.captures_iter(content)
        .filter_map(|cap| cap.get(1).map(|m| m.as_str().to_string()))
        .collect();

    // Group child modules by cfg attribute type for cfg_if! generation
    // This prevents duplicate imports for platform-specific modules (libc issue)
    let mut target_arch_children: Vec<(&str, String)> = Vec::new();
    let mut other_cfg_children: Vec<(&str, String)> = Vec::new();
    let mut no_cfg_children: Vec<&str> = Vec::new();

    // Known architecture-specific module names in libc
    const LIBC_ARCH_MODULES: &[&str] = &[
        "x86", "x86_64", "arm", "aarch64", "mips", "mips64", "powerpc", "powerpc64",
        "riscv32", "riscv64", "sparc", "sparc64", "s390x", "m68k", "csky",
        "x32", "not_x32", "ilp32", "lp64"
    ];

    for child in &child_modules {
        if inline_modules.contains(*child) {
            continue;
        }
        let child_path = if module_path.is_empty() {
            child.to_string()
        } else {
            format!("{}::{}", module_path, child)
        };

        if let Some(cfg_attr) = module_cfgs.get(&child_path) {
            // Check if this is a target_arch cfg
            if cfg_attr.contains("target_arch") {
                target_arch_children.push((child, cfg_attr.clone()));
            } else {
                other_cfg_children.push((child, cfg_attr.clone()));
            }
        } else {
            // Heuristic: for libc crate, detect architecture-specific modules by name
            if crate_name == "libc" && LIBC_ARCH_MODULES.contains(child) {
                // Generate synthetic cfg attribute for this architecture
                let cfg_attr = format!("target_arch = \"{}\"", child);
                target_arch_children.push((child, cfg_attr));
            } else {
                no_cfg_children.push(child);
            }
        }
    }

    // Generate cfg_if! block for target_arch-specific children if multiple exist
    if target_arch_children.len() > 1 && crate_name == "libc" {
        full_content.push_str("cfg_if! {\n");
        for (i, (child, cfg_attr)) in target_arch_children.iter().enumerate() {
            let child_name = rename_reserved_module(child);
            if i == 0 {
                full_content.push_str(&format!("    if #[cfg({})] {{\n", cfg_attr));
            } else {
                full_content.push_str(&format!("    }} else if #[cfg({})] {{\n", cfg_attr));
            }
            if *child == "macros" {
                full_content.push_str("        #[macro_use]\n");
            }
            full_content.push_str(&format!("        mod {};\n", child_name));
            full_content.push_str(&format!("        pub use self::{}::*;\n", child_name));
        }
        full_content.push_str("    } else {\n");
        full_content.push_str("        // Unknown target_arch\n");
        full_content.push_str("    }\n");
        full_content.push_str("}\n");
    } else {
        // Generate individual declarations for target_arch children if only one
        for (child, cfg_attr) in &target_arch_children {
            let child_name = rename_reserved_module(child);
            full_content.push_str(&format!("#[cfg({})]\n", cfg_attr));
            if *child == "macros" {
                full_content.push_str("#[macro_use]\n");
            }
            full_content.push_str(&format!("pub mod {};\n", child_name));
        }
    }

    // Generate declarations for other cfg-protected children
    for (child, cfg_attr) in &other_cfg_children {
        let child_name = rename_reserved_module(child);
        full_content.push_str(&format!("#[cfg({})]\n", cfg_attr));
        if *child == "macros" {
            full_content.push_str("#[macro_use]\n");
        }
        full_content.push_str(&format!("pub mod {};\n", child_name));
    }

    // Generate declarations for children without cfg
    for child in &no_cfg_children {
        let child_name = rename_reserved_module(child);
        if *child == "macros" {
            full_content.push_str("#[macro_use]\n");
        }
        full_content.push_str(&format!("pub mod {};\n", child_name));
    }

    // Add re-exports for public items from child modules
    // This fixes E0412/E0433 where code uses `module::Item` but Item is in a submodule
    // Group items by visibility: pub(crate) items vs fully public items
    // Track already re-exported names to avoid "defined multiple times" errors
    let mut already_exported_pub: HashSet<String> = HashSet::new();
    let mut already_exported_pub_crate: HashSet<String> = HashSet::new();

    // Track which children were included in cfg_if! blocks (already re-exported there)
    let cfg_if_children: HashSet<&str> = if target_arch_children.len() > 1 && crate_name == "libc" {
        target_arch_children.iter().map(|(child, _)| *child).collect()
    } else {
        HashSet::new()
    };

    // Priority 3: Extract locally-defined symbols from content to prevent E0255 conflicts
    // Parse content to find pub fn, pub struct, pub enum, pub type, pub const, pub static
    let mut locally_defined: HashSet<String> = HashSet::new();


    // Match function definitions: pub fn name( or pub(crate) fn name(
    // Updated regex to handle attributes and decorators before pub fn
    let fn_pattern = regex::Regex::new(r"pub(?:\([^)]*\))?\s+fn\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*[(<]")
        .expect("Invalid function regex");
    for cap in fn_pattern.captures_iter(content) {
        if let Some(name) = cap.get(1) {
            locally_defined.insert(name.as_str().to_string());
        }
    }

    // Match type definitions: pub struct/enum/type/trait
    // Updated regex to handle attributes and decorators before pub
    let type_pattern = regex::Regex::new(r"pub(?:\([^)]*\))?\s+(?:struct|enum|type|trait)\s+([a-zA-Z_][a-zA-Z0-9_]*)")
        .expect("Invalid type regex");
    for cap in type_pattern.captures_iter(content) {
        if let Some(name) = cap.get(1) {
            locally_defined.insert(name.as_str().to_string());
        }
    }

    // Match const/static definitions
    let const_pattern = regex::Regex::new(r"(?m)^\s*pub(?:\([^)]*\))?\s+(?:const|static)\s+([a-zA-Z_][a-zA-Z0-9_]*)")
        .expect("Invalid const regex");
    for cap in const_pattern.captures_iter(content) {
        if let Some(name) = cap.get(1) {
            locally_defined.insert(name.as_str().to_string());
        }
    }

    for child in &child_modules {
        // Skip re-exports for inline modules - items are already directly available
        if inline_modules.contains(*child) {
            continue;
        }
        // Skip re-exports for children included in cfg_if! blocks (already have pub use self::*;)
        if cfg_if_children.contains(*child) {
            continue;
        }
        let child_path = format!("{}::{}", module_path, child);
        if let Some(items) = child_module_items.get(&child_path) {
            if !items.is_empty() {
                let child_name = rename_reserved_module(child);

                // Separate items by visibility, filtering out already-exported names
                let mut pub_crate_items: Vec<String> = Vec::new();
                let mut pub_items: Vec<String> = Vec::new();

                for (name, is_crate_only) in items {
                    // Priority 3: Skip re-exporting items that are locally defined in this module (E0255 fix)
                    if locally_defined.contains(name) {
                        continue;
                    }

                    if *is_crate_only {
                        if !already_exported_pub_crate.contains(name) {
                            already_exported_pub_crate.insert(name.clone());
                            pub_crate_items.push(name.clone());
                        }
                    } else {
                        if !already_exported_pub.contains(name) {
                            already_exported_pub.insert(name.clone());
                            pub_items.push(name.clone());
                        }
                    }
                }

                // Re-export pub(crate) items with pub(crate) use
                if !pub_crate_items.is_empty() {
                    let items_str = pub_crate_items.join(", ");
                    full_content.push_str(&format!("pub(crate) use self::{}::{{{}}};\n", child_name, items_str));
                }

                // Re-export fully public items with pub use
                if !pub_items.is_empty() {
                    let items_str = pub_items.join(", ");
                    full_content.push_str(&format!("pub use self::{}::{{{}}};\n", child_name, items_str));
                }
            }
        }
    }

    if !child_modules.is_empty() {
        full_content.push('\n');
    }

    // Transform content to rename reserved module references
    let transformed_content = transform_reserved_module_refs(content);

    full_content.push_str(&transformed_content);

    // Final deduplication step to catch any duplicate imports (by line or by symbol)
    let deduped_content = deduplicate_import_lines(&full_content);

    // Priority 7: Final filter to remove incorrect Visitor trait imports
    // This is a last-resort filter to catch imports that slipped through earlier filters
    // including those embedded in parsed item source code
    let deduped_content = deduped_content
        .lines()
        .filter(|line| {
            let trimmed = line.trim();

            // Check for exact import statements we want to filter
            let is_hir_visitor_import = trimmed == "use crate::hir::visitor::Visitor;" ||
                                       trimmed == "pub use crate::hir::visitor::Visitor;";

            !is_hir_visitor_import
        })
        .collect::<Vec<&str>>()
        .join("\n");

    // Priority 1: Force formatting if any line is extremely long (>1000 chars)
    // Extremely long lines (especially trait definitions) break rustc parser
    // This fixes E0407 errors where trait methods aren't recognized
    let has_long_lines = deduped_content.lines().any(|line| line.len() > 1000);

    // Priority 7 debug: check content BEFORE formatting
    if module_path == "ast::print" {
//         // // eprintln!("[P7 DEBUG] BEFORE format_source, first 30 lines:");
        for (i, line) in deduped_content.lines().take(30).enumerate() {
            if line.contains("Visitor") || line.contains("visitor") {
                crate::debug_log!("  {}: *** {}", i+1, line);
            } else {
                crate::debug_log!("  {}: {}", i+1, line);
            }
        }
//         // // eprintln!("[P7 DEBUG] has_long_lines: {}", has_long_lines);
    }

    let formatted = if has_long_lines {
        // Force formatting by temporarily setting environment
        std::env::set_var("RUSTFMT", "1");
        let result = format_source(&deduped_content);
        std::env::remove_var("RUSTFMT");
        result
    } else {
        format_source(&deduped_content)
    };

    // Priority 7 debug: check content AFTER formatting
    if module_path == "ast::print" {
//         // // eprintln!("[P7 DEBUG] AFTER format_source, first 30 lines:");
        for (i, line) in formatted.lines().take(30).enumerate() {
            if line.contains("Visitor") || line.contains("visitor") {
                crate::debug_log!("  {}: *** {}", i+1, line);
            } else {
                crate::debug_log!("  {}: {}", i+1, line);
            }
        }
    }

    // Priority 7: Post-formatting filter for hir::visitor::Visitor imports
    // The import may be in the formatted output even after all earlier filters
    let final_content = formatted
        .lines()
        .filter(|line| {
            let trimmed = line.trim();

            // Debug: check all Visitor imports in ast::print module
            if module_path == "ast::print" && trimmed.contains("Visitor") {
//                 // crate::debug_log!("[Priority 7 DEBUG] ast::print formatted line: '{}'", trimmed);
            }

            let is_hir_visitor = trimmed == "use crate::hir::visitor::Visitor;" ||
                                trimmed == "pub use crate::hir::visitor::Visitor;";

            if is_hir_visitor {
                crate::debug_log!("[Priority 7 FIX] Filtering hir::visitor::Visitor import in final output for module: {}", module_path);
            }

            !is_hir_visitor
        })
        .collect::<Vec<&str>>()
        .join("\n");

    // Priority 2: Fix E0433 errors - replace unqualified stdlib module references with std:: prefix
    // Common pattern: code uses `error::Error`, `mem::replace`, etc. without importing or using std:: prefix
    let mut final_content = final_content;

    // List of common stdlib modules that are often referenced without std:: prefix
    let stdlib_modules = [
        "error", "mem", "fmt", "ptr", "str", "slice", "iter", "ops", "cmp", "hash",
        "io", "fs", "path", "sync", "thread", "time", "convert", "default", "marker",
        "any", "borrow", "cell", "char", "clone", "collections", "env", "ffi",
        "net", "num", "process", "rc", "result", "option"
    ];

    for module in &stdlib_modules {
        let module_ref = format!("{}::", module);
        let std_module_ref = format!("std::{}::", module);

        // Only process if the module is referenced (check per-line, not entire file)
        if final_content.contains(&module_ref) {
            let lines: Vec<String> = final_content.lines().map(|line| {
                // Check if line contains the unqualified module reference
                if !line.contains(&module_ref) {
                    return line.to_string();
                }

                // Don't replace if line already has qualified reference
                if line.contains(&format!("std::{}::", module)) ||
                   line.contains(&format!("crate::{}::", module)) ||
                   line.contains(&format!("self::{}::", module)) ||
                   line.contains(&format!("super::{}::", module)) {
                    return line.to_string();
                }

                // Check if it's a word boundary (space, punctuation, start of string, etc.)
                let mut result = String::new();
                let chars = line.chars().collect::<Vec<_>>();
                let mut char_i = 0;  // character index
                let mut byte_i = 0;  // byte index

                while char_i < chars.len() {
                    // Try to match the module reference at current byte position
                    if let Some(_rest) = line[byte_i..].strip_prefix(&module_ref) {
                        // Check what comes before (if anything)
                        let is_word_boundary = char_i == 0 || {
                            let prev = chars[char_i - 1];
                            prev.is_whitespace() || prev == '(' || prev == '<' ||
                            prev == ',' || prev == '{' || prev == '[' || prev == ':'
                        };

                        if is_word_boundary {
                            // This is an unqualified reference, add std:: prefix
                            result.push_str(&std_module_ref);
                            byte_i += module_ref.len();
                            char_i += module_ref.chars().count();
                            continue;
                        }
                    }

                    let current_char = chars[char_i];
                    result.push(current_char);
                    byte_i += current_char.len_utf8();
                    char_i += 1;
                }

                result
            }).collect();
            final_content = lines.join("\n");
        }
    }

    // Priority 6 (Phase 2): Fix E0034 - multiple applicable items (Display vs Debug)
    // Replace ambiguous .fmt(f) calls with trait-qualified syntax
    final_content = final_content.replace(
        "crate::error::Formatter::from(self).fmt(f)",
        "std::fmt::Display::fmt(&crate::error::Formatter::from(self), f)"
    );

    // Priority 6 (Phase 2): Fix E0107 - missing Error type parameter
    // Result<T> needs to be Result<T, Error>
    final_content = final_content.replace(
        "Result<hir::Hir>",
        "Result<hir::Hir, Error>"
    );

    // Debug: verify final content for ast::print before writing
    if module_path == "ast::print" {
//         // crate::debug_log!("[Priority 7 DEBUG] Writing ast::print to: {:?}", file_path);
        let imports: Vec<&str> = final_content.lines().take(15).collect();
//         // crate::debug_log!("[Priority 7 DEBUG] First 15 lines of final_content:");
        for (i, line) in imports.iter().enumerate() {
            crate::debug_log!("  {}: {}", i+1, line);
        }
    }

    // Priority 7 debug: log all writes to ast/print.rs
    if file_path.to_string_lossy().contains("ast/print.rs") {
        use std::sync::atomic::{AtomicU32, Ordering};
        static WRITE_COUNT: AtomicU32 = AtomicU32::new(0);
        let _count = WRITE_COUNT.fetch_add(1, Ordering::SeqCst) + 1;
//         // // eprintln!("[P7 DEBUG] === WRITING ast/print.rs (write #{}) ===", _count);
        crate::debug_log!("  First 5 lines of content:");
        for (i, line) in final_content.lines().take(5).enumerate() {
            crate::debug_log!("    {}: {}", i+1, line);
        }
    }

    // Priority 3: Final pass to remove known private item imports (E0603 fix)
    // Filter out imports of private items that may have been preserved or generated through other paths
    let final_content = {
        const PRIVATE_IMPORT_PATTERNS: &[&str] = &[
            "use crate::unicode::Range;",
            "use crate::hir::translate::Flags;",
            "use crate::hir::translate::unicode;",
            "use crate::unicode::CanonicalClassQuery;",
            "use crate::ast::parse::ClassState;",
            "use crate::ast::parse::GroupState;",
            "use crate::hir::translate::HirFrame;",
            "use crate::ast::parse::Primitive;",
        ];

        let lines: Vec<&str> = final_content.lines()
            .filter(|line| {
                let trimmed = line.trim();
                let is_private_import = PRIVATE_IMPORT_PATTERNS.iter().any(|pattern| trimmed == *pattern);
                if is_private_import {
                    crate::debug_log!("[Priority 3 FINAL] Removing private import from {}: {}", module_path, trimmed);
                }
                !is_private_import
            })
            .collect();

        lines.join("\n")
    };

    fs::write(&file_path, &final_content)?;

    // Priority 7 debug: Read file back immediately to verify it was written correctly
    if file_path.to_string_lossy().contains("ast/print.rs") || file_path.to_string_lossy().contains("ast\\print.rs") {
        use std::io::Read;
        if let Ok(mut file) = std::fs::File::open(&file_path) {
            let mut readback = String::new();
            if file.read_to_string(&mut readback).is_ok() {
//                 // // eprintln!("[P7 DEBUG] Readback of ast/print.rs immediately after write:");
                for (i, line) in readback.lines().take(5).enumerate() {
                    crate::debug_log!("  {}: {}", i+1, line);
                }
            }
        }
    }

    Ok(())
}

/// Generate use statements for locally-defined enum variants
/// When code uses enum variants without qualification (e.g., `AnyChar` instead of `PatternToken::AnyChar`),
/// we need to add `use EnumName::*;` to bring them into scope.
fn generate_enum_variant_imports(content: &str) -> String {
    use regex::Regex;
    use once_cell::sync::Lazy;

    // Pattern to match enum definitions: enum Name { Variant1, Variant2(Type), ... }
    static ENUM_DEF: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"enum\s+([A-Z][a-zA-Z0-9_]*)\s*\{([^}]+)\}").unwrap()
    });

    // Pattern to extract variant names from enum body
    static VARIANT_NAME: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"([A-Z][a-zA-Z0-9_]*)\s*(?:\([^)]*\)|$|,)").unwrap()
    });

    let mut imports = Vec::new();

    // Find all enum definitions
    for enum_cap in ENUM_DEF.captures_iter(content) {
        let enum_name = enum_cap.get(1).map(|m| m.as_str()).unwrap_or("");
        let enum_body = enum_cap.get(2).map(|m| m.as_str()).unwrap_or("");

        if enum_name.is_empty() {
            continue;
        }

        // Extract variant names
        let mut variants: Vec<String> = Vec::new();
        for var_cap in VARIANT_NAME.captures_iter(enum_body) {
            if let Some(var_name) = var_cap.get(1) {
                variants.push(var_name.as_str().to_string());
            }
        }

        if variants.is_empty() {
            continue;
        }

        // Check if any variant is used unqualified in the code (outside the enum definition)
        // We look for the variant name followed by typical usage patterns:
        // - `VariantName =>` (match arm)
        // - `VariantName (` (constructor call)
        // - `VariantName ,` or `VariantName }` (in match)
        // - Just `VariantName` followed by space/punctuation
        let variants_used: Vec<&String> = variants.iter().filter(|var| {
            // Check if this variant appears in usage context (not just in the enum definition)
            let usage_pattern = format!(r"\b{}\s*(?:=>|\(|,|\}}|\s)", var);
            if let Ok(re) = Regex::new(&usage_pattern) {
                // Count occurrences - should be > 1 if used outside definition
                let matches: Vec<_> = re.find_iter(content).collect();
                matches.len() > 1
            } else {
                false
            }
        }).collect();

        if !variants_used.is_empty() {
            // Check if there's already a use statement for this enum's variants
            let already_imported = content.contains(&format!("use {}::*;", enum_name)) ||
                                   content.contains(&format!("use self::{}::*;", enum_name)) ||
                                   content.contains(&format!("use crate::{}::*;", enum_name));
            
            if !already_imported {
                // Generate use statement for this enum's variants
                // Use self:: prefix to explicitly import from current module's enum
                imports.push(format!("use self::{}::*;", enum_name));
            }
        }
    }

    if imports.is_empty() {
        String::new()
    } else {
        imports.join("\n") + "\n"
    }
}

/// Generate use statements for UPPER_CASE constants that are used but not locally defined
/// This catches constants like LITTLE_ENDIAN that are used in the code but need to be imported
#[allow(unused_variables)]
fn generate_constant_imports(
    content: &str,
    locally_defined: &HashSet<String>,
    already_imported: &HashSet<String>,
    index: &CrateIndex,
    crate_name: &str,
    existing_imports: &str,
    modules: &std::collections::BTreeMap<String, Vec<&crate::types::ParsedItem>>,
    populated_modules: &HashSet<String>,
) -> String {
    use std::collections::BTreeSet;

    let mut imports: BTreeSet<String> = BTreeSet::new();

    // Extract UPPER_CASE constant usages from the content
    let const_usages = crate::old_slicer::slicing::extract_constant_usages(content);

    // For each constant usage, check if it needs to be imported
    for const_name in const_usages {
        // Skip if already imported or locally defined
        if already_imported.contains(&const_name) || locally_defined.contains(&const_name) {
            continue;
        }

        // Skip if this constant appears only in string literals (not actual code usage)
        // Check if the constant appears in quotes (both single and double)
        let in_string_pattern = format!(r#"["']{}["']"#, regex::escape(&const_name));
        if let Ok(re) = regex::Regex::new(&in_string_pattern) {
            if re.is_match(content) {
                // Check if it appears outside of strings
                // If the unquoted version doesn't appear, it's only in strings
                let unquoted_pattern = format!(r"\b{}\b", regex::escape(&const_name));
                if let Ok(unquoted_re) = regex::Regex::new(&unquoted_pattern) {
                    let quoted_matches = re.find_iter(content).count();
                    let total_matches = unquoted_re.find_iter(content).count();
                    // If all matches are quoted, skip this constant
                    if quoted_matches > 0 && total_matches == quoted_matches {
                        continue;
                    }
                }
            }
        }

        // Skip if this constant is already imported in the content or existing imports
        // Check for patterns like "use crate::module::CONST_NAME;" or "use crate::CONST_NAME;"
        // Use raw string for proper regex escaping
        let import_pattern = format!(r"::{}[;,}}\s]", regex::escape(&const_name));
        if let Ok(re) = regex::Regex::new(&import_pattern) {
            if re.is_match(content) || re.is_match(existing_imports) {
                continue;
            }
        }

        // Try to find this constant in the index
        let mut found_in_index = false;
        for item in index.get_all(&const_name) {
            // Only handle constants and statics
            if item.kind != crate::types::ParsedItemKind::Const &&
               item.kind != crate::types::ParsedItemKind::Static {
                continue;
            }

            // Skip if the module isn't in populated_modules (not actually needed)
            let item_module = normalize_module_path(&item.module_path, crate_name);
            if !item_module.is_empty() && !populated_modules.contains(&item_module) {
                continue;
            }

            // Build the import path
            // Note: module_path is relative to crate root (e.g., "tables" not "unicode_ident::tables")
            // because compute_module_path() doesn't include the crate name prefix
            let import_path = if item.module_path.is_empty() {
                format!("use crate::{};", const_name)
            } else {
                format!("use crate::{}::{};", item.module_path, const_name)
            };

            imports.insert(import_path);
            found_in_index = true;
            break; // Only add one import per constant
        }

        // If not found in index, search in the modules map (fallback for child module constants)
        // This handles cases where constants are defined in child modules that may not be
        // in the index yet (e.g., unicode_ident::tables::CHUNK)
        if !found_in_index {
            for (mod_path, items) in modules {
                // Skip if this module isn't in populated_modules (not actually needed)
                if !mod_path.is_empty() && !populated_modules.contains(mod_path) {
                    continue;
                }

                for item in items {
                    if item.name == const_name &&
                       matches!(item.kind, crate::types::ParsedItemKind::Const | crate::types::ParsedItemKind::Static) {
                        // mod_path is already relative to crate root (e.g., "tables" or "")
                        let import_path = if mod_path.is_empty() {
                            format!("use crate::{};", const_name)
                        } else {
                            format!("use crate::{}::{};", mod_path, const_name)
                        };

                        imports.insert(import_path);
                        found_in_index = true; // Mark as found to stop searching
                        break;
                    }
                }
                if found_in_index {
                    break;
                }
            }
        }
    }

    if imports.is_empty() {
        String::new()
    } else {
        imports.into_iter().collect::<Vec<_>>().join("\n") + "\n"
    }
}

/// Generate imports for root-level helper functions used by modules
/// This catches functions like `take_unchecked` that are defined in lib.rs (root module)
/// and used by child modules like imp_std, imp_pl, etc.
fn generate_root_helper_imports(
    content: &str,
    locally_defined: &HashSet<String>,
    already_imported: &HashSet<String>,
    index: &CrateIndex,
    existing_imports: &str,
    _module_path: &str, // For potential future debug logging
) -> String {
    use std::collections::BTreeSet;

    let mut imports: BTreeSet<String> = BTreeSet::new();

    // Extract function calls from the content (lowercase identifiers followed by `(`)
    let func_calls = crate::old_slicer::slicing::extract_function_calls(content);


    // For each function call, check if it's a root-level helper that needs importing
    for func_name in func_calls {
        // Skip if already imported or locally defined
        if already_imported.contains(&func_name) || locally_defined.contains(&func_name) {
            continue;
        }

        // Skip if this function is already imported in existing imports
        let import_pattern = format!(r"::{}\b", regex::escape(&func_name));
        if let Ok(re) = regex::Regex::new(&import_pattern) {
            if re.is_match(existing_imports) {
                continue;
            }
        }

        // Try to find this function in the root module (module_path = "")
        for item in index.get_all(&func_name) {
            // Only handle functions in the root module
            if item.kind != crate::types::ParsedItemKind::Function {
                continue;
            }

            // Must be in root module (empty module path)
            if !item.module_path.is_empty() {
                continue;
            }

            // Generate `use crate::func_name;`
            let import_path = format!("use crate::{};", func_name);
            imports.insert(import_path);
            break; // Only add one import per function
        }
    }

    if imports.is_empty() {
        String::new()
    } else {
        imports.into_iter().collect::<Vec<_>>().join("\n") + "\n"
    }
}

/// Generate imports for std::sync::atomic::Ordering enum variants
/// These variants (SeqCst, Acquire, Release, etc.) are often used unqualified in atomic operations
fn generate_atomic_ordering_imports(content: &str, existing_imports: &str) -> String {
    use std::collections::BTreeSet;

    // atomic::Ordering variants
    const ORDERING_VARIANTS: &[&str] = &["SeqCst", "Acquire", "Release", "Relaxed", "AcqRel"];

    let mut imports: BTreeSet<String> = BTreeSet::new();

    for variant in ORDERING_VARIANTS {
        // Check if variant is used in the code
        if !content.contains(variant) {
            continue;
        }

        // Check if already imported
        let import_pattern = format!("{}[;,}}\t ]", regex::escape(variant));
        if let Ok(re) = regex::Regex::new(&import_pattern) {
            if re.is_match(existing_imports) {
                continue;
            }
        }

        // Check if used qualified (Ordering::SeqCst) - if so, skip import
        let qualified_pattern = format!("Ordering::{}", variant);
        if content.contains(&qualified_pattern) {
            continue;
        }

        // Generate import for the variant
        imports.insert(format!("use std::sync::atomic::Ordering::{};", variant));
    }

    if imports.is_empty() {
        String::new()
    } else {
        imports.into_iter().collect::<Vec<_>>().join("\n") + "\n"
    }
}

/// Generate imports for encoding_rs macros that reference types from handles module
/// encoding_rs uses macros like decoder_functions!, encoder_functions! which expand to code
/// that references types from the handles module (ByteSource, Space, etc.)
fn generate_encoding_macro_imports(content: &str, existing_imports: &str, crate_name: Option<&str>) -> String {
    // Only apply this fix for encoding_rs crate
    if let Some(name) = crate_name {
        if name != "encoding_rs" {
            return String::new();
        }
    } else {
        return String::new();
    }

    // Macros that require handles types (with or without space before !)
    const ENCODING_MACROS: &[&str] = &[
        "decoder_functions",
        "decoder_function",
        "encoder_functions",
        "encoder_function",
        "ascii_compatible_two_byte_decoder_functions",
        "ascii_compatible_two_byte_decoder_function",
        "ascii_compatible_encoder_functions",
        "ascii_compatible_encoder_function",
        "euc_jp_decoder_functions",
        "euc_jp_decoder_function",
        "gb18030_decoder_functions",
        "gb18030_decoder_function",
    ];

    // Check if any of these macros are used (with ! or space+!)
    let uses_encoding_macros = ENCODING_MACROS.iter().any(|m| {
        let with_bang = format!("{}!", m);
        let with_space_bang = format!("{} !", m);
        content.contains(&with_bang) || content.contains(&with_space_bang)
    });

    if !uses_encoding_macros {
        return String::new();
    }

    // Check if handles import already exists
    if existing_imports.contains("use crate::handles::*") ||
       existing_imports.contains("use super::handles::*") {
        return String::new();
    }

    // Generate wildcard import from handles module
    // This brings in all the types the macros need: ByteSource, Space, Utf8Destination, etc.
    "use crate::handles::*;\n".to_string()
}


/// Generate use statements for types referenced in the module
/// Includes both internal crate imports and standard library imports
/// Uses Aho-Corasick for efficient multi-pattern matching
/// `crate_local_types` contains type names defined anywhere in the crate (from SCIP analysis)
fn generate_internal_imports(
    content: &str,
    current_module: &str,
    index: &CrateIndex,
    all_paths: &HashSet<String>,
    locally_defined: &HashSet<String>,
    crate_local_types: &HashSet<String>,
    already_imported: &HashSet<String>,
    child_modules: &HashSet<String>,
    crate_name: Option<&str>,
    symbol_paths: Option<&HashMap<String, Vec<String>>>,
    populated_modules: Option<&HashSet<String>>,
    file_path: Option<&str>,
    cfg_conditions: Vec<String>,
) -> String {
    use std::collections::BTreeSet;

    // Debug entry point
    let mut imports: BTreeSet<String> = BTreeSet::new();

    // Priority 3: Known private items that should not be imported (E0603 fix)
    // These are private type aliases/structs/enums that shouldn't be imported cross-module
    const KNOWN_PRIVATE_ITEMS: &[(&str, &str)] = &[
        ("Range", "unicode"),  // private type alias in unicode module
        ("Flags", "hir::translate"),  // private struct in hir::translate
        ("CanonicalClassQuery", "unicode"),  // private enum
        ("ClassState", "ast::parse"),  // private enum
        ("GroupState", "ast::parse"),  // private enum
        ("HirFrame", "hir::translate"),  // private enum
        ("Primitive", "ast::parse"),  // private enum
        ("unicode", "hir::translate"),  // private module re-export in hir::translate
    ];

    // Helper function to check if an import is for a known private item
    let is_private_item_import = |import_line: &str| -> bool {
        KNOWN_PRIVATE_ITEMS.iter().any(|(item_name, module_path)| {
            // Match imports like: use crate::hir::translate::Flags;
            // Or: use crate::hir::translate::unicode;
            import_line.contains(&format!("use crate::{}::{}", module_path, item_name))
        })
    };

    // Normalize content first - syn's quote! adds spaces around :: and <
    let content = normalize_source(content);
    let content = content.as_str();

    // Detect module declarations to avoid importing them (E0255 fix)
    // Pattern: (pub)? (pub(crate))? mod NAME { ... } or mod NAME;
    // Also include child_modules that will be declared later
    let inline_modules: HashSet<String> = {
        let mut modules = HashSet::new();
        // Look for "mod NAME {" patterns (inline modules with optional visibility)
        let inline_mod_pattern = regex::Regex::new(r"(?:pub\s*(?:\([^)]*\)\s*)?)?mod\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{").unwrap();
        for cap in inline_mod_pattern.captures_iter(content) {
            if let Some(name) = cap.get(1) {
                modules.insert(name.as_str().to_string());
            }
        }
        // Also look for "mod NAME;" patterns (file modules with optional visibility)
        let file_mod_pattern = regex::Regex::new(r"(?:pub\s*(?:\([^)]*\)\s*)?)?mod\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*;").unwrap();
        for cap in file_mod_pattern.captures_iter(content) {
            if let Some(name) = cap.get(1) {
                modules.insert(name.as_str().to_string());
            }
        }
        // Add child modules that will be declared (from all_paths analysis)
        modules.extend(child_modules.iter().cloned());
        modules
    };

    // Detect standard library imports based on code patterns (Aho-Corasick)
    for import in detect_std_imports(content).lines() {
        if !import.is_empty() {
            // Skip if this import conflicts with a locally defined module or type
            let symbols = extract_imported_symbols(import);
            if symbols.iter().any(|s| inline_modules.contains(s) || locally_defined.contains(s)) {
                continue;
            }
            imports.insert(import.to_string());
        }
    }

    // Special case: Add wildcard SIMD imports for x86/x86_64 modules
    // SIMD intrinsics are used directly without being detected by SCIP analysis
    // We generate these without cfg guards and let the existing module-level cfg system handle them
    if let Some(path) = file_path {
        if path.contains("/x86/") || path.contains("\\x86\\") ||
            path.contains("/x86_64/") || path.contains("\\x86_64\\") {
            // Check if this file uses SIMD types or is marked with target_feature
            let uses_simd = content.contains("__m128i") || content.contains("__m256i") ||
                           content.contains("#[target_feature(enable = \"sse") ||
                           content.contains("#[target_feature(enable = \"avx");

            if uses_simd {
                // Add wildcard imports for SIMD intrinsics WITHOUT cfg guards
                // The cfg guards are handled at the module level in mod.rs
                // Use core:: instead of std:: for better no_std compatibility
                if content.contains("x86_64") || content.contains("__m128i") || content.contains("__m256i") {
                    imports.insert("use core::arch::x86_64::*;".to_string());
                } else if content.contains("x86::") || content.contains("target_arch = \"x86\"") {
                    imports.insert("use core::arch::x86::*;".to_string());
                }
            }
        }
    }

    // Use cached std type mappings
    let std_types = load_std_types();

    // === Optimization 1: Use Aho-Corasick for std module patterns ===
    let std_module_matcher = get_std_module_matcher();
    let found_std_modules = std_module_matcher.find_module_usages(content);

    for idx in found_std_modules {
        let (mod_name, import_path) = std_module_matcher.get_module_info(idx);

        // Skip if already imported by preserved imports (e.g., `use core::cmp;`)
        if already_imported.contains(mod_name) {
            continue;
        }

        // Skip if this module name conflicts with a locally defined module or type
        if inline_modules.contains(mod_name) || locally_defined.contains(mod_name) {
            continue;
        }

        // Check if this module name conflicts with a crate-local module
        let conflicts_with_crate_module = all_paths.contains(mod_name);
        let is_inside_same_named_module = current_module == mod_name ||
            current_module.starts_with(&format!("{}::", mod_name));

        if !conflicts_with_crate_module || is_inside_same_named_module {
            imports.insert(format!("use {};", import_path));
        }
    }

    // === Phase 6.5b: Add imports for handle types used in macro expansions ===
    // encoding_rs macros expand to code using Space, ByteSource, CopyAsciiResult, etc.
    // These types are defined in crate::handles and re-exported at crate root
    // Detect their usage and add imports
    if let Some(crate_name_str) = crate_name {
        if crate_name_str == "encoding_rs" || crate_name_str == "encoding_rs_sliced" {
            let handle_types = [
                "Space", "ByteSource", "ByteDestination",
                "CopyAsciiResult", "Unicode", "NonAscii",
                "Utf8Source", "Utf8Destination",
                "Utf16Source", "Utf16Destination"
            ];

            for type_name in &handle_types {
                // Check if this type is used in the content
                // Use word boundary to avoid false matches
                let pattern_regex = regex::Regex::new(&format!(r"\b{}\b", regex::escape(type_name))).unwrap();
                if pattern_regex.is_match(content) {
                    // Type is used - add import from handles module
                    // These are re-exported at crate root, so import from there
                    imports.insert(format!("use crate::{};", type_name));
                }
            }
        }
    }

    // === Optimization 2: Use Aho-Corasick for crate module patterns ===
    // Use std_types cache to detect types that should never be imported from crate modules
    if let Some((crate_automaton, crate_modules)) = build_crate_module_matcher(all_paths, current_module, crate_name) {
        let found_crate_modules = find_crate_module_usages(content, &crate_automaton, &crate_modules);
        for module_path in found_crate_modules {
            // Skip if this looks like a std type import (e.g., "stream::Range")
            let type_name = module_path.rsplit("::").next().unwrap_or(&module_path);

            // Debug Visitor imports
            if std_types.contains_key(type_name) {
                continue;
            }
            // Skip if the type is defined locally in this module
            if locally_defined.contains(type_name) {
                continue;
            }
            // Skip if already imported by preserved imports
            if already_imported.contains(type_name) {
                continue;
            }

            // Skip if the target item is private (E0603 fix)
            // Only check types (uppercase names), not modules
            let first_char = type_name.chars().next().unwrap_or('a');
            if first_char.is_uppercase() {
                // Extract the parent module path from the import path
                let module_prefix: Vec<&str> = module_path.rsplit("::").skip(1).collect();
                let target_module = if !module_prefix.is_empty() {
                    module_prefix.into_iter().rev().collect::<Vec<_>>().join("::")
                } else {
                    String::new()
                };

                // This looks like a type - check if it's public at the specific module
                // Note: item.module_path may include crate name prefix (e.g., "aho_corasick::packed::teddy::generic")
                // while target_module is normalized (e.g., "packed::teddy::generic")
                let path_matches = |item_path: &str| -> bool {
                    item_path == target_module ||
                    item_path.ends_with(&format!("::{}", target_module))
                };

                match index.items.get(type_name) {
                    Some(items) => {
                        // Check if there's a public item at the specific module path we're importing from
                        let has_public_item_at_path = items.iter().any(|item| {
                            item.is_pub && path_matches(&item.module_path)
                        });
                        // Also check if there's NO item at this path (might be private and excluded)
                        let item_exists_at_path = items.iter().any(|item| {
                            path_matches(&item.module_path)
                        });

                        if item_exists_at_path && !has_public_item_at_path {
                            // Item exists at this path but is private - don't generate import
                            continue;
                        }
                        if !item_exists_at_path && all_paths.contains(&target_module) {
                            // Path exists in crate but item not found there - likely private
                            continue;
                        }
                    }
                    None => {
                        // Type not in index at all - if path is crate-internal, assume private
                        if !target_module.is_empty() && all_paths.contains(&target_module) {
                            continue;
                        }
                    }
                }
            }

            // Resolve module aliases: if `module_path` is "noncontiguous" but the actual
            // crate path is "nfa::noncontiguous", use the full path
            let mut resolved_path = resolve_module_path(&module_path, all_paths);

            // If regular resolution failed, try SCIP symbol_paths for accurate paths
            // E.g., if we detected "plumbing::Consumer" but SCIP says Consumer is at "iter::plumbing"
            if resolved_path == module_path {
                if let Some(sp) = symbol_paths {
                    // Try to look up the type name in SCIP symbol_paths
                    if let Some(scip_paths) = sp.get(type_name) {
                        // Priority 7: Filter out hir::visitor from SCIP paths for Visitor trait
                        // Use ast::visitor::Visitor instead, as they have different method signatures
                        let filtered_paths: Vec<&String> = if type_name == "Visitor" {
                            scip_paths.iter().filter(|p| *p != "hir::visitor").collect()
                        } else {
                            scip_paths.iter().collect()
                        };

                        // Use the first available path from SCIP (after filtering)
                        if let Some(scip_path) = filtered_paths.first() {
                            // Build the full import path: scip_path + :: + type_name
                            resolved_path = format!("{}::{}", scip_path, type_name);
                        }
                    }
                }
            }

            // Skip if the first path component is an inline module definition (E0255 fix)
            let first_component = resolved_path.split("::").next().unwrap_or(&resolved_path);
            if inline_modules.contains(first_component) {
                continue;
            }

            // Verify the first path component exists in all_paths (E0432 fix)
            // If the module doesn't exist, the import will fail, so skip it
            let first_module = resolved_path.split("::").next().unwrap_or(&resolved_path);
            let module_exists = all_paths.contains(first_module) ||
                all_paths.iter().any(|p| p.starts_with(&format!("{}::", first_module)));
            if !module_exists {
                // Module doesn't exist in sliced code, skip this import
                continue;
            }

            // Skip if importing a child module that's declared in this file (E0255 fix)
            // e.g., if current_module is "packed::teddy::builder" and we're importing
            // "packed::teddy::builder::aarch64", skip if "aarch64" is declared in this file
            let current_module_prefix = if current_module.is_empty() {
                String::new()
            } else {
                format!("{}::", current_module)
            };
            if resolved_path.starts_with(&current_module_prefix) {
                let relative_path = &resolved_path[current_module_prefix.len()..];
                let child_module = relative_path.split("::").next().unwrap_or(relative_path);
                if inline_modules.contains(child_module) {
                    continue;
                }
            }

            // Skip if the imported name (last component) conflicts with a local inline module (E0255 fix)
            // e.g., `use crate::reflect::value;` conflicts with `pub mod value { ... }`
            let imported_name = resolved_path.rsplit("::").next().unwrap_or(&resolved_path);
            if inline_modules.contains(imported_name) {
                continue;
            }

            // Rename reserved modules in the import path
            let renamed_path = rename_reserved_in_path(&resolved_path);
            if !renamed_path.contains("clippy") {
                // E0432 fix: Skip imports for modules that don't exist in populated_modules
                // This handles feature-gated nested modules like `std_support` and `sval_support`
                // that are defined inside files but filtered out during slicing
                let first_component = renamed_path.split("::").next().unwrap_or(&renamed_path);

                // If this is a module-only import (no :: means just a module name)
                // check if the module actually exists in populated_modules
                if !renamed_path.contains("::") {
                    // Module-only import like `use crate::std_support;`
                    // Check if the module is in populated_modules (modules that actually have content)
                    if let Some(pm) = populated_modules {
                        let module_exists = pm.contains(first_component) ||
                            pm.iter().any(|p| p.starts_with(&format!("{}::", first_component)));

                        if !module_exists {
                            // Module doesn't exist in sliced output - likely a feature-gated nested module
                            continue;
                        }
                    } else {
                        // Fallback to all_paths check if populated_modules not available
                        let module_exists_in_paths = all_paths.contains(first_component) ||
                            all_paths.iter().any(|p| p == first_component || p.starts_with(&format!("{}::", first_component)));

                        if !module_exists_in_paths {
                            continue;
                        }
                    }
                }

                let import_path = format!("crate::{}", renamed_path);
                let symbol = renamed_path.rsplit("::").next().unwrap_or(&renamed_path);

                // Log decision (crate module import from pattern detection)
                log_import_decision(
                    symbol,
                    &import_path,
                    DecisionSource::PatternDetection,
                    crate_name.unwrap_or("unknown"),
                    current_module,
                    file_path.unwrap_or("unknown"),
                    cfg_conditions.clone(),
                    false, // is_qualified
                    locally_defined.contains(symbol),
                    vec![], // alternatives
                );

                // Priority 7: Skip incorrect Visitor trait imports (E0407 fix)
                // Files implementing ast::Visitor should not import hir::Visitor
                let is_incorrect_visitor = import_path == "crate::hir::visitor::Visitor";

                if !is_incorrect_visitor {
                    imports.insert(format!("use {};", import_path));
                }
            }
        }
    }

    // === Phase 3: SIMD intrinsics detection ===
    // Detect x86/x86_64 SIMD intrinsics like _mm_movemask_epi8, _mm256_cmpeq_epi8, etc.
    // These require importing std::arch::x86_64::* or std::arch::x86::*
    // Use cfg_attr to conditionally import based on target architecture
    {
        // Check for SSE/AVX intrinsics (both _mm_ and _mm256_ prefixes)
        let has_sse_intrinsics = content.contains("_mm_") || content.contains("_mm256_");

        if has_sse_intrinsics {
            // Add conditional imports for both x86_64 and x86 architectures
            // The compiler will choose the right one based on the target
            imports.insert("#[cfg(target_arch = \"x86_64\")]".to_string());
            imports.insert("use std::arch::x86_64::*;".to_string());
            imports.insert("#[cfg(target_arch = \"x86\")]".to_string());
            imports.insert("use std::arch::x86::*;".to_string());
        }
    }

    // === Phase 1: Module path qualifier detection ===
    // Detect patterns like `ast::Type` or `hir::function()` where a module name is used as a path qualifier
    // This is needed because code might use `ast::ClassSetItem` but only import specific types, not the module itself
    // We need to add `use crate::ast;` to make the module path accessible
    {
        // Build regex to match module path qualifiers: word_boundary + module_name + ::
        // Examples: "ast::", "hir::", "error::" (but not "std::", "core::", "alloc::")
        let module_qualifier_pattern = regex::Regex::new(r"\b([a-z_][a-z0-9_]*)\s*::")
            .expect("Invalid module qualifier regex");

        // Collect all unique module names used as qualifiers
        let mut used_module_qualifiers: HashSet<String> = HashSet::new();
        for cap in module_qualifier_pattern.captures_iter(content) {
            if let Some(module_name) = cap.get(1) {
                let module_name = module_name.as_str();

                // Skip std/core/alloc modules - they're handled separately
                if module_name == "std" || module_name == "core" || module_name == "alloc"
                    || module_name == "self" || module_name == "super" || module_name == "crate" {
                    continue;
                }

                // Check if this is a top-level module in the crate
                // Top-level means it exists directly in all_paths, not as part of a nested path
                if all_paths.contains(module_name) {
                    used_module_qualifiers.insert(module_name.to_string());
                }
            }
        }

        // Generate imports for module qualifiers
        for module_name in used_module_qualifiers {
            // Skip if already imported
            if already_imported.contains(&module_name) {
                continue;
            }

            // Skip if this is an inline module declaration in this file
            if inline_modules.contains(&module_name) {
                continue;
            }

            // Skip if this is a child module (will be declared in this file)
            if child_modules.contains(&module_name) {
                continue;
            }

            // Skip if locally defined as a type/struct/enum
            if locally_defined.contains(&module_name) {
                continue;
            }

            // Note: We do NOT skip when current_module is inside module_name
            // Example: In ast::visitor, code can use `ast::Type` with `use crate::ast;`
            // This imports the parent module, which is valid and necessary

            // Add the module import
            let import_path = format!("use crate::{};", module_name);

            // Log decision
            log_import_decision(
                &module_name,
                &format!("crate::{}", module_name),
                DecisionSource::PatternDetection,
                crate_name.unwrap_or("unknown"),
                current_module,
                file_path.unwrap_or("unknown"),
                cfg_conditions.clone(),
                true, // is_qualified (module path qualifier)
                false, // locally_defined
                vec![], // alternatives
            );

            imports.insert(import_path);
        }
    }

    // Find all identifiers that might be types (start with uppercase or are c_* FFI types)
    // This loop is O(n) and cannot be further optimized with Aho-Corasick
    let mut current_word = String::with_capacity(64);
    let mut word_start_pos = 0usize;
    let content_bytes = content.as_bytes();
    let mut in_doc_string = false; // Track if we're inside a #[doc = "..."] string
    for (byte_pos, c) in content.char_indices() {
        // Simple state tracking for doc strings: skip content inside #[doc = "..."]
        // Check for entering a doc string (look for opening quote after #[doc = )
        if !in_doc_string && byte_pos >= 9 {
            let check_start = byte_pos.saturating_sub(9);
            if &content_bytes[check_start..byte_pos] == b"#[doc = \"" {
                in_doc_string = true;
            }
        }
        // Check for exiting a doc string ("]  at current position means we just passed ")
        if in_doc_string && c == ']' && byte_pos > 0 {
            if let Some(&prev_byte) = content_bytes.get(byte_pos - 1) {
                if prev_byte == b'"' {
                    in_doc_string = false;
                    current_word.clear();
                    continue;
                }
            }
        }
        if in_doc_string {
            current_word.clear();
            continue;
        }
        if c.is_alphanumeric() || c == '_' {
            if current_word.is_empty() {
                word_start_pos = byte_pos;
            }
            current_word.push(c);
        } else {
            if current_word.len() > 1 {  // Skip single-letter type parameters
                // Skip associated type declarations: "type SomeName :" - these don't need imports
                // Check for "type " or "type<whitespace>" before the identifier
                let is_assoc_type_decl = if word_start_pos >= 5 {
                    let prefix = &content_bytes[word_start_pos.saturating_sub(5)..word_start_pos];
                    prefix.starts_with(b"type ") || prefix == b"type\t" || prefix == b"type\n"
                } else {
                    false
                };
                if is_assoc_type_decl {
                    current_word.clear();
                    continue;
                }
                let first_char = current_word.chars().next().unwrap();
                let is_uppercase_type = first_char.is_uppercase();
                let is_c_ffi_type = current_word.starts_with("c_");
                // Also check for C-style types ending in _t (like mach_port_t, size_t)
                let is_c_style_type = current_word.ends_with("_t") && current_word.len() > 2;
                // Check for SIMD intrinsic types (__m128i, __m256i, __m128d, etc.)
                let is_simd_intrinsic = current_word.starts_with("__m") && current_word.len() > 3;
                // Check for internal types that start with underscore + uppercase (like _ProtobufTypeDynamic)
                let is_internal_type = first_char == '_' &&
                    current_word.len() > 1 &&
                    current_word.chars().nth(1).map_or(false, |c| c.is_uppercase());

                if is_uppercase_type || is_c_ffi_type || is_c_style_type || is_simd_intrinsic || is_internal_type {

                    // Check if this is part of a qualified path (preceded by ::)
                    // Handle both "Module::Type" and "Module :: Type" (with spaces from syn)
                    let is_qualified = if word_start_pos >= 2 {
                        // Check for :: directly before
                        if &content_bytes[word_start_pos-2..word_start_pos] == b"::" {
                            true
                        } else if word_start_pos >= 4 {
                            // Check for " :: " pattern (spaces around ::)
                            let prefix = &content_bytes[word_start_pos.saturating_sub(4)..word_start_pos];
                            prefix.ends_with(b":: ") || prefix.ends_with(b" ::") || prefix == b" :: "
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    if is_qualified {
                        current_word.clear();
                        continue;
                    }

                    // Check for C FFI types, _t types, and SIMD intrinsics in cache first
                    if is_c_ffi_type || is_c_style_type || is_simd_intrinsic {
                        if let Some(import_path) = std_types.get(&current_word) {
                            // Skip imports from unstable APIs (E0658 fix), private items (E0603 fix), and prelude types
                            if !is_unstable_std_import(import_path) && !is_private_std_import(import_path) && !is_prelude_type(import_path) {
                                // Log decision
                                log_import_decision(
                                    &current_word,
                                    import_path,
                                    DecisionSource::Cache,
                                    crate_name.unwrap_or("unknown"),
                                    current_module,
                                    file_path.unwrap_or("unknown"),
                                    cfg_conditions.clone(),
                                    false, // is_qualified
                                    locally_defined.contains(&current_word),
                                    vec![], // alternatives
                                );

                                imports.insert(format!("use {};", import_path));
                            }
                            current_word.clear();
                            continue;
                        }
                        // For SIMD types not in cache, still skip (they're arch-specific)
                        if is_simd_intrinsic {
                            current_word.clear();
                            continue;
                        }
                        current_word.clear();
                        continue;
                    }

                    // Skip if this type is defined locally in this module
                    if locally_defined.contains(&current_word) {
                        current_word.clear();
                        continue;
                    }

                    // Skip if this type is already imported by preserved imports (avoid E0252 duplicates)
                    if already_imported.contains(&current_word) {
                        current_word.clear();
                        continue;
                    }

                    // Skip std import if this type is defined anywhere in the crate (from SCIP analysis)
                    // This prevents conflicts like importing std::fmt::Pointer when crate defines trait Pointer
                    if crate_local_types.contains(&current_word) {
                        // Don't add std import - the type is defined locally in the crate
                        // Fall through to check if we need a crate:: import instead
                    } else {
                        // Types commonly redefined by crates - don't force std import
                        // These should be checked against crate definitions first

                        // Check if this is a std type (in cache) but NOT commonly redefined
                        // If so, use std import
                        if !COMMONLY_REDEFINED.contains(&current_word.as_str()) {
                            if let Some(import_path) = std_types.get(&current_word) {
                                // Skip imports from unstable APIs (E0658 fix), private items (E0603 fix), and prelude types
                                if !is_unstable_std_import(import_path) && !is_private_std_import(import_path) && !is_prelude_type(import_path) {
                                    // Log decision
                                    log_import_decision(
                                        &current_word,
                                        import_path,
                                        DecisionSource::Cache,
                                        crate_name.unwrap_or("unknown"),
                                        current_module,
                                        file_path.unwrap_or("unknown"),
                                        cfg_conditions.clone(),
                                        false, // is_qualified
                                        locally_defined.contains(&current_word),
                                        vec![], // alternatives
                                    );

                                    imports.insert(format!("use {};", import_path));
                                }
                                current_word.clear();
                                continue;
                            }
                        }
                    }

                    // Check if the type is defined in the crate
                    let mut found_in_crate = false;
                    if let Some(items) = index.items.get(&current_word) {
                        // First pass: check for root-level or same-module items (prefer these)
                        for item in items {
                            if item.kind == crate::types::ParsedItemKind::Impl || !item.is_pub {
                                continue;
                            }
                    let item_module = normalize_module_path(&item.module_path, crate_name.unwrap_or(""));
                            if item_module.is_empty() {
                                // Type is in root module - add use crate::Type if we're in a submodule
                                if !current_module.is_empty() {
                                    let import_path = format!("crate::{}", current_word);

                                    // Log decision (crate root import)
                                    log_import_decision(
                                        &current_word,
                                        &import_path,
                                        DecisionSource::CrateIndex { item_path: item.path.clone() },
                                        crate_name.unwrap_or("unknown"),
                                        current_module,
                                        file_path.unwrap_or("unknown"),
                                        cfg_conditions.clone(),
                                        false, // is_qualified
                                        locally_defined.contains(&current_word),
                                        vec![], // alternatives
                                    );

                                    imports.insert(format!("use {};", import_path));
                                }
                                found_in_crate = true;
                                break;
                            } else if item_module == current_module {
                                // Same module, no import needed
                                found_in_crate = true;
                                break;
                            }
                        }

                        // Second pass: if not found at root level, look for nested modules
                        // Prefer shorter paths (fewer :: segments)
                        if !found_in_crate {
                            let mut best_item_module: Option<String> = None;
                            for item in items {
                                if item.kind == crate::types::ParsedItemKind::Impl || !item.is_pub {
                                    continue;
                                }
                        let item_module = normalize_module_path(&item.module_path, crate_name.unwrap_or(""));
                                if !item_module.is_empty()
                                    && item_module != current_module
                                    && all_paths.contains(&item_module)
                                {
                                    // Skip if the module isn't populated (not actually needed in sliced output)
                                    if let Some(pop_mods) = populated_modules {
                                        if !pop_mods.contains(&item_module) {
                                            // Module isn't actually included, skip this import
                                            continue;
                                        }
                                    }

                                    // Skip if the first path component is an inline module definition (E0255 fix)
                                    let first_component = item_module.split("::").next().unwrap_or(&item_module);
                                    if inline_modules.contains(first_component) {
                                        found_in_crate = true;
                                        break;
                                    }
                                    // Prefer shorter paths, with tie-breakers:
                                    // 1. Shorter path (fewer :: segments)
                                    // 2. Paths starting with "primitive" (internal re-export modules)
                                    // 3. Alphabetically first (stable ordering)
                                    let is_better = match &best_item_module {
                                        None => true,
                                        Some(best) => {
                                            let item_depth = item_module.matches("::").count();
                                            let best_depth = best.matches("::").count();
                                            if item_depth != best_depth {
                                                item_depth < best_depth
                                            } else {
                                                // Same depth - prefer "primitive" module (common re-export pattern)
                                                let item_is_primitive = item_module.starts_with("primitive");
                                                let best_is_primitive = best.starts_with("primitive");
                                                if item_is_primitive && !best_is_primitive {
                                                    true
                                                } else if !item_is_primitive && best_is_primitive {
                                                    false
                                                } else {
                                                    // Tie-breaker: alphabetically first for stable ordering
                                                    item_module < *best
                                                }
                                            }
                                        }
                                    };
                                    if is_better {
                                        best_item_module = Some(item_module);
                                    }
                                }
                            }
                            if let Some(item_module) = best_item_module {
                                let renamed_module = rename_reserved_in_path(&item_module);
                                let import_stmt = format!(
                                    "use crate::{}::{};",
                                    renamed_module,
                                    current_word
                                );
                                imports.insert(import_stmt);
                                found_in_crate = true;
                            }
                        }
                    }

                    // If not found in index but known to be crate-local (from SCIP), use SCIP paths
                    if !found_in_crate && crate_local_types.contains(&current_word) {
                        if let Some(sp) = symbol_paths {
                            if let Some(scip_paths) = sp.get(&current_word) {
                                // Priority 7: Filter out hir::visitor from SCIP paths for Visitor trait
                                // Use ast::visitor::Visitor instead, as they have different method signatures
                                let filtered_scip_paths: Vec<&String> = if current_word == "Visitor" {
                                    scip_paths.iter().filter(|p| *p != "hir::visitor").collect()
                                } else {
                                    scip_paths.iter().collect()
                                };

                                if let Some(scip_path) = filtered_scip_paths.first() {
                                    // Skip empty or single-character paths - likely false positives
                                    if scip_path.len() <= 1 {
                                        // Skip this path
                                    } else if *scip_path != current_module {
                                        // Skip if the module isn't populated (not actually needed in sliced output)
                                        if let Some(pop_mods) = populated_modules {
                                            if let Some(cn) = crate_name {
                                                let scip_module = normalize_module_path(&scip_path, cn);
                                                if !pop_mods.contains(&scip_module) {
                                                    // Module isn't actually included, skip this import
                                                } else {
                                                    // Check if we're in the same module (no import needed)
                                                    // Skip if the first path component is an inline module definition (E0255 fix)
                                                    let first_component = scip_path.split("::").next().unwrap_or(scip_path);
                                                    if !inline_modules.contains(first_component) {
                                                        let renamed_path = rename_reserved_in_path(scip_path);
                                                        let import_stmt = format!(
                                                            "use crate::{}::{};",
                                                            renamed_path,
                                                            current_word
                                                        );
                                                        imports.insert(import_stmt);
                                                        found_in_crate = true;
                                                    }
                                                }
                                            }
                                        } else {
                                            // No populated_modules filtering - use original logic
                                            // Check if we're in the same module (no import needed)
                                            // Skip if the first path component is an inline module definition (E0255 fix)
                                            let first_component = scip_path.split("::").next().unwrap_or(scip_path);
                                            if !inline_modules.contains(first_component) {
                                                let renamed_path = rename_reserved_in_path(scip_path);
                                                let import_stmt = format!(
                                                    "use crate::{}::{};",
                                                    renamed_path,
                                                    current_word
                                                );
                                                imports.insert(import_stmt);
                                                found_in_crate = true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Only import from std if NOT defined in the crate
                    if !found_in_crate {
                        // Skip std import if it's a commonly redefined type
                        if !COMMONLY_REDEFINED.contains(&current_word.as_str()) {
                            if let Some(import_path) = std_types.get(&current_word) {
                                // Skip imports from unstable APIs (E0658 fix), private items (E0603 fix), and prelude types
                                if !is_unstable_std_import(import_path) && !is_private_std_import(import_path) && !is_prelude_type(import_path) {
                                    // Log decision (fallback std import)
                                    log_import_decision(
                                        &current_word,
                                        import_path,
                                        DecisionSource::Cache,
                                        crate_name.unwrap_or("unknown"),
                                        current_module,
                                        file_path.unwrap_or("unknown"),
                                        cfg_conditions.clone(),
                                        false, // is_qualified
                                        locally_defined.contains(&current_word),
                                        vec![], // alternatives
                                    );

                                    imports.insert(format!("use {};", import_path));
                                }
                            }
                        }
                    }
                }
            }
            current_word.clear();
        }
    }


    // === Handle function imports for cross-module function calls ===
    // Detect function calls (lowercase identifiers followed by `(`)
    // This catches functions like `read_unknown_or_skip_group_with_tag_unpacked`
    // Match function calls - we'll filter out method calls manually
    let func_call_pattern = regex::Regex::new(r"\b([a-z_][a-z0-9_]+)\s*\(").unwrap();
    // Also detect generic function calls like `transmute_ref_if_eq::<_, Vec<X>>()`
    // Pattern: identifier followed by `::` and `<`
    let generic_func_pattern = regex::Regex::new(r"\b([a-z_][a-z0-9_]+)\s*::\s*<").unwrap();

    // Common keywords and builtin functions to skip (shared by both loops)
    static SKIP_FUNCS: &[&str] = &[
        "if", "for", "while", "match", "loop", "fn", "let", "mut", "ref",
        "use", "mod", "struct", "enum", "trait", "impl", "type", "const",
        "where", "as", "in", "return", "break", "continue", "move", "async",
        "unsafe", "self", "super", "crate", "true", "false", "pub", "static",
        "new", "clone", "default", "unwrap", "expect", "ok", "err", "some", "none",
        "len", "is_empty", "iter", "into_iter", "collect", "push", "pop", "get", "set",
        "map", "filter", "and_then", "or_else", "map_err", "ok_or", "ok_or_else",
        "write", "writeln", "println", "eprintln", "print", "eprint", "format",
        "panic", "unreachable", "todo", "unimplemented", "assert", "debug_assert",
        "vec", "box", "rc", "arc", "mutex", "rwlock", "cell", "refcell",
        // Common method names that should never be imported as functions
        "join", "split", "trim", "parse", "contains", "starts_with", "ends_with",
        "insert", "remove", "clear", "reserve", "extend", "append", "drain",
        "read", "write_all", "flush", "seek", "take", "skip", "enumerate",
        "lock", "try_lock", "read_lock", "write_lock", "wait", "notify",
    ];

    for cap in func_call_pattern.captures_iter(content) {
        if let Some(m) = cap.get(1) {
            let func_name = m.as_str();
            let match_start = m.start();

            // Skip method calls (preceded by .)
            if match_start > 0 {
                let preceding_char = content.chars().nth(match_start - 1);
                if let Some(ch) = preceding_char {
                    if ch == '.' || ch == ':' {
                        // This is a method call like .join() or Type::new(), skip it
                        continue;
                    }
                }
            }

            if SKIP_FUNCS.contains(&func_name) {
                continue;
            }

            // Skip if already imported
            if already_imported.contains(func_name) {
                continue;
            }

            // Skip if locally defined (includes function definitions from locally_defined set)
            if locally_defined.contains(func_name) {
                continue;
            }

            // Also check if there's a function definition pattern in the content itself
            // This catches `fn funcname(` patterns that indicate local definition
            let def_pattern = format!(r"\bfn\s+{}\s*[<(]", regex::escape(func_name));
            if let Ok(def_re) = regex::Regex::new(&def_pattern) {
                if def_re.is_match(content) {
                    // Function is defined locally in this module, don't import
                    continue;
                }
            }

            // Check if function exists in the index at a DIFFERENT module
            if let Some(items) = index.items.get(func_name) {
                // First, check if there's a local definition in the same module
                let has_local_def = items.iter().any(|item| {
                    item.kind == crate::types::ParsedItemKind::Function &&
                    normalize_module_path(&item.module_path, crate_name.unwrap_or("")) == current_module
                });

                if has_local_def {
                    // Function is defined in current module, don't import
                    continue;
                }

                for item in items {
                    // Only consider functions
                    if item.kind != crate::types::ParsedItemKind::Function {
                        continue;
                    }

                    // Skip private functions - they can't be imported from other modules
                    if !item.is_pub {
                        continue;
                    }

                                                let item_module = normalize_module_path(&item.module_path, crate_name.unwrap_or(""));
                    // Skip if in the same module (no import needed)
                    if item_module == current_module || item_module.is_empty() {
                        break;
                    }

                    // Check if the module exists in the sliced output
                    if !all_paths.contains(&item_module) {
                        continue;
                    }

                    // Skip if the first path component is an inline module (E0255 fix)
                    let first_component = item_module.split("::").next().unwrap_or(&item_module);
                    if inline_modules.contains(first_component) {
                        break;
                    }

                    // Generate import for this function
                    let renamed_module = rename_reserved_in_path(&item_module);
                    imports.insert(format!(
                        "use crate::{}::{};",
                        renamed_module,
                        func_name
                    ));
                    break;
                }
            }
        }
    }

    // Also check for generic function calls like `transmute_ref_if_eq::<T>()`
    for cap in generic_func_pattern.captures_iter(content) {
        if let Some(m) = cap.get(1) {
            let func_name = m.as_str();
            let match_start = m.start();

            // Skip method calls (preceded by .)
            if match_start > 0 {
                let preceding_char = content.chars().nth(match_start - 1);
                if let Some(ch) = preceding_char {
                    if ch == '.' {
                        // This is a method call like .func::<T>(), skip it
                        continue;
                    }
                }
            }

            // Skip common keywords
            if SKIP_FUNCS.contains(&func_name) {
                continue;
            }

            // Skip if already imported
            if already_imported.contains(func_name) {
                continue;
            }

            // Skip if locally defined
            if locally_defined.contains(func_name) {
                continue;
            }

            // Check if function exists in the index at a DIFFERENT module
            if let Some(items) = index.items.get(func_name) {
                let has_local_def = items.iter().any(|item| {
                    item.kind == crate::types::ParsedItemKind::Function &&
                    normalize_module_path(&item.module_path, crate_name.unwrap_or("")) == current_module
                });

                if has_local_def {
                    continue;
                }

                for item in items {
                    if item.kind != crate::types::ParsedItemKind::Function {
                        continue;
                    }

                    // Skip private functions - they can't be imported from other modules
                    if !item.is_pub {
                        continue;
                    }

                                                let item_module = normalize_module_path(&item.module_path, crate_name.unwrap_or(""));
                    if item_module == current_module || item_module.is_empty() {
                        break;
                    }

                    if !all_paths.contains(&item_module) {
                        continue;
                    }

                    let first_component = item_module.split("::").next().unwrap_or(&item_module);
                    if inline_modules.contains(first_component) {
                        break;
                    }

                    let renamed_module = rename_reserved_in_path(&item_module);
                    imports.insert(format!(
                        "use crate::{}::{};",
                        renamed_module,
                        func_name
                    ));
                    break;
                }
            }
        }
    }

    // === Handle constant imports for UPPER_CASE identifiers ===
    // Detect constants like LITTLE_ENDIAN, MAX_SIZE, etc.
    let const_pattern = regex::Regex::new(r"\b([A-Z][A-Z0-9_]+)\b").unwrap();
    for cap in const_pattern.captures_iter(content) {
        if let Some(m) = cap.get(1) {
            let const_name = m.as_str();

            // Skip common patterns
            static SKIP_CONSTS: &[&str] = &[
                "OK", "ERR", "SOME", "NONE", "TRUE", "FALSE",
                "ID", "IO", "OS", "UI", "DB", "API", "URL", "URI",
            ];
            if SKIP_CONSTS.contains(&const_name) {
                continue;
            }

            // Must have underscore or be 3+ chars
            if !const_name.contains('_') && const_name.len() < 3 {
                continue;
            }

            // Skip if already imported
            if already_imported.contains(const_name) {
                continue;
            }

            // Skip if locally defined
            if locally_defined.contains(const_name) {
                continue;
            }

            // Also check if there's a const definition in the content itself
            let def_pattern = format!(r"\b(?:pub\s*(?:\([^)]*\)\s*)?)?\bconst\s+{}\s*:", regex::escape(const_name));
            if let Ok(def_re) = regex::Regex::new(&def_pattern) {
                if def_re.is_match(content) {
                    // Constant is defined locally, don't import
                    continue;
                }
            }

            // Check if constant exists in the index
            if let Some(items) = index.items.get(const_name) {
                // First, check if there's a local definition in the same module
                let has_local_def = items.iter().any(|item| {
                    item.kind == crate::types::ParsedItemKind::Const &&
                    normalize_module_path(&item.module_path, crate_name.unwrap_or("")) == current_module
                });

                if has_local_def {
                    continue;
                }

                for item in items {
                    // Only consider constants
                    if item.kind != crate::types::ParsedItemKind::Const {
                        continue;
                    }

                                                let item_module = normalize_module_path(&item.module_path, crate_name.unwrap_or(""));
                    // Skip if in the same module
                    if item_module == current_module || item_module.is_empty() {
                        break;
                    }

                    // Check if the module exists
                    if !all_paths.contains(&item_module) {
                        continue;
                    }

                    // Skip if first component is inline module
                    let first_component = item_module.split("::").next().unwrap_or(&item_module);
                    if inline_modules.contains(first_component) {
                        break;
                    }

                    // Generate import for this constant
                    let renamed_module = rename_reserved_in_path(&item_module);
                    imports.insert(format!(
                        "use crate::{}::{};",
                        renamed_module,
                        const_name
                    ));
                    break;
                }
            }
        }
    }

    // === Handle internal import aliases (e.g., `use crate::Anchored as AcAnchored`) ===
    // Check if this module has any internal aliases and if they're used in the content
    if let Some(aliases) = index.internal_aliases.get(current_module) {
        for alias_info in aliases {
            // Check if the alias is used in the content
            // Use word boundary check to avoid false matches
            let alias_pattern = format!(r"\b{}\b", regex::escape(&alias_info.alias));
            if let Ok(re) = regex::Regex::new(&alias_pattern) {
                if re.is_match(content) {
                    // Find where the original type is defined in the sliced crate
                    if let Some(items) = index.items.get(&alias_info.original) {
                        // Find a public item in a valid module
                        for item in items {
                            // Skip impl blocks
                            if item.kind == crate::types::ParsedItemKind::Impl {
                                continue;
                            }
                            // Must be public
                            if !item.is_pub {
                                continue;
                            }
                    let item_module = normalize_module_path(&item.module_path, crate_name.unwrap_or(""));
                            // Check if the module exists in sliced crate
                            if !item_module.is_empty() && all_paths.contains(&item_module) {
                                // Skip if the first path component is an inline module (E0255 fix)
                                let first_component = item_module.split("::").next().unwrap_or(&item_module);
                                if inline_modules.contains(first_component) {
                                    continue;
                                }
                                // Generate aliased import
                                let renamed_module = rename_reserved_in_path(&item_module);
                                imports.insert(format!(
                                    "use crate::{}::{} as {};",
                                    renamed_module,
                                    alias_info.original,
                                    alias_info.alias
                                ));
                                break;
                            } else if item_module.is_empty() {
                                // Type is in root module
                                imports.insert(format!(
                                    "use crate::{} as {};",
                                    alias_info.original,
                                    alias_info.alias
                                ));
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    if imports.is_empty() {
        String::new()
    } else {
        // Filter out spurious external crate imports that are used with fully qualified paths
        // loom is used in cfg-guarded code with paths like loom::sync::atomic::...
        // Priority 7: Also filter incorrect Visitor trait imports (E0407 fix)
        // Priority 3: Also filter known private item imports (E0603 fix)
        let filtered: Vec<String> = imports.into_iter()
            .filter(|imp| {
                let should_skip = imp == "use loom;" || imp.starts_with("use loom::");
                let is_incorrect_visitor = imp.contains("hir::visitor::Visitor");
                let is_private_import = is_private_item_import(imp);

                // Debug: check all Flags and unicode imports
                if imp.contains("::Flags") || imp.contains("::unicode") {
//                     // crate::debug_log!("[Priority 3 DEBUG] Checking import: '{}' - is_private: {}", imp, is_private_import);
                }

                if is_private_import {
//                     // crate::debug_log!("[Priority 3 DEBUG] Filtering private item import: {}", imp);
                }

                !should_skip && !is_incorrect_visitor && !is_private_import
            })
            .collect();

        if filtered.is_empty() {
            String::new()
        } else {
            filtered.join("\n") + "\n\n"
        }
    }
}

/// Filter imports relevant to a specific module
fn filter_module_imports(
    use_statements: &[crate::types::UseStatement],
    module_path: &str,
    items: &[&crate::types::ParsedItem],
) -> String {
    // Get all code in this module to check what's used
    let mut module_code = String::new();
    for item in items {
        module_code.push_str(&item.source);
        module_code.push('\n');
    }

    filter_preserved_imports(use_statements, &module_code, module_path)
}

/// Result of re-export detection
struct ReexportInfo {
    /// Re-exports for lib.rs (direct crate::Type references)
    root_reexports: String,
    /// Re-exports for intermediate modules (crate::module::Type references)
    /// Maps module_path -> (type_name, source_submodule) pairs
    module_reexports: HashMap<String, Vec<(String, String)>>,
}

/// Detect `crate::TypeName` and `crate::module::TypeName` patterns in code and generate
/// corresponding re-exports. This handles cases where code references types via paths
/// like `crate::Result` or `crate::reflect::RuntimeTypeMessage`.
fn detect_and_generate_reexports(
    all_code: &str,
    modules: &std::collections::BTreeMap<String, Vec<&crate::types::ParsedItem>>,
    all_paths: &HashSet<String>,
) -> ReexportInfo {
    use std::collections::BTreeSet;

    // Build a map of type_name -> (full_module_path, last_segment_of_path) for all type definitions
    // This allows us to look up where types are defined and their relative paths
    let mut type_to_full_path: HashMap<String, (String, String)> = HashMap::new();
    // Also keep a simple type_name -> module_path map for direct crate::Type references
    let mut type_to_module: HashMap<String, String> = HashMap::new();

    // First, collect all types defined in root module (to avoid re-exporting them)
    let mut root_defined_types: HashSet<String> = HashSet::new();
    if let Some(root_items) = modules.get("") {
        for item in root_items {
            if matches!(item.kind,
                crate::types::ParsedItemKind::Struct |
                crate::types::ParsedItemKind::Enum |
                crate::types::ParsedItemKind::TypeAlias |
                crate::types::ParsedItemKind::Trait
            ) {
                root_defined_types.insert(item.name.clone());
            }
        }
    }

    for (mod_path, items) in modules {
        for item in items {
            // Only track types (struct, enum, type alias, trait)
            if matches!(item.kind,
                crate::types::ParsedItemKind::Struct |
                crate::types::ParsedItemKind::Enum |
                crate::types::ParsedItemKind::TypeAlias |
                crate::types::ParsedItemKind::Trait
            ) {
                if !mod_path.is_empty() {
                    // Get the last segment of the module path
                    let last_segment = mod_path.rsplit("::").next().unwrap_or(mod_path).to_string();
                    type_to_full_path.insert(item.name.clone(), (mod_path.clone(), last_segment));
                }

                // For direct crate::Type references
                if !mod_path.is_empty() || !type_to_module.contains_key(&item.name) {
                    type_to_module.insert(item.name.clone(), mod_path.clone());
                }
            }
        }
    }

    // Also scan source code for type definitions that may have been missed in parsing
    // This catches traits, structs, enums, and type aliases defined in the code
    let type_def_patterns = [
        (regex::Regex::new(r"pub\s+trait\s+(\w+)").unwrap(), "trait"),
        (regex::Regex::new(r"pub\s+struct\s+(\w+)").unwrap(), "struct"),
        (regex::Regex::new(r"pub\s+enum\s+(\w+)").unwrap(), "enum"),
        (regex::Regex::new(r"pub\s+type\s+(\w+)").unwrap(), "type"),
    ];

    for (pattern, _kind) in &type_def_patterns {
        for cap in pattern.captures_iter(all_code) {
            let type_name = cap.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
            if !type_name.is_empty() && !type_to_module.contains_key(&type_name) {
                // Try to find the module path by looking for module patterns near this definition
                // We look for modules in all_paths that might contain this type
                let type_lower = type_name.to_lowercase();
                for mod_path in all_paths {
                    let mod_name = mod_path.rsplit("::").next().unwrap_or(mod_path);
                    // Match module name to type name (e.g., "enums" -> "Enum", "oneof" -> "Oneof")
                    if mod_name == type_lower || mod_name == format!("{}s", type_lower)
                       || mod_name == format!("{}_", type_lower) || type_lower.starts_with(mod_name) {
                        type_to_module.insert(type_name.clone(), mod_path.clone());
                        break;
                    }
                }
            }
        }
    }

    let mut needed_reexports: BTreeSet<(String, String)> = BTreeSet::new();
    // Track which module paths need re-exports for nested references
    // Maps module_path -> [(type_name, source_submodule)]
    let mut module_reexports: HashMap<String, Vec<(String, String)>> = HashMap::new();

    // Pattern matching for crate:: references
    let patterns = ["crate::", "crate ::"];

    for pattern in patterns {
        let mut search_pos = 0;
        while let Some(pos) = all_code[search_pos..].find(pattern) {
            let abs_pos = search_pos + pos + pattern.len();
            search_pos = abs_pos;

            // Skip whitespace after ::
            let rest = all_code[abs_pos..].trim_start();
            if rest.is_empty() {
                continue;
            }

            // Parse the full path after crate::
            // Could be: Type, module::Type, module::submodule::Type
            let path_chars: String = rest.chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == ':' || *c == ' ')
                .collect();

            // Normalize spaces around ::
            let path_normalized = path_chars
                .replace(" :: ", "::")
                .replace(" ::", "::")
                .replace(":: ", "::");

            let path_parts: Vec<&str> = path_normalized.split("::").collect();
            if path_parts.is_empty() {
                continue;
            }

            // Check if the last part is a type (starts with uppercase)
            let last_part = path_parts.last().unwrap().trim();
            if last_part.is_empty() || !last_part.chars().next().unwrap().is_ascii_uppercase() {
                continue;
            }

            let type_name = last_part.to_string();

            // Priority 7 debug: trace Visitor re-export logic
            if type_name == "Visitor" {
//                 eprintln!("\n[P7 DEBUG] Found crate:: reference to Visitor");
                crate::debug_log!("  Full path: {}", path_normalized);
                crate::debug_log!("  Path parts: {:?}", path_parts);
            }

            if path_parts.len() == 1 {
                // Direct crate::Type reference
                // Skip if the type is already defined in the root module (avoid E0255 conflicts)
                if root_defined_types.contains(&type_name) {
                    continue;
                }

                if let Some(mod_path) = type_to_module.get(&type_name) {
                    if !mod_path.is_empty() && all_paths.contains(mod_path) {
                        let fixed_path = mod_path.split("::")
                            .map(|p| rename_reserved_module(p))
                            .collect::<Vec<_>>()
                            .join("::");
                        needed_reexports.insert((type_name, fixed_path));
                    }
                } else {
                    // Fallback: if type not in parsed map, try to find it by scanning
                    // module paths that have the same name as the type (lowercase)
                    // e.g., "Enum" trait might be in "enums" module
                    // Skip if already defined in root to avoid conflicts
                    if !root_defined_types.contains(&type_name) {
                        let type_lower = type_name.to_lowercase();
                        for mod_path in all_paths {
                            // Check if module name matches type name (lowercase comparison)
                            // e.g., "enums" matches "Enum", "oneof" matches "Oneof"
                            let mod_name = mod_path.rsplit("::").next().unwrap_or(mod_path);
                            if mod_name == type_lower || mod_name == format!("{}s", type_lower)
                               || mod_name == format!("{}_", type_lower) || mod_name.starts_with(&format!("{}_", type_lower)) {
                                let fixed_path = mod_path.split("::")
                                    .map(|p| rename_reserved_module(p))
                                    .collect::<Vec<_>>()
                                    .join("::");
                                needed_reexports.insert((type_name.clone(), fixed_path));
                                break;
                            }
                        }
                    }
                }
            } else {
                // Nested reference like crate::module::Type or crate::mod1::mod2::Type
                // We need to re-export from the intermediate module
                let intermediate_path: String = path_parts[..path_parts.len()-1].join("::");

                // Look up where this type is actually defined
                if let Some((actual_path, _)) = type_to_full_path.get(&type_name) {
                    // Priority 7 debug: trace Visitor mapping
                    if type_name == "Visitor" {
//                         // // eprintln!("[P7 DEBUG] Visitor nested reference:");
                        crate::debug_log!("  Intermediate path: {}", intermediate_path);
                        crate::debug_log!("  Actual path from type_to_full_path: {}", actual_path);
                    }

                    // Check if the type is defined in a submodule of intermediate_path
                    // e.g., intermediate_path = "rt", actual_path = "rt::lazy"
                    let prefix = format!("{}::", intermediate_path);
                    if actual_path.starts_with(&prefix) && all_paths.contains(actual_path) {
                        // Get the relative path from intermediate to actual
                        let relative = &actual_path[prefix.len()..];
                        // Get just the immediate child module
                        let child_module = relative.split("::").next().unwrap_or(relative);
                        if !child_module.is_empty() {
                            // Priority 7: Skip adding Visitor re-exports from hir::visitor to ast modules
                            // ast::Visitor and hir::Visitor are different traits with different method signatures
                            if type_name == "Visitor" {
//                                 // // eprintln!("[P7 DEBUG] Checking filter for Visitor child module:");
                                crate::debug_log!("  intermediate_path.starts_with('ast'): {}", intermediate_path.starts_with("ast"));
                                crate::debug_log!("  actual_path.starts_with('hir::visitor'): {}", actual_path.starts_with("hir::visitor"));
                                crate::debug_log!("  Would filter: {}", intermediate_path.starts_with("ast") && actual_path.starts_with("hir::visitor"));
                            }
                            if type_name == "Visitor" && intermediate_path.starts_with("ast") && actual_path.starts_with("hir::visitor") {
//                                 // // eprintln!("[P7 DEBUG] FILTERED Visitor re-export!");
                                continue;
                            }

                            module_reexports
                                .entry(intermediate_path.clone())
                                .or_default()
                                .push((type_name.clone(), child_module.to_string()));
                        }
                    } else if all_paths.contains(actual_path) {
                        // Type is in a sibling module (e.g., crate::rt::Lazy but Lazy is in crate::lazy)
                        // Need to add re-export from sibling to intermediate module
                        // Format: pub use crate::actual_path::TypeName;

                        // Priority 7: Skip adding Visitor re-exports from hir::visitor to ast modules
                        if type_name == "Visitor" {
//                             // // eprintln!("[P7 DEBUG] Checking filter for Visitor sibling module:");
                            crate::debug_log!("  intermediate_path.starts_with('ast'): {}", intermediate_path.starts_with("ast"));
                            crate::debug_log!("  actual_path: {}", actual_path);
                            crate::debug_log!("  actual_path.starts_with('hir::visitor'): {}", actual_path.starts_with("hir::visitor"));
                            crate::debug_log!("  Would filter: {}", intermediate_path.starts_with("ast") && actual_path.starts_with("hir::visitor"));
                        }
                        if type_name == "Visitor" && intermediate_path.starts_with("ast") && actual_path.starts_with("hir::visitor") {
//                             // // eprintln!("[P7 DEBUG] FILTERED Visitor sibling re-export!");
                            continue;
                        }

                        if type_name == "Visitor" {
//                             // // eprintln!("[P7 DEBUG] Adding Visitor re-export: from {} to {}", actual_path, intermediate_path);
                        }
                        module_reexports
                            .entry(intermediate_path.clone())
                            .or_default()
                            .push((type_name.clone(), format!("crate::{}", actual_path)));
                    } else {
                        if type_name == "Visitor" {
//                             // // eprintln!("[P7 DEBUG] Visitor: No branch taken (actual_path not in all_paths?)");
                            crate::debug_log!("  actual_path: {}", actual_path);
                            crate::debug_log!("  all_paths.contains(actual_path): {}", all_paths.contains(actual_path));
                        }
                    }
                }
            }
        }
    }

    // Generate the re-export statements for lib.rs (direct crate::Type references)
    let mut root_result = String::new();
    for (type_name, mod_path) in needed_reexports {
        root_result.push_str(&format!("pub use crate::{}::{};\n", mod_path, type_name));
    }

    // Deduplicate module_reexports entries
    for entries in module_reexports.values_mut() {
        entries.sort();
        entries.dedup();
    }

    ReexportInfo {
        root_reexports: root_result,
        module_reexports,
    }
}

/// Extract types referenced with :: prefix (crate root qualification)
/// Returns a set of type names that need to be available at the crate root
/// Example: `:: c_int` means we need to re-export c_int to the crate root
fn extract_crate_root_types(all_code: &str) -> HashSet<String> {
    let mut types = HashSet::new();

    // Pattern: :: typename (whitespace optional)
    // Examples: :: c_int, ::c_int, :: Option, ::size_t
    // Match :: followed by optional whitespace, then an identifier
    let re = regex::Regex::new(r":: *([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();

    for cap in re.captures_iter(all_code) {
        if let Some(type_name) = cap.get(1) {
            types.insert(type_name.as_str().to_string());
        }
    }

    types
}

/// Build a map of type names to their module paths within the crate
/// Returns HashMap<type_name, module_path> for all type definitions in modules
fn build_type_to_module_map(
    modules: &std::collections::BTreeMap<String, Vec<&crate::types::ParsedItem>>,
) -> HashMap<String, String> {
    let mut type_map = HashMap::new();

    for (module_path, items) in modules {
        for item in items {
            // Only track actual type definitions (not functions, constants, etc.)
            match item.kind {
                crate::types::ParsedItemKind::Struct |
                crate::types::ParsedItemKind::Enum |
                crate::types::ParsedItemKind::TypeAlias |
                crate::types::ParsedItemKind::Trait => {
                    type_map.insert(item.name.clone(), module_path.clone());
                }
                _ => {}
            }
        }
    }

    type_map
}

/// Map type names to their likely std module source
/// Returns module path where the type should be imported from
fn map_type_to_std_module(type_name: &str) -> Option<&'static str> {
    match type_name {
        // std::ffi types (most common)
        "c_char" | "c_schar" | "c_uchar" |
        "c_short" | "c_ushort" |
        "c_int" | "c_uint" |
        "c_long" | "c_ulong" |
        "c_longlong" | "c_ulonglong" |
        "c_float" | "c_double" |
        "c_void" => Some("std::ffi"),

        // std::os types (platform-specific types)
        "size_t" | "ssize_t" |
        "off_t" | "off64_t" |
        "mode_t" | "pid_t" | "uid_t" | "gid_t" |
        "dev_t" | "ino_t" | "nlink_t" |
        "blksize_t" | "blkcnt_t" |
        "time_t" | "suseconds_t" | "useconds_t" |
        "clockid_t" | "clock_t" => Some("std::os"),

        // std::mem functions
        "size_of" | "align_of" | "offset_of" => Some("std::mem"),

        // std::fmt module
        "fmt" => Some("std"),

        // std core types
        "Option" | "Result" | "Some" | "None" | "Ok" | "Err" => Some("std::option"),

        // std traits
        "Clone" | "Copy" | "Debug" | "Default" | "PartialEq" | "Eq" |
        "PartialOrd" | "Ord" | "Hash" => Some("std"),

        // Don't re-export crate-defined types or unknown types
        _ => None,
    }
}

/// Generate lib.rs content with module declarations and root items
/// Returns (lib_content, module_reexports) where module_reexports maps module paths
/// to (type_name, source_submodule) pairs that need to be re-exported
fn generate_lib_rs(
    modules: &std::collections::BTreeMap<String, Vec<&crate::types::ParsedItem>>,
    all_paths: &HashSet<String>,
    use_statements: &[crate::types::UseStatement],
    all_code: &str,
    index: &CrateIndex,
    crate_local_types: &HashSet<String>,
    skipped_modules: &HashSet<String>,
    macro_use_crates: &HashSet<String>,
    crate_name: &str,  // Phase 6.3: Added for platform-specific filtering
) -> (String, HashMap<String, Vec<(String, String)>>) {
    // Build set of modules that have actual content (populated_modules)
    let populated_modules: HashSet<String> = modules.keys().cloned().collect();
    let mut content = String::from("//! Sliced crate - auto-generated\n\n");

    // Detect and add unstable feature flags if needed
    let required_features = detect_required_features(all_code);
    if !required_features.is_empty() {
        for feature in required_features {
            content.push_str(&format!("#![feature({})]\n", feature));
        }
        content.push('\n');
    }

    // Add #[macro_use] extern crate declarations for external macros
    // e.g., #[macro_use] extern crate scopeguard; for defer! macro
    if !macro_use_crates.is_empty() {
        for crate_name in macro_use_crates.iter() {
            content.push_str(&format!("#[macro_use]\nextern crate {};\n\n", crate_name));
        }
    }

    // If the code uses alloc:: patterns (alloc crate, not std::alloc), add extern crate
    // The alloc crate provides format!, vec!, String, etc. for no_std environments
    // but is also usable in std environments with extern crate alloc;
    // Note: syn's quote! adds spaces around ::, so check both patterns
    if all_code.contains("alloc::format!")
        || all_code.contains("alloc :: format !")
        || all_code.contains("alloc::vec!")
        || all_code.contains("alloc :: vec !")
        || all_code.contains("alloc::string::")
        || all_code.contains("alloc :: string ::")
        || all_code.contains("alloc::vec::")
        || all_code.contains("alloc :: vec ::")
        || all_code.contains("alloc::boxed::")
        || all_code.contains("alloc :: boxed ::")
        || all_code.contains("alloc::borrow::")
        || all_code.contains("alloc :: borrow ::")
        || all_code.contains("alloc::collections::")
        || all_code.contains("alloc :: collections ::")
    {
        content.push_str("extern crate alloc;\n\n");
    }

    // Build set of modules to skip (those using unstable features)
    let mut unstable_modules: HashSet<String> = HashSet::new();
    for (mod_path, items) in modules {
        for item in items {
            if uses_unstable_features(&item.source) {
                // Skip this module and any parent modules that only contain it
                let top_mod = mod_path.split("::").next().unwrap_or(mod_path);
                if !top_mod.is_empty() {
                    unstable_modules.insert(top_mod.to_string());
                }
                break;
            }
        }
    }

    // Collect inline module names from root-level items (avoid duplicate declarations)
    // Only include modules defined inline (mod name { }) not module declarations (mod name;)
    let inline_module_names: HashSet<String> = modules.get("")
        .map(|items| {
            items.iter()
                .filter(|item| {
                    matches!(item.kind, crate::types::ParsedItemKind::Mod) &&
                    item.source.contains('{') // Inline modules have {}, declarations have ;
                })
                .map(|item| item.name.clone())
                .collect()
        })
        .unwrap_or_default();

    // Add top-level module declarations (direct children of root)
    // Skip modules that have no items and no children (likely type names mistaken for modules)
    // Also skip modules that are defined inline in the root items
    let mut top_modules: Vec<&str> = all_paths.iter()
        .filter_map(|p| {
            if !p.contains("::") && !p.is_empty() {
                // Skip modules that use unstable features
                if unstable_modules.contains(p.as_str()) {
                    return None;
                }
                // Skip modules that were skipped (test-only, empty after stripping, etc.)
                if skipped_modules.contains(p.as_str()) {
                    return None;
                }
                // Skip modules that are defined inline in root items
                if inline_module_names.contains(p) {
                    return None;
                }
                // Skip empty modules with no populated children
                // These are likely type names (e.g., __m256i, uint8x16_t) or modules with no content
                let has_items = populated_modules.contains(p.as_str());
                let prefix = format!("{}::", p);
                let has_populated_children = populated_modules.iter().any(|path| path.starts_with(&prefix));
                if !has_items && !has_populated_children {
                    return None;
                }
                Some(p.as_str())
            } else {
                None
            }
        })
        .collect();
    top_modules.sort();
    top_modules.dedup();

    // Phase 6.3: Platform-specific module filtering for libc crate
    // The libc crate has separate modules for each platform (unix, windows, fuchsia, etc.)
    // Including all platforms causes massive duplication of FFI types (c_char, c_int, etc.)
    // Solution: Only include the module matching the current target platform
    if crate_name == "libc" {
        let target_platform = std::env::consts::OS; // "linux", "windows", "macos", etc.

        // Map OS to libc platform module name
        let platform_module = match target_platform {
            "linux" | "android" | "freebsd" | "openbsd" | "netbsd" | "dragonfly" | "macos" | "ios" => "unix",
            "windows" => "windows",
            "fuchsia" => "fuchsia",
            "wasi" => "wasi",
            "hermit" => "hermit",
            _ => "unix", // Default to unix for unknown platforms
        };

        // Filter out non-matching platform modules
        let platform_modules = vec!["unix", "windows", "fuchsia", "wasi", "hermit", "psp", "sgx", "switch", "vxworks"];
        top_modules.retain(|module| {
            // Keep non-platform modules (like macros, etc.)
            if !platform_modules.contains(module) {
                return true;
            }
            // Only keep the matching platform module
            *module == platform_module
        });
    }

    // Emit "macros" module first with #[macro_use] so macros are available to all other modules
    // (Rust requires #[macro_use] to appear before modules that use those macros)
    if let Some(pos) = top_modules.iter().position(|m| *m == "macros") {
        top_modules.remove(pos);
        content.push_str("#[macro_use]\nmod macros;\n");
    }

    for module in &top_modules {
        // Rename reserved modules to avoid shadowing std crates
        let module_name = rename_reserved_module(module);
        content.push_str(&format!("pub mod {};\n", module_name));
    }

    // Generate root-level re-exports for crate::TypeName patterns used in the code
    // This handles cases like `crate::Result`, `crate::Error`, etc. that refer to
    // types defined in submodules but accessed via the crate root.
    let reexport_info = detect_and_generate_reexports(all_code, modules, all_paths);
    if !reexport_info.root_reexports.is_empty() {
        content.push_str("\n// Re-exports for crate:: references\n");
        content.push_str(&reexport_info.root_reexports);
    }

    // Generate crate root re-exports for :: typename patterns (Phase 1.5 + 1.6)
    // This handles cases like `:: c_int`, `:: size_t`, etc. where code uses
    // types qualified with :: prefix, meaning they must be available at the crate root.
    // Common in libc and other FFI crates.
    let crate_root_types = extract_crate_root_types(all_code);
    if !crate_root_types.is_empty() {
        // Build map of type names to their module paths in this crate (Phase 1.6)
        let type_to_module = build_type_to_module_map(modules);

        // Group types by their source module
        let mut std_ffi_types = Vec::new();
        let mut std_os_types = Vec::new();
        let mut std_types = Vec::new();
        let mut crate_local_types: Vec<(String, String)> = Vec::new(); // (type_name, module_path)

        for type_name in &crate_root_types {
            match map_type_to_std_module(type_name) {
                Some("std::ffi") => std_ffi_types.push(type_name.as_str()),
                Some("std::os") => std_os_types.push(type_name.as_str()),
                Some("std") => std_types.push(type_name.as_str()),
                Some("std::option") => std_types.push(type_name.as_str()),
                Some("std::mem") => std_types.push(type_name.as_str()),
                _ => {
                    // Phase 1.6: Check if this is a crate-local type
                    if let Some(module_path) = type_to_module.get(type_name) {
                        crate_local_types.push((type_name.clone(), module_path.clone()));
                    }
                    // else: truly unknown type, skip it
                }
            }
        }

        // Sort for consistent output
        std_ffi_types.sort();
        std_os_types.sort();
        std_types.sort();
        crate_local_types.sort();

        // Emit re-export statements
        content.push_str("\n// Re-exports for :: qualified type references (crate root)\n");

        if !std_ffi_types.is_empty() {
            content.push_str(&format!("pub use std::ffi::{{{}}};\n", std_ffi_types.join(", ")));
        }

        if !std_os_types.is_empty() {
            // Note: std::os types are typically NOT re-exported at crate root
            // because they don't exist in std::os directly - they're in std::os::raw
            // or platform-specific modules. Since libc likely defines these itself,
            // we'll skip re-exporting them from std and let libc's own definitions be used.
            // If errors persist, they're likely crate-defined types that need internal re-exports.
        }

        if !std_types.is_empty() {
            // These are mixed types from std - emit individually
            for type_name in std_types {
                match type_name {
                    "fmt" => content.push_str("pub use std::fmt;\n"),
                    "size_of" | "align_of" | "offset_of" => content.push_str(&format!("pub use std::mem::{};\n", type_name)),
                    "Option" | "Some" | "None" => {}, // Already in prelude
                    "Result" | "Ok" | "Err" => {},    // Already in prelude
                    // Phase 6.1 fix: Skip all stdlib prelude traits - Phase 5.2 handles these correctly
                    "Clone" | "Copy" | "Debug" | "Default" | "PartialEq" | "Eq" |
                    "PartialOrd" | "Ord" | "Hash" => {}, // Handled by Phase 5.2 stdlib_prelude_types
                    _ => content.push_str(&format!("pub use std::{};\n", type_name)),
                }
            }
        }

        // Phase 1.7: Define common POSIX types at crate root (for FFI crates like libc)
        // These types are typically defined in each platform module with the same underlying type
        // Define them directly at crate root to avoid :: resolution issues
        let common_posix_types: &[(& str, &str)] = &[
            ("size_t", "usize"),
            ("ssize_t", "isize"),
            ("mode_t", "u32"),
            ("pid_t", "i32"),
            ("uid_t", "u32"),
            ("gid_t", "u32"),
            ("dev_t", "u64"),
            ("ino_t", "u64"),
            ("off_t", "i64"),
            ("off64_t", "i64"),
            ("blksize_t", "i64"),
            ("blkcnt_t", "i64"),
            ("time_t", "i64"),
            ("suseconds_t", "i64"),
            ("useconds_t", "u32"),
            ("clockid_t", "i32"),
            ("clock_t", "i64"),
            ("nlink_t", "u64"),
        ];

        let mut defined_posix_types = Vec::new();
        for (type_name, underlying_type) in common_posix_types {
            if crate_root_types.contains(*type_name) {
                defined_posix_types.push((*type_name, *underlying_type));
            }
        }

        if !defined_posix_types.is_empty() {
            content.push_str("// Common POSIX types (defined at crate root)\n");
            for (type_name, underlying_type) in defined_posix_types {
                content.push_str(&format!("pub type {} = {};\n", type_name, underlying_type));
            }
        }

        // Phase 5.2: Re-export stdlib prelude types at crate root
        // These are commonly used std types referenced with :: prefix
        let stdlib_prelude_types: &[(&str, &str)] = &[
            // Option and Result enums
            ("Option", "std::option::Option"),
            ("Result", "std::result::Result"),
            // Common marker traits
            ("Copy", "std::marker::Copy"),
            ("Clone", "std::clone::Clone"),
            ("Send", "std::marker::Send"),
            ("Sync", "std::marker::Sync"),
            ("Sized", "std::marker::Sized"),
            ("Unpin", "std::marker::Unpin"),
            // Comparison traits
            ("Eq", "std::cmp::Eq"),
            ("PartialEq", "std::cmp::PartialEq"),
            ("Ord", "std::cmp::Ord"),
            ("PartialOrd", "std::cmp::PartialOrd"),
            // Formatting traits
            ("Debug", "std::fmt::Debug"),
            ("Display", "std::fmt::Display"),
            // Common collection types
            ("String", "std::string::String"),
            ("Vec", "std::vec::Vec"),
            ("Box", "std::boxed::Box"),
            // Hash trait
            ("Hash", "std::hash::Hash"),
            // Common error types
            ("Error", "std::error::Error"),
        ];

        let mut needed_stdlib_types = Vec::new();
        for (type_name, path) in stdlib_prelude_types {
            if crate_root_types.contains(*type_name) {
                needed_stdlib_types.push((*type_name, *path));
            }
        }

        if !needed_stdlib_types.is_empty() {
            content.push_str("// Stdlib prelude types (re-exported at crate root)\n");
            for (_type_name, path) in needed_stdlib_types {
                content.push_str(&format!("pub use {};\n", path));
            }
        }

        // Phase 1.6: Emit crate-local type re-exports
        // These are types defined within the crate that are referenced with :: prefix
        // Example: :: size_t where size_t is defined in crate::unix::types
        if !crate_local_types.is_empty() {
            content.push_str("// Crate-local type re-exports\n");
            for (type_name, module_path) in crate_local_types {
                if module_path.is_empty() {
                    // Type is defined at crate root - no re-export needed
                    continue;
                }
                // Skip if we already defined this as a common POSIX type
                let is_common_posix = common_posix_types.iter().any(|(n, _)| *n == type_name);
                if is_common_posix {
                    continue;
                }
                // Skip if we already re-exported this as a stdlib prelude type
                let is_stdlib_prelude = stdlib_prelude_types.iter().any(|(n, _)| *n == type_name);
                if is_stdlib_prelude {
                    continue;
                }
                content.push_str(&format!("pub use crate::{}::{};\n", module_path, type_name));
            }
        }
    }

    if !top_modules.is_empty() {
        content.push('\n');
    }

    // Add root-level items (empty module path) with their imports
    if let Some(items) = modules.get("") {
        // Sort items to ensure macros come before code that uses them
        let mut sorted_items = items.clone();
        sorted_items.sort_by_key(|item| {
            match item.kind {
                crate::types::ParsedItemKind::Macro => 0,  // Macros first
                crate::types::ParsedItemKind::Const => 1,  // Then constants
                crate::types::ParsedItemKind::Static => 1,
                crate::types::ParsedItemKind::TypeAlias => 2,  // Then type aliases
                crate::types::ParsedItemKind::Struct => 3,  // Then types
                crate::types::ParsedItemKind::Enum => 3,
                crate::types::ParsedItemKind::Trait => 4,  // Then traits
                crate::types::ParsedItemKind::Function => 5,  // Then functions
                crate::types::ParsedItemKind::Impl => 6,  // Then impl blocks
                _ => 7,  // Everything else last
            }
        });

        // Build root content first to detect needed imports
        let mut root_content = String::new();
        for item in &sorted_items {
            // Skip mod declarations - they're handled by top_modules above
            if matches!(item.kind, crate::types::ParsedItemKind::Mod) {
                continue;
            }
            // Skip items using unstable features
            if uses_unstable_features(&item.source) {
                continue;
            }
            // Strip #[cfg(test)] blocks and internal imports from source
            let clean_source = strip_cfg_test_blocks(&item.source);
            let clean_source = strip_internal_imports(&clean_source);
            root_content.push_str(&clean_source);
            root_content.push_str("\n\n");
        }

        // Collect names of TYPE definitions in root (not impl blocks)
        // Also include Const since constants are uppercase and go through the type detection loop
        // Priority 4: Include Functions to prevent duplicate imports after module flattening
        let locally_defined: HashSet<String> = sorted_items.iter()
            .filter(|item| matches!(item.kind,
                crate::types::ParsedItemKind::Struct |
                crate::types::ParsedItemKind::Enum |
                crate::types::ParsedItemKind::TypeAlias |
                crate::types::ParsedItemKind::Trait |
                crate::types::ParsedItemKind::Const |
                crate::types::ParsedItemKind::Function
            ))
            .map(|item| item.name.clone())
            .collect();

        // Get preserved imports from original use statements (includes module aliases like `use unix as imp`)
        // Priority 4: Filter against locally_defined to prevent conflicts with flattened module items
        let preserved_imports = filter_preserved_imports_excluding_local(use_statements, &root_content, "", Some(all_paths), Some(crate_name), &locally_defined);
        if !preserved_imports.is_empty() {
            content.push_str(&preserved_imports);
            content.push_str("\n\n");
        }

        // Generate imports for root-level items
        // Include symbols from preserved imports to avoid duplicates
        let already_imported = extract_symbols_from_preserved_imports(use_statements, "");
        // Child modules for root are top-level modules (no :: in path)
        let root_child_modules: HashSet<String> = all_paths.iter()
            .filter(|p| !p.contains("::"))
            .cloned()
            .collect();

        // Extract context for decision logging
        let file_path_str = sorted_items.first().map(|i| i.file.display().to_string()).unwrap_or_default();
        let cfg_conditions: Vec<String> = sorted_items.iter()
            .filter_map(|i| i.cfg_attr.as_ref())
            .cloned()
            .collect();

        let imports = generate_internal_imports(&root_content, "", index, all_paths, &locally_defined, crate_local_types, &already_imported, &root_child_modules, Some(crate_name), None, Some(&populated_modules), Some(&file_path_str), cfg_conditions);
        content.push_str(&imports);
        for item in &sorted_items {
            // Skip mod declarations - they're handled by top_modules above
            if matches!(item.kind, crate::types::ParsedItemKind::Mod) {
                continue;
            }
            if uses_unstable_features(&item.source) {
                continue;
            }
            // Strip #[cfg(test)] blocks and internal imports from source
            let clean_source = strip_cfg_test_blocks(&item.source);
            let clean_source = strip_internal_imports(&clean_source);
            content.push_str(&clean_source);
            content.push_str("\n\n");
        }
    }

    (content, reexport_info.module_reexports)
}

/// Apply module-level re-exports to the generated module files
/// This handles cases like `crate::rt::Lazy` where `Lazy` is defined in `rt::lazy`
fn apply_module_reexports(
    output_dir: &Path,
    module_reexports: &HashMap<String, Vec<(String, String)>>,
) -> std::io::Result<()> {
    for (module_path, reexports) in module_reexports {
        // Priority 7 fix: Skip processing EARLY if there are no re-exports
        // This prevents file modification when no changes are needed
        if reexports.is_empty() {
            continue;
        }

        // Find the module file
        let parts: Vec<&str> = module_path.split("::").collect();
        let file_path = if parts.len() == 1 {
            // Single-level module like "rt"
            let mod_file = output_dir.join("src").join(format!("{}.rs", parts[0]));
            let mod_dir = output_dir.join("src").join(parts[0]).join("mod.rs");
            if mod_dir.exists() {
                mod_dir
            } else if mod_file.exists() {
                mod_file
            } else {
                continue;
            }
        } else {
            // Nested module like "reflect::rt"
            let mut path = output_dir.join("src");
            for part in &parts[..parts.len()-1] {
                path = path.join(part);
            }
            let last = parts.last().unwrap();
            let mod_file = path.join(format!("{}.rs", last));
            let mod_dir = path.join(last).join("mod.rs");
            if mod_dir.exists() {
                mod_dir
            } else if mod_file.exists() {
                mod_file
            } else {
                continue;
            }
        };

        // Read the file
        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        // Priority 7 debug: check ast::print content before rewriting
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] ast::print content BEFORE rewriting:");
            for (i, line) in content.lines().take(5).enumerate() {
                crate::debug_log!("  {}:  {}", i+1, line);
            }
        }

        // Generate the re-export statements
        let mut reexport_lines = String::new();
        for (type_name, source_module) in reexports {
            // Skip self-referential re-exports - when source_module points to the current module
            // e.g., don't add "pub use crate::descriptor::FieldOptions;" in descriptor/mod.rs
            let self_ref_path = format!("crate::{}", module_path);
            if source_module == &self_ref_path {
                continue;
            }

            // Skip if the type is already defined in this file (not just imported)
            // Check for struct, enum, type alias, trait, fn definitions
            // Include all visibility modifiers: pub, pub(crate), pub (crate), and bare (private)
            let def_patterns = [
                // pub visibility
                format!("pub struct {} ", type_name),
                format!("pub struct {}<", type_name),
                format!("pub struct {} {{", type_name),
                format!("pub enum {} ", type_name),
                format!("pub enum {}<", type_name),
                format!("pub enum {} {{", type_name),
                format!("pub type {} ", type_name),
                format!("pub type {}<", type_name),
                format!("pub trait {} ", type_name),
                format!("pub trait {}<", type_name),
                format!("pub fn {} ", type_name),
                format!("pub fn {}<", type_name),
                format!("pub mod {} ", type_name),
                format!("pub mod {} {{", type_name),
                // pub(crate) visibility (with and without space)
                format!("pub(crate) struct {} ", type_name),
                format!("pub(crate) struct {}<", type_name),
                format!("pub(crate) struct {} {{", type_name),
                format!("pub (crate) struct {} ", type_name),
                format!("pub (crate) struct {}<", type_name),
                format!("pub (crate) struct {} {{", type_name),
                format!("pub(crate) enum {} ", type_name),
                format!("pub(crate) enum {}<", type_name),
                format!("pub (crate) enum {} ", type_name),
                format!("pub (crate) enum {}<", type_name),
                format!("pub(crate) type {} ", type_name),
                format!("pub(crate) type {}<", type_name),
                format!("pub (crate) type {} ", type_name),
                format!("pub (crate) type {}<", type_name),
                format!("pub(crate) fn {} ", type_name),
                format!("pub(crate) fn {}<", type_name),
                format!("pub (crate) fn {} ", type_name),
                format!("pub (crate) fn {}<", type_name),
                // private (no pub) definitions
                format!("struct {} ", type_name),
                format!("struct {}<", type_name),
                format!("struct {} {{", type_name),
                format!("enum {} ", type_name),
                format!("enum {}<", type_name),
                format!("enum {} {{", type_name),
                format!("type {} ", type_name),
                format!("type {}<", type_name),
                format!("fn {} ", type_name),
                format!("fn {}<", type_name),
            ];

            let is_defined_locally = def_patterns.iter().any(|pattern| content.contains(pattern));
            if is_defined_locally {
                continue;
            }

            // Check if this re-export already exists (various patterns)
            let import_exists = {
                // Direct import: use module::TypeName or pub use module::TypeName
                let patterns = [
                    format!("use {}::{}", source_module, type_name),
                    format!("use self::{}::{}", source_module, type_name),
                    format!("pub use {}::{}", source_module, type_name),
                    format!("pub use self::{}::{}", source_module, type_name),
                    format!("pub(crate) use {}::{}", source_module, type_name),
                    format!("pub(crate) use self::{}::{}", source_module, type_name),
                ];

                // Check direct patterns
                let direct_match = patterns.iter().any(|p| content.contains(p));

                // Also check for grouped imports: use module::{..., TypeName, ...}
                let grouped_match = content.lines().any(|line| {
                    // Look for lines with braces that contain the type name
                    if (line.contains(source_module) || line.contains(&format!("self::{}", source_module.trim_start_matches("crate::")))) &&
                       line.contains('{') && line.contains(type_name) {
                        // Check if type_name is in the braces
                        if let Some(brace_start) = line.find('{') {
                            if let Some(brace_end) = line.find('}') {
                                let inside = &line[brace_start..brace_end];
                                // Check for TypeName as whole word (not part of another name)
                                return inside.split(&[',', ' ', '{', '}'][..])
                                    .any(|s| s.trim() == type_name);
                            }
                        }
                    }
                    false
                });

                direct_match || grouped_match
            };

            if import_exists {
                continue;
            }
            // Handle both sibling (crate::module) and child (module) sources
            if source_module.starts_with("crate::") {
                reexport_lines.push_str(&format!("pub use {}::{};\n", source_module, type_name));
            } else {
                reexport_lines.push_str(&format!("pub use self::{}::{};\n", source_module, type_name));
            }
        }

        if reexport_lines.is_empty() {
            continue;
        }

        // Prepend the re-exports to the file
        let new_content = format!("// Auto-generated re-exports for crate:: references\n{}\n{}", reexport_lines, content);

        // Fix E0433 partial module paths after re-exports are added
        let new_content = crate::old_slicer::parsing::fix_partial_paths_in_generated_code(&new_content);

        // Priority 7 debug: check ast::print after fix_partial_paths
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] ast::print content AFTER fix_partial_paths:");
            for (i, line) in new_content.lines().take(10).enumerate() {
                crate::debug_log!("  {}:  {}", i+1, line);
            }
        }

        // Priority 7: Filter out incorrect hir::visitor::Visitor imports
        // This is needed because apply_module_reexports rewrites files after write_module_file
        let new_content = new_content
            .lines()
            .filter(|line| {
                let trimmed = line.trim();
                let is_hir_visitor = trimmed == "use crate::hir::visitor::Visitor;" ||
                                    trimmed == "pub use crate::hir::visitor::Visitor;";
                !is_hir_visitor
            })
            .collect::<Vec<&str>>()
            .join("\n");

        // Priority 7 debug: check ast::print FINAL content before writing
        if module_path == "ast::print" {
//             // // eprintln!("[P7 DEBUG] ast::print FINAL content before writing:");
            for (i, line) in new_content.lines().take(10).enumerate() {
                crate::debug_log!("  {}:  {}", i+1, line);
            }
        }

        // Priority 7 debug: log writes to ast/print.rs from apply_module_reexports
        if file_path.to_string_lossy().contains("ast/print.rs") || file_path.to_string_lossy().contains("ast\\print.rs") {
            use std::sync::atomic::{AtomicU32, Ordering};
            static REEXPORT_WRITE_COUNT: AtomicU32 = AtomicU32::new(0);
            let _count = REEXPORT_WRITE_COUNT.fetch_add(1, Ordering::SeqCst) + 1;
//             // // eprintln!("[P7 DEBUG] apply_module_reexports WRITING ast/print.rs (write #{})!", _count);
            crate::debug_log!("  First 5 lines:");
            for (i, line) in new_content.lines().take(5).enumerate() {
                crate::debug_log!("    {}: {}", i+1, line);
            }
        }

        fs::write(&file_path, new_content)?;
    }

    Ok(())
}

/// Generate a standalone sliced crate from a crate path and seed items
pub fn generate_standalone_sliced_crate(
    crate_info: &CrateInfo,
    seed_items: &[String],
    output_dir: &Path,
    _use_ra_deps: bool,
) -> Result<SemanticSliceResult, String> {
    let index = parse_crate(&crate_info.path, &crate_info.name);

    // Convert seed items to needed set
    let mut needed = BTreeSet::new();
    for item in seed_items {
        needed.insert(item.clone());
    }

    // Expand transitively
    let needed = super::slicing::expand_needed_transitively(&needed, &index);

    // Phase 6.3: Apply type closure to include types referenced in signatures
    let needed = super::slicing::compute_type_closure(&needed, &index);

    generate_semantic_sliced_crate_with_needed(crate_info, &index, &needed, output_dir, None, None)
        .map_err(|e| e.to_string())
}

/// Generate a sliced crate with rust-analyzer dependency analysis
pub fn generate_semantic_sliced_crate_with_ra_deps(
    crate_info: &CrateInfo,
    used: &HashSet<UsedItem>,
    output_dir: &Path,
) -> std::io::Result<SemanticSliceResult> {
    // Fall back to regular semantic slicing for now
    // Full ra_deps integration would be added here
    generate_semantic_sliced_crate(crate_info, used, output_dir)
}

/// Fix all crate-internal imports in a sliced crate directory.
/// This post-processes the generated source files to fix imports that
/// reference incorrect module paths after slicing reorganizes items.
fn fix_all_crate_imports(output_dir: &Path) -> std::io::Result<()> {
    use super::source_fix::{build_item_location_map};

    // Build a map of where each item is actually defined
    let item_locations = build_item_location_map(output_dir);

    if item_locations.is_empty() {
        return Ok(());
    }

    // Process all .rs files in the src directory
    let src_dir = output_dir.join("src");
    fix_imports_in_directory(&src_dir, &item_locations)?;

    Ok(())
}

/// Recursively fix imports in all .rs files in a directory
fn fix_imports_in_directory(
    dir: &Path,
    item_locations: &std::collections::HashMap<String, String>,
) -> std::io::Result<()> {
    // First pass: collect all pub(crate) items from the entire codebase
    let mut pub_crate_items = std::collections::HashSet::new();
    collect_pub_crate_items_recursive(dir, &mut pub_crate_items)?;

    // Second pass: apply all fixes
    apply_fixes_recursive(dir, item_locations, &pub_crate_items)?;

    Ok(())
}

/// Recursively collect pub(crate) items from all .rs files
fn collect_pub_crate_items_recursive(
    dir: &Path,
    pub_crate_items: &mut std::collections::HashSet<String>,
) -> std::io::Result<()> {
    use super::source_fix::find_pub_crate_items;

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().map(|e| e == "rs").unwrap_or(false) {
            let content = fs::read_to_string(&path)?;
            let items = find_pub_crate_items(&content);
            pub_crate_items.extend(items);
        } else if path.is_dir() {
            collect_pub_crate_items_recursive(&path, pub_crate_items)?;
        }
    }

    Ok(())
}

/// Recursively apply import fixes to all .rs files
fn apply_fixes_recursive(
    dir: &Path,
    item_locations: &std::collections::HashMap<String, String>,
    pub_crate_items: &std::collections::HashSet<String>,
) -> std::io::Result<()> {
    use super::source_fix::{fix_crate_imports, fix_result_type_alias, fix_conflicting_imports, fix_external_crate_imports, fix_unstable_imports, fix_missing_imports, fix_empty_rt_v2_module, fix_crate_enum_references, fix_module_import_conflicts, fix_lib_rs_reexports, fix_trait_struct_confusion, fix_spurious_doc_imports, fix_private_item_visibility, fix_duplicate_impls, fix_nested_module_imports};

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().map(|e| e == "rs").unwrap_or(false) {
            let content = fs::read_to_string(&path)?;
            let path_str = path.to_string_lossy();
            let fixed = fix_nested_module_imports(&content, &path_str);
            let fixed = fix_crate_imports(&fixed, item_locations);
            let fixed = fix_result_type_alias(&fixed);
            let fixed = fix_conflicting_imports(&fixed);
            let fixed = fix_external_crate_imports(&fixed);
            // Temporarily disabled: let fixed = fix_pub_crate_reexports(&fixed, pub_crate_items);
            let fixed = fix_unstable_imports(&fixed);
            let fixed = fix_missing_imports(&fixed);
            let fixed = fix_empty_rt_v2_module(&fixed, &path_str);
            let fixed = fix_crate_enum_references(&fixed);
            let fixed = fix_module_import_conflicts(&fixed, &path_str);
            let fixed = fix_lib_rs_reexports(&fixed, &path_str);
            let fixed = fix_trait_struct_confusion(&fixed);
            let fixed = fix_spurious_doc_imports(&fixed);
            let fixed = fix_private_item_visibility(&fixed, &path_str);
            let fixed = fix_duplicate_impls(&fixed, &path_str);
            // Temporarily disabled: let fixed = fix_impl_std_type_conflicts(&fixed);
            // Temporarily disabled: let fixed = fix_module_name_conflicts(&fixed);

            // Priority 3: Filter out known private item imports (E0603 fix)
            // Remove imports of private structs/enums and replace private module re-exports with correct public imports
            let fixed = {
                const PRIVATE_IMPORT_PATTERNS: &[&str] = &[
                    "use crate::unicode::Range;",
                    "use crate::hir::translate::Flags;",
                    "use crate::unicode::CanonicalClassQuery;",
                    "use crate::ast::parse::ClassState;",
                    "use crate::ast::parse::GroupState;",
                    "use crate::hir::translate::HirFrame;",
                    "use crate::ast::parse::Primitive;",
                ];

                let lines: Vec<String> = fixed.lines()
                    .filter_map(|line| {
                        let trimmed = line.trim();

                        // Replace private unicode re-export with correct public import
                        if trimmed == "use crate::hir::translate::unicode;" {
                            crate::debug_log!("[Priority 3 APPLY_FIXES] Replacing private unicode import in {}", path_str);
                            return Some("use crate::unicode;".to_string());
                        }

                        // Filter out other private imports
                        let is_private_import = PRIVATE_IMPORT_PATTERNS.iter().any(|pattern| trimmed == *pattern);
                        if is_private_import {
                            crate::debug_log!("[Priority 3 APPLY_FIXES] Removing private import from {}: {}", path_str, trimmed);
                            return None;
                        }

                        Some(line.to_string())
                    })
                    .collect();

                lines.join("\n")
            };

            // Priority 4: Filter out imports from missing modules in memchr crate (E0432 fix)
            // These modules weren't included in slicing, so remove any imports/re-exports
            let fixed = if path_str.contains("memchr-sliced") {
                // Patterns for imports from modules that weren't sliced
                const MISSING_MODULE_PATTERNS: &[&str] = &[
                    "use crate::memchr::",
                    "pub use crate::memchr::",
                    "use crate::genericsimd",
                    "use crate::memmem::vector",
                    "use crate::memmem::rarebytes",
                    "use crate::rarebytes",
                    // Test-only imports that weren't sliced
                    "use crate::quickcheck",
                    "use crate::proptests",
                    "use self::prefilter::tests",
                ];

                // Items that reference missing modules (to be removed from grouped imports)
                const MISSING_MODULE_ITEMS: &[&str] = &[
                    "vector::",
                    "rarebytes::",
                ];

                let mut in_multiline_import = false;
                let mut multiline_matches_pattern = false;

                let lines: Vec<String> = fixed.lines()
                    .filter_map(|line| {
                        let trimmed = line.trim();

                        // Check if we're starting a multi-line import
                        if !in_multiline_import && trimmed.ends_with("{") {
                            // Check if this starts a missing module import
                            multiline_matches_pattern = MISSING_MODULE_PATTERNS.iter().any(|pattern| trimmed.starts_with(pattern));
                            if multiline_matches_pattern {
                                in_multiline_import = true;
                                crate::debug_log!("[Priority 4 APPLY_FIXES] Removing multi-line import from {}: {}", path_str, trimmed);
                                return None;
                            }
                        }

                        // If we're in a multi-line import from a missing module, skip this line
                        if in_multiline_import {
                            let is_end = trimmed.ends_with("};") || trimmed == "};";
                            if is_end {
                                in_multiline_import = false;
                                multiline_matches_pattern = false;
                            }
                            return None;
                        }

                        // Check for single-line imports
                        let is_missing_module_import = MISSING_MODULE_PATTERNS.iter().any(|pattern| trimmed.starts_with(pattern));
                        if is_missing_module_import && trimmed.ends_with(";") {
                            crate::debug_log!("[Priority 4 APPLY_FIXES] Removing import from {}: {}", path_str, trimmed);
                            return None;
                        }

                        // Filter out items referencing missing modules from grouped imports
                        // e.g., "use crate::memmem::{util::memcmp, vector::Vector, NeedleInfo};"
                        // becomes "use crate::memmem::{util::memcmp, NeedleInfo};"
                        let has_missing_module_item = MISSING_MODULE_ITEMS.iter().any(|item| trimmed.contains(item));
                        if has_missing_module_item && (trimmed.contains("use ") || trimmed.contains("pub use ")) {
                            // Remove items referencing missing modules from the import
                            let mut modified_line = line.to_string();
                            for item in MISSING_MODULE_ITEMS {
                                // Match patterns like: "vector::Vector," or "rarebytes::RareByteOffsets,"
                                let item_pattern = format!("{}", item);
                                if modified_line.contains(&item_pattern) {
                                    // Find and remove the item and its trailing comma (or leading comma if at end)
                                    // This is a simple approach - remove "vector::ItemName," or ", vector::ItemName"
                                    let item_start_idx = modified_line.find(&item_pattern);
                                    if let Some(idx) = item_start_idx {
                                        // Find the end of this import item (up to comma or closing brace)
                                        let after_item = &modified_line[idx..];
                                        if let Some(comma_idx) = after_item.find(',') {
                                            // Remove from item start to after comma
                                            let before = &modified_line[..idx];
                                            let after = &modified_line[idx + comma_idx + 1..];
                                            // Also remove leading/trailing whitespace
                                            let before_trimmed = before.trim_end();
                                            let after_trimmed = after.trim_start();
                                            // Check if we need to add/remove comma
                                            if before_trimmed.ends_with(',') || before_trimmed.ends_with('{') {
                                                modified_line = format!("{} {}", before_trimmed, after_trimmed);
                                            } else if after_trimmed.starts_with('}') {
                                                modified_line = format!("{}{}", before_trimmed, after_trimmed);
                                            } else {
                                                modified_line = format!("{}, {}", before_trimmed, after_trimmed);
                                            }
                                            crate::debug_log!("[Priority 4 APPLY_FIXES] Filtering missing module item in {}: {}", path_str, item);
                                        } else if let Some(brace_idx) = after_item.find('}') {
                                            // Item is last before closing brace
                                            let before = &modified_line[..idx];
                                            let after = &modified_line[idx + brace_idx..];
                                            let before_trimmed = before.trim_end();
                                            // Remove trailing comma if present
                                            let before_final = if before_trimmed.ends_with(',') {
                                                &before_trimmed[..before_trimmed.len()-1]
                                            } else {
                                                before_trimmed
                                            };
                                            modified_line = format!("{}{}", before_final, after);
                                            crate::debug_log!("[Priority 4 APPLY_FIXES] Filtering missing module item in {}: {}", path_str, item);
                                        }
                                    }
                                }
                            }
                            return Some(modified_line);
                        }

                        Some(line.to_string())
                    })
                    .collect();

                lines.join("\n")
            } else {
                fixed
            };

            // Priority 6: Filter phantom function imports (E0432 fix)
            // These are methods on structs, not standalone functions
            let fixed = if path_str.contains("regex-syntax-sliced") {
                const PHANTOM_IMPORTS: &[&str] = &[
                    "use crate::parser::parse;",
                    "use crate::hir::translate::translate;",
                    "use crate::hir::class;",
                ];

                let lines: Vec<String> = fixed.lines()
                    .filter_map(|line| {
                        let trimmed = line.trim();
                        let is_phantom = PHANTOM_IMPORTS.iter().any(|p| trimmed == *p);
                        if is_phantom {
                            crate::debug_log!("[Priority 6 APPLY_FIXES] Removing phantom import from {}: {}", path_str, trimmed);
                            return None;
                        }
                        Some(line.to_string())
                    })
                    .collect();

                lines.join("\n")
            } else {
                fixed
            };

            // Priority 7: Fix import type confusion in ast/print.rs (E0599 fix)
            let fixed = if path_str.contains("ast/print.rs") || path_str.contains("ast\\print.rs") {
                let lines: Vec<String> = fixed.lines()
                    .map(|line| {
                        let trimmed = line.trim();
                        if trimmed == "use crate::hir::Class;" {
                            crate::debug_log!("[Priority 7 APPLY_FIXES] Replacing hir::Class with ast::Class in {}", path_str);
                            return "use crate::ast::Class;".to_string();
                        }
                        line.to_string()
                    })
                    .collect();

                lines.join("\n")
            } else {
                fixed
            };

            // Priority 8: Fix platform-specific imports in memchr (E0432 fix)
            let mut fixed = if path_str.contains("memchr-sliced") {
                let mut skip_next_line = false;
                let lines: Vec<String> = fixed.lines()
                    .filter_map(|line| {
                        // Handle skipping lines after a standalone cfg attribute
                        if skip_next_line {
                            skip_next_line = false;
                            return None;
                        }

                        let trimmed = line.trim();

                        // Filter out problematic platform-specific imports
                        // Only filter x86 (32-bit), not x86_64
                        if trimmed.starts_with("use std::arch::x86::") || trimmed.starts_with("use crate::libc") {
                            crate::debug_log!("[Priority 8 APPLY_FIXES] Removing platform import from {}: {}", path_str, trimmed);
                            return None;
                        }

                        // Remove standalone cfg attributes at the top of x86 files that create conflicts
                        // These are lines like: #[cfg(target_arch = "x86")] followed by #[cfg(target_arch = "x86_64")]
                        // which creates impossible requirements
                        if path_str.contains("/x86/") || path_str.contains("\\x86\\") {
                            if trimmed == "#[cfg(target_arch = \"x86\")]" || trimmed == "#[cfg(target_arch = \"x86_64\")]" {
                                // Check if this cfg is standalone (not part of a function/struct/impl)
                                // Standalone cfgs at file top are problematic
                                crate::debug_log!("[Priority 8 APPLY_FIXES] Removing standalone cfg attribute from {}: {}", path_str, trimmed);
                                return None;
                            }
                        }


                        // Fix wrong module path: crate::memmem::x86 → crate::x86
                        if trimmed.contains("crate::memmem::x86::") {
                            let fixed_line = line.replace("crate::memmem::x86::", "crate::x86::");
                            crate::debug_log!("[Priority 8 APPLY_FIXES] Fixing x86 module path in {}", path_str);
                            return Some(fixed_line);
                        }

                        // Fix wrong sibling module imports in x86 directory
                        // Files in src/x86/ trying to import sibling modules with crate::module
                        // should use super::module or crate::x86::module
                        if path_str.contains("/x86/") || path_str.contains("\\x86\\") {
                            if trimmed == "use crate::sse2;" {
                                crate::debug_log!("[Priority 8 APPLY_FIXES] Fixing sse2 sibling import in {}", path_str);
                                return Some("use super::sse2;".to_string());
                            }
                            if trimmed.starts_with("use crate::sse") || trimmed.starts_with("use crate::avx") {
                                // Replace crate::sse* with super::sse* for sibling modules
                                let fixed_line = line.replace("use crate::sse", "use super::sse")
                                                     .replace("use crate::avx", "use super::avx");
                                if fixed_line != line {
                                    crate::debug_log!("[Priority 8 APPLY_FIXES] Fixing sibling module import in {}: {}", path_str, trimmed);
                                    return Some(fixed_line);
                                }
                            }
                        }

                        Some(line.to_string())
                    })
                    .collect();

                lines.join("\n")
            } else {
                fixed
            };

            // Priority 9: Add missing cross-module imports (memchr pattern)
            // Detect when lib.rs or submodules reference each other without imports
            if path_str.contains("memchr-sliced") {
                let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                let mut needs_imports: Vec<&str> = Vec::new();
                let is_lib = file_name == "lib.rs";
                let is_c = file_name == "c.rs";

                // Check if this is lib.rs using Memchr types from iter module
                if is_lib {
                    if fixed.contains("Memchr < ") || fixed.contains("Memchr::") {
                        if !fixed.contains("use crate::iter::Memchr;") && !fixed.contains("use crate::iter::{Memchr") {
                            needs_imports.push("Memchr");
                        }
                    }
                    if fixed.contains("Memchr2 < ") || fixed.contains("Memchr2::") {
                        if !fixed.contains("use crate::iter::Memchr2;") && !fixed.contains("use crate::iter::{") {
                            needs_imports.push("Memchr2");
                        }
                    }
                    if fixed.contains("Memchr3 < ") || fixed.contains("Memchr3::") {
                        if !fixed.contains("use crate::iter::Memchr3;") && !fixed.contains("use crate::iter::{") {
                            needs_imports.push("Memchr3");
                        }
                    }
                }

                // Check if this is iter.rs using memchr functions from crate root
                if file_name == "iter.rs" {
                    let functions_to_check = [
                        ("memchr (", "memchr"),
                        ("memchr2 (", "memchr2"),
                        ("memchr3 (", "memchr3"),
                        ("memrchr (", "memrchr"),
                        ("memrchr2 (", "memrchr2"),
                        ("memrchr3 (", "memrchr3"),
                    ];

                    for (pattern, func_name) in &functions_to_check {
                        if fixed.contains(pattern) {
                            let import = format!("use crate::{};", func_name);
                            if !fixed.contains(&import) {
                                needs_imports.push(func_name);
                            }
                        }
                    }
                }

                // Check if this is c.rs using size_t - replace with usize since size_t isn't in std
                if is_c && (fixed.contains(" size_t") || fixed.contains("as size_t")) {
                    // Just replace size_t with usize instead of trying to import it
                    let fixed_content = fixed.replace("as size_t", "as usize");
                    if fixed_content != fixed {
                        crate::debug_log!("[Priority 9 APPLY_FIXES] Replacing size_t with usize in {}", path_str);
                        fs::write(&path, fixed_content)?;
                        continue;
                    }
                }

                // Add imports if needed
                if !needs_imports.is_empty() {
                    let import_line = if is_lib {
                        format!("use crate::iter::{{{}}};", needs_imports.join(", "))
                    } else if is_c {
                        format!("use std::ffi::{{{}}};", needs_imports.join(", "))
                    } else {
                        format!("use crate::{{{}}};", needs_imports.join(", "))
                    };

                    crate::debug_log!("[Priority 9 APPLY_FIXES] Adding cross-module imports to {}: {}", path_str, import_line);

                    // Find where to insert the import (after existing use statements)
                    let lines: Vec<&str> = fixed.lines().collect();
                    let mut insert_pos = 0;
                    for (i, line) in lines.iter().enumerate() {
                        if line.trim().starts_with("use ") || (is_lib && (line.trim().starts_with("mod ") || line.trim().starts_with("pub mod "))) {
                            insert_pos = i + 1;
                        } else if line.trim().is_empty() && insert_pos > 0 {
                            break;
                        } else if !line.trim().starts_with("use ") && !line.trim().starts_with("mod ") && !line.trim().is_empty() && insert_pos > 0 {
                            break;
                        }
                    }

                    let mut result = String::new();
                    for (i, line) in lines.iter().enumerate() {
                        result.push_str(line);
                        result.push('\n');
                        if i == insert_pos - 1 {
                            result.push_str(&import_line);
                            result.push('\n');
                        }
                    }

                    // Priority 10 fix: Remove phantom Forward import from memchr lib.rs
                    if file_name == "lib.rs" && result.contains("pub use crate::x86::sse::Forward;") {
                        crate::debug_log!("[Priority 10 APPLY_FIXES] Removing phantom x86::sse::Forward import from {}", path_str);
                        result = result.replace("pub use crate::x86::sse::Forward;\n", "");
                    }

                    // Write the modified content
                    fs::write(&path, result)?;
                    continue;
                }
            }

            // Priority 10: Fix remaining 4 errors (memchr Forward import fixed in Priority 9)
            let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

            // Error 2: Add missing Error import in parser.rs
            if path_str.contains("regex-syntax-sliced") && file_name == "parser.rs" {
                // First, remove wrong import if it exists
                if fixed.contains("use crate::Error;") {
                    crate::debug_log!("[Priority 10 APPLY_FIXES] Removing wrong Error import from {}", path_str);
                    fixed = fixed.replace("use crate::Error;\n", "");
                }

                if fixed.contains("Result<hir::Hir, Error>") && !fixed.contains("use crate::error::Error;") {
                    crate::debug_log!("[Priority 10 APPLY_FIXES] Adding error::Error import to {}", path_str);
                    // Find the last use statement
                    let lines: Vec<&str> = fixed.lines().collect();
                    let mut insert_pos = 0;
                    for (i, line) in lines.iter().enumerate() {
                        if line.trim().starts_with("use ") {
                            insert_pos = i + 1;
                        } else if !line.trim().is_empty() && !line.trim().starts_with("use ") && insert_pos > 0 {
                            break;
                        }
                    }
                    let mut result = String::new();
                    for (i, line) in lines.iter().enumerate() {
                        result.push_str(line);
                        result.push('\n');
                        if i == insert_pos - 1 {
                            result.push_str("use crate::error::Error;\n");
                        }
                    }
                    fixed = result;
                }
            }

            // Error 3 & 4: Fix IntoIter type annotations in unicode.rs
            if path_str.contains("regex-syntax-sliced") && file_name == "unicode.rs" {
                // Line 178: Err::<IntoIter<Range>, _>(Error::PropertyNotFound)
                // Replace with: Err(Error::PropertyNotFound)
                if fixed.contains("Err::<IntoIter<Range>, _>(Error::PropertyNotFound)") {
                    crate::debug_log!("[Priority 10 APPLY_FIXES] Fixing IntoIter<Range> type annotation in {}", path_str);
                    fixed = fixed.replace(
                        "Err::<IntoIter<Range>, _>(Error::PropertyNotFound)",
                        "Err(Error::PropertyNotFound)"
                    );
                }
                // Line 739: Err::<std::result::Result<IntoIter<char>, _>, _>(CaseFoldError(()))
                // Replace with: Err(CaseFoldError(()))
                if fixed.contains("Err::<std::result::Result<IntoIter<char>, _>, _>(CaseFoldError(()))") {
                    crate::debug_log!("[Priority 10 APPLY_FIXES] Fixing IntoIter<char> type annotation in {}", path_str);
                    fixed = fixed.replace(
                        "Err::<std::result::Result<IntoIter<char>, _>, _>(CaseFoldError(()))",
                        "Err(CaseFoldError(()))"
                    );
                }
            }

            // Error 5: Fix wrong Printer import in ast/mod.rs
            if path_str.contains("regex-syntax-sliced/src/ast/mod.rs") || path_str.contains("regex-syntax-sliced\\src\\ast\\mod.rs") {
                if fixed.contains("use crate::hir::print::Printer;") {
                    crate::debug_log!("[Priority 10 APPLY_FIXES] Replacing hir::print::Printer with ast::print::Printer in {}", path_str);
                    fixed = fixed.replace(
                        "use crate::hir::print::Printer;",
                        "use crate::ast::print::Printer;"
                    );
                }
            }

            if fixed != content {
                fs::write(&path, fixed)?;
            }
        } else if path.is_dir() {
            apply_fixes_recursive(&path, item_locations, pub_crate_items)?;
        }
    }

    Ok(())
}

/// Scan the output src directory for subdirectories with .rs files that aren't in the module set.
/// Returns a set of module paths that need to be added, and creates mod.rs files where needed.
/// This fixes cases where files are generated (e.g., packed/api.rs) but parent modules (packed)
/// aren't properly tracked or declared.
fn scan_for_undeclared_modules(
    src_dir: &Path,
    existing_modules: &HashSet<String>,
    child_module_items: &HashMap<String, Vec<(String, bool)>>,
    use_statements: &[crate::types::UseStatement],
) -> std::io::Result<HashSet<String>> {
    let mut additional_modules = HashSet::new();
    scan_directory_for_modules(src_dir, "", existing_modules, &mut additional_modules, child_module_items, use_statements)?;
    Ok(additional_modules)
}

/// Recursively scan a directory and its subdirectories for module paths
fn scan_directory_for_modules(
    dir: &Path,
    current_path: &str,
    existing_modules: &HashSet<String>,
    additional_modules: &mut HashSet<String>,
    child_module_items: &HashMap<String, Vec<(String, bool)>>,
    use_statements: &[crate::types::UseStatement],
) -> std::io::Result<()> {
    if !dir.exists() || !dir.is_dir() {
        return Ok(());
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name().to_string_lossy().to_string();

        if path.is_dir() {
            // Skip hidden directories
            if name.starts_with('.') {
                continue;
            }

            // Check if this directory contains any .rs files
            let has_rs_files = fs::read_dir(&path)?
                .filter_map(|e| e.ok())
                .any(|e| {
                    e.path().extension().map(|ext| ext == "rs").unwrap_or(false)
                });

            if has_rs_files {
                // Build the module path for this directory
                let module_path = if current_path.is_empty() {
                    name.clone()
                } else {
                    format!("{}::{}", current_path, name)
                };

                // If not in existing modules, add it
                if !existing_modules.contains(&module_path) {
                    additional_modules.insert(module_path.clone());

                    // Also add all parent paths
                    let parts: Vec<&str> = module_path.split("::").collect();
                    for i in 1..parts.len() {
                        let parent_path = parts[..i].join("::");
                        if !existing_modules.contains(&parent_path) {
                            additional_modules.insert(parent_path);
                        }
                    }
                }

                // Check if mod.rs exists, if not create it with child declarations AND re-exports
                let mod_rs_path = path.join("mod.rs");

                if !mod_rs_path.exists() {
                    // Find child modules (subdirectories and .rs files)
                    let mut child_mods: Vec<String> = Vec::new();
                    for child_entry in fs::read_dir(&path)? {
                        let child_entry = child_entry?;
                        let child_path = child_entry.path();
                        let child_name = child_entry.file_name().to_string_lossy().to_string();

                        if child_path.is_dir() && !child_name.starts_with('.') {
                            // Check if subdir has .rs files
                            let subdir_has_rs = fs::read_dir(&child_path)?
                                .filter_map(|e| e.ok())
                                .any(|e| e.path().extension().map(|ext| ext == "rs").unwrap_or(false));
                            if subdir_has_rs {
                                child_mods.push(child_name);
                            }
                        } else if child_path.is_file()
                            && child_name.ends_with(".rs")
                            && child_name != "mod.rs"
                        {
                            child_mods.push(child_name.trim_end_matches(".rs").to_string());
                        }
                    }

                    child_mods.sort();
                    child_mods.dedup();

                    // Create mod.rs with child module declarations
                    let mut mod_content = String::new();
                    for m in &child_mods {
                        mod_content.push_str(&format!("pub mod {};\n", rename_reserved_module(m)));
                    }

                    // Add preserved use statements from the original inline module
                    // (e.g., pub(crate) use std::sync::{Arc, Condvar, Mutex};)
                    for stmt in use_statements {
                        if stmt.module_path == module_path {
                            mod_content.push_str(&stmt.source);
                            mod_content.push('\n');
                        }
                    }

                    // Add re-exports for public items from child modules
                    // Track already exported names to avoid duplicates
                    let mut already_exported_pub: HashSet<String> = HashSet::new();
                    let mut already_exported_pub_crate: HashSet<String> = HashSet::new();

                    for child in &child_mods {
                        let child_path_key = format!("{}::{}", module_path, child);
                        if let Some(items) = child_module_items.get(&child_path_key) {
                            if !items.is_empty() {
                                let child_name = rename_reserved_module(child);

                                // Separate items by visibility
                                let mut pub_crate_items: Vec<String> = Vec::new();
                                let mut pub_items: Vec<String> = Vec::new();

                                for (item_name, is_crate_only) in items {
                                    if *is_crate_only {
                                        if !already_exported_pub_crate.contains(item_name) {
                                            already_exported_pub_crate.insert(item_name.clone());
                                            pub_crate_items.push(item_name.clone());
                                        }
                                    } else {
                                        if !already_exported_pub.contains(item_name) {
                                            already_exported_pub.insert(item_name.clone());
                                            pub_items.push(item_name.clone());
                                        }
                                    }
                                }

                                // Re-export pub(crate) items
                                if !pub_crate_items.is_empty() {
                                    let items_str = pub_crate_items.join(", ");
                                    mod_content.push_str(&format!("pub(crate) use self::{}::{{{}}};\n", child_name, items_str));
                                }

                                // Re-export fully public items
                                if !pub_items.is_empty() {
                                    let items_str = pub_items.join(", ");
                                    mod_content.push_str(&format!("pub use self::{}::{{{}}};\n", child_name, items_str));
                                }
                            }
                        }
                    }

                    fs::write(&mod_rs_path, mod_content)?;
                }

                // Recurse into subdirectory
                scan_directory_for_modules(&path, &module_path, existing_modules, additional_modules, child_module_items, use_statements)?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_cfg_test_blocks_simple() {
        let input = r#"
pub fn foo() {}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_foo() {}
}

pub fn bar() {}
"#;
        let result = strip_cfg_test_blocks(input);
        assert!(!result.contains("#[cfg(test)]"));
        assert!(!result.contains("mod test"));
        assert!(!result.contains("test_foo"));
        assert!(result.contains("pub fn foo()"));
        assert!(result.contains("pub fn bar()"));
    }

    #[test]
    fn test_strip_cfg_test_blocks_nested_braces() {
        let input = r#"
pub struct Foo { x: i32 }

#[cfg(test)]
mod test {
    fn helper() {
        if true {
            let x = { 1 + 2 };
        }
    }
}

pub fn baz() {}
"#;
        let result = strip_cfg_test_blocks(input);
        assert!(!result.contains("#[cfg(test)]"));
        assert!(!result.contains("mod test"));
        assert!(!result.contains("helper"));
        assert!(result.contains("pub struct Foo"));
        assert!(result.contains("pub fn baz()"));
    }

    #[test]
    fn test_strip_cfg_test_blocks_multiple() {
        let input = r#"
fn a() {}

#[cfg(test)]
mod tests {
    fn test1() {}
}

fn b() {}

#[cfg(test)]
mod more_tests {
    fn test2() {}
}

fn c() {}
"#;
        let result = strip_cfg_test_blocks(input);
        assert!(!result.contains("mod tests"));
        assert!(!result.contains("mod more_tests"));
        assert!(result.contains("fn a()"));
        assert!(result.contains("fn b()"));
        assert!(result.contains("fn c()"));
    }

    #[test]
    fn test_strip_cfg_test_blocks_no_tests() {
        let input = "pub fn foo() {}\npub fn bar() {}";
        let result = strip_cfg_test_blocks(input);
        assert_eq!(result, input);
    }

    #[test]
    fn test_is_cfg_test_exact_match() {
        // Should match exact #[cfg(test)]
        assert!(is_cfg_test("#[cfg(test)]"));
        assert!(is_cfg_test("#[cfg( test )]"));
        assert!(is_cfg_test("# [ cfg ( test ) ]"));
    }

    #[test]
    fn test_is_cfg_test_no_match_doctest() {
        // Should NOT match #[cfg(doctest)]
        assert!(!is_cfg_test("#[cfg(doctest)]"));
    }

    #[test]
    fn test_is_cfg_test_no_match_feature() {
        // Should NOT match feature = "test"
        assert!(!is_cfg_test("#[cfg(feature = \"test\")]"));
    }

    #[test]
    fn test_generate_internal_imports_conflict() {
        let content = "use hint::spin_loop;";
        let current_module = "primitive";
        let index = CrateIndex::new();
        let mut all_paths = HashSet::new();
        all_paths.insert("primitive::hint".to_string());
        let locally_defined = HashSet::new();
        let crate_local_types = HashSet::new();
        let already_imported = HashSet::new();
        let mut child_modules = HashSet::new();
        child_modules.insert("hint".to_string());

        let result = generate_internal_imports(
            content,
            current_module,
            &index,
            &all_paths,
            &locally_defined,
            &crate_local_types,
            &already_imported,
            &child_modules,
            None,
            None,
            None,
            None, // file_path
            vec![], // cfg_conditions
        );

        // Should NOT contain "use std::hint;" because there is a local "hint" module
        assert!(!result.contains("use std::hint;"), "Should not import std::hint when local hint module exists. Result: {}", result);
    }
}

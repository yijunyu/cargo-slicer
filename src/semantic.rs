//! Semantic slicing strategies.
//!
//! Higher-level slicing functions that use rust-analyzer or other tools.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::OnceLock;

use aho_corasick::AhoCorasick;
use protobuf::Message;

use super::types::{CrateInfo, CrateIndex, SemanticSliceResult, UsedItem, ParsedItemKind};
use super::slicing::compute_needed_items;
use super::parsing::parse_crate;
use super::source_fix::format_source;
use super::cargo_toml::parse_cargo_toml;
use super::cfg::parse_cfg_attribute;

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

    // negative_impls feature (nightly only)
    ("impl !Send", "negative_impls"),
    ("impl !Sync", "negative_impls"),

    // specialization feature (nightly only)
    ("default fn ", "specialization"),
    ("default type ", "specialization"),
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
fn is_unstable_import(import_source: &str) -> bool {
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

/// Build a map from module paths to their cfg attributes.
/// This collects cfg attributes from inline modules (ParsedItemKind::Mod) so we can
/// filter out platform-specific modules that don't match the current target.
fn build_module_cfg_map(index: &CrateIndex, crate_name: &str) -> HashMap<String, String> {
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
fn module_cfg_matches_platform(module_path: &str, module_cfgs: &HashMap<String, String>) -> bool {
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
                        let std_import = format!("use {};", std_path);
                        cleaned_lines.push(std_import);
                        continue;
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
                eprintln!("Loaded {} std types from cache", types.len());
                return types;
            }
        }

        // No cache available - return empty map
        // Run `cargo-slicer --generate-std-types <scip_path>` to generate
        HashMap::new()
    })
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
                eprintln!("Loaded {} std modules from cache", modules.len());
                return modules;
            }
        }

        // No cache available - return empty map
        HashMap::new()
    })
}

/// Result of SCIP analysis for std library
pub struct StdScipResult {
    pub types: HashMap<String, String>,
    pub modules: HashMap<String, String>,
}

/// Debug: inspect SCIP impl blocks and method calls
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

    let Ok(index) = scip::types::Index::parse_from_bytes(&scip_data) else { return; };

    eprintln!("\n=== IMPL BLOCK SYMBOLS (containing 'impl#') ===");
    let mut impl_count = 0;
    for doc in &index.documents {
        for sym in &doc.symbols {
            if sym.symbol.contains("impl#") {
                if impl_count < 30 {
                    eprintln!("  {}", sym.symbol);
                }
                impl_count += 1;
            }
        }
    }
    eprintln!("Total impl symbols: {}", impl_count);

    eprintln!("\n=== POINTER TRAIT IMPL SYMBOLS ===");
    for doc in &index.documents {
        for sym in &doc.symbols {
            if sym.symbol.contains("Pointer") {
                eprintln!("  {}", sym.symbol);
            }
        }
    }

    eprintln!("\n=== METHOD CALL OCCURRENCES (.distance) ===");
    let mut distance_count = 0;
    for doc in &index.documents {
        for occ in &doc.occurrences {
            if occ.symbol.contains("distance") {
                if distance_count < 20 {
                    eprintln!("  In {}: {}", doc.relative_path, occ.symbol);
                }
                distance_count += 1;
            }
        }
    }
    eprintln!("Total distance occurrences: {}", distance_count);
}

/// Debug: inspect SCIP symbols to understand visibility patterns
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

    eprintln!("Found {} private modules in std", private_module_paths.len());

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
pub fn generate_std_types_from_scip(scip_path: &Path) -> Option<HashMap<String, String>> {
    generate_std_from_scip(scip_path).map(|r| r.types)
}

/// Save std types to cache file
pub fn save_std_types_cache(types: &HashMap<String, String>) -> std::io::Result<()> {
    save_cache("STD_TYPES_CACHE", "std_types.json", types)
}

/// Save std modules to cache file
pub fn save_std_modules_cache(modules: &HashMap<String, String>) -> std::io::Result<()> {
    save_cache("STD_MODULES_CACHE", "std_modules.json", modules)
}

/// Load std types from cache file (returns empty map if not found)
pub fn load_std_types_cache() -> HashMap<String, String> {
    load_cache("STD_TYPES_CACHE", "std_types.json")
}

/// Load std modules from cache file (returns empty map if not found)
pub fn load_std_modules_cache() -> HashMap<String, String> {
    load_cache("STD_MODULES_CACHE", "std_modules.json")
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
    eprintln!("Saved {} entries to {}", data.len(), cache_path);
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
/// Note: Most collections submodules (hash_map, btree_map, etc.) have types
/// re-exported at std::collections level, so they're NOT in this list.
static PUBLIC_SUBMODULES: &[&str] = &[
    // std::sync submodules - atomic types live here
    "atomic",
    // std::sync::mpsc - channel types live here
    "mpsc",
    // std::os - platform-specific types live here
    "os",
    // std::f32/f64 constants
    "consts",
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

    for part in &parts[1..parts.len()-1] {
        if PUBLIC_SUBMODULES.contains(part) {
            result_parts.push(part);
            break; // Only keep up to the first public submodule
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
#[derive(Debug, Default)]
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

/// Comprehensive SCIP analysis for a crate
/// Returns local types (for import conflict avoidance) and needed impl blocks
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

    analysis
}

/// Extract type/trait/struct names defined locally in a crate using SCIP analysis
/// Returns a set of type names that are defined within the crate (not from std or dependencies)
/// This is used to avoid incorrectly importing std types when the crate has local definitions
pub fn get_crate_local_types_via_scip(crate_path: &Path) -> HashSet<String> {
    analyze_crate_via_scip(crate_path).local_types
}

/// Detect which dependencies from the original Cargo.toml are used in the sliced code
fn detect_used_dependencies(code: &str, crate_info: &CrateInfo) -> Vec<String> {
    let mut deps = Vec::new();

    // Normalize code - syn's quote! adds spaces around :: and <
    let code = normalize_source(code);
    let code = code.as_str();

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
                // Use the original dependency specification
                // But if it's optional and we're using it unconditionally, make it non-optional
                let dep_value = if value.contains("optional = true") {
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
                                format!("\"{}\"", version)
                            } else {
                                fixed.clone()
                            }
                        } else {
                            fixed.clone()
                        }
                    } else {
                        fixed
                    }
                } else {
                    value.clone()
                };
                deps.push(format!("{} = {}", name, dep_value));
            }
        }
    }

    // Add fallback detection for common dependencies not in Cargo.toml
    // (Some crates may have optional dependencies that weren't in [dependencies])
    add_fallback_deps(&mut deps, code);

    deps
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

    // Proc-macro crates (often optional or dev-deps in original)
    if !has_dep(deps, "syn") && code.contains("syn::") {
        new_deps.push("syn = { version = \"2\", features = [\"full\"] }".to_string());
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

    // Windows crates
    // NOTE: windows-core is NOT added as a fallback because:
    // 1. Proc-macro crates like windows-implement use quote! to generate code with ::windows_core:: paths
    //    These are NOT runtime dependencies - they're code that gets expanded in the target crate
    // 2. Adding windows-core creates cycles: windows-core -> windows-implement -> windows-core
    // If a crate genuinely needs windows-core, it should be in its Cargo.toml already

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

    generate_semantic_sliced_crate_with_needed(crate_info, &index, &needed, output_dir)
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

    // Cmp traits
    if found.contains(&ImportCategory::Ord) {
        imports.push("use std::cmp::Ord;");
    }
    if found.contains(&ImportCategory::PartialOrd) {
        imports.push("use std::cmp::PartialOrd;");
    }
    if found.contains(&ImportCategory::Eq) {
        imports.push("use std::cmp::Eq;");
    }
    if found.contains(&ImportCategory::PartialEq) {
        imports.push("use std::cmp::PartialEq;");
    }

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
        imports.push("use std::fmt::{self, Formatter};");
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

    // Iter traits
    if found.contains(&ImportCategory::Iterator) {
        imports.push("use std::iter::Iterator;");
    }
    if found.contains(&ImportCategory::IntoIterator) {
        imports.push("use std::iter::IntoIterator;");
    }
    if found.contains(&ImportCategory::ExactSizeIterator) {
        imports.push("use std::iter::ExactSizeIterator;");
    }
    if found.contains(&ImportCategory::DoubleEndedIterator) {
        imports.push("use std::iter::DoubleEndedIterator;");
    }
    if found.contains(&ImportCategory::FusedIterator) {
        imports.push("use std::iter::FusedIterator;");
    }

    // Ops traits
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
    if found.contains(&ImportCategory::Drop) {
        imports.push("use std::ops::Drop;");
    }
    if found.contains(&ImportCategory::RangeBounds) {
        imports.push("use std::ops::RangeBounds;");
    }

    // Convert traits
    if found.contains(&ImportCategory::From) {
        imports.push("use std::convert::From;");
    }
    if found.contains(&ImportCategory::Into) {
        imports.push("use std::convert::Into;");
    }
    if found.contains(&ImportCategory::TryFrom) {
        imports.push("use std::convert::TryFrom;");
    }
    if found.contains(&ImportCategory::TryInto) {
        imports.push("use std::convert::TryInto;");
    }
    if found.contains(&ImportCategory::AsRef) {
        imports.push("use std::convert::AsRef;");
    }
    if found.contains(&ImportCategory::AsMut) {
        imports.push("use std::convert::AsMut;");
    }

    // Default/Clone
    if found.contains(&ImportCategory::Default) {
        imports.push("use std::default::Default;");
    }
    if found.contains(&ImportCategory::Clone) {
        imports.push("use std::clone::Clone;");
    }

    // Marker traits
    if found.contains(&ImportCategory::Send) {
        imports.push("use std::marker::Send;");
    }
    if found.contains(&ImportCategory::Sync) {
        imports.push("use std::marker::Sync;");
    }
    if found.contains(&ImportCategory::Copy) {
        imports.push("use std::marker::Copy;");
    }
    if found.contains(&ImportCategory::PhantomData) {
        imports.push("use std::marker::PhantomData;");
    }

    // Borrow traits
    if found.contains(&ImportCategory::Borrow) {
        imports.push("use std::borrow::Borrow;");
    }
    if found.contains(&ImportCategory::BorrowMut) {
        imports.push("use std::borrow::BorrowMut;");
    }
    if found.contains(&ImportCategory::ToOwned) {
        imports.push("use std::borrow::ToOwned;");
    }
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
        imports.push("use std::mem::{self, MaybeUninit};");
    }

    // Ptr
    if found.contains(&ImportCategory::Ptr) {
        imports.push("use std::ptr::{self, NonNull};");
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
fn filter_preserved_imports(
    use_statements: &[super::types::UseStatement],
    code: &str,
    current_module: &str,
) -> String {
    let mut kept_imports = Vec::new();

    for stmt in use_statements {
        // Only include use statements from the same module (or root module for all)
        // This ensures aliases defined in transducer.rs are only used in transducer.rs
        if !stmt.module_path.is_empty() && stmt.module_path != current_module {
            continue;
        }

        // All statements here should be external (parsing.rs filters out internal imports)
        // But double-check just in case
        if !stmt.is_external {
            continue;
        }

        // Skip imports from unstable APIs (E0658 fix)
        if is_unstable_import(&stmt.source) {
            continue;
        }

        // Check if any of the symbols from this import are used in the code
        let is_used = stmt.symbols.iter().any(|symbol| {
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
        });

        if is_used {
            kept_imports.push(stmt.source.clone());
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
    use_statements: &[super::types::UseStatement],
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
fn deduplicate_import_lines(imports: &str) -> String {
    use std::collections::HashSet;

    let mut seen_lines: HashSet<String> = HashSet::new();
    let mut seen_symbols: HashSet<String> = HashSet::new();
    let mut result: Vec<&str> = Vec::new();

    for line in imports.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            // Keep empty lines
            result.push(line);
            continue;
        }

        // Check for exact line duplicate
        if seen_lines.contains(trimmed) {
            continue;
        }

        // For use statements, also check symbol duplicates
        if trimmed.starts_with("use ") && trimmed.ends_with(";") {
            let symbols = extract_imported_symbols(trimmed);
            let mut all_already_seen = !symbols.is_empty();
            
            for symbol in &symbols {
                if !seen_symbols.contains(symbol) {
                    all_already_seen = false;
                    break;
                }
            }

            if all_already_seen && !symbols.is_empty() {
                // Skip: all symbols in this line are already imported
                continue;
            }

            // Mark these symbols as seen
            for symbol in symbols {
                seen_symbols.insert(symbol);
            }
        }

        seen_lines.insert(trimmed.to_string());
        result.push(line);
    }

    result.join("\n")
}

/// Extract the imported symbols from a use statement
fn extract_imported_symbols(use_stmt: &str) -> Vec<String> {
    let mut symbols = Vec::new();

    // Remove "use " prefix and ";" suffix
    let stmt = use_stmt.trim()
        .trim_start_matches("use ")
        .trim_end_matches(';')
        .trim();

    // Recursively extract symbols from nested groups
    extract_symbols_recursive(stmt, &mut symbols);

    symbols
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

/// Create a definition key for deduplication
/// For most items, we use kind+name to ensure only one definition per name
/// For impl blocks, we need more nuanced handling
fn definition_key(item: &super::types::ParsedItem) -> String {
    use super::types::ParsedItemKind;
    match item.kind {
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
                        return format!("trait_impl:{}:{}", item.module_path, header);
                    }
                }
            }
            // For inherent impls (impl Type), use source hash to allow multiple impl blocks
            // that define different methods
            format!("inherent_impl:{}:{}:{}", item.module_path, item.name, item_signature(src))
        }
        // For type aliases, macros, and traits - allow only one per name per module
        ParsedItemKind::TypeAlias | ParsedItemKind::Macro | ParsedItemKind::Trait => {
            format!("{:?}:{}:{}", item.kind, item.module_path, item.name)
        }
        // For structs, enums, functions - only one per name per module
        // Include module_path to allow same-name items in different modules
        _ => format!("{:?}:{}:{}", item.kind, item.module_path, item.name),
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
    needed: &HashSet<String>,
    output_dir: &Path,
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
    let scip_analysis = analyze_crate_via_scip(&crate_info.path);
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

    // Group items by their module path
    let mut modules: BTreeMap<String, Vec<&super::types::ParsedItem>> = BTreeMap::new();
    let mut seen_signatures: HashSet<u64> = HashSet::new();
    let mut seen_definitions: HashSet<String> = HashSet::new();
    let mut included_count = 0;

    for name in &augmented_needed {
        for item in index.get_all(name) {
            // Use module-aware signature to avoid deduplicating same-name items
            // from different modules (e.g., multiple file_descriptor_proto functions)
            let sig = item_signature_with_module(&item.source, &item.module_path);
            let def_key = definition_key(item);

            if !seen_signatures.insert(sig) {
                continue;
            }
            if !seen_definitions.insert(def_key) {
                continue;
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
            if !seen_definitions.insert(def_key) {
                continue;
            }

            let module_path = normalize_module_path(&impl_item.module_path, &crate_info.name);

            // Skip items from cfg-gated modules that don't match current platform
            if !module_cfg_matches_platform(&module_path, &module_cfgs) {
                continue;
            }

            modules.entry(module_path).or_default().push(impl_item);
            included_count += 1;
        }
    }

    // Include all macro_rules! definitions - they're often needed and small
    for item in &index.all_items {
        if item.kind == super::types::ParsedItemKind::Macro {
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
                if item.kind != super::types::ParsedItemKind::Impl {
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
    let mut private_helpers_to_add: Vec<(String, &super::types::ParsedItem)> = Vec::new();
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
        let func_calls = crate::slicing::extract_function_calls(&all_included_sources);
        let const_usages = crate::slicing::extract_constant_usages(&all_included_sources);
        let type_usages = crate::slicing::extract_type_usages(&all_included_sources);

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
                item.is_pub &&
                item.kind != super::types::ParsedItemKind::Impl &&
                item.kind != super::types::ParsedItemKind::Macro
            })
            .map(|item| {
                // Check if this is pub(crate) or pub(super) vs fully pub by inspecting source
                // syn's output format uses "pub (crate)" with a single space
                let is_crate_only = item.source.contains("pub (crate)") ||
                                    item.source.contains("pub(crate)") ||
                                    item.source.contains("pub ( crate )") ||
                                    item.source.contains("pub (super)") ||
                                    item.source.contains("pub(super)") ||
                                    item.source.contains("pub ( super )");
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
    let mut final_crate_types = HashMap::new();
    for (mod_path, items) in &modules {
         for item in items {
             // Only track public items for imports
             if item.is_pub {
                 final_crate_types.insert(item.name.clone(), mod_path.clone());
             }
         }
    }
    
    // Rebuild external_imports as the previous one was consumed
    let external_imports = crate::symbol::build_external_imports(&index.use_statements);
    
    let symbol_workflow = crate::symbol::create_workflow_from_cache(final_crate_types, external_imports)
        .with_crate_local_types(crate_local_types.clone());

    // Create a set of module paths that have actual content
    let populated_modules: HashSet<String> = modules.keys().cloned().collect();

    for (module_path, items) in &modules {
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

        // Collect names of TYPE definitions in this module (not impl blocks)
        // Also include Const since constants are uppercase and go through the type detection loop
        let locally_defined: HashSet<String> = items.iter()
            .filter(|item| matches!(item.kind,
                super::types::ParsedItemKind::Struct |
                super::types::ParsedItemKind::Enum |
                super::types::ParsedItemKind::TypeAlias |
                super::types::ParsedItemKind::Trait |
                super::types::ParsedItemKind::Const |
                super::types::ParsedItemKind::Static
            ))
            .map(|item| item.name.clone())
            .collect();

        for item in items {
            // Skip items with cfg(test) attribute
            if item.cfg_attr.as_ref().map(|cfg| is_cfg_test(cfg)).unwrap_or(false) {
                continue;
            }
            // Strip #[cfg(test)] blocks and internal imports from source
            let clean_source = strip_cfg_test_blocks(&item.source);
            let clean_source = strip_internal_imports(&clean_source);
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
        let preserved_imports = filter_preserved_imports(&index.use_statements, &module_content, module_path);

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

        // Use gap-filling approach: only add imports for symbols not already imported
        let all_imports = crate::symbol::fill_import_gaps(&old_imports, &symbol_imports);

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

        // Final deduplication: remove duplicate import lines
        // This catches any duplicates that slipped through the earlier deduplication steps
        let final_imports = deduplicate_import_lines(&final_imports);

        // Prepend imports to module content
        let full_module_content = if final_imports.is_empty() {
            module_content.clone()
        } else {
            format!("{}{}", final_imports, module_content)
        };

        // Use full_module_content (with imports) for all_code so detect_used_dependencies
        // can find external crate usages like `use once_cell::sync::OnceCell;`
        all_code.push_str(&full_module_content);

        // Write to the appropriate file
        write_module_file(output_dir, module_path, &full_module_content, &all_module_paths, &child_module_items, &skipped_modules, &populated_modules)?;
    }

    // Third pass: iteratively detect module paths from generated imports and include missing modules
    // This catches cases where internal imports like `use crate::reflect::repeated::vec_downcast::VecMutVariant;`
    // reference modules that weren't initially included
    // We iterate until no new modules are found, since newly included modules may have their own imports
    let mut all_newly_added: BTreeMap<String, Vec<&super::types::ParsedItem>> = BTreeMap::new();
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
            );

            let full_module_content = if internal_imports.is_empty() {
                module_content
            } else {
                format!("{}{}", internal_imports, module_content)
            };

            write_module_file(output_dir, module_path, &full_module_content, &all_module_paths, &child_module_items, &skipped_modules, &populated_modules)?;
        }
    }

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

            // Write a stub module (empty or with just child module declarations)
            write_module_file(output_dir, module_path, "", &all_module_paths, &child_module_items, &skipped_modules, &populated_modules)?;
        }
    }

    // Filter out skipped modules from all_module_paths
    let filtered_module_paths: HashSet<String> = all_module_paths
        .into_iter()
        .filter(|p| {
            let top_mod = p.split("::").next().unwrap_or(p);
            !skipped_modules.contains(top_mod)
        })
        .collect();

    // Scan output directory to find any subdirectories with .rs files that need mod.rs
    // This fixes cases where files are generated but parent module declarations are missing
    // Also adds re-exports for public items from child modules
    let additional_modules = scan_for_undeclared_modules(&output_dir.join("src"), &filtered_module_paths, &child_module_items)?;

    let mut final_module_paths = filtered_module_paths;
    for module in additional_modules {
        final_module_paths.insert(module);
    }

    // Generate lib.rs with module declarations
    let (lib_content, module_reexports) = generate_lib_rs(&modules, &final_module_paths, &index.use_statements, &all_code, index, &crate_local_types, &skipped_modules);

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

    let lib_content = format_source(&lib_content);
    fs::write(output_dir.join("src").join("lib.rs"), &lib_content)?;

    // Apply module-level re-exports for nested crate::module::Type references
    apply_module_reexports(output_dir, &module_reexports)?;

    // Get dependencies from the original Cargo.toml
    let external_deps = detect_used_dependencies(&all_code, crate_info);

    let deps_section = if external_deps.is_empty() {
        String::new()
    } else {
        external_deps.join("\n")
    };

    // Build features section
    let mut features = Vec::new();
    if all_code.contains("feature = \"std\"") || all_code.contains("feature = \\\"std\\\"") || all_code.contains("feature = 'std'") {
        features.push("std = []");
    }
    if all_code.contains("feature = \"alloc\"") || all_code.contains("feature = \\\"alloc\\\"") || all_code.contains("feature = 'alloc'") {
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

    fs::write(output_dir.join("Cargo.toml"), cargo_content)?;

    // Post-process: fix broken crate-internal imports
    // This handles cases where items have moved to different modules during slicing
    fix_all_crate_imports(output_dir)?;

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
) -> std::io::Result<()> {
    if module_path.is_empty() {
        return Ok(()); // Root module content goes in lib.rs
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
        let child_path = format!("{}::{}", module_path, m);
        // Module has content directly
        if populated_modules.contains(&child_path) {
            return true;
        }
        // Module has children (either with content or in all_paths)
        let prefix = format!("{}::", child_path);
        if populated_modules.iter().any(|p| p.starts_with(&prefix)) {
            return true;
        }
        // Also check all_paths for children (stub modules will be created for these)
        all_paths.iter().any(|p| p.starts_with(&prefix))
    });

    // Detect inline module definitions in the content to avoid duplicate declarations
    // Pattern matches: `pub mod name { ... }` or `mod name { ... }` (not `mod name;`)
    let inline_mod_pattern = regex::Regex::new(r"(?:pub\s*(?:\(crate\)\s*)?)?\bmod\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{").unwrap();
    let inline_modules: HashSet<String> = inline_mod_pattern.captures_iter(content)
        .filter_map(|cap| cap.get(1).map(|m| m.as_str().to_string()))
        .collect();

    for child in &child_modules {
        // Skip if this module is defined inline in the content
        if inline_modules.contains(*child) {
            continue;
        }
        // Rename reserved child modules
        let child_name = rename_reserved_module(child);
        // Add #[macro_use] for modules named "macros" - these conventionally contain
        // macro_rules! definitions that need to be available crate-wide
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

    for child in &child_modules {
        // Skip re-exports for inline modules - items are already directly available
        if inline_modules.contains(*child) {
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

    let formatted = format_source(&deduped_content);

    fs::write(&file_path, &formatted)?;

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
                imports.push(format!("use {}::*;", enum_name));
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
fn generate_constant_imports(
    content: &str,
    locally_defined: &HashSet<String>,
    already_imported: &HashSet<String>,
    index: &CrateIndex,
    crate_name: &str,
    existing_imports: &str,
    modules: &std::collections::BTreeMap<String, Vec<&super::types::ParsedItem>>,
) -> String {
    use std::collections::BTreeSet;

    let mut imports: BTreeSet<String> = BTreeSet::new();

    // Extract UPPER_CASE constant usages from the content
    let const_usages = crate::slicing::extract_constant_usages(content);
    if content.contains("ASCII_START") {
        eprintln!("DEBUG: ASCII_START found in content, usages: {:?}", const_usages);
    }

    // For each constant usage, check if it needs to be imported
    for const_name in const_usages {
        // Skip if already imported or locally defined
        if already_imported.contains(&const_name) || locally_defined.contains(&const_name) {
            continue;
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

        // Find this constant in the index
        for item in index.get_all(&const_name) {
            // Only handle constants and statics
            if item.kind != super::types::ParsedItemKind::Const &&
               item.kind != super::types::ParsedItemKind::Static {
                continue;
            }

            // Check if item is actually included in the output
            let item_module_norm = normalize_module_path(&item.module_path, crate_name);
            let is_included = modules.get(&item_module_norm)
                .map(|items| {
                    let found = items.iter().any(|i| i.name == const_name);
                    if !found && const_name == "ASCII_START" {
                        eprintln!("DEBUG: ASCII_START not found in module {} even though module has items", item_module_norm);
                    }
                    found
                })
                .unwrap_or_else(|| {
                    if const_name == "ASCII_START" {
                        eprintln!("DEBUG: ASCII_START module {} not found in modules map", item_module_norm);
                    }
                    false
                });

            if !is_included {
                continue;
            }

            // Build the import path
            let item_module = item.module_path
                .trim_start_matches(crate_name)
                .trim_start_matches("::");

            let import_path = if item_module.is_empty() {
                format!("use crate::{};", const_name)
            } else {
                format!("use crate::{}::{};", item_module.replace("::", "::"), const_name)
            };

            imports.insert(import_path);
            break; // Only add one import per constant
        }
    }

    if imports.is_empty() {
        String::new()
    } else {
        imports.into_iter().collect::<Vec<_>>().join("\n") + "\n"
    }
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
) -> String {
    use std::collections::BTreeSet;

    let mut imports: BTreeSet<String> = BTreeSet::new();

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

    // === Optimization 2: Use Aho-Corasick for crate module patterns ===
    // Use std_types cache to detect types that should never be imported from crate modules
    if let Some((crate_automaton, crate_modules)) = build_crate_module_matcher(all_paths, current_module, crate_name) {
        let found_crate_modules = find_crate_module_usages(content, &crate_automaton, &crate_modules);
        for module_path in found_crate_modules {
            // Skip if this looks like a std type import (e.g., "stream::Range")
            let type_name = module_path.rsplit("::").next().unwrap_or(&module_path);
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
                        // Use the first available path from SCIP
                        if let Some(scip_path) = scip_paths.first() {
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
                imports.insert(format!("use crate::{};", renamed_path));
            }
        }
    }

    // Find all identifiers that might be types (start with uppercase or are c_* FFI types)
    // This loop is O(n) and cannot be further optimized with Aho-Corasick
    let mut current_word = String::with_capacity(64);
    let mut word_start_pos = 0usize;
    let content_bytes = content.as_bytes();
    for (i, c) in content.chars().enumerate() {
        if c.is_alphanumeric() || c == '_' {
            if current_word.is_empty() {
                word_start_pos = i;
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
                    let is_qualified = if word_start_pos >= 2 {
                        &content_bytes[word_start_pos-2..word_start_pos] == b"::"
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
                            // Skip imports from unstable APIs (E0658 fix)
                            if !is_unstable_import(import_path) {
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
                                // Skip imports from unstable APIs (E0658 fix)
                                if !is_unstable_import(import_path) {
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
                                    imports.insert(format!("use crate::{};", current_word));
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
                                    // Skip if the first path component is an inline module definition (E0255 fix)
                                    let first_component = item_module.split("::").next().unwrap_or(&item_module);
                                    if inline_modules.contains(first_component) {
                                        found_in_crate = true;
                                        break;
                                    }
                                    // Prefer shorter paths
                                    let is_better = match &best_item_module {
                                        None => true,
                                        Some(best) => item_module.matches("::").count() < best.matches("::").count(),
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
                                if let Some(scip_path) = scip_paths.first() {
                                    // Skip empty or single-character paths - likely false positives
                                    if scip_path.len() <= 1 {
                                        // Skip this path
                                    } else if scip_path != current_module {
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

                    // Only import from std if NOT defined in the crate
                    if !found_in_crate {
                        // Skip std import if it's a commonly redefined type
                        if !COMMONLY_REDEFINED.contains(&current_word.as_str()) {
                            if let Some(import_path) = std_types.get(&current_word) {
                                // Skip imports from unstable APIs (E0658 fix)
                                if !is_unstable_import(import_path) {
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
    ];

    for cap in func_call_pattern.captures_iter(content) {
        if let Some(m) = cap.get(1) {
            let func_name = m.as_str();

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
        imports.into_iter().collect::<Vec<_>>().join("\n") + "\n\n"
    }
}

/// Filter imports relevant to a specific module
fn filter_module_imports(
    use_statements: &[super::types::UseStatement],
    module_path: &str,
    items: &[&super::types::ParsedItem],
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
    modules: &std::collections::BTreeMap<String, Vec<&super::types::ParsedItem>>,
    all_paths: &HashSet<String>,
) -> ReexportInfo {
    use std::collections::BTreeSet;

    // Build a map of type_name -> (full_module_path, last_segment_of_path) for all type definitions
    // This allows us to look up where types are defined and their relative paths
    let mut type_to_full_path: HashMap<String, (String, String)> = HashMap::new();
    // Also keep a simple type_name -> module_path map for direct crate::Type references
    let mut type_to_module: HashMap<String, String> = HashMap::new();

    for (mod_path, items) in modules {
        for item in items {
            // Only track types (struct, enum, type alias, trait)
            if matches!(item.kind,
                super::types::ParsedItemKind::Struct |
                super::types::ParsedItemKind::Enum |
                super::types::ParsedItemKind::TypeAlias |
                super::types::ParsedItemKind::Trait
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

            if path_parts.len() == 1 {
                // Direct crate::Type reference
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
            } else {
                // Nested reference like crate::module::Type or crate::mod1::mod2::Type
                // We need to re-export from the intermediate module
                let intermediate_path: String = path_parts[..path_parts.len()-1].join("::");

                // Look up where this type is actually defined
                if let Some((actual_path, _)) = type_to_full_path.get(&type_name) {
                    // Check if the type is defined in a submodule of intermediate_path
                    // e.g., intermediate_path = "rt", actual_path = "rt::lazy"
                    let prefix = format!("{}::", intermediate_path);
                    if actual_path.starts_with(&prefix) && all_paths.contains(actual_path) {
                        // Get the relative path from intermediate to actual
                        let relative = &actual_path[prefix.len()..];
                        // Get just the immediate child module
                        let child_module = relative.split("::").next().unwrap_or(relative);
                        if !child_module.is_empty() {
                            module_reexports
                                .entry(intermediate_path.clone())
                                .or_default()
                                .push((type_name.clone(), child_module.to_string()));
                        }
                    } else if all_paths.contains(actual_path) {
                        // Type is in a sibling module (e.g., crate::rt::Lazy but Lazy is in crate::lazy)
                        // Need to add re-export from sibling to intermediate module
                        // Format: pub use crate::actual_path::TypeName;
                        module_reexports
                            .entry(intermediate_path.clone())
                            .or_default()
                            .push((type_name.clone(), format!("crate::{}", actual_path)));
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

/// Generate lib.rs content with module declarations and root items
/// Returns (lib_content, module_reexports) where module_reexports maps module paths
/// to (type_name, source_submodule) pairs that need to be re-exported
fn generate_lib_rs(
    modules: &std::collections::BTreeMap<String, Vec<&super::types::ParsedItem>>,
    all_paths: &HashSet<String>,
    use_statements: &[super::types::UseStatement],
    all_code: &str,
    index: &CrateIndex,
    crate_local_types: &HashSet<String>,
    skipped_modules: &HashSet<String>,
) -> (String, HashMap<String, Vec<(String, String)>>) {
    let mut content = String::from("//! Sliced crate - auto-generated\n\n");

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
    let inline_module_names: HashSet<String> = modules.get("")
        .map(|items| {
            items.iter()
                .filter(|item| matches!(item.kind, super::types::ParsedItemKind::Mod))
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
                // Skip empty modules with no children
                // These are likely type names (e.g., __m256i, uint8x16_t)
                let has_items = modules.contains_key(p.as_str());
                let prefix = format!("{}::", p);
                let has_children = all_paths.iter().any(|path| path.starts_with(&prefix));
                if !has_items && !has_children {
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

    if !top_modules.is_empty() {
        content.push('\n');
    }

    // Add root-level items (empty module path) with their imports
    if let Some(items) = modules.get("") {
        // Build root content first to detect needed imports
        let mut root_content = String::new();
        for item in items {
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
        let locally_defined: HashSet<String> = items.iter()
            .filter(|item| matches!(item.kind,
                super::types::ParsedItemKind::Struct |
                super::types::ParsedItemKind::Enum |
                super::types::ParsedItemKind::TypeAlias |
                super::types::ParsedItemKind::Trait |
                super::types::ParsedItemKind::Const
            ))
            .map(|item| item.name.clone())
            .collect();

        // Generate imports for root-level items
        // Include symbols from preserved imports to avoid duplicates
        let already_imported = extract_symbols_from_preserved_imports(use_statements, "");
        // Child modules for root are top-level modules (no :: in path)
        let root_child_modules: HashSet<String> = all_paths.iter()
            .filter(|p| !p.contains("::"))
            .cloned()
            .collect();
        let imports = generate_internal_imports(&root_content, "", index, all_paths, &locally_defined, crate_local_types, &already_imported, &root_child_modules, None, None);
        content.push_str(&imports);
        for item in items {
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
    let mut needed = HashSet::new();
    for item in seed_items {
        needed.insert(item.clone());
    }

    // Expand transitively
    let needed = super::slicing::expand_needed_transitively(&needed, &index);

    generate_semantic_sliced_crate_with_needed(crate_info, &index, &needed, output_dir)
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
    use super::source_fix::{fix_crate_imports, fix_result_type_alias, fix_conflicting_imports, fix_external_crate_imports, fix_unstable_imports, fix_missing_imports, fix_empty_rt_v2_module, fix_crate_enum_references, fix_module_import_conflicts, fix_lib_rs_reexports, fix_trait_struct_confusion, fix_spurious_doc_imports, fix_private_item_visibility, fix_duplicate_impls};

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().map(|e| e == "rs").unwrap_or(false) {
            let content = fs::read_to_string(&path)?;
            let path_str = path.to_string_lossy();
            let fixed = fix_crate_imports(&content, item_locations);
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
) -> std::io::Result<HashSet<String>> {
    let mut additional_modules = HashSet::new();
    scan_directory_for_modules(src_dir, "", existing_modules, &mut additional_modules, child_module_items)?;
    Ok(additional_modules)
}

/// Recursively scan a directory and its subdirectories for module paths
fn scan_directory_for_modules(
    dir: &Path,
    current_path: &str,
    existing_modules: &HashSet<String>,
    additional_modules: &mut HashSet<String>,
    child_module_items: &HashMap<String, Vec<(String, bool)>>,
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
                scan_directory_for_modules(&path, &module_path, existing_modules, additional_modules, child_module_items)?;
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
        );

        // Should NOT contain "use std::hint;" because there is a local "hint" module
        assert!(!result.contains("use std::hint;"), "Should not import std::hint when local hint module exists. Result: {}", result);
    }
}

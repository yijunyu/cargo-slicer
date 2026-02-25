//! Static configuration constants for the cargo slicer.

/// Rust reserved keywords that need r# prefix when used as identifiers
pub const RUST_KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern",
    "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod",
    "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct",
    "super", "trait", "true", "type", "unsafe", "use", "where", "while",
    "async", "await", "dyn", "abstract", "become", "box", "do", "final",
    "macro", "override", "priv", "try", "typeof", "unsized", "virtual", "yield",
];

/// Passthrough thresholds for complexity heuristics
pub const MAX_EXTERNAL_DEPS: usize = 10;

/// Known crate categories that are difficult to slice - FFI crates
pub const FFI_CRATES: &[&str] = &["libc", "mimalloc", "jemalloc-sys", "libz-sys", "openssl-sys"];

/// Procedural macro crates
pub const PROC_MACRO_CRATES: &[&str] = &["proc-macro2", "quote", "darling", "derive_more"];

/// Complex crates that are difficult to slice
/// NOTE: Keep this list minimal! Only crates with fundamental issues like:
/// - Macro-heavy code that can't be statically analyzed
/// - Circular proc-macro dependencies
/// For internal dependency issues, fix the slicer instead of blocklisting.
pub const COMPLEX_CRATES: &[&str] = &[
    // Core async runtime crates (macro-heavy, complex trait hierarchies)
    "serde", "tokio", "futures", "hyper", "reqwest", "axum", "actix-web", "thiserror",
    // Circular proc-macro dependencies (type mismatches when mixed sliced/registry)
    "syn", "proc-macro2", "quote",
    // Crates dependent on blocked serde derive functionality
    "serde_json",
    // Build tooling crates (need cargo internals)
    "cargo_toml", "cargo_metadata",
    // Multi-version transitive dependencies (rand v0.8.5 and v0.9.2 conflict)
    "tokei",              // tokei → serde_cbor → half → rand_distr → rand v0.8.x
];

/// Platform-specific crates that should not be sliced
/// These are only for non-Linux targets and cause transitive dependency conflicts
pub const PLATFORM_SPECIFIC_CRATES: &[&str] = &[
    // WASI/WebAssembly crates
    "wasip2", "wasi", "wit-bindgen", "wit-bindgen-rt", "wit-bindgen-rust-macro",
    "wit-parser", "wit-component", "wasmparser", "wasm-encoder",
    "wasm-bindgen", "wasm-bindgen-shared", "wasm-bindgen-macro", "wasm-bindgen-macro-support",
    "wasm-bindgen-backend", "wasm-bindgen-futures", "wasm-streams", "wasm_sync",
    "web-sys", "web-time", "js-sys", "instant",
    // Windows-specific crates
    "windows", "windows-sys", "windows-core", "windows-targets", "windows-result", "windows-link",
    // Apple-specific crates
    "core-foundation", "core-foundation-sys", "security-framework", "security-framework-sys",
    "objc2", "objc2-exception-helper", "block2", "dispatch2", "fsevent-sys",
    // BSD-specific crates
    "kqueue", "kqueue-sys",
    // Android-specific crates
    "android-tzdata", "android_system_properties",
    // Redox-specific crates
    "redox_syscall", "redox_users",
];

/// Crates that cause version conflicts due to `links` attribute or macro dependencies
/// These should use registry versions to avoid conflicts with other registry crates
pub const CONFLICT_PRONE_CRATES: &[&str] = &[
    // Crates with links attribute that conflict
    "rayon", "rayon-core",
    // Crates with complex macro dependencies
    "prettyplease", "syn-test-suite",
    // Crates that have incompatible sliced/registry mixing
    "portable-atomic",
    // Crates with circular optional dependencies (borsh <-> indexmap)
    "borsh",
];

/// Dependencies that have been yanked from crates.io and should be skipped
pub const YANKED_DEPENDENCIES: &[&str] = &[
    "atomic-polyfill",
];

/// Dependencies that should be made non-optional
pub const FORCE_NON_OPTIONAL: &[&str] = &[
    "parking_lot",
];

/// Crates that should skip trial deletion.
/// Previously contained 19 entries that were workarounds for dependency tracking bugs.
/// After fixing: (1) deps.is_empty() filter, (2) multi-match break removal,
/// (3) macro body scanning, (4) underscore-prefixed identifier support,
/// no crates need skipping.
pub const SKIP_TRIAL_DELETION: &[&str] = &[
];

/// Well-known external crates that should be detected as dependencies
pub const KNOWN_EXTERNAL_CRATES: &[&str] = &[
    "proc_macro2", "syn", "quote", "serde", "serde_json",
    "tokio", "futures", "async_trait", "anyhow", "thiserror",
    "log", "tracing", "regex", "once_cell", "lazy_static",
    "chrono", "time", "rand", "rand_core", "parking_lot",
    "lock_api", "crossbeam", "rayon", "memchr", "scopeguard",
    "iana_time_zone", "pure_rust_locales", "num_traits",
    "either", "itertools", "bytes", "http", "hyper", "http_body", "http_body_util",
    "futures_channel", "futures_util", "atomic_waker", "pin_project", "pin_project_lite",
    "reqwest", "url", "uuid", "base64", "sha2", "md5",
    "toml", "yaml", "csv", "xml", "html", "json",
    "libc", "winapi", "nix", "mio", "socket2",
    "bitflags", "smallvec", "tinyvec", "arrayvec",
    "hashbrown", "indexmap", "dashmap", "ahash", "fnv",
    "pin_project", "pin_utils", "futures_core", "futures_util",
    "tower", "tonic", "prost", "protobuf",
    "clap", "structopt", "env_logger", "flexi_logger",
    "tempfile", "walkdir", "globset", "ignore",
    "aho_corasick", "bstr", "unicode_segmentation",
    "num_integer", "num_bigint", "num_rational", "num_complex",
    "ryu", "itoa", "dtoa", "lexical",
    "camino", "semver", "cargo_platform",
    "cargo_metadata", "cargo_toml", "toml_edit",
    "serde_derive", "derive_more", "derive_builder",
    "serde_core",
    "clap_builder", "clap_derive", "clap_lex",
    "tokio_macros", "tokio_stream", "tokio_util",
    "httpdate", "want", "h2", "httparse", "hyper_util",
    "axum_core", "axum_macros", "serde_urlencoded", "serde_path_to_error",
    "actix_http", "actix_router", "actix_server", "actix_service", "actix_codec",
    "actix_rt", "actix_utils", "actix_web_codegen", "actix_macros",
    "sqlx_core", "sqlx_macros", "sqlx_sqlite", "sqlx_postgres", "sqlx_mysql",
    "diesel_derives", "diesel_table_macro_syntax",
    "tower_service", "tower_layer", "tower_http",
    "matchit", "sync_wrapper", "mime", "percent_encoding", "form_urlencoded",
    "bytestring", "language_tags", "encoding_rs", "foldhash", "impl_more",
    "hashlink", "fallible_iterator", "fallible_streaming_iterator", "libsqlite3_sys", "lru_cache",
    "sea_orm", "sea_orm_macros", "sea_query", "sea_query_binder", "sea_query_derive",
    "strum", "strum_macros", "ouroboros",
    "zerocopy", "zerocopy_derive",
];

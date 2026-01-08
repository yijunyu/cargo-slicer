# Rust Crate Slicer - Usage Guide

> This is the detailed usage guide. See [README.md](../README.md) for a quick overview.

## Installation

```bash
# Build and install as a cargo subcommand
cargo install --path . --force

# Or build locally
cargo build --release
```

## Usage

```bash
# Slice a single crate
cargo-slicer <crate_name>

# With options
cargo-slicer <crate_name> --no-union-slice --adaptive

# Analyze dependency graph
cargo-slicer --analyze-deps

# Force re-slicing
FORCE_SLICE_ALL=1 cargo-slicer <crate_name>
```

## Performance Results

### Major Async/Web Framework Crates (December 2025)

| Crate | Version | Module Files | Lines Generated | Status |
|-------|---------|--------------|-----------------|--------|
| **tokio** | 1.48.0 | 315 | 90,982 | ✅ Success |
| **actix-web** | 4.12.1 | 87 | 27,854 | ✅ Success |
| **hyper** | 1.8.1 | 51 | 18,697 | ✅ Success |
| **axum** | 0.7.9 | 45 | 14,955 | ✅ Success |
| **reqwest** | 0.12.28 | 26 | 13,438 | ✅ Success |
| **sqlx** | 0.8.6 | 0 | ~100 | ✅ Success (facade) |
| **syn** | 2.0.x | 47 | ~15,000 | ✅ Success |
| **rand** | 0.9.2 | 31 | ~8,000 | ✅ Success |
| **regex** | 1.x | 21 | ~6,000 | ✅ Success |
| **diesel** | 2.3.5 | 177 | 43,017 | ⚠️ Complex (160 errors) |

### Smaller Utility Crates

| Crate | Slice Time | Items Needed | Files | Lines Generated |
|-------|------------|--------------|-------|-----------------|
| regex | 0.147s | 309 | 20 | 10,671 |
| futures | 0.1s | 3 | 0 | 91 |
| once_cell | 0.025s | 2 | 0 | 29 |
| memchr | 0.198s | 8 | 35 | 14,475 |
| anyhow | 0.058s | 147 | 11 | 4,412 |
| thiserror | 0.010s | 2 | 0 | 28 |

## Known Complex Crates

These crates require special handling but now compile successfully:

| Crate | Complexity | Solution |
|-------|------------|----------|
| **tokio** | Complex feature interactions, cfg macros | Stub trace module, skip platform files |
| **hyper** | Custom cfg macros (cfg_feature!, cfg_proto!) | Cfg expression parser |
| **axum** | Feature-gated types, `self::` re-exports | Item-to-module mapping for `pub use self::` |
| **actix-web** | macro_rules! in lib.rs, log crate usage | Skip macro content, conditional stub macros |
| **serde** | Build-time code generation, derive macros | Proc-macro crate (0 module files) |
| **sqlx** | Facade crate with sqlx-core/sqlx-macros | Added to known externals, re-exports work |
| **diesel** | Macro-generated proxy modules, `pub(crate)` dsl | ⚠️ Partially works - 160 errors remain |

## Issue Categories

### Category 1: Feature Flag Complexity
- **Symptoms:** Conditional compilation errors, missing types
- **Crates:** tokio, futures, hyper, axum
- **Root Cause:** Complex `#[cfg(feature = "...")]` interactions

### Category 2: Re-export Mismatch
- **Symptoms:** Items detected but not found in parsed crate
- **Crates:** actix-web, regex
- **Root Cause:** Items re-exported from internal modules

### Category 3: Build-Time Code Generation
- **Symptoms:** Missing symbols, OUT_DIR references
- **Crates:** serde
- **Root Cause:** `build.rs` generates code at compile time

### Category 4: Facade Crates
- **Symptoms:** Empty sliced output
- **Crates:** futures (re-exports from futures-*)
- **Fix:** Feature-gated re-export detection now supported

### Category 5: Proc Macro Separation
- **Symptoms:** Derive macros not available
- **Crates:** serde, thiserror
- **Root Cause:** Proc macros in separate `*_derive` crates

## Architecture

The slicer works in five phases:

1. **Usage Analysis** (`usage.rs`): Scan project source for `use crate::*` statements
2. **Crate Location** (`crate_info.rs`): Find crate source in cargo registry
3. **AST Parsing** (`parsing.rs`): Parse crate with syn and build dependency graph
4. **SCIP Analysis** (`semantic.rs`): Run rust-analyzer SCIP to identify crate-local types
5. **Code Generation** (`codegen.rs`): Generate sliced crate with only needed items

```
Project Source → Usage Analysis → Crate Parsing → SCIP Analysis → Code Generation
     ↓                ↓               ↓               ↓               ↓
  use crate::X    HashSet<Item>   CrateIndex    LocalTypes     sliced_crates/
                                  {items,deps}  (avoids E0255)  ├── Cargo.toml
                                                                └── src/lib.rs
```

## Module Structure

The cargo_slicer is organized into focused modules grouped by functionality:

```
src/cargo_slicer/
├── mod.rs           # Module declarations, re-exports, main()
├── types.rs         # Core data structures (UsedItem, ParsedItem, CrateIndex, etc.)
├── constants.rs     # Static configuration (RUST_KEYWORDS, FFI_CRATES, etc.)
│
├── cfg.rs           # Cfg expression parsing and evaluation
├── arch.rs          # Architecture detection and filtering
│
├── usage.rs         # Phase 1: Usage analysis (find_used_items, analyze_crate_usage)
├── crate_info.rs    # Phase 2: Crate source location (find_crate_source)
├── parsing.rs       # Phase 3: AST parsing with syn (parse_crate, TypeRefVisitor)
│
├── slicing.rs       # Dependency computation (compute_needed_items)
├── codegen.rs       # Code generation for sliced crates
├── semantic.rs      # Semantic slicing strategies
├── source_fix.rs    # Source code fixing utilities (rustfmt, bare trait fixes)
│
├── auto_fix.rs      # Auto-fix for compilation errors (stub generation)
├── cargo_toml.rs    # Cargo.toml parsing and generation
├── imports.rs       # External import injection
│
├── dep_graph.rs     # Dependency graph analysis (cargo metadata, ra_deps)
└── slice_all.rs     # Multi-crate slicing (union_slice_deps, slice_all_deps)
```

### Module Groupings

| Group | Modules | Changes Together For |
|-------|---------|---------------------|
| **Core Types** | types.rs, constants.rs | Data structure changes |
| **Platform/Config** | cfg.rs, arch.rs | New platform support |
| **Analysis Pipeline** | usage.rs, crate_info.rs, parsing.rs | Parsing bug fixes |
| **Generation Pipeline** | slicing.rs, codegen.rs, semantic.rs, source_fix.rs | Output bug fixes |
| **Build Integration** | auto_fix.rs, cargo_toml.rs, imports.rs | Cargo compatibility |
| **Multi-crate** | dep_graph.rs, slice_all.rs | Batch operation changes |

## Recent Fixes

> For complete fix history with debugging notes, see [crate_slicing_log.md](crate_slicing_log.md).

### January 2026

| Fix | Description |
|-----|-------------|
| E0603 private imports | Skip imports of private types by checking impl block visibility |
| E0255 name conflicts | SCIP-based detection of crate-local types to avoid std import conflicts |
| E0599 missing impls | Include impl blocks via SCIP occurrences for generic types |
| Module re-exports | Generate `pub use self::child::{Item}` for child module items |
| Nested module paths | Fixed path computation for deep nesting (e.g., `arch::generic::memchr`) |
| Full path detection | Detect `crate::a::b::c::Item` patterns |
| Stub modules | Create empty modules for referenced paths without items |

### December 2025

| Fix | Description |
|-----|-------------|
| Modular refactoring | Split 14K-line cargo_slicer.rs into 17 focused modules |
| proc_macro2 detection | Added types to dependency detection for quote/syn |
| use crate:: scanning | Detect regular `use crate::` imports (not just pub) |
| Item-to-module mapping | Trace `use crate::{Item}` via lib.rs re-exports |
| Duplicate definition fix | Filter already-reexported items from crate_refs |
| Stub logging macros | Always add trace/debug/info/warn/error stubs |
| Duplicate re-export detection | Shared exported_names HashSet across sections |
| Orphaned doc comments | First-pass scan to remove docs before stripped test modules |
| Nested grouped imports | Brace-depth tracking for `use a::{b, c}` |
| mio features | Auto-add net, os-poll, os-ext features |
| Feature-gated facade re-exports | Include `pub use X as module` when module is used |
| Crate alias detection | Detect `pub use crate as alias;` for dependencies |
| Cfg expression parser | Parse `all()`, `any()`, `not()`, `feature = ""` cfg expressions |
| Feature resolver | Get enabled features from cargo metadata and evaluate cfg expressions |
| Path-redirected modules | Handle `#[path = "file.rs"]` module declarations with cfg gates |
| Cfg-gated type aliases | Filter type aliases based on cfg evaluation (fixes parking_lot) |
| Features section copying | Copy original [features] section from Cargo.toml |
| Feature filtering | Filter out dep:, ?/, internal (core), and non-optional dep references |
| Multi-line feature arrays | Parse Cargo.toml features with multi-line array syntax |
| Default feature expansion | Expand default features from crate's own Cargo.toml |
| Crate-local imports | Extract `use crate::...` imports for lib.rs functions |
| Platform cfg evaluation | Use cfg!(unix) and cfg!(windows) to evaluate platform conditions |
| Target-specific deps | Capture [target."cfg(unix)".dependencies.X] sections for matching platforms |
| Cfg-gated re-export filtering | Skip extracting re-exports from cfg-gated modules (e.g., rkyv) |
| Self-import rewriting | Replace `use crate_name::` with `use crate::` in sliced files |
| Extern crate proc_macro | Add extern crate proc_macro when proc-macro feature is enabled |
| Feature default augmentation | Add enabled features to default features in sliced Cargo.toml |
| Syn visibility fixes | Make discouraged.rs-required items public for sliced syn |
| `pub use self::` support | Parse `pub use self::module::Item` re-exports (axum) |
| Item-to-module lookup | Check item_to_module when parsing `pub use crate::Type` |
| macro_rules! skipping | Skip content inside macro_rules! when extracting re-exports |
| Conditional stub macros | Skip stub logging macros when crate has log/tracing dependency |
| Actix ecosystem crates | Added actix_http, actix_router, actix_server, etc. to known externals |
| Hyper ecosystem crates | Added hyper_util, serde_path_to_error to known externals |
| SQLx ecosystem crates | Added sqlx_core, sqlx_macros to known externals (facade crate support) |
| include_str! file copying | Copy non-.rs files (SQL, txt) referenced by include_str!/include_bytes! |
| Cfg-gated pub use filtering | Skip `pub use` statements gated by disabled features |
| Indented pub use filtering | Skip `pub use` inside module blocks (dsl, helper_types) |
| extern crate self as alias | Use `extern crate self as crate_name` for derive macro compatibility |

## Precc Dependencies Test Results

| Crate | Status | Notes |
|-------|--------|-------|
| **quote** | COMPILES | Fixed proc_macro2 dep + IdentFragment tracing |
| **memchr** | COMPILES | Fixed duplicate function definition + feature filtering |
| **glob** | COMPILES | Simple crate, works well |
| **once_cell** | COMPILES | Fixed path-redirected imp module with cfg evaluation |
| **parking_lot** | COMPILES | Fixed cfg-gated type alias filtering |
| **rand** | COMPILES | Fixed multi-line feature parsing + default feature expansion |
| **regex** | COMPILES | Fixed nested cfg-gated module handling |
| **chrono** | COMPILES | Fixed platform cfg eval + target-specific deps + cfg-gated re-exports |
| **syn** | COMPILES | Fixed self-imports, proc_macro linkage, feature defaults, visibility |

**Success rate: 9/9 (100%)**

## Major Crate Test Results (December 2025)

| Crate | Version | Status | Notes |
|-------|---------|--------|-------|
| **tokio** | 1.48.0 | ✅ COMPILES | 315 modules, 90K lines, stub trace module |
| **actix-web** | 4.12.1 | ✅ COMPILES | 87 modules, 28K lines, macro_rules! skipping |
| **hyper** | 1.8.1 | ✅ COMPILES | 51 modules, 19K lines |
| **axum** | 0.7.9 | ✅ COMPILES | 45 modules, 15K lines, `pub use self::` support |
| **reqwest** | 0.12.28 | ✅ COMPILES | 26 modules, 13K lines |
| **syn** | 2.0.x | ✅ COMPILES | 47 modules, 15K lines |
| **rand** | 0.9.2 | ✅ COMPILES | 31 modules, 8K lines |
| **regex** | 1.x | ✅ COMPILES | 21 modules, 6K lines |

**Major crate success rate: 8/8 (100%)**

## Known External Crates

The slicer recognizes these external crate dependencies:

### Core/Std
`std`, `core`, `alloc`, `proc_macro`, `proc_macro2`, `quote`, `unicode_ident`

### Tokio Ecosystem
`tokio_macros`, `tokio_stream`, `tokio_util`

### Hyper Ecosystem
`httpdate`, `want`, `h2`, `httparse`, `hyper_util`

### Axum Ecosystem
`axum_core`, `axum_macros`, `serde_urlencoded`, `serde_path_to_error`

### Actix Ecosystem
`actix_http`, `actix_router`, `actix_server`, `actix_service`, `actix_codec`,
`actix_rt`, `actix_utils`, `actix_web_codegen`, `actix_macros`

### Tower Ecosystem
`tower_service`, `tower_layer`, `tower_http`

### Database Ecosystem
`sqlx_core`, `sqlx_macros`, `sqlx_sqlite`, `sqlx_postgres`, `sqlx_mysql`,
`diesel_derives`, `diesel_table_macro_syntax`

### Utilities
`itoa`, `ryu`, `matchit`, `sync_wrapper`, `mime`, `percent_encoding`,
`form_urlencoded`, `bytestring`, `language_tags`, `encoding_rs`, `foldhash`, `impl_more`

## Current Progress

| Metric | Value |
|--------|-------|
| Total Sliced Crates | 59 |
| Successfully Compiling | 27 |
| Failed Crates | 32 |
| Success Rate | 46% |
| Last Updated | 2025-12-28 03:29 |

### Error Categories in Failed Crates

| Error Code | Description | Status |
|------------|-------------|--------|
| E0433 | Unresolved module/crate | ⚠️ Partially fixed (nested paths) |
| E0412 | Cannot find type | Open |
| E0658 | Unstable feature | Open |
| E0603 | Private item import | ✅ Fixed (impl block visibility) |
| E0255 | Name conflict (std vs local) | ✅ Fixed (SCIP-based) |
| E0599 | Missing trait impl | ✅ Fixed (SCIP impl blocks) |
| E0432 | Unresolved import | Open |

## Related Documentation

- [README.md](../README.md) - Quick overview and demo GIFs
- [crate_slicing_workflow.md](crate_slicing_workflow.md) - Automated workflow scripts
- [crate_slicing_log.md](crate_slicing_log.md) - Complete debugging notes and fix history
- [pldi_compilation_unit_slicing.md](pldi_compilation_unit_slicing.md) - Research background

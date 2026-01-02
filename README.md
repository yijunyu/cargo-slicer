# Rust Crate Slicer (cargo-slicer)

**cargo-slicer** applies compilation unit slicing to Rust crates, generating minimal versions containing only the code actually used by a project.

## Installation

```bash
cargo install --path . --force
```

![Demo](assets/demo_1.gif)

## Usage

```bash
# Analyze dependency graph
cargo-slicer --analyze-deps
```

![Demo](assets/demo_2.gif)

```bash
# Slice a single crate
cargo-slicer regex --no-union-slice --adaptive
```

![Demo](assets/demo_3.gif)

```bash
# Slice cargo-slicer itself
cargo-slicer cargo-slicer
```

![Demo](assets/demo_4.gif)

## Performance Results

### Major Crates 

| Crate | Version | Modules | Lines | Status |
|-------|---------|---------|-------|--------|
| **tokio** | 1.48.0 | 315 | 90,982 | ✅ |
| **actix-web** | 4.12.1 | 87 | 27,854 | ✅ |
| **hyper** | 1.8.1 | 51 | 18,697 | ✅ |
| **axum** | 0.7.9 | 45 | 14,955 | ✅ |
| **reqwest** | 0.12.28 | 26 | 13,438 | ✅ |
| **syn** | 2.0.x | 47 | ~15,000 | ✅ |
| **rand** | 0.9.2 | 31 | ~8,000 | ✅ |
| **regex** | 1.x | 21 | ~6,000 | ✅ |

### Utility Crates

| Crate | Time | Items | Files | Lines |
|-------|------|-------|-------|-------|
| regex | 0.147s | 309 | 20 | 10,671 |
| futures | 0.1s | 3 | 0 | 91 |
| once_cell | 0.025s | 2 | 0 | 29 |
| memchr | 0.198s | 8 | 35 | 14,475 |
| anyhow | 0.058s | 147 | 11 | 4,412 |

## Architecture

The slicer works in five phases:

```
Project Source → Usage Analysis → Crate Parsing → SCIP Analysis → Code Generation
     ↓                ↓               ↓               ↓               ↓
  use crate::X    HashSet<Item>   CrateIndex    LocalTypes     sliced_crates/
```

1. **Usage Analysis**: Scan project for `use crate::*` statements
2. **Crate Location**: Find source in cargo registry
3. **AST Parsing**: Parse with syn, build dependency graph
4. **SCIP Analysis**: Use rust-analyzer to identify crate-local types
5. **Code Generation**: Generate sliced crate with only needed items

## Module Structure

```
src/
├── main.rs          # Entry point, CLI
├── types.rs         # Core data structures
├── cfg.rs           # Cfg expression parsing
├── usage.rs         # Usage analysis
├── parsing.rs       # AST parsing with syn
├── semantic.rs      # SCIP analysis + imports
├── codegen.rs       # Code generation
├── slicing.rs       # Dependency computation
└── slice_all.rs     # Multi-crate slicing
```

## Known Issue Categories

| Category | Symptoms | Examples |
|----------|----------|----------|
| Feature flags | Conditional compilation errors | tokio, futures, hyper |
| Re-exports | Items not found in parsed crate | actix-web, regex |
| Build-time codegen | Missing symbols, OUT_DIR refs | serde |
| Facade crates | Empty sliced output | futures |
| Proc macros | Derive macros unavailable | serde, thiserror |

## Documentation

| Document | Description |
|----------|-------------|
| [Usage Guide](docs/cargo_slicer.md) | Complete usage and options |
| [Workflow](docs/crate_slicing_workflow.md) | Automated slicing scripts |
| [Fix History](docs/crate_slicing_log.md) | Detailed debugging notes |

## Current Status

| Metric | Value |
|--------|-------|
| Total Sliced Crates | 59 |
| Successfully Compiling | 27 |
| Success Rate | 46% |
| Major Crate Success | 8/8 (100%) |

### Recent Improvements

- **E0603 fix**: Skip imports of private types (impl block visibility check)
- **E0255 fix**: SCIP-based detection of crate-local types
- **E0599 fix**: Include impl blocks via SCIP occurrences
- **Nested paths**: Fixed module path computation for deep nesting
- **Stub modules**: Create empty modules for referenced paths

See [docs/crate_slicing_log.md](docs/crate_slicing_log.md) for complete fix history.

## Known External Crates

The slicer recognizes these ecosystem crates as external dependencies:

- **Core**: std, core, alloc, proc_macro, proc_macro2, quote
- **Tokio**: tokio_macros, tokio_stream, tokio_util
- **Hyper**: httpdate, want, h2, httparse, hyper_util
- **Axum**: axum_core, axum_macros, serde_urlencoded
- **Actix**: actix_http, actix_router, actix_server, actix_rt
- **Tower**: tower_service, tower_layer, tower_http
- **Database**: sqlx_core, sqlx_macros, diesel_derives

See [docs/cargo_slicer.md](docs/cargo_slicer.md) for complete list.

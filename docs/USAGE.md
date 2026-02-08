# cargo-slicer Usage Reference

## Virtual Slicing

### Complete Workflow

`RUSTC_WRAPPER` must only point to the dispatch binary when building your *target* project, not when building cargo-slicer itself. The dispatch binary handles sccache internally for non-local crates, so you never need to manage sccache manually.

| Phase | `RUSTC_WRAPPER` |
|-------|-----------------|
| Building cargo-slicer itself | unset (or sccache, the default) |
| Building your target project | `cargo_slicer_dispatch` |

#### Linux filesystem

```bash
# Step 1: Install cargo-slicer (RUSTC_WRAPPER must NOT be dispatch)
unset RUSTC_WRAPPER CARGO_SLICER_VIRTUAL CARGO_SLICER_CODEGEN_FILTER
cargo install --path .
cargo +nightly install --path . --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver

# Step 2: (Optional) Pre-analyze for cross-crate reachability
cargo-slicer pre-analyze

# Step 3: Build your target project with virtual slicing
cargo clean
CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +nightly build --release
```

#### WSL on Windows drives (`/mnt/c/`, `/mnt/d/`, etc.)

Same sequence, but disable sccache to avoid NTFS permission errors:

```bash
# Step 1: Install cargo-slicer (disable sccache for NTFS compatibility)
unset RUSTC_WRAPPER CARGO_SLICER_VIRTUAL CARGO_SLICER_CODEGEN_FILTER
SCCACHE_IDLE_TIMEOUT=0 cargo install --path .
SCCACHE_IDLE_TIMEOUT=0 cargo +nightly install --path . --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver

# Step 2: (Optional) Pre-analyze for cross-crate reachability
cargo-slicer pre-analyze

# Step 3: Build with virtual slicing (disable sccache inside dispatch too)
cargo clean
CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  CARGO_SLICER_SCCACHE=/nonexistent \
  RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +nightly build --release
```

Pre-analysis (step 2) uses lightweight syn-based parsing (~0.5s for ripgrep, ~12s for zed) instead of full compilation.

### Environment Variables

#### Core

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_VIRTUAL` | unset | Set to `1` to enable virtual slicing |
| `CARGO_SLICER_CODEGEN_FILTER` | unset | Set to `1` to enable MIR abort stub replacement |
| `RUSTC_WRAPPER` | unset | Set to `cargo_slicer_dispatch` path |

#### Cross-Crate

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_CROSS_CRATE` | unset | Set to `1` to enable cross-crate analysis |
| `CARGO_SLICER_PHASE` | unset | Phase: `analysis` (collect data), `build` (use cached seeds), or empty (legacy single-pass) |
| `CARGO_SLICER_PARSER` | `syn` | Pre-analysis parser backend: `syn`, `fast`, or `ctags` |

#### Caching

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_CACHE_DIR` | `.slicer-cache` | Directory for incremental cache files |
| `CARGO_SLICER_NO_CACHE` | unset | Set to `1` to disable caching (always recompute) |

#### Performance Tuning

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_DAEMON` | unset | Set to `1` to enable fork-server daemon mode (pre-loads driver) |
| `CARGO_SLICER_SCCACHE` | auto-detect | Path to sccache binary, or `/nonexistent` to disable |
| `CARGO_SLICER_SKIP_DEPS` | unset | Set to `1` to skip virtual slicing for registry dependencies |
| `CARGO_SLICER_RELAX_UNSAFE` | unset | Set to `1` to allow stubbing `unsafe fn` (normally forbidden) |

#### Debugging

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_DEBUG` | unset | Set to `1` to enable debug logging |
| `CARGO_SLICER_DEBUG_LOG` | `.cargo-slicer-debug.log` | Custom path for debug log file |
| `CARGO_SLICER_MARKED_ITEMS` | unset | Pre-load marked items from file (skip BFS) |
| `CARGO_SLICER_MARKED_OUT` | unset | Write marked items to file for inspection |

#### Data Collection

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_COLLECT_DATA` | unset | Set to `1` to enable HIR data collection mode |
| `CARGO_SLICER_DATA_OUT` | `.cargo-slicer-data.json` | Output file for collected data |
| `CARGO_SLICER_VSCRIPT_DIR` | unset | Directory for linker version scripts |

---

## Source Slicing (Legacy)

### Basic Usage

```bash
cargo install --path .

cargo-slicer                   # Slice all dependencies
cargo-slicer regex             # Slice specific crate
cargo-slicer --clean           # Clean and re-slice
cargo-slicer --verbose         # Detailed output
cargo-slicer -O                # Fast production mode
```

### Subcommands

| Subcommand | Description |
|------------|-------------|
| *(default)* | Slice all dependencies of the current project |
| `build [ARGS]` | Slice dependencies and build with sliced crates |
| `pre-analyze [--parser BACKEND]` | Run pre-build static analysis for cross-crate virtual slicing |

### Common Options

| Option | Description |
|--------|-------------|
| `-o, --output DIR` | Custom output directory (default: `<project>_sliced/`) |
| `-v, --verbose` | Show detailed output and debug information |
| `-w, --watch` | Watch mode: monitor files and auto-rebuild |
| `-c, --clean` | Clean before slicing |
| `-b, --bench` | Run build benchmarks (sliced vs normal) |
| `-V, --virtual` | Enable virtual slicing mode |
| `--export-symbols FILE` | Export marked symbols to linker script |

### Optimization Levels

| Level | Description |
|-------|-------------|
| `-O0` | No deletion (safe baseline) |
| `-O1` | Delete private functions (with verification) |
| `-O2` | Delete all private items + trial deletion |
| `-O3` | Graph-guided deletion (default) |
| `-O4` | Full aggressive mode |
| `-O` | Fast production mode (skip verification) |

### Feature Flags

Use `-f<feature>` to enable or `-fno-<feature>` to disable.

**Deletion**:
- `-fprivate-fn` — Delete unused private functions
- `-fprivate-const` — Delete unused private constants/statics
- `-fprivate-type` — Delete unused private type aliases
- `-fprivate-struct` — Delete unused private structs/enums
- `-ftrust-graph` — Trust dependency graph for all items
- `-ftrial-delete` — Trial-based deletion (slower but accurate)
- `-fgraph-guided` — Graph-guided deletion with JSON verification

**Analysis**:
- `-fcycle-breaking` — Enable cycle detection & breaking
- `-fcfg-eval` — Evaluate `#[cfg(...)]` for target platform
- `-fauto-fixes` — Apply Cargo.toml compatibility fixes

**Parser selection**:
- `-fuse-ctags` — Use ctags parser (fast, lower accuracy)
- `-fuse-fast-tokenizer` — Use fast tokenizer (fastest, items only)
- `-fuse-rustc-driver` — Use rustc driver (default, 100% accurate)
- `-frustc-via-cargo` — Use `cargo rustc` instead of direct `rustc`

**Verification**:
- `-fverify` — Run `cargo check` after deletion
- `-fprofiling` — Detailed timing analysis per phase

---

## Pre-Analysis

The `pre-analyze` subcommand runs lightweight static analysis on workspace crates before the build phase. This enables cross-crate reachability analysis without requiring full compilation.

```bash
cargo-slicer pre-analyze                # Use default syn backend
cargo-slicer pre-analyze --parser fast  # Use fast tokenizer
cargo-slicer pre-analyze --parser ctags # Use ctags (items only)
```

Or via environment variable:

```bash
CARGO_SLICER_PARSER=fast cargo-slicer pre-analyze
```

### Parser Backends

| Backend | Speed | Call Edges | Accuracy |
|---------|-------|------------|----------|
| `syn` | Moderate (0.5-12s) | Yes (most accurate) | High |
| `fast` | Fast | Yes (fewer edges) | Medium |
| `ctags` | Fastest | None | Items only |

### Output Files

Pre-analysis writes to `.slicer-cache/`:
- `<crate>.analysis` — Items, call edges, trait impls per crate
- `<crate>.seeds` — BFS-computed seed items for the driver

---

## Benchmarking

### CI Scripts

```bash
# Multi-crate benchmarks: 7 projects × {baseline, vslice-cc} × 3 runs
./scripts/ci_bench_multicrate.sh

# Single-crate rustc-perf: 20 crates × {baseline, vslice} × 3 runs
./scripts/ci_bench_rustc_perf.sh
```

Both scripts build binaries, run benchmarks, store results in `bench-results.db` (SQLite), and generate self-contained HTML reports.

### Manual Benchmarks

```bash
# Run a specific project benchmark
./scripts/bench_fresh_build.sh <project> <mode> <runs>

# Examples:
./scripts/bench_fresh_build.sh nushell baseline 3
./scripts/bench_fresh_build.sh nushell vslice-cc 3
```

**Projects**: `ripgrep`, `nushell`, `rustc`, `helix`, `bevy`, `zed`, `slicer`

**Modes**:
- `baseline` — Normal `cargo build --release` (no RUSTC_WRAPPER)
- `vslice` — Virtual slicing without cross-crate pre-analysis
- `vslice-cc` — Virtual slicing with cross-crate pre-analysis (recommended)

### Metrics Collected

Each run records:
- Wall-clock time (via `/usr/bin/time -v`)
- Peak RSS memory
- CPU instructions and cycles (via `perf stat`, userspace-only counters)

---

## Cache Files

The `.slicer-cache/` directory contains:

| File Pattern | Description |
|--------------|-------------|
| `<crate>-<type>-<hash>.marked` | Cached marked-items set (BFS results) |
| `<crate>.analysis` | Pre-analysis output (items, edges, impls) |
| `<crate>.seeds` | Cross-crate BFS seed items |
| `<crate>.skip-driver` | Marker: no unmarked functions, skip driver loading |

Clear the cache when the driver binary changes or when analysis results seem stale:

```bash
rm -rf .slicer-cache/
```

---

## Troubleshooting

### WSL: sccache permission errors on `/mnt/` drives

Building on Windows-mounted drives (`/mnt/c/`, `/mnt/d/`, etc.) causes sccache to fail with `failed to set permissions` errors because NTFS does not support Unix file permissions.

**Fix**: Build on the native Linux filesystem instead:

```bash
cd ~
git clone <repo> cargo-slicer
cd cargo-slicer
cargo install --path .
```

### `RUSTC_WRAPPER` interferes with building cargo-slicer itself

If you previously set `RUSTC_WRAPPER` for virtual slicing, it will intercept compilation of cargo-slicer's own dependencies. Unset it before building cargo-slicer:

```bash
unset RUSTC_WRAPPER CARGO_SLICER_VIRTUAL CARGO_SLICER_CODEGEN_FILTER
cargo install --path .
```

Only set `RUSTC_WRAPPER` when building your *target project*, not cargo-slicer itself.

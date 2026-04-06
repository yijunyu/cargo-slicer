# Usage Reference

## Virtual Slicing (recommended)

### Linux / macOS

```bash
# Install nightly driver (one-time)
cargo +nightly install cargo-slicer \
  --features rustc-driver \
  --bin cargo-slicer-rustc \
  --bin cargo_slicer_dispatch

# Pre-analyze workspace call graph (seconds)
cargo-slicer pre-analyze

# Build with virtual slicing
cargo clean
CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +nightly build --release
```

### WSL on Windows drives (`/mnt/c/`, `/mnt/d/`, …)

Same as above but disable sccache to avoid NTFS permission errors:

```bash
SCCACHE_IDLE_TIMEOUT=0 cargo +nightly install cargo-slicer \
  --features rustc-driver \
  --bin cargo-slicer-rustc \
  --bin cargo_slicer_dispatch

cargo-slicer pre-analyze

cargo clean
CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  CARGO_SLICER_SCCACHE=/nonexistent \
  RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +nightly build --release
```

## Subcommands

| Subcommand | Description |
|------------|-------------|
| *(default)* | Source slicing: copy deps, delete unused items |
| `build [ARGS]` | Slice deps then build with sliced crates |
| `pre-analyze [--parser BACKEND]` | Cross-crate static analysis for virtual slicing |
| `generate [-o DIR] [--delete]` | Write a sliced source copy without modifying the original |
| `rl-bench [OPTIONS]` | Measure compile speedup as RL training KPIs |

## Pre-analysis parser backends

```bash
cargo-slicer pre-analyze                # syn (default, most accurate)
cargo-slicer pre-analyze --parser fast  # fast tokenizer
cargo-slicer pre-analyze --parser ctags # items only, no call edges
```

| Backend | Speed | Call edges | Use when |
|---------|-------|------------|----------|
| `syn` | 0.5–12 s | Yes, accurate | Default — best stubs |
| `fast` | < 1 s | Yes, approximate | Large workspaces, time-sensitive |
| `ctags` | Fastest | None | Items-only analysis |

## Source slicing (stable, no nightly)

```bash
cargo-slicer                # slice all deps
cargo-slicer regex          # slice one crate
cargo-slicer --clean        # clean and re-slice
cargo-slicer -O             # fast production mode (skip verification)
cargo-slicer build --release  # slice + build
```

### Optimization levels

| Level | Description |
|-------|-------------|
| `-O0` | No deletion — safe baseline |
| `-O1` | Delete private functions (with verification) |
| `-O2` | Delete all private items + trial deletion |
| `-O3` | Graph-guided deletion (default) |
| `-O` | Fast production — skip verification |

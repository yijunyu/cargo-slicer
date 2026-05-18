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
| `script <file.rs> [args...]` | Run a nightly `-Zscript` single-file Rust script with slicing enabled |
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

## cargo-script (nightly `-Zscript`)

Run a single-file Rust script (cargo's nightly `-Zscript` feature) with
cargo-slicer enabled. The only user-visible change is the shebang line:

```rust
#!/usr/bin/env -S cargo-slicer script
---cargo
[dependencies]
regex = "1"
---

fn main() {
    let re = regex::Regex::new(r"\w+").unwrap();
    println!("{:?}", re.find("hello world"));
}
```

Make it executable and run it directly:

```bash
chmod +x hello.rs
./hello.rs
```

Or invoke explicitly:

```bash
cargo-slicer script hello.rs [args...]
```

### What it does

1. Sets `CARGO_SLICER_VIRTUAL=1`, `CARGO_SLICER_CODEGEN_FILTER=1`, and
   `CARGO_SLICER_CACHE_DIR=$TMPDIR/cargo-slicer-script-<hash>` (keyed by the
   absolute script path, so caches don't litter the user's cwd).
2. Detects whether the active nightly already has `-Z dead-fn-elimination`
   (the in-tree patch). If so, it skips `RUSTC_WRAPPER` and passes the flag
   via `CARGO_ENCODED_RUSTFLAGS` (Fast Path 3). Otherwise it sets
   `RUSTC_WRAPPER=cargo_slicer_dispatch` and uses the userspace driver.
3. `exec`s `cargo +nightly -Zscript <file> [args...]`.

### Caveats

- `-Zscript` is unstable and only available on nightly.
- For tiny single-file scripts the slicer's contribution is mostly eliminating
  dead code in registry dependencies (the userspace driver path skips the
  one-crate script body by the auto skip-threshold heuristic). The
  `-Z dead-fn-elimination` fast path applies to both.
- No pre-analysis step runs — there's no workspace `Cargo.toml` to walk.
  The in-tree flag does its own BFS from the script's `main()`, so this is
  fine for the upstream-flag path.

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

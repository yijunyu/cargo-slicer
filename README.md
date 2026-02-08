# cargo-slicer

**Speed up Rust release builds by skipping codegen for unreachable functions.**

## Quick Start

```bash
# 1. Install cargo-slicer (no RUSTC_WRAPPER)
unset RUSTC_WRAPPER CARGO_SLICER_VIRTUAL CARGO_SLICER_CODEGEN_FILTER
rustup component add rust-src rustc-dev llvm-tools-preview
cargo install --path .
cargo +nightly install --path . --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver

# 2. (Optional) Pre-analyze for cross-crate reachability
cargo-slicer pre-analyze

# 3. Build your target project with virtual slicing
cargo clean
CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  RUSTC_WRAPPER=~/.cargo/bin/cargo_slicer_dispatch \
  cargo +nightly build --release
```

![A self-slicing demo run on Windows Subsystem for Linux](demo.gif)

On WSL with NTFS drives (`/mnt/c/`, `/mnt/d/`), add `CARGO_SLICER_SCCACHE=/nonexistent` and `SCCACHE_IDLE_TIMEOUT=0` to disable sccache. See [docs/USAGE.md](docs/USAGE.md) for full details.

## Benchmark Results

| Project | Baseline (s) | Virtual Slicing (s) | Change |
|---------|-------------:|--------------------:|--------|
| **helix** | 91 | 18 | **-81%** |
| **bevy** | 105 | 35 | **-67%** |
| **zed** | 1018 | 482 | **-53%** |
| **slicer** | 167 | 79 | **-53%** |
| **nushell** | 111 | 91 | **-18%** |
| **rustc** | 172 | 147 | **-15%** |
| **ripgrep** | 15 | 13 | **-10%** |

Clean release builds, 3 runs averaged (Feb 2026). On 20 single-crate rustc-perf benchmarks, overhead is 0-11%.

## Documentation

- **[docs/USAGE.md](docs/USAGE.md)** — Configuration reference (env vars, CLI flags, benchmarking)
- **[docs/DESIGN.md](docs/DESIGN.md)** — Architecture and design decisions
- **[docs/HISTORY.md](docs/HISTORY.md)** — Development history

## Requirements

- Rust nightly

## License

MIT & Apache 2.0

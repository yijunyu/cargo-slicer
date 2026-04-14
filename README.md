# cargo-slicer — Quick Demo

One command to accelerate any Rust or C/C++ build.

## Docker (no installation needed)

```bash
cd /path/to/your/project
docker run --rm --cpus=48 \
  -v $(pwd):/workspace/project \
  ghcr.io/yijunyu/cargo-slicer:latest
```

**Must run from inside your project directory.** `$(pwd)` mounts it into the container.
Replace `48` with `$(nproc)` on your machine.

---

## Install (30 seconds)

```bash
curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash
```

Installs to `~/.cargo/bin/`. Detects glibc compatibility and builds from source if needed.

## Run

```bash
cd /path/to/your/project
build-accelerate.sh .
```

That's it. No config. No source changes. Auto-detects Rust or C/C++.

---

## What happens for a Rust project

```
=== cargo-slicer: using RUSTC_WRAPPER ===
=== Step 0/4: Warming registry dep cache (one-time) ===
    serde 1.0 ... cached
    syn 2.0  ... cached
    tokio 1  ... cached

=== Step 1/4: Pre-analyzing cross-crate call graph ===
    cargo-slicer: 312 seeds, 1,847 reachable, 217 stubbed (11.6% reduction)

=== Step 2/4: Computing critical-path priority plan ===
    Longest path: tokio → hyper → axum → my-app (estimated 43s)

=== Step 3/4: Building with priority scheduling + registry cache + MIR stubs ===
    Compiling my-app v0.1.0

real  1m22s   (baseline: 1m48s)   Speedup: 1.31×
Stubbed: 217 functions
```

## What happens for a C/C++ project

```
=== build-accelerate: C/C++ project detected ===
    Compiler: /usr/bin/clang++

=== Step 1/3: Auto-detecting fat header ===
    Top headers: llvm/Support/raw_ostream.h (4,821), llvm/ADT/SmallVector.h (3,102) ...
    Fat header: 5 LLVM headers

=== Step 2/3: Starting clang-daemon ===
    Socket: /tmp/build-accelerate-llvm.sock
    Waiting for PCH compilation ... done

=== Step 3/3: Building with daemon-accelerated compiler (-j48) ===
    ninja -j48   (CC = clang-daemon-client → daemon → clang++ -include-pch ...)

real  4m10s   (baseline: 5m08s)   Speedup: 1.22×
```

---

## Benchmark highlights

Bare-metal (48-core, identical RUSTFLAGS, interleaved rounds):

| Project | Baseline | Slicer | Speedup |
|---------|----------|--------|---------|
| helix (Rust, cold) | 68s | 44s | **1.55×** |
| ripgrep (Rust, cold) | 10.5s | 7s | **1.50×** |
| zed (Rust, cold) | 1098s | 767s | **1.43×** |
| zeroclaw (Rust, cold) | 686s | 522s | **1.31×** |
| nushell (Rust, cold) | 103s | 82s | **1.26×** |
| LLVM 21 (C++, -j48) | ~308s | ~252s | **1.22×** |

Docker (single-run, `cargo fetch` + `cargo clean` between runs):

| Project | Baseline | Slicer | Speedup |
|---------|----------|--------|---------|
| zed (209 crates) | 1149s | 545s | **2.11×** |
| helix (16 crates) | 95s | 59s | **1.61×** |
| zeroclaw (4 crates) | 842s | 542s | **1.55×** |
| ripgrep (17 crates) | 15s | 12s | **1.31×** |
| nushell (41 crates) | 118s | 94s | **1.25×** |

---

## Reproduce the evaluation

The Docker image bundles a `bench` entrypoint that runs a fair comparison
(shared `cargo fetch`, `cargo clean` between runs, slicer timing includes
pre-analyze).

### One command per project

```bash
# Clone a target project first
git clone --depth 1 https://github.com/BurntSushi/ripgrep /tmp/ripgrep
docker run --rm -v /tmp/ripgrep:/workspace/project \
  ghcr.io/yijunyu/cargo-slicer:latest bench
```

Output prints `Baseline: …s` / `Slicer: …s` / `Speedup: …x`.

### Project-specific flags

```bash
# helix — tree-sitter grammar fetch needs git safe.directory
git clone --depth 1 https://github.com/helix-editor/helix /tmp/helix
docker run --rm -v /tmp/helix:/workspace/project \
  -e GIT_CONFIG_COUNT=1 -e GIT_CONFIG_KEY_0=safe.directory \
  -e GIT_CONFIG_VALUE_0='*' \
  ghcr.io/yijunyu/cargo-slicer:latest bench

# zed — webrtc-sys C++20 needs clang, not gcc 11.x
git clone --depth 1 https://github.com/zed-industries/zed /tmp/zed
docker run --rm -v /tmp/zed:/workspace/project \
  -e CC=clang -e CXX=clang++ \
  ghcr.io/yijunyu/cargo-slicer:latest bench
```

### Run baseline and slicer separately

If you want to inspect each build independently (e.g. to capture logs):

```bash
docker run --rm -v /tmp/ripgrep:/workspace/project \
  ghcr.io/yijunyu/cargo-slicer:latest build-baseline
# then, in a fresh container (or after `cargo clean`):
docker run --rm -v /tmp/ripgrep:/workspace/project \
  ghcr.io/yijunyu/cargo-slicer:latest build-slicer
```

`build-slicer` runs: `cargo fetch` → `cargo-slicer pre-analyze` →
`cargo +nightly build --release` with the 3-layer `RUSTC_WRAPPER` chain
(`cargo_warmup_pch` → `cargo_warmup_dispatch` → `cargo_slicer_dispatch`).
`build-baseline` runs: `cargo fetch` → plain `cargo +nightly build --release`.

### Interactive debugging

```bash
docker run --rm -it -v /tmp/ripgrep:/workspace/project \
  --entrypoint bash ghcr.io/yijunyu/cargo-slicer:latest
# inside the container:
build-baseline
cargo clean      # required for a fair second run
build-slicer
```

### Bare-metal reproduction

For the 48-core bare-metal numbers (the first table above), use the
benchmark harness in the source tree:

```bash
git clone https://github.com/yijunyu/cargo-slicer /tmp/cargo-slicer
cd /tmp/cargo-slicer
./scripts/ci_bench_multicrate.sh   # 7 projects, baseline vs vslice-cc
# or a single project:
./scripts/bench_fresh_build.sh ripgrep vslice-cc 3
```

Results are written to `bench-results.db` (SQLite) with an auto-generated
HTML report.

---

## Manual C/C++ usage (without build-accelerate.sh)

```bash
# Start the daemon (detects fat header from compile_commands.json)
export CLANG_DAEMON_FAT_HDR='#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallVector.h"'
clang-daemon-server --daemon --socket /tmp/cds.sock &
export CLANG_DAEMON_SOCKET=/tmp/cds.sock

# Build with the drop-in client (transparent fallback on error)
make -j$(nproc) CC=clang-daemon-client CXX=clang-daemon-client
# or: ninja -j$(nproc)  (cmake: -DCMAKE_CXX_COMPILER=clang-daemon-client)
```

## Manual Rust usage (without cargo-slicer.sh)

```bash
# One-time: warm registry dep cache
cargo-warmup init --tier=1

# Per-project: pre-analyze call graph
cd /path/to/project
cargo-slicer pre-analyze

# Build with full 3-layer wrapper chain
RUSTC_WRAPPER=$(which cargo_warmup_pch) \
  CARGO_WARMUP_INNER_WRAPPER=$(which cargo_warmup_dispatch) \
  CARGO_WARMUP_INNER_WRAPPER2=$(which cargo_slicer_dispatch) \
  CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  cargo +nightly build --release
```

---

See [README.md](README.md) for full documentation and [docs/DESIGN.md](docs/DESIGN.md) for architecture details.

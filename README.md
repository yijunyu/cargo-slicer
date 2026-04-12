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

real  1m22s   (baseline: 2m23s)   Speedup: 1.74×
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

| Project | Before | After | Speedup |
|---------|--------|-------|---------|
| nushell (cold) | 597s | 117s | **5.1×** |
| zed (incremental) | 1,025s | 744s | **1.38×** |
| cargo-slicer itself | 143s | 82s | **1.74×** |
| LLVM 21 (-j48) | ~308s | ~252s | **1.22×** |

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

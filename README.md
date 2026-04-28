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

### ASE 2026 corpus sweep — 2,669 crates from crates.io

| Crates fetched | Both legs built | Slicer regressions | Median speedup |
|---------------:|----------------:|-------------------:|---------------:|
| 2,669          | **2,452**       | **0**              | **1.50×**      |

Independent third-party validation on the top crates by downloads. 73% of
crates show ≥1.0× speedup, 50% show ≥1.5×, 36% show ≥2.0×. Zero correctness
regressions — every crate the baseline could build, the slicer also built
with identical binary output (or matching `--version`/`--help` for binary
crates).

Full corpus catalog (all 2,669 crates with per-crate timings):
[**ASE 2026 Corpus**](https://yijunyu.github.io/cargo-slicer/ase2026-corpus.html)
· [CSV](docs/ase2026-corpus.csv)

### Real-world projects (Apr 2026, 48-core, fair RUSTFLAGS)

| Project | Before | After | Speedup |
|---------|--------|-------|---------|
| helix (Rust, cold) | 68s | 44s | **1.55×** |
| ripgrep (Rust, cold) | 10.5s | 7s | **1.50×** |
| zed (Rust, cold) | 1098s | 767s | **1.43×** |
| zeroclaw (Rust, cold) | 686s | 522s | **1.31×** |
| nushell (Rust, cold) | 103s | 82s | **1.26×** |
| LLVM 21 (C++, -j48) | ~308s | ~252s | **1.22×** |

### Upstream `-Z dead-fn-elimination` patch (in-tree rustc MCP)

The same algorithm has been proposed for inclusion in rustc itself
(see [`docs/upstream-rfc.md`](docs/upstream-rfc.md)). The patch has been
through a [nine-point review by @petrochenkov](docs/vadim-petrochenkov-review-feedback.md)
(V1–V9); the [response document](docs/vadim-response-results.md) covers
each concern, the resulting patches P1–P9, and the empirical answer to V9.

| Project | Baseline | `-Z dead-fn-elimination` | Reduction |
|---------|---------:|-------------------------:|----------:|
| zed | 1,790s | 1,238s | **−31%** |
| rustc workspace (67 crates) | 336s | 176s | **−48%** |
| ripgrep (stage1 1.90 oracle) | 62.1s | 59.9s | **904 fns eliminated, output identical** |

When the real rustc supports the flag, `cargo_slicer_dispatch` auto-detects
it and delegates — no userspace driver, no ABI shims. Falls back to the
userspace path otherwise.

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

# Benchmarks

All numbers are cold builds (after `cargo clean`) on a 48-core Linux server
with nightly Rust.

## Virtual slicer — rust-perf standard suite

| Project | Baseline | cargo-slicer | Speedup |
|---------|----------|-------------|---------|
| image 0.25.6 (lib) | 40,742 ms | 1,461 ms | **27.9×** |
| ripgrep 14.1.1 (bin) | 24,094 ms | 5,891 ms | **4.09×** |
| cargo 0.87.1 (workspace) | 133,797 ms | 61,922 ms | **2.16×** |
| diesel 2.2.10 (lib) | 25,854 ms | 14,339 ms | **1.80×** |
| syn 2.0.101 (lib) | 6,711 ms | 4,157 ms | **1.61×** |
| serde 1.0.219 (lib) | 3,951 ms | 3,966 ms | **1.00×** |

`serde` is already minimal — almost all of its code is reachable via derive
macros. The slicer correctly identifies this.

## Virtual slicer — real binary projects

| Project | Baseline | cargo-slicer | Speedup | Notes |
|---------|----------|-------------|---------|-------|
| **zeroclaw** (4 local crates) | 686 s | 522 s | **1.31× (23.9% faster)** | 3,786 stubs / ~241k mono items (1.6% overall, 4.4% bin); speedup from seed-guided codegen filtering on the binary target |
| nushell (41 local crates) | 596,593 ms | 108,488–126,587 ms | **4.7–5.5×** | 644 stubs |
| zed (232 local crates, warm cache) | 505,023 ms | ~355,000 ms | **1.4×** | local crates dominate |

> **zeroclaw measurement history (2026-04-11)**: a prior version of this table
> reported zeroclaw at **15.8×**. That number was invalid — it came from a run
> where the sliced side errored out before producing a binary. Three bugs were
> responsible: (1) pre-analysis keyed cache files by the package name
> `zeroclawlabs` but the compiler sees the target name `zeroclaw` from
> `[lib] name = "zeroclaw"`, so every compilation unit was a cache miss;
> (2) the driver's `mir_built` edge-scanning mode was wrongly allowed to run
> during default-phase builds on binary targets, causing an E0391 query cycle
> through five recursive async fns; (3) the `has_opaque_types()` guard in
> edge scanning was too narrow and missed coroutine/alias substs. All three
> are now fixed. An interim session showed **0.82×** (baseline 511 s,
> vslice-cc 623 s) and was documented as a regression; that session's baseline
> never reproduced. A follow-up 3×3-runs measurement showed **1.29×** (baseline
> 681 s, vslice-cc 528 s), and an interleaved 3-round measurement — round 1:
> 686 s / 524 s; round 2: 688 s / 519 s; round 3: 685 s / 524 s — confirmed
> **1.31×** with baseline spread ±0.2% and vslice-cc spread ±0.5%. Interleaving
> rounds (baseline, vslice-cc, baseline, vslice-cc, …) rules out thermal and
> run-order effects. The speedup does not come from warm-cache hits — no
> cargo-warmup cache exists on the measurement machine; both modes compile
> from scratch. It comes from seed-guided codegen filtering: BFS pre-analysis
> identifies unreachable items per crate, and the driver skips LLVM codegen
> for them. Only 1.6% of zeroclaw's mono items are stubbed overall, but the
> binary target has 4.4% stub density (3,184 of 72,744 mono items) and those
> items carry disproportionate LLVM cost, producing the ~160 s wall-clock win.

## Warm-cache daemon — rust-perf suite

| Crate | Baseline | Warmed | Speedup |
|-------|----------|--------|---------|
| image 0.25.6 | 40,742 ms | 4,800 ms | **8.5×** |
| cargo 0.87.1 | 133,797 ms | 58,000 ms | **2.3×** |
| syn 2.0.101 | 6,711 ms | 4,040 ms | **1.66×** |

A warm cache populated by one project is reused across all projects on the same
machine.

## Upstream -Z dead-fn-elimination patch

| Project | Baseline | -Z dead-fn-elimination | Reduction |
|---------|----------|----------------------|-----------|
| zed | 1,790 s | 1,238 s | **−31%, 9.2 min saved** |
| rustc | 336 s | 176 s | **−48%, 2.7 min saved** |
| ripgrep | 13 s | 13 s | break-even (all fns reachable) |

## C/C++ projects — clang-daemon PCH acceleration

`build-accelerate.sh` (included in the image) auto-detects C/C++ projects and
injects a precompiled header via `clang-daemon`. The technique eliminates
repeated header parsing across parallel compilation units.

**Already benchmarked** (48-core server, Clang 21, `-j48`):

| Project | Stars | Files | Baseline | Accelerated | Speedup | Notes |
|---------|-------|-------|----------|-------------|---------|-------|
| Linux kernel 6.14 | 227k | 26,339 | ~890 s | ~730 s | **1.22×** | GCC fallback for asm-heavy files |
| LLVM 20 | — | ~2,873 | measured | measured | **1.22×** | Clang 21 compiling Clang 20 |
| LLVM 21 | — | ~2,873 | measured | measured | **1.24×** | Self-hosted build |
| vim | — | ~300 | baseline | accelerated | **1.3×** | Small project, overhead minimal |
| sqlite3 | — | 1 (amalgam) | 20 s | 20.2 s | **1.01×** | Single-file; PCH gives nothing |

**Predicted speedup for top starred projects** (based on file count × header density model):

| Rank | Project | Stars | Lang | Files | LOC | Build | Predicted | Reason |
|------|---------|-------|------|-------|-----|-------|-----------|--------|
| 1 | [Linux](https://github.com/torvalds/linux) | 227k | C | 26,339 | ~20M | Make | **1.2×** ✅ benchmarked |
| 2 | [TensorFlow](https://github.com/tensorflow/tensorflow) | 195k | C++ | ~650 | ~2.5M | Bazel/CMake | **1.15–1.25×** | Heavy STL + proto headers |
| 3 | [Godot](https://github.com/godotengine/godot) | 109k | C++ | ~3,500 | ~8.6M | SCons | **1.2–1.3×** | Large header graph |
| 4 | [Electron](https://github.com/electron/electron) | 121k | C++ | (Chromium) | ~25M | ninja | **1.2×** | Chromium-scale header reuse |
| 5 | [OpenCV](https://github.com/opencv/opencv) | 87k | C++ | ~1,000 | ~600K | CMake | **1.15–1.2×** | Dense OpenCV headers |
| 6 | [FFmpeg](https://github.com/FFmpeg/FFmpeg) | 58k | C | ~500 | ~1M | autotools | **1.1–1.2×** | libav* headers per file |
| 7 | [Bitcoin](https://github.com/bitcoin/bitcoin) | 89k | C++ | ~500 | ~750K | CMake | **1.1–1.2×** | Boost + secp256k1 headers |
| 8 | [Netdata](https://github.com/netdata/netdata) | 78k | C | ~700 | ~700K | CMake | **1.1–1.15×** | Moderate header depth |
| 9 | [Redis](https://github.com/redis/redis) | 74k | C | ~250 | ~330K | Make | **1.05–1.1×** | Shallow headers, small codebase |
| 10 | [Git](https://github.com/git/git) | 60k | C | ~400 | ~140K | Make | **1.05–1.1×** | Minimal headers |
| — | [llama.cpp](https://github.com/ggml-org/llama.cpp) | 102k | C++ | ~150 | ~250K | CMake | **1.05×** | Small; GGML headers not dense |
| — | [sqlite3](https://github.com/sqlite/sqlite) | — | C | 1 | ~255K | Make | **≈1×** | Amalgamation; no parallelism |

**Key insight**: speedup scales with (files × header parse fraction). Projects with
thousands of files each including the same heavyweight headers (Linux, Godot,
TensorFlow, Chromium) get the most benefit. Single-file amalgamations (sqlite3)
and projects with shallow headers (Redis, Git) get little to none.

To run against any of these projects:

```bash
# Clone and accelerate (auto-detects C/C++ via compile_commands.json or Makefile)
git clone https://github.com/torvalds/linux
build-accelerate.sh ./linux

# Or via Docker (mounts your checkout)
docker run --rm --cpus=48 \
  -v $(pwd)/linux:/workspace/project \
  ghcr.io/yijunyu/cargo-slicer:latest
```

> For projects using SCons (Godot) or Bazel (TensorFlow), generate
> `compile_commands.json` first:
> ```bash
> # Godot
> scons compiledb
> # TensorFlow (CMake path)
> cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -B build && cp build/compile_commands.json .
> ```

## Running benchmarks yourself

```bash
# Multi-crate CI benchmark (7 projects, baseline vs vslice-cc, 3 runs each)
./scripts/ci_bench_multicrate.sh

# Individual project
./scripts/bench_fresh_build.sh nushell baseline 3
./scripts/bench_fresh_build.sh nushell vslice-cc 3

# RL training KPI report
cargo-slicer rl-bench --project /tmp/your-project --runs 2
```

Results are stored in `bench-results.db` (SQLite).

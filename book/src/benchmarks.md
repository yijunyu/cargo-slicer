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

All measurements use identical RUSTFLAGS for both baseline and vslice-cc
(`-Z threads=8 -C linker=clang -C link-arg=--ld-path=wild`). 48-core machine,
Apr 2026, 2–3 runs per mode.

| Project | Baseline | vslice-cc | Speedup | Notes |
|---------|----------|-----------|---------|-------|
| **ripgrep** (50K LOC) | 10.5 s | 7 s | **1.50×** | |
| **zeroclaw** (4 local crates) | 686 s | 522 s | **1.31×** | 3,786 stubs / ~241k mono items (1.6% overall, 4.4% bin) |
| **nushell** (41 local crates) | 103 s | 82 s | **1.26×** | |

> **Retracted claims**: an earlier version of this table reported nushell at
> **5.1×** (597 s → 117 s). That was an apples-to-oranges comparison: the
> baseline was measured *without* `-Z threads=8` and the wild linker, while
> the vslice-cc run had them enabled. With identical RUSTFLAGS, nushell's
> honest speedup is 1.26×. Similarly, ripgrep was listed at 1.10× (13.4 s →
> 12.2 s) with old RUSTFLAGS; the current measurement shows 1.50×.
>
> Zed (1.38×), helix (1.26×), cargo-slicer (1.74×), and rustc-perf (1.18×)
> have not yet been re-verified with the current protocol and are omitted
> until they are.

## Docker image — `build-slicer` vs plain `cargo build`

The Docker image (`ghcr.io/yijunyu/cargo-slicer:latest`) includes a pre-warmed
registry cache, so the full pipeline benefits from both warm-cache hits and
codegen filtering. Sequential runs, clean `target/` before each:

| Project | Baseline | `build-slicer` | Speedup |
|---------|----------|----------------|---------|
| **zeroclaw** (4 crates) | 794 s | 547 s | **1.45×** |

The Docker baseline is slower than host-native (794 s vs 686 s) due to
container overhead, but `build-slicer` compensates via the warm registry
cache (pre-compiled `.rlib` files for registry deps). The extra ~14% boost
over the host-native 1.31× comes from this cache layer.

## Warm-cache daemon — verified (Apr 2026)

Both baseline and warmed use nightly + `-Z threads=8`. Interleaved rounds,
dispatch pre-warmed, `rm -rf target/` before each run.

| Crate | Baseline | Warmed | Speedup |
|-------|----------|--------|---------|
| image 0.25 | 4.9 s | 2.1 s | **2.3×** |
| syn 2.0 | 1.0 s | 0.66 s | **1.5×** |

> An earlier version of this table claimed **8.5×** for image (40.7 s →
> 4.8 s) and **1.7×** for syn (6.7 s → 4.0 s). Those baselines were
> measured without `-Z threads=8` and the wild linker, while the warmed
> runs had them — the same apples-to-oranges error as the nushell 5.1×.
> cargo 0.87.1 (claimed 2.3×) has not yet been re-verified.

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

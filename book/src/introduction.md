# cargo-slicer

**Rust builds are slow. cargo-slicer makes them fast.**

A cold release build of [Zed](https://zed.dev) takes 17 minutes. With cargo-slicer it takes under 12. The `image` crate goes from 40 seconds to 1.4 seconds — **27.9×**.

These numbers come from two complementary techniques working together:

| Technique | What it does | Typical gain |
|-----------|-------------|-------------|
| **Virtual Slicer** | Stubs unreachable functions at the MIR level so LLVM never sees them | 1.6–28× per-crate |
| **Warm-Cache Daemon** | Pre-compiles registry crates once, serves cached `.rlib` files on every subsequent build | skips 100% of registry compilation |

You do not need to understand the internals to use them. The [all-in-one script](all-in-one.md) runs the full pipeline in one command.

## Real-World Results

### Per-crate speedups (virtual slicer alone)

| Project | Baseline | cargo-slicer | Speedup |
|---------|----------|-------------|---------|
| image 0.25.6 | 40.7 s | 1.5 s | **27.9×** |
| nushell (41 crates) | 597 s | 117 s | **4.7–5.5×** |
| ripgrep 14.1.1 | 24.1 s | 5.9 s | **4.1×** |
| cargo 0.87.1 | 134 s | 62 s | **2.2×** |
| zed (232 crates, warm) | 505 s | ~355 s | **1.4×** |
| zeroclaw (4 crates) | 686 s | 522 s | **1.31× (23.9% faster)** |

> zeroclaw has a volatile measurement history: an early draft claimed 15.8×
> (invalid — silent compile failure), a correction claimed 0.82× regression
> (an outlier session whose baseline never reproduced), and the current
> **1.31×** is from an interleaved 3-round measurement on 2026-04-11
> (baseline 686/688/685 s ±0.2%, vslice-cc 524/519/524 s ±0.5%). Interleaving
> rules out thermal and run-order effects. Only 4.4% of zeroclaw's *binary*
> mono items are stubbed (3,184 of 72,744), but those carry disproportionate
> LLVM cost — they produce the ~160 s wall-clock win. No warm registry cache
> is in play on this machine. See [benchmarks.md](benchmarks.md) for the full
> history and the three bug fixes.

### Registry-cache speedups (warm-cache daemon alone, rust-perf suite)

| Crate | Baseline | Warmed | Speedup |
|-------|----------|--------|---------|
| image 0.25.6 | 40.7 s | 4.8 s | **8.5×** |
| cargo 0.87.1 | 134 s | 58 s | **2.3×** |
| syn 2.0 | 6.7 s | 4.0 s | **1.7×** |

Combined, the two techniques multiply: registry deps are served from cache, local crate codegen is cut by the slicer, and only the irreducible minimum reaches LLVM.

## Requirements

- Rust **stable** (source slicing, warmup CLI)
- Rust **nightly** (virtual slicer — requires `rustc-driver` feature)
- Linux, macOS, or Windows (WSL recommended on Windows)

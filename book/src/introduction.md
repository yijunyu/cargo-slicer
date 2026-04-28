# cargo-slicer

**Rust builds are slow. cargo-slicer makes them fast.**

Two complementary techniques work together:

| Technique | What it does | Typical gain |
|-----------|-------------|-------------|
| **Virtual Slicer** | Stubs unreachable functions at the MIR level so LLVM never sees them | 1.2–1.5× per workspace |
| **Warm-Cache Daemon** | Pre-compiles registry crates once, serves cached `.rlib` files on every subsequent build | skips 100% of registry compilation |

You do not need to understand the internals to use them. The [all-in-one script](all-in-one.md) runs the full pipeline in one command.

## Real-World Results

### ASE 2026 corpus sweep — 2,669 crates, zero regressions

Independent validation on the top crates by downloads from crates.io:

| Crates fetched | Both legs built | Slicer regressions | Median speedup |
|---------------:|----------------:|-------------------:|---------------:|
| 2,669          | **2,452**       | **0**              | **1.50×**      |

Details and full per-crate catalog: [ASE 2026 Corpus](ase2026-corpus.md).

### Verified benchmarks (Apr 2026, host-native, no warm cache)

Both baseline and vslice-cc use identical RUSTFLAGS (`-Z threads=8`, wild linker).
2–3 runs per mode, 48-core machine.

| Project | Baseline | vslice-cc | Speedup |
|---------|----------|-----------|---------|
| helix (16 crates) | 68 s | 44 s | **1.55×** |
| ripgrep (50K LOC) | 10.5 s | 7 s | **1.50×** |
| zed (209 crates) | 1098 s | 767 s | **1.43×** |
| zeroclaw (4 crates) | 686 s | 522 s | **1.31×** |
| nushell (41 crates) | 103 s | 82 s | **1.26×** |

### Docker image (with pre-warmed registry cache)

| Project | Baseline | build-slicer | Speedup |
|---------|----------|-------------|---------|
| zeroclaw (4 crates) | 794 s | 547 s | **1.45×** |

### Registry-cache speedups (warm-cache daemon alone, verified Apr 2026)

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
> cargo 0.87.1 (claimed 2.3×) is a regression with fair RUSTFLAGS
> (dispatch overhead serializes the parallel build).

## Requirements

- Rust **stable** (source slicing, warmup CLI)
- Rust **nightly** (virtual slicer — requires `rustc-driver` feature)
- Linux, macOS, or Windows (WSL recommended on Windows)

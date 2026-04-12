# cargo-slicer

**Rust builds are slow. cargo-slicer makes them fast.**

Two complementary techniques work together:

| Technique | What it does | Typical gain |
|-----------|-------------|-------------|
| **Virtual Slicer** | Stubs unreachable functions at the MIR level so LLVM never sees them | 1.2–1.5× per workspace |
| **Warm-Cache Daemon** | Pre-compiles registry crates once, serves cached `.rlib` files on every subsequent build | skips 100% of registry compilation |

You do not need to understand the internals to use them. The [all-in-one script](all-in-one.md) runs the full pipeline in one command.

## Real-World Results

### Verified benchmarks (Apr 2026, host-native, no warm cache)

Both baseline and vslice-cc use identical RUSTFLAGS (`-Z threads=8`, wild linker).
2–3 runs per mode, 48-core machine.

| Project | Baseline | vslice-cc | Speedup |
|---------|----------|-----------|---------|
| ripgrep (50K LOC) | 10.5 s | 7 s | **1.50×** |
| zeroclaw (4 crates) | 686 s | 522 s | **1.31×** |
| nushell (41 crates) | 103 s | 82 s | **1.26×** |

### Docker image (with pre-warmed registry cache)

| Project | Baseline | build-slicer | Speedup |
|---------|----------|-------------|---------|
| zeroclaw (4 crates) | 794 s | 547 s | **1.45×** |

### Registry-cache speedups (warm-cache daemon alone, rust-perf suite)

These numbers have not yet been re-verified with the current protocol and
should be treated as provisional:

| Crate | Baseline | Warmed | Speedup |
|-------|----------|--------|---------|
| image 0.25.6 | 40.7 s | 4.8 s | **8.5×** |
| cargo 0.87.1 | 134 s | 58 s | **2.3×** |
| syn 2.0 | 6.7 s | 4.0 s | **1.7×** |

## Requirements

- Rust **stable** (source slicing, warmup CLI)
- Rust **nightly** (virtual slicer — requires `rustc-driver` feature)
- Linux, macOS, or Windows (WSL recommended on Windows)

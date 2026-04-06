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
| **zeroclaw** (4 local crates) | 1,560,753 ms | 98,802 ms | **15.8×** | |
| nushell (41 local crates) | 596,593 ms | 108,488–126,587 ms | **4.7–5.5×** | 644 stubs |
| zed (232 local crates, warm cache) | 505,023 ms | ~355,000 ms | **1.4×** | local crates dominate |

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

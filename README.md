# cargo-slicer

**Speed up builds by skipping codegen for unreachable functions and pre-warming
the compiler cache — for both Rust and C/C++ projects.**

![demo](demo.gif)

> **Quick demo**: [README.demo.md](README.demo.md) — annotated example output for Rust and C/C++ projects.

## Install

```bash
# Linux / macOS
curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash
```

```powershell
# Windows (PowerShell)
irm https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.ps1 | iex
```

## Use

```bash
# Auto-detects Rust or C/C++ project
build-accelerate.sh /path/to/your/project
```

```bash
# Rust-only script
cargo-slicer.sh /path/to/rust/project
```

```powershell
# Windows (Rust, PowerShell)
cargo-slicer.ps1 C:\path\to\your\project
```

No config files, no source changes, no build system edits.

`build-accelerate.sh` detects the project type and applies the right optimizations:

**Rust projects** (`Cargo.toml` present) — three complementary optimizations:

1. **cargo-warmup** — pre-compiles registry deps once; subsequent builds skip recompiling `serde`, `syn`, `tokio`, etc.
2. **Virtual slicing** — stubs unreachable functions at MIR level, reducing codegen work
3. **Critical-path scheduling** — starts highest-priority dependency chains first within `cargo build`

**C/C++ projects** (`compile_commands.json` / `CMakeLists.txt` / `Makefile`) — PCH injection:

1. **clang-daemon** — compiles a fat precompiled header once, injects it into every parallel compilation unit via a drop-in `CC`/`CXX` replacement
2. **Auto fat-header detection** — scans `compile_commands.json` for the most-included headers; falls back to heuristics for Linux kernel, LLVM, Qt, and generic C++ projects

## Results

### Rust — clean rebuild with warm registry (typical developer workflow)

Baseline: `cargo clean && cargo +nightly build --release` with the cargo-warmup registry
cache already populated (registry deps pre-compiled). "With cargo-slicer" adds virtual
slicing + critical-path scheduling on top of the warm registry cache.

| Project | Baseline (warm registry) | With cargo-slicer | Speedup |
|---------|--------------------------|-------------------|---------|
| **zed** (500K LOC) | 1,025s | 744s | **1.38×** |
| **rustc-perf** suite | 145s | 123s | **1.18×** |
| **cargo-slicer** itself | 143s | 82s | **1.74×** |
| **helix** (100K LOC) | 78s | 62s | **1.26×** |
| **ripgrep** (50K LOC) | 13.4s | 12.2s | **1.10×** |

### Rust — cold builds (CI / first clone, cargo-warmup alone)

Both columns use the warmup wrapper; "1st run" has an empty cache (all registry deps compile
from source), "2nd run" serves them from cache. Speedup = 1st ÷ 2nd run wall time.

Projects with many registry deps relative to local crates benefit most. Zed has 232 local
crates that cannot be pre-compiled (including a WebRTC C++ build script), so warmup only
helps its registry portion — hence a modest 1.4× vs 15.9× for zeroclaw.

| Project | 1st run (cache empty) | 2nd run (cache warm) | Speedup |
|---------|-----------------------|----------------------|---------|
| **zeroclaw** | 1,561s | 98s | **15.9×** |
| **nushell** | 597s | 117s | **5.1×** |
| **zed** | 505s | 355s | **1.4×** |

These baselines (505s–1,561s) are not directly comparable to the clean-rebuild baselines
above (78s–1,025s): the warmup benchmarks ran at an earlier toolchain snapshot on a
different machine configuration.

### C/C++ — parallel builds (clang-daemon + PCH, -j48)

Baseline is bare `clang++` at `-j48` (compile-only, `-o /dev/null`).
"With clang-daemon" injects the fat PCH into every translation unit via a Unix-socket
daemon; the PCH is compiled once and reused across all parallel jobs.

| Project | Baseline | With clang-daemon | Speedup |
|---------|----------|-------------------|---------|
| **LLVM 20** (2,915 TUs) | 190.9s | 153.7s | **1.24×** |
| **LLVM 21** (2,285 TUs) | 157.2s | 128.6s | **1.22×** |
| **Linux kernel** (2,873 TUs) | 73.9s (gcc) | 60.7s (clang21+PCH) | **1.22×** |

3 runs averaged, Apr 2026.

## How It Works

Read the blog series for a gentle introduction:

- **[Part I: The Waiting Game](docs/blog-part1-en.md)** — Why Rust builds are slow and what doesn't help
- **[Part II: The Gap](docs/blog-part2-en.md)** — The "separate compilation gap" and how much work is wasted
- **[Part III: Closing the Gap](docs/blog-part3-en.md)** — The four-step solution and results

Chinese version:
[第一篇：等待的艺术](docs/blog-part1-zh.md) |
[第二篇：缝隙](docs/blog-part2-zh.md) |
[第三篇：填缝](docs/blog-part3-zh.md)

## Documentation

- **[docs/USAGE.md](docs/USAGE.md)** — Configuration reference (env vars, CLI flags, benchmarking)
- **[docs/DESIGN.md](docs/DESIGN.md)** — Architecture and design decisions

## Build from Source

```bash
# Install all Rust binaries (stable)
cargo install --path .

# Install nightly driver (for virtual slicing)
cargo +nightly install --path . --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver

# Build C/C++ daemon (requires a C++17 compiler)
make -C clang-daemon
cp clang-daemon/clang-daemon-{server,client} ~/.cargo/bin/
```

Requires Rust nightly for the driver; stable Rust suffices for cargo-warmup alone.
The clang-daemon requires only a C++17 compiler (g++ or clang++).

## Contact

- **GitHub Issues**: [github.com/yijunyu/cargo-slicer/issues](https://github.com/yijunyu/cargo-slicer/issues)
- **Email**: yijun.yu@open.ac.uk

We'd love to hear how cargo-slicer works on your project.

## License

MIT & Apache 2.0

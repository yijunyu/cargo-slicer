# cargo-slicer

**Speed up Rust release builds by skipping codegen for unreachable functions and
pre-warming the registry dependency cache.**

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
# Linux / macOS — runs all optimizations in one command
cargo-slicer.sh /path/to/your/project
```

```powershell
# Windows (PowerShell)
cargo-slicer.ps1 C:\path\to\your\project
```

That's it. No config files, no source changes, no Cargo.toml edits.

`cargo-slicer.sh` applies three complementary optimizations automatically:

1. **cargo-warmup** — pre-compiles registry deps once; subsequent builds skip recompiling `serde`, `syn`, `tokio`, etc.
2. **Virtual slicing** — stubs unreachable functions at MIR level, reducing codegen work
3. **Critical-path scheduling** — starts highest-priority dependency chains first within `cargo build`

## Results

### Warm incremental builds (typical developer workflow)

| Project | Baseline | With cargo-slicer | Speedup |
|---------|----------|-------------------|---------|
| **zed** (500K LOC) | 1,025s | 744s | **1.38×** |
| **rustc-perf** suite | 145s | 123s | **1.18×** |
| **cargo-slicer** itself | 143s | 82s | **1.74×** |
| **helix** (100K LOC) | 78s | 62s | **1.26×** |
| **ripgrep** (50K LOC) | 13.4s | 12.2s | **1.10×** |

### Cold builds — CI / first clone (cargo-warmup alone)

| Project | Baseline (cold) | With cargo-warmup | Speedup |
|---------|-----------------|-------------------|---------|
| **zeroclaw** | 1,561s | 98s | **15.9×** |
| **nushell** | 597s | 117s | **5.1×** |
| **zed** | 505s | 355s | **1.4×** |

3 runs averaged, nightly toolchain (Apr 2026).

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
# Install all binaries (stable)
cargo install --path .

# Install nightly driver (for virtual slicing)
cargo +nightly install --path . --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver
```

Requires Rust nightly for the driver; stable Rust suffices for cargo-warmup alone.

## Contact

- **GitHub Issues**: [github.com/yijunyu/cargo-slicer/issues](https://github.com/yijunyu/cargo-slicer/issues)
- **Email**: yijun.yu@open.ac.uk

We'd love to hear how cargo-slicer works on your project.

## License

MIT & Apache 2.0

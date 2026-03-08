# cargo-slicer

**Speed up Rust release builds by skipping codegen for unreachable functions.**

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
# Linux / macOS
cargo-slicer.sh /path/to/your/project
```

```powershell
# Windows (PowerShell)
cargo-slicer.ps1 C:\path\to\your\project
```

That's it. No config files, no source changes, no Cargo.toml edits.

## Results

| Project | Baseline | With cargo-slicer | Speedup |
|---------|----------|-------------------|---------|
| **zed** (500K LOC) | 1,012s | 719s | **-29%** |
| **rustc** (600K LOC) | 135.8s | 112.4s | **-17%** |
| **zeroclaw** (86K LOC) | 192.9s | 170.4s | **-12%** |
| **helix** (100K LOC) | 71.2s | 66.6s | **-6%** |
| **ripgrep** (50K LOC) | 11.1s | 10.7s | **-4%** |

Clean release builds, 3 runs averaged (Feb 2026).

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
# Install all binaries
cargo install --path .
cargo +nightly install --path . --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver
```

Requires Rust nightly.

## Contact

- **GitHub Issues**: [github.com/yijunyu/cargo-slicer/issues](https://github.com/yijunyu/cargo-slicer/issues)
- **Email**: yijun.yu@open.ac.uk

We'd love to hear how cargo-slicer works on your project.

## License

MIT & Apache 2.0

# Getting Started

## Docker (quickest start — no installation needed)

Pull the pre-built image and run it against any Rust project:

```bash
docker run --rm --cpus=48 \
  -v $(pwd):/workspace/project \
  ghcr.io/yijunyu/cargo-slicer:latest
```

The image includes all binaries (`cargo-slicer-rustc`, `cargo_warmup_pch`, etc.) and a
pre-warmed registry cache. `--cpus=48` ensures the container uses all available cores.
Replace `48` with the output of `nproc` on your machine.

> **First run**: the container runs `cargo-slicer pre-analyze` automatically if no
> `.slicer-cache/` directory is found, then builds with the full 3-layer pipeline.

---

## Install

```bash
# Stable binary (source slicing + warmup CLI)
cargo install cargo-slicer

# Nightly driver (virtual slicer — the fast path)
cargo +nightly install cargo-slicer \
  --features rustc-driver \
  --bin cargo-slicer-rustc \
  --bin cargo_slicer_dispatch
```

If you are building from source:

```bash
git clone https://github.com/yijunyu/cargo-slicer
cd cargo-slicer

cargo install --path .
cargo +nightly install --path . --profile release-rustc \
  --features rustc-driver \
  --bin cargo-slicer-rustc \
  --bin cargo_slicer_dispatch
```

> **WSL on Windows drives** (`/mnt/c/`, `/mnt/d/`): prefix every `cargo install` with
> `SCCACHE_IDLE_TIMEOUT=0` to avoid NTFS permission errors. See
> [Troubleshooting](troubleshooting.md).

## Quickstart: one command

Run the full pipeline against your project:

```bash
cd your-project
cargo-slicer.sh .
```

This runs four steps automatically:

1. **Warm** the registry cache (`cargo-warmup init --tier=1`)
2. **Pre-analyze** the workspace call graph
3. **Plan** the critical compilation path
4. **Build** with the three-layer `RUSTC_WRAPPER` chain

On the first run the warmup step takes ~10 minutes (it compiles the top-tier
registry crates once). Every subsequent cold build is served from cache.

## Manual setup (step by step)

If you prefer to control each step:

```bash
# Step 1: warm the registry cache (one-time, ~10 min)
cargo-warmup init --tier=1

# Step 2: pre-analyze the workspace (seconds)
cd your-project
cargo-slicer pre-analyze

# Step 3: build with virtual slicing
cargo clean
CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +nightly build --release
```

> **Important**: never set `RUSTC_WRAPPER` when building cargo-slicer itself.
> Unset it before running `cargo install --path .`.

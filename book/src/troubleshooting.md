# Troubleshooting

## `RUSTC_WRAPPER` breaks building cargo-slicer itself

**Symptom**: `cargo install --path .` fails with mysterious compilation errors.

**Cause**: `RUSTC_WRAPPER=cargo_slicer_dispatch` is set in your environment from
a previous virtual-slicing session. It intercepts compilation of cargo-slicer's
own dependencies.

**Fix**: Unset it before building cargo-slicer:

```bash
unset RUSTC_WRAPPER CARGO_SLICER_VIRTUAL CARGO_SLICER_CODEGEN_FILTER
cargo install --path .
```

Only set `RUSTC_WRAPPER` when building your *target project*.

## sccache permission errors on WSL `/mnt/` drives

**Symptom**: `failed to set permissions` errors during `cargo install` on `/mnt/c/` or `/mnt/d/`.

**Cause**: NTFS does not support Unix file permissions. sccache creates files
with Unix permissions that NTFS cannot store.

**Fix**: Build on the native Linux filesystem:

```bash
cd ~
git clone https://github.com/yijunyu/cargo-slicer
cd cargo-slicer
SCCACHE_IDLE_TIMEOUT=0 cargo install --path .
```

Or, if you must stay on the Windows drive, disable sccache entirely:

```bash
SCCACHE_IDLE_TIMEOUT=0 cargo install --path .
# and when building your project:
CARGO_SLICER_SCCACHE=/nonexistent RUSTC_WRAPPER=... cargo +nightly build --release
```

## Stale `.slicer-cache/` after updating the driver

**Symptom**: unexpected stub failures or missed stubs after upgrading cargo-slicer.

**Fix**: Delete the cache:

```bash
rm -rf .slicer-cache/
```

## Nightly toolchain mismatch

**Symptom**: `cargo-slicer-rustc` crashes at startup with a `rustc_private` ABI error.

**Cause**: The driver binary was compiled against a different nightly than the one currently active.

**Fix**: Rebuild the driver against the active nightly:

```bash
rustup update nightly
cargo +nightly install --path . --profile release-rustc \
  --features rustc-driver \
  --bin cargo-slicer-rustc \
  --bin cargo_slicer_dispatch
```

## Build succeeds but no speedup

**Likely causes**:

1. Check `.cargo-slicer-debug.log` for `skip-driver` markers — all crates skipped means the threshold is too aggressive. Fix: `CARGO_SLICER_SKIP_THRESHOLD=0`.

2. The project is a library crate with no binary entry point. The slicer is most effective on binary crates with deep dependency trees.

3. Pre-analysis was not run. Run `cargo-slicer pre-analyze` first.

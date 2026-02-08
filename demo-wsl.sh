cargo clean
SCCACHE_IDLE_TIMEOUT=0 CARGO_SLICER_SCCACHE=/nonexistent /usr/bin/time -f %e cargo +nightly build --release
rm -rf .slicer-cache/
cargo clean
/usr/bin/time -f %e cargo-slicer pre-analyze
SCCACHE_IDLE_TIMEOUT=0 CARGO_SLICER_SCCACHE=/nonexistent CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 RUSTC_WRAPPER=~/.cargo/bin/cargo_slicer_dispatch /usr/bin/time -f %e cargo +nightly build --release

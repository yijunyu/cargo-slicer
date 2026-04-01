#!/usr/bin/env bash
# cargo-slicer.sh — Accelerated Rust build via dead function elimination
#                   + registry crate pre-compilation cache (cargo-warmup)
#
# Usage:
#   ./cargo-slicer.sh                    # build --release in current dir
#   ./cargo-slicer.sh /path/to/project   # build a specific project
#   ./cargo-slicer.sh . --features foo   # extra args passed to cargo build
#
# Two complementary optimizations applied together:
#   1. cargo-warmup  — serves pre-compiled .rlib/.so for registry deps (serde, syn, tokio...)
#   2. cargo-slicer  — stubs unreachable functions in local crates (codegen filtering)
#
# Prefers the in-tree -Z dead-fn-elimination flag (patched nightly) for step 2.
# Falls back to RUSTC_WRAPPER approach if the flag is not available.
#
# Prerequisites (flag path — recommended):
#   - Patched nightly with -Z dead-fn-elimination compiled from rust-lang/rust
#     with the dead_fn_elim.rs patch applied (see docs/upstream-rfc.md)
#
# Prerequisites (RUSTC_WRAPPER fallback):
#   - Rust nightly toolchain:  rustup toolchain install nightly
#   - cargo-slicer installed:  cargo install --path /path/to/cargo-slicer
#   - Driver installed:        cargo +nightly install --path /path/to/cargo-slicer \
#                                 --profile release-rustc \
#                                 --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
#                                 --features rustc-driver

set -euo pipefail

# ── Parse arguments ──────────────────────────────────────────────────────

PROJECT_DIR="."
EXTRA_ARGS=()

if [[ $# -ge 1 && -d "$1" ]]; then
    PROJECT_DIR="$1"
    shift
fi
EXTRA_ARGS=("$@")

PROJECT_DIR="$(cd "$PROJECT_DIR" && pwd)"

if [[ ! -f "$PROJECT_DIR/Cargo.toml" ]]; then
    echo "Error: No Cargo.toml found in $PROJECT_DIR" >&2
    exit 1
fi

# Verify nightly is available
if ! rustup run nightly rustc --version &>/dev/null; then
    echo "Error: Rust nightly toolchain not installed." >&2
    echo "Install: rustup toolchain install nightly" >&2
    exit 1
fi

# ── Detect -Z dead-fn-elimination support ────────────────────────────────
# Check if the active nightly has the in-tree flag (patched rustc).
# The flag appears in `rustc -Z help` output when the patch is applied.

HAS_ZFLAG=false
if rustup run nightly rustc -Z help 2>&1 | grep -q 'dead-fn-elimination'; then
    HAS_ZFLAG=true
fi

# ── Path A: In-tree -Z flag (no driver binary required) ─────────────────

if [[ "$HAS_ZFLAG" == "true" ]]; then
    echo "=== cargo-slicer: using -Z dead-fn-elimination (in-tree flag) ==="
    cd "$PROJECT_DIR"

    WARMUP_DISPATCH="$(command -v cargo_warmup_dispatch 2>/dev/null || true)"
    WARMUP_CLI="$(command -v cargo-warmup 2>/dev/null || true)"

    if [[ -n "$WARMUP_DISPATCH" && -n "$WARMUP_CLI" ]]; then
        WARMUP_CACHE_MARKER="${CARGO_HOME:-$HOME/.cargo}/warmup-cache/.initialized"
        if [[ ! -f "$WARMUP_CACHE_MARKER" ]]; then
            echo "=== Warming registry dep cache (one-time per toolchain) ==="
            "$WARMUP_CLI" init --tier=1 2>&1
            touch "$WARMUP_CACHE_MARKER" 2>/dev/null || true
            echo ""
        fi
        echo "=== Building with -Z dead-fn-elimination + registry cache ==="
        exec env RUSTC_WRAPPER="$WARMUP_DISPATCH" \
            cargo +nightly build --release \
            --config 'build.rustflags=["-Z", "dead-fn-elimination"]' \
            "${EXTRA_ARGS[@]}"
    else
        exec cargo +nightly build --release \
            --config 'build.rustflags=["-Z", "dead-fn-elimination"]' \
            "${EXTRA_ARGS[@]}"
    fi
fi

# ── Path B: RUSTC_WRAPPER fallback (pre-patch nightly) ──────────────────

echo "=== cargo-slicer: -Z dead-fn-elimination not available, using RUSTC_WRAPPER ==="
echo "    (Apply the patch from docs/upstream-rfc.md for the preferred in-tree path)"
echo ""

DISPATCH="$(command -v cargo_slicer_dispatch 2>/dev/null || true)"
DRIVER="$(command -v cargo-slicer-rustc 2>/dev/null || true)"
WARMUP_DISPATCH="$(command -v cargo_warmup_dispatch 2>/dev/null || true)"
WARMUP_CLI="$(command -v cargo-warmup 2>/dev/null || true)"

SLICER=""
_CARGO_BIN="${CARGO_HOME:-$HOME/.cargo}/bin"
for _dir in $(echo "$PATH:$_CARGO_BIN" | tr ':' ' '); do
    if [[ -f "$_dir/cargo-slicer" && -x "$_dir/cargo-slicer" ]]; then
        _real="$(readlink -f "$_dir/cargo-slicer" 2>/dev/null || echo "$_dir/cargo-slicer")"
        if [[ "$(basename "$_real")" != "cargo" ]]; then
            SLICER="$_dir/cargo-slicer"
            break
        fi
    fi
done

if [[ -z "$DISPATCH" ]]; then
    echo "Error: cargo_slicer_dispatch not found in PATH." >&2
    echo "Install: curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash" >&2
    exit 1
fi
if [[ -z "$DRIVER" ]]; then
    echo "Error: cargo-slicer-rustc not found in PATH." >&2
    exit 1
fi
if [[ -z "$SLICER" ]]; then
    echo "Error: cargo-slicer standalone binary not found in PATH." >&2
    echo "Install: curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash" >&2
    exit 1
fi

# ── Step 0: Warm registry dep cache (cargo-warmup) ──────────────────────
# Pre-compile top crates.io deps once; subsequent cold builds skip recompiling
# serde, syn, proc-macro2, tokio, etc.  Adds ~10s one-time per toolchain.

if [[ -n "$WARMUP_DISPATCH" && -n "$WARMUP_CLI" ]]; then
    WARMUP_CACHE_MARKER="${CARGO_HOME:-$HOME/.cargo}/warmup-cache/.initialized"
    if [[ ! -f "$WARMUP_CACHE_MARKER" ]]; then
        echo "=== Step 0/3: Warming registry dep cache (one-time per toolchain) ==="
        "$WARMUP_CLI" init --tier=1 2>&1
        touch "$WARMUP_CACHE_MARKER" 2>/dev/null || true
        echo ""
    fi
else
    echo "Note: cargo-warmup not found — skipping registry dep cache (install.sh installs it)"
fi

# ── Step 1: Pre-analysis (cross-crate call graph) ───────────────────────

echo "=== Step 1/3: Pre-analyzing cross-crate call graph ==="
cd "$PROJECT_DIR"
"$SLICER" pre-analyze 2>&1

# ── Step 2: Build with virtual slicing + warmup ──────────────────────────
# cargo_warmup_dispatch handles registry crates (cached .rlib/.so).
# It chains to cargo_slicer_dispatch for local crates (MIR stub filtering).

echo ""
echo "=== Step 2/3: Building with virtual slicing + registry cache ==="

export CARGO_SLICER_VIRTUAL=1
export CARGO_SLICER_CODEGEN_FILTER=1
export CARGO_SLICER_DRIVER="$DRIVER"

if [[ -n "$WARMUP_DISPATCH" ]]; then
    # Outer wrapper: warmup handles registry deps
    # Inner wrapper: slicer handles local crates
    export RUSTC_WRAPPER="$WARMUP_DISPATCH"
    export CARGO_WARMUP_INNER_WRAPPER="$DISPATCH"
else
    export RUSTC_WRAPPER="$DISPATCH"
fi

NIGHTLY_TOOLCHAIN="nightly"
IS_RUSTC_TREE=false
if [[ -f "$PROJECT_DIR/compiler/rustc_macros/build.rs" ]]; then
    IS_RUSTC_TREE=true
    export RUSTC_BOOTSTRAP=1
    if [[ -f "$PROJECT_DIR/src/stage0" ]]; then
        STAGE0_DATE="$(grep '^compiler_date=' "$PROJECT_DIR/src/stage0" | cut -d= -f2)"
        if [[ -n "$STAGE0_DATE" ]]; then
            NIGHTLY_TOOLCHAIN="nightly-${STAGE0_DATE}"
            if ! rustup toolchain list | grep -q "^${NIGHTLY_TOOLCHAIN}-"; then
                echo "Installing nightly toolchain for rustc source tree: $NIGHTLY_TOOLCHAIN"
                rustup toolchain install "$NIGHTLY_TOOLCHAIN" --no-self-update
            fi
        fi
    fi
    echo ""
    echo "Note: the rustc source tree must be built via bootstrap, not plain cargo."
    echo "  cargo-slicer pre-analysis complete — call graph written to .slicer-cache/"
    echo ""
    echo "To build with acceleration, wrap x.py:"
    echo "  CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \\"
    echo "    RUSTC_WRAPPER=\$(which cargo_slicer_dispatch) \\"
    echo "    ./x.py build"
    exit 0
fi

time cargo +"$NIGHTLY_TOOLCHAIN" build --release "${EXTRA_ARGS[@]}" 2>&1

# ── Step 3: Summary ─────────────────────────────────────────────────────

echo ""
echo "=== Step 3/3: Done ==="
echo "Project:  $PROJECT_DIR"
echo "Binary:   $(find "$PROJECT_DIR/target/release" -maxdepth 1 -type f -executable 2>/dev/null | head -5)"

if [[ -f "$PROJECT_DIR/.cargo-slicer-debug.log" ]]; then
    STUBBED=$(grep -c '^\[codegen-filter\] STUB:' "$PROJECT_DIR/.cargo-slicer-debug.log" 2>/dev/null || echo 0)
    echo "Stubbed:  $STUBBED functions"
fi

#!/usr/bin/env bash
# cargo-slicer.sh — Drop-in accelerated Rust build
#
# Usage:
#   ./cargo-slicer.sh                    # build --release in current dir
#   ./cargo-slicer.sh /path/to/project   # build a specific project
#   ./cargo-slicer.sh . --features foo   # extra args passed to cargo build
#
# Prerequisites:
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

# ── Locate binaries ─────────────────────────────────────────────────────

DISPATCH="$(command -v cargo_slicer_dispatch 2>/dev/null || true)"
DRIVER="$(command -v cargo-slicer-rustc 2>/dev/null || true)"
SLICER="$(command -v cargo-slicer 2>/dev/null || true)"

if [[ -z "$DISPATCH" ]]; then
    echo "Error: cargo_slicer_dispatch not found in PATH." >&2
    echo "Install: cargo +nightly install --path <cargo-slicer-dir> --profile release-rustc \\" >&2
    echo "           --bin cargo_slicer_dispatch --bin cargo-slicer-rustc --features rustc-driver" >&2
    exit 1
fi
if [[ -z "$DRIVER" ]]; then
    echo "Error: cargo-slicer-rustc not found in PATH." >&2
    exit 1
fi
if [[ -z "$SLICER" ]]; then
    echo "Error: cargo-slicer not found in PATH." >&2
    echo "Install: cargo install --path <cargo-slicer-dir>" >&2
    exit 1
fi

# Verify nightly is available
if ! rustup run nightly rustc --version &>/dev/null; then
    echo "Error: Rust nightly toolchain not installed." >&2
    echo "Install: rustup toolchain install nightly" >&2
    exit 1
fi

# ── Step 1: Pre-analysis (cross-crate call graph) ───────────────────────

echo "=== Step 1/3: Pre-analyzing cross-crate call graph ==="
cd "$PROJECT_DIR"
"$SLICER" pre-analyze 2>&1

# ── Step 2: Build with virtual slicing ──────────────────────────────────

echo ""
echo "=== Step 2/3: Building with virtual slicing (codegen filtering) ==="

export CARGO_SLICER_VIRTUAL=1
export CARGO_SLICER_CODEGEN_FILTER=1
export CARGO_SLICER_DRIVER="$DRIVER"
export RUSTC_WRAPPER="$DISPATCH"

time cargo +nightly build --release "${EXTRA_ARGS[@]}" 2>&1

# ── Step 3: Summary ─────────────────────────────────────────────────────

echo ""
echo "=== Step 3/3: Done ==="
echo "Project:  $PROJECT_DIR"
echo "Binary:   $(find "$PROJECT_DIR/target/release" -maxdepth 1 -type f -executable 2>/dev/null | head -5)"

# Show cache stats if debug was enabled
if [[ -f "$PROJECT_DIR/.cargo-slicer-debug.log" ]]; then
    STUBBED=$(grep -c '^\[codegen-filter\] STUB:' "$PROJECT_DIR/.cargo-slicer-debug.log" 2>/dev/null || echo 0)
    echo "Stubbed:  $STUBBED functions"
fi

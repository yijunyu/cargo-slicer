#!/usr/bin/env bash
# cargo-slicer.sh — Accelerated Rust build: dead function elimination
#                   + registry cache + critical-path dep scheduling
#
# Usage:
#   ./cargo-slicer.sh                    # build --release in current dir
#   ./cargo-slicer.sh /path/to/project   # build a specific project
#   ./cargo-slicer.sh . --features foo   # extra args passed to cargo build
#
# Three complementary optimizations applied together:
#   1. cargo-warmup      — serves pre-compiled .rlib/.so for registry deps
#   2. cargo-slicer      — stubs unreachable functions in local crates
#   3. pch-plan priority — starts highest critical-path deps first
#
# The three form a chain:
#   RUSTC_WRAPPER=cargo_warmup_pch        (outermost: priority scheduler)
#     CARGO_WARMUP_INNER_WRAPPER=cargo_warmup_dispatch  (cache server)
#       CARGO_WARMUP_INNER_WRAPPER2=cargo_slicer_dispatch  (MIR stubs)
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
    WARMUP_PCH="$(command -v cargo_warmup_pch 2>/dev/null || true)"
    WARMUP_CLI="$(command -v cargo-warmup 2>/dev/null || true)"

    if [[ -n "$WARMUP_DISPATCH" && -n "$WARMUP_CLI" ]]; then
        WARMUP_CACHE_MARKER="$HOME/.cargo/warmup-cache/.initialized"
        if [[ ! -f "$WARMUP_CACHE_MARKER" ]]; then
            echo "=== Step 1/3: Warming registry dep cache (one-time per toolchain) ==="
            "$WARMUP_CLI" init --tier=1 2>&1
            touch "$WARMUP_CACHE_MARKER" 2>/dev/null || true
            echo ""
        fi

        # Compute pch-plan if priority scheduler is available
        _PLAN_FILE=""
        if [[ -n "$WARMUP_PCH" ]]; then
            _PLAN_HASH="$(printf '%s' "$PROJECT_DIR" | md5sum | cut -c1-12)"
            _PLAN_FILE="/tmp/cargo-warmup-plan-${_PLAN_HASH}.json"
            echo "=== Step 2/3: Computing critical-path priority plan ==="
            "$WARMUP_CLI" pch-plan \
                --manifest="$PROJECT_DIR/Cargo.toml" \
                --output="$_PLAN_FILE" 2>&1 || _PLAN_FILE=""
            echo ""
        fi

        echo "=== Step 3/3: Building with -Z dead-fn-elimination + registry cache + priority ==="
        if [[ -n "$WARMUP_PCH" && -n "$_PLAN_FILE" && -f "$_PLAN_FILE" ]]; then
            exec env \
                RUSTC_WRAPPER="$WARMUP_PCH" \
                CARGO_WARMUP_PLAN="$_PLAN_FILE" \
                CARGO_WARMUP_INNER_WRAPPER="$WARMUP_DISPATCH" \
                cargo +nightly build --release \
                --config 'build.rustflags=["-Z", "dead-fn-elimination"]' \
                "${EXTRA_ARGS[@]}"
        else
            exec env RUSTC_WRAPPER="$WARMUP_DISPATCH" \
                cargo +nightly build --release \
                --config 'build.rustflags=["-Z", "dead-fn-elimination"]' \
                "${EXTRA_ARGS[@]}"
        fi
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
WARMUP_PCH="$(command -v cargo_warmup_pch 2>/dev/null || true)"
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
    # Marker always in real ~/.cargo so a custom CARGO_HOME doesn't retrigger init
    WARMUP_CACHE_MARKER="$HOME/.cargo/warmup-cache/.initialized"
    if [[ ! -f "$WARMUP_CACHE_MARKER" ]]; then
        echo "=== Step 0/4: Warming registry dep cache (one-time per toolchain) ==="
        "$WARMUP_CLI" init --tier=1 2>&1
        touch "$WARMUP_CACHE_MARKER" 2>/dev/null || true
        echo ""
    fi
else
    echo "Note: cargo-warmup not found — skipping registry dep cache (install.sh installs it)"
fi

# ── Step 1: Pre-analysis (cross-crate call graph) ───────────────────────

echo "=== Step 1/4: Pre-analyzing cross-crate call graph ==="
cd "$PROJECT_DIR"
mkdir -p "$PROJECT_DIR/.slicer-cache"
"$SLICER" pre-analyze 2>&1

# ── Step 2: Critical-path priority plan ─────────────────────────────────
# Read the unit-graph, assign compile-time estimates from warmup cache,
# compute CP(unit) = self_weight + max(CP(dep)) for every unit.
# Stored in /tmp keyed by project path hash — never pollutes the project dir.

_PLAN_FILE=""
if [[ -n "$WARMUP_CLI" && -n "$WARMUP_PCH" ]]; then
    _PLAN_HASH="$(printf '%s' "$PROJECT_DIR" | md5sum | cut -c1-12)"
    _PLAN_FILE="/tmp/cargo-warmup-plan-${_PLAN_HASH}.json"
    echo ""
    echo "=== Step 2/4: Computing critical-path priority plan ==="
    "$WARMUP_CLI" pch-plan \
        --manifest="$PROJECT_DIR/Cargo.toml" \
        --output="$_PLAN_FILE" 2>&1 || {
        echo "Note: pch-plan failed (non-nightly cargo?), skipping priority scheduling"
        _PLAN_FILE=""
    }
fi

# ── Step 3: Build with priority scheduling + warmup + virtual slicing ────
# Wrapper chain (outermost → innermost):
#   cargo_warmup_pch      — priority scheduler (starts highest-CP units first)
#   cargo_warmup_dispatch — registry cache     (serves .rlib/.so hits)
#   cargo_slicer_dispatch — MIR stubs          (eliminates unreachable fn codegen)

echo ""
echo "=== Step 3/4: Building with priority scheduling + registry cache + MIR stubs ==="

export CARGO_SLICER_VIRTUAL=1
export CARGO_SLICER_CODEGEN_FILTER=1
export CARGO_SLICER_DRIVER="$DRIVER"

if [[ -n "$WARMUP_PCH" && -n "$_PLAN_FILE" && -f "$_PLAN_FILE" ]]; then
    # Full three-layer chain
    export RUSTC_WRAPPER="$WARMUP_PCH"
    export CARGO_WARMUP_PLAN="$_PLAN_FILE"
    export CARGO_WARMUP_INNER_WRAPPER="$WARMUP_DISPATCH"
    export CARGO_WARMUP_INNER_WRAPPER2="$DISPATCH"
elif [[ -n "$WARMUP_DISPATCH" ]]; then
    # No pch priority, but warmup + slicer
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

# ── Step 4: Summary ─────────────────────────────────────────────────────

echo ""
echo "=== Step 4/4: Done ==="
echo "Project:  $PROJECT_DIR"
echo "Binary:   $(find "$PROJECT_DIR/target/release" -maxdepth 1 -type f -executable 2>/dev/null | head -5)"

if [[ -f "$PROJECT_DIR/.cargo-slicer-debug.log" ]]; then
    # Sum all "defined-unmarked" counts across crate analyses in the debug log
    STUBBED=$(grep 'defined-unmarked' "$PROJECT_DIR/.cargo-slicer-debug.log" 2>/dev/null \
        | grep -oE '[0-9]+ defined-unmarked' | awk '{sum+=$1} END {print sum+0}')
    if [[ -z "$STUBBED" ]]; then STUBBED=0; fi
    echo "Stubbed:  $STUBBED functions (stubbed out of codegen)"
fi

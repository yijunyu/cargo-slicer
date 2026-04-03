#!/usr/bin/env bash
# install.sh — Install cargo-slicer + cargo-warmup from precompiled binaries (v0.0.8)
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash
#
# What it installs:
#   ~/.cargo/bin/cargo-slicer           — Pre-analysis CLI
#   ~/.cargo/bin/cargo-slicer-rustc     — Rustc driver (MIR analysis + codegen filtering)
#   ~/.cargo/bin/cargo_slicer_dispatch  — RUSTC_WRAPPER dispatcher (dead fn elimination)
#   ~/.cargo/bin/cargo_warmup_dispatch  — RUSTC_WRAPPER dispatcher (registry dep cache)
#   ~/.cargo/bin/cargo_warmup_pch       — RUSTC_WRAPPER priority scheduler (critical-path ordering)
#   ~/.cargo/bin/cargo-warmup           — cargo-warmup CLI (init/status/clean/schedule/pch-plan)
#   ~/.cargo/bin/cargo-slicer.sh        — Drop-in build script (all 4 steps in one command)

set -euo pipefail

REPO="yijunyu/cargo-slicer"
CARGO_SLICER_VERSION="0.0.8"
INSTALL_DIR="${CARGO_HOME:-$HOME/.cargo}/bin"

# ── Detect platform ───────────────────────────────────────────────────

ARCH="$(uname -m)"
OS="$(uname -s)"

case "$OS" in
    Linux)  OS_LABEL="unknown-linux-gnu" ;;
    Darwin) OS_LABEL="apple-darwin" ;;
    *)
        echo "Error: Unsupported OS: $OS" >&2
        echo "cargo-slicer supports Linux and macOS." >&2
        exit 1
        ;;
esac

case "$ARCH" in
    x86_64)        ARCH_LABEL="x86_64" ;;
    aarch64|arm64) ARCH_LABEL="aarch64" ;;
    *)
        echo "Error: Unsupported architecture: $ARCH" >&2
        echo "cargo-slicer supports x86_64 and aarch64." >&2
        exit 1
        ;;
esac

TARGET="${ARCH_LABEL}-${OS_LABEL}"
ARCHIVE="cargo-slicer-${TARGET}.tar.gz"

echo "cargo-slicer installer"
echo "======================"
echo "Platform: ${TARGET} (archive: ${ARCHIVE})"
echo "Install:  ${INSTALL_DIR}"
echo ""

# ── Check prerequisites ───────────────────────────────────────────────

if ! command -v rustup &>/dev/null; then
    echo "Error: rustup not found." >&2
    echo "Install Rust first: https://rustup.rs" >&2
    exit 1
fi

# ── Find latest release ───────────────────────────────────────────────

echo "Finding latest release..."

if command -v gh &>/dev/null; then
    # Use gh CLI to get the tag name (handles auth for private repos)
    TAG="$(gh release view --repo "$REPO" --json tagName --jq '.tagName' 2>/dev/null || true)"
fi

if [[ -z "${TAG:-}" ]]; then
    # Fall back to GitHub API
    RELEASE_JSON="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" 2>/dev/null || true)"

    if [[ -z "$RELEASE_JSON" ]]; then
        echo "Error: Could not fetch release info from GitHub." >&2
        echo "Check: https://github.com/$REPO/releases" >&2
        exit 1
    fi

    TAG="$(echo "$RELEASE_JSON" | grep '"tag_name"' | head -1 | sed 's/.*: *"\(.*\)".*/\1/')"
fi

# Always construct the direct browser download URL (gh CLI .url returns API URL, not download URL)
DOWNLOAD_URL="https://github.com/$REPO/releases/download/$TAG/$ARCHIVE"

if [[ -z "${TAG:-}" ]]; then
    echo "Error: No releases found." >&2
    exit 1
fi

echo "Release: $TAG"
echo "Archive: $ARCHIVE"
echo ""

# ── Download and install ──────────────────────────────────────────────

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

echo "Downloading..."
if ! curl -fSL --progress-bar -o "$TMPDIR/$ARCHIVE" "$DOWNLOAD_URL" 2>/dev/null; then
    echo "" >&2
    echo "Error: no precompiled bundle available for $TARGET." >&2
    echo "Please file an issue at https://github.com/$REPO/issues" >&2
    exit 1
fi

echo "Extracting..."
tar -xzf "$TMPDIR/$ARCHIVE" -C "$TMPDIR"

# ── Verify binary is executable on this system ────────────────────────

CANDIDATE="$TMPDIR/cargo-slicer"
if [[ ! -f "$CANDIDATE" ]]; then
    CANDIDATE="$TMPDIR/cargo-slicer/cargo-slicer"
fi

if [[ -f "$CANDIDATE" ]]; then
    chmod +x "$CANDIDATE"
    if ! "$CANDIDATE" --help &>/dev/null 2>&1; then
        echo ""
        echo "Note: downloaded binary not compatible with this system (likely glibc mismatch)."
        echo "      Will build stable binaries from source instead."
        SKIP_BIN_INSTALL=1
    fi
fi

# If running from a local source checkout, build stable binaries from source
_CALLER_IS_SOURCE=false
if grep -q 'name.*=.*"cargo-slicer"' "$PWD/Cargo.toml" 2>/dev/null; then
    _CALLER_IS_SOURCE=true
fi

_SOURCE_INSTALLED=false
if [[ "${SKIP_BIN_INSTALL:-}" == "1" ]] && [[ "$_CALLER_IS_SOURCE" == "true" ]]; then
    echo ""
    echo "Building stable binaries from source (local checkout)..."
    if cargo install --path "$PWD" --force \
        --bin cargo-slicer --bin cargo_slicer_dispatch \
        --bin cargo_warmup_dispatch --bin cargo_warmup_pch \
        --bin cargo-warmup --bin cargo_warmup_pch \
        --bin gcc_slicer_bfs --bin gcc_slicer_pch \
        2>&1 | tail -5; then
        _SOURCE_INSTALLED=true
        echo "  Stable binaries installed from local source."
    else
        echo "  Warning: source build failed. Will try tarball binaries."
    fi
fi

# ── Install binaries from tarball (unless already installed from source) ─

mkdir -p "$INSTALL_DIR"

if [[ -z "${SKIP_BIN_INSTALL:-}" ]] && [[ "$_SOURCE_INSTALLED" != "true" ]]; then
    BINARIES=(cargo-slicer cargo_slicer_dispatch cargo_warmup_dispatch cargo_warmup_pch cargo-warmup cargo-slicer.sh)
    for bin in "${BINARIES[@]}"; do
        for search in "$TMPDIR/$bin" "$TMPDIR/cargo-slicer/$bin"; do
            if [[ -f "$search" ]]; then
                cp "$search" "$INSTALL_DIR/$bin"
                chmod +x "$INSTALL_DIR/$bin"
                echo "  Installed: $INSTALL_DIR/$bin"
                break
            fi
        done
    done
elif [[ "$_SOURCE_INSTALLED" == "true" ]]; then
    # When installed from source, prefer the local cargo-slicer.sh (most up to date)
    # Fall back to tarball version if local not found
    _SH_INSTALLED=false
    for search in "$PWD/cargo-slicer.sh" "$HOME/precc-c/cargo-slicer.sh" "$HOME/precc/cargo-slicer.sh"; do
        if [[ -f "$search" ]]; then
            cp "$search" "$INSTALL_DIR/cargo-slicer.sh"
            chmod +x "$INSTALL_DIR/cargo-slicer.sh"
            echo "  Installed: $INSTALL_DIR/cargo-slicer.sh (from local source)"
            _SH_INSTALLED=true
            break
        fi
    done
    if [[ "$_SH_INSTALLED" != "true" ]]; then
        for search in "$TMPDIR/cargo-slicer.sh" "$TMPDIR/cargo-slicer/cargo-slicer.sh"; do
            if [[ -f "$search" ]]; then
                cp "$search" "$INSTALL_DIR/cargo-slicer.sh"
                chmod +x "$INSTALL_DIR/cargo-slicer.sh"
                echo "  Installed: $INSTALL_DIR/cargo-slicer.sh"
                break
            fi
        done
    fi
fi

echo ""

# ── Ensure nightly toolchain ──────────────────────────────────────────

if ! rustup run nightly rustc --version &>/dev/null; then
    echo "Installing Rust nightly toolchain..."
    rustup toolchain install nightly
    echo ""
fi

NIGHTLY_VER="$(rustup run nightly rustc --version 2>/dev/null || echo 'unknown')"
echo "Nightly: $NIGHTLY_VER"

# ── Detect -Z dead-fn-elimination support ────────────────────────────────
# If the active nightly has the in-tree patch applied, no driver binary is
# needed — cargo-slicer.sh will use the flag directly via --config rustflags.

if rustup run nightly rustc -Z help 2>&1 | grep -q 'dead-fn-elimination'; then
    echo ""
    echo "Patched nightly detected: -Z dead-fn-elimination is available."
    echo "No driver binary required. cargo-slicer.sh will use the flag directly."
    ZFLAG_AVAILABLE=true
else
    echo ""
    echo "Standard nightly detected (no -Z dead-fn-elimination)."
    echo "cargo-slicer.sh will use the RUSTC_WRAPPER fallback path."
    echo "See docs/upstream-rfc.md for how to build a patched nightly."
    ZFLAG_AVAILABLE=false
fi

# ── Build and install nightly driver (cargo-slicer-rustc) ────────────────
# The driver links against librustc_driver.so, which is toolchain-specific,
# so it cannot be pre-compiled into the release archive.
# We build it from source here — takes ~2-3 min on first install.
# Skipped automatically if already installed for the current nightly.

DRIVER_BIN="$INSTALL_DIR/cargo-slicer-rustc"
DRIVER_MARKER="${CARGO_HOME:-$HOME/.cargo}/warmup-cache/.driver-$(rustup run nightly rustc --version 2>/dev/null | md5sum | cut -c1-8)"

if [[ "${ZFLAG_AVAILABLE:-false}" == "true" ]]; then
    : # In-tree -Z flag: no driver needed
elif [[ -f "$DRIVER_BIN" && -f "$DRIVER_MARKER" ]]; then
    echo ""
    echo "Nightly driver already installed for this toolchain."
elif command -v cargo &>/dev/null; then
    echo ""
    echo "Building nightly driver (cargo-slicer-rustc) from source (~2-3 min)..."
    echo "This is a one-time step per nightly toolchain update."
    echo ""

    # The public deploy repo (yijunyu/cargo-slicer) has no Cargo.toml — it's docs+scripts only.
    # The full source lives in yijunyu/precc (private) or a local checkout.
    # Strategy: prefer local checkout → git install from private repo → skip with hint.

    FULL_SOURCE_REPO="yijunyu/precc"

    # 1. Look for a local checkout: caller's dir, or common locations
    _SRC_DIR=""
    for _candidate in \
        "$PWD" \
        "$HOME/precc-c" \
        "$HOME/precc" \
        "$(dirname "$(command -v cargo-slicer 2>/dev/null || echo /nonexistent)")/../../.."
    do
        _candidate="$(cd "$_candidate" 2>/dev/null && pwd || true)"
        if [[ -n "$_candidate" && -f "$_candidate/Cargo.toml" ]] && \
           grep -q 'cargo-slicer' "$_candidate/Cargo.toml" 2>/dev/null; then
            _SRC_DIR="$_candidate"
            echo "  Found local source checkout: $_SRC_DIR"
            break
        fi
    done

    # Add rustc-private component (needed to link librustc_driver)
    rustup component add rustc-dev llvm-tools-preview --toolchain nightly 2>/dev/null || true

    _BUILD_OK=false
    if [[ -n "$_SRC_DIR" ]]; then
        # Build from local checkout
        if cargo +nightly install --path "$_SRC_DIR" \
            --profile release-rustc \
            --bin cargo-slicer-rustc \
            --bin cargo_slicer_dispatch \
            --features rustc-driver \
            --no-default-features \
            --force 2>&1 | tail -5; then
            _BUILD_OK=true
        fi
    else
        # Try installing directly from the full-source git repo (may require auth for private repo)
        echo "  No local checkout found; trying: https://github.com/$FULL_SOURCE_REPO"
        if cargo +nightly install \
            --git "https://github.com/$FULL_SOURCE_REPO" \
            --profile release-rustc \
            --bin cargo-slicer-rustc \
            --bin cargo_slicer_dispatch \
            --features rustc-driver \
            --no-default-features \
            --force 2>&1 | tail -5; then
            _BUILD_OK=true
        fi
    fi

    if [[ "$_BUILD_OK" == "true" ]]; then
        touch "$DRIVER_MARKER" 2>/dev/null || true
        echo "  Installed: $INSTALL_DIR/cargo-slicer-rustc"
    else
        echo "  Warning: driver build failed — virtual slicing will use pre-analysis only."
        echo "  To build manually from a local checkout:"
        echo "    git clone https://github.com/$FULL_SOURCE_REPO /tmp/precc-src"
        echo "    cargo +nightly install --path /tmp/precc-src \\"
        echo "      --profile release-rustc --bin cargo-slicer-rustc --features rustc-driver"
    fi
else
    echo ""
    echo "Note: cargo not found — skipping nightly driver build."
fi

# ── Pre-warm registry dep cache (cargo-warmup) ───────────────────────
# Run once per toolchain: compiles top-20 crates.io deps (serde, syn, tokio...)
# into ~/.cargo/warmup-cache/ so cold builds of any project skip recompiling them.
# Adds ~10-30s here; saves 2-9× on every subsequent cold build.

WARMUP_BIN="$INSTALL_DIR/cargo-warmup"
WARMUP_CACHE_MARKER="${CARGO_HOME:-$HOME/.cargo}/warmup-cache/.initialized"

if [[ -f "$WARMUP_BIN" ]] && [[ ! -f "$WARMUP_CACHE_MARKER" ]]; then
    echo ""
    echo "Pre-warming registry dep cache (one-time, ~15-30s)..."
    "$WARMUP_BIN" init --tier=1 2>&1 | grep -E "warmup|Compiling|Finished|error" || true
    touch "$WARMUP_CACHE_MARKER" 2>/dev/null || true
    echo "  Registry dep cache ready."
elif [[ -f "$WARMUP_CACHE_MARKER" ]]; then
    echo ""
    echo "Registry dep cache already initialized (run 'cargo warmup status' to inspect)."
fi

# ── Verify PATH ───────────────────────────────────────────────────────

if ! echo "$PATH" | tr ':' '\n' | grep -q "$(dirname "$INSTALL_DIR/cargo-slicer")"; then
    echo ""
    echo "Warning: $INSTALL_DIR is not in your PATH."
    echo "Add to your shell profile:"
    echo "  export PATH=\"$INSTALL_DIR:\$PATH\""
fi

# ── Print usage ───────────────────────────────────────────────────────

echo ""
echo "============================================"
echo "  cargo-slicer + cargo-warmup installed!"
echo "============================================"
echo ""
echo "Quick start — accelerate any Rust project (both tools combined):"
echo ""
echo "  cargo-slicer.sh /path/to/your/project"
echo ""
echo "What happens (4 steps, fully automatic):"
echo "  1. cargo-warmup serves pre-compiled .rlib/.so for serde/syn/tokio/..."
echo "     (registry deps — up to 15.9× on cold builds, 1.1–1.7× incremental)"
echo "  2. cargo-slicer pre-analyzes the cross-crate call graph"
echo "  3. cargo warmup pch-plan computes critical-path build order"
echo "  4. cargo-slicer stubs unreachable functions via MIR"
echo "     (dead code elimination — 10–42% codegen time saved)"
echo ""
if [[ "${ZFLAG_AVAILABLE:-false}" == "true" ]]; then
    echo "Advanced — with patched nightly (-Z dead-fn-elimination):"
    echo ""
    echo "  cd /path/to/your/project"
    echo "  RUSTC_WRAPPER=\$(which cargo_warmup_pch) \\"
    echo "    CARGO_WARMUP_INNER_WRAPPER=\$(which cargo_warmup_dispatch) \\"
    echo "    cargo +nightly build --release \\"
    echo "    --config 'build.rustflags=[\"-Z\", \"dead-fn-elimination\"]'"
    echo ""
else
    echo "Advanced — manual 3-layer RUSTC_WRAPPER chain:"
    echo ""
    echo "  cd /path/to/your/project"
    echo "  cargo-slicer pre-analyze"
    echo "  RUSTC_WRAPPER=\$(which cargo_warmup_pch) \\"
    echo "    CARGO_WARMUP_INNER_WRAPPER=\$(which cargo_warmup_dispatch) \\"
    echo "    CARGO_WARMUP_INNER_WRAPPER2=\$(which cargo_slicer_dispatch) \\"
    echo "    CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \\"
    echo "    cargo +nightly build --release"
    echo ""
    echo "For the in-tree -Z flag, see: docs/upstream-rfc.md"
    echo ""
fi
echo "Registry dep cache: ~/.cargo/warmup-cache/  (run 'cargo warmup status')"
echo "Documentation:      https://github.com/$REPO"

# ── Detect rust compiler checkout and install x.py hook ───────────────
# If we're in a rust-lang/rust checkout (has x.py + src/stage0), install
# a wrapper script so ./x.py build automatically uses cargo-slicer via
# the bootstrap shim's RUSTC_WRAPPER_REAL chaining mechanism.
#
# How it works:
#   Bootstrap always sets RUSTC_WRAPPER to its own rustc shim.
#   The shim checks RUSTC_WRAPPER_REAL and chains through it.
#   So setting RUSTC_WRAPPER_REAL=cargo_slicer_dispatch before x.py
#   injects cargo-slicer into every stage1/stage2 rustc invocation.

CALLER_DIR="${PWD}"

if [[ -f "$CALLER_DIR/x.py" && -f "$CALLER_DIR/src/stage0" ]]; then
    DISPATCH_BIN="$(command -v cargo_slicer_dispatch 2>/dev/null || echo "$INSTALL_DIR/cargo_slicer_dispatch")"
    # Locate librustc_driver.so for the active nightly toolchain.
    # Toolchain dir names use the release date (one day after commit-date), so we
    # look up the actual installed toolchain name rather than deriving it from commit-date.
    NIGHTLY_TOOLCHAIN_DIR="$(rustup toolchain list 2>/dev/null \
        | grep "^nightly-.*${TARGET}" | grep '(active)' \
        | awk '{print $1}' | head -1 || true)"
    if [[ -z "$NIGHTLY_TOOLCHAIN_DIR" ]]; then
        NIGHTLY_TOOLCHAIN_DIR="$(rustup toolchain list 2>/dev/null \
            | grep "^nightly-.*${TARGET}" | awk '{print $1}' | tail -1 || true)"
    fi
    LIB_PATH="${HOME}/.rustup/toolchains/${NIGHTLY_TOOLCHAIN_DIR}/lib"

    # Write x-slicer.sh into the rust checkout root
    cat > "$CALLER_DIR/x-slicer.sh" << XSLICER
#!/usr/bin/env bash
# x-slicer.sh — Drop-in wrapper for ./x.py that injects cargo-slicer
# into the bootstrap build via RUSTC_WRAPPER_REAL.
#
# Usage: ./x-slicer.sh build [args...]   (same as ./x.py build [args...])
#
# How it works:
#   Bootstrap's rustc shim always sets RUSTC_WRAPPER to itself, but
#   honours RUSTC_WRAPPER_REAL by chaining through it.  Setting
#   RUSTC_WRAPPER_REAL=cargo_slicer_dispatch here injects cargo-slicer
#   into every stage1 and stage2 rustc invocation automatically.
set -euo pipefail
SCRIPT_DIR="\$(cd "\$(dirname "\${BASH_SOURCE[0]}")" && pwd)"
export RUSTC_WRAPPER_REAL="${DISPATCH_BIN}"
export CARGO_SLICER_VIRTUAL=1
export CARGO_SLICER_CODEGEN_FILTER=1
# Ensure librustc_driver is findable for the matching nightly
# macOS uses DYLD_LIBRARY_PATH; Linux uses LD_LIBRARY_PATH
if [[ -d "${LIB_PATH}" ]]; then
    if [[ "\$(uname -s)" == "Darwin" ]]; then
        export DYLD_LIBRARY_PATH="${LIB_PATH}\${DYLD_LIBRARY_PATH:+:\$DYLD_LIBRARY_PATH}"
    else
        export LD_LIBRARY_PATH="${LIB_PATH}\${LD_LIBRARY_PATH:+:\$LD_LIBRARY_PATH}"
    fi
fi
if [[ ! -f "\$SCRIPT_DIR/.slicer-cache/NIGHTLY_DATE" ]]; then
    echo "[x-slicer] No pre-computed cache. Running cargo-slicer pre-analyze (~5-10 min)..."
    cargo-slicer pre-analyze
fi
exec "\$SCRIPT_DIR/x.py" "\$@"
XSLICER
    chmod +x "$CALLER_DIR/x-slicer.sh"

    # ── Download pre-computed rustc slicer cache ──────────────────────────
    RUSTC_CACHE_DIR="$CALLER_DIR/.slicer-cache"
    RUSTC_CACHE_DATE_FILE="$RUSTC_CACHE_DIR/NIGHTLY_DATE"
    RUSTC_CACHE_OK=""

    if [[ -f "$RUSTC_CACHE_DATE_FILE" ]] && \
       [[ "$(cat "$RUSTC_CACHE_DATE_FILE" 2>/dev/null)" == "$NIGHTLY_DATE" ]]; then
        echo "  Pre-computed rustc slicer cache already present for $NIGHTLY_DATE"
        RUSTC_CACHE_OK=1
    fi

    if [[ -z "$RUSTC_CACHE_OK" && -n "$NIGHTLY_DATE" && "$NIGHTLY_DATE" != "unknown" ]]; then
        RUSTC_CACHE_ASSET="rustc-slicer-cache-${NIGHTLY_DATE}.tar.gz"
        RUSTC_CACHE_URL="https://github.com/$REPO/releases/download/$TAG/$RUSTC_CACHE_ASSET"
        CACHE_TMPDIR="$(mktemp -d)"
        echo "Fetching pre-computed rustc slicer cache for $NIGHTLY_DATE..."
        if curl -fsSL --progress-bar -o "$CACHE_TMPDIR/$RUSTC_CACHE_ASSET" \
           "$RUSTC_CACHE_URL" 2>/dev/null; then
            tar -xzf "$CACHE_TMPDIR/$RUSTC_CACHE_ASSET" -C "$CALLER_DIR" 2>/dev/null || true
            if [[ -f "$RUSTC_CACHE_DATE_FILE" ]]; then
                CACHE_N=$(find "$RUSTC_CACHE_DIR" -name '*.analysis' 2>/dev/null | wc -l)
                echo "  Installed rustc slicer cache: $CACHE_N analysis files"
            else
                echo "  Warning: cache extraction failed"
            fi
        else
            echo "  Note: no pre-computed rustc cache for nightly-${NIGHTLY_DATE} yet."
            echo "  Run 'cargo-slicer pre-analyze' before first x-slicer.sh build."
        fi
        rm -rf "$CACHE_TMPDIR"
    fi
    echo ""
    echo "============================================"
    echo "  Rust compiler checkout detected!"
    echo "============================================"
    echo ""
    echo "Installed: $CALLER_DIR/x-slicer.sh"
    echo ""
    echo "Use ./x-slicer.sh instead of ./x.py to build with cargo-slicer:"
    echo ""
    echo "  cd $CALLER_DIR"
    echo "  ./x-slicer.sh build --stage 1 compiler/rustc"
    echo ""
    echo "cargo-slicer hooks into bootstrap via RUSTC_WRAPPER_REAL,"
    echo "which the bootstrap shim chains through for all stage1/stage2"
    echo "rustc invocations.  No changes to bootstrap.toml needed."
    echo ""
fi

# ── Auto-run if Cargo.toml exists in the calling directory ────────────

# When piped through bash (curl | bash), PWD is the directory where curl was run
if [[ -f "$CALLER_DIR/Cargo.toml" && ! -f "$CALLER_DIR/x.py" ]]; then
    SLICER_SH=""
    for _dir in $(echo "$PATH:$INSTALL_DIR" | tr ':' ' '); do
        if [[ -f "$_dir/cargo-slicer.sh" && -x "$_dir/cargo-slicer.sh" ]]; then
            SLICER_SH="$_dir/cargo-slicer.sh"
            break
        fi
    done
    if [[ -n "$SLICER_SH" ]]; then
        echo ""
        echo "Found Cargo.toml in $CALLER_DIR — running cargo-slicer.sh..."
        echo ""
        exec "$SLICER_SH" "$CALLER_DIR"
    fi
fi

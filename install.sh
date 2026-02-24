#!/usr/bin/env bash
# install.sh — Install cargo-slicer from precompiled binaries
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash
#
# What it installs:
#   ~/.cargo/bin/cargo-slicer           — Pre-analysis CLI
#   ~/.cargo/bin/cargo-slicer-rustc     — Rustc driver (MIR analysis + codegen filtering)
#   ~/.cargo/bin/cargo_slicer_dispatch  — RUSTC_WRAPPER dispatcher
#   ~/.cargo/bin/cargo-slicer.sh        — Drop-in build script

set -euo pipefail

REPO="yijunyu/cargo-slicer"
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
echo "Platform: ${TARGET}"
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
    # Use gh CLI if available (handles auth)
    DOWNLOAD_URL="$(gh release view --repo "$REPO" --json assets \
        --jq ".assets[] | select(.name == \"$ARCHIVE\") | .url" 2>/dev/null || true)"
    TAG="$(gh release view --repo "$REPO" --json tagName --jq '.tagName' 2>/dev/null || true)"
fi

if [[ -z "${DOWNLOAD_URL:-}" ]]; then
    # Fall back to GitHub API
    RELEASE_JSON="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" 2>/dev/null || true)"

    if [[ -z "$RELEASE_JSON" ]]; then
        echo "Error: Could not fetch release info from GitHub." >&2
        echo "Check: https://github.com/$REPO/releases" >&2
        exit 1
    fi

    TAG="$(echo "$RELEASE_JSON" | grep '"tag_name"' | head -1 | sed 's/.*: *"\(.*\)".*/\1/')"
    DOWNLOAD_URL="https://github.com/$REPO/releases/download/$TAG/$ARCHIVE"
fi

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
if ! curl -fSL --progress-bar -o "$TMPDIR/$ARCHIVE" "$DOWNLOAD_URL"; then
    echo "" >&2
    echo "Error: Download failed." >&2
    echo "The precompiled binary for $TARGET may not be available yet." >&2
    echo "" >&2
    echo "Alternative: Build from source:" >&2
    echo "  cargo install --git https://github.com/$REPO" >&2
    echo "  cargo +nightly install --git https://github.com/$REPO \\" >&2
    echo "    --profile release-rustc --features rustc-driver \\" >&2
    echo "    --bin cargo-slicer-rustc --bin cargo_slicer_dispatch" >&2
    exit 1
fi

echo "Extracting..."
tar -xzf "$TMPDIR/$ARCHIVE" -C "$TMPDIR"

# ── Install binaries ──────────────────────────────────────────────────

mkdir -p "$INSTALL_DIR"

BINARIES=(cargo-slicer cargo-slicer-rustc cargo_slicer_dispatch cargo-slicer.sh)

for bin in "${BINARIES[@]}"; do
    if [[ -f "$TMPDIR/cargo-slicer/$bin" ]]; then
        cp "$TMPDIR/cargo-slicer/$bin" "$INSTALL_DIR/$bin"
        chmod +x "$INSTALL_DIR/$bin"
        echo "  Installed: $INSTALL_DIR/$bin"
    else
        echo "  Warning: $bin not found in archive" >&2
    fi
done

echo ""

# ── Ensure nightly toolchain ──────────────────────────────────────────

if ! rustup run nightly rustc --version &>/dev/null; then
    echo "Installing Rust nightly toolchain..."
    rustup toolchain install nightly
    echo ""
fi

NIGHTLY_VER="$(rustup run nightly rustc --version 2>/dev/null || echo 'unknown')"
echo "Nightly: $NIGHTLY_VER"

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
echo "  cargo-slicer installed successfully!"
echo "============================================"
echo ""
echo "Quick start — accelerate any Rust project:"
echo ""
echo "  cargo-slicer.sh /path/to/your/project"
echo ""
echo "Or manually:"
echo ""
echo "  cd /path/to/your/project"
echo "  cargo-slicer pre-analyze"
echo "  CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \\"
echo "    RUSTC_WRAPPER=\$(which cargo_slicer_dispatch) \\"
echo "    cargo +nightly build --release"
echo ""
echo "Documentation: https://github.com/$REPO"

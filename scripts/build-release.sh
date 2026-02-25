#!/usr/bin/env bash
# build-release.sh — Build and package cargo-slicer binaries for GitHub release
#
# Usage:
#   ./scripts/build-release.sh              # Build for current platform
#   ./scripts/build-release.sh --upload     # Build + upload to GitHub release
#   ./scripts/build-release.sh --target x86_64-pc-windows-msvc  # Cross-compile for Windows
#
# Output:
#   dist/cargo-slicer-<target>.tar.gz       # Binary archive (Linux/macOS)
#   dist/cargo-slicer-<target>.zip          # Binary archive (Windows)
#
# Prerequisites:
#   - Rust nightly toolchain
#   - gh CLI (for --upload)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Parse flags
EXPLICIT_TARGET=""
UPLOAD=false
while [[ $# -gt 0 ]]; do
    case "$1" in
        --upload) UPLOAD=true; shift ;;
        --target=*) EXPLICIT_TARGET="${1#--target=}"; shift ;;
        --target) EXPLICIT_TARGET="$2"; shift 2 ;;
        *) shift ;;
    esac
done

if [[ -n "$EXPLICIT_TARGET" ]]; then
    TARGET="$EXPLICIT_TARGET"
else
    # Detect platform
    ARCH="$(uname -m)"
    OS="$(uname -s)"

    case "$OS" in
        Linux)            OS_LABEL="unknown-linux-gnu" ;;
        Darwin)           OS_LABEL="apple-darwin" ;;
        MINGW*|MSYS*|CYGWIN*) OS_LABEL="pc-windows-msvc" ;;
        *)                echo "Unsupported OS: $OS" >&2; exit 1 ;;
    esac

    case "$ARCH" in
        x86_64)        ARCH_LABEL="x86_64" ;;
        aarch64|arm64) ARCH_LABEL="aarch64" ;;
        *)             echo "Unsupported architecture: $ARCH" >&2; exit 1 ;;
    esac

    TARGET="${ARCH_LABEL}-${OS_LABEL}"
fi

DIST_DIR="$PROJECT_DIR/dist"

# Windows uses .zip; everything else uses .tar.gz
IS_WINDOWS=false
case "$TARGET" in
    *windows*) IS_WINDOWS=true ;;
esac

if $IS_WINDOWS; then
    ARCHIVE_NAME="cargo-slicer-${TARGET}.zip"
    EXE_SUFFIX=".exe"
else
    ARCHIVE_NAME="cargo-slicer-${TARGET}.tar.gz"
    EXE_SUFFIX=""
fi

echo "=== Building cargo-slicer for $TARGET ==="

# ── Build all binaries ─────────────────────────────────────────────────

TARGET_FLAG=()
if [[ -n "$EXPLICIT_TARGET" ]]; then
    TARGET_FLAG=(--target "$EXPLICIT_TARGET")
fi

echo "Building cargo-slicer (stable)..."
cargo build --release --manifest-path "$PROJECT_DIR/Cargo.toml" "${TARGET_FLAG[@]}" 2>&1

echo "Building cargo-slicer-rustc + cargo_slicer_dispatch (nightly)..."
cargo +nightly build --manifest-path "$PROJECT_DIR/Cargo.toml" \
    --profile release-rustc \
    --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
    --features rustc-driver "${TARGET_FLAG[@]}" 2>&1

# ── Package ────────────────────────────────────────────────────────────

mkdir -p "$DIST_DIR"

STAGING_DIR="$(mktemp -d)"
trap 'rm -rf "$STAGING_DIR"' EXIT

PACKAGE_DIR="$STAGING_DIR/cargo-slicer"
mkdir -p "$PACKAGE_DIR"

# Determine binary output dirs (cross-compile puts them under target/<triple>/)
if [[ -n "$EXPLICIT_TARGET" ]]; then
    RELEASE_BIN_DIR="$PROJECT_DIR/target/$EXPLICIT_TARGET/release"
    RUSTC_BIN_DIR="$PROJECT_DIR/target/$EXPLICIT_TARGET/release-rustc"
else
    RELEASE_BIN_DIR="$PROJECT_DIR/target/release"
    RUSTC_BIN_DIR="$PROJECT_DIR/target/release-rustc"
fi

# Copy binaries
cp "$RELEASE_BIN_DIR/cargo-slicer${EXE_SUFFIX}" "$PACKAGE_DIR/"
cp "$RUSTC_BIN_DIR/cargo-slicer-rustc${EXE_SUFFIX}" "$PACKAGE_DIR/"
cp "$RUSTC_BIN_DIR/cargo_slicer_dispatch${EXE_SUFFIX}" "$PACKAGE_DIR/"

# Copy drop-in build scripts
if $IS_WINDOWS; then
    cp "$PROJECT_DIR/cargo-slicer.ps1" "$PACKAGE_DIR/"
else
    cp "$PROJECT_DIR/cargo-slicer.sh" "$PACKAGE_DIR/"
    chmod +x "$PACKAGE_DIR/cargo-slicer.sh"
fi

# Record nightly version used for the build
rustup run nightly rustc --version > "$PACKAGE_DIR/NIGHTLY_VERSION"

echo "Packaging $ARCHIVE_NAME..."
if $IS_WINDOWS; then
    # Use zip for Windows archives
    (cd "$STAGING_DIR" && zip -r "$DIST_DIR/$ARCHIVE_NAME" cargo-slicer/)
else
    tar -czf "$DIST_DIR/$ARCHIVE_NAME" -C "$STAGING_DIR" cargo-slicer/
fi

echo "Created: $DIST_DIR/$ARCHIVE_NAME"
echo "Contents:"
if $IS_WINDOWS; then
    unzip -l "$DIST_DIR/$ARCHIVE_NAME"
else
    tar -tzf "$DIST_DIR/$ARCHIVE_NAME"
fi

# ── Optional: Upload to GitHub release ─────────────────────────────────

if $UPLOAD; then
    VERSION="$(grep '^version' "$PROJECT_DIR/Cargo.toml" | head -1 | sed 's/.*"\(.*\)".*/\1/')"
    TAG="v${VERSION}"
    REPO="yijunyu/cargo-slicer"

    echo ""
    echo "=== Uploading to GitHub release $TAG ==="

    # Create release if it doesn't exist
    if ! gh release view "$TAG" --repo "$REPO" &>/dev/null; then
        echo "Creating release $TAG..."
        gh release create "$TAG" \
            --repo "$REPO" \
            --title "cargo-slicer $TAG" \
            --notes "Precompiled binaries for cargo-slicer.

Install:
\`\`\`bash
# Linux/macOS
curl -fsSL https://raw.githubusercontent.com/$REPO/main/install.sh | bash

# Windows (PowerShell)
irm https://raw.githubusercontent.com/$REPO/main/install.ps1 | iex
\`\`\`" \
            --latest
    fi

    echo "Uploading $ARCHIVE_NAME..."
    gh release upload "$TAG" "$DIST_DIR/$ARCHIVE_NAME" \
        --repo "$REPO" \
        --clobber

    echo "Done! Release: https://github.com/$REPO/releases/tag/$TAG"
fi

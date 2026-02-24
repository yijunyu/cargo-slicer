#!/usr/bin/env bash
# build-release.sh — Build and package cargo-slicer binaries for GitHub release
#
# Usage:
#   ./scripts/build-release.sh              # Build for current platform
#   ./scripts/build-release.sh --upload     # Build + upload to GitHub release
#
# Output:
#   dist/cargo-slicer-<target>.tar.gz       # Binary archive
#
# Prerequisites:
#   - Rust nightly toolchain
#   - gh CLI (for --upload)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Detect platform
ARCH="$(uname -m)"
OS="$(uname -s)"

case "$OS" in
    Linux)  OS_LABEL="unknown-linux-gnu" ;;
    Darwin) OS_LABEL="apple-darwin" ;;
    *)      echo "Unsupported OS: $OS" >&2; exit 1 ;;
esac

case "$ARCH" in
    x86_64)  ARCH_LABEL="x86_64" ;;
    aarch64|arm64) ARCH_LABEL="aarch64" ;;
    *)       echo "Unsupported architecture: $ARCH" >&2; exit 1 ;;
esac

TARGET="${ARCH_LABEL}-${OS_LABEL}"
DIST_DIR="$PROJECT_DIR/dist"
ARCHIVE_NAME="cargo-slicer-${TARGET}.tar.gz"

echo "=== Building cargo-slicer for $TARGET ==="

# ── Build all binaries ─────────────────────────────────────────────────

echo "Building cargo-slicer (stable)..."
cargo build --release --manifest-path "$PROJECT_DIR/Cargo.toml" 2>&1

echo "Building cargo-slicer-rustc + cargo_slicer_dispatch (nightly)..."
cargo +nightly build --manifest-path "$PROJECT_DIR/Cargo.toml" \
    --profile release-rustc \
    --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
    --features rustc-driver 2>&1

# ── Package ────────────────────────────────────────────────────────────

mkdir -p "$DIST_DIR"

STAGING_DIR="$(mktemp -d)"
trap 'rm -rf "$STAGING_DIR"' EXIT

PACKAGE_DIR="$STAGING_DIR/cargo-slicer"
mkdir -p "$PACKAGE_DIR"

# Copy binaries
cp "$PROJECT_DIR/target/release/cargo-slicer" "$PACKAGE_DIR/"
cp "$PROJECT_DIR/target/release-rustc/cargo-slicer-rustc" "$PACKAGE_DIR/"
cp "$PROJECT_DIR/target/release-rustc/cargo_slicer_dispatch" "$PACKAGE_DIR/"

# Copy the drop-in script
cp "$PROJECT_DIR/cargo-slicer.sh" "$PACKAGE_DIR/"
chmod +x "$PACKAGE_DIR/cargo-slicer.sh"

# Record nightly version used for the build
rustup run nightly rustc --version > "$PACKAGE_DIR/NIGHTLY_VERSION"

echo "Packaging $ARCHIVE_NAME..."
tar -czf "$DIST_DIR/$ARCHIVE_NAME" -C "$STAGING_DIR" cargo-slicer/

echo "Created: $DIST_DIR/$ARCHIVE_NAME"
echo "Contents:"
tar -tzf "$DIST_DIR/$ARCHIVE_NAME"

# ── Optional: Upload to GitHub release ─────────────────────────────────

if [[ "${1:-}" == "--upload" ]]; then
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

Install with:
\`\`\`
curl -fsSL https://raw.githubusercontent.com/$REPO/main/install.sh | bash
\`\`\`" \
            --latest
    fi

    echo "Uploading $ARCHIVE_NAME..."
    gh release upload "$TAG" "$DIST_DIR/$ARCHIVE_NAME" \
        --repo "$REPO" \
        --clobber

    echo "Done! Release: https://github.com/$REPO/releases/tag/$TAG"
fi

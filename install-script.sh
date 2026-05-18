#!/usr/bin/env bash
# install-script.sh — Minimal installer for `cargo-slicer script` (nightly -Zscript).
#
# Installs only what's needed to run single-file Rust scripts with cargo-slicer:
#   ~/.cargo/bin/cargo-slicer            — CLI (provides `script` subcommand)
#   ~/.cargo/bin/cargo_slicer_dispatch   — RUSTC_WRAPPER (dead-code elimination)
#
# Does NOT install: warmup cache, clang-daemon, nightly driver. For the full
# workspace-build feature set, use install.sh instead.
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install-script.sh | bash

set -euo pipefail

REPO="yijunyu/cargo-slicer"
INSTALL_DIR="${CARGO_HOME:-$HOME/.cargo}/bin"

# ── Platform ──────────────────────────────────────────────────────────

case "$(uname -s)" in
    Linux)  OS_LABEL="unknown-linux-gnu" ;;
    Darwin) OS_LABEL="apple-darwin" ;;
    *) echo "Unsupported OS: $(uname -s) (Linux/macOS only)" >&2; exit 1 ;;
esac

case "$(uname -m)" in
    x86_64)        ARCH_LABEL="x86_64" ;;
    aarch64|arm64) ARCH_LABEL="aarch64" ;;
    *) echo "Unsupported arch: $(uname -m) (x86_64/aarch64 only)" >&2; exit 1 ;;
esac

TARGET="${ARCH_LABEL}-${OS_LABEL}"
ARCHIVE="cargo-slicer-${TARGET}.tar.gz"

echo "cargo-slicer script installer"
echo "Target: ${TARGET}"
echo "Install: ${INSTALL_DIR}"
echo ""

# ── Prerequisites: nightly toolchain ──────────────────────────────────

if ! command -v rustup &>/dev/null; then
    echo "Error: rustup not found. Install Rust first: https://rustup.rs" >&2
    exit 1
fi

if ! rustup run nightly rustc --version &>/dev/null; then
    echo "Installing Rust nightly toolchain..."
    rustup toolchain install nightly
fi

# ── Find latest release ───────────────────────────────────────────────

echo "Fetching latest release..."
TAG="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" \
       | grep '"tag_name"' | head -1 | sed 's/.*: *"\(.*\)".*/\1/')"

if [[ -z "${TAG:-}" ]]; then
    echo "Error: no release found at https://github.com/$REPO/releases" >&2
    exit 1
fi
echo "Release: $TAG"

# ── Download + extract ────────────────────────────────────────────────

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

DOWNLOAD_URL="https://github.com/$REPO/releases/download/$TAG/$ARCHIVE"
echo "Downloading $ARCHIVE..."
curl -fSL --progress-bar -o "$TMPDIR/$ARCHIVE" "$DOWNLOAD_URL"
tar -xzf "$TMPDIR/$ARCHIVE" -C "$TMPDIR"

mkdir -p "$INSTALL_DIR"

# ── Install only the two binaries the script subcommand needs ─────────

for bin in cargo-slicer cargo_slicer_dispatch; do
    found=""
    for search in "$TMPDIR/$bin" "$TMPDIR/cargo-slicer/$bin"; do
        [[ -f "$search" ]] && found="$search" && break
    done
    if [[ -z "$found" ]]; then
        echo "Error: $bin not found in $ARCHIVE" >&2
        exit 1
    fi
    cp "$found" "$INSTALL_DIR/$bin"
    chmod +x "$INSTALL_DIR/$bin"
    echo "  Installed: $INSTALL_DIR/$bin"
done

# ── Verify ─────────────────────────────────────────────────────────────

echo ""
if ! "$INSTALL_DIR/cargo-slicer" script --check 2>&1; then
    echo "Warning: 'cargo-slicer script --check' reported a problem." >&2
fi

# ── PATH hint + minimal example ───────────────────────────────────────

if ! echo "$PATH" | tr ':' '\n' | grep -qx "$INSTALL_DIR"; then
    echo ""
    echo "Add ${INSTALL_DIR} to your PATH:"
    echo "  export PATH=\"$INSTALL_DIR:\$PATH\""
fi

cat <<'EXAMPLE'

============================================
Done. Try a single-file script:

  cat > hello.rs <<'RS'
  #!/usr/bin/env -S cargo-slicer script
  ---cargo
  [dependencies]
  regex = "1"
  ---
  fn main() {
      let re = regex::Regex::new(r"\w+").unwrap();
      println!("{:?}", re.find("hello world"));
  }
  RS
  chmod +x hello.rs
  ./hello.rs

Or invoke explicitly without changing the shebang:
  cargo-slicer script hello.rs
============================================
EXAMPLE

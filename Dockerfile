FROM rust:latest

# System deps
RUN apt-get update && apt-get install -y git sqlite3 curl clang bc cmake \
    libasound2-dev libxkbcommon-x11-dev libx11-xcb-dev && rm -rf /var/lib/apt/lists/*

# Nightly toolchain + rustc-dev for driver compilation
# Pin to 2026-04-05 — later nightlies moved rustc_middle::mir::mono
RUN rustup toolchain install nightly-2026-04-05 && rustup default nightly-2026-04-05
RUN rustup component add rustc-dev llvm-tools-preview --toolchain nightly-2026-04-05

# Install stable binary (cargo-slicer, cargo-warmup, cargo-slicer.sh etc.)
COPY install.sh /tmp/install.sh
RUN bash /tmp/install.sh || true

# Build driver binaries from source (needs rustc-dev)
COPY . /tmp/precc-src
RUN set -ex && \
    cd /tmp/precc-src && \
    CC=gcc cargo +nightly-2026-04-05 install --path . \
        --profile release-rustc \
        --bin cargo-slicer-rustc \
        --bin cargo_slicer_dispatch \
        --features rustc-driver \
        --no-default-features \
        --force && \
    CC=gcc cargo install --path . \
        --bin cargo_warmup_pch \
        --force && \
    rm -rf /tmp/precc-src

# Pre-warm registry cache
RUN cargo-warmup init --tier=1

# Entry point scripts

# build-slicer: accelerated build with virtual slicing + warmup cache
RUN cat > /usr/local/bin/build-slicer << 'EOF'
#!/bin/bash
set -e
cd /workspace/project

# Fetch deps (ensures registry is populated, no download during timed build)
echo "[build-slicer] Fetching dependencies..."
cargo fetch --quiet 2>/dev/null || true

# Pre-analyze if not already done
if [ ! -d .slicer-cache ]; then
    echo "[build-slicer] Running pre-analyze..."
    cargo-slicer pre-analyze
fi

export CARGO_SLICER_VIRTUAL=1
export CARGO_SLICER_CODEGEN_FILTER=1
export CARGO_SLICER_DRIVER="$(which cargo-slicer-rustc)"
export RUSTC_WRAPPER="$(which cargo_warmup_pch)"
export CARGO_WARMUP_INNER_WRAPPER="$(which cargo_warmup_dispatch)"
export CARGO_WARMUP_INNER_WRAPPER2="$(which cargo_slicer_dispatch)"
exec cargo +nightly-2026-04-05 build --release "$@"
EOF
RUN chmod +x /usr/local/bin/build-slicer

# build-baseline: fair baseline for comparison (cargo fetch + plain cargo build)
RUN cat > /usr/local/bin/build-baseline << 'EOF'
#!/bin/bash
set -e
cd /workspace/project

# Fetch deps first so download time is excluded from the build timing
echo "[build-baseline] Fetching dependencies..."
cargo fetch --quiet 2>/dev/null || true

echo "[build-baseline] Building (plain cargo build --release)..."
exec cargo +nightly-2026-04-05 build --release "$@"
EOF
RUN chmod +x /usr/local/bin/build-baseline

# bench: run both baseline and slicer with timing, fair comparison
RUN cat > /usr/local/bin/bench << 'EOF'
#!/bin/bash
set -e
cd /workspace/project

echo "=== Benchmark: $(basename $(pwd)) ==="
echo ""

# Shared: fetch deps once (both paths use the same registry cache)
echo "[bench] Fetching dependencies..."
cargo fetch --quiet 2>/dev/null || true
echo ""

# --- Baseline ---
echo "=== Baseline: cargo build --release ==="
cargo clean --quiet 2>/dev/null || true
BASELINE_START=$(date +%s%N)
cargo +nightly-2026-04-05 build --release "$@" 2>&1
BASELINE_END=$(date +%s%N)
BASELINE_MS=$(( (BASELINE_END - BASELINE_START) / 1000000 ))
echo ""

# --- Slicer ---
echo "=== Slicer: build-slicer ==="
cargo clean --quiet 2>/dev/null || true
rm -rf .slicer-cache .cache .seeds .mir-cache

SLICER_START=$(date +%s%N)

# Pre-analyze
echo "[slicer] Running pre-analyze..."
cargo-slicer pre-analyze

# Build with virtual slicing + warmup cache
export CARGO_SLICER_VIRTUAL=1
export CARGO_SLICER_CODEGEN_FILTER=1
export CARGO_SLICER_DRIVER="$(which cargo-slicer-rustc)"
export RUSTC_WRAPPER="$(which cargo_warmup_pch)"
export CARGO_WARMUP_INNER_WRAPPER="$(which cargo_warmup_dispatch)"
export CARGO_WARMUP_INNER_WRAPPER2="$(which cargo_slicer_dispatch)"
cargo +nightly-2026-04-05 build --release "$@" 2>&1

SLICER_END=$(date +%s%N)
SLICER_MS=$(( (SLICER_END - SLICER_START) / 1000000 ))
echo ""

# --- Results ---
BASELINE_S=$(echo "scale=1; $BASELINE_MS / 1000" | bc)
SLICER_S=$(echo "scale=1; $SLICER_MS / 1000" | bc)
if [ "$SLICER_MS" -gt 0 ]; then
    SPEEDUP=$(echo "scale=2; $BASELINE_MS / $SLICER_MS" | bc)
else
    SPEEDUP="inf"
fi
echo "=== Results ==="
echo "  Baseline: ${BASELINE_S}s"
echo "  Slicer:   ${SLICER_S}s"
echo "  Speedup:  ${SPEEDUP}x"
EOF
RUN chmod +x /usr/local/bin/bench

WORKDIR /workspace/project
CMD ["build-slicer"]

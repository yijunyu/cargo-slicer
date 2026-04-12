FROM rust:latest

# System deps
RUN apt-get update && apt-get install -y git sqlite3 curl clang && rm -rf /var/lib/apt/lists/*

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

# Entry point script
RUN cat > /usr/local/bin/build-slicer << 'EOF'
#!/bin/bash
set -e
cd /workspace/project

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

WORKDIR /workspace/project
CMD ["build-slicer"]

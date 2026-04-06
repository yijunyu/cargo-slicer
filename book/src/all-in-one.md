# The All-in-One Script

`cargo-slicer.sh` runs the full four-step pipeline automatically.

```bash
cargo-slicer.sh /path/to/your/project
# or, from inside the project:
cargo-slicer.sh .
```

Pass extra `cargo build` arguments after the project path:

```bash
cargo-slicer.sh . --features my-feature
cargo-slicer.sh . --no-default-features
```

## What it does

**Step 0 — Warm the registry cache**

```bash
cargo-warmup init --tier=1
```

Skipped if the cache is already warm. On first run this takes ~10–20 seconds
for tier-1 (the 10 most common registry crates).

**Step 1 — Pre-analyze the workspace call graph**

```bash
cargo-slicer pre-analyze
```

Builds a cross-crate call graph using `syn`-based static analysis. Writes
`.slicer-cache/*.analysis` and `.slicer-cache/*.seeds`. Takes 0.5 s (ripgrep)
to 12 s (zed).

**Step 2 — Plan the critical path**

```bash
cargo-warmup pch-plan
```

Schedules crate compilation in an order that minimises the critical path, so
parallelism is maximised across the three-layer wrapper chain.

**Step 3 — Build with the wrapper chain**

```bash
RUSTC_WRAPPER=cargo_warmup_dispatch \
CARGO_WARMUP_INNER_WRAPPER=cargo_slicer_dispatch \
CARGO_SLICER_VIRTUAL=1 \
CARGO_SLICER_CODEGEN_FILTER=1 \
CARGO_SLICER_DRIVER=$(which cargo-slicer-rustc) \
  cargo +nightly build --release "$@"
```

The three-layer chain:

1. `cargo_warmup_dispatch` — serves registry crates from cache (< 1 ms each)
2. `cargo_slicer_dispatch` — routes local crates to the driver or real rustc
3. `cargo-slicer-rustc` — stubs unreachable functions, filters CGUs

## Installation

`cargo-slicer.sh` is installed alongside the binary:

```bash
cargo install cargo-slicer
which cargo-slicer.sh   # → ~/.cargo/bin/cargo-slicer.sh
```

Or, from a source checkout:

```bash
./cargo-slicer.sh .     # runs directly from the repo
```

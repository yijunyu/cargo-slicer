# Crate Slicing Workflow

This document describes the automated workflow for slicing Rust crates to reduce build times.

## Overview

Crate slicing analyzes which items from dependency crates are actually used and generates minimal "sliced" versions containing only needed code. This can significantly reduce fresh build times.

## Workflow Script

```bash
#!/bin/bash
# scripts/slice_crates.sh - Automated crate slicing workflow

set -e

PROJECT_ROOT="${PROJECT_ROOT:-$(pwd)}"
SLICE_CACHE="${SLICE_CACHE:-$PROJECT_ROOT/sliced_crates}"
LOG_FILE="${LOG_FILE:-$PROJECT_ROOT/slice_crates.log}"

# Phase 1: Analyze dependency graph
phase1_analyze_deps() {
    echo "=== Phase 1: Analyzing dependency graph ===" | tee -a "$LOG_FILE"
    cargo metadata --format-version 1 > "$SLICE_CACHE/metadata.json"
    cargo slicer --analyze-deps
}

# Phase 2: Process crates in topological order
phase2_slice_crates() {
    echo "=== Phase 2: Slicing crates ===" | tee -a "$LOG_FILE"
    cargo slicer --slice-all --output "$SLICE_CACHE"
}

# Phase 3: Generate workspace with sliced crates
phase3_generate_workspace() {
    echo "=== Phase 3: Generating workspace ===" | tee -a "$LOG_FILE"
    cargo slicer --generate-workspace --output "$SLICE_CACHE"
}

# Phase 4: Verify sliced crates compile
phase4_verify() {
    echo "=== Phase 4: Verifying sliced crates ===" | tee -a "$LOG_FILE"
    cd "$SLICE_CACHE"
    cargo check --workspace
}

# Phase 5: Benchmark
phase5_benchmark() {
    echo "=== Phase 5: Benchmarking ===" | tee -a "$LOG_FILE"
    echo "Original build time:"
    cargo clean && time cargo build --release

    echo "Sliced build time:"
    cd "$SLICE_CACHE"
    cargo clean && time cargo build --release
}

# Main
main() {
    mkdir -p "$SLICE_CACHE"
    echo "Starting crate slicing workflow at $(date)" > "$LOG_FILE"

    phase1_analyze_deps
    phase2_slice_crates
    phase3_generate_workspace
    phase4_verify
    phase5_benchmark

    echo "Workflow completed at $(date)" >> "$LOG_FILE"
}

main "$@"
```

## Manual Workflow Steps

### Step 1: Identify Dependencies
```bash
# List all dependencies with versions
cargo tree --prefix none | sort -u

# Get full metadata as JSON
cargo metadata --format-version 1 | jq '.packages[] | {name, version, manifest_path}'
```

### Step 2: Analyze Usage
```bash
# Run cargo slicer in analysis mode
cargo slicer <crate_name> --analyze

# Output: List of used items from the crate
```

### Step 3: Locate Crate Source
```bash
# Find crate in cargo cache
ls ~/.cargo/registry/src/*//<crate_name>-<version>/
```

### Step 4: Generate Sliced Crate
```bash
# Generate sliced version
cargo slicer <crate_name> --output sliced_crates/<crate_name>
```

### Step 5: Fix Compilation Errors
Common fixes needed:
1. **Missing trait impls**: Add `#[derive(Clone, Debug)]` or manual impls
2. **Missing types**: Add stub types for external dependencies
3. **Generic parameters**: Ensure generic structs have correct bounds
4. **Feature flags**: Add features to Cargo.toml if needed

### Step 6: Verify and Benchmark
```bash
# Check sliced crate compiles
cd sliced_crates/<crate_name> && cargo check

# Benchmark build time
cargo clean && time cargo build --release
```

## Iteration Cycle

```
┌─────────────────────────────────────────────────────────────┐
│  1. Slice crate                                              │
│     └─> cargo slicer <crate>                                 │
├─────────────────────────────────────────────────────────────┤
│  2. Attempt compilation                                      │
│     └─> cd sliced_crates/<crate> && cargo check             │
├─────────────────────────────────────────────────────────────┤
│  3. If errors:                                               │
│     a. Analyze error type (missing type, trait, etc.)       │
│     b. Add fix to cargo_slicer.rs (automated stub gen)       │
│     c. Re-run cargo-slicer                                   │
│     d. Goto step 2                                          │
├─────────────────────────────────────────────────────────────┤
│  4. If success:                                              │
│     a. Benchmark build time                                 │
│     b. Record in interaction log                            │
│     c. Move to next crate                                   │
└─────────────────────────────────────────────────────────────┘
```

## Error Categories and Automated Fixes

| Error Pattern | Automated Fix |
|---------------|---------------|
| `struct X takes N generic arguments` | Parse generic params, add PhantomData |
| `trait bound X: Clone not satisfied` | Add `#[derive(Clone)]` or remove derive |
| `X doesn't implement Debug` | Add `#[derive(Debug)]` or manual impl |
| `cannot find type X` | Generate stub type |
| `cannot find module X` | Generate stub module |
| `unresolved import X` | Add `use` statement or stub |

## Performance Targets

| Metric | Target | Notes |
|--------|--------|-------|
| Slicing time per crate | < 1s | Acceptable overhead |
| Build time reduction | > 50% | Worth the complexity |
| Code reduction | > 80% | Typical for focused usage |
| Success rate | > 95% | Minimal manual intervention |

## File Structure

```
precc/
├── src/cargo_slicer.rs          # Main slicing tool (cargo-slicer)
├── sliced_crates/              # Generated sliced crates
│   ├── Cargo.toml              # Workspace manifest
│   ├── regex-sliced/
│   ├── serde-sliced/
│   └── ...
├── docs/
│   ├── crate_slicing_workflow.md   # This file
│   └── crate_slicing_log.md        # Interaction log
└── scripts/
    └── slice_crates.sh         # Automation script
```

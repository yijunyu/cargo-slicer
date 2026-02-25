# Review Feedback from Amanieu

**Date**: 2026-02-22
**Context**: Amanieu (Rust compiler team) reviewed cargo-slicer's virtual slicing approach and suggested an alternative: **deferred compilation**. Instead of analyzing call graphs and stubbing unreachable functions at MIR level, simply emit metadata-only `.rlib` placeholders for dependency crates, let Cargo pipeline downstream compilation, then re-compile dependencies with selective codegen before linking.

## Amanieu's Suggestion

> Instead of MIR stubbing, consider deferred compilation: emit metadata-only `.rlib` placeholders for library crates so Cargo can pipeline downstream crates immediately, then re-compile the libraries with full codegen (only the functions actually needed) before linking the binary.

The key insight is that this avoids the complexity of call graph analysis and MIR body replacement, while achieving similar build parallelism gains through Cargo's pipelining mechanism.

## Implementation

We implemented this as `CARGO_SLICER_DEFERRED=1`, a two-pass compilation mode:

### Pass 1: Metadata-Only (Library Crates)

- Dispatch replaces `--emit=dep-info,metadata,link` with `--emit=dep-info,metadata` in compiler args
- Driver runs analysis + BFS + cache-saving, returns `Compilation::Continue`
- No LLVM codegen runs (rustc's codegen phase is a no-op with metadata-only emit)
- Dispatch wraps the `.rmeta` into a placeholder `.rlib` (ar archive with member `lib.rmeta`)
- Cargo sees the `.rlib` and pipelines downstream crates immediately
- Compilation args are recorded in `.lazy-pending` files for Pass 2

### Pass 2: Selective Codegen (Before Binary)

- When the binary crate is dispatched, Pass 2 runs first for all pending library crates
- Each library is re-compiled with `CARGO_SLICER_PHASE=lazy-codegen` using cached marked items
- Codegen filtering (CGU filtering via `collect_and_partition_mono_items` override) removes stubbed items
- The placeholder `.rlib` is replaced with a real one containing object code
- Binary compilation then proceeds normally

### Files Changed

| File | Changes |
|------|---------|
| `src/bin/cargo_slicer_dispatch.rs` | `--emit` replacement, placeholder `.rlib` creation (with `lib.rmeta` member name), `CARGO_SLICER_DEFERRED` env var |
| `src/bin/cargo_slicer_rustc.rs` | `after_lazy_metadata_phase()`: removed stub-all codegen, excluded `lazy-metadata` from `enable_codegen_filter` |

## Experimental Results

### Zeroclaw Workspace (2 crates: zeroclaw lib + zeroclaw bin)

| Mode | Wall Time | vs Baseline |
|------|-----------|-------------|
| Baseline (no slicing) | 2m 05s | -- |
| vslice-cc (single-pass, existing) | 3m 22s | +37% |
| **Deferred (two-pass, new)** | **3m 57s** | **+90%** |

### Analysis

The deferred mode is **slower** than both baseline and the existing single-pass vslice-cc mode on zeroclaw. The overhead comes from:

1. **Double compilation of library crates**: Pass 1 (metadata) + Pass 2 (codegen) is inherently more total work than a single compilation with codegen filtering.
2. **Driver loading overhead**: Each pass loads `librustc_driver.so` (~300ms). Two passes = double the loading cost.
3. **Minimal pipeline benefit**: Zeroclaw has only 1 workspace library crate and 1 binary. There's no opportunity for Cargo pipelining between workspace crates — the binary must wait for the library regardless.

### Why Overlap Doesn't Work

We also attempted overlapping Pass 2 with binary compilation (`CARGO_SLICER_OVERLAP=1`), which would let the binary start compiling while Pass 2 writes real `.rlib` files in the background. This fails because:

- **`.rmeta` from `--emit=metadata` does not include optimized MIR**. The binary crate's codegen needs cross-crate MIR for monomorphization and inlining. When the binary's rustc tries to read MIR from the placeholder `.rlib`, it fails with `error: missing optimized MIR (was the crate compiled with --emit=metadata?)`.
- This is a fundamental limitation of the metadata-only approach: metadata is sufficient for type checking but not for codegen.

### Where Deferred Mode Could Help (Theoretical)

The benefit of deferred mode comes from **pipeline parallelism** in large workspaces:

```
Without deferred (sequential):
  crate_a: [======compile======]
  crate_b:                       [======compile======]  (waits for a)
  crate_c:                                              [======compile======]
  binary:                                                                     [===compile===]

With deferred (pipelined):
  crate_a: [=meta=]                   [==codegen==]
  crate_b:         [=meta=]           [==codegen==]
  crate_c:                  [=meta=]  [==codegen==]
  binary:                                           [===compile===]
```

In the pipelined case, downstream crates start their metadata pass sooner because upstream metadata-only passes are fast. This reduces the critical path length when there are many crates in a dependency chain.

For this to outweigh the double-compilation overhead, you need:
- 10+ workspace crates with sequential dependencies
- Deep dependency chains (not wide, independent crates)
- The critical path dominated by workspace crate compilation, not external deps

## Conclusions

1. **Amanieu's deferred compilation idea is sound in theory** but its benefits require either:
   - Modifications to rustc itself (not possible as a `RUSTC_WRAPPER`)
   - Very large workspaces with deep dependency chains

2. **As a `RUSTC_WRAPPER`, single-pass codegen filtering (vslice-cc) remains the most effective approach.** It has the lowest overhead: one compilation per crate, with MIR body replacement and CGU filtering reducing LLVM work.

3. **The implementation is kept as an experimental feature** (`CARGO_SLICER_DEFERRED=1`) for:
   - Future testing on larger workspaces
   - Foundation for potential rustc-integrated deferred codegen
   - Demonstrating the approach's limitations and trade-offs

## Technical Lessons Learned

- **Placeholder `.rlib` format**: rustc expects the ar archive member to be named `lib.rmeta`, not the original filename. Using `ar rcs out.rlib libfoo-hash.rmeta` creates a member named `libfoo-hash.rmeta` which rustc rejects with `E0786: found invalid metadata files`.
- **Codegen overrides and metadata**: The `optimized_mir` and `collect_and_partition_mono_items` query overrides must NOT be installed during metadata-only compilation (`lazy-metadata` phase). Even though they wouldn't be called (no codegen), their presence can interfere with metadata serialization.
- **`--emit=metadata` vs MIR availability**: `.rmeta` produced by `--emit=metadata` contains type information and trait impls but NOT optimized MIR. This is sufficient for downstream type checking but not for codegen (monomorphization, cross-crate inlining). This rules out concurrent binary compilation against metadata-only placeholders.

## Acknowledgment

Thank you, Amanieu, for suggesting the deferred compilation approach. While the `RUSTC_WRAPPER` constraint limits the benefit compared to a rustc-integrated solution, the investigation clarified important trade-offs between pipelining and total work, and the limitations of metadata-only compilation for cross-crate codegen.

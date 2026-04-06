# Virtual Slicer

The virtual slicer is a `RUSTC_WRAPPER` that stubs unreachable functions at the
MIR level before LLVM sees them. It does not modify your source files or your
`Cargo.toml`.

## What "unreachable" means

Starting from the binary's `main` function, cargo-slicer traces the call graph
across all workspace crates. Any function that cannot be reached from `main` is
replaced with an `abort()` body. LLVM skips compilation, optimisation, and code
emission for those functions entirely.

The analysis is conservative: trait impls, generics, async functions, closures,
and any function called through a function pointer are always kept.

## How it plugs into cargo

```
RUSTC_WRAPPER=cargo_slicer_dispatch  ← stable binary, < 1 ms startup
    │
    └─ local workspace crate?
           CARGO_SLICER_DRIVER=cargo-slicer-rustc  ← nightly driver, ~300 ms startup
               └─ BFS reachability analysis
               └─ MIR stub replacement
               └─ CGU filtering (skip codegen units with only stubs)
```

The dispatch binary keeps the nightly driver out of the fast path. Registry
crates (which change rarely and are cached) never pay the 300 ms driver load.

## Cross-crate pre-analysis

For accurate reachability across crate boundaries, run pre-analysis before the
build:

```bash
cargo-slicer pre-analyze
```

This uses `syn`-based parsing to build a call graph across all workspace crates
in seconds, writing results to `.slicer-cache/`. The driver reads these files at
build time instead of re-analysing from scratch for every crate.

Without pre-analysis the slicer falls back to conservative per-crate analysis,
which still works but produces fewer stubs.

## Tuning

| Environment variable | Effect |
|----------------------|--------|
| `CARGO_SLICER_VIRTUAL=1` | Enable virtual slicing |
| `CARGO_SLICER_CODEGEN_FILTER=1` | Skip CGUs that contain only stubs |
| `CARGO_SLICER_DEBUG=1` | Write a debug log to `.cargo-slicer-debug.log` |
| `CARGO_SLICER_SKIP_THRESHOLD=auto` | Skip driver for crates with no predicted stubs (default) |
| `CARGO_SLICER_SKIP_THRESHOLD=0` | Always load the driver for every local crate |

## What cannot be stubbed

The slicer never stubs:

- Trait impl associated functions (vtable entries)
- Generic functions (monomorphised at the call site)
- `async fn` and closures
- `unsafe fn` (unless `CARGO_SLICER_RELAX_UNSAFE=1`)
- Any function reachable through a function pointer

These constraints are intentional. Stubbing them would either cause linker
errors or produce incorrect binaries.

## Upstream proposal

The virtual slicing logic has been extracted into a proposed rustc patch behind
a `-Z dead-fn-elimination` flag. If accepted upstream, the install story becomes:

```bash
RUSTFLAGS="-Z dead-fn-elimination" cargo +nightly build --release
```

No extra binary, no nightly ABI compatibility shims.

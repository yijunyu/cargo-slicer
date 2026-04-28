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

### Auto-delegation in `cargo_slicer_dispatch`

The dispatch wrapper detects whether the real rustc already implements the
flag (`rustc -Zhelp | grep dead-fn-elimination`, cached per process). When
present, dispatch transparently appends `-Zdead-fn-elimination` and execs
rustc directly — bypassing the userspace driver, the `.slicer-cache`, and
the per-nightly ABI shim. The userspace path is still used as a fallback
when the flag is absent.

```bash
# Patched stable: dispatch detects the flag and delegates automatically
CARGO_SLICER_VIRTUAL=1 RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +dfe-stage1 build --release

# Same command on unpatched nightly: dispatch falls back to the userspace driver
CARGO_SLICER_VIRTUAL=1 RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +nightly build --release
```

Override knobs: `CARGO_SLICER_NO_UPSTREAM_FLAG=1` forces the userspace
path even when the flag is available; `CARGO_SLICER_FORCE_UPSTREAM_FLAG=1`
skips the probe and assumes the flag is supported.

### Review status (2026-04)

The patch has been through a nine-point review by
[@petrochenkov][vadim-review] (V1–V9 in the response document). After
applying the patches:

- Scope is now explicit: **binary crates only** — library crate types
  early-return (V1).
- Seeds are `reachable_set ∪ entry_fn ∪ address_taken ∪ vtable_methods`
  rather than a re-implementation of `reachable_set` (V3, V5a, V5b, V8).
- The cross-crate call graph has been removed; cross-crate effects come
  through `reachable_set` seeding (V4).
- `requires_monomorphization` replaces blanket `generics > 0`, so
  lifetime-only generics are eligible (V6b).
- Vestigial `#[test]`/`#[bench]`, `unsafe`, and signature-walk checks have
  been deleted — their safety is now provided structurally by the seed set
  (V7, V8a, V8b).
- Net effect on `dead_fn_elim.rs`: **~419 → ~340 lines**.

**Empirical answer to V9** ("will it converge to `reachable_set`?"): on a
2,669-crate ASE 2026 corpus sweep, the userspace slicer (same algorithm)
produced **zero correctness regressions** across 2,452 cleanly-built
crates and a **1.50× median build speedup**. The patched stage1 oracle on
`rust-1.90.0` eliminated **904 functions** on ripgrep with binary output
identical to the baseline.

Full point-by-point response: [`vadim-response-results.md`][vadim-results].

[vadim-review]: https://github.com/yijunyu/cargo-slicer/blob/main/docs/vadim-petrochenkov-review-feedback.md
[vadim-results]: https://github.com/yijunyu/cargo-slicer/blob/main/docs/vadim-response-results.md

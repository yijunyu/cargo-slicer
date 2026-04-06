# How It Works

Rust builds are slow for a structural reason: `rustc` compiles each crate in
isolation. When it compiles a library crate it cannot know which of its public
functions will be called by downstream code, so it compiles all of them. In a
large workspace most of that work is wasted.

cargo-slicer attacks the problem from two angles simultaneously.

## The two techniques

### Virtual Slicer

The virtual slicer inserts itself as a `RUSTC_WRAPPER`. Before each crate is
compiled it performs a reachability analysis — a BFS starting from the entry
point of the binary — and replaces every unreachable function body with an
`abort()` stub. LLVM never sees those functions, so it never optimises,
inlines, or emits machine code for them.

For a crate like `image`, which exposes hundreds of format decoders and pixel
converters, a typical application uses one or two formats. The slicer stubs the
rest. LLVM's work drops by 97%.

→ [Virtual Slicer details](virtual-slicer.md)

### Warm-Cache Daemon

Registry crates (crates.io dependencies) do not change between builds. The
warm-cache daemon pre-compiles them once and stores the resulting `.rlib` and
`.rmeta` artefacts. On every subsequent build, instead of re-running `rustc`,
the `RUSTC_WRAPPER` copies the cached artefact into `target/` in milliseconds.

The cache key is `SHA256(crate + version + rustc_version + features + opt_level)`,
so the cached artefact is safe to share across projects and across git branches.
A warmed cache built while compiling `zed` is reused immediately when compiling
`nushell`.

→ [Warm-Cache Daemon details](warm-cache.md)

## The three-layer pipeline

When both techniques run together, three wrappers are chained:

```
cargo build --release
    │
    ▼  RUSTC_WRAPPER = cargo_warmup_dispatch
    │  ├─ registry crate? → serve from cache, return immediately
    │  └─ local crate? → pass to next wrapper
    │
    ▼  CARGO_WARMUP_INNER_WRAPPER = cargo_slicer_dispatch
    │  ├─ no unreachable fns (cache hit)? → pass to real rustc directly
    │  └─ has unreachable fns? → pass to driver
    │
    ▼  CARGO_SLICER_DRIVER = cargo-slicer-rustc
       └─ MIR analysis → stub unreachable fns → LLVM codegen on minimum set
```

The `cargo-slicer.sh` script sets up this chain automatically.

→ [The All-in-One Script](all-in-one.md)

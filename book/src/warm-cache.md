# Warm-Cache Daemon

The warm-cache daemon (also called `cargo-warmup`) pre-compiles registry
crates once and serves the cached `.rlib` / `.rmeta` artefacts on every
subsequent build. It is the Rust equivalent of a precompiled-header daemon for
C/C++.

## The insight

Registry crates do not change between your builds. `syn`, `serde`, `tokio`,
`proc-macro2` — these are compiled identically every time you run `cargo clean
&& cargo build`. Compilation caches like `sccache` help on the *second* build,
but every fresh environment (new developer, CI machine, Docker container) pays
the full cost again.

The warm-cache daemon shifts that cost to a one-time investment. Pre-warm the
top-tier registry crates once (takes ~10 minutes). Every cold build afterwards
— in any project that depends on those crates — skips their compilation
entirely.

## Cache key

The cache key is:

```
SHA256(crate_name + version + rustc_version + edition + features + opt_level)
```

`-C metadata` and `-C extra-filename` are excluded. These differ per project but
do not affect the correctness of the compiled artefact. Excluding them is what
enables cross-project sharing: the `.rlib` compiled while building `zed` is
reused directly when building `nushell`.

## Usage

```bash
# One-time warm (adds ~10 min, saves that time on every cold build after)
cargo-warmup init --tier=1

# Check cache status
cargo-warmup status

# Use in builds
RUSTC_WRAPPER=$(which cargo_warmup_dispatch) cargo +nightly build --release
```

`cargo-slicer.sh` runs `cargo-warmup init --tier=1` automatically on first use.

## Tiers

| Tier | Crates included | Warm time |
|------|----------------|-----------|
| `--tier=1` | proc-macro2, quote, syn, serde, tokio, + 5 more core crates | ~20 s |
| `--tier=2` | + 50 most common transitive deps | ~3 min |
| `--tier=3` | All crates.io top-500 | ~10 min |

Tier 1 gives the best return on investment for most projects. Tier 3 is useful
in CI environments where build time is money.

## How it plugs into cargo

```
RUSTC_WRAPPER=cargo_warmup_dispatch
    │
    ├─ cache hit?  → copy .rlib to target/, return in < 1 ms
    └─ cache miss? → invoke real rustc, store result in cache
```

The dispatch binary adds less than 1 ms per crate invocation on a cache hit.

## Sharing the cache across projects

By default the cache lives in `~/.cargo/warmup-cache/`. Any project on the same
machine with matching crate versions and rustc toolchain automatically benefits
from a warm cache built by any other project.

To inspect what is cached:

```bash
cargo-warmup status
# or
sqlite3 ~/.cargo/warmup-cache/index.db \
  'SELECT crate, version, cached_at FROM artefacts ORDER BY cached_at DESC LIMIT 20'
```

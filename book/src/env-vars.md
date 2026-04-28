# Environment Variables

## Core

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_VIRTUAL` | unset | Set to `1` to enable virtual slicing |
| `CARGO_SLICER_CODEGEN_FILTER` | unset | Set to `1` to skip CGUs containing only stubs |
| `RUSTC_WRAPPER` | unset | Set to path of `cargo_slicer_dispatch` |
| `CARGO_SLICER_DRIVER` | unset | Set to path of `cargo-slicer-rustc` |

## Cross-crate analysis

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_CROSS_CRATE` | unset | Set to `1` to enable cross-crate analysis |
| `CARGO_SLICER_PARSER` | `syn` | Pre-analysis backend: `syn`, `fast`, or `ctags` |

## MIR-precise analysis

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_MIR_PRECISE` | unset | Set to `1` for MIR-level whole-program analysis |
| `CARGO_SLICER_WORKSPACE_CRATES` | unset | Comma-separated list of workspace crates to harvest |

## Performance tuning

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_SKIP_THRESHOLD` | `auto` | Skip driver when predicted stubs < threshold. `auto` = skip 0-stub crates; `0`/`never` = never skip |
| `CARGO_SLICER_DAEMON` | unset | Set to `1` to enable fork-server (amortises 300 ms driver load) |
| `CARGO_SLICER_SCCACHE` | auto | Path to sccache, or `/nonexistent` to disable |
| `CARGO_SLICER_RELAX_UNSAFE` | unset | Set to `1` to allow stubbing `unsafe fn` |

## Upstream `-Z dead-fn-elimination` delegation

When the real rustc supports the in-tree MCP flag, dispatch delegates
automatically (see [Virtual Slicer → Auto-delegation](virtual-slicer.md#auto-delegation-in-cargo_slicer_dispatch)).

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_NO_UPSTREAM_FLAG` | unset | Set to `1` to force the userspace driver even when `-Z dead-fn-elimination` is available |
| `CARGO_SLICER_FORCE_UPSTREAM_FLAG` | unset | Set to `1` to skip the `rustc -Zhelp` probe and assume the flag is present |

## Caching

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_CACHE_DIR` | `.slicer-cache` | Directory for incremental cache files |
| `CARGO_SLICER_NO_CACHE` | unset | Set to `1` to disable caching entirely |

## Debugging

| Variable | Default | Description |
|----------|---------|-------------|
| `CARGO_SLICER_DEBUG` | unset | Set to `1` to enable debug logging |
| `CARGO_SLICER_DEBUG_LOG` | `.cargo-slicer-debug.log` | Custom path for debug log |
| `CARGO_SLICER_MARKED_OUT` | unset | Write marked items to a file for inspection |

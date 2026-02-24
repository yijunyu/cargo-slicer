# cargo-slicer Design Document

## Overview

cargo-slicer reduces Rust build times by identifying and eliminating unused code. It has evolved through two main approaches:

1. **Source slicing** (legacy): Copy dependency source code, parse it, remove unused items, rebuild from modified sources.
2. **Virtual slicing** (recommended): Intercept compilation as a `RUSTC_WRAPPER`, analyze reachability via MIR, and replace unreachable function bodies with abort stubs — skipping LLVM codegen without modifying any source files.

Both approaches rely on parsing Rust code to build a dependency graph. Four parsers have been implemented, each with different accuracy/speed tradeoffs.

---

## The Four Parsers

### 1. Syn-based AST Parser

**Files**: `src/old_slicer/parsing.rs`

The original parser. Uses the `syn` crate to parse Rust source files into a full AST, then extracts item definitions (functions, structs, enums, traits, impls) and their dependencies (type references, function calls, trait bounds).

- **Technique**: `syn::parse_file()` followed by visitor-based AST traversal
- **Accuracy**: ~70%. Handles most Rust syntax but struggles with macros, procedural macros, and code that depends on edition-specific or feature-gated syntax.
- **Speed**: Moderate. Full AST parsing is thorough but not cheap.
- **Limitations**: Cannot resolve imports across crate boundaries. Macro-expanded code is invisible. Feature-gated items may be incorrectly evaluated.
- **Role**: Fallback parser. Used when the rustc driver is unavailable or fails (e.g., on code with compile errors).

### 2. Ctags-based Text Parser

**Files**: `src/ctags_collector.rs`, `src/slicer/ctags_parser.rs`, `src/ctags_rs/`

A lightweight parser inspired by Universal Ctags. Scans source text line-by-line using pattern matching to identify item boundaries without building a full AST.

- **Technique**: Regex and pattern-based line scanning. Maps ctags "kind" characters (`f`=function, `s`=struct, `e`=enum) to internal item types. Uses `TextExtractor` to convert line numbers to byte ranges.
- **Accuracy**: ~60%. Good at detecting item definitions but poor at resolving cross-item dependencies.
- **Speed**: Fast. No AST construction overhead.
- **Limitations**: No semantic understanding of Rust. Cannot track type references, trait implementations, or complex dependency chains. Skips methods and fields.
- **Role**: Optional alternative. Enabled with `-fuse-ctags`. Was explored for 4-8x speedup over syn but ultimately limited by dependency extraction accuracy.

### 3. Fast Tokenizer

**Files**: `src/slicer/fast_tokenizer.rs`

The lightest-weight approach. A single-pass state machine that counts braces to identify item boundaries, tracks visibility modifiers, and computes byte ranges.

- **Technique**: Character-by-character scanning with brace-depth tracking. Identifies `fn`, `struct`, `enum`, `trait`, `impl`, `const`, `static`, `type`, `mod`, `use` items by keyword.
- **Accuracy**: ~50%. Focused purely on item extraction — does not analyze dependencies at all.
- **Speed**: Very fast. Minimal memory allocation, no parsing overhead.
- **Limitations**: Only identifies items and their byte ranges. Dependency information must come from a separate pass (e.g., `text_dependencies.rs` with Aho-Corasick text matching).
- **Role**: Optional. Enabled with `-fuse-fast-tokenizer`. Best suited for initial item extraction when combined with text-based dependency analysis.

### 4. Rustc Driver (Compiler-Based)

**Files**: `src/bin/cargo_slicer_rustc.rs`, `src/rustc_integration/hir_visitor.rs`, `src/rustc_integration/virtual_slicer.rs`, `src/rustc_subprocess.rs`

Uses the Rust compiler itself to analyze code. The driver binary links against `rustc_driver` and hooks into the compiler's `after_analysis` callback to inspect HIR (High-level IR) and MIR (Mid-level IR).

- **Technique**: Runs as the actual compiler. Iterates HIR items for definitions, visits MIR basic blocks for call edges and function-item references. Sees exactly what the compiler sees — including macro expansions, trait resolution, and monomorphization context.
- **Accuracy**: ~100%. The compiler is the ground truth for Rust semantics.
- **Speed**: Same as a normal compilation (the analysis piggybacks on the compile).
- **Limitations**: Requires nightly Rust (uses unstable `rustc_private` APIs). Cannot analyze code that doesn't compile.
- **Role**: Default parser for source slicing. Foundation for virtual slicing.

### Parser Comparison

| Parser | Accuracy | Speed | Dependencies | Macros | Requires Nightly |
|--------|----------|-------|-------------|--------|-------------------|
| Syn AST | ~70% | Moderate | Yes | No | No |
| Ctags | ~60% | Fast | Limited | No | No |
| Fast Tokenizer | ~50% | Very fast | No | No | No |
| Rustc Driver | ~100% | Same as compile | Yes | Yes | Yes |

### Parser Selection

The parser is selected via feature flags in `src/slicer/features.rs`:

```
Default:   rustc driver (via cargo)
Fallback:  syn AST parser (when rustc driver fails)
Optional:  -fuse-ctags, -fuse-fast-tokenizer
```

When the rustc driver succeeds, its output is authoritative. When it fails (e.g., the crate has compile errors), the system falls back to the syn parser automatically.

---

## Source Slicing Architecture (Legacy)

The original copy-and-delete pipeline, orchestrated by `src/slicer/mod.rs`:

```
 1. Copy        Copy dependency source to sliced_crates/
 2. Parse       Extract items and dependencies (via selected parser)
 3. Graph       Build item dependency graph
 4. Mark        BFS from entry points to mark reachable items
 5. Delete      Remove unmarked items from copied source
 6. Cleanup     Remove unused imports and empty modules
 7. Verify      Optional cargo check on modified source
```

**Key files**:
- `src/slicer/copy.rs` — Copy source files and fix Cargo.toml paths
- `src/slicer/dependency_graph.rs` — Build what-calls-what graph
- `src/slicer/marker.rs` — BFS marking from entry points
- `src/slicer/deleter.rs` — AST-based item removal
- `src/slicer/cache.rs` — Incremental caching via source hashing

**Optimization levels** (`-O0` to `-O3`) control deletion aggressiveness:
- `-O0`: No deletion (baseline)
- `-O1`: Delete unused private functions
- `-O2`: Delete all unused private items + trial deletion
- `-O3`: Add private struct/enum deletion

**Limitations**: Modifies source files, which can break macro-dependent code. Requires rebuilding modified sources. Does not integrate with sccache or incremental compilation.

---

## Virtual Slicing Architecture (Recommended)

Virtual slicing eliminates unreachable code at the compiler level without modifying any source files. It works as a transparent `RUSTC_WRAPPER` that intercepts each crate compilation.

### Rationale

Source slicing has fundamental limitations:

1. **Source modification breaks macros**: Deleting items from source can break `cfg_if!`, `macro_rules!`, and procedural macros that reference items by name in ways invisible to AST analysis.
2. **Incompatible with caching**: Modified source files invalidate sccache and cargo's incremental compilation, negating much of the build-time savings.
3. **Fragile Cargo.toml rewriting**: Redirecting dependencies to sliced copies requires rewriting path dependencies, which interacts poorly with workspaces, build scripts, and feature resolution.
4. **Limited to dependencies**: Source slicing only works on third-party crates. Virtual slicing analyzes the project's own crates too.

Virtual slicing avoids all of these by operating inside the compiler:

- No source files are modified
- Third-party crates go through sccache normally
- The compiler itself determines reachability (100% accurate)
- Works on any crate in the build graph, not just dependencies

### Components

#### Dispatch Binary (`src/bin/cargo_slicer_dispatch.rs`)

A thin (~100 lines) binary that acts as `RUSTC_WRAPPER`. It does NOT link `rustc_driver`, so it loads in under 1ms. Its job is routing:

- **Probe commands** (`-vV`, `--print`): Forward to real `rustc` directly
- **Non-local crates** (from `.cargo/registry`, `.cargo/git`, `/vendor/`): Forward to sccache (if available) or real `rustc`
- **Local crates** with virtual slicing enabled: Forward to the driver binary

This routing is critical for performance. Without it, every crate compilation (including hundreds of third-party deps) would load the 300ms+ `rustc_driver` shared library.

#### Driver Binary (`src/bin/cargo_slicer_rustc.rs`)

The full analysis driver. Links `rustc_driver` and hooks into two compiler queries:

**`mir_built` override** — Captures the pre-optimization call graph:
```
For each function's MIR body:
  - Scan all basic block statements for Operand::Constant with FnDef type
    (catches function items passed as values, e.g. operate(&get_basename))
  - Scan all call terminators for FnDef call targets
  - Scan all call arguments for FnDef constants
  - Record caller→callee edges in a global call graph
```

This runs before MIR optimization, so it captures call edges that inlining would later erase.

**`optimized_mir` override** — Replaces unreachable function bodies:
```
For each function:
  if is_marked(function):
    return original optimized MIR  (normal codegen)
  if is_safe_to_stub(function):
    return abort stub MIR          (minimal codegen)
  else:
    return original optimized MIR  (conservative: keep it)
```

The abort stub is a minimal MIR body that just calls `core::intrinsics::abort()`. LLVM generates a few bytes for it instead of the full function body.

**Safety exclusions** — Functions that are never stubbed:
- Public functions in library crates (downstream crates may link against them)
- Trait impl methods (unless vtable analysis proves no dynamic dispatch exists)
- Generic functions (monomorphization creates concrete instances invisible to pre-mono analysis)
- Async functions (complex state machine codegen)
- Closures (references may be invisible after parent inlining)
- Unsafe functions (may use transmute/raw pointers for hidden dispatch; opt-in relaxation via `CARGO_SLICER_RELAX_UNSAFE=1`)
- Functions with function-pointer or `dyn Trait` parameters (dynamic callees)
- Drop impls (detected via `tcx.lang_items().drop_trait()` lang item check)
- `#[no_mangle]` / `#[export_name]` / `#[used]` functions (externally callable)

#### BFS Reachability

The `after_analysis` callback runs BFS to determine which functions are reachable:

**Seeds**:
- Binary crates: `main` only (aggressive — most internal code becomes unreachable)
- Library crates: all `pub` items (conservative — the public API is the contract)
- Both: `Drop` impls, object-safe trait impls, statics

**Graph**: The pre-optimization call graph from the `mir_built` override, plus HIR-level item dependencies.

**Output**: A set of marked (reachable) items. Everything else is a candidate for stubbing.

#### Caching (`src/rustc_integration/virtual_slicer.rs`)

Analysis results are cached in `.slicer-cache/` keyed by:
- Crate name
- Crate type (lib/bin)
- SHA-256 hash of the crate root source file

If the source hasn't changed, the cached marked-items set is reused without re-running analysis.

### Data Flow

```
cargo build --release
  │
  ├─ RUSTC_WRAPPER=cargo_slicer_dispatch
  │
  ├─ Third-party crate (.cargo/registry/...)
  │    └─ dispatch → sccache → rustc (normal compilation)
  │
  └─ Local crate (src/...)
       └─ dispatch → cargo-slicer-rustc (driver)
            │
            ├─ mir_built override: record call graph edges
            ├─ after_analysis: BFS reachability → marked items
            ├─ optimized_mir override: stub unmarked functions
            └─ Continue to LLVM codegen (with stubs)
```

### Environment Variables

| Variable | Purpose |
|----------|---------|
| `CARGO_SLICER_VIRTUAL=1` | Enable virtual slicing |
| `CARGO_SLICER_CODEGEN_FILTER=1` | Enable MIR abort stub replacement |
| `RUSTC_WRAPPER=/path/to/cargo_slicer_dispatch` | Set dispatch as wrapper |
| `CARGO_SLICER_DEBUG=1` | Write debug log to `.cargo-slicer-debug.log` |
| `CARGO_SLICER_SCCACHE=/path/to/sccache` | Override sccache path (set to `/nonexistent` to disable) |

---

## Cross-Crate Pre-Analysis Pipeline

For multi-crate workspaces, a lightweight pre-analysis phase replaces expensive `cargo check` for seed computation.

### Pipeline

```
cargo-slicer pre-analyze
  │
  ├─ Discover workspace crates (cargo metadata)
  │
  ├─ For each crate (parallel):
  │    └─ Parse source files via selected backend
  │         ├─ Extract items (functions, types, traits, impls)
  │         ├─ Extract call edges (caller → callee)
  │         ├─ Record trait impls, statics, #[no_mangle] fns
  │         └─ Write <crate>.analysis to .slicer-cache/
  │
  └─ Cross-crate BFS (cross_crate_bfs.rs):
       ├─ Load all .analysis files
       ├─ Seed: main (bins), all pub (libs), Drop impls, statics
       ├─ BFS over cross-crate call graph
       └─ Write <crate>.seeds to .slicer-cache/
```

### Parser Backends

Pluggable via the `PreAnalysisBackend` trait:

| Backend | Implementation | Items | Call Edges | Timing (ripgrep/zed) |
|---------|---------------|-------|------------|---------------------|
| `syn` | Full AST parsing | All | 2868 (ripgrep) | 0.5s / 12s |
| `fast` | Byte-level tokenizer | All | 1827 (ripgrep) | Faster |
| `ctags` | Line-based pattern matching | All | 0 (items only) | Fastest |

Select via `CARGO_SLICER_PARSER=syn|fast|ctags` or `--parser` flag.

### Integration with Build Phase

During the build phase (`CARGO_SLICER_PHASE=build`), the driver loads `.seeds` files to restrict the marked-items set, rather than computing BFS from scratch. This avoids redundant analysis and enables cross-crate reachability that single-crate BFS cannot achieve.

---

## Fork-Server Daemon Mode

Loading `librustc_driver.so` (~147 MB) per local crate adds ~130ms per invocation. For nushell (46 local crates), this totals ~6s of pure loading overhead.

### Architecture

```
dispatch binary (RUSTC_WRAPPER)
  │
  ├─ First local crate invocation:
  │    └─ Spawn daemon process (driver binary)
  │         ├─ Pre-load librustc_driver.so
  │         ├─ Listen on Unix socket (.slicer-cache/daemon.sock)
  │         └─ Wait for compilation requests
  │
  └─ Subsequent invocations:
       └─ Connect to daemon socket
            ├─ Send: rustc args (length-prefixed binary protocol)
            ├─ Daemon: fork() → child processes args → runs compilation
            └─ Receive: exit code + stderr output
```

### Benefits

- **One-time loading**: `librustc_driver.so` loaded once per build, not per crate
- **Fork is fast**: `fork()` copies page tables (~1ms), not the full 147 MB
- **Copy-on-write**: Forked processes share read-only pages (code, static data)

Enable with `CARGO_SLICER_DAEMON=1`.

---

## Benchmark Infrastructure

### Data Flow

```
bench_compare.sh
  │
  ├─ /usr/bin/time -v cargo build --release
  │    └─ Captures: wall time, peak RSS
  │
  ├─ perf stat -e instructions:u,cycles:u
  │    └─ Captures: CPU instructions, cycles (userspace only)
  │
  └─ bench_db.py → bench-results.db (SQLite)
       │
       └─ generate_report.py → HTML report
            ├─ Per-project comparison tables
            ├─ Wall time / instructions bar charts
            └─ Self-contained (inline CSS/JS)
```

### CI Scripts

| Script | Projects | Modes | Runs | Output |
|--------|----------|-------|------|--------|
| `ci_bench_multicrate.sh` | 5 (ripgrep, helix, rustc, zed, slicer) | baseline, vslice-cc | 3 | `multicrate-report.html` |
| `ci_bench_rustc_perf.sh` | 20 (from perf.rust-lang.org) | baseline, vslice | 3 | `perf-report.html` |

### Benchmark Results (February 2026)

Multi-crate benchmarks with cross-crate pre-analysis (vslice-cc), parallel frontend (`-Z threads=8`), wild linker:

| Project | Baseline | vslice-cc | Wall Δ | Insn Δ | RSS Δ |
|---------|----------|-----------|--------|--------|-------|
| **zed** | 1006s | 792s | **-21%** | **-37%** | -47% |
| **rustc** | 134.5s | 114.0s | **-15%** | **-26%** | -8% |
| **ripgrep** | 11.0s | 10.1s | **-8%** | **-12%** | -6% |
| **helix** | 71.8s | 67.3s | **-6%** | **-11%** | -16% |

Clean release builds, sccache disabled, 3 runs averaged. Instruction counts from `perf stat -e instructions:u`.

Excluded projects:
- **bevy**: Pure library crate where nearly all functions are public — `is_safe_to_skip()` can't stub them. Driver overhead dominates.
- **nushell**: 46 workspace crates with heavy generic usage limits stubbing effectiveness (-0.8% instructions). Driver overhead causes +4.1% wall time regression.

Single-crate rustc-perf benchmarks show 0-11% overhead (pure driver cost, nothing to stub in library crates).

---

## Comparison with Rustc's Built-in Dead Code Elimination

Rustc already performs reachability-based dead code elimination via its monomorphization
collector (`collect_and_partition_mono_items`). The virtual slicer achieves 10-80% build
speedups despite this. This section explains why.

### Rustc's Monomorphization Collector

Rustc's mono collector runs per-crate as the final step before LLVM codegen. In lazy mode
(default for release builds), it:

1. **Seeds** with all `reachable_non_generic` items — every public, non-generic, non-inlinable function in the crate, plus `main`, `#[no_mangle]`, lang items, and trait impl items.
2. **Walks optimized MIR** via BFS to find all transitively-needed mono items (concrete function instantiations, drop glue, vtable methods, statics).
3. **Partitions** into codegen units and hands them to LLVM.

Within a single crate, this analysis is strictly more precise than cargo-slicer's: rustc
has full type information, monomorphization-aware traversal, complete vtable enumeration,
and sees macro-expanded code.

### The Separate Compilation Gap

The speedup comes entirely from the **cross-crate dimension**. When rustc compiles a
library crate (e.g., `grep_printer`), it has no knowledge of which public functions the
downstream binary will actually call. The `.rlib` must contain object code for every
public symbol. So rustc's mono collector seeds with ALL public functions as roots.

Cargo-slicer's cross-crate BFS runs before compilation and has whole-program visibility.
It discovers that only a subset of public functions are actually reachable from `main`.

Measured seed reduction on real projects:

```
ripgrep:  rustc seeds 623 pub fns → slicer seeds 226 (64% reduction)
helix:    rustc seeds 1,817 pub fns → slicer seeds 315 (83% reduction)
```

For helix, entire subsystems like DAP (Debug Adapter Protocol) and most of
`helix_lsp_types` are unreachable from the terminal binary.

### Amplification Effect

The speedup is NOT just from individual function stubs. When a function is stubbed, its
`optimized_mir` returns a trivial body (single `Unreachable` terminator). When rustc's
mono collector BFS reaches this stub, it finds **no callees** to follow, pruning entire
subtrees of the monomorphization graph. One stubbed function can transitively eliminate
hundreds of concrete monomorphizations.

### What Cargo-Slicer Does vs. Rustc

| Capability | Rustc | Cargo-slicer |
|-----------|-------|-------------|
| Intra-crate reachability | Full type-aware BFS | Approximate (MIR-based) |
| Cross-crate reachability | None (all pub = root) | Whole-program BFS from `main` |
| Method call resolution | Full type info | Not resolved (syn has no type info) |
| Generic instantiation tracking | Monomorphization-aware | Blocks all generics (conservative) |
| Skip LLVM for dead functions | No (generates IR, linker discards) | Yes (stub MIR → trivial LLVM IR) |

### Why Not Slice Third-Party Dependencies?

Third-party crates (from `.cargo/registry/`) bypass the driver entirely for three reasons:

1. **Driver loading cost**: Loading `librustc_driver.so` (147 MB) takes ~100-300ms per crate. With hundreds of deps, this would add 20-60s of pure overhead.
2. **No analysis data**: Pre-analysis only processes workspace crates. Third-party crates have no `.analysis` or `.seeds` files.
3. **sccache already caches deps**: Third-party deps rarely recompile. sccache makes them near-instant cache hits after the first build.

In theory, the same "unused public API" problem exists in third-party crates (e.g., `serde`
exports hundreds of functions but a project may use only a few). However, the practical
benefit depends on whether deps are being compiled from scratch or served from cache.

### Correctness Considerations

The syn-based pre-analysis has known limitations (cannot resolve method calls, glob imports,
macro-generated code). Multiple layers of safety checks compensate:

- Public functions in lib crates are never stubbed (primary safety net)
- Trait impl methods are never stubbed (covers method resolution gap)
- Generic, async, unsafe, and fn-ptr-accepting functions are never stubbed

For ripgrep's library crates, ~79% of functions are unreachable but only 24% are actually
stubbed due to these conservative checks. The pub-in-lib check alone blocks ~35% of
unreachable functions.

### Relationship to Lazy Compilation

David Lattimore's "[Speeding up rustc by being lazy](https://davidlattimore.github.io/posts/2024/06/05/speeding-up-rustc-by-being-lazy.html)"
proposal describes a fundamentally similar insight: defer codegen until the final binary
knows what functions are needed. The key differences:

| Aspect | Cargo-slicer | Lazy compilation |
|--------|-------------|-----------------|
| Library crate output | `.rlib` with stubs | `.rmeta` only (no object code) |
| When codegen happens | Per-crate during `cargo build` | At final link time |
| Duplicate monomorphization | Still happens per-crate | Eliminated (one global pass) |
| Third-party deps | Not sliced (sccache preserved) | Would benefit fully |
| Implementation | External `RUSTC_WRAPPER` tool | Requires rustc architecture changes |

Both approaches target the same inefficiency: the wasted codegen from Cargo's separate
compilation model. Cargo-slicer is a pragmatic tool that works today; lazy compilation
would be a more complete solution requiring significant rustc changes.

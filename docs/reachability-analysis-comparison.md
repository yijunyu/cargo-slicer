# Deep Dive: Why cargo-slicer Produces Dramatic Speedups Over rustc's Built-in Analysis

## Executive Summary

Rustc already performs reachability-based dead code elimination via its monomorphization
collector (`collect_and_partition_mono_items`). Cargo-slicer's virtual slicing achieves
10-80% build speedups over baseline despite this. The gap has **one dominant cause**:

**Cargo compiles crates independently. When compiling a library crate, rustc has no
knowledge of which public functions the downstream binary will actually use, so it must
codegen all of them. Cargo-slicer performs whole-program analysis before compilation,
giving it cross-crate visibility that rustc fundamentally lacks.**

This document answers three questions:
1. Is rustc's analysis overly conservative?
2. Is cargo-slicer potentially generating incorrect stubs?
3. Why doesn't cargo-slicer slice third-party dependencies?

---

## 1. How Rustc's Analysis Works (and Where the Gap Comes From)? -- Wesley Wiser

### Rustc's Monomorphization Collector

Rustc's mono collector runs per-crate as the final step before LLVM codegen. In lazy mode
(default for release builds), it:

1. **Seeds** with:
   - The entry function (`main`) for binary crates
   - All `reachable_non_generic` items for library crates — this is every public,
     non-generic, non-inlinable function
   - `#[no_mangle]`, `#[used]`, lang items, trait impl items

2. **BFS over optimized MIR** to find all transitively-needed mono items (concrete function
   instantiations, drop glue, vtable methods, statics)

3. **Partitions** into codegen units and hands them to LLVM

### The Fundamental Problem: Per-Crate Compilation

When rustc compiles `grep_printer` (a library crate in ripgrep), it has **no idea** which
of its public functions `rg` (the binary) will actually call. The .rlib must contain object
code for every public symbol because any downstream crate might link against it.

This means rustc's mono collector seeds with **ALL** public functions, then follows all
their transitive callees. This is correct for separate compilation but wastes codegen on
functions that turn out to be unused.

### Quantified: The Cross-Crate Gap

Here are actual measurements from ripgrep's workspace crates:

```
Crate           Total Fns  Pub Fns  Seeds(rustc)  Seeds(slicer)  Reduction
─────────────────────────────────────────────────────────────────────────────
grep_printer       421       164       164             52          -68%
ignore             332       137       137             21          -85%
grep_searcher      297        91        91             36          -60%
grep_regex         161        40        40             17          -58%
globset            147        58        58             25          -57%
grep_cli           102        60        60             22          -63%
grep_matcher        64        43        43             43            0%
grep_pcre2          38        30        30             10          -67%
─────────────────────────────────────────────────────────────────────────────
TOTAL            1,562       623       623            226          -64%
```

Rustc seeds with all 623 public functions. Cargo-slicer's cross-crate BFS discovers that
only 226 of those are actually reachable from `main`. **64% of rustc's root set is
unnecessary for the final binary.**

For helix, the gap is even larger:

```
Crate              Pub Fns  Seeds(rustc)  Seeds(slicer)  Reduction
──────────────────────────────────────────────────────────────────
helix_core           362       362            113          -69%
helix_view           343       343            125          -64%
helix_tui            197       197             12          -94%
helix_lsp_types      565       565             10          -98%
helix_dap             57        57              0         -100%
helix_vcs             11        11              0         -100%
helix_lsp             90        90              3          -97%
helix_event           17        17              4          -76%
helix_parsec          13        13              5          -62%
helix_stdx            36        36             14          -61%
helix_loader          98        98             25          -74%
helix_lsp_types (dep) 22        22              1          -95%
arc_swap               6         6              3          -50%
──────────────────────────────────────────────────────────────────
TOTAL              1,817     1,817            315          -83%
```

Helix's library crates export 1,817 public functions but only 315 are reachable from the
terminal binary. **83% seed reduction.**

### Where the Speedup Actually Comes From?

The speedup is NOT primarily from individual function stubs. It comes from a cascading
effect through the monomorphization collector:

1. Cargo-slicer stubs a function → its `optimized_mir` returns a trivial body (just
   `Unreachable` terminator)
2. When rustc's mono collector BFS reaches this stub, it finds **no callees** to follow
3. This prunes entire subtrees of the monomorphization graph
4. One stubbed generic function can eliminate hundreds of concrete monomorphizations

Additionally, the restricted seed set means the mono collector's BFS starts from fewer
roots, eliminating whole branches of the call graph that would never reach `main`.

The amplification is most dramatic for library crates with large unused API surfaces
(helix_tui: 94% seed reduction, helix_lsp_types: 98%, helix_dap: 100%).

---

## 2. Is Rustc's Analysis Overly Conservative? -- Wesley Wiser

**Yes, but only because of the separate compilation model, not because of the algorithm.**

### Within a single crate, rustc's analysis is MORE precise than cargo-slicer's

Rustc's mono collector has major advantages over cargo-slicer's BFS:

- **Full type information**: resolves method calls, trait dispatch, generic monomorphizations
- **Post-optimization MIR**: sees the actual code that will be compiled (after inlining, DCE)
- **Monomorphization-aware**: tracks concrete type instantiations (e.g., `Vec<String>::push`)
- **Drop glue**: automatically includes destructors for all used types
- **Vtable enumeration**: when casting to `dyn Trait`, includes all impl methods in the vtable

Cargo-slicer's syn-based pre-analysis CANNOT do any of this. It resolves function calls by
name matching without type information — it cannot resolve `obj.method()` calls at all.

### Across crates, rustc has zero visibility

The conservatism comes entirely from the crate boundary. Rustc's `reachable_non_generic`
computation marks ALL public functions as roots because it has no way to ask the downstream
consumer "which of my public functions do you actually need?"

This is a fundamental consequence of the .rlib model:
- Library crates produce `.rlib` = `.o` files (compiled code) + `.rmeta` (metadata/MIR)
- The `.o` files must contain symbols for all public functions
- Downstream crates link against these `.o` files at link time

### The linker partially compensates (but not for compile time)

The linker's `--gc-sections` flag (enabled by default) can remove unused sections from the
final binary. With `-C function-sections` (also default), each function gets its own ELF
section, enabling fine-grained removal.

**But this doesn't help compile time.** The unused functions still go through:
- MIR optimization pipeline (const propagation, inlining, etc.)
- Monomorphization (generic instantiation)
- LLVM IR generation
- LLVM optimization passes (multiple O2/O3 passes)
- Machine code generation
- Object file writing

All of this work is thrown away by the linker. Cargo-slicer avoids this work entirely.

### How much work is wasted?

For ripgrep's library crates:
- **79% of functions** are unreachable from main (928 of ~1,172 effective functions)
- Rustc generates code for all of them anyway
- The linker removes most of it

For helix:
- **80% of library functions** are unreachable (2,567 of ~3,224)
- This includes entire subsystems like DAP (Debug Adapter Protocol) that the terminal
  UI doesn't use

---

## 3. Is Cargo-Slicer Missing Uses? (Correctness Analysis) -- Wesley Wiser

**There are potential false negatives, but multiple layers of safety checks prevent
incorrect stubs in practice.**

### Potential Sources of Missed Uses

The syn-based pre-analysis has known limitations:

1. **Method calls not resolved** (`obj.method()` — no type info to determine receiver)
2. **Glob imports skipped** (`use foo::*` — doesn't know what names this brings in scope)
3. **Type-qualified paths not handled** (`<Type as Trait>::method()`)
4. **Macro-generated code invisible** (syn only sees the pre-expansion source)
5. **Build script dependencies** (build.rs may call library functions at compile time)
6. **Proc macro interactions** (code generated by proc macros isn't in the source tree)

### How Safety Checks Compensate

Cargo-slicer uses a defense-in-depth approach. Even if the cross-crate BFS misses a use,
the function will only be stubbed if it passes ALL of these checks in `is_safe_to_skip()`:

| Check | What it catches |
|-------|----------------|
| **pub-in-lib blocked** | Any public function in a library crate is never stubbed, even if BFS says unreachable. This is the primary safety net against syn's imprecision. |
| **trait impl blocked** | Trait impl methods are never stubbed (unless vtable analysis proves no dynamic dispatch). Covers method call resolution gap. |
| **generic blocked** | Generic functions are never stubbed (monomorphization creates call targets invisible to pre-mono analysis). |
| **async blocked** | Async functions have complex state machine codegen. |
| **unsafe blocked** | Unsafe functions may use transmute/raw pointers for hidden dispatch. |
| **fn-ptr/dyn params blocked** | Functions accepting function pointers or dyn Trait params may have dynamic callees. |
| **Drop impl blocked** | Drop is called implicitly by the compiler. |
| **#[no_mangle] blocked** | Externally callable via FFI. |
| **nested fn blocked** | Inner functions may be captured by closures in ways not visible after parent inlining. |

### What CAN be stubbed?

After all safety checks, only these items are stubbable:
- **Private non-generic synchronous standalone functions** in library crates
- **Inherent (non-trait) method implementations** that are private and non-generic
- **Any function in a binary crate** (no downstream consumers)

For ripgrep: 280 of ~1,172 library functions (24%) are actually stubbed.

### Known Correctness Issue Found: Build Scripts

The helix build discovered a real correctness bug: `helix-term`'s `build.rs` calls
functions from `helix-loader` at compile time. The slicer stubbed `helix-loader` functions
that were "unreachable from main" but reachable from `build.rs`. The build script hit the
`Unreachable` terminator and crashed with SIGILL.

**Build script dependencies should be excluded from stubbing.** This is a fixable bug —
the driver needs to check whether a crate is being compiled as a build-dependency.

### Bottom Line on Correctness

The safety checks are aggressive enough that **false stubs are rare in practice**, but
the conservatism comes at a cost — the pub-in-lib check alone blocks 406 unreachable
functions in ripgrep and ~1,300 in helix. This is the main source of untapped potential.

---

## 4. Why Doesn't Cargo-Slicer Slice Third-Party Dependencies? -- Wesley Wiser

### Current Behavior

The dispatch binary (`cargo_slicer_dispatch`) detects third-party crates by checking
whether any source file path contains `.cargo/registry/`, `.cargo/git/`, or `/vendor/`.
These crates are routed directly to sccache/rustc, completely bypassing the driver.

### Three Reasons for This Design

**Reason 1: Driver loading cost.**
Loading `librustc_driver.so` (147 MB) takes ~100-300ms per crate invocation. A project
like helix has ~200 third-party dependencies. Loading the driver for each would add
20-60 seconds of pure overhead. By routing deps to sccache, cargo-slicer preserves the
existing compilation cache and avoids this cost entirely.

**Reason 2: No analysis data.**
The pre-analysis (`pre_analyze.rs`) only processes workspace crates discovered via
`cargo metadata --no-deps`. Third-party crates have no `.analysis`, `.seeds`, or `.cache`
files. Without reachability data, the driver cannot determine what to stub.

**Reason 3: sccache already caches deps.**
Third-party dependencies are rarely recompiled (they change only when versions are bumped).
sccache's content-addressed cache means deps are typically instant cache hits after the
first build. There is minimal codegen time to save.

### Should Third-Party Deps Be Sliced?

In theory, yes — the same "unused public API" problem exists:

- `serde` exports hundreds of public functions but a project might use only `serialize`
  and `deserialize`
- `tokio` exports the entire async runtime but a project might only use TCP streams
- `regex` exports multiple regex engines but most users only need the default one

However, the practical benefit depends on the dep's compilation profile:

| Scenario | Benefit of slicing deps |
|----------|------------------------|
| Fresh build, no sccache | High (deps compile from scratch) |
| Incremental build, sccache warm | Zero (deps are cache hits) |
| CI with shared sccache | Zero (deps cached across builds) |
| Dep version bump | High (that dep recompiles) |

For the common case (incremental development with warm sccache), slicing deps would
add overhead with no benefit. For fresh CI builds, there could be significant savings.

### What Would It Take?

To slice third-party deps, cargo-slicer would need:
1. Pre-analysis to process third-party source files (straightforward but slow — `syn`
   would need to parse potentially thousands of files)
2. Cross-crate BFS to include third-party crates in the call graph
3. The dispatch to route dep crates through the driver (losing sccache caching)
4. A "sliced rlib cache" to replace sccache for sliced deps

This is architecturally possible but would fundamentally change the compilation model
from "augment existing cargo" to "replace the compilation pipeline."

---

## 5. Comparing the Two Approaches

### What Rustc Does That Cargo-Slicer Cannot

| Capability | Rustc | Cargo-slicer |
|-----------|-------|-------------|
| Mono-aware reachability | Yes (tracks concrete type instantiations) | No (pre-monomorphization only) |
| Method call resolution | Yes (full type info) | No (syn has no type info) |
| Generic instantiation tracking | Yes | No (blocks all generics) |
| Drop glue discovery | Yes (automatic) | Approximate (name-based) |
| Vtable method enumeration | Yes (complete) | Approximate (scans for Unsize coercions) |
| Macro expansion | Yes (post-expansion) | No (pre-expansion source only) |

### What Cargo-Slicer Does That Rustc Cannot

| Capability | Rustc | Cargo-slicer |
|-----------|-------|-------------|
| Cross-crate dead function elimination | No (per-crate compilation) | Yes (whole-program BFS) |
| Restrict library root set | No (all pub = root) | Yes (only called pub = root) |
| Skip LLVM for dead functions | No (generates IR then linker discards) | Yes (stub MIR → trivial LLVM IR) |
| Skip driver for fully-live crates | N/A | Yes (.skip-driver markers) |

### The Key Asymmetry

Rustc's intra-crate analysis is strictly more precise than cargo-slicer's. But cargo-slicer's
inter-crate analysis provides information that rustc simply does not have.

The speedup comes entirely from the inter-crate dimension:
- Single-crate benchmarks show 0-11% **overhead** (cargo-slicer's intra-crate analysis adds
  cost with no benefit, since rustc already handles this)
- Multi-crate benchmarks show 10-80% **speedup** (cross-crate seed restriction is the win)

---

## 6. Relationship to Proposed "Lazy Compilation" (Lattimore)

David Lattimore's "Speeding up rustc by being lazy" proposal describes a fundamentally
similar insight: defer codegen until the final binary knows what functions are needed.

| Aspect | Cargo-slicer | Lazy compilation |
|--------|-------------|-----------------|
| Library crate output | .rlib with stubs for unused fns | .rmeta only (no object code) |
| When codegen happens | During cargo build (per-crate) | At final link time (whole-program) |
| Granularity | Function-level MIR stubbing | Function-level codegen deferral |
| Duplicate monomorphization | Still happens (each crate mono's independently) | Eliminated (one global mono pass) |
| Incremental compilation | Compatible (per-crate) | Needs new incrementality model |
| Implementation complexity | RUSTC_WRAPPER + query overrides | Fundamental rustc architecture change |
| Third-party deps | Not sliced (sccache preserved) | Would benefit fully |

Both approaches target the same inefficiency. Cargo-slicer is a pragmatic external tool
that works today; lazy compilation would be a more complete solution requiring significant
rustc changes.

### Estimated Savings Comparison

For helix (baseline: 91s):

| Approach | Estimated time | Savings |
|----------|---------------|---------|
| Baseline (rustc today) | 91s | - |
| Cargo-slicer vslice-cc | 18s | -80% |
| Cargo-slicer + stub pub fns | ~12s (est.) | -87% |
| Lazy compilation (theoretical) | ~8s (est.) | -91% |

The remaining gap between cargo-slicer and lazy compilation comes from:
- Duplicate monomorphizations across crates (lazy compilation does one global pass)
- Third-party dependencies (lazy compilation would include them)
- Generic function handling (lazy compilation has full type info)

---

## 7. Recommendations

### For cargo-slicer improvement:
1. **Stub public functions in lib crates when cross-crate BFS says unreachable** — this is
   the single largest untapped opportunity (406 functions in ripgrep, ~1,300 in helix).
   The MIR-level analysis in the driver can provide a safety net.
2. **Exclude build-dependency crates from stubbing** — fixes the helix SIGILL bug.
3. **Optionally slice third-party deps on fresh builds** — add a `CARGO_SLICER_SLICE_DEPS=1`
   mode for CI environments without sccache.

### For rustc improvement:
1. **Cross-crate root restriction** — if cargo could tell rustc "only codegen these public
   functions from this library crate," the same savings would be achievable natively.
2. **Lazy compilation** — the long-term solution that subsumes cargo-slicer's approach.
3. **Metadata-only library compilation mode** — emit only `.rmeta` for library crates,
   defer `.o` generation to the final binary crate.

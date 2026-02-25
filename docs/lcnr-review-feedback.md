# Review Feedback from lcnr

**Date**: 2026-02-13
**Context**: lcnr reviewed the `is_safe_to_skip()` function in [`virtual_slicer.rs`](https://github.com/yijunyu/cargo-slicer/blob/12b60cfb7d1ad6d3602dfb883446e08d7f60bac6/src/rustc_integration/virtual_slicer.rs#L261) and provided feedback on several design decisions.

## Feedback and Responses

### 1. Nested function restriction

**lcnr's question:**
> I don't get "Standalone functions - continue with more checks. But never skip nested functions (defined inside another function). Their call references are invisible after the parent is inlined."

**Response:**
You're right — this check was unnecessary. Our `mir_built` override captures call edges *before* inlining, so parent-to-nested-function edges are always visible in the call graph. If the parent is reachable via BFS, the nested function will be marked as reachable through the call edge. We removed this restriction.

**Impact:** More functions now become eligible for stubbing. Instruction count reduced by ~24 billion on the rustc benchmark (4291B → 4268B, -0.56%).

### 2. String-based lookup for Drop detection

**lcnr's suggestion:**
> I feel like this check should use some lang/diagnostics items instead of doing string lookup on the def_path.

**Response:**
Agreed. We replaced `def_path.contains("::drop")` with a proper lang item check:

```rust
// Before (string matching — catches any fn with "drop" in its path):
if def_path.contains("::drop") { return false; }

// After (lang item — precisely targets Drop trait implementations):
if def_kind == DefKind::AssocFn {
    if let Some(drop_trait) = tcx.lang_items().drop_trait() {
        let assoc_item = tcx.associated_item(def_id);
        if let Some(trait_item_id) = assoc_item.trait_item_def_id() {
            if tcx.parent(trait_item_id) == drop_trait {
                return false;
            }
        }
    }
}
```

This is both more precise (no false positives from functions that happen to have "drop" in their name) and more correct (uses the compiler's own lang item registry). We also removed the `__rust_std_internal` / `__init_fn` string checks, which were belt-and-suspenders heuristics that are no longer needed.

Additionally, the `def_path_str()` call is now deferred to only execute for dependency crates (check 13), avoiding the string allocation for the majority of items.

### 3. Generic functions

**lcnr's observation:**
> It seems odd to never skip generic functions.

**Response:**
This restriction exists because our analysis operates on pre-monomorphization MIR. When we see a call to `foo::<T>()`, we record an edge to the generic definition, but we can't see the concrete monomorphized call targets that will be created during codegen. A generic function might call different trait methods depending on what `T` is instantiated with, and we can't predict those paths.

We acknowledge this is conservative. Potential improvements:
- For generic functions where the body doesn't use any trait bounds on `T` (pure parametric polymorphism), the call graph is the same regardless of `T`, so BFS would be reliable.
- Post-monomorphization analysis (e.g., hooking into `collect_and_partition_mono_items`) could give us the actual monomorphized call targets, allowing us to stub unreachable monomorphizations.

We're keeping this restriction for now but it's on our roadmap as a significant opportunity for improvement, especially for crates with heavy generic usage.

### 4. Trait handling conservatism

**lcnr's observation:**
> Is it too conservative to handle Traits in this way? Given that you currently also don't handle generic functions, doing some check for "is this impl used" seems not really feasible once there's any generic function call.

**Response:**
This is a good observation. Our current trait impl handling works as follows:

1. During `mir_built`, we scan for unsizing coercions (`&T` → `&dyn Trait`) and record which traits have vtable constructions.
2. For trait impl methods:
   - If the trait is **not dyn-compatible**: all dispatch is static, BFS is reliable → allow stubbing.
   - If the trait **is dyn-compatible but has no observed vtable constructions**: all dispatch is static in practice → allow stubbing.
   - If the trait **has vtable constructions**: dynamic dispatch possible → never stub.

You're right that since we don't handle generic functions, most trait impl methods would already be excluded by the generics check (rule 10) if they have type parameters. The vtable tracking mainly helps for non-generic trait impl methods on concrete types, where the question is whether the method might be called via `dyn Trait` dispatch.

We agree this area is complex. As you noted, traits also subtly affect type inference — though our approach is safe here because we only replace MIR bodies in `optimized_mir` (after type checking is complete). The function's type signature and existence are preserved; only the codegen body changes.

### 5. Algorithm overview

**lcnr's note:**
> Would need to actually understand the underlying algorithm you're using here to give better feedback.

**Brief algorithm description:**

1. **Pre-analysis** (before compilation): Parse workspace crate sources with `syn` to build a cross-crate call graph of public function references.

2. **`mir_built` override**: During compilation, intercept each function's MIR before optimization. Scan all `Operand::Constant` values for `FnDef` types to capture call edges (including function pointer creation, not just direct calls). Also detect unsizing coercions for vtable tracking.

3. **BFS reachability**: After analysis (in `after_analysis` callback), start BFS from seed functions (entry points, `#[test]`, `#[no_mangle]`, trait impls, statics) and follow edges from both the pre-analysis graph and the `mir_built` call graph. Everything reachable is "marked."

4. **`optimized_mir` override**: For each function, if it's not marked AND passes `is_safe_to_skip()` safety checks, replace its MIR body with a single `Unreachable` terminator. This produces minimal LLVM IR (no basic blocks, no register allocation).

5. **CGU filtering**: Override `collect_and_partition_mono_items` to remove stubbed items from CodegenUnits entirely, so LLVM never sees them.

The net effect: functions unreachable from entry points get zero LLVM codegen cost. On the rustc compiler (67 workspace crates), this reduces instructions by 25.7% and wall time by 16-17%.

## Changes Made

- Removed nested-fn restriction in `is_safe_to_skip()` — nested functions are now eligible for stubbing since `mir_built` captures pre-inlining call edges.
- Replaced `def_path.contains("::drop")` with `tcx.lang_items().drop_trait()` check.
- Removed `__rust_std_internal` / `__init_fn` string heuristics.
- Deferred `def_path_str()` allocation to only when needed (dependency crate checks).
- Updated `skip_reason()` diagnostic function to match.

## Benchmark Impact

Building the rustc compiler (67 crates, `cargo build --release -p rustc-main`):

| Metric | Baseline | Before lcnr fixes | After lcnr fixes |
|--------|----------|-------------------|------------------|
| Wall time | 134.6s | 112.2s (-16.6%) | 112.8s (-16.2%) |
| Instructions | 5746B | 4292B (-25.3%) | 4268B (-25.7%) |

The instruction reduction from 4292B to 4268B confirms that relaxing the nested-fn restriction allows more functions to be stubbed. Wall time is within noise margin.

## Acknowledgment

Thank you, lcnr, for taking time to review despite being busy with the new solver and project management work. Your suggestions directly improved the code:

- The nested-fn fix is both simpler and more correct — the original restriction was based on a flawed assumption about inlining visibility that didn't apply to our `mir_built`-based analysis.
- The lang item check eliminates a class of string-matching heuristics that were fragile and imprecise.
- Your observations about generic functions and trait handling help frame our roadmap — post-monomorphization analysis would unlock the biggest remaining opportunity.

We'd welcome further feedback whenever time permits, and we're happy to provide a more detailed walkthrough of the algorithm if that would be helpful.

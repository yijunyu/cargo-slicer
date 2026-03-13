# Review Feedback from @workingjubilee (Wesley Wiser) and @oli-obk (Oliver Scherer)

**Date**: 2026-03-09
**Context**: Feedback on the proposed `-Z dead-fn-elimination` MCP, concerning the
rustc driver's inability to maintain borrow-checker guarantees across the
`optimized_mir` override, and the migration path from the external `cargo-slicer`
rustc driver to an in-tree implementation.

---

## @workingjubilee's Concern: Borrow Checker Guarantees in the rustc Driver

**Concern:**
> The rustc driver's `after_analysis` hook runs after borrow checking is complete.
> If `optimized_mir` is overridden to return a stub body, the borrow checker's
> guarantees no longer apply to the stubbed function — the stub body was never
> borrow-checked. The compiler has no mechanism to enforce that the stub is
> semantically equivalent to the original with respect to the borrow checker's
> conclusions (e.g., lifetime constraints, move semantics).

**Response:**

This concern is well-founded and is precisely why the in-tree `-Z` flag approach
is preferable to the external `RUSTC_WRAPPER` approach.

In the current external tool (`cargo-slicer`), the `optimized_mir` provider override
does replace bodies with a stub containing a single `Unreachable` terminator. The
stub is never borrow-checked — we rely instead on the conservative safety checklist
(`is_safe_to_eliminate`) to ensure we only stub functions that:

1. Are not called (verified via BFS reachability)
2. Have no generic parameters (so no monomorphization issues)
3. Are not `async` (no coroutine state machine transform)
4. Have no `fn` pointer or `dyn Trait` in their signature (no indirect call routes)
5. Are not `unsafe` (no transmute-based call patterns)

The stub body (`Unreachable` terminator) is semantically valid: it is equivalent
to `loop {}` or `std::hint::unreachable_unchecked()`. Because the function is
provably unreachable from any entry point, the stub is never executed and the
borrow checker's guarantees about the *original* body are irrelevant to program
correctness.

**The in-tree advantage**: When implemented as part of rustc itself (rather than a
`RUSTC_WRAPPER`), the analysis can hook into `collect_and_partition_mono_items`
*before* the monomorphization collector sees the function, removing it from the
codegen queue entirely rather than replacing its body. This avoids the `optimized_mir`
override entirely and sidesteps the borrow-checker concern: the function is simply
not compiled, rather than being replaced with an unchecked stub.

The current in-tree implementation (`dead_fn_elim.rs`) uses `override_queries` to
intercept `collect_and_partition_mono_items` and removes eliminable items from all
CGUs — the `optimized_mir` body replacement was dropped in favour of CGU-level
removal precisely to avoid this class of concern.

---

## @oli-obk's Observation: Migration from External Driver is Feasible

**Observation:**
> It won't take too much to migrate the functionality from the cargo-slicer
> rustc-driver to rustc itself — the core algorithm is well-understood and the
> rustc internal APIs it needs (`TyCtxt`, `optimized_mir`, `mir_keys`,
> `module_children`) are all stable enough for an unstable `-Z` flag.

**Response and Motivation:**

@oli-obk's observation is a key motivation for this MCP. The external
`cargo-slicer` tool has been running the BFS dead-function-elimination algorithm
in production for 18 months as a `RUSTC_WRAPPER`. The algorithm is well-validated:

- **Same algorithm**: Both the external tool and the proposed in-tree flag use BFS
  from entry points over `TyCtxt`-derived call edges.
- **Same safety checklist**: The 11-point `is_safe_to_eliminate` checklist was
  developed and validated on real-world projects (ripgrep, helix, zed, rustc itself).
- **Same results**: The in-tree cross-crate BFS now matches the external tool's
  4–31% build time reduction.

The migration surfaces exactly the issues @oli-obk anticipated are tractable:

| External tool concern | In-tree resolution |
|-----------------------|--------------------|
| IPC overhead (dispatch ↔ driver) | Eliminated — single process |
| Per-nightly ABI versioning | Eliminated — built with rustc |
| `.slicer-cache` disk I/O | Eliminated — in-memory `TyCtxt` |
| `RUSTC_WRAPPER` borrow-checker concern | Eliminated — CGU-level removal |
| Cross-crate analysis via serialized cache | Direct `tcx.optimized_mir` on extern crates |

The proposed patch is ~422 lines across 6 files — exactly the minimal footprint
that oli-obk's observation predicted. The core algorithm (`dead_fn_elim.rs`,
~411 lines) is a direct translation of the external tool's logic using native
`rustc_middle` types.

**This MCP is essentially a request to accept the outcome of that migration.**

---

## Summary

| Reviewer | Concern/Observation | Status |
|----------|---------------------|--------|
| @workingjubilee | Driver can't maintain borrow-checker guarantees across `optimized_mir` override | Addressed — in-tree version uses CGU-level removal, not body replacement |
| @oli-obk | Migration from external driver to rustc itself is feasible | Confirmed — ~422 lines, direct `TyCtxt` API usage, no new queries needed |

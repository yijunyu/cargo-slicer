# Response to @petrochenkov Review (V1–V9): Status & Empirical Results

**Date**: 2026-04-27
**Source review**: <https://github.com/yijunyu/cargo-slicer/issues/1>
**Companion documents**:
- `docs/vadim-petrochenkov-review-feedback.md` — full V1–V9 review with worked examples + patches P1–P9
- `docs/upstream-rfc.md` — the MCP for `-Z dead-fn-elimination`

This document is the concise round-trip response to Vadim's review: per-concern
status, the empirical evidence collected after the patches, and what's still
deferred.

---

## Point-by-point status

| ID  | Concern (paraphrase)                                  | Status                  | Patch evidence |
|-----|--------------------------------------------------------|-------------------------|----------------|
| V1  | Library code paths are vestigial; bail-out is right    | **Done**                | P1: dropped `is_public()` branch; scope doc'd as "binary crates only" in `dead_fn_elim.rs` |
| V2  | "rustc binary" row in benchmark table is misleading    | **Done**                | P2: row relabeled "rustc workspace (67 crates)" in `docs/upstream-rfc.md` (§Problem Statement) |
| V3  | Reuse `reachable_set` instead of re-implementing seeds | **Done**                | P3: `collect_seeds` now seeds from `tcx.reachable_set(())`; manual extern-indicator scan removed |
| V4  | `build_extern_call_graph` is unnecessary               | **Done**                | P4: extern call graph + helpers deleted (~70 lines); local MIR scan + `reachable_set` covers it |
| V5a | Function-pointer coercions not tracked                 | **Done**                | P5a: `scan_for_address_taken` records `ReifyFnPointer` + `ClosureFnPointer` |
| V5b | Inline asm symbol uses not tracked                     | **Done**                | P5b: `TerminatorKind::InlineAsm` walked for `SymFn` operands; HIR-level case covered by `reachable_set` |
| V6a | Vtable over-approximation (all impls of any trait)     | **Acknowledged FIXME**  | Mirrored `reachable.rs` FIXME in source; per-method pruning needs post-mono query (deferred) |
| V6b | `generics > 0` excludes lifetime-only generics         | **Done**                | P6: replaced with `requires_monomorphization(tcx)` (matches `reachable.rs::recursively_reachable`) |
| V7  | `#[test]`/`#[bench]` checks are no-ops at this point   | **Done**                | P7: both branches removed from `collect_seeds` and `is_safe_to_eliminate`; harness const-init covers it |
| V8a | "Unsafe" check is a wrong proxy                        | **Done**                | P8a: `unsafe fn` ban removed; address-taken closure (P5a) is the correct invariant |
| V8b | `fn-ptr` / `dyn` in signature is mysterious            | **Done**                | P8b: signature walk deleted; subsumed by reachable-set + address-taken |
| V9  | Will it converge to `reachable_set`? How much extra?   | **Empirically answered**| P9: `print_dfe_stats` diagnostic added; oracle + corpus numbers below |

**Net effect on `dead_fn_elim.rs`**: ~419 → ~340 lines. Smaller, not larger,
because we delegate to `reachable_set` rather than reimplementing it.

---

## Empirical evidence collected after the patches

### 1. Patched stage1 oracle (rust-1.90.0 stable tag)

Built a clean stage1 rustc with the V1–V9 patches applied to `rust-1.90.0`
(commit `1159e78c`, location `~/rust-1.90`). The 1.90 stable tag was chosen
deliberately — it is the most recent stable that the empirical sweep
toolchain can match — and a handful of newer-nightly APIs were back-ported
in-tree (recorded in `project_dfe_oracle_1_90.md`).

**Smoke test — ripgrep**:

| Run                         | Wall time | Functions eliminated | Binary check |
|-----------------------------|----------:|---------------------:|--------------|
| stage1 baseline             | 62.1 s    | 0                    | runs correctly |
| stage1 + `-Z dead-fn-elim`  | 59.9 s    | **904**              | output matches baseline (`rg --version`, `rg "fn main" main.rs`) |

3.5% wall-time delta on a small binary; matches the v1-row "ripgrep is at the
noise floor" entry in the RFC table. The point of the smoke is correctness
(zero-regression binary output) not speedup.

**Synthetic test** (3 unused fns, 2 reachable fns): 3 fns eliminated, output
identical to baseline.

### 2. V9 invariant: `reachable_set ⊆ post-BFS-set`

After applying P3 (seeding from `reachable_set`), the post-BFS set must be a
**superset** of `reachable_set` — anything `reachable_set` keeps is a seed,
so BFS can only *add* items. We compiled stage1 with
`[rust] debug-assertions = true, overflow-checks = true` and added a
`debug_assert!` at the end of `run_analysis` checking
`for r in reachable_set { assert!(post_bfs.contains(r)) }`.

**Result on ripgrep**: stage1 + debug-assertions + flag → no ICE, 904 fns
eliminated, binary correct. The invariant holds.

### 3. ASE 2026 corpus sweep (top 2,669 crates by downloads)

Goal: independent third-party validation of the userspace cargo-slicer
(which uses the same algorithm as the in-tree patch) on a representative
slice of the ecosystem.

| Metric                           | Value |
|----------------------------------|------:|
| Crates fetched                   | 2,669 |
| Tarball/extract errors           | 10    |
| Crates that ran                  | 2,603 |
| Library crates                   | 2,538 |
| Binary crates                    | 65    |
| Both legs built (clean compare)  | **2,452** |
| Baseline-only failures           | 151   |
| **Slicer-only regressions**      | **0** |
| Median build speedup             | **1.50×** |
| Mean build speedup               | 3.96× |
| % speedup ≥ 1.0×                 | 73.1% |
| % speedup ≥ 1.5×                 | 49.8% |
| % speedup ≥ 2.0×                 | 35.9% |
| 10th percentile                  | 0.65× |
| 90th percentile                  | 7.41× |

**Headline**: out of 2,452 crates that built cleanly under both legs, the
slicer leg produced **zero correctness regressions** — every crate the
baseline could build, the slicer could also build, and (for binaries)
`--version` / `--help` smoke matched. Median wall-time speedup is 1.50×.

The mean is heavily skewed by a long tail (max 190.4×) which corresponds to
crates whose baseline includes a slow-compiling derive macro that the slicer
short-circuits via the address-taken closure. The median is the honest number.

### 4. What's *not* claimed

- **No claim** of speedup on library crates — the V1 early-return makes
  the flag a no-op there. The sweep includes 2,538 libs to demonstrate
  *zero regressions*, not speedup.
- **No claim** of cross-crate wins from the in-tree patch. P4 deleted
  `build_extern_call_graph`; the cross-crate effect now comes entirely
  from per-leaf-crate elimination, summed across the workspace. The "zed
  -31%" / "rustc -48%" numbers in the v2 row of the RFC table predate P4
  and should be re-measured before stabilization (deferred — see below).
- **No claim** of vtable-method-level pruning (V6a). All impls of any
  trait used as `dyn Trait` remain conservatively kept.

---

## Deferred (explicitly out of scope for the initial `-Z` flag)

| Item | Why deferred |
|------|--------------|
| Per-method vtable pruning (V6a) | Needs post-mono query; mirrors `reachable.rs` FIXME |
| `cdylib` / `staticlib` mode (V1 future-work) | Needs user-supplied "exported symbols" list |
| Coroutine / `gen` block exclusion (R3) | One-line addition; UI test pending |
| `#[track_caller]` shim guard (R2) | One-line addition; UI test pending |
| LTO + `cross_crate_inlinable` interaction (R5) | One-line addition; UI test pending |
| `is_codegened_item` consumer audit (R6) | Forward-compat measure for stabilization |
| Re-measure zed/rustc-workspace after P4 | Cross-crate path deletion may have changed numbers |

---

## How to reproduce

### Patched stage1 oracle

```bash
git clone --depth=1 --branch=1.90.0 https://github.com/rust-lang/rust ~/rust-1.90
cd ~/rust-1.90
cat > bootstrap.toml << 'EOF'
profile = "compiler"
change-id = 144675
[llvm]
download-ci-llvm = false
[rust]
debug-assertions = true
overflow-checks = true
EOF

# Apply patch from cargo-slicer
cp /path/to/cargo-slicer/src/upstream_patch/dead_fn_elim.rs \
   compiler/rustc_mir_transform/src/dead_fn_elim.rs
git apply /path/to/cargo-slicer/src/upstream_patch/rustc_other_files.patch

env -u RUSTC_WRAPPER ./x.py build --stage 1 compiler/rustc
env -u RUSTC_WRAPPER ./x.py build --stage 1 library
rustup toolchain link dfe-stage1 build/x86_64-unknown-linux-gnu/stage1
```

### Corpus sweep harness

```bash
# precc-c/scripts/bench_ase_corpus.sh runs the slicer leg vs cargo +nightly
# baseline on each tarball, writes results/<name>-<version>.bench.json.
# 8-way parallel via bash job-control:
for f in fetch/*.crate; do
    while [ "$(jobs -r -p | wc -l)" -ge 8 ]; do wait -n; done
    NAME=...; VERSION=...
    ./scripts/bench_ase_corpus.sh "$NAME" "$VERSION" &
done
wait
python3 scripts/aggregate_ase_results.py > results/aggregate_full.json
```

---

## Outstanding posture

1. Open a draft PR against `rust-lang/rust` referencing this document and
   `docs/upstream-rfc.md`.
2. Lead the PR description with: *"this is `reachable_set ∪ entry-BFS`, not
   a `reachable_set` replacement"* — to dissolve the V9 framing up front.
3. Cross-link `docs/lcnr-review-feedback.md`,
   `docs/wesley-workingjubilee-review-feedback.md`,
   `docs/amanieu-review-feedback.md`, this file, and
   `docs/vadim-petrochenkov-review-feedback.md` so reviewers see the full
   review trail.
4. Re-measure zed and rustc-workspace under the post-P4 algorithm before
   filing the PR; if numbers regressed materially, restore a *minimal*
   cross-crate seeding path rather than the full extern call graph.

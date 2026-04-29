# Response to @petrochenkov Review (V1–V11): Status & Empirical Results

**Date**: 2026-04-29 (revised)
**Source review**: <https://github.com/yijunyu/cargo-slicer/issues/1>
**Companion documents**:
- `docs/vadim-petrochenkov-review-feedback.md` — full V1–V9 review with worked examples + patches P1–P9
- `docs/upstream-rfc.md` — the MCP for `-Z dead-fn-elimination`

This document is the concise round-trip response to Vadim's review: per-concern
status, the empirical evidence collected after the patches, and what's still
deferred.

## Reframing (V10–V11, 2026-04-29)

Two follow-up points from Vadim re-scope the contribution and supersede parts
of the V1–V9 framing below:

- **V10 — libraries**: "the same algorithm will actually work on libraries, if
  it's implemented correctly." Correct. The V1 early-return on
  `entry_fn().is_none()` was a scoping convenience, not a correctness
  requirement. With seeds drawn from `tcx.reachable_set(())` (P3), the BFS is
  already type-agnostic; the early-return should be lifted so libraries also
  benefit, with seeds = `reachable_set` (covers `pub` API + `#[no_mangle]` +
  inline/generic items needed downstream). This is now tracked as future-work
  (see "Deferred").
- **V11 — globally is where it pays off**: "crates do not typically contain
  dead code... only really eliminates dead code if applied globally, to a
  whole crate dependency tree." Also correct. Within a single crate
  `-Wunused`/`dead_code` already catches most intra-crate dead code; the real
  prize is the long tail of `pub` items in dependencies that the current
  binary's call graph never reaches. The single-crate `-Z` flag captures only
  the slice of that effect that survives monomorphization + LLVM DCE; the
  cross-crate orchestration in the userspace cargo-slicer (RUSTC_WRAPPER + a
  reconciled per-crate seed set) is what reaches the rest. See §3 below — we
  now report bin-only numbers for the in-tree flag and report the
  whole-corpus numbers separately as a userspace-tool zero-regression sweep.

The thread is **not** withdrawn: V1–V9 still tightened the patch (~419 → ~340
lines, three real bugs caught), the V10 fix is a small follow-on, and the
V11 reframing is what the userspace tool was already doing. See "What's
still claimed" at the end for the precise residual contribution.

---

## Point-by-point status

| ID  | Concern (paraphrase)                                  | Status                  | Patch evidence |
|-----|--------------------------------------------------------|-------------------------|----------------|
| V1  | Library code paths are vestigial; bail-out is right    | **Superseded by V10**   | P1 dropped `is_public()` and scoped to bins; V10 (2026-04-29) clarifies libs *can* work with `reachable_set` seeds — early-return is now future-work to lift |
| V10 | Same algorithm works on libraries with correct seeds   | **Acknowledged**        | Tracked as future-work; not yet patched (see Deferred) |
| V11 | Single-crate elimination is mostly redundant; cross-crate is where the win is | **Acknowledged** | Reframes contribution: in-tree `-Z` = correctness-preserving slice on bins (1.38× median); cross-crate effect lives in userspace cargo-slicer's RUSTC_WRAPPER orchestration |
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

### 3. ASE 2026 corpus sweep — split by crate kind

Per V10/V11 reframing: the in-tree `-Z dead-fn-elimination` flag is a no-op
on libraries (V1 early-return), so library numbers cannot be folded into a
single headline alongside binary numbers. We report two separate sweeps from
the same corpus run (`docs/ase2026-corpus.csv`,
<https://yijunyu.github.io/cargo-slicer/ase2026-corpus.html>):

#### 3a. Binary subset (n=65) — relevant to the in-tree `-Z` flag

| Metric                                  | Value |
|-----------------------------------------|------:|
| Binary crates attempted                 | 65    |
| Both legs built (clean compare)         | **59** |
| Baseline-only failures                  | 6     |
| **Slicer-only failures**                | **0** |
| Median build speedup                    | **1.38×** |
| Mean build speedup                      | 2.45× |
| % speedup ≥ 1.0×                        | 69.5% |
| % speedup ≥ 1.5×                        | 45.8% |
| % speedup ≥ 2.0×                        | 27.1% |
| 10th percentile                         | 0.67× |
| 90th percentile                         | 3.58× |
| Slowest case (`remove_dir_all` 1.0.0)   | 0.43× |
| Fastest case (`weezl` 0.1.12)           | 29.15× |

**On binaries the slicer-leg never failed when baseline succeeded** (0/59
correctness regressions; `--version` / `--help` smoke matched on every
both-built case). Median wall-time speedup is 1.38× — modest, consistent
with V11's observation that single-crate elimination overlaps heavily with
existing pipeline behavior. The 30.5% of bins where the slicer leg is
slower than baseline is dominated by crates whose total build is small
enough that pre-analysis overhead dominates (see slowest five in
`docs/ase2026-corpus.csv`).

#### 3b. Library subset (n=2,538) — userspace cargo-slicer only, NOT the `-Z` flag

This subset is included as a zero-regression stress-test of the userspace
tool's RUSTC_WRAPPER pipeline, **not** as a claim about
`-Z dead-fn-elimination`. The userspace tool computes seeds across the
whole dependency tree (V11) and does run on libraries (V10-style behavior).

| Metric                                  | Value |
|-----------------------------------------|------:|
| Library crates attempted                | 2,538 |
| Both legs built (clean compare)         | **2,393** |
| Baseline-only failures                  | 145   |
| **Slicer-only failures**                | **0** |
| Median build speedup (userspace tool)   | 1.50× |
| Mean build speedup (userspace tool)     | 3.99× |

The library median (1.50×) is **not** a claim about the in-tree flag. It
is a measurement of the userspace cross-crate orchestration that V11
correctly identifies as where the algorithm earns its keep.

### 4. What's *not* claimed

- **Single-crate `-Z` speedup on libraries** — V1 early-return makes the
  flag a no-op there. Lifting that early-return (V10) is tracked as
  future-work; we did *not* benchmark a "library `-Z`" leg.
- **Cross-crate wins from the in-tree patch alone**. P4 deleted
  `build_extern_call_graph`; whatever cross-crate effect remains is what
  bleeds through per-leaf-crate elimination + monomorphization + LLVM DCE.
  The earlier "zed -31%" / "rustc -48%" numbers in the v2 row of the RFC
  table predate P4 and need re-measurement (deferred).
- **Vtable-method-level pruning** (V6a). All impls of any trait used as
  `dyn Trait` remain conservatively kept.

### What *is* still claimed

- **Correctness**: across 59 binary crates that built under both legs, zero
  slicer-leg failures, `--version`/`--help` matched. The patched stage1
  oracle (§1) and the `reachable_set ⊆ post_bfs` invariant (§2) provide
  in-compiler evidence on top of the corpus.
- **Modest single-crate speedup on binaries**: 1.38× median, with the
  honest caveat that this is the slice of the cross-crate effect that
  survives monomorphization + LLVM DCE.
- **Patch hygiene**: V1–V9 caught three real bugs (`trimmed_def_paths`
  ICE, library overhead, `is_codegened_item` clone) and shrank the pass
  from ~419 to ~340 lines.

---

## Deferred (explicitly out of scope for the initial `-Z` flag)

| Item | Why deferred |
|------|--------------|
| **Library `rlib` mode (V10)** | Lift V1 early-return; seeds = `reachable_set` already gives correct roots for libs (`pub` API + `#[no_mangle]` + inline/generic items). Needs UI tests + a benchmarking pass distinct from the binary leg before it's defensible. |
| **Cross-crate orchestration upstream (V11)** | The single-crate `-Z` flag captures only the slice of cross-crate dead code that survives mono + LLVM DCE. A proper upstream answer would thread a global call graph into per-crate compilation (cargo-level mechanism), which is a separate RFC. |
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

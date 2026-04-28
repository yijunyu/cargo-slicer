# Action Plan: From Vadim's Review to a Validated In-Tree Patch

**Companion to:** `docs/vadim-petrochenkov-review-feedback.md`
**Goal:** Land patches P1–P9 with evidence that (1) the result is *more correct*
than the current patch, (2) we *retain or improve* on the published
benchmark numbers, and (3) we exercise correctness + performance
*at scale* on the ASE 2026 paper's crates.io corpus.

The plan is organised into three sequential tracks. Each track has explicit
exit gates. Do not advance until the gate passes.

---

## Track 1 — Correctness (P1–P9 land + new tests pass)

The point of this track is to *prove* the new algorithm is correct, not just
that it compiles. "Correct" means: for every category Vadim flagged, we have
either a **UI test that reproduces the failure mode in the old code and
passes under the new code**, or a **diagnostic that demonstrates
reachable_set ⊆ post-BFS-set ⊆ all-fns**.

### Step 1.1 — Branch and skeleton (½ day)

```bash
cd /home/y00577373/precc-c
git checkout -b vadim-review-fixes
git checkout -b vadim-review-fixes-rustc /tmp/rust   # parallel branch in rustc tree
```

Add a test directory shell:

```bash
mkdir -p tests/dead-fn-elim/{ui,run-make,fixtures}
```

**Gate 1.1**: branch exists; `cargo build --release` is green (no patch yet).

### Step 1.2 — Land patches in dependency order (~3 days)

Apply patches in the order below — each is independently reviewable and
each leaves a green test suite.

| # | Patch | Why it goes in this order |
|---|-------|---------------------------|
| 1 | **P7** delete `#[test]/#[bench]` checks | smallest; uncovers harness reliance early |
| 2 | **P3** seed from `reachable_set` | foundation for V5b/V8 simplifications |
| 3 | **P5a** track `ReifyFnPointer` / `ClosureFnPointer` | enables P8 deletions safely |
| 4 | **P5b** track `InlineAsm::SymFn` | rounds out address-taken set |
| 5 | **P8a** drop `unsafe fn` ban | now subsumed by P5a |
| 6 | **P8b** drop fn-ptr / dyn signature walk | now subsumed by P3 + P5a |
| 7 | **P6** `requires_monomorphization` predicate | unlocks more candidates |
| 8 | **P1** drop `is_public()` branch + scope doc | the V1 cleanup |
| 9 | **P4** delete `build_extern_call_graph` | the largest deletion, must come *last* |
| 10 | **P9** stats diagnostic + RFC update | requires final shape |
| 11 | **P2** RFC text correction | last; depends on P9 numbers |

After each patch:
```bash
# Inside cargo-slicer
cargo test --release
# Inside /tmp/rust (after copying dead_fn_elim.rs)
python3 x.py test compiler/rustc --stage 1
python3 x.py test tests/ui/dead-fn-elimination --stage 1
```

**Gate 1.2**: each patch lands as a *separate commit*; the cargo-slicer test
suite stays green at every step.

### Step 1.3 — UI test matrix for the categories Vadim flagged (~1 day)

Add the following files under `tests/dead-fn-elim/ui/` (mirrored later into
`/tmp/rust/tests/ui/dead-fn-elimination/`):

| File | Sub-issue covered | Expected behaviour |
|------|-------------------|--------------------|
| `fn-ptr-coercion.rs` | V5a | function whose address is taken via `as fn()` is **not** eliminated |
| `closure-fn-ptr.rs` | V5a | `Fn`-typed closure coerced to `fn()` ptr is **not** eliminated |
| `inline-asm-sym.rs` | V5b | function referenced via `asm!("call {f}", f = sym name)` is **not** eliminated |
| `track-caller.rs` | R2 | `#[track_caller]` shim is **not** eliminated |
| `coroutine.rs` | R3 | generator body is **not** eliminated (`feature(coroutines)`) |
| `proc-macro-helper.rs` | R4 | `proc-macro = true` crate's private helper is **not** eliminated |
| `cross-crate-inlinable.rs` | R5 | private `#[inline]` callee is preserved when LTO is on |
| `test-harness.rs` | V7 | `#[test] fn t() {}` survives `--test` compilation |
| `private-helper-pruned.rs` | V9 (positive) | private fn called only by another private unreachable fn **is** eliminated |
| `library-noop.rs` | V1 (negative) | building as `--crate-type rlib` eliminates **zero** functions |

Each test uses the existing `// run-pass` / `// build-pass` pattern plus
a `// stderr` check on the `note: N unreachable functions excluded`
diagnostic from P9.

**Gate 1.3**: all tests above pass under the patched stage1 compiler. Run
in CI via `python3 x.py test tests/ui/dead-fn-elimination`.

### Step 1.4 — Property-based correctness check on cargo-slicer's external tool (~½ day)

The external `cargo-slicer` tool runs the *same algorithm* as a
`RUSTC_WRAPPER`. Reuse it as an oracle:

1. Pick 5 binaries from the existing benchmark suite: ripgrep, helix, zed,
   nushell, rustc-workspace.
2. For each, build twice:
   a. `cargo build --release` (baseline).
   b. `cargo build --release` with the patched stage1 + `-Z dead-fn-elimination`.
3. Run the project's *own* test suite (`cargo test --release`) against the
   flag-built binary. **Every pre-existing test must pass.**
4. Diff the linker symbol tables (`nm --defined-only target/release/<bin> | sort`).
   Count: `(symbols_eliminated, symbols_added, symbols_preserved)`.
   Assert `symbols_added == 0`.

Stash the diffs in `bench-results/symbol-diffs/<project>.txt` for the
write-up.

**Gate 1.4**: zero added symbols, zero project-test regressions on all 5.

### Step 1.5 — Publish the diagnostic invariant (½ day)

Add a `-Z print-dfe-stats` (P9) sanity check:

> For every compilation, **`reachable_set ⊆ post-BFS-set ⊆ all-fns`**.
> Violation = bug.

Wire this as a debug-assertion inside `run_analysis`:

```rust
debug_assert!(tcx.reachable_set(()).iter()
    .all(|d| reachable_keys.contains(&def_id_key(d.to_def_id()))));
```

Build a `release-with-debug-assertions` stage1 and run the full Track 1
test matrix. The assertion must never fire.

**Gate 1.5**: assertion holds on all 10 UI tests + all 5 oracle projects.

---

## Track 2 — Performance regression check (recover the published numbers)

The point of this track is to confirm we didn't regress. We have published
numbers from two sources:

- **External `cargo-slicer` (2026-02-14)** — zed -29%, rustc -17%, helix -6%, ripgrep -4%.
- **In-tree `-Z dead-fn-elimination` v2 (2026-03-08)** — zed -31%, rustc -48%, helix 0%, ripgrep 0%.

After P1–P9 the algorithm is *strictly tighter on the seed surface*
(reachable_set ⊆ old seed surface in many cases) and *strictly wider*
on eligibility (we trust address-taken seeding rather than the
unsafe/sig-walk bans). The expected outcome is "at least as fast,
possibly faster".

### Step 2.1 — Reproduce the existing baseline (1 day)

Before changing anything, lock down the existing 4-project benchmark
numbers on the *current* hardware. Vadim's review concerns are about
correctness; if our hardware shifted since 2026-03-08 we need to know
*before* attributing changes to the patch.

```bash
# All commands assume nightly-2026-03-12 stage1 patched compiler is available
for proj in ripgrep helix zed nushell; do
  ./scripts/bench_fresh_build.sh $proj zflag-baseline 3
  ./scripts/bench_fresh_build.sh $proj zflag 3
done
# rustc workspace via x.py
./scripts/bench_fresh_build.sh rustc zflag-baseline 3
./scripts/bench_fresh_build.sh rustc zflag 3
```

Record results to `bench-results.db` with run-tag `pre-vadim-fixes`.
Generate the comparison HTML:

```bash
python3 scripts/bench_report.py --tag pre-vadim-fixes \
  > docs/bench-pre-vadim.html
```

**Gate 2.1**: numbers reproduce within ±2% of the 2026-03-08 RFC table.
If a project diverges by more than 2%, investigate hardware/toolchain
shift before continuing — patch evaluation is meaningless against a
moving baseline.

### Step 2.2 — Re-run the full benchmark after each major patch (½ day each)

After each of P3, P4, P6, P9 (the patches that actually change traversal
cost), re-run the 5-project benchmark and tag results:

| Run tag | What it captures |
|---------|------------------|
| `post-p3` | reachable_set seeding (expect: ≤1% delta — same closure, different seed) |
| `post-p4` | extern-graph deletion (expect: **faster** for zed/rustc — fewer MIR reads) |
| `post-p6` | `requires_monomorphization` (expect: **more eliminated**, possibly faster) |
| `post-p9` | final state |

Plot `(eliminated_count, wall_time_delta)` as a scatter per project.
Submit `docs/bench-post-vadim.html` with the deltas.

**Gate 2.2**: after P9, no project regresses by more than 3% in wall time
versus `pre-vadim-fixes`. zed and rustc workspace must retain ≥ -25%
versus their baseline.

### Step 2.3 — Per-crate breakdown for the rustc workspace (½ day)

Vadim's V2 specifically calls out the rustc-workspace number.
Generate the per-crate table requested in P2:

```bash
./scripts/bench_db.py --rustc-breakdown --tag post-p9 \
  > docs/rustc-workspace-breakdown.csv
```

Columns: `crate_name, baseline_secs, flag_secs, eliminated_count,
reachable_set_size`. Sort by `eliminated_count` desc.

**Gate 2.3**: top 10 contributors to the workspace-wide saving are
named explicitly in the RFC. Sum of per-crate savings equals
workspace-wide saving within 5% (the rest is link-time).

---

## Track 3 — At-scale validation on the ASE 2026 crates.io corpus

The paper targets the **top 2,669 crates by download count** on crates.io
(Li et al. corpus). We use a *graduated* roll-out — 50 → 500 → 2,669 —
to catch crash patterns early without burning compute on a known-broken
flag.

### Step 3.1 — Fetch the corpus (½ day, mostly waiting)

The corpus directory is currently empty (`ls fetch/ → 0 entries`). Run:

```bash
cd /home/y00577373/ase2026d/crates-corpus
./fetch-crates.sh 2669            # ~45 min, respects 1 req/sec policy
ls ../fetch/ | wc -l               # expect: ≈ 2669
```

The fetcher writes `corpus.json` (index) and `fetch/<name>-<ver>.crate`.
Some crates fail (yanked, missing tarball) — log to
`fetch-failures.log`. Aim for ≥ 95% fetch success.

**Gate 3.1**: ≥ 2,500 crates downloaded; `corpus.json` populated.

### Step 3.2 — Tier the corpus by build profile (1 day)

Most crates.io crates are libraries with no `bin` target — those will
exit early under V1 and contribute nothing to dead-fn-elim performance,
but they *still* exercise correctness (we compile with the flag and
check we don't crash or change output).

Categorise:

```bash
python3 scripts/triage_corpus.py /home/y00577373/ase2026d/corpus.json \
  > /home/y00577373/ase2026d/corpus-tiered.json
```

Tier columns:
- `tier-A` (binaries / has `[[bin]]`): expected speed-up candidates.
- `tier-B` (libraries with examples / tests): correctness-only via test build.
- `tier-C` (proc-macro): R4 mitigation target.
- `tier-D` (no_std / wasm-only / niche targets): skip unless tier-A/B fails.

**Gate 3.2**: tier counts published; tier-A + tier-B ≥ 2,000 crates.

### Step 3.3 — Pilot on 50 crates (1 day)

Pick 50 crates spread across tier-A/B/C by download rank
(top-10, top-50, top-200, top-1000, top-2000 quintiles, 10 each).

```bash
python3 scripts/run_corpus_bench.py \
  --tag pilot-vadim --tier-mix \
  --limit 50 \
  --modes baseline,zflag \
  --runs 1 \
  --timeout 600
```

For each crate the runner:
1. Extracts `<name>-<ver>.crate` to `/tmp/corpus-work/<name>`.
2. Builds with `cargo +stage1 build --release`.
3. Builds with `RUSTFLAGS="-Z dead-fn-elimination" cargo +stage1 build --release`.
4. Records: build success, wall time, `note: N excluded` count, binary
   size delta, *test pass-rate* if `cargo test --release --no-run` is
   buildable.
5. On failure: stash compiler stderr in
   `bench-results/corpus/<crate>-<ver>/failure.log`.

**Gate 3.3**: pilot success rate (build-success-with-flag /
build-success-without-flag) ≥ 99%. Investigate every failure manually:
each one represents a category Vadim's review *missed* — write it up
as an addendum.

### Step 3.4 — Mid-scale on 500 crates (2 days)

```bash
python3 scripts/run_corpus_bench.py \
  --tag mid-vadim --top 500 \
  --modes baseline,zflag --runs 1 --timeout 600
```

Same metrics; aggregate:

```bash
python3 scripts/aggregate_corpus.py --tag mid-vadim \
  > docs/corpus-mid-vadim.csv
```

Key reports:
- **Distribution of `eliminated_count` across tier-A.** Median, p90, max.
- **Distribution of wall-time delta across tier-A.** Negative = speed-up.
- **Failure clusters.** Group failures by compiler stderr prefix; if
  cluster size > 3, classify as a new sub-issue (V10, V11, …) and patch.
- **Symbol-table additions.** Should be 0 across all crates.

**Gate 3.4**: success rate ≥ 99.5%; symbol additions = 0; tier-A median
speed-up ≥ 0% (no regression). Speed-up of any kind is a bonus at
this scale; the *primary* goal is correctness.

### Step 3.5 — Full corpus run (3 days, mostly compute)

```bash
python3 scripts/run_corpus_bench.py \
  --tag full-vadim --all \
  --modes baseline,zflag --runs 1 --timeout 900 \
  --parallel 8
```

Final aggregation feeds the paper:

```bash
python3 scripts/aggregate_corpus.py --tag full-vadim \
  --output docs/corpus-full-vadim.csv \
  --histogram docs/corpus-histogram.svg \
  --pass-fail-table docs/corpus-pass-fail.tex
```

**Gate 3.5**:
- Build-with-flag success rate ≥ 99.5% over the full 2,669.
- Test-pass rate (where tests buildable) ≥ 99.9% — every regression must
  be classified as either a known-broken crate (also fails baseline) or a
  new bug.
- Linker-symbol-additions count = 0 across the full corpus.
- Median tier-A wall-time delta: report honestly, expected ≤ 0%.

### Step 3.6 — Differential analysis vs. v1 algorithm (½ day)

For 100 of the tier-A crates, build with **both** the pre-vadim and
post-vadim algorithms and diff:

| Metric | v1 (current upstream patch) | v2 (post-Vadim) |
|--------|-----------------------------|------------------|
| `eliminated_count` median | … | … |
| Compile wall time median | … | … |
| Stage1 driver crashes | … | … |

The expected story:
- v2 eliminates *equal or more* fns (P6 unlocks generic-monomorphic
  cases, P5/P8 widens eligibility).
- v2 wall time is equal or better (P4 deletes the cross-crate scan).
- v2 crashes less (P5b fixes the inline-asm hole; P7 removes a useless
  branch that occasionally interacted with custom test harnesses).

**Gate 3.6**: v2 dominates v1 on at least two of the three axes; doesn't
lose on any.

---

## Cross-cutting deliverables

By the end of Track 3, the repo should have:

1. **Code**
   - `src/upstream_patch/dead_fn_elim.rs` reduced to ~340 lines (P1–P8).
   - `src/upstream_patch/rustc_other_files.patch` updated in lockstep.
   - `tests/dead-fn-elim/ui/*.rs` (10 tests, Track 1).
   - `scripts/run_corpus_bench.py`, `scripts/triage_corpus.py`,
     `scripts/aggregate_corpus.py` (Track 3).
2. **Documents**
   - `docs/vadim-petrochenkov-review-feedback.md` (already done).
   - `docs/vadim-response-action-plan.md` (this file).
   - `docs/upstream-rfc.md` updated with corrected rustc-workspace label,
     per-crate breakdown, and corpus-scale evidence section.
   - `docs/rustc-workspace-breakdown.csv` (Track 2).
   - `docs/corpus-full-vadim.csv` + `corpus-histogram.svg` +
     `corpus-pass-fail.tex` (Track 3).
3. **Communications**
   - GitHub comment on `yijunyu/cargo-slicer#1` summarising P1–P9 status,
     linking the action plan and post-fix benchmarks.
   - A draft PR against `rust-lang/rust` (do not open until Gate 3.5
     passes), opening with the V9-reframing paragraph.

---

## Time / resource budget

| Track | Effort (engineer-days) | Wall time | Compute |
|-------|-----------------------|-----------|---------|
| 1 (correctness) | 5 | 1 week | 1 build host |
| 2 (regression check) | 3 | 3 days | 1 build host (48-core preferred) |
| 3 (corpus) | 6 | 2 weeks | 1 build host + 8-way parallelism |

Optimistic schedule: **3 weeks** with no surprises. Each gate failure
adds ~2 days of triage; reserve a 1-week buffer.

---

## What to do if a gate fails

The plan is intentionally gated so failures surface early. The default
response is:

1. **Track 1 gate fails**: stop. The patch is wrong; iterate on the patch
   text, *not* the test. Vadim's review is the spec.
2. **Track 2 gate fails (regression > 3%)**: identify which patch caused
   the regression via the per-patch tag (`post-pX`); revert if needed and
   discuss in cargo-slicer issue #1 *before* changing the algorithm.
3. **Track 3 gate fails (success rate < 99.5%)**: cluster the failures by
   compiler stderr; the largest cluster is a new V-issue; patch it,
   re-run from Step 3.3 (pilot), not from full corpus.

The *worst* failure mode is silently shipping a patch that tightens
benchmarks but breaks ≥ 0.1% of the corpus. The corpus track exists
specifically to make that mode loud.

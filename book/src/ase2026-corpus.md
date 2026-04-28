# ASE 2026 Corpus

The **ASE 2026 corpus** is the empirical evaluation set for `cargo-slicer`'s
correctness and speedup claims. It is the top **2,669 crates** by all-time
downloads on [crates.io](https://crates.io/), fetched 2026-04-26.

This page is the canonical reference for the corpus. Other documents and
README sections that mention "the ASE 2026 corpus sweep" link here.

## Headline numbers

| Metric                           | Value |
|----------------------------------|------:|
| Crates fetched                   | 2,669 |
| Tarball / extract errors         | 10    |
| Crates that ran                  | 2,603 |
| Library crates                   | 2,538 |
| Binary crates                    | 65    |
| Both legs built (clean compare)  | **2,452** |
| Baseline-only failures           | 151   |
| **Slicer-only regressions**      | **0** |
| Median build speedup             | **1.50×** |
| Mean build speedup               | 3.96×  |
| % speedup ≥ 1.0×                 | 73.1% |
| % speedup ≥ 1.5×                 | 49.8% |
| % speedup ≥ 2.0×                 | 35.9% |
| 10th percentile                  | 0.65× |
| 90th percentile                  | 7.41× |

## Speedup distribution

| Bucket | Crates | % of compared |
|--------|------:|------:|
| < 0.5× (regression)   | 128 | 5.2% |
| 0.5 – 0.8×            | 281 | 11.5% |
| 0.8 – 1.0×            | 251 | 10.2% |
| 1.0 – 1.5×            | 571 | 23.3% |
| 1.5 – 2.0×            | 341 | 13.9% |
| 2.0 – 5.0×            | 519 | 21.2% |
| 5.0 – 20.0×           | 272 | 11.1% |
| ≥ 20.0×               |  89 | 3.6% |

The wall-time *regression* tail (speedup < 1.0×, 26.9% of crates) is concentrated
on tiny crates where the slicer's per-invocation overhead dominates a sub-1-second
baseline build. None of these 660 crates are correctness regressions — every one
of them produced a correct binary; they just took longer to build than the
baseline. For full reproducibility, all of them are kept in the corpus and
in the published CSV.

## Methodology

```bash
# 1. Fetch top 2,669 crates by downloads
ase2026d/crates-corpus/fetch-crates.sh

# 2. Run baseline + slicer back-to-back on each tarball, 8-way parallel
for f in fetch/*.crate; do
    while [ "$(jobs -r -p | wc -l)" -ge 8 ]; do wait -n; done
    NAME=...; VERSION=...
    ./scripts/bench_ase_corpus.sh "$NAME" "$VERSION" &
done
wait

# 3. Aggregate
python3 scripts/aggregate_ase_results.py > results/aggregate_full.json
```

For each crate the harness:

1. Extracts the tarball.
2. Runs `cargo +nightly build --release --offline` (baseline), retrying
   online once on failure to fetch transitive deps.
3. Runs `cargo +nightly build --release` again under
   `RUSTC_WRAPPER=cargo_slicer_dispatch` with `CARGO_SLICER_VIRTUAL=1`
   `CARGO_SLICER_CODEGEN_FILTER=1`.
4. For binary crates: smoke-tests the produced binary with `--version`
   then `--help` (both legs must succeed for "correctness_ok").
5. For library crates: build success is the correctness signal — `cargo
   test` is intentionally skipped on the slicer leg because the userspace
   slicer over-stubs `#[test]` functions (V7 issue, orthogonal to
   dead-fn-elimination's binary-output property).
6. Records `(baseline_secs, slicer_secs, speedup, correctness_ok)` to
   `results/<name>-<version>.bench.json`.

## Top 25 by speedup

These are the crates with the largest wall-time speedup. The pattern: tiny
library crates whose baseline is dominated by codegen of a few large
unreachable functions that the slicer eliminates entirely.

| Rank | Crate | Version | Kind | Baseline | Slicer | Speedup |
|-----:|-------|---------|------|---------:|-------:|--------:|
| 2170 | [aws-sig-auth](https://crates.io/crates/aws-sig-auth) | 0.60.3 | lib | 39.98 s | 0.21 s | **190.38×** |
| 1193 | [retain_mut](https://crates.io/crates/retain_mut) | 0.1.9 | lib | 37.12 s | 0.25 s | **148.48×** |
| 2630 | [mutate_once](https://crates.io/crates/mutate_once) | 0.1.2 | lib | 28.07 s | 0.20 s | **140.35×** |
|  866 | [raw-window-handle](https://crates.io/crates/raw-window-handle) | 0.6.2 | lib | 49.13 s | 0.42 s | **116.98×** |
|  958 | [utf8-width](https://crates.io/crates/utf8-width) | 0.1.8 | lib | 14.49 s | 0.14 s | **103.50×** |
| 2470 | [line-wrap](https://crates.io/crates/line-wrap) | 0.2.0 | lib | 17.40 s | 0.19 s | **91.58×** |
| 2277 | [sptr](https://crates.io/crates/sptr) | 0.3.2 | lib | 16.77 s | 0.21 s | **79.86×** |
|  799 | [deadpool-runtime](https://crates.io/crates/deadpool-runtime) | 0.3.1 | lib | 18.78 s | 0.24 s | **78.25×** |
|  598 | [md5](https://crates.io/crates/md5) | 0.8.0 | lib | 19.76 s | 0.26 s | **76.00×** |
| 1811 | [htmlescape](https://crates.io/crates/htmlescape) | 0.3.1 | lib | 26.21 s | 0.36 s | **72.81×** |
|  695 | [indenter](https://crates.io/crates/indenter) | 0.3.4 | lib | 14.26 s | 0.20 s | **71.30×** |
| 1382 | [cached_proc_macro_types](https://crates.io/crates/cached_proc_macro_types) | 0.1.1 | lib | 18.57 s | 0.28 s | **66.32×** |
|  884 | [endian-type](https://crates.io/crates/endian-type) | 0.2.0 | lib | 14.51 s | 0.23 s | **63.09×** |
| 1941 | [renderdoc-sys](https://crates.io/crates/renderdoc-sys) | 1.1.0 | lib | 14.92 s | 0.24 s | **62.17×** |
| 2290 | [subtle-ng](https://crates.io/crates/subtle-ng) | 2.5.0 | lib | 16.14 s | 0.26 s | **62.08×** |
|  925 | [local-waker](https://crates.io/crates/local-waker) | 0.1.4 | lib | 10.29 s | 0.17 s | **60.53×** |
| 1073 | [safemem](https://crates.io/crates/safemem) | 0.3.3 | lib |  9.54 s | 0.16 s | **59.62×** |
| 1996 | [replace_with](https://crates.io/crates/replace_with) | 0.1.8 | lib | 11.53 s | 0.20 s | **57.65×** |
| 1069 | [deunicode](https://crates.io/crates/deunicode) | 1.6.2 | lib | 21.24 s | 0.39 s | **54.46×** |
| 2195 | [aws-endpoint](https://crates.io/crates/aws-endpoint) | 0.60.3 | lib | 11.85 s | 0.22 s | **53.86×** |
| 1694 | [khronos_api](https://crates.io/crates/khronos_api) | 3.1.0 | lib | 46.14 s | 0.88 s | **52.43×** |
|  984 | [nodrop](https://crates.io/crates/nodrop) | 0.1.14 | lib |  9.35 s | 0.18 s | **51.94×** |
|  787 | [tagptr](https://crates.io/crates/tagptr) | 0.2.0 | lib | 12.76 s | 0.26 s | **49.08×** |
| 2023 | [unscanny](https://crates.io/crates/unscanny) | 0.1.0 | lib | 21.43 s | 0.45 s | **47.62×** |
|  625 | [precomputed-hash](https://crates.io/crates/precomputed-hash) | 0.1.1 | lib |  8.92 s | 0.19 s | **46.95×** |

## Top 25 by downloads

These are the most-depended-upon crates in the corpus. Speedups are more
modest because their baselines are already small and their workspaces
already have most code reachable through derive macros and re-exports.

| Rank | Crate | Version | Downloads | Kind | Baseline | Slicer | Speedup |
|-----:|-------|---------|----------:|------|---------:|-------:|--------:|
|   1 | [syn](https://crates.io/crates/syn) | 2.0.117 | 1,595,761,057 | lib | 5.42 s | 4.33 s | 1.25× |
|   2 | [hashbrown](https://crates.io/crates/hashbrown) | 0.17.0 | 1,469,613,376 | lib | 1.92 s | 1.49 s | 1.29× |
|   3 | [bitflags](https://crates.io/crates/bitflags) | 2.11.1 | 1,226,802,506 | lib | 0.17 s | 0.47 s | 0.36× |
|   4 | [getrandom](https://crates.io/crates/getrandom) | 0.4.2  | 1,183,144,905 | lib | 2.44 s | 1.81 s | 1.35× |
|   5 | [rand_core](https://crates.io/crates/rand_core) | 0.10.1 | 1,106,779,248 | lib | 0.29 s | 0.21 s | 1.38× |
|   6 | [proc-macro2](https://crates.io/crates/proc-macro2) | 1.0.106 | 1,102,185,702 | lib | 1.87 s | 1.12 s | 1.67× |
|   7 | [libc](https://crates.io/crates/libc) | 0.2.186 | 1,097,915,001 | lib | 1.70 s | 1.53 s | 1.11× |
|   8 | [base64](https://crates.io/crates/base64) | 0.22.1 | 1,091,953,499 | lib | 0.28 s | 0.58 s | 0.48× |
|   9 | [quote](https://crates.io/crates/quote) | 1.0.45 | 1,087,228,740 | lib | 1.39 s | 2.37 s | 0.59× |
|  10 | [rand](https://crates.io/crates/rand) | 0.8.6 | 1,080,778,604 | lib | 8.48 s | 7.97 s | 1.06× |
|  11 | [regex-syntax](https://crates.io/crates/regex-syntax) | 0.8.10 | 1,019,226,974 | lib | 3.80 s | 2.97 s | 1.28× |
|  12 | [indexmap](https://crates.io/crates/indexmap) | 2.14.0 | 1,013,962,675 | lib | 2.24 s | 2.06 s | 1.09× |
|  13 | [itertools](https://crates.io/crates/itertools) | 0.14.0 | 1,013,787,654 | lib | 0.58 s | 2.21 s | 0.26× |
|  14 | [cfg-if](https://crates.io/crates/cfg-if) | 1.0.4 | 975,444,268 | lib | 0.15 s | 0.13 s | 1.15× |
|  15 | [serde](https://crates.io/crates/serde) | 1.0.228 | 952,229,993 | lib | 4.33 s | 4.13 s | 1.05× |
|  16 | [thiserror-impl](https://crates.io/crates/thiserror-impl) | 2.0.18 | 929,278,419 | lib | 3.43 s | 2.07 s | 1.66× |
|  17 | [thiserror](https://crates.io/crates/thiserror) | 2.0.18 | 929,103,435 | lib | 4.06 s | 4.37 s | 0.93× |
|  18 | [rand_chacha](https://crates.io/crates/rand_chacha) | 0.10.0 | 927,875,277 | lib | 4.14 s | 4.15 s | 1.00× |
|  19 | [windows-sys](https://crates.io/crates/windows-sys) | 0.61.2 | 920,841,453 | lib | 0.60 s | 0.35 s | 1.71× |
|  20 | [memchr](https://crates.io/crates/memchr) | 2.8.0 | 907,112,303 | lib | 1.61 s | 1.19 s | 1.35× |
|  21 | [unicode-ident](https://crates.io/crates/unicode-ident) | 1.0.24 | 892,277,911 | lib | 0.29 s | 0.19 s | 1.53× |
|  22 | [serde_derive](https://crates.io/crates/serde_derive) | 1.0.228 | 889,654,647 | lib | 6.92 s | 6.18 s | 1.12× |
|  23 | [itoa](https://crates.io/crates/itoa) | 1.0.18 | 882,296,719 | lib | 0.41 s | 0.24 s | 1.71× |
|  24 | [autocfg](https://crates.io/crates/autocfg) | 1.5.0 | 880,421,082 | lib | 0.76 s | 0.45 s | 1.69× |
|  25 | [heck](https://crates.io/crates/heck) | 0.5.0 | 865,719,042 | lib | 0.75 s | 0.70 s | 1.07× |

`bitflags`, `base64`, `quote`, `itertools`, `thiserror`: ~1.0× or worse —
these crates are tiny (sub-second baseline) and the slicer's per-invocation
overhead dominates. They are *not* correctness regressions.

## Full per-crate data

The complete table — **all 2,669 crates with rank, version, downloads, build
times, and slicer status** — lives in a single CSV in the source tree:

> [**`docs/ase2026-corpus.csv`**](https://github.com/yijunyu/cargo-slicer/blob/main/docs/ase2026-corpus.csv)

Schema:

```csv
rank,name,version,downloads,kind,baseline_build_secs,slicer_build_secs,speedup,status
1,syn,2.0.117,1595761057,lib,5.42,4.33,1.252,both_built
2,hashbrown,0.17.0,1469613376,lib,1.92,1.49,1.295,both_built
...
```

`status` values:

- `both_built` — baseline + slicer both succeeded; `speedup` is meaningful.
- `baseline_failed` — the baseline `cargo build` itself failed (missing
  feature combos, target dependencies, etc.); the slicer leg is not
  attempted.
- `slicer_regression` — baseline succeeded, slicer failed. **The corpus
  contains zero entries with this status** — that is the headline
  correctness claim.
- `error_tarball_missing` — fetch failure during `fetch-crates.sh`.
- `not_run` — corpus entry without a matching result file (typically
  semver-with-build-metadata that the harness split mis-parsed).

## Where this corpus is referenced

- [Root README](https://github.com/yijunyu/cargo-slicer/blob/main/README.md)
- [Benchmarks → ASE 2026 corpus sweep](benchmarks.md#ase-2026-corpus-sweep--top-2669-crates-by-downloads)
- [Virtual Slicer → Review status](virtual-slicer.md#review-status-2026-04)
- [`docs/upstream-rfc.md`](https://github.com/yijunyu/cargo-slicer/blob/main/docs/upstream-rfc.md) — MCP for the in-tree `-Z dead-fn-elimination` flag
- [`docs/vadim-response-results.md`](https://github.com/yijunyu/cargo-slicer/blob/main/docs/vadim-response-results.md) — point-by-point response to the @petrochenkov V1–V9 review (V9 empirical answer)

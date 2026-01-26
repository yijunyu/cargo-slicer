# cargo-slicer Development History

This document tracks major achievements, optimizations, and milestones for research paper and blog documentation.

## January 26, 2026

### Zero-Warning Sliced Builds
- **Problem**: Sliced workspace produced 25 warnings across 7 categories
- **Fix**: Three changes to `src/slicer/copy.rs` Cargo.toml transformation pipeline:
  1. **Lint suppressions**: Added `[lints.rust]` entries for `dead_code`, `unused_must_use`, `mismatched_lifetime_syntaxes`, `unpredictable_function_pointer_comparisons` (10 warnings)
  2. **Strip `[profile.*]` sections**: Non-root packages can't have profile overrides (6 warnings)
  3. **Strip `resolver =` / insert `edition = "2015"`**: Non-root packages can't set resolver; old crates need explicit edition (9 warnings)
- **Result**: 0 warnings, 153/153 crates compile (100%)

### Stop Hook with Slicer Integration
- Added `SessionEnd` hook alongside `Stop` for reliable session-exit triggering
- Hook runs `cargo-slicer --no-bench -c` and emails results with full slicer output
- Covers both normal session end and Ctrl+C exits

## January 25, 2026

### Multi-Version Crate Support
- **Feature**: Automatically routes consumers to correct versioned sliced directories
- Uses cargo metadata resolve graph to determine which version each consumer needs
- Falls back to registry for older versions (only latest version is sliced)
- Example: `bitflags-1.3.2-sliced/` vs `bitflags-2.6.0-sliced/`

### Graph-Guided Deletion Improvements
- Fixed dependency graph accuracy in trial deletion, eliminating all 19 skip entries
- Reduced COMPLEX_CRATES blocklist through better graph analysis
- Changed defaults to `-O3` with clean and bench mode

### Span-Based Deletion and LOC Measurement
- Switched from line-based to span-based deletion for more accurate item removal
- Added total-line LOC measurement for precise reduction tracking

### LOC Report Enhancements
- Added TOTAL and ORIGINAL summary rows showing aggregate reduction
- Added system and compiler info header (Rust version, target, CPU, load)
- Improved table alignment with consistent column widths

### Build Warning Suppression (Initial)
- Suppressed `missing_abi` and `unexpected_cfgs` warnings in sliced output
- Added `extern "C"` ABI to bare extern blocks (deprecated in Rust 2024)

## January 24, 2026

### Dogfooding Success: Self-Hosting with Sliced Dependencies
- **Achievement**: cargo-slicer successfully builds itself using its own sliced dependencies!
- **Process**:
  1. Run `./target/release/cargo-slicer -O2` to generate sliced deps
  2. Switch `Cargo.toml` → `Cargo.toml.sliced`
  3. Run `cargo build --release`
  4. Binary works correctly (73MB)
- **Result**: Self-hosting proven - the slicer can use its own output
- **Flywheel**: Each improvement to slicing makes future slicing faster

### Phase B: Self-Slicing Validation
- **Tested**: All optimization levels on cargo-slicer itself (136 crates)
- **Final Results** (after all fixes):

| Level | Items Deleted | Individual Compile | Full Build | Notes |
|-------|--------------|-------------------|------------|-------|
| `-O0` | 0 | 136/136 ✓ | ✓ | Conservative baseline |
| `-O1` | varies | 136/136 ✓ | ✓ | Private fn deletion safe! |
| `-O2` | varies | 136/136 ✓ | ✓ | +constants, +types safe! |
| `-O3` | varies | 136/136 ✓ | ✓ | Same as O2 |
| `-O4` | varies | 136/136 ✓ | ✗ | trust-graph too aggressive |

### Deletion Analysis (-O2 on Self-Slicing)
- **Total items deleted**: 333 across 136 crates
- **Breakdown**:
  - Functions: 329 (98.8%)
  - Constants: 3 (0.9%)
  - Statics: 1 (0.3%)
  - Types: 0
- **Types of deleted functions**:
  - Test functions (`test_*`)
  - Internal helpers (`_split_to_must_use`)
  - Unused private utilities
- **Observation**: Text-based heuristic is conservative (correct) but limits deletion
- **Impact**: Low LOC reduction explains similar build times for sliced vs normal

### Text-Based Heuristic for Macro Safety
- **Problem**: Functions inside `cfg_if!` macros weren't visible to AST parser
- **Root cause**: `decode_stopped`, `stop_signal` etc. defined/called inside macro bodies
- **Fix**: Added text-based heuristic in `deleter.rs`
- **Logic**: Only delete if `name(` or `name` doesn't appear elsewhere in file
- **Result**: Private functions called from macro bodies are preserved

### Feature Flag Always-True Fix
- **Problem**: `#[cfg(feature = "cjk")]` items filtered during parsing
- **Root cause**: Only `"std"` in `enabled_features`, so `feature = "cjk"` → false
- **Fix**: Changed `cfg_eval.rs` to always return true for `CfgExpr::Feature`
- **Rationale**: Let Cargo decide feature flags at build time, keep all feature-gated code

### Dependency Graph Fix: Private Function Preservation
- **Problem**: -O1 through -O3 were deleting private functions that were still needed
- **Root cause**: Private functions called by public functions weren't marked as used
- **Fix**: Modified `marker.rs` to mark all dependencies as used (Phase 1), not just entry points
- **Key insight**: Public items are ALWAYS kept (they're the API), so their dependencies must also be kept

- **Recommendation**: -O1 through -O3 are now safe for production use
- **Remaining work**: -O4 (trust_graph) needs more work to avoid over-deletion

### Phase A Validation: regex Crate Testing (Initial)
- **Tested**: All optimization levels (-O0 through -O4) on regex crate
- **Initial Results** (before fix):

| Level | Items Deleted | Compilation | Notes |
|-------|--------------|-------------|-------|
| `-O0` | 0 | 100% success | Conservative baseline |
| `-O1` | 4 | 0% success | Private fn deletion breaks builds |
| `-O2` | 17 | 0% success | +constants, +types |
| `-O3` | 17 | 0% success | Same as O2 (blocked crates already registry) |
| `-O4` | 44 | 0% success | trust-graph deletes most |

- **Analysis**: The optimization framework correctly applies features and deletes items
- **Root cause**: Dependency graph doesn't capture all private function references
- **Solution**: Implemented two-phase marking (see "Dependency Graph Fix" above)

### Verification Step Implementation
- **Added**: `verify_crate()` function in `src/slicer/mod.rs`
- **Behavior**: When `-fverify` is enabled and items are deleted, runs `cargo check`
- **Workspace handling**: Gracefully handles workspace membership errors
- **Timing stats**: Tracks verification time, pass/fail counts in timing breakdown
- **Results on regex**: Pass: 2, Fail: 0 (verification takes ~147ms, 45% of total)

### Feature-Aware Deletion Implementation
- **Modified**: `src/slicer/deleter.rs` - deletion now respects feature flags
- **Behavior**:
  - `-O0`: Conservative mode, keeps all items (0 deletions)
  - `-O1`: Delete unused private functions (verified: 4 items deleted on regex)
  - `-O2`: Delete all unused private items (constants, statics, type aliases)
  - `-O4`: Trust dependency graph for all items (most aggressive)
- **Wiring**: Features flow from CLI → SlicerConfig → deleter.rs
- **Tested**: regex crate shows 0 deletions with -O0, 4 deletions with -O1

### CLI Flag System Implementation
- **Added**: Complete CLI parsing for optimization levels and feature flags
- **Flags implemented**:
  - `-O0` to `-O4` optimization levels (like GCC)
  - `-f<feature>` to enable features (e.g., `-fprivate-fn`)
  - `-fno-<feature>` to disable features (e.g., `-fno-verify`)
  - `-b` shorthand for `--bench`
- **Help updated**: Both `--help` and `--help-advanced` show new options
- **Documentation**: Added AGENTS.md and GEMINI.md for multi-agent collaboration

### Feature Configuration System
- **Added**: `src/slicer/features.rs` - Unified feature configuration for slicer
- **Features**: Compiler-style flags (`-O0` to `-O4`, `-f<feature>`)
- **Purpose**: Enable graduated aggressive deletion for better LOC reduction

### Benchmark Enhancement
- **Added**: `--bench` flag with slicing overhead tracking
- **Results**:
  - Slicing overhead: ~6s (down from 20s after Phase 1 caching)
  - Build time comparison now includes end-to-end metrics
  - Shows both build-only and total (slicing + build) comparisons

### Phase 1 Optimization (7.5x speedup)
- **Before**: 20.5s for Phase 1 (75% of total time)
- **After**: ~3.6s for Phase 1
- **Changes**:
  - Added thread-local cache for `find_crate_source()` in `source_location.rs`
  - Changed `is_likely_to_bloat()` to use pre-computed `total_lines` instead of `measure_loc()`
  - Removed double file parsing in `deleter.rs`

### Bug Fixes
- **Fixed**: `pub(crate)` visibility items incorrectly deleted (e.g., `SensibleMoveMask` in memchr)
- **Fixed**: Cargo.toml features referencing missing dependencies (bumpalo, rkyv)

## January 23, 2026

### Architecture Simplification
- Migrated from SCIP-based analysis to copy-and-delete approach
- Removed dependency on rust-analyzer SCIP analysis
- New pipeline: Copy → Parse → Graph → Mark → Delete → Cleanup

## January 22, 2026

### Automatic Compatibility Fixes
- **getrandom feature rename**: `js` → `wasm_js` (v0.2 → v0.3)
- **clap version conflicts**: dev-dep v2 → registry v4 for `derive` feature
- **trybuild exact versions**: `=1.0.89` → `^1.0`

## January 21, 2026

### Self-Hosting Achievement 🎉
- **cargo-slicer successfully builds itself using its own sliced dependencies!**
- 179/179 individual sliced crates compile (100%)
- Full sliced build: 75MB binary with complete functionality
- tokei LOC measurement works correctly

### Critical Fixes
1. EOF handling for target-specific sections
2. Multi-line feature array parsing
3. Bare reference detection in features
4. Workspace exclusions for path conflicts
5. Transitive dependency conflict resolution (clap v2/v4, rayon-core links)

---

## Metrics Summary

| Date | Metric | Value |
|------|--------|-------|
| 2026-01-26 | Build warnings | 0 (down from 25) |
| 2026-01-26 | Sliced crates compiling | 153/153 (100%) |
| 2026-01-26 | LOC reduction (total) | 16.5% (sliced), 71.6% (original tree) |
| 2026-01-25 | Multi-version support | Automatic version routing |
| 2026-01-24 | Phase 1 speedup | 7.5x |
| 2026-01-24 | Slicing overhead | ~6s |
| 2026-01-21 | Sliced crates compiling | 179/179 (100%) |
| 2026-01-21 | Self-hosted binary size | 75MB |
| 2026-01-21 | Average LOC reduction | 18-47% |

## Research Paper Notes

### Key Contributions
1. **Copy-and-delete approach**: Guarantees LOC reduction (can only delete, never add)
2. **AST-based analysis**: Direct syn parsing without external tools
3. **Graduated optimization levels**: `-O0` to `-O4` like compilers
4. **Feature flags**: Fine-grained control via `-f<feature>`
5. **Self-hosting**: Tool validates itself on complex real-world project

### Future Work
- Improve dependency graph coverage (re-exports, macro expansions)
- Enable deletion of more item types while maintaining correctness
- Parallel slicing with verification

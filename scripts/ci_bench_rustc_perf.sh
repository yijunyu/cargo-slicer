#!/bin/bash
# End-to-end CI script: build binaries, benchmark all 20 rustc-perf crates
# (baseline + vslice), and generate an HTML comparison report.
#
# No arguments required. Produces:
#   - bench-results.db   (SQLite with raw measurements)
#   - perf-report.html   (self-contained HTML comparison report)
#
# Usage:
#   ./scripts/ci_bench_rustc_perf.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUNS=3
RUN_ID="ci-$(date +%Y%m%d-%H%M%S)"
DB="$PROJECT_ROOT/bench-results.db"
REPORT="$PROJECT_ROOT/perf-report.html"

export BENCH_DB="$DB"

echo "============================================"
echo "  cargo-slicer rustc-perf CI Benchmark"
echo "============================================"
echo "Run ID:  $RUN_ID"
echo "Runs:    $RUNS per crate per mode"
echo "DB:      $DB"
echo "Report:  $REPORT"
echo ""

# --- Step 1: Build binaries ---

echo "=== Step 1/4: Building cargo-slicer binaries ==="

echo "  Building release binary..."
cargo build --release --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>&1

echo "  Building driver + dispatch (nightly)..."
cargo +nightly build --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver \
  --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>&1

DRIVER="$PROJECT_ROOT/target/release-rustc/cargo-slicer-rustc"
DISPATCH="$PROJECT_ROOT/target/release-rustc/cargo_slicer_dispatch"

for bin in "$DRIVER" "$DISPATCH"; do
  if [ ! -f "$bin" ]; then
    echo "ERROR: $bin not found after build"
    exit 1
  fi
done

echo "  Binaries ready."
echo ""

# --- Step 2: Run baseline benchmarks ---

echo "=== Step 2/4: Baseline benchmarks (all 20 crates) ==="
"$SCRIPT_DIR/bench_rustc_perf.sh" "$RUN_ID" baseline "$RUNS"
echo ""

# --- Step 3: Run vslice benchmarks ---

echo "=== Step 3/4: Vslice benchmarks (all 20 crates) ==="
"$SCRIPT_DIR/bench_rustc_perf.sh" "$RUN_ID" vslice "$RUNS"
echo ""

# --- Step 4: Generate report ---

echo "=== Step 4/4: Generating HTML report ==="
python3 "$SCRIPT_DIR/bench_report.py" "$DB" --output "$REPORT"
echo ""

# --- Summary ---

echo "============================================"
echo "  CI Benchmark Complete"
echo "============================================"
echo "Run ID:  $RUN_ID"
echo "DB:      $DB"
echo "Report:  $REPORT"
echo ""
echo "View summary:"
echo "  python3 $SCRIPT_DIR/bench_db.py summary $DB $RUN_ID"
echo ""
echo "Open report:"
echo "  xdg-open $REPORT"

#!/bin/bash
# End-to-end CI script: build binaries, benchmark multi-crate projects
# (baseline + vslice-cc), and generate an HTML comparison report.
#
# Projects: ripgrep, helix, nushell, bevy, rustc, zed, slicer
#
# No arguments required. Produces:
#   - bench-results.db          (SQLite with raw measurements)
#   - multicrate-report.html    (self-contained HTML comparison report)
#
# Usage:
#   ./scripts/ci_bench_multicrate.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RUNS=3
RUN_ID="mc-$(date +%Y%m%d-%H%M%S)"
DB="$PROJECT_ROOT/bench-results.db"
REPORT="$PROJECT_ROOT/multicrate-report.html"
BENCH="$SCRIPT_DIR/bench_compare.sh"

PROJECTS=(ripgrep helix nushell bevy rustc zed slicer)
MODES=(baseline vslice-cc)

export BENCH_DB="$DB"

echo "============================================"
echo "  cargo-slicer Multi-Crate CI Benchmark"
echo "============================================"
echo "Run ID:   $RUN_ID"
echo "Projects: ${PROJECTS[*]}"
echo "Modes:    ${MODES[*]}"
echo "Runs:     $RUNS per project per mode"
echo "DB:       $DB"
echo "Report:   $REPORT"
echo ""

# --- Step 1: Build binaries ---

echo "=== Step 1/3: Building cargo-slicer binaries ==="

echo "  Building release binary..."
cargo build --release --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>&1

echo "  Building driver + dispatch (nightly)..."
cargo +nightly build --profile release-rustc \
  --bin cargo-slicer-rustc --bin cargo_slicer_dispatch \
  --features rustc-driver \
  --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>&1

DRIVER="$PROJECT_ROOT/target/release-rustc/cargo-slicer-rustc"
DISPATCH="$PROJECT_ROOT/target/release-rustc/cargo_slicer_dispatch"
SLICER="$PROJECT_ROOT/target/release/cargo-slicer"

for bin in "$DRIVER" "$DISPATCH" "$SLICER"; do
  if [ ! -f "$bin" ]; then
    echo "ERROR: $bin not found after build"
    exit 1
  fi
done

echo "  Binaries ready."
echo ""

# --- Step 2: Run benchmarks ---

TOTAL=$((${#PROJECTS[@]} * ${#MODES[@]}))
CURRENT=0

echo "=== Step 2/3: Running benchmarks ($TOTAL combinations) ==="
echo ""

for project in "${PROJECTS[@]}"; do
  for mode in "${MODES[@]}"; do
    CURRENT=$((CURRENT + 1))
    echo "=========================================="
    echo "[$CURRENT/$TOTAL] $project / $mode"
    echo "=========================================="
    "$BENCH" "$RUN_ID" "$project" "$mode" "$RUNS" || {
      echo "WARNING: $project/$mode failed, continuing..."
    }
    echo ""
  done
done

# --- Step 3: Generate report ---

echo "=== Step 3/3: Generating HTML report ==="
python3 "$SCRIPT_DIR/bench_report.py" "$DB" --output "$REPORT"
echo ""

# --- Summary ---

echo "============================================"
echo "  Multi-Crate CI Benchmark Complete"
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

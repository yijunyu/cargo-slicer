#!/usr/bin/env bash
# build-accelerate.sh — Accelerated builds for Rust AND C/C++ projects
#
# Single-command speedup for any project:
#
#   Rust projects   → cargo-slicer (dead-fn elimination) + cargo-warmup (registry cache)
#   C/C++ projects  → clang-daemon (PCH injection) + fat-header auto-detection
#
# Usage:
#   ./build-accelerate.sh                     # auto-detect project type in current dir
#   ./build-accelerate.sh /path/to/project    # explicit project directory
#   ./build-accelerate.sh . --features foo    # extra args (Rust only)
#
# One-line install + run (from the cargo-slicer repo):
#   curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash
#   # (install.sh installs both Rust and C/C++ tools; this script is also installed)
#
# ─────────────────────────────────────────────────────────────────────────────
# RUST path:   cargo-warmup → cargo-slicer pre-analyze → pch-plan → cargo build
# C/C++ path:  clang-daemon start → auto-detect fat header → make -j$(nproc) CC=clang-daemon-client
# ─────────────────────────────────────────────────────────────────────────────

set -euo pipefail

# ── Parse arguments ──────────────────────────────────────────────────────────

PROJECT_DIR="."
EXTRA_ARGS=()

if [[ $# -ge 1 && -d "$1" ]]; then
    PROJECT_DIR="$1"
    shift
fi
EXTRA_ARGS=("$@")

PROJECT_DIR="$(cd "$PROJECT_DIR" && pwd)"

# ── Detect project type ──────────────────────────────────────────────────────

IS_RUST=false
IS_C=false

if [[ -f "$PROJECT_DIR/Cargo.toml" ]]; then
    IS_RUST=true
fi

# C/C++: compile_commands.json (cmake/bear), or CMakeLists.txt / Makefile
if [[ -f "$PROJECT_DIR/compile_commands.json" ]] || \
   [[ -f "$PROJECT_DIR/CMakeLists.txt" ]] || \
   [[ -f "$PROJECT_DIR/Makefile" ]] || \
   [[ -f "$PROJECT_DIR/GNUmakefile" ]]; then
    IS_C=true
fi

if [[ "$IS_RUST" == "false" && "$IS_C" == "false" ]]; then
    echo "Error: No recognizable project found in $PROJECT_DIR" >&2
    echo "Expected: Cargo.toml (Rust) or compile_commands.json/CMakeLists.txt/Makefile (C/C++)" >&2
    exit 1
fi

# ── Detect mixed Rust+C project (build.rs compiles C/C++ via cc/cmake crate) ──
IS_MIXED=false
if [[ "$IS_RUST" == "true" && "$IS_C" == "true" ]]; then
    IS_MIXED=true
fi
# Also detect: Rust project whose build.rs uses cc/cmake crate (no top-level Makefile needed)
# Search up to 4 levels deep to catch workspace members (e.g. zstd-safe/build.rs)
if [[ "$IS_RUST" == "true" && "$IS_MIXED" == "false" ]]; then
    while IFS= read -r _brs; do
        if /usr/bin/grep -qE '"cc"|"cmake"|"cmake-rs"|cc::Build|cmake::Config' "$_brs" 2>/dev/null; then
            IS_MIXED=true
            break
        fi
    done < <(/usr/bin/find "$PROJECT_DIR" -maxdepth 4 -name build.rs 2>/dev/null)
fi

# Pure Rust if not mixed
if [[ "$IS_RUST" == "true" && "$IS_MIXED" == "false" ]]; then
    IS_C=false
fi

# ── Shared helper: locate a binary ──────────────────────────────────────────
_find_bin() {
    local name="$1"
    for d in \
        "$(dirname "$0")" \
        "${CARGO_HOME:-$HOME/.cargo}/bin" \
        "$HOME/.local/bin" \
        /usr/local/bin \
        /usr/bin; do
        if [[ -f "$d/$name" && -x "$d/$name" ]]; then
            echo "$d/$name"; return 0
        fi
    done
    command -v "$name" 2>/dev/null || true
}

# ────────────────────────────────────────────────────────────────────────────
# ── RUST PATH (pure) ─────────────────────────────────────────────────────────
# ────────────────────────────────────────────────────────────────────────────

if [[ "$IS_RUST" == "true" && "$IS_MIXED" == "false" ]]; then
    echo "=== build-accelerate: Rust project detected ==="
    echo "    Project: $PROJECT_DIR"
    echo ""

    SLICER_SH=""
    for _dir in $(echo "${PATH:-}" | tr ':' ' ') "${CARGO_HOME:-$HOME/.cargo}/bin" "$(dirname "$0")"; do
        if [[ -f "$_dir/cargo-slicer.sh" && -x "$_dir/cargo-slicer.sh" ]]; then
            SLICER_SH="$_dir/cargo-slicer.sh"
            break
        fi
    done

    if [[ -z "$SLICER_SH" ]]; then
        echo "Error: cargo-slicer.sh not found in PATH." >&2
        echo "Install: curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash" >&2
        exit 1
    fi

    exec "$SLICER_SH" "$PROJECT_DIR" "${EXTRA_ARGS[@]}"
fi

# ────────────────────────────────────────────────────────────────────────────
# ── MIXED PATH (Rust + C/C++ via build.rs) ───────────────────────────────────
# ────────────────────────────────────────────────────────────────────────────

if [[ "$IS_MIXED" == "true" ]]; then
    echo "=== build-accelerate: Mixed Rust+C/C++ project detected ==="
    echo "    Project: $PROJECT_DIR"

    # Decide whether clang-daemon overhead (~8s) is worth it for this project's C/C++ component.
    # Decision is cached in .build-accelerate-cache after the first run (measured actual time).
    # On first run: use dual static heuristic (files > 150 AND LOC > 150K).
    # On subsequent runs: use cached C build time (daemon worthwhile if > 20s).
    _CACHE_FILE="$PROJECT_DIR/.build-accelerate-cache"
    _USE_DAEMON=false

    if [[ -f "$_CACHE_FILE" ]]; then
        # Read cached C build time from previous run
        _CACHED_C_TIME=$(awk -F= '/^cc_build_time_s=/{print $2}' "$_CACHE_FILE" 2>/dev/null || echo 0)
        _CACHED_DECISION=$(awk -F= '/^use_daemon=/{print $2}' "$_CACHE_FILE" 2>/dev/null || echo "")
        if [[ -n "$_CACHED_DECISION" ]]; then
            echo "    [cache] prior C/C++ build time: ${_CACHED_C_TIME}s → daemon: $_CACHED_DECISION"
            [[ "$_CACHED_DECISION" == "true" ]] && _USE_DAEMON=true
        fi
    else
        # First run: static heuristic — files > 150 AND LOC > 150K (non-test sources)
        _CC_FILES=0
        _CC_LOC=0
        while IFS= read -r _f; do
            _CC_FILES=$(( _CC_FILES + 1 ))
            _CC_LOC=$(( _CC_LOC + $(wc -l < "$_f" 2>/dev/null || echo 0) ))
        done < <(/usr/bin/find "$PROJECT_DIR" -maxdepth 6 \
            \( -name "*.c" -o -name "*.cc" -o -name "*.cpp" -o -name "*.cxx" \) \
            ! -path "*/target/*" ! -path "*/test*" ! -path "*/bench*" \
            ! -path "*/example*" ! -path "*/doc*" 2>/dev/null | head -600)
        echo "    [heuristic] C/C++ non-test: $_CC_FILES files, $_CC_LOC LOC"

        _CC_FILES_MIN="${BUILD_ACCELERATE_MIN_CC_FILES:-150}"
        _CC_LOC_MIN="${BUILD_ACCELERATE_MIN_CC_LOC:-150000}"
        if [[ $_CC_FILES -gt $_CC_FILES_MIN && $_CC_LOC -gt $_CC_LOC_MIN ]]; then
            _USE_DAEMON=true
            echo "    [heuristic] large C/C++ component → clang-daemon enabled"
        else
            echo "    [heuristic] small C/C++ component → cargo-slicer only (daemon overhead not worth it)"
        fi
    fi

    if [[ "$_USE_DAEMON" == "false" ]]; then
        echo "    Strategy: cargo-slicer only (Rust acceleration, no C/C++ daemon)"
        IS_MIXED=false
    else
        echo "    Strategy: clang-daemon (PCH) for C/C++ in build.rs + cargo-slicer for Rust"
    fi
    echo ""

    # If LOC check flipped IS_MIXED to false, fall through to pure Rust path below
    if [[ "$IS_MIXED" == "false" ]]; then
        SLICER_SH=""
        for _dir in $(echo "${PATH:-}" | tr ':' ' ') "${CARGO_HOME:-$HOME/.cargo}/bin" "$(dirname "$0")"; do
            if [[ -f "$_dir/cargo-slicer.sh" && -x "$_dir/cargo-slicer.sh" ]]; then
                SLICER_SH="$_dir/cargo-slicer.sh"; break
            fi
        done
        [[ -z "$SLICER_SH" ]] && { echo "Error: cargo-slicer.sh not found." >&2; exit 1; }
        # Write cache so next run skips heuristic entirely
        if [[ ! -f "$_CACHE_FILE" ]]; then
            printf "# build-accelerate cache — do not edit manually\n" > "$_CACHE_FILE"
            printf "cc_build_time_s=0\n" >> "$_CACHE_FILE"
            printf "use_daemon=false\n" >> "$_CACHE_FILE"
            printf "generated=%s\n" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$_CACHE_FILE"
        fi
        exec "$SLICER_SH" "$PROJECT_DIR" "${EXTRA_ARGS[@]}"
    fi

    # Locate binaries
    DAEMON_SERVER="$(_find_bin clang-daemon-server)"
    DAEMON_CLIENT="$(_find_bin clang-daemon-client)"
    SLICER_SH=""
    for _dir in $(echo "${PATH:-}" | tr ':' ' ') "${CARGO_HOME:-$HOME/.cargo}/bin" "$(dirname "$0")"; do
        if [[ -f "$_dir/cargo-slicer.sh" && -x "$_dir/cargo-slicer.sh" ]]; then
            SLICER_SH="$_dir/cargo-slicer.sh"
            break
        fi
    done

    if [[ -z "$DAEMON_SERVER" || -z "$DAEMON_CLIENT" ]]; then
        echo "Warning: clang-daemon not found — C/C++ build.rs will use plain clang." >&2
        DAEMON_CLIENT=""
    fi
    if [[ -z "$SLICER_SH" ]]; then
        echo "Error: cargo-slicer.sh not found in PATH." >&2
        exit 1
    fi

    # Detect C++ compiler
    CLANG_BIN=""
    for _c in clang++ clang++-21 clang++-20 clang++-19 clang++-18 clang++-17 g++; do
        if command -v "$_c" &>/dev/null; then
            CLANG_BIN="$(command -v "$_c")"; break
        fi
    done

    if [[ -n "$DAEMON_CLIENT" && -n "$CLANG_BIN" ]]; then
        # ── Auto-detect fat header from build.rs C++ sources ─────────────────
        echo "=== Step 1/3: Auto-detecting fat header for C/C++ components ==="

        FAT_HDR="${CLANG_DAEMON_FAT_HDR:-}"
        if [[ -z "$FAT_HDR" ]]; then
            # Look for C/C++ sources referenced in the project (vendor/, src/, third-party/)
            _SAMPLE_DIRS=()
            for _d in "$PROJECT_DIR/vendor" "$PROJECT_DIR/third-party" \
                      "$PROJECT_DIR/src" "$PROJECT_DIR/csrc" "$PROJECT_DIR/cpp"; do
                [[ -d "$_d" ]] && _SAMPLE_DIRS+=("$_d")
            done
            [[ ${#_SAMPLE_DIRS[@]} -eq 0 ]] && _SAMPLE_DIRS=("$PROJECT_DIR")

            if command -v python3 &>/dev/null; then
                FAT_HDR="$(python3 - "${_SAMPLE_DIRS[@]}" <<'PYEOF'
import sys, os, re
from collections import Counter
dirs = sys.argv[1:]
include_re = re.compile(r'^\s*#include\s*[<"](.*?)[>"]', re.MULTILINE)
counts = Counter()
sampled = 0
for d in dirs:
    for root, _, files in os.walk(d):
        for fn in files:
            if not fn.endswith(('.c', '.cc', '.cpp', '.cxx', '.h', '.hpp')):
                continue
            try:
                content = open(os.path.join(root, fn), errors='ignore').read()
                for m in include_re.finditer(content):
                    h = m.group(1)
                    if not h.endswith(('.inc', '.def')):
                        counts[h] += 1
                sampled += 1
            except OSError:
                pass
            if sampled >= 300:
                break
        if sampled >= 300:
            break
top = [h for h, _ in counts.most_common(5)]
print('\n'.join(f'#include <{h}>' for h in top))
PYEOF
)" 2>/dev/null || FAT_HDR=""
            fi

            # Fallbacks
            if [[ -z "$FAT_HDR" ]]; then
                FAT_HDR='#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <algorithm>
'
                echo "    Heuristic: generic C++ stdlib fat header"
            else
                echo "    Fat header from source scan:"
            fi
        fi

        export CLANG_DAEMON_FAT_HDR="$FAT_HDR"
        echo "$FAT_HDR" | sed 's/^/      /'

        # ── Start daemon ─────────────────────────────────────────────────────
        SOCKET="${CLANG_DAEMON_SOCKET:-/tmp/build-accelerate-$(basename "$PROJECT_DIR").sock}"
        echo ""
        echo "=== Step 2/3: Starting clang-daemon for build.rs C/C++ compilation ==="
        echo "    Socket: $SOCKET"

        pkill -f "clang-daemon-server.*$SOCKET" 2>/dev/null || true
        sleep 0.2
        rm -f "$SOCKET"

        "$DAEMON_SERVER" --daemon --socket "$SOCKET" \
            2>/dev/null &
        DAEMON_PID=$!
        trap 'kill $DAEMON_PID 2>/dev/null || true; rm -f "$SOCKET"' EXIT

        for _i in $(seq 1 30); do
            [[ -S "$SOCKET" ]] && break
            sleep 0.2
        done

        export CLANG_DAEMON_SOCKET="$SOCKET"
        export CLANG_DAEMON_CLANG="$CLANG_BIN"

        # Expose daemon client as CC/CXX so build.rs (cc crate) uses it
        export CC="$DAEMON_CLIENT"
        export CXX="$DAEMON_CLIENT"
        # cmake crate reads CMAKE_C_COMPILER / CMAKE_CXX_COMPILER from env
        export CMAKE_C_COMPILER="$DAEMON_CLIENT"
        export CMAKE_CXX_COMPILER="$DAEMON_CLIENT"

        echo "    CC/CXX → $DAEMON_CLIENT (clang-daemon, PCH-accelerated)"
    else
        echo "    (clang-daemon unavailable — C/C++ build.rs will use default compiler)"
    fi

    echo ""
    echo "=== Step 3/3: Building with cargo-slicer (Rust) + clang-daemon (C/C++) ==="

    # Time the build to populate cache for next run
    _BUILD_T0=$(date +%s)
    "$SLICER_SH" "$PROJECT_DIR" "${EXTRA_ARGS[@]}"
    _BUILD_STATUS=$?
    _BUILD_T1=$(date +%s)
    _BUILD_TIME=$(( _BUILD_T1 - _BUILD_T0 ))

    # Write cache: record measured C build time and daemon decision
    # C build time ≈ total build time when daemon is active and Rust is fast
    # Use actual wall time as proxy; user can override with BUILD_ACCELERATE_MIN_CC_LOC
    if [[ ! -f "$_CACHE_FILE" ]]; then
        # Estimate C time as fraction of total (C dominates in mixed builds)
        printf "# build-accelerate cache — do not edit manually\n" > "$_CACHE_FILE"
        printf "cc_build_time_s=%d\n" "$_BUILD_TIME" >> "$_CACHE_FILE"
        printf "use_daemon=%s\n" "$_USE_DAEMON" >> "$_CACHE_FILE"
        printf "generated=%s\n" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$_CACHE_FILE"
        echo "    [cache] written to $_CACHE_FILE (next run will skip heuristic)"
    fi
    exit $_BUILD_STATUS
fi

# ────────────────────────────────────────────────────────────────────────────
# ── C/C++ PATH ───────────────────────────────────────────────────────────────
# ────────────────────────────────────────────────────────────────────────────

echo "=== build-accelerate: C/C++ project detected ==="
echo "    Project: $PROJECT_DIR"
echo ""

# ── Locate binaries (already defined above, reuse _find_bin) ────────────────

DAEMON_SERVER="$(_find_bin clang-daemon-server)"
DAEMON_CLIENT="$(_find_bin clang-daemon-client)"

if [[ -z "$DAEMON_SERVER" || -z "$DAEMON_CLIENT" ]]; then
    echo "Error: clang-daemon-server / clang-daemon-client not found." >&2
    echo "Install: curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash" >&2
    exit 1
fi

# ── Detect compiler ─────────────────────────────────────────────────────────

CLANG_BIN=""
for _c in clang++ clang++-21 clang++-20 clang++-19 clang++-18 clang++-17; do
    if command -v "$_c" &>/dev/null; then
        CLANG_BIN="$(command -v "$_c")"
        break
    fi
done
if [[ -z "$CLANG_BIN" ]]; then
    # Fall back to GCC if no clang available
    for _c in g++ gcc; do
        if command -v "$_c" &>/dev/null; then
            CLANG_BIN="$(command -v "$_c")"
            break
        fi
    done
fi
if [[ -z "$CLANG_BIN" ]]; then
    echo "Error: No C++ compiler found (clang++ or g++ required)" >&2
    exit 1
fi
echo "    Compiler: $CLANG_BIN"

# ── Auto-detect fat header ────────────────────────────────────────────────────
# Strategy:
#   1. If CLANG_DAEMON_FAT_HDR is already set, use it (user override)
#   2. Look for compile_commands.json → extract top-5 most-included headers
#   3. Heuristics: Linux kernel (linux/module.h), LLVM, Qt, generic
#
# The fat header content (not a path) must be exported so the server sees it.

if [[ -z "${CLANG_DAEMON_FAT_HDR:-}" ]]; then
    echo "=== Step 1/3: Auto-detecting fat header ==="

    FAT_HDR=""

    # Compile-commands based: find most-included headers
    CC_JSON=""
    for _f in \
        "$PROJECT_DIR/compile_commands.json" \
        "$PROJECT_DIR/build/compile_commands.json" \
        "$PROJECT_DIR/cmake-build-release/compile_commands.json" \
        "$PROJECT_DIR/cmake-build-debug/compile_commands.json"; do
        if [[ -f "$_f" ]]; then
            CC_JSON="$_f"; break
        fi
    done

    if [[ -n "$CC_JSON" ]] && command -v python3 &>/dev/null; then
        # Count #include occurrences across a sample of 200 source files
        FAT_HDRS_FOUND="$(python3 - "$CC_JSON" "$PROJECT_DIR" <<'PYEOF'
import json, re, sys, os
from collections import Counter
db_path, src_root = sys.argv[1], sys.argv[2]
with open(db_path) as f:
    entries = json.load(f)
include_re = re.compile(r'^\s*#include\s*[<"](.*?)[>"]', re.MULTILINE)
counts = Counter()
sampled = 0
for e in entries[:500]:  # sample first 500 TUs
    src = e.get("file", "")
    if not src.startswith("/"):
        src = os.path.join(e.get("directory", src_root), src)
    try:
        content = open(src, errors='ignore').read()
        for m in include_re.finditer(content):
            h = m.group(1)
            if not h.endswith(".inc") and not h.endswith(".def"):
                counts[h] += 1
        sampled += 1
    except OSError:
        pass
# Print top 10 headers
for hdr, cnt in counts.most_common(10):
    print(f"{cnt} {hdr}")
PYEOF
)" 2>/dev/null || FAT_HDRS_FOUND=""

        if [[ -n "$FAT_HDRS_FOUND" ]]; then
            echo "    Top headers in project:"
            echo "$FAT_HDRS_FOUND" | head -10 | sed 's/^/      /'

            # Build fat header from top-5
            FAT_HDR=""
            while IFS=' ' read -r _count _hdr; do
                FAT_HDR+="#include <${_hdr}>"$'\n'
            done < <(echo "$FAT_HDRS_FOUND" | head -5)
        fi
    fi

    # Keyword-based fallbacks if compile_commands gave nothing useful
    if [[ -z "$FAT_HDR" ]]; then
        # Linux kernel?
        if find "$PROJECT_DIR" -maxdepth 3 -name "*.c" 2>/dev/null | \
           xargs grep -l 'linux/module.h' 2>/dev/null | head -1 | grep -q .; then
            FAT_HDR='#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/slab.h>
#include <linux/errno.h>
'
            echo "    Heuristic: Linux kernel project → kernel fat header"
        # LLVM/Clang?
        elif find "$PROJECT_DIR" -maxdepth 3 \( -name "*.cpp" -o -name "*.h" \) 2>/dev/null | \
             xargs grep -l 'llvm/Support' 2>/dev/null | head -1 | grep -q .; then
            FAT_HDR='#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/SmallVector.h"
'
            echo "    Heuristic: LLVM project → LLVM fat header"
        # Qt?
        elif find "$PROJECT_DIR" -maxdepth 3 \( -name "*.cpp" -o -name "*.h" \) 2>/dev/null | \
             xargs grep -l '#include <Q' 2>/dev/null | head -1 | grep -q .; then
            FAT_HDR='#include <QObject>
#include <QString>
#include <QVector>
#include <QList>
#include <QMap>
'
            echo "    Heuristic: Qt project → Qt fat header"
        else
            # Generic C++ stdlib
            FAT_HDR='#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <algorithm>
'
            echo "    Heuristic: generic C++ project → stdlib fat header"
        fi
    fi

    export CLANG_DAEMON_FAT_HDR="$FAT_HDR"
    echo "    Fat header:"
    echo "$FAT_HDR" | sed 's/^/      /'
else
    echo "=== Step 1/3: Using provided CLANG_DAEMON_FAT_HDR ==="
fi

# ── Start daemon ─────────────────────────────────────────────────────────────

SOCKET="${CLANG_DAEMON_SOCKET:-/tmp/build-accelerate-$(basename "$PROJECT_DIR").sock}"

echo ""
echo "=== Step 2/3: Starting clang-daemon ==="
echo "    Socket:   $SOCKET"

# Kill any stale daemon on this socket
pkill -f "clang-daemon-server.*$SOCKET" 2>/dev/null || true
sleep 0.2
rm -f "$SOCKET"

"$DAEMON_SERVER" --daemon --socket "$SOCKET" &
DAEMON_PID=$!

# Wait for socket to appear
for _i in $(seq 1 30); do
    [[ -S "$SOCKET" ]] && break
    sleep 0.2
done

export CLANG_DAEMON_SOCKET="$SOCKET"
export CLANG_DAEMON_CLANG="$CLANG_BIN"

# Trigger PCH build (probe compile from first TU)
echo "    Triggering PCH compilation ..."
if [[ -n "$CC_JSON" ]] && command -v python3 &>/dev/null; then
    _PROBE_CMD="$(python3 -c "
import json, sys
with open('$CC_JSON') as f:
    e = json.load(f)[0]
import re
tokens = e['command'].split()
tokens[0] = '$DAEMON_CLIENT'
clean, skip = [], False
for t in tokens:
    if skip: skip = False; continue
    if t == '-o': skip = True; continue
    if t.startswith('-o') and len(t) > 2: continue
    clean.append(t)
clean += ['-o', '/dev/null']
print(' '.join(repr(x) for x in clean))
" 2>/dev/null || true)"
    if [[ -n "$_PROBE_CMD" ]]; then
        eval "$DAEMON_CLIENT $_PROBE_CMD" 2>/dev/null || true
    fi
fi

# Wait for PCH file to appear
for _i in $(seq 1 60); do
    if ls /tmp/clang-daemon-fat-hdr-*.h.gch 2>/dev/null | head -1 | grep -q .; then
        PCH_FILE="$(ls /tmp/clang-daemon-fat-hdr-*.h.gch 2>/dev/null | head -1)"
        echo "    PCH ready: $PCH_FILE"
        break
    fi
    sleep 0.5
done

# Cleanup daemon on exit
trap 'echo ""; echo "Stopping daemon..."; kill $DAEMON_PID 2>/dev/null || true; rm -f "$SOCKET"' EXIT

# ── Build ─────────────────────────────────────────────────────────────────────

JOBS="${BUILD_JOBS:-$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 8)}"

echo ""
echo "=== Step 3/3: Building with daemon-accelerated compiler (-j$JOBS) ==="
echo "    CC/CXX = $DAEMON_CLIENT (routes to daemon → clang + PCH)"
echo ""

cd "$PROJECT_DIR"

BUILD_CMD=()

if [[ -f "$PROJECT_DIR/compile_commands.json" ]] && command -v ninja &>/dev/null && \
   [[ -f "$(dirname "$CC_JSON")/build.ninja" ]]; then
    # Ninja build (cmake typically generates this)
    BUILD_DIR="$(dirname "$CC_JSON")"
    echo "    Build system: ninja in $BUILD_DIR"
    BUILD_CMD=(ninja -C "$BUILD_DIR" -j"$JOBS")
elif [[ -f "$PROJECT_DIR/CMakeLists.txt" ]]; then
    # CMake: configure if build dir missing, then build
    BUILD_DIR="$PROJECT_DIR/build"
    if [[ ! -f "$BUILD_DIR/CMakeCache.txt" ]]; then
        echo "    Configuring CMake build..."
        cmake -S "$PROJECT_DIR" -B "$BUILD_DIR" \
            -DCMAKE_C_COMPILER="$DAEMON_CLIENT" \
            -DCMAKE_CXX_COMPILER="$DAEMON_CLIENT" \
            -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
            2>&1
    fi
    BUILD_CMD=(cmake --build "$BUILD_DIR" -- -j"$JOBS")
elif [[ -f "$PROJECT_DIR/Makefile" ]] || [[ -f "$PROJECT_DIR/GNUmakefile" ]]; then
    BUILD_CMD=(make -j"$JOBS" CC="$DAEMON_CLIENT" CXX="$DAEMON_CLIENT")
else
    echo "Error: No supported build system found (ninja/cmake/make)" >&2
    exit 1
fi

time "${BUILD_CMD[@]}" 2>&1

echo ""
echo "=== Done ==="
echo "Project:  $PROJECT_DIR"
echo "Compiler: $CLANG_BIN (via daemon, PCH-accelerated)"
echo "Socket:   $SOCKET"

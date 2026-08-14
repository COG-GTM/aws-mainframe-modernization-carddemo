#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# CardDemo unit test runner (off-mainframe).
#
# Compiles the COBOL programs under test with GnuCOBOL 3.2+ coverage
# instrumentation, runs the unit tests in tests/cobol/ and reports a
# line coverage percentage per program and in total (via gcov).
#
# Requirements: GnuCOBOL >= 3.2 (cobc/cobcrun) and gcov.
# Usage:        tests/run_tests.sh
# Exit code:    0 when all tests pass, non-zero otherwise.
#
# On z/OS the same test programs are compiled and executed as batch jobs
# with the JCL in tests/jcl/ instead of this script. See docs/TESTING.md.
# ---------------------------------------------------------------------------
set -u

TESTS_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(dirname "$TESTS_DIR")"
APP_CBL="$REPO_ROOT/app/cbl"
APP_CPY="$REPO_ROOT/app/cpy"
TST_CPY="$TESTS_DIR/cpy"
BUILD="$TESTS_DIR/.build"
DATA="$BUILD/data"
COVDIR="$BUILD/coverage"

COBC="${COBC:-cobc}"
COBCRUN="${COBCRUN:-cobcrun}"
GCOV="${GCOV:-gcov}"

if ! command -v "$COBC" >/dev/null 2>&1; then
    echo "ERROR: cobc (GnuCOBOL) not found. Install GnuCOBOL 3.2+." >&2
    exit 2
fi
if ! "$COBC" --help 2>/dev/null | grep -q -- --coverage; then
    echo "WARNING: this cobc does not support --coverage (GnuCOBOL < 3.2)."
    echo "         Tests will run but no coverage will be reported."
    COVERAGE_FLAG=""
else
    COVERAGE_FLAG="--coverage"
fi

rm -rf "$BUILD"
mkdir -p "$BUILD" "$DATA" "$COVDIR"

# Common compile flags: copybook search paths, uppercase copybook names
# and IBM semantics for ASSIGN TO <name>: the name is an external DD name
# (resolved through DD_* environment variables), exactly as on z/OS.
CFLAGS=(-I "$APP_CPY" -I "$TST_CPY" -ffold-copy=UPPER
        -fassign-clause=external)

echo "=== Compiling programs under test (with coverage) ==="
# Programs under test are compiled as dynamically loadable modules (-m)
# with coverage instrumentation so gcov can measure executed lines.
(
    cd "$BUILD" || exit 2
    "$COBC" -m $COVERAGE_FLAG "${CFLAGS[@]}" "$APP_CBL/CSUTLDTC.cbl" || exit 2
    "$COBC" -m $COVERAGE_FLAG "${CFLAGS[@]}" "$APP_CBL/CBSTM03B.CBL" || exit 2
) || exit 2

echo "=== Compiling stubs, fixtures and test drivers ==="
(
    cd "$BUILD" || exit 2
    # LE CEEDAYS stub: local replacement for the z/OS LE date service.
    "$COBC" -m "${CFLAGS[@]}" "$TESTS_DIR/stubs/CEEDAYS.cbl" || exit 2
    # Fixture that creates the indexed test files for CBSTM03B.
    "$COBC" -m "${CFLAGS[@]}" "$TESTS_DIR/fixtures/STMFIXTC.cbl" || exit 2
    # Test drivers. TSTDTE1C compiles the CSUTLDPY copybook logic into
    # itself, so it is also instrumented to measure copybook coverage.
    "$COBC" -m "${CFLAGS[@]}" "$TESTS_DIR/cobol/TSTDTC1C.cbl" || exit 2
    "$COBC" -m $COVERAGE_FLAG "${CFLAGS[@]}" \
            "$TESTS_DIR/cobol/TSTDTE1C.cbl" || exit 2
    "$COBC" -m "${CFLAGS[@]}" "$TESTS_DIR/cobol/TSTSTM1C.cbl" || exit 2
) || exit 2

export COB_LIBRARY_PATH="$BUILD"

# GnuCOBOL resolves ASSIGN TO <word> through DD_<word> environment
# variables -- the local equivalent of the DD statements in JCL.
export DD_TRNXFILE="$DATA/trnxfile.idx"
export DD_XREFFILE="$DATA/xreffile.idx"
export DD_CUSTFILE="$DATA/custfile.idx"
export DD_ACCTFILE="$DATA/acctfile.idx"

echo
echo "=== Creating test fixtures ==="
"$COBCRUN" STMFIXTC || { echo "ERROR: fixture creation failed"; exit 2; }

echo
echo "=== Running unit tests ==="
FAILED_SUITES=0
for TEST in TSTDTC1C TSTDTE1C TSTSTM1C; do
    echo
    echo "--- $TEST ---"
    (cd "$BUILD" && "$COBCRUN" "$TEST")
    RC=$?
    if [ "$RC" -ne 0 ]; then
        echo "$TEST: FAILED (rc=$RC)"
        FAILED_SUITES=$((FAILED_SUITES + 1))
    fi
done

if [ -n "$COVERAGE_FLAG" ]; then
    echo
    echo "=== Coverage report (gcov line coverage) ==="
    COV_TXT="$COVDIR/coverage.txt"
    : > "$COV_TXT"
    (
        cd "$BUILD" || exit 0
        for GCDA in *.gcda; do
            [ -e "$GCDA" ] || continue
            "$GCOV" "$GCDA" >/dev/null 2>&1
        done
    )
    # gcov writes one .gcov per source; keep those for the COBOL sources
    # under test and extract executed/total line counts.
    TOTAL_EXEC=0
    TOTAL_LINES=0
    for SRC in CSUTLDTC.cbl CBSTM03B.CBL CSUTLDPY.cpy; do
        GCOV_FILE="$BUILD/$SRC.gcov"
        if [ ! -f "$GCOV_FILE" ]; then
            echo "  $SRC: no coverage data"
            continue
        fi
        cp "$GCOV_FILE" "$COVDIR/"
        EXEC=$(awk -F: '$1 !~ /-/ && $1 !~ /#/ {n++} END {print n+0}' \
               "$GCOV_FILE")
        MISS=$(awk -F: '$1 ~ /#/ {n++} END {print n+0}' "$GCOV_FILE")
        LINES=$((EXEC + MISS))
        if [ "$LINES" -gt 0 ]; then
            PCT=$(awk -v e="$EXEC" -v l="$LINES" \
                  'BEGIN {printf "%.1f", (e/l)*100}')
        else
            PCT="0.0"
        fi
        printf '  %-14s %6s%%  (%s of %s executable lines)\n' \
               "$SRC" "$PCT" "$EXEC" "$LINES" | tee -a "$COV_TXT"
        TOTAL_EXEC=$((TOTAL_EXEC + EXEC))
        TOTAL_LINES=$((TOTAL_LINES + LINES))
    done
    if [ "$TOTAL_LINES" -gt 0 ]; then
        TOTAL_PCT=$(awk -v e="$TOTAL_EXEC" -v l="$TOTAL_LINES" \
                    'BEGIN {printf "%.1f", (e/l)*100}')
        echo "  --------------------------------------------------"
        printf '  %-14s %6s%%  (%s of %s executable lines)\n' \
               "TOTAL" "$TOTAL_PCT" "$TOTAL_EXEC" "$TOTAL_LINES" \
               | tee -a "$COV_TXT"
        echo "COVERAGE_TOTAL_PERCENT=$TOTAL_PCT" >> "$COV_TXT"
        echo
        echo "Coverage details: $COVDIR"
    fi
fi

echo
if [ "$FAILED_SUITES" -ne 0 ]; then
    echo "RESULT: $FAILED_SUITES test suite(s) FAILED"
    exit 1
fi
echo "RESULT: all test suites passed"
exit 0

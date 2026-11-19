#!/usr/bin/env bash
# Compile CSUTLDTC + the CEEDAYS test double + TESTDTC driver and run it.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
TESTS="$(dirname "$HERE")"
REPO="$(dirname "$TESTS")"
BUILD="${CARDDEMO_TEST_BUILD:-$TESTS/.build}"
mkdir -p "$BUILD"
cobc -x -std=ibm -fsign=EBCDIC -ftab-width=4 -w -I "$REPO/app/cpy" \
     -o "$BUILD/testdtc" \
     "$HERE/TESTDTC.cbl" "$REPO/app/cbl/CSUTLDTC.cbl" \
     "$TESTS/lib/stubs/CEEDAYS.c"
"$BUILD/testdtc"

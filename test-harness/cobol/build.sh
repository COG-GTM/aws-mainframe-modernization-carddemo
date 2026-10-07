#!/usr/bin/env bash
# Compile CBACT01C and CBTRN01C (unmodified, from app/cbl) together with
# the stubs/loader in this directory using GnuCOBOL.
#
#   cobc -std=ibm -I app/cpy            as required by the repo
#   -fsign=EBCDIC   the ASCII sample data carries EBCDIC-style zoned
#                   overpunch signs ({ } A-I J-R); this makes GnuCOBOL
#                   read and write exactly that representation.
#   (default -std=ibm initialisation: WORKING-STORAGE PIC X without VALUE
#   is space filled, which is what the unwritten tail of CODATECN-0UT-DATE
#   relies on - see TEST_STRATEGY.md, "Reissue date".)
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
BIN="${BIN:-$HERE/bin}"
mkdir -p "$BIN"

COBC_FLAGS=(-std=ibm -I "$REPO/app/cpy" -fsign=EBCDIC -O)

echo "== cobc $(cobc --version | head -1)"
echo "== building stubs (COBDATFT, CEE3ABD) as modules"
cobc -m "${COBC_FLAGS[@]}" -o "$BIN/COBDATFT.so" "$HERE/COBDATFT.cbl"
cobc -m "${COBC_FLAGS[@]}" -o "$BIN/CEE3ABD.so"  "$HERE/CEE3ABD.cbl"
echo "== building KSDSLOAD"
cobc -x "${COBC_FLAGS[@]}" -o "$BIN/KSDSLOAD" "$HERE/KSDSLOAD.cbl"
echo "== building CBACT01C and CBTRN01C from app/cbl (unmodified)"
cobc -x "${COBC_FLAGS[@]}" -o "$BIN/CBACT01C" "$REPO/app/cbl/CBACT01C.cbl"
cobc -x "${COBC_FLAGS[@]}" -o "$BIN/CBTRN01C" "$REPO/app/cbl/CBTRN01C.cbl"
echo "== done: $(ls "$BIN")"

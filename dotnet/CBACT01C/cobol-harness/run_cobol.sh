#!/usr/bin/env bash
# Compile and run the ORIGINAL app/cbl/CBACT01C.cbl with GnuCOBOL.
# Usage: run_cobol.sh <acctdata.txt> <output-dir>
# Produces: stdout.txt, OUTFILE, ARRYFILE, VBRCFILE in <output-dir>.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../.." && pwd)"
INPUT="$(realpath "$1")"
OUT="$(mkdir -p "$2" && realpath "$2")"
BUILD="$OUT/.build"
mkdir -p "$BUILD"
FLAGS=(-std=ibm -fsign=EBCDIC -I "$REPO/app/cpy")

cobc -x "${FLAGS[@]}" -o "$BUILD/loadacct" "$HERE/LOADACCT.cbl"
cobc -x "${FLAGS[@]}" -o "$BUILD/cbact01c" "$REPO/app/cbl/CBACT01C.cbl" \
     "$HERE/COBDATFT.cbl" "$HERE/CEE3ABD.cbl"

cd "$OUT"
rm -f ACCTFILE* OUTFILE ARRYFILE VBRCFILE
ACCTSEQ="$INPUT" ACCTFILE="$OUT/ACCTFILE" "$BUILD/loadacct" >&2
ACCTFILE="$OUT/ACCTFILE" OUTFILE="$OUT/OUTFILE" ARRYFILE="$OUT/ARRYFILE" \
VBRCFILE="$OUT/VBRCFILE" "$BUILD/cbact01c" > "$OUT/stdout.txt"
rm -rf "$BUILD" ACCTFILE*
echo "GnuCOBOL run complete -> $OUT" >&2

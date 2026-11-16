#!/usr/bin/env bash
# Regenerates the POSTTRAN (CBTRN02C) golden outputs by running the original
# app/cbl/CBTRN02C.cbl under GnuCOBOL against the ASCII samples in app/data/ASCII.
#
#   java-batch/parity/posttran/run-golden.sh [out-dir]
#
# Mirrors app/jcl/POSTTRAN.jcl: XREFFILE/ACCTFILE/TCATBALF are loaded into
# GnuCOBOL indexed files (KSDS), TRANFILE starts empty, DALYTRAN is read
# sequentially and DALYREJS is written as fixed 430-byte records.
# Outputs are one fixed-length record per line; TRAN-PROC-TS (TRANSACT
# offset 304, 26 bytes) is the wall clock and is masked with '#'.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
OUT="${1:-$ROOT/java-batch/src/test/resources/golden/posttran}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$OUT"

# The ASCII samples carry EBCDIC-style zoned-decimal sign overpunches
# ({, A-I positive; }, J-R negative), so -fsign=EBCDIC is required.
COBC_FLAGS=(-std=ibm -fsign=EBCDIC)

build_ksds_tool() { # name reclen keylen
  sed -e "s/@PGM@/$1/" -e "s/@LEN@/$2/" -e "s/@KEYLEN@/$3/" \
      -e "s/@DATALEN@/$(($2 - $3))/" "$HERE/ksdsload.cbl.tmpl" > "$WORK/$1.cbl"
  cobc -x "${COBC_FLAGS[@]}" -o "$WORK/$1" "$WORK/$1.cbl"
}
ksds() { MODE="$2" DD_SEQFILE="$3" DD_KSDSFILE="$4" "$WORK/$1" > /dev/null; }

python3 "$HERE/seqdata.py" prep "$ROOT" "$WORK"

build_ksds_tool KSDSTRAN 350 16
build_ksds_tool KSDSXREF 50 16
build_ksds_tool KSDSACCT 300 11
build_ksds_tool KSDSTCAT 50 17

: > "$WORK/empty.dat"
ksds KSDSTRAN LOAD "$WORK/empty.dat"    "$WORK/transact.ksds"
ksds KSDSXREF LOAD "$WORK/cardxref.dat" "$WORK/cardxref.ksds"
ksds KSDSACCT LOAD "$WORK/acctdata.dat" "$WORK/acctdata.ksds"
ksds KSDSTCAT LOAD "$WORK/tcatbal.dat"  "$WORK/tcatbal.ksds"

cobc -x "${COBC_FLAGS[@]}" -I "$ROOT/app/cpy" -o "$WORK/CBTRN02C" \
     "$ROOT/app/cbl/CBTRN02C.cbl" "$HERE/CEE3ABD.cbl"

set +e
DD_DALYTRAN="$WORK/dalytran.dat" \
DD_TRANFILE="$WORK/transact.ksds" \
DD_XREFFILE="$WORK/cardxref.ksds" \
DD_DALYREJS="$WORK/dalyrejs.dat" \
DD_ACCTFILE="$WORK/acctdata.ksds" \
DD_TCATBALF="$WORK/tcatbal.ksds" \
  "$WORK/CBTRN02C" > "$OUT/SYSOUT.txt" 2>&1
RC=$?
set -e
echo "$RC" > "$OUT/RETURN-CODE.txt"
sed -i 's/[[:space:]]*$//' "$OUT/SYSOUT.txt"

ksds KSDSTRAN UNLOAD "$WORK/transact.out" "$WORK/transact.ksds"
ksds KSDSACCT UNLOAD "$WORK/acctdata.out" "$WORK/acctdata.ksds"
ksds KSDSTCAT UNLOAD "$WORK/tcatbal.out"  "$WORK/tcatbal.ksds"

python3 "$HERE/seqdata.py" lines "$WORK/transact.out" 350 "$OUT/TRANSACT.txt" 304 26
python3 "$HERE/seqdata.py" lines "$WORK/acctdata.out" 300 "$OUT/ACCTFILE.txt"
python3 "$HERE/seqdata.py" lines "$WORK/tcatbal.out"  50  "$OUT/TCATBALF.txt"
python3 "$HERE/seqdata.py" lines "$WORK/dalyrejs.dat" 430 "$OUT/DALYREJS.txt"

echo "CBTRN02C RC=$RC"; grep -E "PROCESSED|REJECTED" "$OUT/SYSOUT.txt"
wc -l "$OUT"/*.txt

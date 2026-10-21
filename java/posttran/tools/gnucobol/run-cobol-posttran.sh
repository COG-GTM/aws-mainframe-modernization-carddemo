#!/usr/bin/env bash
# Runs the original app/cbl/CBTRN02C.cbl under GnuCOBOL (3.1+) against the
# shipped seed data and writes golden-master outputs for the Java port.
#
#   tools/gnucobol/run-cobol-posttran.sh [ascii|ebcdic] [out-dir]
#
# Mirrors the POSTTRAN JCL: TRANFILE/XREFFILE/ACCTFILE/TCATBALF are KSDS
# (GnuCOBOL indexed files), DALYTRAN is read sequentially, DALYREJS is written
# as fixed 430-byte records. TRANFILE starts empty (TRANFILE.jcl seeds it with a
# single all-LOW-VALUES placeholder record, DALYTRAN.PS.INIT, which is omitted).
# TRAN-PROC-TS (cols 305-330) is the wall clock, so it is masked with '#'.
set -euo pipefail

DATASET="${1:-ascii}"
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../../.." && pwd)"
OUT="${2:-$HERE/../../src/test/resources/golden/$DATASET}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$OUT"

COBC_FLAGS=(-std=ibm -fsign=EBCDIC)

build_ksds_tool() { # name reclen keylen
  sed -e "s/@PGM@/$1/" -e "s/@LEN@/$2/" -e "s/@KEYLEN@/$3/" \
      -e "s/@DATALEN@/$(($2 - $3))/" "$HERE/ksdsload.cbl.tmpl" > "$WORK/$1.cbl"
  cobc -x "${COBC_FLAGS[@]}" -o "$WORK/$1" "$WORK/$1.cbl"
}

ksds() { # tool mode seqfile ksdsfile
  MODE="$2" DD_SEQFILE="$3" DD_KSDSFILE="$4" "$WORK/$1"
}

python3 "$HERE/seqdata.py" prep "$DATASET" "$ROOT" "$WORK"

build_ksds_tool KSDSTRAN 350 16
build_ksds_tool KSDSXREF 50 16
build_ksds_tool KSDSACCT 300 11
build_ksds_tool KSDSTCAT 50 17

: > "$WORK/empty.dat"
ksds KSDSTRAN LOAD "$WORK/empty.dat"   "$WORK/transact.ksds"
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
  "$WORK/CBTRN02C" > "$OUT/sysout.txt" 2>&1
RC=$?
set -e
echo "$RC" > "$OUT/return-code.txt"
sed -i 's/[[:space:]]*$//' "$OUT/sysout.txt"

ksds KSDSTRAN UNLOAD "$WORK/transact.out" "$WORK/transact.ksds"
ksds KSDSACCT UNLOAD "$WORK/acctdata.out" "$WORK/acctdata.ksds"
ksds KSDSTCAT UNLOAD "$WORK/tcatbal.out"  "$WORK/tcatbal.ksds"

# Fail loudly if CBTRN02C ever stops emitting DB2-format timestamps.
python3 - "$WORK/transact.out" <<'PY'
import re, sys
d = open(sys.argv[1], 'rb').read()
for i in range(0, len(d), 350):
    ts = d[i + 304:i + 330].decode()
    assert re.fullmatch(r"\d{4}-\d{2}-\d{2}-\d{2}\.\d{2}\.\d{2}\.\d{2}0000", ts), ts
PY

python3 "$HERE/seqdata.py" lines "$WORK/transact.out" 350 "$OUT/transact.txt" 304 26
python3 "$HERE/seqdata.py" lines "$WORK/acctdata.out" 300 "$OUT/acctdata.txt"
python3 "$HERE/seqdata.py" lines "$WORK/tcatbal.out"  50  "$OUT/tcatbal.txt"
python3 "$HERE/seqdata.py" lines "$WORK/dalyrejs.dat" 430 "$OUT/dalyrejs.txt"

echo "CBTRN02C RC=$RC"; cat "$OUT/sysout.txt" | grep -E "PROCESSED|REJECTED"
wc -l "$OUT"/*.txt

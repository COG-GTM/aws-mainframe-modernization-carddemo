#!/usr/bin/env bash
# Regenerates src/test/resources/golden from the original COBOL using GnuCOBOL. Run from the repo root.
set -euo pipefail
ROOT=$(git rev-parse --show-toplevel)
MOD="$ROOT/modernization/programs/cbact01c"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
sed -e '/ORGANIZATION IS INDEXED/{N;N;s/ORGANIZATION IS INDEXED.*RECORD KEY   IS FD-ACCT-ID/ORGANIZATION IS LINE SEQUENTIAL/}' \
  "$ROOT/app/cbl/CBACT01C.cbl" > "$WORK/CBACT01G.cbl"
grep -q 'LINE SEQUENTIAL' "$WORK/CBACT01G.cbl"
cobc -x -std=ibm -fsign=EBCDIC -I "$ROOT/app/cpy" -o "$WORK/cbact01g" \
  "$WORK/CBACT01G.cbl" "$MOD/src/test/golden-harness/COBDATFT.cbl" "$MOD/src/test/golden-harness/CEE3ABD.cbl"
cd "$WORK"
ACCTFILE="$ROOT/app/data/ASCII/acctdata.txt" OUTFILE=out.dat ARRYFILE=arry.dat VBRCFILE=vbrc.dat \
  ./cbact01g > sysout.txt 2> stderr.txt
cp out.dat arry.dat vbrc.dat sysout.txt "$MOD/src/test/resources/golden/"
cp "$ROOT/app/data/ASCII/acctdata.txt" "$MOD/src/test/resources/fixtures/acctdata.txt"
echo "fixture and golden outputs written to $MOD/src/test/resources/golden"

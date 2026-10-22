#!/usr/bin/env bash
# Regenerates test/golden/batch/out/ by running the reference batch COBOL
# (app/cbl CBACT01C-04C, CBCUS01C) under GnuCOBOL 3.x with BDB indexed files
# standing in for the VSAM KSDS clusters, loaded from app/data/ASCII.
set -euo pipefail
here="$(cd "$(dirname "$0")/../test/golden/batch" && pwd)"
app="$(cd "$here/../../../../app" && pwd)"
out="$here/out"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$out"
cob() { cobc -x -fsign=EBCDIC -fstatic-call -I "$app/cpy" "$@"; }
cob -o "$work/loadksds" "$here/cobol/LOADKSDS.cbl"
cob -o "$work/cbact01c" "$app/cbl/CBACT01C.cbl" "$here/cobol/COBDATFT.cbl" "$here/cobol/CEE3ABD.cbl"
for p in CBACT02C CBACT03C CBCUS01C; do cob -o "$work/${p,,}" "$app/cbl/$p.cbl" "$here/cobol/CEE3ABD.cbl"; done
cob -o "$work/intcall" "$here/cobol/INTCALL.cbl" "$app/cbl/CBACT04C.cbl" "$here/cobol/CEE3ABD.cbl"

cd "$work"
export DD_ACCTFILE=acct.dat DD_CARDFILE=card.dat DD_XREFFILE=xref.dat DD_CUSTFILE=cust.dat
export DD_TCATBALF=tcat.dat DD_DISCGRP=disc.dat
export DD_OUTFILE=outfile.bin DD_ARRYFILE=arryfile.bin DD_VBRCFILE=vbrcfile.bin DD_TRANSACT=transact.bin
load() { DD_SEQIN="$2" ./loadksds "$1 L"; }
load_seed() {
  load ACCT "${1:-$app/data/ASCII/acctdata.txt}"; load CARD "$app/data/ASCII/carddata.txt"
  load XREF "$app/data/ASCII/cardxref.txt"; load CUST "$app/data/ASCII/custdata.txt"
  load TCAT "${2:-$app/data/ASCII/tcatbal.txt}"; load DISC "${3:-$app/data/ASCII/discgrp.txt}"
}
unload_acct() { DD_SEQOUT="$1" ./loadksds "ACCT U"; }

load_seed
./cbact01c > "$out/cbact01c.sysout.txt"
cp outfile.bin arryfile.bin vbrcfile.bin "$out/" && for f in outfile arryfile vbrcfile; do mv "$out/$f.bin" "$out/cbact01c.$f.bin"; done
./cbact02c > "$out/cbact02c.sysout.txt"
./cbact03c > "$out/cbact03c.sysout.txt"
./cbcus01c > "$out/cbcus01c.sysout.txt"
./intcall > "$out/cbact04c-seed.sysout.txt"
cp transact.bin "$out/cbact04c-seed.transact.bin"
unload_acct "$out/cbact04c-seed.acctdata.txt"

rm -f ./*.dat*
load_seed "$here/fixtures/acctdata-interest.txt" "$here/fixtures/tcatbal-interest.txt" "$here/fixtures/discgrp-interest.txt"
./intcall > "$out/cbact04c-interest.sysout.txt"
cp transact.bin "$out/cbact04c-interest.transact.bin"
unload_acct "$out/cbact04c-interest.acctdata.txt"

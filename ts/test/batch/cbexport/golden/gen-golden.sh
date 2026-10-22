#!/usr/bin/env bash
# Regenerates the CBEXPORT/CBIMPORT golden files by running the real programs
# under GnuCOBOL 3.x (indexed files stand in for VSAM).
#
# CBEXPORT/CBIMPORT name EXPORT-SEQUENCE-NUM (a WORKING-STORAGE item) as the
# RECORD KEY of the export file, which neither Enterprise COBOL nor GnuCOBOL
# accepts. The copies compiled here only redefine the FD record so the key is
# the JCL's KEYS(4 28) byte range; the PROCEDURE DIVISIONs are untouched.
#
# Run 1: ASCII seeds (dailytran.txt as TRANSACT) -> CBEXPORT -> export.dat.
# Run 2: export.dat plus two unknown-type records -> CBIMPORT -> *out.dat.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
app="$(cd "$here/../../../../../app" && pwd)"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fdrec='       01  EXPORT-\1-RECORD.\n           05 FILLER PIC X(28).\n           05 EXP-FD-KEY PIC X(4).\n           05 FILLER PIC X(468).'
for p in CBEXPORT CBIMPORT; do
  sed -E -e 's/RECORD KEY IS EXPORT-SEQUENCE-NUM/RECORD KEY IS EXP-FD-KEY/' \
    -e "s/^       01  EXPORT-(OUTPUT|INPUT)-RECORD +PIC X\(500\)\./$fdrec/" \
    "$app/cbl/$p.cbl" > "$work/$p.cbl"
  cobc -x -fsign=EBCDIC -fassign-clause=ibm -I "$app/cpy" -o "$work/$p" "$work/$p.cbl"
done
cobc -x -fassign-clause=ibm -o "$work/loadidx" "$here/cobol/LOADIDX.cbl"
cobc -x -fassign-clause=ibm -o "$work/expcopy" "$here/cobol/EXPCOPY.cbl"
cd "$work"
export CUSTSEED="$app/data/ASCII/custdata.txt" ACCTSEED="$app/data/ASCII/acctdata.txt" XREFSEED="$app/data/ASCII/cardxref.txt"
export TRANSEED="$app/data/ASCII/dailytran.txt" CARDSEED="$app/data/ASCII/carddata.txt"
export CUSTFILE=cust.idx ACCTFILE=acct.idx XREFFILE=xref.idx TRANSACT=tran.idx CARDFILE=card.idx
./loadidx
EXPFILE=export.idx ./CBEXPORT > "$here/cbexport.txt"
EXPFILE=export.idx EXPFLAT="$here/export.dat" ./expcopy DUMP
# Unknown types: 'Z' with sequence 0 (sorts first) and 'c' with sequence 12345678 (ERR-SEQUENCE keeps 2345678).
python3 - "$here/export.dat" import-in.dat <<'PY'
import sys
data = open(sys.argv[1], 'rb').read()
def rec(t, seq):
    return (t + 'UNKNOWN'.ljust(26)).encode() + seq.to_bytes(4, 'big') + b'0001NORTH' + b' ' * 460
open(sys.argv[2], 'wb').write(rec('Z', 0) + data + rec('c', 12345678))
PY
EXPFILE=import.idx EXPFLAT=import-in.dat ./expcopy LOAD
EXPFILE=import.idx CUSTOUT="$here/custout.dat" ACCTOUT="$here/acctout.dat" XREFOUT="$here/xrefout.dat" \
  TRNXOUT="$here/trnxout.dat" CARDOUT="$here/cardout.dat" ERROUT="$here/errout.dat" ./CBIMPORT > "$here/cbimport.txt"

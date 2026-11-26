#!/usr/bin/env bash
# Builds CBTRN02C with GnuCOBOL (EBCDIC overpunch signs, as in app/data/ASCII),
# loads the sample VSAM files into BDB indexed files, runs POSTTRAN and dumps outputs.
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$HERE/../../../.." && pwd)
D=$REPO/app/data/ASCII
OUT=${1:-$HERE/../src/test/resources/golden}
W=$(mktemp -d); trap 'rm -rf "$W"' EXIT
cd "$W"
cobc -x -std=ibm -fsign=EBCDIC "$HERE/LOADKS.cbl" -o LOADKS
cobc -x -std=ibm -fsign=EBCDIC "$HERE/DUMPKS.cbl" -o DUMPKS
cobc -x -std=ibm -fsign=EBCDIC -I "$REPO/app/cpy" "$REPO/app/cbl/CBTRN02C.cbl" -o CBTRN02C
tr -d '\r' < "$D/tcatbal.txt" > tcat.txt
INFILE=$D/cardxref.txt XREFFILE=xref KIND=XREF ./LOADKS
INFILE=$D/acctdata.txt ACCTFILE=acct KIND=ACCT ./LOADKS
INFILE=tcat.txt TCATBALF=tcat KIND=TCAT ./LOADKS
tr -d '\n\r' < "$D/dailytran.txt" > dalytran.bin
set +e
DALYTRAN=dalytran.bin TRANFILE=tran XREFFILE=xref DALYREJS=rejs.bin ACCTFILE=acct TCATBALF=tcat ./CBTRN02C > sysout.txt 2>&1
echo $? > return-code.txt
set -e
KIND=TRAN TRANFILE=tran OUTFILE=transact.txt ./DUMPKS
KIND=ACCT ACCTFILE=acct OUTFILE=acctdata.txt ./DUMPKS
KIND=TCAT TCATBALF=tcat OUTFILE=tcatbal.txt ./DUMPKS
fold -w 430 rejs.bin > dalyrejs.txt; echo >> dalyrejs.txt
mkdir -p "$OUT"
cp sysout.txt return-code.txt transact.txt acctdata.txt tcatbal.txt dalyrejs.txt "$OUT/"

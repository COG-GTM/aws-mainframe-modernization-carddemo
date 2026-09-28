#!/usr/bin/env bash
# Compiles the legacy CBTRN02C (POSTTRAN) and CBACT04C (INTCALC) with GnuCOBOL,
# runs them against app/data/ASCII and writes the resulting files to
# src/test/resources/golden/. Requires cobc (GnuCOBOL 3.x with an indexed-file handler).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../../.." && pwd)
work=${WORK_DIR:-$here/../target/golden-work}
out=${GOLDEN_OUT:-$here/../src/test/resources/golden}
ascii=$repo/app/data/ASCII
dalytran_in=${DALYTRAN_IN:-$ascii/dailytran.txt}
rm -rf "$work" && mkdir -p "$work"/{bin,in,posttran,intcalc} "$out"/{posttran,intcalc}
COBC="cobc -fsign=EBCDIC -I $repo/app/cpy"

# name mode reclen keyoff keylen [altoff altlen]
utils=(
  "LACCT LOAD 300 0 11" "UACCT UNLD 300 0 11"
  "LXREF LOAD 50 0 16 25 11"
  "LTCAT LOAD 50 0 17" "UTCAT UNLD 50 0 17"
  "LDISC LOAD 50 0 16"
  "UTRAN UNLD 350 0 16"
)
for u in "${utils[@]}"; do
  set -- $u
  "$here/gen-idxutil.sh" "$@" > "$work/bin/$1.cbl"
  (cd "$work/bin" && $COBC -x -o "$1" "$1.cbl")
done
(cd "$work/bin" && $COBC -x -o CBTRN02C "$repo/app/cbl/CBTRN02C.cbl")
(cd "$work/bin" && $COBC -x -o RUNINTC "$here/RUNINTC.cbl" "$repo/app/cbl/CBACT04C.cbl")

for f in acctdata cardxref tcatbal discgrp; do
  tr -d '\r' < "$ascii/$f.txt" > "$work/in/$f.txt"
done
tr -d '\r' < "$dalytran_in" > "$work/in/dailytran.txt"

load() { # util seqfile idxfile
  SEQFILE=$2 IDXFILE=$3 "$work/bin/$1"
}
unload() { # util idxfile seqfile
  IDXFILE=$2 SEQFILE=$3 COB_LS_FIXED=TRUE "$work/bin/$1"
}
fresh_masters() { # dir
  load LACCT "$work/in/acctdata.txt" "$1/acct.idx"
  load LXREF "$work/in/cardxref.txt" "$1/xref.idx"
  load LTCAT "$work/in/tcatbal.txt" "$1/tcatbal.idx"
  load LDISC "$work/in/discgrp.txt" "$1/discgrp.idx"
}

# ---- POSTTRAN / CBTRN02C
d=$work/posttran
fresh_masters "$d"
# DALYTRAN is RECFM=FB 350 (record sequential, no line terminators)
awk '{printf "%-350.350s", $0}' "$work/in/dailytran.txt" > "$d/dalytran.dat"
set +e
DD_DALYTRAN=$d/dalytran.dat DD_TRANFILE=$d/transact.idx DD_XREFFILE=$d/xref.idx \
DD_DALYREJS=$d/dalyrejs.dat DD_ACCTFILE=$d/acct.idx DD_TCATBALF=$d/tcatbal.idx \
  "$work/bin/CBTRN02C" > "$d/sysout.txt" 2>&1
rc=$?
set -e
echo "$rc" > "$out/posttran/returncode.txt"
unload UACCT "$d/acct.idx" "$out/posttran/acctdata.txt"
unload UTCAT "$d/tcatbal.idx" "$out/posttran/tcatbal.txt"
unload UTRAN "$d/transact.idx" "$out/posttran/transact.txt"
if [[ -s $d/dalyrejs.dat ]]; then { fold -w 430 "$d/dalyrejs.dat"; echo; } > "$out/posttran/dalyrejs.txt"; else : > "$out/posttran/dalyrejs.txt"; fi
grep -E 'TRANSACTIONS (PROCESSED|REJECTED)' "$d/sysout.txt" | sed 's/ *$//' > "$out/posttran/counts.txt"

# ---- INTCALC / CBACT04C PARM='2022071800'
# Chained on the POSTTRAN results (as in the daily -> monthly cycle): the raw sample
# tran_cat_balance rows are all zero, which would only produce zero-interest rows.
d=$work/intcalc
load LACCT "$out/posttran/acctdata.txt" "$d/acct.idx"
load LXREF "$work/in/cardxref.txt" "$d/xref.idx"
load LTCAT "$out/posttran/tcatbal.txt" "$d/tcatbal.idx"
load LDISC "$work/in/discgrp.txt" "$d/discgrp.idx"
set +e
DD_TCATBALF=$d/tcatbal.idx DD_XREFFILE=$d/xref.idx DD_ACCTFILE=$d/acct.idx \
DD_DISCGRP=$d/discgrp.idx DD_TRANSACT=$d/systran.dat \
  "$work/bin/RUNINTC" > "$d/sysout.txt" 2>&1
rc=$?
set -e
echo "$rc" > "$out/intcalc/returncode.txt"
unload UACCT "$d/acct.idx" "$out/intcalc/acctdata.txt"
if [[ -s $d/systran.dat ]]; then { fold -w 350 "$d/systran.dat"; echo; } > "$out/intcalc/systran.txt"; else : > "$out/intcalc/systran.txt"; fi

# TRAN-PROC-TS (and CBACT04C's TRAN-ORIG-TS) come from FUNCTION CURRENT-DATE: mask them so the
# golden files are reproducible.
f=$out/posttran/transact.txt
awk '{print substr($0,1,304) "PROC-TS-MASKED            " substr($0,331)}' "$f" > "$f.tmp" && mv "$f.tmp" "$f"
f=$out/intcalc/systran.txt
awk '{print substr($0,1,278) "ORIG-TS-MASKED            PROC-TS-MASKED            " substr($0,331)}' "$f" > "$f.tmp" && mv "$f.tmp" "$f"
echo "golden files written to $out"

#!/usr/bin/env bash
# Regenerates test/batch/cbtrn/golden/* by running the original app/cbl/CBTRN01C,
# CBTRN02C and CBTRN03C under GnuCOBOL 3.1 (`sudo apt-get install -y gnucobol3`).
# KSDS clusters are GnuCOBOL indexed files loaded from the ASCII seeds (REPRO
# stand-in: ksdsload.cbl.tmpl); sequential files are fixed-length, newline-free.
# TRAN-PROC-TS (cols 305-330) is the wall clock (CURRENT-DATE ignores
# COB_CURRENT_DATE), so it is masked with '#'. CEE3ABD is stubbed (RC 999).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
OUT="$HERE/../../test/batch/cbtrn/golden"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
FLAGS=(-std=ibm -fsign=EBCDIC)
SEQ="python3 $HERE/seqdata.py"

tool() { # name reclen keylen keyoffset
  sed -e "s/@PGM@/$1/" -e "s/@LEN@/$2/" -e "s/@KEYLEN@/$3/" -e "s/@DATALEN@/$(($2 - $3))/" \
    "$HERE/ksdsload.cbl.tmpl" > "$WORK/$1.cbl"
  cobc -x "${FLAGS[@]}" -o "$WORK/$1" "$WORK/$1.cbl"
}
ksds() { (cd "$WORK" && MODE="$2" DD_SEQFILE="$3" DD_KSDSFILE="$4" "./$1" >/dev/null); }
prog() { cobc -x "${FLAGS[@]}" -I "$ROOT/app/cpy" -o "$WORK/$1" "$ROOT/app/cbl/$1.cbl" "$HERE/CEE3ABD.cbl"; }
finish() { # outdir rc
  echo "$2" > "$1/return-code.txt"; sed -i 's/[[:space:]]*$//' "$1/sysout.txt"
}

$SEQ prep "$ROOT" "$WORK"
tool KSDSTRAN 350 16; tool KSDSXREF 50 16; tool KSDSACCT 300 11; tool KSDSTCAT 50 17
tool KSDSCUST 500 9; tool KSDSCARD 150 16; tool KSDSTTYP 60 2; tool KSDSTCAG 60 6
prog CBTRN01C; prog CBTRN02C; prog CBTRN03C
: > "$WORK/empty.dat"

load_common() { # acctfile
  ksds KSDSTRAN LOAD empty.dat transact.ksds
  ksds KSDSXREF LOAD cardxref.dat cardxref.ksds
  ksds KSDSACCT LOAD "$1" acctdata.ksds
  ksds KSDSTCAT LOAD tcatbal.dat tcatbal.ksds
}

# ---- CBTRN01C (DISPLAY only) ----
load_common acctdata.dat
ksds KSDSCUST LOAD custdata.dat custdata.ksds
ksds KSDSCARD LOAD carddata.dat carddata.ksds
d="$OUT/cbtrn01c"; mkdir -p "$d"; set +e
(cd "$WORK" && DD_DALYTRAN=dalytran.dat DD_CUSTFILE=custdata.ksds DD_XREFFILE=cardxref.ksds \
  DD_CARDFILE=carddata.ksds DD_ACCTFILE=acctdata.ksds DD_TRANFILE=transact.ksds ./CBTRN01C) > "$d/sysout.txt" 2>&1
rc=$?; set -e; finish "$d" $rc

# ---- CBTRN02C scenarios: sample | edge | dup ----
post() { # name dalytran acctfile
  load_common "$3"
  local d="$OUT/cbtrn02c-$1"; mkdir -p "$d"; set +e
  (cd "$WORK" && DD_DALYTRAN="$2" DD_TRANFILE=transact.ksds DD_XREFFILE=cardxref.ksds \
    DD_DALYREJS=dalyrejs.dat DD_ACCTFILE=acctdata.ksds DD_TCATBALF=tcatbal.ksds ./CBTRN02C) > "$d/sysout.txt" 2>&1
  local rc=$?; set -e; finish "$d" $rc
  ksds KSDSTRAN UNLOAD transact.out transact.ksds
  ksds KSDSACCT UNLOAD acctdata.out acctdata.ksds
  ksds KSDSTCAT UNLOAD tcatbal.out tcatbal.ksds
  $SEQ lines "$WORK/transact.out" 350 "$d/transact.txt" 304 26
  $SEQ lines "$WORK/acctdata.out" 300 "$d/acctdata.txt"
  $SEQ lines "$WORK/tcatbal.out" 50 "$d/tcatbal.txt"
  $SEQ lines "$WORK/dalyrejs.dat" 430 "$d/dalyrejs.txt"
}
post sample dalytran.dat acctdata.dat
cp "$WORK/transact.out" "$WORK/posted.dat"
post edge dalytran-edge.dat acctdata-edge.dat
post dup dalytran-dup.dat acctdata.dat

# ---- CBTRN03C scenarios over the posted sample transactions ----
ksds KSDSXREF LOAD cardxref.dat cardxref.ksds
ksds KSDSTTYP LOAD trantype.dat trantype.ksds
ksds KSDSTCAG LOAD trancatg.dat trancatg.ksds
rept() { # name variant start end [filter-start filter-end]
  local d="$OUT/cbtrn03c-$1"; mkdir -p "$d"
  $SEQ tranrept "$WORK/posted.dat" "$WORK/tranin.dat" "${5:-$3}" "${6:-$4}" "$2"
  $SEQ parm "$WORK/dateparm.dat" "$3" "$4"
  $SEQ lines "$WORK/tranin.dat" 350 "$d/tranfile.txt"
  set +e
  (cd "$WORK" && DD_TRANFILE=tranin.dat DD_CARDXREF=cardxref.ksds DD_TRANTYPE=trantype.ksds \
    DD_TRANCATG=trancatg.ksds DD_DATEPARM=dateparm.dat DD_TRANREPT=tranrept.dat ./CBTRN03C) > "$d/sysout.txt" 2>&1
  local rc=$?; set -e; finish "$d" $rc
  : >> "$WORK/tranrept.dat"; $SEQ lines "$WORK/tranrept.dat" 133 "$d/tranrept.txt"; rm -f "$WORK/tranrept.dat"
}
rept sorted all 2022-01-01 2022-07-06
# JCL sort filter disabled: an out-of-range record reaches the program (NEXT SENTENCE path)
rept unfiltered mixed 2022-06-01 2022-06-30 0000-00-00 9999-99-99
rept empty all 2023-01-01 2023-12-31
echo done; for f in "$OUT"/*/return-code.txt; do echo "$f: $(cat "$f")"; done

#!/usr/bin/env bash
# Load the ASCII sample files from app/data/ASCII into GnuCOBOL indexed
# files under $WORK/ksds (default test-harness/cobol/work/ksds).
# Equivalent of the IDCAMS REPRO steps that build the VSAM KSDS clusters
# on the mainframe.  TRANFILE is created empty (no sample data; CBTRN01C
# opens it but never reads it).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
BIN="${BIN:-$HERE/bin}"
WORK="${WORK:-$HERE/work}"
DATA="$REPO/app/data/ASCII"
KSDS="${KSDS:-$WORK/ksds}"
# override any input to load a modified fixture (used by the synthetic
# rejection scenario): ACCT_SRC, XREF_SRC, CUST_SRC, CARD_SRC
ACCT_SRC="${ACCT_SRC:-$DATA/acctdata.txt}"
XREF_SRC="${XREF_SRC:-$DATA/cardxref.txt}"
CUST_SRC="${CUST_SRC:-$DATA/custdata.txt}"
CARD_SRC="${CARD_SRC:-$DATA/carddata.txt}"
rm -rf "$KSDS"; mkdir -p "$KSDS"
: > "$KSDS/empty.txt"

load() { # <TYPE> <input file> <DD name>
  local type=$1 input=$2 dd=$3
  env LOADIN="$input" "DD_$dd=$KSDS/$dd" "$BIN/KSDSLOAD" "$type"
}
load ACCT "$ACCT_SRC" ACCTFILE
load XREF "$XREF_SRC" XREFFILE
load CUST "$CUST_SRC" CUSTFILE
load CARD "$CARD_SRC" CARDFILE
load TRAN "$KSDS/empty.txt"    TRANFILE
ls -l "$KSDS"

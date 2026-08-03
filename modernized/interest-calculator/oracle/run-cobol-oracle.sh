#!/usr/bin/env bash
#
# run-cobol-oracle.sh -- produce a TRUE behavioural oracle for CBACT04C.
#
# Compiles the UNMODIFIED legacy program app/cbl/CBACT04C.cbl with GnuCOBOL,
# loads the fixed-width ASCII datasets into INDEXED (VSAM KSDS equivalent)
# files, executes the program exactly as app/jcl/INTCALC.jcl does
# (PGM=CBACT04C,PARM='<run-date>'), and captures:
#
#   <out>/transact.dat        - the TRANSACT output, one 350-byte record per line
#   <out>/acctdata-after.dat  - the ACCTFILE master after the run (300 bytes/record)
#   <out>/cobol-run.log       - everything the program DISPLAYed
#
# Nothing under app/ is read-write: the legacy sources and datasets are only
# ever read. All scratch files live under the output directory.
#
# Usage: run-cobol-oracle.sh <dataset-dir> <output-dir> [run-date]
#
# <dataset-dir> must contain tcatbal.txt, cardxref.txt, acctdata.txt, discgrp.txt
# in the fixed-width ASCII layout used by app/data/ASCII.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$here/../../.." && pwd)"

data_dir="${1:?usage: run-cobol-oracle.sh <dataset-dir> <output-dir> [run-date]}"
out_dir="${2:?usage: run-cobol-oracle.sh <dataset-dir> <output-dir> [run-date]}"
run_date="${3:-2022071800}"

command -v cobc >/dev/null || { echo "GnuCOBOL (cobc) is not installed" >&2; exit 2; }

mkdir -p "$out_dir"
work="$out_dir/work"
rm -rf "$work"
mkdir -p "$work"

# --- normalise the ASCII datasets ------------------------------------------
# The committed sample files use CRLF in places and drop trailing FILLER
# spaces; INDEXED loading needs exact fixed-length records.
normalise() { # <src> <len> <dst>
  tr -d '\r' < "$1" | awk -v n="$2" '{ printf "%-*s\n", n, $0 }' > "$3"
}
normalise "$data_dir/tcatbal.txt"  50  "$work/tcatbal.seq"
normalise "$data_dir/cardxref.txt" 50  "$work/cardxref.seq"
normalise "$data_dir/acctdata.txt" 300 "$work/acctdata.seq"
normalise "$data_dir/discgrp.txt"  50  "$work/discgrp.seq"

# --- compile ----------------------------------------------------------------
# CBACT04C is compiled straight out of app/cbl with the copybooks from app/cpy.
#
# -fsign=EBCDIC makes GnuCOBOL use the mainframe trailing overpunch encoding
# for signed DISPLAY (zoned decimal) fields ('{'..'I' positive, '}'..'R'
# negative). That is the encoding actually present in app/data/ASCII, so
# without it both the reads and the writes would use the Linux-native ASCII
# sign convention and would not match the datasets.
COBFLAGS=(-fsign=EBCDIC)
cobc -m "${COBFLAGS[@]}" -I "$repo_root/app/cpy" -o "$work/CBACT04C.so" "$repo_root/app/cbl/CBACT04C.cbl"
cobc -x "${COBFLAGS[@]}" -o "$work/runcb04"  "$here/RUNCB04.cbl"
cobc -x "${COBFLAGS[@]}" -o "$work/loadvsam" "$here/LOADVSAM.cbl"
cobc -x "${COBFLAGS[@]}" -o "$work/unldacct" "$here/UNLDACCT.cbl"

# --- DD assignments (mirror app/jcl/INTCALC.jcl) -----------------------------
export COB_LS_FIXED=1
export COB_LIBRARY_PATH="$work"
export DD_TCATSEQ="$work/tcatbal.seq"
export DD_XREFSEQ="$work/cardxref.seq"
export DD_ACCTSEQ="$work/acctdata.seq"
export DD_DISCSEQ="$work/discgrp.seq"
export DD_TCATBALF="$work/TCATBALF.idx"
export DD_XREFFILE="$work/XREFFILE.idx"
export DD_ACCTFILE="$work/ACCTFILE.idx"
export DD_DISCGRP="$work/DISCGRP.idx"
export DD_TRANSACT="$work/TRANSACT.dat"
export DD_ACCTOUT="$out_dir/acctdata-after.dat"

{
  "$work/loadvsam"
  "$work/runcb04" "$run_date"
  "$work/unldacct"
} 2>&1 | tee "$out_dir/cobol-run.log"

# TRANSACT is RECFM=F LRECL=350 with no record delimiters; split it so the
# golden file is one 350-character record per line.
fold -w 350 "$work/TRANSACT.dat" | sed -e '$a\' > "$out_dir/transact.dat"

echo "oracle written to $out_dir"

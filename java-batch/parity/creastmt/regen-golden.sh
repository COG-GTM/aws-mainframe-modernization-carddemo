#!/usr/bin/env bash
# Regenerates the CREASTMT golden outputs by running the legacy
# CBSTM03A + CBSTM03B under GnuCOBOL against the ASCII samples.
#
# The only change made to the legacy source (in a scratch copy) is
# removing the z/OS-only PSA/TCB/TIOT walk at the top of the PROCEDURE
# DIVISION (lines 266-292 of CBSTM03A.CBL): it dereferences MVS control
# blocks through a null pointer and only DISPLAYs DD names. CBSTM03B is
# compiled unmodified.
# Requires: GnuCOBOL 3.x with an indexed handler (BDB/VBISAM), python3.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../.." && pwd)"
DATA="$REPO/app/data/ASCII"
GOLDEN="$REPO/java-batch/src/test/resources/golden/creastmt"
WORK="${WORK:-$(mktemp -d)}"
mkdir -p "$WORK" "$GOLDEN"

awk 'NR>=266 && NR<=292 { print substr($0,1,6) "*" substr($0,8); next } { print }' \
    "$REPO/app/cbl/CBSTM03A.CBL" > "$WORK/CBSTM03A.cbl"

# CUSTREC.cpy indents with tabs, which GnuCOBOL fixed-format reads as
# column shifts; use a tab-normalised copy of the copybooks.
mkdir -p "$WORK/cpy"
for c in "$REPO"/app/cpy/*; do
    sed -e 's/^\t\t/      /' -e 's/\t/ /g' "$c" > "$WORK/cpy/$(basename "$c")"
done

cobc -x -std=ibm -fsign=EBCDIC -I "$WORK/cpy" -o "$WORK/CBSTM03A" \
     "$WORK/CBSTM03A.cbl" "$REPO/app/cbl/CBSTM03B.CBL"
cobc -x -o "$WORK/LOADKSDS" "$HERE/LOADKSDS.cbl"

python3 "$HERE/sort-trxfl.py" "$DATA/dailytran.txt" "$WORK/trxfl.seq"

load() { SEQIN="$2" TRNXFILE="$WORK/trxfl.ksds" XREFFILE="$WORK/xref.ksds" \
         CUSTFILE="$WORK/cust.ksds" ACCTFILE="$WORK/acct.ksds" "$WORK/LOADKSDS" "$1"; }
rm -f "$WORK"/*.ksds*
load TRNX "$WORK/trxfl.seq"
load XREF "$DATA/cardxref.txt"
load CUST "$DATA/custdata.txt"
load ACCT "$DATA/acctdata.txt"

TRNXFILE="$WORK/trxfl.ksds" XREFFILE="$WORK/xref.ksds" \
CUSTFILE="$WORK/cust.ksds" ACCTFILE="$WORK/acct.ksds" \
STMTFILE="$GOLDEN/STMTFILE.dat" HTMLFILE="$GOLDEN/HTMLFILE.dat" \
    "$WORK/CBSTM03A"

echo "STMTFILE: $(wc -c < "$GOLDEN/STMTFILE.dat") bytes ($(( $(wc -c < "$GOLDEN/STMTFILE.dat") / 80 )) x 80)"
echo "HTMLFILE: $(wc -c < "$GOLDEN/HTMLFILE.dat") bytes ($(( $(wc -c < "$GOLDEN/HTMLFILE.dat") / 100 )) x 100)"

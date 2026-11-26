#!/usr/bin/env bash
# Regenerates src/test/resources/golden/* by running the original COBOL
# (CBSTM03A + CBSTM03B) under GnuCOBOL against the app/data/ASCII samples.
# The z/OS TIOT control-block walk (PROCEDURE DIVISION prologue) cannot run
# off-mainframe, so it is stripped from a temporary copy before compiling.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
OUT="$HERE/../src/test/resources/golden"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# CREASTMT STEP010: SORT FIELDS=(263,16,CH,A,1,16,CH,A)
#                   OUTREC FIELDS=(1:263,16,17:1,262,279:279,50), LRECL=350
LC_ALL=C awk '{ printf "%-350s\n", substr($0,263,16) substr($0,1,262) substr($0,279,50) }' \
  "$REPO/app/data/ASCII/dailytran.txt" | LC_ALL=C sort -k1.1,1.32 > "$WORK/trxfl.txt"

# Drop the TIOT walk: from the first SET ADDRESS OF PSA-BLOCK up to (not incl.) OPEN OUTPUT.
awk '/SET ADDRESS OF PSA-BLOCK/{skip=1} /OPEN OUTPUT STMT-FILE/{skip=0} !skip' \
  "$REPO/app/cbl/CBSTM03A.CBL" > "$WORK/CBSTM03A.cbl"

# CUSTREC.cpy is indented with TABs (pushing PICs past col 72); re-indent to area B.
mkdir -p "$WORK/cpy"
for f in "$REPO"/app/cpy/*; do sed -E "s/^\t+ */           /" "$f" > "$WORK/cpy/$(basename "$f")"; done

cd "$WORK"
COBFLAGS=(-std=ibm -fsign=EBCDIC -I "$WORK/cpy")
cobc -x "${COBFLAGS[@]}" -o loadksds "$HERE/LOADKSDS.cbl"
cobc -x "${COBFLAGS[@]}" -o cbstm03a CBSTM03A.cbl "$REPO/app/cbl/CBSTM03B.CBL"

export TRNXFILE="$WORK/TRNXFILE.dat" XREFFILE="$WORK/XREFFILE.dat" \
       CUSTFILE="$WORK/CUSTFILE.dat" ACCTFILE="$WORK/ACCTFILE.dat"
INFILE="$WORK/trxfl.txt" ./loadksds TRNXFILE
INFILE="$REPO/app/data/ASCII/cardxref.txt" ./loadksds XREFFILE
INFILE="$REPO/app/data/ASCII/custdata.txt" ./loadksds CUSTFILE
INFILE="$REPO/app/data/ASCII/acctdata.txt" ./loadksds ACCTFILE

STMTFILE="$WORK/stmt.dat" HTMLFILE="$WORK/html.dat" ./cbstm03a
echo "cbstm03a RC=$?"

mkdir -p "$OUT"
# Fixed-length FB records (80 / 100 bytes, no delimiters) -> one record per line.
fold -w 80 "$WORK/stmt.dat" > "$OUT/STMTFILE.txt"
fold -w 100 "$WORK/html.dat" > "$OUT/HTMLFILE.txt"
wc -l "$OUT"/*.txt

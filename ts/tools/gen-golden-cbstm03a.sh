#!/usr/bin/env bash
# Regenerates test/batch/cbstm03a/golden/{statement.txt,statement.html} by running the
# real CBSTM03A + CBSTM03B under GnuCOBOL 3.x (BDB indexed files stand in for VSAM).
#  - TRNXFILE is built like CREASTMT STEP010/020: TRANSACT (= app/data/ASCII/dailytran.txt,
#    there is no TRANSACT seed) sorted on card(263,16)+tran id(1,16), OUTREC
#    (1:263,16,17:1,262,279:279,50), padded to 350 with spaces, then loaded into a KSDS.
#  - The z/OS control-block walk (PSA/TCB/TIOT, lines before OPEN OUTPUT) cannot run
#    off-mainframe and is removed from the compiled copy; everything else is unchanged.
#  - STMTFILE/HTMLFILE are record-sequential (80/100 bytes) and are split into lines,
#    right-trimmed.
set -euo pipefail
here="$(cd "$(dirname "$0")/.." && pwd)"
app="$here/../app"
gold="$here/test/batch/cbstm03a/golden"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

python3 - "$app/data/ASCII/dailytran.txt" "$work/trnx.txt" <<'PY'
import sys
recs = [l.rstrip('\r\n').ljust(350) for l in open(sys.argv[1], encoding='latin-1') if l.strip()]
key = lambda r: (r[262:278] + r[0:16]).encode('cp037')
out = [(r[262:278] + r[0:262] + r[278:328]).ljust(350) for r in sorted(recs, key=key)]
open(sys.argv[2], 'w', encoding='latin-1').write(''.join(o + '\n' for o in out))
PY

lstart=$(grep -n 'SET ADDRESS OF PSA-BLOCK' "$app/cbl/CBSTM03A.CBL" | cut -d: -f1)
lend=$(grep -n 'OPEN OUTPUT STMT-FILE HTML-FILE' "$app/cbl/CBSTM03A.CBL" | cut -d: -f1)
sed "${lstart},$((lend - 1))d" "$app/cbl/CBSTM03A.CBL" > "$work/CBSTM03A.CBL"

cobc -x -o "$work/loadidx" "$gold/cobol/LOADIDX.cbl"
mkdir "$work/cpy"
# CUSTREC.cpy indents with tabs, which cobc rejects in fixed format: re-indent those lines to column 12.
for f in "$app"/cpy/*; do sed -E 's/^[ \t]*\t[ \t]*/           /' "$f" > "$work/cpy/$(basename "$f")"; done
cobc -x -fsign=EBCDIC -I "$work/cpy" -o "$work/cbstm03a" "$work/CBSTM03A.CBL" "$app/cbl/CBSTM03B.CBL"

cd "$work"
export TRNXIN=trnx.txt XREFIN="$app/data/ASCII/cardxref.txt" CUSTIN="$app/data/ASCII/custdata.txt" \
  ACCTIN="$app/data/ASCII/acctdata.txt" TRNXFILE=trnx.idx XREFFILE=xref.idx CUSTFILE=cust.idx \
  ACCTFILE=acct.idx STMTFILE=stmt.dat HTMLFILE=html.dat
./loadidx
./cbstm03a
split_lines() { python3 -c "import sys;d=open(sys.argv[1],'rb').read();n=int(sys.argv[2]);sys.stdout.write(''.join(d[i:i+n].decode('latin-1').rstrip()+'\n' for i in range(0,len(d),n)))" "$1" "$2"; }
split_lines stmt.dat 80 > "$gold/statement.txt"
split_lines html.dat 100 > "$gold/statement.html"

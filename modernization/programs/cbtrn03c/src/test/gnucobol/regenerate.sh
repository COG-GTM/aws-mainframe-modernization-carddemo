#!/usr/bin/env bash
# Regenerates src/test/resources/fixtures/*-gnucobol-* by running the original CBTRN03C under GnuCOBOL 3.1.2.
# -fsign=EBCDIC makes GnuCOBOL decode the EBCDIC-style overpunched TRAN-AMT signs ({A-I / }J-R) in app/data/ASCII
# the same way Enterprise COBOL decodes the original EBCDIC bytes.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd); repo=$(cd "$here/../../../../../.." && pwd); fx="$here/../resources/fixtures"
work=$(mktemp -d); cd "$work"
cobc -x -std=ibm -fsign=EBCDIC -I "$repo/app/cpy" -o cbtrn03c "$repo/app/cbl/CBTRN03C.cbl"
for f in xref ttyp tcat; do cobc -x -o "load_$f" "$here/load_$f.cbl"; done
INFILE="$fx/cardxref.txt" OUTFILE=xref.idx ./load_xref
INFILE="$fx/trantype.txt" OUTFILE=ttyp.idx ./load_ttyp
INFILE="$fx/trancatg.txt" OUTFILE=tcat.idx ./load_tcat
# TRANREPT STEP05R input: TRAN-PROC-TS := TRAN-ORIG-TS, stable sort on TRAN-CARD-NUM, RECFM=FB (no newlines).
python3 - "$fx/dailytran.txt" <<'PY'
import sys
recs = [l.rstrip('\n') for l in open(sys.argv[1], encoding='latin-1')]
recs = sorted((r[:304] + r[278:304] + r[330:] for r in recs), key=lambda r: r[262:278])
open('tran.dat', 'w', encoding='latin-1').write(''.join(recs))
r = recs[9]; recs[9] = r[:304] + '2023-01-01' + r[314:]
open('tran_oor.dat', 'w', encoding='latin-1').write(''.join(recs))
PY
lines() { python3 -c "import sys;d=open(sys.argv[1],encoding='latin-1').read();sys.stdout.write(''.join(d[i:i+133]+'\n' for i in range(0,len(d),133)))" "$1"; }
run() { DD_TRANFILE=$1 DD_CARDXREF=xref.idx DD_TRANTYPE=ttyp.idx DD_TRANCATG=tcat.idx DD_DATEPARM="$fx/dateparm.txt" DD_TRANREPT=$2 ./cbtrn03c; }
run tran.dat rep_full > "$fx/sysout-gnucobol-full.txt"; lines rep_full > "$fx/tranrept-gnucobol-full.txt"
run tran_oor.dat rep_oor > /dev/null; lines rep_oor > "$fx/tranrept-gnucobol-out-of-range.txt"
echo "fixtures regenerated from $work"

#!/usr/bin/env bash
# Run CBTRN01C against the daily transaction sample file and the indexed
# files built by load_ksds.sh.
#   DALYTRAN -> fixed 350-byte records (built from dailytran.txt, or
#               $DALYTRAN_SRC, by stripping the newline after each record)
#   CUSTFILE, XREFFILE, CARDFILE, ACCTFILE, TRANFILE -> indexed files
# stdout (all DISPLAYs) is captured to $OUT/display.txt and echoed.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
BIN="${BIN:-$HERE/bin}"
WORK="${WORK:-$HERE/work}"
OUT="${OUT:-$WORK/CBTRN01C}"
KSDS="${KSDS:-$WORK/ksds}"
DALYTRAN_SRC="${DALYTRAN_SRC:-$REPO/app/data/ASCII/dailytran.txt}"
mkdir -p "$OUT"
# fixed-length copy of the daily transaction file (drop record newlines)
python3 - "$DALYTRAN_SRC" "$OUT/DALYTRAN" <<'PY'
import sys
src, dst = sys.argv[1:3]
data = open(src, 'rb').read()
recs = [r for r in data.split(b'\n') if r]
assert all(len(r) == 350 for r in recs), 'dailytran.txt records must be 350 bytes'
open(dst, 'wb').write(b''.join(recs))
print('DALYTRAN records: %d' % len(recs))
PY
set +e
env COB_LIBRARY_PATH="$BIN" \
    DD_DALYTRAN="$OUT/DALYTRAN" \
    DD_CUSTFILE="$KSDS/CUSTFILE" \
    DD_XREFFILE="$KSDS/XREFFILE" \
    DD_CARDFILE="$KSDS/CARDFILE" \
    DD_ACCTFILE="$KSDS/ACCTFILE" \
    DD_TRANFILE="$KSDS/TRANFILE" \
    "$BIN/CBTRN01C" | tee "$OUT/display.txt"
rc=${PIPESTATUS[0]}
set -e
echo "CBTRN01C return code: $rc"
exit "$rc"

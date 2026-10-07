#!/usr/bin/env bash
# Run CBACT01C the way app/jcl/READACCT.jcl does:
#   ACCTFILE -> indexed file built by load_ksds.sh (the VSAM KSDS)
#   OUTFILE  -> fixed 107-byte records   (JCL: LRECL=107,RECFM=FB)
#   ARRYFILE -> fixed 110-byte records   (JCL: LRECL=110,RECFM=FB)
#   VBRCFILE -> variable records         (JCL: LRECL=84,RECFM=VB)
# Output goes to $OUT (default test-harness/cobol/work/CBACT01C).
# stdout (all DISPLAYs) is captured to $OUT/display.txt and echoed.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN="${BIN:-$HERE/bin}"
WORK="${WORK:-$HERE/work}"
OUT="${OUT:-$WORK/CBACT01C}"
KSDS="${KSDS:-$WORK/ksds}"
mkdir -p "$OUT"
rm -f "$OUT/OUTFILE" "$OUT/ARRYFILE" "$OUT/VBRCFILE"
set +e
env COB_LIBRARY_PATH="$BIN" \
    COB_VARSEQ_FORMAT=1 \
    DD_ACCTFILE="$KSDS/ACCTFILE" \
    DD_OUTFILE="$OUT/OUTFILE" \
    DD_ARRYFILE="$OUT/ARRYFILE" \
    DD_VBRCFILE="$OUT/VBRCFILE" \
    "$BIN/CBACT01C" | tee "$OUT/display.txt"
rc=${PIPESTATUS[0]}
set -e
echo "CBACT01C return code: $rc"
ls -l "$OUT"
exit "$rc"

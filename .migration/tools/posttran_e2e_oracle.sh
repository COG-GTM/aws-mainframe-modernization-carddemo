#!/usr/bin/env bash
# GnuCOBOL expected outputs for every POSTTRAN end-to-end scenario (posttran_e2e.sh), via posttran_gnucobol_oracle.sh.
# Usage: .migration/tools/posttran_e2e_oracle.sh WORK_DIR DEST_DIR   (WORK_DIR must not exist; DEST_DIR/<scenario>/ gets the
#        unchanged oracle files DALYREJS, SYSOUT, RETURN-CODE, ACCTDATA.unl, TCATBALF.unl, TRANSACT.unl)
# Scenarios: baseline, rerun (masters = baseline end state, second OPEN OUTPUT run), empty, restart-leg1 (dup151, abends),
#            restart-leg1-atomic (records 1-150, D-042 table expectation), cp037, abend-dalytran-missing.
set -euo pipefail
TOOLS=$(cd "$(dirname "$0")" && pwd)
WORK=$(realpath -m -- "${1:?WORK_DIR}"); DEST=$(realpath -m -- "${2:?DEST_DIR}")
[ -e "$WORK" ] && { echo "refusing existing WORK_DIR $WORK" >&2; exit 2; }
mkdir -p "$WORK" "$DEST"
"$TOOLS/posttran_e2e_inputs.sh" "$WORK/in" > /dev/null
o() { local name=$1; shift; "$@" > "$WORK/$name.log" 2>&1; mkdir -p "$DEST/$name"; cp "$WORK/$name/expected/"* "$DEST/$name/"; echo "$name RC=$(cat "$DEST/$name/RETURN-CODE")"; }
o baseline "$TOOLS/posttran_gnucobol_oracle.sh" "$WORK/baseline" "$WORK/in/dailytran.txt"
o rerun env ACCTDATA_FILE="$WORK/baseline/expected/ACCTDATA.unl" TCATBAL_FILE="$WORK/baseline/expected/TCATBALF.unl" \
  "$TOOLS/posttran_gnucobol_oracle.sh" "$WORK/rerun" "$WORK/in/dailytran.txt"
o empty "$TOOLS/posttran_gnucobol_oracle.sh" "$WORK/empty" "$WORK/in/empty.txt"
o restart-leg1 "$TOOLS/posttran_gnucobol_oracle.sh" "$WORK/restart-leg1" "$WORK/in/dup151.txt"
o restart-leg1-atomic "$TOOLS/posttran_gnucobol_oracle.sh" "$WORK/restart-leg1-atomic" "$WORK/in/first150.txt"
o cp037 env DALYTRAN_ENCODING=CP037 "$TOOLS/posttran_gnucobol_oracle.sh" "$WORK/cp037" "$WORK/in/DALYTRAN.cp037"
# DALYTRAN DD naming a file that does not exist: CBTRN02C opens DALYTRAN first (cbl:236), so no other file is touched
A="$WORK/abend-dalytran-missing"; mkdir -p "$A/expected" "$A/data"
set +e
( cd "$WORK/baseline/bin" && COB_LIBRARY_PATH="$WORK/baseline/bin" DD_DALYTRAN="$A/data/NO-SUCH-DALYTRAN" \
  DD_TRANFILE="$A/data/TRANSACT" DD_XREFFILE="$A/data/CARDXREF" DD_DALYREJS="$A/expected/DALYREJS" \
  DD_ACCTFILE="$A/data/ACCTDATA" DD_TCATBALF="$A/data/TCATBALF" COB_CURRENT_DATE='2022/07/18 10:30:45' ./CBTRN02C > "$A/expected/SYSOUT" 2>&1 )
echo $? > "$A/expected/RETURN-CODE"
set -e
mkdir -p "$DEST/abend-dalytran-missing"; cp "$A/expected/"* "$DEST/abend-dalytran-missing/"
echo "abend-dalytran-missing RC=$(cat "$A/expected/RETURN-CODE") DALYREJS created: $([ -e "$A/expected/DALYREJS" ] && echo yes || echo no)"

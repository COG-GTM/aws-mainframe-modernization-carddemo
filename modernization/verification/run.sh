#!/usr/bin/env bash
# Golden-set harness: POSTTRAN -> INTCALC -> TRANREPT -> CREASTMT under GnuCOBOL and through the Java
# CLI on a freshly loaded database, then a file-for-file / field-for-field diff per job.
#
#   modernization/verification/run.sh [<work-dir>]        (default modernization/verification/target)
#
# <work-dir>/cobol/<JOB>/   the golden set (reference.py)
# <work-dir>/java/<JOB>/    the Java outputs and table exports (java.sh)
# <work-dir>/reconciliation/<JOB>/reconciliation.md   (compare.py)
# Exit status 0 only when all four jobs reconcile. Needs cobc (GnuCOBOL 3.x), python3, docker compose.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
WORK="$(realpath -m "${1:-$HERE/target}")"
mkdir -p "$WORK"

echo "== GnuCOBOL reference chain"
python3 "$HERE/reference.py" "$WORK/cobol"
echo "== Java chain (docker compose, fresh database)"
"$HERE/java.sh" "$WORK/java"
echo "== Reconciliation"
python3 "$HERE/compare.py" "$WORK/cobol" "$WORK/java" "$WORK/reconciliation"

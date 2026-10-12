#!/usr/bin/env bash
# One-command golden-set equivalence run for the posting slice (CBTRN02C ->
# transaction-posting-service).
#
#   bash scripts/run-equivalence.sh [--set named|volume] [--skip-reference]
#
# Step 1 re-runs the reference: compiles unmodified app/cbl/CBTRN02C.cbl with
# cobc (GnuCOBOL) and posts both golden input sets, then verifies the
# committed expected outputs are unchanged. Step 2 builds the Java candidate
# clean and runs it under faketime(1) for each set. Step 3 prints the
# equivalence table (tests/golden/equivalence_table.py) and writes
# tests/golden/.work/equivalence-summary.{md,json}.
#
# GS_MVN_ARGS: extra Maven arguments passed to the candidate build, e.g.
#   GS_MVN_ARGS="-s /path/to/settings.xml"   # repository mirror
#
# The full console output is also written to
# tests/golden/.work/run-equivalence.log.
#
# Exit 0 only if every selected set passes (compare.py exit 0 and every gated
# row PASS).
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

LOG_TMP="$(mktemp)"
exec > >(tee "$LOG_TMP") 2>&1
trap 'rc=$?; mkdir -p tests/golden/.work && cp "$LOG_TMP" tests/golden/.work/run-equivalence.log; rm -f "$LOG_TMP"; exit $rc' EXIT

SET_FILTER=""
SKIP_REFERENCE=0
while [ $# -gt 0 ]; do
  case "$1" in
    --set)
      case "$2" in named|volume) ;; *) echo "--set must be named or volume" >&2; echo "usage: $0 [--set named|volume] [--skip-reference]" >&2; exit 64 ;; esac
      SET_FILTER="$2"; shift 2 ;;
    --skip-reference) SKIP_REFERENCE=1; shift ;;
    *) echo "usage: $0 [--set named|volume] [--skip-reference]" >&2; exit 64 ;;
  esac
done

# preflight
miss=0
for tool in java mvn cobc faketime python3; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "missing required tool: $tool" >&2
    miss=1
  fi
done
JAVA_VER="$(java -version 2>&1 | head -1 || true)"
case "$JAVA_VER" in
  *"21."*|*'"21'*) : ;;
  *) echo "java 21 required (found: $JAVA_VER); set JAVA_HOME" >&2; miss=1 ;;
esac
[ "$miss" -eq 0 ] || exit 69

SETS="named volume"
[ -n "$SET_FILTER" ] && SETS="$SET_FILTER"

# step 1: reference run proves the committed expected outputs came from
# compiling and running unmodified app/cbl/CBTRN02C.cbl under GnuCOBOL here
if [ "$SKIP_REFERENCE" -eq 0 ]; then
  echo "== step 1: reference run (GnuCOBOL CBTRN02C)"
  bash tests/golden/run_reference.sh --keep-inputs
  if ! git diff --quiet -- tests/golden/sets/*/expected; then
    echo "reference outputs differ from committed golden set" >&2
    exit 1
  fi
else
  echo "== step 1: reference run skipped (--skip-reference)"
fi

# step 2: candidate runs
for s in $SETS; do
  echo "== step 2: candidate run, set $s"
  # the verdict comes from reconciliation.json (read by equivalence_table.py),
  # not from this runner's shell exit
  bash tests/golden/run_candidate_pr9.sh . --set "$s" || true
done

# step 3: equivalence table
echo "== step 3: equivalence table"
set +e
python3 tests/golden/equivalence_table.py $SETS
TBL=$?
set -e
exit "$TBL"

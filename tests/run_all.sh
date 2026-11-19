#!/usr/bin/env bash
# CardDemo test suite runner: static checks, copybook validation,
# CSUTLDTC unit tests and GnuCOBOL batch program tests.
# Exit code 0 only if every suite passes.
set -uo pipefail
TESTS="$(cd "$(dirname "$0")" && pwd)"
LOGDIR="${CARDDEMO_TEST_LOGS:-$TESTS/.build/logs}"
mkdir -p "$LOGDIR"

if ! command -v cobc >/dev/null 2>&1; then
  echo "ERROR: GnuCOBOL (cobc) not found. Install it, e.g. 'sudo apt-get install gnucobol'." >&2
  exit 2
fi
echo "Using $(cobc --version | head -1)"

declare -a NAMES RESULTS
run_suite() {
  local name="$1"; shift
  local log="$LOGDIR/${name// /_}.log"
  printf '\n=== %s ===\n' "$name"
  if "$@" >"$log" 2>&1; then
    RESULTS+=("PASS"); tail -n 3 "$log"
  else
    RESULTS+=("FAIL"); cat "$log"
  fi
  NAMES+=("$name")
}

run_suite "JCL and COPY reference lint"   python3 "$TESTS/static/test_references.py" -v
run_suite "Batch program compile check"   python3 "$TESTS/static/test_batch_compile.py" -v
run_suite "Copybook layout validation"    python3 "$TESTS/validation/test_copybooks.py" -v
run_suite "CSUTLDTC date utility unit"    "$TESTS/unit/run_csutldtc.sh"
run_suite "Batch programs (GnuCOBOL)"     "$TESTS/batch/run_batch_tests.sh"

printf '\n==================== SUMMARY ====================\n'
fail=0
for i in "${!NAMES[@]}"; do
  printf '%-4s  %s\n' "${RESULTS[$i]}" "${NAMES[$i]}"
  [[ "${RESULTS[$i]}" == PASS ]] || fail=1
done
echo "CICS online programs: specification only (tests/cics/), pending a CICS region."
echo "Logs: $LOGDIR"
if (( fail )); then echo "RESULT: FAIL"; else echo "RESULT: PASS"; fi
exit $fail

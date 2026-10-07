#!/usr/bin/env bash
# Build, load, run both programs and regenerate every golden file.
# This is the exact command sequence used to produce golden-files/.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
"$HERE/build.sh"
"$HERE/load_ksds.sh"
"$HERE/run_cbact01c.sh"
"$HERE/run_cbtrn01c.sh"
"$HERE/run_synthetic_rejections.sh"
python3 "$REPO/test-harness/generate_goldens.py" CBACT01C
python3 "$REPO/test-harness/generate_goldens.py" CBTRN01C
python3 "$REPO/test-harness/generate_goldens.py" CBTRN01C \
    --work "$HERE/work/synthetic-rejections/CBTRN01C" \
    --out  "$REPO/golden-files/CBTRN01C/synthetic-rejections" \
    --dailytran "$HERE/work/synthetic-rejections/fixtures/dailytran.txt" \
    --cardxref  "$HERE/work/synthetic-rejections/fixtures/cardxref.txt"

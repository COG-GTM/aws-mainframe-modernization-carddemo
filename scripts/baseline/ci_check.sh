#!/usr/bin/env bash
# CI gate for the GnuCOBOL baseline (make baseline-check, .github/workflows/modernization-ci.yml `baseline` job):
# rebuilds and reruns every batch job with --fast, asserts the summary line, and fails if any committed
# output under docs/validation/baseline/ changed. The phase-6 `equivalence` job builds on this.
set -euo pipefail
cd "$(dirname "$0")/../.."
expected="jobs=26 compile failures=0"
log="$(mktemp)"
trap 'rm -f "$log"' EXIT

cobc --version | head -1
scripts/baseline/run_baseline.sh --fast | tee "$log"

if ! grep -q "$expected" "$log"; then
    echo "::error::baseline summary mismatch: expected '$expected', got '$(tail -1 "$log")'" >&2
    exit 1
fi
if [ -n "$(git status --porcelain -- docs/validation/baseline)" ]; then
    git status --short -- docs/validation/baseline
    git --no-pager diff --stat -- docs/validation/baseline
    echo "::error::docs/validation/baseline differs from the committed baseline" >&2
    exit 1
fi
echo "baseline OK: $expected, outputs identical to docs/validation/baseline/"

#!/usr/bin/env bash
# Regenerate the GnuCOBOL batch baseline under docs/validation/baseline/.
#   scripts/baseline/run_baseline.sh          # full run (WAITSTEP waits 36 s like the JCL)
#   scripts/baseline/run_baseline.sh --fast   # skip the wait; outputs are identical
set -euo pipefail
cd "$(dirname "$0")/../.."
command -v cobc >/dev/null || { echo "cobc (GnuCOBOL) not found" >&2; exit 1; }
exec python3 scripts/baseline/baseline.py "$@"

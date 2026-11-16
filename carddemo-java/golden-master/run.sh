#!/usr/bin/env bash
# CardDemo batch golden master (GnuCOBOL). See README.md.
#   run.sh           regenerate baselines/
#   run.sh --check   rerun twice and fail on any difference from baselines/
set -euo pipefail
exec python3 "$(dirname "${BASH_SOURCE[0]}")/golden_master.py" "$@"

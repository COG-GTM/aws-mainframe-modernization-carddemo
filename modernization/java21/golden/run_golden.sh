#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
export TZ=UTC LC_ALL=C
exec python3 golden.py

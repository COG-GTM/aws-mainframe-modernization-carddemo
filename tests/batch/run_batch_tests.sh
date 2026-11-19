#!/usr/bin/env bash
# Run the GnuCOBOL batch program tests (CBTRN01C/02C/03C, CBACT04C, CBSTM03A/B).
set -euo pipefail
cd "$(dirname "$0")"
exec python3 -m unittest -v test_cbtrn01c test_cbtrn02c test_cbtrn03c test_cbact04c test_cbstm03 "$@"

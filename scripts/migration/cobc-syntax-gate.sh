#!/usr/bin/env bash
# GnuCOBOL syntax gate for the 14 non-CICS programs in app/cbl (see docs/migration/scope.md §5).
# CBEXPORT, CBIMPORT and CBSTM03A are checked through compile shims built in a temporary
# directory; app/ is never modified.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"
SHIM="$(mktemp -d)"
trap 'rm -rf "$SHIM"' EXIT
mkdir -p "$SHIM/cpy" "$SHIM/cbl"

# CBSTM03A: CUSTREC.cpy lines 6-22 start with tab characters. Expand them so the
# entries stay inside columns 8-72.
expand -t 1 app/cpy/CUSTREC.cpy > "$SHIM/cpy/CUSTREC.cpy"

# CBEXPORT / CBIMPORT: RECORD KEY names EXPORT-SEQUENCE-NUM, a WORKING-STORAGE field
# (CVEXPORT). Give the FD record a key field at the same offset (27, PIC 9(9) COMP).
shim_export_key() {
  local pgm="$1" rec="$2"
  sed -e "s/RECORD KEY IS EXPORT-SEQUENCE-NUM/RECORD KEY IS SHIM-EXP-SEQ-KEY/" \
      -e "s/^       01  $rec  *PIC X(500)\./       01  $rec.\n           05  FILLER PIC X(27).\n           05  SHIM-EXP-SEQ-KEY PIC 9(9) COMP.\n           05  FILLER PIC X(469)./" \
      "app/cbl/$pgm.cbl" > "$SHIM/cbl/$pgm.cbl"
}
shim_export_key CBEXPORT EXPORT-OUTPUT-RECORD
shim_export_key CBIMPORT EXPORT-INPUT-RECORD

fail=0
check() {
  local pgm="$1" src="$2" mode="$3"; shift 3
  if cobc -fsyntax-only -std=ibm "$@" "$src" >/dev/null 2>&1; then
    printf '%-9s %-6s OK\n' "$pgm" "$mode"
  else
    printf '%-9s %-6s FAIL\n' "$pgm" "$mode"; fail=1
  fi
}

for pgm in CBACT01C CBACT02C CBACT03C CBACT04C CBCUS01C CBSTM03B CBTRN01C CBTRN02C CBTRN03C COBSWAIT CSUTLDTC; do
  check "$pgm" "$(ls app/cbl/$pgm.* | head -1)" as-is -I app/cpy
done
check CBEXPORT "$SHIM/cbl/CBEXPORT.cbl" shim -I app/cpy
check CBIMPORT "$SHIM/cbl/CBIMPORT.cbl" shim -I app/cpy
check CBSTM03A app/cbl/CBSTM03A.CBL shim -I "$SHIM/cpy" -I app/cpy

exit "$fail"

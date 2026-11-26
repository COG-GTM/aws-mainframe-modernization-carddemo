#!/usr/bin/env bash
# Regenerates src/test/resources/golden/csutldtc-golden.txt by compiling the
# real app/cbl/CSUTLDTC.cbl with GnuCOBOL (-std=ibm, big-endian BINARY) against
# a CEEDAYS stub. Output columns: date|format|sev|msg|neg|rc|hex(LS-RESULT).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../../../../.." && pwd)"
OUT="$HERE/../resources/golden/csutldtc-golden.txt"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
cobc -x -std=ibm -o "$WORK/drvdtc" "$HERE/DRVDTC.cbl" "$REPO/app/cbl/CSUTLDTC.cbl" "$HERE/CEEDAYS.cbl"
CASES=(
  "2022-06-10|YYYY-MM-DD|0|0|N"
  "2024-02-29|YYYY-MM-DD|0|0|N"
  "2023-02-30|YYYY-MM-DD|3|2507|N"
  "2023-02-30|YYYY-MM-DD|3|2508|N"
  "2023-02-30|YYYY-MM-DD|3|2509|N"
  "0001-01-01|YYYY-MM-DD|3|2513|N"
  "2023-13-01|YYYY-MM-DD|3|2517|N"
  "2023-01-01|XXXX-XX-XX|3|2518|N"
  "ABCD-EF-GH|YYYY-MM-DD|3|2520|N"
  "2023-01-01|YYYY-MM-DD|3|2521|N"
  "2023-01-01|YYYY-MM-DD|3|2510|N"
  "2023-01-01|YYYY-MM-DD|1|2508|N"
  "2023-01-01|YYYY-MM-DD|2|2508|Y"
  "          |YYYY-MM-DD|3|2507|N"
  "20230101  |YYYYMMDD  |0|0|N"
)
: > "$OUT"
for c in "${CASES[@]}"; do
  IFS='|' read -r d f s m n <<<"$c"
  raw="$(T_DATE="$d" T_FMT="$f" T_SEV="$s" T_MSG="$m" T_NEG="$n" "$WORK/drvdtc" | xxd -p | tr -d '\n')"
  res="${raw:0:160}"
  rc="$(printf '%s' "${raw:160}" | xxd -r -p)"
  echo "$d|$f|$s|$m|$n|$((10#${rc#+}))|$res" >> "$OUT"
done
echo "wrote $OUT"

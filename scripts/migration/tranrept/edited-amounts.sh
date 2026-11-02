#!/usr/bin/env bash
# Writes the GnuCOBOL rendering of CVTRA07Y's amount pictures (-ZZZ,ZZZ,ZZZ.ZZ and +ZZZ,ZZZ,ZZZ.ZZ)
# for every value in edited-amounts/AMOUNTS.txt to edited-amounts/expected.txt (test resource of
# batch-service EditedAmountTest).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
DIR="$ROOT/modernization/batch-service/src/test/resources/tranrept-edited-amounts"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
cobc -x -std=ibm -o "$tmp/EDITPIC" "$ROOT/scripts/migration/tranrept/EDITPIC.cbl"
DD_AMOUNTS="$DIR/AMOUNTS.txt" "$tmp/EDITPIC" > "$DIR/expected.txt"
wc -l "$DIR/expected.txt"

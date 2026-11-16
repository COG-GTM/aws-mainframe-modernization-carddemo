#!/usr/bin/env bash
# Regenerates the CBACT04C (INTCALC) golden outputs used by IntcalcParityTest.
#   base: app/data/ASCII sample files      edge: parity/intcalc/datasets/edge
# Output: java-batch/src/test/resources/golden/intcalc/<dataset>/{transact,acctdata-after}.dat
# TRAN-ORIG-TS / TRAN-PROC-TS carry the wall-clock run time and are masked by the parity test.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$here/../../.." && pwd)"
golden="$repo_root/java-batch/src/test/resources/golden/intcalc"
run_date="${1:-2022071800}"
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
for dataset in base edge; do
  src="$repo_root/app/data/ASCII"
  [ "$dataset" = edge ] && src="$here/datasets/edge"
  "$here/run-cobol-oracle.sh" "$src" "$scratch/$dataset" "$run_date"
  mkdir -p "$golden/$dataset"
  cp "$scratch/$dataset/transact.dat" "$scratch/$dataset/acctdata-after.dat" "$golden/$dataset/"
done
echo "golden outputs written to $golden"

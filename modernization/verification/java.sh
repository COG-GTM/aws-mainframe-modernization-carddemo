#!/usr/bin/env bash
# Java side of the golden-set harness: the same monthly chain through the services' CLI on a freshly
# loaded database.
#
#   modernization/verification/java.sh <out-dir> [--keep]
#
# 1. docker compose (project carddemo-verify, no published ports): down -v, then up --build --wait the
#    five services, batch-service and PostgreSQL; Flyway creates the five schemas.
# 2. data-loader loads app/data into them (its own reconciliation must pass).
# 3. CLI launches, in the Control-M order, each job reading the tables the previous one left:
#      transaction-service --job=POSTTRAN --date=2022-07-18
#      transaction-service --job=INTCALC  --parm=2022071800        (once: a rerun with the same PARM is refused)
#      batch-service       --job=TRANREPT --parm='<DATEPARM record>'
#      batch-service       --job=CREASTMT
#    after POSTTRAN and INTCALC, `data-loader --export` unloads the five schemas to fixed-width files
#    (TableExport, RecordWriter): EXPORT-POSTTRAN/, EXPORT-INTCALC/.
# <out-dir>/<JOB>/ receives the job's output generation (DD files + RETURN-CODE + SYSOUT).
# Env: GOLDEN_DATE (2022-07-18), GOLDEN_PARM (2022071800), GOLDEN_DATEPARM ('2022-01-01 2099-12-31'),
#      MAVEN_MIRROR_URL (passed to the image build).
set -euo pipefail

OUT="$(realpath -m "${1:?usage: $0 <out-dir> [--keep]}")"
KEEP="${2:-}"
HERE="$(cd "$(dirname "$0")" && pwd)"
cd "$HERE/.."
DATE="${GOLDEN_DATE:-2022-07-18}"
PARM="${GOLDEN_PARM:-2022071800}"
DATEPARM="${GOLDEN_DATEPARM:-2022-01-01 2099-12-31}"

dc() { docker compose -p carddemo-verify -f docker-compose.yml -f verification/compose.verify.yml "$@"; }
cleanup() {
    local rc=$?
    if [ "$rc" -ne 0 ]; then dc --profile tools logs --no-color --tail=200 || true; fi
    if [ "$KEEP" != "--keep" ]; then dc --profile tools down -v --remove-orphans >/dev/null 2>&1 || true; fi; }
trap cleanup EXIT

rm -rf "$OUT"; mkdir -p "$OUT/gen"; chmod 777 "$OUT/gen"
dc --profile tools down -v --remove-orphans >/dev/null 2>&1 || true
dc up -d --build --wait --wait-timeout 300
dc --profile tools run --rm -T --build data-loader | tee "$OUT/data-loader.log" | tail -3

# A CLI launch: one more container of the service's image with the job arguments, output on the host.
cli() {
  local service="$1" job="$2"; shift 2
  local rc=0
  dc run --rm -T --no-deps -u "$(id -u):$(id -g)" -v "$OUT/gen:/batch-out" "$service" \
      --job="$job" --output-dir=/batch-out "$@" > "$OUT/gen/$job.log" 2>&1 || rc=$?
  local gen; gen="$(ls -d "$OUT/gen/$job"/*/ 2>/dev/null | tail -1)"
  if [ -z "$gen" ]; then cat "$OUT/gen/$job.log"; echo "$job produced no output generation (exit $rc)"; exit 1; fi
  mkdir -p "$OUT/$job"; cp -r "$gen". "$OUT/$job/"; cp "$OUT/gen/$job.log" "$OUT/$job/SYSOUT"
  echo "$job exit $rc: $(ls "$OUT/$job" | tr '\n' ' ')"
}
export_tables() {
  dc --profile tools run --rm -T -u "$(id -u):$(id -g)" -v "$OUT/gen:/batch-out" data-loader \
      --export "/batch-out/EXPORT-$1" > "$OUT/gen/export-$1.log"
  cp -r "$OUT/gen/EXPORT-$1" "$OUT/EXPORT-$1"
}

cli transaction-service POSTTRAN --date="$DATE"
export_tables POSTTRAN
cli transaction-service INTCALC --parm="$PARM"
export_tables INTCALC
cli batch-service TRANREPT --parm="$DATEPARM"
cli batch-service CREASTMT
rm -rf "$OUT/gen"

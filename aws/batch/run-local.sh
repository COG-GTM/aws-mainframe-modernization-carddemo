#!/usr/bin/env bash
# Local runner for the CardDemo batch jobs: PostgreSQL in Docker (Aurora stand-in) + a local directory as the
# S3 bucket. Usage:
#   ./run-local.sh up                         start postgres, create schema, seed bucket + tables from app/data/ASCII
#   ./run-local.sh job <job-name> [--k=v ...]  run one job (e.g. job post-daily-transactions --businessDate=2022-07-18)
#   ./run-local.sh daily-cycle [yyyy-MM-dd]   run the daily-cycle order (month-start / Saturday branches by date)
#   ./run-local.sh psql                       open psql on the local database
#   ./run-local.sh down                       stop and remove the container
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
CONTAINER="${CARDDEMO_PG_CONTAINER:-carddemo-batch-pg}"
export DB_HOST="${DB_HOST:-localhost}" DB_PORT="${DB_PORT:-55432}" DB_NAME="${DB_NAME:-carddemo}"
export DB_USER="${DB_USER:-carddemo}" DB_PASSWORD="${DB_PASSWORD:-carddemo}" DB_SCHEMA="${DB_SCHEMA:-carddemo}"
BUCKET_DIR="${CARDDEMO_LOCAL_BUCKET:-$HERE/target/local-bucket}"
JAR="$HERE/target/carddemo-batch.jar"
JAVA="${JAVA_HOME:+$JAVA_HOME/bin/}java"

jar() {
  if [[ ! -f "$JAR" ]]; then
    (cd "$HERE" && mvn -B -q -DskipTests package)
  fi
}

run_job() {
  jar
  local job="$1"; shift
  set +e
  "$JAVA" -jar "$JAR" --spring.profiles.active=local --carddemo.storage.local-dir="$BUCKET_DIR" \
    --job="$job" "$@"
  local exit=$?
  set -e
  echo "[run-local] $job exit=$exit"
  return $exit
}

up() {
  if ! docker ps --format '{{.Names}}' | grep -qx "$CONTAINER"; then
    docker run -d --rm --name "$CONTAINER" -p "$DB_PORT:5432" -e POSTGRES_DB="$DB_NAME" \
      -e POSTGRES_USER="$DB_USER" -e POSTGRES_PASSWORD="$DB_PASSWORD" postgres:16-alpine >/dev/null
  fi
  # the image's first start runs a temporary init server and restarts; wait for the final one
  until docker logs "$CONTAINER" 2>&1 | grep -q 'PostgreSQL init process complete\|Skipping initialization' \
      && docker exec "$CONTAINER" pg_isready -h 127.0.0.1 -U "$DB_USER" -d "$DB_NAME" >/dev/null 2>&1; do
    sleep 1
  done
  docker exec -i "$CONTAINER" psql -q -v ON_ERROR_STOP=1 -U "$DB_USER" -d "$DB_NAME" \
    < "$REPO/aws/db/schema.sql"
  mkdir -p "$BUCKET_DIR/seed/ascii"
  cp "$REPO"/app/data/ASCII/*.txt "$BUCKET_DIR/seed/ascii/"
  local seed_run="seed-$(date -u +%s)"
  for t in customer account card card_xref transaction_type transaction_category disclosure_group tran_cat_balance; do
    run_job load-reference-data --runId="$seed_run-$t" --table="$t"
  done
  echo "[run-local] ready: postgres on $DB_HOST:$DB_PORT, bucket $BUCKET_DIR"
}

daily_cycle() {
  local d="${1:-$(date -u +%F)}"
  local run="daily-${d//-/}-$(date -u +%H%M%S)"
  local input="$BUCKET_DIR/input/dalytran/$d/dalytran.txt"
  if [[ ! -f "$input" ]]; then
    mkdir -p "$(dirname "$input")"
    cp "$REPO/app/data/ASCII/dailytran.txt" "$input"
  fi
  # post-daily-transactions: RC 4 (rejects) -> exit 0, the cycle continues (warning in runs/<runId>/…json)
  run_job post-daily-transactions --runId="$run" --businessDate="$d"
  sleep "${WAIT_SECONDS:-0}"   # WAITSTEP
  run_job backup-transactions --runId="$run" --businessDate="$d"
  if [[ "$(date -u -d "$d" +%d)" == "01" || "${MONTH_START:-}" == "true" ]]; then
    run_job calculate-interest --runId="$run" --businessDate="$d" --parmDate="${d//-/}00"
    run_job combine-transactions --runId="$run" --businessDate="$d"
    run_job create-statements --runId="$run" --businessDate="$d"
    run_job statement-pdf --runId="$run" --businessDate="$d"
  fi
  if [[ "$(date -u -d "$d" +%u)" == "6" || "${SATURDAY:-}" == "true" ]]; then
    if [[ -f "$BUCKET_DIR/input/trantype-maint/$d/maint.txt" ]]; then
      run_job maintain-transaction-types --runId="$run" --businessDate="$d"
    fi
    run_job extract-transaction-types --runId="$run" --businessDate="$d"
    run_job backup-reference-data --runId="$run" --businessDate="$d" --table=disclosure_group
    run_job load-reference-data --runId="$run" --businessDate="$d" --table=disclosure_group
  fi
  echo "[run-local] daily cycle $run complete; run results under $BUCKET_DIR/runs/$run/"
}

case "${1:-}" in
  up) up ;;
  job) shift; run_job "$@" ;;
  daily-cycle) shift; daily_cycle "$@" ;;
  psql) docker exec -it "$CONTAINER" psql -U "$DB_USER" -d "$DB_NAME" ;;
  down) docker rm -f "$CONTAINER" >/dev/null && echo "[run-local] stopped" ;;
  *) sed -n '2,9p' "$0"; exit 2 ;;
esac

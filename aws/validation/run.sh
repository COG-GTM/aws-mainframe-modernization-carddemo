#!/usr/bin/env bash
# One-command CardDemo validation: brings up PostgreSQL 15 (aws/docker-compose.yml), seeds it through aws/etl
# from app/data/EBCDIC (cross-checked against app/data/ASCII), starts the online services and runs the online +
# batch parity suites. Extra arguments are passed to pytest (e.g. `./run.sh -m online`, `./run.sh -k billpay`).
#
# Environment:
#   VALIDATION_SKIP_STACK=1    use an already running stack (VALIDATION_API_BASE / VALIDATION_DB_DSN)
#   VALIDATION_PG_PORT=55433   host port for the compose postgres service
#   VALIDATION_API_PORT=18080  port for the online services started by this script
#   JAVA_HOME                  JDK 21 (defaults to /usr/lib/jvm/java-21-openjdk-amd64 when present)
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AWS="$(cd "$HERE/.." && pwd)"
TARGET="$HERE/target"
VENV="${VALIDATION_VENV:-$TARGET/venv}"
PG_PORT="${VALIDATION_PG_PORT:-55433}"
API_PORT="${VALIDATION_API_PORT:-18080}"
if [[ -z "${JAVA_HOME:-}" && -d /usr/lib/jvm/java-21-openjdk-amd64 ]]; then
  export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
fi
export PATH="${JAVA_HOME:+$JAVA_HOME/bin:}$PATH"
export VALIDATION_DB_DSN="${VALIDATION_DB_DSN:-postgresql://carddemo:carddemo@localhost:$PG_PORT/carddemo}"
export VALIDATION_API_BASE="${VALIDATION_API_BASE:-http://localhost:$API_PORT}"
mkdir -p "$TARGET"

log() { echo "[validation] $*"; }

if [[ ! -x "$VENV/bin/python" ]]; then
  log "creating virtualenv $VENV"
  python3 -m venv "$VENV"
fi
"$VENV/bin/pip" install -q -r "$HERE/requirements.txt" -r "$AWS/etl/requirements.txt"

for module in services batch; do
  jar=$(ls "$AWS/$module"/target/carddemo-*.jar 2>/dev/null | head -1 || true)
  if [[ -z "$jar" || -n "${VALIDATION_REBUILD:-}" ]]; then
    log "building aws/$module"
    (cd "$AWS/$module" && mvn -B -q -DskipTests package)
  fi
done

SVC_PID=""
cleanup() { [[ -n "$SVC_PID" ]] && kill "$SVC_PID" 2>/dev/null || true; }
trap cleanup EXIT

if [[ -z "${VALIDATION_SKIP_STACK:-}" ]]; then
  log "starting postgres:15-alpine on :$PG_PORT"
  (cd "$AWS" && POSTGRES_IMAGE=postgres:15-alpine POSTGRES_PORT="$PG_PORT" docker compose up -d --wait postgres)

  log "ETL: EBCDIC -> CSV, crosscheck against ASCII, load"
  (cd "$AWS/etl" && "$VENV/bin/python" -m etl convert-all && "$VENV/bin/python" -m etl crosscheck \
    && "$VENV/bin/python" -m etl load --dsn "$VALIDATION_DB_DSN" --apply-schema) | tee "$TARGET/etl.log"

  log "starting online services on :$API_PORT"
  DB_HOST=localhost DB_PORT="$PG_PORT" DB_NAME=carddemo DB_USER=carddemo DB_PASSWORD=carddemo \
    SERVER_PORT="$API_PORT" JWT_SECRET="validation-only-jwt-secret-0123456789abcdef0123456789" \
    java -jar "$AWS/services/target/carddemo-online-services.jar" > "$TARGET/online-services.log" 2>&1 &
  SVC_PID=$!
  for _ in $(seq 1 90); do
    curl -fs "$VALIDATION_API_BASE/actuator/health" >/dev/null 2>&1 && break
    kill -0 "$SVC_PID" 2>/dev/null || { tail -50 "$TARGET/online-services.log"; exit 1; }
    sleep 1
  done
  curl -fs "$VALIDATION_API_BASE/actuator/health" >/dev/null
fi

log "running pytest (report: $TARGET/junit.xml)"
cd "$HERE"
"$VENV/bin/python" -m pytest --junitxml="$TARGET/junit.xml" "$@"

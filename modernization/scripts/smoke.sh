#!/usr/bin/env bash
# Smoke test for a running stack (docker compose up, or services started by hand).
# Waits for every service's /actuator/health to report UP, checks each OpenAPI document and
# Swagger UI and signs on with the seeded demo user. Read-only by default.
# RUN_BATCH=1 also runs both batch jobs. They change data: pending daily transactions are posted and
# interest is charged again on the current category balances on every run (CBACT04C does not clear
# TCATBAL either), so use it only against a disposable database, e.g. right after
# `docker compose down -v && docker compose up -d --build` (what CI does).
# Ports default to docker-compose.yml; override with AUTH_PORT ... TRANSACTION_PORT.
set -euo pipefail

HOST="${HOST:-localhost}"
WAIT_SECONDS="${WAIT_SECONDS:-180}"
RUN_BATCH="${RUN_BATCH:-0}"

declare -A PORTS=(
  [auth-service]="${AUTH_PORT:-8080}"
  [customer-service]="${CUSTOMER_PORT:-8081}"
  [account-service]="${ACCOUNT_PORT:-8082}"
  [card-service]="${CARD_PORT:-8083}"
  [transaction-service]="${TRANSACTION_PORT:-8084}"
)
SERVICES=(auth-service customer-service account-service card-service transaction-service)

fail() { echo "FAIL: $*" >&2; exit 1; }

for svc in "${SERVICES[@]}"; do
  url="http://${HOST}:${PORTS[$svc]}/actuator/health"
  deadline=$((SECONDS + WAIT_SECONDS))
  until curl -fsS "$url" 2>/dev/null | grep -q '"status":"UP"'; do
    (( SECONDS < deadline )) || fail "$svc not UP at $url after ${WAIT_SECONDS}s"
    sleep 3
  done
  echo "ok  $svc health UP          $url"
done

for svc in "${SERVICES[@]}"; do
  base="http://${HOST}:${PORTS[$svc]}"
  curl -fsS -o /dev/null "$base/v3/api-docs" || fail "$svc $base/v3/api-docs"
  curl -fsSL -o /dev/null "$base/swagger-ui.html" || fail "$svc $base/swagger-ui.html"
  echo "ok  $svc OpenAPI + Swagger UI  $base/swagger-ui.html"
done

signon=$(curl -fsS -X POST "http://${HOST}:${PORTS[auth-service]}/api/v1/auth/signon" \
  -H 'Content-Type: application/json' -d '{"userId":"ADMIN001","password":"Password1"}') \
  || fail "signon ADMIN001"
echo "$signon" | grep -q '"nextScreen":"ADMIN"' || fail "signon returned $signon"
echo "ok  signon ADMIN001 -> $signon"

if [[ "$RUN_BATCH" == "1" ]]; then
  for job in post-transactions interest-calculation; do
    out=$(curl -fsS -X POST "http://${HOST}:${PORTS[transaction-service]}/api/v1/batch/$job") \
      || fail "batch $job"
    echo "$out" | grep -q '"status":"COMPLETED"' || fail "batch $job returned $out"
    echo "ok  batch $job -> $out"
  done
fi

echo "smoke test passed"

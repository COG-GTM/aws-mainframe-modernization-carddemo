#!/usr/bin/env bash
# Smoke test for a running stack (docker compose up, or services started by hand).
# Waits for every service's /actuator/health to report UP, checks each OpenAPI document and
# Swagger UI and signs on as ADMIN001 and USER0001, USRSEC sample users, so load the sample data first
# (docker compose run --rm data-loader). Uses the returned bearer token on one business endpoint per
# service (and checks 401 without it), the menu and the admin-only user API. Read-only by default.
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
  [batch-service]="${BATCH_PORT:-8085}"
)
SERVICES=(auth-service customer-service account-service card-service transaction-service batch-service)

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

AUTH="http://${HOST}:${PORTS[auth-service]}"
token_of() { sed -n 's/.*"token":"\([^"]*\)".*/\1/p'; }
code_of() { curl -sS -o /dev/null -w '%{http_code}' "$@"; }

signon=$(curl -fsS -X POST "$AUTH/api/v1/auth/signon" \
  -H 'Content-Type: application/json' -d '{"userId":"ADMIN001","password":"PASSWORD"}') \
  || fail "signon ADMIN001"
echo "$signon" | grep -q '"nextScreen":"ADMIN"' || fail "signon returned $signon"
TOKEN=$(echo "$signon" | token_of)
[[ -n "$TOKEN" ]] || fail "signon ADMIN001 returned no token: $signon"
BEARER=(-H "Authorization: Bearer $TOKEN")
echo "ok  signon ADMIN001 -> nextScreen ADMIN, token issued"

wrong=$(curl -sS -X POST "$AUTH/api/v1/auth/signon" -H 'Content-Type: application/json' \
  -d '{"userId":"ADMIN001","password":"WRONGPWD"}')
echo "$wrong" | grep -q 'Wrong Password. Try again ...' || fail "wrong password returned $wrong"
echo "ok  signon wrong password -> COBOL message"

# One business endpoint per service: 401 without a token, 200 with the ADMIN001 token.
declare -A PROBE=(
  [auth-service]="/api/v1/users?size=1"
  [customer-service]="/api/v1/customers?size=1"
  [account-service]="/api/v1/accounts?size=1"
  [card-service]="/api/v1/cards?size=1"
  [transaction-service]="/api/v1/transactions?size=1"
)
for svc in "${SERVICES[@]}"; do
  url="http://${HOST}:${PORTS[$svc]}${PROBE[$svc]}"
  code=$(code_of "$url"); [[ "$code" == "401" ]] || fail "$svc $url without token -> $code, want 401"
  code=$(code_of "${BEARER[@]}" "$url"); [[ "$code" == "200" ]] || fail "$svc $url with token -> $code, want 200"
  echo "ok  $svc 401 without token, 200 with token  ${PROBE[$svc]}"
done

menu=$(curl -fsS "${BEARER[@]}" "$AUTH/api/v1/menu") || fail "menu ADMIN001"
echo "$menu" | grep -q '"menu":"ADMIN"' || fail "menu ADMIN001 returned $menu"
echo "ok  menu ADMIN001 -> admin menu (COADM02Y)"

user=$(curl -fsS -X POST "$AUTH/api/v1/auth/signon" -H 'Content-Type: application/json' \
  -d '{"userId":"USER0001","password":"PASSWORD"}') || fail "signon USER0001"
USER_BEARER=(-H "Authorization: Bearer $(echo "$user" | token_of)")
curl -fsS "${USER_BEARER[@]}" "$AUTH/api/v1/menu" | grep -q '"menu":"MAIN"' || fail "menu USER0001"
code=$(code_of "${USER_BEARER[@]}" "$AUTH/api/v1/users"); [[ "$code" == "403" ]] \
  || fail "USER0001 GET /api/v1/users -> $code, want 403"
echo "ok  USER0001 -> main menu (COMEN02Y), 403 on user admin"

if [[ "$RUN_BATCH" == "1" ]]; then
  # The jobs call account-service/card-service through the gateways, which forward this token.
  for job in post-transactions interest-calculation; do
    out=$(curl -fsS -X POST "${BEARER[@]}" "http://${HOST}:${PORTS[transaction-service]}/api/v1/batch/$job") \
      || fail "batch $job"
    echo "$out" | grep -q '"status":"COMPLETED"' || fail "batch $job returned $out"
    echo "ok  batch $job -> $out"
  done
fi

echo "smoke test passed"

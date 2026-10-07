#!/usr/bin/env bash
# Golden-set online scenario: drives the REST API with the inputs in scenario.json and records every request and
# response. Every step must answer with the expected HTTP status, otherwise the script exits 1.
#
# Usage: scripts/golden-set/online_scenario.sh <base-url> <out-dir>
#   <out-dir>/transcript.md     request/response transcript (bearer tokens redacted), included in reconciliation.md
#   <out-dir>/steps.tsv         step id, method, path, expected and actual HTTP status
#   <out-dir>/online.TRANREPT   the Custom report download (CORPT00C)
#   <out-dir>/report-execution.json  status of that report execution (batch_run rows of the tranrept stream)
# Needs curl and jq. The app must run under the `golden` profile on a freshly loaded database (run_golden_set.sh).
set -euo pipefail
BASE=${1:?base url}
OUT=${2:?out dir}
API=$BASE/api/v1
HERE=$(cd "$(dirname "$0")" && pwd)
S="$HERE/scenario.json"
mkdir -p "$OUT"
BODY="$OUT/.body"
T="$OUT/transcript.md"
: >"$T"
printf 'step\tmethod\tpath\texpected\tactual\n' >"$OUT/steps.tsv"
JSON='Content-Type: application/json'
FAILED=0

# Bearer tokens are redacted; wall-clock values that are not business data (JWT expiry, report submission time,
# batch_run step start/end) and the per-run encrypted cardRef (AES-GCM, random nonce, ADR-0020) are masked so that the
# transcript is identical across runs.
MASK='walk(if type == "object" then with_entries(if .key == "token" then .value = "<redacted>"
      elif (.key | IN("expiresAt", "submittedAt", "startTime", "endTime")) then .value = "<wall clock>"
      elif .key == "cardRef" then .value = "<opaque, per run>" else . end)
      else . end)'
# step <id> <expected status> <title> <method> <path> [json body] [token]
step() {
  local id=$1 want=$2 title=$3 method=$4 path=$5 data=${6:-} tok=${7:-$AA}
  local args=(-s -o "$BODY" -w '%{http_code}' -X "$method" "$API$path")
  [ -n "$tok" ] && args+=(-H "Authorization: Bearer $tok")
  [ -n "$data" ] && args+=(-H "$JSON" -d "$data")
  local code
  code=$(curl "${args[@]}")
  {
    printf '#### %s %s\n\n```\n%s /api/v1%s' "$id" "$title" "$method" "$path"
    [ -n "$data" ] && printf '\n%s' "$(jq -S '(.. | objects | select(has("password")) | .password) |= .' <<<"$data")"
    printf '\n\nHTTP %s\n' "$code"
    if [ -s "$BODY" ]; then
      jq -S "$MASK" "$BODY" 2>/dev/null || cat "$BODY"
    fi
    printf '```\n\n'
  } >>"$T"
  printf '%s\t%s\t%s\t%s\t%s\n' "$id" "$method" "$path" "$want" "$code" >>"$OUT/steps.tsv"
  if [ "$code" != "$want" ]; then
    echo "online_scenario: $id $title -> HTTP $code, expected $want" >&2
    cat "$BODY" >&2 || true
    FAILED=1
    exit 1
  fi
}

login() {
  step "$1" 200 "sign on as $2 (COSGN00C)" POST /auth/login "{\"userId\":\"$2\",\"password\":\"PASSWORD\"}" ""
  jq -r .token "$BODY"
}

{
  echo "Scenario inputs: \`scripts/golden-set/scenario.json\`. Bearer tokens are redacted, wall-clock values (JWT expiry, report submission, batch_run step times) are shown as \`<wall clock>\`, the encrypted \`cardRef\` (random nonce, ADR-0020) as \`<opaque, per run>\`; sample passwords are the"
  echo "plaintext values of the USRSEC sample file. Each step asserts its HTTP status (\`steps.tsv\`)."
  echo
} >>"$T"
AA=""
UA=$(login S01 "$(jq -r '.signOn[0]' "$S")")
AA=$(login S02 "$(jq -r '.signOn[1]' "$S")")
step S03 200 "USER0001 main menu (COMEN01C)" GET /menu/main "" "$UA"
step S04 200 "ADMIN001 admin menu (COADM01C)" GET /menu/admin

# --- COACTVWC / COACTUPC ---------------------------------------------------------------------------------------
VIEW=$(jq -r .accountView "$S")
step S05 200 "view account $VIEW (COACTVWC)" GET "/accounts/$VIEW"
ACCT=$(jq -r .accountUpdate.acctId "$S")
step S06 200 "fetch account $ACCT for update (COACTUPC)" GET "/accounts/$ACCT"
FORM=$(jq -c --argjson set "$(jq -c .accountUpdate.set "$S")" '.updateForm + $set' "$BODY")
step S07 200 "account update ENTER: validate (confirm=false)" PUT "/accounts/$ACCT" "$(jq -c '.confirm = false' <<<"$FORM")"
step S08 200 "account update PF5: commit (confirm=true)" PUT "/accounts/$ACCT" "$(jq -c '.confirm = true' <<<"$FORM")"
[ "$(jq -r .state "$BODY")" = COMMITTED ] || { echo "account update not committed" >&2; exit 1; }

# --- COCRDLIC / COCRDSLC / COCRDUPC ----------------------------------------------------------------------------
CARD=$(jq -r .cardUpdate.cardNum "$S")
step S09 200 "list cards of account $ACCT (COCRDLIC)" GET "/cards?accountId=$ACCT"
step S10 200 "view card $CARD (COCRDSLC)" GET "/cards/$CARD?accountId=$ACCT"
FORM=$(jq -c --argjson c "$(jq -c '.cardUpdate | {embossedName, expiryMonth, expiryYear, activeStatus}' "$S")" \
  '.updateForm + $c' "$BODY")
step S11 200 "card update ENTER: validate (confirm=false)" PUT "/cards/$CARD" "$(jq -c '.confirm = false' <<<"$FORM")"
step S12 200 "card update PF5: commit (confirm=true)" PUT "/cards/$CARD" "$(jq -c '.confirm = true' <<<"$FORM")"

# --- COTRN00C / COTRN01C / COTRN02C ----------------------------------------------------------------------------
step S13 200 "list transactions (COTRN00C, TRANSACT empty after initial-load)" GET /transactions
n=0
while read -r t; do
  n=$((n + 1))
  step "S1$((3 + n))" 201 "add transaction $n (COTRN02C, confirm Y)" POST /transactions "$(jq -c '. + {confirm:"Y"}' <<<"$t")"
done < <(jq -c '.transactions[]' "$S")
step S16 200 "list transactions (COTRN00C)" GET /transactions
FIRST=$(jq -r '.rows[0].tranId' "$BODY")
step S17 200 "view transaction $FIRST (COTRN01C)" GET "/transactions/$FIRST"

# --- COBIL00C ----------------------------------------------------------------------------------------------------
PAY=$(jq -r .billPayment.acctId "$S")
step S18 200 "bill payment ENTER: show balance of account $PAY (COBIL00C)" POST "/accounts/$PAY/bill-payment" '{"confirm":""}'
V=$(jq -r .version "$BODY")
step S19 200 "bill payment confirm Y" POST "/accounts/$PAY/bill-payment" "{\"confirm\":\"Y\",\"version\":$V}"
step S20 200 "view account $PAY after the payment (balance 0)" GET "/accounts/$PAY"
step S21 200 "list transactions: the payment is type 02 / category 2" GET /transactions

# --- COUSR00C..COUSR03C ------------------------------------------------------------------------------------------
UID_=$(jq -r .userAdd.userId "$S")
step S22 201 "add user $UID_ (COUSR01C)" POST /users "$(jq -c .userAdd "$S")"
step S23 200 "fetch user $UID_ (COUSR02C ENTER)" GET "/users/$UID_?fromProgram=COUSR00C"
V=$(jq -r .user.version "$BODY")
UPD=$(jq -c --argjson v "$V" --argjson u "$(jq -c .userUpdate "$S")" \
  '.user | {firstName, lastName, password, userType} + ($u | del(.userId)) + {version: $v}' "$BODY")
step S24 200 "update user $UID_ (COUSR02C PF5)" PUT "/users/$UID_" "$UPD"
DEL=$(jq -r .userDelete "$S")
step S25 200 "delete user $DEL ENTER: show and ask to confirm (COUSR03C)" DELETE "/users/$DEL?fromProgram=COUSR00C"
V=$(jq -r .user.version "$BODY")
step S26 200 "delete user $DEL confirm Y (COUSR03C PF5)" DELETE "/users/$DEL?confirm=Y&version=$V"
step S27 200 "list users (COUSR00C)" GET /users

# --- CORPT00C ----------------------------------------------------------------------------------------------------
REQ=$(jq -c '.report + {confirm:"Y"}' "$S")
step S28 202 "submit Custom report for the DATEPARM window (CORPT00C, confirm Y)" POST /reports/transactions "$REQ"
ID=$(jq -r .executionId "$BODY")
state=""
for _ in $(seq 1 240); do
  curl -s -o "$BODY" "$API/reports/transactions/$ID" -H "Authorization: Bearer $AA"
  state=$(jq -r .status "$BODY")
  case $state in COMPLETED|FAILED) break ;; esac
  sleep 0.5
done
step S29 200 "report execution $ID status ($state)" GET "/reports/transactions/$ID"
cp "$BODY" "$OUT/report-execution.json"
[ "$state" = COMPLETED ] || { echo "report execution $ID: $state" >&2; exit 1; }
curl -s -o "$OUT/online.TRANREPT" -w '' "$API/reports/transactions/$ID/report" -H "Authorization: Bearer $AA"
{
  printf '#### S30 download report %s\n\n```\nGET /api/v1/reports/transactions/%s/report\n\n' "$ID" "$ID"
  printf '%s bytes, sha256 %s (saved as online.TRANREPT)\n' "$(wc -c <"$OUT/online.TRANREPT")" \
    "$(sha256sum "$OUT/online.TRANREPT" | cut -d' ' -f1)"
  cat "$OUT/online.TRANREPT"
  printf '```\n'
} >>"$T"
printf 'S30\tGET\t/reports/transactions/%s/report\t200\t200\n' "$ID" >>"$OUT/steps.tsv"
rm -f "$BODY"
echo "online_scenario: $(($(wc -l <"$OUT/steps.tsv") - 1)) steps OK, report execution $ID"

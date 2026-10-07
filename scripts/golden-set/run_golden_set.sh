#!/usr/bin/env bash
# End-to-end golden-set equivalence run (ticket s6.1): online changes first, then the whole nightly batch cycle, Java
# vs GnuCOBOL, from the same starting data. One command, idempotent; exit 0 only when every difference is explained
# by an allow-list entry under scripts/golden-set/expected-diffs/ and every entry is used exactly once.
#
#   scripts/golden-set/run_golden_set.sh            (or: make golden-set)
#
# Java side (table mode): fresh postgres:16-alpine container -> --job=initial-load from app/data/EBCDIC (row counts
#   checked) -> carddemo-app web under the `golden` profile -> online_scenario.sh (REST) -> app stopped ->
#   --job=unload of the online-changed datasets -> --job=nightly-cycle --run-date=2022-07-06 through
#   scripts/batch/run_nightly_cycle.sh table -> --job=unload of the final datasets.
# COBOL side (file mode): apply_online_scenario.py applies the same scenario to the pristine sample files from the
#   COBOL rules (independently of the Java code) -> cobol_cycle.py runs the GnuCOBOL baseline machinery on them.
# Compare: after-online datasets field by field (compare_datasets.py), online report download vs the COBOL TRANREPT
#   for the same window (byte for byte), every job of the cycle (scripts/batch/compare_*.py with
#   CARDDEMO_BASELINE_DIR = the golden COBOL run), final datasets field by field.
# Writes build/golden-set/ (work) and docs/validation/golden-set/<date>/reconciliation.md (+ the reports it links).
#
# Needs: docker, cobc (GnuCOBOL 3.1.2), Java 21, Maven (only when the jar is missing), python3, curl, jq.
# Environment (all optional): GOLDEN_OUT (build/golden-set), GOLDEN_DATE (today, UTC), GOLDEN_DOC_DIR
# (docs/validation/golden-set/$GOLDEN_DATE), GOLDEN_PG_PORT (55433), GOLDEN_APP_PORT (18095), GOLDEN_BUILD=1 to
# rebuild the jar, GOLDEN_JAVA_HOME (/usr/lib/jvm/java-21-openjdk-amd64 when present, else JAVA_HOME).
set -uo pipefail
cd "$(dirname "$0")/../.."
ROOT=$(pwd)
# Java 21 regardless of the caller's JAVA_HOME (plain JDKs on the image are 17/25); GOLDEN_JAVA_HOME overrides.
JDK21=/usr/lib/jvm/java-21-openjdk-amd64
if [ -n "${GOLDEN_JAVA_HOME:-}" ]; then export JAVA_HOME="$GOLDEN_JAVA_HOME"
elif [ -x "$JDK21/bin/java" ]; then export JAVA_HOME="$JDK21"; fi
JAVA="${JAVA_HOME:+$JAVA_HOME/bin/}java"
OUT="$(realpath -m "${GOLDEN_OUT:-build/golden-set}")"
DATE="${GOLDEN_DATE:-$(date -u +%F)}"
DOC="${GOLDEN_DOC_DIR:-docs/validation/golden-set/$DATE}"
PG_PORT="${GOLDEN_PG_PORT:-55433}"
APP_PORT="${GOLDEN_APP_PORT:-18095}"
PG_NAME="carddemo-golden-pg-$PG_PORT"
JAR="${CARDDEMO_JAR:-modernization/carddemo-app/target/carddemo-app.jar}"
HERE=scripts/golden-set
APP_PID=""
STARTED=$(date +%s)

for tool in docker cobc python3 curl jq; do
    command -v "$tool" >/dev/null || { echo "run_golden_set: $tool not found" >&2; exit 2; }
done

cleanup() {
    [ -n "$APP_PID" ] && kill "$APP_PID" 2>/dev/null && wait "$APP_PID" 2>/dev/null
    docker rm -f "$PG_NAME" >/dev/null 2>&1
}
trap cleanup EXIT
fail() { echo "run_golden_set: $*" >&2; exit 1; }
phase() { printf '\n== %s (%ss)\n' "$1" "$(($(date +%s) - STARTED))"; }

rm -rf "$OUT"
mkdir -p "$OUT/java/after-online" "$OUT/java/final" "$OUT/cobol" "$OUT/reports"
# Per-run credentials for the throw-away database and token signing; never written to disk.
export CARDDEMO_DB_USER=carddemo
export CARDDEMO_DB_PASSWORD="golden-$(head -c 12 /dev/urandom | od -An -tx1 | tr -d ' \n')"
export CARDDEMO_JWT_SECRET="$(head -c 32 /dev/urandom | od -An -tx1 | tr -d ' \n')"
export CARDDEMO_DB_URL="jdbc:postgresql://127.0.0.1:$PG_PORT/carddemo"
export CARDDEMO_BATCH_OUTPUT_DIR="$OUT/java/batch-output"

phase "build"
if [ ! -f "$JAR" ] || [ "${GOLDEN_BUILD:-0}" = 1 ]; then
    (cd modernization && mvn -B -q -DskipTests -pl carddemo-app -am package) || fail "maven package failed"
fi
"$JAVA" -version 2>&1 | head -1 | tee "$OUT/java/java-version.txt"
"$JAVA" -version 2>&1 | head -1 | grep -q '"21' || echo "::warning::not running on Java 21 (set GOLDEN_JAVA_HOME)"
cobc --version | head -1 | tee "$OUT/cobol/cobc-version.txt"

batch() {  # batch <log> <args...>: one batch CLI launch under the golden profile
    local log="$1"; shift
    "$JAVA" -jar "$JAR" --spring.profiles.active=golden --carddemo.initial-load.on-startup=false \
        --spring.main.banner-mode=off "$@" >"$log" 2>&1
}
psql_q() { docker exec "$PG_NAME" psql -U carddemo -d carddemo -tA -F '|' -c "$1"; }

phase "Java: fresh PostgreSQL + initial-load"
docker rm -f "$PG_NAME" >/dev/null 2>&1
docker run -d --name "$PG_NAME" -e POSTGRES_DB=carddemo -e POSTGRES_USER=carddemo \
    -e POSTGRES_PASSWORD="$CARDDEMO_DB_PASSWORD" -p "127.0.0.1:$PG_PORT:5432" postgres:16-alpine >/dev/null \
    || fail "cannot start postgres:16-alpine"
for _ in $(seq 1 60); do
    docker exec "$PG_NAME" pg_isready -U carddemo -d carddemo -h 127.0.0.1 >/dev/null 2>&1 && break
    sleep 1
done
batch "$OUT/java/initial-load.log" --job=initial-load --mode=REPLACE \
    || { tail -40 "$OUT/java/initial-load.log"; fail "initial-load failed"; }
TABLES="user_security transaction_type transaction_category disclosure_group customer account card card_xref tran_cat_balance transaction daily_transaction"
counts=""
for t in $TABLES; do counts+="$(psql_q "select count(*) from $t")/"; done
counts="${counts%/}"
echo "table rows ($TABLES): $counts"
echo "$counts" >"$OUT/java/initial-load.counts"
[ "$counts" = "10/7/18/51/50/50/50/50/50/0/300" ] || fail "initial-load counts $counts, expected 10/7/18/51/50/50/50/50/50/0/300"

phase "Java: online scenario (carddemo-app, golden profile)"
"$JAVA" -jar "$JAR" --spring.profiles.active=golden --carddemo.initial-load.on-startup=false \
    --server.port="$APP_PORT" --carddemo.reports.encoding=ASCII --spring.main.banner-mode=off \
    >"$OUT/java/app.log" 2>&1 &
APP_PID=$!
for _ in $(seq 1 120); do
    curl -sf "http://127.0.0.1:$APP_PORT/actuator/health" >/dev/null 2>&1 && break
    kill -0 "$APP_PID" 2>/dev/null || { tail -40 "$OUT/java/app.log"; fail "carddemo-app did not start"; }
    sleep 1
done
"$HERE/online_scenario.sh" "http://127.0.0.1:$APP_PORT" "$OUT/java/online" || fail "online scenario failed"
kill "$APP_PID" && wait "$APP_PID" 2>/dev/null
APP_PID=""

unload() {  # unload <dir> <datasets...>
    local dir="$1"; shift
    for ds in "$@"; do
        batch "$dir/unload-$ds.log" --job=unload --DATASET="$ds" --OUTFILE="$dir/$ds.txt" --encoding=ASCII \
            || { tail -20 "$dir/unload-$ds.log"; fail "unload $ds failed"; }
        rm -f "$dir/unload-$ds.log"
    done
    echo "  unloaded $* -> ${dir#"$ROOT/"}"
}
phase "Java: export after-online datasets"
unload "$OUT/java/after-online" ACCTDATA CUSTDATA CARDDATA CARDXREF TRANSACT USRSEC

phase "COBOL: apply the scenario to the sample files (independent of Java)"
python3 "$HERE/apply_online_scenario.py" --out "$OUT/cobol/after-online" || fail "apply_online_scenario failed"

phase "Compare after-online datasets"
python3 "$HERE/compare_datasets.py" --cobol-dir "$OUT/cobol/after-online" --java-dir "$OUT/java/after-online" \
    --datasets ACCTDATA,CUSTDATA,CARDDATA,CARDXREF,TRANSACT,USRSEC --expected-diffs "$HERE/expected-diffs/online.txt" \
    --title "Online changes: independent expected files vs Java export" \
    --report "$OUT/reports/online.md" --json "$OUT/reports/online.json"
echo $? >"$OUT/reports/online.rc"

phase "COBOL: GnuCOBOL cycle on the after-online files"
python3 "$HERE/cobol_cycle.py" --after-online "$OUT/cobol/after-online" --out "$OUT/cobol/cycle" \
    --work "$OUT/cobol/work" >"$OUT/cobol/cycle.log" 2>&1 || { tail -30 "$OUT/cobol/cycle.log"; fail "cobol cycle failed"; }
grep -E '^\[' "$OUT/cobol/cycle.log" | sed 's/^/  /'

phase "Compare online report download with the COBOL TRANREPT for the same window"
{
    printf 'online %s %s bytes\n' "$(sha256sum <"$OUT/java/online/online.TRANREPT" | cut -d' ' -f1)" \
        "$(wc -c <"$OUT/java/online/online.TRANREPT")"
    printf 'cobol  %s %s bytes\n' "$(sha256sum <"$OUT/cobol/cycle/ONLINE-TRANREPT/TRANREPT.raw" | cut -d' ' -f1)" \
        "$(wc -c <"$OUT/cobol/cycle/ONLINE-TRANREPT/TRANREPT.raw")"
} | tee "$OUT/reports/report-download.txt"
if cmp -s "$OUT/java/online/online.TRANREPT" "$OUT/cobol/cycle/ONLINE-TRANREPT/TRANREPT.raw"; then
    echo "IDENTICAL" | tee -a "$OUT/reports/report-download.txt"; echo 0 >"$OUT/reports/report-download.rc"
else
    echo "DIFFERENT" | tee -a "$OUT/reports/report-download.txt"; echo 1 >"$OUT/reports/report-download.rc"
    # TRANREPT records are 133-byte FBA lines without newlines: fold them so the diff shows the differing records
    diff <(fold -w 133 "$OUT/cobol/cycle/ONLINE-TRANREPT/TRANREPT.raw") <(fold -w 133 "$OUT/java/online/online.TRANREPT") \
        | head -20 | tee -a "$OUT/reports/report-download.txt"
fi

phase "Java: nightly-cycle on the online-changed database + per-job compare"
CARDDEMO_BASELINE_DIR="$OUT/cobol/cycle" NIGHTLY_CYCLE_SKIP_LOAD=1 \
    NIGHTLY_CYCLE_EXPECTED_DIFFS="$ROOT/$HERE/expected-diffs/cycle" CARDDEMO_JAR="$JAR" \
    scripts/batch/run_nightly_cycle.sh table "$OUT/java/cycle" 2>&1 | grep -v -- '--- ' | tee "$OUT/java/cycle.log"
echo "${PIPESTATUS[0]}" >"$OUT/reports/cycle.rc"

phase "Java: export final datasets + compare"
unload "$OUT/java/final" ACCTDATA CUSTDATA CARDDATA CARDXREF TRANSACT TCATBALF USRSEC
python3 "$HERE/compare_datasets.py" --cobol-dir "$OUT/cobol/cycle/FINAL" --java-dir "$OUT/java/final" \
    --datasets ACCTDATA,CUSTDATA,CARDDATA,CARDXREF,TRANSACT,TCATBALF,USRSEC \
    --expected-diffs "$HERE/expected-diffs/final.txt" \
    --title "Final datasets after the nightly cycle: GnuCOBOL vs Java" \
    --report "$OUT/reports/final.md" --json "$OUT/reports/final.json"
echo $? >"$OUT/reports/final.rc"

phase "Reconciliation"
python3 "$HERE/golden_report.py" --out "$OUT" --doc "$DOC" --elapsed "$(($(date +%s) - STARTED))"

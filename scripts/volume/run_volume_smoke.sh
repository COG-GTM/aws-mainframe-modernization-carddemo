#!/usr/bin/env bash
# s6.4 volume smoke (not in CI): 100,000 generated DALYTRAN records through POSTTRAN and INTCALC in table mode.
#
#   scripts/volume/run_volume_smoke.sh [out-dir]      (make volume-smoke; default build/volume-smoke)
#
# Fresh postgres:16-alpine with pg_stat_statements -> --job=initial-load -> --job=repro DALYTRAN (REPLACE) from
# scripts/volume/gen_dalytran.py -> --job=posttran -> --job=intcalc. Every launch runs under /usr/bin/time -v with
# -Xmx512m (VOLUME_XMX). Writes <out>/metrics.md: wall clock, exit code, peak RSS, batch_run counts, table counts
# and the top 3 statements by total execution time (pg_stat_statements, reset before each job). Fails when an exit
# code or a count differs from <dalytran>.expected.json.
#
# Needs docker, Java 21 (JAVA_HOME), python3 and the packaged jar (CARDDEMO_JAR, default
# modernization/carddemo-app/target/carddemo-app.jar; VOLUME_BUILD=1 builds it). Knobs: VOLUME_RECORDS (100000),
# VOLUME_PG_PORT (55434), VOLUME_XMX (512m).
set -uo pipefail
cd "$(dirname "$0")/../.."
OUT="$(realpath -m "${1:-build/volume-smoke}")"
JAR="${CARDDEMO_JAR:-modernization/carddemo-app/target/carddemo-app.jar}"
JAVA="${JAVA_HOME:+$JAVA_HOME/bin/}java"
RECORDS="${VOLUME_RECORDS:-100000}"
PG_PORT="${VOLUME_PG_PORT:-55434}"
XMX="${VOLUME_XMX:-512m}"
PG_NAME="carddemo-volume-pg-$PG_PORT"
fail() { echo "::error::volume-smoke: $*" >&2; exit 1; }

if [ "${VOLUME_BUILD:-0}" = 1 ] || [ ! -f "$JAR" ]; then
    (cd modernization && mvn -B -q -DskipTests package) || fail "jar build failed"
fi
command -v docker >/dev/null || fail "docker is required"
[ -x /usr/bin/time ] || fail "/usr/bin/time (GNU time) is required"

rm -rf "$OUT" && mkdir -p "$OUT"
export CARDDEMO_BATCH_OUTPUT_DIR="$OUT/output"
export CARDDEMO_DB_URL="jdbc:postgresql://127.0.0.1:$PG_PORT/carddemo" CARDDEMO_DB_USER=carddemo
export CARDDEMO_DB_PASSWORD="${CARDDEMO_DB_PASSWORD:-volume-$(date +%s)}"

echo "== generate $RECORDS DALYTRAN records"
python3 scripts/volume/gen_dalytran.py --out "$OUT/dalytran.txt" --records "$RECORDS" || fail "generator failed"
expected="$OUT/dalytran.txt.expected.json"
exp() { python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))[sys.argv[2]])' "$expected" "$1"; }

echo "== fresh PostgreSQL 16 with pg_stat_statements on 127.0.0.1:$PG_PORT"
docker rm -f "$PG_NAME" >/dev/null 2>&1
docker run -d --name "$PG_NAME" -e POSTGRES_DB=carddemo -e POSTGRES_USER=carddemo \
    -e POSTGRES_PASSWORD="$CARDDEMO_DB_PASSWORD" -p "127.0.0.1:$PG_PORT:5432" postgres:16-alpine \
    -c shared_preload_libraries=pg_stat_statements -c pg_stat_statements.track=top >/dev/null \
    || fail "cannot start postgres:16-alpine"
trap 'docker rm -f "$PG_NAME" >/dev/null 2>&1' EXIT
psql_q() { docker exec "$PG_NAME" psql -U carddemo -d carddemo -h 127.0.0.1 -tA -F '|' -c "$1"; }
for _ in $(seq 1 60); do psql_q "select 1" >/dev/null 2>&1 && break; sleep 1; done
sleep 2
psql_q "create extension if not exists pg_stat_statements" >/dev/null || fail "pg_stat_statements unavailable"

{
    echo "# Volume smoke run ($(date -u +%Y-%m-%dT%H:%M:%SZ))"
    echo
    echo "- Records: $RECORDS (seed $(exp seed)); expected posted $(exp posted), rejected $(exp rejected) $(exp rejected_by_reason)"
    echo "- JVM: \`$("$JAVA" -version 2>&1 | head -1)\`, \`-Xmx$XMX\`; host $(nproc) CPUs, $(free -g | awk '/Mem:/{print $2}') GiB"
    echo "- Jar: \`$JAR\` ($(cd modernization && git rev-parse --short HEAD))"
    echo
    echo "| Job | Exit code | Wall clock (s) | Peak RSS (MiB) | batch_run read / write |"
    echo "| --- | --- | --- | --- | --- |"
} >"$OUT/metrics.md"
hotspots=""

job() {  # job <name> <args...>: one batch launch under GNU time; appends a metrics row and the SQL top 3
    local name="$1"; shift
    psql_q "select pg_stat_statements_reset()" >/dev/null
    local mark start end rc
    mark=$(psql_q "select coalesce(max(batch_run_id), 0) from batch_run" 2>/dev/null || echo 0)
    mark=${mark:-0}
    start=$(date +%s.%N)
    /usr/bin/time -v -o "$OUT/$name.time" "$JAVA" "-Xmx$XMX" -jar "$JAR" --spring.profiles.active=golden \
        --carddemo.initial-load.on-startup=false --spring.main.banner-mode=off "$@" >"$OUT/$name.log" 2>&1
    rc=$?
    end=$(date +%s.%N)
    local wall rss counts
    wall=$(python3 -c "print(f'{$end - $start:.1f}')")
    rss=$(awk -F': ' '/Maximum resident set size/{printf "%.0f", $2/1024}' "$OUT/$name.time")
    counts=$(psql_q "select string_agg(job_name || ' ' || read_count || ' / ' || write_count, ', '
                            order by batch_run_id) from batch_run where step_name is null and batch_run_id > $mark")
    echo "  $name: exit $rc, ${wall}s, peak RSS ${rss} MiB, batch_run $counts"
    echo "| \`$name\` | $rc | $wall | $rss | ${counts:--} |" >>"$OUT/metrics.md"
    hotspots+=$'\n'"### $name"$'\n\n'"| Calls | Total ms | Mean ms | Rows | Statement |"$'\n'"| --- | --- | --- | --- | --- |"$'\n'
    hotspots+="$(psql_q "select calls, round(total_exec_time::numeric, 1), round(mean_exec_time::numeric, 3), rows,
                          '\`' || left(regexp_replace(query, '\s+', ' ', 'g'), 140) || '\`'
                   from pg_stat_statements where query not ilike '%pg_stat_statements%'
                   order by total_exec_time desc limit 3" | sed 's/|/ | /g; s/^/| /; s/$/ |/')"$'\n'
    return $rc
}

echo "== jobs (-Xmx$XMX)"
job initial-load --job=initial-load --mode=REPLACE || fail "initial-load failed (see $OUT/initial-load.log)"
job repro-dalytran --job=repro --DATASET=DALYTRAN --INFILE="$OUT/dalytran.txt" --encoding=ASCII --mode=REPLACE \
    --run.id="$(date +%s%N)" || fail "repro DALYTRAN failed"
job posttran --job=posttran --run-date=2022-07-06 --encoding=ASCII \
    --STEP10.SYSOUT="$OUT/cbtrn01c-sysout.txt" --STEP15.SYSOUT="$OUT/cbtrn02c-sysout.txt"
posttran_rc=$?
job intcalc --job=intcalc --run-date=2022-07-06 --encoding=ASCII --STEP15.SYSOUT="$OUT/cbact04c-sysout.txt"
intcalc_rc=$?

tran=$(psql_q "select count(*) from transaction")
daily=$(psql_q "select count(*) from daily_transaction")
systran=$(psql_q "select record_count from batch_output_file where gdg_base = 'SYSTRAN' order by output_file_id desc limit 1")
tcat=$(psql_q "select count(*) from tran_cat_balance")
rejects=$(psql_q "select record_count from batch_output_file where gdg_base = 'DALYREJS' order by output_file_id desc limit 1")
{
    echo
    echo "| Count | Value |"
    echo "| --- | --- |"
    echo "| \`daily_transaction\` rows (input) | $daily |"
    echo "| \`transaction\` rows after POSTTRAN (posted) | $tran |"
    echo "| SYSTRAN records (INTCALC interest transactions, a GDG file until COMBTRAN) | ${systran:-?} |"
    echo "| DALYREJS records (POSTTRAN rejects) | ${rejects:-?} |"
    echo "| \`tran_cat_balance\` rows | $tcat |"
    echo
    echo "## Top 3 SQL statements per job (pg_stat_statements, by total execution time)"
    echo "$hotspots"
} >>"$OUT/metrics.md"
cat "$OUT/metrics.md"

ok=1
[ "$posttran_rc" = "$(exp expected_rc)" ] || { echo "::error::posttran exit $posttran_rc"; ok=0; }
[ "$intcalc_rc" = 0 ] || { echo "::error::intcalc exit $intcalc_rc"; ok=0; }
[ "$daily" = "$RECORDS" ] || { echo "::error::daily_transaction $daily"; ok=0; }
[ "${rejects:-}" = "$(exp rejected)" ] || { echo "::error::DALYREJS ${rejects:-none}, expected $(exp rejected)"; ok=0; }
[ "$tran" = "$(exp posted)" ] || { echo "::error::transaction rows $tran, expected $(exp posted)"; ok=0; }
[ "$ok" = 1 ] && echo "VOLUME SMOKE OK ($OUT/metrics.md)" && exit 0
exit 1

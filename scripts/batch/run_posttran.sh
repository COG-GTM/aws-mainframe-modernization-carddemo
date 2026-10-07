#!/usr/bin/env bash
# Runs POSTTRAN (STEP10 CBTRN01C, STEP15 CBTRN02C COND=(4,LT,STEP10)) through the batch CLI of the packaged app
# (java -jar carddemo-app.jar --job=posttran ...) and compares SYSOUT, RC, DALYREJS and the TRANSACT/ACCTDATA/
# TCATBALF after-images with docs/validation/baseline/{CBTRN01C,POSTTRAN} (scripts/batch/compare_posttran.py).
# DALYREJS is always the dated generation DALYREJS(+1) (ADR-0012), checked in batch_output_file.
# Then checks the COND path: an abending STEP10 must bypass STEP15 and end the stream with RC 16.
#
#   scripts/batch/run_posttran.sh file  <out-dir>   KSDS DDs are copies of the app/data/ASCII unloads the baseline
#                                                    used, updated in place; TRANFILE starts empty
#   scripts/batch/run_posttran.sh table <out-dir>   every DD is PostgreSQL after --job=initial-load (EBCDIC samples);
#                                                    the tables are unloaded with --job=unload for the comparison
#
# Needs CARDDEMO_DB_URL / CARDDEMO_DB_USER / CARDDEMO_DB_PASSWORD (PostgreSQL 16; batch_run and batch_output_file are
# written in both modes) and a built jar (CARDDEMO_JAR, default modernization/carddemo-app/target/carddemo-app.jar).
set -uo pipefail
cd "$(dirname "$0")/../.."
mode="${1:?usage: $0 file|table <out-dir>}"
out="$(realpath -m "${2:?usage: $0 file|table <out-dir>}")"
jar="${CARDDEMO_JAR:-modernization/carddemo-app/target/carddemo-app.jar}"
java="${JAVA_HOME:+$JAVA_HOME/bin/}java"
: "${CARDDEMO_DB_URL:?}" "${CARDDEMO_DB_USER:?}" "${CARDDEMO_DB_PASSWORD:?}"
case "$mode" in file|table) ;; *) echo "mode must be file or table" >&2; exit 2 ;; esac
rm -rf "$out" && mkdir -p "$out/CBTRN01C" "$out/POSTTRAN" "$out/ds"
export CARDDEMO_BATCH_OUTPUT_DIR="$out/output"

batch() {  # batch <log> <args...>: run one CLI job, print and return its exit code
    local log="$1"; shift
    "$java" -jar "$jar" --spring.profiles.active=golden --carddemo.initial-load.on-startup=false \
        --spring.main.banner-mode=off "$@" >"$log" 2>&1
    local rc=$?
    echo "  exit code $rc ($(basename "$log"))"
    return $rc
}

"$java" -version 2>&1 | head -1
echo "initial-load (REPLACE from app/data/EBCDIC)"
batch "$out/initial-load.log" --job=initial-load --mode=REPLACE || { tail -50 "$out/initial-load.log"; exit 1; }

ds="$out/ds"
args=(--job=posttran --run-date=2022-07-06 --encoding=ASCII
      --STEP10.SYSOUT="$out/CBTRN01C/sysout.txt" --STEP15.SYSOUT="$out/POSTTRAN/sysout.txt")
if [ "$mode" = file ]; then
    cp app/data/ASCII/acctdata.txt "$ds/ACCTDATA.ksds"
    cp app/data/ASCII/tcatbal.txt "$ds/TCATBALF.ksds"
    : >"$ds/TRANSACT.ksds"
    args+=(--DALYTRAN=app/data/ASCII/dailytran.txt --XREFFILE=app/data/ASCII/cardxref.txt
           --CUSTFILE=app/data/ASCII/custdata.txt --CARDFILE=app/data/ASCII/carddata.txt
           --ACCTFILE="$ds/ACCTDATA.ksds" --TCATBALF="$ds/TCATBALF.ksds" --TRANFILE="$ds/TRANSACT.ksds")
fi
echo "posttran (DDs from $mode)"
batch "$out/POSTTRAN/job.log" "${args[@]}"
echo $? >"$out/POSTTRAN/rc.txt"
grep -E "posttran (STEP|ended)" "$out/POSTTRAN/job.log" | sed 's/^.*INFO [^:]*: /  /'

generation="$(sed -n 's/.*-> DALYREJS \(.*\)$/\1/p' "$out/POSTTRAN/job.log" | tail -1)"
if [ -n "$generation" ] && [ -f "$generation" ]; then
    echo "  DALYREJS generation: ${generation#"$out/"}"
    cp "$generation" "$out/POSTTRAN/DALYREJS"
fi
for name in TRANSACT ACCTDATA TCATBALF; do
    if [ "$mode" = file ]; then
        cp "$ds/$name.ksds" "$out/POSTTRAN/$name.ksds"
    else
        batch "$out/POSTTRAN/unload-$name.log" --job=unload --DATASET="$name" --encoding=ASCII \
            --OUTFILE="$out/POSTTRAN/$name.ksds"
    fi
done

expected=()
[ "$mode" = table ] && expected=(--expected-diffs scripts/batch/posttran-table-mode-expected-diffs.txt)
python3 scripts/batch/compare_posttran.py --java-dir "$out" --mode "$mode" "${expected[@]}" \
    --report "$out/REPORT.md"
compare_rc=$?

checks_ok=1
if command -v psql >/dev/null; then
    db="${CARDDEMO_DB_URL#jdbc:postgresql://}"; hostport="${db%%/*}"; name="${db#*/}"; name="${name%%\?*}"
    psql_q() { PGPASSWORD="$CARDDEMO_DB_PASSWORD" psql -h "${hostport%%:*}" -p "${hostport##*:}" \
                 -U "$CARDDEMO_DB_USER" -d "$name" -tA -F ' ' -c "$1"; }
    echo "batch_run (POSTTRAN steps):"
    psql_q "select job_name, coalesce(step_name, '-'), status, return_code, read_count, write_count
            from batch_run where job_name in ('cbtrn01c', 'cbtrn02c') order by batch_run_id desc limit 4" \
        | tee "$out/batch_run.txt"
    last="$(psql_q "select status || ' ' || return_code || ' ' || read_count || ' ' || write_count from batch_run
                    where step_name is null and job_name = 'cbtrn02c' order by batch_run_id desc limit 1")"
    [ "$last" = "COMPLETED 4 300 262" ] || { echo "::error::batch_run for cbtrn02c: '$last'"; checks_ok=0; }
    echo "batch_output_file (DALYREJS):"
    gdg="$(psql_q "select business_date || ' ' || record_count || ' ' || file_path from batch_output_file
                   where gdg_base = 'DALYREJS' order by output_file_id desc limit 1")"
    echo "  $gdg" | tee "$out/batch_output_file.txt"
    [ "$gdg" = "2022-07-06 38 $generation" ] || { echo "::error::batch_output_file row: '$gdg'"; checks_ok=0; }
fi

echo "COND path: STEP10 abends (missing DALYTRAN) -> STEP15 bypassed, stream RC 16, no DALYREJS generation"
neg="$out/negative"; mkdir -p "$neg"
before="$(find "$out/output" -type f 2>/dev/null | wc -l)"
batch "$neg/job.log" --job=posttran --encoding=ASCII --DALYTRAN="$neg/does-not-exist" \
    --STEP10.SYSOUT="$neg/cbtrn01c.txt" --STEP15.SYSOUT="$neg/cbtrn02c.txt"
neg_rc=$?
[ "$neg_rc" = 16 ] || { echo "::error::expected exit code 16, got $neg_rc"; checks_ok=0; }
grep -q "STEP15 cbtrn02c: bypassed" "$neg/job.log" || { echo "::error::STEP15 was not bypassed"; checks_ok=0; }
[ ! -e "$neg/cbtrn02c.txt" ] || { echo "::error::STEP15 ran (SYSOUT written)"; checks_ok=0; }
[ "$(find "$out/output" -type f 2>/dev/null | wc -l)" = "$before" ] \
    || { echo "::error::a DALYREJS generation was catalogued for the bypassed step"; checks_ok=0; }
grep -E "posttran (STEP|ended)" "$neg/job.log" | sed 's/^.*INFO [^:]*: /  /'

[ "$compare_rc" = 0 ] && [ "$checks_ok" = 1 ] && echo "POSTTRAN OK ($mode)" && exit 0
exit 1

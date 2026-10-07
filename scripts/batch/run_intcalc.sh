#!/usr/bin/env bash
# Runs INTCALC (STEP15 CBACT04C, PARM='2022071800' from the golden profile) through the batch CLI of the packaged app
# (java -jar carddemo-app.jar --job=intcalc ...) and compares SYSOUT, RC, SYSTRAN and the ACCTDATA/TCATBALF
# after-images with docs/validation/baseline/INTCALC (scripts/batch/compare_intcalc.py). SYSTRAN is always the dated
# generation SYSTRAN(+1) (ADR-0012), checked in batch_output_file.
#
# Start state = the one the baseline INTCALC read (scripts/baseline/baseline.py runs POSTTRAN before INTCALC, so
# ACCTDATA and TCATBALF are the POSTTRAN after-images in docs/validation/baseline/POSTTRAN). No Java POSTTRAN runs:
#
#   scripts/batch/run_intcalc.sh file  <out-dir>   ACCTFILE/TCATBALF are copies of the POSTTRAN after-images (ACCTFILE
#                                                   updated in place), XREFFILE/DISCGRP the app/data/ASCII unloads
#   scripts/batch/run_intcalc.sh table <out-dir>   every DD is PostgreSQL: --job=initial-load (EBCDIC samples), then
#                                                   --job=repro of the two after-images; unloaded with --job=unload
#
# Then checks the abend path: a missing ACCTFILE ends the job with RC 16 and catalogues no SYSTRAN generation.
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
rm -rf "$out" && mkdir -p "$out/INTCALC" "$out/ds"
export CARDDEMO_BATCH_OUTPUT_DIR="$out/output"
start=docs/validation/baseline/POSTTRAN

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
args=(--job=intcalc --run-date=2022-07-06 --encoding=ASCII --STEP15.SYSOUT="$out/INTCALC/sysout.txt")
if [ "$mode" = file ]; then
    cp "$start/ACCTDATA.ksds.txt" "$ds/ACCTDATA.ksds"
    cp "$start/TCATBALF.ksds.txt" "$ds/TCATBALF.ksds"
    args+=(--TCATBALF="$ds/TCATBALF.ksds" --XREFFILE=app/data/ASCII/cardxref.txt --ACCTFILE="$ds/ACCTDATA.ksds"
           --DISCGRP=app/data/ASCII/discgrp.txt)
else
    for name in ACCTDATA TCATBALF; do
        echo "repro $name (UPSERT from $start/$name.ksds.txt)"
        batch "$out/repro-$name.log" --job=repro --DATASET="$name" --INFILE="$start/$name.ksds.txt" \
            --encoding=ASCII --run.id="$(date +%s%N)" || { tail -50 "$out/repro-$name.log"; exit 1; }
    done
fi
echo "intcalc (DDs from $mode)"
batch "$out/INTCALC/job.log" "${args[@]}"
echo $? >"$out/INTCALC/rc.txt"
grep -E "intcalc (STEP|ended)|cbact04c: PARM" "$out/INTCALC/job.log" | sed 's/^.*INFO [^:]*: /  /'

generation="$(sed -n 's/.*-> SYSTRAN \(.*\)$/\1/p' "$out/INTCALC/job.log" | tail -1)"
if [ -n "$generation" ] && [ -f "$generation" ]; then
    echo "  SYSTRAN generation: ${generation#"$out/"}"
    cp "$generation" "$out/INTCALC/SYSTRAN"
fi
for name in ACCTDATA TCATBALF; do
    if [ "$mode" = file ]; then
        cp "$ds/$name.ksds" "$out/INTCALC/$name.ksds"
    else
        batch "$out/INTCALC/unload-$name.log" --job=unload --DATASET="$name" --encoding=ASCII \
            --OUTFILE="$out/INTCALC/$name.ksds"
    fi
done

expected=()
[ "$mode" = table ] && expected=(--expected-diffs scripts/batch/intcalc-table-mode-expected-diffs.txt)
python3 scripts/batch/compare_intcalc.py --java-dir "$out" --mode "$mode" "${expected[@]}" \
    --report "$out/REPORT.md"
compare_rc=$?

checks_ok=1
if command -v psql >/dev/null; then
    db="${CARDDEMO_DB_URL#jdbc:postgresql://}"; hostport="${db%%/*}"; name="${db#*/}"; name="${name%%\?*}"
    psql_q() { PGPASSWORD="$CARDDEMO_DB_PASSWORD" psql -h "${hostport%%:*}" -p "${hostport##*:}" \
                 -U "$CARDDEMO_DB_USER" -d "$name" -tA -F ' ' -c "$1"; }
    echo "batch_run (cbact04c):"
    psql_q "select job_name, coalesce(step_name, '-'), status, return_code, read_count, write_count
            from batch_run where job_name = 'cbact04c' order by batch_run_id desc limit 2" \
        | tee "$out/batch_run.txt"
    last="$(psql_q "select status || ' ' || return_code || ' ' || read_count || ' ' || write_count from batch_run
                    where step_name is null and job_name = 'cbact04c' order by batch_run_id desc limit 1")"
    [ "$last" = "COMPLETED 0 100 50" ] || { echo "::error::batch_run for cbact04c: '$last'"; checks_ok=0; }
    echo "batch_output_file (SYSTRAN):"
    gdg="$(psql_q "select business_date || ' ' || record_count || ' ' || file_path from batch_output_file
                   where gdg_base = 'SYSTRAN' order by output_file_id desc limit 1")"
    echo "  $gdg" | tee "$out/batch_output_file.txt"
    [ "$gdg" = "2022-07-06 50 $generation" ] || { echo "::error::batch_output_file row: '$gdg'"; checks_ok=0; }
fi

echo "abend path: ACCTFILE missing -> RC 16, no SYSTRAN generation"
neg="$out/negative"; mkdir -p "$neg"
before="$(find "$out/output" -type f 2>/dev/null | wc -l)"
batch "$neg/job.log" --job=intcalc --run-date=2022-07-06 --encoding=ASCII --ACCTFILE="$neg/does-not-exist" \
    --STEP15.SYSOUT="$neg/cbact04c.txt"
neg_rc=$?
[ "$neg_rc" = 16 ] || { echo "::error::expected exit code 16, got $neg_rc"; checks_ok=0; }
grep -q "ERROR OPENING ACCOUNT MASTER FILE" "$neg/cbact04c.txt" 2>/dev/null \
    || { echo "::error::SYSOUT lacks the ACCTFILE open error"; checks_ok=0; }
[ "$(find "$out/output" -type f 2>/dev/null | wc -l)" = "$before" ] \
    || { echo "::error::a SYSTRAN generation was catalogued for the abended step"; checks_ok=0; }

[ "$compare_rc" = 0 ] && [ "$checks_ok" = 1 ] && echo "INTCALC OK ($mode)" && exit 0
exit 1

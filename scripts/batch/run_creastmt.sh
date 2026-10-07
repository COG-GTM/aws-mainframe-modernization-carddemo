#!/usr/bin/env bash
# Runs CREASTMT (STEP010 creastmt-sort, STEP020 trxfl-repro, STEP040 cbstm03a) through the batch CLI of the packaged
# app (java -jar carddemo-app.jar --job=creastmt ...) and compares it with docs/validation/baseline/CREASTMT
# (scripts/batch/compare_creastmt.py). TRXFL.SEQ, TRXFL, STATEMNT.PS and STATEMNT.HTML are dated generations
# (ADR-0012); TRANSACT / CARDXREF / CUSTDATA / ACCTDATA are files or tables.
#
# CREASTMT starts from the baseline after-images of the jobs before it (docs/validation/baseline/00-ORDER.md:
# ... INTCALC -> TRANBKP -> COMBTRAN -> TRANREPT -> CREASTMT), never from another Java job's output:
#   TRANSACT = COMBTRAN/TRANSACT.ksds.txt (TRANREPT reads it only), ACCTDATA = INTCALC/ACCTDATA.ksds.txt,
#   CARDXREF = XREFFILE/CARDXREF.ksds.txt, CUSTDATA = CUSTFILE/CUSTDATA.ksds.txt
#
#   scripts/batch/run_creastmt.sh file  <out-dir>   DDs are copies of the after-images
#   scripts/batch/run_creastmt.sh table <out-dir>   DDs are PostgreSQL: --job=initial-load (CARDXREF, CUSTDATA), then
#                                                    --job=idcams-delete + --job=repro of TRANSACT and --job=repro of
#                                                    ACCTDATA from the after-images
#
# Then checks a failure path: an empty TRNXFILE makes CBSTM03A abend (RC 16) and catalogues no statement generation.
# Needs CARDDEMO_DB_URL / CARDDEMO_DB_USER / CARDDEMO_DB_PASSWORD (PostgreSQL 16) and a built jar (CARDDEMO_JAR,
# default modernization/carddemo-app/target/carddemo-app.jar).
set -uo pipefail
cd "$(dirname "$0")/../.."
mode="${1:?usage: $0 file|table <out-dir>}"
out="$(realpath -m "${2:?usage: $0 file|table <out-dir>}")"
jar="${CARDDEMO_JAR:-modernization/carddemo-app/target/carddemo-app.jar}"
java="${JAVA_HOME:+$JAVA_HOME/bin/}java"
: "${CARDDEMO_DB_URL:?}" "${CARDDEMO_DB_USER:?}" "${CARDDEMO_DB_PASSWORD:?}"
case "$mode" in file|table) ;; *) echo "mode must be file or table" >&2; exit 2 ;; esac
rm -rf "$out" && mkdir -p "$out/ds" "$out/CREASTMT"
export CARDDEMO_BATCH_OUTPUT_DIR="$out/output"
base=docs/validation/baseline
ds="$out/ds"
run="$out/CREASTMT"
common=(--run-date=2022-07-06 --encoding=ASCII)
steps="STEP010 STEP020 STEP040"
gdgs="TRXFL.SEQ TRXFL STATEMNT.PS STATEMNT.HTML"

batch() {  # batch <log> <args...>: run one CLI job, print and return its exit code
    local log="$1"; shift
    "$java" -jar "$jar" --spring.profiles.active=golden --carddemo.initial-load.on-startup=false \
        --spring.main.banner-mode=off "$@" >"$log" 2>&1
    local rc=$?
    echo "  exit code $rc ($(basename "$log"))"
    return $rc
}

latest() {  # latest <gdg>: newest generation file (highest job execution id)
    ls "$out/output/$1" 2>/dev/null | awk -F. '{print $NF, $0}' | sort -n | tail -1 | cut -d' ' -f2 \
        | sed "s|^.|$out/output/$1/&|"
}

"$java" -version 2>&1 | head -1
echo "initial-load (REPLACE from app/data/EBCDIC)"
batch "$out/initial-load.log" --job=initial-load --mode=REPLACE || { tail -50 "$out/initial-load.log"; exit 1; }

extra=()
if [ "$mode" = file ]; then
    cp "$base/COMBTRAN/TRANSACT.ksds.txt" "$ds/TRANSACT.ksds"
    cp "$base/XREFFILE/CARDXREF.ksds.txt" "$ds/CARDXREF.ksds"
    cp "$base/CUSTFILE/CUSTDATA.ksds.txt" "$ds/CUSTDATA.ksds"
    cp "$base/INTCALC/ACCTDATA.ksds.txt" "$ds/ACCTDATA.ksds"
    extra=(--STEP010.SORTIN="$ds/TRANSACT.ksds" --STEP040.XREFFILE="$ds/CARDXREF.ksds"
           --STEP040.CUSTFILE="$ds/CUSTDATA.ksds" --STEP040.ACCTFILE="$ds/ACCTDATA.ksds")
else
    batch "$out/reset-TRANSACT.log" --job=idcams-delete --DATASET=TRANSACT --run.id="$(date +%s%N)" || exit 1
    for image in COMBTRAN/TRANSACT INTCALC/ACCTDATA; do
        name="${image#*/}"
        echo "repro $name (from $base/$image.ksds.txt)"
        batch "$out/repro-$name.log" --job=repro --DATASET="$name" --INFILE="$base/$image.ksds.txt" \
            --encoding=ASCII --run.id="$(date +%s%N)" || { tail -50 "$out/repro-$name.log"; exit 1; }
    done
fi

echo "creastmt ($mode)"
sysouts=()
for step in $steps; do sysouts+=(--"$step".SYSOUT="$run/sysout-$step.txt"); done
batch "$run/job.log" --job=creastmt "${common[@]}" "${sysouts[@]}" "${extra[@]}"
echo $? >"$run/rc.txt"
: >"$run/sysout.txt"
for step in $steps; do
    [ -f "$run/sysout-$step.txt" ] && { echo "--- $step"; cat "$run/sysout-$step.txt"; } >>"$run/sysout.txt"
done
grep -E "creastmt (STEP|ended)" "$run/job.log" | sed 's/^.*INFO [^:]*: /  /'
for gdg in $gdgs; do
    file="$(latest "$gdg")"
    if [ -n "$file" ] && [ -f "$file" ]; then
        cp "$file" "$run/$gdg"
        echo "  $gdg generation: ${file#"$out/"}"
    fi
done

expected=()
[ "$mode" = table ] && [ -f scripts/batch/creastmt-table-mode-expected-diffs.txt ] \
    && expected=(--expected-diffs scripts/batch/creastmt-table-mode-expected-diffs.txt)
python3 scripts/batch/compare_creastmt.py --java-dir "$out" --mode "$mode" "${expected[@]}" --report "$out/REPORT.md"
compare_rc=$?

checks_ok=1
if command -v psql >/dev/null; then
    db="${CARDDEMO_DB_URL#jdbc:postgresql://}"; hostport="${db%%/*}"; name="${db#*/}"; name="${name%%\?*}"
    psql_q() { PGPASSWORD="$CARDDEMO_DB_PASSWORD" psql -h "${hostport%%:*}" -p "${hostport##*:}" \
                 -U "$CARDDEMO_DB_USER" -d "$name" -tA -F ' ' -c "$1"; }
    echo "batch_run (cbstm03a):"
    last="$(psql_q "select status || ' ' || return_code || ' ' || read_count || ' ' || write_count from batch_run
                    where step_name is null and job_name = 'cbstm03a' order by batch_run_id desc limit 1")"
    echo "  $last" | tee "$out/batch_run.txt"
    [ "$last" = "COMPLETED 0 312 7894" ] || { echo "::error::batch_run for cbstm03a: '$last'"; checks_ok=0; }
    echo "batch_output_file:"
    for gdg in $gdgs; do
        row="$(psql_q "select gdg_base || ' ' || business_date || ' ' || record_count || ' ' || file_path
                       from batch_output_file where gdg_base = '$gdg' order by output_file_id desc limit 1")"
        echo "  $row" | tee -a "$out/batch_output_file.txt"
        [ "${row##* }" = "$(latest "$gdg")" ] && [[ "$row" == "$gdg 2022-07-06 "* ]] \
            || { echo "::error::batch_output_file row for $gdg: '$row'"; checks_ok=0; }
    done
fi

neg="$out/negative"; mkdir -p "$neg"
echo "failure path: CBSTM03A with an empty TRNXFILE -> RC 16 (ERROR READING TRNXFILE), no statement generation"
: >"$neg/TRXFL.empty"
before="$(ls "$out/output/STATEMNT.PS" "$out/output/STATEMNT.HTML" 2>/dev/null | wc -l)"
batch "$neg/creastmt.log" --job=creastmt "${common[@]}" "${extra[@]}" --STEP040.TRNXFILE="$neg/TRXFL.empty" \
    --STEP040.SYSOUT="$neg/step040.txt"
neg_rc=$?
[ "$neg_rc" = 16 ] || { echo "::error::expected exit code 16, got $neg_rc"; checks_ok=0; }
[ "$(ls "$out/output/STATEMNT.PS" "$out/output/STATEMNT.HTML" 2>/dev/null | wc -l)" = "$before" ] \
    || { echo "::error::a statement generation was catalogued after the abend"; checks_ok=0; }
grep -q "ERROR READING TRNXFILE" "$neg/step040.txt" 2>/dev/null && grep -q "RETURN CODE: 10" "$neg/step040.txt" \
    || { echo "::error::SYSOUT lacks ERROR READING TRNXFILE / RETURN CODE: 10"; checks_ok=0; }

[ "$compare_rc" = 0 ] && [ "$checks_ok" = 1 ] && echo "CREASTMT OK ($mode)" && exit 0
exit 1

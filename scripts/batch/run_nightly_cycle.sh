#!/usr/bin/env bash
# Gate g-batch evidence: runs the whole nightly cycle through the batch CLI of the packaged app with ONE command
# (java -jar carddemo-app.jar --job=nightly-cycle --run-date=2022-07-06, docs/modernization/06-scheduling.md) on the
# freshly loaded sample data, then compares every output of every in-scope job with docs/validation/baseline/<JOB>/
# using the existing compare scripts (compare_print_jobs.py, compare_posttran.py, compare_intcalc.py,
# compare_tranrept.py, compare_creastmt.py) and checks batch_run. Writes the job x mode x result matrix to
# <out-dir>/REPORT.md (and matrix.json for nightly_cycle_report.py --combine).
#
# Unlike run_<job>.sh, which start each job from the baseline after-images of the job before it, every job here
# reads what the Java job before it wrote: the KSDS files/tables updated in place, and the GDG generations
# (DALYREJS, SYSTRAN, TRANSACT.BKUP/COMBINED/DALY, TRXFL...) resolved as (0) from batch_output_file. Any difference
# that only shows up in this chained run needs an entry in scripts/batch/nightly-cycle-<mode>-expected-diffs/<group>.txt
# (one file per compare script; each entry must apply exactly once) — never a broader exclusion.
#
#   scripts/batch/run_nightly_cycle.sh file  <out-dir>   KSDS DDs are files under <out-dir>/ds, initialised from
#                                                         app/data/ASCII (TRANSACT empty) and updated in place
#   scripts/batch/run_nightly_cycle.sh table <out-dir>   KSDS DDs are the PostgreSQL tables after --job=initial-load
#
# Needs the packaged jar and CARDDEMO_DB_URL / CARDDEMO_DB_USER / CARDDEMO_DB_PASSWORD (psql for the batch_run checks).
# Used by scripts/golden-set/run_golden_set.sh through three optional variables:
#   CARDDEMO_BASELINE_DIR=<dir>        COBOL outputs to compare with (default docs/validation/baseline)
#   NIGHTLY_CYCLE_SKIP_LOAD=1          table mode: keep the database as it is instead of running initial-load
#   NIGHTLY_CYCLE_EXPECTED_DIFFS=<dir> the only expected-diffs files (<dir>/<group>.txt), instead of the
#                                      stand-alone and chained files under scripts/batch/
set -uo pipefail
cd "$(dirname "$0")/../.."
mode="${1:?usage: $0 file|table <out-dir>}"
out="$(realpath -m "${2:?usage: $0 file|table <out-dir>}")"
jar="${CARDDEMO_JAR:-modernization/carddemo-app/target/carddemo-app.jar}"
java="${JAVA_HOME:+$JAVA_HOME/bin/}java"
: "${CARDDEMO_DB_URL:?}" "${CARDDEMO_DB_USER:?}" "${CARDDEMO_DB_PASSWORD:?}"
case "$mode" in file|table) ;; *) echo "mode must be file or table" >&2; exit 2 ;; esac
rm -rf "$out" && mkdir -p "$out/ds" "$out/reports"
export CARDDEMO_BATCH_OUTPUT_DIR="$out/output"
base="${CARDDEMO_BASELINE_DIR:-docs/validation/baseline}"
export CARDDEMO_BASELINE_DIR="$base"
ds="$out/ds"
jobs="READACCT READCARD READCUST READXREF POSTTRAN INTCALC TRANBKP COMBTRAN TRANREPT CREASTMT PRTCATBL"
declare -A steps=([TRANBKP]="STEP05R STEP05 STEP10" [COMBTRAN]="STEP05R STEP10" [TRANREPT]="STEP05 STEP10 STEP15"
                  [CREASTMT]="STEP010 STEP020 STEP040" [PRTCATBL]="STEP05R STEP10R")
for job in $jobs CBTRN01C; do mkdir -p "$out/$job"; done

batch() {
    local log="$1"; shift
    "$java" -jar "$jar" --spring.profiles.active=golden --carddemo.initial-load.on-startup=false \
        --spring.main.banner-mode=off "$@" >"$log" 2>&1
    local rc=$?
    echo "  exit code $rc ($(basename "$log"))"
    return $rc
}

generation() {  # generation <gdg> first|last: oldest / newest generation of this run (file suffix = job execution id)
    ls "$out/output/$1" 2>/dev/null | awk -F. '{print $NF, $0}' | sort -n | { [ "$2" = first ] && head -1 || tail -1; } \
        | cut -d' ' -f2 | sed "s|^.|$out/output/$1/&|"
}

keep() {  # keep <JOB> <gdg> [first|last]: copy a generation of <gdg> into the job directory
    local file; file="$(generation "$2" "${3:-last}")"
    if [ -n "$file" ] && [ -f "$file" ]; then
        cp "$file" "$out/$1/$2"
        echo "  $1 $2 generation: ${file#"$out/"}"
    fi
}

"$java" -version 2>&1 | head -1
if [ "${NIGHTLY_CYCLE_SKIP_LOAD:-0}" = 1 ] && [ "$mode" = table ]; then
    echo "initial-load skipped (NIGHTLY_CYCLE_SKIP_LOAD=1: the cycle runs on the database as it is)"
else
    echo "initial-load (REPLACE from app/data/EBCDIC)"
    batch "$out/initial-load.log" --job=initial-load --mode=REPLACE || { tail -50 "$out/initial-load.log"; exit 1; }
fi

args=(--job=nightly-cycle --run-date=2022-07-06 --encoding=ASCII --AFTER-IMAGES="$out"
      --READACCT.record-prefix=GNUCOBOL_VARSEQ_0 --READACCT.OUTFILE="$out/READACCT/OUTFILE"
      --READACCT.ARRYFILE="$out/READACCT/ARRYFILE" --READACCT.VBRCFILE="$out/READACCT/VBRCFILE"
      --POSTTRAN.STEP10.SYSOUT="$out/CBTRN01C/sysout.txt" --POSTTRAN.STEP15.SYSOUT="$out/POSTTRAN/sysout.txt"
      --INTCALC.STEP15.SYSOUT="$out/INTCALC/sysout.txt")
for job in READACCT READCARD READCUST READXREF; do args+=(--$job.SYSOUT="$out/$job/sysout.txt"); done
for job in "${!steps[@]}"; do
    for step in ${steps[$job]}; do args+=(--$job.$step.SYSOUT="$out/$job/sysout-$step.txt"); done
done
declare -A print_dd=([READACCT]=ACCTFILE:acctdata.txt [READCARD]=CARDFILE:carddata.txt
                     [READXREF]=XREFFILE:cardxref.txt [READCUST]=CUSTFILE:custdata.txt)
for job in "${!print_dd[@]}"; do
    dd="${print_dd[$job]%%:*}"
    if [ "$mode" = file ]; then args+=(--$job.$dd="app/data/ASCII/${print_dd[$job]#*:}"); else args+=(--$job.$dd=table); fi
done
if [ "$mode" = file ]; then
    cp app/data/ASCII/acctdata.txt "$ds/ACCTDATA.ksds"
    cp app/data/ASCII/tcatbal.txt "$ds/TCATBALF.ksds"
    : >"$ds/TRANSACT.ksds"
    a=app/data/ASCII
    args+=(--AFTER-IMAGES.TRANSACT="$ds/TRANSACT.ksds" --AFTER-IMAGES.ACCTDATA="$ds/ACCTDATA.ksds"
           --AFTER-IMAGES.TCATBALF="$ds/TCATBALF.ksds"
           --POSTTRAN.DALYTRAN=$a/dailytran.txt --POSTTRAN.XREFFILE=$a/cardxref.txt
           --POSTTRAN.CUSTFILE=$a/custdata.txt --POSTTRAN.CARDFILE=$a/carddata.txt
           --POSTTRAN.ACCTFILE="$ds/ACCTDATA.ksds" --POSTTRAN.TCATBALF="$ds/TCATBALF.ksds"
           --POSTTRAN.TRANFILE="$ds/TRANSACT.ksds"
           --INTCALC.TCATBALF="$ds/TCATBALF.ksds" --INTCALC.XREFFILE=$a/cardxref.txt
           --INTCALC.ACCTFILE="$ds/ACCTDATA.ksds" --INTCALC.DISCGRP=$a/discgrp.txt
           --TRANBKP.STEP05R.FILEIN="$ds/TRANSACT.ksds" --TRANBKP.CLUSTER="$ds/TRANSACT.ksds"
           --COMBTRAN.STEP10.OUTFILE="$ds/TRANSACT.ksds"
           --TRANREPT.STEP05.FILEIN="$ds/TRANSACT.ksds" --TRANREPT.STEP10.SORTIN="$ds/TRANSACT.ksds"
           --TRANREPT.STEP15.CARDXREF=$a/cardxref.txt --TRANREPT.STEP15.TRANTYPE=$a/trantype.txt
           --TRANREPT.STEP15.TRANCATG=$a/trancatg.txt --TRANREPT.STEP15.DATEPARM="$base/00-DATA/DATEPARM.txt"
           --CREASTMT.STEP010.SORTIN="$ds/TRANSACT.ksds" --CREASTMT.STEP040.XREFFILE=$a/cardxref.txt
           --CREASTMT.STEP040.CUSTFILE=$a/custdata.txt --CREASTMT.STEP040.ACCTFILE="$ds/ACCTDATA.ksds"
           --PRTCATBL.STEP05R.FILEIN="$ds/TCATBALF.ksds")
fi

echo "nightly-cycle ($mode): one launch, $(wc -w <<<"$jobs") jobs"
batch "$out/nightly-cycle.log" "${args[@]}"
cycle_rc=$?
echo "$cycle_rc" >"$out/rc.txt"
grep -E "nightly-cycle [A-Z0-9]+ [^ ]+: " "$out/nightly-cycle.log" | sed 's/^.*INFO [^:]*: /  /'

for job in $jobs; do
    line="$(grep -E "nightly-cycle $job [^ ]+: (RC=|bypassed)" "$out/nightly-cycle.log" | tail -1)"
    if [[ "$line" =~ RC=0*([0-9]+) ]]; then echo "${BASH_REMATCH[1]:-0}" >"$out/$job/rc.txt"; fi
    if [ -n "${steps[$job]:-}" ]; then
        : >"$out/$job/sysout.txt"
        for step in ${steps[$job]}; do
            [ -f "$out/$job/sysout-$step.txt" ] && { echo "--- $step"; cat "$out/$job/sysout-$step.txt"; } \
                >>"$out/$job/sysout.txt"
        done
    fi
done
keep POSTTRAN DALYREJS
keep INTCALC SYSTRAN
keep TRANBKP TRANSACT.BKUP first
keep COMBTRAN TRANSACT.COMBINED
for gdg in TRANSACT.BKUP TRANSACT.DALY TRANREPT; do keep TRANREPT "$gdg"; done
for gdg in TRXFL.SEQ TRXFL STATEMNT.PS STATEMNT.HTML; do keep CREASTMT "$gdg"; done
for gdg in TCATBALF.BKUP TCATBALF.REPT; do keep PRTCATBL "$gdg"; done

expected() {  # expected <group> <stand-alone table-mode file>: --expected-diffs for this group, if any
    local chained="scripts/batch/nightly-cycle-$mode-expected-diffs/$1.txt"
    local files=()
    if [ -n "${NIGHTLY_CYCLE_EXPECTED_DIFFS:-}" ]; then
        [ -f "$NIGHTLY_CYCLE_EXPECTED_DIFFS/$1.txt" ] && echo "--expected-diffs" "$NIGHTLY_CYCLE_EXPECTED_DIFFS/$1.txt"
        return
    fi
    [ "$mode" = table ] && [ -n "$2" ] && [ -f "scripts/batch/$2" ] && files+=("scripts/batch/$2")
    [ -f "$chained" ] && files+=("$chained")
    [ ${#files[@]} -eq 0 ] && return
    if [ ${#files[@]} -eq 1 ]; then echo "--expected-diffs" "${files[0]}"; return; fi
    cat "${files[@]}" >"$out/reports/$1-expected-diffs.txt"
    echo "--expected-diffs" "$out/reports/$1-expected-diffs.txt"
}
compare() {  # compare <group> <script> <stand-alone expected-diffs file> [args...]
    local group="$1" script="$2" exp="$3"; shift 3
    # shellcheck disable=SC2046
    python3 "scripts/batch/$script" --java-dir "$out" $(expected "$group" "$exp") "$@" \
        --report "$out/reports/$group.md" >"$out/reports/$group.log" 2>&1
    local rc=$?
    echo "$rc" >"$out/reports/$group.rc"
    echo "  $script: $([ $rc -eq 0 ] && echo PASS || echo "FAIL (see reports/$group.md)")"
}
echo "compare with $base"
compare print compare_print_jobs.py table-mode-expected-diffs.txt --vb-format varseq0 \
    --title "Print jobs (nightly-cycle, $mode input) vs GnuCOBOL baseline"
compare posttran compare_posttran.py posttran-table-mode-expected-diffs.txt --mode "$mode"
compare intcalc compare_intcalc.py intcalc-table-mode-expected-diffs.txt --mode "$mode"
compare tranrept compare_tranrept.py tranrept-table-mode-expected-diffs.txt --mode "$mode"
compare creastmt compare_creastmt.py creastmt-table-mode-expected-diffs.txt --mode "$mode"

if command -v psql >/dev/null; then
    db="${CARDDEMO_DB_URL#jdbc:postgresql://}"; hostport="${db%%/*}"; name="${db#*/}"; name="${name%%\?*}"
    psql_q() { PGPASSWORD="$CARDDEMO_DB_PASSWORD" psql -h "${hostport%%:*}" -p "${hostport##*:}" \
                 -U "$CARDDEMO_DB_USER" -d "$name" -tA -F '|' -c "$1"; }
    cycle="$(psql_q "select job_execution_id from batch_run where job_name = 'nightly-cycle' and step_name is null
                     order by batch_run_id desc limit 1")"
    psql_q "select coalesce(step_name, '-'), status, exit_code, return_code, read_count, write_count, skip_count,
                   filter_count from batch_run where job_execution_id = ${cycle:-0} order by batch_run_id" \
        >"$out/batch_run.txt"
    psql_q "select job_name, coalesce(step_name, '-'), status, return_code, read_count, write_count,
                   substring(parameters from 'cycle\.member=([A-Z0-9]+)')
            from batch_run where job_execution_id > ${cycle:-0} and step_name is null
              and parameters like '%cycle.member=%' order by batch_run_id" >"$out/batch_run_children.txt"
    echo "batch_run (nightly-cycle job execution ${cycle:-?}):"
    sed 's/^/  /' "$out/batch_run.txt"
else
    echo "::warning::psql not found: batch_run not checked"
fi

python3 scripts/batch/nightly_cycle_report.py --out "$out" --mode "$mode" --cycle-rc "$cycle_rc"

#!/usr/bin/env bash
# Runs the reporting and housekeeping jobs TRANBKP, COMBTRAN, TRANREPT and PRTCATBL through the batch CLI of the
# packaged app (java -jar carddemo-app.jar --job=<stream> ...) in the baseline order and compares them with
# docs/validation/baseline/{TRANBKP,COMBTRAN,TRANREPT,PRTCATBL} (scripts/batch/compare_tranrept.py). Every GDG
# (TRANSACT.BKUP, TRANSACT.COMBINED, TRANSACT.DALY, TRANREPT, TCATBALF.BKUP, TCATBALF.REPT) is a dated generation
# (ADR-0012); the KSDS DDs (TRANSACT, TCATBALF) are files or tables.
#
# Each job starts from the baseline after-images of the job before it (docs/validation/baseline/00-ORDER.md:
# POSTTRAN -> INTCALC -> TRANBKP -> COMBTRAN -> TRANREPT -> PRTCATBL), never from another Java job's output:
#   TRANBKP   TRANSACT = POSTTRAN/TRANSACT.ksds.txt
#   COMBTRAN  TRANSACT = TRANBKP/TRANSACT.ksds.txt (empty), SORTIN = TRANBKP/TRANSACT.BKUP.txt,
#             SORTIN02 = INTCALC/TRANSACT.txt (SYSTRAN)
#   TRANREPT  TRANSACT = COMBTRAN/TRANSACT.ksds.txt; CARDXREF/TRANTYPE/TRANCATG app/data/ASCII (file) or the
#             initial-load tables; DATEPARM 00-DATA/DATEPARM.txt (file) or the golden window 2022-01-01..2022-07-06
#   PRTCATBL  TCATBALF = POSTTRAN/TCATBALF.ksds.txt (INTCALC opens it INPUT)
#
#   scripts/batch/run_tranrept.sh file  <out-dir>   KSDS DDs are copies of the after-images (updated in place)
#   scripts/batch/run_tranrept.sh table <out-dir>   KSDS DDs are PostgreSQL: --job=initial-load, then --job=idcams-delete
#                                                    + --job=repro of the after-image; unloaded with --job=unload
#
# Then checks two failure paths: TRANREPT with a missing TRANSACT ends with RC 16 and catalogues no TRANREPT
# generation; COMBTRAN re-run over the loaded TRANSACT ends with RC 12 (duplicate keys) and loads nothing.
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
rm -rf "$out" && mkdir -p "$out/ds" "$out/TRANBKP" "$out/COMBTRAN" "$out/TRANREPT" "$out/PRTCATBL"
export CARDDEMO_BATCH_OUTPUT_DIR="$out/output"
base=docs/validation/baseline
ds="$out/ds"
common=(--run-date=2022-07-06 --encoding=ASCII)

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

start_ksds() {  # start_ksds <DATASET> <after-image>: the KSDS a job starts from
    local name="$1" image="$2"
    if [ "$mode" = file ]; then
        cp "$image" "$ds/$name.ksds"
    else
        batch "$out/reset-$name-$(date +%s%N).log" --job=idcams-delete --DATASET="$name" --run.id="$(date +%s%N)" \
            || exit 1
        if [ -s "$image" ]; then
            batch "$out/repro-$name-$(date +%s%N).log" --job=repro --DATASET="$name" --INFILE="$image" \
                --encoding=ASCII --run.id="$(date +%s%N)" || exit 1
        fi
    fi
}

after_image() {  # after_image <JOB> <DATASET>
    if [ "$mode" = file ]; then
        cp "$ds/$2.ksds" "$out/$1/$2.ksds"
    else
        batch "$out/$1/unload-$2.log" --job=unload --DATASET="$2" --encoding=ASCII --OUTFILE="$out/$1/$2.ksds"
    fi
}

ksds_args() {  # ksds_args <DD-args...>: --<dd>=<file> in file mode, nothing (tables) in table mode
    [ "$mode" = file ] && printf '%s\n' "$@"
}

run_job() {  # run_job <JOB> <stream> <steps> <args...>: run a stream, keep RC, step SYSOUTs, job log
    local job="$1" stream="$2" steps="$3"; shift 3
    local sysouts=()
    for step in $steps; do sysouts+=(--"$step".SYSOUT="$out/$job/sysout-$step.txt"); done
    echo "$stream ($mode)"
    batch "$out/$job/job.log" --job="$stream" "${common[@]}" "${sysouts[@]}" "$@"
    echo $? >"$out/$job/rc.txt"
    : >"$out/$job/sysout.txt"
    for step in $steps; do
        [ -f "$out/$job/sysout-$step.txt" ] && { echo "--- $step"; cat "$out/$job/sysout-$step.txt"; } \
            >>"$out/$job/sysout.txt"
    done
    grep -E "$stream (STEP|ended)" "$out/$job/job.log" | sed 's/^.*INFO [^:]*: /  /'
}

keep() {  # keep <JOB> <gdg>: copy the newest generation of <gdg> into the job directory
    local file; file="$(latest "$2")"
    if [ -n "$file" ] && [ -f "$file" ]; then
        cp "$file" "$out/$1/$2"
        echo "  $2 generation: ${file#"$out/"}"
    fi
}

"$java" -version 2>&1 | head -1
echo "initial-load (REPLACE from app/data/EBCDIC)"
batch "$out/initial-load.log" --job=initial-load --mode=REPLACE || { tail -50 "$out/initial-load.log"; exit 1; }

# TRANBKP: STEP05R reproc, STEP05 idcams-delete, STEP10 idcams-define
start_ksds TRANSACT "$base/POSTTRAN/TRANSACT.ksds.txt"
mapfile -t extra < <(ksds_args --STEP05R.FILEIN="$ds/TRANSACT.ksds" --CLUSTER="$ds/TRANSACT.ksds")
run_job TRANBKP tranbkp "STEP05R STEP05 STEP10" "${extra[@]}"
keep TRANBKP TRANSACT.BKUP
after_image TRANBKP TRANSACT

# COMBTRAN: STEP05R combtran-sort, STEP10 idcams-repro
start_ksds TRANSACT "$base/TRANBKP/TRANSACT.ksds.txt"
mapfile -t extra < <(ksds_args --STEP10.OUTFILE="$ds/TRANSACT.ksds")
run_job COMBTRAN combtran "STEP05R STEP10" --STEP05R.SORTIN="$base/TRANBKP/TRANSACT.BKUP.txt" \
    --STEP05R.SORTIN02="$base/INTCALC/TRANSACT.txt" "${extra[@]}"
keep COMBTRAN TRANSACT.COMBINED
after_image COMBTRAN TRANSACT

# TRANREPT: STEP05 reproc, STEP10 tranrept-sort, STEP15 cbtrn03c
start_ksds TRANSACT "$base/COMBTRAN/TRANSACT.ksds.txt"
mapfile -t extra < <(ksds_args --STEP05.FILEIN="$ds/TRANSACT.ksds" --STEP10.SORTIN="$ds/TRANSACT.ksds" \
    --STEP15.CARDXREF=app/data/ASCII/cardxref.txt --STEP15.TRANTYPE=app/data/ASCII/trantype.txt \
    --STEP15.TRANCATG=app/data/ASCII/trancatg.txt --STEP15.DATEPARM="$base/00-DATA/DATEPARM.txt")
run_job TRANREPT tranrept "STEP05 STEP10 STEP15" "${extra[@]}"
for gdg in TRANSACT.BKUP TRANSACT.DALY TRANREPT; do keep TRANREPT "$gdg"; done
after_image TRANREPT TRANSACT

# PRTCATBL: STEP05R reproc, STEP10R prtcatbl-sort
if [ "$mode" = file ]; then
    cp "$base/POSTTRAN/TCATBALF.ksds.txt" "$ds/TCATBALF.ksds"
else
    batch "$out/repro-TCATBALF.log" --job=repro --DATASET=TCATBALF --INFILE="$base/POSTTRAN/TCATBALF.ksds.txt" \
        --encoding=ASCII --run.id="$(date +%s%N)" || exit 1
fi
mapfile -t extra < <(ksds_args --STEP05R.FILEIN="$ds/TCATBALF.ksds")
run_job PRTCATBL prtcatbl "STEP05R STEP10R" "${extra[@]}"
for gdg in TCATBALF.BKUP TCATBALF.REPT; do keep PRTCATBL "$gdg"; done

expected=()
[ "$mode" = table ] && [ -f scripts/batch/tranrept-table-mode-expected-diffs.txt ] \
    && expected=(--expected-diffs scripts/batch/tranrept-table-mode-expected-diffs.txt)
python3 scripts/batch/compare_tranrept.py --java-dir "$out" --mode "$mode" "${expected[@]}" \
    --report "$out/REPORT.md"
compare_rc=$?

checks_ok=1
if command -v psql >/dev/null; then
    db="${CARDDEMO_DB_URL#jdbc:postgresql://}"; hostport="${db%%/*}"; name="${db#*/}"; name="${name%%\?*}"
    psql_q() { PGPASSWORD="$CARDDEMO_DB_PASSWORD" psql -h "${hostport%%:*}" -p "${hostport##*:}" \
                 -U "$CARDDEMO_DB_USER" -d "$name" -tA -F ' ' -c "$1"; }
    echo "batch_run (cbtrn03c):"
    last="$(psql_q "select status || ' ' || return_code || ' ' || read_count || ' ' || write_count from batch_run
                    where step_name is null and job_name = 'cbtrn03c' order by batch_run_id desc limit 1")"
    echo "  $last" | tee "$out/batch_run.txt"
    [ "$last" = "COMPLETED 0 312 519" ] || { echo "::error::batch_run for cbtrn03c: '$last'"; checks_ok=0; }
    echo "batch_output_file:"
    for gdg in TRANSACT.BKUP TRANSACT.COMBINED TRANSACT.DALY TRANREPT TCATBALF.BKUP TCATBALF.REPT; do
        row="$(psql_q "select gdg_base || ' ' || business_date || ' ' || record_count || ' ' || file_path
                       from batch_output_file where gdg_base = '$gdg' order by output_file_id desc limit 1")"
        echo "  $row" | tee -a "$out/batch_output_file.txt"
        [ "${row##* }" = "$(latest "$gdg")" ] && [[ "$row" == "$gdg 2022-07-06 "* ]] \
            || { echo "::error::batch_output_file row for $gdg: '$row'"; checks_ok=0; }
    done
fi

neg="$out/negative"; mkdir -p "$neg"
echo "failure path: TRANREPT with a missing TRANSACT -> RC 16, no TRANREPT generation"
before="$(ls "$out/output/TRANREPT" 2>/dev/null | wc -l)"
batch "$neg/tranrept.log" --job=tranrept "${common[@]}" --STEP05.FILEIN="$neg/does-not-exist" \
    --STEP05.SYSOUT="$neg/step05.txt" --STEP15.SYSOUT="$neg/step15.txt"
neg_rc=$?
[ "$neg_rc" = 16 ] || { echo "::error::expected exit code 16, got $neg_rc"; checks_ok=0; }
[ "$(ls "$out/output/TRANREPT" 2>/dev/null | wc -l)" = "$before" ] \
    || { echo "::error::a TRANREPT generation was catalogued after the failed backup"; checks_ok=0; }
[ ! -e "$neg/step15.txt" ] || { echo "::error::STEP15 ran after the STEP05 abend"; checks_ok=0; }

echo "failure path: COMBTRAN again over the loaded TRANSACT -> RC 12 (duplicate keys), nothing loaded"
start_ksds TRANSACT "$base/COMBTRAN/TRANSACT.ksds.txt"
mapfile -t extra < <(ksds_args --STEP10.OUTFILE="$ds/TRANSACT.ksds")
batch "$neg/combtran.log" --job=combtran "${common[@]}" --STEP05R.SORTIN="$base/TRANBKP/TRANSACT.BKUP.txt" \
    --STEP05R.SORTIN02="$base/INTCALC/TRANSACT.txt" --STEP10.SYSOUT="$neg/combtran-step10.txt" "${extra[@]}"
neg_rc=$?
[ "$neg_rc" = 12 ] || { echo "::error::expected exit code 12, got $neg_rc"; checks_ok=0; }
grep -q "IDC3316I DUPLICATE RECORD" "$neg/combtran-step10.txt" 2>/dev/null \
    || { echo "::error::SYSPRINT lacks the duplicate-record message"; checks_ok=0; }
after_image negative TRANSACT
cmp -s <(tr -d '\r' <"$base/COMBTRAN/TRANSACT.ksds.txt" | sed 's/ *$//') <(sed 's/ *$//' "$neg/TRANSACT.ksds") \
    || { echo "::error::TRANSACT changed after the refused load"; checks_ok=0; }

[ "$compare_rc" = 0 ] && [ "$checks_ok" = 1 ] && echo "TRANREPT/TRANBKP/COMBTRAN/PRTCATBL OK ($mode)" && exit 0
exit 1

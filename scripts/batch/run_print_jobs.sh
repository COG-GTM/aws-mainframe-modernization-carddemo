#!/usr/bin/env bash
# Runs the READACCT/READCARD/READXREF/READCUST print jobs through the batch CLI of the packaged app
# (java -jar carddemo-app.jar --job=<name> ...) and compares their SYSOUT, datasets and exit codes with
# docs/validation/baseline/<JOB>/ (scripts/batch/compare_print_jobs.py). Then checks the failure path: a missing
# input dataset must end the process with RC 16 and an ABENDED batch_run row.
#
#   scripts/batch/run_print_jobs.sh file  <out-dir>   KSDS DDs read the app/data/ASCII unloads the baseline used
#   scripts/batch/run_print_jobs.sh table <out-dir>   KSDS DDs read PostgreSQL after --job=initial-load
#
# Needs CARDDEMO_DB_URL / CARDDEMO_DB_USER / CARDDEMO_DB_PASSWORD (PostgreSQL 16) and a built jar
# (CARDDEMO_JAR, default modernization/carddemo-app/target/carddemo-app.jar). JAVA_HOME selects the JDK (21).
set -uo pipefail
cd "$(dirname "$0")/../.."
mode="${1:?usage: $0 file|table <out-dir>}"
out="$(realpath -m "${2:?usage: $0 file|table <out-dir>}")"
jar="${CARDDEMO_JAR:-modernization/carddemo-app/target/carddemo-app.jar}"
java="${JAVA_HOME:+$JAVA_HOME/bin/}java"
: "${CARDDEMO_DB_URL:?}" "${CARDDEMO_DB_USER:?}" "${CARDDEMO_DB_PASSWORD:?}"
case "$mode" in file|table) ;; *) echo "mode must be file or table" >&2; exit 2 ;; esac
rm -rf "$out" && mkdir -p "$out"

batch() {  # batch <log> <args...>: run one CLI job, print and return its exit code
    local log="$1"; shift
    "$java" -jar "$jar" --spring.profiles.active=golden --carddemo.initial-load.on-startup=false \
        --spring.main.banner-mode=off "$@" >"$log" 2>&1
    local rc=$?
    echo "  exit code $rc ($(basename "$log"))"
    return $rc
}

"$java" -version 2>&1 | head -1
if [ "$mode" = table ]; then
    echo "initial-load (REPLACE from app/data/EBCDIC)"
    batch "$out/initial-load.log" --job=initial-load --mode=REPLACE || { tail -50 "$out/initial-load.log"; exit 1; }
fi

declare -A input=([READACCT]=ACCTFILE:acctdata.txt [READCARD]=CARDFILE:carddata.txt
                  [READXREF]=XREFFILE:cardxref.txt [READCUST]=CUSTFILE:custdata.txt)
for job in READACCT READCARD READXREF READCUST; do
    dir="$out/$job"; mkdir -p "$dir"
    dd="${input[$job]%%:*}"
    args=(--job="$job" --run-date=2022-07-06 --encoding=ASCII --SYSOUT="$dir/sysout.txt")
    if [ "$mode" = file ]; then args+=(--"$dd"="app/data/ASCII/${input[$job]#*:}"); else args+=(--"$dd"=table); fi
    if [ "$job" = READACCT ]; then
        args+=(--record-prefix=GNUCOBOL_VARSEQ_0 --OUTFILE="$dir/OUTFILE" --ARRYFILE="$dir/ARRYFILE"
               --VBRCFILE="$dir/VBRCFILE")
    fi
    echo "$job (${dd} from $mode)"
    batch "$dir/job.log" "${args[@]}"
    echo $? >"$dir/rc.txt"
done

expected=()
[ "$mode" = table ] && expected=(--expected-diffs scripts/batch/table-mode-expected-diffs.txt)
python3 scripts/batch/compare_print_jobs.py --java-dir "$out" --vb-format varseq0 "${expected[@]}" \
    --title "Print jobs ($mode input) vs GnuCOBOL baseline" --report "$out/REPORT.md"
compare_rc=$?

echo "failure path: READCARD with a missing CARDFILE must abend with RC 16"
neg="$out/negative"; mkdir -p "$neg"
batch "$neg/job.log" --job=READCARD --CARDFILE="$neg/does-not-exist" --encoding=ASCII --SYSOUT="$neg/sysout.txt"
neg_rc=$?
negative_ok=1
[ "$neg_rc" = 16 ] || { echo "::error::expected exit code 16, got $neg_rc"; negative_ok=0; }
grep -q "ERROR OPENING CARDFILE" "$neg/sysout.txt" && grep -q "ABENDING PROGRAM" "$neg/sysout.txt" \
    || { echo "::error::abend DISPLAYs missing from SYSOUT"; negative_ok=0; }
cat "$neg/sysout.txt"
echo "failure path: unknown job must exit 16"
batch "$neg/unknown.log" --job=NOSUCHJOB; unknown_rc=$?
[ "$unknown_rc" = 16 ] || { echo "::error::expected exit code 16 for an unknown job, got $unknown_rc"; negative_ok=0; }

if command -v psql >/dev/null; then
    db="${CARDDEMO_DB_URL#jdbc:postgresql://}"; hostport="${db%%/*}"; name="${db#*/}"; name="${name%%\?*}"
    psql_q() { PGPASSWORD="$CARDDEMO_DB_PASSWORD" psql -h "${hostport%%:*}" -p "${hostport##*:}" \
                 -U "$CARDDEMO_DB_USER" -d "$name" -tA -F ' ' -c "$1"; }
    echo "batch_run (latest 10 job rows):"
    psql_q "select batch_run_id, job_name, status, return_code, read_count, write_count, coalesce(message, '')
            from batch_run where step_name is null order by batch_run_id desc limit 10" | tee "$out/batch_run.txt"
    last="$(psql_q "select job_name || ' ' || status || ' ' || return_code from batch_run
                    where step_name is null and job_name = 'readcard' order by batch_run_id desc limit 1")"
    [ "$last" = "readcard FAILED 16" ] || { echo "::error::batch_run for the abended READCARD: '$last'"; negative_ok=0; }
fi

[ "$compare_rc" = 0 ] && [ "$negative_ok" = 1 ] && echo "print jobs OK ($mode)" && exit 0
exit 1

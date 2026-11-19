#!/usr/bin/env bash
# POSTTRAN end-to-end on the real local stack: `docker compose` in modernization/ (postgres:16 + the app image), job POSTTRAN
# launched through the CLI seam in the app container, masters seeded and tables unloaded with KsdsDataService (D-053),
# every leg compared with the committed GnuCOBOL oracle outputs (posttran_e2e_oracle.sh, posttran_e2e_compare.py).
# Usage: .migration/tools/posttran_e2e.sh WORK_DIR [ORACLE_DIR]   (needs the jar: mvn -B -DskipTests package; WORK_DIR must not exist)
#        ORACLE_DIR default functional/CARDDEMO/POSTTRAN/evidence/oracle. Env CARDDEMO_DB_PORT (default 55432), JAVA (default java).
# Leaves the compose stack running so the DB can be inspected; stop it with `docker compose down -v` in modernization/.
set -euo pipefail
REPO=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(realpath -m -- "${1:?WORK_DIR}")
ORACLE=$(realpath -e -- "${2:-$REPO/functional/CARDDEMO/POSTTRAN/evidence/oracle}")
JAVA=${JAVA:-java}
export CARDDEMO_DB_PORT=${CARDDEMO_DB_PORT:-55432}
JAR="$REPO/modernization/carddemo-app/target/carddemo-app.jar"
[ -e "$WORK" ] && { echo "refusing existing WORK_DIR $WORK" >&2; exit 2; }
[ -f "$JAR" ] || { echo "missing $JAR (mvn -B -DskipTests package)" >&2; exit 2; }
mkdir -p "$WORK/jar"
"$REPO/.migration/tools/posttran_e2e_inputs.sh" "$WORK/in" > /dev/null
(cd "$WORK/jar" && unzip -q "$JAR")
CP="$WORK/jar/BOOT-INF/classes:$WORK/jar/BOOT-INF/lib/*"
DB=(--spring.datasource.url=jdbc:postgresql://localhost:$CARDDEMO_DB_PORT/carddemo --spring.datasource.username=carddemo
    --spring.datasource.password=carddemo --spring.main.banner-mode=off --logging.level.root=WARN)
DC=(docker compose -f "$REPO/modernization/docker-compose.yml")
CLOCK=--carddemo.batch.clock=2022-07-18T10:30:45
FAILS=0

"${DC[@]}" up -d --wait postgres
"${DC[@]}" build -q app
"${DC[@]}" run --rm app --job=NOOP > "$WORK/noop.log" 2>&1   # Flyway V1..V3 migrate on this first context start

data() { "$JAVA" -cp "$CP" "$REPO/.migration/tools/PosttranE2eData.java" "$@" "${DB[@]}" > /dev/null; }
seed() { data load "$REPO/app/data/ASCII" "$WORK/in/dailytran.txt"; }
# job LEG RUN_ID DALYTRAN [extra args]: runs POSTTRAN in the app container; DALYREJS/SYSOUT under $WORK/LEG
job() {
  local leg=$1 run=$2 daly=$3; shift 3
  mkdir -p "$WORK/$leg"
  set +e
  "${DC[@]}" run --rm -T --user "$(id -u):$(id -g)" -v "$WORK:/e2e" app --job=POSTTRAN --run-id="$run" \
    --dd=DALYTRAN="/e2e/$daly" --dd=SYSOUT="/e2e/$leg/SYSOUT" "$CLOCK" "$@" > "$WORK/$leg/job.log" 2>&1
  echo $? > "$WORK/$leg/RC"
  set -e
  data unload "$WORK/$leg/unload"
  echo "== $leg (run-id $run): RC $(cat "$WORK/$leg/RC")"
}
check() { if python3 "$REPO/.migration/tools/posttran_e2e_compare.py" "$@"; then :; else FAILS=$((FAILS + 1)); fi; }
expect_rc() { local got; got=$(cat "$WORK/$1/RC"); if [ "$got" = "$2" ]; then echo "  PASS RC $got (expected $2)"; else echo "  FAIL RC $got (expected $2)"; FAILS=$((FAILS + 1)); fi; }
same_tables() { if diff -r "$WORK/$1/unload" "$WORK/$2/unload" > /dev/null; then echo "  PASS tables unchanged vs $2"; else echo "  FAIL tables changed vs $2"; FAILS=$((FAILS + 1)); fi; }

# 1 baseline: sample masters + sample DALYTRAN, new instance (stale TRANSACT row must go)
seed
job 01-baseline E2E-BASE in/DALYTRAN --dd=DALYREJS=/e2e/01-baseline/DALYREJS
check "$WORK/01-baseline" "$ORACLE/baseline"
"${DC[@]}" exec -T postgres psql -U carddemo -d carddemo -c "select 'accounts' t, count(*) from account.accounts union all select 'card_xref', count(*) from card.card_xref union all select 'tran_cat_balances', count(*) from reference.tran_cat_balances union all select 'transactions', count(*) from transaction.transactions union all select 'stale rows', count(*) from transaction.transactions where tran_id like 'STALE%'" \
  -c "select acct_id, curr_bal, curr_bal_negative_zero, curr_cyc_credit, curr_cyc_debit, credit_limit from account.accounts order by acct_id limit 5" \
  -c "select tran_id, type_cd, cat_cd, amount, card_num, proc_ts from transaction.transactions order by tran_id limit 5" \
  -c "select job_instance_id, job_name from batch.batch_job_instance order by 1" \
  -c "select e.job_execution_id, i.job_name, e.status, e.exit_code from batch.batch_job_execution e join batch.batch_job_instance i using (job_instance_id) order by 1" \
  > "$WORK/01-baseline/db_rows.txt" 2>&1 || echo "  (psql query failed, see db_rows.txt)"
# 2 relaunch of a COMPLETED run-id: JCL-error class, nothing runs (D-033)
job 02-relaunch-completed E2E-BASE in/DALYTRAN --dd=DALYREJS=/e2e/02-relaunch-completed/DALYREJS
expect_rc 02-relaunch-completed 12; same_tables 02-relaunch-completed 01-baseline
# 3 rerun = new instance on the same tables (D-024): TRANSACT re-created, ACCTDATA/TCATBALF reposted, as OPEN OUTPUT does
job 03-rerun E2E-RERUN in/DALYTRAN --dd=DALYREJS=/e2e/03-rerun/DALYREJS
check "$WORK/03-rerun" "$ORACLE/rerun"
# 4 FR-POSTTRAN-17: DALYREJS already exists for a new instance -> exit 12, nothing posted, file untouched
sha_before=$(sha256sum "$WORK/03-rerun/DALYREJS" | cut -d' ' -f1)
job 04-dalyrejs-exists E2E-EXISTS in/DALYTRAN --dd=DALYREJS=/e2e/03-rerun/DALYREJS
expect_rc 04-dalyrejs-exists 12; same_tables 04-dalyrejs-exists 03-rerun
[ "$(sha256sum "$WORK/03-rerun/DALYREJS" | cut -d' ' -f1)" = "$sha_before" ] && echo "  PASS existing DALYREJS untouched" || { echo "  FAIL existing DALYREJS modified"; FAILS=$((FAILS + 1)); }
# 5 empty DALYTRAN on fresh masters
seed
job 05-empty E2E-EMPTY in/DALYTRAN.empty --dd=DALYREJS=/e2e/05-empty/DALYREJS
check "$WORK/05-empty" "$ORACLE/empty"
# 6 restart after a forced failure: record 151 duplicates record 1's TRAN-ID -> abend after 150 committed records;
#   then the original file is put back under the same path and the same run-id is relaunched (Spring Batch restart)
seed
cp "$WORK/in/DALYTRAN.dup151" "$WORK/in/DALYTRAN.restart"
job 06-restart-leg1 E2E-RESTART in/DALYTRAN.restart --dd=DALYREJS=/e2e/06-restart/DALYREJS
cp "$WORK/06-restart/DALYREJS" "$WORK/06-restart-leg1/DALYREJS"; cp "$WORK/06-restart-leg1/SYSOUT" "$WORK/06-restart-leg1/SYSOUT.leg1"
check "$WORK/06-restart-leg1" "$ORACLE/restart-leg1" --tables-from "$ORACLE/restart-leg1-atomic"
cp "$WORK/in/DALYTRAN" "$WORK/in/DALYTRAN.restart"
job 06-restart-leg2 E2E-RESTART in/DALYTRAN.restart --dd=DALYREJS=/e2e/06-restart/DALYREJS
cp "$WORK/06-restart/DALYREJS" "$WORK/06-restart-leg2/DALYREJS"
check "$WORK/06-restart-leg2" "$ORACLE/baseline" --sysout-tail 3
# 7 abend: DALYTRAN DD names a missing file -> ERROR OPENING DALYTRAN, exit 16 (oracle 231), no DALYREJS, tables untouched
job 07-abend-dalytran-missing E2E-ABEND in/NO-SUCH-DALYTRAN --dd=DALYREJS=/e2e/07-abend-dalytran-missing/DALYREJS
check "$WORK/07-abend-dalytran-missing" "$ORACLE/abend-dalytran-missing" --no-tables; same_tables 07-abend-dalytran-missing 06-restart-leg2
# 8 CP037 (EBCDIC sample DALYTRAN) on fresh masters
seed
job 08-cp037 E2E-CP037 in/DALYTRAN.cp037 --dd=DALYREJS=/e2e/08-cp037/DALYREJS --carddemo.posttran.dalytran-encoding=IBM037
check "$WORK/08-cp037" "$ORACLE/cp037"
# NFR-04: no full card number from CARDXREF in any technical job log
cut -c1-16 "$REPO/app/data/ASCII/cardxref.txt" | tr -d '\r' > "$WORK/cards.txt"
hits=$(cat "$WORK"/*/job.log | grep -cFf "$WORK/cards.txt" || true)
[ "$hits" = 0 ] && echo "== NFR-04: PASS 0 job-log lines carry a full card number" || { echo "== NFR-04: FAIL $hits job-log lines carry a full card number"; FAILS=$((FAILS + 1)); }
echo "== e2e FAILS=$FAILS"
[ "$FAILS" = 0 ]

# CardDemo batch on AWS (`aws/batch/`)

JCL/COBOL batch of CardDemo refactored to **Java 21 / Spring Boot 3 / Spring Batch**, packaged as **one container
image** that runs one job per invocation on **AWS Batch (Fargate)**, chained by **AWS Step Functions**.
Data lives in **Aurora PostgreSQL** (`aws/contracts/data-model.md`), files in **S3** (`aws/contracts/batch.md` §1.2).

```
java -jar carddemo-batch.jar --job=<job-name> [--runId=<id>] [--businessDate=yyyy-MM-dd] [--<param>=<value> ...]
docker run carddemo-batch --job=<job-name> ...
```

## Layout

| Path | Content |
|---|---|
| `src/main/java/com/carddemo/batch/core` | `JobRunner` (arg parsing, `runId` idempotency via `batch_job_run`, Spring Batch execution, run-result JSON, exit code), `ReturnCode` |
| `.../posttran`, `intcalc`, `creastmt`, `combtran`, `tranbkp`, `tranrept`, `tranextr`, `refdata` | One package per legacy job (see mapping table) |
| `.../record` | Fixed-width copybook records (`CVTRA05Y`, `CVACT01Y`, …), zoned/overpunch (`Zoned`), COBOL truncation (`Cobol.fit`), edited pictures (`Edited`) |
| `.../storage` | `ObjectStore` = S3 (`S3_BUCKET`) or a local directory (`carddemo.storage.local-dir`), canonical keys (`S3Keys`) |
| `aws/job-definitions/*.json` | AWS Batch Fargate job definitions `carddemo-<job-name>` (`register-job-definition --cli-input-json`) |
| `aws/state-machine/daily-cycle.asl.json` | Step Functions `carddemo-daily-cycle` (JSONata) |
| `aws/state-machine/transaction-report.asl.json` | Step Functions `carddemo-report` (on-demand `TRANREPT`) |
| `Dockerfile`, `run-local.sh` | Image; local runner (PostgreSQL in Docker + local directory as bucket) |
| `golden/` | GnuCOBOL harness that produced the golden files in `src/test/resources/golden/` |
| `src/test/resources/db/schema.sql` | Test/local schema derived from `data-model.md` (the authoritative Flyway schema is `aws/db/`) |

## Build and test

```bash
cd aws/batch
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify   # unit tests + Testcontainers ITs (needs Docker)
docker build -t carddemo-batch .                             # after mvn package/verify
```

* Unit tests (Surefire): overpunch parsing/formatting, COBOL truncation, edited pictures, `aws/` artifact lint
  (every `Next`/`Catch` target exists, every Batch task has a job definition, job definitions carry exactly the
  `conventions.md` env vars, no `CLOSEFIL`/`OPENFIL`).
* Integration tests (Failsafe, `*IT`): PostgreSQL 16 Testcontainer + `schema.sql`, reference data loaded from
  `app/data/ASCII/*.txt` through `load-reference-data`, fixed clock `2022-07-18T10:00Z`.
  * `GoldenPosttranIntcalcIT`: POSTTRAN and INTCALC against the GnuCOBOL goldens; POSTTRAN restart/idempotency;
    INTCALC rerun no-op; RC 8 on missing input.
  * `DownstreamJobsIT`: backup → interest → combine chain, statements (text/HTML/PDF), transaction report,
    weekly type maintenance/extract/refresh, reference refresh rollback (RC 12), parameter errors (RC 8).

## Local runner

```bash
./run-local.sh up                                   # postgres:16-alpine on :55432, schema, seed tables from app/data/ASCII
./run-local.sh job post-daily-transactions --businessDate=2022-07-18
MONTH_START=true SATURDAY=true ./run-local.sh daily-cycle 2022-07-18   # whole cycle, all branches
./run-local.sh psql
./run-local.sh down
```

Outputs go to `target/local-bucket/` with the same key layout as S3 (`runs/<runId>/<job>.json`,
`output/dalyrejs/…`, `statements/…`, …).

## Runtime contract

* **Environment** (`conventions.md`): `DB_HOST`, `DB_PORT`, `DB_NAME`, `DB_USER`, `DB_PASSWORD`, `DB_SCHEMA`,
  `S3_BUCKET`, `AWS_REGION`. Job definitions take `DB_USER`/`DB_PASSWORD` from Secrets Manager.
* **Parameters**: `--job` (required), `--runId` (default: generated; `[A-Za-z0-9_-]{1,40}`), `--businessDate`
  (default: today UTC), job-specific ones below.
* **Return codes** (`batch.md` §1.1): the logical code 0/4/8/12/16 is written to `runs/<runId>/<job>.json` and
  `batch_job_run.exit_code`; the process exits `0` for 0 and 4 (Batch job `SUCCEEDED`) and with the code itself
  otherwise (Batch job `FAILED`). The state machine reads the JSON to tell 0 from 4.
* **Idempotency**: `(runId, job)` that already completed is a no-op returning the recorded code. POSTTRAN
  restarts with the same `runId` continue at the first `daily_transaction` row with `post_status IS NULL`.
  INTCALC records `{parmDate, lastAcctId}` per account in `batch_job_run.counts` and any later run for the same PARM
  date skips those accounts. `(runId, job)` is held by a PostgreSQL session advisory lock for the whole run: a concurrent
  launch with the same key exits 16 without running, and a Batch retry after a dead attempt (whose session, and so
  its lock, is gone) resumes at once. INTCALC also serializes runs per PARM date the same way. One `runId` per
  job per table (the Step Functions execution uses one `runId` for the whole cycle; each job name appears once per
  table).
* **Generations**: GDG `(0)` = lexicographically last `<runId>` under the prefix (`batch.md` §1.2). Generated run ids
  (JobRunner default and the state machines' default) are `yyyyMMdd'T'HHmmss'Z'-<8 hex>`; explicit ids passed to
  jobs that feed a later `(0)` lookup must sort the same way, or pass the source key explicitly.
* **Logging**: ECS JSON to stdout (`/aws/batch/carddemo`) with `runId`, `jobName`, `businessDate` in MDC.

| Job | Parameters | Reads | Writes | RC 4 when |
|---|---|---|---|---|
| `post-daily-transactions` | `inputKey` (default `input/dalytran/<date>/dalytran.txt`) | S3 daily file, `card_xref`, `account`, `tran_cat_balance` | `daily_transaction`, `transaction`, `account`, `tran_cat_balance`, `output/dalyrejs/<date>/<runId>.txt` | rejects exist |
| `calculate-interest` | `parmDate` (10 chars, default `yyyyMMdd00` of businessDate) | `tran_cat_balance`, `account`, `card_xref`, `disclosure_group` | `transaction`, `account`, `output/systran/<date>/<runId>.txt` | — |
| `combine-transactions` | `systranKey`, `backupKey` (default latest) | systran file, backup, `transaction` | — (verification: every systran and backed-up `tran_id` present; RC 8 if an input is missing, RC 12 on mismatch) | — |
| `create-statements` | — | `transaction`, `card_xref`, `customer`, `account` | `statements/<date>/<runId>/statement.{txt,html}` | cards skipped (missing customer/account) |
| `statement-pdf` | `statementRunId` (default this `runId`) | `statement.txt` | `statement.pdf` | — |
| `backup-transactions` | — | `transaction` | `backup/transaction/<date>/<runId>.csv.gz` | — |
| `transaction-report` | `startDate`+`endDate`, or `dateParm="yyyy-MM-dd yyyy-MM-dd"` | `transaction`, `card_xref`, `transaction_type`, `transaction_category` | `reports/tranrept/<date>/<runId>.txt` | — |
| `maintain-transaction-types` | `inputKey` (default `input/trantype-maint/<date>/maint.txt`) | `A`/`U`/`D`/`*` 53-byte records (`COBTUPDT` input) | `transaction_type` | any record failed |
| `extract-transaction-types` | — | `transaction_type`, `transaction_category` | `refdata/transaction_type/<runId>.txt`, `refdata/transaction_category/<runId>.txt` | — |
| `load-reference-data` | `table`, `sourceKey` (default latest `refdata/<table>/`; `seed/ascii/<file>` only if the table is empty, otherwise skipped), `deleteMissing` | S3 fixed-width file | `<table>` (keyed upsert, one DB transaction; RC 12 if a delete would orphan rows) | — |
| `backup-reference-data` | `table` | `<table>` | `backup/<table>/<date>/<runId>.csv.gz` | — |

## JCL step → Java / ASL mapping

| Legacy JCL (step) | Program / utility | Java (`com.carddemo.batch…`) | Batch job definition | ASL state (`daily-cycle` unless noted) |
|---|---|---|---|---|
| `CLOSEFIL` (CLCIFIL) | SDSF `/F CICS,CEMT SET FIL CLO` | — not needed: Aurora is shared by online and batch with row locking | — | — (dropped) |
| `CBPAUP0J` (STEP01) | `DFSRRC00` BMP `CBPAUP0C` | — replatform candidate (IMS) | `carddemo-purge-expired-authorizations` (auth module) | `AuthorizationModuleInstalled` → `PurgeExpiredAuthorizations` (off unless `authModuleInstalled=true`) |
| `POSTTRAN` (STEP15) | `CBTRN02C` | `posttran.PostDailyTransactionsJob` | `carddemo-post-daily-transactions` | `PostDailyTransactions` → `…Result` → `…Check` (4 → `…Warning` SNS, continue) |
| `WAITSTEP` (WAIT) | `COBSWAIT` / `MVSWAIT`, PARM centiseconds | — | — | `WaitStep` (`Wait`, `waitCentiseconds/100`, default 36 s) |
| `OPENFIL` (OPCIFIL) | SDSF `CEMT SET FIL OPE` | — not needed (see `CLOSEFIL`) | — | — (dropped) |
| `TRANBKP` (STEP05R) | `REPROC` (IDCAMS REPRO) `TRANSACT` → `TRANSACT.BKUP(+1)` | `tranbkp.BackupTransactionsJob` | `carddemo-backup-transactions` | `BackupTransactions` |
| `TRANBKP` (STEP05, STEP10) | IDCAMS DELETE/DEFINE `TRANSACT` KSDS + AIX | — not needed (table persists; indexes in schema) | — | — |
| `INTCALC` (STEP15) | `CBACT04C PARM='2022071800'` | `intcalc.CalculateInterestJob` (`--parmDate`) | `carddemo-calculate-interest` | `IsMonthStart` → `CalculateInterest` (`parmDate = yyyyMMdd00`) |
| `COMBTRAN` (STEP05R) | SORT `TRANSACT.BKUP(0)` + `SYSTRAN(0)` by `TRAN-ID` | `combtran.CombineTransactionsJob` (verification; system transactions are inserted directly by INTCALC) | `carddemo-combine-transactions` | `CombineTransactions` |
| `COMBTRAN` (STEP10) | IDCAMS REPRO → `TRANSACT` | same (no reload needed) | — | — |
| `CREASTMT` (DELDEF01, STEP010, STEP020, STEP030) | IDCAMS define `TRXFL`, SORT by card/tran-id, REPRO, IEFBR14 delete old outputs | ordered SQL in `creastmt.CreateStatementsJob`; outputs are new immutable S3 objects | — | — |
| `CREASTMT` (STEP040) | `CBSTM03A` + `CBSTM03B` | `creastmt.CreateStatementsJob`, `StatementWriter` | `carddemo-create-statements` | `CreateStatements` |
| `TXT2PDF1` (TXT2PDF) | `IKJEFT1B` REXX `TXT2PDF` | `creastmt.StatementPdfJob` (PDFBox) | `carddemo-statement-pdf` | `StatementPdf` |
| `MNTTRDB2` (STEP1) | `IKJEFT01` → `COBTUPDT` (DB2) | `tranextr.MaintainTransactionTypesJob` | `carddemo-maintain-transaction-types` | `IsSaturday` → `MaintainTransactionTypes` (only if `runTransactionTypeMaintenance=true`) |
| `TRANEXTR` (STEP10, STEP20) | IEBGENER backup of previous extracts to GDG | implicit: every extract is a new object `refdata/<table>/<runId>.txt` | — | — |
| `TRANEXTR` (STEP30) | IEFBR14 delete previous extracts | — not needed | — | — |
| `TRANEXTR` (STEP40, STEP50) | `DSNTIAUL` unload `TRANSACTION_TYPE` / `TRANSACTION_TYPE_CATEGORY` | `tranextr.ExtractTransactionTypesJob` | `carddemo-extract-transaction-types` | `ExtractTransactionTypes` |
| `TRANTYPE`, `TRANCATG` (STEP05/10/15) | IDCAMS DELETE/DEFINE/REPRO from extract | `refdata.LoadReferenceDataJob --table=transaction_type\|transaction_category` (source = latest extract) | `carddemo-load-reference-data` | — (DB2 and VSAM tables are one Aurora table; run on demand) |
| `DISCGRP` (STEP05) | IDCAMS DELETE `DISCGRP` | `refdata.BackupReferenceDataJob --table=disclosure_group` (keep the previous version) | `carddemo-backup-reference-data` | `BackupDisclosureGroups` |
| `DISCGRP` (STEP10, STEP15) | IDCAMS DEFINE + REPRO | `refdata.LoadReferenceDataJob --table=disclosure_group` | `carddemo-load-reference-data` | `RefreshDisclosureGroups` |
| `TCATBALF`, `ACCTFILE`, `CARDFILE`, `CUSTFILE`, `XREFFILE`, `TRANFILE` | IDCAMS DELETE/DEFINE/REPRO (+ AIX BLDINDEX) | `refdata.LoadReferenceDataJob --table=<t>` | `carddemo-load-reference-data` | — (initial load / on demand) |
| `TRANREPT` (STEP05R, STEP05R SORT) | REPROC backup + SORT by card, filter on `TRAN-PROC-TS` date range | SQL `ORDER BY card_num, tran_id` `WHERE proc_ts::date BETWEEN` | — | — |
| `TRANREPT` (STEP10R) | `CBTRN03C` (`DATEPARM`) | `tranrept.TransactionReportJob`, `ReportFormatter` | `carddemo-transaction-report` | `transaction-report.asl.json`: `TransactionReport` |
| `DALYREJS`, `DEFGDGB`, `DEFGDGD`, `REPTFILE`, `TRANIDX`, `ESDSRRDS`, `DEFCUST` | IDCAMS define GDG bases / clusters / AIX | — not needed (S3 prefixes, schema) | — | — |
| `INTRDRJ1`, `INTRDRJ2`, `FTPJCL`, `CBADMCDJ` | internal reader, FTP, DFHCSDUP | — not needed (Step Functions chaining, S3, no CSD) | — | — |

## Daily cycle state machine

`aws/state-machine/daily-cycle.asl.json` follows `batch.md` §3 (CA-7 SCHID 030 + Control-M DAILY/MONTHLY/WEEKLY
folders) as one execution per business day:

```
Init/Calendar ─▶ [PurgeExpiredAuthorizations] ─▶ PostDailyTransactions ─▶ WaitStep ─▶ BackupTransactions
   ─▶ IsMonthStart? ─▶ CalculateInterest ─▶ CombineTransactions ─▶ CreateStatements ─▶ StatementPdf
   ─▶ IsSaturday?   ─▶ [MaintainTransactionTypes] ─▶ ExtractTransactionTypes ─▶ BackupDisclosureGroups ─▶ RefreshDisclosureGroups
   ─▶ DailyCycleComplete
any failure ─▶ NotifyFailure (SNS) ─▶ Failed
```

* Input (all optional): `runId` (default: execution name), `businessDate` (default: today UTC), `monthStart`
  (default: day = `01`), `saturday` (default: ISO weekday 6), `authModuleInstalled` (default `false`),
  `runTransactionTypeMaintenance` (default `false`), `waitCentiseconds` (default `3600`).
* Every job = `batch:submitJob.sync` (retries on Batch API errors with backoff; container retries for
  image-pull/start failures and exit 16 in the job definition's `retryStrategy`) → `s3:getObject`
  `runs/<runId>/<job>.json` → `Choice` on `returnCode`: `0` continue, `4` publish to the warning topic and
  continue (JCL `COND=(4,LT)` semantics), anything else / a `FAILED` Batch job (8/12/16) → `NotifyFailure` → `Fail`.
  INTCALC → COMBTRAN has no RC-4 branch (both only return 0 or fail; JCL `COND=(0,NE)`).
* Definition substitutions: `${JobQueueArn}`, `${BucketName}`, `${WarningTopicArn}` (`carddemo-batch-warning`),
  `${FailureTopicArn}`. The state machine role needs `batch:SubmitJob/DescribeJobs/TerminateJob`,
  `events:PutTargets/PutRule/DescribeRule` (for `.sync`), `s3:GetObject` on `runs/*`, `sns:Publish`.
* Schedule: EventBridge Scheduler, daily 02:00 UTC, input `{}`.

## Deployment sketch

```bash
export IMAGE_URI=<acct>.dkr.ecr.<region>.amazonaws.com/carddemo-batch:<tag> JOB_ROLE_ARN=… EXECUTION_ROLE_ARN=… \
       DB_HOST=… DB_PORT=5432 DB_NAME=carddemo DB_SECRET_ARN=… S3_BUCKET=… AWS_REGION=…
for f in aws/job-definitions/*.json; do
  envsubst < "$f" > /tmp/jd.json && aws batch register-job-definition --cli-input-json file:///tmp/jd.json
done
sed -e "s|\${JobQueueArn}|$JOB_QUEUE_ARN|g" -e "s|\${BucketName}|$S3_BUCKET|g" \
    -e "s|\${WarningTopicArn}|$WARN_TOPIC_ARN|g" -e "s|\${FailureTopicArn}|$FAIL_TOPIC_ARN|g" \
    aws/state-machine/daily-cycle.asl.json > /tmp/daily-cycle.json
aws stepfunctions create-state-machine --name carddemo-daily-cycle --role-arn "$SFN_ROLE_ARN" \
    --definition file:///tmp/daily-cycle.json
```

(IaC for the compute environment, job queue, roles, topics and schedule belongs to the infra session.)

ASL validation (done for this PR, `result: OK`, no diagnostics at `WARNING` severity):

```bash
aws stepfunctions validate-state-machine-definition --type STANDARD --severity WARNING \
    --definition file://aws/state-machine/daily-cycle.asl.json
```

## Golden tests (POSTTRAN, INTCALC)

Expected values are **not hand-derived**: `golden/generate-golden.sh` compiles the unmodified
`app/cbl/CBTRN02C.cbl` and `app/cbl/CBACT04C.cbl` with GnuCOBOL 3.1.2 (`-fsign=EBCDIC` so overpunch matches the
ASCII samples), builds the indexed input files from `app/data/ASCII/{acctdata,cardxref,tcatbal,discgrp,dailytran}.txt`
with generated load/unload utilities (`golden/gen-idxutil.sh`), runs the programs (`golden/RUNINTC.cbl` passes the
`INTCALC.jcl` PARM `2022071800` as z/OS does) and unloads the resulting files:

| Program | Legacy result | Golden files |
|---|---|---|
| `CBTRN02C` | RC 4, `TRANSACTIONS PROCESSED :000000300`, `TRANSACTIONS REJECTED :000000038` | `posttran/{returncode,counts,acctdata,tcatbal,transact,dalyrejs}.txt` |
| `CBACT04C` (on the POSTTRAN output) | RC 0, 50 system transactions | `intcalc/{returncode,acctdata,systran}.txt` |

The ITs compare the Java results record-by-record after rendering Aurora rows back into the copybook layouts:
rejects (350-byte record + reason trailer), accounts, category balances (first 28 bytes; the 22-byte `FILLER`
is not stored), transactions and system transactions (timestamps masked, since COBOL uses the wall clock).
Regenerate with `golden/generate-golden.sh` (needs `cobc`).

## Deviations, omissions, incomplete modules

| Item | Decision |
|---|---|
| `CLOSEFIL` / `OPENFIL` | Dropped. They closed CICS VSAM files so batch had exclusive access; Aurora serves online and batch concurrently (row locks: `SELECT … FOR UPDATE` per posted record / account). |
| `CBACT04C` `1400-COMPUTE-FEES` | **Incomplete in the COBOL source** ("To be implemented"): not implemented. |
| `CBPAUP0J` / `CBPAUP0C` (purge expired authorizations) | **Replatform candidate** (IMS, `migration-inventory.md` §9). The ASL has a disabled branch for it (`authModuleInstalled`); no job definition in this module. |
| `TXT2PDF` REXX | Replaced by PDFBox (Courier, one page per statement); byte-level PDF parity not attempted (REXX source not in repo). |
| `COMBTRAN` | Verification instead of SORT + reload: INTCALC already inserts system transactions into `transaction` (the systran file is still written for audit/parity). |
| `CBTRN03C` end of file | COBOL re-adds the last amount to the page total and never prints the last account total; Java prints the last account total without double counting. |
| `CBSTM03A` in-memory limit | Legacy table of 51 cards × 10 transactions per card is not reproduced (no truncation). |
| Timestamps | `transaction.proc_ts` = job clock (UTC) like COBOL `CURRENT-DATE`; goldens mask timestamps. |
| Not implemented in this module (on demand, outside the daily cycle) | `READACCT`/`READCARD`/`READCUST`/`READXREF` (`CBACT01C`–`03C`, `CBCUS01C` prints), `CBEXPORT`/`CBIMPORT`, `PRTCATBL` category-balance report. Listed in `migration-inventory.md` §9. |

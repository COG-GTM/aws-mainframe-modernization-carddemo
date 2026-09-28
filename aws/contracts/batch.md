# Contract: Batch (JCL / CA-7 / Control-M → AWS Batch + Step Functions)

Status: **v1 (Discovery session)**. Producer: batch session (`aws/batch/`), infra session (job definitions,
state machines). Tables: `data-model.md`. Queues: `messaging.md`. Conventions: `conventions.md`.

## 1. Runtime model

* One Maven module `aws/batch` → one container image `carddemo-batch`. Each legacy program = one Spring Batch
  `Job` bean, selected by `--job=<name>` (AWS Batch job definition `carddemo-<job-name>`, command
  `java -jar carddemo-batch.jar --job=<job-name> [params]`).
* JCL chains / scheduler flows = Step Functions state machines (`carddemo-<flow>`), triggered by EventBridge
  Scheduler (cron) or on demand (`StartExecution` from online services).
* Every job reads/writes Aurora (`carddemo` schema) and/or `s3://${S3_BUCKET}/…`. No local files survive a job.
* Business date parameter `businessDate` (`yyyy-MM-dd`) is passed to every job by the state machine and
  defaults to the current UTC date (legacy used `FUNCTION CURRENT-DATE` or a JCL `PARM`).

### 1.1 Return codes

The job computes a legacy `RETURN-CODE` (`returnCode`) as below. The **container process** exits 0 for
`returnCode` 0 and 4 (AWS Batch job `SUCCEEDED`) and exits with `returnCode` for 8/12/16 (`FAILED`). Every job
writes `s3://${S3_BUCKET}/runs/<runId>/<job-name>.json` = `{"returnCode": n, "counts": {…}}` and
`batch_job_run.exit_code` = `returnCode`; after each `batch:submitJob.sync` state the state machine reads that
object (`s3:getObject` SDK integration) to branch on `returnCode`.

| `returnCode` | Meaning | Legacy source | Step Functions handling |
|---|---|---|---|
| 0 | success | normal `GOBACK` | continue |
| 4 | success with warnings (rejects written) | `CBTRN02C` `MOVE 4 TO RETURN-CODE` when reject count > 0; `COBTUPDT` on SQL error | continue; publish `carddemo-batch-warning` SNS/metric |
| 8 | input/parameter error | new (legacy JCL `COND=(0,NE)` / `(4,LT)` stop chains) | fail flow |
| 12 | data/IO error, transaction rolled back | legacy `CEE3ABD` abend code 999 (all `CB*` programs) | fail flow, retry 0 |
| 16 | fatal (IMS/DB2 unavailable) | `CBPAUP0C`, `DBUNLDGS`, `PAUDBLOD`, `PAUDBUNL` `MOVE 16 TO RETURN-CODE` | fail flow |

Steps that in JCL run with `COND=(0,NE)` run in Step Functions only if the previous job's `returnCode` = 0;
steps with `COND=(4,LT)` (e.g. `TRANBKP` STEP10) run if the previous `returnCode` ≤ 4. A `FAILED` Batch job
(8/12/16) always fails the flow.

### 1.2 S3 key layout

```
s3://<bucket>/seed/ascii/<file>.txt                         # app/data/ASCII copies (data-migration)
s3://<bucket>/seed/ebcdic/<file>                            # app/data/EBCDIC copies (data-migration)
s3://<bucket>/input/dalytran/<businessDate>/dalytran.txt    # replaces AWS.M2.CARDDEMO.DALYTRAN.PS
s3://<bucket>/output/dalyrejs/<businessDate>/<runId>.txt    # replaces DALYREJS(+1) GDG
s3://<bucket>/backup/<table>/<businessDate>/<runId>.csv.gz  # replaces *.BKUP(+1) GDGs
s3://<bucket>/output/systran/<businessDate>/<runId>.txt     # replaces SYSTRAN(+1) (audit copy)
s3://<bucket>/reports/tranrept/<businessDate>/<runId>.txt   # replaces TRANREPT(+1)
s3://<bucket>/reports/tcatbal/<businessDate>/<runId>.txt    # replaces TCATBALF.REPT
s3://<bucket>/statements/<businessDate>/<runId>/statement.txt|statement.html|statement.pdf
s3://<bucket>/export/<businessDate>/<runId>/export.dat      # replaces AWS.M2.CARDDEMO.EXPORT.DATA
s3://<bucket>/import/<runId>/{customer,account,xref,transaction,card}.dat, errors.txt
s3://<bucket>/extract/account/<runId>/{fixed.txt,array.txt,variable.txt}  # READACCT outputs
s3://<bucket>/refdata/<table>/<runId>.txt                   # TRANEXTR / reference refresh files
```

GDG `(+1)` = new `<runId>` prefix; `(0)` = latest `<runId>` under the prefix (lexicographic, `runId` =
`yyyyMMdd'T'HHmmss'Z'-<8 hex>`). Retention: S3 lifecycle, default 90 days (legacy GDG bases in `DEFGDGB.jcl`/`DEFGDGD.jcl`/`REPTFILE.jcl`/
`DALYREJS.jcl` use `LIMIT(5)`, one uses `LIMIT(10)`; batch MAY also prune to the same generation count).

### 1.3 Record formats in S3

Fixed-width files keep the legacy copybook layout (ASCII, zoned decimal as display digits with overpunched
sign as in `app/data/ASCII`, one record per `\n`). Parsers live in `aws/data-migration` and are reused by
batch. Formats: daily transaction = `CVTRA06Y` 350 bytes; reject = 350-byte transaction + 80-byte trailer
(`FD-REJECT-RECORD` + `FD-VALIDATION-TRAILER`: `reasonCode 9(04)` + `description X(76)`); report lines = 133
bytes (`CBTRN03C` `FD-REPTFILE-REC`); statement text 80 bytes, HTML 100 bytes (`CBSTM03A`); export = 500-byte
`CVEXPORT`; import errors = 132 bytes (`CBIMPORT`).

## 2. Job catalog

Legend — **In**/**Out**: `T:` Aurora table, `S3:` key prefix from §1.2.

### 2.1 Core daily/periodic jobs

| Job name | Legacy (JCL → program) | In | Out | Params | Notes / exit |
|---|---|---|---|---|---|
| `post-daily-transactions` | `POSTTRAN.jcl` → `CBTRN02C` | S3:`input/dalytran/<d>/`, T:`card_xref`, `account`, `tran_cat_balance` | T:`transaction` (insert), `account` (curr_bal, curr_cyc_credit/debit), `tran_cat_balance` (upsert), S3:`output/dalyrejs/` | `businessDate` | Validation: 100 invalid card (xref NOTFND), 101 account not found, 102 over limit (`curr_cyc_credit - curr_cyc_debit + amt > credit_limit`), 103 after `expiration_date`, 109 rewrite failure. Rejects → file; exit 4 if any reject. `proc_ts` = job timestamp. One DB transaction per input record (legacy: no commit scope; VSAM writes immediate). Also inserts `daily_transaction` staging rows (see §2.4). |
| `calculate-interest` | `INTCALC.jcl` → `CBACT04C` `PARM='2022071800'` | T:`tran_cat_balance`, `card_xref` (by acct), `account`, `disclosure_group` | T:`transaction` (interest rows), `account` (curr_bal += interest; cyc credit/debit reset to 0), S3:`output/systran/` | `businessDate` (legacy PARM = `yyyyMMddNN`; first 10 chars used as `tran_id` prefix) | Monthly interest = `tran_cat_bal * int_rate / 1200` per category; rate from `disclosure_group` (`acct_group_id`,`type_cd`,`cat_cd`), fallback group `DEFAULT`. Interest `transaction`: `tran_id` = `<parm 10 chars><6-digit seq>`, type `01`, cat `05`, source `System`, description `Int. for a/c <acctId>`. **Fees (`1400-COMPUTE-FEES`) are "To be implemented" in source → not implemented; record as incomplete.** |
| `combine-transactions` | `COMBTRAN.jcl` (SORT + IDCAMS REPRO) | S3:`backup/transaction/(0)`, S3:`output/systran/(0)` | T:`transaction` | — | On AWS the interest rows are inserted directly by `calculate-interest`, so this job is a **no-op verification step** (asserts row counts); kept so the Control-M flow shape is preserved. |
| `transaction-report` | `TRANREPT.jcl` / `TRANREPT.prc` (REPROC unload → SORT include by `TRAN-PROC-DT` → `CBTRN03C`) | T:`transaction` (`proc_ts::date BETWEEN startDate AND endDate`, ordered by `card_num`), `card_xref`, `transaction_type`, `transaction_category` | S3:`reports/tranrept/<d>/` (133-col text: detail, account totals, page totals, grand total) | `startDate`, `endDate` (legacy `DATEPARM` 80-byte record `yyyy-mm-dd yyyy-mm-dd`) | Triggered by SQS `carddemo-report-request` (from `POST /reports/transactions`) via the `carddemo-report` state machine, or scheduled. Exit 0. |
| `create-statements` | `CREASTMT.JCL` (IDCAMS define `TRXFL` + SORT by card/tran + REPRO + IEFBR14 + `CBSTM03A` → `CBSTM03B`) | T:`card_xref`, `customer`, `account`, `transaction` (ordered by `card_num`, `tran_id`) | S3:`statements/<d>/<runId>/statement.txt` + `.html` | `businessDate` | `CBSTM03B` file-access subroutine (ops `O`/`C`/`R`/`K`) becomes a repository interface; no separate job. |
| `statement-pdf` | `TXT2PDF1.JCL` (`IKJEFT1B` REXX `TXT2PDF`) | S3 `statement.txt` | S3 `statement.pdf` | `runId` | Implemented with a Java PDF library (e.g. OpenPDF/PDFBox) — `TXT2PDF` REXX is not ported. |
| `backup-transactions` | `TRANBKP.jcl` (`REPROC` unload → IDCAMS delete/define `TRANSACT` + AIX) | T:`transaction` | S3:`backup/transaction/<d>/` | `businessDate` | The delete/redefine of the VSAM cluster is **not needed on AWS** (table persists). |
| `category-balance-report` | `PRTCATBL.jcl` (IEFBR14, `REPROC`, SORT by acct/type/cat + OUTREC) | T:`tran_cat_balance` | S3:`backup/tran_cat_balance/<d>/`, S3:`reports/tcatbal/<d>/` | `businessDate` | — |
| `export-customer-data` | `CBEXPORT.jcl` → `CBEXPORT` | T:`customer`, `account`, `card_xref`, `transaction`, `card` | S3:`export/<d>/<runId>/export.dat` (500-byte `CVEXPORT`, types `C`,`A`,`X`,`T`,`D`) | `branchId` (optional), `businessDate` | IDCAMS define of `EXPORT.DATA` not needed. |
| `import-customer-data` | `CBIMPORT.jcl` → `CBIMPORT` | S3:`export/…/export.dat` | S3:`import/<runId>/{customer,account,xref,transaction,card}.dat` (legacy DDs `CUSTOUT`, `ACCTOUT`, `XREFOUT`, `TRNXOUT`, `CARDOUT`) + `errors.txt`; optional `--load=true` upserts into T:`customer`,`account`,`card`,`card_xref`,`transaction` | `exportKey`, `load` | Legacy writes only normalized sequential files; table load is an opt-in extension. Load order (one DB transaction): `customer` → `account` → `card` → `card_xref` → `transaction`, so FKs hold. |
| `extract-accounts` | `READACCT.jcl` → `CBACT01C` (+ `COBDATFT`) | T:`account` | S3:`extract/account/<runId>/` fixed (`OUT-ACCT-REC`), array (`OCCURS 5`), variable (10–80 bytes) | — | Demonstration job. `COBDATFT` date reformat → `java.time`. |
| `print-cards` / `print-xref` / `print-customers` | `READCARD`/`READXREF`/`READCUST.jcl` → `CBACT02C`/`CBACT03C`/`CBCUS01C` | T:`card` / `card_xref` / `customer` | CloudWatch Logs (legacy `DISPLAY` to SYSOUT) | — | Demonstration jobs. |
| `validate-daily-transactions` | none (no JCL references `CBTRN01C`) | S3 dalytran, T:`customer`,`card_xref`,`card`,`account`,`transaction` | CloudWatch Logs | `businessDate` | `CBTRN01C` only reads/validates and `DISPLAY`s; optional pre-check state. |
| `wait` | `WAITSTEP.jcl` → `COBSWAIT` → `MVSWAIT` | — | — | `centiseconds` (8 digits, e.g. `00003600` = 36 s) | **Not a Batch job**: Step Functions `Wait` state (`Seconds = centiseconds/100`). |

### 2.2 Reference data refresh / file (re)definition jobs

| Job name | Legacy | Target |
|---|---|---|
| `load-reference-data` `--table=<t>` | `TRANTYPE.jcl`, `TRANCATG.jcl`, `DISCGRP.jcl`, `TCATBALF.jcl`, `ACCTFILE.jcl`, `CARDFILE.jcl`, `CUSTFILE.jcl`, `XREFFILE.jcl`, `TRANFILE.jcl`, `DUSRSECJ.jcl` (IDCAMS DELETE/DEFINE/REPRO [+ AIX BLDINDEX]) | Load table `<t>` from S3 `seed/ascii/` or `refdata/<t>/` inside one DB transaction. Initial load (empty schema) in FK order: `customer` → `account` → `card` → `card_xref` → `transaction_type` → `transaction_category` → `disclosure_group` → `tran_cat_balance` → `user_security` → `transaction`. Refresh of a populated table = keyed upsert (`INSERT … ON CONFLICT (pk) DO UPDATE`); rows absent from the source are deleted only if no FK row references them, otherwise the job exits 12 listing the keys. Never `TRUNCATE … CASCADE`. AIX define/BLDINDEX → indexes created by Flyway, **not needed on AWS**. |
| `backup-reference-data` | `DEFGDGD.jcl` (IEBGENER first generation of TRANTYPE/TRANCATG/DISCGRP), `TRANEXTR.jcl` STEP10/20 | Dump table to S3 `backup/<table>/`. |

`TRANIDX.jcl`, `DEFGDGB.jcl`, `REPTFILE.jcl`, `DALYREJS.jcl`, `DEFCUST.jcl`, `ESDSRRDS.jcl` are dataset/GDG
definitions → Flyway migrations + S3 prefixes (no job). `CLOSEFIL.jcl`/`OPENFIL.jcl` (SDSF `CEMT SET FILE
CLOSE/OPEN`) → **not needed on AWS**: Aurora supports concurrent online + batch access; batch jobs use
row-level transactions. `FTPJCL.JCL` → S3 is the exchange point (not needed). `INTRDRJ1/2.JCL` (internal
reader) → Step Functions task chaining (not needed). `CBADMCDJ.jcl` (DFHCSDUP) → not needed.

### 2.3 Optional sub-app jobs

| Job name | Legacy | In / Out | Status |
|---|---|---|---|
| `purge-expired-authorizations` | `CBPAUP0J.jcl` → `DFSRRC00 BMP CBPAUP0C PSBPAUTB` | T:`pending_auth_summary`/`pending_auth_detail` delete where expired (`expiryDays` param, legacy `SYSIN` P-EXPIRY-DAYS), checkpoint every N (`P-CHKP-FREQ`) → commit interval | **Replatform candidate** (depends on IMS refactor) |
| `unload-auth-db` / `load-auth-db` / `unload-auth-gsam` | `UNLDPADB.JCL`→`PAUDBUNL`, `LOADPADB.JCL`→`PAUDBLOD`, `UNLDGSAM.JCL`→`DBUNLDGS`, `DBPAUTP0.jcl` (`DFSURGU0`) | IMS ↔ sequential/GSAM | **Not needed on AWS** if refactored (replaced by `pg_dump`/S3 backup); one-time data migration from the unload files is a data-migration task. |
| `maintain-transaction-types` | `MNTTRDB2.jcl` → `COBTUPDT` (IKJEFT01/DSN RUN) | S3 input file (col 1 `A`/`U`/`D`/`*`, cols 2–3 type, 4–53 description) → T:`transaction_type` | exit 4 on SQL error |
| `extract-transaction-types` | `TRANEXTR.jcl` (IEBGENER backup + DSNTIAUL unload of `TRANSACTION_TYPE` / `TRANSACTION_TYPE_CATEGORY`) | T:`transaction_type`, `transaction_category` → S3 `refdata/transaction_type/`, `refdata/transaction_category/` | Since the core VSAM and DB2 type tables merge into one Aurora table (`data-model.md`), the extract is only a backup. |
| — | `CREADB21.jcl` (DB2 create/load/bind) | Flyway migrations | not needed |

### 2.4 `daily_transaction`

`post-daily-transactions` first stages the S3 daily file into `daily_transaction` (load only if no rows exist yet
for that `runId` — a restart never re-stages; batch id = `runId`, PK (`run_id`,`load_seq`), no de-duplication — `data-model.md` §2.7), then posts from the table ordered by `load_seq` (input file order, as the legacy sequential read). This keeps the input queryable for validation.

## 3. Flows (Step Functions) and daily cycle

Derived from `app/scheduler/CardDemo.ca7` and `app/scheduler/CardDemo.controlm`. `CLOSEFIL`/`OPENFIL`
states are dropped (see §2.2); `WAITSTEP` → `Wait` state.

| State machine | Legacy chain | AWS states | Schedule |
|---|---|---|---|
| `carddemo-daily-cycle` | CA-7 SCHID 030: `CLOSEFIL → CBPAUP0J → POSTTRAN → WAITSTEP → OPENFIL` | `[purge-expired-authorizations (if auth module installed)] → post-daily-transactions → Wait` | daily 02:00 UTC (CA-7 listing carries no calendar; time is an AWS decision) |
| `carddemo-daily-backup` | Control-M `DAILY-TransactionBackup`: `CLOSEFIL → TRANBKP → WAITSTEP → OPENFIL` | `backup-transactions → Wait` | daily, after daily-cycle succeeds |
| `carddemo-statements` | CA-7 SCHID 030: `CLOSEFIL → CREASTMT → TXT2PDF1 → WAITSTEP → OPENFIL` | `create-statements → statement-pdf → Wait` | monthly (day 1), after monthly-interest (CA-7 gives no frequency; monthly is an AWS decision matching the monthly statement cycle) |
| `carddemo-monthly-interest` | Control-M `MONTHLY-InterestCalculation`: `CLOSEFIL → INTCALC → COMBTRAN → WAITSTEP → OPENFIL` | `calculate-interest → combine-transactions → Wait` | monthly (day 1), after daily-cycle, before statements |
| `carddemo-weekly-disclosure-refresh` | Control-M `WEEKLY-DisclosureGroupsRefresh`: `CLOSEFIL → DISCGRP → WAITSTEP → OPENFIL` | `backup-reference-data(disclosure_group) → load-reference-data(disclosure_group) → Wait` | Saturdays (Control-M `DAYS="SA"`) |
| `carddemo-weekly-trantype-refresh` | Control-M `WEEKLY-TransactionTypesDBRefresh`: `MNTTRDB2 → TRANEXTR`; CA-7 `TRANTYPE`/`TRANCATG`/`TCATBALF` refresh chains | `maintain-transaction-types (optional) → extract-transaction-types` | Saturdays (Control-M `DAYS="SA"`) |
| `carddemo-report` | `CORPT00C` → TDQ `JOBS` → `TRANREPT` | `transaction-report` (input from SQS message) | on demand |
| `carddemo-extracts` | CA-7 `READACCT`, `READCARD`, `READCUST`, `READXREF` | parallel `extract-accounts`, `print-cards`, `print-customers`, `print-xref` | on demand |
| `carddemo-export` / `carddemo-import` | `CBEXPORT.jcl` / `CBIMPORT.jcl` | single task | on demand |

**Daily cycle order (business day):**
1. `purge-expired-authorizations` (optional module; CA-7 `CBPAUP0J`)
2. `post-daily-transactions` (CA-7 `POSTTRAN`)
3. `backup-transactions` (Control-M `DAILY-TransactionBackup`)
4. Month start only: `calculate-interest` → `combine-transactions` (Control-M `MONTHLY-InterestCalculation`)
   → `create-statements` → `statement-pdf` (CA-7 `CREASTMT → TXT2PDF1`)
5. Saturday only (Control-M `DAYS="SA"`): `maintain-transaction-types` → `extract-transaction-types`, then
   disclosure-group refresh
6. On demand (not scheduled in either scheduler): `transaction-report` (`CR00`), extracts, export/import,
   category-balance report (`PRTCATBL`, CA-7 SCHID 031)

Ordering between separate state machines is enforced by EventBridge rules on the previous machine's
`SUCCEEDED` event, not by clock time.

## 4. Operational contract

* Idempotency: every job takes `runId`; rerunning a successful `runId` is a no-op (job-execution table
  `batch_job_run(run_id, job_name, business_date, status, exit_code, started_at, ended_at, counts JSONB)`
  in schema `carddemo`). `post-daily-transactions` sets `daily_transaction.post_status`/`reject_reason` in the same DB
  transaction as that record's `transaction`/`account`/`tran_cat_balance` changes, and a restart with the same
  `runId` processes only rows with `post_status IS NULL` (in `load_seq` order), so no record is applied twice (legacy restart = rerun from the start after VSAM restore).
* Logging: JSON to stdout → CloudWatch Logs `/aws/batch/carddemo`; include `runId`, `jobName`, counts
  (legacy `DISPLAY 'TRANSACTIONS PROCESSED :'`, `'TRANSACTIONS REJECTED  :'`).
* Resources: default 1 vCPU / 2 GiB, Fargate compute environment; timeout 1 h.

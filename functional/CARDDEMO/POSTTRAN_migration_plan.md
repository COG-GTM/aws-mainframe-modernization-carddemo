# POSTTRAN migration plan

Produced by `!mf_stream_migration_plan` + `!mf_boundary_resolution` (decide mode) on 2026-11-19. Inputs: `CardDemo_target_state.md` (STOP A), `CardDemo_inventory.md` (STOP B), `POSTTRAN_analysis.md`, `POSTTRAN_functional_requirements.md`. Each phase is one stacked PR (base = previous PR), none merged by Devin; STOP E = user sign-off before merge.

## 1. Phases and waves

| Phase | PR base | Content | Exit gate |
|---|---|---|---|
| Docs | `main` | This PR (#213) | User "sigue" |
| Phase 0 – scaffolding | docs PR | `modernization/pom.xml` (Boot 3.3 parent, Java 21), modules `carddemo-common`, `carddemo-app`; `docker-compose.yml` (postgres:16 + app); Flyway baseline `V1__schemas.sql` (schemas of D-010); Spring Batch JobRepository on PostgreSQL; CLI launcher (`--job`, `--dd`, exit codes D-014/D-015); CI `.github/workflows/modernization-verify.yml` (D-008); smoke test | `mvn -B verify` green locally and in CI; `docker compose up` starts and `--job=NOOP` exits 0 |
| Phase 1 – data | Phase 0 | `carddemo-common`: zoned/overpunch codec, fixed-width `RecordLayout` for CVTRA06Y, CVTRA05Y, CVACT01Y, CVACT03Y, CVTRA01Y, ISO-8859-1 + CP037 (D-005); Flyway `V2__posttran_tables.sql` (accounts, card_xref, tran_cat_balances, transactions); JPA entities with `@Version` (D-019); loader/unloader from/to `app/data/ASCII` (D-021, D-020); DALYTRAN reader and DALYREJS writer; golden cases copied from the oracle (R-04, R-07 syn_all/syn_ok, R-08, R-09 + new `acct-missing`, `dup-id`) under `src/test/resources/golden/posttran/` | Round-trip tests: load → unload = source bytes for all four files; codec unit tests; Testcontainers green |
| Wave 1 – CBTRN02C | Phase 1 | Job `POSTTRAN` / step `STEP15`, chunk 1: reader DALYTRAN, processor = `DailyTransactionValidator` (FR-02..05), writer = `DailyTransactionPostingService` (FR-07..10) or reject writer (FR-06); new-instance listener (FR-11); exit codes (FR-12/13/17); SYSOUT (FR-14) | Every golden case: DALYREJS bytes, 4 tables, SYSOUT and exit code equal to oracle; restart test; failure-injection test |
| Sign-off (STOP E) | Wave 1 | Parity report, boundary register → `VERIFIED`, progress + runbook updated | User approval to merge |

**Waves: exactly one.** The stream has one program (inventory §3: STREAM 1, SHARED 0) and no callee to split on.

## 2. Wave 1 detail

| FR | Target element (package `com.carddemo.*`) |
|---|---|
| FR-01, NFR-01 | `transaction.batch.PosttranJobConfig` (job POSTTRAN, step STEP15, chunk 1) |
| FR-02..05 | `transaction.service.DailyTransactionValidator` |
| FR-06 | `transaction.batch.DailyRejectWriter` (DALYREJS, truncate-to-offset restart D-012) |
| FR-07..10 | `transaction.service.DailyTransactionPostingService` (`@Transactional`, order TCATBAL → account → transaction) |
| FR-11 | `transaction.batch.TransactionMasterResetListener` (new instance only, D-024) |
| FR-12, 13, 17 | `common.batch.JclExitCodeMapper`, `common.batch.CliJobLauncher` |
| FR-14 | `common.batch.SysoutWriter` |
| FR-16 | `common.record.*` codecs |

## 3. Test strategy

Golden master per case: run the Java job on Testcontainers PostgreSQL seeded by the loader, unload the four tables with the record layouts, compare with `expected/*.unl` (TRANSACT cols 325-326 masked), DALYREJS byte compare, SYSOUT line compare, exit code vs `RETURN-CODE` (231 ↔ 16). Unit tests per FR id. Two-run and restart tests for FR-11 / NFR-01.

## 4. Boundary decisions (all DECIDED)

Common to all rows: owner = POSTTRAN engagement (alvaro.leal) unless stated; lead-time request = none needed (no external team/credential; recorded as "fired: N/A, nothing to request" on 2026-11-19); production coexistence = none (D-022), COBOL keeps running in production until the client's cutover, the Java job runs in parallel on copies.

| Id | Class | Contract | Chosen option (recommended) | Justification | Target seam | Error / timeout / retry / idempotency | Strangler routing point | Cutover condition | Coexistence verification |
|---|---|---|---|---|---|---|---|---|---|
| B-01 DALYTRAN | DATASET_IN | Fixed 350-byte CVTRA06Y records, ASCII or CP037 | Keep as file passed by `--dd=DALYTRAN` | USER files rule; byte layout unchanged for the producer | `DailyTransactionReader` | Unreadable/short record → FR-13 exit 16; no retry; re-read safe (read-only) | Scheduler step that launches COBOL vs Java with the same file | All golden cases green + parallel run on a production copy equal | Same file fed to oracle and Java; outputs compared |
| B-02 DALYREJS | DATASET_OUT | 430 bytes = 350 + reason 9(4) + desc X(76); new file per run | Keep as file; path `--dd` or default (D-016) | USER files rule; no consumer in `app/`, layout kept for unknown consumers | `DailyRejectWriter` | Write failure → exit 16; existing file → exit 12 (FR-17); restart truncates to committed offset (D-012) | Same scheduler step | Byte-identical DALYREJS on all cases | `cmp` with oracle DALYREJS |
| B-03 TRANSACT | SHARED_DATA | CVTRA05Y row per posted record; key TRAN-ID | Table `transaction.transactions`, owned by the `transaction` context; reset at new instance (D-024) | Q-01 parity; monolith shares the table in-process | `TransactionRepository`, `TransactionMasterResetListener` | Duplicate id → exit 16 (FR-13); insert in the per-record transaction (D-011); reset never on restart | Other jobs/online keep reading VSAM until their own streams migrate; export via unloader if needed | Unload equals oracle TRANSACT.unl | Unload compare per case |
| B-04 ACCTDATA | SHARED_DATA | CVACT01Y; POSTTRAN updates CURR-BAL, CYC-CREDIT, CYC-DEBIT | Table `account.accounts`, `@Version` optimistic lock | REF conventions §7; online programs update the same rows | `AccountRepository` | Missing row → reject 101; version conflict → exit 16, restart reprocesses the record (no double post: transaction rolled back) | As B-03 | Unload equals ACCTDATA.unl | Unload compare per case |
| B-05 CARDXREF | SHARED_DATA | CVACT03Y, read by card number | Table `card.card_xref`, read-only for POSTTRAN | Lookup only | `CardXrefRepository` | Missing → reject 100; DB error → exit 16 | As B-03 | Golden cases green | Loader round-trip test |
| B-06 TCATBALF | SHARED_DATA | CVTRA01Y, key acct+type+cat | Table `reference.tran_cat_balances`, PK composite, `@Version` | Create-or-add per BR-05 | `TranCatBalanceRepository` | Insert/update failure → exit 16; in per-record transaction | As B-03 | Unload equals TCATBALF.unl | Unload compare per case |
| B-07 CEE3ABD | RUNTIME_SERVICE | Abend U999, no return | Uncaught `PosttranAbendException` → step FAILED → exit 16 | D-006, D-015 | `JclExitCodeMapper` | No retry; SYSOUT error line first | N/A | Duplicate-id case exits 16 | Oracle RC 231 ↔ Java 16 mapping test |
| B-08 Scheduler | SCHEDULER | Triggered after CBPAUP0J; RC 0/4 = success, abend = failure | Exit code contract (D-017); client scheduler owner = operations (to be named by client) | No scheduler code in repo | `CliJobLauncher` | Exit 4 is success-with-rejects (as CA-7 chain treats RC 4 today) | Scheduler job definition switches program | Ops dry run of chain with Java job | Parallel run logs |
| B-09 Online window | ONLINE_COEXISTENCE | CICS files closed during batch | Java job runs inside the same batch window; online streams untouched | Out of stream scope | N/A | N/A | N/A | Online streams migrated separately | Window respected in parallel run |

## 5. Risks

| Risk | Mitigation |
|---|---|
| TRAN-PROC-TS centiseconds not pinnable | Mask cols 325-326; clock injected in Java (fixed in tests) |
| Q-01 reset deletes shared transaction history | Parity-required; D-024 recorded revisable; reset only in new instance |
| Sample data EBCDIC overpunch | Codec tests on both encodings (R-09) |

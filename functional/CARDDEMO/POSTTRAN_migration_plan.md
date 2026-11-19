# POSTTRAN migration plan

**Status: APPROVED at STOP C by user, STOP C, 2026-11-19 ("Approve").**

Produced by `!mf_stream_migration_plan` on 2026-11-19 (session devin-c4fce2ffab2c4abaac2aa02e217e00e4), branch `devin/1795090039-mf-posttran-stream`. Inputs: `POSTTRAN_analysis.md`, `POSTTRAN_functional_requirements.md` (17 FRs), `CardDemo_target_state.md` (CONFIRMED), `CardDemo_inventory.md`, D-001..D-010. **Status: awaiting STOP C (blocking).** Every boundary decision and every new decision below is **PROPOSED**; nothing has been launched or requested externally.

## 1. Goal and scope

- Stream POSTTRAN, **BATCH**, one program CBTRN02C, from `app/jcl/POSTTRAN.jcl:23` to `GOBACK` (`app/cbl/CBTRN02C.cbl:234`). Exclusions per D-010.
- Definition of done: FR-01..FR-17 pass on the golden cases (GnuCOBOL parity, D-004 + D-007 mask + D-009 abend rule) and Java tests JT-1..JT-4, in CI.
- Deliverable: Spring Batch job `POSTTRAN` in `modernization/carddemo-app`, launched by `java -jar carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=<file> --output-dir=<dir>`.

## 2. Target-state mapping (profiles applied: CORE §1, BATCH §3, DATA/BOUNDARY §5; ONLINE and SUBTRANSACTION N/A)

| Layer | Convention (target state) | Lands in POSTTRAN as |
|---|---|---|
| Shape | modular monolith (D-001) | packages `com.carddemo.transaction.batch` (job), `.service` (validation, posting), `com.carddemo.account`, `com.carddemo.card`, `com.carddemo.reference` (repositories/entities); shared code in `com.carddemo.common.{cobol,record,batch,error}` |
| Job/step | one Job per JCL member, one step per EXEC, chunk-oriented | `Job POSTTRAN` -> `Step step15Cbtrn02c`: `DailyTransactionReader` -> `DailyTransactionProcessor` (calls `TransactionValidationService`, CBTRN02C 1500) -> `PostingItemWriter` (calls `PostingService`, 2000/2700/2800/2900, and `RejectStore`, 2500); `RejectFileWriter` publishes DALYREJS at job end |
| Unit of work | one record = one DB transaction (D-009) | `chunk(1)` pinned for `step15Cbtrn02c` (D-011); the target-state default of 100 applies to other jobs only |
| Restart state | restart from last committed chunk (D-002, D-011) | everything a restart needs is committed with each 1-record chunk: (1) the reader position (`DailyTransactionReader` is an `ItemStream`; its read count is saved in the step `ExecutionContext`); (2) the counters `posttran.processed` and `posttran.rejected` in the same `ExecutionContext`; (3) each reject's 430-byte image in table `batch.posttran_reject` (`job_instance_id`, `input_seq`, `record`), inserted in the record's transaction. On restart of the same JobInstance the reader skips the committed records and the counters resume from the context. At job end with RC 0/4, `RejectFileWriter` writes DALYREJS from `batch.posttran_reject` for that JobInstance ordered by `input_seq` into a temp file in the generation dir and renames it to `DALYREJS` (D-012); the SYSOUT counts come from the restored counters. Rows are kept for audit (housekeeping is out of scope) |
| Restart | restart from last committed chunk (D-002); rerun guard (D-008) | `RerunGuard` runs only when the step starts for a **new JobInstance** (new `run.id`, no prior `StepExecution`): any input TRAN-ID already in `transaction.transaction` -> RC 16, no writes (FR-17). A **restart of the same failed JobInstance** (`--restart`, which calls `JobOperator.restart` on the last FAILED execution of `POSTTRAN`) skips the guard, because its own committed records are expected in the table, and resumes from the checkpoint (Restart state row). Restarting a COMPLETED instance is refused by Spring Batch; a new run of the same file is a new instance and gets the full scan |
| Return code | max step RC; 4 rejects; 16 abend; 12 usage | `PostingRunListener` sets 4 when rejects > 0; `BatchCli` maps FAILED -> 16, bad args -> 12 |
| Datasets | DALYTRAN/DALYREJS files, KSDS -> tables (D-003) | §4 |
| Logging | observable DISPLAYs reproduced on SYSOUT; PAN masked in logs | `SysoutWriter` writes the counts, the `Creating.` line and error lines verbatim to `<gen-dir>/SYSOUT`; application logs mask card numbers |
| Time | - | `ProcessingTimestampProvider` with injectable `Clock`; format `yyyy-MM-dd-HH.mm.ss.SS0000` (Z-GET-DB2-FORMAT-TIMESTAMP `:692-705`) |
| Money | BigDecimal scale 2, truncate | `WS-TEMP-BAL` reproduced as S9(9)V99 with high-order truncation (D-014) |
| Drift rules | BATCH §3 drift rules 1-6 | reviewed in every PR |

## 3. Boundary decision table (all DECIDED: approved by user, STOP C, 2026-11-19 ("Approve"))

| BND | Class | Proposed decision | Seam | Error / idempotency | Owner | Evidence | Lead-time request (listed, NOT fired) | Routing point / cutover |
|---|---|---|---|---|---|---|---|---|
| 001 LE CEE3ABD | B11 | **Replace**: typed `PostingAbendException` (subclass of `common.error.FileStatusException` where a status exists); step FAILED -> exit 16; SYSOUT gets the COBOL error line + `ABENDING PROGRAM` | `common.error`, `common.batch.BatchCli` | no retry, no skip (drift rule 3) | transaction context | `CBTRN02C.cbl:707-711`; R-06 (abend 999 = RC 231 on GnuCOBOL) | none | n/a (runtime service) |
| 002 DALYTRAN in | B7 | **Keep the byte contract**: read the same 350-byte file; `--dd=DALYTRAN=<file>`, `--dd-encoding=ASCII|CP037` | `common.record` `RecordReader` + `DailyTransactionLayout` (CVTRA06Y) | `00`/EOF/else abend, as COBOL | transaction context; producer: unknown | `POSTTRAN.jcl:30-31`; `CBTRN02C.cbl:345-369`; GC-1, GC-5 | **LR-1**: data owner to name the DALYTRAN producer, encoding (ASCII vs CP037) and delivery time (OQ-5) | cutover when a production DALYTRAN sample passes parity |
| 003 DALYREJS out | B7 | **Keep the byte contract**: 430-byte records in `<output-dir>/POSTTRAN/<yyyyMMdd-HHmmss-SSS>/DALYREJS`; rejects staged per record in `batch.posttran_reject` (so a restart keeps them), assembled into a temp file and published (renamed) only when the job ends RC 0/4 (D-012, mirrors `DISP=(NEW,CATLG,DELETE)`) | `RejectStore`, `RejectFileWriter`, `common.batch.GenerationDirectory` | on abend no DALYREJS is published | transaction context; consumer: unknown | `POSTTRAN.jcl:34-38`; `CBTRN02C.cbl:446-465`; GC-1, GC-2 | **LR-2**: identify DALYREJS consumers and where they read it (OQ-5) | consumers switch to the generation dir at cutover |
| 004 TRANSACT | B10 | **Java job is the single writer of POSTTRAN rows**; table append-only (D-008); PK on `tran_id` turns a duplicate into an abend. Online writers (COBIL00C, COTRN02C) stay COBOL against VSAM; **deferred**: shared TRAN-ID allocation. Dependency: online streams ONL-M08/M10. Impact: none while COBOL is the oracle (no production dual run, DATA profile "Coexistence"). Re-entry: when ONL-M08 or ONL-M10 enters planning | `transaction.repository.TransactionRepository` (JPA) | duplicate -> `DataIntegrityViolationException` -> abend RC 16; rerun guard before posting | transaction context | `CBTRN02C.cbl:256,562-578`; `COBIL00C.cbl:212-219`; `COTRN02C.cbl:444-451`; R-06, R-07 | none | no strangler routing (DATA profile); cutover = user-approved production switch of the scheduler entry |
| 005 ACCTDATA | B10 | **Account context owns `account.account`**; POSTTRAN updates balances through `AccountRepository` in the record's transaction; `@Version` | `account.repository` | `OptimisticLockException` -> abend RC 16, no retry (records are never silently skipped) | account context | `CBTRN02C.cbl:393-422,545-560`; GC-1, GC-3 | none | as BND-004 |
| 006 TCATBALF | B10 | **Reference context owns `reference.tran_cat_balance`**; read-then-insert-or-update in the record's transaction; `@Version` | `reference.repository` | insert conflict or lock failure -> abend RC 16 | reference context | `CBTRN02C.cbl:467-542`; GC-1, GC-2 | none | as BND-004 |
| 007 CARDXREF | B10 | **Read-only reference table** `card.card_xref`, seeded by the loader from `app/data/ASCII/cardxref.txt` (or EBCDIC) | `card.repository` | not found -> reject 100 | card context | `CBTRN02C.cbl:380-392`; GC-2 | none | as BND-004 |
| 008 scheduler | B9 | **External scheduler** runs the CLI; completion = exit code (0/4 success, 16/12 failure) + `RETURN-CODE` file in the generation dir; CLOSEFIL/OPENFIL/WAITSTEP dropped; CA-7 keeps owning the legacy chain until cutover | `common.batch.BatchCli` | RC 4 is success-with-warnings for the successor | ops (unknown) | `CardDemo.ca7:68-79`; BATCH profile "Scheduler" | **LR-3**: ops to name the scheduler and owner that will run `--job=POSTTRAN` | cutover = scheduler entry switched from CA-7 job to CLI |
| 009 online co-writers | B8 | **Deferred** (no production coexistence): Dependency: a production parallel run or partial cutover. Impact: none while COBOL is the oracle. Re-entry: the user asks for production cutover; then decide the CLOSEFIL-equivalent (file/table lock window) | n/a | n/a | transaction + account contexts | `CLOSEFIL.jcl:22-30`; DATA profile "Coexistence" | none | re-entry condition above |

Shared programs ported on behalf of the module: **none**. Module-level assets this stream builds once and later streams reuse: the record codec and layouts (CVTRA06Y, CVTRA05Y, CVACT01Y, CVACT03Y, CVTRA01Y), the four repositories, the loaders, `BatchCli`.

## 4. Data and persistence (pinned data target: PostgreSQL 16, database `carddemo`)

| Flyway migration (`carddemo-app/src/main/resources/db/migration`) | Content |
|---|---|
| `V1__schemas.sql` | schemas `account`, `card`, `customer`, `transaction`, `reference`, `batch` |
| `V2__spring_batch_metadata.sql` | Spring Batch 5 PostgreSQL metadata tables in schema `batch` (`spring.batch.jdbc.table-prefix=batch.BATCH_`, `initialize-schema=never`) |
| `V3__account.sql` | `account.account` (CVACT01Y: `acct_id BIGINT PK`, `active_status VARCHAR(1)`, `curr_bal/credit_limit/cash_credit_limit/curr_cyc_credit/curr_cyc_debit NUMERIC(12,2)`, `open_date/expiration_date/reissue_date DATE`, `addr_zip/group_id VARCHAR(10)`, `version BIGINT`) |
| `V4__card_xref.sql` | `card.card_xref` (CVACT03Y: `card_num VARCHAR(16) PK`, `cust_id BIGINT`, `acct_id BIGINT`, index on `acct_id` = AIX `KEYS(11,25)`) |
| `V5__tran_cat_balance.sql` | `reference.tran_cat_balance` (CVTRA01Y: PK `acct_id BIGINT, type_cd VARCHAR(2), cat_cd BIGINT`, `balance NUMERIC(11,2)`, `version BIGINT`) |
| `V6__transaction.sql` | `transaction.transaction` (CVTRA05Y: `tran_id VARCHAR(16) PK`, ..., `amount NUMERIC(11,2)`, `orig_ts TIMESTAMP(6)`, `proc_ts TIMESTAMP(6)`, index on `proc_ts` = AIX `KEYS(26 304)`) |
| `V7__posttran_reject.sql` | `batch.posttran_reject` (`job_instance_id BIGINT`, `input_seq BIGINT`, `record CHAR(430)`, PK (`job_instance_id`, `input_seq`)): reject staging for restart (Restart state row, §2) |

Type mapping: the field dictionary in `POSTTRAN_analysis.md` §4.2 (CORE rules). Two INFERRED mappings need a decision (D-013): timestamps and the expiration date. FKs across schemas (e.g. `card_xref.acct_id -> account.acct_id`) are allowed by the DATA profile but **not added** for this stream, because the COBOL tolerates a cross-reference to a missing account (reason 101, FR-07).

Seeding: `--load=<TABLE>=<file>` loader (ASCII line-sequential with the sample quirks in `02_conventions.md`: pad CARDXREF to 50, strip CR from TCATBALF; or EBCDIC CP037 FB). Golden cases load the same files as the oracle.

## 5. Phase 0 scaffolding deltas (main has no `modernization/`; everything is new)

| Path | Delta |
|---|---|
| `modernization/pom.xml` | parent `spring-boot-starter-parent` 3.3.4, Java 21, modules `carddemo-common`, `carddemo-app`; Testcontainers BOM 1.21.4 |
| `modernization/carddemo-common` | `com.carddemo.common.cobol` (zoned/overpunch, COMP-3, COMP codecs, `CobolDecimal` truncation, CP037 charset), `.record` (`RecordLayout`, `Field`, `RecordReader`/`RecordWriter` for ASCII LS and EBCDIC FB), `.batch` (`BatchCli`, `ReturnCode`, `GenerationDirectory`, `SysoutWriter`), `.error` (`FileStatusException` family) |
| `modernization/carddemo-app` | Boot app, `application.yml` (`local` profile: Docker `postgres:16`; `test`: Testcontainers), Flyway, JPA `ddl-auto: validate`, Spring Batch with `spring.batch.job.enabled=false` (CLI launches) |
| Dependencies | `spring-boot-starter-batch`, `-data-jpa`, `-test`, `flyway-core`, `flyway-database-postgresql`, `postgresql`, `spring-batch-test`, Testcontainers `postgresql` + `junit-jupiter` 1.21.4. All on the CORE allow-list. Optional HTTP trigger (`starter-web`) **not** in Phase 0 |
| `.github/workflows/modernization-verify.yml` | Temurin 21, Docker check, `mvn -B verify` in `modernization/` |
| `.migration/07_runbook.md` | promote N-01..N-04 to verified when they run |

Phase 1 (data): Flyway V1-V7, entities and repositories for the 4 tables, the loader, `DailyTransactionLayout` + reader, `RejectRecordLayout` + writer, golden fixtures copied to `modernization/carddemo-app/src/test/resources/golden/POSTTRAN/GC-<n>/` (inputs + oracle outputs from R-04/R-06/R-11/R-12), the golden-compare harness (unload tables to fixed-width text exactly like the oracle unloaders, mask TRANSACT bytes 325-326), and an oracle option for a custom CARDXREF to produce GC-6.

## 6. Waves and child fan-out (one child per step, sequential, each merged and green before the next)

| Order | Child session / playbook | Content | Consumes | Produces | Strict edge |
|---|---|---|---|---|---|
| 1 | `!mf_program_fr_generation` | `functional/CARDDEMO/programs/CBTRN02C_functional_requirement.md` | stream FR, analysis | 1 program FR | before Wave 1 |
| 2 | Phase 0 child | scaffolding (§5) | target state | 1 PR -> main | -> 3 |
| 3 | Phase 1 child | data layer + codec layouts + loader + golden harness (§4, §5) | field dictionary, BND-004..007 decisions | 1 PR -> main | -> 4 |
| 4 | **Wave 1** child, `!mf_program_migration` | CBTRN02C -> `POSTTRAN` job: `TransactionValidationService` (1500), `PostingService` (2000/2700/2800/2900), `RejectStore` + `RejectFileWriter` (2500, staged rejects published at job end), `PostingRunListener` (counts, RC), `ProcessingTimestampProvider`, `RerunGuard` (D-008), `BatchCli` wiring | CBTRN02C FR doc, BND-001..009 decisions, D-008/009/011..014 | 1 PR -> main | -> 5 |
| 5 | `!mf_program_parity_test` (Wave 1) | GC-1..GC-6 + JT-1..JT-4 | oracle outputs | `functional/CARDDEMO/POSTTRAN_parity/wave-1.md` | STOP D (notify) |
| 6 | `!mf_stream_signoff` + independent audit session | end-to-end, CI gate, evidence | all | `POSTTRAN_signoff.md`, `POSTTRAN_audit.md` | STOP E |

Single repo: each step is one PR. Phase 3 (UI testing) is skipped: no screens.

## 7. Testing and verification

- Parity per golden case (output-evidence mode, no recordings), two modes:
  - **Normal-end cases (GC-1, GC-2, GC-3, GC-5, GC-6):** DALYREJS sha256; TRANSACT/ACCTDATA/TCATBALF unloads equal to the oracle run on the same input (TRANSACT bytes 325-326 masked, D-007); exit code equal; SYSOUT count lines equal.
  - **Abend cases (GC-4):** Java exit code 16 (oracle 231); tables equal to the oracle run on the **valid prefix** (the records before the failing one, runbook R-13), not to the oracle's abend run, because COBOL leaves the failing record's account and category updates applied and Java rolls them back (D-009); assert that **no DALYREJS was published** in the generation dir (D-012); no counter comparison (COBOL never reaches its count DISPLAYs, `CBTRN02C.cbl:221-234`).
- GC-1 sample 300, GC-2 `syn_all`, GC-3 `syn_ok`, GC-4 in-file duplicate, GC-5 empty, GC-6 missing account (to build). Java-only JT-1 rerun guard, JT-2 atomic rollback of the failing record, JT-3 reason 101 unit test.
- **JT-4 restart after a transient failure** (CI): run GC-1 with a fault injected after record 150 is committed (e.g. the test datasource fails the next commit) -> exit 16, 150 records committed, no DALYREJS published; then `--restart` the same JobInstance -> the guard is skipped, the reader resumes at record 151, final exit 4, tables, DALYREJS sha256 and SYSOUT counts (300 / 38) equal the GC-1 oracle run. A second test starts a **new** instance on the same file after completion and expects RC 16 from the guard.
- Every FR maps to at least one case (FR traceability matrix §9).
- CI gate: `modernization-verify.yml` runs all of the above with Testcontainers `postgres:16`.

## 8. New decisions at STOP C (APPROVED by user, STOP C, 2026-11-19 ("Approve"), recorded in `06_decisions.md`)

| Id | Question | Proposed | Why |
|---|---|---|---|
| D-011 | Commit interval | **1 record per chunk** for POSTTRAN (the job-level property exists for other jobs; POSTTRAN is pinned to 1, a larger value needs a new decision entry) | FR-16/GC-4: COBOL keeps every record posted before an abend; a 100-record chunk would roll back up to 99 earlier records on an abend. The target state's default of 100 would change abend-path table state. Volume is small (300/day in the sample) |
| D-012 | DALYREJS on abend (OQ-6) | write to a temp file, publish only on RC 0/4; abend-path parity compares RC + tables, not DALYREJS | `POSTTRAN.jcl:34` `DISP=(NEW,CATLG,DELETE)` deletes the generation on abend; amends the DALYREJS half of D-009's abend rule |
| D-013 | INFERRED types | `orig_ts`/`proc_ts` TIMESTAMP(6) with per-field formatters (`yyyy-MM-dd HH:mm:ss.SSSSSS` / `yyyy-MM-dd-HH.mm.ss.SS0000`); an unparseable ORIG-TS abends RC 16 (COBOL would copy the bytes: deviation on malformed input only). `expiration_date` DATE, compared as ISO text-equivalent | CORE type mapping; sample data is ISO throughout |
| D-014 | `WS-TEMP-BAL` truncation | reproduce (truncate to S9(9)V99 before the limit compare) | drift rule 5: quirks are not fixed without a decision |

## 9. Risks and blockers (carried from the analysis)

| Risk | Status in plan |
|---|---|
| OQ-5 DALYTRAN producer/encoding, DALYREJS consumer | LR-1, LR-2; blocks production cutover only |
| Scheduler owner unknown | LR-3; blocks production cutover only |
| Timestamp formats, expiry text compare | D-013 |
| WS-TEMP-BAL truncation | D-014 |
| 103 overrides 102; DEBIT negative | FR-08..10, GC-2/GC-3 |
| 0109 dead path | not implemented as a reject; documented in the program FR |
| GC-6 needs an oracle extension | Phase 1 task |
| TRAN-ID allocation shared with online writers | BND-004 deferral with re-entry condition |

No blocker for Phase 0, Phase 1 or Wave 1 once STOP C approves.

## 10. Effort and sequencing

About one session each for Phase 0, Phase 1, Wave 1, and parity + sign-off (4 sequential child sessions plus the audit session). LR-1..LR-3 run in parallel from approval and only gate production cutover.

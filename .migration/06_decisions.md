# 06 - Decisions (append-only, dated)

Format: id, date, status, decision, evidence. AUTO-APPLIED (revisable) = Devin default under the orchestrator rule "no preguntes nada: aplica tu opción recomendada, regístrala ... y sigue" (USER, 2026-11-19). To revise one, append a new entry that supersedes it; never edit old entries.

| Id | Date | Status | Decision | Evidence |
|---|---|---|---|---|
| D-001 | 2026-11-19 | CONFIRMED (USER) | STOP A: target state confirmed from the brief (Java 21, Boot 3.3, Batch 5, monolith carddemo-app + carddemo-common, PostgreSQL 16, Flyway, files vs tables, chunk 1 restartable, forbidden list) | USER brief; `functional/CARDDEMO/CardDemo_target_state.md` |
| D-002 | 2026-11-19 | CONFIRMED (USER + source) | STOP B: stream = BATCH POSTTRAN, `POSTTRAN.jcl` STEP15 → CBTRN02C, hard stop GOBACK | `app/jcl/POSTTRAN.jcl:23`; `app/cbl/CBTRN02C.cbl:234`; inventory §4 |
| D-003 | 2026-11-19 | AUTO-APPLIED (revisable) | Package layout `com.carddemo.<context>.*`, shared `com.carddemo.common.*`; contexts account, card, transaction, reference | REF `docs/migration/architecture.md` §2, collapsed into one app |
| D-004 | 2026-11-19 | AUTO-APPLIED (revisable) | Adopt REF type conventions; dates/timestamps compared or copied as text stay `VARCHAR` | REF conventions §1–§5; `CBTRN02C.cbl:414` compares `ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)` as text |
| D-005 | 2026-11-19 | AUTO-APPLIED (revisable) | One fixed-width layout per copybook, ISO-8859-1 + CP037 codec in carddemo-common | R-09 shows both encodings give the same result |
| D-006 | 2026-11-19 | AUTO-APPLIED (revisable) | Every abend path → exception, step FAILED, no skip policy | `CBTRN02C.cbl:707-711` |
| D-007 | 2026-11-19 | AUTO-APPLIED (revisable) | Reproduce DISPLAY lines on stdout + SYSOUT file; mask card numbers in technical logs | `CBTRN02C.cbl:193,227-228,233,476` |
| D-008 | 2026-11-19 | AUTO-APPLIED (revisable) | CI: `.github/workflows/modernization-verify.yml`, Temurin 21, `mvn -B verify` | REF uses the same workflow name |
| D-009 | 2026-11-19 | AUTO-APPLIED (revisable) | Dependency allow-list: Boot starters (batch, data-jpa, test), Flyway + flyway-database-postgresql, PostgreSQL driver, Testcontainers postgresql/junit-jupiter | USER "sin dependencias nuevas no registradas" |
| D-010 | 2026-11-19 | AUTO-APPLIED (revisable) | One DB `carddemo`, schemas `account`, `card`, `transaction`, `reference`, `batch` (JobRepository) | REF schema-per-service, merged |
| D-011 | 2026-11-19 | AUTO-APPLIED (revisable) | One DALYTRAN record = one DB transaction (TCATBAL + ACCOUNT + TRANSACT) | USER chunk 1; COBOL updates all three per record `CBTRN02C.cbl:440-442` |
| D-012 | 2026-11-19 | AUTO-APPLIED (revisable) | Restart: reader resumes after committed count; DALYREJS writer truncates to committed byte offset then appends | USER "chunk de 1 con reinicio" |
| D-013 | 2026-11-19 | AUTO-APPLIED (revisable) | No extra rerun guard: re-posting a TRAN-ID fails on PK like COBOL status 22 (RC 16) | R-06 |
| D-014 | 2026-11-19 | AUTO-APPLIED (revisable) | CLI launch `--job=POSTTRAN --dd=DALYTRAN=… --dd=DALYREJS=…`, no PARM | `POSTTRAN.jcl:23` has no PARM |
| D-015 | 2026-11-19 | AUTO-APPLIED (revisable) | Exit codes: 0/4 as source, 16 abend (GnuCOBOL shows 231), 12 usage error | `CBTRN02C.cbl:229-231`; R-06 |
| D-016 | 2026-11-19 | AUTO-APPLIED (revisable) | DALYREJS path from `--dd` or `<output-dir>/POSTTRAN/<run-id>/DALYREJS`; refuse existing file | `POSTTRAN.jcl:34-39` `DISP=(NEW,CATLG,DELETE)`, `(+1)` |
| D-017 | 2026-11-19 | AUTO-APPLIED (revisable) | Scheduler integration = process exit code; CA-7 chain re-created in the client's scheduler (out of repo) | `app/scheduler/CardDemo.ca7` |
| D-018 | 2026-11-19 | AUTO-APPLIED (revisable) | Drift rules of target state §3 are PR-blocking | target state §3 |
| D-019 | 2026-11-19 | AUTO-APPLIED (revisable) | Spring Data JPA, explicit columns, `@Version` on accounts and tcatbal | REF conventions §7 |
| D-020 | 2026-11-19 | AUTO-APPLIED (revisable) | FILLER not stored; spaces on unload | copybooks CVACT01Y/CVTRA01Y/CVTRA05Y FILLER |
| D-021 | 2026-11-19 | AUTO-APPLIED (revisable) | Loader from `app/data/ASCII` fixed-width files | oracle uses the same inputs (R-04) |
| D-022 | 2026-11-19 | AUTO-APPLIED (revisable) | No production strangler routing; parity proven offline against the oracle | USER parity rule |
| D-023 | 2026-11-19 | AUTO-APPLIED (revisable) | Oracle and synthetic-case scripts adapted from PRIOR (evidence) and re-executed in this run; results in 07_runbook R-04..R-09 | runbook |

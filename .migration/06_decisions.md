# 06 - Decision log (append-only, dated)

Never edit or delete a past entry; supersede it with a new dated entry that names the one it replaces.

## Entry schema

`### D-<NNN> <title>` - **Date**, **Decision**, **Alternatives considered**, **Justification / evidence**, **Approved by**, **Scope** (module / stream / boundary id), **Supersedes**.

## Decisions

### D-001 Application shape: modular monolith
- **Date:** 2026-11-19
- **Decision:** one deployable Spring Boot app `modernization/carddemo-app` + library `modernization/carddemo-common`; bounded contexts are packages, not services.
- **Alternatives considered:** REF's five Spring Boot services (REF `docs/migration/architecture.md` §1).
- **Justification / evidence:** REF's split makes POSTTRAN post to account-service over HTTP, so a local failure after posting leaves the account updated without a TRANSACT row (REF `docs/migration/residual-risks.md` C2). A monolith keeps CBTRN02C's TRANSACT write, ACCT rewrite and TCATBAL update (`app/cbl/CBTRN02C.cbl:440-442`) in one DB transaction. Target state C-1.
- **Approved by:** user, STOP A, 2026-11-19 ("confirm").
- **Scope:** module.

### D-002 Batch step model: chunked, restartable; full rerun abends RC 16 on duplicate TRAN-ID
- **Date:** 2026-11-19
- **Decision:** each JCL step is a chunk-oriented Spring Batch step restartable from the last committed chunk (commit interval default 100). A full rerun of an already-posted DALYTRAN file fails the job with RC 16 at the first duplicate TRAN-ID on the TRANSACT insert (no silent skip). Any idempotency beyond that needs a new decision entry.
- **Alternatives considered:** tasklet per program (REF); idempotent skip of already-posted IDs.
- **Justification / evidence:** COBOL has no checkpoint; duplicate key on `WRITE FD-TRANFILE-REC` (`app/cbl/CBTRN02C.cbl:564-577`) abends 999 (verified under GnuCOBOL: FILE STATUS 22, ABCODE 999, exit 231; `07_runbook.md` R-06). **See OQ-1:** CBTRN02C opens TRANSACT `OUTPUT` (`app/cbl/CBTRN02C.cbl:256`), so the "rerun of an already-posted file" case does not behave like this under GnuCOBOL.
- **Approved by:** user, STOP A, 2026-11-19. **Rerun half superseded by D-008** (STOP B).
- **Scope:** BATCH profile, stream POSTTRAN.

### D-003 DALYTRAN and DALYREJS stay files
- **Date:** 2026-11-19
- **Decision:** DALYTRAN is read from the file named by `--dd=DALYTRAN=<file>` (CVTRA06Y, ASCII or EBCDIC); DALYREJS(+1) is written as 430-byte fixed records to `<output-dir>/POSTTRAN/<yyyyMMdd-HHmmss-SSS>/DALYREJS`, with SYSOUT and RETURN-CODE in the same generation directory. KSDS masters (TRANSACT, ACCTDATA, CARDXREF, TCATBALF) become PostgreSQL tables.
- **Alternatives considered:** REF staging DALYTRAN into a `daily_transactions` table and rejects into a table (REF `docs/migration/architecture.md` §4.2).
- **Justification / evidence:** keeps the job byte-comparable with the GnuCOBOL oracle (target state C-3); DD layout `app/jcl/POSTTRAN.jcl:28-42`.
- **Approved by:** user, STOP A, 2026-11-19.
- **Scope:** stream POSTTRAN.

### D-004 Parity bar vs GnuCOBOL
- **Date:** 2026-11-19
- **Decision:** a POSTTRAN golden case passes only with (1) byte-identical DALYREJS, (2) identical end state of TRANSACT, ACCTDATA and TCATBALF (compared as fixed-width unloads in key order), (3) identical RC (COBOL abend 999 = Java RC 16, see `02_conventions.md`), against the unmodified CBTRN02C compiled with `cobc -std=ibm -I app/cpy` on the same inputs.
- **Alternatives considered:** row-count / checksum-level parity; table-level comparison only.
- **Justification / evidence:** oracle runnable today (`07_runbook.md` R-04). **See OQ-2:** TRAN-PROC-TS is `FUNCTION CURRENT-DATE` (`app/cbl/CBTRN02C.cbl:693`); the clock must be pinned and the hundredths masked.
- **Approved by:** user, STOP A, 2026-11-19.
- **Scope:** stream POSTTRAN (default for every batch stream unless a later entry overrides).

### D-005 Forbidden list (CORE)
- **Date:** 2026-11-19
- **Decision:** no Lombok; no H2; no `double`/`float` for money; no silently swallowed I/O errors; no edits under `app/**`; no new runtime dependency beyond Spring Boot starters, Flyway, the PostgreSQL driver and Testcontainers **without a decision entry here**.
- **Approved by:** user, STOP A, 2026-11-19.
- **Scope:** module.

### D-006 DOCS location and delivery
- **Date:** 2026-11-19
- **Decision:** artifacts live in `functional/CARDDEMO/**` and `.migration/**` on `main`, delivered through PRs; pre-migration artifacts are committed on `devin/1795090039-mf-posttran-stream` without a PR until the orchestrator says so.
- **Approved by:** user, STOP A, 2026-11-19 (target state §7 item 3).
- **Scope:** module.

### D-007 GnuCOBOL oracle clock: `COB_CURRENT_DATE`, not faketime
- **Date:** 2026-11-19
- **Decision:** the golden run pins the clock with GnuCOBOL's `COB_CURRENT_DATE` (default `2022/07/18 10:30:45`) instead of `faketime`; comparisons of TRANSACT mask TRAN-PROC-TS hundredths (record bytes 325-326).
- **Alternatives considered:** `faketime` as in REF `scripts/migration/golden/batch-service-reference.sh` (not installable here: `apt-get install faketime` failed, archive.ubuntu.com unreachable).
- **Justification / evidence:** `07_runbook.md` R-04/R-05: with `COB_CURRENT_DATE` two runs give identical DALYREJS, ACCTDATA, TCATBALF, SYSOUT, RC; TRANSACT differs only in hundredths (`.52`/`.53`), equal once bytes 325-326 are masked.
- **Approved by:** user, STOP B, 2026-11-19 ("confirm"; resolves OQ-2).
- **Scope:** stream POSTTRAN oracle.

### D-008 Rerun guard: an already-posted input abends RC 16 before posting (supersedes the rerun half of D-002)
- **Date:** 2026-11-19
- **Decision:** before posting, the Java job checks whether any TRAN-ID in the DALYTRAN input already exists in TRANSACT; if so it abends with RC 16 and changes nothing. TRANSACT is append-only across runs in Java. This case is **excluded from GnuCOBOL parity** (the oracle truncates TRANSACT on `OPEN OUTPUT` under GnuCOBOL, and z/OS behavior is only inferred); it is covered by a Java test instead. A duplicate TRAN-ID **within one input file** stays under parity (COBOL abend 999 = Java RC 16, `07_runbook.md` R-06).
- **Alternatives considered:** mirror GnuCOBOL truncate + double-post; D-002's abend at the first duplicate insert (would leave earlier chunks committed).
- **Justification / evidence:** OQ-1 evidence (`app/cbl/CBTRN02C.cbl:256`, `app/jcl/TRANFILE.jcl:49-61`, `07_runbook.md` R-07).
- **Approved by:** user, STOP B, 2026-11-19 ("confirm"; resolves OQ-1).
- **Scope:** stream POSTTRAN.

### D-009 Atomic per-record posting (deliberate deviation on the abend path)
- **Date:** 2026-11-19
- **Decision:** TCATBAL update, ACCOUNT rewrite and TRANSACT insert for one record commit or roll back together. On an abend, Java leaves no partial posting for the failing record, whereas COBOL leaves TCATBAL and ACCOUNT applied (`app/cbl/CBTRN02C.cbl:440-442`). Abend-path parity compares RC and DALYREJS; table state is compared excluding the failing record's partial updates.
- **Approved by:** user, STOP B, 2026-11-19 ("confirm"; resolves OQ-3).
- **Scope:** stream POSTTRAN.

### D-010 Stream scope confirmed
- **Date:** 2026-11-19
- **Decision:** stream POSTTRAN, BATCH (`app/jcl/POSTTRAN.jcl:23`, `app/scheduler/CardDemo.ca7:72-79`), one program CBTRN02C, hard stop CBTRN02C `GOBACK` (`app/cbl/CBTRN02C.cbl:234`); exclusions as in `functional/CARDDEMO/CardDemo_inventory.md` (CBTRN01C, scheduler neighbours, TRANSACT lifecycle jobs, downstream batch, online co-writers COBIL00C/COTRN02C, DALYTRAN producer / DALYREJS consumer, CEE3ABD -> RC 16).
- **Approved by:** user, STOP B, 2026-11-19 ("confirm").
- **Scope:** stream POSTTRAN.

### D-011..D-014 (APPROVED at STOP C)
- **Date:** 2026-11-19. **Status:** proposed by `!mf_stream_migration_plan`; **approved by user, STOP C, 2026-11-19 ("Approve")**. Detail and evidence: `functional/CARDDEMO/POSTTRAN_migration_plan.md` §8.
- **D-011 commit interval:** 1 record per chunk for POSTTRAN (configurable), so records before an abend stay committed as in COBOL (FR-16, R-06/R-12).
- **D-012 DALYREJS on abend (OQ-6):** publish DALYREJS only on RC 0/4; abend-path parity compares RC + tables (`app/jcl/POSTTRAN.jcl:34` `DISP=(NEW,CATLG,DELETE)`). Amends the DALYREJS half of D-009's abend rule.
- **D-013 inferred types:** TIMESTAMP(6) with per-field formatters for ORIG-TS/PROC-TS (unparseable ORIG-TS -> RC 16, deviation on malformed input only); expiration date as DATE.
- **D-014 WS-TEMP-BAL:** reproduce S9(9)V99 truncation (`app/cbl/CBTRN02C.cbl:187,403-405`).
- **Boundary decisions BND-001..009:** DECIDED (STOP C), see `04_boundary_register.md` "Plan proposals".

## Open questions (carry to the next stop; resolve with a D- entry)

- **OQ-1 (RESOLVED by D-008, STOP B) (blocks the rerun half of D-002):** CBTRN02C opens TRANSACT with `OPEN OUTPUT` (`app/cbl/CBTRN02C.cbl:256`). Under GnuCOBOL this empties the file: a second run on already-posted masters gave RC 4, TRANSACT holding only that run's 187 rows, ACCTDATA balances posted twice and 113 rejects (`07_runbook.md` R-07). On z/OS, `OPEN OUTPUT` on a loaded KSDS defined without `REUSE` (TRANFILE define `app/jcl/TRANFILE.jcl:49-61` has no REUSE) is expected to fail at OPEN, which CBTRN02C turns into abend 999 (`:254-268`) - **INFERRED, not verified** (no mainframe). The operator scripts rebuild TRANSACT before POSTTRAN (`scripts/run_posting.sh`: TRANBKP then POSTTRAN). Decide: which behavior is the parity oracle for a rerun (z/OS open failure = RC 16 before any posting, GnuCOBOL truncate, or the D-002 "duplicate TRAN-ID" abend), and whether TRANSACT in Java is append-only across runs.
- **OQ-2 (RESOLVED by D-007, STOP B):** TRAN-PROC-TS hundredths are not reproducible even with a pinned clock; confirm the masked-field parity rule (D-007).
- **OQ-3 (RESOLVED by D-009, STOP B):** partial posting on abend. CBTRN02C updates TCATBAL (`:440`) and ACCOUNT (`:441`) before the TRANSACT write (`:442`); a TRANSACT write failure abends with the first two already applied (no rollback on VSAM). D-001/the target state put the three in one DB transaction, so Java will roll that record back. This is a deliberate deviation from COBOL end state on the abend path and needs a decision entry before the parity bar is applied to abend cases.
- **OQ-4 (RESOLVED: target-state pointer proposed as a repo blueprint knowledge entry `target-state`, pending user approval in the timeline):** knowledge notes mirroring this workspace were not created: in this org they are plugin skills/rules saved with approval and a scope choice (personal / org). `.migration/` is the source of truth; the orchestrator should ask the user whether to save a pointer note.
- **OQ-5 (non-blocking):** who produces DALYTRAN (ASCII or EBCDIC) and who consumes DALYREJS? Neither is in the repo.
- **OQ-6 (RESOLVED by D-012, STOP C):** on abend z/OS deletes the DALYREJS generation (`app/jcl/POSTTRAN.jcl:34` `DISP=(NEW,CATLG,DELETE)`); GnuCOBOL keeps a partial file; D-009 compares DALYREJS on the abend path.

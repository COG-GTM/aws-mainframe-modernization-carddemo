# What the golden set does not prove

The golden set (`make golden-set`, [reconciliation](reconciliation.md)) proves one thing: from the same sample data,
the scripted online scenario followed by the whole nightly cycle leaves the Java 21 application and the GnuCOBOL
build of the batch programs in the same state, field by field, with the same reports, SYSOUT and return codes,
apart from the allow-listed and justified differences. Everything below is outside that proof and needs other
evidence (rules tests, ITs, Playwright, or production-like testing).

## Online behaviour under CICS

- **CICS itself is not run.** The online side of the COBOL comparison is `apply_online_scenario.py`: the business
  rules of `docs/modernization/rules/COACTUPC.md`, `COCRDUPC.md`, `COTRN02C.md`, `COBIL00C.md`, `COUSR01C/02C/03C.md`
  applied to the fixed-width records by an independent Python implementation. It proves the Java online services
  produce the record changes the rules documents describe; it does not prove the rules documents are a complete
  reading of the COBOL. That reading is covered by the per-R-id rules tests (`com.carddemo.web.*RulesTest`), not
  by execution of the COBOL programs. One defect of the COBOL record layout surfaced exactly here (COACTUPC R-39,
  `ACCT-ADDR-ZIP`) and is kept as a documented deviation.
- **Concurrency.** The scenario is a single user at a time. CICS task isolation, VSAM record-level locking
  (`READ UPDATE`/`REWRITE`), ENQ/DEQ and two terminals updating the same account or card are not reproduced.
  The Java optimistic version checks (`409 CHANGED`) and the `pg_advisory_xact_lock` for transaction ids are tested
  by ITs, not by this run. Known open item: POSTTRAN does not take the online transaction-id lock (s6.4).
- **Pseudo-conversational flow.** COMMAREA round trips, `RETURN TRANSID`, PF-key handling, cursor positioning and
  screen re-display after errors are replaced by REST calls and a `NavigationContext` (ADR-0017/0018). The run calls
  the endpoints in a fixed order; it does not walk every PF3/PF7/PF8 path or every error re-display.
- **Only the scenario's paths.** One account update, one card update, two transaction adds, one bill payment, one
  user add/update/delete, one Custom report. Every other validation branch and error message is proven only by the
  rules tests. Rejections are not part of the scenario (every step expects 2xx).

## Presentation

- **3270 rendering.** BMS maps, attributes, colours and field lengths are not compared. The React UI is a
  replacement (ADR-0022), proven by its own Playwright suite and the BMS field check, and is not a comparison target.
- **Report and statement layout on a real printer / PDF.** TRANREPT and STATEMNT are compared as text records;
  TXT2PDF1 is retired and not run.

## Data and platform

- **EBCDIC collation.** GnuCOBOL runs on ASCII files and PostgreSQL browse keys use `COLLATE "C"` (ASCII byte order,
  `docs/modernization/03-data-model.md`). For keys that mix upper case, lower case and digits, EBCDIC orders letters
  before digits and lower case before upper case; ASCII does the opposite. The sample keys are all digits or all
  upper case + digits in the same positions, so the order agrees here; real data with mixed keys could browse and
  sort in a different order than on z/OS.
- **EBCDIC vs ASCII source data.** The COBOL side starts from `app/data/ASCII`, the Java side from
  `app/data/EBCDIC`. They differ in one value the cycle carries (ACCTDATA record 49 ZIP, allow-listed) and in one
  the cycle never reads (DISCGRP record 34). Packed-decimal (COMP-3) and binary fields of the real files are proven by
  the codec tests, not by this run (the in-scope copybooks are display numerics).
- **GnuCOBOL is not Enterprise COBOL.** The baseline is GnuCOBOL 3.1.2 with the build patches and stubs listed in
  `docs/validation/baseline/` (e.g. CBSTM03A TIOT, CEE3ABD). Compiler-specific arithmetic, truncation (`TRUNC`),
  `NUMPROC` and abend behaviour of IBM Enterprise COBOL on z/OS are not proven.
- **VSAM and IDCAMS.** KSDS are emulated by GnuCOBOL indexed files on one side and PostgreSQL tables on the other.
  AIX upgrade sets, CI/CA splits, SHAREOPTIONS and VSAM file statuses other than the ones the programs test are not
  exercised.
- **Production volumes.** The run uses the sample data: 50 accounts, 50 cards, 300 daily transactions, 10 users.
  Throughput, batch window, memory, lock contention and PostgreSQL query plans at production volumes are untested.

## Out-of-scope applications and scheduling

- **Db2, IMS and MQ extension applications** (`app/app-authorization-ims-db2-mq`, `app/app-transaction-type-db2`,
  `app/app-vsam-mq`: CBPAUP0J, TRANEXTR, MNTTRDB2, COPAU*, COTRTUPC, …) are out of scope (`d-scope`) and not run.
- **Control-M / CA-7 itself.** The `nightly-cycle` job replaces the scheduler's job order and conditions
  (ADR-0016, `docs/modernization/06-scheduling.md`); the run proves that order and the RC/COND semantics on one
  cycle, not calendars, cross-day dependencies, reruns from a failed job, operator holds or alerting. CLOSEFIL,
  OPENFIL and WAITSTEP are retired and not run.
- **Restart and recovery.** Every job is run once to normal end. Abend handling (RC 16), restarts of a failed cycle
  and the refused restart of posting jobs are proven by ITs and `make batch-equivalence`, not here.

## Not run in this golden set

- The refresh loads TRANTYPE, TRANCATG, DISCGRP and TCATBALF are part of the starting data (initial-load on Java,
  IDCAMS REPRO on GnuCOBOL) but are not re-run after the online scenario.
- CBEXPORT/CBIMPORT, the alternate-index rebuild TRANIDX and the statement program's GDG retention are not part of
  the nightly cycle and are covered by their own checks (`docs/validation/data-load/`, `make batch-equivalence`).
- Security: JWT handling, role checks and the ADMIN-only routes are exercised only as far as the scenario signs on
  as USER0001 and ADMIN001; the role matrix is proven by `UserAdminRoleTest` and the online ITs. Hardening (s6.4)
  is not covered.

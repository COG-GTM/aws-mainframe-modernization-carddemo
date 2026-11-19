# 04 - Boundary register (append-only, dated)

A boundary is anything POSTTRAN touches that it does not own alone. Rows are never edited: a status change is a new dated row with the same id. Status flow: `REGISTERED` (inventory) → `DECIDED` (plan, `!mf_boundary_resolution`) → `VERIFIED` (wave sign-off).

| Id | Date | Status | Class | Boundary | Direction | Evidence | Other parties |
|---|---|---|---|---|---|---|---|
| B-01 | 2026-11-19 | REGISTERED | DATASET_IN | DALYTRAN PS, LRECL 350 (CVTRA06Y) | in | `POSTTRAN.jcl:30`; `CBTRN02C.cbl:29-32` | Producer outside `app/` (only `TRANFILE.jcl` defines/loads it) |
| B-02 | 2026-11-19 | REGISTERED | DATASET_OUT | DALYREJS GDG `(+1)`, RECFM=F LRECL 430 | out | `POSTTRAN.jcl:34-39`; `CBTRN02C.cbl:446-465` | GDG base `DALYREJS.jcl`; no consumer in `app/` |
| B-03 | 2026-11-19 | REGISTERED | SHARED_DATA | TRANSACT KSDS (CVTRA05Y), written | out | `POSTTRAN.jcl:28`; `CBTRN02C.cbl:256,562-579` | Jobs CBEXPORT, COMBTRAN, CREASTMT, TRANBKP, TRANFILE, TRANIDX, TRANREPT; CICS COBIL00C, CORPT00C, COTRN00C/01C/02C |
| B-04 | 2026-11-19 | REGISTERED | SHARED_DATA | ACCTDATA KSDS (CVACT01Y), read + rewrite | in/out | `POSTTRAN.jcl:40`; `CBTRN02C.cbl:393-420,545-560` | Jobs ACCTFILE, CBEXPORT, CREASTMT, INTCALC, READACCT; CICS COACTUPC, COACTVWC, COBIL00C, COTRN02C |
| B-05 | 2026-11-19 | REGISTERED | SHARED_DATA | CARDXREF KSDS (CVACT03Y), read | in | `POSTTRAN.jcl:32`; `CBTRN02C.cbl:380-391` | Jobs CBEXPORT, CREASTMT, INTCALC, READXREF, TRANREPT, XREFFILE; CICS COTRN02C |
| B-06 | 2026-11-19 | REGISTERED | SHARED_DATA | TCATBALF KSDS (CVTRA01Y), read + write + rewrite | in/out | `POSTTRAN.jcl:42`; `CBTRN02C.cbl:467-543` | Jobs INTCALC, PRTCATBL, TCATBALF |
| B-07 | 2026-11-19 | REGISTERED | RUNTIME_SERVICE | `CALL 'CEE3ABD'` (LE abend U999) | out | `CBTRN02C.cbl:707-711` | IBM LE runtime |
| B-08 | 2026-11-19 | REGISTERED | SCHEDULER | CA-7 chain CLOSEFIL → CBPAUP0J → POSTTRAN → WAITSTEP; RC 0/4 | in/out | `CardDemo.ca7:70,73,97`; `CBTRN02C.cbl:229-231` | Operations scheduler |
| B-09 | 2026-11-19 | REGISTERED | ONLINE_COEXISTENCE | CICS files TRANSACT/CCXREF/ACCTDAT/CXACAIX closed by CLOSEFIL before and reopened by OPENFIL after the batch window | context | `CLOSEFIL.jcl:26-30`; `OPENFIL.jcl:26-30` | CICS region CICSAWSA (online streams, out of scope) |
| B-01 | 2026-11-19 | DECIDED | DATASET_IN | DALYTRAN kept as file `--dd=DALYTRAN` | in | `POSTTRAN_migration_plan.md` §4 | - |
| B-02 | 2026-11-19 | DECIDED | DATASET_OUT | DALYREJS kept as file, new per run (D-016), restart truncate (D-012) | out | plan §4 | - |
| B-03 | 2026-11-19 | DECIDED | SHARED_DATA | `transaction.transactions`; reset at new instance (D-024) | out | plan §4 | as registered |
| B-04 | 2026-11-19 | DECIDED | SHARED_DATA | `account.accounts` + `@Version` | in/out | plan §4 | as registered |
| B-05 | 2026-11-19 | DECIDED | SHARED_DATA | `card.card_xref` read-only | in | plan §4 | as registered |
| B-06 | 2026-11-19 | DECIDED | SHARED_DATA | `reference.tran_cat_balances` + `@Version` | in/out | plan §4 | as registered |
| B-07 | 2026-11-19 | DECIDED | RUNTIME_SERVICE | Exception → exit 16 | out | plan §4 | - |
| B-08 | 2026-11-19 | DECIDED | SCHEDULER | Exit-code contract 0/4/12/16 | in/out | plan §4 | operations |
| B-09 | 2026-11-19 | DECIDED | ONLINE_COEXISTENCE | Same batch window; online untouched | context | plan §4 | CICS |
| B-01 | 2026-11-19 | SEAM BUILT (Phase 1) | DATASET_IN | `transaction.batch.DailyTransactionReader` (FB 350, ISO-8859-1/IBM037, restart by item count) | in | D-038; `DailyTransactionReaderTest` | - |
| B-02 | 2026-11-19 | SEAM BUILT (Phase 1) | DATASET_OUT | `transaction.batch.DailyRejectWriter` (430-byte records, CREATE_NEW, restart truncate to committed offset) | out | D-012, D-016; `DailyRejectWriterTest` | - |
| B-03 | 2026-11-19 | SEAM BUILT (Phase 1) | SHARED_DATA | `transaction.transactions` (V2) + `TransactionRepository`; reset listener is Wave 1 | out | D-040 | as registered |
| B-04 | 2026-11-19 | SEAM BUILT (Phase 1) | SHARED_DATA | `account.accounts` (V2) + `AccountRepository`, `@Version` | in/out | D-019; `KsdsDataServiceTest` | as registered |
| B-05 | 2026-11-19 | SEAM BUILT (Phase 1) | SHARED_DATA | `card.card_xref` (V2) + `CardXrefRepository` | in | `KsdsDataServiceTest` | as registered |
| B-06 | 2026-11-19 | SEAM BUILT (Phase 1) | SHARED_DATA | `reference.tran_cat_balances` (V2) + `TranCatBalanceRepository`, `@Version` | in/out | D-019, D-036; `KsdsDataServiceTest` | as registered |
| B-01 | 2026-11-19 | IMPLEMENTED (Wave 1) | DATASET_IN | Step-scoped `DailyTransactionReader` in `PosttranJobConfig` from `--dd=DALYTRAN`; encoding `carddemo.posttran.dalytran-encoding` (D-045); open failure → `ERROR OPENING DALYTRAN` + exit 16, short record → `ERROR READING DALYTRAN FILE` + exit 16; unbound DD → exit 12 | in | `PosttranJobParityTest` (7 cases incl. `cp037`), `PosttranJobRunTest` PR_CBTRN02C_01/02, `missingDalytranDdIsAJclError` (R-20) | - |
| B-02 | 2026-11-19 | IMPLEMENTED (Wave 1) | DATASET_OUT | `DailyRejectWriter` from `--dd=DALYREJS` or `<carddemo.batch.output-dir>/POSTTRAN/<run.id>/DALYREJS`; existing file on a new instance → exit 12 (`PosttranParametersValidator`); restart truncates to committed offset | out | DALYREJS byte-identical on all 7 golden cases; `NFR_01_restart...`, `FR_POSTTRAN_17...`, `FR_POSTTRAN_11...` (default path) (R-20, R-21) | - |
| B-03 | 2026-11-19 | IMPLEMENTED (Wave 1) | SHARED_DATA | `DailyTransactionPostingService.writeTransaction` (INSERT, duplicate → status 22 → exit 16); `TransactionMasterResetListener` empties the table on a new instance only, opened after DALYTRAN (D-024, D-048) | out | TRANSACT parity on 7 cases (stale row removed); `FR_POSTTRAN_11...`; restart keeps committed rows | as registered |
| B-04 | 2026-11-19 | IMPLEMENTED (Wave 1) | SHARED_DATA | Validator reads (101, 102, 103); posting rewrites CURR-BAL / CYC-CREDIT / CYC-DEBIT with `CobolDecimal.store` (D-047); missing on rewrite → reason 109 logged (D-027); version conflict → exit 16 | in/out | ACCTDATA parity on 7 cases; `DailyTransactionPostingServiceTest` PR_CBTRN02C_11 | as registered |
| B-05 | 2026-11-19 | IMPLEMENTED (Wave 1) | SHARED_DATA | `DailyTransactionValidator` reads `card.card_xref` by card number (100 when absent) | in | `syn-all` golden; `DailyTransactionValidatorTest` PR_CBTRN02C_04 | as registered |
| B-06 | 2026-11-19 | IMPLEMENTED (Wave 1) | SHARED_DATA | `updateTranCatBalance`: create with SYSOUT line or add + rewrite; failures → `ERROR (RE)WRITING/READING TRANSACTION BALANCE FILE` + exit 16 | in/out | TCATBALF parity + SYSOUT creation lines on 7 cases; PR_CBTRN02C_10 tests | as registered |
| B-07 | 2026-11-19 | IMPLEMENTED (Wave 1) | RUNTIME_SERVICE | `PosttranAbendException` / any step failure → `ABENDING PROGRAM` (`PosttranStepReport`, `SysoutWriter`) → step FAILED → exit 16 | out | golden `dup-id` (oracle 231 ≡ 16, SYSOUT equal); restart test exit 16 | - |
| B-08 | 2026-11-19 | IMPLEMENTED (Wave 1) | SCHEDULER | Exit code = `JclExitCodeMapper`: 0 / 4 (rejects > 0, raised in `PosttranStepReport.afterStep`) / 12 / 16 | in/out | RC asserted on 7 golden cases; CLI run R-21 exits 4, 12, 12 | operations |
| B-09 | 2026-11-19 | IMPLEMENTED (Wave 1, no code) | ONLINE_COEXISTENCE | Nothing to build: the job runs in the same batch window; online programs untouched | context | plan §4 | CICS |
| B-03 | 2026-11-19 | PARITY-VERIFIED (independent pass) | SHARED_DATA | TRANSACT: reset on new instance, kept on restart / on DALYTRAN open failure; duplicate id → 16 | out | `Cbtrn02cIndependentParityTest` cases rerun, restart-fixed, dalytran-missing, dup-*; **DV-01** TRAN-AMT -0.00 written as +0.00 | as registered |
| B-04 | 2026-11-19 | PARITY-VERIFIED (independent pass) | SHARED_DATA | ACCTDATA: 101/102/103 reads and D-047 rewrites match GnuCOBOL | in/out | cases limit-exact, expiry-boundary, same-account-multi, ovf-temp-bal, ovf-acct-bal, ovf-cyc | as registered |
| B-06 | 2026-11-19 | PARITY-VERIFIED (independent pass) | SHARED_DATA | TCATBALF: create vs rewrite and SYSOUT lines match | in/out | cases tcatbal-create-rewrite, dup-abend-newkey; **DV-02** overflow to -0.00 written as +0.00 (`ovf-tcatbal`) | as registered |
| B-01 | 2026-11-19 | PARITY-VERIFIED (independent pass) | DATASET_IN | DALYTRAN ASCII and CP037 equal; missing file → `ERROR OPENING DALYTRAN` + 16, no DALYREJS | in | cases *-cp037, dalytran-missing | - |
| B-03 | 2026-11-19 | PARITY-VERIFIED (re-run after D-052) | SHARED_DATA | TRANSACT TRAN-AMT -0.00 kept: DV-01 RESOLVED | out | case negative-zero | as registered |
| B-04 | 2026-11-19 | PARITY-VERIFIED (re-run after D-052) | SHARED_DATA | ACCTDATA CURR-BAL / CYC-DEBIT truncated to -0 keep `}`, exact-zero ADDs `{` | in/out | case negative-zero-balances | as registered |
| B-06 | 2026-11-19 | PARITY-VERIFIED (re-run after D-052) | SHARED_DATA | TCATBALF overflow to -0.00 kept: DV-02 RESOLVED; exact-zero ADD `{` | in/out | cases ovf-tcatbal, negative-zero-balances | as registered |

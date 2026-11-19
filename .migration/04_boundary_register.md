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

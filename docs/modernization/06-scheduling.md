# Batch scheduling — legacy jobs and their Java mapping (stub)

Status: stub written with the reporting/housekeeping port (step s4.4). The scheduler step (s4.6, ADR on the
`d-scheduler` decision: one Spring Batch flow job with in-app cron) extends this into the full CA-7 → cron mapping.
Legacy schedule: `app/scheduler/CardDemo.ca7` (see `02-dependency-map.md` §4d, scheduler edges).

## Jobs ported so far

| Legacy job | Java stream (`--job=`) | Steps (Java jobs) | Notes |
|---|---|---|---|
| POSTTRAN | `posttran` | `STEP15` `cbtrn02c` | rules `rules/CBTRN02C.md` |
| INTCALC | `intcalc` | `STEP15` `cbact04c` | rules `rules/CBACT04C.md` |
| TRANBKP | `tranbkp` | `STEP05R` `reproc`, `STEP05` `idcams-delete`, `STEP10` `idcams-define` | TRANSACT → dated `TRANSACT.BKUP`, then the table is emptied (DELETE/DEFINE CLUSTER = reset of the `transaction` table) |
| COMBTRAN | `combtran` | `STEP05R` `combtran-sort`, `STEP10` `idcams-repro` | `TRANSACT.BKUP(0)` + `SYSTRAN(0)` sorted on TRAN-ID → dated `TRANSACT.COMBINED`, loaded into `transaction` (duplicate key → RC 12, nothing loaded) |
| TRANREPT | `tranrept` | `STEP05` `reproc`, `STEP10` `tranrept-sort`, `STEP15` `cbtrn03c` (JCL `STEP05R` ×2, `STEP10R`) | dated backup, DATEPARM window extract of that backup → `TRANSACT.DALY`, report → `TRANREPT`; rules `rules/CBTRN03C.md` |
| PRTCATBL | `prtcatbl` | `STEP05R` `reproc`, `STEP10R` `prtcatbl-sort` | `DELDEF` (IEFBR14 delete of `TCATBALF.REPT`) is not needed: every output is a new dated generation. TCATBALF → dated `TCATBALF.BKUP`; DFSORT `SORT` + `OUTREC` → dated `TCATBALF.REPT` (41-byte records as in the baseline, although the JCL says LRECL 40) |

The baseline order is `POSTTRAN → INTCALC → TRANBKP → COMBTRAN → TRANREPT → PRTCATBL`
(`docs/validation/baseline/00-ORDER.md`); the CA-7 schedule and the s4.6 flow job decide the production order.


Generations: a step that reads what an earlier step of the same stream wrote (COMBTRAN `STEP10`, TRANREPT `STEP10`
and `STEP15`, PRTCATBL `STEP10R`) is bound to that step's file (`SequentialDatasets.bind`), not to a `(0)` lookup,
which ranks generations by business date and could return another run's output after a backdated run. Inputs
produced by a different stream (COMBTRAN's `TRANSACT.BKUP(0)` from TRANBKP and `SYSTRAN(0)` from INTCALC) stay
`(0)` lookups, as in the JCL.

## Retired jobs (no PostgreSQL equivalent)

| Legacy job | What it does | Java |
|---|---|---|
| `CLOSEFIL` | SDSF `/F CICSAWSA,'CEMT SET FIL(…) CLO'` for TRANSACT, CCXREF, ACCTDAT, CXACAIX, USRSEC: closes the VSAM files in the CICS region so batch can open them for update | **Retired.** The online app and batch share PostgreSQL; there is no file ownership to hand over. Concurrency is handled by transactions and the `version` columns (optimistic locking) on the online tables. |
| `OPENFIL` | `CEMT SET FIL(…) OPE` for the same files after the batch window: gives them back to CICS | **Retired**, same reason. |
| `WAITSTEP` | `COBSWAIT` (calls `MVSWAIT`) sleeps for the SYSIN value, 3600 centiseconds (36 s), between the CLOSEFIL/OPENFIL steps and the batch jobs in the CA-7 chain | **Retired.** It only lets CICS finish closing files; the flow job sequences steps by completion. If a delay is ever needed it is a scheduler setting, not a job. |

CA-7 dependencies that point at these jobs (e.g. `POSTTRAN → WAITSTEP → OPENFIL`) collapse to direct step-to-step
dependencies in the flow job.

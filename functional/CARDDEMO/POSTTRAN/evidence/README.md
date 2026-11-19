# POSTTRAN sign-off evidence

Produced 2026-11-19 on code `e3dff6f5` (parity head) for `functional/CARDDEMO/POSTTRAN_signoff.md`. Regenerate with the commands of
sign-off §2; CI job `posttran-e2e-compose` re-runs the e2e on every relevant change.

| Path | Content | Produced by |
|---|---|---|
| `oracle/<scenario>/` | unchanged GnuCOBOL 3.1 outputs of `app/cbl/CBTRN02C.cbl`: `RETURN-CODE`, `DALYREJS`, `SYSOUT`, `ACCTDATA.unl`, `TCATBALF.unl`, `TRANSACT.unl` | `.migration/tools/posttran_e2e_oracle.sh` (wraps `posttran_gnucobol_oracle.sh`) |
| `oracle/baseline` | sample masters + `app/data/ASCII/dailytran.txt`: RC 4, 38 rejects | idem |
| `oracle/rerun` | second run on the baseline end state (masters = baseline `.unl`), `OPEN OUTPUT TRANSACT` again: RC 4, 113 rejects | idem |
| `oracle/empty` | empty DALYTRAN: RC 0 | idem |
| `oracle/restart-leg1` | DALYTRAN with record 151 = record 1's TRAN-ID: RC 231 (U999), abend on the TRANSACT write | idem |
| `oracle/restart-leg1-atomic` | records 1-150 only: table expectation of the abending leg under D-042 | idem |
| `oracle/cp037` | `app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS`: identical to baseline | idem |
| `oracle/abend-dalytran-missing` | DALYTRAN DD names a missing file: RC 231, no DALYREJS | idem |
| `inputs/DALYTRAN.dup151` | the forced-failure input (300 × 350 bytes, record 151 TRAN-ID = record 1) | `.migration/tools/posttran_e2e_inputs.sh` |
| `e2e/<NN-scenario>/` | Java run on docker compose: `RC` (process exit code), `job.log` (container stdout/stderr = technical log + SYSOUT echo), `SYSOUT` (`--dd=SYSOUT`), `DALYREJS`, `unload/*.unl` (tables after the run, `KsdsDataService.unload`) | `.migration/tools/posttran_e2e.sh` |
| `e2e/01-baseline/db_rows.txt` | psql on the compose PostgreSQL after L01: row counts, stale row gone, sample account / transaction rows, Spring Batch executions | idem |
| `e2e/batch_executions.txt` | all Spring Batch executions of the run (read / write / commit / rollback counts, restart on the same instance) + `select version()` | psql after the run |
| `e2e/e2e_console.txt` | console of the run: per-scenario PASS/FAIL lines of `posttran_e2e_compare.py`, `e2e FAILS=0` | `posttran_e2e.sh` |
| `nfr02_static_check.txt` | NFR-02: no `double`/`float` in main sources, money columns `NUMERIC(p,2)` | grep (commands inside the file) |
| `AUDIT_CHECKLIST.md` | entry point for the independent audit session | this sign-off |

Notes: `06-restart/DALYREJS` is the one DALYREJS shared by both restart legs (copied into each leg directory at the end of the leg);
leg 2 `SYSOUT` holds both legs (a restart appends like a JES spool, D-049). `TRANSACT.unl` differs between oracle runs only in
TRAN-PROC-TS cols 325-326 (D-041); the comparator masks them and the FILLER bytes, nothing else.

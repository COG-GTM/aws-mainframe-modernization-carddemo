# ADR-0012: GDG → dated rows / dated files

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Context
The JCL defines generation data groups with `LIMIT(5)`: `TRANSACT.BKUP`, `TRANSACT.DALY`, `TRANREPT`,
`TCATBALF.BKUP`, `SYSTRAN`, `DALYREJS` (DEFGDGB.jcl, DALYREJS.jcl, ...). Jobs write `(+1)` and read `(0)`.

## Decision
- A GDG written as a file (reports, rejects, backups) becomes a directory with one file per run, named
  `<base>.<businessDate>.<jobExecutionId>` (business date from the injected `Clock`, ADR-0014). `(0)` = newest file,
  `(-1)` = previous. A housekeeping step keeps the newest 5 to mirror `LIMIT(5) SCRATCH`.
- A GDG that is only an intermediate copy of a table (e.g. `TRANSACT.BKUP`, `TCATBALF.BKUP`) may instead become
  rows tagged with `generation_date` + `job_execution_id` in a history table, if the consuming step reads it by query.
- The generation chosen is recorded in the Spring Batch job parameters/execution context so a restart reads the
  same generation, never "latest at restart time".
- Golden-set tests compare the file of the current run with `docs/validation/baseline/<JOB>/`, ignoring the
  generation suffix in names.

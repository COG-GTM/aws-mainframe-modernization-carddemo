# ADR-0016: Control-M / CA-7 schedule → `nightly-cycle` flow job with in-app cron

- Status: Accepted (UNT51-16, 2026-10-07)
- Applies to: `modernization/carddemo-app`, package `com.carddemo.batch.scheduler`; `docs/modernization/06-scheduling.md`

## Context
The batch jobs run under Control-M (`app/scheduler/CardDemo.controlm`: daily/weekly/monthly folders with
in/out-conditions) and CA-7 (`app/scheduler/CardDemo.ca7`: completion-trigger chains). Every chain is bracketed by
`CLOSEFIL` / `WAITSTEP` / `OPENFIL`, which hand the VSAM files from CICS to batch and back. In Java the online app
and batch share PostgreSQL (ADR-0011) and every JCL job is a harness job or `JobStream` with JCL RCs and `COND`
(ADR-0015). Decision `d-scheduler`: one Spring Batch flow job, triggered by cron inside the application and
runnable by hand, instead of an external scheduler.

## Decision
- **One job, `nightly-cycle`**, with one step per in-scope scheduled job in the GnuCOBOL baseline order: READACCT,
  READCARD, READCUST, READXREF, POSTTRAN, INTCALC, TRANBKP, COMBTRAN, TRANREPT, CREASTMT, PRTCATBL
  (`NightlyCycle.MEMBERS`). A step launches the member's job/stream through `BatchJobLauncher`, so the child jobs
  are recorded and behave exactly as when launched from the CLI.
- **Scheduler conditions → `COND=(4,LT,<predecessor>)`** per member (Control-M in-condition / CA-7 trigger = the
  predecessor ended OK: RC ≤ 4, no abend), evaluated with `JclCond`. A member whose predecessor did not run is
  bypassed. Every member step completes (`COMPLETED` / `ABEND` / `BYPASSED`), so independent branches run on.
- **RC** of a member = MAXCC of its stream; cycle RC = highest member RC = CLI exit code. `batch_run` holds the cycle
  row, a row per member (RC + summed counts) and the child rows tagged `cycle.member` / `cycle.execution-id`.
- **No restart** of the cycle (`preventRestart`); repair by re-running individual streams.
- **Cron**: `NightlyCycleTrigger` (`@Scheduled`, `carddemo.batch.scheduler.nightly-cycle.cron`, default
  `0 0 22 * * *`) registered only in the web application when `carddemo.batch.scheduler.enabled` is true; false in
  the `test` and `golden` profiles. A fire is skipped while a cycle is running; `run-date` comes from the clock.
- **Retired**: CLOSEFIL, OPENFIL, WAITSTEP, TXT2PDF1. **Out of scope**: CBPAUP0J (IMS/DB2/MQ extension), TRANEXTR and
  MNTTRDB2 (Db2 extension). **On demand, not nightly**: TRANTYPE, TRANCATG, TCATBALF, DISCGRP refreshes
  (`initial-load`, `repro`).
- **Evidence**: `scripts/batch/run_nightly_cycle.sh file|table` runs the cycle from freshly loaded sample data and
  compares every output with the baseline; chained-run-only differences need a per-group expected-diffs entry.

## Consequences
Daily, weekly and monthly cadences collapse into one nightly cycle, as in the baseline run; a calendar split is a
cron or flow change. No external scheduler is needed to run the full cycle in phase 6, and one command
reproduces it. Multi-instance deployments must enable the cron on one instance only (the running-execution check
covers a single job repository but is not a distributed lock).

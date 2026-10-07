# 10 — Runbook: the nightly batch cycle

Operator view of `nightly-cycle` (`com.carddemo.batch.scheduler`, ADR-0016). Design and the legacy-to-Java mapping
are in [06-scheduling.md](06-scheduling.md); configuration in [09-configuration.md](09-configuration.md).

## 1. What runs, in which order

One Spring Batch job, `nightly-cycle`, one step per member, strictly one at a time:

| # | Member | Runs `--job=` | Runs only if (`COND`, ADR-0015) | Legacy trigger (in-condition) | Writes |
| --- | --- | --- | --- | --- | --- |
| 1 | `READACCT` | `readacct` | — (chain head) | CA-7 chain C ← `CLOSEFIL` | print files |
| 2 | `READCARD` | `readcard` | READACCT RC ≤ 4 | ← READACCT | print files |
| 3 | `READCUST` | `readcust` | READCARD RC ≤ 4 | ← READCARD | print files |
| 4 | `READXREF` | `readxref` | READCUST RC ≤ 4 | ← READCUST | print files |
| 5 | `POSTTRAN` | stream `posttran` | — (chain head) | CA-7 chain A ← `CBPAUP0J` (out of scope) | `transaction`, `account`, `tran_cat_balance`, `DALYREJS` generation |
| 6 | `INTCALC` | stream `intcalc` | POSTTRAN RC ≤ 4 | Control-M `MONTHLY-…-CLOSEFIL` | `account`, `tran_cat_balance`, `SYSTRAN` generation |
| 7 | `TRANBKP` | stream `tranbkp` | INTCALC RC ≤ 4 | Control-M `DAILY-…-CLOSEFIL` | `TRANSACT.BKUP` generation, then **empties `transaction`** |
| 8 | `COMBTRAN` | stream `combtran` | TRANBKP and INTCALC RC ≤ 4 | Control-M `MONTHLY-…-INTCALC` | `TRANSACT.COMBINED` generation, reloads `transaction` |
| 9 | `TRANREPT` | stream `tranrept` | COMBTRAN RC ≤ 4 | (baseline only) | `TRANSACT.BKUP`, `TRANSACT.DALY`, `TRANREPT` generations |
| 10 | `CREASTMT` | stream `creastmt` | COMBTRAN RC ≤ 4 | CA-7 chain D ← `CLOSEFIL` | `TRXFL.SEQ`, `TRXFL`, `STATEMNT.PS`, `STATEMNT.HTML` generations |
| 11 | `PRTCATBL` | stream `prtcatbl` | CREASTMT RC ≤ 4 | CA-7 chain E ← `CLOSEFIL` | `TCATBALF.BKUP`, `TCATBALF.REPT` generations |

Bypass rules: a Control-M in-condition / CA-7 completion trigger means "predecessor ended OK", i.e. it ran, did not
abend and ended RC ≤ 4. Otherwise the member is **bypassed** (`exit_code = 'BYPASSED'`), and so is every member
downstream of it. Chain heads (READACCT, POSTTRAN) always run, so a print-job failure does not stop posting and vice
versa. `CLOSEFIL`/`OPENFIL`/`WAITSTEP`/`TXT2PDF1` are retired, `CBPAUP0J`/`MNTTRDB2`/`TRANEXTR` out of scope,
`TRANTYPE`/`TRANCATG`/`TCATBALF`/`DISCGRP` are on-demand `initial-load` / `repro` runs (06-scheduling.md §1c, §4).

## 2. Return codes

| RC | Meaning | Effect in the cycle |
| --- | --- | --- |
| 0 | OK | successors run |
| 4 | warning (e.g. POSTTRAN wrote rejects: normal on the sample data) | successors run |
| 8 | error (bad record format, after-image not written, ...) | successors bypassed |
| 12 | severe (unexpected exception, duplicate key in COMBTRAN's REPRO, **second cycle refused by `CycleLock`**) | successors bypassed |
| 16 | abend (`AbendException`, CEE3ABD; also a launch that could not start) | successors bypassed |

Member RC = highest RC of its stream's steps; cycle RC = highest member RC = process exit code of a CLI launch.
On the sample data the cycle ends RC 4.

## 3. Starting it

- **Cron**: the web app fires it at `carddemo.batch.scheduler.nightly-cycle.cron` (default `0 0 22 * * *`, zone
  `carddemo.clock.zone`) with `run-date` = today. Disable with `CARDDEMO_SCHEDULER_ENABLED=false`. A fire is
  skipped (warning in the log) while another `nightly-cycle` execution is running.
- **By hand** (from `modernization/`, database settings in the environment):

```bash
java -jar carddemo-app/target/carddemo-app.jar --job=nightly-cycle --run-date=2022-07-06; echo "RC=$?"
# in compose:
docker compose exec carddemo-app java -jar /app/carddemo-app.jar --job=nightly-cycle --run-date=2022-07-06
```

**One cycle at a time.** `CycleLock` holds a PostgreSQL session advisory lock from job start to job end. A second
cycle (manual launch during the cron run, a second app instance) ends **RC 12 before any member runs**: one
`batch_run` job row, no member rows. Wait for the running cycle; do not kill it to free the lock (the lock dies with
its connection, but the members' tables are then half-updated — see §5).

## 4. What ran: `batch_run`

```sql
-- the last cycles
select batch_run_id, job_execution_id, run_date, status, return_code, start_time, end_time, message
from batch_run where job_name = 'nightly-cycle' and step_name is null
order by batch_run_id desc limit 5;

-- members of one cycle (RC, BYPASSED/ABEND, counts)
select step_name as member, exit_code, return_code, read_count, write_count, skip_count, message
from batch_run where job_execution_id = :cycle_execution_id and step_name is not null
order by batch_run_id;

-- child job and step rows of that cycle (as if run from the CLI)
select job_name, step_name, status, return_code, read_count, write_count, message
from batch_run where parameters like '%cycle.execution-id=' || :cycle_execution_id || '%'
order by batch_run_id;

-- dated outputs written by a run
select gdg_base, business_date, job_execution_id, file_path, record_count
from batch_output_file order by business_date desc, job_execution_id desc;
```

`exit_code` of a member step: `COMPLETED`, `ABEND` or `BYPASSED`; `message` carries the failure text or the `COND`
that bypassed it.

## 5. After a failure: rerun one member, resume the night

`nightly-cycle` cannot be restarted (`preventRestart()`), and neither can the posting streams `posttran` and
`intcalc`: a restart would re-apply updates already committed before the failure. Repair the night by running the
failed member's stream **and every member after it** individually, in the order of §1, after restoring what the
failed member had already changed:

| Failed member | Restore before rerunning | Then run, in order |
| --- | --- | --- |
| READ* | nothing (read-only) | the failed print job and the ones after it |
| `POSTTRAN` | `transaction`, `account`, `tran_cat_balance` (each daily record commits on its own) to their state before the cycle (database backup, or `--job=repro` of `--job=unload` dumps taken before the window) | `posttran`, `intcalc`, `tranbkp`, `combtran`, `tranrept`, `creastmt`, `prtcatbl` |
| `INTCALC` | table mode (the cycle): nothing — STEP15 runs in one transaction that is rolled back on failure; file mode (`--ACCTFILE=<path>` etc.): the account and TCATBALF files as POSTTRAN left them (rewrites already done are not undone) | `intcalc` … `prtcatbl` |
| `TRANBKP` | if `transaction` was already emptied: reload it from the newest `TRANSACT.BKUP` generation (`--job=repro --DATASET=TRANSACT --INFILE=<file>`) | `tranbkp` … `prtcatbl` |
| `COMBTRAN` | nothing: on RC 12 (duplicate key) nothing was loaded; fix the inputs | `combtran` … `prtcatbl` |
| `TRANREPT`, `CREASTMT`, `PRTCATBL` | nothing (outputs are new generations; a failed one is rolled back) | the failed stream and the ones after it |

```bash
# dumps to take before the window (one per updated dataset)
java -jar carddemo-app.jar --job=unload --DATASET=ACCTDATA --OUTFILE=/backup/ACCTDATA.before
java -jar carddemo-app.jar --job=unload --DATASET=TCATBALF --OUTFILE=/backup/TCATBALF.before
java -jar carddemo-app.jar --job=unload --DATASET=TRANSACT --OUTFILE=/backup/TRANSACT.before
# restore one
java -jar carddemo-app.jar --job=repro --DATASET=ACCTDATA --INFILE=/backup/ACCTDATA.before --mode=replace
# rerun one member for the same business date
java -jar carddemo-app.jar --job=intcalc --run-date=2022-07-06; echo "RC=$?"
```

`posttran` also reads `daily_transaction` (DALYTRAN); rejects go to a new `DALYREJS` generation only on a normal step
end. Each rerun writes new dated generations; downstream `(0)` lookups pick the newest one of the business date, so
rerun with the same `--run-date`.

## 6. Where outputs land

`<carddemo.batch.output-dir>/<GDG base>/<base>.<business date>.<job execution id>` (default `batch-output/`; in
compose the `carddemo-batch-output` volume at `/app/batch-output`), cataloged in `batch_output_file`, newest 5 kept
per base. SYSOUT: `<output-dir>/SYSOUT/<job>.<run-date>.<execution id>.txt`. Tables updated in place:
`transaction`, `account`, `tran_cat_balance` (and `daily_transaction` is read). The on-demand report of the online
CORPT00C screen runs the same `tranrept` stream with the requested window.

## 7. Online/batch overlap (nightly window)

The legacy schedule closed the CICS files (`CLOSEFIL`) before batch; the Java app keeps the online API up. Online
writes during the cycle are protected per row (optimistic `version`, `SELECT ... FOR UPDATE`), but:

- **Transaction ids (closed in s6.4).** Online transaction adds and bill payments take
  `pg_advisory_xact_lock(TRAN_ID_LOCK)` and use `max(tran_id)+1`. POSTTRAN's TRANFILE writes (the OPEN OUTPUT clear
  and every posting, held until that record's unit of work commits) and every load of `transaction` through
  `VsamDatasetLoader` (COMBTRAN's IDCAMS REPRO, `repro`, `initial-load`) now take the same lock, so an online add can
  no longer read a stale maximum while a batch row is in flight (`TransactionIdLockIT`). Cost: one extra round trip
  per posted record (`docs/validation/hardening/volume-smoke.md`). What the lock cannot change is the legacy data
  flow below.
- **The batch window: `TRANBKP` → `COMBTRAN` empties `transaction`.** TRANBKP's REPRO copies `transaction` to the
  `TRANSACT.BKUP` generation and its IDCAMS DELETE/DEFINE leaves the table **empty**; COMBTRAN sorts the backup with
  `SYSTRAN` and reloads the table (`idcams-repro`). In between, transaction lists, views and the TRANREPT report see
  no transactions. POSTTRAN also opens TRANFILE OUTPUT, i.e. it replaces the table with its postings. Consequences
  for online work during the cycle:
  - an online transaction add or bill payment made after TRANBKP's backup and before COMBTRAN's reload is **lost**
    (COMBTRAN reloads the backup, not the live table) or, if its id is also in the backup, makes COMBTRAN's REPRO
    fail on a duplicate key (RC 12);
  - an add made before POSTTRAN's TRANFILE open is removed by that open (legacy OPEN OUTPUT semantics).

  The legacy schedule avoided both by closing the CICS files (`CLOSEFIL`) for the window. Operate the same way:
  schedule `CARDDEMO_NIGHTLY_CYCLE_CRON` after the online day, and keep online transaction adds and bill payments
  out of the window (maintenance banner or a proxy rule on `POST /api/v1/transactions` and
  `POST /api/v1/accounts/*/bill-payment` while a `nightly-cycle` `batch_run` row is `STARTED`). The window lasts from
  the start of `tranbkp` to the end of `combtran` (seconds on the sample data; see `batch_run` timestamps).
- Reports requested online queue behind a running `tranrept` only within the report executor, not behind the cycle.

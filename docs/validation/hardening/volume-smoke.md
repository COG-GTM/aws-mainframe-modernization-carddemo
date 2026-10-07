# Volume smoke: 100,000 DALYTRAN records (s6.4)

`make volume-smoke` (= `scripts/volume/run_volume_smoke.sh [out-dir]`, default `build/volume-smoke/`) — one command,
not in CI (≈ 2.5 min plus the jar build). It starts a fresh `postgres:16-alpine` with `pg_stat_statements`, generates
the input with `scripts/volume/gen_dalytran.py`, then runs the batch CLI in **table mode** with `-Xmx512m`, each
launch under `/usr/bin/time -v`:

1. `--job=initial-load --mode=REPLACE` (sample VSAM data, Flyway V1–V6)
2. `--job=repro --DATASET=DALYTRAN` (the 100,000 generated records into `daily_transaction`)
3. `--job=posttran --run-date=2022-07-06` (CBTRN01C + CBTRN02C)
4. `--job=intcalc --PARM=2022071800`

and fails if an exit code or a count differs from `<out>/dalytran.txt.expected.json`. Results: `<out>/metrics.md`
(this page copies two runs of it).

## Input

`gen_dalytran.py --records 100000 --seed 20221006`: byte-for-byte deterministic 350-byte CVTRA06Y records with
ascending 16-digit ids, cards drawn from the 50 CARDXREF cards, amounts sized so each account's credit headroom
holds; **~1 % rejects**: 400 × reason 100 (card not in XREF), 300 × 102 (over limit), 300 × 103 (card expired at
the original timestamp). Expected: 99,000 posted, 1,000 rejected, POSTTRAN RC 4.

## Results (host: 8 vCPU, 31 GiB, Docker PostgreSQL 16 on the same host, JDK 21.0.12)

| Job | RC | Before: wall clock (s) | After: wall clock (s) | Before: peak RSS (MiB) | After: peak RSS (MiB) | `batch_run` read / write |
| --- | --- | --- | --- | --- | --- | --- |
| `initial-load` | 0 | 4.8 | 4.5 | 509 | 513 | 637 / 636 |
| `repro` DALYTRAN | 0 | 7.3 | 7.0 | 653 | 560 | 100,000 / 100,000 |
| `posttran` | 4 | **125.5** | **104.3** | 595 | 589 | cbtrn01c 100,000 / 0; cbtrn02c 100,000 / 99,000 |
| `intcalc` | 0 | 4.4 | 4.3 | 410 | 477 | cbact04c 100 / 50 |

Counts after the run (both): `daily_transaction` 100,000; `transaction` 99,000 (posted); DALYREJS generation 1,000
records; `tran_cat_balance` 100 rows; SYSTRAN generation 50 interest records (INTCALC writes SYSTRAN as a GDG file,
COMBTRAN loads it into `transaction`). Peak RSS stays ≈ 0.6 GiB with a 512 MiB heap: POSTTRAN pages DALYTRAN
(`KsdsInput`, `PAGE_SIZE` rows) and only the 1,000 rejects are buffered (`BufferedSink`).

**Before** = commit `cb93598` (per-record `pg_advisory_xact_lock(TRAN_ID_LOCK)` before each TRANFILE write).
**After** = this PR (`TransactionIdStepLock`: the same key held once as a session lock for the CBTRN02C step).
The change is the concurrency fix itself (`rules/CBTRN02C.md` D-1: a per-record lock still let an online `max+1`
equal the next ascending DALYTRAN id); it also removes 99,000 lock round trips: **−17 % POSTTRAN wall clock**.

## Top 3 SQL statements (pg_stat_statements, reset before each job, by total execution time)

POSTTRAN, after:

| Calls | Total ms | Mean ms | Statement |
| --- | --- | --- | --- |
| 99,000 | 4,697.7 | 0.047 | `update account set active_status=$1, ... curr_bal=$5, ...` (`2800-UPDATE-ACCOUNT-REC`) |
| 99,000 | 1,735.2 | 0.018 | `insert into transaction (...)` (`2900-WRITE-TRANSACTION-FILE`) |
| 98,950 | 1,598.2 | 0.016 | `update tran_cat_balance set balance=$1 where acct_id=$2 and tran_type_cd=$3 and tran_cat_cd=$4` (`2700-UPDATE-TCATBAL`) |

POSTTRAN, before: the same three statements (5,320.5 / 1,608.9 / 1,734.8 ms). `repro`: one `insert into
daily_transaction` per record (100,000 calls, 1,020 ms). INTCALC: 49 account updates, 50 account selects
(< 5 ms in total). `initial-load`: Flyway DDL (< 7 ms per statement).

## Reading

- The database is not the bottleneck: the three hottest POSTTRAN statements total ≈ 8 s of the ≈ 104 s, each is a
  primary-key update/insert (mean ≤ 0.05 ms) and each runs **exactly once per posted record** (no N+1). No index
  is missing: every hot statement is keyed by the primary key.
- The rest is the per-record unit of work: CBTRN02C commits each posting on its own (`REQUIRES_NEW`, 100,000
  commits) so a failure leaves the earlier records posted, as COBOL's non-transactional VSAM writes did
  (`rules/CBTRN02C.md`, posting section; ADR-0015 restart rules). JDBC insert batching would need a multi-record
  commit and is therefore **not applied** — it changes failure semantics the equivalence suite relies on.
- ≈ 1,000 postings/s ⇒ a nightly DALYTRAN of 1 M records would take ≈ 17 min for CBTRN02C on this hardware; online
  transaction adds and bill payments wait for that step (runbook §7).

## Not measured

Production-sized ACCTDATA/CARDXREF (the generator reuses the 50 sample cards and 50 accounts, so account rows are
hot in cache), a remote database (network round trips would scale the per-record cost), COMBTRAN/TRANREPT/CREASTMT
at 100k (they run in the golden set at sample volume only), and concurrent online load during the run
(`TransactionIdLockIT` covers correctness, not throughput).

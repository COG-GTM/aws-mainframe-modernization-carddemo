# 07 - Runbook (build, test, run)

Every command below was executed on 2026-11-19 from the repo root on the Devin VM (Ubuntu, GnuCOBOL 3.1.2.0 with BDB, Java 21 at `/usr/lib/jvm/java-21-openjdk-amd64`). Scratch outputs go under `~/work/` (never under `app/`). `OUT` dirs must not exist: the scripts never delete anything.

## Toolchain

| Id | Command | Observed |
|---|---|---|
| R-01 | `cobc --version \| head -1 ; cobc --info \| grep 'indexed file handler'` | `cobc (GnuCOBOL) 3.1.2.0`; `BDB` |
| R-02 | `/usr/lib/jvm/java-21-openjdk-amd64/bin/java -version` | OpenJDK 21 |
| R-03 | `cobc -fsyntax-only -std=ibm -I app/cpy app/cbl/CBTRN02C.cbl` | exit 0 |

## GnuCOBOL oracle for POSTTRAN

`.migration/tools/posttran_gnucobol_oracle.sh [OUT] [DALYTRAN]` compiles a `CEE3ABD` stub (`cobc -m`), six loaders/unloaders, and CBTRN02C (`-std=ibm -fsign=EBCDIC -I app/cpy`); loads `app/data/ASCII/{acctdata,cardxref,tcatbal}.txt` into BDB indexed files; writes DALYTRAN as fixed 350-byte records; binds DDs with `DD_<DDNAME>` and `COB_LIBRARY_PATH`; pins the clock with `COB_CURRENT_DATE` (env `ORACLE_NOW`, default `2022/07/18 10:30:45`); writes `OUT/expected/{DALYREJS,SYSOUT,RETURN-CODE,TRANSACT.unl,ACCTDATA.unl,TCATBALF.unl}`.

| Id | Command | Observed |
|---|---|---|
| R-04 | `.migration/tools/posttran_gnucobol_oracle.sh ~/work/oracle` | RC 4; `TRANSACTIONS PROCESSED :000000300`, `REJECTED :000000038` (all reason `0102 OVERLIMIT TRANSACTION`); DALYREJS 16340 bytes (38 × 430); TRANSACT.unl 262 rows, ACCTDATA.unl 50, TCATBALF.unl 100. Full sha256 in the R-04 note below |
| R-05 | `.migration/tools/posttran_gnucobol_oracle.sh ~/work/o-b ; diff <(cut -c1-324,327- ~/work/oracle/expected/TRANSACT.unl) <(cut -c1-324,327- ~/work/o-b/expected/TRANSACT.unl)` | no diff: deterministic except TRAN-PROC-TS centiseconds (record cols 325–326), which `COB_CURRENT_DATE` does not pin |
| R-06 | `head -2 app/data/ASCII/dailytran.txt > ~/work/dup.txt; head -1 app/data/ASCII/dailytran.txt >> ~/work/dup.txt; .migration/tools/posttran_gnucobol_oracle.sh ~/work/o-dup ~/work/dup.txt` | RC 231 (= abend U999 mod 256): duplicate TRAN-ID on TRANSACT write → status 22 → `9999-ABEND-PROGRAM` |
| R-07 | `python3 .migration/tools/posttran_synthetic_cases.py ~/work/oracle/expected/DALYREJS ~/work/syn && .migration/tools/posttran_gnucobol_oracle.sh ~/work/syn-all ~/work/syn/syn_all.txt && .migration/tools/posttran_gnucobol_oracle.sh ~/work/syn-ok ~/work/syn/syn_ok.txt` (needs R-04) | syn_all: RC 4, 6 processed / 3 rejected: `0100 INVALID CARD NUMBER FOUND`, `0103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION` ×2 (second one is also overlimit: 103 overwrites 102); SYSOUT `TCATBAL record not found for key : 00000000007010099.. Creating.`; TCATBALF 51 rows. syn_ok: RC 0 |
| R-08 | `: > ~/work/empty.txt; .migration/tools/posttran_gnucobol_oracle.sh ~/work/o-empty ~/work/empty.txt` | RC 0, `PROCESSED :000000000`, empty DALYREJS |
| R-09 | `DALYTRAN_ENCODING=CP037 .migration/tools/posttran_gnucobol_oracle.sh ~/work/o-cp037 app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS; cmp ~/work/o-cp037/expected/DALYREJS ~/work/oracle/expected/DALYREJS` | RC 4; DALYREJS byte-identical to the ASCII run |

R-04 note, full sha256 of the baseline: DALYREJS `b6ef92c07bee6372a6046bc0f65c7ee4afb75178a8ba91e9383fb74932b88410`; ACCTDATA.unl `8b3fc8d1977cc728181646fbad7509027d1e469b67327c7c7ad71bc8cc48d65c`; TCATBALF.unl `b2a8654114a60e095b8c0a5fc4276a9def34494e240d9c7a973096227730df5f`; TRANSACT.unl `4dbcdf24193bdbf82d743d5a14f0b120d1c2ce5933258a3c83ffba8bb0ca8a18` (centiseconds vary, compare with R-05 masking).

## Inventory regeneration

| Id | Command | Observed |
|---|---|---|
| R-10 | `python3 functional/CARDDEMO/inventory/callgraph.py . > functional/CARDDEMO/inventory/edges.tsv` | see `CardDemo_inventory.md` §2 |
| R-11 | `python3 functional/CARDDEMO/inventory/coverage.py .` | exit 0, coverage identity holds (see inventory §3) |
| R-12 | `npx -p @mermaid-js/mermaid-cli@10.9.1 mmdc -i functional/CARDDEMO/diagrams/posttran_module_flow.mmd -o functional/CARDDEMO/diagrams/posttran_module_flow.png` | PNG rendered |

## Java build / run

Executed from the repo root on the Devin VM (Maven 3.9.9, Docker Engine 29.7.2). Plain `mvn` picks Java 17 on this VM: always set `JAVA_HOME`.

| Id | Command | Observed |
|---|---|---|
| R-13 | `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` | BUILD SUCCESS; carddemo-common 6 tests, carddemo-app 6 tests (Testcontainers `postgres:16`), 0 failures |
| R-14 | `(cd modernization && CARDDEMO_DB_PORT=55432 docker compose up -d --build && CARDDEMO_DB_PORT=55432 docker compose run --rm app --job=NOOP; echo $?; CARDDEMO_DB_PORT=55432 docker compose run --rm app --job=NOSUCH; echo $?; CARDDEMO_DB_PORT=55432 docker compose down -v)` (needs R-13 jar) | `app` service exits 0 (`Job NOOP ... status=COMPLETED RC=0`); `--job=NOOP` exit 0; `--job=NOSUCH` exit 12 |
| R-15 | `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify -Dtestcontainers.version=1.19.8)` | BUILD FAILURE on purpose (D-030 evidence): `client version 1.32 is too old. Minimum supported API version is 1.40` → `Could not find a valid Docker environment` |
| R-16 | `.migration/tools/posttran_golden_cases.sh ~/work/golden` | writes `modernization/carddemo-app/src/test/resources/golden/posttran/`: `baseline` RC 4 / 38 rejects, `syn-all` RC 4 / 3, `syn-ok` RC 0 / 0, `empty` RC 0 / 0, `cp037` RC 4 / 38, `acct-missing` RC 4 / 2 (`0101 ACCOUNT RECORD NOT FOUND`, account 00000000007 dropped), `dup-id` RC 231 / 0; baseline sha256 equal to R-04 |
| R-17 | `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` (Phase 1) | BUILD SUCCESS; carddemo-common 14 tests, carddemo-app 26 tests (incl. `KsdsDataServiceTest` on `postgres:16`, `ParityHarnessTest`, `DailyTransactionReaderTest`, `DailyRejectWriterTest`), 0 failures |
| R-18 | `.migration/tools/posttran_golden_cases.sh ~/work/golden2 ~/work/golden2-dest`, then `cmp`/masked `diff` of every file against the committed cases | regeneration reproduces every committed golden file (TRANSACT.unl equal with cols 325–326 masked); adds `dup-id/expected-atomic` (oracle on records 1–2: RC 0, account 7 = 697.77) copied into the repo (D-042) |
| R-19 | `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` (Phase 1 after D-042) | BUILD SUCCESS; carddemo-common 14 tests, carddemo-app 27 tests, 0 failures |
| R-20 | `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` (Wave 1) | BUILD SUCCESS; carddemo-common 21 tests, carddemo-app 55 tests, 0 failures. `PosttranJobParityTest` green on all 7 golden cases (DALYREJS bytes, ACCTDATA/TCATBALF/TRANSACT, SYSOUT, RC 4/4/4/16/0/4/0); `PosttranJobRunTest` restart / rerun / exit-12 / exit-16 cases green |
| R-21 | `docker run -d --rm --name pt-pg -e POSTGRES_DB=carddemo -e POSTGRES_USER=carddemo -e POSTGRES_PASSWORD=carddemo -p 55433:5432 postgres:16`, then `java -jar modernization/carddemo-app/target/carddemo-app.jar --job=POSTTRAN --run-id=CLI-1 --dd=DALYTRAN=modernization/carddemo-app/src/test/resources/golden/posttran/baseline/input/DALYTRAN --dd=DALYREJS=~/work/cli/DALYREJS --dd=SYSOUT=~/work/cli/SYSOUT --spring.datasource.url=jdbc:postgresql://localhost:55433/carddemo` (empty masters; needs R-20 jar) | exit 4; SYSOUT `TRANSACTIONS PROCESSED :000000300`, `REJECTED  :000000300`; DALYREJS 129000 bytes, all `0100INVALID CARD NUMBER FOUND`. Relaunch of `--run-id=CLI-1` → exit 12 (completed); new `--run-id=CLI-2` with the same DALYREJS → exit 12 (exists) |

## Running POSTTRAN (Wave 1)

```
java -jar carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=<file> [--dd=DALYREJS=<file>] [--dd=SYSOUT=<file>] [--run-id=<id>] \
     [--carddemo.posttran.dalytran-encoding=IBM037] [--carddemo.batch.output-dir=<dir>] [--carddemo.batch.clock=2022-07-18T10:30:45] \
     [--spring.datasource.url=jdbc:postgresql://<host>:<port>/carddemo --spring.datasource.username=... --spring.datasource.password=...]
```

- Exit code: 0 no reject, 4 rejects, 12 launch error (unknown job, DALYTRAN not bound, DALYREJS exists, completed `--run-id`), 16 abend.
- New instance (new or omitted `--run-id`): `transaction.transactions` is emptied first (D-024), then DALYTRAN is posted. DALYREJS defaults to `<output-dir>/POSTTRAN/<run-id>/DALYREJS` and must not exist.
- Restart after exit 16: relaunch with the same `--run-id` (and the same DALYREJS). Processing resumes after the last committed record; DALYREJS is truncated to its committed length; nothing is reposted.
- Masters (accounts, card xref, TCATBAL) must already be loaded; tests load them with `KsdsDataService` (D-021).

## Gotchas (verified)
- Sample ASCII files use EBCDIC-style overpunch: compile with `-fsign=EBCDIC`.
- `COB_CURRENT_DATE` pins to the second only.
- Testcontainers 1.19.x (Boot 3.3.4 default) fails against Docker Engine 29; the parent POM pins 1.21.4 (D-030).
- CBTRN02C opens TRANSACT `OUTPUT` (`app/cbl/CBTRN02C.cbl:256`): a rerun on the same files recreates TRANSACT but reposts ACCTDATA/TCATBALF. Always run the oracle in a fresh OUT.

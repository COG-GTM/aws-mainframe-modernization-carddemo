# 07 - Runbook (build, test, run)

Rule: **every command marked VERIFIED was executed successfully in session devin-c4fce2ffab2c4abaac2aa02e217e00e4 on 2026-11-19** (or in the session named in the section heading) (Ubuntu VM, repo root = cwd). Commands that cannot run yet are marked **NOT YET RUNNABLE** with the reason; they are the Phase 0 targets, not facts.

## Toolchain (VERIFIED)

| ID | Command | Observed |
|---|---|---|
| R-01 | `cobc --version \| head -1` ; `cobc --info \| grep 'indexed file handler'` | `cobc (GnuCOBOL) 3.1.2.0`; indexed handler `BDB` |
| R-02 | `JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -v` | Maven 3.9.9, Java 21.0.12.1. Plain `java` on PATH is 17: always set `JAVA_HOME` |
| R-03 | `docker ps` ; `docker pull postgres:16` | Docker daemon up; `postgres:16` pulled (needed by Testcontainers and the local DB profile) |

## COBOL oracle for POSTTRAN (VERIFIED)

| ID | Command | Observed |
|---|---|---|
| R-04a | `cobc -fsyntax-only -std=ibm -I app/cpy app/cbl/CBTRN02C.cbl` | exit 0 |
| R-04 | `.migration/tools/posttran_gnucobol_oracle.sh [OUT_DIR] [DALYTRAN_ASCII]` (relative or absolute paths, resolved before the script changes directory; verified from the repo root with `../../work/oracle-rel app/data/ASCII/dailytran.txt`, outputs identical to the absolute-path run; `DALYTRAN_ENCODING=CP037` reads RECFM=FB EBCDIC input, see R-14; OUT_DIR must not exist or be empty, else exit 2 and nothing is deleted; defaults: `/tmp/carddemo-posttran-oracle`, `app/data/ASCII/dailytran.txt`, clock `ORACLE_NOW='2022/07/18 10:30:45'`) | `RC=4`; SYSOUT ends `TRANSACTIONS PROCESSED :000000300` / `TRANSACTIONS REJECTED  :000000038`; `DALYREJS` 16340 bytes (= 38 x 430), all 38 rejects reason `0102 OVERLIMIT TRANSACTION`; `TRANSACT.unl` 262 rows (= 300 - 38); `ACCTDATA.unl` 50; `TCATBALF.unl` 100. sha256: DALYREJS `b6ef92c0...8410`, ACCTDATA.unl `8b3fc8d1...d65c`, TCATBALF.unl `b2a86541...df5f`, SYSOUT `185cb77e...c19e` |
| R-05 | `.migration/tools/posttran_gnucobol_oracle.sh /tmp/o-a` ; `.migration/tools/posttran_gnucobol_oracle.sh /tmp/o-b` ; `diff <(cut -c1-324,327- /tmp/o-a/expected/TRANSACT.unl) <(cut -c1-324,327- /tmp/o-b/expected/TRANSACT.unl)` | every output except TRANSACT.unl has the same sha256 in both runs; TRANSACT differs only in TRAN-PROC-TS hundredths (`2022-07-18-10.30.45.52xxxx` vs `.53xxxx`); masked diff is empty (determinism rule D-007) |
| R-06 | duplicate TRAN-ID case: `head -2 app/data/ASCII/dailytran.txt > /tmp/dup.txt; head -1 app/data/ASCII/dailytran.txt >> /tmp/dup.txt; .migration/tools/posttran_gnucobol_oracle.sh /tmp/o-dup /tmp/dup.txt` | `RC=231`; SYSOUT `ERROR WRITING TO TRANSACTION FILE` / `FILE STATUS IS: NNNN0022` / `ABENDING PROGRAM` / `CEE3ABD ABCODE=+0000000999` (231 = 999 mod 256; maps to Java RC 16) |
| R-07 | rerun on already-posted masters: run `bin/CBTRN02C` from an R-04 output dir again with the same `DD_*` variables (see script for the list) and `COB_CURRENT_DATE` | `RC=4`, 300 processed / 113 rejected, TRANSACT unload 187 rows (file emptied by `OPEN OUTPUT`, `app/cbl/CBTRN02C.cbl:256`), ACCTDATA changed again (double posting). Evidence for OQ-1 |

How the script works (read it before changing it): compiles a `CEE3ABD` stub (`cobc -m`), six tiny loaders/unloaders (line-sequential <-> indexed, key at offset 0), and CBTRN02C with `-std=ibm -fsign=EBCDIC`; normalizes `acctdata.txt` (300), `cardxref.txt` (50), `tcatbal.txt` (50, CR stripped) and loads them as indexed files; writes DALYTRAN as 350-byte fixed records without newlines; maps DDs with `DD_<DDNAME>` env vars and `COB_LIBRARY_PATH`; runs with `COB_CURRENT_DATE`; writes `expected/{DALYREJS,SYSOUT,RETURN-CODE,TRANSACT.unl,ACCTDATA.unl,TCATBALF.unl}`. It never writes under `app/`.
| R-11 | `python3 .migration/tools/posttran_synthetic_cases.py /tmp/carddemo-posttran-oracle/expected/DALYREJS /tmp/syn && .migration/tools/posttran_gnucobol_oracle.sh /tmp/syn-all /tmp/syn/syn_all.txt && .migration/tools/posttran_gnucobol_oracle.sh /tmp/syn-ok /tmp/syn/syn_ok.txt` (needs an R-04 run first) | syn_all: `RETURN-CODE` 4, 6 processed / 3 rejected: `0100 INVALID CARD NUMBER FOUND`, `0103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION` x2 (the second record is also overlimit: 103 overwrites 102), SYSOUT `TCATBAL record not found for key : 00000000007010099.. Creating.`, TCATBALF 51 rows; syn_ok: `RETURN-CODE` 0, account 7 CYC-CREDIT +504.77, CYC-DEBIT -504.70, CURR-BAL +0.07 |
| R-12 | `: > /tmp/empty.txt && .migration/tools/posttran_gnucobol_oracle.sh /tmp/syn-empty /tmp/empty.txt` | `RETURN-CODE` 0, `TRANSACTIONS PROCESSED :000000000` / `REJECTED  :000000000`, DALYREJS 0 bytes. R-06 output also shows TRANSACT keeps the 2 records posted before the duplicate (VSAM writes are not rolled back) |
| R-13 | GC-4 abend-case expectation: `head -2 app/data/ASCII/dailytran.txt > /tmp/gc4-prefix.txt; head -2 app/data/ASCII/dailytran.txt > /tmp/gc4-dup.txt; head -1 app/data/ASCII/dailytran.txt >> /tmp/gc4-dup.txt; .migration/tools/posttran_gnucobol_oracle.sh /tmp/gc4p /tmp/gc4-prefix.txt; .migration/tools/posttran_gnucobol_oracle.sh /tmp/gc4d /tmp/gc4-dup.txt` | prefix run `RETURN-CODE` 0, 2 TRANSACT rows; duplicate run `RETURN-CODE` 231, same 2 TRANSACT rows (masked compare equal), but ACCTDATA account 7 and TCATBALF key `00000000007010001` differ: COBOL applied the failing record's account and category updates before the TRANSACT write abended (`app/cbl/CBTRN02C.cbl:440-442`). Under D-009 the Java tables must equal the **prefix** run |
| R-14 | GC-7 CP037 input: `DALYTRAN_ENCODING=CP037 .migration/tools/posttran_gnucobol_oracle.sh /tmp/gc7 app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS` | `RC=4`; DALYREJS, ACCTDATA, TCATBALF, RETURN-CODE and SYSOUT byte-identical to the ASCII GC-1 run (R-04), TRANSACT identical with bytes 325-326 masked; the 38 reject images converted back latin-1 -> CP037 equal the EBCDIC input records byte for byte (the CP037 sample converts record by record to exactly `app/data/ASCII/dailytran.txt`) |

## Inventory regeneration (VERIFIED)

| ID | Command | Observed |
|---|---|---|
| R-08 | `python3 functional/CARDDEMO/inventory/callgraph.py . functional/CARDDEMO/inventory > functional/CARDDEMO/inventory/edges.tsv` | 276 edges + header (see inventory) |
| R-09 | `python3 functional/CARDDEMO/inventory/coverage.py . functional/CARDDEMO/inventory` | exit 0; `CHECK 46 = 37 + 8 + 1 -> True`, `CHECK-A` 41 declared / 40 roots (1 cited exception), `CHECK-B` 0 unattributed, `CHECK-C` CBTRN01C clean, `CHECK-D` 19 unresolved / 0 without resolution / 0 inconsistent, `INDEPENDENT CHECKS PASS` |
| R-10 | `cd functional/CARDDEMO/inventory && <mermaid-cli>/mmdc -p <puppeteer.json> -i module_flow.mmd -o module_flow.png` with `@mermaid-js/mermaid-cli@10.9.1` installed under `~/work/mmd` (`PUPPETEER_SKIP_DOWNLOAD=1 npm i @mermaid-js/mermaid-cli@10.9.1`) and `puppeteer.json` = `{"executablePath":"/opt/.devin/chrome/chrome/linux-137.0.7118.2/chrome-linux64/chrome","args":["--no-sandbox"]}` | PNG rendered |

## Java build, test, run (VERIFIED in Phase 0, session devin-8bcaa62b3adc402888e545635bcac91b, 2026-11-19)

| ID | Command | Observed |
|---|---|---|
| R-15 (was N-01) | `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` | `BUILD SUCCESS`; carddemo-common 71 tests, carddemo-app 31 tests (Testcontainers `postgres:16`, one shared container), 0 failures (Phase 1, 102 total). Builds `modernization/carddemo-app/target/carddemo-app.jar`. Becomes the golden-master gate once N-05 lands |
| R-16 (was N-03) | `docker run --rm -d --name carddemo-pg -e POSTGRES_DB=carddemo -e POSTGRES_PASSWORD=postgres -p 5432:5432 postgres:16` ; `java -jar modernization/carddemo-app/target/carddemo-app.jar --spring.profiles.active=local` (Java 21; `local` defaults: `jdbc:postgresql://localhost:5432/carddemo`, user/password `postgres`, override with `CARDDEMO_DB_URL` / `CARDDEMO_DB_USER` / `CARDDEMO_DB_PASSWORD`) ; `docker exec carddemo-pg psql -U postgres -d carddemo -Atc "select version,success from flyway_schema_history"` | exit 0, `Started CardDemoApplication`; Flyway history `0\|t` (`V0__baseline.sql` only), `flyway_schema_history` is the only table in `public` |
| R-17 (was N-02, launch seam only) | with R-16's container up: `java -jar modernization/carddemo-app/target/carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=app/data/ASCII/dailytran.txt --output-dir=<dir> --spring.profiles.active=local` ; and without `--job` value: `java -jar ... --job=` | exit **12** with `Unknown job POSTTRAN; registered: []` (Phase 0 registers no job); `--job=` exits **12** with `--job is required` + usage line. The real POSTTRAN run (RC 0/4/16 + generation dir) is N-02 below |
| R-18 (was N-04) | CI: `.github/workflows/modernization-verify.yml` (Temurin 21, `docker info` check, `mvn -B verify` in `modernization/`, surefire reports uploaded) on pull requests touching `modernization/**`, `app/data/**` or the workflow | see PR CI run (Phase 0 PR) |
| R-19 (Phase 1) | with R-16's container up: `java -jar modernization/carddemo-app/target/carddemo-app.jar --job=LOAD --dd=ACCTDATA=app/data/ASCII/acctdata.txt --dd=CARDXREF=app/data/ASCII/cardxref.txt --dd=TCATBALF=app/data/ASCII/tcatbal.txt --spring.profiles.active=local` (EBCDIC: the `app/data/EBCDIC/AWS.M2.CARDDEMO.{ACCTDATA,CARDXREF,TCATBALF}.PS` files with `--dd-encoding=CP037`) | exit 0, `LOADED ACCTDATA : 50`, `LOADED CARDXREF : 50`, `LOADED TCATBALF : 50`; Flyway at V6. Running it again exits 16 (duplicate key, one transaction, nothing loaded). Verified 2026-11-19, session devin-8bcaa62b3adc402888e545635bcac91b |
| R-20 (Phase 1) | GC-6 oracle (custom CARDXREF): `CARDXREF=modernization/carddemo-app/src/test/resources/golden/POSTTRAN/GC-6/input/CARDXREF.txt .migration/tools/posttran_gnucobol_oracle.sh /tmp/gc6 modernization/carddemo-app/src/test/resources/golden/POSTTRAN/GC-6/input/DALYTRAN.txt` | `RC=4`; DALYREJS 430 bytes, trailer `0101ACCOUNT RECORD NOT FOUND`; TRANSACT 1 record (the valid one). `CARDXREF` defaults to `app/data/ASCII/cardxref.txt`, so R-04..R-14 are unchanged |
| R-21 (Phase 1) | golden fixtures: `modernization/carddemo-app/src/test/resources/golden/POSTTRAN/GC-1..GC-7/{input,expected}` (GC-4 also `expected-prefix`), regenerated with R-04/R-11/R-12/R-13/R-14/R-20 (`ORACLE_NOW` default); harness `com.carddemo.app.golden.{GoldenCase,GoldenCompare,TableUnloader}` runs inside R-15 | `GoldenHarnessTest`, `DataLoaderTest` (load + unload == GC-5 oracle unloads), `RejectFileWriterTest` (replaying the oracle rejects of GC-1/2/6/7 reproduces DALYREJS sha256, GC-7 in CP037) green |

## Java build, test, run (NOT YET RUNNABLE)

| ID | Command | Status / reason |
|---|---|---|
| N-02 | `java -jar modernization/carddemo-app/target/carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=app/data/ASCII/dailytran.txt --output-dir=<dir>` producing DALYREJS, SYSOUT, RETURN-CODE | **NOT YET RUNNABLE**: the jar and launch seam exist (R-17) but no POSTTRAN job is registered; Wave 1 owns it |
| N-05 | golden-master comparison of a Java POSTTRAN run against GC-1..GC-7 (tables, DALYREJS, RC, SYSOUT counts) | **NOT YET RUNNABLE**: fixtures and harness exist (R-21); a full run needs the Wave 1 job and the TRANSACT unload needs V7, both waiting on OQ-7 |

## Not usable on this VM

- `scripts/run_posting.sh`, `scripts/run_full_batch.sh`, `scripts/remote_*.sh`: submit JCL over an FTP tunnel to the original host (`tnftp localhost 2121`); no mainframe here.
- CICS online screens, IMS, DB2, MQ: no runtime available; online streams are source-analysis only.
- `faketime`: `sudo apt-get install faketime` failed (archive.ubuntu.com unreachable); use `COB_CURRENT_DATE` (D-007).

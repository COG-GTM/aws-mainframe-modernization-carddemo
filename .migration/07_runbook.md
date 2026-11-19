# 07 - Runbook (build, test, run)

Rule: **every command marked VERIFIED was executed successfully in session devin-c4fce2ffab2c4abaac2aa02e217e00e4 on 2026-11-19** (Ubuntu VM, repo root = cwd). Commands that cannot run yet are marked **NOT YET RUNNABLE** with the reason; they are the Phase 0 targets, not facts.

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
| R-04 | `.migration/tools/posttran_gnucobol_oracle.sh [OUT_DIR] [DALYTRAN_ASCII]` (defaults: `/tmp/carddemo-posttran-oracle`, `app/data/ASCII/dailytran.txt`, clock `ORACLE_NOW='2022/07/18 10:30:45'`) | `RC=4`; SYSOUT ends `TRANSACTIONS PROCESSED :000000300` / `TRANSACTIONS REJECTED  :000000038`; `DALYREJS` 16340 bytes (= 38 x 430), all 38 rejects reason `0102 OVERLIMIT TRANSACTION`; `TRANSACT.unl` 262 rows (= 300 - 38); `ACCTDATA.unl` 50; `TCATBALF.unl` 100. sha256: DALYREJS `b6ef92c0...8410`, ACCTDATA.unl `8b3fc8d1...d65c`, TCATBALF.unl `b2a86541...df5f`, SYSOUT `185cb77e...c19e` |
| R-05 | `.migration/tools/posttran_gnucobol_oracle.sh /tmp/o-a` ; `.migration/tools/posttran_gnucobol_oracle.sh /tmp/o-b` ; `diff <(cut -c1-324,327- /tmp/o-a/expected/TRANSACT.unl) <(cut -c1-324,327- /tmp/o-b/expected/TRANSACT.unl)` | every output except TRANSACT.unl has the same sha256 in both runs; TRANSACT differs only in TRAN-PROC-TS hundredths (`2022-07-18-10.30.45.52xxxx` vs `.53xxxx`); masked diff is empty (determinism rule D-007) |
| R-06 | duplicate TRAN-ID case: `head -2 app/data/ASCII/dailytran.txt > /tmp/dup.txt; head -1 app/data/ASCII/dailytran.txt >> /tmp/dup.txt; .migration/tools/posttran_gnucobol_oracle.sh /tmp/o-dup /tmp/dup.txt` | `RC=231`; SYSOUT `ERROR WRITING TO TRANSACTION FILE` / `FILE STATUS IS: NNNN0022` / `ABENDING PROGRAM` / `CEE3ABD ABCODE=+0000000999` (231 = 999 mod 256; maps to Java RC 16) |
| R-07 | rerun on already-posted masters: run `bin/CBTRN02C` from an R-04 output dir again with the same `DD_*` variables (see script for the list) and `COB_CURRENT_DATE` | `RC=4`, 300 processed / 113 rejected, TRANSACT unload 187 rows (file emptied by `OPEN OUTPUT`, `app/cbl/CBTRN02C.cbl:256`), ACCTDATA changed again (double posting). Evidence for OQ-1 |

How the script works (read it before changing it): compiles a `CEE3ABD` stub (`cobc -m`), six tiny loaders/unloaders (line-sequential <-> indexed, key at offset 0), and CBTRN02C with `-std=ibm -fsign=EBCDIC`; normalizes `acctdata.txt` (300), `cardxref.txt` (50), `tcatbal.txt` (50, CR stripped) and loads them as indexed files; writes DALYTRAN as 350-byte fixed records without newlines; maps DDs with `DD_<DDNAME>` env vars and `COB_LIBRARY_PATH`; runs with `COB_CURRENT_DATE`; writes `expected/{DALYREJS,SYSOUT,RETURN-CODE,TRANSACT.unl,ACCTDATA.unl,TCATBALF.unl}`. It never writes under `app/`.

## Inventory regeneration (VERIFIED)

| ID | Command | Observed |
|---|---|---|
| R-08 | `python3 functional/CARDDEMO/inventory/callgraph.py . functional/CARDDEMO/inventory > functional/CARDDEMO/inventory/edges.tsv` | 272 edges + header (see inventory) |
| R-09 | `python3 functional/CARDDEMO/inventory/coverage.py . functional/CARDDEMO/inventory` | `CHECK 46 = 37 + 8 + 1 -> True` |
| R-10 | `cd functional/CARDDEMO/inventory && <mermaid-cli>/mmdc -p <puppeteer.json> -i module_flow.mmd -o module_flow.png` with `@mermaid-js/mermaid-cli@10.9.1` installed under `~/work/mmd` (`PUPPETEER_SKIP_DOWNLOAD=1 npm i @mermaid-js/mermaid-cli@10.9.1`) and `puppeteer.json` = `{"executablePath":"/opt/.devin/chrome/chrome/linux-137.0.7118.2/chrome-linux64/chrome","args":["--no-sandbox"]}` | PNG rendered |

## Java build, test, run (NOT YET RUNNABLE - `modernization/` does not exist on `main`; Phase 0 creates it)

| ID | Command | Status / reason |
|---|---|---|
| N-01 | `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` | **NOT YET RUNNABLE**: executed, fails with `cd: modernization: No such file or directory`. Becomes the unit + Testcontainers + golden-master gate |
| N-02 | `java -jar modernization/carddemo-app/target/carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=app/data/ASCII/dailytran.txt --output-dir=<dir>` | **NOT YET RUNNABLE**: no jar. Contract from target state BATCH profile (exit code = JCL RC) |
| N-03 | local DB profile: `docker run --rm -d --name carddemo-pg -e POSTGRES_DB=carddemo -e POSTGRES_PASSWORD=<local> -p 5432:5432 postgres:16` + Flyway on app start | **NOT YET RUNNABLE** as a profile (no app, no Flyway scripts); only the image pull (R-03) is verified |
| N-04 | CI: `.github/workflows/modernization-verify.yml` (Temurin 21, Docker check, `mvn -B verify`) | **NOT YET RUNNABLE**: workflow not created yet (Phase 0) |
| N-05 | golden-master comparison in Java tests against R-04 outputs | **NOT YET RUNNABLE**: Phase 1/2 |

## Not usable on this VM

- `scripts/run_posting.sh`, `scripts/run_full_batch.sh`, `scripts/remote_*.sh`: submit JCL over an FTP tunnel to the original host (`tnftp localhost 2121`); no mainframe here.
- CICS online screens, IMS, DB2, MQ: no runtime available; online streams are source-analysis only.
- `faketime`: `sudo apt-get install faketime` failed (archive.ubuntu.com unreachable); use `COB_CURRENT_DATE` (D-007).

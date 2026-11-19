# 01 - Target state (summary copy)

Authoritative document: [`functional/CARDDEMO/CardDemo_target_state.md`](../functional/CARDDEMO/CardDemo_target_state.md), **CONFIRMED at STOP A on 2026-11-19** (commit `07a8fbb`). If this summary and that file disagree, that file wins; fix this copy.

## Surfaces

| Profile | Status |
|---|---|
| CORE | CONFIRMED |
| ONLINE | **N/A** this run (only POSTTRAN requested). Re-run `!mf_ingest_target_state` before any online stream |
| BATCH | CONFIRMED (applies to POSTTRAN) |
| SUBTRANSACTION | **N/A** (CBTRN02C calls only `CEE3ABD`, `app/cbl/CBTRN02C.cbl:711`) |
| DATA / BOUNDARY | CONFIRMED |

## CORE (essentials)

- Java 21, Spring Boot 3.3.x (parent 3.3.4), Maven multi-module rooted at `modernization/pom.xml`; build `JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify`.
- **Modular monolith**: one deployable `modernization/carddemo-app` + library `modernization/carddemo-common`. Packages `com.carddemo.<context>.{domain,repository,service,batch,api}`, contexts `account, card, customer, transaction, reference`; shared code only in `com.carddemo.common.{cobol,record,batch,error}`.
- Class names follow the business concept; Javadoc cites the COBOL program + paragraph.
- COBOL type mapping = REF `docs/migration/conventions.md` §1-§8 verbatim: money `S9(n)V99` -> `BigDecimal` scale 2 / `NUMERIC(n+2,2)`, truncate unless `ROUNDED`; zoned overpunch, COMP-3, COMP decoders.
- One `RecordLayout` per copybook; `RecordReader`/`RecordWriter` for ASCII line-sequential and EBCDIC CP037 fixed-block.
- Any unexpected FILE STATUS in batch = abend (step fails, job RC 16); never silently skip.
- SLF4J/Logback; observable COBOL `DISPLAY` lines reproduced on SYSOUT; PAN masked to last 4.
- Tests: JUnit 5, AssertJ, Spring Boot Test, `spring-batch-test`, Testcontainers `postgres:16` (pinned 1.21.4), real Flyway, `ddl-auto: validate`, **no H2**.
- Parity oracle: GnuCOBOL golden master (`cobc -std=ibm -I app/cpy`), byte-identical outputs + RC.
- CI: `.github/workflows/modernization-verify.yml` (Temurin 21, Docker check, `mvn -B verify`).
- **Forbidden:** `double`/`float` for money; H2; Lombok; swallowing I/O errors; editing `app/**`; new runtime deps beyond Boot starters, Flyway, PostgreSQL driver, Testcontainers without a `06_decisions.md` entry.

## BATCH (POSTTRAN)

- Spring Batch 5, JobRepository in schema `batch`. One `Job` per JCL member (`POSTTRAN`), one step per `EXEC` (`step15Cbtrn02c`), **chunk-oriented** reader (DALYTRAN file) -> processor (validations 100-103, build TRAN-RECORD) -> writer (TRANSACT insert, ACCT update, TCATBAL upsert, reject write).
- Launch: `java -jar carddemo-app.jar --job=POSTTRAN [--dd=DALYTRAN=<file>] [--output-dir=<dir>]`; exit code = JCL condition code; optional `POST /api/v1/batch/jobs/POSTTRAN`.
- RC: 0 clean; **4** when any reject was written; **16** abend; 12 usage error.
- Datasets: DALYTRAN stays a **file** (CVTRA06Y, ASCII or EBCDIC); TRANSACT, ACCTDATA, CARDXREF, TCATBALF -> PostgreSQL tables; DALYREJS(+1) -> file `<output-dir>/POSTTRAN/<yyyyMMdd-HHmmss-SSS>/DALYREJS` (430-byte fixed records) + SYSOUT in the same generation dir.
- Unit of work: one daily record = one DB transaction across TRANSACT, ACCOUNT, TCATBAL; chunk commit interval default 100.
- Restart from last committed chunk; **full rerun of an already-posted file abends RC 16 on duplicate TRAN-ID** (see open question OQ-1 in `06_decisions.md`: the COBOL opens TRANSACT `OUTPUT`).
- External scheduler replaces CA-7 (`CLOSEFIL -> CBPAUP0J -> POSTTRAN -> WAITSTEP -> OPENFIL`); CLOSEFIL/OPENFIL/WAITSTEP drop out.
- Drift rules (reject the PR if): output differs from the golden master; RC differs; an abend becomes a skip; business rules in reader/writer; a COBOL quirk "fixed" without a decision entry; floating point in the money path.

## DATA / BOUNDARY

- PostgreSQL 16; one DB `carddemo`, schema per context (`account, card, customer, transaction, reference, batch`), cross-schema FKs allowed.
- Spring Data JPA; `@Version` on ACCOUNT and TCATBAL. Flyway `V<n>__<name>.sql` under `carddemo-app/src/main/resources/db/migration`, schema from CVACT01Y, CVACT03Y, CVTRA01Y, CVTRA05Y, CVTRA06Y.
- Loader reads `app/data/ASCII` (or EBCDIC) with the record codec. KSDS key -> PK; AIX -> secondary index.
- No stored procedures, no MQ/HTTP outbound for POSTTRAN; inbound = CLI/HTTP trigger only. No strangler routing: the COBOL is the read-only oracle.

## Cross-profile reconciliation (from the target state)

- C-1 monolith instead of REF's five services (keeps CBTRN02C's three writes in one transaction).
- C-2 type mapping lives in CORE.
- C-3 DALYTRAN / DALYREJS stay files (byte-comparable with GnuCOBOL), unlike REF's staging tables.

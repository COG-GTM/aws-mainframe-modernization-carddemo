# CardDemo target state (CORE + per-surface profiles)

Produced by `!mf_ingest_target_state`, first engagement run. **Status: CONFIRMED at STOP A by the user on 2026-11-19 ("confirm"). Every field previously marked CONFIRMED is now CONFIRMED.**

- **Target (decided by the user, 2026-11-19):** a fresh Java 21 / Spring Batch implementation on `main`, under `modernization/`.
- **Reference repository (evidence only, never a base):** `COG-GTM/aws-mainframe-modernization-carddemo`, branch `migration/carddemo-java` @ `c251be5`, cited below as `REF:`. That branch already ported the module as five Spring Boot services. We reuse its conventions where its own docs mark them as accepted, and we do **not** reuse its service split (see C-1).
- **Marking:** **FACT** = cited from REF, the COBOL source or the user. **CONFIRMED** = a default proposed by Devin and explicitly confirmed by the user at STOP A (2026-11-19). **N/A** = out of scope, with the reason.

## 0. Repository topology (roles)

| Role | Repo | Path | Base branch | Marking |
|---|---|---|---|---|
| SOURCE | COG-GTM/aws-mainframe-modernization-carddemo | `app/cbl`, `app/cpy`, `app/jcl`, `app/proc`, `app/scheduler`, `app/data` | `main` | FACT (repo layout) |
| BACKEND | same repo | `modernization/` (Maven multi-module) | `main` | FACT (user choice "Fresh ... on main") |
| FRONTEND | N/A | - | - | N/A: no UI-bearing stream is in scope (POSTTRAN is BATCH) |
| DOCS | same repo | `functional/CARDDEMO/**`, `.migration/**` | `main` | CONFIRMED |

Single-repo topology, so every wave produces one PR. No look-alike repo is involved. `COG-GTM/mainframe-modernization-carddemo` also exists on the box, but it is **not** used.

## 1. CORE (applies to every surface)

| Field | Value | Marking / source |
|---|---|---|
| Language | Java 21 | FACT: user choice; REF `modernization/pom.xml:33` |
| Framework | Spring Boot 3.3.x (parent `spring-boot-starter-parent` 3.3.4) | CONFIRMED, aligned with REF `modernization/pom.xml:10` |
| Build | Maven multi-module, root `modernization/pom.xml`; `mvn -B verify` with `JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64` | FACT: REF architecture.md §2 "Build"; repo blueprint |
| Application shape | **Modular monolith**: one deployable Spring Boot app, `modernization/carddemo-app`, plus a `modernization/carddemo-common` library (COBOL types, record codec, batch seam). Bounded contexts are packages, not services. | **CONFIRMED**, see C-1 |
| Package layout | `com.carddemo.<context>.{domain,repository,service,batch,api}`. Contexts: `account`, `card`, `customer`, `transaction`, `reference` (trantype/trancatg/discgrp/tcatbal). Shared code only in `com.carddemo.common.{cobol,record,batch,error}` | CONFIRMED, adapted from REF architecture.md §2 "Layout" |
| Naming | Java class names follow the business concept (`DailyTransactionPostingJob`, `PostingService`). Each class Javadoc cites the COBOL program and paragraph it implements (`CBTRN02C 1500-A-LOOKUP-XREF`) | CONFIRMED |
| COBOL type mapping | REF `docs/migration/conventions.md` §1–§8 adopted **verbatim** as the rule. Money `S9(n)V99` → `BigDecimal` scale 2 / `NUMERIC(n+2,2)`, **truncate** unless `ROUNDED`. `PIC 9(n)` id → `Long`/`BIGINT` + CHECK, zero-padded on output. `PIC X(n)` → `String`/`VARCHAR(n)`, trimmed on load, `padRight` on fixed-width output. `PIC X(10)` date → `LocalDate` only where COBOL treats it as a date. `PIC X(26)` ts → `LocalDateTime`/`TIMESTAMP`; zoned overpunch, COMP-3 and COMP decoders | FACT: REF conventions.md §1 table |
| Record codec | One `RecordLayout` class per copybook (CVTRA06Y, CVTRA05Y, CVACT01Y, CVACT03Y, CVTRA01Y, ...), and `RecordReader`/`RecordWriter` for ASCII line-sequential and EBCDIC CP037 fixed-block files | FACT pattern: REF conventions.md §5 "Record layouts"; re-implemented fresh on main |
| Error handling | `FILE STATUS` → typed exceptions per REF conventions.md §8. In batch, any unexpected status is an **abend**: an uncaught exception fails the step (job RC 16). Records are never silently skipped | FACT: REF conventions.md §8 |
| Logging / observability | SLF4J + Logback (Boot default). COBOL `DISPLAY` lines that are part of observable behavior (counts, reject messages) are reproduced on the job's `SYSOUT` file and stdout. Card numbers in logs are masked to the last 4 digits (the PAN is never logged in full) | CONFIRMED (masking per REF PR #182 finding on CBTRN01C) |
| Test framework | JUnit 5 + AssertJ + Spring Boot Test + `spring-batch-test`; Testcontainers `postgres:16` with real Flyway, `ddl-auto: validate`; **no H2**; Testcontainers pinned 1.21.4 | FACT: REF conventions.md §9 |
| Parity oracle | A GnuCOBOL golden master: compile the original program with `cobc -std=ibm -I app/cpy`, run it on the same inputs, and require byte-identical outputs (files + RC) | FACT pattern: REF PR #176, jcl-mapping.md; repo memory note on GnuCOBOL |
| CI gate | GitHub Actions `.github/workflows/modernization-verify.yml`: Temurin 21, Docker check, `mvn -B verify` (unit + Testcontainers + golden master) | CONFIRMED, modeled on REF workflow |
| Forbidden | `double`/`float` for money; H2; Lombok; silently swallowing I/O errors; editing anything under `app/` (read-only reference); new runtime dependencies beyond Boot starters, Flyway, the PostgreSQL driver and Testcontainers without a decision entry | CONFIRMED |

## 2. ONLINE profile

**N/A for this engagement run.** The only stream requested is POSTTRAN (BATCH). If an online stream is picked later, re-run `!mf_ingest_target_state`. REF's accepted choices give a starting point: REST-only API replacing BMS (REF api-style.md), JWT session replacing the COMMAREA (REF architecture.md §3.1), and `PageResponse` for PF7/PF8.

## 3. BATCH profile (applies to POSTTRAN)

| Field | Value | Marking / source |
|---|---|---|
| Runtime | Spring Batch 5 (from Boot 3.3), JobRepository tables in PostgreSQL schema `batch` | FACT pattern: REF `transaction-service/pom.xml` (`spring-boot-starter-batch`) |
| Job / step model | One Spring Batch `Job` per JCL member, registered under the member name (`POSTTRAN`). One step per JCL `EXEC` step (`STEP15` → `step15Cbtrn02c`). Inside the step, the COBOL main loop is a **chunk-oriented step**: `ItemReader` (DALYTRAN file) → `ItemProcessor` (validate 100–103, build TRAN-RECORD) → `ItemWriter` (TRANSACT insert, ACCT update, TCATBAL upsert, reject write) | FACT: job-per-member from REF jcl-mapping.md; **chunking CONFIRMED** (REF used tasklets) |
| Launch seam | CLI `java -jar carddemo-app.jar --job=POSTTRAN [--dd=DALYTRAN=<file>] [--output-dir=<dir>]`. The process exit code is the JCL condition code. Optional HTTP `POST /api/v1/batch/jobs/POSTTRAN` for ops | FACT pattern: REF `common/batch/BatchCli.java` |
| Return codes | Job RC = highest step RC; 4 when any reject was written (CBTRN02C sets `RETURN-CODE 4`); 16 on abend; 12 on usage error | FACT: REF jcl-mapping.md "Return codes"; CBTRN02C source |
| Dataset mapping | **Sequential input DALYTRAN (PS)** → a file passed with `--dd`, read with the CVTRA06Y layout (ASCII or EBCDIC). **KSDS masters** TRANSACT, ACCTDATA, CARDXREF, TCATBALF → PostgreSQL tables. **GDG output DALYREJS(+1)** → file `<output-dir>/POSTTRAN/<yyyyMMdd-HHmmss-SSS>/DALYREJS`, 430-byte fixed records. `SYSOUT` → a file in the same generation dir | CONFIRMED, consistent with REF jcl-mapping.md "How a JCL job runs" |
| Unit of work | One daily-transaction record = one DB transaction across TRANSACT, ACCOUNT and TCATBAL (possible because of the monolith, C-1). Chunk commit interval configurable, default 100 | CONFIRMED |
| Restart / rerun | No mainframe checkpoint exists (CBTRN02C has none). Java: the job is restartable from the last committed chunk via the JobRepository. A **full rerun** of an already-posted file behaves like COBOL: a duplicate TRAN-ID on the TRANSACT write is an abend (RC 16), not a silent skip. Any idempotency beyond that is a decision for `06_decisions.md` | CONFIRMED, flagged for confirmation |
| Scheduler | An external scheduler replaces CA-7 (`CardDemo.ca7`: CLOSEFIL → CBPAUP0J → POSTTRAN → WAITSTEP → OPENFIL). It runs `--job=POSTTRAN`; completion is signalled by the exit code and the `RETURN-CODE` file in the generation dir. CLOSEFIL/OPENFIL/WAITSTEP drop out (no CICS file sharing) | FACT: REF jcl-mapping.md "Schedule"; repo memory note on CLOSEFIL scope |
| Job parameters | No PARM in POSTTRAN.jcl. A `run.id` / launch timestamp keeps Spring Batch job instances unique | FACT: `app/jcl/POSTTRAN.jcl` |
| Reports | None in POSTTRAN. DALYREJS is the only produced dataset | FACT: `app/jcl/POSTTRAN.jcl` |
| Drift rules (reject the PR if) | (1) the output differs byte-for-byte from the GnuCOBOL golden master on any golden case; (2) RC differs; (3) an abend path in COBOL becomes a skip or warning in Java; (4) business rules live in the reader/writer instead of the service; (5) a COBOL quirk is "fixed" without a decision entry; (6) floating point appears anywhere in the money path | CONFIRMED |

## 4. SUBTRANSACTION profile

**N/A.** CBTRN02C calls no subprogram except `CEE3ABD` (abend, `app/cbl/CBTRN02C.cbl:711`), which maps to an uncaught exception (CORE error handling). Re-run if a stream with called sub-flows is picked.

## 5. DATA / BOUNDARY profile

| Field | Value | Marking / source |
|---|---|---|
| Data target | PostgreSQL 16 (Docker `postgres:16` locally, Testcontainers in tests) | FACT pattern: REF docker-compose.yml, conventions.md §9 |
| Schema layout | One database `carddemo`. One schema per bounded context (`account`, `card`, `customer`, `transaction`, `reference`, `batch`); FKs are allowed across schemas because it is one app | CONFIRMED (REF forbade cross-schema FKs because it had five services) |
| Persistence style | Spring Data JPA for entities. `@Version` optimistic locking on rewritten masters (ACCOUNT, TCATBAL) | FACT: REF conventions.md §7 |
| Migrations | Flyway `V<n>__<name>.sql` under `carddemo-app/src/main/resources/db/migration`; schema derived from the copybooks (CVACT01Y, CVACT03Y, CVTRA01Y, CVTRA05Y, CVTRA06Y) | FACT pattern: REF architecture.md §2 |
| Seed / load | A loader reads `app/data/ASCII` (or EBCDIC) with the record codec into the tables, for local runs and golden cases | FACT pattern: REF data-loader (PR #165) |
| Keys | KSDS primary key → `PRIMARY KEY`; AIX → secondary index | FACT: REF conventions.md §6 |
| Stored procedures | None: CardDemo POSTTRAN uses VSAM, not DB2 | FACT: CBTRN02C has no `EXEC SQL` |
| Outbound integration | None for POSTTRAN (no MQ or HTTP in CBTRN02C) | FACT |
| Inbound exposure | CLI and optional HTTP trigger (BATCH launch seam) only | CONFIRMED |
| Coexistence | No strangler routing: the COBOL stays the read-only oracle; the Java job is validated by golden-master parity, not run side by side in production | CONFIRMED |

## 6. Cross-profile reconciliation

- **C-1 (deliberate change from REF):** REF chose five services and records the cost itself. POSTTRAN posts to account-service over HTTP, so a local failure after the posting leaves the account updated without a TRANSACT row (REF `docs/migration/residual-risks.md` C2, architecture.md §1). The fresh target is one app, so CBTRN02C's TRANSACT write, ACCT rewrite and TCATBAL update stay in one database transaction, as on the mainframe.
- **C-2:** the type mapping is shared by every surface and lives in CORE (REF conventions.md), not in the BATCH profile.
- **C-3:** REF staged DALYTRAN into a `daily_transactions` table and wrote rejects to a table, which kept its golden harness from running CBTRN02C directly (REF architecture.md §4.2). The fresh target reads and writes the **files** named by the JCL DDs, so it can be compared byte for byte.

## 7. STOP A decisions (resolved)

All resolved: the user confirmed every item on 2026-11-19.

1. CONFIRMED: the **modular monolith** (C-1) instead of REF's five services.
2. CONFIRMED: **chunked steps with restart from the last committed chunk**, and a **rerun that abends on duplicate TRAN-ID** (COBOL parity).
3. CONFIRMED: **DOCS paths** (`functional/CARDDEMO/**`, `.migration/**` on `main`, delivered through PRs).
4. CONFIRMED: the **parity bar**: byte-identical DALYREJS + identical table state (TRANSACT, ACCTDATA, TCATBALF) + identical RC vs GnuCOBOL.
5. CONFIRMED: the **CORE forbidden list** (no Lombok, no H2, no new dependencies without a decision entry).

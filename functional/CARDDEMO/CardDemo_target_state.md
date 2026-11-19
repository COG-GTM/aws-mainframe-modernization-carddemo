# CardDemo target state (CORE + per-surface profiles)

Produced by `!mf_ingest_target_state` for the POSTTRAN engagement (run started 2026-11-19 from `main`).

**Status: CONFIRMED (STOP A).** STOP A is answered by the user's brief (alvaro.leal, 2026-11-19, quoted in `.migration/00_context.md`). Every field the brief fixes is **FACT (source: user)**. Fields the brief does not fix are Devin defaults recorded as **AUTO-APPLIED (revisable)** in `.migration/06_decisions.md` under the orchestrator rule "apply the recommended option, record it, continue".

Marking: **FACT** = cited from the user brief (`USER`), the COBOL source (`app/...:<line>`), or the reference branch (`REF:`). **AUTO** = Devin default, decision id in `06_decisions.md`. **N/A** = out of scope with reason.

Evidence sources (never a base):
- `REF:` branch `migration/carddemo-java` @ `c251be50` (five Spring Boot services; `docs/migration/*.md`, `modernization/**`).
- `PRIOR:` stopped run, PR #210 branch `devin/1795090039-mf-posttran-stream` @ `e0a9ad14`.
- `COG-GTM/mainframe-modernization-carddemo` is a look-alike repo and is **ignored** (USER).

## 0. Repository topology

| Role | Repo | Path | Base | Marking |
|---|---|---|---|---|
| SOURCE | COG-GTM/aws-mainframe-modernization-carddemo | `app/` (read-only) | `main` | FACT (USER "app/ es solo lectura") |
| BACKEND | same | `modernization/` (`carddemo-app` + `carddemo-common`) | `main` | FACT (USER) |
| FRONTEND | N/A | - | - | N/A: POSTTRAN is BATCH, no screens |
| DOCS | same | `functional/CARDDEMO/**`, `.migration/**` | `main` | FACT (orchestrator rules) |

Single repo: each stacked PR (docs → Phase 0 → Phase 1 → Wave 1 → sign-off) is one PR, each based on the previous one, none merged (USER).

## 1. CORE

| Field | Value | Marking |
|---|---|---|
| Language | Java 21 | FACT (USER); REF `modernization/pom.xml:33` |
| Framework | Spring Boot 3.3 (parent `spring-boot-starter-parent` 3.3.x) | FACT (USER); REF `modernization/pom.xml:10` uses 3.3.4 |
| Build | Maven multi-module, root `modernization/pom.xml`, modules `carddemo-common`, `carddemo-app`; `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` | FACT (USER modules); command FACT (repo blueprint) |
| Application shape | Modular monolith: one deployable `carddemo-app`, shared library `carddemo-common` (COBOL types, record codec, batch launch seam). Bounded contexts are packages | FACT (USER) |
| Package layout | `com.carddemo.<context>.{domain,repository,service,batch}`; contexts used by POSTTRAN: `account`, `card` (xref), `transaction`, `reference` (tcatbal). Shared: `com.carddemo.common.{cobol,record,batch}` | AUTO D-003 (adapted from REF `docs/migration/architecture.md` §2 "Layout") |
| Naming | Classes named after the business concept (`DailyTransactionPosting...`); Javadoc cites the COBOL program + paragraph implemented (`CBTRN02C 1500-B-LOOKUP-ACCT`) | AUTO D-003 |
| COBOL type mapping | REF `docs/migration/conventions.md` §2–§5 adopted: `S9(n)V99` → `BigDecimal` scale 2 / `NUMERIC(n+2,2)`, truncation unless `ROUNDED`; `9(n)` ids → `long`/`BIGINT` with range CHECK, zero-padded on output; `X(n)` → `String`/`VARCHAR(n)`; zoned-decimal sign overpunch decoder/encoder. Dates/timestamps that the stream only compares or copies stay `VARCHAR` text so byte parity holds | FACT (REF conventions.md §1 table); text-date rule AUTO D-004 |
| Money | `BigDecimal` only; `double`/`float` forbidden in the money path | FACT (USER "Prohibido ... double para dinero") |
| Record codec | One layout class per copybook used (CVTRA06Y, CVTRA05Y, CVACT01Y, CVACT03Y, CVTRA01Y); `RecordReader`/`RecordWriter` for fixed-length records, ASCII (ISO-8859-1) and EBCDIC CP037 | AUTO D-005 (pattern from REF conventions.md §5) |
| Error handling | Any COBOL abend path (unexpected FILE STATUS → `9999-ABEND-PROGRAM`, `app/cbl/CBTRN02C.cbl:707-711`) becomes an uncaught exception that fails the step; never a skip or warning | AUTO D-006 (REF conventions.md §8) |
| Logging | SLF4J/Logback. Observable `DISPLAY` lines (start/end, counts, `TCATBAL record not found ... Creating.`) reproduced on stdout and in the job's `SYSOUT` file. Card numbers masked to last 4 in technical logs | AUTO D-007 |
| Tests | JUnit 5, AssertJ, Spring Boot Test, `spring-batch-test`, Testcontainers `postgres:16` with real Flyway and `ddl-auto: validate` | FACT (REF conventions.md §9); H2 forbidden FACT (USER) |
| Parity oracle | GnuCOBOL 3.1 golden master of the original program: same input → same DALYREJS bytes, same table contents, same RC | FACT (USER "Paridad: contra GnuCOBOL, con el mismo DALYREJS, las mismas tablas y el mismo RC") |
| CI gate | GitHub Actions `.github/workflows/modernization-verify.yml`: Temurin 21, `mvn -B verify` (unit + Testcontainers + golden) | AUTO D-008 (REF has the same workflow name) |
| Forbidden | Lombok; H2; `double`/`float` for money; new dependencies not recorded as a decision; editing `app/` | FACT (USER) |
| Dependency allow-list | Boot starters (batch, data-jpa, test), Flyway (+ `flyway-database-postgresql`), PostgreSQL JDBC driver, Testcontainers (postgresql, junit-jupiter) | AUTO D-009 (each further dependency needs its own decision) |
| Artifact language | English for code and docs | FACT (orchestrator rule) |

## 2. ONLINE profile

**N/A** (USER: "ONLINE y SUBTRANSACTION son N/A"). POSTTRAN has no BMS map or CICS transaction (`app/jcl/POSTTRAN.jcl:23`). Re-run this playbook before any online stream.

## 3. BATCH profile (POSTTRAN)

| Field | Value | Marking |
|---|---|---|
| Runtime | Spring Batch 5 (Boot 3.3), JobRepository tables in PostgreSQL (schema `batch`) | FACT (USER "Spring Batch 5"); schema AUTO D-010 |
| Job / step model | One `Job` per JCL member, named after it (`POSTTRAN`); one step per `EXEC` (`STEP15`, `app/jcl/POSTTRAN.jcl:23`) | FACT (USER "un job por JCL y un step por EXEC") |
| Step style | Chunk-oriented, **commit interval 1**, restartable: reader = DALYTRAN file, processor = validation (CBTRN02C 1500-*), writer = posting (TCATBAL, ACCOUNT, TRANSACT) or reject write | FACT (USER "chunk de 1 con reinicio") |
| Unit of work | One DALYTRAN record = one DB transaction (TCATBAL + ACCOUNT + TRANSACT) + its reject line; the monolith allows one transaction where REF needed HTTP (REF `docs/migration/architecture.md` §1) | AUTO D-011 |
| Restart | Restart of a FAILED execution resumes after the last committed record (reader `ExecutionContext` = records read; DALYREJS writer reopens in append and truncates to the committed byte offset) | FACT restart (USER); mechanism AUTO D-012 |
| Rerun of a COMPLETED input | Not guarded: COBOL itself has no guard (`OPEN OUTPUT TRANSACT`, `app/cbl/CBTRN02C.cbl:256`); a new instance with already-posted TRAN-IDs abends on the duplicate key like COBOL (RC 16) | AUTO D-013 |
| Launch seam | CLI: `java -jar carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=<file> --dd=DALYREJS=<file>`; exit code = JCL condition code | AUTO D-014 (pattern from REF `common/batch`) |
| Return codes | 0 = no rejects; 4 = at least one reject (`app/cbl/CBTRN02C.cbl:229-231`); 16 = abend (COBOL ABEND U999; GnuCOBOL prints RC 231 = 999 mod 256); 12 = usage/launch error | FACT 0/4 (source); 16/12 AUTO D-015 |
| Dataset mapping | DALYTRAN (PS, LRECL 350) and DALYREJS (GDG +1, RECFM=F LRECL 430, `app/jcl/POSTTRAN.jcl:30-39`) stay **files**; KSDS TRANSACT, CARDXREF, ACCTDATA, TCATBALF become **tables** | FACT (USER) |
| DALYREJS generation | `--dd=DALYREJS=<path>`; if omitted, `<output-dir>/POSTTRAN/<run-id>/DALYREJS`. Existing target file is refused (GDG `+1` is always NEW, `DISP=(NEW,CATLG,DELETE)`) | AUTO D-016 |
| Parameters | None (no PARM on `EXEC`, `app/jcl/POSTTRAN.jcl:23`); job parameters = `dd.DALYTRAN`, `dd.DALYREJS`, `run.id` | FACT/AUTO D-014 |
| Scheduler | External scheduler replaces CA-7 chain CLOSEFIL → CBPAUP0J → POSTTRAN → WAITSTEP (`app/scheduler/CardDemo.ca7:38,59,80`); completion = exit code | FACT chain; signalling AUTO D-017 |
| Reports | None; DALYREJS is the only dataset produced | FACT (`app/jcl/POSTTRAN.jcl`) |
| Drift rules (reject PR if) | DALYREJS bytes, table contents or RC differ from the GnuCOBOL oracle on any golden case; an abend path becomes skip/warn; business rules in reader/writer instead of the service; a COBOL quirk "fixed" without a decision; chunk ≠ 1 | AUTO D-018 |

## 4. SUBTRANSACTION profile

**N/A** (USER). CBTRN02C's only CALL is `CEE3ABD` (`app/cbl/CBTRN02C.cbl:711`), an LE runtime abend service mapped to CORE error handling.

## 5. DATA / BOUNDARY profile

| Field | Value | Marking |
|---|---|---|
| Data target | PostgreSQL 16 | FACT (USER) |
| Migrations | Flyway `V<n>__<name>.sql` in `carddemo-app/src/main/resources/db/migration` | FACT (USER "Flyway") |
| Schema layout | One database `carddemo`, schemas per context (`account`, `card`, `transaction`, `reference`, `batch`) | AUTO D-010 |
| Persistence | Spring Data JPA entities, explicit columns from the copybooks; `@Version` on rewritten masters (account, tcatbal) | AUTO D-019 (REF conventions.md §7) |
| Keys | KSDS key → primary key (TRANSACT `TRAN-ID`, CARDXREF card num, ACCTDATA acct id, TCATBALF composite acct+type+cat) | FACT (REF conventions.md §6; FD keys `app/cbl/CBTRN02C.cbl:37,43,54,60`) |
| FILLER | Not stored; regenerated as spaces when a record is unloaded for parity | AUTO D-020 |
| Seed / load | Loader from `app/data/ASCII` fixed-width files into the tables (local runs + golden cases) | AUTO D-021 |
| Stored procedures | None (VSAM only, no `EXEC SQL` in CBTRN02C) | FACT |
| Outbound integration | None (no MQ/HTTP/CALL except CEE3ABD) | FACT |
| Inbound exposure | CLI only | AUTO D-014 |
| Coexistence | No strangler routing in production: COBOL stays the oracle; parity is proven offline by the golden master | AUTO D-022 |

## 6. Cross-profile reconciliation

- **C-1** REF split the module in five services and lost the single unit of work for posting (REF `docs/migration/architecture.md` §1). The user's monolith restores it; deliberate change from REF.
- **C-2** REF staged DALYTRAN and rejects into tables (REF `docs/migration/architecture.md` §4.2). The user keeps both as files, so DALYREJS is compared byte for byte.
- **C-3** Type mapping is shared, so it lives in CORE; BATCH only adds dataset and step rules.

## 7. Open questions

None blocking. All defaults are AUTO-APPLIED (revisable) in `.migration/06_decisions.md`.

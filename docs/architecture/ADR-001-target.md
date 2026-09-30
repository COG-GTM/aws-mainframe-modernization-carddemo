# ADR-001: Target design pattern for the CardDemo estate

- **Status:** Accepted (pending merge)
- **Date:** 2026-09-30
- **Decision id:** `d-target` = `java-spring-services`
- **Plan step:** `s2.1` (phase `p2`, Target architecture and foundation)
- **Supersedes:** none. Later ADRs may refine a row below; they must reference this one.

## 1. Context

The estate is 44 COBOL programs (`find app -iname '*.cbl'`):

| Area | Programs | Runtime dependencies |
|---|---|---|
| Base online (`app/cbl`) | 17 CICS programs: COSGN00C, COMEN01C, COADM01C, COACTVWC, COACTUPC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COBIL00C, CORPT00C, COUSR00C-03C | CICS, BMS (17 maps in `app/bms`), VSAM KSDS + AIX |
| Base batch (`app/cbl`) | 13: CBACT01C-04C, CBCUS01C, CBTRN01C-03C, CBSTM03A/B, CBEXPORT, CBIMPORT, COBSWAIT; plus the called subprogram CSUTLDTC | JCL (`app/jcl`), VSAM/QSAM, GDGs, Assembler COBDATFT/MVSWAIT (`app/asm`), Control-M / CA-7 (`app/scheduler`) |
| Optional `app-authorization-ims-db2-mq` | 8: COPAUA0C, COPAUS0C, COPAUS1C, COPAUS2C, CBPAUP0C, DBUNLDGS, PAUDBLOD, PAUDBUNL | CICS, IMS DB (DL/I), Db2, IBM MQ |
| Optional `app-transaction-type-db2` | 3: COTRTLIC, COTRTUPC, COBTUPDT | CICS, Db2 |
| Optional `app-vsam-mq` | 2: COACCT01, CODATE01 | CICS, IBM MQ, VSAM |

CICS usage across the estate (`grep -ho "EXEC CICS [A-Z]*"`): SEND 40, RETURN 38, READ 27, RECEIVE 21,
XCTL 13, HANDLE 10, SYNCPOINT 7, STARTBR/READPREV 6 each, ENDBR 5, ABEND 5, READNEXT 4, WRITE 3,
RETRIEVE 3, WRITEQ 2, REWRITE 2, LINK 1, INQUIRE 1, DELETE 1. EIBRESP is tested against NORMAL (55),
NOTFND (30), ENDFILE (8), DUPREC (7) and DUPKEY (3).

Decisions already recorded for the migration and not reopened here: optional modules are in scope;
`samples/m2` archives are reference only; CSD entries without source (COADM00C, COTSTP1C-4C,
COCRDSEC / transaction `CDV1`) are out of scope; the broker is ActiveMQ Artemis via JMS; the scheduler
is Apache Airflow; validation is golden-set equivalence against GnuCOBOL runs of the originals plus
screen-flow tests.

## 2. Candidates

Three candidates already have code in this repository.

1. **Java 21 / Spring Boot services on PostgreSQL** - branch
   `devin/1789660764-carddemo-java-modernization`, directory `modernization/`: Maven reactor
   (Spring Boot 3.3.4, Java 21) with `common`, `auth-service`, `customer-service`, `account-service`,
   `card-service`, `transaction-service`; one PostgreSQL schema per service with Flyway; Spring Batch
   ports of POSTTRAN/CBTRN02C and INTCALC/CBACT04C; `docker-compose.yml` for postgres + 5 services.
   Its `docs/COBOL-PROGRAM-MAPPING.md` marks 16 of the 17 core online transactions ported (CM00 and
   CA00 as `nextScreen` routing rather than endpoints) and CR00 not built.
2. **Java 21 / Spring Boot modular monolith** - same stack, one deployable, one module per slice. No
   branch of its own; it would be produced by collapsing option 1.
3. **TypeScript / Node** - branch `devin/1789654793-ts-foundation`, directory `ts/`: npm workspaces
   `@carddemo/copybook` (zoned/packed decimal codec), `@carddemo/domain` (typed records for the
   eleven data copybooks), `@carddemo/vsam` (in-process KSDS with COBOL file-status codes). Follow-on
   `devin/17896727xx-ts-*` branches add batch readers, posting, interest, reports and a BMS-driven
   terminal UI, none merged.

Also present but not a candidate: the `aws/` track (`devin/17906*` branches: Spring Boot online
services, Spring Batch + Step Functions, React/Vite frontend, CDK infra, ETL). It targets AWS managed
services (SQS/SNS, Step Functions) that conflict with the Artemis and Airflow decisions, so it is
used only as a reference, chiefly `aws/frontend` for the UI approach in section 4.

### Evidence gathered for this ADR (2026-09-30)

| Command | Result |
|---|---|
| `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` on option 1 branch (`2136cd4`) | exit 0; 41 tests, 0 failures (account 12, auth 4, card 7, customer 2, transaction 16) |
| `(cd ts && npm ci && npm run build && npm test)` on option 3 branch (`ab3661b`) | exit 0; 33 tests, 0 failures (copybook 9, vsam 5, domain 19) |
| `cobc --version` | GnuCOBOL 3.1.2.0 available on the build image for the oracle |

## 3. Comparison

Scored 1 (poor) to 3 (good) against the four criteria set by the plan.

| Criterion | 1. Spring Boot services | 2. Spring Boot modular monolith | 3. TypeScript / Node |
|---|---|---|---|
| **Fit to the 44-program estate** | 3 - the estate already partitions by VSAM file ownership (USRSEC; CUSTDAT; ACCTDAT; CARDDAT + CARDXREF; TRANSACT + TCATBALF + DISCGRP + TRANTYPE/TRANCATG), which is exactly the service split already built. Optional modules add one service (authorization) and listeners; nothing forces a redesign. Cross-service writes (CBTRN02C updates ACCTDAT and TRANSACT in one run) need an idempotency key / outbox - already identified as a known limitation on the branch. | 3 - same mapping, and cross-slice writes are a single local transaction, which is closer to the one-unit-of-work semantics of CBTRN02C and COACTUPC (`app/cbl/COACTUPC.cbl:953`, `:4100`). | 2 - the codec and KSDS emulation map well onto batch, but CICS online flows, Db2, IMS and MQ all still need a framework choice (HTTP, SQL, broker client) that does not exist yet. |
| **Testability against the GnuCOBOL oracle** | 3 - JUnit 5 + Spring Batch test; golden files compared at the record level. Needs a COMP-3 / zoned codec in `common` (the branch only has `CobolValues` for dates and trimming) - that is step `s2.3`. Tests currently run on H2; oracle tests must use PostgreSQL (Testcontainers) so NUMERIC and collation match. | 3 - identical to option 1. | 3 - strongest today: the codec round-trips `app/data/ASCII` byte for byte and file-status codes are first class. |
| **Deployability in containers** | 3 - multi-module Dockerfile + compose already build and start on this image (blueprint `startup`). Five JVMs plus Postgres is more surface for a trial, but it is automated. | 3 - one image, simplest to run. | 2 - trivially containerised, but there is no compose/run story yet and every online program would add HTTP wiring from scratch. |
| **Skills of the eventual owner** | 3 - Java/Spring is what most mainframe shops staff and what the customer asked for (`d-target`). | 3 - same. | 1 - requires a Node/TypeScript platform team most mainframe estates do not have. |
| **Progress already made** | 3 - 5 services, 2 batch jobs, 41 tests green. | 1 - would first discard the service boundaries, Flyway schemas and compose built on option 1. | 1 - foundation only on the base branch; online flows unmerged. |
| **Total** | **15** | 13 | 9 |

## 4. Decision

**Option 1: Java 21 / Spring Boot services on PostgreSQL**, seeded from
`devin/1789660764-carddemo-java-modernization` as a reference (logic is re-derived from `app/`; the
branch is not merged wholesale).

Option 2 is the fallback if the trial's operating cost turns out to matter more than the service
split: because services communicate only through the gateway interfaces below, collapsing them into
one deployable later is mechanical. Option 3's codec is the reference implementation for the Java
COBOL-semantics library in `s2.3`.

| Concern | Decision |
|---|---|
| Language / runtime | Java 21 (Temurin/OpenJDK), Spring Boot 3.3.x, Maven multi-module reactor under `modernization/` |
| Application shape | One Spring Boot service per data-owning domain: `auth-service`, `customer-service`, `account-service`, `card-service`, `transaction-service`, plus new `authorization-service` (optional IMS/Db2/MQ module) and `data-exchange` (CBEXPORT/CBIMPORT). Shared code in `common` (errors, pagination, COBOL value/codec helpers, traceability annotation). |
| Persistence | PostgreSQL 16, one schema per service, Flyway migrations, Spring Data JPA. `PIC S9(n)V99` -> `NUMERIC(n+2,2)` / `BigDecimal` with the COBOL scale; `PIC X(n)` -> `CHAR(n)`/`VARCHAR(n)` with the original length (no widening). VSAM KSDS key -> primary key; AIX -> secondary index. IMS segments -> parent/child tables in `authorization`; Db2 tables -> tables of the same shape. |
| API style | REST/JSON over HTTP, OpenAPI via springdoc, one endpoint per CICS transaction action; stateless. Service-to-service calls through typed gateway interfaces (`AccountGateway`, `CardGateway`, ...) with REST implementations. |
| UI approach | React + TypeScript SPA (Vite), one screen component per BMS map (17 base + 4 optional: COPAU00, COPAU01, COTRTLI, COTRTUP), field inventory generated from the BMS/symbolic map, PF keys bound to keyboard shortcuts, message literals verbatim. `aws/frontend` is the reference. Screen-flow tests drive the SPA with Playwright. |
| Batch framework | Spring Batch 5: one `Job` per JCL job, one `Step` per JCL `EXEC` step, job hosted in the service owning its primary output. Jobs are triggered over HTTP by Apache Airflow DAGs that replace `app/scheduler/CardDemo.controlm` / `CardDemo.ca7`. `RETURN-CODE` -> job exit status; ABEND -> `FAILED` with the abend code. |
| Messaging | ActiveMQ Artemis via Spring JMS; MQ queues keep their names as JMS destinations; MQ correlation id -> `JMSCorrelationID`. |
| Local run | `docker compose` under `modernization/` (postgres, artemis, services, SPA). |
| Tests | JUnit 5; PostgreSQL via Testcontainers for anything that touches NUMERIC arithmetic or ordering; golden-set equivalence against GnuCOBOL runs of the originals. |

### 4.1 Program-to-unit mapping

Every COBOL program maps to exactly one Java class (the "program class") named after the program in
PascalCase of its function, annotated with its source so tools and reviewers can trace it back:

```java
@CobolProgram(source = "app/cbl/CBTRN02C.cbl", jcl = "app/jcl/POSTTRAN.jcl")
public class PostDailyTransactionsJob { ... }
```

`@CobolProgram` lives in `common` (added by `s2.2`/`s2.3`). Paragraphs become methods named after the
paragraph (`1500-VALIDATE-TRAN` -> `validateTran1500()` or `validateTran()` with a `@Paragraph("1500-VALIDATE-TRAN")`
annotation; the exact spelling is fixed by `CONVENTIONS.md` in `s2.4`). Copybooks map to one record
type each, and every business rule carries a `path:line` citation in its Javadoc or test name.

| Target unit | Programs (44) |
|---|---|
| `auth-service` (7) | COSGN00C, COMEN01C, COADM01C, COUSR00C, COUSR01C, COUSR02C, COUSR03C |
| `customer-service` (1) | CBCUS01C |
| `account-service` (5) | COACTVWC, COACTUPC, CBACT01C, COACCT01 (JMS listener), CODATE01 (JMS listener) |
| `card-service` (5) | COCRDLIC, COCRDSLC, COCRDUPC, CBACT02C, CBACT03C |
| `transaction-service` (14) | COTRN00C, COTRN01C, COTRN02C, COBIL00C, CORPT00C, CBTRN01C, CBTRN02C, CBTRN03C, CBACT04C, CBSTM03A, CBSTM03B, COTRTLIC, COTRTUPC, COBTUPDT |
| `authorization-service` (8) | COPAUA0C (JMS listener), COPAUS0C, COPAUS1C, COPAUS2C, CBPAUP0C, DBUNLDGS, PAUDBLOD, PAUDBUNL |
| `data-exchange` (2) | CBEXPORT, CBIMPORT |
| `common` library (2) | CSUTLDTC (date validation, called via `app/cpy/CSUTLDPY.cpy:293`), COBSWAIT (wait utility, used as a Spring Batch tasklet) |

The Assembler routines COBDATFT (called from `app/cbl/CBACT01C.cbl:231`) and MVSWAIT (used by
COBSWAIT) become plain Java methods in `common`.

### 4.2 What replaces CICS constructs

| CICS construct | Example in source | Target |
|---|---|---|
| COMMAREA (`COCOM01Y`) | `app/cbl/COSGN00C.cbl:98-101`; `app/cpy/COCOM01Y.cpy:22-29` | Stateless API; the SPA holds navigation context (from/to program, selected account/card) in client state; identity comes from the signon token, not `CDEMO-USER-ID`/`CDEMO-USER-TYPE`. |
| Pseudo-conversation (`RETURN TRANSID ... COMMAREA`, `RECEIVE MAP`/`SEND MAP`) | `app/cbl/COSGN00C.cbl:98`, `:151` | One request/response per ENTER/PF key; the first-time `EIBCALEN = 0` path becomes the initial GET that renders the empty screen. |
| XCTL | `app/cbl/COMEN01C.cbl:185` (13 sites) | SPA routing; the backend returns the next screen (`nextScreen`) where the COBOL decided it. |
| LINK / static `CALL` | `app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl:248`; `app/cbl/CBSTM03A.CBL:351` | In-process method call inside a service; gateway (REST) call when it crosses a service boundary. |
| EIBAID / PF keys | `app/cbl/COSGN00C.cbl:85-88` | Action field on the request (`ENTER`, `PF3`, `PF7`, `PF8`, ...); keyboard bindings in the SPA. |
| EIBRESP / `DFHRESP(...)` | NORMAL 55, NOTFND 30, ENDFILE 8, DUPREC 7, DUPKEY 3 | Typed exceptions in `common`: NOTFND -> `NotFoundException` (404), DUPREC/DUPKEY -> `DuplicateKeyException` (409), ENDFILE -> end of page (empty result, not an error), other -> `CicsResponseException` carrying the original RESP name. The screen message the COBOL shows is returned verbatim. |
| READ / READ UPDATE + REWRITE | `app/cbl/COBIL00C.cbl:379`, `app/cbl/COUSR02C.cbl:360` | Repository read; UPDATE lock -> JPA `@Version` optimistic locking within one `@Transactional` method. |
| STARTBR / READNEXT / READPREV / ENDBR | `app/cbl/COTRN00C.cbl:593`, `:626`, `:660` | Keyset pagination on the primary key (or AIX index), page size equal to the screen's row count; no server-side browse cursor. |
| SYNCPOINT / SYNCPOINT ROLLBACK | `app/cbl/COACTUPC.cbl:953`, `:4100` | Commit / rollback of the Spring transaction. |
| HANDLE ABEND / ABEND, batch `CEE3ABD` | `app/cbl/COACTVWC.cbl:264`, `app/cbl/CBTRN02C.cbl:707-711` | `AbendException(code)`; online -> 500 with the program's abend message; batch -> step `FAILED`, exit status carries the abend code. |
| WRITEQ TD `JOBS` (job submission) | `app/cbl/CORPT00C.cbl:517-521` | HTTP call to the batch trigger of the target job (Airflow DAG run or Spring Batch launch endpoint). |
| RETRIEVE (started-task data) | `app/app-vsam-mq/cbl/COACCT01.cbl:191` | JMS message payload delivered to the listener. |
| INQUIRE PROGRAM (menu option availability) | `app/cbl/COMEN01C.cbl:148-151` | Feature/route availability check in the menu service. |
| ASKTIME / FORMATTIME | 5 sites each | `java.time.Clock` injected (fixed clock in tests for oracle equivalence). |

## 5. Consequences

- Phases 3-5 stay as planned; no rewrite of downstream steps is needed.
- The option 1 branch diverges from byte-for-byte COBOL behaviour in places that need their own
  recorded decision before they are carried over; until then workers follow the COBOL:
  - passwords: the branch stores BCrypt hashes, the COBOL compares the upper-cased plaintext
    (`app/cbl/COSGN00C.cbl:135`, `:223`);
  - transaction ids: the branch generates 16 random base-36 characters, the COBOL reads the highest
    key and adds 1 (`app/cbl/COTRN02C.cbl:444-451`);
  - messaging: the branch documents SQS/SNS; the recorded broker is Artemis/JMS;
  - tests run on H2; oracle tests must run on PostgreSQL.
- Cross-service updates in batch posting (CBTRN02C) need an idempotency key plus outbox before the
  side-by-side run (`s6.3`); the branch lists this as a known limitation.
- Five to seven JVMs is more to operate than a monolith; accepted for the trial because it is
  already automated and matches the owner's target. Option 2 remains a cheap fallback.

## 6. Review of downstream `depends_on`

Checked against the board's plan on 2026-09-30. Every step below `s2.1` is written for option 1, so
no step needs rewriting.

| Step | Depends on | Verdict |
|---|---|---|
| s2.2 build/run/CI | s2.1 | correct - lands the reactor, compose and `@CobolProgram` defined here |
| s2.4 conventions | s2.1 | correct - fixes paragraph/method naming left open in 4.1 |
| s2.3 COBOL-semantics library | s2.2, s1.5 | correct |
| s2.5 CICS security / MQ / IMS runtimes | s2.2 | correct (transitively on s2.1); implements the Artemis and IMS rows above |
| s3.1 schema | s2.2, s1.1 | correct |
| s3.2-s3.4 data | s3.1, s2.3 (and s3.2/s3.3 for s3.4) | correct |
| s4.1 screen framework | s2.2, s2.4, s3.3 | correct - builds the React SPA framework chosen here |
| s4.2-s4.8 online slices | s4.1 and/or s2.5, s3.3 | correct. **Recommendation:** s4.5 (includes CORPT00C, whose `WRITEQ TD JOBS` submits TRANREPT) should also depend on s5.1 so the job-trigger contract exists before the report request is built. |
| s5.1 batch conventions | s2.2, s2.3, s3.3 | correct |
| s5.2-s5.6 batch and scheduler | s5.1 (+ s3.2/s3.4 where noted) | correct |
| s6.1-s6.5 validation and deploy | as planned | correct |

## 7. References

- `modernization/README.md`, `modernization/docs/COBOL-PROGRAM-MAPPING.md` on
  `devin/1789660764-carddemo-java-modernization`
- `ts/README.md` on `devin/1789654793-ts-foundation`
- `aws/migration-inventory.md`, `aws/frontend/` on `devin/1790619634-validation-parity`

# Contract: Engineering conventions

Status: **v1 (Discovery session)**. All modernization sessions (data migration, online services, batch,
frontend, infra, validation) MUST follow this file. Deviations require editing this file in the same PR and
calling the change out in the PR description.

## 1. Repository layout

All new code lives under the top-level `aws/` directory. Nothing under `app/`, `samples/`, `scripts/` or
`diagrams/` is modified.

| Path | Owner session | Content |
|---|---|---|
| `aws/contracts/` | Discovery (edits by any session, called out in PR) | These contracts |
| `aws/migration-inventory.md` | Discovery | Source inventory and replatform candidates |
| `aws/db/`, `aws/etl/` | Data migration | DDL (`aws/db/schema.sql` + `db2/`, `ims/`), EBCDIC/ASCII decoder + COPY loader, seed CSVs (`aws/etl/output/`), reconciliation tests |
| `aws/services/` | Online services | Spring Boot REST service(s) replacing CICS programs |
| `aws/batch/` | Batch | Spring Batch jobs replacing `CB*` programs + Step Functions definitions |
| `aws/frontend/` | Frontend | React SPA replacing BMS maps |
| `aws/infra/` | Infra | IaC (Aurora, S3, SQS, AWS Batch, Step Functions, ECS/Fargate, networking) |
| `aws/validation/` | Validation | Parity tests / reconciliation reports |

## 2. Java

| Item | Value |
|---|---|
| Language level | **Java 21** (`maven.compiler.release=21`) |
| Framework | **Spring Boot 3.x** (Spring Web, Spring Data JPA or JDBC, Spring Security, Spring Batch 5) |
| Build | **Maven** (multi-module allowed; each session owns its own module(s) under its directory) |
| Package root | **`com.carddemo`** |
| Sub-packages | `com.carddemo.<domain>` where domain ∈ `auth`, `menu`, `account`, `card`, `transaction`, `billpay`, `report`, `user`, `authorization`, `trantype`, `inquiry`, `batch.<jobname>`, `common` |
| Money | `java.math.BigDecimal`, scale from `data-model.md` (never `double`) |
| Dates | `java.time.LocalDate` for `DATE`, `java.time.LocalDateTime` for legacy timestamps (`TRAN-ORIG-TS` etc. carry no zone), `Instant` for technical timestamps |
| Rounding | COBOL `COMPUTE` without `ROUNDED` truncates: use `RoundingMode.DOWN` unless the source uses `ROUNDED` |
| JSON | Jackson, camelCase property names, ISO-8601 dates (`yyyy-MM-dd`), timestamps `yyyy-MM-dd'T'HH:mm:ss.SSSSSS` |
| Logging | SLF4J + Logback, JSON to stdout (CloudWatch Logs) |

## 3. Runtime configuration (environment variables)

Every deployable (service, batch job, loader) reads configuration **only** from these variables
(plus Spring defaults). Infra must provide all of them.

| Variable | Required by | Meaning |
|---|---|---|
| `DB_HOST` | all DB users | Aurora PostgreSQL writer endpoint |
| `DB_PORT` | all DB users | Port (default `5432`) |
| `DB_NAME` | all DB users | Database name (default `carddemo`) |
| `DB_USER` | all DB users | Database user |
| `DB_PASSWORD` | all DB users | Database password (injected from Secrets Manager by infra) |
| `DB_SCHEMA` | all DB users | Optional, default **`carddemo`**; must not be changed in deployed envs |
| `S3_BUCKET` | batch, loaders, report | Single bucket for all batch files (layout in `batch.md` §1.2) |
| `AWS_REGION` | all AWS SDK users | **The** AWS region variable (standard SDK variable). Do not introduce `AWS_DEFAULT_REGION`/`REGION` variants in code |
| `SQS_QUEUE_PREFIX` | messaging users | Optional queue-name prefix per environment, default `carddemo-` (see `messaging.md`) |
| `JWT_SECRET` | online services | HMAC key (≥ 256-bit) for signing JWTs, from Secrets Manager |
| `JWT_TTL_MINUTES` | online services | Token lifetime, default `60` |
| `SERVER_PORT` | services | Default **`8080`** |
| `API_BASE_URL` | frontend build | Base URL of the REST API, default `/api/v1` (same origin) |

Spring datasource mapping (for reference; not code):
`spring.datasource.url=jdbc:postgresql://${DB_HOST}:${DB_PORT}/${DB_NAME}?currentSchema=${DB_SCHEMA:carddemo}`.

## 4. Ports and endpoints

| Component | Port | Health |
|---|---|---|
| Spring Boot services | **8080** | `GET /actuator/health` (unauthenticated) |
| React dev server (Vite) | 5173 (dev only) | n/a |
| PostgreSQL | 5432 | n/a |

REST base path is **`/api/v1`** (see `api.md`).

## 5. Database

* Engine: Aurora PostgreSQL (PostgreSQL 15+ compatible). Local dev/test: PostgreSQL container of same major version.
* Schema: **`carddemo`**. All tables in `data-model.md` live in this schema.
* DDL: `aws/db/schema.sql` (idempotent; core + technical tables) plus `aws/db/db2/*.sql` and `aws/db/ims/*.sql`
  for the optional sub-apps. Services using Flyway use these files, in that order, as baseline `V1__carddemo.sql`;
  later changes are Flyway migrations `aws/db/migration/V<n>__<desc>.sql`.
  Only the data-migration session creates/changes tables; other sessions request changes via contract edits.
* Naming: snake_case, singular table names, rules in `data-model.md` §1.
* Transactions: one CICS task (one pseudo-conversational step that issues `REWRITE`/`WRITE`/`DELETE`,
  ended by `SYNCPOINT` or task end) = one database transaction.

## 6. Frontend

| Item | Value |
|---|---|
| Stack | **React 18 + TypeScript + Vite** |
| Routing | React Router; routes listed in `api.md` §9 and `migration-inventory.md` §6 |
| API client | `fetch`/axios against `API_BASE_URL`; JWT in `Authorization: Bearer <token>` header |
| PF-key parity | PF3 = back/exit, PF7/PF8 = previous/next page, PF5 = save/confirm, PF12 = cancel, ENTER = submit; expose as buttons plus keyboard shortcuts |

## 7. AWS resource naming

`carddemo-<env>-<component>` for infra resources (e.g. `carddemo-dev-aurora`, `carddemo-dev-batch-queue`).
Logical names used in contracts omit `<env>`; infra adds it. SQS queue names are given in `messaging.md`;
Batch job definitions and Step Functions state machines in `batch.md`.

## 8. Testing

* Unit tests: JUnit 5; integration tests with Testcontainers PostgreSQL.
* Frontend: Vitest + React Testing Library.
* Build command per Maven module: `mvn -B verify` with `JAVA_HOME` pointing at a JDK 21.
* Parity fixtures: the ASCII sample files in `app/data/ASCII/` are the canonical seed data for tests.

## 9. Security

* Passwords: legacy `SEC-USR-PWD PIC X(08)` is plain text in `USRSEC`. Target stores **BCrypt hashes**
  in `user_security.password_hash`; the loader hashes legacy values (see `data-model.md`).
* Roles: `SEC-USR-TYPE = 'A'` → `ADMIN`, `'U'` → `USER` (see `api.md` §2).
* No secrets in source control; all secrets from AWS Secrets Manager via env vars above.

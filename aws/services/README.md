# CardDemo online services (CICS → Spring Boot REST)

Java 21 / Spring Boot 3.3 service that replaces the CardDemo CICS online programs with the REST API defined in
[`aws/contracts/api.md`](../contracts/api.md). Data access is Spring JDBC (`JdbcClient`) against the canonical
PostgreSQL schema from [`aws/contracts/data-model.md`](../contracts/data-model.md); authentication is a stateless
HS256 JWT (Spring Security OAuth2 resource server). OpenAPI: [`openapi.yaml`](openapi.yaml).

## COBOL program → endpoint mapping

| Legacy program (transaction) | Endpoint(s) | Package | Notes |
|---|---|---|---|
| `COSGN00C` (`CC00`) | `POST /api/v1/auth/signon` | `auth` | user id + password upper-cased (`FUNCTION UPPER-CASE`); `SEC-USR-TYPE` `A`→`ADMIN`, `U`→`USER`; `nextRoute` `/admin` or `/menu` |
| `COMEN01C` (`CM00`) + `COMEN02Y` | `GET /api/v1/menus/main` | `menu` | 11 options; option 11 (pending authorizations) `installed=false` unless `carddemo.modules.authorizations-installed` |
| `COADM01C` (`CA00`) + `COADM02Y` | `GET /api/v1/menus/admin` | `menu` | ADMIN only; includes the DB2 transaction-type options |
| `COACTVWC` (`CAVW`) | `GET /api/v1/accounts/{acctId}` | `account` | account + customer (via first `card_xref` row) + cards |
| `COACTUPC` (`CAUP`) | `PUT /api/v1/accounts/{acctId}` | `account` | all screen edits (dates via `CSUTLDPY` rules, SSN, phone/area code, state + state/ZIP via `CSLKPCDY`, FICO, money); `FOR UPDATE NOWAIT` + `version` replaces the before/after image compare |
| `COCRDLIC` (`CCLI`) | `GET /api/v1/cards` | `card` | keyset paging, 7 rows; `acctId` / `cardNum` filters |
| `COCRDSLC` (`CCDL`) | `GET /api/v1/cards/{cardNum}` | `card` | optional `acctId` must match |
| `COCRDUPC` (`CCUP`) | `PUT /api/v1/cards/{cardNum}` | `card` | name/status/expiry edits; expiry day kept from the stored card (screen only edits month/year) |
| `COTRN00C` (`CT00`) | `GET /api/v1/transactions` | `transaction` | keyset paging, 10 rows |
| `COTRN01C` (`CT01`) | `GET /api/v1/transactions/{tranId}` | `transaction` | |
| `COTRN02C` (`CT02`) | `POST /api/v1/transactions` | `transaction` | acct→card via `card_xref`; `tran_id` = max+1 under `pg_advisory_xact_lock` (`TranIdAllocator`) |
| `COBIL00C` (`CB00`) | `GET /api/v1/bill-payments/{acctId}`, `POST /api/v1/bill-payments` | `billpay` | `SELECT … FOR UPDATE` on account, type `02`/cat `2` transaction, balance update — one DB transaction |
| `CORPT00C` (`CR00`) | `POST /api/v1/reports/transactions`, `GET /api/v1/reports/transactions/{requestId}` | `report` | `ReportPublisher` (no-op / SQS `carddemo-report-request`), `ReportStatusProvider` (local / Step Functions `DescribeExecution`) |
| `COUSR00C` (`CU00`) | `GET /api/v1/users` | `user` | ADMIN only, 10 rows |
| `COUSR01C` (`CU01`) | `POST /api/v1/users` | `user` | BCrypt hash of the upper-cased password |
| `COUSR02C` (`CU02`) | `GET/PUT /api/v1/users/{userId}` | `user` | `version` check; "Please modify to update ..." when unchanged |
| `COUSR03C` (`CU03`) | `DELETE /api/v1/users/{userId}` | `user` | 204 |
| `COTRTLIC` (`CTLI`) | `GET /api/v1/transaction-types`, `PUT`/`DELETE /api/v1/transaction-types/{typeCd}` | `trantype` | DB2 sub-app; writes ADMIN only; FK violation → 409 `INTEGRITY_VIOLATION` |
| `COTRTUPC` (`CTTU`) | `GET/POST/PUT/DELETE /api/v1/transaction-types[/{typeCd}]`, `GET …/{typeCd}/categories` | `trantype` | |
| `COACCT01` (MQ) | SQS `carddemo-acct-inquiry-request` → `carddemo-acct-inquiry-reply` | `messaging` | `SqsInquiryConsumer`, off by default |
| `CODATE01` (MQ) | SQS `carddemo-date-inquiry-request` → `carddemo-date-inquiry-reply` | `messaging` | same |

### Not implemented here (replatform / deferred)

| Module | Status |
|---|---|
| `COPAUA0C` (MQ authorization + IMS insert), `COPAUS0C`/`COPAUS1C`/`COPAUS2C` (pending-authorization screens, `/api/v1/authorizations`) | **Replatform candidate** (`aws/migration-inventory.md` §9): IMS HIDAM navigation + MQ/IMS/DB2 unit of work. Menu option 11 reports `installed=false`. |
| `COCRDSEC` (CSD `CDV1`) | No source in the repository. |

## Error handling and legacy messages

All 4xx/5xx responses use the contract envelope
`{ errorCode, message, fieldErrors[], legacyProgram, timestamp }`. `message` is the COBOL screen text
(e.g. `"Wrong Password. Try again ..."`, `"You have nothing to pay..."`,
`"Record changed by some one else. Please review"`), `legacyProgram` the program the rule was taken from.
`fieldErrors` lists every failed edit (legacy screens stop at the first one; that one is also `message`).

| HTTP | `errorCode` | Examples (legacy text) |
|---|---|---|
| 400 | `VALIDATION_ERROR` / `INVALID_REQUEST` | "Please enter User ID ...", "Acct ID can NOT be empty...", "Tran ID must be Numeric ...", "Invalid zip code for state" |
| 401 | `INVALID_CREDENTIALS` / `UNAUTHENTICATED` | "Wrong Password. Try again ...", "User not found. Try again ..." |
| 403 | `FORBIDDEN` | "No access - Admin Only option..." |
| 404 | `NOT_FOUND` | "Account ID NOT found...", "User ID NOT found...", "Did not find cards for this search condition" |
| 409 | `DUPLICATE` / `CONCURRENT_UPDATE` / `LOCKED` / `INTEGRITY_VIOLATION` | "User ID already exist...", "Record changed by some one else. Please review", "Could not lock account record for update" |
| 422 | `BUSINESS_RULE` | "You have nothing to pay...", "No change detected with respect to values fetched.", "Please modify to update ..." |
| 500 | `INTERNAL_ERROR` | "Unable to verify the User ...", "Unable to Write TDQ (JOBS)..." |

## Configuration

| Variable | Default | Purpose |
|---|---|---|
| `DB_HOST` / `DB_PORT` / `DB_NAME` | `localhost` / `5432` / `carddemo` | PostgreSQL / Aurora endpoint |
| `DB_USER` / `DB_PASSWORD` | `carddemo` / — | credentials |
| `DB_SCHEMA` | `carddemo` | `currentSchema` of the JDBC URL |
| `JWT_SECRET` | — (required; ≥ 32 bytes) | HS256 signing key |
| `JWT_TTL_MINUTES` | `60` | token lifetime |
| `SERVER_PORT` | `8080` | HTTP port |
| `AWS_REGION` | `us-east-1` | SQS / Step Functions clients |
| `SQS_QUEUE_PREFIX` | `carddemo-` | logical queue name prefix (`messaging.md`) |
| `S3_BUCKET` | — | report bucket (status key only) |
| `CARDDEMO_REPORTS_PUBLISHER` | `noop` | `noop` (log only) or `sqs` (`carddemo-report-request`) |
| `CARDDEMO_REPORTS_STATUS` | `local` | `local` (always `SUBMITTED`) or `stepfunctions` |
| `CARDDEMO_REPORTS_STATE_MACHINE_ARN` | — | `carddemo-report` state machine, required for `stepfunctions` |
| `CARDDEMO_MESSAGING_ENABLED` | `false` | start the `COACCT01`/`CODATE01` SQS request/reply consumers |
| `CARDDEMO_MODULES_AUTHORIZATIONS_INSTALLED` | `false` | mark menu option 11 installed |
| `SPRING_FLYWAY_ENABLED` | `false` (`true` in profile `local`) | apply the service's own schema (`src/main/resources/db/migration`) |
| `CARDDEMO_SEED_ASCII_DIR` | — (`../../app/data/ASCII` in profile `local`) | load `app/data/ASCII` fixtures into an **empty** database at startup |

In AWS the canonical schema is owned by the data-migration session (`aws/db/`); Flyway stays disabled there.
`V1__carddemo_online_schema.sql` is a local/test copy derived from `data-model.md` (history table
`flyway_schema_history_online`).

## Running locally

```bash
docker run -d --name carddemo-pg -e POSTGRES_DB=carddemo -e POSTGRES_USER=carddemo \
  -e POSTGRES_PASSWORD=carddemo -p 5432:5432 postgres:16-alpine

cd aws/services
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
DB_PASSWORD=carddemo mvn spring-boot:run -Dspring-boot.run.profiles=local
```

Profile `local` enables Flyway, seeds the sample data from `app/data/ASCII` (users from `DUSRSECJ.jcl`: `ADMIN001` …
`ADMIN005`, `USER0001` … `USER0005`, password `PASSWORD`) and uses a development JWT secret.

```bash
curl -s localhost:8080/actuator/health
TOKEN=$(curl -s -H 'Content-Type: application/json' -d '{"userId":"admin001","password":"password"}' \
  localhost:8080/api/v1/auth/signon | jq -r .token)
curl -s -H "Authorization: Bearer $TOKEN" localhost:8080/api/v1/accounts/00000000001
curl -s -H "Authorization: Bearer $TOKEN" 'localhost:8080/api/v1/cards?pageSize=3'
```

### Container

```bash
docker build -t carddemo-online-services aws/services
# behind a rate-limited Maven Central: --build-arg MAVEN_MIRROR_URL=https://<mirror>/maven2/
docker run -p 8080:8080 -e DB_HOST=... -e DB_USER=... -e DB_PASSWORD=... -e JWT_SECRET=... carddemo-online-services
```

## Tests

```bash
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify
```

Integration tests (`*IntegrationTest`) start PostgreSQL 16 with Testcontainers (Docker required), apply the Flyway
schema and re-seed from `app/data/ASCII` before every test. They cover each endpoint's happy path and the main
validation failures, role restrictions, keyset paging, optimistic concurrency, concurrent `tran_id` allocation and
concurrent bill payments. `FixedRecordTest` covers the fixed-width / overpunch parser.

## Package layout

```
com.carddemo
├── common      error envelope, legacy messages, paging, COBOL edit routines (CSUTLDPY / CSLKPCDY)
├── security    JWT issue/verify, role rules
├── auth menu account card transaction billpay report user trantype   one package per legacy area
├── messaging   COACCT01 / CODATE01 SQS request/reply
└── seed        app/data/ASCII fixed-width loader (local/test only)
```

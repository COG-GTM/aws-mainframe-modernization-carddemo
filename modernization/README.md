# CardDemo modernization (Java 21 + Spring Boot)

A rewrite of the CardDemo COBOL/CICS/VSAM/JCL application as domain microservices on Aurora
PostgreSQL. The mainframe sources under `app/` are untouched and remain the reference for
migration validation.

## Modules

| Module | Port | Legacy origin | Status |
| --- | --- | --- | --- |
| `common` | - | shared error envelope, pagination, COBOL value helpers | complete |
| `auth-service` | 8080 | CSUSR01Y, COSGN00C, COUSR00C-03C | complete for signon + user CRUD |
| `customer-service` | 8081 | CVCUS01Y | baseline read/update |
| `account-service` | 8082 | CVACT01Y, COACTVWC, COACTUPC, account side of CBTRN02C and CBACT04C | reference vertical slice |
| `card-service` | 8083 | CVACT02Y, CVACT03Y, COCRDLIC/SLC/UPC | reference vertical slice |
| `transaction-service` | 8084 | CVTRA01Y-06Y, COTRN00C/01C/02C, COBIL00C, CBTRN02C, CBACT04C | online API + both batch ports |

Not built (documented, deliberately left as stubs): reports (CORPT00C, CBTRN03C), statements
(CBSTM03A/B), the MQ replacement wiring, and a SPA front end. See `docs/API-CONTRACT.md`.

## Documentation

- `docs/COPYBOOK-TO-SCHEMA-MAPPING.md` — every PIC clause of the eleven copybooks mapped to a
  PostgreSQL column, with key and alternate index handling.
- `docs/COBOL-PROGRAM-MAPPING.md` — CICS transaction and batch program to service mapping, the
  preserved business rules, and the Amazon MQ / SQS / SNS design.
- `docs/API-CONTRACT.md` — the REST contract that replaces the BMS screens.

## Design notes

- One schema per service (`auth`, `customer`, `account`, `card`, `transaction`) in a single
  Aurora PostgreSQL cluster. Foreign keys are used inside a schema (for example `card_xref.card_num`
  to `cards.card_num`); relationships that cross a service boundary are carried by id only.
- VSAM KSDS keys become primary keys, alternate indexes (CXACAIX, CARDAIX) become secondary
  indexes or lookup endpoints.
- `S9(n)V99` fields are `NUMERIC(n+2,2)` and `BigDecimal`; no floating point touches money.
- CICS READ for UPDATE record locking becomes JPA `@Version` optimistic locking.
- The transaction service calls account-service and card-service over HTTP through the
  `AccountGateway` and `CardGateway` seams, which is where a saga or outbox belongs when the
  posting flow moves to events.
- Legacy plaintext `SEC-USR-PWD` is not carried over: passwords are BCrypt hashes. The seeded
  demo hash is a throwaway local value and must be replaced before any real deployment.
- `TRAN-ID` came from a counter in the transaction file. A counter cannot be shared by several
  service replicas, so ids are 16 random base 36 characters (about 2^82.7 values).

## Security boundary of this checkout

This stack is a migration reference that runs on a laptop, not a deployable system. Before any
non-local use it needs:

- Authentication and authorisation on every endpoint. Signon verifies a BCrypt hash but issues no
  token, so user administration, account updates, postings, bill payments and the batch triggers
  are currently open to anyone who can reach the port.
- TLS (ideally mTLS or a service mesh) between transaction-service and account/card-service; the
  gateways speak plain HTTP today.
- Real database credentials. The services read `*_DB_USER` and `*_DB_PASSWORD` from the
  environment with no fallback; `docker-compose.yml` fills them with `carddemo`/`carddemo` for the
  local demo only and publishes PostgreSQL on `127.0.0.1` alone.

## Known limitation: cross service batch retries

Transaction posting updates account-service over HTTP and then writes its own rows. If the local
write fails after the remote update committed, a retry posts the amount twice. Making this safe
needs an idempotency key on the posting endpoint plus an outbox or saga; the `AccountGateway` seam
is where that belongs.

## Build and test

```bash
cd modernization
mvn -B verify
```

Tests run against in memory H2 with the Flyway migrations disabled and Hibernate DDL applied, so
no database is needed for the suite.

## Run locally

```bash
cd modernization
docker compose up --build
```

This starts PostgreSQL and all five services; Flyway creates and seeds each schema on startup.
On networks that cannot reach Maven Central directly, set `MAVEN_MIRROR_URL` before `docker compose
up --build` and the image build uses it as the `central` mirror.
Without Docker, start PostgreSQL yourself and run a single service with:

```bash
mvn -pl account-service -am spring-boot:run
```

Swagger UI for each service is at `http://localhost:<port>/swagger-ui.html`.

## Batch jobs

The JCL jobs are Spring Batch jobs launched over HTTP so an external scheduler (EventBridge,
Airflow, Control-M) keeps the mainframe cadence:

```bash
curl -X POST http://localhost:8084/api/v1/batch/post-transactions     # POSTTRAN / CBTRN02C
curl -X POST http://localhost:8084/api/v1/batch/interest-calculation  # INTCALC  / CBACT04C
```

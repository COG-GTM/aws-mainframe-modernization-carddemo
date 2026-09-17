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
  Aurora PostgreSQL cluster; no cross service foreign keys, relationships are carried by id.
- VSAM KSDS keys become primary keys, alternate indexes (CXACAIX, CARDAIX) become secondary
  indexes or lookup endpoints.
- `S9(n)V99` fields are `NUMERIC(n+2,2)` and `BigDecimal`; no floating point touches money.
- CICS READ for UPDATE record locking becomes JPA `@Version` optimistic locking.
- The transaction service calls account-service and card-service over HTTP through the
  `AccountGateway` and `CardGateway` seams, which is where a saga or outbox belongs when the
  posting flow moves to events.
- Legacy plaintext `SEC-USR-PWD` is not carried over: passwords are BCrypt hashes. The seeded
  demo hash is a throwaway local value and must be replaced before any real deployment.

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

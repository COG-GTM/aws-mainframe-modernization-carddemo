# CardDemo Java

Java 21 / Spring Boot 3 migration of the CardDemo COBOL application in `../app`.

## Modules

| Module | Purpose |
|---|---|
| `carddemo-domain` | Business model (accounts, cards, customers, transactions); no framework dependencies |
| `carddemo-legacy-io` | Copybook codec: fixed-width records, zoned decimal, COMP-3, EBCDIC cp037 |
| `carddemo-data` | PostgreSQL persistence: Flyway migrations (`db/migration`) and JPA entities replacing the VSAM files |
| `carddemo-batch` | Spring Batch jobs replacing the business JCL (POSTTRAN, INTCALC, TRANREPT, CREASTMT, ...) |
| `carddemo-api` | REST API replacing the CICS online transactions |
| `carddemo-web` | Web UI for the online screens |

Dependencies: `legacy-io` and `data` depend on `domain`; `batch` depends on `data` and `legacy-io`; `api` depends on `data`.

## Build

Requires JDK 21 and Docker (integration tests start PostgreSQL 16 with Testcontainers).

```sh
cd java
./mvnw verify
```

`*Test` classes run as unit tests (Surefire); `*IT` classes run as integration tests (Failsafe).
Integration tests in any module get a database by importing
`com.carddemo.data.PostgresTestcontainersConfiguration` from the `carddemo-data` test-jar.

## Running

`carddemo-api` and `carddemo-batch` read the database from `BANK_CARDDEMO_DB_URL` (default
`jdbc:postgresql://localhost:5432/carddemo`), `BANK_CARDDEMO_DB_USER` and `BANK_CARDDEMO_DB_PASSWORD`; the credentials have no
defaults and must be set. Only `/actuator/health` is exposed until Spring Security is added.

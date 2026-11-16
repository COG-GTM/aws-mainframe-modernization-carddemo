# carddemo-java

Java 21 / Spring Boot 3 port of the CardDemo COBOL application in [`../app`](../app).
The COBOL stays untouched as the reference; everything in this port is additive under `carddemo-java/`.

## Modules

| Module | Purpose | Depends on |
|---|---|---|
| `carddemo-domain` | Records, enums and business rules; framework-free (enforced by the build) | – |
| `carddemo-legacy-io` | Copybook codec for the fixed-width files (zoned decimal, COMP-3, ASCII / EBCDIC cp037) | domain |
| `carddemo-data` | PostgreSQL persistence replacing VSAM: Flyway migrations (`db/migration`), JPA repositories | domain |
| `carddemo-batch` | Spring Batch app, one job per business JCL (POSTTRAN, INTCALC, TRANREPT, CREASTMT, ...) | data, legacy-io |
| `carddemo-api` | Spring Boot REST app replacing the CICS/BMS transactions; serves `carddemo-web` | data, web |
| `carddemo-web` | Web UI for the online screens (Phase 3) | – |

`golden-master/` (GnuCOBOL reference outputs for the batch jobs) is a separate harness, not a Maven module.

Architecture decisions are recorded in [`docs/adr/`](docs/adr/).

## Build

Requires JDK 21 and Docker: integration tests start PostgreSQL 16 with Testcontainers.

```sh
cd carddemo-java
./mvnw verify
```

- `*Test` classes are unit tests (Surefire); `*IT` classes are integration tests (Failsafe).
- An integration test in any module gets a database with
  `@Import(com.carddemo.data.test.PostgresTestcontainersConfiguration.class)` from the `carddemo-data` test-jar.
- Override the image with `-Dpostgres.image=...`.
- CI: [`.github/workflows/carddemo-java-ci.yml`](../.github/workflows/carddemo-java-ci.yml) runs `./mvnw verify` on changes under `carddemo-java/`.

## Running

Both apps read `CARDDEMO_DB_URL` (default `jdbc:postgresql://localhost:5432/carddemo`), `CARDDEMO_DB_USER`
(default `carddemo`) and `CARDDEMO_DB_PASSWORD`.

```sh
java -jar carddemo-api/target/carddemo-api-0.1.0-SNAPSHOT.jar      # /actuator/health only, for now
java -jar carddemo-batch/target/carddemo-batch-0.1.0-SNAPSHOT.jar --spring.batch.job.name=<JOB>
```

## Rules for every PR

- Additive under `carddemo-java/`; `app/` is never modified, so each PR merges alone.
- Batch jobs match the golden master field for field (byte for byte for fixed-width output).
- Online endpoints carry REST flow tests whose expected messages come from the COBOL source.
- Money is `BigDecimal` with COBOL scale and truncation, never floating point; COBOL paragraph names
  are kept as method names for traceability.

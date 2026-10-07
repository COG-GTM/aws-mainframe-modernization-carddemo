# CardDemo on Java 21

Java 21 rewrite of the CardDemo core application (`app/`: CICS online programs + JCL batch). It is one
Spring Boot application, a **modular monolith**: one domain package per business area, with dependencies
between packages enforced by ArchUnit at build time. See
[ADR-0001](../docs/modernization/adr/ADR-0001-modular-monolith.md) and the
[ADR index](../docs/modernization/adr/README.md) for the COBOL → Java mapping rules every later ticket follows.

## Build

**Java 21 is required, and the default JDK on the Devin machines is not 21.** `java` defaults to 11 and
plain `mvn` picks up another JDK (17, or 25 on some images), so always set `JAVA_HOME` explicitly:

```bash
cd modernization
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify
```

`maven-enforcer-plugin` (`requireJavaVersion [21,22)`, `requireMavenVersion 3.6.3+`) fails the build with
this hint if another JDK is used. `mvn verify` runs:

| Phase | What | Needs |
| --- | --- | --- |
| `test` (Surefire) | unit tests, ArchUnit module rules (+ self-test on broken fixtures), golden-profile clock test | JDK 21 |
| `integration-test` (Failsafe, `*IT`) | `CardDemoApplicationIT`: boots the app on PostgreSQL 16 via Testcontainers, checks Flyway, JPA, Spring Batch job repository, actuator health, OpenAPI | Docker |

## Layout

```
modernization/
├── pom.xml                      parent: spring-boot-starter-parent 3.3.x, Java 21, enforcer, BOMs
└── carddemo-app/                the single Spring Boot application
    └── src/main/java/com/carddemo/
        ├── CardDemoApplication.java
        ├── common/              shared kernel: AbendException, CICS RESP exceptions + HTTP mapping, business Clock
        ├── customer/            CUSTDAT                                  (may use: common)
        ├── user/                USRSEC, sign-on, menus, COUSR0*C         (may use: common)
        ├── card/                CARDDAT, CARDAIX, CCXREF, CXACAIX        (may use: common)
        ├── account/             ACCTDAT, COACTVWC, COACTUPC              (may use: common, card, customer)
        ├── transaction/         TRANSACT, TCATBALF, DISCGRP, TRANTYPE/CATG, COTRN0*C, COBIL00C
        │                                                                 (may use: common, account, card, customer)
        └── batch/               one Spring Batch job per JCL job         (may use: everything; nobody uses batch)
```

Each domain keeps its non-public types in a `<domain>.internal` sub-package, which other domains may not use.
The rules live in `src/test/java/com/carddemo/architecture/ModularMonolithRules.java`.

Stack: Spring Web, Validation, Data JPA, Spring Batch 5, Flyway (PostgreSQL), springdoc-openapi 2,
Actuator; tests with JUnit 5, Spring Boot Test, Spring Batch Test, Testcontainers, ArchUnit.

Flyway owns the whole schema, including the Spring Batch job repository (`V1__spring_batch_job_repository.sql`,
copied from `spring-batch-core`), so `spring.batch.jdbc.initialize-schema=never`. Jobs never launch at startup
(`spring.batch.job.enabled=false`); the in-app scheduler and REST trigger launch them.

## Run

```bash
docker run -d --name carddemo-db -p 5432:5432 \
  -e POSTGRES_DB=carddemo -e POSTGRES_USER=carddemo -e POSTGRES_PASSWORD=carddemo postgres:16-alpine
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B -pl carddemo-app spring-boot:run
# http://localhost:8080/actuator/health   http://localhost:8080/swagger-ui.html
```

Connection settings: `CARDDEMO_DB_URL`, `CARDDEMO_DB_USER`, `CARDDEMO_DB_PASSWORD`.

## Reproducing the COBOL baseline (golden profile)

The GnuCOBOL baseline (`scripts/baseline/`, outputs in `docs/validation/baseline/`) runs with a frozen clock and
pinned JCL parameters. The `golden` Spring profile pins the same values
([ADR-0014](../docs/modernization/adr/ADR-0014-baseline-clock-pin.md)):

| Baseline | Java (`application-golden.yml`) |
| --- | --- |
| `COB_CURRENT_DATE=2022/07/06 00:00:00.00` | `carddemo.clock.fixed=2022-07-06T00:00:00` (injectable `java.time.Clock`) |
| `INTCALC` `PARM='2022071800'` | `carddemo.baseline.intcalc-parm-date=2022071800` |
| `TRANREPT` / `DATEPARM` `2022-01-01`..`2022-07-06` | `carddemo.baseline.tranrept-start-date` / `-end-date` |
| `WAITSTEP` `SYSIN 00003600` | `carddemo.baseline.waitstep-centiseconds=3600` |

Code must take dates and times from the injected `Clock`, never from `LocalDate.now()` without one.

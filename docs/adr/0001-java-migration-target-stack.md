# ADR 0001: Target stack for the CardDemo COBOL to Java migration

- Status: Accepted
- Date: 2026-11-16
- Deciders: Taylor Curran (approved the migration plan with its defaults)

## Context

CardDemo is a mainframe application: COBOL batch programs driven by JCL, CICS online transactions with BMS screens,
and VSAM KSDS files for storage. We are migrating it to Java in small PRs that can each be merged on their own. This
record captures the stack and layout decisions so that later PRs build on the same base.

## Decisions

1. **Language: Java 21**, the current LTS release.
2. **Framework: Spring Boot 3**, with Spring Web for the API and Spring Boot Actuator for health checks.
3. **Batch: Spring Batch.** Each business JCL job becomes a Spring Batch job. IDCAMS, REPRO and SORT steps become
   SQL or Java steps, and the Control-M job chain is rebuilt as a Spring Batch job flow.
4. **Persistence: Spring Data JPA, with Flyway managing the schema.** Hibernate runs with `ddl-auto: validate`, so
   only Flyway migrations can change the schema.
5. **Database: PostgreSQL** replaces VSAM. Each KSDS becomes one table that keeps the original keys, each alternate
   index becomes a secondary index, and monetary `S9(n)V99` fields are stored as `NUMERIC` and read as `BigDecimal`.
   Integration tests run against PostgreSQL through Testcontainers.
6. **Online UI: a REST API plus a simple React front end.** Each CICS transaction becomes REST endpoints and a React
   screen.
7. **Location: a new top-level `java/` directory in this repository**, built with Maven as a multi-module project:
   `carddemo-domain`, `carddemo-legacy-io`, `carddemo-data`, `carddemo-batch`, `carddemo-api` and `carddemo-web`.
   Every PR goes on its own new branch.

## Options considered

- UI: REST only, or server-rendered pages (Thymeleaf). We chose REST plus React.
- Location: a separate repository. We chose `java/` in this repository so the COBOL source stays alongside the Java
  code for parity checks.

## Consequences

- Building the project needs JDK 21 and Maven 3.9 or later. The `java/mvnw` wrapper provides Maven, and the Maven
  Enforcer plugin checks both versions.
- Integration tests need Docker for Testcontainers, both locally and in CI.
- Spring Security will be added together with the sign-on transaction. Until then, `/actuator/health` is the only
  exposed endpoint.

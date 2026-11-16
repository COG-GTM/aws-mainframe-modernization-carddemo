# ADR 0001: Java 21, Spring Boot 3, JPA, Flyway and PostgreSQL 16 for the CardDemo port

- Status: Accepted
- Date: 2026-11-16

## Context

CardDemo runs on z/OS as COBOL batch (JCL), CICS/BMS online transactions, and VSAM KSDS datasets
with alternate indexes. Its behaviour is being ported, one small PR at a time, into `carddemo-java/`. The COBOL in
`app/` stays as the reference. The port needs a mainstream runtime, a batch framework with restart and
step semantics close to JCL, a relational store that can hold the VSAM data with its keys, and
schema changes that are versioned and reviewable.

## Decision

- **Java 21 (LTS)**: records and sealed types fit COBOL record layouts, and `BigDecimal` covers
  packed and zoned decimal arithmetic.
- **Spring Boot 3**: two deployables. `carddemo-api` serves REST in place of CICS/BMS, and a token replaces
  the COMMAREA. `carddemo-batch` runs Spring Batch, one job per JCL program.
- **JPA (Hibernate)** in `carddemo-data`: one entity and repository per VSAM dataset, with alternate
  indexes as database indexes. `carddemo-domain` stays framework-free, and an enforcer rule backs that.
- **Flyway**: owns the schema from V0, and every change is a versioned SQL migration. Hibernate only
  validates the schema (`ddl-auto: validate`).
- **PostgreSQL 16**: one database for online and batch, plus the Spring Batch job repository.
  Tests run against the same engine through Testcontainers, never an in-memory substitute.

## Consequences

- Every integration test needs Docker, locally and in CI.
- Monetary and numeric fields must map COBOL PIC scale to `NUMERIC(p,s)` and `BigDecimal`. Floating
  point is never used.
- Upgrading Spring Boot moves the managed dependency versions. Version pins in `pom.xml` (such as
  `tomcat.version` for a CVE) should be dropped once Boot manages a fixed version.
- Alternatives not chosen: Quarkus or Micronaut (smaller ecosystem for batch), plain JDBC (more
  hand-written mapping for 11 datasets), and Liquibase (XML/YAML changelogs, where Flyway keeps reviewable SQL).

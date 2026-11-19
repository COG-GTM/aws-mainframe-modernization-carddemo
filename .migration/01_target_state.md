# 01 - Target state (digest)

Canonical: `functional/CARDDEMO/CardDemo_target_state.md` (**CONFIRMED**, STOP A, 2026-11-19). If they diverge, the canonical file wins.

- CORE: Java 21, Spring Boot 3.3, Maven multi-module `modernization/` (`carddemo-common`, `carddemo-app`), JUnit 5 + Testcontainers `postgres:16`, no Lombok/H2/`double` money.
- BATCH: Spring Batch 5, job `POSTTRAN`, step `STEP15`, chunk 1, restartable; DALYTRAN and DALYREJS stay files; RC 0/4 from source, 16 abend, 12 usage.
- DATA: PostgreSQL 16 + Flyway, KSDS TRANSACT / CARDXREF / ACCTDATA / TCATBALF → tables; no stored procedures; no outbound integration.
- ONLINE, SUBTRANSACTION: N/A.

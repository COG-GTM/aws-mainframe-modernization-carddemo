# CardDemo modernization (Java 21 / Spring Boot 3)

Cloud-native re-implementation of the CardDemo CICS/VSAM application. Design, runbook and traceability are in
[`docs/modernization`](../docs/modernization/README.md). The legacy sources in `app/` are not touched.

| Module | Purpose |
|---|---|
| `common` | Flyway DDL, JPA entities (one per copybook), JWT security, keyset browsing, error model |
| `data-migration` | VSAM unload (EBCDIC/ASCII, zoned, COMP-3) to PostgreSQL loader with reconciliation |
| `identity-service` :8081 | COSGN00C, COMEN01C, COADM01C, COUSR00C-03C |
| `account-service` :8082 | COACTVWC, COACTUPC |
| `card-service` :8083 | COCRDLIC, COCRDSLC, COCRDUPC |
| `transaction-service` :8084 | COTRN00C-02C, COBIL00C, CORPT00C |
| `batch-service` :8085 | POSTTRAN, INTCALC, TRANREPT, CREASTMT (Spring Batch) |

```bash
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify       # parity tests
mvn -B package -DskipTests && docker compose up --build           # full stack, loads app/data/ASCII
curl -s -XPOST localhost:8081/api/v1/auth/signon -H 'Content-Type: application/json' \
  -d '{"userId":"ADMIN001","password":"PASSWORD"}'
```

Secrets: `CARDDEMO_DB_PASSWORD` and `CARDDEMO_JWT_SECRET` come from the environment (Secrets Manager on AWS). The
values in `docker-compose.yml` are local-only defaults.

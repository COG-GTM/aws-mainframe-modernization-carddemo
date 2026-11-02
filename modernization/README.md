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

## Runbook

Everything below runs from `modernization/`. Prerequisites: JDK 21, Maven 3.9, Docker with Compose v2.

### Ports

| Service | Port | Health | Swagger UI |
| --- | --- | --- | --- |
| auth-service | 8080 | `http://localhost:8080/actuator/health` | `http://localhost:8080/swagger-ui.html` |
| customer-service | 8081 | `http://localhost:8081/actuator/health` | `http://localhost:8081/swagger-ui.html` |
| account-service | 8082 | `http://localhost:8082/actuator/health` | `http://localhost:8082/swagger-ui.html` |
| card-service | 8083 | `http://localhost:8083/actuator/health` | `http://localhost:8083/swagger-ui.html` |
| transaction-service | 8084 | `http://localhost:8084/actuator/health` | `http://localhost:8084/swagger-ui.html` |
| PostgreSQL 16 | 5432 (127.0.0.1 only) | `pg_isready` | - |

Each service also serves its OpenAPI document at `/v3/api-docs`.

### 1. Build and test

```bash
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify   # JAVA_HOME only where the default JDK is not 21
```

Tests run against PostgreSQL 16 in a Testcontainers container (Testcontainers 1.21.4) with the Flyway
migrations applied and Hibernate in `validate` mode, so `mvn -B verify` needs a running Docker daemon.

CI (`.github/workflows/modernization-verify.yml`) runs on every PR touching `modernization/**`:

- `verify`: `mvn -B verify` on JDK 21, with Docker available for Testcontainers.
- `compose-smoke`: `docker compose up -d --build --wait` on a fresh database, then
  `RUN_BATCH=1 scripts/smoke.sh`.

### 2. Start the stack

```bash
docker compose up -d --build
docker compose logs -f   # wait for five "Started ...Application" lines
```

PostgreSQL starts first (health-checked); each service then runs its Flyway migrations into its own
schema (`auth`, `customer`, `account`, `card`, `transaction`) and seeds demo rows.

- Maven Central blocked (for example on a Devin VM): `export MAVEN_MIRROR_URL=https://<mirror>/maven2/`
  before `docker compose up --build`; the image build uses it as the `central` mirror.
- A host port already taken: remap it with an override file kept out of the repo, for example
  `docker-compose.override.yml` (picked up automatically, listed in `.gitignore`):

  ```yaml
  services:
    transaction-service:
      ports: !override
        - "8094:8084"
  ```

  `!override` is required; a plain `ports:` list is appended to the original and still binds 8084.

Without Docker for the services, start PostgreSQL yourself and run one service with
`mvn -pl account-service -am spring-boot:run`, setting that service's `*_DB_URL`, `*_DB_USER` and
`*_DB_PASSWORD` (see `docker-compose.yml`).

Stop with `docker compose down` (add `-v` to drop the database volume and re-seed next time).

### 3. Smoke test

```bash
./scripts/smoke.sh                      # TRANSACTION_PORT=8094 ./scripts/smoke.sh after a remap
```

Waits for every `/actuator/health` to be `UP`, fetches each `/v3/api-docs` and `/swagger-ui.html`
and signs on as `ADMIN001`; this changes no data. `RUN_BATCH=1 ./scripts/smoke.sh` also runs both
batch jobs, which do change data (pending daily transactions are posted, and interest is charged
again on the current category balances each run), so use it only on a fresh database
(`docker compose down -v` first), as CI does.

### 4. Load sample data

Placeholder until the data migration phase. Today the only data is what the Flyway `V2__seed_*`
migrations insert: users `ADMIN001` / `USER0001` (password `Password1`, local demo only), accounts
`11111111111`, `22222222222`, `33333333333`, their customers, cards and cross-references, and the
transaction type, category and disclosure group reference rows. The loader for the EBCDIC VSAM
extracts under `app/data/EBCDIC` is not built yet.

To give the posting job something to do, insert a daily transaction (the DALYTRAN input of
CBTRN02C) directly:

```bash
docker compose exec -T postgres psql -U carddemo -d carddemo -c "
INSERT INTO transaction.daily_transactions
  (tran_id, type_cd, cat_cd, source, description, amount, card_num, orig_ts)
VALUES ('RUNBOOK000000001', '01', 1, 'POS TERM', 'Runbook purchase', 25.00,
        '4111111111111111', now());"
```

### 5. Run a batch job

The JCL jobs are Spring Batch jobs in transaction-service, started over HTTP so an external
scheduler (EventBridge, Airflow, Control-M) keeps the mainframe cadence:

```bash
curl -X POST http://localhost:8084/api/v1/batch/post-transactions     # POSTTRAN / CBTRN02C
curl -X POST http://localhost:8084/api/v1/batch/interest-calculation  # INTCALC  / CBACT04C
```

Each call runs the job synchronously and returns `jobName`, `executionId`, `status` and the step
counters. Rejected postings land in `transaction.transaction_rejects` (DALYREJS).

### 6. Call the services

```bash
curl -s -X POST http://localhost:8080/api/v1/auth/signon \
  -H 'Content-Type: application/json' -d '{"userId":"ADMIN001","password":"Password1"}'
curl -s http://localhost:8081/api/v1/customers
curl -s http://localhost:8082/api/v1/accounts/11111111111
curl -s 'http://localhost:8083/api/v1/cards?accountId=11111111111'
curl -s 'http://localhost:8084/api/v1/transactions?cardNumber=4111111111111111'
```

The full contract is in `docs/API-CONTRACT.md` and in each service's Swagger UI.

# CardDemo modernization (Java 21 + Spring Boot)

A rewrite of the CardDemo COBOL/CICS/VSAM/JCL application as domain microservices on Aurora
PostgreSQL. The mainframe sources under `app/` are untouched and remain the reference for
migration validation.

## Modules

| Module | Port | Legacy origin | Status |
| --- | --- | --- | --- |
| `common` | - | shared error envelope, pagination, COBOL value helpers, record I/O, batch launch framework (`com.carddemo.common.batch`) | complete |
| `auth-service` | 8080 | CSUSR01Y, COSGN00C, COMEN01C, COADM01C, COUSR00C-03C | signon + bearer token, `GET /api/v1/menu` (COMEN01C, COADM01C), admin-only user CRUD |
| `customer-service` | 8081 | CVCUS01Y | baseline read/update |
| `account-service` | 8082 | CVACT01Y, COACTVWC, COACTUPC, account side of CBTRN02C and CBACT04C | reference vertical slice |
| `card-service` | 8083 | CVACT02Y, CVACT03Y, COCRDLIC/SLC/UPC | reference vertical slice |
| `transaction-service` | 8084 | CVTRA01Y-06Y, COTRN00C/01C/02C, COBIL00C, CBTRN02C, CBACT04C | online API + POSTTRAN, INTCALC, COMBTRAN, CBTRN01C jobs |
| `batch-service` | 8085 | CBTRN03C, CBSTM03A/B, CBEXPORT/CBIMPORT, CBACT01C-03C, CBCUS01C, COBSWAIT | CREASTMT, CBEXPORT, CBIMPORT, READACCT/CARD/CUST/XREF, WAITSTEP jobs (TRANREPT: UNT149-14) |
| `data-loader` | - | the `app/data` sample datasets (nine ASCII extracts + EBCDIC USRSEC) | `load-data` command + reconciliation |

The MQ replacement wiring and a SPA front end are
not built. See `docs/API-CONTRACT.md` and `docs/migration/jcl-mapping.md`.

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
- Legacy plaintext `SEC-USR-PWD` is not carried over: passwords are BCrypt hashes. The loader
  hashes the USRSEC sample passwords (all `PASSWORD`); they are demo values and must not reach any
  real deployment.
- `TRAN-ID` came from a counter in the transaction file. A counter cannot be shared by several
  service replicas, so ids are 16 random base 36 characters (about 2^82.7 values).

## Security boundary of this checkout

This stack is a migration reference that runs on a laptop, not a deployable system. Before any
non-local use it needs:

- A real token key. Every `/api/**` endpoint requires the HS256 bearer token from sign-on (see
  `docs/migration/architecture.md` §3.1); `docker-compose.yml` signs with a dev-only default
  `CARDDEMO_TOKEN_SECRET`, so set your own value outside a laptop.
- TLS (ideally mTLS or a service mesh) between transaction-service and account/card-service; the
  gateways speak plain HTTP today.
- Real database credentials. The services read `*_DB_USER` and `*_DB_PASSWORD` from the
  environment with no fallback; `docker-compose.yml` fills them with `carddemo`/`carddemo` for the
  local demo only and publishes PostgreSQL on `127.0.0.1` alone.

## Cross service batch retries

Transaction posting updates account-service over HTTP and then writes its own rows. The posting
endpoint is idempotent per transaction id (`DALYTRAN-ID`, recorded in `account.account_postings` in
the same database transaction as the account update): if the local write fails after the remote
update committed, posting the same transaction again returns the first result instead of applying
the amount twice. The same id with another account, amount or date is refused with 409. What is
still not automatic is the retry itself: a failed POSTTRAN leaves the transaction unprocessed and
the next run posts it.

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
| batch-service | 8085 | `http://localhost:8085/actuator/health` | `http://localhost:8085/swagger-ui.html` |
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
- `compose-smoke`: `docker compose up -d --build --wait` on a fresh database, the data-loader,
  a CLI launch of POSTTRAN (exit code 4) and INTCALC (0), then `RUN_BATCH=1 scripts/smoke.sh`.

### 2. Start the stack

```bash
docker compose up -d --build
docker compose logs -f   # wait for five "Started ...Application" lines
```

PostgreSQL starts first (health-checked); each service then runs its Flyway migrations into its own
schema (`auth`, `customer`, `account`, `card`, `transaction`). The migrations create empty tables;
load the sample data next (section 4).

Databases created before the `V2__seed_*` migrations were removed still record version 2 in
`flyway_schema_history`, and Flyway validation stops the services ("applied migration not resolved
locally"). Recreate such a database once: `docker compose down -v`.

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

Stop with `docker compose down` (add `-v` to drop the database volume; reload the sample data after the next start).

### 3. Smoke test

```bash
./scripts/smoke.sh                      # TRANSACTION_PORT=8094 ./scripts/smoke.sh after a remap
```

Waits for every `/actuator/health` to be `UP`, fetches each `/v3/api-docs` and `/swagger-ui.html`
and signs on as `ADMIN001` / `PASSWORD` (a USRSEC user, so load the sample data first); this changes
no data. `RUN_BATCH=1 ./scripts/smoke.sh` also runs both
batch jobs, which do change data (pending daily transactions are posted, and interest is charged
again on the current category balances each run), so use it only on a fresh database
(`docker compose down -v` first), as CI does.

### 4. Load sample data

```bash
docker compose up -d --build --wait      # services create the empty schemas with Flyway
docker compose run --rm data-loader      # load app/data, then reconcile
```

`data-loader` (profile `tools`, not started by `up`) mounts `../app/data` read-only and loads, in one
transaction and in dependency order: customers, accounts, cards and cross references, transaction
types, categories and disclosure groups, category balances, daily transactions, users. Sources are
the ASCII extracts in `app/data/ASCII` (DALYTRAN from `dailytran.txt`); users come from the EBCDIC
`AWS.M2.CARDDEMO.USRSEC.PS`, which has no ASCII copy, with `SEC-USR-PWD` stored only as a BCrypt hash.
It then writes every row back as a fixed-width record, compares it field by field with the source and
prints the counts; the exit status is non-zero on any difference. CI's `compose-smoke` job runs the same
command before the smoke test.

| Table | Rows |
| --- | --- |
| `customer.customers` | 50 |
| `account.accounts` | 50 |
| `card.cards`, `card.card_xref` | 50, 50 |
| `transaction.transaction_types`, `transaction_categories`, `disclosure_groups` | 7, 18, 51 |
| `transaction.category_balances` | 50 |
| `transaction.daily_transactions` | 300 (pending, for the posting job) |
| `auth.users` | 10: `ADMIN001`-`ADMIN005` (admin), `USER0001`-`USER0005`; password `PASSWORD` |

- The loader refuses to load into tables that already have rows. `docker compose run --rm data-loader --replace`
  empties the ten target tables plus the batch outputs `transaction.transactions` and
  `transaction.transaction_rejects` (so reloaded daily transactions are not posted twice) and loads again.
- `--report FILE` writes the reconciliation report; the committed one is
  `docs/migration/validation/sample-data-reconciliation.md`, regenerated and checked by
  `SampleDataReconciliationTest` in `mvn verify`.
- Outside Compose: `java -jar data-loader/target/data-loader-*.jar --data-dir ../app/data` with
  `CARDDEMO_DB_URL`, `CARDDEMO_DB_USER` and `CARDDEMO_DB_PASSWORD` set.

### 5. Run a batch job

Each JCL job is a Spring Batch job registered under its JCL member name; the full JCL → Java
mapping and the Control-M order are in `docs/migration/jcl-mapping.md`. transaction-service runs
POSTTRAN (CBTRN02C), INTCALC (CBACT04C) and COMBTRAN (sort); batch-service runs TRANREPT (CBTRN03C), CREASTMT (CBSTM03A/B),
CBEXPORT, CBIMPORT, READACCT (CBACT01C), READCARD (CBACT02C), READCUST (CBCUS01C), READXREF (CBACT03C)
and WAITSTEP (COBSWAIT). batch-service jobs read the five service schemas read-only when no `--dd` is given.

**Command line** (what a scheduler or the golden-set harness calls; the process exits with the
job's return code):

```bash
java -jar transaction-service/target/transaction-service-*.jar --job=POSTTRAN --date=2022-07-18
java -jar transaction-service/target/transaction-service-*.jar --job=INTCALC --date=2022-07-18   # PARM defaults to 2022071800
java -jar transaction-service/target/transaction-service-*.jar --job=INTCALC --parm=2022071800
java -jar transaction-service/target/transaction-service-*.jar --job=COMBTRAN \
    --dd=TRANSACT.BKUP=/path/to/backup                                               # SYSTRAN(0) = latest INTCALC run
java -jar transaction-service/target/transaction-service-*.jar --job=POSTTRAN \
    --dd=DALYTRAN=../app/data/ASCII/dailytran.txt                                    # DALYTRAN from a file
java -jar batch-service/target/batch-service-*.jar --job=TRANREPT \
    --dd=TRANSACT=out/POSTTRAN/<ts>/TRANSACT --parm='2022-01-01 2022-07-06' \
    --dd=CARDXREF=../app/data/ASCII/cardxref.txt --dd=TRANTYPE=../app/data/ASCII/trantype.txt \
    --dd=TRANCATG=../app/data/ASCII/trancatg.txt                                     # every DD a file
java -jar batch-service/target/batch-service-*.jar --job=TRANREPT --dd=DATEPARM=dateparm.txt   # data from the services
java -jar batch-service/target/batch-service-*.jar --job=CREASTMT                  # statements from the tables
java -jar batch-service/target/batch-service-*.jar --job=CBEXPORT                  # tables -> EXPFILE
java -jar batch-service/target/batch-service-*.jar --job=CBIMPORT                  # latest CBEXPORT EXPFILE -> *OUT files
java -jar batch-service/target/batch-service-*.jar --job=READACCT \
    --dd=ACCTFILE=../app/data/ASCII/acctdata.txt                                     # dump a file instead of the table
```

TRANREPT takes DATEPARM as `--dd=DATEPARM=<file>` (its first record, `YYYY-MM-DD YYYY-MM-DD`) or as
`--parm='YYYY-MM-DD YYYY-MM-DD'` (the record text). TRANSACT, CARDXREF, TRANTYPE and TRANCATG are
read from `--dd=` files when given, otherwise from transaction-service and card-service
(`TRANSACTION_SERVICE_URL`, `CARD_SERVICE_URL`). The CLI needs `BATCH_DB_URL` / `BATCH_DB_USER` /
`BATCH_DB_PASSWORD` for the job repository.

The CLI needs the same database settings as the service (`TRANSACTION_DB_URL`, `TRANSACTION_DB_USER`,
`TRANSACTION_DB_PASSWORD`, and `ACCOUNT_SERVICE_URL` / `CARD_SERVICE_URL` for POSTTRAN) plus
`CARDDEMO_TOKEN_SECRET`: with no caller to forward, the account/card calls carry a token minted for
user `BATCH` (type `U`). Against the
compose stack: `docker compose run --rm -v "$PWD/batch-out:/batch-out" -e BATCH_OUTPUT_DIR=/batch-out
transaction-service --job=POSTTRAN --date=2022-07-18`.

| Option | Meaning |
| --- | --- |
| `--job=NAME` | JCL member (required): POSTTRAN, INTCALC, COMBTRAN, or CBTRN01C (no JCL member; needs `--dd=DALYTRAN=`, writes `SYSOUT`) on transaction-service; TRANREPT, CREASTMT, CBEXPORT, CBIMPORT, READACCT, READCARD, READCUST, READXREF, WAITSTEP on batch-service |
| `--date=YYYY-MM-DD` | run date (also `YYYYMMDD`); default today |
| `--parm=VALUE` | the `EXEC PARM=` value; default `<date as yyyyMMdd>00` |
| `--dd=DDNAME=path` | input file for a DD, repeatable (`*.txt` = line sequential, else fixed-length records) |
| `--output-dir=path` | root of the output generations; default `carddemo.batch.output-dir` (`BATCH_OUTPUT_DIR`, else `out`) |
| `--codepage=ascii\|ebcdic`, `--format=fixed\|line` | encoding and organization of the files written (default ASCII, fixed) |

Exit codes: the job's RC (highest step RC; POSTTRAN returns 4 when it rejected transactions, as
CBTRN02C does), 16 when a step abends (later steps do not run, `COND=(0,NE)`), 12 for a usage error.

**Output directory.** Every run (CLI or HTTP) writes one generation directory, the Java form of a
GDG `(+1)`: `<output-dir>/<JOB>/<yyyyMMdd-HHmmss-SSS>/` (UTC), one file per output DD named after
the DD (`<DD>.txt` with `--format=line`), fixed-width records via `RecordWriter`, plus `RETURN-CODE`:

```text
out/POSTTRAN/20220718-000000-000/DALYREJS   430-byte rejects of this run (DALYTRAN record + reason)
                                 TRANSACT   350-byte TRANFILE after the run, by TRAN-ID
                                 TCATBALF   50-byte category balances after the run
                                 RETURN-CODE
out/INTCALC/<ts>/SYSTRAN                    350-byte interest transactions of this run (TRAN-ID = PARM + 000001..)
out/COMBTRAN/<ts>/TRANSACT.COMBINED         TRANSACT.BKUP(0) + SYSTRAN(0) sorted by TRAN-ID
out/TRANREPT/<ts>/TRANSACT.BKUP             350-byte TRANSACT unload (STEP05R REPROC)
                  TRANSACT.DALY             the DATEPARM window, sorted by card number (STEP05R SORT)
                  TRANREPT                  133-byte report records (STEP10R CBTRN03C); absent when it abends
out/CREASTMT/<ts>/TRXFL.SEQ                  STEP010 sort output (card number + TRAN-ID, OUTREC)
                  STMTFILE, HTMLFILE        80-byte statements, 100-byte HTML (STATEMNT.PS / .HTML)
out/CBEXPORT/<ts>/EXPFILE                   500-byte CVEXPORT records
out/CBIMPORT/<ts>/CUSTOUT ACCTOUT XREFOUT TRNXOUT CARDOUT ERROUT
out/READACCT/<ts>/OUTFILE ARRYFILE VBRCFILE SYSOUT   (READCARD/CUST/XREF: SYSOUT)
```

**HTTP** (unchanged endpoints; same job, same output directory, reported as `outputDir`):

```bash
curl -X POST http://localhost:8084/api/v1/batch/post-transactions     # POSTTRAN / CBTRN02C
curl -X POST http://localhost:8084/api/v1/batch/interest-calculation  # INTCALC  / CBACT04C (?date=, ?parm=)
curl -X POST -H "Authorization: Bearer $TOKEN" 'http://localhost:8085/api/v1/batch/transaction-report?startDate=2022-01-01&endDate=2022-07-06'  # TRANREPT / CBTRN03C
# The submit forwards the caller's token to transaction-service and card-service; a --job=TRANREPT CLI launch signs as BATCH.
curl -X POST -H "Authorization: Bearer $TOKEN" http://localhost:8085/api/v1/batch/statements            # CREASTMT / CBSTM03A+B
curl -X POST -H "Authorization: Bearer $TOKEN" http://localhost:8085/api/v1/batch/export                # CBEXPORT
curl -X POST -H "Authorization: Bearer $TOKEN" http://localhost:8085/api/v1/batch/import                # CBIMPORT (latest CBEXPORT EXPFILE)
```

Each call runs the job synchronously and returns `jobName`, `executionId`, `status`, `returnCode`,
`outputDir`, the step list and the step counters (every `/api/**` call needs the bearer token, `$AUTH`
as in §6). Rejected postings also stay in
`transaction.transaction_rejects`.

### 6. Call the services

```bash
TOKEN=$(curl -s -X POST http://localhost:8080/api/v1/auth/signon \
  -H 'Content-Type: application/json' -d '{"userId":"ADMIN001","password":"PASSWORD"}' \
  | sed -n 's/.*"token":"\([^"]*\)".*/\1/p')
AUTH="Authorization: Bearer $TOKEN"
curl -s -H "$AUTH" http://localhost:8080/api/v1/menu
curl -s -H "$AUTH" http://localhost:8081/api/v1/customers
curl -s -H "$AUTH" http://localhost:8082/api/v1/accounts/00000000001
curl -s -H "$AUTH" 'http://localhost:8083/api/v1/cards?accountId=00000000001'
curl -s -H "$AUTH" 'http://localhost:8084/api/v1/transactions?cardNumber=9680294154603697'
```

The full contract is in `docs/API-CONTRACT.md` and in each service's Swagger UI.

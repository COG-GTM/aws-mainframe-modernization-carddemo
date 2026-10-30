# transaction-type-service

Java 21 / Spring Boot 3 service that implements the CardDemo Db2 transaction-type module (`app/app-transaction-type-db2`): `COTRTLIC` list/browse/delete, `COTRTUPC` add/update, `COBTUPDT` batch maintenance and the `TRANEXTR` extract. It was built from the specs in [`specs/aws-transform/transaction-type/`](../../specs/aws-transform/transaction-type/). Rule IDs (`BR-TT-nnn`) come from the [rules catalog](../../specs/aws-transform/transaction-type/BUSINESS-RULES.md).

## Run it
```bash
cp .env.example .env   # gitignored; set DB2_PASSWORD and the four TT_API_* values (no defaults are committed)
make db2-up     # IBM Db2 Community in Docker; applies ddl/*.ddl + loads ctl/DB2LT*.ctl (first start: a few minutes)
make run        # builds the jar with Java 21 and runs it on :8080 against Db2
# or: make up   # Db2 + service both in docker compose (jar is built on the host, then copied in)
make test       # unit + MockMvc tests (no Db2 needed)
make it         # + integration tests against the real Db2 container (-Pdb2-it)
make db2-sql Q="SELECT * FROM CARDDEMO.TRANSACTION_TYPE"
```
Swagger UI: http://localhost:8080/swagger-ui.html · health: `/actuator/health`.

### Credentials and access
`.env` is read by `docker compose` and the Makefile (`-include .env`); `make db2-up` stops with a message if `DB2_PASSWORD` is missing; `make up`/`make run` also need the four `TT_API_*` values. Compose only requires `DB2_PASSWORD`, so Db2 can start without API users.

| Variable | Used for |
|---|---|
| `DB2_PASSWORD` | `db2inst1` password of the Db2 container; passed to the service as `SPRING_DATASOURCE_PASSWORD` and to the ITs |
| `TT_API_USER` / `TT_API_PASSWORD` | HTTP Basic user with role MAINTAINER: every method on `/api/**` |
| `TT_API_READER_USER` / `TT_API_READER_PASSWORD` | HTTP Basic user with role READER: `GET /api/**` only (writes -> 403) |

The service refuses to start if any `TT_API_*` value is blank or both usernames are the same (`config/SecurityConfig`). Public without credentials: `/actuator/health`, `/swagger-ui.html`, `/swagger-ui/**`, `/v3/api-docs/**`; everything else returns 401 without credentials. In Swagger UI click **Authorize** and enter one of the two users. Unit tests and the Db2 ITs run on the `test` profile, which takes test-only users from `src/test/resources/application-test.yml`. The service has no test/debug endpoints; any added later must live in a `@Profile("testing")` controller (off by default; `SecurityTest` asserts `/api/v1/testing/**` is 404 and that no extra handler is registered).

Db2 notes: `db2/init/carddemo-init.sh` runs inside the container. It strips the z/OS-only `ERASE NO`/`CLOSE NO` clauses from the index DDL and drops the comment lines from the load cards. A marker file then makes the container healthy. The jar connects with the IBM JCC driver (`com.ibm.db2:jcc`).

## API
| Legacy | Endpoint |
|---|---|
| COTRTLIC Enter / PF7 / PF8 (+ filters) | `GET /api/v1/transaction-types?typeCode=&description=&direction=FIRST\|NEXT\|PREVIOUS&startKey=` |
| COTRTUPC Enter on key | `GET /api/v1/transaction-types/{code}` |
| COTRTUPC PF5 on new key | `POST /api/v1/transaction-types` |
| COTRTLIC `U`+F10 / COTRTUPC PF5 | `PUT /api/v1/transaction-types/{code}` (`createIfMissing=true` = COTRTUPC UPDATE->INSERT) |
| COTRTLIC `D`+F10 / COTRTUPC PF4+PF4 | `DELETE /api/v1/transaction-types/{code}?confirm=true` (without `confirm` -> 428) |
| child categories | `GET /api/v1/transaction-types/{code}/categories` |
| MNTTRDB2 / COBTUPDT | `POST /api/v1/transaction-types/batch` (text/plain, 53-byte records) |
| TRANEXTR STEP40 / STEP50 | `GET /api/v1/transaction-types/extract`, `GET /api/v1/transaction-type-categories/extract` |

Errors return `{status, error, ruleId, message, sqlCode}`. Status mapping: 400 validation, 404 not found, 409 duplicate / FK / lock, 422 no change, 428 confirmation required, 500 other SQLCODE, 503 Db2 down.

## Traceability (rule -> code -> test)
Paths are relative to `src/main/java/com/carddemo/transactiontype/` and `src/test/java/com/carddemo/transactiontype/`.

| Rule | Code | Test(s) |
|---|---|---|
| BR-TT-001 | `domain/TransactionTypeRules#normalizeTypeCode` | `TransactionTypeRulesTest` BR-TT-001 |
| BR-TT-002 | `TransactionTypeRules#normalizeTypeCode` | `TransactionTypeRulesTest` BR-TT-002, `TransactionTypeControllerTest#validation` |
| BR-TT-003 | `TransactionTypeRules#normalizeTypeCode` | `TransactionTypeRulesTest` BR-TT-003 |
| BR-TT-004 | `TransactionTypeRules#normalizeTypeCode` | `TransactionTypeRulesTest` BR-TT-004 |
| BR-TT-005 | `TransactionTypeRules#validateDescription` | `TransactionTypeRulesTest` BR-TT-005 |
| BR-TT-006 | `TransactionTypeRules#validateDescription` | `TransactionTypeRulesTest` BR-TT-006, `TransactionTypeControllerTest#descriptionChars` |
| BR-TT-007 | `TransactionTypeRules#validateDescription` | `TransactionTypeRulesTest` BR-TT-007 |
| BR-TT-008 | `TransactionTypeRules#isUnchanged`, `TransactionTypeService#update` | `TransactionTypeRulesTest` BR-TT-008, `TransactionTypeServiceTest` BR-TT-008, controller 422 |
| BR-TT-010 | `TransactionTypeService#list/forwardPage` | `TransactionTypeServiceTest` BR-TT-010, `TransactionTypeControllerTest#list`, `TransactionTypeDb2IT` |
| BR-TT-011 | `TransactionTypeRules#parseTypeCodeFilter` | `TransactionTypeRulesTest` BR-TT-011 |
| BR-TT-012 | `TransactionTypeRules#descriptionFilterPattern` | `TransactionTypeRulesTest` BR-TT-012 |
| BR-TT-013 | `TransactionTypeService#forwardPage`, `adapters/db2/JdbcTransactionTypeStore#findForward` | `TransactionTypeServiceTest` BR-TT-013, `TransactionTypeDb2IT` paging |
| BR-TT-014 | `TransactionTypeService#list` (PREVIOUS), `JdbcTransactionTypeStore#findBackward` | `TransactionTypeServiceTest` BR-TT-014, `TransactionTypeDb2IT` paging |
| BR-TT-015 | `TransactionTypeService#list` | `TransactionTypeServiceTest` BR-TT-015 |
| BR-TT-016 | `TransactionTypeService#forwardPage` | `TransactionTypeServiceTest` BR-TT-016 |
| BR-TT-017 | `TransactionTypeService#list` (FIRST ignores startKey), `JdbcTransactionTypeStore` WHERE | `TransactionTypeServiceTest` BR-TT-017 |
| BR-TT-018 | `api/TransactionTypeController` (one row action per HTTP call: PUT = U, DELETE = D) | `TransactionTypeControllerTest#br_tt_018_singleActionPerRequest` |
| BR-TT-019 | `TransactionTypeService#list` | `TransactionTypeServiceTest` BR-TT-019 |
| BR-TT-020 | `TransactionTypeService#get/update` | `TransactionTypeServiceTest` BR-TT-020, controller 404 |
| BR-TT-030 | `TransactionTypeService#create/insert` | `TransactionTypeServiceTest` BR-TT-030, controller 201/409, `TransactionTypeDb2IT` (-803) |
| BR-TT-040 | `TransactionTypeService#update` | `TransactionTypeServiceTest` BR-TT-040 |
| BR-TT-041 | `TransactionTypeService#update` | `TransactionTypeServiceTest` BR-TT-041 |
| BR-TT-042 | `TransactionTypeService#update(createIfMissing)` | `TransactionTypeServiceTest` BR-TT-042 |
| BR-TT-050 | `TransactionTypeService#delete` | `TransactionTypeServiceTest` BR-TT-050, controller 428 |
| BR-TT-051 | `TransactionTypeService#delete` | `TransactionTypeServiceTest` BR-TT-051, controller 409, `TransactionTypeDb2IT` (-532) |
| BR-TT-052 | `TransactionTypeService#delete` | `TransactionTypeServiceTest` BR-TT-052 |
| BR-TT-060 | `TransactionTypeService#dbError/sqlGuard` | `TransactionTypeServiceTest` BR-TT-060 |
| BR-TT-061 | `api/ApiExceptionHandler#unavailable` | `Db2UnavailableTest` |
| BR-TT-070..076 | `domain/batch/BatchMaintenanceService` | `BatchMaintenanceServiceTest` (one test per rule), `TransactionTypeDb2IT` batch |
| BR-TT-080 | `TransactionTypeService#extractTransactionTypes`, `JdbcTransactionTypeStore` | `TransactionTypeServiceTest` BR-TT-080/081, `TransactionTypeDb2IT` extract |
| BR-TT-081 | `TransactionTypeService#extractCategories`, `JdbcTransactionTypeStore` | same as BR-TT-080 |

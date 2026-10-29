# authfrds-service

Java 21 / Spring Boot 3 port of the CardDemo **fraud-authorization** Db2 behaviour
(`app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl`, table `CARDDEMO.AUTHFRDS` + index `XAUTHFRD`),
plus the PF5 / PF8 slice of `COPAUS1C.cbl` that calls it. Specs: `specs/aws-transform/authfrds/`.

**Scope / stubs (read this first):**
- **Db2 is real** (IBM Db2 Community in Docker, schema from the repo DDL).
- **IMS is stubbed** behind `domain/ims/PendingAuthorizationStore`; `adapters/ims/InMemoryPendingAuthorizationStore`
  is seeded from `ims-stub/pending-authorizations.json` (decoded from the IMS unload `DBPAUTP0.dat` by `tools/decode_ims_unload.py`).
  `POST /api/v1/ims-stub/fail-next-repl?status=XX` makes the next REPL fail, to exercise the rollback path.
- **CICS** LINK/SYNCPOINT become an in-process call + Spring `@Transactional`. **MQ** is not touched by COPAUS2C and is out of scope.

## Run
```
make db2-up db2-wait      # first start pulls ~1.5 GB
make test                 # unit + MockMvc
make it                   # + real-Db2 integration tests (Failsafe, *IT.java)
make run                  # http://localhost:8080/swagger-ui/index.html
make db2-sql Q="SELECT CARD_NUM, AUTH_TS, AUTH_FRAUD, FRAUD_RPT_DATE FROM CARDDEMO.AUTHFRDS"
```

## API
| Method | Path | Legacy |
|---|---|---|
| POST | `/api/v1/fraud-records` | COPAUS2C LINK (INSERT, -803 -> FRAUD-UPDATE) |
| GET | `/api/v1/fraud-records?cardNum&acctId&authFraud&page` (page size: `carddemo.list.page-size`, default 5, env `CARDDEMO_LIST_PAGE_SIZE`) | inspection read (BR-AF-030, no legacy cursor) |
| GET | `/api/v1/fraud-records/{cardNum}/{authTs}` | inspection read by PK |
| GET | `/api/v1/accounts`, `/api/v1/accounts/{acctId}/authorizations` | IMS stub browse |
| GET | `/api/v1/accounts/{acctId}/authorizations/{authKey}` | COPAUS1C ENTER |
| GET | `/api/v1/accounts/{acctId}/authorizations/{authKey}/next` | COPAUS1C PF8 |
| POST | `/api/v1/accounts/{acctId}/authorizations/{authKey}/fraud` | COPAUS1C PF5 -> COPAUS2C -> IMS REPL |
| POST | `/api/v1/ims-stub/fail-next-repl`, `/api/v1/ims-stub/reset` | test hooks for the IMS stub |

HTTP mapping: rule/validation failure 400, not found / end of data 404, COPAUS2C failure (`WS-FRD-UPDT-FAILED`) 422, IMS error 500, Db2 down 503.
No DELETE: no legacy program deletes AUTHFRDS rows (see GAP-REVIEW).

## Traceability (rule -> code -> test)
| Rule | Code (`src/main`) | Tests (`src/test`) |
|---|---|---|
| BR-AF-001 | AuthFraudRules, JdbcFraudRecordStore | AuthFraudRulesTest, AuthFrdsDb2IT |
| BR-AF-002 | AuthFraudRules | AuthFraudRulesTest, AuthFrdsDb2IT |
| BR-AF-003 | FraudRecordService, JdbcFraudRecordStore | AuthFrdsDb2IT, FraudRecordServiceTest |
| BR-AF-004 | FraudRecordService | AuthFrdsDb2IT, FraudRecordControllerTest, FraudRecordServiceTest |
| BR-AF-005 | FraudRecordService, JdbcFraudRecordStore | AuthFrdsDb2IT, FraudRecordServiceTest |
| BR-AF-006 | FraudRecordService | AuthFrdsDb2IT, FraudRecordControllerTest, FraudRecordServiceTest |
| BR-AF-007 | AuthFraudRules, FraudRecordController, FraudRecordService, JdbcFraudRecordStore | AuthFraudRulesTest, AuthFrdsDb2IT, FraudRecordControllerTest, FraudRecordServiceTest |
| BR-AF-008 | AuthFraudRules, FraudRecordService | AuthFraudRulesTest, AuthFrdsDb2IT, FraudRecordServiceTest |
| BR-AF-009 | AuthFraudRules | AuthFraudRulesTest |
| BR-AF-010 | AuthFraudRules, FraudRecordService | AuthFraudRulesTest, FraudRecordServiceTest |
| BR-AF-011 | ApiExceptionHandler, AuthorizationDetail | FraudRecordControllerTest |
| BR-AF-020 | AuthFraudRules, AuthorizationFraudService | AuthFraudRulesTest, AuthFrdsDb2IT, AuthorizationFraudServiceTest |
| BR-AF-021 | AuthorizationFraudService | AuthorizationFraudServiceTest |
| BR-AF-022 | AuthFraudRules, AuthorizationFraudService | AuthFraudRulesTest, AuthFrdsDb2IT, AuthorizationFraudServiceTest, FraudRecordControllerTest |
| BR-AF-023 | AuthorizationFraudService | AuthFrdsDb2IT, AuthorizationFraudServiceTest |
| BR-AF-024 | AuthFraudRules, AuthorizationFraudService | AuthFraudRulesTest, AuthFrdsDb2IT, AuthorizationFraudServiceTest, FraudRecordControllerTest |
| BR-AF-025 | AuthorizationFraudService | AuthorizationFraudServiceTest |
| BR-AF-026 | AuthKey | AuthorizationFraudServiceTest |
| BR-AF-027 | AuthorizationFraudService | AuthorizationFraudServiceTest, FraudRecordControllerTest |
| BR-AF-030 | FraudRecordController, JdbcFraudRecordStore | AuthFrdsDb2IT, FraudRecordControllerTest |

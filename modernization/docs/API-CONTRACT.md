# CardDemo API contract

REST replacement for the BMS 3270 screens. Each service also publishes a live OpenAPI document at
`/v3/api-docs` and Swagger UI at `/swagger-ui.html`. The rules behind this contract (naming, errors,
paging, authentication, async jobs) are in `docs/migration/api-style.md`.

**Authentication:** every `/api/**` endpoint except sign-on needs `Authorization: Bearer <token>`
from `POST /api/v1/auth/signon` (claims `sub` = user id, `userType` = `A`/`U`). Missing or invalid
token → 401, a `U` token on admin-only endpoints (`/api/v1/users/**`) → 403, both as the error
envelope below. `/actuator/health`, `/v3/api-docs` and Swagger UI stay open. Design:
`docs/migration/architecture.md` §3.1.

Media type is `application/json`. Errors use the shared envelope:

```json
{
  "timestamp": "2024-05-01T10:00:00Z",
  "status": 404,
  "error": "Not Found",
  "message": "Account 11111111111 not found",
  "fieldErrors": [{"field": "cardNumber", "message": "card number must be 16 digits"}]
}
```

| CICS RESP | HTTP |
| --- | --- |
| NORMAL | 200 / 201 |
| NOTFND | 404 |
| DUPREC | 409 |
| INVREQ / map validation | 400 |
| business rejection (e.g. overlimit) | 422 |

Browse operations (`STARTBR` / `READNEXT` / PF7 / PF8) become page based collections:

```json
{"content": [], "page": 0, "size": 20, "totalElements": 0, "totalPages": 0}
```

## auth-service (port 8080) — CSUSR01Y, COSGN00C, COUSR00C-03C

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| POST | `/api/v1/auth/signon` | COSGN00C | public; returns `token`, `tokenType`, `expiresAt` and `nextScreen` = `ADMIN` or `MAIN` (COADM01C vs COMEN01C routing); 422 `Wrong Password. Try again ...` / `User not found. Try again ...` |
| GET | `/api/v1/menu` | COMEN01C / COADM01C | menu for the caller's user type, see "Menus" |
| GET | `/api/v1/menu/options/{number}` | COMEN01C / COADM01C PROCESS-ENTER-KEY | the selected option; 400 `Please enter a valid option number...`, 403 admin-only, 422 not installed |
| GET | `/api/v1/users` | COUSR00C | paged user list; all `/api/v1/users/**` operations are admin-only (403 for `U`) |
| GET | `/api/v1/users/{userId}` | COUSR03C | |
| POST | `/api/v1/users` | COUSR01C | password is hashed with BCrypt, never stored or returned in clear |
| PUT | `/api/v1/users/{userId}` | COUSR02C | |
| DELETE | `/api/v1/users/{userId}` | COUSR03C | |

## customer-service (port 8081) — CVCUS01Y

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/customers` | CUSTDAT browse | |
| GET | `/api/v1/customers/{customerId}` | CUSTDAT read | SSN is returned as last four digits only |
| PUT | `/api/v1/customers/{customerId}` | CUSTDAT rewrite | |

## account-service (port 8082) — CVACT01Y, COACTVWC, COACTUPC, account side of CBTRN02C/CBACT04C

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/accounts/{accountId}` | COACTVWC | |
| GET | `/api/v1/accounts` | ACCTDAT browse | |
| PUT | `/api/v1/accounts/{accountId}` | COACTUPC | optimistic locking replaces the CICS READ for UPDATE lock |
| POST | `/api/v1/accounts/{accountId}/postings` | CBTRN02C 1500-VALIDATE-TRAN + 2700-UPDATE-ACCOUNT-REC | 200 when posted, 422 with `reasonCode` 101/102/103 when rejected |
| POST | `/api/v1/accounts/{accountId}/interest-settlements` | CBACT04C account break | adds the accumulated interest and resets the cycle totals |

## card-service (port 8083) — CVACT02Y, CVACT03Y, COCRDLIC, COCRDSLC, COCRDUPC

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/cards` | COCRDLIC | optional `accountId` filter, replacing the AIX browse on CARDDAT |
| GET | `/api/v1/cards/{cardNumber}` | COCRDSLC | CVV is never exposed |
| PUT | `/api/v1/cards/{cardNumber}` | COCRDUPC | |
| GET | `/api/v1/cards/{cardNumber}/xref` | CARDXREF read | |
| GET | `/api/v1/card-xrefs/by-account/{accountId}` | CXACAIX alternate index | |

## transaction-service (port 8084) — CVTRA01Y-06Y, COTRN00C/01C/02C, COBIL00C, CBTRN02C, CBACT04C

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/transactions` | COTRN00C | optional `cardNumber` filter |
| GET | `/api/v1/transactions/{transactionId}` | COTRN01C | |
| POST | `/api/v1/transactions` | COTRN02C | card must resolve through CARDXREF |
| POST | `/api/v1/bill-payments` | COBIL00C | pays the full current balance; the amount comes from account-service, not the caller |
| POST | `/api/v1/batch/post-transactions` | JCL POSTTRAN / CBTRN02C | launches the Spring Batch job, returns posted and rejected counts |
| POST | `/api/v1/batch/interest-calculation` | JCL INTCALC / CBACT04C | returns accounts settled and total interest |
| GET | `/api/v1/transaction-types` | TRANTYPE (CVTRA03Y) | paged, by type code; read by batch-service TRANREPT |
| GET | `/api/v1/transaction-categories` | TRANCATG (CVTRA04Y) | paged, by type and category code; read by batch-service TRANREPT |

## batch-service (port 8085) — CBTRN03C

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| POST | `/api/v1/batch/transaction-report?startDate=&endDate=` | `EXEC PROC=TRANREPT` (CORPT00C) / CBTRN03C | runs TRANREPT synchronously with the dates as DATEPARM; data from transaction-service and card-service; returns the run summary with `outputDir` (`TRANREPT` = the report) |

## Menus — COMEN01C (CM00, `COMEN02Y`) and COADM01C (CA00, `COADM02Y`)

`GET /api/v1/menu` in auth-service returns the menu for the caller's user type: `menu` (`MAIN`/`ADMIN`),
`transactionId` (`CM00`/`CA00`), `program`, `title` and `options[]` with `number`, `name`, `program`,
`adminOnly`, `available`, `endpoint`. Unavailable options (`available: false`, `endpoint: null`):
9 Transaction Reports (CORPT00C endpoint not built yet), 11 Pending Authorization View, admin 5–6.

Main menu (user type `U`; every option is flagged `U` in `COMEN02Y`, none is admin-only):

| # | Option | Program | Endpoint |
| --- | --- | --- | --- |
| 1 | Account View | COACTVWC | `GET /api/v1/accounts/{accountId}` |
| 2 | Account Update | COACTUPC | `PUT /api/v1/accounts/{accountId}` |
| 3 | Credit Card List | COCRDLIC | `GET /api/v1/cards` |
| 4 | Credit Card View | COCRDSLC | `GET /api/v1/cards/{cardNumber}` |
| 5 | Credit Card Update | COCRDUPC | `PUT /api/v1/cards/{cardNumber}` |
| 6 | Transaction List | COTRN00C | `GET /api/v1/transactions` |
| 7 | Transaction View | COTRN01C | `GET /api/v1/transactions/{transactionId}` |
| 8 | Transaction Add | COTRN02C | `POST /api/v1/transactions` |
| 9 | Transaction Reports | CORPT00C | `POST /api/v1/reports/transactions` (planned) |
| 10 | Bill Payment | COBIL00C | `POST /api/v1/bill-payments` |
| 11 | Pending Authorization View | COPAUS0C | not available (IMS/MQ extension, excluded by d-scope) |

Admin menu (user type `A`):

| # | Option | Program | Endpoint |
| --- | --- | --- | --- |
| 1 | User List (Security) | COUSR00C | `GET /api/v1/users` |
| 2 | User Add (Security) | COUSR01C | `POST /api/v1/users` |
| 3 | User Update (Security) | COUSR02C | `PUT /api/v1/users/{userId}` |
| 4 | User Delete (Security) | COUSR03C | `DELETE /api/v1/users/{userId}` |
| 5 | Transaction Type List/Update (Db2) | COTRTLIC | not available (DB2 extension, excluded by d-scope) |
| 6 | Transaction Type Maintenance (Db2) | COTRTUPC | not available (DB2 extension, excluded by d-scope) |

## Reports — CORPT00C (CR00) → TRANREPT / CBTRN03C

Planned (UNT149-20), asynchronous job with polling:

| Method | Path | Notes |
| --- | --- | --- |
| POST | `/api/v1/reports/transactions` | body `{"reportType": "MONTHLY"\|"YEARLY"\|"CUSTOM", "startDate", "endDate"}`; dates only for `CUSTOM`; 202 + `{"reportId", "status", "startDate", "endDate"}` |
| GET | `/api/v1/reports/transactions/{reportId}` | `SUBMITTED` / `RUNNING` / `COMPLETED` / `FAILED` |
| GET | `/api/v1/reports/transactions/{reportId}/content` | `text/plain` report as CBTRN03C writes it |

## Statements — CREASTMT / CBSTM03A / CBSTM03B

Batch only (no CICS screen). Planned: `POST /api/v1/batch/statements` runs statement generation, one
statement per card cross-reference record (as CBSTM03A), text and HTML output. Renamed from the earlier `POST /api/v1/statements` to follow the
`/batch` convention.

## Not yet implemented

| Legacy | Planned endpoint | Status |
| --- | --- | --- |
| COSGN00C token, all `CDEMO-USRTYP-ADMIN` checks | bearer token + `common` auth filter | not built (UNT149-16) |
| COMEN01C / COADM01C menus | `GET /api/v1/menu` | only `nextScreen` today (UNT149-16) |
| CORPT00C | `POST /api/v1/reports/transactions` | not built (UNT149-20); the CBTRN03C job it submits is built (batch-service `TRANREPT`) |
| CBSTM03A / CBSTM03B | `POST /api/v1/batch/statements` | not built |
| CDRD / CDRA MQ pairs | SQS request queue + SNS event topic | designed in COBOL-PROGRAM-MAPPING.md, not built |

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
| GET | `/api/v1/users?page=&size=&startUserId=` | COUSR00C | 10 users a page (default `size`) in user-id order, from `startUserId` if given; all `/api/v1/users/**` operations are admin-only (403 for `U`) |
| GET | `/api/v1/users/{userId}` | COUSR03C | |
| POST | `/api/v1/users` | COUSR01C | `firstName`, `lastName`, `userId`, `password`, `userType` all required, checked in that order with the COBOL texts (400, e.g. `First Name can NOT be empty...`); type `A`/`U`; duplicate → 409 `User ID already exist...`; password is hashed with BCrypt, never stored or returned in clear |
| PUT | `/api/v1/users/{userId}` | COUSR02C | same fields (`userId` in the body optional, must match the path); unknown → 404 `User ID NOT found...`; no change → 422 `Please modify to update ...` |
| DELETE | `/api/v1/users/{userId}` | COUSR03C | unknown → 404 `User ID NOT found...` |

## customer-service (port 8081) — CVCUS01Y

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/customers` | CUSTDAT browse | |
| GET | `/api/v1/customers/{customerId}` | CUSTDAT read | SSN is returned as last four digits only |
| PUT | `/api/v1/customers/{customerId}` | CUSTDAT rewrite | optional `ssn` (null keeps it) and `version` (409 when stale); called by the COACTUPC update |

## account-service (port 8082) — CVACT01Y, COACTVWC, COACTUPC, account side of CBTRN02C/CBACT04C

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/accounts/{accountId}` | ACCTDAT read | the account record alone (transaction-service, batch) |
| GET | `/api/v1/accounts/{accountId}/view` | COACTVWC | account id → card-xref (card-service) → account → customer (customer-service), one DTO with the COACTVW fields and both record versions; SSN masked |
| GET | `/api/v1/accounts` | ACCTDAT browse | |
| PUT | `/api/v1/accounts/{accountId}` | COACTUPC | account and customer fields of the COACTUP map plus `accountVersion`/`customerVersion` from the view; COBOL edit messages (400), `No change detected with respect to values fetched.` (422), stale version 409; see `docs/migration/account-view-update.md` |
| POST | `/api/v1/accounts/{accountId}/postings` | CBTRN02C 1500-VALIDATE-TRAN + 2700-UPDATE-ACCOUNT-REC | body `transactionId` (idempotency key, max 16), `amount`, `transactionDate`, optional `type` (`DAILY_TRANSACTION` default, or `BILL_PAYMENT` = COBIL00C: balance only, no 102/103); 200 when posted (a repeat of the same `transactionId` returns the first result without posting again), 422 with `reasonCode` 101/102/103 when rejected, 409 when the `transactionId` was already posted with another account, amount, date or type |
| POST | `/api/v1/accounts/{accountId}/interest-settlements` | CBACT04C account break | adds the accumulated interest and resets the cycle totals |

## card-service (port 8083) — CVACT02Y, CVACT03Y, COCRDLIC, COCRDSLC, COCRDUPC

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/cards` | COCRDLIC | optional `accountId` (11 digits) and `cardNumber` (16 digits) filters, intersecting; ordered by card number |
| GET | `/api/v1/cards/{cardNumber}` | COCRDSLC | CVV is never exposed; `version` is returned for the update |
| PUT | `/api/v1/cards/{cardNumber}` | COCRDUPC | `embossedName`, `activeStatus`, `expirationMonth`, `expirationYear`, `version`; 422 when nothing changed, 409 when the version is stale |
| GET | `/api/v1/cards/{cardNumber}/xref` | CARDXREF read | |
| GET | `/api/v1/card-xrefs/by-account/{accountId}` | CXACAIX alternate index | |

## transaction-service (port 8084) — CVTRA01Y-06Y, COTRN00C/01C/02C, COBIL00C, CBTRN02C, CBACT04C

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| GET | `/api/v1/transactions` | COTRN00C | TRAN-ID order; optional `startTransactionId` (TRNIDIN start key) and `cardNumber` filter |
| GET | `/api/v1/transactions/{transactionId}` | COTRN01C | |
| POST | `/api/v1/transactions` | COTRN02C | `accountId` or `cardNumber`, `typeCode`, `categoryCode`, `source`, `description`, `amount`, `originDate`, `processDate` (`YYYY-MM-DD`), `merchantId`, `merchantName`, `merchantCity`, `merchantZip`, all required, COBOL messages; numeric TRAN-ID = last + 1 |
| POST | `/api/v1/bill-payments` | COBIL00C | `{"accountId"}`; pays the full current balance; the amount comes from account-service, not the caller; posts with `type: BILL_PAYMENT` keyed by the new TRAN-ID |
| POST | `/api/v1/batch/post-transactions` | JCL POSTTRAN / CBTRN02C | launches the Spring Batch job, returns posted and rejected counts |
| POST | `/api/v1/batch/interest-calculation` | JCL INTCALC / CBACT04C | returns accounts settled and total interest |
| GET | `/api/v1/transaction-types` | TRANTYPE (CVTRA03Y) | paged, by type code; read by batch-service TRANREPT |
| GET | `/api/v1/transaction-categories` | TRANCATG (CVTRA04Y) | paged, by type and category code; read by batch-service TRANREPT |

## batch-service (port 8085) — CBTRN03C

| Method | Path | Legacy | Notes |
| --- | --- | --- | --- |
| POST | `/api/v1/batch/transaction-report?startDate=&endDate=` | `EXEC PROC=TRANREPT` (CORPT00C) / CBTRN03C | runs TRANREPT synchronously with the dates as DATEPARM; data from transaction-service and card-service, called with the caller's bearer token; returns the run summary with `outputDir` (`TRANREPT` = the report) |

| POST | `/api/v1/reports/transactions` | CORPT00C | async TRANREPT submission, see Reports below |
| GET | `/api/v1/reports/transactions/{reportId}` and `/content` | CORPT00C | poll status, read the report |

## Menus — COMEN01C (CM00, `COMEN02Y`) and COADM01C (CA00, `COADM02Y`)

`GET /api/v1/menu` in auth-service returns the menu for the caller's user type: `menu` (`MAIN`/`ADMIN`),
`transactionId` (`CM00`/`CA00`), `program`, `title` and `options[]` with `number`, `name`, `program`,
`adminOnly`, `available`, `endpoint`. Unavailable options (`available: false`, `endpoint: null`):
11 Pending Authorization View, admin 5–6.

Main menu (user type `U`; every option is flagged `U` in `COMEN02Y`, none is admin-only):

| # | Option | Program | Endpoint |
| --- | --- | --- | --- |
| 1 | Account View | COACTVWC | `GET /api/v1/accounts/{accountId}/view` |
| 2 | Account Update | COACTUPC | `PUT /api/v1/accounts/{accountId}` |
| 3 | Credit Card List | COCRDLIC | `GET /api/v1/cards` |
| 4 | Credit Card View | COCRDSLC | `GET /api/v1/cards/{cardNumber}` |
| 5 | Credit Card Update | COCRDUPC | `PUT /api/v1/cards/{cardNumber}` |
| 6 | Transaction List | COTRN00C | `GET /api/v1/transactions` |
| 7 | Transaction View | COTRN01C | `GET /api/v1/transactions/{transactionId}` |
| 8 | Transaction Add | COTRN02C | `POST /api/v1/transactions` |
| 9 | Transaction Reports | CORPT00C | `POST /api/v1/reports/transactions` (batch-service, port 8085) |
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

batch-service (port 8085), asynchronous job with polling (docs/migration/architecture.md §3.2):

| Method | Path | Notes |
| --- | --- | --- |
| POST | `/api/v1/reports/transactions` | body `{"reportType": "MONTHLY"\|"YEARLY"\|"CUSTOM", "startMonth", "startDay", "startYear", "endMonth", "endDay", "endYear"}`; date parts only for `CUSTOM`, validated as CORPT00C (400 with its message); 202 + `Location` + `{"reportId", "status": "SUBMITTED", "startDate", "endDate", ...}` |
| GET | `/api/v1/reports/transactions/{reportId}` | `SUBMITTED` / `RUNNING` / `COMPLETED` / `FAILED`, `returnCode`, `reportPath` (the `TRANREPT` file); submitter or admin |
| GET | `/api/v1/reports/transactions/{reportId}/content` | `text/plain` report as CBTRN03C writes it; 404 until `COMPLETED` |

## Statements — CREASTMT / CBSTM03A / CBSTM03B

Batch only (no CICS screen). Planned: `POST /api/v1/batch/statements` runs statement generation, one
statement per card cross-reference record (as CBSTM03A), text and HTML output. Renamed from the earlier `POST /api/v1/statements` to follow the
`/batch` convention.

## Not yet implemented

| Legacy | Planned endpoint | Status |
| --- | --- | --- |
| COSGN00C token, all `CDEMO-USRTYP-ADMIN` checks | bearer token + `common` auth filter | not built (UNT149-16) |
| COMEN01C / COADM01C menus | `GET /api/v1/menu` | only `nextScreen` today (UNT149-16) |
| CBSTM03A / CBSTM03B | `POST /api/v1/batch/statements` | not built |
| CDRD / CDRA MQ pairs | SQS request queue + SNS event topic | designed in COBOL-PROGRAM-MAPPING.md, not built |

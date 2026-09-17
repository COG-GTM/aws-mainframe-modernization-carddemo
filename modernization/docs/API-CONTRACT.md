# CardDemo API contract

REST replacement for the BMS 3270 screens. Each service also publishes a live OpenAPI document at
`/v3/api-docs` and Swagger UI at `/swagger-ui.html`.

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
| POST | `/api/v1/auth/signon` | COSGN00C | returns `nextScreen` = `ADMIN` or `MAIN`, mirroring the admin/user routing of COMEN01C vs COADM01C |
| GET | `/api/v1/users` | COUSR00C | paged user list |
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

## Not yet implemented

| Legacy | Planned endpoint | Status |
| --- | --- | --- |
| CORPT00C, CBTRN03C | `POST /api/v1/reports` | stub |
| CBSTM03A / CBSTM03B | `POST /api/v1/statements` | stub |
| CDRD / CDRA MQ pairs | SQS request queue + SNS event topic | designed in COBOL-PROGRAM-MAPPING.md, not built |
| COMEN01C / COADM01C menus | SPA routes | replaced by the route table above; no SPA in this change |

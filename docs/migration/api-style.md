# CardDemo migration: API style (what replaces the BMS screens)

Decision **d-ui: REST API only.** The 17 BMS maps are replaced by JSON endpoints documented with
OpenAPI (`/v3/api-docs`, Swagger UI at `/swagger-ui.html` on every service) and exercised by
integration tests. A web UI that mirrors the screens is a later, optional phase; it would add demo
value but no business-logic coverage, and it can be built on this API without changing it.

The endpoint list is in [`modernization/docs/API-CONTRACT.md`](../../modernization/docs/API-CONTRACT.md).
This document is the set of rules those endpoints follow, so later tickets add endpoints the same
way. "Current" means on `migration/carddemo-java` today; "planned" means a later ticket builds it.

## 1. Screen to endpoint

| 3270 / CICS concept | REST replacement |
|---|---|
| One transaction id / program (`CAUP` → `COACTUPC`) | One endpoint, or a small set on one resource (`PUT /api/v1/accounts/{accountId}`) |
| ENTER on an inquiry screen | `GET` |
| PF5 (save) / ENTER after confirmation on an update or add screen | `PUT` (update) / `POST` (add); the request itself is the confirmation, there is no separate "confirm" field |
| PF3 (exit), PF4 / PF12 (clear, cancel) | Nothing; client navigation, no server call |
| PF7 / PF8 (scroll back / forward) | `page` query parameter, see §4 |
| BMS input fields | Request DTO fields; BMS length / `NUM` / required checks become Bean Validation on the DTO |
| BMS output fields | Response DTO fields |
| `WS-MESSAGE` / `ERRMSG` line, cursor on the bad field (`MOVE -1 TO xxxL`) | `ApiError.message`, `ApiError.fieldErrors[].field`, see §3 |
| COMMAREA (`CARDDEMO-COMMAREA`, `COCOM01Y`) | Split: user id and type → session token (§5); `CDEMO-ACCT-ID`, `CDEMO-CARD-NUM`, selected row → path or query parameters; `CDEMO-FROM-PROGRAM` / `CDEMO-TO-PROGRAM` → dropped (client navigation) |
| Menu option number + XCTL | `GET /api/v1/menu`, see §6 |
| JCL written to TDQ `JOBS` (CORPT00C) | Asynchronous job resource, `202 Accepted` + poll, see §7 |

## 2. Resource naming

- Base path `/api/v1/`, then a plural kebab-case noun: `accounts`, `cards`, `card-xrefs`,
  `transactions`, `bill-payments`, `users`, `customers`.
- The id in the path is the COBOL record key, unchanged in meaning and format: `accountId` =
  `ACCT-ID` (11 digits), `cardNumber` = `CARD-NUM` (16 digits), `customerId` = `CUST-ID`, `userId` =
  `SEC-USR-ID` (8 characters), `transactionId` = `TRAN-ID` (16 characters).
- Alternate-index reads (VSAM AIX) are either a filter on the collection
  (`GET /api/v1/cards?accountId=`, `GET /api/v1/transactions?cardNumber=`) or a named lookup
  (`GET /api/v1/card-xrefs/by-account/{accountId}`).
- Business actions that are not plain CRUD are a noun sub-resource created with `POST`:
  `/accounts/{id}/postings`, `/accounts/{id}/interest-settlements`, `/bill-payments`.
- Batch jobs (the JCL replacement) live under `/api/v1/batch/<job-name>` and are started with
  `POST`. A job that an online screen triggers for a user (CORPT00C) is a resource of its own
  (`/api/v1/reports/transactions`), see §7.
- JSON field names are camelCase English names, not the COBOL names (`acctCurrBal` → `currentBalance`
  is fine); the mapping to copybook fields belongs in the DTO Javadoc or `COBOL-PROGRAM-MAPPING.md`.
- Money is a JSON number with two decimals mapped to `BigDecimal`; dates are ISO-8601
  (`2030-01-31`); timestamps are UTC instants.

## 3. Errors: `ApiError` replaces `WS-MESSAGE`

Every COBOL screen has one message line (`ERRMSG`, filled from `WS-MESSAGE`) and puts the cursor on
the offending field. Every service returns errors as `com.carddemo.common.api.ApiError`, rendered by
`common/error/RestExceptionHandler`:

```json
{
  "timestamp": "2026-11-02T21:21:25.308Z",
  "status": 400,
  "error": "Bad Request",
  "message": "Input validation failed",
  "fieldErrors": [{"field": "userId", "message": "must not be blank"}]
}
```

| COBOL outcome | Java | HTTP |
|---|---|---|
| `RESP NORMAL` | return value | 200, 201 on create, 202 for an accepted job |
| `RESP NOTFND` ("... NOT found...") | `NotFoundException` | 404 |
| `RESP DUPKEY` / `DUPREC` ("... already exist...") | `DuplicateKeyException` | 409 |
| Field edit failed (empty, not numeric, bad date), cursor on field | Bean Validation or `fieldErrors` | 400 |
| Business rejection after the edits pass (wrong password, over limit, "No access - Admin Only option...") | `BusinessRuleException` | 422 (`ForbiddenException` 403 for the admin-only case) |
| Other `RESP` / abend ("Unable to ...") | unhandled exception | 500 |
| Optimistic lock lost (CICS READ UPDATE then record changed) | `@Version` conflict | 409 (target) |

Rules:

- `message` carries the COBOL message text where a flow test asserts on it (UNT149-16 requires this
  for sign-on, e.g. `Wrong Password. Try again ...`). Where the COBOL edits one field at a time and
  stops, the API may report all failing fields at once in `fieldErrors`; the first entry is the field
  the COBOL would have put the cursor on.
- Informational messages on success (COACTUPC's `Changes committed to database`) are not
  part of the response; the 2xx status says it.
- **Current gap:** `AuthService.signon` returns one message, `Invalid user id or password` (422), for
  both COBOL cases `User not found. Try again ...` and `Wrong Password. Try again ...`.
  architecture.md §4 records this as a deliberate deviation; UNT149-16 now requires the COBOL text, so
  it is treated as a gap here and UNT149-16 restores it.
- `RestExceptionHandler` maps optimistic-lock failures to 409, `ForbiddenException` to 403, malformed
  bodies/parameters to 400, Spring MVC errors to their own status, and anything unexpected to 500
  `Unexpected error. Try again later ...` (logged, no internals in the body), all as `ApiError`.

## 4. Paging: `PageResponse` replaces PF7 / PF8

The list screens browse a VSAM file with `STARTBR` / `READNEXT` / `READPREV` and show a fixed number
of rows: COCRDLIC 7 cards, COTRN00C and COUSR00C 10 rows. PF8 reads forward from the last key on the
screen, PF7 backward from the first.

Collections return `com.carddemo.common.api.PageResponse`:

```json
{"content": [ ... ], "page": 0, "size": 20, "totalElements": 4, "totalPages": 1}
```

- Request: `?page=<0-based>&size=<n>`; default `size` 20, maximum 200 (`MAX_PAGE_SIZE` in each
  controller). PF8 is `page+1`, PF7 is `page-1`.
- Order is ascending by the record key the COBOL browses (`cardNum`, `tranId`, `userId`, `acctId`,
  `custId`), so page *n* at the screen's size returns the rows the n-th PF8 shows.
- Parity tests pass the screen's size (`size=7` for cards, `size=10` for transactions and users).
- COTRN00C / COUSR00C's `You are already at the top of the page...` / `...at the bottom of the
  page...` have no error equivalent: a page before 0 is a 400 with `fieldErrors[0].field = page`, a
  page past the end returns empty `content`, and the client uses `totalPages` to disable the keys.
- Filters and start keys: COCRDLIC has two optional filters, account and card number. The API has the
  `accountId` query filter today; a `cardNumber` filter on `GET /api/v1/cards` is missing (gap for the
  card-service review ticket). COTRN00C starts the browse at a typed transaction id
  (`TRNIDIN` → `STARTBR` on `TRAN-ID`); `GET /api/v1/transactions?startTransactionId=` does the same
  (1–16 digits, zero padded; `Tran ID must be Numeric ...`).
  **Hypothesis:** offset paging gives the same rows as a key browse because the data does not change
  between screens in the parity scenarios; if concurrent inserts matter, switch to a key cursor
  (`?after=<key>`) without changing `PageResponse`.

## 5. Session and authentication

**COBOL.** COSGN00C reads `USRSEC` by user id, compares the password, and copies `SEC-USR-ID` and
`SEC-USR-TYPE` into the COMMAREA (`CDEMO-USER-ID`, `CDEMO-USER-TYPE`). Every later program trusts the
COMMAREA. Two checks use the type: COSGN00C routes `CDEMO-USRTYP-ADMIN` ('A') to COADM01C and
everyone else to COMEN01C; COMEN01C refuses a main-menu option flagged `'A'` to a `'U'` user with
`No access - Admin Only option...`. COCRDLIC's header comment says an admin sees all cards and a user
only the COMMAREA account's cards, but the code does not branch on user type: the account filter is
optional for everyone (`2210-EDIT-ACCOUNT`) and the program sets `CDEMO-USRTYP-USER` itself. The security record (`CSUSR01Y`) has no account or customer id, so the
COBOL has **no per-user data ownership**: a regular user can view any account by typing its number.

**Implemented (UNT149-16).** Mechanism, key, lifetime and gateway details are in
`architecture.md` §3.1.

- Sign-on returns a signed bearer token (`Authorization: Bearer <token>`) whose claims are the two
  COMMAREA fields: `sub` = user id, `userType` = `A` / `U`. The response keeps `userType` and
  `nextScreen` so a client can still route like COSGN00C.
- A filter in `common` validates the token on every service. Missing or invalid token → 401
  `ApiError`. The transaction-service gateways forward the caller's token to account- and
  card-service.
- `CDEMO-USRTYP-ADMIN` checks become a role check: `userType = A` → role `ADMIN`. Admin-only
  endpoints answer 403 `ApiError` to a `U` token. Admin-only today in COBOL: the admin menu and
  everything behind it (`/api/v1/users/**`, the user CRUD of COUSR00C–03C).
- No ownership checks or role-dependent filters are added on accounts, cards or transactions,
  because the COBOL code has none (`accountId` stays optional on `GET /api/v1/cards` for both types).
  If the COCRDLIC header comment's admin/user rule is wanted anyway, it is new behavior and needs a
  decision, not a parity fix.
- Sign-off (PF3 on the menu, `CICS RETURN` without TRANSID) needs no endpoint; the client drops the
  token. Lifetime: `CARDDEMO_TOKEN_TTL`, default 8 hours.

## 6. Menus (COMEN01C / COADM01C)

The menus contain no business logic beyond the option table and the admin check, so they become one
read endpoint the client renders: `GET /api/v1/menu`, answered by auth-service from the caller's
token, plus `GET /api/v1/menu/options/{number}` for the COBOL selection checks (range, admin-only,
not installed).

```json
{"menu": "MAIN", "options": [
  {"number": 1, "name": "Account View", "program": "COACTVWC", "endpoint": "GET /api/v1/accounts/{accountId}", "adminOnly": false, "available": true}
]}
```

`menu` is `ADMIN` for an `A` token (COADM02Y options) and `MAIN` for a `U` token (COMEN02Y options),
matching COSGN00C routing. Options are listed in COBOL order with their COBOL numbers. The full
option-to-endpoint table is in `API-CONTRACT.md` ("Menus"). Facts from the copybooks:

- `COMEN02Y` has 11 options and every one is flagged `'U'` (`CDEMO-MENU-OPT-USRTYPE`), so the
  COMEN01C admin-only branch is unreachable with the shipped table. `adminOnly` stays in the response
  because the check exists in the code.
- Option 11 (`COPAUS0C`) and admin options 5–6 (`COTRTLIC`, `COTRTUPC`) belong to the IMS/DB2/MQ
  extensions excluded by d-scope (scope.md); they are returned with `available: false`, the
  equivalent of COMEN01C's `... is not installed...` message (it checks with `EXEC CICS INQUIRE`).

## 7. Report submission (CORPT00C, CR00)

**COBOL.** The screen offers Monthly, Yearly or Custom. Monthly = first to last day of the current
month; Yearly = 1 January to 31 December of the current year; Custom = start and end dates typed as
MM / DD / YYYY, each part checked for empty and numeric, then the whole date checked by `CSUTLDTC`.
A `Y` / `N` confirmation follows. On `Y` the program fills the dates into an inline TRANREPT JCL
stream and writes it line by line to TDQ `JOBS` (the internal reader); the screen only says the job
was submitted. The batch side (TRANREPT → `CBTRN03C`) filters `TRAN-PROC-TS` (first 10 characters)
between start and end date inclusive and writes the report file `TRANREPT`.

**Planned (UNT149-20, decision d-report-trigger: in-process async job with polling).**

| Endpoint | Meaning |
|---|---|
| `POST /api/v1/reports/transactions` body `{"reportType": "MONTHLY" \| "YEARLY" \| "CUSTOM", "startDate", "endDate"}` | Validates like CORPT00C (dates only for `CUSTOM`; server computes them for the other two), starts the job, returns `202` with `{"reportId", "status": "SUBMITTED", "startDate", "endDate"}` and a `Location` header |
| `GET /api/v1/reports/transactions/{reportId}` | Status `SUBMITTED` / `RUNNING` / `COMPLETED` / `FAILED` and the date range |
| `GET /api/v1/reports/transactions/{reportId}/content` | The produced report, `text/plain`, same layout as `CBTRN03C` writes |

The confirmation flag is not part of the request (§1). Date errors use the CORPT00C message text, for
example `Start Date - Not a valid date...`, with `fieldErrors[].field` = `startDate` / `endDate`.
The ticket text places this in a "batch-service"; this branch has no batch-service and runs Spring
Batch jobs in transaction-service (architecture.md §2). **Hypothesis:** the endpoint lives in
transaction-service unless phase 4 introduces a separate batch service; the path does not change
either way.

## 8. Statements (CBSTM03A / CBSTM03B)

There is no statement screen. `CREASTMT.JCL` sorts the transactions, then `CBSTM03A` walks the
card cross-reference file and writes a statement for each record (card → account → customer), in plain text
(`STATEMNT.PS`) and HTML (`STATEMNT.HTML`), using `CBSTM03B` for file I/O. So the replacement is a
batch job, not an online resource:

| Endpoint | Meaning |
|---|---|
| `POST /api/v1/batch/statements` | Runs the CREASTMT equivalent: one statement per card cross-reference record, so an account with two cards gets two statements, as in the COBOL; same response shape as the other `/batch` jobs |
| **Hypothesis**, optional: `GET /api/v1/accounts/{accountId}/statements/latest` (`text/plain` or `text/html`) | Read one produced statement. The COBOL has no reader, so this is new behavior and not part of parity |

`API-CONTRACT.md` previously listed `POST /api/v1/statements`; it is renamed to follow the batch
convention.

## 9. What is verified here

This ticket changes documentation only. The current behavior quoted above (envelope, paging, sign-on
without token, unauthenticated `GET /users`) was observed against `docker compose up` on this branch;
the commands and output are in the PR description. Everything marked *planned* is design for later
tickets and is not built.

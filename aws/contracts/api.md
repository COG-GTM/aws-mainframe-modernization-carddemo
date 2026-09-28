# Contract: REST API (CICS/BMS → Spring Boot + React)

Status: **v1 (Discovery session)**. Producer: online-services (`aws/online-services/`, port 8080).
Consumer: frontend (`aws/frontend/`), validation. Base path **`/api/v1`**. JSON camelCase. Money = decimal
string with 2 decimals (`"1940.00"`). Dates `yyyy-MM-dd`. Column semantics come from `data-model.md`.

## 1. Conventions

### 1.1 Error envelope (all 4xx/5xx)

```json
{
  "errorCode": "NOT_FOUND",
  "message": "Account ID NOT found...",
  "fieldErrors": [ { "field": "acctId", "message": "Acct ID can NOT be empty..." } ],
  "legacyProgram": "COBIL00C",
  "timestamp": "2026-09-28T17:00:00Z"
}
```

`message` reproduces the legacy COBOL screen message text (quoted per endpoint below) so screens show the
same wording; `fieldErrors` lists per-field validation messages (legacy programs stop at the first error and
position the cursor; the API returns at least that first error, and may return all).

### 1.2 CICS RESP / COBOL condition → HTTP

| Legacy condition | HTTP | `errorCode` |
|---|---|---|
| `DFHRESP(NORMAL)` read/browse | 200 | — |
| `DFHRESP(NORMAL)` on `WRITE` | 201 (+`Location`) | — |
| `DFHRESP(NORMAL)` on `REWRITE`/`DELETE` | 200 / 204 | — |
| Field validation failure ("… can NOT be empty…", "… must be Numeric…", "… Not a valid date…") | 400 | `VALIDATION_ERROR` |
| `DFHRESP(INVREQ)`, `LENGERR`, invalid PF key / action ("INVALID ACTION CODE") | 400 | `INVALID_REQUEST` |
| `DFHRESP(NOTFND)` | 404 | `NOT_FOUND` |
| `DFHRESP(DUPREC)` / `DUPKEY` ("User ID already exist…", "Tran ID already exist…") | 409 | `DUPLICATE` |
| Optimistic lock mismatch ("Record changed by some one else. Please review") | 409 | `CONCURRENT_UPDATE` |
| `READ UPDATE` lock failure ("Could not lock … record for update") | 409 | `LOCKED` |
| Business rule rejection ("You have nothing to pay…", "No change detected…") | 422 | `BUSINESS_RULE` |
| Missing/invalid/expired JWT | 401 | `UNAUTHENTICATED` |
| Role not permitted ("No access - Admin Only option…") | 403 | `FORBIDDEN` |
| Other `RESP` (`NOTOPEN`, `DISABLED`, `IOERR`…) / "Unable to …" / ABEND | 500 | `INTERNAL_ERROR` (legacy "File Error: …", "Unable to lookup …") |

### 1.3 Paging (replaces `STARTBR`/`READNEXT`/`READPREV`/`ENDBR`)

Keyset pagination on the VSAM key:

```
GET …?startKey=<key>&direction=next|prev&pageSize=<n>
→ { "items": [...], "firstKey": "...", "lastKey": "...", "hasNext": true, "hasPrev": false }
```

* `direction=next` (PF8): rows with key `>= startKey` ascending (legacy `STARTBR GTEQ` + `READNEXT`);
  client passes `lastKey` of current page with `exclusive=true` to get the following page.
* `direction=prev` (PF7): rows with key `< startKey` descending then re-sorted ascending (legacy `READPREV`).
* `pageSize` default = legacy rows per screen (cards 7, transactions 10, users 10); max 100.
* `hasNext=false` ↔ legacy "You have reached the bottom of the page…"/"NO MORE PAGES TO DISPLAY";
  `hasPrev=false` ↔ "You are already at the top of the page…"/"NO PREVIOUS PAGES TO DISPLAY".

### 1.4 Optimistic concurrency

`GET` detail responses include `"version": <long>` (column `version`). `PUT` bodies must echo it; mismatch
→ 409 `CONCURRENT_UPDATE`. This replaces the legacy compare of the old screen image against a fresh
`READ UPDATE` in `COACTUPC`/`COCRDUPC`.

### 1.5 Confirmation steps

Legacy screens require a second ENTER/PF5 or a `Y` confirmation field ("Confirm to add this transaction…",
"Press PF5 key to save your updates …"). The API is single-shot: the frontend performs the confirmation UI,
then calls the mutating endpoint once. `POST /bill-payments` and `POST /transactions` accept no confirm flag.

## 2. Authentication and roles

* `POST /api/v1/auth/signon` issues a JWT (HS256, `JWT_SECRET`, TTL `JWT_TTL_MINUTES`).
* Claims: `sub` = user id, `role` = `ADMIN` | `USER`, `name` = first + last name.
* Role mapping from `user_security.user_type` (`SEC-USR-TYPE`): `A` → `ADMIN`, `U` → `USER`
  (`CDEMO-USRTYP-ADMIN` / `CDEMO-USRTYP-USER` in `COCOM01Y`).
* All endpoints except signon and `/actuator/health` require `Authorization: Bearer <jwt>`.
* `ADMIN` may call every endpoint. `USER` may call everything except §8 (user admin), `/menus/admin` and the
  write endpoints of §10.2 (transaction types). Legacy: `COSGN00C` routes `A` to `COADM01C` (admin menu) and
  `U` to `COMEN01C`; the admin menu options (`COADM02Y`) are reachable only from the admin menu.
* Signoff (PF3 on signon / menu, `RETURN` without `TRANSID`) = client discards token; no endpoint.

## 3. Signon (`COSGN00C`, `CC00`, map `COSGN0A`)

### `POST /api/v1/auth/signon`

Request `{ "userId": "ADMIN001", "password": "PASSWORD" }` (both upper-cased server-side before lookup, as
`FUNCTION UPPER-CASE` in `COSGN00C`).

200:
```json
{ "token": "<jwt>", "tokenType": "Bearer", "expiresAt": "2026-09-28T18:00:00Z",
  "userId": "ADMIN001", "firstName": "Margaret", "lastName": "Gold", "role": "ADMIN",
  "nextRoute": "/admin" }
```
`nextRoute` = `/admin` for `ADMIN`, `/menu` for `USER`.

| HTTP | errorCode | Legacy message |
|---|---|---|
| 400 | `VALIDATION_ERROR` | "Please enter User ID ..." / "Please enter Password ..." |
| 401 | `INVALID_CREDENTIALS` | "User not found. Try again ..." (NOTFND) / "Wrong Password. Try again ..." — both return 401 with the same `errorCode`; `message` MAY be generic ("Invalid user ID or password") to avoid user enumeration (**documented deviation** from the distinct legacy texts) |
| 500 | `INTERNAL_ERROR` | "Unable to verify the User ..." |

## 4. Menus (`COMEN01C` `CM00` / `COADM01C` `CA00`)

### `GET /api/v1/menus/main` (USER, ADMIN)

Options from `COMEN02Y` (all have `CDEMO-MENU-OPT-USRTYPE = 'U'`):

```json
{ "options": [
  {"number":1,"name":"Account View","legacyProgram":"COACTVWC","route":"/accounts/view","adminOnly":false,"installed":true},
  {"number":2,"name":"Account Update","legacyProgram":"COACTUPC","route":"/accounts/update","adminOnly":false,"installed":true},
  {"number":3,"name":"Credit Card List","legacyProgram":"COCRDLIC","route":"/cards","adminOnly":false,"installed":true},
  {"number":4,"name":"Credit Card View","legacyProgram":"COCRDSLC","route":"/cards/view","adminOnly":false,"installed":true},
  {"number":5,"name":"Credit Card Update","legacyProgram":"COCRDUPC","route":"/cards/update","adminOnly":false,"installed":true},
  {"number":6,"name":"Transaction List","legacyProgram":"COTRN00C","route":"/transactions","adminOnly":false,"installed":true},
  {"number":7,"name":"Transaction View","legacyProgram":"COTRN01C","route":"/transactions/view","adminOnly":false,"installed":true},
  {"number":8,"name":"Transaction Add","legacyProgram":"COTRN02C","route":"/transactions/new","adminOnly":false,"installed":true},
  {"number":9,"name":"Transaction Reports","legacyProgram":"CORPT00C","route":"/reports","adminOnly":false,"installed":true},
  {"number":10,"name":"Bill Payment","legacyProgram":"COBIL00C","route":"/bill-payment","adminOnly":false,"installed":true},
  {"number":11,"name":"Pending Authorization View","legacyProgram":"COPAUS0C","route":"/authorizations","adminOnly":false,"installed":false}
]}
```

`installed` reflects deployment of optional modules (legacy: `INQUIRE PROGRAM` → "This option … is not
installed …" / "is coming soon ..."). Option 11 is `false` unless the authorization module is deployed.

### `GET /api/v1/menus/admin` (ADMIN only; USER → 403 "No access - Admin Only option... ")

Options from `COADM02Y`: 1 User List (Security) `COUSR00C` `/admin/users`; 2 User Add `COUSR01C`
`/admin/users/new`; 3 User Update `COUSR02C` `/admin/users/:userId/edit`; 4 User Delete `COUSR03C`
`/admin/users/:userId/delete`; 5 Transaction Type List/Update (Db2) `COTRTLIC` `/admin/transaction-types`;
6 Transaction Type Maintenance (Db2) `COTRTUPC` `/admin/transaction-types/maintain`. Same JSON shape.

## 5. Accounts

### `GET /api/v1/accounts/{acctId}` — `COACTVWC` (`CAVW`, map `CACTVWA`)

Reads `card_xref` by `acct_id` (AIX `CXACAIX`) → `account` → `customer`.

200:
```json
{ "acctId": 11, "activeStatus": "Y", "currBal": "1940.00", "creditLimit": "2020.00",
  "cashCreditLimit": "1020.00", "openDate": "2014-11-20", "expirationDate": "2025-05-20",
  "reissueDate": "2025-05-20", "currCycCredit": "0.00", "currCycDebit": "0.00", "groupId": "A000000000",
  "version": 0,
  "customer": { "custId": 1, "firstName": "…", "middleName": "…", "lastName": "…",
    "addrLine1": "…", "addrLine2": "…", "addrLine3": "…", "addrStateCd": "NY", "addrCountryCd": "USA",
    "addrZip": "…", "phoneNum1": "(123)456-7890", "phoneNum2": "…", "ssn": "123456789",
    "govtIssuedId": "…", "dob": "1970-01-01", "eftAccountId": "…", "priCardHolderInd": "Y",
    "ficoCreditScore": 750, "version": 0 } }
```

| HTTP | Legacy message |
|---|---|
| 400 | "Account number not provided" / "Account number must be a non zero 11 digit number" |
| 404 | "Did not find this account in account card xref file" / "Did not find this account in account master file" / "Did not find associated customer in master file" |
| 500 | "Error reading account card xref File" / "File Error: …" |

### `PUT /api/v1/accounts/{acctId}` — `COACTUPC` (`CAUP`, map `CACTUPA`)

Body = same shape as the GET response (all account fields except `acctId`, plus nested `customer` fields
except `custId`), including both `version`s. Account and customer are updated in **one DB transaction**
(legacy: `READ UPDATE` + `REWRITE` of `ACCTDAT` and `CUSTDAT`, rollback on failure).

Validation (from `COACTUPC`): `activeStatus` ∈ Y/N ("Account Active Status must be Y or N"); credit limits
numeric ("Credit Limit must be supplied"/"Credit Limit is not valid"); dates valid (`CSUTLDPY` rules);
names alphabetic ("Name can only contain alphabets and spaces", "Last name not provided"); SSN parts
("SSN: First 3 chars" … not 000/666/900–999); phone parts; state/ZIP consistency ("Invalid zip code for
state", `CSLKPCDY`); `ficoCreditScore` 300–850; `priCardHolderInd` Y/N.

| HTTP | Legacy message |
|---|---|
| 200 | "Changes committed to database" (returns updated resource) |
| 400 | field validation messages above |
| 404 | as GET |
| 409 `CONCURRENT_UPDATE` | "Record changed by some one else. Please review" |
| 409 `LOCKED` | "Could not lock account record for update" / "Could not lock customer record for update" |
| 422 | "No change detected with respect to values fetched." |
| 500 | "Update of record failed" / "Changes unsuccessful. Please try again" |

## 6. Cards

### `GET /api/v1/cards?acctId=&cardNum=&startKey=&direction=&pageSize=7` — `COCRDLIC` (`CCLI`, map `CCRDLIA`)

Browse `card` by `card_num` (`STARTBR`/`READNEXT`/`READPREV` on `CARDDAT`), optional filters `acctId`
(11 digits) and `cardNum` (16 digits). Items: `{ "cardNum", "acctId", "activeStatus" }` (screen columns).

| HTTP | Legacy message |
|---|---|
| 200 (empty `items`) | "NO RECORDS FOUND FOR THIS SEARCH CONDITION." |
| 400 | "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" / "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER" |

Row actions `S` (detail → §6 GET) and `U` (update → §6 PUT) are frontend navigation; "PLEASE SELECT ONLY ONE
RECORD TO VIEW OR UPDATE" is enforced client-side.

### `GET /api/v1/cards/{cardNum}?acctId=` — `COCRDSLC` (`CCDL`, map `CCRDSLA`)

200: `{ "cardNum", "acctId", "cvvCd", "embossedName", "expirationDate", "activeStatus", "version" }`.
If `acctId` is given it must match `card.acct_id`.

| HTTP | Legacy message |
|---|---|
| 400 | "Card number not provided" / "Card number if supplied must be a 16 digit number" / "Account number must be a non zero 11 digit number" |
| 404 | "Did not find cards for this search condition" / "Did not find this account in cards database" |

### `PUT /api/v1/cards/{cardNum}` — `COCRDUPC` (`CCUP`, map `CCRDUPA`)

Body `{ "acctId", "embossedName", "activeStatus", "expirationDate", "version" }` (`cvvCd` not editable on the
screen). Validation: "Card name not provided", "Card name can only contain alphabets and spaces",
"Card Active Status must be Y or N", "Card expiry month must be between 1 and 12", "Invalid card expiry year".

Responses: 200 "Changes committed to database"; 400 validation; 404 as GET; 409 `CONCURRENT_UPDATE`
"Record changed by some one else. Please review"; 409 `LOCKED` "Could not lock record for update";
422 "No change detected with respect to values fetched."; 500 "Update of record failed".

## 7. Transactions, bill payment, reports

### `GET /api/v1/transactions?startKey=&direction=&pageSize=10` — `COTRN00C` (`CT00`, map `COTRN0A`)

Browse `transaction` by `tran_id`. `startKey` must be numeric ("Tran ID must be Numeric ..." → 400).
Items: `{ "tranId", "origDate", "description", "amt" }` (screen columns; `origDate` = date part of `orig_ts`).

### `GET /api/v1/transactions/{tranId}` — `COTRN01C` (`CT01`, map `COTRN1A`)

200: all `transaction` columns: `{ "tranId","cardNum","typeCd","catCd","source","description","amt",
"origTs","procTs","merchantId","merchantName","merchantCity","merchantZip" }`.
400 "Tran ID can NOT be empty..."; 404 "Transaction ID NOT found..."; 500 "Unable to lookup Transaction...".

### `POST /api/v1/transactions` — `COTRN02C` (`CT02`, map `COTRN2A`)

Request:
```json
{ "acctId": 11, "cardNum": null, "typeCd": "01", "catCd": 1, "source": "POS TERM",
  "description": "Purchase", "amt": "-12.34", "origDate": "2026-09-28", "procDate": "2026-09-28",
  "merchantId": 800000000, "merchantName": "…", "merchantCity": "…", "merchantZip": "…" }
```
Exactly one of `acctId`/`cardNum` required ("Account or Card Number must be entered..."); account resolved to
card via `card_xref` (AIX) or card validated via `card_xref`. `tranId` = (max existing `tran_id`) + 1,
zero-padded to 16 (legacy `STARTBR HIGH-VALUES` + `READPREV` + `ADD 1`), generated in the same DB transaction.
`amt` format `-99999999.99` ("Amount should be in format -99999999.99"); dates valid `yyyy-MM-dd`
(`CSUTLDTC`). `origTs`/`procTs` stored as the given date at 00:00:00.

201 `{ "tranId": "0000000000000301", "message": "Transaction added successfully. Your Tran ID is 0000000000000301." }`.
400 validation messages (see inventory §3 for the full list); 404 "Account ID NOT found..." / "Card Number NOT
found..."; 409 "Tran ID already exist..."; 500 "Unable to Add Transaction...".

### `POST /api/v1/bill-payments` — `COBIL00C` (`CB00`, map `COBIL0A`)

Request `{ "acctId": 11 }`. Pays the full current balance:
1. read `account` (404 "Account ID NOT found..."); if `curr_bal <= 0` → 422 "You have nothing to pay...";
2. read `card_xref` by `acct_id` (500 "Unable to lookup XREF AIX file...");
3. insert `transaction` { `tran_id` = max+1, `type_cd`='02', `cat_cd`=2, `source`='POS TERM',
   `description`='BILL PAYMENT - ONLINE', `amt`=`curr_bal`, `merchant_id`=999999999,
   `merchant_name`='BILL PAYMENT', `merchant_city`/`merchant_zip`='N/A', `card_num`=xref card,
   `orig_ts`=`proc_ts`=now };
4. `account.curr_bal = curr_bal - amt` — steps 3–4 in one DB transaction.

`GET /api/v1/bill-payments/{acctId}` returns `{ "acctId", "currBal" }` for the confirmation screen.
201 `{ "tranId": "…", "amount": "1940.00", "message": "Payment successful. Your Transaction ID is …." }`;
400 "Acct ID can NOT be empty..."; 409 "Tran ID already exist..."; 500 "Unable to Add Bill pay Transaction..." /
"Unable to Update Account...".

### `POST /api/v1/reports/transactions` — `CORPT00C` (`CR00`, map `CORPT0A`)

Request `{ "reportType": "MONTHLY" | "YEARLY" | "CUSTOM", "startDate": "…", "endDate": "…" }` (dates required
only for `CUSTOM`; server derives month/year ranges from the current date as `CORPT00C` does).
Publishes to SQS `carddemo-report-request` (`messaging.md` §6) — replaces the JCL written to TD queue `JOBS`.

202 `{ "requestId": "<uuid>", "reportType": "MONTHLY", "startDate": "2026-09-01", "endDate": "2026-09-30",
"message": "Monthly report submitted for printing ..." }` (legacy text: `<Monthly|Yearly|Custom> report submitted for printing ...`).
400 "Select a report type to print report..." / "Start Date - Not a valid date..." / "End Date - …" messages;
500 "Unable to Write TDQ (JOBS)...".

`GET /api/v1/reports/transactions/{requestId}` → `{ "status": "SUBMITTED|RUNNING|SUCCEEDED|FAILED",
"reportS3Key": "reports/tranrept/…" }` (new capability; legacy output went to the `TRANREPT` GDG).

## 8. User administration (ADMIN only)

| Method & path | Legacy | Notes |
|---|---|---|
| `GET /api/v1/users?startKey=&direction=&pageSize=10` | `COUSR00C` `CU00` map `COUSR0A` | items `{ userId, firstName, lastName, userType }`; browse on `user_id` |
| `GET /api/v1/users/{userId}` | `COUSR02C`/`COUSR03C` lookup | 404 "User ID NOT found..." |
| `POST /api/v1/users` | `COUSR01C` `CU01` map `COUSR1A` | body `{ userId, firstName, lastName, password, userType }`; 201; 409 "User ID already exist..." |
| `PUT /api/v1/users/{userId}` | `COUSR02C` `CU02` map `COUSR2A` | body `{ firstName, lastName, password?, userType, version }`; 422 "Please modify to update ..." when unchanged |
| `DELETE /api/v1/users/{userId}` | `COUSR03C` `CU03` map `COUSR3A` | 204; 404 "User ID NOT found..." |

Validation messages: "User ID can NOT be empty...", "First Name can NOT be empty...", "Last Name can NOT be
empty...", "Password can NOT be empty..." (required on POST), "User Type can NOT be empty..." (`A`/`U`).
Responses never include the password/hash. 500: "Unable to Add User..." / "Unable to Update User..." /
"Unable to lookup User...".

## 9. Frontend routes (React Router)

| Route | Legacy map (mapset/map) | API |
|---|---|---|
| `/login` | `COSGN00`/`COSGN0A` | §3 |
| `/menu` | `COMEN01`/`COMEN1A` | §4 main |
| `/admin` | `COADM01`/`COADM1A` | §4 admin |
| `/accounts/view` | `COACTVW`/`CACTVWA` | §5 GET |
| `/accounts/update` | `COACTUP`/`CACTUPA` | §5 GET+PUT |
| `/cards` | `COCRDLI`/`CCRDLIA` | §6 list |
| `/cards/view` | `COCRDSL`/`CCRDSLA` | §6 GET |
| `/cards/update` | `COCRDUP`/`CCRDUPA` | §6 GET+PUT |
| `/transactions` | `COTRN00`/`COTRN0A` | §7 list |
| `/transactions/view` | `COTRN01`/`COTRN1A` | §7 GET |
| `/transactions/new` | `COTRN02`/`COTRN2A` | §7 POST |
| `/reports` | `CORPT00`/`CORPT0A` | §7 reports |
| `/bill-payment` | `COBIL00`/`COBIL0A` | §7 bill payments |
| `/admin/users` | `COUSR00`/`COUSR0A` | §8 list |
| `/admin/users/new` | `COUSR01`/`COUSR1A` | §8 POST |
| `/admin/users/:userId/edit` | `COUSR02`/`COUSR2A` | §8 GET+PUT |
| `/admin/users/:userId/delete` | `COUSR03`/`COUSR3A` | §8 GET+DELETE |
| `/authorizations` | `COPAU00`/`COPAU0A` (optional) | §10.1 |
| `/authorizations/:acctId/:authKey` | `COPAU01`/`COPAU1A` (optional) | §10.1 |
| `/admin/transaction-types` | `COTRTLI`/`CTRTLIA` (optional) | §10.2 |
| `/admin/transaction-types/maintain` | `COTRTUP`/`CTRTUPA` (optional) | §10.2 |

View/update routes take `?acctId=&cardNum=` / `?tranId=` query parameters (legacy passes them in the
COMMAREA `CDEMO-ACCT-ID`, `CDEMO-CARD-NUM`, `CDEMO-CT00-TRN-SELECTED`).

## 10. Optional sub-app endpoints

### 10.1 Pending authorizations (`COPAUS0C` `CPVS`, `COPAUS1C` `CPVD`, `COPAUS2C`) — **replatform candidate**

| Method & path | Legacy |
|---|---|
| `GET /api/v1/authorizations/{acctId}?startKey=&direction=&pageSize=5` | `COPAUS0C`: summary (`PAUTSUM0`) + detail list (`GNP` of `PAUTDTL1`), account/customer/card info |
| `GET /api/v1/authorizations/{acctId}/{authKey}` | `COPAUS1C`: one detail (`authKey` = `<auth_date_9c>-<auth_time_9c>`) |
| `POST /api/v1/authorizations/{acctId}/{authKey}/fraud` body `{ "action": "FLAG" \| "REMOVE" }` | `COPAUS1C` PF5 → `LINK COPAUS2C`: `INSERT`/`UPDATE` `authfrds` (`auth_fraud` = `F` / `R`) and `REPL` detail |

Only implemented if the IMS data is refactored to `pending_auth_*` tables; otherwise these screens stay on
the replatformed runtime.

### 10.2 Transaction types (DB2) (`COTRTLIC` `CTLI`, `COTRTUPC` `CTTU`)

| Method & path | Role | Legacy |
|---|---|---|
| `GET /api/v1/transaction-types?typeCd=&description=&startKey=&direction=&pageSize=7` | USER, ADMIN | `COTRTLIC` cursor browse of `TRANSACTION_TYPE` (filters by type / description `LIKE`) |
| `GET /api/v1/transaction-types/{typeCd}` | USER, ADMIN | `COTRTUPC` `SELECT` |
| `POST /api/v1/transaction-types` `{ typeCd, description }` | ADMIN | `COTRTUPC` `INSERT` (duplicate key → 409; source treats any negative SQLCODE as error) |
| `PUT /api/v1/transaction-types/{typeCd}` `{ description }` | ADMIN | `COTRTLIC`/`COTRTUPC` `UPDATE` (404 on `SQLCODE +100`) |
| `DELETE /api/v1/transaction-types/{typeCd}` | ADMIN | `COTRTLIC`/`COTRTUPC` `DELETE` (409 on FK `SQLCODE -532`) |
| `GET /api/v1/transaction-types/{typeCd}/categories` | USER, ADMIN | `TRANSACTION_TYPE_CATEGORY` |

SQLCODE mapping: `0` → 200/201/204, `+100` → 404, unique violation → 409 `DUPLICATE`, `-532` (handled in `COTRTLIC`/`COTRTUPC`) → 409
`INTEGRITY_VIOLATION`, other negative → 500.

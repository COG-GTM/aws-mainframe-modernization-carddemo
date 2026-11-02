# CardDemo migration: target architecture

Ticket UNT149-4 (phase: target architecture and project foundation). Inputs: `docs/migration/scope.md`
(§2 status tables, §4 definition of done) and `docs/migration/prior-work.md` (§1, the base branch).
Anything marked **Hypothesis** is an inference, not something read in the source. The COBOL under
`app/` is the read-only reference.

## 1. Shape decision

**Decision d-shape: continue the existing five-service branch.** The Java application is
`devin/1789660764-carddemo-java-modernization`, brought onto `migration/carddemo-java` unchanged
under `modernization/`: Java 21, Spring Boot 3.3, a Maven multi-module build (`common` plus five
services), one PostgreSQL schema per service, Flyway migrations, docker-compose, Swagger UI.

The alternative was a fresh modular monolith (one app, one schema). It was recommended because it
keeps batch posting in one database transaction. The five-service shape cannot: transaction-service
updates account-service over HTTP and then writes its own rows, so a retry after a local failure
posts twice (`modernization/README.md`, "Known limitation"). That limitation is carried forward and
has to be closed by a later ticket (idempotency key on `POST /api/v1/accounts/{id}/postings` plus an
outbox or saga at the `AccountGateway` seam). Later phases are cut as "verify against the COBOL and
close gaps", not "port from scratch".

## 2. Conventions

These are already in the code; they are written down here so later tickets follow them.

| Topic | Convention |
|---|---|
| Layout | `modernization/<service>/src/main/java/com/carddemo/<service>/{api,api/dto,domain,repository,service}`; shared code only in `common` (error envelope, `PageResponse`, `CobolValues`). |
| Ports | auth 8080, customer 8081, account 8082, card 8083, transaction 8084, batch 8085 (container and default host port). |
| Data | One schema per service (`auth`, `customer`, `account`, `card`, `transaction`) in one PostgreSQL. Foreign keys only inside a schema; cross-service references are ids. VSAM keys become primary keys, alternate indexes become secondary indexes or lookup endpoints. |
| Schema changes | Flyway `V<n>__<name>.sql` per service. Tests run on PostgreSQL 16 (Testcontainers) with Flyway on and `ddl-auto: validate`; see [conventions.md](conventions.md) §9. |
| Money | `S9(n)V99` → `NUMERIC(n+2,2)` / `BigDecimal`; no floating point. |
| Locking | CICS READ UPDATE → JPA `@Version` optimistic locking. |
| Cross-service calls | transaction-service → account/card over HTTP through `AccountGateway` / `CardGateway`. |
| Batch | Spring Batch jobs registered by JCL member name (`com.carddemo.common.batch`), in transaction-service (POSTTRAN, INTCALC, COMBTRAN) and batch-service (the rest, see `jcl-mapping.md`); launched by `java -jar <service>.jar --job=<JCL member>` or `POST /api/v1/batch/...`; output DDs are files in `<output-dir>/<JOB>/<timestamp>/<DD>`; an external scheduler replaces the JCL submit. |
| Errors | Business rule failures → `BusinessRuleException`, missing keys → `NotFoundException`, DUPKEY → `DuplicateKeyException`, rendered by `RestExceptionHandler` as `ApiError`. |
| Build | `cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify`; CI runs the same in `.github/workflows/modernization-verify.yml`. |
| Local run | `docker compose up -d --build`. Where Maven Central is blocked set `MAVEN_MIRROR_URL`; if host port 8084 is taken, remap it with an override file (`ports: !override`), not by editing `docker-compose.yml`. |

## 3. What replaces the BMS screens

The REST contract in `modernization/docs/API-CONTRACT.md` replaces the 3270 maps: each CICS
transaction is one endpoint, the BMS fields are the request/response fields, and the COMMAREA
becomes explicit request parameters plus a signed session token (§3.1). Decision d-ui: REST API only;
a web UI is an optional later phase. The API rules (resource naming, `ApiError` for `WS-MESSAGE`,
`PageResponse` for PF7/PF8, token and admin role for the COMMAREA user id and `CDEMO-USRTYP-ADMIN`,
`GET /api/v1/menu` for COMEN01C/COADM01C, async report jobs for CORPT00C, a batch job for statements)
are in [`api-style.md`](api-style.md). Online programs are verified at the REST level against
scenarios derived from the COBOL source (scope §4.2).

### 3.1 Authentication: the token replaces the COMMAREA user fields

- **Token.** `POST /api/v1/auth/signon` returns `token` (a JWT signed with HS256, `nimbus-jose-jwt`),
  `tokenType: Bearer` and `expiresAt`. Claims: `sub` = `CDEMO-USER-ID`, `userType` = `CDEMO-USER-TYPE`
  (`A`/`U`), `iss` = `carddemo-auth-service`, `iat`, `exp`, `jti`. HS256 was chosen over RS256 because
  all services are one trust domain deployed together; one shared key keeps the setup to a single
  variable. Moving to RS256/JWKS later only touches `common/security/TokenService`.
- **Key and lifetime.** `CARDDEMO_TOKEN_SECRET` (`carddemo.security.token-secret`, at least 32 bytes)
  must be the same on every service; the services refuse to start without it. `docker-compose.yml`
  supplies a dev-only default, nothing else does. Lifetime `CARDDEMO_TOKEN_TTL`, default `PT8H`
  (one working shift, the closest analogue of a CICS terminal session); there is no refresh or
  revocation, sign-off (PF3) is the client dropping the token.
- **Filter.** `common/security/SecurityConfig` (picked up by every service's `com.carddemo.common`
  scan) registers `AuthenticationFilter` on `/api/*`: missing token → 401 `Authentication required.
  Sign on first ...`, bad signature/expired/unknown `userType` → 401 `Session token is invalid or
  expired. Sign on again ...`, both as `ApiError` with `WWW-Authenticate: Bearer`. Public:
  `/actuator/**`, `/v3/api-docs/**`, Swagger UI (all outside `/api/`) and `carddemo.security.public-paths`
  (only `/api/v1/auth/signon`, set in auth-service).
- **Roles.** `@AdminOnly` on a controller or method → 403 `ApiError` `No access - Admin Only option... `
  (COMEN01C's text) for a `U` token. Applied to `UserController` (`/api/v1/users/**`). No other endpoint
  is role-restricted and there are no per-user data checks, because the COBOL has none (`CSUSR01Y`
  carries no account or customer id).
- **Caller in code.** `CallerIdentity.from(request)` / `CallerIdentity.current()` give user id and type;
  later tickets (UNT149-17/18/19) read it there instead of adding parameters.
- **Gateways.** transaction-service's account/card `RestClient`s carry `CallerTokenForwarder`, which
  copies the inbound `Authorization` header, so account/card-service authorise the original caller. The
  HTTP batch triggers run in the request thread and forward the caller's token the same way. A job
  launched from the command line (`--job=`, the scheduler path) runs in a non-web context with no
  caller, so there (and only there) the forwarder signs with the shared key as `sub` = `BATCH`,
  `userType` = `U` (no admin rights; the account/card endpoints the jobs call are not admin-only).
  Inside the web service a call with no request bound still goes out without a token and gets 401.
- **batch-service** scans `com.carddemo.common` like the others, so the same filter guards its `/api/**`
  (it has no endpoints yet) and it needs `CARDDEMO_TOKEN_SECRET` too, also for CLI launches.
- **Password case (COBOL fact, adapted).** COSGN00C upper-cases the typed password before comparing,
  so only upper-case stored passwords can ever match in CICS (COUSR01C stores what was typed). The
  service accepts the password as typed or upper-cased: USRSEC users sign on case-insensitively as in
  CICS, and mixed-case passwords set through the user API still work.

### 3.2 Online → batch hand-off (CORPT00C → TRANREPT)

**COBOL.** CORPT00C edits the dates, fills them into the inline TRANREPT JCL in `JOB-DATA` and
writes it line by line to TDQ `JOBS`, the CICS internal reader (`SUBMIT-JOB-TO-INTRDR`). JES runs
the job later; the screen only says `<type> report submitted for printing ...` and never sees the
result, which is the `TRANREPT` report data set.

**Java (decision d-report-trigger: in-process async job with polling).**

1. `POST /api/v1/reports/transactions` (batch-service, any signed-on user, menu option 9) runs the
   CORPT00C edits (`ReportDates`) and records the request in `batch.transaction_report_request`
   (status `SUBMITTED`, submitter, dates). That row is the job card and the JES job status.
2. The request is queued on one worker thread in the same JVM (one initiator, so runs never overlap)
   and the caller gets `202` + `Location: /api/v1/reports/transactions/{reportId}` at once.
3. The worker runs the same TRANREPT job as `POST /api/v1/batch/transaction-report` and the CLI
   (`TranreptJobConfig.arguments(start, end)` → `BatchJobLauncher`), calling card-service and
   transaction-service with the **submitter's** bearer token (captured on the request,
   `CallerTokenForwarder.onBehalfOf`); it never signs as `BATCH`.
4. The row moves to `RUNNING`, then `COMPLETED` (with job execution id, return code and output
   generation) or `FAILED`. `GET .../{reportId}` polls it (submitter or admin) and gives `reportPath`;
   `GET .../{reportId}/content` returns the `TRANREPT` file.
5. The queue lives in memory: requests a stopped batch-service had not finished are marked `FAILED`
   (`Interrupted: ...`) at the next start-up of the web service (CLI `--job=` launches skip this). This
   assumes one batch-service web instance, as in compose; several would need an owner/lease column. A message queue (what the internal reader models) is
   the production-grade replacement and would change only step 2.

## 4. Status per service

Status labels: **complete** – does what its COBOL does as far as code reading shows; **partial** –
the endpoint or job exists and runs, but COBOL rules are missing or changed (listed); **stub** –
named in the docs, no code. Nothing here has been compared with COBOL output yet; every row still
needs the definition of done in scope §4. "Branch label" is the status the base branch gives itself
(`modernization/docs/COBOL-PROGRAM-MAPPING.md`, copied into scope §2).

| Module | Status | Notes |
|---|---|---|
| `common` | complete | Error envelope, pagination, `CobolValues` (PIC X trim, date/timestamp parsing). |
| `auth-service` | partial | Signon issues the bearer token (§3.1), user CRUD is admin-only and follows COUSR00C–03C (§4.1), `GET /api/v1/menu` serves COMEN01C/COADM01C. |
| `customer-service` | partial | Read/list/update with no field edits; the customer edits of COACTUPC are not here. |
| `account-service` | partial | View works; update covers account fields only; posting/interest settlement follow CBTRN02C/CBACT04C. |
| `card-service` | complete | COCRDLIC/SLC/UPC reviewed paragraph by paragraph (`card-service-review.md`). |
| `transaction-service` | partial | Online list/view/add/bill pay and both batch jobs run; reports are stubs. |
| `batch-service` | partial | TRANREPT (CBTRN03C) and the CORPT00C report submission (§3.2); CREASTMT, CBEXPORT/CBIMPORT, READACCT/CARD/CUST/XREF and WAITSTEP run (UNT149-15, golden set in [validation/batch-service-golden-set.md](validation/batch-service-golden-set.md)); those UNT149-15 jobs read the service schemas read-only over JDBC and call no other service. |
| MQ replacement, client | stub | Not built (`modernization/README.md`). |

### 4.1 Online programs

| Program | Branch label | Verified status | What the code shows |
|---|---|---|---|
| COSGN00C | ported | complete | `AuthService.signon`: BCrypt check, COBOL messages (`Wrong Password. Try again ...`, `User not found. Try again ...`, `Please enter User ID ...`, `Please enter Password ...`), `nextScreen` routing, HS256 token replacing `CDEMO-USER-ID`/`CDEMO-USER-TYPE` (§3.1). |
| COMEN01C | ported | complete | `GET /api/v1/menu` for a `U` token (COMEN02Y, 11 options in order); `GET /api/v1/menu/options/{n}` keeps the range check, the admin-only check (unreachable with the shipped table) and `This option ... is not installed...`. |
| COADM01C | ported | complete | `GET /api/v1/menu` for an `A` token (COADM02Y, 6 options); options 5–6 (Db2) `available: false`, `This option is not installed ...`. |
| COACTVWC | ported | complete | `GET /api/v1/accounts/{id}`; card → account through `GET /api/v1/cards/{n}/xref`. |
| COACTUPC | ported | partial | COBOL rewrites both ACCTDATA and CUSTDATA (lines 4066, 4086) and edits phone, SSN, state, ZIP, FICO (`1260`–`1280-EDIT-*`). Java `AccountService.update` writes account fields only, with three record-level checks; `CustomerService.update` has no edits. |
| COCRDLIC | ported | complete | `GET /api/v1/cards?accountId=&cardNumber=&customerId=`, all filters intersecting, COBOL filter messages, no user-type filter (none in the code). Paging is by page number, not STARTBR key. |
| COCRDSLC | ported | complete | `GET /api/v1/cards/{n}`, 2220-EDIT-CARD messages, NOTFND `Did not find cards for this search condition`. |
| COCRDUPC | ported | complete | `PUT /api/v1/cards/{n}` with name/status/month/year edits and messages, upper-case no-change check, `READ UPDATE` lock + client `version` → 409; day, CVV and account kept (`card-service-review.md`). |
| COTRN00C | ported | complete | `GET /api/v1/transactions?startTransactionId=&page=&size=10`: TRAN-ID order from the typed start id (zero padded to 16 digits; COBOL `STARTBR` at `TRNIDIN`), `Tran ID must be Numeric ...`; the `cardNumber` filter is an extra. Paging is by page number, not READNEXT/READPREV keys (api-style §4). |
| COTRN01C | ported | complete | `GET /api/v1/transactions/{id}`, `Transaction ID NOT found...`. The COBOL `READ ... UPDATE` is never followed by a REWRITE, so it is a plain read. |
| COTRN02C | ported | complete | `TransactionAddValidator`: account id (via CXACAIX) or card number (via CCXREF), every empty/numeric/amount (`-99999999.99`)/date-format edit with the COBOL message, dates through `CsutldtcDate` (CEEDAYS; msg 2513 ignored as in COBOL). TRAN-ID from `TransactionIdGenerator` (database sequence lifted past the highest numeric id = last id + 1, d-tran-id). Differences: all failing fields are reported (first = the COBOL cursor field); the XREF lookup runs after the field edits, COBOL looks up first; PF5 "copy last transaction" is client-side (`GET` the last row). |
| CORPT00C | not built | complete | batch-service `POST /api/v1/reports/transactions` (§3.2): the CORPT00C edits in COBOL order and text, monthly/yearly dates computed, TRANREPT run asynchronously in-process, polled by report id. Confirmation is the POST itself. |
| COBIL00C | ported | complete | Type 02 / cat 2 / `POS TERM` / "BILL PAYMENT - ONLINE" / merchant 999999999 / N/A, full `ACCT-CURR-BAL`, `You have nothing to pay...`, `Account ID NOT found...`, `Acct ID can NOT be empty...`. TRAN-ID as COTRN02C. The balance update is the account-service posting endpoint with `type: BILL_PAYMENT` (balance only, no CBTRN02C 102/103 rejects, cycle buckets untouched, like COBIL00C), keyed by the TRAN-ID (PR #168 ledger). Residual risk in §6. |
| COUSR00C | ported | complete | `GET /api/v1/users`: 10 rows a page in user-id order (USRID01–10), `startUserId` = the "Search User ID" STARTBR key, page number for PF7/PF8. Row selection `U`/`D` is the client calling PUT/DELETE. |
| COUSR01C | ported | complete | All five fields required, checked in COUSR01C order (first name, last name, user id, password, type) with its texts (`First Name can NOT be empty...`); duplicate → 409 `User ID already exist...`. Deliberate deviations: password BCrypt-hashed, min 8 chars; type must be `A`/`U` (COBOL stores any non-blank character, but only `A` is ever admin); user id upper-cased (COSGN00C upper-cases at sign-on, so a lower-case id could never sign on). |
| COUSR02C | ported | complete | `PUT /api/v1/users/{id}`: fields checked in COUSR02C order; unknown id → 404 `User ID NOT found...`; nothing changed → 422 `Please modify to update ...`; READ UPDATE/REWRITE → JPA `@Version` (409 on a concurrent change). |
| COUSR03C | ported | complete | `DELETE /api/v1/users/{id}`: unknown id → 404 `User ID NOT found...`; READ UPDATE/DELETE in one transaction. |

### 4.2 Batch programs and utilities

| Program | Branch label | Verified status | What the code shows |
|---|---|---|---|
| CBTRN02C | ported | partial | Reasons 100–103, TCATBAL upsert and the cycle-bucket sign handling are in `PostingService` / `AccountService.post`. Input and rejects are tables, not the DALYTRAN / DALYREJS files, so the golden-set harness cannot run it yet. Double posting on retry (§1). When a record is both over limit and expired, Java rejects with 102; COBOL `1500-B-LOOKUP-ACCT` runs both checks and the later 103 overwrites 102. |
| CBACT04C | ported | partial | `InterestCalculationService` follows the account break and DISCGRP fallback; same table-only I/O gap. Differences from the COBOL: monthly interest was rounded `HALF_UP` although the COBOL `COMPUTE` (line 464) has no `ROUNDED` (fixed: now truncates, [conventions.md](conventions.md) §2); a missing account is skipped, but `1100-GET-ACCT-DATA` abends; a missing DEFAULT rate becomes zero, but `1200-A-GET-DEFAULT-INT-RATE` abends. |
| CBTRN03C | built (UNT149-14) | verified against GnuCOBOL | batch-service `TRANREPT`: REPROC unload, SORT on the DATEPARM window, `TransactionReportProgram`. TRANREPT is byte-identical to the GnuCOBOL run on 11 cases (`batch-service/src/test/resources/tranrept`, `scripts/migration/tranrept/`), including the source's quirks: `NEXT SENTENCE` ends the report on an out-of-range record, the last card's account total is never written, the last amount is added twice to the final page total, `S9(09)V99` totals overflow by truncation. |
| CBSTM03A / CBSTM03B | ported (UNT149-15) | complete | batch-service `CREASTMT`: `StatementWriter` + `StatementFileService` (the called subprogram as a service). Byte-identical STMTFILE/HTMLFILE on the sample data, from files and from the tables. Deviation: more than 51 cards or 10 transactions per card overflow the COBOL tables (undefined there); Java abends (RC 16). |
| CBEXPORT / CBIMPORT | ported (UNT149-15) | complete | batch-service `CBEXPORT` / `CBIMPORT`, byte-identical to the COBOL (EXPFILE; *OUT and ERROUT incl. an unknown-type edge set). Round trip tables → EXPFILE → *OUT equals the tables except FILLER, which CBIMPORT leaves as binary zeros, as the COBOL does. CBIMPORT writes files, not tables (as the COBOL). |
| CBTRN01C | not built | stub | |
| CBACT01C, CBACT02C, CBACT03C, CBCUS01C | ported (UNT149-15) | complete | batch-service `READACCT` / `READCARD` / `READXREF` / `READCUST`: SYSOUT and CBACT01C's OUTFILE/ARRYFILE/VBRCFILE byte-identical on the sample data (+ a CBACT01C edge set). `COBDATFT` replaced by `java.time`. |
| CSUTLDTC | `DateValidation` in `common` (UNT149-15) | partial | Same severity / message number / 80-byte message as CSUTLDTC. Which input gives which CEEDAYS condition is a hypothesis from the IBM LE docs (no LE runtime to compare). Not yet called by COTRN02C / CORPT00C ports; transaction-service `CsutldtcDate` (UNT149-19, the COTRN02C `YYYY-MM-DD` check) and batch-service `ReportDates` (UNT149-20, CORPT00C: 2517 bad month, 2508 bad day, 2513 before 1582-10-15 accepted) are stopgaps that can switch to `DateValidation`. |
| COBSWAIT | replaced | replaced | Job ordering replaces the wait; batch-service `WAITSTEP` keeps the job (`Thread.sleep` for `MVSWAIT`). |

## 5. Verified on this branch

- `mvn -B verify`: BUILD SUCCESS, 6 modules, 41 tests, 0 failures.
- `docker compose up -d --build` (with `MAVEN_MIRROR_URL` and host 8084 remapped to 8094): five
  `Started ...Application` lines, `/actuator/health` UP on 8080–8083 and 8094.

Command output is in the PR that added this file.

## 6. Open review findings not fixed here

This PR changes no business logic, so these findings stay open for the phases that close gaps:

- The CBTRN02C and CBACT04C differences listed in §4.2.
- Running interest calculation again charges unchanged category balances again. The COBOL behaves the
  same way, because CBACT04C opens TCATBAL as input only. Guarding against a second run per period is
  a scheduling and idempotency decision for the batch phase, not a parity fix.
- COBIL00C across two services (UNT149-19). COBOL holds ACCTDAT with READ UPDATE and CICS backs out
  the TRANSACT WRITE if the REWRITE fails. Java takes a per-account advisory lock, writes and flushes
  the TRANSACT row, posts `-balance` with the TRAN-ID as idempotency key (retried up to 3 times when
  the outcome is unknown), and rolls the row back if the posting is rejected or never answered.
  Remaining window: account-service applied the posting but every answer was lost, or the local
  commit fails after a successful posting. Then the account is debited with no TRANSACT row.
  Detection: an `account.account_postings` row whose `tran_id` is missing from
  `transaction.transactions`. There is no automatic repair.
- Online TRAN-IDs come from `transaction.tran_id_seq`, lifted past the highest numeric TRAN-ID
  already stored on each allocation. A DALYTRAN id loaded later that equals an id already handed out
  makes POSTTRAN abend on the duplicate. COBOL has the same collision.
- Card update keeps COCRDUPC 1230-EDIT-NAME exactly (letters and spaces only, on every update), so 1
  of the 50 sample cards (`7026637615032277`, `Lucious O'Connell`) cannot change status or expiry
  without changing its name, as on the mainframe. Relaxing the edit for unchanged names is a product
  decision (UNT149-18, `card-service-review.md`).
- Security, carried over from the base branch: no TLS between services, the USRSEC
  sample users loaded by `data-loader` (`ADMIN001` … `USER0005`, password `PASSWORD`), `carddemo` / `carddemo` default database credentials, and Compose
  publishing the service ports on every host interface. These are acceptable for a local demo only.

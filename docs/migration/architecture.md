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

## 4. Status per service

Status labels: **complete** – does what its COBOL does as far as code reading shows; **partial** –
the endpoint or job exists and runs, but COBOL rules are missing or changed (listed); **stub** –
named in the docs, no code. Nothing here has been compared with COBOL output yet; every row still
needs the definition of done in scope §4. "Branch label" is the status the base branch gives itself
(`modernization/docs/COBOL-PROGRAM-MAPPING.md`, copied into scope §2).

| Module | Status | Notes |
|---|---|---|
| `common` | complete | Error envelope, pagination, `CobolValues` (PIC X trim, date/timestamp parsing). |
| `auth-service` | partial | Signon issues the bearer token (§3.1), user CRUD is admin-only, `GET /api/v1/menu` serves COMEN01C/COADM01C. |
| `customer-service` | partial | Read/list/update; the COACTUPC customer edits run in account-service before it calls the update (version-checked, optional SSN). |
| `account-service` | partial | COACTVWC/COACTUPC reviewed (UNT149-17, `account-view-update.md`); posting/interest settlement follow CBTRN02C/CBACT04C. |
| `card-service` | partial | List/view/update work; COCRDUPC date edits incomplete. |
| `transaction-service` | partial | Online list/view/add/bill pay and both batch jobs run; reports and statements are stubs. |
| Reports, statements, export/import, MQ replacement, client | stub | Not built (`modernization/README.md`). |

### 4.1 Online programs

| Program | Branch label | Verified status | What the code shows |
|---|---|---|---|
| COSGN00C | ported | complete | `AuthService.signon`: BCrypt check, COBOL messages (`Wrong Password. Try again ...`, `User not found. Try again ...`, `Please enter User ID ...`, `Please enter Password ...`), `nextScreen` routing, HS256 token replacing `CDEMO-USER-ID`/`CDEMO-USER-TYPE` (§3.1). |
| COMEN01C | ported | complete | `GET /api/v1/menu` for a `U` token (COMEN02Y, 11 options in order); `GET /api/v1/menu/options/{n}` keeps the range check, the admin-only check (unreachable with the shipped table) and `This option ... is not installed...`. |
| COADM01C | ported | complete | `GET /api/v1/menu` for an `A` token (COADM02Y, 6 options); options 5–6 (Db2) `available: false`, `This option is not installed ...`. |
| COACTVWC | ported | complete | `GET /api/v1/accounts/{id}/view`: xref → account → customer with the COBOL not-found messages (UNT149-17). |
| COACTUPC | ported | complete | `PUT /api/v1/accounts/{id}`: all 1200-EDIT-MAP-INPUTS edits with COBOL messages (`AccountUpdateValidator`, `CSLKPCDY` tables), no-change check, version check on both records, account rewrite rolled back when the customer rewrite fails. Deviations listed in `account-view-update.md` §4 (UNT149-17). |
| COCRDLIC | ported | complete | `GET /api/v1/cards?accountId=&customerId=` with both filters intersecting. Paging is by page number, not STARTBR key. |
| COCRDSLC | ported | complete | `GET /api/v1/cards/{n}`. |
| COCRDUPC | ported | partial | COBOL requires month 1–12 and year 1950–2099 (`VALID-MONTH`, `VALID-YEAR`); Java only rejects year > 2099. |
| COTRN00C | ported | partial | COBOL browses TRANSACT from a typed transaction id; Java filters by card number. |
| COTRN01C | ported | complete | `GET /api/v1/transactions/{id}`. |
| COTRN02C | ported | partial | COBOL accepts account id or card number, validates dates through CSUTLDTC, and sets TRAN-ID = last id + 1 (READPREV, lines 446–451). Java accepts card number only, has no date edit, and uses a random 16-char id. |
| CORPT00C | not built | stub | No endpoint. |
| COBIL00C | ported | partial | Type 02 / cat 2 / "BILL PAYMENT - ONLINE" / merchant 999999999 and the "nothing to pay" check match. TRAN-ID is random instead of last + 1. Java posts through account-service's CBTRN02C rules, so an expired account rejects the payment (reason 103); COBOL COBIL00C rewrites the balance without that check. |
| COUSR00C | ported | complete | `GET /api/v1/users`, page number instead of STARTBR key. |
| COUSR01C | ported | complete | Duplicate key rejected; password stored as BCrypt (deliberate deviation). |
| COUSR02C | ported | complete | `PUT /api/v1/users/{id}`. |
| COUSR03C | ported | complete | `DELETE /api/v1/users/{id}`. |

### 4.2 Batch programs and utilities

| Program | Branch label | Verified status | What the code shows |
|---|---|---|---|
| CBTRN02C | ported | partial | Reasons 100–103, TCATBAL upsert and the cycle-bucket sign handling are in `PostingService` / `AccountService.post`. Input and rejects are tables, not the DALYTRAN / DALYREJS files, so the golden-set harness cannot run it yet. Double posting on retry (§1). When a record is both over limit and expired, Java rejects with 102; COBOL `1500-B-LOOKUP-ACCT` runs both checks and the later 103 overwrites 102. |
| CBACT04C | ported | partial | `InterestCalculationService` follows the account break and DISCGRP fallback; same table-only I/O gap. Differences from the COBOL: monthly interest was rounded `HALF_UP` although the COBOL `COMPUTE` (line 464) has no `ROUNDED` (fixed: now truncates, [conventions.md](conventions.md) §2); a missing account is skipped, but `1100-GET-ACCT-DATA` abends; a missing DEFAULT rate becomes zero, but `1200-A-GET-DEFAULT-INT-RATE` abends. |
| CBTRN03C | built (UNT149-14) | verified against GnuCOBOL | batch-service `TRANREPT`: REPROC unload, SORT on the DATEPARM window, `TransactionReportProgram`. TRANREPT is byte-identical to the GnuCOBOL run on 11 cases (`batch-service/src/test/resources/tranrept`, `scripts/migration/tranrept/`), including the source's quirks: `NEXT SENTENCE` ends the report on an out-of-range record, the last card's account total is never written, the last amount is added twice to the final page total, `S9(09)V99` totals overflow by truncation. |
| CBSTM03A / CBSTM03B | not built | stub | |
| CBEXPORT / CBIMPORT | not built | stub | |
| CBTRN01C | not built | stub | |
| CBACT01C, CBACT02C, CBACT03C, CBCUS01C | replaced by list endpoints | partial | `GET` list endpoints exist; the dump files are not produced, which scope §2.2 requires or records as a deviation. |
| CSUTLDTC | "shared Java date check" (scope §2.2) | stub | There is no such check. Dates are only parsed (`CobolValues.date`, Jackson `LocalDate`); no CEEDAYS-style validation or messages. Scope §2.2 is wrong on this row. |
| COBSWAIT | replaced | replaced | Job ordering replaces the wait. |

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
- Security, carried over from the base branch: no TLS between services, the USRSEC
  sample users loaded by `data-loader` (`ADMIN001` … `USER0005`, password `PASSWORD`), `carddemo` / `carddemo` default database credentials, and Compose
  publishing the service ports on every host interface. These are acceptable for a local demo only.

# Review findings — whole `modernization/` tree (s6.4)

Review surface: `git diff main...HEAD -- modernization` (the full Java/UI tree, since `main` has none of it) plus
`scripts/`, `docs/modernization/` and the CI workflow, against the checklist correctness / tests / security /
dependencies / data / operability. Each finding is **fixed** in this PR or **accepted** with a reason.

## Ticket items 1.1–1.7

| Item | Outcome | Where |
| --- | --- | --- |
| 1.1 Passwords | **Fixed.** `user_security.password_hash` (Flyway V6), BCrypt via `PasswordEncoderFactories` (`{bcrypt}` prefix), upgrade on first successful plain-text login, COUSR01C/02C write both columns. `SEC-USR-PWD` unchanged → `unload`/`cbexport`/EBCDIC re-encode and golden set identical. | ADR-0023 (supersedes ADR-0018 §Decision for storage), `PasswordHashSignOnTest`, `SignOnServiceTest`, `UserPasswordWriteTest`, `PasswordAndAdminHardeningIT` (first sign-on hashes, unload byte-identical; add/update write both) |
| 1.2 PAN | **Fixed.** Audit found one leak path: a 4xx/5xx RFC 7807 body echoed the request URI (`instance`) for `/api/v1/cards/{pan}`; now masked via `PanMask.maskCardPath` (`PanMaskPathTest`). | `CardPanLoggingTest`, `TransactionPanLoggingTest` (Logback appender capture over every card/transaction endpoint, asserts no full PAN in logs/bodies except the two detail screens of ADR-0020) |
| 1.3 Secrets | **Fixed.** No secret values in `application*.yml` or compose (the `local`/`test` JWT defaults were removed; tests and CI generate a random key per run); `.env.example` lists every variable without values; < 32 bytes or blank → startup fails. | `JwtSecretStartupIT` (blank, 31 bytes, 32 bytes), `09-configuration.md` |
| 1.4 Dependencies | **Fixed where feasible**, remainder justified. Spring Boot 3.3.13 → 3.5.16 + patch pins; 112 → 23 Snyk issues; UI 0. | `dependency-scan.md`, `make dependency-scan` |
| 1.5a POSTTRAN/COMBTRAN id lock | **Fixed.** Table-mode POSTTRAN holds `TRAN_ID_LOCK` as a session lock for the whole step; COMBTRAN/`repro`/`initial-load` take it per load transaction; bill payment takes it before the account row lock (one lock order). | `TransactionIdLockIT`, rules `CBTRN02C.md` D-1, runbook |
| 1.5b Demoted admin | **Fixed.** Every ADMIN route re-reads `usr_type` (`CurrentAdminAuthorization`) → 403 `NOTAUTH` immediately after demotion, delete, or a lookup failure. | `PasswordAndAdminHardeningIT` (demotion with the old token), `UserAdminRoleTest` (deleted user, lookup failure) |
| 1.5c TRANBKP window | **Accepted, documented** (no code change): online adds between TRANBKP and COMBTRAN are lost; operate the cycle as a batch window. | `10-runbook-nightly-cycle.md` §"The batch window" |
| 1.6 CREASTMT HTML | **Fixed behind default-off option** `carddemo.batch.creastmt.html-escape`. | rules `CBSTM03A.md` D-1, `Cbstm03aHtmlEscapeTest` |
| 1.7 First password rejected | **Not reproduced, no defect**, regression spec added. | `signon-first-entry.md`, `e2e/signon-first-entry.spec.ts` |

## Other findings

| # | Area | Finding | Outcome |
| --- | --- | --- | --- |
| F-1 | Security | Plain-text `user_security.password` is still stored (the 8-byte `SEC-USR-PWD` must round-trip for `unload`/`cbexport`/golden set). The hash protects the compare, not the data at rest. | **Accepted.** Retiring the column means dropping VSAM export parity of USRSEC; recorded as a cutover topic in `11-handover.md` and ADR-0023 "Consequences". |
| F-2 | Security | No rate limiting / lockout / constant-time answer for unknown users on `POST /api/v1/auth/login` (COSGN00C has none; ADR-0018 §4). BCrypt cost slows brute force; an unknown user id is answered without a BCrypt compare, so timing may reveal which ids exist (not measured). | **Accepted** for the core port; belongs in the API gateway / IdP at cutover. |
| F-3 | Security | JWT is HS256 with a shared secret, 1 h TTL (`CARDDEMO_JWT_TTL`), no revocation list. Admin rights are now re-checked per request (1.5b); a deleted USER keeps its token until expiry. | **Accepted**; replace with the enterprise IdP at cutover (ADR-0017). |
| F-4 | Security | `anonymousSecurity` chain `permitAll()` for `/api/v1/auth/**`, `/actuator/health|info`, `/v3/api-docs`, `/swagger-ui*`, `/error`. Reviewed: scoped by `securityMatcher`, everything else goes to the JWT chain. OpenAPI exposure in production is a choice. | **Fixed (cheap):** `CARDDEMO_API_DOCS_ENABLED=false` turns off OpenAPI/Swagger (`09-configuration.md`). Default stays on for the demo stack. |
| F-5 | Security | nginx sets `X-Content-Type-Options`, `X-Frame-Options`, `Referrer-Policy` but no CSP/HSTS; TLS is not terminated in compose. | **Accepted**: TLS/HSTS/CSP belong to the ingress of the target platform. |
| F-6 | Security | Native SQL (`nativeQuery`, `JdbcTemplate`) reviewed: all parameterised, no string concatenation of input. No `System.out`/`printStackTrace` in `web`; no log statement with password/PAN arguments (grep + 1.2 tests). | No action. |
| F-7 | Correctness | Per-record id lock in POSTTRAN (first attempt in this PR) still allowed an online `max+1` between two batch records to collide with a later, higher DALYTRAN id. | **Fixed:** step-wide session lock (CBTRN02C D-1). Cost: online adds wait for the POSTTRAN step (104 s for 100k records, `volume-smoke.md`). |
| F-8 | Correctness | Bill payment took the account row lock before the id lock while POSTTRAN takes the id lock then account rows → possible deadlock. | **Fixed:** `BillPaymentService` takes `TransactionIds.lock()` first. |
| F-9 | Correctness | `what-this-does-not-prove.md` in the golden-set dir still says "POSTTRAN does not take the online transaction-id lock (s6.4)". | **Accepted:** it is a copied template inside the dated reconciliation that `golden-set-check` compares; regenerating a new dated dir for a sentence would hide the "no output change" evidence. Closed by this PR (F-7); `11-handover.md` says so. |
| F-10 | Tests | Unit-only coverage of `batch.load` (60%), `batch.posttran` (60%), `batch.creastmt` (76%) — these are exercised by Testcontainers ITs. | **Fixed:** JaCoCo merges unit + IT data; package rule ≥ 80% on the merged report (`coverage.md`). |
| F-11 | Dependencies | 23 open Snyk issues (2 critical, 11 high) in Spring Framework/Security/Batch/Data, Micrometer, Logback, Log4j API; fixes need Spring Boot 4 / Framework 7 or are not reachable. | **Accepted with per-issue justification** in `dependency-scan.md`. |
| F-12 | Operability | Report queue is in-process (ADR-0021): one instance only; queued requests are lost on restart (`report_request` row stays QUEUED). | **Accepted**, known limitation in `11-handover.md`. |
| F-13 | Operability | Volume: POSTTRAN 100k records 104 s, 589 MiB peak RSS at `-Xmx512m`; top SQL = per-record account update. | **Accepted** (no tuning spree); `volume-smoke.md`. |
| F-14 | Artifacts | No tracked `target/`, `dist/`, `node_modules/`, `*.orig`, `*.rej`, `*.class`, `*.pyc`, `__pycache__`; no `TODO`/`FIXME` in tracked sources (one `XXX` hit is a `PIC X` test literal). `build/volume-smoke/` and `build/dependency-scan/` added to `.gitignore`. | No action. |

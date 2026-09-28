# CardDemo on AWS — validation & parity report (Session 7)

Branch `devin/1790619634-validation-parity` = `main` + PRs 40 (discovery/contracts), 41 (data migration),
44 (online services), 45 (batch), 42 (infra/CDK), 43 (frontend) + the validation work below.

**Verdict:** 111/111 automated parity checks pass (98 online, 13 batch) and the React golden path passes against
the real services. Five small defects were fixed on this branch; the remaining discrepancies are either legacy
COBOL defects that the Java code deliberately does not reproduce, cosmetic, or structural (listed in §5).

## 1. Environment

| Component | Version / setting |
|---|---|
| Host | Ubuntu 22.04, Docker Engine + Compose v2 |
| Database | `postgres:15-alpine` via `aws/docker-compose.yml` (`POSTGRES_IMAGE`, host port 55433), schema `aws/db/schema.sql` |
| Seed | `aws/etl` (Python 3.10 venv, pinned `requirements.txt`): `app/data/EBCDIC` → CSV → Postgres, cross-checked against `app/data/ASCII` |
| Online | `aws/services` Spring Boot 3 / Java 21 (OpenJDK 21), jar started by `run.sh` on :18080; also the compose image behind nginx on :3000 |
| Batch | `aws/batch` Java 21 jar (`--spring.profiles.active=local`, file-system bucket), one fresh database per scenario |
| Ground truth | GnuCOBOL 3.1.2 (`cobc -x -std=ibm -fsign=EBCDIC -I app/cpy`) running the original `CBTRN02C` / `CBACT04C` on `app/data/ASCII` through `aws/batch/golden/generate-golden.sh` (DD_* env vars; `app/` untouched) |
| Frontend | `aws/frontend` React 18 + Vite, `VITE_USE_MOCKS` unset, same-origin `/api` proxy to the real services |
| Test runner | pytest 8.3 + requests + psycopg 3 (`aws/validation/requirements.txt`, all pinned) |

## 2. How to run

```bash
aws/validation/run.sh                  # everything: postgres 15 + ETL + services + online & batch suites
aws/validation/run.sh -m online        # REST parity only      (-m batch: batch parity only)
VALIDATION_SKIP_STACK=1 VALIDATION_API_BASE=http://localhost:3000 aws/validation/run.sh -m online   # against compose/nginx
```

`run.sh` creates a venv, builds missing jars, starts `postgres` (compose, PG 15), runs
`etl convert-all && etl crosscheck && etl load --apply-schema`, starts the services jar, waits for
`/actuator/health`, runs pytest and writes `aws/validation/target/junit.xml` (plus logs, the GnuCOBOL outputs,
the Java job bucket and `batch-summary.json`). Online tests snapshot and restore the mutated tables around every
test, so the suite is repeatable against the seeded database.

## 3. What was run

1. **Stack**: compose `postgres` (PG 15) + `localstack`; ETL crosscheck: 0 unexpected differences, 2 known
   sample-content differences (account 49 ZIP `ZEROAPR` vs `A000000000`; `DEFAULT/07/0001` rate 15.00 vs 0.00;
   EBCDIC loaded). Rows: user_security 10, account/customer/card/card_xref/tran_cat_balance 50 each,
   transaction_type 7, transaction_category 18, disclosure_group 51, daily_transaction 300, pending_auth 21/202,
   transaction 0 (by design, `DALYTRAN.PS.INIT` is a dummy record). Full compose stack (`services` + `frontend`
   images) built and healthy; the online suite passes both directly (:18080) and through nginx (:3000).
2. **Online parity** (`aws/validation/online/`, 98 cases): every expected message, status and field was taken
   from the COBOL source (paragraph names in the test docstrings/comments); every mutation is verified in
   PostgreSQL, not only in the response.
3. **Batch parity** (`aws/validation/batch/`, 13 checks): GnuCOBOL and Java run on the same input; outputs
   compared field by field (parsed with the copybook layouts in `batch/records.py`) and byte for byte where the
   Java job writes the legacy record format. A second, synthetic scenario (sample DALYTRAN + 3 records) exercises
   reject reasons that the sample data never produces.
4. **UI**: React golden path driven in Chrome against the real services + PostgreSQL (screenshots and a
   recording are attached to the PR).
5. **Module builds**: `aws/services` `mvn verify`, `aws/batch` `mvn verify` (unit + Testcontainers ITs, now on
   `aws/db/schema.sql`), `aws/frontend` `npm run lint && typecheck && test && build` — all green.

## 4. Pass/fail matrix

### 4.1 Online (REST vs COBOL)

Result column: tests / parametrized cases.

| Transaction | COBOL | Checks | Result |
|---|---|---|---|
| Signon | `COSGN00C` | blank user / blank password / unknown user / wrong password messages; case-insensitive user id; `U` → main menu, `A` → admin menu; unauthenticated call → 401 | PASS (7) |
| Menus | `COMEN01C` / `COADM01C` | 11 main options (`COMEN02Y`), 6 admin options (`COADM02Y`), admin menu → 403 "No access - Admin Only option..." for users | PASS (3) |
| Account view | `COACTVWC` | account + customer + xref card vs DB; non-numeric / zero id; account without xref | PASS (3 / 6) |
| Account update | `COACTUPC` | no change → "No change detected…"; seeded FICO 274 rejected; 18 field edits (status, dates, amounts, SSN, DOB incl. century, FICO, state, state/ZIP, phones, names, …); happy path writes `account` + `customer` (ZIP+4 kept); stale version → 409 | PASS (4 / 21) |
| Card list | `COCRDLIC` | 7-row pages in key order, next/prev, last page, account filter, filter edits, "no records" | PASS (6 / 7) |
| Card detail | `COCRDSLC` | detail vs DB; account/card key edits | PASS (2 / 2) |
| Card update | `COCRDUPC` | name/status/expiry month & year edits; happy path writes `card`; stale version → "Record changed by some one else. Please review" | PASS (2 / 8) |
| Transaction add | `COTRN02C` | 26 edits (key required, numeric acct/card, type/category/merchant numeric, amount format, dates format + calendar validity, …); add by account resolves card via xref; add by card; tran id = max+1 (16 digits); row in `transaction` | PASS (3 / 28) |
| Transaction list / detail | `COTRN00C` / `COTRN01C` | 10-row pages next/prev with `startKey`, non-numeric start key, detail vs DB, not found | PASS (4 / 4) |
| Bill payment | `COBIL00C` | balance lookup; blank / unknown account; full-balance payment writes type 02 / cat 2 / `POS TERM` / `BILL PAYMENT - ONLINE` / merchant 999999999 and zeroes `curr_bal`; second payment → "You have nothing to pay..." | PASS (4) |
| User admin | `COUSR00C`–`03C` | admin only; 10-row paging; required-field and type edits; create (bcrypt hash stored) → duplicate "User ID already exist..." → detail → no-change "Please modify to update ..." → update → delete 204 → "User ID NOT found..." | PASS (4 / 8) |

### 4.2 Batch (Java vs GnuCOBOL)

| Job | Check | Result |
|---|---|---|
| Ground truth | fresh GnuCOBOL run == committed `aws/batch/src/test/resources/golden` | PASS |
| POSTTRAN (`CBTRN02C`) | RC 4 (process exit 0), 300 read / 262 posted / 38 rejected | PASS |
| | rejects: 38 × reason 102 `OVERLIMIT TRANSACTION`, byte-identical 430-byte records | PASS |
| | `account` (all 12 columns, 50 rows) | PASS |
| | `tran_cat_balance` (50 rows; unchanged by INTCALC) | PASS |
| | posted `transaction` rows (262, all columns except the masked `proc_ts`) | PASS |
| | synthetic: +unknown card → 100, +after expiry → 103, +overlimit **and** expired → 103 (later edit wins); 303 / 41, rejects byte-identical, balances equal | PASS |
| INTCALC (`CBACT04C`) | RC 0; 50 system transactions byte-identical (timestamps masked); same rows in `transaction` | PASS |
| | `account` after interest: 49 accounts identical; account 50 differs by design (D6) and matches the COBOL formula | PASS (documented deviation) |
| | hand check of `1300-COMPUTE-INTEREST` `(bal × rate) / 1200` truncated, `DEFAULT` group fallback | PASS |
| COMBTRAN (smoke) | 262 backup + 50 systran = 312 rows, ids = union of posted and interest ids | PASS |
| CREASTMT (smoke) | 50 statements (one per xref), 80-byte text lines, `Total EXP` per account = Σ card transactions, 50 HTML statements | PASS |

### 4.3 UI (React → real services)

| Step | Result |
|---|---|
| Signon page; wrong password → "Wrong Password. Try again ..." | PASS |
| `USER0001` → main menu (11 options) | PASS |
| Account view `00000000001` (balance 194.00, customer 000000001) | PASS |
| Transaction list (empty by design) → add transaction via UI → listed | PASS |
| Bill payment 194.00 → "Payment successful. Your Transaction ID is 0000000000000002." → balance 0.00, payment in list/detail, DB confirmed | PASS (cosmetic D7) |
| Sign out → `ADMIN001` → admin menu → user list (10 users) | PASS |

## 5. Discrepancies

### Fixed on this branch

| # | Discrepancy | Root cause | Owning module | Fix |
|---|---|---|---|---|
| D1 | Account update rejected every seeded ZIP+4 customer with "Zip must be all numeric." before any other edit | Java applied the 5-digit `ACSZIPC` edit to the whole `CUST-ADDR-ZIP X(10)` value | `aws/services` `AccountService` | Edit the first 5 characters, keep the suffix, cap at 10; covered by `test_update_happy_path_writes_account_and_customer` |
| D2 | Compose `frontend` could not reach the API (and was unreachable on :3000) | nginx had no `/api` location, compose mapped `3000:80` while nginx listens on 8080, build arg `API_BASE_URL` did not match the Dockerfile's `VITE_API_BASE_URL` | `aws/frontend` (`nginx.conf`, `Dockerfile`), `aws/docker-compose.yml` | nginx template with `location /api/ { proxy_pass ${API_UPSTREAM}; }` (default `http://services:8080`), `3000:8080`, `VITE_API_BASE_URL: ""` (same origin, like CloudFront) |
| D3 | Vite dev server cannot talk to real services (no CORS on the services, no proxy) | Dev setup only covered the MSW mocks | `aws/frontend` `vite.config.ts` | Optional `VITE_API_PROXY_TARGET` → same-origin `/api` proxy |
| D4 | Batch ITs and `run-local.sh` used a private copy of the schema | Batch session predates `aws/db/schema.sql` | `aws/batch` | Both now load `aws/db/schema.sql`; duplicate `src/test/resources/db/schema.sql` removed; `mvn verify` green |
| D5 | Compose pinned PostgreSQL 16; services image build fails on Maven Central 429s | Hard-coded image; `MAVEN_MIRROR_URL` build arg not wired | `aws/docker-compose.yml` | `POSTGRES_IMAGE` (default unchanged), `MAVEN_MIRROR_URL` pass-through; `generate-golden.sh` got `GOLDEN_OUT` / `DALYTRAN_IN` so validation never overwrites the committed goldens |

### Open (not fixed)

| # | Discrepancy | Root cause | Owning module | Suggested fix |
|---|---|---|---|---|
| D6 | INTCALC: account 50 (last TCATBAL account) — COBOL leaves `curr_bal` 1945.87 and cycle totals 1501.75 / -47.88; Java posts the 18.77 interest (1964.64) and resets the cycles | Legacy defect: `CBACT04C` only calls `1050-UPDATE-ACCOUNT` on an account break; the end-of-file branch is never reached after the last `READ`, so the last account is never rewritten | `aws/batch` `calculate-interest` (intentional) | Keep the Java behaviour (interest transaction is written by COBOL too, so COBOL is internally inconsistent); obtain business sign-off. Asserted explicitly in `test_intcalc_account_balances` and `GoldenPosttranIntcalcIT` |
| D7 | Bill-payment success: COBOL text has two spaces ("Payment successful.  Your Transaction ID is …") and clears account/balance fields; React keeps the account and shows 0.00, text has one space | `COBIL00C` `STRING 'Payment successful. ' ' Your Transaction ID is '`; UI design choice | `aws/services` `BillPaymentService`, `aws/frontend` bill-payment page | Cosmetic; accept, or clear the form after success if exact screen behaviour is required |
| D8 | POSTTRAN reject 101 `ACCOUNT RECORD NOT FOUND` cannot occur | `card_xref.acct_id` has an FK to `account`; the VSAM files had no such constraint | `aws/db/schema.sql` (by design) | None; orphan xrefs are rejected at ETL time instead. Java code path kept |
| D9 | Sample DALYTRAN only produces reason 102 | Data | `app/data` (read-only) | Covered by the synthetic scenario (100, 103, 102+103) |
| D10 | `aws/services` still ships its own Flyway schema (`V1__carddemo_online_schema.sql`, used only by the `local` profile / service tests) | Parallel sessions; canonical schema landed later | `aws/services` | Load `aws/db/schema.sql` in the local profile and tests (as done for batch in D4) to remove drift risk |
| D11 | Every seeded account fails at least one `COACTUPC` edit (e.g. account 1 FICO 274) | Legacy sample data; identical COBOL behaviour | data | None (users must correct the field, exactly as on the 3270 screen) |
| D12 | EBCDIC vs ASCII sample content differs (account 49 ZIP, `DEFAULT/07/0001` rate) | Source samples differ at byte level | `app/data` | Online DB uses EBCDIC, batch goldens use ASCII (the COBOL ground truth input); no job in the chain reads that disclosure row for the sample balances |

## 6. Coverage gaps

* The original CICS/BMS programs were not executed (no CICS runtime); online expectations are derived from the
  COBOL source (messages, edit order, file updates). Only batch has executable COBOL ground truth.
* UI: multi-page transaction paging not exercised in the browser (≤ 2 rows); covered at API level.
* Not in the validation suite (covered only by module tests): transaction-type admin (`COTRTLIC`/`COTRTUPC`,
  `/api/v1/transaction-types`), `COACCT01`/`CODATE01` SQS request/reply, `TRANREPT`, transaction-type
  maintenance/extract, reference-data backups, `statement-pdf`.
* AWS-only paths not executed: CDK stacks (synth/unit tests only in `aws/infra`), Step Functions ASL, AWS Batch
  job definitions, real SQS/S3 (LocalStack only), Cognito/ALB/CloudFront routing.
* No load, concurrency (beyond optimistic-version checks) or failover testing.
* Statements (`CREASTMT`) and `COMBTRAN` are smoke-tested, not compared with a COBOL run (`CBSTM03A/B` need
  the `TRNXFILE` sort step and the REXX `TXT2PDF` is not in the repo).

## 7. Replatform candidates and incomplete modules (consolidated)

From `aws/migration-inventory.md` §9 and the module READMEs; none were faked.

| Module | Decision | Reason |
|---|---|---|
| IMS/DB2/MQ authorization sub-app: `COPAUA0C`, `COPAUS0C`, `COPAUS1C`, `COPAUS2C`, `CBPAUP0C`, `PAUDBUNL`, `PAUDBLOD`, `DBUNLDGS`, DB `DBPAUTP0`/`DBPAUTX0`, DB2 `AUTHFRDS` | **Replatform candidate** (AWS Mainframe Modernization runtime). Data is migrated (`pending_auth_*`, 21/202 rows); no Java/REST/UI | Hierarchical DL/I navigation, COMP-3 keys, BMP checkpoint/restart, combined MQ+IMS+DB2 unit of work |
| Frontend screens `COPAU00`/`COPAU01`, `COTRTLI`/`COTRTUP` | Not built (menu shows "not installed") | Follow the backing module decisions |
| `CBACT04C` `1400-COMPUTE-FEES` | Incomplete in the COBOL source | "To be implemented" in the original |
| On-demand batch `CBACT01C`–`03C`, `CBCUS01C`, `CBEXPORT`, `CBIMPORT`, `PRTCATBL` | Left incomplete (outside the scheduled cycle) | Low risk, refactor later |
| `COCRDSEC` (CSD `CDV1`) | Not migrated | Source not in repository |
| Assembler `COBDATFT`, `MVSWAIT`/`COBSWAIT`; REXX `TXT2PDF` | Re-implemented with `java.time`, Step Functions `Wait`, PDFBox | Utility code; REXX source absent |

# Prior modernization work: assessment and reuse plan

Ticket UNT149-2. Assessed on 2026-11-02 against `origin/main` @ `59cc6c2` after `git fetch --all --prune`;
`git branch -r | grep origin/devin/` lists **153 branches**. Every one is covered below (grouped by family where
branches share a lineage). The COBOL under `app/` was not modified.

**Decision already taken (human):** the Java application continues
`devin/1789660764-carddemo-java-modernization` (five Spring Boot services under `modernization/`). It is not
started fresh. Everything else is judged by what it can contribute to that branch.

Legend for "Builds": **verified** = run in this assessment (command and result given); **not run** = not executed
here, documentation claims only. Legend for "vs COBOL": what evidence exists that the code matches the COBOL
programs, and whether that evidence was re-executed here.

Environment used: Ubuntu VM, Maven 3.9, JDK 21 at `/usr/lib/jvm/java-21-openjdk-amd64`, JDK 25 at
`/usr/lib/jvm/jdk-25.0.5+8`, GnuCOBOL (`cobc`) 3.1.2, Node 20.20 / npm 10.8, Docker Compose v2, `faketime`
(installed via apt for the posting equivalence run). Each branch was checked out in its own `git worktree`.

---

## 1. The foundation: `devin/1789660764-carddemo-java-modernization`

**Contents.** 5 commits ahead of main (last 2026-09-17), 128 files, ~6.3k lines, all under `modernization/`:

| Module | Port | Replaces | Tests (verified) |
|---|---|---|---|
| `common` | – | shared JPA/web/error plumbing | 0 |
| `auth-service` | 8080 | CSUSR01Y, COSGN00C, COUSR00C-03C | `AuthControllerTest` 4 |
| `customer-service` | 8081 | CVCUS01Y | `CustomerControllerTest` 2 |
| `account-service` | 8082 | CVACT01Y, COACTVWC, COACTUPC, account side of CBTRN02C/CBACT04C | `AccountControllerTest` 6, `AccountPostingTest` 6 |
| `card-service` | 8083 | CVACT02Y/03Y, COCRDLIC/SLC/UPC | `CardControllerTest` 7 |
| `transaction-service` | 8084 | CVTRA01Y-06Y, COTRN00C/01C/02C, COBIL00C, CBTRN02C, CBACT04C (Spring Batch jobs launched over HTTP) | `TransactionControllerTest` 8, `PostingServiceTest` 4, `InterestCalculationServiceTest` 4 |

Plus `docs/API-CONTRACT.md`, `docs/COBOL-PROGRAM-MAPPING.md`, `docs/COPYBOOK-TO-SCHEMA-MAPPING.md`, a shared
`Dockerfile` (multi-stage, `MODULE` build arg, optional `MAVEN_MIRROR_URL`), `docker-compose.yml` (PostgreSQL 16 +
five services), Flyway migrations per service. Commits after the initial rewrite are all bug fixes (executable jars +
Maven mirror; honour posting rejections / column types / pagination edges; Spring Batch metadata tables via Flyway;
zero-interest cycle totals and card list filter intersection).

**Builds: verified.** `cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify` →
`BUILD SUCCESS` in 38.9 s, all 6 modules SUCCESS, **41 tests, 0 failures, 0 errors** (full output in the PR).

**Starts: verified.** `docker compose up -d --build` needed two environment-specific adjustments on this VM, neither a
code change: (1) the Docker build network cannot reach Maven Central, so the first attempt failed at `Dockerfile:19`
(`Non-resolvable parent POM ... spring-boot-starter-parent:pom:3.3.4 ... Could not transfer artifact`); rerunning with
`MAVEN_MIRROR_URL=https://maven-central.storage-download.googleapis.com/maven2/` (the README-documented knob) built all
five images. (2) Host port 8084 is held by an unrelated process (`mono`, pid 812) on this VM, so `transaction-service`
was published on host 8094 via a compose override file (`ports: !override` → `"8094:8084"`). After that, all five
`Started ...Application` lines appeared (customer/card/account/auth in ~7.5 s, transaction in 4.9 s) and
`/actuator/health` returned `{"status":"UP"}` on 8080, 8081, 8082, 8083 and 8094 (→ container 8084). Exact commands
and output are in the PR description.

**What the tests cover.** Controller slices (request/response mapping, validation, pagination) and the two batch
services with stubbed gateways. The README is accurate: tests run on in-memory **H2 with Flyway disabled** and Hibernate
DDL; the Flyway SQL and the PostgreSQL types are therefore only exercised by `docker compose up`. There is **no
comparison against COBOL output** anywhere on this branch; parity is asserted by code reading only.

**Known gaps (README, confirmed by reading the code).**
- Double-posting on retry: posting calls account-service over HTTP, then writes locally; if the local write fails after
  the remote commit, a retry posts twice (no idempotency key / outbox).
- `TRAN-ID` is 16 random base-36 characters, not the legacy counter from the transaction file.
- Not built (stubs/omissions): reports (CORPT00C, CBTRN03C), statements (CBSTM03A/B), menus (COMEN01C/COADM01C),
  the MQ replacement, any UI. Sign-on verifies a BCrypt hash but issues no token, so every endpoint is open and
  there is no role check (admin vs user) anywhere.
- Plain HTTP between services; local default DB credentials; batch jobs are launched by HTTP `POST` on
  transaction-service rather than by a scheduler.
- Batch I/O is database-to-database; nothing reads or writes the fixed-width files (`DALYTRAN`, `TRANREPT`, statements),
  so the existing golden-set harness cannot be pointed at it without an export/import adapter.

**Recommendation: continue (decided).** Open the next tickets against this branch for: idempotent posting (outbox or
idempotency key), a `TRAN-ID` strategy decision, sign-on token + role enforcement, menus/reports/statements, and a
fixed-width import/export adapter so the golden-set harness below can gate it.

---

## 2. What to cherry-pick into the foundation (ranked)

| # | From branch | What | Why / how | Verified here |
|---|---|---|---|---|
| 1 | `1789609454-golden-set-harness` | `tests/golden/` (generate.py, compare.py, mutate.py, run_reference.sh, selftest.sh, layouts.py, GSIDXUTL.cbl, named + volume sets) and `docs/validation/golden-set/` | The parity gate for posting. Pure python3 + GnuCOBOL, no Java dependency; runs the **unmodified** `app/cbl/CBTRN02C.cbl` and compares TRANSACT/DALYREJS/ACCTFILE/TCATBALF field by field plus RETURN-CODE. Lands cleanly (no overlap with `modernization/`). Needs a fixed-width adapter on transaction-service to consume it. **Gotcha:** `run_reference.sh` dumps the whole process environment into `sets/*/expected/env.txt`; strip that before any CI use. | `selftest.sh`: 45/45 checks, 18/18 injected defects caught. `run_reference.sh --set named`: EXIT 0, regenerated outputs byte-identical to the committed set (only `env.txt`/`manifest.json` metadata changed). |
| 2 | `1788437007-posttran-modernization` | `scripts/run-equivalence.sh` + `tests/golden/equivalence_table.py`; `docs/modernization/open-questions.md` (23 OQs), `strangler-cutover.md`, `03-data-model.sql`; the fixed-width record I/O module `carddemo-recordio` | The only branch that **proved a Java port equals the COBOL output**. Reuse the equivalence runner and the open-questions list as the acceptance checklist for transaction-service posting; `carddemo-recordio` is the fixed-width adapter the foundation lacks. The services themselves (Spring Batch, file based) are a parallel design, do not merge them. | `mvn -B verify`: 84 tests pass. `run-equivalence.sh --set named --skip-reference`: **OVERALL PASS** – TRANSACT 14/14, DALYREJS 8/8, ACCTFILE 17/17, TCATBALF 13/13 records byte-identical, RETURN-CODE 4 = 4. |
| 3 | `1786708098-cobol-test-foundation` | `tests/run_tests.sh`, `tests/cobol|cpy|jcl|stubs|fixtures`, `docs/TESTING.md` | Off-mainframe COBOL unit tests for CSUTLDTC, CSUTLDPY and CBSTM03B. Lands cleanly. Gives the migration a way to pin down COBOL utility behaviour (date validation, statement file access) before porting it; coverage needs GnuCOBOL ≥ 3.2 (host has 3.1.2, tests still run). | `bash tests/run_tests.sh`: 15/15 pass. |
| 4 | `1787758095-java-interest-port` | `CobolDecimal` (fixed-point, truncating, wrapping), `Db2Timestamp`, and the documented fidelity choices | Drop-in for the interest calculation in transaction-service (`InterestCalculationService`), which today uses `BigDecimal` without the COBOL truncation/overflow semantics. Plain Java 17, no framework. | `mvn -B verify`: 22 tests pass. Parity by source reading only (no golden run). |
| 5 | `unt51-26-hardening` lineage | `docs/validation/` COBOL baseline (164 files from `unt51-4-cobol-baseline`), `scripts/baseline/`, `scripts/golden-set/`, ArchUnit rules, the React `carddemo-ui` | Largest body of behaviour capture (all batch + online programs). Take the baseline docs/scripts and selectively the UI; the app itself is a single modular monolith that conflicts with the five-service layout. | `mvn -B verify` on `modernization/`: 1,258 tests pass (161 s). Baseline scripts not re-run. |
| 6 | `sfind-*` (40 branches) | The list of authorization rules they add to the COBOL (sign-on required, admin-only user admin/menus, account/card ownership, generic sign-on errors + lockout, no default seed credentials, password hashing, bill-pay dup-TRAN-ID rollback, export without CVV) | Use as the authz requirement checklist for auth-service and the ownership filters in account/card/transaction services. Do **not** merge the COBOL changes into `app/` (reference must stay untouched). | COBOL changes not compiled here. |
| 7 | `1789602209-estate-discovery-dossier` / `UNT17-1-estate-inventory`, `unt51-1/2/3`, `UNT17-6-target-adr`, `docs/specs` branches | Inventory, dependency map, prior-work audit (as of 2026-10-07), target ADR, business specs for INTCALC/POSTTRAN/TRANREPT/CREASTMT | Docs only; land under `docs/migration/` as reference. Specs for TRANREPT/CREASTMT are the starting point for the missing reports/statements. | n/a |

---

## 3. Branch-by-branch

### 3.1 Named branches

| Branch | Contents | Builds | vs COBOL | Reusable | Recommendation |
|---|---|---|---|---|---|
| `1789660764-carddemo-java-modernization` | see §1 | verified ✅ 41 tests; compose ✅ 5/5 UP | none | everything | **Continue** (decided) |
| `1789609454-golden-set-harness` | 6 commits, 80 files: `tests/golden/`, `docs/validation/golden-set/` | verified ✅ selftest 45/45 | runs unmodified CBTRN02C under GnuCOBOL; reference reproduced byte-identical here. Explicitly does not prove z/OS Enterprise COBOL, EBCDIC collation, overflow or abend behaviour (`what-this-does-not-prove.md`) | all | **Cherry-pick whole** (#1) |
| `1786708098-cobol-test-foundation` | 1 commit, 12 files: GnuCOBOL unit tests + gcov runner, zUnit-style JCL | verified ✅ 15/15 | tests the COBOL itself | all | **Cherry-pick whole** (#3) |
| `1787758095-java-interest-port` | 1 commit, 15 files: `java/interest-calc`, plain Java 17 port of CBACT04C | verified ✅ 22 tests | source reading; documents truncation, overflow wrap, DEFAULT group fallback, unreachable last-account ELSE | `CobolDecimal`, `Db2Timestamp`, fidelity notes | **Cherry-pick classes** (#4); drop the module |
| `1788437007-posttran-modernization` | 17 commits, 174 files: `modernization/posttran-cycle` (recordio, posting, interest, report services on Spring Batch), golden set, `docs/modernization/01-06`, open questions, strangler plan | verified ✅ 84 tests | **verified ✅ OVERALL PASS** on named set (byte-identical, RC 4) | equivalence runner, recordio, docs | **Cherry-pick runner/docs/recordio** (#2); do not merge services (directory `modernization/` conflicts) |

### 3.2 TypeScript branches (two generations; a non-`devin/` `origin/migration/ts` also exists)

September generation, npm workspaces under `ts/packages` (`@carddemo/copybook`, `domain`, `vsam`; each feature
branch adds packages). Verified on `1789654793-ts-foundation`: `npm ci && npm test` fails (`Failed to resolve entry
for package "@carddemo/copybook"` – workspace packages must be built first); `npm run build && npm test` → 33 tests
pass. The feature branches were not run.

| Branch | Contents | Builds | vs COBOL | Recommendation |
|---|---|---|---|---|
| `1789654793-ts-foundation` | codecs, record layouts, KSDS/sequential files | verified ✅ (after build) | unit tests only | Discard (language decision is Java) |
| `1789672811-ts-batch-readers`, `1789672812-ts-reports`, `1789672817-ts-posting`, `1789672818-ts-interest`, `1789672889-ts-utilities` | batch program ports on the Sept foundation | not run | unit tests only | Discard; reports/interest tests are a readable spec if useful |
| `1789673709-ts-online-accounts`, `1789673713-ts-online-cards`, `1789673716-ts-online-transactions`, `1789673719-ts-online-usradmin`, `1789673719-ts-signon-menus`, `1789673718-ts-terminal-ui` | online program ports + terminal UI | not run | unit tests only | Discard |
| `1789674277-ts-ci` | CI for the above | not run | – | Discard |

October generation, single `ts/` package (`src/common` codecs/VSAM/CICS-file, `batch/<program>` for all 13 batch
programs, online framework, `typescript` 6 / `vitest` 5), linear history ending in `1792679661-s7-header-fix`.

| Branch | Contents | Builds | vs COBOL | Recommendation |
|---|---|---|---|---|
| `1792676823-ts-foundation`, `1792677655-ts-foundation-review-fixes` | foundation | verified ✅ 84 tests | unit tests only | Discard |
| `1792677667-batch-trn-posting`, `1792677668-batch-acct-cust`, `1792677671-cbstm03`, `1792677672-cbexport-cbimport` | batch ports | not run | unit tests | Discard |
| `1792677672-ts-online-framework`, `1792678235-s7-account-card`, `1792678302-ts-s8-transactions`, `1792678236-ts-s9-user-admin`, `1792678329-ts-indexedfile-declared-altkeys`, `1792678412-ts-online-review-fixes`, `1792679661-s7-header-fix` | online ports, alt-key indexed files, review fixes | `1792678412` verified ✅ 292 tests; others not run | unit tests | Discard |
| `1790172194-posttran-ts-codec`, `1790172336-posttran-ts-file-adapters`, `1790172388-posttran-ts-validation`, `1790172422-posttran-ts-posting-job` (`app/ts`), `1790172292-intcalc-typescript-migration` (`typescript/intcalc`) | earlier TS slices of POSTTRAN / INTCALC | not run | unit tests | Discard |

### 3.3 Other Java implementations (parallel designs; none merge cleanly into the foundation)

| Branch | Contents | Builds | vs COBOL | Reusable | Recommendation |
|---|---|---|---|---|---|
| `1785783419-cbact04c-java-migration` | `modernized/interest-calculator` (Aug 2026) + GH workflow | verified ✅ 30 tests | source reading | superseded by interest-port | Discard |
| `1786306028-interest-sliver` | `modernization/interest-service`, SLIVER-PLAYBOOK, PARITY-REPORT, CBACT04C logic map | verified ✅ 24 tests | documented parity report (not re-run) | logic map doc | Discard code; keep logic map |
| `1786307670-posting-sliver` | interest + posting services + mainframe-io (pre-posttran-cycle) | verified ✅ 36 tests | – | superseded by posttran-cycle | Discard |
| `1788805718-cbact03c-java-poc` | `java-poc` CBACT03C POC | verified ✅ 21 tests | – | – | Discard |
| `1791777710-zero-trust-controls` | posttran-modernization + `modernization/zero-trust` (authn/authz/audit wrapper around posting) | posttran-cycle ✅ 84 tests; `modernization/zero-trust` ❌ – standalone: `Could not find artifact com.carddemo.posttran:transaction-posting-service:jar:0.1.0-SNAPSHOT`; after `mvn install` of posttran-cycle: `COMPILATION ERROR ... PostingAuditBuffer.java:[3,35] package com.carddemo.posting.domain does not exist` (full output in the PR) | inherits posttran golden set | design notes for securing the batch endpoint | Defer |
| `1791991151-java-cosgn00c`, `1791992035-java-comen01c`, `1791992038-java-coactupc`, `1791992044-java-coactupc`, `1791992039-java-cocrdlic`, `1791992049-java-cocrdlic`, `1791992042-java-coactvwc`, `1791992107-java-coactvwc`, `1791992044-java-cocrdslc`, `1791992045-java-cocrdslc` | `java/` Java 25 / Spring Boot 4.1 "CICS runtime" (PicX, CobolNumeric, CicsContext, BmsParser, KsdsFile) + one online program each, `POST /api/cics/<tran>` | ❌ with JDK 21 (`error: release version 25 not supported`, all 10); ✅ with JDK 25: cosgn00c 95 tests, coactvwc 149 tests (others not run) | BMS-map-driven screen emulation, no COBOL run | `BmsParser` (reads `app/bms`) could seed menu/screen field validation | Discard as implementation (Java 25 + CICS emulation is the opposite of the foundation's design) |
| `1792002963-cbtrn02c-java-posttran` | `java/posttran` Java 25 / Spring Batch 6 port of CBTRN02C with `GoldenMasterTest` against committed ASCII + EBCDIC COBOL outputs, exit codes 0/4/8/12 | ❌ JDK 21 (release 25); ✅ JDK 25: 38 tests | golden master tests vs committed COBOL outputs (not regenerated here) | EBCDIC golden fixtures, exit-code mapping | Discard code; copy EBCDIC fixtures into the golden set |
| `1793120148-transaction-java-migration` | `app/app-java` Spring Boot 3: CBTRN01C/02C jobs + COTRN00C/01C/02C REST, H2 file, seed loader (based on `cf9d9d3`, not current main) | verified ✅ 43 tests | – | – | Discard (duplicates transaction-service) |
| `1792074256-carddemo-cloud-native-modernization` | `modernization/` with identity(8081)/account/card/transaction/batch(8085)/data-migration services, JWT | verified ✅ 95 tests | – | JWT identity-service pattern; `data-migration` loader of `app/data/ASCII` | Same directory, different layout → **conflicts**; port the JWT + loader ideas by hand, discard the rest |
| `cd-103-classified-evidence` (tip), `cd-104-golden-master`, `cd-105-posting-reject-handling`, `cd-106-copybook-records-codecs`, `cd-107-business-rules-catalog`, `cd-108-spring-batch-job`, `cd-109-interest-calculation` | `modernization/java21` plain Java 21 + Spring Batch: codecs (zoned DISPLAY semantics, negative zero), records, posting, interest, evidence/golden master | cd-103 ✅ 94 tests, cd-105 ✅ 11, cd-106 ✅ 48, cd-108 ✅, cd-109 ✅ 18; cd-104/cd-107 not run | golden master docs (not re-run) | `cd-107` business-rules catalog; codec edge-case tests | Discard code; keep rules catalog |
| `cbact01c-java17`, `cbtrn01c-java17`, `cobol-safety-net` | `java/carddemo-batch` Java 17 ports of CBACT01C/CBTRN01C, `golden-files/`, `test-harness/` (python reconcile + COBOL) | cbtrn01c ✅ 118 tests, cbact01c ✅ 77 tests; safety-net has no Java | golden files generated from COBOL (not regenerated here) | `golden-files/CBACT01C|CBTRN01C` as additional golden sets | Discard code; copy golden files |
| `unt51-5-java21-scaffold` … `unt51-26-hardening` (20 branches, linear) | `modernization/carddemo-app` modular monolith + React UI + full docs/validation | tip ✅ 1,258 tests | COBOL baseline + golden-set scripts (not re-run) | see §2 #5 | Harvest docs/scripts/UI; discard app |
| `1793293556-transform-specs-to-code`, `1793297073-authfrds-specs-to-java` | `modernization/transaction-type-service` (Db2) and `modernization/authfrds-service` for the `app-transaction-type-db2` / `app-authorization-ims-db2-mq` extensions, from AWS Transform specs | verified ✅ 63 / 48 tests | – | – | Out of scope (extensions); do not merge into `modernization/` now |
| `1792618061-cbact01c-csharp` | `dotnet/CBACT01C` | not run | – | – | Discard |

### 3.4 AWS target architecture (`aws/`, Sept 2026, linear)

`1790616097-discovery-contracts` → `1790617842-data-migration` → `1790617863-batch` / `1790617863-online-services` →
`1790617957-infra-cdk` → `1790618699-frontend-react` → `1790619634-validation-parity` (tip, 350 files). Verified on
the tip: `aws/batch` ✅ 13 tests, `aws/services` ✅ 67 tests (CDK and React not built). A full parallel target
(ECS services, AWS Batch, Aurora, CDK, React) with no COBOL execution evidence. **Defer**: reuse `aws/contracts` and
`aws/db/schema.sql` as input to the schema/API design; do not merge.

### 3.5 Docs / specs only

| Branch | Contents | Recommendation |
|---|---|---|
| `1782511099-webinar-demo-script` | demo script | Ignore |
| `1782840346-five-phase-migration-prompts` | prompt sequence | Ignore |
| `1789584353-modernization-plan` | `MODERNIZATION_PLAN.md` | Reference only |
| `1789602209-estate-discovery-dossier`, `UNT17-1-estate-inventory` | `docs/discovery` dossier + `tests/test_discovery.py` | Land under `docs/migration/` |
| `UNT17-6-target-adr` | target architecture ADR | Reconcile with the five-service decision, then land |
| `1790171260-intcalc-business-spec`, `1790171286-posttran-business-spec`, `1790172392-tranrept-spec`, `1790172399-creastmt-business-spec` | business specs | Land; TRANREPT/CREASTMT are the spec for the missing reports/statements |
| `1791777911-financial-feeder-readout` | discovery readout + wiki.json | Reference only |
| `unt51-1-mainframe-inventory`, `unt51-2-dependency-map`, `unt51-3-prior-work-audit`, `unt51-4-cobol-baseline` | inventory, dependency map (+ generator scripts), earlier prior-work audit, COBOL behaviour baseline | Land under `docs/migration/` (this document supersedes `00-prior-work.md`) |

### 3.6 COBOL-side changes (do not merge into `app/`; reference must stay untouched)

| Branch | Contents | Recommendation |
|---|---|---|
| `1789604381-daily-feed-validation` | new `app/cbl/CBTRN04C.cbl`, `DALYRJ04/POSTTRN2/VALDTRAN.jcl`, `tests/cbtrn04c`, `docs/sustainment` | Defer; extends the legacy app beyond main's scope |
| `1791778009-ledger-summary-interface` | new `app/cbl/CBGLX01C.cbl`, `CVGL*` copybooks, `GLEXTRCT.jcl`, interface doc | Defer; same |
| `1789926524-cbimport-cardout-dd` | one JCL DD fix | Defer (legacy fix, not modernization) |
| `sfind-000228cb-cotrn00c-account-scope`, `sfind-03fdf5da-caup-require-signon`, `sfind-0b0f68fe-cbexport-no-cvv-encrypted-export`, `sfind-148211bf-cbimport-validation`, `sfind-1d9e349f-admin-role-check`, `sfind-1eece5df-acctview-ownership-check`, `sfind-1f50517c-cousr02c-no-password-echo`, `sfind-1fa7b370-cotrn02c-admin-only`, `sfind-28e00a2a-cotrn01c-ownership-check`, `sfind-2ccc2b08-cocrdupc-require-signon`, `sfind-48aaedc3-copaus1c-fraud-signon-role`, `sfind-527eb919-copaua0c-validate-amount`, `sfind-6230900d-bill-pay-account-ownership`, `sfind-6d544ad5-no-default-seed-password`, `sfind-70384d81-copaus-require-signon`, `sfind-779fd116-posttran-validation`, `sfind-7eb4245b-usrsec-no-default-creds`, `sfind-8056c74c-copaua0c-reset-reply-state`, `sfind-871aacc2-ftpjcl-netrc`, `sfind-90d3ee4d-ccli-require-signon`, `sfind-91e23d59-cttu-require-admin-signon`, `sfind-9c7a5183-card-list-owner-filter`, `sfind-aaa388b9-cavw-require-signon`, `sfind-ae64636b-coacct01-validate-mq-key`, `sfind-b282c511-signon-generic-msg-lockout`, `sfind-b9f40df4-online-signon-guard`, `sfind-c0924757-cousr-admin-gate`, `sfind-c66c0a2a-ctli-require-admin-signon`, `sfind-cb2b020d-copaua0c-card-acct-fraud-checks`, `sfind-d36bb6a2-coactupc-admin-only`, `sfind-d3bb3b53-copaua0c-truncated-mq-msg`, `sfind-d4ee6bc5-admin-menu-role-check`, `sfind-e1ca71e2-coacct01-mq-authz`, `sfind-e7d2f0af-cocrdupc-card-acct-binding`, `sfind-ebd14132-billpay-dup-tranid-rollback`, `sfind-ef3fbe14-user-admin-require-admin`, `sfind-f9cfa18e-cousr03c-admin-check`, `sfind-ffb600fb-usrsec-password-hash` | 40 security-finding fixes to the COBOL (2026-10-30), 1–15 files each | Do not merge; mine as the authz checklist (§2 #6). The `copau*`/`coacct01` ones target the out-of-scope extensions |

---

## 4. Not verified in this assessment

- Not built: Sept/Oct TS feature branches other than the three listed; `cd-104`, `cd-107`; `java-*` branches
  other than cosgn00c/coactvwc under JDK 25; `aws/infra` (CDK), `aws/frontend`, `carddemo-ui` React; `dotnet/CBACT01C`;
  intermediate `unt51-*` commits (tip built).
- Not re-executed: posttran **volume** set (named only); unt51 and cd-* golden-master scripts; cbtrn02c-java-posttran
  `GoldenMasterTest` against freshly generated COBOL output (its tests ran against committed fixtures); sfind COBOL fixes.
- Not run anywhere: any Java port against **z/OS Enterprise COBOL** output – every parity claim in this repo is against
  GnuCOBOL 3.1.2 on Linux with synthetic data.

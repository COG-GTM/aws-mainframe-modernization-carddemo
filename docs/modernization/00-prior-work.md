# 00 — Prior modernization work on `origin/devin/*`: audit for reuse

Step `s1.3` of the CardDemo COBOL/CICS → Java 21 plan. Audited 2026-10-07 after `git fetch --all`
on `COG-GTM/aws-mainframe-modernization-carddemo`; 51 `origin/devin/*` branches exist (49 prior-work
branches + the two UNT51 stack branches). Decisions taken as given and **not** reopened: `d-scope`
core app only, `d-reuse` fresh build on `main` under `modernization/` + harvest, `d-decomp` modular
monolith (one Spring Boot app, domain modules), `d-scheduler` Spring Batch flow job + in-app cron,
`d-ui` web UI (one page per BMS map), `d-verify` golden set vs GnuCOBOL baseline in CI.

How each branch was assessed:

* contents: `git diff --stat main...origin/devin/<branch>` plus its README/docs;
* Java branches: a worktree per branch and `JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B -q verify`
  (Maven 3.6.3; test counts are the sum of `surefire-reports`/`failsafe-reports` XML, which is why they
  sometimes exceed the single `Tests run:` line Maven prints for multi-module reactors);
* non-Java harnesses: the branch's own self-test / pytest / `npm test` command, as noted;
* fit: against the modular-monolith Spring Boot 3.x / PostgreSQL / Spring Batch / React+Vite target.

Verdicts: **keep** = use as-is as the base of future work; **harvest** = do not merge, copy the named
artifacts (with the SHA) into the fresh build in a later step; **ignore** = nothing to carry forward.
"Last commit" is the committer date of the branch head. Nothing was copied into `modernization/` by this
step.

## 1. Summary table (all 51 branches)

| # | Branch (`origin/devin/…`) | Head | Last commit | What it is | JDK 21 build / test (actual) | CardDemo coverage | Verdict |
|---|---|---|---|---|---|---|---|
| 1 | `1782511099-webinar-demo-script` | `2ab4e99` | 2026-06-26 | `demo/WEBINAR_DEMO_SCRIPT.md` — runbook for a COBOL→**Python** webinar | n/a (docs) | none | **ignore** |
| 2 | `1782840346-five-phase-migration-prompts` | `a9a1878` | 2026-06-30 | `migration/five-phase-migration-prompts.md` — prompt pack for a COBOL→**Python** migration | n/a (docs) | none | **ignore** |
| 3 | `1785783419-cbact04c-java-migration` | `2d5b9c4` | 2026-08-03 | `modernized/interest-calculator/` — Java 21 / Spring Boot 3.5.4 / Spring Batch port of CBACT04C with GnuCOBOL oracle + golden-master harness | `BUILD SUCCESS`, 30 tests, 0 F/E/S | CBACT04C (INTCALC) only | **harvest** (oracle scripts) |
| 4 | `1786306028-interest-sliver` | `c92cf02` | 2026-08-12 | `modernization/interest-service/` — plain Java 21 (no Spring) CBACT04C sliver + parity harness, logic map, playbook | `BUILD SUCCESS`, 24 tests, 0 F/E/S | CBACT04C only | **ignore** (superseded by #5) |
| 5 | `1786307670-posting-sliver` | `4affd4c` | 2026-09-03 | `modernization/{carddemo-mainframe-io,interest-service,transaction-posting-service}` — plain Java 21 slivers for CBACT04C + CBTRN02C, logic maps, parity reports | `BUILD SUCCESS`, 36 tests, 0 F/E/S | CBACT04C, CBTRN02C | **harvest** (logic maps, playbook) |
| 6 | `1786708098-cobol-test-foundation` | `54f4eee` | 2026-08-14 | `tests/` — GnuCOBOL unit-test foundation (`run_tests.sh`, assertion copybook, CEEDAYS stub, gcov coverage) for CSUTLDTC/CBSTM03B | GnuCOBOL, not Java: `bash tests/run_tests.sh` → `TESTS RUN....: 0015`, `TESTS PASSED.: 0015`, `RESULT: all test suites passed` | CSUTLDTC, CSUTLDWY-style date edits, CBSTM03B | **harvest** (CEEDAYS stub pattern for UNT51-4) |
| 7 | `1787758095-java-interest-port` | `b661c5d` | 2026-08-26 | `java/interest-calc/` — plain Java **17** CBACT04C port with fixed-point arithmetic | `BUILD SUCCESS` (compiles `--release 17` under JDK 21), 22 tests, 0 F/E/S | CBACT04C only | **ignore** (superseded by #3, #5, #8) |
| 8 | `1788437007-posttran-modernization` | `0391252` | 2026-09-16 | `modernization/posttran-cycle/` — `carddemo-recordio` codec + 3 Spring Boot 3.3.4 / Spring Batch services (INTCALC, POSTTRAN, TRANREPT) + `docs/modernization/01…05`, call graph | `BUILD SUCCESS`, 81 tests / 15 suites, 0 F/E/S | CBACT04C, CBTRN02C, CBTRN03C + their 7 record layouts | **harvest** (recordio, module docs) |
| 9 | `1788805718-cbact03c-java-poc` | `22fbe47` | 2026-09-07 | `java-poc/` — byte-identical DISPLAY port of CBACT03C and the read path of CBACT01C | `BUILD SUCCESS`, 21 tests, 0 F/E/S | CBACT03C, CBACT01C (read only) | **ignore** (superseded by #49–50) |
| 10 | `1789584353-modernization-plan` | `e7a43bd` | 2026-09-16 | `MODERNIZATION_PLAN.md` — 1 300-line plan targeting microservices + DynamoDB + AWS Batch | n/a (docs) | estate-wide (narrative) | **harvest** (risk register only) |
| 11 | `1789602209-estate-discovery-dossier` | `4640a65` | 2026-09-17 | `docs/discovery/` — `build_discovery.py` generator, `inventory.json`, 01-inventory … 05-government-decisions, `tests/test_discovery.py` | `python3 docs/discovery/build_discovery.py --check` → `OK: 7 generated files are current (edges resolved 548, unresolved 125; artifacts 237)`; `pytest -q tests/test_discovery.py` → `57 passed` | 237 artifacts, all app/ programs/copybooks/BMS/JCL | **harvest** (construct register, lineage, decisions) |
| 12 | `1789604381-daily-feed-validation` | `6b3fdd0` | 2026-09-17 | **New legacy function**: `app/cbl/CBTRN04C.cbl` (1 938 lines), 3 JCL, 425-file `tests/cbtrn04c/`, `docs/sustainment/` | n/a (COBOL); not run | adds a program that is not in the estate on `main` | **ignore** (out of `d-scope`) |
| 13 | `1789609454-golden-set-harness` | `c068d6f` | 2026-09-17 | `tests/golden/` — GnuCOBOL reference runner for CBTRN02C, dataset generator, field-level reconciliation, mutation self-test; `docs/validation/golden-set/` | `bash tests/golden/selftest.sh` → `checks passed: 45 of 45`, `RESULT: PASS - 18 of 18 injected defects caught` | CBTRN02C (POSTTRAN) end to end, 7 datasets | **harvest** (whole harness + report format) |
| 14 | `1789654793-ts-foundation` | `ab3661b` | 2026-09-17 | TypeScript track: copybook codecs, domain records, VSAM-equivalent stores | not run (TypeScript) | n/a | **ignore** |
| 15 | `1789660764-carddemo-java-modernization` | `2136cd4` | 2026-09-17 | `modernization/` — `common` + 5 Spring Boot 3.3.4 **micro**services (auth, customer, account, card, transaction), JPA, schema-per-service PostgreSQL, docker-compose, program/copybook/API mapping docs | `BUILD SUCCESS`, 41 tests (auth 4, customer 2, account 12, card 7, transaction 16), 0 F/E/S; no DB integration tests | 9 controllers: signon/user admin, customer, account, card + xref, transaction, bill pay, batch trigger (CBTRN02C/CBACT04C ports); no reports/statements/MQ/UI | **harvest** (mapping docs, `common`) |
| 16 | `1789672811-ts-batch-readers` | `9657add` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 17 | `1789672812-ts-reports` | `e4e6ee3` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 18 | `1789672817-ts-posting` | `e9f724d` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 19 | `1789672818-ts-interest` | `3b63770` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 20 | `1789672889-ts-utilities` | `7b85577` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 21 | `1789673709-ts-online-accounts` | `fb5ecff` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 22 | `1789673713-ts-online-cards` | `d35b842` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 23 | `1789673716-ts-online-transactions` | `2e96bed` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 24 | `1789673718-ts-terminal-ui` | `d254597` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 25 | `1789673719-ts-online-usradmin` | `a09d131` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 26 | `1789673719-ts-signon-menus` | `7b82d18` | 2026-09-17 | TypeScript track | not run | n/a | **ignore** |
| 27 | `1789674277-ts-ci` | `d835097` | 2026-09-17 | TypeScript track (CI workflow) | not run | n/a | **ignore** |
| 28 | `1789926524-cbimport-cardout-dd` | `34fbd0e` | 2026-09-20 | 5-line JCL fix: adds the missing `CARDOUT` DD to `app/jcl/CBIMPORT.jcl` | n/a (JCL) | CBIMPORT job | **ignore** as code; **note** the finding (§3.9) |
| 29 | `1790171260-intcalc-business-spec` | `6ac4378` | 2026-09-28 | `docs/specs/INTCALC-CBACT04C-business-spec.md` (434 lines) | n/a (docs) | INTCALC / CBACT04C | **harvest** |
| 30 | `1790171286-posttran-business-spec` | `cbb33f0` | 2026-09-23 | `docs/specs/POSTTRAN-CBTRN02C-business-spec.md` (333 lines) | n/a (docs) | POSTTRAN / CBTRN02C | **harvest** |
| 31 | `1790172194-posttran-ts-codec` | `e31f694` | 2026-09-23 | TypeScript POSTTRAN track (`app/ts/`) | not run | n/a | **ignore** |
| 32 | `1790172292-intcalc-typescript-migration` | `ca54279` | 2026-09-28 | TypeScript INTCALC port (`typescript/intcalc/`) | not run | n/a | **ignore** |
| 33 | `1790172336-posttran-ts-file-adapters` | `f480ed9` | 2026-09-23 | TypeScript POSTTRAN track | not run | n/a | **ignore** |
| 34 | `1790172388-posttran-ts-validation` | `0241cbc` | 2026-09-23 | TypeScript POSTTRAN track | not run | n/a | **ignore** |
| 35 | `1790172392-tranrept-spec` | `b4a0900` | 2026-09-23 | `docs/specs/TRANREPT-CBTRN03C-business-spec.md` (510 lines) | n/a (docs) | TRANREPT / CBTRN03C | **harvest** |
| 36 | `1790172399-creastmt-business-spec` | `410dda3` | 2026-09-23 | `docs/specs/CREASTMT-CBSTM03A-CBSTM03B-business-spec.md` (649 lines) | n/a (docs) | CREASTMT / CBSTM03A + CBSTM03B | **harvest** |
| 37 | `1790172422-posttran-ts-posting-job` | `bf0e22f` | 2026-09-23 | TypeScript POSTTRAN track | not run | n/a | **ignore** |
| 38 | `1790616097-discovery-contracts` | `7c9e714` | 2026-09-28 | `aws/contracts/{api,batch,conventions,data-model,messaging}.md`, `aws/migration-inventory.md` — target contracts for the AWS track (#39–#44) | n/a (docs) | all online transactions, all batch jobs, data model | **harvest** (api, batch, data-model contracts) |
| 39 | `1790617842-data-migration` | `672043f` | 2026-09-28 | `aws/db/schema.sql` (+ `db2/`, `ims/`), `aws/etl/` Python 3.11 copybook-driven EBCDIC→CSV→`COPY` loader, generated seed CSVs | Python, not Java: `pytest -q tests` in a venv with pinned `requirements.txt` → `105 passed, 8 skipped` (skips need a live PostgreSQL) | every VSAM file / copybook that the online+batch app reads | **harvest** (schema.sql, ETL as seed loader) |
| 40 | `1790617863-batch` | `e9658ca` | 2026-09-28 | `aws/batch/` — Java 21 / Spring Boot 3.5.14 / Spring Batch single jar, one job per JCL job, record codec, S3/local object store, GnuCOBOL golden generator, Testcontainers ITs | `BUILD SUCCESS`, 41 tests (24 Surefire + 17 Failsafe ITs with Testcontainers PostgreSQL), 0 F/E/S | POSTTRAN, INTCALC, CREASTMT, COMBTRAN, TRANBKP, TRANREPT, TRANEXTR + reference-data loads (8 job packages) | **harvest (strong)** |
| 41 | `1790617863-online-services` | `5db95c6` | 2026-09-28 | `aws/services/` — Java 21 / Spring Boot 3.3.4 **single** REST service, Spring JDBC `JdbcClient`, JWT, `openapi.yaml`; all online CICS programs incl. COTRTLIC/COTRTUPC and the two MQ consumers (as SQS) | `BUILD SUCCESS`, 67 tests, 0 F/E/S | 17 base online programs + 2 tran-type maintenance + 2 MQ seams | **harvest (strong)** |
| 42 | `1790617957-infra-cdk` | `de0a353` | 2026-09-28 | `aws/infra/` — CDK v2 (TypeScript): VPC, Aurora PostgreSQL, SQS, ECS/Fargate, AWS Batch, Step Functions, EventBridge, CloudFront | not run (CDK) | deployment only | **ignore** |
| 43 | `1790618699-frontend-react` | `5c0cc16` | 2026-09-28 | `aws/frontend/` — React 18 + Vite 7 + TypeScript, 16 screens (one per BMS map), MSW mocks, vitest | `npm ci && npm run build` → `✓ built in 1.17s`; `npm test -- --run` → `Test Files 4 passed (4)`, `Tests 44 passed (44)` | 16 of the 17 base BMS maps (COADM01 admin menu folded into `MenuScreen`) | **harvest (strong)** |
| 44 | `1790619634-validation-parity` | `b60d33f` | 2026-09-28 | Union of #38–#43 under `aws/` + `aws/validation/` (pytest parity suite: 98 online + 13 batch checks vs GnuCOBOL) + `validation-report.md` | `BUILD SUCCESS`: services 67 tests; batch 28 (15 Surefire + 13 Failsafe) — 95 total, 0 F/E/S. Python parity suite not re-run (needs the full stack + LocalStack) | the whole core app, online + batch + UI | **harvest** (parity suite + report format) |
| 45 | `UNT17-1-estate-inventory` | `faa36ab` | 2026-09-30 | Later revision of #11 (`docs/discovery/`, 240 artifacts, `.github/workflows/discovery.yml`) | `build_discovery.py --check` → `OK … artifacts 240`; `pytest -q tests/test_discovery.py` → `67 passed` | estate-wide | **ignore** (superseded by UNT51-1/-2 on the stack base) |
| 46 | `UNT17-6-target-adr` | `8353e72` | 2026-09-30 | `docs/architecture/ADR-001-target.md` — Java 21 / Spring Boot per-domain **services**, PostgreSQL, React+Vite, Spring Batch 5, Artemis, **Airflow** | n/a (docs) | estate-wide | **harvest** (traceability convention, PIC→type rules); reject service split + Airflow |
| 47 | `cobol-safety-net` | `1c845c7` | 2026-10-07 | `TEST_STRATEGY.md`, `golden-files/{CBACT01C,CBTRN01C}`, `test-harness/` (Python copybook parser, `reconcile.py`, GnuCOBOL `build.sh`, CEE3ABD/COBDATFT/KSDS-loader stubs) | Python/COBOL; exercised indirectly by #49's `PythonHarness` tests | CBACT01C, CBTRN01C | **harvest (strong)** for UNT51-4 |
| 48 | `cbact01c-java17` | `c15e719` | 2026-10-07 | `java/carddemo-batch/` — Java 17 port of CBACT01C on top of #47 (open PR #51) | subset of #49 | CBACT01C | **ignore** (superset is #49) |
| 49 | `cbtrn01c-java17` | `02406cb` | 2026-10-07 | #48 + CBTRN01C, codec (`PackedDecimal`, `ZonedDecimal`), `KsdsFile`, `VariableRecordWriter`, `CobDatFt`, parity tests driving #47's Python harness (open PR #52) | `BUILD SUCCESS` (`--release 17` under JDK 21), 118 tests, 0 F/E/S | CBACT01C, CBTRN01C | **harvest** (codec/IO, parity-test pattern) |
| 50 | `unt51-1-mainframe-inventory` | `4e81b4b` | 2026-10-07 | **This stack**, step s1.1 (PR #50): `docs/modernization/{inventory.json,01-inventory.md,build_inventory.py}` | `python3 docs/modernization/build_inventory.py --check` (on base) | estate-wide | **keep** (stack base) |
| 51 | `unt51-2-dependency-map` | `6f334bd` | 2026-10-07 | **This stack**, step s1.2 (PR #53), base of this PR: `02-dependency-map.md`, `dependency-map.json`, `build_dependency_map.py` | n/a (docs + generator) | estate-wide | **keep** (stack base) |

Totals: 2 keep (stack), 20 harvest, 29 ignore (18 TypeScript, 2 Python-era docs, 1 CDK, 8 superseded /
out-of-scope).

## 2. Verification commands actually run

All Java builds: `cd /home/ubuntu/worktrees/<branch>/<module> && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B -q verify`,
followed by summing `tests/failures/errors/skipped` across `**/surefire-reports/*.xml` and
`**/failsafe-reports/*.xml`. Full output is in the PR description; condensed:

| Branch / module | Result |
|---|---|
| #3 `modernized/interest-calculator` | BUILD SUCCESS — 30 / 0 / 0 / 0 |
| #4 `modernization/interest-service` | BUILD SUCCESS — 24 / 0 / 0 / 0 |
| #5 `modernization` (3 modules) | BUILD SUCCESS — 36 / 0 / 0 / 0 |
| #7 `java/interest-calc` | BUILD SUCCESS — 22 / 0 / 0 / 0 |
| #8 `modernization/posttran-cycle` (4 modules) | BUILD SUCCESS — 81 / 0 / 0 / 0 |
| #9 `java-poc` | BUILD SUCCESS — 21 / 0 / 0 / 0 |
| #15 `modernization` (6 modules) | BUILD SUCCESS — 41 / 0 / 0 / 0 |
| #40 `aws/batch` | BUILD SUCCESS — 41 / 0 / 0 / 0 (17 of them Failsafe ITs on Testcontainers PostgreSQL 16) |
| #41 `aws/services` | BUILD SUCCESS — 67 / 0 / 0 / 0 |
| #44 `aws/services` + `aws/batch` | BUILD SUCCESS — 67 + 28 = 95 / 0 / 0 / 0 |
| #49 `java/carddemo-batch` | BUILD SUCCESS — 118 / 0 / 0 / 0 |
| #11 discovery | `--check` OK (548/125 edges, 237 artifacts); pytest `57 passed` |
| #45 discovery | `--check` OK (240 artifacts); pytest `67 passed` |
| #13 golden set | `selftest.sh` → `checks passed: 45 of 45`, `18 of 18 injected defects caught` |
| #39 ETL | pytest `105 passed, 8 skipped` |
| #6 COBOL test foundation | `run_tests.sh` → 15 run, 15 passed, 0 failed |
| #43 frontend | `vite build` OK; vitest `44 passed` |

Not run: the TypeScript track (`ignore`), the CDK app (`ignore`), the COBOL-only test suite of #12,
and the end-to-end Python parity suite of #44 (needs PostgreSQL + LocalStack + both Java apps up; its
committed `validation-report.md` claims 111/111 — treated as a claim, not verified here).

## 3. Per-branch assessment of the harvest candidates

### 3.1 `1788437007-posttran-modernization` @ `0391252` — recordio codec + POSTTRAN cycle

*What it is.* `modernization/posttran-cycle/` with four Maven modules: `carddemo-recordio` (library) and
three Spring Boot 3.3.4 / Spring Batch 5 services for INTCALC (CBACT04C), POSTTRAN (CBTRN02C) and TRANREPT
(CBTRN03C). Plus `docs/modernization/01-inventory.md`, `02-module-{CBACT04C,CBTRN02C,CBTRN03C}.md`,
`03-data-model.{md,sql}`, `04-target-implementation.md`, `05-equivalence-evidence.md`, `call-graph.{mmd,svg}`,
`open-questions.md`.

*Quality.* 81 tests, all green. The codec is small (≈444 lines in `codec/` + `store/`), handles EBCDIC
(`Cp037`) and ASCII, signed zoned decimals with overpunch, implied decimals, and is round-trip tested
against every shipped dataset under `app/data/{ASCII,EBCDIC}` (`ShippedDatasetRoundTripTest`). It does
**not** implement COMP-3/packed decimal (none of the seven layouts it covers need it) and the services'
equivalence is "Level B" by its own `05-equivalence-evidence.md`: Java behaviour vs *documented* COBOL
behaviour, never vs COBOL execution output. Batch jobs use file-based `KeyedRecordStore`s, not a database.

*Fit.* Spring Boot 3 / Spring Batch 5 — yes. Three separate executables, no PostgreSQL — no; in the
monolith they become three `Job` beans in the `batch` module reading from JPA/JDBC repositories.

*Harvest (carry forward, adapt package to `com.carddemo.…` of the monolith):*

| Artifact | Path on branch | Change needed |
|---|---|---|
| Numeric/encoding codec | `modernization/posttran-cycle/carddemo-recordio/src/main/java/com/carddemo/recordio/codec/{CobolNumeric,FixedWidthRecord,RecordEncoding,RecordFormatException}.java` | add COMP-3 (take `PackedDecimal` from #49, see 3.11) |
| Declarative layouts | `…/recordio/layout/{RecordLayout,Account,AccountLayout,CardXref,CardXrefLayout,DisclosureGroup,DisclosureGroupLayout,Transaction,TransactionLayout,TransactionCategory,TransactionCategoryLayout,TransactionCategoryBalance,TransactionCategoryBalanceLayout,TransactionType,TransactionTypeLayout}.java` | keep as the fixed-width *file* view used by the golden harness and ETL; the persistent model is PostgreSQL |
| File/keyed stores | `…/recordio/store/{FixedWidthFile,KeyedRecordStore,DuplicateKeyException,RecordNotFoundException}.java` | use only in tests/golden replay; production reads PostgreSQL |
| Round-trip test | `…/carddemo-recordio/src/test/java/com/carddemo/recordio/ShippedDatasetRoundTripTest.java` | keep verbatim as a codec regression test |
| Module behaviour docs | `docs/modernization/02-module-CBACT04C.md`, `02-module-CBTRN02C.md`, `02-module-CBTRN03C.md`, `03-data-model.sql`, `open-questions.md` | **rename** before copying — the branch's `docs/modernization/01-inventory.md` collides with the UNT51-1 file of the same name |

### 3.2 `1790617863-batch` @ `e9658ca` — AWS Batch track, the most complete Spring Batch implementation

*What it is.* `aws/batch/`: one Spring Boot 3.5.14 / Spring Batch jar, one job per JCL job
(`posttran`, `intcalc`, `creastmt`, `combtran`, `tranbkp`, `tranrept`, `tranextr`, `refdata` packages),
`core/JobRunner` mapping job outcome to mainframe return codes (0/4/8/12) with `runId` idempotency,
`record/` codec (`Zoned`, `Cobol.fit`, `Edited` — the only codec in the estate that implements COBOL
*edited pictures* for report lines, `Fixed`), `storage/` with S3 and local-directory implementations, and
`golden/generate-golden.sh` + `gen-idxutil.sh` + `RUNINTC.cbl` that compile the unmodified COBOL with
GnuCOBOL to produce expected outputs.

*Quality.* 41 tests incl. 17 Failsafe ITs against Testcontainers PostgreSQL: each IT loads reference data,
runs a job, and compares to the GnuCOBOL golden output. The best-tested batch code on any branch. Codec is
ASCII-only (no EBCDIC, no COMP-3) because it reads PostgreSQL rather than VSAM images.

*Fit.* Spring Batch on PostgreSQL — exactly the target; the AWS-Batch "one job per container invocation"
entry point and the S3 store are the only things to drop (replace with the `d-scheduler` flow job + in-app
cron, keep the local `ObjectStore`).

*Harvest:* `aws/batch/src/main/java/com/carddemo/batch/{core,record,storage}/**`, the eight job packages as
the reference implementation for the monolith's `batch` module, `aws/batch/src/test/java/com/carddemo/batch/it/**`
(IT pattern), `aws/batch/golden/{generate-golden.sh,gen-idxutil.sh,RUNINTC.cbl}` (GnuCOBOL compile scripts),
`aws/batch/README.md` job↔JCL table.

### 3.3 `1790617863-online-services` @ `5db95c6` — single Spring Boot online service

*What it is.* `aws/services/`: one Spring Boot 3.3.4 application, Spring JDBC (`JdbcClient`, 27 call sites) rather than
JPA, JWT sign-on, Flyway `db/migration/V1__carddemo_online_schema.sql`, `openapi.yaml`, controllers/services per CICS program (signon,
menus, account view/update, card list/view/update, transaction list/view/add, bill pay, report request,
user list/add/update/delete, tran-type list/update) plus two MQ consumer seams (COPAUA0C/COPAUS* as SQS).

*Quality.* 67 tests (MockMvc slices + service unit tests); error texts follow the COBOL screen messages so the UI can
show the same wording (spot-checked, not exhaustively verified). No DB integration tests on this branch (they live
in #44's Python parity suite).

*Fit.* This is already a monolith for the online side; package layout is by program rather than by domain,
so re-fold into the `d-decomp` domain modules (`account`, `card`, `transaction`, `user`, `security`,
`reporting`). Drop SQS.

*Harvest:* `aws/services/src/main/java/com/carddemo/services/**` as reference, `aws/services/openapi.yaml`
(REST contract per BMS map), `aws/services/src/test/**` (67 cases are a ready acceptance-test list),
`aws/services/README.md` program→endpoint table.

### 3.4 `1790618699-frontend-react` @ `5c0cc16` — React 18 + Vite UI

*What it is.* `aws/frontend/src/screens/*.tsx`: `SignonScreen`, `MenuScreen`, `AccountViewScreen`,
`AccountUpdateScreen`, `CardListScreen`, `CardViewScreen`, `CardUpdateScreen`, `TransactionListScreen`,
`TransactionViewScreen`, `TransactionAddScreen`, `BillPayScreen`, `ReportScreen`, `UserListScreen`,
`UserAddScreen`, `UserUpdateScreen`, `UserDeleteScreen`; typed API client, MSW mocks, vitest.

*Quality.* Builds and 44 tests pass; screens mirror the BMS field set (PF-key semantics mapped to buttons).
Missing: COTRTLI/COTRTUP tran-type maintenance screens, no auth-expiry handling, no accessibility pass.

*Fit.* Matches `d-ui` (React+Vite, one page per BMS map). Change needed: point the API client at the
monolith's single base URL; add the two missing screens.

*Harvest:* `aws/frontend/{package.json,vite.config.ts,src/**}` as the UI starting point in the UI step.

### 3.5 `1790619634-validation-parity` @ `b60d33f` — parity suite and report format

Union of #38–#43 plus `aws/validation/` (pytest; 98 online checks that replay each CICS screen flow against
the Spring service and compare with GnuCOBOL-derived expectations; 13 batch checks) and
`aws/validation/validation-report.md`. Harvest the suite layout and the report format (per-check table with
program, scenario, expected source, result) as the template for `d-verify` CI reporting; the suite itself
needs the whole AWS-shaped stack, so port the *cases*, not the runner.

### 3.6 `1789609454-golden-set-harness` @ `c068d6f` — golden set for CBTRN02C

*What it is.* `tests/golden/{generate.py,layouts.py,run_reference.sh,GSIDXUTL.cbl,compare.py,mutate.py,selftest.sh}`
and `docs/validation/golden-set/{README.md,reconciliation-named.md,reconciliation-volume.md}`.
`run_reference.sh` compiles the **unmodified** `app/cbl/CBTRN02C.cbl` with
`cobc -x -std=ibm -fsign=EBCDIC -I app/cpy` (plus a tiny indexed-file loader), runs it over generated
datasets and captures `TRANSACT`/`DALYREJS`/`ACCTFILE`/`TCATBALF` outputs, control totals and RC.
`compare.py` reconciles byte-level, field-level (via `layouts.py`, a copybook-driven parser), control
totals, order and return code, with exit codes suitable for CI; `mutate.py` injects 18 defect classes and
`selftest.sh` proves the comparator catches all of them.

*Quality.* Self-test 45/45; named set 610 fields / volume set 22 167 fields reconciled with 0 differences.
This is the only branch whose oracle is actual COBOL execution rather than a Java re-reading of the source
— it is the model for `d-verify`.

*Change needed.* Generalise `run_reference.sh`/`generate.py` from CBTRN02C to a program table (CBTRN01C,
CBACT04C, CBTRN03C, CBSTM03A/B …) in UNT51-4; keep the reconciliation output format
(`reconciliation-*.md`) as the CI artefact.

### 3.7 `cobol-safety-net` @ `1c845c7` and `cbtrn01c-java17` @ `02406cb` — second golden/parity stack

`cobol-safety-net` carries `TEST_STRATEGY.md`, JSON + raw-byte goldens for CBACT01C/CBTRN01C
(`golden-files/`), and `test-harness/` (`copybook.py`, `records.py`, `compare.py`, `reconcile.py`,
`generate_goldens.py`, `cobol/build.sh`, GnuCOBOL stubs `CEE3ABD.cbl`, `COBDATFT.cbl`, `KSDSLOAD.cbl`).
`cbtrn01c-java17` adds `java/carddemo-batch` (Java 17 `--release`, builds on JDK 21, 118 tests) with
`codec/{PackedDecimal,ZonedDecimal}` (PIC-width truncation semantics), `io/{KsdsFile,SequentialFile,VariableRecordWriter}`
(GnuCOBOL RDW prefix handling), `CobDatFt` (port of the assembler date routine) and parity tests that shell
out to the Python harness. Both are open PRs (#51/#52) by the same owner and keep moving; harvest the stubs
and the codec *behaviour* (the COMP-3 and variable-record pieces missing from 3.1), do not depend on the
branches.

### 3.8 Business specs (#29, #30, #35, #36)

`docs/specs/INTCALC-CBACT04C-business-spec.md` @ `6ac4378`, `POSTTRAN-CBTRN02C-business-spec.md` @ `cbb33f0`,
`TRANREPT-CBTRN03C-business-spec.md` @ `b4a0900`, `CREASTMT-CBSTM03A-CBSTM03B-business-spec.md` @ `410dda3`.
Each covers purpose/job position, DD-level inputs and outputs, record layouts, step-by-step processing
rules, error/abend and return-code behaviour, restartability, edge cases, Java-precision concerns and an
explicit "ambiguities / open questions" list (e.g. INTCALC's unimplemented `1400-COMPUTE-FEES`,
CREASTMT's fixed-capacity transaction table). Quality is high and consistent across the four; they do not
prescribe architecture. Carry them into `docs/modernization/specs/` unchanged and add specs for the online
programs in the same template.

### 3.9 Smaller harvests

* **#11 `estate-discovery-dossier` @ `4640a65`** — the generator overlaps `build_inventory.py` /
  `build_dependency_map.py` already on the stack base, so do not carry `build_discovery.py`; carry
  `docs/discovery/03-conversion-construct-register.md` (per-construct occurrence list with `path:line`),
  `04-field-lineage.md` and `05-government-decisions.md` as inputs to the ADRs. Note: this branch holds
  **no** GnuCOBOL compile scripts (the ticket text suggested it does); the compile scripts live on #13, #3
  (`modernized/interest-calculator/oracle/{run-cobol-oracle.sh,LOADVSAM.cbl,RUNCB04.cbl,UNLDACCT.cbl,verify-golden.py,make-edge-dataset.py}`),
  #40 (`aws/batch/golden/`) and #47 (`test-harness/cobol/build.sh`).
* **#3 `cbact04c-java-migration` @ `2d5b9c4`** — harvest `modernized/interest-calculator/oracle/*` (above)
  and the `ProcessExitCodeTest` pattern; the Spring Batch job itself is superseded by #40's `intcalc`.
* **#5 `posting-sliver` @ `4affd4c`** — harvest `modernization/docs/{CBACT04C-logic-map.md,CBTRN02C-logic-map.md,SLIVER-PLAYBOOK.md}`
  (paragraph-by-paragraph logic maps, useful when annotating Java with the COBOL paragraph it implements).
  Its `carddemo-mainframe-io` codec duplicates 3.1 — do not carry both.
* **#6 `cobol-test-foundation` @ `54f4eee`** — harvest the `CEEDAYS` stub and `run_tests.sh` gcov pattern
  for UNT51-4 baselines of programs that call LE date services.
* **#15 `carddemo-java-modernization` @ `2136cd4`** — harvest `modernization/docs/{COBOL-PROGRAM-MAPPING.md,COPYBOOK-TO-SCHEMA-MAPPING.md,API-CONTRACT.md}`
  and `modernization/common/src/main/java/com/carddemo/common/**` (`cobol/CobolValues`, `api/{ApiError,PageResponse}`,
  `error/{BusinessRuleException,DuplicateKeyException}`). The five-service split, schema-per-service and inter-service REST calls
  contradict `d-decomp`; its README also lists open auth and non-idempotent cross-service batch retries.
* **#38 `discovery-contracts` @ `7c9e714`** — harvest `aws/contracts/{api.md,batch.md,data-model.md,conventions.md}`;
  `messaging.md` only if the MQ sub-app is later pulled into scope.
* **#39 `data-migration` @ `672043f`** — harvest `aws/db/schema.sql` as the first cut of the Flyway `V1`
  (review against the monolith's single schema) and `aws/etl/` as the copybook-driven seed loader for
  Testcontainers/golden tests (it already decodes `app/data/EBCDIC`).
* **#46 `UNT17-6-target-adr` @ `8353e72`** — harvest the `@CobolProgram`/`@Paragraph` traceability
  convention, the program→unit table and the PIC→Java type rules from `docs/architecture/ADR-001-target.md`;
  reject its per-domain *services* and Airflow in favour of `d-decomp`/`d-scheduler`.
* **#10 `modernization-plan` @ `e7a43bd`** — only the risk register section is still valid; the target
  (microservices + DynamoDB) is not.
* **#28 `cbimport-cardout-dd` @ `34fbd0e`** — legacy JCL fix, not carried, but it records that
  `CBIMPORT.jcl` on `main` lacks the `CARDOUT` DD that `CBIMPORT.cbl` writes; the Java CBIMPORT port must
  produce the card output regardless of what the JCL on `main` says.

### 3.10 Ignored, with reason

TypeScript track (#14, #16–#27, #31–#34, #37): different target language; nothing transfers except
knowledge already captured better in the specs. #1/#2: Python-era demo material. #4/#7/#9/#48: strictly
superseded by later branches with the same coverage and more tests. #12: adds a new COBOL program
(CBTRN04C) to the legacy estate — a scope change, excluded by `d-scope`. #42: AWS deployment, not needed
for a monolith that runs locally/CI first. #45: superseded by the UNT51 inventory on this stack.

### 3.11 One codec, not six

Six independent Java COBOL codecs exist (#3, #5, #7, #8, #9, #40, #49). Recommendation for the data-type
step: base the monolith's codec on **#8 `carddemo-recordio`** (clean API, EBCDIC+ASCII, round-trip tests on
shipped data) and add from **#49** `PackedDecimal` (COMP-3 with PIC-width truncation) and
`VariableRecordWriter`, and from **#40** `Edited` (edited pictures for report lines). Validate the merged
codec with #13's and #47's goldens, not with the branches' own unit tests.

## 4. Recommendation on `d-reuse`

**`d-reuse` (fresh build on `main` under `modernization/`, harvest reference artifacts) still holds.** Reasons:

1. No branch is the target. The three Java candidates for "keep" each miss a decision: #15 is five
   microservices with JPA and schema-per-service (`d-decomp`); #40/#41/#44 are two separate deployables
   wired to S3/SQS/AWS Batch/Step Functions (`d-decomp`, `d-scheduler`); #8 is three executables on flat
   files with no database. Merging any of them would mean re-architecting inside someone else's package
   layout while also deleting AWS plumbing — more work and more risk than starting the monolith skeleton and
   pulling the proven pieces in.
2. The paths collide. #8 and #15 both live under `modernization/` and `docs/modernization/` with their own
   `01-inventory.md`/`README.md`; the stack already owns those paths. A fresh tree avoids three-way merges.
3. Everything valuable is a *leaf* artifact that copies cleanly: codecs (3.1, 3.11), Spring Batch job
   implementations and ITs (3.2), REST/validation behaviour and the OpenAPI contract (3.3), React screens
   (3.4), four business specs (3.8), two GnuCOBOL golden harnesses (3.6, 3.7), schema + ETL (3.9).
4. The evidence is uneven. Only #13, #40 and #47/#49 compare against COBOL *execution*; #8/#15/#41 test
   against the COBOL *source as read*. `d-verify` requires the former, so the fresh build should be wired to
   the golden harness from the first module, and harvested code must re-pass those goldens before it counts.

One amendment to how the decision is executed, not to the decision: treat the **AWS track (#40, #41, #43)**
as the primary harvest source for implementation code and **#8 recordio + #13/#47 harnesses** as the
primary source for codec and verification, rather than the ticket's original emphasis on #15 as the
"skeleton". #15 contributes documentation and `common` only.

Implications for the next steps: UNT51-4 (GnuCOBOL baseline) should start from #13 `tests/golden/` and
#47's stubs; UNT51-5 (scaffold + ADRs) should record 3.11 and the module list derived from #41's package
inventory; the data-type step should harvest per 3.11; the UI step should fork #43.

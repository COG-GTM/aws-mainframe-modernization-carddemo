# POSTTRAN migration sign-off (Phase 4, `mf-stream-signoff`)

| Item | Value |
|---|---|
| Stream | BATCH POSTTRAN: `app/jcl/POSTTRAN.jcl` STEP15 → `app/cbl/CBTRN02C.cbl` → hard stop `GOBACK` (`CBTRN02C.cbl:234`). ONLINE and SUBTRANSACTION: N/A (D-002) |
| Target | `modernization/` (Java 21, Spring Boot 3.3, Spring Batch 5, PostgreSQL 16 + Flyway V1..V3): job `POSTTRAN`, step `STEP15`, chunk 1, restartable |
| PR stack (none merged) | docs #213 → Phase 0 #214 → Phase 1 #215 → Wave 1 #216 → parity #217 → **this sign-off PR** (base `devin/1795101425-cbtrn02c-parity`) |
| Code under sign-off | `e3dff6f5` (parity head). This PR adds docs, evidence, e2e tooling and one CI job; no program or test logic is changed |
| Date | 2026-11-19 |
| Verdict of this session | **READY FOR INDEPENDENT AUDIT, then STOP E.** 37 / 38 requirements PASS with evidence, 0 FAIL, 1 UNTESTED accepted as a deviation pending STOP E (PR-CBTRN02C-15); 4 human decisions open (§9) |
| Independent audit | **NOT RUN BY THIS SESSION** (the migrating side may not audit itself). A fresh child launched by the orchestrator runs it from `POSTTRAN/evidence/AUDIT_CHECKLIST.md`; its verdict goes in §10 |

## 1. Scope and hard stop

One program, one wave (plan §1). In scope: every statement of `CBTRN02C` from `PROCEDURE DIVISION` to `GOBACK`, the six DDs of STEP15
(DALYTRAN, TRANFILE, XREFFILE, DALYREJS, ACCTFILE, TCATBALF), the RC returned to the scheduler, and the nine boundaries B-01..B-09.
Out of scope: anything the JCL does outside STEP15, other jobs / CICS programs that share the KSDS (B-03..B-06 "other parties"),
the producer of DALYTRAN and any consumer of DALYREJS. No program outside the stream was migrated.

| In-scope program | FR doc | Implementation (`modernization/carddemo-app`) | Tests |
|---|---|---|---|
| `CBTRN02C` | `programs/CBTRN02C_functional_requirement.md` | `transaction.batch.*` (`PosttranJobConfig`, `DailyTransactionReader`, `DailyRejectWriter`, `DailyTransactionPostingWriter`, `TransactionMasterResetListener`, `PosttranStepReport`, `PosttranParametersValidator`), `transaction.service.*` (`DailyTransactionValidator`, `DailyTransactionPostingService`), entities/repositories in `account`, `card`, `reference`, `transaction`; launch seam + codecs in `carddemo-common` | unit + `PosttranJobRunTest`, `PosttranJobParityTest` (7 golden), `Cbtrn02cIndependentParityTest` (20 oracle cases), `JclLaunchSeamRestartTest` |

## 2. End-to-end run on the real stack (procedure step 1)

Command (from the repo root, jar built first):

```bash
(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B -DskipTests package)
.migration/tools/posttran_e2e_oracle.sh <work>/oracle functional/CARDDEMO/POSTTRAN/evidence/oracle   # GnuCOBOL expected outputs (committed)
JAVA=/usr/lib/jvm/java-21-openjdk-amd64/bin/java .migration/tools/posttran_e2e.sh <work>/run         # docker compose stack, 8 scenarios
```

What runs: `docker compose up postgres` (image `postgres:16`, server `PostgreSQL 16.16`), `docker compose build app` (the
`modernization/Dockerfile` image), then **every job launch is `docker compose run app --job=POSTTRAN --run-id=… --dd=DALYTRAN=… --dd=DALYREJS=… --dd=SYSOUT=…`**,
i.e. the shipped image and CLI seam, Flyway-migrated schema, no test profile, no in-memory DB. Masters are loaded and the tables
unloaded with the existing `KsdsDataService` (no master-load CLI exists; D-053) through `.migration/tools/PosttranE2eData.java`.
Inputs come only from `app/data` (ASCII `dailytran.txt` / `acctdata.txt` / `cardxref.txt` / `tcatbal.txt`, EBCDIC
`AWS.M2.CARDDEMO.DALYTRAN.PS`). Each load also inserts one stale TRANSACT row (`STALE-E2E-000001`) that a new instance must remove.
Comparison rule = the parity rule (`CBTRN02C_parity_results.md` §1): RC (oracle 231 ≡ Java 16), DALYREJS byte for byte, SYSOUT lines,
ACCTDATA / TCATBALF / TRANSACT rows with FILLER and TRAN-PROC-TS centiseconds masked (`posttran_e2e_compare.py`).

Result of the run captured in `POSTTRAN/evidence/e2e/` (`e2e_console.txt`): **all 8 scenarios PASS, `e2e FAILS=0`.**

| # | Scenario (run-id) | Expected (source) | Obtained (Java on compose) | Result |
|---|---|---|---|---|
| L01 | `01-baseline` (E2E-BASE): sample masters + sample DALYTRAN (300) | oracle `baseline`: RC 4, DALYREJS 16340 B = 38 × `0102`, SYSOUT 54 lines (50 TCATBAL creations, 300 / 38), 50 / 100 / 262 rows | RC 4; DALYREJS =; SYSOUT =; ACCTDATA 50 =, TCATBALF 100 =, TRANSACT 262 =; stale row gone (`db_rows.txt`); 300 reads, 301 commits (`batch_executions.txt`) | PASS |
| L02 | `02-relaunch-completed` (E2E-BASE again) | FR acceptance / D-033: COMPLETED run-id → exit 12, nothing runs | RC 12 (`already COMPLETED; use a new --run-id`); tables identical to L01; no job execution created | PASS |
| L03 | `03-rerun` (E2E-RERUN): new instance on L01's tables | oracle `rerun` (second `OPEN OUTPUT` run on the baseline end state): RC 4, 113 rejects (48590 B), 187 TRANSACT rows, balances reposted | RC 4; DALYREJS =; SYSOUT =; ACCTDATA / TCATBALF / TRANSACT (187) = | PASS |
| L04 | `04-dalyrejs-exists` (E2E-EXISTS): `--dd=DALYREJS` names L03's existing file | FR-POSTTRAN-17 acceptance (no oracle: JCL `DISP=NEW` allocation): exit 12, nothing posted, file untouched | RC 12; tables identical to L03; DALYREJS sha256 unchanged; no job execution created | PASS |
| L05 | `05-empty` (E2E-EMPTY): empty DALYTRAN on fresh masters | oracle `empty`: RC 0, empty DALYREJS, 0 / 0, TRANSACT empty | RC 0; DALYREJS 0 B =; SYSOUT =; tables = (stale row removed) | PASS |
| L06a | `06-restart-leg1` (E2E-RESTART): record 151 carries record 1's TRAN-ID → forced duplicate-key failure | oracle `restart-leg1`: RC 231, 10 rejects, `ERROR WRITING TO TRANSACTION FILE` / `0022` / `ABENDING PROGRAM`; tables = oracle on records 1-150 (D-042 atomic end state, oracle `restart-leg1-atomic`) | RC 16; DALYREJS 4300 B =; SYSOUT 28 lines =; ACCTDATA 50 =, TCATBALF 74 =, TRANSACT 140 =; execution FAILED: 151 read, 150 written, 150 commits, 1 rollback | PASS (D-042) |
| L06b | `06-restart-leg2` (same E2E-RESTART, original file put back under the same DD path) | Spring Batch restart from the last commit (NFR-01, FR-11, D-033); final state = oracle `baseline` (fresh run of the fixed file) | RC 4; execution #6 on the **same instance** read 150 (records 151-300); DALYREJS (appended) 16340 B = baseline; final `TRANSACTIONS PROCESSED :000000300` / `REJECTED :000000038` =; ACCTDATA / TCATBALF / TRANSACT (262) = baseline | PASS |
| L07 | `07-abend-dalytran-missing` (E2E-ABEND): DALYTRAN DD names a missing file | oracle `abend-dalytran-missing`: RC 231, `ERROR OPENING DALYTRAN` / `FILE STATUS IS: NNNN0035` / `ABENDING PROGRAM`, DALYREJS not created | RC 16; SYSOUT =; DALYREJS not created; tables identical to L06b (TRANSACT not reset) | PASS |
| L08 | `08-cp037` (E2E-CP037): EBCDIC sample DALYTRAN, `--carddemo.posttran.dalytran-encoding=IBM037` | oracle `cp037`: identical to baseline | RC 4; DALYREJS =; SYSOUT =; tables = | PASS |
| — | NFR-04 scan of all 9 job logs | no full card number of `cardxref.txt` in technical logs | 0 matching lines | PASS |

The same oracle generator was re-run in this session and reproduced the runbook baseline hashes (DALYREJS
`b6ef92c0…88410`, ACCTDATA `8b3fc8d1…d65c`, TCATBALF `b2a86541…df5f`). TRANSACT.unl differs between oracle runs only in TRAN-PROC-TS
cols 325-326 (D-041).

## 3. CI regression gate (procedure step 2)

Workflow `.github/workflows/modernization-verify.yml`, on every push to `main` and every PR touching `modernization/**`, `app/data/**`,
the workflow, the e2e tools or the committed oracle outputs:

| Job | What it proves | Required to merge |
|---|---|---|
| `verify` (existing) | `mvn -B verify`: common 24 + app 80 tests = unit, Spring Batch integration on Testcontainers `postgres:16`, CLI-seam restart, 7 golden + 20 independent GnuCOBOL parity cases | **yes** |
| `posttran-e2e-compose` (added here, D-054) | the 8 scenarios of §2 on `docker compose` (postgres:16 + app image) against the committed oracle outputs | **yes** |

Branch protection (making both checks *required*) is a repository-admin setting and is not changed by this PR: listed in §11.
Local run on the code under sign-off: `mvn -B verify` BUILD SUCCESS (common 24, app 80, 0 failures); e2e `FAILS=0`. CI status of
the sign-off head is recorded in the PR.

## 4. Requirement traceability (procedure steps 3 and 6)

Legend: `#n` = case n of `Cbtrn02cIndependentParityTest` (`CBTRN02C_parity_results.md` §2), `G:` = golden case of
`PosttranJobParityTest`, `Lnn` = e2e scenario of §2. Every PASS has a passing executable check; nothing is marked verified from code
reading alone except NFR-02, whose evidence is a reproducible static check.

### 4.1 Stream requirements (`POSTTRAN_functional_requirements.md`)

| Req | Covering tests | E2E / output evidence | Result |
|---|---|---|---|
| FR-POSTTRAN-01 sequential DALYTRAN, one step | #1, #2, G:baseline; `PosttranJobRunTest` | L01 (300 read / 301 commits), L05 | PASS |
| FR-POSTTRAN-02 reject 0100 | #5, #15, G:syn-all; `PR_CBTRN02C_04_*` | — (sample data has no 0100) | PASS |
| FR-POSTTRAN-03 reject 0101 | #5, G:acct-missing; `PR_CBTRN02C_05_*` | — (sample data has no 0101) | PASS |
| FR-POSTTRAN-04 reject 0102 / WS-TEMP-BAL | #1, #3, #5, #6, #9, #12; `PR_CBTRN02C_06_*` | L01 (38 × 0102), L03 (113 rejects) | PASS |
| FR-POSTTRAN-05 reject 0103 | #4, #5; `PR_CBTRN02C_07_*` | — (sample data has no 0103) | PASS |
| FR-POSTTRAN-06 430-byte DALYREJS | #1-#20 DALYREJS byte compare; `DailyRejectWriterTest` | L01, L03, L06, L08 DALYREJS byte-equal | PASS |
| FR-POSTTRAN-07 TCATBAL add / create | #8, #13, #20; `PR_CBTRN02C_10_*` | L01 (50 creations + TCATBALF 100 rows =) | PASS |
| FR-POSTTRAN-08 account balance / cycles | #6, #11, #12, #14, #20; `PR_CBTRN02C_11_*` | L01, L03 ACCTDATA = | PASS |
| FR-POSTTRAN-09 transaction copy + PROC-TS | #1, #14; `PR_CBTRN02C_09_*` | L01 TRANSACT 262 = (`db_rows.txt` proc_ts) | PASS |
| FR-POSTTRAN-10 atomic posting | #16, G:dup-id; `PR_CBTRN02C_12_PR_CBTRN02C_13_*` | L06a (abending record fully rolled back) | PASS against the D-042 atomic expectation — **accepted deviation pending STOP E** (§8 AD-1) |
| FR-POSTTRAN-11 new instance resets / restart keeps | #17, #18; `FR_POSTTRAN_11_*`, `NFR_01_*` | L01 (stale row removed), L03 rerun, L06b restart, L07 (not reset on open failure) | PASS (semantics = D-024, open human decision §9) |
| FR-POSTTRAN-12 RC 4 / 0 | all cases | L01/L03/L06b/L08 RC 4, L05 RC 0 | PASS |
| FR-POSTTRAN-13 failure → exit 16 | #16, #19; G:dup-id | L06a, L07 (oracle 231 ≡ 16) | PASS (RC mapping D-015/D-039, open human decision §9) |
| FR-POSTTRAN-14 SYSOUT lines | all cases SYSOUT compare | L01 54 lines =, L03, L05, L06, L07, L08 = | PASS |
| FR-POSTTRAN-15 empty input | #2, G:empty | L05 | PASS |
| FR-POSTTRAN-16 ASCII or CP037 | #7, #10, G:cp037 | L08 | PASS |
| FR-POSTTRAN-17 refuse existing DALYREJS | `FR_POSTTRAN_17_anExistingDalyrejsIsAJclErrorAndNothingRuns`, `DailyRejectWriterTest.newRunRefusesAnExistingFile…` | **L04** (exit 12, tables and file untouched) | **PASS** (was UNTESTED in the parity report; covered now by e2e against the FR acceptance criterion; no GnuCOBOL oracle exists because the check is JCL allocation) |
| NFR-01 chunk 1, restartable | `NFR_01_*`, `JclLaunchSeamRestartTest`, #17 | L06a/L06b (150 commits, restart reads only 151-300, no double posting, DALYREJS not duplicated); 301 commits for 300 records | PASS |
| NFR-02 money `BigDecimal`, never `double` | — | **`evidence/nfr02_static_check.txt`**: 0 `double/float/Double/Float` in main sources; money columns `NUMERIC(p,2)`; BigDecimal in domain/codec classes | **PASS** (was UNTESTED; static evidence, not observable in outputs) |
| NFR-03 byte/row parity | 20 + 7 cases | L01-L08 | PASS |
| NFR-04 card masked in technical logs | `DailyTransactionPostingServiceTest` (`maskCard` → `************3697`) | **L01-L08 job logs: 0 full card numbers**; SYSOUT business lines = oracle | **PASS** (was UNTESTED). Limitation: the only log statement that carries a card (reason 109) is unreachable by any input (parity §5), so the e2e proves absence of leaks, the unit test proves the mask |

### 4.2 Program requirements (`programs/CBTRN02C_functional_requirement.md`)

| Req | Covering tests | E2E / output evidence | Result |
|---|---|---|---|
| PR-CBTRN02C-01 open six files | #19; `PR_CBTRN02C_01_*` | L07 | PASS for DALYTRAN; other open failures UNTESTED (tables, no oracle) — **accepted deviation pending STOP E** (§8 AD-2) |
| PR-CBTRN02C-02 read to EOF | #1, #2; `PR_CBTRN02C_02_*` | L01, L05 | PASS; non-EOF read failure only as short last record — residual in AD-2 |
| PR-CBTRN02C-03 reset reason | `PR_CBTRN02C_03_*`, #5 | — | PASS |
| PR-CBTRN02C-04 xref 100 | #5, #15; `PR_CBTRN02C_04_*` | — | PASS |
| PR-CBTRN02C-05 account 101 | #5; `PR_CBTRN02C_05_*` | — | PASS |
| PR-CBTRN02C-06 over limit 102 | #3, #6, #9, #12; `PR_CBTRN02C_06_*` | L01 | PASS |
| PR-CBTRN02C-07 expiry 103 over 102 | #4, #5; `PR_CBTRN02C_07_*` | — | PASS |
| PR-CBTRN02C-08 reject write | all reject cases; `DailyRejectWriterTest` | L01, L03, L06 | PASS; write failure UNTESTED — residual in AD-2 |
| PR-CBTRN02C-09 build transaction | #1, #14; `PR_CBTRN02C_09_*` | L01 | PASS |
| PR-CBTRN02C-10 TCATBAL | #8, #13, #20; `PR_CBTRN02C_10_*` | L01 (creation lines) | PASS |
| PR-CBTRN02C-11 account rewrite | #6, #11, #12, #14, #20; `PR_CBTRN02C_11_*` | L01, L03 | PASS (reason 109 unreachable, D-027) |
| PR-CBTRN02C-12 transaction insert / dup abend | #15, #16; `PR_CBTRN02C_12_*` | L06a | PASS |
| PR-CBTRN02C-13 order + one DB transaction | #16; `PR_CBTRN02C_12_PR_CBTRN02C_13_*` | L06a | PASS (D-042, AD-1) |
| PR-CBTRN02C-14 reset on new instance only | #17, #18; `FR_POSTTRAN_11_*` | L01, L03, L06b, L07 | PASS (D-024) |
| PR-CBTRN02C-15 close; close failure abends | — | normal close shown by every completed leg (DALYREJS flushed, RC 0/4) | **UNTESTED — accepted deviation pending STOP E** (§8 AD-3) |
| PR-CBTRN02C-16 counters + RC | all cases | L01-L08 | PASS |
| PR-CBTRN02C-17 abend U999 | #16, #19 | L06a, L07 | PASS (D-015/D-039) |

**Totals: 38 requirements; 37 PASS, 0 FAIL, 1 UNTESTED (PR-CBTRN02C-15, AD-3).** The orchestrator's four previously untested items:

| Item | Disposition |
|---|---|
| PR-CBTRN02C-15 (CLOSE failures) | not covered; **accepted deviation pending STOP E** (AD-3) |
| FR-POSTTRAN-17 | **covered by e2e** L04 (+ existing tests) |
| NFR-02 | **covered by static evidence** `nfr02_static_check.txt` (a code property, not observable in outputs) |
| NFR-04 | **covered by e2e** log scan L01-L08 + unit test, with the limitation stated above |

## 5. Parity summary

Independent pass (PR #217, `CBTRN02C_parity_results.md` §10): **20 / 20 PASS** on `e3dff6f5`; DV-01 / DV-02 (negative zero) RESOLVED by
D-052 (Flyway V3 sign flags). Golden harness: 7 / 7 PASS. E2E on compose: 8 / 8 PASS (§2).

## 6. Boundary closure (procedure step 4)

All nine boundaries are **IMPLEMENTED and VERIFIED**; none is deferred. Dated rows appended to `.migration/04_boundary_register.md`.
Routing point / cutover condition / coexistence verification are those of plan §4 (production coexistence none, D-022). Lead-time
request: none needed (plan §4, "fired: N/A, nothing to request").

| Id | Class | Status | E2E evidence | Routing point / cutover condition |
|---|---|---|---|---|
| B-01 DALYTRAN | DATASET_IN | VERIFIED | L01, L05, L07 (missing file), L08 (CP037) | scheduler step launches COBOL or Java on the same file / parallel run on a production copy equal |
| B-02 DALYREJS | DATASET_OUT | VERIFIED | L01, L03, L06 (restart append), L04 (refuse existing) | same step / byte-identical DALYREJS |
| B-03 TRANSACT | SHARED_DATA | VERIFIED | L01 (reset), L03 (rerun), L06 (kept on restart), L07 (kept on open failure) | other jobs / CICS stay on VSAM until their streams migrate / unload = oracle |
| B-04 ACCTDATA | SHARED_DATA | VERIFIED | L01, L03, L06 | as B-03 |
| B-05 CARDXREF | SHARED_DATA | VERIFIED | L01 (read-only, 50 rows unchanged) | as B-03 |
| B-06 TCATBALF | SHARED_DATA | VERIFIED | L01 (50 creations), L03, L06 | as B-03 |
| B-07 CEE3ABD | RUNTIME_SERVICE | VERIFIED | L06a, L07 (exit 16 ≡ 231) | N/A / dup-id exits 16 |
| B-08 Scheduler | SCHEDULER | VERIFIED (exit codes) | L01 4, L05 0, L02/L04 12, L06a/L07 16 | CA-7 job definition switches program / ops dry run of the chain (open track §11) |
| B-09 Online window | ONLINE_COEXISTENCE | VERIFIED (no code) | — (CLOSEFIL/OPENFIL untouched) | N/A / online streams migrated separately |

## 7. Conformance to the target state

Java 21 / Boot 3.3 / Batch 5, modules `carddemo-app` + `carddemo-common`, PostgreSQL 16 + Flyway (`ddl-auto: validate`), one job per
JCL, one step per EXEC, chunk 1 with restart, DALYTRAN / DALYREJS files, KSDS → tables. No Lombok, no H2 (Testcontainers
`postgres:16` only), no `double` (NFR-02 evidence). New dependencies all registered (D-030 Testcontainers 1.21.4). This PR adds no
dependency.

## 8. Accepted deviations pending STOP E (residual risks)

| Id | Requirement | Deviation | Why accepted (evidence) | Re-entry condition |
|---|---|---|---|---|
| AD-1 | FR-POSTTRAN-10, PR-CBTRN02C-13 | **D-042**: on a duplicate-TRAN-ID abend COBOL leaves the abending record half posted (TCATBAL + account rewritten, no transaction); Java rolls the whole record back | FR-10 requires atomicity; a clean restart (NFR-01) depends on it. RC, DALYREJS and SYSOUT equal the oracle; tables equal the oracle on the records before the abending one (#16, G:dup-id, L06a) | user rejects D-042 at STOP E → reproduce the half-posted state (would break restart safety) |
| AD-2 | PR-CBTRN02C-01/02/08 (sub-cases) | open failure of TRANSACT / CARDXREF / ACCTDATA / TCATBALF / DALYREJS, non-EOF read failure, DALYREJS write failure are not exercised | tables have no open; GnuCOBOL cannot be made to return these statuses without runtime fault injection, so no oracle exists (parity §5). Code maps them to `ERROR …` + exit 16 | a fault-injection oracle becomes available or the user requests mocked-failure tests |
| AD-3 | PR-CBTRN02C-15 | CLOSE failure → abend not exercised | no target equivalent beyond stream close (tables are not closed); same oracle limitation as AD-2 | as AD-2 |

## 9. Open human decisions (call out at STOP E, not changed here)

| Decision | Content | Where it shows |
|---|---|---|
| D-015 / D-039 | GnuCOBOL U999 is RC 231 on Linux; Java exits 16; parity treats 231 ≡ 16 | L06a, L07; parity cases #16, #17, #19 |
| D-024 | a new instance empties `transaction.transactions` (like `OPEN OUTPUT`); a restart keeps it. Consequence: a rerun re-posts every amount on the already-posted balances (L03: 113 rejects, 187 transactions) exactly as COBOL does | L01, L03, L06b |
| D-042 | AD-1 above | L06a; G:dup-id; #16 |
| PR #214 Devin Review findings, not addressed | (a) batch options without `=` (`--run-id R1`, `--dd DALYTRAN=/in/a`) are passed through to Spring and silently ignored, so the job runs with a generated run-id / without that DD instead of exiting 12 (`BatchArguments.parse`, review comment on #214); (b) `docker-compose.yml` publishes PostgreSQL on host port `${CARDDEMO_DB_PORT:-5432}` with fixed credentials | (a) every launch here uses `--opt=value`, so the sign-off is not affected; (b) the e2e uses `CARDDEMO_DB_PORT=55432`; neither breaks sign-off. Registered in D-056 |

## 10. Independent audit verdict

**PASS WITH FINDINGS** — independent audit by a fresh session that did not perform the migration, on `5f92dd43`, full document
`functional/CARDDEMO/POSTTRAN_audit.md`. No blocker, no major. Findings reproduced verbatim by id and severity:

| Id | Severity | Finding |
|---|---|---|
| F-01 | Minor | Launch options without `=` are silently passed through: `--run-id AUDIT-OWN` after a COMPLETED `AUDIT-OWN` starts a new instance (run-id from the batch clock), resets TRANSACT and re-posts ACCTDATA/TCATBALF with RC 4 instead of exit 12 (Devin Review finding on #214; not worse than a legacy JCL re-submit) |
| F-02 | Minor | DALYREJS abnormal disposition `DISP=(NEW,CATLG,DELETE)` not modelled nor registered: Java keeps the partial reject file after an abend (needed for restart) |
| F-03 | Minor | `docker-compose.yml` publishes PostgreSQL on all host interfaces with fixed credentials `carddemo/carddemo` (Devin Review finding on #214) |
| F-04 | Minor | No executable evidence for CLOSE failure (PR-CBTRN02C-15, AD-3) nor OPEN/WRITE failures of table-backed files and DALYREJS write (AD-2) |
| N-01 | Info | `posttran_e2e.sh` defaults to `JAVA=java`; with Java 17 the seeding helper fails to compile (R-25 passes Java 21) |
| N-02 | Info | `spring-batch-core` declared directly in `carddemo-common`; D-009 names the batch starter, not the artifact |
| N-03 | Info | PR #218: `verify`, `posttran-e2e-compose`, Socket pass; Devin Review checks fail (not run nor addressed by instruction) |
| N-04 | Info | Checklist names `POSTTRAN_independent_audit.md`; requester asked for `POSTTRAN_audit.md` |

Auditor re-execution: `mvn -B verify` green (24 + 80 tests); GnuCOBOL oracle regenerated (7 scenarios, identical to the committed
oracle except TRANSACT centiseconds); compose e2e against the auditor's oracle 8/8 legs PASS, NFR-04 0 hits; a new auditor-built case
(0100/0102/0103, debit, zero amount, TCATBAL creation) equal on RC, DALYREJS, SYSOUT and the three tables. Deviations: D-015/D-039,
D-024, AD-2/AD-3, NFR-04 partial acceptable; D-042 (AD-1) acceptable but needs explicit user acceptance at STOP E; F-02 should be
recorded as a decision.

## 11. Outstanding items / open track (must land before decommissioning the legacy path)

| Item | Owner | Status |
|---|---|---|
| Independent audit (§10) | orchestrator → fresh child | pending |
| STOP E: user authorization to merge #213 → #214 → #215 → #216 → #217 → sign-off PR in order | user | pending |
| Human decisions D-015/D-039, D-024, D-042, PR #214 findings (§9) | user | pending |
| Branch protection: mark `verify` and `posttran-e2e-compose` as required checks on `main` | repo admin | not done (admin setting) |
| Master-data load for production copies: today only `KsdsDataService` (D-053); a parallel run on a production copy needs an unload from VSAM + load step | operations / next stream | open |
| CA-7: replace CBTRN02C in POSTTRAN with `java -jar carddemo-app.jar --job=POSTTRAN --dd=…`, RC 4 = warning, 12/16 = failure; ops dry run of CLOSEFIL → CBPAUP0J → POSTTRAN → WAITSTEP (B-08 cutover condition) | operations | open |
| Data coexistence for B-03..B-06: other jobs and CICS programs still read the VSAM files; keep COBOL authoritative until their streams migrate (D-022) | program owners | open |
| DBA registration of schemas `account`, `card`, `reference`, `transaction`, `batch` on the production PostgreSQL 16 | DBA | not requested (no production target named) |

## 12. Evidence index

`functional/CARDDEMO/POSTTRAN/evidence/README.md` lists every file; `AUDIT_CHECKLIST.md` is the auditor's entry point.

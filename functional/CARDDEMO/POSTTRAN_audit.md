# POSTTRAN independent audit (BATCH: `POSTTRAN.jcl` → `CBTRN02C` → `GOBACK`)

- Auditor: fresh session that did not take part in the migration (`mf-stream-signoff` step 5). Migration reports, the sign-off and the evidence folder were read as claims only; every result below was re-run by the auditor.
- Audited revision: `devin/1795102915-posttran-signoff` @ `5f92dd43` (PR #218, stack #213 → #214 → #215 → #216 → #217 → #218, none merged).
- Date: 2026-11-19. Guide used (not trusted): `functional/CARDDEMO/POSTTRAN/evidence/AUDIT_CHECKLIST.md`.

## Verdict

**PASS WITH FINDINGS.** No blocker, no major. Every in-scope paragraph of `CBTRN02C` has behaviour in the Java job, every FR has executable evidence, B-01..B-09 are closed, and the auditor's own GnuCOBOL vs Java runs (7 oracle scenarios, 8 compose legs, 1 new auditor-built case) match on DALYREJS, SYSOUT, tables and RC. Four minor findings and four informational notes follow; the minor ones should be decided at STOP E but do not block it.

## Findings

| Id | Severity | Finding | Reproduction / cite |
|---|---|---|---|
| F-01 | Minor | Launch options written without `=` are passed through silently instead of failing as usage errors (exit 12). `--run-id AUDIT-OWN` after a COMPLETED `AUDIT-OWN` starts a **new** instance with a run-id generated from the batch clock (`20220718-103045-000`): TRANSACT is reset (26 → 25 rows) and ACCTDATA/TCATBALF are posted again, RC 4, no warning. `--dd DALYREJS /x` would also be ignored (default path used). Not worse than legacy (re-submitting the JCL also re-runs `OPEN OUTPUT` and re-posts), so minor; still breaks the D-032/D-050 contract "bad arguments → 12". Same as Devin Review finding on #214. Recommend: reject any `--job`/`--dd`/`--run-id` token without `=`. | `BatchArguments.parse` (`modernization/carddemo-common/src/main/java/com/carddemo/common/batch/BatchArguments.java`) sends unmatched tokens to pass-through. Repro (compose stack of `posttran_e2e.sh`, fresh `down -v`, masters seeded): run `--run-id=AUDIT-OWN` → RC 4; `--run-id=AUDIT-OWN` again → RC 12 (correct); `--run-id AUDIT-OWN` → RC 4 and `batch.batch_job_execution_params` shows a new `run.id=20220718-103045-000` instance, `ACCTDATA.unl` differs from the first run. |
| F-02 | Minor | Abnormal disposition of DALYREJS is not modelled and not registered as a deviation. JCL `DALYREJS DD DISP=(NEW,CATLG,DELETE)` deletes the new generation when the step abends; Java keeps the partial file (needed for Spring Batch restart, L06a leaves 4300 bytes / 10 records). Operational effect: after an abend a partial reject file exists at the DD path; a new instance on that path is refused (exit 12, D-050), so the operator must either restart or delete it. Acceptable trade-off for restart (NFR-01), but it should be an explicit decision. | `app/jcl/POSTTRAN.jcl:34-38`; D-016 cites the DISP only for "refuse existing file". Repro: e2e leg `06-restart-leg1` (RC 16) → `DALYREJS` 4300 bytes remains. |
| F-03 | Minor | `modernization/docker-compose.yml` publishes PostgreSQL on all host interfaces (`${CARDDEMO_DB_PORT:-5432}:5432`) with fixed credentials `carddemo/carddemo`. Dev/e2e-only file, but nothing marks it as such and it is the documented local stack. Recommend binding to `127.0.0.1` and reading the password from env. Same as Devin Review finding on #214. | `modernization/docker-compose.yml` lines 5-12. |
| F-04 | Minor | Failure paths without executable evidence: PR-CBTRN02C-15 CLOSE failure → abend (AD-3, no test at all), and OPEN/WRITE failures of the table-backed files and of DALYREJS write (AD-2). The happy close is shown by every completed leg; the abend mapping itself is proven by L06a/L07. Acceptable as documented residual risk; a fault-injection test (e.g. unwritable DALYREJS directory, DB stopped mid-run) would close it cheaply. | Sign-off §4 rows PR-CBTRN02C-01/-08/-15; no test method references `-15`. |
| N-01 | Info | `posttran_e2e.sh` defaults to `JAVA=java`; on this VM that is Java 17 and the seeding helper `PosttranE2eData.java` fails to compile (16 errors, `cannot access CardDemoApplication`). Runbook R-25 does pass `JAVA=.../java-21...`; CI uses Java 21. Recommend failing fast with a version check. | `.migration/tools/posttran_e2e.sh` line 12; auditor's first e2e attempt `~/audit/e2e.log`. |
| N-02 | Info | `carddemo-common` declares `org.springframework.batch:spring-batch-core` directly; D-009 registers the Boot batch starter (which brings it), not the artifact itself. No new library is introduced; recommend naming it in D-009/D-031 for completeness. | `modernization/carddemo-common/pom.xml:21`; `.migration/06_decisions.md` D-009, D-031. |
| N-03 | Info | PR #218 checks: `verify`, `posttran-e2e-compose`, Socket Security = pass; `Devin Review` and `Devin Review / Sparta Review Guardrails` = fail. Not run nor addressed (explicit instruction). Branch-protection "required checks" were not verifiable from this session. | `git_pr_checks` on #218. |
| N-04 | Info | The checklist names the output `POSTTRAN_independent_audit.md`; this audit follows the requester's name `POSTTRAN_audit.md`. | `AUDIT_CHECKLIST.md`. |

## 1. Scope rebuilt independently

From `app/jcl/POSTTRAN.jcl` (one `EXEC`: `STEP15 PGM=CBTRN02C`, DDs TRANFILE, DALYTRAN, XREFFILE, DALYREJS, ACCTFILE, TCATBALF) and `app/cbl/CBTRN02C.cbl` (procedure `:193` to `GOBACK` `:234`, no `CALL` except `CEE3ABD` in `9999-ABEND-PROGRAM`). Every paragraph mapped to Java behaviour (no empty class, stub or mock-only path found):

| COBOL paragraph(s) | Java | Evidence re-run |
|---|---|---|
| `0000`..`0500` opens (6 files) | `DailyTransactionReader` open, `DailyRejectWriter` (`CREATE_NEW` / restart truncate to `dalyrejs.offset`), `TransactionMasterResetListener` (`OPEN OUTPUT` = new instance empties TRANSACT), JPA repositories | L01, L04, L06, L07 |
| `1000-DALYTRAN-GET-NEXT` loop to EOF | `PosttranJobConfig` step `STEP15`, chunk `JclSteps.COMMIT_INTERVAL = 1` | L01 300 records, L05 empty |
| `1500-VALIDATE-TRAN`, `1500-A`, `1500-B` (0100/0101/0102/0103) | `DailyTransactionValidator` (`CobolDecimal` truncation for `WS-TEMP-BAL`) | own case (0100, 0102, 0103), golden `acct-missing`, `reason-precedence`, `limit-exact`, `expiry-boundary` |
| `2500-WRITE-REJECT-REC` | `DailyRejectWriter` (350 + 4 + 76 bytes) | DALYREJS byte-equal in every leg |
| `2000`, `2700`/`2700-A`/`2700-B`, `2800`, `2900` | `DailyTransactionPostingService` (`@Transactional`, `BigDecimal`, status 22/30 mapping) | ACCTDATA/TCATBALF/TRANSACT unloads equal; creation lines in SYSOUT |
| `Z-GET-DB2-FORMAT-TIMESTAMP` | `Db2Timestamp` + `carddemo.batch.clock` | TRANSACT equal with cols 325-326 masked |
| counters, RC 4, `9000`..`9500` closes | `PosttranStepReport`, `JclExitCodeMapper` | SYSOUT equal, RC 0/4 |
| `9999-ABEND-PROGRAM`, `9910-DISPLAY-IO-STATUS` | `PosttranAbendException` → FAILED → exit 16 | L06a, L07 |

## 2. Target-state conformance (verified by the auditor)

| Check | Result | How |
|---|---|---|
| `app/` unchanged vs `main` | PASS | `git diff --stat origin/main 5f92dd43 -- app` empty |
| Java 21 / Boot 3.3 / Batch 5 / modules | PASS | `modernization/pom.xml` (parent 3.3.4, `java.version` 21); modules `carddemo-common`, `carddemo-app` |
| PostgreSQL 16 + Flyway | PASS | `db/migration/V1__schemas.sql`, `V2__posttran_tables.sql`, `V3__negative_zero_signs.sql`; compose and Testcontainers use `postgres:16` |
| No Lombok, no H2 | PASS | grep of all POMs and sources (only a comment in `PostgresContainerSupport.java` mentions H2 as forbidden) |
| No `double`/`float` | PASS | 0 occurrences of `double`/`float`/`Double`/`Float` in `src/main` and `src/test`; money is `BigDecimal` |
| New dependencies registered | PASS (N-02) | D-009, D-030 (Testcontainers 1.21.4), D-031 (slf4j-api), D-034 (runtime image) |
| One job per JCL, one step per EXEC | PASS | job `POSTTRAN`, step `STEP15` only |
| Chunk 1, restart | PASS | `JclSteps.COMMIT_INTERVAL = 1`; L06 restart re-runs from record 151 and ends equal to the baseline oracle |
| DALYTRAN / DALYREJS stay files | PASS | `--dd=DALYTRAN=`, `--dd=DALYREJS=` file paths; KSDS → `account.accounts`, `card.card_xref`, `reference.tran_cat_balances`, `transaction.transactions` |

## 3. Hands-on re-execution

| Command (auditor) | Result |
|---|---|
| `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)` | BUILD SUCCESS; common 24 tests, app 80 tests, 0 failures (incl. `Cbtrn02cIndependentParityTest` 20 GnuCOBOL cases, `PosttranJobParityTest` 7 golden cases, Testcontainers `postgres:16`) |
| `.migration/tools/posttran_e2e_oracle.sh ~/audit/oracle-work ~/audit/oracle` (GnuCOBOL 3.1.2) | RC baseline 4, rerun 4, empty 0, restart-leg1 231, restart-leg1-atomic 4, cp037 4, abend-dalytran-missing 231. Diff vs committed `evidence/oracle`: identical except TRANSACT bytes 325-326 (centiseconds, D-041) |
| `JAVA=/usr/lib/jvm/java-21-openjdk-amd64/bin/java .migration/tools/posttran_e2e.sh ~/audit/e2e2 ~/audit/oracle` (compared with the **auditor's** oracle, not the committed one) | 8 legs PASS, `NFR-04: PASS 0 job-log lines carry a full card number`, `e2e FAILS=0` |
| New auditor case (not in any committed fixture): 30 sample records, rec 3 unknown card, rec 5 amount -1234.50, rec 7 origin 2099, rec 9 amount +999999999.99, rec 11 new type/cat `99`/`9999`, rec 13 zero amount; GnuCOBOL `posttran_gnucobol_oracle.sh` vs Java on compose | Oracle RC 4, 30 processed / 4 rejected (0100, 0103, 0102, 0102), 4 TCATBAL creation lines. Java: RC 4, DALYREJS 1720 bytes byte-equal, SYSOUT 8 lines equal, ACCTDATA 50, TCATBALF 54, TRANSACT 26 rows equal; 0 full card numbers in the job log |

Requested comparisons: **baseline** (RC 4/4, DALYREJS 38 records, tables equal), **empty** (RC 0/0, 0-byte DALYREJS, TRANSACT 0 rows), **rejection** (baseline 38 rejects, rerun 113 rejects, auditor case 4 rejects), **abend** (231 ≡ 16, L06a, L07) — all equal.

Tests are not tautological: `Cbtrn02cIndependentParityTest` and `PosttranJobParityTest` assert RC, DALYREJS bytes, SYSOUT lines and table unloads against GnuCOBOL output files committed beside each case (`oracleRc` maps only 231 → 16); the e2e compares against oracle output produced independently (the auditor regenerated it).

## 4. Requirement evidence

| Requirement | Evidence re-run by the auditor | Status |
|---|---|---|
| FR-01 sequential to EOF | L01 300 processed | PASS |
| FR-02 0100 | own case rec 3; L01 | PASS |
| FR-03 0101 | golden `acct-missing` (mvn verify) | PASS (no e2e leg; sample data has no orphan xref) |
| FR-04 0102 | own case rec 9; golden `limit-exact`, `ovf-temp-bal` | PASS |
| FR-05 0103, precedence | own case rec 7; golden `reason-precedence`, `expiry-boundary` | PASS |
| FR-06 430-byte reject | DALYREJS byte-equal everywhere | PASS |
| FR-07 TCATBAL add/create + line | own case rec 11; golden `tcatbal-create-rewrite` | PASS |
| FR-08 account balances | own case rec 5 (debit) and rec 13 (zero → credit) | PASS |
| FR-09 transaction insert, proc ts | TRANSACT equal (cols 325-326 masked) | PASS |
| FR-10 atomic posting | `DailyTransactionPostingServiceTest.PR_CBTRN02C_12_PR_CBTRN02C_13_*`, L06a | PASS vs D-042 expectation (deviation, §5) |
| FR-11 new instance resets / restart keeps | L01, L03, L06; F-01 repro | PASS |
| FR-12 RC 4 / 0 | L01, L05, own case | PASS |
| FR-13 failure → 16 | L06a, L07 | PASS (D-015/D-039) |
| FR-14 SYSOUT | all legs | PASS |
| FR-15 empty | L05 | PASS |
| FR-16 ASCII / CP037 | L08, golden `cp037`, `*-cp037` | PASS |
| FR-17 existing DALYREJS refused | L04 (exit 12, file untouched, tables unchanged) | PASS |
| NFR-01 chunk 1 + restart | L06 | PASS |
| NFR-02 BigDecimal | grep | PASS |
| NFR-03 exact parity | §3 | PASS |
| NFR-04 card masking | e2e log scan (auditor's runs: 0 hits), `cardNumbersAreMaskedInTechnicalLogs` | PASS, partial: the only masked log line (reason 109) is unreachable by input |

## 5. Boundaries and deviations

Boundaries B-01..B-09 (`.migration/04_boundary_register.md`): B-01, B-02, B-03, B-04, B-05, B-06, B-07 re-proved by the auditor's e2e and own case; B-08 exit codes 0/4/12/16 observed by the auditor (CA-7 switch is an open operations track, sign-off §11); B-09 no code, online streams out of scope. **All closed**; none undecided.

| Deviation | Auditor judgement |
|---|---|
| D-015 / D-039 (oracle RC 231 ≡ Java 16) | **Acceptable.** 231 is GnuCOBOL/Linux's `999 mod 256`, not a mainframe value; on z/OS this is an abend U999, which a scheduler treats as "not normal end". Exit 16 is the conventional failure code; the scheduler definition (B-08) must treat 16 as abend. |
| D-024 (new instance empties TRANSACT, restart keeps) | **Acceptable.** Same observable effect as `OPEN OUTPUT` (`CBTRN02C.cbl:256`) for every new run (L01, L03); restart is a strict improvement. F-01 shows the one way to trigger a reset by mistake. |
| D-042 / AD-1 (atomic record vs COBOL half-posted record before the duplicate abend) | **Acceptable, needs explicit user acceptance at STOP E.** It is the only place where "same tables" does not hold, only on an abend path, and the Java state is the consistent one (COBOL leaves TCATBAL+ACCT updated without the transaction, which needs manual repair). |
| AD-2 / AD-3 (PR-CBTRN02C-15 and other failure paths untested) | **Acceptable as residual risk** (F-04). |
| NFR-04 partial | **Acceptable** (§4). |
| PR #214 Devin Review findings | Both confirmed by the auditor: F-01 and F-03, minor. |
| DALYREJS `DISP=(…,DELETE)` on abend | **Not registered** (F-02); acceptable if recorded as a decision. |

## 6. Auditor working files

Scratch under `~/audit/` on the auditor VM (not committed): `oracle/` (regenerated oracle), `e2e2/` (compose legs + `A-own`, `C-relaunch-eq`, `B-noeq`), `own/` (auditor case input and oracle), `mvn_verify.log`.

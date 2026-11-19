# CBTRN02C – independent parity results (Wave 1, POSTTRAN / STEP15)

| Item | Value |
|---|---|
| Date | 2026-11-19 |
| Implementation under test | branch `devin/1795099675-posttran-wave1` (PR #216), SHA `cb5affa9`, module `modernization/`, job `POSTTRAN` |
| Oracle | GnuCOBOL 3.1 (BDB indexed) on `app/cbl/CBTRN02C.cbl` via `.migration/tools/posttran_gnucobol_oracle.sh`, `COB_CURRENT_DATE='2022/07/18 10:30:45'` |
| Java side | real CLI seam `CardDemoApplication.launch` (`--job=POSTTRAN --run-id=… --dd=DALYTRAN=… --dd=DALYREJS=… --dd=SYSOUT=…`, `--carddemo.batch.clock` = oracle clock, D-046) on PostgreSQL 16 (Testcontainers `postgres:16`, Flyway V1+V2). No H2 |
| Test | `modernization/carddemo-app/src/test/java/com/carddemo/app/Cbtrn02cIndependentParityTest.java` (19 cases) |
| Inputs + oracle outputs | `modernization/carddemo-app/src/test/resources/parity/cbtrn02c/<case>/{input,expected*}` (reproducible seed data) |
| Generators | `.migration/tools/cbtrn02c_parity_cases.py` (inputs, reads `app/data/ASCII` only), `.migration/tools/cbtrn02c_parity_oracle.sh` (runs GnuCOBOL, copies its outputs unchanged) |
| Customer-supplied cases | none supplied |
| Verdict | **17 PASS, 2 FAIL** (DV-01, DV-02: negative zero sign), plus untested requirements listed in §5 |

Independence: expected values come only from GnuCOBOL runs of the COBOL source and from the FR docs; the case list was built from
`CBTRN02C.cbl` and the FR docs, not from the Java code. The existing 7-case golden harness (`ParityHarness`, `GoldenCase`) is
not reused: this test has its own case loader, seeding, CLI launch and comparison. None of the 15 new mock cases exists in the
golden set; `baseline` and `empty` are re-generated here as anchors.

## 1. Normalisation rule (both sides, one rule)

- **RC**: oracle 231 (U999 through the CEE3ABD stub on Linux) is read as 16 (D-015, D-039); 0 and 4 compared as is.
- **DALYREJS**: byte for byte, no normalisation.
- **Tables**: Java unload (`KsdsDataService.unload`, key order) vs oracle `.unl` (line sequential, padded to LRECL). Masked on
  both sides: FILLER bytes from the copybooks (CVACT01Y cols 123-300, CVTRA01Y 29-50, CVTRA05Y 331-350; not stored, D-020/D-036)
  and the TRAN-PROC-TS centiseconds (CVTRA05Y cols 325-326, not pinned by `COB_CURRENT_DATE`, D-041). Nothing else.
- **SYSOUT**: same lines in the same order after removing the GnuCOBOL-only lines `CEE3ABD ABCODE=…` and `libcob: …` and
  trailing blanks.
- Every case seeds one stale TRANSACT row (`STALE-PARITY-001`) that a new instance must remove (`OPEN OUTPUT`, `cbl:256`).

## 2. Case table (expected = oracle, obtained = Java)

Columns: RC / DALYREJS records (reason codes) / SYSOUT counters (processed, rejected) / TRANSACT rows after the run.
"=" means the artefact is identical after normalisation (DALYREJS bytes, all ACCTDATA / TCATBALF / TRANSACT rows, SYSOUT lines).

| # | Case | Requirements | Input (source) | Expected (oracle) | Obtained (Java) | Result |
|---|---|---|---|---|---|---|
| 1 | `baseline` | FR-01/04/06/07/08/09/12/14, PR-02/06/08/09/10/11/16 | `app/data/ASCII/dailytran.txt` (300) + sample masters | RC 4; 38 × 0102; 300 / 38; 262 rows | RC 4; DALYREJS =; 300 / 38; tables =; SYSOUT = | PASS |
| 2 | `empty` | FR-15, PR-02/16 | empty DALYTRAN | RC 0; 0 rejects; 0 / 0; 0 rows (stale row removed) | RC 0; =; =; = | PASS |
| 3 | `limit-exact` | FR-04, PR-06 | 4 records, accts 1-4: temp = limit (posts), limit + 0.01 (0102), negative amount landing on limit (posts), zero amount on limit (posts) | RC 4; 1 × 0102 (rec 2); 4 / 1; 3 rows | RC 4; =; =; = | PASS |
| 4 | `expiry-boundary` | FR-05, PR-07 | 7 records, accts 5-11: expiry = orig date, -1 day, ORIG-TS garbage after col 10, expiry spaces, ORIG-TS spaces, expiry `2022/01/01`, orig = expiry + 1 day | RC 4; 0103 on rec 2, 4, 7; 7 / 3; 4 rows | RC 4; =; =; = | PASS |
| 5 | `reason-precedence` | FR-02/03/04/05/06, PR-03/04/05/06/07/08 | 7 records: unknown card, over limit, xref → missing acct 13, expired + over limit, expired, unknown card after 0103, valid | RC 4; 0100, 0102, 0101, 0103, 0103, 0100; 7 / 6; 1 row | RC 4; =; =; = | PASS |
| 6 | `same-account-multi` | FR-04/07/08, PR-06/10/11 | 7 records on acct 17 (limit 100): +40, +40, +40, -50, 0, -200, +10 | RC 4; 0102 on rec 3, 5, 7; 7 / 3; 4 rows; acct 17 CURR-BAL / CYC-CREDIT / CYC-DEBIT from the oracle | RC 4; =; =; = | PASS |
| 7 | `same-account-multi-cp037` | FR-16, PR-02 | case 6 re-encoded CP037 (`dalytran.charset=IBM037`) | identical to case 6 (oracle DALYREJS `cmp`-equal to the ASCII run) | RC 4; =; =; = | PASS |
| 8 | `tcatbal-create-rewrite` | FR-07/14, PR-10 | 5 records accts 18/19: new key (create + SYSOUT line), same key (rewrite), existing sample key, new key after a successful read, seeded key with 123.45 | RC 0; 0 rejects; 5 / 0; 5 rows; 2 creation lines | RC 0; =; =; = | PASS |
| 9 | `ovf-temp-bal` | D-047, FR-04, PR-06 | accts 20-22: CYC-CREDIT 1000000000.00 + 5.00, 999999999.99 + 0.02, 0 - (-999999999.99) + 1.00 (WS-TEMP-BAL `S9(09)V99`, `cbl:187,403`) | RC 0; all 3 post (truncated temp ≤ limit); 3 / 0 | RC 0; =; =; = | PASS |
| 10 | `ovf-temp-bal-cp037` | FR-16, D-047 | case 9 in CP037 | identical to case 9 | RC 0; =; =; = | PASS |
| 11 | `ovf-acct-bal` | D-047, FR-08, PR-11 | accts 23/24: CURR-BAL ±9999999999.99 ± 1.00 (`S9(10)V99`, `cbl:547`) | RC 0; 2 / 0; acct 23 → `00000000009I` (+0.99), acct 24 → `00000000009R` (-0.99) | RC 0; =; =; = | PASS |
| 12 | `ovf-cyc` | D-047, FR-04/08, PR-06/11 | accts 25/26: CYC-CREDIT 9999999999.50 + 1.00, CYC-DEBIT -9999999999.50 - 1.00 (`cbl:549,551`) | RC 0; 2 / 0; truncated cycle fields | RC 0; =; =; = | PASS |
| 13 | `ovf-tcatbal` | D-047, FR-07, PR-10 | accts 27-29: TRAN-CAT-BAL 999999999.99 + 1.00, -999999999.99 - 0.01, create with 999999999.99 (`S9(09)V99`, `cbl:508,527`) | RC 0; 3 / 0; TCATBAL 28/01/0001 = `0000000000}` (**negative zero**) | RC 0; DALYREJS =; SYSOUT =; ACCTDATA =; TRANSACT =; TCATBAL 28/01/0001 = `0000000000{` | **FAIL (DV-02)** |
| 14 | `negative-zero` | FR-08/09, PR-09/11 | acct 30, DALYTRAN-AMT `0000000000}` (-0.00) | RC 0; 1 / 0; CYC-DEBIT branch not taken (`-0 >= 0`), TRANSACT TRAN-AMT = `0000000000}` | RC 0; DALYREJS =; SYSOUT =; ACCTDATA =; TCATBALF =; TRANSACT TRAN-AMT = `0000000000{` | **FAIL (DV-01)** |
| 15 | `dup-rejected-id` | FR-02/13, PR-04/12 | 4 records: id A posts, id A rejected 0100 (no WRITE, no abend), id B rejected, id B posts | RC 4; 2 × 0100; 4 / 2; 2 rows | RC 4; =; =; = | PASS |
| 16 | `dup-abend-newkey` | FR-10/13, PR-10/11/12/13/17 | 4 records: post, 0100, duplicate id on a new TCATBAL key (acct 34), never-read record | RC 231; 1 × 0100; SYSOUT creation line + `ERROR WRITING TO TRANSACTION FILE` / status `0022` / `ABENDING PROGRAM`; tables vs `expected-atomic` (oracle run on records 1-2, D-042) | RC 16; =; SYSOUT =; tables = `expected-atomic` | PASS (under D-042) |
| 17 | `restart-fixed` | NFR-01, FR-11, PR-14 | leg 1 = case 16 input with the same `--run-id`; leg 2 = record 3 replaced by a new id, relaunch same `--run-id` | leg 1: RC 231, 1 × 0100, SYSOUT as case 16. Final: oracle fresh run on the fixed file: RC 4; 1 × 0100; 4 / 1; 3 rows | leg 1: RC 16, =, =. Final: RC 4; DALYREJS =; tables =; last 3 SYSOUT lines (counters + end) = | PASS |
| 18 | `rerun` | FR-11, PR-14 | same DALYTRAN, two new instances on the same tables (accts 36/37) | run 1: RC 4, 1 × 0100, 3 / 1, 2 rows; run 2: RC 4, 0102 + 0100 (acct 36 now over limit), 3 / 2, 1 row (TRANSACT re-created) | run 1 =; run 2 = | PASS |
| 19 | `dalytran-missing` | FR-13, PR-01/17 | `--dd=DALYTRAN` names a file that does not exist | RC 231; SYSOUT `ERROR OPENING DALYTRAN` / `FILE STATUS IS: NNNN0035` / `ABENDING PROGRAM`; DALYREJS never created; TRANSACT never opened | RC 16; SYSOUT =; no DALYREJS; stale TRANSACT row kept | PASS |

Mock data: every card / account key is a real sample key (`app/data/ASCII/cardxref.txt` accounts 1-37); only the fields under
test were changed in the account / TCATBAL override files of the case. `4000000000000001` is used only where a card that is
*not* in CARDXREF is the point of the case (reason 0100). Account 13 is dropped from ACCTDATA for reason 0101.

## 3. Failures (diff + COBOL cite)

### DV-01 – negative zero amount loses its sign in TRANSACT (`negative-zero`)

- Reproduce: `(cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B test -pl carddemo-app -am -Dtest=Cbtrn02cIndependentParityTest -Dsurefire.failIfNoSpecifiedTests=false)`
  and remove the `divergence.*` lines from `parity/cbtrn02c/negative-zero/case.properties`.
- COBOL: `MOVE DALYTRAN-AMT TO TRAN-AMT` (`app/cbl/CBTRN02C.cbl:430`), both `PIC S9(09)V99` (`CVTRA06Y`, `CVTRA05Y.cpy:10`). GnuCOBOL keeps the
  negative sign of -0.00. FR-POSTTRAN-09: "all DALYTRAN fields copied unchanged".
- Diff (TRANSACT record 1, cols 133-143 = TRAN-AMT; everything else equal):

```
expected NEGZERO000000001010001POS TERM  Purchase at Abshire-Lowe   …   0000000000}800000000Abshire-Lowe …
actual   NEGZERO000000001010001POS TERM  Purchase at Abshire-Lowe   …   0000000000{800000000Abshire-Lowe …
```

- Same-case observations that match: ACCTDATA (the `IF DALYTRAN-AMT >= 0` branch, `cbl:548`, took CYC-CREDIT on both sides),
  TCATBALF, DALYREJS, SYSOUT, RC 0.
- Likely cause (inference, not verified in code): the Java amount is a `BigDecimal`, which has no signed zero, so the zoned encoder
  writes `{` (+0).

### DV-02 – TCATBAL overflow to zero keeps a negative sign in COBOL (`ovf-tcatbal`)

- Reproduce: as DV-01 with `parity/cbtrn02c/ovf-tcatbal/case.properties`.
- COBOL: `ADD DALYTRAN-AMT TO TRAN-CAT-BAL` without `ON SIZE ERROR` (`app/cbl/CBTRN02C.cbl:527`), `TRAN-CAT-BAL PIC S9(09)V99`
  (`CVTRA01Y.cpy:9`). -999999999.99 + (-0.01) = -1000000000.00; the high-order digit is dropped (D-047) and the result is -0.00.
- Diff (TCATBALF key `00000000028010001`, cols 18-28 = TRAN-CAT-BAL; the other 50 rows equal):

```
expected 000000000280100010000000000}
actual   000000000280100010000000000{
```

- The other overflow results of the same case match (key 27: 999999999.99 + 1.00 → `0000000009I` = +0.99; key 29/01/0009 created
  with 999999999.99), as do the 3 other D-047 cases (9, 11, 12): D-047 truncation itself is at parity; only the sign of a zero
  result differs.
- Likely cause (inference): same as DV-01.

Both divergences are confined to the sign nibble of a zero amount. The test keeps them visible without turning CI red: a case
with `divergence.artifact=<artefact>` passes only while the mismatch exists and is confined to that artefact (D-051); once the
implementation is fixed the case fails and the property must be removed.

## 4. Requirement coverage

Program FR doc (`CBTRN02C_functional_requirement.md`, 17) and stream FR doc (`POSTTRAN_functional_requirements.md`, 17 + 4 NFR).
Each requirement appears once.

| Requirement | Cases | Outcome |
|---|---|---|
| PR-CBTRN02C-01 start line, open, open failure abend | all (START line), 19 | PASS (DALYTRAN open failure only; the other 5 files: UNTESTED, see §5) |
| PR-CBTRN02C-02 read to EOF, count | 1, 2, 7, 10 | PASS (read failure other than EOF: UNTESTED) |
| PR-CBTRN02C-03 reset reason / description | 5 (0100 after 0103) | PASS |
| PR-CBTRN02C-04 xref lookup, 100 | 5, 15, 16, 18 | PASS |
| PR-CBTRN02C-05 account lookup, 101 | 5 | PASS |
| PR-CBTRN02C-06 over-limit 102 | 1, 3, 5, 6, 9, 12, 18 | PASS |
| PR-CBTRN02C-07 expiry 103 over 102 | 4, 5 | PASS |
| PR-CBTRN02C-08 reject record + count | 1, 3-6, 15-19 | PASS (write failure: UNTESTED) |
| PR-CBTRN02C-09 build transaction, PROC-TS | all posting cases | **FAIL** (DV-01) |
| PR-CBTRN02C-10 TCATBAL create / rewrite | 6, 8, 13, 16 | **FAIL** (DV-02); create vs rewrite and SYSOUT lines PASS (8) |
| PR-CBTRN02C-11 account update | 6, 11, 12, 14 | PASS |
| PR-CBTRN02C-12 insert transaction, duplicate abend | 15, 16 | PASS |
| PR-CBTRN02C-13 order + one DB transaction | 16 | PASS against D-042 `expected-atomic` (COBOL itself half-posts, see §6) |
| PR-CBTRN02C-14 new instance empties TRANSACT, restart keeps | all (stale row), 17, 18, 19 | PASS |
| PR-CBTRN02C-15 close, close failure abend | - | UNTESTED |
| PR-CBTRN02C-16 counters, RC 4/0, end line | all | PASS |
| PR-CBTRN02C-17 abend U999 | 16, 17, 19 | PASS (231 ≡ 16) |
| FR-POSTTRAN-01 one step, sequential, file order | 1, 6 | PASS |
| FR-POSTTRAN-02 0100 | 5, 15 | PASS |
| FR-POSTTRAN-03 0101 | 5 | PASS |
| FR-POSTTRAN-04 0102 | 3, 6, 9, 12 | PASS (FR text discrepancy, §6) |
| FR-POSTTRAN-05 0103 | 4, 5 | PASS |
| FR-POSTTRAN-06 430-byte reject record | 1, 3-6, 15-18 | PASS |
| FR-POSTTRAN-07 TCATBAL add / create | 8, 13 | **FAIL** (DV-02) |
| FR-POSTTRAN-08 account balances | 6, 11, 12, 14 | PASS |
| FR-POSTTRAN-09 transaction copied unchanged | 14 | **FAIL** (DV-01) |
| FR-POSTTRAN-10 atomic posting | 16 | PASS (D-042) |
| FR-POSTTRAN-11 new instance / restart | 17, 18 | PASS |
| FR-POSTTRAN-12 RC 4 / 0 | all | PASS |
| FR-POSTTRAN-13 failure → exit 16 | 16, 19 | PASS |
| FR-POSTTRAN-14 SYSOUT lines | all | PASS |
| FR-POSTTRAN-15 empty DALYTRAN | 2 | PASS |
| FR-POSTTRAN-16 ASCII / CP037 | 7, 10 | PASS |
| FR-POSTTRAN-17 DALYREJS new file per run | - | UNTESTED |
| NFR-01 commit 1, restart | 17 | PASS |
| NFR-02 BigDecimal, never double | - | UNTESTED (code property, not observable) |
| NFR-03 byte/row parity | all | **FAIL** (DV-01, DV-02) |
| NFR-04 card masking in logs | - | UNTESTED |

Count: 38 requirements; 29 PASS, 4 FAIL (PR-09, PR-10, FR-07, FR-09; NFR-03 follows), 5 UNTESTED (PR-15, FR-17, NFR-02, NFR-04, plus
the partial gaps noted in PR-01/02/08). This is a requirement count, not a parity percentage.

## 5. Untested

| Item | Reason |
|---|---|
| Open failure of TRANSACT / CARDXREF / ACCTDATA / TCATBALF / DALYREJS (PR-01) | Java has no file to make unopenable (tables); a DB outage has no GnuCOBOL equivalent to produce an oracle |
| DALYTRAN read failure other than EOF (PR-02), DALYREJS write failure (PR-08), close failure (PR-15) | no input that makes GnuCOBOL return such a status without fault injection in the runtime |
| FR-POSTTRAN-17 (refuse existing DALYREJS) | JCL `DISP=(NEW,CATLG,DELETE)` allocation, outside CBTRN02C (before its first statement); no oracle |
| NFR-02, NFR-04 | not observable through DALYREJS / tables / RC |
| Reason 109 (account REWRITE INVALID KEY, D-027) | cannot occur: the account was read in the same record; no input reaches it |

## 6. FR doc vs COBOL discrepancies and notes

1. **FR-POSTTRAN-04 says "exact decimal arithmetic, scale 2"; the COBOL computes into `WS-TEMP-BAL PIC S9(09)V99` (`cbl:187,403-405`) with no
   `ON SIZE ERROR`.** Case 9: CYC-CREDIT 1000000000.00 + 5.00 truncates to 5.00 and the record **posts** in COBOL; exact arithmetic
   would reject it with 0102. The Java job follows the COBOL (D-047), so the case passes; the FR text should say "COBOL `S9(09)V99`
   truncation (D-047)".
2. FR-POSTTRAN-09 "copied unchanged" is right; the Java side does not meet it for -0.00 (DV-01).
3. PR-CBTRN02C-13 / FR-POSTTRAN-10 describe the target (one DB transaction per record, D-026). COBOL half-posts on the abending
   record: the full-run oracle tables (`dup-abend-newkey/expected-full/`) contain the TCATBAL key 34/01/0006 and account 34 +20.00
   that the Java job rolls back. Accepted difference under D-042, not counted as FAIL.
4. FR-POSTTRAN-05 and PR-CBTRN02C-07 say the expiry check is a text compare: confirmed by case 4 (`2022/01/01` ≥ `2022-06-10`
   because `/` > `-`; spaces sort low).

## 7. Commands and outputs

Oracle (inputs + expected outputs):

```
$ .migration/tools/cbtrn02c_parity_oracle.sh ~/work/po
limit-exact RC=4 rejects=1
expiry-boundary RC=4 rejects=3
reason-precedence RC=4 rejects=6
same-account-multi RC=4 rejects=3
tcatbal-create-rewrite RC=0 rejects=0
ovf-temp-bal RC=0 rejects=0
ovf-acct-bal RC=0 rejects=0
ovf-cyc RC=0 rejects=0
ovf-tcatbal RC=0 rejects=0
negative-zero RC=0 rejects=0
dup-rejected-id RC=4 rejects=2
baseline RC=4 rejects=38
empty RC=0 rejects=0
dup-abend-newkey RC=231 rejects=1
restart-fixed RC=4 rejects=1
rerun RC=4 rejects=1
rerun run2 RC=4
same-account-multi-cp037 RC=4 rejects=3
ovf-temp-bal-cp037 RC=0 rejects=0
dalytran-missing RC=231 DALYREJS created: no TRANSACT created: no
```

Java parity test, before the divergence markers were added (raw outcome):

```
$ (cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B test -pl carddemo-app -am -Dtest=Cbtrn02cIndependentParityTest -Dsurefire.failIfNoSpecifiedTests=false)
[ERROR] Tests run: 19, Failures: 2, Errors: 0, Skipped: 0
[ERROR]   Cbtrn02cIndependentParityTest.javaPosttranMatchesGnuCobol:87->assertTables:163 [ovf-tcatbal expected TCATBALF]
[ERROR]   Cbtrn02cIndependentParityTest.javaPosttranMatchesGnuCobol:87->assertTables:163 [negative-zero expected TRANSACT]
```

Java obtained summary (from `target/parity-actual/cbtrn02c/<case>/<run>/`, written by the test: RC, DALYREJS, SYSOUT, table unloads):

```
baseline|expected|RC=4|rej=38|PROCESSED:000000300 REJECTED:000000038|tran=262
dalytran-missing|expected|RC=16|rej=-|tran=1 (stale row kept)
dup-abend-newkey|expected|RC=16|rej=1|tables=expected-atomic, tran=1
dup-rejected-id|expected|RC=4|rej=2|PROCESSED:000000004 REJECTED:000000002|tran=2
empty|expected|RC=0|rej=0|PROCESSED:000000000 REJECTED:000000000|tran=0
expiry-boundary|expected|RC=4|rej=3|PROCESSED:000000007 REJECTED:000000003|tran=4
limit-exact|expected|RC=4|rej=1|PROCESSED:000000004 REJECTED:000000001|tran=3
negative-zero|expected|RC=0|rej=0|PROCESSED:000000001 REJECTED:000000000|tran=1
ovf-acct-bal|expected|RC=0|rej=0|PROCESSED:000000002 REJECTED:000000000|tran=2
ovf-cyc|expected|RC=0|rej=0|PROCESSED:000000002 REJECTED:000000000|tran=2
ovf-tcatbal|expected|RC=0|rej=0|PROCESSED:000000003 REJECTED:000000000|tran=3
ovf-temp-bal-cp037|expected|RC=0|rej=0|PROCESSED:000000003 REJECTED:000000000|tran=3
ovf-temp-bal|expected|RC=0|rej=0|PROCESSED:000000003 REJECTED:000000000|tran=3
reason-precedence|expected|RC=4|rej=6|PROCESSED:000000007 REJECTED:000000006|tran=1
rerun|expected-run1|RC=4|rej=1|PROCESSED:000000003 REJECTED:000000001|tran=2
rerun|expected|RC=4|rej=2|PROCESSED:000000003 REJECTED:000000002|tran=1
restart-fixed|expected-leg1|RC=16|rej=1
restart-fixed|expected|RC=4|rej=1|PROCESSED:000000004 REJECTED:000000001|tran=3
same-account-multi-cp037|expected|RC=4|rej=3|PROCESSED:000000007 REJECTED:000000003|tran=4
same-account-multi|expected|RC=4|rej=3|PROCESSED:000000007 REJECTED:000000003|tran=4
tcatbal-create-rewrite|expected|RC=0|rej=0|PROCESSED:000000005 REJECTED:000000000|tran=5
```

Java SYSOUT, `dup-abend-newkey` (oracle identical after dropping `CEE3ABD ABCODE=+0000000999` and 6 `libcob: warning: implicit CLOSE` lines):

```
START OF EXECUTION OF PROGRAM CBTRN02C
TCATBAL record not found for key : 00000000034010006.. Creating.
ERROR WRITING TO TRANSACTION FILE
FILE STATUS IS: NNNN0022
ABENDING PROGRAM
```

Java SYSOUT, `dalytran-missing` (oracle identical after the same filter):

```
START OF EXECUTION OF PROGRAM CBTRN02C
ERROR OPENING DALYTRAN
FILE STATUS IS: NNNN0035
ABENDING PROGRAM
```

Full build: see §8.

## 8. Full build

```
$ (cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify)
[INFO] Tests run: 21, Failures: 0, Errors: 0, Skipped: 0        (carddemo-common)
[INFO] Tests run: 74, Failures: 0, Errors: 0, Skipped: 0        (carddemo-app: 55 existing + 19 Cbtrn02cIndependentParityTest)
[INFO] CardDemo modernization ............................. SUCCESS
[INFO] CardDemo common .................................... SUCCESS
[INFO] CardDemo app ....................................... SUCCESS
```

`negative-zero` and `ovf-tcatbal` are green only because of their `divergence.*` markers (D-051): they assert that DV-01 / DV-02
still reproduce. They are FAIL in this report.

## 9. Hand-back to `!mf_program_migration`

| Id | Case | Requirement | Cite | Fix owner |
|---|---|---|---|---|
| DV-01 | `negative-zero` | FR-POSTTRAN-09, PR-CBTRN02C-09, NFR-03 | `CBTRN02C.cbl:430` | Wave 1 implementation (not fixed here) |
| DV-02 | `ovf-tcatbal` | FR-POSTTRAN-07, PR-CBTRN02C-10, NFR-03 | `CBTRN02C.cbl:527`, `CVTRA01Y.cpy:9` | Wave 1 implementation (not fixed here) |
| FR-04 text | `ovf-temp-bal` | FR-POSTTRAN-04 | `CBTRN02C.cbl:187,403-405` | FR doc wording (implementation already follows COBOL) |

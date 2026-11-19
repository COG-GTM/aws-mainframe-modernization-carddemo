# CBTRN02C functional requirement (program FR)

Produced by `!mf_program_fr_generation` on 2026-11-19 (session devin-c4fce2ffab2c4abaac2aa02e217e00e4), branch `devin/1795090039-mf-posttran-stream` @ `741f2d2`. Decomposes the approved stream FR `../POSTTRAN_functional_requirements.md` (FR-01..FR-17) using `../POSTTRAN_analysis.md` and the approved plan `../POSTTRAN_migration_plan.md` (STOP C, 2026-11-19). Source `app/cbl/CBTRN02C.cbl` is ASCII; no transcoding needed, line numbers are the repo's. Stream hard stop: `GOBACK` (`:234`). Exclusions: D-010.

## 1. Identity and role

| Item | Value |
|---|---|
| Program | CBTRN02C, `app/cbl/CBTRN02C.cbl` (PROGRAM-ID `:23`) |
| Stream / wave | POSTTRAN (BATCH), **Wave 1** (the only wave; after Phase 0 and Phase 1) |
| Role | entry and step driver, validator, posting logic, reject writer: the whole stream |
| Shared? | **No** (exclusive to BAT-POSTTRAN, inventory §7) |
| Target | Spring Batch job `POSTTRAN`, step `step15Cbtrn02c` in `modernization/carddemo-app` (`com.carddemo.transaction.batch` / `.service`), per plan §2 |

Ownership map: this is the only in-scope program, so it owns **every** stream requirement FR-01..FR-17. No overlap is possible. FR-17 (rerun guard) has no COBOL source; it is a target rule (D-008) implemented in the same job.

## 2. Trigger / caller contract

- Invoked by JCL `POSTTRAN` step `STEP15 EXEC PGM=CBTRN02C` (`app/jcl/POSTTRAN.jcl:23`); no PARM, no SYSIN, no LINKAGE SECTION (PROCEDURE DIVISION without USING, `:193`).
- Target: `java -jar carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=<file> [--dd-encoding=ASCII|CP037] --output-dir=<dir>` (BATCH profile launch seam; BND-008).

## 3. Inputs and outputs (field level; names from analysis §4.2)

| DD | Mode | Layout (copybook) | Fields used by CBTRN02C | Cite |
|---|---|---|---|---|
| DALYTRAN | input sequential | CVTRA06Y 350 | all fields copied to TRAN-RECORD except DALYTRAN-PROC-TS; CARD-NUM (xref key), AMT, ORIG-TS(1:10), TYPE-CD, CAT-CD (balance key) | `:29-32,66-69,102,345-346` |
| XREFFILE | input random | CVACT03Y 50, key XREF-CARD-NUM | XREF-ACCT-ID | `:112,275,382-383` |
| ACCTFILE | I-O random | CVACT01Y 300, key ACCT-ID | read: CREDIT-LIMIT, CURR-CYC-CREDIT, CURR-CYC-DEBIT, EXPIRAION-DATE; written: CURR-BAL, CURR-CYC-CREDIT, CURR-CYC-DEBIT | `:121,311,394-420,545-554` |
| TCATBALF | I-O random | CVTRA01Y 50, key ACCT-ID+TYPE-CD+CAT-CD | TRAN-CAT-BAL | `:126,329,467-542` |
| TRANFILE | output | CVTRA05Y 350, key TRAN-ID | all fields | `:107,256,425-438,564` |
| DALYREJS | output | 430 = DALYTRAN image 350 + WS-VALIDATION-FAIL-REASON 9(4) + WS-VALIDATION-FAIL-REASON-DESC X(76) | all | `:81-84,176-182,446-451` |
| SYSOUT | listing | DISPLAY lines | counts, `Creating.` line, error lines | `:194,227-228,232,476-477,708,721,725` |
| RETURN-CODE | to scheduler | 0 / 4 / abend U999 (target 16) | | `:229-231,707-711` |

## 4. Functional requirements owned

| ID | Business trigger | Observable result | Source | Stream FR |
|---|---|---|---|---|
| CBTRN02C-01 | record passes card, account, limit and expiry checks | TRANSACT row with input fields copied, PROC-TS = run time | `:211-212,424-438,562-564` | FR-01 |
| CBTRN02C-02 | posted amount A | ACCT-CURR-BAL += A | `:547,554` | FR-02 |
| CBTRN02C-03 | A >= 0 / A < 0 | CURR-CYC-CREDIT += A / CURR-CYC-DEBIT += A | `:548-552` | FR-03 |
| CBTRN02C-04 | balance row exists for XREF-ACCT-ID + TYPE-CD + CAT-CD | TRAN-CAT-BAL += A | `:469-474,526-528` | FR-04 |
| CBTRN02C-05 | no balance row for that key | new row, balance = A; SYSOUT `TCATBAL record not found for key : <17-byte key>.. Creating.` | `:474-478,503-510` | FR-05 |
| CBTRN02C-06 | card not in XREF | reject `0100` `INVALID CARD NUMBER FOUND`; no master change | `:380-387` | FR-06 |
| CBTRN02C-07 | XREF found, account missing | reject `0101` `ACCOUNT RECORD NOT FOUND`; no master change | `:393-399` | FR-07 |
| CBTRN02C-08 | CREDIT-LIMIT < CYC-CREDIT - CYC-DEBIT + A (in S9(9)V99, D-014) | reject `0102` `OVERLIMIT TRANSACTION` | `:403-412` | FR-08 |
| CBTRN02C-09 | EXPIRAION-DATE < ORIG-TS(1:10) | reject `0103` `TRANSACTION RECEIVED AFTER ACCT EXPIRATION` | `:414-419` | FR-09 |
| CBTRN02C-10 | both 102 and 103 conditions | single reject with `0103` | `:407-420` | FR-10 |
| CBTRN02C-11 | card unknown | account not read; reason `0100` | `:371-376` | FR-11 |
| CBTRN02C-12 | any reject | DALYREJS record = input 350 bytes + reason 9(4) + text padded to 76, input order | `:446-451` | FR-12 |
| CBTRN02C-13 | end of input | SYSOUT `TRANSACTIONS PROCESSED :nnnnnnnnn` and `TRANSACTIONS REJECTED  :nnnnnnnnn` | `:206,214,227-228` | FR-13 |
| CBTRN02C-14 | rejects > 0 / = 0 | RC 4 / RC 0 | `:229-231` | FR-14 |
| CBTRN02C-15 | empty DALYTRAN | 0/0, empty DALYREJS published, RC 0, no master change | `:202-219,351-361` | FR-15 |
| CBTRN02C-16 | a valid record repeats a TRAN-ID posted earlier in the run | abend, RC 16; earlier records stay committed (D-011); failing record leaves no partial update (D-009); DALYREJS not published (D-012) | `:562-578,707-711` | FR-16 |
| CBTRN02C-17 | any input TRAN-ID already in TRANSACT before the run | RC 16 before any read-for-posting or write; no change (D-008) | target rule, no COBOL source (`:256` context) | FR-17 |

**17 program requirements; coverage 17/17 stream FRs both ways.**

## 5. Business rules and validations

| Rule | Behavior | Cite |
|---|---|---|
| Validation order: XREF, then account (limit, then expiry) | 102 and 103 are both evaluated; the later one wins | `:370-378,393-422` |
| Over-limit value | `WS-TEMP-BAL = CYC-CREDIT - CYC-DEBIT + A`, S9(9)V99, high-order truncation reproduced (D-014) | `:187,403-405` |
| Expiry | 10-char ISO text compare, implemented as DATE compare (D-013) | `:414` |
| Rejects | reject-and-continue (warning); counted; RC 4 at end | `:213-215,229-231` |
| Posting order inside one record | TCATBAL, ACCOUNT, TRANSACT; in the target all three commit or roll back together (D-009) | `:440-442` |
| Abend | any unexpected file status (blocking) | `:707-711` |

## 6. Data access and boundaries (all DECIDED at STOP C)

| BND | Class | Resource / access | Decided mechanism (plan §3) | Error / timeout contract |
|---|---|---|---|---|
| 001 | B11 | CEE3ABD | typed `PostingAbendException` -> step FAILED -> exit 16; SYSOUT error line + `ABENDING PROGRAM` | no retry, no skip |
| 002 | B7 | DALYTRAN, sequential | `RecordReader` + `DailyTransactionLayout` (CVTRA06Y), ASCII or CP037 | read error -> abend; EOF ends loop |
| 003 | B7 | DALYREJS, write | `RejectFileWriter` to temp file in the generation dir, renamed on RC 0/4 only (D-012) | write error -> abend |
| 004 | B10 | `transaction.transaction`, insert, PK `tran_id` | `TransactionRepository` (JPA); append-only; rerun guard (D-008) | duplicate -> RC 16 |
| 005 | B10 | `account.account`, read + update, PK `acct_id`, `@Version` | `AccountRepository` in the record's transaction | lock failure -> RC 16, no retry |
| 006 | B10 | `reference.tran_cat_balance`, read + insert/update, composite PK, `@Version` | repository upsert in the record's transaction | conflict -> RC 16 |
| 007 | B10 | `card.card_xref`, read-only, PK `card_num` | `card.repository` | not found -> reject 100 |
| 008 | B9 | scheduler | `BatchCli` exit code + `RETURN-CODE` file | 0/4 success, 12 usage, 16 failure |
| 009 | B8 | online co-writers | deferred (no production coexistence) | n/a |

Transaction behavior: one record = one DB transaction, commit interval 1 (D-009, D-011). No stored procedures (VSAM only). No undecided boundary: **ready for Wave 1**.

## 7. Error and edge behavior

- Business errors: reasons 0100-0103 (§4).
- Empty input: CBTRN02C-15. Single record, all rejected: RC 4, no master change.
- Reason 0109 (`:555-558`): **dead** after the successful READ at `:395` in the same record; not implemented as a reject. If the target's update finds no row, it is an abend (technical).
- Restart: no COBOL checkpoint. Target restarts from the last committed record (D-002, D-011). Rerun of an already-posted input: CBTRN02C-17.
- Technical (not requirements): open/read/write/rewrite/close failures (`:236-343,345-369,446-465,481-542,562-579,582-690`) -> abend RC 16 with the verbatim `ERROR ...` line and `FILE STATUS IS: NNNN<xxxx>`.
- Timestamps: PROC-TS format `yyyy-MM-dd-HH.mm.ss.SS0000` (`:692-705`); hundredths masked in parity (D-007). Malformed ORIG-TS -> RC 16 (D-013 deviation).

## 8. Hard-stop boundary

CBTRN02C delegates only the abend to LE `CEE3ABD` (`:711`, BND-001). It is **not** responsible for: producing DALYTRAN or consuming DALYREJS (BND-002/003, OQ-5); opening/closing CICS files (CLOSEFIL/OPENFIL, BND-009); TRANSACT backup/reindex (TRANBKP/TRANIDX); interest, reports, statements (INTCALC, TRANREPT, CREASTMT); scheduling (BND-008). Nothing after `GOBACK` (`:234`).

## 9. Acceptance criteria (golden cases from the stream FR §8)

- CBTRN02C-01..05, 12..14: GC-1 (300 sample) RC 4, 262 posted, 38 rejects 0102, DALYREJS sha256 and table unloads equal to the oracle (TRANSACT bytes 325-326 masked).
- CBTRN02C-05, 06, 09, 10, 11: GC-2 `syn_all`: rejects 0100, 0103, 0103; TCATBAL row `00000000007010099` created.
- CBTRN02C-03, 14: GC-3 `syn_ok`: RC 0; credit +504.77, debit -504.70.
- CBTRN02C-16: GC-4 duplicate: RC 16 (oracle 231); tables equal the oracle run on the 2-record valid prefix (R-13); no DALYREJS published; no counter comparison.
- CBTRN02C-15: GC-5 empty: RC 0, 0/0.
- CBTRN02C-07: GC-6 (built in Phase 1) and JT-3.
- CBTRN02C-17: JT-1. CBTRN02C-16 rollback detail: JT-2.

Given/When/Then per requirement: as stream FR §8 for the mapped FR id.

## 10. Open questions and assumptions

- OQ-5 (open, non-blocking): DALYTRAN producer/encoding, DALYREJS consumer.
- GC-6 depends on a Phase 1 oracle extension (custom CARDXREF).
- TRAN FILLER written as spaces (INFERRED; not observable in the oracle unload).
- No gap: every stream FR maps to a program requirement, and no program requirement lacks a stream FR.

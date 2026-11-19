# POSTTRAN functional requirements (stream FR, business sign-off oracle)

Produced by `!mf_stream_fr_generation` on 2026-11-19 (session devin-c4fce2ffab2c4abaac2aa02e217e00e4) from `POSTTRAN_analysis.md` and `app/cbl/CBTRN02C.cbl` (ASCII source, so no transcoding was needed and line numbers are the repo's). Branch `devin/1795090039-mf-posttran-stream`. Per-program FRs come later (`!mf_program_fr_generation`, after STOP C).

## 1. Purpose and scope

POSTTRAN posts the day's card transactions. For each record of the daily transaction file it checks that the card is known, the account exists, the account would stay within its credit limit and the account had not expired when the transaction happened. A valid transaction is added to the transaction master and its amount is added to the account balances and to the account's balance for that transaction type and category. An invalid transaction is written to the daily rejects file with a reason code. The job ends with a count of processed and rejected records and a return code that tells the scheduler whether anything was rejected.

- Process type: **BATCH**. Trigger: job POSTTRAN, step STEP15 `EXEC PGM=CBTRN02C` (`app/jcl/POSTTRAN.jcl:23`), scheduled by CA-7 after CBPAUP0J (`app/scheduler/CardDemo.ca7:68-79`).
- In scope: CBTRN02C from its PROCEDURE DIVISION (`app/cbl/CBTRN02C.cbl:193`) to the **hard stop** `GOBACK` (`:234`).
- Exclusions (D-010): CBTRN01C; CLOSEFIL, CBPAUP0J, WAITSTEP, OPENFIL; TRANBKP, TRANIDX, COMBTRAN; INTCALC, TRANREPT, CREASTMT, CBEXPORT; online COBIL00C and COTRN02C; the producer of DALYTRAN and any consumer of DALYREJS; CEE3ABD (becomes RC 16).

## 2. Actors and preconditions

- Actor: the batch scheduler (CA-7 today, an external scheduler in the target, BND-008). No human interaction.
- Preconditions: the daily transaction file exists (DALYTRAN, `POSTTRAN.jcl:30-31`); the card cross-reference, account master, transaction-category-balance master and transaction master are available (`:28-29,32-33,39-42`); online CICS access to these files is closed (CLOSEFIL, BND-009).
- No parameters or control cards (`POSTTRAN.jcl:23`).

## 3. Surface specification (BATCH)

| Dataset | Direction | Layout | Notes |
|---|---|---|---|
| DALYTRAN | input, sequential | CVTRA06Y, 350 bytes (`app/cpy/CVTRA06Y.cpy:4-18`): ID X16, TYPE-CD X2, CAT-CD 9(4), SOURCE X10, DESC X100, AMT S9(9)V99, MERCHANT-ID 9(9), MERCHANT-NAME X50, MERCHANT-CITY X50, MERCHANT-ZIP X10, CARD-NUM X16, ORIG-TS X26, PROC-TS X26, FILLER X20 | read to end of file |
| DALYREJS | output, new generation | 430 bytes: the 350-byte input record unchanged + reason code 9(4) + reason text X(76) (`CBTRN02C.cbl:176-182`) | one record per rejected input, input order |
| TRANSACT | output (insert) | CVTRA05Y, 350 bytes (`app/cpy/CVTRA05Y.cpy:4-18`), key TRAN-ID | one row per posted input |
| ACCTDATA | read + update | CVACT01Y, 300 bytes (`app/cpy/CVACT01Y.cpy:4-17`), key ACCT-ID | balances CURR-BAL, CURR-CYC-CREDIT, CURR-CYC-DEBIT |
| TCATBALF | read + insert/update | CVTRA01Y, 50 bytes (`app/cpy/CVTRA01Y.cpy:4-10`), key ACCT-ID + TYPE-CD + CAT-CD | TRAN-CAT-BAL |
| CARDXREF | read | CVACT03Y, 50 bytes (`app/cpy/CVACT03Y.cpy:4-8`), key CARD-NUM | gives ACCT-ID |
| SYSOUT | listing | `TRANSACTIONS PROCESSED :nnnnnnnnn`, `TRANSACTIONS REJECTED  :nnnnnnnnn` (`:227-228`) | |
| Return code | to scheduler | 0, 4, or abend (target RC 16) | `:229-231,707-711` |

Restart and rerun: CBTRN02C has no checkpoint. The target rules are D-002 (restart from the last committed unit) and D-008 (an already-posted input abends RC 16 before posting).

## 4. Functional requirements

Covering tests name the golden cases of §8 (GC-n, GnuCOBOL parity) or a Java test (JT-n) where parity cannot apply.

| ID | Flow | Business trigger | Observable result | Program | Source | BND | Covering test |
|---|---|---|---|---|---|---|---|
| FR-01 | Post valid transaction | card known, account found, within limit, not expired | one transaction-master row: ID, type, category, source, description, amount, merchant id/name/city/zip, card number and original timestamp copied from the input; processing timestamp = run time (§6) | CBTRN02C | `:211-212,424-438,562-564` | 004 | GC-1, GC-2, GC-3 |
| FR-02 | Account balance | a transaction is posted | account current balance increases by the amount (decreases if negative) | CBTRN02C | `:545-547,554` | 005 | GC-1, GC-3 |
| FR-03 | Cycle credit vs debit | a posted amount is zero or positive / negative | zero or positive: added to current-cycle credit; negative: added to current-cycle debit (the debit total becomes more negative) | CBTRN02C | `:548-552` | 005 | GC-3 |
| FR-04 | Category balance update | posted; a balance row exists for account + type + category | that row's balance increases by the amount | CBTRN02C | `:467-474,495-498,526-528` | 006 | GC-1 |
| FR-05 | Category balance creation | posted; no balance row for account + type + category | a new row is created with balance = amount; SYSOUT shows `TCATBAL record not found for key : <key>.. Creating.` | CBTRN02C | `:474-478,503-510` | 006 | GC-2 |
| FR-06 | Reject: unknown card | card number not in the cross-reference | reject record with reason `0100 INVALID CARD NUMBER FOUND`; no master changes | CBTRN02C | `:380-387,213-215` | 007 | GC-2 |
| FR-07 | Reject: account missing | card known but its account does not exist | reject record `0101 ACCOUNT RECORD NOT FOUND`; no master changes | CBTRN02C | `:393-399` | 005 | GC-6 (to build), JT-3 |
| FR-08 | Reject: over limit | credit limit < current-cycle credit - current-cycle debit + amount | reject record `0102 OVERLIMIT TRANSACTION`; no master changes | CBTRN02C | `:403-412` | 005 | GC-1 (38 cases) |
| FR-09 | Reject: account expired | account expiration date (yyyy-mm-dd) is earlier than the transaction's original date (first 10 characters of ORIG-TS) | reject record `0103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION`; no master changes | CBTRN02C | `:414-419` | 005 | GC-2 |
| FR-10 | Reason precedence | both over limit and expired | the reject shows reason 103 only (one reject record) | CBTRN02C | `:407-420` | - | GC-2 |
| FR-11 | Card check first | card unknown | the account is not looked at; reason is 100 even if other conditions would fail | CBTRN02C | `:371-376` | - | GC-2 |
| FR-12 | Reject record content and order | any reject | the reject record holds the input record byte for byte followed by the 4-digit reason and its text padded to 76; rejects appear in input order | CBTRN02C | `:446-451,176-182` | 003 | GC-1, GC-2 (byte compare) |
| FR-13 | Run totals | end of input | SYSOUT shows processed count and rejected count (9 digits, zero-padded) | CBTRN02C | `:206,214,227-228` | - | GC-1..5 |
| FR-14 | Return code | at least one reject / no rejects | RC 4 / RC 0 | CBTRN02C | `:229-231` | 008 | GC-1 (4), GC-3 (0) |
| FR-15 | Empty input | daily file has no records | 0 processed, 0 rejected, empty rejects file, RC 0, no master changes | CBTRN02C | `:202-219,351-361` | 002 | GC-5 |
| FR-16 | Duplicate transaction id in the input | a valid record repeats an id already posted in this run | the job abends (target RC 16) at that record; records before it stay posted; that record leaves no partial update in the target (D-009) | CBTRN02C | `:562-578,707-711` | 004, 001 | GC-4 (RC + prior state), JT-2 |
| FR-17 | Already-posted input (rerun) | a **new run** (new job instance, not a restart of a failed one) whose input has an id already in the transaction master | RC 16 before posting; nothing changes (D-008) | target rule D-008 | D-008; `:256` | 004 | JT-1 (Java only, excluded from parity) |

**17 requirements** (15 source-derived + FR-16 partly and FR-17 fully from the STOP B decisions D-008/D-009). Sign-off set: FR-01..FR-17.

## 5. Validation and error catalogue

| Code | Trigger | Cite | Text (in source) | Blocking? | Resulting state |
|---|---|---|---|---|---|
| 0100 | card not in cross-reference | `:384-387` | `INVALID CARD NUMBER FOUND` | warning (reject and continue) | record in DALYREJS; masters unchanged; RC 4 at end |
| 0101 | account of the card not found | `:396-399` | `ACCOUNT RECORD NOT FOUND` | warning | same |
| 0102 | over credit limit | `:407-412` | `OVERLIMIT TRANSACTION` | warning | same |
| 0103 | after account expiration | `:414-419` | `TRANSACTION RECEIVED AFTER ACCT EXPIRATION` | warning | same |
| 0109 | account rewrite finds no record | `:555-558` | `ACCOUNT RECORD NOT FOUND` | **dead in practice**: the record was read moments before by the same run (`:395`); if reached, no reject is written, the transaction is still written (`:442`) and RC is unaffected | demoted (§7) |
| U999 / RC 16 | any unexpected file status on open, read, write, rewrite or close, including duplicate key `22` | `:247-689,707-711` | `ERROR ... ` line, `ABENDING PROGRAM`, `FILE STATUS IS: NNNN<xxxx>` | **blocking** (abend) | FR-16; technical causes demoted |

All message texts are literals in CBTRN02C (no external message table).

## 6. Field and data derivations

| Produced field | Derivation | Cite |
|---|---|---|
| TRAN-ID .. TRAN-ORIG-TS | copied from the same DALYTRAN field | `:425-436` |
| TRAN-PROC-TS | run time as `YYYY-MM-DD-HH.MM.SS.hh0000` (hundredths then `0000`); DALYTRAN-PROC-TS is ignored | `:437-438,692-705` |
| TRAN FILLER | not moved by CBTRN02C; the oracle unload is line-sequential and trims trailing spaces (330-byte lines), so its content is not observed. Target writes spaces (INFERRED) | CVTRA05Y:18 |
| ACCT-CURR-BAL | + amount | `:547` |
| ACCT-CURR-CYC-CREDIT / -DEBIT | + amount when amount >= 0 / when < 0 | `:548-552` |
| TRAN-CAT-BAL (existing) | + amount | `:527` |
| TRAN-CAT-BAL (new row) | = amount; key = XREF-ACCT-ID + DALYTRAN-TYPE-CD + DALYTRAN-CAT-CD | `:503-508` |
| Over-limit test value | CYC-CREDIT - CYC-DEBIT + amount, held in S9(9)V99 (high-order truncation possible) | `:187,403-405` |
| Reject trailer | reason 9(4) + text X(76) | `:180-182,448` |
| Counts | +1 per record read / per reject | `:206,214` |

## 7. Mechanics (demoted, cited)

- Six files opened at start (`:195-200`); TRANSACT opened OUTPUT (`:256`), see D-008; open/read/close failures abend (`:236-343,582-690`). Technical failures, not requirements.
- Main loop and EOF handling (`:202-219,345-369`).
- Posting order TCATBAL, ACCOUNT, TRANSACT (`:440-442`); COBOL leaves partial updates on a later abend, the target does not (D-009).
- No checkpoint, no commit logic (VSAM immediate update). Target restart semantics D-002.
- Start/end SYSOUT banners (`:194,232`): formatting.
- Commented-out DISPLAYs (`:381,389,401-402,449,704`): dead.
- JCL: DALYREJS `DISP=(NEW,CATLG,DELETE)` (`POSTTRAN.jcl:34`), so on z/OS an abend deletes the new rejects generation; GnuCOBOL keeps the file (OQ-6).
- Hard stop: `GOBACK` (`:234`). Nothing after it is specified.

## 8. Acceptance criteria and golden cases

Golden cases (GnuCOBOL oracle, runbook R-04/R-06/R-11/R-12; parity bar D-004 with the D-007 mask and D-009 abend rule):
- **GC-1** repo sample (300 records): RC 4, 262 posted, 38 rejected all 0102.
- **GC-2** synthetic `syn_all` (6 records): RC 4; rejects 0100, 0103, 0103 (overlimit + expired); TCATBAL row created for key `00000000007010099`.
- **GC-3** synthetic `syn_ok` (2 records): RC 0; credit +504.77, debit -504.70.
- **GC-4** in-file duplicate (3 records, third repeats the first): Java RC 16 (oracle 231); tables equal to the oracle run on the 2-record valid prefix (R-13), not to the oracle abend run (D-009); no DALYREJS published (D-012); no counter comparison (COBOL ends before its count DISPLAYs).
- **GC-5** empty input: RC 0, 0/0, empty DALYREJS.
- **GC-7** CP037 input (EBCDIC sample, R-14): Java reads CP037 directly; outputs equal GC-1, DALYREJS in CP037 equal to the oracle DALYREJS converted to CP037.
- **GC-6** (to build in Phase 1): CARDXREF fixture whose account is missing -> 0101. Needs an oracle option for a custom CARDXREF.
Java-only: **JT-1** rerun guard on a new instance (D-008); **JT-2** atomic rollback of the failing record (D-009); **JT-3** reason 101 unit test; **JT-4** restart of a failed instance skips the guard and resumes from its checkpoint, final outputs equal GC-1 (D-002, D-011; plan §7).

Given/When/Then:
- FR-01: Given a record whose card, account, limit and expiry pass, When POSTTRAN runs, Then TRANSACT has a row with the input fields and a processing timestamp of the run.
- FR-02/03: Given a posted amount A, Then CURR-BAL increases by A, and CYC-CREDIT increases by A if A >= 0, else CYC-DEBIT increases by A.
- FR-04/05: Given a posted record, Then the category-balance row for account+type+category increases by A, or is created with A and the SYSOUT line is written.
- FR-06..09: Given the failing condition, Then DALYREJS holds the input plus that reason and no master changes.
- FR-10: Given over limit and expired, Then the reason is 0103.
- FR-11: Given an unknown card, Then the reason is 0100.
- FR-12: Given N rejects, Then DALYREJS is N x 430 bytes in input order, byte-identical to the oracle.
- FR-13/14: Given R rejects out of P records, Then SYSOUT shows P and R, and RC is 4 if R > 0 else 0.
- FR-15: Given an empty input, Then RC 0, counts 0, empty DALYREJS.
- FR-16: Given a duplicate id later in the input, Then RC 16, the tables equal the oracle run on the records before the duplicate, and no DALYREJS is published.
- FR-17: Given a new run whose input has an id already in TRANSACT, Then RC 16 and no table changes. Given a restart of a failed run of the same instance that had passed the guard, Then the guard is skipped and posting resumes after the last committed record; Given a restart of an instance that the guard itself rejected, Then the guard runs again, RC 16, no table changes (JT-4).

## 9. Traceability matrix

| FR | Program | Source | Test | Verification case |
|---|---|---|---|---|
| FR-01 | CBTRN02C | `:424-438,562-564` | GC-1..3 | TRANSACT unload compare (mask 325-326) |
| FR-02 | CBTRN02C | `:547,554` | GC-1, GC-3 | ACCTDATA unload compare |
| FR-03 | CBTRN02C | `:548-552` | GC-3 | ACCTDATA unload compare |
| FR-04 | CBTRN02C | `:526-528` | GC-1 | TCATBALF unload compare |
| FR-05 | CBTRN02C | `:503-510` | GC-2 | TCATBALF unload + SYSOUT line |
| FR-06 | CBTRN02C | `:380-387` | GC-2 | DALYREJS byte compare |
| FR-07 | CBTRN02C | `:393-399` | GC-6, JT-3 | DALYREJS byte compare |
| FR-08 | CBTRN02C | `:403-412` | GC-1 | DALYREJS byte compare |
| FR-09 | CBTRN02C | `:414-419` | GC-2 | DALYREJS byte compare |
| FR-10 | CBTRN02C | `:407-420` | GC-2 | DALYREJS byte compare |
| FR-11 | CBTRN02C | `:371-376` | GC-2 | DALYREJS byte compare |
| FR-12 | CBTRN02C | `:446-451` | GC-1, GC-2, GC-7 | DALYREJS sha256 (GC-7: in CP037) |
| FR-13 | CBTRN02C | `:227-228` | GC-1..5 | SYSOUT count lines |
| FR-14 | CBTRN02C | `:229-231` | GC-1, GC-3 | process exit code |
| FR-15 | CBTRN02C | `:202-219` | GC-5 | all outputs |
| FR-16 | CBTRN02C | `:562-578,707-711` | GC-4, JT-2 | RC 16 + tables vs valid-prefix oracle run (R-13) + no DALYREJS published |
| FR-17 | (D-008) | `:256` | JT-1, JT-4 | RC + unchanged tables (new instance); resume equals GC-1 (restart) |

Requirements without an existing covering case today: FR-07 (GC-6 must be built). All others have an oracle run already executed in this session.

## 10. Program index

| Program | Role | Requirements |
|---|---|---|
| [CBTRN02C](programs/CBTRN02C_functional_requirement.md) | entry, step driver, validation, posting, reject writer | FR-01..FR-16 (FR-17 is a target-only rule implemented in the same job) |

## 11. Open questions and assumptions

- OQ-5 (open, non-blocking): producer/encoding of DALYTRAN, consumer of DALYREJS.
- OQ-6 (new): on an abend z/OS deletes the DALYREJS generation (`DISP=(NEW,CATLG,DELETE)`), while D-009 compares DALYREJS on the abend path and GnuCOBOL keeps a partial file. Proposed in the plan: on RC 16 the Java job does not publish DALYREJS, and abend-path parity compares RC + table state only.
- Reason 0109 treated as dead (§5).
- The expiry check is a 10-character text comparison; treated as a date comparison only for valid ISO dates (analysis §4.2).
- `WS-TEMP-BAL` truncation (§6) is reproduced, not fixed, unless a decision says otherwise (drift rule 5).

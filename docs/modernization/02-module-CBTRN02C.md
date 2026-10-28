# 2a. Module documentation — CBTRN02C (daily transaction posting)

Source: `app/cbl/CBTRN02C.cbl` (731 lines). Job: `app/jcl/POSTTRAN.jcl` STEP15.
Every statement below cites the line range it was taken from. Where the code is silent, the
question is in `open-questions.md` and referenced as OQ-nn.

## Business purpose (lines 3-6, 189-234)

Reads the day's transaction feed (`DALYTRAN`), validates each transaction against the card
cross-reference and the account master, and for each valid transaction: (a) adds the amount to
the per-account/type/category balance file, (b) adds it to the account's current balance and
current-cycle credit or debit bucket, and (c) writes the transaction into the transaction master.
Invalid transactions are written to a reject file with a 4-digit reason code and description.
The job ends with return code 4 if any transaction was rejected (lines 229-231), otherwise 0.

## Inputs and outputs (FILE-CONTROL lines 28-61; JCL lines 28-42)

| DD | Org / access | Open mode (paragraph, lines) | Copybook layout | LRECL |
|---|---|---|---|---|
| `DALYTRAN` | sequential | INPUT (`0000-DALYTRAN-OPEN`, 236-253) | CVTRA06Y `DALYTRAN-RECORD` | 350 |
| `TRANFILE` | indexed, random, key `FD-TRANS-ID` | **OUTPUT** (`0100-TRANFILE-OPEN`, 254-272) | CVTRA05Y `TRAN-RECORD` | 350 |
| `XREFFILE` | indexed, random, key `FD-XREF-CARD-NUM` | INPUT (273-290) | CVACT03Y `CARD-XREF-RECORD` | 50 |
| `DALYREJS` | sequential | OUTPUT (291-308) | `REJECT-RECORD` (lines 128-132, defined inline) | 430 |
| `ACCTFILE` | indexed, random, key `FD-ACCT-ID` | I-O (309-326) | CVACT01Y `ACCOUNT-RECORD` | 300 |
| `TCATBALF` | indexed, random, key `FD-TRAN-CAT-KEY` | I-O (327-344) | CVTRA01Y `TRAN-CAT-BAL-RECORD` | 50 |

`TRANFILE` is opened OUTPUT, not I-O or EXTEND (line 256). On a VSAM KSDS this replaces the
cluster's contents (requires the cluster to be defined REUSE or empty). Consequence: the master
after POSTTRAN contains only this run's postings. See OQ-02.

## Record layouts

### DALYTRAN-RECORD / TRAN-RECORD (CVTRA06Y / CVTRA05Y, identical shape, 350 bytes)

| Offset | Len | Field (DALYTRAN-/TRAN-) | PIC |
|---|---|---|---|
| 0 | 16 | ID | X(16) |
| 16 | 2 | TYPE-CD | X(02) |
| 18 | 4 | CAT-CD | 9(04) |
| 22 | 10 | SOURCE | X(10) |
| 32 | 100 | DESC | X(100) |
| 132 | 11 | AMT | S9(09)V99 (zoned, trailing overpunch sign) |
| 143 | 9 | MERCHANT-ID | 9(09) |
| 152 | 50 | MERCHANT-NAME | X(50) |
| 202 | 50 | MERCHANT-CITY | X(50) |
| 252 | 10 | MERCHANT-ZIP | X(10) |
| 262 | 16 | CARD-NUM | X(16) |
| 278 | 26 | ORIG-TS | X(26) |
| 304 | 26 | PROC-TS | X(26) |
| 330 | 20 | FILLER | X(20) |

### REJECT-RECORD (lines 128-132; trailer lines 134-139), 430 bytes

| Offset | Len | Field | Source |
|---|---|---|---|
| 0 | 350 | `REJECT-TRAN-DATA` | whole `DALYTRAN-RECORD` (line 447) |
| 350 | 4 | `WS-VALIDATION-FAIL-REASON` PIC 9(04) | 100/101/102/103 |
| 354 | 76 | `WS-VALIDATION-FAIL-REASON-DESC` PIC X(76) | text below |

Other layouts (CARD-XREF-RECORD 50, ACCOUNT-RECORD 300, TRAN-CAT-BAL-RECORD 50) are tabulated
once in `03-data-model.md`.

## Processing flow (lines 189-234)

1. Open the six files (195-200). Any open failure → abend (see error paths).
2. `PERFORM UNTIL END-OF-FILE = 'Y'` (202): read next DALYTRAN (`1000-DALYTRAN-GET-NEXT`,
   345-369; status `10` sets EOF, any other non-`00` status abends).
3. For each record: `ADD 1 TO WS-TRANSACTION-COUNT`, clear reason code/desc to 0/spaces
   (206-209), `PERFORM 1500-VALIDATE-TRAN` (210).
4. Reason 0 → `2000-POST-TRANSACTION`; else `ADD 1 TO WS-REJECT-COUNT` and
   `2500-WRITE-REJECT-REC` (211-216).
5. Close all files, display counts, `MOVE 4 TO RETURN-CODE` if `WS-REJECT-COUNT > 0`, `GOBACK`
   (221-234).

## Validation rules (`1500-*`, lines 370-423)

| Order | Check | Lines | Reason code / description |
|---|---|---|---|
| 1 | `READ XREF-FILE` by `DALYTRAN-CARD-NUM`; `INVALID KEY` | 380-392 | 100 `INVALID CARD NUMBER FOUND` |
| 2 | Only if reason still 0 (372-373): `READ ACCOUNT-FILE` by `XREF-ACCT-ID`; `INVALID KEY` | 393-399 | 101 `ACCOUNT RECORD NOT FOUND` |
| 3 | `WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT`; reject unless `ACCT-CREDIT-LIMIT >= WS-TEMP-BAL` | 403-412 | 102 `OVERLIMIT TRANSACTION` |
| 4 | Reject unless `ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)` (string compare, `YYYY-MM-DD`) | 414-420 | 103 `TRANSACTION RECEIVED AFTER ACCT EXPIRATION` |

Non-obvious rules encoded here:

* **The credit-limit check does not look at `ACCT-CURR-BAL`.** Exposure is cycle credit minus
  cycle debit plus this amount (403-405). An account with a large carried balance but zero cycle
  activity passes any transaction up to its full limit.
* **Equality passes** (`>=`, line 407): a transaction that brings exposure exactly to the limit is
  posted.
* **Checks 3 and 4 both run; 4 overwrites 3.** They are two independent `IF`s (407-420), so a
  transaction that is both over-limit and after expiry is rejected with reason 103, not 102.
* **`WS-TEMP-BAL` is `PIC S9(09)V99`** (line 79) while the operands are `S9(10)V99`. A
  `COMPUTE` into a narrower field truncates high-order digits silently (no `ON SIZE ERROR`).
  Exposure ≥ 1 000 000 000.00 wraps. OQ-03.
* **Expiry compares against the transaction's origination date, not the run date** (line 414),
  and uses `>=`: a transaction originated *on* the expiration day is accepted.
* **Debit vs credit is decided by sign** (2800, lines 548-552): `DALYTRAN-AMT >= 0` → added to
  `ACCT-CURR-CYC-CREDIT`, negative → added (as a negative number) to `ACCT-CURR-CYC-DEBIT`. The
  debit bucket therefore accumulates *negative* values, which then makes the exposure formula
  `CREDIT - DEBIT` larger, not smaller. Whether that is intended is OQ-04.
* Comment `ADD MORE VALIDATIONS HERE` (line 377): validation set is explicitly incomplete.

## Posting (`2000-POST-TRANSACTION`, lines 424-445)

* Field-by-field copy of the 13 named fields from DALYTRAN to TRAN (425-436); the 20-byte
  FILLER is not copied, so it remains whatever `TRAN-RECORD` held (WORKING-STORAGE initial
  value: spaces via copybook `VALUE`? — CVTRA05Y has no VALUE clause; initial content is
  compiler-dependent, OQ-05).
* `TRAN-PROC-TS` = current timestamp in `YYYY-MM-DD-HH.MM.SS.mmm000` built by
  `Z-GET-DB2-FORMAT-TIMESTAMP` (692-705: `FUNCTION CURRENT-DATE`, milliseconds then literal
  `0000`).
* `2700-UPDATE-TCATBAL` (467-544): key = `XREF-ACCT-ID` + `DALYTRAN-TYPE-CD` + `DALYTRAN-CAT-CD`
  (469-471). `READ` status `23` → create path (`INITIALIZE`, set key, `ADD DALYTRAN-AMT TO
  TRAN-CAT-BAL`, `WRITE`; 503-525). Status `00` → `ADD DALYTRAN-AMT TO TRAN-CAT-BAL`,
  `REWRITE` (526-544). Any other status → abend (481-493).
* `2800-UPDATE-ACCOUNT-REC` (545-560): `ADD DALYTRAN-AMT TO ACCT-CURR-BAL`; sign-split as above;
  `REWRITE ... INVALID KEY MOVE 109 ...` (554-558). **Reason 109 is set but never acted on**:
  the caller has already committed to posting, so the transaction is still written to TRANFILE
  (2900) and is not rejected. The account update is silently lost. OQ-06.
* `2900-WRITE-TRANSACTION-FILE` (562-581): `WRITE FD-TRAN-RECORD FROM TRAN-RECORD`; non-`00`
  status (including duplicate key `22`) → abend.

## Reject path (`2500-WRITE-REJECT-REC`, lines 446-466)

Copies the original 350-byte record and the 80-byte trailer into `REJECT-RECORD`, `WRITE`s it;
non-`00` status → abend. The reject file therefore preserves the input bytes verbatim.

## Error and abend paths

| Condition | Paragraph / lines | Message displayed | Outcome |
|---|---|---|---|
| Any OPEN status ≠ `00` | 236-344 (six paragraphs) | `ERROR OPENING <file>` + `FILE STATUS IS: NNNN` | `9999-ABEND-PROGRAM` |
| DALYTRAN READ status ≠ `00`/`10` | 345-369 | `ERROR READING DALYTRAN FILE` | abend |
| XREF/ACCT READ invalid key | 380-399 | (none) | reject 100/101, continue |
| TCATBAL READ status ∉ {`00`,`23`} | 481-493 | `ERROR READING TRANSACTION BALANCE FILE` | abend |
| TCATBAL WRITE/REWRITE ≠ `00` | 512-524, 530-543 | `ERROR (RE)WRITING TRANSACTION BALANCE FILE` | abend |
| ACCT REWRITE invalid key | 554-558 | (none) | reason 109 set, ignored |
| TRANFILE WRITE ≠ `00` | 562-581 | `ERROR WRITING TRANSACTION RECORD` | abend |
| DALYREJS WRITE ≠ `00` | 451-465 | `ERROR WRITING TO REJECTS FILE` | abend |
| Any CLOSE ≠ `00` | 582-691 | `ERROR CLOSING <file>` | abend |
| Any reject | 229-231 | — | `RETURN-CODE 4` |

`9999-ABEND-PROGRAM` (707-712): `DISPLAY 'ABENDING PROGRAM'`, `TIMING=0`, `ABCODE=999`,
`CALL 'CEE3ABD'` → LE user abend U0999, no dump. `9910-DISPLAY-IO-STATUS` (714-729) renders the
2-byte file status as a 4-digit number; for status `9x` the second byte is treated as binary.

**No commit/rollback scope.** VSAM updates to ACCTFILE/TCATBALF made before an abend persist;
a rerun re-applies them. OQ-07.

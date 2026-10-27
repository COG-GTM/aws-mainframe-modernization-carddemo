# CBTRN02C business rules (daily transaction posting)

Program `app/cbl/CBTRN02C.cbl`, run by job POSTTRAN (`app/jcl/POSTTRAN.jcl:23-42`). It reads the daily
transaction file (DALYTRAN), validates each record against the card cross-reference and the account master, and then
either posts it (category balance, account balance, transaction master) or writes it to the rejects file (DALYREJS).

Line numbers refer to `main`. Classes: **calculation**, **validation**, **file I/O**, **control flow**,
**error handling**. Rules marked **SME question** describe behaviour that a port may or may not want to keep; the
port follows the source until an SME decides otherwise.

## Rules

| ID | Class | Rule | Inputs | Outputs | Source |
|----|-------|------|--------|---------|--------|
| POST-01 | file I/O | Files are opened DALYTRAN INPUT (sequential), TRANSACT OUTPUT (indexed by TRAN-ID, random; DD TRANFILE), XREFFILE INPUT (random by card number), DALYREJS OUTPUT (sequential, a new generation), ACCTFILE I-O and TCATBALF I-O (both random), and all six are closed at the end. | DD DALYTRAN, TRANFILE, XREFFILE, DALYREJS, ACCTFILE, TCATBALF | open files | `app/cbl/CBTRN02C.cbl:29-61`; `app/cbl/CBTRN02C.cbl:195-200`; `app/cbl/CBTRN02C.cbl:221-226`; `app/jcl/POSTTRAN.jcl:28-42` |
| POST-02 | control flow | The driver reads DALYTRAN until file status `10`; for each record it adds 1 to the processed count, resets the reason code to 0 and the description to spaces, validates, and posts the record if the reason is still 0, otherwise counts and writes a reject. | DALYTRAN | WS-TRANSACTION-COUNT, WS-REJECT-COUNT | `app/cbl/CBTRN02C.cbl:202-219`; `app/cbl/CBTRN02C.cbl:345-369`; `app/cpy/CVTRA06Y.cpy:4-18` |
| POST-03 | validation | Validation order is cross-reference first, then account; the account checks run only when the cross-reference check passed, and no other validations exist (`ADD MORE VALIDATIONS HERE`). | WS-VALIDATION-FAIL-REASON | (gate) | `app/cbl/CBTRN02C.cbl:370-378` |
| POST-04 | validation | Reason 100: a card number not found in XREFFILE (INVALID KEY) sets reason 100, `INVALID CARD NUMBER FOUND`. | DALYTRAN-CARD-NUM | reason 100, CARD-XREF-RECORD | `app/cbl/CBTRN02C.cbl:380-392`; `app/cpy/CVACT03Y.cpy:4-8` |
| POST-05 | validation | Reason 101: an XREF-ACCT-ID not found in ACCTFILE (INVALID KEY) sets reason 101, `ACCOUNT RECORD NOT FOUND`. | XREF-ACCT-ID | reason 101, ACCOUNT-RECORD | `app/cbl/CBTRN02C.cbl:393-400`; `app/cpy/CVACT01Y.cpy:4-17` |
| POST-06 | validation | Reason 102: when ACCT-CURR-CYC-CREDIT − ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT is greater than ACCT-CREDIT-LIMIT, the record gets reason 102, `OVERLIMIT TRANSACTION`; ACCT-CURR-BAL is not used. | ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT, DALYTRAN-AMT, ACCT-CREDIT-LIMIT | WS-TEMP-BAL, reason 102 | `app/cbl/CBTRN02C.cbl:403-413`; `app/cbl/CBTRN02C.cbl:187` |
| POST-07 | validation | Reason 103: when ACCT-EXPIRAION-DATE is less than the first 10 characters of DALYTRAN-ORIG-TS (alphanumeric compare), reason 103 `TRANSACTION RECEIVED AFTER ACCT EXPIRATION` is set; the check runs after the limit check and overwrites a 102. | ACCT-EXPIRAION-DATE, DALYTRAN-ORIG-TS(1:10) | reason 103 | `app/cbl/CBTRN02C.cbl:414-420`; `app/cpy/CVACT01Y.cpy:11`; `app/cpy/CVTRA06Y.cpy:16` |
| POST-08 | file I/O | A rejected record is written to DALYREJS as 430 bytes: the 350-byte DALYTRAN record followed by an 80-byte trailer of reason code 9(04) and description X(76); a write failure abends. | DALYTRAN-RECORD, WS-VALIDATION-TRAILER | DALYREJS record | `app/cbl/CBTRN02C.cbl:81-84`; `app/cbl/CBTRN02C.cbl:176-182`; `app/cbl/CBTRN02C.cbl:446-465`; `app/jcl/POSTTRAN.jcl:34-38` |
| POST-09 | control flow | After closing, the program DISPLAYs the processed and rejected counts and sets RETURN-CODE 4 when at least one record was rejected; otherwise RETURN-CODE is not set (RC 0). | WS-REJECT-COUNT | RC 0 or 4 | `app/cbl/CBTRN02C.cbl:214`; `app/cbl/CBTRN02C.cbl:227-232` |
| POST-10 | calculation | A posted transaction copies every DALYTRAN field into TRAN-RECORD except the processing timestamp, which is replaced by the current timestamp (POST-18); the input DALYTRAN-PROC-TS is discarded. | DALYTRAN-RECORD | TRAN-RECORD | `app/cbl/CBTRN02C.cbl:424-438`; `app/cpy/CVTRA05Y.cpy:4-18` |
| POST-11 | control flow | **SME question.** Posting updates the category balance, then the account, then writes the transaction, with no rollback: an abend in a later step leaves the earlier updates in place. | TRAN-RECORD | TCATBALF, ACCTFILE, TRANSACT | `app/cbl/CBTRN02C.cbl:440-444` |
| POST-12 | file I/O | The category balance key is (XREF-ACCT-ID, DALYTRAN-TYPE-CD, DALYTRAN-CAT-CD); status `23` on the read creates a new record, `00` updates the existing one, and any other status abends. | XREF-ACCT-ID, DALYTRAN-TYPE-CD, DALYTRAN-CAT-CD | WS-CREATE-TRANCAT-REC | `app/cbl/CBTRN02C.cbl:467-501`; `app/cpy/CVTRA01Y.cpy:4-10` |
| POST-13 | calculation | Create path: the record is INITIALIZEd, the key is set, the balance becomes 0 + DALYTRAN-AMT, and the record is WRITTEN; any status other than `00` abends. | DALYTRAN-AMT | new TCATBALF record | `app/cbl/CBTRN02C.cbl:503-524` |
| POST-14 | calculation | Update path: DALYTRAN-AMT is added to TRAN-CAT-BAL and the record is REWRITTEN; any status other than `00` abends. | DALYTRAN-AMT, TRAN-CAT-BAL | updated TCATBALF record | `app/cbl/CBTRN02C.cbl:526-542` |
| POST-15 | calculation | The account update adds DALYTRAN-AMT to ACCT-CURR-BAL, and also to ACCT-CURR-CYC-CREDIT when the amount is ≥ 0 or to ACCT-CURR-CYC-DEBIT when it is negative (so the debit total accumulates negative values). | DALYTRAN-AMT | ACCT-CURR-BAL, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT | `app/cbl/CBTRN02C.cbl:545-552`; `app/cpy/CVACT01Y.cpy:7`; `app/cpy/CVACT01Y.cpy:13-14` |
| POST-16 | error handling | **SME question.** Reason 109: an INVALID KEY on the account REWRITE sets reason 109, `ACCOUNT RECORD NOT FOUND`, but nothing reads it afterwards, so the transaction is still written, no reject record is produced, the reject count and RC are unchanged, and the reason is reset for the next record. | ACCTFILE REWRITE | reason 109 (not acted on) | `app/cbl/CBTRN02C.cbl:554-560`; `app/cbl/CBTRN02C.cbl:440-442`; `app/cbl/CBTRN02C.cbl:208-209` |
| POST-17 | file I/O | The posted transaction is WRITTEN to TRANSACT keyed by TRAN-ID; any status other than `00` (for example a duplicate key) abends. | TRAN-RECORD | TRANSACT record | `app/cbl/CBTRN02C.cbl:562-579`; `app/cbl/CBTRN02C.cbl:34-38` |
| POST-18 | calculation | The processing timestamp comes from `FUNCTION CURRENT-DATE` per record, formatted `YYYY-MM-DD-HH.MM.SS.hh0000` (26 bytes). | system clock | TRAN-PROC-TS | `app/cbl/CBTRN02C.cbl:692-705`; `app/cbl/CBTRN02C.cbl:149-174` |
| POST-19 | error handling | Any open, read, write, rewrite or close status other than the expected one DISPLAYs a message and the file status, then abends through `CEE3ABD` with code 999; the two validation reads are the exception, as only their INVALID KEY condition is tested. | file status fields | SYSOUT message, abend U999 | `app/cbl/CBTRN02C.cbl:707-727`; `app/cbl/CBTRN02C.cbl:236-252` |

## Notes for the SME

- **POST-06.** CBTRN02C posts debits to ACCT-CURR-CYC-DEBIT as negative amounts (POST-15). `credit − debit` therefore
  adds the magnitude of the cycle's debits. WS-TEMP-BAL is S9(09)V99 while the account fields are S9(10)V99, and the
  COMPUTE has no ON SIZE ERROR. An amount equal to the limit passes (`>=`).
- **POST-07.** This is a byte comparison of two X(10) fields. It is chronological only when both hold `YYYY-MM-DD`.
  A transaction dated on the expiry date passes. Because both checks run in the same `NOT INVALID KEY` block, a
  record that is over limit and also past expiry is rejected with 103 only.
- **POST-04 / POST-05.** XREFFILE-STATUS and ACCTFILE-STATUS are not tested after these two READs. A read error
  other than INVALID KEY does not set a reason code.
- **POST-08.** The reject reason is written as four digits (`0100`, `0101`, `0102`, `0103`). Reason 109 never reaches
  this file (POST-16).
- **POST-16.** The account was read successfully in POST-05 just before this, so the INVALID KEY branch is not
  expected to fire. ACCTFILE-STATUS is not tested after the REWRITE either.
- **POST-17 / POST-01.** TRANSACT is a KSDS opened OUTPUT. On z/OS VSAM that needs an empty cluster (or one
  defined REUSE). How the batch cycle prepares the cluster is outside this program; confirm it on the target.
- **POST-19.** The DALYREJS close-error path displays XREFFILE-STATUS instead of DALYREJS-STATUS
  (`app/cbl/CBTRN02C.cbl:649`). This affects only the message.

## Paragraph map

| Paragraph | Line | Maps to |
|-----------|------|---------|
| `0000-DALYTRAN-OPEN` | 236 | POST-01, POST-19 |
| `0100-TRANFILE-OPEN` | 254 | POST-01, POST-19 |
| `0200-XREFFILE-OPEN` | 273 | POST-01, POST-19 |
| `0300-DALYREJS-OPEN` | 291 | POST-01, POST-19 |
| `0400-ACCTFILE-OPEN` | 309 | POST-01, POST-19 |
| `0500-TCATBALF-OPEN` | 327 | POST-01, POST-19 |
| `1000-DALYTRAN-GET-NEXT` | 345 | POST-02, POST-19 |
| `1500-VALIDATE-TRAN` | 370 | POST-03 |
| `1500-A-LOOKUP-XREF` | 380 | POST-04 |
| `1500-B-LOOKUP-ACCT` | 393 | POST-05, POST-06, POST-07 |
| `2000-POST-TRANSACTION` | 424 | POST-10, POST-11 |
| `2500-WRITE-REJECT-REC` | 446 | POST-08, POST-19 |
| `2700-UPDATE-TCATBAL` | 467 | POST-12, POST-19 |
| `2700-A-CREATE-TCATBAL-REC` | 503 | POST-13, POST-19 |
| `2700-B-UPDATE-TCATBAL-REC` | 526 | POST-14, POST-19 |
| `2800-UPDATE-ACCOUNT-REC` | 545 | POST-15, POST-16 |
| `2900-WRITE-TRANSACTION-FILE` | 562 | POST-17, POST-19 |
| `9000-DALYTRAN-CLOSE` | 582 | POST-01, POST-19 |
| `9100-TRANFILE-CLOSE` | 600 | POST-01, POST-19 |
| `9200-XREFFILE-CLOSE` | 619 | POST-01, POST-19 |
| `9300-DALYREJS-CLOSE` | 637 | POST-01, POST-19 |
| `9400-ACCTFILE-CLOSE` | 655 | POST-01, POST-19 |
| `9500-TCATBALF-CLOSE` | 674 | POST-01, POST-19 |
| `Z-GET-DB2-FORMAT-TIMESTAMP` | 692 | POST-18 |
| `9999-ABEND-PROGRAM` | 707 | POST-19 |
| `9910-DISPLAY-IO-STATUS` | 714 | POST-19 |

The unnamed main line (lines 193-234) is covered by POST-01, POST-02 and POST-09.

## Branch map

Every IF, ELSE, INVALID KEY, NOT INVALID KEY and PERFORM UNTIL in the PROCEDURE DIVISION falls in one row below.

| Lines | Paragraph | Decision | Maps to |
|-------|-----------|----------|---------|
| 202-205 | main line | loop until EOF; read only while not EOF | POST-02 |
| 211-216 | main line | reason 0 posts, else reject | POST-02 |
| 229-231 | main line | rejects > 0 sets RC 4 | POST-09 |
| 239-251 | 0000-DALYTRAN-OPEN | open status | POST-01, POST-19 |
| 257-269 | 0100-TRANFILE-OPEN | open status | POST-01, POST-19 |
| 276-288 | 0200-XREFFILE-OPEN | open status | POST-01, POST-19 |
| 294-306 | 0300-DALYREJS-OPEN | open status | POST-01, POST-19 |
| 312-324 | 0400-ACCTFILE-OPEN | open status | POST-01, POST-19 |
| 330-342 | 0500-TCATBALF-OPEN | open status | POST-01, POST-19 |
| 347-368 | 1000-DALYTRAN-GET-NEXT | `00` / `10` EOF / other | POST-02, POST-19 |
| 372-376 | 1500-VALIDATE-TRAN | account check only if xref passed | POST-03 |
| 384-391 | 1500-A-LOOKUP-XREF | INVALID KEY / NOT INVALID KEY | POST-04 |
| 396-400 | 1500-B-LOOKUP-ACCT | INVALID KEY / NOT INVALID KEY | POST-05 |
| 407-413 | 1500-B-LOOKUP-ACCT | credit limit test | POST-06 |
| 414-420 | 1500-B-LOOKUP-ACCT | expiry test | POST-07 |
| 452-464 | 2500-WRITE-REJECT-REC | write status | POST-08, POST-19 |
| 475-493 | 2700-UPDATE-TCATBAL | INVALID KEY; `00`/`23` vs other | POST-12, POST-19 |
| 495-499 | 2700-UPDATE-TCATBAL | create or update | POST-12, POST-13, POST-14 |
| 512-524 | 2700-A-CREATE-TCATBAL-REC | write status | POST-13, POST-19 |
| 530-542 | 2700-B-UPDATE-TCATBAL-REC | rewrite status | POST-14, POST-19 |
| 548-552 | 2800-UPDATE-ACCOUNT-REC | amount sign picks credit or debit | POST-15 |
| 555 | 2800-UPDATE-ACCOUNT-REC | REWRITE INVALID KEY | POST-16 |
| 566-578 | 2900-WRITE-TRANSACTION-FILE | write status | POST-17, POST-19 |
| 585-597 | 9000-DALYTRAN-CLOSE | close status | POST-01, POST-19 |
| 603-615 | 9100-TRANFILE-CLOSE | close status | POST-01, POST-19 |
| 622-634 | 9200-XREFFILE-CLOSE | close status | POST-01, POST-19 |
| 640-652 | 9300-DALYREJS-CLOSE | close status | POST-01, POST-19 |
| 658-670 | 9400-ACCTFILE-CLOSE | close status | POST-01, POST-19 |
| 677-689 | 9500-TCATBALF-CLOSE | close status | POST-01, POST-19 |
| 715-726 | 9910-DISPLAY-IO-STATUS | status display format | POST-19 |

## No business logic

- DISPLAY of the start/end banners and of the TCATBAL "Creating" message (`app/cbl/CBTRN02C.cbl:194`,
  `app/cbl/CBTRN02C.cbl:232`, `app/cbl/CBTRN02C.cbl:476-477`).
- Commented-out DISPLAYs (`app/cbl/CBTRN02C.cbl:207`, `app/cbl/CBTRN02C.cbl:349`, `app/cbl/CBTRN02C.cbl:381`,
  `app/cbl/CBTRN02C.cbl:389`, `app/cbl/CBTRN02C.cbl:401-402`, `app/cbl/CBTRN02C.cbl:449`,
  `app/cbl/CBTRN02C.cbl:704`).

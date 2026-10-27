# CBACT04C business rules (interest calculation)

Program `app/cbl/CBACT04C.cbl`, run by job INTCALC (`app/jcl/INTCALC.jcl:22-41`). It reads the transaction category
balance file (TCATBALF) in key order, looks up an interest rate per category in the disclosure group file (DISCGRP),
writes one interest transaction per category with a non-zero rate, and adds the account's total interest to the
account master.

Line numbers refer to `main`. Classes: **calculation**, **validation**, **file I/O**, **control flow**,
**error handling**. Rules marked **SME question** describe behaviour that a port may or may not want to keep; the
port follows the source until an SME decides otherwise.

## Rules

| ID | Class | Rule | Inputs | Outputs | Source |
|----|-------|------|--------|---------|--------|
| INT-01 | control flow | The job takes one 10-character run parameter, PARM-DATE (INTCALC passes `PARM='2022071800'`); PARM-LENGTH is never checked and PARM-DATE is used only as the TRAN-ID prefix (INT-14), never as a business date. | JCL PARM | PARM-DATE | `app/cbl/CBACT04C.cbl:175-180`; `app/jcl/INTCALC.jcl:22` |
| INT-02 | file I/O | Files are opened TCATBALF INPUT (indexed, sequential access), XREFFILE INPUT (random, alternate key XREF-ACCT-ID), DISCGRP INPUT (random), ACCTFILE I-O (random) and TRANSACT OUTPUT (sequential, a new SYSTRAN generation), and all five are closed at the end. | DD TCATBALF, XREFFILE, DISCGRP, ACCTFILE, TRANSACT | open files | `app/cbl/CBACT04C.cbl:28-56`; `app/cbl/CBACT04C.cbl:182-186`; `app/cbl/CBACT04C.cbl:224-228`; `app/jcl/INTCALC.jcl:27-41` |
| INT-03 | control flow | The driver reads TCATBALF sequentially in key order (account id, type code, category code) until file status `10`, counting and DISPLAYing every record. | TCATBALF | TRAN-CAT-BAL-RECORD, WS-RECORD-COUNT | `app/cbl/CBACT04C.cbl:188-193`; `app/cbl/CBACT04C.cbl:325-348`; `app/cpy/CVTRA01Y.cpy:4-10` |
| INT-04 | control flow | On an account break (TRANCAT-ACCT-ID differs from the previous record's), the previous account is updated (INT-17) unless this is the first record, the interest total is reset to zero, and the new account and its card cross-reference are read. | TRANCAT-ACCT-ID, WS-LAST-ACCT-NUM, WS-FIRST-TIME | WS-TOTAL-INT, ACCOUNT-RECORD, CARD-XREF-RECORD | `app/cbl/CBACT04C.cbl:194-206` |
| INT-05 | control flow | **SME question.** The last account in TCATBALF is never updated: the `ELSE PERFORM 1050-UPDATE-ACCOUNT` branch needs END-OF-FILE = `Y` at the top of an iteration, but `PERFORM UNTIL` tests first and exits, so that account's interest transactions are written but its balance and cycle totals are left unchanged. | END-OF-FILE | (no account REWRITE for the last account) | `app/cbl/CBACT04C.cbl:188-191`; `app/cbl/CBACT04C.cbl:219-222` |
| INT-06 | validation | The account is read by TRANCAT-ACCT-ID; a missing account displays `ACCOUNT NOT FOUND` and then abends, because any status other than `00` abends. | TRANCAT-ACCT-ID | ACCOUNT-RECORD | `app/cbl/CBACT04C.cbl:202-203`; `app/cbl/CBACT04C.cbl:372-391` |
| INT-07 | validation | The card cross-reference is read by the alternate key XREF-ACCT-ID to get the card number for the interest transaction; a missing cross-reference abends. | TRANCAT-ACCT-ID | CARD-XREF-RECORD (XREF-CARD-NUM) | `app/cbl/CBACT04C.cbl:34-39`; `app/cbl/CBACT04C.cbl:204-205`; `app/cbl/CBACT04C.cbl:393-413`; `app/cpy/CVACT03Y.cpy:4-8` |
| INT-08 | calculation | The interest rate is looked up in DISCGRP by the key (ACCT-GROUP-ID, TRANCAT-TYPE-CD, TRANCAT-CD). | ACCT-GROUP-ID, TRANCAT-TYPE-CD, TRANCAT-CD | DIS-INT-RATE | `app/cbl/CBACT04C.cbl:210-213`; `app/cbl/CBACT04C.cbl:415-416`; `app/cpy/CVTRA02Y.cpy:4-9`; `app/cpy/CVACT01Y.cpy:16` |
| INT-09 | validation | If the group/type/category key is not found (status `23`), the lookup is retried with group id `DEFAULT` and the same type and category; a failure on the `DEFAULT` read, or any primary status other than `00`/`23`, abends. | DISCGRP-STATUS | DIS-INT-RATE | `app/cbl/CBACT04C.cbl:417-440`; `app/cbl/CBACT04C.cbl:443-460` |
| INT-10 | control flow | A category whose rate is zero produces no interest, no transaction and no fee call. | DIS-INT-RATE | (skip) | `app/cbl/CBACT04C.cbl:214-217` |
| INT-11 | calculation | Monthly interest = TRAN-CAT-BAL × DIS-INT-RATE / 1200 into S9(09)V99 with no ROUNDED and no ON SIZE ERROR, so it truncates to the cent and keeps the sign of the balance. | TRAN-CAT-BAL, DIS-INT-RATE | WS-MONTHLY-INT | `app/cbl/CBACT04C.cbl:462-465`; `app/cbl/CBACT04C.cbl:168`; `app/cpy/CVTRA01Y.cpy:9`; `app/cpy/CVTRA02Y.cpy:9` |
| INT-12 | calculation | Each category's monthly interest is added to the account's running total, which is reset to zero at every account break. | WS-MONTHLY-INT | WS-TOTAL-INT | `app/cbl/CBACT04C.cbl:467`; `app/cbl/CBACT04C.cbl:200`; `app/cbl/CBACT04C.cbl:169` |
| INT-13 | file I/O | One 350-byte interest transaction is written to TRANSACT for each category with a non-zero rate. | TRAN-RECORD | TRANSACT record | `app/cbl/CBACT04C.cbl:468`; `app/cbl/CBACT04C.cbl:500-515`; `app/cpy/CVTRA05Y.cpy:4-18` |
| INT-14 | calculation | TRAN-ID = PARM-DATE (10 bytes) followed by a 6-digit counter that starts at 0 and is incremented before each write for the whole run, not per account. | PARM-DATE, WS-TRANID-SUFFIX | TRAN-ID | `app/cbl/CBACT04C.cbl:173`; `app/cbl/CBACT04C.cbl:474-480` |
| INT-15 | calculation | The interest transaction has type `01`, category `05`, source `System`, description `Int. for a/c ` + ACCT-ID, amount = monthly interest, merchant id 0, blank merchant name/city/zip, and the cross-reference card number. | WS-MONTHLY-INT, ACCT-ID, XREF-CARD-NUM | TRAN-RECORD fields | `app/cbl/CBACT04C.cbl:482-495` |
| INT-16 | calculation | TRAN-ORIG-TS and TRAN-PROC-TS are both set from `FUNCTION CURRENT-DATE` at write time, formatted `YYYY-MM-DD-HH.MM.SS.hh0000` (26 bytes); the run parameter is not used. | system clock | TRAN-ORIG-TS, TRAN-PROC-TS | `app/cbl/CBACT04C.cbl:496-498`; `app/cbl/CBACT04C.cbl:613-626`; `app/cbl/CBACT04C.cbl:140-165` |
| INT-17 | file I/O | The account update adds the total interest to ACCT-CURR-BAL, sets ACCT-CURR-CYC-CREDIT and ACCT-CURR-CYC-DEBIT to zero, and REWRITEs the account; any status other than `00` abends. | WS-TOTAL-INT, ACCOUNT-RECORD | ACCTFILE record | `app/cbl/CBACT04C.cbl:350-370`; `app/cpy/CVACT01Y.cpy:7`; `app/cpy/CVACT01Y.cpy:13-14` |
| INT-18 | calculation | **SME question.** Fees are not computed: `1400-COMPUTE-FEES` holds only a "To be implemented" comment and EXIT, although the job's comment says it computes interest and fees. | none | none | `app/cbl/CBACT04C.cbl:216`; `app/cbl/CBACT04C.cbl:518-520`; `app/jcl/INTCALC.jcl:20` |
| INT-19 | error handling | Any open, read, write, rewrite or close status other than the expected one DISPLAYs a message and the file status, then abends through `CEE3ABD` with code 999. | file status fields | SYSOUT message, abend U999 | `app/cbl/CBACT04C.cbl:628-648`; `app/cbl/CBACT04C.cbl:234-250` |
| INT-20 | control flow | On normal completion the program DISPLAYs start and end messages and returns without setting RETURN-CODE (RC 0); there is no reject path and no warning return code. | none | RC 0 | `app/cbl/CBACT04C.cbl:181`; `app/cbl/CBACT04C.cbl:230-232` |

## Notes for the SME

- **INT-05.** A GnuCOBOL 3.1.2 run of a program with the same loop shape (lines 188-222) over the keys `A, A, B`
  printed `UPDATE ACCOUNT A (BREAK)` and never reached the `ELSE` branch for `B`. In practice the last account's
  interest is in SYSTRAN but not in ACCT-CURR-BAL, and its cycle credit/debit are not zeroed. The port follows the
  source by default. Choose **Change** if the port should also update the last account.
- **INT-11.** Truncation example: 1000.00 × 18.99 / 1200 = 15.825 is stored as 15.82; with ROUNDED it would be
  15.83 (same GnuCOBOL run). A negative category balance gives a negative interest amount, because no sign test is
  made. The divisor 1200 treats DIS-INT-RATE as an annual percentage applied for one month.
- **INT-09.** The fallback replaces only the group id. The type and category codes still come from the TCATBAL
  record, so the `DEFAULT` group must have a row for every type/category in use, or the job abends.
- **INT-14.** IDs are unique only within one run. Re-running with the same PARM-DATE produces the same IDs. The
  counter is 9(06) with no ON SIZE ERROR.
- **INT-15.** TRAN-RECORD is never INITIALIZEd in this program. TRAN-DESC positions after the 24 STRINGed
  characters and the trailing FILLER are never set here, so their content depends on how the compiler/runtime
  initializes WORKING-STORAGE (no VALUE clause, `app/cpy/CVTRA05Y.cpy:9`, `app/cpy/CVTRA05Y.cpy:18`). The category
  code is the literal `'05'` moved to PIC 9(04), which stores `0005`.
- **INT-16.** `COB-MIL` receives positions 15-16 of `FUNCTION CURRENT-DATE`, which are hundredths of a second, and
  `DB2-REST` is `0000`. Two runs over the same data therefore differ in these two timestamp fields.
- **INT-17.** Only accounts that have at least one TCATBAL record have their cycle totals zeroed, and the last
  account is excluded (INT-05). This happens even when every rate is zero.
- **INT-07.** INTCALC supplies the alternate-index path as DD XREFFIL1 (`app/jcl/INTCALC.jcl:31-32`). The source has
  no rule for choosing among several cards on one account. The card used is whichever record the alternate-key
  READ returns.
- **INT-19.** The DISCGRP open-error text says `ERROR OPENING DALY REJECTS FILE` (`app/cbl/CBACT04C.cbl:281`). This
  is a copy-paste message, not business logic.

## Paragraph map

| Paragraph | Line | Maps to |
|-----------|------|---------|
| `0000-TCATBALF-OPEN` | 234 | INT-02, INT-19 |
| `0100-XREFFILE-OPEN` | 252 | INT-02, INT-19 |
| `0200-DISCGRP-OPEN` | 270 | INT-02, INT-19 |
| `0300-ACCTFILE-OPEN` | 289 | INT-02, INT-19 |
| `0400-TRANFILE-OPEN` | 307 | INT-02, INT-19 |
| `1000-TCATBALF-GET-NEXT` | 325 | INT-03, INT-19 |
| `1050-UPDATE-ACCOUNT` | 350 | INT-17, INT-19 |
| `1100-GET-ACCT-DATA` | 372 | INT-06, INT-19 |
| `1110-GET-XREF-DATA` | 393 | INT-07, INT-19 |
| `1200-GET-INTEREST-RATE` | 415 | INT-08, INT-09, INT-19 |
| `1200-A-GET-DEFAULT-INT-RATE` | 443 | INT-09, INT-19 |
| `1300-COMPUTE-INTEREST` | 462 | INT-11, INT-12 |
| `1300-B-WRITE-TX` | 473 | INT-13, INT-14, INT-15, INT-16, INT-19 |
| `1400-COMPUTE-FEES` | 518 | INT-18 |
| `9000-TCATBALF-CLOSE` | 522 | INT-02, INT-19 |
| `9100-XREFFILE-CLOSE` | 541 | INT-02, INT-19 |
| `9200-DISCGRP-CLOSE` | 559 | INT-02, INT-19 |
| `9300-ACCTFILE-CLOSE` | 577 | INT-02, INT-19 |
| `9400-TRANFILE-CLOSE` | 595 | INT-02, INT-19 |
| `Z-GET-DB2-FORMAT-TIMESTAMP` | 613 | INT-16 |
| `9999-ABEND-PROGRAM` | 628 | INT-19 |
| `9910-DISPLAY-IO-STATUS` | 635 | INT-19 |

The unnamed main line (lines 180-232) is covered by INT-01 to INT-05, INT-10 and INT-20.

## Branch map

Every IF, ELSE, INVALID KEY and PERFORM UNTIL in the PROCEDURE DIVISION falls in one row below.

| Lines | Paragraph | Decision | Maps to |
|-------|-----------|----------|---------|
| 188-191 | main line | loop until EOF; read only while not EOF | INT-03 |
| 194-206 | main line | account break; first-record test | INT-04 |
| 214-217 | main line | rate not zero | INT-10 |
| 219-221 | main line | ELSE update on EOF (unreachable) | INT-05 |
| 237-249 | 0000-TCATBALF-OPEN | open status | INT-02, INT-19 |
| 255-267 | 0100-XREFFILE-OPEN | open status | INT-02, INT-19 |
| 273-285 | 0200-DISCGRP-OPEN | open status | INT-02, INT-19 |
| 292-304 | 0300-ACCTFILE-OPEN | open status | INT-02, INT-19 |
| 310-322 | 0400-TRANFILE-OPEN | open status | INT-02, INT-19 |
| 327-347 | 1000-TCATBALF-GET-NEXT | `00` / `10` EOF / other | INT-03, INT-19 |
| 357-369 | 1050-UPDATE-ACCOUNT | rewrite status | INT-17, INT-19 |
| 374-390 | 1100-GET-ACCT-DATA | INVALID KEY; read status | INT-06, INT-19 |
| 396-412 | 1110-GET-XREF-DATA | INVALID KEY; read status | INT-07, INT-19 |
| 417-435 | 1200-GET-INTEREST-RATE | INVALID KEY; `00`/`23` vs other | INT-09, INT-19 |
| 436-439 | 1200-GET-INTEREST-RATE | `23` triggers DEFAULT retry | INT-09 |
| 446-459 | 1200-A-GET-DEFAULT-INT-RATE | default read status | INT-09, INT-19 |
| 501-514 | 1300-B-WRITE-TX | write status | INT-13, INT-19 |
| 525-537 | 9000-TCATBALF-CLOSE | close status | INT-02, INT-19 |
| 544-556 | 9100-XREFFILE-CLOSE | close status | INT-02, INT-19 |
| 562-574 | 9200-DISCGRP-CLOSE | close status | INT-02, INT-19 |
| 580-592 | 9300-ACCTFILE-CLOSE | close status | INT-02, INT-19 |
| 598-610 | 9400-TRANFILE-CLOSE | close status | INT-02, INT-19 |
| 636-647 | 9910-DISPLAY-IO-STATUS | status display format | INT-19 |

## No business logic

- DISPLAY of each TCATBAL record and of the start/end banners (`app/cbl/CBACT04C.cbl:181`,
  `app/cbl/CBACT04C.cbl:193`, `app/cbl/CBACT04C.cbl:230`). WS-RECORD-COUNT is incremented at line 192 but never
  displayed or tested.
- Commented-out DISPLAYs (`app/cbl/CBACT04C.cbl:207-209`, `app/cbl/CBACT04C.cbl:625`).

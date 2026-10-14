# CBTRN02C → Java mapping

Package root: `com.carddemo.batch.posttran`.

## Files

| COBOL DD / FD | Copybook | Java | Key |
|---|---|---|---|
| DALYTRAN (sequential, 350) | CVTRA06Y | `record.DailyTransaction`, `batch.DailyTransactionReader` | n/a |
| TRANFILE (KSDS, 350) | CVTRA05Y | `domain.TransactionRecord` → table `transact` | `TRAN-ID X(16)` |
| XREFFILE (KSDS, 50) | CVACT03Y | `domain.CardXref` → table `card_xref` | `XREF-CARD-NUM X(16)` |
| ACCTFILE (KSDS, 300) | CVACT01Y | `domain.Account` → table `account` | `ACCT-ID 9(11)` |
| TCATBALF (KSDS, 50) | CVTRA01Y | `domain.TranCatBalance` → table `tran_cat_balance` | `domain.TranCatBalanceKey` (acct id 9(11) + type X(2) + cat 9(4)) |
| DALYREJS (sequential, 430) | inline in CBTRN02C | `record.RejectRecord`, `batch.RejectFileWriter` | n/a |

Each entity has `fromRecord(String)` / `toRecord()` that reproduce the copybook layout byte for
byte, FILLER included, so unloaded tables can be compared to the VSAM output.

## Paragraphs

| Paragraph | Java |
|---|---|
| PROCEDURE DIVISION main loop | `batch.PostTranLauncher#run` (job `POSTTRAN`, step `CBTRN02C`) + `batch.PostingItemWriter#write` |
| 0000-DALYTRAN-OPEN / 1000-DALYTRAN-GET-NEXT / 9000-DALYTRAN-CLOSE | `batch.DailyTransactionReader` |
| 0100-TRANFILE-OPEN, 0200-XREFFILE-OPEN, 0400-ACCTFILE-OPEN, 0500-TCATBALF-OPEN and their CLOSEs | JPA `EntityManager` / datasource (Spring-managed) |
| 0300-DALYREJS-OPEN / 9300-DALYREJS-CLOSE | `batch.RejectFileWriter#open` / `#close` |
| 1500-VALIDATE-TRAN, 1500-A-LOOKUP-XREF, 1500-B-LOOKUP-ACCT | `logic.PostingService#validate` → `logic.ValidationResult` |
| 2000-POST-TRANSACTION | `logic.PostingService#post` |
| 2500-WRITE-REJECT-REC | `record.RejectRecord#format` + `batch.RejectFileWriter#write` |
| 2700-UPDATE-TCATBAL, 2700-A-CREATE-TCATBAL-REC, 2700-B-UPDATE-TCATBAL-REC | `logic.PostingService#updateTranCatBalance` |
| 2800-UPDATE-ACCOUNT-REC | `logic.PostingService#updateAccount` |
| 2900-WRITE-TRANSACTION-FILE | `logic.PostingService#writeTransaction` |
| Z-GET-DB2-FORMAT-TIMESTAMP | `cobol.Db2Timestamp#now` (`YYYY-MM-DD-HH.MM.SS.hh0000`) |
| DISPLAY counts, MOVE 4 TO RETURN-CODE | `batch.PostTranLauncher#run`, `PostTranCommandLineRunner#getExitCode` |
| 9999-ABEND-PROGRAM, 9910-DISPLAY-IO-STATUS | `cobol.Abend` (thrown) → `PostTranLauncher#abend` prints message, `FILE STATUS IS: NNNN00xx`, `ABENDING PROGRAM`; exit code 12 |

## Behaviours kept on purpose

- **Validation order.** 100 (no xref) and 101 (no account) stop validation. 102 (overlimit) and 103
  (expired) are both evaluated; when both fail, 103 is the reason written.
- **Overlimit arithmetic.** `WS-TEMP-BAL` is `S9(09)V99` without `ON SIZE ERROR`, so the sum is
  truncated to 9 integer digits before the compare (`ZonedDecimal#truncate`). Balance updates are
  truncated to their PIC sizes the same way.
- **Expiry compare** is alphanumeric (`ACCT-EXPIRAION-DATE` vs the first 10 characters of
  `DALYTRAN-ORIG-TS`), done in EBCDIC collating order (`cobol.CobolCompare`).
- **Zero amount** counts as a credit (`IF DALYTRAN-AMT >= 0`).
- **New TCATBAL filler.** `INITIALIZE` does not clear FILLER, so a created TCATBAL record carries
  the filler of the last TCATBAL record read in the run. The seed fillers are blank, so this only
  matters for non-standard data.
- **TCATBAL "not found" DISPLAY** is printed on every create, as in the COBOL.
- **Reason 109** (account REWRITE INVALID KEY) cannot happen: the account was read in the same
  transaction. In the COBOL it is also a no-op because `WS-VALIDATION-FAIL-REASON` is not checked
  after 2800. It is not implemented as a reject.
- **Records are processed one by one in input order**, so validation sees balances updated by
  earlier postings in the same run.

## Deviations

- Amounts are **zoned decimal** (DISPLAY, sign overpunched on the last digit), not COMP-3: none of the
  copybook amount fields has a USAGE clause. They are decoded into `BigDecimal` (`cobol.ZonedDecimal`).
  EBCDIC data is decoded with CP037, which maps the sign zones X'C0'-X'D9' to `{A-I}J-R`.
- DALYTRAN records with non-numeric data in numeric fields abend (`ERROR READING DALYTRAN FILE`,
  status 04). The COBOL would carry on with undefined arithmetic.
- An abend returns exit code 12 and leaves the job `FAILED`; the last uncommitted chunk is rolled
  back. On z/OS CEE3ABD gives user abend U999 and earlier VSAM updates stay applied. With the default
  chunk size of 1 every posted record is already committed, matching the COBOL.

## Golden master

`tools/gnucobol/run-cobol-posttran.sh` runs the unmodified program under GnuCOBOL 3.1.2. Both seed
sets give 300 processed / 38 rejected (all reason 0102) / RC 4, 262 posted transactions and
100 TCATBAL records (62 created). `GoldenMasterTest` matches all outputs exactly for both seed sets.

The ASCII and EBCDIC golden files differ in one place, and it comes from the seed data, not the
program: account `00000000049` has `ACCT-ADDR-ZIP` = spaces in `app/data/ASCII/acctdata.txt` but
`ZEROAPR` in `AWS.M2.CARDDEMO.ACCTDATA.PS`. CBTRN02C never touches that field, and the COBOL and Java
outputs agree for each seed set.

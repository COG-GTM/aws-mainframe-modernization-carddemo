# CBTRN02C — Daily Transaction Posting: Business Logic Map

**Audience:** a subject-matter expert who knows the credit-card business but does not read COBOL or Java.
**Sources:** only the static export in this repository — `app/cbl`, `app/cpy`, `app/jcl`, `app/asm`, `app/data`.
No mainframe was connected at any point; every statement below is cited to a file and line you can open yourself.

---

## 1. What this program does, in one paragraph

Every night CardDemo receives a flat file of the day's card transactions. This program takes each
one in turn and decides whether it may be posted. A transaction is posted only if its card is
known, the account behind that card exists, the account has room under its credit limit and the
account has not expired. A posted transaction is copied into the transaction master with a
processing timestamp added, its amount is accumulated into the account's balance and into a
per-category bucket for that account, and the account record is rewritten. A transaction that
fails any check is not posted at all: it is copied verbatim onto a rejects file with a numeric
reason and a description appended, and the step ends with return code 4 so the operator knows
there is something to look at.

---

## 2. Where it runs: the JCL job context

| Item | Value | Source |
| --- | --- | --- |
| Job | `POSTTRAN` — "Process and load daily transaction file and create transaction category balance and update transaction master vsam" | `app/jcl/POSTTRAN.jcl:1, 19-22` |
| Step | `STEP15 EXEC PGM=CBTRN02C` — no `PARM`, unlike the interest sliver | `app/jcl/POSTTRAN.jcl:23` |
| Schedule | Control-M folder `DAILY-*`; see `app/scheduler/CardDemo.controlm` | `app/scheduler/CardDemo.controlm` |

### DD name → dataset mapping

| DD name | Dataset | Direction | Organisation / access | Static dump used by the Java harness |
| --- | --- | --- | --- | --- |
| `DALYTRAN` | `AWS.M2.CARDDEMO.DALYTRAN.PS` | in | sequential, read start to finish | `app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS` (300 records) |
| `XREFFILE` | `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` | in | indexed, random by card number | `app/data/EBCDIC/AWS.M2.CARDDEMO.CARDXREF.PS` (50 records) |
| `ACCTFILE` | `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` | in **and** out (rewritten in place) | indexed, random by account id | `app/data/EBCDIC/AWS.M2.CARDDEMO.ACCTDATA.PS` (50 records) |
| `TCATBALF` | `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS` | in **and** out (rewritten and inserted) | indexed, random by account+type+category | `app/data/EBCDIC/AWS.M2.CARDDEMO.TCATBALF.PS` (50 records) |
| `TRANFILE` | `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS` | out | indexed, `WRITE` by transaction id | produced by the Java job |
| `DALYREJS` | `AWS.M2.CARDDEMO.DALYREJS(+1)`, `RECFM=F LRECL=430` | out, new generation each run | sequential | produced by the Java job |

Source: `app/jcl/POSTTRAN.jcl:26-43`; organisations, access modes and record keys at
`app/cbl/CBTRN02C.cbl:29-61`.

Note the asymmetry that matters for testing: two of the six files are **read and written**, so a
rerun is not idempotent. Posting the same daily file twice doubles the balances. There is no
checkpoint, no commit scope and no restart logic anywhere in the program.

---

## 3. The data the program works with

Copybook fields, their COBOL picture clauses, and the Java type used in
`modernization/transaction-posting-service`.

### 3.1 `CVTRA06Y` — daily transaction, the driving input (350 bytes)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `DALYTRAN-ID` | `X(16)` | transaction id, becomes the transaction-master key | `DailyTransaction.transactionId` (`String`) |
| `DALYTRAN-TYPE-CD` | `X(02)` | transaction type | `String` |
| `DALYTRAN-CAT-CD` | `9(04)` | transaction category | `String` (kept as text: it is a key, never arithmetic) |
| `DALYTRAN-SOURCE` | `X(10)` | capture source | `String` |
| `DALYTRAN-DESC` | `X(100)` | description | `String` |
| `DALYTRAN-AMT` | `S9(09)V99` | signed amount; positive is a purchase, negative a refund | `BigDecimal`, scale 2 |
| `DALYTRAN-MERCHANT-ID` | `9(09)` | merchant number | `String` |
| `DALYTRAN-MERCHANT-NAME/-CITY/-ZIP` | `X(50)/X(50)/X(10)` | merchant details | `String` |
| `DALYTRAN-CARD-NUM` | `X(16)` | card number, the cross-reference key | `CardNumber` |
| `DALYTRAN-ORIG-TS` | `X(26)` | when the transaction originated | `String`; its first 10 characters are the origination **date** (BR-5) |
| `DALYTRAN-PROC-TS` | `X(26)` | processing timestamp — blank on input, stamped on output | `String` |
| `FILLER` | `X(20)` | unused | preserved byte for byte |

### 3.2 `CVTRA05Y` — transaction master record, the posted output (350 bytes)

Field for field identical to `CVTRA06Y` (`app/cpy/CVTRA05Y.cpy:4-18`) — which is why posting is a
copy, see BR-7. → `PostedTransaction`.

### 3.3 `CVACT03Y` — card cross-reference (50 bytes)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `XREF-CARD-NUM` | `X(16)` | card number, primary key | `CardXref.cardNumber` |
| `XREF-CUST-ID` | `9(09)` | customer number (read, never used here) | `String` |
| `XREF-ACCT-ID` | `9(11)` | account number | `AccountId` |

### 3.4 `CVACT01Y` — account master (300 bytes)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `ACCT-ID` | `9(11)` | account number | `AccountId` |
| `ACCT-ACTIVE-STATUS` | `X(01)` | active flag — **never tested by this program** | `String` |
| `ACCT-CURR-BAL` | `S9(10)V99` | current balance — **updated** | `BigDecimal` |
| `ACCT-CREDIT-LIMIT` | `S9(10)V99` | credit limit — drives BR-4 | `BigDecimal` |
| `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | cash limit — not used here | `BigDecimal` |
| `ACCT-OPEN-DATE`, `ACCT-REISSUE-DATE` | `X(10)` | dates as text — not used here | `String` |
| `ACCT-EXPIRAION-DATE` | `X(10)` | expiry date as text (spelling is the copybook's) — drives BR-5 | `String` |
| `ACCT-CURR-CYC-CREDIT`, `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | this cycle's totals — **updated** | `BigDecimal` |
| `ACCT-ADDR-ZIP`, `ACCT-GROUP-ID` | `X(10)` | postcode, pricing group — not used here | `String` |
| `FILLER` | `X(178)` | unused | preserved byte for byte |

### 3.5 `CVTRA01Y` — transaction category balance (50 bytes)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `TRANCAT-ACCT-ID` | `9(11)` | account number | `TransactionCategoryKey.accountId` |
| `TRANCAT-TYPE-CD` | `X(02)` | transaction type | `TransactionCategoryKey.typeCode` |
| `TRANCAT-CD` | `9(04)` | transaction category | `TransactionCategoryKey.categoryCode` |
| `TRAN-CAT-BAL` | `S9(09)V99` | balance in that bucket — **updated** | `BigDecimal`, scale 2 |
| `FILLER` | `X(22)` | unused | preserved byte for byte |

The three key fields together are the VSAM key `FD-TRAN-CAT-KEY` (`app/cbl/CBTRN02C.cbl:57-61`).

### 3.6 The reject record (430 bytes) — declared in the program, not in a copybook

`REJECT-TRAN-DATA X(350)` followed by `VALIDATION-TRAILER X(80)`, which is
`WS-VALIDATION-FAIL-REASON 9(04)` plus `WS-VALIDATION-FAIL-REASON-DESC X(76)`
(`app/cbl/CBTRN02C.cbl:176-183`). The `LRECL=430` on the DD statement
(`app/jcl/POSTTRAN.jcl:36`) is the only other place this layout appears.
→ `RejectedTransaction`, whose codec copies the original 350 bytes through unchanged.

### 3.7 How the numbers are physically stored, and why it matters

* **Zoned decimal (`DISPLAY`)** — every numeric field in all five copybooks above is stored as
  characters, one digit per byte, with the sign "overpunched" into the last digit. Decoded and
  encoded by the shared `ZonedDecimalCodec`.
* **Packed decimal (`COMP-3`)** — **this sliver uses none.** Verified by reading all five
  copybooks: no `COMP-3`, `COMP` or `BINARY` usage appears on any field of `CVTRA06Y`, `CVTRA05Y`,
  `CVACT03Y`, `CVACT01Y` or `CVTRA01Y`. The program's only binary items are working-storage
  counters (`app/cbl/CBTRN02C.cbl:134, 142, 147-148`) that never reach a dataset. The shared
  `PackedDecimalCodec` built for the interest sliver is therefore carried unchanged and unused
  here — the point of a shared module is that the next sliver that *does* meet COMP-3 pays nothing
  for it. Recording an absence explicitly is deliberate: on a disconnected engagement you have to
  prove a field is not packed rather than assume it.
* **`V` is an implied decimal point** and occupies no byte: `S9(09)V99` is eleven digit characters
  holding two decimal places.
* **Truncation, not rounding, and silent overflow.** No `ROUNDED` and no `ON SIZE ERROR` appears
  anywhere in this program, so a value too large for its receiving field loses its **high-order**
  digits without any diagnostic. This is not theoretical here: BR-4b and BR-8 both depend on it,
  and the parity harness has a scenario dedicated to it. `CobolFixedPoint.fit` reproduces it;
  `BigDecimal` with explicit scales is used everywhere and `double` appears nowhere.

---

## 4. The business rules

### BR-1 — One pass, one transaction at a time, in file order

The daily file is read sequentially to end of file; each record is counted, validated, and then
either posted or rejected (`app/cbl/CBTRN02C.cbl:202-219`, read at `1000-DALYTRAN-GET-NEXT`,
`app/cbl/CBTRN02C.cbl:345-369`). Order matters: two transactions on the same account accumulate in
input order, and the balances the second one sees are the ones the first one left behind.

*Java:* `DailyTransactionPostingService.post` iterates the decoded transactions in order;
`DailyTransactionPostingJob` is the only class that knows they came from a file.

### BR-2 — The card must be known (reject reason 100)

The card number is looked up in the cross-reference file. If there is no such card the transaction
is rejected with reason **100, `INVALID CARD NUMBER FOUND`** and nothing else is checked
(`1500-A-LOOKUP-XREF`, `app/cbl/CBTRN02C.cbl:380-392`).

*Java:* `CardXrefRepository`; `TransactionValidator` returns `RejectReason.UNKNOWN_CARD`.

### BR-3 — The account behind the card must exist (reject reason 101)

The cross-reference gives an account number, which is read from the account master. A missing
account is reject reason **101, `ACCOUNT RECORD NOT FOUND`** (`1500-B-LOOKUP-ACCT`,
`app/cbl/CBTRN02C.cbl:393-399`). Note this is a *data integrity* failure — a cross-reference
pointing at an account that is not there — and the program treats it as a routine rejection rather
than an abend.

*Java:* `AccountRepository`; `RejectReason.ACCOUNT_NOT_FOUND`.

### BR-4 — The transaction must fit under the credit limit (reject reason 102)

```
WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT
reject 102 unless ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
```
(`app/cbl/CBTRN02C.cbl:403-413`). In business terms: the limit is tested against **this cycle's
net activity plus the new amount**, and the comparison is inclusive, so a transaction that lands
exactly on the limit is allowed.

#### BR-4a — Defect: the limit test ignores the account balance

`ACCT-CURR-BAL` — what the customer actually owes, carried over from previous cycles — takes no
part in the test. An account well over its limit from last month accepts new spend as long as this
cycle's activity is small. Whether that is intended is a question for the business (§7); the Java
module reproduces it and says so in `CreditLimitRule`'s JavaDoc.

#### BR-4b — Defect: the exposure figure is one digit too narrow

`WS-TEMP-BAL` is `PIC S9(09)V99` (`app/cbl/CBTRN02C.cbl:187`) while the fields feeding it are
`S9(10)V99`. An exposure of 1,999,999,999.98 becomes 999,999,999.98 — the leading digit is
dropped silently and the transaction passes a limit check it should have failed. Reproduced by
`CreditLimitRule.projectedCycleBalance` via `CobolFixedPoint.fit`, and pinned by
`PostingRulesTest.projectedBalanceTruncatesToNineIntegerDigits`.

### BR-5 — The account must not have expired (reject reason 103)

The account's expiry date is compared as **text** against the first ten characters of the
transaction's origination timestamp, i.e. its date (`app/cbl/CBTRN02C.cbl:414-420`). Transactions
originating after the expiry date are rejected with reason **103,
`TRANSACTION RECEIVED AFTER ACCT EXPIRATION`**. Both values are `YYYY-MM-DD`, so text comparison
and date comparison agree — as long as the data really is in that format, which nothing enforces.

*Java:* `AccountExpiryRule.expiredFor`, deliberately comparing strings for the same reason.

### BR-6 — One reason survives, and it is the last one that failed

The two checks in BR-4 and BR-5 run unconditionally one after the other, each overwriting the
reason field. A transaction that is both over limit and past expiry is reported as **103 only**.

#### BR-6a — Defect: rejections mask each other

Only one reason ever reaches the rejects file, so the operations team cannot see that an account
breached two rules. The comment `* ADD MORE VALIDATIONS HERE`
(`app/cbl/CBTRN02C.cbl:377`) shows more rules were anticipated; each one added makes the
masking worse. `TransactionValidator` reproduces the precedence exactly and documents why the
code looks like a bug — because it is one.

### BR-7 — Posting a transaction is a copy plus a processing timestamp

`2000-POST-TRANSACTION` (`app/cbl/CBTRN02C.cbl:424-444`) moves all fourteen input fields into the
identically shaped transaction-master record and overwrites only `TRAN-PROC-TS` with the current
time in DB2 format `YYYY-MM-DD-HH.MM.SS.MM0000` — hundredths of a second followed by four zeros,
not microseconds (`Z-GET-DB2-FORMAT-TIMESTAMP`, `app/cbl/CBTRN02C.cbl:692-705`). Nothing is
recalculated, reformatted or defaulted.

*Java:* `PostedTransactionFactory` with the shared `Db2TimestampFormatter`, which takes an injected
`java.time.Clock` so the parity harness can freeze it. The three updates then happen in this order,
which is preserved because the last of them can abend and leave the first two applied (BR-13):
category balance, then account, then transaction master.

### BR-8 — Category balances: update if the bucket exists, create it if not

The bucket for account + transaction type + category is read
(`2700-UPDATE-TCATBAL`, `app/cbl/CBTRN02C.cbl:467-501`). If it exists, the amount is added and the
record rewritten (`2700-B-UPDATE-TCATBAL-REC`, `app/cbl/CBTRN02C.cbl:526-542`); if it does not, a
new record is initialised with the key, the amount added to a zero balance, and the record inserted
(`2700-A-CREATE-TCATBAL-REC`, `app/cbl/CBTRN02C.cbl:503-524`). `TRAN-CAT-BAL` is `S9(09)V99`, one
digit narrower than the account balances, so this is where overflow bites first.

*Java:* `TransactionCategoryBalanceRepository` plus `TransactionCategoryBalance.opened` /
`.withAmountAdded`.

### BR-9 — A rejected transaction is passed through verbatim

`2500-WRITE-REJECT-REC` (`app/cbl/CBTRN02C.cbl:446-465`) writes the original 350-byte record
unchanged, followed by the four-digit reason and its 76-character description. Nothing about the
transaction is normalised, so the rejects file can be re-presented to the job once the underlying
data is fixed. **A rejected transaction changes nothing**: no balance, no bucket, no master record.

*Java:* `RejectedTransactionCodec` copies the raw input bytes rather than re-encoding the decoded
object, so a reject is byte-identical to its input even if the input was malformed.

### BR-10 — Posting to the account: balance always, then one of two cycle buckets

```
ADD DALYTRAN-AMT TO ACCT-CURR-BAL
IF DALYTRAN-AMT >= 0  ADD to ACCT-CURR-CYC-CREDIT  ELSE  ADD to ACCT-CURR-CYC-DEBIT
```
(`2800-UPDATE-ACCOUNT-REC`, `app/cbl/CBTRN02C.cbl:545-560`). A zero-amount transaction counts as a
credit. Nothing else on the 300-byte record changes.

#### BR-10a — The "debit" bucket accumulates negative numbers

Because the negative amount is *added*, `ACCT-CURR-CYC-DEBIT` grows more negative with every
refund — and BR-4 then **subtracts** it, so refunds increase the exposure figure instead of
reducing it. Either the sign convention or the limit formula is wrong; the static export cannot
say which, so both are reproduced exactly (`Account.withTransactionPosted`, and
`PostingRulesTest.creditLimitIgnoresBalanceAndSubtractsNegativeDebits`).

### BR-11 — Keys are fixed-width and zero padded

Account ids are `9(11)` and category codes `9(04)`; they are compared and looked up as
fixed-width text, which is what makes VSAM key order the same as string order. `AccountId.of`
zero-pads to eleven characters and `TransactionCategoryKey.keyText` concatenates the seventeen key
bytes, so Java map lookups behave like the `READ ... INVALID KEY` they replace.

### BR-12 — The step ends RC=4 if anything was rejected

`IF WS-REJECT-COUNT > 0 MOVE 4 TO RETURN-CODE` (`app/cbl/CBTRN02C.cbl:226-232`), after displaying
the processed and rejected counts. Downstream steps can therefore branch on rejects.

*Java:* `PostingResult.returnCode()`, returned by `DailyTransactionPostingJob.main`.

### BR-13 — Operational behaviour that does not survive as business logic

Opening and closing six files with file-status checks (`app/cbl/CBTRN02C.cbl:236-343, 582-690`),
`9910-DISPLAY-IO-STATUS` (`app/cbl/CBTRN02C.cbl:714-727`) and `9999-ABEND-PROGRAM`'s
`CALL 'CEE3ABD'` (`app/cbl/CBTRN02C.cbl:707-712`) are plumbing, not rules. In Java they become
repository lifecycles and typed exceptions: `DatasetAccessException` and
`DatasetIntegrityException`, both `PostingBatchException`, fail the run exactly as an abend fails
the step — but with a message instead of a `U0999` and a dump.

Worth stating for the risk register rather than the rule book: because there is no commit scope, an
abend part-way through leaves the account and category files updated for every transaction posted
so far, with no record of where the run stopped. Rerunning the daily file after an abend
double-posts everything before the failure.

---

## 5. Assembler and subprogram dependency chain

Verified by searching every COBOL source in `app/cbl` for `CALL` statements:

| Callable | Called from | Purpose | Handling in Java |
| --- | --- | --- | --- |
| `CEE3ABD` | `CBTRN02C` (`app/cbl/CBTRN02C.cbl:711`) | Language Environment abend service | replaced by throwing `PostingBatchException`, see BR-13 |
| `COBDATFT` (`app/asm/COBDATFT.asm`) | `CBACT01C` only (`app/cbl/CBACT01C.cbl:231`) | date formatting service | **not reachable from this sliver** |
| `MVSWAIT` (`app/asm/MVSWAIT.asm`) | `COBSWAIT` only (`app/cbl/COBSWAIT.cbl:38`) | MVS timed wait | **not reachable from this sliver** |

Date and time handling is `FUNCTION CURRENT-DATE` (`app/cbl/CBTRN02C.cbl:693`), replaced by an
injected `java.time.Clock`, not by `COBDATFT`. As with the first sliver, the Assembler chain is
**empty** — and, as with the first sliver, that absence is a finding to be proved rather than
assumed.

---

## 6. Rule-to-code index

| Rule | COBOL | Java |
| --- | --- | --- |
| BR-1 sequential drive | `CBTRN02C.cbl:202-219`, `345-369` | `service/DailyTransactionPostingService.java`, `batch/DailyTransactionPostingJob.java` |
| BR-2 card lookup (100) | `CBTRN02C.cbl:380-392` | `repository/CardXrefRepository.java`, `rules/TransactionValidator.java` |
| BR-3 account lookup (101) | `CBTRN02C.cbl:393-399` | `repository/AccountRepository.java`, `rules/TransactionValidator.java` |
| BR-4, 4a, 4b credit limit (102) | `CBTRN02C.cbl:403-413`, `187` | `rules/CreditLimitRule.java`, `cobol/CobolFixedPoint.java` |
| BR-5 expiry (103) | `CBTRN02C.cbl:414-420` | `rules/AccountExpiryRule.java` |
| BR-6, 6a reason precedence | `CBTRN02C.cbl:370-378`, `403-420` | `rules/TransactionValidator.java`, `domain/RejectReason.java` |
| BR-7 posted record | `CBTRN02C.cbl:424-444`, `692-705` | `rules/PostedTransactionFactory.java`, `cobol/Db2TimestampFormatter.java` |
| BR-8 category balances | `CBTRN02C.cbl:467-542` | `domain/TransactionCategoryBalance.java`, `repository/InMemoryTransactionCategoryBalanceRepository.java` |
| BR-9 reject record | `CBTRN02C.cbl:446-465`, `176-183` | `domain/RejectedTransaction.java`, `io/codec/RejectedTransactionCodec.java` |
| BR-10, 10a account update | `CBTRN02C.cbl:545-560` | `domain/Account.java` |
| BR-11 key padding | `CBTRN02C.cbl:57-61` | `domain/AccountId.java`, `domain/TransactionCategoryKey.java` |
| BR-12 return code | `CBTRN02C.cbl:226-232` | `service/PostingResult.java` |
| BR-13 file status / abend | `CBTRN02C.cbl:236-343`, `582-727` | `exception/PostingBatchException.java` and subclasses |
| Record layouts | `app/cpy/CVTRA06Y.cpy`, `CVTRA05Y.cpy`, `CVACT03Y.cpy`, `CVACT01Y.cpy`, `CVTRA01Y.cpy` | `carddemo-mainframe-io` `io/layout/CardDemoLayouts.java`, `io/codec/*` |

---

## 7. Questions this static export cannot answer

1. **Is BR-4a intended?** Testing the credit limit against cycle activity rather than the balance
   owed lets an over-limit account keep spending. This needs a product answer, not a code fix.
2. **Which side of BR-10a is wrong** — the sign the debit bucket carries, or the subtraction in the
   limit formula? Both are reproduced; only the business can say which was meant.
3. **Are rejects re-presented?** The rejects file is a new generation each run
   (`app/jcl/POSTTRAN.jcl:34-38`) and nothing in the export consumes it. If a human fixes the data
   and resubmits, BR-9's verbatim copy is what makes that possible.
4. **What is the restart procedure after an abend?** With no commit scope (BR-13) the answer
   determines whether double-posting is a live operational risk.
5. **Can `DALYTRAN-CARD-NUM` legitimately be blank or short?** Nothing validates the format; a
   blank card simply becomes reject 100.

# CBACT04C — Interest Calculation: Business Logic Map

**Audience:** a subject-matter expert who knows the credit-card business but does not read COBOL or Java.
**Sources:** only the static export in this repository — `app/cbl`, `app/cpy`, `app/jcl`, `app/asm`, `app/data`.
No mainframe was connected at any point; every statement below is cited to a file and line you can open yourself.

---

## 1. What this program does, in one paragraph

Once a month, CardDemo prices the balances a cardholder is carrying. Balances are not held as one
number per account: they are held per **transaction category** (purchases, cash advances, and so
on). For each of those category balances the program looks up the interest rate the customer was
disclosed, works out one month's interest on that category, writes an **interest transaction** so
the charge appears on the statement, and — once all of an account's categories are priced — adds
the total interest to the account balance and resets the current cycle's credit and debit totals
so the next cycle starts clean.

---

## 2. Where it runs: the JCL job context

| Item | Value | Source |
| --- | --- | --- |
| Job | `INTCALC` — "INTEREST CALCULATOR" | `app/jcl/INTCALC.jcl:1` |
| Step | `STEP15 EXEC PGM=CBACT04C,PARM='2022071800'` | `app/jcl/INTCALC.jcl:22` |
| Schedule | Control-M folder `MONTHLY-InterestCalculation`; runs after `CLOSEFIL`, followed by `COMBTRAN` | `app/scheduler/CardDemo.controlm` |

The `PARM` is a ten-character run date. It is not used as a date: it is used as the **prefix of
every generated transaction id** (`app/cbl/CBACT04C.cbl:476-479`), so a rerun with the same parm
produces the same ids.

### DD name → dataset mapping

| DD name | Dataset | Direction | Static dump used by the Java harness |
| --- | --- | --- | --- |
| `TCATBALF` | `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS` | in, read sequentially | `app/data/EBCDIC/AWS.M2.CARDDEMO.TCATBALF.PS` |
| `XREFFILE` | `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` | in, random by card number | `app/data/EBCDIC/AWS.M2.CARDDEMO.CARDXREF.PS` |
| `XREFFIL1` | `AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH` | in, the account-id alternate index over the same file | same dump |
| `ACCTFILE` | `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` | in **and** out (records are rewritten in place) | `app/data/EBCDIC/AWS.M2.CARDDEMO.ACCTDATA.PS` |
| `DISCGRP` | `AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS` | in, random by group/type/category | `app/data/EBCDIC/AWS.M2.CARDDEMO.DISCGRP.PS` |
| `TRANSACT` | `AWS.M2.CARDDEMO.SYSTRAN(+1)`, `RECFM=F LRECL=350` | out, new generation each run | produced by the Java job |

Source: `app/jcl/INTCALC.jcl:27-41`; file organisations and access modes at
`app/cbl/CBACT04C.cbl:28-56`.

---

## 3. The data the program works with

Copybook fields, their COBOL picture clauses, and the Java type used in
`modernization/interest-service`.

### 3.1 `CVTRA01Y` — transaction category balance (50 bytes, the driving file)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `TRANCAT-ACCT-ID` | `9(11)` | account number | `AccountId` (String, zero padded) |
| `TRANCAT-TYPE-CD` | `X(02)` | transaction type | `TransactionCategory.typeCode` |
| `TRANCAT-CD` | `9(04)` | transaction category | `TransactionCategory.categoryCode` (`int`) |
| `TRAN-CAT-BAL` | `S9(09)V99` | balance carried in that category | `BigDecimal`, scale 2 |
| `FILLER` | `X(22)` | unused | not modelled |

### 3.2 `CVACT03Y` — card cross-reference (50 bytes)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `XREF-CARD-NUM` | `X(16)` | card number, the primary key | `CardXref.cardNumber` |
| `XREF-CUST-ID` | `9(09)` | customer number | `CardXref.customerId` |
| `XREF-ACCT-ID` | `9(11)` | account number, the alternate key | `CardXref.accountId` |

### 3.3 `CVTRA02Y` — disclosure group, i.e. the rate card (50 bytes)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `DIS-ACCT-GROUP-ID` | `X(10)` | pricing group: `A000000000`, `ZEROAPR` or `DEFAULT` in the shipped rate card | `DisclosureGroupKey.accountGroupId` |
| `DIS-TRAN-TYPE-CD` | `X(02)` | transaction type | `DisclosureGroupKey.category` |
| `DIS-TRAN-CAT-CD` | `9(04)` | transaction category | `DisclosureGroupKey.category` |
| `DIS-INT-RATE` | `S9(04)V99` | **annual** rate as a percentage — `15.00` means 15% APR | `DisclosureGroup.annualRatePercent` (`BigDecimal`) |

### 3.4 `CVACT01Y` — account master (300 bytes)

| Field | PIC | Meaning | Java |
| --- | --- | --- | --- |
| `ACCT-ID` | `9(11)` | account number | `AccountId` |
| `ACCT-ACTIVE-STATUS` | `X(01)` | active flag | `String` (read only; the program never tests it) |
| `ACCT-CURR-BAL` | `S9(10)V99` | current balance — **updated** | `BigDecimal` |
| `ACCT-CREDIT-LIMIT`, `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | limits | `BigDecimal` |
| `ACCT-OPEN-DATE`, `ACCT-EXPIRAION-DATE`, `ACCT-REISSUE-DATE` | `X(10)` | dates as text | `String` |
| `ACCT-CURR-CYC-CREDIT`, `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | cycle totals — **reset to zero** | `BigDecimal` |
| `ACCT-ADDR-ZIP` | `X(10)` | postcode | `String` |
| `ACCT-GROUP-ID` | `X(10)` | the account's pricing group; drives rate selection | `Account.pricingGroupId` |

### 3.5 `CVTRA05Y` — transaction (350 bytes, the output record)

`TRAN-ID X(16)`, `TRAN-TYPE-CD X(02)`, `TRAN-CAT-CD 9(04)`, `TRAN-SOURCE X(10)`,
`TRAN-DESC X(100)`, `TRAN-AMT S9(09)V99`, `TRAN-MERCHANT-ID 9(09)`, `TRAN-MERCHANT-NAME X(50)`,
`TRAN-MERCHANT-CITY X(50)`, `TRAN-MERCHANT-ZIP X(10)`, `TRAN-CARD-NUM X(16)`,
`TRAN-ORIG-TS X(26)`, `TRAN-PROC-TS X(26)`, `FILLER X(20)` → `InterestTransaction`.

### 3.6 How the numbers are physically stored, and why it matters

* **Zoned decimal (`DISPLAY`)** — all five copybooks above store numbers as characters, one digit
  per byte, with the sign "overpunched" into the last digit: `1941J` is `-194.19`, `1941{` is
  `+194.10`. Decoded and encoded by `ZonedDecimalCodec`.
* **Packed decimal (`COMP-3`)** — two digits per byte plus a sign nibble. Not used by this sliver's
  five copybooks, but pervasive elsewhere in CardDemo, so the reader ships a reusable
  `PackedDecimalCodec` with the same interface; the next sliver gets it for free.
* **`V` is an implied decimal point** — it occupies no byte. `S9(09)V99` is eleven digit characters
  holding a value with two decimal places.
* **Truncation, not rounding.** A COBOL `COMPUTE` without the `ROUNDED` keyword discards excess
  decimals toward zero, and a receiving field silently drops excess *leading* digits unless
  `ON SIZE ERROR` is coded. Neither appears in this program, so both behaviours are live.
  `CobolFixedPoint.truncate`/`fit` reproduce them; `BigDecimal` with explicit scales is used
  everywhere and `double` appears nowhere.

---

## 4. The business rules

Each rule is stated in business terms, then cited.

### BR-1 — Work one account at a time, in account order

The category-balance file is read from start to finish in key order
(`app/cbl/CBACT04C.cbl:28-32, 325-348`). Every time the account number changes, the previous
account is finished off and a new one is started; interest accumulated so far is reset to zero
(`app/cbl/CBACT04C.cbl:194-207`).

*Java:* `AccountBalanceGrouping.groupConsecutively` turns the stream into per-account groups;
`InterestAccrualService.accrue` drives them.

### BR-2 — On each new account, fetch the account and one card number

The account master record is read by account number (`1100-GET-ACCT-DATA`,
`app/cbl/CBACT04C.cbl:372-391`) and the card cross-reference is read by account number through the
alternate index (`1110-GET-XREF-DATA`, `app/cbl/CBACT04C.cbl:393-413`). If either is missing the
job abends — there is no skip-and-continue path (`app/cbl/CBACT04C.cbl:381-390`, `402-412`).

*Java:* `AccountRepository` / `CardXrefRepository`, throwing `AccountNotFoundException` /
`CardXrefNotFoundException`. An account can have several cards; the alternate index returns the
lowest card number, which the in-memory repository reproduces by sorting on card number.

### BR-3 — Choose the interest rate: own pricing group first, `DEFAULT` second

The rate is looked up on the three-part key **account pricing group + transaction type +
transaction category** (`1200-GET-INTEREST-RATE`, `app/cbl/CBACT04C.cbl:415-440`). If that
combination is not on the rate card, the lookup is retried with the pricing group replaced by the
literal `DEFAULT` (`app/cbl/CBACT04C.cbl:427-435` and `1200-A-GET-DEFAULT-INT-RATE`,
`app/cbl/CBACT04C.cbl:443-460`). If even the `DEFAULT` row is missing, the job abends.

Note the fallback is per **category**, not per account: an account can take its own rate for
purchases and the `DEFAULT` rate for a category its group does not price.

In the shipped data (`app/data/EBCDIC/AWS.M2.CARDDEMO.DISCGRP.PS`, 51 rows) the only pricing
groups on the rate card are `A000000000`, `ZEROAPR` and `DEFAULT`; every shipped account has a
**blank** `ACCT-GROUP-ID`, so in practice today every rate comes from `DEFAULT` and the
`A000000000` and `ZEROAPR` rows are dormant.

*Java:* `DisclosureGroupRateResolver` (`RateResolver` interface), backed by
`DisclosureGroupRepository`, throwing `DisclosureGroupNotFoundException`.

### BR-4 — A zero rate means no interest and no transaction

If the disclosed rate is zero, the program neither accrues interest nor writes a transaction
(`IF DIS-INT-RATE NOT = 0`, `app/cbl/CBACT04C.cbl:214-217`). This is what the `ZEROAPR` pricing
group is for: a promotional 0% account produces no statement noise at all.

*Java:* the rate check in `InterestAccrualService.accrueAccount`.

### BR-5 — Monthly interest = balance × annual rate ÷ 1200

`COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200`
(`1300-COMPUTE-INTEREST`, `app/cbl/CBACT04C.cbl:462-467`). The 1200 is 12 months × the 100 that
turns a percentage into a fraction. The result is held in `S9(09)V99`, so it is **truncated to two
decimals toward zero, never rounded**: a balance of 79.60 at 15% gives exactly 0.995, and the
program keeps **0.99**. Interest is accrued on the raw category balance, including negative
(credit) balances, which produce negative interest.

Each category's interest is added to a running total for the account
(`ADD WS-MONTHLY-INT TO WS-TOTAL-INT`, `app/cbl/CBACT04C.cbl:466`).

*Java:* `MonthlyInterestCalculator.monthlyInterest` / `.accumulate`.

### BR-6 — Post the account: add total interest, clear the cycle totals

When the account is finished: `ACCT-CURR-BAL` is increased by the total interest, and
`ACCT-CURR-CYC-CREDIT` and `ACCT-CURR-CYC-DEBIT` are set to zero; the record is rewritten in place
(`1050-UPDATE-ACCOUNT`, `app/cbl/CBACT04C.cbl:350-370`). Nothing else on the account changes.

*Java:* `Account.withInterestPosted`, which returns a new immutable account.

### BR-7 — One interest transaction per priced category

Every non-zero-rate category produces one 350-byte transaction record
(`1300-B-WRITE-TX`, `app/cbl/CBACT04C.cbl:473-515`):

| Field | Value | Line |
| --- | --- | --- |
| `TRAN-ID` | run-date parm + a six-digit counter, restarting at `000001` each run | `476-479` |
| `TRAN-TYPE-CD` | `01` | `481` |
| `TRAN-CAT-CD` | `05` (the "interest" category) | `482` |
| `TRAN-SOURCE` | `System` | `483` |
| `TRAN-DESC` | `Int. for a/c ` + account number | `484-488` |
| `TRAN-AMT` | that category's monthly interest | `489` |
| `TRAN-MERCHANT-*` | merchant id zero, name/city/zip blank | `490-493` |
| `TRAN-CARD-NUM` | the card from BR-2 | `494` |
| `TRAN-ORIG-TS`, `TRAN-PROC-TS` | the same timestamp, `YYYY-MM-DD-HH.MM.SS.hh0000` | `495-497`, `613-626` |

Note the counter is a **global** run counter, not per account, and the timestamp is taken per
transaction from the system clock.

*Java:* `InterestTransactionFactory` with `TransactionIdSequence` and `Db2TimestampFormatter`
(the latter takes a `java.time.Clock`, so the parity harness can freeze it).

### BR-8 — Known defect: the last account of the run is never posted

The account is rewritten only when the *next* record belongs to a different account
(`app/cbl/CBACT04C.cbl:194-199`). The catch-up branch intended to post the final account at end of
file (`app/cbl/CBACT04C.cbl:219-221`) can never execute, because `PERFORM UNTIL END-OF-FILE = 'Y'`
re-tests its condition the moment the read sets the flag and leaves the loop. So the final
account's interest transactions **are** written, but its balance is **not** updated — the customer
is billed on the statement while the ledger stays behind.

This is a genuine production defect, not a modernisation artefact. The Java module reproduces it by
default and makes it a named, auditable decision rather than an accident:
`FinalAccountPolicy.MAINFRAME_PARITY` (default, bug-for-bug) versus
`FinalAccountPolicy.POST_FINAL_ACCOUNT` (the corrected behaviour, once the business signs it off).

### BR-9 — Fees are not implemented

`1400-COMPUTE-FEES` (`app/cbl/CBACT04C.cbl:518-520`) contains only the comment
"To be implemented". No fee is ever computed. The Java module deliberately implements nothing here;
inventing fee logic would be inventing business rules.

### BR-10 — Operational behaviour that does not survive as business logic

Opening and closing the five files with file-status checks
(`app/cbl/CBACT04C.cbl:234-323`, `522-611`), `9910-DISPLAY-IO-STATUS`
(`app/cbl/CBACT04C.cbl:635-648`) and `9999-ABEND-PROGRAM`'s `CALL 'CEE3ABD'`
(`app/cbl/CBACT04C.cbl:628-633`) are plumbing, not rules. In Java they become repository
lifecycles and typed exceptions: a failure throws `InterestBatchException` (or a subclass) and the
run fails, exactly as an abend fails the job step, but with a message instead of a `U3000` and a
dump.

---

## 5. Assembler dependency chain

`app/asm` contains two Assembler programs. **Neither is reachable from CBACT04C** — verified by
searching every COBOL source in `app/cbl` for `CALL` statements:

| Assembler program | Called from | Purpose | Handling in Java |
| --- | --- | --- | --- |
| `COBDATFT` (`app/asm/COBDATFT.asm`) | `CBACT01C` only (`app/cbl/CBACT01C.cbl:231`) | date formatting/conversion service | **Not required by this sliver.** When a future sliver needs it, `java.time` with an explicit `DateTimeFormatter` replaces it outright; there is no reason to port the Assembler. |
| `MVSWAIT` (`app/asm/MVSWAIT.asm`) | `COBSWAIT` only (`app/cbl/COBSWAIT.cbl:38`) | issues an MVS timed wait, i.e. sleeps | **Not required by this sliver.** It is a test/pacing utility with no business meaning; the Java equivalent, where ever needed, is scheduler configuration, not code. |

CBACT04C's own external call is `CEE3ABD` (`app/cbl/CBACT04C.cbl:632`), the Language Environment
abend service — replaced by throwing an exception, see BR-10. Its date handling is
`FUNCTION CURRENT-DATE` (`app/cbl/CBACT04C.cbl:614`), replaced by an injected
`java.time.Clock`, not by `COBDATFT`.

So the Assembler chain for this sliver is **empty**, and that is a finding worth stating explicitly:
a disconnected-mainframe engagement must prove the absence of a dependency, not assume it.

---

## 6. Rule-to-code index

| Rule | COBOL | Java |
| --- | --- | --- |
| BR-1 account break | `CBACT04C.cbl:188-222` | `service/AccountBalanceGrouping.java`, `service/InterestAccrualService.java` |
| BR-2 lookups | `CBACT04C.cbl:372-413` | `repository/AccountRepository.java`, `repository/CardXrefRepository.java` |
| BR-3 rate + `DEFAULT` | `CBACT04C.cbl:415-460` | `rules/DisclosureGroupRateResolver.java` |
| BR-4 zero rate | `CBACT04C.cbl:214-217` | `service/InterestAccrualService.java` |
| BR-5 interest maths | `CBACT04C.cbl:462-468` | `rules/MonthlyInterestCalculator.java`, `io/CobolFixedPoint.java` |
| BR-6 account posting | `CBACT04C.cbl:350-370` | `domain/Account.java` |
| BR-7 transaction creation | `CBACT04C.cbl:473-515`, `613-626` | `rules/InterestTransactionFactory.java`, `rules/TransactionIdSequence.java`, `rules/Db2TimestampFormatter.java` |
| BR-8 final-account defect | `CBACT04C.cbl:194-199`, `219-221` | `service/FinalAccountPolicy.java` |
| BR-9 fees | `CBACT04C.cbl:518-520` | intentionally absent |
| BR-10 file status / abend | `CBACT04C.cbl:234-323`, `628-648` | `exception/InterestBatchException.java` and subclasses |
| Record layouts | `app/cpy/CVTRA01Y.cpy`, `CVACT03Y.cpy`, `CVTRA02Y.cpy`, `CVACT01Y.cpy`, `CVTRA05Y.cpy` | `io/layout/CardDemoLayouts.java` and `io/codec/*` |

---

## 7. Questions this static export cannot answer

Recorded honestly, because a disconnected engagement must be explicit about its blind spots:

1. **Is the final-account defect (BR-8) already known and compensated downstream?** `COMBTRAN`
   runs next in the Control-M chain; whether it re-derives balances is out of this sliver's scope.
2. **Are `ACCT-GROUP-ID` values really blank in production, or is the export misaligned?**
   `ACCT-GROUP-ID` (offset 112) is blank in every shipped account record, which would mean the
   `A000000000` and `ZEROAPR` rate rows are dormant and everything prices off `DEFAULT`. Note that
   the value `A000000000` does appear one field earlier, in `ACCT-ADDR-ZIP` (offset 102), on every
   record — so the pricing group may be present but shifted. Confirm with the business whether the
   blank group is real or an artefact of the dump.
3. **Is negative (credit-balance) interest intended?** The code computes it without comment.
4. **Should interest be truncated rather than rounded?** It is today, to the cent, in the customer's
   favour on debit balances. Reproduced deliberately; worth a policy decision.
5. **Which duplicate does the card cross-reference alternate index return?** An account can hold
   several cards. `1110-GET-XREF-DATA` takes whatever the `XREFFIL1` `NONUNIQUEKEY` path returns
   first; VSAM holds duplicates in insertion order, which the static export does not record. Both
   the Java repository and the parity oracle assume the lowest card number. The shipped `CARDXREF`
   has one card per account, so the assumption is currently untestable.
6. **Should a blank zoned numeric field be tolerated?** `ZonedDecimalCodec` reads spaces as zero;
   a mainframe `COMPUTE` over a blank `DISPLAY` field normally raises a data exception (S0C7). No
   shipped record is blank, so the two behaviours are indistinguishable on this data.

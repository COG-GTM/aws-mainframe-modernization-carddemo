# 2b. Module documentation — CBACT04C (interest calculation)

Source: `app/cbl/CBACT04C.cbl` (652 lines). Job: `app/jcl/INTCALC.jcl` STEP15,
`PARM='2022071800'`. Control-M folder `MONTHLY-InterestCalculation` (`CardDemo.controlm` line 69).

## Business purpose (lines 3-6, 180-232)

Walks the transaction-category-balance file in key order (account, type, category). For each
balance row it finds the interest rate for the account's disclosure group (falling back to the
`DEFAULT` group), computes one month's interest on that balance, writes an interest transaction
to a new sequential "system transactions" file, and at each account boundary adds the account's
total interest to its current balance and zeroes both cycle buckets. Fees are a declared but
unimplemented step.

## Inputs and outputs (FILE-CONTROL lines 27-56; JCL lines 27-41)

| DD | Org / access | Open mode (lines) | Layout | LRECL |
|---|---|---|---|---|
| `TCATBALF` | indexed, **sequential** access, key `FD-TRAN-CAT-KEY` | INPUT (234-251) | CVTRA01Y | 50 |
| `XREFFILE` (+ `XREFFIL1` AIX path) | indexed, random, primary `FD-XREF-CARD-NUM`, `ALTERNATE RECORD KEY IS FD-XREF-ACCT-ID` (line 38; no `WITH DUPLICATES` clause — see OQ-08) | INPUT (252-269) | CVACT03Y | 50 |
| `ACCTFILE` | indexed, random, key `FD-ACCT-ID` | I-O (289-306) | CVACT01Y | 300 |
| `DISCGRP` | indexed, random, key `FD-DISCGRP-KEY` = group(10)+type(2)+cat(4) (lines 76-82) | INPUT (270-288) | CVTRA02Y | 50 |
| `TRANSACT` | sequential | OUTPUT (307-324) → `SYSTRAN(+1)` | CVTRA05Y | 350 |
| `PARM` | `EXTERNAL-PARMS`: `PARM-LENGTH S9(4) COMP`, `PARM-DATE X(10)` (176-178) | — | — | — |

`PARM-DATE` receives the first 10 bytes of the JCL PARM. With `PARM='2022071800'` it is the
literal `2022071800`; it is used only as the prefix of generated transaction IDs (line 476).
Its intended semantics (`YYYYMMDDHH`? `YYYYMMDD` + `00`?) are not stated. OQ-09.

## Record layouts

DIS-GROUP-RECORD (CVTRA02Y, 50): `DIS-ACCT-GROUP-ID X(10)` @0, `DIS-TRAN-TYPE-CD X(02)` @10,
`DIS-TRAN-CAT-CD 9(04)` @12, `DIS-INT-RATE S9(04)V99` @16, FILLER X(28) @22.
TRAN-CAT-BAL-RECORD, ACCOUNT-RECORD, CARD-XREF-RECORD, TRAN-RECORD: see `03-data-model.md`.

## Processing flow (lines 181-232)

1. Open five files (182-186).
2. Loop until EOF (188-223). `1000-TCATBALF-GET-NEXT` (325-349): status `10` → EOF; other
   non-`00` → abend.
3. For each row (191-217):
   * Control break on `TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM` (194): if not the first record,
     `1050-UPDATE-ACCOUNT` for the *previous* account (195-196); then `WS-TOTAL-INT = 0`,
     remember account id, `1100-GET-ACCT-DATA`, `1110-GET-XREF-DATA` by alternate key (200-205).
   * Build disclosure key from `ACCT-GROUP-ID`, `TRANCAT-CD`, `TRANCAT-TYPE-CD` (210-212),
     `1200-GET-INTEREST-RATE` (213).
   * **Only if `DIS-INT-RATE NOT = 0`** (214): `1300-COMPUTE-INTEREST` then `1400-COMPUTE-FEES`.
     A zero rate produces no transaction and no addition to the account total.
4. At EOF (`ELSE` branch, 221-222): `1050-UPDATE-ACCOUNT` for the last account.
5. Close, display, `GOBACK` (225-232). No `RETURN-CODE` is set anywhere.

## Business rules

| Rule | Lines | Notes |
|---|---|---|
| Monthly interest `WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200` | 464-465 | `DIS-INT-RATE` is an annual percentage (`/100` for percent, `/12` for month). Result is stored in `S9(09)V99` (line 168); COBOL `COMPUTE` without `ROUNDED` **truncates** to 2 decimals. |
| Account total `WS-TOTAL-INT += WS-MONTHLY-INT` | 467 | Sum of truncated monthly amounts, not truncation of the sum. |
| Rate lookup: group = `ACCT-GROUP-ID`; `INVALID KEY` (status 23) → retry with group `DEFAULT` | 415-441 | Any status other than `00`/`23` on the first read abends. A blank `ACCT-GROUP-ID` (as in the shipped sample data) is looked up as 10 spaces, misses, and falls to `DEFAULT`. |
| Default-group miss abends | 443-460 | `ERROR READING DEFAULT DISCLOSURE GROUP`. |
| Generated transaction | 473-516 | `TRAN-ID = PARM-DATE(10) ‖ WS-TRANID-SUFFIX 9(06)` (474-479; suffix is a program-wide counter, so IDs are unique within one run only, OQ-10); `TYPE-CD='01'`, `CAT-CD='05'`, `SOURCE='System'`, `DESC='Int. for a/c ' ‖ ACCT-ID`, `AMT=WS-MONTHLY-INT`, `MERCHANT-ID=0`, merchant name/city/zip spaces, `CARD-NUM=XREF-CARD-NUM`, `ORIG-TS=PROC-TS=` current timestamp (497-499). `TRAN-CAT-CD` is `PIC 9(04)` but receives the alphanumeric literal `'05'` (482); under the standard alphanumeric-to-numeric MOVE rule the sender is treated as an unsigned integer, giving `0005`. The Java uses 5. Recorded as OQ-11 because it is a compiler rule, not something the source states. |
| Card on generated transaction | 393-413 | `READ XREF-FILE KEY IS FD-XREF-ACCT-ID` returns the **first** XREF record for the account in alternate-index order (base key = card number). Accounts with several cards get all interest posted to the lowest card number. |
| Account update at control break / EOF | 350-371 | `ADD WS-TOTAL-INT TO ACCT-CURR-BAL`, `ACCT-CURR-CYC-CREDIT = 0`, `ACCT-CURR-CYC-DEBIT = 0`, `REWRITE`; non-`00` abends. The cycle reset happens **regardless of whether any interest was computed**. |
| EOF with zero input records | 221-222 | `1050-UPDATE-ACCOUNT` runs against an `ACCOUNT-RECORD` that was never read (`WS-FIRST-TIME` still `'Y'`). The REWRITE of uninitialised storage would fail with a non-`00` status → abend, or, if storage happens to hold a valid key, corrupt that account. OQ-12. |
| Fees | 518-520 | `1400-COMPUTE-FEES` is `* To be implemented` — no-op. |

## Error and abend paths

| Condition | Lines | Message | Outcome |
|---|---|---|---|
| Any OPEN ≠ `00` | 234-324 | `ERROR OPENING <file>` | abend U0999 |
| TCATBALF READ ∉ {`00`,`10`} | 325-349 | `ERROR READING TRANSACTION CATEGORY FILE` | abend |
| ACCTFILE READ ≠ `00` (incl. not found) | 372-392 | `ACCOUNT NOT FOUND: id` + `ERROR READING ACCOUNT FILE` | abend |
| XREF alt-key READ ≠ `00` | 393-413 | `ACCOUNT NOT FOUND: id` + `ERROR READING XREF FILE` | abend |
| DISCGRP READ ∉ {`00`,`23`} | 415-441 | `ERROR READING DISCLOSURE GROUP FILE` | abend |
| DEFAULT DISCGRP READ ≠ `00` | 443-460 | `ERROR READING DEFAULT DISCLOSURE GROUP` | abend |
| SYSTRAN WRITE ≠ `00` | 501-516 | `ERROR WRITING TRANSACTION RECORD` | abend |
| ACCTFILE REWRITE ≠ `00` | 356-370 | `ERROR RE-WRITING ACCOUNT FILE` | abend |
| Any CLOSE ≠ `00` | 522-612 | `ERROR CLOSING <file>` | abend |

`9999-ABEND-PROGRAM` (628-633) and `9910-DISPLAY-IO-STATUS` (635-650) are byte-identical in
behaviour to CBTRN02C's. There is no restart/checkpoint: an abend mid-file leaves some accounts
updated and some not, and SYSTRAN partially written. OQ-07.

# Ledger-Summary Interface (GLSUMM01)

Producer: batch program `CBGLX01C` (`app/cbl/CBGLX01C.cbl`), job `GLEXTRCT` (`app/jcl/GLEXTRCT.jcl`).
Record layouts: `app/cpy/CVGLHDR.cpy`, `app/cpy/CVGLDTL.cpy`, `app/cpy/CVGLTRL.cpy`; control card `app/cpy/CVGLCTL.cpy`.
Tests: `tests/cbglx01c/` (GnuCOBOL, byte-for-byte expected outputs; see
`tests/cbglx01c/README.md` for running the suite and its cases).

This step turns posted activity into an outbound ledger-summary file for a downstream
accounting system. Each run produces two artifacts:

1. **GLSUMOUT** - the fixed-width interface file: one header, one detail per
   account / transaction type / category with activity, one trailer that carries
   control totals.
2. **GLRECRPT** - a reconciliation report showing that source totals, the detail
   records re-read from the written file, and the trailer all agree, and that each
   key's posted activity ties to its category balance. The report ends in the step
   return code.

## 1. Data flow

```
TRANSACT (KSDS, CVTRA05Y) --REPRO--> TRANFILE (FB 350) --+
CARDXREF (KSDS, CVACT03Y) --REPRO--> XREFFILE (FB  50) --+--> CBGLX01C --> GLSUMOUT (FB 150) --[RC<=4]--> outbound copy
TCATBALF (KSDS, CVTRA01Y) --REPRO--> TCATBALF (FB  50) --+         |
GLPARM control card (FB 80, instream) -------------------+         +--> GLRECRPT (FB 132) reconciliation report
```

* TRANSACT has a card number but no account number, so each transaction's account
  comes from the card cross-reference (CARDXREF), the same lookup POSTTRAN
  (`CBTRN02C`) makes before it updates TCATBALF.
* The program reads flat sequential unloads of the three KSDS files and never touches
  the VSAM clusters, so the same source runs under GnuCOBOL for tests. The JCL handles
  the REPRO unloads.
* Internal `SORT` orders accepted activity by account, type, category and TRAN-ID. A
  control break then builds one detail per key, and a matched merge against TCATBALF
  (already in key order) produces the tie-out. The program keeps no in-memory key
  table, so the number of keys is limited only by sort work space.

## 2. Where the step runs in the daily batch order

The documented daily order is in `scripts/run_full_batch.sh`. That script and the existing JCL are **not modified**. GLEXTRCT goes here:

| Order | Job | Purpose |
|---|---|---|
| 1-11 | CLOSEFIL ... DUSRSECJ | Close CICS files, refresh master and reference data (TRANBKP leaves TRANSACT empty; TCATBALF reloads with zero balances) |
| 12 | POSTTRAN | Post daily transactions: writes TRANSACT and adds the same amount to TCATBALF |
| **12a** | **GLEXTRCT** | **Ledger-summary interface plus reconciliation (this step)** |
| 13 | INTCALC | Interest calculation (writes system transactions to a GDG) |
| 14-15 | TRANBKP, COMBTRAN | Back up TRANSACT and merge in the system transactions |
| 16 | TRANIDX, OPENFIL | AIX rebuild, reopen CICS files |

Why it runs right after POSTTRAN:

* After POSTTRAN, every TRANSACT record has been added to the TCATBALF key
  `(XREF-ACCT-ID, TRAN-TYPE-CD, TRAN-CAT-CD)` by the same amount (`CBTRN02C` paragraph
  2700). The refresh in jobs 6 and 8 starts the day with an empty TRANSACT and
  zero balances, so **net activity per key equals that key's TCATBALF balance**.
  That equality is the ledger tie-out.
* COMBTRAN (job 15) merges INTCALC's interest transactions into TRANSACT. TCATBALF
  does not get those amounts, so a run after COMBTRAN would not tie.
* CICS files are still closed at this point (CLOSEFIL ran in job 1), so the
  REPRO unloads see a stable copy.

Scope note: this tie-out assumes TCATBALF starts each cycle at zero, which is how
the documented CardDemo daily cycle refreshes it. A ledger that carries balances
forward needs an "opening balance + net activity = closing balance" tie that reads
the prior-cycle backup. That extension is **not implemented** here.

## 3. Encoding and representation

| Topic | Rule |
|---|---|
| Character set | On z/OS the file is EBCDIC (code page 037/1140). Every field is character data: letters, digits, space, and `+`/`-`. Converting to ASCII (ISO-8859-1) with a standard code-page table is a 1:1 byte mapping, so text-mode transfer is lossless. The GnuCOBOL test outputs are ASCII. |
| No packed or binary fields | Internal accumulators are `COMP-3` (packed decimal), but each value is MOVEd to a display field before it is written. The file has no `COMP`/`COMP-3` bytes, and those cannot survive a code-page conversion. |
| Signed amounts | `PIC S9(n)V99 SIGN IS LEADING SEPARATE CHARACTER`: byte 1 is `+` or `-` and the digits follow. The decimal point is implied (2 decimals). Zero is written as `+`. The format avoids trailing overpunch (zoned sign in the last digit's zone), where the same value is `{`/`A`-`I`/`}`/`J`-`R` in EBCDIC and other bytes in ASCII environments. |
| Source amounts | `TRAN-AMT` and `TRAN-CAT-BAL` are zoned decimal with trailing overpunch (`S9(09)V99` DISPLAY) in the CardDemo files. They are not COMP-3. The program reads them natively. GnuCOBOL tests compile with `-fsign=EBCDIC` so ASCII fixtures use the same overpunch convention as `app/data/ASCII/*.txt`. |
| Record format | RECFM=FB, LRECL=150. No record delimiters: under GnuCOBOL the file is a flat byte stream of 150-byte records. |
| Collating order | Details are in ascending `(ACCT-ID, TYPE-CD, CAT-CD)` order under the platform's native collating sequence. CardDemo type codes are numeric (`01`-`07`), so EBCDIC and ASCII give the same order. A type code with letters would sort differently on the two platforms (in EBCDIC, letters sort before digits). |
| Dates | Calendar logic is plain COBOL (numeric checks, month range, days-in-month, Gregorian leap rule: divisible by 4, not by 100 unless also by 400). The program uses no LE callable services (no CEEDAYS) and no `FUNCTION CURRENT-DATE`. The run date comes from the control card, so outputs are reproducible. |

## 4. Control card (GLPARM, FB 80) - `CVGLCTL`

| Pos | Len | Field | Picture | Rule |
|---|---|---|---|---|
| 1 | 10 | CTL-RUN-DATE | X(10) `CCYY-MM-DD` | Required, a valid calendar date, year 1900-2099. Becomes header RUN-DATE `CCYYMMDD`. |
| 11 | 1 | filler | X | |
| 12 | 8 | CTL-SOURCE-SYS-ID | X(8) | Required, not all spaces. Copied to the header. |
| 20 | 1 | filler | X | |
| 21 | 9 | CTL-EXPECTED-COUNT | 9(9) or spaces | Optional feeder control total: expected number of accepted transactions. |
| 30 | 1 | filler | X | |
| 31 | 16 | CTL-EXPECTED-NET | S9(13)V99 SIGN LEADING SEPARATE or spaces | Optional feeder control total: expected net amount (debits minus credits) of accepted transactions. |
| 47 | 34 | filler | X(34) | Ignored |

The two expected fields must both be blank or both be valid. Anything else, a missing card, or an
invalid run date or source system id is a control error (RC 12, `E02`).

## 5. Record layouts (LRECL 150)

Positions are 1-based. `SLS` = `SIGN IS LEADING SEPARATE CHARACTER`. Amounts carry 2 implied decimals.

### 5.1 Header - `CVGLHDR` (exactly one, first record)

| Pos | Len | Field | Picture | Content |
|---|---|---|---|---|
| 1 | 1 | GLH-REC-TYPE | X | `H` |
| 2 | 8 | GLH-INTERFACE-ID | X(8) | `GLSUMM01` |
| 10 | 8 | GLH-SOURCE-SYS-ID | X(8) | From the control card |
| 18 | 8 | GLH-RUN-DATE | 9(8) | `CCYYMMDD` from the control card |
| 26 | 4 | GLH-LAYOUT-VERSION | X(4) | `0100` |
| 30 | 121 | filler | X(121) | Spaces |

### 5.2 Detail - `CVGLDTL` (zero or more, ascending key)

| Pos | Len | Field | Picture | Content |
|---|---|---|---|---|
| 1 | 1 | GLD-REC-TYPE | X | `D` |
| 2 | 9 | GLD-SEQ-NO | 9(9) | 1, 2, 3, ... with no gaps |
| 11 | 11 | GLD-ACCT-ID | 9(11) | From CARDXREF via TRAN-CARD-NUM |
| 22 | 2 | GLD-TYPE-CD | X(2) | TRAN-TYPE-CD |
| 24 | 4 | GLD-CAT-CD | 9(4) | TRAN-CAT-CD |
| 28 | 9 | GLD-TRAN-COUNT | 9(9) | Accepted transactions in this key |
| 37 | 16 | GLD-DEBIT-AMT | S9(13)V99 SLS | Sum of TRAN-AMT >= 0 (always `+`) |
| 53 | 16 | GLD-CREDIT-AMT | S9(13)V99 SLS | Absolute value of the sum of TRAN-AMT < 0 (always `+`) |
| 69 | 16 | GLD-NET-AMT | S9(13)V99 SLS | DEBIT - CREDIT |
| 85 | 16 | GLD-CATBAL-AMT | S9(13)V99 SLS | TCATBALF TRAN-CAT-BAL for the key (`+0` if absent) |
| 101 | 1 | GLD-TIE-STATUS | X | `T` NET = CATBAL; `D` differs; `M` key missing from TCATBALF |
| 102 | 49 | filler | X(49) | Spaces |

Sign convention: a non-negative TRAN-AMT (purchase) is a **debit** to the cardholder
receivable. A negative TRAN-AMT (payment, return) is a **credit**. A zero amount is
counted in TRAN-COUNT and adds 0 to debits.

### 5.3 Trailer - `CVGLTRL` (exactly one, last record)

| Pos | Len | Field | Picture | Content |
|---|---|---|---|---|
| 1 | 1 | GLT-REC-TYPE | X | `T` |
| 2 | 9 | GLT-DETAIL-COUNT | 9(9) | Number of `D` records |
| 11 | 11 | GLT-TRAN-COUNT | 9(11) | Sum of GLD-TRAN-COUNT |
| 22 | 18 | GLT-TOTAL-DEBITS | S9(15)V99 SLS | Sum of GLD-DEBIT-AMT |
| 40 | 18 | GLT-TOTAL-CREDITS | S9(15)V99 SLS | Sum of GLD-CREDIT-AMT |
| 58 | 18 | GLT-TOTAL-NET | S9(15)V99 SLS | DEBITS - CREDITS |
| 76 | 18 | GLT-HASH-TOTAL | 9(18) | Sum of GLD-ACCT-ID over all `D` records (non-monetary hash) |
| 94 | 9 | GLT-RECORD-COUNT | 9(9) | Header + details + trailer = DETAIL-COUNT + 2 |
| 103 | 48 | filler | X(48) | Spaces |

### 5.4 What a receiving system should validate

1. Every record is 150 bytes and byte 1 is `H`, `D` or `T`. There is exactly one `H`, it comes first, there is exactly one `T`, it comes last, and every record in between is a `D`.
2. The header has `GLSUMM01`, layout `0100`, an expected source id, and a valid `CCYYMMDD` run date that has not been processed before (duplicate-file check).
3. Detail sequence numbers run 1..n with no gaps, and keys are strictly ascending.
4. Numeric fields are all digits. Signed fields start with `+`/`-` and the rest are digits. DEBIT and CREDIT are `+`.
5. Each detail satisfies NET = DEBIT - CREDIT.
6. Trailer: DETAIL-COUNT = number of D records, RECORD-COUNT = total records, TRAN-COUNT / DEBITS / CREDITS / HASH = the sums of the details, NET = DEBITS - CREDITS.
7. Reject the whole file if any check fails, if the trailer is missing (the producer aborted, see RC 12), or if the file arrives without the producer's RC <= 4 release (the JCL gate below).

## 6. Validation, exceptions and return codes

The checks on each TRANSACT record run in this order. The first failing check decides the outcome.

| Code | Condition | Effect |
|---|---|---|
| E03 | TRAN-ID lower than the previous record's TRAN-ID (input not in KSDS key order) | Abort, RC 12 |
| R01 | TRAN-ID equal to the previous one (duplicate key) | Rejected (excluded), RC >= 4 |
| R02 | TRAN-AMT not numeric | Rejected, RC >= 4 |
| R03 | TRAN-CAT-CD not numeric | Rejected, RC >= 4 |
| R04 | TRAN-CARD-NUM not in CARDXREF | Rejected, RC >= 4 |
| W01 | TRAN-PROC-TS(1:10) not a valid `CCYY-MM-DD` date | Included, RC >= 4 |
| W02 | Valid TRAN-PROC-TS date later than the run date | Included, RC >= 4 |

Records that pass the R checks are *accepted*. W01 and W02 can both be raised on accepted records,
but a record raises at most one of them (W02 only applies to a valid date). The record still goes into
the interface because it was posted to the ledger, and leaving it out would break the tie-out.
The report lists the record so the feeder can correct the date.

Run-level conditions:

| Code | Condition | RC |
|---|---|---|
| W03 | TRANFILE has no records (header and zero-total trailer still written) | 4 |
| B01 | Key NET != TCATBALF balance (detail `D`) | 8 |
| B02 | Key with activity missing from TCATBALF (detail `M`) | 8 |
| B03 | TCATBALF key with a non-zero balance and no accepted activity | 8 |
| B04 | Control-card expected count != accepted count | 8 |
| B05 | Control-card expected net != accepted net | 8 |
| B06 | Re-read of the closed GLSUMOUT disagrees with the source totals or with its own trailer (structure, counts, sums, hash) | 8 |
| E01 | File open/read/write error (report shows DD name and file status) | 12 |
| E02 | Control card missing or invalid | 12 |
| E04 | TCATBALF out of key sequence, duplicate key, or non-numeric balance | 12 |
| E05 | CARDXREF out of card-number sequence, duplicate, non-numeric account, or more than 10,000 entries | 12 |
| E06 | Amount or hash overflow: a key's debits or credits exceed S9(13)V99, a run total exceeds S9(15)V99, or the hash exceeds 9(18) | 12 |

The return code is the highest severity raised: **0** balanced and clean, **4** balanced
with warnings or rejects, **8** out of balance, **12** file or control error.

On RC 12 the program stops processing, closes its files, and writes a report that ends in
`RUN ABORTED` with the reason. The trailer is **never** written on an abort. GLSUMOUT holds
only the records written before the failure, or does not exist if the failure came before it
was opened (E01 on input open, E02). A receiver rejects a file with no trailer (check 7).

## 7. Reconciliation report (GLRECRPT, FB 132)

Sections, in order:

1. Title, run date, source system id.
2. **Input counts**: TRANSACT read / accepted / rejected / warned, XREF entries loaded, TCATBALF records read.
3. **Exceptions**: code, TRAN-ID or key, and description, up to 50 lines, then `nnnnnnnnn MORE EXCEPTIONS NOT LISTED`.
4. **Control totals, three ways**: rows TRANSACTIONS, DEBITS, CREDITS, NET, DETAIL RECORDS, HASH TOTAL; columns
   SOURCE (accumulated while reading accepted input; DETAIL RECORDS and HASH TOTAL come from the build pass),
   FILE DETAIL (sum of `D` records re-read from the closed GLSUMOUT), FILE TRAILER (values from the re-read `T` record), and
   a status column (`OK` / `DIFF`).
5. **Ledger tie-out (activity vs TCATBALF)**: keys tied / differing / missing / balance-only-non-zero.
6. **Feeder control totals**, when supplied: expected vs actual count and net.
7. **Result**: `BALANCED`, `BALANCED WITH WARNINGS`, `OUT OF BALANCE`, or `RUN ABORTED - <reason>`, followed by the return code.

## 8. JCL release gate

`GLEXTRCT` writes `AWS.M2.CARDDEMO.GLSUMM.EXTRACT`. A later IEBGENER step, with
`COND=(4,LT,STEP30)`, copies it to `AWS.M2.CARDDEMO.GLSUMM.OUTBOUND`, the dataset a
transfer job would pick up. That step runs only when CBGLX01C ends with RC 0 or 4,
so an out-of-balance or aborted run never reaches the outbound dataset.

## 9. Verification status

| Claim | Status |
|---|---|
| Program compiles and runs under GnuCOBOL 3.1.2 (`--std=ibm-strict -fsign=EBCDIC`), and every test case matches its expected interface bytes, report bytes, and RC | Verified locally, see `tests/cbglx01c/README.md` |
| Interface expected bytes and RCs come from an independent Python model of this spec, not from the COBOL output | Verified (by construction, `tests/cbglx01c/gen_fixtures.py`) |
| Same results on IBM Enterprise COBOL / z/OS (EBCDIC, DFSORT, VSAM REPRO) | **Not verified**: no mainframe was available. The program uses no GnuCOBOL extensions, but byte-for-byte mainframe equivalence is not claimed. |
| GLEXTRCT JCL | Reviewed for consistency with the existing jobs; **not executed** (no JES available) |

## 10. Test cases

| Case | What it proves | Expected RC |
|---|---|---:|
| `01_balanced_multi_account` | Multiple accounts, types, categories, debits, and credits | 0 |
| `02_empty_input` | Empty source still writes a header and zero-total trailer | 4 |
| `03_single_record` | One accepted transaction | 0 |
| `04_negative_amounts_only` | Credit-only input and negative net | 0 |
| `05_max_picture_amounts` | Maximum transaction amounts and zero net | 0 |
| `06_key_amount_overflow` | Key accumulator overflow leaves a header only | 12 |
| `07_out_of_balance_tcatbal` | Category-balance difference of 0.01 | 8 |
| `08_key_missing_from_tcatbal` | Activity key absent from category balances | 8 |
| `09_tcatbal_only_balance` | Nonzero category balance without activity | 8 |
| `10_feeder_totals_match` | Matching feeder count and net | 0 |
| `11_feeder_totals_mismatch` | Both feeder control totals differ | 8 |
| `12_duplicate_tran_id` | Duplicate transaction is rejected | 4 |
| `13_transact_out_of_sequence` | Input key order aborts processing | 12 |
| `14_tcatbal_duplicate_key` | Duplicate category-balance key aborts processing | 12 |
| `15_bad_proc_dates` | Invalid processing dates and valid leap days | 4 |
| `16_proc_date_after_run_date` | Processing date later than run date | 4 |
| `17_card_not_in_xref` | Missing card cross-reference | 4 |
| `18_non_numeric_amount` | Invalid transaction amount | 4 |
| `19_invalid_run_date` | Invalid control-card date; interface absent | 12 |
| `20_missing_input_file` | Missing transaction input; interface absent | 12 |
| `21_carddemo_sample_data` | 300 fixed records with derived category balances | 0 |
| `22_zero_amount` | Accepted zero-amount transaction | 0 |

The sample-data case assumes every input transaction posts successfully. Its
category balances start at zero and add the per-key activity sums; it does not
replay the posting program's transaction-limit or expiry checks.

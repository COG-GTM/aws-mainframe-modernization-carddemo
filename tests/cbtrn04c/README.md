# CBTRN04C tests — how to run and what you see

## Prerequisites

GnuCOBOL 3.1.x (`cobc`) and `python3`. Run from the repository root.

## Run

```bash
bash tests/cbtrn04c/run_tests.sh
```

The script prints these headings and performs the steps in order:
1. `== Quality gate: cobc -x -std=ibm -I app/cpy app/cbl/CBTRN04C.cbl` — compile quality gate, zero warnings.
2. `== Quality gate: cobc -fsyntax-only -std=ibm -I app/cpy app/cbl/CBTRN02C.cbl` — syntax check proving the posting program is untouched.
3. `== Test build: CBTRN04C + CEE3PRM stub, -fsign=EBCDIC for overpunched signs` — build with the CEE3PRM stub and `-fsign=EBCDIC`.
4. `== Fixture drift check: regenerate and compare with committed fixtures` — regenerate and compare fixtures.
5. `== Reference files: standard set (tests/cbtrn04c/refdata/standard)` — `LOADIDX` loads indexed reference files.
6. `== Reference files: sample set (app/data/ASCII)` — `LOADIDX` loads the sample indexed reference files.
7. `== Cases` — compare each case's rc, stdout, VALDRPT, DALYRJ04, and DALYVALD byte-for-byte with expected outputs.
8. `== Docs/source synchronisation check` — check docs/source consistency.
9. `== Summary`

Actual tail from the successful run:

```text
== Summary
cases passed: 61
cases failed: 0
ALL TESTS PASSED
```

## Where outputs land

Run outputs: `tests/cbtrn04c/build/run/<case>/` (`rc`, `stdout.txt`, `valdrpt.txt`, `dalyvald.dat`, `dalyrj04.dat`). Expected files: `tests/cbtrn04c/cases/<case>/expected/`. Catalogue: `tests/cbtrn04c/cases/INDEX.md`.

## What to show on screen

### Control totals for the 300-record sample feed

Run `cat tests/cbtrn04c/build/run/sample_data/valdrpt.txt`. Actual report below, with trailing spaces removed:

```text
 CBTRN04C  DAILY TRANSACTION PRE-POSTING VALIDATION - CONTROL TOTALS
 RUN DATE (GREGORIAN)  : 2022-07-18   RUN DATE (JULIAN)  : 2022199

 RECORDS READ                                      :         300
 RECORDS ACCEPTED                                  :         262
 RECORDS REJECTED                                  :          38

 REJECTED BY REASON CODE
   0201 TRANSACTION ID MISSING (SPACES OR LOW-VALUES)     :           0
   0202 TRANSACTION TYPE CODE NOT IN TRANTYPE FILE        :           0
   0203 TRANSACTION TYPE/CATEGORY NOT IN TRANCATG FILE    :           0
   0204 TRANSACTION AMOUNT NOT NUMERIC OR INVALID SIGN    :           0
   0100 INVALID CARD NUMBER FOUND                         :           0
   0101 ACCOUNT RECORD NOT FOUND                          :           0
   0102 OVERLIMIT TRANSACTION                             :          38
   0103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION        :           0
   0205 AMOUNT WOULD OVERFLOW CATEGORY BALANCE S9(09)V99  :           0
   0206 ORIGINATION TIMESTAMP DATE INVALID                :           0
   0207 PROCESSING TIMESTAMP DATE INVALID                 :           0
   0208 ORIGINATION DATE AFTER PROCESSING DATE            :           0
   0209 TRANSACTION DATE AFTER RUN DATE                   :           0
   0210 AMOUNT WOULD OVERFLOW ACCOUNT FIELDS S9(10)V99    :           0
   0211 DUPLICATE TRANSACTION ID IN FEED OR TRANSACT FILE :           0

 ACCEPTED AMOUNT TOTAL                             :                 77,954.70
 REJECTED AMOUNT TOTAL (NUMERIC AMOUNTS ONLY)      :                 26,846.84
 TOTAL AMOUNT READ (NUMERIC AMOUNTS ONLY)          :                104,801.54
 REJECTED WITH NON-NUMERIC AMOUNT (NOT SUMMED)     :           0
 ACCEPTED AMOUNT COMP-3 IMAGE (S9(16)V99, 10 BYTES): 0000000000007795470C

 ACCEPTED ORIGINATION DATE RANGE (JULIAN YYYYDDD)  : 2022161 - 2022161

 RECONCILIATION: RECORDS READ = ACCEPTED + REJECTED                                                                      OK
 RECONCILIATION: TOTAL AMOUNT = ACCEPTED AMOUNT + REJECTED AMOUNT                                                        OK
 RETURN CODE                                       :  4
```

- 300 read = 262 accepted + 38 rejected; all 38 are reason 0102 (overlimit).
- 77,954.70 accepted + 26,846.84 rejected = 104,801.54 total; both reconciliation lines say `OK`.
- `0000000000007795470C` is the 10-byte packed accepted total: 19 digit nibbles plus sign nibble C (positive); 7795470 with two implied decimals is 77,954.70.
- RC 4: rejects present, posting may proceed on the clean file. RC 0: none rejected. RC 8: bad PARM. RC 12: file error, overflow, or table full; overflow and table-full paths are desk-checked only.

### Reject reasons

DALYRJ04 has fixed 430-byte records: 350-byte original + 4-digit reason + 76-byte text, with no newlines. Count reasons with `fold -w 430 tests/cbtrn04c/build/run/sample_data/dalyrj04.dat | cut -c351-430 | sort | uniq -c`; actual output:

```text
     38 0102OVERLIMIT TRANSACTION
```

`all_rejects` has three distinct codes. Command: `fold -w 430 tests/cbtrn04c/build/run/all_rejects/dalyrj04.dat | cut -c351-430 | sort | uniq -c`. Actual output:

```text
      1 0100INVALID CARD NUMBER FOUND
      1 0201TRANSACTION ID MISSING (SPACES OR LOW-VALUES)
      1 0202TRANSACTION TYPE CODE NOT IN TRANTYPE FILE
```

Its `REJECTED BY REASON CODE` report block:

```text
 REJECTED BY REASON CODE
   0201 TRANSACTION ID MISSING (SPACES OR LOW-VALUES)     :           1
   0202 TRANSACTION TYPE CODE NOT IN TRANTYPE FILE        :           1
   0203 TRANSACTION TYPE/CATEGORY NOT IN TRANCATG FILE    :           0
   0204 TRANSACTION AMOUNT NOT NUMERIC OR INVALID SIGN    :           0
   0100 INVALID CARD NUMBER FOUND                         :           1
   0101 ACCOUNT RECORD NOT FOUND                          :           0
   0102 OVERLIMIT TRANSACTION                             :           0
   0103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION        :           0
   0205 AMOUNT WOULD OVERFLOW CATEGORY BALANCE S9(09)V99  :           0
   0206 ORIGINATION TIMESTAMP DATE INVALID                :           0
   0207 PROCESSING TIMESTAMP DATE INVALID                :           0
   0208 ORIGINATION DATE AFTER PROCESSING DATE            :           0
   0209 TRANSACTION DATE AFTER RUN DATE                   :           0
   0210 AMOUNT WOULD OVERFLOW ACCOUNT FIELDS S9(10)V99    :           0
   0211 DUPLICATE TRANSACTION ID IN FEED OR TRANSACT FILE :           0
```

Precedence: `precedence_amount_and_date` fails the non-numeric amount and invalid origination-date rules, but is rejected once as 0204 (0206 count is zero).

## Reason codes

Evaluation order; report text verbatim. Codes 0100–0103 reuse CBTRN02C's existing codes and text.

| Order | Code | Report text |
|---|---|---|
| 1 | 0201 | TRANSACTION ID MISSING (SPACES OR LOW-VALUES) |
| 2 | 0202 | TRANSACTION TYPE CODE NOT IN TRANTYPE FILE |
| 3 | 0203 | TRANSACTION TYPE/CATEGORY NOT IN TRANCATG FILE |
| 4 | 0204 | TRANSACTION AMOUNT NOT NUMERIC OR INVALID SIGN |
| 5 | 0100 | INVALID CARD NUMBER FOUND |
| 6 | 0101 | ACCOUNT RECORD NOT FOUND |
| 7 | 0205 | AMOUNT WOULD OVERFLOW CATEGORY BALANCE S9(09)V99 |
| 8 | 0206 | ORIGINATION TIMESTAMP DATE INVALID |
| 9 | 0207 | PROCESSING TIMESTAMP DATE INVALID |
| 10 | 0208 | ORIGINATION DATE AFTER PROCESSING DATE |
| 11 | 0209 | TRANSACTION DATE AFTER RUN DATE |
| 12 | 0103 | TRANSACTION RECEIVED AFTER ACCT EXPIRATION |
| 13 | 0102 | OVERLIMIT TRANSACTION |
| 14 | 0210 | AMOUNT WOULD OVERFLOW ACCOUNT FIELDS S9(10)V99 |
| 15 | 0211 | DUPLICATE TRANSACTION ID IN FEED OR TRANSACT FILE |

## Limits

GnuCOBOL with ASCII fixtures stands in for Enterprise COBOL with EBCDIC; indexed files stand in for VSAM KSDS. JCL (`VALDTRAN`, `POSTTRN2`, `DALYRJ04`) is not executed here. Accumulator-overflow and table-full RC 12 paths are desk-checked only.
See [test evidence](../../docs/sustainment/cbtrn04c/test-evidence.md) and [change request](../../docs/sustainment/cbtrn04c/change-request.md).

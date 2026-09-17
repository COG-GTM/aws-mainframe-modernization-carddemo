# Test evidence (CBTRN04C)

## How to run

Requirements: Linux, GnuCOBOL 3.x (`cobc`), `python3` (fixture generation and
byte-level file helpers only). Nothing is contacted outside the repository.

```bash
cobc -x -std=ibm -I app/cpy app/cbl/CBTRN04C.cbl          # quality gate: no errors, no warnings
cobc -fsyntax-only -std=ibm -I app/cpy app/cbl/CBTRN02C.cbl  # proves the posting program is untouched
bash tests/cbtrn04c/run_tests.sh                          # exit 0 == every case passed
```

`run_tests.sh` does, in order:

1. the two `cobc` commands above (the first must produce no diagnostics at all);
2. a test build of `CBTRN04C` with `tests/cbtrn04c/stubs/CEE3PRM.cbl` and
   `-fsign=EBCDIC`, plus `tests/cbtrn04c/tools/LOADIDX.cbl` (loads the text
   reference files, including the 300-byte account master, into GnuCOBOL
   indexed files);
3. a fixture drift check: `tools/gen_fixtures.py` is re-run and its output must
   equal the committed `cases/*/dalytran.dat`, `cases/*/parm`, `cases/INDEX.md`
   and `refdata/standard/*`;
4. loading of the standard reference set and of the repository sample reference
   set (`app/data/ASCII/{trantype,trancatg,cardxref,acctdata,tcatbal}.txt`);
5. one run per case directory. Return code, `stdout`, `DALYVALD`, `DALYRJ04`
   and the 133-byte `VALDRPT` report are compared byte-for-byte with the files
   under `cases/<case>/expected/`. `DALYRJ04` must be a whole number of 430-byte
   records and `DALYVALD` of 350-byte records;
6. `tools/check_docs_sync.py`, which fails the run if `change-request.md`,
   `government-decisions.md` or this file disagree with the source, the fixtures
   or the sample report (reason codes, case names, citations, all numbers).

`UPDATE_EXPECTED=1 bash tests/cbtrn04c/run_tests.sh` rewrites the expected files
from the current program; review the diff before committing.

## Run output

<!-- run-output:begin -->
```text

== Quality gate: cobc -x -std=ibm -I app/cpy app/cbl/CBTRN04C.cbl
OK: compiled with no errors and no warnings

== Quality gate: cobc -fsyntax-only -std=ibm -I app/cpy app/cbl/CBTRN02C.cbl
OK: CBTRN02C still passes the syntax check

== Test build: CBTRN04C + CEE3PRM stub, -fsign=EBCDIC for overpunched signs
OK

== Fixture drift check: regenerate and compare with committed fixtures
OK: committed fixtures match the generator

== Reference files: standard set (tests/cbtrn04c/refdata/standard)
LOADIDX: TRANTYPE RECORDS LOADED 0000003
LOADIDX: TRANCATG RECORDS LOADED 0000004
LOADIDX: XREFFILE RECORDS LOADED 0000008
LOADIDX: ACCTFILE RECORDS LOADED 0000007
LOADIDX: TCATBALF RECORDS LOADED 0000004

== Reference files: sample set (app/data/ASCII)
LOADIDX: TRANTYPE RECORDS LOADED 0000007
LOADIDX: TRANCATG RECORDS LOADED 0000018
LOADIDX: XREFFILE RECORDS LOADED 0000050
LOADIDX: ACCTFILE RECORDS LOADED 0000050
LOADIDX: TCATBALF RECORDS LOADED 0000050

== Cases
PASS  all_rejects (rc=4)
PASS  clean_record (rc=0)
PASS  empty_input (rc=0)
PASS  file_error_missing_input (rc=12)
PASS  mixed_feed (rc=4)
PASS  parm_invalid_date (rc=8)
PASS  parm_missing (rc=8)
PASS  parm_wrong_length (rc=8)
PASS  precedence_amount_and_date (rc=4)
PASS  precedence_expired_and_overlimit (rc=4)
PASS  precedence_id_and_amount (rc=4)
PASS  precedence_type_and_card (rc=4)
PASS  rule01_id_low_values (rc=4)
PASS  rule01_id_spaces (rc=4)
PASS  rule02_type_unknown (rc=4)
PASS  rule03_category_unknown (rc=4)
PASS  rule04_amount_alpha (rc=4)
PASS  rule04_amount_invalid_sign (rc=4)
PASS  rule04_amount_spaces (rc=4)
PASS  rule05_card_unknown (rc=4)
PASS  rule06_acct_missing (rc=4)
PASS  rule07_amount_max_downstream (rc=0)
PASS  rule07_amount_negative_floor (rc=4)
PASS  rule07_amount_negative_max (rc=0)
PASS  rule07_amount_one_cent_over (rc=4)
PASS  rule07_batch_reject_not_projected (rc=4)
PASS  rule07_batch_second_record_overflows (rc=4)
PASS  rule08_orig_century_leap (rc=0)
PASS  rule08_orig_century_nonleap (rc=4)
PASS  rule08_orig_feb30 (rc=4)
PASS  rule08_orig_leap_day_nonleap (rc=4)
PASS  rule08_orig_leap_day_valid (rc=0)
PASS  rule08_orig_month_13 (rc=4)
PASS  rule08_orig_not_numeric (rc=4)
PASS  rule09_proc_apr31 (rc=4)
PASS  rule09_proc_blank_accepted (rc=0)
PASS  rule10_orig_after_proc (rc=4)
PASS  rule11_dates_equal_run_date (rc=0)
PASS  rule11_orig_future (rc=4)
PASS  rule11_proc_future (rc=4)
PASS  rule12_acct_expired (rc=4)
PASS  rule12_acct_expiry_boundary (rc=0)
PASS  rule13_acct_reject_not_projected (rc=4)
PASS  rule13_credit_limit_at (rc=0)
PASS  rule13_credit_limit_over (rc=4)
PASS  rule13_credit_projection (rc=4)
PASS  sample_data (rc=4)

== Docs/source synchronisation check
check_docs_sync: 14 rules, 66 citations, 47 cases, 6 confirmed / 8 inferred, 12 decisions, sample totals agree
OK

== Summary
cases passed: 47
cases failed: 0
ALL TESTS PASSED
```
<!-- run-output:end -->

## Control-total report from the repository sample data

`app/data/ASCII/dailytran.txt` (300 records, converted to a 350-byte fixed file
by `tools/seqfile.py`) validated against the sample `trantype`, `trancatg`,
`cardxref`, `acctdata` and `tcatbal` files with `PARM='20220718'` (the run date
used by `INTCALC.jcl`). 262 records are accepted and 38 are rejected with 0102
(over limit): every one of the 38 fails the `CBTRN02C` credit-limit test
(`ACCT-CREDIT-LIMIT >= CYC-CREDIT - CYC-DEBIT + AMT`) against the sample account
master, so `CBTRN02C` would reject the same records at posting time; the
validator stops them one step earlier. The split and both amount totals were
recomputed independently from the three text files with the same arithmetic and
agree (first three rejects: 715.44 against a 446.00 limit, 633.00 against 568.00,
948.44 against 868.00). The 10-byte packed accumulator reads `+7795470` cents
with sign nibble `C`. The accepted file holds the 262 records byte-for-byte as
read (`cases/sample_data/expected/dalyvald.dat`). This is
`tests/cbtrn04c/cases/sample_data/expected/valdrpt.txt` with trailing blanks
removed:

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

The sample feed carries a blank `DALYTRAN-PROC-TS` on every record and a single
origination day (2022-06-10 = Julian 2022161), which is why the range collapses to
one value and why no 0207/0208 rejects appear (see `government-decisions.md` #4);
the 38 over-limit rejects are `government-decisions.md` #12.

A report with rejects, for contrast (`cases/mixed_feed`: six records, four
accepted, one unknown card, one origination after processing):

```text
 CBTRN04C  DAILY TRANSACTION PRE-POSTING VALIDATION - CONTROL TOTALS
 RUN DATE (GREGORIAN)  : 2024-03-15   RUN DATE (JULIAN)  : 2024075

 RECORDS READ                                      :           6
 RECORDS ACCEPTED                                  :           4
 RECORDS REJECTED                                  :           2

 REJECTED BY REASON CODE
   0201 TRANSACTION ID MISSING (SPACES OR LOW-VALUES)     :           0
   0202 TRANSACTION TYPE CODE NOT IN TRANTYPE FILE        :           0
   0203 TRANSACTION TYPE/CATEGORY NOT IN TRANCATG FILE    :           0
   0204 TRANSACTION AMOUNT NOT NUMERIC OR INVALID SIGN    :           0
   0100 INVALID CARD NUMBER FOUND                         :           1
   0101 ACCOUNT RECORD NOT FOUND                          :           0
   0102 OVERLIMIT TRANSACTION                             :           0
   0103 TRANSACTION RECEIVED AFTER ACCT EXPIRATION        :           0
   0205 AMOUNT WOULD OVERFLOW CATEGORY BALANCE S9(09)V99  :           0
   0206 ORIGINATION TIMESTAMP DATE INVALID                :           0
   0207 PROCESSING TIMESTAMP DATE INVALID                 :           0
   0208 ORIGINATION DATE AFTER PROCESSING DATE            :           1
   0209 TRANSACTION DATE AFTER RUN DATE                   :           0

 ACCEPTED AMOUNT TOTAL                             :                  1,105.49
 REJECTED AMOUNT TOTAL (NUMERIC AMOUNTS ONLY)      :                     40.00
 TOTAL AMOUNT READ (NUMERIC AMOUNTS ONLY)          :                  1,145.49
 REJECTED WITH NON-NUMERIC AMOUNT (NOT SUMMED)     :           0
 ACCEPTED AMOUNT COMP-3 IMAGE (S9(16)V99, 10 BYTES): 0000000000000110549C

 ACCEPTED ORIGINATION DATE RANGE (JULIAN YYYYDDD)  : 2024005 - 2024070

 RECONCILIATION: RECORDS READ = ACCEPTED + REJECTED                                                                      OK
 RECONCILIATION: TOTAL AMOUNT = ACCEPTED AMOUNT + REJECTED AMOUNT                                                        OK
 RETURN CODE                                       :  4
```

## What the Linux test proves, and what it does not

| Topic | Under GnuCOBOL (this evidence) | On IBM Enterprise COBOL / z/OS (not verified here) |
|-------|-------------------------------|-----------------------------------------------------|
| Compiler | GnuCOBOL 3.1.2, `-std=ibm`, zero diagnostics | Enterprise COBOL not available; the program uses only statements already found in `CBTRN02C` plus `READ ... INVALID KEY` on the reference files, `COMP-3` arithmetic and `FUNCTION ORD` (intrinsic functions are standard in Enterprise COBOL) |
| `PARM` | `CEE3PRM` is not a GnuCOBOL service. The test build links `tests/cbtrn04c/stubs/CEE3PRM.cbl`, which returns the `CBTRN04C_PARM` environment variable in the 80-byte parameter string with a zero feedback code. The quality-gate compile (no stub) succeeds because the call is dynamic with `ON EXCEPTION` | Language Environment supplies `CEE3PRM`; `PARM='YYYYMMDD'` on the `EXEC` statement is returned in the same 80-byte form. Behaviour of `ON EXCEPTION` if LE were unavailable is not exercised |
| `CEEDAYS` | Not called. Gregorian validation and Julian conversion are plain COBOL (`5000-VALIDATE-GREGORIAN-DATE`), so no date-service stub is needed | Same code path; `CEEDAYS` / `CSUTLDTC` are not invoked |
| Sign encoding of `DALYTRAN-AMT` | The ASCII sample data uses EBCDIC overpunch letters (`{`, `A`–`I`, `}`, `J`–`R`). The test build adds `-fsign=EBCDIC` so the `NUMERIC` test and the `MOVE` to `COMP-3` read them as the mainframe would; `rule04_amount_invalid_sign` uses a byte outside that set | Native zoned decimal; `-fsign` does not apply. The overpunch bytes are the sign zones the mainframe uses, but EBCDIC fixtures were not produced |
| Character set | ASCII fixtures; comparisons of `SPACES` / `LOW-VALUES` and `YYYY-MM-DD` digits are code-page independent | EBCDIC; the only alphanumeric `>=` (R12, `ACCT-EXPIRAION-DATE` against the origination date) runs after R08 has proven both operands are `YYYY-MM-DD` digit strings, so the collating sequence cannot change its result |
| Files | GnuCOBOL indexed files built by `LOADIDX` from the text reference files; sequential files are fixed-length byte streams (`RECFM=F`) | VSAM KSDS for the five reference files, `RECFM=FB` for the feed, rejects and report. `INVALID KEY` / status 23 semantics are the same |
| File-error path | Ends with RC 12 (`file_error_missing_input`, OPEN status 35). The `ON SIZE ERROR` overflow path (RC 12) and the projection-table-full path (RC 12) are not driven by a fixture: the smallest feed that overflows an `S9(16)V99` total is about 10^7 records at the feed maximum, and the tables hold 20,000 keys; both paths are desk-checked only | `CBTRN02C` calls `CEE3ABD`; `CBTRN04C` deliberately returns RC 12 instead (see `government-decisions.md` #7) |
| JCL | Not executable on Linux; `VALDTRAN.jcl`, `POSTTRN2.jcl`, `DALYRJ04.jcl` were desk-checked against `POSTTRAN.jcl`, `INTCALC.jcl`, `DALYREJS.jcl` | Data set names use a `SITE.HLQ` placeholder to be replaced at implementation |

Pattern credit: the idea of compiling a tiny COBOL stub for a Language Environment
service alongside the program under test (`tests/stubs/CEEDAYS.cbl` on the
repository's open test-foundation branch) is reused here for `CEE3PRM`; nothing
from that branch is a dependency of this one.

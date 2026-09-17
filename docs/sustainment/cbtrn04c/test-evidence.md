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
   reference files into GnuCOBOL indexed files);
3. a fixture drift check: `tools/gen_fixtures.py` is re-run and its output must
   equal the committed `cases/*/dalytran.dat`, `cases/*/parm`, `cases/INDEX.md`
   and `refdata/standard/*`;
4. loading of the standard reference set and of the repository sample reference
   set (`app/data/ASCII/{trantype,trancatg,cardxref,tcatbal}.txt`);
5. one run per case directory. Return code, `stdout`, `DALYVALD`, `DALYRJ04`
   and the 133-byte `VALDRPT` report are compared byte-for-byte with the files
   under `cases/<case>/expected/`. `DALYRJ04` must be a whole number of 430-byte
   records and `DALYVALD` of 350-byte records; where `expected/dalyvald.same-as-input`
   exists the accepted file must be byte-identical to the input;
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
LOADIDX: XREFFILE RECORDS LOADED 0000003
LOADIDX: TCATBALF RECORDS LOADED 0000003

== Reference files: sample set (app/data/ASCII)
LOADIDX: TRANTYPE RECORDS LOADED 0000007
LOADIDX: TRANCATG RECORDS LOADED 0000018
LOADIDX: XREFFILE RECORDS LOADED 0000050
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
PASS  rule06_amount_max_downstream (rc=0)
PASS  rule06_amount_negative_floor (rc=4)
PASS  rule06_amount_negative_max (rc=0)
PASS  rule06_amount_one_cent_over (rc=4)
PASS  rule06_batch_reject_not_projected (rc=4)
PASS  rule06_batch_second_record_overflows (rc=4)
PASS  rule07_orig_century_leap (rc=0)
PASS  rule07_orig_century_nonleap (rc=4)
PASS  rule07_orig_feb30 (rc=4)
PASS  rule07_orig_leap_day_nonleap (rc=4)
PASS  rule07_orig_leap_day_valid (rc=0)
PASS  rule07_orig_month_13 (rc=4)
PASS  rule07_orig_not_numeric (rc=4)
PASS  rule08_proc_apr31 (rc=4)
PASS  rule08_proc_blank_accepted (rc=0)
PASS  rule09_orig_after_proc (rc=4)
PASS  rule10_dates_equal_run_date (rc=0)
PASS  rule10_orig_future (rc=4)
PASS  rule10_proc_future (rc=4)
PASS  sample_data (rc=0)

== Docs/source synchronisation check
check_docs_sync: 11 rules, 43 citations, 39 cases, 3 confirmed / 8 inferred, 9 decisions, sample totals agree
OK

== Summary
cases passed: 39
cases failed: 0
ALL TESTS PASSED
```
<!-- run-output:end -->

## Control-total report from the repository sample data

`app/data/ASCII/dailytran.txt` (300 records, converted to a 350-byte fixed file
by `tools/seqfile.py`) validated against the sample `trantype`, `trancatg`,
`cardxref` and `tcatbal` files with `PARM='20220718'` (the run date used by
`INTCALC.jcl`). All 300 records are accepted; the accepted file is byte-identical
to the input; the 8-byte packed accumulator reads `+10480154` cents with sign
nibble `C`. This is `tests/cbtrn04c/cases/sample_data/expected/valdrpt.txt` with
trailing blanks removed:

```text
 CBTRN04C  DAILY TRANSACTION PRE-POSTING VALIDATION - CONTROL TOTALS
 RUN DATE (GREGORIAN)  : 2022-07-18   RUN DATE (JULIAN)  : 2022199

 RECORDS READ                                      :         300
 RECORDS ACCEPTED                                  :         300
 RECORDS REJECTED                                  :           0

 REJECTED BY REASON CODE
   0201 TRANSACTION ID MISSING (SPACES OR LOW-VALUES)     :           0
   0202 TRANSACTION TYPE CODE NOT IN TRANTYPE FILE        :           0
   0203 TRANSACTION TYPE/CATEGORY NOT IN TRANCATG FILE    :           0
   0204 TRANSACTION AMOUNT NOT NUMERIC OR INVALID SIGN    :           0
   0100 INVALID CARD NUMBER FOUND                         :           0
   0205 AMOUNT WOULD OVERFLOW CATEGORY BALANCE S9(09)V99  :           0
   0206 ORIGINATION TIMESTAMP DATE INVALID                :           0
   0207 PROCESSING TIMESTAMP DATE INVALID                 :           0
   0208 ORIGINATION DATE AFTER PROCESSING DATE            :           0
   0209 TRANSACTION DATE AFTER RUN DATE                   :           0

 ACCEPTED AMOUNT TOTAL                             :            104,801.54
 REJECTED AMOUNT TOTAL (NUMERIC AMOUNTS ONLY)      :                  0.00
 TOTAL AMOUNT READ (NUMERIC AMOUNTS ONLY)          :            104,801.54
 REJECTED WITH NON-NUMERIC AMOUNT (NOT SUMMED)     :           0
 ACCEPTED AMOUNT COMP-3 IMAGE (S9(13)V99, 8 BYTES) : 000000010480154C

 ACCEPTED ORIGINATION DATE RANGE (JULIAN YYYYDDD)  : 2022161 - 2022161

 RECONCILIATION: RECORDS READ = ACCEPTED + REJECTED                                                                      OK
 RECONCILIATION: TOTAL AMOUNT = ACCEPTED AMOUNT + REJECTED AMOUNT                                                        OK
 RETURN CODE                                       :  0
```

The sample feed carries a blank `DALYTRAN-PROC-TS` on every record and a single
origination day (2022-06-10 = Julian 2022161), which is why the range collapses to
one value and why no 0207/0208 rejects appear (see `government-decisions.md` #4).

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
   0205 AMOUNT WOULD OVERFLOW CATEGORY BALANCE S9(09)V99  :           0
   0206 ORIGINATION TIMESTAMP DATE INVALID                :           0
   0207 PROCESSING TIMESTAMP DATE INVALID                 :           0
   0208 ORIGINATION DATE AFTER PROCESSING DATE            :           1
   0209 TRANSACTION DATE AFTER RUN DATE                   :           0

 ACCEPTED AMOUNT TOTAL                             :              1,105.49
 REJECTED AMOUNT TOTAL (NUMERIC AMOUNTS ONLY)      :                 40.00
 TOTAL AMOUNT READ (NUMERIC AMOUNTS ONLY)          :              1,145.49
 REJECTED WITH NON-NUMERIC AMOUNT (NOT SUMMED)     :           0
 ACCEPTED AMOUNT COMP-3 IMAGE (S9(13)V99, 8 BYTES) : 000000000110549C

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
| Character set | ASCII fixtures; comparisons of `SPACES` / `LOW-VALUES` and `YYYY-MM-DD` digits are code-page independent | EBCDIC; collating-sequence differences do not affect any rule (no `>`/`<` on alphanumeric fields) |
| Files | GnuCOBOL indexed files built by `LOADIDX` from the text reference files; sequential files are fixed-length byte streams (`RECFM=F`) | VSAM KSDS for the four reference files, `RECFM=FB` for the feed, rejects and report. `INVALID KEY` / status 23 semantics are the same |
| File-error path | Ends with RC 12 (`file_error_missing_input`, OPEN status 35) | `CBTRN02C` calls `CEE3ABD`; `CBTRN04C` deliberately returns RC 12 instead (see `government-decisions.md` #7) |
| JCL | Not executable on Linux; `VALDTRAN.jcl`, `POSTTRN2.jcl`, `DALYRJ04.jcl` were desk-checked against `POSTTRAN.jcl`, `INTCALC.jcl`, `DALYREJS.jcl` | Data set names use a `SITE.HLQ` placeholder to be replaced at implementation |

Pattern credit: the idea of compiling a tiny COBOL stub for a Language Environment
service alongside the program under test (`tests/stubs/CEEDAYS.cbl` on the
repository's open test-foundation branch) is reused here for `CEE3PRM`; nothing
from that branch is a dependency of this one.

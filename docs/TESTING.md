# CardDemo Automated Testing and Coverage

This document describes the automated unit test suite for the CardDemo
COBOL programs, how to run it, how test coverage is measured and how to
interpret the coverage report. It also lists which programs are covered
today and which are still pending.

## 1. Framework decision

Two complementary mechanisms are used:

| Environment | Test execution | Coverage measurement |
|---|---|---|
| z/OS (target) | **zUnit** (IBM z/OS Automated Unit Testing Framework, part of IBM Developer for z/OS) and/or the plain batch drivers in `tests/jcl/` | IBM Debug for z/OS **Code Coverage** (headless collector or IDz UI) |
| Off-mainframe (local/CI) | GnuCOBOL 3.2+ via `tests/run_tests.sh` | `cobc --coverage` + `gcov` line coverage |

Rationale:

* **zUnit** is IBM's unit testing framework for Enterprise COBOL and is
  the recommended long-term target for this application. zUnit test
  cases are themselves COBOL programs driven by a runner
  (`AZU TSTRUN`), and integrate with IDz's code coverage reports.
  Running zUnit requires a z/OS system with IBM Developer for z/OS
  server components, which is not available in every environment
  (including this repository's CI).
* To make the suite executable anywhere (laptop, container, CI), the
  same test-driver programs are written as **plain COBOL batch
  programs** with a tiny assertion copybook (`tests/cpy/TSTASWY.cpy` /
  `TSTASPY.cpy`). They compile and run both under GnuCOBOL
  (off-mainframe) and as normal batch jobs on z/OS (`tests/jcl/`).
  This mirrors the zUnit pattern (Arrange/Act/Assert per test case,
  non-zero return code on failure) without requiring zUnit to be
  installed, and the drivers can later be converted into zUnit test
  cases mechanically.

## 2. Directory layout

```
tests/
  cobol/       Test drivers (one per program/copybook under test)
    TSTDTC1C.cbl   Tests for CSUTLDTC (date validation subroutine)
    TSTDTE1C.cbl   Tests for the CSUTLDPY/CSUTLDWY date edit copybooks
    TSTSTM1C.cbl   Tests for CBSTM03B (statement file access subroutine)
  cpy/         Test harness copybooks
    TSTASWY.cpy    Working-storage: test counters and current test name
    TSTASPY.cpy    Paragraphs: 8000-TEST-PASSED / 8100-TEST-FAILED /
                   8900-TEST-SUMMARY (puts #failures in RETURN-CODE)
  fixtures/    Programs that create test data
    STMFIXTC.cbl   Creates the four indexed (VSAM-like) files that
                   CBSTM03B operates on, with known records
  stubs/       Off-mainframe stand-ins for z/OS services
    CEEDAYS.cbl    Local replacement for the LE CEEDAYS date service
                   (NOT to be compiled on z/OS)
  jcl/         Batch runners for z/OS, styled after samples/jcl/
    TESTCMPL.jcl   Compiles fixture + drivers with the BUILDBAT proc
    TESTRUN.jcl    Defines VSAM test files, loads fixtures, runs the
                   three drivers as batch steps
  run_tests.sh Off-mainframe runner: compiles with coverage, runs all
               suites, prints per-program and total coverage %
```

## 3. Running the tests

### 3.1 Off-mainframe (local / CI)

Requirements: GnuCOBOL >= 3.2 (`cobc`, `cobcrun`) and `gcov`.

```
tests/run_tests.sh
```

The script:

1. compiles the programs under test (`CSUTLDTC.cbl`, `CBSTM03B.CBL`)
   as dynamically loadable modules with `--coverage` instrumentation;
2. compiles the stub, fixture and test drivers;
3. creates the indexed test files (the local equivalent of the VSAM
   KSDS files, addressed through `DD_*` environment variables exactly
   like DD names in JCL);
4. runs the three test suites and reports PASS/FAIL per assertion;
5. runs `gcov` over the instrumented modules and prints line coverage
   per program and in total.

Exit code 0 means all assertions passed; non-zero otherwise. All build
artifacts stay under `tests/.build/` (git-ignored).

Sample output:

```
TESTS RUN....: 0015
TESTS PASSED.: 0015
TESTS FAILED.: 0000
=== Coverage report (gcov line coverage) ===
  CSUTLDTC.cbl     89.8%  (53 of 59 executable lines)
  CBSTM03B.CBL     98.7%  (74 of 75 executable lines)
  CSUTLDPY.cpy     75.8%  (113 of 149 executable lines)
  --------------------------------------------------
  TOTAL            84.8%  (240 of 283 executable lines)
```

Note: the programs are compiled with `-fassign-clause=external` so that
`ASSIGN TO XREFFILE` is treated as an external DD name (IBM semantics),
resolved locally through the `DD_XREFFILE` environment variable.

### 3.2 On z/OS

1. Upload `tests/cobol/*.cbl`, `tests/fixtures/*.cbl` to the
   `<HLQ>.CARDDEMO.CBL` PDS and `tests/cpy/*.cpy` to
   `<HLQ>.CARDDEMO.CPY`. Do **not** upload `tests/stubs/CEEDAYS.cbl`
   (on z/OS the real LE CEEDAYS service is used).
2. Submit `tests/jcl/TESTCMPL.jcl` to compile the fixture and drivers
   with the same `BUILDBAT` procedure used for application programs.
3. Submit `tests/jcl/TESTRUN.jcl`. It defines four small VSAM KSDS
   test files, loads the fixture records, then executes the three
   drivers as batch steps. Each driver DISPLAYs one PASS/FAIL line per
   assertion plus a summary, and sets its RETURN-CODE to the number of
   failed assertions, so any step RC > 0 indicates failures.

To run under **zUnit** instead, wrap each driver's test paragraphs as
zUnit test cases in an `AZU` test runner configuration; the assertion
copybooks were deliberately kept free of any GnuCOBOL-specific syntax
so the sources compile unchanged with Enterprise COBOL.

## 4. Coverage measurement

### 4.1 Off-mainframe: GnuCOBOL + gcov

`cobc --coverage` instruments the generated code and records, per
COBOL source line, whether it was executed. After the tests run,
`tests/run_tests.sh` invokes `gcov`, which writes one `.gcov` file per
source (kept in `tests/.build/coverage/`) and the script aggregates
them into the percentages shown above.

Interpreting a `.gcov` file: each line is `count:lineno:source`.

* `-` in the count column: not executable (comments, data division…)
* `#####`: executable but **never executed** (a coverage gap)
* a number: how many times the line was executed

The reported percentage is `executed / (executed + never-executed)`
lines, i.e. plain line coverage of the executable lines. Paragraph
coverage can be derived by checking whether the first statement of
each paragraph was hit.

### 4.2 On z/OS: IBM Debug / IDz Code Coverage

On z/OS, run the same batch steps under the IBM Debug for z/OS
headless code coverage collector (add
`//CEEOPTS DD *` with `TEST(,,,TCPIP&<host>%<port>:*)` or use the
`EQANMDBG` front-end), or launch them from IDz with *Run As > Code
Coverage*. IDz produces line/branch coverage reports per compile unit
that are equivalent to (and typically slightly stricter than) the
local gcov numbers. zUnit runs started from IDz collect the same
reports automatically.

## 5. What is covered today

30 assertions across three suites, all passing. Current line coverage
of the code under test (from `tests/run_tests.sh`): **84.8%** total.

| Program / copybook | Suite | Coverage | Notes |
|---|---|---|---|
| `app/cbl/CSUTLDTC.cbl` | `TSTDTC1C` | 89.8% | Date validation subroutine; valid/invalid dates, formats, return codes |
| `app/cpy/CSUTLDPY.cpy` (+`CSUTLDWY.cpy`) | `TSTDTE1C` | 75.8% | Date edit logic used by `COACTUPC`: century/month/day rules, leap years, date of birth |
| `app/cbl/CBSTM03B.CBL` | `TSTSTM1C` | 98.7% | File access subroutine: open/read/keyed-read/close on all four files, EOF and not-found statuses |

## 6. What is still pending

| Programs | Why pending | Suggested approach |
|---|---|---|
| `COSGN00C`, `COMEN01C`, `COADM01C`, `COACTVWC`, `COACTUPC`, `COCRDLIC`, `COCRDSLC`, `COCRDUPC`, `COTRN00C`, `COTRN01C`, `COTRN02C`, `CORPT00C`, `COBIL00C`, `COUSR00C`, `COUSR01C`, `COUSR02C`, `COUSR03C` | Online CICS programs (`EXEC CICS` commands, BMS maps, COMMAREA flows) cannot run outside CICS | zUnit with CICS stubbing (IDz generated stubs), or extract business logic into callable subroutines/copybooks and test those (as done for `CSUTLDPY`) |
| `CBACT01C`, `CBACT02C`, `CBACT03C`, `CBACT04C`, `CBCUS01C`, `CBTRN01C`, `CBTRN02C`, `CBTRN03C`, `CBEXPORT`, `CBIMPORT`, `CBSTM03A` | Batch main programs driven by multiple VSAM/sequential files; testable with the same fixture pattern as `CBSTM03B` but need larger fixtures and output assertions | Extend `tests/fixtures/` with per-program input files, run the program, assert on output files and RETURN-CODE |
| `COBSWAIT` | Thin wrapper around a system WAIT service | Low value; stub the wait service if ever needed |
| `CSUTLDTC`, `CSUTLDPY`, `CBSTM03B` remaining lines | Error paragraphs not yet exercised (e.g. unreachable defensive code) | Add negative-path assertions as needed |

## 7. Adding a new test

1. Create `tests/cobol/TSTxxxxC.cbl`: copy an existing driver, include
   `COPY TSTASWY.` in working storage and `COPY TSTASPY.` at the end of
   the procedure division; one `MOVE ... TO WS-TEST-NAME` + assertion
   per test; `PERFORM 8900-TEST-SUMMARY` before `GOBACK`.
2. If the program reads/writes files, add a fixture under
   `tests/fixtures/` and DD statements to `tests/jcl/TESTRUN.jcl`.
3. Add the program under test and the driver to the compile lists in
   `tests/run_tests.sh` (instrument the program under test with
   `$COVERAGE_FLAG`) and add its source to the coverage `for SRC in`
   list.
4. Add a compile step to `tests/jcl/TESTCMPL.jcl` and a run step to
   `tests/jcl/TESTRUN.jcl`.

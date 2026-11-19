# CardDemo test suite

```bash
sudo apt-get install -y gnucobol python3   # GnuCOBOL 3.1+ (CI uses Ubuntu 22.04's package)
tests/run_all.sh                           # all suites, PASS/FAIL summary, exit 0 = pass
```

Per suite: `python3 tests/static/test_references.py -v`,
`python3 tests/static/test_batch_compile.py -v`,
`python3 tests/validation/test_copybooks.py -v`, `tests/unit/run_csutldtc.sh`,
`tests/batch/run_batch_tests.sh` (or `cd tests/batch && python3 -m unittest -v test_cbtrn02c`).
Set `CARDDEMO_KEEP_WORK=1` to keep each batch test's datasets under `/tmp/carddemo-*`.
CI: `.github/workflows/tests.yml` runs `tests/run_all.sh` on every PR.

## Strategy

| Layer | What runs | Where |
|---|---|---|
| Static lint | JCL `EXEC PGM=`/`PROC=` resolve to `app/cbl`, `app/asm`, `app/proc` or a z/OS utility; every `COPY` resolves to `app/cpy`/`app/cpy-bms`; every static `CALL` resolves; no new TABs | `static/test_references.py` |
| Compile check | Every batch program passes `cobc -fsyntax-only` with the harness flags | `static/test_batch_compile.py` |
| Record layouts | Copybook parser: PIC validity, record lengths (300/150/50/500/350...), REDEFINES/OCCURS consistency, `app/data/ASCII` line lengths, batch FD record lengths per DD | `validation/test_copybooks.py` |
| Unit | `CSUTLDTC` called from a COBOL driver: leap years, month/day errors, range boundaries, non-numeric, blank/short dates, bad masks | `unit/TESTDTC.cbl` |
| Batch | Unmodified programs compiled with GnuCOBOL and run against small fixtures; assertions on RC, output datasets, updated KSDS contents, SYSOUT | `batch/test_*.py` |
| CICS | Structured manual/emulator specs | `cics/*.md` |

### Batch coverage

| Program | JCL | Fixture scenario | Key assertions |
|---|---|---|---|
| CBTRN02C | POSTTRAN | 7 daily trans: 3 valid, reasons 100/101/102/103 | RC 4; account balance 1075.50, cycle credit/debit; TCATBAL updated + created; 3 posted; 430-byte rejects with reason trailer; all-valid run RC 0 |
| CBACT04C | INTCALC | 5 category balances, GOLD / DEFAULT fallback | Interest = bal x rate / 1200 truncated: 15.00, 5.00, 3.33, 18.51 (total 41.84); tran ids `2022071800000001..4`; balances + cycle reset |
| CBSTM03A/B | CREASTMT | 3 accounts, 0-2 trans each | 3 statements, 19 + n lines each, Total EXP 50.00/200.00/0.00; 3 HTML statements |
| CBTRN01C | (none) | same daily file | valid cards/accounts verified; unknown card skipped; missing account reported |
| CBTRN03C | TRANREPT | 3 trans, 2 cards; bad card; bad type | header/date range, detail lines, account total 50.00; invalid card/type ABEND (RC 16) |

### How the harness works (`lib/harness.py`)

* **Compile**: `cobc -x -std=ibm -fsign=EBCDIC -ftab-width=4 -I app/cpy`.
  `-fsign=EBCDIC` makes zoned-decimal signs (`{`, `}`, `A-R`) match z/OS and
  `app/data/ASCII`; `-ftab-width=4` is needed because `CUSTREC.cpy` contains TABs.
  Executables are cached in `tests/.build/` (git-ignored) by source hash.
* **VSAM KSDS** become GnuCOBOL indexed files built by a generated loader
  (`Ksds(reclen, key, alt_keys)` in `batch/base.py` mirrors the IDCAMS
  DEFINE CLUSTER/AIX). Outputs are unloaded the same way for assertions.
  QSAM datasets are fixed-length record-sequential files.
* **DD names** map to files with GnuCOBOL's `DD_<ddname>` environment variables.
* **Fixtures** (`fixtures/*.txt`, one record per line) are generated from
  readable values by `fixtures/make_fixtures.py`, which encodes fields using
  the layouts parsed from `app/cpy`. Re-run it after editing.
* **z/OS-only pieces**, kept as small and explicit as possible:
  * `lib/stubs/CEE3ABD.cbl` - LE abend service; prints and exits RC 16.
  * `lib/stubs/CEEDAYS.c` - LE date service used by CSUTLDTC; implements the
    documented feedback codes (2507/2508/2513/2517/2518/2520) and Lilian day
    numbers. The unit suite cross-checks Lilian numbers against GnuCOBOL's
    `INTEGER-OF-DATE`. CSUTLDTC's own logic (VSTRING setup, feedback mapping,
    result text, RETURN-CODE) is what is under test.
  * `lib/drivers/RUNACT04.cbl` - passes `PARM='2022071800'` to CBACT04C as LE would.
  * CBSTM03A's PSA/TCB/TIOT control-block walk (diagnostic DD listing) is
    commented out in a build copy; nothing else in any program is modified.

## Known defects found (tests marked `expectedFailure`)

Each test asserts the correct, hand-calculated result and is decorated with
`@unittest.expectedFailure`. When the program is fixed the run reports an
"unexpected success" and fails, so the decorator must be removed.

| Program | Defect |
|---|---|
| CBACT04C | The last account in TCATBAL never gets its interest added / cycle reset: the end-of-file `ELSE PERFORM 1050-UPDATE-ACCOUNT` is unreachable |
| CBTRN01C | The last daily transaction is validated twice after end-of-file |
| CBTRN03C | At end-of-file the last transaction is added to the totals again (grand total 450.00 instead of 250.00) and the last card gets no account total |

Other findings that the static checks allow-list (so new cases still fail):
CBTRN01C has no JCL; CBEXPORT/CBIMPORT declare their `RECORD KEY` in
WORKING-STORAGE (GnuCOBOL rejects it); `COADM02Y`/`COMEN02Y` option tables
REDEFINE a smaller VALUE block (OCCURS 9 over 6 entries, 12 over 11);
`CUSTREC.cpy`/`CSLKPCDY.cpy` contain TABs.

## CICS online programs (not executed)

The `CO*` programs need a CICS region (EXEC CICS, BMS maps, COMMAREA, CICS
file control). GnuCOBOL cannot translate or run them, so they are covered
only by the screen-flow specifications in [`cics/`](cics/README.md), to be
executed manually or on an emulator (IBM ZD&T, Micro Focus Enterprise
Server). Status: pending a CICS test environment. Date validation used by
CT02/CR00 (`CSUTLDTC`) is unit-tested here.

## Adding a batch test

1. Add records to `fixtures/make_fixtures.py`, run it.
2. Create `batch/test_<pgm>.py` subclassing `BatchTestCase`; build KSDS with
   `self.ksds(KSDS_..., name, fixture)`, run with `harness.run(self.build(), dds)`.
3. Decode outputs with the `records` layouts and assert; add the module to
   `batch/run_batch_tests.sh`.

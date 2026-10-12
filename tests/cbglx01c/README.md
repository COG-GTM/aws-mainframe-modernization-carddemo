# CBGLX01C verification

Run the complete local suite from the repository root:

```sh
python3 tests/cbglx01c/run_tests.py
```

The test runner compiles `app/cbl/CBGLX01C.cbl` with GnuCOBOL and the EBCDIC
trailing-overpunch option, creates fixed-record inputs under `work/`, and
compares return codes, `GLSUMOUT` bytes, and `GLRECRPT` bytes.

## Artifacts

- `gen_fixtures.py` defines the cases and an independent Python model of the
  specified validation, reconciliation, and interface layouts. It does not
  read output from the COBOL program.
- `expected/<case>/GLSUMOUT.dat` and `expected/<case>/RC` contain the
  model-derived interface bytes and return code. An absent interface is
  represented by `GLSUMOUT.absent`.
- `expected/<case>/GLRECRPT.rpt` is a reviewed snapshot of the program's
  fixed-width report. The runner checks its result code, control totals,
  exception codes, and exception cap against the independent model before
  comparing report bytes.
- `build/` and `work/` are generated locally and are ignored by Git.

Regenerate interface expectations only from the Python model:

```sh
python3 tests/cbglx01c/gen_fixtures.py --write-expected
```

To run the program directly against the Python model before storing any
expectations, use:

```sh
python3 tests/cbglx01c/run_tests.py --oracle-only
```

Report snapshots can be refreshed from actual program output only after the
runner verifies each report's result and control totals:

```sh
python3 tests/cbglx01c/run_tests.py --update-report-goldens
```

Review updated report snapshots before treating them as expected output. Print
one report while running the suite with:

```sh
python3 tests/cbglx01c/run_tests.py -v 07_out_of_balance_tcatbal
```

The interface and return-code expectations are oracle-derived; report goldens
are reviewed program-output snapshots. Tests run with GnuCOBOL against ASCII
files and are not run on z/OS; mainframe execution and byte equivalence are
not verified or claimed.

## Cases

| Case | Coverage | Expected RC |
|---|---|---:|
| `01_balanced_multi_account` | Multiple accounts, types, categories, debits, and credits | 0 |
| `02_empty_input` | Empty source; header and zero-total trailer | 4 |
| `03_single_record` | One accepted transaction | 0 |
| `04_negative_amounts_only` | Credit-only input and negative net | 0 |
| `05_max_picture_amounts` | Maximum transaction amounts and zero net | 0 |
| `06_key_amount_overflow` | Key accumulator overflow; header only | 12 |
| `07_out_of_balance_tcatbal` | Category-balance difference of 0.01 | 8 |
| `08_key_missing_from_tcatbal` | Activity key absent from category balances | 8 |
| `09_tcatbal_only_balance` | Nonzero category balance without activity | 8 |
| `10_feeder_totals_match` | Matching feeder count and net | 0 |
| `11_feeder_totals_mismatch` | Both feeder control totals differ | 8 |
| `12_duplicate_tran_id` | Duplicate transaction rejected | 4 |
| `13_transact_out_of_sequence` | Input key order abort | 12 |
| `14_tcatbal_duplicate_key` | Duplicate category-balance key abort | 12 |
| `15_bad_proc_dates` | Invalid dates and valid leap days | 4 |
| `16_proc_date_after_run_date` | Processing date later than run date | 4 |
| `17_card_not_in_xref` | Missing card cross-reference | 4 |
| `18_non_numeric_amount` | Invalid transaction amount | 4 |
| `19_invalid_run_date` | Invalid control-card date; interface absent | 12 |
| `20_missing_input_file` | Missing transaction input; interface absent | 12 |
| `21_carddemo_sample_data` | 300 fixed records with derived category balances | 0 |
| `22_zero_amount` | Accepted zero-amount transaction | 0 |
| `23_exception_cap` | Caps 60 missing-card exceptions at 50 and reports 10 omitted | 4 |

The sample-data case assumes all 300 transactions post successfully. Its
category balances start at zero and add the per-key activity sums; this does
not replay the posting program's transaction-limit or expiry checks.

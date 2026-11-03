## Matches COBOL, source defect

- CBACT04C never rewrites the account of the last TCATBAL record (the end-of-file branch skips
  `1050-UPDATE-ACCOUNT`); the Java job reproduces it (`docs/migration/jcl-mapping.md`). Both ACCTFILE
  after-images agree on it, so it is not a deviation.
- INTCALC runs once with `--parm=2022071800`: the Java job refuses a second run with the same PARM (the
  generated TRAN-IDs would collide), so the harness never reruns it against the same database.

## Notes

- The export rows marked "not written by the chain" are the tables POSTTRAN/INTCALC only read, unloaded after
  INTCALC and compared with `app/data/ASCII`: the five schemas end the chain holding the source data.

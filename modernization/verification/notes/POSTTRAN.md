## Notes

- Java POSTTRAN writes TRANSACT, DALYREJS and TCATBALF but no ACCTFILE: the account after-image is
  `account.accounts` unloaded with `data-loader --export` (TableExport, RecordWriter) and compared field for
  field with the CBTRN02C ACCTFILE KSDS (ACCT-CURR-BAL, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT and the
  other mapped fields).
- Source-data discrepancies between the ASCII and EBCDIC copies (acctdata record 49 ZIP, discgrp record 34
  rate, `docs/migration/conventions.md`) do not enter this comparison: both sides read `app/data/ASCII`.

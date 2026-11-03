## Matches COBOL, source defect

- CBTRN03C quirks (page/account totals, the last-card total) are reproduced by the Java report
  (`docs/migration/architecture.md`, UNT149-14); the TRANREPT files are compared byte for byte.

## Notes

- The COBOL TRANSACT the report reads is POSTTRAN's TRANSACT plus INTCALC's SYSTRAN in TRAN-ID order: the
  COMBTRAN step (SORT + REPRO), emulated in `reference.py`. On the Java side both jobs write
  `transaction.transactions` directly, and TRANREPT unloads it from transaction-service (TRANSACT.BKUP).
- DATEPARM `2022-01-01 2099-12-31` selects every transaction on both sides (COBOL stamps 2022-07-18,
  Java the wall clock).

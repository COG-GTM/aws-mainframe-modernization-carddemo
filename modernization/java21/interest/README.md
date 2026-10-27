# Interest calculation

This Java 21 module ports the CBACT04C interest calculation to a plain Java API. It consumes the fixed-width record models from `records` and returns transactions, account records, and the DISPLAY output lines in an `InterestRun`; it does not open files or use Spring.

## Rule-to-code map

| Rule | Implementation |
| --- | --- |
| INT-01 | `InterestCalculator` validates the ten-character run parameter. |
| INT-03, INT-04 | `run` sorts category balances by key and processes account breaks. |
| INT-05 | `skipLastAccountUpdatePerInt05` preserves the legacy last-account behavior. |
| INT-06, INT-07 | Account and cross-reference indexes raise `InterestAbendException` on misses. The cross-reference selection is an explicit assumption: first by card-number primary-key order. |
| INT-08, INT-09 | `disclosureKey` and the rate lookup implement group/type/category lookup with `DEFAULT` fallback. |
| INT-10 | A zero rate skips transaction creation and the fee call. |
| INT-11, INT-12 | `truncateToPic` and `run` calculate monthly interest and the per-account total. |
| INT-13, INT-14, INT-15 | `run` creates transaction records and run-wide transaction ids. |
| INT-16 | `timestamp` formats the injected clock in its zone to hundredths of a second. |
| INT-17 | `updateAccount` adds the total and clears cycle credit/debit. |
| INT-18 | `computeFees` is an explicit no-op. |
| INT-19 | `InterestAbendException` carries displayed error lines and the file status. |
| INT-02, INT-20 | The in-memory API returns the start/end output lines instead of opening/closing datasets. |

## Port decisions

- **INT-05:** Keep the legacy behavior: the final account's transactions are returned, but its account record is not rewritten.
- **INT-11:** Truncate interest to cents without rounding, then discard high-order digits to fit the COBOL PIC field.
- **INT-18:** Fees remain uncomputed because the COBOL fee paragraph is empty.

## Verification

Run the Java 21 reactor verification from the repository root:

```sh
mvn -f modernization/java21/pom.xml verify
```

`InterestDifferentialTest` runs the calculator on the golden master's post-posting snapshots (`golden/expected/{mixed,volume}/CBTRN02C.*.bin`) with the pinned clock and run date from `golden/expected/metadata.json`, then compares `SYSTRAN.bin`, `CBACT04C.ACCTFILE.bin` and `CBACT04C.sysout.txt` byte for byte and field by field. It checks each file against `golden/acceptance.sha256` first and applies no tolerance.

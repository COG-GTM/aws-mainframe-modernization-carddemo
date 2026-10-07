## Online changes: independent expected files vs Java export

COBOL side: `build/golden-set/cobol/after-online` · Java side: `build/golden-set/java/after-online` · allow-list: `scripts/golden-set/expected-diffs/online.txt` (2 entries)

| dataset | copybook | fields/record | COBOL records | Java records | records compared | differences | explained | unexplained |
|---|---|---|---|---|---|---|---|---|
| ACCTDATA | CVACT01Y | 13 | 50 | 50 | 50 | 2 | 2 | 0 |
| CUSTDATA | CVCUS01Y | 19 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDDATA | CVACT02Y | 7 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDXREF | CVACT03Y | 4 | 50 | 50 | 50 | 0 | 0 | 0 |
| TRANSACT | CVTRA05Y | 14 | 3 | 3 | 3 | 0 | 0 | 0 |
| USRSEC | CSUSR01Y | 6 | 10 | 10 | 10 | 0 | 0 | 0 |

| dataset | key | field | COBOL value | Java value | explained by |
|---|---|---|---|---|---|
| ACCTDATA | 00000000010 | ACCT-ADDR-ZIP | `''` | `A000000000` | docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP |
| ACCTDATA | 00000000049 | ACCT-ADDR-ZIP | `A000000000` | `ZEROAPR` | Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt |

Result: **PASS** — 2 differences, 2 explained, 0 unexplained, 0 allow-list entries not matched exactly once.

## Final datasets after the nightly cycle: GnuCOBOL vs Java

COBOL side: `build/golden-set/cobol/cycle/FINAL` · Java side: `build/golden-set/java/final` · allow-list: `scripts/golden-set/expected-diffs/final.txt` (3 entries)

| dataset | copybook | fields/record | COBOL records | Java records | records compared | differences | explained | unexplained |
|---|---|---|---|---|---|---|---|---|
| ACCTDATA | CVACT01Y | 13 | 50 | 50 | 50 | 2 | 2 | 0 |
| CUSTDATA | CVCUS01Y | 19 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDDATA | CVACT02Y | 7 | 50 | 50 | 50 | 0 | 0 | 0 |
| CARDXREF | CVACT03Y | 4 | 50 | 50 | 50 | 0 | 0 | 0 |
| TRANSACT | CVTRA05Y | 14 | 312 | 312 | 312 | 0 | 0 | 0 |
| TCATBALF | CVTRA01Y | 5 | 100 | 100 | 100 | 100 | 100 | 0 |
| USRSEC | CSUSR01Y | 6 | 10 | 10 | 10 | 0 | 0 | 0 |

| dataset | key | field | COBOL value | Java value | explained by |
|---|---|---|---|---|---|
| ACCTDATA | 00000000010 | ACCT-ADDR-ZIP | `''` | `A000000000` | docs/modernization/rules/COACTUPC.md "Deviation (R-39, ACCT-ADDR-ZIP)": COBOL ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP, so the update writes the (blank) group id over the ZIP; Java keeps the stored ZIP |
| ACCTDATA | 00000000049 | ACCT-ADDR-ZIP | `A000000000` | `ZEROAPR` | Sample data, not behaviour: ACCT-ADDR-ZIP is A000000000 in app/data/ASCII (COBOL input) and ZEROAPR in app/data/EBCDIC (initial-load input); modernization/carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt |
| TCATBALF | every record (100) | FILLER | `0000000000000000000000` | `''` | docs/modernization/adr/ADR-0011-vsam-to-relational.md: FILLER is not persisted in table mode |

Result: **PASS** — 102 differences, 102 explained, 0 unexplained, 0 allow-list entries not matched exactly once.

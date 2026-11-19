# POSTTRAN golden cases

GnuCOBOL oracle outputs of `.migration/tools/posttran_gnucobol_oracle.sh` (`ORACLE_NOW` default, D-007), one directory per case:
`input/` (DALYTRAN, plus CARDXREF for GC-6) and `expected/` (`DALYREJS`, `SYSOUT`, `RETURN-CODE`, `ACCTDATA.unl`, `TCATBALF.unl`, `TRANSACT.unl`).

| Case | Input | Oracle RC | Runbook |
|---|---|---|---|
| GC-1 | `app/data/ASCII/dailytran.txt` (300 records, 38 rejects) | 4 | R-04 |
| GC-2 | `syn_all` (reasons 100/102/103) | 4 | R-11 |
| GC-3 | `syn_ok` (all valid) | 0 | R-11 |
| GC-4 | in-file duplicate TRAN-ID (abend); `expected-prefix/` = oracle on the 2 valid records | 231 (Java: 16) | R-06, R-13 |
| GC-5 | empty | 0 | R-12 |
| GC-6 | 1 valid record + 1 card whose account is missing, custom CARDXREF (reason 101) | 4 | R-20 |
| GC-7 | `app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS` (CP037); oracle DALYREJS is in ISO-8859-1, compare after converting to CP037 | 4 | R-14 |

Comparison rules (plan section 7) live in `com.carddemo.app.golden.GoldenCompare` / `GoldenCase`: unloads compare per record padded to LRECL
(GnuCOBOL drops trailing spaces), TRANSACT bytes 325-326 masked (D-007); abend cases compare RC 16, tables against `expected-prefix/`, and expect no DALYREJS (D-009, D-012).

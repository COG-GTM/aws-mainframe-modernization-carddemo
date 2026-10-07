# CBIMPORT

JCL: `app/jcl/CBIMPORT.jcl`
- STEP10 EXEC PGM=CBIMPORT reading the KSDS written by CBEXPORT — compiled from the patched copy (see `00-COMPILE/CBIMPORT.gnucobol.patch`).
- The EBCDIC sample `AWS.M2.CARDDEMO.EXPORT.DATA.PS` is not used: the dependency map chains CBIMPORT on CBEXPORT's output.

| DD | file |
|---|---|
| EXPFILE | `ksds/EXPORT.idx` |
| CUSTOUT | `ds/AWS.M2.CARDDEMO.IMPORT.CUSTOUT` |
| ACCTOUT | `ds/AWS.M2.CARDDEMO.IMPORT.ACCTOUT` |
| XREFOUT | `ds/AWS.M2.CARDDEMO.IMPORT.XREFOUT` |
| TRNXOUT | `ds/AWS.M2.CARDDEMO.IMPORT.TRNXOUT` |
| CARDOUT | `ds/AWS.M2.CARDDEMO.IMPORT.CARDOUT` |
| ERROUT | `ds/AWS.M2.CARDDEMO.IMPORT.ERROUT` |

## Outputs
- `CUSTOUT.txt`: 50 records x LRECL 500
- `ACCTOUT.txt`: 50 records x LRECL 300
- `XREFOUT.txt`: 50 records x LRECL 50
- `TRNXOUT.txt`: 312 records x LRECL 350
- `CARDOUT.txt`: 50 records x LRECL 150
- `ERROUT.txt`: 0 records x LRECL 132

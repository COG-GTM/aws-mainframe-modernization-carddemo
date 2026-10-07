# POSTTRAN

JCL: `app/jcl/POSTTRAN.jcl STEP15`
Program: `CBTRN02C`
- CBTRN02C OPENs TRANFILE OUTPUT: GnuCOBOL recreates the KSDS, so the TRANFILE seed record is replaced by the posted transactions.
- TRAN-PROC-TS comes from CURRENT-DATE, frozen at 2022-07-06 00:00:00.00.
- RC 4 is set by the program when any transaction is rejected.

| DD | file |
|---|---|
| TRANFILE | `ksds/TRANSACT.idx` |
| DALYTRAN | `data/DALYTRAN.dat` |
| XREFFILE | `ksds/CARDXREF.idx` |
| DALYREJS | `gdg/AWS.M2.CARDDEMO.DALYREJS.G0001V00` |
| ACCTFILE | `ksds/ACCTDATA.idx` |
| TCATBALF | `ksds/TCATBALF.idx` |

## Outputs
- `DALYREJS.txt`: 38 records x LRECL 430
- `TRANSACT.ksds.txt`: after-image of AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS (262 records, key order)
- `ACCTDATA.ksds.txt`: after-image of AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS (50 records, key order)
- `TCATBALF.ksds.txt`: after-image of AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS (100 records, key order)

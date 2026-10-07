# CBTRN01C

JCL: `(none — no JCL in app/jcl runs CBTRN01C; DD names from its SELECTs)`
Program: `CBTRN01C`
- Read-only validation pass over DALYTRAN against XREF/ACCT; run before POSTTRAN so it sees the seeded masters.

| DD | file |
|---|---|
| DALYTRAN | `data/DALYTRAN.dat` |
| CUSTFILE | `ksds/CUSTDATA.idx` |
| XREFFILE | `ksds/CARDXREF.idx` |
| CARDFILE | `ksds/CARDDATA.idx` |
| ACCTFILE | `ksds/ACCTDATA.idx` |
| TRANFILE | `ksds/TRANSACT.idx` |

## Outputs

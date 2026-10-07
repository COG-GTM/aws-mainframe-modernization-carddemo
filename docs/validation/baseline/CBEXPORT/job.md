# CBEXPORT

JCL: `app/jcl/CBEXPORT.jcl`
- STEP10 IDCAMS DELETE/DEFINE CLUSTER EXPORT.DATA KEYS(4 28) RECSZ(500) (emulated: IDXEXPOR LOAD of an empty file; the key is built at offset 27 = EXPORT-SEQUENCE-NUM per CVEXPORT; CBEXPORT then OPENs it OUTPUT anyway).
- STEP20 EXEC PGM=CBEXPORT — compiled from the patched copy (see `00-COMPILE/CBEXPORT.gnucobol.patch`).
- EXPORT-TIMESTAMP comes from ACCEPT DATE/TIME, frozen at 2022-07-06 00:00:00.00.
- The 4-byte COMP sequence key renders as `\xNN` escapes in the KSDS after-image.

| DD | file |
|---|---|
| CUSTFILE | `ksds/CUSTDATA.idx` |
| ACCTFILE | `ksds/ACCTDATA.idx` |
| XREFFILE | `ksds/CARDXREF.idx` |
| TRANSACT | `ksds/TRANSACT.idx` |
| CARDFILE | `ksds/CARDDATA.idx` |
| EXPFILE | `ksds/EXPORT.idx` |

## Outputs
- `EXPORT.ksds.txt`: after-image of AWS.M2.CARDDEMO.EXPORT.DATA (512 records, key order)

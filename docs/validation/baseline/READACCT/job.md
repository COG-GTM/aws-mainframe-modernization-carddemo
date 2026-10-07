# READACCT

JCL: `app/jcl/READACCT.jcl`
Program: `CBACT01C`
- CALL 'COBDATFT' is satisfied by `scripts/baseline/stubs/COBDATFT.cbl`.
- OUTFILE/ARRYFILE contain COMP-3 fields; non-printable bytes are rendered as `\xNN`.
- VBRCFILE is RECORDING MODE V; rendered as `length|data`.

| DD | file |
|---|---|
| ACCTFILE | `ksds/ACCTDATA.idx` |
| OUTFILE | `ds/AWS.M2.CARDDEMO.ACCTDATA.PSCOMP` |
| ARRYFILE | `ds/AWS.M2.CARDDEMO.ACCTDATA.PSARRY` |
| VBRCFILE | `ds/AWS.M2.CARDDEMO.ACCTDATA.PSVBRC` |

## Outputs
- `OUTFILE.txt`: 50 records x LRECL 107
- `ARRYFILE.txt`: 50 records x LRECL 110
- `VBRCFILE.txt`: 100 variable records (GnuCOBOL VARSEQ format 0: 2-byte BE length + 2 NUL)

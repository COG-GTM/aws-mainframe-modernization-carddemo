# INTCALC

JCL: `app/jcl/INTCALC.jcl STEP15 (EXEC PGM=CBACT04C,PARM='2022071800')`
Program: `CBACT04C`
- Run through the RUNCB04 driver which passes PARM='2022071800' as a halfword-prefixed area.
- XREFFILE is opened with ALTERNATE RECORD KEY FD-XREF-ACCT-ID (built by XREFFILE load).

| DD | file |
|---|---|
| TCATBALF | `ksds/TCATBALF.idx` |
| XREFFILE | `ksds/CARDXREF.idx` |
| ACCTFILE | `ksds/ACCTDATA.idx` |
| DISCGRP | `ksds/DISCGRP.idx` |
| TRANSACT | `gdg/AWS.M2.CARDDEMO.SYSTRAN.G0001V00` |

## Outputs
- `TRANSACT.txt`: 50 records x LRECL 350
- `ACCTDATA.ksds.txt`: after-image of AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS (50 records, key order)

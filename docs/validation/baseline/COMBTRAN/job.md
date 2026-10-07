# COMBTRAN

JCL: `app/jcl/COMBTRAN.jcl`
- STEP05 SORT: `SORT FIELDS=(1,16,CH,A)` over TRANSACT.BKUP(0) + SYSTRAN(0) -> TRANSACT.COMBINED(+1) (emulated in Python, stable byte-order sort on TRAN-ID).
- STEP10 IDCAMS REPRO TRANSACT.COMBINED(0) -> TRANSACT.VSAM.KSDS (emulated: IDXTRANS LOAD; the cluster was emptied by TRANBKP).

## Outputs
- `TRANSACT.COMBINED.txt`: 312 records x LRECL 350
- `TRANSACT.ksds.txt`: after-image of AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS (312 records, key order)

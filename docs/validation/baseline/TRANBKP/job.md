# TRANBKP

JCL: `app/jcl/TRANBKP.jcl`
- STEP05 IDCAMS REPRO TRANSACT.VSAM.KSDS -> TRANSACT.BKUP(+1) (emulated: IDXTRANS UNLD).
- STEP10/STEP15 IDCAMS DELETE + DEFINE CLUSTER TRANSACT.VSAM.KSDS (emulated: IDXTRANS LOAD from an empty file -> empty KSDS).
- The AIX/PATH redefinition (TRANIDX) is not emulated: no batch program declares that key.

## Outputs
- `TRANSACT.BKUP.txt`: 262 records x LRECL 350
- `TRANSACT.ksds.txt`: after-image of AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS (0 records, key order)

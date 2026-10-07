# TRANFILE

JCL: `app/jcl/TRANFILE.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS` RECSZ(350) KEYS(16 0)
- Input: `00-DATA/DALYTRAN.INIT.txt` (fixed 350-byte records)
- Emulation: generated GnuCOBOL utility `IDXTRANS LOAD` writes a BDB indexed file.
- Source is `AWS.M2.CARDDEMO.DALYTRAN.PS.INIT` (EBCDIC only, 1 seed record) converted with cp037. The AIX TRANIDX (TRANFILE.jcl) is not built: no batch program declares it.

## Outputs
- `TRANSACT.ksds.txt`: after-image of AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS (1 records, key order)

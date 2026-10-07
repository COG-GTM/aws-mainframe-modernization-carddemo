# CARDFILE

JCL: `app/jcl/CARDFILE.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS` RECSZ(150) KEYS(16 0)
- Input: `00-DATA/CARDDATA.txt` (fixed 150-byte records)
- Emulation: generated GnuCOBOL utility `IDXCARDD LOAD` writes a BDB indexed file.
- The AIX on account id (CARDFILE.jcl CARDAIX) is not built: no batch program declares it.

## Outputs
- `CARDDATA.ksds.txt`: after-image of AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS (50 records, key order)

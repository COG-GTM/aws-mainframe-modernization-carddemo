# CUSTFILE

JCL: `app/jcl/CUSTFILE.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS` RECSZ(500) KEYS(9 0)
- Input: `00-DATA/CUSTDATA.txt` (fixed 500-byte records)
- Emulation: generated GnuCOBOL utility `IDXCUSTD LOAD` writes a BDB indexed file.


## Outputs
- `CUSTDATA.ksds.txt`: after-image of AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS (50 records, key order)

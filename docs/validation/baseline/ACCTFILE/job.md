# ACCTFILE

JCL: `app/jcl/ACCTFILE.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` RECSZ(300) KEYS(11 0)
- Input: `00-DATA/ACCTDATA.txt` (fixed 300-byte records)
- Emulation: generated GnuCOBOL utility `IDXACCTD LOAD` writes a BDB indexed file.


## Outputs
- `ACCTDATA.ksds.txt`: after-image of AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS (50 records, key order)

# TCATBALF

JCL: `app/jcl/TCATBALF.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS` RECSZ(50) KEYS(17 0)
- Input: `00-DATA/TCATBALF.txt` (fixed 50-byte records)
- Emulation: generated GnuCOBOL utility `IDXTCATB LOAD` writes a BDB indexed file.
- `tcatbal.txt` has CRLF line ends; normalised to 50-byte records.

## Outputs
- `TCATBALF.ksds.txt`: after-image of AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS (50 records, key order)

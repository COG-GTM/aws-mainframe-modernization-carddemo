# TRANCATG

JCL: `app/jcl/TRANCATG.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS` RECSZ(60) KEYS(6 0)
- Input: `00-DATA/TRANCATG.txt` (fixed 60-byte records)
- Emulation: generated GnuCOBOL utility `IDXTRANC LOAD` writes a BDB indexed file.
- `trancatg.txt` has CRLF line ends; normalised to 60-byte records.

## Outputs
- `TRANCATG.ksds.txt`: after-image of AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS (18 records, key order)

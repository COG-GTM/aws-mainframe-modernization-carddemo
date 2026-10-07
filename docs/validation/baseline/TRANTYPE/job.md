# TRANTYPE

JCL: `app/jcl/TRANTYPE.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS` RECSZ(60) KEYS(2 0)
- Input: `00-DATA/TRANTYPE.txt` (fixed 60-byte records)
- Emulation: generated GnuCOBOL utility `IDXTRANT LOAD` writes a BDB indexed file.
- `trantype.txt` has CRLF line ends and a 60-byte last line; normalised to 60-byte records.

## Outputs
- `TRANTYPE.ksds.txt`: after-image of AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS (7 records, key order)

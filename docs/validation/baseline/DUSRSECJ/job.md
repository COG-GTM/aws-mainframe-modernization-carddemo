# DUSRSECJ

JCL: `app/jcl/DUSRSECJ.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS` RECSZ(80) KEYS(8 0)
- Input: `00-DATA/USRSEC.txt` (fixed 80-byte records)
- Emulation: generated GnuCOBOL utility `IDXUSRSE LOAD` writes a BDB indexed file.
- Online-only security file (used by COSGN00C/COUSR*); loaded for completeness from the EBCDIC sample.

## Outputs
- `USRSEC.ksds.txt`: after-image of AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS (10 records, key order)

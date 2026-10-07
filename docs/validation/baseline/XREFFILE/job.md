# XREFFILE

JCL: `app/jcl/XREFFILE.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` RECSZ(50) KEYS(16 0), alternate key(s) [(25, 11)] (declared by CBACT04C)
- Input: `00-DATA/CARDXREF.txt` (fixed 50-byte records)
- Emulation: generated GnuCOBOL utility `IDXCARDX LOAD` writes a BDB indexed file.
- `cardxref.txt` lines are 36 bytes; padded with spaces to the 50-byte LRECL of the cluster. Alternate key (offset 25, len 11 = XREF-ACCT-ID) is defined because CBACT04C declares it; the sample has no duplicate account ids so it is built UNIQUE like the COBOL declaration.

## Outputs
- `CARDXREF.ksds.txt`: after-image of AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS (50 records, key order)

# DISCGRP

JCL: `app/jcl/DISCGRP.jcl` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).
- Cluster: `AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS` RECSZ(50) KEYS(16 0)
- Input: `00-DATA/DISCGRP.txt` (fixed 50-byte records)
- Emulation: generated GnuCOBOL utility `IDXDISCG LOAD` writes a BDB indexed file.


## Outputs
- `DISCGRP.ksds.txt`: after-image of AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS (51 records, key order)

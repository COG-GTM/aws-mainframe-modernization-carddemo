# CREASTMT

JCL: `app/jcl/CREASTMT.JCL`
- STEP010 IDCAMS DELETE TRXFL.VSAM.KSDS (emulated by recreating the cluster).
- STEP020 SORT: `SORT FIELDS=(263,16,CH,A,1,16,CH,A)`, `OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)` over TRANSACT.VSAM.KSDS -> TRXFL.SEQ (emulated in Python; the 328-byte reformatted record is blank-padded to the SORTOUT LRECL 350).
- STEP030 IDCAMS DEFINE CLUSTER TRXFL KEYS(32 0) RECSZ(350) + REPRO TRXFL.SEQ (emulated: IDXTRXFL LOAD).
- STEP040 EXEC PGM=CBSTM03A (patched copy without the z/OS TIOT walk, see `00-COMPILE/CBSTM03A.gnucobol.patch`; statically linked with CBSTM03B).

| DD | file |
|---|---|
| TRNXFILE | `ksds/TRXFL.idx` |
| XREFFILE | `ksds/CARDXREF.idx` |
| ACCTFILE | `ksds/ACCTDATA.idx` |
| CUSTFILE | `ksds/CUSTDATA.idx` |
| STMTFILE | `ds/AWS.M2.CARDDEMO.STATEMNT.PS` |
| HTMLFILE | `ds/AWS.M2.CARDDEMO.STATEMNT.HTML` |

## Outputs
- `TRXFL.SEQ.txt`: 312 records x LRECL 350
- `STMTFILE.txt`: 1262 records x LRECL 80
- `HTMLFILE.txt`: 6632 records x LRECL 100

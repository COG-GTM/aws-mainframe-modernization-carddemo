## nightly-cycle (table mode): one launch vs the golden GnuCOBOL run

`java -jar carddemo-app.jar --job=nightly-cycle --run-date=2022-07-06` on the database after the online scenario (golden set); every job reads the previous Java job's outputs.

Cycle RC (JCL max): 4 (expected 4); batch_run cycle row: COMPLETED COMPLETED RC 4

| job | mode | Java RC / baseline | batch_run step | child jobs | checks | result |
|---|---|---|---|---|---|---|
| READACCT | table | 0 / 0 | COMPLETED RC 0, read 50 / write 200 | 1 | 5 | PASS |
| READCARD | table | 0 / 0 | COMPLETED RC 0, read 50 / write 0 | 1 | 2 | PASS |
| READCUST | table | 0 / 0 | COMPLETED RC 0, read 50 / write 0 | 1 | 2 | PASS |
| READXREF | table | 0 / 0 | COMPLETED RC 0, read 50 / write 0 | 1 | 2 | PASS |
| POSTTRAN | table | 4 / 4 | COMPLETED RC 4, read 600 / write 262 | 5 | 8 | PASS |
| INTCALC | table | 0 / 0 | COMPLETED RC 0, read 100 / write 50 | 3 | 6 | PASS |
| TRANBKP | table | 0 / 0 | COMPLETED RC 0, read 524 / write 262 | 4 | 5 | PASS |
| COMBTRAN | table | 0 / 0 | COMPLETED RC 0, read 624 / write 624 | 3 | 5 | PASS |
| TRANREPT | table | 0 / 0 | COMPLETED RC 0, read 936 / write 1143 | 4 | 6 | PASS |
| CREASTMT | table | 0 / 0 | COMPLETED RC 0, read 936 / write 8518 | 3 | 8 | PASS |
| PRTCATBL | table | 0 / 0 | COMPLETED RC 0, read 200 / write 200 | 2 | 6 | PASS |

Not run by the cycle:

- CLOSEFIL / OPENFIL / WAITSTEP: retired (06-scheduling.md)
- CBPAUP0J: out of scope (IMS/DB2 authorization extension)
- TXT2PDF1: retired (STATEMNT.HTML already produced by CREASTMT)
- TRANTYPE / TRANCATG / TCATBALF / DISCGRP (refresh loads): initial-load / repro (not nightly)
- TRANEXTR / MNTTRDB2: out of scope (Db2 extension)

**RESULT: PASS**

<details><summary>Per-group compare reports</summary>

## CREASTMT (table mode) vs the golden GnuCOBOL run

| check | result |
|---|---|
| CREASTMT RC | Java 0 / baseline 0 |
| STEP010 TRXFL.SEQ(+1) | 312 records of 350 bytes, byte-identical |
| STEP020 TRXFL cluster (+1) | 312 records of 350 bytes, byte-identical |
| STEP040 STATEMNT.PS (STMTFILE) | 1262 records of 80 bytes, byte-identical |
| STEP040 STATEMNT.HTML (HTMLFILE) | 6632 records of 100 bytes, byte-identical |
| STEP010 SORT in/out | Java [312, 312] / baseline [312, 312] |
| STEP020 REPRO count | Java [312] / baseline [312] |
| CBSTM03A SYSOUT | 1 lines, identical |

Documented differences:

- CBSTM03A SYSOUT: baseline `Running JCL : CREASTMT  Step STEP040 (TIOT walk bypassed under GnuCOBOL)` (build-time TIOT patch) / Java `Running JCL : CREASTMT Step STEP040` (CBSTM03A.md R-2)

**RESULT: PASS**

## INTCALC (table mode) vs the golden GnuCOBOL run

| check | result |
|---|---|
| CBACT04C SYSOUT | 302 lines, match (FILLER-only: 100) |
| RC | Java 0 / baseline 0, match |
| SYSTRAN (TRANSACT DD) | 50 records, match |
| ACCTDATA after-image (vs INTCALC/ACCTDATA.ksds.txt) | 50 records, match |
| TCATBALF after-image (vs POSTTRAN/TCATBALF.ksds.txt) | 100 records, match (FILLER-only: 100) |
| DISCGRP record 34 (DEFAULT/07/0001) lookups | 0 of the TCATBALF rows |

Documented differences:

- SYSOUT: 100 DISPLAYed TCATBALF images end in spaces instead of the 22-zero FILLER of the sample (not persisted in table mode, ADR-0011)
- ACCTDATA record 10 ACCT-ADDR-ZIP: baseline `` / Java `A000000000` (documented input-data difference)
- ACCTDATA record 49 ACCT-ADDR-ZIP: baseline `A000000000` / Java `ZEROAPR` (documented input-data difference)
- TCATBALF: FILLER differs in 100 record(s); FILLER is not persisted in table mode (ADR-0011), so the unload writes spaces
- DISCGRP record 34 (DEFAULT/07/0001) has DIS-INT-RATE 15.00 in the EBCDIC sample loaded by initial-load vs 0.00 in the ASCII sample of the baseline; 0 TCATBALF row(s) look it up (no TCATBALF row has type 07), so it cannot change SYSTRAN or ACCTDATA

**RESULT: PASS**

## POSTTRAN (table mode) vs the golden GnuCOBOL run

| check | result |
|---|---|
| CBTRN01C SYSOUT | 1807 lines, match |
| POSTTRAN SYSOUT | 54 lines, match |
| RC | Java 4 / baseline 4, match |
| DALYREJS | 38 records, match |
| DALYREJS reason codes | 0102: 38, match |
| TRANSACT after-image | 262 records, match |
| ACCTDATA after-image | 50 records, match |
| TCATBALF after-image | 100 records, match (FILLER-only: 100) |

Documented differences:

- ACCTDATA record 10 ACCT-ADDR-ZIP: baseline `` / Java `A000000000` (documented input-data difference)
- ACCTDATA record 49 ACCT-ADDR-ZIP: baseline `A000000000` / Java `ZEROAPR` (documented input-data difference)
- TCATBALF: FILLER differs in 100 record(s); FILLER is not persisted in table mode (ADR-0011), so the unload writes spaces

**RESULT: PASS**

## Print jobs (nightly-cycle, table input) vs the golden GnuCOBOL run

| job | output | result |
|---|---|---|
| READACCT | SYSOUT | identical (752 lines, trailing spaces normalised) |
| READACCT | RC | identical (baseline 0, java 0) |
| READACCT | ARRYFILE | identical (50 records, LRECL 110, byte-level) |
| READACCT | OUTFILE | identical (50 records, LRECL 107, byte-level) |
| READACCT | VBRCFILE | identical (100 records, RECFM=V (varseq0), byte-level) |
| READCARD | SYSOUT | identical (52 lines, trailing spaces normalised) |
| READCARD | RC | identical (baseline 0, java 0) |
| READXREF | SYSOUT | identical (102 lines, trailing spaces normalised) |
| READXREF | RC | identical (baseline 0, java 0) |
| READCUST | SYSOUT | identical (102 lines, trailing spaces normalised) |
| READCUST | RC | identical (baseline 0, java 0) |

Known input-data differences (see --expected-diffs):

- READACCT/sysout: expected diff on line `00000000010Y...` `00000000000{00000000000{` -> `00000000000{00000000000{A000000000`: applied
- READACCT/sysout: expected diff on line `00000000049...` `A000000000` -> `ZEROAPR   `: applied

**RESULT: PASS**

## TRANBKP / COMBTRAN / TRANREPT / PRTCATBL (table mode) vs the golden GnuCOBOL run

| check | result |
|---|---|
| TRANBKP RC | Java 0 / baseline 0 |
| TRANBKP TRANSACT.BKUP(+1) | 262 records, match |
| TRANBKP TRANSACT after-image rows | Java 0 / baseline 0 |
| TRANBKP REPRO count | Java [262] / baseline [262] |
| COMBTRAN RC | Java 0 / baseline 0 |
| COMBTRAN TRANSACT.COMBINED(+1) | 312 records, match |
| COMBTRAN TRANSACT after-image | 312 records, match |
| COMBTRAN SORT in/out | Java [312, 312] / baseline [312, 312] |
| COMBTRAN REPRO count | Java [312] / baseline [312] |
| TRANREPT RC | Java 0 / baseline 0 |
| TRANREPT TRANSACT.BKUP(+1) | 312 records, match |
| TRANREPT TRANSACT.DALY(+1) | 312 records, match |
| TRANREPT report (CBTRN03C) | 519 lines of 133 bytes, identical after trailing-space normalisation |
| TRANREPT TRANSACT after-image (unchanged) | 312 records, match |
| CBTRN03C SYSOUT | 317 lines, identical |
| TRANREPT REPRO count | Java [312] / baseline [312] |
| PRTCATBL RC | Java 0 / baseline 0 |
| PRTCATBL TCATBALF.BKUP(+1) | 100 records, match (FILLER-only: 100) |
| PRTCATBL TCATBALF.REPT(+1) | 100 records of 41 bytes, identical |
| PRTCATBL SYSOUT (program lines) | Java 0 / baseline 0 lines, identical |
| PRTCATBL REPRO count | Java [100] / baseline [100] |
| PRTCATBL SORT/OUTREC count | Java [100] / baseline [100] |

Documented differences:

- TCATBALF: FILLER differs in 100 record(s); FILLER is not persisted in table mode (ADR-0011), so the unload writes spaces

**RESULT: PASS**

</details>

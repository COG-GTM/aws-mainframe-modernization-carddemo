# CardDemo batch golden master

Reference outputs for the nightly batch chain, produced by the unchanged COBOL in
`app/cbl` compiled with GnuCOBOL. Java batch jobs under `carddemo-java/` must
reproduce `baselines/` byte for byte.

```
carddemo-java/golden-master/run.sh           # rerun the chain, rewrite baselines/
carddemo-java/golden-master/run.sh --check   # rerun the chain, fail on any difference
```

Both modes run the whole chain twice in separate work directories and fail unless
the two runs are byte-identical. Needs `cobc` (GnuCOBOL 3.1+, BDB indexed files,
e.g. `apt-get install gnucobol3`) and Python 3.10+. CI runs `--check` on every PR
that touches the COBOL, copybooks, JCL, ASCII sample data or this folder.

## The chain

| Job      | Program(s)          | Input                                       | Baselines                                              |
|----------|---------------------|---------------------------------------------|--------------------------------------------------------|
| POSTTRAN | CBTRN02C            | `dailytran.txt`, sample ACCT/XREF/TCATBAL   | `transact`, `acctdata`, `tcatbal`, `dalyrejs` (430-byte rejects) |
| INTCALC  | CBACT04C `PARM='2022071800'` | POSTTRAN's TCATBAL/ACCT, `discgrp.txt` | `systran`, `acctdata`                                  |
| COMBTRAN | SORT + REPRO        | POSTTRAN `transact` + INTCALC `systran`     | `transact`                                             |
| TRANREPT | SORT + CBTRN03C     | COMBTRAN `transact`, 2022-01-01..2022-07-06 | `transact-daly`, `tranrept` (133-byte report)          |
| CREASTMT | SORT + CBSTM03A/CBSTM03B | COMBTRAN `transact`, INTCALC `acctdata`  | `trxfl`, `statemnt.txt`, `statemnt.html`               |

Every job also has a `sysout.txt` (its DISPLAY output). `baselines/MANIFEST.txt`
lists each job's return code and each file's record count and SHA-256.
POSTTRAN ends with RC=4 because 38 daily transactions are rejected; that is the
programs' real behaviour, not a harness failure.

Datasets are stored one record per line with no trailing-space trimming, so a
fixed-width record is line *n* columns 1..LRECL.

## How the mainframe pieces are replaced

- **VSAM KSDS**: GnuCOBOL indexed files. `cobol/KSDSREPRO.cbl.in` is a per-layout
  IDCAMS REPRO stand-in (LOAD a sequential file into the KSDS, UNLOAD it in key order).
- **DD names**: `DD_<ddname>` environment variables pointing into the run's work directory.
- **SORT**: the SORT/INCLUDE/OUTREC cards in `app/jcl` are done in Python with a stable sort
  under `LC_ALL=C`; every key is digits only, so ASCII order equals EBCDIC order.
- **EXEC PARM**: `cobol/JCLPARM.cbl` passes a halfword length plus the PARM text, as z/OS does.
- **CEE3ABD**: `cobol/CEE3ABD.cbl` ends the run unit with RC=16.
- **CBSTM03A**: the build copy drops only the PSA/TCB/TIOT control-block walk that
  DISPLAYs job and DD names; those blocks do not exist off the mainframe.

## Determinism

- Clock frozen with `COB_CURRENT_DATE=2022/07/06 12:00:00.00` (the TRANREPT end date, so
  POSTTRAN and INTCALC postings fall inside the report window). The hundredths must be
  given: without them GnuCOBOL takes them from the real clock.
- `LC_ALL=C`, `TZ=UTC`, and any inherited `DD_*`/`COB_*` variables are cleared.
- Every run starts from fresh copies of `app/data/ASCII`; nothing is reused between runs.

## Customer masking

The sample customers are synthetic but read like real people, and the statements print
names and addresses. Before CREASTMT the harness replaces first/middle/last name and
address lines 1-3 with placeholders keyed on `CUST-ID` (`FIRST-000000050`,
`CITY-000000050`, ...). State, country, ZIP and every other field are unchanged. A Java
port comparing against these baselines must load the same masked CUSTDATA.

# CardDemo batch golden master (GnuCOBOL)

Compiles the CardDemo batch COBOL programs with GnuCOBOL, runs them on the
ASCII sample data in `app/data/ASCII`, and keeps every output under
`baselines/` as the reference that the Java batch jobs must reproduce byte for byte.

```bash
golden-master/run.sh            # regenerate baselines/
golden-master/run.sh --check    # rerun and fail if the output differs from baselines/
```

Requires GnuCOBOL 3.1.x with the BDB indexed-file handler (`apt-get install gnucobol3`).
Build files and scratch datasets go to `golden-master/.work/` (ignored).

## What runs

The jobs run in their Control-M order. Each job starts from the files the previous
job left behind, and the harness rebuilds every VSAM KSDS from text before a step,
the way the JCL's IDCAMS REPRO steps do.

| Job | Program(s) | Inputs | Baselines |
|---|---|---|---|
| POSTTRAN | CBTRN02C | dailytran, cardxref, acctdata, tcatbal | `transact.txt`, `acctdata.txt`, `tcatbal.txt`, `dalyrejs.txt` |
| INTCALC | CBACT04C, `PARM='2022071800'` | POSTTRAN tcatbal and acctdata, cardxref, discgrp | `systran.txt`, `acctdata.txt` |
| COMBTRAN | SORT + REPRO (no COBOL) | POSTTRAN transact + INTCALC systran | `transact.txt` |
| TRANREPT | REPRO, SORT INCLUDE, CBTRN03C | COMBTRAN transact, cardxref, trantype, trancatg | `transact-daly.txt`, `tranrept.txt` |
| CREASTMT | SORT/OUTREC, CBSTM03A + CBSTM03B | COMBTRAN transact, INTCALC acctdata, cardxref, custdata | `trxfl.txt`, `statemnt.txt`, `statemnt.html` |

Each job folder also has `sysout.txt` (the program's DISPLAY output).
`baselines/MANIFEST.txt` lists return codes, record counts and SHA-256 of every file.

## File format

Every baseline is text with one record per line, padded with spaces to the record
length (trailing spaces are significant). Signed zoned-decimal fields keep the
mainframe overpunch the sample data uses (`{`, `A`-`I` positive, `}`, `J`-`R` negative),
via `cobc -fsign=EBCDIC`.

## Fixed run parameters

- Clock frozen at `2022-07-06 12:00:00.00` (`COB_CURRENT_DATE`), so the
  `TRAN-PROC-TS` stamps written by CBTRN02C and CBACT04C are stable and fall in the
  TRANREPT JCL's date range.
- INTCALC `PARM='2022071800'`, TRANREPT range `2022-01-01` to `2022-07-06`, both from the JCL.
- Sorts are stable, byte order (`LC_ALL=C`).

## Off-mainframe adjustments

The COBOL sources in `app/` are compiled unchanged except for two build-copy fixes:

- `CUSTREC.cpy` indents its fields with tabs; copybooks are copied with tabs
  expanded at width 3, which puts them in area B where they belong.
- `CBSTM03A` walks the z/OS PSA/TCB/TIOT control blocks to DISPLAY the job and DD
  names. Those blocks do not exist here, so that diagnostic block is removed from
  the build copy. The statement logic is untouched.

Input masking: the sample customers in `custdata.txt` are synthetic but look like
real people, and the statements print their names and addresses. Before the
CREASTMT load, the harness replaces name and address lines 1-3 with placeholders
keyed on CUST-ID (`F000000050 FIRST`, `000000050 MAIN STREET`, ...). The Java
parity run must use the same masked input.

Harness programs in `src/`:

- `KSDSUTIL.cbl.in`: KSDS load/unload utility (IDCAMS REPRO), one build per record layout.
- `PARMDRV.cbl`: passes a JCL `PARM` to a program as z/OS does (halfword length + text).
- `CEE3ABD.cbl`: stand-in for the LE abend service; ends the run with RC 16.

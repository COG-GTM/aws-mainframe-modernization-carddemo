# GnuCOBOL batch baseline (`scripts/baseline/`)

Reproducible off-mainframe baseline of every CardDemo batch job, used as the
oracle for the Java 21 rewrite. One command regenerates everything under
[`docs/validation/baseline/`](../../docs/validation/baseline/README.md):

```bash
scripts/baseline/run_baseline.sh          # full run; WAITSTEP really sleeps 36 s (JCL SYSIN 00003600)
scripts/baseline/run_baseline.sh --fast   # identical outputs, skips the sleep
```

Requirements: GnuCOBOL 3.1.2 (`cobc` with the BDB indexed-file handler) and Python 3.8+.
The run takes ~5 s (`--fast`); it deletes and recreates `docs/validation/baseline/` and the scratch dir
`build/baseline-work/` (git-ignored, removed at the end unless `--keep-work`), so it is
idempotent: two consecutive runs produce byte-identical trees (`diff -r`).

## What the runner does

1. **Compile matrix** (`00-COMPILE/`): `cobc -fsyntax-only -std=ibm -I app/cpy` on every pristine
   batch source, then the real `cobc -x -std=ibm -fsign=EBCDIC` build with whatever each program needs
   (stubs, drivers, `-ftab-width=1`, build-time patched copy). Each log starts with the exact command.
2. **Data preparation** (`00-DATA/`): fixed-width records from `app/data/ASCII` — CR/LF stripped,
   space-padded to the LRECL from `docs/modernization/inventory.json` (e.g. `cardxref.txt` 36 → 50).
   The ASCII samples are used because GnuCOBOL reads ASCII fixed-length records and the programs
   compare/display text; `-fsign=EBCDIC` makes the `{`/`}`/`A-R` overpunch signs in those files
   decode correctly. Files that exist only in EBCDIC (`DALYTRAN.PS.INIT`, `USRSEC.PS`) are converted
   with codec cp037; where both forms exist the conversion is cross-checked against the ASCII sample
   (result in the summary table). `DATEPARM` (LRECL 80) is generated.
3. **IDCAMS emulation**: one generated GnuCOBOL utility per cluster (`IDX<name> LOAD|UNLD`) implements
   DEFINE CLUSTER + REPRO (load) and REPRO to a flat file in key order (unload). Keys come from the
   load JCLs; alternate indexes are built only where a batch program declares them (CBACT04C on XREF).
4. **DFSORT emulation**: `dfsort()` in `baseline.py` implements `SORT FIELDS` (CH/ZD, A/D), `INCLUDE COND`
   and the `OUTREC` layouts used by COMBTRAN, TRANREPT, CREASTMT and PRTCATBL; the control cards are
   quoted in each job's `job.md`.
5. **Job execution** in dependency order (checked against `dependency-map.json`, see `00-ORDER.md`):
   file loads → READACCT/READCARD/READCUST/READXREF → CBTRN01C → POSTTRAN → INTCALC → TRANBKP →
   COMBTRAN → TRANREPT → CREASTMT → PRTCATBL → CBEXPORT → CBIMPORT → WAITSTEP → CSUTLDTC.
   Files are assigned with `DD_<ddname>` environment variables (GnuCOBOL `-std=ibm` filename mapping
   for `SELECT ... ASSIGN TO ddname`). GDG `(+1)`/`(0)` references are emulated by a tiny catalog.
6. **Capture**: per job `sysout.txt`, `rc.txt`, `job.md` (steps, emulations, DD table) and one
   `.txt` per output dataset (one record per line; bytes outside 0x20–0x7E as `\xNN`) plus the
   after-image of every KSDS the job updated (`<KSDS>.ksds.txt`).

## Fixed values (determinism)

| What | Value | Source |
|---|---|---|
| Clock (`COB_CURRENT_DATE`) | `2022-07-06 00:00:00.00` | end of the TRANREPT window, so posted TRAN-PROC-TS fall inside it |
| INTCALC PARM | `2022071800` | `app/jcl/INTCALC.jcl` |
| DATEPARM / TRANREPT window | `2022-01-01` .. `2022-07-06` | `app/jcl/TRANREPT.jcl` SYMNAMES |
| WAITSTEP SYSIN | `00003600` | `app/jcl/WAITSTEP.jcl` |

## Stubs (`stubs/`) and drivers (`drivers/`)

| Program | Replaces | Semantics |
|---|---|---|
| `CEE3ABD` | LE abend service | prints `USER ABEND Unnnn`, STOP RUN with RC = abend code (no-arg calls → RC 16) |
| `COBDATFT` | `app/asm/COBDATFT.asm` | byte-for-byte port of the assembler date reformatter |
| `MVSWAIT` | `app/asm/MVSWAIT.asm` | prints the centisecond delay and sleeps (`BASELINE_MVSWAIT_NOSLEEP=1` skips) |
| `CEEDAYS` | LE date service | mask-driven date validation + Lilian day, feedback tokens CSUTLDTC tests |
| `RUNCB04` | JCL `PARM=` | halfword-prefixed PARM area for `CBACT04C` |
| `RUNDTC` | (none) | drives subprogram `CSUTLDTC` through 12 fixed dates |

Each file's header cites the branch/SHA it was harvested from.

## Build-time source patches (never applied to `app/`)

Written as unified diffs to `docs/validation/baseline/00-COMPILE/<PGM>.gnucobol.patch`:

- **CBEXPORT / CBIMPORT** — `RECORD KEY IS EXPORT-SEQUENCE-NUM` names a WORKING-STORAGE item
  (copybook `CVEXPORT`), which no COBOL compiler accepts; the patch adds a second 01 record view of
  the FD exposing the same 4 bytes at offset 27.
- **CBSTM03A** — walks z/OS control blocks (PSA → TCB → TIOT) only to DISPLAY the job name and DD
  list; this SIGSEGVs off-mainframe, so the walk is replaced by a DISPLAY.

## Not runnable here

The 17 CICS programs (`CO*`) need `EXEC CICS`; their behaviour is written up as testable rules in
[`docs/modernization/rules/`](../../docs/modernization/rules/).

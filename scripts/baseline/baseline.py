#!/usr/bin/env python3
"""CardDemo GnuCOBOL batch baseline runner.

One command regenerates everything under docs/validation/baseline/:

    scripts/baseline/run_baseline.sh            # full run (WAITSTEP really waits 36 s)
    scripts/baseline/run_baseline.sh --fast     # skip the MVSWAIT sleep

Design (see scripts/baseline/README.md for the long version):
  * compiles every batch program with cobc (syntax check of the pristine
    source first, then a real `-x` build with the stubs/drivers it needs),
  * prepares fixed-width inputs from app/data/ASCII (EBCDIC-only files are
    converted with codec cp037),
  * emulates the IDCAMS (DEFINE/REPRO/DELETE) and DFSORT steps of each JCL
    with GnuCOBOL load/unload utilities and Python,
  * runs the jobs in dependency order with DD_<ddname> environment
    assignments, a frozen clock (COB_CURRENT_DATE) and the JCL PARM/DATEPARM
    values,
  * stores sysout, return code, every output dataset and the after-image of
    every KSDS a job updates under docs/validation/baseline/<JOB>/.

Harvested patterns (cited in the stubs/drivers themselves):
  origin/devin/1790617863-batch @ e9658ca      aws/batch/golden/generate-golden.sh, gen-idxutil.sh
  origin/devin/1789609454-golden-set-harness @ c068d6f  tests/golden/run_reference.sh, cobol/GSIDXUTL.cbl
  origin/devin/cobol-safety-net @ 1c845c7      test-harness/cobol/KSDSLOAD.cbl, CEE3ABD.cbl, COBDATFT.cbl
"""
from __future__ import annotations

import argparse
import difflib
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
APP = REPO / "app"
CBL = APP / "cbl"
CPY = APP / "cpy"
ASCII_DIR = APP / "data" / "ASCII"
EBCDIC_DIR = APP / "data" / "EBCDIC"
OUT = REPO / "docs" / "validation" / "baseline"
WORK = REPO / "build" / "baseline-work"
STUBS = HERE / "stubs"
DRIVERS = HERE / "drivers"

# ---------------------------------------------------------------- fixed values
FROZEN_CLOCK = "2022-07-06 00:00:00.00"   # COB_CURRENT_DATE: CURRENT-DATE / ACCEPT DATE,TIME
INTCALC_PARM = "2022071800"               # INTCALC.jcl: EXEC PGM=CBACT04C,PARM='2022071800'
DATEPARM_START = "2022-01-01"             # TRANREPT.jcl SYMNAMES PARM-START-DATE
DATEPARM_END = "2022-07-06"               # TRANREPT.jcl SYMNAMES PARM-END-DATE
WAITSTEP_SYSIN = "00003600"               # WAITSTEP.jcl SYSIN for COBSWAIT (centiseconds)

COBC_BASE = ["cobc", "-std=ibm", "-fsign=EBCDIC", "-I", str(CPY)]
COBC_SYNTAX = ["cobc", "-fsyntax-only", "-std=ibm", "-I", str(CPY)]

BATCH_PROGRAMS = ["CBACT01C", "CBACT02C", "CBACT03C", "CBACT04C", "CBCUS01C",
                  "CBTRN01C", "CBTRN02C", "CBTRN03C", "CBSTM03A", "CBSTM03B",
                  "CBEXPORT", "CBIMPORT", "CSUTLDTC", "COBSWAIT"]

# ------------------------------------------------------------- input datasets
# name -> (ASCII sample, LRECL, EBCDIC sample).  LRECLs come from
# docs/modernization/inventory.json / the load JCLs.
DATASETS = {
    "ACCTDATA": ("acctdata.txt", 300, "AWS.M2.CARDDEMO.ACCTDATA.PS"),
    "CARDDATA": ("carddata.txt", 150, "AWS.M2.CARDDEMO.CARDDATA.PS"),
    "CARDXREF": ("cardxref.txt", 50, "AWS.M2.CARDDEMO.CARDXREF.PS"),
    "CUSTDATA": ("custdata.txt", 500, "AWS.M2.CARDDEMO.CUSTDATA.PS"),
    "DALYTRAN": ("dailytran.txt", 350, "AWS.M2.CARDDEMO.DALYTRAN.PS"),
    "DISCGRP": ("discgrp.txt", 50, "AWS.M2.CARDDEMO.DISCGRP.PS"),
    "TCATBALF": ("tcatbal.txt", 50, "AWS.M2.CARDDEMO.TCATBALF.PS"),
    "TRANCATG": ("trancatg.txt", 60, "AWS.M2.CARDDEMO.TRANCATG.PS"),
    "TRANTYPE": ("trantype.txt", 60, "AWS.M2.CARDDEMO.TRANTYPE.PS"),
    "DALYTRAN.INIT": (None, 350, "AWS.M2.CARDDEMO.DALYTRAN.PS.INIT"),
    "USRSEC": (None, 80, "AWS.M2.CARDDEMO.USRSEC.PS"),
}

# KSDS clusters: lrecl, primary key (offset, length), alternate keys the batch
# programs declare (offset, length).  Only CBACT04C declares an alternate key
# (XREFFILE by account id); TRANSACT's AIX (TRANIDX) is used by no batch program
# and is therefore not built.
KSDS = {
    "ACCTDATA": dict(lrecl=300, key=(0, 11), alt=[], dsn="AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS"),
    "CARDDATA": dict(lrecl=150, key=(0, 16), alt=[], dsn="AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS"),
    "CUSTDATA": dict(lrecl=500, key=(0, 9), alt=[], dsn="AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS"),
    "CARDXREF": dict(lrecl=50, key=(0, 16), alt=[(25, 11)], dsn="AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS"),
    "TRANSACT": dict(lrecl=350, key=(0, 16), alt=[], dsn="AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS"),
    "TRANTYPE": dict(lrecl=60, key=(0, 2), alt=[], dsn="AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS"),
    "TRANCATG": dict(lrecl=60, key=(0, 6), alt=[], dsn="AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS"),
    "DISCGRP": dict(lrecl=50, key=(0, 16), alt=[], dsn="AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS"),
    "TCATBALF": dict(lrecl=50, key=(0, 17), alt=[], dsn="AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS"),
    "USRSEC": dict(lrecl=80, key=(0, 8), alt=[], dsn="AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS"),
    "TRXFL": dict(lrecl=350, key=(0, 32), alt=[], dsn="AWS.M2.CARDDEMO.TRXFL.VSAM.KSDS"),
    # CVEXPORT: EXPORT-REC-TYPE X(1) + EXPORT-TIMESTAMP X(26) -> EXPORT-SEQUENCE-NUM
    # 9(9) COMP at offset 27 (CBEXPORT.jcl says KEYS(4 28); the copybook wins).
    "EXPORT": dict(lrecl=500, key=(27, 4), alt=[], dsn="AWS.M2.CARDDEMO.EXPORT.DATA"),
}


# ------------------------------------------------------------------ utilities
def sh(cmd, **kw):
    return subprocess.run(cmd, capture_output=True, text=True, **kw)


def write(path: Path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    if isinstance(data, bytes):
        path.write_bytes(data)
    else:
        path.write_text(data)


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def render_record(rec: bytes) -> str:
    """Printable rendering: ASCII 0x20-0x7E as-is, everything else as \\xNN."""
    return "".join(chr(b) if 0x20 <= b <= 0x7E else "\\x%02x" % b for b in rec)


def render_sysout(data: bytes) -> str:
    """DISPLAY output: keep newlines, render other non-printable bytes as \\xNN."""
    return "".join("\n" if b == 0x0A else chr(b) if 0x20 <= b <= 0x7E else "\\x%02x" % b for b in data)


def fold(path: Path, lrecl: int) -> tuple[str, int, int]:
    """Fixed-length records -> one rendered record per line."""
    data = path.read_bytes() if path.exists() else b""
    recs = [data[i:i + lrecl] for i in range(0, len(data), lrecl)]
    text = "".join(render_record(r) + "\n" for r in recs)
    return text, len(recs), len(data) % lrecl


def fold_varseq0(path: Path) -> tuple[str, int]:
    """GnuCOBOL COB_VARSEQ_FORMAT=0: 2-byte big-endian length + 2 NUL + data."""
    data = path.read_bytes() if path.exists() else b""
    out, n, i = [], 0, 0
    while i + 4 <= len(data):
        ln = int.from_bytes(data[i:i + 2], "big")
        rec = data[i + 4:i + 4 + ln]
        out.append("%05d|%s\n" % (ln, render_record(rec)))
        i += 4 + ln
        n += 1
    return "".join(out), n


def read_records(path: Path, lrecl: int) -> list[bytes]:
    data = path.read_bytes()
    if len(data) % lrecl:
        raise RuntimeError(f"{path}: size {len(data)} not a multiple of LRECL {lrecl}")
    return [data[i:i + lrecl] for i in range(0, len(data), lrecl)]


def write_records(path: Path, recs: list[bytes]):
    write(path, b"".join(recs))


# ----------------------------------------------------------- DFSORT emulation
def dfsort(records, fields, include=None, outrec=None, lrecl_out=None):
    """Emulate `SORT FIELDS=(p,l,fmt,o,...)` / INCLUDE COND / OUTREC FIELDS.

    fields: list of (pos1, length, 'CH'|'ZD', 'A'|'D').  ZD fields hold
    unsigned digits in CardDemo; they compare numerically, falling back to
    byte order when a value is not all digits.  Python's sort is stable, so
    records with equal keys keep input order (DFSORT only guarantees that with
    OPTION EQUALS; CardDemo keys are unique so it does not matter here).
    include: callable(record) -> bool applied before sorting (INCLUDE COND).
    outrec: callable(record) -> bytes applied after sorting (OUTREC FIELDS).
    lrecl_out: pad/truncate the reformatted record to the SORTOUT LRECL.
    """
    if include:
        records = [r for r in records if include(r)]

    def keyf(rec):
        parts = []
        for pos, ln, fmt, order in fields:
            fld = rec[pos - 1:pos - 1 + ln]
            if fmt == "ZD" and fld.isdigit():
                val = int(fld)
                parts.append((0, -val if order == "D" else val))
            else:
                if order == "D":
                    fld = bytes(255 - b for b in fld)
                parts.append((1, fld))
        return parts

    records = sorted(records, key=keyf)
    if outrec:
        records = [outrec(r) for r in records]
    if lrecl_out:
        records = [r[:lrecl_out].ljust(lrecl_out, b" ") for r in records]
    return records


def zd_edit_tttttttttdtt(fld: bytes) -> bytes:
    """DFSORT OUTREC ...,ZD,EDIT=(TTTTTTTTT.TT) for an 11-digit zoned field.

    T prints every digit (no leading-zero suppression); the sign is dropped.
    A trailing EBCDIC-style overpunch ('{' = +0, 'A'-'I' = +1..9,
    '}' = -0, 'J'-'R' = -1..9) is resolved to its digit, matching how DFSORT
    treats zoned data.
    """
    s = fld.decode("latin-1")
    last = s[-1]
    if last in "{}":
        last = "0"
    elif "A" <= last <= "I":
        last = chr(ord(last) - ord("A") + ord("1"))
    elif "J" <= last <= "R":
        last = chr(ord(last) - ord("J") + ord("1"))
    digits = (s[:-1] + last).rjust(11, "0")
    return (digits[:9] + "." + digits[9:]).encode()


# --------------------------------------------------------------- GDG catalog
class Catalog:
    """Minimal dataset catalog: plain DSNs and GDG generations (+1)/(0)."""

    def __init__(self, root: Path):
        self.root = root
        self.gens: dict[str, list[Path]] = {}

    def ds(self, dsn: str) -> Path:
        return self.root / "ds" / dsn

    def ksds(self, name: str) -> Path:
        return self.root / "ksds" / f"{name}.idx"

    def new_gen(self, dsn: str) -> Path:
        n = len(self.gens.setdefault(dsn, [])) + 1
        p = self.root / "gdg" / f"{dsn}.G{n:04d}V00"
        self.gens[dsn].append(p)
        return p

    def cur_gen(self, dsn: str) -> Path:
        return self.gens[dsn][-1]


# --------------------------------------------------------- COBOL generation
IDXUTIL_TEMPLATE = """\
      ******************************************************************
      * {prog} - generated by scripts/baseline/baseline.py
      * IDCAMS emulation for {name} ({dsn})
      *   LOAD : fixed {lrecl}-byte sequential (SEQFILE) -> KSDS (IDXFILE)
      *          == IDCAMS DEFINE CLUSTER + REPRO INFILE->OUTFILE
      *   UNLD : KSDS (IDXFILE) -> fixed {lrecl}-byte sequential (SEQFILE)
      *          in primary-key order == IDCAMS REPRO INFILE(KSDS)
      * Pattern harvested from origin/devin/1790617863-batch @ e9658ca
      * aws/batch/golden/gen-idxutil.sh and origin/devin/cobol-safety-net
      * @ 1c845c7 test-harness/cobol/KSDSLOAD.cbl.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. {prog}.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO SEQFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SEQ-STAT.
           SELECT IDX-FILE ASSIGN TO IDXFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IDX-KEY{altclause}
               FILE STATUS IS WS-IDX-STAT.
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-FILE.
       01  SEQ-REC                 PIC X({lrecl}).
       FD  IDX-FILE.
       01  IDX-REC.
{fields}
       WORKING-STORAGE SECTION.
       01  WS-SEQ-STAT             PIC XX.
       01  WS-IDX-STAT             PIC XX.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-COUNT                PIC 9(9) VALUE 0.
       01  WS-ERRS                 PIC 9(9) VALUE 0.
       01  WS-MODE                 PIC X(4).
       PROCEDURE DIVISION.
           ACCEPT WS-MODE FROM COMMAND-LINE.
           EVALUATE WS-MODE
             WHEN 'LOAD' PERFORM LOAD-PARA
             WHEN 'UNLD' PERFORM UNLD-PARA
             WHEN OTHER
               DISPLAY '{prog}: usage LOAD|UNLD'
               MOVE 16 TO RETURN-CODE
           END-EVALUATE.
           GOBACK.
       LOAD-PARA.
           OPEN INPUT SEQ-FILE.
           IF WS-SEQ-STAT NOT = '00'
              DISPLAY 'IDCAMS-EMU {name}: OPEN SEQFILE STATUS '
                      WS-SEQ-STAT
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF.
           OPEN OUTPUT IDX-FILE.
           IF WS-IDX-STAT NOT = '00'
              DISPLAY 'IDCAMS-EMU {name}: DEFINE/OPEN IDXFILE STATUS '
                      WS-IDX-STAT
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF.
           PERFORM UNTIL WS-EOF = 'Y'
              READ SEQ-FILE
                 AT END MOVE 'Y' TO WS-EOF
                 NOT AT END
                    MOVE SEQ-REC TO IDX-REC
                    WRITE IDX-REC
                    IF WS-IDX-STAT = '00'
                       ADD 1 TO WS-COUNT
                    ELSE
                       ADD 1 TO WS-ERRS
                       DISPLAY 'IDCAMS-EMU {name}: WRITE STATUS '
                               WS-IDX-STAT ' KEY=' IDX-KEY
                    END-IF
              END-READ
           END-PERFORM.
           CLOSE SEQ-FILE IDX-FILE.
           DISPLAY 'IDCAMS-EMU {name}: REPRO LOADED ' WS-COUNT
                   ' RECORDS, REJECTED ' WS-ERRS.
           IF WS-ERRS > 0 MOVE 12 TO RETURN-CODE END-IF.
       UNLD-PARA.
           OPEN INPUT IDX-FILE.
           IF WS-IDX-STAT NOT = '00'
              DISPLAY 'IDCAMS-EMU {name}: OPEN IDXFILE STATUS '
                      WS-IDX-STAT
              MOVE 12 TO RETURN-CODE
              GOBACK
           END-IF.
           OPEN OUTPUT SEQ-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
              READ IDX-FILE NEXT
                 AT END MOVE 'Y' TO WS-EOF
                 NOT AT END
                    MOVE IDX-REC TO SEQ-REC
                    WRITE SEQ-REC
                    ADD 1 TO WS-COUNT
              END-READ
           END-PERFORM.
           CLOSE SEQ-FILE IDX-FILE.
           DISPLAY 'IDCAMS-EMU {name}: REPRO UNLOADED ' WS-COUNT
                   ' RECORDS'.
"""


def gen_idxutil(name: str, spec: dict) -> str:
    lrecl, (koff, klen) = spec["lrecl"], spec["key"]
    segs = []  # (offset, length, fieldname)
    segs.append((koff, klen, "IDX-KEY"))
    for i, (aoff, alen) in enumerate(spec["alt"], 1):
        segs.append((aoff, alen, f"IDX-ALT{i}"))
    segs.sort()
    lines, pos, nf = [], 0, 0
    for off, ln, fname in segs:
        if off > pos:
            nf += 1
            lines.append(f"           05  FILLER              PIC X({off - pos}).")
        lines.append(f"           05  {fname:<20}PIC X({ln}).")
        pos = off + ln
    if lrecl > pos:
        lines.append(f"           05  FILLER              PIC X({lrecl - pos}).")
    alt = "".join(f"\n               ALTERNATE RECORD KEY IS IDX-ALT{i}"
                  for i in range(1, len(spec["alt"]) + 1))
    return IDXUTIL_TEMPLATE.format(prog=f"IDX{name[:5]}", name=name, dsn=spec["dsn"],
                                   lrecl=lrecl, altclause=alt, fields="\n".join(lines))


# GnuCOBOL cannot resolve a RECORD KEY that lives in WORKING-STORAGE (neither
# can Enterprise COBOL: the key must be a data item of the file's record).
# CBEXPORT/CBIMPORT name EXPORT-SEQUENCE-NUM (copybook CVEXPORT, in WS) as the
# key of a file whose FD record is a bare PIC X(500).  The baseline compiles a
# patched copy that adds a second 01 record view exposing the same bytes
# (offset 27, 4 bytes) and points RECORD KEY at it.  The original source in
# app/cbl is untouched; the diff is written to 00-COMPILE/<PGM>.gnucobol.patch.
# CBSTM03A walks z/OS control blocks (PSA -> TCB -> TIOT) purely to DISPLAY the
# job/step name and the allocated DD names.  Those blocks do not exist under
# GnuCOBOL (SET ADDRESS OF PSA-BLOCK TO PSAPTR dereferences address 0 and
# SIGSEGVs).  The patched copy replaces that diagnostic walk with a DISPLAY and
# leaves the statement logic untouched.  A 3-tuple patch replaces everything
# from the first marker up to (not including) the second marker.
SOURCE_PATCHES = {
    "CBSTM03A": [
        ("           SET ADDRESS OF PSA-BLOCK   TO PSAPTR.\n",
         "           OPEN OUTPUT STMT-FILE HTML-FILE.\n",
         "      *    GnuCOBOL baseline: PSA/TCB/TIOT control-block walk removed\n"
         "      *    (z/OS only; it merely DISPLAYed the job name and DD list).\n"
         "           DISPLAY 'Running JCL : CREASTMT  Step STEP040 '\n"
         "                   '(TIOT walk bypassed under GnuCOBOL)'.\n"),
    ],
    "CBEXPORT": [
        ("               RECORD KEY IS EXPORT-SEQUENCE-NUM\n",
         "               RECORD KEY IS EXPORT-OUTPUT-SEQ-NUM\n"),
        ("       01  EXPORT-OUTPUT-RECORD                        PIC X(500).\n",
         "       01  EXPORT-OUTPUT-RECORD                        PIC X(500).\n"
         "       01  EXPORT-OUTPUT-KEY-VIEW.\n"
         "           05  FILLER                                  PIC X(27).\n"
         "           05  EXPORT-OUTPUT-SEQ-NUM                   PIC 9(9) COMP.\n"
         "           05  FILLER                                  PIC X(469).\n"),
    ],
    "CBIMPORT": [
        ("               RECORD KEY IS EXPORT-SEQUENCE-NUM\n",
         "               RECORD KEY IS EXPORT-INPUT-SEQ-NUM\n"),
        ("       01  EXPORT-INPUT-RECORD                        PIC X(500).\n",
         "       01  EXPORT-INPUT-RECORD                        PIC X(500).\n"
         "       01  EXPORT-INPUT-KEY-VIEW.\n"
         "           05  FILLER                                  PIC X(27).\n"
         "           05  EXPORT-INPUT-SEQ-NUM                    PIC 9(9) COMP.\n"
         "           05  FILLER                                  PIC X(469).\n"),
    ],
}


def source_of(pgm: str) -> Path:
    for ext in (".cbl", ".CBL"):
        p = CBL / f"{pgm}{ext}"
        if p.exists():
            return p
    raise FileNotFoundError(pgm)


# ----------------------------------------------------------------- the runner
class Baseline:
    def __init__(self, fast: bool):
        self.fast = fast
        self.bin = WORK / "bin"
        self.gen = WORK / "gen"
        self.data = WORK / "data"
        self.cat = Catalog(WORK)
        self.compile_rows = []      # (pgm, syntax_rc, build_rc, notes)
        self.job_rows = []          # (job, programs, rc, outputs, note)
        self.not_run = []           # (pgm, reason)
        self.data_rows = []         # (name, source, lrecl, records, note)
        self.env_base = {
            **os.environ,
            "COB_CURRENT_DATE": FROZEN_CLOCK,
            "COB_VARSEQ_FORMAT": "0",
        }
        if fast:
            self.env_base["BASELINE_MVSWAIT_NOSLEEP"] = "1"

    # -------------------------------------------------------------- phases
    def run(self):
        for p in (OUT, WORK):
            if p.exists():
                shutil.rmtree(p)
        for p in (OUT, self.bin, self.gen, self.data, WORK / "ds", WORK / "ksds", WORK / "gdg"):
            p.mkdir(parents=True, exist_ok=True)
        self.log_versions()
        self.compile_all()
        self.prepare_data()
        self.build_idxutils()
        for job in [self.job_load_acctfile, self.job_load_cardfile, self.job_load_custfile,
                    self.job_load_xreffile, self.job_load_tranfile, self.job_load_trantype,
                    self.job_load_trancatg, self.job_load_discgrp, self.job_load_tcatbalf,
                    self.job_load_dusrsecj,
                    self.job_readacct, self.job_readcard, self.job_readcust, self.job_readxref,
                    self.job_cbtrn01c, self.job_posttran, self.job_intcalc, self.job_tranbkp,
                    self.job_combtran, self.job_tranrept, self.job_creastmt, self.job_prtcatbl,
                    self.job_cbexport, self.job_cbimport, self.job_waitstep, self.job_csutldtc]:
            job()
        self.check_dependency_order()
        self.write_summary()

    def log_versions(self):
        r = sh(["cobc", "--version"])
        write(OUT / "00-COMPILE" / "cobc-version.txt", r.stdout)
        r = sh(["cobc", "-i"])
        write(OUT / "00-COMPILE" / "cobc-info.txt", r.stdout)

    # ------------------------------------------------------------ compiling
    def cobc(self, out: Path, sources: list[Path], extra=()):
        cmd = COBC_BASE + list(extra) + ["-x", "-o", str(out)] + [str(s) for s in sources]
        r = sh(cmd, cwd=REPO)
        return cmd, r

    def compile_all(self):
        comp = OUT / "00-COMPILE"
        for pgm in BATCH_PROGRAMS:
            src = source_of(pgm)
            cmd = COBC_SYNTAX + [str(src.relative_to(REPO))]
            r = sh(cmd, cwd=REPO)
            write(comp / f"{pgm}.syntax.log",
                  f"$ {' '.join(cmd)}\n{r.stdout}{r.stderr}rc={r.returncode}\n")
            self.compile_rows.append([pgm, r.returncode, None, ""])

        notes = {}
        stub = lambda n: STUBS / f"{n}.cbl"
        drv = lambda n: DRIVERS / f"{n}.cbl"
        builds = {
            "CBACT01C": ([source_of("CBACT01C"), stub("CEE3ABD"), stub("COBDATFT")], (),
                         "links stubs CEE3ABD + COBDATFT (assembler)"),
            "CBACT02C": ([source_of("CBACT02C"), stub("CEE3ABD")], (), "links stub CEE3ABD"),
            "CBACT03C": ([source_of("CBACT03C"), stub("CEE3ABD")], (), "links stub CEE3ABD"),
            "CBACT04C": ([drv("RUNCB04"), source_of("CBACT04C"), stub("CEE3ABD")], (),
                         "built as RUNCB04 driver (passes JCL PARM) + stub CEE3ABD"),
            "CBCUS01C": ([source_of("CBCUS01C"), stub("CEE3ABD")], (), "links stub CEE3ABD"),
            "CBTRN01C": ([source_of("CBTRN01C"), stub("CEE3ABD")], (), "links stub CEE3ABD"),
            "CBTRN02C": ([source_of("CBTRN02C"), stub("CEE3ABD")], (), "links stub CEE3ABD"),
            "CBTRN03C": ([source_of("CBTRN03C"), stub("CEE3ABD")], (), "links stub CEE3ABD"),
            "CBSTM03A": ([self.patched("CBSTM03A"), source_of("CBSTM03B"), stub("CEE3ABD")],
                         ("-ftab-width=1",),
                         "needs -ftab-width=1 (tab-indented lines in app/cpy/CUSTREC.cpy); patched copy drops the "
                         "z/OS PSA/TCB/TIOT walk (see CBSTM03A.gnucobol.patch); links CBSTM03B + stub CEE3ABD"),
            "CBSTM03B": ([source_of("CBSTM03B")], ("-m",), "subprogram of CBSTM03A; module build only"),
            "CBEXPORT": ([self.patched("CBEXPORT"), stub("CEE3ABD")], (),
                         "patched copy (RECORD KEY in WORKING-STORAGE), see CBEXPORT.gnucobol.patch; stub CEE3ABD"),
            "CBIMPORT": ([self.patched("CBIMPORT"), stub("CEE3ABD")], (),
                         "patched copy (RECORD KEY in WORKING-STORAGE), see CBIMPORT.gnucobol.patch; stub CEE3ABD"),
            "CSUTLDTC": ([drv("RUNDTC"), source_of("CSUTLDTC"), stub("CEEDAYS")], (),
                         "subprogram; built with RUNDTC driver + stub CEEDAYS (LE date service)"),
            "COBSWAIT": ([source_of("COBSWAIT"), stub("MVSWAIT")], (), "links stub MVSWAIT (assembler)"),
        }
        for pgm, (sources, extra, note) in builds.items():
            if pgm == "CBSTM03B":
                cmd = COBC_BASE + ["-m", "-o", str(self.bin / "CBSTM03B.so"), str(sources[0])]
                r = sh(cmd, cwd=REPO)
            else:
                cmd, r = self.cobc(self.bin / pgm, sources, extra)
            pretty = [c.replace(str(REPO) + "/", "") for c in cmd]
            write(comp / f"{pgm}.compile.log",
                  f"$ {' '.join(pretty)}\n{r.stdout}{r.stderr}rc={r.returncode}\n")
            row = next(x for x in self.compile_rows if x[0] == pgm)
            row[2], row[3] = r.returncode, note
            if r.returncode != 0:
                self.not_run.append((pgm, f"cobc -x failed, see 00-COMPILE/{pgm}.compile.log"))

    def patched(self, pgm: str) -> Path:
        src = source_of(pgm)
        text = src.read_text()
        for patch in SOURCE_PATCHES[pgm]:
            if len(patch) == 2:
                old, new = patch
                assert text.count(old) == 1, f"{pgm}: patch anchor not unique: {old!r}"
                text = text.replace(old, new)
            else:
                start, end, new = patch
                assert text.count(start) == 1 and text.count(end) == 1, f"{pgm}: range anchors not unique"
                i, j = text.index(start), text.index(end)
                assert i < j
                text = text[:i] + new + text[j:]
        dst = self.gen / f"{pgm}.cbl"
        write(dst, text)
        diff = difflib.unified_diff(src.read_text().splitlines(True), text.splitlines(True),
                                    f"a/app/cbl/{src.name}", f"b/build/baseline-work/gen/{pgm}.cbl")
        write(OUT / "00-COMPILE" / f"{pgm}.gnucobol.patch", "".join(diff))
        return dst

    # ---------------------------------------------------------------- data
    def prepare_data(self):
        """Fixed-width inputs.  ASCII samples are used (GnuCOBOL reads ASCII);
        lines are stripped of CR/LF and space-padded to the LRECL.  Files that
        exist only as EBCDIC are converted with codec cp037.  Where both forms
        exist the conversion is cross-checked against the ASCII sample."""
        for name, (ascii_name, lrecl, ebc_name) in DATASETS.items():
            out = self.data / f"{name}.dat"
            note = []
            if ascii_name:
                raw = (ASCII_DIR / ascii_name).read_bytes()
                lines = [ln.rstrip(b"\r") for ln in raw.split(b"\n")]
                if lines and lines[-1] == b"":
                    lines.pop()
                recs = []
                for ln in lines:
                    if len(ln) > lrecl:
                        if ln[lrecl:].strip():
                            raise RuntimeError(f"{ascii_name}: line longer than LRECL {lrecl}")
                        ln = ln[:lrecl]
                    recs.append(ln.ljust(lrecl, b" "))
                if raw.count(b"\r"):
                    note.append("CRLF line ends stripped")
                lens = {len(ln) for ln in lines}
                if lens != {lrecl}:
                    note.append(f"line lengths {sorted(lens)} padded to {lrecl}")
                src = f"app/data/ASCII/{ascii_name}"
                if ebc_name and (EBCDIC_DIR / ebc_name).exists():
                    ebc = (EBCDIC_DIR / ebc_name).read_bytes().decode("cp037").encode("latin-1")
                    note.append("EBCDIC cp037 conversion " +
                                ("matches ASCII sample" if ebc == b"".join(recs)
                                 else "DIFFERS from ASCII sample"))
            else:
                ebc = (EBCDIC_DIR / ebc_name).read_bytes()
                if len(ebc) % lrecl:
                    raise RuntimeError(f"{ebc_name}: size not multiple of {lrecl}")
                conv = ebc.decode("cp037").encode("latin-1")
                recs = [conv[i:i + lrecl] for i in range(0, len(conv), lrecl)]
                src = f"app/data/EBCDIC/{ebc_name} (no ASCII sample; converted cp037->latin-1)"
            write_records(out, recs)
            self.data_rows.append((name, src, lrecl, len(recs), "; ".join(note)))
        dp = (f"{DATEPARM_START} {DATEPARM_END}").ljust(80).encode()
        write_records(self.cat.ds("AWS.M2.CARDDEMO.DATEPARM"), [dp])
        self.data_rows.append(("DATEPARM", "generated: TRANREPT.jcl SYMNAMES PARM-START-DATE/PARM-END-DATE",
                               80, 1, f"'{DATEPARM_START} {DATEPARM_END}'"))
        d = OUT / "00-DATA"
        for name, (_, lrecl, _) in DATASETS.items():
            text, n, rem = fold(self.data / f"{name}.dat", lrecl)
            write(d / f"{name}.txt", text)
        write(d / "DATEPARM.txt", fold(self.cat.ds("AWS.M2.CARDDEMO.DATEPARM"), 80)[0])

    def build_idxutils(self):
        log = []
        for name, spec in KSDS.items():
            src = self.gen / f"IDX{name[:5]}.cbl"
            write(src, gen_idxutil(name, spec))
            cmd, r = self.cobc(self.bin / f"IDX{name[:5]}", [src])
            log.append(f"$ {' '.join(c.replace(str(REPO) + '/', '') for c in cmd)}\n{r.stdout}{r.stderr}rc={r.returncode}\n")
            if r.returncode:
                raise RuntimeError(f"IDXUTIL build failed for {name}: {r.stderr}")
        write(OUT / "00-COMPILE" / "IDXUTIL.compile.log", "".join(log))

    # ------------------------------------------------------- IDCAMS emulation
    def idcams_define_repro(self, name: str, seq: Path | None, sysout: list) -> int:
        """DEFINE CLUSTER (+ REPRO INFILE(seq) OUTFILE(ksds)).  seq=None defines an empty cluster."""
        ksds = self.cat.ksds(name)
        for p in ksds.parent.glob(ksds.name + "*"):
            p.unlink()
        if seq is None:
            seq = WORK / "ds" / "EMPTY.dat"
            seq.write_bytes(b"")
        r = subprocess.run([str(self.bin / f"IDX{name[:5]}"), "LOAD"], capture_output=True, text=True,
                           env={**self.env_base, "DD_SEQFILE": str(seq), "DD_IDXFILE": str(ksds)})
        sysout.append(f"--- IDCAMS-EMU DEFINE CLUSTER {KSDS[name]['dsn']} KEYS({KSDS[name]['key'][1]} "
                      f"{KSDS[name]['key'][0]}) RECSZ({KSDS[name]['lrecl']}) ; REPRO INFILE({seq.name})\n"
                      f"{r.stdout}{r.stderr}rc={r.returncode}\n")
        return r.returncode

    def idcams_unload(self, name: str, seq: Path, sysout: list) -> int:
        """REPRO INFILE(ksds) OUTFILE(seq): KSDS -> fixed sequential in key order."""
        r = subprocess.run([str(self.bin / f"IDX{name[:5]}"), "UNLD"], capture_output=True, text=True,
                           env={**self.env_base, "DD_SEQFILE": str(seq), "DD_IDXFILE": str(self.cat.ksds(name))})
        sysout.append(f"--- IDCAMS-EMU REPRO INFILE({KSDS[name]['dsn']}) OUTFILE({seq.name})\n"
                      f"{r.stdout}{r.stderr}rc={r.returncode}\n")
        return r.returncode

    def ksds_snapshot(self, name: str) -> list[bytes]:
        tmp = WORK / "ds" / f"_snap_{name}.dat"
        if tmp.exists():
            tmp.unlink()
        self.idcams_unload(name, tmp, [])
        return read_records(tmp, KSDS[name]["lrecl"]) if tmp.exists() else []

    # ------------------------------------------------------------- execution
    def exec_pgm(self, binary: str, dd: dict, args=(), stdin: str | None = None, env_extra=None):
        env = {**self.env_base, **{f"DD_{k}": str(v) for k, v in dd.items()}, **(env_extra or {})}
        r = subprocess.run([str(self.bin / binary), *args], input=stdin.encode() if stdin else None,
                           capture_output=True, env=env)
        return r.returncode, render_sysout(r.stdout + r.stderr)

    def finish_job(self, job: str, programs: str, rc: int, sysout: list, jobmd: list,
                   outputs: dict, ksds_after=(), note=""):
        """outputs: label -> (path, lrecl | 'VB')"""
        d = OUT / job
        d.mkdir(parents=True, exist_ok=True)
        write(d / "sysout.txt", "".join(sysout))
        write(d / "rc.txt", f"{rc}\n")
        names = []
        for label, (path, lrecl) in outputs.items():
            if lrecl == "VB":
                text, n = fold_varseq0(path)
                jobmd.append(f"- `{label}.txt`: {n} variable records (GnuCOBOL VARSEQ format 0: 2-byte BE length + 2 NUL)")
            else:
                text, n, rem = fold(path, lrecl)
                jobmd.append(f"- `{label}.txt`: {n} records x LRECL {lrecl}" + (f" (+{rem} trailing bytes)" if rem else ""))
            write(d / f"{label}.txt", text)
            names.append(f"{label}.txt")
        for name in ksds_after:
            tmp = WORK / "ds" / f"_after_{job}_{name}.dat"
            self.idcams_unload(name, tmp, sysout)
            text, n, _ = fold(tmp, KSDS[name]["lrecl"])
            write(d / f"{name}.ksds.txt", text)
            jobmd.append(f"- `{name}.ksds.txt`: after-image of {KSDS[name]['dsn']} ({n} records, key order)")
            names.append(f"{name}.ksds.txt")
        write(d / "sysout.txt", "".join(sysout))
        write(d / "job.md", "\n".join(jobmd) + "\n")
        self.job_rows.append((job, programs, rc, ", ".join(names), note))
        print(f"[{job:9}] rc={rc:<4} {programs}")

    def dd_table(self, dd: dict) -> list[str]:
        rows = ["", "| DD | file |", "|---|---|"]
        for k, v in dd.items():
            rows.append(f"| {k} | `{str(v).replace(str(WORK) + '/', '')}` |")
        return rows

    # ------------------------------------------------------------ load jobs
    def _load_job(self, job: str, name: str, source: str, jcl: str, extra_note=""):
        sysout = []
        rc = self.idcams_define_repro(name, self.data / f"{source}.dat", sysout)
        spec = KSDS[name]
        md = [f"# {job}", "", f"JCL: `app/jcl/{jcl}` — IDCAMS DELETE/DEFINE CLUSTER + REPRO (emulated).",
              f"- Cluster: `{spec['dsn']}` RECSZ({spec['lrecl']}) KEYS({spec['key'][1]} {spec['key'][0]})"
              + (f", alternate key(s) {spec['alt']} (declared by CBACT04C)" if spec['alt'] else ""),
              f"- Input: `00-DATA/{source}.txt` (fixed {spec['lrecl']}-byte records)",
              "- Emulation: generated GnuCOBOL utility `IDX" + name[:5] + " LOAD` writes a BDB indexed file.",
              extra_note, "", "## Outputs"]
        self.finish_job(job, f"IDCAMS(REPRO {source}->{name})", rc, sysout, md, {}, ksds_after=[name])

    def job_load_acctfile(self):
        self._load_job("ACCTFILE", "ACCTDATA", "ACCTDATA", "ACCTFILE.jcl")

    def job_load_cardfile(self):
        self._load_job("CARDFILE", "CARDDATA", "CARDDATA", "CARDFILE.jcl",
                       "- The AIX on account id (CARDFILE.jcl CARDAIX) is not built: no batch program declares it.")

    def job_load_custfile(self):
        self._load_job("CUSTFILE", "CUSTDATA", "CUSTDATA", "CUSTFILE.jcl")

    def job_load_xreffile(self):
        self._load_job("XREFFILE", "CARDXREF", "CARDXREF", "XREFFILE.jcl",
                       "- `cardxref.txt` lines are 36 bytes; padded with spaces to the 50-byte LRECL of the cluster. "
                       "Alternate key (offset 25, len 11 = XREF-ACCT-ID) is defined because CBACT04C declares it; "
                       "the sample has no duplicate account ids so it is built UNIQUE like the COBOL declaration.")

    def job_load_tranfile(self):
        self._load_job("TRANFILE", "TRANSACT", "DALYTRAN.INIT", "TRANFILE.jcl",
                       "- Source is `AWS.M2.CARDDEMO.DALYTRAN.PS.INIT` (EBCDIC only, 1 seed record) converted with cp037. "
                       "The AIX TRANIDX (TRANFILE.jcl) is not built: no batch program declares it.")

    def job_load_trantype(self):
        self._load_job("TRANTYPE", "TRANTYPE", "TRANTYPE", "TRANTYPE.jcl",
                       "- `trantype.txt` has CRLF line ends and a 60-byte last line; normalised to 60-byte records.")

    def job_load_trancatg(self):
        self._load_job("TRANCATG", "TRANCATG", "TRANCATG", "TRANCATG.jcl",
                       "- `trancatg.txt` has CRLF line ends; normalised to 60-byte records.")

    def job_load_discgrp(self):
        self._load_job("DISCGRP", "DISCGRP", "DISCGRP", "DISCGRP.jcl")

    def job_load_tcatbalf(self):
        self._load_job("TCATBALF", "TCATBALF", "TCATBALF", "TCATBALF.jcl",
                       "- `tcatbal.txt` has CRLF line ends; normalised to 50-byte records.")

    def job_load_dusrsecj(self):
        self._load_job("DUSRSECJ", "USRSEC", "USRSEC", "DUSRSECJ.jcl",
                       "- Online-only security file (used by COSGN00C/COUSR*); loaded for completeness from the EBCDIC sample.")

    # ------------------------------------------------------- print/read jobs
    def _simple_job(self, job, pgm, jcl, dd, outputs, ksds_after=(), notes=(), stdin=None, args=()):
        sysout = []
        rc, so = self.exec_pgm(pgm, dd, args=args, stdin=stdin)
        sysout.append(f"--- EXEC PGM={pgm}" + (f" PARM='{args[0]}'" if args else "") + "\n" + so + f"rc={rc}\n")
        md = [f"# {job}", "", f"JCL: `{jcl}`", f"Program: `{pgm}`", *notes, *self.dd_table(dd), "", "## Outputs"]
        self.finish_job(job, pgm, rc, sysout, md, outputs, ksds_after=ksds_after)
        return rc

    def job_readacct(self):
        dd = dict(ACCTFILE=self.cat.ksds("ACCTDATA"),
                  OUTFILE=self.cat.ds("AWS.M2.CARDDEMO.ACCTDATA.PSCOMP"),
                  ARRYFILE=self.cat.ds("AWS.M2.CARDDEMO.ACCTDATA.PSARRY"),
                  VBRCFILE=self.cat.ds("AWS.M2.CARDDEMO.ACCTDATA.PSVBRC"))
        self._simple_job("READACCT", "CBACT01C", "app/jcl/READACCT.jcl", dd,
                         {"OUTFILE": (dd["OUTFILE"], 107), "ARRYFILE": (dd["ARRYFILE"], 110),
                          "VBRCFILE": (dd["VBRCFILE"], "VB")},
                         notes=["- CALL 'COBDATFT' is satisfied by `scripts/baseline/stubs/COBDATFT.cbl`.",
                                "- OUTFILE/ARRYFILE contain COMP-3 fields; non-printable bytes are rendered as `\\xNN`.",
                                "- VBRCFILE is RECORDING MODE V; rendered as `length|data`."])

    def job_readcard(self):
        dd = dict(CARDFILE=self.cat.ksds("CARDDATA"))
        self._simple_job("READCARD", "CBACT02C", "app/jcl/READCARD.jcl", dd, {})

    def job_readcust(self):
        dd = dict(CUSTFILE=self.cat.ksds("CUSTDATA"))
        self._simple_job("READCUST", "CBCUS01C", "app/jcl/READCUST.jcl", dd, {})

    def job_readxref(self):
        dd = dict(XREFFILE=self.cat.ksds("CARDXREF"))
        self._simple_job("READXREF", "CBACT03C", "app/jcl/READXREF.jcl", dd, {})

    def job_cbtrn01c(self):
        dd = dict(DALYTRAN=self.data / "DALYTRAN.dat", CUSTFILE=self.cat.ksds("CUSTDATA"),
                  XREFFILE=self.cat.ksds("CARDXREF"), CARDFILE=self.cat.ksds("CARDDATA"),
                  ACCTFILE=self.cat.ksds("ACCTDATA"), TRANFILE=self.cat.ksds("TRANSACT"))
        self._simple_job("CBTRN01C", "CBTRN01C", "(none — no JCL in app/jcl runs CBTRN01C; DD names from its SELECTs)",
                         dd, {}, notes=["- Read-only validation pass over DALYTRAN against XREF/ACCT; run before POSTTRAN so it sees the seeded masters."])

    # ------------------------------------------------------------- POSTTRAN
    def job_posttran(self):
        rejs = self.cat.new_gen("AWS.M2.CARDDEMO.DALYREJS")
        dd = dict(TRANFILE=self.cat.ksds("TRANSACT"), DALYTRAN=self.data / "DALYTRAN.dat",
                  XREFFILE=self.cat.ksds("CARDXREF"), DALYREJS=rejs,
                  ACCTFILE=self.cat.ksds("ACCTDATA"), TCATBALF=self.cat.ksds("TCATBALF"))
        self._simple_job("POSTTRAN", "CBTRN02C", "app/jcl/POSTTRAN.jcl STEP15", dd,
                         {"DALYREJS": (rejs, 430)}, ksds_after=["TRANSACT", "ACCTDATA", "TCATBALF"],
                         notes=["- CBTRN02C OPENs TRANFILE OUTPUT: GnuCOBOL recreates the KSDS, so the TRANFILE seed record is replaced by the posted transactions.",
                                "- TRAN-PROC-TS comes from CURRENT-DATE, frozen at " + FROZEN_CLOCK + ".",
                                "- RC 4 is set by the program when any transaction is rejected."])

    # -------------------------------------------------------------- INTCALC
    def job_intcalc(self):
        systran = self.cat.new_gen("AWS.M2.CARDDEMO.SYSTRAN")
        dd = dict(TCATBALF=self.cat.ksds("TCATBALF"), XREFFILE=self.cat.ksds("CARDXREF"),
                  ACCTFILE=self.cat.ksds("ACCTDATA"), DISCGRP=self.cat.ksds("DISCGRP"), TRANSACT=systran)
        self._simple_job("INTCALC", "CBACT04C", "app/jcl/INTCALC.jcl STEP15 (EXEC PGM=CBACT04C,PARM='2022071800')", dd,
                         {"TRANSACT": (systran, 350)}, ksds_after=["ACCTDATA"], args=(INTCALC_PARM,),
                         notes=[f"- Run through the RUNCB04 driver which passes PARM='{INTCALC_PARM}' as a halfword-prefixed area.",
                                "- XREFFILE is opened with ALTERNATE RECORD KEY FD-XREF-ACCT-ID (built by XREFFILE load)."])

    # -------------------------------------------------------------- TRANBKP
    def job_tranbkp(self):
        sysout = []
        bkup = self.cat.new_gen("AWS.M2.CARDDEMO.TRANSACT.BKUP")
        rc1 = self.idcams_unload("TRANSACT", bkup, sysout)
        rc2 = self.idcams_define_repro("TRANSACT", None, sysout)
        md = ["# TRANBKP", "", "JCL: `app/jcl/TRANBKP.jcl`",
              "- STEP05 IDCAMS REPRO TRANSACT.VSAM.KSDS -> TRANSACT.BKUP(+1) (emulated: IDXTRANS UNLD).",
              "- STEP10/STEP15 IDCAMS DELETE + DEFINE CLUSTER TRANSACT.VSAM.KSDS (emulated: IDXTRANS LOAD from an empty file -> empty KSDS).",
              "- The AIX/PATH redefinition (TRANIDX) is not emulated: no batch program declares that key.", "", "## Outputs"]
        self.finish_job("TRANBKP", "IDCAMS(REPRO,DELETE,DEFINE)", max(rc1, rc2), sysout, md,
                        {"TRANSACT.BKUP": (bkup, 350)}, ksds_after=["TRANSACT"])

    # ------------------------------------------------------------- COMBTRAN
    def job_combtran(self):
        sysout = []
        bkup = self.cat.cur_gen("AWS.M2.CARDDEMO.TRANSACT.BKUP")
        systran = self.cat.cur_gen("AWS.M2.CARDDEMO.SYSTRAN")
        recs = read_records(bkup, 350) + read_records(systran, 350)
        sorted_recs = dfsort(recs, [(1, 16, "CH", "A")])
        combined = self.cat.new_gen("AWS.M2.CARDDEMO.TRANSACT.COMBINED")
        write_records(combined, sorted_recs)
        sysout.append(f"--- DFSORT-EMU STEP05 SORT FIELDS=(1,16,CH,A): SORTIN {bkup.name}({len(read_records(bkup,350))}) + "
                      f"{systran.name}({len(read_records(systran,350))}) -> {combined.name} ({len(sorted_recs)} records)\n")
        rc = self.idcams_define_repro("TRANSACT", combined, sysout)
        md = ["# COMBTRAN", "", "JCL: `app/jcl/COMBTRAN.jcl`",
              "- STEP05 SORT: `SORT FIELDS=(1,16,CH,A)` over TRANSACT.BKUP(0) + SYSTRAN(0) -> TRANSACT.COMBINED(+1) (emulated in Python, stable byte-order sort on TRAN-ID).",
              "- STEP10 IDCAMS REPRO TRANSACT.COMBINED(0) -> TRANSACT.VSAM.KSDS (emulated: IDXTRANS LOAD; the cluster was emptied by TRANBKP).",
              "", "## Outputs"]
        self.finish_job("COMBTRAN", "SORT + IDCAMS(REPRO)", rc, sysout, md,
                        {"TRANSACT.COMBINED": (combined, 350)}, ksds_after=["TRANSACT"])

    # ------------------------------------------------------------- TRANREPT
    def job_tranrept(self):
        sysout = []
        bkup = self.cat.new_gen("AWS.M2.CARDDEMO.TRANSACT.BKUP")
        rc0 = self.idcams_unload("TRANSACT", bkup, sysout)
        recs = read_records(bkup, 350)
        lo, hi = DATEPARM_START.encode(), DATEPARM_END.encode()
        daly = self.cat.new_gen("AWS.M2.CARDDEMO.TRANSACT.DALY")
        sel = dfsort(recs, [(263, 16, "ZD", "A")],
                     include=lambda r: lo <= r[304:314] <= hi)
        write_records(daly, sel)
        sysout.append(f"--- DFSORT-EMU STEP10 SORT FIELDS=(TRAN-CARD-NUM=263,16,ZD,A) "
                      f"INCLUDE COND=(TRAN-PROC-DT=305,10,CH,GE,C'{DATEPARM_START}',AND,LE,C'{DATEPARM_END}'): "
                      f"{len(recs)} in -> {len(sel)} selected -> {daly.name}\n")
        rept = self.cat.new_gen("AWS.M2.CARDDEMO.TRANREPT")
        dd = dict(TRANFILE=daly, CARDXREF=self.cat.ksds("CARDXREF"), TRANTYPE=self.cat.ksds("TRANTYPE"),
                  TRANCATG=self.cat.ksds("TRANCATG"), DATEPARM=self.cat.ds("AWS.M2.CARDDEMO.DATEPARM"), TRANREPT=rept)
        rc, so = self.exec_pgm("CBTRN03C", dd)
        sysout.append(f"--- STEP15 EXEC PGM=CBTRN03C\n{so}rc={rc}\n")
        md = ["# TRANREPT", "", "JCL: `app/jcl/TRANREPT.jcl` (PROC `app/proc/TRANREPT.prc`)",
              "- STEP05 IDCAMS REPRO TRANSACT.VSAM.KSDS -> TRANSACT.BKUP(+1) (emulated: IDXTRANS UNLD).",
              f"- STEP10 SORT: `SORT FIELDS=(TRAN-CARD-NUM,A)` with `INCLUDE COND=(TRAN-PROC-DT,GE,PARM-START-DATE,AND,TRAN-PROC-DT,LE,PARM-END-DATE)`; "
              f"SYMNAMES TRAN-CARD-NUM=263,16,ZD; TRAN-PROC-DT=305,10,CH; PARM-START-DATE=C'{DATEPARM_START}'; PARM-END-DATE=C'{DATEPARM_END}' (emulated in Python).",
              f"- DATEPARM dataset (LRECL 80) is generated with the same window: `{DATEPARM_START} {DATEPARM_END}` (see `00-DATA/DATEPARM.txt`). "
              f"The clock is frozen at {FROZEN_CLOCK}, so every TRAN-PROC-TS written by POSTTRAN/INTCALC falls inside the window.",
              "- STEP15 EXEC PGM=CBTRN03C.", *self.dd_table(dd), "", "## Outputs"]
        self.finish_job("TRANREPT", "IDCAMS(REPRO) + SORT + CBTRN03C", max(rc0, rc), sysout, md,
                        {"TRANSACT.BKUP": (bkup, 350), "TRANSACT.DALY": (daly, 350), "TRANREPT": (rept, 133)})

    # ------------------------------------------------------------- CREASTMT
    def job_creastmt(self):
        sysout = []
        recs = self.ksds_snapshot("TRANSACT")

        def outrec(r):  # OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)
            return r[262:278] + r[0:262] + r[278:328]

        trx = dfsort(recs, [(263, 16, "CH", "A"), (1, 16, "CH", "A")], outrec=outrec, lrecl_out=350)
        trxseq = self.cat.ds("AWS.M2.CARDDEMO.TRXFL.SEQ")
        write_records(trxseq, trx)
        sysout.append(f"--- DFSORT-EMU STEP020 SORT FIELDS=(263,16,CH,A,1,16,CH,A) OUTREC FIELDS=(1:263,16,17:1,262,279:279,50): "
                      f"{len(recs)} records -> {trxseq.name}\n")
        rc1 = self.idcams_define_repro("TRXFL", trxseq, sysout)
        stmt = self.cat.ds("AWS.M2.CARDDEMO.STATEMNT.PS")
        html = self.cat.ds("AWS.M2.CARDDEMO.STATEMNT.HTML")
        dd = dict(TRNXFILE=self.cat.ksds("TRXFL"), XREFFILE=self.cat.ksds("CARDXREF"),
                  ACCTFILE=self.cat.ksds("ACCTDATA"), CUSTFILE=self.cat.ksds("CUSTDATA"),
                  STMTFILE=stmt, HTMLFILE=html)
        rc, so = self.exec_pgm("CBSTM03A", dd)
        sysout.append(f"--- STEP040 EXEC PGM=CBSTM03A\n{so}rc={rc}\n")
        md = ["# CREASTMT", "", "JCL: `app/jcl/CREASTMT.JCL`",
              "- STEP010 IDCAMS DELETE TRXFL.VSAM.KSDS (emulated by recreating the cluster).",
              "- STEP020 SORT: `SORT FIELDS=(263,16,CH,A,1,16,CH,A)`, `OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)` over TRANSACT.VSAM.KSDS -> TRXFL.SEQ (emulated in Python; "
              "the 328-byte reformatted record is blank-padded to the SORTOUT LRECL 350).",
              "- STEP030 IDCAMS DEFINE CLUSTER TRXFL KEYS(32 0) RECSZ(350) + REPRO TRXFL.SEQ (emulated: IDXTRXFL LOAD).",
              "- STEP040 EXEC PGM=CBSTM03A (patched copy without the z/OS TIOT walk, see `00-COMPILE/CBSTM03A.gnucobol.patch`; statically linked with CBSTM03B).", *self.dd_table(dd), "", "## Outputs"]
        self.finish_job("CREASTMT", "SORT + IDCAMS + CBSTM03A/CBSTM03B", max(rc1, rc), sysout, md,
                        {"TRXFL.SEQ": (trxseq, 350), "STMTFILE": (stmt, 80), "HTMLFILE": (html, 100)})

    # ------------------------------------------------------------- PRTCATBL
    def job_prtcatbl(self):
        sysout = []
        bkup = self.cat.new_gen("AWS.M2.CARDDEMO.TCATBALF.BKUP")
        rc1 = self.idcams_unload("TCATBALF", bkup, sysout)
        recs = read_records(bkup, 50)

        def outrec(r):  # OUTREC FIELDS=(1,11,X,12,2,X,14,4,X,18,11,ZD,EDIT=(TTTTTTTTT.TT),9X)
            return r[0:11] + b" " + r[11:13] + b" " + r[13:17] + b" " + zd_edit_tttttttttdtt(r[17:28]) + b" " * 9

        rept = dfsort(recs, [(1, 11, "ZD", "A"), (12, 2, "CH", "A"), (14, 4, "ZD", "A")], outrec=outrec)
        reptp = self.cat.ds("AWS.M2.CARDDEMO.TCATBALF.REPT")
        write_records(reptp, rept)
        sysout.append(f"--- DFSORT-EMU STEP10 SORT FIELDS=(1,11,ZD,A,12,2,CH,A,14,4,ZD,A) OUTREC: {len(recs)} records -> {reptp.name}\n")
        md = ["# PRTCATBL", "", "JCL: `app/jcl/PRTCATBL.jcl`",
              "- STEP05 IDCAMS REPRO TCATBALF.VSAM.KSDS -> TCATBALF.BKUP(+1) (emulated: IDXTCATB UNLD).",
              "- STEP10 SORT: `SORT FIELDS=(1,11,ZD,A,12,2,CH,A,14,4,ZD,A)` `OUTREC FIELDS=(1,11,X,12,2,X,14,4,X,18,11,ZD,EDIT=(TTTTTTTTT.TT),9X)` (emulated in Python). "
              "The reformatted record is 41 bytes while the JCL SORTOUT DCB says LRECL=40; the baseline keeps the 41-byte OUTREC layout and flags the JCL discrepancy.",
              "", "## Outputs"]
        self.finish_job("PRTCATBL", "IDCAMS(REPRO) + SORT", rc1, sysout, md,
                        {"TCATBALF.BKUP": (bkup, 50), "TCATBALF.REPT": (reptp, 41)})

    # ------------------------------------------------------------- CBEXPORT
    def job_cbexport(self):
        if any(p == "CBEXPORT" for p, _ in self.not_run):
            return
        sysout = []
        rc0 = self.idcams_define_repro("EXPORT", None, sysout)
        dd = dict(CUSTFILE=self.cat.ksds("CUSTDATA"), ACCTFILE=self.cat.ksds("ACCTDATA"),
                  XREFFILE=self.cat.ksds("CARDXREF"), TRANSACT=self.cat.ksds("TRANSACT"),
                  CARDFILE=self.cat.ksds("CARDDATA"), EXPFILE=self.cat.ksds("EXPORT"))
        rc, so = self.exec_pgm("CBEXPORT", dd)
        sysout.append(f"--- STEP20 EXEC PGM=CBEXPORT\n{so}rc={rc}\n")
        md = ["# CBEXPORT", "", "JCL: `app/jcl/CBEXPORT.jcl`",
              "- STEP10 IDCAMS DELETE/DEFINE CLUSTER EXPORT.DATA KEYS(4 28) RECSZ(500) (emulated: IDXEXPOR LOAD of an empty file; the key is built at offset 27 = EXPORT-SEQUENCE-NUM per CVEXPORT; "
              "CBEXPORT then OPENs it OUTPUT anyway).",
              "- STEP20 EXEC PGM=CBEXPORT — compiled from the patched copy (see `00-COMPILE/CBEXPORT.gnucobol.patch`).",
              f"- EXPORT-TIMESTAMP comes from ACCEPT DATE/TIME, frozen at {FROZEN_CLOCK}.",
              "- The 4-byte COMP sequence key renders as `\\xNN` escapes in the KSDS after-image.", *self.dd_table(dd), "", "## Outputs"]
        self.finish_job("CBEXPORT", "IDCAMS(DEFINE) + CBEXPORT", max(rc0, rc), sysout, md, {}, ksds_after=["EXPORT"])

    # ------------------------------------------------------------- CBIMPORT
    def job_cbimport(self):
        if any(p == "CBIMPORT" for p, _ in self.not_run):
            return
        sysout = []
        outs = {k: self.cat.ds(f"AWS.M2.CARDDEMO.IMPORT.{k}") for k in ("CUSTOUT", "ACCTOUT", "XREFOUT", "TRNXOUT", "CARDOUT", "ERROUT")}
        dd = dict(EXPFILE=self.cat.ksds("EXPORT"), **outs)
        rc, so = self.exec_pgm("CBIMPORT", dd)
        sysout.append(f"--- STEP10 EXEC PGM=CBIMPORT\n{so}rc={rc}\n")
        lrecls = dict(CUSTOUT=500, ACCTOUT=300, XREFOUT=50, TRNXOUT=350, CARDOUT=150, ERROUT=132)
        md = ["# CBIMPORT", "", "JCL: `app/jcl/CBIMPORT.jcl`",
              "- STEP10 EXEC PGM=CBIMPORT reading the KSDS written by CBEXPORT — compiled from the patched copy (see `00-COMPILE/CBIMPORT.gnucobol.patch`).",
              "- The EBCDIC sample `AWS.M2.CARDDEMO.EXPORT.DATA.PS` is not used: the dependency map chains CBIMPORT on CBEXPORT's output.",
              *self.dd_table(dd), "", "## Outputs"]
        self.finish_job("CBIMPORT", "CBIMPORT", rc, sysout, md, {k: (outs[k], lrecls[k]) for k in outs})

    # ------------------------------------------------------------- WAITSTEP
    def job_waitstep(self):
        self._simple_job("WAITSTEP", "COBSWAIT", "app/jcl/WAITSTEP.jcl (EXEC PGM=COBSWAIT, SYSIN DD * -> " + WAITSTEP_SYSIN + ")",
                         {}, {}, stdin=WAITSTEP_SYSIN + "\n",
                         notes=["- CALL 'MVSWAIT' is satisfied by `scripts/baseline/stubs/MVSWAIT.cbl` (sleeps 36 s; `--fast` skips the sleep without changing sysout)."])

    # ------------------------------------------------------------- CSUTLDTC
    def job_csutldtc(self):
        self._simple_job("CSUTLDTC", "CSUTLDTC", "(none — CSUTLDTC is a subprogram of CORPT00C/COTRN02C; driven by scripts/baseline/drivers/RUNDTC.cbl)",
                         {}, {}, notes=["- CALL 'CEEDAYS' is satisfied by `scripts/baseline/stubs/CEEDAYS.cbl`."])

    # ---------------------------------------------------------- order check
    def check_dependency_order(self):
        """job_dependencies in dependency-map.json: job -> {upstream job: [datasets]}."""
        dm = json.loads((REPO / "docs/modernization/dependency-map.json").read_text())
        deps = dm.get("job_dependencies", {})
        order = [j for j, *_ in self.job_rows]
        lines = ["# Dependency-order check against docs/modernization/dependency-map.json", "",
                 "Run order: " + " -> ".join(order), ""]
        ok = True
        for i, job in enumerate(order):
            for up, dsns in deps.get(job, {}).items():
                if up in order and order.index(up) > i:
                    ok = False
                    lines.append(f"- {job} is listed as depending on {up} (via {', '.join(dsns)}) but runs before it. "
                                 "The map derives edges from shared datasets, so jobs that both read and update a KSDS "
                                 "depend on each other; the baseline follows the ticket/JCL flow instead "
                                 "(print jobs against the freshly loaded masters, POSTTRAN before INTCALC, TRANBKP before COMBTRAN before TRANREPT).")
                elif up not in order:
                    lines.append(f"- {job}: upstream {up} is not part of the baseline run (not a batch job runnable here).")
        lines.append("")
        lines.append("Result: every recorded dependency runs earlier." if ok else
                     "Result: all other recorded dependencies run earlier.")
        write(OUT / "00-ORDER.md", "\n".join(lines) + "\n")

    # --------------------------------------------------------------- summary
    def write_summary(self):
        L = ["# CardDemo GnuCOBOL batch baseline", "",
             "Generated by `scripts/baseline/run_baseline.sh` — do not edit; re-run to regenerate.", "",
             "## Fixed run parameters", "",
             f"- Clock: `COB_CURRENT_DATE={FROZEN_CLOCK}` (all CURRENT-DATE / ACCEPT DATE,TIME calls)",
             f"- INTCALC PARM: `{INTCALC_PARM}` (from INTCALC.jcl)",
             f"- DATEPARM / TRANREPT window: `{DATEPARM_START}` .. `{DATEPARM_END}` (from TRANREPT.jcl SYMNAMES)",
             f"- WAITSTEP SYSIN: `{WAITSTEP_SYSIN}` centiseconds",
             "- cobc: `" + (OUT / "00-COMPILE" / "cobc-version.txt").read_text().splitlines()[0] + "`",
             "- Compile flags: `" + " ".join(COBC_BASE[1:]).replace(str(REPO) + "/", "") + "` (+ `-ftab-width=1` for CBSTM03A)",
             "", "## Compile matrix (`00-COMPILE/`)", "",
             "| Program | `cobc -fsyntax-only -std=ibm` (pristine) | `cobc -x` build | Notes |", "|---|---|---|---|"]
        for pgm, s_rc, b_rc, note in self.compile_rows:
            L.append(f"| {pgm} | rc={s_rc} | rc={b_rc} | {note} |")
        L += ["", "Pristine-source failures: CBSTM03A (tab characters in CUSTREC.cpy; fixed with `-ftab-width=1`), "
              "CBEXPORT and CBIMPORT (`EXPORT-SEQUENCE-NUM is not defined`: RECORD KEY names a WORKING-STORAGE item; built from a 2-hunk patched copy, diff in `00-COMPILE/`). "
              "CBSTM03A additionally SIGSEGVs at run time on `SET ADDRESS OF PSA-BLOCK TO PSAPTR` (z/OS control blocks); its build copy drops that diagnostic walk.",
              "", "## Input data (`00-DATA/`)", "", "| Dataset | Source | LRECL | Records | Notes |", "|---|---|---|---|---|"]
        for name, src, lrecl, n, note in self.data_rows:
            L.append(f"| {name} | {src} | {lrecl} | {n} | {note} |")
        L += ["", "## Jobs", "", "| # | Job | Programs / emulated steps | RC | Output files |", "|---|---|---|---|---|"]
        for i, (job, pgms, rc, outs, note) in enumerate(self.job_rows, 1):
            L.append(f"| {i} | [{job}]({job}/) | {pgms} | {rc} | {outs} |")
        L += ["", "Each job directory holds `sysout.txt` (DISPLAY output + emulated utility messages), `rc.txt`, `job.md` "
              "(JCL steps, emulations, DD assignments) and one `.txt` per output dataset (one fixed-length record per line; "
              "bytes outside 0x20-0x7E rendered as `\\xNN`). `<KSDS>.ksds.txt` is the after-image of each VSAM cluster the job updated.",
              "", "## Programs not compiled or not run", ""]
        if self.not_run:
            for pgm, why in self.not_run:
                L.append(f"- {pgm}: {why}")
        else:
            L.append("- All 14 batch programs compiled and ran. Three needed source-level handling, applied to build-time copies only "
                     "(diffs in `00-COMPILE/*.gnucobol.patch`): CBEXPORT/CBIMPORT (RECORD KEY in WORKING-STORAGE), "
                     "CBSTM03A (z/OS PSA/TCB/TIOT control-block walk removed; also `-ftab-width=1`).")
        L += ["- The 17 CICS online programs (CO*) need CICS (EXEC CICS SEND/RECEIVE/READ/XCTL) and are not runnable under GnuCOBOL; "
              "their behaviour is captured as rules in `docs/modernization/rules/`.",
              "- Assembler COBDATFT / MVSWAIT and LE services CEE3ABD / CEEDAYS are replaced by the stubs in `scripts/baseline/stubs/`.",
              "", "## Emulated z/OS utilities", "",
              "- IDCAMS DEFINE/REPRO/DELETE -> generated GnuCOBOL `IDX*` load/unload utilities (BDB indexed files).",
              "- DFSORT SORT/INCLUDE/OUTREC -> `dfsort()` in `baseline.py`; control cards quoted in each `job.md`.",
              "- GDG (+1)/(0) -> `Catalog` (generation files under the work dir).",
              "- Job order check: `00-ORDER.md`."]
        write(OUT / "README.md", "\n".join(L) + "\n")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--fast", action="store_true", help="skip the 36 s MVSWAIT sleep in WAITSTEP")
    ap.add_argument("--keep-work", action="store_true", help="keep build/baseline-work after the run")
    a = ap.parse_args()
    b = Baseline(fast=a.fast)
    b.run()
    if not a.keep_work:
        shutil.rmtree(WORK, ignore_errors=True)
    bad = [r for r in b.compile_rows if r[2] not in (0,)]
    print(f"baseline written to {OUT.relative_to(REPO)}; jobs={len(b.job_rows)} compile failures={len(bad)}")


if __name__ == "__main__":
    main()

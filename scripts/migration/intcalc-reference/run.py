#!/usr/bin/env python3
"""INTCALC reference run: CBACT04C compiled with GnuCOBOL on one input set.

usage: run.py <set-dir> <out-dir> [--parm 2022071800] [--clock "2022/07/18 00:00:00.00"]

<set-dir> holds acctdata.txt, cardxref.txt, discgrp.txt, tcatbal.txt (the app/data/ASCII
line-sequential shape). Each is loaded into a GnuCOBOL indexed file (the VSAM KSDS, with the
CARDXREF alternate key on the account id), CBACT04C runs through RUNCB04 with the JCL PARM and a
frozen CURRENT-DATE (COB_CURRENT_DATE), and <out-dir> receives:
  SYSTRAN       350-byte fixed records, the TRANSACT DD (AWS.M2.CARDDEMO.SYSTRAN(+1))
  ACCTDATA      300-byte fixed records, the ACCTFILE KSDS after the run, in key order
  SYSOUT.txt    the program's DISPLAY output;  RETURN-CODE
app/cbl and app/cpy are compiled as they are (read only).
"""
import argparse, os, shutil, subprocess, sys, tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
# name: (input file, lrecl, primary key (offset, length), alternate keys)
KSDS = {
    "TCATBALF": ("tcatbal.txt", 50, (0, 17), []),
    "XREFFILE": ("cardxref.txt", 50, (0, 16), [(25, 11)]),
    "ACCTFILE": ("acctdata.txt", 300, (0, 11), []),
    "DISCGRP": ("discgrp.txt", 50, (0, 16), []),
}

IDXUTIL = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. {prog}.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO SEQFILE
               ORGANIZATION IS SEQUENTIAL FILE STATUS IS WS-SEQ-STAT.
           SELECT IDX-FILE ASSIGN TO IDXFILE
               ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
               RECORD KEY IS IDX-KEY{alt}
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
       01  WS-MODE                 PIC X(4).
       PROCEDURE DIVISION.
           ACCEPT WS-MODE FROM COMMAND-LINE.
           IF WS-MODE = 'LOAD'
              OPEN INPUT SEQ-FILE OUTPUT IDX-FILE
              PERFORM UNTIL WS-EOF = 'Y'
                 READ SEQ-FILE AT END MOVE 'Y' TO WS-EOF
                 NOT AT END
                    WRITE IDX-REC FROM SEQ-REC
                    IF WS-IDX-STAT NOT = '00'
                       DISPLAY 'LOAD STATUS ' WS-IDX-STAT ' ' IDX-KEY
                       MOVE 12 TO RETURN-CODE
                    END-IF
                 END-READ
              END-PERFORM
           ELSE
              OPEN INPUT IDX-FILE OUTPUT SEQ-FILE
              PERFORM UNTIL WS-EOF = 'Y'
                 READ IDX-FILE NEXT AT END MOVE 'Y' TO WS-EOF
                 NOT AT END WRITE SEQ-REC FROM IDX-REC
                 END-READ
              END-PERFORM
           END-IF.
           CLOSE SEQ-FILE IDX-FILE.
           GOBACK.
"""


def idxutil(name, lrecl, key, alts):
    segs = sorted([(key[0], key[1], "IDX-KEY")] + [(o, n, f"IDX-ALT{i}") for i, (o, n) in enumerate(alts, 1)])
    lines, pos = [], 0
    for off, length, field in segs:
        if off > pos:
            lines.append(f"           05  FILLER              PIC X({off - pos}).")
        lines.append(f"           05  {field:<20}PIC X({length}).")
        pos = off + length
    if lrecl > pos:
        lines.append(f"           05  FILLER              PIC X({lrecl - pos}).")
    alt = "".join(f"\n               ALTERNATE RECORD KEY IS IDX-ALT{i}" for i in range(1, len(alts) + 1))
    return IDXUTIL.format(prog=f"IDX{name[:5]}", alt=alt, lrecl=lrecl, fields="\n".join(lines))


def fixed(src: Path, lrecl: int) -> bytes:
    out = bytearray()
    for line in src.read_bytes().split(b"\n"):
        line = line.rstrip(b"\r")
        if line:
            out += line[:lrecl].ljust(lrecl, b" ")
    return bytes(out)


def cobc(*args):
    subprocess.run(["cobc", "-std=ibm", "-fsign=EBCDIC", "-I", str(REPO / "app/cpy"), *args], check=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("set_dir", type=Path)
    ap.add_argument("out_dir", type=Path)
    ap.add_argument("--parm", default="2022071800")
    ap.add_argument("--clock", default="2022/07/18 00:00:00.00")
    a = ap.parse_args()
    out = a.out_dir.resolve()
    out.mkdir(parents=True, exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix="intcalc-ref-"))
    try:
        env = dict(os.environ, COB_CURRENT_DATE=a.clock)
        program = work / "runcb04"
        cobc("-x", "-o", str(program), str(HERE / "RUNCB04.cbl"), str(REPO / "app/cbl/CBACT04C.cbl"),
             str(HERE / "CEE3ABD.cbl"))
        for name, (file, lrecl, key, alts) in KSDS.items():
            src = work / f"IDX{name}.cbl"
            src.write_text(idxutil(name, lrecl, key, alts))
            cobc("-x", "-o", str(work / f"idx{name}"), str(src))
            (work / f"{name}.seq").write_bytes(fixed(a.set_dir / file, lrecl))
            subprocess.run([str(work / f"idx{name}"), "LOAD"], check=True, env=dict(
                env, DD_SEQFILE=str(work / f"{name}.seq"), DD_IDXFILE=str(work / f"{name}.idx")))
            env[f"DD_{name}"] = str(work / f"{name}.idx")
        env["DD_TRANSACT"] = str(out / "SYSTRAN")
        run = subprocess.run([str(program), a.parm], env=env, capture_output=True)
        (out / "SYSOUT.txt").write_bytes(run.stdout + run.stderr)
        (out / "RETURN-CODE").write_text(f"{run.returncode}\n")
        subprocess.run([str(work / "idxACCTFILE"), "UNLD"], check=True, env=dict(
            env, DD_SEQFILE=str(out / "ACCTDATA"), DD_IDXFILE=str(work / "ACCTFILE.idx")))
        print(f"CBACT04C RC={run.returncode}; SYSTRAN {os.path.getsize(out / 'SYSTRAN') // 350 if (out / 'SYSTRAN').exists() else 0} records -> {out}")
        return run.returncode
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())

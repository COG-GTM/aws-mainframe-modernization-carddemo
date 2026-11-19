"""GnuCOBOL batch test harness for CardDemo.

* Compiles programs from app/cbl with IBM-compatible flags.
* Emulates VSAM KSDS datasets with GnuCOBOL indexed files (generated
  loader/unloader programs), and QSAM datasets with fixed-length
  record-sequential files.
* Maps JCL DD names to files via GnuCOBOL's ``DD_<name>`` environment
  variables and runs the program, capturing RC and SYSOUT.
"""
import hashlib
import os
import re
import shutil
import subprocess
import tempfile
from decimal import Decimal
from pathlib import Path

TESTS = Path(__file__).resolve().parents[1]
REPO = TESTS.parent
APP_CBL = REPO / "app" / "cbl"
APP_CPY = REPO / "app" / "cpy"
STUBS = TESTS / "lib" / "stubs"
DRIVERS = TESTS / "lib" / "drivers"
FIXTURES = TESTS / "fixtures"
BUILD = Path(os.environ.get("CARDDEMO_TEST_BUILD", TESTS / ".build"))

COBC = os.environ.get("COBC", "cobc")
# -std=ibm      : Enterprise COBOL dialect (big-endian binary, etc.)
# -ftab-width=4 : CUSTREC.cpy/CSLKPCDY.cpy contain TABs; 4 keeps PICs
#                 inside column 72 (8 would truncate them).
# -fsign=EBCDIC : zoned-decimal overpunch signs ({ A-I } J-R) as on z/OS,
#                 which is also how app/data/ASCII stores signed numbers.
COBC_FLAGS = ["-std=ibm", "-fsign=EBCDIC", "-ftab-width=4", "-I", str(APP_CPY), "-w"]
CEE_STUBS = [STUBS / "CEE3ABD.cbl", STUBS / "CEEDAYS.c"]

ABEND_RC = 16  # RC produced by the CEE3ABD stub


# --------------------------------------------------------------------------
# Zoned decimal helpers (EBCDIC overpunch, as produced by -fsign=EBCDIC)
# --------------------------------------------------------------------------
_POS = "{ABCDEFGHI"
_NEG = "}JKLMNOPQR"


def zoned(value, digits, scale=0, signed=True):
    """Encode a number as zoned decimal text of ``digits`` characters."""
    q = (Decimal(str(value)) * (Decimal(10) ** scale)).to_integral_value()
    neg = q < 0
    text = str(abs(int(q))).rjust(digits, "0")
    if len(text) > digits:
        raise ValueError(f"{value} does not fit in {digits} digits")
    if not signed:
        return text
    last = int(text[-1])
    return text[:-1] + (_NEG[last] if neg else _POS[last])


def unzoned(text, scale=0):
    """Decode zoned decimal text (signed or unsigned) into a Decimal."""
    text = text.strip() or "0"
    last = text[-1]
    neg = False
    if last in _POS:
        last = str(_POS.index(last))
    elif last in _NEG:
        last = str(_NEG.index(last))
        neg = True
    elif last in "pqrstuvwxy":  # ASCII-style negative overpunch
        last = str("pqrstuvwxy".index(last))
        neg = True
    n = Decimal(int(text[:-1] + last))
    if neg:
        n = -n
    return n / (Decimal(10) ** scale)


# --------------------------------------------------------------------------
# Record files
# --------------------------------------------------------------------------
def read_fixture(name, reclen):
    """Read a fixture (one record per line) padded to ``reclen`` bytes."""
    recs = []
    for line in (FIXTURES / name).read_text().splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        if len(line) > reclen:
            raise ValueError(f"{name}: record longer than {reclen}: {line!r}")
        recs.append(line.ljust(reclen))
    return recs


def write_seq(path, records, reclen):
    """Write a fixed-length record-sequential (QSAM-like) file."""
    with open(path, "wb") as fh:
        for r in records:
            if len(r) != reclen:
                raise ValueError(f"record length {len(r)} != {reclen}")
            fh.write(r.encode("latin-1"))


def read_seq(path, reclen):
    data = Path(path).read_bytes().decode("latin-1")
    if len(data) % reclen:
        raise AssertionError(f"{path}: size {len(data)} not a multiple of {reclen}")
    return [data[i:i + reclen] for i in range(0, len(data), reclen)]


# --------------------------------------------------------------------------
# Compilation
# --------------------------------------------------------------------------
def _digest(paths, extra=""):
    h = hashlib.sha1(extra.encode())
    for p in paths:
        h.update(str(p).encode())
        h.update(Path(p).read_bytes())
    return h.hexdigest()[:12]


def _compile(sources, out, main_first=True):
    BUILD.mkdir(parents=True, exist_ok=True)
    cmd = [COBC, "-x", *COBC_FLAGS, "-o", str(out), *map(str, sources)]
    res = subprocess.run(cmd, capture_output=True, text=True, cwd=BUILD)
    if res.returncode != 0:
        raise RuntimeError(f"compile failed: {' '.join(cmd)}\n{res.stdout}{res.stderr}")
    return out


def build(name, sources):
    """Compile ``sources`` (main program first) into a cached executable."""
    sources = [Path(s) for s in sources]
    exe = BUILD / f"{name}-{_digest(sources, ' '.join(COBC_FLAGS))}"
    if not exe.exists():
        _compile(sources, exe)
    return exe


def program_source(pgm):
    for ext in (".cbl", ".CBL"):
        p = APP_CBL / f"{pgm}{ext}"
        if p.exists():
            return p
    raise FileNotFoundError(pgm)


def patched_source(pgm, patch):
    """Copy a program to the build dir applying ``patch(text) -> text``."""
    src = program_source(pgm).read_text(errors="replace")
    BUILD.mkdir(parents=True, exist_ok=True)
    out = BUILD / "patched" / f"{pgm}.cbl"
    out.parent.mkdir(exist_ok=True)
    new = patch(src)
    if not out.exists() or out.read_text() != new:
        out.write_text(new)
    return out


def comment_out(text, start_marker, end_marker):
    """Turn lines from the one containing start_marker up to (not including)
    the one containing end_marker into fixed-format comments."""
    lines = text.splitlines(keepends=True)
    active = False
    for i, line in enumerate(lines):
        if start_marker in line:
            active = True
        if end_marker in line:
            active = False
        if active and len(line) > 6:
            lines[i] = line[:6] + "*" + line[7:]
    return "".join(lines)


# --------------------------------------------------------------------------
# VSAM KSDS emulation (GnuCOBOL indexed files)
# --------------------------------------------------------------------------
class Ksds:
    """Describes a KSDS: record length, primary key and alternate keys.

    Keys are (offset, length) with 0-based offsets; alternate keys may add a
    third element ``True`` to allow duplicates.  The definition must match
    the SELECT clause of the program that opens the file.
    """

    def __init__(self, reclen, key, alt_keys=()):
        self.reclen = reclen
        self.key = key
        self.alt_keys = list(alt_keys)

    def _layout(self):
        marks = [(self.key[0], self.key[1], "PK", None)]
        for i, ak in enumerate(self.alt_keys):
            marks.append((ak[0], ak[1], f"AK{i}", ak[2] if len(ak) > 2 else False))
        marks.sort()
        lines, pos = [], 0
        for off, ln, nm, _ in marks:
            if off < pos:
                raise ValueError("overlapping keys not supported")
            if off > pos:
                lines.append(f"           05 FILLER PIC X({off - pos}).")
            lines.append(f"           05 {nm} PIC X({ln}).")
            pos = off + ln
        if pos < self.reclen:
            lines.append(f"           05 FILLER PIC X({self.reclen - pos}).")
        sel = "               RECORD KEY IS PK"
        for i, ak in enumerate(self.alt_keys):
            dup = " WITH DUPLICATES" if len(ak) > 2 and ak[2] else ""
            sel += f"\n               ALTERNATE RECORD KEY IS AK{i}{dup}"
        return "\n".join(lines), sel

    def _utility(self):
        rec, sel = self._layout()
        src = f"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. KSDSUTL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO SEQFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS SEQ-ST.
           SELECT KSDS-FILE ASSIGN TO KSDSFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
{sel}
               FILE STATUS IS KSDS-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-FILE.
       01  SEQ-REC PIC X({self.reclen}).
       FD  KSDS-FILE.
       01  KSDS-REC.
{rec}
       WORKING-STORAGE SECTION.
       01  SEQ-ST  PIC XX.
       01  KSDS-ST PIC XX.
       01  MODE-ARG PIC X(8).
       PROCEDURE DIVISION.
           ACCEPT MODE-ARG FROM COMMAND-LINE
           IF MODE-ARG = 'LOAD'
              OPEN INPUT SEQ-FILE OUTPUT KSDS-FILE
              PERFORM UNTIL SEQ-ST NOT = '00'
                 READ SEQ-FILE
                 IF SEQ-ST = '00'
                    WRITE KSDS-REC FROM SEQ-REC
                    IF KSDS-ST NOT = '00' AND NOT = '02'
                       DISPLAY 'KSDS WRITE STATUS ' KSDS-ST
                       MOVE 8 TO RETURN-CODE
                       STOP RUN
                    END-IF
                 END-IF
              END-PERFORM
           ELSE
              OPEN INPUT KSDS-FILE OUTPUT SEQ-FILE
              PERFORM UNTIL KSDS-ST NOT = '00' AND NOT = '02'
                 READ KSDS-FILE NEXT
                 IF KSDS-ST = '00' OR '02'
                    WRITE SEQ-REC FROM KSDS-REC
                 END-IF
              END-PERFORM
           END-IF
           CLOSE SEQ-FILE KSDS-FILE
           STOP RUN.
"""
        h = hashlib.sha1(src.encode()).hexdigest()[:12]
        exe = BUILD / f"ksdsutl-{h}"
        if not exe.exists():
            BUILD.mkdir(parents=True, exist_ok=True)
            srcf = BUILD / f"ksdsutl-{h}.cbl"
            srcf.write_text(src)
            _compile([srcf], exe)
        return exe

    def create(self, path, records):
        """Load records (any order; sorted by key like IDCAMS REPRO input)."""
        records = sorted(records, key=lambda r: r[self.key[0]:self.key[0] + self.key[1]])
        path = Path(path)
        for p in path.parent.glob(path.name + "*"):
            p.unlink()
        seq = path.with_suffix(".load")
        write_seq(seq, records, self.reclen)
        self._run("LOAD", seq, path)
        seq.unlink()

    def dump(self, path):
        seq = Path(path).with_suffix(".dump")
        self._run("UNLOAD", seq, path)
        recs = read_seq(seq, self.reclen) if seq.stat().st_size else []
        seq.unlink()
        return recs

    def _run(self, mode, seq, ksds):
        env = dict(os.environ, DD_SEQFILE=str(seq), DD_KSDSFILE=str(ksds))
        res = subprocess.run([str(self._utility()), mode], env=env,
                             capture_output=True, text=True)
        if res.returncode != 0:
            raise RuntimeError(f"KSDS {mode} failed: {res.stdout}{res.stderr}")


# --------------------------------------------------------------------------
# Running
# --------------------------------------------------------------------------
class Workdir:
    """Temporary directory holding the datasets of one test run."""

    def __init__(self, label):
        keep = os.environ.get("CARDDEMO_KEEP_WORK")
        self.path = Path(tempfile.mkdtemp(prefix=f"carddemo-{label}-"))
        self._keep = bool(keep)

    def __truediv__(self, name):
        return self.path / name

    def cleanup(self):
        if not self._keep:
            shutil.rmtree(self.path, ignore_errors=True)


class RunResult:
    def __init__(self, proc):
        self.rc = proc.returncode
        self.stdout = proc.stdout
        self.stderr = proc.stderr

    def __repr__(self):
        return f"RC={self.rc}\n--- SYSOUT ---\n{self.stdout}\n--- STDERR ---\n{self.stderr}"


def run(exe, dds, args=(), timeout=60):
    """Run ``exe`` with each ``{DDNAME: path}`` exposed as DD_<DDNAME>."""
    env = dict(os.environ)
    for dd, path in dds.items():
        env[f"DD_{dd}"] = str(path)
    env.setdefault("COB_SET_TRACE", "N")
    proc = subprocess.run([str(exe), *args], env=env, capture_output=True,
                          text=True, errors="replace", timeout=timeout)
    return RunResult(proc)


def field(rec, offset, length):
    return rec[offset:offset + length]

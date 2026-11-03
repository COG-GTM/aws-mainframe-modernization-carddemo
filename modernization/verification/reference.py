#!/usr/bin/env python3
"""COBOL side of the golden-set harness: the monthly chain under GnuCOBOL on app/data/ASCII.

usage: reference.py <out-dir> [--parm 2022071800] [--clock "2022/07/18 00:00:00.00"]
                              [--dateparm "2022-01-01 2099-12-31"]

Runs the unmodified app/cbl programs in the Control-M order of docs/migration/jcl-mapping.md, each
step reading what the previous one left (VSAM KSDS = GnuCOBOL indexed files loaded from the
sequential extracts, as the per-job reference harnesses do), CURRENT-DATE frozen at --clock:

  POSTTRAN  CBTRN02C   DALYTRAN -> TRANFILE, DALYREJS, ACCTFILE, TCATBALF
  INTCALC   CBACT04C   scripts/migration/intcalc-reference/run.py on POSTTRAN's after-images
  COMBTRAN  SORT       TRANSACT + SYSTRAN by TRAN-ID: the TRANSACT KSDS the next jobs read
  TRANREPT  CBTRN03C   scripts/migration/tranrept/reference.sh on the combined TRANSACT
  CREASTMT  CBSTM03A/B STEP010 sort + OUTREC, TRXFL KSDS, compile shims of
                       scripts/migration/golden/batch-service-reference.sh

<out-dir>/<JOB>/ receives every output file of the step (the golden set) plus RETURN-CODE and
SYSOUT. app/ is read only.
"""
import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
DATA = REPO / "app/data/ASCII"
sys.dont_write_bytecode = True
sys.path.insert(0, str(REPO / "scripts/migration/intcalc-reference"))
from run import cobc, fixed, idxutil  # noqa: E402  (INTCALC reference: KSDS load/unload utility)

TRAN_LEN = 350


def records(data: bytes, lrecl: int):
    return [data[i:i + lrecl] for i in range(0, len(data), lrecl)]


def lines(data: bytes, lrecl: int) -> bytes:
    """Fixed records as the line-sequential shape of app/data/ASCII (what the per-job runners take)."""
    return b"".join(r + b"\n" for r in records(data, lrecl))


class Ksds:
    """GnuCOBOL indexed files standing in for the VSAM clusters."""

    def __init__(self, work: Path, env: dict):
        self.work, self.env, self.built = work, env, set()

    def _util(self, name, lrecl, key, alts):
        exe = self.work / f"idx{name}"
        if name not in self.built:
            src = self.work / f"IDX{name}.cbl"
            src.write_text(idxutil(name, lrecl, key, alts))
            cobc("-x", "-o", str(exe), str(src))
            self.built.add(name)
        return exe

    def load(self, name, seq: bytes, lrecl, key, alts=()):
        exe = self._util(name, lrecl, key, list(alts))
        (self.work / f"{name}.seq").write_bytes(seq)
        idx = self.work / f"{name}.idx"
        for stale in self.work.glob(f"{name}.idx*"):
            stale.unlink()
        subprocess.run([str(exe), "LOAD"], check=True, env=dict(
            self.env, DD_SEQFILE=str(self.work / f"{name}.seq"), DD_IDXFILE=str(idx)))
        return idx

    def unload(self, name, lrecl, key, alts=()) -> bytes:
        exe = self._util(name, lrecl, key, list(alts))
        seq = self.work / f"{name}.unl"
        subprocess.run([str(exe), "UNLD"], check=True, env=dict(
            self.env, DD_SEQFILE=str(seq), DD_IDXFILE=str(self.work / f"{name}.idx")))
        return seq.read_bytes()


def run(cmd, out: Path, env: dict, cwd=None) -> int:
    proc = subprocess.run(cmd, env=env, cwd=cwd, capture_output=True)
    (out / "SYSOUT").write_bytes(proc.stdout + proc.stderr)
    (out / "RETURN-CODE").write_text(f"{proc.returncode}\n")
    return proc.returncode


def posttran(out: Path, work: Path, env: dict, ksds: Ksds):
    exe = work / "CBTRN02C"
    cobc("-x", "-o", str(exe), str(REPO / "app/cbl/CBTRN02C.cbl"),
         str(REPO / "scripts/migration/intcalc-reference/CEE3ABD.cbl"))
    files = {
        "XREFFILE": ksds.load("XREFFILE", fixed(DATA / "cardxref.txt", 50), 50, (0, 16), [(25, 11)]),
        "ACCTFILE": ksds.load("ACCTFILE", fixed(DATA / "acctdata.txt", 300), 300, (0, 11)),
        "TCATBALF": ksds.load("TCATBALF", fixed(DATA / "tcatbal.txt", 50), 50, (0, 17)),
        "TRANFILE": ksds.load("TRANFILE", b"", TRAN_LEN, (0, 16)),
    }
    (work / "DALYTRAN").write_bytes(fixed(DATA / "dailytran.txt", TRAN_LEN))
    dd = {f"DD_{k}": str(v) for k, v in files.items()}
    rc = run([str(exe)], out, dict(env, **dd, DD_DALYTRAN=str(work / "DALYTRAN"),
                                   DD_DALYREJS=str(out / "DALYREJS")))
    (out / "TRANSACT").write_bytes(ksds.unload("TRANFILE", TRAN_LEN, (0, 16)))
    (out / "ACCTFILE").write_bytes(ksds.unload("ACCTFILE", 300, (0, 11)))
    (out / "TCATBALF").write_bytes(ksds.unload("TCATBALF", 50, (0, 17)))
    return rc


def intcalc(out: Path, posttran_out: Path, parm: str, clock: str):
    inputs = out / "input"
    inputs.mkdir()
    (inputs / "acctdata.txt").write_bytes(lines((posttran_out / "ACCTFILE").read_bytes(), 300))
    (inputs / "tcatbal.txt").write_bytes(lines((posttran_out / "TCATBALF").read_bytes(), 50))
    shutil.copy(DATA / "cardxref.txt", inputs / "cardxref.txt")
    shutil.copy(DATA / "discgrp.txt", inputs / "discgrp.txt")
    subprocess.run([sys.executable, str(REPO / "scripts/migration/intcalc-reference/run.py"), str(inputs), str(out),
                    "--parm", parm, "--clock", clock], check=True, stdout=subprocess.DEVNULL)
    shutil.rmtree(inputs)
    (out / "SYSOUT.txt").rename(out / "SYSOUT")
    (out / "ACCTDATA").rename(out / "ACCTFILE")
    return int((out / "RETURN-CODE").read_text())


def combtran(out: Path, posttran_out: Path, intcalc_out: Path):
    systran = intcalc_out / "SYSTRAN"
    recs = records((posttran_out / "TRANSACT").read_bytes() + (systran.read_bytes() if systran.exists() else b""),
                   TRAN_LEN)
    recs.sort(key=lambda r: r[:16])
    (out / "TRANSACT").write_bytes(b"".join(recs))
    (out / "RETURN-CODE").write_text("0\n")
    return 0


def tranrept(out: Path, transact: Path, dateparm: str, work: Path):
    case = work / "tranrept-case"
    case.mkdir()
    (case / "TRANSACT.txt").write_bytes(lines(transact.read_bytes(), TRAN_LEN))
    (case / "DATEPARM.txt").write_text(dateparm + "\n")
    run_dir = work / "tranrept-run"
    proc = subprocess.run(["bash", str(REPO / "scripts/migration/tranrept/reference.sh"), str(case), str(run_dir)],
                          capture_output=True)
    if not (run_dir / "RETURN-CODE").exists():
        sys.stderr.write(proc.stdout.decode() + proc.stderr.decode())
        raise SystemExit("TRANREPT reference run failed")
    for name in ("TRANREPT", "TRANSACT.DALY", "RETURN-CODE"):
        if (run_dir / name).exists():
            shutil.copy(run_dir / name, out / name)
    (out / "SYSOUT").write_bytes(proc.stdout + proc.stderr)
    return int((out / "RETURN-CODE").read_text())


def creastmt(out: Path, transact: Path, acctfile: Path, work: Path, env: dict, ksds: Ksds):
    cpy, cbl, lib = work / "stmt-cpy", work / "stmt-cbl", work / "stmt-lib"
    for d in (cpy, cbl, lib):
        d.mkdir()
    # Compile shims of batch-service-reference.sh (see shims.md): CUSTREC.cpy tabs expanded, the
    # DISPLAY-only TIOT walk removed. Temporary copies; app/ is not touched.
    (cpy / "CUSTREC.cpy").write_bytes(subprocess.run(["expand", "-t", "1", str(REPO / "app/cpy/CUSTREC.cpy")],
                                                     check=True, capture_output=True).stdout)
    (cbl / "CBSTM03A.cbl").write_bytes(subprocess.run(
        ["sed", "-e", "/SET ADDRESS OF PSA-BLOCK/,/OPEN OUTPUT STMT-FILE/{/OPEN OUTPUT STMT-FILE/!d}",
         str(REPO / "app/cbl/CBSTM03A.CBL")], check=True, capture_output=True).stdout)
    shim_report(out, cpy / "CUSTREC.cpy", cbl / "CBSTM03A.cbl")
    shim_cobc = ["cobc", "-std=ibm", "-fsign=EBCDIC", "-I", str(cpy), "-I", str(REPO / "app/cpy")]
    subprocess.run([*shim_cobc, "-m", "-o", str(lib / "CBSTM03B.so"), str(REPO / "app/cbl/CBSTM03B.CBL")], check=True)
    exe = work / "CBSTM03A"
    subprocess.run([*shim_cobc, "-x", "-o", str(exe), str(cbl / "CBSTM03A.cbl")], check=True)
    # STEP010 SORT FIELDS=(263,16,CH,A,1,16,CH,A) OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)
    recs = sorted(records(transact.read_bytes(), TRAN_LEN), key=lambda r: (r[262:278], r[0:16]))
    trxfl = b"".join((r[262:278] + r[0:262] + r[278:328]).ljust(TRAN_LEN, b" ") for r in recs)
    (out / "TRXFL.SEQ").write_bytes(trxfl)
    dd = {
        "DD_TRNXFILE": ksds.load("TRXFL", trxfl, TRAN_LEN, (0, 32)),
        "DD_XREFFILE": work / "XREFFILE.idx",
        "DD_ACCTFILE": ksds.load("ACCTSTMT", acctfile.read_bytes(), 300, (0, 11)),
        "DD_CUSTFILE": ksds.load("CUSTFILE", fixed(DATA / "custdata.txt", 500), 500, (0, 9)),
    }
    return run([str(exe)], out, dict(env, **{k: str(v) for k, v in dd.items()}, COB_LIBRARY_PATH=str(lib),
                                     DD_STMTFILE="STMTFILE", DD_HTMLFILE="HTMLFILE"), cwd=out)


def shim_report(out: Path, custrec: Path, cbstm03a: Path):
    """SHIMS.txt: evidence that the compile shims change no behavior. CUSTREC.cpy must differ from
    app/cpy only in white space; the CBSTM03A copy must only lose lines, all of them listed here for
    review (the TIOT walk: it only DISPLAYs the job and step names found in z/OS control blocks)."""
    squash = lambda b: b"".join(b.split())  # noqa: E731
    orig_cpy = (REPO / "app/cpy/CUSTREC.cpy").read_bytes()
    if squash(orig_cpy) != squash(custrec.read_bytes()):
        raise SystemExit("CUSTREC.cpy shim changed more than white space")
    orig = (REPO / "app/cbl/CBSTM03A.CBL").read_bytes().splitlines()
    kept = cbstm03a.read_bytes().splitlines()
    removed, k = [], 0
    for n, line in enumerate(orig, 1):
        if k < len(kept) and kept[k] == line:
            k += 1
        else:
            removed.append(f"{n:5}: {line.decode('latin-1').rstrip()}")
    if k != len(kept):
        raise SystemExit("CBSTM03A shim is not a pure deletion")
    if not removed or "SET ADDRESS OF PSA-BLOCK" not in removed[0]:
        raise SystemExit("CBSTM03A shim does not start at the TIOT walk")
    (out / "SHIMS.txt").write_text(
        "CUSTREC.cpy: tabs expanded (expand -t 1); identical to app/cpy/CUSTREC.cpy apart from white space.\n"
        f"CBSTM03A.CBL: pure deletion of {len(removed)} lines (the z/OS TIOT walk), nothing added:\n"
        + "\n".join(removed) + "\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("out_dir", type=Path)
    ap.add_argument("--parm", default="2022071800")
    ap.add_argument("--clock", default="2022/07/18 00:00:00.00")
    ap.add_argument("--dateparm", default="2022-01-01 2099-12-31")
    a = ap.parse_args()
    out = a.out_dir.resolve()
    shutil.rmtree(out, ignore_errors=True)
    jobs = {j: out / j for j in ("POSTTRAN", "INTCALC", "COMBTRAN", "TRANREPT", "CREASTMT")}
    for d in jobs.values():
        d.mkdir(parents=True)
    work = Path(tempfile.mkdtemp(prefix="golden-ref-"))
    env = dict(os.environ, COB_CURRENT_DATE=a.clock, TZ="UTC")
    ksds = Ksds(work, env)
    try:
        rcs = {
            "POSTTRAN": posttran(jobs["POSTTRAN"], work, env, ksds),
            "INTCALC": intcalc(jobs["INTCALC"], jobs["POSTTRAN"], a.parm, a.clock),
            "COMBTRAN": combtran(jobs["COMBTRAN"], jobs["POSTTRAN"], jobs["INTCALC"]),
        }
        rcs["TRANREPT"] = tranrept(jobs["TRANREPT"], jobs["COMBTRAN"] / "TRANSACT", a.dateparm, work)
        rcs["CREASTMT"] = creastmt(jobs["CREASTMT"], jobs["COMBTRAN"] / "TRANSACT", jobs["INTCALC"] / "ACCTFILE",
                                   work, env, ksds)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    for job, rc in rcs.items():
        print(f"{job:<9} RC={rc}  {' '.join(sorted(p.name for p in jobs[job].iterdir()))}")


if __name__ == "__main__":
    main()

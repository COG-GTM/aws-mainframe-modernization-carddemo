#!/usr/bin/env python3
"""GnuCOBOL golden master for the CardDemo nightly batch chain.

Compiles the unchanged batch programs from app/cbl, runs
POSTTRAN -> INTCALC -> COMBTRAN -> TRANREPT -> CREASTMT on app/data/ASCII
with a frozen clock, and writes every dataset and SYSOUT the chain produces
to baselines/. Each invocation runs the chain twice in separate work
directories and fails unless both runs are byte-identical.

  golden_master.py            regenerate baselines/
  golden_master.py --check    rerun and fail on any difference from baselines/
"""

import argparse
import difflib
import hashlib
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
CBL = REPO / "app" / "cbl"
CPY = REPO / "app" / "cpy"
DATA = REPO / "app" / "data" / "ASCII"
BASELINES = HERE / "baselines"
WORK = HERE / ".work"

JOBS = ["POSTTRAN", "INTCALC", "COMBTRAN", "TRANREPT", "CREASTMT"]

# Run parameters. The clock drives TRAN-PROC-TS in CBTRN02C and CBACT04C;
# 2022-07-06 is the TRANREPT JCL end date, so both programs' postings fall
# inside the report window. INTCALC's PARM and the report window come
# straight from app/jcl/INTCALC.jcl and app/jcl/TRANREPT.jcl.
CLOCK = "2022/07/06 12:00:00.00"
INTCALC_PARM = "2022071800"
TRANREPT_START = "2022-01-01"
TRANREPT_END = "2022-07-06"

# -ftab-width=3: CUSTREC.cpy indents with tabs that line up in area B only
# at width 3; at GnuCOBOL's default of 8 its PICTUREs run past column 72.
COBC_FLAGS = ["-std=ibm", "-fsign=EBCDIC", "-ftab-width=3", "-I", str(CPY)]


@dataclass(frozen=True)
class Ksds:
    """A VSAM KSDS layout: record length, primary key and optional AIX key."""
    program: str
    reclen: int
    keylen: int
    alt_offset: int = 0
    alt_len: int = 0


# Layouts from the FDs / IDCAMS DEFINEs; all primary keys start at offset 0.
ACCTDATA = Ksds("KSACCT", 300, 11)
CARDXREF = Ksds("KSXREF", 50, 16, alt_offset=25, alt_len=11)  # AIX on XREF-ACCT-ID
CUSTDATA = Ksds("KSCUST", 500, 9)
DISCGRP = Ksds("KSDISC", 50, 16)
TCATBALF = Ksds("KSTCAT", 50, 17)
TRANCATG = Ksds("KSTRCAT", 60, 6)
TRANTYPE = Ksds("KSTRTYP", 60, 2)
TRANSACT = Ksds("KSTRAN", 350, 16)
TRXFL = Ksds("KSTRXFL", 350, 32)
LAYOUTS = [ACCTDATA, CARDXREF, CUSTDATA, DISCGRP, TCATBALF, TRANCATG, TRANTYPE, TRANSACT, TRXFL]


class HarnessError(Exception):
    pass


# ---------------------------------------------------------------- records
def read_lines(path: Path, reclen: int) -> list[str]:
    """Text file, one record per line (CR/LF tolerated), padded to reclen."""
    records = []
    for n, line in enumerate(path.read_text(encoding="ascii").splitlines(), 1):
        if len(line) > reclen:
            raise HarnessError(f"{path}:{n}: record longer than {reclen}")
        records.append(line.ljust(reclen))
    return records


def write_lines(path: Path, records: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(r + "\n" for r in records), encoding="ascii")


def to_fixed(path: Path, records: list[str]) -> None:
    path.write_bytes("".join(records).encode("ascii"))


def from_fixed(path: Path, reclen: int) -> list[str]:
    raw = path.read_bytes().decode("ascii") if path.exists() else ""
    if len(raw) % reclen:
        raise HarnessError(f"{path}: {len(raw)} bytes is not a multiple of {reclen}")
    return [raw[i:i + reclen] for i in range(0, len(raw), reclen)]


# ---------------------------------------------------------------- build
def compile_programs(bindir: Path) -> None:
    bindir.mkdir(parents=True)
    src = bindir / "src"
    src.mkdir()

    def cobc(exe: str, *sources: Path) -> None:
        proc = subprocess.run(["cobc", "-x", *COBC_FLAGS, "-o", str(bindir / exe), *map(str, sources)],
                              capture_output=True, text=True)
        if proc.returncode:
            raise HarnessError(f"cobc {exe} failed:\n{proc.stdout}{proc.stderr}")

    template = (HERE / "cobol" / "KSDSREPRO.cbl.in").read_text()
    for k in LAYOUTS:
        lines = []
        for line in template.splitlines():
            if line.rstrip().endswith("ALTKEY"):
                if not k.alt_len:
                    continue
                line = line.rstrip()[:-len("ALTKEY")].rstrip()
            lines.append(line)
        text = "\n".join(lines) + "\n"
        for name, value in {"PROGRAM": k.program, "RECLEN": k.reclen, "KEYLEN": k.keylen,
                            "RESTLEN": k.reclen - k.keylen, "ALTOFFSET": k.alt_offset,
                            "ALTLEN": k.alt_len}.items():
            text = text.replace(f"@{name}@", str(value))
        path = src / f"{k.program}.cbl"
        path.write_text(text)
        cobc(k.program, path)

    # CBSTM03A opens by walking the z/OS PSA -> TCB -> TIOT control blocks to
    # DISPLAY the job, step and DD names. Those blocks do not exist off the
    # mainframe, so the build copy drops that diagnostic block (from the first
    # SET ADDRESS OF PSA-BLOCK up to OPEN OUTPUT STMT-FILE). Nothing else changes.
    stm03a = (CBL / "CBSTM03A.CBL").read_text().splitlines()
    start = next(i for i, l in enumerate(stm03a) if "SET ADDRESS OF PSA-BLOCK" in l)
    end = next(i for i, l in enumerate(stm03a) if "OPEN OUTPUT STMT-FILE" in l)
    (src / "CBSTM03A.CBL").write_text("\n".join(stm03a[:start] + stm03a[end:]) + "\n")

    abend = HERE / "cobol" / "CEE3ABD.cbl"
    cobc("CBTRN02C", CBL / "CBTRN02C.cbl", abend)
    cobc("CBACT04C", HERE / "cobol" / "JCLPARM.cbl", CBL / "CBACT04C.cbl", abend)
    cobc("CBTRN03C", CBL / "CBTRN03C.cbl", abend)
    cobc("CBSTM03A", src / "CBSTM03A.CBL", CBL / "CBSTM03B.CBL", abend)


# ---------------------------------------------------------------- run
class Run:
    """One pass of the batch chain in its own work directory."""

    def __init__(self, workdir: Path, bindir: Path, out: Path):
        self.dir = workdir
        self.bin = bindir
        self.out = out
        self.rc: dict[str, int] = {}
        self.dir.mkdir(parents=True)
        self.out.mkdir(parents=True)
        self.env = {k: v for k, v in os.environ.items() if not k.startswith(("DD_", "COB_"))}
        self.env.update(LC_ALL="C", TZ="UTC", COB_CURRENT_DATE=CLOCK)

    def path(self, name: str) -> Path:
        return self.dir / name

    def exec(self, args: list[str], dds: dict[str, str]) -> subprocess.CompletedProcess:
        env = dict(self.env, **{f"DD_{dd}": str(self.path(f)) for dd, f in dds.items()})
        return subprocess.run(args, cwd=self.dir, env=env, capture_output=True)

    def load(self, k: Ksds, records: list[str], name: str) -> None:
        seq = self.path(f"{name}.load")
        to_fixed(seq, records)
        for stale in self.dir.glob(f"{name}*"):
            if stale != seq:
                stale.unlink()
        proc = self.exec([str(self.bin / k.program), "LOAD"], {"SEQFILE": seq.name, "KSDSFILE": name})
        seq.unlink()
        if proc.returncode:
            raise HarnessError(f"{k.program} LOAD {name}: RC={proc.returncode}\n{proc.stdout.decode()}")

    def unload(self, k: Ksds, name: str) -> list[str]:
        seq = self.path(f"{name}.unload")
        proc = self.exec([str(self.bin / k.program), "UNLOAD"], {"SEQFILE": seq.name, "KSDSFILE": name})
        if proc.returncode:
            raise HarnessError(f"{k.program} UNLOAD {name}: RC={proc.returncode}\n{proc.stdout.decode()}")
        records = from_fixed(seq, k.reclen)
        seq.unlink()
        return records

    def step(self, job: str, args: list[str], dds: dict[str, str]) -> None:
        proc = self.exec([str(self.bin / args[0]), *args[1:]], dds)
        sysout = self.out / job / "sysout.txt"
        sysout.parent.mkdir(parents=True, exist_ok=True)
        sysout.write_bytes(proc.stdout + proc.stderr)
        self.rc[job] = proc.returncode
        if proc.returncode > 4:
            raise HarnessError(f"{job} ({args[0]}) RC={proc.returncode}\n{(proc.stdout + proc.stderr).decode()}")

    def save(self, job: str, name: str, records: list[str]) -> None:
        write_lines(self.out / job / name, records)

    # ------------------------------------------------------------ jobs
    def run(self) -> None:
        xref = read_lines(DATA / "cardxref.txt", CARDXREF.reclen)

        # POSTTRAN / CBTRN02C: validate and post DALYTRAN into TRANSACT,
        # ACCTDATA and TCATBALF; rejects go to DALYREJS. TRANSACT starts empty,
        # as TRANBKP leaves it after its DELETE/DEFINE.
        self.load(CARDXREF, xref, "xref.ksds")
        self.load(ACCTDATA, read_lines(DATA / "acctdata.txt", ACCTDATA.reclen), "acct.ksds")
        self.load(TCATBALF, read_lines(DATA / "tcatbal.txt", TCATBALF.reclen), "tcatbal.ksds")
        self.load(TRANSACT, [], "transact.ksds")
        to_fixed(self.path("dalytran.seq"), read_lines(DATA / "dailytran.txt", 350))
        self.step("POSTTRAN", ["CBTRN02C"], {
            "DALYTRAN": "dalytran.seq", "TRANFILE": "transact.ksds", "XREFFILE": "xref.ksds",
            "DALYREJS": "dalyrejs.seq", "ACCTFILE": "acct.ksds", "TCATBALF": "tcatbal.ksds"})
        transact = self.unload(TRANSACT, "transact.ksds")
        acct = self.unload(ACCTDATA, "acct.ksds")
        tcatbal = self.unload(TCATBALF, "tcatbal.ksds")
        self.save("POSTTRAN", "transact.txt", transact)
        self.save("POSTTRAN", "acctdata.txt", acct)
        self.save("POSTTRAN", "tcatbal.txt", tcatbal)
        self.save("POSTTRAN", "dalyrejs.txt", from_fixed(self.path("dalyrejs.seq"), 430))

        # INTCALC / CBACT04C PARM='2022071800': interest and fees per TCATBALF
        # record, written to SYSTRAN and added to the account balances.
        self.load(TCATBALF, tcatbal, "tcatbal.ksds")
        self.load(CARDXREF, xref, "xref.ksds")
        self.load(ACCTDATA, acct, "acct.ksds")
        self.load(DISCGRP, read_lines(DATA / "discgrp.txt", DISCGRP.reclen), "discgrp.ksds")
        self.step("INTCALC", ["CBACT04C", "CBACT04C", INTCALC_PARM], {
            "TCATBALF": "tcatbal.ksds", "XREFFILE": "xref.ksds", "ACCTFILE": "acct.ksds",
            "DISCGRP": "discgrp.ksds", "TRANSACT": "systran.seq"})
        systran = from_fixed(self.path("systran.seq"), 350)
        acct = self.unload(ACCTDATA, "acct.ksds")
        self.save("INTCALC", "systran.txt", systran)
        self.save("INTCALC", "acctdata.txt", acct)

        # COMBTRAN: SORT TRANSACT.BKUP(0) + SYSTRAN(0) on TRAN-ID (1,16,CH,A),
        # then REPRO into the TRANSACT KSDS. The backup is POSTTRAN's TRANSACT.
        combined = sorted(transact + systran, key=lambda r: r[0:16])
        self.load(TRANSACT, combined, "transact.ksds")
        transact = self.unload(TRANSACT, "transact.ksds")
        self.rc["COMBTRAN"] = 0
        self.save("COMBTRAN", "transact.txt", transact)

        # TRANREPT: REPRO TRANSACT to a backup, SORT INCLUDE TRAN-PROC-DT
        # (305,10) within the JCL window on TRAN-CARD-NUM (263,16,ZD,A), then
        # CBTRN03C prints the report.
        daily = sorted((r for r in transact if TRANREPT_START <= r[304:314] <= TRANREPT_END),
                       key=lambda r: r[262:278])
        to_fixed(self.path("tranfile.seq"), daily)
        to_fixed(self.path("dateparm.seq"), [f"{TRANREPT_START} {TRANREPT_END}".ljust(80)])
        self.load(CARDXREF, xref, "xref.ksds")
        self.load(TRANTYPE, read_lines(DATA / "trantype.txt", TRANTYPE.reclen), "trantype.ksds")
        self.load(TRANCATG, read_lines(DATA / "trancatg.txt", TRANCATG.reclen), "trancatg.ksds")
        self.step("TRANREPT", ["CBTRN03C"], {
            "TRANFILE": "tranfile.seq", "CARDXREF": "xref.ksds", "TRANTYPE": "trantype.ksds",
            "TRANCATG": "trancatg.ksds", "DATEPARM": "dateparm.seq", "TRANREPT": "tranrept.seq"})
        self.save("TRANREPT", "transact-daly.txt", daily)
        self.save("TRANREPT", "tranrept.txt", from_fixed(self.path("tranrept.seq"), 133))

        # CREASTMT: SORT TRANSACT on (263,16,CH,A,1,16,CH,A) with
        # OUTREC FIELDS=(1:263,16,17:1,262,279:279,50) into the TRXFL KSDS
        # (the 328-byte OUTREC is space-padded to LRECL 350), then
        # CBSTM03A/CBSTM03B write the plain-text and HTML statements.
        trxfl = [(r[262:278] + r[0:262] + r[278:328]).ljust(350)
                 for r in sorted(transact, key=lambda r: (r[262:278], r[0:16]))]
        self.load(TRXFL, trxfl, "trxfl.ksds")
        self.load(CARDXREF, xref, "xref.ksds")
        self.load(ACCTDATA, acct, "acct.ksds")
        self.load(CUSTDATA, masked_customers(), "cust.ksds")
        self.step("CREASTMT", ["CBSTM03A"], {
            "TRNXFILE": "trxfl.ksds", "XREFFILE": "xref.ksds", "ACCTFILE": "acct.ksds",
            "CUSTFILE": "cust.ksds", "STMTFILE": "statemnt.seq", "HTMLFILE": "statemnt.html.seq"})
        self.save("CREASTMT", "trxfl.txt", self.unload(TRXFL, "trxfl.ksds"))
        self.save("CREASTMT", "statemnt.txt", from_fixed(self.path("statemnt.seq"), 80))
        self.save("CREASTMT", "statemnt.html", from_fixed(self.path("statemnt.html.seq"), 100))

        write_manifest(self.out, self.rc)


def masked_customers() -> list[str]:
    """custdata.txt with names and address lines replaced by placeholders.

    The sample customers are synthetic but read like real people, and the
    statements print them. Placeholders are keyed on CUST-ID and contain
    no space, because CBSTM03A builds the name and city lines with
    STRING ... DELIMITED BY ' ', so each statement stays distinguishable.
    """
    masked = []
    for r in read_lines(DATA / "custdata.txt", CUSTDATA.reclen):
        cid = r[0:9]
        fields = [(f"FIRST-{cid}", 25), (f"MIDDLE-{cid}", 25), (f"LAST-{cid}", 25),
                  (f"{cid} ADDRESS LINE 1", 50), ("ADDRESS LINE 2", 50), (f"CITY-{cid}", 50)]
        masked.append(cid + "".join(v.ljust(n) for v, n in fields) + r[234:])
    return masked


def write_manifest(out: Path, rc: dict[str, int]) -> None:
    lines = ["# CardDemo batch golden master. Generated by carddemo-java/golden-master/run.sh; do not edit.",
             f"# clock={CLOCK} intcalc-parm={INTCALC_PARM} tranrept={TRANREPT_START}..{TRANREPT_END}",
             "", "# job        return-code"]
    lines += [f"{job:<12} {rc[job]}" for job in JOBS]
    lines += ["", "# file                          records  sha256"]
    for f in sorted(p for p in out.rglob("*") if p.is_file() and p.name != "MANIFEST.txt"):
        data = f.read_bytes()
        records = data.count(b"\n")
        lines.append(f"{f.relative_to(out).as_posix():<30} {records:>7}  {hashlib.sha256(data).hexdigest()}")
    (out / "MANIFEST.txt").write_text("\n".join(lines) + "\n")


def tree(root: Path) -> dict[str, bytes]:
    return {p.relative_to(root).as_posix(): p.read_bytes() for p in sorted(root.rglob("*")) if p.is_file()}


def compare(expected: Path, actual: Path, label: str) -> bool:
    a, b = tree(expected), tree(actual)
    ok = True
    for name in sorted(a.keys() | b.keys()):
        if a.get(name) == b.get(name):
            continue
        ok = False
        if name not in b:
            print(f"{label}: missing {name}", file=sys.stderr)
        elif name not in a:
            print(f"{label}: unexpected {name}", file=sys.stderr)
        else:
            diff = difflib.unified_diff(a[name].decode().splitlines(), b[name].decode().splitlines(),
                                        f"expected/{name}", f"actual/{name}", lineterm="")
            print(f"{label}: {name} differs", file=sys.stderr)
            for i, line in enumerate(diff):
                if i == 40:
                    print("  ...", file=sys.stderr)
                    break
                print(f"  {line}", file=sys.stderr)
    return ok


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--check", action="store_true", help="compare with the committed baselines instead of writing them")
    args = parser.parse_args()

    if not shutil.which("cobc"):
        print("cobc (GnuCOBOL 3.1+) not found on PATH", file=sys.stderr)
        return 2
    version = subprocess.run(["cobc", "--version"], capture_output=True, text=True).stdout.splitlines()[0]
    print(f"compiling with {version}", file=sys.stderr)

    shutil.rmtree(WORK, ignore_errors=True)
    try:
        compile_programs(WORK / "bin")
        outs = []
        for n in (1, 2):
            run = Run(WORK / f"run{n}", WORK / "bin", WORK / f"out{n}")
            run.run()
            print(f"run {n}: " + " ".join(f"{j}={run.rc[j]}" for j in JOBS), file=sys.stderr)
            outs.append(run.out)
    except HarnessError as e:
        print(f"error: {e}", file=sys.stderr)
        return 1

    if not compare(outs[0], outs[1], "run 1 vs run 2"):
        print("FAIL: two consecutive runs differ; the chain is not deterministic", file=sys.stderr)
        return 1
    print("two consecutive runs are identical", file=sys.stderr)

    if args.check:
        if not compare(BASELINES, outs[0], "baselines vs run"):
            print("FAIL: output differs from carddemo-java/golden-master/baselines "
                  "(rerun run.sh without --check to regenerate if the change is intended)", file=sys.stderr)
            return 1
        print("OK: output matches the committed baselines", file=sys.stderr)
    else:
        shutil.rmtree(BASELINES, ignore_errors=True)
        shutil.copytree(outs[0], BASELINES)
        print(f"baselines written to {BASELINES.relative_to(REPO)}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())

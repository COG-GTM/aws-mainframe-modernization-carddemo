#!/usr/bin/env python3
"""Compare a Java run of CREASTMT (scripts/batch/run_creastmt.sh) with docs/validation/baseline/CREASTMT.

Checks: the RC; TRXFL.SEQ(+1) and the TRXFL cluster generation record for record against TRXFL.SEQ.txt (312 x 350
bytes); STATEMNT.PS (STMTFILE, 1262 x 80 bytes) identical after trailing-space normalisation; STATEMNT.HTML
(HTMLFILE, 6632 x 100 bytes) identical after whitespace normalisation (runs of blanks collapsed, ends stripped).
Every output must be a whole number of fixed-length records with the baseline's record count; whether the text and
HTML are also byte-identical is reported.

SYSOUT: the baseline ran SORT and IDCAMS through emulators, so STEP010/STEP020 are checked by their record counts
(`DFSORT-EMU ...: n records` / `REPRO LOADED n` vs ICE054I / IDC0005I). CBSTM03A's own lines must be identical, with
one documented substitution: the baseline build had the z/OS PSA/TCB/TIOT walk removed (00-COMPILE/
CBSTM03A.gnucobol.patch) and DISPLAYs `Running JCL : CREASTMT  Step STEP040 (TIOT walk bypassed under GnuCOBOL)`;
the Java program DISPLAYs what the unpatched program shows, `'Running JCL : ' TIOTNJOB ' Step ' TIOTJSTP` with the
job and step names (CBSTM03A.md R-2). The DD-name list the unpatched walk would print is not reproduced (R-2).

--expected-diffs (table mode only): FILE|record|baseline-text|java-text lines (FILE = STMTFILE|HTMLFILE|TRXFL.SEQ|
TRXFL, texts after the same normalisation); each entry must match exactly one differing record or the run fails.

Java run layout (--java-dir): CREASTMT/{rc.txt,sysout.txt,TRXFL.SEQ,TRXFL,STATEMNT.PS,STATEMNT.HTML}.
"""
from __future__ import annotations

import argparse
import difflib
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from compare_posttran import BASELINE, fixed_records, sysout_lines  # noqa: E402

TRXFL_LRECL, STMT_LRECL, HTML_LRECL = 350, 80, 100
COUNTS = {"TRXFL.SEQ": 312, "TRXFL": 312, "STMTFILE": 1262, "HTMLFILE": 6632}
PATCHED_BANNER = "Running JCL : CREASTMT  Step STEP040 (TIOT walk bypassed under GnuCOBOL)"
JAVA_BANNER = "Running JCL : CREASTMT Step STEP040"


def baseline_lines(name: str) -> list[str]:
    return (BASELINE / "CREASTMT" / f"{name}.txt").read_text(encoding="latin-1").splitlines()


def trailing(line: str) -> str:
    return line.rstrip(" ")


def whitespace(line: str) -> str:
    return " ".join(line.split())


def load_expected(path: Path | None) -> dict[tuple[str, int], list]:
    entries: dict[tuple[str, int], list] = {}
    if path is None:
        return entries
    for raw in path.read_text().splitlines():
        if not raw.strip() or raw.startswith("#"):
            continue
        parts = raw.split("|")
        if len(parts) != 4 or parts[0] not in COUNTS:
            raise SystemExit(f"{path}: expected FILE|record|baseline-text|java-text, got {raw!r}")
        entries[(parts[0], int(parts[1]))] = [parts[2], parts[3], 0]
    return entries


class Check:
    def __init__(self, expected: dict):
        self.expected = expected
        self.problems: list[str] = []
        self.notes: list[str] = []
        self.rows: list[tuple[str, str]] = []

    def row(self, what: str, ok: bool, text: str, problem: str | None = None) -> None:
        self.rows.append((what, text if ok else f"DIFF: {text}"))
        if not ok:
            self.problems.append(problem or f"{what}: {text}")

    def records(self, what: str, name: str, base: list[str], java: list[str], lrecl: int, normalise,
                label: str) -> None:
        count = COUNTS[name]
        if len(base) != count or any(len(l) != lrecl for l in base):
            self.problems.append(f"{what}: baseline is not {count} records of {lrecl} bytes")
        nb, nj = [normalise(l) for l in base], [normalise(l) for l in java]
        bad = []
        for i, (b, j) in enumerate(zip(nb, nj), start=1):
            if b == j:
                continue
            entry = self.expected.get((name, i))
            if entry and entry[0] == b and entry[1] == j:
                entry[2] += 1
                self.notes.append(f"{name} record {i}: baseline `{b}` / Java `{j}` (documented table-mode "
                                  "difference)")
            else:
                bad.append(i)
        ok = not bad and len(nb) == len(nj)
        identical = base == java
        text = f"{len(java)} records of {lrecl} bytes, " + (
            ("byte-identical" if identical else f"identical after {label}") if ok
            else f"{len(bad)} differing record(s), counts Java {len(java)} / baseline {len(base)}")
        diff = "\n".join(list(difflib.unified_diff(nb, nj, "baseline", "java", lineterm="", n=1))[:40])
        self.row(what, ok and len(java) == count, text, None if ok else f"{what} differs:\n{diff}")

    def counts(self, what: str, base: list[int], java: list[int]) -> None:
        self.row(what, base == java and bool(base), f"Java {java} / baseline {base}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--java-dir", required=True, type=Path)
    ap.add_argument("--mode", choices=["file", "table"], required=True)
    ap.add_argument("--expected-diffs", type=Path)
    ap.add_argument("--report", type=Path)
    a = ap.parse_args()
    if a.expected_diffs and a.mode != "table":
        raise SystemExit("--expected-diffs applies to table mode only")
    c = Check(load_expected(a.expected_diffs))
    p = c.problems
    run = a.java_dir / "CREASTMT"

    base_rc = (BASELINE / "CREASTMT" / "rc.txt").read_text().strip()
    got_rc = (run / "rc.txt").read_text().strip() if (run / "rc.txt").exists() else "missing"
    c.row("CREASTMT RC", got_rc == base_rc, f"Java {got_rc} / baseline {base_rc}")

    trxfl = baseline_lines("TRXFL.SEQ")
    c.records("STEP010 TRXFL.SEQ(+1)", "TRXFL.SEQ", trxfl,
              fixed_records(run / "TRXFL.SEQ", TRXFL_LRECL, p, "TRXFL.SEQ"), TRXFL_LRECL, lambda l: l, "")
    c.records("STEP020 TRXFL cluster (+1)", "TRXFL", trxfl,
              fixed_records(run / "TRXFL", TRXFL_LRECL, p, "TRXFL"), TRXFL_LRECL, lambda l: l, "")
    c.records("STEP040 STATEMNT.PS (STMTFILE)", "STMTFILE", baseline_lines("STMTFILE"),
              fixed_records(run / "STATEMNT.PS", STMT_LRECL, p, "STATEMNT.PS"), STMT_LRECL, trailing,
              "trailing-space normalisation")
    c.records("STEP040 STATEMNT.HTML (HTMLFILE)", "HTMLFILE", baseline_lines("HTMLFILE"),
              fixed_records(run / "STATEMNT.HTML", HTML_LRECL, p, "STATEMNT.HTML"), HTML_LRECL, whitespace,
              "whitespace normalisation")

    base = [l.rstrip() for l in (BASELINE / "CREASTMT" / "sysout.txt").read_text(encoding="latin-1").splitlines()]
    java = sysout_lines(run / "sysout.txt")
    c.counts("STEP010 SORT in/out", [int(n) for l in base if "DFSORT-EMU" in l
                                     for n in re.findall(r": (\d+) records", l)] * 2,
             [int(n) for m in re.findall(r"ICE054I 0 RECORDS - IN: (\d+), OUT: (\d+)", "\n".join(java)) for n in m])
    c.counts("STEP020 REPRO count", [int(n) for l in base for n in re.findall(r"REPRO LOADED (\d+) RECORDS", l)],
             [int(n) for l in java for n in re.findall(r"IDC0005I NUMBER OF RECORDS PROCESSED WAS (\d+)", l)])
    start = base.index("--- STEP040 EXEC PGM=CBSTM03A")
    base_prog = [l for l in base[start + 1:] if not re.match(r"^(rc=-?\d+|libcob: )", l)]
    if PATCHED_BANNER in base_prog:
        base_prog[base_prog.index(PATCHED_BANNER)] = JAVA_BANNER
        c.notes.append(f"CBSTM03A SYSOUT: baseline `{PATCHED_BANNER}` (build-time TIOT patch) / Java `{JAVA_BANNER}`"
                       " (CBSTM03A.md R-2)")
    else:
        p.append("baseline CBSTM03A SYSOUT lacks the patched TIOT banner")
    java_prog = java[java.index("--- STEP040") + 1:] if "--- STEP040" in java else []
    c.row("CBSTM03A SYSOUT", base_prog == java_prog, f"{len(java_prog)} lines" + (
        ", identical" if base_prog == java_prog else ""), None if base_prog == java_prog else
        "CBSTM03A SYSOUT differs:\n" + "\n".join(difflib.unified_diff(base_prog, java_prog, "baseline", "java",
                                                                      lineterm="", n=1)))

    for (name, rec), (b, j, hits) in c.expected.items():
        if hits != 1:
            p.append(f"expected diff {name} record {rec} `{b}`->`{j}` applied {hits} times")

    report = [f"## CREASTMT ({a.mode} mode) vs GnuCOBOL baseline", "", "| check | result |", "|---|---|"]
    report += [f"| {k} | {v} |" for k, v in c.rows]
    if c.notes:
        report += ["", "Documented differences:", ""] + [f"- {n}" for n in c.notes]
    report += ["", "**RESULT: " + ("PASS" if not p else f"FAIL ({len(p)} problem(s))") + "**", ""]
    text = "\n".join(report)
    print(text)
    for problem in p:
        print(problem, file=sys.stderr)
    if a.report:
        a.report.parent.mkdir(parents=True, exist_ok=True)
        a.report.write_text(text + ("\n```\n" + "\n\n".join(p) + "\n```\n" if p else ""))
    return 0 if not p else 1


if __name__ == "__main__":
    sys.exit(main())

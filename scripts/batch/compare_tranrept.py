#!/usr/bin/env python3
"""Compare a Java run of TRANBKP, COMBTRAN, TRANREPT and PRTCATBL (scripts/batch/run_tranrept.sh) with
docs/validation/baseline/{TRANBKP,COMBTRAN,TRANREPT,PRTCATBL}.

Per job: the RC; every GDG output record by record (TRANSACT layouts field by field); the KSDS after-image in key
order with its row count; the SYSOUT. The GnuCOBOL baseline ran IDCAMS and DFSORT through emulators whose SYSOUT
lines are not program output, so the utility steps are checked by their record counts (`REPRO UNLOADED n` /
`LOADED n` / `SORTIN ... (n)` / `OUTREC: n records` vs the Java IDC0005I / ICE054I messages); program SYSOUT
(CBTRN03C) must be identical. TRANREPT report lines must be identical after trailing-space normalisation (and every
record must be 133 bytes); PRTCATBL's report records must be identical and 41 bytes.

--mode file: every byte must match, FILLER included.
--mode table: FILLER is not persisted (ADR-0011): FILLER-only differences are counted and reported, not failed;
other differences must be listed in --expected-diffs (DATASET|record|FIELD|baseline-text|java-text, each applied
exactly once).

Java run layout (--java-dir): <JOB>/{rc.txt,sysout.txt,<GDG>...,<DATASET>.ksds}; GDG outputs are the fixed-length
dated generations, the .ksds files one record per line.
"""
from __future__ import annotations

import argparse
import difflib
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from compare_posttran import (BASELINE, compare_records, fixed_records, line_records, load_expected,  # noqa: E402
                              sysout_lines)

REPORT_LRECL = 133
CATBAL_REPT_LRECL = 41
JAVA_UTILITY = re.compile(r"^(--- STEP|IDC|ICE)")


def raw_baseline_sysout(job: str) -> list[str]:
    """The baseline SYSOUT without the runner's after-image unloads (`REPRO ... OUTFILE(_after_<JOB>_...)`)."""
    out: list[str] = []
    skip = False
    for line in (BASELINE / job / "sysout.txt").read_text(encoding="latin-1").splitlines():
        if line.startswith("--- "):
            skip = "OUTFILE(_after_" in line
        if not skip:
            out.append(line.rstrip())
    return out


def baseline_lines(job: str, name: str) -> list[str]:
    return (BASELINE / job / f"{name}.txt").read_text(encoding="latin-1").splitlines()


def counts(pattern: str, lines: list[str]) -> list[int]:
    return [int(n) for l in lines for n in re.findall(pattern, l)]


class Check:
    def __init__(self, mode: str, expected: dict):
        self.mode, self.expected = mode, expected
        self.problems: list[str] = []
        self.notes: list[str] = []
        self.rows: list[tuple[str, str]] = []

    def row(self, what: str, ok: bool, text: str, problem: str | None = None) -> None:
        self.rows.append((what, text if ok else f"DIFF: {text}"))
        if not ok:
            self.problems.append(problem or f"{what}: {text}")

    def rc(self, run: Path, job: str) -> None:
        base = (BASELINE / job / "rc.txt").read_text().strip() if (BASELINE / job / "rc.txt").exists() else "0"
        got = (run / "rc.txt").read_text().strip() if (run / "rc.txt").exists() else "missing"
        self.row(f"{job} RC", got == base, f"Java {got} / baseline {base}")

    def records(self, what: str, dataset: str, base: list[str], java: list[str], count: int) -> None:
        if len(base) != count:
            self.problems.append(f"{what}: baseline has {len(base)} records, expected {count}")
        result = compare_records(dataset, base, java, self.mode, self.expected, self.problems, self.notes)
        self.rows.append((what, result if "match" in result and len(java) == count else f"DIFF: {result}"))

    def counts(self, what: str, base: list[int], java: list[int]) -> None:
        self.row(what, base == java and bool(base), f"Java {java} / baseline {base}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--java-dir", required=True, type=Path)
    ap.add_argument("--mode", choices=["file", "table"], required=True)
    ap.add_argument("--expected-diffs", type=Path)
    ap.add_argument("--report", type=Path)
    a = ap.parse_args()
    c = Check(a.mode, load_expected(a.expected_diffs))
    p = c.problems

    # TRANBKP: REPRO TRANSACT -> TRANSACT.BKUP(+1), DELETE + DEFINE TRANSACT (empty)
    run = a.java_dir / "TRANBKP"
    c.rc(run, "TRANBKP")
    c.records("TRANBKP TRANSACT.BKUP(+1)", "TRANSACT", baseline_lines("TRANBKP", "TRANSACT.BKUP"),
              fixed_records(run / "TRANSACT.BKUP", 350, p, "TRANBKP TRANSACT.BKUP"), 262)
    after = line_records(run / "TRANSACT.ksds", p, "TRANBKP TRANSACT")
    c.row("TRANBKP TRANSACT after-image rows", len(after) == len(baseline_lines("TRANBKP", "TRANSACT.ksds")) == 0,
          f"Java {len(after)} / baseline {len(baseline_lines('TRANBKP', 'TRANSACT.ksds'))}")
    java = sysout_lines(run / "sysout.txt")
    c.counts("TRANBKP REPRO count", counts(r"REPRO UNLOADED (\d+) RECORDS", raw_baseline_sysout("TRANBKP")),
             counts(r"IDC0005I NUMBER OF RECORDS PROCESSED WAS (\d+)", java))

    # COMBTRAN: SORT BKUP(0)+SYSTRAN(0) -> TRANSACT.COMBINED(+1), REPRO into TRANSACT
    run = a.java_dir / "COMBTRAN"
    c.rc(run, "COMBTRAN")
    c.records("COMBTRAN TRANSACT.COMBINED(+1)", "TRANSACT", baseline_lines("COMBTRAN", "TRANSACT.COMBINED"),
              fixed_records(run / "TRANSACT.COMBINED", 350, p, "COMBTRAN TRANSACT.COMBINED"), 312)
    c.records("COMBTRAN TRANSACT after-image", "TRANSACT", baseline_lines("COMBTRAN", "TRANSACT.ksds"),
              line_records(run / "TRANSACT.ksds", p, "COMBTRAN TRANSACT"), 312)
    base = raw_baseline_sysout("COMBTRAN")
    java = sysout_lines(run / "sysout.txt")
    sortin = counts(r"\((\d+)\)", [l for l in base if "DFSORT-EMU" in l])
    c.counts("COMBTRAN SORT in/out", [sum(sortin)] * 2,
             [int(n) for m in re.findall(r"ICE054I 0 RECORDS - IN: (\d+), OUT: (\d+)", "\n".join(java))
              for n in m])
    c.counts("COMBTRAN REPRO count", counts(r"REPRO LOADED (\d+) RECORDS", base),
             counts(r"IDC0005I NUMBER OF RECORDS PROCESSED WAS (\d+)", java))

    # TRANREPT: REPROC -> TRANSACT.BKUP(+1), SORT INCLUDE -> TRANSACT.DALY(+1), CBTRN03C -> TRANREPT(+1)
    run = a.java_dir / "TRANREPT"
    c.rc(run, "TRANREPT")
    c.records("TRANREPT TRANSACT.BKUP(+1)", "TRANSACT", baseline_lines("TRANREPT", "TRANSACT.BKUP"),
              fixed_records(run / "TRANSACT.BKUP", 350, p, "TRANREPT TRANSACT.BKUP"), 312)
    c.records("TRANREPT TRANSACT.DALY(+1)", "TRANSACT", baseline_lines("TRANREPT", "TRANSACT.DALY"),
              fixed_records(run / "TRANSACT.DALY", 350, p, "TRANREPT TRANSACT.DALY"), 312)
    base_rep = [l.rstrip() for l in baseline_lines("TRANREPT", "TRANREPT")]
    java_raw = fixed_records(run / "TRANREPT", REPORT_LRECL, p, "TRANREPT report")
    java_rep = [l.rstrip() for l in java_raw]
    if base_rep == java_rep:
        c.rows.append(("TRANREPT report (CBTRN03C)", f"{len(java_rep)} lines of {REPORT_LRECL} bytes, identical "
                                                     "after trailing-space normalisation"))
    else:
        diff = list(difflib.unified_diff(base_rep, java_rep, "baseline", "java", lineterm="", n=1))
        c.row("TRANREPT report (CBTRN03C)", False, f"{len(java_rep)} vs {len(base_rep)} lines",
              "TRANREPT report differs:\n" + "\n".join(diff[:40]))
    after = line_records(run / "TRANSACT.ksds", p, "TRANREPT TRANSACT")
    c.records("TRANREPT TRANSACT after-image (unchanged)", "TRANSACT", baseline_lines("COMBTRAN", "TRANSACT.ksds"),
              after, 312)
    base = raw_baseline_sysout("TRANREPT")
    java = sysout_lines(run / "sysout.txt")
    start = base.index("--- STEP15 EXEC PGM=CBTRN03C")
    base_prog = [l for l in base[start + 1:] if not re.match(r"^(rc=-?\d+|libcob: )", l)]
    java_prog = java[java.index("--- STEP15") + 1:] if "--- STEP15" in java else []
    if base_prog == java_prog:
        c.rows.append(("CBTRN03C SYSOUT", f"{len(java_prog)} lines, identical"))
    else:
        diff = list(difflib.unified_diff(base_prog, java_prog, "baseline", "java", lineterm="", n=1))
        c.row("CBTRN03C SYSOUT", False, "differs", "CBTRN03C SYSOUT differs:\n" + "\n".join(diff[:40]))
    c.counts("TRANREPT REPRO count", counts(r"REPRO UNLOADED (\d+) RECORDS", base),
             counts(r"IDC0005I NUMBER OF RECORDS PROCESSED WAS (\d+)", java))

    # PRTCATBL: REPROC TCATBALF -> TCATBALF.BKUP(+1), SORT + OUTREC -> TCATBALF.REPT(+1)
    run = a.java_dir / "PRTCATBL"
    c.rc(run, "PRTCATBL")
    c.records("PRTCATBL TCATBALF.BKUP(+1)", "TCATBALF", baseline_lines("PRTCATBL", "TCATBALF.BKUP"),
              fixed_records(run / "TCATBALF.BKUP", 50, p, "PRTCATBL TCATBALF.BKUP"), 100)
    base_rept = baseline_lines("PRTCATBL", "TCATBALF.REPT")
    java_rept = fixed_records(run / "TCATBALF.REPT", CATBAL_REPT_LRECL, p, "PRTCATBL TCATBALF.REPT")
    ok = base_rept == java_rept and all(len(l) == CATBAL_REPT_LRECL for l in base_rept)
    c.row("PRTCATBL TCATBALF.REPT(+1)", ok, f"{len(java_rept)} records of {CATBAL_REPT_LRECL} bytes"
          + (", identical" if ok else ""), None if ok else "PRTCATBL report differs:\n" + "\n".join(
              difflib.unified_diff(base_rept, java_rept, "baseline", "java", lineterm="", n=1)))
    base = raw_baseline_sysout("PRTCATBL")
    java = sysout_lines(run / "sysout.txt")
    base_prog = [l for l in base if not re.match(r"^(--- |IDCAMS-EMU |rc=-?\d+$|libcob: )", l)]
    java_prog = [l for l in java if not JAVA_UTILITY.match(l)]
    c.row("PRTCATBL SYSOUT (program lines)", base_prog == java_prog,
          f"Java {len(java_prog)} / baseline {len(base_prog)} lines" + (", identical" if base_prog == java_prog
                                                                        else ""))
    c.counts("PRTCATBL REPRO count", counts(r"REPRO UNLOADED (\d+) RECORDS", base),
             counts(r"IDC0005I NUMBER OF RECORDS PROCESSED WAS (\d+)", java))
    c.counts("PRTCATBL SORT/OUTREC count", counts(r"OUTREC: (\d+) records", base),
             counts(r"ICE054I 0 RECORDS - IN: \d+, OUT: (\d+)", java))

    for (ds, rec, field), (b, j, hits) in c.expected.items():
        if hits != 1:
            p.append(f"expected diff {ds} record {rec} {field} `{b}`->`{j}` applied {hits} times")

    report = [f"## TRANBKP / COMBTRAN / TRANREPT / PRTCATBL ({a.mode} mode) vs GnuCOBOL baseline", "",
              "| check | result |", "|---|---|"]
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

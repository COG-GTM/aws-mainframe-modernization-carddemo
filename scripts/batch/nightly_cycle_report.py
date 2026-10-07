#!/usr/bin/env python3
"""Job x mode x result matrix of a nightly-cycle run (scripts/batch/run_nightly_cycle.sh).

Per mode (``--out <dir> --mode file|table --cycle-rc N``): reads the per-group compare reports in
``<dir>/reports/<group>.md`` (written by the existing compare_*.py scripts), the member RCs (``<dir>/<JOB>/rc.txt``)
and the batch_run rows of the cycle (``<dir>/batch_run.txt``, ``<dir>/batch_run_children.txt``), and writes
``<dir>/REPORT.md`` and ``<dir>/matrix.json``. Exit code 0 only when every compare passed, every member ran with
the baseline RC, and batch_run holds the cycle job row, one row per member step and the child jobs.

``--combine <dir> <dir> ... --report <file>`` merges the matrix.json of several modes into one table.
"""
from __future__ import annotations

import argparse
import os
import json
import re
import sys
from pathlib import Path

BASELINE = Path(os.environ.get("CARDDEMO_BASELINE_DIR") or "docs/validation/baseline")
JOBS = ["READACCT", "READCARD", "READCUST", "READXREF", "POSTTRAN", "INTCALC", "TRANBKP", "COMBTRAN", "TRANREPT",
        "CREASTMT", "PRTCATBL"]
GROUP = {"READACCT": "print", "READCARD": "print", "READCUST": "print", "READXREF": "print", "POSTTRAN": "posttran",
         "INTCALC": "intcalc", "TRANBKP": "tranrept", "COMBTRAN": "tranrept", "TRANREPT": "tranrept",
         "PRTCATBL": "tranrept", "CREASTMT": "creastmt"}
ALIASES = {"CBTRN01C": "POSTTRAN"}
NOT_RUN = [("CLOSEFIL / OPENFIL / WAITSTEP", "retired (06-scheduling.md)"),
           ("CBPAUP0J", "out of scope (IMS/DB2 authorization extension)"),
           ("TXT2PDF1", "retired (STATEMNT.HTML already produced by CREASTMT)"),
           ("TRANTYPE / TRANCATG / TCATBALF / DISCGRP (refresh loads)", "initial-load / repro (not nightly)"),
           ("TRANEXTR / MNTTRDB2", "out of scope (Db2 extension)")]
FAILED = re.compile(r"\b(DIFF|DIFFERS|MISSING|UNKNOWN)\b")


def read(path: Path) -> str:
    return path.read_text().strip() if path.exists() else ""


def group_rows(report: str, default: str) -> dict[str, list[tuple[str, str]]]:
    """Rows of a compare report table, keyed by the job each row is about."""
    rows: dict[str, list[tuple[str, str]]] = {}
    for line in report.splitlines():
        if not line.startswith("| ") or line.startswith("| check") or line.startswith("| job "):
            continue
        cells = [c.strip() for c in line.strip("|").split("|")]
        if len(cells) == 3:
            job, check, result = cells[0], f"{cells[0]} {cells[1]}", cells[2]
        else:
            check, result = cells[0], cells[-1]
            first = check.split()[0]
            job = ALIASES.get(first, first if first in GROUP else default)
        rows.setdefault(job, []).append((check, result))
    return rows


def table(path: Path) -> list[list[str]]:
    return [line.split("|") for line in read(path).splitlines() if line]


def per_mode(out: Path, mode: str, cycle_rc: int) -> int:
    problems: list[str] = []
    groups = sorted(set(GROUP.values()))
    reports = {g: read(out / "reports" / f"{g}.md") for g in groups}
    passed = {g: read(out / "reports" / f"{g}.rc") == "0" for g in groups}
    rows = {g: group_rows(reports[g], next(j for j, gg in GROUP.items() if gg == g)) for g in groups}
    steps = {r[0]: r for r in table(out / "batch_run.txt")}
    children: dict[str, list[list[str]]] = {}
    for r in table(out / "batch_run_children.txt"):
        children.setdefault(r[-1], []).append(r)
    cycle_row = steps.get("-")
    if not cycle_row:
        problems.append("batch_run: no nightly-cycle job row")
    elif int(cycle_row[3]) != cycle_rc:
        problems.append(f"batch_run: nightly-cycle RC {cycle_row[3]} but the process exit code was {cycle_rc}")

    matrix = []
    max_base = 0
    for job in JOBS:
        base_rc = int(read(BASELINE / job / "rc.txt") or 0)
        max_base = max(max_base, base_rc)
        java_rc = read(out / job / "rc.txt") or "-"
        step = steps.get(job)
        if not step:
            problems.append(f"{job}: no batch_run step row")
            batch_run = "missing"
        else:
            batch_run = f"{step[2]} RC {step[3]}, read {step[4]} / write {step[5]}"
            if step[3] != java_rc:
                problems.append(f"{job}: batch_run RC {step[3]} vs log RC {java_rc}")
        kids = children.get(job, [])
        if not kids:
            problems.append(f"{job}: no child job rows in batch_run")
        job_rows = rows[GROUP[job]].get(job, [])
        failing = [c for c, r in job_rows if FAILED.search(r)]
        rc_ok = java_rc == str(base_rc)
        if not rc_ok:
            problems.append(f"{job}: RC {java_rc}, baseline {base_rc}")
        ok = rc_ok and not failing and bool(job_rows) and passed[GROUP[job]]
        if not job_rows:
            problems.append(f"{job}: no compare rows")
        result = "PASS" if ok else ("FAIL: " + ", ".join(failing) if failing else f"FAIL (see {GROUP[job]} report)")
        matrix.append(dict(job=job, mode=mode, java_rc=java_rc, baseline_rc=base_rc, batch_run=batch_run,
                           children=len(kids), checks=len(job_rows), result=result))
    if cycle_rc != max_base:
        problems.append(f"nightly-cycle RC {cycle_rc}, expected JCL max {max_base}")
    for g in groups:
        if not passed[g]:
            problems.append(f"compare group {g} failed (reports/{g}.md)")

    lines = [f"## nightly-cycle ({mode} mode): one launch vs GnuCOBOL baseline", "",
             "`java -jar carddemo-app.jar --job=nightly-cycle --run-date=2022-07-06` on freshly loaded sample data; "
             "every job reads the previous Java job's outputs.", "",
             f"Cycle RC (JCL max): {cycle_rc} (expected {max_base}); batch_run cycle row: "
             + (f"{cycle_row[1]} {cycle_row[2]} RC {cycle_row[3]}" if cycle_row else "missing"), "",
             "| job | mode | Java RC / baseline | batch_run step | child jobs | checks | result |",
             "|---|---|---|---|---|---|---|"]
    lines += [f"| {m['job']} | {mode} | {m['java_rc']} / {m['baseline_rc']} | {m['batch_run']} | {m['children']} "
              f"| {m['checks']} | {m['result']} |" for m in matrix]
    lines += ["", "Not run by the cycle:", ""] + [f"- {j}: {why}" for j, why in NOT_RUN]
    lines += ["", f"**RESULT: {'PASS' if not problems else f'FAIL ({len(problems)} problem(s))'}**", ""]
    if problems:
        lines += ["```", *problems, "```", ""]
    lines += ["<details><summary>Per-group compare reports</summary>", ""]
    for g in groups:
        lines += [reports[g] or f"(no {g} report)", ""]
    lines += ["</details>", ""]
    text = "\n".join(lines)
    (out / "REPORT.md").write_text(text)
    (out / "matrix.json").write_text(json.dumps(dict(mode=mode, cycle_rc=cycle_rc, rows=matrix,
                                                     problems=problems), indent=2))
    print("\n".join(lines[: lines.index("<details><summary>Per-group compare reports</summary>")]))
    return 0 if not problems else 1


def combine(dirs: list[Path], report: Path) -> int:
    runs = [json.loads((d / "matrix.json").read_text()) for d in dirs if (d / "matrix.json").exists()]
    modes = [r["mode"] for r in runs]
    lines = ["## nightly-cycle: job x mode x result (gate g-batch)", "",
             "| job | " + " | ".join(modes) + " |", "|---|" + "---|" * len(modes)]
    for i, job in enumerate(JOBS):
        lines.append(f"| {job} | " + " | ".join(r["rows"][i]["result"] for r in runs) + " |")
    lines.append("| **cycle RC** | " + " | ".join(str(r["cycle_rc"]) for r in runs) + " |")
    ok = len(runs) == len(dirs) and all(not r["problems"] for r in runs)
    lines += ["", f"**RESULT: {'PASS' if ok else 'FAIL'}**", ""]
    report.parent.mkdir(parents=True, exist_ok=True)
    report.write_text("\n".join(lines))
    print("\n".join(lines))
    return 0 if ok else 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", type=Path)
    ap.add_argument("--mode", choices=["file", "table"])
    ap.add_argument("--cycle-rc", type=int)
    ap.add_argument("--combine", nargs="+", type=Path)
    ap.add_argument("--report", type=Path)
    a = ap.parse_args()
    if a.combine:
        return combine(a.combine, a.report)
    return per_mode(a.out, a.mode, a.cycle_rc)


if __name__ == "__main__":
    sys.exit(main())

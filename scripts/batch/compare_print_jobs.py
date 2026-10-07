#!/usr/bin/env python3
"""Compare Java batch job outputs with the GnuCOBOL baseline in docs/validation/baseline/<JOB>/.

Layout of --java-dir (written by scripts/batch/run_print_jobs.sh):
  <JOB>/sysout.txt   the job's SYSOUT DD (DISPLAY lines)
  <JOB>/rc.txt       the CLI process exit code (= JCL condition code)
  <JOB>/<DD>         every dataset the baseline has a <DD>.txt for (fixed-width or RECFM=V)

Rules (UNT51-11 acceptance):
  * SYSOUT: the baseline minus its harness lines ("--- EXEC ...", "libcob: ..." runtime warnings, "rc=N") must equal
    the Java SYSOUT line for line after trailing-space normalisation. Non-printable bytes render as \\xNN on both
    sides (scripts/baseline/baseline.py render_sysout).
  * rc.txt must match.
  * Datasets are compared byte-level: rendered with the baseline's own fold()/fold_varseq0() (LRECL from job.md)
    and compared exactly, no normalisation.
  * --expected-diffs lists known input-data differences ("JOB|FILE|line-prefix|baseline-text|java-text"): on the
    single baseline line starting with line-prefix, baseline-text is replaced by java-text before comparing. Each
    entry must apply exactly once, so a stale entry fails the run.

Exit status 0 when every job matches, 1 otherwise. --report writes the summary as Markdown.
"""
from __future__ import annotations

import argparse
import os
import difflib
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "scripts" / "baseline"))
from baseline import fold, fold_varseq0, render_record, render_sysout  # noqa: E402

BASELINE = Path(os.environ.get("CARDDEMO_BASELINE_DIR") or REPO / "docs" / "validation" / "baseline")
DEFAULT_JOBS = ["READACCT", "READCARD", "READXREF", "READCUST"]
HARNESS_LINE = re.compile(r"^(--- EXEC |libcob: |rc=-?\d+$)")
FIXED = re.compile(r"`(\w+)\.txt`: \d+ records x LRECL (\d+)")
VARIABLE = re.compile(r"`(\w+)\.txt`: \d+ variable records")


def fold_rdw(path: Path) -> tuple[str, int]:
    """z/OS RECFM=V: 4-byte RDW (2-byte big-endian length including the RDW + 2 NUL) + data, rendered like
    fold_varseq0 (data length|data)."""
    data = path.read_bytes() if path.exists() else b""
    out, n, i = [], 0, 0
    while i + 4 <= len(data):
        ln = int.from_bytes(data[i:i + 2], "big") - 4
        out.append("%05d|%s\n" % (ln, render_record(data[i + 4:i + 4 + ln])))
        i += 4 + ln
        n += 1
    return "".join(out), n


def load_expected(path: Path | None) -> dict[tuple[str, str], list[list]]:
    entries: dict[tuple[str, str], list[list]] = {}
    if path is None:
        return entries
    for raw in path.read_text().splitlines():
        if not raw.strip() or raw.startswith("#"):
            continue
        parts = raw.split("|")
        if len(parts) != 5:
            raise SystemExit(f"{path}: expected JOB|FILE|line-prefix|baseline-text|java-text, got {raw!r}")
        job, name, prefix, base_text, java_text = parts
        entries.setdefault((job, name), []).append([prefix, base_text, java_text, 0])
    return entries


def apply_expected(lines: list[str], entries: list[list], notes: list[str], job: str, name: str) -> list[str]:
    out = []
    for line in lines:
        for e in entries:
            prefix, base_text, java_text = e[0], e[1], e[2]
            if line.startswith(prefix) and base_text in line:
                line = line.replace(base_text, java_text, 1)
                e[3] += 1
        out.append(line)
    for prefix, base_text, java_text, hits in entries:
        state = "applied" if hits == 1 else f"applied {hits} times (expected once)"
        notes.append(f"{job}/{name}: expected diff on line `{prefix}...` `{base_text}` -> `{java_text}`: {state}")
    return out


def compare_lines(job: str, name: str, base: list[str], java: list[str], problems: list[str]) -> bool:
    if base == java:
        return True
    diff = list(difflib.unified_diff(base, java, f"baseline/{job}/{name}", f"java/{job}/{name}", lineterm="", n=1))
    problems.append(f"{job}/{name}: {len(base)} baseline vs {len(java)} java lines differ\n"
                    + "\n".join(diff[:40]) + ("\n..." if len(diff) > 40 else ""))
    return False


def compare_job(job: str, java_dir: Path, vb_format: str, expected: dict, problems: list[str],
                notes: list[str]) -> list[tuple[str, str, str]]:
    base_dir, out_dir = BASELINE / job, java_dir / job
    rows = []
    if not base_dir.is_dir():
        problems.append(f"{job}: no baseline directory {base_dir}")
        return [(job, "-", "MISSING BASELINE")]

    base_sysout = [line.rstrip(" ") for line in render_sysout((base_dir / "sysout.txt").read_bytes()).splitlines()
                   if not HARNESS_LINE.match(line)]
    base_sysout = [line.rstrip(" ") for line in
                   apply_expected(base_sysout, expected.get((job, "sysout"), []), notes, job, "sysout")]
    java_path = out_dir / "sysout.txt"
    if not java_path.exists():
        problems.append(f"{job}/sysout: {java_path} missing")
        rows.append((job, "SYSOUT", "MISSING"))
    else:
        java_sysout = [line.rstrip(" ") for line in render_sysout(java_path.read_bytes()).splitlines()]
        ok = compare_lines(job, "sysout", base_sysout, java_sysout, problems)
        rows.append((job, "SYSOUT", f"{'identical' if ok else 'DIFFERS'} ({len(java_sysout)} lines, "
                                    "trailing spaces normalised)"))

    base_rc = (base_dir / "rc.txt").read_text().strip()
    java_rc_path = out_dir / "rc.txt"
    java_rc = java_rc_path.read_text().strip() if java_rc_path.exists() else "missing"
    if java_rc != base_rc:
        problems.append(f"{job}/rc: baseline {base_rc}, java {java_rc}")
    rows.append((job, "RC", f"{'identical' if java_rc == base_rc else 'DIFFERS'} (baseline {base_rc}, java {java_rc})"))

    job_md = (base_dir / "job.md").read_text() if (base_dir / "job.md").exists() else ""
    lrecls = {m.group(1): int(m.group(2)) for m in FIXED.finditer(job_md)}
    variables = {m.group(1) for m in VARIABLE.finditer(job_md)}
    for txt in sorted(base_dir.glob("*.txt")):
        dd = txt.stem
        if dd in ("sysout", "rc"):
            continue
        java_file = out_dir / dd
        if dd in lrecls:
            java_text, count, rest = fold(java_file, lrecls[dd])
            if rest:
                problems.append(f"{job}/{dd}: size not a multiple of LRECL {lrecls[dd]}")
            kind = f"LRECL {lrecls[dd]}"
        elif dd in variables:
            java_text, count = {"varseq0": fold_varseq0, "rdw": fold_rdw}[vb_format](java_file)
            kind = f"RECFM=V ({vb_format})"
        else:
            problems.append(f"{job}/{dd}: job.md does not describe {txt.name}")
            rows.append((job, dd, "UNKNOWN FORMAT"))
            continue
        if not java_file.exists():
            problems.append(f"{job}/{dd}: {java_file} missing")
            rows.append((job, dd, "MISSING"))
            continue
        base_lines = apply_expected(txt.read_text().splitlines(), expected.get((job, dd), []), notes, job, dd)
        ok = compare_lines(job, dd, base_lines, java_text.splitlines(), problems)
        rows.append((job, dd, f"{'identical' if ok else 'DIFFERS'} ({count} records, {kind}, byte-level)"))
    return rows


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--java-dir", required=True, type=Path)
    ap.add_argument("--jobs", default=",".join(DEFAULT_JOBS))
    ap.add_argument("--vb-format", choices=["varseq0", "rdw"], default="varseq0",
                    help="record prefix of the Java RECFM=V outputs (--record-prefix GNUCOBOL_VARSEQ_0 / ZOS_RDW)")
    ap.add_argument("--expected-diffs", type=Path)
    ap.add_argument("--title", default="Java batch vs GnuCOBOL baseline")
    ap.add_argument("--report", type=Path)
    a = ap.parse_args()

    expected = load_expected(a.expected_diffs)
    problems: list[str] = []
    notes: list[str] = []
    rows = []
    for job in [j.strip() for j in a.jobs.split(",") if j.strip()]:
        rows += compare_job(job, a.java_dir, a.vb_format, expected, problems, notes)
    for (job, name), entries in expected.items():
        if job not in a.jobs.split(","):
            continue
        for prefix, base_text, java_text, hits in entries:
            if hits != 1:
                problems.append(f"{job}/{name}: expected diff `{prefix}...` `{base_text}` applied {hits} times")

    report = [f"## {a.title}", "", "| job | output | result |", "|---|---|---|"]
    report += [f"| {job} | {name} | {result} |" for job, name, result in rows]
    if notes:
        report += ["", "Known input-data differences (see --expected-diffs):", ""] + [f"- {n}" for n in notes]
    report += ["", "**RESULT: " + ("PASS" if not problems else f"FAIL ({len(problems)} problem(s))") + "**", ""]
    text = "\n".join(report)
    print(text)
    for p in problems:
        print(p, file=sys.stderr)
    if a.report:
        a.report.parent.mkdir(parents=True, exist_ok=True)
        a.report.write_text(text + ("\n```\n" + "\n\n".join(problems) + "\n```\n" if problems else ""))
    return 0 if not problems else 1


if __name__ == "__main__":
    sys.exit(main())

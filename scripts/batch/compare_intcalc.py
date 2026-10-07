#!/usr/bin/env python3
"""Compare a Java INTCALC run (scripts/batch/run_intcalc.sh) with docs/validation/baseline/INTCALC.

Checks, in order: the CBACT04C SYSOUT, the job RC, SYSTRAN (the TRANSACT DD, SYSTRAN(+1)) record by record and field
by field, the ACCTDATA after-image against INTCALC/ACCTDATA.ksds.txt and the TCATBALF after-image (opened INPUT, so
it must still equal the POSTTRAN after-image INTCALC started from), field by field in key order. Also reports whether
the known EBCDIC-vs-ASCII DISCGRP input difference (record 34) is on any lookup path of this TCATBALF.

--mode file: every byte must match, FILLER included.
--mode table: FILLER is not persisted (ADR-0011): FILLER-only differences in the after-images and in the DISPLAYed
TCATBALF images of the SYSOUT are counted and reported, not failed; every other field must match except the
documented input-data differences listed in --expected-diffs (DATASET|record-number|FIELD|baseline-text|java-text,
each must apply exactly once).

Java run layout (--java-dir): INTCALC/{sysout.txt,rc.txt,SYSTRAN,ACCTDATA.ksds,TCATBALF.ksds}; SYSTRAN is the
fixed-length dated generation, the .ksds files one full-length record per line.
"""
from __future__ import annotations

import argparse
import difflib
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from compare_posttran import (BASELINE, REPO, baseline_sysout, compare_records, fixed_records,  # noqa: E402
                              line_records, load_expected, sysout_lines)

DRIVER_LINE = re.compile(r"^RUNCB04: ")  # the baseline's PARM-passing driver (scripts/baseline), not CBACT04C
TCATBAL_IMAGE = re.compile(r"^(\d{17}.{11})(0{22})$")
DISCGRP_REC = 34


def intcalc_baseline_sysout() -> list[str]:
    return [l for l in baseline_sysout("INTCALC") if not DRIVER_LINE.match(l)]


def discgrp_reach() -> tuple[str, int]:
    """Key of DISCGRP record 34 and how many CBACT04C reads of this TCATBALF use it (specific or DEFAULT)."""
    rec = (REPO / "app/data/ASCII/discgrp.txt").read_text(encoding="latin-1").splitlines()[DISCGRP_REC - 1]
    key = (rec[0:10], rec[10:12], rec[12:16])
    groups = {l[0:11]: l[112:122] for l in
              (BASELINE / "POSTTRAN" / "ACCTDATA.ksds.txt").read_text(encoding="latin-1").splitlines()}
    discgrp = {(l[0:10], l[10:12], l[12:16]) for l in
               (REPO / "app/data/ASCII/discgrp.txt").read_text(encoding="latin-1").splitlines()}
    hits = 0
    for row in (BASELINE / "POSTTRAN" / "TCATBALF.ksds.txt").read_text(encoding="latin-1").splitlines():
        specific = (groups[row[0:11]], row[11:13], row[13:17])
        used = specific if specific in discgrp else ("DEFAULT   ", row[11:13], row[13:17])
        hits += used == key
    return f"{key[0].rstrip()}/{key[1]}/{key[2]}", hits


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--java-dir", required=True, type=Path)
    ap.add_argument("--mode", choices=["file", "table"], required=True)
    ap.add_argument("--expected-diffs", type=Path)
    ap.add_argument("--report", type=Path)
    a = ap.parse_args()

    expected = load_expected(a.expected_diffs)
    problems: list[str] = []
    notes: list[str] = []
    rows: list[tuple[str, str]] = []
    run = a.java_dir / "INTCALC"

    base, got = intcalc_baseline_sysout(), sysout_lines(run / "sysout.txt")
    filler_lines = 0
    if a.mode == "table" and len(base) == len(got):
        for i, (b, g) in enumerate(zip(base, got)):
            m = TCATBAL_IMAGE.match(b)
            if b != g and m and g == m.group(1):
                base[i] = g
                filler_lines += 1
    if base == got:
        rows.append(("CBACT04C SYSOUT", f"{len(got)} lines, match"
                     + (f" (FILLER-only: {filler_lines})" if filler_lines else "")))
        if filler_lines:
            notes.append(f"SYSOUT: {filler_lines} DISPLAYed TCATBALF images end in spaces instead of the 22-zero "
                         "FILLER of the sample (not persisted in table mode, ADR-0011)")
    else:
        diff = list(difflib.unified_diff(base, got, "baseline", "java", lineterm="", n=1))
        problems.append("CBACT04C SYSOUT differs:\n" + "\n".join(diff[:40]))
        rows.append(("CBACT04C SYSOUT", "DIFF"))

    base_rc = (BASELINE / "INTCALC" / "rc.txt").read_text().strip()
    java_rc = (run / "rc.txt").read_text().strip() if (run / "rc.txt").exists() else "missing"
    rows.append(("RC", f"Java {java_rc} / baseline {base_rc}" + (", match" if java_rc == base_rc else "")))
    if java_rc != base_rc:
        problems.append(f"RC: Java {java_rc} vs baseline {base_rc}")

    base_tx = (BASELINE / "INTCALC" / "TRANSACT.txt").read_text(encoding="latin-1").splitlines()
    java_tx = fixed_records(run / "SYSTRAN", 350, problems, "SYSTRAN")
    rows.append(("SYSTRAN (TRANSACT DD)", compare_records("TRANSACT", base_tx, java_tx, a.mode, expected, problems,
                                                         notes)))

    for ds, job in (("ACCTDATA", "INTCALC"), ("TCATBALF", "POSTTRAN")):
        base_ds = (BASELINE / job / f"{ds}.ksds.txt").read_text(encoding="latin-1").splitlines()
        java_ds = line_records(run / f"{ds}.ksds", problems, ds)
        rows.append((f"{ds} after-image (vs {job}/{ds}.ksds.txt)",
                     compare_records(ds, base_ds, java_ds, a.mode, expected, problems, notes)))

    key, hits = discgrp_reach()
    rows.append((f"DISCGRP record {DISCGRP_REC} ({key}) lookups", f"{hits} of the TCATBALF rows"))
    if a.mode == "table":
        notes.append(f"DISCGRP record {DISCGRP_REC} ({key}) has DIS-INT-RATE 15.00 in the EBCDIC sample loaded by "
                     f"initial-load vs 0.00 in the ASCII sample of the baseline; {hits} TCATBALF row(s) look it up"
                     + (" (no TCATBALF row has type 07), so it cannot change SYSTRAN or ACCTDATA" if not hits else ""))

    for (ds, rec, field), (b, j, hits_) in expected.items():
        if hits_ != 1:
            problems.append(f"expected diff {ds} record {rec} {field} `{b}`->`{j}` applied {hits_} times")

    report = [f"## INTCALC ({a.mode} mode) vs GnuCOBOL baseline", "", "| check | result |", "|---|---|"]
    report += [f"| {k} | {v} |" for k, v in rows]
    if notes:
        report += ["", "Documented differences:", ""] + [f"- {n}" for n in notes]
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

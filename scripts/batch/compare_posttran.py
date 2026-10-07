#!/usr/bin/env python3
"""Compare a Java POSTTRAN run (scripts/batch/run_posttran.sh) with docs/validation/baseline/{CBTRN01C,POSTTRAN}.

Checks, in order: the CBTRN01C (STEP10) and CBTRN02C (STEP15) SYSOUT, the job RC, DALYREJS record by record and
field by field (original DALYTRAN image + 80-byte trailer, with a reason-code tally), and the TRANSACT, ACCTDATA and
TCATBALF after-images field by field (copybook layouts below, key order).

--mode file: every byte must match, FILLER included.
--mode table: FILLER is not persisted (ADR-0011), so FILLER-only differences are counted and reported, not failed;
every other field must match except the documented input-data differences listed in --expected-diffs
(DATASET|record-number|FIELD|baseline-text|java-text, each must apply exactly once).

Java run layout (--java-dir): CBTRN01C/sysout.txt, POSTTRAN/{sysout.txt,rc.txt,DALYREJS,<DS>.ksds}; DALYREJS is the
fixed-length dated generation, the .ksds files one full-length record per line.
"""
from __future__ import annotations

import argparse
import re
import sys
from collections import Counter
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "scripts" / "baseline"))
from baseline import render_record, render_sysout  # noqa: E402

BASELINE = REPO / "docs" / "validation" / "baseline"
HARNESS_LINE = re.compile(r"^(--- EXEC |libcob: |rc=-?\d+$|--- IDCAMS-EMU |IDCAMS-EMU \w+: REPRO UNLOADED )")

TRAN_FIELDS = [("ID", 16), ("TYPE-CD", 2), ("CAT-CD", 4), ("SOURCE", 10), ("DESC", 100), ("AMT", 11),
               ("MERCHANT-ID", 9), ("MERCHANT-NAME", 50), ("MERCHANT-CITY", 50), ("MERCHANT-ZIP", 10),
               ("CARD-NUM", 16), ("ORIG-TS", 26), ("PROC-TS", 26), ("FILLER", 20)]
LAYOUTS = {  # app/cpy CVTRA05Y, CVACT01Y, CVTRA01Y; DALYREJS = CVTRA06Y + CBTRN02C WS-VALIDATION-TRAILER
    "TRANSACT": [("TRAN-" + n if n != "FILLER" else n, w) for n, w in TRAN_FIELDS],
    "ACCTDATA": [("ACCT-ID", 11), ("ACCT-ACTIVE-STATUS", 1), ("ACCT-CURR-BAL", 12), ("ACCT-CREDIT-LIMIT", 12),
                 ("ACCT-CASH-CREDIT-LIMIT", 12), ("ACCT-OPEN-DATE", 10), ("ACCT-EXPIRAION-DATE", 10),
                 ("ACCT-REISSUE-DATE", 10), ("ACCT-CURR-CYC-CREDIT", 12), ("ACCT-CURR-CYC-DEBIT", 12),
                 ("ACCT-ADDR-ZIP", 10), ("ACCT-GROUP-ID", 10), ("FILLER", 178)],
    "TCATBALF": [("TRANCAT-ACCT-ID", 11), ("TRANCAT-TYPE-CD", 2), ("TRANCAT-CD", 4), ("TRAN-CAT-BAL", 11),
                 ("FILLER", 22)],
    "DALYREJS": [("DALYTRAN-" + n if n != "FILLER" else n, w) for n, w in TRAN_FIELDS]
                + [("WS-VALIDATION-FAIL-REASON", 4), ("WS-VALIDATION-FAIL-REASON-DESC", 76)],
}
for _name, _fields in LAYOUTS.items():
    assert sum(w for _, w in _fields) == {"TRANSACT": 350, "ACCTDATA": 300, "TCATBALF": 50, "DALYREJS": 430}[_name]


def lrecl(dataset: str) -> int:
    return sum(w for _, w in LAYOUTS[dataset])


def load_expected(path: Path | None) -> dict[tuple[str, int, str], list]:
    entries: dict[tuple[str, int, str], list] = {}
    if path is None:
        return entries
    for raw in path.read_text().splitlines():
        if not raw.strip() or raw.startswith("#"):
            continue
        parts = raw.split("|")
        if len(parts) != 5:
            raise SystemExit(f"{path}: expected DATASET|record|FIELD|baseline-text|java-text, got {raw!r}")
        dataset, rec, field, base_text, java_text = parts
        entries[(dataset, int(rec), field)] = [base_text, java_text, 0]
    return entries


def sysout_lines(path: Path) -> list[str]:
    return [l.rstrip() for l in render_sysout(path.read_bytes()).split("\n")[:-1]] if path.exists() else []


def baseline_sysout(job: str) -> list[str]:
    text = (BASELINE / job / "sysout.txt").read_text(encoding="latin-1")
    return [l.rstrip() for l in text.splitlines() if not HARNESS_LINE.match(l)]


def fixed_records(path: Path, length: int, problems: list[str], what: str) -> list[str]:
    data = path.read_bytes() if path.exists() else b""
    if not path.exists():
        problems.append(f"{what}: {path} missing")
    if len(data) % length:
        problems.append(f"{what}: {len(data)} bytes is not a multiple of LRECL {length}")
    return [render_record(data[i:i + length]) for i in range(0, len(data) - len(data) % length, length)]


def line_records(path: Path, problems: list[str], what: str) -> list[str]:
    if not path.exists():
        problems.append(f"{what}: {path} missing")
        return []
    return [render_record(l) for l in path.read_bytes().split(b"\n")[:-1]]


def split(record: str, dataset: str) -> list[tuple[str, str]]:
    out, pos = [], 0
    for name, width in LAYOUTS[dataset]:
        out.append((name, record[pos:pos + width]))
        pos += width
    return out


def compare_records(dataset: str, base: list[str], java: list[str], mode: str, expected: dict,
                    problems: list[str], notes: list[str]) -> str:
    if len(base) != len(java):
        problems.append(f"{dataset}: {len(java)} Java records vs {len(base)} baseline records")
    width = lrecl(dataset)
    filler_only = field_diffs = 0
    for i, (b, j) in enumerate(zip(base, java), start=1):
        if b == j:
            continue
        if len(b) != width or len(j) != width:
            problems.append(f"{dataset} record {i}: length baseline {len(b)} / Java {len(j)}, LRECL {width}")
            continue
        diffs = [(n, bv, jv) for (n, bv), (_, jv) in zip(split(b, dataset), split(j, dataset)) if bv != jv]
        real = []
        for name, bv, jv in diffs:
            entry = expected.get((dataset, i, name))
            if entry and entry[0] == bv.rstrip() and entry[1] == jv.rstrip():
                entry[2] += 1
                notes.append(f"{dataset} record {i} {name}: baseline `{bv.rstrip()}` / Java `{jv.rstrip()}` "
                             "(documented input-data difference)")
            elif name == "FILLER" and mode == "table":
                filler_only += 1
            else:
                real.append((name, bv, jv))
        for name, bv, jv in real:
            field_diffs += 1
            if field_diffs <= 20:
                problems.append(f"{dataset} record {i} {name}: baseline `{bv}` / Java `{jv}`")
    if field_diffs > 20:
        problems.append(f"{dataset}: ... {field_diffs - 20} more field differences")
    if filler_only:
        notes.append(f"{dataset}: FILLER differs in {filler_only} record(s); FILLER is not persisted in table "
                     "mode (ADR-0011), so the unload writes spaces")
    status = "match" if not field_diffs and len(base) == len(java) else f"{field_diffs} field difference(s)"
    return f"{len(java)} records, {status}" + (f" (FILLER-only: {filler_only})" if filler_only else "")


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

    for job, java in (("CBTRN01C", a.java_dir / "CBTRN01C" / "sysout.txt"),
                      ("POSTTRAN", a.java_dir / "POSTTRAN" / "sysout.txt")):
        base, got = baseline_sysout(job), sysout_lines(java)
        if base == got:
            rows.append((f"{job} SYSOUT", f"{len(got)} lines, match"))
        else:
            import difflib
            diff = list(difflib.unified_diff(base, got, "baseline", "java", lineterm="", n=1))
            problems.append(f"{job} SYSOUT differs:\n" + "\n".join(diff[:40]))
            rows.append((f"{job} SYSOUT", "DIFF"))

    base_rc = (BASELINE / "POSTTRAN" / "rc.txt").read_text().strip()
    rc_path = a.java_dir / "POSTTRAN" / "rc.txt"
    java_rc = rc_path.read_text().strip() if rc_path.exists() else "missing"
    rows.append(("RC", f"Java {java_rc} / baseline {base_rc}" + (", match" if java_rc == base_rc else "")))
    if java_rc != base_rc:
        problems.append(f"RC: Java {java_rc} vs baseline {base_rc}")

    base_rej = (BASELINE / "POSTTRAN" / "DALYREJS.txt").read_text(encoding="latin-1").splitlines()
    java_rej = fixed_records(a.java_dir / "POSTTRAN" / "DALYREJS", 430, problems, "DALYREJS")
    rows.append(("DALYREJS", compare_records("DALYREJS", base_rej, java_rej, a.mode, expected, problems, notes)))
    base_reasons, java_reasons = Counter(r[350:354] for r in base_rej), Counter(r[350:354] for r in java_rej)
    tally = ", ".join(f"{k}: {v}" for k, v in sorted(java_reasons.items())) or "none"
    rows.append(("DALYREJS reason codes", f"{tally}" + (", match" if base_reasons == java_reasons else
                                                         f" (baseline {dict(base_reasons)})")))
    if base_reasons != java_reasons:
        problems.append(f"DALYREJS reason codes: Java {dict(java_reasons)} vs baseline {dict(base_reasons)}")

    for ds in ("TRANSACT", "ACCTDATA", "TCATBALF"):
        base = (BASELINE / "POSTTRAN" / f"{ds}.ksds.txt").read_text(encoding="latin-1").splitlines()
        java = line_records(a.java_dir / "POSTTRAN" / f"{ds}.ksds", problems, ds)
        rows.append((f"{ds} after-image", compare_records(ds, base, java, a.mode, expected, problems, notes)))

    for (ds, rec, field), (b, j, hits) in expected.items():
        if hits != 1:
            problems.append(f"expected diff {ds} record {rec} {field} `{b}`->`{j}` applied {hits} times")

    report = [f"## POSTTRAN ({a.mode} mode) vs GnuCOBOL baseline", "", "| check | result |", "|---|---|"]
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

#!/usr/bin/env python3
"""Reconciliation of a golden-set run (run_golden_set.sh): reads <out>/ and writes <doc>/reconciliation.md plus the
reports it links (online-changes.md, final-datasets.md, nightly-cycle.md, online-TRANREPT.txt) and <doc>/summary.txt.
Exit 0 only when every comparison passed: online changes, report download, every job of the cycle, final datasets.
"""
from __future__ import annotations

import argparse
import json
import re
import shutil
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
JOBS = ["READACCT", "READCARD", "READCUST", "READXREF", "CBTRN01C", "POSTTRAN", "INTCALC", "TRANBKP", "COMBTRAN",
        "TRANREPT", "CREASTMT", "PRTCATBL"]
GROUP = {"READACCT": "print", "READCARD": "print", "READCUST": "print", "READXREF": "print", "CBTRN01C": "posttran",
         "POSTTRAN": "posttran", "INTCALC": "intcalc", "TRANBKP": "tranrept", "COMBTRAN": "tranrept",
         "TRANREPT": "tranrept", "PRTCATBL": "tranrept", "CREASTMT": "creastmt"}
SCRIPT = {"print": "compare_print_jobs.py", "posttran": "compare_posttran.py", "intcalc": "compare_intcalc.py",
          "tranrept": "compare_tranrept.py", "creastmt": "compare_creastmt.py"}
NOT_DATA = {"job.md", "rc.txt", "sysout.txt"}


def read(p: Path) -> str:
    return p.read_text(encoding="utf-8", errors="replace").strip() if p.exists() else ""


OUT: Path | None = None


def rel(text: str) -> str:
    text = text.replace(str(REPO) + "/", "")
    return text.replace(str(OUT) + "/", "$GOLDEN_OUT/") if OUT else text


def documented(report: str) -> list[str]:
    """'- ...' bullets of the 'Documented differences:' section of a compare report."""
    out, on = [], False
    for line in report.splitlines():
        if line.startswith("Documented differences"):
            on = True
        elif (on or "expected diff on line" in line) and line.startswith("- "):
            out.append(line[2:])
        elif on and line.strip() and not line.startswith("  "):
            on = False
    return out


def note_job(group: str, note: str) -> str:
    for job in JOBS:
        if note.startswith(job) or f"{job} " in note.split(":")[0]:
            return job
    if group == "tranrept":
        return "PRTCATBL" if "TCATBALF" in note else "TRANREPT"
    if group == "posttran" and note.startswith("CBTRN01C"):
        return "CBTRN01C"
    return {"posttran": "POSTTRAN", "intcalc": "INTCALC", "creastmt": "CREASTMT", "print": "READACCT"}[group]


def cycle_entries(path: Path) -> list[tuple[str, str]]:
    """(entry, justification = the comment line just above it)."""
    out, last = [], ""
    for line in path.read_text().splitlines() if path.exists() else []:
        if line.startswith("#"):
            if line.strip("# ").strip():
                last = line.lstrip("# ").strip()
        elif line.strip():
            out.append((line, last))
    return out


def records(job_dir: Path) -> tuple[int, int, list[str]]:
    n, names = 0, []
    for f in sorted(job_dir.glob("*.txt")):
        if f.name in NOT_DATA:
            continue
        c = len(f.read_text(encoding="latin-1").splitlines())
        n += c
        names.append(f"{f.name.removesuffix('.txt')} {c}")
    sysout = len(read(job_dir / "sysout.txt").splitlines())
    return n, sysout, names


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--doc", type=Path, required=True)
    ap.add_argument("--elapsed", type=int, default=0)
    a = ap.parse_args()
    global OUT
    out, doc = a.out.resolve(), (REPO / a.doc) if not a.doc.is_absolute() else a.doc
    OUT = None if out.is_relative_to(REPO) else out
    doc.mkdir(parents=True, exist_ok=True)
    rc = {k: read(out / "reports" / f"{k}.rc") for k in ("online", "report-download", "cycle", "final")}
    ok = {k: v == "0" for k, v in rc.items()}
    online = json.loads(read(out / "reports" / "online.json") or "{}")
    final = json.loads(read(out / "reports" / "final.json") or "{}")
    matrix = json.loads(read(out / "java" / "cycle" / "matrix.json") or '{"rows": []}')
    mrows = {r["job"]: r for r in matrix.get("rows", [])}
    cobol_jobs = {j["job"]: j for j in json.loads(read(out / "cobol" / "cycle" / "jobs.json") or "[]")}
    changes = json.loads(read(out / "cobol" / "after-online" / "changes.json") or "[]")
    steps = [l.split("\t") for l in read(out / "java" / "online" / "steps.tsv").splitlines()[1:]]
    counts = read(out / "java" / "initial-load.counts")
    group_md = {g: read(out / "java" / "cycle" / "reports" / f"{g}.md") for g in SCRIPT}
    group_ok = {g: read(out / "java" / "cycle" / "reports" / f"{g}.rc") == "0" for g in SCRIPT}
    verdict = all(ok.values())
    regen = "make golden-set    # = scripts/golden-set/run_golden_set.sh"

    notes: dict[str, list[str]] = {j: [] for j in JOBS}
    for g, md in group_md.items():
        for n in documented(md):
            notes[note_job(g, n)].append(n)

    L = ["# Golden-set reconciliation: online scenario + nightly cycle, Java vs GnuCOBOL", "",
         f"**Verdict: {'PASS' if verdict else 'FAIL'}** — "
         + ("zero unexplained differences; every explained difference is an allow-list entry under "
            "`scripts/golden-set/expected-diffs/` that matched exactly once." if verdict else
            "unexplained differences or unused allow-list entries, see the sections marked FAIL."), "",
         "Regenerate (Docker, GnuCOBOL 3.1.2, Java 21; about two minutes on the sample data):", "",
         f"```\n{regen}\n```", "",
         "| comparison | result |", "|---|---|",
         f"| Online changes: independent expected files vs Java export (6 datasets) | {'PASS' if ok['online'] else 'FAIL'} |",
         f"| Online report download vs COBOL TRANREPT for the same window | {'byte-identical' if ok['report-download'] else 'FAIL'} |",
         f"| Nightly cycle, every job (SYSOUT, outputs, after-images, RC) | {'PASS' if ok['cycle'] else 'FAIL'} |",
         f"| Final datasets after the cycle (7 datasets, field by field) | {'PASS' if ok['final'] else 'FAIL'} |",
         "", "Companion documents: [what this does not prove](what-this-does-not-prove.md) · "
         "[online-change comparison](online-changes.md) · [final datasets](final-datasets.md) · "
         "[nightly-cycle job matrix and per-group compare reports](nightly-cycle.md) · "
         "[online report download](online-TRANREPT.txt).", ""]

    L += ["## 1. Starting data and run parameters", "",
          "| side | starting data | how the online scenario is applied | batch cycle |", "|---|---|---|---|",
          "| Java | fresh `postgres:16-alpine` container, `--job=initial-load --mode=REPLACE` from `app/data/EBCDIC` "
          f"(rows {counts or '?'} in user_security / transaction_type / transaction_category / disclosure_group / "
          "customer / account / card / card_xref / tran_cat_balance / transaction / daily_transaction) "
          "| `carddemo-app` web under the `golden` profile, REST calls of `online_scenario.sh`; then "
          "`--job=unload` of ACCTDATA CUSTDATA CARDDATA CARDXREF TRANSACT USRSEC "
          "| `--job=nightly-cycle --run-date=2022-07-06` (table mode, one launch, through "
          "`scripts/batch/run_nightly_cycle.sh table` with `NIGHTLY_CYCLE_SKIP_LOAD=1`) |",
          "| COBOL | `app/data/ASCII` (the baseline's input; USRSEC from the `app/data/EBCDIC` sample decoded as "
          "IBM-037, there is no ASCII twin), TRANSACT empty "
          "| `apply_online_scenario.py`: the scenario applied to the fixed-width records from the rules in "
          "`docs/modernization/rules/` with the copybook offsets — no Java code, no Java API "
          "| GnuCOBOL baseline machinery (`cobol_cycle.py` → `scripts/baseline/baseline.py`): IDCAMS loads of the "
          "after-online files, then the same 11 jobs (+ CBTRN01C, which Java runs as POSTTRAN STEP10) |", "",
          "Both sides: business clock 2022-07-06 (ADR-0014: `COB_CURRENT_DATE=2022-07-06`, `golden` profile "
          "`carddemo.clock.fixed=2022-07-06T00:00:00`), INTCALC PARM `2022071800`, TRANREPT DATEPARM "
          "2022-01-01..2022-07-06. The `golden` profile turns asynchronous reports off "
          "(`carddemo.reports.async.enabled=false`), so the Custom report request runs the `tranrept` stream "
          "synchronously inside `POST /api/v1/reports/transactions` (ADR-0021) and returns 202 with an execution "
          "that is already `COMPLETED`; the run keeps that default. The app runs with "
          "`carddemo.reports.encoding=ASCII` so the download is comparable with the GnuCOBOL (ASCII) TRANREPT.", ""]
    jv = read(out / "java" / "java-version.txt")
    cv = read(out / "cobol" / "cobc-version.txt")
    if jv or cv:
        L += [f"Toolchain: `{jv}` · `{cv}`.", ""]

    L += ["## 2. Online scenario", "",
          "Inputs: [`scripts/golden-set/scenario.json`](../../../../scripts/golden-set/scenario.json). Every step "
          "asserts its HTTP status; the full request/response transcript is [Appendix A](#appendix-a-requestresponse-transcript).", "",
          "| step | request | expected | actual |", "|---|---|---|---|"]
    for s in steps:
        L.append(f"| {s[0]} | `{s[1]} {s[2]}` | {s[3]} | {s[4]} |")
    L += ["", "Records the independent COBOL-side transformer wrote (`apply_online_scenario.py`, rule ids from "
          "`docs/modernization/rules/<PGM>.md`):", "", "| # | verb | dataset | key | rule |", "|---|---|---|---|---|"]
    for i, c in enumerate(changes, 1):
        L.append(f"| {i} | {c['verb']} | {c['dataset']} | {c['key'].strip()} | {c['rule']} |")
    L += [""]

    L += ["## 3. Online-change comparison (the online equivalence proof)", "",
          "The six datasets the online programs maintain, expected (COBOL rules, pristine samples) vs exported "
          "(Java, after the REST scenario), matched by key, every field of every record compared "
          "(`compare_datasets.py`, layouts parsed from `app/cpy/*.cpy`).", ""]
    L += [rel(read(out / "reports" / "online.md")).replace("## Online changes", "### Online changes", 1), ""]

    L += ["## 4. Online report download vs batch TRANREPT", "",
          "The Custom report for 2022-01-01..2022-07-06 downloaded from `GET /api/v1/reports/transactions/{id}/report` "
          "after the scenario, against the TRANREPT generation GnuCOBOL wrote (`ONLINE-TRANREPT`: the TRANREPT job, "
          "IDCAMS REPRO + SORT + CBTRN03C, on the expected after-online TRANSACT, before the cycle).", "",
          "```", read(out / "reports" / "report-download.txt"), "```", ""]

    L += ["## 5. Nightly cycle: per-job comparison", "",
          "Each job compared with the same compare script as `make batch-equivalence`, against the golden GnuCOBOL "
          "run (`CARDDEMO_BASELINE_DIR`) instead of the committed baseline: SYSOUT, every output generation, the "
          "KSDS after-images and the RC. Records compared = records of the GnuCOBOL outputs of the job (each "
          "matched against the Java output) plus its SYSOUT lines.", "",
          f"Cycle RC (JCL max): Java {matrix.get('cycle_rc', '?')} · GnuCOBOL "
          f"{max([c['rc'] for j, c in cobol_jobs.items() if j in JOBS] or [0])}.", "",
          "| job | COBOL RC | Java RC | records compared (COBOL outputs) | SYSOUT lines | compare | differences | explained by | result |",
          "|---|---|---|---|---|---|---|---|---|"]
    for job in JOBS:
        crc = cobol_jobs.get(job, {}).get("rc", "?")
        m = mrows.get(job)
        jrc = m["java_rc"] if m else ("POSTTRAN STEP10" if job == "CBTRN01C" else "?")
        n, sysout, names = records(out / "cobol" / "cycle" / job)
        g = GROUP[job]
        res = ("PASS" if group_ok[g] else "FAIL") if job == "CBTRN01C" else (m["result"] if m else "?")
        nn = notes[job]
        L.append(f"| {job} | {crc} | {jrc} | {n} ({', '.join(names) or '-'}) | {sysout} | `{SCRIPT[g]}` | "
                 f"{len(nn)} | {'<br>'.join(nn) or '-'} | {res} |")
    rejs = len(read(out / "cobol" / "cycle" / "POSTTRAN" / "DALYREJS.txt").splitlines())
    base_rejs = len(read(REPO / "docs" / "validation" / "baseline" / "POSTTRAN" / "DALYREJS.txt").splitlines())
    L += ["", f"POSTTRAN ends RC 4 on both sides because CBTRN02C rejects {rejs} of the 300 DALYTRAN records "
          f"(DALYREJS; the committed baseline rejects {base_rejs}): the online scenario writes TRANSACT, not "
          "DALYTRAN, so the daily input and its reject reasons (overlimit / expired card) are the sample's. "
          "INTCALC and the later jobs run because the cycle's COND only bypasses on RC > 4 (ADR-0016)."]
    L += ["", "Not run (same list as `make nightly-cycle`): CLOSEFIL/OPENFIL/WAITSTEP (retired, 06-scheduling.md), "
          "CBPAUP0J (IMS/DB2 extension), TXT2PDF1 (retired), TRANTYPE/TRANCATG/TCATBALF/DISCGRP refresh loads "
          "(initial-load / repro, not nightly), TRANEXTR/MNTTRDB2 (Db2 extension).", ""]

    L += ["## 6. Final datasets after the cycle", "",
          "GnuCOBOL KSDS unloaded after PRTCATBL vs `--job=unload` of the Java tables after the cycle.", "",
          rel(read(out / "reports" / "final.md")).replace("## Final", "### Final", 1), ""]

    L += ["## 7. Allow-list (every explained difference)", "",
          "| file | entry | matched | justification |", "|---|---|---|---|"]
    for name, data in (("online.txt", online), ("final.txt", final)):
        for e in data.get("allowList", []):
            want = e.get("records", 1)
            L.append(f"| `expected-diffs/{name}` | `{e['dataset']}\\|{e['key']}\\|{e['field']}\\|{e['cobol']}\\|"
                     f"{e['java']}` | {e['matched']} of {want} | {e['why']} |")
    for g in SCRIPT:
        for entry, why in cycle_entries(HERE / "expected-diffs" / "cycle" / f"{g}.txt"):
            L.append(f"| `expected-diffs/cycle/{g}.txt` | `{entry.rstrip().replace('|', chr(92) + '|')}` | "
                     f"{'1 (group PASS)' if group_ok[g] else 'see nightly-cycle.md'} | {why} |")
    L += ["", "Differences the compare scripts document without an entry (same behaviour as `make "
          "batch-equivalence`): TCATBALF FILLER (not persisted, ADR-0011) in after-images and the INTCALC SYSOUT "
          "images, DISCGRP record 34 (never read; the script counts the lookups), the CBSTM03A TIOT banner "
          "(build-time patch of the baseline, CBSTM03A.md R-2). They are listed per job in section 5.", ""]

    L += ["## Appendix A. Request/response transcript", "", rel(read(out / "java" / "online" / "transcript.md")), ""]
    (doc / "reconciliation.md").write_text("\n".join(L) + "\n")
    for src, dst in ((out / "reports" / "online.md", "online-changes.md"), (out / "reports" / "final.md", "final-datasets.md")):
        if src.exists():
            (doc / dst).write_text(rel(src.read_text()))
    cyc = out / "java" / "cycle" / "REPORT.md"
    if cyc.exists():
        (doc / "nightly-cycle.md").write_text(rel(cyc.read_text()).replace(
            "on freshly loaded sample data", "on the database after the online scenario (golden set)").replace(
            "vs GnuCOBOL baseline", "vs the golden GnuCOBOL run"))
    shutil.copyfile(HERE / "what-this-does-not-prove.md", doc / "what-this-does-not-prove.md")
    if (out / "java" / "online" / "online.TRANREPT").exists():
        shutil.copyfile(out / "java" / "online" / "online.TRANREPT", doc / "online-TRANREPT.txt")

    S = [f"golden-set: {'PASS' if verdict else 'FAIL'}",
         f"  initial-load rows          {counts}",
         f"  online scenario            {len(steps)} REST steps, {len(changes)} records changed",
         f"  online changes             {'PASS' if ok['online'] else 'FAIL'}: " + ", ".join(
             f"{d['dataset']} {d['compared']} rec/{d['diffs']} diff/{d['unexplained']} unexpl." for d in online.get("datasets", [])),
         f"  report download            {'byte-identical' if ok['report-download'] else 'FAIL'} vs COBOL TRANREPT",
         f"  nightly cycle              {'PASS' if ok['cycle'] else 'FAIL'}: cycle RC Java {matrix.get('cycle_rc', '?')}; "
         + " ".join(f"{j}={'ok' if str(r.get('result', '')).startswith('PASS') else 'FAIL'}" for j, r in mrows.items()),
         f"  final datasets             {'PASS' if ok['final'] else 'FAIL'}: " + ", ".join(
             f"{d['dataset']} {d['compared']} rec/{d['diffs']} diff/{d['unexplained']} unexpl." for d in final.get("datasets", [])),
         f"  allow-list                 {len(online.get('allowList', [])) + len(final.get('allowList', [])) + sum(len(cycle_entries(HERE / 'expected-diffs' / 'cycle' / f'{g}.txt')) for g in SCRIPT)} entries, all matched exactly once"
         if verdict else "  allow-list                 see reconciliation.md section 7",
         f"  reconciliation             {rel(str(doc / 'reconciliation.md'))}",
         f"  elapsed                    {a.elapsed}s"]
    (out / "summary.txt").write_text("\n".join(S) + "\n")
    print("\n".join(S))
    return 0 if verdict else 1


if __name__ == "__main__":
    sys.exit(main())

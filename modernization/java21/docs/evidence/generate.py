#!/usr/bin/env python3
"""Regenerate differential evidence on synthetic fixtures. Standard library only."""

import argparse
import csv
import gzip
import hashlib
import io
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from collections import Counter
from itertools import zip_longest
from pathlib import Path
from typing import Any

HERE = Path(__file__).resolve().parent
JAVA = HERE.parents[1]
ROOT = JAVA.parents[1]
CATALOG = JAVA / "docs/business-rules"
RECORDS = JAVA / "records/src/main/java/com/carddemo/modernization/records"
SETS = ("mixed", "volume")
MASTERS = ("ACCTFILE", "DISCGRP", "TCATBALF", "TRANFILE", "XREFFILE")
TYPES = dict(
    zip(
        MASTERS,
        (
            "AccountRecord",
            "DisclosureGroupRecord",
            "TranCatBalanceRecord",
            "TransactionRecord",
            "CardXrefRecord",
        ),
    )
)
TYPES.update({"SYSTRAN": "TransactionRecord", "DALYREJS": "RejectRecord"})
CLASSES = ("records", "interest", "posting")

# Explicit assertions are trace links, not a claim of full branch coverage.
RULE_TESTS = {
    "INT-01": ("int01Requires",),
    "INT-02": ("packagedJarReproduces",),
    "INT-03": ("int03Sorts",),
    "INT-04": ("int05Rewrites", "int12Adds"),
    "INT-05": ("int05Rewrites",),
    "INT-06": ("int06AndInt07", "int06Missing"),
    "INT-07": ("int06AndInt07", "int07Chooses"),
    "INT-08": ("int09UsesPrimary",),
    "INT-09": ("int09UsesPrimary",),
    "INT-10": ("int10ZeroRate",),
    "INT-11": ("int11Truncates", "int11TruncateToPic"),
    "INT-12": ("int12Adds",),
    "INT-13": ("int15Populates",),
    "INT-14": ("int14Counter",),
    "INT-15": ("int15Populates",),
    "INT-16": ("int16Formats",),
    "INT-17": ("int05Rewrites", "int12Adds"),
    "INT-18": (),
    "INT-19": ("int06AndInt07", "int09UsesPrimary"),
    "INT-20": ("matchesGoldenMaster",),
    "POST-01": ("reusedStoreStartsFresh", "packagedJarReproduces"),
    "POST-02": ("namedDifferential", "volumeDifferential", "emptyInput"),
    "POST-03": ("malformedNumericFieldsDoNotPrevent",),
    "POST-04": ("namedDifferential",),
    "POST-05": ("namedDifferential",),
    "POST-06": ("namedDifferential",),
    "POST-07": ("namedDifferential",),
    "POST-08": ("malformedNumericFieldsDoNotPrevent",),
    "POST-09": ("namedDifferential", "volumeDifferential"),
    "POST-10": ("nonNumericMerchantIdIsPosted", "namedDifferential"),
    "POST-11": ("transactionWriteFailureDoesNotRollBack", "failedRecordKeeps"),
    "POST-12": ("namedDifferential", "volumeDifferential"),
    "POST-13": ("volumeDifferential",),
    "POST-14": ("namedDifferential",),
    "POST-15": ("namedDifferential",),
    "POST-16": ("failedRewriteDoesNotReject",),
    "POST-17": ("transactionWriteFailureDoesNotRollBack", "failedRecordKeeps"),
    "POST-18": ("namedDifferential", "volumeDifferential"),
    "POST-19": ("transactionWriteFailureDoesNotRollBack", "postingSysoutFailureStops"),
}
GAPS = {
    "INT-01": "Java rejects wrong-length PARM; COBOL does not check PARM-LENGTH. No invalid-PARM differential.",
    "INT-02": "No COBOL open/close failure injection.",
    "INT-06": "Missing-account failure is Java-only; no COBOL abend fixture.",
    "INT-07": "Missing-xref and multi-card choice are Java-only; golden has one card per account.",
    "INT-08": "Primary-rate success is Java-only; golden uses DEFAULT fallback.",
    "INT-09": "Missing DEFAULT and non-23 I/O failure have no COBOL differential fixture.",
    "INT-10": "Zero-rate branch is Java-only; zero balance in golden is not zero rate.",
    "INT-11": "PIC overflow is Java-only; no COBOL overflow fixture.",
    "INT-14": "Counter wrap is Java-only; no COBOL wrap fixture.",
    "INT-18": "No dedicated fee assertion or fee implementation; SME decision pending.",
    "INT-19": "Only selected Java abends; no exhaustive COBOL open/read/write/rewrite/close failures.",
    "POST-01": "No COBOL open/close failures or target indexed-file lifecycle test.",
    "POST-03": "Malformed-field lookup ordering is Java-only; golden tests valid record encodings.",
    "POST-06": "No COBOL equality-at-limit or PIC-overflow fixture.",
    "POST-07": "No COBOL equality-at-expiry or malformed-date fixture.",
    "POST-10": "Opaque merchant-ID test is Java-only.",
    "POST-11": "No COBOL mid-write failure injection; Java Store and batch tests only; no multi-file atomicity.",
    "POST-12": "No COBOL unexpected category-read status injection.",
    "POST-16": "Rewrite INVALID KEY is injected only in the Java Store; no COBOL failure fixture.",
    "POST-17": "Duplicate/write failure is Java-only, not a COBOL differential.",
    "POST-19": "Selected Java failures only; no complete COBOL I/O status matrix or close-error display check.",
}
PENDING = {
    "INT-05": "Last account's interest transactions exist but its balance/cycles are not updated. Preserved; SME decision pending.",
    "INT-07": "Multiple cards: Java chooses first card in card-number primary-key order. Not established by a COBOL multi-card fixture; SME confirmation pending.",
    "INT-18": "Fee paragraph is a stub. No fees computed; SME decision pending.",
    "POST-11": "Category, account, transaction updates have no rollback. Failure/recovery semantics remain SME-pending.",
    "POST-16": "Account REWRITE failure sets 109 but does not reject, change RC or stop posting. Preserved in Java-only injected test; SME decision pending.",
}


def digest(data):
    return hashlib.sha256(data).hexdigest()


def command(args, log=None, check=True):
    result = subprocess.run(
        [str(a) for a in args],
        cwd=ROOT,
        env=dict(os.environ, TZ="UTC", LC_ALL="C"),
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        timeout=600,
        check=False,
    )
    if log:
        log.write_bytes(result.stdout)
    if check and result.returncode:
        raise RuntimeError(
            f"Command failed ({result.returncode}): {args}\n{result.stdout.decode(errors='replace')}"
        )
    return result


def git(*args):
    return command(["git", *args]).stdout.decode().strip()


def layout(type_name):
    source = (RECORDS / f"{type_name}.java").read_text()
    declaration_match = re.search(r"public record \w+\((.*?)\)\s*\{", source, re.DOTALL)
    if declaration_match is None:
        raise ValueError(f"Unrecognized record declaration: {type_name}")
    declaration = declaration_match.group(1)
    names = [part.strip().split()[-1] for part in declaration.split(",")]
    decoder = source.split("public " + type_name + " decode(", 1)[1].split(
        "public byte[] encode", 1
    )[0]
    widths = [
        int(w) for w in re.findall(r"fields\.(?:text|unsigned|money)\((\d+)\)", decoder)
    ]
    size_match = re.search(r"length\(\) \{ return (\d+);", source)
    if size_match is None:
        raise ValueError(f"Unrecognized record size: {type_name}")
    size = int(size_match.group(1))
    if len(names) != len(widths) or sum(widths) != size:
        raise ValueError(f"Unrecognized or incomplete codec layout: {type_name}")
    fields = list(zip(names, widths))
    if type_name == "RejectRecord":
        fields = [
            ("transaction." + n, w) for n, w in layout("DailyTransactionRecord")
        ] + fields[1:]
    return fields


def chunks(data, size):
    # A partial trailing record must be compared, never dropped.
    return [data[i : i + size] for i in range(0, len(data), size)]


def compare(writer, stage, program, fixture, name, expected, actual, fields):
    size = sum(w for _, w in fields) if fields else None
    e_rows = chunks(expected, size) if size else expected.splitlines(keepends=True)
    a_rows = chunks(actual, size) if size else actual.splitlines(keepends=True)
    counts: dict[str, Counter[str]] = {"record": Counter(), "field": Counter()}
    differences = []

    def classify(index, field, e, a, level):
        status = "match" if e == a else "unresolved"
        counts[level][status] += 1
        writer.writerow(
            [
                stage,
                program,
                fixture,
                name,
                index,
                field,
                status,
                None if e is None else e.hex(),
                None if a is None else a.hex(),
                "",
                ""
                if status == "match"
                else "Exact bytes differ; no approved catalog change.",
            ]
        )
        if status != "match":
            differences.append(
                {
                    "record": index,
                    "field": field,
                    "classification": status,
                    "expected_hex": None if e is None else e.hex(),
                    "actual_hex": None if a is None else a.hex(),
                    "reason": "Exact bytes differ; no approved catalog change.",
                }
            )

    for index, (e, a) in enumerate(zip_longest(e_rows, a_rows), 1):
        classify(index, "@record" if fields else "@line", e, a, "record")
        offset = 0
        for field, width in fields or []:
            classify(
                index,
                field,
                None if e is None else e[offset : offset + width],
                None if a is None else a[offset : offset + width],
                "field",
            )
            offset += width
    return {
        "stage": stage,
        "program": program,
        "fixture": fixture,
        "file": name,
        "expected_records": len(e_rows),
        "actual_records": len(a_rows),
        "partial_expected_bytes": len(expected) % size if size else 0,
        "partial_actual_bytes": len(actual) % size if size else 0,
        "records": dict(counts["record"]),
        "fields": dict(counts["field"]),
        "expected_sha256": digest(expected),
        "actual_sha256": digest(actual),
        "differences": differences,
    }


def read_tests():
    tests = []
    for module in (*CLASSES, "batch"):
        reports = sorted((JAVA / module / "target").glob("*-reports/TEST-*.xml"))
        if not reports:
            raise RuntimeError(f"No fresh test reports for {module}")
        for path in reports:
            for case in ET.parse(path).getroot().iter("testcase"):
                bad = next(
                    (
                        child
                        for child in case
                        if child.tag in ("skipped", "failure", "error")
                    ),
                    None,
                )
                tests.append(
                    {
                        "module": module,
                        "test": case.attrib["classname"].split(".")[-1]
                        + "#"
                        + case.attrib["name"],
                        "status": "passed" if bad is None else bad.tag,
                        "reason": ""
                        if bad is None
                        else (
                            bad.attrib.get("message", "") + "\n" + (bad.text or "")
                        ).strip(),
                    }
                )
    return sorted(tests, key=lambda t: (t["module"], t["test"]))


def catalog(tests):
    rules: dict[str, dict[str, Any]] = {}
    for program in ("CBACT04C", "CBTRN02C"):
        for line in (CATALOG / f"{program}.md").read_text().splitlines():
            cells = [c.strip() for c in line.split("|")]
            if len(cells) > 3 and re.fullmatch(r"(?:INT|POST)-\d+", cells[1]):
                rules[cells[1]] = {
                    "program": program,
                    "rule": cells[1],
                    "description": cells[3],
                }
    if set(rules) != set(RULE_TESTS):
        raise RuntimeError("Catalog changed: update explicit trace links and gaps")
    checklist = {}
    for line in (CATALOG / "SME-CHECKLIST.md").read_text().splitlines():
        cells = [c.strip() for c in line.split("|")]
        if len(cells) > 9 and cells[1] in rules:
            choices = [
                name
                for name, cell in zip(("Approve", "Change", "Question"), cells[4:7])
                if "[x]" in cell.lower()
            ]
            checklist[cells[1]] = {
                "status": ", ".join(choices) if choices else "pending",
                "initials": cells[7],
                "comments": cells[8],
            }
    if set(checklist) != set(rules):
        raise RuntimeError("Checklist and catalog rule IDs differ")
    for rule, item in rules.items():
        selected = []
        for prefix in RULE_TESTS[rule]:
            found = [t for t in tests if t["test"].split("#", 1)[1].startswith(prefix)]
            if not found:
                raise RuntimeError(
                    f"{rule}: test selector has no executed result: {prefix}"
                )
            selected.extend(found)
        # Golden supports normal paths, not every branch of a rule. Exceptions are stated explicitly.
        no_golden = {"INT-10", "INT-19", "POST-16", "POST-19"}
        item.update(
            tests=sorted({t["test"] for t in selected}),
            fixtures=[]
            if rule in no_golden
            else ["mixed"]
            if rule in {"POST-04", "POST-05", "POST-08"}
            else ["mixed", "volume"],
            fixture_scope="Normal success path only; see gaps and test-specific Java fixtures.",
            gaps=GAPS.get(
                rule,
                "No additional gap identified in this synthetic scope; not exhaustive branch coverage.",
            ),
            sme=checklist[rule],
        )
    return list(rules.values())


def disabled_annotations():
    rows = []
    for module in (*CLASSES, "batch"):
        for path in sorted((JAVA / module / "src/test").rglob("*.java")):
            source = path.read_text()
            for match in re.finditer(
                r"@(?:org\.junit\.jupiter\.api\.)?Disabled\b(?:\s*\((.*?)\))?",
                source,
                re.DOTALL,
            ):
                rows.append(
                    {
                        "file": str(path.relative_to(ROOT)),
                        "line": source[: match.start()].count("\n") + 1,
                        "reason": (match.group(1) or "No reason supplied").strip(),
                    }
                )
    return rows


def reject_rows(data):
    return [
        {
            "index": i,
            "transaction_id": row[:16].decode("ascii", errors="backslashreplace"),
            "reason_code": row[350:354].decode("ascii", errors="backslashreplace"),
            "description": row[354:]
            .decode("ascii", errors="backslashreplace")
            .rstrip(),
            "partial_bytes": len(row) if len(row) != 430 else 0,
        }
        for i, row in enumerate(chunks(data, 430), 1)
    ]


def provenance():
    paths = ["app/cbl/CBACT04C.cbl", "app/cbl/CBTRN02C.cbl", "app/cpy"]
    paths += [
        str((JAVA / p).relative_to(ROOT))
        for p in (*CLASSES, "batch", "golden", "docs/business-rules", "pom.xml")
    ]
    dirty = (
        command(["git", "status", "--porcelain", "--", *paths]).stdout.decode().strip()
    )
    if dirty:
        raise RuntimeError("Evidence inputs must be committed and clean:\n" + dirty)
    return {
        "source_commits": {p: git("log", "-1", "--format=%H", "--", p) for p in paths},
        "source_trees": {p: git("rev-parse", "HEAD:" + p) for p in paths},
        "generator_sha256": {
            p.name: digest(p.read_bytes())
            for p in sorted(HERE.iterdir())
            if p.suffix in (".py", ".java")
        },
        "tools": {
            name: re.sub(r"\x1b\[[0-9;]*m", "", command(args).stdout.decode())
            .strip()
            .splitlines()
            for name, args in {
                "cobc": ["cobc", "-V"],
                "jdk": ["java", "-version"],
                "maven": ["mvn", "-B", "-Dstyle.color=never", "--version"],
                "python": ["python3", "--version"],
            }.items()
        },
    }


def render(report):
    lines = [
        "# Classified differential evidence",
        "",
        "Differential results on synthetic fixtures only. No SME sign-off is implied.",
        "",
        "Generated by `python3 modernization/java21/docs/evidence/generate.py`.",
        "No wall-clock timestamp is emitted; provenance is source-path commits/trees and generator hashes (not a self-referential report commit).",
        "",
        "## Provenance",
        "",
        f"Pinned clock: `{report['clock']['clock_utc']}` UTC; PARM: `{report['clock']['parm_date']}`.",
        "",
    ]
    for path, sha in report["provenance"]["source_commits"].items():
        lines.append(
            f"- `{path}`: `{sha}` (tree/blob `{report['provenance']['source_trees'][path]}`)"
        )
    for name, version in report["provenance"]["tools"].items():
        lines += ["", f"{name}:", "```text", *version, "```"]
    lines += [
        "",
        "## Counts from executed comparisons",
        "",
        "Records include SYSOUT lines where the file is text. Field counts include fillers and opaque reject payload subfields; no tolerance or rounding.",
        "",
        "| Run | Program | Fixture | File | Expected | Actual | Match | Unresolved | Fields match | Fields unresolved |",
        "|---|---|---|---|---:|---:|---:|---:|---:|---:|",
    ]
    for c in report["comparisons"]:
        lines.append(
            f"| {c['stage']} | {c['program']} | {c['fixture']} | {c['file']} | {c['expected_records']} | {c['actual_records']} | {c['records'].get('match', 0)} | {c['records'].get('unresolved', 0)} | {c['fields'].get('match', 0)} | {c['fields'].get('unresolved', 0)} |"
        )
    lines += ["", "## Posting, rejects and process codes", ""]
    for run in report["runs"]:
        lines += [
            f"### {run['fixture']}",
            "",
            f"Posting: `{json.dumps(run['posting'], sort_keys=True)}`; jar exit `{run['jar_exit']}`, expected `{run['expected_exit']}`.",
            "",
            f"Reject counts by reason: `{json.dumps(run['reject_counts'], sort_keys=True)}`.",
            "",
            "| Index | Synthetic transaction | Reason | Description |",
            "|---:|---|---|---|",
        ]
        for reject in run["rejects"]:
            lines.append(
                f"| {reject['index']} | {reject['transaction_id']} | {reject['reason_code']} | {reject['description']} |"
            )
    lines += [
        "",
        "## Classification and gate",
        "",
        f"- Classified record/line and field rows: `{json.dumps(report['classification'], sort_keys=True)}`.",
        f"- Unclassified differences: {report['unclassified_differences']}.",
        "- Intended changes: none. There is no approved-change allowlist; every differing byte, missing/extra record or line is unresolved.",
        f"- Full lossless per-record/per-field ledger: `comparisons.csv.gz` (SHA-256 `{report['ledger_sha256']}`). Both sides are hex; empty plus unresolved identifies missing records; no values are elided.",
        f"- Maven exit: {report['maven_exit']}; executed test results: `{json.dumps(dict(Counter(t['status'] for t in report['tests'])), sort_keys=True)}`.",
        f"- Generator regression tests passed: {report['generator_tests']}.",
        f"- Disabled source annotations: {len(report['disabled'])}; skipped test executions: {sum(t['status'] == 'skipped' for t in report['tests'])}.",
        f"- Gate: {'PASS' if report['gate_passed'] else 'FAIL'}.",
        "",
        "### Unresolved differences",
        "",
    ]
    differences = [
        dict(
            stage=c["stage"],
            program=c["program"],
            fixture=c["fixture"],
            file=c["file"],
            **d,
        )
        for c in report["comparisons"]
        for d in c["differences"]
    ]
    lines += [
        "```json",
        json.dumps(differences, indent=2),
        "```",
        "",
        "### Disabled / non-passing tests",
        "",
        "```json",
        json.dumps(
            report["disabled"]
            + [t for t in report["tests"] if t["status"] != "passed"],
            indent=2,
        ),
        "```",
        "",
        "### Legacy behaviors still unresolved / SME-pending",
        "",
    ]
    lines += [
        f"- **{rule}**: {reason}"
        for rule, reason in report["sme_pending_behaviors"].items()
    ]
    lines += [
        "",
        "These are semantic decisions, separate from byte mismatches. A passing differential does not resolve them.",
        "",
        "## Rule coverage and SME checklist",
        "",
        "Trace links are explicit assertions plus normal-path synthetic fixtures, not measured branch coverage. Gaps remain gaps.",
        "",
        "| Rule | Golden fixture scope | Executed tests | Gaps | SME status / initials / comments |",
        "|---|---|---|---|---|",
    ]
    for rule in report["rules"]:
        sme = rule["sme"]
        cells = [
            rule["rule"],
            ", ".join(rule["fixtures"]) or "gap (Java-only)",
            "<br>".join(rule["tests"]) or "gap (no dedicated assertion)",
            rule["gaps"],
            " / ".join((sme["status"], sme["initials"], sme["comments"])),
        ]
        lines.append(
            "| "
            + " | ".join(c.replace("|", "\\|").replace("\n", " ") for c in cells)
            + " |"
        )
    lines += [
        "",
        f"SME checklist totals: `{json.dumps(dict(Counter(r['sme']['status'] for r in report['rules'])), sort_keys=True)}`.",
        "The checklist is reported as found, never approved by this generator. Machine-readable details and all test names are in `report.json`.",
        "",
    ]
    return "\n".join(lines)


def gate(report):
    return (
        report["maven_exit"] == 0
        and all(t["status"] == "passed" for t in report["tests"])
        and not report["disabled"]
        and not report["classification"].get("unresolved", 0)
        and not report["unclassified_differences"]
        and all(
            r["jar_exit"] == r["expected_exit"] and r["posting_counts_match"]
            for r in report["runs"]
        )
        and not any(
            c["partial_actual_bytes"] or c["partial_expected_bytes"]
            for c in report["comparisons"]
        )
    )


def generate(output, work):
    source = provenance()
    self_tests = command(
        [sys.executable, "-m", "unittest", "discover", "-s", HERE, "-p", "test_*.py"],
        work / "generator-tests.log",
    ).stdout.decode()
    self_test_summary = re.search(r"Ran (\d+) tests?", self_tests)
    if self_test_summary is None:
        raise RuntimeError("Missing generator regression test summary")
    self_test_count = int(self_test_summary.group(1))
    command(["bash", JAVA / "golden/run_golden.sh"], work / "golden.log")
    golden = JAVA / "golden/.build/generated"
    metadata = json.loads((golden / "metadata.json").read_text())
    build = command(
        [
            "mvn",
            "-B",
            "-ntp",
            "-Dstyle.color=never",
            "-f",
            JAVA / "pom.xml",
            "clean",
            "verify",
        ],
        work / "maven.log",
        check=False,
    )
    tests = read_tests()
    classpath = os.pathsep.join(
        str(JAVA / module / "target/classes") for module in CLASSES
    )
    command(
        [
            "javac",
            "--release",
            "21",
            "-Xlint:all",
            "-Werror",
            "-cp",
            classpath,
            "-d",
            work,
            HERE / "EvidenceExport.java",
        ]
    )
    ledger_path = work / "comparisons.csv.gz"
    comparisons, runs = [], []
    with (
        ledger_path.open("wb") as raw,
        gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=0) as compressed,
        io.TextIOWrapper(compressed, encoding="utf-8", newline="") as stream,
    ):
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(
            [
                "stage",
                "program",
                "fixture",
                "file",
                "record",
                "field",
                "classification",
                "expected_hex",
                "actual_hex",
                "rule_id",
                "reason",
            ]
        )
        for fixture in SETS:
            reference = golden / fixture
            actual = work / fixture
            command(
                [
                    "java",
                    "-cp",
                    classpath + os.pathsep + str(work),
                    "EvidenceExport",
                    reference,
                    actual,
                    metadata["clock_utc"].replace(" ", "T") + "Z",
                    metadata["parm_date"],
                ]
            )
            datasets = actual / "datasets"
            datasets.mkdir()
            for master in MASTERS:
                shutil.copyfile(
                    reference / f"initial.{master}.bin", datasets / f"{master}.bin"
                )
            shutil.copyfile(reference / "DALYTRAN.bin", datasets / "DALYTRAN.bin")
            jar = command(
                [
                    "java",
                    "-jar",
                    JAVA / "batch/target/batch-1.0-SNAPSHOT.jar",
                    f"--carddemo.datasets-dir={datasets}",
                    f"--carddemo.run-root={actual / 'runs'}",
                    "--carddemo.clock.fixed-instant="
                    + metadata["clock_utc"].replace(" ", "T")
                    + "Z",
                    "--carddemo.clock.zone=UTC",
                    "runId=" + fixture,
                    "parmDate=" + metadata["parm_date"],
                ],
                work / f"jar-{fixture}.log",
                check=False,
            )
            run_dir = actual / "runs" / fixture
            output_names = {
                "DALYREJS.bin",
                "SYSTRAN.bin",
                "CBTRN02C.sysout.txt",
                "CBACT04C.sysout.txt",
            }
            if {p.name for p in run_dir.iterdir()} != output_names:
                raise RuntimeError(
                    "Unexpected or missing jar output; see "
                    + str(work / f"jar-{fixture}.log")
                )
            pairs = []
            for stage, names in (
                (
                    "posting",
                    (
                        "CBTRN02C.TRANFILE.bin",
                        "DALYREJS.bin",
                        "CBTRN02C.ACCTFILE.bin",
                        "CBTRN02C.TCATBALF.bin",
                    ),
                ),
                (
                    "interest",
                    ("SYSTRAN.bin", "CBACT04C.ACCTFILE.bin", "CBACT04C.sysout.txt"),
                ),
            ):
                pairs += [(stage, name, actual / stage / name) for name in names]
            pairs += [("batch", name, run_dir / name) for name in sorted(output_names)]
            pairs += [
                ("batch", f"CBACT04C.{dd}.bin", datasets / f"{dd}.bin")
                for dd in MASTERS
            ]
            for stage, name, path in pairs:
                dd = name.split(".")[-2]
                fields = None if name.endswith(".txt") else layout(TYPES[dd])
                program = (
                    "CBTRN02C"
                    if stage == "posting"
                    or name.startswith("CBTRN02C")
                    or dd == "DALYREJS"
                    else "CBACT04C"
                )
                comparisons.append(
                    compare(
                        writer,
                        stage,
                        program,
                        fixture,
                        name,
                        (reference / name).read_bytes(),
                        path.read_bytes(),
                        fields,
                    )
                )
            rejects = reject_rows((actual / "posting/DALYREJS.bin").read_bytes())
            codes = json.loads((reference / "return-codes.json").read_text())
            verification = json.loads((reference / "verification.json").read_text())
            posting = json.loads((actual / "posting/result.json").read_text())
            expected_posting = {
                "posted": verification["accepted"],
                "rejected": verification["daily_records"] - verification["accepted"],
                "return_code": codes["CBTRN02C"],
            }
            runs.append(
                {
                    "fixture": fixture,
                    "posting": posting,
                    "expected_posting": expected_posting,
                    "posting_counts_match": posting == expected_posting,
                    "jar_exit": jar.returncode,
                    "expected_exit": max(codes.values()),
                    "program_return_codes": codes,
                    "rejects": rejects,
                    "reject_counts": dict(
                        sorted(Counter(r["reason_code"] for r in rejects).items())
                    ),
                }
            )
    classifications: Counter[str] = Counter()
    for c in comparisons:
        classifications.update(c["records"])
        classifications.update(c["fields"])
    report = {
        "clock": metadata,
        "provenance": source,
        "comparisons": comparisons,
        "runs": runs,
        "tests": tests,
        "disabled": disabled_annotations(),
        "rules": catalog(tests),
        "sme_pending_behaviors": PENDING,
        "classification": dict(classifications),
        "unclassified_differences": sum(
            count
            for status, count in classifications.items()
            if status not in ("match", "intended change", "unresolved")
        ),
        "generator_tests": self_test_count,
        "maven_exit": build.returncode,
        "ledger_sha256": digest(ledger_path.read_bytes()),
    }
    report["gate_passed"] = gate(report)
    output.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(ledger_path, output / ledger_path.name)
    (output / "report.json").write_text(
        json.dumps(report, indent=2, ensure_ascii=True) + "\n"
    )
    (output / "REPORT.md").write_text(render(report))
    for line in (work / "golden.log").read_text().splitlines():
        print(line)
    for line in (work / "maven.log").read_text().splitlines():
        if any(
            token in line
            for token in (
                "records compared",
                "lines compared",
                "DIFFERENTIAL ",
                "E2E ",
                "Tests run:",
                "BUILD SUCCESS",
                "BUILD FAILURE",
            )
        ):
            print(line)
    for run in runs:
        print(
            f"JAR {run['fixture']}: exit={run['jar_exit']} expected={run['expected_exit']}"
        )
    print(f"Generator regression tests: {self_test_count} passed")
    print("CLASSIFICATION " + json.dumps(report["classification"], sort_keys=True))
    print(
        f"Unclassified differences: {report['unclassified_differences']}; gate={'PASS' if report['gate_passed'] else 'FAIL'}"
    )
    return report["gate_passed"]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        type=Path,
        default=HERE,
        help="Destination of the three generated artifacts",
    )
    parser.add_argument(
        "--work",
        type=Path,
        help="Retain raw logs and service/jar outputs in a NEW directory",
    )
    args = parser.parse_args()
    if args.work:
        args.work = args.work.resolve()
        args.work.mkdir(parents=True, exist_ok=False)
        passed = generate(args.output.resolve(), args.work)
    else:
        with tempfile.TemporaryDirectory(prefix="carddemo-evidence-") as directory:
            passed = generate(args.output.resolve(), Path(directory))
    raise SystemExit(0 if passed else 1)


if __name__ == "__main__":
    main()

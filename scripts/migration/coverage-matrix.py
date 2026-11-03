#!/usr/bin/env python3
"""Regenerates the online-flow section of docs/migration/coverage-matrix.md from the flow tests.

Each online flow test carries @CobolFlow(program, paragraph, key) and its method name starts with
<PROGRAM>_<PARAGRAPH>_ (FlowCoverage enforces both). This script reads them from the test sources, checks
that the method name and the annotation agree, and rewrites the block between the GENERATED markers.

  scripts/migration/coverage-matrix.py           rewrite the block
  scripts/migration/coverage-matrix.py --check   exit 1 if the committed block is stale
"""
import re
import sys
from collections import OrderedDict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "docs/migration/coverage-matrix.md"
BEGIN = "<!-- BEGIN GENERATED: online flow tests (scripts/migration/coverage-matrix.py) -->"
END = "<!-- END GENERATED: online flow tests -->"

FLOW = re.compile(
    r'@CobolFlow\(\s*program\s*=\s*"(?P<program>[A-Z0-9]+)"\s*,\s*paragraph\s*=\s*"(?P<paragraph>[^"]+)"\s*,'
    r'\s*key\s*=\s*ScreenKey\.(?P<key>[A-Z0-9]+)\s*\)'
    r'(?:\s*@\w+(?:\([^)]*\))?)*\s*(?:public\s+)?void\s+(?P<method>\w+)\s*\(')


def flow_tests():
    tests = []
    problems = []
    for path in sorted(ROOT.glob("modernization/*/src/test/java/**/flow/*.java")):
        text = path.read_text(encoding="utf-8")
        found = list(FLOW.finditer(text))
        if text.count("@CobolFlow(") != len(found):
            problems.append(f"{path}: {text.count('@CobolFlow(')} @CobolFlow, {len(found)} parsed")
        module = path.relative_to(ROOT / "modernization").parts[0]
        for m in found:
            prefix = f"{m['program']}_{m['paragraph'].replace('-', '_')}_"
            if not m["method"].startswith(prefix):
                problems.append(f"{path.name}.{m['method']}: name does not start with {prefix}")
            tests.append(dict(m.groupdict(), module=module, cls=path.stem))
    if problems:
        sys.exit("coverage-matrix.py:\n  " + "\n  ".join(problems))
    return tests


def render(tests):
    programs = OrderedDict()
    for t in sorted(tests, key=lambda t: t["program"]):
        programs.setdefault(t["program"], []).append(t)
    lines = [
        BEGIN,
        "",
        f"{len(tests)} `@CobolFlow` tests over {len(programs)} programs, read from the test sources "
        "(`modernization/*/src/test/java/**/flow/`). Do not edit by hand.",
        "",
        "| Program | Module | Test class | Paragraphs (screen keys) | Tests |",
        "|---|---|---|---|---|",
    ]
    for program, ts in programs.items():
        paragraphs = OrderedDict()
        for t in ts:
            paragraphs.setdefault(t["paragraph"], [])
            if t["key"] not in paragraphs[t["paragraph"]]:
                paragraphs[t["paragraph"]].append(t["key"])
        modules = ", ".join(sorted({f"`{t['module']}`" for t in ts}))
        classes = ", ".join(sorted({f"`{t['cls']}`" for t in ts}))
        paras = "<br>".join(f"{p} ({', '.join(k)})" for p, k in paragraphs.items())
        lines.append(f"| {program} | {modules} | {classes} | {paras} | {len(ts)} |")
    lines += ["", "<details><summary>Test methods</summary>", ""]
    for program, ts in programs.items():
        lines.append(f"- **{program}**: " + ", ".join(f"`{t['method']}`" for t in ts))
    lines += ["", "</details>", "", END]
    return "\n".join(lines)


def main():
    doc = DOC.read_text(encoding="utf-8")
    start, end = doc.index(BEGIN), doc.index(END) + len(END)
    updated = doc[:start] + render(flow_tests()) + doc[end:]
    if "--check" in sys.argv[1:]:
        if updated != doc:
            sys.exit(f"{DOC.relative_to(ROOT)} is stale: run scripts/migration/coverage-matrix.py")
        print(f"{DOC.relative_to(ROOT)}: online flow section up to date")
        return
    DOC.write_text(updated, encoding="utf-8")
    print(f"wrote {DOC.relative_to(ROOT)}")


if __name__ == "__main__":
    main()

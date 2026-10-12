#!/usr/bin/env python3
"""Paste a run_tests.sh log into docs/sustainment/cbtrn04c/test-evidence.md.

usage: bash tests/cbtrn04c/run_tests.sh 2>&1 | tee run.log
       python3 tests/cbtrn04c/tools/paste_run_output.py run.log

Replaces the fenced block between the run-output markers so the evidence
document never carries a hand-edited copy of the test output.
"""
import os
import re
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
DOC = os.path.join(ROOT, "docs", "sustainment", "cbtrn04c", "test-evidence.md")


def main(argv) -> int:
    if len(argv) != 2:
        sys.stderr.write(__doc__)
        return 2
    with open(argv[1], encoding="utf-8", errors="replace") as fh:
        log = fh.read().rstrip("\n")
    with open(DOC, encoding="utf-8") as fh:
        doc = fh.read()
    new, n = re.subn(
        r"(<!-- run-output:begin -->\n)```text\n.*?```(\n<!-- run-output:end -->)",
        lambda m: m.group(1) + "```text\n" + log + "\n```" + m.group(2),
        doc, count=1, flags=re.S)
    if n != 1:
        sys.stderr.write("run-output markers not found in test-evidence.md\n")
        return 1
    with open(DOC, "w", encoding="utf-8") as fh:
        fh.write(new)
    print("paste_run_output: test-evidence.md updated")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

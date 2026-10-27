#!/usr/bin/env python3
"""Mechanical checks for the CBACT04C / CBTRN02C business-rules catalog.

Checks, per catalog file:
  1. Rule ids are unique, well formed and numbered without gaps.
  2. Every rule row cites at least one app/cbl or app/cpy line range.
  3. Every `path:line[-line]` cite in the catalog resolves (file exists,
     range inside the file, at least one non-blank line), and every
     .cbl/.cpy/.jcl line reference is written as such a backticked cite.
  4. Every paragraph of the PROCEDURE DIVISION is listed in the paragraph
     map at its real header line.
  5. Every decision line (IF, ELSE, INVALID KEY, NOT INVALID KEY,
     PERFORM UNTIL) of the PROCEDURE DIVISION falls inside a branch-map row,
     and every branch-map row maps to known rule ids or "no business logic".
     Each row must lie inside the paragraph it names, start on a decision
     line, end on a decision line or END- terminator, and not overlap
     another row.
  6. The SME checklist lists every rule id exactly once and nothing else.
  7. No banned claim words appear.

Usage:
  python3 check_catalog.py [--ref <git-ref>] [--show]

Before the real check it runs a self-test: tampered copies of the catalog
(a catch-all branch range, a cite past end of file, an unquoted source
reference, an unknown rule id) must
each be rejected.

--ref reads the COBOL sources from a git ref (e.g. origin/main) instead of
the working tree. --show prints the source lines behind each rule cite.
"""
import argparse
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", "..", "..", ".."))

CATALOGS = [
    ("CBACT04C.md", "INT", "app/cbl/CBACT04C.cbl"),
    ("CBTRN02C.md", "POST", "app/cbl/CBTRN02C.cbl"),
]
CHECKLIST = "SME-CHECKLIST.md"
BANNED = re.compile(r"production[- ]ready|certified|compliant", re.I)
CITE = re.compile(r"`(app/[A-Za-z0-9_./-]+):(\d+)(?:-(\d+))?`")
# Any source reference with a line number, quoted or not; each must be a CITE.
ANY_REF = re.compile(r"(?:app/)?[A-Za-z0-9_./-]*\.(?:cbl|cpy|jcl):\d+(?:-\d+)?", re.I)
PARAGRAPH = re.compile(r"^ {7}([A-Z0-9][A-Z0-9-]*)\.\s*$")
DECISION = re.compile(r"(?<![-\w])IF\b|\bELSE\b|\bINVALID KEY\b|\bPERFORM UNTIL\b")
END_SCOPE = re.compile(r"\bEND-(IF|READ|REWRITE|PERFORM)\b")
MAIN_LINE = "main line"
NBL = "no business logic"

_cache = {}


def source_lines(path, ref):
    key = (path, ref)
    if key not in _cache:
        if ref:
            out = subprocess.run(["git", "-C", REPO, "show", f"{ref}:{path}"],
                                 capture_output=True, text=True)
            _cache[key] = out.stdout.splitlines() if out.returncode == 0 else None
        else:
            full = os.path.join(REPO, path)
            if os.path.isfile(full):
                with open(full, encoding="utf-8", errors="replace") as fh:
                    _cache[key] = fh.read().splitlines()
            else:
                _cache[key] = None
    return _cache[key]


def table_rows(text, heading):
    """Rows of the first Markdown table under a '## heading' section."""
    rows, inside = [], False
    for line in text.splitlines():
        if line.startswith("## "):
            inside = line[3:].strip().lower().startswith(heading.lower())
            continue
        if inside and line.startswith("|"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if set("".join(cells)) <= set("-: "):
                continue
            rows.append(cells)
    return rows[1:] if rows else []


def is_code(line):
    return len(line) > 6 and line[6] not in "*/"


def procedure_division(lines):
    start = next(i for i, l in enumerate(lines) if "PROCEDURE DIVISION" in l and is_code(l))
    return start + 1, len(lines)


def parse_range(text):
    m = re.fullmatch(r"(\d+)(?:-(\d+))?", text.strip())
    if not m:
        return None
    a = int(m.group(1))
    return a, int(m.group(2) or a)


def check_cites(name, text, ref, errors):
    quoted = {m.span(0) for m in CITE.finditer(text)}
    for m in ANY_REF.finditer(text):
        a, b = m.span(0)
        if not any(qa < a and b < qb for qa, qb in quoted):
            errors.append(f"{name}: source reference {m.group(0)!r} is not a backticked app/... cite")
    count = 0
    for m in CITE.finditer(text):
        path, a = m.group(1), int(m.group(2))
        b = int(m.group(3) or a)
        lines = source_lines(path, ref)
        count += 1
        if lines is None:
            errors.append(f"{name}: cite {m.group(0)} -> file not found")
        elif not (1 <= a <= b <= len(lines)):
            errors.append(f"{name}: cite {m.group(0)} -> outside 1..{len(lines)}")
        elif not any(l.strip() for l in lines[a - 1:b]):
            errors.append(f"{name}: cite {m.group(0)} -> only blank lines")
    return count


def check_catalog(fname, prefix, program, ref, show, errors, text=None):
    if text is None:
        with open(os.path.join(HERE, fname), encoding="utf-8") as fh:
            text = fh.read()
    if BANNED.search(text):
        errors.append(f"{fname}: banned claim word: {BANNED.search(text).group(0)}")

    rules = []
    for cells in table_rows(text, "Rules"):
        rid = cells[0]
        if not re.fullmatch(rf"{prefix}-\d\d", rid):
            errors.append(f"{fname}: bad rule id {rid!r}")
            continue
        rules.append(rid)
        row = " | ".join(cells)
        cites = [m for m in CITE.finditer(row) if m.group(1).startswith(("app/cbl/", "app/cpy/"))]
        if not cites:
            errors.append(f"{fname}: {rid} has no app/cbl or app/cpy cite")
        if show:
            print(f"--- {rid}")
            for m in CITE.finditer(row):
                a = int(m.group(2)); b = int(m.group(3) or a)
                lines = source_lines(m.group(1), ref) or []
                for n in range(a, min(b, len(lines)) + 1):
                    print(f"{m.group(1)}:{n}: {lines[n - 1].rstrip()}")
    if len(set(rules)) != len(rules):
        errors.append(f"{fname}: duplicate rule ids")
    expected = [f"{prefix}-{i:02d}" for i in range(1, len(rules) + 1)]
    if rules != expected:
        errors.append(f"{fname}: rule ids not numbered 01..{len(rules):02d} in order")
    known = set(rules)
    ncites = check_cites(fname, text, ref, errors)

    lines = source_lines(program, ref)
    first, last = procedure_division(lines)

    # Paragraph map
    paragraphs = {}
    for n in range(first, last + 1):
        m = PARAGRAPH.match(lines[n - 1])
        if m:
            paragraphs[m.group(1)] = n
    mapped = {}
    for cells in table_rows(text, "Paragraph map"):
        para, line, target = cells[0].strip("`"), cells[1], cells[2]
        mapped[para] = line
        if para not in paragraphs:
            errors.append(f"{fname}: paragraph map lists unknown paragraph {para}")
        elif str(paragraphs[para]) != line:
            errors.append(f"{fname}: paragraph {para} is at line {paragraphs[para]}, map says {line}")
        check_targets(fname, f"paragraph {para}", target, known, errors)
    for para in paragraphs:
        if para not in mapped:
            errors.append(f"{fname}: paragraph {para} (line {paragraphs[para]}) missing from paragraph map")

    # Branch map: each row must sit inside its declared paragraph, start on a
    # decision line, end on a decision line or scope terminator, and not
    # overlap another row, so a catch-all range cannot stand in for real rows.
    decisions = [n for n in range(first, last + 1)
                 if is_code(lines[n - 1]) and DECISION.search(lines[n - 1][7:72])]
    headers = sorted(paragraphs.values())
    spans = {MAIN_LINE: (first, (headers[0] - 1) if headers else last)}
    for i, n in enumerate(headers):
        name = next(k for k, v in paragraphs.items() if v == n)
        spans[name] = (n, headers[i + 1] - 1 if i + 1 < len(headers) else last)
    ranges = []
    for cells in table_rows(text, "Branch map"):
        r = parse_range(cells[0])
        if not r or r[0] > r[1]:
            errors.append(f"{fname}: bad branch-map line range {cells[0]!r}")
            continue
        a, b = r
        para = cells[1].strip("`")
        span = spans.get(para)
        if span is None:
            errors.append(f"{fname}: branch {cells[0]} names unknown paragraph {para!r}")
        elif not (span[0] <= a <= b <= span[1]):
            errors.append(f"{fname}: branch {cells[0]} is outside {para} (lines {span[0]}-{span[1]})")
        if a not in decisions:
            errors.append(f"{fname}: branch {cells[0]} does not start on a decision line")
        if b <= len(lines) and b not in decisions and not END_SCOPE.search(lines[b - 1][7:72]):
            errors.append(f"{fname}: branch {cells[0]} does not end on a decision line or END- terminator")
        for pa, pb in ranges:
            if a <= pb and pa <= b:
                errors.append(f"{fname}: branch {cells[0]} overlaps {pa}-{pb}")
        ranges.append(r)
        check_targets(fname, f"branch {cells[0]}", cells[-1], known, errors)
    for n in decisions:
        if not any(a <= n <= b for a, b in ranges):
            errors.append(f"{fname}: decision at {program}:{n} not in branch map: {lines[n - 1].strip()}")
    return rules, ncites, len(paragraphs), len(decisions)


def check_targets(fname, where, target, known, errors):
    if target.strip().lower() == NBL:
        return
    ids = re.findall(r"[A-Z]+-\d\d", target)
    if not ids:
        errors.append(f"{fname}: {where} maps to neither rule ids nor '{NBL}': {target!r}")
    for rid in ids:
        if rid not in known:
            errors.append(f"{fname}: {where} maps to unknown rule {rid}")


def self_test(ref):
    """Negative cases: each tampered catalog must be rejected."""
    fname, prefix, program = CATALOGS[0]
    with open(os.path.join(HERE, fname), encoding="utf-8") as fh:
        good = fh.read()
    head, rest = good.split("## Branch map", 1)
    tail = rest[rest.index("\n## ", 1):]
    catch_all = ("## Branch map\n\n| Lines | Paragraph | Decision | Maps to |\n"
                 "|---|---|---|---|\n| 1-99999 | main line | all decisions | INT-01 |\n")
    cases = {
        "oversized branch range": head + catch_all + tail,
        "main-line range spanning paragraphs": head + catch_all.replace("1-99999", "188-647") + tail,
        "cite past end of file": good.replace("CBACT04C.cbl:462-465", "CBACT04C.cbl:462-999"),
        "unquoted source reference": good.replace("`app/cbl/CBACT04C.cbl:467`", "app/cbl/CBACT04C.cbl:467"),
        "unknown rule in branch map": good.replace("| 214-217 | main line | rate not zero | INT-10 |",
                                                   "| 214-217 | main line | rate not zero | INT-99 |"),
    }
    failed = 0
    for name, text in cases.items():
        errors = []
        check_catalog(fname, prefix, program, ref, False, errors, text=text)
        ok = bool(errors)
        failed += not ok
        print(f"self-test {name}: {'rejected' if ok else 'NOT REJECTED'}"
              + (f" ({errors[0]})" if ok else ""))
    return failed


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ref")
    ap.add_argument("--show", action="store_true")
    args = ap.parse_args()
    if self_test(args.ref):
        print("RESULT: FAIL (self-test)")
        return 1
    errors, all_rules = [], []
    for fname, prefix, program in CATALOGS:
        rules, ncites, npara, ndec = check_catalog(fname, prefix, program, args.ref, args.show, errors)
        all_rules += rules
        print(f"{fname}: {len(rules)} rules, {ncites} cites, "
              f"{npara} paragraphs mapped, {ndec} decision lines covered")

    with open(os.path.join(HERE, CHECKLIST), encoding="utf-8") as fh:
        checklist = fh.read()
    if BANNED.search(checklist):
        errors.append(f"{CHECKLIST}: banned claim word: {BANNED.search(checklist).group(0)}")
    listed = re.findall(r"^\|\s*((?:INT|POST)-\d\d)\s*\|", checklist, re.M)
    for rid in all_rules:
        if listed.count(rid) != 1:
            errors.append(f"{CHECKLIST}: {rid} listed {listed.count(rid)} times")
    for rid in set(listed) - set(all_rules):
        errors.append(f"{CHECKLIST}: lists unknown rule {rid}")
    print(f"{CHECKLIST}: {len(listed)} rows for {len(all_rules)} rules")

    for e in errors:
        print("FAIL", e)
    print("RESULT:", "FAIL" if errors else "PASS",
          f"({len(errors)} problems)" if errors else "")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())

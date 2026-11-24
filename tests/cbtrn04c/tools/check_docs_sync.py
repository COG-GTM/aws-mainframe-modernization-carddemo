#!/usr/bin/env python3
"""Keep docs/sustainment/cbtrn04c in step with the source and the tests.

Checks (bidirectional wherever a set is involved):
  * reason codes in CBTRN04C WS-REASON-TABLE-DATA == reason codes in the
    rule table of change-request.md
  * test case directories under tests/cbtrn04c/cases == case names named
    in change-request.md
  * every `path:line` citation in change-request.md points at an existing,
    non-blank line
  * the Numbers table in change-request.md (rules, citations, cases,
    confirmed/inferred split, decisions, sample-data totals) equals the
    values derived from the rule table, the case directories,
    government-decisions.md and the sample_data expected report
  * test-evidence.md contains the sample_data expected report verbatim
Exit status 0 when everything agrees, 1 otherwise.
"""
import os
import re
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
DOCS = os.path.join(ROOT, "docs", "sustainment", "cbtrn04c")
CASES = os.path.join(ROOT, "tests", "cbtrn04c", "cases")
PROGRAM = os.path.join(ROOT, "app", "cbl", "CBTRN04C.cbl")
CASE_NAME = re.compile(
    r"(rule\d\d_|precedence_|parm_|file_error_|sample_|clean_|empty_|all_|mixed_)\w+")

errors = []


def err(msg):
    errors.append(msg)


def read(path):
    with open(path, encoding="utf-8") as fh:
        return fh.read()


def block(text, name):
    m = re.search(rf"<!-- {name}:begin -->\n(.*?)<!-- {name}:end -->", text, re.S)
    if not m:
        err(f"marker block '{name}' not found")
        return ""
    return m.group(1)


def table_rows(md):
    rows = []
    for line in md.splitlines():
        if not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if all(re.fullmatch(r"-+", c) for c in cells):
            continue
        rows.append(cells)
    return rows[1:] if rows else []       # drop header


# ---------------------------------------------------------------- source
program = read(PROGRAM)
src_codes = set(re.findall(r"^\s+'(\d{4})[A-Z]", program, re.M))

cr = read(os.path.join(DOCS, "change-request.md"))
rules = table_rows(block(cr, "rules"))
doc_codes = {r[2] for r in rules if re.fullmatch(r"\d{4}", r[2])}
if src_codes != doc_codes:
    err(f"reason codes differ: source {sorted(src_codes)} docs {sorted(doc_codes)}")

# ---------------------------------------------------------------- cases
case_dirs = {d for d in os.listdir(CASES)
             if os.path.isdir(os.path.join(CASES, d))}
doc_cases = {c for c in re.findall(r"`([a-z0-9_]+)`", cr) if CASE_NAME.fullmatch(c)}
if case_dirs - doc_cases:
    err(f"cases missing from change-request.md: {sorted(case_dirs - doc_cases)}")
if doc_cases - case_dirs:
    err(f"change-request.md names cases that do not exist: "
        f"{sorted(doc_cases - case_dirs)}")

# ---------------------------------------------------------------- citations
cites = set(re.findall(r"`((?:app|tests)/[\w./-]+):(\d+)`", cr))
for path, line in sorted(cites):
    full = os.path.join(ROOT, path)
    if not os.path.isfile(full):
        err(f"cited file missing: {path}")
        continue
    lines = read(full).splitlines()
    n = int(line)
    if n < 1 or n > len(lines) or not lines[n - 1].strip():
        err(f"citation {path}:{line} does not point at a non-blank line")

rule_cites = set()
for r in rules:
    rule_cites |= set(re.findall(r"`((?:app|tests)/[\w./-]+:\d+)`", r[3]))
    if not re.findall(r"`((?:app|tests)/[\w./-]+:\d+)`", r[3]):
        err(f"rule {r[0]} has no path:line citation")
    if not re.findall(r"`([a-z0-9_]+)`", r[5]):
        err(f"rule {r[0]} has no test case")

# ---------------------------------------------------------------- numbers
numbers = {r[0]: r[1] for r in table_rows(block(cr, "numbers"))}
confirmed = sum(1 for r in rules if r[4].startswith("Confirmed"))
inferred = sum(1 for r in rules if r[4].startswith("Inferred"))
if confirmed + inferred != len(rules):
    err("every rule must be Confirmed or Inferred")

gd = read(os.path.join(DOCS, "government-decisions.md"))
decisions = [r for r in table_rows(block(gd, "decisions"))
             if re.fullmatch(r"\d+", r[0])]

sample_rpt = read(os.path.join(CASES, "sample_data", "expected", "valdrpt.txt"))


def rpt_value(label):
    m = re.search(rf"^ {re.escape(label)}\s*: *(\S.*?)\s*$", sample_rpt, re.M)
    return m.group(1) if m else None


sample_rc = read(os.path.join(CASES, "sample_data", "expected", "rc")).strip()
run_date_jul = re.search(r"RUN DATE \(JULIAN\)  : (\d{7})", sample_rpt)

expected = {
    "Validation rules implemented": str(len(rules)),
    "Source citations (distinct `path:line`)": str(len(rule_cites)),
    "Test cases": str(len(case_dirs)),
    "Rules confirmed by repository source": str(confirmed),
    "Rules inferred (need owner confirmation)": str(inferred),
    "Government decisions listed": str(len(decisions)),
    "Sample-data records read": rpt_value("RECORDS READ"),
    "Sample-data records accepted": rpt_value("RECORDS ACCEPTED"),
    "Sample-data records rejected": rpt_value("RECORDS REJECTED"),
    "Sample-data accepted amount total": rpt_value("ACCEPTED AMOUNT TOTAL"),
    "Sample-data rejected amount total":
        rpt_value("REJECTED AMOUNT TOTAL (NUMERIC AMOUNTS ONLY)"),
    "Sample-data run date (Julian)":
        run_date_jul.group(1) if run_date_jul else None,
    "Sample-data accepted origination range (Julian)":
        rpt_value("ACCEPTED ORIGINATION DATE RANGE (JULIAN YYYYDDD)"),
    "Sample-data return code": sample_rc,
}
for key, want in expected.items():
    got = numbers.get(key)
    if got is None:
        err(f"Numbers table lacks row '{key}'")
    elif got != want:
        err(f"Numbers row '{key}': doc says {got}, derived value is {want}")
for key in numbers:
    if key not in expected:
        err(f"Numbers table has a row this check does not derive: '{key}'")

headline = re.search(
    r"Headline: \*\*(\d+) validation rules .*?; (\d+) test cases", cr, re.S)
if not headline:
    err("headline sentence not found")
elif (headline.group(1) != str(len(rules))
      or headline.group(2) != str(len(case_dirs))):
    err(f"headline says {headline.group(1)} rules / {headline.group(2)} cases, "
        f"derived {len(rules)} / {len(case_dirs)}")

# ---------------------------------------------------------------- evidence
te = read(os.path.join(DOCS, "test-evidence.md"))
rpt_clean = "\n".join(l.rstrip() for l in sample_rpt.splitlines()).strip("\n")
if rpt_clean not in te:
    err("test-evidence.md does not contain the sample_data expected report")

run_out = block(te, "run-output")
m = re.search(r"^cases passed: (\d+)$", run_out, re.M)
if not m:
    err("test-evidence.md run output has no 'cases passed' summary")
elif m.group(1) != str(len(case_dirs)):
    err(f"test-evidence.md run output shows {m.group(1)} cases passed; "
        f"{len(case_dirs)} cases exist")
if re.search(r"^FAIL  ", run_out, re.M):
    err("test-evidence.md run output contains a failed case")
for name in sorted(case_dirs):
    if not re.search(rf"^PASS  {re.escape(name)} \(rc=\d+\)$", run_out, re.M):
        err(f"test-evidence.md run output has no PASS line for {name}")

if errors:
    for e in errors:
        print("check_docs_sync: " + e)
    sys.exit(1)
print(f"check_docs_sync: {len(rules)} rules, {len(rule_cites)} citations, "
      f"{len(case_dirs)} cases, {confirmed} confirmed / {inferred} inferred, "
      f"{len(decisions)} decisions, sample totals agree")

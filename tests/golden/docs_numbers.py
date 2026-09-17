#!/usr/bin/env python3
"""Derive every number quoted in docs/validation/golden-set/ from the artefacts
themselves (manifests, reconciliation reports, the copybook parser, mutate.py),
so the documentation cannot drift from the code.

    python3 tests/golden/docs_numbers.py                 # print the "Numbers" markdown block
    python3 tests/golden/docs_numbers.py --layouts       # print the layouts.md field tables
    python3 tests/golden/docs_numbers.py --check    # exit 1 if the README.md / layouts.md /
                                                    # findings.md blocks differ from what
                                                    # would be generated now
    python3 tests/golden/docs_numbers.py --write    # rewrite those blocks in place

The documents carry the generated text between marker comments:
    README.md    <!-- generated: numbers -->     ... <!-- /generated: numbers -->
    layouts.md   <!-- generated: layouts -->     ... <!-- /generated: layouts -->
    findings.md  <!-- generated: predictions --> ... <!-- /generated: predictions -->
(the last one lists every generator/program disagreement from prediction-check.json,
so a new disagreement fails selftest.sh until findings.md is regenerated)
Standard library only.
"""
import argparse
import json
import os
import re
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", ".."))
sys.path.insert(0, HERE)
import compare  # noqa: E402
import layouts  # noqa: E402
import mutate  # noqa: E402

SETS = ("named", "volume")
DOCS = os.path.join(REPO, "docs", "validation", "golden-set")
CANDIDATE_DIR = os.path.join(DOCS, "candidate-pr9")
SELFTEST_RESULT = os.path.join(HERE, "sets", "selftest-result.json")   # written by selftest.sh
OUTPUT_ORDER = ("TRANSACT", "DALYREJS", "ACCTFILE", "TCATBALF")
INPUT_ORDER = ("DALYTRAN", "XREFFILE", "ACCTFILE", "TCATBALF")


def load_json(path):
    with open(path) as fh:
        return json.load(fh)


def rel(p):
    return os.path.relpath(p, REPO)


def set_dir(s, sub):
    return os.path.join(HERE, "sets", s, sub)


def run_compare(expected, candidate):
    """Run compare.py in-process into a scratch directory; return its reconciliation.json."""
    with tempfile.TemporaryDirectory() as tmp:
        code = compare.main([expected, candidate, "--out-dir", tmp, "--quiet"])
        rep = load_json(os.path.join(tmp, "reconciliation.json"))
    assert rep["exit_code"] == code
    return rep


def fmt(n):
    return format(n, ",")


def collect():
    d = {"sets": {}}
    for s in SETS:
        man = load_json(os.path.join(set_dir(s, "input"), "manifest.json"))
        pred = load_json(os.path.join(set_dir(s, "expected"), "prediction-check.json"))
        run = load_json(os.path.join(set_dir(s, "expected"), "run.json"))
        exact = run_compare(set_dir(s, "expected"), set_dir(s, "expected"))
        variant = run_compare(set_dir(s, "expected"), set_dir(s, "expected-variant"))
        out_recs = {f["file"]: f["records_expected"] for f in variant["files"] if not f.get("text")}
        cand_path = os.path.join(CANDIDATE_DIR, s, "reconciliation.json")
        cand = load_json(cand_path) if os.path.exists(cand_path) else None
        d["sets"][s] = {
            "manifest": man, "prediction": pred, "run": run, "exact": exact, "variant": variant,
            "output_records": out_recs, "candidate": cand,
        }
    d["named_cases"] = len(d["sets"]["named"]["manifest"]["cases"])
    d["mutants"] = list(mutate.MUTANTS)
    d["selftest"] = load_json(SELFTEST_RESULT)
    expected_mutants = sorted("%s/%s" % (s, m) for s in SETS for m in d["mutants"])
    recorded = sorted(d["selftest"]["mutants"])
    if recorded != expected_mutants:
        raise SystemExit("%s does not list exactly the mutants of mutate.py x %s; rerun selftest.sh"
                         % (rel(SELFTEST_RESULT), "/".join(SETS)))
    d["layouts"] = {name: lay for name, lay in [
        ("DALYTRAN", layouts.DALYTRAN), ("XREFFILE", layouts.XREF), ("ACCTFILE", layouts.ACCT),
        ("TCATBALF", layouts.TCATBAL), ("TRANSACT", layouts.TRANSACT), ("DALYREJS", layouts.DALYREJS)]}
    return d


def numbers_block(d):
    named, volume = d["sets"]["named"], d["sets"]["volume"]
    n_mut = len(d["mutants"])
    n_sets = len(SETS)
    L = []
    for s in SETS:
        v = d["sets"][s]["variant"]["summary"]
        L.append("- **%s set, reference vs reference:** %s records, **%s fields reconciled, %s differences**"
                 " (GnuCOBOL primary run vs GnuCOBOL variant run; verdict `%s`, exit %d)."
                 % (s, fmt(v["records_reconciled"]), fmt(v["fields_reconciled"]),
                    fmt(v["field_differences"]), d["sets"][s]["variant"]["verdict"],
                    d["sets"][s]["variant"]["exit_code"]))
    st = d["selftest"]
    L.append("- **Injected defects:** %d mutant classes x %d sets = %d injected, %d caught"
             " (`tests/golden/selftest.sh`, recorded in `%s`)."
             % (n_mut, n_sets, len(st["mutants"]), len(st["caught"]), rel(SELFTEST_RESULT)))
    L.append("- **Tolerance path:** %d of %d checks passed (a bound that covers the one-cent defect gives"
             " `MATCH WITHIN TOLERANCE`, exit 3; a bound that does not still gives `MISMATCH`, exit 1;"
             " a tolerance naming an unknown field or a non-finite bound is refused, exit 64, no report)."
             % (len(st["tolerance_checks_passed"]), len(st["tolerance_checks"])))
    L.append("- **Absence path:** %d of %d checks passed (an exact copy with no input `DALYTRAN` reachable, and"
             " an exact copy missing its `RETURN-CODE`, both give `MISMATCH`, exit 1)."
             % (len(st["absence_checks_passed"]), len(st["absence_checks"])))
    L.append("- **SYSOUT policy:** %d of %d checks passed (an operator log differing only by edge whitespace is"
             " informational by default, exit 0, and a byte-for-byte `MISMATCH`, exit 1, under `--strict-sysout`)."
             % (len(st["sysout_policy_checks_passed"]), len(st["sysout_policy_checks"])))
    L.append("- **Pairing:** %d of %d checks passed (`RETURN-CODE` is compared as an integer: `04` equals `4`,"
             " exit 0, while two non-integer files are a `MISMATCH`, exit 1; two same-key `DALYREJS` records"
             " swapped in the candidate are `SAME RECORDS, DIFFERENT ORDER`, exit 2, with 0 field differences)."
             % (len(st["pairing_checks_passed"]), len(st["pairing_checks"])))
    L.append("")
    L.append("| Metric | Value | Derived from |")
    L.append("|---|---|---|")
    L.append("| Named cases | %d | `sets/named/input/manifest.json` -> `cases` |" % d["named_cases"])
    L.append("| Named-set DALYTRAN records | %s | `sets/named/input/manifest.json` -> `record_counts` |"
             % fmt(named["manifest"]["record_counts"]["DALYTRAN"]))
    L.append("| Volume-set DALYTRAN records | %s | `sets/volume/input/manifest.json` -> `record_counts` |"
             % fmt(volume["manifest"]["record_counts"]["DALYTRAN"]))
    for s in SETS:
        p = d["sets"][s]["prediction"]["actual"]
        L.append("| %s set: accepted / rejected (program outcome) | %s / %s; by reason %s | `sets/%s/expected/prediction-check.json` |"
                 % (s, fmt(p["accepted"]), fmt(p["rejected"]),
                    ", ".join("%s=%s" % (k, v) for k, v in sorted(p["rejected_by_reason"].items())), s))
    for s in SETS:
        L.append("| %s set: generator/program disagreements | %d of %d items checked | `sets/%s/expected/prediction-check.json` |"
                 % (s, len(d["sets"][s]["prediction"]["disagreements"]),
                    d["sets"][s]["prediction"]["items_checked"], s))
    for name in INPUT_ORDER:
        lay = d["layouts"][name]
        L.append("| Fields per record: %s (%s) | %d fields, %d bytes | `%s` via `layouts.py` |"
                 % (name, "input", len(lay.fields), lay.length, lay.source))
    for name in OUTPUT_ORDER:
        lay = d["layouts"][name]
        L.append("| Fields per record: %s (%s) | %d fields, %d bytes | `%s` via `layouts.py` |"
                 % (name, "output", len(lay.fields), lay.length, lay.source))
    for s in SETS:
        recs = d["sets"][s]["output_records"]
        L.append("| %s set: output records per file | %s | `compare.py` |"
                 % (s, ", ".join("%s=%s" % (n, fmt(recs[n])) for n in OUTPUT_ORDER)))
    for s in SETS:
        e = d["sets"][s]["exact"]["summary"]
        L.append("| %s set: expected vs exact copy | %s records, %s fields, %s differences, exit %d (`%s`) | `compare.py` |"
                 % (s, fmt(e["records_reconciled"]), fmt(e["fields_reconciled"]),
                    fmt(e["field_differences"]), d["sets"][s]["exact"]["exit_code"],
                    d["sets"][s]["exact"]["verdict"]))
    L.append("| Mutant classes | %d: %s | `mutate.py --list` |" % (n_mut, ", ".join("`%s`" % m for m in d["mutants"])))
    L.append("| Reference return code | %s | `sets/*/expected/run.json` |"
             % " / ".join("%s=%d" % (s, d["sets"][s]["run"]["return_code"]) for s in SETS))
    for s in SETS:
        c = d["sets"][s]["candidate"]
        if c is None:
            L.append("| Candidate (PR #9), %s set | not run - see `candidate-pr9/NOT-RUN.md` | |" % s)
            continue
        cs = c["summary"]
        rc = [f for f in c["files"] if f.get("text") and f["file"] == "RETURN-CODE"][0]
        L.append("| Candidate (PR #9), %s set | verdict `%s` (exit %d): %s records, %s fields reconciled, %s field differences, "
                 "%d control-total mismatches, RETURN-CODE expected `%s` got `%s` | `candidate-pr9/%s/reconciliation.json` |"
                 % (s, c["verdict"], c["exit_code"], fmt(cs["records_reconciled"]), fmt(cs["fields_reconciled"]),
                    fmt(cs["field_differences"]), cs["control_total_mismatches"],
                    rc["expected_text"], rc["candidate_text"], s))
    return "\n".join(L) + "\n"


def predictions_block(d):
    """findings.md section 1: generator/program disagreements, straight from prediction-check.json."""
    L = ["| set | records | items checked | disagreements |", "|---|---:|---:|---:|"]
    for s in SETS:
        p = d["sets"][s]["prediction"]
        L.append("| %s | %s | %s | %d |" % (s, fmt(p["records_in"]), fmt(p["items_checked"]), len(p["disagreements"])))
    L.append("")
    L.append("Source: " + " and ".join("`tests/golden/sets/%s/expected/prediction-check.json`" % s for s in SETS)
             + " (written by `check_prediction.py` at the end of every `run_reference.sh`).")
    L.append("Items checked = one per `DALYTRAN` record (accept/reject + reason), one per")
    L.append("closing account balance, one per closing category balance.")
    any_dis = False
    for s in SETS:
        for item in d["sets"][s]["prediction"]["disagreements"]:
            if not any_dis:
                L += ["", "| set | item | generator predicted | program did |", "|---|---|---|---|"]
                any_dis = True
            L.append("| %s | %s | %s | %s |" % (s, item.get("item", item), item.get("predicted", ""), item.get("actual", "")))
    if not any_dis:
        L.append("")
        L.append("No disagreement is currently recorded (`\"disagreements\": []` in both files).")
    return "\n".join(L) + "\n"


def layouts_block(d):
    L = []
    for name in ("DALYTRAN", "XREFFILE", "ACCTFILE", "TCATBALF", "TRANSACT", "DALYREJS"):
        lay = d["layouts"][name]
        role = {"DALYTRAN": "sequential input", "XREFFILE": "indexed input (random read)",
                "ACCTFILE": "indexed input/output (random read + rewrite)",
                "TCATBALF": "indexed input/output (random read + rewrite/write)",
                "TRANSACT": "indexed output", "DALYREJS": "sequential output"}[name]
        L.append("## `%s` - `%s`, %d bytes, %d elementary fields (%s)" % (name, lay.record_name, lay.length, len(lay.fields), role))
        L.append("")
        L.append("Source: `%s`%s" % (lay.source, "; key field `%s`" % lay.key_field if lay.key_field else ""))
        L.append("")
        L.append("| # | Field | Offset | Length | PIC | Usage | Defined at |")
        L.append("|---|---|---|---|---|---|---|")
        for i, f in enumerate(lay.fields, 1):
            usage = {"X": "alphanumeric", "9": "unsigned zoned decimal", "S9": "signed zoned decimal (trailing overpunch)"}[f.usage]
            if f.scale:
                usage += ", %d implied decimals" % f.scale
            L.append("| %d | `%s` | %d | %d | `%s` | %s | `%s` |" % (i, f.name, f.offset, f.length, f.pic, usage, f.source))
        L.append("")
    return "\n".join(L)


MARK = "<!-- generated: %s -->"
ENDMARK = "<!-- /generated: %s -->"


def check_block(path, tag, expected):
    text = open(path).read()
    m = re.search(re.escape(MARK % tag) + r"\n(.*?)" + re.escape(ENDMARK % tag), text, re.S)
    if not m:
        return "%s: markers for '%s' not found" % (rel(path), tag)
    if m.group(1).strip() != expected.strip():
        return "%s: generated block '%s' is stale; regenerate with docs_numbers.py" % (rel(path), tag)
    return None


def write_block(path, tag, content):
    text = open(path).read()
    pat = re.compile(re.escape(MARK % tag) + r"\n.*?" + re.escape(ENDMARK % tag), re.S)
    if not pat.search(text):
        raise SystemExit("%s: markers for '%s' not found" % (rel(path), tag))
    new = pat.sub(lambda _m: "%s\n%s\n%s" % (MARK % tag, content.strip(), ENDMARK % tag), text, count=1)
    if new != text:
        open(path, "w").write(new)
        print("docs_numbers.py --write: updated block '%s' in %s" % (tag, rel(path)))
    else:
        print("docs_numbers.py --write: block '%s' in %s already current" % (tag, rel(path)))


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--layouts", action="store_true", help="print the layouts.md tables instead of the numbers block")
    ap.add_argument("--check", action="store_true", help="verify README.md, layouts.md and findings.md generated blocks are current")
    ap.add_argument("--write", action="store_true", help="rewrite the generated blocks in README.md, layouts.md and findings.md in place")
    ap.add_argument("--json", action="store_true", help="print the headline figures as JSON")
    args = ap.parse_args(argv)
    d = collect()
    blocks = (
        (os.path.join(DOCS, "README.md"), "numbers", numbers_block),
        (os.path.join(DOCS, "layouts.md"), "layouts", layouts_block),
        (os.path.join(DOCS, "findings.md"), "predictions", predictions_block),
    )
    if args.write:
        for path, tag, fn in blocks:
            write_block(path, tag, fn(d))
        return 0
    if args.check:
        problems = [p for p in (check_block(path, tag, fn(d)) for path, tag, fn in blocks) if p]
        for p in problems:
            print("docs_numbers.py --check: " + p)
        if not problems:
            print("docs_numbers.py --check: README.md, layouts.md and findings.md generated blocks match the artefacts")
        return 1 if problems else 0
    if args.layouts:
        print(layouts_block(d))
        return 0
    if args.json:
        v = d["sets"]["volume"]["variant"]["summary"]
        print(json.dumps({
            "named_cases": d["named_cases"],
            "volume_records": d["sets"]["volume"]["manifest"]["record_counts"]["DALYTRAN"],
            "volume_fields_reconciled": v["fields_reconciled"], "volume_records_reconciled": v["records_reconciled"],
            "volume_differences": v["field_differences"],
            "mutants_injected": len(d["selftest"]["mutants"]), "mutants_caught": len(d["selftest"]["caught"]),
        }, indent=2))
        return 0
    print(numbers_block(d))
    return 0


if __name__ == "__main__":
    sys.exit(main())

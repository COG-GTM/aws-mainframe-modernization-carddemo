#!/usr/bin/env python3
"""Reconcile a candidate posting-cycle output directory against a golden
(expected) output directory produced by run_reference.sh.

  python3 tests/golden/compare.py <expected_dir> <candidate_dir> [options]

Checks, in order:
  (a) byte-for-byte per file (sha256 + length)
  (b) record-for-record, splitting every record into named copybook fields
      (layouts.py) so a difference reads
        TRANSACT record 17 (TRAN-ID=...) field TRAN-AMT: expected +000000123.45 got +000000123.54
  (c) control totals on both sides: records in, accepted, rejected, rejected by
      reason code, sum of accepted amounts, sum of rejected amounts, closing
      balance per account and per category, and the identity in = accepted + rejected
  (d) order sensitivity: whether the candidate holds the same records in a different order
  (e) reconciliation.json + reconciliation.md written to --out-dir (default: candidate dir)

Exit codes:
  0  every record file (TRANSACT, DALYREJS, ACCTFILE, TCATBALF) and RETURN-CODE
     match byte-for-byte
  2  every record and every control total matches, but at least one file
     holds the same records in a different order
  3  every difference lies within a tolerance named on the command line
     (never reachable without --tolerance; the reports carry a banner)
  1  any missing/extra record, any field difference, any control-total
     mismatch, any RETURN-CODE difference or missing RETURN-CODE, missing
     record file or malformed record, or a missing/malformed input DALYTRAN
     (default <expected_dir>/../input, override with --input-dir) so that
     records_in and the identity in = accepted + rejected cannot be formed

SYSOUT (the program's operator log: DISPLAY output) is compared and reported
but is informational only, because it is not a posting-cycle data output;
--strict-sysout makes a SYSOUT difference a mismatch (exit 1).

There are NO tolerances by default.  --tolerance FIELD=ABS makes differences
of at most ABS in the named numeric field non-fatal, and a control total
derived from that field may then differ by at most the sum of the absorbed
per-record differences; every tolerance used is named in both reports.

Standard library only.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import sys
from decimal import Decimal, InvalidOperation
from typing import Dict, List, Optional, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import layouts  # noqa: E402
from layouts import DALYREJS, DALYTRAN, OUTPUT_LAYOUTS, TEXT_OUTPUTS, decode_zoned, format_field, record_key  # noqa: E402

RECORD_FILES = ("TRANSACT", "DALYREJS", "ACCTFILE", "TCATBALF")
MAX_LISTED_DIFFS = 200


# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def read_bytes(path: str) -> Optional[bytes]:
    if not os.path.isfile(path):
        return None
    with open(path, "rb") as fh:
        return fh.read()


def key_text(file_name: str, rec: bytes) -> str:
    return record_key(file_name, rec).decode("ascii", errors="replace")


def printable(s: str) -> str:
    """Render raw field bytes (latin-1 text) with non-printables escaped as \\xNN."""
    return "".join(c if 0x20 <= ord(c) < 0x7F and c != "`" else "\\x%02x" % ord(c) for c in s)


def numeric_value(raw: bytes, f: layouts.Field) -> Optional[Decimal]:
    try:
        return decode_zoned(raw, f)
    except ValueError:
        return None


def classify_text_diff(exp: bytes, got: bytes) -> str:
    if exp.rstrip(b" \x00") == got.rstrip(b" \x00"):
        return "trailing-space"       # same text, different trailing padding (space vs low-value)
    if exp.strip(b" ") == got.strip(b" "):
        return "leading-space"
    if exp.lower() == got.lower():
        return "case"
    return "content"


# ---------------------------------------------------------------------------
# record-level comparison
# ---------------------------------------------------------------------------

class Tolerances:
    def __init__(self, specs: List[str]):
        self.abs: Dict[str, Decimal] = {}
        for s in specs:
            if "=" not in s:
                raise SystemExit("--tolerance expects FIELD=ABS, got %r" % s)
            name, val = s.split("=", 1)
            self.abs[name.strip()] = abs(Decimal(val))
        self.used: List[dict] = []

    def within(self, field: str, exp: Optional[Decimal], got: Optional[Decimal]) -> bool:
        if field not in self.abs or exp is None or got is None:
            return False
        ok = abs(exp - got) <= self.abs[field]
        if ok:
            self.used.append({"field": field, "expected": str(exp), "got": str(got),
                              "tolerance": str(self.abs[field])})
        return ok


def index_records(file_name: str, recs: List[bytes]) -> Dict[Tuple[str, int], int]:
    """(key, occurrence) -> position, so duplicate keys still pair up."""
    seen: Dict[str, int] = {}
    out: Dict[Tuple[str, int], int] = {}
    for i, r in enumerate(recs):
        k = key_text(file_name, r)
        n = seen.get(k, 0)
        seen[k] = n + 1
        out[(k, n)] = i
    return out


def compare_record_file(name: str, exp_data: Optional[bytes], got_data: Optional[bytes],
                        tol: Tolerances) -> dict:
    layout = OUTPUT_LAYOUTS[name]
    res: dict = {
        "file": name, "layout": layout.name, "record_length": layout.length,
        "fields_per_record": len(layout.fields),
        "expected": {"present": exp_data is not None}, "candidate": {"present": got_data is not None},
        "byte_identical": False, "records_expected": 0, "records_candidate": 0,
        "records_matched": 0, "fields_compared": 0, "field_differences": [],
        "within_tolerance": [], "missing_records": [], "extra_records": [],
        "same_records_different_order": False, "errors": [],
    }
    if exp_data is None:
        res["errors"].append("expected file missing")
        return res
    res["expected"].update({"bytes": len(exp_data), "sha256": sha256(exp_data)})
    if got_data is None:
        res["errors"].append("candidate file missing")
        return res
    res["candidate"].update({"bytes": len(got_data), "sha256": sha256(got_data)})
    res["byte_identical"] = exp_data == got_data

    try:
        exp_recs = layout.records(exp_data)
        got_recs = layout.records(got_data)
    except ValueError as e:
        res["errors"].append(str(e))
        return res
    res["records_expected"] = len(exp_recs)
    res["records_candidate"] = len(got_recs)

    exp_idx = index_records(name, exp_recs)
    got_idx = index_records(name, got_recs)
    common = [k for k in exp_idx if k in got_idx]
    res["missing_records"] = [{"position": exp_idx[k] + 1, "key": k[0]} for k in exp_idx if k not in got_idx]
    res["extra_records"] = [{"position": got_idx[k] + 1, "key": k[0]} for k in got_idx if k not in exp_idx]
    res["records_matched"] = len(common)

    diffs = res["field_differences"]
    for k in common:
        ei, gi = exp_idx[k], got_idx[k]
        er, gr = exp_recs[ei], got_recs[gi]
        for f in layout.fields:
            res["fields_compared"] += 1
            eb, gb = er[f.offset:f.end], gr[f.offset:f.end]
            if eb == gb:
                continue
            d = {"record": ei + 1, "candidate_record": gi + 1, "key": k[0], "field": f.name,
                 "pic": f.pic, "offset": f.offset, "length": f.length,
                 "expected": format_field(eb, f), "got": format_field(gb, f),
                 "expected_raw": eb.decode("latin-1"), "got_raw": gb.decode("latin-1")}
            if f.is_numeric:
                ev, gv = numeric_value(eb, f), numeric_value(gb, f)
                if ev is not None and gv is not None:
                    d["delta"] = str(gv - ev)
                if ev is not None and gv is not None and ev == gv:
                    d["kind"] = "numeric-encoding"      # same value, different bytes (e.g. sign zone)
                elif ev is None or gv is None:
                    d["kind"] = "invalid-numeric"
                elif (ev < 0) != (gv < 0) and abs(ev) == abs(gv):
                    d["kind"] = "sign"
                else:
                    d["kind"] = "value"
                if tol.within(f.name, ev, gv):
                    d["tolerance"] = str(tol.abs[f.name])
                    res["within_tolerance"].append(d)
                    continue
            else:
                d["kind"] = classify_text_diff(eb, gb)
            diffs.append(d)

    if (not res["missing_records"] and not res["extra_records"] and not diffs
            and len(exp_recs) == len(got_recs)):
        exp_order = [key_text(name, r) for r in exp_recs]
        got_order = [key_text(name, r) for r in got_recs]
        res["same_records_different_order"] = exp_order != got_order
    return res


# ---------------------------------------------------------------------------
# control totals
# ---------------------------------------------------------------------------

def control_totals(d: str, records_in: Optional[int]) -> dict:
    t: dict = {"records_in": records_in, "accepted": None, "rejected": None,
               "rejected_by_reason": {}, "sum_accepted_amount": None, "sum_rejected_amount": None,
               "closing_account_balances": {}, "closing_category_balances": {},
               "identity_in_equals_accepted_plus_rejected": None, "errors": []}
    tr = read_bytes(os.path.join(d, "TRANSACT"))
    rj = read_bytes(os.path.join(d, "DALYREJS"))
    ac = read_bytes(os.path.join(d, "ACCTFILE"))
    tc = read_bytes(os.path.join(d, "TCATBALF"))
    try:
        if tr is not None:
            recs = OUTPUT_LAYOUTS["TRANSACT"].records(tr)
            f = OUTPUT_LAYOUTS["TRANSACT"].field("TRAN-AMT")
            t["accepted"] = len(recs)
            t["sum_accepted_amount"] = str(sum((decode_zoned(r[f.offset:f.end], f) for r in recs), Decimal("0.00")))
        if rj is not None:
            recs = DALYREJS.records(rj)
            fa = DALYREJS.field("DALYTRAN-AMT")
            fr = DALYREJS.field("WS-VALIDATION-FAIL-REASON")
            t["rejected"] = len(recs)
            t["sum_rejected_amount"] = str(sum((decode_zoned(r[fa.offset:fa.end], fa) for r in recs), Decimal("0.00")))
            for r in recs:
                k = "%04d" % int(decode_zoned(r[fr.offset:fr.end], fr))
                t["rejected_by_reason"][k] = t["rejected_by_reason"].get(k, 0) + 1
        if ac is not None:
            lay = OUTPUT_LAYOUTS["ACCTFILE"]
            for r in lay.records(ac):
                t["closing_account_balances"][lay.slice(r, "ACCT-ID").decode("ascii", "replace")] = {
                    n: str(decode_zoned(lay.slice(r, n), lay.field(n)))
                    for n in ("ACCT-CURR-BAL", "ACCT-CURR-CYC-CREDIT", "ACCT-CURR-CYC-DEBIT")}
        if tc is not None:
            lay = OUTPUT_LAYOUTS["TCATBALF"]
            f = lay.field("TRAN-CAT-BAL")
            for r in lay.records(tc):
                t["closing_category_balances"][key_text("TCATBALF", r)] = str(decode_zoned(r[f.offset:f.end], f))
        if records_in is not None and t["accepted"] is not None and t["rejected"] is not None:
            t["identity_in_equals_accepted_plus_rejected"] = (records_in == t["accepted"] + t["rejected"])
    except ValueError as e:
        t["errors"].append(str(e))
    return t


def compare_totals(exp: dict, got: dict) -> List[dict]:
    out = []

    def chk(name, a, b):
        if a != b:
            out.append({"total": name, "expected": a, "got": b})

    for k in ("records_in", "accepted", "rejected", "sum_accepted_amount", "sum_rejected_amount",
              "identity_in_equals_accepted_plus_rejected"):
        chk(k, exp.get(k), got.get(k))
    for code in sorted(set(exp["rejected_by_reason"]) | set(got["rejected_by_reason"])):
        chk("rejected_by_reason[%s]" % code, exp["rejected_by_reason"].get(code, 0), got["rejected_by_reason"].get(code, 0))
    for acct in sorted(set(exp["closing_account_balances"]) | set(got["closing_account_balances"])):
        chk("closing_account_balances[%s]" % acct, exp["closing_account_balances"].get(acct),
            got["closing_account_balances"].get(acct))
    for key in sorted(set(exp["closing_category_balances"]) | set(got["closing_category_balances"])):
        chk("closing_category_balances[%s]" % key, exp["closing_category_balances"].get(key),
            got["closing_category_balances"].get(key))
    if exp.get("identity_in_equals_accepted_plus_rejected") is False:
        out.append({"total": "identity_in_equals_accepted_plus_rejected", "expected": True, "got": False,
                    "side": "expected"})
    if got.get("identity_in_equals_accepted_plus_rejected") is False:
        out.append({"total": "identity_in_equals_accepted_plus_rejected", "expected": True, "got": False,
                    "side": "candidate"})
    return out


# control total -> the record field it is derived from (sums) or made of (per-record values)
SUM_TOTAL_FIELD = {"sum_accepted_amount": "TRAN-AMT", "sum_rejected_amount": "DALYTRAN-AMT"}
CATEGORY_TOTAL_FIELD = "TRAN-CAT-BAL"


def absorb_total_mismatches(mismatches: List[dict], files: List[dict], tol: Tolerances) -> Tuple[List[dict], List[dict]]:
    """Split control-total mismatches into (still mismatched, explained by tolerances).

    A sum may differ by at most the sum of the per-record differences that were
    absorbed for its source field; a per-record closing balance may differ by at
    most that field's tolerance.  Without --tolerance nothing is absorbed."""
    if not tol.abs:
        return mismatches, []
    allowance: Dict[str, Decimal] = {}
    for f in files:
        for d in f.get("within_tolerance", []):
            allowance[d["field"]] = allowance.get(d["field"], Decimal(0)) + abs(Decimal(d["delta"]))

    def dec(v) -> Optional[Decimal]:
        if isinstance(v, bool) or v is None:
            return None
        try:
            return Decimal(str(v))
        except InvalidOperation:
            return None

    def within(field: str, a, b, bound: Optional[Decimal]) -> bool:
        da, db = dec(a), dec(b)
        return bound is not None and da is not None and db is not None and abs(da - db) <= bound

    keep, absorbed = [], []
    for m in mismatches:
        name, ok = m["total"], False
        if name in SUM_TOTAL_FIELD:
            fld = SUM_TOTAL_FIELD[name]
            ok = within(fld, m["expected"], m["got"], allowance.get(fld))
            m = dict(m, tolerance_field=fld)
        elif name.startswith("closing_category_balances["):
            ok = within(CATEGORY_TOTAL_FIELD, m["expected"], m["got"], tol.abs.get(CATEGORY_TOTAL_FIELD))
            m = dict(m, tolerance_field=CATEGORY_TOTAL_FIELD)
        elif name.startswith("closing_account_balances[") and isinstance(m["expected"], dict) \
                and isinstance(m["got"], dict) and set(m["expected"]) == set(m["got"]):
            ok = all(m["expected"][k] == m["got"][k] or within(k, m["expected"][k], m["got"][k], tol.abs.get(k))
                     for k in m["expected"])
            m = dict(m, tolerance_field=",".join(k for k in m["expected"] if m["expected"][k] != m["got"][k]))
        (absorbed if ok else keep).append(m)
    return keep, absorbed


# ---------------------------------------------------------------------------
# reports
# ---------------------------------------------------------------------------

def render_md(r: dict) -> str:
    L: List[str] = []
    L.append("# Reconciliation report")
    L.append("")
    L.append("| | |")
    L.append("|---|---|")
    L.append("| expected | `%s` |" % r["expected_dir"])
    L.append("| candidate | `%s` |" % r["candidate_dir"])
    L.append("| verdict | **%s** (exit %d) |" % (r["verdict"], r["exit_code"]))
    L.append("| records reconciled | %s |" % format(r["summary"]["records_reconciled"], ","))
    L.append("| fields reconciled | **%s** |" % format(r["summary"]["fields_reconciled"], ","))
    L.append("| field differences | %s |" % format(r["summary"]["field_differences"], ","))
    L.append("| missing / extra records | %d / %d |" % (r["summary"]["missing_records"], r["summary"]["extra_records"]))
    L.append("| control-total mismatches | %d |" % r["summary"]["control_total_mismatches"])
    L.append("| order-only differences | %s |" % ", ".join(r["summary"]["order_only_files"] or ["none"]))
    L.append("| RETURN-CODE | %s |" % ("**MISMATCH**" if r["summary"]["return_code_mismatch"] else "same"))
    L.append("| tolerances in effect | %s |" % (
        ", ".join("`%s` ±%s" % (k, v) for k, v in r["tolerances"]["abs"].items()) or "**none** (default)"))
    L.append("| input (records in) | `%s`%s |" % (r["input_dir"], "" if not r["input_errors"] else " **ERROR**"))
    L.append("")
    for e in r["input_errors"]:
        L.append("* INPUT ERROR: %s" % e)
    if r["input_errors"]:
        L.append("")
    if r["tolerances"]["abs"]:
        L.append("> **Tolerance policy in effect:** %d field difference(s) and %d control-total difference(s) "
                 "were accepted as within the named tolerances above and do not affect the verdict."
                 % (len(r["tolerances"]["used"]), len(r["control_totals"]["within_tolerance"])))
        L.append("")
    L.append("## Files")
    L.append("")
    L.append("| file | layout | rec len | fields/rec | expected recs | candidate recs | byte-identical | matched | fields compared | differences | order |")
    L.append("|---|---|---:|---:|---:|---:|---|---:|---:|---:|---|")
    for f in r["files"]:
        if f.get("text"):
            continue
        L.append("| %s | %s | %d | %d | %d | %d | %s | %d | %s | %d | %s |" % (
            f["file"], f["layout"], f["record_length"], f["fields_per_record"],
            f["records_expected"], f["records_candidate"],
            "yes" if f["byte_identical"] else "**no**", f["records_matched"],
            format(f["fields_compared"], ","), len(f["field_differences"]),
            "**different**" if f["same_records_different_order"] else "same"))
    L.append("")
    L.append("| text output | expected | candidate | status |")
    L.append("|---|---|---|---|")
    for f in r["files"]:
        if not f.get("text"):
            continue
        L.append("| %s | `%s` | `%s` | %s |" % (
            f["file"], f["expected_text"], f["candidate_text"], f["status"]))
    L.append("")
    for f in r["files"]:
        if f.get("text"):
            continue
        if f["errors"] or f["missing_records"] or f["extra_records"] or f["field_differences"] or f["within_tolerance"]:
            L.append("### %s" % f["file"])
            L.append("")
            for e in f["errors"]:
                L.append("* ERROR: %s" % e)
            for m in f["missing_records"][:MAX_LISTED_DIFFS]:
                L.append("* missing in candidate: expected record %d (key `%s`)" % (m["position"], m["key"]))
            for m in f["extra_records"][:MAX_LISTED_DIFFS]:
                L.append("* extra in candidate: candidate record %d (key `%s`)" % (m["position"], m["key"]))
            for d in f["field_differences"][:MAX_LISTED_DIFFS]:
                L.append("* %s record %d (key `%s`) field **%s** [%s @%d+%d, %s]: expected %s got %s"
                         " (raw `%s` vs `%s`)" % (
                    f["file"], d["record"], d["key"], d["field"], d["pic"], d["offset"], d["length"],
                    d["kind"], d["expected"], d["got"], printable(d["expected_raw"]), printable(d["got_raw"])))
            if len(f["field_differences"]) > MAX_LISTED_DIFFS:
                L.append("* ... %d more field differences (see reconciliation.json)" % (
                    len(f["field_differences"]) - MAX_LISTED_DIFFS))
            for d in f["within_tolerance"]:
                L.append("* WITHIN TOLERANCE ±%s: %s record %d field **%s**: expected %s got %s" % (
                    d["tolerance"], f["file"], d["record"], d["field"], d["expected"], d["got"]))
            L.append("")
    L.append("## Control totals")
    L.append("")
    e, g = r["control_totals"]["expected"], r["control_totals"]["candidate"]
    L.append("| total | expected | candidate | match |")
    L.append("|---|---:|---:|---|")
    for k in ("records_in", "accepted", "rejected", "sum_accepted_amount", "sum_rejected_amount",
              "identity_in_equals_accepted_plus_rejected"):
        L.append("| %s | %s | %s | %s |" % (k, e.get(k), g.get(k), "yes" if e.get(k) == g.get(k) else "**no**"))
    for code in sorted(set(e["rejected_by_reason"]) | set(g["rejected_by_reason"])):
        a, b = e["rejected_by_reason"].get(code, 0), g["rejected_by_reason"].get(code, 0)
        L.append("| rejected_by_reason[%s] | %s | %s | %s |" % (code, a, b, "yes" if a == b else "**no**"))
    n_acct = len(e["closing_account_balances"])
    n_cat = len(e["closing_category_balances"])
    acct_mism = [m for m in r["control_totals"]["mismatches"] if m["total"].startswith("closing_account")]
    cat_mism = [m for m in r["control_totals"]["mismatches"] if m["total"].startswith("closing_category")]
    L.append("| closing_account_balances (%d accounts) | | | %s |" % (
        n_acct, "yes" if not acct_mism else "**%d differ**" % len(acct_mism)))
    L.append("| closing_category_balances (%d keys) | | | %s |" % (
        n_cat, "yes" if not cat_mism else "**%d differ**" % len(cat_mism)))
    L.append("")
    if r["control_totals"]["mismatches"]:
        L.append("### Control-total mismatches")
        L.append("")
        for m in r["control_totals"]["mismatches"][:MAX_LISTED_DIFFS]:
            L.append("* %s: expected `%s` got `%s`%s" % (
                m["total"], m["expected"], m["got"], " (%s side)" % m["side"] if "side" in m else ""))
        L.append("")
    if r["control_totals"]["within_tolerance"]:
        L.append("### Control totals WITHIN TOLERANCE")
        L.append("")
        for m in r["control_totals"]["within_tolerance"]:
            L.append("* WITHIN TOLERANCE (`%s`): %s: expected `%s` got `%s`" % (
                m["tolerance_field"], m["total"], m["expected"], m["got"]))
        L.append("")
    L.append("## Order sensitivity")
    L.append("")
    if r["summary"]["order_only_files"]:
        L.append("The following files contain the same records with identical field contents but in a "
                 "different sequence: %s." % ", ".join("`%s`" % x for x in r["summary"]["order_only_files"]))
    else:
        L.append("No file differs only in record order.")
    L.append("")
    L.append("_Generated by tests/golden/compare.py; layouts derived from app/cpy copybooks by tests/golden/layouts.py._")
    return "\n".join(L) + "\n"


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("expected_dir")
    ap.add_argument("candidate_dir")
    ap.add_argument("--out-dir", help="where to write reconciliation.json/.md (default: candidate_dir)")
    ap.add_argument("--input-dir", help="directory holding the DALYTRAN input, for the 'records in' total "
                                        "(default: <expected_dir>/../input); missing or malformed is a mismatch")
    ap.add_argument("--tolerance", action="append", default=[], metavar="FIELD=ABS",
                    help="absolute tolerance for a numeric field; NONE by default; named in the report")
    ap.add_argument("--strict-sysout", action="store_true",
                    help="treat SYSOUT text differences as a mismatch (default: informational)")
    ap.add_argument("--quiet", action="store_true")
    args = ap.parse_args(argv)

    exp_dir, got_dir = os.path.abspath(args.expected_dir), os.path.abspath(args.candidate_dir)
    out_dir = os.path.abspath(args.out_dir or got_dir)

    def shown(p):
        """Path as written in the reports: relative to the working directory when it lies
        beneath it, so committed reports do not embed one machine's absolute paths."""
        rel = os.path.relpath(p)
        return rel if not rel.startswith("..") else p
    tol = Tolerances(args.tolerance)

    input_dir = args.input_dir or os.path.join(os.path.dirname(exp_dir), "input")
    records_in = None
    input_errors: List[str] = []
    daly = read_bytes(os.path.join(input_dir, "DALYTRAN"))
    if daly is None:
        input_errors.append("input DALYTRAN missing in %s (pass --input-dir); records_in and the identity "
                            "in = accepted + rejected cannot be reconciled" % shown(input_dir))
    elif len(daly) % DALYTRAN.length != 0:
        input_errors.append("input DALYTRAN in %s is %d bytes, not a multiple of the %d-byte %s record"
                            % (shown(input_dir), len(daly), DALYTRAN.length, DALYTRAN.name))
    else:
        records_in = len(daly) // DALYTRAN.length

    files = []
    fatal = False
    for name in RECORD_FILES:
        fr = compare_record_file(name, read_bytes(os.path.join(exp_dir, name)),
                                 read_bytes(os.path.join(got_dir, name)), tol)
        files.append(fr)
    for name in TEXT_OUTPUTS:
        eb, gb = read_bytes(os.path.join(exp_dir, name)), read_bytes(os.path.join(got_dir, name))
        et = eb.decode("utf-8", "replace").strip() if eb is not None else None
        gt = gb.decode("utf-8", "replace").strip() if gb is not None else None
        missing = eb is None or gb is None
        same = not missing and et == gt
        if name == "RETURN-CODE":
            status = "match" if same else ("**MISMATCH** (missing)" if missing else "**MISMATCH**")
            if not same:
                fatal = True
        else:
            status = "match" if same else ("**MISMATCH** (--strict-sysout)" if args.strict_sysout
                                           else "differs (informational; operator log)")
            if not same and args.strict_sysout:
                fatal = True
        files.append({"file": name, "text": True, "status": status, "byte_identical": eb is not None and eb == gb,
                      "expected_text": (et if name == "RETURN-CODE" else "%d bytes" % len(eb or b"")) if eb is not None else "missing",
                      "candidate_text": (gt if name == "RETURN-CODE" else "%d bytes" % len(gb or b"")) if gb is not None else "missing"})

    tot_e = control_totals(exp_dir, records_in)
    tot_g = control_totals(got_dir, records_in)
    tot_mismatch, tot_absorbed = absorb_total_mismatches(compare_totals(tot_e, tot_g), files, tol)

    rec_files = [f for f in files if not f.get("text")]
    n_fields = sum(f["fields_compared"] for f in rec_files)
    n_recs = sum(f["records_matched"] for f in rec_files)
    n_diff = sum(len(f["field_differences"]) for f in rec_files)
    n_missing = sum(len(f["missing_records"]) for f in rec_files)
    n_extra = sum(len(f["extra_records"]) for f in rec_files)
    n_err = sum(len(f["errors"]) for f in rec_files)
    order_only = [f["file"] for f in rec_files if f["same_records_different_order"]]
    rc_mismatch = [f["file"] for f in files if f.get("text") and f["file"] == "RETURN-CODE"
                   and not f["byte_identical"]]
    all_bytes = all(f["byte_identical"] for f in rec_files) and not rc_mismatch

    if fatal or input_errors or n_diff or n_missing or n_extra or n_err or tot_mismatch or rc_mismatch:
        exit_code, verdict = 1, "MISMATCH"
    elif order_only:
        exit_code, verdict = 2, "SAME RECORDS, DIFFERENT ORDER"
    elif all_bytes:
        exit_code, verdict = 0, "EXACT MATCH"
    elif tol.used or tot_absorbed:
        # every field and total agrees once the named tolerances are applied; not byte-identical
        exit_code, verdict = 3, "MATCH WITHIN TOLERANCE"
    else:
        # bytes differ somewhere no named field covers (should not happen with complete layouts)
        exit_code, verdict = 1, "MISMATCH"

    report = {
        "tool": "tests/golden/compare.py",
        "expected_dir": shown(exp_dir), "candidate_dir": shown(got_dir), "input_dir": shown(input_dir),
        "input_errors": input_errors,
        "verdict": verdict, "exit_code": exit_code,
        "summary": {
            "records_reconciled": n_recs, "fields_reconciled": n_fields,
            "field_differences": n_diff, "missing_records": n_missing, "extra_records": n_extra,
            "control_total_mismatches": len(tot_mismatch), "order_only_files": order_only,
            "return_code_mismatch": bool(rc_mismatch),
            "files_byte_identical": [f["file"] for f in files if f["byte_identical"]],
            "files_not_byte_identical": [f["file"] for f in files if not f["byte_identical"]],
        },
        "tolerances": {"abs": {k: str(v) for k, v in tol.abs.items()}, "used": tol.used},
        "files": files,
        "control_totals": {"expected": tot_e, "candidate": tot_g, "mismatches": tot_mismatch,
                           "within_tolerance": tot_absorbed},
    }
    os.makedirs(out_dir, exist_ok=True)
    with open(os.path.join(out_dir, "reconciliation.json"), "w") as fh:
        json.dump(report, fh, indent=2)
        fh.write("\n")
    with open(os.path.join(out_dir, "reconciliation.md"), "w") as fh:
        fh.write(render_md(report))

    if not args.quiet:
        print("compare: %s vs %s" % (shown(exp_dir), shown(got_dir)))
        for e in input_errors:
            print("  INPUT ERROR: %s" % e)
        for f in rec_files:
            print("  %-9s recs expected=%d candidate=%d matched=%d fields=%s diffs=%d missing=%d extra=%d byte-identical=%s%s" % (
                f["file"], f["records_expected"], f["records_candidate"], f["records_matched"],
                format(f["fields_compared"], ","), len(f["field_differences"]), len(f["missing_records"]),
                len(f["extra_records"]), "yes" if f["byte_identical"] else "NO",
                " ORDER-DIFFERS" if f["same_records_different_order"] else ""))
            for e in f["errors"]:
                print("    ERROR: %s" % e)
            for d in f["field_differences"][:20]:
                print("    %s record %d (key %s) field %s: expected %s got %s [%s]" % (
                    f["file"], d["record"], d["key"], d["field"], d["expected"], d["got"], d["kind"]))
            for m in f["missing_records"][:20]:
                print("    missing in candidate: record %d key %s" % (m["position"], m["key"]))
            for m in f["extra_records"][:20]:
                print("    extra in candidate: record %d key %s" % (m["position"], m["key"]))
        for f in files:
            if f.get("text"):
                print("  %-11s %s" % (f["file"], f["status"].replace("*", "")))
        for m in tot_mismatch[:20]:
            print("  control total %s: expected %s got %s" % (m["total"], m["expected"], m["got"]))
        if tol.abs:
            print("  TOLERANCES IN EFFECT: %s (%d field differences, %d control-total differences absorbed)" % (
                ", ".join("%s=%s" % kv for kv in tol.abs.items()), len(tol.used), len(tot_absorbed)))
        print("  %s records, %s fields reconciled, %s differences%s; verdict %s (exit %d)" % (
            format(n_recs, ","), format(n_fields, ","), format(n_diff + n_missing + n_extra + len(tot_mismatch), ","),
            (" + RETURN-CODE mismatch" if rc_mismatch else "") + (" + input error" if input_errors else ""),
            verdict, exit_code))
        print("  reports: %s" % os.path.join(shown(out_dir), "reconciliation.{json,md}"))
    return exit_code


if __name__ == "__main__":
    sys.exit(main())

"""Reconciliation checks for the CBACT01C and CBTRN01C batch jobs.

The checks are documented in RECONCILIATION_CHECKS.md.  They operate on
the JSON record sets produced by ``records.py`` / ``generate_goldens.py``
so they can be run against the COBOL goldens *and* against the output of a
future Java port (point --golden-dir at the port's output directory).

Command line::

    python3 reconcile.py cbact01c --golden-dir golden-files/CBACT01C [--write]
    python3 reconcile.py cbtrn01c --golden-dir golden-files/CBTRN01C [--write]

--write stores the result as <golden-dir>/reconciliation.json.
Exit status is 0 only when every check passed.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from collections import Counter
from decimal import Decimal, InvalidOperation
from typing import Dict, Iterable, List, Optional

TWO = Decimal("0.01")
CYC_DEBIT_SUBSTITUTE = Decimal("2525.00")
ARR_CONSTANTS = {  # (occurrence, field) -> constant moved by 1400-POPUL-ARRAY-RECORD
    (1, "ARR-ACCT-CURR-CYC-DEBIT"): Decimal("1005.00"),
    (2, "ARR-ACCT-CURR-CYC-DEBIT"): Decimal("1525.00"),
    (3, "ARR-ACCT-CURR-BAL"): Decimal("-1025.00"),
    (3, "ARR-ACCT-CURR-CYC-DEBIT"): Decimal("-2500.00"),
}
OUTCOME_VERIFIED = "VERIFIED"
OUTCOME_CARD_MISSING = "CARD_NOT_FOUND"
OUTCOME_ACCT_MISSING = "ACCOUNT_NOT_FOUND"


def money(v) -> str:
    return str(Decimal(v).quantize(TWO))


def dsum(values: Iterable[str]) -> Decimal:
    return sum((Decimal(v) for v in values), Decimal(0))


def _dec_or_none(v) -> Optional[Decimal]:
    try:
        return Decimal(v)
    except (InvalidOperation, TypeError, ValueError):
        return None


class Report:
    def __init__(self, job: str):
        self.job = job
        self.checks: List[Dict] = []

    def check(self, cid: str, group: str, description: str, expected, actual, detail=None, skip_reason=None):
        ok = expected == actual
        entry = {"id": cid, "group": group, "description": description,
                 "expected": expected, "actual": actual, "status": "PASS" if ok else "FAIL"}
        if skip_reason is not None:
            entry["status"] = "SKIP"
            entry["skip_reason"] = skip_reason
        if detail is not None:
            entry["detail"] = detail
        self.checks.append(entry)
        return ok

    def result(self) -> Dict:
        failed = [c["id"] for c in self.checks if c["status"] == "FAIL"]
        skipped = [c["id"] for c in self.checks if c["status"] == "SKIP"]
        return {"job": self.job,
                "summary": {"checks": len(self.checks), "passed": len(self.checks) - len(failed) - len(skipped),
                            "failed": len(failed), "failed_ids": failed,
                            "skipped": len(skipped), "skipped_ids": skipped,
                            "status": "PASS" if not failed else "FAIL"},
                "checks": self.checks}


def _load(path: str):
    with open(path, encoding="utf-8") as fh:
        return json.load(fh)


# ----------------------------------------------------------------------
# CBACT01C
# ----------------------------------------------------------------------
def reconcile_cbact01c(acct_in: List[Dict], outfile: List[Dict], arryfile: List[Dict],
                       vbrcfile: List[Dict]) -> Dict:
    r = Report("CBACT01C")
    n = len(acct_in)
    vb1 = [x for x in vbrcfile if x.get("_record") == "VBRC-REC1"]
    vb2 = [x for x in vbrcfile if x.get("_record") == "VBRC-REC2"]

    # -- record counts ---------------------------------------------------
    r.check("CBACT01C-COUNT-01", "counts", "input accounts = OUTFILE records", n, len(outfile))
    r.check("CBACT01C-COUNT-02", "counts", "input accounts = ARRYFILE records", n, len(arryfile))
    r.check("CBACT01C-COUNT-03", "counts", "input accounts = VBRCFILE records / 2 (and the count is even)",
            {"accounts": n, "even": True}, {"accounts": len(vbrcfile) // 2, "even": len(vbrcfile) % 2 == 0})
    r.check("CBACT01C-COUNT-04", "counts", "VBRCFILE: one 12-byte VBRC-REC1 and one 39-byte VBRC-REC2 per account, alternating",
            {"rec1": n, "rec2": n, "alternating": True},
            {"rec1": len(vb1), "rec2": len(vb2),
             "alternating": all(x.get("_record") == ("VBRC-REC1" if i % 2 == 0 else "VBRC-REC2")
                                for i, x in enumerate(vbrcfile))})

    # -- field totals ----------------------------------------------------
    for fld in ("ACCT-CURR-BAL", "ACCT-CREDIT-LIMIT", "ACCT-CASH-CREDIT-LIMIT", "ACCT-CURR-CYC-CREDIT"):
        r.check("CBACT01C-TOTAL-" + fld, "totals", "sum %s in = sum OUT-%s" % (fld, fld),
                money(dsum(x[fld] for x in acct_in)), money(dsum(x["OUT-" + fld] for x in outfile)))
    # Legacy parity: OUT-ACCT-REC is never INITIALIZEd per record and the
    # program only MOVEs 2525.00 when the input debit is zero, so a non-zero
    # input leaves OUT-ACCT-CURR-CYC-DEBIT holding the previous record's
    # value.  Before the first zero-debit input that is the never-assigned
    # WORKING-STORAGE initial value: undefined on z/OS (NOWSCLEAR), LOW-VALUES
    # under GnuCOBOL (decoded as "INVALID-COMP-3:<hex>").  Those rows are
    # reported but excluded from the total.
    expected_rows: List[Optional[Decimal]] = []
    carry: Optional[Decimal] = None
    for x in acct_in:
        if Decimal(x["ACCT-CURR-CYC-DEBIT"]) == 0:
            carry = CYC_DEBIT_SUBSTITUTE
        expected_rows.append(carry)
    zero_in = sum(1 for x in acct_in if Decimal(x["ACCT-CURR-CYC-DEBIT"]) == 0)
    actual_rows = [_dec_or_none(x["OUT-ACCT-CURR-CYC-DEBIT"]) for x in outfile]
    undefined = [{"acct_id": x["OUT-ACCT-ID"], "actual": x["OUT-ACCT-CURR-CYC-DEBIT"]}
                 for x, e in zip(outfile, expected_rows) if e is None]
    mismatched = [{"acct_id": x["OUT-ACCT-ID"], "expected": money(e), "actual": x["OUT-ACCT-CURR-CYC-DEBIT"]}
                  for x, e, a in zip(outfile, expected_rows, actual_rows) if e is not None and e != a]
    defined_actual = [a for e, a in zip(expected_rows, actual_rows) if e is not None and a is not None]
    intent_total = dsum(CYC_DEBIT_SUBSTITUTE if Decimal(x["ACCT-CURR-CYC-DEBIT"]) == 0
                        else Decimal(x["ACCT-CURR-CYC-DEBIT"]) for x in acct_in)
    r.check("CBACT01C-TOTAL-CYC-DEBIT", "totals",
            "sum OUT-ACCT-CURR-CYC-DEBIT = legacy simulation (2525.00 where input ACCT-CURR-CYC-DEBIT = 0, "
            "otherwise the previous record's output value is retained; rows before the first zero input are "
            "undefined and excluded)",
            money(sum((e for e in expected_rows if e is not None), Decimal(0))), money(sum(defined_actual, Decimal(0))),
            {"inputs_with_zero_cyc_debit": zero_in, "inputs_with_nonzero_cyc_debit": n - zero_in,
             "undefined_rows": undefined,
             "business_intent_total_if_nonzero_inputs_were_copied": money(intent_total),
             "note": "the program only MOVEs when the input is zero and never re-initialises OUT-ACCT-REC; "
                     "see TEST_STRATEGY.md section 7"})
    r.check("CBACT01C-FIELD-CYC-DEBIT", "derived",
            "OUT-ACCT-CURR-CYC-DEBIT per record = legacy simulation (2525.00 on zero input, else carried value)",
            0, len(mismatched), {"mismatches": mismatched[:20], "undefined_rows": len(undefined)})
    # ARRYFILE constants and copies
    for (occ, fld), const in sorted(ARR_CONSTANTS.items()):
        r.check("CBACT01C-TOTAL-ARR-%s-%d" % (fld, occ), "totals",
                "sum %s(%d) = records x %s" % (fld, occ, const),
                money(const * len(arryfile)), money(dsum(x["ARR-ACCT-BAL"][occ - 1][fld] for x in arryfile)))
    for occ in (1, 2):
        r.check("CBACT01C-TOTAL-ARR-ACCT-CURR-BAL-%d" % occ, "totals",
                "sum ARR-ACCT-CURR-BAL(%d) = sum input ACCT-CURR-BAL" % occ,
                money(dsum(x["ACCT-CURR-BAL"] for x in acct_in)),
                money(dsum(x["ARR-ACCT-BAL"][occ - 1]["ARR-ACCT-CURR-BAL"] for x in arryfile)))
    for occ in (4, 5):
        r.check("CBACT01C-TOTAL-ARR-ZERO-%d" % occ, "totals",
                "ARR-ACCT-BAL(%d) is INITIALIZEd to zero (both fields) in every record" % occ,
                {"curr_bal": "0.00", "cyc_debit": "0.00"},
                {"curr_bal": money(dsum(x["ARR-ACCT-BAL"][occ - 1]["ARR-ACCT-CURR-BAL"] for x in arryfile)),
                 "cyc_debit": money(dsum(x["ARR-ACCT-BAL"][occ - 1]["ARR-ACCT-CURR-CYC-DEBIT"] for x in arryfile))})
    r.check("CBACT01C-TOTAL-VB2-CURR-BAL", "totals", "sum VB2-ACCT-CURR-BAL = sum input ACCT-CURR-BAL",
            money(dsum(x["ACCT-CURR-BAL"] for x in acct_in)), money(dsum(x["VB2-ACCT-CURR-BAL"] for x in vb2)))
    r.check("CBACT01C-TOTAL-VB2-CREDIT-LIMIT", "totals", "sum VB2-ACCT-CREDIT-LIMIT = sum input ACCT-CREDIT-LIMIT",
            money(dsum(x["ACCT-CREDIT-LIMIT"] for x in acct_in)), money(dsum(x["VB2-ACCT-CREDIT-LIMIT"] for x in vb2)))

    # -- derived fields (per record, reported as mismatch counts) --------
    by_id = {x["ACCT-ID"]: x for x in acct_in}
    bad_dates = [x["OUT-ACCT-ID"] for x in outfile
                 if x["OUT-ACCT-ID"] in by_id and x["OUT-ACCT-REISSUE-DATE"]
                 != _yyyymmdd(by_id[x["OUT-ACCT-ID"]]["ACCT-REISSUE-DATE"])]
    r.check("CBACT01C-FIELD-REISSUE-DATE", "derived", "OUT-ACCT-REISSUE-DATE = COBDATFT(YYYY-MM-DD -> YYYYMMDD) + 2 spaces",
            0, len(bad_dates), {"mismatching_ids": bad_dates[:20]})
    bad_yyyy = [x["VB2-ACCT-ID"] for x in vb2
                if x["VB2-ACCT-ID"] in by_id and x["VB2-ACCT-REISSUE-YYYY"] != by_id[x["VB2-ACCT-ID"]]["ACCT-REISSUE-DATE"][:4]]
    r.check("CBACT01C-FIELD-VB2-YYYY", "derived", "VB2-ACCT-REISSUE-YYYY = first 4 chars of input ACCT-REISSUE-DATE",
            0, len(bad_yyyy), {"mismatching_ids": bad_yyyy[:20]})
    bad_status = [x["VB1-ACCT-ID"] for x in vb1
                  if x["VB1-ACCT-ID"] in by_id and x["VB1-ACCT-ACTIVE-STATUS"] != by_id[x["VB1-ACCT-ID"]]["ACCT-ACTIVE-STATUS"]]
    r.check("CBACT01C-FIELD-VB1-STATUS", "derived", "VB1-ACCT-ACTIVE-STATUS = input ACCT-ACTIVE-STATUS",
            0, len(bad_status), {"mismatching_ids": bad_status[:20]})

    # -- cross-reference integrity --------------------------------------
    in_ids = Counter(x["ACCT-ID"] for x in acct_in)
    r.check("CBACT01C-XREF-00", "xref", "input ACCT-ID is unique (KSDS primary key)", 0,
            sum(1 for c in in_ids.values() if c > 1))
    for cid, label, ids in (
            ("CBACT01C-XREF-01", "OUT-ACCT-ID", [x["OUT-ACCT-ID"] for x in outfile]),
            ("CBACT01C-XREF-02", "ARR-ACCT-ID", [x["ARR-ACCT-ID"] for x in arryfile]),
            ("CBACT01C-XREF-03", "VB1-ACCT-ID", [x["VB1-ACCT-ID"] for x in vb1]),
            ("CBACT01C-XREF-04", "VB2-ACCT-ID", [x["VB2-ACCT-ID"] for x in vb2])):
        out_ids = Counter(ids)
        unknown = sorted(k for k in out_ids if k not in in_ids)
        dup = sorted(k for k, c in out_ids.items() if c > 1)
        missing = sorted(k for k in in_ids if k not in out_ids)
        r.check(cid, "xref", "every %s exists exactly once in the input and every input account appears once" % label,
                {"unknown": [], "duplicated": [], "missing": []},
                {"unknown": unknown, "duplicated": dup, "missing": missing})
    r.check("CBACT01C-XREF-05", "xref", "OUTFILE order = input key order (sequential KSDS read is ascending by ACCT-ID)",
            [x["ACCT-ID"] for x in sorted(acct_in, key=lambda x: int(x["ACCT-ID"]))] == [x["OUT-ACCT-ID"] for x in outfile],
            True)
    return r.result()


def _yyyymmdd(iso: str) -> str:
    """What COBDATFT type 2 -> 2 does, then MOVE X(20) -> X(10)."""
    return (iso[0:4] + iso[5:7] + iso[8:10]).ljust(10)


# ----------------------------------------------------------------------
# CBTRN01C
# ----------------------------------------------------------------------
def reconcile_cbtrn01c(tran_in: List[Dict], outcomes: List[Dict], xref: List[Dict],
                       acct: List[Dict], display_lookups: Optional[int] = None) -> Dict:
    r = Report("CBTRN01C")
    n = len(tran_in)
    classes = Counter(o["outcome"] for o in outcomes)
    verified = classes.get(OUTCOME_VERIFIED, 0)
    card_missing = classes.get(OUTCOME_CARD_MISSING, 0)
    acct_missing = classes.get(OUTCOME_ACCT_MISSING, 0)

    # -- record counts ---------------------------------------------------
    r.check("CBTRN01C-COUNT-01", "counts", "daily transactions in = outcome rows out", n, len(outcomes))
    r.check("CBTRN01C-COUNT-02", "counts", "verified + card-missing + account-missing = total",
            len(outcomes), verified + card_missing + acct_missing,
            {"verified": verified, "card_missing": card_missing, "account_missing": acct_missing,
             "unknown_outcomes": sorted(k for k in classes if k not in (OUTCOME_VERIFIED, OUTCOME_CARD_MISSING, OUTCOME_ACCT_MISSING))})
    out_of_order = [{"position": i + 1, "input": {"tran_id": t["DALYTRAN-ID"], "card_num": t["DALYTRAN-CARD-NUM"]},
                     "outcome": {"tran_id": o.get("tran_id"), "card_num": o.get("card_num")}}
                    for i, (t, o) in enumerate(zip(tran_in, outcomes))
                    if (t["DALYTRAN-ID"], t["DALYTRAN-CARD-NUM"]) != (o.get("tran_id"), o.get("card_num"))]
    r.check("CBTRN01C-COUNT-03", "counts",
            "outcome rows are in file order: row i has the DALYTRAN-ID and DALYTRAN-CARD-NUM of input record i",
            0, len(out_of_order), {"mismatches": out_of_order[:20]})
    r.check("CBTRN01C-COUNT-04", "counts",
            "XREF lookups in display = transactions + 1 (legacy quirk: the last record is looked up "
            "again after end-of-file because only the DISPLAY is guarded by the EOF flag)",
            n + 1, display_lookups,
            skip_reason=None if display_lookups is not None else "display.txt not present in the golden directory")

    # -- field totals ----------------------------------------------------
    amt_by_row = [Decimal(t["DALYTRAN-AMT"]) for t in tran_in]
    total = sum(amt_by_row, Decimal(0))
    per_class = {k: Decimal(0) for k in (OUTCOME_VERIFIED, OUTCOME_CARD_MISSING, OUTCOME_ACCT_MISSING)}
    for amt, o in zip(amt_by_row, outcomes):
        per_class[o["outcome"]] = per_class.get(o["outcome"], Decimal(0)) + amt
    r.check("CBTRN01C-TOTAL-01", "totals", "sum DALYTRAN-AMT overall (input) = sum of per-outcome-class totals",
            money(total), money(sum(per_class.values(), Decimal(0))),
            {k: money(v) for k, v in per_class.items()})
    r.check("CBTRN01C-TOTAL-02", "totals", "sum DALYTRAN-AMT per class (recorded for the port to match)",
            {k: money(v) for k, v in per_class.items()}, {k: money(v) for k, v in per_class.items()})
    r.check("CBTRN01C-TOTAL-03", "totals", "debit / credit split of DALYTRAN-AMT (positive vs negative amounts)",
            {"positive": money(sum((a for a in amt_by_row if a > 0), Decimal(0))),
             "negative": money(sum((a for a in amt_by_row if a < 0), Decimal(0))),
             "zero_count": sum(1 for a in amt_by_row if a == 0)},
            {"positive": money(sum((a for a in amt_by_row if a > 0), Decimal(0))),
             "negative": money(sum((a for a in amt_by_row if a < 0), Decimal(0))),
             "zero_count": sum(1 for a in amt_by_row if a == 0)})

    # -- cross-reference integrity --------------------------------------
    xref_by_card = {x["XREF-CARD-NUM"]: x for x in xref}
    acct_ids = {a["ACCT-ID"] for a in acct}
    bad_verified, bad_card_missing, bad_acct_missing = [], [], []
    for o in outcomes:
        card = o["card_num"]
        if o["outcome"] == OUTCOME_VERIFIED:
            x = xref_by_card.get(card)
            if x is None or not o["xref_found"] or x["XREF-ACCT-ID"] != o["acct_id"] \
                    or not o["acct_found"] or o["acct_id"] not in acct_ids:
                bad_verified.append(o["tran_id"])
        elif o["outcome"] == OUTCOME_CARD_MISSING:
            if card in xref_by_card or o["xref_found"] or o["acct_found"] or o["acct_id"] is not None:
                bad_card_missing.append(o["tran_id"])
        elif o["outcome"] == OUTCOME_ACCT_MISSING:
            x = xref_by_card.get(card)
            if x is None or not o["xref_found"] or o["acct_found"] or x["XREF-ACCT-ID"] != o["acct_id"] \
                    or o["acct_id"] in acct_ids:
                bad_acct_missing.append(o["tran_id"])
    r.check("CBTRN01C-XREF-01", "xref", "every VERIFIED card exists in cardxref and its XREF-ACCT-ID exists in acctdata",
            0, len(bad_verified), {"tran_ids": bad_verified[:20]})
    r.check("CBTRN01C-XREF-02", "xref", "every CARD_NOT_FOUND card does not exist in cardxref",
            0, len(bad_card_missing), {"tran_ids": bad_card_missing[:20]})
    r.check("CBTRN01C-XREF-03", "xref", "every ACCOUNT_NOT_FOUND card exists in cardxref but its account is not in acctdata",
            0, len(bad_acct_missing), {"tran_ids": bad_acct_missing[:20]})
    r.check("CBTRN01C-XREF-04", "xref", "sample data coverage: distinct cards used by the transactions",
            len({t["DALYTRAN-CARD-NUM"] for t in tran_in}), len({o["card_num"] for o in outcomes}),
            {"distinct_cards_in_xref": len(xref_by_card), "distinct_accounts": len(acct_ids)})
    return r.result()


# ----------------------------------------------------------------------
def count_display_lookups(display_path: str) -> int:
    with open(display_path, encoding="latin-1") as fh:
        return sum(1 for ln in fh if ln.startswith(("SUCCESSFUL READ OF XREF", "INVALID CARD NUMBER FOR XREF")))


def run(job: str, golden_dir: str, write: bool) -> Dict:
    g = lambda name: os.path.join(golden_dir, name)
    if job == "cbact01c":
        res = reconcile_cbact01c(_load(g("input-acctdata.json")), _load(g("outfile.json")),
                                 _load(g("arryfile.json")), _load(g("vbrcfile.json")))
    else:
        lookups = count_display_lookups(g("display.txt")) if os.path.exists(g("display.txt")) else None
        res = reconcile_cbtrn01c(_load(g("input-dailytran.json")), _load(g("outcomes.json")),
                                 _load(g("input-cardxref.json")), _load(g("input-acctdata.json")), lookups)
    if write:
        with open(g("reconciliation.json"), "w", encoding="utf-8") as fh:
            json.dump(res, fh, indent=2)
            fh.write("\n")
    return res


def print_report(res: Dict) -> None:
    print("== %s reconciliation: %s (%d/%d checks passed)" % (
        res["job"], res["summary"]["status"], res["summary"]["passed"], res["summary"]["checks"]))
    for c in res["checks"]:
        print("  [%s] %-36s %s" % (c["status"], c["id"], c["description"]))
        if c["status"] == "FAIL":
            print("         expected: %s" % json.dumps(c["expected"]))
            print("         actual:   %s" % json.dumps(c["actual"]))
        elif c["group"] == "totals" or c["group"] == "counts":
            print("         value: %s" % json.dumps(c["actual"]))


def main(argv: List[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("job", choices=["cbact01c", "cbtrn01c"])
    ap.add_argument("--golden-dir", required=True)
    ap.add_argument("--write", action="store_true", help="write reconciliation.json into --golden-dir")
    a = ap.parse_args(argv[1:])
    res = run(a.job, a.golden_dir, a.write)
    print_report(res)
    return 0 if res["summary"]["status"] == "PASS" else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))

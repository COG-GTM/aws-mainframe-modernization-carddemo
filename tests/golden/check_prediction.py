#!/usr/bin/env python3
"""Compare the generator's predicted outcome per record (manifest.json) with
what CBTRN02C actually did (expected/TRANSACT, expected/DALYREJS,
expected/ACCTFILE, expected/TCATBALF).

The program wins.  Every disagreement is printed and written to a JSON file so
it can be logged in docs/validation/golden-set/findings.md.  Exit 0 always
unless --strict is given (then exit 1 on any disagreement).

Standard library only.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from decimal import Decimal

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from layouts import ACCT, DALYREJS, TCATBAL, TRANSACT, decode_zoned, tcatbal_key  # noqa: E402


def load(path: str, layout):
    with open(path, "rb") as fh:
        return layout.records(fh.read())


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("manifest")
    ap.add_argument("expected_dir")
    ap.add_argument("--out", help="write prediction-check.json here")
    ap.add_argument("--strict", action="store_true")
    args = ap.parse_args(argv)

    with open(args.manifest) as fh:
        m = json.load(fh)
    ex = args.expected_dir
    posted = {}
    for r in load(os.path.join(ex, "TRANSACT"), TRANSACT):
        posted[TRANSACT.slice(r, "TRAN-ID").decode("ascii")] = r
    rejected = {}
    for r in load(os.path.join(ex, "DALYREJS"), DALYREJS):
        rejected[DALYREJS.slice(r, "DALYTRAN-ID").decode("ascii")] = r
    accts = {}
    for r in load(os.path.join(ex, "ACCTFILE"), ACCT):
        accts[ACCT.slice(r, "ACCT-ID").decode("ascii")] = r
    tcat = {}
    for r in load(os.path.join(ex, "TCATBALF"), TCATBAL):
        tcat[tcatbal_key(r).decode("ascii")] = r

    disagreements = []
    checked = 0
    for p in m["records"]:
        tid = p["tran_id"]
        checked += 1
        if tid in posted and tid in rejected:
            actual, reason = "BOTH", None
        elif tid in posted:
            actual, reason = "ACCEPT", 0
        elif tid in rejected:
            actual = "REJECT"
            reason = int(decode_zoned(DALYREJS.slice(rejected[tid], "WS-VALIDATION-FAIL-REASON"),
                                      DALYREJS.field("WS-VALIDATION-FAIL-REASON")))
        else:
            actual, reason = "MISSING", None
        if actual != p["predicted_outcome"] or reason != p["predicted_reason"]:
            disagreements.append({
                "kind": "outcome", "tran_id": tid, "case": p["case"],
                "predicted": "%s/%s" % (p["predicted_outcome"], p["predicted_reason"]),
                "actual": "%s/%s" % (actual, reason)})

    reason_f = DALYREJS.field("WS-VALIDATION-FAIL-REASON")
    actual_by_reason = {}
    for r in rejected.values():
        k = "%04d" % int(decode_zoned(DALYREJS.slice(r, "WS-VALIDATION-FAIL-REASON"), reason_f))
        actual_by_reason[k] = actual_by_reason.get(k, 0) + 1

    for acct_id, pred in m.get("predicted_closing_accounts", {}).items():
        checked += 1
        rec = accts.get(acct_id)
        if rec is None:
            disagreements.append({"kind": "closing_account", "acct_id": acct_id,
                                  "predicted": pred, "actual": "MISSING"})
            continue
        for fname, pv in pred.items():
            av = str(decode_zoned(ACCT.slice(rec, fname), ACCT.field(fname)))
            if Decimal(av) != Decimal(pv):
                disagreements.append({"kind": "closing_account", "acct_id": acct_id, "field": fname,
                                      "predicted": pv, "actual": av})
    bal_f = TCATBAL.field("TRAN-CAT-BAL")
    for key, pv in m.get("predicted_closing_tcatbal", {}).items():
        checked += 1
        acct_id, type_cd, cat_cd = key.split("|")
        rec = tcat.get(acct_id + type_cd + cat_cd)
        if rec is None:
            disagreements.append({"kind": "closing_tcatbal", "key": key, "predicted": pv, "actual": "MISSING"})
            continue
        av = str(decode_zoned(TCATBAL.slice(rec, "TRAN-CAT-BAL"), bal_f))
        if Decimal(av) != Decimal(pv):
            disagreements.append({"kind": "closing_tcatbal", "key": key, "field": "TRAN-CAT-BAL",
                                  "predicted": pv, "actual": av})
    if len(tcat) != len(m.get("predicted_closing_tcatbal", {})):
        disagreements.append({"kind": "closing_tcatbal_count",
                              "predicted": len(m.get("predicted_closing_tcatbal", {})), "actual": len(tcat)})

    result = {
        "set_name": m.get("set_name"),
        "records_in": len(m["records"]),
        "actual": {"accepted": len(posted), "rejected": len(rejected), "rejected_by_reason": actual_by_reason},
        "predicted": m.get("predicted_summary"),
        "items_checked": checked,
        "disagreements": disagreements,
        "note": "generator prediction vs. program outcome; the program wins",
    }
    if args.out:
        with open(args.out, "w") as fh:
            json.dump(result, fh, indent=2)
            fh.write("\n")
    print("== prediction check %s: %d records, actual accepted=%d rejected=%d by_reason=%s; %d disagreement(s)" % (
        m.get("set_name"), len(m["records"]), len(posted), len(rejected),
        json.dumps(actual_by_reason, sort_keys=True), len(disagreements)))
    for d in disagreements:
        print("   DISAGREEMENT %s" % json.dumps(d, sort_keys=True))
    return 1 if (args.strict and disagreements) else 0


if __name__ == "__main__":
    sys.exit(main())

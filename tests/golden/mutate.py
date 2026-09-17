#!/usr/bin/env python3
"""Produce candidate directories that each carry exactly ONE injected defect,
so selftest.sh can prove compare.py detects it.

  python3 tests/golden/mutate.py <expected_dir> <out_root> [--only NAME]
  python3 tests/golden/mutate.py --list

Each mutant directory is a byte copy of <expected_dir> with one change and a
mutant.json describing: the change, the exit code compare.py must return and
the strings (field names / totals / keys) that must appear in reconciliation.md.

Standard library only.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import sys
from decimal import Decimal
from typing import Callable, Dict, List, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from layouts import ACCT, DALYREJS, TCATBAL, TRANSACT, decode_zoned, encode_zoned, tcatbal_key  # noqa: E402

Mutation = Callable[[str], dict]   # takes mutant dir, edits files in place, returns details
MUTANTS: Dict[str, Tuple[str, int, Mutation]] = {}


def mutant(name: str, doc: str, expected_exit: int):
    def deco(fn):
        MUTANTS[name] = (doc, expected_exit, fn)
        return fn
    return deco


def _load(d: str, name: str, layout) -> List[bytes]:
    with open(os.path.join(d, name), "rb") as fh:
        return layout.records(fh.read())


def _save(d: str, name: str, recs: List[bytes]) -> None:
    with open(os.path.join(d, name), "wb") as fh:
        fh.write(b"".join(recs))


def _patch(rec: bytes, layout, field: str, raw: bytes) -> bytes:
    f = layout.field(field)
    assert len(raw) == f.length
    return rec[:f.offset] + raw + rec[f.end:]


def _first_nonzero(recs: List[bytes], layout, field: str) -> int:
    f = layout.field(field)
    for i, r in enumerate(recs):
        if decode_zoned(r[f.offset:f.end], f) != 0:
            return i
    raise SystemExit("no record with non-zero %s to mutate" % field)


@mutant("amount_off_by_one_cent", "one accepted amount (TRANSACT TRAN-AMT) increased by 0.01", 1)
def m_amount(d: str) -> dict:
    recs = _load(d, "TRANSACT", TRANSACT)
    i = _first_nonzero(recs, TRANSACT, "TRAN-AMT")
    f = TRANSACT.field("TRAN-AMT")
    v = decode_zoned(recs[i][f.offset:f.end], f)
    recs[i] = _patch(recs[i], TRANSACT, "TRAN-AMT", encode_zoned(v + Decimal("0.01"), f))
    _save(d, "TRANSACT", recs)
    key = TRANSACT.slice(recs[i], "TRAN-ID").decode()
    return {"file": "TRANSACT", "record": i + 1, "key": key, "field": "TRAN-AMT",
            "from": str(v), "to": str(v + Decimal("0.01")),
            "expect_in_report": ["TRAN-AMT", key, "sum_accepted_amount"]}


@mutant("reject_moved_to_accepted", "one DALYREJS record removed and its 350-byte transaction written to TRANSACT", 1)
def m_reject_to_accept(d: str) -> dict:
    rej = _load(d, "DALYREJS", DALYREJS)
    tr = _load(d, "TRANSACT", TRANSACT)
    moved = rej.pop(0)
    body = moved[:TRANSACT.length]
    if tr:
        body = _patch(body, TRANSACT, "TRAN-PROC-TS", TRANSACT.slice(tr[0], "TRAN-PROC-TS"))
    tr.append(body)
    _save(d, "DALYREJS", rej)
    _save(d, "TRANSACT", tr)
    key = TRANSACT.slice(body, "TRAN-ID").decode()
    return {"file": "DALYREJS->TRANSACT", "key": key,
            "expect_in_report": [key, "accepted", "rejected", "rejected_by_reason"]}


@mutant("sign_flipped_on_balance", "zoned sign overpunch flipped on one ACCTFILE ACCT-CURR-BAL", 1)
def m_sign(d: str) -> dict:
    recs = _load(d, "ACCTFILE", ACCT)
    i = _first_nonzero(recs, ACCT, "ACCT-CURR-BAL")
    f = ACCT.field("ACCT-CURR-BAL")
    v = decode_zoned(recs[i][f.offset:f.end], f)
    recs[i] = _patch(recs[i], ACCT, "ACCT-CURR-BAL", encode_zoned(-v, f))
    _save(d, "ACCTFILE", recs)
    key = ACCT.slice(recs[i], "ACCT-ID").decode()
    return {"file": "ACCTFILE", "record": i + 1, "key": key, "field": "ACCT-CURR-BAL",
            "from": str(v), "to": str(-v),
            "expect_in_report": ["ACCT-CURR-BAL", key, "sign", "closing_account_balances"]}


@mutant("record_dropped", "one TRANSACT record deleted", 1)
def m_drop(d: str) -> dict:
    recs = _load(d, "TRANSACT", TRANSACT)
    i = len(recs) // 2
    key = TRANSACT.slice(recs[i], "TRAN-ID").decode()
    del recs[i]
    _save(d, "TRANSACT", recs)
    return {"file": "TRANSACT", "record": i + 1, "key": key,
            "expect_in_report": ["missing in candidate", key, "accepted"]}


@mutant("two_records_swapped", "two adjacent TRANSACT records exchanged (same content, different order)", 2)
def m_swap(d: str) -> dict:
    recs = _load(d, "TRANSACT", TRANSACT)
    if len(recs) < 2:
        raise SystemExit("need at least two TRANSACT records")
    recs[0], recs[1] = recs[1], recs[0]
    _save(d, "TRANSACT", recs)
    return {"file": "TRANSACT", "records": [1, 2],
            "expect_in_report": ["TRANSACT", "DIFFERENT ORDER"]}


@mutant("trailing_space_in_text", "one TRANSACT TRAN-MERCHANT-NAME padded with a trailing LOW-VALUE (x'00') instead of a space", 1)
def m_trailing(d: str) -> dict:
    recs = _load(d, "TRANSACT", TRANSACT)
    f = TRANSACT.field("TRAN-MERCHANT-NAME")
    i = 0
    raw = bytearray(recs[i][f.offset:f.end])
    if raw[-1:] != b" ":
        raise SystemExit("TRAN-MERCHANT-NAME has no trailing space to mutate")
    raw[-1] = 0
    recs[i] = _patch(recs[i], TRANSACT, "TRAN-MERCHANT-NAME", bytes(raw))
    _save(d, "TRANSACT", recs)
    key = TRANSACT.slice(recs[i], "TRAN-ID").decode()
    return {"file": "TRANSACT", "record": 1, "key": key, "field": "TRAN-MERCHANT-NAME",
            "expect_in_report": ["TRAN-MERCHANT-NAME", key]}


@mutant("reason_code_changed", "one DALYREJS WS-VALIDATION-FAIL-REASON changed to another code", 1)
def m_reason(d: str) -> dict:
    recs = _load(d, "DALYREJS", DALYREJS)
    f = DALYREJS.field("WS-VALIDATION-FAIL-REASON")
    i = 0
    v = int(decode_zoned(recs[i][f.offset:f.end], f))
    nv = 101 if v != 101 else 102
    recs[i] = _patch(recs[i], DALYREJS, "WS-VALIDATION-FAIL-REASON", encode_zoned(nv, f))
    _save(d, "DALYREJS", recs)
    key = DALYREJS.slice(recs[i], "DALYTRAN-ID").decode()
    return {"file": "DALYREJS", "record": 1, "key": key, "field": "WS-VALIDATION-FAIL-REASON",
            "from": v, "to": nv,
            "expect_in_report": ["WS-VALIDATION-FAIL-REASON", key, "rejected_by_reason"]}


@mutant("category_row_dropped", "one TCATBALF record deleted (category balance never created/updated)", 1)
def m_tcat_drop(d: str) -> dict:
    recs = _load(d, "TCATBALF", TCATBAL)
    i = len(recs) - 1
    key = tcatbal_key(recs[i]).decode()
    del recs[i]
    _save(d, "TCATBALF", recs)
    return {"file": "TCATBALF", "record": i + 1, "key": key,
            "expect_in_report": ["missing in candidate", key, "closing_category_balances"]}


@mutant("return_code_changed", "RETURN-CODE 4 (rejects present, CBTRN02C.cbl:229) replaced by 0", 1)
def m_rc(d: str) -> dict:
    p = os.path.join(d, "RETURN-CODE")
    old = open(p).read().strip()
    new = "0" if old != "0" else "4"
    with open(p, "w") as fh:
        fh.write(new + "\n")
    return {"file": "RETURN-CODE", "from": old, "to": new,
            "expect_in_report": ["RETURN-CODE", "MISMATCH"]}


def build(expected_dir: str, out_root: str, only: List[str]) -> List[dict]:
    out = []
    for name, (doc, exit_code, fn) in MUTANTS.items():
        if only and name not in only:
            continue
        d = os.path.join(out_root, name)
        if os.path.isdir(d):
            shutil.rmtree(d)
        shutil.copytree(expected_dir, d)
        for stale in ("reconciliation.json", "reconciliation.md", "mutant.json"):
            if os.path.exists(os.path.join(d, stale)):
                os.remove(os.path.join(d, stale))
        details = fn(d)
        meta = {"mutant": name, "description": doc, "expected_exit": exit_code,
                "source_expected_dir": os.path.abspath(expected_dir)}
        meta.update(details)
        with open(os.path.join(d, "mutant.json"), "w") as fh:
            json.dump(meta, fh, indent=2)
            fh.write("\n")
        out.append(meta)
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("expected_dir", nargs="?")
    ap.add_argument("out_root", nargs="?")
    ap.add_argument("--only", action="append", default=[])
    ap.add_argument("--list", action="store_true")
    args = ap.parse_args(argv)
    if args.list:
        for name, (doc, ec, _) in MUTANTS.items():
            print("%-28s exit %d  %s" % (name, ec, doc))
        return 0
    if not args.expected_dir or not args.out_root:
        ap.error("expected_dir and out_root are required")
    for m in build(args.expected_dir, args.out_root, args.only):
        print("mutant %-28s -> %s/%s  (expects exit %d)" % (
            m["mutant"], args.out_root, m["mutant"], m["expected_exit"]))
    return 0


if __name__ == "__main__":
    sys.exit(main())

"""Field-by-field comparison of two JSON record sets.

Both inputs are JSON arrays of records as produced by ``records.py``
(nested OCCURS arrays allowed).  Records are matched by a key field
(default: file order, i.e. the record index) and every difference is
reported as ``{record_key, field, expected, actual}``.

Rules:
* strings are compared exactly (trailing spaces matter);
* numeric decimal strings are compared exactly too: "1940.00" != "1940.0"
  because the scale is part of the contract.  Pass --numeric-value to
  compare by Decimal value instead (scale-insensitive, for triage only);
* a record missing on either side is one mismatch with field "<record>";
* extra / missing fields are reported per field.

Command line::

    python3 compare.py expected.json actual.json [--key ACCT-ID] [--numeric-value]

Exit status 0 when identical, 1 when mismatches were found.
"""
from __future__ import annotations

import argparse
import json
import sys
from decimal import Decimal, InvalidOperation
from typing import Any, Dict, List, Optional


def flatten(obj: Any, prefix: str = "") -> Dict[str, Any]:
    """{"A": {"B": 1}, "C": [{"D": 2}]} -> {"A.B": 1, "C[1].D": 2}"""
    out: Dict[str, Any] = {}
    if isinstance(obj, (dict, list)) and not obj and prefix:
        out[prefix] = obj          # keep empty structures visible ({} != [] != missing)
    elif isinstance(obj, dict):
        for k, v in obj.items():
            out.update(flatten(v, "%s.%s" % (prefix, k) if prefix else k))
    elif isinstance(obj, list):
        for i, v in enumerate(obj, 1):
            out.update(flatten(v, "%s[%d]" % (prefix, i)))
    else:
        out[prefix] = obj
    return out


def _values_equal(e: Any, a: Any, numeric_value: bool) -> bool:
    if e == a:
        return True
    if numeric_value and isinstance(e, str) and isinstance(a, str):
        try:
            return Decimal(e) == Decimal(a)
        except InvalidOperation:
            return False
    return False


def compare_records(expected: List[Dict], actual: List[Dict], key: Optional[str] = None,
                    numeric_value: bool = False) -> List[Dict]:
    """Return the list of mismatches (empty when the sets are identical)."""
    mismatches: List[Dict] = []

    def rec_key(rec: Dict, idx: int) -> str:
        if key is None:
            return "#%d" % (idx + 1)
        if key not in rec:
            return "#%d(no %s)" % (idx + 1, key)
        return str(rec[key])

    exp_map = {rec_key(r, i): r for i, r in enumerate(expected)}
    act_map = {rec_key(r, i): r for i, r in enumerate(actual)}
    if key is not None:
        if len(exp_map) != len(expected):
            mismatches.append({"record_key": "*", "field": "<duplicate keys>",
                               "expected": "unique %s in expected" % key, "actual": "duplicates present"})
        if len(act_map) != len(actual):
            mismatches.append({"record_key": "*", "field": "<duplicate keys>",
                               "expected": "unique %s in actual" % key, "actual": "duplicates present"})
    if len(expected) != len(actual):
        mismatches.append({"record_key": "*", "field": "<record count>",
                           "expected": len(expected), "actual": len(actual)})
    for k in exp_map:
        if k not in act_map:
            mismatches.append({"record_key": k, "field": "<record>", "expected": "present", "actual": "missing"})
    for k in act_map:
        if k not in exp_map:
            mismatches.append({"record_key": k, "field": "<record>", "expected": "missing", "actual": "present"})
    for k, erec in exp_map.items():
        arec = act_map.get(k)
        if arec is None:
            continue
        ef, af = flatten(erec), flatten(arec)
        for fld in ef:
            if fld not in af:
                mismatches.append({"record_key": k, "field": fld, "expected": ef[fld], "actual": "<missing field>"})
            elif not _values_equal(ef[fld], af[fld], numeric_value):
                mismatches.append({"record_key": k, "field": fld, "expected": ef[fld], "actual": af[fld]})
        for fld in af:
            if fld not in ef:
                mismatches.append({"record_key": k, "field": fld, "expected": "<missing field>", "actual": af[fld]})
    if key is not None:
        exp_order = [rec_key(r, i) for i, r in enumerate(expected)]
        act_order = [rec_key(r, i) for i, r in enumerate(actual)]
        if sorted(exp_order) == sorted(act_order) and exp_order != act_order:
            mismatches.append({"record_key": "*", "field": "<record order>",
                               "expected": exp_order, "actual": act_order})
    return mismatches


def compare_files(expected_path: str, actual_path: str, key: Optional[str] = None,
                  numeric_value: bool = False) -> List[Dict]:
    with open(expected_path, encoding="utf-8") as fh:
        expected = json.load(fh)
    with open(actual_path, encoding="utf-8") as fh:
        actual = json.load(fh)
    return compare_records(expected, actual, key, numeric_value)


def main(argv: List[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("expected")
    ap.add_argument("actual")
    ap.add_argument("--key", help="field used to match records (default: file order)")
    ap.add_argument("--numeric-value", action="store_true", help="compare numerics by value, not text")
    ap.add_argument("--json", action="store_true", help="print mismatches as JSON")
    a = ap.parse_args(argv[1:])
    mm = compare_files(a.expected, a.actual, a.key, a.numeric_value)
    if a.json:
        print(json.dumps(mm, indent=2))
    else:
        for m in mm:
            print("%-20s %-45s expected=%r actual=%r" % (m["record_key"], m["field"], m["expected"], m["actual"]))
        print("%d mismatch(es); %s vs %s" % (len(mm), a.expected, a.actual))
    return 1 if mm else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

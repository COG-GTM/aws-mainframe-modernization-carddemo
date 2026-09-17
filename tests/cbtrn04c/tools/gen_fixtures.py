#!/usr/bin/env python3
"""Fixture generator for the CBTRN04C test suite.

Writes, for every named case, a DALYTRAN input file made of exact
350-byte records laid out per app/cpy/CVTRA06Y.cpy, the PARM value the
case runs with, and (for the shared "standard" reference set) the
TRANTYPE / TRANCATG / XREFFILE / TCATBALF reference records laid out per
CVTRA03Y / CVTRA04Y / CVACT03Y / CVTRA01Y.

Signed zoned amounts use the mainframe overpunch convention as it
appears in the repository's own ASCII sample data (app/data/ASCII):
last digit 0-9 positive -> { A B C D E F G H I, negative -> } J K L M N
O P Q R. GnuCOBOL reads these with -fsign=EBCDIC.

Usage: gen_fixtures.py <output-dir>
  <output-dir>/cases/<case>/dalytran.dat
  <output-dir>/cases/<case>/parm            (absent when no PARM is passed)
  <output-dir>/cases/INDEX.md               (case list, derived from this file)
  <output-dir>/refdata/standard/*.txt
"""
import os
import sys
from dataclasses import dataclass, field
from typing import List, Optional

# ---------------------------------------------------------------------------
# CVTRA06Y.cpy DALYTRAN-RECORD, 350 bytes
# ---------------------------------------------------------------------------
LAYOUT = [
    ("id", 16), ("type", 2), ("cat", 4), ("source", 10), ("desc", 100),
    ("amt", 11), ("merchant_id", 9), ("merchant_name", 50),
    ("merchant_city", 50), ("merchant_zip", 10), ("card", 16),
    ("orig_ts", 26), ("proc_ts", 26), ("filler", 20),
]
LRECL = sum(n for _, n in LAYOUT)
assert LRECL == 350

POS_ZONE = "{ABCDEFGHI"
NEG_ZONE = "}JKLMNOPQR"


def zoned(amount_cents: int, digits: int = 11) -> bytes:
    """PIC S9(09)V99 as signed zoned decimal with an overpunched sign."""
    mag = abs(amount_cents)
    text = str(mag).rjust(digits, "0")
    if len(text) != digits:
        raise ValueError(f"amount {amount_cents} does not fit S9(09)V99")
    zone = NEG_ZONE if amount_cents < 0 else POS_ZONE
    return (text[:-1] + zone[int(text[-1])]).encode("ascii")


def cents(text: str) -> int:
    sign = -1 if text.startswith("-") else 1
    whole, _, frac = text.lstrip("-").partition(".")
    return sign * (int(whole) * 100 + int(frac.ljust(2, "0")[:2]))


def fit(value, width: int, name: str) -> bytes:
    raw = value if isinstance(value, bytes) else str(value).encode("ascii")
    if len(raw) > width:
        raise ValueError(f"{name}: {raw!r} longer than {width}")
    return raw.ljust(width)


BASE = dict(
    id="TX00000000000001", type="01", cat="0001", source="POS TERM",
    desc="GROCERY PURCHASE", amt="125.50", merchant_id="123456789",
    merchant_name="CORNER MARKET", merchant_city="SPRINGFIELD",
    merchant_zip="12345", card="4000000000000001",
    orig_ts="2024-03-01 10:15:30.000000", proc_ts="2024-03-02 02:00:00.000000",
    filler="",
)


def record(**overrides) -> bytes:
    fields = dict(BASE)
    fields.update(overrides)
    out = b""
    for name, width in LAYOUT:
        value = fields[name]
        if name == "amt" and isinstance(value, str):
            value = zoned(cents(value))
        out += fit(value, width, name)
    assert len(out) == LRECL, len(out)
    return out


# ---------------------------------------------------------------------------
# Standard reference set
# ---------------------------------------------------------------------------
def trantype(code: str, desc: str) -> bytes:          # CVTRA03Y, 60 bytes
    return fit(code, 2, "type") + fit(desc, 50, "desc") + b" " * 8


def trancatg(code: str, cat: str, desc: str) -> bytes:  # CVTRA04Y, 60 bytes
    return fit(code, 2, "type") + fit(cat, 4, "cat") + fit(desc, 50, "desc") \
        + b" " * 4


def cardxref(card: str, cust: str, acct: str) -> bytes:  # CVACT03Y, 50 bytes
    return fit(card, 16, "card") + fit(cust, 9, "cust") + fit(acct, 11, "acct") \
        + b" " * 14


def tcatbal(acct: str, code: str, cat: str, bal: str) -> bytes:  # CVTRA01Y, 50
    return fit(acct, 11, "acct") + fit(code, 2, "type") + fit(cat, 4, "cat") \
        + zoned(cents(bal)) + b" " * 22


CARD_BAL_ZERO = "4000000000000001"      # account 1: category balance 0.00
CARD_BAL_ONE_CENT = "4000000000000002"  # account 2: category balance 0.01
CARD_BAL_MINUS_CENT = "4000000000000003"  # account 3: category balance -0.01
CARD_UNKNOWN = "4999999999999999"

STANDARD_REFSET = {
    "trantype.txt": [
        trantype("01", "Purchase"),
        trantype("02", "Payment"),
        trantype("03", "Credit"),
    ],
    "trancatg.txt": [
        trancatg("01", "0001", "Regular Sales Draft"),
        trancatg("01", "0002", "Regular Cash Advance"),
        trancatg("02", "0001", "Payment received"),
        trancatg("03", "0001", "Merchandise return credit"),
    ],
    "cardxref.txt": [
        cardxref(CARD_BAL_ZERO, "000000001", "00000000001"),
        cardxref(CARD_BAL_ONE_CENT, "000000002", "00000000002"),
        cardxref(CARD_BAL_MINUS_CENT, "000000003", "00000000003"),
    ],
    "tcatbal.txt": [
        tcatbal("00000000001", "01", "0001", "0.00"),
        tcatbal("00000000002", "01", "0001", "0.01"),
        tcatbal("00000000003", "01", "0001", "-0.01"),
    ],
}


# ---------------------------------------------------------------------------
# Cases
# ---------------------------------------------------------------------------
RUN_DATE = "20240315"


@dataclass
class Case:
    name: str
    rule: str
    purpose: str
    expect: str
    records: List[bytes] = field(default_factory=list)
    parm: Optional[str] = RUN_DATE
    input_present: bool = True      # False: no dalytran.dat (file error)


CASES: List[Case] = [
    Case("clean_record", "all", "One well-formed record passes every rule",
         "accepted, RC 0", [record()]),
    Case("rule01_id_spaces", "R01", "DALYTRAN-ID all spaces",
         "reject 0201", [record(id=" " * 16)]),
    Case("rule01_id_low_values", "R01", "DALYTRAN-ID all LOW-VALUES",
         "reject 0201", [record(id=b"\x00" * 16)]),
    Case("rule02_type_unknown", "R02", "DALYTRAN-TYPE-CD 99 not in TRANTYPE",
         "reject 0202", [record(type="99")]),
    Case("rule03_category_unknown", "R03",
         "type 01 exists but 01/9999 not in TRANCATG",
         "reject 0203", [record(cat="9999")]),
    Case("rule04_amount_alpha", "R04", "DALYTRAN-AMT is letters",
         "reject 0204", [record(amt=b"ABCDEFGHIJK")]),
    Case("rule04_amount_invalid_sign", "R04",
         "DALYTRAN-AMT digits with '-' in the sign position",
         "reject 0204", [record(amt=b"0000012550-")]),
    Case("rule04_amount_spaces", "R04", "DALYTRAN-AMT all spaces",
         "reject 0204", [record(amt=b" " * 11)]),
    Case("rule05_card_unknown", "R05", "DALYTRAN-CARD-NUM not in XREFFILE",
         "reject 0100 (CBTRN02C code)", [record(card=CARD_UNKNOWN)]),
    Case("rule06_amount_max_downstream", "R06",
         "amount 999999999.99 onto category balance 0.00 = S9(09)V99 max",
         "accepted, RC 0", [record(amt="999999999.99", card=CARD_BAL_ZERO)]),
    Case("rule06_amount_one_cent_over", "R06",
         "amount 999999999.99 onto category balance 0.01 = one cent over",
         "reject 0205",
         [record(amt="999999999.99", card=CARD_BAL_ONE_CENT)]),
    Case("rule06_amount_negative_max", "R06",
         "amount -999999999.99 onto category balance 0.00 = S9(09)V99 min",
         "accepted, RC 0",
         [record(amt="-999999999.99", card=CARD_BAL_ZERO)]),
    Case("rule06_amount_negative_floor", "R06",
         "amount -999999999.99 onto category balance -0.01 = one cent under",
         "reject 0205",
         [record(amt="-999999999.99", card=CARD_BAL_MINUS_CENT)]),
    Case("rule06_batch_second_record_overflows", "R06",
         "two 600000000.00 records for one account/type/category on "
         "balance 0.00: 600000000.00 fits, 1200000000.00 does not",
         "first accepted, second reject 0205",
         [record(id="TX00000000000101", amt="600000000.00",
                 card=CARD_BAL_ZERO),
          record(id="TX00000000000102", amt="600000000.00",
                 card=CARD_BAL_ZERO)]),
    Case("rule06_batch_reject_not_projected", "R06",
         "600000000.00 accepted, 300000000.00 rejected on date (0206) so "
         "it must not advance the projection, then 300000000.00 accepted "
         "at 900000000.00; a fourth 100000000.00 overflows",
         "accepted, reject 0206, accepted, reject 0205",
         [record(id="TX00000000000201", amt="600000000.00",
                 card=CARD_BAL_ZERO),
          record(id="TX00000000000202", amt="300000000.00",
                 card=CARD_BAL_ZERO,
                 orig_ts="2024-02-30 10:15:30.000000"),
          record(id="TX00000000000203", amt="300000000.00",
                 card=CARD_BAL_ZERO),
          record(id="TX00000000000204", amt="100000000.00",
                 card=CARD_BAL_ZERO)]),
    Case("rule07_orig_feb30", "R07", "origination date 2024-02-30",
         "reject 0206", [record(orig_ts="2024-02-30 10:15:30.000000")]),
    Case("rule07_orig_leap_day_valid", "R07",
         "origination 2024-02-29, a real leap day",
         "accepted, Julian 2024060",
         [record(orig_ts="2024-02-29 10:15:30.000000")]),
    Case("rule07_orig_leap_day_nonleap", "R07",
         "origination 2023-02-29, 2023 is not a leap year",
         "reject 0206", [record(orig_ts="2023-02-29 10:15:30.000000")]),
    Case("rule07_orig_century_leap", "R07",
         "origination 2000-02-29, divisible by 400 so a leap year",
         "accepted, Julian 2000060",
         [record(orig_ts="2000-02-29 10:15:30.000000")]),
    Case("rule07_orig_century_nonleap", "R07",
         "origination 1900-02-29, century not divisible by 400",
         "reject 0206", [record(orig_ts="1900-02-29 10:15:30.000000")]),
    Case("rule07_orig_month_13", "R07", "origination month 13",
         "reject 0206", [record(orig_ts="2024-13-01 10:15:30.000000")]),
    Case("rule07_orig_not_numeric", "R07", "origination date is letters",
         "reject 0206", [record(orig_ts="ABCD-EF-GH 10:15:30.000000")]),
    Case("rule08_proc_apr31", "R08", "processing date 2024-04-31",
         "reject 0207", [record(proc_ts="2024-04-31 02:00:00.000000")]),
    Case("rule08_proc_blank_accepted", "R08",
         "processing timestamp blank, as in the sample feed",
         "accepted, RC 0", [record(proc_ts="")]),
    Case("rule09_orig_after_proc", "R09",
         "origination 2024-03-02 after processing 2024-03-01",
         "reject 0208",
         [record(orig_ts="2024-03-02 10:15:30.000000",
                 proc_ts="2024-03-01 02:00:00.000000")]),
    Case("rule10_orig_future", "R10",
         "origination 2024-03-16 after run date 2024-03-15",
         "reject 0209",
         [record(orig_ts="2024-03-16 10:15:30.000000",
                 proc_ts="2024-03-16 12:00:00.000000")]),
    Case("rule10_proc_future", "R10",
         "processing 2024-03-16 after run date 2024-03-15",
         "reject 0209",
         [record(orig_ts="2024-03-14 10:15:30.000000",
                 proc_ts="2024-03-16 12:00:00.000000")]),
    Case("rule10_dates_equal_run_date", "R10",
         "origination and processing both on the run date",
         "accepted, RC 0",
         [record(orig_ts="2024-03-15 10:15:30.000000",
                 proc_ts="2024-03-15 12:00:00.000000")]),
    Case("precedence_type_and_card", "R11",
         "type 99 and unknown card on one record",
         "reject 0202 only", [record(type="99", card=CARD_UNKNOWN)]),
    Case("precedence_id_and_amount", "R11",
         "blank id and non-numeric amount on one record",
         "reject 0201 only", [record(id=" " * 16, amt=b"ABCDEFGHIJK")]),
    Case("precedence_amount_and_date", "R11",
         "non-numeric amount and 30 February on one record",
         "reject 0204 only",
         [record(amt=b"ABCDEFGHIJK", orig_ts="2024-02-30 10:15:30.000000")]),
    Case("empty_input", "totals", "DALYTRAN with no records",
         "RC 0, zero totals, Julian range NONE", []),
    Case("all_rejects", "totals", "three records, every one rejected",
         "RC 4, DALYVALD empty",
         [record(id=" " * 16),
          record(id="TX00000000000002", type="99"),
          record(id="TX00000000000003", card=CARD_UNKNOWN)]),
    Case("mixed_feed", "totals",
         "four accepted and two rejected records, mixed signs and dates",
         "RC 4, read 6 = 4 + 2, amounts reconcile, Julian 2024005-2024070",
         [record(id="TX00000000000101", amt="125.50",
                 orig_ts="2024-01-05 08:00:00.000000",
                 proc_ts="2024-01-06 02:00:00.000000"),
          record(id="TX00000000000102", type="02", amt="-20.00",
                 orig_ts="2024-02-10 08:00:00.000000",
                 proc_ts="2024-02-11 02:00:00.000000"),
          record(id="TX00000000000103", amt="50.00", card=CARD_UNKNOWN),
          record(id="TX00000000000104", amt="999.99",
                 orig_ts="2024-03-10 08:00:00.000000",
                 proc_ts="2024-03-11 02:00:00.000000"),
          record(id="TX00000000000105", amt="-10.00",
                 orig_ts="2024-03-02 10:15:30.000000",
                 proc_ts="2024-03-01 02:00:00.000000"),
          record(id="TX00000000000106", amt="0.00",
                 orig_ts="2024-02-29 08:00:00.000000",
                 proc_ts="2024-03-01 02:00:00.000000")]),
    Case("parm_missing", "parm", "no PARM passed",
         "RC 8, no files opened", [record()], parm=None),
    Case("parm_invalid_date", "parm", "PARM 20241301 is not a date",
         "RC 8, no files opened", [record()], parm="20241301"),
    Case("parm_wrong_length", "parm", "PARM 2024031 is seven characters",
         "RC 8, no files opened", [record()], parm="2024031"),
    Case("file_error_missing_input", "file", "DALYTRAN data set absent "
         "(OPEN status 35)", "RC 12, no output written", [],
         input_present=False),
]

SAMPLE_CASE_NOTE = (
    "sample_data", "sample",
    "app/data/ASCII/dailytran.txt (300 records) against the sample "
    "reference files, built by run_tests.sh at run time",
    "RC 0, 300 accepted")


def write(path: str, data: bytes) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as fh:
        fh.write(data)


def index_markdown() -> str:
    lines = [
        "# CBTRN04C test cases",
        "",
        "Generated by tools/gen_fixtures.py; do not edit by hand.",
        "",
        f"Cases: {len(CASES) + 1} ({len(CASES)} generated fixtures plus "
        "sample_data built from app/data/ASCII at run time).",
        "",
        "| # | Case | Rule | Input | Expected |",
        "|---|------|------|-------|----------|",
    ]
    rows = [(c.name, c.rule, c.purpose, c.expect) for c in CASES]
    rows.append(SAMPLE_CASE_NOTE)
    for n, (name, rule, purpose, expect) in enumerate(rows, 1):
        lines.append(f"| {n} | `{name}` | {rule} | {purpose} | {expect} |")
    return "\n".join(lines) + "\n"


def main(argv) -> int:
    if len(argv) != 2:
        sys.stderr.write(__doc__)
        return 2
    out = argv[1]
    names = [c.name for c in CASES]
    assert len(names) == len(set(names)), "duplicate case name"
    for case in CASES:
        for rec in case.records:
            assert len(rec) == LRECL
        if case.input_present:
            write(os.path.join(out, "cases", case.name, "dalytran.dat"),
                  b"".join(case.records))
        else:
            os.makedirs(os.path.join(out, "cases", case.name), exist_ok=True)
        if case.parm is not None:
            write(os.path.join(out, "cases", case.name, "parm"),
                  case.parm.encode("ascii") + b"\n")
    write(os.path.join(out, "cases", "INDEX.md"),
          index_markdown().encode("ascii"))
    for fname, recs in STANDARD_REFSET.items():
        write(os.path.join(out, "refdata", "standard", fname),
              b"".join(r + b"\n" for r in recs))
    print(f"gen_fixtures: {len(CASES)} cases written under {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

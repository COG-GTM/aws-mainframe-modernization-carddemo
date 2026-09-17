#!/usr/bin/env python3
"""Fixture generator for the CBTRN04C test suite.

Writes, for every named case, a DALYTRAN input file made of exact
350-byte records laid out per app/cpy/CVTRA06Y.cpy, the PARM value the
case runs with, and (for the shared "standard" reference set) the
TRANTYPE / TRANCATG / XREFFILE / ACCTFILE / TCATBALF reference records
laid out per CVTRA03Y / CVTRA04Y / CVACT03Y / CVACT01Y / CVTRA01Y.

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
    """Signed zoned decimal with an overpunched sign: 11 digits for
    PIC S9(09)V99 (CVTRA06Y, CVTRA01Y), 12 for S9(10)V99 (CVACT01Y)."""
    mag = abs(amount_cents)
    text = str(mag).rjust(digits, "0")
    if len(text) != digits:
        raise ValueError(f"amount {amount_cents} does not fit {digits} digits")
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


def acct(acct_id: str, credit_limit: str, expires: str, cyc_credit: str,
         cyc_debit: str) -> bytes:                       # CVACT01Y, 300 bytes
    return (fit(acct_id, 11, "acct") + b"Y" + zoned(0, 12)
            + zoned(cents(credit_limit), 12) + zoned(cents(credit_limit), 12)
            + fit("2020-01-01", 10, "open") + fit(expires, 10, "expires")
            + fit("2020-01-01", 10, "reissue") + zoned(cents(cyc_credit), 12)
            + zoned(cents(cyc_debit), 12) + fit("12345", 10, "zip")
            + fit("GRP01", 10, "group") + b" " * 178)


NO_LIMIT = "9999999999.99"              # S9(10)V99 maximum, never binding
FAR_FUTURE = "2099-12-31"

CARD_BAL_ZERO = "4000000000000001"      # account 1: category balance 0.00
CARD_BAL_ONE_CENT = "4000000000000002"  # account 2: category balance 0.01
CARD_BAL_MINUS_CENT = "4000000000000003"  # account 3: category balance -0.01
CARD_NO_ACCT = "4000000000000004"     # account 4: in XREFFILE, not ACCTFILE
CARD_LIMIT = "4000000000000005"       # account 5: limit 1000.00, cycle
                                      # credit 500.00, debit -100.00, so
                                      # CBTRN02C's CYC-CREDIT - CYC-DEBIT
                                      # starts at 600.00 and 400.00 more fits
CARD_EXPIRED = "4000000000000006"     # account 6: expired 2024-02-28,
                                      # credit limit 0.00
CARD_EXPIRED_NO_LIMIT = "4000000000000008"  # account 8: expired
                                      # 2024-02-28, limit never binding
CARD_EXP_CAT = "4000000000000007"     # account 7: limit 1000.00, expired
                                      # 2024-03-10, category balance
                                      # 999999000.00
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
        cardxref(CARD_NO_ACCT, "000000004", "00000000004"),
        cardxref(CARD_LIMIT, "000000005", "00000000005"),
        cardxref(CARD_EXPIRED, "000000006", "00000000006"),
        cardxref(CARD_EXP_CAT, "000000007", "00000000007"),
        cardxref(CARD_EXPIRED_NO_LIMIT, "000000008", "00000000008"),
    ],
    "acctdata.txt": [
        acct("00000000001", NO_LIMIT, FAR_FUTURE, "0.00", "0.00"),
        acct("00000000002", NO_LIMIT, FAR_FUTURE, "0.00", "0.00"),
        acct("00000000003", NO_LIMIT, FAR_FUTURE, "0.00", "0.00"),
        acct("00000000005", "1000.00", FAR_FUTURE, "500.00", "-100.00"),
        acct("00000000006", "0.00", "2024-02-28", "0.00", "0.00"),
        acct("00000000007", "1000.00", "2024-03-10", "0.00", "0.00"),
        acct("00000000008", NO_LIMIT, "2024-02-28", "0.00", "0.00"),
    ],
    "tcatbal.txt": [
        tcatbal("00000000001", "01", "0001", "0.00"),
        tcatbal("00000000002", "01", "0001", "0.01"),
        tcatbal("00000000003", "01", "0001", "-0.01"),
        tcatbal("00000000007", "01", "0001", "999999000.00"),
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
    Case("rule06_acct_missing", "R06",
         "card in XREFFILE but its account 00000000004 not in ACCTFILE",
         "reject 0101 (CBTRN02C code)", [record(card=CARD_NO_ACCT)]),
    Case("rule07_amount_max_downstream", "R07",
         "amount 999999999.99 onto category balance 0.00 = S9(09)V99 max",
         "accepted, RC 0", [record(amt="999999999.99", card=CARD_BAL_ZERO)]),
    Case("rule07_amount_one_cent_over", "R07",
         "amount 999999999.99 onto category balance 0.01 = one cent over",
         "reject 0205",
         [record(amt="999999999.99", card=CARD_BAL_ONE_CENT)]),
    Case("rule07_amount_negative_max", "R07",
         "amount -999999999.99 onto category balance 0.00 = S9(09)V99 min",
         "accepted, RC 0",
         [record(amt="-999999999.99", card=CARD_BAL_ZERO)]),
    Case("rule07_amount_negative_floor", "R07",
         "amount -999999999.99 onto category balance -0.01 = one cent under",
         "reject 0205",
         [record(amt="-999999999.99", card=CARD_BAL_MINUS_CENT)]),
    Case("rule07_batch_second_record_overflows", "R07",
         "two 600000000.00 records for one account/type/category on "
         "balance 0.00: 600000000.00 fits, 1200000000.00 does not",
         "first accepted, second reject 0205",
         [record(id="TX00000000000101", amt="600000000.00",
                 card=CARD_BAL_ZERO),
          record(id="TX00000000000102", amt="600000000.00",
                 card=CARD_BAL_ZERO)]),
    Case("rule07_batch_reject_not_projected", "R07",
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
    Case("rule08_orig_feb30", "R08", "origination date 2024-02-30",
         "reject 0206", [record(orig_ts="2024-02-30 10:15:30.000000")]),
    Case("rule08_orig_leap_day_valid", "R08",
         "origination 2024-02-29, a real leap day",
         "accepted, Julian 2024060",
         [record(orig_ts="2024-02-29 10:15:30.000000")]),
    Case("rule08_orig_leap_day_nonleap", "R08",
         "origination 2023-02-29, 2023 is not a leap year",
         "reject 0206", [record(orig_ts="2023-02-29 10:15:30.000000")]),
    Case("rule08_orig_century_leap", "R08",
         "origination 2000-02-29, divisible by 400 so a leap year",
         "accepted, Julian 2000060",
         [record(orig_ts="2000-02-29 10:15:30.000000")]),
    Case("rule08_orig_century_nonleap", "R08",
         "origination 1900-02-29, century not divisible by 400",
         "reject 0206", [record(orig_ts="1900-02-29 10:15:30.000000")]),
    Case("rule08_orig_month_13", "R08", "origination month 13",
         "reject 0206", [record(orig_ts="2024-13-01 10:15:30.000000")]),
    Case("rule08_orig_not_numeric", "R08", "origination date is letters",
         "reject 0206", [record(orig_ts="ABCD-EF-GH 10:15:30.000000")]),
    Case("rule09_proc_apr31", "R09", "processing date 2024-04-31",
         "reject 0207", [record(proc_ts="2024-04-31 02:00:00.000000")]),
    Case("rule09_proc_blank_accepted", "R09",
         "processing timestamp blank, as in the sample feed",
         "accepted, RC 0", [record(proc_ts="")]),
    Case("rule10_orig_after_proc", "R10",
         "origination 2024-03-02 after processing 2024-03-01",
         "reject 0208",
         [record(orig_ts="2024-03-02 10:15:30.000000",
                 proc_ts="2024-03-01 02:00:00.000000")]),
    Case("rule11_orig_future", "R11",
         "origination 2024-03-16 after run date 2024-03-15",
         "reject 0209",
         [record(orig_ts="2024-03-16 10:15:30.000000",
                 proc_ts="2024-03-16 12:00:00.000000")]),
    Case("rule11_proc_future", "R11",
         "processing 2024-03-16 after run date 2024-03-15",
         "reject 0209",
         [record(orig_ts="2024-03-14 10:15:30.000000",
                 proc_ts="2024-03-16 12:00:00.000000")]),
    Case("rule11_dates_equal_run_date", "R11",
         "origination and processing both on the run date",
         "accepted, RC 0",
         [record(orig_ts="2024-03-15 10:15:30.000000",
                 proc_ts="2024-03-15 12:00:00.000000")]),
    Case("rule12_acct_expired", "R12",
         "account 6 expired 2024-02-28, origination 2024-03-01",
         "reject 0103 (CBTRN02C code)", [record(card=CARD_EXPIRED)]),
    Case("rule12_acct_expiry_boundary", "R12",
         "account 8 expired 2024-02-28, origination 2024-02-28 (equal "
         "passes, as in CBTRN02C's >=)",
         "accepted, RC 0",
         [record(card=CARD_EXPIRED_NO_LIMIT,
                 orig_ts="2024-02-28 10:15:30.000000")]),
    Case("rule13_credit_limit_at", "R13",
         "account 5: 500.00 - (-100.00) + 400.00 = 1000.00 = limit",
         "accepted, RC 0", [record(card=CARD_LIMIT, amt="400.00")]),
    Case("rule13_credit_limit_over", "R13",
         "account 5: 500.00 - (-100.00) + 400.01 = 1000.01 > limit",
         "reject 0102 (CBTRN02C code)",
         [record(card=CARD_LIMIT, amt="400.01")]),
    Case("rule13_credit_projection", "R13",
         "account 5, four 300.00 / 300.00 / 300.00 / 100.00 purchases: "
         "600 + 300 = 900 fits, 1200 does not, the reject must not "
         "advance the account projection so the third also fails at "
         "1200, then 900 + 100 = 1000 fits",
         "accepted, reject 0102, reject 0102, accepted",
         [record(id="TX00000000000301", amt="300.00", card=CARD_LIMIT),
          record(id="TX00000000000302", amt="300.00", card=CARD_LIMIT),
          record(id="TX00000000000303", amt="300.00", card=CARD_LIMIT),
          record(id="TX00000000000304", amt="100.00", card=CARD_LIMIT)]),
    Case("rule13_acct_reject_not_projected", "R13",
         "account 7 (limit 1000.00, expires 2024-03-10, category balance "
         "999999000.00): 400.00 accepted; 100.00 dated 2024-03-11 rejected "
         "0103 so it must advance neither projection; 599.99 then lands "
         "the category at exactly 999999999.99 and the account at 999.99 "
         "(either projection advanced by the reject would fail it); 0.01 "
         "overflows the category (0205, checked before the limit); 0.02 "
         "as type 02 (separate category key) fails only the limit (0102)",
         "accepted, reject 0103, accepted, reject 0205, reject 0102",
         [record(id="TX00000000000401", amt="400.00", card=CARD_EXP_CAT),
          record(id="TX00000000000402", amt="100.00", card=CARD_EXP_CAT,
                 orig_ts="2024-03-11 10:15:30.000000",
                 proc_ts="2024-03-12 02:00:00.000000"),
          record(id="TX00000000000403", amt="599.99", card=CARD_EXP_CAT),
          record(id="TX00000000000404", amt="0.01", card=CARD_EXP_CAT),
          record(id="TX00000000000405", amt="0.02", type="02",
                 card=CARD_EXP_CAT)]),
    Case("precedence_type_and_card", "R14",
         "type 99 and unknown card on one record",
         "reject 0202 only", [record(type="99", card=CARD_UNKNOWN)]),
    Case("precedence_id_and_amount", "R14",
         "blank id and non-numeric amount on one record",
         "reject 0201 only", [record(id=" " * 16, amt=b"ABCDEFGHIJK")]),
    Case("precedence_expired_and_overlimit", "R14",
         "account 6 is expired and has credit limit 0.00, amount 125.50",
         "reject 0103 only (the code CBTRN02C ends with when both fail)",
         [record(card=CARD_EXPIRED, amt="125.50")]),
    Case("precedence_amount_and_date", "R14",
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
    "RC 4, 262 accepted, 38 rejected 0102 (over limit at posting)")


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

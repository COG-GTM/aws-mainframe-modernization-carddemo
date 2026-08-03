#!/usr/bin/env python3
"""Generate the synthetic edge-case dataset used by the parity harness.

The datasets that ship in app/data/ASCII are excellent for proving that the
Java job reproduces the legacy record-for-record behaviour, but every single
TCATBALF record in them carries a zero balance, so they cannot exercise the
decimal semantics of 1300-COMPUTE-INTEREST at all.

This script builds a *second* dataset in exactly the same fixed-width layouts
(copybooks CVTRA01Y / CVACT01Y / CVACT03Y / CVTRA02Y) that does exercise them.
It writes new files only; nothing under app/ is touched. The disclosure group
file is copied verbatim from app/data/ASCII/discgrp.txt so the rates used are
the ones that actually ship with CardDemo.

Cases covered (see README.md for the expected values):

  acct 101  group A000000000  - direct disclosure-group hit (never exercised by
                                the shipped data, where every lookup falls back)
      01/0001  +100.48   rate 15.00 -> 1.256      -> truncates to 1.25
      01/0002  +100.00   rate 25.00 -> 2.08333... -> truncates to 2.08
      01/0003     0.00   rate 25.00 -> 0.00       -> zero-balance category
  acct 102  group <spaces> - '23' fallback to DEFAULT
      01/0001  -100.48   rate 15.00 -> -1.256     -> truncates toward zero to -1.25
  acct 103  group ZEROAPR - zero rate, so no transaction is written at all
      01/0001  +999.99   rate  0.00
      01/0002  +500.00   rate  0.00
  acct 104  group A000000000 - last account in key order, so 1050-UPDATE-ACCOUNT
                               is never reached for it (see README "dead ELSE")
      01/0002  +12345678.99 rate 25.00 -> 257201.645625 -> truncates to 257201.64
"""
from __future__ import annotations

import pathlib
import shutil
from decimal import Decimal

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[2]
OUT = HERE / "datasets" / "edge"

POSITIVE_OVERPUNCH = "{ABCDEFGHI"
NEGATIVE_OVERPUNCH = "}JKLMNOPQR"


def zoned(value: Decimal, int_digits: int, dec_digits: int) -> str:
    """Signed zoned decimal (trailing EBCDIC overpunch), as in app/data/ASCII."""
    scaled = int(value.scaleb(dec_digits).to_integral_value())
    digits = f"{abs(scaled):0{int_digits + dec_digits}d}"
    assert len(digits) == int_digits + dec_digits, f"{value} overflows PIC S9({int_digits})V9({dec_digits})"
    table = NEGATIVE_OVERPUNCH if scaled < 0 else POSITIVE_OVERPUNCH
    return digits[:-1] + table[int(digits[-1])]


def unsigned(value: int, digits: int) -> str:
    return f"{value:0{digits}d}"


def text(value: str, length: int) -> str:
    assert len(value) <= length
    return value.ljust(length)


def tcatbal(acct: int, type_cd: str, cat_cd: int, balance: str) -> str:
    """CVTRA01Y - TRAN-CAT-BAL-RECORD, RECLN 50."""
    return (
        unsigned(acct, 11)
        + text(type_cd, 2)
        + unsigned(cat_cd, 4)
        + zoned(Decimal(balance), 9, 2)
        + text("", 22)
    )


def account(acct: int, balance: str, group_id: str) -> str:
    """CVACT01Y - ACCOUNT-RECORD, RECLN 300."""
    return (
        unsigned(acct, 11)
        + text("Y", 1)
        + zoned(Decimal(balance), 10, 2)
        + zoned(Decimal("9999.00"), 10, 2)
        + zoned(Decimal("4999.00"), 10, 2)
        + text("2015-01-01", 10)
        + text("2027-01-01", 10)
        + text("2025-01-01", 10)
        + zoned(Decimal("111.11"), 10, 2)
        + zoned(Decimal("222.22"), 10, 2)
        + text("A000000000", 10)
        + text(group_id, 10)
        + text("", 178)
    )


def xref(card: str, cust: int, acct: int) -> str:
    """CVACT03Y - CARD-XREF-RECORD, RECLN 50."""
    return text(card, 16) + unsigned(cust, 9) + unsigned(acct, 11) + text("", 14)


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)

    tcat = [
        tcatbal(101, "01", 1, "100.48"),
        tcatbal(101, "01", 2, "100.00"),
        tcatbal(101, "01", 3, "0.00"),
        tcatbal(102, "01", 1, "-100.48"),
        tcatbal(103, "01", 1, "999.99"),
        tcatbal(103, "01", 2, "500.00"),
        tcatbal(104, "01", 2, "12345678.99"),
    ]
    accts = [
        account(101, "1000.00", "A000000000"),
        account(102, "1000.00", ""),
        account(103, "1000.00", "ZEROAPR"),
        account(104, "1000.00", "A000000000"),
    ]
    xrefs = [
        xref("4111111111110101", 101, 101),
        xref("4111111111110102", 102, 102),
        xref("4111111111110103", 103, 103),
        xref("4111111111110104", 104, 104),
    ]

    for name, rows in (("tcatbal", tcat), ("acctdata", accts), ("cardxref", xrefs)):
        (OUT / f"{name}.txt").write_text("\n".join(rows) + "\n")

    # Reuse the shipped disclosure-group rates verbatim (CRLF stripped).
    src = (REPO / "app" / "data" / "ASCII" / "discgrp.txt").read_text()
    (OUT / "discgrp.txt").write_text(src.replace("\r\n", "\n"))

    print(f"wrote edge dataset to {OUT}")


if __name__ == "__main__":
    main()

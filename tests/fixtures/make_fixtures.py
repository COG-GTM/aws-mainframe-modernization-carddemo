#!/usr/bin/env python3
"""Regenerate the fixed-width fixture files in this directory.

Fixtures are written one record per line (trailing blanks stripped; the
harness pads them back to the copybook record length).  Signed numeric
fields use zoned-decimal overpunch exactly like app/data/ASCII.
Run:  python3 tests/fixtures/make_fixtures.py
"""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "lib"))

from records import (ACCOUNT, CARD, CUSTOMER, DALYTRAN, DISCGRP, TCATBAL,  # noqa: E402
                     TRAN, TRANCATG, TRANTYPE, XREF)

CARD1, CARD2, CARD3, CARD9 = ("4000000000000001", "4000000000000002",
                              "4000000000000003", "4000000000000009")
UNKNOWN_CARD = "4999999999999999"


def acct(i, bal, limit, credit, debit, exp, group):
    return ACCOUNT.encode(**{
        "ACCT-ID": i, "ACCT-ACTIVE-STATUS": "Y", "ACCT-CURR-BAL": bal,
        "ACCT-CREDIT-LIMIT": limit, "ACCT-CASH-CREDIT-LIMIT": 1000,
        "ACCT-OPEN-DATE": "2020-01-01", "ACCT-EXPIRAION-DATE": exp,
        "ACCT-REISSUE-DATE": "2025-01-01", "ACCT-CURR-CYC-CREDIT": credit,
        "ACCT-CURR-CYC-DEBIT": debit, "ACCT-ADDR-ZIP": "10001",
        "ACCT-GROUP-ID": group})


def cust(i, first, last):
    return CUSTOMER.encode(**{
        "CUST-ID": i, "CUST-FIRST-NAME": first, "CUST-MIDDLE-NAME": "Q",
        "CUST-LAST-NAME": last, "CUST-ADDR-LINE-1": f"{i} Main Street",
        "CUST-ADDR-LINE-2": "Apt 1", "CUST-ADDR-LINE-3": "Springfield",
        "CUST-ADDR-STATE-CD": "NY", "CUST-ADDR-COUNTRY-CD": "USA",
        "CUST-ADDR-ZIP": "10001", "CUST-PHONE-NUM-1": "(212)555-0100",
        "CUST-PHONE-NUM-2": "(212)555-0101", "CUST-SSN": 123456789,
        "CUST-GOVT-ISSUED-ID": "DL0000001", "CUST-DOB-YYYY-MM-DD": "1980-05-17",
        "CUST-EFT-ACCOUNT-ID": "0000000001", "CUST-PRI-CARD-HOLDER-IND": "Y",
        "CUST-FICO-CREDIT-SCORE": 750})


def card(num, acct_id, name):
    return CARD.encode(**{
        "CARD-NUM": num, "CARD-ACCT-ID": acct_id, "CARD-CVV-CD": 123,
        "CARD-EMBOSSED-NAME": name, "CARD-EXPIRAION-DATE": "2030-12-31",
        "CARD-ACTIVE-STATUS": "Y"})


def xref(num, cust_id, acct_id):
    return XREF.encode(**{"XREF-CARD-NUM": num, "XREF-CUST-ID": cust_id,
                          "XREF-ACCT-ID": acct_id})


def daly(tid, typ, cat, amt, card_num, desc, ts="2024-06-01 10:00:00.000000"):
    return DALYTRAN.encode(**{
        "DALYTRAN-ID": tid, "DALYTRAN-TYPE-CD": typ, "DALYTRAN-CAT-CD": cat,
        "DALYTRAN-SOURCE": "POS TERM", "DALYTRAN-DESC": desc,
        "DALYTRAN-AMT": amt, "DALYTRAN-MERCHANT-ID": 800000001,
        "DALYTRAN-MERCHANT-NAME": "Test Merchant",
        "DALYTRAN-MERCHANT-CITY": "New York", "DALYTRAN-MERCHANT-ZIP": "10001",
        "DALYTRAN-CARD-NUM": card_num, "DALYTRAN-ORIG-TS": ts,
        "DALYTRAN-PROC-TS": ""})


def tran(tid, typ, cat, amt, card_num, desc, proc_ts):
    return TRAN.encode(**{
        "TRAN-ID": tid, "TRAN-TYPE-CD": typ, "TRAN-CAT-CD": cat,
        "TRAN-SOURCE": "POS TERM", "TRAN-DESC": desc, "TRAN-AMT": amt,
        "TRAN-MERCHANT-ID": 800000001, "TRAN-MERCHANT-NAME": "Test Merchant",
        "TRAN-MERCHANT-CITY": "New York", "TRAN-MERCHANT-ZIP": "10001",
        "TRAN-CARD-NUM": card_num, "TRAN-ORIG-TS": proc_ts,
        "TRAN-PROC-TS": proc_ts})


def tcat(acct_id, typ, cat, bal):
    return TCATBAL.encode(**{"TRANCAT-ACCT-ID": acct_id, "TRANCAT-TYPE-CD": typ,
                             "TRANCAT-CD": cat, "TRAN-CAT-BAL": bal})


def disc(group, typ, cat, rate):
    return DISCGRP.encode(**{"DIS-ACCT-GROUP-ID": group, "DIS-TRAN-TYPE-CD": typ,
                             "DIS-TRAN-CAT-CD": cat, "DIS-INT-RATE": rate})


FIXTURES = {
    # --- master files ------------------------------------------------------
    # acct 1: healthy; acct 2: 100.00 under its limit; acct 3: expired 2020.
    "accounts.txt": [
        acct(10000000001, 1000.00, 5000.00, 0, 0, "2030-12-31", "GOLD"),
        acct(10000000002, 4900.00, 5000.00, 4900.00, 0, "2030-12-31", "NOGROUP"),
        acct(10000000003, 0, 5000.00, 0, 0, "2020-12-31", "GOLD"),
    ],
    "customers.txt": [
        cust(1, "Alice", "Anders"), cust(2, "Bob", "Brown"),
        cust(3, "Carol", "Chen"), cust(9, "Dan", "Dangling"),
    ],
    "cards.txt": [
        card(CARD1, 10000000001, "ALICE ANDERS"),
        card(CARD2, 10000000002, "BOB BROWN"),
        card(CARD3, 10000000003, "CAROL CHEN"),
        card(CARD9, 10000000099, "DAN DANGLING"),
    ],
    # card 9 points at an account that does not exist (reason 101 path).
    "cardxref.txt": [
        xref(CARD1, 1, 10000000001), xref(CARD2, 2, 10000000002),
        xref(CARD3, 3, 10000000003), xref(CARD9, 9, 10000000099),
    ],
    "cardxref_stmt.txt": [
        xref(CARD1, 1, 10000000001), xref(CARD2, 2, 10000000002),
        xref(CARD3, 3, 10000000003),
    ],
    "trantype.txt": [
        TRANTYPE.encode(**{"TRAN-TYPE": "01", "TRAN-TYPE-DESC": "Purchase"}),
        TRANTYPE.encode(**{"TRAN-TYPE": "02", "TRAN-TYPE-DESC": "Payment"}),
    ],
    "trancatg.txt": [
        TRANCATG.encode(**{"TRAN-TYPE-CD": "01", "TRAN-CAT-CD": 1,
                           "TRAN-CAT-TYPE-DESC": "Regular Sales Draft"}),
        TRANCATG.encode(**{"TRAN-TYPE-CD": "01", "TRAN-CAT-CD": 5,
                           "TRAN-CAT-TYPE-DESC": "Interest"}),
        TRANCATG.encode(**{"TRAN-TYPE-CD": "02", "TRAN-CAT-CD": 1,
                           "TRAN-CAT-TYPE-DESC": "Cash payment"}),
    ],
    # --- CBTRN01C / CBTRN02C: daily transactions ---------------------------
    "dailytran.txt": [
        daly("T000000000000001", "01", 1, 100.00, CARD1, "Purchase posts"),
        daly("T000000000000002", "02", 1, -50.00, CARD1, "Payment posts"),
        daly("T000000000000003", "01", 1, 25.50, CARD1, "Purchase posts"),
        daly("T000000000000004", "01", 1, 200.00, CARD2, "Over limit"),
        daly("T000000000000005", "01", 1, 10.00, CARD3, "Account expired"),
        daly("T000000000000006", "01", 1, 5.00, UNKNOWN_CARD, "Unknown card"),
        daly("T000000000000007", "01", 1, 5.00, CARD9, "Account missing"),
    ],
    "tcatbal_posting.txt": [tcat(10000000001, "01", 1, 500.00)],
    # --- CBACT04C: interest -------------------------------------------------
    "tcatbal_interest.txt": [
        tcat(10000000001, "01", 1, 1000.00),   # GOLD 18.00% -> 15.00
        tcat(10000000001, "02", 1, 500.00),    # GOLD rate 0 -> no interest
        tcat(10000000002, "01", 1, 250.00),    # DEFAULT 24.00% -> 5.00
        tcat(10000000002, "01", 2, 333.33),    # DEFAULT 12.00% -> 3.33
        tcat(10000000003, "01", 1, 1234.56),   # GOLD 18.00% -> 18.51
    ],
    "discgrp.txt": [
        disc("GOLD", "01", 1, 18.00), disc("GOLD", "02", 1, 0),
        disc("DEFAULT", "01", 1, 24.00), disc("DEFAULT", "01", 2, 12.00),
    ],
    # --- CBTRN03C / CBSTM03A: posted transactions (sorted by card) ---------
    "transact.txt": [
        tran("0000000000000101", "01", 1, 100.00, CARD1, "Grocery", "2022-03-01 10:00:00.000000"),
        tran("0000000000000102", "02", 1, -50.00, CARD1, "Payment", "2022-03-02 10:00:00.000000"),
        tran("0000000000000103", "01", 1, 200.00, CARD2, "Electronics", "2022-03-03 10:00:00.000000"),
    ],
    "transact_bad_card.txt": [
        tran("0000000000000201", "01", 1, 10.00, UNKNOWN_CARD, "Unknown card", "2022-03-01 10:00:00.000000"),
    ],
    "transact_bad_type.txt": [
        tran("0000000000000301", "99", 1, 10.00, CARD1, "Unknown type", "2022-03-01 10:00:00.000000"),
    ],
    "dateparm.txt": ["2022-01-01 2022-07-06"],
}


def main():
    for name, recs in FIXTURES.items():
        (HERE / name).write_text("".join(r.rstrip() + "\n" for r in recs))
        print(f"wrote {name} ({len(recs)} records)")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Generate deterministic fixed-record inputs and an independent oracle."""

from __future__ import annotations

import argparse
import calendar
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
TEST_DIR = Path(__file__).resolve().parent
RUN_DATE = "2026-10-12"
SOURCE_ID = "CARDDEMO"
POSTING_TS = "2026-10-11-12.34.56.780000"

POSITIVE_OVERPUNCH = "{ABCDEFGHI"
NEGATIVE_OVERPUNCH = "}JKLMNOPQR"
MAX_TRAN_CENTS = 99_999_999_999
MAX_KEY_CENTS = 999_999_999_999_999
MAX_RUN_CENTS = 99_999_999_999_999_999


@dataclass
class Transaction:
    tran_id: str
    card: str = "0000000000000001"
    amount_cents: int | None = 100
    tran_type: str = "01"
    category: str = "0001"
    proc_ts: str = POSTING_TS
    raw_amount: bytes | None = None
    raw_record: bytes | None = None


@dataclass
class Case:
    name: str
    transactions: list[Transaction] = field(default_factory=list)
    xref: list[tuple[str, str]] = field(
        default_factory=lambda: [("0000000000000001", "00000000001")]
    )
    raw_xref: list[bytes] | None = None
    tcatbal: list[tuple[str, str, str, int]] | None = None
    run_date: str = RUN_DATE
    source_id: str = SOURCE_ID
    expected_count: int | None = None
    expected_net_cents: int | None = None
    missing: set[str] = field(default_factory=set)


def overpunched(cents: int, width: int) -> bytes:
    digits = f"{abs(cents):0{width}d}"
    if len(digits) != width:
        raise ValueError(f"amount outside {width}-digit source picture: {cents}")
    sign_table = POSITIVE_OVERPUNCH if cents >= 0 else NEGATIVE_OVERPUNCH
    return (digits[:-1] + sign_table[int(digits[-1])]).encode("ascii")


def decode_overpunched(raw: bytes) -> int:
    if len(raw) < 1:
        raise ValueError("empty amount")
    last = chr(raw[-1])
    if last in POSITIVE_OVERPUNCH:
        sign = 1
        digit = POSITIVE_OVERPUNCH.index(last)
    elif last in NEGATIVE_OVERPUNCH:
        sign = -1
        digit = NEGATIVE_OVERPUNCH.index(last)
    else:
        if not raw.isdigit():
            raise ValueError("non-numeric amount")
        sign = 1
        digit = int(last)
    prefix = raw[:-1]
    if not prefix.isdigit():
        raise ValueError("non-numeric amount")
    return sign * int(prefix.decode("ascii") + str(digit))


def _key(acct: str, tran_type: str, category: str) -> tuple[str, str, str]:
    return acct, tran_type, category


def build_tcat(
    transactions: list[Transaction],
    xref: list[tuple[str, str]],
    include_keys: set[tuple[str, str, str]] | None = None,
) -> list[tuple[str, str, str, int]]:
    accounts = dict(xref)
    sums: dict[tuple[str, str, str], int] = {}
    for transaction in transactions:
        if transaction.amount_cents is None:
            continue
        key = _key(
            accounts[transaction.card],
            transaction.tran_type,
            transaction.category,
        )
        sums[key] = sums.get(key, 0) + transaction.amount_cents
    for key in include_keys or set():
        sums.setdefault(key, 0)
    return [(acct, typ, cat, amount)
            for (acct, typ, cat), amount in sorted(sums.items())]


def _txn(number: int, amount: int, **kwargs: Any) -> Transaction:
    return Transaction(tran_id=f"{number:016d}", amount_cents=amount, **kwargs)


def _account_fixtures() -> list[tuple[str, str]]:
    return [
        ("0000000000000001", "00000000001"),
        ("0000000000000002", "00000000002"),
        ("0000000000000003", "00000000003"),
    ]


def _sample_case() -> Case:
    transaction_lines = (ROOT / "app/data/ASCII/dailytran.txt").read_bytes()
    xref_lines = (ROOT / "app/data/ASCII/cardxref.txt").read_bytes()
    balance_lines = (ROOT / "app/data/ASCII/tcatbal.txt").read_bytes()
    xref: list[tuple[str, str]] = []
    raw_xref: list[bytes] = []
    for source_record in xref_lines.splitlines():
        if len(source_record) > 50:
            raise ValueError("cross-reference sample record exceeds 50 bytes")
        record = source_record.ljust(50, b" ")
        raw_xref.append(record)
        xref.append((record[:16].decode(), record[25:36].decode()))
    transactions: list[Transaction] = []
    raw_records: list[bytes] = []
    for original in transaction_lines.splitlines():
        if len(original) != 350:
            raise ValueError("transaction sample record is not 350 bytes")
        record = bytearray(original)
        record[304:330] = POSTING_TS.encode("ascii")
        raw_records.append(bytes(record))
        transactions.append(
            Transaction(
                tran_id=record[:16].decode(),
                card=record[262:278].decode(),
                amount_cents=decode_overpunched(bytes(record[132:143])),
                tran_type=record[16:18].decode(),
                category=record[18:22].decode(),
                proc_ts=POSTING_TS,
                raw_record=bytes(record),
            )
        )
    transactions.sort(key=lambda item: item.tran_id)
    raw_by_id = {record[:16].decode(): record for record in raw_records}
    transactions = [
        Transaction(
            tran_id=item.tran_id,
            card=item.card,
            amount_cents=item.amount_cents,
            tran_type=item.tran_type,
            category=item.category,
            proc_ts=item.proc_ts,
            raw_amount=raw_by_id[item.tran_id][132:143],
            raw_record=next(
                record for record in raw_records
                if record[:16].decode() == item.tran_id
            ),
        )
        for item in transactions
    ]
    # The original balance file supplies zero-valued opening keys.
    balances: dict[tuple[str, str, str], int] = {}
    for record in balance_lines.splitlines():
        if len(record) != 50:
            raise ValueError("category balance sample record is not 50 bytes")
        acct = record[:11].decode()
        tran_type = record[11:13].decode()
        category = record[13:17].decode()
        balances[(acct, tran_type, category)] = 0
    account_by_card = dict(xref)
    for transaction in transactions:
        key = _key(
            account_by_card[transaction.card],
            transaction.tran_type,
            transaction.category,
        )
        balances[key] = balances.get(key, 0) + int(transaction.amount_cents)
    tcat = [
        (acct, typ, cat, amount)
        for (acct, typ, cat), amount in sorted(balances.items())
    ]
    return Case(
        "21_carddemo_sample_data",
        transactions=transactions,
        xref=xref,
        raw_xref=raw_xref,
        tcatbal=tcat,
    )


def all_cases() -> list[Case]:
    xref = _account_fixtures()
    cases: list[Case] = []

    c = Case("01_balanced_multi_account", xref=xref)
    c.transactions = [
        _txn(1, 1250),
        _txn(2, -400, tran_type="02", category="0002"),
        _txn(3, 75, card=xref[1][0], tran_type="01", category="0001"),
        _txn(4, -25, card=xref[1][0], tran_type="01", category="0001"),
        _txn(5, 800, card=xref[2][0], tran_type="03", category="0004"),
    ]
    c.tcatbal = build_tcat(c.transactions, xref)
    cases.append(c)

    c = Case("02_empty_input", xref=xref, transactions=[])
    c.tcatbal = []
    cases.append(c)

    c = Case("03_single_record", transactions=[_txn(1, 501)])
    c.tcatbal = build_tcat(c.transactions, c.xref)
    cases.append(c)

    c = Case("04_negative_amounts_only", transactions=[
        _txn(1, -1250),
        _txn(2, -75),
    ])
    c.tcatbal = build_tcat(c.transactions, c.xref)
    cases.append(c)

    c = Case("05_max_picture_amounts", xref=xref)
    c.transactions = [
        _txn(1, MAX_TRAN_CENTS),
        _txn(2, -MAX_TRAN_CENTS, tran_type="02"),
        _txn(3, MAX_TRAN_CENTS, tran_type="03"),
        _txn(4, -MAX_TRAN_CENTS, tran_type="03"),
    ]
    c.tcatbal = build_tcat(c.transactions, xref)
    cases.append(c)

    c = Case(
        "06_key_amount_overflow",
        transactions=[
            _txn(number, MAX_TRAN_CENTS) for number in range(1, 10002)
        ],
    )
    c.tcatbal = []
    cases.append(c)

    c = Case("07_out_of_balance_tcatbal", transactions=[_txn(1, 10000)])
    c.tcatbal = [("00000000001", "01", "0001", 9999)]
    cases.append(c)

    c = Case("08_key_missing_from_tcatbal",
             transactions=[_txn(1, 275)], tcatbal=[])
    cases.append(c)

    c = Case(
        "09_tcatbal_only_balance",
        transactions=[_txn(1, 100)],
        xref=xref,
    )
    c.tcatbal = build_tcat(c.transactions, xref) + [
        ("00000000002", "01", "0001", 250)
    ]
    cases.append(c)

    c = Case("10_feeder_totals_match", transactions=[
        _txn(1, 1000),
        _txn(2, -200),
    ])
    c.expected_count = 2
    c.expected_net_cents = 800
    c.tcatbal = build_tcat(c.transactions, c.xref)
    cases.append(c)

    c = Case("11_feeder_totals_mismatch", transactions=[
        _txn(1, 1000),
        _txn(2, -200),
    ])
    c.expected_count = 3
    c.expected_net_cents = 700
    c.tcatbal = build_tcat(c.transactions, c.xref)
    cases.append(c)

    c = Case("12_duplicate_tran_id", transactions=[
        _txn(1, 100),
        _txn(1, 500),
    ])
    c.tcatbal = build_tcat(c.transactions[:1], c.xref)
    cases.append(c)

    c = Case("13_transact_out_of_sequence", transactions=[
        _txn(2, 100),
        _txn(1, 200),
    ])
    c.tcatbal = build_tcat([c.transactions[0]], c.xref)
    cases.append(c)

    c = Case("14_tcatbal_duplicate_key", xref=xref,
             transactions=[_txn(1, 100)])
    c.tcatbal = [
        ("00000000001", "01", "0001", 0),
        ("00000000001", "01", "0001", 0),
    ]
    # The activity belongs to account 2, so the duplicate is detected first.
    c.xref = [(xref[0][0], "00000000002"), xref[1], xref[2]]
    c.tcatbal[0] = ("00000000001", "01", "0001", 0)
    c.tcatbal[1] = ("00000000001", "01", "0001", 0)
    cases.append(c)

    bad_dates = [
        "2026-13-01",
        "2026-02-30",
        "2023-02-29",
        "1900-02-29",
        "          ",
        "20A6-01-01",
        "2024-02-29",
        "2000-02-29",
    ]
    c = Case("15_bad_proc_dates", transactions=[
        _txn(index + 1, 100, proc_ts=date + "-12.00.00.000000")
        for index, date in enumerate(bad_dates)
    ])
    c.tcatbal = build_tcat(c.transactions, c.xref)
    cases.append(c)

    c = Case("16_proc_date_after_run_date", transactions=[
        _txn(1, 100, proc_ts="2026-10-13-00.00.00.000000")
    ])
    c.tcatbal = build_tcat(c.transactions, c.xref)
    cases.append(c)

    c = Case("17_card_not_in_xref", transactions=[
        _txn(1, 100, card="9999999999999999")
    ], tcatbal=[])
    cases.append(c)

    c = Case("18_non_numeric_amount", transactions=[
        Transaction(
            tran_id=f"{1:016d}",
            amount_cents=None,
            raw_amount=b"ABCDEFGHIJK",
        )
    ], tcatbal=[])
    cases.append(c)

    c = Case("19_invalid_run_date", transactions=[], run_date="2026-02-30")
    c.tcatbal = []
    cases.append(c)

    c = Case("20_missing_input_file", transactions=[], tcatbal=[])
    c.missing.add("TRANFILE")
    cases.append(c)

    cases.append(_sample_case())

    c = Case("22_zero_amount", transactions=[_txn(1, 0)])
    c.tcatbal = build_tcat(c.transactions, c.xref)
    cases.append(c)

    return cases


def transaction_record(transaction: Transaction) -> bytes:
    if transaction.raw_record is not None:
        record = bytearray(transaction.raw_record)
        record[304:330] = transaction.proc_ts.encode("ascii")
        return bytes(record)
    record = bytearray(b" " * 350)
    record[0:16] = transaction.tran_id.encode("ascii")
    record[16:18] = transaction.tran_type.encode("ascii")
    record[18:22] = transaction.category.encode("ascii")
    if transaction.raw_amount is not None:
        record[132:143] = transaction.raw_amount
    elif transaction.amount_cents is not None:
        record[132:143] = overpunched(transaction.amount_cents, 11)
    record[262:278] = transaction.card.encode("ascii")
    record[304:330] = transaction.proc_ts.encode("ascii")
    return bytes(record)


def xref_record(card: str, account: str, customer: int) -> bytes:
    return (
        card.encode("ascii")
        + f"{customer:09d}".encode("ascii")
        + account.encode("ascii")
        + b" " * 14
    )


def tcat_record(
    account: str, tran_type: str, category: str, balance: int
) -> bytes:
    return (
        account.encode("ascii")
        + tran_type.encode("ascii")
        + category.encode("ascii")
        + overpunched(balance, 11)
        + b" " * 22
    )


def control_card(case: Case) -> bytes:
    date = case.run_date.encode("ascii")
    source = case.source_id.encode("ascii")
    expected_count = (
        f"{case.expected_count:09d}".encode("ascii")
        if case.expected_count is not None
        else b" " * 9
    )
    expected_net = (
        (("+" if case.expected_net_cents >= 0 else "-")
         + f"{abs(case.expected_net_cents):015d}").encode("ascii")
        if case.expected_net_cents is not None
        else b" " * 16
    )
    record = (
        date + b" " + source + b" " + expected_count + b" "
        + expected_net + b" " * 34
    )
    if len(record) != 80:
        raise AssertionError(f"control card has {len(record)} bytes")
    return record


def write_inputs(case: Case, directory: Path) -> dict[str, Path]:
    directory.mkdir(parents=True, exist_ok=True)
    paths = {
        "GLPARM": directory / "GLPARM.dat",
        "TRANFILE": directory / "TRANFILE.dat",
        "XREFFILE": directory / "XREFFILE.dat",
        "TCATBALF": directory / "TCATBALF.dat",
    }
    paths["GLPARM"].write_bytes(control_card(case))
    if "TRANFILE" not in case.missing:
        paths["TRANFILE"].write_bytes(
            b"".join(transaction_record(item) for item in case.transactions)
        )
    paths["XREFFILE"].write_bytes(
        b"".join(case.raw_xref)
        if case.raw_xref is not None
        else b"".join(
            xref_record(card, account, index + 1)
            for index, (card, account) in enumerate(case.xref)
        )
    )
    paths["TCATBALF"].write_bytes(
        b"".join(
            tcat_record(account, tran_type, category, balance)
            for account, tran_type, category, balance in (case.tcatbal or [])
        )
    )
    return paths


def _valid_date(value: str) -> bool:
    try:
        year, month, day = (int(part) for part in value.split("-"))
        return 1 <= year <= 9999 and 1 <= month <= 12 and (
            1 <= day <= calendar.monthrange(year, month)[1]
        )
    except (ValueError, TypeError):
        return False


def _format_amount(cents: int, width: int) -> bytes:
    sign = b"+" if cents >= 0 else b"-"
    digits = f"{abs(cents):0{width - 1}d}".encode("ascii")
    return sign + digits


def _detail_record(
    sequence: int,
    account: str,
    tran_type: str,
    category: str,
    count: int,
    debits: int,
    credits: int,
    net: int,
    balance: int,
    tie: str,
) -> bytes:
    fields = (
        b"D"
        + f"{sequence:09d}".encode("ascii")
        + account.encode("ascii")
        + tran_type.encode("ascii")
        + category.encode("ascii")
        + f"{count:09d}".encode("ascii")
        + _format_amount(debits, 16)
        + _format_amount(credits, 16)
        + _format_amount(net, 16)
        + _format_amount(balance, 16)
        + tie.encode("ascii")
        + b" " * 49
    )
    if len(fields) != 150:
        raise AssertionError(f"detail has {len(fields)} bytes")
    return fields


def _trailer_record(
    detail_count: int,
    transaction_count: int,
    debits: int,
    credits: int,
    net: int,
    hash_total: int,
) -> bytes:
    record = (
        b"T"
        + f"{detail_count:09d}".encode("ascii")
        + f"{transaction_count:011d}".encode("ascii")
        + _format_amount(debits, 18)
        + _format_amount(credits, 18)
        + _format_amount(net, 18)
        + f"{hash_total:018d}".encode("ascii")
        + f"{detail_count + 2:09d}".encode("ascii")
        + b" " * 48
    )
    if len(record) != 150:
        raise AssertionError(f"trailer has {len(record)} bytes")
    return record


def oracle(case: Case) -> dict[str, Any]:
    """Model the interface bytes without reading a program output."""
    header = (
        b"HGLSUMM01"
        + case.source_id.encode("ascii")
        + case.run_date.replace("-", "").encode("ascii")
        + b"0100"
        + b" " * 121
    )
    exceptions: list[str] = []
    rc = 0
    accepted: list[tuple[Transaction, str]] = []
    rejected = 0
    warned = 0
    source_debits = 0
    source_credits = 0
    previous_id: str | None = None
    aborted = False
    absent = False

    if not _valid_date(case.run_date) or not (1900 <= int(case.run_date[:4]) <= 2099):
        return {
            "interface": None,
            "rc": 12,
            "accepted": 0,
            "debits": 0,
            "credits": 0,
            "net": 0,
            "details": 0,
            "hash": 0,
            "exceptions": ["E02 CONTROL CARD INVALID"],
        }
    if "TRANFILE" in case.missing:
        return {
            "interface": None,
            "rc": 12,
            "accepted": 0,
            "debits": 0,
            "credits": 0,
            "net": 0,
            "details": 0,
            "hash": 0,
            "exceptions": ["E01 FILE OPEN READ WRITE OR CLOSE ERROR"],
        }

    xref_map: dict[str, str] = {}
    previous_card: str | None = None
    for card, account in case.xref:
        if previous_card is not None and card <= previous_card:
            exceptions.append("E05 XREF ORDER OR ACCOUNT INVALID")
            rc = 12
            aborted = True
            break
        if not account.isdigit():
            exceptions.append("E05 XREF ORDER OR ACCOUNT INVALID")
            rc = 12
            aborted = True
            break
        xref_map[card] = account
        previous_card = card
    if aborted:
        return {
            "interface": header,
            "rc": rc,
            "accepted": 0,
            "debits": 0,
            "credits": 0,
            "net": 0,
            "details": 0,
            "hash": 0,
            "exceptions": exceptions,
        }

    for transaction in case.transactions:
        if previous_id is not None and transaction.tran_id < previous_id:
            exceptions.append("E03 TRANSACTION KEY OUT OF ORDER")
            rc = 12
            aborted = True
            break
        if previous_id is not None and transaction.tran_id == previous_id:
            exceptions.append("R01 DUPLICATE TRANSACTION KEY")
            rejected += 1
            rc = max(rc, 4)
            previous_id = transaction.tran_id
            continue
        previous_id = transaction.tran_id

        if transaction.amount_cents is None:
            exceptions.append("R02 TRANSACTION AMOUNT NOT NUMERIC")
            rejected += 1
            rc = max(rc, 4)
            continue
        if not transaction.category.isdigit():
            exceptions.append("R03 TRANSACTION CATEGORY NOT NUMERIC")
            rejected += 1
            rc = max(rc, 4)
            continue
        account = xref_map.get(transaction.card)
        if account is None:
            exceptions.append("R04 CARD NOT FOUND IN CROSS REFERENCE")
            rejected += 1
            rc = max(rc, 4)
            continue

        accepted.append((transaction, account))
        if transaction.amount_cents >= 0:
            source_debits += transaction.amount_cents
        else:
            source_credits += -transaction.amount_cents
        proc_date = transaction.proc_ts[:10]
        if not _valid_date(proc_date):
            exceptions.append("W01 PROCESSING DATE IS INVALID")
            warned += 1
            rc = max(rc, 4)
        elif proc_date > case.run_date:
            exceptions.append("W02 PROCESSING DATE AFTER RUN DATE")
            warned += 1
            rc = max(rc, 4)

    if aborted:
        return {
            "interface": header,
            "rc": rc,
            "accepted": len(accepted),
            "debits": source_debits,
            "credits": source_credits,
            "net": source_debits - source_credits,
            "details": 0,
            "hash": 0,
            "exceptions": exceptions,
        }
    if not case.transactions:
        exceptions.append("W03 NO TRANSACTIONS READ")
        rc = max(rc, 4)

    activity: dict[tuple[str, str, str], list[Transaction]] = {}
    for transaction, account in accepted:
        key = (account, transaction.tran_type, transaction.category)
        activity.setdefault(key, []).append(transaction)
    ordered_activity = sorted(activity.items())
    tcat = sorted(case.tcatbal or [])
    for previous, current in zip(tcat, tcat[1:]):
        if current[:3] <= previous[:3]:
            exceptions.append("E04 CATEGORY BALANCE ORDER OR VALUE INVALID")
            rc = 12
            aborted = True
            break

    details: list[bytes] = []
    hash_total = 0
    build_debits = 0
    build_credits = 0
    build_count = 0
    build_trans_count = 0
    balance_index = 0
    balance_only = 0
    if aborted:
        return {
            "interface": header,
            "rc": rc,
            "accepted": len(accepted),
            "debits": source_debits,
            "credits": source_credits,
            "net": source_debits - source_credits,
            "details": 0,
            "hash": 0,
            "exceptions": exceptions,
        }

    for key, transactions in ordered_activity:
        if any(
            transaction.amount_cents is None
            for transaction in transactions
        ):
            raise AssertionError("rejected amount entered accepted activity")
        debits = sum(
            max(int(transaction.amount_cents), 0)
            for transaction in transactions
        )
        credits = sum(
            max(-int(transaction.amount_cents), 0)
            for transaction in transactions
        )
        if debits > MAX_KEY_CENTS or credits > MAX_KEY_CENTS:
            exceptions.append("E06 AMOUNT OR HASH CAPACITY EXCEEDED")
            rc = 12
            aborted = True
            break

        while balance_index < len(tcat) and tcat[balance_index][:3] < key:
            if tcat[balance_index][3] != 0:
                exceptions.append("B03 NONZERO BALANCE HAS NO ACTIVITY")
                balance_only += 1
                rc = max(rc, 8)
            balance_index += 1

        if balance_index < len(tcat) and tcat[balance_index][:3] == key:
            balance = tcat[balance_index][3]
            balance_index += 1
            tie = "T" if debits - credits == balance else "D"
            if tie == "D":
                exceptions.append("B01 ACTIVITY DIFFERS FROM CATEGORY BALANCE")
                rc = max(rc, 8)
        else:
            balance = 0
            tie = "M"
            exceptions.append("B02 ACTIVITY KEY MISSING FROM BALANCE FILE")
            rc = max(rc, 8)

        account, tran_type, category = key
        build_count += 1
        build_trans_count += len(transactions)
        build_debits += debits
        build_credits += credits
        hash_total += int(account)
        if hash_total > 999_999_999_999_999_999:
            exceptions.append("E06 AMOUNT OR HASH CAPACITY EXCEEDED")
            rc = 12
            aborted = True
            break
        details.append(
            _detail_record(
                build_count,
                account,
                tran_type,
                category,
                len(transactions),
                debits,
                credits,
                debits - credits,
                balance,
                tie,
            )
        )

    if not aborted:
        while balance_index < len(tcat):
            if tcat[balance_index][3] != 0:
                exceptions.append("B03 NONZERO BALANCE HAS NO ACTIVITY")
                balance_only += 1
                rc = max(rc, 8)
            balance_index += 1

    if case.expected_count is not None:
        if case.expected_count != len(accepted):
            exceptions.append("B04 EXPECTED TRANSACTION COUNT DIFFERS")
            rc = max(rc, 8)
        expected_net = source_debits - source_credits
        if case.expected_net_cents != expected_net:
            exceptions.append("B05 EXPECTED NET AMOUNT DIFFERS")
            rc = max(rc, 8)

    if aborted:
        interface = header + b"".join(details)
    else:
        net = build_debits - build_credits
        interface = header + b"".join(details) + _trailer_record(
            build_count,
            build_trans_count,
            build_debits,
            build_credits,
            net,
            hash_total,
        )
    return {
        "interface": interface,
        "rc": rc,
        "accepted": len(accepted),
        "debits": source_debits,
        "credits": source_credits,
        "net": source_debits - source_credits,
        "details": build_count,
        "hash": hash_total,
        "exceptions": exceptions,
        "rejected": rejected,
        "warned": warned,
        "balance_only": balance_only,
    }


def write_expected() -> None:
    for case in all_cases():
        result = oracle(case)
        expected = TEST_DIR / "expected" / case.name
        expected.mkdir(parents=True, exist_ok=True)
        interface = result["interface"]
        if interface is None:
            (expected / "GLSUMOUT.dat").unlink(missing_ok=True)
            (expected / "GLSUMOUT.absent").write_text("absent\n")
        else:
            (expected / "GLSUMOUT.absent").unlink(missing_ok=True)
            (expected / "GLSUMOUT.dat").write_bytes(interface)
        (expected / "RC").write_text(f'{result["rc"]}\n')
        print(f'{case.name:34} RC {result["rc"]:2} '
              f'GLSUMOUT {"absent" if interface is None else len(interface)}')


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--write-expected", action="store_true")
    args = parser.parse_args()
    if args.write_expected:
        write_expected()
    else:
        parser.error("use --write-expected")


if __name__ == "__main__":
    main()

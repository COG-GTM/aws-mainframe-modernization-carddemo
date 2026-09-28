"""Record counts and representative field values for every sample file (decoded from EBCDIC)."""

from decimal import Decimal

import bcrypt
import pytest

from etl import convert
from etl.layouts import EBCDIC, LAYOUTS


def rows(layout: str, output: str | None = None) -> list[dict]:
    lay = LAYOUTS[layout]
    decoded = convert.decode(lay)
    out = next(o for o in lay.outputs if output in (None, o.name))
    return [dict(zip((c.name for c in out.columns), r)) for r in decoded[out.name]]


def by(rs: list[dict], key: str) -> dict:
    return {r[key]: r for r in rs}


def test_every_ebcdic_file_is_covered():
    covered = {lay.input.name for lay in LAYOUTS.values()} | {d.name for lay in LAYOUTS.values() for d in lay.duplicates}
    present = {p.name for p in EBCDIC.iterdir() if not p.name.startswith(".")}
    assert present <= covered


def test_accdata_is_duplicate_of_acctdata():
    lay = LAYOUTS["account"]
    assert [d.read_bytes() for d in lay.duplicates] == [lay.input.read_bytes()]


@pytest.mark.parametrize(
    "layout,expected",
    [
        ("usrsec", {"user_security": 10}),
        ("account", {"account": 50}),
        ("card", {"card": 50}),
        ("cardxref", {"card_xref": 50}),
        ("customer", {"customer": 50}),
        ("dalytran", {"daily_transaction": 300}),
        ("tranfile_init", {"transaction": 0}),
        ("discgrp", {"disclosure_group": 51}),
        ("tcatbal", {"tran_cat_balance": 50}),
        ("trancatg", {"transaction_category": 18}),
        ("trantype", {"transaction_type": 7}),
        ("export", {"export_customer": 50, "export_account": 50, "export_transaction": 300,
                    "export_card_xref": 50, "export_card": 50}),
        ("dbpautp0", {"pending_auth_summary": 21, "pending_auth_detail": 202}),
    ],
)
def test_record_counts(layout, expected):
    assert {k: len(v) for k, v in convert.decode(LAYOUTS[layout]).items()} == expected


def test_usrsec():
    rs = by(rows("usrsec"), "user_id")
    assert list(rs) == [f"ADMIN00{i}" for i in range(1, 6)] + [f"USER000{i}" for i in range(1, 6)]
    admin = rs["ADMIN001"]
    assert (admin["first_name"], admin["last_name"], admin["user_type"]) == ("MARGARET", "GOLD", "A")
    assert rs["USER0003"]["last_name"] == "ALME" and rs["USER0003"]["user_type"] == "U"
    assert bcrypt.checkpw(b"PASSWORD", admin["password_hash"].encode())
    assert not bcrypt.checkpw(b"password", admin["password_hash"].encode())


def test_account():
    rs = by(rows("account"), "acct_id")
    a1 = rs[1]
    assert a1["active_status"] == "Y"
    assert a1["curr_bal"] == Decimal("194.00") and a1["credit_limit"] == Decimal("2020.00")
    assert a1["cash_credit_limit"] == Decimal("1020.00")
    assert (a1["open_date"], a1["expiration_date"], a1["reissue_date"]) == ("2014-11-20", "2025-05-20", "2025-05-20")
    assert a1["group_id"] is None and a1["addr_zip"] == "A000000000"
    assert rs[49]["addr_zip"] == "ZEROAPR"
    assert rs[49]["open_date"] == "2019-04-06"
    assert min(rs) == 1 and max(rs) == 50


def test_card():
    rs = by(rows("card"), "card_num")
    c = rs["0500024453765740"]
    assert (c["acct_id"], c["cvv_cd"], c["embossed_name"]) == (50, 747, "Aniya Von")
    assert c["expiration_date"] == "2023-03-09" and c["active_status"] == "Y"


def test_card_xref():
    rs = by(rows("cardxref"), "card_num")
    assert rs["0500024453765740"]["cust_id"] == 50 and rs["0500024453765740"]["acct_id"] == 50
    assert {r["acct_id"] for r in rs.values()} == set(range(1, 51))


def test_customer():
    rs = by(rows("customer"), "cust_id")
    c = rs[1]
    assert (c["first_name"], c["middle_name"], c["last_name"]) == ("Immanuel", "Madeline", "Kessler")
    assert c["addr_line_1"] == "618 Deshaun Route" and c["addr_state_cd"] == "NC"
    assert c["ssn"] == "020973888" and c["dob"] == "1961-06-08"
    assert c["phone_num_1"] == "(908)119-8310" and c["fico_credit_score"] == 274
    assert all(len(r["ssn"]) == 9 for r in rs.values())


def test_daily_transaction():
    rs = rows("dalytran")
    assert [r["load_seq"] for r in rs] == list(range(1, 301))
    first = rs[0]
    assert first["tran_id"] == "0000000000683580"
    assert (first["type_cd"], first["cat_cd"], first["source"]) == ("01", 1, "POS TERM")
    assert first["amt"] == Decimal("504.77") and first["merchant_id"] == 800000000
    assert first["card_num"] == "4859452612877065"
    assert first["orig_ts"] == "2022-06-10 19:27:53.000000" and first["proc_ts"] is None
    neg = rs[1]
    assert neg["tran_id"] == "0000000001774260" and neg["amt"] == Decimal("-919.00") and neg["type_cd"] == "03"
    assert sum(r["amt"] < 0 for r in rs) > 0


def test_transaction_init_is_priming_record_only():
    raw = LAYOUTS["tranfile_init"].input.read_bytes()
    assert len(raw) == 350
    assert rows("tranfile_init") == []


def test_disclosure_group():
    rs = {(r["acct_group_id"], r["type_cd"], r["cat_cd"]): r["int_rate"] for r in rows("discgrp")}
    assert rs[("A000000000", "01", 1)] == Decimal("15.00")
    assert rs[("DEFAULT", "07", 1)] == Decimal("15.00")
    assert rs[("ZEROAPR", "01", 1)] == Decimal("0.00")
    assert {k[0] for k in rs} == {"A000000000", "DEFAULT", "ZEROAPR"}


def test_tran_cat_balance():
    rs = rows("tcatbal")
    assert {(r["type_cd"], r["cat_cd"], r["balance"]) for r in rs} == {("01", 1, Decimal("0.00"))}
    assert sorted(r["acct_id"] for r in rs) == list(range(1, 51))


def test_reference_tables():
    types = {r["type_cd"]: r["description"] for r in rows("trantype")}
    assert types == {"01": "Purchase", "02": "Payment", "03": "Credit", "04": "Authorization",
                     "05": "Refund", "06": "Reversal", "07": "Adjustment"}
    cats = {(r["type_cd"], r["cat_cd"]): r["description"] for r in rows("trancatg")}
    assert cats[("01", 5)] == "Interest Amount"
    assert cats[("02", 2)] == "Electronic payment"
    assert cats[("07", 1)] == "Sales draft credit adjustment"
    assert {t for t, _ in cats} == set(types)


def test_export_redefines():
    cust = rows("export", "export_customer")
    assert cust[0]["rec_type"] == "C" and cust[0]["sequence_num"] == 1
    assert cust[0]["export_ts"] == "2025-09-28 22:53:40.000000" and cust[0]["region_code"] == "NORTH"
    assert cust[0]["first_name"] == "IMMANUEL" and cust[0]["addr_line_3"] == "ALTENWERTHSHIRE"
    assert cust[0]["fico_credit_score"] == 300 and cust[0]["ssn"] == "020973888"
    acct = rows("export", "export_account")
    assert acct[0]["acct_id"] == 1 and acct[0]["credit_limit"] == Decimal("2020.00")
    assert acct[0]["addr_zip"] is None  # low-values in the export file
    tran = rows("export", "export_transaction")
    assert tran[0]["tran_id"] == "0000000000683580" and tran[0]["amt"] == Decimal("504.77")
    xref = rows("export", "export_card_xref")
    assert (xref[0]["card_num"], xref[0]["cust_id"], xref[0]["acct_id"]) == ("0500024453765740", 50, 50)
    card = rows("export", "export_card")
    assert card[0]["cvv_cd"] == 747 and card[0]["sequence_num"] == 460
    seqs = sorted(r["sequence_num"] for o in ("export_customer", "export_account", "export_transaction",
                                              "export_card_xref", "export_card") for r in rows("export", o))
    # CBEXPORT numbers records across all types; the sample has a gap at 451-459 (source data).
    assert len(seqs) == len(set(seqs)) == 500
    assert seqs == list(range(1, 451)) + list(range(460, 510))

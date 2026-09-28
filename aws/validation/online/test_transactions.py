"""COTRN00C transaction list, COTRN01C detail, COTRN02C add (edits in PROCESS-ENTER-KEY order)."""

import pytest

from conftest import expect_error

pytestmark = pytest.mark.online

ACCT1_CARD = "9680294154603697"  # CXACAIX: account 00000000001 -> card


def valid_add(**overrides) -> dict:
    body = {"acctId": "00000000001", "typeCd": "01", "catCd": "0001", "source": "POS TERM",
            "description": "Validation purchase", "amt": "-00000012.34", "origDate": "2022-07-18",
            "procDate": "2022-07-18", "merchantId": "000000123", "merchantName": "Validation Store",
            "merchantCity": "Seattle", "merchantZip": "98101"}
    body.update(overrides)
    return body


def next_tran_id(db) -> str:
    max_id = db.execute("SELECT COALESCE(MAX(CAST(tran_id AS NUMERIC)), 0) FROM transaction").fetchone()[0]
    return f"{int(max_id) + 1:016d}"


@pytest.mark.parametrize("overrides, status, message", [
    ({"acctId": None}, 400, "Account or Card Number must be entered..."),
    ({"acctId": "12AB"}, 400, "Account ID must be Numeric..."),
    ({"acctId": "99999999999"}, 404, "Account ID NOT found..."),
    ({"acctId": None, "cardNum": "12AB"}, 400, "Card Number must be Numeric..."),
    ({"acctId": None, "cardNum": "9999999999999999"}, 404, "Card Number NOT found..."),
    ({"typeCd": ""}, 400, "Type CD can NOT be empty..."),
    ({"catCd": ""}, 400, "Category CD can NOT be empty..."),
    ({"source": ""}, 400, "Source can NOT be empty..."),
    ({"description": ""}, 400, "Description can NOT be empty..."),
    ({"amt": ""}, 400, "Amount can NOT be empty..."),
    ({"origDate": ""}, 400, "Orig Date can NOT be empty..."),
    ({"procDate": ""}, 400, "Proc Date can NOT be empty..."),
    ({"merchantId": ""}, 400, "Merchant ID can NOT be empty..."),
    ({"merchantName": ""}, 400, "Merchant Name can NOT be empty..."),
    ({"merchantCity": ""}, 400, "Merchant City can NOT be empty..."),
    ({"merchantZip": ""}, 400, "Merchant Zip can NOT be empty..."),
    ({"typeCd": "AB"}, 400, "Type CD must be Numeric..."),
    ({"catCd": "XY"}, 400, "Category CD must be Numeric..."),
    ({"amt": "12.3"}, 400, "Amount should be in format -99999999.99"),
    ({"amt": "-0000001X.34"}, 400, "Amount should be in format -99999999.99"),
    ({"origDate": "2022/07/18"}, 400, "Orig Date should be in format YYYY-MM-DD"),
    ({"procDate": "18-07-2022"}, 400, "Proc Date should be in format YYYY-MM-DD"),
    ({"origDate": "2022-02-30"}, 400, "Orig Date - Not a valid date..."),
    ({"procDate": "2022-13-01"}, 400, "Proc Date - Not a valid date..."),
    ({"merchantId": "ABC"}, 400, "Merchant ID must be Numeric..."),
    # first failing edit wins (WS-ERR-FLG), exactly like the single WS-MESSAGE line of COTRN2A
    ({"typeCd": "", "catCd": "", "amt": "bad"}, 400, "Type CD can NOT be empty..."),
])
def test_add_edits(user_api, overrides, status, message):
    body = {k: v for k, v in valid_add(**overrides).items() if v is not None}
    expect_error(user_api.post("/transactions", json=body), status, message, "COTRN02C")


def test_add_by_account_resolves_card_and_writes_transact(user_api, db):
    expected_id = next_tran_id(db)
    resp = user_api.post("/transactions", json=valid_add())
    assert resp.status_code == 201, resp.text
    body = resp.json()
    assert body["tranId"] == expected_id
    # STRING 'Transaction added successfully. ' ' Your Tran ID is ' TRAN-ID '.' (see report: blank collapsed)
    assert " ".join(body["message"].split()) == f"Transaction added successfully. Your Tran ID is {expected_id}."
    row = db.execute("SELECT type_cd, cat_cd, source, description, amt, merchant_id, merchant_name, card_num, "
                     "orig_ts::date::text, proc_ts::date::text FROM transaction WHERE tran_id = %s",
                     (expected_id,)).fetchone()
    assert row[:4] == ("01", 1, "POS TERM", "Validation purchase")
    assert (f"{row[4]:.2f}", row[5], row[6], row[7], row[8], row[9]) == (
        "-12.34", 123, "Validation Store", ACCT1_CARD, "2022-07-18", "2022-07-18")


def test_add_by_card_number(user_api, db):
    expected_id = next_tran_id(db)
    resp = user_api.post("/transactions", json=valid_add(acctId=None, cardNum=ACCT1_CARD, amt="+00000100.00"))
    assert resp.status_code == 201, resp.text
    assert resp.json()["tranId"] == expected_id


@pytest.fixture
def twelve_transactions(user_api, db) -> list[str]:
    ids = []
    while db.execute("SELECT count(*) FROM transaction").fetchone()[0] < 12:
        resp = user_api.post("/transactions", json=valid_add(description=f"Paging row {len(ids)}"))
        assert resp.status_code == 201, resp.text
        ids.append(resp.json()["tranId"])
    return [r[0] for r in db.execute("SELECT tran_id FROM transaction ORDER BY tran_id")]


def test_list_paging(user_api, twelve_transactions):
    # COTRN00C: 10 rows per screen, STARTBR/READNEXT forward, READPREV backward
    first = user_api.get("/transactions").json()
    assert [t["tranId"] for t in first["items"]] == twelve_transactions[:10]
    assert first["hasNext"] and not first["hasPrev"]
    second = user_api.get("/transactions", params={"startKey": first["lastKey"], "direction": "next"}).json()
    assert [t["tranId"] for t in second["items"]] == twelve_transactions[10:20]
    assert second["hasPrev"]
    back = user_api.get("/transactions", params={"startKey": second["firstKey"], "direction": "prev"}).json()
    assert [t["tranId"] for t in back["items"]] == twelve_transactions[:10]
    item = first["items"][0]
    assert set(item) == {"tranId", "origDate", "description", "amt"}


def test_list_start_key_must_be_numeric(user_api):
    expect_error(user_api.get("/transactions", params={"startKey": "ABC"}), 400, "Tran ID must be Numeric ...",
                 "COTRN00C")


def test_detail(user_api, twelve_transactions, db):
    tran_id = twelve_transactions[0]
    detail = user_api.get(f"/transactions/{tran_id}").json()
    card, desc, amt = db.execute("SELECT card_num, description, amt FROM transaction WHERE tran_id = %s",
                                 (tran_id,)).fetchone()
    assert (detail["tranId"], detail["cardNum"], detail["description"], detail["amt"]) == (
        tran_id, card, desc, f"{amt:.2f}")


def test_detail_not_found(user_api):
    expect_error(user_api.get("/transactions/9999999999999999"), 404, "Transaction ID NOT found...", "COTRN01C")

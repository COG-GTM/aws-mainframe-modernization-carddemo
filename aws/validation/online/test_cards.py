"""COCRDLIC card list, COCRDSLC card detail, COCRDUPC card update."""

import pytest

from conftest import expect_error

pytestmark = pytest.mark.online


def test_list_first_page_is_seven_cards_in_key_order(user_api, db):
    page = user_api.get("/cards").json()
    expected = [r[0] for r in db.execute('SELECT card_num FROM card ORDER BY card_num COLLATE "C" LIMIT 7')]
    assert [c["cardNum"] for c in page["items"]] == expected
    assert page["hasNext"] and not page["hasPrev"]


def test_list_paging_forward_and_back(user_api, db):
    first = user_api.get("/cards").json()
    second = user_api.get("/cards", params={"startKey": first["lastKey"], "direction": "next"}).json()
    expected = [r[0] for r in db.execute('SELECT card_num FROM card ORDER BY card_num COLLATE "C" LIMIT 7 OFFSET 7')]
    assert [c["cardNum"] for c in second["items"]] == expected
    back = user_api.get("/cards", params={"startKey": second["firstKey"], "direction": "prev"}).json()
    assert [c["cardNum"] for c in back["items"]] == [c["cardNum"] for c in first["items"]]


def test_list_last_page_has_no_next(user_api):
    last = user_api.get("/cards", params={"startKey": "9999999999999999", "direction": "prev"}).json()
    assert last["items"] and not last["hasNext"]


def test_list_filter_by_account(user_api, db):
    cards = [r[0] for r in db.execute("SELECT card_num FROM card WHERE acct_id = 5")]
    page = user_api.get("/cards", params={"acctId": "00000000005"}).json()
    assert sorted(c["cardNum"] for c in page["items"]) == sorted(cards)


@pytest.mark.parametrize("params, message", [
    ({"acctId": "12AB"}, "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"),
    ({"cardNum": "1234"}, "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"),
])
def test_list_filter_edits(user_api, params, message):
    expect_error(user_api.get("/cards", params=params), 400, message, "COCRDLIC")


def test_list_no_records(user_api):
    page = user_api.get("/cards", params={"acctId": "99999999999"}).json()
    assert page["items"] == [] and page["message"] == "NO RECORDS FOUND FOR THIS SEARCH CONDITION."


def test_detail(user_api, db):
    card, acct, name, status = db.execute(
        "SELECT card_num, acct_id, embossed_name, active_status FROM card ORDER BY card_num LIMIT 1").fetchone()
    detail = user_api.get(f"/cards/{card}", params={"acctId": acct}).json()
    assert (detail["cardNum"], detail["acctId"], detail["embossedName"], detail["activeStatus"]) == (
        card, acct, name, status)


def test_detail_edits(user_api, db):
    card = db.execute("SELECT card_num FROM card LIMIT 1").fetchone()[0]
    expect_error(user_api.get("/cards/12345"), 400, "Card number if supplied must be a 16 digit number",
                 "COCRDSLC")
    expect_error(user_api.get(f"/cards/{card}", params={"acctId": "99999999999"}), 404,
                 "Did not find cards for this search condition", "COCRDSLC")


def _update_body(detail: dict, **overrides) -> dict:
    body = {"acctId": str(detail["acctId"]), "embossedName": detail["embossedName"],
            "activeStatus": detail["activeStatus"], "expirationDate": detail["expirationDate"][:7],
            "version": detail["version"]}
    body.update(overrides)
    return body


@pytest.fixture
def card_detail(user_api, db) -> dict:
    card, acct = db.execute("SELECT card_num, acct_id FROM card WHERE acct_id = 7").fetchone()
    return user_api.get(f"/cards/{card}", params={"acctId": acct}).json()


@pytest.mark.parametrize("overrides, message", [
    ({}, "No change detected with respect to values fetched."),
    ({"embossedName": "J0hn Smith"}, "Card name can only contain alphabets and spaces"),
    ({"embossedName": " "}, "Card name not provided"),
    ({"activeStatus": "X"}, "Card Active Status must be Y or N"),
    ({"expirationDate": "2030-13"}, "Card expiry month must be between 1 and 12"),
    ({"expirationDate": "1900-01"}, "Invalid card expiry year"),
    ({"acctId": ""}, "Account number not provided"),
])
def test_update_edits(user_api, card_detail, overrides, message):
    resp = user_api.put(f"/cards/{card_detail['cardNum']}", json=_update_body(card_detail, **overrides))
    expect_error(resp, 422 if message.startswith("No change") else 400, message, "COCRDUPC")


def test_update_happy_path_and_stale_version(user_api, card_detail, db):
    body = _update_body(card_detail, embossedName="Validation Holder", activeStatus="N", expirationDate="2031-02")
    resp = user_api.put(f"/cards/{card_detail['cardNum']}", json=body)
    assert resp.status_code == 200, resp.text
    assert resp.json()["message"] == "Changes committed to database"
    name, status, exp = db.execute("SELECT embossed_name, active_status, expiration_date FROM card "
                                   "WHERE card_num = %s", (card_detail["cardNum"],)).fetchone()
    assert (name, status, exp.year, exp.month) == ("Validation Holder", "N", 2031, 2)
    body["embossedName"] = "Another Name"
    expect_error(user_api.put(f"/cards/{card_detail['cardNum']}", json=body), 409,
                 "Record changed by some one else. Please review", "COCRDUPC")

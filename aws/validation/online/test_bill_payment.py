"""COBIL00C bill payment: pays the full ACCT-CURR-BAL, writes a type 02 / cat 2 TRANSACT record."""

import pytest

from conftest import expect_error

pytestmark = pytest.mark.online


@pytest.fixture
def payable(db) -> tuple[int, str, str]:
    acct, bal = db.execute("SELECT acct_id, curr_bal FROM account WHERE curr_bal > 0 AND acct_id <> 1 "
                           "ORDER BY acct_id LIMIT 1").fetchone()
    card = db.execute("SELECT card_num FROM card_xref WHERE acct_id = %s ORDER BY card_num LIMIT 1",
                      (acct,)).fetchone()[0]
    return int(acct), f"{bal:.2f}", card


def test_balance_view(user_api, payable):
    acct, bal, _ = payable
    assert user_api.get(f"/bill-payments/{acct:011d}").json() == {"acctId": acct, "currBal": bal}


def test_blank_account(user_api):
    expect_error(user_api.post("/bill-payments", json={"acctId": ""}), 400, "Acct ID can NOT be empty...",
                 "COBIL00C")


def test_unknown_account(user_api):
    expect_error(user_api.post("/bill-payments", json={"acctId": "99999999999"}), 404,
                 "Account ID NOT found...", "COBIL00C")


def test_pay_full_balance_then_nothing_to_pay(user_api, db, payable):
    acct, bal, card = payable
    expected_id = f"{int(db.execute('SELECT COALESCE(MAX(CAST(tran_id AS NUMERIC)), 0) FROM transaction').fetchone()[0]) + 1:016d}"
    resp = user_api.post("/bill-payments", json={"acctId": f"{acct:011d}"})
    assert resp.status_code == 201, resp.text
    body = resp.json()
    assert (body["tranId"], body["amount"]) == (expected_id, bal)
    # STRING 'Payment successful. ' ' Your Transaction ID is ' TRAN-ID '.' (see report: blank collapsed)
    assert " ".join(body["message"].split()) == f"Payment successful. Your Transaction ID is {expected_id}."

    row = db.execute("SELECT type_cd, cat_cd, source, description, amt, merchant_id, merchant_name, "
                     "merchant_city, merchant_zip, card_num, orig_ts = proc_ts FROM transaction "
                     "WHERE tran_id = %s", (expected_id,)).fetchone()
    assert row[:4] == ("02", 2, "POS TERM", "BILL PAYMENT - ONLINE")
    assert (f"{row[4]:.2f}", row[5], row[6], row[7], row[8], row[9], row[10]) == (
        bal, 999999999, "BILL PAYMENT", "N/A", "N/A", card, True)
    # COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
    assert db.execute("SELECT curr_bal FROM account WHERE acct_id = %s", (acct,)).fetchone()[0] == 0

    expect_error(user_api.post("/bill-payments", json={"acctId": f"{acct:011d}"}), 422,
                 "You have nothing to pay...", "COBIL00C")

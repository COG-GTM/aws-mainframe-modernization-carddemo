"""COACTVWC account view, COACTUPC account update (edits in 1200-EDIT-MAP-INPUTS order)."""

import copy

import pytest

from conftest import expect_error

pytestmark = pytest.mark.online

ACCOUNT_FIELDS = ["activeStatus", "currBal", "creditLimit", "cashCreditLimit", "openDate", "expirationDate",
                  "reissueDate", "currCycCredit", "currCycDebit", "groupId", "addrZip", "version"]
CUSTOMER_FIELDS = ["firstName", "middleName", "lastName", "addrLine1", "addrLine2", "addrLine3", "addrStateCd",
                   "addrCountryCd", "addrZip", "phoneNum1", "phoneNum2", "ssn", "govtIssuedId", "dob",
                   "eftAccountId", "priCardHolderInd", "version"]

# Every one of the 50 sample customers fails at least one COACTUPC edit (FICO < 300, ZIP not valid for the state,
# SSN area 9xx, non-NANPA area codes ...), exactly as on the mainframe. The happy path therefore corrects them.
VALID_CUSTOMER = {"ficoCreditScore": "750", "addrStateCd": "NY", "addrZip": "10001", "phoneNum1": "(212)555-0100",
                  "phoneNum2": "(212)555-0101", "ssn": "123456789", "dob": "1980-01-15"}


def update_body(view: dict) -> dict:
    body = {k: view[k] for k in ACCOUNT_FIELDS}
    body["customer"] = {k: view["customer"][k] for k in CUSTOMER_FIELDS}
    body["customer"]["ficoCreditScore"] = str(view["customer"]["ficoCreditScore"])
    return body


@pytest.fixture
def acct1(user_api) -> dict:
    return user_api.get("/accounts/00000000001").json()


def test_view_matches_account_customer_and_xref(user_api, db):
    view = user_api.get("/accounts/1").json()
    acct = db.execute("SELECT active_status, curr_bal, credit_limit, cash_credit_limit, open_date "
                      "FROM account WHERE acct_id = 1").fetchone()
    assert (view["activeStatus"], view["currBal"], view["creditLimit"], view["cashCreditLimit"],
            view["openDate"]) == (acct[0], f"{acct[1]:.2f}", f"{acct[2]:.2f}", f"{acct[3]:.2f}", str(acct[4]))
    cust_id, card = db.execute("SELECT cust_id, card_num FROM card_xref WHERE acct_id = 1").fetchone()
    assert view["customer"]["custId"] == cust_id
    first, last, ssn = db.execute("SELECT first_name, last_name, ssn FROM customer WHERE cust_id = %s",
                                  (cust_id,)).fetchone()
    assert (view["customer"]["firstName"], view["customer"]["lastName"], view["customer"]["ssn"]) == (
        first, last, ssn)
    assert card in [c["cardNum"] for c in view["cards"]]


@pytest.mark.parametrize("acct", ["abc", "0", "00000000000", "123456789012"])
def test_view_invalid_account_number(user_api, acct):
    expect_error(user_api.get(f"/accounts/{acct}"), 400, "Account number must be a non zero 11 digit number",
                 "COACTVWC")


def test_view_account_not_in_xref(user_api):
    # 9200-GETCARDXREF-BYACCT reads CXACAIX first
    expect_error(user_api.get("/accounts/99999999999"), 404,
                 "Did not find this account in account card xref file", "COACTVWC")


def test_update_no_change(user_api, acct1):
    expect_error(user_api.put("/accounts/1", json=update_body(acct1)), 422,
                 "No change detected with respect to values fetched.", "COACTUPC")


def test_update_sample_data_fails_fico_edit(user_api, acct1):
    # account 1 has FICO 274: 1275-EDIT-FICO-SCORE rejects it as soon as anything is changed
    body = update_body(acct1)
    body["customer"]["middleName"] = "Validation"
    expect_error(user_api.put("/accounts/1", json=body), 400, "FICO Score: should be between 300 and 850")


@pytest.mark.parametrize("path, value, message", [
    ("activeStatus", "X", "Account Status must be Y or N."),
    ("activeStatus", " ", "Account Status must be supplied."),
    ("openDate", "2020-13-01", "Open Date: Month must be a number between 1 and 12."),
    ("creditLimit", "abc", "Credit Limit is not valid"),
    ("creditLimit", "", "Credit Limit must be supplied."),
    ("customer.ssn", "900123456", "SSN: First 3 chars: should not be 000, 666, or between 900 and 999"),
    ("customer.dob", "2099-01-01", "Date of Birth:cannot be in the future"),
    ("customer.dob", "2999-01-01", "Date of Birth : Century is not valid."),
    ("customer.ficoCreditScore", "900", "FICO Score: should be between 300 and 850"),
    ("customer.firstName", "", "First Name must be supplied."),
    ("customer.firstName", "J0hn", "First Name can have alphabets only."),
    ("customer.addrStateCd", "XX", "State: is not a valid state code"),
    ("customer.addrZip", "ABCDE", "Zip must be all numeric."),
    ("customer.addrZip", "90210", "Invalid zip code for state"),
    ("customer.phoneNum1", "(000)555-0100", "Phone Number 1: Area code cannot be zero"),
    ("customer.phoneNum1", "(999)555-0100", "Phone Number 1: Not valid North America general purpose area code"),
    ("customer.eftAccountId", "12AB", "EFT Account Id must be all numeric."),
    ("customer.priCardHolderInd", "Q", "Primary Card Holder must be Y or N."),
])
def test_update_edit_messages(user_api, acct1, path, value, message):
    body = update_body(acct1)
    body["customer"].update(VALID_CUSTOMER)
    target = body
    *parents, leaf = path.split(".")
    for p in parents:
        target = target[p]
    target[leaf] = value
    expect_error(user_api.put("/accounts/1", json=body), 400, message, "COACTUPC")


def test_update_happy_path_writes_account_and_customer(user_api, acct1, db):
    body = update_body(acct1)
    body["customer"].update(VALID_CUSTOMER)
    body["creditLimit"] = "2500.00"
    body["customer"]["addrZip"] = "10001-1234"  # ZIP+4: the legacy edit only looks at the 5-character screen field
    resp = user_api.put("/accounts/1", json=body)
    assert resp.status_code == 200, resp.text
    out = resp.json()
    assert out["message"] == "Changes committed to database"
    assert out["creditLimit"] == "2500.00" and out["version"] == acct1["version"] + 1
    row = db.execute("SELECT a.credit_limit, c.fico_credit_score, c.addr_zip, c.phone_num_1 FROM account a "
                     "JOIN card_xref x ON x.acct_id = a.acct_id JOIN customer c ON c.cust_id = x.cust_id "
                     "WHERE a.acct_id = 1").fetchone()
    assert (f"{row[0]:.2f}", row[1], row[2], row[3]) == ("2500.00", 750, "10001-1234", "(212)555-0100")

    stale = copy.deepcopy(body)
    stale["creditLimit"] = "2600.00"
    expect_error(user_api.put("/accounts/1", json=stale), 409, "Record changed by some one else. Please review",
                 "COACTUPC")

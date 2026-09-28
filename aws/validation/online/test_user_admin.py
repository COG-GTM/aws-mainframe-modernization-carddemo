"""COUSR00C list, COUSR01C add, COUSR02C update, COUSR03C delete (admin only)."""

import bcrypt
import pytest

from conftest import expect_error, signon

pytestmark = pytest.mark.online

NEW_USER = {"userId": "valusr01", "firstName": "Val", "lastName": "Idation", "password": "secret1",
            "userType": "U"}


def test_list_requires_admin(user_api):
    expect_error(user_api.get("/users"), 403, "No access - Admin Only option...")


def test_list_paging(admin_api, db):
    ids = [r[0] for r in db.execute('SELECT user_id FROM user_security ORDER BY user_id COLLATE "C"')]
    first = admin_api.get("/users").json()
    assert [u["userId"] for u in first["items"]] == ids[:10]
    assert set(first["items"][0]) == {"userId", "firstName", "lastName", "userType"}
    if len(ids) > 10:
        second = admin_api.get("/users", params={"startKey": first["lastKey"], "direction": "next"}).json()
        assert [u["userId"] for u in second["items"]] == ids[10:20]
    else:
        assert not first["hasNext"]


@pytest.mark.parametrize("blank, message", [
    ("firstName", "First Name can NOT be empty..."),
    ("lastName", "Last Name can NOT be empty..."),
    ("userId", "User ID can NOT be empty..."),
    ("password", "Password can NOT be empty..."),
    ("userType", "User Type can NOT be empty..."),
])
def test_add_edits(admin_api, blank, message):
    expect_error(admin_api.post("/users", json={**NEW_USER, blank: ""}), 400, message, "COUSR01C")


def test_crud_lifecycle(admin_api, db):
    resp = admin_api.post("/users", json=NEW_USER)
    assert resp.status_code == 201, resp.text
    assert resp.json()["message"] == "User VALUSR01 has been added ..."
    fname, utype, pwd_hash = db.execute("SELECT first_name, user_type, password_hash FROM user_security "
                                        "WHERE user_id = 'VALUSR01'").fetchone()
    assert (fname, utype) == ("Val", "U")
    assert bcrypt.checkpw(b"SECRET1", pwd_hash.encode())  # SEC-USR-PWD compared upper-cased by COSGN00C
    assert signon("VALUSR01", "secret1").json()["nextRoute"] == "/menu"

    expect_error(admin_api.post("/users", json=NEW_USER), 409, "User ID already exist...", "COUSR01C")

    detail = admin_api.get("/users/valusr01").json()
    assert (detail["userId"], detail["lastName"]) == ("VALUSR01", "Idation")
    update = {"firstName": "Val", "lastName": "Idation", "password": "", "userType": "U",
              "version": detail["version"]}
    expect_error(admin_api.put("/users/VALUSR01", json=update), 422, "Please modify to update ...", "COUSR02C")
    expect_error(admin_api.put("/users/VALUSR01", json={**update, "firstName": ""}), 400,
                 "First Name can NOT be empty...", "COUSR02C")
    resp = admin_api.put("/users/VALUSR01", json={**update, "userType": "A", "lastName": "Admin"})
    assert resp.status_code == 200, resp.text
    assert resp.json()["message"] == "User VALUSR01 has been updated ..."
    assert db.execute("SELECT user_type, last_name FROM user_security WHERE user_id = 'VALUSR01'").fetchone() == (
        "A", "Admin")
    assert signon("VALUSR01", "SECRET1").json()["nextRoute"] == "/admin"
    expect_error(admin_api.put("/users/VALUSR01", json={**update, "lastName": "Stale"}), 409,
                 "Record changed by some one else. Please review", "COUSR02C")

    # contract: 204 (the COUSR03C confirmation text "User VALUSR01 has been deleted ..." is rendered by the UI)
    assert admin_api.delete("/users/VALUSR01").status_code == 204
    assert db.execute("SELECT count(*) FROM user_security WHERE user_id = 'VALUSR01'").fetchone()[0] == 0
    expect_error(admin_api.delete("/users/VALUSR01"), 404, "User ID NOT found...", "COUSR03C")
    expect_error(admin_api.get("/users/VALUSR01"), 404, "User ID NOT found...", "COUSR02C")

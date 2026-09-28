"""COSGN00C signon, COMEN01C / COADM01C menus."""

import pytest

from conftest import Api, expect_error, signon

pytestmark = pytest.mark.online


def test_blank_user_id():
    expect_error(signon("", "PASSWORD"), 400, "Please enter User ID ...", "COSGN00C")


def test_blank_password():
    expect_error(signon("USER0001", " "), 400, "Please enter Password ...", "COSGN00C")


def test_unknown_user():
    # READ USRSEC RESP=NOTFND(13)
    expect_error(signon("NOSUCHUS", "PASSWORD"), 401, "User not found. Try again ...", "COSGN00C")


def test_wrong_password():
    expect_error(signon("USER0001", "WRONGPWD"), 401, "Wrong Password. Try again ...", "COSGN00C")


def test_user_signon_routes_to_main_menu():
    # SEC-USR-TYPE 'U' -> XCTL COMEN01C; user id and password are upper-cased (FUNCTION UPPER-CASE) before the READ
    resp = signon("user0001", "password")
    assert resp.status_code == 200, resp.text
    body = resp.json()
    assert (body["userId"], body["role"], body["nextRoute"]) == ("USER0001", "USER", "/menu")
    assert body["tokenType"] == "Bearer" and body["token"]


def test_admin_signon_routes_to_admin_menu():
    # SEC-USR-TYPE 'A' -> XCTL COADM01C
    body = signon("ADMIN001", "PASSWORD").json()
    assert (body["userId"], body["role"], body["nextRoute"]) == ("ADMIN001", "ADMIN", "/admin")


def test_unauthenticated_request_rejected():
    assert Api().get("/menus/main").status_code == 401


def test_main_menu_options(user_api):
    # COMEN02Y: CDEMO-MENU-OPT-COUNT = 11, option 11 (COPAUS0C) only when the authorization sub-app is installed
    options = user_api.get("/menus/main").json()["options"]
    assert [o["number"] for o in options] == list(range(1, 12))
    assert [o["legacyProgram"] for o in options] == [
        "COACTVWC", "COACTUPC", "COCRDLIC", "COCRDSLC", "COCRDUPC", "COTRN00C", "COTRN01C", "COTRN02C",
        "CORPT00C", "COBIL00C", "COPAUS0C"]


def test_admin_menu_options(admin_api):
    # COADM02Y: CDEMO-ADMIN-OPT-COUNT = 6 (user list/add/update/delete + transaction type list/maintenance)
    options = admin_api.get("/menus/admin").json()["options"]
    assert [o["legacyProgram"] for o in options] == [
        "COUSR00C", "COUSR01C", "COUSR02C", "COUSR03C", "COTRTLIC", "COTRTUPC"]


def test_admin_menu_forbidden_for_user(user_api):
    # COMEN01C: option with CDEMO-MENU-OPT-USRTYPE 'A' selected by a 'U' user
    expect_error(user_api.get("/menus/admin"), 403, "No access - Admin Only option...")

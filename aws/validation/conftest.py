"""Shared fixtures for the CardDemo validation suite (see README.md)."""

from __future__ import annotations

import os
from collections.abc import Iterator
from pathlib import Path

import psycopg
import pytest
import requests

REPO_ROOT = Path(__file__).resolve().parents[2]
API_BASE = os.environ.get("VALIDATION_API_BASE", "http://localhost:18080").rstrip("/") + "/api/v1"
DB_DSN = os.environ.get("VALIDATION_DB_DSN", "postgresql://carddemo:carddemo@localhost:55433/carddemo")

ADMIN = ("ADMIN001", "PASSWORD")
USER = ("USER0001", "PASSWORD")

# Tables the online tests mutate; restored at session end so the suite can be re-run against the same stack.
MUTATED_TABLES = ["user_security", "account", "customer", "card", "transaction"]


class Api:
    def __init__(self, token: str | None = None) -> None:
        self.session = requests.Session()
        if token:
            self.session.headers["Authorization"] = f"Bearer {token}"

    def request(self, method: str, path: str, **kwargs) -> requests.Response:
        return self.session.request(method, API_BASE + path, timeout=30, **kwargs)

    def get(self, path: str, **kwargs) -> requests.Response:
        return self.request("GET", path, **kwargs)

    def post(self, path: str, **kwargs) -> requests.Response:
        return self.request("POST", path, **kwargs)

    def put(self, path: str, **kwargs) -> requests.Response:
        return self.request("PUT", path, **kwargs)

    def delete(self, path: str, **kwargs) -> requests.Response:
        return self.request("DELETE", path, **kwargs)


def signon(user_id: str, password: str) -> requests.Response:
    return Api().post("/auth/signon", json={"userId": user_id, "password": password})


def expect_error(resp: requests.Response, status: int, message: str, program: str | None = None) -> dict:
    """Asserts the contract error envelope and the legacy WS-MESSAGE text (trailing blanks ignored)."""
    assert resp.status_code == status, f"{resp.status_code} {resp.text}"
    body = resp.json()
    assert body["message"].rstrip() == message.rstrip(), body
    if program is not None:
        assert body["legacyProgram"] == program, body
    assert {"errorCode", "message", "fieldErrors", "legacyProgram", "timestamp"} <= body.keys()
    return body


@pytest.fixture(scope="session")
def db() -> Iterator[psycopg.Connection]:
    with psycopg.connect(DB_DSN, autocommit=True) as conn:
        conn.execute("SET search_path TO carddemo")
        yield conn


@pytest.fixture(scope="session", autouse=True)
def restore_online_state(request: pytest.FixtureRequest) -> Iterator[None]:
    online = any(item.get_closest_marker("online") for item in request.session.items)
    if not online:
        yield
        return
    with psycopg.connect(DB_DSN, autocommit=True) as conn:
        conn.execute("CREATE SCHEMA IF NOT EXISTS validation_snapshot")
        for t in MUTATED_TABLES:
            conn.execute(f"DROP TABLE IF EXISTS validation_snapshot.{t}")
            conn.execute(f"CREATE TABLE validation_snapshot.{t} AS TABLE carddemo.{t}")
    yield
    with psycopg.connect(DB_DSN) as conn:
        with conn.transaction():
            conn.execute("SET CONSTRAINTS ALL DEFERRED")
            for t in MUTATED_TABLES:
                conn.execute(f"DELETE FROM carddemo.{t}")
                conn.execute(f"INSERT INTO carddemo.{t} SELECT * FROM validation_snapshot.{t}")
        conn.execute("DROP SCHEMA validation_snapshot CASCADE")
        conn.commit()


@pytest.fixture(scope="session")
def admin_api() -> Api:
    resp = signon(*ADMIN)
    assert resp.status_code == 200, resp.text
    return Api(resp.json()["token"])


@pytest.fixture(scope="session")
def user_api() -> Api:
    resp = signon(*USER)
    assert resp.status_code == 200, resp.text
    return Api(resp.json()["token"])

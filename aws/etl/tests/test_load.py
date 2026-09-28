"""Integration: schema + COPY load into PostgreSQL 15. Set CARDDEMO_TEST_DSN to run, e.g.
CARDDEMO_TEST_DSN=postgresql://postgres:postgres@localhost:55432/carddemo"""

import os

import pytest

from etl import load
from etl.cli import DEFAULT_OUT, DEFAULT_SCHEMA

DSN = os.environ.get("CARDDEMO_TEST_DSN")
pytestmark = pytest.mark.skipif(not DSN, reason="CARDDEMO_TEST_DSN not set")

EXPECTED = {
    "user_security": 10, "transaction_type": 7, "transaction_category": 18, "disclosure_group": 51,
    "account": 50, "customer": 50, "card": 50, "card_xref": 50, "tran_cat_balance": 50,
    "transaction": 0, "daily_transaction": 300, "pending_auth_summary": 21, "pending_auth_detail": 202,
}


@pytest.fixture(scope="module")
def conn():
    psycopg = pytest.importorskip("psycopg")
    assert load.load(DEFAULT_OUT, DSN, schema_sql=DEFAULT_SCHEMA) == EXPECTED
    with psycopg.connect(DSN) as c:
        yield c


def test_schema_and_load_are_idempotent(conn):
    assert load.load(DEFAULT_OUT, DSN, schema_sql=DEFAULT_SCHEMA) == EXPECTED


def q(conn, sql):
    with conn.cursor() as cur:
        cur.execute(sql)
        return cur.fetchall()


def test_values_round_trip(conn):
    assert q(conn, "SELECT curr_bal::text, open_date::text FROM carddemo.account WHERE acct_id = 1") == [
        ("194.00", "2014-11-20")
    ]
    assert q(conn, "SELECT count(*) FROM carddemo.daily_transaction WHERE amt < 0")[0][0] > 0
    assert q(conn, "SELECT account_status FROM carddemo.pending_auth_summary WHERE acct_id = 1") == [
        ([None, None, None, None, "00"],)
    ]
    assert q(conn, "SELECT trc_type_category FROM carddemo.db2_transaction_type_category "
                   "WHERE trc_type_code = '01' ORDER BY 1 LIMIT 1") == [("0001",)]
    assert q(conn, "SELECT count(*) FROM carddemo.user_security WHERE password_hash LIKE '$2b$10$%'") == [(10,)]


def test_indexes_present(conn):
    names = {r[0] for r in q(conn, "SELECT indexname FROM pg_indexes WHERE schemaname = 'carddemo'")}
    assert {"ix_card_acct_id", "ix_card_xref_acct_id", "ix_transaction_proc_ts", "ix_transaction_card_num",
            "ix_pending_auth_detail_card", "ix_authfrds_card_ts"} <= names


def test_constraints_enforced(conn):
    import psycopg

    with pytest.raises(psycopg.errors.CheckViolation):
        with conn.transaction():
            q(conn, "UPDATE carddemo.account SET active_status = 'X' WHERE acct_id = 1")
    with pytest.raises(psycopg.errors.ForeignKeyViolation):
        with conn.transaction():
            q(conn, "INSERT INTO carddemo.card VALUES ('1234567890123456', 99999, 1, 'X', NULL, 'Y', 0)")


def test_partial_truncate_reload_is_refused_before_changes(conn, tmp_path):
    (tmp_path / "account.csv").write_text((DEFAULT_OUT / "account.csv").read_text(encoding="utf-8"), encoding="utf-8")
    with pytest.raises(ValueError, match="card -> account"):
        load.load(tmp_path, DSN)
    assert q(conn, "SELECT count(*) FROM carddemo.card") == [(50,)]


def test_processed_message_matches_messaging_contract(conn):
    cols = q(conn, "SELECT column_name FROM information_schema.columns "
                   "WHERE table_schema = 'carddemo' AND table_name = 'processed_message' ORDER BY ordinal_position")
    assert [c for (c,) in cols] == ["message_id", "queue", "reply_payload", "processed_at"]

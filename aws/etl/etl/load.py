"""Load the generated CSVs into the carddemo schema with PostgreSQL COPY (one transaction)."""

from __future__ import annotations

import os
from pathlib import Path

import psycopg
from psycopg.conninfo import make_conninfo
from psycopg import sql

from etl.layouts import LAYOUTS

SCHEMA = "carddemo"

# Parents before children so the load also works with immediate FK checking.
LOAD_ORDER = (
    "user_security",
    "transaction_type",
    "transaction_category",
    "disclosure_group",
    "account",
    "customer",
    "card",
    "card_xref",
    "tran_cat_balance",
    "transaction",
    "daily_transaction",
    "pending_auth_summary",
    "pending_auth_detail",
)


def table_csvs(csv_dir: Path) -> list[tuple[str, Path]]:
    tables = {out.table for layout in LAYOUTS.values() for out in layout.outputs if out.table}
    assert tables <= set(LOAD_ORDER), tables - set(LOAD_ORDER)
    return [(tbl, csv_dir / f"{tbl}.csv") for tbl in LOAD_ORDER if (csv_dir / f"{tbl}.csv").exists()]


def ddl_files(schema_sql: Path) -> list[Path]:
    """schema.sql followed by the optional sub-app DDL next to it (db2/*.sql, ims/*.sql)."""
    root = schema_sql.parent
    return [schema_sql, *sorted((root / "db2").glob("*.sql")), *sorted((root / "ims").glob("*.sql"))]


def dsn_from_env(env: dict[str, str] | None = None) -> str | None:
    """libpq keyword DSN from the DB_* variables of conventions.md section 3, or None if DB_HOST is unset."""
    env = os.environ if env is None else env
    if not env.get("DB_HOST"):
        return None
    parts = {
        "host": env["DB_HOST"],
        "port": env.get("DB_PORT", "5432"),
        "dbname": env.get("DB_NAME", "carddemo"),
        "user": env.get("DB_USER"),
        "password": env.get("DB_PASSWORD"),
    }
    return make_conninfo(**{k: v for k, v in parts.items() if v})


def _check_truncate_closure(cur: psycopg.Cursor, tables: list[str]) -> None:
    """TRUNCATE needs every referencing table in the same statement; refuse partial sets up front."""
    cur.execute(
        """SELECT DISTINCT child.relname, parent.relname
             FROM pg_constraint c
             JOIN pg_class child ON child.oid = c.conrelid
             JOIN pg_class parent ON parent.oid = c.confrelid
             JOIN pg_namespace n ON n.oid = parent.relnamespace
            WHERE c.contype = 'f' AND n.nspname = %s AND parent.relname = ANY(%s) AND NOT child.relname = ANY(%s)
            ORDER BY 1, 2""",
        (SCHEMA, tables, tables),
    )
    missing = cur.fetchall()
    if missing:
        refs = ", ".join(f"{child} -> {parent}" for child, parent in missing)
        raise ValueError(f"cannot truncate+reload a partial table set; add CSVs for the referencing tables "
                         f"({refs}) or use --no-truncate")


def load(csv_dir: Path, dsn: str, schema_sql: Path | None = None, truncate: bool = True) -> dict[str, int]:
    files = table_csvs(csv_dir)
    counts: dict[str, int] = {}
    with psycopg.connect(dsn) as conn:
        with conn.cursor() as cur:
            if schema_sql is not None:
                for ddl in ddl_files(schema_sql):
                    cur.execute(ddl.read_text(encoding="utf-8"))
            if truncate:
                _check_truncate_closure(cur, [tbl for tbl, _ in files])
                cur.execute(
                    sql.SQL("TRUNCATE {}").format(
                        sql.SQL(", ").join(sql.Identifier(SCHEMA, tbl) for tbl, _ in files)
                    )
                )
            for tbl, path in files:
                with path.open(encoding="utf-8") as fh:
                    header = fh.readline().strip().split(",")
                    stmt = sql.SQL("COPY {} ({}) FROM STDIN WITH (FORMAT csv)").format(
                        sql.Identifier(SCHEMA, tbl), sql.SQL(", ").join(map(sql.Identifier, header))
                    )
                    with cur.copy(stmt) as copy:
                        while chunk := fh.read(65536):
                            copy.write(chunk)
            cur.execute("SET CONSTRAINTS ALL IMMEDIATE")
            for tbl, _ in files:
                cur.execute(sql.SQL("SELECT count(*) FROM {}").format(sql.Identifier(SCHEMA, tbl)))
                counts[tbl] = cur.fetchone()[0]
    return counts

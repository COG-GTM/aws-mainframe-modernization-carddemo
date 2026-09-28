"""Field -> column value conversions (rules from aws/contracts/data-model.md §1 and §5)."""

from __future__ import annotations

import datetime as dt
import re
from decimal import Decimal

import bcrypt

_PAD = " \x00"
_DB2_TS = re.compile(r"^(\d{4}-\d{2}-\d{2})-(\d{2})\.(\d{2})\.(\d{2})\.(\d{1,6})$")
_ISO_TS = re.compile(r"^(\d{4}-\d{2}-\d{2})[ T](\d{2}):(\d{2}):(\d{2})\.(\d{1,6})$")

BCRYPT_ROUNDS = 10  # Spring Security BCryptPasswordEncoder default strength


def text(v: str | None) -> str | None:
    """VARCHAR / CHAR: trailing spaces (and low-values) trimmed; blank -> NULL."""
    if v is None:
        return None
    s = v.rstrip(_PAD)
    return s or None


def text_nn(v: str) -> str:
    """NOT NULL text: trailing spaces trimmed, blank stays ''."""
    return v.rstrip(_PAD)


def upper_text(v: str) -> str:
    return v.strip(_PAD).upper()


def num(v: int | Decimal | None) -> int | Decimal | None:
    return v


def date(v: str) -> str | None:
    """PIC X(10) YYYY-MM-DD -> DATE; spaces/zeros/low-values -> NULL."""
    s = v.strip(_PAD)
    if not s or set(s) <= {"0", "-"}:
        return None
    return dt.date.fromisoformat(s).isoformat()


def timestamp(v: str) -> str | None:
    """PIC X(26) timestamp in DB2 (YYYY-MM-DD-HH.MM.SS.NNNNNN) or ISO-with-space form -> TIMESTAMP(6)."""
    s = v.strip(_PAD)
    if not s or set(s) <= {"0", "-", ".", ":", " "}:
        return None
    m = _DB2_TS.match(s) or _ISO_TS.match(s)
    if not m:
        raise ValueError(f"unparseable timestamp {v!r}")
    day, hh, mm, ss, frac = m.groups()
    ts = dt.datetime.fromisoformat(f"{day} {hh}:{mm}:{ss}.{frac.ljust(6, '0')}")
    return ts.isoformat(sep=" ", timespec="microseconds")


def zero_pad9(v: int) -> str:
    return f"{v:09d}"


def bcrypt_upper(v: str) -> str:
    """Legacy SEC-USR-PWD is plain text; COSGN00C upper-cases input, so hash the upper-cased value."""
    pwd = v.strip(_PAD).upper().encode("utf-8")
    return bcrypt.hashpw(pwd, bcrypt.gensalt(rounds=BCRYPT_ROUNDS)).decode("ascii")


def pg_array(values: list[str | None]) -> str:
    """PostgreSQL array literal (text form, usable in COPY CSV)."""
    parts = []
    for v in values:
        t = text(v)
        parts.append("NULL" if t is None else '"' + t.replace("\\", "\\\\").replace('"', '\\"') + '"')
    return "{" + ",".join(parts) + "}"

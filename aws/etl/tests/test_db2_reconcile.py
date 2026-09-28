"""DB2 TRANSACTION_TYPE / TRANSACTION_TYPE_CATEGORY seed vs VSAM TRANTYPE / TRANCATG."""

import re

from etl import convert
from etl.layouts import LAYOUTS, REPO_ROOT

CTL = REPO_ROOT / "app" / "app-transaction-type-db2" / "ctl"


def db2_rows(name: str) -> list[tuple[str, ...]]:
    return re.findall(r"SELECT\s+((?:'[^']*'\s*,\s*)*'[^']*')\s+FROM", (CTL / name).read_text())


def parse(row: str) -> tuple[str, ...]:
    return tuple(re.findall(r"'([^']*)'", row))


def test_type_keys_match():
    db2 = {k: d for k, d in map(parse, db2_rows("DB2LTTYP.ctl"))}
    vsam = {r[0]: r[1] for r in convert.decode(LAYOUTS["trantype"])["transaction_type"]}
    assert set(db2) == set(vsam)
    diffs = {k for k in vsam if vsam[k].upper() != db2[k]}
    assert diffs == {"06"}  # DB2 seed spells 'REVERAL'


def test_category_keys_match():
    db2 = {(t, int(c)): d for t, c, d in map(parse, db2_rows("DB2LTCAT.ctl"))}
    vsam = {(r[0], r[1]): r[2] for r in convert.decode(LAYOUTS["trancatg"])["transaction_category"]}
    assert set(db2) == set(vsam)
    diffs = {k for k in vsam if vsam[k].upper() != db2[k]}
    assert diffs == {("06", 2)}  # 'Non-fraud reversal' vs 'NON FRAUD REVERSAL'

"""The committed CSVs in aws/etl/output/ are exactly what the converter produces."""

import bcrypt

from etl import convert
from etl.cli import DEFAULT_OUT
from etl.layouts import LAYOUTS


def test_committed_csvs_are_current(tmp_path):
    counts = convert.convert_all(tmp_path)
    for layout in LAYOUTS.values():
        for out in layout.outputs:
            fresh = convert.output_path(tmp_path, out)
            committed = convert.output_path(DEFAULT_OUT, out)
            assert committed.exists(), committed
            if out.name == "user_security":
                a, b = convert.read_csv(fresh), convert.read_csv(committed)
                assert [{k: v for k, v in r.items() if k != "password_hash"} for r in a] == [
                    {k: v for k, v in r.items() if k != "password_hash"} for r in b
                ]
                assert all(bcrypt.checkpw(b"PASSWORD", r["password_hash"].encode()) for r in b)
            else:
                assert fresh.read_text() == committed.read_text(), out.name
    assert counts["account"] == 50


def test_rerun_keeps_existing_password_hashes(tmp_path):
    convert.convert_all(tmp_path)
    first = (tmp_path / "user_security.csv").read_text()
    convert.convert_all(tmp_path)
    assert (tmp_path / "user_security.csv").read_text() == first


def test_csv_null_vs_empty_encoding():
    assert convert.format_value(None) == ""
    assert convert.format_value("") == '""'
    assert convert.format_value("a,b") == '"a,b"'
    assert convert.format_value('say "hi"') == '"say ""hi"""'

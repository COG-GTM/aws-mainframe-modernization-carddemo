"""EBCDIC-decoded rows must equal rows decoded from the ASCII copies in app/data/ASCII/."""

from decimal import Decimal

import pytest

from etl import codec, convert
from etl.layouts import LAYOUTS

WITH_ASCII = [name for name, lay in LAYOUTS.items() if lay.ascii is not None]

# Genuine content differences between the two sample sets (verified byte-for-byte; not decoder issues).
# The EBCDIC files are the load source (contract data-model.md section 5).
KNOWN_DIFFERENCES = {
    "account": {"account row 49 addr_zip: 'ZEROAPR' != 'A000000000'"},
    "discgrp": {"disclosure_group row 34 int_rate: Decimal('15.00') != Decimal('0.00')"},
}


def test_every_ascii_file_has_a_layout():
    assert {LAYOUTS[n].ascii.name for n in WITH_ASCII} == {
        "acctdata.txt", "carddata.txt", "cardxref.txt", "custdata.txt", "dailytran.txt",
        "discgrp.txt", "tcatbal.txt", "trancatg.txt", "trantype.txt",
    }


@pytest.mark.parametrize("name", WITH_ASCII)
def test_ebcdic_matches_ascii(name):
    n, diffs = convert.crosscheck(LAYOUTS[name])
    assert n > 0
    assert set(diffs) == KNOWN_DIFFERENCES.get(name, set())


def test_ascii_reader_strips_crlf_and_pads(tmp_path):
    p = tmp_path / "x.txt"
    p.write_bytes(b"01Purchase\r\n02Payment\n\n")
    recs = convert.read_ascii(p, 60)
    assert len(recs) == 2 and all(len(r) == 60 for r in recs)
    assert recs[0].decode("cp037").rstrip() == "01Purchase"


def test_ascii_reader_rejects_long_lines(tmp_path):
    p = tmp_path / "x.txt"
    p.write_text("0" * 61 + "\n")
    with pytest.raises(codec.DecodeError):
        convert.read_ascii(p, 60)


def test_ascii_overpunch_amount_matches_ebcdic():
    lay = LAYOUTS["dalytran"]
    asc = convert.decode(lay, records=convert.read_ascii(lay.ascii, 350))["daily_transaction"]
    assert asc[1][7] == Decimal("-919.00")  # dailytran.txt line 2 ends the amount with '}'

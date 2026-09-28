import pytest

from etl.copybook import Copybook
from etl.layouts import CPY, PAUTH_CPY

LENGTHS = {
    "CSUSR01Y": 80, "CVACT01Y": 300, "CVACT02Y": 150, "CVACT03Y": 50, "CVCUS01Y": 500,
    "CVTRA01Y": 50, "CVTRA02Y": 50, "CVTRA03Y": 60, "CVTRA04Y": 60, "CVTRA05Y": 350,
    "CVTRA06Y": 350, "CVEXPORT": 500,
}


@pytest.mark.parametrize("name,length", LENGTHS.items())
def test_core_record_lengths(name, length):
    assert Copybook(CPY / f"{name}.cpy").record_length == length


def test_ims_segment_lengths():
    assert Copybook(PAUTH_CPY / "CIPAUSMY.cpy").record_length == 100
    assert Copybook(PAUTH_CPY / "CIPAUDTY.cpy").record_length == 200


def test_offsets_match_vsam_keys():
    assert Copybook(CPY / "CVACT02Y.cpy").leaf("CARD-ACCT-ID").offset == 16  # CARDAIX key @16
    assert Copybook(CPY / "CVACT03Y.cpy").leaf("XREF-ACCT-ID").offset == 25  # CXACAIX key @25
    assert Copybook(CPY / "CVTRA05Y.cpy").leaf("TRAN-PROC-TS").offset == 304  # TRANSACT AIX @304


def test_export_redefines_share_offset_and_comp_sizes():
    cb = Copybook(CPY / "CVEXPORT.cpy")
    seq = cb.leaf("EXPORT-SEQUENCE-NUM")
    assert (seq.offset, seq.length, seq.usage) == (27, 4, "COMP")
    firsts = ["EXP-CUST-ID", "EXP-ACCT-ID", "EXP-TRAN-ID", "EXP-XREF-CARD-NUM", "EXP-CARD-NUM"]
    assert {cb.leaf(n).offset for n in firsts} == {40}
    assert cb.leaf("EXP-ACCT-CURR-BAL").length == 7  # S9(10)V99 COMP-3
    assert cb.leaf("EXP-CUST-FICO-CREDIT-SCORE").length == 2  # 9(03) COMP-3
    assert cb.leaf("EXP-CARD-CVV-CD").usage == "COMP"


def test_occurs_expansion():
    cb = Copybook(CPY / "CVEXPORT.cpy")
    lines = [cb.leaf(f"EXP-CUST-ADDR-LINE({i})") for i in (1, 2, 3)]
    assert [x.offset for x in lines] == [119, 169, 219]
    assert all(x.length == 50 for x in lines)
    summ = Copybook(PAUTH_CPY / "CIPAUSMY.cpy")
    status = [summ.leaf(f"PA-ACCOUNT-STATUS({i})") for i in range(1, 6)]
    assert [s.offset for s in status] == [16, 18, 20, 22, 24]

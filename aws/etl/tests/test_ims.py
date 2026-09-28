"""IMS DBPAUTP0 unload: segment framing and parent/child consistency."""

from etl import convert, ims
from etl.layouts import LAYOUTS

LAY = LAYOUTS["dbpautp0"]


def segments():
    return ims.read_segments(LAY.input.read_bytes())


def test_segment_framing():
    segs = segments()
    assert len(LAY.input.read_bytes()) == 51736
    assert sum(s.name == "PAUTSUM0" for s in segs) == 22  # incl. one all-spaces root at the end
    assert sum(s.name == "PAUTDTL1" for s in segs) == 202
    assert all(len(s.data) == (100 if s.name == "PAUTSUM0" else 200) for s in segs)
    assert segs[0].name == "PAUTSUM0" and segs[-1].data == b"\x40" * 6 + segs[-1].data[6:]


def test_children_follow_their_root():
    out = convert.decode(LAY)
    summary = {r[0]: r for r in out["pending_auth_summary"]}
    assert len(summary) == 21
    assert {r[0] for r in out["pending_auth_detail"]} <= set(summary)
    # the summary's approved count equals the number of its child segments in the sample
    counts = {}
    for r in out["pending_auth_detail"]:
        counts[r[0]] = counts.get(r[0], 0) + 1
    assert all(summary[a][8] == counts.get(a, 0) for a in summary)


def test_keys_agree_with_core_data():
    out = convert.decode(LAY)
    xref = convert.decode(LAYOUTS["cardxref"])["card_xref"]
    card_acct = {r[0]: r[2] for r in xref}
    acct_cust = {(r[2], r[1]) for r in xref}
    assert all((r[0], r[1]) in acct_cust for r in out["pending_auth_summary"])
    assert all(card_acct[r[5]] == r[0] for r in out["pending_auth_detail"])


def test_detail_values():
    d = sorted(convert.decode(LAY)["pending_auth_detail"], key=lambda r: (r[0], r[1], r[2]))[0]
    assert d[:6] == [1, 76699, 998747444, "231027", "041252", "9680294154603697"]
    assert str(d[14]) == "1.24" and d[20] == "Amazon.com" and d[25] == "P"
    # 9's complement keys: 99999 - YYDDD, where 231027 is 2023-10-27 = day 300
    assert 99999 - d[1] == 23300


def test_summary_values():
    s = {r[0]: r for r in convert.decode(LAY)["pending_auth_summary"]}[1]
    assert s[1] == 1 and s[2] is None
    assert s[3] == '{NULL,NULL,NULL,NULL,"00"}'
    assert str(s[4]) == "2022.00" and s[8] == 6

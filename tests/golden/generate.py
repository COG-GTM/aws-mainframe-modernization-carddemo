#!/usr/bin/env python3
"""Golden input generator for the CBTRN02C daily posting cycle.

Writes one complete, self-consistent input set (DALYTRAN + XREFFILE +
ACCTFILE + TCATBALF as record-sequential fixtures in exact copybook layout)
plus a manifest.json that records the seed, the case list, record counts and
the generator's *prediction* of accept/reject per record.

The prediction is a model of CBTRN02C as read from the source (citations in
``predict_outcome``).  It is NOT the golden truth: run_reference.sh runs the
real program and the program wins.  Disagreements are logged by the runner
into docs/validation/golden-set/findings.md.

Every value is synthetic and labelled as such:
  * card numbers start with the documented test prefix 0000 and fail Luhn
  * account ids start with 9000, customer ids with 9000, merchant ids with 9000
  * names/descriptions are "GOLDEN ..." / "SYNTHETIC ..."

Standard library only.

Usage:
  generate.py --set named  --out tests/golden/sets/named/input   [--seed N]
  generate.py --set volume --out tests/golden/sets/volume/input  [--seed N] [--volume 1200]
  generate.py --list-cases
"""

from __future__ import annotations

import argparse
import json
import os
import random
import sys
from dataclasses import dataclass, field
from decimal import Decimal
from typing import Dict, List, Optional, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import layouts  # noqa: E402
from layouts import ACCT, DALYTRAN, TCATBAL, XREF, build_record, encode_zoned  # noqa: E402

# ---------------------------------------------------------------------------
# Fixed synthetic constants (all documented in docs/validation/golden-set/README.md)
# ---------------------------------------------------------------------------
POSTING_DATE = "2026-03-15"                 # date of every DALYTRAN-ORIG-TS
# COB_CURRENT_DATE for the reference run.  The ".00" matters: without an explicit
# fraction GnuCOBOL 3.1.2 freezes only the seconds and keeps the wall-clock
# hundredths, which leak into TRAN-PROC-TS and make two runs differ.
FROZEN_CLOCK = "2026-03-15 12:00:00.00"
# What CBTRN02C writes to TRAN-PROC-TS from that clock
# (Z-GET-DB2-FORMAT-TIMESTAMP, app/cbl/CBTRN02C.cbl:692-705): YYYY-MM-DD-HH.MM.SS.mm0000
FROZEN_PROC_TS = "2026-03-15-12.00.00.000000"
FUTURE_EXPIRY = "2030-12-31"
PAST_EXPIRY = "2025-12-31"
CARD_TEST_PREFIX = "0000"
ACCT_PREFIX = 9000
CUST_PREFIX = 9000
MERCHANT_PREFIX = 9000
DEFAULT_TYPE = "01"
DEFAULT_CAT = 1

AMT_MAX = Decimal("999999999.99")           # largest PIC S9(09)V99 (CVTRA06Y.cpy:10)
LIMIT_MAX = Decimal("9999999999.99")        # largest PIC S9(10)V99 (CVACT01Y.cpy:8)


def luhn_ok(digits: str) -> bool:
    total = 0
    for i, ch in enumerate(reversed(digits)):
        d = int(ch)
        if i % 2 == 1:
            d *= 2
            if d > 9:
                d -= 9
        total += d
    return total % 10 == 0


def synthetic_card(n: int) -> str:
    """16-digit card number with test prefix 0000 that always FAILS Luhn.

    The 16th digit is the correct Luhn check digit for the first 15 shifted by 5,
    so the number is never valid and distinct n never collide."""
    body = CARD_TEST_PREFIX + ("%011d" % n)
    for d in range(10):
        if luhn_ok(body + str(d)):
            card = body + str((d + 5) % 10)
            break
    else:  # pragma: no cover - a Luhn check digit always exists
        raise AssertionError("no Luhn check digit for %s" % body)
    assert not luhn_ok(card)
    return card


def synthetic_acct(n: int) -> str:
    return "%04d%07d" % (ACCT_PREFIX, n)          # PIC 9(11)


def synthetic_cust(n: int) -> str:
    return "%04d%05d" % (CUST_PREFIX, n)          # PIC 9(09)


def synthetic_merchant(n: int) -> str:
    return "%04d%05d" % (MERCHANT_PREFIX, n)      # PIC 9(09)


def orig_ts(seq: int) -> str:
    """DALYTRAN-ORIG-TS X(26) as 'YYYY-MM-DD HH:MM:SS.ffffff' (same shape as
    app/data/ASCII/dailytran.txt).  CBTRN02C only looks at (1:10)
    (app/cbl/CBTRN02C.cbl:414)."""
    h, rem = divmod(seq, 3600)
    m, s = divmod(rem, 60)
    return "%s %02d:%02d:%02d.000000" % (POSTING_DATE, h % 24, m, s)


# ---------------------------------------------------------------------------
# World model
# ---------------------------------------------------------------------------

@dataclass
class Account:
    acct_id: str
    credit_limit: Decimal
    curr_bal: Decimal = Decimal("0.00")
    cyc_credit: Decimal = Decimal("0.00")
    cyc_debit: Decimal = Decimal("0.00")
    expiry: str = FUTURE_EXPIRY
    cash_limit: Decimal = Decimal("0.00")
    active: str = "Y"
    open_date: str = "2020-01-01"
    reissue_date: str = FUTURE_EXPIRY
    zip: str = "00000"
    group: str = "GOLDGRP01"

    def record(self) -> bytes:
        return build_record(ACCT, {
            "ACCT-ID": self.acct_id,
            "ACCT-ACTIVE-STATUS": self.active,
            "ACCT-CURR-BAL": self.curr_bal,
            "ACCT-CREDIT-LIMIT": self.credit_limit,
            "ACCT-CASH-CREDIT-LIMIT": self.cash_limit,
            "ACCT-OPEN-DATE": self.open_date,
            "ACCT-EXPIRAION-DATE": self.expiry,
            "ACCT-REISSUE-DATE": self.reissue_date,
            "ACCT-CURR-CYC-CREDIT": self.cyc_credit,
            "ACCT-CURR-CYC-DEBIT": self.cyc_debit,
            "ACCT-ADDR-ZIP": self.zip,
            "ACCT-GROUP-ID": self.group,
        })


@dataclass
class Xref:
    card: str
    cust_id: str
    acct_id: str

    def record(self) -> bytes:
        return build_record(XREF, {
            "XREF-CARD-NUM": self.card,
            "XREF-CUST-ID": self.cust_id,
            "XREF-ACCT-ID": self.acct_id,
        })


@dataclass
class Tran:
    tran_id: str
    case: str
    card: str
    amt: Decimal
    type_cd: str = DEFAULT_TYPE
    cat_cd: int = DEFAULT_CAT
    seq: int = 0
    note: str = ""
    merchant_n: int = 1

    def record(self) -> bytes:
        return build_record(DALYTRAN, {
            "DALYTRAN-ID": self.tran_id,
            "DALYTRAN-TYPE-CD": self.type_cd,
            "DALYTRAN-CAT-CD": self.cat_cd,
            "DALYTRAN-SOURCE": "GOLDENSET",
            "DALYTRAN-DESC": ("GOLDEN CASE %s" % self.case)[:100],
            "DALYTRAN-AMT": self.amt,
            "DALYTRAN-MERCHANT-ID": synthetic_merchant(self.merchant_n),
            "DALYTRAN-MERCHANT-NAME": "GOLDEN MERCHANT %04d" % self.merchant_n,
            "DALYTRAN-MERCHANT-CITY": "SYNTHETIC CITY",
            "DALYTRAN-MERCHANT-ZIP": "00000",
            "DALYTRAN-CARD-NUM": self.card,
            "DALYTRAN-ORIG-TS": orig_ts(self.seq),
            "DALYTRAN-PROC-TS": "",
        })


@dataclass
class World:
    seed: int
    accounts: Dict[str, Account] = field(default_factory=dict)
    xrefs: Dict[str, Xref] = field(default_factory=dict)
    tcatbal: Dict[Tuple[str, str, int], Decimal] = field(default_factory=dict)
    trans: List[Tran] = field(default_factory=list)
    _n_acct: int = 0
    _n_card: int = 0
    _n_tran: int = 0

    def new_account(self, limit, **kw) -> Account:
        self._n_acct += 1
        a = Account(synthetic_acct(self._n_acct), Decimal(limit), **kw)
        self.accounts[a.acct_id] = a
        return a

    def new_card(self, acct_id: Optional[str]) -> str:
        """Card with an XREF entry pointing at acct_id (which may not exist)."""
        self._n_card += 1
        card = synthetic_card(self._n_card)
        if card in self.xrefs or any(t.card == card for t in self.trans):
            raise AssertionError("synthetic card collision: %s" % card)
        if acct_id is not None:
            self.xrefs[card] = Xref(card, synthetic_cust(self._n_card), acct_id)
        return card

    def seed_tcat(self, acct_id: str, type_cd: str, cat_cd: int, bal) -> None:
        self.tcatbal[(acct_id, type_cd, cat_cd)] = Decimal(bal).quantize(Decimal("0.01"))

    def add_tran(self, case: str, card: str, amt, **kw) -> Tran:
        self._n_tran += 1
        tid = "GS%04d%010d" % (self.seed % 10000, self._n_tran)   # X(16), unique per set
        t = Tran(tid, case, card, Decimal(amt), seq=self._n_tran, **kw)
        self.trans.append(t)
        return t


# ---------------------------------------------------------------------------
# Named cases.  Each builds the entities it needs and adds its DALYTRAN records.
# ---------------------------------------------------------------------------

CASES: Dict[str, Tuple[str, "callable"]] = {}


def case(name: str, doc: str):
    def deco(fn):
        CASES[name] = (doc, fn)
        return fn
    return deco


@case("clean_accept", "Card and account exist, within limit, not expired, TCATBAL row exists.")
def _c01(w: World):
    a = w.new_account("5000.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "0.00")
    w.add_tran("clean_accept", c, "123.45")


@case("card_not_in_xref", "DALYTRAN-CARD-NUM has no XREFFILE record -> reason 100 (CBTRN02C.cbl:383-387).")
def _c02(w: World):
    c = w.new_card(None)
    w.add_tran("card_not_in_xref", c, "10.00")


@case("account_not_on_file", "XREF exists but XREF-ACCT-ID has no ACCTFILE record -> reason 101 (CBTRN02C.cbl:394-399).")
def _c03(w: World):
    w._n_acct += 1
    ghost = synthetic_acct(w._n_acct)           # allocated but never written to ACCTFILE
    c = w.new_card(ghost)
    w.add_tran("account_not_on_file", c, "10.00")


@case("overlimit", "CYC-CREDIT - CYC-DEBIT + AMT > CREDIT-LIMIT -> reason 102 (CBTRN02C.cbl:403-413).")
def _c04(w: World):
    a = w.new_account("100.00", cyc_credit=Decimal("50.00"))
    c = w.new_card(a.acct_id)
    w.add_tran("overlimit", c, "60.00")


@case("expired_account", "ACCT-EXPIRAION-DATE < ORIG-TS(1:10) -> reason 103 (CBTRN02C.cbl:414-420).")
def _c05(w: World):
    a = w.new_account("5000.00", expiry=PAST_EXPIRY)
    c = w.new_card(a.acct_id)
    w.add_tran("expired_account", c, "25.00")


@case("zero_amount", "AMT = 0 posts; 0 >= 0 so it is added to CYC-CREDIT (CBTRN02C.cbl:548-552).")
def _c06(w: World):
    a = w.new_account("5000.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "10.00")
    w.add_tran("zero_amount", c, "0.00")


@case("negative_amount_credit", "AMT < 0 posts and is ADDed to CYC-DEBIT, making it negative (CBTRN02C.cbl:551).")
def _c07(w: World):
    a = w.new_account("5000.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "0.00")
    w.add_tran("negative_amount_credit", c, "-250.00")


@case("max_amount", "AMT = +999999999.99, the largest PIC S9(09)V99 (CVTRA06Y.cpy:10); limit large enough.")
def _c08(w: World):
    a = w.new_account(LIMIT_MAX)
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "0.00")
    w.add_tran("max_amount", c, AMT_MAX)


@case("max_negative_amount", "AMT = -999999999.99 posts; added to CYC-DEBIT and to a TCATBAL row.")
def _c08b(w: World):
    a = w.new_account("0.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "0.00")
    w.add_tran("max_negative_amount", c, -AMT_MAX)


@case("category_balance_crosses_zero", "TCATBAL -100.00 + AMT 250.00 -> +150.00; sign of TRAN-CAT-BAL flips (CBTRN02C.cbl:527).")
def _c09(w: World):
    a = w.new_account("5000.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "-100.00")
    w.add_tran("category_balance_crosses_zero", c, "250.00")


@case("same_account_running_balance", "Two records on one account: 600 posts, then 500 is over the 1000 limit only because of the first (CBTRN02C.cbl:403-407, :554).")
def _c10(w: World):
    a = w.new_account("1000.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "0.00")
    w.add_tran("same_account_running_balance", c, "600.00", note="first")
    w.add_tran("same_account_running_balance", c, "500.00", note="second; over limit after first posts")


@case("same_account_two_accepts", "Two records on one account, both post; balances accumulate in ACCTFILE and TCATBALF.")
def _c11(w: World):
    a = w.new_account("5000.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "0.00")
    w.add_tran("same_account_two_accepts", c, "100.00", note="first")
    w.add_tran("same_account_two_accepts", c, "200.50", note="second")


@case("category_not_in_tcatbal", "No TCATBALF row for (acct,type,cat): program creates one and posts (CBTRN02C.cbl:474-479, :503-510).")
def _c12(w: World):
    a = w.new_account("5000.00")
    c = w.new_card(a.acct_id)
    w.add_tran("category_not_in_tcatbal", c, "42.00", cat_cd=2)


@case("overlimit_and_expired", "Both 102 and 103 conditions hold; 103 is assigned last and wins (CBTRN02C.cbl:410, :417).")
def _c13(w: World):
    a = w.new_account("100.00", expiry=PAST_EXPIRY)
    c = w.new_card(a.acct_id)
    w.add_tran("overlimit_and_expired", c, "500.00")


@case("expiry_boundary_equal", "ACCT-EXPIRAION-DATE equal to ORIG-TS(1:10): '>=' accepts (CBTRN02C.cbl:414).")
def _c14(w: World):
    a = w.new_account("5000.00", expiry=POSTING_DATE)
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "0.00")
    w.add_tran("expiry_boundary_equal", c, "15.00")


@case("expiry_boundary_day_before", "ACCT-EXPIRAION-DATE one day before ORIG-TS(1:10) -> reason 103.")
def _c15(w: World):
    a = w.new_account("5000.00", expiry="2026-03-14")
    c = w.new_card(a.acct_id)
    w.add_tran("expiry_boundary_day_before", c, "15.00")


@case("debit_sign_interaction", "CYC-DEBIT already negative (as the program leaves it); '- CYC-DEBIT' then ADDS to the temp balance and a 50.00 purchase is over a 100.00 limit (CBTRN02C.cbl:403-405).")
def _c16(w: World):
    a = w.new_account("100.00", cyc_debit=Decimal("-500.00"))
    c = w.new_card(a.acct_id)
    w.add_tran("debit_sign_interaction", c, "50.00")


@case("credit_relieves_overlimit", "Account already over limit; a negative amount brings temp balance under the limit and posts.")
def _c17(w: World):
    a = w.new_account("100.00", cyc_credit=Decimal("150.00"), curr_bal=Decimal("150.00"))
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, DEFAULT_TYPE, DEFAULT_CAT, "150.00")
    w.add_tran("credit_relieves_overlimit", c, "-100.00")


@case("two_categories_one_account", "Same account, two (type,cat) keys: two TCATBAL rows touched, one created.")
def _c18(w: World):
    a = w.new_account("5000.00")
    c = w.new_card(a.acct_id)
    w.seed_tcat(a.acct_id, "01", 1, "0.00")
    w.add_tran("two_categories_one_account", c, "10.00", type_cd="01", cat_cd=1)
    w.add_tran("two_categories_one_account", c, "20.00", type_cd="02", cat_cd=3)


# ---------------------------------------------------------------------------
# Volume set: mixes the same behaviours at a documented ratio
# ---------------------------------------------------------------------------

VOLUME_MIX = [
    # (label, weight percent)
    ("vol_clean_accept", 80),
    ("vol_negative_amount", 4),
    ("vol_card_not_in_xref", 4),
    ("vol_account_not_on_file", 3),
    ("vol_overlimit_attempt", 4),
    ("vol_expired_account", 3),
    ("vol_zero_amount", 2),
]
assert sum(w for _, w in VOLUME_MIX) == 100


def build_volume(w: World, rng: random.Random, n_records: int, n_accounts: int = 150) -> None:
    good_accts: List[Account] = []
    expired_accts: List[Account] = []
    tight_accts: List[Account] = []
    for i in range(n_accounts):
        limit = Decimal(rng.choice([2500, 5000, 10000, 25000, 50000])) + Decimal("0.00")
        a = w.new_account(limit)
        good_accts.append(a)
    for i in range(max(3, n_accounts // 20)):
        expired_accts.append(w.new_account("5000.00", expiry=PAST_EXPIRY))
    for i in range(max(3, n_accounts // 15)):
        tight_accts.append(w.new_account("100.00"))
    cards = {a.acct_id: w.new_card(a.acct_id) for a in good_accts + expired_accts + tight_accts}
    # ~70% of the (account, type, cat) combinations used by the good accounts get a
    # pre-existing TCATBAL row; the rest are created by the program.
    type_cats = [("01", 1), ("01", 2), ("02", 3), ("03", 5)]
    for a in good_accts:
        for tc in type_cats:
            if rng.random() < 0.70:
                w.seed_tcat(a.acct_id, tc[0], tc[1], Decimal(rng.randint(-50000, 500000)) / 100)
    labels = [lbl for lbl, wt in VOLUME_MIX for _ in range(wt)]
    ghost_cards: List[str] = []
    for i in range(max(3, n_records // 50)):
        w._n_acct += 1
        ghost_cards.append(w.new_card(synthetic_acct(w._n_acct)))   # XREF -> no ACCTFILE row
    for i in range(n_records):
        lbl = rng.choice(labels)
        tc = rng.choice(type_cats)
        merchant = rng.randint(1, 500)
        if lbl == "vol_clean_accept":
            a = rng.choice(good_accts)
            amt = Decimal(rng.randint(100, 50000)) / 100
            w.add_tran(lbl, cards[a.acct_id], amt, type_cd=tc[0], cat_cd=tc[1], merchant_n=merchant)
        elif lbl == "vol_negative_amount":
            a = rng.choice(good_accts)
            amt = -Decimal(rng.randint(100, 20000)) / 100
            w.add_tran(lbl, cards[a.acct_id], amt, type_cd=tc[0], cat_cd=tc[1], merchant_n=merchant)
        elif lbl == "vol_card_not_in_xref":
            w.add_tran(lbl, w.new_card(None), Decimal(rng.randint(100, 50000)) / 100,
                       type_cd=tc[0], cat_cd=tc[1], merchant_n=merchant)
        elif lbl == "vol_account_not_on_file":
            w.add_tran(lbl, rng.choice(ghost_cards), Decimal(rng.randint(100, 50000)) / 100,
                       type_cd=tc[0], cat_cd=tc[1], merchant_n=merchant)
        elif lbl == "vol_overlimit_attempt":
            a = rng.choice(tight_accts)
            amt = Decimal(rng.randint(2000, 90000)) / 100    # 20.00 .. 900.00 against a 100.00 limit
            w.add_tran(lbl, cards[a.acct_id], amt, type_cd=tc[0], cat_cd=tc[1], merchant_n=merchant)
        elif lbl == "vol_expired_account":
            a = rng.choice(expired_accts)
            w.add_tran(lbl, cards[a.acct_id], Decimal(rng.randint(100, 50000)) / 100,
                       type_cd=tc[0], cat_cd=tc[1], merchant_n=merchant)
        elif lbl == "vol_zero_amount":
            a = rng.choice(good_accts)
            w.add_tran(lbl, cards[a.acct_id], Decimal("0.00"), type_cd=tc[0], cat_cd=tc[1],
                       merchant_n=merchant)


# ---------------------------------------------------------------------------
# Prediction: the generator's model of CBTRN02C (program wins on disagreement)
# ---------------------------------------------------------------------------

def _trunc(value: Decimal, f: layouts.Field) -> Decimal:
    """COBOL store semantics for a DISPLAY numeric receiving field: drop excess
    fraction, drop high-order digits (no ON SIZE ERROR in CBTRN02C)."""
    return layouts.decode_zoned(encode_zoned(value, f), f)


REASONS = {
    0: "",
    100: "INVALID CARD NUMBER FOUND",                       # CBTRN02C.cbl:385-386
    101: "ACCOUNT RECORD NOT FOUND",                        # CBTRN02C.cbl:397-398
    102: "OVERLIMIT TRANSACTION",                           # CBTRN02C.cbl:410-411
    103: "TRANSACTION RECEIVED AFTER ACCT EXPIRATION",      # CBTRN02C.cbl:417-418
}


def predict_outcome(w: World) -> List[dict]:
    """Replays the DALYTRAN records in order against copies of the master
    files, following CBTRN02C's main loop (app/cbl/CBTRN02C.cbl:202-219)."""
    accts = {k: Account(**vars(v)) for k, v in w.accounts.items()}
    tcat = dict(w.tcatbal)
    temp_field = layouts.Field("WS-TEMP-BAL", 5, 0, 11, "S9(09)V99", "S9", 11, 2, True)
    acct_bal_f = ACCT.field("ACCT-CURR-BAL")
    tcat_bal_f = TCATBAL.field("TRAN-CAT-BAL")
    out = []
    seen_ids = set()
    for t in w.trans:
        reason = 0
        # 1500-A-LOOKUP-XREF (:380-392): READ XREF-FILE by card; INVALID KEY -> 100
        x = w.xrefs.get(t.card)
        if x is None:
            reason = 100
        else:
            # 1500-B-LOOKUP-ACCT (:393-422): READ ACCOUNT-FILE by XREF-ACCT-ID; INVALID KEY -> 101
            a = accts.get(x.acct_id)
            if a is None:
                reason = 101
            else:
                # :403-405 COMPUTE WS-TEMP-BAL = CYC-CREDIT - CYC-DEBIT + AMT (S9(09)V99 receiver)
                temp = _trunc(a.cyc_credit - a.cyc_debit + t.amt, temp_field)
                # :407-413 IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL ... ELSE 102
                if not (a.credit_limit >= temp):
                    reason = 102
                # :414-420 IF ACCT-EXPIRAION-DATE >= ORIG-TS(1:10) ... ELSE 103 (overwrites 102)
                if not (a.expiry >= orig_ts(t.seq)[:10]):
                    reason = 103
        rec = {"tran_id": t.tran_id, "case": t.case, "note": t.note,
               "card": t.card, "amount": str(t.amt),
               "predicted_outcome": "REJECT" if reason else "ACCEPT",
               "predicted_reason": reason, "predicted_reason_desc": REASONS[reason]}
        if reason == 0:
            # 2000-POST-TRANSACTION (:424-444)
            a = accts[w.xrefs[t.card].acct_id]
            key = (a.acct_id, t.type_cd, t.cat_cd)
            # 2700-UPDATE-TCATBAL (:467-501): create (:503-510) or update (:526-528)
            created = key not in tcat
            tcat[key] = _trunc(tcat.get(key, Decimal("0.00")) + t.amt, tcat_bal_f)
            # 2800-UPDATE-ACCOUNT-REC (:545-560)
            a.curr_bal = _trunc(a.curr_bal + t.amt, acct_bal_f)
            if t.amt >= 0:
                a.cyc_credit = _trunc(a.cyc_credit + t.amt, acct_bal_f)
            else:
                a.cyc_debit = _trunc(a.cyc_debit + t.amt, acct_bal_f)
            # 2900-WRITE-TRANSACTION-FILE (:596-613): WRITE keyed by TRAN-ID; duplicate -> abend
            if t.tran_id in seen_ids:
                raise RuntimeError("duplicate TRAN-ID %s would abend CBTRN02C" % t.tran_id)
            seen_ids.add(t.tran_id)
            rec["predicted_acct_id"] = a.acct_id
            rec["predicted_tcatbal_created"] = created
            rec["predicted_acct_curr_bal_after"] = str(a.curr_bal)
        out.append(rec)
    closing = {k: {"ACCT-CURR-BAL": str(v.curr_bal), "ACCT-CURR-CYC-CREDIT": str(v.cyc_credit),
                   "ACCT-CURR-CYC-DEBIT": str(v.cyc_debit)} for k, v in sorted(accts.items())}
    return out, closing, {"%s|%s|%04d" % k: str(v) for k, v in sorted(tcat.items())}


# ---------------------------------------------------------------------------
# Writing the set
# ---------------------------------------------------------------------------

def write_set(w: World, out_dir: str, set_name: str, case_list: List[str], extra: dict) -> dict:
    os.makedirs(out_dir, exist_ok=True)
    with open(os.path.join(out_dir, "DALYTRAN"), "wb") as fh:
        for t in w.trans:
            fh.write(t.record())
    with open(os.path.join(out_dir, "XREFFILE"), "wb") as fh:
        for card in sorted(w.xrefs):
            fh.write(w.xrefs[card].record())
    with open(os.path.join(out_dir, "ACCTFILE"), "wb") as fh:
        for acct_id in sorted(w.accounts):
            fh.write(w.accounts[acct_id].record())
    with open(os.path.join(out_dir, "TCATBALF"), "wb") as fh:
        for key in sorted(w.tcatbal):
            acct_id, type_cd, cat_cd = key
            fh.write(build_record(TCATBAL, {
                "TRANCAT-ACCT-ID": acct_id, "TRANCAT-TYPE-CD": type_cd,
                "TRANCAT-CD": cat_cd, "TRAN-CAT-BAL": w.tcatbal[key]}))

    predictions, closing, tcat_closing = predict_outcome(w)
    n_acc = sum(1 for p in predictions if p["predicted_outcome"] == "ACCEPT")
    by_reason: Dict[str, int] = {}
    for p in predictions:
        if p["predicted_reason"]:
            k = "%04d" % p["predicted_reason"]
            by_reason[k] = by_reason.get(k, 0) + 1
    manifest = {
        "set_name": set_name,
        "seed": w.seed,
        "generator": "tests/golden/generate.py",
        "cases": case_list,
        "synthetic_markers": {
            "card_prefix": CARD_TEST_PREFIX, "cards_fail_luhn": True,
            "account_id_prefix": "%04d" % ACCT_PREFIX, "customer_id_prefix": "%04d" % CUST_PREFIX,
            "merchant_id_prefix": "%04d" % MERCHANT_PREFIX,
            "names": "GOLDEN MERCHANT nnnn / SYNTHETIC CITY / GOLDEN CASE <case>",
        },
        "posting_date": POSTING_DATE,
        "frozen_clock": {"COB_CURRENT_DATE": FROZEN_CLOCK, "expected_TRAN_PROC_TS": FROZEN_PROC_TS},
        "sign_convention": "cobc -fsign=EBCDIC (trailing overpunch { A-I / } J-R)",
        "record_counts": {
            "DALYTRAN": len(w.trans), "XREFFILE": len(w.xrefs),
            "ACCTFILE": len(w.accounts), "TCATBALF": len(w.tcatbal),
        },
        "layouts": {name: {"record_length": lay.length, "fields": len(lay.fields), "source": lay.source}
                    for name, lay in (("DALYTRAN", DALYTRAN), ("XREFFILE", XREF), ("ACCTFILE", ACCT),
                                      ("TCATBALF", TCATBAL), ("TRANSACT", layouts.TRANSACT),
                                      ("DALYREJS", layouts.DALYREJS))},
        "prediction_disclaimer": (
            "predicted_* values are the GENERATOR'S model of CBTRN02C, not the golden truth. "
            "run_reference.sh runs the program; where they disagree the program wins and the "
            "disagreement is logged in docs/validation/golden-set/findings.md."),
        "predicted_summary": {
            "records_in": len(predictions), "accepted": n_acc, "rejected": len(predictions) - n_acc,
            "rejected_by_reason": by_reason,
        },
        "predicted_closing_accounts": closing,
        "predicted_closing_tcatbal": tcat_closing,
        "records": predictions,
    }
    manifest.update(extra)
    with open(os.path.join(out_dir, "manifest.json"), "w") as fh:
        json.dump(manifest, fh, indent=2)
        fh.write("\n")
    return manifest


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--set", choices=("named", "volume"), help="which set to generate")
    ap.add_argument("--out", help="output directory for the input fixtures + manifest.json")
    ap.add_argument("--seed", type=int, default=20260315)
    ap.add_argument("--cases", default="all", help="comma-separated case names for --set named")
    ap.add_argument("--volume", type=int, default=1200, help="record count for --set volume")
    ap.add_argument("--list-cases", action="store_true")
    args = ap.parse_args(argv)

    if args.list_cases:
        for name, (doc, _) in CASES.items():
            print("%-32s %s" % (name, doc))
        return 0
    if not args.set or not args.out:
        ap.error("--set and --out are required")

    w = World(seed=args.seed)
    rng = random.Random(args.seed)
    if args.set == "named":
        names = list(CASES) if args.cases == "all" else args.cases.split(",")
        for n in names:
            if n not in CASES:
                ap.error("unknown case %s" % n)
            CASES[n][1](w)
        extra = {"case_docs": {n: CASES[n][0] for n in names}}
        manifest = write_set(w, args.out, "named", names, extra)
    else:
        build_volume(w, rng, args.volume)
        extra = {"volume_mix_percent": dict(VOLUME_MIX), "volume_records_requested": args.volume}
        manifest = write_set(w, args.out, "volume", [lbl for lbl, _ in VOLUME_MIX], extra)

    rc = manifest["record_counts"]
    ps = manifest["predicted_summary"]
    print("generated %s set -> %s" % (args.set, os.path.relpath(args.out)))
    print("  DALYTRAN=%d XREFFILE=%d ACCTFILE=%d TCATBALF=%d" % (
        rc["DALYTRAN"], rc["XREFFILE"], rc["ACCTFILE"], rc["TCATBALF"]))
    print("  predicted: accepted=%d rejected=%d by_reason=%s (generator prediction, not golden truth)" % (
        ps["accepted"], ps["rejected"], json.dumps(ps["rejected_by_reason"], sort_keys=True)))
    return 0


if __name__ == "__main__":
    sys.exit(main())

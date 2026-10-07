#!/usr/bin/env python3
"""Deterministic DALYTRAN volume file for the s6.4 volume smoke test (CVTRA06Y, RECLN 350, ASCII line sequential).

Every record uses a card of app/data/ASCII/cardxref.txt and the type/category pairs of the sample DALYTRAN
(01/0001 purchases, 03/0001 returns). About 1% are built to be rejected by CBTRN02C, in fixed counts so the
expected outcome is known before the run:

  reason 100  card number not in CARDXREF                         (--invalid-card, default 400)
  reason 102  amount 99999999.99, over any credit limit           (--overlimit,    default 300)
  reason 103  ORIG-TS 2099-12-31, after every account expiry date (--expired,      default 300)

Accepted records keep each account under its credit limit. CBTRN02C R-6 adds |amount| to
ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT for both signs (the debit bucket accumulates negative values), so the
generator spends at most 90% of each account's headroom (limit - credit + debit in app/data/ASCII/acctdata.txt,
which the EBCDIC initial-load sample matches for these fields).

  scripts/volume/gen_dalytran.py --out dalytran-100k.txt [--records 100000] [--seed 20221006]

Writes <out> and <out>.expected.json (read / posted / rejected counts by reason, expected RC).
"""
import argparse
import json
import random
from decimal import Decimal
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NEG = "}JKLMNOPQR"
POS = "{ABCDEFGHI"


def zoned_value(text: str) -> Decimal:
    last = text[-1]
    if last.isdigit():
        digit, sign = int(last), 1
    elif last in POS:
        digit, sign = POS.index(last), 1
    else:
        digit, sign = NEG.index(last), -1
    return sign * Decimal(text[:-1] + str(digit)) / 100


def zoned(cents: int, digits: int) -> str:
    text = f"{abs(cents):0{digits}d}"
    return text[:-1] + (NEG if cents < 0 else POS)[int(text[-1])]


def record(tran_id, type_cd, desc, cents, card, orig_ts, rng):
    merchant = rng.randrange(1, 10**9)
    fields = [
        tran_id, type_cd, "0001", "POS TERM".ljust(10), desc.ljust(100), zoned(cents, 11),
        f"{merchant:09d}", f"Volume Merchant {merchant % 997:03d}".ljust(50), "Volume City".ljust(50),
        f"{rng.randrange(10000, 99999)}".ljust(10), card, orig_ts.ljust(26), " " * 26, " " * 20,
    ]
    line = "".join(fields)
    assert len(line) == 350, len(line)
    return line


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", required=True, type=Path)
    ap.add_argument("--records", type=int, default=100_000)
    ap.add_argument("--seed", type=int, default=20221006)
    ap.add_argument("--invalid-card", type=int, default=400)
    ap.add_argument("--overlimit", type=int, default=300)
    ap.add_argument("--expired", type=int, default=300)
    a = ap.parse_args()
    rng = random.Random(a.seed)

    xref = [line[:36] for line in (ROOT / "app/data/ASCII/cardxref.txt").read_text().splitlines() if line.strip()]
    card_acct = {x[:16]: x[25:36] for x in xref}
    headroom = {}
    for line in (ROOT / "app/data/ASCII/acctdata.txt").read_text(encoding="latin-1").splitlines():
        limit, credit, debit = (zoned_value(line[s:s + 12]) for s in (24, 78, 90))
        headroom[line[:11]] = int((limit - credit + debit) * 100)

    rejects = ["100"] * a.invalid_card + ["102"] * a.overlimit + ["103"] * a.expired
    plan = rejects + [None] * (a.records - len(rejects))
    rng.shuffle(plan)
    cards = sorted(card_acct)
    per_card = {c: 0 for c in cards}
    picks = [rng.choice(cards) for _ in plan]
    for kind, card in zip(plan, picks):
        if kind is None:
            per_card[card] += 1
    budget = {c: headroom[card_acct[c]] * 9 // 10 for c in cards}
    left = dict(per_card)

    counts = {"100": 0, "102": 0, "103": 0}
    with a.out.open("w") as out:
        for n, (kind, card) in enumerate(zip(plan, picks), start=1):
            tran_id = f"{9_000_000_000_000_000 + n:016d}"
            day = 1 + n * 5 // (a.records + 1)
            ts = f"2022-07-{day:02d} {n % 24:02d}:{n % 60:02d}:{(n * 7) % 60:02d}.000000"
            if kind == "100":
                bad = f"9{rng.randrange(10**14, 10**15):015d}"
                while bad in card_acct:
                    bad = f"9{rng.randrange(10**14, 10**15):015d}"
                card, cents, desc = bad, rng.randrange(100, 10000), "Volume invalid card"
            elif kind == "102":
                cents, desc = 9_999_999_999, "Volume overlimit"
            elif kind == "103":
                cents, desc, ts = rng.randrange(100, 10000), "Volume after expiry", "2099-12-31 00:00:00.000000"
            else:
                fair = budget[card] // left[card]
                cents = max(1, min(fair * 2 - 1, 5000)) if fair > 0 else 1
                cents = min(rng.randint(1, cents) if cents > 1 else 1, budget[card] - (left[card] - 1))
                if cents < 1:
                    raise SystemExit(f"headroom exhausted for card {card}")
                budget[card] -= cents
                left[card] -= 1
                if rng.random() < 1 / 6:
                    out.write(record(tran_id, "03", "Volume return", -cents, card, ts, rng) + "\n")
                    continue
                desc = "Volume purchase"
            if kind:
                counts[kind] += 1
            out.write(record(tran_id, "01", desc, cents, card, ts, rng) + "\n")

    rejected = sum(counts.values())
    expected = {"seed": a.seed, "read": a.records, "posted": a.records - rejected, "rejected": rejected,
                "rejected_by_reason": counts, "expected_rc": 4 if rejected else 0}
    Path(str(a.out) + ".expected.json").write_text(json.dumps(expected, indent=2) + "\n")
    print(json.dumps(expected))


if __name__ == "__main__":
    main()

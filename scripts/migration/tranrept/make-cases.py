#!/usr/bin/env python3
"""Builds the TRANREPT edge cases from the sample case (a COBOL-produced TRANSACT unload).

    python3 scripts/migration/tranrept/make-cases.py
    for c in modernization/batch-service/src/test/resources/tranrept/*/; do
        scripts/migration/tranrept/reference.sh --write "$c"; done

Each case directory gets TRANSACT.txt, DATEPARM.txt and MODE; reference.sh adds expected/.
"""
import datetime
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[3]
CASES = ROOT / "modernization/batch-service/src/test/resources/tranrept"
SAMPLE = [l.ljust(350) for l in (CASES / "sample/TRANSACT.txt").read_text("ascii").splitlines()]
POS = "{ABCDEFGHI"
NEG = "}JKLMNOPQR"


def card(r):
    return r[262:278]


def with_proc_date(r, date):
    return r[:304] + date + r[314:]


def with_amount(r, cents):
    digits = f"{abs(cents):011d}"
    last = (NEG if cents < 0 else POS)[int(digits[-1])]
    return r[:132] + digits[:-1] + last + r[143:]


def with_field(r, offset, value):
    return r[:offset] + value + r[offset + len(value):]


def write(name, records, dateparm, mode):
    d = CASES / name
    d.mkdir(parents=True, exist_ok=True)
    (d / "TRANSACT.txt").write_text("".join(r + "\n" for r in records), "ascii")
    (d / "DATEPARM.txt").write_text(dateparm + "\n" if dateparm else "", "ascii")
    (d / "MODE").write_text(mode + "\n", "ascii")


by_card = sorted(SAMPLE, key=card)
FULL = "2022-01-01 2022-07-06"

# SORT INCLUDE: processing dates spread over the first half of 2022, a window in the middle.
base = datetime.date(2022, 1, 1)
spread = [with_proc_date(r, (base + datetime.timedelta(days=(i * 7) % 200)).isoformat()) for i, r in enumerate(SAMPLE)]
write("window", spread, "2022-03-01 2022-05-31", "job")

# NEXT SENTENCE: an out-of-range record in the middle of TRANFILE ends the loop (no totals).
ns = by_card[:30]
ns[12] = with_proc_date(ns[12], "2022-08-01")
write("next-sentence", ns, FULL, "program")

# The last record is out of range: the loop ends before the end-of-file branch (no totals).
last = by_card[:10]
last[-1] = with_proc_date(last[-1], "2021-12-31")
write("last-out-of-range", last, FULL, "program")

# TRANFILE not sorted by card: every card change writes an account total and re-reads CARDXREF.
write("unsorted", SAMPLE[:45], FULL, "program")

# No DATEPARM record: END-OF-FILE is set before the loop, an empty report.
write("empty-dateparm", SAMPLE, "", "job")

# Nothing in the window: TRANFILE is empty.
write("no-match", SAMPLE, "2030-01-01 2030-12-31", "job")

# Edited amounts and S9(09)V99 overflow: zero, small, negative and maximum amounts, page break.
amounts = [0, 5, -1, 100, -4788, 99999999999, 99999999999, 2, -99999999999, -99999999999, -99999999999,
           123456789, -123456789, 0, 10, 1, 50000000000, 50000000000, -7, 3, 4, 999, -999, 25, 0, 99999999999]
recs = []
for i, cents in enumerate(amounts):
    r = by_card[i // 9 * 7]  # three cards
    recs.append(with_amount(r, cents))
write("amounts", recs, FULL, "program")

# Abends: a card missing from CARDXREF, a type missing from TRANTYPE, a category missing from TRANCATG.
bad = by_card[:5]
bad[3] = with_field(bad[3], 262, "9999999999999999")
write("abend-xref", bad, FULL, "program")
bad = by_card[:5]
bad[2] = with_field(bad[2], 16, "99")
write("abend-trantype", bad, FULL, "program")
bad = by_card[:5]
bad[4] = with_field(bad[4], 18, "0042")
write("abend-trancatg", bad, FULL, "program")

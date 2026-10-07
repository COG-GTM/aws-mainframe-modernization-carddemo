#!/usr/bin/env python3
"""Generate the CBSTM03A edge-case fixtures under modernization/carddemo-app/src/test/resources/creastmt/ by running
the GnuCOBOL baseline build of CBSTM03A (+ CBSTM03B) on synthetic TRXFL / CARDXREF inputs, so the Java unit tests
(Cbstm03aEdgeCaseTest) compare with what the legacy program really writes rather than with expectations written
by hand.

Needs the baseline work directory: run `scripts/baseline/run_baseline.sh --fast --keep-work` first (it compiles
CBSTM03A and the IDCAMS emulators into build/baseline-work/bin). CUSTFILE / ACCTFILE are the baseline after-images
the CREASTMT baseline run read (CUSTFILE/CUSTDATA.ksds.txt, INTCALC/ACCTDATA.ksds.txt); the tests read them from
docs/validation/baseline directly.

Cases (TRXFL records are taken from docs/validation/baseline/CREASTMT/TRXFL.SEQ.txt; A and B are its first two
cards, A with 7 transactions):
  no-transactions  XREF: a card below every TRXFL card, A, a card between A and B, B, a card above every TRXFL card;
                   TRXFL: A's and B's transactions. Three statements have no transaction lines and a zero total.
  overflow         XREF: A, a card between A and B, B; TRXFL: A's 7 transactions + 5 more (ids 9000000000000001..5,
                   amounts 100.00..500.00) = 12 > OCCURS 10, then B's. Shows the legacy storage overlay.
  empty            XREF: A; TRXFL: no records. CBSTM03A abends on the first TRNXFILE read (status 10).
"""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
BIN = REPO / "build" / "baseline-work" / "bin"
BASELINE = REPO / "docs" / "validation" / "baseline"
OUT = REPO / "modernization" / "carddemo-app" / "src" / "test" / "resources" / "creastmt"
CUSTDATA = BASELINE / "CUSTFILE" / "CUSTDATA.ksds.txt"
ACCTDATA = BASELINE / "INTCALC" / "ACCTDATA.ksds.txt"
XREFDATA = BASELINE / "XREFFILE" / "CARDXREF.ksds.txt"


def lines(path: Path, lrecl: int) -> list[bytes]:
    return [l.ljust(lrecl).encode("latin-1") for l in path.read_text(encoding="latin-1").splitlines()]


def cases() -> dict[str, tuple[list[bytes], list[bytes]]]:
    trx = lines(BASELINE / "CREASTMT" / "TRXFL.SEQ.txt", 350)
    cards: list[tuple[bytes, list[bytes]]] = []
    for r in trx:
        if not cards or cards[-1][0] != r[:16]:
            cards.append((r[:16], []))
        cards[-1][1].append(r)
    (a, ar), (b, br) = cards[0], cards[1]
    xref = {l[:16]: l for l in lines(XREFDATA, 50)}
    # customer / account ids for the synthetic cards: other XREF records (the account id is a unique alternate key)
    used = {xref[a][25:36], xref[b][25:36]}
    spares = [xref[k][16:] for k in sorted(xref) if xref[k][25:36] not in used][:3]
    between = b"%016d" % (int(a) + 1)
    below = b"%016d" % 1
    above = b"9" * 16
    assert below < a < between < b and above > cards[-1][0]
    extra = []
    for i in range(5):
        rec = ar[i][:16] + b"9%015d" % (i + 1) + ar[i][32:]
        rec = rec[:148] + b"%010d{" % (1000 * (i + 1)) + rec[159:]
        extra.append(rec)
    return {
        "no-transactions": (ar + br, [below + spares[0], xref[a], between + spares[1], xref[b], above + spares[2]]),
        "overflow": (ar + extra + br, [xref[a], between + spares[0], xref[b]]),
        "empty": ([], [xref[a]]),
    }


def run(name: str, trxfl: list[bytes], xref: list[bytes], work: Path) -> None:
    env = {**os.environ, "COB_CURRENT_DATE": "2022-07-06 00:00:00.00", "COB_LIBRARY_PATH": str(BIN)}
    inputs = {"trx": trxfl, "xref": xref, "cust": lines(CUSTDATA, 500), "acct": lines(ACCTDATA, 300)}
    for key, records in inputs.items():
        (work / f"{key}.seq").write_bytes(b"".join(records))
    for prog, key in (("IDXTRXFL", "trx"), ("IDXCARDX", "xref"), ("IDXCUSTD", "cust"), ("IDXACCTD", "acct")):
        r = subprocess.run([str(BIN / prog), "LOAD"], capture_output=True, text=True,
                           env={**env, "DD_SEQFILE": str(work / f"{key}.seq"), "DD_IDXFILE": str(work / f"{key}.idx")})
        if r.returncode != 0 or "REJECTED 000000000" not in r.stdout:
            sys.exit(f"{name}: {prog} LOAD failed: {r.stdout}{r.stderr}")
    dd = {"DD_TRNXFILE": "trx.idx", "DD_XREFFILE": "xref.idx", "DD_CUSTFILE": "cust.idx", "DD_ACCTFILE": "acct.idx",
          "DD_STMTFILE": "stmt.out", "DD_HTMLFILE": "html.out"}
    r = subprocess.run([str(BIN / "CBSTM03A")], capture_output=True,
                       env={**env, **{k: str(work / v) for k, v in dd.items()}})
    out = OUT / name
    shutil.rmtree(out, ignore_errors=True)
    out.mkdir(parents=True)
    (out / "TRXFL.txt").write_bytes(b"".join(x + b"\n" for x in trxfl))
    (out / "XREF.txt").write_bytes(b"".join(x + b"\n" for x in xref))
    (out / "rc.txt").write_text(f"{r.returncode}\n")
    sysout = [l.rstrip() for l in r.stdout.decode("latin-1").splitlines() if not l.startswith("libcob: ")]
    (out / "sysout.txt").write_text("".join(l + "\n" for l in sysout))
    for f, lrecl, target in (("stmt.out", 80, "STMTFILE.txt"), ("html.out", 100, "HTMLFILE.txt")):
        data = (work / f).read_bytes() if (work / f).exists() else b""
        assert len(data) % lrecl == 0, (name, f, len(data))
        (out / target).write_bytes(b"".join(data[i:i + lrecl] + b"\n" for i in range(0, len(data), lrecl)))
    print(f"{name}: rc={r.returncode} TRXFL={len(trxfl)} XREF={len(xref)} -> {out.relative_to(REPO)}")


def main() -> int:
    if not (BIN / "CBSTM03A").exists():
        sys.exit(f"{BIN}/CBSTM03A missing: run scripts/baseline/run_baseline.sh --fast --keep-work first")
    for name, (trxfl, xref) in cases().items():
        with tempfile.TemporaryDirectory() as tmp:
            run(name, trxfl, xref, Path(tmp))
    return 0


if __name__ == "__main__":
    sys.exit(main())

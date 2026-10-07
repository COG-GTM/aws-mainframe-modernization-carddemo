"""Build the golden files from a GnuCOBOL run of CBACT01C / CBTRN01C.

Prerequisites (see cobol/run_all.sh, which runs them in order):

    test-harness/cobol/build.sh          # compile programs, stubs, loader
    test-harness/cobol/load_ksds.sh      # ASCII sample data -> indexed files
    test-harness/cobol/run_cbact01c.sh   # -> cobol/work/CBACT01C/{OUTFILE,ARRYFILE,VBRCFILE,display.txt}
    test-harness/cobol/run_cbtrn01c.sh   # -> cobol/work/CBTRN01C/display.txt

Then::

    python3 test-harness/generate_goldens.py CBACT01C
    python3 test-harness/generate_goldens.py CBTRN01C
    python3 test-harness/generate_goldens.py CBTRN01C --work cobol/work/synthetic-rejections \
            --out golden-files/CBTRN01C/synthetic-rejections --dailytran <file> --cardxref <file>

For CBTRN01C the program writes no data file: outcomes.json is derived
from the DISPLAY output (display.txt) by pairing each echoed DALYTRAN
record with the lookup messages that follow it.
"""
from __future__ import annotations

import argparse
import os
import re
import shutil
import sys
from typing import Dict, List

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
sys.path.insert(0, HERE)

from copybook import load_layout, parse_file, layout  # noqa: E402
from records import decode_file, dump_json  # noqa: E402
import reconcile  # noqa: E402

CPY = os.path.join(REPO, "app", "cpy")
CBL = os.path.join(REPO, "app", "cbl")
DATA = os.path.join(REPO, "app", "data", "ASCII")


def golden_cbact01c(work: str, out: str, acctdata: str) -> Dict:
    os.makedirs(os.path.join(out, "raw"), exist_ok=True)
    acct_in = decode_file(acctdata, load_layout(os.path.join(CPY, "CVACT01Y.cpy")), "line")
    prog = parse_file(os.path.join(CBL, "CBACT01C.cbl"))
    outfile = decode_file(os.path.join(work, "OUTFILE"), layout(prog, "OUT-ACCT-REC"))
    arryfile = decode_file(os.path.join(work, "ARRYFILE"), layout(prog, "ARR-ARRAY-REC"))
    vbrcfile = decode_file(os.path.join(work, "VBRCFILE"), None, "vb",
                           layouts_by_length={12: layout(prog, "VBRC-REC1"), 39: layout(prog, "VBRC-REC2")})
    dump_json(acct_in, os.path.join(out, "input-acctdata.json"))
    dump_json(outfile, os.path.join(out, "outfile.json"))
    dump_json(arryfile, os.path.join(out, "arryfile.json"))
    dump_json(vbrcfile, os.path.join(out, "vbrcfile.json"))
    for name in ("OUTFILE", "ARRYFILE", "VBRCFILE"):
        shutil.copyfile(os.path.join(work, name), os.path.join(out, "raw", name))
    shutil.copyfile(os.path.join(work, "display.txt"), os.path.join(out, "display.txt"))
    res = reconcile.run("cbact01c", out, write=True)
    print("CBACT01C goldens -> %s : input %d, OUTFILE %d, ARRYFILE %d, VBRCFILE %d records" % (
        out, len(acct_in), len(outfile), len(arryfile), len(vbrcfile)))
    return res


RE_ACCT_NOT_FOUND = re.compile(r"^ACCOUNT (\d{11}) NOT FOUND$")
RE_CARD_SKIP = re.compile(r"^CARD NUMBER (.{16}) COULD NOT BE VERIFIED\. SKIPPING TRANSACTION ID-(.{16})$")


def parse_cbtrn01c_display(display_path: str, tran_in: List[Dict]) -> List[Dict]:
    """Pair every echoed DALYTRAN record with the lookup lines after it."""
    with open(display_path, encoding="latin-1") as fh:
        lines = [ln.rstrip("\n") for ln in fh]
    tran_ids = [t["DALYTRAN-ID"] for t in tran_in]
    outcomes: List[Dict] = []
    cur = None
    for ln in lines:
        if len(ln) == 350 and ln[:16] in tran_ids and not ln.startswith(("CARD NUMBER", "ACCOUNT ")):
            cur = {"tran_id": ln[:16], "card_num": ln[262:278], "xref_found": None,
                   "acct_id": None, "acct_found": None, "outcome": None}
            outcomes.append(cur)
            continue
        if cur is None or cur["outcome"] is not None:
            # lookup lines after the outcome is settled belong to the
            # post-EOF duplicate lookup (see reconcile CBTRN01C-COUNT-04)
            continue
        if ln == "SUCCESSFUL READ OF XREF":
            cur["xref_found"] = True
        elif ln == "INVALID CARD NUMBER FOR XREF":
            cur["xref_found"] = False
        elif ln.startswith("ACCOUNT ID : "):
            cur["acct_id"] = str(int(ln[len("ACCOUNT ID : "):].strip()))
        elif ln == "SUCCESSFUL READ OF ACCOUNT FILE":
            cur["acct_found"] = True
            cur["outcome"] = reconcile.OUTCOME_VERIFIED
        elif ln == "INVALID ACCOUNT NUMBER FOUND":
            cur["acct_found"] = False
        elif RE_ACCT_NOT_FOUND.match(ln):
            cur["outcome"] = reconcile.OUTCOME_ACCT_MISSING
        elif RE_CARD_SKIP.match(ln):
            cur["outcome"] = reconcile.OUTCOME_CARD_MISSING
    return outcomes


def golden_cbtrn01c(work: str, out: str, dailytran: str, cardxref: str, acctdata: str) -> Dict:
    os.makedirs(out, exist_ok=True)
    tran_in = decode_file(dailytran, load_layout(os.path.join(CPY, "CVTRA06Y.cpy")), "line")
    xref = decode_file(cardxref, load_layout(os.path.join(CPY, "CVACT03Y.cpy")), "line")
    acct = decode_file(acctdata, load_layout(os.path.join(CPY, "CVACT01Y.cpy")), "line")
    outcomes = parse_cbtrn01c_display(os.path.join(work, "display.txt"), tran_in)
    dump_json(tran_in, os.path.join(out, "input-dailytran.json"))
    dump_json(xref, os.path.join(out, "input-cardxref.json"))
    dump_json(acct, os.path.join(out, "input-acctdata.json"))
    dump_json(outcomes, os.path.join(out, "outcomes.json"))
    shutil.copyfile(os.path.join(work, "display.txt"), os.path.join(out, "display.txt"))
    res = reconcile.run("cbtrn01c", out, write=True)
    print("CBTRN01C goldens -> %s : input %d transactions, %d outcome rows" % (out, len(tran_in), len(outcomes)))
    return res


def main(argv: List[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("program", choices=["CBACT01C", "CBTRN01C"])
    ap.add_argument("--work", help="run directory (default test-harness/cobol/work/<program>)")
    ap.add_argument("--out", help="golden directory (default golden-files/<program>)")
    ap.add_argument("--acctdata", default=os.path.join(DATA, "acctdata.txt"))
    ap.add_argument("--dailytran", default=os.path.join(DATA, "dailytran.txt"))
    ap.add_argument("--cardxref", default=os.path.join(DATA, "cardxref.txt"))
    a = ap.parse_args(argv[1:])
    work = a.work or os.path.join(HERE, "cobol", "work", a.program)
    out = a.out or os.path.join(REPO, "golden-files", a.program)
    if a.program == "CBACT01C":
        res = golden_cbact01c(work, out, a.acctdata)
    else:
        res = golden_cbtrn01c(work, out, a.dailytran, a.cardxref, a.acctdata)
    reconcile.print_report(res)
    return 0 if res["summary"]["status"] == "PASS" else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))

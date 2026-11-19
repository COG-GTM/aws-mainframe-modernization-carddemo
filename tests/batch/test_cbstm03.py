"""CBSTM03A (+ CBSTM03B file-access subroutine) - account statements
(CREASTMT.JCL).

CBSTM03A walks the z/OS PSA/TCB/TIOT control blocks to list DD names
before doing any work; those absolute-address reads cannot run off z/OS,
so the harness comments that diagnostic block out (and nothing else).

Fixtures: 3 cross-reference records (cards 1-3) and transact.txt
rebuilt into TRNXFILE (key = card number + tran id) exactly like the
SORT/REPRO steps of CREASTMT.JCL.
  card1: +100.00, -50.00  -> Total EXP  50.00
  card2: +200.00          -> Total EXP 200.00
  card3: (none)           -> Total EXP   0.00
"""
import re
import unittest
from decimal import Decimal as D

from base import (KSDS_ACCT, KSDS_CUST, KSDS_TRNX, KSDS_XREF, BatchTestCase,
                  harness)
from harness import comment_out, patched_source, program_source

FIXED_LINES_PER_STATEMENT = 19  # header/address/details/summary/total/end


def strip_tiot(text):
    return comment_out(text, "SET ADDRESS OF PSA-BLOCK", "OPEN OUTPUT STMT-FILE")


def to_trnx(tran_rec):
    # OUTREC FIELDS=(1:263,16,17:1,262,279:279,50) from CREASTMT.JCL
    return tran_rec[262:278] + tran_rec[0:262] + tran_rec[278:350]


class TestCBSTM03(BatchTestCase):
    PROGRAM = "CBSTM03A"

    def setUp(self):
        super().setUp()
        trnx = [to_trnx(r) for r in harness.read_fixture("transact.txt", 350)]
        self.dds = {
            "TRNXFILE": self.ksds(KSDS_TRNX, "trnxfile", records=trnx),
            "XREFFILE": self.ksds(KSDS_XREF, "cardxref", "cardxref_stmt.txt"),
            "CUSTFILE": self.ksds(KSDS_CUST, "custdata", "customers.txt"),
            "ACCTFILE": self.ksds(KSDS_ACCT, "acctdata", "accounts.txt"),
            "STMTFILE": self.work / "statemnt.ps",
            "HTMLFILE": self.work / "statemnt.html",
        }
        exe = self.build(sources=[patched_source("CBSTM03A", strip_tiot),
                                  program_source("CBSTM03B")])
        self.result = harness.run(exe, self.dds)
        self.stmt = [r.rstrip() for r in harness.read_seq(self.dds["STMTFILE"], 80)]
        self.html = "\n".join(r.rstrip() for r in harness.read_seq(self.dds["HTMLFILE"], 100))

    def statements(self):
        out, cur = [], None
        for line in self.stmt:
            if "START OF STATEMENT" in line:
                cur = []
                out.append(cur)
            cur.append(line)
        return out

    def test_return_code(self):
        self.assertRC(self.result, 0)

    def test_statement_and_record_counts(self):
        stmts = self.statements()
        self.assertEqual(len(stmts), 3)
        self.assertEqual(sum("END OF STATEMENT" in l for l in self.stmt), 3)
        self.assertEqual([len(s) for s in stmts],
                         [FIXED_LINES_PER_STATEMENT + n for n in (2, 1, 0)])
        self.assertEqual(len(self.stmt), 3 * FIXED_LINES_PER_STATEMENT + 3)

    def test_statement_totals(self):
        totals = []
        for line in self.stmt:
            if line.startswith("Total EXP:"):
                m = re.search(r"\$\s*([\d,]*\.\d\d)(-?)$", line)
                v = D(m.group(1).replace(",", "") or "0")
                totals.append(-v if m.group(2) else v)
        self.assertEqual(totals, [D("50.00"), D("200.00"), D("0.00")])

    def test_statement_contents(self):
        s1 = self.statements()[0]
        self.assertEqual(s1[1], "Alice Q Anders")
        self.assertIn("Account ID         :10000000001", s1)
        self.assertIn("Current Balance    :000001000.00", s1)
        self.assertIn("FICO Score         :750", s1)
        trans = [l for l in s1 if re.match(r"^\d{16} ", l)]
        self.assertEqual([l[:16] for l in trans], ["0000000000000101", "0000000000000102"])
        self.assertTrue(trans[1].endswith("50.00-"))

    def test_html_statements(self):
        self.assertEqual(self.html.count("<h3>Statement for Account Number:"), 3)
        for acct in ("10000000001", "10000000002", "10000000003"):
            self.assertIn(f"Statement for Account Number: {acct}", self.html)
        for tid in ("0000000000000101", "0000000000000102", "0000000000000103"):
            self.assertIn(tid, self.html)
        self.assertTrue(self.html.lstrip().startswith("<!DOCTYPE html>"))


if __name__ == "__main__":
    unittest.main()

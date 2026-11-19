"""CBTRN03C - daily transaction detail report (TRANREPT.jcl).

transact.txt (already sorted by card, as the JCL SORT step does):
  card1: +100.00, -50.00   -> account total  50.00
  card2: +200.00           -> grand total   250.00 (by hand)
Invalid card / type / category keys make the program ABEND (no report).
"""
import re
import unittest
from decimal import Decimal as D

from base import (KSDS_TRANCATG, KSDS_TRANTYPE, KSDS_XREF, BatchTestCase,
                  harness)
from harness import ABEND_RC


def amount(line):
    m = re.search(r"([+-])\s*([\d,]*\.\d\d)\s*$", line)
    value = D(m.group(2).replace(",", ""))
    return -value if m.group(1) == "-" else value


class TestCBTRN03C(BatchTestCase):
    PROGRAM = "CBTRN03C"

    def run_report(self, transact="transact.txt"):
        self.dds = {
            "TRANFILE": self.seq("transact", transact, 350),
            "CARDXREF": self.ksds(KSDS_XREF, "cardxref", "cardxref.txt"),
            "TRANTYPE": self.ksds(KSDS_TRANTYPE, "trantype", "trantype.txt"),
            "TRANCATG": self.ksds(KSDS_TRANCATG, "trancatg", "trancatg.txt"),
            "DATEPARM": self.seq("dateparm", "dateparm.txt", 80),
            "TRANREPT": self.work / "tranrept",
        }
        return harness.run(self.build(), self.dds)

    def report(self):
        return [r.rstrip() for r in harness.read_seq(self.dds["TRANREPT"], 133)]

    def test_report_header_and_rc(self):
        result = self.run_report()
        self.assertRC(result, 0)
        lines = self.report()
        self.assertIn("Daily Transaction Report", lines[0])
        self.assertIn("Date Range: 2022-01-01 to 2022-07-06", lines[0])

    def test_detail_lines(self):
        self.run_report()
        details = [l for l in self.report() if re.match(r"^\d{16} ", l)]
        self.assertEqual([l[:16] for l in details],
                         ["0000000000000101", "0000000000000102", "0000000000000103"])
        self.assertIn("01-Purchase", details[0])
        self.assertIn("0001-Regular Sales Draft", details[0])
        self.assertIn("02-Payment", details[1])
        self.assertEqual(amount(details[1]), D("-50.00"))
        self.assertTrue(details[0].startswith("0000000000000101 10000000001"))

    def test_account_total_for_first_card(self):
        self.run_report()
        totals = [l for l in self.report() if l.startswith("Account Total")]
        self.assertEqual(amount(totals[0]), D("50.00"))

    @unittest.expectedFailure
    def test_grand_total_matches_sum_of_transactions(self):
        """KNOWN DEFECT: at end-of-file the last record (still in the record
        area) is added to the totals a second time, so the grand total is
        450.00 instead of 250.00 and no account total is written for the
        last card.  Remove expectedFailure once CBTRN03C is fixed."""
        self.run_report()
        grand = [l for l in self.report() if l.startswith("Grand Total")]
        self.assertEqual(amount(grand[0]), D("250.00"))

    def test_grand_total_current_behaviour(self):
        """Characterises current output so regressions are still caught."""
        self.run_report()
        grand = [l for l in self.report() if l.startswith("Grand Total")]
        self.assertEqual(len(grand), 1)
        self.assertEqual(amount(grand[0]), D("450.00"))

    def test_unknown_card_abends(self):
        result = self.run_report("transact_bad_card.txt")
        self.assertRC(result, ABEND_RC)
        self.assertIn("INVALID CARD NUMBER : 4999999999999999", result.stdout)
        self.assertIn("ABENDING PROGRAM", result.stdout)

    def test_unknown_transaction_type_abends(self):
        result = self.run_report("transact_bad_type.txt")
        self.assertRC(result, ABEND_RC)
        self.assertIn("INVALID TRANSACTION TYPE : 99", result.stdout)


if __name__ == "__main__":
    unittest.main()

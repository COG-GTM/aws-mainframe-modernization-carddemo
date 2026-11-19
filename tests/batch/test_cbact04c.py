"""CBACT04C - interest calculator (INTCALC.jcl, PARM='2022071800').

Monthly interest = category balance * rate / 1200 (COMPUTE truncates).
Hand-calculated expectations for fixtures tcatbal_interest.txt/discgrp.txt:

  acct  type cat  balance  group    rate   interest
  ----  ---- ---  -------  -------  -----  -----------------------------
  1     01   0001 1000.00  GOLD     18.00  1000.00*18/1200   = 15.00
  1     02   0001  500.00  GOLD      0.00  (rate 0: no transaction)
  2     01   0001  250.00  DEFAULT  24.00  250.00*24/1200    =  5.00
  2     01   0002  333.33  DEFAULT  12.00  333.33*12/1200    =  3.33(33)
  3     01   0001 1234.56  GOLD     18.00  1234.56*18/1200   = 18.51(84)

Account 2's group 'NOGROUP' has no disclosure record, so the program falls
back to the DEFAULT group.
"""
import unittest
from decimal import Decimal as D

from base import (ACCOUNT, KSDS_ACCT, KSDS_DISCGRP, KSDS_TCATBAL,
                  KSDS_XREF_AIX, TRAN, BatchTestCase, harness)
from harness import DRIVERS, program_source


class TestCBACT04C(BatchTestCase):
    PROGRAM = "CBACT04C"

    def setUp(self):
        super().setUp()
        self.dds = {
            "TCATBALF": self.ksds(KSDS_TCATBAL, "tcatbal", "tcatbal_interest.txt"),
            "XREFFILE": self.ksds(KSDS_XREF_AIX, "cardxref", "cardxref.txt"),
            "ACCTFILE": self.ksds(KSDS_ACCT, "acctdata", "accounts.txt"),
            "DISCGRP": self.ksds(KSDS_DISCGRP, "discgrp", "discgrp.txt"),
            "TRANSACT": self.work / "systran",
        }
        exe = self.build(sources=[DRIVERS / "RUNACT04.cbl", program_source("CBACT04C")])
        self.result = harness.run(exe, self.dds, ["2022071800"])

    def interest_trans(self):
        return [TRAN.decode(r) for r in harness.read_seq(self.dds["TRANSACT"], 350)]

    def accounts(self):
        return {int(a["ACCT-ID"]): a for a in map(ACCOUNT.decode, KSDS_ACCT.dump(self.dds["ACCTFILE"]))}

    def test_return_code(self):
        self.assertRC(self.result, 0)
        self.assertIn("DISCLOSURE GROUP RECORD MISSING", self.result.stdout)
        self.assertIn("TRY WITH DEFAULT GROUP CODE", self.result.stdout)

    def test_interest_transactions_match_hand_calculation(self):
        got = [(t["TRAN-ID"], t["TRAN-CARD-NUM"], t["TRAN-AMT"]) for t in self.interest_trans()]
        self.assertEqual(got, [
            ("2022071800000001", "4000000000000001", D("15.00")),
            ("2022071800000002", "4000000000000002", D("5.00")),
            ("2022071800000003", "4000000000000002", D("3.33")),
            ("2022071800000004", "4000000000000003", D("18.51")),
        ])

    def test_interest_transaction_attributes(self):
        for t in self.interest_trans():
            self.assertEqual((t["TRAN-TYPE-CD"], t["TRAN-CAT-CD"], t["TRAN-SOURCE"]),
                             ("01", D(5), "System"))
            self.assertTrue(t["TRAN-DESC"].startswith("Int. for a/c "))

    def test_total_interest(self):
        self.assertEqual(sum(t["TRAN-AMT"] for t in self.interest_trans()), D("41.84"))

    def test_account_balances_include_interest_and_cycle_reset(self):
        accts = self.accounts()
        self.assertEqual(accts[10000000001]["ACCT-CURR-BAL"], D("1015.00"))
        self.assertEqual(accts[10000000002]["ACCT-CURR-BAL"], D("4908.33"))
        for acct_id in (10000000001, 10000000002):
            self.assertEqual(accts[acct_id]["ACCT-CURR-CYC-CREDIT"], D(0))
            self.assertEqual(accts[acct_id]["ACCT-CURR-CYC-DEBIT"], D(0))

    @unittest.expectedFailure
    def test_last_account_in_file_is_updated(self):
        """KNOWN DEFECT: 1050-UPDATE-ACCOUNT runs only on an account break.
        The ELSE branch meant to flush the last account is unreachable
        (PERFORM UNTIL END-OF-FILE tests before each iteration), so the last
        account never gets its interest (18.51) added.  Remove the
        expectedFailure decorator once CBACT04C is fixed."""
        self.assertEqual(self.accounts()[10000000003]["ACCT-CURR-BAL"], D("18.51"))


if __name__ == "__main__":
    unittest.main()

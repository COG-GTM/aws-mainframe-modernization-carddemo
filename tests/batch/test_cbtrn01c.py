"""CBTRN01C - daily transaction validation (card xref + account lookup).

The program does not write a reject file: it reports each transaction
whose card or account cannot be verified on SYSOUT and skips it.
"""
import unittest

from base import (KSDS_ACCT, KSDS_CARD, KSDS_CUST, KSDS_TRAN, KSDS_XREF,
                  BatchTestCase, harness)


class TestCBTRN01C(BatchTestCase):
    PROGRAM = "CBTRN01C"

    def run_validation(self, daily=None):
        KSDS_TRAN.create(self.work / "transact", [])
        dds = {
            "DALYTRAN": self.seq("dalytran", "dailytran.txt", 350, daily),
            "CUSTFILE": self.ksds(KSDS_CUST, "custdata", "customers.txt"),
            "XREFFILE": self.ksds(KSDS_XREF, "cardxref", "cardxref.txt"),
            "CARDFILE": self.ksds(KSDS_CARD, "carddata", "cards.txt"),
            "ACCTFILE": self.ksds(KSDS_ACCT, "acctdata", "accounts.txt"),
            "TRANFILE": self.work / "transact",
        }
        return harness.run(self.build(), dds)

    def test_valid_records_are_verified(self):
        result = self.run_validation(harness.read_fixture("dailytran.txt", 350)[:3])
        self.assertRC(result, 0)
        for tid in ("T000000000000001", "T000000000000002", "T000000000000003"):
            self.assertIn(tid, result.stdout)
        self.assertIn("SUCCESSFUL READ OF XREF", result.stdout)
        self.assertIn("SUCCESSFUL READ OF ACCOUNT FILE", result.stdout)
        self.assertNotIn("SKIPPING", result.stdout)
        self.assertNotIn("NOT FOUND", result.stdout)
        self.assertIn("END OF EXECUTION OF PROGRAM CBTRN01C", result.stdout)

    def test_unknown_card_is_skipped(self):
        result = self.run_validation()
        self.assertRC(result, 0)
        self.assertIn("CARD NUMBER 4999999999999999 COULD NOT BE VERIFIED. "
                      "SKIPPING TRANSACTION ID-T000000000000006", result.stdout)

    def test_missing_account_is_reported(self):
        result = self.run_validation()
        self.assertIn("ACCOUNT 10000000099 NOT FOUND", result.stdout)

    def test_valid_records_unaffected_by_invalid_ones(self):
        result = self.run_validation()
        # T1..T5 have valid card + account (CBTRN01C does not check limits
        # or expiry - that is CBTRN02C's job).
        for card in ("4000000000000001", "4000000000000002", "4000000000000003"):
            self.assertIn(f"CARD NUMBER: {card}", result.stdout)

    @unittest.expectedFailure
    def test_last_record_validated_once(self):
        """KNOWN DEFECT: after the READ hits end-of-file the main loop still
        validates the previous record again, so the last transaction is
        processed twice.  Remove expectedFailure once CBTRN01C is fixed."""
        result = self.run_validation()
        self.assertEqual(result.stdout.count("ACCOUNT 10000000099 NOT FOUND"), 1)


if __name__ == "__main__":
    unittest.main()

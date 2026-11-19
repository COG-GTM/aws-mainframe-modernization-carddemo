"""CBTRN02C - post daily transactions (POSTTRAN.jcl).

Fixture dailytran.txt holds 7 transactions:
  T1 +100.00 card1  -> posted        T5  +10.00 card3 -> reject 103 (expired)
  T2  -50.00 card1  -> posted        T6   +5.00 ????  -> reject 100 (no xref)
  T3  +25.50 card1  -> posted        T7   +5.00 card9 -> reject 101 (no acct)
  T4 +200.00 card2  -> reject 102 (4900 + 200 > 5000 limit)
"""
import unittest
from decimal import Decimal as D

from base import (ACCOUNT, KSDS_ACCT, KSDS_TCATBAL, KSDS_TRAN, KSDS_XREF,
                  TCATBAL, TRAN, BatchTestCase, harness)


class TestCBTRN02C(BatchTestCase):
    PROGRAM = "CBTRN02C"

    def run_posting(self, daily_records=None):
        dds = {
            "DALYTRAN": self.seq("dalytran", "dailytran.txt", 350, daily_records),
            "TRANFILE": self.work / "transact",
            "XREFFILE": self.ksds(KSDS_XREF, "cardxref", "cardxref.txt"),
            "DALYREJS": self.work / "dalyrejs",
            "ACCTFILE": self.ksds(KSDS_ACCT, "acctdata", "accounts.txt"),
            "TCATBALF": self.ksds(KSDS_TCATBAL, "tcatbal", "tcatbal_posting.txt"),
        }
        result = harness.run(self.build(), dds)
        return result, dds

    def accounts(self, dds):
        return {int(a["ACCT-ID"]): a for a in map(ACCOUNT.decode, KSDS_ACCT.dump(dds["ACCTFILE"]))}

    def test_mixed_batch_return_code_and_counts(self):
        result, _ = self.run_posting()
        self.assertRC(result, 4)  # any reject -> RC 4
        self.assertIn("TRANSACTIONS PROCESSED :000000007", result.stdout)
        self.assertIn("TRANSACTIONS REJECTED  :000000004", result.stdout)

    def test_account_balances_updated(self):
        _, dds = self.run_posting()
        accts = self.accounts(dds)
        a1 = accts[10000000001]
        self.assertEqual(a1["ACCT-CURR-BAL"], D("1075.50"))       # 1000+100-50+25.50
        self.assertEqual(a1["ACCT-CURR-CYC-CREDIT"], D("125.50"))  # 100+25.50
        self.assertEqual(a1["ACCT-CURR-CYC-DEBIT"], D("-50.00"))
        # Rejected transactions must not touch their accounts.
        self.assertEqual(accts[10000000002]["ACCT-CURR-BAL"], D("4900.00"))
        self.assertEqual(accts[10000000002]["ACCT-CURR-CYC-CREDIT"], D("4900.00"))
        self.assertEqual(accts[10000000003]["ACCT-CURR-BAL"], D("0"))

    def test_category_balances_updated_and_created(self):
        _, dds = self.run_posting()
        bals = {(int(r["TRANCAT-ACCT-ID"]), r["TRANCAT-TYPE-CD"], int(r["TRANCAT-CD"])):
                r["TRAN-CAT-BAL"]
                for r in map(TCATBAL.decode, KSDS_TCATBAL.dump(dds["TCATBALF"]))}
        self.assertEqual(bals, {
            (10000000001, "01", 1): D("625.50"),   # existing 500 + 100 + 25.50
            (10000000001, "02", 1): D("-50.00"),   # new record created
        })

    def test_posted_transactions_written(self):
        _, dds = self.run_posting()
        trans = [TRAN.decode(r) for r in KSDS_TRAN.dump(dds["TRANFILE"])]
        self.assertEqual([t["TRAN-ID"] for t in trans],
                         ["T000000000000001", "T000000000000002", "T000000000000003"])
        self.assertEqual(sum(t["TRAN-AMT"] for t in trans), D("75.50"))
        for t in trans:
            self.assertRegex(t["TRAN-PROC-TS"], r"^\d{4}-\d\d-\d\d-\d\d\.\d\d\.\d\d\.\d{6}$")
            self.assertEqual(t["TRAN-CARD-NUM"], "4000000000000001")

    def test_rejects_written_with_reason_codes(self):
        _, dds = self.run_posting()
        rejects = harness.read_seq(dds["DALYREJS"], 430)
        got = [(r[:16], r[350:354], r[354:430].strip()) for r in rejects]
        self.assertEqual(got, [
            ("T000000000000004", "0102", "OVERLIMIT TRANSACTION"),
            ("T000000000000005", "0103", "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"),
            ("T000000000000006", "0100", "INVALID CARD NUMBER FOUND"),
            ("T000000000000007", "0101", "ACCOUNT RECORD NOT FOUND"),
        ])
        # The rejected record is carried verbatim in the first 350 bytes.
        daily = harness.read_fixture("dailytran.txt", 350)
        self.assertEqual(rejects[0][:350], daily[3])

    def test_all_valid_batch_returns_zero(self):
        daily = harness.read_fixture("dailytran.txt", 350)[:3]
        result, dds = self.run_posting(daily)
        self.assertRC(result, 0)
        self.assertEqual(harness.read_seq(dds["DALYREJS"], 430), [])
        self.assertIn("TRANSACTIONS REJECTED  :000000000", result.stdout)


if __name__ == "__main__":
    unittest.main()

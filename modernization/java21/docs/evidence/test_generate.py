import copy
import csv
import io
import unittest
from typing import Any
from unittest.mock import patch

import generate as evidence


class ComparisonTest(unittest.TestCase):
    def compare(self, expected, actual, fields=None):
        stream = io.StringIO()
        result = evidence.compare(
            csv.writer(stream),
            "test",
            "program",
            "mixed",
            "file",
            expected,
            actual,
            fields,
        )
        return result, list(csv.reader(io.StringIO(stream.getvalue())))

    def test_every_record_and_field_including_fillers(self):
        result, ledger = self.compare(
            b"12AB34CD", b"12AB34CD", [("id", 2), ("filler", 2)]
        )
        self.assertEqual({"match": 2}, result["records"])
        self.assertEqual({"match": 4}, result["fields"])
        self.assertEqual(6, len(ledger))
        self.assertTrue(all(row[6] == "match" and row[7] == row[8] for row in ledger))

    def test_one_byte_in_filler_is_not_hidden(self):
        result, ledger = self.compare(b"12AB", b"12AC", [("id", 2), ("filler", 2)])
        self.assertEqual({"unresolved": 1}, result["records"])
        self.assertEqual({"match": 1, "unresolved": 1}, result["fields"])
        self.assertEqual(
            ["@record", "filler"], [d["field"] for d in result["differences"]]
        )
        self.assertEqual("4142", ledger[-1][7])
        self.assertEqual("4143", ledger[-1][8])

    def test_no_mismatch_limit(self):
        result, _ = self.compare(b"A" * 101, b"B" * 101, [("byte", 1)])
        self.assertEqual(202, len(result["differences"]))

    def test_missing_record_and_every_field_are_unresolved(self):
        result, ledger = self.compare(b"1234", b"12", [("id", 1), ("filler", 1)])
        self.assertEqual(2, result["expected_records"])
        self.assertEqual(1, result["actual_records"])
        self.assertEqual(3, len(result["differences"]))
        self.assertTrue(
            all(row[8] == "" and row[6] == "unresolved" for row in ledger[-3:])
        )

    def test_extra_record_not_dropped(self):
        result, _ = self.compare(b"12", b"1234", [("id", 2)])
        self.assertEqual(2, len(result["differences"]))
        self.assertIsNone(result["differences"][0]["expected_hex"])

    def test_partial_record_not_dropped_even_when_equal(self):
        result, _ = self.compare(b"123", b"123", [("id", 2)])
        self.assertEqual(2, result["actual_records"])
        self.assertEqual(1, result["partial_actual_bytes"])
        self.assertEqual(1, result["partial_expected_bytes"])

    def test_raw_overpunch_and_high_bytes_not_normalized(self):
        result, _ = self.compare(b"0{\xff", b"0}\xff", [("amount", 2), ("filler", 1)])
        self.assertEqual("307b", result["differences"][1]["expected_hex"])
        self.assertEqual("307d", result["differences"][1]["actual_hex"])

    def test_text_final_newline_is_compared(self):
        result, _ = self.compare(b"line\n", b"line")
        self.assertEqual({"unresolved": 1}, result["records"])

    def test_empty_output(self):
        result, ledger = self.compare(b"", b"", [("id", 1)])
        self.assertEqual(0, result["actual_records"])
        self.assertEqual([], ledger)
        self.assertEqual(evidence.digest(b""), result["actual_sha256"])

    def test_all_codec_layout_bytes_are_covered_and_reject_is_expanded(self):
        expected = {
            "ACCTFILE": 300,
            "DISCGRP": 50,
            "TCATBALF": 50,
            "TRANFILE": 350,
            "XREFFILE": 50,
            "SYSTRAN": 350,
            "DALYREJS": 430,
        }
        for name, size in expected.items():
            self.assertEqual(
                size, sum(width for _, width in evidence.layout(evidence.TYPES[name]))
            )
        reject = evidence.layout("RejectRecord")
        self.assertIn(("transaction.amount", 11), reject)
        self.assertIn(("reasonCode", 4), reject)

    def test_rejects_are_listed_individually(self):
        payload = b"TX".ljust(350, b" ") + b"0103" + b"expired".ljust(76, b" ")
        rejects = evidence.reject_rows(payload + payload)
        self.assertEqual([1, 2], [r["index"] for r in rejects])
        self.assertEqual(["0103", "0103"], [r["reason_code"] for r in rejects])
        self.assertEqual(429, evidence.reject_rows(payload[:-1])[0]["partial_bytes"])
        self.assertEqual(
            "\\xff" + "X" * 15,
            evidence.reject_rows(b"\xff" + b"X" * 429)[0]["transaction_id"],
        )


class GateTest(unittest.TestCase):
    def test_no_test_failure_skip_disabled_unresolved_partial_or_bad_exit_can_pass(
        self,
    ):
        good = {
            "maven_exit": 0,
            "tests": [{"status": "passed"}],
            "disabled": [],
            "classification": {"match": 10},
            "unclassified_differences": 0,
            "runs": [{"jar_exit": 4, "expected_exit": 4, "posting_counts_match": True}],
            "comparisons": [{"partial_actual_bytes": 0, "partial_expected_bytes": 0}],
        }
        self.assertTrue(evidence.gate(good))
        changes: list[dict[str, Any]] = [
            {"maven_exit": 1},
            {"tests": [{"status": "skipped"}]},
            {"tests": [{"status": "failure"}]},
            {"disabled": [{"reason": "pending"}]},
            {"classification": {"unresolved": 1}},
            {"unclassified_differences": 1},
            {"runs": [{"jar_exit": 0, "expected_exit": 4}]},
            {
                "runs": [
                    {"jar_exit": 4, "expected_exit": 4, "posting_counts_match": False}
                ]
            },
            {"comparisons": [{"partial_actual_bytes": 1, "partial_expected_bytes": 0}]},
        ]
        for change in changes:
            bad = copy.deepcopy(good)
            bad.update(change)
            self.assertFalse(evidence.gate(bad), change)

    def test_unknown_catalog_rule_fails_closed(self):
        with (
            patch.object(evidence, "RULE_TESTS", {}),
            self.assertRaisesRegex(RuntimeError, "Catalog changed"),
        ):
            evidence.catalog([])

    def test_missing_test_selector_fails_closed(self):
        with self.assertRaisesRegex(RuntimeError, "no executed result"):
            evidence.catalog([])


if __name__ == "__main__":
    unittest.main()

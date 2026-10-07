"""Unit tests for the harness.  Run from the repository root:

    python3 -m unittest discover -s test-harness/tests -v
"""
import json
import os
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
HARNESS = os.path.dirname(HERE)
REPO = os.path.dirname(HARNESS)
sys.path.insert(0, HARNESS)

import compare  # noqa: E402
import reconcile  # noqa: E402
from copybook import CopybookError, layout, load_layout, parse_file, parse_text  # noqa: E402
from records import (RecordError, decode_binary, decode_comp3, decode_file, decode_record,  # noqa: E402
                     decode_zoned, encode_binary, encode_comp3, encode_record, encode_zoned, split_records)

CPY = os.path.join(REPO, "app", "cpy")
CBL = os.path.join(REPO, "app", "cbl")
ASCII = os.path.join(REPO, "app", "data", "ASCII")
EBCDIC = os.path.join(REPO, "app", "data", "EBCDIC")
GOLD = os.path.join(REPO, "golden-files")


class CopybookTests(unittest.TestCase):
    def test_account_record_layout(self):
        rec = load_layout(os.path.join(CPY, "CVACT01Y.cpy"))
        self.assertEqual(rec.length, 300)
        f = rec.find("ACCT-CURR-BAL")
        self.assertEqual((f.offset, f.length, f.type, f.digits, f.scale, f.sign), (12, 12, "numeric", 12, 2, True))
        self.assertEqual(rec.find("ACCT-GROUP-ID").offset, 112)
        self.assertTrue(rec.children[-1].is_filler)

    def test_program_records_comp3_and_occurs(self):
        prog = parse_file(os.path.join(CBL, "CBACT01C.cbl"))
        out = layout(prog, "OUT-ACCT-REC")
        self.assertEqual(out.length, 107)
        dbt = out.find("OUT-ACCT-CURR-CYC-DEBIT")
        self.assertEqual((dbt.usage, dbt.length, dbt.offset), ("COMP-3", 7, 90))
        arr = layout(prog, "ARR-ARRAY-REC")
        self.assertEqual(arr.length, 110)
        self.assertEqual(arr.find("ARR-ACCT-BAL").occurs, 5)
        self.assertEqual(arr.find("ARR-FILLER").offset, 106)
        self.assertEqual(layout(prog, "VBRC-REC1").length, 12)
        self.assertEqual(layout(prog, "VBRC-REC2").length, 39)

    def test_redefines_and_88_levels(self):
        rec = load_layout(os.path.join(CPY, "CODATECN.cpy"))
        self.assertEqual(rec.length, 1 + 20 + 1 + 20 + 38)
        self.assertEqual(rec.find("CODATECN-2INP").offset, rec.find("CODATECN-INP-DATE").offset)
        self.assertEqual(rec.find("CODATECN-ERROR-MSG").offset, 42)
        self.assertIsNone(rec.find("YYYYMMDD-IN"))

    def test_other_copybooks(self):
        for name, length in (("CVTRA06Y", 350), ("CVACT03Y", 50), ("CVACT02Y", 150), ("CVCUS01Y", 500), ("CVTRA05Y", 350)):
            self.assertEqual(load_layout(os.path.join(CPY, name + ".cpy")).length, length, name)

    def test_binary_sizes(self):
        roots = parse_text("       01  A.\n           05 B PIC 9(4) BINARY.\n           05 C PIC S9(9) COMP.\n"
                           "           05 D PIC S9(18) COMP.\n           05 E PIC S9(7)V99 COMP-3.\n")
        self.assertEqual([c.length for c in roots[0].children], [2, 4, 8, 5])

    def test_unknown_token_is_an_error(self):
        with self.assertRaises(CopybookError):
            parse_text("       01  A PIC X(3) BOGUS.\n")


class RecordTests(unittest.TestCase):
    def test_zoned_overpunch(self):
        self.assertEqual(decode_zoned(b"00000001940{", 2, True, False), "194.00")
        self.assertEqual(decode_zoned(b"00000010250}", 2, True, False), "-1025.00")
        self.assertEqual(decode_zoned(b"0000005047G", 2, True, False), "504.77")
        self.assertEqual(decode_zoned(b"0000009190}", 2, True, False), "-919.00")
        self.assertEqual(decode_zoned(b"0000000678R", 2, True, False), "-67.89")
        self.assertEqual(decode_zoned(b"00000000001", 0, False, False), "1")
        self.assertEqual(decode_zoned(b"000000000000", 2, True, False), "0.00")  # unsigned zone on signed field
        self.assertEqual(decode_zoned(b"0000000678y", 2, True, False), "-67.89")  # GnuCOBOL ASCII negative
        self.assertEqual(encode_zoned("194.00", 12, 2, True, False), b"00000001940{")
        self.assertEqual(encode_zoned("-1025.00", 12, 2, True, False), b"00000010250}")
        self.assertEqual(encode_zoned("504.77", 11, 2, True, False), b"0000005047G")
        self.assertEqual(encode_zoned("7", 11, 0, False, False), b"00000000007")

    def test_zoned_ebcdic(self):
        self.assertEqual(decode_zoned(bytes.fromhex("F0F0F1F9F4C0"), 2, True, True), "19.40")
        self.assertEqual(decode_zoned(bytes.fromhex("F0F0F1F9F4D0"), 2, True, True), "-19.40")
        self.assertEqual(encode_zoned("-19.40", 6, 2, True, True), bytes.fromhex("F0F0F1F9F4D0"))

    def test_comp3(self):
        self.assertEqual(decode_comp3(bytes.fromhex("0000000252500C"), 2), "2525.00")
        self.assertEqual(decode_comp3(bytes.fromhex("0000000250000D"), 2), "-2500.00")
        self.assertEqual(decode_comp3(bytes.fromhex("0000000000000C"), 2), "0.00")
        self.assertEqual(encode_comp3("2525.00", 12, 2, True), bytes.fromhex("0000000252500C"))
        self.assertEqual(encode_comp3("-2500.00", 12, 2, True), bytes.fromhex("0000000250000D"))

    def test_roundtrip_sample_files(self):
        cases = [("CVACT01Y.cpy", "acctdata.txt"), ("CVTRA06Y.cpy", "dailytran.txt"),
                 ("CVACT03Y.cpy", "cardxref.txt"), ("CVACT02Y.cpy", "carddata.txt"), ("CVCUS01Y.cpy", "custdata.txt")]
        for cpy, data in cases:
            lay = load_layout(os.path.join(CPY, cpy))
            with open(os.path.join(ASCII, data), "rb") as fh:
                raws = split_records(fh.read(), lay.length, "line")
            self.assertEqual(len(raws), 50 if data != "dailytran.txt" else 300, data)
            for raw in raws:
                self.assertEqual(encode_record(decode_record(raw, lay, include_filler=True), lay), raw, data)

    def test_ebcdic_matches_ascii_for_dailytran_and_xref(self):
        for cpy, asc, ebc in (("CVTRA06Y.cpy", "dailytran.txt", "AWS.M2.CARDDEMO.DALYTRAN.PS"),
                              ("CVACT03Y.cpy", "cardxref.txt", "AWS.M2.CARDDEMO.CARDXREF.PS")):
            lay = load_layout(os.path.join(CPY, cpy))
            a = decode_file(os.path.join(ASCII, asc), lay, "line")
            e = decode_file(os.path.join(EBCDIC, ebc), lay, "fixed", ebcdic=True)
            self.assertEqual(a, e, cpy)

    def test_trailing_spaces_are_kept(self):
        lay = load_layout(os.path.join(CPY, "CVACT01Y.cpy"))
        rec = decode_record(b"00000000001Y" + b"0" * 12 * 3 + b"2014-11-20" * 3 + b"0" * 24 + b"A000000000" + b" " * 188, lay)
        self.assertEqual(rec["ACCT-GROUP-ID"], " " * 10)
        self.assertEqual(rec["ACCT-OPEN-DATE"], "2014-11-20")


class EncodeValidationTests(unittest.TestCase):
    def test_binary_rejects_scale_overflow_and_sign(self):
        self.assertEqual(decode_binary(encode_binary("1.23", 4, 2, True), 2, True), "1.23")
        with self.assertRaises(RecordError):
            encode_binary("1.239", 4, 2, True)          # more decimals than the scale -> no silent truncation
        with self.assertRaises(RecordError):
            encode_binary("-1", 2, 0, False)            # negative into an unsigned field
        with self.assertRaises(RecordError):
            encode_binary("65536", 2, 0, False)         # does not fit in 2 bytes
        with self.assertRaises(RecordError):
            encode_binary("32768", 2, 0, True)

    def test_comp3_rejects_digit_overflow(self):
        # PIC S9(10)V99 = 12 digits -> 7 bytes with 13 nibbles; 13 digits must not slip in
        self.assertEqual(len(encode_comp3("9999999999.99", 12, 2, True)), 7)
        with self.assertRaises(RecordError):
            encode_comp3("10000000000.00", 12, 2, True)
        # odd number of digits: 3 digits fit, 4 do not
        self.assertEqual(encode_comp3("999", 3, 0, True), b"\x99\x9c")
        with self.assertRaises(RecordError):
            encode_comp3("1000", 3, 0, True)
        with self.assertRaises(RecordError):
            encode_comp3("1.239", 12, 2, True)

    def test_invalid_comp3_marker_round_trip(self):
        from records import INVALID_PREFIX, encode_record
        rec = layout(parse_file(os.path.join(CBL, "CBACT01C.cbl")), "OUT-ACCT-REC")
        with open(os.path.join(GOLD, "CBACT01C", "raw", "OUTFILE"), "rb") as fh:
            raw = bytearray(fh.read(rec.length))
        f = rec.find("OUT-ACCT-CURR-CYC-DEBIT")
        raw[f.offset:f.offset + f.length] = b"\x00" * f.length      # never-assigned storage under GnuCOBOL
        with self.assertRaises(RecordError):
            decode_record(bytes(raw), rec)
        d = decode_record(bytes(raw), rec, lenient=True)
        self.assertEqual(d["OUT-ACCT-CURR-CYC-DEBIT"], INVALID_PREFIX + "00" * f.length)
        self.assertEqual(encode_record(d, rec), bytes(raw))


class CompareTests(unittest.TestCase):
    def test_reports_every_mismatch(self):
        exp = [{"ACCT-ID": "1", "BAL": "1.00", "ARR": [{"X": "a"}, {"X": "b"}]},
               {"ACCT-ID": "2", "BAL": "2.00", "ARR": [{"X": "a"}, {"X": "b"}]}]
        act = [{"ACCT-ID": "1", "BAL": "1.0", "ARR": [{"X": "a"}, {"X": "c"}]},
               {"ACCT-ID": "3", "BAL": "2.00", "ARR": [{"X": "a"}, {"X": "b"}]}]
        mm = compare.compare_records(exp, act, key="ACCT-ID")
        fields = {(m["record_key"], m["field"]) for m in mm}
        self.assertIn(("1", "BAL"), fields)          # scale difference is a mismatch
        self.assertIn(("1", "ARR[2].X"), fields)
        self.assertIn(("2", "<record>"), fields)
        self.assertIn(("3", "<record>"), fields)
        self.assertEqual(compare.compare_records(exp, exp, key="ACCT-ID"), [])
        self.assertEqual([m for m in compare.compare_records(exp[:1], act[:1], key="ACCT-ID", numeric_value=True)
                          if m["field"] == "BAL"], [])


class CompareFlattenTests(unittest.TestCase):
    def test_empty_structures_are_not_collapsed(self):
        self.assertEqual(compare.flatten({"A": [], "B": {}, "C": [{"D": []}]}), {"A": [], "B": {}, "C[1].D": []})
        self.assertEqual(compare.flatten({"A": [1]}), {"A[1]": 1})
        self.assertEqual(compare.flatten([]), {})
        exp = [{"K": "1", "ARR": []}]
        self.assertEqual([m["field"] for m in compare.compare_records(exp, [{"K": "1", "ARR": {}}], key="K")], ["ARR"])
        self.assertEqual([m["field"] for m in compare.compare_records(exp, [{"K": "1"}], key="K")], ["ARR"])
        self.assertEqual(compare.compare_records(exp, [{"K": "1", "ARR": []}], key="K"), [])


class ReconcileTests(unittest.TestCase):
    def _load(self, *parts):
        with open(os.path.join(GOLD, *parts), encoding="utf-8") as fh:
            return json.load(fh)

    def test_goldens_pass(self):
        for job, d in (("cbact01c", "CBACT01C"), ("cbtrn01c", "CBTRN01C"),
                       ("cbtrn01c", os.path.join("CBTRN01C", "synthetic-rejections"))):
            res = reconcile.run(job, os.path.join(GOLD, d), write=False)
            self.assertEqual(res["summary"]["status"], "PASS", json.dumps(res["summary"]))
            self.assertEqual(res, self._load(d, "reconciliation.json"), "stored reconciliation.json is stale for " + d)

    def test_cbact01c_detects_dropped_record_and_bad_total(self):
        acct = self._load("CBACT01C", "input-acctdata.json")
        out = self._load("CBACT01C", "outfile.json")
        arr = self._load("CBACT01C", "arryfile.json")
        vb = self._load("CBACT01C", "vbrcfile.json")
        res = reconcile.reconcile_cbact01c(acct, out[:-1], arr, vb)
        self.assertIn("CBACT01C-COUNT-01", res["summary"]["failed_ids"])
        self.assertIn("CBACT01C-XREF-01", res["summary"]["failed_ids"])
        bad = json.loads(json.dumps(out))
        bad[0]["OUT-ACCT-CURR-CYC-DEBIT"] = "0.00"
        bad[1]["OUT-ACCT-REISSUE-DATE"] = "2025-05-20"
        res = reconcile.reconcile_cbact01c(acct, bad, arr, vb)
        self.assertEqual(set(res["summary"]["failed_ids"]),
                         {"CBACT01C-TOTAL-CYC-DEBIT", "CBACT01C-FIELD-CYC-DEBIT", "CBACT01C-FIELD-REISSUE-DATE"})

    def test_cbact01c_mixed_debit_models_legacy_carry_over(self):
        d = os.path.join("CBACT01C", "synthetic-mixed-debit")
        acct, out = self._load(d, "input-acctdata.json"), self._load(d, "outfile.json")
        arr, vb = self._load(d, "arryfile.json"), self._load(d, "vbrcfile.json")
        self.assertEqual([a["ACCT-CURR-CYC-DEBIT"] for a in acct], ["10.00", "0.00", "120.50", "-75.25", "0.00"])
        # what the real program wrote: never-assigned storage, then 2525.00 carried through non-zero inputs
        self.assertEqual([o["OUT-ACCT-CURR-CYC-DEBIT"] for o in out],
                         ["INVALID-COMP-3:00000000000000", "2525.00", "2525.00", "2525.00", "2525.00"])
        res = reconcile.reconcile_cbact01c(acct, out, arr, vb)
        self.assertEqual(res["summary"]["status"], "PASS", json.dumps(res["summary"]))
        self.assertEqual(res, self._load(d, "reconciliation.json"), "stored reconciliation.json is stale for " + d)
        tot = next(c for c in res["checks"] if c["id"] == "CBACT01C-TOTAL-CYC-DEBIT")
        self.assertEqual(tot["expected"], "10100.00")                      # 4 defined rows x 2525.00
        self.assertEqual(tot["detail"]["business_intent_total_if_nonzero_inputs_were_copied"], "5105.25")
        self.assertEqual(len(tot["detail"]["undefined_rows"]), 1)
        # a port that "fixes" the bug by copying the input debit must be flagged
        ported = json.loads(json.dumps(out))
        ported[0]["OUT-ACCT-CURR-CYC-DEBIT"] = "10.00"
        ported[2]["OUT-ACCT-CURR-CYC-DEBIT"] = "120.50"
        ported[3]["OUT-ACCT-CURR-CYC-DEBIT"] = "-75.25"
        res = reconcile.reconcile_cbact01c(acct, ported, arr, vb)
        self.assertEqual(set(res["summary"]["failed_ids"]), {"CBACT01C-TOTAL-CYC-DEBIT", "CBACT01C-FIELD-CYC-DEBIT"})
        field = next(c for c in res["checks"] if c["id"] == "CBACT01C-FIELD-CYC-DEBIT")
        self.assertEqual([m["acct_id"] for m in field["detail"]["mismatches"]], [out[2]["OUT-ACCT-ID"], out[3]["OUT-ACCT-ID"]])

    def test_cbtrn01c_detects_wrong_outcome(self):
        d = os.path.join("CBTRN01C", "synthetic-rejections")
        tran, outc = self._load(d, "input-dailytran.json"), self._load(d, "outcomes.json")
        xref, acct = self._load(d, "input-cardxref.json"), self._load(d, "input-acctdata.json")
        self.assertEqual([o["outcome"] for o in outc], ["VERIFIED", "CARD_NOT_FOUND", "ACCOUNT_NOT_FOUND"])
        bad = json.loads(json.dumps(outc))
        bad[1]["outcome"] = "VERIFIED"
        res = reconcile.reconcile_cbtrn01c(tran, bad, xref, acct)
        self.assertIn("CBTRN01C-XREF-01", res["summary"]["failed_ids"])
        res = reconcile.reconcile_cbtrn01c(tran, outc[1:], xref, acct)
        self.assertIn("CBTRN01C-COUNT-01", res["summary"]["failed_ids"])
        self.assertIn("CBTRN01C-COUNT-03", res["summary"]["failed_ids"])

    def test_cbtrn01c_detects_swapped_card_numbers(self):
        d = os.path.join("CBTRN01C", "synthetic-rejections")
        tran, outc = self._load(d, "input-dailytran.json"), self._load(d, "outcomes.json")
        xref, acct = self._load(d, "input-cardxref.json"), self._load(d, "input-acctdata.json")
        swapped = json.loads(json.dumps(outc))
        swapped[0]["card_num"], swapped[1]["card_num"] = swapped[1]["card_num"], swapped[0]["card_num"]
        res = reconcile.reconcile_cbtrn01c(tran, swapped, xref, acct)
        self.assertIn("CBTRN01C-COUNT-03", res["summary"]["failed_ids"])
        c03 = next(c for c in res["checks"] if c["id"] == "CBTRN01C-COUNT-03")
        self.assertEqual(c03["actual"], 2)
        for m in c03["detail"]["mismatches"]:
            self.assertEqual(m["input"]["tran_id"], m["outcome"]["tran_id"])      # ids still line up ...
            self.assertNotEqual(m["input"]["card_num"], m["outcome"]["card_num"])  # ... only the cards are swapped

    def test_cbtrn01c_display_free_golden_skips_count_04(self):
        import tempfile
        import shutil
        src = os.path.join(GOLD, "CBTRN01C", "synthetic-rejections")
        tmp = tempfile.mkdtemp()
        try:
            for f in ("input-dailytran.json", "outcomes.json", "input-cardxref.json", "input-acctdata.json"):
                shutil.copy(os.path.join(src, f), tmp)
            res = reconcile.run("cbtrn01c", tmp, write=False)
        finally:
            shutil.rmtree(tmp)
        self.assertEqual(res["summary"]["status"], "PASS")
        self.assertEqual(res["summary"]["checks"], 11)
        self.assertEqual(res["summary"]["passed"], 10)
        self.assertEqual(res["summary"]["failed"], 0)
        self.assertEqual(res["summary"]["skipped"], 1)
        self.assertEqual(res["summary"]["skipped_ids"], ["CBTRN01C-COUNT-04"])
        c04 = next(c for c in res["checks"] if c["id"] == "CBTRN01C-COUNT-04")
        self.assertEqual(c04["status"], "SKIP")
        self.assertEqual(c04["skip_reason"], "display.txt not present in the golden directory")
        with_display = reconcile.run("cbtrn01c", src, write=False)
        self.assertEqual(with_display["summary"]["skipped"], 0)


class GoldenShapeTests(unittest.TestCase):
    def test_golden_counts(self):
        with open(os.path.join(GOLD, "CBACT01C", "outfile.json")) as fh:
            out = json.load(fh)
        self.assertEqual(len(out), 50)
        self.assertEqual(out[0]["OUT-ACCT-REISSUE-DATE"], "20250520  ")
        self.assertTrue(all(o["OUT-ACCT-CURR-CYC-DEBIT"] == "2525.00" for o in out))
        self.assertEqual(os.path.getsize(os.path.join(GOLD, "CBACT01C", "raw", "OUTFILE")), 50 * 107)
        self.assertEqual(os.path.getsize(os.path.join(GOLD, "CBACT01C", "raw", "ARRYFILE")), 50 * 110)
        self.assertEqual(os.path.getsize(os.path.join(GOLD, "CBACT01C", "raw", "VBRCFILE")), 50 * (4 + 12 + 4 + 39))
        with open(os.path.join(GOLD, "CBTRN01C", "outcomes.json")) as fh:
            outc = json.load(fh)
        self.assertEqual(len(outc), 300)
        self.assertTrue(all(o["outcome"] == "VERIFIED" for o in outc))


if __name__ == "__main__":
    unittest.main()

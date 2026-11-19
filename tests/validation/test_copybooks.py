#!/usr/bin/env python3
"""Copybook / record-layout validation.

Parses every copybook in app/cpy and app/cpy-bms with tests/lib/copybook.py
and checks:
  * every PIC clause is well formed;
  * VSAM/QSAM record copybooks add up to their documented record lengths;
  * REDEFINES name an earlier sibling at the same level and are not larger
    than the item they redefine; OCCURS counts/DEPENDING ON are sane;
  * app/data/ASCII sample files have exactly the copybook record length;
  * FD records in the batch programs match the dataset record lengths.
Run: python3 tests/validation/test_copybooks.py -v
"""
import re
import sys
import unittest
from pathlib import Path

TESTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(TESTS / "lib"))
import copybook  # noqa: E402

REPO = TESTS.parent
CPY_DIRS = [REPO / "app" / "cpy", REPO / "app" / "cpy-bms"]
CBL = REPO / "app" / "cbl"
DATA = REPO / "app" / "data" / "ASCII"

# (copybook, 01-level record) -> record length (bytes), as defined by the
# IDCAMS DEFINE CLUSTER RECORDSIZE / DCB LRECL in app/jcl.
EXPECTED_LENGTHS = {
    ("CVACT01Y", "ACCOUNT-RECORD"): 300,
    ("CVACT02Y", "CARD-RECORD"): 150,
    ("CVACT03Y", "CARD-XREF-RECORD"): 50,
    ("CVCUS01Y", "CUSTOMER-RECORD"): 500,
    ("CUSTREC", "CUSTOMER-RECORD"): 500,
    ("CVTRA01Y", "TRAN-CAT-BAL-RECORD"): 50,
    ("CVTRA02Y", "DIS-GROUP-RECORD"): 50,
    ("CVTRA03Y", "TRAN-TYPE-RECORD"): 60,
    ("CVTRA04Y", "TRAN-CAT-RECORD"): 60,
    ("CVTRA05Y", "TRAN-RECORD"): 350,
    ("CVTRA06Y", "DALYTRAN-RECORD"): 350,
    ("COSTM01", "TRNX-RECORD"): 350,
    ("CSUSR01Y", "SEC-USER-DATA"): 80,
    ("CVEXPORT", "EXPORT-RECORD"): 500,
    ("COCOM01Y", "CARDDEMO-COMMAREA"): 160,
}

# Report lines written to 133-byte (FBA) TRANREPT must fit the record.
REPORT_COPYBOOK, REPORT_LRECL = "CVTRA07Y", 133

# app/data/ASCII file -> copybook.  Trailing FILLER blanks may be trimmed
# in the sample files, so each line must cover every named field and not
# exceed the record length.
DATA_FILES = {
    "acctdata.txt": "CVACT01Y", "carddata.txt": "CVACT02Y",
    "cardxref.txt": "CVACT03Y", "custdata.txt": "CVCUS01Y",
    "dailytran.txt": "CVTRA06Y", "discgrp.txt": "CVTRA02Y",
    "tcatbal.txt": "CVTRA01Y", "trancatg.txt": "CVTRA04Y",
    "trantype.txt": "CVTRA03Y",
}

# Table REDEFINES that are larger than the VALUE block they overlay:
# the menu option tables declare OCCURS 9/12 but only 6/11 entries are
# populated.  Enterprise COBOL accepts this; listed so new cases fail.
KNOWN_OVERSIZED_REDEFINES = {
    ("COADM02Y.cpy", "CDEMO-ADMIN-OPTIONS"),
    ("COMEN02Y.cpy", "CDEMO-MENU-OPTIONS"),
}

# DD name -> record length used by the batch programs' FDs.
DD_LENGTHS = {
    "ACCTFILE": 300, "CARDFILE": 150, "CUSTFILE": 500, "XREFFILE": 50,
    "CARDXREF": 50, "TRANFILE": 350, "TRANSACT": 350, "DALYTRAN": 350,
    "TRNXFILE": 350, "TCATBALF": 50, "DISCGRP": 50, "TRANTYPE": 60,
    "TRANCATG": 60, "DALYREJS": 430, "TRANREPT": 133, "DATEPARM": 80,
    "STMTFILE": 80, "HTMLFILE": 100,
}

PIC_RE = re.compile(r"^(S?([AX9ZBVP0/,.+\-*$]|CR|DB)(\(\d+\))?)+$")


def copybooks():
    for d in CPY_DIRS:
        for p in sorted(d.iterdir()):
            if p.suffix.lower() == ".cpy":
                yield p


def find_copybook(stem):
    for d in CPY_DIRS:
        for p in d.iterdir():
            if p.stem.upper() == stem.upper():
                return p
    raise FileNotFoundError(stem)


class TestCopybookLayouts(unittest.TestCase):
    def test_all_copybooks_parse_with_valid_pics(self):
        for path in copybooks():
            with self.subTest(copybook=path.name):
                for root in copybook.parse_file(path):
                    for f in root.walk():
                        if f.pic:
                            self.assertRegex(f.pic, PIC_RE, f"{f.name} line {f.line}")

    def test_record_lengths(self):
        for (stem, record), expected in EXPECTED_LENGTHS.items():
            with self.subTest(copybook=stem, record=record):
                roots = {r.name: r for r in copybook.parse_file(find_copybook(stem))}
                self.assertIn(record, roots)
                self.assertEqual(roots[record].total_size, expected)

    def test_documented_recln_comments(self):
        """'(RECLN nnn)' header comments must agree with the parsed layout."""
        for path in copybooks():
            m = re.search(r"RECLN\s*[=:]?\s*(\d+)", path.read_text(errors="replace"))
            if not m:
                continue
            with self.subTest(copybook=path.name):
                roots = copybook.parse_file(path)
                self.assertEqual(roots[0].total_size, int(m.group(1)))

    def test_leaf_fields_are_contiguous(self):
        for (stem, record), expected in EXPECTED_LENGTHS.items():
            with self.subTest(copybook=stem):
                root = next(r for r in copybook.parse_file(find_copybook(stem)) if r.name == record)
                pos = 0
                for f in copybook.leaf_fields(root):
                    if f.parent and f.parent.occurs > 1:
                        continue
                    self.assertEqual(f.offset, pos, f"gap/overlap before {f.name}")
                    pos += f.total_size
                self.assertLessEqual(pos, expected)

    def test_report_lines_fit_report_record(self):
        for root in copybook.parse_file(find_copybook(REPORT_COPYBOOK)):
            with self.subTest(record=root.name):
                self.assertLessEqual(root.total_size, REPORT_LRECL)

    def test_redefines_consistency(self):
        for path in copybooks():
            roots = copybook.parse_file(path)
            for root in roots:
                for f in root.walk():
                    if not f.redefines:
                        continue
                    with self.subTest(copybook=path.name, field=f.name):
                        siblings = f.parent.children if f.parent else roots
                        idx = next(i for i, s in enumerate(siblings) if s is f)
                        earlier = [s for s in siblings[:idx] if s.name == f.redefines]
                        self.assertTrue(earlier, f"{f.name} REDEFINES unknown/later item {f.redefines}")
                        target = earlier[-1]
                        self.assertEqual(target.level, f.level)
                        if f.level > 1 and (path.name, f.name) not in KNOWN_OVERSIZED_REDEFINES:
                            self.assertLessEqual(f.total_size, target.total_size,
                                                 f"{f.name} larger than {target.name}")
                            self.assertEqual(f.offset, target.offset)

    def test_occurs_consistency(self):
        for path in copybooks():
            text = path.read_text(errors="replace").upper()
            for root in copybook.parse_file(path):
                for f in root.walk():
                    if f.occurs == 1 and not f.depending_on:
                        continue
                    with self.subTest(copybook=path.name, field=f.name):
                        self.assertGreater(f.level, 1, "OCCURS not allowed at 01/77")
                        self.assertGreaterEqual(f.occurs, 1)
                        if f.depending_on:
                            self.assertIn(f.depending_on, text)
                        if f.redefines is None and f.parent is not None:
                            self.assertEqual(f.parent.size >= f.total_size, True)


class TestSampleDataMatchesLayouts(unittest.TestCase):
    def test_ascii_data_record_lengths(self):
        for name, stem in DATA_FILES.items():
            with self.subTest(file=name):
                root = copybook.parse_file(find_copybook(stem))[0]
                reclen = root.total_size
                named_end = max(f.offset + f.total_size
                                for f in copybook.leaf_fields(root) if f.name != "FILLER")
                lines = (DATA / name).read_text(errors="replace").splitlines()
                self.assertTrue(lines)
                bad = [i + 1 for i, l in enumerate(lines)
                       if not named_end <= len(l) <= reclen]
                self.assertEqual(bad, [], f"lines not {named_end}..{reclen} bytes")


def fd_records(program_text):
    """Map DD name -> [record lengths] of the 01 records under each FD."""
    text = copybook.expand_copies(program_text, CPY_DIRS)
    dd_of = {k: v.rstrip(".") for k, v in re.findall(
        r"SELECT\s+(\S+)\s+ASSIGN\s+TO\s+([A-Z0-9-]+)", text.upper())}
    data = copybook.data_division(text)
    data = re.split(r"(?mi)^.{6} +(WORKING-STORAGE|LINKAGE|LOCAL-STORAGE) +SECTION", data)[0]
    out = {}
    for chunk in re.split(r"(?m)^(?=.{6} +FD +)", data)[1:]:
        name = chunk[7:].split()[1].rstrip(".").upper()
        records = [r for r in copybook.parse_records(chunk) if r.level == 1]
        if name in dd_of:
            out[dd_of[name]] = [r.total_size for r in records]
    return out


class TestBatchProgramFileDefinitions(unittest.TestCase):
    def test_fd_record_lengths_match_datasets(self):
        programs = [p for p in CBL.iterdir() if p.name.upper().startswith("CB")]
        self.assertTrue(programs)
        checked = 0
        for prog in sorted(programs):
            text = prog.read_text(errors="replace")
            for dd, sizes in fd_records(text).items():
                if dd not in DD_LENGTHS:
                    continue
                with self.subTest(program=prog.name, dd=dd):
                    self.assertTrue(sizes, "FD without record")
                    self.assertEqual(max(sizes), DD_LENGTHS[dd])
                    checked += 1
        self.assertGreater(checked, 20)


if __name__ == "__main__":
    unittest.main()

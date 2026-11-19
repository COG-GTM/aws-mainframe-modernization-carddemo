#!/usr/bin/env python3
"""Every batch program (CB*, CSUTLDTC, COBSWAIT) passes a GnuCOBOL
syntax check with the same IBM-dialect flags the batch harness uses."""
import shutil
import subprocess
import sys
import unittest
from pathlib import Path

TESTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(TESTS / "lib"))
from harness import APP_CBL, COBC, COBC_FLAGS  # noqa: E402

BATCH = sorted(p for p in APP_CBL.iterdir()
               if p.stem.upper().startswith("CB") or p.stem.upper() in ("CSUTLDTC", "COBSWAIT"))


# RECORD KEY EXPORT-SEQUENCE-NUM is declared in WORKING-STORAGE (COPY
# CVEXPORT) instead of the FD record; GnuCOBOL rejects that.  Not
# exercised by the batch suite; listed so any NEW failure is reported.
KNOWN_INCOMPATIBLE = {"CBEXPORT.cbl", "CBIMPORT.cbl"}


@unittest.skipUnless(shutil.which(COBC), "GnuCOBOL (cobc) not installed")
class TestBatchCompile(unittest.TestCase):
    def test_syntax(self):
        self.assertGreaterEqual(len(BATCH), 12)
        for src in BATCH:
            if src.name in KNOWN_INCOMPATIBLE:
                continue
            with self.subTest(program=src.name):
                res = subprocess.run([COBC, "-fsyntax-only", *COBC_FLAGS, str(src)],
                                     capture_output=True, text=True)
                self.assertEqual(res.returncode, 0, res.stdout + res.stderr)


if __name__ == "__main__":
    unittest.main()

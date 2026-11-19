"""Shared setup for the batch program tests."""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "lib"))

import harness  # noqa: E402,F401
from harness import CEE_STUBS, Ksds, Workdir, program_source, read_fixture  # noqa: E402
from records import (ACCOUNT, CARD, CUSTOMER, DALYTRAN, DISCGRP,  # noqa: E402,F401
                     TCATBAL, TRAN, TRANCATG, TRANTYPE, XREF)

# VSAM KSDS definitions (record length, key offset/length) matching the
# IDCAMS DEFINE CLUSTER statements in app/jcl and the programs' SELECTs.
KSDS_ACCT = Ksds(300, (0, 11))
KSDS_CARD = Ksds(150, (0, 16))
KSDS_CUST = Ksds(500, (0, 9))
KSDS_XREF = Ksds(50, (0, 16))
KSDS_XREF_AIX = Ksds(50, (0, 16), [(25, 11)])   # CXACAIX alternate index
KSDS_TCATBAL = Ksds(50, (0, 17))
KSDS_DISCGRP = Ksds(50, (0, 16))
KSDS_TRANTYPE = Ksds(60, (0, 2))
KSDS_TRANCATG = Ksds(60, (0, 6))
KSDS_TRAN = Ksds(350, (0, 16))
KSDS_TRNX = Ksds(350, (0, 32))                  # card number + tran id


class BatchTestCase(unittest.TestCase):
    PROGRAM = None

    def setUp(self):
        self.work = Workdir(self.PROGRAM or "batch")

    def tearDown(self):
        self.work.cleanup()

    def ksds(self, spec, name, fixture=None, records=None):
        path = self.work / name
        if records is None:
            records = read_fixture(fixture, spec.reclen)
        spec.create(path, records)
        return path

    def seq(self, name, fixture=None, reclen=None, records=None):
        path = self.work / name
        if records is None:
            records = read_fixture(fixture, reclen)
        harness.write_seq(path, records, reclen)
        return path

    def build(self, *extra, sources=None):
        srcs = sources or [program_source(self.PROGRAM), *extra]
        return harness.build(self.PROGRAM.lower(), [*srcs, *CEE_STUBS])

    def assertRC(self, result, rc):
        self.assertEqual(result.rc, rc, msg=repr(result))

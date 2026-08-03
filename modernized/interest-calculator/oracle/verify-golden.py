#!/usr/bin/env python3
"""Re-run the GnuCOBOL oracle and check the committed golden files still match it.

This is what makes the golden master falsifiable: it proves the files under
src/test/resources/golden are the output of the *unmodified* app/cbl/CBACT04C.cbl
and not a transcription of what the Java happens to produce.

    verify-golden.py <freshly-generated-dir> <committed-golden-dir>

TRAN-ORIG-TS / TRAN-PROC-TS (bytes 279-330 of CVTRA05Y) come from
FUNCTION CURRENT-DATE and therefore differ on every run; they are excluded from
the comparison and only checked for shape. Everything else must be identical.
"""
from __future__ import annotations

import pathlib
import re
import sys

TS_START, TS_END = 278, 330  # zero-based slice of TRAN-ORIG-TS + TRAN-PROC-TS
DB2_TS = re.compile(r"\d{4}-\d{2}-\d{2}-\d{2}\.\d{2}\.\d{2}\.\d{2}0000")


def lines(path: pathlib.Path) -> list[str]:
    return path.read_text(encoding="latin-1").splitlines()


def compare_transact(fresh: pathlib.Path, golden: pathlib.Path) -> list[str]:
    problems: list[str] = []
    new, old = lines(fresh), lines(golden)
    if len(new) != len(old):
        return [f"{golden}: {len(old)} records committed but the oracle now produces {len(new)}"]
    for i, (a, b) in enumerate(zip(new, old), start=1):
        for ts in (a[TS_START:TS_START + 26], a[TS_START + 26:TS_END]):
            if not DB2_TS.fullmatch(ts):
                problems.append(f"{fresh}: record {i} timestamp {ts!r} is not a DB2 timestamp")
        if a[:TS_START] + a[TS_END:] != b[:TS_START] + b[TS_END:]:
            problems.append(f"{golden}: record {i} differs from the oracle (timestamps excluded)")
    return problems


def main() -> int:
    fresh_dir, golden_dir = (pathlib.Path(p) for p in sys.argv[1:3])
    problems = compare_transact(fresh_dir / "transact.dat", golden_dir / "transact.dat")
    if lines(fresh_dir / "acctdata-after.dat") != lines(golden_dir / "acctdata-after.dat"):
        problems.append(f"{golden_dir / 'acctdata-after.dat'}: differs from the oracle")
    for problem in problems:
        print(f"MISMATCH {problem}")
    if problems:
        return 1
    print(f"OK {golden_dir} still matches a fresh GnuCOBOL run of app/cbl/CBACT04C.cbl")
    return 0


if __name__ == "__main__":
    sys.exit(main())

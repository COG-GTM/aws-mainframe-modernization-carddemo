#!/usr/bin/env python3
"""Fixed-length record helpers for the CBTRN02C golden-master harness.

prep <ascii|ebcdic> <repo-root> <out-dir>
    Writes DALYTRAN/ACCTDATA/CARDXREF/TCATBAL as fixed-length, newline-free
    ASCII files (the form GnuCOBOL reads for ORGANIZATION SEQUENTIAL).
    EBCDIC seeds are decoded with CP037, which maps zoned-decimal sign
    overpunches (X'C0'..X'D9') onto the same '{A..I}J..R' characters the
    ASCII seeds use.

lines <in-file> <reclen> <out-file> [mask-from mask-len]
    Splits a fixed-length file into one record per line (no trimming),
    optionally masking a column range (used for the wall-clock TRAN-PROC-TS).
"""
import sys
from pathlib import Path

FILES = {
    # name: (ascii seed, ebcdic seed, record length)
    "dalytran": ("dailytran.txt", "AWS.M2.CARDDEMO.DALYTRAN.PS", 350),
    "acctdata": ("acctdata.txt", "AWS.M2.CARDDEMO.ACCTDATA.PS", 300),
    "cardxref": ("cardxref.txt", "AWS.M2.CARDDEMO.CARDXREF.PS", 50),
    "tcatbal": ("tcatbal.txt", "AWS.M2.CARDDEMO.TCATBALF.PS", 50),
}


def ascii_records(path: Path, reclen: int):
    lines = path.read_bytes().replace(b"\r", b"").split(b"\n")
    if lines and lines[-1] == b"":
        lines.pop()
    return [line.ljust(reclen) for line in lines]


def ebcdic_records(path: Path, reclen: int):
    data = path.read_bytes()
    return [data[i:i + reclen].decode("cp037").encode("latin-1")
            for i in range(0, len(data), reclen)]


def prep(dataset: str, root: Path, out: Path):
    out.mkdir(parents=True, exist_ok=True)
    for name, (asc, ebc, reclen) in FILES.items():
        if dataset == "ascii":
            recs = ascii_records(root / "app/data/ASCII" / asc, reclen)
        else:
            recs = ebcdic_records(root / "app/data/EBCDIC" / ebc, reclen)
        assert all(len(r) == reclen for r in recs), name
        (out / f"{name}.dat").write_bytes(b"".join(recs))
        print(f"{name}: {len(recs)} records")


def lines(src: Path, reclen: int, dst: Path, mask_from=None, mask_len=None):
    data = src.read_bytes() if src.exists() else b""
    assert len(data) % reclen == 0, (src, len(data))
    out = []
    for i in range(0, len(data), reclen):
        rec = bytearray(data[i:i + reclen])
        if mask_from is not None:
            rec[mask_from:mask_from + mask_len] = b"#" * mask_len
        out.append(bytes(rec) + b"\n")
    dst.write_bytes(b"".join(out))


if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "prep":
        prep(sys.argv[2], Path(sys.argv[3]), Path(sys.argv[4]))
    elif cmd == "lines":
        extra = [int(x) for x in sys.argv[5:7]] if len(sys.argv) > 5 else []
        lines(Path(sys.argv[2]), int(sys.argv[3]), Path(sys.argv[4]), *extra)
    else:
        sys.exit(f"unknown command {cmd}")

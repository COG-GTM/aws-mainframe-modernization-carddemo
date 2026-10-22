#!/usr/bin/env python3
"""Fixed-length record helpers for the CBTRN01C/02C/03C GnuCOBOL golden harness.

prep <repo-root> <work-dir>
    Writes the ASCII seeds as fixed-length, newline-free files (the form
    GnuCOBOL reads for ORGANIZATION SEQUENTIAL) plus the crafted inputs:
      dalytran-edge.dat / acctdata-edge.dat  CBTRN02C reject codes 100-103 + debit + new TCATBAL
      dalytran-dup.dat                        duplicate TRAN-ID -> WRITE status 22 -> abend
lines <in-file> <reclen> <out-file> [mask-from mask-len]
    One record per line (no trimming), optionally masking a column range.
tranrept <transact.out> <out.dat> <start> <end> [variant]
    TRANREPT STEP05R stand-in: stamps a deterministic TRAN-PROC-TS, filters on
    TRAN-PROC-DT between start and end and stable-sorts by TRAN-CARD-NUM.
parm <out.dat> <start> <end>
    80-byte DATEPARM record.
"""
import sys
from pathlib import Path

SEEDS = {"dalytran": ("dailytran.txt", 350), "acctdata": ("acctdata.txt", 300),
         "cardxref": ("cardxref.txt", 50), "tcatbal": ("tcatbal.txt", 50),
         "custdata": ("custdata.txt", 500), "carddata": ("carddata.txt", 150),
         "trantype": ("trantype.txt", 60), "trancatg": ("trancatg.txt", 60)}


def records(path: Path, reclen: int):
    lines = path.read_bytes().replace(b"\r", b"").split(b"\n")
    if lines and lines[-1] == b"":
        lines.pop()
    return [bytearray(line.ljust(reclen)) for line in lines]


def put(rec, off, text):
    rec[off:off + len(text)] = text.encode("latin-1")


def prep(root: Path, out: Path):
    out.mkdir(parents=True, exist_ok=True)
    data = {}
    for name, (seed, reclen) in SEEDS.items():
        data[name] = records(root / "app/data/ASCII" / seed, reclen)
        (out / f"{name}.dat").write_bytes(b"".join(data[name]))
    edge = [bytearray(r) for r in data["dalytran"][:8]]
    put(edge[0], 262, "9999999999999999")             # 100 card not in XREF
    # edge[1]: card 0927987108636232 -> account 20, removed below      (101)
    put(edge[2], 278, "2099-01-01 00:00:00.000000")   # 103 after expiry
    put(edge[3], 132, "9999999999I")                  # 102 overlimit ...
    put(edge[3], 278, "2099-01-01 00:00:00.000000")   # ... then 103 wins
    put(edge[4], 132, "0000001234J")                  # negative -> cycle debit
    put(edge[5], 16, "099999")                        # new TCATBAL key -> create
    (out / "dalytran-edge.dat").write_bytes(b"".join(edge))
    acct = [r for r in data["acctdata"] if r[:11] != b"00000000020"]
    (out / "acctdata-edge.dat").write_bytes(b"".join(acct))
    dup = [bytearray(r) for r in data["dalytran"][:3]]
    dup[2][0:16] = dup[0][0:16]
    (out / "dalytran-dup.dat").write_bytes(b"".join(dup))


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


def proc_ts(i: int, variant: str) -> str:
    if variant == "mixed":
        # mostly in range; every 7th record dated after the range
        return "2022-08-01-10.00.00.000000" if i % 7 == 6 else f"2022-06-{1 + i % 28:02d}-10.00.00.000000"
    return "2022-06-10-10.00.00.000000"


def tranrept(src: Path, dst: Path, start: str, end: str, variant="all"):
    data = src.read_bytes()
    recs = []
    for n, i in enumerate(range(0, len(data), 350)):
        rec = bytearray(data[i:i + 350])
        put(rec, 304, proc_ts(n, variant))
        recs.append(rec)
    keep = [r for r in recs if start <= r[304:314].decode() <= end]
    keep.sort(key=lambda r: r[262:278])
    dst.write_bytes(b"".join(keep))


def parm(dst: Path, start: str, end: str):
    dst.write_bytes(f"{start} {end}".ljust(80).encode())


if __name__ == "__main__":
    cmd, args = sys.argv[1], sys.argv[2:]
    if cmd == "prep":
        prep(Path(args[0]), Path(args[1]))
    elif cmd == "lines":
        lines(Path(args[0]), int(args[1]), Path(args[2]), *[int(x) for x in args[3:5]])
    elif cmd == "tranrept":
        tranrept(Path(args[0]), Path(args[1]), *args[2:])
    elif cmd == "parm":
        parm(Path(args[0]), args[1], args[2])
    else:
        sys.exit(f"unknown command {cmd}")

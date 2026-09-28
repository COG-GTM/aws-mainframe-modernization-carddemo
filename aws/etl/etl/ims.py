"""Reader for the IMS HD unload file of DBPAUTP0 (DFSURGU0 output, RECFM=VB).

Each record: RDW (LL,ZZ) | segment code+flags (2) | 2 bytes | segment length (2) | segment name (8)
| prefix area (21) | segment data (segment length) | 1 pad byte. The first and last records are the
unload header/statistics records (segment code 00) and carry no segment data.
Offsets were derived from the sample file and cross-checked against the account/xref data
(see tests/test_ims.py).
"""

from __future__ import annotations

from dataclasses import dataclass

from etl import codec

DATA_OFFSET = 39


@dataclass(frozen=True)
class Segment:
    code: int
    name: str
    data: bytes


def read_segments(raw: bytes) -> list[Segment]:
    segments = []
    pos = 0
    while pos < len(raw):
        length = int.from_bytes(raw[pos : pos + 2], "big")
        if length < 18 or pos + length > len(raw):
            raise codec.DecodeError(f"bad RDW length {length} at offset {pos}")
        rec = raw[pos : pos + length]
        pos += length
        code = rec[4]
        if code == 0:
            continue
        seglen = int.from_bytes(rec[8:10], "big")
        name = rec[10:18].decode(codec.CODEPAGE).strip()
        if length < DATA_OFFSET + seglen:
            raise codec.DecodeError(f"segment {name} truncated at offset {pos - length}")
        segments.append(Segment(code, name, rec[DATA_OFFSET : DATA_OFFSET + seglen]))
    return segments

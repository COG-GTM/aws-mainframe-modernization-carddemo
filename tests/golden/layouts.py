#!/usr/bin/env python3
"""Record layouts for the CBTRN02C posting cycle, derived from the copybooks.

Nothing in this module hand-types an offset.  Every layout is parsed from the
copybook text under ``app/cpy`` (and, for the reject trailer, from the
WORKING-STORAGE of ``app/cbl/CBTRN02C.cbl``) at import time, so the harness
cannot drift from the source of truth.

Standard library only.

Sign convention
---------------
The harness compiles with ``cobc -fsign=EBCDIC``.  Zoned-decimal fields
therefore carry the mainframe overpunch on the last digit even in ASCII:
positive ``{ A-I`` (0..9), negative ``} J-R`` (0..9).  This matches the
repository's own ASCII sample datasets (``app/data/ASCII/*.txt``) and the
decoder used by the PR #9 candidate, so fixtures are portable between the two.
"""

from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from decimal import Decimal
from typing import Dict, List, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", ".."))
CPY_DIR = os.path.join(REPO, "app", "cpy")
CBTRN02C = os.path.join(REPO, "app", "cbl", "CBTRN02C.cbl")

POSITIVE_OVERPUNCH = "{ABCDEFGHI"
NEGATIVE_OVERPUNCH = "}JKLMNOPQR"


@dataclass
class Field:
    name: str
    level: int
    offset: int
    length: int
    pic: str
    usage: str  # "X" alphanumeric, "9" unsigned numeric, "S9" signed numeric, "GROUP"
    digits: int = 0
    scale: int = 0
    signed: bool = False
    source: str = ""  # path:line of the copybook line that defines it

    @property
    def is_numeric(self) -> bool:
        return self.usage in ("9", "S9")

    @property
    def end(self) -> int:
        return self.offset + self.length


@dataclass
class Layout:
    name: str
    record_name: str
    length: int
    fields: List[Field]  # elementary fields only, in copybook order
    source: str = ""
    key_field: Optional[str] = None

    def field(self, name: str) -> Field:
        for f in self.fields:
            if f.name == name:
                return f
        raise KeyError(name)

    def slice(self, record: bytes, name: str) -> bytes:
        f = self.field(name)
        return record[f.offset:f.end]

    def split(self, record: bytes) -> Dict[str, bytes]:
        return {f.name: record[f.offset:f.end] for f in self.fields}

    def key(self, record: bytes) -> bytes:
        if self.key_field is None:
            raise ValueError("layout %s has no key field" % self.name)
        return self.slice(record, self.key_field)

    def records(self, data: bytes) -> List[bytes]:
        if len(data) % self.length != 0:
            raise ValueError(
                "%s: data length %d is not a multiple of record length %d"
                % (self.name, len(data), self.length)
            )
        return [data[i:i + self.length] for i in range(0, len(data), self.length)]


# ---------------------------------------------------------------------------
# PIC parsing
# ---------------------------------------------------------------------------

_PIC_TOKEN = re.compile(r"([SXV9])(?:\((\d+)\))?")


def parse_pic(pic: str) -> Tuple[str, int, int, int, bool]:
    """Return (usage, byte_length, digits, scale, signed) for a DISPLAY PIC."""
    pic = pic.upper()
    signed = pic.startswith("S")
    body = pic[1:] if signed else pic
    expanded = ""
    pos = 0
    for m in _PIC_TOKEN.finditer(body):
        if m.start() != pos:
            raise ValueError("unsupported PIC %r" % pic)
        pos = m.end()
        sym, rep = m.group(1), m.group(2)
        expanded += sym * (int(rep) if rep else 1)
    if pos != len(body):
        raise ValueError("unsupported PIC %r" % pic)
    if "X" in expanded:
        if signed or "9" in expanded or "V" in expanded:
            raise ValueError("unsupported PIC %r" % pic)
        return "X", len(expanded), 0, 0, False
    if "V" in expanded:
        if expanded.count("V") != 1:
            raise ValueError("unsupported PIC %r" % pic)
        ints, frac = expanded.split("V")
        digits, scale = len(ints) + len(frac), len(frac)
    else:
        digits, scale = len(expanded), 0
    return ("S9" if signed else "9"), digits, digits, scale, signed


# ---------------------------------------------------------------------------
# Copybook parsing
# ---------------------------------------------------------------------------

def _cobol_lines(path: str) -> List[Tuple[int, str]]:
    out = []
    with open(path, "r", encoding="ascii", errors="replace") as fh:
        for n, raw in enumerate(fh, start=1):
            line = raw.rstrip("\r\n")
            if len(line) < 7:
                continue
            if line[6] in "*/":
                continue
            out.append((n, line[7:72]))
    return out


def _statements(lines: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
    """Join continuation lines into whole '.'-terminated data-description entries."""
    stmts = []
    buf, start = "", 0
    for n, text in lines:
        if not text.strip():
            continue
        if not buf:
            start = n
        buf += " " + text.strip()
        if buf.rstrip().endswith("."):
            stmts.append((start, buf.strip()))
            buf = ""
    if buf:
        stmts.append((start, buf.strip()))
    return stmts


_ENTRY = re.compile(
    r"^(\d{2})\s+([A-Z0-9-]+)(?:\s+PIC(?:TURE)?\s+(?:IS\s+)?([SXV9()0-9]+))?(?:\s+VALUE\s+.+?)?\.$",
    re.IGNORECASE,
)


def parse_entries(path: str, first_line: int = 1, last_line: int = 10 ** 9) -> List[Field]:
    """Parse level/name/PIC entries from a COBOL source file, computing offsets.

    Returns elementary and group fields (groups have usage "GROUP").
    Only the entries between first_line and last_line (inclusive) are read,
    which is how the reject trailer is lifted out of CBTRN02C WORKING-STORAGE.
    """
    lines = [(n, t) for (n, t) in _cobol_lines(path) if first_line <= n <= last_line]
    fields: List[Field] = []
    stack: List[Field] = []  # open group items
    offset = 0
    rel = os.path.relpath(path, REPO)
    for n, stmt in _statements(lines):
        m = _ENTRY.match(stmt)
        if not m:
            raise ValueError("%s:%d: unsupported data entry %r" % (rel, n, stmt))
        level, name, pic = int(m.group(1)), m.group(2).upper(), m.group(3)
        if level == 88:
            continue
        if level == 1:
            offset = 0
            stack = []
        while stack and stack[-1].level >= level:
            stack.pop()
        if pic is None:
            f = Field(name, level, offset, 0, "", "GROUP", source="%s:%d" % (rel, n))
            fields.append(f)
            stack.append(f)
            continue
        usage, length, digits, scale, signed = parse_pic(pic)
        f = Field(name, level, offset, length, pic.upper(), usage, digits, scale, signed,
                  source="%s:%d" % (rel, n))
        fields.append(f)
        offset += length
        for g in stack:
            g.length = offset - g.offset
    return fields


def load_copybook(member: str, key_field: Optional[str] = None) -> Layout:
    path = os.path.join(CPY_DIR, member + ".cpy")
    entries = parse_entries(path)
    record = entries[0]
    elementary = [f for f in entries if f.usage != "GROUP"]
    return Layout(member, record.name, record.length, elementary,
                  source=os.path.relpath(path, REPO), key_field=key_field)


# ---------------------------------------------------------------------------
# The layouts used by the posting cycle
# ---------------------------------------------------------------------------

# Copybook -> (ddname in CBTRN02C, record key used by the program)
#   app/cbl/CBTRN02C.cbl:29-61  SELECT clauses
#   app/cbl/CBTRN02C.cbl:102-126 COPY statements
DALYTRAN = load_copybook("CVTRA06Y", key_field="DALYTRAN-ID")
TRANSACT = load_copybook("CVTRA05Y", key_field="TRAN-ID")
XREF = load_copybook("CVACT03Y", key_field="XREF-CARD-NUM")
ACCT = load_copybook("CVACT01Y", key_field="ACCT-ID")
TCATBAL = load_copybook("CVTRA01Y", key_field="TRAN-CAT-KEY")

# TCATBAL's key is the group TRAN-CAT-KEY (CVTRA01Y.cpy:5-8); expose it as a
# derived slice so Layout.key() works on elementary fields only.
_tcat_entries = parse_entries(os.path.join(CPY_DIR, "CVTRA01Y.cpy"))
_tcat_key_group = [f for f in _tcat_entries if f.name == "TRAN-CAT-KEY"][0]
TCATBAL_KEY_LEN = _tcat_key_group.length
TCATBAL.key_field = None  # handled by tcatbal_key()


def tcatbal_key(record: bytes) -> bytes:
    return record[_tcat_key_group.offset:_tcat_key_group.offset + TCATBAL_KEY_LEN]


def _find_reject_trailer_lines() -> Tuple[int, int]:
    """Locate '01 WS-VALIDATION-TRAILER' in CBTRN02C so the trailer layout is
    parsed from the program itself (app/cbl/CBTRN02C.cbl:180-182)."""
    start = end = last_nonblank = None
    with open(CBTRN02C, "r", encoding="ascii", errors="replace") as fh:
        for n, line in enumerate(fh, start=1):
            if start is None and re.search(r"\b01\s+WS-VALIDATION-TRAILER\b", line):
                start = n
            elif start is not None and re.search(r"\b01\s+", line[7:72]):
                end = last_nonblank
                break
            if start is not None and line[7:72].strip():
                last_nonblank = n
    if start is None or end is None:
        raise RuntimeError("WS-VALIDATION-TRAILER not found in CBTRN02C")
    return start, end


def _build_dalyrejs() -> Layout:
    """DALYREJS = 350-byte DALYTRAN record + 80-byte validation trailer.

    REJECT-RECORD (CBTRN02C.cbl:176-178) is X(350) + X(80); the program fills
    the X(80) from WS-VALIDATION-TRAILER (CBTRN02C.cbl:180-182, :448), so the
    reject layout is the CVTRA06Y fields followed by the trailer fields.
    """
    s, e = _find_reject_trailer_lines()
    trailer = [f for f in parse_entries(CBTRN02C, s, e) if f.usage != "GROUP"]
    fields = [Field(**vars(f)) for f in DALYTRAN.fields]
    base = DALYTRAN.length
    for f in trailer:
        fields.append(Field(f.name, f.level, base + f.offset, f.length, f.pic, f.usage,
                            f.digits, f.scale, f.signed, f.source))
    total = base + sum(f.length for f in trailer)
    return Layout("DALYREJS", "REJECT-RECORD", total, fields,
                  source="app/cpy/CVTRA06Y.cpy + app/cbl/CBTRN02C.cbl:%d-%d" % (s, e),
                  key_field="DALYTRAN-ID")


DALYREJS = _build_dalyrejs()

# Output file name -> layout, as captured by run_reference.sh
OUTPUT_LAYOUTS: Dict[str, Layout] = {
    "TRANSACT": TRANSACT,
    "DALYREJS": DALYREJS,
    "ACCTFILE": ACCT,
    "TCATBALF": TCATBAL,
}
INPUT_LAYOUTS: Dict[str, Layout] = {
    "DALYTRAN": DALYTRAN,
    "XREFFILE": XREF,
    "ACCTFILE": ACCT,
    "TCATBALF": TCATBAL,
}

# Output files that are compared as text, not as records
TEXT_OUTPUTS = ("RETURN-CODE", "SYSOUT")


def record_key(file_name: str, record: bytes) -> bytes:
    if file_name == "TCATBALF":
        return tcatbal_key(record)
    return OUTPUT_LAYOUTS[file_name].key(record)


# ---------------------------------------------------------------------------
# Zoned decimal encode / decode
# ---------------------------------------------------------------------------

def encode_zoned(value, f: Field) -> bytes:
    """Encode a Decimal/int into f's PIC with COBOL store semantics
    (truncate excess fraction, truncate high-order digits, unsigned drops sign)."""
    if not f.is_numeric:
        raise TypeError("%s is not numeric" % f.name)
    d = Decimal(value)
    q = Decimal(1).scaleb(-f.scale)
    d = d.quantize(q, rounding="ROUND_DOWN")
    unscaled = int(d.scaleb(f.scale))
    negative = unscaled < 0
    unscaled = abs(unscaled) % (10 ** f.digits)
    text = str(unscaled).rjust(f.digits, "0")
    if f.signed:
        last = int(text[-1])
        text = text[:-1] + (NEGATIVE_OVERPUNCH if negative else POSITIVE_OVERPUNCH)[last]
    return text.encode("ascii")


def decode_zoned(raw: bytes, f: Field) -> Decimal:
    if not f.is_numeric:
        raise TypeError("%s is not numeric" % f.name)
    text = raw.decode("ascii", errors="replace")
    if len(text) != f.digits:
        raise ValueError("%s: expected %d bytes got %d" % (f.name, f.digits, len(text)))
    negative = False
    last = text[-1]
    if f.signed and last in POSITIVE_OVERPUNCH:
        text = text[:-1] + str(POSITIVE_OVERPUNCH.index(last))
    elif f.signed and last in NEGATIVE_OVERPUNCH:
        text = text[:-1] + str(NEGATIVE_OVERPUNCH.index(last))
        negative = True
    if not text.isdigit():
        raise ValueError("%s: not zoned decimal: %r" % (f.name, raw))
    d = Decimal(int(text)).scaleb(-f.scale)
    return -d if negative else d


def format_field(raw: bytes, f: Field) -> str:
    """Human rendering used in reports: numerics as signed decimal, text as repr."""
    if f.is_numeric:
        try:
            d = decode_zoned(raw, f)
            return ("%+" + ("." + str(f.scale) + "f" if f.scale else "d")) % (d if f.scale else int(d)) \
                if f.signed else (("%." + str(f.scale) + "f") % d if f.scale else str(int(d)))
        except ValueError:
            return "INVALID%r" % (raw,)
    return repr(raw.decode("ascii", errors="replace"))


def encode_text(value: str, f: Field) -> bytes:
    if f.is_numeric:
        raise TypeError("%s is numeric" % f.name)
    b = value.encode("ascii")
    if len(b) > f.length:
        raise ValueError("%s: %r longer than %d" % (f.name, value, f.length))
    return b.ljust(f.length, b" ")


def build_record(layout: Layout, values: Dict[str, object]) -> bytes:
    """Assemble a record from a field->value dict.  Unmentioned text fields are
    spaces; unmentioned numeric fields are zero (as INITIALIZE would do)."""
    out = bytearray(b" " * layout.length)
    seen = set()
    for f in layout.fields:
        if f.name in values:
            v = values[f.name]
            seen.add(f.name)
            raw = encode_zoned(v, f) if f.is_numeric else encode_text(str(v), f)
        elif f.is_numeric:
            raw = encode_zoned(0, f)
        else:
            raw = b" " * f.length
        out[f.offset:f.end] = raw
    unknown = set(values) - seen
    if unknown:
        raise KeyError("unknown fields for %s: %s" % (layout.name, sorted(unknown)))
    return bytes(out)


def layout_table(layout: Layout) -> List[Dict[str, object]]:
    return [
        {"name": f.name, "offset": f.offset, "length": f.length, "pic": f.pic,
         "usage": f.usage, "scale": f.scale, "signed": f.signed, "source": f.source}
        for f in layout.fields
    ]


if __name__ == "__main__":
    import json
    for name, lay in list(INPUT_LAYOUTS.items()) + [("TRANSACT", TRANSACT), ("DALYREJS", DALYREJS)]:
        print("%s (%s, %d bytes, %d fields) <- %s" % (name, lay.record_name, lay.length,
                                                      len(lay.fields), lay.source))
        for row in layout_table(lay):
            print("  %-32s off=%4d len=%4d pic=%-12s %s" % (row["name"], row["offset"],
                                                           row["length"], row["pic"], row["source"]))
    print(json.dumps({k: len(v.fields) for k, v in OUTPUT_LAYOUTS.items()}))

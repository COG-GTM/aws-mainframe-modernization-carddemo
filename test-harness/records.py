"""Decode / encode fixed-width COBOL record files using a copybook layout.

Decoding rules (the parity rules a Java port is judged against):

* alphanumeric (PIC X) fields -> JSON strings, trailing spaces kept exactly
  (EBCDIC input is translated with code page 037, ASCII is used as is);
* numeric fields -> decimal strings with the PIC's exact scale, e.g.
  PIC S9(10)V99 holding 1940.00 -> "1940.00", -1025.00 -> "-1025.00",
  PIC 9(11) holding 1 -> "1" (scale 0, no decimal point).  Negative zero
  is emitted as "0.00" (see TEST_STRATEGY.md);
* zoned decimal (USAGE DISPLAY) signs: overpunch in the last byte, both
  the EBCDIC convention carried by the ASCII sample data ({ = +0, A-I = +1
  to +9, } = -0, J-R = -1 to -9) and GnuCOBOL's native ASCII convention
  (p-y = -0 to -9) are accepted;
* COMP-3 (packed decimal): nibbles, sign nibble C/F positive, D negative;
* COMP / BINARY: big-endian two's complement (IBM byte order);
* dates are PIC X and stay as the 10-character text (e.g. "2014-11-20").
* OCCURS groups become JSON arrays of objects, one per occurrence.
* REDEFINES views are skipped (the original item is decoded); FILLER is
  skipped unless include_filler=True (then named FILLER@<offset>).

File formats: 'fixed' (records back to back), 'line' (fixed record
followed by '\n', the app/data/ASCII convention) and 'vb' (GnuCOBOL
COB_VARSEQ_FORMAT=1: 4-byte big-endian record length then the record).

Command line::

    python3 records.py decode  <copybook> <RECORD-NAME|-> <datafile> [--format line|fixed|vb] [--ebcdic] > out.json
    python3 records.py encode  <copybook> <RECORD-NAME|-> <in.json> <datafile> [--format fixed|line]
"""
from __future__ import annotations

import argparse
import json
import struct
import sys
from decimal import Decimal
from typing import Dict, List, Optional

from copybook import Field, load_layout

ZONED_ASCII_OVERPUNCH = {
    "{": (0, 1), "A": (1, 1), "B": (2, 1), "C": (3, 1), "D": (4, 1), "E": (5, 1),
    "F": (6, 1), "G": (7, 1), "H": (8, 1), "I": (9, 1),
    "}": (0, -1), "J": (1, -1), "K": (2, -1), "L": (3, -1), "M": (4, -1), "N": (5, -1),
    "O": (6, -1), "P": (7, -1), "Q": (8, -1), "R": (9, -1),
}
# GnuCOBOL native ASCII negative overpunch: 'p'..'y' = -0..-9
for _i in range(10):
    ZONED_ASCII_OVERPUNCH[chr(0x70 + _i)] = (_i, -1)
POS_OVERPUNCH = "{ABCDEFGHI"
NEG_OVERPUNCH = "}JKLMNOPQR"


class RecordError(ValueError):
    pass


# ----------------------------------------------------------------------
# scalar decode / encode
# ----------------------------------------------------------------------
def _format_decimal(digits: str, scale: int, negative: bool) -> str:
    if scale:
        s = digits[:-scale].lstrip("0") or "0"
        s += "." + digits[-scale:]
    else:
        s = digits.lstrip("0") or "0"
    if negative and Decimal(s) != 0:
        s = "-" + s
    return s


def decode_zoned(raw: bytes, scale: int, signed: bool, ebcdic: bool) -> str:
    if ebcdic:
        digits = []
        negative = False
        for i, b in enumerate(raw):
            zone, digit = b >> 4, b & 0x0F
            if digit > 9:
                raise RecordError("bad zoned digit %r" % raw)
            if i == len(raw) - 1 and signed:
                negative = zone == 0x0D
            digits.append(str(digit))
        return _format_decimal("".join(digits), scale, negative)
    text = raw.decode("ascii")
    negative = False
    if text and not text[-1].isdigit():
        last = text[-1]
        if last not in ZONED_ASCII_OVERPUNCH:
            raise RecordError("bad zoned sign byte %r in %r" % (last, raw))
        d, sgn = ZONED_ASCII_OVERPUNCH[last]
        text = text[:-1] + str(d)
        negative = sgn < 0
    if not text.isdigit():
        raise RecordError("non numeric zoned data %r" % raw)
    return _format_decimal(text, scale, negative)


def encode_zoned(value: str, digits: int, scale: int, signed: bool, ebcdic: bool) -> bytes:
    d = Decimal(value)
    negative = d < 0
    q = abs(d).scaleb(scale)
    if q != q.to_integral_value():
        raise RecordError("value %s has more than %d decimals" % (value, scale))
    s = str(int(q)).rjust(digits, "0")
    if len(s) > digits:
        raise RecordError("value %s does not fit in %d digits" % (value, digits))
    if ebcdic:
        out = bytearray(0xF0 | int(c) for c in s)
        if signed:
            out[-1] = ((0xD0 if negative else 0xC0) | int(s[-1]))
        return bytes(out)
    if signed:
        last = int(s[-1])
        s = s[:-1] + (NEG_OVERPUNCH if negative else POS_OVERPUNCH)[last]
    return s.encode("ascii")


def decode_comp3(raw: bytes, scale: int) -> str:
    nibbles = []
    for b in raw:
        nibbles.append(b >> 4)
        nibbles.append(b & 0x0F)
    sign = nibbles.pop()
    if sign not in (0x0C, 0x0D, 0x0F):
        raise RecordError("bad COMP-3 sign nibble %x in %r" % (sign, raw))
    if any(n > 9 for n in nibbles):
        raise RecordError("bad COMP-3 digit in %r" % raw)
    return _format_decimal("".join(str(n) for n in nibbles), scale, sign == 0x0D)


def encode_comp3(value: str, digits: int, scale: int, signed: bool) -> bytes:
    d = Decimal(value)
    negative = d < 0
    q = abs(d).scaleb(scale)
    if q != q.to_integral_value():
        raise RecordError("value %s has more than %d decimals" % (value, scale))
    length = digits // 2 + 1
    ndig = length * 2 - 1
    if len(str(int(q))) > digits:
        raise RecordError("value %s does not fit in %d digits" % (value, digits))
    s = str(int(q)).rjust(ndig, "0")
    nibbles = [int(c) for c in s] + [0x0D if negative else (0x0C if signed else 0x0F)]
    return bytes((nibbles[i] << 4) | nibbles[i + 1] for i in range(0, len(nibbles), 2))


def decode_binary(raw: bytes, scale: int, signed: bool) -> str:
    v = int.from_bytes(raw, "big", signed=signed)
    return _format_decimal(str(abs(v)), scale, v < 0)


def encode_binary(value: str, length: int, scale: int, signed: bool) -> bytes:
    q = Decimal(value).scaleb(scale)
    if q != q.to_integral_value():
        raise RecordError("value %s has more than %d decimals" % (value, scale))
    if q < 0 and not signed:
        raise RecordError("negative value %s in unsigned field" % value)
    try:
        return int(q).to_bytes(length, "big", signed=signed)
    except OverflowError:
        raise RecordError("value %s does not fit in %d bytes" % (value, length))


def decode_alnum(raw: bytes, ebcdic: bool) -> str:
    return raw.decode("cp037" if ebcdic else "latin-1")


def encode_alnum(value: str, length: int, ebcdic: bool) -> bytes:
    b = value.encode("cp037" if ebcdic else "latin-1")
    if len(b) > length:
        raise RecordError("string %r longer than %d" % (value, length))
    return b.ljust(length, b"\x40" if ebcdic else b" ")


# ----------------------------------------------------------------------
# record decode / encode
# ----------------------------------------------------------------------
INVALID_PREFIX = "INVALID-COMP-3:"


def decode_record(raw: bytes, rec: Field, ebcdic: bool = False,
                  include_filler: bool = False, lenient: bool = False) -> Dict:
    """``lenient=True`` turns a COMP-3 field whose bytes are not a valid
    packed decimal (e.g. never-assigned WORKING-STORAGE that a program wrote
    out) into the marker string ``INVALID-COMP-3:<hex>`` instead of raising,
    so the golden still records exactly what the program produced.
    ``encode_record`` turns the marker back into the original bytes."""
    if len(raw) < rec.length:
        raise RecordError("record is %d bytes, layout %s needs %d" % (len(raw), rec.name, rec.length))
    return _decode_group(raw, rec, 0, ebcdic, include_filler, lenient)


def _decode_group(raw: bytes, grp: Field, base: int, ebcdic: bool, include_filler: bool,
                  lenient: bool = False) -> Dict:
    out: Dict = {}
    for c in grp.children:
        if c.redefines:
            continue
        if c.is_filler and not include_filler:
            continue
        name = c.name if not c.is_filler else "FILLER@%d" % c.offset
        vals = []
        for idx in range(c.occurs):
            off = base + (c.offset - grp.offset) + idx * c.length
            if c.is_group:
                vals.append(_decode_group(raw, c, off, ebcdic, include_filler, lenient))
            else:
                vals.append(_decode_scalar(raw[off:off + c.length], c, ebcdic, lenient))
        out[name] = vals if c.occurs > 1 else vals[0]
    return out


def _decode_scalar(b: bytes, f: Field, ebcdic: bool, lenient: bool = False) -> str:
    if f.type == "alnum":
        return decode_alnum(b, ebcdic)
    if f.usage == "DISPLAY":
        return decode_zoned(b, f.scale, f.sign, ebcdic)
    if f.usage == "COMP-3":
        try:
            return decode_comp3(b, f.scale)
        except RecordError:
            if not lenient:
                raise
            return INVALID_PREFIX + b.hex()
    if f.usage == "COMP":
        return decode_binary(b, f.scale, f.sign)
    raise RecordError("cannot decode usage %s" % f.usage)


def encode_record(data: Dict, rec: Field, ebcdic: bool = False) -> bytes:
    buf = bytearray(b"\x40" * rec.length if ebcdic else b" " * rec.length)
    _encode_group(buf, data, rec, 0, ebcdic)
    return bytes(buf)


def _encode_group(buf: bytearray, data: Dict, grp: Field, base: int, ebcdic: bool) -> None:
    for c in grp.children:
        if c.redefines:
            continue
        name = c.name if not c.is_filler else "FILLER@%d" % c.offset
        if name not in data:
            if c.is_filler:
                continue
            raise RecordError("field %s missing from record" % name)
        vals = data[name] if c.occurs > 1 else [data[name]]
        for idx in range(c.occurs):
            off = base + (c.offset - grp.offset) + idx * c.length
            if c.is_group:
                _encode_group(buf, vals[idx], c, off, ebcdic)
            else:
                buf[off:off + c.length] = _encode_scalar(vals[idx], c, ebcdic)


def _encode_scalar(v: str, f: Field, ebcdic: bool) -> bytes:
    if f.type == "alnum":
        return encode_alnum(v, f.length, ebcdic)
    if f.usage == "DISPLAY":
        return encode_zoned(v, f.digits, f.scale, f.sign, ebcdic)
    if f.usage == "COMP-3":
        if isinstance(v, str) and v.startswith(INVALID_PREFIX):
            raw = bytes.fromhex(v[len(INVALID_PREFIX):])
            if len(raw) != f.length:
                raise RecordError("%s has %d bytes, field needs %d" % (v, len(raw), f.length))
            return raw
        return encode_comp3(v, f.digits, f.scale, f.sign)
    if f.usage == "COMP":
        return encode_binary(v, f.length, f.scale, f.sign)
    raise RecordError("cannot encode usage %s" % f.usage)


# ----------------------------------------------------------------------
# file level
# ----------------------------------------------------------------------
def split_records(data: bytes, record_length: int, fmt: str = "fixed") -> List[bytes]:
    """Split a file into raw records. fmt: fixed | line | vb."""
    recs: List[bytes] = []
    if fmt == "fixed":
        if len(data) % record_length:
            raise RecordError("file size %d is not a multiple of %d" % (len(data), record_length))
        return [data[i:i + record_length] for i in range(0, len(data), record_length)]
    if fmt == "line":
        for ln in data.split(b"\n"):
            if ln == b"":
                continue
            ln = ln.rstrip(b"\r")
            if len(ln) > record_length:
                raise RecordError("line longer (%d) than record length %d" % (len(ln), record_length))
            recs.append(ln.ljust(record_length, b" "))
        return recs
    if fmt == "vb":
        pos = 0
        while pos < len(data):
            (ln,) = struct.unpack(">I", data[pos:pos + 4])
            pos += 4
            recs.append(data[pos:pos + ln])
            pos += ln
        return recs
    raise RecordError("unknown format %s" % fmt)


def decode_file(path: str, rec: Field, fmt: str = "fixed", ebcdic: bool = False,
                include_filler: bool = False, layouts_by_length: Optional[Dict[int, Field]] = None,
                lenient: bool = False) -> List[Dict]:
    with open(path, "rb") as fh:
        data = fh.read()
    out = []
    for raw in split_records(data, rec.length if rec else 0, fmt):
        lay = rec
        if layouts_by_length is not None:
            lay = layouts_by_length.get(len(raw))
            if lay is None:
                raise RecordError("no layout for record length %d" % len(raw))
        d = decode_record(raw, lay, ebcdic, include_filler, lenient)
        if layouts_by_length is not None:
            d = {"_record": lay.name, "_length": len(raw), **d}
        out.append(d)
    return out


def encode_file(path: str, records: List[Dict], rec: Field, fmt: str = "fixed", ebcdic: bool = False) -> None:
    with open(path, "wb") as fh:
        for d in records:
            raw = encode_record(d, rec, ebcdic)
            if fmt == "vb":
                fh.write(struct.pack(">I", len(raw)))
            fh.write(raw)
            if fmt == "line":
                fh.write(b"\n")


def dump_json(obj, path: Optional[str] = None) -> str:
    text = json.dumps(obj, indent=2, ensure_ascii=False) + "\n"
    if path:
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(text)
    return text


def main(argv: List[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("action", choices=["decode", "encode"])
    ap.add_argument("copybook")
    ap.add_argument("record", help="01-level name, or - for the only one")
    ap.add_argument("src")
    ap.add_argument("dst", nargs="?")
    ap.add_argument("--format", default="fixed", choices=["fixed", "line", "vb"])
    ap.add_argument("--ebcdic", action="store_true")
    ap.add_argument("--include-filler", action="store_true")
    a = ap.parse_args(argv[1:])
    rec = load_layout(a.copybook, None if a.record == "-" else a.record)
    if a.action == "decode":
        recs = decode_file(a.src, rec, a.format, a.ebcdic, a.include_filler)
        sys.stdout.write(dump_json(recs, a.dst))
    else:
        with open(a.src, encoding="utf-8") as fh:
            recs = json.load(fh)
        encode_file(a.dst, recs, rec, a.format, a.ebcdic)
        print("wrote %d records to %s" % (len(recs), a.dst))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

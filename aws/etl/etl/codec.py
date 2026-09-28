"""Primitive decoders for mainframe field encodings (code page CP037)."""

from __future__ import annotations

from decimal import Decimal

CODEPAGE = "cp037"

_POSITIVE_SIGNS = {0xC, 0xF, 0xA, 0xE}
_NEGATIVE_SIGNS = {0xD, 0xB}


class DecodeError(ValueError):
    pass


def _scaled(unscaled: int, scale: int) -> int | Decimal:
    if scale == 0:
        return unscaled
    return Decimal(unscaled).scaleb(-scale)


def decode_alnum(data: bytes) -> str:
    """PIC X / PIC A: CP037 text, returned untrimmed."""
    return data.decode(CODEPAGE)


def decode_zoned(data: bytes, scale: int = 0, signed: bool = False) -> int | Decimal:
    """Zoned decimal (USAGE DISPLAY). The sign lives in the zone nibble of the last byte
    (overpunch: C=+, D=-, F=unsigned), e.g. F1 F2 C3 = +123, F1 F2 D3 = -123."""
    if not data:
        raise DecodeError("empty zoned field")
    value = 0
    for i, b in enumerate(data):
        zone, digit = b >> 4, b & 0x0F
        if digit > 9:
            raise DecodeError(f"invalid zoned digit nibble in {data.hex()}")
        last = i == len(data) - 1
        if not last and zone != 0xF:
            raise DecodeError(f"invalid zoned zone nibble in {data.hex()}")
        value = value * 10 + digit
    sign = data[-1] >> 4
    if sign in _NEGATIVE_SIGNS:
        if not signed:
            raise DecodeError(f"negative sign on unsigned field {data.hex()}")
        value = -value
    elif sign not in _POSITIVE_SIGNS:
        raise DecodeError(f"invalid zoned sign nibble in {data.hex()}")
    return _scaled(value, scale)


def decode_packed(data: bytes, scale: int = 0, signed: bool = True) -> int | Decimal:
    """Packed decimal (COMP-3): two digits per byte, sign in the low nibble of the last byte."""
    if not data:
        raise DecodeError("empty packed field")
    value = 0
    nibbles = []
    for b in data:
        nibbles.extend((b >> 4, b & 0x0F))
    sign = nibbles.pop()
    for n in nibbles:
        if n > 9:
            raise DecodeError(f"invalid packed digit nibble in {data.hex()}")
        value = value * 10 + n
    if sign in _NEGATIVE_SIGNS:
        value = -value
    elif sign not in _POSITIVE_SIGNS:
        raise DecodeError(f"invalid packed sign nibble in {data.hex()}")
    return _scaled(value, scale)


def decode_binary(data: bytes, scale: int = 0, signed: bool = False) -> int | Decimal:
    """Binary (COMP / COMP-4 / BINARY / COMP-5): big-endian two's complement when signed."""
    return _scaled(int.from_bytes(data, "big", signed=signed), scale)


def is_blank(data: bytes) -> bool:
    """All EBCDIC spaces or all low-values."""
    return all(b == 0x40 for b in data) or all(b == 0x00 for b in data)


def text_to_ebcdic(line: str) -> bytes:
    """Re-encode an ASCII sample line to CP037 so the same decoders apply. ASCII overpunch
    characters map onto the EBCDIC sign zones: '{'->C0, 'A'..'I'->C1..C9, '}'->D0, 'J'..'R'->D1..D9."""
    return line.encode(CODEPAGE)

from decimal import Decimal

import pytest

from etl import codec
from etl.copybook import Picture


def eb(s: str) -> bytes:
    return s.encode("cp037")


class TestZoned:
    def test_unsigned(self):
        assert codec.decode_zoned(bytes.fromhex("f0f1f2f3")) == 123

    def test_positive_overpunch_c(self):
        assert codec.decode_zoned(bytes.fromhex("f1f2c3"), signed=True) == 123

    def test_negative_overpunch_d(self):
        assert codec.decode_zoned(bytes.fromhex("f1f2d3"), signed=True) == -123

    def test_unsigned_f_zone_on_signed_field_is_positive(self):
        assert codec.decode_zoned(bytes.fromhex("f1f2f3"), signed=True) == 123

    def test_scale(self):
        assert codec.decode_zoned(bytes.fromhex("f0f0f1f0f0f0c0"), scale=2, signed=True) == Decimal("100.00")
        assert codec.decode_zoned(bytes.fromhex("f0f0f0f5f6f7d7"), scale=2, signed=True) == Decimal("-56.77")

    @pytest.mark.parametrize(
        "text,expected",
        [
            ("0000{", 0), ("0000A", 1), ("0001I", 19), ("0000}", 0), ("0000J", -1), ("0001R", -19),
            ("00919}", -9190), ("00504G", 5047),
        ],
    )
    def test_ascii_overpunch_characters(self, text, expected):
        # ASCII sample convention: '{' / 'A'-'I' = +0..+9, '}' / 'J'-'R' = -0..-9 in the last position.
        assert codec.decode_zoned(codec.text_to_ebcdic(text), signed=True) == expected

    def test_overpunch_scaled_amount(self):
        assert codec.decode_zoned(eb("00000091900}"), scale=2, signed=True) == Decimal("-9190.00")

    def test_negative_on_unsigned_field_rejected(self):
        with pytest.raises(codec.DecodeError):
            codec.decode_zoned(bytes.fromhex("f1d2"), signed=False)

    def test_bad_digit_rejected(self):
        with pytest.raises(codec.DecodeError):
            codec.decode_zoned(eb("12A4"))

    def test_spaces_rejected(self):
        with pytest.raises(codec.DecodeError):
            codec.decode_zoned(eb("   "))


class TestPacked:
    def test_odd_digits_positive(self):
        # S9(11) COMP-3 = 6 bytes, 11 digits + sign
        assert codec.decode_packed(bytes.fromhex("00000000001c")) == 1

    def test_even_digit_picture_has_leading_zero_nibble(self):
        # S9(04) COMP-3 = 3 bytes: 0 1 2 3 4 C
        assert codec.decode_packed(bytes.fromhex("01234c")) == 1234

    def test_negative(self):
        assert codec.decode_packed(bytes.fromhex("12345d"), scale=2) == Decimal("-123.45")

    def test_unsigned_f_sign(self):
        assert codec.decode_packed(bytes.fromhex("12345f")) == 12345

    def test_scaled_amount(self):
        # S9(09)V99 COMP-3 = 6 bytes
        assert codec.decode_packed(bytes.fromhex("00000202200c"), scale=2) == Decimal("2022.00")

    def test_zero(self):
        assert codec.decode_packed(bytes.fromhex("000c"), scale=2) == Decimal("0.00")

    @pytest.mark.parametrize("raw", ["404040", "1a2c", "12340"])
    def test_invalid(self, raw):
        with pytest.raises(codec.DecodeError):
            codec.decode_packed(bytes.fromhex(raw) if len(raw) % 2 == 0 else bytes.fromhex(raw + "0"))


class TestBinary:
    def test_unsigned(self):
        assert codec.decode_binary(bytes.fromhex("000001f4")) == 500

    def test_signed_negative(self):
        assert codec.decode_binary(bytes.fromhex("fffe"), signed=True) == -2

    def test_signed_positive(self):
        assert codec.decode_binary(bytes.fromhex("0032"), signed=True) == 50


class TestAlnum:
    def test_cp037(self):
        assert codec.decode_alnum(bytes.fromhex("c1c4d4c9d5f0f0f1")) == "ADMIN001"

    def test_blank(self):
        assert codec.is_blank(b"\x40" * 4)
        assert codec.is_blank(b"\x00" * 4)
        assert not codec.is_blank(b"\x40\xf0")


class TestPicture:
    @pytest.mark.parametrize(
        "pic,expected",
        [
            ("X(08)", Picture("X", 8)),
            ("9(11)", Picture("9", 11)),
            ("S9(10)V99", Picture("9", 12, 2, True)),
            ("S9(04)V99", Picture("9", 6, 2, True)),
            ("S9(4)", Picture("9", 4, 0, True)),
            ("99V9", Picture("9", 3, 1, False)),
        ],
    )
    def test_parse(self, pic, expected):
        assert Picture.parse(pic) == expected

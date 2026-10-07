package com.carddemo.common.codec;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CobolNumericTest {

    @ParameterizedTest
    @CsvSource({
            "0000012{, 2, true, 1.20",
            "0000012}, 2, true, -1.20",
            "0000012A, 2, true, 1.21",
            "0000012R, 2, true, -1.29",
            "00000120, 2, false, 1.20",
            "12p, 0, true, -120",
            "12y, 1, true, -12.9",
            "000}, 0, true, 0",
            "9999999999I, 2, true, 999999999.99"
    })
    void decodesZonedOverpunch(String text, int scale, boolean signed, String expected) {
        BigDecimal v = CobolNumeric.decodeZoned(text, scale, signed);
        assertThat(v).isEqualTo(new BigDecimal(expected));
        assertThat(v.scale()).isEqualTo(scale);
    }

    @ParameterizedTest
    @CsvSource({
            "1.20, 8, 2, true, 0000012{",
            "-1.20, 8, 2, true, 0000012}",
            "-1.29, 8, 2, true, 0000012R",
            "1.29, 8, 2, true, 0000012I",
            "-1.20, 8, 2, false, 00000120",
            "-0.00, 3, 2, true, 00{",
            "12, 4, 0, false, 0012"
    })
    void encodesZonedLikeCobolMove(String value, int digits, int scale, boolean signed, String expected) {
        String encoded = CobolNumeric.encodeZoned(new BigDecimal(value), digits, scale, signed);
        assertThat(encoded).isEqualTo(expected);
    }

    @Test
    void zonedEncodingDropsHighOrderDigitsAndTruncatesFraction() {
        assertThat(CobolNumeric.encodeZoned(new BigDecimal("123456.789"), 5, 2, true)).isEqualTo("4567H");
        assertThat(CobolNumeric.encodeZoned(new BigDecimal("-1.999"), 3, 2, true)).isEqualTo("19R");
    }

    @Test
    void rejectsInvalidZonedText() {
        assertThatThrownBy(() -> CobolNumeric.decodeZoned("12 ", 0, true)).isInstanceOf(RecordFormatException.class)
                .hasMessageContaining("position 3");
        assertThatThrownBy(() -> CobolNumeric.decodeZoned("12{", 0, false))
                .isInstanceOf(RecordFormatException.class).hasMessageContaining("unsigned");
        assertThatThrownBy(() -> CobolNumeric.decodeZoned("1{2", 0, true)).isInstanceOf(RecordFormatException.class);
        assertThatThrownBy(() -> CobolNumeric.decodeZoned("", 0, true)).isInstanceOf(RecordFormatException.class);
    }

    @Test
    void encodesPackedWithSignNibbles() {
        assertThat(packed("12345", 5, 0, true)).containsExactly(0x12, 0x34, 0x5C);
        assertThat(packed("-12345", 5, 0, true)).containsExactly(0x12, 0x34, 0x5D);
        assertThat(packed("-12345", 5, 0, false)).containsExactly(0x12, 0x34, 0x5F);
        assertThat(packed("1234", 4, 0, true)).containsExactly(0x01, 0x23, 0x4C);
        assertThat(packed("-0.00", 3, 2, true)).containsExactly(0x00, 0x0C);
        assertThat(packed("1234567.899", 7, 2, true)).containsExactly(0x34, 0x56, 0x78, 0x9C);
    }

    @ParameterizedTest
    @CsvSource({"C, 123", "F, 123", "A, 123", "E, 123", "D, -123", "B, -123"})
    void decodesPackedSignNibbles(String nibble, String expected) {
        byte[] image = {0x12, (byte) (0x30 | Integer.parseInt(nibble, 16))};
        assertThat(CobolNumeric.decodePacked(image, 0, 2, 0)).isEqualTo(new BigDecimal(expected));
    }

    @Test
    void negativeZeroPackedDecodesAsZero() {
        BigDecimal v = CobolNumeric.decodePacked(new byte[] {0x00, 0x0D}, 0, 2, 2);
        assertThat(v.signum()).isZero();
        assertThat(v.scale()).isEqualTo(2);
    }

    @Test
    void rejectsInvalidPackedNibbles() {
        assertThatThrownBy(() -> CobolNumeric.decodePacked(new byte[] {0x12, 0x35}, 0, 2, 0))
                .isInstanceOf(RecordFormatException.class).hasMessageContaining("sign nibble 5")
                .hasMessageContaining("X'1235'");
        assertThatThrownBy(() -> CobolNumeric.decodePacked(new byte[] {(byte) 0xA2, 0x3C}, 0, 2, 0))
                .isInstanceOf(RecordFormatException.class).hasMessageContaining("digit nibble A");
        assertThatThrownBy(() -> CobolNumeric.decodePacked(new byte[] {0x1B, 0x3C}, 0, 2, 0))
                .isInstanceOf(RecordFormatException.class).hasMessageContaining("digit nibble B");
        assertThatThrownBy(() -> CobolNumeric.encodePacked(new byte[4], 0, 4, 5, 0, true, BigDecimal.ONE))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void packedAndBinaryLengths() {
        assertThat(CobolNumeric.packedLength(1)).isEqualTo(1);
        assertThat(CobolNumeric.packedLength(3)).isEqualTo(2);
        assertThat(CobolNumeric.packedLength(12)).isEqualTo(7);
        assertThat(CobolNumeric.binaryLength(4)).isEqualTo(2);
        assertThat(CobolNumeric.binaryLength(5)).isEqualTo(4);
        assertThat(CobolNumeric.binaryLength(9)).isEqualTo(4);
        assertThat(CobolNumeric.binaryLength(10)).isEqualTo(8);
        assertThat(CobolNumeric.binaryLength(18)).isEqualTo(8);
        assertThatThrownBy(() -> CobolNumeric.binaryLength(19)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> CobolNumeric.packedLength(0)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void encodesAndDecodesBinary() {
        byte[] image = new byte[4];
        CobolNumeric.encodeBinary(image, 0, 4, 9, 0, false, new BigDecimal("123456789"));
        assertThat(unsigned(image)).containsExactly(0x07, 0x5B, 0xCD, 0x15);
        assertThat(CobolNumeric.decodeBinary(image, 0, 4, 0, false)).isEqualTo(new BigDecimal("123456789"));

        byte[] half = new byte[2];
        CobolNumeric.encodeBinary(half, 0, 2, 4, 2, true, new BigDecimal("-0.01"));
        assertThat(unsigned(half)).containsExactly(0xFF, 0xFF);
        assertThat(CobolNumeric.decodeBinary(half, 0, 2, 2, true)).isEqualTo(new BigDecimal("-0.01"));
        assertThat(CobolNumeric.decodeBinary(half, 0, 2, 0, false)).isEqualTo(new BigDecimal("65535"));

        CobolNumeric.encodeBinary(half, 0, 2, 4, 0, true, new BigDecimal("123456"));
        assertThat(CobolNumeric.decodeBinary(half, 0, 2, 0, true)).isEqualTo(new BigDecimal("3456"));

        byte[] wide = new byte[8];
        CobolNumeric.encodeBinary(wide, 0, 8, 12, 2, true, new BigDecimal("-9999999999.99"));
        assertThat(CobolNumeric.decodeBinary(wide, 0, 8, 2, true)).isEqualTo(new BigDecimal("-9999999999.99"));
        assertThatThrownBy(() -> CobolNumeric.encodeBinary(wide, 0, 8, 4, 0, true, BigDecimal.ONE))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void truncateFollowsCobolMove() {
        assertThat(CobolNumeric.truncate(new BigDecimal("12345.678"), 5, 2, true)).isEqualTo(new BigDecimal("345.67"));
        assertThat(CobolNumeric.truncate(new BigDecimal("-1.999"), 3, 2, true)).isEqualTo(new BigDecimal("-1.99"));
        assertThat(CobolNumeric.truncate(new BigDecimal("-5"), 3, 0, false)).isEqualTo(new BigDecimal("5"));
        assertThatThrownBy(() -> CobolNumeric.truncate(BigDecimal.ONE, 3, 4, true))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> CobolNumeric.truncate(BigDecimal.ONE, 3, -1, true))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void fitsChecksIntegerPartOnly() {
        assertThat(CobolNumeric.fits(new BigDecimal("999.999"), 5, 2)).isTrue();
        assertThat(CobolNumeric.fits(new BigDecimal("-999.99"), 5, 2)).isTrue();
        assertThat(CobolNumeric.fits(new BigDecimal("1000"), 5, 2)).isFalse();
    }

    private static int[] packed(String value, int digits, int scale, boolean signed) {
        byte[] image = new byte[CobolNumeric.packedLength(digits)];
        CobolNumeric.encodePacked(image, 0, image.length, digits, scale, signed, new BigDecimal(value));
        return unsigned(image);
    }

    private static int[] unsigned(byte[] bytes) {
        int[] out = new int[bytes.length];
        for (int i = 0; i < bytes.length; i++) {
            out[i] = bytes[i] & 0xFF;
        }
        return out;
    }
}

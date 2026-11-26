package com.carddemo.programs.paudbunl.support;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CobolDataTest {

    private static byte[] hex(String h) {
        byte[] b = new byte[h.length() / 2];
        for (int i = 0; i < b.length; i++) {
            b[i] = (byte) Integer.parseInt(h.substring(i * 2, i * 2 + 2), 16);
        }
        return b;
    }

    @Test
    void packedLengthMatchesComp3Storage() {
        assertEquals(6, CobolData.packedLength(11));
        assertEquals(3, CobolData.packedLength(5));
        assertEquals(5, CobolData.packedLength(9));
        assertEquals(7, CobolData.packedLength(12));
    }

    @Test
    void isNumericAcceptsCDFSigns() {
        assertTrue(CobolData.isNumericPacked(hex("00000000001C"), 0, 6));
        assertTrue(CobolData.isNumericPacked(hex("00000000001D"), 0, 6));
        assertTrue(CobolData.isNumericPacked(hex("00000000001F"), 0, 6));
    }

    @Test
    void isNumericRejectsSpacesLowValuesAndBadDigits() {
        assertFalse(CobolData.isNumericPacked(hex("404040404040"), 0, 6));
        assertFalse(CobolData.isNumericPacked(hex("000000000000"), 0, 6));
        assertFalse(CobolData.isNumericPacked(hex("0000000A001C"), 0, 6));
        assertFalse(CobolData.isNumericPacked(hex("00000000001A"), 0, 6));
    }

    @Test
    void decodePackedSignAndScale() {
        assertEquals(new BigDecimal("2022.00"), CobolData.decodePacked(hex("00000202200C"), 0, 6, 2));
        assertEquals(new BigDecimal("-123.45"), CobolData.decodePacked(hex("00000012345D"), 0, 6, 2));
        assertEquals(new BigDecimal("1"), CobolData.decodePacked(hex("00000000001F"), 0, 6, 0));
    }

    @Test
    void decodeInvalidPackedIsDataException() {
        assertThrows(IllegalArgumentException.class, () -> CobolData.decodePacked(hex("404040404040"), 0, 6, 0));
    }

    @Test
    void encodePackedTruncatesLikeCobolMove() {
        byte[] b = new byte[6];
        CobolData.encodePacked(new BigDecimal("12.349"), 11, 2, b, 0);
        assertArrayEquals(hex("00000001234C"), b);
        CobolData.encodePacked(new BigDecimal("123456789012"), 11, 0, b, 0);
        assertArrayEquals(hex("23456789012C"), b);
        CobolData.encodePacked(new BigDecimal("-5"), 11, 0, b, 0);
        assertArrayEquals(hex("00000000005D"), b);
        CobolData.encodePacked(BigDecimal.ZERO, 11, 0, b, 0);
        assertArrayEquals(hex("00000000000C"), b);
    }

    @Test
    void binaryRoundTripIsSignedBigEndian() {
        byte[] b = new byte[2];
        CobolData.encodeBinary(-2, b, 0, 2);
        assertArrayEquals(hex("FFFE"), b);
        assertEquals(-2, CobolData.decodeBinary(b, 0, 2));
        CobolData.encodeBinary(6, b, 0, 2);
        assertEquals(6, CobolData.decodeBinary(b, 0, 2));
    }

    @Test
    void alphaPadsAndTruncates() {
        byte[] b = new byte[4];
        CobolData.encodeAlpha("AB", b, 0, 4, CobolData.EBCDIC);
        assertArrayEquals(hex("C1C24040"), b);
        CobolData.encodeAlpha("ABCDEF", b, 0, 4, CobolData.EBCDIC);
        assertEquals("ABCD", CobolData.decodeAlpha(b, 0, 4, CobolData.EBCDIC));
    }

    @Test
    void zonedRoundTripAndValidation() {
        byte[] b = new byte[9];
        CobolData.encodeZoned(1, b, 0, 9, CobolData.EBCDIC);
        assertArrayEquals(hex("F0F0F0F0F0F0F0F0F1"), b);
        assertEquals(1, CobolData.decodeZoned(b, 0, 9, CobolData.EBCDIC));
        CobolData.encodeZoned(1234567, b, 0, 2, CobolData.EBCDIC);
        assertEquals(67, CobolData.decodeZoned(b, 0, 2, CobolData.EBCDIC));
        byte[] spaces = hex("4040");
        assertThrows(IllegalArgumentException.class, () -> CobolData.decodeZoned(spaces, 0, 2, CobolData.EBCDIC));
    }

    @Test
    void addWithoutSizeErrorDropsHighOrderDigit() {
        assertEquals(1, CobolData.addTruncating(0, 1, 8));
        assertEquals(0, CobolData.addTruncating(99_999_999L, 1, 8));
    }
}

package com.carddemo.programs.paudblod;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.paudblod.copybook.CobolFields;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;

class CobolFieldsTest {

    @Test
    void packedRoundTripsSignedValuesWithScale() {
        byte[] b = new byte[6];
        CobolFields.encodePacked(new BigDecimal("-1234567.89"), 11, 2, b, 0);
        assertEquals("00123456789D", CobolFields.hex(b, 0, 6));
        assertEquals(new BigDecimal("-1234567.89"), CobolFields.decodePacked(b, 0, 6, 2));
    }

    @Test
    void packedMoveTruncatesExtraDecimalsAndHighOrderDigits() {
        byte[] b = new byte[6];
        CobolFields.encodePacked(new BigDecimal("123456789012.999"), 11, 2, b, 0);
        assertEquals(new BigDecimal("456789012.99"), CobolFields.decodePacked(b, 0, 6, 2));
    }

    @Test
    void packedNegativeZeroIsStoredPositive() {
        byte[] b = new byte[3];
        CobolFields.encodePacked(new BigDecimal("-0.001"), 5, 2, b, 0);
        assertEquals("00000C", CobolFields.hex(b, 0, 3));
    }

    @Test
    void isNumericAcceptsSignNibblesCdfOnly() {
        assertTrue(CobolFields.isPackedNumeric(new byte[] {0x00, 0x1C}, 0, 2));
        assertTrue(CobolFields.isPackedNumeric(new byte[] {0x00, 0x1D}, 0, 2));
        assertTrue(CobolFields.isPackedNumeric(new byte[] {0x00, 0x1F}, 0, 2));
        assertFalse(CobolFields.isPackedNumeric(new byte[] {0x00, 0x1A}, 0, 2));
        assertFalse(CobolFields.isPackedNumeric(new byte[] {0x00, 0x1E}, 0, 2));
        assertFalse(CobolFields.isPackedNumeric(new byte[] {0x0A, 0x1C}, 0, 2));
        assertFalse(CobolFields.isPackedNumeric(new byte[] {0x40, 0x40}, 0, 2));
    }

    @Test
    void decodePackedRejectsInvalidData() {
        assertThrows(IllegalArgumentException.class,
                () -> CobolFields.decodePacked(new byte[] {0x40, 0x40}, 0, 2, 0));
    }

    @Test
    void binaryIsBigEndianSignedAndTruncatedToPicDigits() {
        byte[] b = new byte[2];
        CobolFields.encodeBinary(-2, 4, 2, b, 0);
        assertEquals(-2, CobolFields.decodeBinary(b, 0, 2));
        CobolFields.encodeBinary(12345, 4, 2, b, 0);
        assertEquals(2345, CobolFields.decodeBinary(b, 0, 2));
    }

    @Test
    void zonedAndTextArePaddedAndTruncatedLikeCobolMoves() {
        byte[] b = new byte[6];
        CobolFields.encodeZoned(1234567, 6, b, 0, StandardCharsets.US_ASCII);
        assertEquals("234567", new String(b, StandardCharsets.US_ASCII));
        CobolFields.encodeText("AB", 6, b, 0, StandardCharsets.US_ASCII);
        assertEquals("AB    ", new String(b, StandardCharsets.US_ASCII));
        CobolFields.encodeText("ABCDEFGH", 6, b, 0, StandardCharsets.US_ASCII);
        assertEquals("ABCDEF", new String(b, StandardCharsets.US_ASCII));
        assertThrows(IllegalArgumentException.class,
                () -> CobolFields.decodeZoned("12 4".getBytes(StandardCharsets.US_ASCII), 0, 4,
                        StandardCharsets.US_ASCII));
    }
}

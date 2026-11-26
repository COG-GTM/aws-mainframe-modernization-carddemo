package com.carddemo.programs.cbstm03a.support;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class PicTest {

    @Test
    void alphanumericMovePadsAndTruncates() {
        assertEquals("AB   ", Pic.x("AB", 5));
        assertEquals("ABC", Pic.x("ABCDEF", 3));
        assertEquals("   ", Pic.x(null, 3));
    }

    @Test
    void unsignedDisplayDecodeAndEncode() {
        assertEquals(1234L, Pic.unsigned("001234"));
        assertEquals(0L, Pic.unsigned("     "));
        assertEquals("000042", Pic.unsigned(42, 6));
        assertEquals("345", Pic.unsigned(12345, 3));
    }

    @Test
    void zonedDecimalTrailingOverpunch() {
        assertEquals(new BigDecimal("194.00"), Pic.zoned("00000001940{", 10, 2));
        assertEquals(new BigDecimal("504.77"), Pic.zoned("0000005047G", 9, 2));
        assertEquals(new BigDecimal("-47.88"), Pic.zoned("0000000478Q", 9, 2));
        assertEquals(new BigDecimal("-0.00"), Pic.zoned("0000000000}", 9, 2));
        assertEquals(new BigDecimal("12.30"), Pic.zoned("00000001230", 9, 2));
        assertEquals("0000000478Q", Pic.zoned(new BigDecimal("-47.88"), 9, 2));
        assertEquals("0000005047G", Pic.zoned(new BigDecimal("504.77"), 9, 2));
        IllegalArgumentException e =
                assertThrows(IllegalArgumentException.class, () -> Pic.zoned("12345678A00", 9, 2));
        assertEquals("invalid zoned decimal byte at position 9 of 11", e.getMessage());
    }

    @Test
    void storeTruncatesDecimalsAndHighOrderDigitsWithoutRounding() {
        assertEquals(new BigDecimal("1.99"), Pic.store(new BigDecimal("1.999"), 9, 2));
        assertEquals(new BigDecimal("-1.99"), Pic.store(new BigDecimal("-1.999"), 9, 2));
        assertEquals(new BigDecimal("234567890.12"), Pic.store(new BigDecimal("1234567890.12"), 9, 2));
    }

    @Test
    void editNinesKeepsLeadingZerosAndTrailingSign() {
        assertEquals("000000492.00 ", Pic.editNines(new BigDecimal("492.00"), 9));
        assertEquals("000001234.56-", Pic.editNines(new BigDecimal("-1234.56"), 9));
        // PIC S9(10)V99 into 9(9).99- drops the high-order digit
        assertEquals("234567890.12 ", Pic.editNines(new BigDecimal("1234567890.12"), 9));
    }

    @Test
    void editZeroSuppressedBlanksLeadingZeros() {
        assertEquals("      183.88 ", Pic.editZeroSuppressed(new BigDecimal("183.88"), 9));
        assertEquals("       47.88-", Pic.editZeroSuppressed(new BigDecimal("-47.88"), 9));
        assertEquals("         .00 ", Pic.editZeroSuppressed(BigDecimal.ZERO, 9));
        assertEquals("         .05-", Pic.editZeroSuppressed(new BigDecimal("-0.05"), 9));
    }
}

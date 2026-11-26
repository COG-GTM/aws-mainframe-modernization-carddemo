package com.carddemo.programs.cbimport.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PicTest {

    @Test
    void alnumPadsAndTruncatesOnTheRight() {
        assertEquals("AB   ", Pic.alnum("AB", 5));
        assertEquals("ABCDE", Pic.alnum("ABCDEFG", 5));
        assertEquals("   ", Pic.alnum(null, 3));
    }

    @Test
    void unsignedMoveTruncatesHighOrderDigitsAndDropsSign() {
        assertEquals(294_967_295L, Pic.unsigned(new BigInteger("4294967295"), 9));
        assertEquals(123L, Pic.unsigned(-123, 3));
        assertEquals(234L, Pic.unsigned(1234, 3));
    }

    @Test
    void signedMoveTruncatesDecimalsWithoutRoundingAndHighOrderDigits() {
        assertEquals(new BigDecimal("1.99"), Pic.signed(new BigDecimal("1.999"), 10, 2));
        assertEquals(new BigDecimal("-1.99"), Pic.signed(new BigDecimal("-1.999"), 10, 2));
        assertEquals(new BigDecimal("2345678901.23"), Pic.signed(new BigDecimal("12345678901.23"), 10, 2));
        assertEquals(new BigDecimal("-2345678901.23"), Pic.signed(new BigDecimal("-12345678901.23"), 10, 2));
    }

    @Test
    void displayUnsignedIsZeroPadded() {
        assertEquals("000000042", Pic.displayUnsigned(42, 9));
    }

    @Test
    void displaySignedUsesTrailingOverpunch() {
        assertEquals("00000000194{", Pic.displaySigned(new BigDecimal("19.40"), 10, 2));
        assertEquals("0000005047G", Pic.displaySigned(new BigDecimal("504.77"), 9, 2));
        assertEquals("0000000919}", Pic.displaySigned(new BigDecimal("-91.90"), 9, 2));
        assertEquals("0000000000R", Pic.displaySigned(new BigDecimal("-0.09"), 9, 2));
        assertEquals("00000000000{", Pic.displaySigned(BigDecimal.ZERO, 10, 2));
    }

    @Test
    void parseSignedAcceptsIbmAndMicroFocusOverpunch() {
        assertEquals(new BigDecimal("504.77"), Pic.parseSigned("0000005047G", 2));
        assertEquals(new BigDecimal("-91.90"), Pic.parseSigned("0000000919}", 2));
        assertEquals(new BigDecimal("-91.93"), Pic.parseSigned("000000919s", 2));
        assertEquals(new BigDecimal("1.23"), Pic.parseSigned("00123", 2));
    }

    @Test
    void parseUnsignedUsesDigitPortionOfNonNumericBytes() {
        assertEquals(BigInteger.valueOf(120), Pic.parseUnsigned("12 "));
    }
}

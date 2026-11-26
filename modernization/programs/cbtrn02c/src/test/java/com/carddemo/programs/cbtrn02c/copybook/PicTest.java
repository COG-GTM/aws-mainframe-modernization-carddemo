package com.carddemo.programs.cbtrn02c.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PicTest {

    @Test
    void alnumPadsAndTruncatesOnTheRight() {
        assertEquals("AB   ", Pic.alnum("AB", 5));
        assertEquals("ABC", Pic.alnum("ABCDE", 3));
        assertEquals("   ", Pic.alnum(null, 3));
    }

    @Test
    void unsignedZeroFillsAndDropsHighOrderDigits() {
        assertEquals("00042", Pic.unsigned(42, 5));
        assertEquals("345", Pic.unsigned(12345, 3));
        assertEquals("007", Pic.unsigned(-7, 3));
    }

    @Test
    void signedUsesTrailingOverpunch() {
        assertEquals("0000005047G", Pic.signed(new BigDecimal("504.77"), 9, 2));
        assertEquals("0000009190}", Pic.signed(new BigDecimal("-919.00"), 9, 2));
        assertEquals("0000000000{", Pic.signed(BigDecimal.ZERO, 9, 2));
        assertEquals("00000001940{", Pic.signed(new BigDecimal("194.00"), 10, 2));
        assertEquals("0000000012L", Pic.signed(new BigDecimal("-1.239"), 9, 2));
    }

    @Test
    void parseSignedAcceptsEbcdicOverpunchAsciiSignAndPlainDigits() {
        assertEquals(new BigDecimal("504.77"), Pic.parseSigned("0000005047G", 2));
        assertEquals(new BigDecimal("-919.00"), Pic.parseSigned("0000009190}", 2));
        assertEquals(new BigDecimal("-1.21"), Pic.parseSigned("0000000012J", 2));
        assertEquals(new BigDecimal("-1.21"), Pic.parseSigned("0000000012q", 2));
        assertEquals(new BigDecimal("12.34"), Pic.parseSigned("00000001234", 2));
    }

    @Test
    void parseRejectsNonNumeric() {
        assertThrows(NumberFormatException.class, () -> Pic.parseSigned("00000000AB{", 2));
        assertThrows(NumberFormatException.class, () -> Pic.parseSigned("0000000000*", 2));
        assertThrows(NumberFormatException.class, () -> Pic.parseUnsigned("12 4"));
    }

    @Test
    void truncateDropsHighOrderDigitsAndExcessDecimalsKeepingSign() {
        assertEquals(new BigDecimal("234567890.12"), Pic.truncate(new BigDecimal("1234567890.129"), 9, 2));
        assertEquals(new BigDecimal("-0.01"), Pic.truncate(new BigDecimal("-1000000000.01"), 9, 2));
        assertEquals(new BigDecimal("5.00"), Pic.truncate(new BigDecimal("5"), 9, 2));
    }
}

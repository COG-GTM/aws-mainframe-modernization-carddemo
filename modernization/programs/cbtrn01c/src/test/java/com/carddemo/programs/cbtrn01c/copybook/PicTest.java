package com.carddemo.programs.cbtrn01c.copybook;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PicTest {

    @Test
    void alphaPadsRightAndTruncatesRight() {
        assertEquals("AB   ", Pic.alpha("AB", 5));
        assertEquals("ABCDE", Pic.alpha("ABCDEFG", 5));
        assertEquals("   ", Pic.alpha(null, 3));
    }

    @Test
    void unsignedDropsSignAndHighOrderDigits() {
        assertEquals(2345, Pic.unsigned(12345, 4));
        assertEquals(7, Pic.unsigned(-7, 4));
        assertEquals("0042", Pic.formatUnsigned(42, 4));
    }

    @Test
    void signedTruncatesDecimalsWithoutRoundingAndHighOrderDigits() {
        assertEquals(new BigDecimal("1.23"), Pic.signed(new BigDecimal("1.239"), 9, 2));
        assertEquals(new BigDecimal("-1.23"), Pic.signed(new BigDecimal("-1.239"), 9, 2));
        assertEquals(new BigDecimal("234567890.12"), Pic.signed(new BigDecimal("1234567890.12"), 9, 2));
    }

    @ParameterizedTest
    @CsvSource({
            "0000005047G, 504.77",
            "0000009190}, -919.00",
            "0000000000{, 0.00",
            "0000000123R, -12.39",
            "00000001239, 12.39"
    })
    void parsesTrailingOverpunchSign(String zoned, String expected) {
        assertEquals(new BigDecimal(expected), Pic.parseSigned(zoned, 2));
    }

    @ParameterizedTest
    @CsvSource({
            "504.77, 0000005047G",
            "-919.00, 0000009190}",
            "0.00, 0000000000{",
            "-12.39, 0000000123R"
    })
    void formatsTrailingOverpunchSign(String value, String expected) {
        assertEquals(expected, Pic.formatSigned(new BigDecimal(value), 9, 2));
    }

    @Test
    void rejectsNonNumericData() {
        assertThrows(IllegalArgumentException.class, () -> Pic.parseUnsigned("12 4"));
        assertThrows(IllegalArgumentException.class, () -> Pic.parseSigned("12 4{", 2));
        assertThrows(IllegalArgumentException.class, () -> Pic.parseSigned("1234*", 2));
    }

    @Test
    void readerRejectsOverlongImage() {
        assertThrows(IllegalArgumentException.class, () -> new Pic.Reader("ABC", 2));
    }
}

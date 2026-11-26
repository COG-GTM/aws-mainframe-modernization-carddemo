package com.carddemo.programs.cbact04c.copybook;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PicTest {

    @ParameterizedTest
    @CsvSource({
            "0.00, 00000000000{",
            "194.00, 00000001940{",
            "12.51, 00000000125A",
            "-12.51, 00000000125J",
            "-0.10, 00000000001}",
            "9999999999.99, 99999999999I"
    })
    void signedZonedEncodeUsesTrailingOverpunch(String value, String expected) {
        assertEquals(expected, Pic.signedZoned(new BigDecimal(value), 10, 2));
    }

    @ParameterizedTest
    @CsvSource({
            "00000001940{, 194.00",
            "00000000125A, 12.51",
            "00000000125J, -12.51",
            "00000000125q, -12.51",
            "000000001251, 12.51"
    })
    void signedZonedDecode(String image, String expected) {
        assertEquals(new BigDecimal(expected), Pic.parseSignedZoned(image, 2));
    }

    @Test
    void signedZonedRejectsGarbage() {
        assertThrows(IllegalArgumentException.class, () -> Pic.parseSignedZoned("0000A000000{", 2));
        assertThrows(IllegalArgumentException.class, () -> Pic.parseSignedZoned("00000000000#", 2));
    }

    @Test
    void truncateDropsLowAndHighOrderDigitsKeepingSign() {
        assertEquals(new BigDecimal("1.85"), Pic.truncate(new BigDecimal("1.85072"), 9, 2));
        assertEquals(new BigDecimal("-0.12"), Pic.truncate(new BigDecimal("-0.125125"), 9, 2));
        assertEquals(new BigDecimal("333324999.91"), Pic.truncate(new BigDecimal("8333324999.9166"), 9, 2));
        assertEquals(new BigDecimal("-23.45"), Pic.truncate(new BigDecimal("-1000000023.456"), 9, 2));
    }

    @Test
    void unsignedMoveTruncatesHighOrderAndPads() {
        assertEquals("000005", Pic.unsigned(5, 6));
        assertEquals("000000", Pic.unsigned(1_000_000, 6));
        assertEquals("00000000042", Pic.unsigned(42, 11));
    }

    @Test
    void alphaMovePadsAndTruncates() {
        assertEquals("DEFAULT   ", Pic.alpha("DEFAULT", 10));
        assertEquals("AB", Pic.alpha("ABC", 2));
    }

    @Test
    void stringIntoOverwritesPrefixAndLeavesRemainder() {
        assertEquals("Int. for a/c 00000000001XXXXX",
                Pic.stringInto("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX", 29, "Int. for a/c ", "00000000001"));
        assertEquals("2022071800000001", Pic.stringInto("", 16, "2022071800", "000001"));
    }
}

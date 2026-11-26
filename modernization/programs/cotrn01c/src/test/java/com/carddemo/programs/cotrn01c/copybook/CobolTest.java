package com.carddemo.programs.cotrn01c.copybook;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CobolTest {

    /** Expected values cross-checked with GnuCOBOL (cobc -std=ibm) MOVE S9(09)V99 TO +99999999.99. */
    @ParameterizedTest
    @CsvSource({
        "123456789.12, +23456789.12",
        "-504.77, -00000504.77",
        "0, +00000000.00",
        "0.00, +00000000.00",
        "504.77, +00000504.77",
        "-999999999.99, -99999999.99",
        "12.345, +00000012.34",
        "-0.019, -00000000.01"
    })
    void editPlus8Dot2_signTruncationAndScale(String in, String expected) {
        assertEquals(expected, Cobol.editPlus8Dot2(new BigDecimal(in)));
        assertEquals(12, expected.length());
    }

    @ParameterizedTest
    @CsvSource({
        "0000005047G, 504.77",
        "0000009190}, -919.00",
        "0000000567P, -56.77",
        "0000000000{, 0.00",
        "0000000123J, -12.31",
        "00000001234, 12.34"
    })
    void parseZonedSigned_trailingOverpunch(String raw, String expected) {
        assertEquals(new BigDecimal(expected), Cobol.parseZonedSigned(raw, 2));
    }

    @ParameterizedTest
    @CsvSource({
        "504.77, 0000005047G",
        "-919.00, 0000009190}",
        "-56.77, 0000000567P",
        "0, 0000000000{",
        "1234567890.12, 2345678901B"
    })
    void formatZonedSigned_trailingOverpunchWithHighOrderTruncation(String value, String expected) {
        assertEquals(expected, Cobol.formatZonedSigned(new BigDecimal(value), 9, 2));
    }

    @Test
    void parseZonedSigned_rejectsInvalidSign() {
        assertThrows(IllegalArgumentException.class, () -> Cobol.parseZonedSigned("0000000000X", 2));
    }

    @Test
    void moveAlnum_padsAndTruncates() {
        assertEquals("AB   ", Cobol.moveAlnum("AB", 5));
        assertEquals("ABCDE", Cobol.moveAlnum("ABCDEFG", 5));
        assertEquals("     ", Cobol.moveAlnum(null, 5));
    }

    @Test
    void moveUnsignedNumeric_zeroFillsAndKeepsLowOrderDigits() {
        assertEquals("0001", Cobol.moveUnsignedNumeric(1, 4));
        assertEquals("2345", Cobol.moveUnsignedNumeric(12345, 4));
    }

    @Test
    void spacesOrLowValues_isAllOneOrTheOther() {
        assertTrue(Cobol.isSpacesOrLowValues("    "));
        assertTrue(Cobol.isSpacesOrLowValues(Cobol.lowValues(4)));
        assertFalse(Cobol.isSpacesOrLowValues(" \u0000  "));
        assertFalse(Cobol.isSpacesOrLowValues("  1 "));
    }

    /** Matches GnuCOBOL: DISPLAY 'RESP:' R 'REAS:' Q with R = 13, Q = -7 (PIC S9(09) COMP). */
    @Test
    void displayBinaryS9_matchesGnuCobol() {
        assertEquals("RESP:+0000000013REAS:-0000000007",
                "RESP:" + Cobol.displayBinaryS9(13) + "REAS:" + Cobol.displayBinaryS9(-7));
    }
}

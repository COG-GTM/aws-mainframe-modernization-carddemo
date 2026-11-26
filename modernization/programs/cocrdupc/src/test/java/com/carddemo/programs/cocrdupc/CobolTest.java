package com.carddemo.programs.cocrdupc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class CobolTest {

    @Test
    void alphanumericMovePadsAndTruncates() {
        assertEquals("AB   ", Cobol.alnum("AB", 5));
        assertEquals("COCRDUP", Cobol.alnum("COCRDUP ", 7));
        assertEquals("   ", Cobol.alnum(null, 3));
    }

    @Test
    void numericMoveTruncatesHighOrderDigitsAndDropsSign() {
        assertEquals("00000000050", Cobol.numeric(50, 11));
        assertEquals("345", Cobol.numeric(12345, 3));
        assertEquals("013", Cobol.numeric(-13, 3));
    }

    @Test
    void numericClassTestRequiresEveryByteToBeADigit() {
        assertTrue(Cobol.isNumeric("00000000050"));
        assertFalse(Cobol.isNumeric("1234       "));
        assertFalse(Cobol.isNumeric("12A"));
    }

    @Test
    void displayValueUsesLowNibbleLikePack() {
        assertEquals(5, Cobol.displayValue(" 5"));
        assertEquals(0, Cobol.displayValue(Cobol.lowValues(4)));
        assertEquals(12340000000L, Cobol.displayValue("1234       "));
        assertTrue(Cobol.displayIsZero("00000000000"));
        assertTrue(Cobol.displayIsZero("   "));
        assertFalse(Cobol.displayIsZero("0001"));
    }

    @Test
    void figurativeConstantComparisons() {
        assertTrue(Cobol.isSpaces("   "));
        assertTrue(Cobol.isLowValues(Cobol.lowValues(2)));
        assertTrue(Cobol.isZeros("000"));
        assertFalse(Cobol.isSpaces(" A "));
        assertTrue(Cobol.eq("*          ", "*"));
    }

    @Test
    void upperCaseConvertsOnlyAsciiLetters() {
        assertEquals("ANIYA VON 1-É", Cobol.upper("Aniya von 1-É"));
    }

    @Test
    void rightJustifyPadsOnTheLeft() {
        assertEquals(" 5", Cobol.rightJustify("5", 2));
        assertEquals("2025", Cobol.rightJustify("2025", 4));
    }
}

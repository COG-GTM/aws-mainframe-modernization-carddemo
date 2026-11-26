package com.carddemo.programs.cbact02c.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PicTest {

    @Test
    void alphaPadsAndTruncates() {
        assertEquals("AB  ", Pic.alpha("AB", 4));
        assertEquals("ABCD", Pic.alpha("ABCDEF", 4));
        assertEquals("   ", Pic.alpha(null, 3));
    }

    @Test
    void unsignedNumericKeepsLowOrderDigits() {
        assertEquals("007", Pic.unsignedNumeric(7, 3));
        assertEquals("234", Pic.unsignedNumeric(1234, 3));
        assertThrows(IllegalArgumentException.class, () -> Pic.unsignedNumeric(-1, 3));
    }

    @Test
    void numericClassTest() {
        assertTrue(Pic.isNumeric("0123"));
        assertFalse(Pic.isNumeric("12 "));
        assertFalse(Pic.isNumeric("1A"));
        assertFalse(Pic.isNumeric(""));
    }
}

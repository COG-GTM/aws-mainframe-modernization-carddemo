package com.carddemo.programs.corpt00c;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CobolTextTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "'5 '|5", "' 5'|5", "12|12", "-5|-5", "'5-'|-5", "'12CR'|-12", "'7 DB'|-7",
            "'$1,234.56'|1234.56", "'1.9'|1.9", "'+ 3'|3", "ab|0", "'1a'|0", "'  '|0", "'.'|0", "'5-5'|0",
    })
    void numvalC(String arg, String expected) {
        assertEquals(0, new BigDecimal(expected).compareTo(CobolText.numvalC(arg)));
    }

    @Test
    void numvalCOfLowValuesIsZero() {
        assertEquals(0, BigDecimal.ZERO.compareTo(CobolText.numvalC(CobolText.lowValues(2))));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"5|2|05", "-7|2|07", "1.9|2|01", "123|2|23", "2024|4|2024", "24|4|0024"})
    void toUnsignedDisplayTruncates(String value, int digits, String expected) {
        assertEquals(expected, CobolText.toUnsignedDisplay(new BigDecimal(value), digits));
    }

    @Test
    void moveAndClassTests() {
        assertEquals("ab  ", CobolText.move("ab", 4));
        assertEquals("ab", CobolText.move("abcd", 2));
        assertEquals(CobolText.lowValues(3), CobolText.move(null, 3));
        assertTrue(CobolText.isSpacesOrLowValues("   "));
        assertTrue(CobolText.isSpacesOrLowValues(CobolText.lowValues(2)));
        assertFalse(CobolText.isSpacesOrLowValues(" \u0000"));
        assertTrue(CobolText.isNumeric("0123"));
        assertFalse(CobolText.isNumeric("1 "));
    }

    @Test
    void stringIntoOverlaysAndDelimits() {
        assertEquals("Monthly report  ", CobolText.stringInto(CobolText.spaces(16),
                CobolText.delimitedBySpace("Monthly   "), " report"));
        assertEquals("abc", CobolText.stringInto("xyz", "abcdef"));
        assertEquals("abz", CobolText.stringInto("xyz", "ab"));
    }

    @Test
    void displaySignedOverpunches() {
        assertEquals("00000004D", CobolText.displaySigned(44, 9));
        assertEquals("00000000{", CobolText.displaySigned(0, 9));
        assertEquals("00000001J", CobolText.displaySigned(-11, 9));
    }

    @Test
    void zonedDecimalRoundTrip() {
        assertEquals(new BigDecimal("504.77"), CobolText.decodeZonedSigned("0000005047G", 2));
        assertEquals(new BigDecimal("-919.00"), CobolText.decodeZonedSigned("0000009190}", 2));
        assertEquals("0000009190}", CobolText.encodeZonedSigned(new BigDecimal("-919.00"), 9, 2));
        assertEquals("0000005047G", CobolText.encodeZonedSigned(new BigDecimal("504.77"), 9, 2));
    }
}

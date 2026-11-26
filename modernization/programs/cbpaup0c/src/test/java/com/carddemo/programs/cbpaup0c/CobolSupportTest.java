package com.carddemo.programs.cbpaup0c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

/** Expected values were confirmed with GnuCOBOL 3.1.2 (default dialect, TRUNC(STD)-like truncation). */
class CobolSupportTest {

    @Test
    void signedHalfwordTruncation() {
        assertEquals(0, CobolSupport.truncateSigned(-10000, 4));     // -9999 - 1
        assertEquals(4001, CobolSupport.truncateSigned(14001, 4));   // 26001 - 12000
        assertEquals(-8999, CobolSupport.truncateSigned(-98999, 4)); // 1000 - 99999
    }

    @Test
    void unsignedTruncationDropsSign() {
        assertEquals(0, CobolSupport.truncateUnsigned(100000, 5));
        assertEquals(99899, CobolSupport.truncateUnsigned(99899, 5));
        assertEquals(5, CobolSupport.truncateUnsigned(-5, 5));
    }

    @Test
    void decimalTruncationKeepsNineIntegerDigits() {
        assertEquals(new BigDecimal("-0.99"),
                CobolSupport.truncateSigned(new BigDecimal("-1000000000.99"), 9, 2));
        assertEquals(new BigDecimal("123.45"), CobolSupport.truncateSigned(new BigDecimal("123.459"), 9, 2));
    }

    @Test
    void numericToAlphanumericComparison() {
        assertTrue(CobolSupport.compareNumericToAlphanumeric(6, 8, "5    ") < 0);
        assertTrue(CobolSupport.compareNumericToAlphanumeric(2, 8, "00001") < 0);
        assertTrue(CobolSupport.compareNumericToAlphanumeric(10000, 8, "00001") > 0);
        assertTrue(CobolSupport.compareNumericToAlphanumeric(1, 8, "00000") > 0);
        assertTrue(CobolSupport.compareNumericToAlphanumeric(2, 8, "1    ") < 0);
    }

    @Test
    void comparisonUsesEbcdicCollation() {
        // EBCDIC: letters (X'C1'..) sort below digits (X'F0'..), unlike ASCII.
        assertTrue(CobolSupport.compareAlphanumeric("A", "0") < 0);
        assertTrue(CobolSupport.compareNumericToAlphanumeric(1, 8, "ABCDE") > 0);
        assertTrue(CobolSupport.compareAlphanumeric(" ", "0") < 0);
    }

    @Test
    void displayFormats() {
        assertEquals("+00000022", CobolSupport.displaySigned(22, 8));
        assertEquals("-00000005", CobolSupport.displaySigned(-5, 8));
        assertEquals("+00000000001", CobolSupport.displaySigned(1, 11));
        assertEquals("00000000048", CobolSupport.displayUnsigned(48, 11));
        assertEquals("23324", CobolSupport.displayUnsigned(23324, 5));
    }

    @Test
    void isNumeric() {
        assertTrue(CobolSupport.isNumeric("00"));
        assertFalse(CobolSupport.isNumeric("0 "));
        assertFalse(CobolSupport.isNumeric("7A"));
        assertFalse(CobolSupport.isNumeric("-1"));
    }
}

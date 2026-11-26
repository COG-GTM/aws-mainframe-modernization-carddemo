package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.support.CobolText;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Intrinsic-function semantics, cross-checked against GnuCOBOL 3 (-std=ibm). */
class CobolTextTest {

    @ParameterizedTest
    @ValueSource(strings = {"12", " 7", "1 ", "-1", "+1", "1-", "1+", ".5", "5.", "1CR", "2 DB"})
    void testNumvalAccepts(String s) {
        assertEquals(0, CobolText.testNumval(s));
    }

    @ParameterizedTest
    @ValueSource(strings = {"AB", "1A", "1 2", "+", ".", "  ", "\u0000\u0000", "+1-", "1..2"})
    void testNumvalRejects(String s) {
        assertNotEquals(0, CobolText.testNumval(s));
    }

    @Test
    void numvalValues() {
        assertEquals(0, new BigDecimal("-1").compareTo(CobolText.numval("-1")));
        assertEquals(0, new BigDecimal("0.5").compareTo(CobolText.numval(".5")));
        assertEquals(0, new BigDecimal("-12").compareTo(CobolText.numval("12CR")));
        assertEquals(0, new BigDecimal("7").compareTo(CobolText.numval(" 7")));
    }

    @Test
    void unsignedDisplayTruncates() {
        assertEquals("01", CobolText.toUnsignedDisplay(new BigDecimal("-1"), 2));
        assertEquals("00", CobolText.toUnsignedDisplay(new BigDecimal("0.5"), 2));
        assertEquals("23", CobolText.toUnsignedDisplay(new BigDecimal("123.9"), 2));
    }

    @Test
    void editedSqlcode() {
        assertEquals("    0", CobolText.editMinus4Z9(0));
        assertEquals("  100", CobolText.editMinus4Z9(100));
        assertEquals(" -911", CobolText.editMinus4Z9(-911));
        assertEquals("   -1", CobolText.editMinus4Z9(-1));
        assertEquals("-9999", CobolText.editMinus4Z9(-9999));
    }

    @Test
    void trimOnlyRemovesSpaces() {
        assertEquals("", CobolText.trim("     "));
        assertEquals("a b", CobolText.trim("  a b  "));
        assertEquals("\u0000", CobolText.trim("\u0000"));
    }

    @Test
    void stringIntoOverwritesPrefixAndTruncates() {
        // GnuCOBOL: MOVE 'Update of record failed' TO M (X(20)); STRING 'XYZ' ... INTO M -> 'XYZate of record fai'
        assertEquals("XYZate of record fai",
                CobolText.stringInto(CobolText.pic("Update of record failed", 20), "XYZ"));
        assertEquals("ABCDE", CobolText.stringInto("     ", "ABC", "DEFG"));
    }

    @Test
    void comparisonPadsWithSpaces() {
        assertTrue(CobolText.eq("1", "1 "));
        assertTrue(CobolText.eq("", "  "));
        assertFalse(CobolText.eq("1", "\u0000\u0000"));
        assertEquals("ABC\u0000", CobolText.upper("abc\u0000"));
    }
}

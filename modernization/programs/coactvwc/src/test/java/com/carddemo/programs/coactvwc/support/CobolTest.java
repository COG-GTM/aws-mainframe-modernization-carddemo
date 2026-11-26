package com.carddemo.programs.coactvwc.support;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class CobolTest {

    @Test
    void alnumPadsTruncatesAndNullIsLowValues() {
        assertEquals("AB   ", Cobol.alnum("AB", 5));
        assertEquals("ABC", Cobol.alnum("ABCDEF", 3));
        assertEquals("\0\0", Cobol.alnum(null, 2));
    }

    @Test
    void numericClassTest() {
        assertTrue(Cobol.isNumeric("00000000001"));
        assertFalse(Cobol.isNumeric("0000000000 "));
        assertFalse(Cobol.isNumeric("1234567890A"));
        assertFalse(Cobol.isNumeric(""));
    }

    @Test
    void digitsDropsSignAndTruncatesHighOrder() {
        assertEquals("000000013", Cobol.digits(13, 9));
        assertEquals("000000013", Cobol.digits(-13, 9));
        assertEquals("234", Cobol.digits(1234, 3));
    }

    @Test
    void stringIntoStopsOnOverflowAndKeepsTail() {
        assertEquals("ABCxx", Cobol.stringInto("xxxxx", "A", "BC"));
        assertEquals("ABCDE", Cobol.stringInto("xxxxx", "ABC", "DEFG"));
    }

    @Test
    void zonedDecimalOverpunch() {
        assertEquals(new BigDecimal("194.00"), Cobol.zonedDecimal("00000001940{", 2));
        assertEquals(new BigDecimal("12.34"), Cobol.zonedDecimal("00000000123D", 2));
        assertEquals(new BigDecimal("-12.34"), Cobol.zonedDecimal("00000000123M", 2));
        assertEquals(new BigDecimal("-12.30"), Cobol.zonedDecimal("00000000123}", 2));
        assertEquals("00000000123M", Cobol.toZonedDecimal(new BigDecimal("-12.34"), 10, 2));
        assertEquals("00000001940{", Cobol.toZonedDecimal(new BigDecimal("194.00"), 10, 2));
    }

    @Test
    void editedPictureSignZeroSuppressionAndCommas() {
        assertEquals("+        194.00", Cobol.editPlusZzzComma99(new BigDecimal("194.00")));
        assertEquals("+      2,020.00", Cobol.editPlusZzzComma99(new BigDecimal("2020.00")));
        assertEquals("-123,456,789.01", Cobol.editPlusZzzComma99(new BigDecimal("-123456789.01")));
        assertEquals("+           .00", Cobol.editPlusZzzComma99(BigDecimal.ZERO));
        assertEquals("-           .05", Cobol.editPlusZzzComma99(new BigDecimal("-0.05")));
        // S9(10)V99 into a 9-integer-digit edited field: the high-order digit is truncated.
        assertEquals("+234,567,890.12", Cobol.editPlusZzzComma99(new BigDecimal("1234567890.12")));
        assertEquals(15, Cobol.editPlusZzzComma99(new BigDecimal("1")).length());
    }
}

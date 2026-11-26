package com.carddemo.programs.coacct01.support;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SupportTest {

    @Test
    void picPadsAndTruncates() {
        assertEquals("AB   ", CobolText.pic("AB", 5));
        assertEquals("ERROR WHILE READING ACCTF", CobolText.pic("ERROR WHILE READING ACCTFILE", 25));
    }

    @Test
    void stringIntoOverlaysFromPositionOneAndStopsOnOverflow() {
        assertEquals("ABCDxx", CobolText.stringInto("xxxxxx", "AB", "CD"));
        assertEquals("ABC", CobolText.stringInto("xxx", "AB", "CDEF"));
    }

    @Test
    void unsignedDisplayMoveDropsSignAndHighOrderDigits() {
        assertEquals(33, CobolText.unsignedDisplay(2033, 2));
        assertEquals(2033, CobolText.unsignedDisplay(-2033, 5));
        assertEquals("02085", CobolText.digits(2085, 5));
        assertEquals("00000016", CobolText.digits(16, 8));
    }

    @Test
    void zonedDecimalOverpunchRoundTrip() {
        assertEquals("00000001940{", ZonedDecimal.format(new BigDecimal("194.00"), 10, 2));
        assertEquals("00000001234M", ZonedDecimal.format(new BigDecimal("-123.44"), 10, 2));
        assertEquals("00000000000}".replace('}', '{'), ZonedDecimal.format(BigDecimal.ZERO, 10, 2));
        assertEquals(new BigDecimal("194.00"), ZonedDecimal.parse("00000001940{", 2));
        assertEquals(new BigDecimal("-123.44"), ZonedDecimal.parse("00000001234M", 2));
        assertEquals(new BigDecimal("1.05"), ZonedDecimal.parse("000000000105", 2));
        assertThrows(NumberFormatException.class, () -> ZonedDecimal.parse("0000000001X{", 2));
    }

    @Test
    void zonedDecimalMoveTruncatesInsteadOfRounding() {
        assertEquals(new BigDecimal("1.99"), ZonedDecimal.truncate(new BigDecimal("1.999"), 10, 2));
        assertEquals(new BigDecimal("2345678901.00"),
                ZonedDecimal.truncate(new BigDecimal("12345678901"), 10, 2));
        assertEquals(new BigDecimal("-0.01"), ZonedDecimal.truncate(new BigDecimal("-0.019"), 10, 2));
    }

    @Test
    void ebcdicNumericUsesLowNibbleAndRaisesS0c7OnInvalidDigit() {
        assertEquals(1, EbcdicNumeric.value("00000000001"));
        assertEquals(0, EbcdicNumeric.value("           "));
        assertEquals(1, EbcdicNumeric.value("0000000000A"));   // EBCDIC C1
        assertEquals(10, EbcdicNumeric.value("000000000J{"));  // D1, C0
        assertThrows(DataException.class, () -> EbcdicNumeric.value("0000000000:")); // 7A
    }
}

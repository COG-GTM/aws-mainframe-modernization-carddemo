package com.carddemo.poc.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ZonedDecimalTest {

    @Test
    void parsesPositiveOverpunch() {
        assertEquals(new BigDecimal("194.00"), ZonedDecimal.parse("00000001940{", 2, true));
        assertEquals(new BigDecimal("2020.00"), ZonedDecimal.parse("00000020200{", 2, true));
        assertEquals(new BigDecimal("1234567890.19"), ZonedDecimal.parse("12345678901I", 2, true));
    }

    @Test
    void parsesNegativeOverpunch() {
        assertEquals(new BigDecimal("-10.25"), ZonedDecimal.parse("00000000102N", 2, true));
        assertEquals(new BigDecimal("-2500.00"), ZonedDecimal.parse("00000025000}", 2, true));
    }

    @Test
    void parsesUnsignedInteger() {
        assertEquals(50002445L, ZonedDecimal.parseUnsignedLong("000050002445"));
        assertEquals(1L, ZonedDecimal.parseUnsignedLong("00000000001"));
    }

    @Test
    void roundTripsFormat() {
        assertEquals("00000001940{", ZonedDecimal.format(new BigDecimal("194.00"), 12, 2, true));
        assertEquals("00000000102N", ZonedDecimal.format(new BigDecimal("-10.25"), 12, 2, true));
        assertEquals("00000025250{", ZonedDecimal.format(new BigDecimal("2525.00"), 12, 2, true));
        assertEquals("00000000012E", ZonedDecimal.format(new BigDecimal("1.25"), 12, 2, true));
        assertEquals("00000000001", ZonedDecimal.format(BigDecimal.ONE, 11, 0, false));
    }

    @Test
    void rejectsGarbage() {
        assertThrows(NumberFormatException.class, () -> ZonedDecimal.parse("0000000X940{", 2, true));
    }
}

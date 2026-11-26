package com.carddemo.programs.cbact01c.support;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ZonedDecimalTest {

    @ParameterizedTest
    @CsvSource({
            "00000001940{, 194.00",
            "00000020200{, 2020.00",
            "00000001025}, -102.50",
            "00000000000A, 0.01",
            "00000000000R, -0.09",
            "000000000123, 1.23",
            "00000000000}, 0.00"
    })
    void decodesOverpunchedSigns(String field, String expected) {
        assertEquals(new BigDecimal(expected), ZonedDecimal.decode(field, 2));
    }

    @Test
    void encodesPreferredSign() {
        assertEquals("00000010250}", ZonedDecimal.encode(new BigDecimal("-1025.00"), 10, 2, true));
        assertEquals("00000025250{", ZonedDecimal.encode(new BigDecimal("2525.00"), 10, 2, true));
        assertEquals("00000000000{", ZonedDecimal.encode(BigDecimal.ZERO, 10, 2, true));
        assertEquals("00000000042", ZonedDecimal.encode(BigDecimal.valueOf(42), 11, 0, false));
    }

    @Test
    void moveTruncatesHighOrderDigitsAndExcessDecimals() {
        assertEquals("12345F", ZonedDecimal.encode(new BigDecimal("123.4567"), 3, 3, true));
        assertEquals("12345F", ZonedDecimal.encode(new BigDecimal("1123.4567"), 3, 3, true));
        assertEquals("456", ZonedDecimal.encode(new BigDecimal("-123456.9"), 3, 0, false));
    }

    @Test
    void invalidDataIsADataException() {
        assertThrows(DataException.class, () -> ZonedDecimal.decode("0000000 940{", 2));
        assertThrows(DataException.class, () -> ZonedDecimal.decode("00000001940*", 2));
    }
}

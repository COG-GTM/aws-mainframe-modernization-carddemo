package com.carddemo.programs.cbtrn03c.support;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ZonedDecimalTest {

    @ParameterizedTest
    @CsvSource({
        "0000005047G, 504.77",
        "0000009190}, -919.00",
        "0000000678H, 67.88",
        "0000000259R, -25.99",
        "0000007656{, 765.60",
        "00000000123, 1.23",
        "'           ', 0.00",
    })
    void decodesOverpunchedSign(String field, String expected) {
        assertEquals(new BigDecimal(expected), ZonedDecimal.decode(field, 2));
    }

    @ParameterizedTest
    @CsvSource({
        "504.77, 0000005047G",
        "-919.00, 0000009190}",
        "0, 0000000000{",
        "4635.70, 0000046357{",
        "-0.01, 0000000000J",
    })
    void encodesWithPreferredSign(String value, String expected) {
        assertEquals(expected, ZonedDecimal.encode(new BigDecimal(value), 9, 2));
    }

    @Test
    void rejectsInvalidSignByte() {
        assertThrows(IllegalArgumentException.class, () -> ZonedDecimal.decode("0000000000#", 2));
    }

    @ParameterizedTest(name = "{0} truncates to {1}")
    @CsvSource({
        "1000000000.01, 0.01",
        "-1000000001.01, -1.01",
        "123.456, 123.45",
        "-123.459, -123.45",
    })
    void cobolStoreTruncatesHighAndLowOrderDigits(String value, String expected) {
        assertEquals(new BigDecimal(expected), CobolNumeric.truncate(new BigDecimal(value), 9, 2));
    }
}

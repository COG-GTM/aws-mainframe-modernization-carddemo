package com.carddemo.programs.cbact01c.support;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PackedDecimalTest {

    @Test
    void s9_10v99Comp3IsSevenBytes() {
        assertEquals(7, PackedDecimal.byteLength(12));
    }

    @Test
    void encodesPositiveAndNegative() {
        assertArrayEquals(new byte[] {0x00, 0x00, 0x00, 0x02, 0x52, 0x50, 0x0C},
                PackedDecimal.encode(new BigDecimal("2525.00"), 10, 2));
        assertArrayEquals(new byte[] {0x00, 0x00, 0x00, 0x02, 0x50, 0x00, 0x0D},
                PackedDecimal.encode(new BigDecimal("-2500.00"), 10, 2));
        assertArrayEquals(new byte[] {0, 0, 0, 0, 0, 0, 0x0C}, PackedDecimal.encode(BigDecimal.ZERO, 10, 2));
    }

    @Test
    void roundTrips() {
        BigDecimal v = new BigDecimal("-9876543210.12");
        assertEquals(v, PackedDecimal.decode(PackedDecimal.encode(v, 10, 2), 2));
    }

    @Test
    void lowValuesAreInvalidPackedData() {
        assertThrows(DataException.class, () -> PackedDecimal.decode(new byte[7], 2));
    }
}

package com.carddemo.poc.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

class PackedDecimalTest {

    // PIC S9(10)V99 COMP-3 -> 12 digits -> 7 bytes
    private static final int DIGITS = 12;

    @Test
    void formatsPositiveValue() {
        byte[] packed = PackedDecimal.format(new BigDecimal("2525.00"), DIGITS, 2, true);
        // 2525.00 -> unscaled 252500 -> nibbles 0 000000252500 C
        assertArrayEquals(new byte[] {0x00, 0x00, 0x00, 0x02, 0x52, 0x50, 0x0C}, packed);
        assertEquals(7, PackedDecimal.byteLength(DIGITS));
    }

    @Test
    void roundTripsNegativeValue() {
        BigDecimal value = new BigDecimal("-2500.00");
        byte[] packed = PackedDecimal.format(value, DIGITS, 2, true);
        assertEquals(7, packed.length);
        assertEquals((byte) 0x0D, (byte) (packed[6] & 0x0F));
        assertEquals(value, PackedDecimal.parse(packed, 0, packed.length, 2));
    }
}

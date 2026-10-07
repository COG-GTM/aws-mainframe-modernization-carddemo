package com.carddemo.batch.codec;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/** PIC S9(10)V99 COMP-3: 12 digits + sign nibble in 7 bytes. */
class PackedDecimalTest {

    private static byte[] hex(String h) {
        byte[] out = new byte[h.length() / 2];
        for (int i = 0; i < out.length; i++) {
            out[i] = (byte) Integer.parseInt(h.substring(2 * i, 2 * i + 2), 16);
        }
        return out;
    }

    @Test
    void sevenBytesForTwelveDigits() {
        assertEquals(7, PackedDecimal.bytesFor(12));
        assertEquals(7, PackedDecimal.bytesFor(13));
        assertEquals(2, PackedDecimal.bytesFor(3));
        assertEquals(1, PackedDecimal.bytesFor(1));
    }

    @Test
    void decodesPositiveNegativeAndUnsignedSignNibbles() {
        assertEquals(0, new BigDecimal("2525.00").compareTo(PackedDecimal.decode(hex("0000000252500C"), 0, 7, 2)));
        assertEquals(2, PackedDecimal.decode(hex("0000000252500C"), 0, 7, 2).scale());
        assertEquals(0, new BigDecimal("-2500.00").compareTo(PackedDecimal.decode(hex("0000000250000D"), 0, 7, 2)));
        assertEquals(0, new BigDecimal("1005.00").compareTo(PackedDecimal.decode(hex("0000000100500F"), 0, 7, 2)));
        assertEquals(0, PackedDecimal.decode(hex("0000000000000C"), 0, 7, 2).signum());
    }

    @Test
    void encodesCobolSignNibbles() {
        byte[] buf = new byte[7];
        PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal("2525.00"));
        assertArrayEquals(hex("0000000252500C"), buf);
        PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal("1005.00"));
        assertArrayEquals(hex("0000000100500C"), buf);
        PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal("1525.00"));
        assertArrayEquals(hex("0000000152500C"), buf);
        PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal("-2500.00"));
        assertArrayEquals(hex("0000000250000D"), buf);
        PackedDecimal.encode(buf, 0, 7, 2, true, BigDecimal.ZERO);
        assertArrayEquals(hex("0000000000000C"), buf);
        PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal("-0.00"));
        assertArrayEquals(hex("0000000000000C"), buf, "negative zero is written as +0");
    }

    @Test
    void roundTripsExtremes() {
        byte[] buf = new byte[7];
        for (String v : new String[] {"9999999999.99", "-9999999999.99", "0.01", "-0.01", "123456.78"}) {
            PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal(v));
            BigDecimal back = PackedDecimal.decode(buf, 0, 7, 2);
            assertEquals(0, new BigDecimal(v).compareTo(back), "round trip " + v);
            assertEquals(2, back.scale(), "scale " + v);
        }
    }

    @Test
    void lowValuesAreNotAValidPackedDecimal() {
        assertThrows(CodecException.class, () -> PackedDecimal.decode(hex("00000000000000"), 0, 7, 2));
        assertThrows(CodecException.class, () -> PackedDecimal.decode(hex("0000000A52500C"), 0, 7, 2));
    }

    @Test
    void oversizedValuesTruncateHighOrderDigitsLikeACobolMove() {
        byte[] buf = new byte[7];
        PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal("12345678901234.56"));
        assertArrayEquals(hex("4567890123456C"), buf, "16 digits into 13 nibbles keeps the low-order 13");
        PackedDecimal.encode(buf, 0, 7, 2, true, new BigDecimal("1.999"));
        assertArrayEquals(hex("0000000000199C"), buf, "extra decimals are truncated, not rounded");
    }

    @Test
    void initializeWritesPositiveZero() {
        byte[] buf = hex("FFFFFFFFFFFFFF");
        PackedDecimal.initialize(buf, 0, 7);
        assertArrayEquals(hex("0000000000000C"), buf);
    }
}

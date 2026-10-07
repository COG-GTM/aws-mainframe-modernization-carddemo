package com.carddemo.batch.codec;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** PIC S9(10)V99 DISPLAY: ASCII zoned decimal with the sign over-punched on the last digit. */
class ZonedDecimalTest {

    private static byte[] b(String s) {
        return s.getBytes(StandardCharsets.ISO_8859_1);
    }

    private static String s(byte[] b) {
        return new String(b, StandardCharsets.ISO_8859_1);
    }

    @Test
    void decodesPositiveOverpunch() {
        BigDecimal v = ZonedDecimal.decode(b("00000001940{"), 0, 12, 2, true);
        assertEquals(0, new BigDecimal("194.00").compareTo(v));
        assertEquals(2, v.scale());
        assertEquals(0, new BigDecimal("100.50").compareTo(ZonedDecimal.decode(b("00000001005{"), 0, 12, 2, true)));
        assertEquals(0, new BigDecimal("2020.01").compareTo(ZonedDecimal.decode(b("00000020200A"), 0, 12, 2, true)));
        assertEquals(0, new BigDecimal("0.09").compareTo(ZonedDecimal.decode(b("00000000000I"), 0, 12, 2, true)));
    }

    @Test
    void decodesNegativeOverpunch() {
        assertEquals(0, new BigDecimal("-1025.00").compareTo(ZonedDecimal.decode(b("00000010250}"), 0, 12, 2, true)));
        assertEquals(0, new BigDecimal("-75.25").compareTo(ZonedDecimal.decode(b("00000000752N"), 0, 12, 2, true)));
        assertEquals(0, new BigDecimal("-0.09").compareTo(ZonedDecimal.decode(b("00000000000R"), 0, 12, 2, true)));
        assertEquals(0, new BigDecimal("-0.01").compareTo(ZonedDecimal.decode(b("00000000000J"), 0, 12, 2, true)));
    }

    @Test
    void negativeZeroDecodesAsZero() {
        BigDecimal v = ZonedDecimal.decode(b("00000000000}"), 0, 12, 2, true);
        assertEquals(0, v.signum());
        assertEquals(2, v.scale());
        assertTrue(ZonedDecimal.isNegative(b("00000000000}"), 0, 12));
    }

    @Test
    void acceptsGnuCobolNativeNegativeOverpunch() {
        assertEquals(0, new BigDecimal("-1025.00").compareTo(ZonedDecimal.decode(b("00000010250p"), 0, 12, 2, true)));
        assertEquals(0, new BigDecimal("-75.25").compareTo(ZonedDecimal.decode(b("00000000752u"), 0, 12, 2, true)));
    }

    @Test
    void unsignedAndPlainDigitsDecode() {
        assertEquals(0, new BigDecimal("1").compareTo(ZonedDecimal.decode(b("00000000001"), 0, 11, 0, false)));
        assertEquals(0, ZonedDecimal.decode(b("00000000001"), 0, 11, 0, false).scale());
        assertEquals(0, new BigDecimal("194.00").compareTo(ZonedDecimal.decode(b("000000019400"), 0, 12, 2, true)));
    }

    @Test
    void encodesWithOverpunchAndRoundTrips() {
        byte[] buf = new byte[12];
        ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal("194.00"));
        assertEquals("00000001940{", s(buf));
        ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal("-1025.00"));
        assertEquals("00000010250}", s(buf));
        ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal("-2500.00"));
        assertEquals("00000025000}", s(buf));
        ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal("-75.25"));
        assertEquals("00000000752N", s(buf));
        ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal("1525.00"));
        assertEquals("00000015250{", s(buf));
        ZonedDecimal.encode(buf, 0, 12, 2, true, BigDecimal.ZERO);
        assertEquals("00000000000{", s(buf));
        for (String v : new String[] {"0.00", "194.00", "-1025.00", "9999999999.99", "-9999999999.99", "0.01", "-0.01"}) {
            ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal(v));
            assertEquals(0, new BigDecimal(v).compareTo(ZonedDecimal.decode(buf, 0, 12, 2, true)), "round trip " + v);
        }
        byte[] id = new byte[11];
        ZonedDecimal.encode(id, 0, 11, 0, false, new BigDecimal("42"));
        assertEquals("00000000042", s(id));
    }

    @Test
    void encodesWithinASurroundingBuffer() {
        byte[] buf = b("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        ZonedDecimal.encode(buf, 5, 12, 2, true, new BigDecimal("1005.00"));
        assertEquals("ABCDE00000010050{RSTUVWXYZ", s(buf));
    }

    @Test
    void displayRendersDigitsWithTrailingSign() {
        assertEquals("000000019400+", ZonedDecimal.display(b("00000001940{"), 0, 12, true));
        assertEquals("000000102500-", ZonedDecimal.display(b("00000010250}"), 0, 12, true));
        assertEquals("000000007525-", ZonedDecimal.display(b("00000000752N"), 0, 12, true));
        assertEquals("000000000000+", ZonedDecimal.display(b("00000000000{"), 0, 12, true));
        assertEquals("00000000001", ZonedDecimal.display(b("00000000001"), 0, 11, false));
    }

    @Test
    void oversizedValuesTruncateHighOrderDigitsLikeACobolMove() {
        byte[] buf = new byte[12];
        ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal("10000000001.50"));
        assertEquals("00000000015{", s(buf), "13 digits into PIC S9(10)V99 drops the high-order digit");
        ZonedDecimal.encode(buf, 0, 12, 2, true, new BigDecimal("1.999"));
        assertEquals("00000000019I", s(buf), "extra decimals are truncated, not rounded");
        assertThrows(CodecException.class, () -> ZonedDecimal.decode(b("0000000X940{"), 0, 12, 2, true));
        assertThrows(CodecException.class, () -> ZonedDecimal.decode(b("0000000194 {"), 0, 12, 2, true),
                "spaces in a numeric DISPLAY field are rejected rather than silently read as zero");
    }
}

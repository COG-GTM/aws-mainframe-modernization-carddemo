package com.carddemo.programs.copaus0c;

import com.carddemo.programs.copaus0c.cobol.Cobol;
import com.carddemo.programs.copaus0c.cobol.PackedDecimal;
import com.carddemo.programs.copaus0c.cobol.ZonedDecimal;
import com.carddemo.programs.copaus0c.copybook.PendingAuthDetails;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CobolPrimitivesTest {

    @Test
    void numericEditedMinusZ9V99() {
        assertEquals("        0.00", Cobol.editMinusZ9V99(BigDecimal.ZERO, 8));
        assertEquals("-       1.50", Cobol.editMinusZ9V99(new BigDecimal("-1.5"), 8));
        assertEquals(" 99999999.99", Cobol.editMinusZ9V99(new BigDecimal("199999999.99"), 8));
        assertEquals("    1.99", Cobol.editMinusZ9V99(new BigDecimal("1.999"), 4).substring(0, 8));
        assertEquals("  1020.00", Cobol.editMinusZ9V99(new BigDecimal("1020.00"), 5));
    }

    @Test
    void stringIntoDoesNotPad() {
        assertEquals("abXYZf", Cobol.stringInto("abcdef", "ab", "XYZ"));
        assertEquals("1234", Cobol.stringInto("    ", "123", "456"));
    }

    @Test
    void numericClassTest() {
        assertTrue(Cobol.isNumeric("00000000001"));
        assertFalse(Cobol.isNumeric("0000000001 "));
        assertFalse(Cobol.isNumeric("\0\0"));
    }

    @Test
    void unsignedDisplayMoveTruncatesAndDropsSign() {
        assertEquals("013", Cobol.moveToUnsignedDisplay(13, 3));
        assertEquals("234", Cobol.moveToUnsignedDisplay(-1234, 3));
    }

    @Test
    void packedDecimalRoundTrip() {
        byte[] p = PackedDecimal.encode(-12345, 5);
        assertArrayEquals(new byte[] {0x12, 0x34, 0x5D}, p);
        assertEquals(-12345, PackedDecimal.decodeLong(p, 0, 5));
        assertEquals(new BigDecimal("123.45"), PackedDecimal.decode(PackedDecimal.encode(12345, 11), 0, 11, 2));
    }

    @Test
    void authorizationKeyIsPackedImage() {
        String key = PendingAuthDetails.authorizationKey(12345, 123456789);
        assertEquals(8, key.length());
        assertEquals((char) 0x12, key.charAt(0));
        assertEquals((char) 0x5C, key.charAt(2));
        assertEquals((char) 0x9C, key.charAt(7));
    }

    @Test
    void zonedOverpunch() {
        assertEquals(new BigDecimal("194.00"), ZonedDecimal.parse("00000001940{", 2));
        assertEquals(new BigDecimal("-12.34"), ZonedDecimal.parse("00000000123M", 2));
    }
}

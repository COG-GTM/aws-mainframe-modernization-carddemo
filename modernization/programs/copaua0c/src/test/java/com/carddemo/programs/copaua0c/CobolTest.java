package com.carddemo.programs.copaua0c;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.carddemo.programs.copaua0c.support.Cobol;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class CobolTest {

    @Test
    void editMinusZPictureSuppressesLeadingZerosAndUsesFixedSign() {
        assertEquals("          0.00", Cobol.editMinusZ9v2(BigDecimal.ZERO));
        assertEquals("        125.50", Cobol.editMinusZ9v2(new BigDecimal("125.5")));
        assertEquals("-         1.00", Cobol.editMinusZ9v2(new BigDecimal("-1")));
        assertEquals(" 9999999999.99", Cobol.editMinusZ9v2(new BigDecimal("9999999999.99")));
    }

    @Test
    void editPlusPictureTruncatesHighOrderAndDecimals() {
        assertEquals("+0000000125.50", Cobol.editPlus10v2(new BigDecimal("125.5")));
        assertEquals("-0000000001.00", Cobol.editPlus10v2(new BigDecimal("-1")));
        assertEquals("+0000000001.23", Cobol.editPlus10v2(new BigDecimal("1.239")));
        assertEquals("+2345678901.00", Cobol.editPlus10v2(new BigDecimal("12345678901")));
        assertEquals(new BigDecimal("125.50"), Cobol.deEditPlus10v2("+0000000125.50"));
    }

    @Test
    void numvalHandlesSignsSpacesAndInvalidInput() {
        assertEquals(new BigDecimal("125.50"), Cobol.numval("  125.50     "));
        assertEquals(new BigDecimal("-12.3"), Cobol.numval("-12.3"));
        assertEquals(new BigDecimal("-7"), Cobol.numval("7CR"));
        assertEquals(new BigDecimal("4"), Cobol.numval("+4"));
        assertEquals(BigDecimal.ZERO, Cobol.numval("12A"));
        assertEquals(BigDecimal.ZERO, Cobol.numval("             "));
    }

    @Test
    void truncateDropsHighOrderDigitsLikeComputeWithoutSizeError() {
        assertEquals(new BigDecimal("234567890.12"), Cobol.truncate(new BigDecimal("1234567890.129"), 9, 2));
        assertEquals(new BigDecimal("-5.00"), Cobol.truncate(new BigDecimal("-5"), 9, 2));
        assertEquals(1, Cobol.truncateBinary(10001, 4));
    }

    @Test
    void zonedDecimalOverpunchAndDisplayMoves() {
        assertEquals(new BigDecimal("194.00"), Cobol.zoned("00000001940{", 2));
        assertEquals(new BigDecimal("-12.35"), Cobol.zoned("00000000123N", 2));
        assertEquals("000000084", Cobol.unsignedDisplay9(84));
        assertEquals("000000002", Cobol.unsignedDisplay9(-2));
        assertEquals("000123", Cobol.numericDisplay("123", 6));
        assertEquals("34", Cobol.numericDisplay("1234", 2));
        assertEquals("AB  ", Cobol.alpha("AB", 4));
    }
}

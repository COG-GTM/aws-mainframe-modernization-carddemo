package com.carddemo.programs.cotrn01c.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookLayoutTest {

    @Test
    void cocom01y_is160BytesWithFieldOffsetsAnd88Levels() {
        CardDemoCommarea c = new CardDemoCommarea();
        assertEquals(160, c.toFixedString().length());
        c.fromTranid("CT00");
        c.fromProgram("COTRN00C");
        c.userId("USER0001");
        c.userType("A");
        c.custId(123);
        c.acctId(42);
        c.cardNum(4859452612877065L);
        c.lastMap("COTRN0A");
        String s = c.toFixedString();
        assertEquals("CT00COTRN00C", s.substring(0, 12));
        assertEquals("USER0001A0000000123", s.substring(24, 43));
        assertEquals("00000000042", s.substring(118, 129));
        assertEquals("4859452612877065", s.substring(130, 146));
        assertEquals("COTRN0A", s.substring(146, 153));
        assertTrue(c.isUsrtypAdmin());
        assertFalse(c.isUsrtypUser());
        assertTrue(c.isPgmEnter());
        c.pgmContext(CardDemoCommarea.PgmContext.REENTER);
        assertTrue(c.isPgmReenter());
        assertEquals(123, c.custId());
        assertEquals(4859452612877065L, c.cardNum());
    }

    @Test
    void cottl01y_and_csmsg01y_keepPicWidths() {
        assertEquals(40, Cottl01y.CCDA_TITLE01.length());
        assertEquals(40, Cottl01y.CCDA_TITLE02.length());
        assertEquals(40, Cottl01y.CCDA_THANK_YOU.length());
        assertEquals(50, Csmsg01y.CCDA_MSG_INVALID_KEY.length());
        assertEquals(50, Csmsg01y.CCDA_MSG_THANK_YOU.length());
    }

    @Test
    void csdat01y_formatsCurrentDateGroups() {
        Csdat01y d = new Csdat01y();
        d.moveCurrentDate(LocalDateTime.of(2026, 1, 5, 7, 8, 9, 123_456_000));
        assertEquals("01/05/26", d.curdateMmDdYy());
        assertEquals("07:08:09", d.curtimeHhMmSs());
        assertEquals("20260105", d.curdateN());
        assertEquals("07080912", d.curtimeN());
        assertEquals("2026-01-05 07:08:09.123456", d.timestamp());
    }

    @Test
    void cvtra05y_roundTripsFixedWidth350() {
        TranRecord r = new TranRecord("T1", "01", 5, "POS TERM", "desc", new BigDecimal("-12.34"), 7,
                "name", "city", "12345", "4859452612877065", "2022-06-10 19:27:53.000000", "");
        String line = r.format();
        assertEquals(350, line.length());
        assertEquals("0005", line.substring(18, 22));
        assertEquals("0000000123M", line.substring(132, 143));
        assertEquals("000000007", line.substring(143, 152));
        assertEquals(r, TranRecord.parse(line));
    }

    @Test
    void cotrn01Map_fieldWidthsMatchSymbolicMap() {
        int[] expected = {4, 40, 8, 8, 40, 8, 16, 16, 16, 2, 4, 10, 60, 12, 10, 10, 9, 30, 25, 10, 78};
        assertEquals(Arrays.toString(expected),
                Arrays.toString(Arrays.stream(Cotrn1a.Field.values()).mapToInt(Cotrn1a.Field::length).toArray()));
        Cotrn1a m = new Cotrn1a();
        assertTrue(Cobol.isLowValues(m.get(Cotrn1a.Field.TRNIDIN)));
        assertNull(m.cursorField());
        m.setCursor(Cotrn1a.Field.TRNIDIN);
        assertEquals(Cotrn1a.Field.TRNIDIN, m.cursorField());
    }

    @Test
    void dfhaid_keyValues() {
        assertEquals(0x7D, Dfhaid.DFHENTER.ebcdic());
        assertEquals(0xF3, Dfhaid.DFHPF3.ebcdic());
        assertEquals(0x6D, Dfhaid.DFHCLEAR.ebcdic());
    }
}

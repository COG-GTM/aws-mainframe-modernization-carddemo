package com.carddemo.programs.corpt00c.copybook;

import com.carddemo.programs.corpt00c.CobolText;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @Test
    void commareaRoundTripsAt160Bytes() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.setFromTranid("CM00");
        c.setFromProgram("COMEN01C");
        c.setUserId("USER0001");
        c.setUserType("U");
        c.setPgmReenter();
        c.setCustId(123);
        c.setAcctId(4567);
        c.setCardNum(4859452612877065L);
        c.setLastMap("CORPT0A");
        String raw = c.toFixedWidth();
        assertEquals(CarddemoCommarea.LENGTH, raw.length());
        CarddemoCommarea back = CarddemoCommarea.fromFixedWidth(raw);
        assertEquals(raw, back.toFixedWidth());
        assertEquals("000000123", back.custIdText());
        assertEquals(4567, back.acctId());
        assertEquals(4859452612877065L, back.cardNum());
        assertTrue(back.isUsrtypUser());
        assertFalse(back.isUsrtypAdmin());
        assertTrue(back.isPgmReenter());
        assertEquals("1", raw.substring(33, 34));
    }

    @Test
    void mapReceiveSetsLengthsAndLowValues() {
        Corpt0aMap map = new Corpt0aMap();
        map.receive(new Corpt0aInput("X", null, null, "5", null, null, null, null, null, null));
        assertEquals("X", map.monthly.data());
        assertEquals(1, map.monthly.l);
        assertEquals("5 ", map.sdtmm.data());
        assertEquals(CobolText.lowValues(1), map.yearly.data());
        assertEquals(0, map.yearly.l);
        assertEquals(CobolText.lowValues(40), map.title01.data());
        assertEquals(17, map.fields().size());
        map.confirm.cursor();
        assertEquals("CONFIRM", map.toOutput().cursorField());
    }

    @Test
    void aidCodes() {
        assertEquals(Dfhaid.DFHENTER, Dfhaid.fromCode('\''));
        assertEquals(Dfhaid.DFHPF3, Dfhaid.fromCode('3'));
        assertEquals('4', Dfhbmsca.DFHGREEN);
    }

    @Test
    void constantsHaveCopybookLengths() {
        assertEquals(40, ScreenTitle.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitle.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitle.CCDA_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
    }

    @Test
    void dateTimeWorkArea() {
        DateTimeWorkArea dt = new DateTimeWorkArea();
        dt.moveCurrentDate(java.time.LocalDateTime.of(2024, 3, 1, 7, 8, 9, 120_000_000));
        assertEquals(20240301, dt.curdateN());
        assertEquals(12, dt.curtimeMilsec);
        dt.setCurdateN(20231231);
        assertEquals(java.time.LocalDate.of(2023, 12, 31), dt.curdate());
    }
}

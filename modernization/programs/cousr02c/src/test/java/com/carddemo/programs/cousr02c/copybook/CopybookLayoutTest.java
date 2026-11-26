package com.carddemo.programs.cousr02c.copybook;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookLayoutTest {

    @Test
    void secUserDataRoundTripsEightyBytes() {
        String rec = "USER0001FIRSTNAME20CHARACTERSLASTNAME-20-CHARACTERSPASSWORDUFILLER-23-BYTES-LONG!!!";
        SecUserData d = SecUserData.fromRecord(rec);
        assertEquals("USER0001", d.getSecUsrId());
        assertEquals("FIRSTNAME20CHARACTER", d.getSecUsrFname());
        assertEquals(80, d.toRecord().length());
        assertEquals(rec.substring(0, 80), d.toRecord());
    }

    @Test
    void secUserDataSettersTruncateAndPad() {
        SecUserData d = SecUserData.of("TOOLONGUSERID", "A", "B", "PASSWORD99", "UA");
        assertEquals("TOOLONGU", d.getSecUsrId());
        assertEquals(Pic.x("A", 20), d.getSecUsrFname());
        assertEquals("PASSWORD", d.getSecUsrPwd());
        assertEquals("U", d.getSecUsrType());
        assertEquals(Pic.spaces(23), d.getSecUsrFiller());
    }

    @Test
    void commareaLayoutOffsets() {
        CardDemoCommarea c = CardDemoCommarea.blank();
        c.setCdemoFromTranid("CU00");
        c.setCdemoFromProgram("COUSR00C");
        c.setCdemoToProgram("COUSR02C");
        c.setCdemoUserType("A");
        c.setCdemoPgmContext(1);
        c.setCdemoCustId(42);
        c.setCdemoAcctId(12345678901L);
        c.setCdemoCardNum(4111111111111111L);
        c.setCdemoLastMap("COUSR2A");
        c.setCdemoCu02PageNum(7);
        c.setCdemoCu02UsrSelected("USER0001");
        String raw = c.toBytes();

        assertEquals(194, raw.length());
        assertEquals("COUSR00C", raw.substring(4, 12));
        assertEquals("COUSR02C", raw.substring(16, 24));
        assertEquals("A", raw.substring(32, 33));
        assertEquals("1", raw.substring(33, 34));
        assertEquals("000000042", raw.substring(34, 43));
        assertEquals("12345678901", raw.substring(118, 129));
        assertEquals("4111111111111111", raw.substring(130, 146));
        assertEquals("COUSR2A", raw.substring(146, 153));
        assertEquals("00000007", raw.substring(176, 184));
        assertEquals("USER0001", raw.substring(186, 194));
        assertTrue(c.isCdemoUsrtypAdmin());
        assertTrue(c.isCdemoPgmReenter());
        assertEquals(42L, c.cdemoCustIdValue().getAsLong());
        assertEquals(raw, CardDemoCommarea.fromBytes(raw).toBytes());
    }

    @Test
    void commarea88Levels() {
        CardDemoCommarea c = CardDemoCommarea.initialWorkingStorage();
        assertTrue(c.isNextPageNo());
        assertFalse(c.isCdemoPgmEnter());
        assertFalse(c.isCdemoPgmReenter());
        c.setCdemoCu02NextPageFlg("Y");
        assertTrue(c.isNextPageYes());
        c.setCdemoUserType("U");
        assertTrue(c.isCdemoUsrtypUser());
        assertFalse(c.cdemoCustIdValue().isPresent());
        assertThrows(IllegalArgumentException.class, () -> c.setCdemoCustId(-1));
    }

    @Test
    void longCommareaIsTruncated() {
        String raw = "X".repeat(300);
        assertEquals("X".repeat(194), CardDemoCommarea.fromBytes(raw).toBytes());
    }

    @Test
    void dateTimeFormatting() {
        DateTimeFields dt = new DateTimeFields();
        dt.moveCurrentDate(LocalDateTime.of(2009, 3, 7, 4, 5, 6, 780_000_000));
        assertEquals("03/07/09", dt.wsCurdateMmDdYy());
        assertEquals("04:05:06", dt.wsCurtimeHhMmSs());
        assertEquals(20090307L, dt.wsCurdateN());
        assertEquals(4050678L, dt.wsCurtimeN());
        assertEquals("2009-03-07 04:05:06.780000", dt.wsTimestamp());
    }

    @Test
    void picHelpers() {
        assertTrue(Pic.isSpacesOrLowValues("   "));
        assertTrue(Pic.isSpacesOrLowValues("\u0000\u0000"));
        assertFalse(Pic.isSpacesOrLowValues(" \u0000"));
        assertEquals("AB", Pic.delimitedBySpace("AB CD"));
        assertEquals("ABCD", Pic.delimitedBySpace("ABCD"));
        assertEquals("AB  ", Pic.x("AB", 4));
        assertEquals("ABC", Pic.x("ABCDE", 3));
    }

    @Test
    void mapCursorIsFirstFieldWithMinusOneLength() {
        Cousr2aMap map = new Cousr2aMap();
        assertEquals("USRIDIN", map.cursorField().name());
        map.passwd.setCursor();
        assertEquals("PASSWD", map.cursorField().name());
        map.fname.setCursor();
        assertEquals("FNAME", map.cursorField().name());
        map.lowValues();
        assertEquals(0, map.fname.getLength());
        assertEquals(Pic.lowValues(78), map.errmsg.getData());
    }

    @Test
    void constantsHaveCopybookWidths() {
        assertEquals(40, ScreenTitle.CCDA_TITLE01.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals('@', Dfhaid.DFHPF12.eibaid());
        assertEquals('\'', Dfhaid.DFHENTER.eibaid());
    }
}

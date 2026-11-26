package com.carddemo.programs.cousr03c.copybook;

import com.carddemo.programs.cousr03c.Cousr03cCommarea;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookLayoutTest {

    @Test
    void secUserDataIs80BytesWithCopybookOffsets() {
        SecUserData r = SecUserData.of("USER0001", "LAWRENCE", "THOMAS", "PASSWORX", "U");
        byte[] b = r.toBytes();
        assertEquals(80, b.length);
        String s = new String(b, StandardCharsets.ISO_8859_1);
        assertEquals("USER0001", s.substring(0, 8));
        assertEquals("LAWRENCE            ", s.substring(8, 28));
        assertEquals("THOMAS              ", s.substring(28, 48));
        assertEquals("PASSWORX", s.substring(48, 56));
        assertEquals("U", s.substring(56, 57));
        assertEquals(" ".repeat(23), r.usrFiller());
        assertTrue(r.isUser());
        assertFalse(r.isAdmin());
    }

    @Test
    void alphanumericMoveTruncatesOnTheRight() {
        SecUserData r = SecUserData.of("TOOLONGUSERID", "A".repeat(25), "", "", "AU");
        assertEquals("TOOLONGU", r.usrId());
        assertEquals("A".repeat(20), r.usrFname());
        assertEquals("A", r.usrType());
    }

    @Test
    void carddemoCommareaLayout() {
        CarddemoCommarea c = new CarddemoCommarea();
        assertEquals(160, c.length());
        c.fill((byte) ' ');
        c.setFromTranid("CU03");
        c.setFromProgram("COUSR03C");
        c.setToTranid("CA00");
        c.setToProgram("COADM01C");
        c.setUserId("ADMIN001");
        c.setUserType(CarddemoCommarea.UserType.ADMIN);
        c.setPgmContext(CarddemoCommarea.PgmContext.REENTER);
        c.setCustId(1234567890L);
        c.setAcctId(42);
        c.setCardNum(4111111111111111L);
        c.setLastMap("COUSR3A");
        c.setLastMapset("COUSR03");
        String s = new String(c.toBytes(), StandardCharsets.ISO_8859_1);
        assertEquals("CU03COUSR03CCA00COADM01CADMIN001A1", s.substring(0, 34));
        assertEquals("234567890", s.substring(34, 43), "PIC 9(09) truncates high-order digits");
        assertEquals("00000000042", s.substring(118, 129));
        assertEquals("4111111111111111", s.substring(130, 146));
        assertEquals("COUSR3ACOUSR03", s.substring(146, 160));
        assertTrue(c.isUsrtypAdmin());
        assertFalse(c.isUsrtypUser());
        assertTrue(c.isPgmReenter());
        assertFalse(c.isPgmEnter());
        assertEquals(1, c.pgmContext());
    }

    @Test
    void nonNumericPgmContextIsNeitherEnterNorReenter() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.fill(FixedRecord.LOW_VALUE);
        assertFalse(c.isPgmEnter());
        assertFalse(c.isPgmReenter());
        assertThrows(NumberFormatException.class, c::pgmContext);
    }

    @Test
    void cousr03cCommareaAppendsCu03Info() {
        Cousr03cCommarea c = new Cousr03cCommarea();
        assertEquals(194, c.length());
        assertTrue(c.isNextPageNo());
        c.setCu03UsridFirst("ADMIN001");
        c.setCu03UsridLast("USER0005");
        c.setCu03PageNum(3);
        c.setNextPageYes();
        c.setCu03UsrSelFlg("D");
        c.setCu03UsrSelected("USER0002");
        String s = new String(c.toBytes(), StandardCharsets.ISO_8859_1).substring(160);
        assertEquals("ADMIN001USER000500000003YDUSER0002", s);
        assertTrue(c.isNextPageYes());
        assertEquals(3, c.cu03PageNum());
    }

    @Test
    void mapInputAndOutputViewsShareStorage() {
        Cousr03Map m = new Cousr03Map();
        assertEquals(324, m.length());
        m.setI(Cousr03Map.Field.USRIDIN, "USER0001");
        assertEquals("USER0001", m.o(Cousr03Map.Field.USRIDIN));
        m.setO(Cousr03Map.Field.ERRMSG, "x".repeat(80));
        assertEquals("x".repeat(78), m.i(Cousr03Map.Field.ERRMSG));
        m.setL(Cousr03Map.Field.FNAME, -1);
        assertEquals(-1, m.l(Cousr03Map.Field.FNAME));
        m.setC(Cousr03Map.Field.ERRMSG, Dfhbmsca.DFHGREEN);
        assertEquals(Dfhbmsca.DFHGREEN, m.c(Cousr03Map.Field.ERRMSG));
        assertEquals("DFHGREEN", Dfhbmsca.colorName(m.c(Cousr03Map.Field.ERRMSG)));
        // ERRMSGO is the last 78 bytes of the 324-byte map
        byte[] b = m.toBytes();
        assertEquals('x', b[323]);
        assertEquals('x', b[246]);
        assertEquals(Dfhbmsca.DFHGREEN, b[242]);
    }

    @Test
    void dateTimeFormatting() {
        WsDateTime d = new WsDateTime();
        d.moveCurrentDate(LocalDateTime.of(2009, 1, 2, 3, 4, 5, 670_000_000));
        d.setCurdateMm(d.curdateMonth());
        d.setCurdateDd(d.curdateDay());
        d.setCurdateYy(d.curdateYear() % 100);
        d.setCurtimeHh(d.curtimeHours());
        d.setCurtimeMm(d.curtimeMinute());
        d.setCurtimeSs(d.curtimeSecond());
        assertEquals("01/02/09", d.curdateMmDdYy());
        assertEquals("03:04:05", d.curtimeHhMmSs());
        assertEquals(20090102, d.curdateN());
        assertEquals(3040567, d.curtimeN());
        assertEquals("2009-01-02 03:04:05.670000", d.timestamp());
    }

    @Test
    void constantCopybooksKeepPicLengths() {
        assertEquals(40, ScreenTitle.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitle.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitle.CCDA_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
        assertEquals(0xF5, Dfhaid.DFHPF5.ebcdic);
        assertEquals(0x7D, Dfhaid.DFHENTER.ebcdic);
    }
}

package com.carddemo.programs.cousr00c.copybook;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

class CopybookTest {

    @Test
    void secUserDataIs80BytesWithCsusr01yOffsets() {
        String raw = "ABCDEFGH" + "F".repeat(20) + "L".repeat(20) + "PPPPPPPP" + "A" + "x".repeat(23);
        SecUserData d = SecUserData.fromFixedWidth(raw);
        assertEquals("ABCDEFGH", d.secUsrId());
        assertEquals("F".repeat(20), d.secUsrFname());
        assertEquals("L".repeat(20), d.secUsrLname());
        assertEquals("PPPPPPPP", d.secUsrPwd());
        assertEquals("A", d.secUsrType());
        assertEquals("x".repeat(23), d.secUsrFiller());
        assertEquals(raw, d.toFixedWidth());
        assertEquals(SecUserData.RECORD_LENGTH, d.toFixedWidth().length());
    }

    @Test
    void secUserDataMovePadsAndTruncates() {
        SecUserData d = SecUserData.of("AB", "A-very-long-first-name-here", "X", "p", "UA");
        assertEquals("AB      ", d.secUsrId());
        assertEquals("A-very-long-first-na", d.secUsrFname());
        assertEquals("U", d.secUsrType());
        assertEquals(" ".repeat(23), d.secUsrFiller());
        assertFalse(d.toString().contains("p "), "password not exposed by toString");
    }

    @Test
    void commareaRoundTripsAt160BytesWith88Levels() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setFromTranid("CA00");
        c.setFromProgram("COADM01C");
        c.setToTranid("CU00");
        c.setToProgram("COUSR00C");
        c.setUserId("ADMIN001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        c.setCustId(123456789L);
        c.setCustFname("JOHN");
        c.setCardNum(4111111111111111L);
        c.setLastMap("COUSR0A");
        c.setLastMapset("COUSR00");
        String raw = c.toFixedWidth();
        assertEquals(CardDemoCommarea.LENGTH, raw.length());
        assertTrue(raw.startsWith("CA00COADM01CCU00COUSR00CADMIN001A1123456789JOHN"));
        CardDemoCommarea back = CardDemoCommarea.fromFixedWidth(raw);
        assertEquals(raw, back.toFixedWidth());
        assertTrue(back.isUsrtypAdmin());
        assertFalse(back.isUsrtypUser());
        assertTrue(back.isPgmReenter());
        assertFalse(back.isPgmEnter());
        assertEquals(123456789L, back.getCustId());
        assertEquals(4111111111111111L, back.getCardNum());
        assertEquals("COUSR0A", back.getLastMap());
    }

    @Test
    void commareaNumericFieldsTruncateHighOrderDigits() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setCustId(9_123_456_789L);
        assertEquals(123_456_789L, CardDemoCommarea.fromFixedWidth(c.toFixedWidth()).getCustId());
    }

    @Test
    void userType88LevelsMatchCopybookValues() {
        assertEquals('A', CardDemoCommarea.UserType.ADMIN.code());
        assertEquals('U', CardDemoCommarea.UserType.USER.code());
        assertEquals(0, CardDemoCommarea.PgmContext.ENTER.code());
        assertEquals(1, CardDemoCommarea.PgmContext.REENTER.code());
    }

    @Test
    void dateTimeFieldsFormatLikeCsdat01y() {
        DateTimeFields d = DateTimeFields.fromCurrentDate(LocalDateTime.of(2005, 3, 7, 4, 5, 6, 789_000_000));
        assertEquals("20050307", d.curdateN());
        assertEquals("04050678", d.curtimeN());
        assertEquals("03/07/05", d.curdateMmDdYy());
        assertEquals("04:05:06", d.curtimeHhMmSs());
        assertEquals("2005-03-07 04:05:06.780000", d.timestamp());
    }

    @Test
    void messageAndTitleConstantsHaveTheirPicLength() {
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
        assertEquals("Invalid key pressed. Please see below...", CommonMessages.CCDA_MSG_INVALID_KEY.strip());
        assertEquals(40, ScreenTitle.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitle.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitle.CCDA_THANK_YOU.length());
        assertEquals("AWS Mainframe Modernization", ScreenTitle.CCDA_TITLE01.strip());
    }

    @Test
    void cousr00MapHasTenRowsAndFieldLengthsFromBms() {
        Cousr00Map m = new Cousr00Map();
        assertEquals(10, Cousr00Map.ROWS);
        m.usrid[0].move("ABCDEFGHIJ");
        assertEquals("ABCDEFGH", m.usrid[0].data());
        m.fname[9].move("X");
        assertEquals(20, m.fname[9].data().length());
        m.lname[9].move("");
        assertEquals(20, m.lname[9].data().length());
        m.utype[0].move("AB");
        assertEquals("A", m.utype[0].data());
        m.sel[0].move("UX");
        assertEquals("U", m.sel[0].data());
        m.errmsg.move("E");
        assertEquals(78, m.errmsg.data().length());
        m.pagenum.move("1");
        assertEquals(8, m.pagenum.data().length());
        m.usridin.move("1");
        assertEquals(8, m.usridin.data().length());
        m.lowValues();
        assertEquals("\u0000".repeat(8), m.usrid[0].data());
    }

    @Test
    void bmsReceiveOfUntransmittedFieldIsLowValues() {
        BmsField f = new BmsField("USRIDIN", 8);
        f.move("ABC");
        f.receive(null);
        assertEquals("\u0000".repeat(8), f.data());
        f.receive("XY");
        assertEquals("XY      ", f.data());
    }

    @Test
    void dfhaidValuesAndShortReadKeys() {
        assertEquals('\'', Dfhaid.DFHENTER.value());
        assertEquals('3', Dfhaid.DFHPF3.value());
        assertEquals('7', Dfhaid.DFHPF7.value());
        assertEquals('8', Dfhaid.DFHPF8.value());
        assertTrue(Dfhaid.DFHCLEAR.isShortRead());
        assertTrue(Dfhaid.DFHPA1.isShortRead());
        assertFalse(Dfhaid.DFHENTER.isShortRead());
        assertFalse(Dfhaid.DFHPF8.isShortRead());
    }
}

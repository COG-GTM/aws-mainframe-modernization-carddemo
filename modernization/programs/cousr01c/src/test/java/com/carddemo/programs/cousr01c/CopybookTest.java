package com.carddemo.programs.cousr01c;

import com.carddemo.programs.cousr01c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr01c.copybook.CommonMessages;
import com.carddemo.programs.cousr01c.copybook.Cousr1aMap;
import com.carddemo.programs.cousr01c.copybook.DateTimeFields;
import com.carddemo.programs.cousr01c.copybook.Pic;
import com.carddemo.programs.cousr01c.copybook.ScreenTitle;
import com.carddemo.programs.cousr01c.copybook.SecUserData;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @Test
    void picXPadsAndTruncates() {
        assertEquals("AB   ", Pic.x("AB", 5));
        assertEquals("ABCDE", Pic.x("ABCDEFG", 5));
        assertEquals("     ", Pic.x(null, 5));
    }

    @Test
    void picNineDropsHighOrderDigitsAndSign() {
        assertEquals("007", Pic.nine(7, 3));
        assertEquals("234", Pic.nine(1234, 3));
        assertEquals("005", Pic.nine(-5, 3));
    }

    @Test
    void spacesOrLowValuesRequiresWholeFieldToMatchOne() {
        assertTrue(Pic.isSpacesOrLowValues("    "));
        assertTrue(Pic.isSpacesOrLowValues(Pic.lowValues(4)));
        assertFalse(Pic.isSpacesOrLowValues("  \u0000\u0000"), "a mix is neither SPACES nor LOW-VALUES");
        assertFalse(Pic.isSpacesOrLowValues(" A  "));
    }

    @Test
    void commareaIs160BytesAndRoundTrips() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid("CU01");
        c.fromProgram("COUSR01C");
        c.toProgram("COADM01C");
        c.userId("ADMIN001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        c.custId(123456789);
        c.custFname("MARGARET");
        c.acctId(12345678901L);
        c.acctStatus("Y");
        c.cardNum(4111111111111111L);
        c.lastMap("COUSR1A");
        c.lastMapset("COUSR01");
        String raw = c.toFixedWidth();
        assertEquals(CardDemoCommarea.LENGTH, raw.length());
        assertEquals(c, CardDemoCommarea.fromFixedWidth(raw));
        assertTrue(c.isUsrtypAdmin());
        assertFalse(c.isUsrtypUser());
        assertTrue(c.isPgmReenter());
        assertEquals("CU01COUSR01C    COADM01CADMIN001A1123456789", raw.substring(0, 43));
    }

    @Test
    void shortCommareaIsSpacePadded() {
        CardDemoCommarea c = CardDemoCommarea.fromFixedWidth("CA00COADM01C");
        assertEquals("CA00", c.fromTranid());
        assertEquals("COADM01C", c.fromProgram());
        assertEquals(Pic.spaces(8), c.toProgram());
        assertEquals(" ", c.pgmContext());
        assertFalse(c.isPgmEnter());
        assertFalse(c.isPgmReenter());
    }

    @Test
    void secUserDataLayout() {
        SecUserData u = new SecUserData("USER0001", "LAWRENCE", "THOMAS", "PASSWORD", "U", null);
        String raw = u.toFixedWidth();
        assertEquals(80, raw.length());
        assertEquals("USER0001", raw.substring(0, 8));
        assertEquals(Pic.x("LAWRENCE", 20), raw.substring(8, 28));
        assertEquals(Pic.x("THOMAS", 20), raw.substring(28, 48));
        assertEquals("PASSWORD", raw.substring(48, 56));
        assertEquals("U", raw.substring(56, 57));
        assertEquals(Pic.spaces(23), raw.substring(57));
        assertEquals(u, SecUserData.fromFixedWidth(raw));
        assertEquals(u, SecUserData.fromEbcdic(u.toEbcdic()));
        assertEquals((byte) 0xE4, u.toEbcdic()[0], "EBCDIC 'U'");
    }

    @Test
    void ebcdicEncodingRejectsUnmappableCharacters() {
        SecUserData u = new SecUserData("USER0009", "\u4E2D\u6587", "X", "P", "U", null);
        org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class, u::toEbcdic);
        SecUserData emoji = new SecUserData("USER0009", "A\uD83D\uDE00", "X", "P", "U", null);
        org.junit.jupiter.api.Assertions.assertThrows(IllegalArgumentException.class, emoji::toEbcdic);
    }

    @Test
    void ebcdicKeyOrderPutsLettersBeforeDigits() {
        assertTrue(SecUserData.compareEbcdicKeys("A0000001", "00000001") < 0);
        assertTrue(SecUserData.compareEbcdicKeys("ADMIN001", "USER0001") < 0);
        assertTrue(SecUserData.compareEbcdicKeys("a0000001", "A0000001") < 0);
    }

    @Test
    void dateTimeFormatting() {
        DateTimeFields d = new DateTimeFields();
        d.moveCurrentDate(LocalDateTime.of(1999, 12, 31, 23, 59, 58, 990_000_000));
        assertEquals("19991231", d.curdateN());
        assertEquals("23595899", d.curtimeN());
        assertEquals("1999-12-31 23:59:58.990000", d.timestamp());
        d.curdateMm(d.curdateMonth());
        d.curdateDd(d.curdateDay());
        d.curdateYy(Pic.nine(d.curdateYear(), 4).substring(2, 4));
        assertEquals("12/31/99", d.curdateMmDdYy());
        d.curtimeHh(d.curtimeHours());
        d.curtimeMm(d.curtimeMinute());
        d.curtimeSs(d.curtimeSecond());
        assertEquals("23:59:58", d.curtimeHhMmSs());
    }

    @Test
    void constantsKeepPicLengths() {
        assertEquals(40, ScreenTitle.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitle.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitle.CCDA_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
    }

    @Test
    void mapFieldLengthsMatchBms() {
        assertEquals(20, Cousr1aMap.Field.FNAME.length());
        assertEquals(20, Cousr1aMap.Field.LNAME.length());
        assertEquals(8, Cousr1aMap.Field.USERID.length());
        assertEquals(8, Cousr1aMap.Field.PASSWD.length());
        assertEquals(1, Cousr1aMap.Field.USRTYPE.length());
        assertEquals(78, Cousr1aMap.Field.ERRMSG.length());
        Cousr1aMap m = new Cousr1aMap();
        m.field(Cousr1aMap.Field.ERRMSG).data(Pic.x("X", 80));
        assertEquals(78, m.field(Cousr1aMap.Field.ERRMSG).data().length(), "WS-MESSAGE X(80) -> ERRMSGO X(78)");
    }
}

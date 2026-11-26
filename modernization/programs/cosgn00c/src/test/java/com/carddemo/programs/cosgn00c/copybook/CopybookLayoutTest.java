package com.carddemo.programs.cosgn00c.copybook;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;

class CopybookLayoutTest {

    @Test
    void secUserDataIs80BytesAndRoundTrips() {
        String rec = "ADMIN001MARGARET            GOLD                PASSWORDA";
        SecUserData u = SecUserData.fromRecord(rec);
        assertEquals("ADMIN001", u.usrId());
        assertEquals("MARGARET            ", u.fname());
        assertEquals("GOLD                ", u.lname());
        assertEquals("PASSWORD", u.pwd());
        assertEquals("A", u.type());
        assertEquals(23, u.filler().length());
        assertEquals(80, u.toRecord().length());
        assertEquals(CobolText.picX(rec, 80), u.toRecord());
    }

    @Test
    void secUserDataTruncatesOversizedFields() {
        SecUserData u = SecUserData.of("TOOLONGID", "F", "L", "PASSWORD9", "AB");
        assertEquals("TOOLONGI", u.usrId());
        assertEquals("PASSWORD", u.pwd());
        assertEquals("A", u.type());
    }

    @Test
    void commareaIs160BytesAndRoundTrips() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid("CC00");
        c.fromProgram("COSGN00C");
        c.userId("ADMIN001");
        c.userType("A");
        c.pgmContext(1);
        c.custId(123456789L);
        c.acctId(12345678901L);
        c.cardNum(4111111111111111L);
        c.lastMap("COSGN0A");
        c.lastMapset("COSGN00");
        String rec = c.toRecord();
        assertEquals(CardDemoCommarea.LENGTH, rec.length());
        assertEquals(rec, CardDemoCommarea.fromRecord(rec).toRecord());
        assertTrue(c.isUserTypeAdmin());
        assertFalse(c.isUserTypeUser());
        assertTrue(c.isPgmReenter());
        assertFalse(c.isPgmEnter());
    }

    @Test
    void commareaNumericMovesKeepLowOrderDigits() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.custId(1_234_567_890L);
        c.pgmContext(12);
        assertEquals(234_567_890L, c.custId());
        assertEquals(2, c.pgmContext());
    }

    @Test
    void commareaDefaultsAreSpacesAndZeros() {
        String rec = new CardDemoCommarea().toRecord();
        assertEquals(" ".repeat(33) + "0" + "0".repeat(9), rec.substring(0, 43));
    }

    @Test
    void dateTimeWorkAreaFormats() {
        DateTimeWorkArea d = new DateTimeWorkArea();
        d.moveCurrentDate(LocalDateTime.of(2024, 3, 9, 7, 8, 9, 120_000_000));
        d.buildMmDdYy();
        d.buildHhMmSs();
        d.buildTimestamp();
        assertEquals("03/09/24", d.curdateMmDdYy());
        assertEquals("07:08:09", d.curtimeHhMmSs());
        assertEquals("20240309", d.curdateN());
        assertEquals("07080912", d.curtimeN());
        assertEquals("2024-03-09 07:08:09.120000", d.timestamp());
    }

    @Test
    void upperCaseFoldsOnlyAsciiLetters() {
        assertEquals("ABC1-\u00e9\u0000Z", CobolText.upperCase("abc1-\u00e9\u0000z"));
    }

    @Test
    void spacesOrLowValuesPredicates() {
        assertTrue(CobolText.isSpacesOrLowValues("    "));
        assertTrue(CobolText.isSpacesOrLowValues(CobolText.lowValues(4)));
        assertFalse(CobolText.isSpacesOrLowValues("  \u0000\u0000"));
        assertFalse(CobolText.isSpacesOrLowValues(" a  "));
    }

    @Test
    void messageAndTitleConstantsKeepPicWidths() {
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(40, ScreenTitles.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitles.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitles.CCDA_THANK_YOU.length());
    }

    @Test
    void bmsInputPadsTransmittedAndLowValuesUntransmittedFields() {
        Cosgn0aInput in = Cosgn0aInput.of("ab", null);
        assertEquals("ab      ", in.useridI());
        assertEquals(CobolText.lowValues(8), in.passwdI());
        assertEquals(78, in.errmsgI().length());
    }

    @Test
    void dfhaidCodes() {
        assertEquals('\'', CicsAid.DFHENTER.code());
        assertEquals('3', CicsAid.DFHPF3.code());
        assertEquals('_', CicsAid.DFHCLEAR.code());
    }
}

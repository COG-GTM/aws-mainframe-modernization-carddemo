package com.carddemo.programs.comen01c.copybook;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @Test
    void commareaIs160BytesAndRoundTrips() {
        CardDemoCommarea c = CardDemoCommarea.fromBytes(CobolText.spaces(CardDemoCommarea.LENGTH));
        c.fromTranid("CM00");
        c.fromProgram("COMEN01C");
        c.toTranid("CC00");
        c.toProgram("COSGN00C");
        c.userId("USER0001");
        c.setUserType(CardDemoCommarea.UserType.USER);
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        c.custId(123456789L);
        c.custFname("ANA");
        c.acctId(1L);
        c.acctStatus("Y");
        c.cardNum(4111111111111111L);
        c.lastMap("COMEN1A");
        c.lastMapset("COMEN01");
        String bytes = c.toBytes();
        assertEquals(160, bytes.length());
        assertEquals("CM00COMEN01CCC00COSGN00CUSER0001U1123456789", bytes.substring(0, 43));
        assertEquals("00000000001Y4111111111111111COMEN1ACOMEN01", bytes.substring(118));
        CardDemoCommarea back = CardDemoCommarea.fromBytes(bytes);
        assertEquals(bytes, back.toBytes());
        assertTrue(back.isUsrtypUser());
        assertFalse(back.isUsrtypAdmin());
        assertTrue(back.isPgmReenter());
        assertEquals(123456789L, back.custId());
        assertEquals(4111111111111111L, back.cardNum());
    }

    @Test
    void commareaMoveTruncatesLongInput() {
        CardDemoCommarea c = CardDemoCommarea.fromBytes("X".repeat(200));
        assertEquals(160, c.toBytes().length());
        c.fromProgram("TOOLONGNAME");
        assertEquals("TOOLONGN", c.fromProgram());
        c.custId(1234567890L);
        assertEquals("234567890", c.custIdDisplay(), "PIC 9(09) high-order truncation");
    }

    @Test
    void uninitialisedCommareaIsLowValues() {
        CardDemoCommarea c = new CardDemoCommarea();
        assertEquals(CobolText.lowValues(160), c.toBytes());
        assertFalse(c.isPgmEnter());
        assertFalse(c.isPgmReenter());
    }

    @Test
    void mainMenuOptionsMatchComen02y() {
        MainMenuOptions m = MainMenuOptions.standard();
        assertEquals(11, m.count());
        assertEquals("Account View                       ", m.option(1).name());
        assertEquals("COACTVWC", m.option(1).pgmName());
        assertEquals("COPAUS0C", m.option(11).pgmName());
        assertEquals("Pending Authorization View         ", m.option(11).name());
        for (int i = 1; i <= 11; i++) {
            assertEquals(i, m.option(i).num());
            assertEquals("U", m.option(i).usrType());
            assertEquals(35, m.option(i).name().length());
        }
        assertEquals(MainMenuOptions.MenuOption.BLANK, m.option(12));
        assertEquals(MainMenuOptions.MenuOption.BLANK, m.option(0));
        assertEquals(MainMenuOptions.MenuOption.BLANK, m.option(99));
        assertThrows(IllegalArgumentException.class,
                () -> new MainMenuOptions(13, java.util.Collections.nCopies(13, MainMenuOptions.MenuOption.BLANK)));
        assertThrows(IllegalArgumentException.class,
                () -> new MainMenuOptions(12, java.util.List.of(MainMenuOptions.MenuOption.BLANK)));
    }

    @Test
    void symbolicMapFieldNamesIgnoreDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault();
        try {
            java.util.Locale.setDefault(java.util.Locale.forLanguageTag("ar-EG"));
            assertEquals("OPTN012", new Comen01Map().optn(12).name());
        } finally {
            java.util.Locale.setDefault(saved);
        }
    }

    @Test
    void titlesAndMessagesKeepPicWidths() {
        assertEquals(40, ScreenTitle.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitle.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitle.CCDA_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
        assertEquals("Invalid key pressed. Please see below...", CommonMessages.CCDA_MSG_INVALID_KEY.stripTrailing());
    }

    @Test
    void wsDateTimeFormatsLikeCsdat01y() {
        WsDateTime d = new WsDateTime();
        d.moveCurrentDate(LocalDateTime.of(2005, 3, 4, 23, 59, 1, 990_000_000));
        assertEquals("20050304", d.curdateN());
        assertEquals("23590199", d.curtimeN());
        d.curdateMm(d.curdateMonth());
        d.curdateDd(d.curdateDay());
        d.curdateYy(d.curdateYear() % 100);
        assertEquals("03/04/05", d.curdateMmDdYy());
        d.curtimeHh(d.curtimeHours());
        d.curtimeMm(d.curtimeMinute());
        d.curtimeSs(d.curtimeSecond());
        assertEquals("23:59:01", d.curtimeHhMmSs());
        d.timestamp(2005, 3, 4, 23, 59, 1, 123456);
        assertEquals("2005-03-04 23:59:01.123456", d.timestamp());
    }

    @Test
    void secUserDataParsesFixedWidthRecord() {
        String record = CobolText.move("ADMIN001", 8) + CobolText.move("FIRST", 20) + CobolText.move("LAST", 20)
                + CobolText.move("PASSWORD", 8) + "A" + CobolText.spaces(23);
        SecUserData u = SecUserData.parse(record);
        assertEquals(80, u.toRecord().length());
        assertEquals("ADMIN001", u.usrId());
        assertEquals("A", u.usrType());
        assertEquals(record, u.toRecord());
    }

    @Test
    void comen01MapFieldSizesMatchSymbolicMap() {
        Comen01Map map = new Comen01Map();
        List<Integer> sizes = map.all().stream().map(Comen01Map.MapField::size).toList();
        assertEquals(List.of(4, 40, 8, 8, 40, 8, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 2, 78), sizes);
        map.errmsg().move("hello");
        map.errmsg().color(Dfhbmsca.DFHRED);
        map.lowValues();
        assertEquals(CobolText.lowValues(78), map.errmsg().data());
        assertEquals(CobolText.LOW_VALUE, map.errmsg().color());
        assertEquals("OPTN012", map.optn(12).name());
    }

    @Test
    void stringIntoStopsOnOverflowAndKeepsTail() {
        assertEquals("abcde", CobolText.stringInto("     ", "abc", "defgh"));
        assertEquals("ab   ", CobolText.stringInto("     ", "ab"));
        assertEquals("Pending", CobolText.delimitedBy("Pending Authorization", " "));
        assertEquals("Pending Authorization", CobolText.delimitedBy("Pending Authorization  View", "  "));
        assertEquals(" 1", CobolText.moveJustRight("1", 2));
        assertEquals("23", CobolText.moveJustRight("123", 2));
    }

    @Test
    void dfhaidCodesMatchCics() {
        assertEquals('\'', Dfhaid.DFHENTER.code());
        assertEquals('3', Dfhaid.DFHPF3.code());
        assertEquals('2', Dfhbmsca.DFHRED);
        assertEquals('4', Dfhbmsca.DFHGREEN);
    }
}

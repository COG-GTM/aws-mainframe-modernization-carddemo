package com.carddemo.programs.coadm01c.copybook;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @Test
    void commareaLayoutCoversExactly160Bytes() {
        int next = 0;
        for (CardDemoCommarea.Field f : CardDemoCommarea.Field.values()) {
            assertEquals(next, f.offset, f.name());
            next += f.length;
        }
        assertEquals(CardDemoCommarea.LENGTH, next);
        assertEquals(160, new CardDemoCommarea().toRecord().length());
    }

    @Test
    void commareaMovesPadTruncateAndZeroFill() {
        CardDemoCommarea c = CardDemoCommarea.fromRecord("");
        c.setFromProgram("COADM01CXYZ");
        c.setCustId(1234567890L);
        c.setAcctId(42);
        assertEquals("COADM01C", c.fromProgram());
        assertEquals("234567890", c.get(CardDemoCommarea.Field.CUST_ID), "PIC 9(09) high-order truncation");
        assertEquals("00000000042", c.get(CardDemoCommarea.Field.ACCT_ID));
        assertEquals(c, CardDemoCommarea.fromRecord(c.toRecord()));
        assertEquals(c.toRecord().substring(0, 160), CardDemoCommarea.fromRecord(c.toRecord() + "EXTRA").toRecord());
        assertThrows(NumberFormatException.class, () -> CardDemoCommarea.fromRecord("").custId());
    }

    @Test
    void commarea88Levels() {
        CardDemoCommarea c = CardDemoCommarea.fromRecord("");
        assertFalse(c.isPgmEnter());
        assertFalse(c.isPgmReenter());
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        assertTrue(c.isPgmReenter());
        c.setPgmContext(CardDemoCommarea.PgmContext.ENTER);
        assertTrue(c.isPgmEnter());
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        assertTrue(c.isUsrTypAdmin());
        c.setUserType(CardDemoCommarea.UserType.USER);
        assertTrue(c.isUsrTypUser());
    }

    @Test
    void adminMenuOptionsMatchCopybookValues() {
        AdminMenuOptions o = AdminMenuOptions.standard();
        assertEquals(6, o.count());
        assertEquals("Transaction Type List/Update (Db2) ", o.option(5).name());
        assertEquals("COTRTLIC", o.option(5).pgmName());
        assertEquals(List.of("COUSR00C", "COUSR01C", "COUSR02C", "COUSR03C", "COTRTLIC", "COTRTUPC"),
                o.options().stream().map(AdminMenuOptions.AdminMenuOption::pgmName).toList());
        o.options().forEach(opt -> assertEquals(35, opt.name().length()));
        var ten = Arrays.asList(new AdminMenuOptions.AdminMenuOption[10]);
        assertThrows(IllegalArgumentException.class, () -> new AdminMenuOptions(1, ten));
        assertThrows(IllegalArgumentException.class, () -> new AdminMenuOptions(7, o.options()));
    }

    @Test
    void titlesAndMessagesKeepCopybookWidths() {
        assertEquals(40, ScreenTitle.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitle.CCDA_TITLE02.length());
        assertEquals("Thank you for using CCDA application... ", ScreenTitle.CCDA_THANK_YOU);
        assertEquals(CobolFields.alnum("Invalid key pressed. Please see below...", 50), CommonMessages.CCDA_MSG_INVALID_KEY);
        assertEquals(CobolFields.alnum("Thank you for using CardDemo application...", 50), CommonMessages.CCDA_MSG_THANK_YOU);
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
    }

    @Test
    void dateTimeFields() {
        DateTimeFields d = DateTimeFields.currentDate(LocalDateTime.of(2024, 1, 2, 3, 4, 5, 670_000_000));
        assertEquals("20240102", d.curdateN());
        assertEquals("03040567", d.curtimeN());
        assertEquals("01/02/24", d.curdateMmDdYy());
        assertEquals("03:04:05", d.curtimeHhMmSs());
        assertEquals("2024-01-02 03:04:05.670000", d.timestamp());
    }

    @Test
    void secUserDataRoundTrips() {
        SecUserData u = new SecUserData("ADMIN001", "First", "Last", "PASSWORD", "A", null);
        assertEquals(80, u.toRecord().length());
        assertEquals(u, SecUserData.fromRecord(u.toRecord()));
    }

    @Test
    void bmsAndAidConstants() {
        assertEquals('4', Dfhbmsca.DFHGREEN.code);
        assertEquals('\'', Dfhaid.DFHENTER.code);
        assertEquals('3', Dfhaid.DFHPF3.code);
    }

    @Test
    void cobolFieldHelpers() {
        assertEquals("ab  ", CobolFields.alnum("ab", 4));
        assertEquals("ab", CobolFields.alnum("abcd", 2));
        assertEquals("  ab", CobolFields.justRight("ab", 4));
        assertEquals("cd", CobolFields.justRight("abcd", 2));
        assertEquals("07", CobolFields.unsigned(107, 2));
        assertTrue(CobolFields.isLowValuesOrSpaces("    "));
        assertTrue(CobolFields.isLowValuesOrSpaces(CobolFields.lowValues(3)));
        assertFalse(CobolFields.isLowValuesOrSpaces(" \u0000"));
        assertFalse(CobolFields.isNumeric("1 "));
        assertTrue(CobolFields.isNumeric("09"));
    }

    @Test
    void outputSettersAreCobolMoves() {
        Coadm1aOutput o = Coadm1aOutput.lowValues();
        o.setErrmsg("x".repeat(80));
        assertEquals(78, o.errmsg().length());
        o.setOption("123");
        assertEquals("12", o.option());
        assertThrows(IllegalArgumentException.class, () -> Coadm1aInput.keyed("123"));
    }
}

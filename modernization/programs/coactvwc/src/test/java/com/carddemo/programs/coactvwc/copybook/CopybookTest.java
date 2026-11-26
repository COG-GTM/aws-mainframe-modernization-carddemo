package com.carddemo.programs.coactvwc.copybook;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CopybookTest {

    @Test
    void accountRecordRoundTripsAt300Bytes() {
        AccountRecord a = new AccountRecord(12345678901L, "Y", new BigDecimal("-10.25"), new BigDecimal("5000.00"),
                new BigDecimal("1000.00"), "2020-01-01", "2025-01-01", "2023-01-01", new BigDecimal("1.00"),
                new BigDecimal("2.00"), "12345", "GRP1");
        String raw = a.toFixedWidth();
        assertEquals(AccountRecord.LENGTH, raw.length());
        assertEquals(a.acctCurrBal(), AccountRecord.parse(raw).acctCurrBal());
        assertEquals(raw, AccountRecord.parse(raw).toFixedWidth());
    }

    @Test
    void recordLengthsMatchCopybooks() {
        assertEquals(50, CardXrefRecord.parse("x").toFixedWidth().length());
        assertEquals(150, CardRecord.parse("x").toFixedWidth().length());
        assertEquals(500, CustomerRecord.parse("x").toFixedWidth().length());
        assertEquals(80, SecUserData.parse("x").toFixedWidth().length());
        assertEquals(134, new AbendData().toString().length());
    }

    @Test
    void cardRecordParsesFields() {
        CardRecord c = CardRecord.parse("0500024453765740000000000507471234");
        assertEquals("0500024453765740", c.cardNum());
        assertEquals(50L, c.cardAcctId());
        assertEquals(747, c.cardCvvCd());
    }

    @Test
    void commareaIs160BytesAndRoundTrips() {
        CardDemoCommarea c = CardDemoCommarea.initialized();
        assertEquals(CardDemoCommarea.LENGTH, c.toString().length());
        assertTrue(c.isPgmEnter());
        c.setUserId("USER0001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        c.setAcctId(42);
        c.setCardNum(4111111111111111L);
        CardDemoCommarea back = CardDemoCommarea.fromString(c.toString());
        assertTrue(back.isUsrtypAdmin());
        assertTrue(back.isPgmReenter());
        assertEquals("00000000042", back.acctIdText());
        assertEquals(4111111111111111L, back.cardNum());
        assertEquals("USER0001", back.userId());
    }

    @Test
    void lowValueCommareaMatchesNo88() {
        CardDemoCommarea c = CardDemoCommarea.lowValues();
        assertFalse(c.isPgmEnter());
        assertFalse(c.isPgmReenter());
    }

    @Test
    void currentDateFormats() {
        CurrentDateTime d = CurrentDateTime.of(LocalDateTime.of(2026, 11, 26, 7, 5, 9, 120_000_000));
        assertEquals("11/26/26", d.curdateMmDdYy());
        assertEquals("07:05:09", d.curtimeHhMmSs());
        assertEquals("20261126", d.curdateN());
        assertEquals("07050912", d.curtimeN());
        assertEquals("2026-11-26 07:05:09.120000", d.timestamp());
    }

    @ParameterizedTest
    @CsvSource({"DFHENTER,ENTER", "DFHCLEAR,CLEAR", "DFHPA1,PA1", "DFHPA2,PA2", "DFHPF3,PFK03", "DFHPF15,PFK03",
            "DFHPF1,PFK01", "DFHPF13,PFK01", "DFHPF12,PFK12", "DFHPF24,PFK12", "DFHPF22,PFK10"})
    void storePfKeyMapsAid(DfhAid aid, CcardAid expected) {
        CcWorkArea wa = new CcWorkArea();
        StorePfKey.yyyyStorePfkey(aid, wa);
        assertEquals(expected, wa.ccardAid());
    }

    @Test
    void storePfKeyLeavesUnmappedAidUnchanged() {
        CcWorkArea wa = new CcWorkArea();
        StorePfKey.yyyyStorePfkey(DfhAid.DFHPA3, wa);
        assertNull(wa.ccardAid());
    }

    @Test
    void workAreaRedefines() {
        CcWorkArea wa = new CcWorkArea();
        wa.setCcAcctId("00000000123");
        assertEquals(123L, wa.ccAcctIdN());
        assertFalse(wa.isCcardReturnMsgOff());
        assertEquals("COACTVW", fit7("COACTVW "));
    }

    private static String fit7(String s) {
        CcWorkArea wa = new CcWorkArea();
        wa.setCcardNextMapset(s);
        return wa.ccardNextMapset();
    }
}

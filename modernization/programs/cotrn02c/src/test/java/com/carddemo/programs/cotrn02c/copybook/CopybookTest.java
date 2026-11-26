package com.carddemo.programs.cotrn02c.copybook;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @ParameterizedTest
    @CsvSource({
        "12.5, +00000012.50", "-0.01, -00000000.01", "123456789.99, +23456789.99", "0, +00000000.00"
    })
    void signedEditedPicture(String in, String out) {
        assertEquals(out, Pic.signedEdited(new BigDecimal(in), 8, 2));
    }

    @ParameterizedTest
    @CsvSource({"0000006032B, 603.22", "0000009190}, -919.00", "0000001234J, -123.41", "00000001000, 10.00"})
    void zonedOverpunch(String image, String value) {
        assertEquals(new BigDecimal(value), Pic.parseZonedSigned(image, 2));
        assertEquals(image.replaceAll("0$", "{"), Pic.zonedSigned(new BigDecimal(value), 9, 2));
    }

    @Test
    void numvalAndNumvalC() {
        assertEquals(new BigDecimal("-12.50"), Pic.numvalC("-00000012.50"));
        assertEquals(new BigDecimal("1234.50"), Pic.numvalC(" $1,234.50 "));
        assertEquals(new BigDecimal("-5"), Pic.numval("5 CR"));
        assertEquals(new BigDecimal("42"), Pic.numval("00000000042"));
    }

    @Test
    void classAndFigurativeTests() {
        assertTrue(Pic.isNumeric("0123"));
        assertFalse(Pic.isNumeric("12 "));
        assertTrue(Pic.isSpacesOrLowValues("   "));
        assertTrue(Pic.isSpacesOrLowValues(Pic.lowValues(3)));
        assertFalse(Pic.isSpacesOrLowValues(" \u0000"));
        assertEquals(new BigDecimal("999999999.99"), Pic.fit(new BigDecimal("1999999999.999"), 9, 2));
    }

    @Test
    void commareaLayoutIs218BytesAndRoundTrips() {
        CardDemoCommarea c = CardDemoCommarea.fromRecord("");
        c.setFromTranId("CT00");
        c.setUserType('A');
        c.setPgmReenter();
        c.setCt02TrnSelected("0000000000000042");
        String raw = c.toRecord();
        assertEquals(218, raw.length());
        assertEquals("CT00", raw.substring(0, 4));
        assertEquals('1', raw.charAt(33));
        assertEquals("0000000000000042", raw.substring(202, 218));
        CardDemoCommarea back = CardDemoCommarea.fromRecord(raw);
        assertTrue(back.isUserTypeAdmin());
        assertTrue(back.isPgmReenter());
        assertTrue(CardDemoCommarea.initial().isNextPageNo());
    }

    @Test
    void tranRecordInitializeKeepsFiller() {
        TranRecord t = new TranRecord();
        t.setTranId("1");
        t.setAmt(new BigDecimal("5"));
        t.setFiller("KEEP");
        t.initialize();
        assertEquals(Pic.spaces(16), t.getTranId());
        assertEquals(new BigDecimal("0.00"), t.getAmt());
        assertEquals(Pic.x("KEEP", 20), t.getFiller());
        assertEquals(350, t.toRecord().length());
    }

    @Test
    void accountRecordRoundTrips() {
        AccountRecord a = new AccountRecord(50, "Y", new BigDecimal("-12.34"), new BigDecimal("1000"),
                new BigDecimal("200"), "2020-01-01", "2030-01-01", "2025-01-01", BigDecimal.ZERO, BigDecimal.ONE,
                "2000", "GRP", "");
        String raw = a.toRecord();
        assertEquals(300, raw.length());
        assertEquals(a, AccountRecord.parse(raw));
    }

    @Test
    void dateTimeFieldsFormatting() {
        DateTimeFields d = DateTimeFields.currentDate(java.time.LocalDateTime.of(2026, 1, 2, 3, 4, 5, 670_000_000));
        assertEquals("01/02/26", d.curdateMmDdYy());
        assertEquals("03:04:05", d.curtimeHhMmSs());
        assertEquals("20260102", d.curdateN());
        assertEquals("03040567", d.curtimeN());
        assertEquals("2026-01-02 03:04:05.670000", d.timestamp());
    }

    @Test
    void mapReceiveSetsLengthAndPadsWithSpaces() {
        Cotrn2aMap m = new Cotrn2aMap();
        m.receive(Cotrn2aMap.Field.TTYPCD, "1");
        assertEquals("1 ", m.get(Cotrn2aMap.Field.TTYPCD));
        assertEquals(1, m.f(Cotrn2aMap.Field.TTYPCD).getLength());
        m.receive(Cotrn2aMap.Field.TCATCD, null);
        assertEquals(Pic.lowValues(4), m.get(Cotrn2aMap.Field.TCATCD));
        m.cursor(Cotrn2aMap.Field.MID);
        m.cursor(Cotrn2aMap.Field.TTYPCD);
        assertEquals(Cotrn2aMap.Field.TTYPCD, m.cursorField());
    }
}

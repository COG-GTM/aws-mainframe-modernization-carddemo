package com.carddemo.programs.cotrn00c.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    private static final String LINE1 = "0000000000683580010001POS TERM  Purchase at Abshire-Lowe"
            + " ".repeat(76) + "0000005047G800000000Abshire-Lowe" + " ".repeat(38) + "North Enoshaven"
            + " ".repeat(35) + "72112     48594526128770652022-06-10 19:27:53.000000" + " ".repeat(46);

    @Test
    void tranRecordParsesEveryFieldOfCvtra05y() {
        TranRecord r = TranRecord.parse(LINE1);
        assertEquals("0000000000683580", r.tranId());
        assertEquals("01", r.tranTypeCd());
        assertEquals(1, r.tranCatCd());
        assertEquals("POS TERM  ", r.tranSource());
        assertEquals("Purchase at Abshire-Lowe", r.tranDesc().stripTrailing());
        assertEquals(100, r.tranDesc().length());
        assertEquals(new BigDecimal("504.77"), r.tranAmt());
        assertEquals(800000000L, r.tranMerchantId());
        assertEquals("Abshire-Lowe", r.tranMerchantName().stripTrailing());
        assertEquals("North Enoshaven", r.tranMerchantCity().stripTrailing());
        assertEquals("72112     ", r.tranMerchantZip());
        assertEquals("4859452612877065", r.tranCardNum());
        assertEquals("2022-06-10 19:27:53.000000", r.tranOrigTs());
        assertEquals(" ".repeat(26), r.tranProcTs());
    }

    @Test
    void tranRecordRoundTripsTo350Bytes() {
        assertEquals(350, LINE1.length());
        assertEquals(LINE1, TranRecord.parse(LINE1).toFixedWidth());
    }

    @Test
    void zonedDecimalOverpunchSigns() {
        assertEquals(new BigDecimal("-919.00"), ZonedDecimal.parse("0000009190}", 2));
        assertEquals(new BigDecimal("-12.34"), ZonedDecimal.parse("0000000123M", 2));
        assertEquals(new BigDecimal("12.30"), ZonedDecimal.parse("0000000123{", 2));
        assertEquals(new BigDecimal("12.34"), ZonedDecimal.parse("00000001234", 2));
        assertEquals(new BigDecimal("-0.01"), ZonedDecimal.parse("0000000000q", 2));
        assertEquals("0000009190}", ZonedDecimal.format(new BigDecimal("-919.00"), 11, 2));
        assertEquals("0000005047G", ZonedDecimal.format(new BigDecimal("504.77"), 11, 2));
        assertThrows(IllegalArgumentException.class, () -> ZonedDecimal.parse("00000000 1A", 2));
    }

    @Test
    void cobolTextFigurativeConstantsAndNumericTest() {
        assertTrue(CobolText.isSpacesOrLowValues("    "));
        assertTrue(CobolText.isSpacesOrLowValues(CobolText.lowValues(4)));
        assertFalse(CobolText.isSpacesOrLowValues(" \u0000"));
        assertTrue(CobolText.isNumeric("0000000000000123"));
        assertFalse(CobolText.isNumeric("123             "));
        assertEquals("AB", CobolText.fit("ABC", 2));
        assertEquals("A  ", CobolText.fit("A", 3));
        assertEquals("00000001", CobolText.unsignedDigits(100000001L, 8));
        assertEquals(0L, CobolText.parseUnsigned("        "));
    }

    @Test
    void carddemoCommareaIs160BytesWith88Levels() {
        CarddemoCommarea c = CarddemoCommarea.blank();
        c.setUserType("A");
        c.setPgmContext(CarddemoCommarea.PgmContext.REENTER);
        c.setCustId(123);
        c.setCardNum(4859452612877065L);
        String bytes = c.encode();
        assertEquals(CarddemoCommarea.LENGTH, bytes.length());
        CarddemoCommarea back = CarddemoCommarea.decode(bytes, 0);
        assertTrue(back.isUsrtypAdmin());
        assertFalse(back.isUsrtypUser());
        assertTrue(back.isPgmReenter());
        assertFalse(back.isPgmEnter());
        assertEquals(123, back.custId());
        assertEquals(4859452612877065L, back.cardNum());
    }

    @Test
    void dateTimeFieldsFormatHeaderAndTimestamp() {
        DateTimeFields d = new DateTimeFields();
        d.setCurrentDate(LocalDateTime.of(2026, 3, 7, 9, 5, 4, 120_000_000));
        assertEquals("20260307", d.curdateN());
        assertEquals("09050412", d.curtimeN());
        d.setTimestamp("2022-06-10 19:27:53.000000");
        assertEquals("2022", d.timestampDtYyyy());
        assertEquals("06", d.timestampDtMm());
        assertEquals("10", d.timestampDtDd());
        assertEquals("19", d.timestampTmHh());
        assertEquals("000000", d.timestampTmMs6());
    }

    @Test
    void mapCursorIsFirstFieldWithLengthMinusOne() {
        Cotrn00Map m = new Cotrn00Map();
        assertNull(m.cursorField());
        m.row(3).sel.setLength(-1);
        m.trnidin.setLength(-1);
        assertEquals("TRNIDIN", m.cursorField());
        assertEquals(8 + 50 + 1, m.fields().size());
        assertEquals("TAMT010", m.row(10).tamt.name());
        assertEquals('\'', DfhAid.DFHENTER.code());
        assertEquals("Invalid key pressed. Please see below...         ", CommonMessages.CCDA_MSG_INVALID_KEY);
        assertEquals(40, ScreenTitles.CCDA_TITLE01.length());
    }
}

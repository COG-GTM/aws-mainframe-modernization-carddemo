package com.carddemo.programs.cobil00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cobil00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cobil00c.copybook.Cobil00Map;
import com.carddemo.programs.cobil00c.copybook.Cobol;
import com.carddemo.programs.cobil00c.copybook.CommonMessages;
import com.carddemo.programs.cobil00c.copybook.DateTimeFields;
import com.carddemo.programs.cobil00c.copybook.TranRecord;
import com.carddemo.programs.cobil00c.copybook.ZonedDecimal;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

class CopybookTest {

    @Test
    void commareaLayoutIs218BytesAndRoundTrips() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranId("CM00");
        c.fromProgram("COMEN01C");
        c.userType("A");
        c.pgmContext("1");
        c.acctId("00000000001");
        c.cb00TrnSelected("0000000000000123");
        String bytes = c.toBytes();
        assertEquals(CardDemoCommarea.LENGTH, bytes.length());
        assertEquals(218, bytes.length());
        CardDemoCommarea back = CardDemoCommarea.fromBytes(bytes, bytes.length());
        assertEquals(bytes, back.toBytes());
        assertTrue(back.isUserTypeAdmin());
        assertFalse(back.isUserTypeUser());
        assertTrue(back.isPgmReenter());
        assertEquals("0000000000000123", bytes.substring(202, 218));
    }

    @Test
    void commareaNextPageFlagDefaultsToN() {
        CardDemoCommarea c = new CardDemoCommarea();
        assertTrue(c.isNextPageNo());
        assertFalse(c.isNextPageYes());
    }

    @Test
    void commonMessagesArePaddedToFifty() {
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
    }

    @Test
    void zonedDecimalOverpunch() {
        assertEquals(new BigDecimal("194.00"), ZonedDecimal.decode("00000001940{", 2));
        assertEquals(new BigDecimal("-12.34"), ZonedDecimal.decode("00000000123M", 2));
        assertEquals("00000000123M", ZonedDecimal.encode(new BigDecimal("-12.34"), 12, 2));
        assertEquals("00000001940{", ZonedDecimal.encode(new BigDecimal("194.00"), 12, 2));
    }

    @Test
    void editedBalancePicture() {
        assertEquals("+0000000194.00", Cobol.editSignedFixed(new BigDecimal("194"), 10));
        assertEquals("-0000000025.50", Cobol.editSignedFixed(new BigDecimal("-25.5"), 10));
        assertEquals("+9999999999.99", Cobol.editSignedFixed(new BigDecimal("9999999999.99"), 10));
    }

    @Test
    void tranAmtPictureTruncatesHighOrderAndExtraDecimals() {
        assertEquals(new BigDecimal("234567890.12"), Cobol.toPic(new BigDecimal("1234567890.129"), 9, 2));
        TranRecord t = TranRecord.initialized();
        assertEquals(Cobol.spaces(16), t.tranId());
        assertEquals(0, t.tranCatCd());
        assertEquals(new BigDecimal("0.00"), t.tranAmt());
    }

    @Test
    void zonedValueRejectsHighValues() {
        assertEquals(42, Cobol.zonedValue("0000000000000042"));
        assertThrows(NumberFormatException.class, () -> Cobol.zonedValue(Cobol.highValues(16)));
    }

    @Test
    void displaySignedUsesTrailingOverpunch() {
        assertEquals("00000001C", Cobol.displaySigned(13, 9));
        assertEquals("00000000{", Cobol.displaySigned(0, 9));
    }

    @Test
    void dateTimeFieldsFormatHeaderAndTimestamp() {
        DateTimeFields d = new DateTimeFields();
        d.moveCurrentDate(LocalDateTime.of(2022, 7, 9, 3, 4, 5, 670_000_000));
        assertEquals("07/09/22", d.curdateMmDdYy());
        assertEquals("03:04:05", d.curtimeHhMmSs());
        assertEquals(20220709, d.curdateN());
        assertEquals(3040567, d.curtimeN());
        d.initializeTimestamp();
        d.moveToTimestamp(1, 10, "2022-07-09");
        d.moveToTimestamp(12, 8, "03:04:05");
        d.zeroTimestampMicros();
        assertEquals("2022-07-09 03:04:05.000000", d.timestamp());
    }

    @Test
    void mapCursorDefaultsToInitialCursorField() {
        Cobil00Map m = new Cobil00Map();
        assertEquals("ACTIDIN", m.cursorField());
        m.confirm.length(-1);
        assertEquals("CONFIRM", m.cursorField());
        m.actidin.length(-1);
        assertEquals("ACTIDIN", m.cursorField());
        m.lowValues();
        assertEquals(Cobol.lowValues(78), m.errmsg.data());
    }
}

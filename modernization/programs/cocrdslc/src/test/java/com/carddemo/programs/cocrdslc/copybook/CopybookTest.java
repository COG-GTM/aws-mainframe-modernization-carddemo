package com.carddemo.programs.cocrdslc.copybook;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cocrdslc.CobolText;
import com.carddemo.programs.cocrdslc.copybook.CcWorkArea.CcardAid;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CopybookTest {

    @Test
    void commareaIs160BytesAndRoundTrips() {
        CarddemoCommarea c = CarddemoCommarea.initialized();
        c.cdemoFromTranid = "CCLI";
        c.cdemoFromProgram = "COCRDLIC";
        c.cdemoUserId = "ADMIN001";
        c.setUsrtypAdmin();
        c.setPgmReenter();
        c.custId(123456789);
        c.acctId(50);
        c.cardNum(500024453765740L);
        c.cdemoLastMap = "CCRDLIA";
        c.cdemoLastMapset = "COCRDLI";
        String raw = c.toFixedWidth();
        assertEquals(CarddemoCommarea.LENGTH, raw.length());
        CarddemoCommarea back = CarddemoCommarea.parse(raw);
        assertEquals(raw, back.toFixedWidth());
        assertEquals("00000000050", back.cdemoAcctId);
        assertEquals("0500024453765740", back.cdemoCardNum);
        assertEquals(123456789L, back.custId());
        assertTrue(back.isUsrtypAdmin());
        assertTrue(back.isPgmReenter());
        assertFalse(back.isPgmEnter());
    }

    @Test
    void initializeSetsSpacesAndZeros() {
        CarddemoCommarea c = CarddemoCommarea.initialized();
        assertEquals(" ".repeat(8), c.cdemoFromProgram);
        assertEquals("00000000000", c.cdemoAcctId);
        assertTrue(c.isPgmEnter());
        assertFalse(c.isUsrtypUser());
    }

    @Test
    void lowValueCommareaIsNeitherEnterNorReenterNumericallyZero() {
        CarddemoCommarea c = CarddemoCommarea.lowValues();
        assertTrue(c.isPgmEnter());
        assertFalse(c.isPgmReenter());
        c.cdemoPgmContext = "*";
        assertFalse(c.isPgmEnter());
        assertFalse(c.isPgmReenter());
    }

    @ParameterizedTest
    @CsvSource({"DFHENTER,ENTER", "DFHCLEAR,CLEAR", "DFHPA1,PA1", "DFHPA2,PA2", "DFHPF1,PFK01",
        "DFHPF3,PFK03", "DFHPF12,PFK12", "DFHPF13,PFK01", "DFHPF15,PFK03", "DFHPF24,PFK12"})
    void storePfKeyMapsAids(Dfhaid aid, CcardAid expected) {
        CcWorkArea cc = new CcWorkArea();
        StorePfKey.storePfKey(aid, cc);
        assertTrue(cc.is(expected));
        assertEquals(expected.value(), cc.ccardAid);
        assertEquals(5, cc.ccardAid.length());
    }

    @Test
    void storePfKeyLeavesAidUnchangedForUnmappedKeys() {
        CcWorkArea cc = new CcWorkArea();
        StorePfKey.storePfKey(Dfhaid.DFHPA3, cc);
        assertEquals("     ", cc.ccardAid);
        assertNull(StorePfKey.map(Dfhaid.DFHNULL));
        assertNull(StorePfKey.map(null));
    }

    @Test
    void workAreaRedefinesExposeNumericViews() {
        CcWorkArea cc = new CcWorkArea();
        assertTrue(CobolText.isSpaces(cc.ccAcctId));
        cc.ccAcctIdN(50);
        assertEquals("00000000050", cc.ccAcctId);
        cc.ccCardNumN(500024453765740L);
        assertEquals("0500024453765740", cc.ccCardNum);
        assertEquals(500024453765740L, cc.ccCardNumN());
        assertFalse(cc.isCcardReturnMsgOff());
        cc.ccardReturnMsg = CobolText.lowValues(75);
        assertTrue(cc.isCcardReturnMsgOff());
    }

    @Test
    void cardRecordParsesCopybookOffsets() {
        String line = "0500024453765740" + "00000000050" + "747" + CobolText.pic("Aniya Von", 50)
                + "2023-03-09" + "Y" + " ".repeat(59);
        CardRecord r = CardRecord.parse(line);
        assertEquals("0500024453765740", r.cardNum());
        assertEquals(50L, r.cardAcctId());
        assertEquals(747, r.cardCvvCd());
        assertEquals("2023-03-09", r.cardExpiraionDate());
        assertEquals("Y", r.cardActiveStatus());
        assertEquals(line, r.toFixedWidth());
    }

    @Test
    void customerRecordIs500BytesAndRoundTrips() {
        CustomerRecord c = new CustomerRecord(1, "First", "M", "Last", "1 Test St", "", "Town", "NY",
                "USA", "10001", "(555)555-0100", "", 0, "ID", "1990-01-01", "EFT1", "Y", 750);
        String raw = c.toFixedWidth();
        assertEquals(CustomerRecord.RECORD_LENGTH, raw.length());
        assertEquals(c, CustomerRecord.parse(raw));
    }

    @Test
    void secUserDataIs80Bytes() {
        SecUserData u = new SecUserData("USER0001", "F", "L", "PASSWORD", "U", "");
        assertEquals(SecUserData.RECORD_LENGTH, u.toFixedWidth().length());
        assertEquals(u, SecUserData.parse(u.toFixedWidth()));
    }

    @Test
    void abendDataStartsAsSpacesAnd134Bytes() {
        AbendData a = new AbendData();
        assertEquals(" ".repeat(AbendData.LENGTH), a.toFixedWidth());
        a.abendCode("0001");
        a.abendCulprit("COCRDSLC");
        assertTrue(a.toFixedWidth().startsWith("0001COCRDSLC"));
    }

    @Test
    void wsDateTimeFormatsLikeCsdat01y() {
        WsDateTime d = new WsDateTime();
        d.setFromCurrentDate(LocalDateTime.of(2026, 1, 2, 3, 4, 5, 670_000_000));
        assertEquals("20260102", d.wsCurdateN());
        assertEquals("01/02/26", d.wsCurdateMmDdYy());
        assertEquals("03:04:05", d.wsCurtimeHhMmSs());
        assertEquals("2026-01-02 03:04:05.670000", d.wsTimestamp());
    }

    @Test
    void mapInputPadsTransmittedDataAndLowValuesTheRest() {
        CcrdslaInput in = CcrdslaInput.of("50", null);
        assertEquals("50         ", in.acctsidi());
        assertEquals(CobolText.lowValues(16), in.cardsidi());
        assertEquals(CobolText.lowValues(80), in.errmsgi());
    }

    @Test
    void mapOutputStartsAsLowValuesWithNoCursor() {
        CcrdslaOutput o = new CcrdslaOutput();
        for (CcrdslaField f : CcrdslaField.values()) {
            assertEquals(CobolText.lowValues(f.length()), o.data(f));
            assertEquals(0, o.field(f).attribute());
        }
        assertTrue(o.cursor().isEmpty());
        o.field(CcrdslaField.ERRMSG).data("x".repeat(100));
        assertEquals(80, o.data(CcrdslaField.ERRMSG).length());
    }

    @Test
    void titlesAndMessagesKeepPictureLengths() {
        assertEquals(40, ScreenTitles.CCDA_TITLE01.length());
        assertEquals(40, ScreenTitles.CCDA_TITLE02.length());
        assertEquals(40, ScreenTitles.CCDA_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_THANK_YOU.length());
        assertEquals(50, CommonMessages.CCDA_MSG_INVALID_KEY.length());
    }

    @Test
    void bmsAndAidConstantsMatchIbmValues() {
        assertEquals(0x61, Dfhbmsca.DFHBMPRF);
        assertEquals(0xC1, Dfhbmsca.DFHBMFSE);
        assertEquals(0xF2, Dfhbmsca.DFHRED);
        assertEquals(0x7D, Dfhaid.DFHENTER.ebcdic());
        assertEquals(0xF3, Dfhaid.DFHPF3.ebcdic());
    }
}

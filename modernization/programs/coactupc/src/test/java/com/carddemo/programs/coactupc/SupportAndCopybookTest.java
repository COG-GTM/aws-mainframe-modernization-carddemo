package com.carddemo.programs.coactupc;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.coactupc.copybook.*;
import com.carddemo.programs.coactupc.support.*;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import org.junit.jupiter.api.Test;

class SupportAndCopybookTest {

    static List<String> lines(String name) throws Exception {
        return Files.readAllLines(CoactupcTest.DATA.resolve(name), StandardCharsets.ISO_8859_1);
    }

    // CVACT01Y: every fixture record round-trips byte for byte through the 300-byte layout
    @Test
    void accountFixtureRoundTrips() throws Exception {
        for (String line : lines("acctdata.txt")) {
            AccountRecord a = AccountRecord.parse(line);
            assertEquals(Cobol.pad(line, AccountRecord.RECORD_LENGTH), a.toFixedWidth());
        }
        AccountRecord first = AccountRecord.parse(lines("acctdata.txt").get(0));
        assertEquals(new BigDecimal("194.00"), first.currBal());
        assertEquals(new BigDecimal("2020.00"), first.creditLimit());
        assertEquals(2, first.currBal().scale());
    }

    // CVCUS01Y / CVACT03Y round trips
    @Test
    void customerAndXrefFixturesRoundTrip() throws Exception {
        for (String line : lines("custdata.txt")) {
            assertEquals(Cobol.pad(line, CustomerRecord.RECORD_LENGTH), CustomerRecord.parse(line).toFixedWidth());
        }
        for (String line : lines("cardxref.txt")) {
            assertEquals(Cobol.pad(line, CardXrefRecord.RECORD_LENGTH), CardXrefRecord.parse(line).toFixedWidth());
        }
    }

    // signed zoned decimal: trailing overpunch sign, high-order truncation
    @Test
    void zonedDecimal() {
        assertEquals("00000001940{", ZonedDecimal.encode(new BigDecimal("194.00"), 10, 2));
        assertEquals("00000001940}", ZonedDecimal.encode(new BigDecimal("-194.00"), 10, 2));
        assertEquals(new BigDecimal("-12.34"), ZonedDecimal.decode("00000000123M", 2));
        assertEquals(new BigDecimal("2.99"), ZonedDecimal.truncate(new BigDecimal("12.999"), 1, 2));
    }

    // FUNCTION NUMVAL / NUMVAL-C / TEST-NUMVAL(-C)
    @Test
    void numericFunctions() {
        assertEquals(0, NumericFunctions.testNumvalC("+      2,020.00"));
        assertEquals(0, NumericFunctions.testNumval(" -12.5 "));
        assertNotEquals(0, NumericFunctions.testNumvalC("12A"));
        assertNotEquals(0, NumericFunctions.testNumval("1.2.3"));
        assertEquals(0, new BigDecimal("2020.00").compareTo(NumericFunctions.numvalC("+      2,020.00")));
        assertEquals(0, new BigDecimal("-3.5").compareTo(NumericFunctions.numval("3.5-")));
    }

    // PIC +ZZZ,ZZZ,ZZZ.99
    @Test
    void editedPicture() {
        assertEquals("+      2,020.00", EditedPicture.plusZzzCommaZzzCommaZzzDot99(new BigDecimal("2020.00")));
        assertEquals("-           .50", EditedPicture.plusZzzCommaZzzCommaZzzDot99(new BigDecimal("-0.50")));
        assertEquals("+123,456,789.12", EditedPicture.plusZzzCommaZzzCommaZzzDot99(new BigDecimal("123456789.129")));
    }

    @Test
    void cobolAlphanumericHelpers() {
        assertEquals("AB   ", Cobol.pad("AB", 5));
        assertEquals("ABC", Cobol.pad("ABCDE", 3));
        assertTrue(Cobol.alnumEquals("AB", "AB   "));
        assertFalse(Cobol.alnumEquals(Cobol.lowValues(2), "  "));
        assertTrue(Cobol.isNumeric("0123"));
        assertFalse(Cobol.isNumeric("12 3"));
        assertEquals("AB", Cobol.trim("  AB  "));
        assertEquals("000000013 ", Cobol.binaryToAlnum(13, 10));
    }

    // CSLKPCDY 88-level value lists
    @Test
    void lookupCodes() {
        assertEquals(490, LookupCodes.VALID_PHONE_AREA_CODE.size());
        assertEquals(410, LookupCodes.VALID_GENERAL_PURP_CODE.size());
        assertEquals(80, LookupCodes.VALID_EASY_RECOG_AREA_CODE.size());
        assertEquals(56, LookupCodes.VALID_US_STATE_CODE.size());
        assertEquals(240, LookupCodes.VALID_US_STATE_ZIP_CD2_COMBO.size());
        assertTrue(LookupCodes.validGeneralPurpCode("908"));
        assertFalse(LookupCodes.validGeneralPurpCode("211"));
        assertTrue(LookupCodes.validUsStateZipCd2Combo("NC27"));
        assertFalse(LookupCodes.validUsStateCode("XX"));
    }

    // CSSTRPFY: PF13-PF24 fold onto PF1-PF12
    @Test
    void pfKeyStore() {
        CcWorkArea cc = new CcWorkArea();
        PfKeyStore.storePfkey(AidKey.DFHPF15, cc);
        assertTrue(cc.is(CcWorkArea.CcardAid.PFK03));
        PfKeyStore.storePfkey(AidKey.DFHENTER, cc);
        assertTrue(cc.is(CcWorkArea.CcardAid.ENTER));
        PfKeyStore.storePfkey(AidKey.DFHPF24, cc);
        assertTrue(cc.is(CcWorkArea.CcardAid.PFK12));
    }

    // FUNCTION INTEGER-OF-DATE
    @Test
    void integerOfDate() {
        assertEquals(1, DateEditProcedures.integerOfDate(16010101L));
        assertEquals(DateEditProcedures.integerOfDate(20240228L) + 1, DateEditProcedures.integerOfDate(20240229L));
    }
}

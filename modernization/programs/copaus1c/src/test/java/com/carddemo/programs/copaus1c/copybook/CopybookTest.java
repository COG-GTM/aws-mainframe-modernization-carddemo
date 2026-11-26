package com.carddemo.programs.copaus1c.copybook;

import com.carddemo.programs.copaus1c.CpvdInfo;
import com.carddemo.programs.copaus1c.DeclineReasonTable;
import com.carddemo.programs.copaus1c.FraudCommarea;
import com.carddemo.programs.copaus1c.ImsStatus;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @Test
    void packedDecimalRoundTripsSignedValues() {
        byte[] rec = new byte[7];
        Cobol.writePacked(rec, 0, 7, 2, new BigDecimal("-12345.67"));
        assertEquals(new BigDecimal("-12345.67"), Cobol.readPacked(rec, 0, 7, 2));
        Cobol.writePacked(rec, 0, 7, 2, new BigDecimal("1.24"));
        assertEquals((byte) 0x4C, rec[6]);
        assertEquals((byte) 0x12, rec[5]);
    }

    @Test
    void authorizationKeySplitsIntoPackedDateAndTime() {
        AuthorizationKey k = AuthorizationKey.of(76699, 998747444L);
        assertEquals(76699, k.authDate9c());
        assertEquals(998747444L, k.authTime9c());
        assertEquals(8, k.bytes().length);
        assertTrue(AuthorizationKey.SPACES.isSpaces());
        assertTrue(AuthorizationKey.LOW_VALUES.isLowValues());
        assertFalse(k.isSpaces());
        assertTrue(k.compareTo(AuthorizationKey.of(76699, 998748388L)) < 0);
    }

    @Test
    void pendingAuthDetailsFieldsTruncateAndPad() {
        PendingAuthDetails d = PendingAuthDetails.blank();
        d.setMerchantName("A VERY LONG MERCHANT NAME THAT OVERFLOWS");
        assertEquals("A VERY LONG MERCHANT N", d.getMerchantName());
        d.setCardNum("41");
        assertEquals("41              ", d.getCardNum());
        d.setAuthRespCode("00");
        assertTrue(d.isAuthApproved());
        d.setFraudStatus(PendingAuthDetails.FraudStatus.PA_FRAUD_CONFIRMED);
        assertTrue(d.isFraudConfirmed());
        d.setFraudStatus(PendingAuthDetails.FraudStatus.PA_FRAUD_REMOVED);
        assertTrue(d.isFraudRemoved());
        d.setMatchStatus('M');
        assertTrue(d.hasMatchStatus(PendingAuthDetails.MatchStatus.PA_MATCHED_WITH_TRAN));
        assertEquals(200, d.toEbcdic().length);
    }

    @Test
    void numericDisplayFieldsKeepRawBytesForAlphanumericMoves() {
        PendingAuthDetails d = PendingAuthDetails.blank();
        d.setProcessingCodeDisplay("      ");
        assertEquals("      ", d.getProcessingCodeDisplay());
        assertEquals("      ", PendingAuthDetails.fromEbcdic(d.toEbcdic()).getProcessingCodeDisplay());
        d.setPosEntryMode(105);
        assertEquals("05", d.getPosEntryModeDisplay());
        assertEquals(5L, d.getPosEntryMode());
    }

    @Test
    void summaryEncodesToHundredBytes() {
        PendingAuthSummary s = PendingAuthSummary.of(12345678901L, 987654321L);
        s.setCreditLimit(new BigDecimal("5000.009"));
        assertEquals(new BigDecimal("5000.00"), s.getCreditLimit());
        PendingAuthSummary back = PendingAuthSummary.fromEbcdic(s.toEbcdic());
        assertEquals(12345678901L, back.getAcctId());
        assertEquals(987654321L, back.getCustId());
        assertEquals(new BigDecimal("5000.00"), back.getCreditLimit());
    }

    @Test
    void commareaInitializeAndContextFlags() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initialize();
        assertTrue(c.isPgmEnter());
        assertEquals("00000000000", c.getAcctId());
        c.setPgmContext(CardDemoCommarea.PgmContext.CDEMO_PGM_REENTER);
        assertTrue(c.isPgmReenter());
        c.setUserType('A');
        assertTrue(c.isUserType(CardDemoCommarea.UserType.CDEMO_USRTYP_ADMIN));
    }

    @Test
    void cpvdInfoInitializeBlanksKeysAndZeroesPage() {
        CpvdInfo info = new CpvdInfo();
        assertTrue(info.isNextPageNo());
        info.setPageNum(12);
        info.initialize();
        assertEquals(0, info.getPageNum());
        assertTrue(info.getPauSelected().isSpaces());
        assertEquals(' ', info.getNextPageFlg());
    }

    @Test
    void dateTimeWorkAreaFormatsMmDdYyAndHhMmSs() {
        DateTimeWorkArea dt = new DateTimeWorkArea();
        dt.moveCurrentDate(LocalDateTime.of(2024, 12, 31, 23, 59, 58));
        assertEquals("2024", dt.getCurdateYear());
        dt.setCurdateMm("12");
        dt.setCurdateDd("31");
        dt.setCurdateYy("24");
        assertEquals("12/31/24", dt.getCurdateMmDdYy());
        dt.setCurtimeHh("23");
        dt.setCurtimeMm("59");
        dt.setCurtimeSs("58");
        assertEquals("23:59:58", dt.getCurtimeHhMmSs());
    }

    @Test
    void declineReasonTableSearchAll() {
        assertEquals("INSUFFICNT FUND ", DeclineReasonTable.searchAll("4100").orElseThrow().declDesc());
        assertEquals("UNKNOWN         ", DeclineReasonTable.searchAll("9000").orElseThrow().declDesc());
        assertTrue(DeclineReasonTable.searchAll("1234").isEmpty());
        assertEquals(10, DeclineReasonTable.ENTRIES.size());
    }

    @Test
    void imsStatusConditionNames() {
        assertTrue(ImsStatus.isStatusOk("  "));
        assertTrue(ImsStatus.isStatusOk("FW"));
        assertTrue(ImsStatus.isSegmentNotFound("GE"));
        assertTrue(ImsStatus.isEndOfDb("GB"));
        assertTrue(ImsStatus.isPsbScheduledMoreThanOnce("TC"));
        assertTrue(ImsStatus.isRetryCondition("FH"));
        assertFalse(ImsStatus.isStatusOk("GE"));
    }

    @Test
    void fraudCommareaActionsAndStatus() {
        FraudCommarea f = new FraudCommarea();
        f.setAction(FraudCommarea.Action.WS_REPORT_FRAUD);
        assertTrue(f.isReportFraud());
        f.setAction(FraudCommarea.Action.WS_REMOVE_FRAUD);
        assertTrue(f.isRemoveFraud());
        f.setUpdateFailed();
        assertTrue(f.isUpdateFailed());
        f.setActMsg("X".repeat(60));
        assertEquals(50, f.getActMsg().length());
    }

    @Test
    void messageAndTitleConstants() {
        assertEquals(Cobol.x("Invalid key pressed. Please see below...", 50), CommonMessages.CCDA_MSG_INVALID_KEY);
        assertEquals(40, ScreenTitles.CCDA_TITLE01.length());
    }
}

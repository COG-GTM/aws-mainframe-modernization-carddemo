package com.carddemo.programs.copaus2c;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.copaus2c.copybook.FraudCommarea;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea.FraudAction;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea.FraudStatusRecord;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea.FraudUpdateStatus;
import com.carddemo.programs.copaus2c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus2c.copybook.PendingAuthDetails.AuthFraud;
import com.carddemo.programs.copaus2c.copybook.PendingAuthDetails.MatchStatus;
import com.carddemo.programs.copaus2c.copybook.Pic;

import java.math.BigDecimal;
import java.util.Optional;

import org.junit.jupiter.api.Test;

class CopybookTest {

    @Test
    void picXPadsAndTruncates() {
        assertEquals("AB  ", Pic.x("AB", 4));
        assertEquals("ABCD", Pic.x("ABCDEF", 4));
        assertEquals("    ", Pic.x(null, 4));
    }

    @Test
    void numericDisplayIgnoresDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault(java.util.Locale.Category.FORMAT);
        try {
            java.util.Locale.setDefault(java.util.Locale.Category.FORMAT, java.util.Locale.forLanguageTag("ar-EG"));
            assertEquals("+000803", Pic.plusEdited(803, 6));
        } finally {
            java.util.Locale.setDefault(java.util.Locale.Category.FORMAT, saved);
        }
    }

    @Test
    void plusEditedSignAndTruncation() {
        assertEquals("+000000", Pic.plusEdited(0, 6));
        assertEquals("+000100", Pic.plusEdited(100, 6));
        assertEquals("-000803", Pic.plusEdited(-803, 6));
        assertEquals("-234567", Pic.plusEdited(-1_234_567, 6));
    }

    @Test
    void alphanumericSqlstateToNumericEditedUsesDigitNibbles() {
        assertEquals("+000023505", Pic.plusEditedFromAlphanumeric("23505", 9));
        assertEquals("+000042893", Pic.plusEditedFromAlphanumeric("42Y93", 9));
    }

    @Test
    void stringIntoDoesNotPad() {
        assertEquals("abcXYZ", Pic.stringInto("XXXXYZ", "ab", "c"));
        assertEquals("abcd", Pic.stringInto("XXXX", "abcdef"));
    }

    @Test
    void comp3FieldsTruncateScaleAndHighOrderDigits() {
        PendingAuthDetails a = TestData.auth(1, "231027");
        PendingAuthDetails b = new PendingAuthDetails(a.authDate9C(), a.authTime9C(), a.authOrigDate(),
                a.authOrigTime(), a.cardNum(), a.authType(), a.cardExpiryDate(), a.messageType(), a.messageSource(),
                a.authIdCode(), a.authRespCode(), a.authRespReason(), 1_234_567, new BigDecimal("12345678901.239"),
                new BigDecimal("-0.999"), a.merchantCatagoryCode(), a.acqrCountryCode(), 123, a.merchantId(),
                "A merchant name longer than 22", a.merchantCity(), a.merchantState(), a.merchantZip(),
                a.transactionId(), a.matchStatus(), a.authFraud(), a.fraudRptDate(), a.filler());
        assertEquals(new BigDecimal("2345678901.23"), b.transactionAmt());
        assertEquals(new BigDecimal("-0.99"), b.approvedAmt());
        assertEquals(234_567, b.processingCode());
        assertEquals(23, b.posEntryMode());
        assertEquals("A merchant name longer", b.merchantName());
    }

    @Test
    void level88Conditions() {
        PendingAuthDetails a = TestData.auth(1, "231027");
        assertTrue(a.isAuthApproved());
        assertEquals(Optional.of(MatchStatus.PA_MATCH_PENDING), a.matchStatusCode());
        assertEquals(Optional.empty(), a.authFraudCode());
        assertEquals(Optional.of(AuthFraud.PA_FRAUD_CONFIRMED), AuthFraud.of("F"));
        assertEquals(Optional.of(AuthFraud.PA_FRAUD_REMOVED), AuthFraud.of("R"));
        assertEquals(Optional.of(MatchStatus.PA_MATCHED_WITH_TRAN), MatchStatus.of("M"));

        FraudStatusRecord report = new FraudStatusRecord("F", "S", "");
        assertTrue(report.isReportFraud());
        assertFalse(report.isRemoveFraud());
        assertTrue(report.isUpdtSuccess());
        assertFalse(report.isUpdtFailed());
        assertEquals(Optional.of(FraudAction.WS_REMOVE_FRAUD), FraudAction.of("R"));
        assertEquals(Optional.of(FraudUpdateStatus.WS_FRD_UPDT_FAILED), FraudUpdateStatus.of("F"));
    }

    @Test
    void commareaEbcdicRoundTripIs272Bytes() {
        FraudCommarea c = TestData.commarea("F", TestData.auth(998_747_444L, "231027"), "msg");
        byte[] bytes = c.toEbcdic();
        assertEquals(272, FraudCommarea.LENGTH);
        assertEquals(272, bytes.length);
        FraudCommarea back = FraudCommarea.fromEbcdic(bytes);
        assertEquals(c, back);
        assertArrayEquals(bytes, back.toEbcdic());
    }
}

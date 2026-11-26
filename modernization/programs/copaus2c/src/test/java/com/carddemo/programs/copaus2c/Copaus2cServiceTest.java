package com.carddemo.programs.copaus2c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.copaus2c.copybook.AuthFrdsRow;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea;
import com.carddemo.programs.copaus2c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus2c.copybook.Sqlca;
import com.carddemo.programs.copaus2c.gateway.InMemoryAuthFrdsGateway;
import com.carddemo.programs.copaus2c.gateway.InMemoryAuthFrdsGateway.StoredRow;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class Copaus2cServiceTest {

    private InMemoryAuthFrdsGateway db2;
    private Copaus2cService service;

    @BeforeEach
    void setUp() {
        db2 = new InMemoryAuthFrdsGateway(TestData.CLOCK);
        service = new Copaus2cService(db2, TestData.CLOCK);
    }

    private static String pad50(String s) {
        return s + " ".repeat(50 - s.length());
    }

    @Nested
    class MainParaTimestamp {

        @Test
        void authTsIsYyMmDdFromOrigDateAndTimeFromNinesComplement() {
            // 999999999 - 998747444 = 001252555 -> HH=00 MI=12 SS=52 SSS=555
            assertEquals("23-10-27 00.12.52555000", Copaus2cService.buildWsAuthTs(TestData.auth(998_747_444L, "231027")));
        }

        @Test
        void authTsHostVariableIsPaddedToChar26() {
            AuthFrdsRow row = Copaus2cService.buildAuthFrdsRow(1, 2, TestData.auth(998_747_444L, "231027"), "F");
            assertEquals("23-10-27 00.12.52555000   ", row.authTs());
            assertEquals(26, row.authTs().length());
        }

        @Test
        void zeroAuthTimeGivesAllNines() {
            assertEquals("23-10-27 99.99.99999000", Copaus2cService.buildWsAuthTs(TestData.auth(0, "231027")));
        }

        @Test
        void negativeAuthTimeOverflowsUnsignedNineDigitsByHighOrderTruncation() {
            // 999999999 - (-1) = 1000000000 -> PIC 9(09) keeps 000000000
            assertEquals("23-10-27 00.00.00000000", Copaus2cService.buildWsAuthTs(TestData.auth(-1, "231027")));
        }

        @Test
        void formatTimeMmddyyUsesSlashDateSeparator() {
            assertEquals("11/26/26", Copaus2cService.formatTimeMmddyy(LocalDate.of(2026, 11, 26)));
        }
    }

    @Nested
    class MainParaMoves {

        @Test
        void everyHostVariableIsPopulatedFromCommarea() {
            PendingAuthDetails auth = TestData.auth(998_747_444L, "231027");
            AuthFrdsRow row = Copaus2cService.buildAuthFrdsRow(41_000_000_123L, 123_456_789L, auth, "F");

            assertEquals("4154603697010011", row.cardNum());
            assertEquals("0100", row.authType());
            assertEquals("1122", row.cardExpiryDate());
            assertEquals("1234  ", row.messageType());
            assertEquals("102030", row.messageSource());
            assertEquals("041252", row.authIdCode());
            assertEquals("00", row.authRespCode());
            assertEquals("0000", row.authRespReason());
            assertEquals("005000", row.processingCode());
            assertEquals(new BigDecimal("1234567890.12"), row.transactionAmt());
            assertEquals(new BigDecimal("-7.50"), row.approvedAmt());
            assertEquals("5442", row.merchantCatagoryCode());
            assertEquals("USA", row.acqrCountryCode());
            assertEquals(1, row.posEntryMode());
            assertEquals("235010006754230", row.merchantId());
            assertEquals(22, row.merchantNameLen());
            assertEquals("Amazon.com            ", row.merchantNameText());
            assertEquals("Amazon.com            ", row.merchantName());
            assertEquals("Wilmington   ", row.merchantCity());
            assertEquals("DE", row.merchantState());
            assertEquals("19801    ", row.merchantZip());
            assertEquals("960b9b5480d045a", row.transactionId());
            assertEquals("P", row.matchStatus());
            assertEquals("F", row.authFraud());
            assertEquals("          ", row.fraudRptDate());
            assertEquals(41_000_000_123L, row.acctId());
            assertEquals(123_456_789L, row.custId());
        }

        @Test
        void fraudRptDateInCommareaIsSetToCurrentDateButAuthFraudIsLeftAlone() {
            FraudCommarea out = service.execute(TestData.commarea("F", TestData.auth(998_747_444L, "231027"), ""));
            assertEquals("11/26/26", out.fraudAuthRecord().fraudRptDate());
            assertEquals(" ", out.fraudAuthRecord().authFraud());
            assertEquals(41_000_000_123L, out.acctId());
            assertEquals(123_456_789L, out.custId());
        }
    }

    @Nested
    class InsertPath {

        @Test
        void insertSuccessSetsAddSuccess() {
            FraudCommarea out = service.execute(TestData.commarea("F", TestData.auth(998_747_444L, "231027"), "old"));

            assertTrue(out.fraudStatusRecord().isUpdtSuccess());
            assertEquals("S", out.fraudStatusRecord().frdUpdateStatus());
            assertEquals(pad50("ADD SUCCESS"), out.fraudStatusRecord().frdActMsg());
            assertEquals("F", out.fraudStatusRecord().frdAction());

            StoredRow stored = db2.find("4154603697010011", LocalDateTime.of(2023, 10, 27, 0, 12, 52, 555_000_000))
                    .orElseThrow();
            assertEquals("F", stored.authFraud());
            assertEquals(LocalDate.of(2026, 11, 26), stored.fraudRptDate());
            assertEquals(1, db2.rows().size());
        }

        @Test
        void otherSqlErrorSetsFailedWithSystemErrorMessage() {
            db2.failNextInsert(new Sqlca(-911, "40001"));
            FraudCommarea out = service.execute(TestData.commarea("F", TestData.auth(998_747_444L, "231027"), ""));

            assertTrue(out.fraudStatusRecord().isUpdtFailed());
            assertEquals(" SYSTEM ERROR DB2: CODE:-000911, STATE: +000040001", out.fraudStatusRecord().frdActMsg());
            assertEquals(50, out.fraudStatusRecord().frdActMsg().length());
            assertTrue(db2.rows().isEmpty());
        }

        @Test
        void invalidTimestampFromDb2IsReportedAsSystemError() {
            FraudCommarea out = service.execute(TestData.commarea("F", TestData.auth(0, "231027"), ""));

            assertTrue(out.fraudStatusRecord().isUpdtFailed());
            assertEquals(" SYSTEM ERROR DB2: CODE:-020448, STATE: +000022007", out.fraudStatusRecord().frdActMsg());
        }
    }

    @Nested
    class FraudUpdatePath {

        @Test
        void duplicateKeyUpdatesExistingRowAndSetsUpdtSuccess() {
            PendingAuthDetails auth = TestData.auth(998_747_444L, "231027");
            service.execute(TestData.commarea("F", auth, ""));

            FraudCommarea out = service.execute(TestData.commarea("R", auth, ""));

            assertTrue(out.fraudStatusRecord().isUpdtSuccess());
            assertTrue(out.fraudStatusRecord().isRemoveFraud());
            assertEquals(pad50("UPDT SUCCESS"), out.fraudStatusRecord().frdActMsg());
            assertEquals(1, db2.rows().size());
            assertEquals("R", db2.rows().iterator().next().authFraud());
        }

        @Test
        void updateNotFoundSetsFailedAndStringLeavesTrailingMessageBytes() {
            db2.failNextInsert(InMemoryAuthFrdsGateway.DUPLICATE);
            String incoming = "x".repeat(48) + "YZ";
            FraudCommarea out = service.execute(TestData.commarea("R", TestData.auth(998_747_444L, "231027"), incoming));

            assertTrue(out.fraudStatusRecord().isUpdtFailed());
            // STRING writes 48 chars and does not pad: positions 49-50 keep the caller's bytes
            assertEquals(" UPDT ERROR DB2: CODE:+000100, STATE: +000002000YZ", out.fraudStatusRecord().frdActMsg());
        }

        @Test
        void updateErrorWithBlankIncomingMessageEndsInSpaces() {
            db2.failNextInsert(InMemoryAuthFrdsGateway.DUPLICATE);
            db2.failNextUpdate(new Sqlca(-904, "57011"));
            FraudCommarea out = service.execute(TestData.commarea("F", TestData.auth(998_747_444L, "231027"), ""));

            assertEquals(" UPDT ERROR DB2: CODE:-000904, STATE: +000057011  ", out.fraudStatusRecord().frdActMsg());
            assertFalse(out.fraudStatusRecord().isUpdtSuccess());
        }
    }
}

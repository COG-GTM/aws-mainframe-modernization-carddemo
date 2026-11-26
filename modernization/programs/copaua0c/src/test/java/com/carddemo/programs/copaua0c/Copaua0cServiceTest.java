package com.carddemo.programs.copaua0c;

import static com.carddemo.programs.copaua0c.Harness.ACCT;
import static com.carddemo.programs.copaua0c.Harness.CARD;
import static com.carddemo.programs.copaua0c.Harness.CUST;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.copaua0c.copybook.ErrorLogRecord;
import com.carddemo.programs.copaua0c.copybook.MqConstants;
import com.carddemo.programs.copaua0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaua0c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaua0c.gateway.inmemory.InMemoryMqGateway.Reply;
import java.math.BigDecimal;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class Copaua0cServiceTest {

    private static final String TXN1 = "TXN000000000001";
    private static final String TXN2 = "TXN000000000002";

    private static String reply(String card, String txn, String code, String reason, String amt) {
        return card + "," + txn + ",101530," + code + "," + reason + "," + amt + ",";
    }

    private static ErrorLogRecord onlyLog(Harness h) {
        assertEquals(1, h.tdq.entries().size(), "error log entries");
        assertEquals("CSSL", h.tdq.entries().get(0).queue());
        return h.tdq.entries().get(0).record();
    }

    private static void assertLog(ErrorLogRecord r, String loc, char level, char subsystem, String code1,
            String code2, String message, String eventKey) {
        assertEquals(loc, r.location());
        assertEquals(level, r.levelCode());
        assertEquals(subsystem, r.subsystemCode());
        assertEquals(String.format("%-9s", code1), r.code1());
        assertEquals(String.format("%-9s", code2), r.code2());
        assertEquals(String.format("%-50s", message), r.message());
        assertEquals(String.format("%-20s", eventKey), r.eventKey());
    }

    private static PendingAuthSummary summary(String limit, String balance) {
        PendingAuthSummary s = new PendingAuthSummary();
        s.acctId(ACCT);
        s.custId(CUST);
        s.creditLimit(new BigDecimal(limit));
        s.creditBalance(new BigDecimal(balance));
        s.approvedAuthCnt(3);
        s.approvedAuthAmt(new BigDecimal("300.00"));
        return s;
    }

    @Nested
    class MainFlow {

        @Test
        void initializeOpensTriggerQueueAndReadsWithCobolOptions() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            Copaua0cRunResult r = h.run();

            assertEquals(Harness.REQUEST_Q, h.mq.openedQueue().trim());
            assertEquals(MqConstants.MQOO_INPUT_SHARED, h.mq.openOptions());
            int expected = MqConstants.MQGMO_NO_SYNCPOINT + MqConstants.MQGMO_WAIT + MqConstants.MQGMO_CONVERT
                    + MqConstants.MQGMO_FAIL_IF_QUIESCING;
            assertEquals(2, h.mq.getOptions().size());
            h.mq.getOptions().forEach(g -> {
                assertEquals(expected, g.options());
                assertEquals(5000, g.waitInterval());
            });
            assertEquals(new Copaua0cRunResult(1, false, false), r);
            assertEquals(1, h.syncpoints);
            assertTrue(h.mq.closed());
            assertTrue(h.tdq.entries().isEmpty());
        }

        @Test
        void noTriggerDataLeavesQueueNameSpaces() {
            Harness h = new Harness();
            h.service().run(Optional.empty());
            assertEquals(" ".repeat(48), h.mq.openedQueue());
        }

        @Test
        void emptyQueueEndsWithoutProcessing() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00");
            Copaua0cRunResult r = h.run();
            assertEquals(new Copaua0cRunResult(0, false, false), r);
            assertTrue(h.ims.calls().isEmpty());
            assertTrue(h.mq.replies().isEmpty());
            assertTrue(h.mq.closed());
        }

        @Test
        void loopStopsAfterRequestLimitOf500() {
            Harness h = new Harness().withStandardCard("9999999.00", "0.00");
            for (int i = 0; i < 502; i++) {
                h.send(CARD, "1.00", TXN1);
            }
            Copaua0cRunResult r = h.run();
            assertEquals(new Copaua0cRunResult(501, false, true), r);
            assertEquals(1, h.mq.pending());
            assertEquals(501, h.syncpoints);
            assertEquals(501, h.ims.details(ACCT).size());
            assertEquals(501, h.ims.summary(ACCT).approvedAuthCnt());
        }
    }

    @Nested
    class Decision {

        @Test
        void approvedFromAccountMasterWhenNoSummary() {
            Harness h = new Harness().withStandardCard("1000.00", "250.00").send(CARD, "100.00", TXN1);
            h.run();

            Reply reply = h.mq.replies().get(0);
            assertEquals(reply(CARD, TXN1, "00", "0000", "        100.00") + " ", reply.payload());
            assertEquals(64, reply.bufferLength());

            PendingAuthSummary s = h.ims.summary(ACCT);
            assertEquals(CUST, s.custId());
            assertEquals(new BigDecimal("1000.00"), s.creditLimit());
            assertEquals(new BigDecimal("500.00"), s.cashLimit());
            assertEquals(1, s.approvedAuthCnt());
            assertEquals(new BigDecimal("100.00"), s.approvedAuthAmt());
            assertEquals(new BigDecimal("100.00"), s.creditBalance());
            assertEquals(List.of("SCHD PSBPAUTB", "GU PAUTSUM0 " + ACCT, "ISRT PAUTSUM0 " + ACCT,
                    "ISRT PAUTDTL1 " + ACCT), h.ims.calls());
        }

        @Test
        void vsamKeysAndErrorCodesUseAsciiDigitsUnderAnyLocale() {
            Locale saved = Locale.getDefault();
            Locale.setDefault(Locale.forLanguageTag("th-TH-u-nu-thai"));
            try {
                Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
                h.cust.failWith(84, 0);
                h.run();
                assertLog(onlyLog(h), "C003", 'C', 'C', "000000084", "000000000", "FAILED TO READ CUST FILE",
                        "123456789");
            } finally {
                Locale.setDefault(saved);
            }
        }

        @Test
        void amountEqualToAvailableIsApproved() {
            Harness h = new Harness().withStandardCard("1000.00", "250.00").send(CARD, "750.00", TXN1);
            h.run();
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "00", "0000", "        750.00")));
        }

        @Test
        void declinedInsufficientFundFromAccountMaster() {
            Harness h = new Harness().withStandardCard("1000.00", "250.00").send(CARD, "750.01", TXN1);
            h.run();
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "05", "4100", "          0.00")));
            PendingAuthSummary s = h.ims.summary(ACCT);
            assertEquals(1, s.declinedAuthCnt());
            assertEquals(0, s.approvedAuthCnt());
            PendingAuthDetails d = h.ims.details(ACCT).get(0);
            assertTrue(d.isMatchAuthDeclined());
            assertEquals(new BigDecimal("0.00"), d.approvedAmt());
        }

        @Test
        void summaryLimitTakesPrecedenceOverAccountMaster() {
            Harness h = new Harness().withStandardCard("5000.00", "0.00").send(CARD, "100.00", TXN1);
            h.ims.putSummary(summary("1000.00", "950.00"));
            h.run();
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "05", "4100", "          0.00")));
            PendingAuthSummary s = h.ims.summary(ACCT);
            assertEquals(new BigDecimal("5000.00"), s.creditLimit(), "8400 refreshes limit from ACCT master");
            assertEquals(1, s.declinedAuthCnt());
            assertEquals(3, s.approvedAuthCnt());
            assertTrue(h.ims.calls().contains("REPL PAUTSUM0 " + ACCT));
        }

        @Test
        void approvedFromSummaryAccumulatesBalanceAndZeroesCashBalance() {
            Harness h = new Harness().withStandardCard("5000.00", "0.00").send(CARD, "40.00", TXN1);
            PendingAuthSummary existing = summary("1000.00", "950.00");
            existing.cashBalance(new BigDecimal("12.34"));
            h.ims.putSummary(existing);
            h.run();
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "00", "0000", "         40.00")));
            PendingAuthSummary s = h.ims.summary(ACCT);
            assertEquals(new BigDecimal("990.00"), s.creditBalance());
            assertEquals(new BigDecimal("0.00"), s.cashBalance().setScale(2));
            assertEquals(4, s.approvedAuthCnt());
            assertEquals(new BigDecimal("340.00"), s.approvedAuthAmt());
        }

        @Test
        void declinedAmountAccumulatesPreviousDetailAmount() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00")
                    .send(CARD, "100.00", TXN1).send(CARD, "5000.00", TXN2);
            h.run();
            PendingAuthSummary s = h.ims.summary(ACCT);
            assertEquals(1, s.declinedAuthCnt());
            // COBOL adds PA-TRANSACTION-AMT (detail segment, still holding message 1) not the request amount.
            assertEquals(new BigDecimal("100.00"), s.declinedAuthAmt());
            assertEquals(new BigDecimal("5000.00"), h.ims.details(ACCT).get(1).transactionAmt());
        }

        @Test
        void negativeAndNonNumericAmounts() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00")
                    .send(CARD, "-25.00", TXN1).send(CARD, "ABC", TXN2);
            h.run();
            String second = h.mq.replies().get(1).payload();
            assertTrue(second.startsWith(reply(CARD, TXN1, "00", "0000", "-        25.00")));
            assertTrue(second.contains(reply(CARD, TXN2, "00", "0000", "          0.00")));
        }

        @Test
        void amountTruncatedToPictureBeforeDecision() {
            Harness h = new Harness().withStandardCard("9999999.00", "0.00").send(CARD, "12345678901.999", TXN1);
            h.run();
            assertEquals(new BigDecimal("2345678901.90"), h.ims.details(ACCT).get(0).transactionAmt());
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "05", "4100", "          0.00")));
        }
    }

    @Nested
    class ReasonCodes {

        @Test
        void cardNotFoundLogsWarningDeclines3100AndSkipsDb() {
            Harness h = new Harness().send(CARD, "10.00", TXN1);
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "A001", 'W', 'A', "", "", "CARD NOT FOUND IN XREF", CARD);
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "05", "3100", "          0.00")));
            assertEquals(List.of("SCHD PSBPAUTB"), h.ims.calls());
            assertFalse(r.endedByCriticalError());
        }

        @Test
        void accountNotFoundLogsWarningDeclines3100() {
            Harness h = new Harness().send(CARD, "10.00", TXN1);
            h.xref.put(new com.carddemo.programs.copaua0c.copybook.CardXrefRecord(CARD, CUST, ACCT));
            h.cust.put(Harness.customer(CUST));
            h.run();
            assertLog(onlyLog(h), "A002", 'W', 'A', "", "", "ACCT NOT FOUND IN XREF", "12345678901");
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "05", "3100", "          0.00")));
            assertEquals(1, h.ims.summary(ACCT).declinedAuthCnt());
        }

        @Test
        void customerNotFoundDoesNotBlockApproval() {
            Harness h = new Harness().send(CARD, "10.00", TXN1);
            h.xref.put(new com.carddemo.programs.copaua0c.copybook.CardXrefRecord(CARD, CUST, ACCT));
            h.acct.put(Harness.account(ACCT, "1000.00", "0.00", "0.00"));
            h.run();
            assertLog(onlyLog(h), "A003", 'W', 'A', "", "", "CUST NOT FOUND IN XREF", "123456789");
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "00", "0000", "         10.00")));
        }

        @Test
        void customerNotFoundTakesPriorityOverInsufficientFund() {
            Harness h = new Harness().send(CARD, "5000.00", TXN1);
            h.xref.put(new com.carddemo.programs.copaua0c.copybook.CardXrefRecord(CARD, CUST, ACCT));
            h.acct.put(Harness.account(ACCT, "1000.00", "0.00", "0.00"));
            h.run();
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "05", "3100", "          0.00")));
        }
    }

    @Nested
    class SendResponse {

        @Test
        void replyDescriptorFields() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.run();
            Reply r = h.mq.replies().get(0);
            assertEquals(Harness.REPLY_Q, r.od().objectName().trim());
            assertEquals(MqConstants.MQMT_REPLY, r.md().msgType());
            assertEquals("CORREL" + TXN1, r.md().correlId().trim());
            assertEquals(MqConstants.MQPER_NOT_PERSISTENT, r.md().persistence());
            assertEquals(50, r.md().expiry());
            assertEquals(MqConstants.MQFMT_STRING, r.md().format());
            assertEquals(MqConstants.MQPMO_NO_SYNCPOINT + MqConstants.MQPMO_DEFAULT_CONTEXT, r.pmo().options());
        }

        @Test
        void responsePointerIsNeverResetSoRepliesAccumulate() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00")
                    .send(CARD, "10.00", TXN1).send(CARD, "20.00", TXN2);
            h.run();
            Reply second = h.mq.replies().get(1);
            assertEquals(127, second.bufferLength());
            assertEquals(reply(CARD, TXN1, "00", "0000", "         10.00")
                    + reply(CARD, TXN2, "00", "0000", "         20.00") + " ", second.payload());
        }

        @Test
        void putBufferOverflowStopsTransfer() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00");
            for (int i = 0; i < 5; i++) {
                h.send(CARD, "1.00", TXN1);
            }
            h.run();
            assertEquals(201, h.mq.replies().get(3).bufferLength());
            assertEquals(200, h.mq.replies().get(3).payload().length());
            assertEquals(201, h.mq.replies().get(4).bufferLength());
            assertEquals(h.mq.replies().get(3).payload(), h.mq.replies().get(4).payload());
        }
    }

    @Nested
    class Extract {

        @Test
        void unstringLeavesMissingReceiversUnchanged() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.mq.enqueue("261126,101531," + CARD + ",AUTH,1228,0100,POS001,000000,20.00", "C2", Harness.REPLY_Q);
            h.run();
            PendingAuthDetails d = h.ims.details(ACCT).get(1);
            assertEquals(TXN1, d.transactionId());
            assertEquals("101531", d.authOrigTime());
            assertEquals(new BigDecimal("20.00"), d.transactionAmt());
            assertEquals("CORNER STORE", d.merchantName().trim());
        }

        @Test
        void unstringSplitsOnCommas() {
            assertEquals(List.of("A", "", "B"), List.of(Copaua0cService.unstring("A,,B", 3)));
            String[] r = Copaua0cService.unstring("A,", 3);
            assertEquals("A", r[0]);
            assertEquals(null, r[1]);
        }

        @Test
        void insertAuthMapsRequestAndComplementedTimestamps() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.run();
            PendingAuthDetails d = h.ims.details(ACCT).get(0);
            assertEquals(99999 - 26330, d.authDate9c());
            assertEquals(999999999L - 101530250L, d.authTime9c());
            assertEquals("261126", d.authOrigDate());
            assertEquals(CARD, d.cardNum());
            assertEquals("AUTH", d.authType());
            assertEquals("1228", d.cardExpiryDate());
            assertEquals("0100  ", d.messageType());
            assertEquals("POS001", d.messageSource());
            assertEquals("101530", d.authIdCode());
            assertEquals("00", d.authRespCode());
            assertEquals("0000", d.authRespReason());
            assertEquals(new BigDecimal("10.00"), d.approvedAmt());
            assertEquals("5411", d.merchantCatagoryCode());
            assertEquals("AUS", d.acqrCountryCode());
            assertEquals("05", d.posEntryMode());
            assertEquals("SYDNEY", d.merchantCity().trim());
            assertEquals("NS", d.merchantState());
            assertEquals("2000", d.merchantZip().trim());
            assertEquals(TXN1, d.transactionId());
            assertTrue(d.isMatchPending());
            assertTrue(d.isAuthApproved());
            assertEquals(' ', d.authFraudCode());
        }
    }

    @Nested
    class CriticalErrors {

        private void assertEnded(Harness h, Copaua0cRunResult r) {
            assertTrue(r.endedByCriticalError());
            assertEquals(0, r.messagesProcessed());
            assertEquals(0, h.syncpoints);
        }

        @Test
        void mqOpenFailure() {
            Harness h = new Harness().send(CARD, "10.00", TXN1);
            h.mq.failOpen(2, 2085);
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "M001", 'C', 'M', "000000002", "000002085", "REQ MQ OPEN ERROR", "");
            assertEnded(h, r);
            assertTrue(h.mq.getOptions().isEmpty());
            assertFalse(h.mq.closed());
        }

        @Test
        void mqGetFailure() {
            Harness h = new Harness();
            h.mq.failGet(2, 2009);
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "M003", 'C', 'C', "000000002", "000002009", "FAILED TO READ REQUEST MQ", "");
            assertEnded(h, r);
            assertTrue(h.mq.closed());
        }

        @Test
        void oversizedRequestIsLeftOnQueueAndLoggedAsReadFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00");
            h.mq.enqueue(Harness.request(CARD, "10.00", TXN1) + ",".repeat(400), "C1", Harness.REPLY_Q);
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "M003", 'C', 'C', "000000001", "000002080", "FAILED TO READ REQUEST MQ", "");
            assertEnded(h, r);
            assertEquals(1, h.mq.pending());
            assertTrue(h.ims.calls().isEmpty());
        }

        @Test
        void mqPutFailureSkipsDbWriteAndTerminatesPsb() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.mq.failPut(2, 2087);
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "M004", 'C', 'M', "000000002", "000002087", "FAILED TO PUT ON REPLY MQ", "");
            assertEnded(h, r);
            assertTrue(h.ims.details(ACCT).isEmpty());
            assertEquals("TERM", h.ims.calls().get(h.ims.calls().size() - 1));
        }

        @Test
        void mqCloseFailureIsOnlyAWarning() {
            Harness h = new Harness();
            h.mq.failClose(2, 2019);
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "M005", 'W', 'M', "000000002", "000002019", "FAILED TO CLOSE REQUEST MQ", "");
            assertFalse(r.endedByCriticalError());
        }

        @Test
        void xrefReadFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.xref.failWith(84, 3);
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "C001", 'C', 'C', "000000084", "000000003", "FAILED TO READ XREF FILE", CARD);
            assertEnded(h, r);
            assertTrue(h.mq.replies().isEmpty());
            assertEquals(List.of("SCHD PSBPAUTB", "TERM"), h.ims.calls());
            assertTrue(h.mq.closed());
        }

        @Test
        void acctReadFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.acct.failWith(19, 0);
            assertEnded(h, h.run());
            assertLog(onlyLog(h), "C002", 'C', 'C', "000000019", "000000000", "FAILED TO READ ACCT FILE",
                    "12345678901");
        }

        @Test
        void custReadFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.cust.failWith(84, 0);
            assertEnded(h, h.run());
            assertLog(onlyLog(h), "C003", 'C', 'C', "000000084", "000000000", "FAILED TO READ CUST FILE",
                    "123456789");
        }

        @Test
        void psbScheduleFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.ims.scheduleStatuses("AJ");
            assertEnded(h, h.run());
            assertLog(onlyLog(h), "I001", 'C', 'I', "AJ", "", "IMS SCHD FAILED", "");
            assertEquals(List.of("SCHD PSBPAUTB"), h.ims.calls());
        }

        @Test
        void psbScheduledMoreThanOnceIsTerminatedAndRescheduled() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.ims.scheduleStatuses("TC", "  ");
            Copaua0cRunResult r = h.run();
            assertFalse(r.endedByCriticalError());
            assertEquals(List.of("SCHD PSBPAUTB", "TERM", "SCHD PSBPAUTB"), h.ims.calls().subList(0, 3));
        }

        @Test
        void summaryGetFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.ims.getStatus("AI");
            assertEnded(h, h.run());
            assertLog(onlyLog(h), "I002", 'C', 'I', "AI", "", "IMS GET SUMMARY FAILED", "");
            assertTrue(h.mq.replies().isEmpty());
        }

        @Test
        void summaryReplaceFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.ims.putSummary(summary("1000.00", "0.00"));
            h.ims.replStatus("DJ");
            assertEnded(h, h.run());
            assertLog(onlyLog(h), "I003", 'C', 'I', "DJ", "", "IMS UPDATE SUMRY FAILED", "");
            assertTrue(h.ims.details(ACCT).isEmpty());
        }

        @Test
        void duplicateDetailKeyFailsInsert() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00")
                    .send(CARD, "10.00", TXN1).send(CARD, "20.00", TXN2);
            h.clock = () -> Harness.NOW;
            Copaua0cRunResult r = h.run();
            assertLog(onlyLog(h), "I004", 'C', 'I', "II", "", "IMS INSERT DETL FAILED", CARD);
            assertEquals(1, r.messagesProcessed());
            assertEquals(1, h.ims.details(ACCT).size());
        }

        @Test
        void injectedOkGetStatusReturnsStoredSummary() {
            Harness h = new Harness().withStandardCard("5000.00", "0.00").send(CARD, "100.00", TXN1);
            h.ims.putSummary(summary("1000.00", "950.00"));
            h.ims.getStatus("FW");
            Copaua0cRunResult r = h.run();
            assertFalse(r.endedByCriticalError());
            assertTrue(h.replyFields(0).startsWith(reply(CARD, TXN1, "05", "4100", "          0.00")));
        }

        @Test
        void summaryInsertFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.ims.isrtSummaryStatus("II");
            assertEnded(h, h.run());
            assertLog(onlyLog(h), "I003", 'C', 'I', "II", "", "IMS UPDATE SUMRY FAILED", "");
        }

        @Test
        void detailInsertFailure() {
            Harness h = new Harness().withStandardCard("1000.00", "0.00").send(CARD, "10.00", TXN1);
            h.ims.isrtDetailStatus("II");
            assertEnded(h, h.run());
            assertLog(onlyLog(h), "I004", 'C', 'I', "II", "", "IMS INSERT DETL FAILED", CARD);
        }

        @Test
        void errorRecordHeaderAndFixedWidth() {
            Harness h = new Harness().send(CARD, "10.00", TXN1);
            h.run();
            ErrorLogRecord r = onlyLog(h);
            assertEquals("261126", r.date());
            assertEquals("101530", r.time());
            assertEquals("CP00    ", r.application());
            assertEquals("COPAUA0C", r.program());
            assertEquals(ErrorLogRecord.RECORD_LENGTH, r.toFixed().length());
        }
    }
}

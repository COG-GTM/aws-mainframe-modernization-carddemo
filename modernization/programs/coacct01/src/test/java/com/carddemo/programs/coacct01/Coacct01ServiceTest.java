package com.carddemo.programs.coacct01;

import com.carddemo.programs.coacct01.gateway.CicsResp;
import com.carddemo.programs.coacct01.gateway.inmemory.InMemoryAccountRepository;
import com.carddemo.programs.coacct01.gateway.inmemory.InMemoryCicsRuntime;
import com.carddemo.programs.coacct01.gateway.inmemory.InMemoryMqGateway;
import com.carddemo.programs.coacct01.gateway.inmemory.InMemoryMqGateway.Message;
import com.carddemo.programs.coacct01.support.CobolText;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import static com.carddemo.programs.coacct01.copybook.MqConstants.*;
import static org.junit.jupiter.api.Assertions.*;

class Coacct01ServiceTest {

    static final String INPUT_QUEUE = "CARD.DEMO.REQUEST.ACCT";
    static final String ERROR_QUEUE = Coacct01Service.ERROR_QUEUE;
    static final String REPLY_QUEUE = Coacct01Service.REPLY_QUEUE;

    static final String ACCOUNT_1_REPLY = "ACCOUNT ID : 00000000001ACCOUNT STATUS : Y"
            + "BALANCE : 00000001940{CREDIT LIMIT : 00000020200{CASH LIMIT : 00000010200{"
            + "OPEN DATE : 2014-11-20EXPR DATE : 2025-05-20REIS DATE : 2025-05-20"
            + "CREDIT BAL : 00000000000{DEBIT BAL : 00000000000{GROUP ID :           ";

    InMemoryMqGateway mq;
    InMemoryCicsRuntime cics;
    InMemoryAccountRepository accounts;
    List<String> displays;

    @BeforeEach
    void setUp() throws Exception {
        mq = new InMemoryMqGateway().defineQueue(INPUT_QUEUE).defineQueue(REPLY_QUEUE).defineQueue(ERROR_QUEUE);
        cics = InMemoryCicsRuntime.triggeredBy(mq, INPUT_QUEUE);
        accounts = InMemoryAccountRepository.load(new InputStreamReader(
                getClass().getResourceAsStream("/acctdata.txt"), StandardCharsets.US_ASCII));
        displays = new ArrayList<>();
    }

    Coacct01Result run() {
        return new Coacct01Service(mq, cics, accounts, displays::add).run();
    }

    static String errDisplay(String para, String returnMessage, String cc, String rc, String queue) {
        return CobolText.pic(para, 25) + "  " + CobolText.pic(returnMessage, 25) + "  " + cc + "  " + rc
                + "  " + CobolText.pic(queue, 48);
    }

    /** 8000-TERMINATION's successful MQCLOSEs overwrite the codes after the diagnostic was written. */
    static String afterClose(String display) {
        return display.substring(0, 54) + "00  00000" + display.substring(63);
    }

    List<Message> replies() {
        return mq.messages(REPLY_QUEUE);
    }

    List<Message> errors() {
        return mq.messages(ERROR_QUEUE);
    }

    void assertAllQueuesClosed() {
        assertFalse(mq.isOpen(INPUT_QUEUE));
        assertFalse(mq.isOpen(REPLY_QUEUE));
        assertFalse(mq.isOpen(ERROR_QUEUE));
        assertTrue(cics.returned());
    }

    @Nested
    class ProcessRequestReply {

        @Test
        void inqaForFixtureAccountRepliesWithLabelledRecord() {
            Message req = mq.enqueue(INPUT_QUEUE, "INQA00000000001", "CORREL-1", "SOME.REPLY.Q");

            Coacct01Result result = run();

            assertEquals(1, result.messageCount());
            assertTrue(result.noMoreMessages());
            assertEquals(1, replies().size());
            Message reply = replies().get(0);
            assertEquals(CobolText.pic(ACCOUNT_1_REPLY, 1000), reply.data());
            assertEquals(req.msgId(), reply.msgId());
            assertEquals(CobolText.pic("CORREL-1", 24), reply.correlId());
            assertEquals(MQFMT_STRING, reply.format());
            assertEquals(MQCCSI_Q_MGR, reply.codedCharSetId());
            assertTrue(errors().isEmpty());
            assertTrue(displays.isEmpty());
            assertAllQueuesClosed();
            assertTrue(mq.messages(INPUT_QUEUE).isEmpty());
        }

        @Test
        void everyFixtureAccountIsAnsweredFromItsOwnRecord() {
            for (int id = 1; id <= 50; id++) {
                mq.enqueue(INPUT_QUEUE, "INQA" + CobolText.digits(id, 11));
            }
            Coacct01Result result = run();
            assertEquals(50, result.messageCount());
            assertEquals(50, replies().size());
            for (int i = 0; i < 50; i++) {
                assertTrue(replies().get(i).data().startsWith("ACCOUNT ID : " + CobolText.digits(i + 1, 11)
                        + "ACCOUNT STATUS : "), "reply " + (i + 1));
            }
            assertEquals(50, cics.syncpointCount());
        }

        @Test
        void accountNotFoundRepliesWithKeyOnly() {
            mq.enqueue(INPUT_QUEUE, "INQA00000099999");
            run();
            assertEquals(CobolText.pic("INVALID REQUEST PARAMETERS ACCT ID : 00000099999", 1000),
                    replies().get(0).data());
        }

        @Test
        void unknownFunctionRepliesWithKeyAndFunctionWithoutReading() {
            mq.enqueue(INPUT_QUEUE, "INQX00000000001");
            run();
            assertEquals(CobolText.pic("INVALID REQUEST PARAMETERS ACCT ID : 00000000001FUNCTION : INQX", 1000),
                    replies().get(0).data());
            assertEquals(0, accounts.readCount());
        }

        @Test
        void zeroKeyIsInvalidEvenForInqa() {
            mq.enqueue(INPUT_QUEUE, "INQA00000000000");
            run();
            assertEquals(CobolText.pic("INVALID REQUEST PARAMETERS ACCT ID : 00000000000FUNCTION : INQA", 1000),
                    replies().get(0).data());
            assertEquals(0, accounts.readCount());
        }

        @Test
        void lowerCaseFunctionIsNotInqa() {
            mq.enqueue(INPUT_QUEUE, "inqa00000000001");
            run();
            assertTrue(replies().get(0).data().endsWith("FUNCTION : inqa" + CobolText.spaces(937)));
        }

        @Test
        void overpunchedKeyIsNormalisedForTheReadButEchoedRaw() {
            mq.enqueue(INPUT_QUEUE, "INQA0000000009A");   // EBCDIC C1 -> digit 1 => account 91
            run();
            assertEquals(CobolText.pic("INVALID REQUEST PARAMETERS ACCT ID : 0000000009A", 1000),
                    replies().get(0).data());
            assertEquals(1, accounts.readCount());
        }

        @Test
        void shortMessageKeepsStaleBufferTail() {
            // 14 bytes: position 15 of MQ-BUFFER keeps its previous (space) content.
            mq.enqueue(INPUT_QUEUE, "INQA0000000000");
            run();
            assertEquals(CobolText.pic("INVALID REQUEST PARAMETERS ACCT ID : 0000000000 FUNCTION : INQA", 1000),
                    replies().get(0).data());
        }

        @Test
        void readErrorWritesDiagnosticAndEndsTask() {
            accounts.failWith(CicsResp.NOTOPEN, 0);
            mq.enqueue(INPUT_QUEUE, "INQA00000000001");
            mq.enqueue(INPUT_QUEUE, "INQA00000000002");

            Coacct01Result result = run();

            String expected = errDisplay("", "ERROR WHILE READING ACCTF", "19", "00000", INPUT_QUEUE);
            assertEquals(afterClose(expected), result.mqErrDisplay());
            assertEquals(1, errors().size());
            assertEquals(CobolText.pic(expected, 1000), errors().get(0).data());
            assertTrue(replies().isEmpty());
            assertEquals(0, result.messageCount());
            assertFalse(result.noMoreMessages());
            assertEquals(1, mq.messages(INPUT_QUEUE).size());
            assertAllQueuesClosed();
        }

        @Test
        void readErrorCodesAreTruncatedToPic() {
            accounts.failWith(CicsResp.DISABLED, 123456);
            mq.enqueue(INPUT_QUEUE, "INQA00000000001");
            run();
            assertEquals(CobolText.pic(errDisplay("", "ERROR WHILE READING ACCTF", "84", "23456", INPUT_QUEUE), 1000),
                    errors().get(0).data());
        }
    }

    @Nested
    class GetRequest {

        @Test
        void emptyQueueEndsNormallyWithoutReplies() {
            Coacct01Result result = run();
            assertEquals(0, result.messageCount());
            assertTrue(result.noMoreMessages());
            assertEquals(errDisplay("", "", "00", "00000", ""), result.mqErrDisplay());
            assertTrue(replies().isEmpty());
            assertTrue(errors().isEmpty());
            assertEquals(0, cics.syncpointCount());
            assertAllQueuesClosed();
        }

        @Test
        void getOptionsAndSyncpointBeforeEachSubsequentGet() {
            mq.enqueue(INPUT_QUEUE, "INQA00000000001");
            mq.enqueue(INPUT_QUEUE, "INQA00000000002");
            run();
            int getOptions = MQGMO_SYNCPOINT + MQGMO_FAIL_IF_QUIESCING + MQGMO_CONVERT + MQGMO_WAIT;
            int putOptions = MQPMO_SYNCPOINT + MQPMO_DEFAULT_CONTEXT + MQPMO_FAIL_IF_QUIESCING;
            assertEquals(List.of(
                    "MQOPEN " + ERROR_QUEUE + " " + (MQOO_OUTPUT + MQOO_PASS_ALL_CONTEXT + MQOO_FAIL_IF_QUIESCING),
                    "MQOPEN " + INPUT_QUEUE + " " + (MQOO_INPUT_SHARED + MQOO_SAVE_ALL_CONTEXT + MQOO_FAIL_IF_QUIESCING),
                    "MQOPEN " + REPLY_QUEUE + " " + (MQOO_OUTPUT + MQOO_PASS_ALL_CONTEXT + MQOO_FAIL_IF_QUIESCING),
                    "MQGET " + INPUT_QUEUE + " " + getOptions,
                    "MQPUT " + REPLY_QUEUE + " " + putOptions,
                    "COMMIT",
                    "MQGET " + INPUT_QUEUE + " " + getOptions,
                    "MQPUT " + REPLY_QUEUE + " " + putOptions,
                    "COMMIT",
                    "MQGET " + INPUT_QUEUE + " " + getOptions,
                    "MQCLOSE " + INPUT_QUEUE + " " + MQCO_NONE,
                    "MQCLOSE " + REPLY_QUEUE + " " + MQCO_NONE,
                    "MQCLOSE " + ERROR_QUEUE + " " + MQCO_NONE,
                    "COMMIT"), mq.calls());
            assertEquals(2, cics.syncpointCount());
        }

        @Test
        void getFailureOtherThanNoMessageIsReported() {
            mq.failGet(INPUT_QUEUE, MQCC_FAILED, MQRC_CONNECTION_BROKEN);
            Coacct01Result result = run();
            String expected = errDisplay("", "INP MQGET ERR:", "02", "02009", INPUT_QUEUE);
            assertEquals(afterClose(expected), result.mqErrDisplay());
            assertEquals(CobolText.pic(expected, 1000), errors().get(0).data());
            assertFalse(result.noMoreMessages());
            assertAllQueuesClosed();
        }

        @Test
        void messageLongerThanBufferFailsWithTruncatedMsgFailed() {
            mq.enqueue(INPUT_QUEUE, "INQA00000000001" + "X".repeat(1000));
            run();
            assertEquals(CobolText.pic(errDisplay("", "INP MQGET ERR:", "01", "02080", INPUT_QUEUE), 1000),
                    errors().get(0).data());
            assertTrue(replies().isEmpty());
            assertEquals(1, mq.messages(INPUT_QUEUE).size());
        }

        @Test
        void invalidKeyDigitAbendsAsraAndBacksOutTheUnitOfWork() {
            mq.enqueue(INPUT_QUEUE, "INQA00000000001");
            // Only 4 bytes: WS-KEY comes from the previous reply left in MQ-BUFFER ("UNT ID : 00"),
            // whose ':' is not a valid digit.
            mq.enqueue(INPUT_QUEUE, "INQA");

            Coacct01AbendException e = assertThrows(Coacct01AbendException.class, Coacct01ServiceTest.this::run);

            assertEquals("ASRA", e.abendCode());
            assertEquals("ASRA", cics.abendCode());
            assertFalse(cics.returned());
            assertEquals(1, replies().size());   // committed by the SYNCPOINT before the 2nd GET
            assertEquals(List.of("INQA"), mq.messages(INPUT_QUEUE).stream().map(Message::data).toList());
        }

        @Test
        void nonNumericKeyWithInqaAbends() {
            mq.enqueue(INPUT_QUEUE, "INQAABC:0000000");
            assertThrows(Coacct01AbendException.class, Coacct01ServiceTest.this::run);
            assertTrue(replies().isEmpty());
            assertEquals(1, mq.messages(INPUT_QUEUE).size());
        }
    }

    @Nested
    class Control {

        @Test
        void retrieveFailureReportsRespCodesAndEndsTask() {
            cics = InMemoryCicsRuntime.retrieveFails(mq, CicsResp.INVREQ, 3);
            Coacct01Result result = run();
            // RESP2 is never moved to WS-CICS-RESP2-CD-D in the COBOL, so it always shows zeros.
            String expected = errDisplay("CICS RETREIVE", "RESP: 0000001600000000END", "00", "00000", "");
            assertEquals(expected, result.mqErrDisplay());
            assertEquals(CobolText.pic(expected, 1000), errors().get(0).data());
            assertEquals(List.of(
                    "MQOPEN " + ERROR_QUEUE + " " + (MQOO_OUTPUT + MQOO_PASS_ALL_CONTEXT + MQOO_FAIL_IF_QUIESCING),
                    "MQPUT " + ERROR_QUEUE + " " + (MQPMO_SYNCPOINT + MQPMO_DEFAULT_CONTEXT + MQPMO_FAIL_IF_QUIESCING),
                    "MQCLOSE " + ERROR_QUEUE + " 0",
                    "COMMIT"), mq.calls());
            assertTrue(cics.returned());
        }

        @Test
        void errorQueueOpenFailureIsDisplayedOnly() {
            mq.failOpen(ERROR_QUEUE, MQCC_FAILED, MQRC_UNKNOWN_OBJECT_NAME);
            mq.enqueue(INPUT_QUEUE, "INQA00000000001");
            Coacct01Result result = run();
            String expected = errDisplay("", "ERR MQOPEN ERR", "02", "02085", ERROR_QUEUE);
            assertEquals(List.of(expected), displays);
            assertEquals(expected, result.mqErrDisplay());
            assertEquals(MqErrDisplay.LENGTH, expected.length());
            assertTrue(replies().isEmpty());
            assertEquals(1, mq.calls().stream().filter(c -> c.startsWith("MQ")).count());
            assertTrue(cics.returned());
        }

        @Test
        void inputQueueOpenFailureIsReportedOnErrorQueue() {
            mq.failOpen(INPUT_QUEUE, MQCC_FAILED, MQRC_UNKNOWN_OBJECT_NAME);
            Coacct01Result result = run();
            String expected = errDisplay("", "INP MQOPEN ERR", "02", "02085", INPUT_QUEUE);
            assertEquals(afterClose(expected), result.mqErrDisplay());
            assertEquals(CobolText.pic(expected, 1000), errors().get(0).data());
            assertFalse(mq.calls().contains("MQCLOSE " + INPUT_QUEUE + " 0"));
            assertFalse(mq.isOpen(ERROR_QUEUE));
        }

        @Test
        void outputQueueOpenFailureClosesInputQueue() {
            mq.failOpen(REPLY_QUEUE, MQCC_FAILED, MQRC_UNKNOWN_OBJECT_NAME);
            mq.enqueue(INPUT_QUEUE, "INQA00000000001");
            run();
            assertEquals(1, errors().size());
            assertEquals(CobolText.pic(errDisplay("", "OUT MQOPEN ERR", "02", "02085", REPLY_QUEUE), 1000),
                    errors().get(0).data());
            assertEquals(1, mq.messages(INPUT_QUEUE).size());
            assertAllQueuesClosed();
        }

        @Test
        void unknownTriggerQueueNameFailsInputOpen() {
            cics = InMemoryCicsRuntime.triggeredBy(mq, "NO.SUCH.QUEUE");
            run();
            assertEquals(CobolText.pic(errDisplay("", "INP MQOPEN ERR", "02", "02085", "NO.SUCH.QUEUE"), 1000),
                    errors().get(0).data());
        }
    }

    @Nested
    class PutAndError {

        @Test
        void replyPutFailureIsReportedWithReplyQueueName() {
            mq.failPut(REPLY_QUEUE, MQCC_FAILED, MQRC_Q_FULL);
            mq.enqueue(INPUT_QUEUE, "INQA00000000001");
            Coacct01Result result = run();
            String expected = errDisplay("", "MQPUT ERR", "02", "02053", REPLY_QUEUE);
            assertEquals(afterClose(expected), result.mqErrDisplay());
            assertEquals(CobolText.pic(expected, 1000), errors().get(0).data());
            assertEquals(0, result.messageCount());
            assertAllQueuesClosed();
        }

        @Test
        void errorQueuePutFailureIsDisplayed() {
            cics = InMemoryCicsRuntime.retrieveFails(mq, CicsResp.ENDDATA, 0);
            mq.failPut(ERROR_QUEUE, MQCC_FAILED, MQRC_Q_FULL);
            Coacct01Result result = run();
            String expected = errDisplay("CICS RETREIVE", "MQPUT ERR", "02", "02053", ERROR_QUEUE);
            assertEquals(List.of(expected), displays);
            assertEquals(afterClose(expected), result.mqErrDisplay());
            assertTrue(errors().isEmpty());
            assertTrue(cics.returned());
        }
    }

    @Nested
    class Termination {

        @Test
        void inputCloseFailureEndsTaskWithoutClosingOtherQueues() {
            mq.failClose(INPUT_QUEUE, MQCC_FAILED, MQRC_HOBJ_ERROR);
            Coacct01Result result = run();
            assertEquals(errDisplay("", "MQCLOSE ERR", "02", "02019", INPUT_QUEUE), result.mqErrDisplay());
            assertTrue(mq.isOpen(REPLY_QUEUE));
            assertTrue(mq.isOpen(ERROR_QUEUE));
            assertTrue(errors().isEmpty());
            assertTrue(cics.returned());
        }

        @Test
        void outputCloseFailureReportsInputQueueName() {
            mq.failClose(REPLY_QUEUE, MQCC_FAILED, MQRC_HOBJ_ERROR);
            Coacct01Result result = run();
            assertEquals(errDisplay("", "MQCLOSE ERR", "02", "02019", INPUT_QUEUE), result.mqErrDisplay());
            assertFalse(mq.isOpen(INPUT_QUEUE));
            assertTrue(mq.isOpen(ERROR_QUEUE));
        }

        @Test
        void errorCloseFailureWritesDiagnosticToErrorQueue() {
            mq.failClose(ERROR_QUEUE, MQCC_FAILED, MQRC_HOBJ_ERROR);
            Coacct01Result result = run();
            String expected = errDisplay("", "MQCLOSE ERR", "02", "02019", ERROR_QUEUE);
            assertEquals(CobolText.pic(expected, 1000), errors().get(0).data());
            assertEquals(errDisplay("", "MQCLOSE ERR", "00", "00000", ERROR_QUEUE), result.mqErrDisplay());
            assertTrue(cics.returned());
        }
    }

    @Test
    void workingStorageIsResetForEachTask() {
        Coacct01Service service = new Coacct01Service(mq, cics, accounts, displays::add);
        mq.enqueue(INPUT_QUEUE, "INQA00000000001");
        assertEquals(1, service.run().messageCount());
        mq.enqueue(INPUT_QUEUE, "INQA00000000002");
        assertEquals(1, service.run().messageCount());
        assertEquals(2, replies().size());
    }
}

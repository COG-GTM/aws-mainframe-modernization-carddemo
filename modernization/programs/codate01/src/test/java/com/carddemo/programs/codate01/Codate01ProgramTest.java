package com.carddemo.programs.codate01;

import static com.carddemo.programs.codate01.TestSupport.INPUT_QUEUE;
import static com.carddemo.programs.codate01.TestSupport.errDisplay;
import static com.carddemo.programs.codate01.TestSupport.pad;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.codate01.copybook.MqConstants;
import com.carddemo.programs.codate01.gateway.CicsResp;
import com.carddemo.programs.codate01.gateway.InMemoryCicsGateway;
import com.carddemo.programs.codate01.gateway.InMemoryMqGateway.CloseCall;
import com.carddemo.programs.codate01.gateway.InMemoryMqGateway.OpenCall;
import com.carddemo.programs.codate01.gateway.InMemoryMqGateway.PutCall;
import com.carddemo.programs.codate01.gateway.InMemoryMqGateway.StoredMessage;
import com.carddemo.programs.codate01.gateway.RetrieveResult;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class Codate01ProgramTest {

    private static final String EXPECTED_REPLY = "SYSTEM DATE : 11-26-2026SYSTEM TIME : 17:22:05";
    private static final String REPLY_Q = Codate01Program.REPLY_QUEUE_NAME;
    private static final String ERROR_Q = Codate01Program.ERROR_QUEUE_NAME;

    @Nested
    class MainLine {

        @Test
        void requestIsAnsweredWithSystemDateAndTimeOnReplyQueue() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "SOME.OTHER.REPLY.Q");

            Codate01Result r = t.run();

            List<StoredMessage> replies = t.mq.messages(REPLY_Q);
            assertEquals(1, replies.size());
            assertEquals(pad(EXPECTED_REPLY, 1000), replies.get(0).data());
            assertEquals(1, r.messagesProcessed());
            assertEquals(pad(INPUT_QUEUE, 48), r.inputQueueName());
            assertEquals(pad(REPLY_Q, 48), r.replyQueueName());
            assertEquals(pad(ERROR_Q, 48), r.errorQueueName());
            assertEquals(EXPECTED_REPLY, r.lastReply().text().trim());
            assertTrue(t.mq.messages("SOME.OTHER.REPLY.Q").isEmpty(), "MQMD-REPLYTOQ is saved but never used");
            assertTrue(t.mq.messages(ERROR_Q).isEmpty());
            assertTrue(t.mq.messages(INPUT_QUEUE).isEmpty());
            assertTrue(t.displayed.isEmpty());
        }

        @Test
        void replyCarriesRequestMsgIdAndCorrelIdAsStringFormatUnderSyncpoint() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");

            t.run();

            PutCall put = t.mq.puts().get(0);
            assertEquals(REPLY_Q, put.queueName());
            assertEquals(pad("REQMSG000000000000000001", 24), put.descriptor().msgId());
            assertEquals(pad("REQCOR000000000000000001", 24), put.descriptor().correlId());
            assertEquals("MQSTR   ", put.descriptor().format());
            assertEquals(MqConstants.MQCCSI_Q_MGR, put.descriptor().codedCharSetId);
            assertEquals(2 + 32 + 8192, put.options());
            assertEquals(1000, put.bufferLength());
        }

        @Test
        void queuesAreOpenedErrorFirstThenInputThenOutputWithCobolOptions() {
            TestSupport t = new TestSupport();
            t.run();

            List<OpenCall> opens = t.mq.opens();
            assertEquals(List.of(ERROR_Q, INPUT_QUEUE, REPLY_Q), opens.stream().map(OpenCall::queueName).toList());
            assertEquals(16 + 512 + 8192, opens.get(0).options());
            assertEquals(2 + 128 + 8192, opens.get(1).options());
            assertEquals(16 + 512 + 8192, opens.get(2).options());
            assertTrue(opens.stream().allMatch(o -> o.hconn() == 0));
        }

        @Test
        void getUsesFiveSecondWaitSyncpointConvertAndResetsMsgIdCorrelId() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");
            t.run();

            var get = t.mq.gets().get(0);
            assertEquals(INPUT_QUEUE, get.queueName());
            assertEquals(5000, get.waitInterval());
            assertEquals(1 + 2 + 8192 + 16384, get.options());
            assertEquals(MqConstants.MQMI_NONE, get.msgIdIn());
            assertEquals(MqConstants.MQCI_NONE, get.correlIdIn());
            assertEquals(1000, get.bufferLength());
            // the second GET resets MsgId/CorrelId again after the first GET filled them
            var second = t.mq.gets().get(1);
            assertEquals(MqConstants.MQMI_NONE, second.msgIdIn());
            assertEquals(MqConstants.MQCI_NONE, second.correlIdIn());
        }

        @Test
        void emptyQueueEndsWithoutSyncpointOrReplyAndClosesAllQueues() {
            TestSupport t = new TestSupport();

            Codate01Result r = t.run();

            assertEquals(0, r.messagesProcessed());
            assertNull(r.lastReply());
            assertEquals(0, t.cics.syncpointCount());
            assertEquals(0, t.cics.askTimeCount());
            assertEquals(1, t.cics.returnCount());
            assertTrue(t.mq.messages(REPLY_Q).isEmpty());
            assertEquals(List.of(INPUT_QUEUE, REPLY_Q, ERROR_Q),
                    t.mq.closes().stream().map(CloseCall::queueName).toList());
            assertTrue(t.mq.closes().stream().allMatch(c -> c.options() == MqConstants.MQCO_NONE));
            assertEquals(0, t.mq.openHandleCount());
            assertEquals(errDisplay("", "", "00", "00000", ""), r.errDisplay());
        }

        @Test
        void fixtureRequestsAreDrainedOneReplyAndOneSyncpointEach() throws IOException {
            List<String> requests = fixtureRequests();
            TestSupport t = new TestSupport();
            Iterator<LocalDateTime> clock = List.of(
                    LocalDateTime.of(2026, 1, 2, 3, 4, 5),
                    LocalDateTime.of(2026, 12, 31, 23, 59, 59, 999_000_000),
                    LocalDateTime.of(2000, 2, 29, 0, 0, 0),
                    LocalDateTime.of(1999, 12, 31, 12, 0, 1),
                    LocalDateTime.of(2026, 11, 26, 9, 30, 0)).iterator();
            t.cics = new InMemoryCicsGateway(t.cics.retrieve(), clock::next);
            requests.forEach(req -> t.mq.enqueue(INPUT_QUEUE, req, ""));

            Codate01Result r = t.run();

            assertEquals(5, requests.size());
            assertEquals(5, r.messagesProcessed());
            assertEquals(5, t.cics.syncpointCount());
            assertEquals(1, t.cics.returnCount());
            List<String> replies = t.mq.messages(REPLY_Q).stream().map(m -> m.data().trim()).toList();
            assertEquals(List.of(
                    "SYSTEM DATE : 01-02-2026SYSTEM TIME : 03:04:05",
                    "SYSTEM DATE : 12-31-2026SYSTEM TIME : 23:59:59",
                    "SYSTEM DATE : 02-29-2000SYSTEM TIME : 00:00:00",
                    "SYSTEM DATE : 12-31-1999SYSTEM TIME : 12:00:01",
                    "SYSTEM DATE : 11-26-2026SYSTEM TIME : 09:30:00"), replies);
            assertEquals(requests, r.requests().stream().map(q -> q.func() + q.key()).toList());
            // bytes past MQ-DATA-LENGTH keep the previous reply that 4100-PUT-REPLY moved into MQ-BUFFER
            assertTrue(r.requests().get(1).filler().startsWith("1-02-2026SYSTEM TIME : 03:04:05"));
            assertEquals("00000000005", r.requests().get(4).key());
            // the final (empty) MQGET re-initializes WS-KEY only
            assertEquals("INQA", r.lastRequest().func());
            assertEquals("00000000000", r.lastRequest().key());
            for (int i = 0; i < 5; i++) {
                assertEquals(pad(String.format("REQCOR%018d", i + 1), 24),
                        t.mq.messages(REPLY_Q).get(i).descriptor().correlId());
            }
        }

        @Test
        void programIsInitialSoEachRunStartsFromFreshWorkingStorage() {
            TestSupport t = new TestSupport();
            Codate01Program program = t.program();
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000002", "");
            assertEquals(2, program.run().messagesProcessed());

            t.mq.enqueue(INPUT_QUEUE, "INQA00000000003", "");
            Codate01Result second = program.run();
            assertEquals(1, second.messagesProcessed());
            assertEquals(1, second.requests().size());
            assertEquals("00000000003", second.requests().get(0).key());
        }

        @Test
        void shortRequestKeepsResidueOfPreviousReplyInMqBuffer() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");
            t.mq.enqueue(INPUT_QUEUE, "DATE", "");

            Codate01Result r = t.run();

            // MQGET overwrites only MQ-DATA-LENGTH bytes; MQ-BUFFER still holds the first reply
            RequestMsgCopy second = r.requests().get(1);
            assertEquals("DATE", second.func());
            assertEquals(EXPECTED_REPLY.substring(4, 15), second.key());
            assertTrue(second.filler().startsWith(EXPECTED_REPLY.substring(15)));
            assertEquals(2, t.mq.messages(REPLY_Q).size());
        }

        @Test
        void nonNumericKeyIsAcceptedBecauseRequestIsNeverValidated() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "XXXXnot-a-number", "");

            Codate01Result r = t.run();

            assertEquals(1, r.messagesProcessed());
            assertEquals("XXXX", r.requests().get(0).func());
            assertEquals(false, r.requests().get(0).keyIsNumeric());
            assertEquals(EXPECTED_REPLY, t.mq.messages(REPLY_Q).get(0).data().trim());
        }
    }

    @Nested
    class RetrieveFailure {

        @Test
        void notFoundIsReportedOnErrorQueueWithResp1AndZeroResp2() {
            TestSupport t = new TestSupport();
            t.cics = new InMemoryCicsGateway(RetrieveResult.failed(CicsResp.NOTFND, 7), () -> TestSupport.NOW);

            Codate01Result r = t.run();

            String expected = errDisplay("CICS RETRIEVE", "RESP: 0000001300000000END", "00", "00000", "");
            List<StoredMessage> errors = t.mq.messages(ERROR_Q);
            assertEquals(1, errors.size());
            assertEquals(pad(expected, 1000), errors.get(0).data());
            assertEquals("MQSTR   ", errors.get(0).descriptor().format());
            assertEquals(MqConstants.MQMI_NONE, errors.get(0).descriptor().msgId());
            assertEquals(List.of(ERROR_Q), t.mq.opens().stream().map(OpenCall::queueName).toList());
            assertEquals(List.of(ERROR_Q), t.mq.closes().stream().map(CloseCall::queueName).toList());
            assertTrue(t.mq.gets().isEmpty());
            assertEquals(1, t.cics.returnCount());
            assertEquals(pad("", 48), r.inputQueueName());
            assertEquals(pad("", 48), r.replyQueueName());
        }

        @Test
        void lengthErrorShowsResp22() {
            TestSupport t = new TestSupport();
            t.cics = new InMemoryCicsGateway(RetrieveResult.failed(CicsResp.LENGERR, 11), () -> TestSupport.NOW);

            t.run();

            assertEquals(pad(errDisplay("CICS RETRIEVE", "RESP: 0000002200000000END", "00", "00000", ""), 1000),
                    t.mq.messages(ERROR_Q).get(0).data());
        }
    }

    @Nested
    class OpenFailures {

        @Test
        void errorQueueOpenFailureIsDisplayedAndTaskEndsBeforeRetrieve() {
            TestSupport t = new TestSupport();
            t.mq.failOpen(ERROR_Q, MqConstants.MQRC_UNKNOWN_OBJECT_NAME);

            Codate01Result r = t.run();

            String expected = errDisplay("", "ERR MQOPEN ERR", "02", "02085", ERROR_Q);
            assertEquals(List.of(expected), t.displayed);
            assertEquals(expected, r.errDisplay());
            assertEquals(0, t.cics.retrieveCount());
            assertTrue(t.mq.closes().isEmpty());
            assertTrue(t.mq.puts().isEmpty());
            assertEquals(1, t.cics.returnCount());
        }

        @Test
        void inputQueueOpenFailureGoesToErrorQueueAndClosesOnlyErrorQueue() {
            TestSupport t = new TestSupport();
            t.mq.failOpen(INPUT_QUEUE, MqConstants.MQRC_NOT_AUTHORIZED);

            Codate01Result r = t.run();

            assertEquals(pad(errDisplay("", "INP MQOPEN ERR", "02", "02035", INPUT_QUEUE), 1000),
                    t.mq.messages(ERROR_Q).get(0).data());
            assertEquals(List.of(ERROR_Q, INPUT_QUEUE), t.mq.opens().stream().map(OpenCall::queueName).toList());
            assertEquals(List.of(ERROR_Q), t.mq.closes().stream().map(CloseCall::queueName).toList());
            // the successful error-queue MQPUT overwrites CC/RC afterwards
            assertEquals(errDisplay("", "INP MQOPEN ERR", "00", "00000", INPUT_QUEUE), r.errDisplay());
        }

        @Test
        void outputQueueOpenFailureClosesInputAndErrorQueues() {
            TestSupport t = new TestSupport();
            t.mq.failOpen(REPLY_Q, MqConstants.MQRC_UNKNOWN_OBJECT_NAME);
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");

            t.run();

            assertEquals(pad(errDisplay("", "OUT MQOPEN ERR", "02", "02085", REPLY_Q), 1000),
                    t.mq.messages(ERROR_Q).get(0).data());
            assertEquals(List.of(INPUT_QUEUE, ERROR_Q), t.mq.closes().stream().map(CloseCall::queueName).toList());
            assertTrue(t.mq.gets().isEmpty());
            assertEquals(1, t.mq.messages(INPUT_QUEUE).size());
        }

        @Test
        void triggerForUndefinedQueueFailsInputOpen() {
            TestSupport t = new TestSupport();
            t.cics = new InMemoryCicsGateway(RetrieveResult.normal(
                    com.carddemo.programs.codate01.copybook.MqTriggerMessage.forQueue("NO.SUCH.QUEUE", "CDRD")),
                    () -> TestSupport.NOW);

            t.run();

            assertEquals(pad(errDisplay("", "INP MQOPEN ERR", "02", "02085", "NO.SUCH.QUEUE"), 1000),
                    t.mq.messages(ERROR_Q).get(0).data());
        }
    }

    @Nested
    class GetAndPutFailures {

        @Test
        void getFailureOtherThanNoMessageIsReportedAndAllQueuesClosed() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");
            t.mq.failGet(INPUT_QUEUE, MqConstants.MQRC_GET_INHIBITED, 1);

            Codate01Result r = t.run();

            assertEquals(pad(errDisplay("", "INP MQGET ERR:", "02", "02016", INPUT_QUEUE), 1000),
                    t.mq.messages(ERROR_Q).get(0).data());
            assertEquals(0, r.messagesProcessed());
            assertEquals(List.of(INPUT_QUEUE, REPLY_Q, ERROR_Q),
                    t.mq.closes().stream().map(CloseCall::queueName).toList());
        }

        @Test
        void getFailureAfterFirstMessageStillCommitsFirstReplyViaSyncpoint() {
            TestSupport u = new TestSupport();
            u.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");
            u.mq.enqueue(INPUT_QUEUE, "INQA00000000002", "");
            FailSecondGet wrapper = new FailSecondGet(u.mq);
            Codate01Result r = new Codate01Program(wrapper, u.cics, u.displayed::add).run();
            assertEquals(1, r.messagesProcessed());
            assertEquals(1, u.cics.syncpointCount());
            assertEquals(1, u.mq.messages(REPLY_Q).size());
            assertEquals(pad(errDisplay("", "INP MQGET ERR:", "02", "02161", INPUT_QUEUE), 1000),
                    u.mq.messages(ERROR_Q).get(0).data());
        }

        @Test
        void messageLongerThan1000BytesFailsWithTruncatedMsgFailed() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "INQA" + "9".repeat(1200), "");

            Codate01Result r = t.run();

            assertEquals(pad(errDisplay("", "INP MQGET ERR:", "02", "02080", INPUT_QUEUE), 1000),
                    t.mq.messages(ERROR_Q).get(0).data());
            assertEquals(0, r.messagesProcessed());
            assertEquals(1, t.mq.messages(INPUT_QUEUE).size(), "message is left on the queue");
        }

        @Test
        void replyPutFailureIsReportedWithReplyQueueNameAndCountNotIncremented() {
            TestSupport t = new TestSupport();
            t.mq.enqueue(INPUT_QUEUE, "INQA00000000001", "");
            t.mq.failPut(REPLY_Q, MqConstants.MQRC_PUT_INHIBITED, -1);

            Codate01Result r = t.run();

            StoredMessage err = t.mq.messages(ERROR_Q).get(0);
            assertEquals(pad(errDisplay("", "MQPUT ERR", "02", "02051", REPLY_Q), 1000), err.data());
            // 9000-ERROR reuses the MQMD that 4100-PUT-REPLY primed with the request ids
            assertEquals(pad("REQMSG000000000000000001", 24), err.descriptor().msgId());
            assertEquals(0, r.messagesProcessed());
            assertEquals(0, t.cics.syncpointCount());
            assertEquals(List.of(INPUT_QUEUE, REPLY_Q, ERROR_Q),
                    t.mq.closes().stream().map(CloseCall::queueName).toList());
        }

        @Test
        void errorQueuePutFailureIsDisplayedInsteadOfQueued() {
            TestSupport t = new TestSupport();
            t.mq.failOpen(INPUT_QUEUE, MqConstants.MQRC_NOT_AUTHORIZED);
            t.mq.failPut(ERROR_Q, MqConstants.MQRC_Q_FULL, -1);

            Codate01Result r = t.run();

            assertEquals(List.of(errDisplay("", "MQPUT ERR", "02", "02053", ERROR_Q)), t.displayed);
            // 5200-CLOSE-ERROR-QUEUE then succeeds and overwrites CC/RC
            assertEquals(errDisplay("", "MQPUT ERR", "00", "00000", ERROR_Q), r.errDisplay());
            assertTrue(t.mq.messages(ERROR_Q).isEmpty());
            assertEquals(List.of(ERROR_Q), t.mq.closes().stream().map(CloseCall::queueName).toList());
        }
    }

    @Nested
    class CloseFailures {

        @Test
        void transientInputCloseFailureReentersTerminationAndRetries() {
            TestSupport t = new TestSupport();
            t.mq.failClose(INPUT_QUEUE, MqConstants.MQRC_HOBJ_ERROR, 1);

            Codate01Result r = t.run();

            assertEquals(List.of(INPUT_QUEUE, INPUT_QUEUE, REPLY_Q, ERROR_Q),
                    t.mq.closes().stream().map(CloseCall::queueName).toList());
            assertEquals(errDisplay("", "MQCLOSE ERR", "00", "00000", INPUT_QUEUE), r.errDisplay());
            assertTrue(t.mq.messages(ERROR_Q).isEmpty(), "5000-CLOSE-INPUT-QUEUE does not call 9000-ERROR");
            assertEquals(1, t.cics.returnCount());
        }

        @Test
        void persistentInputCloseFailureLoopsUntilRunawayAbendAica() {
            TestSupport t = new TestSupport();
            t.mq.failClose(INPUT_QUEUE, MqConstants.MQRC_HOBJ_ERROR, -1);

            Codate01AbendException e = assertThrows(Codate01AbendException.class, t::run);

            assertEquals("AICA", e.abendCode());
            assertEquals(Codate01Program.MAX_TERMINATION_DEPTH, t.mq.closes().size());
            assertEquals(errDisplay("", "MQCLOSE ERR", "02", "02019", INPUT_QUEUE), e.errDisplay());
            assertEquals(0, t.cics.returnCount());
        }

        @Test
        void outputCloseFailureReclosesTheAlreadyClosedInputQueueUntilAica() {
            TestSupport t = new TestSupport();
            t.mq.failClose(REPLY_Q, MqConstants.MQRC_Q_MGR_QUIESCING, 1);

            Codate01AbendException e = assertThrows(Codate01AbendException.class, t::run);

            assertEquals("AICA", e.abendCode());
            List<CloseCall> closes = t.mq.closes();
            assertEquals(INPUT_QUEUE, closes.get(0).queueName());
            assertEquals(REPLY_Q, closes.get(1).queueName());
            assertEquals(MqConstants.MQRC_Q_MGR_QUIESCING, closes.get(1).result().reasonCode());
            // REPLY-QUEUE-OPEN is never reset, so 8000 re-closes the stale input handle forever
            assertTrue(closes.subList(2, closes.size()).stream()
                    .allMatch(c -> c.queueName() == null
                            && c.result().reasonCode() == MqConstants.MQRC_HOBJ_ERROR));
        }

        @Test
        void errorCloseFailureReportsViaErrorQueueThenLoopsUntilAica() {
            TestSupport t = new TestSupport();
            t.mq.failClose(ERROR_Q, MqConstants.MQRC_Q_MGR_QUIESCING, 1);

            Codate01AbendException e = assertThrows(Codate01AbendException.class, t::run);

            assertEquals("AICA", e.abendCode());
            assertEquals(pad(errDisplay("", "MQCLOSE ERR", "02", "02161", ERROR_Q), 1000),
                    t.mq.messages(ERROR_Q).get(0).data());
        }
    }

    /** Lets the first GET through, then fails the next one with MQRC-Q-MGR-QUIESCING. */
    private static final class FailSecondGet implements com.carddemo.programs.codate01.gateway.MqGateway {
        private final com.carddemo.programs.codate01.gateway.InMemoryMqGateway delegate;
        private int gets;

        FailSecondGet(com.carddemo.programs.codate01.gateway.InMemoryMqGateway delegate) {
            this.delegate = delegate;
        }

        @Override
        public com.carddemo.programs.codate01.gateway.MqOpenResult mqOpen(int hconn,
                com.carddemo.programs.codate01.copybook.MqObjectDescriptor od, int options) {
            return delegate.mqOpen(hconn, od, options);
        }

        @Override
        public com.carddemo.programs.codate01.gateway.MqGetResult mqGet(int hconn, int hobj,
                com.carddemo.programs.codate01.copybook.MqMessageDescriptor md,
                com.carddemo.programs.codate01.copybook.MqGetMessageOptions gmo, int bufferLength) {
            if (++gets == 2) {
                return new com.carddemo.programs.codate01.gateway.MqGetResult(MqConstants.MQCC_FAILED,
                        MqConstants.MQRC_Q_MGR_QUIESCING, 0, null);
            }
            return delegate.mqGet(hconn, hobj, md, gmo, bufferLength);
        }

        @Override
        public com.carddemo.programs.codate01.gateway.MqCallResult mqPut(int hconn, int hobj,
                com.carddemo.programs.codate01.copybook.MqMessageDescriptor md,
                com.carddemo.programs.codate01.copybook.MqPutMessageOptions pmo, int bufferLength, String buffer) {
            return delegate.mqPut(hconn, hobj, md, pmo, bufferLength, buffer);
        }

        @Override
        public com.carddemo.programs.codate01.gateway.MqCallResult mqClose(int hconn, int hobj, int options) {
            return delegate.mqClose(hconn, hobj, options);
        }
    }

    private static List<String> fixtureRequests() throws IOException {
        try (BufferedReader in = new BufferedReader(new InputStreamReader(
                Objects.requireNonNull(Codate01ProgramTest.class.getResourceAsStream("/date-requests.txt")),
                StandardCharsets.US_ASCII))) {
            return in.lines().filter(l -> !l.isBlank() && !l.startsWith("#")).toList();
        }
    }
}

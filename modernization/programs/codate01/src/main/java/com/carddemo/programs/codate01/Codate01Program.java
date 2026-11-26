package com.carddemo.programs.codate01;

import static com.carddemo.programs.codate01.CobolFields.alnum;

import com.carddemo.programs.codate01.copybook.MqConstants;
import com.carddemo.programs.codate01.copybook.MqGetMessageOptions;
import com.carddemo.programs.codate01.copybook.MqMessageDescriptor;
import com.carddemo.programs.codate01.copybook.MqObjectDescriptor;
import com.carddemo.programs.codate01.copybook.MqPutMessageOptions;
import com.carddemo.programs.codate01.copybook.MqTriggerMessage;
import com.carddemo.programs.codate01.gateway.CicsGateway;
import com.carddemo.programs.codate01.gateway.CicsResp;
import com.carddemo.programs.codate01.gateway.MqCallResult;
import com.carddemo.programs.codate01.gateway.MqGateway;
import com.carddemo.programs.codate01.gateway.MqGetResult;
import com.carddemo.programs.codate01.gateway.MqOpenResult;
import com.carddemo.programs.codate01.gateway.RetrieveResult;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * CODATE01 (transaction CDRD): MQ-triggered system date inquiry. Started by the CICS trigger monitor
 * for the request queue, it drains that queue and answers every request with the CICS system date and
 * time on CARD.DEMO.REPLY.DATE; MQ failures are reported on CARD.DEMO.ERROR.
 *
 * <p>The COBOL program is {@code IS INITIAL}: every {@link #run()} starts from fresh WORKING-STORAGE.
 */
public final class Codate01Program {

    public static final String REPLY_QUEUE_NAME = "CARD.DEMO.REPLY.DATE";
    public static final String ERROR_QUEUE_NAME = "CARD.DEMO.ERROR";
    public static final int MESSAGE_LENGTH = 1000;
    public static final int GET_WAIT_INTERVAL_MS = 5000;

    /**
     * A failing MQCLOSE PERFORMs 8000-TERMINATION, which retries the same close: in CICS that loops until
     * the runaway-task monitor abends the task (AICA). The port stops after this many nested terminations.
     */
    public static final int MAX_TERMINATION_DEPTH = 50;

    private final MqGateway mq;
    private final CicsGateway cics;
    private final Consumer<String> display;

    // 88-level flags (WS-MQ-MSG-FLAG, WS-RESP-QUEUE-STS, WS-ERR-QUEUE-STS, WS-REPLY-QUEUE-STS)
    private boolean noMoreMsgs;
    private boolean respQueueOpen;
    private boolean errQueueOpen;
    private boolean replyQueueOpen;

    // WS-CICS-RESP-CDS
    private int cicsResp1Cd;
    private int cicsResp2Cd;
    private String cicsResp1CdD;
    private String cicsResp2CdD;

    // WS-DATE-TIME
    private long absTime;
    private String mmddyyyy;
    private String time;

    // MQ fields
    private String mqQueue;
    private String mqQueueReply;
    private final int mqHconn = 0;
    private int mqConditionCode;
    private int mqReasonCode;
    private int mqHobj;
    private int mqOptions;
    private int mqBufferLength;
    private String mqBuffer;
    private int mqDataLength;
    private String mqCorrelId;
    private String mqMsgId;
    private long mqMsgCount;
    private String saveCorelId;
    private String saveMsgId;
    private String saveReply2Q;
    private MqErrDisplay errDisplay;
    private MqGetMessageOptions getMessageOptions;
    private MqPutMessageOptions putMessageOptions;
    private MqMessageDescriptor messageDescriptor;
    private MqObjectDescriptor objectDescriptor;
    private MqTriggerMessage mqtm;

    // QUEUE-INFO and handles
    private String qmgrName;
    private String inputQueueName;
    private String replyQueueName;
    private String errorQueueName;
    private int inputQueueHandle;
    private int outputQueueHandle;
    private int errorQueueHandle;
    private final int qmgrHandleConn = 0;

    private String queueMessage;
    private String requestMessage;
    private String replyMessage;
    private String errorMessage;
    private RequestMsgCopy requestMsgCopy;
    private boolean replyBuilt;
    private Consumer<RequestMsgCopy> requestListener = request -> { };

    private int terminationDepth;

    public Codate01Program(MqGateway mq, CicsGateway cics, Consumer<String> display) {
        this.mq = Objects.requireNonNull(mq);
        this.cics = Objects.requireNonNull(cics);
        this.display = Objects.requireNonNull(display);
    }

    public Codate01Program(MqGateway mq, CicsGateway cics) {
        this(mq, cics, System.out::println);
    }

    /** Observer called with REQUEST-MSG-COPY at the start of each 4000-PROCESS-REQUEST-REPLY. */
    public Codate01Program onRequest(Consumer<RequestMsgCopy> listener) {
        this.requestListener = Objects.requireNonNull(listener);
        return this;
    }

    /** Runs one CDRD task to EXEC CICS RETURN. */
    public Codate01Result run() {
        initializeWorkingStorage();
        try {
            control1000();
        } catch (CicsReturn ignored) {
            // EXEC CICS RETURN ends the task
        }
        return new Codate01Result(mqMsgCount, inputQueueName, replyQueueName, errorQueueName,
                errDisplay.render(), requestMsgCopy,
                replyBuilt ? new DateReplyMessage(replyMessage) : null);
    }

    private void initializeWorkingStorage() {
        noMoreMsgs = false;
        respQueueOpen = false;
        errQueueOpen = false;
        replyQueueOpen = false;
        cicsResp1Cd = 0;
        cicsResp2Cd = 0;
        cicsResp1CdD = "00000000";
        cicsResp2CdD = "00000000";
        absTime = 0;
        mmddyyyy = alnum("", 10);
        time = alnum("", 8);
        // Items without VALUE are assumed to start as spaces / zero.
        mqQueue = alnum("", 48);
        mqQueueReply = alnum("", 48);
        mqConditionCode = 0;
        mqReasonCode = 0;
        mqHobj = 0;
        mqOptions = 0;
        mqBufferLength = 0;
        mqBuffer = alnum("", MESSAGE_LENGTH);
        mqDataLength = 0;
        mqCorrelId = alnum("", 24);
        mqMsgId = alnum("", 24);
        mqMsgCount = 0;
        saveCorelId = alnum("", 24);
        saveMsgId = alnum("", 24);
        saveReply2Q = alnum("", 48);
        errDisplay = new MqErrDisplay();
        getMessageOptions = new MqGetMessageOptions();
        putMessageOptions = new MqPutMessageOptions();
        messageDescriptor = new MqMessageDescriptor();
        objectDescriptor = new MqObjectDescriptor();
        mqtm = null;
        qmgrName = alnum("", 48);
        inputQueueName = alnum("", 48);
        replyQueueName = alnum("", 48);
        errorQueueName = alnum("", 48);
        inputQueueHandle = 0;
        outputQueueHandle = 0;
        errorQueueHandle = 0;
        queueMessage = alnum("", MESSAGE_LENGTH);
        requestMessage = alnum("", MESSAGE_LENGTH);
        replyMessage = alnum("", MESSAGE_LENGTH);
        errorMessage = alnum("", MESSAGE_LENGTH);
        requestMsgCopy = RequestMsgCopy.initial();
        replyBuilt = false;
        terminationDepth = 0;
    }

    // 1000-CONTROL
    private void control1000() {
        inputQueueName = alnum("", 48);
        qmgrName = alnum("", 48);
        queueMessage = alnum("", MESSAGE_LENGTH);
        errDisplay.initialize();

        openErrorQueue2100();

        RetrieveResult retrieved = cics.retrieve();
        cicsResp1Cd = retrieved.resp();
        cicsResp2Cd = retrieved.resp2();
        if (cicsResp1Cd == CicsResp.NORMAL) {
            mqtm = retrieved.data();
            inputQueueName = mqtm.qName();
            replyQueueName = alnum(REPLY_QUEUE_NAME, 48);
        } else {
            errDisplay.errorPara("CICS RETRIEVE");
            cicsResp1CdD = CobolFields.unsignedDisplay(cicsResp1Cd, 8);
            // COBOL: MOVE WS-CICS-RESP2-CD TO WS-CICS-RESP2-CD (self-move), so RESP2-CD-D stays zero.
            cicsResp2Cd = cicsResp2Cd;
            errDisplay.stringIntoApplReturnMessage("RESP: ", cicsResp1CdD, cicsResp2CdD, "END");
            error9000();
            termination8000();
        }

        openInputQueue2300();
        openOutputQueue2400();
        getRequest3000();
        while (!noMoreMsgs) {
            mainProcess4000();
        }
        termination8000();
    }

    // 2300-OPEN-INPUT-QUEUE
    private void openInputQueue2300() {
        objectDescriptor.objectQMgrName("");
        objectDescriptor.objectName(inputQueueName);
        mqOptions = MqConstants.MQOO_INPUT_SHARED + MqConstants.MQOO_SAVE_ALL_CONTEXT
                + MqConstants.MQOO_FAIL_IF_QUIESCING;
        MqOpenResult r = mq.mqOpen(qmgrHandleConn, objectDescriptor, mqOptions);
        mqHobj = r.hobj();
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
            inputQueueHandle = mqHobj;
            replyQueueOpen = true;
        } else {
            setAppl();
            errDisplay.applQueueName(inputQueueName);
            errDisplay.applReturnMessage("INP MQOPEN ERR");
            error9000();
            termination8000();
        }
    }

    // 2400-OPEN-OUTPUT-QUEUE
    private void openOutputQueue2400() {
        objectDescriptor.objectQMgrName("");
        objectDescriptor.objectName(replyQueueName);
        mqOptions = MqConstants.MQOO_OUTPUT + MqConstants.MQOO_PASS_ALL_CONTEXT
                + MqConstants.MQOO_FAIL_IF_QUIESCING;
        MqOpenResult r = mq.mqOpen(qmgrHandleConn, objectDescriptor, mqOptions);
        mqHobj = r.hobj();
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
            outputQueueHandle = mqHobj;
            respQueueOpen = true;
        } else {
            setAppl();
            errDisplay.applQueueName(replyQueueName);
            errDisplay.applReturnMessage("OUT MQOPEN ERR");
            error9000();
            termination8000();
        }
    }

    // 2100-OPEN-ERROR-QUEUE
    private void openErrorQueue2100() {
        errorQueueName = alnum(ERROR_QUEUE_NAME, 48);
        objectDescriptor.objectQMgrName("");
        objectDescriptor.objectName(errorQueueName);
        mqOptions = MqConstants.MQOO_OUTPUT + MqConstants.MQOO_PASS_ALL_CONTEXT
                + MqConstants.MQOO_FAIL_IF_QUIESCING;
        MqOpenResult r = mq.mqOpen(qmgrHandleConn, objectDescriptor, mqOptions);
        mqHobj = r.hobj();
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
            errorQueueHandle = mqHobj;
            errQueueOpen = true;
        } else {
            setAppl();
            errDisplay.applQueueName(errorQueueName);
            errDisplay.applReturnMessage("ERR MQOPEN ERR");
            display.accept(errDisplay.render());
            termination8000();
        }
    }

    // 4000-MAIN-PROCESS
    private void mainProcess4000() {
        cics.syncpoint();
        getRequest3000();
    }

    // 3000-GET-REQUEST
    private void getRequest3000() {
        getMessageOptions.waitInterval = GET_WAIT_INTERVAL_MS;
        mqCorrelId = alnum("", 24);
        mqMsgId = alnum("", 24);
        mqQueue = inputQueueName;
        mqHobj = inputQueueHandle;
        mqBufferLength = MESSAGE_LENGTH;
        messageDescriptor.msgId(MqConstants.MQMI_NONE);
        messageDescriptor.correlId(MqConstants.MQCI_NONE);
        requestMsgCopy = requestMsgCopy.initializeNumericToZeroes();
        getMessageOptions.options = MqConstants.MQGMO_SYNCPOINT + MqConstants.MQGMO_FAIL_IF_QUIESCING
                + MqConstants.MQGMO_CONVERT + MqConstants.MQGMO_WAIT;

        MqGetResult r = mq.mqGet(mqHconn, mqHobj, messageDescriptor, getMessageOptions, mqBufferLength);
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        mqDataLength = r.dataLength();
        if (r.data() != null) {
            String received = r.data().length() > mqBufferLength ? r.data().substring(0, mqBufferLength) : r.data();
            mqBuffer = CobolFields.overlay(mqBuffer, received);
        }

        if (mqConditionCode == MqConstants.MQCC_OK) {
            mqMsgId = messageDescriptor.msgId();
            mqCorrelId = messageDescriptor.correlId();
            mqQueueReply = messageDescriptor.replyToQ();
            setAppl();
            requestMessage = mqBuffer;
            saveCorelId = mqCorrelId;
            saveReply2Q = mqQueueReply;
            saveMsgId = mqMsgId;
            requestMsgCopy = RequestMsgCopy.fromMessage(requestMessage);
            processRequestReply4000();
            mqMsgCount = (mqMsgCount + 1) % 1_000_000_000L;
        } else if (mqReasonCode == MqConstants.MQRC_NO_MSG_AVAILABLE) {
            noMoreMsgs = true;
        } else {
            setAppl();
            errDisplay.applQueueName(inputQueueName);
            errDisplay.applReturnMessage("INP MQGET ERR:");
            error9000();
            termination8000();
        }
    }

    // 4000-PROCESS-REQUEST-REPLY
    private void processRequestReply4000() {
        requestListener.accept(requestMsgCopy);
        replyMessage = alnum("", MESSAGE_LENGTH);
        absTime = 0; // INITIALIZE WS-DATE-TIME REPLACING NUMERIC BY ZEROES
        absTime = cics.askTime();
        mmddyyyy = CicsTime.mmddyyyy(absTime, '-');
        time = CicsTime.time(absTime, ':');
        replyMessage = DateReplyMessage.of(mmddyyyy, time).text();
        replyBuilt = true;
        putReply4100();
    }

    // 4100-PUT-REPLY
    private void putReply4100() {
        mqBuffer = replyMessage;
        mqBufferLength = MESSAGE_LENGTH;
        messageDescriptor.msgId(saveMsgId);
        messageDescriptor.correlId(saveCorelId);
        messageDescriptor.format(MqConstants.MQFMT_STRING);
        messageDescriptor.codedCharSetId = MqConstants.MQCCSI_Q_MGR;
        putMessageOptions.options = MqConstants.MQPMO_SYNCPOINT + MqConstants.MQPMO_DEFAULT_CONTEXT
                + MqConstants.MQPMO_FAIL_IF_QUIESCING;

        MqCallResult r = mq.mqPut(mqHconn, outputQueueHandle, messageDescriptor, putMessageOptions,
                mqBufferLength, mqBuffer);
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
        } else {
            setAppl();
            errDisplay.applQueueName(replyQueueName);
            errDisplay.applReturnMessage("MQPUT ERR");
            error9000();
            termination8000();
        }
    }

    // 9000-ERROR
    private void error9000() {
        errorMessage = alnum(errDisplay.render(), MESSAGE_LENGTH);
        mqBuffer = errorMessage;
        mqBufferLength = MESSAGE_LENGTH;
        messageDescriptor.format(MqConstants.MQFMT_STRING);
        messageDescriptor.codedCharSetId = MqConstants.MQCCSI_Q_MGR;
        putMessageOptions.options = MqConstants.MQPMO_SYNCPOINT + MqConstants.MQPMO_DEFAULT_CONTEXT
                + MqConstants.MQPMO_FAIL_IF_QUIESCING;

        MqCallResult r = mq.mqPut(mqHconn, errorQueueHandle, messageDescriptor, putMessageOptions,
                mqBufferLength, mqBuffer);
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
        } else {
            setAppl();
            errDisplay.applQueueName(errorQueueName);
            errDisplay.applReturnMessage("MQPUT ERR");
            display.accept(errDisplay.render());
            termination8000();
        }
    }

    // 8000-TERMINATION
    private void termination8000() {
        terminationDepth++;
        if (terminationDepth > MAX_TERMINATION_DEPTH) {
            throw new Codate01AbendException("AICA",
                    "runaway task: 8000-TERMINATION re-entered " + MAX_TERMINATION_DEPTH + " times",
                    errDisplay.render());
        }
        if (replyQueueOpen) {
            closeInputQueue5000();
        }
        if (respQueueOpen) {
            closeOutputQueue5100();
        }
        if (errQueueOpen) {
            closeErrorQueue5200();
        }
        cics.returnToCics();
        throw CicsReturn.INSTANCE;
    }

    // 5000-CLOSE-INPUT-QUEUE
    private void closeInputQueue5000() {
        mqQueue = inputQueueName;
        mqHobj = inputQueueHandle;
        mqOptions = MqConstants.MQCO_NONE;
        MqCallResult r = mq.mqClose(mqHconn, mqHobj, mqOptions);
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
        } else {
            setAppl();
            errDisplay.applQueueName(inputQueueName);
            errDisplay.applReturnMessage("MQCLOSE ERR");
            termination8000();
        }
    }

    // 5100-CLOSE-OUTPUT-QUEUE
    private void closeOutputQueue5100() {
        mqQueue = replyQueueName;
        mqHobj = outputQueueHandle;
        mqOptions = MqConstants.MQCO_NONE;
        MqCallResult r = mq.mqClose(mqHconn, mqHobj, mqOptions);
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
        } else {
            setAppl();
            // COBOL reports INPUT-QUEUE-NAME here, not REPLY-QUEUE-NAME.
            errDisplay.applQueueName(inputQueueName);
            errDisplay.applReturnMessage("MQCLOSE ERR");
            termination8000();
        }
    }

    // 5200-CLOSE-ERROR-QUEUE
    private void closeErrorQueue5200() {
        mqQueue = errorQueueName;
        mqHobj = errorQueueHandle;
        mqOptions = MqConstants.MQCO_NONE;
        MqCallResult r = mq.mqClose(mqHconn, mqHobj, mqOptions);
        mqConditionCode = r.conditionCode();
        mqReasonCode = r.reasonCode();
        if (mqConditionCode == MqConstants.MQCC_OK) {
            setAppl();
        } else {
            setAppl();
            errDisplay.applQueueName(errorQueueName);
            errDisplay.applReturnMessage("MQCLOSE ERR");
            error9000();
            termination8000();
        }
    }

    /** MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE / MOVE MQ-REASON-CODE TO MQ-APPL-REASON-CODE. */
    private void setAppl() {
        errDisplay.applConditionCode(mqConditionCode);
        errDisplay.applReasonCode(mqReasonCode);
    }

    /** Control-flow signal for EXEC CICS RETURN. */
    private static final class CicsReturn extends RuntimeException {
        static final CicsReturn INSTANCE = new CicsReturn();

        private CicsReturn() {
            super(null, null, false, false);
        }
    }
}

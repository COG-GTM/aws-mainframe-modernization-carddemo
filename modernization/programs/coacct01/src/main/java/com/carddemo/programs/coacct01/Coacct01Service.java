package com.carddemo.programs.coacct01;

import com.carddemo.programs.coacct01.copybook.MqGetMessageOptions;
import com.carddemo.programs.coacct01.copybook.MqMessageDescriptor;
import com.carddemo.programs.coacct01.copybook.MqObjectDescriptor;
import com.carddemo.programs.coacct01.copybook.MqPutMessageOptions;
import com.carddemo.programs.coacct01.dto.AccountInquiryRequest;
import com.carddemo.programs.coacct01.dto.AccountInquiryResponse;
import com.carddemo.programs.coacct01.gateway.AccountRepository;
import com.carddemo.programs.coacct01.gateway.AccountRepository.AccountReadResult;
import com.carddemo.programs.coacct01.gateway.CicsResp;
import com.carddemo.programs.coacct01.gateway.CicsRuntime;
import com.carddemo.programs.coacct01.gateway.CicsRuntime.RetrieveResult;
import com.carddemo.programs.coacct01.gateway.MqGateway;
import com.carddemo.programs.coacct01.gateway.MqGateway.MqCallResult;
import com.carddemo.programs.coacct01.gateway.MqGateway.MqGetResult;
import com.carddemo.programs.coacct01.gateway.MqGateway.MqOpenResult;
import com.carddemo.programs.coacct01.support.CobolText;
import com.carddemo.programs.coacct01.support.DataException;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Consumer;

import static com.carddemo.programs.coacct01.copybook.MqConstants.*;

/**
 * COACCT01 (transaction CDRA): MQ-triggered account inquiry. Reads 'INQA' + 11-digit account id
 * requests from the triggering queue, looks the account up in ACCTDAT and puts a labelled reply on
 * CARD.DEMO.REPLY.ACCT; diagnostics go to CARD.DEMO.ERROR. One instance per task: the COBOL
 * program is IS INITIAL, so working storage is reset on every {@link #run()}.
 */
public final class Coacct01Service {

    public static final String ERROR_QUEUE = "CARD.DEMO.ERROR";
    public static final String REPLY_QUEUE = "CARD.DEMO.REPLY.ACCT";
    public static final String LIT_ACCTFILENAME = "ACCTDAT ";
    static final int MQ_BUFFER_SIZE = 1000;
    static final int GET_WAIT_INTERVAL_MS = 5000;

    private final MqGateway mq;
    private final CicsRuntime cics;
    private final AccountRepository accounts;
    private final Consumer<String> display;

    // 88-levels
    private boolean noMoreMsgs;       // WS-MQ-MSG-FLAG      88 NO-MORE-MSGS
    private boolean respQueueOpen;    // WS-RESP-QUEUE-STS   88 RESP-QUEUE-OPEN  (reply/output queue)
    private boolean errQueueOpen;     // WS-ERR-QUEUE-STS    88 ERR-QUEUE-OPEN
    private boolean replyQueueOpen;   // WS-REPLY-QUEUE-STS  88 REPLY-QUEUE-OPEN (set for the INPUT queue)

    private int cicsResp1Cd;          // WS-CICS-RESP1-CD   PIC S9(08) COMP
    private int cicsResp2Cd;          // WS-CICS-RESP2-CD   PIC S9(08) COMP
    private long cicsResp1CdD;        // WS-CICS-RESP1-CD-D PIC 9(08)
    private long cicsResp2CdD;        // WS-CICS-RESP2-CD-D PIC 9(08)

    private String mqQueue;
    private String mqQueueReply;
    private final int mqHconn = 0;
    private int mqConditionCode;
    private int mqReasonCode;
    private int mqHobj;
    private int mqOptions;
    private int mqBufferLength;
    private char[] mqBuffer;          // MQ-BUFFER PIC X(1000), not cleared between GETs
    private int mqDataLength;
    private String mqCorrelId;
    private String mqMsgId;
    private long mqMsgCount;          // MQ-MSG-COUNT PIC 9(09)
    private String saveCorelId;
    private String saveMsgId;
    private String saveReply2Q;       // saved from MQMD-REPLYTOQ but never used by the COBOL
    private MqErrDisplay mqErrDisplay;

    private MqGetMessageOptions gmo;
    private MqPutMessageOptions pmo;
    private MqMessageDescriptor md;
    private MqObjectDescriptor od;

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
    private AccountInquiryRequest requestMsgCopy;
    private AccountInquiryResponse wsAcctResponse;
    private int wsRespCd;
    private int wsReasCd;
    private String wsCardRidAcctIdX;

    private boolean terminating;

    public Coacct01Service(MqGateway mq, CicsRuntime cics, AccountRepository accounts, Consumer<String> display) {
        this.mq = Objects.requireNonNull(mq);
        this.cics = Objects.requireNonNull(cics);
        this.accounts = Objects.requireNonNull(accounts);
        this.display = Objects.requireNonNull(display);
    }

    public Coacct01Result run() {
        initialiseWorkingStorage();
        try {
            control();
        } catch (CicsReturn ended) {
            // EXEC CICS RETURN
        } catch (DataException s0c7) {
            cics.abend("ASRA");
            throw new Coacct01AbendException("ASRA", s0c7);
        }
        return new Coacct01Result(mqMsgCount, mqErrDisplay.render(), noMoreMsgs);
    }

    private void initialiseWorkingStorage() {
        noMoreMsgs = false;
        respQueueOpen = false;
        errQueueOpen = false;
        replyQueueOpen = false;
        cicsResp1Cd = 0;
        cicsResp2Cd = 0;
        cicsResp1CdD = 0;
        cicsResp2CdD = 0;
        mqQueue = CobolText.spaces(48);
        mqQueueReply = CobolText.spaces(48);
        mqConditionCode = 0;
        mqReasonCode = 0;
        mqHobj = 0;
        mqOptions = 0;
        mqBufferLength = 0;
        mqBuffer = new char[MQ_BUFFER_SIZE];
        Arrays.fill(mqBuffer, ' ');
        mqDataLength = 0;
        mqCorrelId = CobolText.spaces(24);
        mqMsgId = CobolText.spaces(24);
        mqMsgCount = 0;
        saveCorelId = CobolText.spaces(24);
        saveMsgId = CobolText.spaces(24);
        saveReply2Q = CobolText.spaces(48);
        mqErrDisplay = new MqErrDisplay();
        gmo = new MqGetMessageOptions();
        pmo = new MqPutMessageOptions();
        md = new MqMessageDescriptor();
        od = new MqObjectDescriptor();
        qmgrName = CobolText.spaces(48);
        inputQueueName = CobolText.spaces(48);
        replyQueueName = CobolText.spaces(48);
        errorQueueName = CobolText.spaces(48);
        inputQueueHandle = 0;
        outputQueueHandle = 0;
        errorQueueHandle = 0;
        queueMessage = CobolText.spaces(MQ_BUFFER_SIZE);
        requestMessage = CobolText.spaces(MQ_BUFFER_SIZE);
        replyMessage = CobolText.spaces(MQ_BUFFER_SIZE);
        errorMessage = CobolText.spaces(MQ_BUFFER_SIZE);
        requestMsgCopy = AccountInquiryRequest.initial();
        wsAcctResponse = AccountInquiryResponse.initial();
        wsRespCd = 0;
        wsReasCd = 0;
        wsCardRidAcctIdX = CobolText.spaces(11);
        terminating = false;
    }

    // 1000-CONTROL
    private void control() {
        inputQueueName = CobolText.spaces(48);
        qmgrName = CobolText.spaces(48);
        queueMessage = CobolText.spaces(MQ_BUFFER_SIZE);
        mqErrDisplay.initialize();

        openErrorQueue();

        RetrieveResult retrieved = cics.retrieve();
        cicsResp1Cd = retrieved.resp();
        cicsResp2Cd = retrieved.resp2();
        if (cicsResp1Cd == CicsResp.NORMAL) {
            inputQueueName = CobolText.pic(retrieved.triggerMessage().qName(), 48);
            replyQueueName = CobolText.pic(REPLY_QUEUE, 48);
        } else {
            mqErrDisplay.errorPara("CICS RETREIVE");
            cicsResp1CdD = CobolText.unsignedDisplay(cicsResp1Cd, 8);
            // COBOL moves WS-CICS-RESP2-CD to itself, so WS-CICS-RESP2-CD-D stays zero.
            mqErrDisplay.stringIntoApplReturnMessage("RESP: ", CobolText.digits(cicsResp1CdD, 8),
                    CobolText.digits(cicsResp2CdD, 8), "END");
            error();
            termination();
        }

        openInputQueue();
        openOutputQueue();
        getRequest();
        while (!noMoreMsgs) {
            mainProcess();
        }
        termination();
    }

    // 2300-OPEN-INPUT-QUEUE
    private void openInputQueue() {
        od.objectQMgrName = CobolText.spaces(48);
        od.objectName = inputQueueName;
        mqOptions = MQOO_INPUT_SHARED + MQOO_SAVE_ALL_CONTEXT + MQOO_FAIL_IF_QUIESCING;
        MqOpenResult r = mq.open(qmgrHandleConn, od, mqOptions);
        setMqResult(r.conditionCode(), r.reasonCode());
        mqHobj = r.hobj();
        if (mqConditionCode == MQCC_OK) {
            recordCodes();
            inputQueueHandle = mqHobj;
            replyQueueOpen = true;
        } else {
            recordCodes();
            mqErrDisplay.applQueueName(inputQueueName);
            mqErrDisplay.applReturnMessage("INP MQOPEN ERR");
            error();
            termination();
        }
    }

    // 2400-OPEN-OUTPUT-QUEUE
    private void openOutputQueue() {
        od.objectQMgrName = CobolText.spaces(48);
        od.objectName = replyQueueName;
        mqOptions = MQOO_OUTPUT + MQOO_PASS_ALL_CONTEXT + MQOO_FAIL_IF_QUIESCING;
        MqOpenResult r = mq.open(qmgrHandleConn, od, mqOptions);
        setMqResult(r.conditionCode(), r.reasonCode());
        mqHobj = r.hobj();
        if (mqConditionCode == MQCC_OK) {
            recordCodes();
            outputQueueHandle = mqHobj;
            respQueueOpen = true;
        } else {
            recordCodes();
            mqErrDisplay.applQueueName(replyQueueName);
            mqErrDisplay.applReturnMessage("OUT MQOPEN ERR");
            error();
            termination();
        }
    }

    // 2100-OPEN-ERROR-QUEUE
    private void openErrorQueue() {
        errorQueueName = CobolText.pic(ERROR_QUEUE, 48);
        od.objectQMgrName = CobolText.spaces(48);
        od.objectName = errorQueueName;
        mqOptions = MQOO_OUTPUT + MQOO_PASS_ALL_CONTEXT + MQOO_FAIL_IF_QUIESCING;
        MqOpenResult r = mq.open(qmgrHandleConn, od, mqOptions);
        setMqResult(r.conditionCode(), r.reasonCode());
        mqHobj = r.hobj();
        if (mqConditionCode == MQCC_OK) {
            recordCodes();
            errorQueueHandle = mqHobj;
            errQueueOpen = true;
        } else {
            recordCodes();
            mqErrDisplay.applQueueName(errorQueueName);
            mqErrDisplay.applReturnMessage("ERR MQOPEN ERR");
            display.accept(mqErrDisplay.render());
            termination();
        }
    }

    // 4000-MAIN-PROCESS
    private void mainProcess() {
        cics.syncpoint();
        getRequest();
    }

    // 3000-GET-REQUEST
    private void getRequest() {
        gmo.waitInterval = GET_WAIT_INTERVAL_MS;
        mqCorrelId = CobolText.spaces(24);
        mqMsgId = CobolText.spaces(24);
        mqQueue = inputQueueName;
        mqHobj = inputQueueHandle;
        mqBufferLength = MQ_BUFFER_SIZE;
        md.msgId = MQMI_NONE;
        md.correlId = MQCI_NONE;
        requestMsgCopy = AccountInquiryRequest.initial();
        gmo.options = MQGMO_SYNCPOINT + MQGMO_FAIL_IF_QUIESCING + MQGMO_CONVERT + MQGMO_WAIT;

        MqGetResult r = mq.get(mqHconn, mqHobj, md, gmo, mqBufferLength);
        setMqResult(r.conditionCode(), r.reasonCode());
        mqDataLength = r.dataLength();
        String data = r.data() == null ? "" : r.data();
        data.getChars(0, Math.min(data.length(), MQ_BUFFER_SIZE), mqBuffer, 0);

        if (mqConditionCode == MQCC_OK) {
            mqMsgId = md.msgId;
            mqCorrelId = md.correlId;
            mqQueueReply = md.replyToQ;
            recordCodes();
            requestMessage = new String(mqBuffer);
            saveCorelId = mqCorrelId;
            saveReply2Q = mqQueueReply;
            saveMsgId = mqMsgId;
            requestMsgCopy = AccountInquiryRequest.fromMessage(requestMessage);
            processRequestReply();
            mqMsgCount = (mqMsgCount + 1) % 1_000_000_000L;
        } else if (mqReasonCode == MQRC_NO_MSG_AVAILABLE) {
            noMoreMsgs = true;
        } else {
            recordCodes();
            mqErrDisplay.applQueueName(inputQueueName);
            mqErrDisplay.applReturnMessage("INP MQGET ERR:");
            error();
            termination();
        }
    }

    // 4000-PROCESS-REQUEST-REPLY
    private void processRequestReply() {
        replyMessage = CobolText.spaces(MQ_BUFFER_SIZE);
        // INITIALIZE WS-DATE-TIME: the date fields are never referenced afterwards.
        String wsFunc = requestMsgCopy.func();
        String wsKey = requestMsgCopy.keyText();
        if (wsFunc.equals(AccountInquiryRequest.FUNC_INQUIRE_ACCOUNT) && requestMsgCopy.keyValue() > 0) {
            wsCardRidAcctIdX = CobolText.digits(requestMsgCopy.keyValue(), 11);

            AccountReadResult read = accounts.read(LIT_ACCTFILENAME, wsCardRidAcctIdX);
            wsRespCd = read.resp();
            wsReasCd = read.resp2();

            if (wsRespCd == CicsResp.NORMAL) {
                wsAcctResponse = AccountInquiryResponse.from(read.record());
                replyMessage = CobolText.pic(wsAcctResponse.toMessage(), MQ_BUFFER_SIZE);
                putReply();
            } else if (wsRespCd == CicsResp.NOTFND) {
                replyMessage = CobolText.stringInto(replyMessage,
                        "INVALID REQUEST PARAMETERS ", "ACCT ID : ", wsKey);
                putReply();
            } else {
                mqErrDisplay.applConditionCode(wsRespCd);
                mqErrDisplay.applReasonCode(wsReasCd);
                mqErrDisplay.applQueueName(inputQueueName);
                mqErrDisplay.applReturnMessage("ERROR WHILE READING ACCTFILE");
                error();
                termination();
            }
        } else {
            replyMessage = CobolText.stringInto(replyMessage,
                    "INVALID REQUEST PARAMETERS ", "ACCT ID : ", wsKey, "FUNCTION : ", wsFunc);
            putReply();
        }
    }

    // 4100-PUT-REPLY
    private void putReply() {
        setBuffer(replyMessage);
        mqBufferLength = MQ_BUFFER_SIZE;
        md.msgId = saveMsgId;
        md.correlId = saveCorelId;
        md.format = MQFMT_STRING;
        md.codedCharSetId = MQCCSI_Q_MGR;
        pmo.options = MQPMO_SYNCPOINT + MQPMO_DEFAULT_CONTEXT + MQPMO_FAIL_IF_QUIESCING;

        MqCallResult r = mq.put(mqHconn, outputQueueHandle, md, pmo, mqBufferLength, new String(mqBuffer));
        setMqResult(r.conditionCode(), r.reasonCode());
        recordCodes();
        if (mqConditionCode != MQCC_OK) {
            mqErrDisplay.applQueueName(replyQueueName);
            mqErrDisplay.applReturnMessage("MQPUT ERR");
            error();
            termination();
        }
    }

    // 9000-ERROR
    private void error() {
        errorMessage = CobolText.pic(mqErrDisplay.render(), MQ_BUFFER_SIZE);
        setBuffer(errorMessage);
        mqBufferLength = MQ_BUFFER_SIZE;
        md.format = MQFMT_STRING;
        md.codedCharSetId = MQCCSI_Q_MGR;
        pmo.options = MQPMO_SYNCPOINT + MQPMO_DEFAULT_CONTEXT + MQPMO_FAIL_IF_QUIESCING;

        MqCallResult r = mq.put(mqHconn, errorQueueHandle, md, pmo, mqBufferLength, new String(mqBuffer));
        setMqResult(r.conditionCode(), r.reasonCode());
        recordCodes();
        if (mqConditionCode != MQCC_OK) {
            mqErrDisplay.applQueueName(errorQueueName);
            mqErrDisplay.applReturnMessage("MQPUT ERR");
            display.accept(mqErrDisplay.render());
            termination();
        }
    }

    // 8000-TERMINATION
    private void termination() {
        if (terminating) {
            // Recursive PERFORM of 8000 (from a failed close) is undefined in COBOL; end the task here.
            cics.returnToCics();
            throw new CicsReturn();
        }
        terminating = true;
        if (replyQueueOpen) {
            closeInputQueue();
        }
        if (respQueueOpen) {
            closeOutputQueue();
        }
        if (errQueueOpen) {
            closeErrorQueue();
        }
        cics.returnToCics();
        throw new CicsReturn();
    }

    // 5000-CLOSE-INPUT-QUEUE
    private void closeInputQueue() {
        mqQueue = inputQueueName;
        mqHobj = inputQueueHandle;
        mqOptions = MQCO_NONE;
        MqCallResult r = mq.close(mqHconn, mqHobj, mqOptions);
        setMqResult(r.conditionCode(), r.reasonCode());
        recordCodes();
        if (mqConditionCode != MQCC_OK) {
            mqErrDisplay.applQueueName(inputQueueName);
            mqErrDisplay.applReturnMessage("MQCLOSE ERR");
            termination();
        }
    }

    // 5100-CLOSE-OUTPUT-QUEUE
    private void closeOutputQueue() {
        mqQueue = replyQueueName;
        mqHobj = outputQueueHandle;
        mqOptions = MQCO_NONE;
        MqCallResult r = mq.close(mqHconn, mqHobj, mqOptions);
        setMqResult(r.conditionCode(), r.reasonCode());
        recordCodes();
        if (mqConditionCode != MQCC_OK) {
            // COBOL reports INPUT-QUEUE-NAME here, not REPLY-QUEUE-NAME.
            mqErrDisplay.applQueueName(inputQueueName);
            mqErrDisplay.applReturnMessage("MQCLOSE ERR");
            termination();
        }
    }

    // 5200-CLOSE-ERROR-QUEUE
    private void closeErrorQueue() {
        mqQueue = errorQueueName;
        mqHobj = errorQueueHandle;
        mqOptions = MQCO_NONE;
        MqCallResult r = mq.close(mqHconn, mqHobj, mqOptions);
        setMqResult(r.conditionCode(), r.reasonCode());
        recordCodes();
        if (mqConditionCode != MQCC_OK) {
            mqErrDisplay.applQueueName(errorQueueName);
            mqErrDisplay.applReturnMessage("MQCLOSE ERR");
            error();
            termination();
        }
    }

    private void setMqResult(int conditionCode, int reasonCode) {
        mqConditionCode = conditionCode;
        mqReasonCode = reasonCode;
    }

    /** MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE, MOVE MQ-REASON-CODE TO MQ-APPL-REASON-CODE. */
    private void recordCodes() {
        mqErrDisplay.applConditionCode(mqConditionCode);
        mqErrDisplay.applReasonCode(mqReasonCode);
    }

    private void setBuffer(String value) {
        CobolText.pic(value, MQ_BUFFER_SIZE).getChars(0, MQ_BUFFER_SIZE, mqBuffer, 0);
    }

    /** Unwinds the call stack on EXEC CICS RETURN. */
    private static final class CicsReturn extends RuntimeException {
        CicsReturn() {
            super(null, null, false, false);
        }
    }
}

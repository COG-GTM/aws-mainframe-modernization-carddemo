package com.carddemo.programs.copaua0c;

import com.carddemo.programs.copaua0c.copybook.AccountRecord;
import com.carddemo.programs.copaua0c.copybook.CardXrefRecord;
import com.carddemo.programs.copaua0c.copybook.CustomerRecord;
import com.carddemo.programs.copaua0c.copybook.ErrorLogRecord;
import com.carddemo.programs.copaua0c.copybook.ErrorLogRecord.Level;
import com.carddemo.programs.copaua0c.copybook.ErrorLogRecord.Subsystem;
import com.carddemo.programs.copaua0c.copybook.ImsReturnCode;
import com.carddemo.programs.copaua0c.copybook.MqConstants;
import com.carddemo.programs.copaua0c.copybook.MqGetMessageOptions;
import com.carddemo.programs.copaua0c.copybook.MqMessageDescriptor;
import com.carddemo.programs.copaua0c.copybook.MqObjectDescriptor;
import com.carddemo.programs.copaua0c.copybook.MqPutMessageOptions;
import com.carddemo.programs.copaua0c.copybook.MqTriggerMessage;
import com.carddemo.programs.copaua0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaua0c.copybook.PendingAuthRequest;
import com.carddemo.programs.copaua0c.copybook.PendingAuthResponse;
import com.carddemo.programs.copaua0c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaua0c.gateway.CicsClock;
import com.carddemo.programs.copaua0c.gateway.CicsKsdsFile;
import com.carddemo.programs.copaua0c.gateway.ImsGateway;
import com.carddemo.programs.copaua0c.gateway.MqGateway;
import com.carddemo.programs.copaua0c.gateway.Syncpoint;
import com.carddemo.programs.copaua0c.gateway.TransientDataQueue;
import com.carddemo.programs.copaua0c.support.Cobol;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Optional;

/**
 * COPAUA0C - Card Authorization Decision Program (CICS, MQ-triggered, IMS + VSAM).
 *
 * <p>Reads authorization requests from the trigger queue, decides approve/decline against the
 * pending-auth summary (or the account master), replies on the ReplyToQ and records the
 * authorization in IMS. One instance represents one CICS task: WORKING-STORAGE lives in
 * instance fields and deliberately persists across messages, exactly as in the COBOL program.
 */
public final class Copaua0cService {

    static final String WS_PGM_AUTH = "COPAUA0C";
    static final String WS_CICS_TRANID = "CP00";
    static final String PSB_NAME = "PSBPAUTB";
    static final String ERROR_QUEUE = "CSSL";
    static final int WS_REQSTS_PROCESS_LIMIT = 500;
    static final int GET_BUFFER_LENGTH = 500;
    static final int PUT_BUFFER_LENGTH = 200;
    static final int HCONN = 0;

    private final MqGateway mq;
    private final CicsKsdsFile<CardXrefRecord> ccxrefFile;
    private final CicsKsdsFile<AccountRecord> acctFile;
    private final CicsKsdsFile<CustomerRecord> custFile;
    private final ImsGateway ims;
    private final TransientDataQueue tdq;
    private final CicsClock clock;
    private final Syncpoint syncpoint;

    // WS-VARIABLES
    private int wsMsgProcessed;
    private String wsRequestQname = Cobol.spaces(48);
    private String wsReplyQname = Cobol.spaces(48);
    private String wsSaveCorrelid = Cobol.spaces(24);
    private int wsRespLength = 1;
    private int wsWaitInterval;
    private BigDecimal wsAvailableAmt = BigDecimal.ZERO.setScale(2);
    private String wsTransactionAmtAn = Cobol.spaces(13);
    private BigDecimal wsTransactionAmt = BigDecimal.ZERO.setScale(2);
    private BigDecimal wsApprovedAmt = BigDecimal.ZERO.setScale(2);
    private String wsTriggerData = Cobol.spaces(64);
    private long wsCardRidCustId;
    private long wsCardRidAcctId;

    // WS-IMS-VARIABLES
    private String imsReturnCode = "  ";
    private char wsImsPsbSchdFlg = ' ';

    // MQ handles / buffers
    private int w01HobjRequest;
    private int w01Datalen;
    private final char[] w01GetBuffer = Cobol.spaces(GET_BUFFER_LENGTH).toCharArray();
    private final char[] w02PutBuffer = Cobol.spaces(PUT_BUFFER_LENGTH).toCharArray();
    private final MqMessageDescriptor mqmMdRequest = new MqMessageDescriptor();

    // WS-SWITCHES
    private char wsAuthRespFlg = ' ';
    private boolean wsLoopEnd;
    private char wsMsgAvailableFlg = 'M';
    private char wsRequestMqFlg = 'C';
    private char wsXrefReadFlg = ' ';
    private char wsAcctMasterReadFlg = ' ';
    private char wsCustMasterReadFlg = ' ';
    private char wsPautSmrySegFlg = ' ';
    private char wsDeclineFlg = ' ';
    private char wsDeclineReasonFlg = ' ';

    // Copybook areas
    private final PendingAuthRequest request = new PendingAuthRequest();
    private final PendingAuthResponse response = new PendingAuthResponse();
    private final ErrorLogRecord errorLog = new ErrorLogRecord();
    private PendingAuthSummary summary = new PendingAuthSummary();
    private final PendingAuthDetails details = new PendingAuthDetails();
    private CardXrefRecord cardXref = new CardXrefRecord("", 0, 0);
    private String xrefCardNum = Cobol.spaces(16);
    private AccountRecord account = AccountRecord.fromFixed("");
    private CustomerRecord customer = CustomerRecord.fromFixed("");

    private boolean used;

    /** EXEC CICS RETURN issued from 9990-END-ROUTINE. */
    private static final class CicsReturn extends RuntimeException {
        private static final long serialVersionUID = 1L;

        CicsReturn() {
            super(null, null, false, false);
        }
    }

    public Copaua0cService(MqGateway mq, CicsKsdsFile<CardXrefRecord> ccxrefFile,
            CicsKsdsFile<AccountRecord> acctFile, CicsKsdsFile<CustomerRecord> custFile, ImsGateway ims,
            TransientDataQueue tdq, CicsClock clock, Syncpoint syncpoint) {
        this.mq = mq;
        this.ccxrefFile = ccxrefFile;
        this.acctFile = acctFile;
        this.custFile = custFile;
        this.ims = ims;
        this.tdq = tdq;
        this.clock = clock;
        this.syncpoint = syncpoint;
    }

    // MAIN-PARA
    public Copaua0cRunResult run(Optional<MqTriggerMessage> retrievedTrigger) {
        if (used) {
            throw new IllegalStateException("One Copaua0cService instance per CICS task");
        }
        used = true;
        try {
            initialize(retrievedTrigger);
            mainProcess();
            terminate();
        } catch (CicsReturn ret) {
            return new Copaua0cRunResult(wsMsgProcessed, true, wsLoopEnd);
        }
        return new Copaua0cRunResult(wsMsgProcessed, false, wsLoopEnd);
    }

    // 1000-INITIALIZE
    private void initialize(Optional<MqTriggerMessage> retrievedTrigger) {
        retrievedTrigger.ifPresent(mqtm -> {
            wsRequestQname = mqtm.qName();
            wsTriggerData = mqtm.triggerData();
        });
        wsWaitInterval = 5000;
        openRequestQueue();
        readRequestMq();
    }

    // 1100-OPEN-REQUEST-QUEUE
    private void openRequestQueue() {
        MqObjectDescriptor od = new MqObjectDescriptor(MqConstants.MQOT_Q, wsRequestQname);
        MqGateway.OpenResult r = mq.open(HCONN, od, MqConstants.MQOO_INPUT_SHARED);
        w01HobjRequest = r.hObj();
        if (r.compCode() == MqConstants.MQCC_OK) {
            wsRequestMqFlg = 'O';
        } else {
            errorLog.location("M001");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.MQ);
            errorLog.code1(Cobol.unsignedDisplay9(r.compCode()));
            errorLog.code2(Cobol.unsignedDisplay9(r.reason()));
            errorLog.message("REQ MQ OPEN ERROR");
            logError();
        }
    }

    // 1200-SCHEDULE-PSB
    private void schedulePsb() {
        imsReturnCode = ims.schedule(PSB_NAME);
        if (ImsReturnCode.isPsbScheduledMoreThanOnce(imsReturnCode)) {
            ims.terminate();
            imsReturnCode = ims.schedule(PSB_NAME);
        }
        if (ImsReturnCode.isStatusOk(imsReturnCode)) {
            wsImsPsbSchdFlg = 'Y';
        } else {
            errorLog.location("I001");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.IMS);
            errorLog.code1(imsReturnCode);
            errorLog.message("IMS SCHD FAILED");
            logError();
        }
    }

    // 2000-MAIN-PROCESS
    private void mainProcess() {
        while (!(wsMsgAvailableFlg == 'N' || wsLoopEnd)) {
            extractRequestMsg();
            processAuth();
            wsMsgProcessed = Cobol.truncateBinary(wsMsgProcessed + 1L, 4);
            syncpoint.syncpoint();
            wsImsPsbSchdFlg = 'N';
            if (wsMsgProcessed > WS_REQSTS_PROCESS_LIMIT) {
                wsLoopEnd = true;
            } else {
                readRequestMq();
            }
        }
    }

    // 2100-EXTRACT-REQUEST-MSG: UNSTRING W01-GET-BUFFER(1:W01-DATALEN) DELIMITED BY ','
    private void extractRequestMsg() {
        String data = new String(w01GetBuffer, 0, Math.min(Math.max(w01Datalen, 0), GET_BUFFER_LENGTH));
        String[] parts = unstring(data, 18);
        if (parts[0] != null) request.authDate(parts[0]);
        if (parts[1] != null) request.authTime(parts[1]);
        if (parts[2] != null) request.cardNum(parts[2]);
        if (parts[3] != null) request.authType(parts[3]);
        if (parts[4] != null) request.cardExpiryDate(parts[4]);
        if (parts[5] != null) request.messageType(parts[5]);
        if (parts[6] != null) request.messageSource(parts[6]);
        if (parts[7] != null) request.processingCode(parts[7]);
        if (parts[8] != null) wsTransactionAmtAn = Cobol.alpha(parts[8], 13);
        if (parts[9] != null) request.merchantCatagoryCode(parts[9]);
        if (parts[10] != null) request.acqrCountryCode(parts[10]);
        if (parts[11] != null) request.posEntryMode(parts[11]);
        if (parts[12] != null) request.merchantId(parts[12]);
        if (parts[13] != null) request.merchantName(parts[13]);
        if (parts[14] != null) request.merchantCity(parts[14]);
        if (parts[15] != null) request.merchantState(parts[15]);
        if (parts[16] != null) request.merchantZip(parts[16]);
        if (parts[17] != null) request.transactionId(parts[17]);

        request.transactionAmt(Cobol.numval(wsTransactionAmtAn));
        wsTransactionAmt = request.transactionAmt();
    }

    /**
     * UNSTRING semantics: one delimited segment per receiver; receivers beyond the last segment are
     * left unchanged (null); a trailing delimiter does not create an extra empty segment.
     */
    static String[] unstring(String data, int receivers) {
        String[] out = new String[receivers];
        int pos = 0;
        for (int i = 0; i < receivers && pos < data.length(); i++) {
            int idx = data.indexOf(',', pos);
            if (idx < 0) {
                out[i] = data.substring(pos);
                pos = data.length();
            } else {
                out[i] = data.substring(pos, idx);
                pos = idx + 1;
            }
        }
        return out;
    }

    // 3100-READ-REQUEST-MQ
    private void readRequestMq() {
        int options = MqConstants.MQGMO_NO_SYNCPOINT + MqConstants.MQGMO_WAIT
                + MqConstants.MQGMO_CONVERT + MqConstants.MQGMO_FAIL_IF_QUIESCING;
        MqGetMessageOptions gmo = new MqGetMessageOptions(options, wsWaitInterval);
        mqmMdRequest.msgId(MqConstants.MQMI_NONE);
        mqmMdRequest.correlId(MqConstants.MQCI_NONE);
        mqmMdRequest.format(MqConstants.MQFMT_STRING);

        MqGateway.GetResult r = mq.get(HCONN, w01HobjRequest, mqmMdRequest, gmo, GET_BUFFER_LENGTH);
        w01Datalen = r.dataLength();
        String data = r.data() == null ? "" : r.data();
        data.getChars(0, Math.min(data.length(), GET_BUFFER_LENGTH), w01GetBuffer, 0);

        if (r.compCode() == MqConstants.MQCC_OK) {
            wsSaveCorrelid = mqmMdRequest.correlId();
            wsReplyQname = mqmMdRequest.replyToQ();
        } else if (r.reason() == MqConstants.MQRC_NO_MSG_AVAILABLE) {
            wsMsgAvailableFlg = 'N';
        } else {
            errorLog.location("M003");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.CICS);
            errorLog.code1(Cobol.unsignedDisplay9(r.compCode()));
            errorLog.code2(Cobol.unsignedDisplay9(r.reason()));
            errorLog.message("FAILED TO READ REQUEST MQ");
            errorLog.eventKey(details.cardNum());
            logError();
        }
    }

    // 5000-PROCESS-AUTH
    private void processAuth() {
        wsDeclineFlg = 'A';
        schedulePsb();
        wsXrefReadFlg = 'Y';
        wsAcctMasterReadFlg = 'Y';
        readXrefRecord();
        if (wsXrefReadFlg == 'Y') {
            readAcctRecord();
            readCustRecord();
            readAuthSummry();
            readProfileData();
        }
        makeDecision();
        sendResponse();
        if (wsXrefReadFlg == 'Y') {
            writeAuthToDb();
        }
    }

    // 5100-READ-XREF-RECORD
    private void readXrefRecord() {
        xrefCardNum = request.cardNum();
        CicsKsdsFile.ReadResult<CardXrefRecord> r = ccxrefFile.read(xrefCardNum);
        if (r.resp() == CicsKsdsFile.RESP_NORMAL) {
            cardXref = r.record();
            wsXrefReadFlg = 'Y';
        } else if (r.resp() == CicsKsdsFile.RESP_NOTFND) {
            wsXrefReadFlg = 'N';
            wsAcctMasterReadFlg = 'N';
            errorLog.location("A001");
            errorLog.level(Level.WARNING);
            errorLog.subsystem(Subsystem.APP);
            errorLog.message("CARD NOT FOUND IN XREF");
            errorLog.eventKey(xrefCardNum);
            logError();
        } else {
            errorLog.location("C001");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.CICS);
            errorLog.code1(Cobol.unsignedDisplay9(r.resp()));
            errorLog.code2(Cobol.unsignedDisplay9(r.resp2()));
            errorLog.message("FAILED TO READ XREF FILE");
            errorLog.eventKey(xrefCardNum);
            logError();
        }
    }

    // 5200-READ-ACCT-RECORD
    private void readAcctRecord() {
        wsCardRidAcctId = cardXref.acctId();
        String key = String.format("%011d", wsCardRidAcctId);
        CicsKsdsFile.ReadResult<AccountRecord> r = acctFile.read(key);
        if (r.resp() == CicsKsdsFile.RESP_NORMAL) {
            account = r.record();
            wsAcctMasterReadFlg = 'Y';
        } else if (r.resp() == CicsKsdsFile.RESP_NOTFND) {
            wsAcctMasterReadFlg = 'N';
            errorLog.location("A002");
            errorLog.level(Level.WARNING);
            errorLog.subsystem(Subsystem.APP);
            errorLog.message("ACCT NOT FOUND IN XREF");
            errorLog.eventKey(key);
            logError();
        } else {
            errorLog.location("C002");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.CICS);
            errorLog.code1(Cobol.unsignedDisplay9(r.resp()));
            errorLog.code2(Cobol.unsignedDisplay9(r.resp2()));
            errorLog.message("FAILED TO READ ACCT FILE");
            errorLog.eventKey(key);
            logError();
        }
    }

    // 5300-READ-CUST-RECORD
    private void readCustRecord() {
        wsCardRidCustId = cardXref.custId();
        String key = String.format("%09d", wsCardRidCustId);
        CicsKsdsFile.ReadResult<CustomerRecord> r = custFile.read(key);
        if (r.resp() == CicsKsdsFile.RESP_NORMAL) {
            customer = r.record();
            wsCustMasterReadFlg = 'Y';
        } else if (r.resp() == CicsKsdsFile.RESP_NOTFND) {
            wsCustMasterReadFlg = 'N';
            errorLog.location("A003");
            errorLog.level(Level.WARNING);
            errorLog.subsystem(Subsystem.APP);
            errorLog.message("CUST NOT FOUND IN XREF");
            errorLog.eventKey(key);
            logError();
        } else {
            errorLog.location("C003");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.CICS);
            errorLog.code1(Cobol.unsignedDisplay9(r.resp()));
            errorLog.code2(Cobol.unsignedDisplay9(r.resp2()));
            errorLog.message("FAILED TO READ CUST FILE");
            errorLog.eventKey(key);
            logError();
        }
    }

    // 5500-READ-AUTH-SUMMRY
    private void readAuthSummry() {
        summary.acctId(cardXref.acctId());
        ImsGateway.GetResult r = ims.getUniqueSummary(summary.acctId());
        imsReturnCode = r.status();
        if (ImsReturnCode.isStatusOk(imsReturnCode)) {
            summary = r.segment().copy();
            wsPautSmrySegFlg = 'Y';
        } else if (ImsReturnCode.isSegmentNotFound(imsReturnCode)) {
            wsPautSmrySegFlg = 'N';
        } else {
            errorLog.location("I002");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.IMS);
            errorLog.code1(imsReturnCode);
            errorLog.message("IMS GET SUMMARY FAILED");
            errorLog.eventKey(details.cardNum());
            logError();
        }
    }

    // 5600-READ-PROFILE-DATA
    private void readProfileData() {
        // CONTINUE
    }

    // 6000-MAKE-DECISION
    private void makeDecision() {
        response.cardNum(request.cardNum());
        response.transactionId(request.transactionId());
        response.authIdCode(request.authTime());

        if (wsPautSmrySegFlg == 'Y') {
            wsAvailableAmt = Cobol.truncate(summary.creditLimit().subtract(summary.creditBalance()), 9, 2);
            if (wsTransactionAmt.compareTo(wsAvailableAmt) > 0) {
                wsDeclineFlg = 'D';
                wsDeclineReasonFlg = 'I';
            }
        } else if (wsAcctMasterReadFlg == 'Y') {
            wsAvailableAmt = Cobol.truncate(account.creditLimit().subtract(account.currBal()), 9, 2);
            if (wsTransactionAmt.compareTo(wsAvailableAmt) > 0) {
                wsDeclineFlg = 'D';
                wsDeclineReasonFlg = 'I';
            }
        } else {
            wsDeclineFlg = 'D';
        }

        if (wsDeclineFlg == 'D') {
            wsAuthRespFlg = 'D';
            response.authRespCode("05");
            response.approvedAmt(BigDecimal.ZERO);
            wsApprovedAmt = BigDecimal.ZERO.setScale(2);
        } else {
            wsAuthRespFlg = 'A';
            response.authRespCode("00");
            response.approvedAmtEdited(request.transactionAmtEdited());
            wsApprovedAmt = request.transactionAmt();
        }

        response.authRespReason("0000");
        if (wsAuthRespFlg == 'D') {
            if (wsXrefReadFlg == 'N' || wsAcctMasterReadFlg == 'N' || wsCustMasterReadFlg == 'N') {
                response.authRespReason("3100");
            } else if (wsDeclineReasonFlg == 'I') {
                response.authRespReason("4100");
            } else if (wsDeclineReasonFlg == 'A') {
                response.authRespReason("4200");
            } else if (wsDeclineReasonFlg == 'C') {
                response.authRespReason("4300");
            } else if (wsDeclineReasonFlg == 'F') {
                response.authRespReason("5100");
            } else if (wsDeclineReasonFlg == 'M') {
                response.authRespReason("5200");
            } else {
                response.authRespReason("9000");
            }
        }

        String approvedAmtDis = Cobol.editMinusZ9v2(wsApprovedAmt);
        stringIntoPutBuffer(response.cardNum(), ",", response.transactionId(), ",", response.authIdCode(), ",",
                response.authRespCode(), ",", response.authRespReason(), ",", approvedAmtDis, ",");
    }

    /**
     * STRING ... DELIMITED BY SIZE INTO W02-PUT-BUFFER WITH POINTER WS-RESP-LENGTH.
     * WS-RESP-LENGTH is never reset, so successive replies are appended after earlier ones;
     * on overflow the transfer stops and the pointer is left unchanged.
     */
    private void stringIntoPutBuffer(String... parts) {
        if (wsRespLength < 1 || wsRespLength > PUT_BUFFER_LENGTH) {
            return;
        }
        for (String part : parts) {
            for (int i = 0; i < part.length(); i++) {
                if (wsRespLength > PUT_BUFFER_LENGTH) {
                    return;
                }
                w02PutBuffer[wsRespLength - 1] = part.charAt(i);
                wsRespLength++;
            }
        }
    }

    // 7100-SEND-RESPONSE
    private void sendResponse() {
        MqObjectDescriptor od = new MqObjectDescriptor(MqConstants.MQOT_Q, wsReplyQname);
        MqMessageDescriptor md = new MqMessageDescriptor();
        md.msgType(MqConstants.MQMT_REPLY);
        md.correlId(wsSaveCorrelid);
        md.msgId(MqConstants.MQMI_NONE);
        md.replyToQ("");
        md.replyToQMgr("");
        md.persistence(MqConstants.MQPER_NOT_PERSISTENT);
        md.expiry(50);
        md.format(MqConstants.MQFMT_STRING);
        MqPutMessageOptions pmo =
                new MqPutMessageOptions(MqConstants.MQPMO_NO_SYNCPOINT + MqConstants.MQPMO_DEFAULT_CONTEXT);
        int bufflen = wsRespLength;
        String payload = new String(Arrays.copyOf(w02PutBuffer, Math.min(bufflen, PUT_BUFFER_LENGTH)));

        MqGateway.CallResult r = mq.put1(HCONN, od, md, pmo, bufflen, payload);
        if (r.compCode() != MqConstants.MQCC_OK) {
            errorLog.location("M004");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.MQ);
            errorLog.code1(Cobol.unsignedDisplay9(r.compCode()));
            errorLog.code2(Cobol.unsignedDisplay9(r.reason()));
            errorLog.message("FAILED TO PUT ON REPLY MQ");
            errorLog.eventKey(details.cardNum());
            logError();
        }
    }

    // 8000-WRITE-AUTH-TO-DB
    private void writeAuthToDb() {
        updateSummary();
        insertAuth();
    }

    // 8400-UPDATE-SUMMARY
    private void updateSummary() {
        if (wsPautSmrySegFlg == 'N') {
            summary.initializeNumeric();
            summary.acctId(cardXref.acctId());
            summary.custId(cardXref.custId());
        }
        summary.creditLimit(account.creditLimit());
        summary.cashLimit(account.cashCreditLimit());

        if (wsAuthRespFlg == 'A') {
            summary.approvedAuthCnt(summary.approvedAuthCnt() + 1L);
            summary.approvedAuthAmt(summary.approvedAuthAmt().add(wsApprovedAmt));
            summary.creditBalance(summary.creditBalance().add(wsApprovedAmt));
            summary.cashBalance(BigDecimal.ZERO);
        } else {
            summary.declinedAuthCnt(summary.declinedAuthCnt() + 1L);
            // PA-TRANSACTION-AMT of PENDING-AUTH-DETAILS: still the previous message's amount here.
            summary.declinedAuthAmt(summary.declinedAuthAmt().add(details.transactionAmt()));
        }

        if (wsPautSmrySegFlg == 'Y') {
            imsReturnCode = ims.replaceSummary(summary);
        } else {
            imsReturnCode = ims.insertSummary(summary);
        }
        if (!ImsReturnCode.isStatusOk(imsReturnCode)) {
            errorLog.location("I003");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.IMS);
            errorLog.code1(imsReturnCode);
            errorLog.message("IMS UPDATE SUMRY FAILED");
            errorLog.eventKey(details.cardNum());
            logError();
        }
    }

    // 8500-INSERT-AUTH
    private void insertAuth() {
        CicsClock.FormattedTime t = clock.now();
        long yyddd = Cobol.unsigned(Cobol.numericDisplay(Cobol.alpha(t.yyddd(), 5), 5));
        long curTimeN6 = Cobol.unsigned(Cobol.numericDisplay(t.hhmmss(), 6));
        long timeWithMs = (curTimeN6 * 1000 + t.milliseconds()) % 1_000_000_000L;

        details.authDate9c(99999 - yyddd);
        details.authTime9c(999999999L - timeWithMs);

        details.authOrigDate(request.authDate());
        details.authOrigTime(request.authTime());
        details.cardNum(request.cardNum());
        details.authType(request.authType());
        details.cardExpiryDate(request.cardExpiryDate());
        details.messageType(request.messageType());
        details.messageSource(request.messageSource());
        details.processingCode(request.processingCode());
        details.transactionAmt(request.transactionAmt());
        details.merchantCatagoryCode(request.merchantCatagoryCode());
        details.acqrCountryCode(request.acqrCountryCode());
        details.posEntryMode(request.posEntryMode());
        details.merchantId(request.merchantId());
        details.merchantName(request.merchantName());
        details.merchantCity(request.merchantCity());
        details.merchantState(request.merchantState());
        details.merchantZip(request.merchantZip());
        details.transactionId(request.transactionId());

        details.authIdCode(response.authIdCode());
        details.authRespCode(response.authRespCode());
        details.authRespReason(response.authRespReason());
        details.approvedAmt(response.approvedAmt());

        if (wsAuthRespFlg == 'A') {
            details.matchStatus(PendingAuthDetails.MatchStatus.PENDING);
        } else {
            details.matchStatus(PendingAuthDetails.MatchStatus.AUTH_DECLINED);
        }
        details.authFraud(' ');
        details.fraudRptDate("");

        summary.acctId(cardXref.acctId());
        imsReturnCode = ims.insertDetail(summary.acctId(), details);
        if (!ImsReturnCode.isStatusOk(imsReturnCode)) {
            errorLog.location("I004");
            errorLog.level(Level.CRITICAL);
            errorLog.subsystem(Subsystem.IMS);
            errorLog.code1(imsReturnCode);
            errorLog.message("IMS INSERT DETL FAILED");
            errorLog.eventKey(details.cardNum());
            logError();
        }
    }

    // 9000-TERMINATE
    private void terminate() {
        if (wsImsPsbSchdFlg == 'Y') {
            ims.terminate();
        }
        closeRequestQueue();
    }

    // 9100-CLOSE-REQUEST-QUEUE
    private void closeRequestQueue() {
        if (wsRequestMqFlg == 'O') {
            MqGateway.CallResult r = mq.close(HCONN, w01HobjRequest, MqConstants.MQCO_NONE);
            if (r.compCode() == MqConstants.MQCC_OK) {
                wsRequestMqFlg = 'C';
            } else {
                errorLog.location("M005");
                errorLog.level(Level.WARNING);
                errorLog.subsystem(Subsystem.MQ);
                errorLog.code1(Cobol.unsignedDisplay9(r.compCode()));
                errorLog.code2(Cobol.unsignedDisplay9(r.reason()));
                errorLog.message("FAILED TO CLOSE REQUEST MQ");
                logError();
            }
        }
    }

    // 9500-LOG-ERROR
    private void logError() {
        CicsClock.FormattedTime t = clock.now();
        errorLog.application(WS_CICS_TRANID);
        errorLog.program(WS_PGM_AUTH);
        errorLog.date(t.yymmdd());
        errorLog.time(t.hhmmss());
        tdq.writeq(ERROR_QUEUE, errorLog);
        if (errorLog.isCritical()) {
            endRoutine();
        }
    }

    // 9990-END-ROUTINE
    private void endRoutine() {
        terminate();
        throw new CicsReturn();
    }

    String triggerData() {
        return wsTriggerData;
    }

    CustomerRecord lastCustomer() {
        return customer;
    }
}

package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.AidKey;
import com.carddemo.programs.copaus1c.copybook.AuthorizationKey;
import com.carddemo.programs.copaus1c.copybook.BmsColor;
import com.carddemo.programs.copaus1c.copybook.Cobol;
import com.carddemo.programs.copaus1c.copybook.CommonMessages;
import com.carddemo.programs.copaus1c.copybook.Copau1aMap;
import com.carddemo.programs.copaus1c.copybook.Copau1aMap.Field;
import com.carddemo.programs.copaus1c.copybook.DateTimeWorkArea;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus1c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaus1c.copybook.ScreenTitles;
import com.carddemo.programs.copaus1c.gateway.DliResult;
import com.carddemo.programs.copaus1c.gateway.FraudLinkGateway;
import com.carddemo.programs.copaus1c.gateway.PendingAuthDbGateway;
import com.carddemo.programs.copaus1c.gateway.SyncpointGateway;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * COPAUS1C - CardDemo Authorization Module: detail view of a pending authorization (transaction CPVD,
 * map COPAU1A / mapset COPAU01). Each {@link #execute} call is one pseudo-conversational CICS task with
 * fresh WORKING-STORAGE; paragraphs are methods of the per-task {@link Task}.
 */
public final class Copaus1cService {

    public static final String WS_PGM_AUTH_DTL = "COPAUS1C";
    public static final String WS_PGM_AUTH_SMRY = "COPAUS0C";
    public static final String WS_PGM_AUTH_FRAUD = "COPAUS2C";
    public static final String WS_CICS_TRANID = "CPVD";
    public static final String PSB_NAME = "PSBPAUTB";

    private final PendingAuthDbGateway ims;
    private final SyncpointGateway syncpoints;
    private final FraudLinkGateway fraudLink;
    private final Clock clock;

    public Copaus1cService(PendingAuthDbGateway ims, SyncpointGateway syncpoints, FraudLinkGateway fraudLink,
                           Clock clock) {
        this.ims = Objects.requireNonNull(ims);
        this.syncpoints = Objects.requireNonNull(syncpoints);
        this.fraudLink = Objects.requireNonNull(fraudLink);
        this.clock = Objects.requireNonNull(clock);
    }

    public AuthViewResult execute(AuthViewRequest request) {
        return new Task(request).run();
    }

    /** PIC -zzzzzzz9.99 edit of an S9(10)V99 value (high-order digits beyond 8 are truncated). */
    static String editAmount(BigDecimal amount) {
        BigInteger cents = amount.setScale(2, RoundingMode.DOWN).unscaledValue();
        boolean negative = cents.signum() < 0;
        String digits = cents.abs().mod(BigInteger.TEN.pow(10)).toString();
        digits = "0".repeat(10 - digits.length()) + digits;
        String intPart = digits.substring(0, 8);
        String decPart = digits.substring(8);
        String stripped = intPart.replaceFirst("^0{1,7}", "");
        String zsuppressed = " ".repeat(8 - stripped.length()) + stripped;
        return (negative ? "-" : " ") + zsuppressed + "." + decPart;
    }

    /** Thrown to leave the task on EXEC CICS XCTL, which never returns to the caller. */
    private static final class XctlTransfer extends RuntimeException {
        private final String program;

        XctlTransfer(String program) {
            super(null, null, false, false);
            this.program = program;
        }
    }

    /** One CICS task: WORKING-STORAGE fields plus the PROCEDURE DIVISION paragraphs. */
    private final class Task {
        // WS-VARIABLES
        private final char[] wsMessage = Cobol.spaces(80).toCharArray();      // WS-MESSAGE X(80)
        private char wsErrFlg = 'N';                                           // WS-ERR-FLG
        private char wsAuthsEof = 'N';                                         // WS-AUTHS-EOF
        private char wsSendEraseFlg = 'Y';                                     // WS-SEND-ERASE-FLG
        private long wsAcctId;                                                 // WS-ACCT-ID  9(11)
        private AuthorizationKey wsAuthKey = AuthorizationKey.LOW_VALUES;      // WS-AUTH-KEY X(08)
        private String wsAuthAmt = Cobol.spaces(12);                           // WS-AUTH-AMT -zzzzzzz9.99
        private String wsAuthDate = "00/00/00";                                // WS-AUTH-DATE X(08)
        private String wsAuthTime = "00:00:00";                                // WS-AUTH-TIME X(08)
        // WS-IMS-VARIABLES
        private String imsReturnCode = Cobol.spaces(2);                        // IMS-RETURN-CODE
        private char wsImsPsbSchdFlg = Cobol.LOW_VALUE;                        // WS-IMS-PSB-SCHD-FLG
        // WS-FRAUD-DATA
        private final FraudCommarea wsFraudData = new FraudCommarea();

        private final Copaus1cCommarea commarea = new Copaus1cCommarea();     // CARDDEMO-COMMAREA
        private final Copau1aMap copau1a = Copau1aMap.lowValues();             // COPAU1AI / COPAU1AO
        private final DateTimeWorkArea dateTime = new DateTimeWorkArea();      // CSDAT01Y
        @SuppressWarnings("unused")
        private PendingAuthSummary pendingAuthSummary = PendingAuthSummary.of(0, 0);
        private PendingAuthDetails pendingAuthDetails = PendingAuthDetails.lowValues();

        private final AuthViewRequest request;
        private final List<AuthViewScreen> screens = new ArrayList<>();
        private final List<String> displays = new ArrayList<>();

        Task(AuthViewRequest request) {
            this.request = request;
        }

        AuthViewResult run() {
            try {
                mainPara();
            } catch (XctlTransfer x) {
                return new AuthViewResult(List.copyOf(screens), new AuthViewResult.Xctl(x.program),
                        commarea.copy(), List.copyOf(displays));
            } catch (RuntimeException abend) {
                // A CICS task that abends has its uncommitted recoverable updates backed out.
                syncpoints.rollback();
                throw abend;
            }
            // EXEC CICS RETURN TRANSID(WS-CICS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
            return new AuthViewResult(List.copyOf(screens), new AuthViewResult.ReturnTransid(WS_CICS_TRANID),
                    commarea.copy(), List.copyOf(displays));
        }

        // MAIN-PARA
        private void mainPara() {
            wsErrFlg = 'N';
            wsSendEraseFlg = 'Y';
            moveMessage("");
            copau1a.move(Field.ERRMSG, "");

            if (request.commarea() == null) {
                commarea.initialize();
                commarea.setToProgram(WS_PGM_AUTH_SMRY);
                returnToPrevScreen();
            } else {
                Copaus1cCommarea in = request.commarea().copy();
                commarea.copyFrom(in);
                commarea.cpvd().copyFrom(in.cpvd());
                commarea.cpvd().setFraudData("");
                if (!commarea.isPgmReenter()) {
                    commarea.setPgmContext(Copaus1cCommarea.PgmContext.CDEMO_PGM_REENTER);
                    processEnterKey();
                    sendAuthviewScreen();
                } else {
                    receiveAuthviewScreen();
                    AidKey aid = request.aid();
                    if (aid == AidKey.DFHENTER) {
                        processEnterKey();
                        sendAuthviewScreen();
                    } else if (aid == AidKey.DFHPF3) {
                        commarea.setToProgram(WS_PGM_AUTH_SMRY);
                        returnToPrevScreen();
                    } else if (aid == AidKey.DFHPF5) {
                        markAuthFraud();
                        sendAuthviewScreen();
                    } else if (aid == AidKey.DFHPF8) {
                        processPf8Key();
                        sendAuthviewScreen();
                    } else {
                        processEnterKey();
                        moveMessage(CommonMessages.CCDA_MSG_INVALID_KEY);
                        sendAuthviewScreen();
                    }
                }
            }
        }

        // PROCESS-ENTER-KEY
        private void processEnterKey() {
            copau1a.moveLowValues();
            AuthorizationKey selected = commarea.cpvd().getPauSelected();
            if (Cobol.isNumeric(commarea.getAcctId()) && !selected.isSpaces() && !selected.isLowValues()) {
                wsAcctId = Cobol.displayValue(commarea.getAcctId());
                wsAuthKey = selected;
                readAuthRecord();
                if (wsImsPsbSchdFlg == 'Y') {
                    wsImsPsbSchdFlg = 'N';
                    takeSyncpoint();
                }
            } else {
                wsErrFlg = 'Y';
            }
            populateAuthDetails();
        }

        // MARK-AUTH-FRAUD
        private void markAuthFraud() {
            wsAcctId = Cobol.displayValue(commarea.getAcctId());
            wsAuthKey = commarea.cpvd().getPauSelected();

            readAuthRecord();

            if (pendingAuthDetails.isFraudConfirmed()) {
                pendingAuthDetails.setFraudStatus(PendingAuthDetails.FraudStatus.PA_FRAUD_REMOVED);
                wsFraudData.setAction(FraudCommarea.Action.WS_REMOVE_FRAUD);
            } else {
                pendingAuthDetails.setFraudStatus(PendingAuthDetails.FraudStatus.PA_FRAUD_CONFIRMED);
                wsFraudData.setAction(FraudCommarea.Action.WS_REPORT_FRAUD);
            }

            wsFraudData.setFraudAuthRecord(pendingAuthDetails);
            wsFraudData.setAcctId(commarea.getAcctId());
            wsFraudData.setCustId(commarea.getCustId());

            boolean normal = fraudLink.link(WS_PGM_AUTH_FRAUD, wsFraudData);
            if (normal) {
                if (wsFraudData.isUpdateSuccess()) {
                    updateAuthDetails();
                } else {
                    moveMessage(wsFraudData.getActMsg());
                    rollBack();
                }
            } else {
                rollBack();
            }

            commarea.cpvd().setPauSelected(pendingAuthDetails.getAuthorizationKey());
            populateAuthDetails();
        }

        // PROCESS-PF8-KEY
        private void processPf8Key() {
            wsAcctId = Cobol.displayValue(commarea.getAcctId());
            wsAuthKey = commarea.cpvd().getPauSelected();

            readAuthRecord();
            readNextAuthRecord();

            if (wsImsPsbSchdFlg == 'Y') {
                wsImsPsbSchdFlg = 'N';
                takeSyncpoint();
            }

            if (wsAuthsEof == 'Y') {
                wsSendEraseFlg = 'N';
                moveMessage("Already at the last Authorization...");
            } else {
                commarea.cpvd().setPauSelected(pendingAuthDetails.getAuthorizationKey());
                populateAuthDetails();
            }
        }

        // POPULATE-AUTH-DETAILS
        private void populateAuthDetails() {
            if (wsErrFlg != 'N') {
                return;
            }
            PendingAuthDetails pa = pendingAuthDetails;
            copau1a.move(Field.CARDNUM, pa.getCardNum());

            String origDate = pa.getAuthOrigDate();
            dateTime.setCurdateYy(origDate.substring(0, 2));
            dateTime.setCurdateMm(origDate.substring(2, 4));
            dateTime.setCurdateDd(origDate.substring(4, 6));
            wsAuthDate = dateTime.getCurdateMmDdYy();
            copau1a.move(Field.AUTHDT, wsAuthDate);

            String origTime = pa.getAuthOrigTime();
            wsAuthTime = Cobol.overlay(wsAuthTime, 1, origTime.substring(0, 2));
            wsAuthTime = Cobol.overlay(wsAuthTime, 4, origTime.substring(2, 4));
            wsAuthTime = Cobol.overlay(wsAuthTime, 7, origTime.substring(4, 6));
            copau1a.move(Field.AUTHTM, wsAuthTime);

            wsAuthAmt = editAmount(pa.getApprovedAmt());
            copau1a.move(Field.AUTHAMT, wsAuthAmt);

            if ("00".equals(pa.getAuthRespCode())) {
                copau1a.move(Field.AUTHRSP, "A");
                copau1a.setColor(Field.AUTHRSP, BmsColor.DFHGREEN);
            } else {
                copau1a.move(Field.AUTHRSP, "D");
                copau1a.setColor(Field.AUTHRSP, BmsColor.DFHRED);
            }

            DeclineReasonTable.searchAll(pa.getAuthRespReason()).ifPresentOrElse(e -> {
                copau1a.move(Field.AUTHRSN, pa.getAuthRespReason());
                copau1a.moveRef(Field.AUTHRSN, 5, 1, "-");
                copau1a.moveRef(Field.AUTHRSN, 6, null, e.declDesc());
            }, () -> {
                copau1a.move(Field.AUTHRSN, "9999");
                copau1a.moveRef(Field.AUTHRSN, 5, 1, "-");
                copau1a.moveRef(Field.AUTHRSN, 6, null, "ERROR");
            });

            copau1a.move(Field.AUTHCD, pa.getProcessingCodeDisplay());
            copau1a.move(Field.POSEMD, pa.getPosEntryModeDisplay());
            copau1a.move(Field.AUTHSRC, pa.getMessageSource());
            copau1a.move(Field.MCCCD, pa.getMerchantCategoryCode());

            copau1a.moveRef(Field.CRDEXP, 1, 2, pa.getCardExpiryDate().substring(0, 2));
            copau1a.moveRef(Field.CRDEXP, 3, 1, "/");
            copau1a.moveRef(Field.CRDEXP, 4, 2, pa.getCardExpiryDate().substring(2, 4));

            copau1a.move(Field.AUTHTYP, pa.getAuthType());
            copau1a.move(Field.TRNID, pa.getTransactionId());
            copau1a.move(Field.AUTHMTC, String.valueOf(pa.getMatchStatus()));

            if (pa.isFraudConfirmed() || pa.isFraudRemoved()) {
                copau1a.moveRef(Field.AUTHFRD, 1, 1, String.valueOf(pa.getAuthFraud()));
                copau1a.moveRef(Field.AUTHFRD, 2, 1, "-");
                copau1a.moveRef(Field.AUTHFRD, 3, null, pa.getFraudRptDate());
            } else {
                copau1a.move(Field.AUTHFRD, "-");
            }

            copau1a.move(Field.MERNAME, pa.getMerchantName());
            copau1a.move(Field.MERID, pa.getMerchantId());
            copau1a.move(Field.MERCITY, pa.getMerchantCity());
            copau1a.move(Field.MERST, pa.getMerchantState());
            copau1a.move(Field.MERZIP, pa.getMerchantZip());
        }

        // RETURN-TO-PREV-SCREEN
        private void returnToPrevScreen() {
            commarea.setFromTranid(WS_CICS_TRANID);
            commarea.setFromProgram(WS_PGM_AUTH_DTL);
            commarea.setPgmContext(0);
            commarea.setPgmContext(Copaus1cCommarea.PgmContext.CDEMO_PGM_ENTER);
            throw new XctlTransfer(commarea.getToProgram());
        }

        // SEND-AUTHVIEW-SCREEN
        private void sendAuthviewScreen() {
            populateHeaderInfo();
            copau1a.move(Field.ERRMSG, new String(wsMessage));
            copau1a.setLength(Field.CARDNUM, -1);
            screens.add(AuthViewScreen.from(copau1a, wsSendEraseFlg == 'Y'));
        }

        // RECEIVE-AUTHVIEW-SCREEN
        private void receiveAuthviewScreen() {
            // EXEC CICS RECEIVE MAP('COPAU1A') NOHANDLE: every COPAU1A field is ASKIP, so nothing is read.
        }

        // POPULATE-HEADER-INFO
        private void populateHeaderInfo() {
            dateTime.moveCurrentDate(LocalDateTime.now(clock));
            copau1a.move(Field.TITLE01, ScreenTitles.CCDA_TITLE01);
            copau1a.move(Field.TITLE02, ScreenTitles.CCDA_TITLE02);
            copau1a.move(Field.TRNNAME, WS_CICS_TRANID);
            copau1a.move(Field.PGMNAME, WS_PGM_AUTH_DTL);

            dateTime.setCurdateMm(dateTime.getCurdateMonth());
            dateTime.setCurdateDd(dateTime.getCurdateDay());
            dateTime.setCurdateYy(dateTime.getCurdateYear().substring(2, 4));
            copau1a.move(Field.CURDATE, dateTime.getCurdateMmDdYy());

            dateTime.setCurtimeHh(dateTime.getCurtimeHours());
            dateTime.setCurtimeMm(dateTime.getCurtimeMinute());
            dateTime.setCurtimeSs(dateTime.getCurtimeSecond());
            copau1a.move(Field.CURTIME, dateTime.getCurtimeHhMmSs());
        }

        // READ-AUTH-RECORD
        private void readAuthRecord() {
            schedulePsb();
            pendingAuthSummary.setAcctId(wsAcctId % 100_000_000_000L);
            pendingAuthDetails.setAuthorizationKey(wsAuthKey);

            DliResult<PendingAuthSummary> gu = ims.getUniqueSummary(wsAcctId % 100_000_000_000L);
            imsReturnCode = gu.dibstat();
            if (gu.segment() != null) {
                pendingAuthSummary = gu.segment().copy();
            }
            if (ImsStatus.isStatusOk(imsReturnCode)) {
                wsAuthsEof = 'N';
            } else if (ImsStatus.isSegmentNotFound(imsReturnCode) || ImsStatus.isEndOfDb(imsReturnCode)) {
                wsAuthsEof = 'Y';
            } else {
                wsErrFlg = 'Y';
                stringIntoMessage(" System error while reading Auth Summary: Code:" + imsReturnCode);
                sendAuthviewScreen();
            }

            if (wsAuthsEof == 'N') {
                DliResult<PendingAuthDetails> gnp = ims.getNextDetailWhere(wsAuthKey);
                imsReturnCode = gnp.dibstat();
                if (gnp.segment() != null) {
                    pendingAuthDetails = gnp.segment().copy();
                }
                if (ImsStatus.isStatusOk(imsReturnCode)) {
                    wsAuthsEof = 'N';
                } else if (ImsStatus.isSegmentNotFound(imsReturnCode) || ImsStatus.isEndOfDb(imsReturnCode)) {
                    wsAuthsEof = 'Y';
                } else {
                    wsErrFlg = 'Y';
                    stringIntoMessage(" System error while reading Auth Details: Code:" + imsReturnCode);
                    sendAuthviewScreen();
                }
            }
        }

        // READ-NEXT-AUTH-RECORD
        private void readNextAuthRecord() {
            DliResult<PendingAuthDetails> gnp = ims.getNextDetail();
            imsReturnCode = gnp.dibstat();
            if (gnp.segment() != null) {
                pendingAuthDetails = gnp.segment().copy();
            }
            if (ImsStatus.isStatusOk(imsReturnCode)) {
                wsAuthsEof = 'N';
            } else if (ImsStatus.isSegmentNotFound(imsReturnCode) || ImsStatus.isEndOfDb(imsReturnCode)) {
                wsAuthsEof = 'Y';
            } else {
                wsErrFlg = 'Y';
                stringIntoMessage(" System error while reading next Auth: Code:" + imsReturnCode);
                sendAuthviewScreen();
            }
        }

        // UPDATE-AUTH-DETAILS
        private void updateAuthDetails() {
            pendingAuthDetails = wsFraudData.getFraudAuthRecord().copy();
            displays.add("RPT DT: " + pendingAuthDetails.getFraudRptDate());

            imsReturnCode = ims.replaceDetail(pendingAuthDetails.copy());
            if (ImsStatus.isStatusOk(imsReturnCode)) {
                takeSyncpoint();
                if (pendingAuthDetails.isFraudRemoved()) {
                    moveMessage("AUTH FRAUD REMOVED...");
                } else {
                    moveMessage("AUTH MARKED FRAUD...");
                }
            } else {
                rollBack();
                wsErrFlg = 'Y';
                stringIntoMessage(" System error while FRAUD Tagging, ROLLBACK||" + imsReturnCode);
                sendAuthviewScreen();
            }
        }

        // TAKE-SYNCPOINT
        private void takeSyncpoint() {
            syncpoints.syncpoint();
        }

        // ROLL-BACK
        private void rollBack() {
            syncpoints.rollback();
        }

        // SCHEDULE-PSB
        private void schedulePsb() {
            imsReturnCode = ims.schedulePsb(PSB_NAME);
            if (ImsStatus.isPsbScheduledMoreThanOnce(imsReturnCode)) {
                ims.terminate();
                imsReturnCode = ims.schedulePsb(PSB_NAME);
            }
            if (ImsStatus.isStatusOk(imsReturnCode)) {
                wsImsPsbSchdFlg = 'Y';
            } else {
                wsErrFlg = 'Y';
                stringIntoMessage(" System error while scheduling PSB: Code:" + imsReturnCode);
                sendAuthviewScreen();
            }
        }

        /** MOVE x TO WS-MESSAGE. */
        private void moveMessage(String text) {
            char[] v = Cobol.x(text, 80).toCharArray();
            System.arraycopy(v, 0, wsMessage, 0, 80);
        }

        /** STRING ... DELIMITED BY SIZE INTO WS-MESSAGE: overwrites from position 1, the rest is unchanged. */
        private void stringIntoMessage(String text) {
            for (int i = 0; i < text.length() && i < 80; i++) {
                wsMessage[i] = text.charAt(i);
            }
        }
    }
}

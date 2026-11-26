package com.carddemo.programs.cotrn02c;

import com.carddemo.programs.cotrn02c.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cotrn02c.copybook.CommonMessages;
import com.carddemo.programs.cotrn02c.copybook.Cotrn2aMap;
import com.carddemo.programs.cotrn02c.copybook.Cotrn2aMap.Field;
import com.carddemo.programs.cotrn02c.copybook.CsutldtcParm;
import com.carddemo.programs.cotrn02c.copybook.DateTimeFields;
import com.carddemo.programs.cotrn02c.copybook.Dfhaid;
import com.carddemo.programs.cotrn02c.copybook.Dfhbmsca;
import com.carddemo.programs.cotrn02c.copybook.Pic;
import com.carddemo.programs.cotrn02c.copybook.ScreenTitles;
import com.carddemo.programs.cotrn02c.copybook.TranRecord;
import com.carddemo.programs.cotrn02c.gateway.CardXrefGateway;
import com.carddemo.programs.cotrn02c.gateway.CicsResp;
import com.carddemo.programs.cotrn02c.gateway.DateValidator;
import com.carddemo.programs.cotrn02c.gateway.TransactFileGateway;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * COTRN02C (transaction CT02): add a new transaction to the TRANSACT file.
 *
 * <p>Each call to {@link #execute} is one pseudo-conversational CICS task with fresh working storage.
 * {@code SEND-TRNADD-SCREEN} and {@code RETURN-TO-PREV-SCREEN} end the task (EXEC CICS RETURN / XCTL),
 * so the first validation failure stops all further processing, exactly as in the COBOL.
 */
public final class Cotrn02cService {

    public static final String WS_PGMNAME = "COTRN02C";
    public static final String WS_TRANID = "CT02";
    public static final String WS_TRANSACT_FILE = "TRANSACT";
    public static final String WS_ACCTDAT_FILE = "ACCTDAT ";
    public static final String WS_CCXREF_FILE = "CCXREF  ";
    public static final String WS_CXACAIX_FILE = "CXACAIX ";
    public static final String WS_DATE_FORMAT = "YYYY-MM-DD";
    public static final String MAPSET = "COTRN02";
    public static final String MAP = "COTRN2A";

    private final TransactFileGateway transact;
    private final CardXrefGateway xref;
    private final DateValidator csutldtc;
    private final Clock clock;
    private final Consumer<String> display;

    public Cotrn02cService(TransactFileGateway transact, CardXrefGateway xref, DateValidator csutldtc,
                           Clock clock, Consumer<String> display) {
        this.transact = Objects.requireNonNull(transact);
        this.xref = Objects.requireNonNull(xref);
        this.csutldtc = Objects.requireNonNull(csutldtc);
        this.clock = Objects.requireNonNull(clock);
        this.display = Objects.requireNonNull(display);
    }

    public Cotrn02cService(TransactFileGateway transact, CardXrefGateway xref, DateValidator csutldtc) {
        this(transact, xref, csutldtc, Clock.systemDefaultZone(), System.out::println);
    }

    /**
     * Run one task.
     *
     * @param dfhcommarea the COMMAREA passed in ({@code null} or empty means EIBCALEN = 0)
     * @param eibaid      the attention key (EIBAID)
     * @param received    the data RECEIVE MAP would return (used only on re-entry)
     */
    public CicsOutcome execute(String dfhcommarea, Dfhaid eibaid, Cotrn2aRequest received) {
        Task task = new Task(dfhcommarea == null ? "" : dfhcommarea, eibaid,
                received == null ? Cotrn2aRequest.EMPTY : received);
        try {
            task.mainPara();
            throw new IllegalStateException("MAIN-PARA fell through without RETURN");
        } catch (TaskEnd end) {
            return end.outcome;
        }
    }

    /** EXEC CICS RETURN / XCTL: unwinds the PERFORM stack. */
    private static final class TaskEnd extends RuntimeException {
        private final transient CicsOutcome outcome;

        TaskEnd(CicsOutcome outcome) {
            super(null, null, false, false);
            this.outcome = outcome;
        }
    }

    /** Working storage + procedure division of one task. */
    private final class Task {
        private final String dfhcommarea;
        private final Dfhaid eibaid;
        private final Cotrn2aRequest received;

        // 01 WS-VARIABLES
        private String wsMessage = Pic.spaces(80);
        private char wsErrFlg = 'N';
        private int wsRespCd;
        private int wsReasCd;
        private char wsUsrModified = 'N';
        private long wsAcctIdN;
        private BigInteger wsCardNumN = BigInteger.ZERO;
        private BigInteger wsTranIdN = BigInteger.ZERO;
        private BigDecimal wsTranAmtN = new BigDecimal("0.00");
        private String wsTranAmtE = "+00000000.00";

        private CardDemoCommarea commarea = CardDemoCommarea.initial();
        private final Cotrn2aMap map = new Cotrn2aMap();
        private TranRecord tranRecord = new TranRecord();
        private CardXrefRecord cardXrefRecord = new CardXrefRecord("", 0, 0);

        Task(String dfhcommarea, Dfhaid eibaid, Cotrn2aRequest received) {
            this.dfhcommarea = dfhcommarea;
            this.eibaid = eibaid;
            this.received = received;
        }

        private boolean errFlgOn() {
            return wsErrFlg == 'Y';
        }

        private void error(String message, Field cursor) {
            wsErrFlg = 'Y';
            wsMessage = Pic.x(message, 80);
            map.cursor(cursor);
            sendTrnaddScreen();
        }

        // MAIN-PARA
        void mainPara() {
            wsErrFlg = 'N';
            wsUsrModified = 'N';
            wsMessage = Pic.spaces(80);
            map.f(Field.ERRMSG).spaces();

            if (dfhcommarea.isEmpty()) {
                commarea.setToProgram("COSGN00C");
                returnToPrevScreen();
            } else {
                commarea = CardDemoCommarea.fromRecord(dfhcommarea);
                if (!commarea.isPgmReenter()) {
                    commarea.setPgmReenter();
                    map.lowValues();
                    map.cursor(Field.ACTIDIN);
                    String selected = commarea.getCt02TrnSelected();
                    if (!Pic.isSpaces(selected) && !Pic.isLowValues(selected)) {
                        map.set(Field.CARDNIN, selected);
                        processEnterKey();
                    }
                    sendTrnaddScreen();
                } else {
                    receiveTrnaddScreen();
                    switch (eibaid) {
                        case DFHENTER -> processEnterKey();
                        case DFHPF3 -> {
                            if (Pic.isSpacesOrLowValues(commarea.getFromProgram())) {
                                commarea.setToProgram("COMEN01C");
                            } else {
                                commarea.setToProgram(commarea.getFromProgram());
                            }
                            returnToPrevScreen();
                        }
                        case DFHPF4 -> clearCurrentScreen();
                        case DFHPF5 -> copyLastTranData();
                        default -> {
                            wsErrFlg = 'Y';
                            wsMessage = Pic.x(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                            sendTrnaddScreen();
                        }
                    }
                }
            }
            throw new TaskEnd(new CicsOutcome.Return(WS_TRANID, commarea.copy()));
        }

        // PROCESS-ENTER-KEY
        void processEnterKey() {
            validateInputKeyFields();
            validateInputDataFields();

            switch (map.get(Field.CONFIRM)) {
                case "Y", "y" -> addTransaction();
                case "N", "n", " ", "\u0000" -> error("Confirm to add this transaction...", Field.CONFIRM);
                default -> error("Invalid value. Valid values are (Y/N)...", Field.CONFIRM);
            }
        }

        // VALIDATE-INPUT-KEY-FIELDS
        void validateInputKeyFields() {
            String acctIdIn = map.get(Field.ACTIDIN);
            String cardNumIn = map.get(Field.CARDNIN);
            if (!Pic.isSpaces(acctIdIn) && !Pic.isLowValues(acctIdIn)) {
                if (!Pic.isNumeric(acctIdIn)) {
                    error("Account ID must be Numeric...", Field.ACTIDIN);
                }
                wsAcctIdN = Long.parseLong(Pic.nine(Pic.numval(acctIdIn).toBigInteger(), 11));
                cardXrefRecord = new CardXrefRecord(cardXrefRecord.cardNum(), cardXrefRecord.custId(), wsAcctIdN,
                        cardXrefRecord.filler());
                readCxacaixFile();
                map.set(Field.CARDNIN, cardXrefRecord.cardNum());
            } else if (!Pic.isSpaces(cardNumIn) && !Pic.isLowValues(cardNumIn)) {
                if (!Pic.isNumeric(cardNumIn)) {
                    error("Card Number must be Numeric...", Field.CARDNIN);
                }
                wsCardNumN = new BigInteger(Pic.nine(Pic.numval(cardNumIn).toBigInteger(), 16));
                String cardImage = Pic.nine(wsCardNumN, 16);
                cardXrefRecord = new CardXrefRecord(cardImage, cardXrefRecord.custId(), cardXrefRecord.acctId(),
                        cardXrefRecord.filler());
                map.set(Field.CARDNIN, cardImage);
                readCcxrefFile();
                map.set(Field.ACTIDIN, cardXrefRecord.acctIdImage());
            } else {
                error("Account or Card Number must be entered...", Field.ACTIDIN);
            }
        }

        // VALIDATE-INPUT-DATA-FIELDS
        void validateInputDataFields() {
            if (errFlgOn()) {
                for (Field f : new Field[] {Field.TTYPCD, Field.TCATCD, Field.TRNSRC, Field.TRNAMT, Field.TDESC,
                        Field.TORIGDT, Field.TPROCDT, Field.MID, Field.MNAME, Field.MCITY, Field.MZIP}) {
                    map.f(f).spaces();
                }
            }

            if (empty(Field.TTYPCD)) {
                error("Type CD can NOT be empty...", Field.TTYPCD);
            } else if (empty(Field.TCATCD)) {
                error("Category CD can NOT be empty...", Field.TCATCD);
            } else if (empty(Field.TRNSRC)) {
                error("Source can NOT be empty...", Field.TRNSRC);
            } else if (empty(Field.TDESC)) {
                error("Description can NOT be empty...", Field.TDESC);
            } else if (empty(Field.TRNAMT)) {
                error("Amount can NOT be empty...", Field.TRNAMT);
            } else if (empty(Field.TORIGDT)) {
                error("Orig Date can NOT be empty...", Field.TORIGDT);
            } else if (empty(Field.TPROCDT)) {
                error("Proc Date can NOT be empty...", Field.TPROCDT);
            } else if (empty(Field.MID)) {
                error("Merchant ID can NOT be empty...", Field.MID);
            } else if (empty(Field.MNAME)) {
                error("Merchant Name can NOT be empty...", Field.MNAME);
            } else if (empty(Field.MCITY)) {
                error("Merchant City can NOT be empty...", Field.MCITY);
            } else if (empty(Field.MZIP)) {
                error("Merchant Zip can NOT be empty...", Field.MZIP);
            }

            if (!Pic.isNumeric(map.get(Field.TTYPCD))) {
                error("Type CD must be Numeric...", Field.TTYPCD);
            } else if (!Pic.isNumeric(map.get(Field.TCATCD))) {
                error("Category CD must be Numeric...", Field.TCATCD);
            }

            String amt = map.get(Field.TRNAMT);
            if ((amt.charAt(0) != '-' && amt.charAt(0) != '+')
                    || !Pic.isNumeric(amt.substring(1, 9))
                    || amt.charAt(9) != '.'
                    || !Pic.isNumeric(amt.substring(10, 12))) {
                error("Amount should be in format -99999999.99", Field.TRNAMT);
            }

            if (!dateFormatOk(map.get(Field.TORIGDT))) {
                error("Orig Date should be in format YYYY-MM-DD", Field.TORIGDT);
            }
            if (!dateFormatOk(map.get(Field.TPROCDT))) {
                error("Proc Date should be in format YYYY-MM-DD", Field.TPROCDT);
            }

            wsTranAmtN = Pic.fit(Pic.numvalC(map.get(Field.TRNAMT)), 9, 2);
            wsTranAmtE = Pic.signedEdited(wsTranAmtN, 8, 2);
            map.set(Field.TRNAMT, wsTranAmtE);

            CsutldtcParm orig = csutldtc.validate(map.get(Field.TORIGDT), WS_DATE_FORMAT);
            if (!orig.resultSevCd().equals("0000") && !orig.resultMsgNum().equals("2513")) {
                error("Orig Date - Not a valid date...", Field.TORIGDT);
            }

            CsutldtcParm proc = csutldtc.validate(map.get(Field.TPROCDT), WS_DATE_FORMAT);
            if (!proc.resultSevCd().equals("0000") && !proc.resultMsgNum().equals("2513")) {
                error("Proc Date - Not a valid date...", Field.TPROCDT);
            }

            if (!Pic.isNumeric(map.get(Field.MID))) {
                error("Merchant ID must be Numeric...", Field.MID);
            }
        }

        private boolean empty(Field f) {
            return Pic.isSpacesOrLowValues(map.get(f));
        }

        private boolean dateFormatOk(String d) {
            return Pic.isNumeric(d.substring(0, 4)) && d.charAt(4) == '-'
                    && Pic.isNumeric(d.substring(5, 7)) && d.charAt(7) == '-'
                    && Pic.isNumeric(d.substring(8, 10));
        }

        // ADD-TRANSACTION
        void addTransaction() {
            tranRecord.setTranId(Pic.highValues(16));
            startbrTransactFile();
            readprevTransactFile();
            endbrTransactFile();
            wsTranIdN = Pic.alphaToUnsigned(tranRecord.getTranId()).mod(BigInteger.TEN.pow(16));
            wsTranIdN = wsTranIdN.add(BigInteger.ONE).mod(BigInteger.TEN.pow(16));
            tranRecord.initialize();
            tranRecord.setTranId(Pic.nine(wsTranIdN, 16));
            tranRecord.setTypeCd(map.get(Field.TTYPCD));
            tranRecord.setCatCd(Pic.alphaToUnsigned(map.get(Field.TCATCD)).intValue());
            tranRecord.setSource(map.get(Field.TRNSRC));
            tranRecord.setDesc(map.get(Field.TDESC));
            wsTranAmtN = Pic.fit(Pic.numvalC(map.get(Field.TRNAMT)), 9, 2);
            tranRecord.setAmt(wsTranAmtN);
            tranRecord.setCardNum(map.get(Field.CARDNIN));
            tranRecord.setMerchantId(Pic.alphaToUnsigned(map.get(Field.MID)).longValue());
            tranRecord.setMerchantName(map.get(Field.MNAME));
            tranRecord.setMerchantCity(map.get(Field.MCITY));
            tranRecord.setMerchantZip(map.get(Field.MZIP));
            tranRecord.setOrigTs(map.get(Field.TORIGDT));
            tranRecord.setProcTs(map.get(Field.TPROCDT));
            writeTransactFile();
        }

        // COPY-LAST-TRAN-DATA
        void copyLastTranData() {
            validateInputKeyFields();

            tranRecord.setTranId(Pic.highValues(16));
            startbrTransactFile();
            readprevTransactFile();
            endbrTransactFile();

            if (!errFlgOn()) {
                wsTranAmtE = Pic.signedEdited(tranRecord.getAmt(), 8, 2);
                map.set(Field.TTYPCD, tranRecord.getTypeCd());
                map.set(Field.TCATCD, tranRecord.catCdImage());
                map.set(Field.TRNSRC, tranRecord.getSource());
                map.set(Field.TRNAMT, wsTranAmtE);
                map.set(Field.TDESC, tranRecord.getDesc());
                map.set(Field.TORIGDT, tranRecord.getOrigTs());
                map.set(Field.TPROCDT, tranRecord.getProcTs());
                map.set(Field.MID, tranRecord.merchantIdImage());
                map.set(Field.MNAME, tranRecord.getMerchantName());
                map.set(Field.MCITY, tranRecord.getMerchantCity());
                map.set(Field.MZIP, tranRecord.getMerchantZip());
            }

            processEnterKey();
        }

        // RETURN-TO-PREV-SCREEN
        void returnToPrevScreen() {
            if (Pic.isSpacesOrLowValues(commarea.getToProgram())) {
                commarea.setToProgram("COSGN00C");
            }
            commarea.setFromTranId(WS_TRANID);
            commarea.setFromProgram(WS_PGMNAME);
            commarea.setPgmContext(0);
            throw new TaskEnd(new CicsOutcome.Xctl(commarea.getToProgram(), commarea.copy()));
        }

        // SEND-TRNADD-SCREEN
        void sendTrnaddScreen() {
            populateHeaderInfo();
            map.set(Field.ERRMSG, wsMessage);
            throw new TaskEnd(new CicsOutcome.SendMap(MAPSET, MAP, Cotrn2aResponse.from(map), WS_TRANID,
                    commarea.copy()));
        }

        // RECEIVE-TRNADD-SCREEN
        void receiveTrnaddScreen() {
            Cotrn2aRequest r = received;
            map.lowValues();
            map.receive(Field.ACTIDIN, r.acctId());
            map.receive(Field.CARDNIN, r.cardNum());
            map.receive(Field.TTYPCD, r.typeCd());
            map.receive(Field.TCATCD, r.catCd());
            map.receive(Field.TRNSRC, r.source());
            map.receive(Field.TDESC, r.desc());
            map.receive(Field.TRNAMT, r.amount());
            map.receive(Field.TORIGDT, r.origDate());
            map.receive(Field.TPROCDT, r.procDate());
            map.receive(Field.MID, r.merchantId());
            map.receive(Field.MNAME, r.merchantName());
            map.receive(Field.MCITY, r.merchantCity());
            map.receive(Field.MZIP, r.merchantZip());
            map.receive(Field.CONFIRM, r.confirm());
            wsRespCd = CicsResp.NORMAL.code;
            wsReasCd = 0;
        }

        // POPULATE-HEADER-INFO
        void populateHeaderInfo() {
            DateTimeFields dt = DateTimeFields.currentDate(LocalDateTime.now(clock));
            map.set(Field.TITLE01, ScreenTitles.CCDA_TITLE01);
            map.set(Field.TITLE02, ScreenTitles.CCDA_TITLE02);
            map.set(Field.TRNNAME, WS_TRANID);
            map.set(Field.PGMNAME, WS_PGMNAME);
            map.set(Field.CURDATE, dt.curdateMmDdYy());
            map.set(Field.CURTIME, dt.curtimeHhMmSs());
        }

        // READ-CXACAIX-FILE
        void readCxacaixFile() {
            CardXrefGateway.ReadResult r = xref.readByAcctId(cardXrefRecord.acctId());
            setResp(r.resp(), r.resp2());
            if (r.record() != null) {
                cardXrefRecord = r.record();
            }
            switch (r.resp()) {
                case NORMAL -> { }
                case NOTFND -> error("Account ID NOT found...", Field.ACTIDIN);
                default -> {
                    displayResp();
                    error("Unable to lookup Acct in XREF AIX file...", Field.ACTIDIN);
                }
            }
        }

        // READ-CCXREF-FILE
        void readCcxrefFile() {
            CardXrefGateway.ReadResult r = xref.readByCardNum(cardXrefRecord.cardNum());
            setResp(r.resp(), r.resp2());
            if (r.record() != null) {
                cardXrefRecord = r.record();
            }
            switch (r.resp()) {
                case NORMAL -> { }
                case NOTFND -> error("Card Number NOT found...", Field.CARDNIN);
                default -> {
                    displayResp();
                    error("Unable to lookup Card # in XREF file...", Field.CARDNIN);
                }
            }
        }

        // STARTBR-TRANSACT-FILE
        void startbrTransactFile() {
            TransactFileGateway.Result r = transact.startBrowse(tranRecord.getTranId());
            setResp(r.resp(), r.resp2());
            switch (r.resp()) {
                case NORMAL -> { }
                case NOTFND -> error("Transaction ID NOT found...", Field.ACTIDIN);
                default -> {
                    displayResp();
                    error("Unable to lookup Transaction...", Field.ACTIDIN);
                }
            }
        }

        // READPREV-TRANSACT-FILE
        void readprevTransactFile() {
            TransactFileGateway.Result r = transact.readPrev();
            setResp(r.resp(), r.resp2());
            switch (r.resp()) {
                case NORMAL -> tranRecord = r.record().copy();
                case ENDFILE -> tranRecord.setTranId(Pic.zeros(16));
                default -> {
                    displayResp();
                    error("Unable to lookup Transaction...", Field.ACTIDIN);
                }
            }
        }

        // ENDBR-TRANSACT-FILE
        void endbrTransactFile() {
            transact.endBrowse();
        }

        // WRITE-TRANSACT-FILE
        void writeTransactFile() {
            TransactFileGateway.Result r = transact.write(tranRecord);
            setResp(r.resp(), r.resp2());
            switch (r.resp()) {
                case NORMAL -> {
                    initializeAllFields();
                    wsMessage = Pic.spaces(80);
                    map.f(Field.ERRMSG).setColor(Dfhbmsca.DFHGREEN.code);
                    String tranId = tranRecord.getTranId();
                    int sp = tranId.indexOf(' ');
                    String delimited = sp < 0 ? tranId : tranId.substring(0, sp);
                    wsMessage = Pic.x("Transaction added successfully. " + " Your Tran ID is " + delimited + ".", 80);
                    sendTrnaddScreen();
                }
                case DUPKEY, DUPREC -> error("Tran ID already exist...", Field.ACTIDIN);
                default -> {
                    displayResp();
                    error("Unable to Add Transaction...", Field.ACTIDIN);
                }
            }
        }

        // CLEAR-CURRENT-SCREEN
        void clearCurrentScreen() {
            initializeAllFields();
            sendTrnaddScreen();
        }

        // INITIALIZE-ALL-FIELDS
        void initializeAllFields() {
            map.cursor(Field.ACTIDIN);
            for (Field f : new Field[] {Field.ACTIDIN, Field.CARDNIN, Field.TTYPCD, Field.TCATCD, Field.TRNSRC,
                    Field.TRNAMT, Field.TDESC, Field.TORIGDT, Field.TPROCDT, Field.MID, Field.MNAME, Field.MCITY,
                    Field.MZIP, Field.CONFIRM}) {
                map.f(f).spaces();
            }
            wsMessage = Pic.spaces(80);
        }

        private void setResp(CicsResp resp, int resp2) {
            wsRespCd = resp.code;
            wsReasCd = resp2;
        }

        // DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
        private void displayResp() {
            display.accept(String.format("RESP:%09dREAS:%09d", wsRespCd, wsReasCd));
        }
    }
}

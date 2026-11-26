package com.carddemo.programs.cobil00c;

import com.carddemo.programs.cobil00c.copybook.AccountRecord;
import com.carddemo.programs.cobil00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cobil00c.copybook.CardXrefRecord;
import com.carddemo.programs.cobil00c.copybook.Cobil00Map;
import com.carddemo.programs.cobil00c.copybook.Cobol;
import com.carddemo.programs.cobil00c.copybook.CommonMessages;
import com.carddemo.programs.cobil00c.copybook.DateTimeFields;
import com.carddemo.programs.cobil00c.copybook.Dfhaid;
import com.carddemo.programs.cobil00c.copybook.Dfhbmsca;
import com.carddemo.programs.cobil00c.copybook.ScreenTitle;
import com.carddemo.programs.cobil00c.copybook.TranRecord;
import com.carddemo.programs.cobil00c.gateway.AccountDataGateway;
import com.carddemo.programs.cobil00c.gateway.CardXrefAixGateway;
import com.carddemo.programs.cobil00c.gateway.CicsAbendException;
import com.carddemo.programs.cobil00c.gateway.CicsResp;
import com.carddemo.programs.cobil00c.gateway.CicsResponse;
import com.carddemo.programs.cobil00c.gateway.TransactGateway;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * COBIL00C - CardDemo online bill payment (transaction CB00). Pays the account's current balance
 * in full by writing a bill-payment transaction to TRANSACT and zeroing ACCT-CURR-BAL in ACCTDAT.
 *
 * <p>Each call to {@link #execute(BillPayRequest)} is one pseudo-conversational CICS task. The
 * paragraph structure of the COBOL source is preserved as methods on {@link Task}; like the
 * original, a paragraph that issues SEND MAP does not stop the flow, so a task can send the
 * screen several times and later statements still run.
 */
public final class Cobil00c {

    // 01 WS-VARIABLES constants
    public static final String WS_PGMNAME = "COBIL00C";
    public static final String WS_TRANID = "CB00";
    public static final String WS_TRANSACT_FILE = "TRANSACT";
    public static final String WS_ACCTDAT_FILE = "ACCTDAT ";
    public static final String WS_CXACAIX_FILE = "CXACAIX ";

    // Literal messages (exact COBOL text)
    public static final String MSG_ACCT_EMPTY = "Acct ID can NOT be empty...";
    public static final String MSG_INVALID_CONFIRM = "Invalid value. Valid values are (Y/N)...";
    public static final String MSG_NOTHING_TO_PAY = "You have nothing to pay...";
    public static final String MSG_CONFIRM = "Confirm to make a bill payment...";
    public static final String MSG_ACCT_NOT_FOUND = "Account ID NOT found...";
    public static final String MSG_ACCT_LOOKUP = "Unable to lookup Account...";
    public static final String MSG_ACCT_UPDATE = "Unable to Update Account...";
    public static final String MSG_XREF_LOOKUP = "Unable to lookup XREF AIX file...";
    public static final String MSG_TRAN_NOT_FOUND = "Transaction ID NOT found...";
    public static final String MSG_TRAN_LOOKUP = "Unable to lookup Transaction...";
    public static final String MSG_TRAN_EXISTS = "Tran ID already exist...";
    public static final String MSG_TRAN_ADD = "Unable to Add Bill pay Transaction...";

    private static final DateTimeFormatter FMT_DATE = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    private static final DateTimeFormatter FMT_TIME = DateTimeFormatter.ofPattern("HH:mm:ss");

    private final AccountDataGateway acctdat;
    private final CardXrefAixGateway cxacaix;
    private final TransactGateway transact;
    private final Clock clock;

    public Cobil00c(AccountDataGateway acctdat, CardXrefAixGateway cxacaix, TransactGateway transact, Clock clock) {
        this.acctdat = acctdat;
        this.cxacaix = cxacaix;
        this.transact = transact;
        this.clock = clock;
    }

    public BillPayResult execute(BillPayRequest request) {
        Task task = new Task(request);
        try {
            task.mainPara();
        } catch (XctlSignal x) {
            return new BillPayResult(BillPayResult.Action.XCTL, x.program, null, task.commarea,
                    List.copyOf(task.screens), List.copyOf(task.displays));
        } catch (CicsAbendException abend) {
            throw abend.withDisplayLines(task.displays);
        }
        return new BillPayResult(BillPayResult.Action.RETURN, null, WS_TRANID, task.commarea,
                List.copyOf(task.screens), List.copyOf(task.displays));
    }

    /** EXEC CICS XCTL: control never returns to COBIL00C. */
    private static final class XctlSignal extends RuntimeException {
        private final String program;

        XctlSignal(String program) {
            super(null, null, false, false);
            this.program = program;
        }
    }

    /** Working storage and procedure division for one task. */
    private final class Task {
        private final BillPayRequest request;
        private final List<BillPayScreen> screens = new ArrayList<>();
        private final List<String> displays = new ArrayList<>();

        // 01 WS-VARIABLES
        private String wsMessage = Cobol.spaces(80);   // PIC X(80)
        private boolean errFlgOn;                       // WS-ERR-FLG 88 ERR-FLG-ON/OFF
        private boolean usrModifiedYes;                 // WS-USR-MODIFIED 88 USR-MODIFIED-YES/NO
        private boolean confPayYes;                     // WS-CONF-PAY-FLG 88 CONF-PAY-YES/NO
        private int wsRespCd;                           // PIC S9(09) COMP
        private int wsReasCd;                           // PIC S9(09) COMP
        private String wsCurrBal;                       // PIC +9999999999.99
        private long wsTranIdNum;                       // PIC 9(16)

        private CardDemoCommarea commarea = new CardDemoCommarea();
        private final Cobil00Map map = new Cobil00Map();
        private final DateTimeFields dateTime = new DateTimeFields();

        // CVACT01Y / CVACT03Y / CVTRA05Y record areas (no VALUE clauses: modelled as zero/spaces)
        private AccountRecord accountRecord = new AccountRecord(0, "", BigDecimal.ZERO, BigDecimal.ZERO,
                BigDecimal.ZERO, "", "", "", BigDecimal.ZERO, BigDecimal.ZERO, "", "");
        private String acctIdKey = Cobol.spaces(AccountRecord.ACCT_ID_LENGTH);       // ACCT-ID bytes
        private CardXrefRecord cardXrefRecord = new CardXrefRecord("", 0, 0);
        private String xrefAcctIdKey = Cobol.spaces(CardXrefRecord.ACCT_ID_LENGTH);  // XREF-ACCT-ID bytes
        private TranRecord tranRecord = TranRecord.initialized();
        private String tranIdKey = Cobol.spaces(TranRecord.TRAN_ID_LENGTH);           // TRAN-ID bytes (RIDFLD)

        Task(BillPayRequest request) {
            this.request = request;
        }

        // MAIN-PARA
        void mainPara() {
            errFlgOn = false;
            usrModifiedYes = false;
            wsMessage = Cobol.spaces(80);
            map.errmsg.spaces();

            if (request.eibcalen() == 0) {
                commarea.toProgram("COSGN00C");
                returnToPrevScreen();
            } else {
                commarea = CardDemoCommarea.fromBytes(request.dfhcommarea(), request.eibcalen());
                if (!commarea.isPgmReenter()) {
                    commarea.setPgmReenter();
                    map.lowValues();
                    map.actidin.length(-1);
                    String selected = commarea.cb00TrnSelected();
                    if (!Cobol.isSpaces(selected) && !Cobol.isLowValues(selected)) {
                        map.actidin.data(selected);
                        processEnterKey();
                    }
                    sendBillpayScreen();
                } else {
                    receiveBillpayScreen();
                    Dfhaid aid = request.eibaid();
                    if (aid == Dfhaid.DFHENTER) {
                        processEnterKey();
                    } else if (aid == Dfhaid.DFHPF3) {
                        if (Cobol.isSpacesOrLowValues(commarea.fromProgram())) {
                            commarea.toProgram("COMEN01C");
                        } else {
                            commarea.toProgram(commarea.fromProgram());
                        }
                        returnToPrevScreen();
                    } else if (aid == Dfhaid.DFHPF4) {
                        clearCurrentScreen();
                    } else {
                        errFlgOn = true;
                        moveMessage(CommonMessages.CCDA_MSG_INVALID_KEY);
                        sendBillpayScreen();
                    }
                }
            }
            // EXEC CICS RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
        }

        // PROCESS-ENTER-KEY
        void processEnterKey() {
            confPayYes = false;

            if (Cobol.isSpacesOrLowValues(map.actidin.data())) {
                errFlgOn = true;
                moveMessage(MSG_ACCT_EMPTY);
                map.actidin.length(-1);
                sendBillpayScreen();
            }

            if (!errFlgOn) {
                acctIdKey = map.actidin.data();
                xrefAcctIdKey = map.actidin.data();

                char confirm = map.confirm.data().charAt(0);
                switch (confirm) {
                    case 'Y', 'y' -> {
                        confPayYes = true;
                        readAcctdatFile();
                    }
                    case 'N', 'n' -> {
                        clearCurrentScreen();
                        errFlgOn = true;
                    }
                    case ' ', Cobol.LOW_VALUE -> readAcctdatFile();
                    default -> {
                        errFlgOn = true;
                        moveMessage(MSG_INVALID_CONFIRM);
                        map.confirm.length(-1);
                        sendBillpayScreen();
                    }
                }

                wsCurrBal = Cobol.editSignedFixed(accountRecord.acctCurrBal(), 10);
                map.curbal.data(wsCurrBal);
            }

            if (!errFlgOn) {
                if (accountRecord.acctCurrBal().signum() <= 0
                        && !Cobol.isSpaces(map.actidin.data()) && !Cobol.isLowValues(map.actidin.data())) {
                    errFlgOn = true;
                    moveMessage(MSG_NOTHING_TO_PAY);
                    map.actidin.length(-1);
                    sendBillpayScreen();
                }
            }

            if (!errFlgOn) {
                if (confPayYes) {
                    readCxacaixFile();
                    tranIdKey = Cobol.highValues(TranRecord.TRAN_ID_LENGTH);
                    startbrTransactFile();
                    readprevTransactFile();
                    endbrTransactFile();
                    try {
                        wsTranIdNum = Cobol.zonedValue(tranIdKey);
                    } catch (NumberFormatException e) {
                        throw new CicsAbendException("ASRA", "data exception moving TRAN-ID to WS-TRAN-ID-NUM: "
                                + e.getMessage());
                    }
                    wsTranIdNum = (wsTranIdNum + 1) % 10_000_000_000_000_000L;
                    tranIdKey = Cobol.unsignedDigits(wsTranIdNum, 16);
                    tranRecord = new TranRecord(
                            tranIdKey,
                            "02",
                            2,
                            "POS TERM",
                            "BILL PAYMENT - ONLINE",
                            accountRecord.acctCurrBal(),
                            999999999L,
                            "BILL PAYMENT",
                            "N/A",
                            "N/A",
                            cardXrefRecord.xrefCardNum(),
                            "",
                            "");
                    getCurrentTimestamp();
                    tranRecord = new TranRecord(tranRecord.tranId(), tranRecord.tranTypeCd(), tranRecord.tranCatCd(),
                            tranRecord.tranSource(), tranRecord.tranDesc(), tranRecord.tranAmt(),
                            tranRecord.tranMerchantId(), tranRecord.tranMerchantName(),
                            tranRecord.tranMerchantCity(), tranRecord.tranMerchantZip(), tranRecord.tranCardNum(),
                            dateTime.timestamp(), dateTime.timestamp());
                    writeTransactFile();
                    accountRecord = accountRecord.withAcctCurrBal(
                            accountRecord.acctCurrBal().subtract(tranRecord.tranAmt()));
                    updateAcctdatFile();
                } else {
                    moveMessage(MSG_CONFIRM);
                    map.confirm.length(-1);
                }

                sendBillpayScreen();
            }
        }

        // GET-CURRENT-TIMESTAMP
        void getCurrentTimestamp() {
            LocalDateTime now = LocalDateTime.now(clock);          // EXEC CICS ASKTIME
            String curDateX10 = Cobol.alnum(now.format(FMT_DATE), 10); // FORMATTIME YYYYMMDD DATESEP('-')
            String curTimeX08 = now.format(FMT_TIME);              // FORMATTIME TIME TIMESEP(':')
            dateTime.initializeTimestamp();
            dateTime.moveToTimestamp(1, 10, curDateX10);
            dateTime.moveToTimestamp(12, 8, curTimeX08);
            dateTime.zeroTimestampMicros();
        }

        // RETURN-TO-PREV-SCREEN
        void returnToPrevScreen() {
            if (Cobol.isSpacesOrLowValues(commarea.toProgram())) {
                commarea.toProgram("COSGN00C");
            }
            commarea.fromTranId(WS_TRANID);
            commarea.fromProgram(WS_PGMNAME);
            commarea.setPgmEnter();
            throw new XctlSignal(commarea.toProgram());
        }

        // SEND-BILLPAY-SCREEN
        void sendBillpayScreen() {
            populateHeaderInfo();
            map.errmsg.data(wsMessage);
            screens.add(new BillPayScreen(map.trnname.data(), map.title01.data(), map.curdate.data(),
                    map.pgmname.data(), map.title02.data(), map.curtime.data(), map.actidin.data(),
                    map.curbal.data(), map.confirm.data(), map.errmsg.data(), map.errmsg.color(),
                    map.cursorField()));
        }

        // RECEIVE-BILLPAY-SCREEN
        void receiveBillpayScreen() {
            BillPayScreenInput in = request.screen();
            receive(map.trnname, in.trnname());
            receive(map.title01, in.title01());
            receive(map.curdate, in.curdate());
            receive(map.pgmname, in.pgmname());
            receive(map.title02, in.title02());
            receive(map.curtime, in.curtime());
            receive(map.actidin, in.actidin());
            receive(map.curbal, in.curbal());
            receive(map.confirm, in.confirm());
            receive(map.errmsg, in.errmsg());
            wsRespCd = CicsResp.NORMAL.code();
            wsReasCd = 0;
        }

        private void receive(Cobil00Map.BmsField field, String value) {
            field.lowValuesForReceive();
            if (value != null) {
                field.data(value);
                field.length(Math.min(value.length(), field.dataLength()));
            }
        }

        // POPULATE-HEADER-INFO
        void populateHeaderInfo() {
            dateTime.moveCurrentDate(LocalDateTime.now(clock));
            map.title01.data(ScreenTitle.CCDA_TITLE01);
            map.title02.data(ScreenTitle.CCDA_TITLE02);
            map.trnname.data(WS_TRANID);
            map.pgmname.data(WS_PGMNAME);
            map.curdate.data(dateTime.curdateMmDdYy());
            map.curtime.data(dateTime.curtimeHhMmSs());
        }

        // READ-ACCTDAT-FILE
        void readAcctdatFile() {
            CicsResponse<AccountRecord> r = acctdat.readForUpdate(acctIdKey);
            setResp(r);
            if (r.resp() == CicsResp.NORMAL) {
                accountRecord = r.record().orElseThrow();
            } else if (r.resp() == CicsResp.NOTFND) {
                fileError(MSG_ACCT_NOT_FOUND);
            } else {
                display();
                fileError(MSG_ACCT_LOOKUP);
            }
        }

        // UPDATE-ACCTDAT-FILE
        void updateAcctdatFile() {
            CicsResponse<Void> r = acctdat.rewrite(accountRecord);
            setResp(r);
            if (r.resp() == CicsResp.NORMAL) {
                return;
            } else if (r.resp() == CicsResp.NOTFND) {
                fileError(MSG_ACCT_NOT_FOUND);
            } else {
                display();
                fileError(MSG_ACCT_UPDATE);
            }
        }

        // READ-CXACAIX-FILE
        void readCxacaixFile() {
            CicsResponse<CardXrefRecord> r = cxacaix.readByAcctId(xrefAcctIdKey);
            setResp(r);
            r.record().ifPresent(rec -> cardXrefRecord = rec);
            if (r.resp() == CicsResp.NORMAL) {
                return;
            } else if (r.resp() == CicsResp.NOTFND) {
                fileError(MSG_ACCT_NOT_FOUND);
            } else {
                display();
                fileError(MSG_XREF_LOOKUP);
            }
        }

        // STARTBR-TRANSACT-FILE
        void startbrTransactFile() {
            CicsResponse<Void> r = transact.startBrowse(tranIdKey);
            setResp(r);
            if (r.resp() == CicsResp.NORMAL) {
                return;
            } else if (r.resp() == CicsResp.NOTFND) {
                fileError(MSG_TRAN_NOT_FOUND);
            } else {
                display();
                fileError(MSG_TRAN_LOOKUP);
            }
        }

        // READPREV-TRANSACT-FILE
        void readprevTransactFile() {
            CicsResponse<TranRecord> r = transact.readPrev();
            setResp(r);
            if (r.resp() == CicsResp.NORMAL) {
                tranRecord = r.record().orElseThrow();
                tranIdKey = tranRecord.tranId();
            } else if (r.resp() == CicsResp.ENDFILE) {
                tranIdKey = Cobol.unsignedDigits(0, TranRecord.TRAN_ID_LENGTH);
            } else {
                display();
                fileError(MSG_TRAN_LOOKUP);
            }
        }

        // ENDBR-TRANSACT-FILE
        void endbrTransactFile() {
            transact.endBrowse();
        }

        // WRITE-TRANSACT-FILE
        void writeTransactFile() {
            CicsResponse<Void> r = transact.write(tranRecord);
            setResp(r);
            if (r.resp() == CicsResp.NORMAL) {
                initializeAllFields();
                wsMessage = Cobol.spaces(80);
                map.errmsg.color(Dfhbmsca.DFHGREEN);
                String tranId = tranRecord.tranId();
                int sp = tranId.indexOf(' ');
                String delimited = sp >= 0 ? tranId.substring(0, sp) : tranId;
                String text = "Payment successful. " + " Your Transaction ID is " + delimited + ".";
                wsMessage = Cobol.alnum(text, 80);
                sendBillpayScreen();
            } else if (r.resp() == CicsResp.DUPKEY || r.resp() == CicsResp.DUPREC) {
                fileError(MSG_TRAN_EXISTS);
            } else {
                display();
                fileError(MSG_TRAN_ADD);
            }
        }

        // CLEAR-CURRENT-SCREEN
        void clearCurrentScreen() {
            initializeAllFields();
            sendBillpayScreen();
        }

        // INITIALIZE-ALL-FIELDS
        void initializeAllFields() {
            map.actidin.length(-1);
            map.actidin.spaces();
            map.curbal.spaces();
            map.confirm.spaces();
            wsMessage = Cobol.spaces(80);
        }

        /** Common file-error tail: MOVE 'Y' TO WS-ERR-FLG, message, MOVE -1 TO ACTIDINL, SEND. */
        private void fileError(String message) {
            errFlgOn = true;
            moveMessage(message);
            map.actidin.length(-1);
            sendBillpayScreen();
        }

        private void moveMessage(String message) {
            wsMessage = Cobol.alnum(message, 80);
        }

        private void setResp(CicsResponse<?> r) {
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
        }

        /** DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD. */
        private void display() {
            displays.add("RESP:" + Cobol.displaySigned(wsRespCd, 9) + "REAS:" + Cobol.displaySigned(wsReasCd, 9));
        }
    }
}

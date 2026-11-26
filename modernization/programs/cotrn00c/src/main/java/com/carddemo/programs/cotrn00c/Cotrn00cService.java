package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrn00c.copybook.CobolText;
import com.carddemo.programs.cotrn00c.copybook.CommonMessages;
import com.carddemo.programs.cotrn00c.copybook.Cotrn00Map;
import com.carddemo.programs.cotrn00c.copybook.DateTimeFields;
import com.carddemo.programs.cotrn00c.copybook.DfhAid;
import com.carddemo.programs.cotrn00c.copybook.MapField;
import com.carddemo.programs.cotrn00c.copybook.ScreenTitles;
import com.carddemo.programs.cotrn00c.copybook.TranRecord;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * COTRN00C - List Transactions from TRANSACT file (transaction CT00).
 * Each {@link #execute} call is one pseudo-conversational CICS task.
 */
public final class Cotrn00cService {

    public static final String WS_PGMNAME = "COTRN00C";
    public static final String WS_TRANID = "CT00";
    public static final String WS_TRANSACT_FILE = "TRANSACT";

    public static final String MSG_TOP_OF_PAGE = "You are at the top of the page...";
    public static final String MSG_REACHED_BOTTOM = "You have reached the bottom of the page...";
    public static final String MSG_REACHED_TOP = "You have reached the top of the page...";
    public static final String MSG_ALREADY_TOP = "You are already at the top of the page...";
    public static final String MSG_ALREADY_BOTTOM = "You are already at the bottom of the page...";
    public static final String MSG_UNABLE_TO_LOOKUP = "Unable to lookup transaction...";
    public static final String MSG_INVALID_SELECTION = "Invalid selection. Valid value is S";
    public static final String MSG_TRAN_ID_NOT_NUMERIC = "Tran ID must be Numeric ...";

    private final TransactFileGateway transactFile;
    private final Clock clock;

    public Cotrn00cService(TransactFileGateway transactFile, Clock clock) {
        this.transactFile = transactFile;
        this.clock = clock;
    }

    public Cotrn00cResponse execute(Cotrn00cRequest request) {
        return new Task(request).mainPara();
    }

    /** EXEC CICS XCTL: control never returns to the program. */
    private static final class XctlSignal extends RuntimeException {
        final String program;

        XctlSignal(String program) {
            super(null, null, false, false);
            this.program = program;
        }
    }

    /** Unhandled CICS condition: the task abends. */
    private static final class AbendSignal extends RuntimeException {
        final String code;

        AbendSignal(String code) {
            super(null, null, false, false);
            this.code = code;
        }
    }

    /** WORKING-STORAGE of one task. */
    private final class Task {
        private final DfhAid eibaid;
        private final Cotrn00cCommarea dfhCommarea;
        private final ScreenInput received;

        private Cotrn00cCommarea commarea = Cotrn00cCommarea.initial(); // CARDDEMO-COMMAREA + CT00-INFO
        private final Cotrn00Map map = new Cotrn00Map();                // COTRN0AI / COTRN0AO
        private final DateTimeFields dateTime = new DateTimeFields();   // CSDAT01Y
        private String wsMessage = CobolText.spaces(80);                // WS-MESSAGE X(80)
        private boolean errFlg;                                         // WS-ERR-FLG
        private boolean transactEof;                                    // WS-TRANSACT-EOF
        private boolean sendErase = true;                               // WS-SEND-ERASE-FLG
        private int wsRespCd;                                           // WS-RESP-CD S9(09) COMP
        private int wsReasCd;                                           // WS-REAS-CD S9(09) COMP
        private int wsIdx;                                              // WS-IDX S9(04) COMP
        // TRAN-RECORD has no VALUE clause; assumed LOW-VALUES at task start.
        private String tranId = CobolText.lowValues(TranRecord.TRAN_ID_LENGTH);
        private TranRecord tranRecord;

        private final List<ScreenImage> screens = new ArrayList<>();
        private final List<String> displayLines = new ArrayList<>();

        Task(Cotrn00cRequest request) {
            this.eibaid = request.eibaid();
            this.dfhCommarea = request.commarea();
            this.received = request.screen();
        }

        // MAIN-PARA
        Cotrn00cResponse mainPara() {
            try {
                errFlg = false;
                transactEof = false;
                commarea.ct00().setNextPageNo();
                sendErase = true;
                wsMessage = CobolText.spaces(80);
                map.errmsg.set("");
                map.trnidin.setLength(-1);

                if (dfhCommarea == null) {
                    commarea.general().setToProgram("COSGN00C");
                    returnToPrevScreen();
                } else {
                    commarea = dfhCommarea.copy();
                    if (!commarea.general().isPgmReenter()) {
                        commarea.general().setPgmContext(CarddemoCommarea.PgmContext.REENTER);
                        map.setLowValues();
                        processEnterKey();
                        sendTrnlstScreen();
                    } else {
                        receiveTrnlstScreen();
                        switch (eibaid) {
                            case DFHENTER -> processEnterKey();
                            case DFHPF3 -> {
                                commarea.general().setToProgram("COMEN01C");
                                returnToPrevScreen();
                            }
                            case DFHPF7 -> processPf7Key();
                            case DFHPF8 -> processPf8Key();
                            default -> {
                                errFlg = true;
                                map.trnidin.setLength(-1);
                                wsMessage = CobolText.fit(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                                sendTrnlstScreen();
                            }
                        }
                    }
                }
                return response(Cotrn00cResponse.Outcome.RETURN, WS_TRANID, null, null);
            } catch (XctlSignal x) {
                return response(Cotrn00cResponse.Outcome.XCTL, null, x.program, null);
            } catch (AbendSignal a) {
                return response(Cotrn00cResponse.Outcome.ABEND, null, null, a.code);
            }
        }

        private Cotrn00cResponse response(Cotrn00cResponse.Outcome outcome, String transid, String program,
                String abend) {
            return new Cotrn00cResponse(outcome, transid, program, abend, commarea.copy(), List.copyOf(screens),
                    List.copyOf(displayLines));
        }

        // PROCESS-ENTER-KEY
        private void processEnterKey() {
            Ct00Info ct00 = commarea.ct00();
            boolean selected = false;
            for (int i = 1; i <= Cotrn00Map.ROWS; i++) {
                Cotrn00Map.Row row = map.row(i);
                if (!CobolText.isSpacesOrLowValues(row.sel.data())) {
                    ct00.setTrnSelFlg(row.sel.data());
                    ct00.setTrnSelected(row.trnid.data());
                    selected = true;
                    break;
                }
            }
            if (!selected) {
                ct00.setTrnSelFlg(" ");
                ct00.setTrnSelected(CobolText.spaces(16));
            }
            if (!CobolText.isSpacesOrLowValues(ct00.trnSelFlg())
                    && !CobolText.isSpacesOrLowValues(ct00.trnSelected())) {
                switch (ct00.trnSelFlg()) {
                    case "S", "s" -> {
                        CarddemoCommarea g = commarea.general();
                        g.setToProgram("COTRN01C");
                        g.setFromTranid(WS_TRANID);
                        g.setFromProgram(WS_PGMNAME);
                        g.setPgmContext(0);
                        throw new XctlSignal(g.toProgram().stripTrailing());
                    }
                    default -> {
                        wsMessage = CobolText.fit(MSG_INVALID_SELECTION, 80);
                        map.trnidin.setLength(-1);
                    }
                }
            }

            String trnidinI = map.trnidin.data();
            if (CobolText.isSpacesOrLowValues(trnidinI)) {
                tranId = CobolText.lowValues(TranRecord.TRAN_ID_LENGTH);
            } else if (CobolText.isNumeric(trnidinI)) {
                tranId = trnidinI;
            } else {
                errFlg = true;
                wsMessage = CobolText.fit(MSG_TRAN_ID_NOT_NUMERIC, 80);
                map.trnidin.setLength(-1);
                sendTrnlstScreen();
            }

            map.trnidin.setLength(-1);

            ct00.setPageNum(0);
            processPageForward();

            if (!errFlg) {
                map.trnidin.set(" ");
            }
        }

        // PROCESS-PF7-KEY
        private void processPf7Key() {
            Ct00Info ct00 = commarea.ct00();
            tranId = CobolText.isSpacesOrLowValues(ct00.trnidFirst())
                    ? CobolText.lowValues(TranRecord.TRAN_ID_LENGTH)
                    : ct00.trnidFirst();
            ct00.setNextPageYes();
            map.trnidin.setLength(-1);

            if (ct00.pageNum() > 1) {
                processPageBackward();
            } else {
                wsMessage = CobolText.fit(MSG_ALREADY_TOP, 80);
                sendErase = false;
                sendTrnlstScreen();
            }
        }

        // PROCESS-PF8-KEY
        private void processPf8Key() {
            Ct00Info ct00 = commarea.ct00();
            tranId = CobolText.isSpacesOrLowValues(ct00.trnidLast())
                    ? CobolText.highValues(TranRecord.TRAN_ID_LENGTH)
                    : ct00.trnidLast();
            map.trnidin.setLength(-1);

            if (ct00.isNextPageYes()) {
                processPageForward();
            } else {
                wsMessage = CobolText.fit(MSG_ALREADY_BOTTOM, 80);
                sendErase = false;
                sendTrnlstScreen();
            }
        }

        // PROCESS-PAGE-FORWARD
        private void processPageForward() {
            Ct00Info ct00 = commarea.ct00();
            startbrTransactFile();

            if (!errFlg) {
                if (eibaid != DfhAid.DFHENTER && eibaid != DfhAid.DFHPF7 && eibaid != DfhAid.DFHPF3) {
                    readnextTransactFile();
                }

                if (!transactEof && !errFlg) {
                    for (wsIdx = 1; wsIdx <= 10; wsIdx++) {
                        initializeTranData();
                    }
                }

                wsIdx = 1;
                while (!(wsIdx >= 11 || transactEof || errFlg)) {
                    readnextTransactFile();
                    if (!transactEof && !errFlg) {
                        populateTranData();
                        wsIdx++;
                    }
                }

                if (!transactEof && !errFlg) {
                    ct00.setPageNum(ct00.pageNum() + 1);
                    readnextTransactFile();
                    if (!transactEof && !errFlg) {
                        ct00.setNextPageYes();
                    } else {
                        ct00.setNextPageNo();
                    }
                } else {
                    ct00.setNextPageNo();
                    if (wsIdx > 1) {
                        ct00.setPageNum(ct00.pageNum() + 1);
                    }
                }

                endbrTransactFile();

                map.pagenum.set(CobolText.unsignedDigits(ct00.pageNum(), 8));
                map.trnidin.set(" ");
                sendTrnlstScreen();
            }
        }

        // PROCESS-PAGE-BACKWARD
        private void processPageBackward() {
            Ct00Info ct00 = commarea.ct00();
            startbrTransactFile();

            if (!errFlg) {
                if (eibaid != DfhAid.DFHENTER && eibaid != DfhAid.DFHPF8) {
                    readprevTransactFile();
                }

                if (!transactEof && !errFlg) {
                    for (wsIdx = 1; wsIdx <= 10; wsIdx++) {
                        initializeTranData();
                    }
                }

                wsIdx = 10;
                while (!(wsIdx <= 0 || transactEof || errFlg)) {
                    readprevTransactFile();
                    if (!transactEof && !errFlg) {
                        populateTranData();
                        wsIdx--;
                    }
                }

                if (!transactEof && !errFlg) {
                    readprevTransactFile();
                    if (ct00.isNextPageYes()) {
                        if (!transactEof && !errFlg && ct00.pageNum() > 1) {
                            ct00.setPageNum(ct00.pageNum() - 1);
                        } else {
                            ct00.setPageNum(1);
                        }
                    }
                }

                endbrTransactFile();

                map.pagenum.set(CobolText.unsignedDigits(ct00.pageNum(), 8));
                sendTrnlstScreen();
            }
        }

        // POPULATE-TRAN-DATA
        private void populateTranData() {
            String wsTranAmt = formatTranAmt(tranRecord.tranAmt());
            dateTime.setTimestamp(tranRecord.tranOrigTs());
            dateTime.setCurdateYy(dateTime.timestampDtYyyy().substring(2, 4));
            dateTime.setCurdateMm(dateTime.timestampDtMm());
            dateTime.setCurdateDd(dateTime.timestampDtDd());
            String wsTranDate = dateTime.curdateMmDdYy();

            if (wsIdx >= 1 && wsIdx <= Cotrn00Map.ROWS) {
                Cotrn00Map.Row row = map.row(wsIdx);
                row.trnid.set(tranRecord.tranId());
                row.tdate.set(wsTranDate);
                row.tdesc.set(tranRecord.tranDesc());
                row.tamt.set(wsTranAmt);
                if (wsIdx == 1) {
                    commarea.ct00().setTrnidFirst(tranRecord.tranId());
                }
                if (wsIdx == 10) {
                    commarea.ct00().setTrnidLast(tranRecord.tranId());
                }
            }
        }

        // INITIALIZE-TRAN-DATA
        private void initializeTranData() {
            if (wsIdx >= 1 && wsIdx <= Cotrn00Map.ROWS) {
                Cotrn00Map.Row row = map.row(wsIdx);
                for (MapField f : List.of(row.trnid, row.tdate, row.tdesc, row.tamt)) {
                    f.set("");
                }
            }
        }

        // RETURN-TO-PREV-SCREEN
        private void returnToPrevScreen() {
            CarddemoCommarea g = commarea.general();
            if (CobolText.isSpacesOrLowValues(g.toProgram())) {
                g.setToProgram("COSGN00C");
            }
            g.setFromTranid(WS_TRANID);
            g.setFromProgram(WS_PGMNAME);
            g.setPgmContext(0);
            throw new XctlSignal(g.toProgram().stripTrailing());
        }

        // SEND-TRNLST-SCREEN
        private void sendTrnlstScreen() {
            populateHeaderInfo();
            map.errmsg.set(wsMessage);
            screens.add(ScreenImage.of(map, sendErase));
        }

        // RECEIVE-TRNLST-SCREEN
        private void receiveTrnlstScreen() {
            map.setLowValues();
            receiveField(map.trnidin, received.trnidin());
            receiveField(map.pagenum, received.pagenum());
            for (int i = 1; i <= Cotrn00Map.ROWS; i++) {
                ScreenInput.RowInput in = received.rows().get(i - 1);
                Cotrn00Map.Row row = map.row(i);
                receiveField(row.sel, in.sel());
                receiveField(row.trnid, in.trnid());
                receiveField(row.tdate, in.tdate());
                receiveField(row.tdesc, in.tdesc());
                receiveField(row.tamt, in.tamt());
            }
        }

        private void receiveField(MapField field, String value) {
            if (value != null) {
                field.set(value);
                field.setLength(Math.min(value.length(), field.size()));
            }
        }

        // POPULATE-HEADER-INFO
        private void populateHeaderInfo() {
            dateTime.setCurrentDate(LocalDateTime.now(clock));

            map.title01.set(ScreenTitles.CCDA_TITLE01);
            map.title02.set(ScreenTitles.CCDA_TITLE02);
            map.trnname.set(WS_TRANID);
            map.pgmname.set(WS_PGMNAME);

            dateTime.setCurdateMm(dateTime.curdateMonth());
            dateTime.setCurdateDd(dateTime.curdateDay());
            dateTime.setCurdateYy(dateTime.curdateYear().substring(2, 4));
            map.curdate.set(dateTime.curdateMmDdYy());

            dateTime.setCurtimeHh(dateTime.curtimeHours());
            dateTime.setCurtimeMm(dateTime.curtimeMinute());
            dateTime.setCurtimeSs(dateTime.curtimeSecond());
            map.curtime.set(dateTime.curtimeHhMmSs());
        }

        // STARTBR-TRANSACT-FILE
        private void startbrTransactFile() {
            CicsResponse r = transactFile.startBr(tranId);
            setResp(r);
            switch (r.resp()) {
                case NORMAL -> {
                }
                case NOTFND -> {
                    transactEof = true;
                    wsMessage = CobolText.fit(MSG_TOP_OF_PAGE, 80);
                    map.trnidin.setLength(-1);
                    sendTrnlstScreen();
                }
                default -> lookupFailed();
            }
        }

        // READNEXT-TRANSACT-FILE
        private void readnextTransactFile() {
            ReadResult r = transactFile.readNext();
            setResp(r.response());
            switch (r.response().resp()) {
                case NORMAL -> recordRead(r.record());
                case ENDFILE -> {
                    transactEof = true;
                    wsMessage = CobolText.fit(MSG_REACHED_BOTTOM, 80);
                    map.trnidin.setLength(-1);
                    sendTrnlstScreen();
                }
                default -> lookupFailed();
            }
        }

        // READPREV-TRANSACT-FILE
        private void readprevTransactFile() {
            ReadResult r = transactFile.readPrev();
            setResp(r.response());
            switch (r.response().resp()) {
                case NORMAL -> recordRead(r.record());
                case ENDFILE -> {
                    transactEof = true;
                    wsMessage = CobolText.fit(MSG_REACHED_TOP, 80);
                    map.trnidin.setLength(-1);
                    sendTrnlstScreen();
                }
                default -> lookupFailed();
            }
        }

        // ENDBR-TRANSACT-FILE: no RESP option, so any exceptional condition abends the task.
        private void endbrTransactFile() {
            CicsResponse r = transactFile.endBr();
            if (r.resp() != CicsResp.NORMAL) {
                throw new AbendSignal(r.resp().abendCode());
            }
        }

        private void recordRead(TranRecord record) {
            tranRecord = record;
            tranId = record.tranId();
        }

        private void setResp(CicsResponse r) {
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
        }

        private void lookupFailed() {
            displayLines.add("RESP:" + displayComp9(wsRespCd) + "REAS:" + displayComp9(wsReasCd));
            errFlg = true;
            wsMessage = CobolText.fit(MSG_UNABLE_TO_LOOKUP, 80);
            map.trnidin.setLength(-1);
            sendTrnlstScreen();
        }
    }

    /** DISPLAY of a PIC S9(09) COMP item: nine digits, with a trailing '-' only when negative. */
    static String displayComp9(int v) {
        String digits = CobolText.unsignedDigits(Math.abs((long) v), 9);
        return v < 0 ? digits + "-" : digits;
    }

    /**
     * MOVE TRAN-AMT (S9(09)V99) TO WS-TRAN-AMT (PIC +99999999.99): sign always shown, the ninth
     * integer digit is truncated (high-order), decimals kept as-is.
     */
    static String formatTranAmt(BigDecimal amount) {
        BigDecimal scaled = amount.setScale(2, RoundingMode.DOWN);
        char sign = scaled.signum() < 0 ? '-' : '+';
        BigInteger unscaled = scaled.unscaledValue().abs().mod(BigInteger.TEN.pow(10));
        String digits = unscaled.toString();
        digits = "0".repeat(10 - digits.length()) + digits;
        return sign + digits.substring(0, 8) + "." + digits.substring(8);
    }
}

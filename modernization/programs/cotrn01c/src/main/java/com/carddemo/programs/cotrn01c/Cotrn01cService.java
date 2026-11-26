package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.Cobol;
import com.carddemo.programs.cotrn01c.copybook.Cotrn1a;
import com.carddemo.programs.cotrn01c.copybook.Cotrn1a.Field;
import com.carddemo.programs.cotrn01c.copybook.Cottl01y;
import com.carddemo.programs.cotrn01c.copybook.Csdat01y;
import com.carddemo.programs.cotrn01c.copybook.Csmsg01y;
import com.carddemo.programs.cotrn01c.copybook.TranRecord;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/** COTRN01C - View a Transaction from the TRANSACT file (CICS transaction CT01). */
public final class Cotrn01cService {

    public static final String WS_PGMNAME = "COTRN01C";
    public static final String WS_TRANID = "CT01";

    public static final String MSG_TRAN_ID_EMPTY = "Tran ID can NOT be empty...";
    public static final String MSG_TRAN_ID_NOT_FOUND = "Transaction ID NOT found...";
    public static final String MSG_UNABLE_TO_LOOKUP = "Unable to lookup Transaction...";

    private static final Field[] DETAIL_FIELDS = {
        Field.TRNID, Field.CARDNUM, Field.TTYPCD, Field.TCATCD, Field.TRNSRC, Field.TRNAMT, Field.TDESC,
        Field.TORIGDT, Field.TPROCDT, Field.MID, Field.MNAME, Field.MCITY, Field.MZIP
    };

    private final TransactFileGateway transactFile;
    private final Clock clock;
    private final Consumer<String> display;

    public Cotrn01cService(TransactFileGateway transactFile, Clock clock, Consumer<String> display) {
        this.transactFile = transactFile;
        this.clock = clock;
        this.display = display;
    }

    public Cotrn01cService(TransactFileGateway transactFile) {
        this(transactFile, Clock.systemDefaultZone(), System.out::println);
    }

    /** Runs one task. Working storage is fresh per task, as in CICS. */
    public CicsOutcome execute(CicsRequest request) {
        return new Task(request).mainPara();
    }

    /** WORKING-STORAGE SECTION of one task plus the paragraphs operating on it. */
    private final class Task {
        private final CicsRequest request;
        private final Cotrn1a map = new Cotrn1a();
        private final Csdat01y dateTime = new Csdat01y();
        private final List<TrnViewScreen> sent = new ArrayList<>();
        private Cotrn01cCommarea commarea = new Cotrn01cCommarea();
        private String wsMessage = Cobol.spaces(80);      // WS-MESSAGE X(80)
        private boolean errFlg;                           // WS-ERR-FLG 88 ERR-FLG-ON 'Y' / OFF 'N'
        private boolean usrModified;                      // WS-USR-MODIFIED (unused by the logic)
        private String tranId = Cobol.spaces(16);         // TRAN-ID of TRAN-RECORD
        private TranRecord tranRecord;

        Task(CicsRequest request) {
            this.request = request;
        }

        // MAIN-PARA
        CicsOutcome mainPara() {
            errFlg = false;
            usrModified = false;
            wsMessage = Cobol.spaces(80);
            map.move(Field.ERRMSG, "");

            if (request.eibcalen() == 0) {
                commarea.toProgram("COSGN00C");
                return returnToPrevScreen();
            }
            commarea = Cotrn01cCommarea.fromDfhcommarea(request.dfhcommarea());
            if (!commarea.isPgmReenter()) {
                commarea.pgmContext(Cotrn01cCommarea.PgmContext.REENTER);
                map.moveLowValues();
                map.setCursor(Field.TRNIDIN);
                String selected = commarea.ct01TrnSelected();
                if (!Cobol.isSpaces(selected) && !Cobol.isLowValues(selected)) {
                    map.move(Field.TRNIDIN, selected);
                    processEnterKey();
                }
                sendTrnviewScreen();
            } else {
                receiveTrnviewScreen();
                switch (request.eibaid()) {
                    case DFHENTER -> processEnterKey();
                    case DFHPF3 -> {
                        if (Cobol.isSpacesOrLowValues(commarea.fromProgram())) {
                            commarea.toProgram("COMEN01C");
                        } else {
                            commarea.toProgram(commarea.fromProgram());
                        }
                        return returnToPrevScreen();
                    }
                    case DFHPF4 -> clearCurrentScreen();
                    case DFHPF5 -> {
                        commarea.toProgram("COTRN00C");
                        return returnToPrevScreen();
                    }
                    default -> {
                        errFlg = true;
                        wsMessage = Cobol.moveAlnum(Csmsg01y.CCDA_MSG_INVALID_KEY, 80);
                        sendTrnviewScreen();
                    }
                }
            }
            return new CicsOutcome.Return(WS_TRANID, commarea, sent);
        }

        // PROCESS-ENTER-KEY
        void processEnterKey() {
            if (Cobol.isSpacesOrLowValues(map.get(Field.TRNIDIN))) {
                errFlg = true;
                wsMessage = Cobol.moveAlnum(MSG_TRAN_ID_EMPTY, 80);
                map.setCursor(Field.TRNIDIN);
                sendTrnviewScreen();
            } else {
                map.setCursor(Field.TRNIDIN);
            }

            if (!errFlg) {
                for (Field f : DETAIL_FIELDS) {
                    map.move(f, "");
                }
                tranId = Cobol.moveAlnum(map.get(Field.TRNIDIN), 16);
                readTransactFile();
            }

            if (!errFlg) {
                TranRecord r = tranRecord;
                String wsTranAmt = Cobol.editPlus8Dot2(r.tranAmt());   // WS-TRAN-AMT PIC +99999999.99
                map.move(Field.TRNID, r.tranId());
                map.move(Field.CARDNUM, r.tranCardNum());
                map.move(Field.TTYPCD, r.tranTypeCd());
                map.move(Field.TCATCD, Cobol.moveUnsignedNumeric(r.tranCatCd(), 4));
                map.move(Field.TRNSRC, r.tranSource());
                map.move(Field.TRNAMT, wsTranAmt);
                map.move(Field.TDESC, r.tranDesc());
                map.move(Field.TORIGDT, r.tranOrigTs());
                map.move(Field.TPROCDT, r.tranProcTs());
                map.move(Field.MID, Cobol.moveUnsignedNumeric(r.tranMerchantId(), 9));
                map.move(Field.MNAME, r.tranMerchantName());
                map.move(Field.MCITY, r.tranMerchantCity());
                map.move(Field.MZIP, r.tranMerchantZip());
                sendTrnviewScreen();
            }
        }

        // RETURN-TO-PREV-SCREEN
        CicsOutcome returnToPrevScreen() {
            if (Cobol.isSpacesOrLowValues(commarea.toProgram())) {
                commarea.toProgram("COSGN00C");
            }
            commarea.fromTranid(WS_TRANID);
            commarea.fromProgram(WS_PGMNAME);
            commarea.pgmContextZeros();
            return new CicsOutcome.Xctl(commarea.toProgram(), commarea);
        }

        // SEND-TRNVIEW-SCREEN
        void sendTrnviewScreen() {
            populateHeaderInfo();
            map.move(Field.ERRMSG, wsMessage);
            sent.add(TrnViewScreen.of(map));
        }

        // RECEIVE-TRNVIEW-SCREEN (RESP/RESP2 are captured but never tested by COTRN01C)
        void receiveTrnviewScreen() {
            Map<Field, String> in = request.input().fields();
            for (Field f : Field.values()) {
                String v = in.get(f);
                if (v == null) {
                    map.putRaw(f, Cobol.lowValues(f.length()));
                    map.setLength(f, 0);
                } else {
                    map.move(f, v);
                    map.setLength(f, Math.min(v.length(), f.length()));
                }
            }
        }

        // POPULATE-HEADER-INFO
        void populateHeaderInfo() {
            dateTime.moveCurrentDate(LocalDateTime.now(clock));
            map.move(Field.TITLE01, Cottl01y.CCDA_TITLE01);
            map.move(Field.TITLE02, Cottl01y.CCDA_TITLE02);
            map.move(Field.TRNNAME, WS_TRANID);
            map.move(Field.PGMNAME, WS_PGMNAME);
            map.move(Field.CURDATE, dateTime.curdateMmDdYy());
            map.move(Field.CURTIME, dateTime.curtimeHhMmSs());
        }

        // READ-TRANSACT-FILE
        void readTransactFile() {
            TransactFileGateway.ReadResult result = transactFile.readForUpdate(tranId);
            int resp = result.resp();
            if (resp == CicsResp.NORMAL) {
                tranRecord = result.record();
            } else if (resp == CicsResp.NOTFND) {
                errFlg = true;
                wsMessage = Cobol.moveAlnum(MSG_TRAN_ID_NOT_FOUND, 80);
                map.setCursor(Field.TRNIDIN);
                sendTrnviewScreen();
            } else {
                display.accept("RESP:" + Cobol.displayBinaryS9(resp) + "REAS:" + Cobol.displayBinaryS9(result.resp2()));
                errFlg = true;
                wsMessage = Cobol.moveAlnum(MSG_UNABLE_TO_LOOKUP, 80);
                map.setCursor(Field.TRNIDIN);
                sendTrnviewScreen();
            }
        }

        // CLEAR-CURRENT-SCREEN
        void clearCurrentScreen() {
            initializeAllFields();
            sendTrnviewScreen();
        }

        // INITIALIZE-ALL-FIELDS
        void initializeAllFields() {
            map.setCursor(Field.TRNIDIN);
            map.move(Field.TRNIDIN, "");
            for (Field f : DETAIL_FIELDS) {
                map.move(f, "");
            }
            wsMessage = Cobol.spaces(80);
        }
    }
}

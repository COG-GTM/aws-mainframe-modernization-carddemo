package com.carddemo.online.cocrdlic;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardRecord;
import com.carddemo.common.copybook.DateTimeFields;
import com.carddemo.common.copybook.ScreenTitles;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsContext.SendMapOption;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cics.PfKeys;
import com.carddemo.runtime.cobol.CobolField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.vsam.FileResult;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COCRDLIC - list credit cards (transaction CCLI, mapset COCRDLI / map CCRDLIA). Browses CARDDAT 7 rows per
 * page with optional account / card filters; row action S goes to COCRDSLC (detail), U to COCRDUPC (update).
 * Each method is the COBOL paragraph of the same name; line numbers refer to app/cbl/COCRDLIC.cbl.
 */
@Component
public class Cocrdlic implements CicsProgram {

    public static final String PROGRAM = "COCRDLIC";
    public static final String TRANSID = "CCLI";
    public static final String MAPSET = "COCRDLI";
    public static final String MAP = "CCRDLIA";
    public static final String MENU_PROGRAM = "COMEN01C";
    public static final String CARD_DETAIL_PROGRAM = "COCRDSLC";
    public static final String CARD_UPDATE_PROGRAM = "COCRDUPC";
    /** LIT-CARD-FILE. */
    public static final String CARD_FILE = "CARDDAT";
    /** {@code WS-COMMAREA PIC X(2000)}: what RETURN TRANSID passes back (line 262, 615-619). */
    public static final int WS_COMMAREA_LENGTH = 2000;

    // LIT-* (lines 176-217)
    static final String LIT_MENUTRANID = "CM00";
    static final String LIT_MENUMAPSET = "COMEN01";
    static final String LIT_MENUMAP = "COMEN1A";
    static final String LIT_CARDDTLTRANID = "CCDL";
    static final String LIT_CARDDTLMAPSET = "COCRDSL";
    static final String LIT_CARDDTLMAP = "CCRDSLA";
    static final String LIT_CARDUPDTRANID = "CCUP";
    static final String LIT_CARDUPDMAPSET = "COCRDUP";
    static final String LIT_CARDUPDMAP = "CCRDUPA";
    static final String LIT_CARD_FILE_ACCT_PATH = "CARDAIX";
    static final int WS_MAX_SCREEN_LINES = 7;

    // Messages (lines 115-126, 903, 908, 1022, 1058, 1219, 1239)
    static final String MSG_INFORM_REC_ACTIONS = "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD";
    static final String MSG_EXIT = "PF03 PRESSED.EXITING";
    static final String MSG_NO_RECORDS_FOUND = "NO RECORDS FOUND FOR THIS SEARCH CONDITION.";
    static final String MSG_MORE_THAN_1_ACTION = "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE";
    static final String MSG_INVALID_ACTION_CODE = "INVALID ACTION CODE";
    static final String MSG_NO_PREVIOUS_PAGES = "NO PREVIOUS PAGES TO DISPLAY";
    static final String MSG_NO_MORE_PAGES = "NO MORE PAGES TO DISPLAY";
    static final String MSG_NO_MORE_RECORDS = "NO MORE RECORDS TO SHOW";
    static final String MSG_ACCT_FILTER = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    static final String MSG_CARD_FILTER = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";

    /** DFHBMSCA constants used here (3270 attribute / colour bytes, EBCDIC values). */
    static final class Dfhbmsca {
        static final int DFHBMPRO = 0x60;
        static final int DFHBMPRF = 0x61;
        static final int DFHBMFSE = 0xC1;
        static final int DFHBMDAR = 0x4C;
        static final int DFHRED = 0xF2;
        static final int DFHNEUTR = 0xF7;

        private Dfhbmsca() {
        }
    }

    /** WS-COMMAREA as returned to CICS: CARDDEMO-COMMAREA + WS-THIS-PROGCOMMAREA + SPACES. */
    public static final RecordLayout COMMAREA_LAYOUT;

    static {
        RecordLayout.Builder b = RecordLayout.builder("WS-COMMAREA");
        b.include(CardDemoCommarea.LAYOUT);
        b.include(ThisProgCommarea.LAYOUT);
        b.filler(WS_COMMAREA_LENGTH - CardDemoCommarea.LENGTH - ThisProgCommarea.LENGTH);
        COMMAREA_LAYOUT = b.build();
    }

    @Override
    public String programName() {
        return PROGRAM;
    }

    @Override
    public Optional<String> transactionId() {
        return Optional.of(TRANSID);
    }

    @Override
    public Optional<RecordLayout> commareaLayout() {
        return Optional.of(COMMAREA_LAYOUT);
    }

    @Override
    public void execute(CicsContext ctx) {
        new Task(ctx).mainPara();
    }

    /** One task's WORKING-STORAGE (lines 41-290) plus the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // INITIALIZE CC-WORK-AREA / WS-MISC-STORAGE (line 300): PIC X -> SPACES, numeric -> ZERO
        final CardWorkArea work = new CardWorkArea();
        int wsRespCd;
        int wsReasCd;
        String wsTranid = PicX.spaces(4);
        String wsInputFlag = " ";
        String wsEditAcctFlag = " ";
        String wsEditCardFlag = " ";
        final char[] wsEditSelect = PicX.spaces(7).toCharArray();
        final char[] wsEditSelectErrors = PicX.spaces(7).toCharArray();
        int i;
        int iSelected;
        String flgProtectSelectRows = " ";
        String wsInfoMsg = PicX.spaces(45);
        String wsErrorMsg = PicX.spaces(75);
        String wsPfkFlag = " ";
        String wsCardRidCardnum = PicX.spaces(16);
        int wsScrnCounter;
        String wsFilterRecordFlag = " ";
        String wsRecordsToProcessFlag = " ";
        String errorOpname = PicX.spaces(8);
        String errorFile = PicX.spaces(9);
        String errorResp = PicX.spaces(10);
        String errorResp2 = PicX.spaces(10);

        // COPY COCOM01Y, WS-THIS-PROGCOMMAREA, COCRDLI, CVACT02Y (no VALUE clauses: LOW-VALUES)
        final CardDemoCommarea commarea = new CardDemoCommarea();
        final ThisProgCommarea prog = new ThisProgCommarea();
        final SymbolicMap ccrdlia = SymbolicMap.of(MAPSET, MAP);
        final CardRecord cardRecord = new CardRecord();

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        // ------------------------------------------------------------------ 88-levels

        private boolean inputOk() {
            return wsInputFlag.equals("0") || wsInputFlag.equals(" ") || wsInputFlag.equals(String.valueOf(PicX.LOW_VALUE));
        }

        private boolean inputError() {
            return wsInputFlag.equals("1");
        }

        private boolean acctFilterNotOk() {
            return wsEditAcctFlag.equals("0");
        }

        private boolean acctFilterIsValid() {
            return wsEditAcctFlag.equals("1");
        }

        private boolean cardFilterNotOk() {
            return wsEditCardFlag.equals("0");
        }

        private boolean cardFilterIsValid() {
            return wsEditCardFlag.equals("1");
        }

        private boolean protectSelectRowsYes() {
            return flgProtectSelectRows.equals("1");
        }

        private boolean errorMsgOff() {
            return PicX.isSpaces(wsErrorMsg);
        }

        private boolean errorMsgIs(String text) {
            return PicX.equal(wsErrorMsg, text);
        }

        private boolean noInfoMessage() {
            return PicX.isSpaces(wsInfoMsg) || PicX.isLowValues(wsInfoMsg);
        }

        private void setErrorMsg(String text) {
            wsErrorMsg = PicX.of(text, 75);
        }

        /** SELECT-OK / VIEW-REQUESTED-ON / UPDATE-REQUESTED-ON on WS-EDIT-SELECT(n). */
        private boolean selectIs(int n, char c) {
            // n = 0 (no selection) addresses the byte before the array (last byte of the COMP-3
            // WS-EDIT-SELECT-COUNTER, X'0C'), which is never 'S' or 'U'.
            return n >= 1 && n <= 7 && wsEditSelect[n - 1] == c;
        }

        private boolean fromThisProgram() {
            return PicX.equal(commarea.fromProgram(), PROGRAM);
        }

        private boolean aid(String value) {
            return work.aidIs(value);
        }

        private int screenNum() {
            return prog.screenNum();
        }

        // ------------------------------------------------------------------ procedure division

        /** 0000-MAIN (lines 298-602). */
        void mainPara() {
            wsTranid = TRANSID;
            wsErrorMsg = PicX.spaces(75);

            int eibcalen = ctx.eib().eibcalen();
            if (eibcalen == 0) {
                initialize(commarea, CardDemoCommarea.LAYOUT);
                prog.initialize();
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                CardDemoCommarea.USRTYP_USER.set(commarea);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMap(MAP);
                commarea.setLastMapset(MAPSET);
                ThisProgCommarea.CA_FIRST_PAGE.set(prog);
                ThisProgCommarea.CA_LAST_PAGE_NOT_SHOWN.set(prog);
            } else {
                byte[] in = ctx.commarea();
                commarea.setBytes(Arrays.copyOf(in, Math.min(in.length, CardDemoCommarea.LENGTH)));
                prog.setBytes(in.length > CardDemoCommarea.LENGTH
                        ? Arrays.copyOfRange(in, CardDemoCommarea.LENGTH,
                                Math.min(in.length, CardDemoCommarea.LENGTH + ThisProgCommarea.LENGTH))
                        : new byte[0]);
            }

            if (commarea.isPgmEnter() && !fromThisProgram()) {
                prog.initialize();
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMap(MAP);
                ThisProgCommarea.CA_FIRST_PAGE.set(prog);
                ThisProgCommarea.CA_LAST_PAGE_NOT_SHOWN.set(prog);
            }

            yyyyStorePfkey();

            if (eibcalen > 0 && fromThisProgram()) {
                receiveMap2000();
            }

            wsPfkFlag = "1";
            if (aid("ENTER") || aid("PFK03") || aid("PFK07") || aid("PFK08")) {
                wsPfkFlag = "0";
            }
            if (wsPfkFlag.equals("1")) {
                work.setAid("ENTER");
            }

            if (aid("PFK03") && fromThisProgram()) {
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                CardDemoCommarea.USRTYP_USER.set(commarea);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMapset(MAPSET);
                commarea.setLastMap(MAP);
                commarea.setToProgram(MENU_PROGRAM);
                CardWorkArea.CCARD_NEXT_MAPSET.set(work, LIT_MENUMAPSET);
                CardWorkArea.CCARD_NEXT_MAP.set(work, MAP);
                setErrorMsg(MSG_EXIT);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                ctx.xctl(MENU_PROGRAM, commarea);
            }

            if (!aid("PFK08")) {
                ThisProgCommarea.CA_LAST_PAGE_NOT_SHOWN.set(prog);
            }

            if (inputError()) {
                CardWorkArea.CCARD_ERROR_MSG.set(work, wsErrorMsg);
                commarea.setFromProgram(PROGRAM);
                commarea.setLastMapset(MAPSET);
                commarea.setLastMap(MAP);
                setNextScreen(PROGRAM, MAPSET, MAP);
                if (!acctFilterNotOk() && !cardFilterNotOk()) {
                    readForward9000();
                }
                sendMap1000();
                commonReturn();
            } else if (aid("PFK07") && ThisProgCommarea.CA_FIRST_PAGE.isTrue(prog)) {
                wsCardRidCardnum = prog.firstCardNum();
                readForward9000();
                sendMap1000();
                commonReturn();
            } else if (aid("PFK03") || (commarea.isPgmReenter() && !fromThisProgram())) {
                initialize(commarea, CardDemoCommarea.LAYOUT);
                prog.initialize();
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                CardDemoCommarea.USRTYP_USER.set(commarea);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMap(MAP);
                commarea.setLastMapset(MAPSET);
                ThisProgCommarea.CA_FIRST_PAGE.set(prog);
                ThisProgCommarea.CA_LAST_PAGE_NOT_SHOWN.set(prog);
                wsCardRidCardnum = prog.firstCardNum();
                readForward9000();
                sendMap1000();
                commonReturn();
            } else if (aid("PFK08") && ThisProgCommarea.CA_NEXT_PAGE_EXISTS.isTrue(prog)) {
                wsCardRidCardnum = prog.lastCardNum();
                ThisProgCommarea.SCREEN_NUM.set(prog, ThisProgCommarea.SCREEN_NUM.get(prog).add(BigDecimal.ONE));
                readForward9000();
                sendMap1000();
                commonReturn();
            } else if (aid("PFK07") && !ThisProgCommarea.CA_FIRST_PAGE.isTrue(prog)) {
                wsCardRidCardnum = prog.firstCardNum();
                ThisProgCommarea.SCREEN_NUM.set(prog, ThisProgCommarea.SCREEN_NUM.get(prog).subtract(BigDecimal.ONE));
                readBackwards9100();
                sendMap1000();
                commonReturn();
            } else if (aid("ENTER") && selectIs(iSelected, 'S') && fromThisProgram()) {
                transferWithSelectedRow(CARD_DETAIL_PROGRAM, LIT_CARDDTLMAPSET, LIT_CARDDTLMAP);
            } else if (aid("ENTER") && selectIs(iSelected, 'U') && fromThisProgram()) {
                transferWithSelectedRow(CARD_UPDATE_PROGRAM, LIT_CARDUPDMAPSET, LIT_CARDUPDMAP);
            } else {
                wsCardRidCardnum = prog.firstCardNum();
                readForward9000();
                sendMap1000();
                commonReturn();
            }
            // Lines 585-601 are unreachable: every WHEN above ends with GO TO COMMON-RETURN or XCTL.
        }

        /** Lines 517-541 / 545-569: XCTL to the detail or update program with the selected row's keys. */
        private void transferWithSelectedRow(String program, String mapset, String map) {
            commarea.setFromTranid(TRANSID);
            commarea.setFromProgram(PROGRAM);
            CardDemoCommarea.USRTYP_USER.set(commarea);
            CardDemoCommarea.PGM_ENTER.set(commarea);
            commarea.setLastMapset(MAPSET);
            commarea.setLastMap(MAP);
            setNextScreen(program, mapset, map);
            moveToNumeric(commarea, CardDemoCommarea.ACCT_ID, prog.rowAcctNo(iSelected));
            moveToNumeric(commarea, CardDemoCommarea.CARD_NUM, prog.rowCardNum(iSelected));
            ctx.xctl(CardWorkArea.CCARD_NEXT_PROG.get(work), commarea);
        }

        private void setNextScreen(String program, String mapset, String map) {
            CardWorkArea.CCARD_NEXT_PROG.set(work, program);
            CardWorkArea.CCARD_NEXT_MAPSET.set(work, mapset);
            CardWorkArea.CCARD_NEXT_MAP.set(work, map);
        }

        /** COMMON-RETURN (lines 604-620). */
        void commonReturn() {
            commarea.setFromTranid(TRANSID);
            commarea.setFromProgram(PROGRAM);
            commarea.setLastMapset(MAPSET);
            commarea.setLastMap(MAP);
            byte[] out = PicX.toBytes(PicX.spaces(WS_COMMAREA_LENGTH));
            System.arraycopy(commarea.toBytes(), 0, out, 0, CardDemoCommarea.LENGTH);
            System.arraycopy(prog.toBytes(), 0, out, CardDemoCommarea.LENGTH, ThisProgCommarea.LENGTH);
            ctx.returnTransid(wsTranid, CobolRecord.ofBytes(out));
        }

        /** 1000-SEND-MAP (lines 624-641). */
        void sendMap1000() {
            screenInit1100();
            screenArrayInit1200();
            setupArrayAttribs1250();
            setupScreenAttrs1300();
            setupMessage1400();
            sendScreen1500();
        }

        /** 1100-SCREEN-INIT (lines 642-676). */
        void screenInit1100() {
            ccrdlia.lowValues();
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            ccrdlia.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            ccrdlia.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            ccrdlia.setData("TRNNAME", TRANSID);
            ccrdlia.setData("PGMNAME", PROGRAM);
            ccrdlia.setData("CURDATE", dt.curdateMmDdYy());
            ccrdlia.setData("CURTIME", dt.curtimeHhMmSs());
            // MOVE WS-CA-SCREEN-NUM (9(1)) TO PAGENOO (X(3)): the digit, left-justified
            ccrdlia.setData("PAGENO", ThisProgCommarea.SCREEN_NUM.raw(prog));
            wsInfoMsg = PicX.spaces(45);
            ccrdlia.setData("INFOMSG", wsInfoMsg);
            ccrdlia.setColor("INFOMSG", Dfhbmsca.DFHBMDAR);
        }

        /** 1200-SCREEN-ARRAY-INIT (lines 678-747). */
        void screenArrayInit1200() {
            for (int n = 1; n <= 7; n++) {
                if (PicX.isLowValues(prog.row(n))) {
                    continue;
                }
                ccrdlia.setData("CRDSEL" + n, String.valueOf(wsEditSelect[n - 1]));
                ccrdlia.setData("ACCTNO" + n, prog.rowAcctNo(n));
                ccrdlia.setData("CRDNUM" + n, prog.rowCardNum(n));
                ccrdlia.setData("CRDSTS" + n, prog.rowCardStatus(n));
            }
        }

        /** 1250-SETUP-ARRAY-ATTRIBS (lines 748-836). */
        void setupArrayAttribs1250() {
            // Row 1 differs from rows 2-7: protected with DFHBMPRF (not DFHBMPRO), and an error shows '*' in a
            // blank selection instead of positioning the cursor.
            if (PicX.isLowValues(prog.row(1)) || protectSelectRowsYes()) {
                ccrdlia.setAttribute("CRDSEL1", Dfhbmsca.DFHBMPRF);
            } else {
                if (wsEditSelectErrors[0] == '1') {
                    ccrdlia.setColor("CRDSEL1", Dfhbmsca.DFHRED);
                    if (wsEditSelect[0] == ' ' || wsEditSelect[0] == PicX.LOW_VALUE) {
                        ccrdlia.setData("CRDSEL1", "*");
                    }
                }
                ccrdlia.setAttribute("CRDSEL1", Dfhbmsca.DFHBMFSE);
            }
            for (int n = 2; n <= 7; n++) {
                if (PicX.isLowValues(prog.row(n)) || protectSelectRowsYes()) {
                    ccrdlia.setAttribute("CRDSEL" + n, Dfhbmsca.DFHBMPRO);
                    if (n == 4) {
                        // Line 790: a stray 'I' makes the MOVE also target subscript I (X'60' -> 0); I is
                        // not used again in this task.
                        i = 0;
                    }
                } else {
                    if (wsEditSelectErrors[n - 1] == '1') {
                        ccrdlia.setColor("CRDSEL" + n, Dfhbmsca.DFHRED);
                        ccrdlia.setCursor("CRDSEL" + n);
                    }
                    ccrdlia.setAttribute("CRDSEL" + n, Dfhbmsca.DFHBMFSE);
                }
            }
        }

        /** 1300-SETUP-SCREEN-ATTRS (lines 837-892). */
        void setupScreenAttrs1300() {
            boolean fromMenuOnEntry = commarea.isPgmEnter() && PicX.equal(commarea.fromProgram(), MENU_PROGRAM);
            if (!(ctx.eib().eibcalen() == 0 || fromMenuOnEntry)) {
                if (acctFilterIsValid() || acctFilterNotOk()) {
                    ccrdlia.setData("ACCTSID", work.acctId());
                    ccrdlia.setAttribute("ACCTSID", Dfhbmsca.DFHBMFSE);
                } else if (isZero(commarea, CardDemoCommarea.ACCT_ID)) {
                    ccrdlia.setData("ACCTSID", PicX.lowValues(11));
                } else {
                    ccrdlia.setData("ACCTSID", CardDemoCommarea.ACCT_ID.raw(commarea));
                    ccrdlia.setAttribute("ACCTSID", Dfhbmsca.DFHBMFSE);
                }

                if (cardFilterIsValid() || cardFilterNotOk()) {
                    ccrdlia.setData("CARDSID", work.cardNum());
                    ccrdlia.setAttribute("CARDSID", Dfhbmsca.DFHBMFSE);
                } else if (isZero(commarea, CardDemoCommarea.CARD_NUM)) {
                    ccrdlia.setData("CARDSID", PicX.lowValues(16));
                } else {
                    ccrdlia.setData("CARDSID", CardDemoCommarea.CARD_NUM.raw(commarea));
                    ccrdlia.setAttribute("CARDSID", Dfhbmsca.DFHBMFSE);
                }
            }

            if (acctFilterNotOk()) {
                ccrdlia.setColor("ACCTSID", Dfhbmsca.DFHRED);
                ccrdlia.setCursor("ACCTSID");
            }
            if (cardFilterNotOk()) {
                ccrdlia.setColor("CARDSID", Dfhbmsca.DFHRED);
                ccrdlia.setCursor("CARDSID");
            }
            if (inputOk()) {
                ccrdlia.setCursor("ACCTSID");
            }
        }

        /** 1400-SETUP-MESSAGE (lines 895-935). */
        void setupMessage1400() {
            if (acctFilterNotOk() || cardFilterNotOk()) {
                // CONTINUE
            } else if (aid("PFK07") && ThisProgCommarea.CA_FIRST_PAGE.isTrue(prog)) {
                setErrorMsg(MSG_NO_PREVIOUS_PAGES);
            } else if (aid("PFK08") && ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog)
                    && ThisProgCommarea.CA_LAST_PAGE_SHOWN.isTrue(prog)) {
                setErrorMsg(MSG_NO_MORE_PAGES);
            } else if (aid("PFK08") && ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog)) {
                wsInfoMsg = PicX.of(MSG_INFORM_REC_ACTIONS, 45);
                if (ThisProgCommarea.CA_LAST_PAGE_NOT_SHOWN.isTrue(prog)
                        && ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog)) {
                    ThisProgCommarea.CA_LAST_PAGE_SHOWN.set(prog);
                }
            } else if (noInfoMessage() || ThisProgCommarea.CA_NEXT_PAGE_EXISTS.isTrue(prog)) {
                wsInfoMsg = PicX.of(MSG_INFORM_REC_ACTIONS, 45);
            } else {
                wsInfoMsg = PicX.spaces(45);
            }

            ccrdlia.setData("ERRMSG", wsErrorMsg);

            if (!noInfoMessage() && !errorMsgIs(MSG_NO_RECORDS_FOUND)) {
                ccrdlia.setData("INFOMSG", wsInfoMsg);
                ccrdlia.setColor("INFOMSG", Dfhbmsca.DFHNEUTR);
            }
        }

        /** 1500-SEND-SCREEN (lines 938-950). */
        void sendScreen1500() {
            wsRespCd = ctx.sendMap(ccrdlia, SendMapOption.CURSOR, SendMapOption.ERASE, SendMapOption.FREEKB).code();
        }

        /** 2000-RECEIVE-MAP (lines 951-961). */
        void receiveMap2000() {
            receiveScreen2100();
            editInputs2200();
        }

        /** 2100-RECEIVE-SCREEN (lines 962-983). */
        void receiveScreen2100() {
            wsRespCd = ctx.receiveMap(ccrdlia).code();
            CardWorkArea.CC_ACCT_ID.set(work, ccrdlia.data("ACCTSID"));
            CardWorkArea.CC_CARD_NUM.set(work, ccrdlia.data("CARDSID"));
            for (int n = 1; n <= 7; n++) {
                wsEditSelect[n - 1] = ccrdlia.data("CRDSEL" + n).charAt(0);
            }
        }

        /** 2200-EDIT-INPUTS (lines 985-1001). */
        void editInputs2200() {
            wsInputFlag = "0";
            flgProtectSelectRows = "0";
            editAccount2210();
            editCard2220();
            editArray2250();
        }

        /** 2210-EDIT-ACCOUNT (lines 1003-1034). */
        void editAccount2210() {
            wsEditAcctFlag = " ";
            String cc = work.acctId();
            if (PicX.isLowValues(cc) || PicX.isSpaces(cc) || isZero(work, CardWorkArea.CC_ACCT_ID_N)) {
                wsEditAcctFlag = " ";
                CardDemoCommarea.ACCT_ID.set(commarea, 0);
                return;
            }
            if (!isDigits(cc)) {
                wsInputFlag = "1";
                wsEditAcctFlag = "0";
                flgProtectSelectRows = "1";
                setErrorMsg(MSG_ACCT_FILTER);
                CardDemoCommarea.ACCT_ID.set(commarea, 0);
                return;
            }
            CardDemoCommarea.ACCT_ID.set(commarea, new BigDecimal(cc));
            wsEditAcctFlag = "1";
        }

        /** 2220-EDIT-CARD (lines 1036-1071). */
        void editCard2220() {
            wsEditCardFlag = " ";
            String cc = work.cardNum();
            if (PicX.isLowValues(cc) || PicX.isSpaces(cc) || isZero(work, CardWorkArea.CC_CARD_NUM_N)) {
                wsEditCardFlag = " ";
                CardDemoCommarea.CARD_NUM.set(commarea, 0);
                return;
            }
            if (!isDigits(cc)) {
                wsInputFlag = "1";
                wsEditCardFlag = "0";
                flgProtectSelectRows = "1";
                if (errorMsgOff()) {
                    setErrorMsg(MSG_CARD_FILTER);
                }
                CardDemoCommarea.CARD_NUM.set(commarea, 0);
                return;
            }
            CardDemoCommarea.CARD_NUM.set(commarea, CardWorkArea.CC_CARD_NUM_N.get(work));
            wsEditCardFlag = "1";
        }

        /** 2250-EDIT-ARRAY (lines 1073-1121). */
        void editArray2250() {
            if (inputError()) {
                return;
            }
            for (char c : wsEditSelect) {
                if (c == 'S' || c == 'U') {
                    i++;
                }
            }
            if (i > 1) {
                wsInputFlag = "1";
                setErrorMsg(MSG_MORE_THAN_1_ACTION);
                for (int n = 0; n < 7; n++) {
                    wsEditSelectErrors[n] = wsEditSelect[n] == 'S' || wsEditSelect[n] == 'U' ? '1' : '0';
                }
            }
            iSelected = 0;
            for (i = 1; i <= 7; i++) {
                char c = wsEditSelect[i - 1];
                if (c == 'S' || c == 'U') {
                    iSelected = i;
                    if (errorMsgIs(MSG_MORE_THAN_1_ACTION)) {
                        wsEditSelectErrors[i - 1] = '1';
                    }
                } else if (c == ' ' || c == PicX.LOW_VALUE) {
                    // CONTINUE
                } else {
                    wsInputFlag = "1";
                    wsEditSelectErrors[i - 1] = '1';
                    if (errorMsgOff()) {
                        setErrorMsg(MSG_INVALID_ACTION_CODE);
                    }
                }
            }
        }

        /** 9000-READ-FORWARD (lines 1123-1263). */
        void readForward9000() {
            ThisProgCommarea.ALL_ROWS.fill(prog, PicX.LOW_VALUE);

            FileResult r = ctx.track(ctx.files().startbr(CARD_FILE, wsCardRidCardnum));
            setResp(r);

            wsScrnCounter = 0;
            ThisProgCommarea.CA_NEXT_PAGE_EXISTS.set(prog);
            wsRecordsToProcessFlag = "1";

            while (!wsRecordsToProcessFlag.equals("0")) {
                r = readnext();
                if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                    filterRecords9500();
                    if (wsFilterRecordFlag.equals("1")) {
                        wsScrnCounter++;
                        moveCardToRow(wsScrnCounter);
                        if (wsScrnCounter == 1) {
                            moveRaw(prog, ThisProgCommarea.FIRST_CARD_ACCT_ID, cardRecord.acctIdRaw());
                            ThisProgCommarea.FIRST_CARD_NUM.set(prog, cardRecord.cardNum());
                            if (screenNum() == 0) {
                                ThisProgCommarea.SCREEN_NUM.set(prog, 1);
                            }
                        }
                    }
                    if (wsScrnCounter == WS_MAX_SCREEN_LINES) {
                        wsRecordsToProcessFlag = "0";
                        moveCardToLastKey();
                        r = readnext();
                        if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                            ThisProgCommarea.CA_NEXT_PAGE_EXISTS.set(prog);
                            moveCardToLastKey();
                        } else if (r.resp() == CicsResp.ENDFILE) {
                            ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.set(prog);
                            if (errorMsgOff()) {
                                setErrorMsg(MSG_NO_MORE_RECORDS);
                            }
                        } else {
                            wsRecordsToProcessFlag = "0";
                            setFileErrorMessage();
                        }
                    }
                } else if (r.resp() == CicsResp.ENDFILE) {
                    wsRecordsToProcessFlag = "0";
                    ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.set(prog);
                    moveCardToLastKey();
                    if (errorMsgOff()) {
                        setErrorMsg(MSG_NO_MORE_RECORDS);
                    }
                    if (screenNum() == 1 && wsScrnCounter == 0) {
                        setErrorMsg(MSG_NO_RECORDS_FOUND);
                    }
                } else {
                    wsRecordsToProcessFlag = "0";
                    setFileErrorMessage();
                }
            }

            endbr();
        }

        /** 9100-READ-BACKWARDS (lines 1264-1380). */
        void readBackwards9100() {
            ThisProgCommarea.ALL_ROWS.fill(prog, PicX.LOW_VALUE);
            ThisProgCommarea.LAST_CARDKEY.set(prog, ThisProgCommarea.FIRST_CARDKEY.get(prog));

            FileResult r = ctx.track(ctx.files().startbr(CARD_FILE, wsCardRidCardnum));
            setResp(r);

            wsScrnCounter = WS_MAX_SCREEN_LINES + 1;
            ThisProgCommarea.CA_NEXT_PAGE_EXISTS.set(prog);
            wsRecordsToProcessFlag = "1";

            r = readprev();
            if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                wsScrnCounter--;
            } else {
                wsRecordsToProcessFlag = "0";
                setFileErrorMessage();
                readBackwardsExit9100();
                return;
            }

            while (!wsRecordsToProcessFlag.equals("0")) {
                r = readprev();
                if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                    filterRecords9500();
                    if (wsFilterRecordFlag.equals("1")) {
                        moveCardToRow(wsScrnCounter);
                        wsScrnCounter--;
                        if (wsScrnCounter == 0) {
                            wsRecordsToProcessFlag = "0";
                            moveRaw(prog, ThisProgCommarea.FIRST_CARD_ACCT_ID, cardRecord.acctIdRaw());
                            ThisProgCommarea.FIRST_CARD_NUM.set(prog, cardRecord.cardNum());
                        }
                    }
                } else {
                    wsRecordsToProcessFlag = "0";
                    setFileErrorMessage();
                }
            }
            readBackwardsExit9100();
        }

        /** 9100-READ-BACKWARDS-EXIT (lines 1374-1380): ENDBR. */
        void readBackwardsExit9100() {
            endbr();
        }

        /** 9500-FILTER-RECORDS (lines 1382-1411). */
        void filterRecords9500() {
            wsFilterRecordFlag = "1";
            if (acctFilterIsValid() && !PicX.equal(cardRecord.acctIdRaw(), work.acctId())) {
                wsFilterRecordFlag = "0";
                return;
            }
            if (cardFilterIsValid() && !PicX.equal(cardRecord.cardNum(), CardWorkArea.CC_CARD_NUM_N.raw(work))) {
                wsFilterRecordFlag = "0";
            }
        }

        /** YYYY-STORE-PFKEY (COPY CSSTRPFY, line 1416). */
        void yyyyStorePfkey() {
            work.setAid(PfKeys.storePfKey(ctx.eib().eibaid(), work.aid()));
        }

        /** SEND-PLAIN-TEXT (lines 1422-1435). Not performed by any path; kept for completeness. */
        void sendPlainText() {
            ctx.sendText(wsErrorMsg, wsErrorMsg.length(), true, true);
            ctx.returnToCics();
        }

        /** SEND-LONG-TEXT (lines 1441-1454). Not performed by any path; WS-LONG-MSG is always SPACES. */
        void sendLongText() {
            ctx.sendText(PicX.spaces(500), 500, true, true);
            ctx.returnToCics();
        }

        // ------------------------------------------------------------------ helpers

        private FileResult readnext() {
            FileResult r = ctx.track(ctx.files().readnext(CARD_FILE));
            return afterRead(r);
        }

        private FileResult readprev() {
            FileResult r = ctx.track(ctx.files().readprev(CARD_FILE));
            return afterRead(r);
        }

        /** INTO(CARD-RECORD) and RIDFLD(WS-CARD-RID-CARDNUM) are only updated when a record is returned. */
        private FileResult afterRead(FileResult r) {
            setResp(r);
            if (r.data() != null) {
                cardRecord.setBytes(r.data());
                wsCardRidCardnum = r.key();
            }
            return r;
        }

        /** EXEC CICS ENDBR without RESP: a failure raises the condition, i.e. the task abends. */
        private void endbr() {
            FileResult r = ctx.track(ctx.files().endbr(CARD_FILE));
            if (!r.isNormal()) {
                ctx.abend(abendCode(r.resp()));
            }
        }

        private void setResp(FileResult r) {
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
        }

        private void moveCardToRow(int n) {
            ThisProgCommarea.ROW_CARD_NUM[n - 1].set(prog, cardRecord.cardNum());
            ThisProgCommarea.ROW_ACCTNO[n - 1].set(prog, cardRecord.acctIdRaw());
            ThisProgCommarea.ROW_CARD_STATUS[n - 1].set(prog, cardRecord.activeStatus());
        }

        private void moveCardToLastKey() {
            moveRaw(prog, ThisProgCommarea.LAST_CARD_ACCT_ID, cardRecord.acctIdRaw());
            ThisProgCommarea.LAST_CARD_NUM.set(prog, cardRecord.cardNum());
        }

        /** WS-FILE-ERROR-MESSAGE (lines 153-171) built for a READ error and moved to WS-ERROR-MSG (75 bytes). */
        private void setFileErrorMessage() {
            errorOpname = PicX.of("READ", 8);
            errorFile = PicX.of(CARD_FILE, 9);
            errorResp = PicX.of(String.format("%09d", Math.abs((long) wsRespCd)), 10);
            errorResp2 = PicX.of(String.format("%09d", Math.abs((long) wsReasCd)), 10);
            setErrorMsg(PicX.of("File Error:", 12) + errorOpname + " on " + errorFile + " returned RESP " + errorResp
                    + ",RESP2 " + errorResp2 + PicX.lowValues(5));
        }
    }

    // ---------------------------------------------------------------------- static helpers

    /** CICS abend code for an unhandled file-control condition. */
    static String abendCode(CicsResp resp) {
        return switch (resp) {
            case FILENOTFOUND -> "AEIL";
            case NOTOPEN -> "AEIM";
            default -> "AEIP";
        };
    }

    /** {@code INITIALIZE record} for a layout without REDEFINES: PIC X to SPACES, numeric items to ZERO. */
    static void initialize(CobolRecord record, RecordLayout layout) {
        for (CobolField f : layout.fields()) {
            if (f instanceof NumericField n) {
                n.set(record, 0);
            } else {
                f.fill(record, PicX.SPACE);
            }
        }
    }

    /** {@code item = ZERO} for a zoned item; invalid (non-digit) content is treated as not equal to zero. */
    static boolean isZero(CobolRecord record, NumericField field) {
        return field.isNumeric(record) && field.get(record).signum() == 0;
    }

    static boolean isDigits(String s) {
        for (int k = 0; k < s.length(); k++) {
            char c = s.charAt(k);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return !s.isEmpty();
    }

    /** Byte-for-byte MOVE into an item (same-size DISPLAY moves, or invalid data that a MOVE copies unchanged). */
    static void moveRaw(CobolRecord record, CobolField field, String value) {
        byte[] bytes = record.toBytes();
        System.arraycopy(PicX.toBytes(PicX.of(value, field.length())), 0, bytes, field.offset(), field.length());
        record.setBytes(bytes);
    }

    /** MOVE PIC X TO PIC 9: numeric value when the source holds digits, otherwise its bytes unchanged. */
    static void moveToNumeric(CobolRecord record, NumericField field, String value) {
        if (isDigits(value)) {
            field.set(record, new BigDecimal(value));
        } else {
            moveRaw(record, field, value);
        }
    }
}

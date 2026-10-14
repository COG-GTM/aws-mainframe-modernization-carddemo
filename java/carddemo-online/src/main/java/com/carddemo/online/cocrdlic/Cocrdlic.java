package com.carddemo.online.cocrdlic;

import static com.carddemo.online.cocrdlic.Dfhbmsca.DFHBMDAR;
import static com.carddemo.online.cocrdlic.Dfhbmsca.DFHBMFSE;
import static com.carddemo.online.cocrdlic.Dfhbmsca.DFHBMPRF;
import static com.carddemo.online.cocrdlic.Dfhbmsca.DFHBMPRO;
import static com.carddemo.online.cocrdlic.Dfhbmsca.DFHNEUTR;
import static com.carddemo.online.cocrdlic.Dfhbmsca.DFHRED;

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
import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.Condition88;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.cobol.ZonedDecimal;
import com.carddemo.runtime.vsam.FileResult;
import java.util.Arrays;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COCRDLIC - list credit cards (transaction CCLI, mapset COCRDLI / map CCRDLIA). Browses CARDDAT 7 rows per
 * page with optional account / card filters; row selection S (view, XCTL COCRDSLC) or U (update, XCTL
 * COCRDUPC); PF3 back to COMEN01C, PF7/PF8 page. Each method is the COBOL paragraph of the same name; line
 * numbers refer to app/cbl/COCRDLIC.cbl.
 */
@Component
public class Cocrdlic implements CicsProgram {

    // WS-CONSTANTS (lines 176-217)
    public static final String PROGRAM = "COCRDLIC";
    public static final String TRANSID = "CCLI";
    public static final String MAPSET = "COCRDLI";
    public static final String MAP = "CCRDLIA";
    public static final String MENU_PROGRAM = "COMEN01C";
    static final String MENU_MAPSET = "COMEN01";
    public static final String CARD_DETAIL_PROGRAM = "COCRDSLC";
    static final String CARD_DETAIL_MAPSET = "COCRDSL";
    static final String CARD_DETAIL_MAP = "CCRDSLA";
    public static final String CARD_UPDATE_PROGRAM = "COCRDUPC";
    static final String CARD_UPDATE_MAPSET = "COCRDUP";
    static final String CARD_UPDATE_MAP = "CCRDUPA";
    static final String CARD_FILE = "CARDDAT ";
    static final int MAX_SCREEN_LINES = 7;

    // 88-level values of WS-INFO-MSG / WS-ERROR-MSG (lines 111-126) and literals in the procedure division
    static final String MSG_INFORM_REC_ACTIONS = "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD";
    static final String MSG_EXIT = "PF03 PRESSED.EXITING";
    static final String MSG_NO_RECORDS_FOUND = "NO RECORDS FOUND FOR THIS SEARCH CONDITION.";
    static final String MSG_MORE_THAN_1_ACTION = "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE";
    static final String MSG_INVALID_ACTION_CODE = "INVALID ACTION CODE";
    static final String MSG_ACCT_FILTER = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    static final String MSG_CARD_FILTER = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";
    static final String MSG_NO_MORE_RECORDS = "NO MORE RECORDS TO SHOW";
    static final String MSG_NO_PREVIOUS_PAGES = "NO PREVIOUS PAGES TO DISPLAY";
    static final String MSG_NO_MORE_PAGES = "NO MORE PAGES TO DISPLAY";

    // 88-levels of WS-THIS-PROGCOMMAREA (lines 237-248)
    static final Condition88 CA_FIRST_PAGE = Condition88.of("CA-FIRST-PAGE", ThisProgCommarea.SCREEN_NUM, 1);
    static final Condition88 CA_LAST_PAGE_SHOWN =
            Condition88.of("CA-LAST-PAGE-SHOWN", ThisProgCommarea.LAST_PAGE_DISPLAYED, 0);
    static final Condition88 CA_LAST_PAGE_NOT_SHOWN =
            Condition88.of("CA-LAST-PAGE-NOT-SHOWN", ThisProgCommarea.LAST_PAGE_DISPLAYED, 9);
    static final Condition88 CA_NEXT_PAGE_NOT_EXISTS =
            Condition88.of("CA-NEXT-PAGE-NOT-EXISTS", ThisProgCommarea.NEXT_PAGE_IND, String.valueOf(PicX.LOW_VALUE));
    static final Condition88 CA_NEXT_PAGE_EXISTS = Condition88.of("CA-NEXT-PAGE-EXISTS", ThisProgCommarea.NEXT_PAGE_IND, "Y");

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
        return Optional.of(ThisProgCommarea.WS_COMMAREA_LAYOUT);
    }

    @Override
    public void execute(CicsContext ctx) {
        new Task(ctx).main();
    }

    /**
     * Value of a zoned field as PACK would see it (low nibble of each byte), used for explicit {@code = 0} /
     * {@code = ZEROS} tests and ADD/SUBTRACT on items that may hold SPACES or LOW-VALUES. Valid digits give
     * their value; SPACES and LOW-VALUES count as 0 (same low nibble in EBCDIC and ASCII).
     */
    static long nibbleValue(String raw) {
        long v = 0;
        for (int i = 0; i < raw.length(); i++) {
            v = v * 10 + (raw.charAt(i) & 0x0F);
        }
        return v;
    }

    /** Default CICS abend for a condition raised by a command coded without RESP (no HANDLE CONDITION). */
    static String abendCodeFor(CicsResp resp) {
        return switch (resp) {
            case FILENOTFOUND -> "AEIL";
            case NOTFND -> "AEIM";
            case DUPREC -> "AEIN";
            case DUPKEY -> "AEIO";
            case INVREQ -> "AEIP";
            case IOERR -> "AEIQ";
            case NOSPACE -> "AEIR";
            case NOTOPEN -> "AEIS";
            case ENDFILE -> "AEIT";
            case ILLOGIC -> "AEIU";
            case LENGERR -> "AEIV";
            case DISABLED -> "AEXL";
            default -> "AEI0";
        };
    }

    /** One task's WORKING-STORAGE (lines 41-290) plus the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // CVCRD01Y CC-WORK-AREA - INITIALIZE (line 300) sets every item to SPACES
        String ccardAid = PicX.spaces(5);
        String ccardNextProg = PicX.spaces(8);
        String ccardNextMapset = PicX.spaces(7);
        String ccardNextMap = PicX.spaces(7);
        String ccardErrorMsg = PicX.spaces(75);
        String ccAcctId = PicX.spaces(11);
        String ccCardNum = PicX.spaces(16);

        // WS-MISC-STORAGE (lines 41-171) after INITIALIZE (line 300): alphanumerics SPACES, numerics ZERO
        int wsRespCd;
        int wsReasCd;
        String wsTranid = PicX.spaces(4);
        char wsInputFlag = ' ';
        char wsEditAcctFlag = ' ';
        char wsEditCardFlag = ' ';
        final char[] wsEditSelect = PicX.spaces(7).toCharArray();
        final char[] wsEditSelectErrorFlags = PicX.spaces(7).toCharArray();
        int i;
        int iSelected;
        char flgProtectSelectRows = ' ';
        final String wsLongMsg = PicX.spaces(500);
        String wsInfoMsg = PicX.spaces(45);
        String wsErrorMsg = PicX.spaces(75);
        char wsPfkFlag = ' ';
        String wsCardRidCardnum = PicX.spaces(16);
        int wsScrnCounter;
        char wsFilterRecordFlag = ' ';
        char wsRecordsToProcessFlag = ' ';
        // WS-FILE-ERROR-MESSAGE (lines 153-171): INITIALIZE skips the FILLERs, which keep their VALUEs
        String errorOpname = PicX.spaces(8);
        String errorFile = PicX.spaces(9);
        String errorResp = PicX.spaces(10);
        String errorResp2 = PicX.spaces(10);

        // COCOM01Y, WS-THIS-PROGCOMMAREA, COCRDLI, CVACT02Y (no VALUE clauses: LOW-VALUES)
        final CardDemoCommarea commarea = new CardDemoCommarea();
        final ThisProgCommarea prog = new ThisProgCommarea();
        final SymbolicMap ccrdlia = SymbolicMap.of(MAPSET, MAP);
        final CardRecord cardRecord = new CardRecord();

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        // ------------------------------------------------------------------ 88-level helpers

        private boolean aid(String value) {
            return ccardAid.equals(PicX.of(value, 5));
        }

        private boolean inputOk() {
            return wsInputFlag == '0' || wsInputFlag == ' ' || wsInputFlag == PicX.LOW_VALUE;
        }

        private boolean inputError() {
            return wsInputFlag == '1';
        }

        private boolean errorMsgOff() {
            return PicX.isSpaces(wsErrorMsg);
        }

        private boolean noInfoMessage() {
            return PicX.isSpacesOrLowValues(wsInfoMsg);
        }

        private boolean noRecordsFound() {
            return wsErrorMsg.equals(PicX.of(MSG_NO_RECORDS_FOUND, 75));
        }

        private boolean moreThan1Action() {
            return wsErrorMsg.equals(PicX.of(MSG_MORE_THAN_1_ACTION, 75));
        }

        private boolean fromThisProgram() {
            return PicX.equal(commarea.fromProgram(), PROGRAM);
        }

        private void setErrorMsg(String text) {
            wsErrorMsg = PicX.of(text, 75);
        }

        /** VIEW-REQUESTED-ON(I-SELECTED); subscript 0 addresses the X'0C' sign byte of WS-EDIT-SELECT-COUNTER. */
        private boolean viewRequestedOn(int index) {
            return index >= 1 && index <= 7 && wsEditSelect[index - 1] == 'S';
        }

        private boolean updateRequestedOn(int index) {
            return index >= 1 && index <= 7 && wsEditSelect[index - 1] == 'U';
        }

        /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics SPACES, numerics ZERO. */
        private void initializeCarddemoCommarea() {
            for (CobolField f : CardDemoCommarea.LAYOUT.fields()) {
                if (f instanceof NumericField n) {
                    n.set(commarea, 0);
                } else {
                    f.fill(commarea, PicX.SPACE);
                }
            }
        }

        /** MOVE alphanumeric TO an unsigned DISPLAY numeric of the same length: the bytes are copied. */
        private static void moveRaw(NumericField target, CobolRecord record, String value) {
            new AlphanumericField(target.name(), target.offset(), target.length()).set(record, value);
        }

        // ------------------------------------------------------------------ procedure division

        /** 0000-MAIN (lines 298-602) and COMMON-RETURN (604-620). */
        void main() {
            wsTranid = PicX.of(TRANSID, 4);
            wsErrorMsg = PicX.spaces(75);

            if (ctx.eib().eibcalen() == 0) {
                initializeCarddemoCommarea();
                prog.initialize();
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                CardDemoCommarea.USRTYP_USER.set(commarea);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMap(MAP);
                commarea.setLastMapset(MAPSET);
                CA_FIRST_PAGE.set(prog);
                CA_LAST_PAGE_NOT_SHOWN.set(prog);
            } else {
                byte[] in = ctx.commarea();
                commarea.setBytes(in);
                int end = Math.min(in.length, CardDemoCommarea.LENGTH + ThisProgCommarea.LENGTH);
                prog.setBytes(in.length > CardDemoCommarea.LENGTH
                        ? Arrays.copyOfRange(in, CardDemoCommarea.LENGTH, end) : new byte[0]);
            }

            // lines 336-343: coming in from the menu, forget the past
            if (commarea.isPgmEnter() && !fromThisProgram()) {
                prog.initialize();
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMap(MAP);
                CA_FIRST_PAGE.set(prog);
                CA_LAST_PAGE_NOT_SHOWN.set(prog);
            }

            yyyyStorePfkey();

            if (ctx.eib().eibcalen() > 0 && fromThisProgram()) {
                receiveMap2000();
            }

            // lines 370-380
            wsPfkFlag = '1';
            if (aid("ENTER") || aid("PFK03") || aid("PFK07") || aid("PFK08")) {
                wsPfkFlag = '0';
            }
            if (wsPfkFlag == '1') {
                ccardAid = "ENTER";
            }

            // lines 384-406: PF3 back to the main menu
            if (aid("PFK03") && fromThisProgram()) {
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                CardDemoCommarea.USRTYP_USER.set(commarea);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMapset(MAPSET);
                commarea.setLastMap(MAP);
                commarea.setToProgram(MENU_PROGRAM);
                ccardNextMapset = PicX.of(MENU_MAPSET, 7);
                ccardNextMap = PicX.of(MAP, 7);
                setErrorMsg(MSG_EXIT);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                ctx.xctl(MENU_PROGRAM, commarea);
            }

            // lines 410-414
            if (!aid("PFK08")) {
                CA_LAST_PAGE_NOT_SHOWN.set(prog);
            }

            // EVALUATE TRUE (lines 418-583); every branch ends in COMMON-RETURN or XCTL
            if (inputError()) {
                ccardErrorMsg = wsErrorMsg;
                commarea.setFromProgram(PROGRAM);
                commarea.setLastMapset(MAPSET);
                commarea.setLastMap(MAP);
                ccardNextProg = PicX.of(PROGRAM, 8);
                ccardNextMapset = PicX.of(MAPSET, 7);
                ccardNextMap = PicX.of(MAP, 7);
                if (wsEditAcctFlag != '0' && wsEditCardFlag != '0') {
                    readForward9000();
                }
                sendMap1000();
                commonReturn();
            } else if (aid("PFK07") && CA_FIRST_PAGE.isTrue(prog)) {
                wsCardRidCardnum = ThisProgCommarea.FIRST_CARD_NUM.get(prog);
                readForward9000();
                sendMap1000();
                commonReturn();
            } else if (aid("PFK03") || (commarea.isPgmReenter() && !fromThisProgram())) {
                initializeCarddemoCommarea();
                prog.initialize();
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                CardDemoCommarea.USRTYP_USER.set(commarea);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMap(MAP);
                commarea.setLastMapset(MAPSET);
                CA_FIRST_PAGE.set(prog);
                CA_LAST_PAGE_NOT_SHOWN.set(prog);
                wsCardRidCardnum = ThisProgCommarea.FIRST_CARD_NUM.get(prog);
                readForward9000();
                sendMap1000();
                commonReturn();
            } else if (aid("PFK08") && CA_NEXT_PAGE_EXISTS.isTrue(prog)) {
                wsCardRidCardnum = ThisProgCommarea.LAST_CARD_NUM.get(prog);
                ThisProgCommarea.SCREEN_NUM.set(prog, nibbleValue(prog.screenNumRaw()) + 1);
                readForward9000();
                sendMap1000();
                commonReturn();
            } else if (aid("PFK07") && !CA_FIRST_PAGE.isTrue(prog)) {
                wsCardRidCardnum = ThisProgCommarea.FIRST_CARD_NUM.get(prog);
                ThisProgCommarea.SCREEN_NUM.set(prog, nibbleValue(prog.screenNumRaw()) - 1);
                readBackwards9100();
                sendMap1000();
                commonReturn();
            } else if (aid("ENTER") && viewRequestedOn(iSelected) && fromThisProgram()) {
                transferToSelectedCard(CARD_DETAIL_PROGRAM, CARD_DETAIL_MAPSET, CARD_DETAIL_MAP);
            } else if (aid("ENTER") && updateRequestedOn(iSelected) && fromThisProgram()) {
                transferToSelectedCard(CARD_UPDATE_PROGRAM, CARD_UPDATE_MAPSET, CARD_UPDATE_MAP);
            } else {
                wsCardRidCardnum = ThisProgCommarea.FIRST_CARD_NUM.get(prog);
                readForward9000();
                sendMap1000();
                commonReturn();
            }
            // Lines 586-601 (IF INPUT-ERROR ... GO TO COMMON-RETURN) are unreachable: every WHEN above exits.
        }

        /** Lines 517-541 (view, COCRDSLC) and 545-569 (update, COCRDUPC). */
        private void transferToSelectedCard(String program, String mapset, String map) {
            commarea.setFromTranid(TRANSID);
            commarea.setFromProgram(PROGRAM);
            CardDemoCommarea.USRTYP_USER.set(commarea);
            CardDemoCommarea.PGM_ENTER.set(commarea);
            commarea.setLastMapset(MAPSET);
            commarea.setLastMap(MAP);
            ccardNextProg = PicX.of(program, 8);
            ccardNextMapset = PicX.of(mapset, 7);
            ccardNextMap = PicX.of(map, 7);
            moveRaw(CardDemoCommarea.ACCT_ID, commarea, ThisProgCommarea.ROW_ACCTNO[iSelected - 1].get(prog));
            moveRaw(CardDemoCommarea.CARD_NUM, commarea, ThisProgCommarea.ROW_CARD_NUM[iSelected - 1].get(prog));
            ctx.xctl(program, commarea);
        }

        /** COMMON-RETURN (lines 604-620): RETURN TRANSID(CCLI) COMMAREA(WS-COMMAREA) LENGTH(2000). */
        void commonReturn() {
            commarea.setFromTranid(TRANSID);
            commarea.setFromProgram(PROGRAM);
            commarea.setLastMapset(MAPSET);
            commarea.setLastMap(MAP);
            byte[] ws = new byte[ThisProgCommarea.WS_COMMAREA_LENGTH];
            Arrays.fill(ws, (byte) ' ');
            System.arraycopy(commarea.toBytes(), 0, ws, 0, CardDemoCommarea.LENGTH);
            System.arraycopy(prog.toBytes(), 0, ws, CardDemoCommarea.LENGTH, ThisProgCommarea.LENGTH);
            ctx.returnTransid(TRANSID, CobolRecord.ofBytes(ws));
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
            ccrdlia.setData("PAGENO", prog.screenNumRaw());
            wsInfoMsg = PicX.spaces(45);
            ccrdlia.setData("INFOMSG", wsInfoMsg);
            ccrdlia.setColor("INFOMSG", DFHBMDAR);
        }

        /** 1200-SCREEN-ARRAY-INIT (lines 678-747). */
        void screenArrayInit1200() {
            for (int n = 0; n < ThisProgCommarea.ROWS; n++) {
                if (PicX.isLowValues(ThisProgCommarea.EACH_CARD[n].get(prog))) {
                    continue;
                }
                int row = n + 1;
                ccrdlia.setData("CRDSEL" + row, String.valueOf(wsEditSelect[n]));
                ccrdlia.setData("ACCTNO" + row, ThisProgCommarea.ROW_ACCTNO[n].get(prog));
                ccrdlia.setData("CRDNUM" + row, ThisProgCommarea.ROW_CARD_NUM[n].get(prog));
                ccrdlia.setData("CRDSTS" + row, ThisProgCommarea.ROW_CARD_STATUS[n].get(prog));
            }
        }

        /** 1250-SETUP-ARRAY-ATTRIBS (lines 748-836). */
        void setupArrayAttribs1250() {
            for (int n = 0; n < ThisProgCommarea.ROWS; n++) {
                int row = n + 1;
                String sel = "CRDSEL" + row;
                if (PicX.isLowValues(ThisProgCommarea.EACH_CARD[n].get(prog)) || flgProtectSelectRows == '1') {
                    // row 1 uses DFHBMPRF (line 753), rows 2-7 DFHBMPRO
                    ccrdlia.setAttribute(sel, row == 1 ? DFHBMPRF : DFHBMPRO);
                    if (row == 4) {
                        // line 790: a stray "I" makes this MOVE DFHBMPRO TO CRDSEL4A OF CCRDLIAI I; X'60' moved to
                        // a numeric item contributes its low nibble, so I = 0 (I is not used afterwards)
                        i = DFHBMPRO & 0x0F;
                    }
                } else {
                    if (wsEditSelectErrorFlags[n] == '1') {
                        ccrdlia.setColor(sel, DFHRED);
                        if (row == 1) {
                            // lines 757-759: row 1 marks a blank selection with '*' and sets no cursor
                            if (wsEditSelect[n] == ' ' || wsEditSelect[n] == PicX.LOW_VALUE) {
                                ccrdlia.setData(sel, "*");
                            }
                        } else {
                            ccrdlia.setCursor(sel);
                        }
                    }
                    ccrdlia.setAttribute(sel, DFHBMFSE);
                }
            }
        }

        /** 1300-SETUP-SCREEN-ATTRS (lines 837-892). */
        void setupScreenAttrs1300() {
            boolean fromMenu = commarea.isPgmEnter() && PicX.equal(commarea.fromProgram(), MENU_PROGRAM);
            if (!(ctx.eib().eibcalen() == 0 || fromMenu)) {
                if (wsEditAcctFlag == '1' || wsEditAcctFlag == '0') {
                    ccrdlia.setData("ACCTSID", ccAcctId);
                    ccrdlia.setAttribute("ACCTSID", DFHBMFSE);
                } else if (nibbleValue(CardDemoCommarea.ACCT_ID.raw(commarea)) == 0) {
                    ccrdlia.setData("ACCTSID", PicX.lowValues(11));
                } else {
                    ccrdlia.setData("ACCTSID", CardDemoCommarea.ACCT_ID.raw(commarea));
                    ccrdlia.setAttribute("ACCTSID", DFHBMFSE);
                }

                if (wsEditCardFlag == '1' || wsEditCardFlag == '0') {
                    ccrdlia.setData("CARDSID", ccCardNum);
                    ccrdlia.setAttribute("CARDSID", DFHBMFSE);
                } else if (nibbleValue(CardDemoCommarea.CARD_NUM.raw(commarea)) == 0) {
                    ccrdlia.setData("CARDSID", PicX.lowValues(16));
                } else {
                    ccrdlia.setData("CARDSID", CardDemoCommarea.CARD_NUM.raw(commarea));
                    ccrdlia.setAttribute("CARDSID", DFHBMFSE);
                }
            }

            if (wsEditAcctFlag == '0') {
                ccrdlia.setColor("ACCTSID", DFHRED);
                ccrdlia.setCursor("ACCTSID");
            }
            if (wsEditCardFlag == '0') {
                ccrdlia.setColor("CARDSID", DFHRED);
                ccrdlia.setCursor("CARDSID");
            }
            if (inputOk()) {
                ccrdlia.setCursor("ACCTSID");
            }
        }

        /** 1400-SETUP-MESSAGE (lines 895-935). */
        void setupMessage1400() {
            if (wsEditAcctFlag == '0' || wsEditCardFlag == '0') {
                // CONTINUE
            } else if (aid("PFK07") && CA_FIRST_PAGE.isTrue(prog)) {
                setErrorMsg(MSG_NO_PREVIOUS_PAGES);
            } else if (aid("PFK08") && CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog) && CA_LAST_PAGE_SHOWN.isTrue(prog)) {
                setErrorMsg(MSG_NO_MORE_PAGES);
            } else if (aid("PFK08") && CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog)) {
                wsInfoMsg = PicX.of(MSG_INFORM_REC_ACTIONS, 45);
                if (CA_LAST_PAGE_NOT_SHOWN.isTrue(prog) && CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog)) {
                    CA_LAST_PAGE_SHOWN.set(prog);
                }
            } else if (noInfoMessage() || CA_NEXT_PAGE_EXISTS.isTrue(prog)) {
                wsInfoMsg = PicX.of(MSG_INFORM_REC_ACTIONS, 45);
            } else {
                wsInfoMsg = PicX.spaces(45);
            }

            ccrdlia.setData("ERRMSG", wsErrorMsg);

            if (!noInfoMessage() && !noRecordsFound()) {
                ccrdlia.setData("INFOMSG", wsInfoMsg);
                ccrdlia.setColor("INFOMSG", DFHNEUTR);
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
            ccAcctId = PicX.of(ccrdlia.data("ACCTSID"), 11);
            ccCardNum = PicX.of(ccrdlia.data("CARDSID"), 16);
            for (int n = 0; n < 7; n++) {
                wsEditSelect[n] = ccrdlia.data("CRDSEL" + (n + 1)).charAt(0);
            }
        }

        /** 2200-EDIT-INPUTS (lines 985-1001). */
        void editInputs2200() {
            wsInputFlag = '0';
            flgProtectSelectRows = '0';
            editAccount2210();
            editCard2220();
            editArray2250();
        }

        /** 2210-EDIT-ACCOUNT (lines 1003-1034). */
        void editAccount2210() {
            wsEditAcctFlag = ' ';
            if (PicX.isLowValues(ccAcctId) || PicX.isSpaces(ccAcctId) || nibbleValue(ccAcctId) == 0) {
                wsEditAcctFlag = ' ';
                CardDemoCommarea.ACCT_ID.set(commarea, 0);
                return;
            }
            if (!ZonedDecimal.isNumeric(ccAcctId, false)) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                flgProtectSelectRows = '1';
                setErrorMsg(MSG_ACCT_FILTER);
                CardDemoCommarea.ACCT_ID.set(commarea, 0);
                return;
            }
            moveRaw(CardDemoCommarea.ACCT_ID, commarea, ccAcctId);
            wsEditAcctFlag = '1';
        }

        /** 2220-EDIT-CARD (lines 1036-1071). */
        void editCard2220() {
            wsEditCardFlag = ' ';
            if (PicX.isLowValues(ccCardNum) || PicX.isSpaces(ccCardNum) || nibbleValue(ccCardNum) == 0) {
                wsEditCardFlag = ' ';
                CardDemoCommarea.CARD_NUM.set(commarea, 0);
                return;
            }
            if (!ZonedDecimal.isNumeric(ccCardNum, false)) {
                wsInputFlag = '1';
                wsEditCardFlag = '0';
                flgProtectSelectRows = '1';
                if (errorMsgOff()) {
                    setErrorMsg(MSG_CARD_FILTER);
                }
                CardDemoCommarea.CARD_NUM.set(commarea, 0);
                return;
            }
            moveRaw(CardDemoCommarea.CARD_NUM, commarea, ccCardNum);
            wsEditCardFlag = '1';
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
                wsInputFlag = '1';
                setErrorMsg(MSG_MORE_THAN_1_ACTION);
                for (int n = 0; n < 7; n++) {
                    wsEditSelectErrorFlags[n] = wsEditSelect[n] == 'S' || wsEditSelect[n] == 'U' ? '1' : '0';
                }
            }
            iSelected = 0;
            for (i = 1; i <= 7; i++) {
                char c = wsEditSelect[i - 1];
                if (c == 'S' || c == 'U') {
                    iSelected = i;
                    if (moreThan1Action()) {
                        wsEditSelectErrorFlags[i - 1] = '1';
                    }
                } else if (c == ' ' || c == PicX.LOW_VALUE) {
                    // CONTINUE
                } else {
                    wsInputFlag = '1';
                    wsEditSelectErrorFlags[i - 1] = '1';
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
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();

            wsScrnCounter = 0;
            CA_NEXT_PAGE_EXISTS.set(prog);
            wsRecordsToProcessFlag = '1';

            while (wsRecordsToProcessFlag != '0') {
                r = readnext();
                if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                    filterRecords9500();
                    if (wsFilterRecordFlag == '1') {
                        wsScrnCounter++;
                        int n = wsScrnCounter - 1;
                        ThisProgCommarea.ROW_CARD_NUM[n].set(prog, cardRecord.cardNum());
                        ThisProgCommarea.ROW_ACCTNO[n].set(prog, cardRecord.acctIdRaw());
                        ThisProgCommarea.ROW_CARD_STATUS[n].set(prog, cardRecord.activeStatus());
                        if (wsScrnCounter == 1) {
                            moveRaw(ThisProgCommarea.FIRST_CARD_ACCT_ID, prog, cardRecord.acctIdRaw());
                            ThisProgCommarea.FIRST_CARD_NUM.set(prog, cardRecord.cardNum());
                            if (nibbleValue(prog.screenNumRaw()) == 0) {
                                ThisProgCommarea.SCREEN_NUM.set(prog, nibbleValue(prog.screenNumRaw()) + 1);
                            }
                        }
                    }
                    if (wsScrnCounter == MAX_SCREEN_LINES) {
                        wsRecordsToProcessFlag = '0';
                        moveRaw(ThisProgCommarea.LAST_CARD_ACCT_ID, prog, cardRecord.acctIdRaw());
                        ThisProgCommarea.LAST_CARD_NUM.set(prog, cardRecord.cardNum());
                        r = readnext();
                        if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                            CA_NEXT_PAGE_EXISTS.set(prog);
                            moveRaw(ThisProgCommarea.LAST_CARD_ACCT_ID, prog, cardRecord.acctIdRaw());
                            ThisProgCommarea.LAST_CARD_NUM.set(prog, cardRecord.cardNum());
                        } else if (r.resp() == CicsResp.ENDFILE) {
                            CA_NEXT_PAGE_NOT_EXISTS.set(prog);
                            if (errorMsgOff()) {
                                setErrorMsg(MSG_NO_MORE_RECORDS);
                            }
                        } else {
                            wsRecordsToProcessFlag = '0';
                            fileErrorMessage();
                        }
                    }
                } else if (r.resp() == CicsResp.ENDFILE) {
                    wsRecordsToProcessFlag = '0';
                    CA_NEXT_PAGE_NOT_EXISTS.set(prog);
                    moveRaw(ThisProgCommarea.LAST_CARD_ACCT_ID, prog, cardRecord.acctIdRaw());
                    ThisProgCommarea.LAST_CARD_NUM.set(prog, cardRecord.cardNum());
                    if (errorMsgOff()) {
                        setErrorMsg(MSG_NO_MORE_RECORDS);
                    }
                    if (nibbleValue(prog.screenNumRaw()) == 1 && wsScrnCounter == 0) {
                        setErrorMsg(MSG_NO_RECORDS_FOUND);
                    }
                } else {
                    wsRecordsToProcessFlag = '0';
                    fileErrorMessage();
                }
            }
            endbr();
        }

        /** 9100-READ-BACKWARDS (lines 1264-1380). */
        void readBackwards9100() {
            ThisProgCommarea.ALL_ROWS.fill(prog, PicX.LOW_VALUE);
            ThisProgCommarea.LAST_CARDKEY.set(prog, ThisProgCommarea.FIRST_CARDKEY.get(prog));
            FileResult r = ctx.track(ctx.files().startbr(CARD_FILE, wsCardRidCardnum));
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();

            wsScrnCounter = MAX_SCREEN_LINES + 1;
            CA_NEXT_PAGE_EXISTS.set(prog);
            wsRecordsToProcessFlag = '1';

            r = readprev();
            if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                wsScrnCounter--;
            } else {
                wsRecordsToProcessFlag = '0';
                fileErrorMessage();
                endbr();
                return;
            }

            while (wsRecordsToProcessFlag != '0') {
                r = readprev();
                if (r.resp() == CicsResp.NORMAL || r.resp() == CicsResp.DUPREC) {
                    filterRecords9500();
                    if (wsFilterRecordFlag == '1') {
                        int n = wsScrnCounter - 1;
                        ThisProgCommarea.ROW_CARD_NUM[n].set(prog, cardRecord.cardNum());
                        ThisProgCommarea.ROW_ACCTNO[n].set(prog, cardRecord.acctIdRaw());
                        ThisProgCommarea.ROW_CARD_STATUS[n].set(prog, cardRecord.activeStatus());
                        wsScrnCounter--;
                        if (wsScrnCounter == 0) {
                            wsRecordsToProcessFlag = '0';
                            moveRaw(ThisProgCommarea.FIRST_CARD_ACCT_ID, prog, cardRecord.acctIdRaw());
                            ThisProgCommarea.FIRST_CARD_NUM.set(prog, cardRecord.cardNum());
                        }
                    }
                } else {
                    wsRecordsToProcessFlag = '0';
                    fileErrorMessage();
                }
            }
            // 9100-READ-BACKWARDS-EXIT (lines 1374-1380)
            endbr();
        }

        /** 9500-FILTER-RECORDS (lines 1382-1411). */
        void filterRecords9500() {
            wsFilterRecordFlag = '1';
            if (wsEditAcctFlag == '1' && !cardRecord.acctIdRaw().equals(ccAcctId)) {
                wsFilterRecordFlag = '0';
                return;
            }
            if (wsEditCardFlag == '1' && !cardRecord.cardNum().equals(ccCardNum)) {
                wsFilterRecordFlag = '0';
            }
        }

        /** YYYY-STORE-PFKEY (CSSTRPFY, line 1416). */
        void yyyyStorePfkey() {
            ccardAid = PfKeys.storePfKey(ctx.eib().eibaid(), ccardAid);
        }

        /** SEND-PLAIN-TEXT (lines 1422-1435); never performed. */
        void sendPlainText() {
            ctx.sendText(wsErrorMsg, wsErrorMsg.length(), true, true);
            ctx.returnToCics();
        }

        /** SEND-LONG-TEXT (lines 1441-1454); never performed. */
        void sendLongText() {
            ctx.sendText(wsLongMsg, wsLongMsg.length(), true, true);
            ctx.returnToCics();
        }

        // ------------------------------------------------------------------ file helpers

        /** EXEC CICS READNEXT DATASET(CARDDAT) INTO(CARD-RECORD) RIDFLD(WS-CARD-RID-CARDNUM) RESP RESP2. */
        private FileResult readnext() {
            return browseResult(ctx.track(ctx.files().readnext(CARD_FILE)));
        }

        /** EXEC CICS READPREV ... (same options as READNEXT). */
        private FileResult readprev() {
            return browseResult(ctx.track(ctx.files().readprev(CARD_FILE)));
        }

        private FileResult browseResult(FileResult r) {
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null) {
                cardRecord.setBytes(r.data());
                wsCardRidCardnum = PicX.of(r.key(), 16);
            }
            return r;
        }

        /** EXEC CICS ENDBR FILE(CARDDAT) - coded without RESP, so any condition abends the task. */
        private void endbr() {
            FileResult r = ctx.track(ctx.files().endbr(CARD_FILE));
            if (!r.isNormal()) {
                ctx.abend(abendCodeFor(r.resp()));
            }
        }

        /** MOVE 'READ' / LIT-CARD-FILE / WS-RESP-CD / WS-REAS-CD ... MOVE WS-FILE-ERROR-MESSAGE TO WS-ERROR-MSG. */
        private void fileErrorMessage() {
            errorOpname = PicX.of("READ", 8);
            errorFile = PicX.of(CARD_FILE, 9);
            errorResp = PicX.of(String.format("%09d", Math.abs((long) wsRespCd)), 10);
            errorResp2 = PicX.of(String.format("%09d", Math.abs((long) wsReasCd)), 10);
            String fileErrorMessage = PicX.of("File Error:", 12) + errorOpname + " on " + errorFile
                    + " returned RESP " + errorResp + ",RESP2 " + errorResp2 + PicX.lowValues(5);
            setErrorMsg(fileErrorMessage);
        }
    }
}

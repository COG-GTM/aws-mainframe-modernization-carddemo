package com.carddemo.programs.cocrdlic;

import com.carddemo.programs.cocrdlic.copybook.BmsAttributes;
import com.carddemo.programs.cocrdlic.copybook.BmsField;
import com.carddemo.programs.cocrdlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdlic.copybook.CardListMap;
import com.carddemo.programs.cocrdlic.copybook.CardRecord;
import com.carddemo.programs.cocrdlic.copybook.CardWorkAreas;
import com.carddemo.programs.cocrdlic.copybook.CardWorkAreas.CcardAid;
import com.carddemo.programs.cocrdlic.copybook.DateTimeFields;
import com.carddemo.programs.cocrdlic.copybook.PfKeyStore;
import com.carddemo.programs.cocrdlic.copybook.Pic;
import com.carddemo.programs.cocrdlic.copybook.ScreenTitles;

import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Arrays;

/**
 * COCRDLIC - list credit cards (transaction CCLI, map CCRDLIA / mapset COCRDLI).
 * Pages through CARDDAT seven rows at a time, optionally filtered by account and/or card number, and routes
 * an 'S' selection to COCRDSLC (detail) or a 'U' selection to COCRDUPC (update).
 * Each {@link #execute} call is one pseudo-conversational CICS task with fresh WORKING-STORAGE.
 */
public final class CocrdlicService {

    public static final int WS_MAX_SCREEN_LINES = 7;
    public static final String LIT_THISPGM = "COCRDLIC";
    public static final String LIT_THISTRANID = "CCLI";
    public static final String LIT_THISMAPSET = "COCRDLI";
    public static final String LIT_THISMAP = "CCRDLIA";
    public static final String LIT_MENUPGM = "COMEN01C";
    public static final String LIT_MENUTRANID = "CM00";
    public static final String LIT_MENUMAPSET = "COMEN01";
    public static final String LIT_MENUMAP = "COMEN1A";
    public static final String LIT_CARDDTLPGM = "COCRDSLC";
    public static final String LIT_CARDDTLTRANID = "CCDL";
    public static final String LIT_CARDDTLMAPSET = "COCRDSL";
    public static final String LIT_CARDDTLMAP = "CCRDSLA";
    public static final String LIT_CARDUPDPGM = "COCRDUPC";
    public static final String LIT_CARDUPDTRANID = "CCUP";
    public static final String LIT_CARDUPDMAPSET = "COCRDUP";
    public static final String LIT_CARDUPDMAP = "CCRDUPA";
    public static final String LIT_CARD_FILE = "CARDDAT ";
    public static final String LIT_CARD_FILE_ACCT_PATH = "CARDAIX ";
    public static final int WS_COMMAREA_LENGTH = 2000;

    public static final String MSG_ACCT_FILTER = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    public static final String MSG_CARD_FILTER = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";
    public static final String MSG_EXIT = "PF03 PRESSED.EXITING";
    public static final String MSG_NO_RECORDS_FOUND = "NO RECORDS FOUND FOR THIS SEARCH CONDITION.";
    public static final String MSG_MORE_THAN_1_ACTION = "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE";
    public static final String MSG_INVALID_ACTION_CODE = "INVALID ACTION CODE";
    public static final String MSG_NO_PREVIOUS_PAGES = "NO PREVIOUS PAGES TO DISPLAY";
    public static final String MSG_NO_MORE_PAGES = "NO MORE PAGES TO DISPLAY";
    public static final String MSG_NO_MORE_RECORDS = "NO MORE RECORDS TO SHOW";
    public static final String MSG_INFORM_REC_ACTIONS = "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD";

    private final CardFileGateway cardFile;
    private final Clock clock;

    public CocrdlicService(CardFileGateway cardFile, Clock clock) {
        this.cardFile = cardFile;
        this.clock = clock;
    }

    public CocrdlicService(CardFileGateway cardFile) {
        this(cardFile, Clock.systemDefaultZone());
    }

    public CardListResponse execute(CardListRequest request) {
        return new Task(request).main();
    }

    /** WORKING-STORAGE of one task plus the paragraphs of the PROCEDURE DIVISION. */
    private final class Task {
        private final CardListRequest request;
        private final int eibcalen;

        // CVCRD01Y, COCOM01Y, WS-THIS-PROGCOMMAREA, COCRDLI, CSDAT01Y
        private final CardWorkAreas cc = new CardWorkAreas();
        private CardDemoCommarea commarea = new CardDemoCommarea();
        private final ThisProgCommarea ca = new ThisProgCommarea();
        private final CardListMap map = new CardListMap();
        private final DateTimeFields dateTime = new DateTimeFields();
        // CVACT02Y CARD-RECORD (never INITIALIZEd: LOW-VALUES until the first successful read)
        private CardRecord cardRecord;

        // WS-MISC-STORAGE after INITIALIZE
        private int respCd;
        private int reasCd;
        private char inputFlag = ' ';            // 88 INPUT-OK '0',' ',LOW-VALUES / INPUT-ERROR '1'
        private char editAcctFlag = ' ';         // 88 NOT-OK '0' / ISVALID '1' / BLANK ' '
        private char editCardFlag = ' ';
        private final char[] editSelect = "       ".toCharArray();       // WS-EDIT-SELECT OCCURS 7
        private final char[] editSelectError = "       ".toCharArray();  // WS-ROW-CRDSELECT-ERROR OCCURS 7
        private int iSelected;                   // I-SELECTED, 88 DETAIL-WAS-REQUESTED 1 THRU 7
        private char protectSelectRows = ' ';    // 88 NO '0' / YES '1'
        private String infoMsg = Pic.spaces(45);
        private String errorMsg = Pic.spaces(75);
        private String cardRidCardnum = Pic.spaces(16);
        private int scrnCounter;
        private boolean readLoopExit;

        Task(CardListRequest request) {
            this.request = request;
            this.eibcalen = request.eibcalen();
        }

        // 0000-MAIN
        CardListResponse main() {
            if (eibcalen == 0) {
                commarea.initialize();
                ca.initialize();
                commarea.setFromTranid(LIT_THISTRANID);
                commarea.setFromProgram(LIT_THISPGM);
                commarea.setUserType(CardDemoCommarea.UserType.USER);
                commarea.setPgmEnter();
                commarea.setLastMap(LIT_THISMAP);
                commarea.setLastMapset(LIT_THISMAPSET);
                ca.setScreenNum(1);
                ca.setLastPageNotShown();
            } else {
                String dfhcommarea = Pic.x(request.commarea(), CardDemoCommarea.LENGTH + ThisProgCommarea.LENGTH);
                commarea = CardDemoCommarea.fromFixedWidth(dfhcommarea.substring(0, CardDemoCommarea.LENGTH));
                ThisProgCommarea passed = ThisProgCommarea.fromFixedWidth(
                        dfhcommarea.substring(CardDemoCommarea.LENGTH));
                copyInto(passed, ca);
            }

            if (commarea.isPgmEnter() && !fromThisProgram()) {
                ca.initialize();
                commarea.setPgmEnter();
                commarea.setLastMap(LIT_THISMAP);
                ca.setScreenNum(1);
                ca.setLastPageNotShown();
            }

            cc.ccardAid = PfKeyStore.storePfKey(request.eibaid(), cc.ccardAid);

            if (eibcalen > 0 && fromThisProgram()) {
                receiveMap();
            }

            boolean pfkValid = aid(CcardAid.ENTER) || aid(CcardAid.PFK03) || aid(CcardAid.PFK07)
                    || aid(CcardAid.PFK08);
            if (!pfkValid) {
                cc.ccardAid = CcardAid.ENTER;
            }

            if (aid(CcardAid.PFK03) && fromThisProgram()) {
                commarea.setFromTranid(LIT_THISTRANID);
                commarea.setFromProgram(LIT_THISPGM);
                commarea.setUserType(CardDemoCommarea.UserType.USER);
                commarea.setPgmEnter();
                commarea.setLastMapset(LIT_THISMAPSET);
                commarea.setLastMap(LIT_THISMAP);
                commarea.setToProgram(LIT_MENUPGM);
                cc.ccardNextMapset = Pic.x(LIT_MENUMAPSET, 7);
                cc.ccardNextMap = Pic.x(LIT_THISMAP, 7);
                errorMsg = Pic.x(MSG_EXIT, 75);
                commarea.setPgmEnter();
                return new CardListResponse.Xctl(LIT_MENUPGM, commarea.copy());
            }

            if (!aid(CcardAid.PFK08)) {
                ca.setLastPageNotShown();
            }

            if (isInputError()) {
                cc.ccardErrorMsg = errorMsg;
                commarea.setFromProgram(LIT_THISPGM);
                commarea.setLastMapset(LIT_THISMAPSET);
                commarea.setLastMap(LIT_THISMAP);
                cc.ccardNextProg = Pic.x(LIT_THISPGM, 8);
                cc.ccardNextMapset = Pic.x(LIT_THISMAPSET, 7);
                cc.ccardNextMap = Pic.x(LIT_THISMAP, 7);
                if (editAcctFlag != '0' && editCardFlag != '0') {
                    readForward();
                }
                sendMap();
                return commonReturn();
            }
            if (aid(CcardAid.PFK07) && ca.isFirstPage()) {
                cardRidCardnum = ca.firstCardNum;
                readForward();
                sendMap();
                return commonReturn();
            }
            if (aid(CcardAid.PFK03) || (commarea.isPgmReenter() && !fromThisProgram())) {
                commarea.initialize();
                ca.initialize();
                commarea.setFromTranid(LIT_THISTRANID);
                commarea.setFromProgram(LIT_THISPGM);
                commarea.setUserType(CardDemoCommarea.UserType.USER);
                commarea.setPgmEnter();
                commarea.setLastMap(LIT_THISMAP);
                commarea.setLastMapset(LIT_THISMAPSET);
                ca.setScreenNum(1);
                ca.setLastPageNotShown();
                cardRidCardnum = ca.firstCardNum;
                readForward();
                sendMap();
                return commonReturn();
            }
            if (aid(CcardAid.PFK08) && ca.isNextPageExists()) {
                cardRidCardnum = ca.lastCardNum;
                ca.setScreenNum(ca.screenNum + 1);
                readForward();
                sendMap();
                return commonReturn();
            }
            if (aid(CcardAid.PFK07) && !ca.isFirstPage()) {
                cardRidCardnum = ca.firstCardNum;
                ca.setScreenNum(ca.screenNum - 1);
                readBackwards();
                sendMap();
                return commonReturn();
            }
            if (aid(CcardAid.ENTER) && selected('S') && fromThisProgram()) {
                return transferToSelected(LIT_CARDDTLPGM, LIT_CARDDTLMAPSET, LIT_CARDDTLMAP);
            }
            if (aid(CcardAid.ENTER) && selected('U') && fromThisProgram()) {
                return transferToSelected(LIT_CARDUPDPGM, LIT_CARDUPDMAPSET, LIT_CARDUPDMAP);
            }
            cardRidCardnum = ca.firstCardNum;
            readForward();
            sendMap();
            return commonReturn();
        }

        /** WHEN CCARD-AID-ENTER AND VIEW/UPDATE-REQUESTED-ON(I-SELECTED): XCTL with the selected row's keys. */
        private CardListResponse transferToSelected(String program, String mapset, String mapName) {
            commarea.setFromTranid(LIT_THISTRANID);
            commarea.setFromProgram(LIT_THISPGM);
            commarea.setUserType(CardDemoCommarea.UserType.USER);
            commarea.setPgmEnter();
            commarea.setLastMapset(LIT_THISMAPSET);
            commarea.setLastMap(LIT_THISMAP);
            cc.ccardNextProg = Pic.x(program, 8);
            cc.ccardNextMapset = Pic.x(mapset, 7);
            cc.ccardNextMap = Pic.x(mapName, 7);
            commarea.setAcctId(Pic.toUnsigned(ca.rowAcctNo(iSelected), 11));
            commarea.setCardNum(Pic.toUnsigned(ca.rowCardNum(iSelected), 16));
            return new CardListResponse.Xctl(cc.ccardNextProg.strip(), commarea.copy());
        }

        // COMMON-RETURN
        private CardListResponse commonReturn() {
            commarea.setFromTranid(LIT_THISTRANID);
            commarea.setFromProgram(LIT_THISPGM);
            commarea.setLastMapset(LIT_THISMAPSET);
            commarea.setLastMap(LIT_THISMAP);
            String wsCommarea = Pic.x(commarea.toFixedWidth() + ca.toFixedWidth(), WS_COMMAREA_LENGTH);
            return new CardListResponse.SendMapAndReturn(LIT_THISTRANID, map, wsCommarea);
        }

        // 1000-SEND-MAP
        private void sendMap() {
            screenInit();
            screenArrayInit();
            setupArrayAttribs();
            setupScreenAttrs();
            setupMessage();
            sendScreen();
        }

        // 1100-SCREEN-INIT
        private void screenInit() {
            map.clear();
            dateTime.setFromCurrentDate(LocalDateTime.now(clock));
            map.title01.setData(ScreenTitles.CCDA_TITLE01);
            map.title02.setData(ScreenTitles.CCDA_TITLE02);
            map.trnname.setData(LIT_THISTRANID);
            map.pgmname.setData(LIT_THISPGM);
            map.curdate.setData(dateTime.curdateMmDdYy());
            map.curtime.setData(dateTime.curtimeHhMmSs());
            map.pageno.setData(Pic.nine(ca.screenNum, 1));
            infoMsg = Pic.spaces(45);
            map.infomsg.setData(infoMsg);
            map.infomsg.setColor(BmsAttributes.DFHBMDAR);
        }

        // 1200-SCREEN-ARRAY-INIT
        private void screenArrayInit() {
            for (int row = 1; row <= WS_MAX_SCREEN_LINES; row++) {
                if (!ca.isRowLowValues(row)) {
                    map.crdsel(row).setData(String.valueOf(editSelect[row - 1]));
                    map.acctno(row).setData(ca.rowAcctNo(row));
                    map.crdnum(row).setData(ca.rowCardNum(row));
                    map.crdsts(row).setData(String.valueOf(ca.rowCardStatus(row)));
                }
            }
        }

        // 1250-SETUP-ARRAY-ATTRIBS
        private void setupArrayAttribs() {
            for (int row = 1; row <= WS_MAX_SCREEN_LINES; row++) {
                BmsField sel = map.crdsel(row);
                if (ca.isRowLowValues(row) || protectSelectRows == '1') {
                    // row 1 is set to DFHBMPRF, rows 2-7 to DFHBMPRO in the original
                    sel.setAttribute(row == 1 ? BmsAttributes.DFHBMPRF : BmsAttributes.DFHBMPRO);
                } else {
                    if (editSelectError[row - 1] == '1') {
                        sel.setColor(BmsAttributes.DFHRED);
                        if (row == 1) {
                            if (editSelect[0] == ' ' || editSelect[0] == Pic.LOW_VALUE) {
                                sel.setData("*");
                            }
                        } else {
                            sel.setL(BmsField.CURSOR);
                        }
                    }
                    sel.setAttribute(BmsAttributes.DFHBMFSE);
                }
            }
        }

        // 1300-SETUP-SCREEN-ATTRS
        private void setupScreenAttrs() {
            if (!(eibcalen == 0 || (commarea.isPgmEnter() && commarea.getFromProgram().equals(LIT_MENUPGM)))) {
                if (editAcctFlag == '1' || editAcctFlag == '0') {
                    map.acctsid.setData(cc.ccAcctId);
                    map.acctsid.setAttribute(BmsAttributes.DFHBMFSE);
                } else if (commarea.getAcctId() == 0) {
                    map.acctsid.setData(null);
                } else {
                    map.acctsid.setData(Pic.nine(commarea.getAcctId(), 11));
                    map.acctsid.setAttribute(BmsAttributes.DFHBMFSE);
                }
                if (editCardFlag == '1' || editCardFlag == '0') {
                    map.cardsid.setData(cc.ccCardNum);
                    map.cardsid.setAttribute(BmsAttributes.DFHBMFSE);
                } else if (commarea.getCardNum() == 0) {
                    map.cardsid.setData(null);
                } else {
                    map.cardsid.setData(Pic.nine(commarea.getCardNum(), 16));
                    map.cardsid.setAttribute(BmsAttributes.DFHBMFSE);
                }
            }
            if (editAcctFlag == '0') {
                map.acctsid.setColor(BmsAttributes.DFHRED);
                map.acctsid.setL(BmsField.CURSOR);
            }
            if (editCardFlag == '0') {
                map.cardsid.setColor(BmsAttributes.DFHRED);
                map.cardsid.setL(BmsField.CURSOR);
            }
            if (!isInputError()) {
                map.acctsid.setL(BmsField.CURSOR);
            }
        }

        // 1400-SETUP-MESSAGE
        private void setupMessage() {
            if (editAcctFlag == '0' || editCardFlag == '0') {
                // keep the filter edit message
            } else if (aid(CcardAid.PFK07) && ca.isFirstPage()) {
                errorMsg = Pic.x(MSG_NO_PREVIOUS_PAGES, 75);
            } else if (aid(CcardAid.PFK08) && ca.isNextPageNotExists() && ca.isLastPageShown()) {
                errorMsg = Pic.x(MSG_NO_MORE_PAGES, 75);
            } else if (aid(CcardAid.PFK08) && ca.isNextPageNotExists()) {
                infoMsg = Pic.x(MSG_INFORM_REC_ACTIONS, 45);
                if (ca.isLastPageNotShown() && ca.isNextPageNotExists()) {
                    ca.setLastPageShown();
                }
            } else if (isNoInfoMessage() || ca.isNextPageExists()) {
                infoMsg = Pic.x(MSG_INFORM_REC_ACTIONS, 45);
            } else {
                infoMsg = Pic.spaces(45);
            }
            map.errmsg.setData(errorMsg);
            if (!isNoInfoMessage() && !errorMsg.equals(Pic.x(MSG_NO_RECORDS_FOUND, 75))) {
                map.infomsg.setData(infoMsg);
                map.infomsg.setColor(BmsAttributes.DFHNEUTR);
            }
        }

        // 1500-SEND-SCREEN: the populated map is returned in the response (RESP is not checked).
        private void sendScreen() {
            respCd = CicsResp.NORMAL;
        }

        // 2000-RECEIVE-MAP
        private void receiveMap() {
            receiveScreen();
            editInputs();
        }

        // 2100-RECEIVE-SCREEN
        private void receiveScreen() {
            CardListScreenInput in = request.screen();
            cc.ccAcctId = in.acctFilter() == null ? Pic.lowValues(11) : Pic.x(in.acctFilter(), 11);
            cc.ccCardNum = in.cardFilter() == null ? Pic.lowValues(16) : Pic.x(in.cardFilter(), 16);
            for (int row = 1; row <= WS_MAX_SCREEN_LINES; row++) {
                String s = in.selection(row);
                editSelect[row - 1] = s == null ? Pic.LOW_VALUE : Pic.x(s, 1).charAt(0);
            }
        }

        // 2200-EDIT-INPUTS
        private void editInputs() {
            inputFlag = '0';
            protectSelectRows = '0';
            editAccount();
            editCard();
            editArray();
        }

        // 2210-EDIT-ACCOUNT
        private void editAccount() {
            editAcctFlag = ' ';
            if (Pic.isLowValues(cc.ccAcctId) || Pic.isSpaces(cc.ccAcctId) || Pic.isZeros(cc.ccAcctId)) {
                editAcctFlag = ' ';
                commarea.setAcctId(0);
                return;
            }
            if (!Pic.isNumeric(cc.ccAcctId)) {
                inputFlag = '1';
                editAcctFlag = '0';
                protectSelectRows = '1';
                errorMsg = Pic.x(MSG_ACCT_FILTER, 75);
                commarea.setAcctId(0);
            } else {
                commarea.setAcctId(Pic.toUnsigned(cc.ccAcctId, 11));
                editAcctFlag = '1';
            }
        }

        // 2220-EDIT-CARD
        private void editCard() {
            editCardFlag = ' ';
            if (Pic.isLowValues(cc.ccCardNum) || Pic.isSpaces(cc.ccCardNum) || Pic.isZeros(cc.ccCardNum)) {
                editCardFlag = ' ';
                commarea.setCardNum(0);
                return;
            }
            if (!Pic.isNumeric(cc.ccCardNum)) {
                inputFlag = '1';
                editCardFlag = '0';
                protectSelectRows = '1';
                if (isErrorMsgOff()) {
                    errorMsg = Pic.x(MSG_CARD_FILTER, 75);
                }
                commarea.setCardNum(0);
            } else {
                commarea.setCardNum(cc.ccCardNumN());
                editCardFlag = '1';
            }
        }

        // 2250-EDIT-ARRAY
        private void editArray() {
            if (isInputError()) {
                return;
            }
            int i = 0;
            for (char c : editSelect) {
                if (c == 'S' || c == 'U') {
                    i++;
                }
            }
            if (i > 1) {
                inputFlag = '1';
                errorMsg = Pic.x(MSG_MORE_THAN_1_ACTION, 75);
                for (int k = 0; k < editSelect.length; k++) {
                    editSelectError[k] = (editSelect[k] == 'S' || editSelect[k] == 'U') ? '1' : '0';
                }
            }
            iSelected = 0;
            for (int row = 1; row <= WS_MAX_SCREEN_LINES; row++) {
                char c = editSelect[row - 1];
                if (c == 'S' || c == 'U') {
                    iSelected = row;
                    if (errorMsg.equals(Pic.x(MSG_MORE_THAN_1_ACTION, 75))) {
                        editSelectError[row - 1] = '1';
                    }
                } else if (c == ' ' || c == Pic.LOW_VALUE) {
                    continue;
                } else {
                    inputFlag = '1';
                    editSelectError[row - 1] = '1';
                    if (isErrorMsgOff()) {
                        errorMsg = Pic.x(MSG_INVALID_ACTION_CODE, 75);
                    }
                }
            }
        }

        // 9000-READ-FORWARD
        private void readForward() {
            ca.clearRowsToLowValues();
            respCd = cardFile.startBrowseGteq(LIT_CARD_FILE, cardRidCardnum);
            scrnCounter = 0;
            ca.setNextPageExists();
            readLoopExit = false;

            while (!readLoopExit) {
                CardFileGateway.ReadResult r = cardFile.readNext(LIT_CARD_FILE);
                setResp(r);
                if (r.isOk()) {
                    cardRecord = r.record();
                    if (!filterRecords()) {
                        scrnCounter++;
                        ca.setRow(scrnCounter, cardRecord.cardAcctIdDisplay(), cardRecord.cardNum(),
                                cardRecord.cardActiveStatus());
                        if (scrnCounter == 1) {
                            ca.setFirstCardKey(cardRecord.cardNum(), cardRecord.cardAcctId());
                            if (ca.screenNum == 0) {
                                ca.setScreenNum(ca.screenNum + 1);
                            }
                        }
                    }
                    if (scrnCounter == WS_MAX_SCREEN_LINES) {
                        readLoopExit = true;
                        ca.setLastCardKey(cardRecord.cardNum(), cardRecord.cardAcctId());
                        CardFileGateway.ReadResult ahead = cardFile.readNext(LIT_CARD_FILE);
                        setResp(ahead);
                        if (ahead.isOk()) {
                            cardRecord = ahead.record();
                            ca.setNextPageExists();
                            ca.setLastCardKey(cardRecord.cardNum(), cardRecord.cardAcctId());
                        } else if (ahead.resp() == CicsResp.ENDFILE) {
                            ca.setNextPageNotExists();
                            if (isErrorMsgOff()) {
                                errorMsg = Pic.x(MSG_NO_MORE_RECORDS, 75);
                            }
                        } else {
                            readLoopExit = true;
                            setFileErrorMessage();
                        }
                    }
                } else if (r.resp() == CicsResp.ENDFILE) {
                    readLoopExit = true;
                    ca.setNextPageNotExists();
                    setLastKeyFromCardRecord();
                    if (isErrorMsgOff()) {
                        errorMsg = Pic.x(MSG_NO_MORE_RECORDS, 75);
                    }
                    if (ca.screenNum == 1 && scrnCounter == 0) {
                        errorMsg = Pic.x(MSG_NO_RECORDS_FOUND, 75);
                    }
                } else {
                    readLoopExit = true;
                    setFileErrorMessage();
                }
            }
            endBrowse();
        }

        // 9100-READ-BACKWARDS
        private void readBackwards() {
            ca.clearRowsToLowValues();
            ca.setLastCardKey(ca.firstCardNum, ca.firstCardAcctId);
            respCd = cardFile.startBrowseGteq(LIT_CARD_FILE, cardRidCardnum);
            scrnCounter = WS_MAX_SCREEN_LINES + 1;
            ca.setNextPageExists();
            readLoopExit = false;

            CardFileGateway.ReadResult first = cardFile.readPrev(LIT_CARD_FILE);
            setResp(first);
            if (first.isOk()) {
                cardRecord = first.record();
                scrnCounter--;
            } else {
                readLoopExit = true;
                setFileErrorMessage();
                return; // GO TO 9100-READ-BACKWARDS-EXIT (the browse is not ended)
            }

            while (!readLoopExit) {
                CardFileGateway.ReadResult r = cardFile.readPrev(LIT_CARD_FILE);
                setResp(r);
                if (r.isOk()) {
                    cardRecord = r.record();
                    if (!filterRecords()) {
                        ca.setRow(scrnCounter, cardRecord.cardAcctIdDisplay(), cardRecord.cardNum(),
                                cardRecord.cardActiveStatus());
                        scrnCounter--;
                        if (scrnCounter == 0) {
                            readLoopExit = true;
                            ca.setFirstCardKey(cardRecord.cardNum(), cardRecord.cardAcctId());
                        }
                    }
                } else {
                    readLoopExit = true;
                    setFileErrorMessage();
                }
            }
            endBrowse();
        }

        /** 9500-FILTER-RECORDS; returns true when WS-EXCLUDE-THIS-RECORD. */
        private boolean filterRecords() {
            if (editAcctFlag == '1' && !cardRecord.cardAcctIdDisplay().equals(cc.ccAcctId)) {
                return true;
            }
            return editCardFlag == '1' && !cardRecord.cardNum().equals(Pic.nine(cc.ccCardNumN(), 16));
        }

        /** EXEC CICS ENDBR has no RESP(): any condition abends the task. */
        private void endBrowse() {
            int resp = cardFile.endBrowse(LIT_CARD_FILE);
            if (resp != CicsResp.NORMAL) {
                throw new CicsAbendException(resp == CicsResp.INVREQ ? "AEIP" : "RESP" + resp,
                        "ENDBR FILE(" + LIT_CARD_FILE.strip() + ") RESP " + resp);
            }
        }

        private void setResp(CardFileGateway.ReadResult r) {
            respCd = r.resp();
            reasCd = r.resp2();
        }

        private void setLastKeyFromCardRecord() {
            if (cardRecord == null) {
                ca.setLastCardKey(Pic.lowValues(16), 0);
            } else {
                ca.setLastCardKey(cardRecord.cardNum(), cardRecord.cardAcctId());
            }
        }

        /** Builds WS-FILE-ERROR-MESSAGE (80 bytes) and MOVEs it to WS-ERROR-MSG X(75). */
        private void setFileErrorMessage() {
            errorMsg = Pic.x(fileErrorMessage("READ", LIT_CARD_FILE, respCd, reasCd), 75);
        }

        private boolean aid(CcardAid a) {
            return cc.ccardAid == a;
        }

        private boolean fromThisProgram() {
            return commarea.getFromProgram().equals(LIT_THISPGM);
        }

        private boolean isInputError() {
            return inputFlag == '1';
        }

        private boolean isErrorMsgOff() {
            return Pic.isSpaces(errorMsg);
        }

        private boolean isNoInfoMessage() {
            return Pic.isSpaces(infoMsg) || Pic.isLowValues(infoMsg);
        }

        /** VIEW-REQUESTED-ON / UPDATE-REQUESTED-ON (I-SELECTED); subscript 0 never matches. */
        private boolean selected(char code) {
            return iSelected >= 1 && iSelected <= 7 && editSelect[iSelected - 1] == code;
        }

        private void copyInto(ThisProgCommarea from, ThisProgCommarea to) {
            to.lastCardNum = from.lastCardNum;
            to.lastCardAcctId = from.lastCardAcctId;
            to.firstCardNum = from.firstCardNum;
            to.firstCardAcctId = from.firstCardAcctId;
            to.screenNum = from.screenNum;
            to.lastPageDisplayed = from.lastPageDisplayed;
            to.nextPageInd = from.nextPageInd;
            to.returnFlag = from.returnFlag;
            System.arraycopy(from.rowAcctNo, 0, to.rowAcctNo, 0, 7);
            System.arraycopy(from.rowCardNum, 0, to.rowCardNum, 0, 7);
            System.arraycopy(from.rowCardStatus, 0, to.rowCardStatus, 0, 7);
        }

        @Override
        public String toString() {
            return "Task[aid=" + cc.ccardAid + ", select=" + Arrays.toString(editSelect) + "]";
        }
    }

    /**
     * WS-FILE-ERROR-MESSAGE: 'File Error:' X(12), ERROR-OPNAME X(8), ' on ' X(4), ERROR-FILE X(9),
     * ' returned RESP ' X(15), ERROR-RESP X(10), ',RESP2 ' X(7), ERROR-RESP2 X(10), FILLER X(5).
     * RESP values are S9(9) COMP moved to X(10): nine unsigned digits, space padded.
     */
    static String fileErrorMessage(String opname, String file, int resp, int resp2) {
        return Pic.x("File Error:", 12) + Pic.x(opname, 8) + " on " + Pic.x(file, 9) + " returned RESP "
                + Pic.x(Pic.nine(resp, 9), 10) + ",RESP2 " + Pic.x(Pic.nine(resp2, 9), 10) + Pic.spaces(5);
    }
}

package com.carddemo.programs.cocrdslc;

import static com.carddemo.programs.cocrdslc.CobolText.isLowValues;
import static com.carddemo.programs.cocrdslc.CobolText.isSpaces;
import static com.carddemo.programs.cocrdslc.CobolText.pic;

import com.carddemo.programs.cocrdslc.CocrdslcResult.Abended;
import com.carddemo.programs.cocrdslc.CocrdslcResult.ScreenSent;
import com.carddemo.programs.cocrdslc.CocrdslcResult.TextSent;
import com.carddemo.programs.cocrdslc.CocrdslcResult.Xctl;
import com.carddemo.programs.cocrdslc.copybook.AbendData;
import com.carddemo.programs.cocrdslc.copybook.CardRecord;
import com.carddemo.programs.cocrdslc.copybook.CarddemoCommarea;
import com.carddemo.programs.cocrdslc.copybook.CcWorkArea;
import com.carddemo.programs.cocrdslc.copybook.CcWorkArea.CcardAid;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaField;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaInput;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaOutput;
import com.carddemo.programs.cocrdslc.copybook.Dfhbmsca;
import com.carddemo.programs.cocrdslc.copybook.ScreenTitles;
import com.carddemo.programs.cocrdslc.copybook.StorePfKey;
import com.carddemo.programs.cocrdslc.copybook.WsDateTime;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * COCRDSLC - "Accept and process credit card detail request" (transaction CCDL, map CCRDSLA).
 * Framework-free port: each {@link #execute} call is one CICS task of the pseudo-conversation.
 */
public final class Cocrdslc {

    // WS-LITERALS
    public static final String LIT_THISPGM = "COCRDSLC";
    public static final String LIT_THISTRANID = "CCDL";
    public static final String LIT_THISMAPSET = "COCRDSL ";
    public static final String LIT_THISMAP = "CCRDSLA";
    public static final String LIT_CCLISTPGM = "COCRDLIC";
    public static final String LIT_CCLISTTRANID = "CCLI";
    public static final String LIT_CCLISTMAPSET = "COCRDLI";
    public static final String LIT_CCLISTMAP = "CCRDSLA";
    public static final String LIT_MENUPGM = "COMEN01C";
    public static final String LIT_MENUTRANID = "CM00";
    public static final String LIT_MENUMAPSET = "COMEN01";
    public static final String LIT_MENUMAP = "COMEN1A";
    public static final String LIT_CARDFILENAME = "CARDDAT ";
    public static final String LIT_CARDFILENAME_ACCT_PATH = "CARDAIX ";

    /** WS-COMMAREA PIC X(2000). */
    public static final int WS_COMMAREA_LENGTH = 2000;

    // 88-level values of WS-INFO-MSG (PIC X(40))
    public static final String FOUND_CARDS_FOR_ACCOUNT = "   Displaying requested details";
    public static final String WS_PROMPT_FOR_INPUT = "Please enter Account and Card Number";

    // 88-level values of WS-RETURN-MSG (PIC X(75))
    public static final String WS_EXIT_MESSAGE = "PF03 pressed.Exiting              ";
    public static final String WS_PROMPT_FOR_ACCT = "Account number not provided";
    public static final String WS_PROMPT_FOR_CARD = "Card number not provided";
    public static final String NO_SEARCH_CRITERIA_RECEIVED = "No input received";
    public static final String SEARCHED_ACCT_ZEROES = "Account number must be a non zero 11 digit number";
    public static final String SEARCHED_ACCT_NOT_NUMERIC = "Account number must be a non zero 11 digit number";
    public static final String SEARCHED_CARD_NOT_NUMERIC = "Card number if supplied must be a 16 digit number";
    public static final String DID_NOT_FIND_ACCT_IN_CARDXREF = "Did not find this account in cards database";
    public static final String DID_NOT_FIND_ACCTCARD_COMBO = "Did not find cards for this search condition";
    public static final String XREF_READ_ERROR = "Error reading Card Data File";
    public static final String CODING_TO_BE_DONE = "Looks Good.... so far";

    // literals MOVEd to WS-RETURN-MSG
    public static final String MSG_ACCT_FILTER_NOT_NUMERIC = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    public static final String MSG_CARD_FILTER_NOT_NUMERIC = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";
    public static final String MSG_UNEXPECTED_DATA_SCENARIO = "UNEXPECTED DATA SCENARIO";
    public static final String MSG_UNEXPECTED_ABEND = "UNEXPECTED ABEND OCCURRED.";

    private final CardFileGateway cardFile;
    private final Clock clock;

    public Cocrdslc(CardFileGateway cardFile, Clock clock) {
        this.cardFile = Objects.requireNonNull(cardFile);
        this.clock = Objects.requireNonNull(clock);
    }

    public CocrdslcResult execute(CocrdslcRequest request) {
        return newTask(request).run();
    }

    Task newTask(CocrdslcRequest request) {
        return new Task(request);
    }

    /** Signals EXEC CICS RETURN / XCTL: control never comes back to the paragraph that issued it. */
    private static final class CicsReturn extends RuntimeException {
        final transient CocrdslcResult result;

        CicsReturn(CocrdslcResult result) {
            super(null, null, false, false);
            this.result = result;
        }
    }

    /** Working storage + procedure division for a single task. */
    final class Task {
        final CocrdslcRequest req;
        final int eibcalen;

        // WS-MISC-STORAGE (after INITIALIZE: PIC X -> SPACES, PIC 9 -> ZERO)
        String wsTranid = CobolText.spaces(4);
        char wsInputFlag = ' ';
        char wsEditAcctFlag = ' ';
        char wsEditCardFlag = ' ';
        char wsReturnFlag = ' ';
        char wsPfkFlag = ' ';
        String wsCardRidCardnum = CobolText.spaces(16);
        String wsCardRidAcctId = CobolText.zoned(0, 11);
        String errorOpname = CobolText.spaces(8);
        String errorFile = CobolText.spaces(9);
        String errorResp = CobolText.spaces(10);
        String errorResp2 = CobolText.spaces(10);
        String wsLongMsg = CobolText.spaces(500);
        String wsInfoMsg = CobolText.spaces(40);
        String wsReturnMsg = CobolText.spaces(75);

        final CcWorkArea cc = new CcWorkArea();
        CarddemoCommarea commarea = CarddemoCommarea.lowValues();
        ThisProgCommarea thisProg = ThisProgCommarea.lowValues();
        CardRecord cardRecord;
        CcrdslaInput mapIn;
        CcrdslaOutput mapOut = new CcrdslaOutput();
        final WsDateTime wsDateTime = new WsDateTime();
        final AbendData abendData = new AbendData();

        Task(CocrdslcRequest req) {
            this.req = req;
            this.eibcalen = req.eibcalen();
        }

        CocrdslcResult run() {
            // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE)
            try {
                main();
                throw new IllegalStateException("0000-MAIN fell through without a CICS RETURN");
            } catch (CicsReturn r) {
                return r.result;
            } catch (RuntimeException e) {
                return abendRoutine(e);
            }
        }

        // 0000-MAIN
        void main() {
            wsTranid = LIT_THISTRANID;
            setReturnMsgOff();

            // Tested against working storage *before* DFHCOMMAREA is copied in, exactly as the COBOL does.
            if (eibcalen == 0
                    || (commarea.cdemoFromProgram.equals(LIT_MENUPGM) && !commarea.isPgmReenter())) {
                commarea.initialize();
                thisProg.initialize();
            } else {
                commarea = CarddemoCommarea.parse(CobolText.slice(req.commarea(), 0, CarddemoCommarea.LENGTH));
                thisProg = ThisProgCommarea.parse(
                        CobolText.slice(req.commarea(), CarddemoCommarea.LENGTH, ThisProgCommarea.LENGTH));
            }

            StorePfKey.storePfKey(req.eibaid(), cc);

            wsPfkFlag = '1';
            if (cc.is(CcardAid.ENTER) || cc.is(CcardAid.PFK03)) {
                wsPfkFlag = '0';
            }
            if (wsPfkFlag == '1') {
                cc.set(CcardAid.ENTER);
            }

            if (cc.is(CcardAid.PFK03)) {
                xctlToCaller();
            } else if (commarea.isPgmEnter() && commarea.cdemoFromProgram.equals(LIT_CCLISTPGM)) {
                // coming from credit card list screen: selection criteria already validated
                wsInputFlag = '0';
                cc.ccAcctIdN(commarea.acctId());
                cc.ccCardNumN(commarea.cardNum());
                readData9000();
                sendMap1000();
                commonReturn();
            } else if (commarea.isPgmEnter()) {
                sendMap1000();
                commonReturn();
            } else if (commarea.isPgmReenter()) {
                processInputs2000();
                if (isInputError()) {
                    sendMap1000();
                    commonReturn();
                } else {
                    readData9000();
                    sendMap1000();
                    commonReturn();
                }
            } else {
                abendData.abendCulprit(LIT_THISPGM);
                abendData.abendCode("0001");
                abendData.abendReason(" ");
                wsReturnMsg = pic(MSG_UNEXPECTED_DATA_SCENARIO, 75);
                sendPlainText();
            }

            // Unreachable: every branch above ends in XCTL or RETURN. Kept for fidelity.
            if (isInputError()) {
                cc.ccardErrorMsg(wsReturnMsg);
                sendMap1000();
                commonReturn();
            }
        }

        // WHEN CCARD-AID-PFK03 - XCTL to calling program or main menu
        void xctlToCaller() {
            if (isLowValues(commarea.cdemoFromTranid) || isSpaces(commarea.cdemoFromTranid)) {
                commarea.cdemoToTranid = LIT_MENUTRANID;
            } else {
                commarea.cdemoToTranid = commarea.cdemoFromTranid;
            }
            if (isLowValues(commarea.cdemoFromProgram) || isSpaces(commarea.cdemoFromProgram)) {
                commarea.cdemoToProgram = LIT_MENUPGM;
            } else {
                commarea.cdemoToProgram = commarea.cdemoFromProgram;
            }
            commarea.cdemoFromTranid = LIT_THISTRANID;
            commarea.cdemoFromProgram = LIT_THISPGM;
            commarea.setUsrtypUser();
            commarea.setPgmEnter();
            commarea.cdemoLastMapset = pic(LIT_THISMAPSET, 7);
            commarea.cdemoLastMap = pic(LIT_THISMAP, 7);
            throw new CicsReturn(new Xctl(commarea.cdemoToProgram, commarea.copy()));
        }

        // COMMON-RETURN
        void commonReturn() {
            cc.ccardErrorMsg(wsReturnMsg);
            String wsCommarea = pic(commarea.toFixedWidth() + thisProg.toFixedWidth(), WS_COMMAREA_LENGTH);
            throw new CicsReturn(new ScreenSent(mapOut, LIT_THISTRANID, wsCommarea));
        }

        // 1000-SEND-MAP
        void sendMap1000() {
            screenInit1100();
            setupScreenVars1200();
            setupScreenAttrs1300();
            sendScreen1400();
        }

        // 1100-SCREEN-INIT
        void screenInit1100() {
            mapOut = new CcrdslaOutput();
            LocalDateTime now = LocalDateTime.now(clock);
            wsDateTime.setFromCurrentDate(now);
            out(CcrdslaField.TITLE01, ScreenTitles.CCDA_TITLE01);
            out(CcrdslaField.TITLE02, ScreenTitles.CCDA_TITLE02);
            out(CcrdslaField.TRNNAME, LIT_THISTRANID);
            out(CcrdslaField.PGMNAME, LIT_THISPGM);
            wsDateTime.setFromCurrentDate(now);
            out(CcrdslaField.CURDATE, wsDateTime.wsCurdateMmDdYy());
            out(CcrdslaField.CURTIME, wsDateTime.wsCurtimeHhMmSs());
        }

        // 1200-SETUP-SCREEN-VARS
        void setupScreenVars1200() {
            if (eibcalen == 0) {
                wsInfoMsg = pic(WS_PROMPT_FOR_INPUT, 40);
            } else {
                if (commarea.acctId() == 0) {
                    mapOut.field(CcrdslaField.ACCTSID).lowValues();
                } else {
                    out(CcrdslaField.ACCTSID, cc.ccAcctId);
                }
                if (commarea.cardNum() == 0) {
                    mapOut.field(CcrdslaField.CARDSID).lowValues();
                } else {
                    out(CcrdslaField.CARDSID, cc.ccCardNum);
                }
                if (isFoundCardsForAccount()) {
                    out(CcrdslaField.CRDNAME, cardRecord.cardEmbossedName());
                    // CARD-EXPIRAION-DATE-X redefined as YYYY-MM-DD
                    String expiry = cardRecord.cardExpiraionDate();
                    out(CcrdslaField.EXPMON, expiry.substring(5, 7));
                    out(CcrdslaField.EXPYEAR, expiry.substring(0, 4));
                    out(CcrdslaField.CRDSTCD, cardRecord.cardActiveStatus());
                }
            }
            if (isNoInfoMessage()) {
                wsInfoMsg = pic(WS_PROMPT_FOR_INPUT, 40);
            }
            out(CcrdslaField.ERRMSG, wsReturnMsg);
            out(CcrdslaField.INFOMSG, wsInfoMsg);
        }

        // 1300-SETUP-SCREEN-ATTRS
        void setupScreenAttrs1300() {
            boolean fromList = commarea.cdemoLastMapset.equals(LIT_CCLISTMAPSET)
                    && commarea.cdemoFromProgram.equals(LIT_CCLISTPGM);
            if (fromList) {
                mapOut.field(CcrdslaField.ACCTSID).attribute(Dfhbmsca.DFHBMPRF);
                mapOut.field(CcrdslaField.CARDSID).attribute(Dfhbmsca.DFHBMPRF);
            } else {
                mapOut.field(CcrdslaField.ACCTSID).attribute(Dfhbmsca.DFHBMFSE);
                mapOut.field(CcrdslaField.CARDSID).attribute(Dfhbmsca.DFHBMFSE);
            }

            if (isAcctFilterNotOk() || isAcctFilterBlank()) {
                mapOut.field(CcrdslaField.ACCTSID).length((short) -1);
            } else if (isCardFilterNotOk() || isCardFilterBlank()) {
                mapOut.field(CcrdslaField.CARDSID).length((short) -1);
            } else {
                mapOut.field(CcrdslaField.ACCTSID).length((short) -1);
            }

            if (fromList) {
                mapOut.field(CcrdslaField.ACCTSID).color(Dfhbmsca.DFHDFCOL);
                mapOut.field(CcrdslaField.CARDSID).color(Dfhbmsca.DFHDFCOL);
            }
            if (isAcctFilterNotOk()) {
                mapOut.field(CcrdslaField.ACCTSID).color(Dfhbmsca.DFHRED);
            }
            if (isCardFilterNotOk()) {
                mapOut.field(CcrdslaField.CARDSID).color(Dfhbmsca.DFHRED);
            }
            if (isAcctFilterBlank() && commarea.isPgmReenter()) {
                out(CcrdslaField.ACCTSID, "*");
                mapOut.field(CcrdslaField.ACCTSID).color(Dfhbmsca.DFHRED);
            }
            if (isCardFilterBlank() && commarea.isPgmReenter()) {
                out(CcrdslaField.CARDSID, "*");
                mapOut.field(CcrdslaField.CARDSID).color(Dfhbmsca.DFHRED);
            }
            if (isNoInfoMessage()) {
                mapOut.field(CcrdslaField.INFOMSG).color(Dfhbmsca.DFHBMDAR);
            } else {
                mapOut.field(CcrdslaField.INFOMSG).color(Dfhbmsca.DFHNEUTR);
            }
        }

        // 1400-SEND-SCREEN
        void sendScreen1400() {
            cc.ccardNextMapset(LIT_THISMAPSET);
            cc.ccardNextMap(LIT_THISMAP);
            commarea.setPgmReenter();
            // EXEC CICS SEND MAP(CCARD-NEXT-MAP) MAPSET(CCARD-NEXT-MAPSET) FROM(CCRDSLAO) CURSOR ERASE FREEKB
        }

        // 2000-PROCESS-INPUTS
        void processInputs2000() {
            receiveMap2100();
            editMapInputs2200();
            cc.ccardErrorMsg(wsReturnMsg);
            cc.ccardNextProg(LIT_THISPGM);
            cc.ccardNextMapset(LIT_THISMAPSET);
            cc.ccardNextMap(LIT_THISMAP);
        }

        // 2100-RECEIVE-MAP
        void receiveMap2100() {
            mapIn = req.mapOrEmpty();
        }

        // 2200-EDIT-MAP-INPUTS
        void editMapInputs2200() {
            wsInputFlag = '0';
            wsEditCardFlag = '1';
            wsEditAcctFlag = '1';

            String acct = mapIn.acctsidi();
            if (acct.equals(pic("*", 11)) || isSpaces(acct)) {
                cc.ccAcctId = CobolText.lowValues(11);
            } else {
                cc.ccAcctId(acct);
            }
            String card = mapIn.cardsidi();
            if (card.equals(pic("*", 16)) || isSpaces(card)) {
                cc.ccCardNum = CobolText.lowValues(16);
            } else {
                cc.ccCardNum(card);
            }

            editAccount2210();
            editCard2220();

            if (isAcctFilterBlank() && isCardFilterBlank()) {
                wsReturnMsg = pic(NO_SEARCH_CRITERIA_RECEIVED, 75);
            }
        }

        // 2210-EDIT-ACCOUNT
        void editAccount2210() {
            wsEditAcctFlag = '0';
            if (isLowValues(cc.ccAcctId) || isSpaces(cc.ccAcctId) || cc.ccAcctIdN() == 0) {
                wsInputFlag = '1';
                wsEditAcctFlag = ' ';
                if (isReturnMsgOff()) {
                    wsReturnMsg = pic(WS_PROMPT_FOR_ACCT, 75);
                }
                commarea.acctId(0);
                return;
            }
            if (!CobolText.isNumeric(cc.ccAcctId)) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                if (isReturnMsgOff()) {
                    wsReturnMsg = pic(MSG_ACCT_FILTER_NOT_NUMERIC, 75);
                }
                commarea.acctId(0);
            } else {
                commarea.cdemoAcctId = cc.ccAcctId;
                wsEditAcctFlag = '1';
            }
        }

        // 2220-EDIT-CARD
        void editCard2220() {
            wsEditCardFlag = '0';
            if (isLowValues(cc.ccCardNum) || isSpaces(cc.ccCardNum) || cc.ccCardNumN() == 0) {
                wsInputFlag = '1';
                wsEditCardFlag = ' ';
                if (isReturnMsgOff()) {
                    wsReturnMsg = pic(WS_PROMPT_FOR_CARD, 75);
                }
                commarea.cardNum(0);
                return;
            }
            if (!CobolText.isNumeric(cc.ccCardNum)) {
                wsInputFlag = '1';
                wsEditCardFlag = '0';
                if (isReturnMsgOff()) {
                    wsReturnMsg = pic(MSG_CARD_FILTER_NOT_NUMERIC, 75);
                }
                commarea.cardNum(0);
            } else {
                commarea.cardNum(cc.ccCardNumN());
                wsEditCardFlag = '1';
            }
        }

        // 9000-READ-DATA
        void readData9000() {
            getCardByAcctCard9100();
        }

        // 9100-GETCARD-BYACCTCARD (keyed on card number only; the account id is not used)
        void getCardByAcctCard9100() {
            wsCardRidCardnum = cc.ccCardNum;
            CardReadResponse r = cardFile.readCardDat(wsCardRidCardnum);
            if (r.resp() == CicsResp.NORMAL) {
                cardRecord = r.record();
                wsInfoMsg = pic(FOUND_CARDS_FOR_ACCOUNT, 40);
            } else if (r.resp() == CicsResp.NOTFND) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                wsEditCardFlag = '0';
                if (isReturnMsgOff()) {
                    wsReturnMsg = pic(DID_NOT_FIND_ACCTCARD_COMBO, 75);
                }
            } else {
                wsInputFlag = '1';
                if (isReturnMsgOff()) {
                    wsEditAcctFlag = '0';
                }
                fileError("READ", LIT_CARDFILENAME, r);
            }
        }

        // 9150-GETCARD-BYACCT (present in the COBOL but never PERFORMed)
        void getCardByAcct9150() {
            CardReadResponse r = cardFile.readCardAix(wsCardRidAcctId);
            if (r.resp() == CicsResp.NORMAL) {
                cardRecord = r.record();
                wsInfoMsg = pic(FOUND_CARDS_FOR_ACCOUNT, 40);
            } else if (r.resp() == CicsResp.NOTFND) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                wsReturnMsg = pic(DID_NOT_FIND_ACCT_IN_CARDXREF, 75);
            } else {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                fileError("READ", LIT_CARDFILENAME_ACCT_PATH, r);
            }
        }

        private void fileError(String opname, String file, CardReadResponse r) {
            errorOpname = pic(opname, 8);
            errorFile = pic(file, 9);
            errorResp = CobolText.binaryS9_9ToAlpha(r.resp(), 10);
            errorResp2 = CobolText.binaryS9_9ToAlpha(r.resp2(), 10);
            wsReturnMsg = pic(wsFileErrorMessage(), 75);
        }

        /** WS-FILE-ERROR-MESSAGE (80 bytes). */
        String wsFileErrorMessage() {
            return "File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp
                    + ",RESP2 " + errorResp2 + CobolText.spaces(5);
        }

        // SEND-LONG-TEXT (debug only)
        void sendLongText() {
            throw new CicsReturn(new TextSent(wsLongMsg, abendData));
        }

        // SEND-PLAIN-TEXT
        void sendPlainText() {
            throw new CicsReturn(new TextSent(wsReturnMsg, abendData));
        }

        // ABEND-ROUTINE
        CocrdslcResult abendRoutine(Throwable cause) {
            if (isLowValues(abendData.abendMsg())) {
                abendData.abendMsg(MSG_UNEXPECTED_ABEND);
            }
            abendData.abendCulprit(LIT_THISPGM);
            // EXEC CICS SEND FROM(ABEND-DATA) NOHANDLE; HANDLE ABEND CANCEL; ABEND ABCODE('9999')
            return new Abended("9999", abendData, cause);
        }

        private void out(CcrdslaField f, String value) {
            mapOut.field(f).data(value);
        }

        // 88-level predicates
        boolean isInputOk() {
            return wsInputFlag == '0';
        }

        boolean isInputError() {
            return wsInputFlag == '1';
        }

        boolean isAcctFilterNotOk() {
            return wsEditAcctFlag == '0';
        }

        boolean isAcctFilterIsValid() {
            return wsEditAcctFlag == '1';
        }

        boolean isAcctFilterBlank() {
            return wsEditAcctFlag == ' ';
        }

        boolean isCardFilterNotOk() {
            return wsEditCardFlag == '0';
        }

        boolean isCardFilterIsValid() {
            return wsEditCardFlag == '1';
        }

        boolean isCardFilterBlank() {
            return wsEditCardFlag == ' ';
        }

        boolean isReturnMsgOff() {
            return isSpaces(wsReturnMsg);
        }

        void setReturnMsgOff() {
            wsReturnMsg = CobolText.spaces(75);
        }

        boolean isNoInfoMessage() {
            return isSpaces(wsInfoMsg) || isLowValues(wsInfoMsg);
        }

        boolean isFoundCardsForAccount() {
            return wsInfoMsg.equals(pic(FOUND_CARDS_FOR_ACCOUNT, 40));
        }

        String wsReturnMsg() {
            return wsReturnMsg;
        }
    }
}

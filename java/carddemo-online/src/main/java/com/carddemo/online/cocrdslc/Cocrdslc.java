package com.carddemo.online.cocrdslc;

import static com.carddemo.online.cocrdslc.Dfhbmsca.DFHBMDAR;
import static com.carddemo.online.cocrdslc.Dfhbmsca.DFHBMFSE;
import static com.carddemo.online.cocrdslc.Dfhbmsca.DFHBMPRF;
import static com.carddemo.online.cocrdslc.Dfhbmsca.DFHDFCOL;
import static com.carddemo.online.cocrdslc.Dfhbmsca.DFHNEUTR;
import static com.carddemo.online.cocrdslc.Dfhbmsca.DFHRED;

import com.carddemo.common.copybook.AbendData;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardRecord;
import com.carddemo.common.copybook.DateTimeFields;
import com.carddemo.common.copybook.ScreenTitles;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsContext.SendMapOption;
import com.carddemo.runtime.cics.CicsControlTransfer;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cics.PfKeys;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.vsam.FileResult;
import com.carddemo.runtime.vsam.KsdsFile;
import java.math.BigDecimal;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COCRDSLC - card detail view (transaction CCDL, mapset COCRDSL / map CCRDSLA). Reached from the card list
 * (COCRDLIC, 'S' selection) or the main menu (COMEN01C option); reads CARDDAT by card number.
 * Each method is the COBOL paragraph of the same name; line numbers refer to app/cbl/COCRDSLC.cbl.
 */
@Component
public class Cocrdslc implements CicsProgram {

    public static final String PROGRAM = "COCRDSLC";
    public static final String TRANSID = "CCDL";
    /** LIT-THISMAPSET PIC X(8) VALUE 'COCRDSL ' (line 168). */
    public static final String MAPSET = "COCRDSL ";
    public static final String MAP = "CCRDSLA";
    public static final String CCLIST_PROGRAM = "COCRDLIC";
    public static final String CCLIST_TRANID = "CCLI";
    /** LIT-CCLISTMAPSET PIC X(7) VALUE 'COCRDLI' (line 176). */
    public static final String CCLIST_MAPSET = "COCRDLI";
    public static final String MENU_PROGRAM = "COMEN01C";
    public static final String MENU_TRANID = "CM00";
    /** LIT-CARDFILENAME PIC X(8) VALUE 'CARDDAT ' (line 188). */
    public static final String CARD_FILE = "CARDDAT ";
    /** LIT-CARDFILENAME-ACCT-PATH PIC X(8) VALUE 'CARDAIX ' (line 190). */
    public static final String CARD_ACCT_PATH = "CARDAIX ";
    /** WS-COMMAREA PIC X(2000) (line 193): RETURN TRANSID passes all 2000 bytes. */
    public static final int WS_COMMAREA_LENGTH = 2000;
    /** WS-THIS-PROGCOMMAREA (lines 197-199): CA-FROM-PROGRAM X(8) + CA-FROM-TRANID X(4). */
    public static final int PROGCOMMAREA_LENGTH = 12;

    static final String MSG_PF03_EXIT = "PF03 pressed.Exiting ";
    static final String MSG_PROMPT_FOR_ACCT = "Account number not provided";
    static final String MSG_PROMPT_FOR_CARD = "Card number not provided";
    static final String MSG_NO_SEARCH_CRITERIA = "No input received";
    static final String MSG_ACCT_FILTER_NOT_NUMERIC = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    static final String MSG_CARD_FILTER_NOT_NUMERIC = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";
    static final String MSG_DID_NOT_FIND_ACCT_IN_CARDXREF = "Did not find this account in cards database";
    static final String MSG_DID_NOT_FIND_ACCTCARD_COMBO = "Did not find cards for this search condition";
    static final String MSG_UNEXPECTED_DATA_SCENARIO = "UNEXPECTED DATA SCENARIO";
    /** 88 FOUND-CARDS-FOR-ACCOUNT (line 129-130) - note the leading space. */
    static final String INFO_FOUND_CARDS = " Displaying requested details";
    static final String INFO_PROMPT_FOR_INPUT = "Please enter Account and Card Number";

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
        return Optional.of(CardDemoCommarea.LAYOUT);
    }

    @Override
    public void execute(CicsContext ctx) {
        new Task(ctx).mainPara();
    }

    /**
     * Numeric comparison of an unsigned zoned-decimal (PIC 9 DISPLAY) item against ZERO using each byte's digit
     * nibble, as the compiled comparison does when the item holds non-digit bytes (SPACES, LOW-VALUES, letters):
     * '0', ' ', X'00', '@', 'P', 'p' all carry digit 0. Never raises a data exception (see LIMITATIONS).
     */
    static boolean zonedEqualsZero(String raw) {
        return zonedDigitValue(raw) == 0;
    }

    /** Value of a 1-digit unsigned zoned item from its digit nibble (used for CDEMO-PGM-CONTEXT 88-levels). */
    static int zonedDigitValue(String raw) {
        int v = 0;
        for (int i = 0; i < raw.length(); i++) {
            v |= raw.charAt(i) & 0x0F;
        }
        return raw.length() == 1 ? raw.charAt(0) & 0x0F : (v == 0 ? 0 : 1);
    }

    /** Per-task WORKING-STORAGE and paragraphs. */
    static final class Task {
        private final CicsContext ctx;

        // CVCRD01Y CC-WORK-AREAS
        final CardWorkArea ccWorkArea = new CardWorkArea();
        // COCOM01Y CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA
        final CardDemoCommarea commarea = new CardDemoCommarea();
        String progCommarea = PicX.spaces(PROGCOMMAREA_LENGTH);
        // CVACT02Y CARD-RECORD (no VALUE clauses: LOW-VALUES)
        final CardRecord cardRecord = new CardRecord();
        // CSMSG02Y ABEND-DATA (VALUE SPACES)
        final AbendData abendData = new AbendData();
        // COCRDSL CCRDSLAI / CCRDSLAO
        final SymbolicMap ccrdsla = SymbolicMap.of(MAPSET.trim(), MAP);

        // WS-MISC-STORAGE (lines 38-158), state after INITIALIZE (line 255): PIC X -> SPACES, numerics -> 0
        int wsRespCd;
        int wsReasCd;
        String wsTranid = PicX.spaces(4);
        char wsInputFlag = ' ';
        char wsEditAcctFlag = ' ';
        char wsEditCardFlag = ' ';
        char wsPfkFlag = ' ';
        String wsCardRidCardnum = PicX.spaces(16);
        String wsCardRidAcctIdX = "00000000000";
        String wsInfoMsg = PicX.spaces(40);
        String wsReturnMsg = PicX.spaces(75);
        String errorOpname = PicX.spaces(8);
        String errorFile = PicX.spaces(9);
        String errorResp = PicX.spaces(10);
        String errorResp2 = PicX.spaces(10);

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        // ---- 88-levels -------------------------------------------------------------------------------------

        boolean inputError() {
            return wsInputFlag == '1';
        }

        boolean flgAcctfilterNotOk() {
            return wsEditAcctFlag == '0';
        }

        boolean flgAcctfilterBlank() {
            return wsEditAcctFlag == ' ';
        }

        boolean flgCardfilterNotOk() {
            return wsEditCardFlag == '0';
        }

        boolean flgCardfilterBlank() {
            return wsEditCardFlag == ' ';
        }

        boolean wsReturnMsgOff() {
            return PicX.isSpaces(wsReturnMsg);
        }

        boolean wsNoInfoMessage() {
            return PicX.isSpaces(wsInfoMsg) || PicX.isLowValues(wsInfoMsg);
        }

        boolean cdemoPgmEnter() {
            return zonedDigitValue(commarea.pgmContextRaw()) == 0;
        }

        boolean cdemoPgmReenter() {
            return zonedDigitValue(commarea.pgmContextRaw()) == 1;
        }

        private void setReturnMsg(String msg) {
            wsReturnMsg = PicX.of(msg, 75);
        }

        private void setInfoMsg(String msg) {
            wsInfoMsg = PicX.of(msg, 40);
        }

        // ---- paragraphs ------------------------------------------------------------------------------------

        /** 0000-MAIN (lines 248-392), with EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) (lines 250-252). */
        void mainPara() {
            try {
                main0000();
            } catch (RuntimeException e) {
                if (e instanceof CicsControlTransfer) {
                    throw e;
                }
                abendRoutine();
            }
        }

        private void main0000() {
            // INITIALIZE CC-WORK-AREA WS-MISC-STORAGE WS-COMMAREA (lines 254-256): field initialisers above
            wsTranid = TRANSID;
            wsReturnMsg = PicX.spaces(75);

            // lines 268-279. CARDDEMO-COMMAREA is WORKING-STORAGE (line 198) and is not yet populated here, so
            // CDEMO-FROM-PROGRAM is still LOW-VALUES and the menu clause can never be true: only EIBCALEN = 0
            // initialises the commarea (bug-for-bug; see mapping doc).
            if (ctx.eib().eibcalen() == 0 || (MENU_PROGRAM.equals(commarea.fromProgram()) && !cdemoPgmReenter())) {
                initializeCommarea();
                progCommarea = PicX.spaces(PROGCOMMAREA_LENGTH);
            } else {
                commarea.setBytes(ctx.commarea());
                byte[] in = ctx.commarea();
                int start = CardDemoCommarea.LENGTH;
                int len = Math.max(0, Math.min(PROGCOMMAREA_LENGTH, in.length - start));
                progCommarea = PicX.of(len == 0 ? "" : new String(in, start, len, PicX.CHARSET), PROGCOMMAREA_LENGTH);
            }

            // lines 284-299
            CardWorkArea.CCARD_AID.set(ccWorkArea, PfKeys.storePfKey(ctx.eib().eibaid(),
                    ccWorkArea.ccardAid()));
            wsPfkFlag = '1';
            if (ccWorkArea.isCcardAidEnter() || ccWorkArea.isCcardAidPfk03()) {
                wsPfkFlag = '0';
            }
            if (wsPfkFlag == '1') {
                CardWorkArea.CCARD_AID.set(ccWorkArea, "ENTER");
            }

            // lines 304-381
            if (ccWorkArea.isCcardAidPfk03()) {
                String fromTranid = CardDemoCommarea.FROM_TRANID.raw(commarea);
                commarea.setToTranid(PicX.isLowValues(fromTranid) || PicX.isSpaces(fromTranid) ? MENU_TRANID
                        : fromTranid);
                String fromPgm = CardDemoCommarea.FROM_PROGRAM.raw(commarea);
                commarea.setToProgram(PicX.isLowValues(fromPgm) || PicX.isSpaces(fromPgm) ? MENU_PROGRAM : fromPgm);
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                commarea.setUserType("U");
                commarea.setPgmContext(0);
                commarea.setLastMapset(MAPSET);
                commarea.setLastMap(MAP);
                ctx.xctl(CardDemoCommarea.TO_PROGRAM.raw(commarea), commarea);
            } else if (cdemoPgmEnter() && CCLIST_PROGRAM.equals(commarea.fromProgram())) {
                wsInputFlag = '0';
                CardWorkArea.CC_ACCT_ID.set(ccWorkArea, CardDemoCommarea.ACCT_ID.raw(commarea));
                CardWorkArea.CC_CARD_NUM.set(ccWorkArea, CardDemoCommarea.CARD_NUM.raw(commarea));
                readData9000();
                sendMap1000();
                commonReturn();
            } else if (cdemoPgmEnter()) {
                sendMap1000();
                commonReturn();
            } else if (cdemoPgmReenter()) {
                processInputs2000();
                if (inputError()) {
                    sendMap1000();
                    commonReturn();
                } else {
                    readData9000();
                    sendMap1000();
                    commonReturn();
                }
            } else {
                abendData.setCulprit(PROGRAM);
                abendData.setCode("0001");
                abendData.setReason("");
                setReturnMsg(MSG_UNEXPECTED_DATA_SCENARIO);
                sendPlainText();
            }
            // lines 386-391 (IF INPUT-ERROR ... GO TO COMMON-RETURN) are unreachable: every WHEN above ends in
            // XCTL, RETURN or GO TO COMMON-RETURN.
        }

        /** INITIALIZE CARDDEMO-COMMAREA (line 271): PIC X items to SPACES, PIC 9 items to ZERO. */
        private void initializeCommarea() {
            commarea.initializeSpaces();
            CardDemoCommarea.PGM_CONTEXT.set(commarea, 0);
            CardDemoCommarea.CUST_ID.set(commarea, 0);
            CardDemoCommarea.ACCT_ID.set(commarea, 0);
            CardDemoCommarea.CARD_NUM.set(commarea, 0);
        }

        /** COMMON-RETURN (lines 394-407). */
        void commonReturn() {
            CardWorkArea.CCARD_ERROR_MSG.set(ccWorkArea, wsReturnMsg);
            CobolRecord wsCommarea = new CobolRecord(WS_COMMAREA_LENGTH);
            wsCommarea.initializeSpaces();
            wsCommarea.move(commarea.asString() + progCommarea);
            ctx.returnTransid(TRANSID, wsCommarea);
        }

        /** 1000-SEND-MAP (lines 412-421). */
        void sendMap1000() {
            screenInit1100();
            setupScreenVars1200();
            setupScreenAttrs1300();
            sendScreen1400();
        }

        /** 1100-SCREEN-INIT (lines 427-451). */
        void screenInit1100() {
            ccrdsla.lowValues();
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            ccrdsla.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            ccrdsla.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            ccrdsla.setData("TRNNAME", TRANSID);
            ccrdsla.setData("PGMNAME", PROGRAM);
            ccrdsla.setData("CURDATE", dt.curdateMmDdYy());
            ccrdsla.setData("CURTIME", dt.curtimeHhMmSs());
        }

        /** 1200-SETUP-SCREEN-VARS (lines 457-497). */
        void setupScreenVars1200() {
            if (ctx.eib().eibcalen() == 0) {
                setInfoMsg(INFO_PROMPT_FOR_INPUT);
            } else {
                if (zonedEqualsZero(CardDemoCommarea.ACCT_ID.raw(commarea))) {
                    ccrdsla.setData("ACCTSID", PicX.lowValues(11));
                } else {
                    ccrdsla.setData("ACCTSID", ccWorkArea.ccAcctId());
                }
                if (zonedEqualsZero(CardDemoCommarea.CARD_NUM.raw(commarea))) {
                    ccrdsla.setData("CARDSID", PicX.lowValues(16));
                } else {
                    ccrdsla.setData("CARDSID", ccWorkArea.ccCardNum());
                }
                if (INFO_FOUND_CARDS.equals(wsInfoMsg.stripTrailing())) {
                    ccrdsla.setData("CRDNAME", cardRecord.embossedName());
                    // CARD-EXPIRAION-DATE-X REDEFINES: YEAR (1:4), MONTH (6:2) (lines 84-90)
                    String expiry = cardRecord.expiraionDate();
                    ccrdsla.setData("EXPMON", expiry.substring(5, 7));
                    ccrdsla.setData("EXPYEAR", expiry.substring(0, 4));
                    ccrdsla.setData("CRDSTCD", cardRecord.activeStatus());
                }
            }
            if (wsNoInfoMessage()) {
                setInfoMsg(INFO_PROMPT_FOR_INPUT);
            }
            ccrdsla.setData("ERRMSG", wsReturnMsg);
            ccrdsla.setData("INFOMSG", wsInfoMsg);
        }

        private boolean cameFromCardList() {
            return CCLIST_MAPSET.equals(commarea.lastMapset()) && CCLIST_PROGRAM.equals(commarea.fromProgram());
        }

        /** 1300-SETUP-SCREEN-ATTRS (lines 502-558). */
        void setupScreenAttrs1300() {
            if (cameFromCardList()) {
                ccrdsla.setAttribute("ACCTSID", DFHBMPRF);
                ccrdsla.setAttribute("CARDSID", DFHBMPRF);
            } else {
                ccrdsla.setAttribute("ACCTSID", DFHBMFSE);
                ccrdsla.setAttribute("CARDSID", DFHBMFSE);
            }

            if (flgAcctfilterNotOk() || flgAcctfilterBlank()) {
                ccrdsla.setCursor("ACCTSID");
            } else if (flgCardfilterNotOk() || flgCardfilterBlank()) {
                ccrdsla.setCursor("CARDSID");
            } else {
                ccrdsla.setCursor("ACCTSID");
            }

            if (cameFromCardList()) {
                ccrdsla.setColor("ACCTSID", DFHDFCOL);
                ccrdsla.setColor("CARDSID", DFHDFCOL);
            }
            if (flgAcctfilterNotOk()) {
                ccrdsla.setColor("ACCTSID", DFHRED);
            }
            if (flgCardfilterNotOk()) {
                ccrdsla.setColor("CARDSID", DFHRED);
            }
            if (flgAcctfilterBlank() && cdemoPgmReenter()) {
                ccrdsla.setData("ACCTSID", "*");
                ccrdsla.setColor("ACCTSID", DFHRED);
            }
            if (flgCardfilterBlank() && cdemoPgmReenter()) {
                ccrdsla.setData("CARDSID", "*");
                ccrdsla.setColor("CARDSID", DFHRED);
            }
            ccrdsla.setColor("INFOMSG", wsNoInfoMessage() ? DFHBMDAR : DFHNEUTR);
        }

        /** 1400-SEND-SCREEN (lines 563-577). */
        void sendScreen1400() {
            CardWorkArea.CCARD_NEXT_MAPSET.set(ccWorkArea, MAPSET);
            CardWorkArea.CCARD_NEXT_MAP.set(ccWorkArea, MAP);
            commarea.setPgmContext(1);
            wsRespCd = ctx.sendMap(ccrdsla, SendMapOption.CURSOR, SendMapOption.ERASE, SendMapOption.FREEKB).code();
        }

        /** 2000-PROCESS-INPUTS (lines 582-591). */
        void processInputs2000() {
            receiveMap2100();
            editMapInputs2200();
            CardWorkArea.CCARD_ERROR_MSG.set(ccWorkArea, wsReturnMsg);
            CardWorkArea.CCARD_NEXT_PROG.set(ccWorkArea, PROGRAM);
            CardWorkArea.CCARD_NEXT_MAPSET.set(ccWorkArea, MAPSET);
            CardWorkArea.CCARD_NEXT_MAP.set(ccWorkArea, MAP);
        }

        /** 2100-RECEIVE-MAP (lines 596-603). RESP is captured but never tested. */
        void receiveMap2100() {
            CicsResp resp = ctx.receiveMap(ccrdsla);
            wsRespCd = resp.code();
        }

        /** 2200-EDIT-MAP-INPUTS (lines 608-641). */
        void editMapInputs2200() {
            wsInputFlag = '0';
            wsEditCardFlag = '1';
            wsEditAcctFlag = '1';

            String acctsidi = ccrdsla.data("ACCTSID");
            if (PicX.of("*", 11).equals(acctsidi) || PicX.isSpaces(acctsidi)) {
                CardWorkArea.CC_ACCT_ID.fill(ccWorkArea, PicX.LOW_VALUE);
            } else {
                CardWorkArea.CC_ACCT_ID.set(ccWorkArea, acctsidi);
            }
            String cardsidi = ccrdsla.data("CARDSID");
            if (PicX.of("*", 16).equals(cardsidi) || PicX.isSpaces(cardsidi)) {
                CardWorkArea.CC_CARD_NUM.fill(ccWorkArea, PicX.LOW_VALUE);
            } else {
                CardWorkArea.CC_CARD_NUM.set(ccWorkArea, cardsidi);
            }

            editAccount2210();
            editCard2220();

            if (flgAcctfilterBlank() && flgCardfilterBlank()) {
                setReturnMsg(MSG_NO_SEARCH_CRITERIA);
            }
        }

        /** 2210-EDIT-ACCOUNT (lines 647-679). */
        void editAccount2210() {
            wsEditAcctFlag = '0';
            String ccAcctId = CardWorkArea.CC_ACCT_ID.raw(ccWorkArea);
            if (PicX.isLowValues(ccAcctId) || PicX.isSpaces(ccAcctId) || zonedEqualsZero(ccAcctId)) {
                wsInputFlag = '1';
                wsEditAcctFlag = ' ';
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_PROMPT_FOR_ACCT);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, 0);
                return;
            }
            if (!CardWorkArea.CC_ACCT_ID_N.isNumeric(ccWorkArea)) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_ACCT_FILTER_NOT_NUMERIC);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, 0);
                return;
            }
            // MOVE CC-ACCT-ID (X) TO CDEMO-ACCT-ID (9): digits already validated, byte copy
            CardDemoCommarea.ACCT_ID.set(commarea, new BigDecimal(ccAcctId));
            wsEditAcctFlag = '1';
        }

        /** 2220-EDIT-CARD (lines 685-720). */
        void editCard2220() {
            wsEditCardFlag = '0';
            String ccCardNum = CardWorkArea.CC_CARD_NUM.raw(ccWorkArea);
            if (PicX.isLowValues(ccCardNum) || PicX.isSpaces(ccCardNum) || zonedEqualsZero(ccCardNum)) {
                wsInputFlag = '1';
                wsEditCardFlag = ' ';
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_PROMPT_FOR_CARD);
                }
                CardDemoCommarea.CARD_NUM.set(commarea, 0);
                return;
            }
            if (!CardWorkArea.CC_CARD_NUM_N.isNumeric(ccWorkArea)) {
                wsInputFlag = '1';
                wsEditCardFlag = '0';
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_CARD_FILTER_NOT_NUMERIC);
                }
                CardDemoCommarea.CARD_NUM.set(commarea, 0);
                return;
            }
            CardDemoCommarea.CARD_NUM.set(commarea, CardWorkArea.CC_CARD_NUM_N.get(ccWorkArea));
            wsEditCardFlag = '1';
        }

        /** 9000-READ-DATA (lines 726-730). */
        void readData9000() {
            getcardByacctcard9100();
        }

        /** 9100-GETCARD-BYACCTCARD (lines 736-773): READ CARDDAT by card number only (account is not checked). */
        void getcardByacctcard9100() {
            wsCardRidCardnum = CardWorkArea.CC_CARD_NUM.raw(ccWorkArea);
            FileResult result = ctx.read(CARD_FILE, wsCardRidCardnum, CardRecord.LENGTH);
            wsRespCd = result.resp().code();
            wsReasCd = result.resp2();
            if (result.data() != null) {
                cardRecord.setBytes(result.data());
            }
            if (result.resp() == CicsResp.NORMAL) {
                setInfoMsg(INFO_FOUND_CARDS);
            } else if (result.resp() == CicsResp.NOTFND) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                wsEditCardFlag = '0';
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_DID_NOT_FIND_ACCTCARD_COMBO);
                }
            } else {
                wsInputFlag = '1';
                if (wsReturnMsgOff()) {
                    wsEditAcctFlag = '0';
                }
                fileError(CARD_FILE);
            }
        }

        /**
         * 9150-GETCARD-BYACCT (lines 779-809): READ CARDAIX by CARD-ACCT-ID. Never PERFORMed by COCRDSLC (dead code
         * in the COBOL); kept for traceability. WS-CARD-RID-ACCT-ID is never assigned, so the key is its
         * INITIALIZEd value (11 zeros).
         */
        void getcardByacct9150() {
            Optional<KsdsFile> base = ctx.files().ksds(CARD_FILE.trim());
            FileResult result = base.isEmpty() ? FileResult.of(CicsResp.FILENOTFOUND, 1)
                    : ctx.track(base.get().path(CARD_ACCT_PATH.trim(), CardRecord.ACCT_AIX_OFFSET,
                            CardRecord.ACCT_AIX_LENGTH, false).read(wsCardRidAcctIdX));
            wsRespCd = result.resp().code();
            wsReasCd = result.resp2();
            if (result.data() != null) {
                cardRecord.setBytes(result.data());
            }
            if (result.resp() == CicsResp.NORMAL) {
                setInfoMsg(INFO_FOUND_CARDS);
            } else if (result.resp() == CicsResp.NOTFND) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                setReturnMsg(MSG_DID_NOT_FIND_ACCT_IN_CARDXREF);
            } else {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                fileError(CARD_ACCT_PATH);
            }
        }

        /** WS-FILE-ERROR-MESSAGE (lines 102-121) build-up from the WHEN OTHER branches of 9100/9150. */
        private void fileError(String file) {
            errorOpname = PicX.of("READ", 8);
            errorFile = PicX.of(file, 9);
            // MOVE S9(9) COMP TO X(10): unsigned 9-digit DISPLAY image, left-justified, space-padded
            errorResp = PicX.of(String.format("%09d", Math.abs((long) wsRespCd) % 1_000_000_000L), 10);
            errorResp2 = PicX.of(String.format("%09d", Math.abs((long) wsReasCd) % 1_000_000_000L), 10);
            setReturnMsg("File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp
                    + ",RESP2 " + errorResp2 + PicX.spaces(5));
        }

        /** SEND-LONG-TEXT (lines 820-830): never PERFORMed (debug aid). */
        void sendLongText(String wsLongMsg) {
            ctx.sendText(PicX.of(wsLongMsg, 500), 500, true, true);
            ctx.returnToCics();
        }

        /** SEND-PLAIN-TEXT (lines 838-848). */
        void sendPlainText() {
            ctx.sendText(wsReturnMsg, 75, true, true);
            ctx.returnToCics();
        }

        /** ABEND-ROUTINE (lines 857-878). */
        void abendRoutine() {
            if (PicX.isLowValues(abendData.msg())) {
                abendData.setMsg("UNEXPECTED ABEND OCCURRED.");
            }
            abendData.setCulprit(PROGRAM);
            ctx.sendText(abendData.asString(), AbendData.LENGTH, false, false);
            ctx.abend("9999");
        }
    }
}

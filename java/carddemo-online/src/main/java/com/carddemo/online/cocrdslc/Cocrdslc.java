package com.carddemo.online.cocrdslc;

import com.carddemo.common.copybook.AbendData;
import com.carddemo.common.copybook.CardDataSeed;
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
import com.carddemo.runtime.cobol.CobolField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.Ebcdic;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.vsam.FileResult;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COCRDSLC - credit card detail view (transaction CCDL, mapset COCRDSL / map CCRDSLA).
 * Each method is the COBOL paragraph of the same name; line numbers refer to app/cbl/COCRDSLC.cbl.
 */
@Component
public class Cocrdslc implements CicsProgram {

    public static final String PROGRAM = "COCRDSLC";
    public static final String TRANSID = "CCDL";
    public static final String MAPSET = "COCRDSL";
    public static final String MAP = "CCRDSLA";
    public static final String CARD_LIST_PROGRAM = "COCRDLIC";
    public static final String CARD_LIST_TRANSID = "CCLI";
    public static final String CARD_LIST_MAPSET = "COCRDLI";
    public static final String MENU_PROGRAM = "COMEN01C";
    public static final String MENU_TRANSID = "CM00";
    /** RETURN ... COMMAREA(WS-COMMAREA) LENGTH(LENGTH OF WS-COMMAREA) (line 205: PIC X(2000)). */
    public static final int WS_COMMAREA_LENGTH = 2000;
    /** WS-THIS-PROGCOMMAREA: CA-FROM-PROGRAM X(8) + CA-FROM-TRANID X(4) (lines 200-203). */
    public static final int THIS_PROGCOMMAREA_LENGTH = 12;

    // WS-LITERALS (lines 162-190)
    static final String LIT_THISPGM = PicX.of(PROGRAM, 8);
    static final String LIT_THISTRANID = PicX.of(TRANSID, 4);
    static final String LIT_THISMAPSET = PicX.of(MAPSET, 8);
    static final String LIT_THISMAP = PicX.of(MAP, 7);
    static final String LIT_CCLISTPGM = PicX.of(CARD_LIST_PROGRAM, 8);
    static final String LIT_CCLISTMAPSET = PicX.of(CARD_LIST_MAPSET, 7);
    static final String LIT_MENUPGM = PicX.of(MENU_PROGRAM, 8);
    static final String LIT_MENUTRANID = PicX.of(MENU_TRANSID, 4);
    static final String LIT_CARDFILENAME = PicX.of(CardDataSeed.FILE_NAME, 8);
    static final String LIT_CARDFILENAME_ACCT_PATH = PicX.of(CardDataSeed.AIX_PATH_NAME, 8);

    // WS-INFO-MSG 88-levels (lines 126-132)
    static final String FOUND_CARDS_FOR_ACCOUNT = "   Displaying requested details";
    static final String WS_PROMPT_FOR_INPUT = "Please enter Account and Card Number";
    // WS-RETURN-MSG 88-levels (lines 134-158)
    static final String WS_PROMPT_FOR_ACCT = "Account number not provided";
    static final String WS_PROMPT_FOR_CARD = "Card number not provided";
    static final String NO_SEARCH_CRITERIA_RECEIVED = "No input received";
    static final String DID_NOT_FIND_ACCT_IN_CARDXREF = "Did not find this account in cards database";
    static final String DID_NOT_FIND_ACCTCARD_COMBO = "Did not find cards for this search condition";
    static final String MSG_ACCT_FILTER = "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER";
    static final String MSG_CARD_FILTER = "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER";
    static final String MSG_UNEXPECTED_DATA = "UNEXPECTED DATA SCENARIO";
    static final String MSG_UNEXPECTED_ABEND = "UNEXPECTED ABEND OCCURRED.";

    // DFHBMSCA values (3270 attribute / colour bytes, EBCDIC)
    static final int DFHBMPRF = 0x61;
    static final int DFHBMFSE = 0xC1;
    static final int DFHBMDAR = 0x4C;
    static final int DFHDFCOL = 0x00;
    static final int DFHRED = 0xF2;
    static final int DFHNEUTR = 0xF7;

    // flag values (lines 51-68)
    static final String FLG_NOT_OK = "0";
    static final String FLG_ISVALID = "1";
    static final String FLG_BLANK = " ";
    static final String INPUT_OK = "0";
    static final String INPUT_ERROR = "1";

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
        new Task(ctx).run();
    }

    /**
     * Value of a USAGE DISPLAY unsigned numeric item as a numeric comparison sees it on z/OS (PACK + sign
     * fix-up): each byte contributes its low nibble (EBCDIC), so SPACES and LOW-VALUES compare equal to zero.
     * Returns {@code null} when a nibble is not a decimal digit (the compare would be a data exception);
     * callers treat that as "not equal".
     */
    static Long displayValue(String raw) {
        byte[] ebcdic = Ebcdic.encode(raw);
        long v = 0;
        for (byte b : ebcdic) {
            int nibble = b & 0x0F;
            if (nibble > 9) {
                return null;
            }
            v = v * 10 + nibble;
        }
        return v;
    }

    static boolean displayEqualsZero(String raw) {
        Long v = displayValue(raw);
        return v != null && v == 0L;
    }

    /** {@code IF alnum IS NUMERIC} for a PIC X item: every character is a digit. */
    static boolean isNumericText(String value) {
        return !value.isEmpty() && value.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    /** INITIALIZE of a record: alphanumeric items to SPACES, numeric items to ZERO. */
    static void initialize(CobolRecord record, RecordLayout layout) {
        record.initializeSpaces();
        for (CobolField f : layout.fields()) {
            if (f instanceof NumericField n) {
                n.set(record, 0L);
            }
        }
    }

    /** One task's WORKING-STORAGE (lines 35-240) plus the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // WS-CICS-PROCESSNG-VARS (lines 40-46)
        int wsRespCd;
        int wsReasCd;
        String wsTranid = PicX.spaces(4);
        // input edit flags (lines 51-68), INITIALIZEd to SPACES at line 254
        String wsInputFlag = PicX.spaces(1);
        String wsEditAcctFlag = PicX.spaces(1);
        String wsEditCardFlag = PicX.spaces(1);
        String wsReturnFlag = PicX.spaces(1);
        String wsPfkFlag = PicX.spaces(1);
        // CICS-OUTPUT-EDIT-VARS (lines 72-92): only CARD-EXPIRAION-DATE-X is used
        String cardExpiraionDateX = PicX.spaces(10);
        // WS-CARD-RID (lines 97-101)
        String wsCardRidCardnum = PicX.spaces(16);
        String wsCardRidAcctId = "00000000000";
        // WS-FILE-ERROR-MESSAGE named items (lines 102-121); FILLERs keep their VALUEs
        String errorOpname = PicX.spaces(8);
        String errorFile = PicX.spaces(9);
        String errorResp = PicX.spaces(10);
        String errorResp2 = PicX.spaces(10);
        // output messages (lines 125-158)
        String wsLongMsg = PicX.spaces(500);
        String wsInfoMsg = PicX.spaces(40);
        String wsReturnMsg = PicX.spaces(75);

        // CVCRD01Y CC-WORK-AREA (INITIALIZEd to SPACES)
        String ccardAid = PicX.spaces(5);
        String ccardNextProg = PicX.spaces(8);
        String ccardNextMapset = PicX.spaces(7);
        String ccardNextMap = PicX.spaces(7);
        String ccardErrorMsg = PicX.spaces(75);
        String ccardReturnMsg = PicX.spaces(75);
        String ccAcctId = PicX.spaces(11);
        String ccCardNum = PicX.spaces(16);
        String ccCustId = PicX.spaces(9);

        // COCOM01Y / WS-THIS-PROGCOMMAREA / COCRDSL / CSMSG02Y / CVACT02Y
        CardDemoCommarea commarea = new CardDemoCommarea();
        String wsThisProgcommarea = PicX.lowValues(THIS_PROGCOMMAREA_LENGTH);
        final SymbolicMap ccrdsla = SymbolicMap.of(MAPSET, MAP);
        final AbendData abendData = new AbendData();
        final CardRecord cardRecord = new CardRecord();

        /** Colour bytes moved to {@code xxxC OF CCRDSLAO} (the symbolic map does not report them). */
        final Map<String, Integer> colors = new LinkedHashMap<>();

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        /** EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) (lines 250-252) around 0000-MAIN. */
        void run() {
            try {
                mainPara();
            } catch (CicsControlTransfer transfer) {
                throw transfer;
            } catch (NumberFormatException | ArithmeticException | IndexOutOfBoundsException programCheck) {
                // program checks (S0C7 data exception, S0C9, addressing) are routed to the HANDLE ABEND label
                abendRoutine();
            }
        }

        boolean inputError() {
            return INPUT_ERROR.equals(wsInputFlag);
        }

        boolean wsReturnMsgOff() {
            return PicX.isSpaces(wsReturnMsg);
        }

        boolean wsNoInfoMessage() {
            return PicX.isSpacesOrLowValues(wsInfoMsg);
        }

        boolean pgmEnter() {
            Long v = displayValue(CardDemoCommarea.PGM_CONTEXT.raw(commarea));
            return v != null && v == 0L;
        }

        boolean pgmReenter() {
            Long v = displayValue(CardDemoCommarea.PGM_CONTEXT.raw(commarea));
            return v != null && v == 1L;
        }

        void setReturnMsg(String text) {
            wsReturnMsg = PicX.of(text, 75);
        }

        void setInfoMsg(String text) {
            wsInfoMsg = PicX.of(text, 40);
        }

        /** WS-FILE-ERROR-MESSAGE (lines 102-121), 80 bytes. */
        String wsFileErrorMessage() {
            return "File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp + ",RESP2 "
                    + errorResp2 + PicX.spaces(5);
        }

        /** MOVE S9(9) COMP TO PIC X(10): 9 unsigned digits, left-justified. */
        static String binaryToAlnum(int value) {
            return PicX.of(String.format("%09d", Math.abs((long) value) % 1_000_000_000L), 10);
        }

        void setColor(String field, int color) {
            ccrdsla.setColor(field, color);
            colors.put(field, color);
        }

        /** 0000-MAIN (lines 248-392) and COMMON-RETURN (lines 394-407). */
        void mainPara() {
            wsTranid = LIT_THISTRANID;
            wsReturnMsg = PicX.spaces(75);

            // Bug-for-bug: CDEMO-FROM-PROGRAM / CDEMO-PGM-CONTEXT are tested on the WORKING-STORAGE commarea
            // before DFHCOMMAREA is copied into it, so the second operand only matters when EIBCALEN = 0.
            if (ctx.eib().eibcalen() == 0
                    || (PicX.equal(commarea.fromProgram(), LIT_MENUPGM) && !pgmReenter())) {
                initialize(commarea, CardDemoCommarea.LAYOUT);
                wsThisProgcommarea = PicX.spaces(THIS_PROGCOMMAREA_LENGTH);
            } else {
                byte[] in = ctx.commarea();
                commarea = CardDemoCommarea.of(Arrays.copyOf(in, Math.min(in.length, CardDemoCommarea.LENGTH)));
                byte[] rest = in.length > CardDemoCommarea.LENGTH
                        ? Arrays.copyOfRange(in, CardDemoCommarea.LENGTH,
                                Math.min(in.length, CardDemoCommarea.LENGTH + THIS_PROGCOMMAREA_LENGTH))
                        : new byte[0];
                wsThisProgcommarea = PicX.of(PicX.fromBytes(rest), THIS_PROGCOMMAREA_LENGTH);
            }

            yyyyStorePfkey();

            wsPfkFlag = "1";
            if (ccardAid.equals("ENTER") || ccardAid.equals("PFK03")) {
                wsPfkFlag = "0";
            }
            if (wsPfkFlag.equals("1")) {
                ccardAid = "ENTER";
            }

            if (ccardAid.equals("PFK03")) {
                xctlToCallerOrMenu();
            } else if (pgmEnter() && PicX.equal(commarea.fromProgram(), LIT_CCLISTPGM)) {
                wsInputFlag = INPUT_OK;
                ccAcctId = moveDisplayNumeric(CardDemoCommarea.ACCT_ID.raw(commarea), 11);
                ccCardNum = moveDisplayNumeric(CardDemoCommarea.CARD_NUM.raw(commarea), 16);
                readData9000();
                sendMap1000();
                commonReturn();
            } else if (pgmEnter()) {
                sendMap1000();
                commonReturn();
            } else if (pgmReenter()) {
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
                abendData.setCulprit(LIT_THISPGM);
                abendData.setCode("0001");
                abendData.setReason("");
                setReturnMsg(MSG_UNEXPECTED_DATA);
                sendPlainText();
            }

            if (inputError()) {
                ccardErrorMsg = wsReturnMsg;
                sendMap1000();
            }
            commonReturn();
        }

        /** MOVE 9(n) DISPLAY TO 9(n) DISPLAY (CC-xxx-N): digits normalised as by PACK/UNPK. */
        static String moveDisplayNumeric(String raw, int digits) {
            Long v = displayValue(raw);
            return v == null ? PicX.of(raw, digits) : String.format("%0" + digits + "d", v);
        }

        /** EVALUATE WHEN CCARD-AID-PFK03 (lines 305-334): XCTL to the calling program or the main menu. */
        void xctlToCallerOrMenu() {
            String fromTranid = commarea.fromTranid();
            commarea.setToTranid(PicX.isLowValues(fromTranid) || PicX.isSpaces(fromTranid) ? LIT_MENUTRANID : fromTranid);
            String fromProgram = commarea.fromProgram();
            commarea.setToProgram(PicX.isLowValues(fromProgram) || PicX.isSpaces(fromProgram) ? LIT_MENUPGM : fromProgram);
            commarea.setFromTranid(LIT_THISTRANID);
            commarea.setFromProgram(LIT_THISPGM);
            CardDemoCommarea.USRTYP_USER.set(commarea);
            CardDemoCommarea.PGM_ENTER.set(commarea);
            commarea.setLastMapset(LIT_THISMAPSET);
            commarea.setLastMap(LIT_THISMAP);
            ctx.xctl(commarea.toProgram(), commarea);
        }

        /** COMMON-RETURN (lines 394-407). */
        void commonReturn() {
            ccardErrorMsg = wsReturnMsg;
            CobolRecord wsCommarea = new CobolRecord(WS_COMMAREA_LENGTH);
            wsCommarea.initializeSpaces();
            String all = wsCommarea.asString();
            all = commarea.asString() + wsThisProgcommarea
                    + all.substring(CardDemoCommarea.LENGTH + THIS_PROGCOMMAREA_LENGTH);
            ctx.returnTransid(LIT_THISTRANID, CobolRecord.ofBytes(PicX.toBytes(all)));
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
            colors.clear();
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            ccrdsla.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            ccrdsla.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            ccrdsla.setData("TRNNAME", LIT_THISTRANID);
            ccrdsla.setData("PGMNAME", LIT_THISPGM);
            ccrdsla.setData("CURDATE", dt.curdateMmDdYy());
            ccrdsla.setData("CURTIME", dt.curtimeHhMmSs());
        }

        /** 1200-SETUP-SCREEN-VARS (lines 457-497). */
        void setupScreenVars1200() {
            if (ctx.eib().eibcalen() == 0) {
                setInfoMsg(WS_PROMPT_FOR_INPUT);
            } else {
                if (displayEqualsZero(CardDemoCommarea.ACCT_ID.raw(commarea))) {
                    ccrdsla.setData("ACCTSID", PicX.lowValues(11));
                } else {
                    ccrdsla.setData("ACCTSID", ccAcctId);
                }
                if (displayEqualsZero(CardDemoCommarea.CARD_NUM.raw(commarea))) {
                    ccrdsla.setData("CARDSID", PicX.lowValues(16));
                } else {
                    ccrdsla.setData("CARDSID", ccCardNum);
                }
                if (PicX.equal(wsInfoMsg, FOUND_CARDS_FOR_ACCOUNT)) {
                    ccrdsla.setData("CRDNAME", cardRecord.embossedName());
                    cardExpiraionDateX = cardRecord.expirationDate();
                    ccrdsla.setData("EXPMON", cardExpiraionDateX.substring(5, 7));
                    ccrdsla.setData("EXPYEAR", cardExpiraionDateX.substring(0, 4));
                    ccrdsla.setData("CRDSTCD", cardRecord.activeStatus());
                }
            }

            if (wsNoInfoMessage()) {
                setInfoMsg(WS_PROMPT_FOR_INPUT);
            }
            ccrdsla.setData("ERRMSG", wsReturnMsg);
            ccrdsla.setData("INFOMSG", wsInfoMsg);
        }

        boolean cameFromCardList() {
            return PicX.equal(commarea.lastMapset(), LIT_CCLISTMAPSET) && PicX.equal(commarea.fromProgram(), LIT_CCLISTPGM);
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

            if (FLG_NOT_OK.equals(wsEditAcctFlag) || FLG_BLANK.equals(wsEditAcctFlag)) {
                ccrdsla.setCursor("ACCTSID");
            } else if (FLG_NOT_OK.equals(wsEditCardFlag) || FLG_BLANK.equals(wsEditCardFlag)) {
                ccrdsla.setCursor("CARDSID");
            } else {
                ccrdsla.setCursor("ACCTSID");
            }

            if (cameFromCardList()) {
                setColor("ACCTSID", DFHDFCOL);
                setColor("CARDSID", DFHDFCOL);
            }
            if (FLG_NOT_OK.equals(wsEditAcctFlag)) {
                setColor("ACCTSID", DFHRED);
            }
            if (FLG_NOT_OK.equals(wsEditCardFlag)) {
                setColor("CARDSID", DFHRED);
            }
            if (FLG_BLANK.equals(wsEditAcctFlag) && pgmReenter()) {
                ccrdsla.setData("ACCTSID", "*");
                setColor("ACCTSID", DFHRED);
            }
            if (FLG_BLANK.equals(wsEditCardFlag) && pgmReenter()) {
                ccrdsla.setData("CARDSID", "*");
                setColor("CARDSID", DFHRED);
            }
            // Bug-for-bug: DFHBMDAR is an attribute value, moved here into the colour byte.
            setColor("INFOMSG", wsNoInfoMessage() ? DFHBMDAR : DFHNEUTR);
        }

        /** 1400-SEND-SCREEN (lines 563-577). */
        void sendScreen1400() {
            ccardNextMapset = PicX.of(LIT_THISMAPSET, 7);
            ccardNextMap = LIT_THISMAP;
            CardDemoCommarea.PGM_REENTER.set(commarea);
            wsRespCd = ctx.sendMap(ccrdsla, SendMapOption.CURSOR, SendMapOption.ERASE, SendMapOption.FREEKB).code();
        }

        /** 2000-PROCESS-INPUTS (lines 582-591). */
        void processInputs2000() {
            receiveMap2100();
            editMapInputs2200();
            ccardErrorMsg = wsReturnMsg;
            ccardNextProg = LIT_THISPGM;
            ccardNextMapset = PicX.of(LIT_THISMAPSET, 7);
            ccardNextMap = LIT_THISMAP;
        }

        /** 2100-RECEIVE-MAP (lines 596-603). MAPFAIL is ignored: the map stays LOW-VALUES. */
        void receiveMap2100() {
            CicsResp resp = ctx.receiveMap(ccrdsla);
            wsRespCd = resp.code();
            wsReasCd = ctx.eib().eibresp2();
        }

        /** 2200-EDIT-MAP-INPUTS (lines 608-641). */
        void editMapInputs2200() {
            wsInputFlag = INPUT_OK;
            wsEditCardFlag = FLG_ISVALID;
            wsEditAcctFlag = FLG_ISVALID;

            String acctsidi = ccrdsla.data("ACCTSID");
            ccAcctId = PicX.equal(acctsidi, "*") || PicX.isSpaces(acctsidi) ? PicX.lowValues(11) : acctsidi;
            String cardsidi = ccrdsla.data("CARDSID");
            ccCardNum = PicX.equal(cardsidi, "*") || PicX.isSpaces(cardsidi) ? PicX.lowValues(16) : cardsidi;

            editAccount2210();
            editCard2220();

            if (FLG_BLANK.equals(wsEditAcctFlag) && FLG_BLANK.equals(wsEditCardFlag)) {
                setReturnMsg(NO_SEARCH_CRITERIA_RECEIVED);
            }
        }

        /** 2210-EDIT-ACCOUNT (lines 647-679). */
        void editAccount2210() {
            wsEditAcctFlag = FLG_NOT_OK;
            if (PicX.isLowValues(ccAcctId) || PicX.isSpaces(ccAcctId) || displayEqualsZero(ccAcctId)) {
                wsInputFlag = INPUT_ERROR;
                wsEditAcctFlag = FLG_BLANK;
                if (wsReturnMsgOff()) {
                    setReturnMsg(WS_PROMPT_FOR_ACCT);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, 0L);
                return;
            }
            if (!isNumericText(ccAcctId)) {
                wsInputFlag = INPUT_ERROR;
                wsEditAcctFlag = FLG_NOT_OK;
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_ACCT_FILTER);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, 0L);
            } else {
                CardDemoCommarea.ACCT_ID.set(commarea, Long.parseLong(ccAcctId));
                wsEditAcctFlag = FLG_ISVALID;
            }
        }

        /** 2220-EDIT-CARD (lines 685-720). */
        void editCard2220() {
            wsEditCardFlag = FLG_NOT_OK;
            if (PicX.isLowValues(ccCardNum) || PicX.isSpaces(ccCardNum) || displayEqualsZero(ccCardNum)) {
                wsInputFlag = INPUT_ERROR;
                wsEditCardFlag = FLG_BLANK;
                if (wsReturnMsgOff()) {
                    setReturnMsg(WS_PROMPT_FOR_CARD);
                }
                CardDemoCommarea.CARD_NUM.set(commarea, 0L);
                return;
            }
            if (!isNumericText(ccCardNum)) {
                wsInputFlag = INPUT_ERROR;
                wsEditCardFlag = FLG_NOT_OK;
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_CARD_FILTER);
                }
                CardDemoCommarea.CARD_NUM.set(commarea, 0L);
            } else {
                CardDemoCommarea.CARD_NUM.set(commarea, Long.parseLong(ccCardNum));
                wsEditCardFlag = FLG_ISVALID;
            }
        }

        /** 9000-READ-DATA (lines 726-730). */
        void readData9000() {
            getcardByacctcard9100();
        }

        /** 9100-GETCARD-BYACCTCARD (lines 736-773): READ CARDDAT by card number only. */
        void getcardByacctcard9100() {
            wsCardRidCardnum = ccCardNum;
            FileResult r = ctx.read(LIT_CARDFILENAME, wsCardRidCardnum, CardRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null) {
                cardRecord.setBytes(r.data());
            }
            if (r.resp() == CicsResp.NORMAL) {
                setInfoMsg(FOUND_CARDS_FOR_ACCOUNT);
            } else if (r.resp() == CicsResp.NOTFND) {
                wsInputFlag = INPUT_ERROR;
                wsEditAcctFlag = FLG_NOT_OK;
                wsEditCardFlag = FLG_NOT_OK;
                if (wsReturnMsgOff()) {
                    setReturnMsg(DID_NOT_FIND_ACCTCARD_COMBO);
                }
            } else {
                wsInputFlag = INPUT_ERROR;
                if (wsReturnMsgOff()) {
                    wsEditAcctFlag = FLG_NOT_OK;
                }
                fileError(LIT_CARDFILENAME);
            }
        }

        /**
         * 9150-GETCARD-BYACCT (lines 779-809): READ CARDAIX by account. Never PERFORMed by the COBOL (dead code);
         * kept for completeness. The PATH is opened over the CARDDAT cluster registered in file control.
         */
        void getcardByacct9150() {
            FileResult r = ctx.track(ctx.files().ksds(CardDataSeed.FILE_NAME)
                    .map(f -> f.path(CardDataSeed.AIX_PATH_NAME, CardRecord.ACCT_ID_OFFSET, CardRecord.ACCT_ID_LENGTH, false)
                            .read(wsCardRidAcctId))
                    .orElse(FileResult.of(CicsResp.FILENOTFOUND, 1)));
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null) {
                cardRecord.setBytes(r.data());
            }
            if (r.resp() == CicsResp.NORMAL) {
                setInfoMsg(FOUND_CARDS_FOR_ACCOUNT);
            } else if (r.resp() == CicsResp.NOTFND) {
                wsInputFlag = INPUT_ERROR;
                wsEditAcctFlag = FLG_NOT_OK;
                setReturnMsg(DID_NOT_FIND_ACCT_IN_CARDXREF);
            } else {
                wsInputFlag = INPUT_ERROR;
                wsEditAcctFlag = FLG_NOT_OK;
                fileError(LIT_CARDFILENAME_ACCT_PATH);
            }
        }

        private void fileError(String file) {
            errorOpname = PicX.of("READ", 8);
            errorFile = PicX.of(file, 9);
            errorResp = binaryToAlnum(wsRespCd);
            errorResp2 = binaryToAlnum(wsReasCd);
            setReturnMsg(wsFileErrorMessage());
        }

        /** SEND-LONG-TEXT (lines 820-830): debugging aid, never PERFORMed. */
        void sendLongText() {
            ctx.sendText(wsLongMsg, wsLongMsg.length(), true, true);
            ctx.returnToCics();
        }

        /** SEND-PLAIN-TEXT (lines 838-848). */
        void sendPlainText() {
            ctx.sendText(wsReturnMsg, wsReturnMsg.length(), true, true);
            ctx.returnToCics();
        }

        /** CSSTRPFY YYYY-STORE-PFKEY (line 855). */
        void yyyyStorePfkey() {
            String aid = PfKeys.storePfKey(ctx.eib().eibaid());
            if (aid != null) {
                ccardAid = aid;
            }
        }

        /** ABEND-ROUTINE (lines 857-878). */
        void abendRoutine() {
            if (PicX.isLowValues(abendData.msg())) {
                abendData.setMsg(MSG_UNEXPECTED_ABEND);
            }
            abendData.setCulprit(LIT_THISPGM);
            // EXEC CICS SEND FROM(ABEND-DATA) NOHANDLE: unformatted terminal write (no ERASE)
            ctx.sendText(abendData.asString(), AbendData.LENGTH, false, false);
            ctx.abend("9999");
        }
    }
}

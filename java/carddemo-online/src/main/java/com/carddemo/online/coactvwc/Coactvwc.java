package com.carddemo.online.coactvwc;

import com.carddemo.common.copybook.AbendData;
import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.common.copybook.DateTimeFields;
import com.carddemo.common.copybook.ScreenTitles;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsContext.SendMapOption;
import com.carddemo.runtime.cics.CicsControlTransfer;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cics.PfKeys;
import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericEdited;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.vsam.FileResult;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Locale;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COACTVWC - account view (transaction CAVW, mapset COACTVW / map CACTVWA). Reads the card cross-reference
 * through the CXACAIX alternate-index path, then ACCTDAT and CUSTDAT, and shows account + customer details.
 * Each method is the COBOL paragraph of the same name; line numbers refer to app/cbl/COACTVWC.cbl.
 */
@Component
public class Coactvwc implements CicsProgram {

    public static final String PROGRAM = "COACTVWC";
    public static final String TRANSID = "CAVW";
    public static final String MAPSET = "COACTVW";
    public static final String MAP = "CACTVWA";
    public static final String MENU_PROGRAM = "COMEN01C";
    public static final String MENU_TRANSID = "CM00";

    /** Length of WS-COMMAREA (line 218), passed on every RETURN TRANSID. */
    public static final int WS_COMMAREA_LENGTH = 2000;

    // WS-LITERALS (lines 142-202)
    static final String LIT_THISPGM = PicX.of(PROGRAM, 8);
    static final String LIT_THISTRANID = PicX.of(TRANSID, 4);
    static final String LIT_THISMAPSET = PicX.of("COACTVW ", 8);
    static final String LIT_THISMAP = PicX.of(MAP, 7);
    static final String LIT_MENUPGM = PicX.of(MENU_PROGRAM, 8);
    static final String LIT_MENUTRANID = PicX.of(MENU_TRANSID, 4);
    static final String LIT_ACCTFILENAME = PicX.of("ACCTDAT ", 8);
    static final String LIT_CUSTFILENAME = PicX.of("CUSTDAT ", 8);
    static final String LIT_CARDXREFNAME_ACCT_PATH = PicX.of("CXACAIX ", 8);

    // WS-INFO-MSG / WS-RETURN-MSG 88-levels (lines 110-139)
    static final String WS_PROMPT_FOR_INPUT = "Enter or update id of account to display";
    static final String WS_INFORM_OUTPUT = "Displaying details of given Account";
    static final String WS_PROMPT_FOR_ACCT = "Account number not provided";
    static final String NO_SEARCH_CRITERIA_RECEIVED = "No input received";
    static final String MSG_ACCT_FILTER_INVALID = "Account Filter must  be a non-zero 11 digit number";
    static final String UNEXPECTED_DATA_SCENARIO = "UNEXPECTED DATA SCENARIO";
    static final String UNEXPECTED_ABEND = "UNEXPECTED ABEND OCCURRED.";

    /** BMS PICOUT of ACRDLIM/ACSHLIM/ACURBAL/ACRCYCR/ACRCYDB (symbolic map COACTVW.CPY, e.g. line 326). */
    static final String AMOUNT_PICOUT = "+ZZZ,ZZZ,ZZZ.99";

    /** WS-THIS-PROGCOMMAREA (lines 213-216). */
    static final RecordLayout THIS_PROGCOMMAREA;
    static final AlphanumericField CA_FROM_PROGRAM;
    static final AlphanumericField CA_FROM_TRANID;

    static {
        RecordLayout.Builder b = RecordLayout.builder("WS-THIS-PROGCOMMAREA");
        b.group("CA-CALL-CONTEXT");
        CA_FROM_PROGRAM = b.x("CA-FROM-PROGRAM", 8);
        CA_FROM_TRANID = b.x("CA-FROM-TRANID", 4);
        b.endGroup();
        THIS_PROGCOMMAREA = b.build();
    }

    @Override
    public String programName() {
        return PROGRAM;
    }

    @Override
    public Optional<String> transactionId() {
        return Optional.of(TRANSID);
    }

    /** The first 160 bytes of the 2000-byte WS-COMMAREA are CARDDEMO-COMMAREA. */
    @Override
    public Optional<RecordLayout> commareaLayout() {
        return Optional.of(CardDemoCommarea.LAYOUT);
    }

    @Override
    public void execute(CicsContext ctx) {
        Task task = new Task(ctx);
        try {
            task.main();
        } catch (CicsControlTransfer transfer) {
            throw transfer;
        } catch (IllegalArgumentException invalidRequest) {
            if (!(invalidRequest instanceof NumberFormatException)) {
                // not a task failure: e.g. terminal input naming a field the map does not have (REST 400)
                throw invalidRequest;
            }
            task.abendRoutine();
        } catch (RuntimeException unexpected) {
            // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) (lines 264-266): e.g. a data exception (S0C7)
            task.abendRoutine();
        }
    }

    /** INITIALIZE of a record: alphanumeric items to SPACES, numeric items to ZERO; FILLER untouched. */
    static void initialize(RecordLayout layout, CobolRecord record) {
        for (CobolField f : layout.fields()) {
            if (f instanceof AlphanumericField a) {
                a.set(record, "");
            } else if (f instanceof NumericField n) {
                n.set(record, BigDecimal.ZERO);
            }
        }
    }

    /** MOVE binary RESP/RESP2 (S9(09) COMP) TO PIC X(10): the 9 digits unsigned, left-justified. */
    static String respText(int value) {
        return PicX.of(String.format(Locale.ROOT, "%09d", Math.abs((long) value) % 1_000_000_000L), 10);
    }

    /** Alphanumeric class test {@code IS NUMERIC}: every character is a digit 0-9. */
    static boolean isNumericText(String value) {
        if (value.isEmpty()) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    /**
     * MOVE zoned S9(10)V99 TO numeric-edited. LOW-VALUES (an ACCOUNT-RECORD that was never read, see
     * 9000-READ-ACCT) edit as zero, as PACK + ED of X'00' digits do on z/Architecture; any other invalid
     * content throws (data exception) and is handled by ABEND-ROUTINE.
     */
    static String editAmount(NumericField field, CobolRecord record) {
        if (field.isLowValues(record)) {
            return NumericEdited.format(BigDecimal.ZERO, AMOUNT_PICOUT);
        }
        return NumericEdited.format(field.get(record), AMOUNT_PICOUT);
    }

    /** One task's WORKING-STORAGE (lines 35-254) plus the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // WS-CICS-PROCESSNG-VARS (lines 39-45)
        int wsRespCd;
        int wsReasCd;
        String wsTranid = PicX.spaces(4);
        // Input edits (lines 50-65)
        char wsInputFlag = PicX.LOW_VALUE;
        char wsPfkFlag = PicX.LOW_VALUE;
        char wsEditAcctFlag = PicX.LOW_VALUE;
        char wsEditCustFlag = PicX.LOW_VALUE;
        // WS-XREF-RID (lines 73-80)
        String wsCardRidCardnum = PicX.lowValues(16);
        String wsCardRidCustId = PicX.lowValues(9);
        String wsCardRidAcctId = PicX.lowValues(11);
        // WS-FILE-READ-FLAGS (lines 81-85)
        char wsAccountMasterReadFlag = PicX.LOW_VALUE;
        char wsCustMasterReadFlag = PicX.LOW_VALUE;
        // WS-FILE-ERROR-MESSAGE variable parts (lines 86-105); the FILLERs are constants
        String errorOpname = PicX.spaces(8);
        String errorFile = PicX.spaces(9);
        String errorResp = PicX.spaces(10);
        String errorResp2 = PicX.spaces(10);
        // Output message construction (lines 109-139)
        String wsLongMsg = PicX.lowValues(500);
        String wsInfoMsg = PicX.lowValues(40);
        String wsReturnMsg = PicX.lowValues(75);

        // CVCRD01Y CC-WORK-AREA (program-private copybook)
        String ccardAid = PicX.lowValues(5);
        String ccardNextProg = PicX.lowValues(8);
        String ccardNextMapset = PicX.lowValues(7);
        String ccardNextMap = PicX.lowValues(7);
        String ccardErrorMsg = PicX.lowValues(75);
        String ccardReturnMsg = PicX.lowValues(75);
        String ccAcctId = PicX.spaces(11);
        String ccCardNum = PicX.spaces(16);
        String ccCustId = PicX.spaces(9);

        // COCOM01Y, WS-THIS-PROGCOMMAREA, WS-COMMAREA, COACTVW, CSMSG02Y, CVACT01Y, CVACT03Y, CVCUS01Y
        final CardDemoCommarea commarea = new CardDemoCommarea();
        final CobolRecord thisProgCommarea = new CobolRecord(THIS_PROGCOMMAREA.length());
        final CobolRecord wsCommarea = new CobolRecord(WS_COMMAREA_LENGTH);
        final SymbolicMap cactvwa = SymbolicMap.of(MAPSET, MAP);
        final AbendData abendData = new AbendData();
        final AccountRecord accountRecord = new AccountRecord();
        final CardXrefRecord cardXrefRecord = new CardXrefRecord();
        final CustomerRecord customerRecord = new CustomerRecord();

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        // ------------------------------------------------------------- 88-levels

        boolean inputError() {
            return wsInputFlag == '1';
        }

        boolean flgAcctfilterNotOk() {
            return wsEditAcctFlag == '0';
        }

        boolean flgAcctfilterBlank() {
            return wsEditAcctFlag == ' ';
        }

        boolean foundAcctInMaster() {
            return wsAccountMasterReadFlag == '1';
        }

        boolean foundCustInMaster() {
            return wsCustMasterReadFlag == '1';
        }

        boolean wsNoInfoMessage() {
            return PicX.isSpacesOrLowValues(wsInfoMsg);
        }

        boolean wsReturnMsgOff() {
            return PicX.isSpaces(wsReturnMsg);
        }

        boolean ccardAidIs(String value) {
            return PicX.equal(ccardAid, value);
        }

        void setReturnMsg(String text) {
            wsReturnMsg = PicX.of(text, 75);
        }

        /** WS-FILE-ERROR-MESSAGE (lines 86-105) as a whole. */
        String wsFileErrorMessage() {
            return "File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp + ",RESP2 "
                    + errorResp2 + PicX.spaces(5);
        }

        // ------------------------------------------------------------- 0000-MAIN (lines 262-407)

        void main() {
            // INITIALIZE CC-WORK-AREA WS-MISC-STORAGE WS-COMMAREA (lines 268-270)
            ccardAid = PicX.spaces(5);
            ccardNextProg = PicX.spaces(8);
            ccardNextMapset = PicX.spaces(7);
            ccardNextMap = PicX.spaces(7);
            ccardErrorMsg = PicX.spaces(75);
            ccardReturnMsg = PicX.spaces(75);
            ccAcctId = PicX.spaces(11);
            ccCardNum = PicX.spaces(16);
            ccCustId = PicX.spaces(9);
            wsRespCd = 0;
            wsReasCd = 0;
            wsTranid = PicX.spaces(4);
            wsInputFlag = ' ';
            wsPfkFlag = ' ';
            wsEditAcctFlag = ' ';
            wsEditCustFlag = ' ';
            wsCardRidCardnum = PicX.spaces(16);
            wsCardRidCustId = "000000000";
            wsCardRidAcctId = "00000000000";
            wsAccountMasterReadFlag = ' ';
            wsCustMasterReadFlag = ' ';
            errorOpname = PicX.spaces(8);
            errorFile = PicX.spaces(9);
            errorResp = PicX.spaces(10);
            errorResp2 = PicX.spaces(10);
            wsLongMsg = PicX.spaces(500);
            wsInfoMsg = PicX.spaces(40);
            wsReturnMsg = PicX.spaces(75);
            wsCommarea.initializeSpaces();

            wsTranid = LIT_THISTRANID;
            wsReturnMsg = PicX.spaces(75);

            // lines 282-293. CARDDEMO-COMMAREA is tested *before* DFHCOMMAREA is copied into it, i.e. on this
            // task's uninitialised WORKING-STORAGE (LOW-VALUES), so the COMEN01C test is never true.
            if (ctx.eib().eibcalen() == 0
                    || (PicX.equal(commarea.fromProgram(), LIT_MENUPGM) && !commarea.isPgmReenter())) {
                initialize(CardDemoCommarea.LAYOUT, commarea);
                initialize(THIS_PROGCOMMAREA, thisProgCommarea);
            } else {
                byte[] dfh = ctx.commarea();
                commarea.setBytes(Arrays.copyOfRange(dfh, 0, Math.min(dfh.length, CardDemoCommarea.LENGTH)));
                int from = Math.min(dfh.length, CardDemoCommarea.LENGTH);
                int to = Math.min(dfh.length, CardDemoCommarea.LENGTH + THIS_PROGCOMMAREA.length());
                thisProgCommarea.setBytes(Arrays.copyOfRange(dfh, from, to));
            }

            yyyyStorePfkey();

            // lines 306-314
            wsPfkFlag = '1';
            if (ccardAidIs("ENTER") || ccardAidIs("PFK03")) {
                wsPfkFlag = '0';
            }
            if (wsPfkFlag == '1') {
                ccardAid = "ENTER";
            }

            // lines 323-383
            if (ccardAidIs("PFK03")) {
                if (PicX.isLowValues(commarea.fromTranid()) || PicX.isSpaces(commarea.fromTranid())) {
                    commarea.setToTranid(LIT_MENUTRANID);
                } else {
                    commarea.setToTranid(commarea.fromTranid());
                }
                if (PicX.isLowValues(commarea.fromProgram()) || PicX.isSpaces(commarea.fromProgram())) {
                    commarea.setToProgram(LIT_MENUPGM);
                } else {
                    commarea.setToProgram(commarea.fromProgram());
                }
                commarea.setFromTranid(LIT_THISTRANID);
                commarea.setFromProgram(LIT_THISPGM);
                CardDemoCommarea.USRTYP_USER.set(commarea);
                CardDemoCommarea.PGM_ENTER.set(commarea);
                commarea.setLastMapset(LIT_THISMAPSET);
                commarea.setLastMap(LIT_THISMAP);
                ctx.xctl(commarea.toProgram(), commarea);
            } else if (commarea.isPgmEnter()) {
                sendMap1000();
                commonReturn();
            } else if (commarea.isPgmReenter()) {
                processInputs2000();
                if (inputError()) {
                    sendMap1000();
                    commonReturn();
                } else {
                    readAcct9000();
                    sendMap1000();
                    commonReturn();
                }
            } else {
                AbendData.ABEND_CULPRIT.set(abendData, LIT_THISPGM);
                AbendData.ABEND_CODE.set(abendData, "0001");
                AbendData.ABEND_REASON.set(abendData, "");
                setReturnMsg(UNEXPECTED_DATA_SCENARIO);
                sendPlainText();
            }

            // lines 387-392 (unreachable: every WHEN above ends the task or goes to COMMON-RETURN)
            if (inputError()) {
                ccardErrorMsg = wsReturnMsg;
                sendMap1000();
            }
            commonReturn();
        }

        /** COMMON-RETURN (lines 394-407). */
        void commonReturn() {
            ccardErrorMsg = wsReturnMsg;
            wsCommarea.move(commarea);
            byte[] all = wsCommarea.toBytes();
            byte[] ca = thisProgCommarea.toBytes();
            System.arraycopy(ca, 0, all, CardDemoCommarea.LENGTH, ca.length);
            CobolRecord out = CobolRecord.ofBytes(all);
            ctx.returnTransid(LIT_THISTRANID, out);
        }

        // ------------------------------------------------------------- screen

        /** 1000-SEND-MAP (lines 416-425). */
        void sendMap1000() {
            screenInit1100();
            setupScreenVars1200();
            setupScreenAttrs1300();
            sendScreen1400();
        }

        /** 1100-SCREEN-INIT (lines 431-456). */
        void screenInit1100() {
            cactvwa.lowValues();
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            cactvwa.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            cactvwa.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            cactvwa.setData("TRNNAME", LIT_THISTRANID);
            cactvwa.setData("PGMNAME", LIT_THISPGM);
            cactvwa.setData("CURDATE", dt.curdateMmDdYy());
            cactvwa.setData("CURTIME", dt.curtimeHhMmSs());
        }

        /** 1200-SETUP-SCREEN-VARS (lines 460-536). */
        void setupScreenVars1200() {
            if (ctx.eib().eibcalen() == 0) {
                wsInfoMsg = PicX.of(WS_PROMPT_FOR_INPUT, 40);
            } else {
                if (flgAcctfilterBlank()) {
                    cactvwa.setData("ACCTSID", PicX.lowValues(11));
                } else {
                    cactvwa.setData("ACCTSID", ccAcctId);
                }

                if (foundAcctInMaster() || foundCustInMaster()) {
                    cactvwa.setData("ACSTTUS", AccountRecord.ACTIVE_STATUS.get(accountRecord));
                    cactvwa.setData("ACURBAL", editAmount(AccountRecord.CURR_BAL, accountRecord));
                    cactvwa.setData("ACRDLIM", editAmount(AccountRecord.CREDIT_LIMIT, accountRecord));
                    cactvwa.setData("ACSHLIM", editAmount(AccountRecord.CASH_CREDIT_LIMIT, accountRecord));
                    cactvwa.setData("ACRCYCR", editAmount(AccountRecord.CURR_CYC_CREDIT, accountRecord));
                    cactvwa.setData("ACRCYDB", editAmount(AccountRecord.CURR_CYC_DEBIT, accountRecord));
                    cactvwa.setData("ADTOPEN", AccountRecord.OPEN_DATE.get(accountRecord));
                    cactvwa.setData("AEXPDT", AccountRecord.EXPIRAION_DATE.get(accountRecord));
                    cactvwa.setData("AREISDT", AccountRecord.REISSUE_DATE.get(accountRecord));
                    cactvwa.setData("AADDGRP", AccountRecord.GROUP_ID.get(accountRecord));
                }

                if (foundCustInMaster()) {
                    cactvwa.setData("ACSTNUM", CustomerRecord.CUST_ID.raw(customerRecord));
                    // STRING ... DELIMITED BY SIZE INTO ACSTSSNO (lines 496-504): 11 bytes into X(12), byte 12 keeps
                    // its LOW-VALUE from 1100-SCREEN-INIT.
                    String ssn = CustomerRecord.SSN.raw(customerRecord);
                    cactvwa.setDataPrefix("ACSTSSN", ssn.substring(0, 3) + "-" + ssn.substring(3, 5) + "-"
                            + ssn.substring(5, 9));
                    cactvwa.setData("ACSTFCO", CustomerRecord.FICO_CREDIT_SCORE.raw(customerRecord));
                    cactvwa.setData("ACSTDOB", CustomerRecord.DOB_YYYY_MM_DD.get(customerRecord));
                    cactvwa.setData("ACSFNAM", CustomerRecord.FIRST_NAME.get(customerRecord));
                    cactvwa.setData("ACSMNAM", CustomerRecord.MIDDLE_NAME.get(customerRecord));
                    cactvwa.setData("ACSLNAM", CustomerRecord.LAST_NAME.get(customerRecord));
                    cactvwa.setData("ACSADL1", CustomerRecord.ADDR_LINE_1.get(customerRecord));
                    cactvwa.setData("ACSADL2", CustomerRecord.ADDR_LINE_2.get(customerRecord));
                    cactvwa.setData("ACSCITY", CustomerRecord.ADDR_LINE_3.get(customerRecord));
                    cactvwa.setData("ACSSTTE", CustomerRecord.ADDR_STATE_CD.get(customerRecord));
                    cactvwa.setData("ACSZIPC", CustomerRecord.ADDR_ZIP.get(customerRecord));
                    cactvwa.setData("ACSCTRY", CustomerRecord.ADDR_COUNTRY_CD.get(customerRecord));
                    cactvwa.setData("ACSPHN1", CustomerRecord.PHONE_NUM_1.get(customerRecord));
                    cactvwa.setData("ACSPHN2", CustomerRecord.PHONE_NUM_2.get(customerRecord));
                    cactvwa.setData("ACSGOVT", CustomerRecord.GOVT_ISSUED_ID.get(customerRecord));
                    cactvwa.setData("ACSEFTC", CustomerRecord.EFT_ACCOUNT_ID.get(customerRecord));
                    cactvwa.setData("ACSPFLG", CustomerRecord.PRI_CARD_HOLDER_IND.get(customerRecord));
                }
            }

            if (wsNoInfoMessage()) {
                wsInfoMsg = PicX.of(WS_PROMPT_FOR_INPUT, 40);
            }
            cactvwa.setData("ERRMSG", wsReturnMsg);
            cactvwa.setData("INFOMSG", wsInfoMsg);
        }

        /** 1300-SETUP-SCREEN-ATTRS (lines 541-573). */
        void setupScreenAttrs1300() {
            cactvwa.setAttribute("ACCTSID", Dfhbmsca.DFHBMFSE);
            // EVALUATE (lines 546-552): both branches MOVE -1 TO ACCTSIDL
            cactvwa.setCursor("ACCTSID");
            cactvwa.setColor("ACCTSID", Dfhbmsca.DFHDFCOL);
            if (flgAcctfilterNotOk()) {
                cactvwa.setColor("ACCTSID", Dfhbmsca.DFHRED);
            }
            if (flgAcctfilterBlank() && commarea.isPgmReenter()) {
                cactvwa.setData("ACCTSID", "*");
                cactvwa.setColor("ACCTSID", Dfhbmsca.DFHRED);
            }
            if (wsNoInfoMessage()) {
                cactvwa.setColor("INFOMSG", Dfhbmsca.DFHBMDAR);
            } else {
                cactvwa.setColor("INFOMSG", Dfhbmsca.DFHNEUTR);
            }
        }

        /** 1400-SEND-SCREEN (lines 577-591). */
        void sendScreen1400() {
            ccardNextMapset = PicX.of(LIT_THISMAPSET, 7);
            ccardNextMap = LIT_THISMAP;
            CardDemoCommarea.PGM_REENTER.set(commarea);
            wsRespCd = ctx.sendMap(cactvwa, SendMapOption.ERASE, SendMapOption.CURSOR, SendMapOption.FREEKB).code();
        }

        // ------------------------------------------------------------- input

        /** 2000-PROCESS-INPUTS (lines 596-606). */
        void processInputs2000() {
            receiveMap2100();
            editMapInputs2200();
            ccardErrorMsg = wsReturnMsg;
            ccardNextProg = LIT_THISPGM;
            ccardNextMapset = PicX.of(LIT_THISMAPSET, 7);
            ccardNextMap = LIT_THISMAP;
        }

        /** 2100-RECEIVE-MAP (lines 610-618). */
        void receiveMap2100() {
            CicsResp resp = ctx.receiveMap(cactvwa);
            wsRespCd = resp.code();
            wsReasCd = ctx.eib().eibresp2();
        }

        /** 2200-EDIT-MAP-INPUTS (lines 622-644). */
        void editMapInputs2200() {
            wsInputFlag = '0';
            wsEditAcctFlag = '1';

            String acctsidi = cactvwa.data("ACCTSID");
            if (PicX.equal(acctsidi, "*") || PicX.isSpaces(acctsidi)) {
                ccAcctId = PicX.lowValues(11);
            } else {
                ccAcctId = PicX.of(acctsidi, 11);
            }

            editAccount2210();

            if (flgAcctfilterBlank()) {
                setReturnMsg(NO_SEARCH_CRITERIA_RECEIVED);
            }
        }

        /** 2210-EDIT-ACCOUNT (lines 649-682). */
        void editAccount2210() {
            wsEditAcctFlag = '0';

            if (PicX.isLowValues(ccAcctId) || PicX.isSpaces(ccAcctId)) {
                wsInputFlag = '1';
                wsEditAcctFlag = ' ';
                if (wsReturnMsgOff()) {
                    setReturnMsg(WS_PROMPT_FOR_ACCT);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, BigDecimal.ZERO);
                return;
            }

            if (!isNumericText(ccAcctId) || ccAcctId.equals("00000000000")) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                if (wsReturnMsgOff()) {
                    setReturnMsg(MSG_ACCT_FILTER_INVALID);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, BigDecimal.ZERO);
            } else {
                CardDemoCommarea.ACCT_ID.set(commarea, new BigDecimal(ccAcctId));
                wsEditAcctFlag = '1';
            }
        }

        // ------------------------------------------------------------- file access

        /** 9000-READ-ACCT (lines 687-719). */
        void readAcct9000() {
            wsInfoMsg = PicX.spaces(40);
            wsCardRidAcctId = CardDemoCommarea.ACCT_ID.raw(commarea);

            getCardXrefByAcct9200();
            if (flgAcctfilterNotOk()) {
                return;
            }

            getAcctDataByAcct9300();
            // DID-NOT-FIND-ACCT-IN-ACCTDAT is never SET (line 792 is commented out): this test is never true
            if (PicX.equal(wsReturnMsg, "Did not find this account in account master file")) {
                return;
            }

            wsCardRidCustId = CardDemoCommarea.CUST_ID.raw(commarea);
            getCustDataByCust9400();
            // DID-NOT-FIND-CUST-IN-CUSTDAT is never SET either (line 842 is commented out)
            if (PicX.equal(wsReturnMsg, "Did not find associated customer in master file")) {
                return;
            }
        }

        /** 9200-GETCARDXREF-BYACCT (lines 723-770). */
        void getCardXrefByAcct9200() {
            FileResult r = ctx.read(LIT_CARDXREFNAME_ACCT_PATH, wsCardRidAcctId, CardXrefRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            intoRecord(r, cardXrefRecord);

            if (r.resp() == CicsResp.NORMAL) {
                CardDemoCommarea.CUST_ID.set(commarea, CardXrefRecord.CUST_ID.get(cardXrefRecord));
                moveAlphanumericToNumeric(CardXrefRecord.CARD_NUM.get(cardXrefRecord), CardDemoCommarea.CARD_NUM);
            } else if (r.resp() == CicsResp.NOTFND) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                if (wsReturnMsgOff()) {
                    errorResp = respText(wsRespCd);
                    errorResp2 = respText(wsReasCd);
                    setReturnMsg("Account:" + wsCardRidAcctId + " not found in" + " Cross ref file.  Resp:"
                            + errorResp + " Reas:" + errorResp2);
                }
            } else {
                fileError(LIT_CARDXREFNAME_ACCT_PATH);
            }
        }

        /** 9300-GETACCTDATA-BYACCT (lines 774-820). */
        void getAcctDataByAcct9300() {
            FileResult r = ctx.read(LIT_ACCTFILENAME, wsCardRidAcctId, AccountRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            intoRecord(r, accountRecord);

            if (r.resp() == CicsResp.NORMAL) {
                wsAccountMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                if (wsReturnMsgOff()) {
                    errorResp = respText(wsRespCd);
                    errorResp2 = respText(wsReasCd);
                    setReturnMsg("Account:" + wsCardRidAcctId + " not found in" + " Acct Master file.Resp:"
                            + errorResp + " Reas:" + errorResp2);
                }
            } else {
                fileError(LIT_ACCTFILENAME);
            }
        }

        /** 9400-GETCUSTDATA-BYCUST (lines 825-869). */
        void getCustDataByCust9400() {
            FileResult r = ctx.read(LIT_CUSTFILENAME, wsCardRidCustId, CustomerRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            intoRecord(r, customerRecord);

            if (r.resp() == CicsResp.NORMAL) {
                wsCustMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                wsInputFlag = '1';
                wsEditCustFlag = '0';
                errorResp = respText(wsRespCd);
                errorResp2 = respText(wsReasCd);
                if (wsReturnMsgOff()) {
                    setReturnMsg("CustId:" + wsCardRidCustId + " not found" + " in customer master.Resp: "
                            + errorResp + " REAS:" + errorResp2);
                }
            } else {
                wsInputFlag = '1';
                wsEditCustFlag = '0';
                errorOpname = PicX.of("READ", 8);
                errorFile = PicX.of(LIT_CUSTFILENAME, 9);
                errorResp = respText(wsRespCd);
                errorResp2 = respText(wsReasCd);
                setReturnMsg(wsFileErrorMessage());
            }
        }

        /** WHEN OTHER of 9200/9300: INPUT-ERROR, FLG-ACCTFILTER-NOT-OK, WS-FILE-ERROR-MESSAGE TO WS-RETURN-MSG. */
        private void fileError(String fileName) {
            wsInputFlag = '1';
            wsEditAcctFlag = '0';
            errorOpname = PicX.of("READ", 8);
            errorFile = PicX.of(fileName, 9);
            errorResp = respText(wsRespCd);
            errorResp2 = respText(wsReasCd);
            setReturnMsg(wsFileErrorMessage());
        }

        /** READ ... INTO: the area is only changed when CICS returns data (NORMAL, DUPKEY, LENGERR). */
        private static void intoRecord(FileResult r, CobolRecord into) {
            if (r.data() != null) {
                byte[] bytes = into.toBytes();
                System.arraycopy(r.data(), 0, bytes, 0, Math.min(r.data().length, bytes.length));
                into.setBytes(bytes);
            }
        }

        /** MOVE PIC X TO unsigned PIC 9 DISPLAY integer of the same length: the bytes are copied as they are. */
        private void moveAlphanumericToNumeric(String value, NumericField target) {
            if (isNumericText(value) && value.length() == target.length()) {
                target.set(commarea, new BigDecimal(value));
            } else {
                byte[] bytes = commarea.toBytes();
                byte[] src = PicX.toBytes(PicX.of(value, target.length()));
                System.arraycopy(src, 0, bytes, target.offset(), target.length());
                commarea.setBytes(bytes);
            }
        }

        // ------------------------------------------------------------- exits

        /** SEND-PLAIN-TEXT (lines 877-890). */
        void sendPlainText() {
            ctx.sendText(wsReturnMsg, 75, true, true);
            ctx.returnToCics();
        }

        /** SEND-LONG-TEXT (lines 896-909). Debug exit; no PERFORM reaches it (lines 768, 818, 867 are comments). */
        void sendLongText() {
            ctx.sendText(wsLongMsg, 500, true, true);
            ctx.returnToCics();
        }

        /** YYYY-STORE-PFKEY (CSSTRPFY, line 913). */
        void yyyyStorePfkey() {
            ccardAid = PfKeys.storePfKey(ctx.eib().eibaid(), ccardAid);
        }

        /** ABEND-ROUTINE (lines 916-937). */
        void abendRoutine() {
            if (PicX.isLowValues(AbendData.ABEND_MSG.get(abendData))) {
                AbendData.ABEND_MSG.set(abendData, UNEXPECTED_ABEND);
            }
            AbendData.ABEND_CULPRIT.set(abendData, LIT_THISPGM);
            // EXEC CICS SEND FROM(ABEND-DATA) LENGTH(134) NOHANDLE (no ERASE / FREEKB)
            ctx.sendText(abendData.asString(), AbendData.LENGTH, false, false);
            // EXEC CICS HANDLE ABEND CANCEL + EXEC CICS ABEND ABCODE('9999')
            ctx.abend("9999");
        }
    }
}

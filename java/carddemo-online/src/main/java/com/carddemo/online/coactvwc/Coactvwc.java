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
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericEdited;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.cobol.ZonedDecimal;
import com.carddemo.runtime.vsam.FileResult;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COACTVWC - CardDemo account view (transaction CAVW, mapset COACTVW / map CACTVWA). Reads the card
 * cross-reference through the CXACAIX alternate-index path, then ACCTDAT and CUSTDAT, and shows the account
 * and customer details. Each method is the COBOL paragraph of the same name; line numbers refer to
 * app/cbl/COACTVWC.cbl.
 */
@Component
public class Coactvwc implements CicsProgram {

    public static final String PROGRAM = "COACTVWC";
    public static final String TRANSID = "CAVW";
    public static final String MAPSET = "COACTVW";
    public static final String MAP = "CACTVWA";
    public static final String MENU_PROGRAM = "COMEN01C";
    public static final String MENU_TRANSID = "CM00";
    public static final String ACCT_FILE = "ACCTDAT";
    public static final String CUST_FILE = "CUSTDAT";
    public static final String XREF_ACCT_PATH = "CXACAIX";

    /** WS-COMMAREA PIC X(2000): what RETURN TRANSID passes back (COMMON-RETURN lines 396-404). */
    public static final int WS_COMMAREA_LENGTH = 2000;
    /** WS-THIS-PROGCOMMAREA: CA-FROM-PROGRAM X(8) + CA-FROM-TRANID X(4) (lines 251-254). */
    public static final int PROGCOMMAREA_LENGTH = 12;

    static final String MSG_PROMPT_FOR_INPUT = "Enter or update id of account to display";
    static final String MSG_PROMPT_FOR_ACCT = "Account number not provided";
    static final String MSG_NO_SEARCH_CRITERIA = "No input received";
    static final String MSG_ACCT_FILTER_INVALID = "Account Filter must  be a non-zero 11 digit number";
    static final String MSG_DID_NOT_FIND_ACCT_IN_ACCTDAT = "Did not find this account in account master file";
    static final String MSG_UNEXPECTED_DATA = "UNEXPECTED DATA SCENARIO";

    /** DFHBMSCA values (EBCDIC bytes). */
    static final int DFHBMFSE = 0xC8;
    static final int DFHDFCOL = 0x00;
    static final int DFHRED = 0xF2;
    static final int DFHNEUTR = 0xF7;
    static final int DFHBMDAR = 0x4C;

    /** BMS PICOUT of ACURBAL/ACRDLIM/ACSHLIM/ACRCYCR/ACRCYDB (app/bms/COACTVW.bms). */
    static final String MONEY_PICOUT = "+ZZZ,ZZZ,ZZZ.99";

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
        Task task = new Task(ctx);
        try {
            task.main0000();
        } catch (CicsControlTransfer transfer) {
            throw transfer;
        } catch (RuntimeException e) {
            // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) (lines 263-265)
            task.abendRoutine();
        }
    }

    /** One task's WORKING-STORAGE (lines 40-255) plus the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // CC-WORK-AREA (CVCRD01Y): only the items COACTVWC uses
        String ccardAid = PicX.lowValues(5);
        String ccAcctId = PicX.lowValues(11);

        // WS-MISC-STORAGE (lines 40-157)
        int wsRespCd;
        int wsReasCd;
        String wsTranid = PicX.spaces(4);
        String wsInputFlag = PicX.lowValues(1);
        String wsPfkFlag = PicX.lowValues(1);
        String wsEditAcctFlag = PicX.lowValues(1);
        String wsEditCustFlag = PicX.lowValues(1);
        String wsCardRidAcctIdX = PicX.lowValues(11);
        String wsCardRidCustIdX = PicX.lowValues(9);
        String wsAccountMasterReadFlag = PicX.lowValues(1);
        String wsCustMasterReadFlag = PicX.lowValues(1);
        String errorOpname = PicX.spaces(8);
        String errorFile = PicX.spaces(9);
        String errorResp = PicX.spaces(10);
        String errorResp2 = PicX.spaces(10);
        String wsInfoMsg = PicX.lowValues(40);
        String wsReturnMsg = PicX.lowValues(75);

        // COPY COCOM01Y / COACTVW / CVACT01Y / CVACT03Y / CVCUS01Y / CSMSG02Y
        final CardDemoCommarea commarea = new CardDemoCommarea();
        String wsThisProgcommarea = PicX.lowValues(PROGCOMMAREA_LENGTH);
        final SymbolicMap cactvwa = SymbolicMap.of(MAPSET, MAP);
        final AccountRecord accountRecord = new AccountRecord();
        final CardXrefRecord cardXrefRecord = new CardXrefRecord();
        final CustomerRecord customerRecord = new CustomerRecord();
        final AbendData abendData = AbendData.initial();

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        boolean inputError() {
            return "1".equals(wsInputFlag);
        }

        boolean acctFilterNotOk() {
            return "0".equals(wsEditAcctFlag);
        }

        boolean acctFilterBlank() {
            return " ".equals(wsEditAcctFlag);
        }

        boolean returnMsgOff() {
            return PicX.isSpaces(wsReturnMsg);
        }

        boolean noInfoMessage() {
            return PicX.isSpaces(wsInfoMsg) || PicX.isLowValues(wsInfoMsg);
        }

        boolean foundAcctInMaster() {
            return "1".equals(wsAccountMasterReadFlag);
        }

        boolean foundCustInMaster() {
            return "1".equals(wsCustMasterReadFlag);
        }

        void setReturnMsg(String text) {
            wsReturnMsg = PicX.of(text, 75);
        }

        /** 0000-MAIN (lines 262-407) including COMMON-RETURN (lines 394-404). */
        void main0000() {
            // INITIALIZE CC-WORK-AREA WS-MISC-STORAGE WS-COMMAREA (lines 267-269): PIC X -> SPACES,
            // numeric -> ZERO; FILLERs (the WS-FILE-ERROR-MESSAGE literals) keep their VALUEs.
            ccardAid = PicX.spaces(5);
            ccAcctId = PicX.spaces(11);
            wsRespCd = 0;
            wsReasCd = 0;
            wsTranid = PicX.spaces(4);
            wsInputFlag = " ";
            wsPfkFlag = " ";
            wsEditAcctFlag = " ";
            wsEditCustFlag = " ";
            wsCardRidAcctIdX = "0".repeat(11);
            wsCardRidCustIdX = "0".repeat(9);
            wsAccountMasterReadFlag = " ";
            wsCustMasterReadFlag = " ";
            errorOpname = PicX.spaces(8);
            errorFile = PicX.spaces(9);
            errorResp = PicX.spaces(10);
            errorResp2 = PicX.spaces(10);
            wsInfoMsg = PicX.spaces(40);
            wsReturnMsg = PicX.spaces(75);

            wsTranid = TRANSID;
            wsReturnMsg = PicX.spaces(75);

            // Line 283: CDEMO-FROM-PROGRAM is tested before the commarea is copied in, so it is still the
            // fresh task's LOW-VALUES and only EIBCALEN = 0 can take this branch.
            if (ctx.eib().eibcalen() == 0
                    || (CardDemoCommarea.FROM_PROGRAM.raw(commarea).equals(PicX.of(MENU_PROGRAM, 8))
                        && !commarea.isPgmReenter())) {
                initializeCommarea();
                wsThisProgcommarea = PicX.spaces(PROGCOMMAREA_LENGTH);
            } else {
                byte[] in = ctx.commarea();
                commarea.setBytes(in);
                wsThisProgcommarea = progcommareaOf(in);
            }

            ccardAid = PfKeys.storePfKey(ctx.eib().eibaid(), ccardAid);

            wsPfkFlag = "1";
            if (ccardAid.equals("ENTER") || ccardAid.equals("PFK03")) {
                wsPfkFlag = "0";
            }
            if ("1".equals(wsPfkFlag)) {
                ccardAid = "ENTER";
            }

            if (ccardAid.equals("PFK03")) {
                pf3Exit();
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
                AbendData.CULPRIT.set(abendData, PROGRAM);
                AbendData.CODE.set(abendData, "0001");
                AbendData.REASON.set(abendData, "");
                setReturnMsg(MSG_UNEXPECTED_DATA);
                sendPlainText();
            }
            if (inputError()) {
                sendMap1000();
                commonReturn();
            }
        }

        /** Lines 320-344: WHEN CCARD-AID-PFK03 -> XCTL to the caller (or the main menu). */
        void pf3Exit() {
            String fromTranid = commarea.fromTranid();
            if (PicX.isLowValues(fromTranid) || PicX.isSpaces(fromTranid)) {
                commarea.setToTranid(MENU_TRANSID);
            } else {
                commarea.setToTranid(fromTranid);
            }
            String fromProgram = commarea.fromProgram();
            if (PicX.isLowValues(fromProgram) || PicX.isSpaces(fromProgram)) {
                commarea.setToProgram(MENU_PROGRAM);
            } else {
                commarea.setToProgram(fromProgram);
            }
            commarea.setFromTranid(TRANSID);
            commarea.setFromProgram(PROGRAM);
            CardDemoCommarea.USRTYP_USER.set(commarea);
            CardDemoCommarea.PGM_ENTER.set(commarea);
            commarea.setLastMapset(MAPSET);
            commarea.setLastMap(MAP);
            ctx.xctl(commarea.toProgram(), commarea);
        }

        /** COMMON-RETURN (lines 394-404): RETURN TRANSID(CAVW) COMMAREA(WS-COMMAREA) LENGTH(2000). */
        void commonReturn() {
            CobolRecord wsCommarea = new CobolRecord(WS_COMMAREA_LENGTH);
            wsCommarea.initializeSpaces();
            wsCommarea.move(commarea.asString() + wsThisProgcommarea);
            ctx.returnTransid(wsTranid, wsCommarea);
        }

        /** 1000-SEND-MAP (lines 416-428). */
        void sendMap1000() {
            screenInit1100();
            setupScreenVars1200();
            setupScreenAttrs1300();
            sendScreen1400();
        }

        /** 1100-SCREEN-INIT (lines 431-458). */
        void screenInit1100() {
            cactvwa.lowValues();
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            cactvwa.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            cactvwa.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            cactvwa.setData("TRNNAME", TRANSID);
            cactvwa.setData("PGMNAME", PROGRAM);
            cactvwa.setData("CURDATE", dt.curdateMmDdYy());
            cactvwa.setData("CURTIME", dt.curtimeHhMmSs());
        }

        /** 1200-SETUP-SCREEN-VARS (lines 460-540). */
        void setupScreenVars1200() {
            if (ctx.eib().eibcalen() == 0) {
                wsInfoMsg = PicX.of(MSG_PROMPT_FOR_INPUT, 40);
            } else {
                if (acctFilterBlank()) {
                    cactvwa.setData("ACCTSID", PicX.lowValues(11));
                } else {
                    cactvwa.setData("ACCTSID", ccAcctId);
                }
                if (foundAcctInMaster() || foundCustInMaster()) {
                    cactvwa.setData("ACSTTUS", AccountRecord.ACTIVE_STATUS.raw(accountRecord));
                    cactvwa.setData("ACURBAL", money(AccountRecord.CURR_BAL));
                    cactvwa.setData("ACRDLIM", money(AccountRecord.CREDIT_LIMIT));
                    cactvwa.setData("ACSHLIM", money(AccountRecord.CASH_CREDIT_LIMIT));
                    cactvwa.setData("ACRCYCR", money(AccountRecord.CURR_CYC_CREDIT));
                    cactvwa.setData("ACRCYDB", money(AccountRecord.CURR_CYC_DEBIT));
                    cactvwa.setData("ADTOPEN", AccountRecord.OPEN_DATE.raw(accountRecord));
                    cactvwa.setData("AEXPDT", AccountRecord.EXPIRAION_DATE.raw(accountRecord));
                    cactvwa.setData("AREISDT", AccountRecord.REISSUE_DATE.raw(accountRecord));
                    cactvwa.setData("AADDGRP", AccountRecord.GROUP_ID.raw(accountRecord));
                }
                if (foundCustInMaster()) {
                    cactvwa.setData("ACSTNUM", CustomerRecord.CUST_ID.raw(customerRecord));
                    String ssn = CustomerRecord.SSN.raw(customerRecord);
                    // STRING ... DELIMITED BY SIZE INTO ACSTSSNO: 11 of its 12 bytes; the 12th keeps LOW-VALUE
                    cactvwa.setDataPrefix("ACSTSSN",
                            ssn.substring(0, 3) + "-" + ssn.substring(3, 5) + "-" + ssn.substring(5, 9));
                    cactvwa.setData("ACSTFCO", CustomerRecord.FICO_CREDIT_SCORE.raw(customerRecord));
                    cactvwa.setData("ACSTDOB", CustomerRecord.DOB_YYYY_MM_DD.raw(customerRecord));
                    cactvwa.setData("ACSFNAM", CustomerRecord.FIRST_NAME.raw(customerRecord));
                    cactvwa.setData("ACSMNAM", CustomerRecord.MIDDLE_NAME.raw(customerRecord));
                    cactvwa.setData("ACSLNAM", CustomerRecord.LAST_NAME.raw(customerRecord));
                    cactvwa.setData("ACSADL1", CustomerRecord.ADDR_LINE_1.raw(customerRecord));
                    cactvwa.setData("ACSADL2", CustomerRecord.ADDR_LINE_2.raw(customerRecord));
                    cactvwa.setData("ACSCITY", CustomerRecord.ADDR_LINE_3.raw(customerRecord));
                    cactvwa.setData("ACSSTTE", CustomerRecord.ADDR_STATE_CD.raw(customerRecord));
                    cactvwa.setData("ACSZIPC", CustomerRecord.ADDR_ZIP.raw(customerRecord));
                    cactvwa.setData("ACSCTRY", CustomerRecord.ADDR_COUNTRY_CD.raw(customerRecord));
                    cactvwa.setData("ACSPHN1", CustomerRecord.PHONE_NUM_1.raw(customerRecord));
                    cactvwa.setData("ACSPHN2", CustomerRecord.PHONE_NUM_2.raw(customerRecord));
                    cactvwa.setData("ACSGOVT", CustomerRecord.GOVT_ISSUED_ID.raw(customerRecord));
                    cactvwa.setData("ACSEFTC", CustomerRecord.EFT_ACCOUNT_ID.raw(customerRecord));
                    cactvwa.setData("ACSPFLG", CustomerRecord.PRI_CARD_HOLDER_IND.raw(customerRecord));
                }
            }
            if (noInfoMessage()) {
                wsInfoMsg = PicX.of(MSG_PROMPT_FOR_INPUT, 40);
            }
            cactvwa.setData("ERRMSG", wsReturnMsg);
            cactvwa.setData("INFOMSG", wsInfoMsg);
        }

        /** MOVE ACCT-xxx (S9(10)V99) TO xxxO (PIC +ZZZ,ZZZ,ZZZ.99). */
        private String money(NumericField field) {
            return NumericEdited.format(displayValue(field.raw(accountRecord), field.scale()), MONEY_PICOUT);
        }

        /** 1300-SETUP-SCREEN-ATTRS (lines 541-576). */
        void setupScreenAttrs1300() {
            cactvwa.setAttribute("ACCTSID", DFHBMFSE);
            // Every WHEN of the EVALUATE moves -1 to ACCTSIDL (lines 546-553).
            cactvwa.setCursor("ACCTSID");
            cactvwa.setColor("ACCTSID", DFHDFCOL);
            if (acctFilterNotOk()) {
                cactvwa.setColor("ACCTSID", DFHRED);
            }
            if (acctFilterBlank() && commarea.isPgmReenter()) {
                cactvwa.setData("ACCTSID", "*");
                cactvwa.setColor("ACCTSID", DFHRED);
            }
            if (noInfoMessage()) {
                cactvwa.setColor("INFOMSG", DFHBMDAR);
            } else {
                cactvwa.setColor("INFOMSG", DFHNEUTR);
            }
        }

        /** 1400-SEND-SCREEN (lines 577-594). */
        void sendScreen1400() {
            commarea.setPgmContext(1);
            wsRespCd = ctx.sendMap(cactvwa, SendMapOption.CURSOR, SendMapOption.ERASE, SendMapOption.FREEKB)
                    .code();
        }

        /** 2000-PROCESS-INPUTS (lines 596-608). */
        void processInputs2000() {
            receiveMap2100();
            editMapInputs2200();
        }

        /** 2100-RECEIVE-MAP (lines 610-621). */
        void receiveMap2100() {
            CicsResp resp = ctx.receiveMap(cactvwa);
            wsRespCd = resp.code();
            wsReasCd = ctx.eib().eibresp2();
        }

        /** 2200-EDIT-MAP-INPUTS (lines 622-648). */
        void editMapInputs2200() {
            wsInputFlag = "0";
            wsEditAcctFlag = "1";
            String acctsidi = cactvwa.data("ACCTSID");
            if (acctsidi.equals(PicX.of("*", 11)) || PicX.isSpaces(acctsidi)) {
                ccAcctId = PicX.lowValues(11);
            } else {
                ccAcctId = acctsidi;
            }
            editAccount2210();
            if (acctFilterBlank()) {
                setReturnMsg(MSG_NO_SEARCH_CRITERIA);
            }
        }

        /** 2210-EDIT-ACCOUNT (lines 649-686). */
        void editAccount2210() {
            wsEditAcctFlag = "0";
            if (PicX.isLowValues(ccAcctId) || PicX.isSpaces(ccAcctId)) {
                wsInputFlag = "1";
                wsEditAcctFlag = " ";
                if (returnMsgOff()) {
                    setReturnMsg(MSG_PROMPT_FOR_ACCT);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, 0L);
                return;
            }
            if (!ZonedDecimal.isNumeric(ccAcctId, false) || ccAcctId.equals("0".repeat(11))) {
                wsInputFlag = "1";
                wsEditAcctFlag = "0";
                if (returnMsgOff()) {
                    setReturnMsg(MSG_ACCT_FILTER_INVALID);
                }
                CardDemoCommarea.ACCT_ID.set(commarea, 0L);
            } else {
                moveRaw(commarea, CardDemoCommarea.ACCT_ID.offset(), ccAcctId);
                wsEditAcctFlag = "1";
            }
        }

        /** 9000-READ-ACCT (lines 687-722). */
        void readAcct9000() {
            wsInfoMsg = PicX.spaces(40);
            wsCardRidAcctIdX = CardDemoCommarea.ACCT_ID.raw(commarea);
            getCardXrefByAcct9200();
            if (acctFilterNotOk()) {
                return;
            }
            getAcctDataByAcct9300();
            // Line 710 tests DID-NOT-FIND-ACCT-IN-ACCTDAT, a WS-RETURN-MSG value no paragraph ever moves, so
            // the customer read happens even after an ACCTDAT NOTFND.
            if (wsReturnMsg.equals(PicX.of(MSG_DID_NOT_FIND_ACCT_IN_ACCTDAT, 75))) {
                return;
            }
            wsCardRidCustIdX = CardDemoCommarea.CUST_ID.raw(commarea);
            getCustDataByCust9400();
        }

        /** 9200-GETCARDXREF-BYACCT (lines 723-773). */
        void getCardXrefByAcct9200() {
            FileResult result = ctx.read(PicX.of(XREF_ACCT_PATH, 8), wsCardRidAcctIdX, CardXrefRecord.LENGTH);
            readInto(result, cardXrefRecord);
            if (wsRespCd == CicsResp.NORMAL.code()) {
                moveRaw(commarea, CardDemoCommarea.CUST_ID.offset(), CardXrefRecord.CUST_ID.raw(cardXrefRecord));
                moveRaw(commarea, CardDemoCommarea.CARD_NUM.offset(), CardXrefRecord.CARD_NUM.raw(cardXrefRecord));
            } else if (wsRespCd == CicsResp.NOTFND.code()) {
                wsInputFlag = "1";
                wsEditAcctFlag = "0";
                if (returnMsgOff()) {
                    errorResp = respX(wsRespCd);
                    errorResp2 = respX(wsReasCd);
                    setReturnMsg("Account:" + wsCardRidAcctIdX + " not found in" + " Cross ref file.  Resp:"
                            + errorResp + " Reas:" + errorResp2);
                }
            } else {
                wsInputFlag = "1";
                wsEditAcctFlag = "0";
                fileError("READ", XREF_ACCT_PATH);
            }
        }

        /** 9300-GETACCTDATA-BYACCT (lines 774-824). */
        void getAcctDataByAcct9300() {
            FileResult result = ctx.read(PicX.of(ACCT_FILE, 8), wsCardRidAcctIdX, AccountRecord.LENGTH);
            readInto(result, accountRecord);
            if (wsRespCd == CicsResp.NORMAL.code()) {
                wsAccountMasterReadFlag = "1";
            } else if (wsRespCd == CicsResp.NOTFND.code()) {
                wsInputFlag = "1";
                wsEditAcctFlag = "0";
                if (returnMsgOff()) {
                    errorResp = respX(wsRespCd);
                    errorResp2 = respX(wsReasCd);
                    setReturnMsg("Account:" + wsCardRidAcctIdX + " not found in" + " Acct Master file.Resp:"
                            + errorResp + " Reas:" + errorResp2);
                }
            } else {
                wsInputFlag = "1";
                wsEditAcctFlag = "0";
                fileError("READ", ACCT_FILE);
            }
        }

        /** 9400-GETCUSTDATA-BYCUST (lines 825-870). */
        void getCustDataByCust9400() {
            FileResult result = ctx.read(PicX.of(CUST_FILE, 8), wsCardRidCustIdX, CustomerRecord.LENGTH);
            readInto(result, customerRecord);
            if (wsRespCd == CicsResp.NORMAL.code()) {
                wsCustMasterReadFlag = "1";
            } else if (wsRespCd == CicsResp.NOTFND.code()) {
                wsInputFlag = "1";
                wsEditCustFlag = "0";
                errorResp = respX(wsRespCd);
                errorResp2 = respX(wsReasCd);
                if (returnMsgOff()) {
                    setReturnMsg("CustId:" + wsCardRidCustIdX + " not found" + " in customer master.Resp: "
                            + errorResp + " REAS:" + errorResp2);
                }
            } else {
                wsInputFlag = "1";
                wsEditCustFlag = "0";
                fileError("READ", CUST_FILE);
            }
        }

        /** WHEN OTHER of 9200/9300/9400: MOVE WS-FILE-ERROR-MESSAGE (80 bytes) TO WS-RETURN-MSG (75). */
        private void fileError(String opname, String file) {
            errorOpname = PicX.of(opname, 8);
            errorFile = PicX.of(file, 9);
            errorResp = respX(wsRespCd);
            errorResp2 = respX(wsReasCd);
            setReturnMsg(wsFileErrorMessage());
        }

        /** WS-FILE-ERROR-MESSAGE (lines 87-105), 80 bytes. */
        String wsFileErrorMessage() {
            return "File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp
                    + ",RESP2 " + errorResp2 + PicX.spaces(5);
        }

        private void readInto(FileResult result, CobolRecord into) {
            wsRespCd = result.resp().code();
            wsReasCd = result.resp2();
            if (result.data() != null && result.isNormal()) {
                into.setBytes(result.data());
            }
        }

        /** SEND-PLAIN-TEXT (lines 877-890). */
        void sendPlainText() {
            ctx.sendText(wsReturnMsg, wsReturnMsg.length(), true, true);
            ctx.returnToCics();
        }

        /** SEND-LONG-TEXT (lines 891-909): never PERFORMed (debugging aid); kept for completeness. */
        void sendLongText() {
            String wsLongMsg = PicX.spaces(500);
            ctx.sendText(wsLongMsg, wsLongMsg.length(), true, true);
            ctx.returnToCics();
        }

        /** ABEND-ROUTINE (lines 916-933). */
        void abendRoutine() {
            if (AbendData.MSG.isLowValues(abendData)) {
                AbendData.MSG.set(abendData, "UNEXPECTED ABEND OCCURRED.");
            }
            AbendData.CULPRIT.set(abendData, PROGRAM);
            ctx.sendText(abendData.asString(), AbendData.LENGTH, false, false);
            ctx.abend("9999");
        }

        private void initializeCommarea() {
            commarea.initializeSpaces();
            for (NumericField f : new NumericField[] {CardDemoCommarea.PGM_CONTEXT, CardDemoCommarea.CUST_ID,
                    CardDemoCommarea.ACCT_ID, CardDemoCommarea.CARD_NUM}) {
                f.set(commarea, 0L);
            }
        }
    }

    /** DFHCOMMAREA(161:12); storage past EIBCALEN is treated as spaces (see LIMITATIONS). */
    static String progcommareaOf(byte[] in) {
        byte[] b = new byte[PROGCOMMAREA_LENGTH];
        Arrays.fill(b, (byte) ' ');
        int from = CardDemoCommarea.LENGTH;
        if (in.length > from) {
            System.arraycopy(in, from, b, 0, Math.min(PROGCOMMAREA_LENGTH, in.length - from));
        }
        return PicX.fromBytes(b);
    }

    /** MOVE S9(9) COMP TO PIC X(10): the 9 digits, sign dropped, left-justified and space-padded. */
    static String respX(int code) {
        return PicX.of(String.format("%09d", Math.abs(code) % 1_000_000_000), 10);
    }

    /**
     * Value of a zoned DISPLAY field as a MOVE to a numeric-edited item sees it. Invalid data (e.g. the
     * LOW-VALUES of a record that was never read) is taken digit-by-digit from each byte's low nibble.
     */
    static BigDecimal displayValue(String raw, int scale) {
        if (ZonedDecimal.isNumeric(raw, true)) {
            return ZonedDecimal.decode(raw, scale);
        }
        StringBuilder digits = new StringBuilder(raw.length());
        for (int i = 0; i < raw.length(); i++) {
            int nibble = raw.charAt(i) & 0x0F;
            digits.append((char) ('0' + Math.min(nibble, 9)));
        }
        return new BigDecimal(new java.math.BigInteger(digits.toString()), scale);
    }

    /** Group/alphanumeric MOVE into the bytes of a numeric DISPLAY item (no conversion). */
    static void moveRaw(CobolRecord record, int offset, String value) {
        byte[] b = record.toBytes();
        byte[] v = PicX.toBytes(value);
        System.arraycopy(v, 0, b, offset, v.length);
        record.setBytes(b);
    }
}

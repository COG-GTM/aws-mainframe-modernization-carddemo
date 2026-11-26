package com.carddemo.programs.coactupc;

import static com.carddemo.programs.coactupc.AcctUpdateCommarea.DetailField.*;
import static com.carddemo.programs.coactupc.AcctUpdateCommarea.Side.NEW;
import static com.carddemo.programs.coactupc.AcctUpdateCommarea.Side.OLD;

import com.carddemo.programs.coactupc.AcctUpdateCommarea.DetailField;
import com.carddemo.programs.coactupc.copybook.AbendData;
import com.carddemo.programs.coactupc.copybook.AccountRecord;
import com.carddemo.programs.coactupc.copybook.BmsAttribute;
import com.carddemo.programs.coactupc.copybook.BmsColor;
import com.carddemo.programs.coactupc.copybook.CactupaField;
import com.carddemo.programs.coactupc.copybook.CactupaInput;
import com.carddemo.programs.coactupc.copybook.CactupaOutput;
import com.carddemo.programs.coactupc.copybook.CardDemoCommarea;
import com.carddemo.programs.coactupc.copybook.CardXrefRecord;
import com.carddemo.programs.coactupc.copybook.CcWorkArea;
import com.carddemo.programs.coactupc.copybook.CcWorkArea.CcardAid;
import com.carddemo.programs.coactupc.copybook.CustomerRecord;
import com.carddemo.programs.coactupc.copybook.DateEditProcedures;
import com.carddemo.programs.coactupc.copybook.DateTimeWork;
import com.carddemo.programs.coactupc.copybook.EditMessages;
import com.carddemo.programs.coactupc.copybook.LookupCodes;
import com.carddemo.programs.coactupc.copybook.PfKeyStore;
import com.carddemo.programs.coactupc.copybook.ScreenAttributeSetter;
import com.carddemo.programs.coactupc.copybook.ScreenTitle;
import com.carddemo.programs.coactupc.gateway.AccountRepository;
import com.carddemo.programs.coactupc.gateway.CardXrefRepository;
import com.carddemo.programs.coactupc.gateway.CicsResp;
import com.carddemo.programs.coactupc.gateway.CustomerRepository;
import com.carddemo.programs.coactupc.gateway.DateValidator;
import com.carddemo.programs.coactupc.gateway.FileResponse;
import com.carddemo.programs.coactupc.gateway.TransactionManager;
import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.EditedPicture;
import com.carddemo.programs.coactupc.support.NumericFunctions;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * COACTUPC (transaction CAUP) - accept and process ACCOUNT UPDATE.
 * Pseudo-conversational CICS program ported to a framework-free service: each {@link #execute}
 * call is one CICS task. Paragraph names are kept in comments above the methods that port them.
 */
public final class Coactupc {

    public static final String LIT_THISPGM = "COACTUPC";
    public static final String LIT_THISTRANID = "CAUP";
    public static final String LIT_THISMAPSET = "COACTUP ";
    public static final String LIT_THISMAP = "CACTUPA";
    public static final String LIT_CARDUPDATE_PGM = "COCRDUPC";
    public static final String LIT_CARDUPDATE_TRANID = "CCUP";
    public static final String LIT_CARDUPDATE_MAPSET = "COCRDUP ";
    public static final String LIT_CARDUPDATE_MAP = "CCRDUPA";
    public static final String LIT_CCLISTPGM = "COCRDLIC";
    public static final String LIT_CCLISTTRANID = "CCLI";
    public static final String LIT_CCLISTMAPSET = "COCRDLI";
    public static final String LIT_CCLISTMAP = "CCRDSLA";
    public static final String LIT_MENUPGM = "COMEN01C";
    public static final String LIT_MENUTRANID = "CM00";
    public static final String LIT_MENUMAPSET = "COMEN01";
    public static final String LIT_MENUMAP = "COMEN1A";
    public static final String LIT_CARDDTLPGM = "COCRDSLC";
    public static final String LIT_CARDDTLTRANID = "CCDL";
    public static final String LIT_CARDDTLMAPSET = "COCRDSL";
    public static final String LIT_CARDDTLMAP = "CCRDSLA";
    public static final String LIT_ACCTFILENAME = "ACCTDAT ";
    public static final String LIT_CUSTFILENAME = "CUSTDAT ";
    public static final String LIT_CARDFILENAME = "CARDDAT ";
    public static final String LIT_CARDFILENAME_ACCT_PATH = "CARDAIX ";
    public static final String LIT_CARDXREFNAME_ACCT_PATH = "CXACAIX ";
    static final String LIT_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    static final String LIT_LOWER = "abcdefghijklmnopqrstuvwxyz";
    static final String LIT_NUMBERS = "0123456789";
    static final String LIT_ALL_ALPHA_FROM = LIT_UPPER + LIT_LOWER;
    static final String LIT_ALL_ALPHANUM_FROM = LIT_UPPER + LIT_LOWER + LIT_NUMBERS;

    private final CardXrefRepository xrefFile;
    private final AccountRepository acctFile;
    private final CustomerRepository custFile;
    private final TransactionManager transactions;
    private final DateValidator dateValidator;
    private final Clock clock;

    public Coactupc(CardXrefRepository xrefFile, AccountRepository acctFile, CustomerRepository custFile,
            TransactionManager transactions, DateValidator dateValidator, Clock clock) {
        this.xrefFile = xrefFile;
        this.acctFile = acctFile;
        this.custFile = custFile;
        this.transactions = transactions;
        this.dateValidator = dateValidator;
        this.clock = clock;
    }

    /** Runs one CICS task of transaction CAUP. */
    public CicsResult execute(CicsRequest request) {
        return new Task(request).run();
    }

    /** Signals EXEC CICS ABEND: unwinds the task to its final result. */
    private static final class AbendSignal extends RuntimeException {
        private final transient CicsResult result;

        AbendSignal(CicsResult result) {
            super(null, null, false, false);
            this.result = result;
        }
    }

    /** One task: a fresh copy of WORKING-STORAGE. */
    private final class Task implements EditMessages {

        private final CicsRequest req;
        private final CcWorkArea cc = new CcWorkArea();
        private final WsMiscStorage ws = new WsMiscStorage();
        private CardDemoCommarea commarea = CardDemoCommarea.lowValues();
        private AcctUpdateCommarea acup = AcctUpdateCommarea.lowValues();
        private AccountRecord accountRecord = AccountRecord.lowValues();
        private CustomerRecord customerRecord = CustomerRecord.lowValues();
        private CardXrefRecord cardXrefRecord = CardXrefRecord.lowValues();
        private final AbendData abendData = new AbendData();
        private CactupaInput mapIn = CactupaInput.empty();
        private CactupaOutput mapOut = CactupaOutput.lowValues();

        Task(CicsRequest req) {
            this.req = req;
        }

        CicsResult run() {
            try {
                return mainPara();
            } catch (AbendSignal signal) {
                return signal.result;
            } catch (RuntimeException unexpected) {
                // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE)
                return abendRoutine();
            }
        }

        // ------------------------------------------------------------------ EditMessages
        @Override
        public String editVariableName() {
            return ws.editVariableName;
        }

        @Override
        public boolean isInputError() {
            return ws.isInputError();
        }

        @Override
        public void setInputError() {
            ws.inputFlag = '1';
        }

        @Override
        public boolean isReturnMsgOff() {
            return ws.isReturnMsgOff();
        }

        @Override
        public void stringIntoReturnMsg(String... parts) {
            ws.returnMsg = Cobol.stringInto(75, parts);
        }

        private void message(String... parts) {
            if (ws.isReturnMsgOff()) {
                stringIntoReturnMsg(parts);
            }
        }

        private String varName() {
            return Cobol.trim(ws.editVariableName);
        }

        private void setVarName(String name) {
            ws.editVariableName = Cobol.pad(name, 25);
        }

        // ------------------------------------------------------------------ 0000-MAIN
        private CicsResult mainPara() {
            cc.initialize();
            ws.initialize();
            ws.tranId = LIT_THISTRANID;
            ws.setReturnMsgOff();

            // CARDDEMO-COMMAREA is still the task's fresh working storage when this IF is evaluated,
            // so in practice only EIBCALEN = 0 selects this branch.
            boolean eibcalenZero = req.commarea() == null;
            if (eibcalenZero || (commarea.fromProgram.equals(LIT_MENUPGM) && !commarea.isPgmReenter())) {
                commarea.initialize();
                acup.initialize();
                commarea.setPgmEnter();
                acup.setChangeAction(AcctUpdateCommarea.DETAILS_NOT_FETCHED);
            } else {
                commarea = req.commarea().copy();
                acup = req.programCommarea() == null ? AcctUpdateCommarea.lowValues() : req.programCommarea().copy();
            }

            PfKeyStore.storePfkey(req.eibaid(), cc);

            ws.pfkFlag = '1';
            if (cc.is(CcardAid.ENTER) || cc.is(CcardAid.PFK03)
                    || (cc.is(CcardAid.PFK05) && acup.isChangesOkNotConfirmed())
                    || (cc.is(CcardAid.PFK12) && !acup.isDetailsNotFetched())) {
                ws.pfkFlag = '0';
            }
            if (ws.pfkFlag == '1') {
                cc.set(CcardAid.ENTER);
            }

            if (cc.is(CcardAid.PFK03)) {
                cc.set(CcardAid.PFK03);
                if (Cobol.isLowValuesOrSpaces(commarea.fromTranid)) {
                    commarea.toTranid = LIT_MENUTRANID;
                } else {
                    commarea.toTranid = commarea.fromTranid;
                }
                if (Cobol.isLowValuesOrSpaces(commarea.fromProgram)) {
                    commarea.toProgram = LIT_MENUPGM;
                } else {
                    commarea.toProgram = commarea.fromProgram;
                }
                commarea.fromTranid = LIT_THISTRANID;
                commarea.fromProgram = LIT_THISPGM;
                commarea.setUsrtypUser();
                commarea.setPgmEnter();
                commarea.lastMapset = Cobol.pad(LIT_THISMAPSET, 7);
                commarea.lastMap = Cobol.pad(LIT_THISMAP, 7);
                transactions.syncpoint();
                return CicsResult.xctl(commarea.toProgram, commarea.copy());
            }
            if ((acup.isDetailsNotFetched() && commarea.isPgmEnter())
                    || (commarea.fromProgram.equals(LIT_MENUPGM) && !commarea.isPgmReenter())) {
                acup.initialize();
                sendMap3000();
                commarea.setPgmReenter();
                acup.setChangeAction(AcctUpdateCommarea.DETAILS_NOT_FETCHED);
                return commonReturn();
            }
            if (acup.isChangesOkayedAndDone() || acup.isChangesFailed()) {
                acup.initialize();
                ws.initialize();
                commarea.acctId = 0;
                commarea.setPgmEnter();
                sendMap3000();
                commarea.setPgmReenter();
                acup.setChangeAction(AcctUpdateCommarea.DETAILS_NOT_FETCHED);
                return commonReturn();
            }
            processInputs1000();
            decideAction2000();
            sendMap3000();
            return commonReturn();
        }

        // COMMON-RETURN
        private CicsResult commonReturn() {
            cc.ccardErrorMsg = ws.returnMsg;
            // EXEC CICS RETURN TRANSID ends the task: implicit syncpoint
            transactions.syncpoint();
            return CicsResult.returned(LIT_THISTRANID, commarea.copy(), acup.copy(), mapOut);
        }

        // ------------------------------------------------------------------ 1000-PROCESS-INPUTS
        private void processInputs1000() {
            receiveMap1100();
            editMapInputs1200();
            cc.ccardErrorMsg = ws.returnMsg;
            cc.ccardNextProg = LIT_THISPGM;
            cc.ccardNextMapset = Cobol.pad(LIT_THISMAPSET, 7);
            cc.ccardNextMap = LIT_THISMAP;
        }

        private boolean starOrSpaces(String input) {
            return Cobol.alnumEquals(input, "*") || Cobol.isSpaces(input);
        }

        private void receiveInto(CactupaField field, DetailField target) {
            String in = mapIn.get(field);
            if (starOrSpaces(in)) {
                acup.set(NEW, target, Cobol.lowValues(target.length));
            } else {
                acup.set(NEW, target, in);
            }
        }

        private String receiveAmount(CactupaField field, DetailField target) {
            String in = mapIn.get(field);
            if (starOrSpaces(in)) {
                return Cobol.lowValues(15);
            }
            String x = Cobol.pad(in, 15);
            if (NumericFunctions.testNumvalC(x) == 0) {
                acup.setAmount(NEW, target, NumericFunctions.numvalC(in));
            }
            return x;
        }

        // 1100-RECEIVE-MAP
        private void receiveMap1100() {
            mapIn = req.map() == null ? CactupaInput.empty() : req.map();
            acup.initializeDetails(NEW);

            String acctsid = mapIn.get(CactupaField.ACCTSID);
            if (starOrSpaces(acctsid)) {
                cc.ccAcctId = Cobol.lowValues(11);
                acup.set(NEW, ACCT_ID_X, Cobol.lowValues(11));
            } else {
                cc.ccAcctId = acctsid;
                acup.set(NEW, ACCT_ID_X, acctsid);
            }
            if (acup.isDetailsNotFetched()) {
                return;
            }
            receiveInto(CactupaField.ACSTTUS, ACTIVE_STATUS);
            ws.newCreditLimitX = receiveAmount(CactupaField.ACRDLIM, CREDIT_LIMIT);
            ws.newCashCreditLimitX = receiveAmount(CactupaField.ACSHLIM, CASH_CREDIT_LIMIT);
            ws.newCurrBalX = receiveAmount(CactupaField.ACURBAL, CURR_BAL);
            ws.newCurrCycCreditX = receiveAmount(CactupaField.ACRCYCR, CURR_CYC_CREDIT);
            ws.newCurrCycDebitX = receiveAmount(CactupaField.ACRCYDB, CURR_CYC_DEBIT);
            receiveInto(CactupaField.OPNYEAR, OPEN_YEAR);
            receiveInto(CactupaField.OPNMON, OPEN_MON);
            receiveInto(CactupaField.OPNDAY, OPEN_DAY);
            receiveInto(CactupaField.EXPYEAR, EXP_YEAR);
            receiveInto(CactupaField.EXPMON, EXP_MON);
            receiveInto(CactupaField.EXPDAY, EXP_DAY);
            receiveInto(CactupaField.RISYEAR, REISSUE_YEAR);
            receiveInto(CactupaField.RISMON, REISSUE_MON);
            receiveInto(CactupaField.RISDAY, REISSUE_DAY);
            receiveInto(CactupaField.AADDGRP, GROUP_ID);
            receiveInto(CactupaField.ACSTNUM, CUST_ID_X);
            receiveInto(CactupaField.ACTSSN1, CUST_SSN_1);
            receiveInto(CactupaField.ACTSSN2, CUST_SSN_2);
            receiveInto(CactupaField.ACTSSN3, CUST_SSN_3);
            receiveInto(CactupaField.DOBYEAR, CUST_DOB_YEAR);
            receiveInto(CactupaField.DOBMON, CUST_DOB_MON);
            receiveInto(CactupaField.DOBDAY, CUST_DOB_DAY);
            receiveInto(CactupaField.ACSTFCO, CUST_FICO_SCORE_X);
            receiveInto(CactupaField.ACSFNAM, CUST_FIRST_NAME);
            receiveInto(CactupaField.ACSMNAM, CUST_MIDDLE_NAME);
            receiveInto(CactupaField.ACSLNAM, CUST_LAST_NAME);
            receiveInto(CactupaField.ACSADL1, CUST_ADDR_LINE_1);
            receiveInto(CactupaField.ACSADL2, CUST_ADDR_LINE_2);
            receiveInto(CactupaField.ACSCITY, CUST_ADDR_LINE_3);
            receiveInto(CactupaField.ACSSTTE, CUST_ADDR_STATE_CD);
            receiveInto(CactupaField.ACSCTRY, CUST_ADDR_COUNTRY_CD);
            receiveInto(CactupaField.ACSZIPC, CUST_ADDR_ZIP);
            receiveInto(CactupaField.ACSPH1A, CUST_PHONE_NUM_1A);
            receiveInto(CactupaField.ACSPH1B, CUST_PHONE_NUM_1B);
            receiveInto(CactupaField.ACSPH1C, CUST_PHONE_NUM_1C);
            receiveInto(CactupaField.ACSPH2A, CUST_PHONE_NUM_2A);
            receiveInto(CactupaField.ACSPH2B, CUST_PHONE_NUM_2B);
            receiveInto(CactupaField.ACSPH2C, CUST_PHONE_NUM_2C);
            receiveInto(CactupaField.ACSGOVT, CUST_GOVT_ISSUED_ID);
            receiveInto(CactupaField.ACSEFTC, CUST_EFT_ACCOUNT_ID);
            receiveInto(CactupaField.ACSPFLG, CUST_PRI_HOLDER_IND);
        }

        // 1200-EDIT-MAP-INPUTS
        private void editMapInputs1200() {
            ws.inputFlag = '0';
            if (acup.isDetailsNotFetched()) {
                editAccount1210();
                acup.setAcctDataLowValues(OLD);
                if (ws.isFlgAcctfilterBlank()) {
                    ws.setReturnMsg(Messages.NO_SEARCH_CRITERIA_RECEIVED);
                }
                return;
            }

            ws.setInfoMsg(Messages.FOUND_ACCOUNT_DATA);
            ws.acctMasterReadFlag = '1';
            ws.editAcctFlag = '1';
            ws.custMasterReadFlag = '1';
            ws.editCustFlag = '1';

            compareOldNew1205();

            if (ws.isNoChangesFound() || acup.isChangesOkNotConfirmed() || acup.isChangesOkayedAndDone()) {
                ws.nonKeyLowValues();
                return;
            }

            acup.setChangeAction(AcctUpdateCommarea.CHANGES_NOT_OK);

            setVarName("Account Status");
            ws.editYesNo = acup.get(NEW, ACTIVE_STATUS).charAt(0);
            editYesNo1220();
            ws.setFlag(NonKeyFlag.ACCT_STATUS, ws.editYesNo);

            editDate("Open Date", OPEN_DATE, NonKeyFlag.OPEN_YEAR, NonKeyFlag.OPEN_MONTH, NonKeyFlag.OPEN_DAY);

            editSignedAmount("Credit Limit", ws.newCreditLimitX, NonKeyFlag.CRED_LIMIT);

            editDate("Expiry Date", EXPIRAION_DATE, NonKeyFlag.EXPIRY_YEAR, NonKeyFlag.EXPIRY_MONTH,
                    NonKeyFlag.EXPIRY_DAY);

            editSignedAmount("Cash Credit Limit", ws.newCashCreditLimitX, NonKeyFlag.CASH_CREDIT_LIMIT);

            editDate("Reissue Date", REISSUE_DATE, NonKeyFlag.REISSUE_YEAR, NonKeyFlag.REISSUE_MONTH,
                    NonKeyFlag.REISSUE_DAY);

            editSignedAmount("Current Balance", ws.newCurrBalX, NonKeyFlag.CURR_BAL);
            editSignedAmount("Current Cycle Credit Limit", ws.newCurrCycCreditX, NonKeyFlag.CURR_CYC_CREDIT);
            editSignedAmount("Current Cycle Debit Limit", ws.newCurrCycDebitX, NonKeyFlag.CURR_CYC_DEBIT);

            setVarName("SSN");
            editUsSsn1265();

            editDate("Date of Birth", CUST_DOB_YYYY_MM_DD, NonKeyFlag.DT_OF_BIRTH_YEAR,
                    NonKeyFlag.DT_OF_BIRTH_MONTH, NonKeyFlag.DT_OF_BIRTH_DAY);
            if (dobFlagsValid()) {
                DateEditProcedures.editDateOfBirth(ws.dateEdit, this, LocalDate.now(clock));
                setDateFlags(NonKeyFlag.DT_OF_BIRTH_YEAR, NonKeyFlag.DT_OF_BIRTH_MONTH, NonKeyFlag.DT_OF_BIRTH_DAY);
            }

            setVarName("FICO Score");
            loadAlphanum(acup.get(NEW, CUST_FICO_SCORE_X), 3);
            editNumReqd1245();
            ws.setFlag(NonKeyFlag.FICO_SCORE, ws.editAlphanumOnlyFlags);
            if (ws.isValid(NonKeyFlag.FICO_SCORE)) {
                editFicoScore1275();
            }

            setVarName("First Name");
            loadAlphanum(acup.get(NEW, CUST_FIRST_NAME), 25);
            editAlphaReqd1225();
            ws.setFlag(NonKeyFlag.FIRST_NAME, ws.editAlphaOnlyFlags);

            setVarName("Middle Name");
            loadAlphanum(acup.get(NEW, CUST_MIDDLE_NAME), 25);
            editAlphaOpt1235();
            ws.setFlag(NonKeyFlag.MIDDLE_NAME, ws.editAlphaOnlyFlags);

            setVarName("Last Name");
            loadAlphanum(acup.get(NEW, CUST_LAST_NAME), 25);
            editAlphaReqd1225();
            ws.setFlag(NonKeyFlag.LAST_NAME, ws.editAlphaOnlyFlags);

            setVarName("Address Line 1");
            loadAlphanum(acup.get(NEW, CUST_ADDR_LINE_1), 50);
            editMandatory1215();
            ws.setFlag(NonKeyFlag.ADDRESS_LINE_1, ws.editMandatoryFlags);

            setVarName("State");
            loadAlphanum(acup.get(NEW, CUST_ADDR_STATE_CD), 2);
            editAlphaReqd1225();
            ws.setFlag(NonKeyFlag.STATE, ws.editAlphaOnlyFlags);
            if (ws.editAlphaOnlyFlags == WsMiscStorage.FLAG_ISVALID) {
                editUsStateCd1270();
            }

            setVarName("Zip");
            loadAlphanum(acup.get(NEW, CUST_ADDR_ZIP), 5);
            editNumReqd1245();
            ws.setFlag(NonKeyFlag.ZIPCODE, ws.editAlphanumOnlyFlags);

            setVarName("City");
            loadAlphanum(acup.get(NEW, CUST_ADDR_LINE_3), 50);
            editAlphaReqd1225();
            ws.setFlag(NonKeyFlag.CITY, ws.editAlphaOnlyFlags);

            setVarName("Country");
            loadAlphanum(acup.get(NEW, CUST_ADDR_COUNTRY_CD), 3);
            editAlphaReqd1225();
            ws.setFlag(NonKeyFlag.COUNTRY, ws.editAlphaOnlyFlags);

            setVarName("Phone Number 1");
            ws.editUsPhoneNum = acup.get(NEW, CUST_PHONE_NUM_1);
            editUsPhoneNum1260();
            ws.setFlag(NonKeyFlag.PHONE_NUM_1A, ws.editUsPhoneFlags[0]);
            ws.setFlag(NonKeyFlag.PHONE_NUM_1B, ws.editUsPhoneFlags[1]);
            ws.setFlag(NonKeyFlag.PHONE_NUM_1C, ws.editUsPhoneFlags[2]);

            setVarName("Phone Number 2");
            ws.editUsPhoneNum = acup.get(NEW, CUST_PHONE_NUM_2);
            editUsPhoneNum1260();
            ws.setFlag(NonKeyFlag.PHONE_NUM_2A, ws.editUsPhoneFlags[0]);
            ws.setFlag(NonKeyFlag.PHONE_NUM_2B, ws.editUsPhoneFlags[1]);
            ws.setFlag(NonKeyFlag.PHONE_NUM_2C, ws.editUsPhoneFlags[2]);

            setVarName("EFT Account Id");
            loadAlphanum(acup.get(NEW, CUST_EFT_ACCOUNT_ID), 10);
            editNumReqd1245();
            ws.setFlag(NonKeyFlag.EFT_ACCOUNT_ID, ws.editAlphanumOnlyFlags);

            setVarName("Primary Card Holder");
            ws.editYesNo = acup.get(NEW, CUST_PRI_HOLDER_IND).charAt(0);
            editYesNo1220();
            ws.setFlag(NonKeyFlag.PRI_CARDHOLDER, ws.editYesNo);

            if (ws.isValid(NonKeyFlag.STATE) && ws.isValid(NonKeyFlag.ZIPCODE)) {
                editUsStateZipCd1280();
            }

            if (!ws.isInputError()) {
                acup.setChangeAction(AcctUpdateCommarea.CHANGES_OK_NOT_CONFIRMED);
            }
        }

        private void editDate(String name, DetailField date, NonKeyFlag year, NonKeyFlag month, NonKeyFlag day) {
            setVarName(name);
            ws.dateEdit.editDateCcyymmdd = acup.get(NEW, date);
            DateEditProcedures.editDateCcyymmdd(ws.dateEdit, this, dateValidator);
            setDateFlags(year, month, day);
        }

        private void setDateFlags(NonKeyFlag year, NonKeyFlag month, NonKeyFlag day) {
            ws.setFlag(year, ws.dateEdit.yearFlg);
            ws.setFlag(month, ws.dateEdit.monthFlg);
            ws.setFlag(day, ws.dateEdit.dayFlg);
        }

        /** 88 WS-EDIT-DT-OF-BIRTH-ISVALID VALUE LOW-VALUES. */
        private boolean dobFlagsValid() {
            return ws.flag(NonKeyFlag.DT_OF_BIRTH_YEAR) == Cobol.LOW_VALUE
                    && ws.flag(NonKeyFlag.DT_OF_BIRTH_MONTH) == Cobol.LOW_VALUE
                    && ws.flag(NonKeyFlag.DT_OF_BIRTH_DAY) == Cobol.LOW_VALUE;
        }

        private void editSignedAmount(String name, String amountX, NonKeyFlag flag) {
            setVarName(name);
            ws.editSignedNumber9v2X = amountX;
            editSigned9v2_1250();
            ws.setFlag(flag, ws.flgSignedNumberEdit);
        }

        private void loadAlphanum(String value, int length) {
            ws.editAlphanumOnly = Cobol.pad(value, 256);
            ws.editAlphanumLength = length;
        }

        private String alphanumSlice() {
            return ws.editAlphanumOnly.substring(0, ws.editAlphanumLength);
        }

        private boolean sliceIsBlank() {
            String slice = alphanumSlice();
            return Cobol.isLowValues(slice) || Cobol.isSpaces(slice) || Cobol.trim(slice).isEmpty();
        }

        // 1205-COMPARE-OLD-NEW
        private void compareOldNew1205() {
            ws.dataChangedFlag = '0';
            boolean acctSame = acup.get(NEW, ACCT_ID_X).equals(acup.get(OLD, ACCT_ID_X))
                    && Cobol.upper(acup.get(NEW, ACTIVE_STATUS)).equals(Cobol.upper(acup.get(OLD, ACTIVE_STATUS)))
                    && same(CURR_BAL) && same(CREDIT_LIMIT) && same(CASH_CREDIT_LIMIT)
                    && same(OPEN_DATE) && same(EXPIRAION_DATE) && same(REISSUE_DATE)
                    && same(CURR_CYC_CREDIT) && same(CURR_CYC_DEBIT)
                    && sameTrimUpper(GROUP_ID);
            if (!acctSame) {
                ws.dataChangedFlag = '1';
                return;
            }
            boolean custSame = sameTrimUpper(CUST_ID_X)
                    && sameTrimUpper(CUST_FIRST_NAME)
                    && sameTrimUpper(CUST_MIDDLE_NAME)
                    && sameTrimUpper(CUST_LAST_NAME)
                    && sameTrimUpper(CUST_ADDR_LINE_1)
                    && sameTrimUpper(CUST_ADDR_LINE_2)
                    && sameTrimUpper(CUST_ADDR_LINE_3)
                    && sameTrimUpper(CUST_ADDR_STATE_CD)
                    && sameTrimUpper(CUST_ADDR_COUNTRY_CD)
                    && sameTrimUpper(CUST_ADDR_ZIP)
                    && same(CUST_PHONE_NUM_1A) && same(CUST_PHONE_NUM_1B) && same(CUST_PHONE_NUM_1C)
                    && same(CUST_PHONE_NUM_2A) && same(CUST_PHONE_NUM_2B) && same(CUST_PHONE_NUM_2C)
                    && same(CUST_SSN_X)
                    && sameTrimUpper(CUST_GOVT_ISSUED_ID)
                    && same(CUST_DOB_YYYY_MM_DD)
                    && same(CUST_EFT_ACCOUNT_ID)
                    && sameTrimUpper(CUST_PRI_HOLDER_IND)
                    && same(CUST_FICO_SCORE_X);
            if (custSame) {
                ws.setReturnMsg(Messages.NO_CHANGES_DETECTED);
            } else {
                ws.dataChangedFlag = '1';
            }
        }

        private boolean same(DetailField f) {
            return acup.get(NEW, f).equals(acup.get(OLD, f));
        }

        private boolean sameTrimUpper(DetailField f) {
            return Cobol.alnumEquals(Cobol.upper(Cobol.trim(acup.get(NEW, f))),
                    Cobol.upper(Cobol.trim(acup.get(OLD, f))));
        }

        // 1210-EDIT-ACCOUNT
        private void editAccount1210() {
            ws.editAcctFlag = '0';
            if (Cobol.isLowValues(cc.ccAcctId) || Cobol.isSpaces(cc.ccAcctId)) {
                setInputError();
                ws.editAcctFlag = ' ';
                if (ws.isReturnMsgOff()) {
                    ws.setReturnMsg(Messages.WS_PROMPT_FOR_ACCT);
                }
                commarea.acctId = 0;
                acup.setUnsigned(NEW, ACCT_ID_X, 0);
                return;
            }
            acup.set(NEW, ACCT_ID_X, cc.ccAcctId);
            if (!Cobol.isNumeric(cc.ccAcctId) || cc.ccAcctIdN() == 0) {
                setInputError();
                message("Account Number if supplied must be a 11 digit", " Non-Zero Number");
                commarea.acctId = 0;
                return;
            }
            commarea.acctId = cc.ccAcctIdN();
            ws.editAcctFlag = '1';
        }

        // 1215-EDIT-MANDATORY
        private void editMandatory1215() {
            ws.editMandatoryFlags = WsMiscStorage.FLAG_NOT_OK;
            if (sliceIsBlank()) {
                setInputError();
                ws.editMandatoryFlags = WsMiscStorage.FLAG_BLANK;
                message(varName(), " must be supplied.");
                return;
            }
            ws.editMandatoryFlags = WsMiscStorage.FLAG_ISVALID;
        }

        // 1220-EDIT-YESNO
        private void editYesNo1220() {
            char v = ws.editYesNo;
            if (v == Cobol.LOW_VALUE || v == ' ' || v == '0') {
                setInputError();
                ws.editYesNo = WsMiscStorage.FLAG_BLANK;
                message(varName(), " must be supplied.");
                return;
            }
            if (v == 'Y' || v == 'N') {
                return;
            }
            setInputError();
            ws.editYesNo = WsMiscStorage.FLAG_NOT_OK;
            message(varName(), " must be Y or N.");
        }

        // 1225-EDIT-ALPHA-REQD
        private void editAlphaReqd1225() {
            ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
            if (sliceIsBlank()) {
                setInputError();
                ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_BLANK;
                message(varName(), " must be supplied.");
                return;
            }
            if (!onlyCharsFrom(LIT_ALL_ALPHA_FROM)) {
                setInputError();
                ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), " can have alphabets only.");
                return;
            }
            ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_ISVALID;
        }

        /** INSPECT ... CONVERTING allowed TO spaces, then LENGTH(TRIM(...)) = 0. */
        private boolean onlyCharsFrom(String allowed) {
            String converted = Cobol.convertToSpaces(alphanumSlice(), allowed);
            ws.editAlphanumOnly = converted + ws.editAlphanumOnly.substring(ws.editAlphanumLength);
            return Cobol.trim(converted).isEmpty();
        }

        // 1230-EDIT-ALPHANUM-REQD
        void editAlphanumReqd1230() {
            ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
            if (sliceIsBlank()) {
                setInputError();
                ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_BLANK;
                message(varName(), " must be supplied.");
                return;
            }
            if (!onlyCharsFrom(LIT_ALL_ALPHANUM_FROM)) {
                setInputError();
                ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), " can have numbers or alphabets only.");
                return;
            }
            ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_ISVALID;
        }

        // 1235-EDIT-ALPHA-OPT
        private void editAlphaOpt1235() {
            ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
            if (sliceIsBlank()) {
                ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_ISVALID;
                return;
            }
            if (!onlyCharsFrom(LIT_ALL_ALPHA_FROM)) {
                setInputError();
                ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), " can have alphabets only.");
                return;
            }
            ws.editAlphaOnlyFlags = WsMiscStorage.FLAG_ISVALID;
        }

        // 1240-EDIT-ALPHANUM-OPT
        void editAlphanumOpt1240() {
            ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
            if (sliceIsBlank()) {
                ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_ISVALID;
                return;
            }
            if (!onlyCharsFrom(LIT_ALL_ALPHANUM_FROM)) {
                setInputError();
                ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), " can have numbers or alphabets only.");
                return;
            }
            ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_ISVALID;
        }

        // 1245-EDIT-NUM-REQD
        private void editNumReqd1245() {
            ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
            if (sliceIsBlank()) {
                setInputError();
                ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_BLANK;
                message(varName(), " must be supplied.");
                return;
            }
            if (!Cobol.isNumeric(alphanumSlice())) {
                setInputError();
                ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), " must be all numeric.");
                return;
            }
            if (NumericFunctions.numval(alphanumSlice()).signum() == 0) {
                setInputError();
                ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), " must not be zero.");
                return;
            }
            ws.editAlphanumOnlyFlags = WsMiscStorage.FLAG_ISVALID;
        }

        // 1250-EDIT-SIGNED-9V2
        private void editSigned9v2_1250() {
            ws.flgSignedNumberEdit = WsMiscStorage.FLAG_NOT_OK;
            if (Cobol.isLowValues(ws.editSignedNumber9v2X) || Cobol.isSpaces(ws.editSignedNumber9v2X)) {
                setInputError();
                ws.flgSignedNumberEdit = WsMiscStorage.FLAG_BLANK;
                message(varName(), " must be supplied.");
                return;
            }
            if (NumericFunctions.testNumvalC(ws.editSignedNumber9v2X) != 0) {
                setInputError();
                ws.flgSignedNumberEdit = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), " is not valid");
                return;
            }
            ws.flgSignedNumberEdit = WsMiscStorage.FLAG_ISVALID;
        }

        private String phoneA() {
            return ws.editUsPhoneNum.substring(1, 4);
        }

        private String phoneB() {
            return ws.editUsPhoneNum.substring(5, 8);
        }

        private String phoneC() {
            return ws.editUsPhoneNum.substring(9, 13);
        }

        // 1260-EDIT-US-PHONE-NUM THRU 1260-EDIT-US-PHONE-NUM-EXIT
        private void editUsPhoneNum1260() {
            ws.setUsPhoneInvalid();
            // the third test compares NUMA (not NUMC) with SPACES, as coded in the COBOL
            if (Cobol.isLowValuesOrSpaces(phoneA()) && Cobol.isLowValuesOrSpaces(phoneB())
                    && (Cobol.isSpaces(phoneA()) || Cobol.isLowValues(phoneC()))) {
                ws.setUsPhoneValid();
                return;
            }
            editAreaCode();
            editUsPhonePrefix();
            editUsPhoneLinenum();
        }

        // EDIT-AREA-CODE
        private void editAreaCode() {
            String a = phoneA();
            if (Cobol.isLowValuesOrSpaces(a)) {
                setInputError();
                ws.editUsPhoneFlags[0] = WsMiscStorage.FLAG_BLANK;
                message(varName(), ": Area code must be supplied.");
                return;
            }
            if (!Cobol.isNumeric(a)) {
                setInputError();
                ws.editUsPhoneFlags[0] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": Area code must be A 3 digit number.");
                return;
            }
            if (Cobol.unsignedValue(a) == 0) {
                setInputError();
                ws.editUsPhoneFlags[0] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": Area code cannot be zero");
                return;
            }
            if (!LookupCodes.validGeneralPurpCode(Cobol.pad(Cobol.trim(a), 3))) {
                setInputError();
                ws.editUsPhoneFlags[0] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": Not valid North America general purpose area code");
                return;
            }
            ws.editUsPhoneFlags[0] = WsMiscStorage.FLAG_ISVALID;
        }

        // EDIT-US-PHONE-PREFIX
        private void editUsPhonePrefix() {
            String b = phoneB();
            if (Cobol.isLowValuesOrSpaces(b)) {
                setInputError();
                ws.editUsPhoneFlags[1] = WsMiscStorage.FLAG_BLANK;
                message(varName(), ": Prefix code must be supplied.");
                return;
            }
            if (!Cobol.isNumeric(b)) {
                setInputError();
                ws.editUsPhoneFlags[1] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": Prefix code must be A 3 digit number.");
                return;
            }
            if (Cobol.unsignedValue(b) == 0) {
                setInputError();
                ws.editUsPhoneFlags[1] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": Prefix code cannot be zero");
                return;
            }
            ws.editUsPhoneFlags[1] = WsMiscStorage.FLAG_ISVALID;
        }

        // EDIT-US-PHONE-LINENUM
        private void editUsPhoneLinenum() {
            String c = phoneC();
            if (Cobol.isLowValuesOrSpaces(c)) {
                setInputError();
                ws.editUsPhoneFlags[2] = WsMiscStorage.FLAG_BLANK;
                message(varName(), ": Line number code must be supplied.");
                return;
            }
            if (!Cobol.isNumeric(c)) {
                setInputError();
                ws.editUsPhoneFlags[2] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": Line number code must be A 4 digit number.");
                return;
            }
            if (Cobol.unsignedValue(c) == 0) {
                setInputError();
                ws.editUsPhoneFlags[2] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": Line number code cannot be zero");
                return;
            }
            ws.editUsPhoneFlags[2] = WsMiscStorage.FLAG_ISVALID;
        }

        // 1265-EDIT-US-SSN
        private void editUsSsn1265() {
            setVarName("SSN: First 3 chars");
            loadAlphanum(acup.get(NEW, CUST_SSN_1), 3);
            editNumReqd1245();
            ws.editUsSsnFlags[0] = ws.editAlphanumOnlyFlags;
            // The IF below has no END-IF of its own: the period ending the paragraph closes it, so
            // parts 2 and 3 are only edited when part 1 passed 1245-EDIT-NUM-REQD.
            if (ws.editUsSsnFlags[0] != WsMiscStorage.FLAG_ISVALID) {
                return;
            }
            ws.editUsSsn = Cobol.replace(ws.editUsSsn, 0, acup.get(NEW, CUST_SSN_1));
            long part1 = Cobol.unsignedValue(ws.editUsSsn.substring(0, 3));
            if (part1 == 0 || part1 == 666 || (part1 >= 900 && part1 <= 999)) {
                setInputError();
                ws.editUsSsnFlags[0] = WsMiscStorage.FLAG_NOT_OK;
                message(varName(), ": should not be 000, 666, or between 900 and 999");
            }

            setVarName("SSN 4th & 5th chars");
            loadAlphanum(acup.get(NEW, CUST_SSN_2), 2);
            editNumReqd1245();
            ws.editUsSsnFlags[1] = ws.editAlphanumOnlyFlags;

            setVarName("SSN Last 4 chars");
            loadAlphanum(acup.get(NEW, CUST_SSN_3), 4);
            editNumReqd1245();
            ws.editUsSsnFlags[2] = ws.editAlphanumOnlyFlags;
        }

        // 1270-EDIT-US-STATE-CD
        private void editUsStateCd1270() {
            if (!LookupCodes.validUsStateCode(acup.get(NEW, CUST_ADDR_STATE_CD))) {
                setInputError();
                ws.setFlag(NonKeyFlag.STATE, NonKeyFlag.NOT_OK);
                message(varName(), ": is not a valid state code");
            }
        }

        // 1275-EDIT-FICO-SCORE
        private void editFicoScore1275() {
            if (!acup.isFicoRangeValid()) {
                setInputError();
                ws.setFlag(NonKeyFlag.FICO_SCORE, NonKeyFlag.NOT_OK);
                message(varName(), ": should be between 300 and 850");
            }
        }

        // 1280-EDIT-US-STATE-ZIP-CD
        private void editUsStateZipCd1280() {
            String stateZip2 = acup.get(NEW, CUST_ADDR_STATE_CD) + acup.get(NEW, CUST_ADDR_ZIP).substring(0, 2);
            if (!LookupCodes.validUsStateZipCd2Combo(stateZip2)) {
                setInputError();
                ws.setFlag(NonKeyFlag.STATE, NonKeyFlag.NOT_OK);
                ws.setFlag(NonKeyFlag.ZIPCODE, NonKeyFlag.NOT_OK);
                message("Invalid zip code for state");
            }
        }

        // ------------------------------------------------------------------ 2000-DECIDE-ACTION
        private void decideAction2000() {
            if (acup.isDetailsNotFetched() || cc.is(CcardAid.PFK12)) {
                if (ws.isFlgAcctfilterIsvalid()) {
                    ws.setReturnMsgOff();
                    readAcct9000();
                    if (ws.isFoundCustInMaster()) {
                        acup.setChangeAction(AcctUpdateCommarea.SHOW_DETAILS);
                    }
                }
            } else if (acup.isShowDetails()) {
                if (!(ws.isInputError() || ws.returnMsgIs(Messages.NO_CHANGES_DETECTED))) {
                    acup.setChangeAction(AcctUpdateCommarea.CHANGES_OK_NOT_CONFIRMED);
                }
            } else if (acup.isChangesNotOk()) {
                // CONTINUE
            } else if (acup.isChangesOkNotConfirmed() && cc.is(CcardAid.PFK05)) {
                writeProcessing9600();
                if (ws.returnMsgIs(Messages.COULD_NOT_LOCK_ACCT_FOR_UPDATE)) {
                    acup.setChangeAction(AcctUpdateCommarea.CHANGES_OKAYED_LOCK_ERROR);
                } else if (ws.returnMsgIs(Messages.LOCKED_BUT_UPDATE_FAILED)) {
                    acup.setChangeAction(AcctUpdateCommarea.CHANGES_OKAYED_BUT_FAILED);
                } else if (ws.returnMsgIs(Messages.DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                    acup.setChangeAction(AcctUpdateCommarea.SHOW_DETAILS);
                } else {
                    acup.setChangeAction(AcctUpdateCommarea.CHANGES_OKAYED_AND_DONE);
                }
            } else if (acup.isChangesOkNotConfirmed()) {
                // CONTINUE
            } else if (acup.isChangesOkayedAndDone()) {
                acup.setChangeAction(AcctUpdateCommarea.SHOW_DETAILS);
                if (Cobol.isLowValuesOrSpaces(commarea.fromTranid)) {
                    commarea.acctId = 0;
                    commarea.cardNum = 0;
                    commarea.acctStatus = Cobol.lowValues(1);
                }
            } else {
                abendData.abendCulprit = Cobol.pad(LIT_THISPGM, 8);
                abendData.abendCode = "0001";
                abendData.abendReason = Cobol.spaces(50);
                abendData.abendMsg = Cobol.pad("UNEXPECTED DATA SCENARIO", 72);
                throw new AbendSignal(abendRoutine());
            }
        }

        // ------------------------------------------------------------------ 3000-SEND-MAP
        private void sendMap3000() {
            screenInit3100();
            setupScreenVars3200();
            setupInfomsg3250();
            setupScreenAttrs3300();
            setupInfomsgAttrs3390();
            sendScreen3400();
        }

        // 3100-SCREEN-INIT
        private void screenInit3100() {
            mapOut = CactupaOutput.lowValues();
            DateTimeWork now = new DateTimeWork(LocalDateTime.now(clock));
            mapOut.setData(CactupaField.TITLE01, ScreenTitle.CCDA_TITLE01);
            mapOut.setData(CactupaField.TITLE02, ScreenTitle.CCDA_TITLE02);
            mapOut.setData(CactupaField.TRNNAME, LIT_THISTRANID);
            mapOut.setData(CactupaField.PGMNAME, LIT_THISPGM);
            mapOut.setData(CactupaField.CURDATE, now.curdateMmDdYy());
            mapOut.setData(CactupaField.CURTIME, now.curtimeHhMmSs());
        }

        // 3200-SETUP-SCREEN-VARS
        private void setupScreenVars3200() {
            if (commarea.isPgmEnter()) {
                return;
            }
            if (cc.ccAcctIdN() == 0 && ws.isFlgAcctfilterIsvalid()) {
                mapOut.setLowValues(CactupaField.ACCTSID);
            } else {
                mapOut.setData(CactupaField.ACCTSID, cc.ccAcctId);
            }
            if (acup.isDetailsNotFetched() || cc.ccAcctIdN() == 0) {
                showInitialValues3201();
            } else if (acup.isShowDetails()) {
                showOriginalValues3202();
            } else if (acup.isChangesMade()) {
                showUpdatedValues3203();
            } else {
                showOriginalValues3202();
            }
        }

        // 3201-SHOW-INITIAL-VALUES
        private void showInitialValues3201() {
            for (CactupaField f : CactupaField.values()) {
                if (f.ordinal() >= CactupaField.ACSTTUS.ordinal() && f.ordinal() <= CactupaField.ACSPFLG.ordinal()) {
                    mapOut.setLowValues(f);
                }
            }
        }

        private String currency(BigDecimal value) {
            ws.editCurrency92 = EditedPicture.plusZzzCommaZzzCommaZzzDot99(value);
            return ws.editCurrency92;
        }

        // 3202-SHOW-ORIGINAL-VALUES
        private void showOriginalValues3202() {
            ws.nonKeyLowValues();
            ws.setInfoMsg(Messages.PROMPT_FOR_CHANGES);
            if (ws.isFoundAcctInMaster() || ws.isFoundCustInMaster()) {
                mapOut.setData(CactupaField.ACSTTUS, acup.get(OLD, ACTIVE_STATUS));
                mapOut.setData(CactupaField.ACURBAL, currency(acup.amount(OLD, CURR_BAL)));
                mapOut.setData(CactupaField.ACRDLIM, currency(acup.amount(OLD, CREDIT_LIMIT)));
                mapOut.setData(CactupaField.ACSHLIM, currency(acup.amount(OLD, CASH_CREDIT_LIMIT)));
                mapOut.setData(CactupaField.ACRCYCR, currency(acup.amount(OLD, CURR_CYC_CREDIT)));
                mapOut.setData(CactupaField.ACRCYDB, currency(acup.amount(OLD, CURR_CYC_DEBIT)));
                mapOut.setData(CactupaField.OPNYEAR, acup.get(OLD, OPEN_YEAR));
                mapOut.setData(CactupaField.OPNMON, acup.get(OLD, OPEN_MON));
                mapOut.setData(CactupaField.OPNDAY, acup.get(OLD, OPEN_DAY));
                mapOut.setData(CactupaField.EXPYEAR, acup.get(OLD, EXP_YEAR));
                mapOut.setData(CactupaField.EXPMON, acup.get(OLD, EXP_MON));
                mapOut.setData(CactupaField.EXPDAY, acup.get(OLD, EXP_DAY));
                mapOut.setData(CactupaField.RISYEAR, acup.get(OLD, REISSUE_YEAR));
                mapOut.setData(CactupaField.RISMON, acup.get(OLD, REISSUE_MON));
                mapOut.setData(CactupaField.RISDAY, acup.get(OLD, REISSUE_DAY));
                mapOut.setData(CactupaField.AADDGRP, acup.get(OLD, GROUP_ID));
            }
            if (ws.isFoundCustInMaster()) {
                String ssn = acup.get(OLD, CUST_SSN_X);
                String phone1 = acup.get(OLD, CUST_PHONE_NUM_1);
                String phone2 = acup.get(OLD, CUST_PHONE_NUM_2);
                mapOut.setData(CactupaField.ACSTNUM, acup.get(OLD, CUST_ID_X));
                mapOut.setData(CactupaField.ACTSSN1, ssn.substring(0, 3));
                mapOut.setData(CactupaField.ACTSSN2, ssn.substring(3, 5));
                mapOut.setData(CactupaField.ACTSSN3, ssn.substring(5, 9));
                mapOut.setData(CactupaField.ACSTFCO, acup.get(OLD, CUST_FICO_SCORE_X));
                mapOut.setData(CactupaField.DOBYEAR, acup.get(OLD, CUST_DOB_YEAR));
                mapOut.setData(CactupaField.DOBMON, acup.get(OLD, CUST_DOB_MON));
                mapOut.setData(CactupaField.DOBDAY, acup.get(OLD, CUST_DOB_DAY));
                mapOut.setData(CactupaField.ACSFNAM, acup.get(OLD, CUST_FIRST_NAME));
                mapOut.setData(CactupaField.ACSMNAM, acup.get(OLD, CUST_MIDDLE_NAME));
                mapOut.setData(CactupaField.ACSLNAM, acup.get(OLD, CUST_LAST_NAME));
                mapOut.setData(CactupaField.ACSADL1, acup.get(OLD, CUST_ADDR_LINE_1));
                mapOut.setData(CactupaField.ACSADL2, acup.get(OLD, CUST_ADDR_LINE_2));
                mapOut.setData(CactupaField.ACSCITY, acup.get(OLD, CUST_ADDR_LINE_3));
                mapOut.setData(CactupaField.ACSSTTE, acup.get(OLD, CUST_ADDR_STATE_CD));
                mapOut.setData(CactupaField.ACSZIPC, acup.get(OLD, CUST_ADDR_ZIP));
                mapOut.setData(CactupaField.ACSCTRY, acup.get(OLD, CUST_ADDR_COUNTRY_CD));
                mapOut.setData(CactupaField.ACSPH1A, phone1.substring(1, 4));
                mapOut.setData(CactupaField.ACSPH1B, phone1.substring(5, 8));
                mapOut.setData(CactupaField.ACSPH1C, phone1.substring(9, 13));
                mapOut.setData(CactupaField.ACSPH2A, phone2.substring(1, 4));
                mapOut.setData(CactupaField.ACSPH2B, phone2.substring(5, 8));
                mapOut.setData(CactupaField.ACSPH2C, phone2.substring(9, 13));
                mapOut.setData(CactupaField.ACSGOVT, acup.get(OLD, CUST_GOVT_ISSUED_ID));
                mapOut.setData(CactupaField.ACSEFTC, acup.get(OLD, CUST_EFT_ACCOUNT_ID));
                mapOut.setData(CactupaField.ACSPFLG, acup.get(OLD, CUST_PRI_HOLDER_IND));
            }
        }

        private void showAmount(NonKeyFlag flag, CactupaField field, DetailField amount, String amountX) {
            if (ws.flag(flag) == Cobol.LOW_VALUE) {
                mapOut.setData(field, currency(acup.amount(NEW, amount)));
            } else {
                mapOut.setData(field, amountX);
            }
        }

        // 3203-SHOW-UPDATED-VALUES
        private void showUpdatedValues3203() {
            mapOut.setData(CactupaField.ACSTTUS, acup.get(NEW, ACTIVE_STATUS));
            showAmount(NonKeyFlag.CRED_LIMIT, CactupaField.ACRDLIM, CREDIT_LIMIT, ws.newCreditLimitX);
            showAmount(NonKeyFlag.CASH_CREDIT_LIMIT, CactupaField.ACSHLIM, CASH_CREDIT_LIMIT, ws.newCashCreditLimitX);
            showAmount(NonKeyFlag.CURR_BAL, CactupaField.ACURBAL, CURR_BAL, ws.newCurrBalX);
            showAmount(NonKeyFlag.CURR_CYC_CREDIT, CactupaField.ACRCYCR, CURR_CYC_CREDIT, ws.newCurrCycCreditX);
            showAmount(NonKeyFlag.CURR_CYC_DEBIT, CactupaField.ACRCYDB, CURR_CYC_DEBIT, ws.newCurrCycDebitX);
            mapOut.setData(CactupaField.OPNYEAR, acup.get(NEW, OPEN_YEAR));
            mapOut.setData(CactupaField.OPNMON, acup.get(NEW, OPEN_MON));
            mapOut.setData(CactupaField.OPNDAY, acup.get(NEW, OPEN_DAY));
            mapOut.setData(CactupaField.EXPYEAR, acup.get(NEW, EXP_YEAR));
            mapOut.setData(CactupaField.EXPMON, acup.get(NEW, EXP_MON));
            mapOut.setData(CactupaField.EXPDAY, acup.get(NEW, EXP_DAY));
            mapOut.setData(CactupaField.RISYEAR, acup.get(NEW, REISSUE_YEAR));
            mapOut.setData(CactupaField.RISMON, acup.get(NEW, REISSUE_MON));
            mapOut.setData(CactupaField.RISDAY, acup.get(NEW, REISSUE_DAY));
            mapOut.setData(CactupaField.AADDGRP, acup.get(NEW, GROUP_ID));
            mapOut.setData(CactupaField.ACSTNUM, acup.get(NEW, CUST_ID_X));
            mapOut.setData(CactupaField.ACTSSN1, acup.get(NEW, CUST_SSN_1));
            mapOut.setData(CactupaField.ACTSSN2, acup.get(NEW, CUST_SSN_2));
            mapOut.setData(CactupaField.ACTSSN3, acup.get(NEW, CUST_SSN_3));
            mapOut.setData(CactupaField.ACSTFCO, acup.get(NEW, CUST_FICO_SCORE_X));
            mapOut.setData(CactupaField.DOBYEAR, acup.get(NEW, CUST_DOB_YEAR));
            mapOut.setData(CactupaField.DOBMON, acup.get(NEW, CUST_DOB_MON));
            mapOut.setData(CactupaField.DOBDAY, acup.get(NEW, CUST_DOB_DAY));
            mapOut.setData(CactupaField.ACSFNAM, acup.get(NEW, CUST_FIRST_NAME));
            mapOut.setData(CactupaField.ACSMNAM, acup.get(NEW, CUST_MIDDLE_NAME));
            mapOut.setData(CactupaField.ACSLNAM, acup.get(NEW, CUST_LAST_NAME));
            mapOut.setData(CactupaField.ACSADL1, acup.get(NEW, CUST_ADDR_LINE_1));
            mapOut.setData(CactupaField.ACSADL2, acup.get(NEW, CUST_ADDR_LINE_2));
            mapOut.setData(CactupaField.ACSCITY, acup.get(NEW, CUST_ADDR_LINE_3));
            mapOut.setData(CactupaField.ACSSTTE, acup.get(NEW, CUST_ADDR_STATE_CD));
            mapOut.setData(CactupaField.ACSZIPC, acup.get(NEW, CUST_ADDR_ZIP));
            mapOut.setData(CactupaField.ACSCTRY, acup.get(NEW, CUST_ADDR_COUNTRY_CD));
            mapOut.setData(CactupaField.ACSPH1A, acup.get(NEW, CUST_PHONE_NUM_1A));
            mapOut.setData(CactupaField.ACSPH1B, acup.get(NEW, CUST_PHONE_NUM_1B));
            mapOut.setData(CactupaField.ACSPH1C, acup.get(NEW, CUST_PHONE_NUM_1C));
            mapOut.setData(CactupaField.ACSPH2A, acup.get(NEW, CUST_PHONE_NUM_2A));
            mapOut.setData(CactupaField.ACSPH2B, acup.get(NEW, CUST_PHONE_NUM_2B));
            mapOut.setData(CactupaField.ACSPH2C, acup.get(NEW, CUST_PHONE_NUM_2C));
            mapOut.setData(CactupaField.ACSGOVT, acup.get(NEW, CUST_GOVT_ISSUED_ID));
            mapOut.setData(CactupaField.ACSEFTC, acup.get(NEW, CUST_EFT_ACCOUNT_ID));
            mapOut.setData(CactupaField.ACSPFLG, acup.get(NEW, CUST_PRI_HOLDER_IND));
        }

        // 3250-SETUP-INFOMSG
        private void setupInfomsg3250() {
            if (commarea.isPgmEnter()) {
                ws.setInfoMsg(Messages.PROMPT_FOR_SEARCH_KEYS);
            } else if (acup.isDetailsNotFetched()) {
                ws.setInfoMsg(Messages.PROMPT_FOR_SEARCH_KEYS);
            } else if (acup.isShowDetails()) {
                ws.setInfoMsg(Messages.PROMPT_FOR_CHANGES);
            } else if (acup.isChangesNotOk()) {
                ws.setInfoMsg(Messages.PROMPT_FOR_CHANGES);
            } else if (acup.isChangesOkNotConfirmed()) {
                ws.setInfoMsg(Messages.PROMPT_FOR_CONFIRMATION);
            } else if (acup.isChangesOkayedAndDone()) {
                ws.setInfoMsg(Messages.CONFIRM_UPDATE_SUCCESS);
            } else if (acup.isChangesOkayedLockError()) {
                ws.setInfoMsg(Messages.INFORM_FAILURE);
            } else if (acup.isChangesOkayedButFailed()) {
                ws.setInfoMsg(Messages.INFORM_FAILURE);
            } else if (ws.isNoInfoMessage()) {
                ws.setInfoMsg(Messages.PROMPT_FOR_SEARCH_KEYS);
            }
            mapOut.setData(CactupaField.INFOMSG, ws.infoMsg);
            mapOut.setData(CactupaField.ERRMSG, ws.returnMsg);
        }

        private boolean notOkOrBlank(NonKeyFlag f) {
            return ws.isNotOk(f) || ws.isBlank(f);
        }

        private boolean ssnNotOkOrBlank(int part) {
            char v = ws.editUsSsnFlags[part];
            return v == WsMiscStorage.FLAG_NOT_OK || v == WsMiscStorage.FLAG_BLANK;
        }

        // 3300-SETUP-SCREEN-ATTRS
        private void setupScreenAttrs3300() {
            protectAllAttrs3310();
            if (acup.isDetailsNotFetched()) {
                mapOut.setAttribute(CactupaField.ACCTSID, BmsAttribute.DFHBMFSE);
            } else if (acup.isShowDetails() || acup.isChangesNotOk()) {
                unprotectFewAttrs3320();
            } else if (acup.isChangesOkNotConfirmed() || acup.isChangesOkayedAndDone()) {
                // CONTINUE
            } else {
                mapOut.setAttribute(CactupaField.ACCTSID, BmsAttribute.DFHBMFSE);
            }

            mapOut.setCursor(cursorField());

            if (Cobol.pad(commarea.lastMapset, 7).equals(LIT_CCLISTMAPSET)) {
                mapOut.setColor(CactupaField.ACCTSID, BmsColor.DFHDFCOL);
            }
            if (ws.isFlgAcctfilterNotOk()) {
                mapOut.setColor(CactupaField.ACCTSID, BmsColor.DFHRED);
            }
            if (ws.isFlgAcctfilterBlank() && commarea.isPgmReenter()) {
                mapOut.setData(CactupaField.ACCTSID, "*");
                mapOut.setColor(CactupaField.ACCTSID, BmsColor.DFHRED);
            }
            if (acup.isDetailsNotFetched() || ws.isFlgAcctfilterBlank() || ws.isFlgAcctfilterNotOk()) {
                return;
            }
            // COPY CSSETATY REPLACING ... (one per edited field, in program order)
            NonKeyFlag[] beforeSsn = {
                NonKeyFlag.ACCT_STATUS, NonKeyFlag.OPEN_YEAR, NonKeyFlag.OPEN_MONTH, NonKeyFlag.OPEN_DAY,
                NonKeyFlag.CRED_LIMIT, NonKeyFlag.EXPIRY_YEAR, NonKeyFlag.EXPIRY_MONTH, NonKeyFlag.EXPIRY_DAY,
                NonKeyFlag.CASH_CREDIT_LIMIT, NonKeyFlag.REISSUE_YEAR, NonKeyFlag.REISSUE_MONTH,
                NonKeyFlag.REISSUE_DAY, NonKeyFlag.CURR_BAL, NonKeyFlag.CURR_CYC_CREDIT, NonKeyFlag.CURR_CYC_DEBIT
            };
            for (NonKeyFlag f : beforeSsn) {
                setAttr(f);
            }
            CactupaField[] ssnFields = {CactupaField.ACTSSN1, CactupaField.ACTSSN2, CactupaField.ACTSSN3};
            for (int i = 0; i < 3; i++) {
                char v = ws.editUsSsnFlags[i];
                ScreenAttributeSetter.apply(v == WsMiscStorage.FLAG_NOT_OK, v == WsMiscStorage.FLAG_BLANK,
                        commarea.isPgmReenter(), mapOut, ssnFields[i]);
            }
            NonKeyFlag[] afterSsn = {
                NonKeyFlag.DT_OF_BIRTH_YEAR, NonKeyFlag.DT_OF_BIRTH_MONTH, NonKeyFlag.DT_OF_BIRTH_DAY,
                NonKeyFlag.FICO_SCORE, NonKeyFlag.FIRST_NAME, NonKeyFlag.MIDDLE_NAME, NonKeyFlag.LAST_NAME,
                NonKeyFlag.ADDRESS_LINE_1, NonKeyFlag.STATE, NonKeyFlag.ADDRESS_LINE_2, NonKeyFlag.ZIPCODE,
                NonKeyFlag.CITY, NonKeyFlag.COUNTRY, NonKeyFlag.PHONE_NUM_1A, NonKeyFlag.PHONE_NUM_1B,
                NonKeyFlag.PHONE_NUM_1C, NonKeyFlag.PHONE_NUM_2A, NonKeyFlag.PHONE_NUM_2B, NonKeyFlag.PHONE_NUM_2C,
                NonKeyFlag.PRI_CARDHOLDER, NonKeyFlag.EFT_ACCOUNT_ID
            };
            for (NonKeyFlag f : afterSsn) {
                setAttr(f);
            }
        }

        private void setAttr(NonKeyFlag f) {
            ScreenAttributeSetter.apply(ws.isNotOk(f), ws.isBlank(f), commarea.isPgmReenter(), mapOut,
                    f.screenField);
        }

        /** EVALUATE TRUE in 3300 that moves -1 to one xxxxL field. */
        private CactupaField cursorField() {
            if (ws.infoMsgIs(Messages.FOUND_ACCOUNT_DATA) || ws.returnMsgIs(Messages.NO_CHANGES_DETECTED)) {
                return CactupaField.ACSTTUS;
            }
            if (ws.isFlgAcctfilterNotOk() || ws.isFlgAcctfilterBlank()) {
                return CactupaField.ACCTSID;
            }
            NonKeyFlag[] order = {
                NonKeyFlag.ACCT_STATUS, NonKeyFlag.OPEN_YEAR, NonKeyFlag.OPEN_MONTH, NonKeyFlag.OPEN_DAY,
                NonKeyFlag.CRED_LIMIT, NonKeyFlag.EXPIRY_YEAR, NonKeyFlag.EXPIRY_MONTH, NonKeyFlag.EXPIRY_DAY,
                NonKeyFlag.CASH_CREDIT_LIMIT, NonKeyFlag.REISSUE_YEAR, NonKeyFlag.REISSUE_MONTH,
                NonKeyFlag.REISSUE_DAY, NonKeyFlag.CURR_BAL, NonKeyFlag.CURR_CYC_CREDIT, NonKeyFlag.CURR_CYC_DEBIT
            };
            for (NonKeyFlag f : order) {
                if (notOkOrBlank(f)) {
                    return f.screenField;
                }
            }
            if (ssnNotOkOrBlank(0)) {
                return CactupaField.ACTSSN1;
            }
            if (ssnNotOkOrBlank(1)) {
                return CactupaField.ACTSSN2;
            }
            if (ssnNotOkOrBlank(2)) {
                return CactupaField.ACTSSN3;
            }
            NonKeyFlag[] order2 = {
                NonKeyFlag.DT_OF_BIRTH_YEAR, NonKeyFlag.DT_OF_BIRTH_MONTH, NonKeyFlag.DT_OF_BIRTH_DAY,
                NonKeyFlag.FICO_SCORE, NonKeyFlag.FIRST_NAME
            };
            for (NonKeyFlag f : order2) {
                if (notOkOrBlank(f)) {
                    return f.screenField;
                }
            }
            if (ws.isNotOk(NonKeyFlag.MIDDLE_NAME)) {
                return CactupaField.ACSMNAM;
            }
            NonKeyFlag[] order3 = {
                NonKeyFlag.LAST_NAME, NonKeyFlag.ADDRESS_LINE_1, NonKeyFlag.STATE, NonKeyFlag.ZIPCODE,
                NonKeyFlag.CITY, NonKeyFlag.COUNTRY, NonKeyFlag.PHONE_NUM_1A, NonKeyFlag.PHONE_NUM_1B,
                NonKeyFlag.PHONE_NUM_1C, NonKeyFlag.PHONE_NUM_2A, NonKeyFlag.PHONE_NUM_2B, NonKeyFlag.PHONE_NUM_2C,
                NonKeyFlag.EFT_ACCOUNT_ID, NonKeyFlag.PRI_CARDHOLDER
            };
            for (NonKeyFlag f : order3) {
                if (notOkOrBlank(f)) {
                    return f.screenField;
                }
            }
            return CactupaField.ACCTSID;
        }

        // 3310-PROTECT-ALL-ATTRS
        private void protectAllAttrs3310() {
            for (CactupaField f : CactupaField.values()) {
                if (f.ordinal() >= CactupaField.ACCTSID.ordinal() && f.ordinal() <= CactupaField.ACSPFLG.ordinal()) {
                    mapOut.setAttribute(f, BmsAttribute.DFHBMPRF);
                }
            }
            mapOut.setAttribute(CactupaField.INFOMSG, BmsAttribute.DFHBMPRF);
        }

        // 3320-UNPROTECT-FEW-ATTRS
        private void unprotectFewAttrs3320() {
            for (CactupaField f : CactupaField.values()) {
                if (f.ordinal() >= CactupaField.ACSTTUS.ordinal() && f.ordinal() <= CactupaField.ACSPFLG.ordinal()) {
                    mapOut.setAttribute(f, BmsAttribute.DFHBMFSE);
                }
            }
            mapOut.setAttribute(CactupaField.ACSTNUM, BmsAttribute.DFHBMPRF);
            mapOut.setAttribute(CactupaField.ACSCTRY, BmsAttribute.DFHBMPRF);
            mapOut.setAttribute(CactupaField.INFOMSG, BmsAttribute.DFHBMPRF);
        }

        // 3390-SETUP-INFOMSG-ATTRS
        private void setupInfomsgAttrs3390() {
            if (ws.isNoInfoMessage()) {
                mapOut.setAttribute(CactupaField.INFOMSG, BmsAttribute.DFHBMDAR);
            } else {
                mapOut.setAttribute(CactupaField.INFOMSG, BmsAttribute.DFHBMASB);
            }
            if (acup.isChangesMade() && !acup.isChangesOkayedAndDone()) {
                mapOut.setAttribute(CactupaField.FKEY12, BmsAttribute.DFHBMASB);
            }
            if (ws.infoMsgIs(Messages.PROMPT_FOR_CONFIRMATION)) {
                mapOut.setAttribute(CactupaField.FKEY05, BmsAttribute.DFHBMASB);
                mapOut.setAttribute(CactupaField.FKEY12, BmsAttribute.DFHBMASB);
            }
        }

        // 3400-SEND-SCREEN
        private void sendScreen3400() {
            cc.ccardNextMapset = Cobol.pad(LIT_THISMAPSET, 7);
            cc.ccardNextMap = LIT_THISMAP;
            mapOut.sent(cc.ccardNextMap, cc.ccardNextMapset, true, true);
        }

        // ------------------------------------------------------------------ 9000-READ-ACCT
        private void readAcct9000() {
            acup.initializeDetails(OLD);
            ws.infoMsg = Cobol.spaces(40);
            acup.set(OLD, ACCT_ID_X, cc.ccAcctId);
            ws.cardRidAcctIdX = cc.ccAcctId;

            getCardXrefByAcct9200();
            if (ws.isFlgAcctfilterNotOk()) {
                return;
            }
            getAcctDataByAcct9300();
            // never true: 9300 builds its own NOTFND text instead of setting this 88
            if (ws.returnMsgIs(Messages.DID_NOT_FIND_ACCT_IN_ACCTDAT)) {
                return;
            }
            ws.cardRidCustIdX = Cobol.unsignedDisplay(commarea.custId, 9);
            getCustDataByCust9400();
            // never true: 9400 builds its own NOTFND text instead of setting this 88
            if (ws.returnMsgIs(Messages.DID_NOT_FIND_CUST_IN_CUSTDAT)) {
                return;
            }
            storeFetchedData9500();
        }

        private void fileError(String file) {
            ws.errorOpname = Cobol.pad("READ", 8);
            ws.errorFile = Cobol.pad(file, 9);
            ws.errorResp = Cobol.binaryToAlnum(ws.respCd, 10);
            ws.errorResp2 = Cobol.binaryToAlnum(ws.reasCd, 10);
            ws.setReturnMsg(ws.fileErrorMessage());
        }

        // 9200-GETCARDXREF-BYACCT
        private void getCardXrefByAcct9200() {
            FileResponse<CardXrefRecord> r = xrefFile.readByAcctId(ws.cardRidAcctIdX);
            ws.respCd = r.resp();
            ws.reasCd = r.resp2();
            if (r.record() != null) {
                cardXrefRecord = r.record();
            }
            if (r.resp() == CicsResp.NORMAL) {
                commarea.custId = cardXrefRecord.custId();
                commarea.cardNum = Cobol.unsignedValue(cardXrefRecord.cardNum());
            } else if (r.resp() == CicsResp.NOTFND) {
                setInputError();
                ws.editAcctFlag = '0';
                if (ws.isReturnMsgOff()) {
                    ws.errorResp = Cobol.binaryToAlnum(ws.respCd, 10);
                    ws.errorResp2 = Cobol.binaryToAlnum(ws.reasCd, 10);
                    stringIntoReturnMsg("Account:", ws.cardRidAcctIdX, " not found in", " Cross ref file.  Resp:",
                            ws.errorResp, " Reas:", ws.errorResp2);
                }
            } else {
                setInputError();
                ws.editAcctFlag = '0';
                fileError(LIT_CARDXREFNAME_ACCT_PATH);
            }
        }

        // 9300-GETACCTDATA-BYACCT
        private void getAcctDataByAcct9300() {
            FileResponse<AccountRecord> r = acctFile.read(ws.cardRidAcctIdX);
            ws.respCd = r.resp();
            ws.reasCd = r.resp2();
            if (r.resp() == CicsResp.NORMAL) {
                accountRecord = r.record();
                ws.acctMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                setInputError();
                ws.editAcctFlag = '0';
                if (ws.isReturnMsgOff()) {
                    ws.errorResp = Cobol.binaryToAlnum(ws.respCd, 10);
                    ws.errorResp2 = Cobol.binaryToAlnum(ws.reasCd, 10);
                    stringIntoReturnMsg("Account:", ws.cardRidAcctIdX, " not found in", " Acct Master file.Resp:",
                            ws.errorResp, " Reas:", ws.errorResp2);
                }
            } else {
                setInputError();
                ws.editAcctFlag = '0';
                fileError(LIT_ACCTFILENAME);
            }
        }

        // 9400-GETCUSTDATA-BYCUST
        private void getCustDataByCust9400() {
            FileResponse<CustomerRecord> r = custFile.read(ws.cardRidCustIdX);
            ws.respCd = r.resp();
            ws.reasCd = r.resp2();
            if (r.resp() == CicsResp.NORMAL) {
                customerRecord = r.record();
                ws.custMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                setInputError();
                ws.editCustFlag = '0';
                ws.errorResp = Cobol.binaryToAlnum(ws.respCd, 10);
                ws.errorResp2 = Cobol.binaryToAlnum(ws.reasCd, 10);
                if (ws.isReturnMsgOff()) {
                    stringIntoReturnMsg("CustId:", ws.cardRidCustIdX, " not found", " in customer master.Resp: ",
                            ws.errorResp, " REAS:", ws.errorResp2);
                }
            } else {
                setInputError();
                ws.editCustFlag = '0';
                fileError(LIT_CUSTFILENAME);
            }
        }

        // 9500-STORE-FETCHED-DATA
        private void storeFetchedData9500() {
            commarea.acctId = accountRecord.acctId();
            commarea.custId = customerRecord.custId();
            commarea.custFname = customerRecord.firstName();
            commarea.custMname = customerRecord.middleName();
            commarea.custLname = customerRecord.lastName();
            commarea.acctStatus = accountRecord.activeStatus();
            commarea.cardNum = Cobol.unsignedValue(cardXrefRecord.cardNum());

            acup.initializeDetails(OLD);
            acup.setUnsigned(OLD, ACCT_ID_X, accountRecord.acctId());
            acup.set(OLD, ACTIVE_STATUS, accountRecord.activeStatus());
            acup.setAmount(OLD, CURR_BAL, accountRecord.currBal());
            acup.setAmount(OLD, CREDIT_LIMIT, accountRecord.creditLimit());
            acup.setAmount(OLD, CASH_CREDIT_LIMIT, accountRecord.cashCreditLimit());
            acup.setAmount(OLD, CURR_CYC_CREDIT, accountRecord.currCycCredit());
            acup.setAmount(OLD, CURR_CYC_DEBIT, accountRecord.currCycDebit());
            acup.set(OLD, OPEN_YEAR, accountRecord.openDate().substring(0, 4));
            acup.set(OLD, OPEN_MON, accountRecord.openDate().substring(5, 7));
            acup.set(OLD, OPEN_DAY, accountRecord.openDate().substring(8, 10));
            acup.set(OLD, EXP_YEAR, accountRecord.expirationDate().substring(0, 4));
            acup.set(OLD, EXP_MON, accountRecord.expirationDate().substring(5, 7));
            acup.set(OLD, EXP_DAY, accountRecord.expirationDate().substring(8, 10));
            acup.set(OLD, REISSUE_YEAR, accountRecord.reissueDate().substring(0, 4));
            acup.set(OLD, REISSUE_MON, accountRecord.reissueDate().substring(5, 7));
            acup.set(OLD, REISSUE_DAY, accountRecord.reissueDate().substring(8, 10));
            acup.set(OLD, GROUP_ID, accountRecord.groupId());

            acup.setUnsigned(OLD, CUST_ID_X, customerRecord.custId());
            acup.setUnsigned(OLD, CUST_SSN_X, customerRecord.ssn());
            acup.set(OLD, CUST_DOB_YEAR, customerRecord.dobYyyyMmDd().substring(0, 4));
            acup.set(OLD, CUST_DOB_MON, customerRecord.dobYyyyMmDd().substring(5, 7));
            acup.set(OLD, CUST_DOB_DAY, customerRecord.dobYyyyMmDd().substring(8, 10));
            acup.setUnsigned(OLD, CUST_FICO_SCORE_X, customerRecord.ficoCreditScore());
            acup.set(OLD, CUST_FIRST_NAME, customerRecord.firstName());
            acup.set(OLD, CUST_MIDDLE_NAME, customerRecord.middleName());
            acup.set(OLD, CUST_LAST_NAME, customerRecord.lastName());
            acup.set(OLD, CUST_ADDR_LINE_1, customerRecord.addrLine1());
            acup.set(OLD, CUST_ADDR_LINE_2, customerRecord.addrLine2());
            acup.set(OLD, CUST_ADDR_LINE_3, customerRecord.addrLine3());
            acup.set(OLD, CUST_ADDR_STATE_CD, customerRecord.addrStateCd());
            acup.set(OLD, CUST_ADDR_COUNTRY_CD, customerRecord.addrCountryCd());
            acup.set(OLD, CUST_ADDR_ZIP, customerRecord.addrZip());
            acup.set(OLD, CUST_PHONE_NUM_1, customerRecord.phoneNum1());
            acup.set(OLD, CUST_PHONE_NUM_2, customerRecord.phoneNum2());
            acup.set(OLD, CUST_GOVT_ISSUED_ID, customerRecord.govtIssuedId());
            acup.set(OLD, CUST_EFT_ACCOUNT_ID, customerRecord.eftAccountId());
            acup.set(OLD, CUST_PRI_HOLDER_IND, customerRecord.priCardHolderInd());
        }

        // 9600-WRITE-PROCESSING
        private void writeProcessing9600() {
            ws.cardRidAcctIdX = cc.ccAcctId;
            FileResponse<AccountRecord> acct = acctFile.readForUpdate(ws.cardRidAcctIdX);
            ws.respCd = acct.resp();
            ws.reasCd = acct.resp2();
            if (!acct.isNormal()) {
                setInputError();
                if (ws.isReturnMsgOff()) {
                    ws.setReturnMsg(Messages.COULD_NOT_LOCK_ACCT_FOR_UPDATE);
                }
                return;
            }
            accountRecord = acct.record();

            ws.cardRidCustIdX = Cobol.unsignedDisplay(commarea.custId, 9);
            FileResponse<CustomerRecord> cust = custFile.readForUpdate(ws.cardRidCustIdX);
            ws.respCd = cust.resp();
            ws.reasCd = cust.resp2();
            if (!cust.isNormal()) {
                setInputError();
                if (ws.isReturnMsgOff()) {
                    ws.setReturnMsg(Messages.COULD_NOT_LOCK_CUST_FOR_UPDATE);
                }
                return;
            }
            customerRecord = cust.record();

            checkChangeInRec9700();
            if (ws.returnMsgIs(Messages.DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                return;
            }

            AcctUpdateRecord au = ws.acctUpdate;
            au.initialize();
            au.id = acup.unsigned(NEW, ACCT_ID_X);
            au.activeStatus = acup.get(NEW, ACTIVE_STATUS);
            au.currBal = acup.amount(NEW, CURR_BAL);
            au.creditLimit = acup.amount(NEW, CREDIT_LIMIT);
            au.cashCreditLimit = acup.amount(NEW, CASH_CREDIT_LIMIT);
            au.currCycCredit = acup.amount(NEW, CURR_CYC_CREDIT);
            au.currCycDebit = acup.amount(NEW, CURR_CYC_DEBIT);
            au.openDate = Cobol.stringInto(10, acup.get(NEW, OPEN_YEAR), "-", acup.get(NEW, OPEN_MON), "-",
                    acup.get(NEW, OPEN_DAY));
            au.expiraionDate = Cobol.stringInto(10, acup.get(NEW, EXP_YEAR), "-", acup.get(NEW, EXP_MON), "-",
                    acup.get(NEW, EXP_DAY));
            au.reissueDate = accountRecord.reissueDate();
            au.reissueDate = Cobol.stringInto(10, acup.get(NEW, REISSUE_YEAR), "-", acup.get(NEW, REISSUE_MON), "-",
                    acup.get(NEW, REISSUE_DAY));
            au.groupId = acup.get(NEW, GROUP_ID);

            CustUpdateRecord cu = ws.custUpdate;
            cu.initialize();
            cu.id = acup.unsigned(NEW, CUST_ID_X);
            cu.firstName = acup.get(NEW, CUST_FIRST_NAME);
            cu.middleName = acup.get(NEW, CUST_MIDDLE_NAME);
            cu.lastName = acup.get(NEW, CUST_LAST_NAME);
            cu.addrLine1 = acup.get(NEW, CUST_ADDR_LINE_1);
            cu.addrLine2 = acup.get(NEW, CUST_ADDR_LINE_2);
            cu.addrLine3 = acup.get(NEW, CUST_ADDR_LINE_3);
            cu.addrStateCd = acup.get(NEW, CUST_ADDR_STATE_CD);
            cu.addrCountryCd = acup.get(NEW, CUST_ADDR_COUNTRY_CD);
            cu.addrZip = acup.get(NEW, CUST_ADDR_ZIP);
            cu.phoneNum1 = Cobol.stringInto(15, "(", acup.get(NEW, CUST_PHONE_NUM_1A), ")",
                    acup.get(NEW, CUST_PHONE_NUM_1B), "-", acup.get(NEW, CUST_PHONE_NUM_1C));
            cu.phoneNum2 = Cobol.stringInto(15, "(", acup.get(NEW, CUST_PHONE_NUM_2A), ")",
                    acup.get(NEW, CUST_PHONE_NUM_2B), "-", acup.get(NEW, CUST_PHONE_NUM_2C));
            cu.ssn = acup.unsigned(NEW, CUST_SSN_X);
            cu.govtIssuedId = acup.get(NEW, CUST_GOVT_ISSUED_ID);
            cu.dobYyyyMmDd = Cobol.stringInto(10, acup.get(NEW, CUST_DOB_YEAR), "-", acup.get(NEW, CUST_DOB_MON), "-",
                    acup.get(NEW, CUST_DOB_DAY));
            cu.eftAccountId = acup.get(NEW, CUST_EFT_ACCOUNT_ID);
            cu.priCardInd = acup.get(NEW, CUST_PRI_HOLDER_IND);
            cu.ficoCreditScore = acup.unsigned(NEW, CUST_FICO_SCORE_X);

            FileResponse<Void> rwAcct = acctFile.rewrite(au.toFixedWidth());
            ws.respCd = rwAcct.resp();
            ws.reasCd = rwAcct.resp2();
            if (!rwAcct.isNormal()) {
                ws.setReturnMsg(Messages.LOCKED_BUT_UPDATE_FAILED);
                return;
            }
            FileResponse<Void> rwCust = custFile.rewrite(cu.toFixedWidth());
            ws.respCd = rwCust.resp();
            ws.reasCd = rwCust.resp2();
            if (!rwCust.isNormal()) {
                ws.setReturnMsg(Messages.LOCKED_BUT_UPDATE_FAILED);
                transactions.rollback();
            }
        }

        // 9700-CHECK-CHANGE-IN-REC
        private void checkChangeInRec9700() {
            AccountRecord a = accountRecord;
            boolean acctSame = a.activeStatus().equals(acup.get(OLD, ACTIVE_STATUS))
                    && a.currBal().compareTo(acup.amount(OLD, CURR_BAL)) == 0
                    && a.creditLimit().compareTo(acup.amount(OLD, CREDIT_LIMIT)) == 0
                    && a.cashCreditLimit().compareTo(acup.amount(OLD, CASH_CREDIT_LIMIT)) == 0
                    && a.currCycCredit().compareTo(acup.amount(OLD, CURR_CYC_CREDIT)) == 0
                    && a.currCycDebit().compareTo(acup.amount(OLD, CURR_CYC_DEBIT)) == 0
                    && a.openDate().substring(0, 4).equals(acup.get(OLD, OPEN_YEAR))
                    && a.openDate().substring(5, 7).equals(acup.get(OLD, OPEN_MON))
                    && a.openDate().substring(8, 10).equals(acup.get(OLD, OPEN_DAY))
                    && a.expirationDate().substring(0, 4).equals(acup.get(OLD, EXP_YEAR))
                    && a.expirationDate().substring(5, 7).equals(acup.get(OLD, EXP_MON))
                    && a.expirationDate().substring(8, 10).equals(acup.get(OLD, EXP_DAY))
                    && a.reissueDate().substring(0, 4).equals(acup.get(OLD, REISSUE_YEAR))
                    && a.reissueDate().substring(5, 7).equals(acup.get(OLD, REISSUE_MON))
                    && a.reissueDate().substring(8, 10).equals(acup.get(OLD, REISSUE_DAY))
                    && Cobol.lower(a.groupId()).equals(Cobol.lower(acup.get(OLD, GROUP_ID)));
            if (!acctSame) {
                ws.setReturnMsg(Messages.DATA_WAS_CHANGED_BEFORE_UPDATE);
                return;
            }
            CustomerRecord c = customerRecord;
            String oldDob = acup.get(OLD, CUST_DOB_YYYY_MM_DD);
            boolean custSame = upperEq(c.firstName(), CUST_FIRST_NAME)
                    && upperEq(c.middleName(), CUST_MIDDLE_NAME)
                    && upperEq(c.lastName(), CUST_LAST_NAME)
                    && upperEq(c.addrLine1(), CUST_ADDR_LINE_1)
                    && upperEq(c.addrLine2(), CUST_ADDR_LINE_2)
                    && upperEq(c.addrLine3(), CUST_ADDR_LINE_3)
                    && upperEq(c.addrStateCd(), CUST_ADDR_STATE_CD)
                    && upperEq(c.addrCountryCd(), CUST_ADDR_COUNTRY_CD)
                    && c.addrZip().equals(acup.get(OLD, CUST_ADDR_ZIP))
                    && c.phoneNum1().equals(acup.get(OLD, CUST_PHONE_NUM_1))
                    && c.phoneNum2().equals(acup.get(OLD, CUST_PHONE_NUM_2))
                    && c.ssn() == acup.unsigned(OLD, CUST_SSN_X)
                    && upperEq(c.govtIssuedId(), CUST_GOVT_ISSUED_ID)
                    && c.dobYyyyMmDd().substring(0, 4).equals(oldDob.substring(0, 4))
                    && c.dobYyyyMmDd().substring(5, 7).equals(oldDob.substring(4, 6))
                    && c.dobYyyyMmDd().substring(8, 10).equals(oldDob.substring(6, 8))
                    && c.eftAccountId().equals(acup.get(OLD, CUST_EFT_ACCOUNT_ID))
                    && c.priCardHolderInd().equals(acup.get(OLD, CUST_PRI_HOLDER_IND))
                    && c.ficoCreditScore() == acup.unsigned(OLD, CUST_FICO_SCORE_X);
            if (!custSame) {
                ws.setReturnMsg(Messages.DATA_WAS_CHANGED_BEFORE_UPDATE);
            }
        }

        private boolean upperEq(String fileValue, DetailField oldField) {
            return Cobol.alnumEquals(Cobol.upper(fileValue), Cobol.upper(acup.get(OLD, oldField)));
        }

        // ABEND-ROUTINE
        private CicsResult abendRoutine() {
            if (Cobol.isLowValues(abendData.abendMsg)) {
                abendData.abendMsg = Cobol.pad("UNEXPECTED ABEND OCCURRED.", 72);
            }
            abendData.abendCulprit = Cobol.pad(LIT_THISPGM, 8);
            // EXEC CICS SEND FROM(ABEND-DATA); HANDLE ABEND CANCEL; ABEND ABCODE('9999') - the task's
            // uncommitted updates are backed out.
            transactions.rollback();
            return CicsResult.abend("9999", abendData.copy());
        }
    }
}

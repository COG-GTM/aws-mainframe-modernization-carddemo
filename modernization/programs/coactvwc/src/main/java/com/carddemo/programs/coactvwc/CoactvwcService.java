package com.carddemo.programs.coactvwc;

import static com.carddemo.programs.coactvwc.WsMessages.*;

import com.carddemo.programs.coactvwc.copybook.AbendData;
import com.carddemo.programs.coactvwc.copybook.AccountRecord;
import com.carddemo.programs.coactvwc.copybook.CactvwaInput;
import com.carddemo.programs.coactvwc.copybook.CactvwaOutput;
import com.carddemo.programs.coactvwc.copybook.CardDemoCommarea;
import com.carddemo.programs.coactvwc.copybook.CardXrefRecord;
import com.carddemo.programs.coactvwc.copybook.CcWorkArea;
import com.carddemo.programs.coactvwc.copybook.CcardAid;
import com.carddemo.programs.coactvwc.copybook.CurrentDateTime;
import com.carddemo.programs.coactvwc.copybook.CustomerRecord;
import com.carddemo.programs.coactvwc.copybook.DfhBmsca;
import com.carddemo.programs.coactvwc.copybook.ScreenTitles;
import com.carddemo.programs.coactvwc.copybook.StorePfKey;
import com.carddemo.programs.coactvwc.repository.AccountRepository;
import com.carddemo.programs.coactvwc.repository.CardXrefRepository;
import com.carddemo.programs.coactvwc.repository.CicsResp;
import com.carddemo.programs.coactvwc.repository.CustomerRepository;
import com.carddemo.programs.coactvwc.repository.FileReadResult;
import com.carddemo.programs.coactvwc.support.Cobol;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Objects;

/** COACTVWC (transaction CAVW): Account View. One {@link #process} call = one pseudo-conversational CICS task. */
public final class CoactvwcService {

    // WS-LITERALS
    public static final String LIT_THISPGM = "COACTVWC";
    public static final String LIT_THISTRANID = "CAVW";
    public static final String LIT_THISMAPSET = "COACTVW ";
    public static final String LIT_THISMAP = "CACTVWA";
    public static final String LIT_CCLISTPGM = "COCRDLIC";
    public static final String LIT_CCLISTTRANID = "CCLI";
    public static final String LIT_CCLISTMAPSET = "COCRDLI";
    public static final String LIT_CCLISTMAP = "CCRDSLA";
    public static final String LIT_CARDUPDATEPGM = "COCRDUPC";
    public static final String LIT_CARDUDPATETRANID = "CCUP";
    public static final String LIT_CARDUPDATEMAPSET = "COCRDUP ";
    public static final String LIT_CARDUPDATEMAP = "CCRDUPA";
    public static final String LIT_MENUPGM = "COMEN01C";
    public static final String LIT_MENUTRANID = "CM00";
    public static final String LIT_MENUMAPSET = "COMEN01";
    public static final String LIT_MENUMAP = "COMEN1A";
    public static final String LIT_CARDDTLPGM = "COCRDSLC";
    public static final String LIT_CARDDTLTRANID = "CCDL";
    public static final String LIT_CARDDTLMAPSET = "COCRDSL";
    public static final String LIT_CARDDTLMAP = "CCRDSLA";
    public static final String LIT_ACCTFILENAME = "ACCTDAT ";
    public static final String LIT_CARDFILENAME = "CARDDAT ";
    public static final String LIT_CUSTFILENAME = "CUSTDAT ";
    public static final String LIT_CARDFILENAME_ACCT_PATH = "CARDAIX ";
    public static final String LIT_CARDXREFNAME_ACCT_PATH = "CXACAIX ";

    /** WS-COMMAREA PIC X(2000). */
    public static final int WS_COMMAREA_LEN = 2000;
    static final int WS_RETURN_MSG_LEN = 75;
    static final int WS_INFO_MSG_LEN = 40;

    /**
     * ACCOUNT-RECORD has no VALUE clause and is only filled by a NORMAL read; this stands in for that untouched
     * storage (see 1200-SETUP-SCREEN-VARS when the customer is found but the account is not).
     */
    static final AccountRecord UNREAD_ACCOUNT_RECORD = new AccountRecord(0, Cobol.lowValues(1), BigDecimal.ZERO.setScale(2),
            BigDecimal.ZERO.setScale(2), BigDecimal.ZERO.setScale(2), Cobol.lowValues(10), Cobol.lowValues(10),
            Cobol.lowValues(10), BigDecimal.ZERO.setScale(2), BigDecimal.ZERO.setScale(2), Cobol.lowValues(10),
            Cobol.lowValues(10));

    private final CardXrefRepository cardXrefRepository;
    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    private final Clock clock;

    public CoactvwcService(CardXrefRepository cardXrefRepository, AccountRepository accountRepository,
                           CustomerRepository customerRepository, Clock clock) {
        this.cardXrefRepository = Objects.requireNonNull(cardXrefRepository);
        this.accountRepository = Objects.requireNonNull(accountRepository);
        this.customerRepository = Objects.requireNonNull(customerRepository);
        this.clock = Objects.requireNonNull(clock);
    }

    public CoactvwcResult process(CoactvwcRequest request) {
        return new Task(request).main();
    }

    /** Working storage of one task (CICS gives every task a fresh copy). */
    private final class Task {
        private final CoactvwcRequest req;

        // WS-CICS-PROCESSNG-VARS
        int wsRespCd;
        int wsReasCd;
        String wsTranid;
        // Input edits
        char wsInputFlag;
        char wsPfkFlag;
        char wsEditAcctFlag;
        char wsEditCustFlag;
        // WS-XREF-RID
        String wsCardRidCardnum;
        long wsCardRidCustId;
        long wsCardRidAcctId;
        // WS-FILE-READ-FLAGS
        char wsAccountMasterReadFlag;
        char wsCustMasterReadFlag;
        // WS-FILE-ERROR-MESSAGE variable parts
        String errorOpname;
        String errorFile;
        String errorResp;
        String errorResp2;
        // Output message construction
        String wsLongMsg;
        String wsInfoMsg;
        String wsReturnMsg;

        final CcWorkArea ccWorkArea = new CcWorkArea();
        CardDemoCommarea carddemoCommarea = CardDemoCommarea.lowValues();
        ThisProgCommarea thisProgCommarea = ThisProgCommarea.lowValues();
        String wsCommarea;
        final AbendData abendData = new AbendData();
        AccountRecord accountRecord = UNREAD_ACCOUNT_RECORD;
        CustomerRecord customerRecord;
        CardXrefRecord cardXrefRecord;
        CactvwaInput cactvwai;
        CactvwaOutput cactvwao;

        Task(CoactvwcRequest req) {
            this.req = req;
        }

        // ---- 88-level predicates -------------------------------------------------------------------------
        boolean inputOk() { return wsInputFlag == '0'; }
        boolean inputError() { return wsInputFlag == '1'; }
        boolean pfkInvalid() { return wsPfkFlag == '1'; }
        boolean flgAcctfilterNotOk() { return wsEditAcctFlag == '0'; }
        boolean flgAcctfilterBlank() { return wsEditAcctFlag == ' '; }
        boolean foundAcctInMaster() { return wsAccountMasterReadFlag == '1'; }
        boolean foundCustInMaster() { return wsCustMasterReadFlag == '1'; }
        boolean wsNoInfoMessage() { return Cobol.isSpaces(wsInfoMsg) || Cobol.isLowValues(wsInfoMsg); }
        boolean wsReturnMsgOff() { return Cobol.isSpaces(wsReturnMsg); }
        boolean returnMsgIs(String value88) { return wsReturnMsg.equals(Cobol.alnum(value88, WS_RETURN_MSG_LEN)); }

        void setReturnMsg(String v) { wsReturnMsg = Cobol.alnum(v, WS_RETURN_MSG_LEN); }
        void setInfoMsg(String v) { wsInfoMsg = Cobol.alnum(v, WS_INFO_MSG_LEN); }

        /** INITIALIZE WS-MISC-STORAGE: alphanumerics to SPACES, numerics to ZERO (FILLERs untouched). */
        void initializeMiscStorage() {
            wsRespCd = 0;
            wsReasCd = 0;
            wsTranid = Cobol.spaces(4);
            wsInputFlag = ' ';
            wsPfkFlag = ' ';
            wsEditAcctFlag = ' ';
            wsEditCustFlag = ' ';
            wsCardRidCardnum = Cobol.spaces(16);
            wsCardRidCustId = 0;
            wsCardRidAcctId = 0;
            wsAccountMasterReadFlag = ' ';
            wsCustMasterReadFlag = ' ';
            errorOpname = Cobol.spaces(8);
            errorFile = Cobol.spaces(9);
            errorResp = Cobol.spaces(10);
            errorResp2 = Cobol.spaces(10);
            wsLongMsg = Cobol.spaces(500);
            wsInfoMsg = Cobol.spaces(WS_INFO_MSG_LEN);
            wsReturnMsg = Cobol.spaces(WS_RETURN_MSG_LEN);
        }

        // 0000-MAIN
        CoactvwcResult main() {
            try {
                // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE): see catch block.
                ccWorkArea.initialize();
                initializeMiscStorage();
                wsCommarea = Cobol.spaces(WS_COMMAREA_LEN);
                wsTranid = LIT_THISTRANID;
                setReturnMsg(Cobol.spaces(WS_RETURN_MSG_LEN)); // SET WS-RETURN-MSG-OFF

                // The menu test reads CARDDEMO-COMMAREA before anything has been moved into it.
                if (req.eibcalen() == 0
                        || (carddemoCommarea.fromProgram().equals(LIT_MENUPGM) && !carddemoCommarea.isPgmReenter())) {
                    carddemoCommarea = CardDemoCommarea.initialized();
                    thisProgCommarea = ThisProgCommarea.initialized();
                } else {
                    String raw = req.dfhcommarea();
                    carddemoCommarea = CardDemoCommarea.fromString(slice(raw, 0, CardDemoCommarea.LENGTH));
                    thisProgCommarea = ThisProgCommarea.fromString(
                            slice(raw, CardDemoCommarea.LENGTH, ThisProgCommarea.LENGTH));
                }

                StorePfKey.yyyyStorePfkey(req.eibaid(), ccWorkArea);

                wsPfkFlag = '1';
                if (ccWorkArea.isCcardAidEnter() || ccWorkArea.isCcardAidPfk03()) {
                    wsPfkFlag = '0';
                }
                if (pfkInvalid()) {
                    ccWorkArea.setCcardAid(CcardAid.ENTER);
                }

                if (ccWorkArea.isCcardAidPfk03()) {
                    return xctlToCaller();
                } else if (carddemoCommarea.isPgmEnter()) {
                    sendMap1000();
                    return commonReturn();
                } else if (carddemoCommarea.isPgmReenter()) {
                    processInputs2000();
                    if (inputError()) {
                        sendMap1000();
                        return commonReturn();
                    }
                    readAcct9000();
                    sendMap1000();
                    return commonReturn();
                } else {
                    abendData.setAbendCulprit(LIT_THISPGM);
                    abendData.setAbendCode("0001");
                    abendData.setAbendReason(Cobol.spaces(50));
                    setReturnMsg(UNEXPECTED_DATA_SCENARIO);
                    return sendPlainText();
                }
                // The trailing "IF INPUT-ERROR ... 1000-SEND-MAP" after END-EVALUATE is unreachable: every WHEN
                // ends in XCTL, GO TO COMMON-RETURN or SEND-PLAIN-TEXT (which RETURNs).
            } catch (RuntimeException e) {
                return abendRoutine(e);
            }
        }

        /** WHEN CCARD-AID-PFK03: XCTL TO CALLING PROGRAM OR MAIN MENU. */
        CoactvwcResult xctlToCaller() {
            String fromTranid = carddemoCommarea.fromTranid();
            if (Cobol.isLowValues(fromTranid) || Cobol.isSpaces(fromTranid)) {
                carddemoCommarea.setToTranid(LIT_MENUTRANID);
            } else {
                carddemoCommarea.setToTranid(fromTranid);
            }
            String fromProgram = carddemoCommarea.fromProgram();
            if (Cobol.isLowValues(fromProgram) || Cobol.isSpaces(fromProgram)) {
                carddemoCommarea.setToProgram(LIT_MENUPGM);
            } else {
                carddemoCommarea.setToProgram(fromProgram);
            }
            carddemoCommarea.setFromTranid(LIT_THISTRANID);
            carddemoCommarea.setFromProgram(LIT_THISPGM);
            carddemoCommarea.setUserType(CardDemoCommarea.UserType.USER);
            carddemoCommarea.setPgmContext(CardDemoCommarea.PgmContext.ENTER);
            carddemoCommarea.setLastMapset(LIT_THISMAPSET);
            carddemoCommarea.setLastMap(LIT_THISMAP);
            return new CoactvwcResult.Xctl(carddemoCommarea.toProgram(), carddemoCommarea.copy());
        }

        // COMMON-RETURN
        CoactvwcResult commonReturn() {
            ccWorkArea.setCcardErrorMsg(wsReturnMsg);
            wsCommarea = Cobol.alnum(carddemoCommarea.toString() + thisProgCommarea.toString(), WS_COMMAREA_LEN);
            return new CoactvwcResult.SendMapAndReturn(ccWorkArea.ccardNextMapset(), ccWorkArea.ccardNextMap(),
                    cactvwao, LIT_THISTRANID, wsCommarea);
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
            cactvwao = new CactvwaOutput();
            CurrentDateTime now = CurrentDateTime.of(LocalDateTime.now(clock));
            cactvwao.setTitle01o(ScreenTitles.CCDA_TITLE01);
            cactvwao.setTitle02o(ScreenTitles.CCDA_TITLE02);
            cactvwao.setTrnnameo(LIT_THISTRANID);
            cactvwao.setPgmnameo(LIT_THISPGM);
            cactvwao.setCurdateo(now.curdateMmDdYy());
            cactvwao.setCurtimeo(now.curtimeHhMmSs());
        }

        // 1200-SETUP-SCREEN-VARS
        void setupScreenVars1200() {
            if (req.eibcalen() == 0) {
                setInfoMsg(WS_PROMPT_FOR_INPUT);
            } else {
                if (flgAcctfilterBlank()) {
                    cactvwao.setAcctsido(null);
                } else {
                    cactvwao.setAcctsido(ccWorkArea.ccAcctId());
                }
                if (foundAcctInMaster() || foundCustInMaster()) {
                    AccountRecord a = accountRecord;
                    cactvwao.setAcsttuso(a.acctActiveStatus());
                    cactvwao.setAcurbalo(Cobol.editPlusZzzComma99(a.acctCurrBal()));
                    cactvwao.setAcrdlimo(Cobol.editPlusZzzComma99(a.acctCreditLimit()));
                    cactvwao.setAcshlimo(Cobol.editPlusZzzComma99(a.acctCashCreditLimit()));
                    cactvwao.setAcrcycro(Cobol.editPlusZzzComma99(a.acctCurrCycCredit()));
                    cactvwao.setAcrcydbo(Cobol.editPlusZzzComma99(a.acctCurrCycDebit()));
                    cactvwao.setAdtopeno(a.acctOpenDate());
                    cactvwao.setAexpdto(a.acctExpirationDate());
                    cactvwao.setAreisdto(a.acctReissueDate());
                    cactvwao.setAaddgrpo(a.acctGroupId());
                }
                if (foundCustInMaster()) {
                    CustomerRecord c = customerRecord;
                    cactvwao.setAcstnumo(Cobol.digits(c.custId(), 9));
                    String ssn = Cobol.digits(c.custSsn(), 9);
                    // STRING ... INTO ACSTSSNO: 11 bytes written, byte 12 keeps its LOW-VALUE.
                    cactvwao.setAcstssno(Cobol.stringInto(Cobol.lowValues(12),
                            ssn.substring(0, 3), "-", ssn.substring(3, 5), "-", ssn.substring(5, 9)));
                    cactvwao.setAcstfcoo(Cobol.digits(c.custFicoCreditScore(), 3));
                    cactvwao.setAcstdobo(c.custDobYyyyMmDd());
                    cactvwao.setAcsfnamo(c.custFirstName());
                    cactvwao.setAcsmnamo(c.custMiddleName());
                    cactvwao.setAcslnamo(c.custLastName());
                    cactvwao.setAcsadl1o(c.custAddrLine1());
                    cactvwao.setAcsadl2o(c.custAddrLine2());
                    cactvwao.setAcscityo(c.custAddrLine3());
                    cactvwao.setAcsstteo(c.custAddrStateCd());
                    cactvwao.setAcszipco(c.custAddrZip());
                    cactvwao.setAcsctryo(c.custAddrCountryCd());
                    cactvwao.setAcsphn1o(c.custPhoneNum1());
                    cactvwao.setAcsphn2o(c.custPhoneNum2());
                    cactvwao.setAcsgovto(c.custGovtIssuedId());
                    cactvwao.setAcseftco(c.custEftAccountId());
                    cactvwao.setAcspflgo(c.custPriCardHolderInd());
                }
            }
            // SETUP MESSAGE
            if (wsNoInfoMessage()) {
                setInfoMsg(WS_PROMPT_FOR_INPUT);
            }
            cactvwao.setErrmsgo(wsReturnMsg);
            cactvwao.setInfomsgo(wsInfoMsg);
        }

        // 1300-SETUP-SCREEN-ATTRS
        void setupScreenAttrs1300() {
            cactvwao.setAcctsida(DfhBmsca.DFHBMFSE);
            // POSITION CURSOR: every branch of the EVALUATE moves -1 to ACCTSIDL.
            cactvwao.setAcctsidl(-1);
            cactvwao.setAcctsidc(DfhBmsca.DFHDFCOL);
            if (flgAcctfilterNotOk()) {
                cactvwao.setAcctsidc(DfhBmsca.DFHRED);
            }
            if (flgAcctfilterBlank() && carddemoCommarea.isPgmReenter()) {
                cactvwao.setAcctsido("*");
                cactvwao.setAcctsidc(DfhBmsca.DFHRED);
            }
            if (wsNoInfoMessage()) {
                cactvwao.setInfomsgc(DfhBmsca.DFHBMDAR);
            } else {
                cactvwao.setInfomsgc(DfhBmsca.DFHNEUTR);
            }
        }

        // 1400-SEND-SCREEN
        void sendScreen1400() {
            ccWorkArea.setCcardNextMapset(LIT_THISMAPSET);
            ccWorkArea.setCcardNextMap(LIT_THISMAP);
            carddemoCommarea.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
            // EXEC CICS SEND MAP ... CURSOR ERASE FREEKB: the map is handed back in the result.
        }

        // 2000-PROCESS-INPUTS
        void processInputs2000() {
            receiveMap2100();
            editMapInputs2200();
            ccWorkArea.setCcardErrorMsg(wsReturnMsg);
            ccWorkArea.setCcardNextProg(LIT_THISPGM);
            ccWorkArea.setCcardNextMapset(LIT_THISMAPSET);
            ccWorkArea.setCcardNextMap(LIT_THISMAP);
        }

        // 2100-RECEIVE-MAP (RESP is captured but never tested, so MAPFAIL behaves like an empty map)
        void receiveMap2100() {
            cactvwai = req.mapInput() != null ? req.mapInput() : CactvwaInput.ofAcctsid(null);
        }

        // 2200-EDIT-MAP-INPUTS
        void editMapInputs2200() {
            wsInputFlag = '0';
            wsEditAcctFlag = '1';
            String acctsidi = cactvwai.acctsidiField();
            // REPLACE * WITH LOW-VALUES
            if (acctsidi.equals(Cobol.alnum("*", CactvwaInput.ACCTSID_LEN)) || Cobol.isSpaces(acctsidi)) {
                ccWorkArea.setCcAcctId(Cobol.lowValues(CcWorkArea.CC_ACCT_ID_LEN));
            } else {
                ccWorkArea.setCcAcctId(acctsidi);
            }
            editAccount2210();
            // CROSS FIELD EDITS
            if (flgAcctfilterBlank()) {
                setReturnMsg(NO_SEARCH_CRITERIA_RECEIVED);
            }
        }

        // 2210-EDIT-ACCOUNT
        void editAccount2210() {
            wsEditAcctFlag = '0';
            String acct = ccWorkArea.ccAcctId();
            // Not supplied
            if (Cobol.isLowValues(acct) || Cobol.isSpaces(acct)) {
                wsInputFlag = '1';
                wsEditAcctFlag = ' ';
                if (wsReturnMsgOff()) {
                    setReturnMsg(WS_PROMPT_FOR_ACCT);
                }
                carddemoCommarea.setAcctId(0);
                return;
            }
            // Not numeric / not 11 characters
            if (!Cobol.isNumeric(acct) || Cobol.isAll(acct, '0')) {
                wsInputFlag = '1';
                wsEditAcctFlag = '0';
                if (wsReturnMsgOff()) {
                    setReturnMsg(ACCOUNT_FILTER_NOT_NUMERIC);
                }
                carddemoCommarea.setAcctId(0);
                return;
            }
            carddemoCommarea.setAcctId(ccWorkArea.ccAcctIdN());
            wsEditAcctFlag = '1';
        }

        // 9000-READ-ACCT
        void readAcct9000() {
            setInfoMsg(Cobol.spaces(WS_INFO_MSG_LEN)); // SET WS-NO-INFO-MESSAGE
            wsCardRidAcctId = carddemoCommarea.acctId();
            getCardXrefByAcct9200();
            if (flgAcctfilterNotOk()) {
                return;
            }
            getAcctDataByAcct9300();
            // The NOTFND branch of 9300 never sets this 88 (the SET is commented out), so the read carries on.
            if (returnMsgIs(DID_NOT_FIND_ACCT_IN_ACCTDAT)) {
                return;
            }
            wsCardRidCustId = carddemoCommarea.custId();
            getCustDataByCust9400();
        }

        String wsCardRidAcctIdX() {
            return Cobol.digits(wsCardRidAcctId, 11);
        }

        String wsCardRidCustIdX() {
            return Cobol.digits(wsCardRidCustId, 9);
        }

        // 9200-GETCARDXREF-BYACCT
        void getCardXrefByAcct9200() {
            FileReadResult<CardXrefRecord> r = cardXrefRepository.readByAcctId(wsCardRidAcctIdX());
            wsRespCd = r.resp();
            wsReasCd = r.resp2();
            if (r.record() != null) {
                cardXrefRecord = r.record();
            }
            switch (wsRespCd) {
                case CicsResp.NORMAL -> {
                    carddemoCommarea.setCustId(cardXrefRecord.xrefCustId());
                    carddemoCommarea.setCardNum(Cobol.toLong(cardXrefRecord.xrefCardNum()));
                }
                case CicsResp.NOTFND -> {
                    wsInputFlag = '1';
                    wsEditAcctFlag = '0';
                    if (wsReturnMsgOff()) {
                        errorResp = respText(wsRespCd);
                        errorResp2 = respText(wsReasCd);
                        wsReturnMsg = Cobol.stringInto(wsReturnMsg, "Account:", wsCardRidAcctIdX(), " not found in",
                                " Cross ref file.  Resp:", errorResp, " Reas:", errorResp2);
                    }
                }
                default -> {
                    wsInputFlag = '1';
                    wsEditAcctFlag = '0';
                    fileError(LIT_CARDXREFNAME_ACCT_PATH);
                }
            }
        }

        // 9300-GETACCTDATA-BYACCT
        void getAcctDataByAcct9300() {
            FileReadResult<AccountRecord> r = accountRepository.read(wsCardRidAcctIdX());
            wsRespCd = r.resp();
            wsReasCd = r.resp2();
            if (r.record() != null) {
                accountRecord = r.record();
            }
            switch (wsRespCd) {
                case CicsResp.NORMAL -> wsAccountMasterReadFlag = '1';
                case CicsResp.NOTFND -> {
                    wsInputFlag = '1';
                    wsEditAcctFlag = '0';
                    if (wsReturnMsgOff()) {
                        errorResp = respText(wsRespCd);
                        errorResp2 = respText(wsReasCd);
                        wsReturnMsg = Cobol.stringInto(wsReturnMsg, "Account:", wsCardRidAcctIdX(), " not found in",
                                " Acct Master file.Resp:", errorResp, " Reas:", errorResp2);
                    }
                }
                default -> {
                    wsInputFlag = '1';
                    wsEditAcctFlag = '0';
                    fileError(LIT_ACCTFILENAME);
                }
            }
        }

        // 9400-GETCUSTDATA-BYCUST
        void getCustDataByCust9400() {
            FileReadResult<CustomerRecord> r = customerRepository.read(wsCardRidCustIdX());
            wsRespCd = r.resp();
            wsReasCd = r.resp2();
            if (r.record() != null) {
                customerRecord = r.record();
            }
            switch (wsRespCd) {
                case CicsResp.NORMAL -> wsCustMasterReadFlag = '1';
                case CicsResp.NOTFND -> {
                    wsInputFlag = '1';
                    wsEditCustFlag = '0';
                    errorResp = respText(wsRespCd);
                    errorResp2 = respText(wsReasCd);
                    if (wsReturnMsgOff()) {
                        wsReturnMsg = Cobol.stringInto(wsReturnMsg, "CustId:", wsCardRidCustIdX(), " not found",
                                " in customer master.Resp: ", errorResp, " REAS:", errorResp2);
                    }
                }
                default -> {
                    wsInputFlag = '1';
                    wsEditCustFlag = '0';
                    fileError(LIT_CUSTFILENAME);
                }
            }
        }

        /** WHEN OTHER of 92xx-94xx: build WS-FILE-ERROR-MESSAGE and MOVE it (80 bytes) to WS-RETURN-MSG (75). */
        void fileError(String fileName) {
            errorOpname = Cobol.alnum("READ", 8);
            errorFile = Cobol.alnum(fileName, 9);
            errorResp = respText(wsRespCd);
            errorResp2 = respText(wsReasCd);
            setReturnMsg(wsFileErrorMessage());
        }

        String wsFileErrorMessage() {
            return "File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp + ",RESP2 "
                    + errorResp2 + Cobol.spaces(5);
        }

        // SEND-PLAIN-TEXT
        CoactvwcResult sendPlainText() {
            return new CoactvwcResult.SendTextAndReturn(wsReturnMsg);
        }

        // ABEND-ROUTINE
        CoactvwcResult abendRoutine(RuntimeException cause) {
            if (Cobol.isLowValues(abendData.abendMsg())) {
                abendData.setAbendMsg(UNEXPECTED_ABEND_OCCURRED);
            }
            abendData.setAbendCulprit(LIT_THISPGM);
            return new CoactvwcResult.Abend("9999", abendData, cause);
        }
    }

    /** MOVE of a PIC S9(09) COMP value to PIC X(10): sign dropped, nine digits, left-justified. */
    static String respText(int value) {
        return Cobol.alnum(Cobol.digits(value, 9), 10);
    }

    private static String slice(String raw, int from, int len) {
        if (raw.length() <= from) {
            return "";
        }
        return raw.substring(from, Math.min(raw.length(), from + len));
    }
}

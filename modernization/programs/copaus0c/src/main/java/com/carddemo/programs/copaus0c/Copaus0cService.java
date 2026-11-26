package com.carddemo.programs.copaus0c;

import com.carddemo.programs.copaus0c.cobol.Cobol;
import com.carddemo.programs.copaus0c.copybook.AccountRecord;
import com.carddemo.programs.copaus0c.copybook.CardDemoCommarea;
import com.carddemo.programs.copaus0c.copybook.CardXrefRecord;
import com.carddemo.programs.copaus0c.copybook.CommonMessages;
import com.carddemo.programs.copaus0c.copybook.Copau0aInput;
import com.carddemo.programs.copaus0c.copybook.Copau0aMap;
import com.carddemo.programs.copaus0c.copybook.Copau0aScreen;
import com.carddemo.programs.copaus0c.copybook.CustomerRecord;
import com.carddemo.programs.copaus0c.copybook.DateTimeFields;
import com.carddemo.programs.copaus0c.copybook.Dfh;
import com.carddemo.programs.copaus0c.copybook.Dfh.Aid;
import com.carddemo.programs.copaus0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus0c.copybook.PendingAuthSummary;
import com.carddemo.programs.copaus0c.copybook.ScreenTitles;
import com.carddemo.programs.copaus0c.gateway.AccountRepository;
import com.carddemo.programs.copaus0c.gateway.CardXrefRepository;
import com.carddemo.programs.copaus0c.gateway.CustomerRepository;
import com.carddemo.programs.copaus0c.gateway.DliResult;
import com.carddemo.programs.copaus0c.gateway.FileReadResult;
import com.carddemo.programs.copaus0c.gateway.PendingAuthDbGateway;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * COPAUS0C - Summary view of pending authorization messages (transaction CPVS, map COPAU0A/COPAU00).
 * Framework-free port of the CICS/IMS online program; one {@link #execute} call is one CICS task.
 */
public final class Copaus0cService {

    public static final String WS_PGM_AUTH_SMRY = "COPAUS0C";
    public static final String WS_PGM_AUTH_DTL = "COPAUS1C";
    public static final String WS_PGM_MENU = "COMEN01C";
    public static final String WS_PGM_SIGNON = "COSGN00C";
    public static final String WS_CICS_TRANID = "CPVS";

    private final CardXrefRepository cardXrefRepository;
    private final AccountRepository accountRepository;
    private final CustomerRepository customerRepository;
    private final PendingAuthDbGateway pautDb;
    private final Clock clock;

    public Copaus0cService(CardXrefRepository cardXrefRepository, AccountRepository accountRepository,
                           CustomerRepository customerRepository, PendingAuthDbGateway pautDb, Clock clock) {
        this.cardXrefRepository = cardXrefRepository;
        this.accountRepository = accountRepository;
        this.customerRepository = customerRepository;
        this.pautDb = pautDb;
        this.clock = clock;
    }

    /**
     * Runs one task.
     *
     * @param eibaid   EIBAID (attention key)
     * @param commarea DFHCOMMAREA, or {@code null} when EIBCALEN = 0; never mutated
     * @param input    terminal input for EXEC CICS RECEIVE MAP (only used on re-entry)
     */
    public Copaus0cResult execute(Aid eibaid, CardDemoCommarea commarea, Copau0aInput input) {
        Task task = new Task(eibaid, commarea == null ? null : commarea.copy(),
                input == null ? Copau0aInput.none() : input);
        try {
            task.mainPara();
            return new Copaus0cResult(task.sends, null, WS_CICS_TRANID, task.ca);
        } catch (Xctl x) {
            return new Copaus0cResult(task.sends, x.program, null, task.ca);
        } finally {
            if (task.imsPsbSchd) {
                pautDb.syncpoint(); // implicit syncpoint at task end
            }
        }
    }

    /** EXEC CICS XCTL: control never returns to COPAUS0C. */
    private static final class Xctl extends RuntimeException {
        final String program;

        Xctl(String program) {
            super(null, null, false, false);
            this.program = program;
        }
    }

    private static final AccountRecord EMPTY_ACCOUNT = new AccountRecord(0, Cobol.lowValues(1), BigDecimal.ZERO,
            BigDecimal.ZERO, BigDecimal.ZERO, Cobol.lowValues(10), Cobol.lowValues(10), Cobol.lowValues(10),
            BigDecimal.ZERO, BigDecimal.ZERO, Cobol.lowValues(10), Cobol.lowValues(10));
    private static final CustomerRecord EMPTY_CUSTOMER = new CustomerRecord(0, Cobol.lowValues(25),
            Cobol.lowValues(25), Cobol.lowValues(25), Cobol.lowValues(50), Cobol.lowValues(50), Cobol.lowValues(50),
            Cobol.lowValues(2), Cobol.lowValues(3), Cobol.lowValues(10), Cobol.lowValues(15), Cobol.lowValues(15), 0,
            Cobol.lowValues(20), Cobol.lowValues(10), Cobol.lowValues(10), Cobol.lowValues(1), 0);
    private static final CardXrefRecord EMPTY_XREF = new CardXrefRecord(Cobol.lowValues(16), 0, 0);

    /** WORKING-STORAGE of one task plus the paragraphs operating on it. */
    private final class Task {
        final Aid eibaid;
        final boolean eibcalenZero;
        CardDemoCommarea ca;
        final Copau0aInput input;
        final Copau0aMap map = new Copau0aMap();
        final DateTimeFields dt = new DateTimeFields();
        final List<Copau0aScreen> sends = new ArrayList<>();

        String wsMessage = Cobol.spaces(80);       // WS-MESSAGE PIC X(80)
        String wsAcctId = Cobol.lowValues(11);     // WS-ACCT-ID PIC X(11)
        String wsAuthKeySave = Cobol.lowValues(8); // WS-AUTH-KEY-SAVE PIC X(08)
        String wsAuthAprvStat = " ";               // WS-AUTH-APRV-STAT PIC X(01)
        int wsRespCd;                              // WS-RESP-CD PIC S9(09) COMP
        int wsReasCd;                              // WS-REAS-CD PIC S9(09) COMP
        int wsIdx;                                 // WS-IDX PIC S9(04) COMP
        String wsAuthAmt = Cobol.spaces(12);       // WS-AUTH-AMT PIC -zzzzzzz9.99
        String wsAuthDate = "00/00/00";            // WS-AUTH-DATE PIC X(08)
        String wsAuthTime = "00:00:00";            // WS-AUTH-TIME PIC X(08)
        String imsReturnCode = "  ";               // IMS-RETURN-CODE PIC X(02)
        boolean imsPsbSchd;                        // WS-IMS-PSB-SCHD-FLG 88 IMS-PSB-SCHD
        Boolean pautSmrySegFound;                  // WS-PAUT-SMRY-SEG-FLG (null = never set)
        boolean errFlgOn;                          // WS-ERR-FLG 88 ERR-FLG-ON
        boolean authsEof;                          // WS-AUTHS-EOF 88 AUTHS-EOF
        boolean sendEraseYes = true;               // WS-SEND-ERASE-FLG 88 SEND-ERASE-YES

        CardXrefRecord cardXrefRecord = EMPTY_XREF;
        AccountRecord accountRecord = EMPTY_ACCOUNT;
        CustomerRecord customerRecord = EMPTY_CUSTOMER;
        PendingAuthSummary pendingAuthSummary;
        PendingAuthDetails pendingAuthDetails;
        String paAuthorizationKey = Cobol.lowValues(8);

        Task(Aid eibaid, CardDemoCommarea commarea, Copau0aInput input) {
            this.eibaid = eibaid;
            this.eibcalenZero = commarea == null;
            this.ca = commarea == null ? new CardDemoCommarea() : commarea;
            this.input = input;
        }

        // MAIN-PARA
        void mainPara() {
            errFlgOn = false;
            authsEof = false;
            // SET NEXT-PAGE-NO: overwritten by INITIALIZE / MOVE DFHCOMMAREA below, so it has no effect
            sendEraseYes = true;
            wsMessage = Cobol.spaces(80);
            map.errmsg.move(Cobol.spaces(78));
            map.acctid.length = -1;

            if (eibcalenZero) {
                ca.initialize();
                ca.cdemoToProgram = Cobol.moveX(WS_PGM_AUTH_SMRY, 8);
                ca.cdemoPgmContext = CardDemoCommarea.CDEMO_PGM_REENTER;
                map.lowValues();
                map.acctid.length = -1;
                sendPaulstScreen();
            } else if (!ca.isCdemoPgmReenter()) {
                ca.cdemoPgmContext = CardDemoCommarea.CDEMO_PGM_REENTER;
                map.lowValues();
                if (Cobol.isNumeric(ca.cdemoAcctId)) {
                    wsAcctId = ca.cdemoAcctId;
                    map.acctid.move(ca.cdemoAcctId);
                } else {
                    map.acctid.move(" ");
                    wsAcctId = Cobol.lowValues(11);
                }
                gatherDetails();
                sendEraseYes = true;
                sendPaulstScreen();
            } else {
                receivePaulstScreen();
                switch (eibaid) {
                    case ENTER -> {
                        processEnterKey();
                        if (Cobol.isAll(wsAcctId, Cobol.LOW_VALUE)) {
                            map.acctid.move(" ");
                        } else {
                            map.acctid.move(wsAcctId);
                        }
                        sendPaulstScreen();
                    }
                    case PF3 -> {
                        ca.cdemoToProgram = Cobol.moveX(WS_PGM_MENU, 8);
                        returnToPrevScreen();
                        sendPaulstScreen();
                    }
                    case PF7 -> {
                        processPf7Key();
                        sendPaulstScreen();
                    }
                    case PF8 -> {
                        processPf8Key();
                        sendPaulstScreen();
                    }
                    default -> {
                        errFlgOn = true;
                        map.acctid.length = -1;
                        wsMessage = Cobol.moveX(CommonMessages.CCDA_MSG_INVALID_KEY, 80);
                        sendPaulstScreen();
                    }
                }
            }
            // EXEC CICS RETURN TRANSID(WS-CICS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
        }

        // PROCESS-ENTER-KEY
        void processEnterKey() {
            String acctIdI = map.acctid.get();
            if (Cobol.isSpacesOrLowValues(acctIdI)) {
                wsAcctId = Cobol.lowValues(11);
                errFlgOn = true;
                wsMessage = Cobol.moveX("Please enter Acct Id...", 80);
                map.acctid.length = -1;
            } else if (!Cobol.isNumeric(acctIdI)) {
                wsAcctId = Cobol.lowValues(11);
                errFlgOn = true;
                wsMessage = Cobol.moveX("Acct Id must be Numeric ...", 80);
                map.acctid.length = -1;
            } else {
                wsAcctId = acctIdI;
                ca.cdemoAcctId = acctIdI;
                int selected = 0;
                for (int i = 1; i <= 5 && selected == 0; i++) {
                    if (!Cobol.isSpacesOrLowValues(map.row(i).sel.get())) {
                        selected = i;
                    }
                }
                if (selected > 0) {
                    ca.cdemoCpvsPauSelFlg = map.row(selected).sel.get();
                    ca.cdemoCpvsPauSelected = ca.authKey(selected);
                } else {
                    ca.cdemoCpvsPauSelFlg = Cobol.spaces(1);
                    ca.cdemoCpvsPauSelected = Cobol.spaces(8);
                }
                if (!Cobol.isSpacesOrLowValues(ca.cdemoCpvsPauSelFlg)
                        && !Cobol.isSpacesOrLowValues(ca.cdemoCpvsPauSelected)) {
                    switch (ca.cdemoCpvsPauSelFlg) {
                        case "S", "s" -> {
                            ca.cdemoToProgram = Cobol.moveX(WS_PGM_AUTH_DTL, 8);
                            ca.cdemoFromTranid = WS_CICS_TRANID;
                            ca.cdemoFromProgram = Cobol.moveX(WS_PGM_AUTH_SMRY, 8);
                            ca.cdemoPgmContext = 0;
                            ca.cdemoPgmContext = CardDemoCommarea.CDEMO_PGM_ENTER;
                            xctl(ca.cdemoToProgram);
                        }
                        default -> {
                            wsMessage = Cobol.moveX("Invalid selection. Valid value is S", 80);
                            map.acctid.length = -1;
                        }
                    }
                }
            }
            gatherDetails();
        }

        // GATHER-DETAILS
        void gatherDetails() {
            map.acctid.length = -1;
            ca.cdemoCpvsPageNum = 0;
            if (!Cobol.isAll(wsAcctId, Cobol.LOW_VALUE)) {
                gatherAccountDetails();
                initializeAuthData();
                if (Boolean.TRUE.equals(pautSmrySegFound)) {
                    processPageForward();
                }
            }
        }

        // PROCESS-PF7-KEY
        void processPf7Key() {
            if (ca.cdemoCpvsPageNum > 1) {
                ca.cdemoCpvsPageNum = ca.cdemoCpvsPageNum - 1;
                wsAuthKeySave = ca.paukeyPrevPg(ca.cdemoCpvsPageNum);
                getAuthSummary();
                sendEraseYes = false;
                ca.cdemoCpvsNextPageFlg = "Y";
                map.acctid.length = -1;
                initializeAuthData();
                processPageForward();
            } else {
                wsMessage = Cobol.moveX("You are already at the top of the page...", 80);
                sendEraseYes = false;
            }
        }

        // PROCESS-PF8-KEY
        void processPf8Key() {
            if (Cobol.isSpacesOrLowValues(ca.cdemoCpvsPaukeyLast)) {
                wsAuthKeySave = Cobol.lowValues(8);
            } else {
                wsAuthKeySave = ca.cdemoCpvsPaukeyLast;
                getAuthSummary();
                repositionAuthorizations();
            }
            map.acctid.length = -1;
            sendEraseYes = false;
            if (ca.isNextPageYes()) {
                initializeAuthData();
                processPageForward();
            } else {
                wsMessage = Cobol.moveX("You are already at the bottom of the page...", 80);
            }
        }

        // PROCESS-PAGE-FORWARD
        void processPageForward() {
            if (!errFlgOn) {
                wsIdx = 1;
                ca.cdemoCpvsPaukeyLast = Cobol.lowValues(8);
                while (!(wsIdx > 5 || authsEof || errFlgOn)) {
                    if (eibaid == Aid.PF7 && wsIdx == 1) {
                        repositionAuthorizations();
                    } else {
                        getAuthorizations();
                    }
                    if (!authsEof && !errFlgOn) {
                        populateAuthList();
                        wsIdx = wsIdx + 1;
                        ca.cdemoCpvsPaukeyLast = paAuthorizationKey;
                        if (wsIdx == 2) {
                            ca.cdemoCpvsPageNum = ca.cdemoCpvsPageNum + 1;
                            ca.paukeyPrevPg(ca.cdemoCpvsPageNum, paAuthorizationKey);
                        }
                    }
                }
                if (!authsEof && !errFlgOn) {
                    getAuthorizations();
                    ca.cdemoCpvsNextPageFlg = (!authsEof && !errFlgOn) ? "Y" : "N";
                }
            }
        }

        // GET-AUTHORIZATIONS
        void getAuthorizations() {
            DliResult<PendingAuthDetails> r = pautDb.getNextDetail();
            evaluateDetailStatus(r, " System error while reading AUTH Details: Code:");
        }

        // REPOSITION-AUTHORIZATIONS
        void repositionAuthorizations() {
            paAuthorizationKey = wsAuthKeySave;
            DliResult<PendingAuthDetails> r = pautDb.getNextDetailByKey(paAuthorizationKey);
            evaluateDetailStatus(r, " System error while repos. AUTH Details: Code:");
        }

        private void evaluateDetailStatus(DliResult<PendingAuthDetails> r, String errorText) {
            imsReturnCode = Cobol.moveX(r.dibstat(), 2);
            ImsStatus status = ImsStatus.of(imsReturnCode);
            if (r.segment() != null) {
                pendingAuthDetails = r.segment();
                paAuthorizationKey = r.segment().authorizationKey();
            }
            if (status.isStatusOk()) {
                authsEof = false;
            } else if (status == ImsStatus.SEGMENT_NOT_FOUND || status == ImsStatus.END_OF_DB) {
                authsEof = true;
            } else {
                errFlgOn = true;
                wsMessage = Cobol.stringInto(wsMessage, errorText, imsReturnCode);
                map.acctid.length = -1;
                sendPaulstScreen();
            }
        }

        // POPULATE-AUTH-LIST
        void populateAuthList() {
            PendingAuthDetails d = pendingAuthDetails;
            wsAuthAmt = Cobol.editMinusZ9V99(d.paApprovedAmt(), 8);
            String t = d.paAuthOrigTime();
            wsAuthTime = t.substring(0, 2) + wsAuthTime.charAt(2) + t.substring(2, 4) + wsAuthTime.charAt(5)
                    + t.substring(4, 6);
            String o = d.paAuthOrigDate();
            dt.wsCurdateYy = o.substring(0, 2);
            dt.wsCurdateMm = o.substring(2, 4);
            dt.wsCurdateDd = o.substring(4, 6);
            wsAuthDate = dt.curdateMmDdYy();
            wsAuthAprvStat = "00".equals(d.paAuthRespCode()) ? "A" : "D";
            if (wsIdx >= 1 && wsIdx <= 5) {
                ca.authKey(wsIdx, paAuthorizationKey);
                Copau0aMap.Row row = map.row(wsIdx);
                row.trnid.move(d.paTransactionId());
                row.pdate.move(wsAuthDate);
                row.ptime.move(wsAuthTime);
                row.ptype.move(d.paAuthType());
                row.paprv.move(wsAuthAprvStat);
                row.pstat.move(d.paMatchStatus());
                row.pamt.move(wsAuthAmt);
                row.sel.attr = Dfh.DFHBMUNP;
            }
        }

        // INITIALIZE-AUTH-DATA
        void initializeAuthData() {
            for (wsIdx = 1; wsIdx <= 5; wsIdx++) {
                Copau0aMap.Row row = map.row(wsIdx);
                row.sel.attr = Dfh.DFHBMPRO;
                row.trnid.move(" ");
                row.pdate.move(" ");
                row.ptime.move(" ");
                row.ptype.move(" ");
                row.paprv.move(" ");
                row.pstat.move(" ");
                row.pamt.move(" ");
            }
        }

        // RETURN-TO-PREV-SCREEN
        void returnToPrevScreen() {
            if (Cobol.isSpacesOrLowValues(ca.cdemoToProgram)) {
                ca.cdemoToProgram = Cobol.moveX(WS_PGM_SIGNON, 8);
            }
            ca.cdemoFromTranid = WS_CICS_TRANID;
            ca.cdemoFromProgram = Cobol.moveX(WS_PGM_AUTH_SMRY, 8);
            ca.cdemoPgmContext = 0;
            xctl(ca.cdemoToProgram);
        }

        private void xctl(String program) {
            throw new Xctl(program.trim());
        }

        // SEND-PAULST-SCREEN
        void sendPaulstScreen() {
            if (imsPsbSchd) {
                imsPsbSchd = false;
                pautDb.syncpoint();
            }
            populateHeaderInfo();
            map.errmsg.move(wsMessage);
            sends.add(map.snapshot(sendEraseYes));
        }

        // RECEIVE-PAULST-SCREEN
        void receivePaulstScreen() {
            map.receive(input);
            wsRespCd = FileReadResult.NORMAL;
            wsReasCd = 0;
        }

        // POPULATE-HEADER-INFO
        void populateHeaderInfo() {
            dt.setCurrentDate(LocalDateTime.now(clock));
            map.title01.move(ScreenTitles.CCDA_TITLE01);
            map.title02.move(ScreenTitles.CCDA_TITLE02);
            map.trnname.move(WS_CICS_TRANID);
            map.pgmname.move(WS_PGM_AUTH_SMRY);
            dt.wsCurdateMm = dt.wsCurdateMonth;
            dt.wsCurdateDd = dt.wsCurdateDay;
            dt.wsCurdateYy = dt.wsCurdateYear.substring(2, 4);
            map.curdate.move(dt.curdateMmDdYy());
            dt.wsCurtimeHh = dt.wsCurtimeHours;
            dt.wsCurtimeMm = dt.wsCurtimeMinute;
            dt.wsCurtimeSs = dt.wsCurtimeSecond;
            map.curtime.move(dt.curtimeHhMmSs());
        }

        // GATHER-ACCOUNT-DETAILS
        void gatherAccountDetails() {
            getcardxrefByacct();
            getacctdataByacct();
            getcustdataBycust();

            CustomerRecord c = customerRecord;
            map.custid.move(Cobol.moveToUnsignedDisplay(c.custId(), 9));
            map.cname.setRaw(Cobol.stringInto(map.cname.get(),
                    Cobol.delimitedBy(c.custFirstName(), " "), " ", c.custMiddleName().substring(0, 1), " ",
                    Cobol.delimitedBy(c.custLastName(), " ")));
            map.addr001.setRaw(Cobol.stringInto(map.addr001.get(),
                    Cobol.delimitedBy(c.custAddrLine1(), "  "), ",", Cobol.delimitedBy(c.custAddrLine2(), "  ")));
            map.addr002.setRaw(Cobol.stringInto(map.addr002.get(),
                    Cobol.delimitedBy(c.custAddrLine3(), "  "), ",", c.custAddrStateCd(), ",",
                    c.custAddrZip().substring(0, 5)));
            map.phone1.move(c.custPhoneNum1());
            map.credlim.move(Cobol.editMinusZ9V99(accountRecord.acctCreditLimit(), 8));
            map.cashlim.move(Cobol.editMinusZ9V99(accountRecord.acctCashCreditLimit(), 5));

            getAuthSummary();
            if (Boolean.TRUE.equals(pautSmrySegFound)) {
                PendingAuthSummary s = pendingAuthSummary;
                map.apprcnt.move(Cobol.moveToUnsignedDisplay(s.paApprovedAuthCnt(), 3));
                map.declcnt.move(Cobol.moveToUnsignedDisplay(s.paDeclinedAuthCnt(), 3));
                map.credbal.move(Cobol.editMinusZ9V99(s.paCreditBalance(), 8));
                map.cashbal.move(Cobol.editMinusZ9V99(s.paCashBalance(), 5));
                map.appramt.move(Cobol.editMinusZ9V99(s.paApprovedAuthAmt(), 5));
                map.declamt.move(Cobol.editMinusZ9V99(s.paDeclinedAuthAmt(), 5));
            } else {
                for (Copau0aMap.Field f : List.of(map.apprcnt, map.declcnt, map.credbal, map.cashbal,
                        map.appramt, map.declamt)) {
                    f.move("0".repeat(f.size)); // MOVE ZERO to an alphanumeric item fills it with zeros
                }
            }
        }

        // GETCARDXREF-BYACCT
        void getcardxrefByacct() {
            String ridAcctIdX = wsAcctId;
            FileReadResult<CardXrefRecord> r = cardXrefRepository.readByAcctId(ridAcctIdX);
            readResponse(r);
            if (r.resp() == FileReadResult.NORMAL) {
                cardXrefRecord = r.record();
                ca.cdemoCustId = Cobol.moveToUnsignedDisplay(cardXrefRecord.xrefCustId(), 9);
                ca.cdemoCardNum = cardXrefRecord.xrefCardNum();
            } else if (r.resp() == FileReadResult.NOTFND) {
                fileError("Account:", wsAcctId, " not found in XREF file. Resp:", false);
            } else {
                fileError("Account:", ridAcctIdX, " System error while reading XREF file. Resp:", true);
            }
        }

        // GETACCTDATA-BYACCT
        void getacctdataByacct() {
            String ridAcctIdX = Cobol.moveToUnsignedDisplay(cardXrefRecord.xrefAcctId(), 11);
            FileReadResult<AccountRecord> r = accountRepository.read(ridAcctIdX);
            readResponse(r);
            if (r.resp() == FileReadResult.NORMAL) {
                accountRecord = r.record();
            } else if (r.resp() == FileReadResult.NOTFND) {
                fileError("Account:", ridAcctIdX, " not found in ACCT file. Resp:", false);
            } else {
                fileError("Account:", ridAcctIdX, " System error while reading ACCT file. Resp:", true);
            }
        }

        // GETCUSTDATA-BYCUST
        void getcustdataBycust() {
            String ridCustIdX = Cobol.moveToUnsignedDisplay(cardXrefRecord.xrefCustId(), 9);
            FileReadResult<CustomerRecord> r = customerRepository.read(ridCustIdX);
            readResponse(r);
            if (r.resp() == FileReadResult.NORMAL) {
                customerRecord = r.record();
            } else if (r.resp() == FileReadResult.NOTFND) {
                fileError("Customer:", ridCustIdX, " not found in CUST file. Resp:", false);
            } else {
                fileError("Customer:", ridCustIdX, " System error while reading CUST file. Resp:", true);
            }
        }

        private void readResponse(FileReadResult<?> r) {
            wsRespCd = r.resp();
            wsReasCd = r.resp2();
        }

        private void fileError(String label, String key, String text, boolean setErrFlag) {
            if (setErrFlag) {
                errFlgOn = true;
            }
            String respDis = Cobol.moveToUnsignedDisplay(wsRespCd, 9);
            String reasDis = Cobol.moveToUnsignedDisplay(wsReasCd, 9);
            wsMessage = Cobol.stringInto(wsMessage, label, key, text, respDis, " Reas:", reasDis);
            map.acctid.length = -1;
            sendPaulstScreen();
        }

        // GET-AUTH-SUMMARY
        void getAuthSummary() {
            schedulePsb();
            long paAcctId = Cobol.displayValue(ca.cdemoAcctId);
            DliResult<PendingAuthSummary> r = pautDb.getUniqueSummary(paAcctId);
            imsReturnCode = Cobol.moveX(r.dibstat(), 2);
            if (r.segment() != null) {
                pendingAuthSummary = r.segment();
            }
            ImsStatus status = ImsStatus.of(imsReturnCode);
            if (status.isStatusOk()) {
                pautSmrySegFound = true;
            } else if (status == ImsStatus.SEGMENT_NOT_FOUND) {
                pautSmrySegFound = false;
            } else {
                errFlgOn = true;
                wsMessage = Cobol.stringInto(wsMessage, " System error while reading AUTH Summary: Code:",
                        imsReturnCode);
                map.acctid.length = -1;
                sendPaulstScreen();
            }
        }

        // SCHEDULE-PSB
        void schedulePsb() {
            imsReturnCode = Cobol.moveX(pautDb.schedulePsb(PendingAuthDbGateway.PSB_NAME), 2);
            if (ImsStatus.of(imsReturnCode) == ImsStatus.PSB_SCHEDULED_MORE_THAN_ONCE) {
                pautDb.terminate();
                imsReturnCode = Cobol.moveX(pautDb.schedulePsb(PendingAuthDbGateway.PSB_NAME), 2);
            }
            if (ImsStatus.of(imsReturnCode).isStatusOk()) {
                imsPsbSchd = true;
            } else {
                errFlgOn = true;
                wsMessage = Cobol.stringInto(wsMessage, " System error while scheduling PSB: Code:",
                        imsReturnCode);
                map.acctid.length = -1;
                sendPaulstScreen();
            }
        }
    }
}

package com.carddemo.online.coactupc;

import static com.carddemo.runtime.bms.BmsAttributes.DFHBMASB;
import static com.carddemo.runtime.bms.BmsAttributes.DFHBMDAR;
import static com.carddemo.runtime.bms.BmsAttributes.DFHBMFSE;
import static com.carddemo.runtime.bms.BmsAttributes.DFHBMPRF;
import static com.carddemo.runtime.bms.BmsAttributes.DFHDFCOL;
import static com.carddemo.runtime.bms.BmsAttributes.DFHRED;

import com.carddemo.common.copybook.AbendData;
import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.common.copybook.DateTimeFields;
import com.carddemo.common.copybook.ScreenTitles;
import com.carddemo.online.coactupc.AcupRecords.Rec;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsContext.SendMapOption;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cics.PfKeys;
import com.carddemo.runtime.cobol.CobolDates;
import com.carddemo.runtime.cobol.CobolNumval;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericEdited;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.vsam.FileResult;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COACTUPC - account update (transaction CAUP, mapset COACTUP / map CACTUPA). Each method is the COBOL
 * paragraph of the same name; line numbers refer to app/cbl/COACTUPC.cbl (CSUTLDPY/CSSTRPFY/CSSETATY
 * paragraphs are copied in from app/cpy). Bug-for-bug faithful: see docs/mapping/COACTUPC.md.
 */
@Component
public class Coactupc implements CicsProgram {

    public static final String PROGRAM = "COACTUPC";
    public static final String TRANSID = "CAUP";
    public static final String MAPSET = "COACTUP";
    public static final String MAP = "CACTUPA";
    public static final String MENU_PROGRAM = "COMEN01C";
    public static final String MENU_TRANSID = "CM00";
    static final String CCLIST_MAPSET = "COCRDLI";
    static final String ACCT_FILE = "ACCTDAT ";
    static final String CUST_FILE = "CUSTDAT ";
    static final String XREF_ACCT_PATH = "CXACAIX ";
    static final String CURRENCY_PIC = "+ZZZ,ZZZ,ZZZ.99";

    // WS-INFO-MSG 88s (lines 463-477)
    static final String FOUND_ACCOUNT_DATA = "Details of selected account shown above";
    static final String PROMPT_FOR_SEARCH_KEYS = "Enter or update id of account to update";
    static final String PROMPT_FOR_CHANGES = "Update account details presented above.";
    static final String PROMPT_FOR_CONFIRMATION = "Changes validated.Press F5 to save";
    static final String CONFIRM_UPDATE_SUCCESS = "Changes committed to database";
    static final String INFORM_FAILURE = "Changes unsuccessful. Please try again";
    // WS-RETURN-MSG 88s (lines 479-528)
    static final String NO_SEARCH_CRITERIA_RECEIVED = "No input received";
    static final String NO_CHANGES_DETECTED = "No change detected with respect to values fetched.";
    static final String DID_NOT_FIND_ACCT_IN_ACCTDAT = "Did not find this account in account master file";
    static final String DID_NOT_FIND_CUST_IN_CUSTDAT = "Did not find associated customer in master file";
    static final String COULD_NOT_LOCK_ACCT_FOR_UPDATE = "Could not lock account record for update";
    static final String COULD_NOT_LOCK_CUST_FOR_UPDATE = "Could not lock customer record for update";
    static final String DATA_WAS_CHANGED_BEFORE_UPDATE = "Record changed by some one else. Please review";
    static final String LOCKED_BUT_UPDATE_FAILED = "Update of record failed";

    /** WS-NON-KEY-FLAGS (lines 191-385) in storage order, plus the SSN part flags (WS-EDIT-US-SSN-FLGS). */
    static final List<String> NON_KEY_FLAGS = List.of("ACCT-STATUS", "CRED-LIMIT", "CASH-CREDIT-LIMIT", "CURR-BAL",
            "CURR-CYC-CREDIT", "CURR-CYC-DEBIT", "DT-OF-BIRTH-YEAR", "DT-OF-BIRTH-MONTH", "DT-OF-BIRTH-DAY",
            "FICO-SCORE", "OPEN-YEAR", "OPEN-MONTH", "OPEN-DAY", "EXPIRY-YEAR", "EXPIRY-MONTH", "EXPIRY-DAY",
            "REISSUE-YEAR", "REISSUE-MONTH", "REISSUE-DAY", "FIRST-NAME", "MIDDLE-NAME", "LAST-NAME",
            "ADDRESS-LINE-1", "ADDRESS-LINE-2", "CITY", "STATE", "ZIPCODE", "COUNTRY", "PHONE-NUM-1A",
            "PHONE-NUM-1B", "PHONE-NUM-1C", "PHONE-NUM-2A", "PHONE-NUM-2B", "PHONE-NUM-2C", "EFT-ACCOUNT-ID",
            "PRI-CARDHOLDER");

    /** 3300 cursor EVALUATE order (lines 3008-3205): flag -> screen field; middle name only on NOT-OK. */
    static final String[][] CURSOR_ORDER = {
        {"ACCT-STATUS", "ACSTTUS"}, {"OPEN-YEAR", "OPNYEAR"}, {"OPEN-MONTH", "OPNMON"}, {"OPEN-DAY", "OPNDAY"},
        {"CRED-LIMIT", "ACRDLIM"}, {"EXPIRY-YEAR", "EXPYEAR"}, {"EXPIRY-MONTH", "EXPMON"}, {"EXPIRY-DAY", "EXPDAY"},
        {"CASH-CREDIT-LIMIT", "ACSHLIM"}, {"REISSUE-YEAR", "RISYEAR"}, {"REISSUE-MONTH", "RISMON"},
        {"REISSUE-DAY", "RISDAY"}, {"CURR-BAL", "ACURBAL"}, {"CURR-CYC-CREDIT", "ACRCYCR"},
        {"CURR-CYC-DEBIT", "ACRCYDB"}, {"EDIT-US-SSN-PART1", "ACTSSN1"}, {"EDIT-US-SSN-PART2", "ACTSSN2"},
        {"EDIT-US-SSN-PART3", "ACTSSN3"}, {"DT-OF-BIRTH-YEAR", "DOBYEAR"}, {"DT-OF-BIRTH-MONTH", "DOBMON"},
        {"DT-OF-BIRTH-DAY", "DOBDAY"}, {"FICO-SCORE", "ACSTFCO"}, {"FIRST-NAME", "ACSFNAM"},
        {"MIDDLE-NAME", "ACSMNAM"}, {"LAST-NAME", "ACSLNAM"}, {"ADDRESS-LINE-1", "ACSADL1"}, {"STATE", "ACSSTTE"},
        {"ZIPCODE", "ACSZIPC"}, {"CITY", "ACSCITY"}, {"COUNTRY", "ACSCTRY"}, {"PHONE-NUM-1A", "ACSPH1A"},
        {"PHONE-NUM-1B", "ACSPH1B"}, {"PHONE-NUM-1C", "ACSPH1C"}, {"PHONE-NUM-2A", "ACSPH2A"},
        {"PHONE-NUM-2B", "ACSPH2B"}, {"PHONE-NUM-2C", "ACSPH2C"}, {"EFT-ACCOUNT-ID", "ACSEFTC"},
        {"PRI-CARDHOLDER", "ACSPFLG"}};

    /** COPY CSSETATY REPLACING (TESTVAR1)/(SCRNVAR2), in source order (lines 3208-3436). */
    static final String[][] CSSETATY = {
        {"ACCT-STATUS", "ACSTTUS"}, {"OPEN-YEAR", "OPNYEAR"}, {"OPEN-MONTH", "OPNMON"}, {"OPEN-DAY", "OPNDAY"},
        {"CRED-LIMIT", "ACRDLIM"}, {"EXPIRY-YEAR", "EXPYEAR"}, {"EXPIRY-MONTH", "EXPMON"}, {"EXPIRY-DAY", "EXPDAY"},
        {"CASH-CREDIT-LIMIT", "ACSHLIM"}, {"REISSUE-YEAR", "RISYEAR"}, {"REISSUE-MONTH", "RISMON"},
        {"REISSUE-DAY", "RISDAY"}, {"CURR-BAL", "ACURBAL"}, {"CURR-CYC-CREDIT", "ACRCYCR"},
        {"CURR-CYC-DEBIT", "ACRCYDB"}, {"EDIT-US-SSN-PART1", "ACTSSN1"}, {"EDIT-US-SSN-PART2", "ACTSSN2"},
        {"EDIT-US-SSN-PART3", "ACTSSN3"}, {"DT-OF-BIRTH-YEAR", "DOBYEAR"}, {"DT-OF-BIRTH-MONTH", "DOBMON"},
        {"DT-OF-BIRTH-DAY", "DOBDAY"}, {"FICO-SCORE", "ACSTFCO"}, {"FIRST-NAME", "ACSFNAM"},
        {"MIDDLE-NAME", "ACSMNAM"}, {"LAST-NAME", "ACSLNAM"}, {"ADDRESS-LINE-1", "ACSADL1"}, {"STATE", "ACSSTTE"},
        {"ADDRESS-LINE-2", "ACSADL2"}, {"ZIPCODE", "ACSZIPC"}, {"CITY", "ACSCITY"}, {"COUNTRY", "ACSCTRY"},
        {"PHONE-NUM-1A", "ACSPH1A"}, {"PHONE-NUM-1B", "ACSPH1B"}, {"PHONE-NUM-1C", "ACSPH1C"},
        {"PHONE-NUM-2A", "ACSPH2A"}, {"PHONE-NUM-2B", "ACSPH2B"}, {"PHONE-NUM-2C", "ACSPH2C"},
        {"PRI-CARDHOLDER", "ACSPFLG"}, {"EFT-ACCOUNT-ID", "ACSEFTC"}};

    /** Screen fields protected by 3310-PROTECT-ALL-ATTRS (lines 3441-3497). */
    static final List<String> ALL_INPUT_FIELDS = List.of("ACCTSID", "ACSTTUS", "ACRDLIM", "ACSHLIM", "ACURBAL",
            "ACRCYCR", "ACRCYDB", "OPNYEAR", "OPNMON", "OPNDAY", "EXPYEAR", "EXPMON", "EXPDAY", "RISYEAR", "RISMON",
            "RISDAY", "AADDGRP", "ACSTNUM", "ACTSSN1", "ACTSSN2", "ACTSSN3", "ACSTFCO", "DOBYEAR", "DOBMON", "DOBDAY",
            "ACSFNAM", "ACSMNAM", "ACSLNAM", "ACSADL1", "ACSADL2", "ACSCITY", "ACSSTTE", "ACSZIPC", "ACSCTRY",
            "ACSPH1A", "ACSPH1B", "ACSPH1C", "ACSPH2A", "ACSPH2B", "ACSPH2C", "ACSGOVT", "ACSEFTC", "ACSPFLG",
            "INFOMSG");

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
        return Optional.of(AcupRecords.WS_COMMAREA);
    }

    @Override
    public void execute(CicsContext ctx) {
        Task task = new Task(ctx);
        try {
            task.main();
        } catch (DisplayNumerics.DataException e) {
            // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) (line 862): program check (ASRA) -> ABEND-ROUTINE
            task.abendRoutine();
        }
    }

    /** One task's WORKING-STORAGE (lines 35-858) and the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // WS-CICS-PROCESSNG-VARS
        int wsRespCd;
        int wsReasCd;
        String wsTranid;
        // WS-GENERIC-EDITS
        String wsEditVariableName;
        String wsEditSignedNumber9v2X;
        char wsFlgSignedNumberEdit;
        String wsEditAlphanumOnly;
        int wsEditAlphanumLength;
        char wsEditAlphaOnlyFlags;
        char wsEditAlphanumOnlyFlags;
        char wsEditMandatoryFlags;
        char wsEditYesNo;
        String wsEditUsPhoneNum;
        char[] wsEditUsPhoneNumFlgs = new char[3];
        String wsEditUsSsnPart1;
        char[] wsEditUsSsnFlgs = new char[3];
        // WS-CALCULATION-VARS + CSUTLDWY
        BigDecimal wsDivBy;
        BigDecimal wsDividend;
        BigDecimal wsRemainder;
        String wsEditDateCcyymmdd;
        char[] wsEditDateFlgs = new char[3];
        String wsSeverity;
        String wsMsgNo;
        // flags
        char wsDatachangedFlag;
        char wsInputFlag;
        char wsPfkFlag;
        char wsEditAcctFlag;
        char wsEditCustFlag;
        char[] wsNonKeyFlags = new char[NON_KEY_FLAGS.size()];
        // WS-XREF-RID, WS-FILE-READ-FLAGS, WS-FILE-ERROR-MESSAGE
        String wsCardRidAcctId;
        String wsCardRidCustId;
        char wsAccountMasterReadFlag;
        char wsCustMasterReadFlag;
        String errorOpname;
        String errorFile;
        String errorResp;
        String errorResp2;
        // ALPHA-VARS-FOR-DATA-EDITING
        String acupNewCreditLimitX;
        String acupNewCashCreditLimitX;
        String acupNewCurrBalX;
        String acupNewCurrCycCreditX;
        String acupNewCurrCycDebitX;
        final Rec acctUpdate = new Rec(AcupRecords.ACCT_UPDATE);
        final Rec custUpdate = new Rec(AcupRecords.CUST_UPDATE);
        String wsInfoMsg;
        String wsReturnMsg;

        // CVCRD01Y CC-WORK-AREA
        String ccardAid;
        String ccardNextProg;
        String ccardNextMapset;
        String ccardNextMap;
        String ccardErrorMsg;
        String ccAcctId;

        // copybooks
        final SymbolicMap cactupa = SymbolicMap.of(MAPSET, MAP);
        final AbendData abendData = new AbendData();
        final AccountRecord accountRecord = new AccountRecord();
        final CardXrefRecord cardXrefRecord = new CardXrefRecord();
        final CustomerRecord customerRecord = new CustomerRecord();
        CardDemoCommarea commarea = new CardDemoCommarea();
        final Rec acup = new Rec(AcupRecords.PROG_COMMAREA);

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        // ------------------------------------------------------------------ helpers

        private boolean returnMsgOff() {
            return PicX.isSpaces(wsReturnMsg);
        }

        private void setReturnMsg(String text) {
            wsReturnMsg = PicX.of(text, 75);
        }

        private void setInfoMsg(String text) {
            wsInfoMsg = PicX.of(text, 40);
        }

        private boolean returnMsgIs(String text) {
            return PicX.equal(wsReturnMsg, text);
        }

        private void inputError() {
            wsInputFlag = '1';
        }

        private boolean isInputError() {
            return wsInputFlag == '1';
        }

        /** STRING FUNCTION TRIM(WS-EDIT-VARIABLE-NAME) suffix DELIMITED BY SIZE INTO WS-RETURN-MSG (if msg off). */
        private void fieldMessage(String suffix) {
            if (returnMsgOff()) {
                setReturnMsg(trim(wsEditVariableName) + suffix);
            }
        }

        private void setVariableName(String name) {
            wsEditVariableName = PicX.of(name, 25);
        }

        static String trim(String s) {
            int b = 0;
            int e = s.length();
            while (b < e && s.charAt(b) == ' ') {
                b++;
            }
            while (e > b && s.charAt(e - 1) == ' ') {
                e--;
            }
            return s.substring(b, e);
        }

        private static boolean eqTrimUpper(String a, String b) {
            return PicX.equal(PicX.upperCase(trim(a)), PicX.upperCase(trim(b)));
        }

        private static boolean isAllDigits(String s) {
            return !s.isEmpty() && s.chars().allMatch(c -> c >= '0' && c <= '9');
        }

        private int nk(String flag) {
            int i = NON_KEY_FLAGS.indexOf(flag);
            if (i < 0) {
                throw new IllegalArgumentException(flag);
            }
            return i;
        }

        char flag(String name) {
            if (name.startsWith("EDIT-US-SSN-PART")) {
                return wsEditUsSsnFlgs[name.charAt(name.length() - 1) - '1'];
            }
            return wsNonKeyFlags[nk(name)];
        }

        private void setFlag(String name, char value) {
            wsNonKeyFlags[nk(name)] = value;
        }

        private void setDateFlags(String first, char[] flags) {
            int i = nk(first);
            System.arraycopy(flags, 0, wsNonKeyFlags, i, 3);
        }

        private boolean notOk(String name) {
            return flag(name) == '0';
        }

        private boolean blank(String name) {
            return flag(name) == 'B';
        }

        private boolean flagValid(String name) {
            return flag(name) == PicX.LOW_VALUE;
        }

        private boolean detailsNotFetched() {
            String a = acup.x("ACUP-CHANGE-ACTION");
            return a.charAt(0) == PicX.LOW_VALUE || a.charAt(0) == ' ';
        }

        private char changeAction() {
            return acup.x("ACUP-CHANGE-ACTION").charAt(0);
        }

        private void setChangeAction(char c) {
            acup.setX("ACUP-CHANGE-ACTION", String.valueOf(c));
        }

        private boolean changesMade() {
            return "ENCLF".indexOf(changeAction()) >= 0;
        }

        private BigDecimal ccAcctIdN() {
            return DisplayNumerics.value(ccAcctId, 0, false);
        }

        // ------------------------------------------------------------------ INITIALIZE

        /** INITIALIZE CC-WORK-AREA: alphanumerics to SPACES. */
        void initializeCcWorkArea() {
            ccardAid = PicX.spaces(5);
            ccardNextProg = PicX.spaces(8);
            ccardNextMapset = PicX.spaces(7);
            ccardNextMap = PicX.spaces(7);
            ccardErrorMsg = PicX.spaces(75);
            ccAcctId = PicX.spaces(11);
        }

        /** INITIALIZE WS-MISC-STORAGE (lines 35-528): X items to SPACES (VALUE clauses ignored), numerics to 0. */
        void initializeMiscStorage() {
            wsRespCd = 0;
            wsReasCd = 0;
            wsTranid = PicX.spaces(4);
            wsEditVariableName = PicX.spaces(25);
            wsEditSignedNumber9v2X = PicX.spaces(15);
            wsFlgSignedNumberEdit = ' ';
            wsEditAlphanumOnly = PicX.spaces(256);
            wsEditAlphanumLength = 0;
            wsEditAlphaOnlyFlags = ' ';
            wsEditAlphanumOnlyFlags = ' ';
            wsEditMandatoryFlags = ' ';
            wsEditYesNo = ' ';
            wsEditUsPhoneNum = PicX.spaces(15);
            Arrays.fill(wsEditUsPhoneNumFlgs, ' ');
            wsEditUsSsnPart1 = PicX.spaces(3);
            Arrays.fill(wsEditUsSsnFlgs, ' ');
            wsDivBy = BigDecimal.ZERO;
            wsDividend = BigDecimal.ZERO;
            wsRemainder = BigDecimal.ZERO;
            wsEditDateCcyymmdd = PicX.spaces(8);
            Arrays.fill(wsEditDateFlgs, ' ');
            wsSeverity = PicX.spaces(4);
            wsMsgNo = PicX.spaces(4);
            wsDatachangedFlag = ' ';
            wsInputFlag = ' ';
            wsPfkFlag = ' ';
            wsEditAcctFlag = ' ';
            wsEditCustFlag = ' ';
            Arrays.fill(wsNonKeyFlags, ' ');
            wsCardRidAcctId = "00000000000";
            wsCardRidCustId = "000000000";
            wsAccountMasterReadFlag = ' ';
            wsCustMasterReadFlag = ' ';
            errorOpname = PicX.spaces(8);
            errorFile = PicX.spaces(9);
            errorResp = PicX.spaces(10);
            errorResp2 = PicX.spaces(10);
            acupNewCreditLimitX = PicX.spaces(15);
            acupNewCashCreditLimitX = PicX.spaces(15);
            acupNewCurrBalX = PicX.spaces(15);
            acupNewCurrCycCreditX = PicX.spaces(15);
            acupNewCurrCycDebitX = PicX.spaces(15);
            acctUpdate.initializeItems();
            custUpdate.initializeItems();
            wsInfoMsg = PicX.spaces(40);
            wsReturnMsg = PicX.spaces(75);
        }

        /** INITIALIZE CARDDEMO-COMMAREA: X items SPACES, numeric items ZERO. */
        void initializeCommarea() {
            commarea = new CardDemoCommarea();
            for (var f : CardDemoCommarea.LAYOUT.fields()) {
                if (f instanceof com.carddemo.runtime.cobol.NumericField n) {
                    n.set(commarea, BigDecimal.ZERO);
                } else {
                    f.fill(commarea, ' ');
                }
            }
        }

        // ------------------------------------------------------------------ 0000-MAIN (lines 859-1022)

        void main() {
            initializeCcWorkArea();
            initializeMiscStorage();
            wsTranid = TRANSID;
            wsReturnMsg = PicX.spaces(75);

            // The test reads the (fresh) WORKING-STORAGE commarea before DFHCOMMAREA is copied (lines 878-880).
            if (ctx.eib().eibcalen() == 0
                    || (PicX.equal(commarea.fromProgram(), MENU_PROGRAM) && !commarea.isPgmReenter())) {
                initializeCommarea();
                acup.initializeSpaces();
                commarea.setPgmContext(0);
                setChangeAction(PicX.LOW_VALUE);
            } else {
                byte[] in = ctx.commarea();
                // DFHCOMMAREA beyond EIBCALEN is undefined storage: modelled as LOW-VALUES.
                byte[] full = Arrays.copyOf(in, Math.max(in.length, CardDemoCommarea.LENGTH + acup.length()));
                commarea = new CardDemoCommarea();
                commarea.setBytes(Arrays.copyOfRange(full, 0, CardDemoCommarea.LENGTH));
                acup.setBytes(Arrays.copyOfRange(full, CardDemoCommarea.LENGTH,
                        CardDemoCommarea.LENGTH + acup.length()));
            }

            yyyyStorePfkey();

            wsPfkFlag = '1';
            if (ccardAid.equals("ENTER") || ccardAid.equals("PFK03")
                    || (ccardAid.equals("PFK05") && changeAction() == 'N')
                    || (ccardAid.equals("PFK12") && !detailsNotFetched())) {
                wsPfkFlag = '0';
            }
            if (wsPfkFlag == '1') {
                ccardAid = "ENTER";
            }

            if (ccardAid.equals("PFK03")) {
                if (PicX.isSpacesOrLowValues(commarea.fromTranid())) {
                    commarea.setToTranid(MENU_TRANSID);
                } else {
                    commarea.setToTranid(commarea.fromTranid());
                }
                if (PicX.isSpacesOrLowValues(commarea.fromProgram())) {
                    commarea.setToProgram(MENU_PROGRAM);
                } else {
                    commarea.setToProgram(commarea.fromProgram());
                }
                commarea.setFromTranid(TRANSID);
                commarea.setFromProgram(PROGRAM);
                commarea.setUserType("U");
                commarea.setPgmContext(0);
                commarea.setLastMapset(MAPSET);
                commarea.setLastMap(MAP);
                ctx.syncpoint();
                ctx.xctl(commarea.toProgram(), commarea);
            } else if ((detailsNotFetched() && commarea.isPgmEnter())
                    || (PicX.equal(commarea.fromProgram(), MENU_PROGRAM) && !commarea.isPgmReenter())) {
                acup.initializeSpaces();
                sendMap3000();
                commarea.setPgmContext(1);
                setChangeAction(PicX.LOW_VALUE);
                commonReturn();
            } else if (changeAction() == 'C' || changeAction() == 'L' || changeAction() == 'F') {
                acup.initializeSpaces();
                initializeMiscStorage();
                CardDemoCommarea.ACCT_ID.set(commarea, BigDecimal.ZERO);
                commarea.setPgmContext(0);
                sendMap3000();
                commarea.setPgmContext(1);
                setChangeAction(PicX.LOW_VALUE);
                commonReturn();
            } else {
                processInputs1000();
                decideAction2000();
                sendMap3000();
                commonReturn();
            }
        }

        /** COMMON-RETURN (lines 1007-1020). */
        void commonReturn() {
            ccardErrorMsg = wsReturnMsg;
            CobolRecord wsCommarea = new CobolRecord(AcupRecords.WS_COMMAREA_LENGTH);
            wsCommarea.initializeSpaces();
            byte[] out = wsCommarea.toBytes();
            System.arraycopy(commarea.toBytes(), 0, out, 0, CardDemoCommarea.LENGTH);
            System.arraycopy(acup.toBytes(), 0, out, CardDemoCommarea.LENGTH, acup.length());
            ctx.returnTransid(TRANSID, CobolRecord.ofBytes(out));
        }

        /** YYYY-STORE-PFKEY (CSSTRPFY). */
        void yyyyStorePfkey() {
            ccardAid = PfKeys.storePfKey(ctx.eib().eibaid(), ccardAid);
        }

        // ------------------------------------------------------------------ 1000 / 1100

        /** 1000-PROCESS-INPUTS (lines 1025-1037). */
        void processInputs1000() {
            receiveMap1100();
            editMapInputs1200();
            ccardErrorMsg = wsReturnMsg;
            ccardNextProg = PROGRAM;
            ccardNextMapset = MAPSET;
            ccardNextMap = MAP;
        }

        private static boolean starOrSpaces(String v) {
            return PicX.equal(v, "*") || PicX.isSpaces(v);
        }

        /** IF xxxI = '*' OR SPACES MOVE LOW-VALUES ELSE MOVE xxxI TO target. */
        private void receiveField(String mapField, String target) {
            String v = cactupa.data(mapField);
            if (starOrSpaces(v)) {
                acup.fill(target, PicX.LOW_VALUE);
            } else {
                acup.setX(target, v);
            }
        }

        /** Money fields: -X alpha copy, COMPUTE -N = NUMVAL-C(...) when TEST-NUMVAL-C = 0. */
        private String receiveMoney(String mapField, String numericTarget, boolean numvalOfX) {
            String v = cactupa.data(mapField);
            if (starOrSpaces(v)) {
                return PicX.lowValues(15);
            }
            String x = PicX.of(v, 15);
            if (CobolNumval.testNumvalC(x) == 0) {
                acup.setNum(numericTarget, CobolNumval.numvalC(numvalOfX ? x : v));
            }
            return x;
        }

        /** 1100-RECEIVE-MAP (lines 1039-1427). */
        void receiveMap1100() {
            CicsResp resp = ctx.receiveMap(cactupa);
            wsRespCd = resp.code();
            wsReasCd = ctx.eib().eibresp2();

            acup.fill("ACUP-NEW-DETAILS", ' ');

            String acctsid = cactupa.data("ACCTSID");
            if (starOrSpaces(acctsid)) {
                ccAcctId = PicX.lowValues(11);
                acup.fill("ACUP-NEW-ACCT-ID-X", PicX.LOW_VALUE);
            } else {
                ccAcctId = PicX.of(acctsid, 11);
                acup.setX("ACUP-NEW-ACCT-ID-X", acctsid);
            }

            if (detailsNotFetched()) {
                return;
            }

            receiveField("ACSTTUS", "ACUP-NEW-ACTIVE-STATUS");
            acupNewCreditLimitX = receiveMoney("ACRDLIM", "ACUP-NEW-CREDIT-LIMIT-N", false);
            acupNewCashCreditLimitX = receiveMoney("ACSHLIM", "ACUP-NEW-CASH-CREDIT-LIMIT-N", false);
            acupNewCurrBalX = receiveMoney("ACURBAL", "ACUP-NEW-CURR-BAL-N", true);
            acupNewCurrCycCreditX = receiveMoney("ACRCYCR", "ACUP-NEW-CURR-CYC-CREDIT-N", false);
            acupNewCurrCycDebitX = receiveMoney("ACRCYDB", "ACUP-NEW-CURR-CYC-DEBIT-N", false);
            receiveField("OPNYEAR", "ACUP-NEW-OPEN-YEAR");
            receiveField("OPNMON", "ACUP-NEW-OPEN-MON");
            receiveField("OPNDAY", "ACUP-NEW-OPEN-DAY");
            receiveField("EXPYEAR", "ACUP-NEW-EXP-YEAR");
            receiveField("EXPMON", "ACUP-NEW-EXP-MON");
            receiveField("EXPDAY", "ACUP-NEW-EXP-DAY");
            receiveField("RISYEAR", "ACUP-NEW-REISSUE-YEAR");
            receiveField("RISMON", "ACUP-NEW-REISSUE-MON");
            receiveField("RISDAY", "ACUP-NEW-REISSUE-DAY");
            receiveField("AADDGRP", "ACUP-NEW-GROUP-ID");
            receiveField("ACSTNUM", "ACUP-NEW-CUST-ID-X");
            receiveField("ACTSSN1", "ACUP-NEW-CUST-SSN-1");
            receiveField("ACTSSN2", "ACUP-NEW-CUST-SSN-2");
            receiveField("ACTSSN3", "ACUP-NEW-CUST-SSN-3");
            receiveField("DOBYEAR", "ACUP-NEW-CUST-DOB-YEAR");
            receiveField("DOBMON", "ACUP-NEW-CUST-DOB-MON");
            receiveField("DOBDAY", "ACUP-NEW-CUST-DOB-DAY");
            receiveField("ACSTFCO", "ACUP-NEW-CUST-FICO-SCORE-X");
            receiveField("ACSFNAM", "ACUP-NEW-CUST-FIRST-NAME");
            receiveField("ACSMNAM", "ACUP-NEW-CUST-MIDDLE-NAME");
            receiveField("ACSLNAM", "ACUP-NEW-CUST-LAST-NAME");
            receiveField("ACSADL1", "ACUP-NEW-CUST-ADDR-LINE-1");
            receiveField("ACSADL2", "ACUP-NEW-CUST-ADDR-LINE-2");
            receiveField("ACSCITY", "ACUP-NEW-CUST-ADDR-LINE-3");
            receiveField("ACSSTTE", "ACUP-NEW-CUST-ADDR-STATE-CD");
            receiveField("ACSCTRY", "ACUP-NEW-CUST-ADDR-COUNTRY-CD");
            receiveField("ACSZIPC", "ACUP-NEW-CUST-ADDR-ZIP");
            receiveField("ACSPH1A", "ACUP-NEW-CUST-PHONE-NUM-1A");
            receiveField("ACSPH1B", "ACUP-NEW-CUST-PHONE-NUM-1B");
            receiveField("ACSPH1C", "ACUP-NEW-CUST-PHONE-NUM-1C");
            receiveField("ACSPH2A", "ACUP-NEW-CUST-PHONE-NUM-2A");
            receiveField("ACSPH2B", "ACUP-NEW-CUST-PHONE-NUM-2B");
            receiveField("ACSPH2C", "ACUP-NEW-CUST-PHONE-NUM-2C");
            receiveField("ACSGOVT", "ACUP-NEW-CUST-GOVT-ISSUED-ID");
            receiveField("ACSEFTC", "ACUP-NEW-CUST-EFT-ACCOUNT-ID");
            receiveField("ACSPFLG", "ACUP-NEW-CUST-PRI-HOLDER-IND");
        }

        // ------------------------------------------------------------------ 1200 edits

        /** 1200-EDIT-MAP-INPUTS (lines 1429-1679). */
        void editMapInputs1200() {
            wsInputFlag = '0';

            if (detailsNotFetched()) {
                editAccount1210();
                acup.fill("ACUP-OLD-ACCT-DATA", PicX.LOW_VALUE);
                if (wsEditAcctFlag == ' ') {
                    setReturnMsg(NO_SEARCH_CRITERIA_RECEIVED);
                }
                return;
            }

            setInfoMsg(FOUND_ACCOUNT_DATA);
            wsAccountMasterReadFlag = '1';
            wsEditAcctFlag = '1';
            wsCustMasterReadFlag = '1';
            wsEditCustFlag = '1';

            compareOldNew1205();

            if (wsDatachangedFlag == '0' || changeAction() == 'N' || changeAction() == 'C') {
                Arrays.fill(wsNonKeyFlags, PicX.LOW_VALUE);
                return;
            }

            setChangeAction('E');

            setVariableName("Account Status");
            wsEditYesNo = acup.x("ACUP-NEW-ACTIVE-STATUS").charAt(0);
            editYesno1220();
            setFlag("ACCT-STATUS", wsEditYesNo);

            setVariableName("Open Date");
            wsEditDateCcyymmdd = acup.x("ACUP-NEW-OPEN-DATE");
            editDateCcyymmdd();
            setDateFlags("OPEN-YEAR", wsEditDateFlgs);

            setVariableName("Credit Limit");
            wsEditSignedNumber9v2X = acupNewCreditLimitX;
            editSigned9v2_1250();
            setFlag("CRED-LIMIT", wsFlgSignedNumberEdit);

            setVariableName("Expiry Date");
            wsEditDateCcyymmdd = acup.x("ACUP-NEW-EXPIRAION-DATE");
            editDateCcyymmdd();
            setDateFlags("EXPIRY-YEAR", wsEditDateFlgs);

            setVariableName("Cash Credit Limit");
            wsEditSignedNumber9v2X = acupNewCashCreditLimitX;
            editSigned9v2_1250();
            setFlag("CASH-CREDIT-LIMIT", wsFlgSignedNumberEdit);

            setVariableName("Reissue Date");
            wsEditDateCcyymmdd = acup.x("ACUP-NEW-REISSUE-DATE");
            editDateCcyymmdd();
            setDateFlags("REISSUE-YEAR", wsEditDateFlgs);

            setVariableName("Current Balance");
            wsEditSignedNumber9v2X = acupNewCurrBalX;
            editSigned9v2_1250();
            setFlag("CURR-BAL", wsFlgSignedNumberEdit);

            setVariableName("Current Cycle Credit Limit");
            wsEditSignedNumber9v2X = acupNewCurrCycCreditX;
            editSigned9v2_1250();
            setFlag("CURR-CYC-CREDIT", wsFlgSignedNumberEdit);

            setVariableName("Current Cycle Debit Limit");
            wsEditSignedNumber9v2X = acupNewCurrCycDebitX;
            editSigned9v2_1250();
            setFlag("CURR-CYC-DEBIT", wsFlgSignedNumberEdit);

            setVariableName("SSN");
            editUsSsn1265();

            setVariableName("Date of Birth");
            wsEditDateCcyymmdd = acup.x("ACUP-NEW-CUST-DOB-YYYY-MM-DD");
            editDateCcyymmdd();
            setDateFlags("DT-OF-BIRTH-YEAR", wsEditDateFlgs);
            if (flagValid("DT-OF-BIRTH-YEAR") && flagValid("DT-OF-BIRTH-MONTH") && flagValid("DT-OF-BIRTH-DAY")) {
                editDateOfBirth();
                setDateFlags("DT-OF-BIRTH-YEAR", wsEditDateFlgs);
            }

            setVariableName("FICO Score");
            alnumEdit(acup.x("ACUP-NEW-CUST-FICO-SCORE-X"), 3);
            editNumReqd1245();
            setFlag("FICO-SCORE", wsEditAlphanumOnlyFlags);
            if (flagValid("FICO-SCORE")) {
                editFicoScore1275();
            }

            setVariableName("First Name");
            alnumEdit(acup.x("ACUP-NEW-CUST-FIRST-NAME"), 25);
            editAlphaReqd1225();
            setFlag("FIRST-NAME", wsEditAlphaOnlyFlags);

            setVariableName("Middle Name");
            alnumEdit(acup.x("ACUP-NEW-CUST-MIDDLE-NAME"), 25);
            editAlphaOpt1235();
            setFlag("MIDDLE-NAME", wsEditAlphaOnlyFlags);

            setVariableName("Last Name");
            alnumEdit(acup.x("ACUP-NEW-CUST-LAST-NAME"), 25);
            editAlphaReqd1225();
            setFlag("LAST-NAME", wsEditAlphaOnlyFlags);

            setVariableName("Address Line 1");
            alnumEdit(acup.x("ACUP-NEW-CUST-ADDR-LINE-1"), 50);
            editMandatory1215();
            setFlag("ADDRESS-LINE-1", wsEditMandatoryFlags);

            setVariableName("State");
            alnumEdit(acup.x("ACUP-NEW-CUST-ADDR-STATE-CD"), 2);
            editAlphaReqd1225();
            setFlag("STATE", wsEditAlphaOnlyFlags);
            if (wsEditAlphaOnlyFlags == PicX.LOW_VALUE) {
                editUsStateCd1270();
            }

            setVariableName("Zip");
            alnumEdit(acup.x("ACUP-NEW-CUST-ADDR-ZIP"), 5);
            editNumReqd1245();
            setFlag("ZIPCODE", wsEditAlphanumOnlyFlags);

            setVariableName("City");
            alnumEdit(acup.x("ACUP-NEW-CUST-ADDR-LINE-3"), 50);
            editAlphaReqd1225();
            setFlag("CITY", wsEditAlphaOnlyFlags);

            setVariableName("Country");
            alnumEdit(acup.x("ACUP-NEW-CUST-ADDR-COUNTRY-CD"), 3);
            editAlphaReqd1225();
            setFlag("COUNTRY", wsEditAlphaOnlyFlags);

            setVariableName("Phone Number 1");
            wsEditUsPhoneNum = acup.x("ACUP-NEW-CUST-PHONE-NUM-1");
            editUsPhoneNum1260();
            System.arraycopy(wsEditUsPhoneNumFlgs, 0, wsNonKeyFlags, nk("PHONE-NUM-1A"), 3);

            setVariableName("Phone Number 2");
            wsEditUsPhoneNum = acup.x("ACUP-NEW-CUST-PHONE-NUM-2");
            editUsPhoneNum1260();
            System.arraycopy(wsEditUsPhoneNumFlgs, 0, wsNonKeyFlags, nk("PHONE-NUM-2A"), 3);

            setVariableName("EFT Account Id");
            alnumEdit(acup.x("ACUP-NEW-CUST-EFT-ACCOUNT-ID"), 10);
            editNumReqd1245();
            setFlag("EFT-ACCOUNT-ID", wsEditAlphanumOnlyFlags);

            setVariableName("Primary Card Holder");
            wsEditYesNo = acup.x("ACUP-NEW-CUST-PRI-HOLDER-IND").charAt(0);
            editYesno1220();
            setFlag("PRI-CARDHOLDER", wsEditYesNo);

            if (flagValid("STATE") && flagValid("ZIPCODE")) {
                editUsStateZipCd1280();
            }

            if (!isInputError()) {
                setChangeAction('N');
            }
        }

        /** MOVE value TO WS-EDIT-ALPHANUM-ONLY, MOVE n TO WS-EDIT-ALPHANUM-LENGTH. */
        private void alnumEdit(String value, int length) {
            wsEditAlphanumOnly = PicX.of(value, 256);
            wsEditAlphanumLength = length;
        }

        private String alnumPart() {
            return wsEditAlphanumOnly.substring(0, wsEditAlphanumLength);
        }

        /** 1205-COMPARE-OLD-NEW (lines 1681-1778). */
        void compareOldNew1205() {
            wsDatachangedFlag = '0';
            boolean acctSame = PicX.equal(acup.x("ACUP-NEW-ACCT-ID-X"), acup.x("ACUP-OLD-ACCT-ID-X"))
                    && PicX.equal(PicX.upperCase(acup.x("ACUP-NEW-ACTIVE-STATUS")),
                    PicX.upperCase(acup.x("ACUP-OLD-ACTIVE-STATUS")))
                    && same("CURR-BAL") && same("CREDIT-LIMIT") && same("CASH-CREDIT-LIMIT") && same("OPEN-DATE")
                    && same("EXPIRAION-DATE") && same("REISSUE-DATE") && same("CURR-CYC-CREDIT")
                    && same("CURR-CYC-DEBIT") && sameTrimUpper("GROUP-ID");
            if (!acctSame) {
                wsDatachangedFlag = '1';
                return;
            }
            boolean custSame = sameTrimUpper("CUST-ID-X") && sameTrimUpper("CUST-FIRST-NAME")
                    && sameTrimUpper("CUST-MIDDLE-NAME") && sameTrimUpper("CUST-LAST-NAME")
                    && sameTrimUpper("CUST-ADDR-LINE-1") && sameTrimUpper("CUST-ADDR-LINE-2")
                    && sameTrimUpper("CUST-ADDR-LINE-3") && sameTrimUpper("CUST-ADDR-STATE-CD")
                    && sameTrimUpper("CUST-ADDR-COUNTRY-CD") && sameTrimUpper("CUST-ADDR-ZIP")
                    && same("CUST-PHONE-NUM-1A") && same("CUST-PHONE-NUM-1B") && same("CUST-PHONE-NUM-1C")
                    && same("CUST-PHONE-NUM-2A") && same("CUST-PHONE-NUM-2B") && same("CUST-PHONE-NUM-2C")
                    && same("CUST-SSN-X") && sameTrimUpper("CUST-GOVT-ISSUED-ID") && same("CUST-DOB-YYYY-MM-DD")
                    && same("CUST-EFT-ACCOUNT-ID") && sameTrimUpper("CUST-PRI-HOLDER-IND")
                    && same("CUST-FICO-SCORE-X");
            if (custSame) {
                setReturnMsg(NO_CHANGES_DETECTED);
            } else {
                wsDatachangedFlag = '1';
            }
        }

        private boolean same(String suffix) {
            return PicX.equal(acup.x("ACUP-NEW-" + suffix), acup.x("ACUP-OLD-" + suffix));
        }

        private boolean sameTrimUpper(String suffix) {
            return eqTrimUpper(acup.x("ACUP-NEW-" + suffix), acup.x("ACUP-OLD-" + suffix));
        }

        /** 1210-EDIT-ACCOUNT (lines 1783-1821). */
        void editAccount1210() {
            wsEditAcctFlag = '0';
            if (PicX.isSpacesOrLowValues(ccAcctId)) {
                inputError();
                wsEditAcctFlag = ' ';
                if (returnMsgOff()) {
                    setReturnMsg("Account number not provided");
                }
                CardDemoCommarea.ACCT_ID.set(commarea, BigDecimal.ZERO);
                acup.setNum("ACUP-NEW-ACCT-ID", BigDecimal.ZERO);
                return;
            }
            acup.setX("ACUP-NEW-ACCT-ID", ccAcctId);
            if (!isAllDigits(ccAcctId) || ccAcctIdN().signum() == 0) {
                inputError();
                if (returnMsgOff()) {
                    setReturnMsg("Account Number if supplied must be a 11 digit Non-Zero Number");
                }
                CardDemoCommarea.ACCT_ID.set(commarea, BigDecimal.ZERO);
                return;
            }
            CardDemoCommarea.ACCT_ID.set(commarea, new BigDecimal(ccAcctId));
            wsEditAcctFlag = '1';
        }

        private boolean editedBlank() {
            String part = alnumPart();
            return PicX.isLowValues(part) || PicX.isSpaces(part) || trim(part).isEmpty();
        }

        /** INSPECT ... CONVERTING letters (and digits) TO SPACES; then LENGTH(TRIM(..)) = 0. */
        private boolean onlyAllowed(boolean digitsAllowed) {
            for (char c : alnumPart().toCharArray()) {
                boolean letter = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
                boolean digit = c >= '0' && c <= '9';
                if (!(letter || c == ' ' || (digitsAllowed && digit))) {
                    return false;
                }
            }
            return true;
        }

        /** 1215-EDIT-MANDATORY (lines 1824-1853). */
        void editMandatory1215() {
            wsEditMandatoryFlags = '0';
            if (editedBlank()) {
                inputError();
                wsEditMandatoryFlags = 'B';
                fieldMessage(" must be supplied.");
                return;
            }
            wsEditMandatoryFlags = PicX.LOW_VALUE;
        }

        /** 1220-EDIT-YESNO (lines 1856-1895). */
        void editYesno1220() {
            if (wsEditYesNo == PicX.LOW_VALUE || wsEditYesNo == ' ' || wsEditYesNo == '0') {
                inputError();
                wsEditYesNo = 'B';
                fieldMessage(" must be supplied.");
                return;
            }
            if (wsEditYesNo != 'Y' && wsEditYesNo != 'N') {
                inputError();
                wsEditYesNo = '0';
                fieldMessage(" must be Y or N.");
            }
        }

        /** 1225-EDIT-ALPHA-REQD (lines 1898-1952). */
        void editAlphaReqd1225() {
            wsEditAlphaOnlyFlags = '0';
            if (editedBlank()) {
                inputError();
                wsEditAlphaOnlyFlags = 'B';
                fieldMessage(" must be supplied.");
                return;
            }
            if (!onlyAllowed(false)) {
                inputError();
                wsEditAlphaOnlyFlags = '0';
                fieldMessage(" can have alphabets only.");
                return;
            }
            wsEditAlphaOnlyFlags = PicX.LOW_VALUE;
        }

        /** 1230-EDIT-ALPHANUM-REQD (lines 1955-2010); not performed by COACTUPC. */
        void editAlphanumReqd1230() {
            wsEditAlphanumOnlyFlags = '0';
            if (editedBlank()) {
                inputError();
                wsEditAlphanumOnlyFlags = 'B';
                fieldMessage(" must be supplied.");
                return;
            }
            if (!onlyAllowed(true)) {
                inputError();
                wsEditAlphanumOnlyFlags = '0';
                fieldMessage(" can have numbers or alphabets only.");
                return;
            }
            wsEditAlphanumOnlyFlags = PicX.LOW_VALUE;
        }

        /** 1235-EDIT-ALPHA-OPT (lines 2012-2058). */
        void editAlphaOpt1235() {
            wsEditAlphaOnlyFlags = '0';
            if (editedBlank()) {
                wsEditAlphaOnlyFlags = PicX.LOW_VALUE;
                return;
            }
            if (!onlyAllowed(false)) {
                inputError();
                wsEditAlphaOnlyFlags = '0';
                fieldMessage(" can have alphabets only.");
                return;
            }
            wsEditAlphaOnlyFlags = PicX.LOW_VALUE;
        }

        /** 1240-EDIT-ALPHANUM-OPT (lines 2061-2106); not performed by COACTUPC. */
        void editAlphanumOpt1240() {
            wsEditAlphanumOnlyFlags = '0';
            if (editedBlank()) {
                wsEditAlphanumOnlyFlags = PicX.LOW_VALUE;
                return;
            }
            if (!onlyAllowed(true)) {
                inputError();
                wsEditAlphanumOnlyFlags = '0';
                fieldMessage(" can have numbers or alphabets only.");
                return;
            }
            wsEditAlphanumOnlyFlags = PicX.LOW_VALUE;
        }

        /** 1245-EDIT-NUM-REQD (lines 2109-2177). */
        void editNumReqd1245() {
            wsEditAlphanumOnlyFlags = '0';
            if (editedBlank()) {
                inputError();
                wsEditAlphanumOnlyFlags = 'B';
                fieldMessage(" must be supplied.");
                return;
            }
            if (!isAllDigits(alnumPart())) {
                inputError();
                wsEditAlphanumOnlyFlags = '0';
                fieldMessage(" must be all numeric.");
                return;
            }
            if (CobolNumval.numval(alnumPart()).signum() == 0) {
                inputError();
                wsEditAlphanumOnlyFlags = '0';
                fieldMessage(" must not be zero.");
                return;
            }
            wsEditAlphanumOnlyFlags = PicX.LOW_VALUE;
        }

        /** 1250-EDIT-SIGNED-9V2 (lines 2180-2222). */
        void editSigned9v2_1250() {
            wsFlgSignedNumberEdit = '0';
            if (PicX.isSpacesOrLowValues(wsEditSignedNumber9v2X)) {
                inputError();
                wsFlgSignedNumberEdit = 'B';
                fieldMessage(" must be supplied.");
                return;
            }
            if (CobolNumval.testNumvalC(wsEditSignedNumber9v2X) != 0) {
                inputError();
                wsFlgSignedNumberEdit = '0';
                fieldMessage(" is not valid");
                return;
            }
            wsFlgSignedNumberEdit = PicX.LOW_VALUE;
        }

        private String phoneA() {
            return wsEditUsPhoneNum.substring(1, 4);
        }

        private String phoneB() {
            return wsEditUsPhoneNum.substring(5, 8);
        }

        private String phoneC() {
            return wsEditUsPhoneNum.substring(9, 13);
        }

        /**
         * 1260-EDIT-US-PHONE-NUM (lines 2225-2428) including the fall-through paragraphs EDIT-AREA-CODE,
         * EDIT-US-PHONE-PREFIX, EDIT-US-PHONE-LINENUM and EDIT-US-PHONE-EXIT.
         */
        void editUsPhoneNum1260() {
            Arrays.fill(wsEditUsPhoneNumFlgs, '0');
            // Bug kept: the third test checks NUMA = SPACES instead of NUMC = SPACES (line 2212).
            if (PicX.isSpacesOrLowValues(phoneA()) && PicX.isSpacesOrLowValues(phoneB())
                    && (PicX.isSpaces(phoneA()) || PicX.isLowValues(phoneC()))) {
                Arrays.fill(wsEditUsPhoneNumFlgs, PicX.LOW_VALUE);
                return;
            }
            editAreaCode();
            editUsPhonePrefix();
            editUsPhoneLinenum();
        }

        /** EDIT-AREA-CODE (lines 2246-2315). */
        void editAreaCode() {
            if (PicX.isSpacesOrLowValues(phoneA())) {
                inputError();
                wsEditUsPhoneNumFlgs[0] = 'B';
                fieldMessage(": Area code must be supplied.");
                return;
            }
            if (!isAllDigits(phoneA())) {
                inputError();
                wsEditUsPhoneNumFlgs[0] = '0';
                fieldMessage(": Area code must be A 3 digit number.");
                return;
            }
            if (Integer.parseInt(phoneA()) == 0) {
                inputError();
                wsEditUsPhoneNumFlgs[0] = '0';
                fieldMessage(": Area code cannot be zero");
                return;
            }
            if (!LookupCodes.VALID_GENERAL_PURP_CODE.contains(PicX.of(trim(phoneA()), 3))) {
                inputError();
                wsEditUsPhoneNumFlgs[0] = '0';
                fieldMessage(": Not valid North America general purpose area code");
                return;
            }
            wsEditUsPhoneNumFlgs[0] = PicX.LOW_VALUE;
        }

        /** EDIT-US-PHONE-PREFIX (lines 2316-2369). */
        void editUsPhonePrefix() {
            if (PicX.isSpacesOrLowValues(phoneB())) {
                inputError();
                wsEditUsPhoneNumFlgs[1] = 'B';
                fieldMessage(": Prefix code must be supplied.");
                return;
            }
            if (!isAllDigits(phoneB())) {
                inputError();
                wsEditUsPhoneNumFlgs[1] = '0';
                fieldMessage(": Prefix code must be A 3 digit number.");
                return;
            }
            if (Integer.parseInt(phoneB()) == 0) {
                inputError();
                wsEditUsPhoneNumFlgs[1] = '0';
                fieldMessage(": Prefix code cannot be zero");
                return;
            }
            wsEditUsPhoneNumFlgs[1] = PicX.LOW_VALUE;
        }

        /** EDIT-US-PHONE-LINENUM (lines 2370-2423). */
        void editUsPhoneLinenum() {
            if (PicX.isSpacesOrLowValues(phoneC())) {
                inputError();
                wsEditUsPhoneNumFlgs[2] = 'B';
                fieldMessage(": Line number code must be supplied.");
                return;
            }
            if (!isAllDigits(phoneC())) {
                inputError();
                wsEditUsPhoneNumFlgs[2] = '0';
                fieldMessage(": Line number code must be A 4 digit number.");
                return;
            }
            if (Integer.parseInt(phoneC()) == 0) {
                inputError();
                wsEditUsPhoneNumFlgs[2] = '0';
                fieldMessage(": Line number code cannot be zero");
                return;
            }
            wsEditUsPhoneNumFlgs[2] = PicX.LOW_VALUE;
        }

        /** 1265-EDIT-US-SSN (lines 2431-2490). */
        void editUsSsn1265() {
            setVariableName("SSN: First 3 chars");
            alnumEdit(acup.x("ACUP-NEW-CUST-SSN-1"), 3);
            editNumReqd1245();
            wsEditUsSsnFlgs[0] = wsEditAlphanumOnlyFlags;
            if (wsEditUsSsnFlgs[0] == PicX.LOW_VALUE) {
                wsEditUsSsnPart1 = acup.x("ACUP-NEW-CUST-SSN-1");
                int part1 = Integer.parseInt(wsEditUsSsnPart1);
                if (part1 == 0 || part1 == 666 || (part1 >= 900 && part1 <= 999)) {
                    inputError();
                    wsEditUsSsnFlgs[0] = '0';
                    fieldMessage(": should not be 000, 666, or between 900 and 999");
                }
            }
            setVariableName("SSN 4th & 5th chars");
            alnumEdit(acup.x("ACUP-NEW-CUST-SSN-2"), 2);
            editNumReqd1245();
            wsEditUsSsnFlgs[1] = wsEditAlphanumOnlyFlags;

            setVariableName("SSN Last 4 chars");
            alnumEdit(acup.x("ACUP-NEW-CUST-SSN-3"), 4);
            editNumReqd1245();
            wsEditUsSsnFlgs[2] = wsEditAlphanumOnlyFlags;
        }

        /** 1270-EDIT-US-STATE-CD (lines 2493-2512). */
        void editUsStateCd1270() {
            if (!LookupCodes.VALID_US_STATE_CODE.contains(acup.x("ACUP-NEW-CUST-ADDR-STATE-CD"))) {
                inputError();
                setFlag("STATE", '0');
                fieldMessage(": is not a valid state code");
            }
        }

        /** 1275-EDIT-FICO-SCORE (lines 2514-2532). */
        void editFicoScore1275() {
            long fico = acup.num("ACUP-NEW-CUST-FICO-SCORE").longValue();
            if (fico < 300 || fico > 850) {
                inputError();
                setFlag("FICO-SCORE", '0');
                fieldMessage(": should be between 300 and 850");
            }
        }

        /** 1280-EDIT-US-STATE-ZIP-CD (lines 2536-2559). */
        void editUsStateZipCd1280() {
            String combo = acup.x("ACUP-NEW-CUST-ADDR-STATE-CD") + acup.x("ACUP-NEW-CUST-ADDR-ZIP").substring(0, 2);
            if (!LookupCodes.VALID_US_STATE_ZIP_CD2_COMBO.contains(combo)) {
                inputError();
                setFlag("STATE", '0');
                setFlag("ZIPCODE", '0');
                if (returnMsgOff()) {
                    setReturnMsg("Invalid zip code for state");
                }
            }
        }

        // ------------------------------------------------------------------ CSUTLDPY (date edits)

        private String dateCcyy() {
            return wsEditDateCcyymmdd.substring(0, 4);
        }

        private String dateMm() {
            return wsEditDateCcyymmdd.substring(4, 6);
        }

        private String dateDd() {
            return wsEditDateCcyymmdd.substring(6, 8);
        }

        private void setDateMm(BigDecimal v) {
            wsEditDateCcyymmdd = dateCcyy() + String.format("%02d", v.longValue() % 100) + dateDd();
        }

        private void setDateDd(BigDecimal v) {
            wsEditDateCcyymmdd = dateCcyy() + dateMm() + String.format("%02d", v.longValue() % 100);
        }

        private boolean dateFlagsValid() {
            return wsEditDateFlgs[0] == PicX.LOW_VALUE && wsEditDateFlgs[1] == PicX.LOW_VALUE
                    && wsEditDateFlgs[2] == PicX.LOW_VALUE;
        }

        /** EDIT-DATE-CCYYMMDD THRU EDIT-DATE-CCYYMMDD-EXIT (CSUTLDPY): all paragraphs fall through in order. */
        void editDateCcyymmdd() {
            Arrays.fill(wsEditDateFlgs, '0');
            editYearCcyy();
            editMonth();
            editDay();
            if (!editDayMonthYear()) {
                return;
            }
            editDateLe();
            Arrays.fill(wsEditDateFlgs, PicX.LOW_VALUE);
        }

        /** EDIT-YEAR-CCYY (CSUTLDPY). */
        void editYearCcyy() {
            wsEditDateFlgs[0] = '0';
            if (PicX.isSpacesOrLowValues(dateCcyy())) {
                inputError();
                wsEditDateFlgs[0] = 'B';
                fieldMessage(" : Year must be supplied.");
                return;
            }
            if (!isAllDigits(dateCcyy())) {
                inputError();
                wsEditDateFlgs[0] = '0';
                fieldMessage(" must be 4 digit number.");
                return;
            }
            String cc = dateCcyy().substring(0, 2);
            if (!cc.equals("20") && !cc.equals("19")) {
                inputError();
                wsEditDateFlgs[0] = '0';
                fieldMessage(" : Century is not valid.");
                return;
            }
            wsEditDateFlgs[0] = PicX.LOW_VALUE;
        }

        /** EDIT-MONTH (CSUTLDPY). WS-VALID-MONTH tests the PIC 9(2) REDEFINES before TEST-NUMVAL. */
        void editMonth() {
            wsEditDateFlgs[1] = '0';
            if (PicX.isSpacesOrLowValues(dateMm())) {
                inputError();
                wsEditDateFlgs[1] = 'B';
                fieldMessage(" : Month must be supplied.");
                return;
            }
            long mm = DisplayNumerics.longValue(dateMm());
            if (mm < 1 || mm > 12) {
                inputError();
                wsEditDateFlgs[1] = '0';
                fieldMessage(": Month must be a number between 1 and 12.");
                return;
            }
            if (CobolNumval.testNumval(dateMm()) == 0) {
                setDateMm(CobolNumval.numval(dateMm()));
            } else {
                inputError();
                wsEditDateFlgs[1] = '0';
                fieldMessage(": Month must be a number between 1 and 12.");
                return;
            }
            wsEditDateFlgs[1] = PicX.LOW_VALUE;
        }

        /** EDIT-DAY (CSUTLDPY). */
        void editDay() {
            wsEditDateFlgs[2] = PicX.LOW_VALUE;
            if (PicX.isSpacesOrLowValues(dateDd())) {
                inputError();
                wsEditDateFlgs[2] = 'B';
                fieldMessage(" : Day must be supplied.");
                return;
            }
            if (CobolNumval.testNumval(dateDd()) == 0) {
                setDateDd(CobolNumval.numval(dateDd()));
            } else {
                inputError();
                wsEditDateFlgs[2] = '0';
                fieldMessage(":day must be a number between 1 and 31.");
                return;
            }
            long dd = DisplayNumerics.longValue(dateDd());
            if (dd < 1 || dd > 31) {
                inputError();
                wsEditDateFlgs[2] = '0';
                fieldMessage(":day must be a number between 1 and 31.");
                return;
            }
            wsEditDateFlgs[2] = PicX.LOW_VALUE;
        }

        /** EDIT-DAY-MONTH-YEAR (CSUTLDPY); false = GO TO EDIT-DATE-CCYYMMDD-EXIT. */
        boolean editDayMonthYear() {
            long mm = DisplayNumerics.longValue(dateMm());
            long dd = DisplayNumerics.longValue(dateDd());
            boolean month31 = mm == 1 || mm == 3 || mm == 5 || mm == 7 || mm == 8 || mm == 10 || mm == 12;
            if (!month31 && dd == 31) {
                inputError();
                wsEditDateFlgs[2] = '0';
                wsEditDateFlgs[1] = '0';
                fieldMessage(":Cannot have 31 days in this month.");
                return false;
            }
            if (mm == 2 && dd == 30) {
                inputError();
                wsEditDateFlgs[2] = '0';
                wsEditDateFlgs[1] = '0';
                fieldMessage(":Cannot have 30 days in this month.");
                return false;
            }
            if (mm == 2 && dd == 29) {
                long yy = DisplayNumerics.longValue(dateCcyy().substring(2, 4));
                wsDivBy = BigDecimal.valueOf(yy == 0 ? 400 : 4);
                BigDecimal ccyy = DisplayNumerics.value(dateCcyy(), 0, false);
                wsDividend = new BigDecimal(ccyy.toBigInteger().divide(wsDivBy.toBigInteger()));
                wsRemainder = ccyy.subtract(wsDividend.multiply(wsDivBy));
                if (wsRemainder.signum() != 0) {
                    inputError();
                    wsEditDateFlgs[2] = '0';
                    wsEditDateFlgs[1] = '0';
                    wsEditDateFlgs[0] = '0';
                    fieldMessage(":Not a leap year.Cannot have 29 days in this month.");
                    return false;
                }
            }
            return dateFlagsValid();
        }

        /** EDIT-DATE-LE (CSUTLDPY): CALL 'CSUTLDTC' (CEEDAYS with picture YYYYMMDD). */
        void editDateLe() {
            CobolDates.CeedaysResult r = CobolDates.ceedays(wsEditDateCcyymmdd, "YYYYMMDD");
            wsSeverity = String.format("%04d", r.severity());
            wsMsgNo = String.format("%04d", r.msgNo());
            if (r.severity() != 0) {
                inputError();
                wsEditDateFlgs[2] = '0';
                wsEditDateFlgs[1] = '0';
                wsEditDateFlgs[0] = '0';
                fieldMessage(" validation error Sev code: " + wsSeverity + " Message code: " + wsMsgNo);
                return;
            }
            if (!isInputError()) {
                wsEditDateFlgs[2] = PicX.LOW_VALUE;
            }
        }

        /** EDIT-DATE-OF-BIRTH (CSUTLDPY): must be before today. */
        void editDateOfBirth() {
            long today = Long.parseLong(ctx.currentDate().substring(0, 8));
            int dob = CobolDates.integerOfDate(Long.parseLong(wsEditDateCcyymmdd));
            int now = CobolDates.integerOfDate(today);
            if (!(now > dob)) {
                inputError();
                Arrays.fill(wsEditDateFlgs, '0');
                fieldMessage(":cannot be in the future ");
            }
        }

        // ------------------------------------------------------------------ 2000

        /** 2000-DECIDE-ACTION (lines 2562-2644). */
        void decideAction2000() {
            char action = changeAction();
            if (detailsNotFetched() || ccardAid.equals("PFK12")) {
                if (wsEditAcctFlag == '1') {
                    wsReturnMsg = PicX.spaces(75);
                    readAcct9000();
                    if (wsCustMasterReadFlag == '1') {
                        setChangeAction('S');
                    }
                }
            } else if (action == 'S') {
                if (!(isInputError() || returnMsgIs(NO_CHANGES_DETECTED))) {
                    setChangeAction('N');
                }
            } else if (action == 'E') {
                // CONTINUE
            } else if (action == 'N' && ccardAid.equals("PFK05")) {
                writeProcessing9600();
                if (returnMsgIs(COULD_NOT_LOCK_ACCT_FOR_UPDATE)) {
                    setChangeAction('L');
                } else if (returnMsgIs(LOCKED_BUT_UPDATE_FAILED)) {
                    setChangeAction('F');
                } else if (returnMsgIs(DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                    setChangeAction('S');
                } else {
                    setChangeAction('C');
                }
            } else if (action == 'N') {
                // CONTINUE
            } else if (action == 'C') {
                setChangeAction('S');
                if (PicX.isSpacesOrLowValues(commarea.fromTranid())) {
                    CardDemoCommarea.ACCT_ID.set(commarea, BigDecimal.ZERO);
                    CardDemoCommarea.CARD_NUM.set(commarea, BigDecimal.ZERO);
                    CardDemoCommarea.ACCT_STATUS.fill(commarea, PicX.LOW_VALUE);
                }
            } else {
                AbendData.ABEND_CULPRIT.set(abendData, PROGRAM);
                AbendData.ABEND_CODE.set(abendData, "0001");
                AbendData.ABEND_REASON.set(abendData, "");
                AbendData.ABEND_MSG.set(abendData, "UNEXPECTED DATA SCENARIO");
                abendRoutine();
            }
        }

        // ------------------------------------------------------------------ 3000 send map

        /** 3000-SEND-MAP (lines 2649-2665). */
        void sendMap3000() {
            screenInit3100();
            setupScreenVars3200();
            setupInfomsg3250();
            setupScreenAttrs3300();
            setupInfomsgAttrs3390();
            sendScreen3400();
        }

        /** 3100-SCREEN-INIT (lines 2668-2695). */
        void screenInit3100() {
            cactupa.lowValues();
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            cactupa.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            cactupa.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            cactupa.setData("TRNNAME", TRANSID);
            cactupa.setData("PGMNAME", PROGRAM);
            cactupa.setData("CURDATE", dt.curdateMmDdYy());
            cactupa.setData("CURTIME", dt.curtimeHhMmSs());
        }

        /** 3200-SETUP-SCREEN-VARS (lines 2698-2728). */
        void setupScreenVars3200() {
            if (commarea.isPgmEnter()) {
                return;
            }
            if (ccAcctIdN().signum() == 0 && wsEditAcctFlag == '1') {
                cactupa.setData("ACCTSID", PicX.lowValues(11));
            } else {
                cactupa.setData("ACCTSID", ccAcctId);
            }
            if (detailsNotFetched() || ccAcctIdN().signum() == 0) {
                showInitialValues3201();
            } else if (changeAction() == 'S') {
                showOriginalValues3202();
            } else if (changesMade()) {
                showUpdatedValues3203();
            } else {
                showOriginalValues3202();
            }
        }

        /** 3201-SHOW-INITIAL-VALUES (lines 2731-2784). */
        void showInitialValues3201() {
            for (String f : ALL_INPUT_FIELDS) {
                if (!f.equals("ACCTSID") && !f.equals("INFOMSG")) {
                    cactupa.setData(f, PicX.lowValues(cactupa.data(f).length()));
                }
            }
        }

        private String currency(BigDecimal v) {
            return NumericEdited.format(v, CURRENCY_PIC);
        }

        /** 3202-SHOW-ORIGINAL-VALUES (lines 2787-2868). */
        void showOriginalValues3202() {
            Arrays.fill(wsNonKeyFlags, PicX.LOW_VALUE);
            setInfoMsg(PROMPT_FOR_CHANGES);
            if (wsAccountMasterReadFlag == '1' || wsCustMasterReadFlag == '1') {
                cactupa.setData("ACSTTUS", acup.x("ACUP-OLD-ACTIVE-STATUS"));
                cactupa.setData("ACURBAL", currency(acup.num("ACUP-OLD-CURR-BAL-N")));
                cactupa.setData("ACRDLIM", currency(acup.num("ACUP-OLD-CREDIT-LIMIT-N")));
                cactupa.setData("ACSHLIM", currency(acup.num("ACUP-OLD-CASH-CREDIT-LIMIT-N")));
                cactupa.setData("ACRCYCR", currency(acup.num("ACUP-OLD-CURR-CYC-CREDIT-N")));
                cactupa.setData("ACRCYDB", currency(acup.num("ACUP-OLD-CURR-CYC-DEBIT-N")));
                cactupa.setData("OPNYEAR", acup.x("ACUP-OLD-OPEN-YEAR"));
                cactupa.setData("OPNMON", acup.x("ACUP-OLD-OPEN-MON"));
                cactupa.setData("OPNDAY", acup.x("ACUP-OLD-OPEN-DAY"));
                cactupa.setData("EXPYEAR", acup.x("ACUP-OLD-EXP-YEAR"));
                cactupa.setData("EXPMON", acup.x("ACUP-OLD-EXP-MON"));
                cactupa.setData("EXPDAY", acup.x("ACUP-OLD-EXP-DAY"));
                cactupa.setData("RISYEAR", acup.x("ACUP-OLD-REISSUE-YEAR"));
                cactupa.setData("RISMON", acup.x("ACUP-OLD-REISSUE-MON"));
                cactupa.setData("RISDAY", acup.x("ACUP-OLD-REISSUE-DAY"));
                cactupa.setData("AADDGRP", acup.x("ACUP-OLD-GROUP-ID"));
            }
            if (wsCustMasterReadFlag == '1') {
                String ssn = acup.x("ACUP-OLD-CUST-SSN-X");
                String ph1 = acup.x("ACUP-OLD-CUST-PHONE-NUM-1");
                String ph2 = acup.x("ACUP-OLD-CUST-PHONE-NUM-2");
                cactupa.setData("ACSTNUM", acup.x("ACUP-OLD-CUST-ID-X"));
                cactupa.setData("ACTSSN1", ssn.substring(0, 3));
                cactupa.setData("ACTSSN2", ssn.substring(3, 5));
                cactupa.setData("ACTSSN3", ssn.substring(5, 9));
                cactupa.setData("ACSTFCO", acup.x("ACUP-OLD-CUST-FICO-SCORE-X"));
                cactupa.setData("DOBYEAR", acup.x("ACUP-OLD-CUST-DOB-YEAR"));
                cactupa.setData("DOBMON", acup.x("ACUP-OLD-CUST-DOB-MON"));
                cactupa.setData("DOBDAY", acup.x("ACUP-OLD-CUST-DOB-DAY"));
                cactupa.setData("ACSFNAM", acup.x("ACUP-OLD-CUST-FIRST-NAME"));
                cactupa.setData("ACSMNAM", acup.x("ACUP-OLD-CUST-MIDDLE-NAME"));
                cactupa.setData("ACSLNAM", acup.x("ACUP-OLD-CUST-LAST-NAME"));
                cactupa.setData("ACSADL1", acup.x("ACUP-OLD-CUST-ADDR-LINE-1"));
                cactupa.setData("ACSADL2", acup.x("ACUP-OLD-CUST-ADDR-LINE-2"));
                cactupa.setData("ACSCITY", acup.x("ACUP-OLD-CUST-ADDR-LINE-3"));
                cactupa.setData("ACSSTTE", acup.x("ACUP-OLD-CUST-ADDR-STATE-CD"));
                cactupa.setData("ACSZIPC", acup.x("ACUP-OLD-CUST-ADDR-ZIP"));
                cactupa.setData("ACSCTRY", acup.x("ACUP-OLD-CUST-ADDR-COUNTRY-CD"));
                cactupa.setData("ACSPH1A", ph1.substring(1, 4));
                cactupa.setData("ACSPH1B", ph1.substring(5, 8));
                cactupa.setData("ACSPH1C", ph1.substring(9, 13));
                cactupa.setData("ACSPH2A", ph2.substring(1, 4));
                cactupa.setData("ACSPH2B", ph2.substring(5, 8));
                cactupa.setData("ACSPH2C", ph2.substring(9, 13));
                cactupa.setData("ACSGOVT", acup.x("ACUP-OLD-CUST-GOVT-ISSUED-ID"));
                cactupa.setData("ACSEFTC", acup.x("ACUP-OLD-CUST-EFT-ACCOUNT-ID"));
                cactupa.setData("ACSPFLG", acup.x("ACUP-OLD-CUST-PRI-HOLDER-IND"));
            }
        }

        private void showMoney(String mapField, String flagName, String numericField, String alpha) {
            if (flagValid(flagName)) {
                cactupa.setData(mapField, currency(acup.num(numericField)));
            } else {
                cactupa.setData(mapField, alpha);
            }
        }

        /** 3203-SHOW-UPDATED-VALUES (lines 2870-2952). */
        void showUpdatedValues3203() {
            cactupa.setData("ACSTTUS", acup.x("ACUP-NEW-ACTIVE-STATUS"));
            showMoney("ACRDLIM", "CRED-LIMIT", "ACUP-NEW-CREDIT-LIMIT-N", acupNewCreditLimitX);
            showMoney("ACSHLIM", "CASH-CREDIT-LIMIT", "ACUP-NEW-CASH-CREDIT-LIMIT-N", acupNewCashCreditLimitX);
            showMoney("ACURBAL", "CURR-BAL", "ACUP-NEW-CURR-BAL-N", acupNewCurrBalX);
            showMoney("ACRCYCR", "CURR-CYC-CREDIT", "ACUP-NEW-CURR-CYC-CREDIT-N", acupNewCurrCycCreditX);
            showMoney("ACRCYDB", "CURR-CYC-DEBIT", "ACUP-NEW-CURR-CYC-DEBIT-N", acupNewCurrCycDebitX);
            cactupa.setData("OPNYEAR", acup.x("ACUP-NEW-OPEN-YEAR"));
            cactupa.setData("OPNMON", acup.x("ACUP-NEW-OPEN-MON"));
            cactupa.setData("OPNDAY", acup.x("ACUP-NEW-OPEN-DAY"));
            cactupa.setData("EXPYEAR", acup.x("ACUP-NEW-EXP-YEAR"));
            cactupa.setData("EXPMON", acup.x("ACUP-NEW-EXP-MON"));
            cactupa.setData("EXPDAY", acup.x("ACUP-NEW-EXP-DAY"));
            cactupa.setData("RISYEAR", acup.x("ACUP-NEW-REISSUE-YEAR"));
            cactupa.setData("RISMON", acup.x("ACUP-NEW-REISSUE-MON"));
            cactupa.setData("RISDAY", acup.x("ACUP-NEW-REISSUE-DAY"));
            cactupa.setData("AADDGRP", acup.x("ACUP-NEW-GROUP-ID"));
            cactupa.setData("ACSTNUM", acup.x("ACUP-NEW-CUST-ID-X"));
            cactupa.setData("ACTSSN1", acup.x("ACUP-NEW-CUST-SSN-1"));
            cactupa.setData("ACTSSN2", acup.x("ACUP-NEW-CUST-SSN-2"));
            cactupa.setData("ACTSSN3", acup.x("ACUP-NEW-CUST-SSN-3"));
            cactupa.setData("ACSTFCO", acup.x("ACUP-NEW-CUST-FICO-SCORE-X"));
            cactupa.setData("DOBYEAR", acup.x("ACUP-NEW-CUST-DOB-YEAR"));
            cactupa.setData("DOBMON", acup.x("ACUP-NEW-CUST-DOB-MON"));
            cactupa.setData("DOBDAY", acup.x("ACUP-NEW-CUST-DOB-DAY"));
            cactupa.setData("ACSFNAM", acup.x("ACUP-NEW-CUST-FIRST-NAME"));
            cactupa.setData("ACSMNAM", acup.x("ACUP-NEW-CUST-MIDDLE-NAME"));
            cactupa.setData("ACSLNAM", acup.x("ACUP-NEW-CUST-LAST-NAME"));
            cactupa.setData("ACSADL1", acup.x("ACUP-NEW-CUST-ADDR-LINE-1"));
            cactupa.setData("ACSADL2", acup.x("ACUP-NEW-CUST-ADDR-LINE-2"));
            cactupa.setData("ACSCITY", acup.x("ACUP-NEW-CUST-ADDR-LINE-3"));
            cactupa.setData("ACSSTTE", acup.x("ACUP-NEW-CUST-ADDR-STATE-CD"));
            cactupa.setData("ACSZIPC", acup.x("ACUP-NEW-CUST-ADDR-ZIP"));
            cactupa.setData("ACSCTRY", acup.x("ACUP-NEW-CUST-ADDR-COUNTRY-CD"));
            cactupa.setData("ACSPH1A", acup.x("ACUP-NEW-CUST-PHONE-NUM-1A"));
            cactupa.setData("ACSPH1B", acup.x("ACUP-NEW-CUST-PHONE-NUM-1B"));
            cactupa.setData("ACSPH1C", acup.x("ACUP-NEW-CUST-PHONE-NUM-1C"));
            cactupa.setData("ACSPH2A", acup.x("ACUP-NEW-CUST-PHONE-NUM-2A"));
            cactupa.setData("ACSPH2B", acup.x("ACUP-NEW-CUST-PHONE-NUM-2B"));
            cactupa.setData("ACSPH2C", acup.x("ACUP-NEW-CUST-PHONE-NUM-2C"));
            cactupa.setData("ACSGOVT", acup.x("ACUP-NEW-CUST-GOVT-ISSUED-ID"));
            cactupa.setData("ACSEFTC", acup.x("ACUP-NEW-CUST-EFT-ACCOUNT-ID"));
            cactupa.setData("ACSPFLG", acup.x("ACUP-NEW-CUST-PRI-HOLDER-IND"));
        }

        /** 3250-SETUP-INFOMSG (lines 2955-2984). */
        void setupInfomsg3250() {
            char a = changeAction();
            if (commarea.isPgmEnter()) {
                setInfoMsg(PROMPT_FOR_SEARCH_KEYS);
            } else if (detailsNotFetched()) {
                setInfoMsg(PROMPT_FOR_SEARCH_KEYS);
            } else if (a == 'S') {
                setInfoMsg(PROMPT_FOR_CHANGES);
            } else if (a == 'E') {
                setInfoMsg(PROMPT_FOR_CHANGES);
            } else if (a == 'N') {
                setInfoMsg(PROMPT_FOR_CONFIRMATION);
            } else if (a == 'C') {
                setInfoMsg(CONFIRM_UPDATE_SUCCESS);
            } else if (a == 'L' || a == 'F') {
                setInfoMsg(INFORM_FAILURE);
            } else if (PicX.isSpacesOrLowValues(wsInfoMsg)) {
                setInfoMsg(PROMPT_FOR_SEARCH_KEYS);
            }
            cactupa.setData("INFOMSG", wsInfoMsg);
            cactupa.setData("ERRMSG", wsReturnMsg);
        }

        /** 3300-SETUP-SCREEN-ATTRS (lines 2986-3438). */
        void setupScreenAttrs3300() {
            protectAllAttrs3310();
            char a = changeAction();
            if (detailsNotFetched()) {
                cactupa.setAttribute("ACCTSID", DFHBMFSE);
            } else if (a == 'S' || a == 'E') {
                unprotectFewAttrs3320();
            } else if (a == 'N' || a == 'C') {
                // CONTINUE
            } else {
                cactupa.setAttribute("ACCTSID", DFHBMFSE);
            }

            positionCursor();

            if (PicX.equal(commarea.lastMapset(), CCLIST_MAPSET)) {
                cactupa.setColor("ACCTSID", DFHDFCOL);
            }
            if (wsEditAcctFlag == '0') {
                cactupa.setColor("ACCTSID", DFHRED);
            }
            if (wsEditAcctFlag == ' ' && commarea.isPgmReenter()) {
                cactupa.setData("ACCTSID", "*");
                cactupa.setColor("ACCTSID", DFHRED);
            }
            if (detailsNotFetched() || wsEditAcctFlag == ' ' || wsEditAcctFlag == '0') {
                return;
            }
            for (String[] f : CSSETATY) {
                csSetAty(f[0], f[1]);
            }
        }

        /** POSITION CURSOR - EVALUATE in 3300 (lines 3008-3205). */
        private void positionCursor() {
            if (PicX.equal(wsInfoMsg, FOUND_ACCOUNT_DATA) || returnMsgIs(NO_CHANGES_DETECTED)) {
                cactupa.setCursor("ACSTTUS");
                return;
            }
            if (wsEditAcctFlag == '0' || wsEditAcctFlag == ' ') {
                cactupa.setCursor("ACCTSID");
                return;
            }
            for (String[] f : CURSOR_ORDER) {
                boolean hit = f[0].equals("MIDDLE-NAME") ? notOk(f[0]) : (notOk(f[0]) || blank(f[0]));
                if (hit) {
                    cactupa.setCursor(f[1]);
                    return;
                }
            }
            cactupa.setCursor("ACCTSID");
        }

        /** COPY CSSETATY: red + '*' for a field in error, only when re-entered. */
        private void csSetAty(String testVar, String screenVar) {
            if ((notOk(testVar) || blank(testVar)) && commarea.isPgmReenter()) {
                cactupa.setColor(screenVar, DFHRED);
                if (blank(testVar)) {
                    cactupa.setData(screenVar, "*");
                }
            }
        }

        /** 3310-PROTECT-ALL-ATTRS (lines 3441-3497). */
        void protectAllAttrs3310() {
            for (String f : ALL_INPUT_FIELDS) {
                cactupa.setAttribute(f, DFHBMPRF);
            }
        }

        /** 3320-UNPROTECT-FEW-ATTRS (lines 3500-3563). */
        void unprotectFewAttrs3320() {
            for (String f : List.of("ACSTTUS", "ACRDLIM", "ACSHLIM", "ACURBAL", "ACRCYCR", "ACRCYDB", "OPNYEAR",
                    "OPNMON", "OPNDAY", "EXPYEAR", "EXPMON", "EXPDAY", "RISYEAR", "RISMON", "RISDAY", "DOBYEAR",
                    "DOBMON", "DOBDAY", "AADDGRP")) {
                cactupa.setAttribute(f, DFHBMFSE);
            }
            cactupa.setAttribute("ACSTNUM", DFHBMPRF);
            for (String f : List.of("ACTSSN1", "ACTSSN2", "ACTSSN3", "ACSTFCO", "ACSFNAM", "ACSMNAM", "ACSLNAM",
                    "ACSADL1", "ACSADL2", "ACSCITY", "ACSSTTE", "ACSZIPC")) {
                cactupa.setAttribute(f, DFHBMFSE);
            }
            cactupa.setAttribute("ACSCTRY", DFHBMPRF);
            for (String f : List.of("ACSPH1A", "ACSPH1B", "ACSPH1C", "ACSPH2A", "ACSPH2B", "ACSPH2C", "ACSGOVT",
                    "ACSEFTC", "ACSPFLG")) {
                cactupa.setAttribute(f, DFHBMFSE);
            }
            cactupa.setAttribute("INFOMSG", DFHBMPRF);
        }

        /** 3390-SETUP-INFOMSG-ATTRS (lines 3566-3585). */
        void setupInfomsgAttrs3390() {
            if (PicX.isSpacesOrLowValues(wsInfoMsg)) {
                cactupa.setAttribute("INFOMSG", DFHBMDAR);
            } else {
                cactupa.setAttribute("INFOMSG", DFHBMASB);
            }
            if (changesMade() && changeAction() != 'C') {
                cactupa.setAttribute("FKEY12", DFHBMASB);
            }
            if (PicX.equal(wsInfoMsg, PROMPT_FOR_CONFIRMATION)) {
                cactupa.setAttribute("FKEY05", DFHBMASB);
                cactupa.setAttribute("FKEY12", DFHBMASB);
            }
        }

        /** 3400-SEND-SCREEN (lines 3589-3604). */
        void sendScreen3400() {
            ccardNextMapset = MAPSET;
            ccardNextMap = MAP;
            CicsResp r = ctx.sendMap(cactupa, SendMapOption.CURSOR, SendMapOption.ERASE, SendMapOption.FREEKB);
            wsRespCd = r.code();
        }

        // ------------------------------------------------------------------ 9000 reads

        /** 9000-READ-ACCT (lines 3608-3648). */
        void readAcct9000() {
            acup.fill("ACUP-OLD-DETAILS", ' ');
            wsInfoMsg = PicX.spaces(40);
            acup.setX("ACUP-OLD-ACCT-ID", ccAcctId);
            wsCardRidAcctId = ccAcctId;

            getCardxrefByacct9200();
            if (wsEditAcctFlag == '0') {
                return;
            }
            getAcctdataByacct9300();
            if (returnMsgIs(DID_NOT_FIND_ACCT_IN_ACCTDAT)) {
                return;
            }
            wsCardRidCustId = String.format("%09d", CardDemoCommarea.CUST_ID.get(commarea).longValue());
            getCustdataBycust9400();
            if (returnMsgIs(DID_NOT_FIND_CUST_IN_CUSTDAT)) {
                return;
            }
            storeFetchedData9500();
        }

        private static String respX(int v) {
            return PicX.of(String.format("%09d", Math.abs(v)), 10);
        }

        private String fileErrorMessage() {
            return "File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp + ",RESP2 "
                    + errorResp2 + PicX.spaces(5);
        }

        private void fileError(String file) {
            inputError();
            errorOpname = PicX.of("READ", 8);
            errorFile = PicX.of(file, 9);
            errorResp = respX(wsRespCd);
            errorResp2 = respX(wsReasCd);
            setReturnMsg(fileErrorMessage());
        }

        /** 9200-GETCARDXREF-BYACCT (lines 3650-3699). */
        void getCardxrefByacct9200() {
            FileResult r = ctx.readPath(XREF_ACCT_PATH, wsCardRidAcctId);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null) {
                cardXrefRecord.setBytes(r.data());
            }
            if (r.resp() == CicsResp.NORMAL) {
                CardDemoCommarea.CUST_ID.set(commarea, DisplayNumerics.value(
                        CardXrefRecord.XREF_CUST_ID.raw(cardXrefRecord), 0, false));
                CardDemoCommarea.CARD_NUM.set(commarea, DisplayNumerics.value(
                        CardXrefRecord.XREF_CARD_NUM.get(cardXrefRecord), 0, false));
            } else if (r.resp() == CicsResp.NOTFND) {
                inputError();
                wsEditAcctFlag = '0';
                if (returnMsgOff()) {
                    errorResp = respX(wsRespCd);
                    errorResp2 = respX(wsReasCd);
                    setReturnMsg("Account:" + wsCardRidAcctId + " not found in" + " Cross ref file.  Resp:"
                            + errorResp + " Reas:" + errorResp2);
                }
            } else {
                wsEditAcctFlag = '0';
                fileError(XREF_ACCT_PATH);
            }
        }

        /** 9300-GETACCTDATA-BYACCT (lines 3701-3749). */
        void getAcctdataByacct9300() {
            FileResult r = ctx.read(ACCT_FILE, wsCardRidAcctId, AccountRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null && (r.isNormal() || r.resp() == CicsResp.LENGERR)) {
                accountRecord.setBytes(r.data());
            }
            if (r.resp() == CicsResp.NORMAL) {
                wsAccountMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                inputError();
                wsEditAcctFlag = '0';
                if (returnMsgOff()) {
                    errorResp = respX(wsRespCd);
                    errorResp2 = respX(wsReasCd);
                    setReturnMsg("Account:" + wsCardRidAcctId + " not found in" + " Acct Master file.Resp:"
                            + errorResp + " Reas:" + errorResp2);
                }
            } else {
                wsEditAcctFlag = '0';
                fileError(ACCT_FILE);
            }
        }

        /** 9400-GETCUSTDATA-BYCUST (lines 3752-3798). */
        void getCustdataBycust9400() {
            FileResult r = ctx.read(CUST_FILE, wsCardRidCustId, CustomerRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null && (r.isNormal() || r.resp() == CicsResp.LENGERR)) {
                customerRecord.setBytes(r.data());
            }
            if (r.resp() == CicsResp.NORMAL) {
                wsCustMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                inputError();
                wsEditCustFlag = '0';
                errorResp = respX(wsRespCd);
                errorResp2 = respX(wsReasCd);
                if (returnMsgOff()) {
                    setReturnMsg("CustId:" + wsCardRidCustId + " not found" + " in customer master.Resp: "
                            + errorResp + " REAS:" + errorResp2);
                }
            } else {
                wsEditCustFlag = '0';
                fileError(CUST_FILE);
            }
        }

        private BigDecimal accountNum(com.carddemo.runtime.cobol.NumericField f) {
            return DisplayNumerics.value(f.raw(accountRecord), f.scale(), f.signed());
        }

        private BigDecimal customerNum(com.carddemo.runtime.cobol.NumericField f) {
            return DisplayNumerics.value(f.raw(customerRecord), f.scale(), f.signed());
        }

        /** 9500-STORE-FETCHED-DATA (lines 3801-3886). */
        void storeFetchedData9500() {
            CardDemoCommarea.ACCT_ID.set(commarea, accountNum(AccountRecord.ACCT_ID));
            CardDemoCommarea.CUST_ID.set(commarea, customerNum(CustomerRecord.CUST_ID));
            CardDemoCommarea.CUST_FNAME.set(commarea, CustomerRecord.CUST_FIRST_NAME.get(customerRecord));
            CardDemoCommarea.CUST_MNAME.set(commarea, CustomerRecord.CUST_MIDDLE_NAME.get(customerRecord));
            CardDemoCommarea.CUST_LNAME.set(commarea, CustomerRecord.CUST_LAST_NAME.get(customerRecord));
            CardDemoCommarea.ACCT_STATUS.set(commarea, AccountRecord.ACCT_ACTIVE_STATUS.get(accountRecord));
            CardDemoCommarea.CARD_NUM.set(commarea, DisplayNumerics.value(
                    CardXrefRecord.XREF_CARD_NUM.get(cardXrefRecord), 0, false));

            acup.fill("ACUP-OLD-DETAILS", ' ');
            acup.setNum("ACUP-OLD-ACCT-ID", accountNum(AccountRecord.ACCT_ID));
            acup.setX("ACUP-OLD-ACTIVE-STATUS", AccountRecord.ACCT_ACTIVE_STATUS.get(accountRecord));
            acup.setNum("ACUP-OLD-CURR-BAL-N", accountNum(AccountRecord.ACCT_CURR_BAL));
            acup.setNum("ACUP-OLD-CREDIT-LIMIT-N", accountNum(AccountRecord.ACCT_CREDIT_LIMIT));
            acup.setNum("ACUP-OLD-CASH-CREDIT-LIMIT-N", accountNum(AccountRecord.ACCT_CASH_CREDIT_LIMIT));
            acup.setNum("ACUP-OLD-CURR-CYC-CREDIT-N", accountNum(AccountRecord.ACCT_CURR_CYC_CREDIT));
            acup.setNum("ACUP-OLD-CURR-CYC-DEBIT-N", accountNum(AccountRecord.ACCT_CURR_CYC_DEBIT));
            String open = AccountRecord.ACCT_OPEN_DATE.get(accountRecord);
            acup.setX("ACUP-OLD-OPEN-YEAR", open.substring(0, 4));
            acup.setX("ACUP-OLD-OPEN-MON", open.substring(5, 7));
            acup.setX("ACUP-OLD-OPEN-DAY", open.substring(8, 10));
            String exp = AccountRecord.ACCT_EXPIRAION_DATE.get(accountRecord);
            acup.setX("ACUP-OLD-EXP-YEAR", exp.substring(0, 4));
            acup.setX("ACUP-OLD-EXP-MON", exp.substring(5, 7));
            acup.setX("ACUP-OLD-EXP-DAY", exp.substring(8, 10));
            String ris = AccountRecord.ACCT_REISSUE_DATE.get(accountRecord);
            acup.setX("ACUP-OLD-REISSUE-YEAR", ris.substring(0, 4));
            acup.setX("ACUP-OLD-REISSUE-MON", ris.substring(5, 7));
            acup.setX("ACUP-OLD-REISSUE-DAY", ris.substring(8, 10));
            acup.setX("ACUP-OLD-GROUP-ID", AccountRecord.ACCT_GROUP_ID.get(accountRecord));

            acup.setNum("ACUP-OLD-CUST-ID", customerNum(CustomerRecord.CUST_ID));
            acup.setNum("ACUP-OLD-CUST-SSN", customerNum(CustomerRecord.CUST_SSN));
            String dob = CustomerRecord.CUST_DOB_YYYY_MM_DD.get(customerRecord);
            acup.setX("ACUP-OLD-CUST-DOB-YEAR", dob.substring(0, 4));
            acup.setX("ACUP-OLD-CUST-DOB-MON", dob.substring(5, 7));
            acup.setX("ACUP-OLD-CUST-DOB-DAY", dob.substring(8, 10));
            acup.setNum("ACUP-OLD-CUST-FICO-SCORE", customerNum(CustomerRecord.CUST_FICO_CREDIT_SCORE));
            acup.setX("ACUP-OLD-CUST-FIRST-NAME", CustomerRecord.CUST_FIRST_NAME.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-MIDDLE-NAME", CustomerRecord.CUST_MIDDLE_NAME.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-LAST-NAME", CustomerRecord.CUST_LAST_NAME.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-ADDR-LINE-1", CustomerRecord.CUST_ADDR_LINE_1.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-ADDR-LINE-2", CustomerRecord.CUST_ADDR_LINE_2.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-ADDR-LINE-3", CustomerRecord.CUST_ADDR_LINE_3.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-ADDR-STATE-CD", CustomerRecord.CUST_ADDR_STATE_CD.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-ADDR-COUNTRY-CD", CustomerRecord.CUST_ADDR_COUNTRY_CD.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-ADDR-ZIP", CustomerRecord.CUST_ADDR_ZIP.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-PHONE-NUM-1", CustomerRecord.CUST_PHONE_NUM_1.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-PHONE-NUM-2", CustomerRecord.CUST_PHONE_NUM_2.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-GOVT-ISSUED-ID", CustomerRecord.CUST_GOVT_ISSUED_ID.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-EFT-ACCOUNT-ID", CustomerRecord.CUST_EFT_ACCOUNT_ID.get(customerRecord));
            acup.setX("ACUP-OLD-CUST-PRI-HOLDER-IND", CustomerRecord.CUST_PRI_CARD_HOLDER_IND.get(customerRecord));
        }

        // ------------------------------------------------------------------ 9600 / 9700 write

        /** 9600-WRITE-PROCESSING (lines 3888-4106). */
        void writeProcessing9600() {
            wsCardRidAcctId = ccAcctId;
            FileResult r = ctx.readForUpdate(ACCT_FILE, wsCardRidAcctId);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null) {
                accountRecord.setBytes(r.data());
            }
            if (!r.isNormal()) {
                inputError();
                if (returnMsgOff()) {
                    setReturnMsg(COULD_NOT_LOCK_ACCT_FOR_UPDATE);
                }
                return;
            }

            wsCardRidCustId = String.format("%09d", CardDemoCommarea.CUST_ID.get(commarea).longValue());
            r = ctx.readForUpdate(CUST_FILE, wsCardRidCustId);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.data() != null) {
                customerRecord.setBytes(r.data());
            }
            if (!r.isNormal()) {
                inputError();
                if (returnMsgOff()) {
                    setReturnMsg(COULD_NOT_LOCK_CUST_FOR_UPDATE);
                }
                return;
            }

            checkChangeInRec9700();
            if (returnMsgIs(DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                return;
            }

            acctUpdate.initializeItems();
            acctUpdate.setNum("ACCT-UPDATE-ID", acup.num("ACUP-NEW-ACCT-ID"));
            acctUpdate.setX("ACCT-UPDATE-ACTIVE-STATUS", acup.x("ACUP-NEW-ACTIVE-STATUS"));
            acctUpdate.setNum("ACCT-UPDATE-CURR-BAL", acup.num("ACUP-NEW-CURR-BAL-N"));
            acctUpdate.setNum("ACCT-UPDATE-CREDIT-LIMIT", acup.num("ACUP-NEW-CREDIT-LIMIT-N"));
            acctUpdate.setNum("ACCT-UPDATE-CASH-CREDIT-LIMIT", acup.num("ACUP-NEW-CASH-CREDIT-LIMIT-N"));
            acctUpdate.setNum("ACCT-UPDATE-CURR-CYC-CREDIT", acup.num("ACUP-NEW-CURR-CYC-CREDIT-N"));
            acctUpdate.setNum("ACCT-UPDATE-CURR-CYC-DEBIT", acup.num("ACUP-NEW-CURR-CYC-DEBIT-N"));
            acctUpdate.setX("ACCT-UPDATE-OPEN-DATE", acup.x("ACUP-NEW-OPEN-YEAR") + "-" + acup.x("ACUP-NEW-OPEN-MON")
                    + "-" + acup.x("ACUP-NEW-OPEN-DAY"));
            acctUpdate.setX("ACCT-UPDATE-EXPIRAION-DATE", acup.x("ACUP-NEW-EXP-YEAR") + "-"
                    + acup.x("ACUP-NEW-EXP-MON") + "-" + acup.x("ACUP-NEW-EXP-DAY"));
            acctUpdate.setX("ACCT-UPDATE-REISSUE-DATE", AccountRecord.ACCT_REISSUE_DATE.get(accountRecord));
            acctUpdate.setX("ACCT-UPDATE-REISSUE-DATE", acup.x("ACUP-NEW-REISSUE-YEAR") + "-"
                    + acup.x("ACUP-NEW-REISSUE-MON") + "-" + acup.x("ACUP-NEW-REISSUE-DAY"));
            acctUpdate.setX("ACCT-UPDATE-GROUP-ID", acup.x("ACUP-NEW-GROUP-ID"));

            custUpdate.initializeItems();
            custUpdate.setNum("CUST-UPDATE-ID", acup.num("ACUP-NEW-CUST-ID"));
            custUpdate.setX("CUST-UPDATE-FIRST-NAME", acup.x("ACUP-NEW-CUST-FIRST-NAME"));
            custUpdate.setX("CUST-UPDATE-MIDDLE-NAME", acup.x("ACUP-NEW-CUST-MIDDLE-NAME"));
            custUpdate.setX("CUST-UPDATE-LAST-NAME", acup.x("ACUP-NEW-CUST-LAST-NAME"));
            custUpdate.setX("CUST-UPDATE-ADDR-LINE-1", acup.x("ACUP-NEW-CUST-ADDR-LINE-1"));
            custUpdate.setX("CUST-UPDATE-ADDR-LINE-2", acup.x("ACUP-NEW-CUST-ADDR-LINE-2"));
            custUpdate.setX("CUST-UPDATE-ADDR-LINE-3", acup.x("ACUP-NEW-CUST-ADDR-LINE-3"));
            custUpdate.setX("CUST-UPDATE-ADDR-STATE-CD", acup.x("ACUP-NEW-CUST-ADDR-STATE-CD"));
            custUpdate.setX("CUST-UPDATE-ADDR-COUNTRY-CD", acup.x("ACUP-NEW-CUST-ADDR-COUNTRY-CD"));
            custUpdate.setX("CUST-UPDATE-ADDR-ZIP", acup.x("ACUP-NEW-CUST-ADDR-ZIP"));
            custUpdate.setX("CUST-UPDATE-PHONE-NUM-1", "(" + acup.x("ACUP-NEW-CUST-PHONE-NUM-1A") + ")"
                    + acup.x("ACUP-NEW-CUST-PHONE-NUM-1B") + "-" + acup.x("ACUP-NEW-CUST-PHONE-NUM-1C"));
            custUpdate.setX("CUST-UPDATE-PHONE-NUM-2", "(" + acup.x("ACUP-NEW-CUST-PHONE-NUM-2A") + ")"
                    + acup.x("ACUP-NEW-CUST-PHONE-NUM-2B") + "-" + acup.x("ACUP-NEW-CUST-PHONE-NUM-2C"));
            custUpdate.setNum("CUST-UPDATE-SSN", acup.num("ACUP-NEW-CUST-SSN"));
            custUpdate.setX("CUST-UPDATE-GOVT-ISSUED-ID", acup.x("ACUP-NEW-CUST-GOVT-ISSUED-ID"));
            custUpdate.setX("CUST-UPDATE-DOB-YYYY-MM-DD", acup.x("ACUP-NEW-CUST-DOB-YEAR") + "-"
                    + acup.x("ACUP-NEW-CUST-DOB-MON") + "-" + acup.x("ACUP-NEW-CUST-DOB-DAY"));
            custUpdate.setX("CUST-UPDATE-EFT-ACCOUNT-ID", acup.x("ACUP-NEW-CUST-EFT-ACCOUNT-ID"));
            custUpdate.setX("CUST-UPDATE-PRI-CARD-IND", acup.x("ACUP-NEW-CUST-PRI-HOLDER-IND"));
            custUpdate.setNum("CUST-UPDATE-FICO-CREDIT-SCORE", acup.num("ACUP-NEW-CUST-FICO-SCORE"));

            r = ctx.rewrite(ACCT_FILE, acctUpdate.asString());
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (!r.isNormal()) {
                setReturnMsg(LOCKED_BUT_UPDATE_FAILED);
                return;
            }
            r = ctx.rewrite(CUST_FILE, custUpdate.asString());
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (!r.isNormal()) {
                setReturnMsg(LOCKED_BUT_UPDATE_FAILED);
                ctx.syncpointRollback();
            }
        }

        /** 9700-CHECK-CHANGE-IN-REC (lines 4109-4194); GO TO 9600-WRITE-PROCESSING-EXIT on a difference. */
        void checkChangeInRec9700() {
            String open = AccountRecord.ACCT_OPEN_DATE.get(accountRecord);
            String exp = AccountRecord.ACCT_EXPIRAION_DATE.get(accountRecord);
            String ris = AccountRecord.ACCT_REISSUE_DATE.get(accountRecord);
            boolean acctSame = PicX.equal(AccountRecord.ACCT_ACTIVE_STATUS.get(accountRecord),
                    acup.x("ACUP-OLD-ACTIVE-STATUS"))
                    && accountNum(AccountRecord.ACCT_CURR_BAL).compareTo(acup.num("ACUP-OLD-CURR-BAL-N")) == 0
                    && accountNum(AccountRecord.ACCT_CREDIT_LIMIT).compareTo(acup.num("ACUP-OLD-CREDIT-LIMIT-N")) == 0
                    && accountNum(AccountRecord.ACCT_CASH_CREDIT_LIMIT)
                    .compareTo(acup.num("ACUP-OLD-CASH-CREDIT-LIMIT-N")) == 0
                    && accountNum(AccountRecord.ACCT_CURR_CYC_CREDIT)
                    .compareTo(acup.num("ACUP-OLD-CURR-CYC-CREDIT-N")) == 0
                    && accountNum(AccountRecord.ACCT_CURR_CYC_DEBIT)
                    .compareTo(acup.num("ACUP-OLD-CURR-CYC-DEBIT-N")) == 0
                    && PicX.equal(open.substring(0, 4), acup.x("ACUP-OLD-OPEN-YEAR"))
                    && PicX.equal(open.substring(5, 7), acup.x("ACUP-OLD-OPEN-MON"))
                    && PicX.equal(open.substring(8, 10), acup.x("ACUP-OLD-OPEN-DAY"))
                    && PicX.equal(exp.substring(0, 4), acup.x("ACUP-OLD-EXP-YEAR"))
                    && PicX.equal(exp.substring(5, 7), acup.x("ACUP-OLD-EXP-MON"))
                    && PicX.equal(exp.substring(8, 10), acup.x("ACUP-OLD-EXP-DAY"))
                    && PicX.equal(ris.substring(0, 4), acup.x("ACUP-OLD-REISSUE-YEAR"))
                    && PicX.equal(ris.substring(5, 7), acup.x("ACUP-OLD-REISSUE-MON"))
                    && PicX.equal(ris.substring(8, 10), acup.x("ACUP-OLD-REISSUE-DAY"))
                    && PicX.equal(PicX.lowerCase(AccountRecord.ACCT_GROUP_ID.get(accountRecord)),
                    PicX.lowerCase(acup.x("ACUP-OLD-GROUP-ID")));
            if (!acctSame) {
                setReturnMsg(DATA_WAS_CHANGED_BEFORE_UPDATE);
                return;
            }
            String dob = CustomerRecord.CUST_DOB_YYYY_MM_DD.get(customerRecord);
            String oldDob = acup.x("ACUP-OLD-CUST-DOB-YYYY-MM-DD");
            boolean custSame = upperEq(CustomerRecord.CUST_FIRST_NAME, "ACUP-OLD-CUST-FIRST-NAME")
                    && upperEq(CustomerRecord.CUST_MIDDLE_NAME, "ACUP-OLD-CUST-MIDDLE-NAME")
                    && upperEq(CustomerRecord.CUST_LAST_NAME, "ACUP-OLD-CUST-LAST-NAME")
                    && upperEq(CustomerRecord.CUST_ADDR_LINE_1, "ACUP-OLD-CUST-ADDR-LINE-1")
                    && upperEq(CustomerRecord.CUST_ADDR_LINE_2, "ACUP-OLD-CUST-ADDR-LINE-2")
                    && upperEq(CustomerRecord.CUST_ADDR_LINE_3, "ACUP-OLD-CUST-ADDR-LINE-3")
                    && upperEq(CustomerRecord.CUST_ADDR_STATE_CD, "ACUP-OLD-CUST-ADDR-STATE-CD")
                    && upperEq(CustomerRecord.CUST_ADDR_COUNTRY_CD, "ACUP-OLD-CUST-ADDR-COUNTRY-CD")
                    && PicX.equal(CustomerRecord.CUST_ADDR_ZIP.get(customerRecord), acup.x("ACUP-OLD-CUST-ADDR-ZIP"))
                    && PicX.equal(CustomerRecord.CUST_PHONE_NUM_1.get(customerRecord),
                    acup.x("ACUP-OLD-CUST-PHONE-NUM-1"))
                    && PicX.equal(CustomerRecord.CUST_PHONE_NUM_2.get(customerRecord),
                    acup.x("ACUP-OLD-CUST-PHONE-NUM-2"))
                    && customerNum(CustomerRecord.CUST_SSN).compareTo(acup.num("ACUP-OLD-CUST-SSN")) == 0
                    && upperEq(CustomerRecord.CUST_GOVT_ISSUED_ID, "ACUP-OLD-CUST-GOVT-ISSUED-ID")
                    && PicX.equal(dob.substring(0, 4), oldDob.substring(0, 4))
                    && PicX.equal(dob.substring(5, 7), oldDob.substring(4, 6))
                    && PicX.equal(dob.substring(8, 10), oldDob.substring(6, 8))
                    && PicX.equal(CustomerRecord.CUST_EFT_ACCOUNT_ID.get(customerRecord),
                    acup.x("ACUP-OLD-CUST-EFT-ACCOUNT-ID"))
                    && PicX.equal(CustomerRecord.CUST_PRI_CARD_HOLDER_IND.get(customerRecord),
                    acup.x("ACUP-OLD-CUST-PRI-HOLDER-IND"))
                    && customerNum(CustomerRecord.CUST_FICO_CREDIT_SCORE)
                    .compareTo(acup.num("ACUP-OLD-CUST-FICO-SCORE")) == 0;
            if (!custSame) {
                setReturnMsg(DATA_WAS_CHANGED_BEFORE_UPDATE);
            }
        }

        private boolean upperEq(com.carddemo.runtime.cobol.AlphanumericField f, String oldName) {
            return PicX.equal(PicX.upperCase(f.get(customerRecord)), PicX.upperCase(acup.x(oldName)));
        }

        // ------------------------------------------------------------------ ABEND-ROUTINE

        /** ABEND-ROUTINE (lines 4203-4227). */
        void abendRoutine() {
            if (PicX.isLowValues(AbendData.ABEND_MSG.get(abendData))) {
                AbendData.ABEND_MSG.set(abendData, "UNEXPECTED ABEND OCCURRED.");
            }
            AbendData.ABEND_CULPRIT.set(abendData, PROGRAM);
            ctx.sendText(abendData.asString(), AbendData.LENGTH, true, false);
            ctx.abend("9999");
        }
    }
}

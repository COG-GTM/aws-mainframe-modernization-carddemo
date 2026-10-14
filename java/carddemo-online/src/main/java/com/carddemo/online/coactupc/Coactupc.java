package com.carddemo.online.coactupc;

import com.carddemo.common.copybook.AbendData;
import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.common.copybook.DateTimeFields;
import com.carddemo.common.copybook.ScreenTitles;
import com.carddemo.runtime.bms.Dfhbmsca;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsContext.SendMapOption;
import com.carddemo.runtime.cics.CicsControlTransfer;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cics.PfKeys;
import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolNumval;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.IbmDisplayNumeric;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import com.carddemo.runtime.cobol.ZonedDecimal;
import com.carddemo.runtime.vsam.FileResult;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Optional;
import org.springframework.stereotype.Component;

/**
 * COACTUPC - account update (transaction CAUP, mapset COACTUP / map CACTUPA). Each method is the COBOL paragraph of
 * the same name; line numbers refer to app/cbl/COACTUPC.cbl (and app/cpy/CSUTLDPY.cpy for the date paragraphs).
 */
@Component
public class Coactupc implements CicsProgram {

    public static final String PROGRAM = "COACTUPC";
    public static final String TRANSID = "CAUP";
    public static final String MAPSET = "COACTUP";
    public static final String MAP = "CACTUPA";
    static final String LIT_MENUPGM = "COMEN01C";
    static final String LIT_MENUTRANID = "CM00";
    static final String LIT_CCLISTMAPSET = "COCRDLI";
    /** LENGTH OF WS-COMMAREA (line 850). */
    static final int WS_COMMAREA_LENGTH = 2000;

    // WS-INFO-MSG 88-levels (lines 463-477)
    static final String FOUND_ACCOUNT_DATA = "Details of selected account shown above";
    static final String PROMPT_FOR_SEARCH_KEYS = "Enter or update id of account to update";
    static final String PROMPT_FOR_CHANGES = "Update account details presented above.";
    static final String PROMPT_FOR_CONFIRMATION = "Changes validated.Press F5 to save";
    static final String CONFIRM_UPDATE_SUCCESS = "Changes committed to database";
    static final String INFORM_FAILURE = "Changes unsuccessful. Please try again";
    // WS-RETURN-MSG 88-levels (lines 479-528)
    static final String WS_PROMPT_FOR_ACCT = "Account number not provided";
    static final String NO_SEARCH_CRITERIA_RECEIVED = "No input received";
    static final String NO_CHANGES_DETECTED = "No change detected with respect to values fetched.";
    static final String DID_NOT_FIND_ACCT_IN_ACCTDAT = "Did not find this account in account master file";
    static final String DID_NOT_FIND_CUST_IN_CUSTDAT = "Did not find associated customer in master file";
    static final String COULD_NOT_LOCK_ACCT_FOR_UPDATE = "Could not lock account record for update";
    static final String COULD_NOT_LOCK_CUST_FOR_UPDATE = "Could not lock customer record for update";
    static final String DATA_WAS_CHANGED_BEFORE_UPDATE = "Record changed by some one else. Please review";
    static final String LOCKED_BUT_UPDATE_FAILED = "Update of record failed";

    /** Flag byte values of the WS edit flags (88 ...-ISVALID / -NOT-OK / -BLANK). */
    static final char LOW = '\u0000';
    static final char NOT_OK = '0';
    static final char BLANK = 'B';

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
        new Task(ctx).main0000();
    }

    /**
     * WS-NON-KEY-FLAGS (lines 201-338) in storage order, plus the SSN part flags (WS-EDIT-US-SSN-FLGS, lines
     * 132-144). {@code mapField} is the screen field the flag drives in 3300-SETUP-SCREEN-ATTRS.
     */
    enum Flag {
        ACCT_STATUS("ACSTTUS"), CRED_LIMIT("ACRDLIM"), CASH_CREDIT_LIMIT("ACSHLIM"), CURR_BAL("ACURBAL"),
        CURR_CYC_CREDIT("ACRCYCR"), CURR_CYC_DEBIT("ACRCYDB"),
        DT_OF_BIRTH_YEAR("DOBYEAR"), DT_OF_BIRTH_MONTH("DOBMON"), DT_OF_BIRTH_DAY("DOBDAY"),
        FICO_SCORE("ACSTFCO"),
        OPEN_YEAR("OPNYEAR"), OPEN_MONTH("OPNMON"), OPEN_DAY("OPNDAY"),
        EXPIRY_YEAR("EXPYEAR"), EXPIRY_MONTH("EXPMON"), EXPIRY_DAY("EXPDAY"),
        REISSUE_YEAR("RISYEAR"), REISSUE_MONTH("RISMON"), REISSUE_DAY("RISDAY"),
        FIRST_NAME("ACSFNAM"), MIDDLE_NAME("ACSMNAM"), LAST_NAME("ACSLNAM"),
        ADDRESS_LINE_1("ACSADL1"), ADDRESS_LINE_2("ACSADL2"), CITY("ACSCITY"), STATE("ACSSTTE"), ZIPCODE("ACSZIPC"),
        COUNTRY("ACSCTRY"),
        PHONE_NUM_1A("ACSPH1A"), PHONE_NUM_1B("ACSPH1B"), PHONE_NUM_1C("ACSPH1C"),
        PHONE_NUM_2A("ACSPH2A"), PHONE_NUM_2B("ACSPH2B"), PHONE_NUM_2C("ACSPH2C"),
        EFT_ACCOUNT_ID("ACSEFTC"), PRI_CARDHOLDER("ACSPFLG"),
        EDIT_US_SSN_PART1("ACTSSN1"), EDIT_US_SSN_PART2("ACTSSN2"), EDIT_US_SSN_PART3("ACTSSN3");

        final String mapField;

        Flag(String mapField) {
            this.mapField = mapField;
        }

        static final int NON_KEY_COUNT = 36;
    }

    /** 3300 cursor EVALUATE order (lines 3009-3167); MIDDLE-NAME only tests NOT-OK, ADDRESS-LINE-2 is absent. */
    static final Flag[] CURSOR_ORDER = {
        Flag.ACCT_STATUS, Flag.OPEN_YEAR, Flag.OPEN_MONTH, Flag.OPEN_DAY, Flag.CRED_LIMIT, Flag.EXPIRY_YEAR,
        Flag.EXPIRY_MONTH, Flag.EXPIRY_DAY, Flag.CASH_CREDIT_LIMIT, Flag.REISSUE_YEAR, Flag.REISSUE_MONTH,
        Flag.REISSUE_DAY, Flag.CURR_BAL, Flag.CURR_CYC_CREDIT, Flag.CURR_CYC_DEBIT, Flag.EDIT_US_SSN_PART1,
        Flag.EDIT_US_SSN_PART2, Flag.EDIT_US_SSN_PART3, Flag.DT_OF_BIRTH_YEAR, Flag.DT_OF_BIRTH_MONTH,
        Flag.DT_OF_BIRTH_DAY, Flag.FICO_SCORE, Flag.FIRST_NAME, Flag.MIDDLE_NAME, Flag.LAST_NAME,
        Flag.ADDRESS_LINE_1, Flag.STATE, Flag.ZIPCODE, Flag.CITY, Flag.COUNTRY, Flag.PHONE_NUM_1A,
        Flag.PHONE_NUM_1B, Flag.PHONE_NUM_1C, Flag.PHONE_NUM_2A, Flag.PHONE_NUM_2B, Flag.PHONE_NUM_2C,
        Flag.EFT_ACCOUNT_ID, Flag.PRI_CARDHOLDER};

    /** COPY CSSETATY REPLACING ... order (lines 3207-3435). */
    static final Flag[] CSSETATY_ORDER = {
        Flag.ACCT_STATUS, Flag.OPEN_YEAR, Flag.OPEN_MONTH, Flag.OPEN_DAY, Flag.CRED_LIMIT, Flag.EXPIRY_YEAR,
        Flag.EXPIRY_MONTH, Flag.EXPIRY_DAY, Flag.CASH_CREDIT_LIMIT, Flag.REISSUE_YEAR, Flag.REISSUE_MONTH,
        Flag.REISSUE_DAY, Flag.CURR_BAL, Flag.CURR_CYC_CREDIT, Flag.CURR_CYC_DEBIT, Flag.EDIT_US_SSN_PART1,
        Flag.EDIT_US_SSN_PART2, Flag.EDIT_US_SSN_PART3, Flag.DT_OF_BIRTH_YEAR, Flag.DT_OF_BIRTH_MONTH,
        Flag.DT_OF_BIRTH_DAY, Flag.FICO_SCORE, Flag.FIRST_NAME, Flag.MIDDLE_NAME, Flag.LAST_NAME,
        Flag.ADDRESS_LINE_1, Flag.STATE, Flag.ADDRESS_LINE_2, Flag.ZIPCODE, Flag.CITY, Flag.COUNTRY,
        Flag.PHONE_NUM_1A, Flag.PHONE_NUM_1B, Flag.PHONE_NUM_1C, Flag.PHONE_NUM_2A, Flag.PHONE_NUM_2B,
        Flag.PHONE_NUM_2C, Flag.PRI_CARDHOLDER, Flag.EFT_ACCOUNT_ID};

    /** ACCT-UPDATE-RECORD (lines 418-434): like CVACT01Y but without ACCT-ADDR-ZIP, so GROUP-ID sits at offset 102. */
    static final class AcctUpdateRecord extends CobolRecord {
        private static final RecordLayout.Builder B = RecordLayout.builder("ACCT-UPDATE-RECORD");
        static final NumericField ID = B.zoned("ACCT-UPDATE-ID", 11, 0, false);
        static final AlphanumericField ACTIVE_STATUS = B.x("ACCT-UPDATE-ACTIVE-STATUS", 1);
        static final NumericField CURR_BAL = B.zoned("ACCT-UPDATE-CURR-BAL", 10, 2, true);
        static final NumericField CREDIT_LIMIT = B.zoned("ACCT-UPDATE-CREDIT-LIMIT", 10, 2, true);
        static final NumericField CASH_CREDIT_LIMIT = B.zoned("ACCT-UPDATE-CASH-CREDIT-LIMIT", 10, 2, true);
        static final AlphanumericField OPEN_DATE = B.x("ACCT-UPDATE-OPEN-DATE", 10);
        static final AlphanumericField EXPIRAION_DATE = B.x("ACCT-UPDATE-EXPIRAION-DATE", 10);
        static final AlphanumericField REISSUE_DATE = B.x("ACCT-UPDATE-REISSUE-DATE", 10);
        static final NumericField CURR_CYC_CREDIT = B.zoned("ACCT-UPDATE-CURR-CYC-CREDIT", 10, 2, true);
        static final NumericField CURR_CYC_DEBIT = B.zoned("ACCT-UPDATE-CURR-CYC-DEBIT", 10, 2, true);
        static final AlphanumericField GROUP_ID = B.x("ACCT-UPDATE-GROUP-ID", 10);

        static {
            B.filler(188);
        }

        static final RecordLayout LAYOUT = B.build();

        AcctUpdateRecord() {
            super(LAYOUT.length());
        }
    }

    /** Field formatted with PIC +ZZZ,ZZZ,ZZZ.99 (WS-EDIT-CURRENCY-9-2-F, line 383) from a S9(10)V99 DISPLAY item. */
    static String editCurrency(String zonedRaw) {
        BigDecimal v = zonedValue(zonedRaw, 2);
        String digits = String.format("%011d", v.abs().unscaledValue().mod(java.math.BigInteger.TEN.pow(11)));
        String intPart = digits.substring(0, 9);
        String frac = digits.substring(9);
        String pattern = "ZZZ,ZZZ,ZZZ";
        StringBuilder sb = new StringBuilder();
        boolean significant = false;
        int d = 0;
        for (char p : pattern.toCharArray()) {
            if (p == ',') {
                sb.append(significant ? ',' : ' ');
            } else {
                char c = intPart.charAt(d++);
                if (c != '0') {
                    significant = true;
                }
                sb.append(significant ? c : ' ');
            }
        }
        return (v.signum() < 0 ? "-" : "+") + sb + "." + frac;
    }

    /** Value of a signed zoned item; non-numeric content is evaluated as IBM PACK would (low nibbles, sign zone). */
    static BigDecimal zonedValue(String raw, int scale) {
        if (ZonedDecimal.isNumeric(raw, true)) {
            return ZonedDecimal.decode(raw, scale);
        }
        String body = raw.isEmpty() ? raw : raw.substring(0, raw.length() - 1) + "0";
        long magnitude = IbmDisplayNumeric.unsignedValue(body) + (raw.isEmpty() ? 0 : lowNibble(raw.charAt(raw.length() - 1)));
        BigDecimal v = BigDecimal.valueOf(magnitude, scale);
        char last = raw.isEmpty() ? '0' : raw.charAt(raw.length() - 1);
        boolean negative = "}JKLMNOPQRpqrstuvwxy".indexOf(last) >= 0;
        return negative ? v.negate() : v;
    }

    private static int lowNibble(char c) {
        return com.carddemo.runtime.cobol.Ebcdic.encode(String.valueOf(c))[0] & 0x0F;
    }

    /** FUNCTION TRIM: removes leading and trailing spaces only. */
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

    /** Numeric to alphanumeric MOVE of a PIC S9(9) COMP item into X(10) (ERROR-RESP): 9 digits, space padded. */
    static String respText(int value) {
        return PicX.of(String.format("%09d", Math.abs((long) value) % 1_000_000_000L), 10);
    }

    /** One task's WORKING-STORAGE (lines 35-850) and the procedure division. */
    static final class Task {
        private final CicsContext ctx;

        // COPY COCOM01Y, COACTUP, CVACT01Y, CVACT03Y, CVCUS01Y, CSMSG02Y (no VALUE: LOW-VALUES)
        final CardDemoCommarea cdemo = new CardDemoCommarea();
        final AcupCommarea acup = new AcupCommarea();
        final SymbolicMap map = SymbolicMap.of(MAPSET, MAP);
        final AccountRecord accountRecord = new AccountRecord();
        final CardXrefRecord xrefRecord = new CardXrefRecord();
        final CustomerRecord customerRecord = new CustomerRecord();
        final AbendData abendData = new AbendData();
        final AcctUpdateRecord acctUpdate = new AcctUpdateRecord();
        final CustomerRecord custUpdate = new CustomerRecord();

        // CVCRD01Y CC-WORK-AREA
        String ccardAid;
        String ccardNextProg;
        String ccardNextMapset;
        String ccardNextMap;
        String ccardErrorMsg;
        String ccAcctId;

        // WS-MISC-STORAGE
        int wsRespCd;
        int wsReasCd;
        String wsTranid;
        String editVariableName;
        String editSignedNumber9v2X;
        char signedNumberFlag;
        String editAlphanumOnly;
        int editAlphanumLength;
        char alphaFlag;
        char alphanumFlag;
        char mandatoryFlag;
        char editYesNo;
        String editUsPhoneNum;
        final char[] phoneFlags = new char[3];
        String editUsSsnPart1;
        final char[] nonKey = new char[Flag.NON_KEY_COUNT];
        final char[] ssnFlags = new char[3];
        String editDate;
        final char[] dateFlags = new char[3];
        char dataChangedFlag;
        char inputFlag;
        char acctFilterFlag;
        char custFilterFlag;
        char acctMasterReadFlag;
        char custMasterReadFlag;
        String ridAcctId;
        String ridCustId;
        String errorOpname;
        String errorFile;
        String errorResp;
        String errorResp2;
        String newCreditLimitX;
        String newCashCreditLimitX;
        String newCurrBalX;
        String newCurrCycCreditX;
        String newCurrCycDebitX;
        String wsInfoMsg;
        String wsReturnMsg;

        Task(CicsContext ctx) {
            this.ctx = ctx;
        }

        // ----------------------------------------------------------------- helpers

        char flag(Flag f) {
            return f.ordinal() < Flag.NON_KEY_COUNT ? nonKey[f.ordinal()] : ssnFlags[f.ordinal() - Flag.NON_KEY_COUNT];
        }

        void setFlag(Flag f, char v) {
            if (f.ordinal() < Flag.NON_KEY_COUNT) {
                nonKey[f.ordinal()] = v;
            } else {
                ssnFlags[f.ordinal() - Flag.NON_KEY_COUNT] = v;
            }
        }

        void setFlags(Flag first, char[] three) {
            for (int i = 0; i < 3; i++) {
                setFlag(Flag.values()[first.ordinal() + i], three[i]);
            }
        }

        String g(AlphanumericField f) {
            return f.get(acup);
        }

        void s(AlphanumericField f, String v) {
            f.set(acup, v);
        }

        boolean msgOff() {
            return PicX.isSpaces(wsReturnMsg);
        }

        void setReturnMsg(String text) {
            wsReturnMsg = PicX.of(text, 75);
        }

        /** STRING FUNCTION TRIM(WS-EDIT-VARIABLE-NAME) suffix DELIMITED BY SIZE INTO WS-RETURN-MSG (guarded). */
        void editMsg(String suffix) {
            if (msgOff()) {
                setReturnMsg(trim(editVariableName) + suffix);
            }
        }

        String action() {
            return g(AcupCommarea.CHANGE_ACTION);
        }

        boolean detailsNotFetched() {
            String a = action();
            return a.equals(AcupCommarea.DETAILS_NOT_FETCHED) || a.equals(" ");
        }

        boolean changesMade() {
            return "ENCLF".contains(action()) && !action().isEmpty() && action().charAt(0) != LOW;
        }

        boolean changesFailed() {
            return action().equals("L") || action().equals("F");
        }

        boolean inputError() {
            return inputFlag == '1';
        }

        boolean aid(String code) {
            return PicX.equal(ccardAid, code);
        }

        /** Raw copy for a MOVE between DISPLAY items of the same picture. */
        static void copyRaw(CobolRecord to, com.carddemo.runtime.cobol.CobolField toField, CobolRecord from,
                            com.carddemo.runtime.cobol.CobolField fromField) {
            AlphanumericField t = new AlphanumericField(toField.name(), toField.offset(), toField.length());
            t.set(to, fromField.raw(from));
        }

        static void initializeCommarea(CardDemoCommarea c) {
            c.initializeSpaces();
            CardDemoCommarea.PGM_CONTEXT.set(c, 0);
            CardDemoCommarea.CUST_ID.set(c, 0);
            CardDemoCommarea.ACCT_ID.set(c, 0);
            CardDemoCommarea.CARD_NUM.set(c, 0);
        }

        /** INITIALIZE WS-MISC-STORAGE (line 867): alphanumerics to SPACES, numerics to ZERO, FILLER untouched. */
        void initializeMiscStorage() {
            wsRespCd = 0;
            wsReasCd = 0;
            wsTranid = PicX.spaces(4);
            editVariableName = PicX.spaces(25);
            editSignedNumber9v2X = PicX.spaces(15);
            signedNumberFlag = ' ';
            editAlphanumOnly = PicX.spaces(256);
            editAlphanumLength = 0;
            alphaFlag = ' ';
            alphanumFlag = ' ';
            mandatoryFlag = ' ';
            editYesNo = ' ';
            editUsPhoneNum = PicX.spaces(15);
            Arrays.fill(phoneFlags, ' ');
            editUsSsnPart1 = PicX.spaces(3);
            Arrays.fill(nonKey, ' ');
            Arrays.fill(ssnFlags, ' ');
            editDate = PicX.spaces(8);
            Arrays.fill(dateFlags, ' ');
            dataChangedFlag = ' ';
            inputFlag = ' ';
            acctFilterFlag = ' ';
            custFilterFlag = ' ';
            acctMasterReadFlag = ' ';
            custMasterReadFlag = ' ';
            ridAcctId = "00000000000";
            ridCustId = "000000000";
            errorOpname = PicX.spaces(8);
            errorFile = PicX.spaces(9);
            errorResp = PicX.spaces(10);
            errorResp2 = PicX.spaces(10);
            newCreditLimitX = PicX.spaces(15);
            newCashCreditLimitX = PicX.spaces(15);
            newCurrBalX = PicX.spaces(15);
            newCurrCycCreditX = PicX.spaces(15);
            newCurrCycDebitX = PicX.spaces(15);
            initializeAcctUpdateRecord();
            initializeCustUpdateRecord();
            wsInfoMsg = PicX.spaces(40);
            wsReturnMsg = PicX.spaces(75);
        }

        void initializeAcctUpdateRecord() {
            for (var f : AcctUpdateRecord.LAYOUT.fields()) {
                if (f instanceof NumericField n) {
                    n.set(acctUpdate, 0);
                } else {
                    f.fill(acctUpdate, ' ');
                }
            }
        }

        void initializeCustUpdateRecord() {
            for (var f : CustomerRecord.LAYOUT.fields()) {
                if (f instanceof NumericField n) {
                    n.set(custUpdate, 0);
                } else {
                    f.fill(custUpdate, ' ');
                }
            }
        }

        // ----------------------------------------------------------------- 0000-MAIN (lines 859-1023)

        void main0000() {
            try {
                mainBody();
            } catch (CicsControlTransfer t) {
                throw t;
            } catch (RuntimeException e) {
                // EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) (lines 862-864)
                abendRoutine();
            }
        }

        private void mainBody() {
            ccardAid = PicX.spaces(5);
            ccardNextProg = PicX.spaces(8);
            ccardNextMapset = PicX.spaces(7);
            ccardNextMap = PicX.spaces(7);
            ccardErrorMsg = PicX.spaces(75);
            ccAcctId = PicX.spaces(11);
            initializeMiscStorage();
            wsTranid = TRANSID;
            wsReturnMsg = PicX.spaces(75);

            // CDEMO-FROM-PROGRAM is tested before DFHCOMMAREA is copied (still the initial LOW-VALUES).
            if (ctx.eib().eibcalen() == 0
                    || (PicX.equal(cdemo.fromProgram(), LIT_MENUPGM) && !cdemo.isPgmReenter())) {
                initializeCommarea(cdemo);
                acup.initializeSpaces();
                cdemo.setPgmContext(0);
                s(AcupCommarea.CHANGE_ACTION, AcupCommarea.DETAILS_NOT_FETCHED);
            } else {
                byte[] in = ctx.commarea();
                byte[] padded = Arrays.copyOf(in, Math.max(in.length, CardDemoCommarea.LENGTH + AcupCommarea.LENGTH));
                cdemo.setBytes(Arrays.copyOfRange(padded, 0, CardDemoCommarea.LENGTH));
                acup.setBytes(Arrays.copyOfRange(padded, CardDemoCommarea.LENGTH,
                        CardDemoCommarea.LENGTH + AcupCommarea.LENGTH));
            }

            yyyyStorePfkey();

            boolean pfkValid = aid("ENTER") || aid("PFK03")
                    || (aid("PFK05") && action().equals(AcupCommarea.CHANGES_OK_NOT_CONFIRMED))
                    || (aid("PFK12") && !detailsNotFetched());
            if (!pfkValid) {
                ccardAid = "ENTER";
            }

            if (aid("PFK03")) {
                if (PicX.isSpacesOrLowValues(cdemo.fromTranid())) {
                    cdemo.setToTranid(LIT_MENUTRANID);
                } else {
                    cdemo.setToTranid(cdemo.fromTranid());
                }
                if (PicX.isSpacesOrLowValues(cdemo.fromProgram())) {
                    cdemo.setToProgram(LIT_MENUPGM);
                } else {
                    cdemo.setToProgram(cdemo.fromProgram());
                }
                cdemo.setFromTranid(TRANSID);
                cdemo.setFromProgram(PROGRAM);
                cdemo.setUserType("U");
                cdemo.setPgmContext(0);
                cdemo.setLastMapset(MAPSET);
                cdemo.setLastMap(MAP);
                ctx.syncpoint();
                ctx.xctl(cdemo.toProgram(), cdemo);
            } else if ((detailsNotFetched() && cdemo.isPgmEnter())
                    || (PicX.equal(cdemo.fromProgram(), LIT_MENUPGM) && !cdemo.isPgmReenter())) {
                acup.initializeSpaces();
                sendMap3000();
                cdemo.setPgmContext(1);
                s(AcupCommarea.CHANGE_ACTION, AcupCommarea.DETAILS_NOT_FETCHED);
                commonReturn();
            } else if (action().equals(AcupCommarea.CHANGES_OKAYED_AND_DONE) || changesFailed()) {
                acup.initializeSpaces();
                initializeMiscStorage();
                CardDemoCommarea.ACCT_ID.set(cdemo, 0);
                cdemo.setPgmContext(0);
                sendMap3000();
                cdemo.setPgmContext(1);
                s(AcupCommarea.CHANGE_ACTION, AcupCommarea.DETAILS_NOT_FETCHED);
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
            CobolRecord wsCommarea = new CobolRecord(WS_COMMAREA_LENGTH);
            wsCommarea.move(cdemo);
            byte[] all = wsCommarea.toBytes();
            System.arraycopy(acup.toBytes(), 0, all, CardDemoCommarea.LENGTH, AcupCommarea.LENGTH);
            ctx.returnTransid(TRANSID, CobolRecord.ofBytes(all));
        }

        /** YYYY-STORE-PFKEY (CSSTRPFY). */
        void yyyyStorePfkey() {
            ccardAid = PfKeys.storePfKey(ctx.eib().eibaid(), ccardAid);
        }

        // ----------------------------------------------------------------- 1000 / 1100

        /** 1000-PROCESS-INPUTS (lines 1025-1038). */
        void processInputs1000() {
            receiveMap1100();
            editMapInputs1200();
            ccardErrorMsg = wsReturnMsg;
            ccardNextProg = PROGRAM;
            ccardNextMapset = PicX.of(MAPSET, 7);
            ccardNextMap = MAP;
        }

        /** IF xxxI = '*' OR SPACES: LOW-VALUES, else the input, moved to a target of {@code len} bytes. */
        private String in(String field, int len) {
            String v = map.data(field);
            if (PicX.equal(v, "*") || PicX.isSpaces(v)) {
                return PicX.lowValues(len);
            }
            return PicX.of(v, len);
        }

        private String money(String field, NumericField target) {
            String x = in(field, 15);
            if (!PicX.isLowValues(x) && CobolNumval.testNumvalC(x) == 0) {
                target.set(acup, CobolNumval.numvalC(map.data(field)));
            }
            return x;
        }

        /** 1100-RECEIVE-MAP (lines 1039-1428). */
        void receiveMap1100() {
            CicsResp resp = ctx.receiveMap(map);
            wsRespCd = resp.code();
            wsReasCd = ctx.eib().eibresp2();
            AcupCommarea.NEW.group.fill(acup, ' ');
            var n = AcupCommarea.NEW;
            ccAcctId = in("ACCTSID", 11);
            s(n.acctIdX, ccAcctId);
            if (detailsNotFetched()) {
                return;
            }
            s(n.activeStatus, in("ACSTTUS", 1));
            newCreditLimitX = money("ACRDLIM", n.creditLimitN);
            newCashCreditLimitX = money("ACSHLIM", n.cashCreditLimitN);
            newCurrBalX = money("ACURBAL", n.currBalN);
            newCurrCycCreditX = money("ACRCYCR", n.currCycCreditN);
            newCurrCycDebitX = money("ACRCYDB", n.currCycDebitN);
            s(n.openYear, in("OPNYEAR", 4));
            s(n.openMon, in("OPNMON", 2));
            s(n.openDay, in("OPNDAY", 2));
            s(n.expYear, in("EXPYEAR", 4));
            s(n.expMon, in("EXPMON", 2));
            s(n.expDay, in("EXPDAY", 2));
            s(n.reissueYear, in("RISYEAR", 4));
            s(n.reissueMon, in("RISMON", 2));
            s(n.reissueDay, in("RISDAY", 2));
            s(n.groupId, in("AADDGRP", 10));
            s(n.custIdX, in("ACSTNUM", 9));
            s(n.ssn1, in("ACTSSN1", 3));
            s(n.ssn2, in("ACTSSN2", 2));
            s(n.ssn3, in("ACTSSN3", 4));
            s(n.dobYear, in("DOBYEAR", 4));
            s(n.dobMon, in("DOBMON", 2));
            s(n.dobDay, in("DOBDAY", 2));
            s(n.ficoScoreX, in("ACSTFCO", 3));
            s(n.firstName, in("ACSFNAM", 25));
            s(n.middleName, in("ACSMNAM", 25));
            s(n.lastName, in("ACSLNAM", 25));
            s(n.addrLine1, in("ACSADL1", 50));
            s(n.addrLine2, in("ACSADL2", 50));
            s(n.addrLine3, in("ACSCITY", 50));
            s(n.stateCd, in("ACSSTTE", 2));
            s(n.countryCd, in("ACSCTRY", 3));
            s(n.zip, in("ACSZIPC", 10));
            s(n.phone1a, in("ACSPH1A", 3));
            s(n.phone1b, in("ACSPH1B", 3));
            s(n.phone1c, in("ACSPH1C", 4));
            s(n.phone2a, in("ACSPH2A", 3));
            s(n.phone2b, in("ACSPH2B", 3));
            s(n.phone2c, in("ACSPH2C", 4));
            s(n.govtIssuedId, in("ACSGOVT", 20));
            s(n.eftAccountId, in("ACSEFTC", 10));
            s(n.priHolderInd, in("ACSPFLG", 1));
        }

        // ----------------------------------------------------------------- 1200 edits

        /** 1200-EDIT-MAP-INPUTS (lines 1429-1680). */
        void editMapInputs1200() {
            inputFlag = '0';
            var n = AcupCommarea.NEW;
            if (detailsNotFetched()) {
                editAccount1210();
                AcupCommarea.OLD.acctData.fill(acup, LOW);
                if (acctFilterFlag == ' ') {
                    setReturnMsg(NO_SEARCH_CRITERIA_RECEIVED);
                }
                return;
            }
            wsInfoMsg = PicX.of(FOUND_ACCOUNT_DATA, 40);
            acctMasterReadFlag = '1';
            acctFilterFlag = '1';
            custMasterReadFlag = '1';
            custFilterFlag = '1';
            compareOldNew1205();
            if (dataChangedFlag == '0' || action().equals(AcupCommarea.CHANGES_OK_NOT_CONFIRMED)
                    || action().equals(AcupCommarea.CHANGES_OKAYED_AND_DONE)) {
                Arrays.fill(nonKey, LOW);
                return;
            }
            s(AcupCommarea.CHANGE_ACTION, AcupCommarea.CHANGES_NOT_OK);

            editVariableName = PicX.of("Account Status", 25);
            editYesNo = g(n.activeStatus).charAt(0);
            editYesno1220();
            setFlag(Flag.ACCT_STATUS, editYesNo);

            editVariableName = PicX.of("Open Date", 25);
            editDate = g(n.openDate);
            editDateCcyymmdd();
            setFlags(Flag.OPEN_YEAR, dateFlags);

            editVariableName = PicX.of("Credit Limit", 25);
            editSignedNumber9v2X = newCreditLimitX;
            editSigned9v2_1250();
            setFlag(Flag.CRED_LIMIT, signedNumberFlag);

            editVariableName = PicX.of("Expiry Date", 25);
            editDate = g(n.expirationDate);
            editDateCcyymmdd();
            setFlags(Flag.EXPIRY_YEAR, dateFlags);

            editVariableName = PicX.of("Cash Credit Limit", 25);
            editSignedNumber9v2X = newCashCreditLimitX;
            editSigned9v2_1250();
            setFlag(Flag.CASH_CREDIT_LIMIT, signedNumberFlag);

            editVariableName = PicX.of("Reissue Date", 25);
            editDate = g(n.reissueDate);
            editDateCcyymmdd();
            setFlags(Flag.REISSUE_YEAR, dateFlags);

            editVariableName = PicX.of("Current Balance", 25);
            editSignedNumber9v2X = newCurrBalX;
            editSigned9v2_1250();
            setFlag(Flag.CURR_BAL, signedNumberFlag);

            editVariableName = PicX.of("Current Cycle Credit Limit", 25);
            editSignedNumber9v2X = newCurrCycCreditX;
            editSigned9v2_1250();
            setFlag(Flag.CURR_CYC_CREDIT, signedNumberFlag);

            editVariableName = PicX.of("Current Cycle Debit Limit", 25);
            editSignedNumber9v2X = newCurrCycDebitX;
            editSigned9v2_1250();
            setFlag(Flag.CURR_CYC_DEBIT, signedNumberFlag);

            editVariableName = PicX.of("SSN", 25);
            editUsSsn1265();

            editVariableName = PicX.of("Date of Birth", 25);
            editDate = g(n.dob);
            editDateCcyymmdd();
            setFlags(Flag.DT_OF_BIRTH_YEAR, dateFlags);
            if (flag(Flag.DT_OF_BIRTH_YEAR) == LOW && flag(Flag.DT_OF_BIRTH_MONTH) == LOW
                    && flag(Flag.DT_OF_BIRTH_DAY) == LOW) {
                editDateOfBirth();
                setFlags(Flag.DT_OF_BIRTH_YEAR, dateFlags);
            }

            editVariableName = PicX.of("FICO Score", 25);
            setAlphanum(g(n.ficoScoreX), 3);
            editNumReqd1245();
            setFlag(Flag.FICO_SCORE, alphanumFlag);
            if (flag(Flag.FICO_SCORE) == LOW) {
                editFicoScore1275();
            }

            editVariableName = PicX.of("First Name", 25);
            setAlphanum(g(n.firstName), 25);
            editAlphaReqd1225();
            setFlag(Flag.FIRST_NAME, alphaFlag);

            editVariableName = PicX.of("Middle Name", 25);
            setAlphanum(g(n.middleName), 25);
            editAlphaOpt1235();
            setFlag(Flag.MIDDLE_NAME, alphaFlag);

            editVariableName = PicX.of("Last Name", 25);
            setAlphanum(g(n.lastName), 25);
            editAlphaReqd1225();
            setFlag(Flag.LAST_NAME, alphaFlag);

            editVariableName = PicX.of("Address Line 1", 25);
            setAlphanum(g(n.addrLine1), 50);
            editMandatory1215();
            setFlag(Flag.ADDRESS_LINE_1, mandatoryFlag);

            editVariableName = PicX.of("State", 25);
            setAlphanum(g(n.stateCd), 2);
            editAlphaReqd1225();
            setFlag(Flag.STATE, alphaFlag);
            if (alphaFlag == LOW) {
                editUsStateCd1270();
            }

            editVariableName = PicX.of("Zip", 25);
            setAlphanum(g(n.zip), 5);
            editNumReqd1245();
            setFlag(Flag.ZIPCODE, alphanumFlag);

            editVariableName = PicX.of("City", 25);
            setAlphanum(g(n.addrLine3), 50);
            editAlphaReqd1225();
            setFlag(Flag.CITY, alphaFlag);

            editVariableName = PicX.of("Country", 25);
            setAlphanum(g(n.countryCd), 3);
            editAlphaReqd1225();
            setFlag(Flag.COUNTRY, alphaFlag);

            editVariableName = PicX.of("Phone Number 1", 25);
            editUsPhoneNum = g(n.phone1);
            editUsPhoneNum1260();
            setFlags(Flag.PHONE_NUM_1A, phoneFlags);

            editVariableName = PicX.of("Phone Number 2", 25);
            editUsPhoneNum = g(n.phone2);
            editUsPhoneNum1260();
            setFlags(Flag.PHONE_NUM_2A, phoneFlags);

            editVariableName = PicX.of("EFT Account Id", 25);
            setAlphanum(g(n.eftAccountId), 10);
            editNumReqd1245();
            setFlag(Flag.EFT_ACCOUNT_ID, alphanumFlag);

            editVariableName = PicX.of("Primary Card Holder", 25);
            editYesNo = g(n.priHolderInd).charAt(0);
            editYesno1220();
            setFlag(Flag.PRI_CARDHOLDER, editYesNo);

            if (flag(Flag.STATE) == LOW && flag(Flag.ZIPCODE) == LOW) {
                editUsStateZipCd1280();
            }
            if (!inputError()) {
                s(AcupCommarea.CHANGE_ACTION, AcupCommarea.CHANGES_OK_NOT_CONFIRMED);
            }
        }

        private static boolean eqU(String a, String b) {
            return PicX.equal(PicX.upperCase(a), PicX.upperCase(b));
        }

        private static boolean eqUT(String a, String b) {
            return PicX.equal(PicX.upperCase(trim(a)), PicX.upperCase(trim(b)));
        }

        /** 1205-COMPARE-OLD-NEW (lines 1681-1779). */
        void compareOldNew1205() {
            var n = AcupCommarea.NEW;
            var o = AcupCommarea.OLD;
            dataChangedFlag = '0';
            boolean acctSame = PicX.equal(g(n.acctIdX), g(o.acctIdX))
                    && eqU(g(n.activeStatus), g(o.activeStatus))
                    && PicX.equal(g(n.currBal), g(o.currBal))
                    && PicX.equal(g(n.creditLimit), g(o.creditLimit))
                    && PicX.equal(g(n.cashCreditLimit), g(o.cashCreditLimit))
                    && PicX.equal(g(n.openDate), g(o.openDate))
                    && PicX.equal(g(n.expirationDate), g(o.expirationDate))
                    && PicX.equal(g(n.reissueDate), g(o.reissueDate))
                    && PicX.equal(g(n.currCycCredit), g(o.currCycCredit))
                    && PicX.equal(g(n.currCycDebit), g(o.currCycDebit))
                    && eqUT(g(n.groupId), g(o.groupId));
            if (!acctSame) {
                dataChangedFlag = '1';
                return;
            }
            boolean custSame = eqUT(g(n.custIdX), g(o.custIdX))
                    && eqUT(g(n.firstName), g(o.firstName))
                    && eqUT(g(n.middleName), g(o.middleName))
                    && eqUT(g(n.lastName), g(o.lastName))
                    && eqUT(g(n.addrLine1), g(o.addrLine1))
                    && eqUT(g(n.addrLine2), g(o.addrLine2))
                    && eqUT(g(n.addrLine3), g(o.addrLine3))
                    && eqUT(g(n.stateCd), g(o.stateCd))
                    && eqUT(g(n.countryCd), g(o.countryCd))
                    && eqUT(g(n.zip), g(o.zip))
                    && PicX.equal(g(n.phone1a), g(o.phone1a))
                    && PicX.equal(g(n.phone1b), g(o.phone1b))
                    && PicX.equal(g(n.phone1c), g(o.phone1c))
                    && PicX.equal(g(n.phone2a), g(o.phone2a))
                    && PicX.equal(g(n.phone2b), g(o.phone2b))
                    && PicX.equal(g(n.phone2c), g(o.phone2c))
                    && PicX.equal(g(n.ssnX), g(o.ssnX))
                    && eqUT(g(n.govtIssuedId), g(o.govtIssuedId))
                    && PicX.equal(g(n.dob), g(o.dob))
                    && PicX.equal(g(n.eftAccountId), g(o.eftAccountId))
                    && eqUT(g(n.priHolderInd), g(o.priHolderInd))
                    && PicX.equal(g(n.ficoScoreX), g(o.ficoScoreX));
            if (custSame) {
                setReturnMsg(NO_CHANGES_DETECTED);
            } else {
                dataChangedFlag = '1';
            }
        }

        /** 1210-EDIT-ACCOUNT (lines 1783-1822). */
        void editAccount1210() {
            acctFilterFlag = '0';
            if (PicX.isSpacesOrLowValues(ccAcctId)) {
                inputFlag = '1';
                acctFilterFlag = ' ';
                if (msgOff()) {
                    setReturnMsg(WS_PROMPT_FOR_ACCT);
                }
                CardDemoCommarea.ACCT_ID.set(cdemo, 0);
                AcupCommarea.NEW.acctId.set(acup, 0);
                return;
            }
            s(AcupCommarea.NEW.acctIdX, IbmDisplayNumeric.moveAlphanumericToUnsigned(ccAcctId));
            if (!CobolNumval.isNumeric(ccAcctId) || IbmDisplayNumeric.unsignedValue(ccAcctId) == 0) {
                inputFlag = '1';
                if (msgOff()) {
                    setReturnMsg("Account Number if supplied must be a 11 digit Non-Zero Number");
                }
                CardDemoCommarea.ACCT_ID.set(cdemo, 0);
                return;
            }
            new AlphanumericField("CDEMO-ACCT-ID", CardDemoCommarea.ACCT_ID.offset(), 11).set(cdemo, ccAcctId);
            acctFilterFlag = '1';
        }

        /** MOVE value TO WS-EDIT-ALPHANUM-ONLY; MOVE len TO WS-EDIT-ALPHANUM-LENGTH. */
        void setAlphanum(String value, int len) {
            editAlphanumOnly = PicX.of(value, 256);
            editAlphanumLength = len;
        }

        private String alnum() {
            return editAlphanumOnly.substring(0, editAlphanumLength);
        }

        private boolean alnumBlank() {
            String v = alnum();
            return PicX.isLowValues(v) || PicX.isSpaces(v) || trim(v).isEmpty();
        }

        /** INSPECT ... CONVERTING letters (and digits) TO SPACES, then LENGTH(TRIM(..)) = 0. */
        private boolean onlyChars(boolean digitsToo) {
            for (char c : alnum().toCharArray()) {
                boolean letter = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
                boolean digit = c >= '0' && c <= '9';
                if (!(letter || c == ' ' || (digitsToo && digit))) {
                    return false;
                }
            }
            return true;
        }

        /** 1215-EDIT-MANDATORY (lines 1824-1854). */
        void editMandatory1215() {
            mandatoryFlag = NOT_OK;
            if (alnumBlank()) {
                inputFlag = '1';
                mandatoryFlag = BLANK;
                editMsg(" must be supplied.");
                return;
            }
            mandatoryFlag = LOW;
        }

        /** 1220-EDIT-YESNO (lines 1856-1896). */
        void editYesno1220() {
            if (editYesNo == LOW || editYesNo == ' ' || editYesNo == '0') {
                inputFlag = '1';
                editYesNo = BLANK;
                editMsg(" must be supplied.");
                return;
            }
            if (editYesNo != 'Y' && editYesNo != 'N') {
                inputFlag = '1';
                editYesNo = NOT_OK;
                editMsg(" must be Y or N.");
            }
        }

        /** 1225-EDIT-ALPHA-REQD (lines 1898-1953). */
        void editAlphaReqd1225() {
            alphaFlag = NOT_OK;
            if (alnumBlank()) {
                inputFlag = '1';
                alphaFlag = BLANK;
                editMsg(" must be supplied.");
                return;
            }
            if (!onlyChars(false)) {
                inputFlag = '1';
                alphaFlag = NOT_OK;
                editMsg(" can have alphabets only.");
                return;
            }
            alphaFlag = LOW;
        }

        /** 1230-EDIT-ALPHANUM-REQD (lines 1955-2011; not PERFORMed by COACTUPC). */
        void editAlphanumReqd1230() {
            alphanumFlag = NOT_OK;
            if (alnumBlank()) {
                inputFlag = '1';
                alphanumFlag = BLANK;
                editMsg(" must be supplied.");
                return;
            }
            if (!onlyChars(true)) {
                inputFlag = '1';
                alphanumFlag = NOT_OK;
                editMsg(" can have numbers or alphabets only.");
                return;
            }
            alphanumFlag = LOW;
        }

        /** 1235-EDIT-ALPHA-OPT (lines 2012-2059). */
        void editAlphaOpt1235() {
            alphaFlag = NOT_OK;
            if (alnumBlank()) {
                alphaFlag = LOW;
                return;
            }
            if (!onlyChars(false)) {
                inputFlag = '1';
                alphaFlag = NOT_OK;
                editMsg(" can have alphabets only.");
                return;
            }
            alphaFlag = LOW;
        }

        /** 1240-EDIT-ALPHANUM-OPT (lines 2061-2107; not PERFORMed by COACTUPC). */
        void editAlphanumOpt1240() {
            alphanumFlag = NOT_OK;
            if (alnumBlank()) {
                alphanumFlag = LOW;
                return;
            }
            if (!onlyChars(true)) {
                inputFlag = '1';
                alphanumFlag = NOT_OK;
                editMsg(" can have numbers or alphabets only.");
                return;
            }
            alphanumFlag = LOW;
        }

        /** 1245-EDIT-NUM-REQD (lines 2109-2178). */
        void editNumReqd1245() {
            alphanumFlag = NOT_OK;
            if (alnumBlank()) {
                inputFlag = '1';
                alphanumFlag = BLANK;
                editMsg(" must be supplied.");
                return;
            }
            if (!CobolNumval.isNumeric(alnum())) {
                inputFlag = '1';
                alphanumFlag = NOT_OK;
                editMsg(" must be all numeric.");
                return;
            }
            if (CobolNumval.numval(alnum()).signum() == 0) {
                inputFlag = '1';
                alphanumFlag = NOT_OK;
                editMsg(" must not be zero.");
                return;
            }
            alphanumFlag = LOW;
        }

        /** 1250-EDIT-SIGNED-9V2 (lines 2180-2223). */
        void editSigned9v2_1250() {
            signedNumberFlag = NOT_OK;
            if (PicX.isSpacesOrLowValues(editSignedNumber9v2X)) {
                inputFlag = '1';
                signedNumberFlag = BLANK;
                editMsg(" must be supplied.");
                return;
            }
            if (CobolNumval.testNumvalC(editSignedNumber9v2X) != 0) {
                inputFlag = '1';
                signedNumberFlag = NOT_OK;
                editMsg(" is not valid");
                return;
            }
            signedNumberFlag = LOW;
        }

        /** 1260-EDIT-US-PHONE-NUM .. 1260-EDIT-US-PHONE-NUM-EXIT (lines 2225-2429), flags in {@link #phoneFlags}. */
        void editUsPhoneNum1260() {
            String a = editUsPhoneNum.substring(1, 4);
            String b = editUsPhoneNum.substring(5, 8);
            String c = editUsPhoneNum.substring(9, 13);
            Arrays.fill(phoneFlags, '0');
            if (PicX.isSpacesOrLowValues(a) && PicX.isSpacesOrLowValues(b)
                    && (PicX.isSpaces(a) || PicX.isLowValues(c))) {
                Arrays.fill(phoneFlags, LOW);
                return;
            }
            phoneFlags[0] = phonePart(phoneFlags[0], a, "Area code", "A 3 digit number", true);
            phoneFlags[1] = phonePart(phoneFlags[1], b, "Prefix code", "A 3 digit number", false);
            phoneFlags[2] = phonePart(phoneFlags[2], c, "Line number code", "A 4 digit number", false);
        }

        /** EDIT-AREA-CODE / EDIT-US-PHONE-PREFIX / EDIT-US-PHONE-LINENUM. */
        private char phonePart(char current, String part, String label, String digits, boolean areaCode) {
            if (PicX.isSpacesOrLowValues(part)) {
                inputFlag = '1';
                editMsg(": " + label + " must be supplied.");
                return BLANK;
            }
            if (!CobolNumval.isNumeric(part)) {
                inputFlag = '1';
                editMsg(": " + label + " must be " + digits + ".");
                return NOT_OK;
            }
            if (Long.parseLong(part) == 0) {
                inputFlag = '1';
                editMsg(": " + label + " cannot be zero");
                return NOT_OK;
            }
            if (areaCode && !Cslkpcdy.VALID_GENERAL_PURP_CODE.contains(PicX.of(trim(part), 3))) {
                inputFlag = '1';
                editMsg(": Not valid North America general purpose area code");
                return NOT_OK;
            }
            return LOW;
        }

        /** 1265-EDIT-US-SSN (lines 2431-2491). Parts 2 and 3 are only edited when part 1 is valid (scope bug). */
        void editUsSsn1265() {
            var n = AcupCommarea.NEW;
            editVariableName = PicX.of("SSN: First 3 chars", 25);
            setAlphanum(g(n.ssn1), 3);
            editNumReqd1245();
            ssnFlags[0] = alphanumFlag;
            if (ssnFlags[0] == LOW) {
                editUsSsnPart1 = g(n.ssn1);
                long p1 = IbmDisplayNumeric.unsignedValue(editUsSsnPart1);
                if (p1 == 0 || p1 == 666 || (p1 >= 900 && p1 <= 999)) {
                    inputFlag = '1';
                    ssnFlags[0] = NOT_OK;
                    editMsg(": should not be 000, 666, or between 900 and 999");
                }
                editVariableName = PicX.of("SSN 4th & 5th chars", 25);
                setAlphanum(g(n.ssn2), 2);
                editNumReqd1245();
                ssnFlags[1] = alphanumFlag;
                editVariableName = PicX.of("SSN Last 4 chars", 25);
                setAlphanum(g(n.ssn3), 4);
                editNumReqd1245();
                ssnFlags[2] = alphanumFlag;
            }
        }

        /** 1270-EDIT-US-STATE-CD (lines 2493-2513). */
        void editUsStateCd1270() {
            if (!Cslkpcdy.VALID_US_STATE_CODE.contains(g(AcupCommarea.NEW.stateCd))) {
                inputFlag = '1';
                setFlag(Flag.STATE, NOT_OK);
                editMsg(": is not a valid state code");
            }
        }

        /** 1275-EDIT-FICO-SCORE (lines 2514-2533). */
        void editFicoScore1275() {
            String raw = AcupCommarea.NEW.ficoScore.raw(acup);
            long v = CobolNumval.isNumeric(raw) ? Long.parseLong(raw) : -1;
            if (v < 300 || v > 850) {
                inputFlag = '1';
                setFlag(Flag.FICO_SCORE, NOT_OK);
                editMsg(": should be between 300 and 850");
            }
        }

        /** 1280-EDIT-US-STATE-ZIP-CD (lines 2536-2560). */
        void editUsStateZipCd1280() {
            String combo = g(AcupCommarea.NEW.stateCd) + g(AcupCommarea.NEW.zip).substring(0, 2);
            if (!Cslkpcdy.VALID_US_STATE_ZIP_CD2_COMBO.contains(combo)) {
                inputFlag = '1';
                setFlag(Flag.STATE, NOT_OK);
                setFlag(Flag.ZIPCODE, NOT_OK);
                if (msgOff()) {
                    setReturnMsg("Invalid zip code for state");
                }
            }
        }

        // ----------------------------------------------------------------- CSUTLDPY

        private long nibbles(String s) {
            try {
                return IbmDisplayNumeric.unsignedValue(s);
            } catch (NumberFormatException e) {
                return -1;
            }
        }

        /** EDIT-DATE-CCYYMMDD .. EDIT-DATE-CCYYMMDD-EXIT (CSUTLDPY): validates {@link #editDate} into {@link #dateFlags}. */
        void editDateCcyymmdd() {
            Arrays.fill(dateFlags, '0');
            // EDIT-YEAR-CCYY
            String ccyy = editDate.substring(0, 4);
            dateFlags[0] = NOT_OK;
            if (PicX.isSpacesOrLowValues(ccyy)) {
                inputFlag = '1';
                dateFlags[0] = BLANK;
                editMsg(" : Year must be supplied.");
            } else if (!CobolNumval.isNumeric(ccyy)) {
                inputFlag = '1';
                editMsg(" must be 4 digit number.");
            } else if (!ccyy.startsWith("20") && !ccyy.startsWith("19")) {
                inputFlag = '1';
                editMsg(" : Century is not valid.");
            } else {
                dateFlags[0] = LOW;
            }
            // EDIT-MONTH
            String mm = editDate.substring(4, 6);
            dateFlags[1] = NOT_OK;
            long mmN = nibbles(mm);
            if (PicX.isSpacesOrLowValues(mm)) {
                inputFlag = '1';
                dateFlags[1] = BLANK;
                editMsg(" : Month must be supplied.");
            } else if (mmN < 1 || mmN > 12 || CobolNumval.testNumval(mm) != 0) {
                inputFlag = '1';
                editMsg(": Month must be a number between 1 and 12.");
            } else {
                mm = String.format("%02d", CobolNumval.numval(mm).longValue() % 100);
                dateFlags[1] = LOW;
            }
            // EDIT-DAY
            String dd = editDate.substring(6, 8);
            dateFlags[2] = LOW;
            if (PicX.isSpacesOrLowValues(dd)) {
                inputFlag = '1';
                dateFlags[2] = BLANK;
                editMsg(" : Day must be supplied.");
            } else if (CobolNumval.testNumval(dd) != 0) {
                inputFlag = '1';
                dateFlags[2] = NOT_OK;
                editMsg(":day must be a number between 1 and 31.");
            } else {
                dd = String.format("%02d", CobolNumval.numval(dd).abs().longValue() % 100);
                long d = Long.parseLong(dd);
                if (d < 1 || d > 31) {
                    inputFlag = '1';
                    dateFlags[2] = NOT_OK;
                    editMsg(":day must be a number between 1 and 31.");
                }
            }
            editDate = ccyy + mm + dd;
            // EDIT-DAY-MONTH-YEAR
            long m = nibbles(mm);
            long d = nibbles(dd);
            boolean month31 = m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12;
            if (!month31 && d == 31) {
                inputFlag = '1';
                dateFlags[2] = NOT_OK;
                dateFlags[1] = NOT_OK;
                editMsg(":Cannot have 31 days in this month.");
                return;
            }
            if (m == 2 && d == 30) {
                inputFlag = '1';
                dateFlags[2] = NOT_OK;
                dateFlags[1] = NOT_OK;
                editMsg(":Cannot have 30 days in this month.");
                return;
            }
            if (m == 2 && d == 29) {
                long year = nibbles(ccyy);
                long divBy = nibbles(ccyy.substring(2)) == 0 ? 400 : 4;
                if (year % divBy != 0) {
                    inputFlag = '1';
                    Arrays.fill(dateFlags, NOT_OK);
                    editMsg(":Not a leap year.Cannot have 29 days in this month.");
                    return;
                }
            }
            if (dateFlags[0] != LOW || dateFlags[1] != LOW || dateFlags[2] != LOW) {
                return;
            }
            // EDIT-DATE-LE: CALL 'CSUTLDTC' (CEEDAYS with mask YYYYMMDD)
            String[] sevMsg = ceedays(editDate);
            if (!sevMsg[0].equals("0000")) {
                inputFlag = '1';
                Arrays.fill(dateFlags, NOT_OK);
                editMsg(" validation error Sev code: " + sevMsg[0] + " Message code: " + sevMsg[1]);
                return;
            }
            if (!inputError()) {
                dateFlags[2] = LOW;
            }
            Arrays.fill(dateFlags, LOW);
        }

        /** CEEDAYS emulation for CSUTLDTC: severity and message number (both 4 digits). */
        static String[] ceedays(String yyyymmdd) {
            if (!CobolNumval.isNumeric(yyyymmdd)) {
                return new String[] {"0003", "2520"};
            }
            try {
                LocalDate dt = LocalDate.of(Integer.parseInt(yyyymmdd.substring(0, 4)),
                        Integer.parseInt(yyyymmdd.substring(4, 6)), Integer.parseInt(yyyymmdd.substring(6, 8)));
                if (dt.isBefore(LocalDate.of(1582, 10, 15))) {
                    return new String[] {"0003", "2513"};
                }
                return new String[] {"0000", "0000"};
            } catch (java.time.DateTimeException e) {
                return new String[] {"0003", "2508"};
            }
        }

        /** EDIT-DATE-OF-BIRTH (CSUTLDPY): the date must be before today. */
        void editDateOfBirth() {
            String today = ctx.currentDate().substring(0, 8);
            if (Long.parseLong(today) <= Long.parseLong(editDate)) {
                inputFlag = '1';
                Arrays.fill(dateFlags, NOT_OK);
                editMsg(":cannot be in the future ");
            }
        }

        // ----------------------------------------------------------------- 2000

        /** 2000-DECIDE-ACTION (lines 2562-2645). */
        void decideAction2000() {
            String a = action();
            if (detailsNotFetched() || aid("PFK12")) {
                if (acctFilterFlag == '1') {
                    wsReturnMsg = PicX.spaces(75);
                    readAcct9000();
                    if (custMasterReadFlag == '1') {
                        s(AcupCommarea.CHANGE_ACTION, AcupCommarea.SHOW_DETAILS);
                    }
                }
            } else if (a.equals(AcupCommarea.SHOW_DETAILS)) {
                if (!(inputError() || PicX.equal(wsReturnMsg, NO_CHANGES_DETECTED))) {
                    s(AcupCommarea.CHANGE_ACTION, AcupCommarea.CHANGES_OK_NOT_CONFIRMED);
                }
            } else if (a.equals(AcupCommarea.CHANGES_NOT_OK)) {
                // CONTINUE
            } else if (a.equals(AcupCommarea.CHANGES_OK_NOT_CONFIRMED) && aid("PFK05")) {
                writeProcessing9600();
                if (PicX.equal(wsReturnMsg, COULD_NOT_LOCK_ACCT_FOR_UPDATE)) {
                    s(AcupCommarea.CHANGE_ACTION, AcupCommarea.CHANGES_OKAYED_LOCK_ERROR);
                } else if (PicX.equal(wsReturnMsg, LOCKED_BUT_UPDATE_FAILED)) {
                    s(AcupCommarea.CHANGE_ACTION, AcupCommarea.CHANGES_OKAYED_BUT_FAILED);
                } else if (PicX.equal(wsReturnMsg, DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                    s(AcupCommarea.CHANGE_ACTION, AcupCommarea.SHOW_DETAILS);
                } else {
                    s(AcupCommarea.CHANGE_ACTION, AcupCommarea.CHANGES_OKAYED_AND_DONE);
                }
            } else if (a.equals(AcupCommarea.CHANGES_OK_NOT_CONFIRMED)) {
                // CONTINUE
            } else if (a.equals(AcupCommarea.CHANGES_OKAYED_AND_DONE)) {
                s(AcupCommarea.CHANGE_ACTION, AcupCommarea.SHOW_DETAILS);
                if (PicX.isSpacesOrLowValues(cdemo.fromTranid())) {
                    CardDemoCommarea.ACCT_ID.set(cdemo, 0);
                    CardDemoCommarea.CARD_NUM.set(cdemo, 0);
                    CardDemoCommarea.ACCT_STATUS.fill(cdemo, LOW);
                }
            } else {
                AbendData.CULPRIT.set(abendData, PROGRAM);
                AbendData.CODE.set(abendData, "0001");
                AbendData.REASON.set(abendData, "");
                AbendData.MSG.set(abendData, "UNEXPECTED DATA SCENARIO");
                abendRoutine();
            }
        }

        // ----------------------------------------------------------------- 3000 send map

        /** 3000-SEND-MAP (lines 2649-2666). */
        void sendMap3000() {
            screenInit3100();
            setupScreenVars3200();
            setupInfomsg3250();
            setupScreenAttrs3300();
            setupInfomsgAttrs3390();
            sendScreen3400();
        }

        /** 3100-SCREEN-INIT (lines 2668-2696). */
        void screenInit3100() {
            map.lowValues();
            DateTimeFields dt = DateTimeFields.fromCurrentDate(ctx.currentDate());
            map.setData("TITLE01", ScreenTitles.CCDA_TITLE01);
            map.setData("TITLE02", ScreenTitles.CCDA_TITLE02);
            map.setData("TRNNAME", TRANSID);
            map.setData("PGMNAME", PROGRAM);
            map.setData("CURDATE", dt.curdateMmDdYy());
            map.setData("CURTIME", dt.curtimeHhMmSs());
        }

        private long ccAcctIdN() {
            try {
                return IbmDisplayNumeric.unsignedValue(ccAcctId);
            } catch (NumberFormatException e) {
                return -1;
            }
        }

        /** 3200-SETUP-SCREEN-VARS (lines 2698-2729). */
        void setupScreenVars3200() {
            if (cdemo.isPgmEnter()) {
                return;
            }
            if (ccAcctIdN() == 0 && acctFilterFlag == '1') {
                map.setData("ACCTSID", PicX.lowValues(11));
            } else {
                map.setData("ACCTSID", ccAcctId);
            }
            if (detailsNotFetched() || ccAcctIdN() == 0) {
                showInitialValues3201();
            } else if (action().equals(AcupCommarea.SHOW_DETAILS)) {
                showOriginalValues3202();
            } else if (changesMade()) {
                showUpdatedValues3203();
            } else {
                showOriginalValues3202();
            }
        }

        private static final String[] DETAIL_FIELDS = {"ACSTTUS", "ACRDLIM", "ACURBAL", "ACSHLIM", "ACRCYCR",
            "ACRCYDB", "OPNYEAR", "OPNMON", "OPNDAY", "EXPYEAR", "EXPMON", "EXPDAY", "RISYEAR", "RISMON", "RISDAY",
            "AADDGRP", "ACSTNUM", "ACTSSN1", "ACTSSN2", "ACTSSN3", "ACSTFCO", "DOBYEAR", "DOBMON", "DOBDAY",
            "ACSFNAM", "ACSMNAM", "ACSLNAM", "ACSADL1", "ACSADL2", "ACSCITY", "ACSSTTE", "ACSZIPC", "ACSCTRY",
            "ACSPH1A", "ACSPH1B", "ACSPH1C", "ACSPH2A", "ACSPH2B", "ACSPH2C", "ACSGOVT", "ACSEFTC", "ACSPFLG"};

        /** 3201-SHOW-INITIAL-VALUES (lines 2731-2785). */
        void showInitialValues3201() {
            for (String f : DETAIL_FIELDS) {
                map.setData(f, PicX.lowValues(map.data(f).length()));
            }
        }

        /** 3202-SHOW-ORIGINAL-VALUES (lines 2787-2869). */
        void showOriginalValues3202() {
            var o = AcupCommarea.OLD;
            Arrays.fill(nonKey, LOW);
            wsInfoMsg = PicX.of(PROMPT_FOR_CHANGES, 40);
            if (acctMasterReadFlag == '1' || custMasterReadFlag == '1') {
                map.setData("ACSTTUS", g(o.activeStatus));
                map.setData("ACURBAL", editCurrency(g(o.currBal)));
                map.setData("ACRDLIM", editCurrency(g(o.creditLimit)));
                map.setData("ACSHLIM", editCurrency(g(o.cashCreditLimit)));
                map.setData("ACRCYCR", editCurrency(g(o.currCycCredit)));
                map.setData("ACRCYDB", editCurrency(g(o.currCycDebit)));
                map.setData("OPNYEAR", g(o.openYear));
                map.setData("OPNMON", g(o.openMon));
                map.setData("OPNDAY", g(o.openDay));
                map.setData("EXPYEAR", g(o.expYear));
                map.setData("EXPMON", g(o.expMon));
                map.setData("EXPDAY", g(o.expDay));
                map.setData("RISYEAR", g(o.reissueYear));
                map.setData("RISMON", g(o.reissueMon));
                map.setData("RISDAY", g(o.reissueDay));
                map.setData("AADDGRP", g(o.groupId));
            }
            if (custMasterReadFlag == '1') {
                String ssn = g(o.ssnX);
                map.setData("ACSTNUM", g(o.custIdX));
                map.setData("ACTSSN1", ssn.substring(0, 3));
                map.setData("ACTSSN2", ssn.substring(3, 5));
                map.setData("ACTSSN3", ssn.substring(5, 9));
                map.setData("ACSTFCO", g(o.ficoScoreX));
                map.setData("DOBYEAR", g(o.dobYear));
                map.setData("DOBMON", g(o.dobMon));
                map.setData("DOBDAY", g(o.dobDay));
                map.setData("ACSFNAM", g(o.firstName));
                map.setData("ACSMNAM", g(o.middleName));
                map.setData("ACSLNAM", g(o.lastName));
                map.setData("ACSADL1", g(o.addrLine1));
                map.setData("ACSADL2", g(o.addrLine2));
                map.setData("ACSCITY", g(o.addrLine3));
                map.setData("ACSSTTE", g(o.stateCd));
                map.setData("ACSZIPC", g(o.zip));
                map.setData("ACSCTRY", g(o.countryCd));
                String p1 = g(o.phone1);
                String p2 = g(o.phone2);
                map.setData("ACSPH1A", p1.substring(1, 4));
                map.setData("ACSPH1B", p1.substring(5, 8));
                map.setData("ACSPH1C", p1.substring(9, 13));
                map.setData("ACSPH2A", p2.substring(1, 4));
                map.setData("ACSPH2B", p2.substring(5, 8));
                map.setData("ACSPH2C", p2.substring(9, 13));
                map.setData("ACSGOVT", g(o.govtIssuedId));
                map.setData("ACSEFTC", g(o.eftAccountId));
                map.setData("ACSPFLG", g(o.priHolderInd));
            }
        }

        /** 3203-SHOW-UPDATED-VALUES (lines 2870-2953). */
        void showUpdatedValues3203() {
            var n = AcupCommarea.NEW;
            map.setData("ACSTTUS", g(n.activeStatus));
            map.setData("ACRDLIM", flag(Flag.CRED_LIMIT) == LOW ? editCurrency(g(n.creditLimit)) : newCreditLimitX);
            map.setData("ACSHLIM", flag(Flag.CASH_CREDIT_LIMIT) == LOW ? editCurrency(g(n.cashCreditLimit)) : newCashCreditLimitX);
            map.setData("ACURBAL", flag(Flag.CURR_BAL) == LOW ? editCurrency(g(n.currBal)) : newCurrBalX);
            map.setData("ACRCYCR", flag(Flag.CURR_CYC_CREDIT) == LOW ? editCurrency(g(n.currCycCredit)) : newCurrCycCreditX);
            map.setData("ACRCYDB", flag(Flag.CURR_CYC_DEBIT) == LOW ? editCurrency(g(n.currCycDebit)) : newCurrCycDebitX);
            map.setData("OPNYEAR", g(n.openYear));
            map.setData("OPNMON", g(n.openMon));
            map.setData("OPNDAY", g(n.openDay));
            map.setData("EXPYEAR", g(n.expYear));
            map.setData("EXPMON", g(n.expMon));
            map.setData("EXPDAY", g(n.expDay));
            map.setData("RISYEAR", g(n.reissueYear));
            map.setData("RISMON", g(n.reissueMon));
            map.setData("RISDAY", g(n.reissueDay));
            map.setData("AADDGRP", g(n.groupId));
            map.setData("ACSTNUM", g(n.custIdX));
            map.setData("ACTSSN1", g(n.ssn1));
            map.setData("ACTSSN2", g(n.ssn2));
            map.setData("ACTSSN3", g(n.ssn3));
            map.setData("ACSTFCO", g(n.ficoScoreX));
            map.setData("DOBYEAR", g(n.dobYear));
            map.setData("DOBMON", g(n.dobMon));
            map.setData("DOBDAY", g(n.dobDay));
            map.setData("ACSFNAM", g(n.firstName));
            map.setData("ACSMNAM", g(n.middleName));
            map.setData("ACSLNAM", g(n.lastName));
            map.setData("ACSADL1", g(n.addrLine1));
            map.setData("ACSADL2", g(n.addrLine2));
            map.setData("ACSCITY", g(n.addrLine3));
            map.setData("ACSSTTE", g(n.stateCd));
            map.setData("ACSZIPC", g(n.zip));
            map.setData("ACSCTRY", g(n.countryCd));
            map.setData("ACSPH1A", g(n.phone1a));
            map.setData("ACSPH1B", g(n.phone1b));
            map.setData("ACSPH1C", g(n.phone1c));
            map.setData("ACSPH2A", g(n.phone2a));
            map.setData("ACSPH2B", g(n.phone2b));
            map.setData("ACSPH2C", g(n.phone2c));
            map.setData("ACSGOVT", g(n.govtIssuedId));
            map.setData("ACSEFTC", g(n.eftAccountId));
            map.setData("ACSPFLG", g(n.priHolderInd));
        }

        /** 3250-SETUP-INFOMSG (lines 2955-2985). */
        void setupInfomsg3250() {
            String a = action();
            if (cdemo.isPgmEnter() || detailsNotFetched()) {
                wsInfoMsg = PicX.of(PROMPT_FOR_SEARCH_KEYS, 40);
            } else if (a.equals(AcupCommarea.SHOW_DETAILS) || a.equals(AcupCommarea.CHANGES_NOT_OK)) {
                wsInfoMsg = PicX.of(PROMPT_FOR_CHANGES, 40);
            } else if (a.equals(AcupCommarea.CHANGES_OK_NOT_CONFIRMED)) {
                wsInfoMsg = PicX.of(PROMPT_FOR_CONFIRMATION, 40);
            } else if (a.equals(AcupCommarea.CHANGES_OKAYED_AND_DONE)) {
                wsInfoMsg = PicX.of(CONFIRM_UPDATE_SUCCESS, 40);
            } else if (changesFailed()) {
                wsInfoMsg = PicX.of(INFORM_FAILURE, 40);
            } else if (PicX.isSpacesOrLowValues(wsInfoMsg)) {
                wsInfoMsg = PicX.of(PROMPT_FOR_SEARCH_KEYS, 40);
            }
            map.setData("INFOMSG", wsInfoMsg);
            map.setData("ERRMSG", wsReturnMsg);
        }

        /** 3300-SETUP-SCREEN-ATTRS (lines 2986-3439). */
        void setupScreenAttrs3300() {
            protectAllAttrs3310();
            String a = action();
            if (detailsNotFetched()) {
                map.setAttribute("ACCTSID", Dfhbmsca.DFHBMFSE);
            } else if (a.equals(AcupCommarea.SHOW_DETAILS) || a.equals(AcupCommarea.CHANGES_NOT_OK)) {
                unprotectFewAttrs3320();
            } else if (a.equals(AcupCommarea.CHANGES_OK_NOT_CONFIRMED) || a.equals(AcupCommarea.CHANGES_OKAYED_AND_DONE)) {
                // CONTINUE
            } else {
                map.setAttribute("ACCTSID", Dfhbmsca.DFHBMFSE);
            }
            if (PicX.equal(wsInfoMsg, FOUND_ACCOUNT_DATA) || PicX.equal(wsReturnMsg, NO_CHANGES_DETECTED)) {
                map.setCursor("ACSTTUS");
            } else if (acctFilterFlag == '0' || acctFilterFlag == ' ') {
                map.setCursor("ACCTSID");
            } else {
                String cursor = "ACCTSID";
                for (Flag f : CURSOR_ORDER) {
                    char v = flag(f);
                    if (v == NOT_OK || (v == BLANK && f != Flag.MIDDLE_NAME)) {
                        cursor = f.mapField;
                        break;
                    }
                }
                map.setCursor(cursor);
            }
            if (PicX.equal(cdemo.lastMapset(), LIT_CCLISTMAPSET)) {
                map.setColor("ACCTSID", Dfhbmsca.DFHDFCOL);
            }
            if (acctFilterFlag == '0') {
                map.setColor("ACCTSID", Dfhbmsca.DFHRED);
            }
            if (acctFilterFlag == ' ' && cdemo.isPgmReenter()) {
                map.setData("ACCTSID", "*");
                map.setColor("ACCTSID", Dfhbmsca.DFHRED);
            }
            if (detailsNotFetched() || acctFilterFlag == ' ' || acctFilterFlag == '0') {
                return;
            }
            for (Flag f : CSSETATY_ORDER) {
                char v = flag(f);
                if ((v == NOT_OK || v == BLANK) && cdemo.isPgmReenter()) {
                    map.setColor(f.mapField, Dfhbmsca.DFHRED);
                    if (v == BLANK) {
                        map.setData(f.mapField, "*");
                    }
                }
            }
        }

        /** 3310-PROTECT-ALL-ATTRS (lines 3441-3498). */
        void protectAllAttrs3310() {
            map.setAttribute("ACCTSID", Dfhbmsca.DFHBMPRF);
            for (String f : DETAIL_FIELDS) {
                map.setAttribute(f, Dfhbmsca.DFHBMPRF);
            }
            map.setAttribute("INFOMSG", Dfhbmsca.DFHBMPRF);
        }

        /** 3320-UNPROTECT-FEW-ATTRS (lines 3500-3564). */
        void unprotectFewAttrs3320() {
            for (String f : DETAIL_FIELDS) {
                map.setAttribute(f, Dfhbmsca.DFHBMFSE);
            }
            map.setAttribute("ACSTNUM", Dfhbmsca.DFHBMPRF);
            map.setAttribute("ACSCTRY", Dfhbmsca.DFHBMPRF);
            map.setAttribute("INFOMSG", Dfhbmsca.DFHBMPRF);
        }

        /** 3390-SETUP-INFOMSG-ATTRS (lines 3566-3586). */
        void setupInfomsgAttrs3390() {
            map.setAttribute("INFOMSG", PicX.isSpacesOrLowValues(wsInfoMsg) ? Dfhbmsca.DFHBMDAR : Dfhbmsca.DFHBMASB);
            if (changesMade() && !action().equals(AcupCommarea.CHANGES_OKAYED_AND_DONE)) {
                map.setAttribute("FKEY12", Dfhbmsca.DFHBMASB);
            }
            if (PicX.equal(wsInfoMsg, PROMPT_FOR_CONFIRMATION)) {
                map.setAttribute("FKEY05", Dfhbmsca.DFHBMASB);
                map.setAttribute("FKEY12", Dfhbmsca.DFHBMASB);
            }
        }

        /** 3400-SEND-SCREEN (lines 3589-3605). */
        void sendScreen3400() {
            ccardNextMapset = PicX.of(MAPSET, 7);
            ccardNextMap = MAP;
            CicsResp resp = ctx.sendMap(map, SendMapOption.CURSOR, SendMapOption.ERASE, SendMapOption.FREEKB);
            wsRespCd = resp.code();
        }

        // ----------------------------------------------------------------- 9000 file reads

        /** 9000-READ-ACCT (lines 3608-3649). */
        void readAcct9000() {
            AcupCommarea.OLD.group.fill(acup, ' ');
            wsInfoMsg = PicX.spaces(40);
            String acctId = IbmDisplayNumeric.moveAlphanumericToUnsigned(ccAcctId);
            s(AcupCommarea.OLD.acctIdX, acctId);
            ridAcctId = acctId;
            getCardXrefByAcct9200();
            if (acctFilterFlag == '0') {
                return;
            }
            getAcctDataByAcct9300();
            if (PicX.equal(wsReturnMsg, DID_NOT_FIND_ACCT_IN_ACCTDAT)) {
                return;
            }
            ridCustId = CardDemoCommarea.CUST_ID.raw(cdemo);
            getCustDataByCust9400();
            if (PicX.equal(wsReturnMsg, DID_NOT_FIND_CUST_IN_CUSTDAT)) {
                return;
            }
            storeFetchedData9500();
        }

        /** WS-FILE-ERROR-MESSAGE (lines 552-566) moved to WS-RETURN-MSG. */
        private void fileError(String opname, String file, FileResult r) {
            inputFlag = '1';
            errorOpname = PicX.of(opname, 8);
            errorFile = PicX.of(file, 9);
            errorResp = respText(r.resp().code());
            errorResp2 = respText(r.resp2());
            setReturnMsg("File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp
                    + ",RESP2 " + errorResp2 + PicX.spaces(5));
        }

        /** 9200-GETCARDXREF-BYACCT (lines 3650-3699). */
        void getCardXrefByAcct9200() {
            FileResult r = ctx.read(CardXrefRecord.ACCT_PATH_NAME, ridAcctId, CardXrefRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.resp() == CicsResp.NORMAL) {
                xrefRecord.setBytes(r.data());
                CardDemoCommarea.CUST_ID.fill(cdemo, ' ');
                copyRaw(cdemo, CardDemoCommarea.CUST_ID, xrefRecord, CardXrefRecord.CUST_ID);
                setRaw(cdemo, CardDemoCommarea.CARD_NUM,
                        IbmDisplayNumeric.moveAlphanumericToUnsigned(CardXrefRecord.CARD_NUM.get(xrefRecord)));
            } else if (r.resp() == CicsResp.NOTFND) {
                inputFlag = '1';
                acctFilterFlag = '0';
                if (msgOff()) {
                    errorResp = respText(wsRespCd);
                    errorResp2 = respText(wsReasCd);
                    setReturnMsg("Account:" + ridAcctId + " not found in Cross ref file.  Resp:" + errorResp
                            + " Reas:" + errorResp2);
                }
            } else {
                acctFilterFlag = '0';
                fileError("READ", CardXrefRecord.ACCT_PATH_NAME, r);
            }
        }

        /** 9300-GETACCTDATA-BYACCT (lines 3701-3749). */
        void getAcctDataByAcct9300() {
            FileResult r = ctx.read(AccountRecord.FILE_NAME, ridAcctId, AccountRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.resp() == CicsResp.NORMAL) {
                accountRecord.setBytes(r.data());
                acctMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                inputFlag = '1';
                acctFilterFlag = '0';
                if (msgOff()) {
                    errorResp = respText(wsRespCd);
                    errorResp2 = respText(wsReasCd);
                    setReturnMsg("Account:" + ridAcctId + " not found in Acct Master file.Resp:" + errorResp
                            + " Reas:" + errorResp2);
                }
            } else {
                acctFilterFlag = '0';
                fileError("READ", AccountRecord.FILE_NAME, r);
            }
        }

        /** 9400-GETCUSTDATA-BYCUST (lines 3752-3798). */
        void getCustDataByCust9400() {
            FileResult r = ctx.read(CustomerRecord.FILE_NAME, ridCustId, CustomerRecord.LENGTH);
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (r.resp() == CicsResp.NORMAL) {
                customerRecord.setBytes(r.data());
                custMasterReadFlag = '1';
            } else if (r.resp() == CicsResp.NOTFND) {
                inputFlag = '1';
                custFilterFlag = '0';
                errorResp = respText(wsRespCd);
                errorResp2 = respText(wsReasCd);
                if (msgOff()) {
                    setReturnMsg("CustId:" + ridCustId + " not found in customer master.Resp: " + errorResp
                            + " REAS:" + errorResp2);
                }
            } else {
                custFilterFlag = '0';
                fileError("READ", CustomerRecord.FILE_NAME, r);
            }
        }

        static void setRaw(CobolRecord rec, com.carddemo.runtime.cobol.CobolField field, String raw) {
            new AlphanumericField(field.name(), field.offset(), field.length()).set(rec, raw);
        }

        /** 9500-STORE-FETCHED-DATA (lines 3801-3886). */
        void storeFetchedData9500() {
            var o = AcupCommarea.OLD;
            AccountRecord a = accountRecord;
            CustomerRecord c = customerRecord;
            copyRaw(cdemo, CardDemoCommarea.ACCT_ID, a, AccountRecord.ACCT_ID);
            copyRaw(cdemo, CardDemoCommarea.CUST_ID, c, CustomerRecord.CUST_ID);
            CardDemoCommarea.CUST_FNAME.set(cdemo, CustomerRecord.FIRST_NAME.get(c));
            CardDemoCommarea.CUST_MNAME.set(cdemo, CustomerRecord.MIDDLE_NAME.get(c));
            CardDemoCommarea.CUST_LNAME.set(cdemo, CustomerRecord.LAST_NAME.get(c));
            CardDemoCommarea.ACCT_STATUS.set(cdemo, AccountRecord.ACTIVE_STATUS.get(a));
            setRaw(cdemo, CardDemoCommarea.CARD_NUM,
                    IbmDisplayNumeric.moveAlphanumericToUnsigned(CardXrefRecord.CARD_NUM.get(xrefRecord)));

            o.group.fill(acup, ' ');
            copyRaw(acup, o.acctId, a, AccountRecord.ACCT_ID);
            s(o.activeStatus, AccountRecord.ACTIVE_STATUS.get(a));
            copyRaw(acup, o.currBalN, a, AccountRecord.CURR_BAL);
            copyRaw(acup, o.creditLimitN, a, AccountRecord.CREDIT_LIMIT);
            copyRaw(acup, o.cashCreditLimitN, a, AccountRecord.CASH_CREDIT_LIMIT);
            copyRaw(acup, o.currCycCreditN, a, AccountRecord.CURR_CYC_CREDIT);
            copyRaw(acup, o.currCycDebitN, a, AccountRecord.CURR_CYC_DEBIT);
            String open = AccountRecord.OPEN_DATE.get(a);
            s(o.openYear, open.substring(0, 4));
            s(o.openMon, open.substring(5, 7));
            s(o.openDay, open.substring(8, 10));
            String exp = AccountRecord.EXPIRAION_DATE.get(a);
            s(o.expYear, exp.substring(0, 4));
            s(o.expMon, exp.substring(5, 7));
            s(o.expDay, exp.substring(8, 10));
            String ris = AccountRecord.REISSUE_DATE.get(a);
            s(o.reissueYear, ris.substring(0, 4));
            s(o.reissueMon, ris.substring(5, 7));
            s(o.reissueDay, ris.substring(8, 10));
            s(o.groupId, AccountRecord.GROUP_ID.get(a));

            copyRaw(acup, o.custId, c, CustomerRecord.CUST_ID);
            copyRaw(acup, o.ssn, c, CustomerRecord.SSN);
            String dob = CustomerRecord.DOB_YYYY_MM_DD.get(c);
            s(o.dobYear, dob.substring(0, 4));
            s(o.dobMon, dob.substring(5, 7));
            s(o.dobDay, dob.substring(8, 10));
            copyRaw(acup, o.ficoScore, c, CustomerRecord.FICO_CREDIT_SCORE);
            s(o.firstName, CustomerRecord.FIRST_NAME.get(c));
            s(o.middleName, CustomerRecord.MIDDLE_NAME.get(c));
            s(o.lastName, CustomerRecord.LAST_NAME.get(c));
            s(o.addrLine1, CustomerRecord.ADDR_LINE_1.get(c));
            s(o.addrLine2, CustomerRecord.ADDR_LINE_2.get(c));
            s(o.addrLine3, CustomerRecord.ADDR_LINE_3.get(c));
            s(o.stateCd, CustomerRecord.ADDR_STATE_CD.get(c));
            s(o.countryCd, CustomerRecord.ADDR_COUNTRY_CD.get(c));
            s(o.zip, CustomerRecord.ADDR_ZIP.get(c));
            s(o.phone1, CustomerRecord.PHONE_NUM_1.get(c));
            s(o.phone2, CustomerRecord.PHONE_NUM_2.get(c));
            s(o.govtIssuedId, CustomerRecord.GOVT_ISSUED_ID.get(c));
            s(o.eftAccountId, CustomerRecord.EFT_ACCOUNT_ID.get(c));
            s(o.priHolderInd, CustomerRecord.PRI_CARD_HOLDER_IND.get(c));
        }

        // ----------------------------------------------------------------- 9600 / 9700 update

        /** 9600-WRITE-PROCESSING (lines 3888-4106). */
        void writeProcessing9600() {
            var n = AcupCommarea.NEW;
            ridAcctId = IbmDisplayNumeric.moveAlphanumericToUnsigned(ccAcctId);
            FileResult r = ctx.track(ctx.files().readForUpdate(AccountRecord.FILE_NAME, ridAcctId));
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (!r.isNormal()) {
                inputFlag = '1';
                if (msgOff()) {
                    setReturnMsg(COULD_NOT_LOCK_ACCT_FOR_UPDATE);
                }
                return;
            }
            accountRecord.setBytes(r.data());
            ridCustId = CardDemoCommarea.CUST_ID.raw(cdemo);
            r = ctx.track(ctx.files().readForUpdate(CustomerRecord.FILE_NAME, ridCustId));
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (!r.isNormal()) {
                inputFlag = '1';
                if (msgOff()) {
                    setReturnMsg(COULD_NOT_LOCK_CUST_FOR_UPDATE);
                }
                return;
            }
            customerRecord.setBytes(r.data());

            checkChangeInRec9700();
            if (PicX.equal(wsReturnMsg, DATA_WAS_CHANGED_BEFORE_UPDATE)) {
                return;
            }

            initializeAcctUpdateRecord();
            copyRaw(acctUpdate, AcctUpdateRecord.ID, acup, n.acctId);
            AcctUpdateRecord.ACTIVE_STATUS.set(acctUpdate, g(n.activeStatus));
            copyRaw(acctUpdate, AcctUpdateRecord.CURR_BAL, acup, n.currBalN);
            copyRaw(acctUpdate, AcctUpdateRecord.CREDIT_LIMIT, acup, n.creditLimitN);
            copyRaw(acctUpdate, AcctUpdateRecord.CASH_CREDIT_LIMIT, acup, n.cashCreditLimitN);
            copyRaw(acctUpdate, AcctUpdateRecord.CURR_CYC_CREDIT, acup, n.currCycCreditN);
            copyRaw(acctUpdate, AcctUpdateRecord.CURR_CYC_DEBIT, acup, n.currCycDebitN);
            AcctUpdateRecord.OPEN_DATE.set(acctUpdate, g(n.openYear) + "-" + g(n.openMon) + "-" + g(n.openDay));
            AcctUpdateRecord.EXPIRAION_DATE.set(acctUpdate, g(n.expYear) + "-" + g(n.expMon) + "-" + g(n.expDay));
            AcctUpdateRecord.REISSUE_DATE.set(acctUpdate, AccountRecord.REISSUE_DATE.get(accountRecord));
            AcctUpdateRecord.REISSUE_DATE.set(acctUpdate,
                    g(n.reissueYear) + "-" + g(n.reissueMon) + "-" + g(n.reissueDay));
            AcctUpdateRecord.GROUP_ID.set(acctUpdate, g(n.groupId));

            initializeCustUpdateRecord();
            CustomerRecord u = custUpdate;
            copyRaw(u, CustomerRecord.CUST_ID, acup, n.custId);
            CustomerRecord.FIRST_NAME.set(u, g(n.firstName));
            CustomerRecord.MIDDLE_NAME.set(u, g(n.middleName));
            CustomerRecord.LAST_NAME.set(u, g(n.lastName));
            CustomerRecord.ADDR_LINE_1.set(u, g(n.addrLine1));
            CustomerRecord.ADDR_LINE_2.set(u, g(n.addrLine2));
            CustomerRecord.ADDR_LINE_3.set(u, g(n.addrLine3));
            CustomerRecord.ADDR_STATE_CD.set(u, g(n.stateCd));
            CustomerRecord.ADDR_COUNTRY_CD.set(u, g(n.countryCd));
            CustomerRecord.ADDR_ZIP.set(u, g(n.zip));
            CustomerRecord.PHONE_NUM_1.set(u, "(" + g(n.phone1a) + ")" + g(n.phone1b) + "-" + g(n.phone1c));
            CustomerRecord.PHONE_NUM_2.set(u, "(" + g(n.phone2a) + ")" + g(n.phone2b) + "-" + g(n.phone2c));
            copyRaw(u, CustomerRecord.SSN, acup, n.ssn);
            CustomerRecord.GOVT_ISSUED_ID.set(u, g(n.govtIssuedId));
            CustomerRecord.DOB_YYYY_MM_DD.set(u, g(n.dobYear) + "-" + g(n.dobMon) + "-" + g(n.dobDay));
            CustomerRecord.EFT_ACCOUNT_ID.set(u, g(n.eftAccountId));
            CustomerRecord.PRI_CARD_HOLDER_IND.set(u, g(n.priHolderInd));
            copyRaw(u, CustomerRecord.FICO_CREDIT_SCORE, acup, n.ficoScore);

            r = ctx.track(ctx.files().rewrite(AccountRecord.FILE_NAME, acctUpdate.asString()));
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (!r.isNormal()) {
                setReturnMsg(LOCKED_BUT_UPDATE_FAILED);
                return;
            }
            r = ctx.track(ctx.files().rewrite(CustomerRecord.FILE_NAME, custUpdate.asString()));
            wsRespCd = r.resp().code();
            wsReasCd = r.resp2();
            if (!r.isNormal()) {
                setReturnMsg(LOCKED_BUT_UPDATE_FAILED);
                ctx.syncpointRollback();
            }
        }

        /** Numeric comparison of two DISPLAY items (raw comparison if either is not valid numeric data). */
        private static boolean numEq(String a, String b, int scale, boolean signed) {
            if (ZonedDecimal.isNumeric(a, signed) && ZonedDecimal.isNumeric(b, signed)) {
                return ZonedDecimal.decode(a, scale).compareTo(ZonedDecimal.decode(b, scale)) == 0;
            }
            return a.equals(b);
        }

        /** 9700-CHECK-CHANGE-IN-REC (lines 4109-4194). */
        void checkChangeInRec9700() {
            var o = AcupCommarea.OLD;
            AccountRecord a = accountRecord;
            CustomerRecord c = customerRecord;
            String open = AccountRecord.OPEN_DATE.get(a);
            String exp = AccountRecord.EXPIRAION_DATE.get(a);
            String ris = AccountRecord.REISSUE_DATE.get(a);
            boolean acctSame = PicX.equal(AccountRecord.ACTIVE_STATUS.get(a), g(o.activeStatus))
                    && numEq(AccountRecord.CURR_BAL.raw(a), o.currBalN.raw(acup), 2, true)
                    && numEq(AccountRecord.CREDIT_LIMIT.raw(a), o.creditLimitN.raw(acup), 2, true)
                    && numEq(AccountRecord.CASH_CREDIT_LIMIT.raw(a), o.cashCreditLimitN.raw(acup), 2, true)
                    && numEq(AccountRecord.CURR_CYC_CREDIT.raw(a), o.currCycCreditN.raw(acup), 2, true)
                    && numEq(AccountRecord.CURR_CYC_DEBIT.raw(a), o.currCycDebitN.raw(acup), 2, true)
                    && open.substring(0, 4).equals(g(o.openYear)) && open.substring(5, 7).equals(g(o.openMon))
                    && open.substring(8, 10).equals(g(o.openDay))
                    && exp.substring(0, 4).equals(g(o.expYear)) && exp.substring(5, 7).equals(g(o.expMon))
                    && exp.substring(8, 10).equals(g(o.expDay))
                    && ris.substring(0, 4).equals(g(o.reissueYear)) && ris.substring(5, 7).equals(g(o.reissueMon))
                    && ris.substring(8, 10).equals(g(o.reissueDay))
                    && PicX.equal(PicX.lowerCase(AccountRecord.GROUP_ID.get(a)), PicX.lowerCase(g(o.groupId)));
            if (!acctSame) {
                setReturnMsg(DATA_WAS_CHANGED_BEFORE_UPDATE);
                return;
            }
            String dob = CustomerRecord.DOB_YYYY_MM_DD.get(c);
            String oldDob = g(o.dob);
            boolean custSame = eqU(CustomerRecord.FIRST_NAME.get(c), g(o.firstName))
                    && eqU(CustomerRecord.MIDDLE_NAME.get(c), g(o.middleName))
                    && eqU(CustomerRecord.LAST_NAME.get(c), g(o.lastName))
                    && eqU(CustomerRecord.ADDR_LINE_1.get(c), g(o.addrLine1))
                    && eqU(CustomerRecord.ADDR_LINE_2.get(c), g(o.addrLine2))
                    && eqU(CustomerRecord.ADDR_LINE_3.get(c), g(o.addrLine3))
                    && eqU(CustomerRecord.ADDR_STATE_CD.get(c), g(o.stateCd))
                    && eqU(CustomerRecord.ADDR_COUNTRY_CD.get(c), g(o.countryCd))
                    && PicX.equal(CustomerRecord.ADDR_ZIP.get(c), g(o.zip))
                    && PicX.equal(CustomerRecord.PHONE_NUM_1.get(c), g(o.phone1))
                    && PicX.equal(CustomerRecord.PHONE_NUM_2.get(c), g(o.phone2))
                    && numEq(CustomerRecord.SSN.raw(c), o.ssn.raw(acup), 0, false)
                    && eqU(CustomerRecord.GOVT_ISSUED_ID.get(c), g(o.govtIssuedId))
                    && dob.substring(0, 4).equals(oldDob.substring(0, 4))
                    && dob.substring(5, 7).equals(oldDob.substring(4, 6))
                    && dob.substring(8, 10).equals(oldDob.substring(6, 8))
                    && PicX.equal(CustomerRecord.EFT_ACCOUNT_ID.get(c), g(o.eftAccountId))
                    && PicX.equal(CustomerRecord.PRI_CARD_HOLDER_IND.get(c), g(o.priHolderInd))
                    && numEq(CustomerRecord.FICO_CREDIT_SCORE.raw(c), o.ficoScore.raw(acup), 0, false);
            if (!custSame) {
                setReturnMsg(DATA_WAS_CHANGED_BEFORE_UPDATE);
            }
        }

        /** ABEND-ROUTINE (lines 4203-4228). */
        void abendRoutine() {
            if (AbendData.MSG.isLowValues(abendData)) {
                AbendData.MSG.set(abendData, "UNEXPECTED ABEND OCCURRED.");
            }
            AbendData.CULPRIT.set(abendData, PROGRAM);
            ctx.sendText(abendData.asString(), AbendData.LENGTH, true, false);
            ctx.abend("9999");
        }
    }
}

package com.carddemo.programs.coactupc;

import com.carddemo.programs.coactupc.copybook.DateEditWorkArea;
import com.carddemo.programs.coactupc.support.Cobol;
import java.util.EnumMap;
import java.util.Map;

/** WS-MISC-STORAGE - COACTUPC working storage (edit work areas, flags, messages, update records). */
final class WsMiscStorage {

    static final char FLAG_ISVALID = Cobol.LOW_VALUE;
    static final char FLAG_NOT_OK = '0';
    static final char FLAG_BLANK = 'B';

    // WS-CICS-PROCESSNG-VARS
    int respCd;
    int reasCd;
    String tranId;
    String ucTrans;

    // WS-GENERIC-EDITS
    String editVariableName;
    String editSignedNumber9v2X;
    char flgSignedNumberEdit;
    String editAlphanumOnly;
    int editAlphanumLength;
    char editAlphaOnlyFlags;
    char editAlphanumOnlyFlags;
    char editMandatoryFlags;
    char editYesNo;
    String editUsPhoneNum;
    final char[] editUsPhoneFlags = new char[3];
    String editUsSsn;
    final char[] editUsSsnFlags = new char[3];

    // WS-CALCULATION-VARS (WS-DIV-BY / WS-DIVIDEND / WS-REMAINDER are locals of the date edit)
    String currDate;
    final DateEditWorkArea dateEdit = new DateEditWorkArea();

    char dataChangedFlag;
    char inputFlag;
    char returnFlag;
    char pfkFlag;
    char editAcctFlag;
    char editCustFlag;
    final Map<NonKeyFlag, Character> nonKey = new EnumMap<>(NonKeyFlag.class);

    // CICS-OUTPUT-EDIT-VARS
    String custAcctIdX;
    String editDateX;
    String editCurrency92;

    // WS-XREF-RID
    String cardRidCardnum;
    String cardRidCustIdX;
    String cardRidAcctIdX;

    // WS-FILE-READ-FLAGS
    char acctMasterReadFlag;
    char custMasterReadFlag;

    // WS-FILE-ERROR-MESSAGE
    String errorOpname;
    String errorFile;
    String errorResp;
    String errorResp2;

    // ALPHA-VARS-FOR-DATA-EDITING
    String newCreditLimitX;
    String newCashCreditLimitX;
    String newCurrBalX;
    String newCurrCycCreditX;
    String newCurrCycDebitX;

    final AcctUpdateRecord acctUpdate = new AcctUpdateRecord();
    final CustUpdateRecord custUpdate = new CustUpdateRecord();

    String longMsg;
    String infoMsg;
    String returnMsg;

    WsMiscStorage() {
        initialize();
    }

    /** INITIALIZE WS-MISC-STORAGE: alphanumerics to SPACES, numerics to ZERO (VALUE clauses ignored). */
    void initialize() {
        respCd = 0;
        reasCd = 0;
        tranId = Cobol.spaces(4);
        ucTrans = Cobol.spaces(4);
        editVariableName = Cobol.spaces(25);
        editSignedNumber9v2X = Cobol.spaces(15);
        flgSignedNumberEdit = ' ';
        editAlphanumOnly = Cobol.spaces(256);
        editAlphanumLength = 0;
        editAlphaOnlyFlags = ' ';
        editAlphanumOnlyFlags = ' ';
        editMandatoryFlags = ' ';
        editYesNo = ' ';
        editUsPhoneNum = Cobol.spaces(15);
        java.util.Arrays.fill(editUsPhoneFlags, ' ');
        editUsSsn = Cobol.spaces(9);
        java.util.Arrays.fill(editUsSsnFlags, ' ');
        currDate = Cobol.spaces(21);
        dateEdit.initialize();
        dataChangedFlag = ' ';
        inputFlag = ' ';
        returnFlag = ' ';
        pfkFlag = ' ';
        editAcctFlag = ' ';
        editCustFlag = ' ';
        for (NonKeyFlag f : NonKeyFlag.values()) {
            nonKey.put(f, ' ');
        }
        custAcctIdX = Cobol.spaces(11);
        editDateX = Cobol.spaces(10);
        editCurrency92 = Cobol.spaces(15);
        cardRidCardnum = Cobol.spaces(16);
        cardRidCustIdX = Cobol.unsignedDisplay(0, 9);
        cardRidAcctIdX = Cobol.unsignedDisplay(0, 11);
        acctMasterReadFlag = ' ';
        custMasterReadFlag = ' ';
        errorOpname = Cobol.spaces(8);
        errorFile = Cobol.spaces(9);
        errorResp = Cobol.spaces(10);
        errorResp2 = Cobol.spaces(10);
        newCreditLimitX = Cobol.spaces(15);
        newCashCreditLimitX = Cobol.spaces(15);
        newCurrBalX = Cobol.spaces(15);
        newCurrCycCreditX = Cobol.spaces(15);
        newCurrCycDebitX = Cobol.spaces(15);
        acctUpdate.initialize();
        custUpdate.initialize();
        longMsg = Cobol.spaces(500);
        infoMsg = Cobol.spaces(40);
        returnMsg = Cobol.spaces(75);
    }

    /** MOVE LOW-VALUES TO WS-NON-KEY-FLAGS. */
    void nonKeyLowValues() {
        for (NonKeyFlag f : NonKeyFlag.values()) {
            nonKey.put(f, Cobol.LOW_VALUE);
        }
    }

    char flag(NonKeyFlag f) {
        return nonKey.get(f);
    }

    void setFlag(NonKeyFlag f, char value) {
        nonKey.put(f, value);
    }

    boolean isValid(NonKeyFlag f) {
        return f.isValid(flag(f));
    }

    boolean isNotOk(NonKeyFlag f) {
        return f.isNotOk(flag(f));
    }

    boolean isBlank(NonKeyFlag f) {
        return f.isBlank(flag(f));
    }

    /** WS-FILE-ERROR-MESSAGE as one 80-byte item. */
    String fileErrorMessage() {
        return "File Error: " + Cobol.pad(errorOpname, 8) + " on " + Cobol.pad(errorFile, 9) + " returned RESP "
                + Cobol.pad(errorResp, 10) + ",RESP2 " + Cobol.pad(errorResp2, 10) + Cobol.spaces(5);
    }

    // 88-levels
    boolean isReturnMsgOff() {
        return Cobol.isSpaces(returnMsg);
    }

    void setReturnMsgOff() {
        returnMsg = Cobol.spaces(75);
    }

    void setReturnMsg(String value) {
        returnMsg = Cobol.pad(value, 75);
    }

    boolean returnMsgIs(String value) {
        return returnMsg.equals(Cobol.pad(value, 75));
    }

    void setInfoMsg(String value) {
        infoMsg = Cobol.pad(value, 40);
    }

    boolean infoMsgIs(String value) {
        return infoMsg.equals(Cobol.pad(value, 40));
    }

    boolean isNoInfoMessage() {
        return Cobol.isLowValuesOrSpaces(infoMsg);
    }

    boolean isInputError() {
        return inputFlag == '1';
    }

    boolean isNoChangesFound() {
        return dataChangedFlag == '0';
    }

    boolean isFlgAcctfilterIsvalid() {
        return editAcctFlag == '1';
    }

    boolean isFlgAcctfilterNotOk() {
        return editAcctFlag == '0';
    }

    boolean isFlgAcctfilterBlank() {
        return editAcctFlag == ' ';
    }

    boolean isFoundAcctInMaster() {
        return acctMasterReadFlag == '1';
    }

    boolean isFoundCustInMaster() {
        return custMasterReadFlag == '1';
    }

    /** 88 WS-EDIT-US-PHONE-IS-VALID / FLG-EDIT-US-PHONEx-* live on editUsPhoneFlags[0..2]. */
    void setUsPhoneValid() {
        java.util.Arrays.fill(editUsPhoneFlags, FLAG_ISVALID);
    }

    void setUsPhoneInvalid() {
        java.util.Arrays.fill(editUsPhoneFlags, FLAG_NOT_OK);
    }
}

package com.carddemo.programs.cocrdupc;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.numeric;
import static com.carddemo.programs.cocrdupc.Cobol.spaces;

/** 01 WS-MISC-STORAGE, in the state INITIALIZE leaves it (alphanumerics SPACES, numerics ZERO). */
final class MiscStorage {

    /** WS-EDIT-xxx-FLAG PIC X(1) 88-levels shared by every edit flag. */
    enum EditFlag {
        NOT_OK('0'), IS_VALID('1'), BLANK(' ');

        final char value;

        EditFlag(char value) {
            this.value = value;
        }
    }

    // WS-CICS-PROCESSNG-VARS
    int wsRespCd;
    int wsReasCd;
    String wsTranid = spaces(4);
    String wsUcTrans = spaces(4);

    /** WS-INPUT-FLAG: 88 INPUT-OK '0', INPUT-ERROR '1', INPUT-PENDING LOW-VALUES. */
    char wsInputFlag = ' ';
    char wsEditAcctFlag = ' ';
    char wsEditCardFlag = ' ';
    char wsEditCardnameFlag = ' ';
    char wsEditCardstatusFlag = ' ';
    char wsEditCardexpmonFlag = ' ';
    char wsEditCardexpyearFlag = ' ';
    /** WS-RETURN-FLAG: 88 OFF LOW-VALUES, ON '1'. */
    char wsReturnFlag = ' ';
    /** WS-PFK-FLAG: 88 PFK-VALID '0', PFK-INVALID '1'. */
    char wsPfkFlag = ' ';
    String cardNameCheck = spaces(50);
    /** FLG-YES-NO-CHECK: 88 FLG-YES-NO-VALID 'Y','N'. */
    char flgYesNoCheck = ' ';
    /** CARD-MONTH-CHECK X(2) / CARD-MONTH-CHECK-N 9(2): 88 VALID-MONTH 1 THRU 12. */
    String cardMonthCheck = spaces(2);
    /** CARD-YEAR-CHECK X(4) / CARD-YEAR-CHECK-N 9(4): 88 VALID-YEAR 1950 THRU 2099. */
    String cardYearCheck = spaces(4);

    // CICS-OUTPUT-EDIT-VARS
    String cardAcctIdX = spaces(11);
    String cardCvvCdX = spaces(3);
    String cardCardNumX = spaces(16);
    String cardNameEmbossedX = spaces(50);
    String cardStatusX = spaces(1);
    String cardExpiraionDateX = spaces(10);

    // WS-CARD-RID
    String wsCardRidCardnum = spaces(16);
    long wsCardRidAcctId;

    // WS-FILE-ERROR-MESSAGE named fields (FILLER literals survive INITIALIZE)
    String errorOpname = spaces(8);
    String errorFile = spaces(9);
    String errorResp = spaces(10);
    String errorResp2 = spaces(10);

    String wsLongMsg = spaces(500);
    String wsInfoMsg = spaces(InfoMessage.LENGTH);
    String wsReturnMsg = spaces(ReturnMessage.LENGTH);

    boolean isInputOk() { return wsInputFlag == '0'; }
    boolean isInputError() { return wsInputFlag == '1'; }
    void setInputOk() { wsInputFlag = '0'; }
    void setInputError() { wsInputFlag = '1'; }

    boolean isPfkValid() { return wsPfkFlag == '0'; }
    boolean isPfkInvalid() { return wsPfkFlag == '1'; }
    void setPfkValid() { wsPfkFlag = '0'; }
    void setPfkInvalid() { wsPfkFlag = '1'; }

    boolean isFlgYesNoValid() { return flgYesNoCheck == 'Y' || flgYesNoCheck == 'N'; }

    boolean isValidMonth() {
        long n = Cobol.displayValue(cardMonthCheck);
        return n >= 1 && n <= 12;
    }

    boolean isValidYear() {
        long n = Cobol.displayValue(cardYearCheck);
        return n >= 1950 && n <= 2099;
    }

    // WS-INFO-MSG 88-levels
    boolean isNoInfoMessage() {
        return Cobol.isSpaces(wsInfoMsg) || Cobol.isLowValues(wsInfoMsg);
    }

    boolean is(InfoMessage m) { return wsInfoMsg.equals(m.value()); }
    void set(InfoMessage m) { wsInfoMsg = m.value(); }

    // WS-RETURN-MSG 88-levels
    boolean isReturnMsgOff() { return Cobol.isSpaces(wsReturnMsg); }
    void setReturnMsgOff() { wsReturnMsg = spaces(ReturnMessage.LENGTH); }
    boolean is(ReturnMessage m) { return wsReturnMsg.equals(m.value()); }
    void set(ReturnMessage m) { wsReturnMsg = m.value(); }
    void setReturnMsg(String v) { wsReturnMsg = alnum(v, ReturnMessage.LENGTH); }

    /** WS-FILE-ERROR-MESSAGE group (80 bytes). */
    String wsFileErrorMessage() {
        return "File Error: " + errorOpname + " on " + errorFile + " returned RESP " + errorResp
                + ",RESP2 " + errorResp2 + spaces(5);
    }

    /** MOVE S9(09) COMP TO X(10): nine unsigned digits, left-justified. */
    static String respToAlnum(int resp) {
        return alnum(numeric(resp, 9), 10);
    }
}

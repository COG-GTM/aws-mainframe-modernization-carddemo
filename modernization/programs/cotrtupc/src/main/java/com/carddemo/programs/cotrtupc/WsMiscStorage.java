package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.DateEditArea;
import com.carddemo.programs.cotrtupc.support.CobolText;

/** WS-MISC-STORAGE of COTRTUPC (flags, edit work fields and message areas). */
final class WsMiscStorage {

    static final char LOW = CobolText.LOW_VALUE;

    /** WS-INFO-MSG PIC X(40) 88-levels. */
    enum InfoMsg {
        FOUND_TRANTYPE_DATA("Selected transaction type shown above"),
        PROMPT_FOR_SEARCH_KEYS("Enter transaction type to be maintained"),
        PROMPT_CREATE_NEW_RECORD("Press F05 to add. F12 to cancel"),
        PROMPT_DELETE_CONFIRM("Delete this record ? Press F4 to confirm"),
        CONFIRM_DELETE_SUCCESS("Delete successful."),
        PROMPT_FOR_CHANGES("Update transaction type details shown."),
        PROMPT_FOR_NEWDATA("Enter new transaction type details."),
        PROMPT_FOR_CONFIRMATION("Changes validated.Press F5 to save"),
        CONFIRM_UPDATE_SUCCESS("Changes committed to database"),
        INFORM_FAILURE("Changes unsuccessful");

        final String text;

        InfoMsg(String text) {
            this.text = text;
        }
    }

    /** WS-RETURN-MSG PIC X(75) 88-levels. */
    enum ReturnMsg {
        WS_EXIT_MESSAGE("PF03 pressed.Exiting              "),
        WS_INVALID_KEY("Invalid Key pressed. "),
        WS_NAME_MUST_BE_ALPHA("Name can only contain alphabets and spaces"),
        WS_RECORD_NOT_FOUND("No record found for this key in database"),
        NO_SEARCH_CRITERIA_RECEIVED("No input received"),
        NO_CHANGES_DETECTED("No change detected with respect to values fetched."),
        COULD_NOT_LOCK_REC_FOR_UPDATE("Could not lock record for update"),
        DATA_WAS_CHANGED_BEFORE_UPDATE("Record changed by some one else. Please review"),
        WS_UPDATE_WAS_CANCELLED("Update was cancelled"),
        TABLE_UPDATE_FAILED("Update of record failed"),
        RECORD_DELETE_FAILED("Delete of record failed"),
        WS_DELETE_WAS_CANCELLED("Delete was cancelled"),
        WS_INVALID_KEY_PRESSED("Invalid key pressed"),
        CODING_TO_BE_DONE("Looks Good.... so far");

        final String text;

        ReturnMsg(String text) {
            this.text = text;
        }
    }

    // 05 WS-CICS-PROCESSNG-VARS
    int wsRespCd;
    int wsReasCd;
    String wsTranid;
    String wsUctrans;
    // 05 WS-GENERIC-EDITS
    String wsEditVariableName;     // X(25)
    String wsEditAlphanumOnly;     // X(256)
    int wsEditAlphanumLength;      // S9(4) COMP-3
    char wsEditAlphanumOnlyFlags;  // X(1)
    // 05 WS-MISC-VARS
    String wsDispSqlcode;          // ----9
    int wsStringMid;               // 9(3)
    int wsStringLen;               // 9(3)
    String wsStringOut;            // X(40)
    final DateEditArea dateEdit = new DateEditArea();  // COPY CSUTLDWY
    char wsDatachangedFlag;
    char wsInputFlag;
    char wsReturnFlag;
    char wsPfkFlag;
    char wsEditTtypFlag;
    char wsEditDescFlags;          // WS-NON-KEY-FLAGS
    // 05 CICS-OUTPUT-EDIT-VARS
    String wsEditDateX;            // X(10)
    String wsEditCurrency92;       // X(15)
    String wsEditCurrency92F;      // +ZZZ,ZZZ,ZZZ.99
    String wsEditNumeric2;         // 9(02)
    String wsEditAlphanumeric2;    // X(02)
    char wsTrantypeMasterReadFlag;
    // 05 TTYP-UPDATE-RECORD
    String ttupUpdateTtypType;     // X(02)
    String ttupUpdateTtypTypeDesc; // X(50)
    String wsInfoMsg;              // X(40)
    String wsReturnMsg;            // X(75)

    /** INITIALIZE WS-MISC-STORAGE: alphanumerics to SPACES, numerics to ZERO. */
    void initialize() {
        wsRespCd = 0;
        wsReasCd = 0;
        wsTranid = CobolText.spaces(4);
        wsUctrans = CobolText.spaces(4);
        wsEditVariableName = CobolText.spaces(25);
        wsEditAlphanumOnly = CobolText.spaces(256);
        wsEditAlphanumLength = 0;
        wsEditAlphanumOnlyFlags = ' ';
        wsDispSqlcode = CobolText.editMinus4Z9(0);
        wsStringMid = 0;
        wsStringLen = 0;
        wsStringOut = CobolText.spaces(40);
        dateEdit.initialize();
        wsDatachangedFlag = ' ';
        wsInputFlag = ' ';
        wsReturnFlag = ' ';
        wsPfkFlag = ' ';
        wsEditTtypFlag = ' ';
        wsEditDescFlags = ' ';
        wsEditDateX = CobolText.spaces(10);
        wsEditCurrency92 = CobolText.spaces(15);
        wsEditCurrency92F = "              0";
        wsEditNumeric2 = "00";
        wsEditAlphanumeric2 = CobolText.spaces(2);
        wsTrantypeMasterReadFlag = ' ';
        ttupUpdateTtypType = CobolText.spaces(2);
        ttupUpdateTtypTypeDesc = CobolText.spaces(50);
        wsInfoMsg = CobolText.spaces(40);
        wsReturnMsg = CobolText.spaces(75);
    }

    // WS-DATACHANGED-FLAG
    boolean isNoChangesFound() { return wsDatachangedFlag == '0'; }
    void setNoChangesFound() { wsDatachangedFlag = '0'; }
    boolean isChangeHasOccurred() { return wsDatachangedFlag == '1'; }
    void setChangeHasOccurred() { wsDatachangedFlag = '1'; }

    // WS-INPUT-FLAG
    boolean isInputOk() { return wsInputFlag == '0'; }
    void setInputOk() { wsInputFlag = '0'; }
    boolean isInputError() { return wsInputFlag == '1'; }
    void setInputError() { wsInputFlag = '1'; }
    boolean isInputPending() { return wsInputFlag == LOW; }

    // WS-PFK-FLAG
    boolean isPfkValid() { return wsPfkFlag == '0'; }
    void setPfkValid() { wsPfkFlag = '0'; }
    boolean isPfkInvalid() { return wsPfkFlag == '1'; }
    void setPfkInvalid() { wsPfkFlag = '1'; }

    // WS-EDIT-TTYP-FLAG
    boolean isTranfilterIsValid() { return wsEditTtypFlag == LOW; }
    void setTranfilterIsValid() { wsEditTtypFlag = LOW; }
    boolean isTranfilterNotOk() { return wsEditTtypFlag == '0'; }
    void setTranfilterNotOk() { wsEditTtypFlag = '0'; }
    boolean isTranfilterBlank() { return wsEditTtypFlag == 'B'; }

    // WS-EDIT-DESC-FLAGS
    boolean isDescriptionIsValid() { return wsEditDescFlags == LOW; }
    boolean isDescriptionNotOk() { return wsEditDescFlags == '0'; }
    boolean isDescriptionBlank() { return wsEditDescFlags == 'B'; }

    // WS-EDIT-ALPHANUM-ONLY-FLAGS
    void setAlphanumIsValid() { wsEditAlphanumOnlyFlags = LOW; }
    void setAlphanumNotOk() { wsEditAlphanumOnlyFlags = '0'; }
    void setAlphanumBlank() { wsEditAlphanumOnlyFlags = 'B'; }

    // WS-TRANTYPE-MASTER-READ-FLAG
    boolean isFoundTrantypeInTable() { return wsTrantypeMasterReadFlag == '1'; }
    void setFoundTrantypeInTable() { wsTrantypeMasterReadFlag = '1'; }

    // WS-INFO-MSG
    boolean isNoInfoMessage() { return CobolText.isSpaces(wsInfoMsg) || CobolText.isLowValues(wsInfoMsg); }
    void setNoInfoMessage() { wsInfoMsg = CobolText.spaces(40); }
    void setInfo(InfoMsg m) { wsInfoMsg = CobolText.pic(m.text, 40); }
    boolean is(InfoMsg m) { return wsInfoMsg.equals(CobolText.pic(m.text, 40)); }

    // WS-RETURN-MSG
    boolean isReturnMsgOff() { return CobolText.isSpaces(wsReturnMsg); }
    void setReturnMsgOff() { wsReturnMsg = CobolText.spaces(75); }
    void setReturn(ReturnMsg m) { wsReturnMsg = CobolText.pic(m.text, 75); }
    boolean is(ReturnMsg m) { return wsReturnMsg.equals(CobolText.pic(m.text, 75)); }
}

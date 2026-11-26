package com.carddemo.programs.cotrtlic;

import com.carddemo.programs.cotrtlic.copybook.Db2CommonVars;
import com.carddemo.programs.cotrtlic.support.Cob;
import java.util.Arrays;

/** 01 WS-MISC-STORAGE (includes CSDB2RWY). Flags are single characters so 88-level values match exactly. */
final class MiscStorage {

    static final int ROWS = 7;

    // 88 WS-INFO-MSG values (PIC X(45))
    static final String INFORM_REC_ACTIONS = "Type U to update, D to delete any record";
    static final String INFORM_DELETE = "Delete HIGHLIGHTED row ? Press F10 to confirm";
    static final String INFORM_UPDATE = "Update HIGHLIGHTED row. Press F10 to save";
    static final String INFORM_DELETE_SUCCESS = "HIGHLIGHTED row deleted.Hit Enter to continue";
    static final String INFORM_UPDATE_SUCCESS = "HIGHLIGHTED row was updated";
    // 88 WS-RETURN-MSG values (PIC X(75))
    static final String EXIT_MESSAGE = "PF03 pressed. Exiting";
    static final String MESG_NO_RECORDS_FOUND = "No records found for this search condition.";
    static final String MESG_NO_MORE_RECORDS = "No more pages for these search conditions";
    static final String MESG_MORE_THAN_1_ACTION = "Please select only 1 action";
    static final String MESG_INVALID_ACTION_CODE = "Action code selected is invalid";
    static final String MESG_NO_CHANGES_DETECTED = "No change detected with respect to database values.";

    int respCd;
    int reasCd;
    String tranid;
    char inputFlag;
    char editTypeFlag;
    char editDescFlag;
    char typefilterChanged;
    char descfilterChanged;
    final char[] rowRecordsChanged = new char[ROWS];
    char deleteStatus;
    char updateStatus;
    char rowSelectionChanged;
    char badSelectionAction;
    char arrayDescriptionFlgs;
    char datachangedFlag;
    /** WS-EDIT-VARIABLE-NAME PIC X(25). */
    String editVariableName;
    /** WS-EDIT-ALPHANUM-ONLY PIC X(256). */
    String editAlphanumOnly;
    /** WS-EDIT-ALPHANUM-LENGTH PIC S9(4) COMP-3. */
    int editAlphanumLength;
    char editAlphanumOnlyFlags;
    /** WS-RECORDS-COUNT PIC S9(4) COMP-3. */
    int recordsCount;
    /** WS-ROW-TR-CODE-IN PIC X(02) OCCURS 7. */
    final String[] rowTrCodeIn = new String[ROWS];
    /** WS-ROW-TR-DESC-IN PIC X(50) OCCURS 7. */
    final String[] rowTrDescIn = new String[ROWS];
    int editSelectCounter;
    /** WS-EDIT-SELECT-FLAGS PIC X(7) / WS-EDIT-SELECT PIC X(1) OCCURS 7. */
    final char[] editSelect = new char[ROWS];
    /** WS-ROW-TRTSELECT-ERROR PIC X(1) OCCURS 7. */
    final char[] rowTrtselectError = new char[ROWS];
    int i;
    int iSelected;
    int actionsRequested;
    int deletesRequested;
    int updatesRequested;
    int noActionsSelected;
    int validActionsSelected;
    String tranTypeCdX;
    char protectSelectRows;
    /** WS-LONG-MSG PIC X(800). */
    String longMsg;
    /** WS-INFO-MSG PIC X(45). */
    String infoMsg;
    /** WS-RETURN-MSG PIC X(75). */
    String returnMsg;
    char pfkFlag;
    /** WS-STRING-MID PIC 9(3). */
    int stringMid;
    /** WS-STRING-LEN PIC 9(3). */
    int stringLen;
    /** WS-STRING-OUT PIC X(45). */
    String stringOut;
    String startKey;
    String typeCdFilter;
    /** WS-TYPE-DESC-FILTER PIC X(52). */
    String typeDescFilter;
    final String[] typeCdDeleteKey = new String[ROWS];
    final Db2CommonVars db2 = new Db2CommonVars();
    String inTypeCd;
    String inTypeDesc;
    /** WS-ROW-NUMBER PIC S9(4) COMP. */
    int rowNumber;
    char recordsToProcessFlag;

    MiscStorage() {
        initialize();
    }

    /** INITIALIZE WS-MISC-STORAGE: alphanumerics to SPACES, numerics to ZERO (VALUE clauses ignored). */
    void initialize() {
        respCd = 0;
        reasCd = 0;
        tranid = Cob.spaces(4);
        inputFlag = ' ';
        editTypeFlag = ' ';
        editDescFlag = ' ';
        typefilterChanged = ' ';
        descfilterChanged = ' ';
        Arrays.fill(rowRecordsChanged, ' ');
        deleteStatus = ' ';
        updateStatus = ' ';
        rowSelectionChanged = ' ';
        badSelectionAction = ' ';
        arrayDescriptionFlgs = ' ';
        datachangedFlag = ' ';
        editVariableName = Cob.spaces(25);
        editAlphanumOnly = Cob.spaces(256);
        editAlphanumLength = 0;
        editAlphanumOnlyFlags = ' ';
        recordsCount = 0;
        Arrays.fill(rowTrCodeIn, Cob.spaces(2));
        Arrays.fill(rowTrDescIn, Cob.spaces(50));
        editSelectCounter = 0;
        Arrays.fill(editSelect, ' ');
        Arrays.fill(rowTrtselectError, ' ');
        i = 0;
        iSelected = 0;
        actionsRequested = 0;
        deletesRequested = 0;
        updatesRequested = 0;
        noActionsSelected = 0;
        validActionsSelected = 0;
        tranTypeCdX = Cob.spaces(2);
        protectSelectRows = ' ';
        longMsg = Cob.spaces(800);
        infoMsg = Cob.spaces(45);
        returnMsg = Cob.spaces(75);
        pfkFlag = ' ';
        stringMid = 0;
        stringLen = 0;
        stringOut = Cob.spaces(45);
        startKey = Cob.spaces(2);
        typeCdFilter = Cob.spaces(2);
        typeDescFilter = Cob.spaces(52);
        Arrays.fill(typeCdDeleteKey, Cob.spaces(2));
        db2.initialize();
        inTypeCd = Cob.spaces(2);
        inTypeDesc = Cob.spaces(50);
        rowNumber = 0;
        recordsToProcessFlag = ' ';
    }

    boolean isInputOk() {
        return inputFlag == '0' || inputFlag == ' ' || inputFlag == Cob.LOW_VALUE;
    }

    boolean isInputError() {
        return inputFlag == '1';
    }

    boolean isReturnMsgOff() {
        return Cob.isSpaces(returnMsg);
    }

    void setReturnMsg(String text) {
        returnMsg = Cob.move(text, 75);
    }

    boolean isNoInfoMessage() {
        return Cob.isSpaces(infoMsg) || Cob.isLowValues(infoMsg);
    }

    void setInfoMsg(String text) {
        infoMsg = Cob.move(text, 45);
    }

    boolean isMoreThan1Action() {
        return actionsRequested >= 2 && actionsRequested <= 7;
    }

    boolean isOnly1Action() {
        return actionsRequested == 1;
    }

    boolean isOnly1ValidAction() {
        return validActionsSelected == 1;
    }

    boolean isBadActionsSelectedNo() {
        return badSelectionAction == Cob.LOW_VALUE;
    }

    boolean isRowDescriptionIsValid() {
        return arrayDescriptionFlgs == Cob.LOW_VALUE || arrayDescriptionFlgs == ' ';
    }
}

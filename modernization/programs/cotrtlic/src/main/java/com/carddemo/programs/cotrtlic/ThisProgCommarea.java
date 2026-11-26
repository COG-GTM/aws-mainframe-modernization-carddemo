package com.carddemo.programs.cotrtlic;

import com.carddemo.programs.cotrtlic.support.Cob;

/** 01 WS-THIS-PROGCOMMAREA: COTRTLIC's private pseudo-conversational state (after CARDDEMO-COMMAREA). */
public final class ThisProgCommarea {

    public static final int ROWS = 7;

    /** WS-CA-TYPE-CD PIC X(02) (REDEFINES WS-CA-TYPE-CD-N PIC 9(02)). */
    public String caTypeCd;
    /** WS-CA-TYPE-DESC PIC X(50). */
    public String caTypeDesc;
    /** WS-CA-ROW-TR-CODE-OUT PIC X(02) OCCURS 7 (0-based here). */
    public final String[] caRowTrCodeOut = new String[ROWS];
    /** WS-CA-ROW-TR-DESC-OUT PIC X(50) OCCURS 7 (0-based here). */
    public final String[] caRowTrDescOut = new String[ROWS];
    /** WS-CA-ROW-SELECTED PIC S9(4) COMP. */
    public int caRowSelected;
    /** WS-CA-LAST-TR-CODE PIC X(02). */
    public String caLastTrCode;
    /** WS-CA-FIRST-TR-CODE PIC X(02). */
    public String caFirstTrCode;
    /** WS-CA-SCREEN-NUM PIC 9(1): 88 CA-FIRST-PAGE VALUE 1. */
    public int caScreenNum;
    /** WS-CA-LAST-PAGE-DISPLAYED PIC 9(1): 88 SHOWN 0, 88 NOT-SHOWN 9. */
    public int caLastPageDisplayed;
    /** WS-CA-NEXT-PAGE-IND PIC X(1): 88 NOT-EXISTS LOW-VALUES, 88 EXISTS 'Y'. */
    public char caNextPageInd;
    /** WS-CA-DELETE-FLAG PIC X: 88 NOT-REQUESTED/SUCCEEDED LOW-VALUES, 88 REQUESTED 'Y'. */
    public char caDeleteFlag;
    /** WS-CA-UPDATE-FLAG PIC X: 88 NOT-REQUESTED/SUCCEEDED LOW-VALUES, 88 REQUESTED 'Y'. */
    public char caUpdateFlag;

    public ThisProgCommarea() {
        initialize();
    }

    /** INITIALIZE WS-THIS-PROGCOMMAREA. */
    public void initialize() {
        caTypeCd = Cob.spaces(2);
        caTypeDesc = Cob.spaces(50);
        for (int i = 0; i < ROWS; i++) {
            caRowTrCodeOut[i] = Cob.spaces(2);
            caRowTrDescOut[i] = Cob.spaces(50);
        }
        caRowSelected = 0;
        initializePagingVariables();
        caDeleteFlag = ' ';
        caUpdateFlag = ' ';
    }

    /** INITIALIZE WS-CA-PAGING-VARIABLES. */
    public void initializePagingVariables() {
        caLastTrCode = Cob.spaces(2);
        caFirstTrCode = Cob.spaces(2);
        caScreenNum = 0;
        caLastPageDisplayed = 0;
        caNextPageInd = ' ';
    }

    /** MOVE LOW-VALUES TO WS-CA-ALL-ROWS-OUT. */
    public void clearAllRowsOut() {
        for (int i = 0; i < ROWS; i++) {
            caRowTrCodeOut[i] = Cob.lowValues(2);
            caRowTrDescOut[i] = Cob.lowValues(50);
        }
    }

    /** WS-CA-EACH-ROW-OUT(I) EQUAL LOW-VALUES (1-based). */
    public boolean eachRowOutIsLowValues(int i) {
        return Cob.isLowValues(caRowTrCodeOut[i - 1]) && Cob.isLowValues(caRowTrDescOut[i - 1]);
    }

    public boolean isFirstPage() {
        return caScreenNum == 1;
    }

    public void setFirstPage() {
        caScreenNum = 1;
    }

    public boolean isLastPageShown() {
        return caLastPageDisplayed == 0;
    }

    public boolean isLastPageNotShown() {
        return caLastPageDisplayed == 9;
    }

    public void setLastPageShown() {
        caLastPageDisplayed = 0;
    }

    public void setLastPageNotShown() {
        caLastPageDisplayed = 9;
    }

    public boolean isNextPageExists() {
        return caNextPageInd == 'Y';
    }

    public boolean isNextPageNotExists() {
        return caNextPageInd == Cob.LOW_VALUE;
    }

    public boolean isDeleteRequested() {
        return caDeleteFlag == 'Y';
    }

    /** 88 CA-DELETE-SUCCEEDED VALUE LOW-VALUES (same value as CA-DELETE-NOT-REQUESTED). */
    public boolean isDeleteSucceeded() {
        return caDeleteFlag == Cob.LOW_VALUE;
    }

    public boolean isUpdateRequested() {
        return caUpdateFlag == 'Y';
    }

    /** 88 CA-UPDATE-SUCCEEDED VALUE LOW-VALUES (same value as CA-UPDATE-NOT-REQUESTED). */
    public boolean isUpdateSucceeded() {
        return caUpdateFlag == Cob.LOW_VALUE;
    }

    public ThisProgCommarea copy() {
        ThisProgCommarea c = new ThisProgCommarea();
        c.caTypeCd = caTypeCd;
        c.caTypeDesc = caTypeDesc;
        System.arraycopy(caRowTrCodeOut, 0, c.caRowTrCodeOut, 0, ROWS);
        System.arraycopy(caRowTrDescOut, 0, c.caRowTrDescOut, 0, ROWS);
        c.caRowSelected = caRowSelected;
        c.caLastTrCode = caLastTrCode;
        c.caFirstTrCode = caFirstTrCode;
        c.caScreenNum = caScreenNum;
        c.caLastPageDisplayed = caLastPageDisplayed;
        c.caNextPageInd = caNextPageInd;
        c.caDeleteFlag = caDeleteFlag;
        c.caUpdateFlag = caUpdateFlag;
        return c;
    }
}

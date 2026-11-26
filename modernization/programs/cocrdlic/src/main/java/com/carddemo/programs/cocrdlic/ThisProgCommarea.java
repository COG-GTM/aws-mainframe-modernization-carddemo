package com.carddemo.programs.cocrdlic;

import com.carddemo.programs.cocrdlic.copybook.Pic;

/**
 * WS-THIS-PROGCOMMAREA (254 bytes): COCRDLIC's private paging state, carried after CARDDEMO-COMMAREA
 * in the 2000-byte COMMAREA returned to CICS.
 */
public final class ThisProgCommarea {
    public static final int LENGTH = 254;
    public static final int ROW_LENGTH = 28;
    public static final char NEXT_PAGE_EXISTS = 'Y';

    String lastCardNum;       // WS-CA-LAST-CARD-NUM       X(16)
    long lastCardAcctId;      // WS-CA-LAST-CARD-ACCT-ID   9(11)
    String firstCardNum;      // WS-CA-FIRST-CARD-NUM      X(16)
    long firstCardAcctId;     // WS-CA-FIRST-CARD-ACCT-ID  9(11)
    int screenNum;            // WS-CA-SCREEN-NUM          9(1)  88 CA-FIRST-PAGE 1
    int lastPageDisplayed;    // WS-CA-LAST-PAGE-DISPLAYED 9(1)  88 SHOWN 0 / NOT-SHOWN 9
    char nextPageInd;         // WS-CA-NEXT-PAGE-IND       X(1)  88 NOT-EXISTS LOW-VALUES / EXISTS 'Y'
    char returnFlag;          // WS-RETURN-FLAG            X(1)  88 OFF LOW-VALUES / ON '1'
    final String[] rowAcctNo = new String[7];     // WS-ROW-ACCTNO     X(11)
    final String[] rowCardNum = new String[7];    // WS-ROW-CARD-NUM   X(16)
    final char[] rowCardStatus = new char[7];     // WS-ROW-CARD-STATUS X(1)

    public ThisProgCommarea() {
        initialize();
    }

    /** INITIALIZE WS-THIS-PROGCOMMAREA. */
    public void initialize() {
        lastCardNum = Pic.spaces(16);
        lastCardAcctId = 0;
        firstCardNum = Pic.spaces(16);
        firstCardAcctId = 0;
        screenNum = 0;
        lastPageDisplayed = 0;
        nextPageInd = ' ';
        returnFlag = ' ';
        for (int i = 0; i < 7; i++) {
            rowAcctNo[i] = Pic.spaces(11);
            rowCardNum[i] = Pic.spaces(16);
            rowCardStatus[i] = ' ';
        }
    }

    /** MOVE LOW-VALUES TO WS-ALL-ROWS. */
    void clearRowsToLowValues() {
        for (int i = 0; i < 7; i++) {
            rowAcctNo[i] = Pic.lowValues(11);
            rowCardNum[i] = Pic.lowValues(16);
            rowCardStatus[i] = Pic.LOW_VALUE;
        }
    }

    /** WS-EACH-CARD(row) EQUAL LOW-VALUES. */
    boolean isRowLowValues(int row) {
        return eachCard(row).equals(Pic.lowValues(ROW_LENGTH));
    }

    String eachCard(int row) {
        int i = row - 1;
        return rowAcctNo[i] + rowCardNum[i] + rowCardStatus[i];
    }

    public String toFixedWidth() {
        StringBuilder sb = new StringBuilder(LENGTH);
        sb.append(lastCardNum).append(Pic.nine(lastCardAcctId, 11))
                .append(firstCardNum).append(Pic.nine(firstCardAcctId, 11))
                .append(Pic.nine(screenNum, 1)).append(Pic.nine(lastPageDisplayed, 1))
                .append(nextPageInd).append(returnFlag);
        for (int r = 1; r <= 7; r++) {
            sb.append(eachCard(r));
        }
        return sb.toString();
    }

    public static ThisProgCommarea fromFixedWidth(String image) {
        String s = Pic.x(image, LENGTH);
        ThisProgCommarea c = new ThisProgCommarea();
        c.lastCardNum = s.substring(0, 16);
        c.lastCardAcctId = Pic.toUnsigned(s.substring(16, 27), 11);
        c.firstCardNum = s.substring(27, 43);
        c.firstCardAcctId = Pic.toUnsigned(s.substring(43, 54), 11);
        c.screenNum = (int) Pic.toUnsigned(s.substring(54, 55), 1);
        c.lastPageDisplayed = (int) Pic.toUnsigned(s.substring(55, 56), 1);
        c.nextPageInd = s.charAt(56);
        c.returnFlag = s.charAt(57);
        for (int i = 0; i < 7; i++) {
            int o = 58 + i * ROW_LENGTH;
            c.rowAcctNo[i] = s.substring(o, o + 11);
            c.rowCardNum[i] = s.substring(o + 11, o + 27);
            c.rowCardStatus[i] = s.charAt(o + 27);
        }
        return c;
    }

    public String lastCardNum() { return lastCardNum; }
    public long lastCardAcctId() { return lastCardAcctId; }
    public String firstCardNum() { return firstCardNum; }
    public long firstCardAcctId() { return firstCardAcctId; }
    public int screenNum() { return screenNum; }
    public boolean isFirstPage() { return screenNum == 1; }
    public boolean isLastPageShown() { return lastPageDisplayed == 0; }
    public boolean isLastPageNotShown() { return lastPageDisplayed == 9; }
    public boolean isNextPageExists() { return nextPageInd == NEXT_PAGE_EXISTS; }
    public boolean isNextPageNotExists() { return nextPageInd == Pic.LOW_VALUE; }
    public String rowAcctNo(int row) { return rowAcctNo[row - 1]; }
    public String rowCardNum(int row) { return rowCardNum[row - 1]; }
    public char rowCardStatus(int row) { return rowCardStatus[row - 1]; }

    public void setFirstCardKey(String cardNum, long acctId) {
        firstCardNum = Pic.x(cardNum, 16);
        firstCardAcctId = Pic.truncUnsigned(acctId, 11);
    }

    public void setLastCardKey(String cardNum, long acctId) {
        lastCardNum = Pic.x(cardNum, 16);
        lastCardAcctId = Pic.truncUnsigned(acctId, 11);
    }

    public void setScreenNum(int n) { screenNum = (int) Pic.truncUnsigned(n, 1); }
    public void setLastPageShown() { lastPageDisplayed = 0; }
    public void setLastPageNotShown() { lastPageDisplayed = 9; }
    public void setNextPageExists() { nextPageInd = NEXT_PAGE_EXISTS; }
    public void setNextPageNotExists() { nextPageInd = Pic.LOW_VALUE; }

    public void setRow(int row, String acctNo, String cardNum, char status) {
        rowAcctNo[row - 1] = Pic.x(acctNo, 11);
        rowCardNum[row - 1] = Pic.x(cardNum, 16);
        rowCardStatus[row - 1] = status;
    }
}

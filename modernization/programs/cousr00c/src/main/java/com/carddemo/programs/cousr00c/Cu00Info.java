package com.carddemo.programs.cousr00c;

/**
 * CDEMO-CU00-INFO - COUSR00C's own extension appended to CARDDEMO-COMMAREA (34 bytes).
 */
public class Cu00Info {

    public static final int LENGTH = 34;

    private String usridFirst = Cobol.lowValues(8);   // CDEMO-CU00-USRID-FIRST   PIC X(08)
    private String usridLast = Cobol.lowValues(8);    // CDEMO-CU00-USRID-LAST    PIC X(08)
    private long pageNum;                             // CDEMO-CU00-PAGE-NUM      PIC 9(08)
    private char nextPageFlg = 'N';                   // CDEMO-CU00-NEXT-PAGE-FLG PIC X(01) VALUE 'N'
    private char usrSelFlg = Cobol.LOW_VALUE;         // CDEMO-CU00-USR-SEL-FLG   PIC X(01)
    private String usrSelected = Cobol.lowValues(8);  // CDEMO-CU00-USR-SELECTED  PIC X(08)

    public static Cu00Info fromFixedWidth(String raw) {
        String s = Cobol.alnum(raw, LENGTH);
        Cu00Info c = new Cu00Info();
        c.usridFirst = s.substring(0, 8);
        c.usridLast = s.substring(8, 16);
        c.pageNum = Cobol.parseNumericDisplay(s.substring(16, 24));
        c.nextPageFlg = s.charAt(24);
        c.usrSelFlg = s.charAt(25);
        c.usrSelected = s.substring(26, 34);
        return c;
    }

    public String toFixedWidth() {
        return usridFirst + usridLast + Cobol.numericDisplay(pageNum, 8) + nextPageFlg + usrSelFlg + usrSelected;
    }

    /** 88 NEXT-PAGE-YES VALUE 'Y'. */
    public boolean isNextPageYes() {
        return nextPageFlg == 'Y';
    }

    /** 88 NEXT-PAGE-NO VALUE 'N'. */
    public boolean isNextPageNo() {
        return nextPageFlg == 'N';
    }

    public void setNextPageYes() {
        nextPageFlg = 'Y';
    }

    public void setNextPageNo() {
        nextPageFlg = 'N';
    }

    public String getUsridFirst() {
        return usridFirst;
    }

    public void setUsridFirst(String v) {
        usridFirst = Cobol.alnum(v, 8);
    }

    public String getUsridLast() {
        return usridLast;
    }

    public void setUsridLast(String v) {
        usridLast = Cobol.alnum(v, 8);
    }

    public long getPageNum() {
        return pageNum;
    }

    /** Store into PIC 9(08): high-order digits are truncated. */
    public void setPageNum(long v) {
        pageNum = Cobol.truncateUnsigned(v, 8);
    }

    public char getNextPageFlg() {
        return nextPageFlg;
    }

    public void setNextPageFlg(char v) {
        nextPageFlg = v;
    }

    public char getUsrSelFlg() {
        return usrSelFlg;
    }

    public void setUsrSelFlg(char v) {
        usrSelFlg = v;
    }

    public String getUsrSelected() {
        return usrSelected;
    }

    public void setUsrSelected(String v) {
        usrSelected = Cobol.alnum(v, 8);
    }
}

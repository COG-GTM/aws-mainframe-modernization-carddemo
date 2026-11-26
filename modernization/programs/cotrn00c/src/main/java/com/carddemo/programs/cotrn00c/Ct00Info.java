package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.CobolText;

/** CDEMO-CT00-INFO - COTRN00C's 58-byte extension appended to CARDDEMO-COMMAREA. */
public final class Ct00Info {

    public static final int LENGTH = 58;

    private String trnidFirst;   // CDEMO-CT00-TRNID-FIRST   PIC X(16)
    private String trnidLast;    // CDEMO-CT00-TRNID-LAST    PIC X(16)
    private long pageNum;        // CDEMO-CT00-PAGE-NUM      PIC 9(08)
    private String nextPageFlg;  // CDEMO-CT00-NEXT-PAGE-FLG PIC X(01) VALUE 'N'
    private String trnSelFlg;    // CDEMO-CT00-TRN-SEL-FLG   PIC X(01)
    private String trnSelected;  // CDEMO-CT00-TRN-SELECTED  PIC X(16)

    static Ct00Info decode(String data, int offset) {
        Ct00Info c = new Ct00Info();
        int p = offset;
        c.trnidFirst = data.substring(p, p += 16);
        c.trnidLast = data.substring(p, p += 16);
        c.pageNum = CobolText.parseUnsigned(data.substring(p, p += 8));
        c.nextPageFlg = data.substring(p, p += 1);
        c.trnSelFlg = data.substring(p, p += 1);
        c.trnSelected = data.substring(p, p + 16);
        return c;
    }

    String encode() {
        return trnidFirst + trnidLast + CobolText.unsignedDigits(pageNum, 8) + nextPageFlg + trnSelFlg
                + trnSelected;
    }

    /** 88 NEXT-PAGE-YES VALUE 'Y'. */
    public boolean isNextPageYes() {
        return "Y".equals(nextPageFlg);
    }

    /** 88 NEXT-PAGE-NO VALUE 'N'. */
    public boolean isNextPageNo() {
        return "N".equals(nextPageFlg);
    }

    public void setNextPageYes() {
        nextPageFlg = "Y";
    }

    public void setNextPageNo() {
        nextPageFlg = "N";
    }

    public String trnidFirst() {
        return trnidFirst;
    }

    public void setTrnidFirst(String v) {
        trnidFirst = CobolText.fit(v, 16);
    }

    public String trnidLast() {
        return trnidLast;
    }

    public void setTrnidLast(String v) {
        trnidLast = CobolText.fit(v, 16);
    }

    public long pageNum() {
        return pageNum;
    }

    /** MOVE / COMPUTE into PIC 9(08): unsigned, high-order truncation. */
    public void setPageNum(long v) {
        pageNum = Long.parseLong(CobolText.unsignedDigits(v, 8));
    }

    public String nextPageFlg() {
        return nextPageFlg;
    }

    public void setNextPageFlg(String v) {
        nextPageFlg = CobolText.fit(v, 1);
    }

    public String trnSelFlg() {
        return trnSelFlg;
    }

    public void setTrnSelFlg(String v) {
        trnSelFlg = CobolText.fit(v, 1);
    }

    public String trnSelected() {
        return trnSelected;
    }

    public void setTrnSelected(String v) {
        trnSelected = CobolText.fit(v, 16);
    }
}

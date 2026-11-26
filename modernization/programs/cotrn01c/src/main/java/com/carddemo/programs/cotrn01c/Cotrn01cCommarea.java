package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrn01c.copybook.Cobol;

/**
 * COTRN01C's CARDDEMO-COMMAREA: COCOM01Y (160 bytes) followed by the program-local
 * 05 CDEMO-CT01-INFO group (58 bytes) declared right after COPY COCOM01Y. Total 218 bytes.
 */
public final class Cotrn01cCommarea extends CardDemoCommarea {

    public static final int LENGTH = CardDemoCommarea.LENGTH + 58;

    private static final int CT01_TRNID_FIRST = 160;   // X(16)
    private static final int CT01_TRNID_LAST = 176;    // X(16)
    private static final int CT01_PAGE_NUM = 192;      // 9(08)
    private static final int CT01_NEXT_PAGE_FLG = 200; // X(01) VALUE 'N'
    private static final int CT01_TRN_SEL_FLG = 201;   // X(01)
    private static final int CT01_TRN_SELECTED = 202;  // X(16)

    /** Working-storage initial state (VALUE 'N' on CDEMO-CT01-NEXT-PAGE-FLG). */
    public Cotrn01cCommarea() {
        super(LENGTH);
        put(CT01_PAGE_NUM, 8, "00000000");
        put(CT01_NEXT_PAGE_FLG, 1, "N");
    }

    /** MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA. */
    public static Cotrn01cCommarea fromDfhcommarea(String dfhcommarea) {
        Cotrn01cCommarea c = new Cotrn01cCommarea();
        c.moveFrom(dfhcommarea);
        return c;
    }

    public String ct01TrnidFirst() { return get(CT01_TRNID_FIRST, 16); }
    public void ct01TrnidFirst(String v) { put(CT01_TRNID_FIRST, 16, v); }
    public String ct01TrnidLast() { return get(CT01_TRNID_LAST, 16); }
    public void ct01TrnidLast(String v) { put(CT01_TRNID_LAST, 16, v); }
    public long ct01PageNum() { return Cobol.parseUnsigned(get(CT01_PAGE_NUM, 8)); }
    public void ct01PageNum(long v) { put(CT01_PAGE_NUM, 8, Cobol.moveUnsignedNumeric(v, 8)); }
    public String ct01NextPageFlg() { return get(CT01_NEXT_PAGE_FLG, 1); }
    public void ct01NextPageFlg(String v) { put(CT01_NEXT_PAGE_FLG, 1, v); }
    /** 88 NEXT-PAGE-YES VALUE 'Y'. */
    public boolean isNextPageYes() { return buf[CT01_NEXT_PAGE_FLG] == 'Y'; }
    /** 88 NEXT-PAGE-NO VALUE 'N'. */
    public boolean isNextPageNo() { return buf[CT01_NEXT_PAGE_FLG] == 'N'; }
    public String ct01TrnSelFlg() { return get(CT01_TRN_SEL_FLG, 1); }
    public void ct01TrnSelFlg(String v) { put(CT01_TRN_SEL_FLG, 1, v); }
    public String ct01TrnSelected() { return get(CT01_TRN_SELECTED, 16); }
    public void ct01TrnSelected(String v) { put(CT01_TRN_SELECTED, 16, v); }
}

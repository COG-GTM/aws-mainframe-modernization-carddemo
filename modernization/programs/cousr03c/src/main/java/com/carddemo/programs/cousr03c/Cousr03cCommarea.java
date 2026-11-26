package com.carddemo.programs.cousr03c;

import com.carddemo.programs.cousr03c.copybook.CarddemoCommarea;

/**
 * COUSR03C's CARDDEMO-COMMAREA: COCOM01Y (160 bytes) followed by the program-local
 * CDEMO-CU03-INFO group (34 bytes) declared under the same 01 level in WORKING-STORAGE.
 */
public final class Cousr03cCommarea extends CarddemoCommarea {

    public static final int LENGTH = CarddemoCommarea.LENGTH + 34;

    private static final int CU03_USRID_FIRST = 160;    // X(08)
    private static final int CU03_USRID_LAST = 168;     // X(08)
    private static final int CU03_PAGE_NUM = 176;       // 9(08)
    private static final int CU03_NEXT_PAGE_FLG = 184;  // X(01) VALUE 'N'
    private static final int CU03_USR_SEL_FLG = 185;    // X(01)
    private static final int CU03_USR_SELECTED = 186;   // X(08)

    /** Fresh WORKING-STORAGE: LOW-VALUES except CDEMO-CU03-NEXT-PAGE-FLG VALUE 'N'. */
    public Cousr03cCommarea() {
        super(LENGTH);
        fill(LOW_VALUE);
        setNextPageNo();
    }

    /** MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA. */
    public static Cousr03cCommarea fromDfhcommarea(byte[] dfhcommarea) {
        Cousr03cCommarea c = new Cousr03cCommarea();
        c.moveFrom(dfhcommarea);
        return c;
    }

    public String cu03UsridFirst() { return getX(CU03_USRID_FIRST, 8); }
    public void setCu03UsridFirst(String v) { setX(CU03_USRID_FIRST, 8, v); }
    public String cu03UsridLast() { return getX(CU03_USRID_LAST, 8); }
    public void setCu03UsridLast(String v) { setX(CU03_USRID_LAST, 8, v); }
    public long cu03PageNum() { return getNum(CU03_PAGE_NUM, 8); }
    public void setCu03PageNum(long v) { setNum(CU03_PAGE_NUM, 8, v); }
    public String cu03NextPageFlg() { return getX(CU03_NEXT_PAGE_FLG, 1); }
    public boolean isNextPageYes() { return getByte(CU03_NEXT_PAGE_FLG) == 'Y'; }
    public boolean isNextPageNo() { return getByte(CU03_NEXT_PAGE_FLG) == 'N'; }
    public void setNextPageYes() { setByte(CU03_NEXT_PAGE_FLG, (byte) 'Y'); }
    public void setNextPageNo() { setByte(CU03_NEXT_PAGE_FLG, (byte) 'N'); }
    public String cu03UsrSelFlg() { return getX(CU03_USR_SEL_FLG, 1); }
    public void setCu03UsrSelFlg(String v) { setX(CU03_USR_SEL_FLG, 1, v); }
    public String cu03UsrSelected() { return getX(CU03_USR_SELECTED, 8); }
    public void setCu03UsrSelected(String v) { setX(CU03_USR_SELECTED, 8, v); }
}

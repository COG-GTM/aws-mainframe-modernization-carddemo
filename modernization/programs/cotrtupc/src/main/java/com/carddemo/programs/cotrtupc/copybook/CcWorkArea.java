package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;
import com.carddemo.programs.cotrtupc.support.FixedRecord;

/** CVCRD01Y - CC-WORK-AREAS / CC-WORK-AREA (213 bytes). */
public final class CcWorkArea extends FixedRecord {

    public static final int LENGTH = 213;

    static final int AID = 0;            // X(5)
    static final int NEXT_PROG = 5;      // X(8)
    static final int NEXT_MAPSET = 13;   // X(7)
    static final int NEXT_MAP = 20;      // X(7)
    static final int ERROR_MSG = 27;     // X(75)
    static final int RETURN_MSG = 102;   // X(75)
    static final int ACCT_ID = 177;      // X(11) / 9(11)
    static final int CARD_NUM = 188;     // X(16) / 9(16)
    static final int CUST_ID = 204;      // X(09) / 9(9)

    /** CCARD-AID 88-level values. */
    public enum CcardAid {
        ENTER("ENTER"), CLEAR("CLEAR"), PA1("PA1  "), PA2("PA2  "),
        PFK01("PFK01"), PFK02("PFK02"), PFK03("PFK03"), PFK04("PFK04"), PFK05("PFK05"), PFK06("PFK06"),
        PFK07("PFK07"), PFK08("PFK08"), PFK09("PFK09"), PFK10("PFK10"), PFK11("PFK11"), PFK12("PFK12");

        public final String value;

        CcardAid(String value) {
            this.value = value;
        }
    }

    public CcWorkArea() {
        super(LENGTH);
        set(ACCT_ID, 11, "");
        set(CARD_NUM, 16, "");
        set(CUST_ID, 9, "");
    }

    /** INITIALIZE CC-WORK-AREA (all elementary items are alphanumeric -> SPACES). */
    public void initialize() {
        fill(0, LENGTH, ' ');
    }

    public String aid() { return get(AID, 5); }
    public void setAid(CcardAid aid) { set(AID, 5, aid.value); }
    public boolean is(CcardAid aid) { return aid().equals(aid.value); }

    public String nextProg() { return get(NEXT_PROG, 8); }
    public void nextProg(String v) { set(NEXT_PROG, 8, v); }
    public String nextMapset() { return get(NEXT_MAPSET, 7); }
    public void nextMapset(String v) { set(NEXT_MAPSET, 7, v); }
    public String nextMap() { return get(NEXT_MAP, 7); }
    public void nextMap(String v) { set(NEXT_MAP, 7, v); }
    public String errorMsg() { return get(ERROR_MSG, 75); }
    public void errorMsg(String v) { set(ERROR_MSG, 75, v); }
    public String returnMsg() { return get(RETURN_MSG, 75); }
    public void returnMsg(String v) { set(RETURN_MSG, 75, v); }
    public boolean isReturnMsgOff() { return CobolText.isLowValues(returnMsg()); }
    public String ccAcctId() { return get(ACCT_ID, 11); }
    public String ccCardNum() { return get(CARD_NUM, 16); }
    public String ccCustId() { return get(CUST_ID, 9); }
}

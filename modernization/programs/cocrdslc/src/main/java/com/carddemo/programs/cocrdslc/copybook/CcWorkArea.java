package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;

/**
 * CVCRD01Y - CC-WORK-AREAS / CC-WORK-AREA.
 *
 * <pre>
 * 10 CCARD-AID          PIC X(5).   88s ENTER, CLEAR, PA1, PA2, PFK01..PFK12
 * 10 CCARD-NEXT-PROG    PIC X(8).
 * 10 CCARD-NEXT-MAPSET  PIC X(7).
 * 10 CCARD-NEXT-MAP     PIC X(7).
 * 10 CCARD-ERROR-MSG    PIC X(75).
 * 10 CCARD-RETURN-MSG   PIC X(75).  88 CCARD-RETURN-MSG-OFF VALUE LOW-VALUES.
 * 10 CC-ACCT-ID         PIC X(11).  REDEFINES CC-ACCT-ID-N  PIC 9(11).
 * 10 CC-CARD-NUM        PIC X(16).  REDEFINES CC-CARD-NUM-N PIC 9(16).
 * 10 CC-CUST-ID         PIC X(09).  REDEFINES CC-CUST-ID-N  PIC 9(9).
 * </pre>
 */
public final class CcWorkArea {

    /** 88-level values of CCARD-AID. */
    public enum CcardAid {
        ENTER("ENTER"), CLEAR("CLEAR"), PA1("PA1  "), PA2("PA2  "),
        PFK01("PFK01"), PFK02("PFK02"), PFK03("PFK03"), PFK04("PFK04"),
        PFK05("PFK05"), PFK06("PFK06"), PFK07("PFK07"), PFK08("PFK08"),
        PFK09("PFK09"), PFK10("PFK10"), PFK11("PFK11"), PFK12("PFK12");

        private final String value;

        CcardAid(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    public String ccardAid;
    public String ccardNextProg;
    public String ccardNextMapset;
    public String ccardNextMap;
    public String ccardErrorMsg;
    public String ccardReturnMsg;
    public String ccAcctId;
    public String ccCardNum;
    public String ccCustId;

    public CcWorkArea() {
        initialize();
    }

    /** INITIALIZE CC-WORK-AREA: every elementary item is PIC X, so all become SPACES. */
    public void initialize() {
        ccardAid = CobolText.spaces(5);
        ccardNextProg = CobolText.spaces(8);
        ccardNextMapset = CobolText.spaces(7);
        ccardNextMap = CobolText.spaces(7);
        ccardErrorMsg = CobolText.spaces(75);
        ccardReturnMsg = CobolText.spaces(75);
        ccAcctId = CobolText.spaces(11);
        ccCardNum = CobolText.spaces(16);
        ccCustId = CobolText.spaces(9);
    }

    public boolean is(CcardAid aid) {
        return aid.value().equals(ccardAid);
    }

    public void set(CcardAid aid) {
        ccardAid = aid.value();
    }

    public boolean isCcardReturnMsgOff() {
        return CobolText.isLowValues(ccardReturnMsg);
    }

    public void ccardNextProg(String v) {
        ccardNextProg = CobolText.pic(v, 8);
    }

    public void ccardNextMapset(String v) {
        ccardNextMapset = CobolText.pic(v, 7);
    }

    public void ccardNextMap(String v) {
        ccardNextMap = CobolText.pic(v, 7);
    }

    public void ccardErrorMsg(String v) {
        ccardErrorMsg = CobolText.pic(v, 75);
    }

    public void ccAcctId(String v) {
        ccAcctId = CobolText.pic(v, 11);
    }

    public void ccCardNum(String v) {
        ccCardNum = CobolText.pic(v, 16);
    }

    /** CC-ACCT-ID-N (PIC 9(11) REDEFINES CC-ACCT-ID). */
    public long ccAcctIdN() {
        return CobolText.zonedValue(ccAcctId);
    }

    public void ccAcctIdN(long v) {
        ccAcctId = CobolText.zoned(v, 11);
    }

    /** CC-CARD-NUM-N (PIC 9(16) REDEFINES CC-CARD-NUM). */
    public long ccCardNumN() {
        return CobolText.zonedValue(ccCardNum);
    }

    public void ccCardNumN(long v) {
        ccCardNum = CobolText.zoned(v, 16);
    }

    public long ccCustIdN() {
        return CobolText.zonedValue(ccCustId);
    }
}

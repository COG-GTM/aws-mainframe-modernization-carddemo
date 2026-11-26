package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.displayValue;
import static com.carddemo.programs.cocrdupc.Cobol.numeric;
import static com.carddemo.programs.cocrdupc.Cobol.spaces;

/** CVCRD01Y: 01 CC-WORK-AREAS / 05 CC-WORK-AREA. */
public final class CcWorkArea {

    /** CCARD-AID PIC X(5) and its 88-levels. */
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

    private String ccardAid = spaces(5);
    private String ccardNextProg = spaces(8);
    private String ccardNextMapset = spaces(7);
    private String ccardNextMap = spaces(7);
    private String ccardErrorMsg = spaces(75);
    private String ccardReturnMsg = spaces(75);
    private String ccAcctId = spaces(11);
    private String ccCardNum = spaces(16);
    private String ccCustId = spaces(9);

    /** INITIALIZE CC-WORK-AREA. */
    public static CcWorkArea initialized() {
        return new CcWorkArea();
    }

    public String ccardAid() {
        return ccardAid;
    }

    public void setCcardAid(CcardAid aid) {
        this.ccardAid = aid.value();
    }

    public boolean is(CcardAid aid) {
        return ccardAid.equals(aid.value());
    }

    public String ccardNextProg() {
        return ccardNextProg;
    }

    public void setCcardNextProg(String v) {
        ccardNextProg = alnum(v, 8);
    }

    public String ccardNextMapset() {
        return ccardNextMapset;
    }

    public void setCcardNextMapset(String v) {
        ccardNextMapset = alnum(v, 7);
    }

    public String ccardNextMap() {
        return ccardNextMap;
    }

    public void setCcardNextMap(String v) {
        ccardNextMap = alnum(v, 7);
    }

    public String ccardErrorMsg() {
        return ccardErrorMsg;
    }

    public void setCcardErrorMsg(String v) {
        ccardErrorMsg = alnum(v, 75);
    }

    public String ccardReturnMsg() {
        return ccardReturnMsg;
    }

    /** 88 CCARD-RETURN-MSG-OFF VALUE LOW-VALUES. */
    public boolean isCcardReturnMsgOff() {
        return com.carddemo.programs.cocrdupc.Cobol.isLowValues(ccardReturnMsg);
    }

    public void setCcardReturnMsg(String v) {
        ccardReturnMsg = alnum(v, 75);
    }

    /** CC-ACCT-ID PIC X(11). */
    public String ccAcctId() {
        return ccAcctId;
    }

    public void setCcAcctId(String v) {
        ccAcctId = alnum(v, 11);
    }

    /** CC-ACCT-ID-N REDEFINES CC-ACCT-ID PIC 9(11). */
    public long ccAcctIdN() {
        return displayValue(ccAcctId);
    }

    public void setCcAcctIdN(long v) {
        ccAcctId = numeric(v, 11);
    }

    /** CC-CARD-NUM PIC X(16). */
    public String ccCardNum() {
        return ccCardNum;
    }

    public void setCcCardNum(String v) {
        ccCardNum = alnum(v, 16);
    }

    /** CC-CARD-NUM-N REDEFINES CC-CARD-NUM PIC 9(16). */
    public long ccCardNumN() {
        return displayValue(ccCardNum);
    }

    public void setCcCardNumN(long v) {
        ccCardNum = numeric(v, 16);
    }

    /** CC-CUST-ID PIC X(09). */
    public String ccCustId() {
        return ccCustId;
    }

    public void setCcCustId(String v) {
        ccCustId = alnum(v, 9);
    }

    /** CC-CUST-ID-N REDEFINES CC-CUST-ID PIC 9(9). */
    public long ccCustIdN() {
        return displayValue(ccCustId);
    }
}

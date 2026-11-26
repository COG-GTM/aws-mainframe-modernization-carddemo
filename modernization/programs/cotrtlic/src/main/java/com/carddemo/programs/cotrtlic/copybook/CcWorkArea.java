package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;

/** Copybook CVCRD01Y: 01 CC-WORK-AREAS / 05 CC-WORK-AREA. */
public final class CcWorkArea {

    /** 88-levels of CCARD-AID PIC X(5). */
    public enum CcardAid {
        ENTER("ENTER"), CLEAR("CLEAR"), PA1("PA1  "), PA2("PA2  "),
        PFK01("PFK01"), PFK02("PFK02"), PFK03("PFK03"), PFK04("PFK04"), PFK05("PFK05"), PFK06("PFK06"),
        PFK07("PFK07"), PFK08("PFK08"), PFK09("PFK09"), PFK10("PFK10"), PFK11("PFK11"), PFK12("PFK12");

        private final String value;

        CcardAid(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    /** CCARD-AID PIC X(5). */
    public String ccardAid;
    /** CCARD-NEXT-PROG PIC X(8). */
    public String ccardNextProg;
    /** CCARD-NEXT-MAPSET PIC X(7). */
    public String ccardNextMapset;
    /** CCARD-NEXT-MAP PIC X(7). */
    public String ccardNextMap;
    /** CCARD-ERROR-MSG PIC X(75). */
    public String ccardErrorMsg;
    /** CCARD-RETURN-MSG PIC X(75); 88 CCARD-RETURN-MSG-OFF VALUE LOW-VALUES. */
    public String ccardReturnMsg;
    /** CC-ACCT-ID PIC X(11) (REDEFINES CC-ACCT-ID-N PIC 9(11)). */
    public String ccAcctId;
    /** CC-CARD-NUM PIC X(16) (REDEFINES CC-CARD-NUM-N PIC 9(16)). */
    public String ccCardNum;
    /** CC-CUST-ID PIC X(09) (REDEFINES CC-CUST-ID-N PIC 9(9)). */
    public String ccCustId;

    public CcWorkArea() {
        initialize();
        ccAcctId = Cob.spaces(11);
        ccCardNum = Cob.spaces(16);
        ccCustId = Cob.spaces(9);
    }

    /** INITIALIZE CC-WORK-AREA. */
    public void initialize() {
        ccardAid = Cob.spaces(5);
        ccardNextProg = Cob.spaces(8);
        ccardNextMapset = Cob.spaces(7);
        ccardNextMap = Cob.spaces(7);
        ccardErrorMsg = Cob.spaces(75);
        ccardReturnMsg = Cob.spaces(75);
        ccAcctId = Cob.spaces(11);
        ccCardNum = Cob.spaces(16);
        ccCustId = Cob.spaces(9);
    }

    public boolean is(CcardAid aid) {
        return aid.value().equals(ccardAid);
    }

    public void set(CcardAid aid) {
        ccardAid = aid.value();
    }

    public boolean isReturnMsgOff() {
        return Cob.isLowValues(ccardReturnMsg);
    }

    /** CC-ACCT-ID-N view. */
    public boolean ccAcctIdIsNumeric() {
        return Cob.isNumeric(ccAcctId);
    }
}

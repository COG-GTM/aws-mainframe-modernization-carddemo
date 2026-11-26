package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;

/** CVCRD01Y - CC-WORK-AREAS / CC-WORK-AREA (AID tracking, next program/map, filter keys). */
public final class CcWorkArea {

    /** 88-levels on CCARD-AID PIC X(5). */
    public enum CcardAid {
        ENTER("ENTER"), CLEAR("CLEAR"), PA1("PA1  "), PA2("PA2  "),
        PFK01("PFK01"), PFK02("PFK02"), PFK03("PFK03"), PFK04("PFK04"), PFK05("PFK05"), PFK06("PFK06"),
        PFK07("PFK07"), PFK08("PFK08"), PFK09("PFK09"), PFK10("PFK10"), PFK11("PFK11"), PFK12("PFK12");

        public final String value;

        CcardAid(String value) {
            this.value = value;
        }
    }

    /** CCARD-AID PIC X(5). */
    public String ccardAid = Cobol.spaces(5);
    /** CCARD-NEXT-PROG PIC X(8). */
    public String ccardNextProg = Cobol.spaces(8);
    /** CCARD-NEXT-MAPSET PIC X(7). */
    public String ccardNextMapset = Cobol.spaces(7);
    /** CCARD-NEXT-MAP PIC X(7). */
    public String ccardNextMap = Cobol.spaces(7);
    /** CCARD-ERROR-MSG PIC X(75). */
    public String ccardErrorMsg = Cobol.spaces(75);
    /** CCARD-RETURN-MSG PIC X(75): 88 CCARD-RETURN-MSG-OFF LOW-VALUES. */
    public String ccardReturnMsg = Cobol.spaces(75);
    /** CC-ACCT-ID PIC X(11) VALUE SPACES, redefined by CC-ACCT-ID-N PIC 9(11). */
    public String ccAcctId = Cobol.spaces(11);
    /** CC-CARD-NUM PIC X(16) VALUE SPACES, redefined by CC-CARD-NUM-N PIC 9(16). */
    public String ccCardNum = Cobol.spaces(16);
    /** CC-CUST-ID PIC X(09) VALUE SPACES, redefined by CC-CUST-ID-N PIC 9(9). */
    public String ccCustId = Cobol.spaces(9);

    /** INITIALIZE CC-WORK-AREA. */
    public void initialize() {
        ccardAid = Cobol.spaces(5);
        ccardNextProg = Cobol.spaces(8);
        ccardNextMapset = Cobol.spaces(7);
        ccardNextMap = Cobol.spaces(7);
        ccardErrorMsg = Cobol.spaces(75);
        ccardReturnMsg = Cobol.spaces(75);
        ccAcctId = Cobol.spaces(11);
        ccCardNum = Cobol.spaces(16);
        ccCustId = Cobol.spaces(9);
    }

    public boolean is(CcardAid aid) {
        return aid.value.equals(ccardAid);
    }

    public void set(CcardAid aid) {
        ccardAid = aid.value;
    }

    public boolean isCcardReturnMsgOff() {
        return Cobol.isLowValues(ccardReturnMsg);
    }

    public long ccAcctIdN() {
        return Cobol.unsignedValue(ccAcctId);
    }

    public long ccCardNumN() {
        return Cobol.unsignedValue(ccCardNum);
    }

    public long ccCustIdN() {
        return Cobol.unsignedValue(ccCustId);
    }
}

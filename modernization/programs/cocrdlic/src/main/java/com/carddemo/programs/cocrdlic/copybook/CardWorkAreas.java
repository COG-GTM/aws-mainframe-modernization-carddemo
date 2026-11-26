package com.carddemo.programs.cocrdlic.copybook;

/** CVCRD01Y - CC-WORK-AREAS: mapped AID key, next program routing, messages and key search fields. */
public final class CardWorkAreas {

    /** CCARD-AID PIC X(5) and its 88-level values. {@link #NONE} represents SPACES (no 88 true). */
    public enum CcardAid {
        NONE("     "),
        ENTER("ENTER"), CLEAR("CLEAR"), PA1("PA1  "), PA2("PA2  "),
        PFK01("PFK01"), PFK02("PFK02"), PFK03("PFK03"), PFK04("PFK04"), PFK05("PFK05"), PFK06("PFK06"),
        PFK07("PFK07"), PFK08("PFK08"), PFK09("PFK09"), PFK10("PFK10"), PFK11("PFK11"), PFK12("PFK12");

        public final String value;

        CcardAid(String value) {
            this.value = value;
        }
    }

    public CcardAid ccardAid;          // X(5)
    public String ccardNextProg;       // X(8)
    public String ccardNextMapset;     // X(7)
    public String ccardNextMap;        // X(7)
    public String ccardErrorMsg;       // X(75)
    public String ccardReturnMsg;      // X(75)  88 CCARD-RETURN-MSG-OFF LOW-VALUES
    public String ccAcctId;            // X(11)  REDEFINES CC-ACCT-ID-N 9(11)
    public String ccCardNum;           // X(16)  REDEFINES CC-CARD-NUM-N 9(16)
    public String ccCustId;            // X(09)  REDEFINES CC-CUST-ID-N 9(9)

    public CardWorkAreas() {
        initialize();
    }

    /** INITIALIZE CC-WORK-AREA: every alphanumeric to SPACES. */
    public void initialize() {
        ccardAid = CcardAid.NONE;
        ccardNextProg = Pic.spaces(8);
        ccardNextMapset = Pic.spaces(7);
        ccardNextMap = Pic.spaces(7);
        ccardErrorMsg = Pic.spaces(75);
        ccardReturnMsg = Pic.spaces(75);
        ccAcctId = Pic.spaces(11);
        ccCardNum = Pic.spaces(16);
        ccCustId = Pic.spaces(9);
    }

    public boolean isReturnMsgOff() {
        return Pic.isLowValues(ccardReturnMsg);
    }

    public long ccAcctIdN() {
        return Pic.toUnsigned(ccAcctId, 11);
    }

    public long ccCardNumN() {
        return Pic.toUnsigned(ccCardNum, 16);
    }

    public long ccCustIdN() {
        return Pic.toUnsigned(ccCustId, 9);
    }
}

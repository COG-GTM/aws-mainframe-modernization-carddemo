package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/** CVCRD01Y: {@code CC-WORK-AREAS / CC-WORK-AREA}. Alphanumerics are kept at their exact PIC width. */
public final class CcWorkArea {
    public static final int CCARD_NEXT_PROG_LEN = 8;
    public static final int CCARD_NEXT_MAPSET_LEN = 7;
    public static final int CCARD_NEXT_MAP_LEN = 7;
    public static final int CCARD_ERROR_MSG_LEN = 75;
    public static final int CCARD_RETURN_MSG_LEN = 75;
    public static final int CC_ACCT_ID_LEN = 11;
    public static final int CC_CARD_NUM_LEN = 16;
    public static final int CC_CUST_ID_LEN = 9;

    /** CCARD-AID PIC X(5); {@code null} = no 88-level value (e.g. after INITIALIZE to spaces). */
    private CcardAid ccardAid;
    private String ccardNextProg;
    private String ccardNextMapset;
    private String ccardNextMap;
    private String ccardErrorMsg;
    private String ccardReturnMsg;
    private String ccAcctId;
    private String ccCardNum;
    private String ccCustId;

    public CcWorkArea() {
        initialize();
    }

    /** INITIALIZE CC-WORK-AREA: every alphanumeric to SPACES. */
    public void initialize() {
        ccardAid = null;
        ccardNextProg = Cobol.spaces(CCARD_NEXT_PROG_LEN);
        ccardNextMapset = Cobol.spaces(CCARD_NEXT_MAPSET_LEN);
        ccardNextMap = Cobol.spaces(CCARD_NEXT_MAP_LEN);
        ccardErrorMsg = Cobol.spaces(CCARD_ERROR_MSG_LEN);
        ccardReturnMsg = Cobol.spaces(CCARD_RETURN_MSG_LEN);
        ccAcctId = Cobol.spaces(CC_ACCT_ID_LEN);
        ccCardNum = Cobol.spaces(CC_CARD_NUM_LEN);
        ccCustId = Cobol.spaces(CC_CUST_ID_LEN);
    }

    public CcardAid ccardAid() { return ccardAid; }
    public void setCcardAid(CcardAid aid) { this.ccardAid = aid; }
    public boolean isCcardAidEnter() { return ccardAid == CcardAid.ENTER; }
    public boolean isCcardAidPfk03() { return ccardAid == CcardAid.PFK03; }

    public String ccardNextProg() { return ccardNextProg; }
    public void setCcardNextProg(String v) { ccardNextProg = Cobol.alnum(v, CCARD_NEXT_PROG_LEN); }
    public String ccardNextMapset() { return ccardNextMapset; }
    public void setCcardNextMapset(String v) { ccardNextMapset = Cobol.alnum(v, CCARD_NEXT_MAPSET_LEN); }
    public String ccardNextMap() { return ccardNextMap; }
    public void setCcardNextMap(String v) { ccardNextMap = Cobol.alnum(v, CCARD_NEXT_MAP_LEN); }
    public String ccardErrorMsg() { return ccardErrorMsg; }
    public void setCcardErrorMsg(String v) { ccardErrorMsg = Cobol.alnum(v, CCARD_ERROR_MSG_LEN); }
    public String ccardReturnMsg() { return ccardReturnMsg; }
    public void setCcardReturnMsg(String v) { ccardReturnMsg = Cobol.alnum(v, CCARD_RETURN_MSG_LEN); }
    /** 88 CCARD-RETURN-MSG-OFF VALUE LOW-VALUES. */
    public boolean isCcardReturnMsgOff() { return Cobol.isLowValues(ccardReturnMsg); }

    /** CC-ACCT-ID PIC X(11) (REDEFINED as CC-ACCT-ID-N PIC 9(11)). */
    public String ccAcctId() { return ccAcctId; }
    public void setCcAcctId(String v) { ccAcctId = Cobol.alnum(v, CC_ACCT_ID_LEN); }
    public long ccAcctIdN() { return Cobol.toLong(ccAcctId); }
    public String ccCardNum() { return ccCardNum; }
    public void setCcCardNum(String v) { ccCardNum = Cobol.alnum(v, CC_CARD_NUM_LEN); }
    public long ccCardNumN() { return Cobol.toLong(ccCardNum); }
    public String ccCustId() { return ccCustId; }
    public void setCcCustId(String v) { ccCustId = Cobol.alnum(v, CC_CUST_ID_LEN); }
    public long ccCustIdN() { return Cobol.toLong(ccCustId); }
}

package com.carddemo.programs.cocrdlic.copybook;

/**
 * COCOM01Y - CARDDEMO-COMMAREA (160 bytes), the application-wide COMMAREA passed between programs.
 */
public final class CardDemoCommarea {
    public static final int LENGTH = 160;

    /** CDEMO-USER-TYPE 88-levels. */
    public enum UserType {
        ADMIN('A'), USER('U');

        public final char code;

        UserType(char code) {
            this.code = code;
        }
    }

    // 05 CDEMO-GENERAL-INFO
    private String fromTranid;      // X(04)
    private String fromProgram;     // X(08)
    private String toTranid;        // X(04)
    private String toProgram;       // X(08)
    private String userId;          // X(08)
    private char userType;          // X(01)
    private int pgmContext;         // 9(01)  88 ENTER 0, REENTER 1
    // 05 CDEMO-CUSTOMER-INFO
    private long custId;            // 9(09)
    private String custFname;       // X(25)
    private String custMname;       // X(25)
    private String custLname;       // X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private long acctId;            // 9(11)
    private char acctStatus;        // X(01)
    // 05 CDEMO-CARD-INFO
    private long cardNum;           // 9(16)
    // 05 CDEMO-MORE-INFO
    private String lastMap;         // X(7)
    private String lastMapset;      // X(7)

    public CardDemoCommarea() {
        initialize();
    }

    /** COBOL INITIALIZE: alphanumerics to SPACES, numerics to ZERO. */
    public void initialize() {
        fromTranid = Pic.spaces(4);
        fromProgram = Pic.spaces(8);
        toTranid = Pic.spaces(4);
        toProgram = Pic.spaces(8);
        userId = Pic.spaces(8);
        userType = ' ';
        pgmContext = 0;
        custId = 0;
        custFname = Pic.spaces(25);
        custMname = Pic.spaces(25);
        custLname = Pic.spaces(25);
        acctId = 0;
        acctStatus = ' ';
        cardNum = 0;
        lastMap = Pic.spaces(7);
        lastMapset = Pic.spaces(7);
    }

    public CardDemoCommarea copy() {
        return fromFixedWidth(toFixedWidth());
    }

    public String toFixedWidth() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType
                + Pic.nine(pgmContext, 1)
                + Pic.nine(custId, 9) + custFname + custMname + custLname
                + Pic.nine(acctId, 11) + acctStatus
                + Pic.nine(cardNum, 16)
                + lastMap + lastMapset;
    }

    public static CardDemoCommarea fromFixedWidth(String image) {
        String s = Pic.x(image, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid = s.substring(0, 4);
        c.fromProgram = s.substring(4, 12);
        c.toTranid = s.substring(12, 16);
        c.toProgram = s.substring(16, 24);
        c.userId = s.substring(24, 32);
        c.userType = s.charAt(32);
        c.pgmContext = (int) Pic.toUnsigned(s.substring(33, 34), 1);
        c.custId = Pic.toUnsigned(s.substring(34, 43), 9);
        c.custFname = s.substring(43, 68);
        c.custMname = s.substring(68, 93);
        c.custLname = s.substring(93, 118);
        c.acctId = Pic.toUnsigned(s.substring(118, 129), 11);
        c.acctStatus = s.charAt(129);
        c.cardNum = Pic.toUnsigned(s.substring(130, 146), 16);
        c.lastMap = s.substring(146, 153);
        c.lastMapset = s.substring(153, 160);
        return c;
    }

    public boolean isUserTypeAdmin() {
        return userType == UserType.ADMIN.code;
    }

    public boolean isUserTypeUser() {
        return userType == UserType.USER.code;
    }

    public void setUserType(UserType t) {
        userType = t.code;
    }

    public boolean isPgmEnter() {
        return pgmContext == 0;
    }

    public boolean isPgmReenter() {
        return pgmContext == 1;
    }

    public void setPgmEnter() {
        pgmContext = 0;
    }

    public void setPgmReenter() {
        pgmContext = 1;
    }

    public String getFromTranid() { return fromTranid; }
    public void setFromTranid(String v) { fromTranid = Pic.x(v, 4); }
    public String getFromProgram() { return fromProgram; }
    public void setFromProgram(String v) { fromProgram = Pic.x(v, 8); }
    public String getToTranid() { return toTranid; }
    public void setToTranid(String v) { toTranid = Pic.x(v, 4); }
    public String getToProgram() { return toProgram; }
    public void setToProgram(String v) { toProgram = Pic.x(v, 8); }
    public String getUserId() { return userId; }
    public void setUserId(String v) { userId = Pic.x(v, 8); }
    public char getUserType() { return userType; }
    public void setUserType(char v) { userType = v; }
    public int getPgmContext() { return pgmContext; }
    public void setPgmContext(int v) { pgmContext = (int) Pic.truncUnsigned(v, 1); }
    public long getCustId() { return custId; }
    public void setCustId(long v) { custId = Pic.truncUnsigned(v, 9); }
    public String getCustFname() { return custFname; }
    public void setCustFname(String v) { custFname = Pic.x(v, 25); }
    public String getCustMname() { return custMname; }
    public void setCustMname(String v) { custMname = Pic.x(v, 25); }
    public String getCustLname() { return custLname; }
    public void setCustLname(String v) { custLname = Pic.x(v, 25); }
    public long getAcctId() { return acctId; }
    public void setAcctId(long v) { acctId = Pic.truncUnsigned(v, 11); }
    public char getAcctStatus() { return acctStatus; }
    public void setAcctStatus(char v) { acctStatus = v; }
    public long getCardNum() { return cardNum; }
    public void setCardNum(long v) { cardNum = Pic.truncUnsigned(v, 16); }
    public String getLastMap() { return lastMap; }
    public void setLastMap(String v) { lastMap = Pic.x(v, 7); }
    public String getLastMapset() { return lastMapset; }
    public void setLastMapset(String v) { lastMapset = Pic.x(v, 7); }
}

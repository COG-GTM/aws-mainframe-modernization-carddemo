package com.carddemo.programs.copaus1c.copybook;

/**
 * COCOM01Y - CARDDEMO-COMMAREA general/customer/account/card sections.
 * CDEMO-ACCT-ID is kept as its raw PIC 9(11) display text because COPAUS1C applies an IS NUMERIC test to it.
 */
public class CardDemoCommarea {

    /** 88-levels on CDEMO-USER-TYPE. */
    public enum UserType {
        CDEMO_USRTYP_ADMIN('A'), CDEMO_USRTYP_USER('U');

        public final char code;

        UserType(char code) {
            this.code = code;
        }
    }

    /** 88-levels on CDEMO-PGM-CONTEXT. */
    public enum PgmContext {
        CDEMO_PGM_ENTER(0), CDEMO_PGM_REENTER(1);

        public final int value;

        PgmContext(int value) {
            this.value = value;
        }
    }

    private String fromTranid = Cobol.spaces(4);     // CDEMO-FROM-TRANID  X(04)
    private String fromProgram = Cobol.spaces(8);    // CDEMO-FROM-PROGRAM X(08)
    private String toTranid = Cobol.spaces(4);       // CDEMO-TO-TRANID    X(04)
    private String toProgram = Cobol.spaces(8);      // CDEMO-TO-PROGRAM   X(08)
    private String userId = Cobol.spaces(8);         // CDEMO-USER-ID      X(08)
    private char userType = ' ';                     // CDEMO-USER-TYPE    X(01)
    private int pgmContext;                          // CDEMO-PGM-CONTEXT  9(01)
    private long custId;                             // CDEMO-CUST-ID      9(09)
    private String custFname = Cobol.spaces(25);     // CDEMO-CUST-FNAME   X(25)
    private String custMname = Cobol.spaces(25);     // CDEMO-CUST-MNAME   X(25)
    private String custLname = Cobol.spaces(25);     // CDEMO-CUST-LNAME   X(25)
    private String acctId = Cobol.nine(0, 11);       // CDEMO-ACCT-ID      9(11)
    private char acctStatus = ' ';                   // CDEMO-ACCT-STATUS  X(01)
    private long cardNum;                            // CDEMO-CARD-NUM     9(16)
    private String lastMap = Cobol.spaces(7);        // CDEMO-LAST-MAP     X(07)
    private String lastMapset = Cobol.spaces(7);     // CDEMO-LAST-MAPSET  X(07)

    /** INITIALIZE: alphanumerics to SPACES, numerics to ZERO. */
    public void initialize() {
        copyFrom(new CardDemoCommarea());
    }

    public void copyFrom(CardDemoCommarea o) {
        fromTranid = o.fromTranid;
        fromProgram = o.fromProgram;
        toTranid = o.toTranid;
        toProgram = o.toProgram;
        userId = o.userId;
        userType = o.userType;
        pgmContext = o.pgmContext;
        custId = o.custId;
        custFname = o.custFname;
        custMname = o.custMname;
        custLname = o.custLname;
        acctId = o.acctId;
        acctStatus = o.acctStatus;
        cardNum = o.cardNum;
        lastMap = o.lastMap;
        lastMapset = o.lastMapset;
    }

    public boolean isUserType(UserType t) { return userType == t.code; }
    public boolean isPgmEnter() { return pgmContext == PgmContext.CDEMO_PGM_ENTER.value; }
    public boolean isPgmReenter() { return pgmContext == PgmContext.CDEMO_PGM_REENTER.value; }
    public void setPgmContext(PgmContext c) { pgmContext = c.value; }

    public String getFromTranid() { return fromTranid; }
    public void setFromTranid(String v) { fromTranid = Cobol.x(v, 4); }
    public String getFromProgram() { return fromProgram; }
    public void setFromProgram(String v) { fromProgram = Cobol.x(v, 8); }
    public String getToTranid() { return toTranid; }
    public void setToTranid(String v) { toTranid = Cobol.x(v, 4); }
    public String getToProgram() { return toProgram; }
    public void setToProgram(String v) { toProgram = Cobol.x(v, 8); }
    public String getUserId() { return userId; }
    public void setUserId(String v) { userId = Cobol.x(v, 8); }
    public char getUserType() { return userType; }
    public void setUserType(char v) { userType = v; }
    public int getPgmContext() { return pgmContext; }
    public void setPgmContext(int v) { pgmContext = Math.abs(v) % 10; }
    public long getCustId() { return custId; }
    public void setCustId(long v) { custId = Math.abs(v) % 1_000_000_000L; }
    public String getCustFname() { return custFname; }
    public void setCustFname(String v) { custFname = Cobol.x(v, 25); }
    public String getCustMname() { return custMname; }
    public void setCustMname(String v) { custMname = Cobol.x(v, 25); }
    public String getCustLname() { return custLname; }
    public void setCustLname(String v) { custLname = Cobol.x(v, 25); }
    /** Raw PIC 9(11) display content (may be non-numeric if the caller put garbage in it). */
    public String getAcctId() { return acctId; }
    public void setAcctId(String raw) { acctId = Cobol.x(raw, 11); }
    public void setAcctId(long v) { acctId = Cobol.nine(v, 11); }
    public char getAcctStatus() { return acctStatus; }
    public void setAcctStatus(char v) { acctStatus = v; }
    public long getCardNum() { return cardNum; }
    public void setCardNum(long v) { cardNum = Math.abs(v) % 10_000_000_000_000_000L; }
    public String getLastMap() { return lastMap; }
    public void setLastMap(String v) { lastMap = Cobol.x(v, 7); }
    public String getLastMapset() { return lastMapset; }
    public void setLastMapset(String v) { lastMapset = Cobol.x(v, 7); }
}

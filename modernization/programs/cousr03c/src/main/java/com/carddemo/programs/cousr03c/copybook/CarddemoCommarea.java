package com.carddemo.programs.cousr03c.copybook;

/** COCOM01Y: 01 CARDDEMO-COMMAREA (160 bytes). */
public class CarddemoCommarea extends FixedRecord {

    public static final int LENGTH = 160;

    /** 88-levels on CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), USER('U');

        public final char code;

        UserType(char code) {
            this.code = code;
        }
    }

    /** 88-levels on CDEMO-PGM-CONTEXT. */
    public enum PgmContext {
        ENTER('0'), REENTER('1');

        public final char code;

        PgmContext(char code) {
            this.code = code;
        }
    }

    // CDEMO-GENERAL-INFO
    static final int FROM_TRANID = 0;     // X(04)
    static final int FROM_PROGRAM = 4;    // X(08)
    static final int TO_TRANID = 12;      // X(04)
    static final int TO_PROGRAM = 16;     // X(08)
    static final int USER_ID = 24;        // X(08)
    static final int USER_TYPE = 32;      // X(01)
    static final int PGM_CONTEXT = 33;    // 9(01)
    // CDEMO-CUSTOMER-INFO
    static final int CUST_ID = 34;        // 9(09)
    static final int CUST_FNAME = 43;     // X(25)
    static final int CUST_MNAME = 68;     // X(25)
    static final int CUST_LNAME = 93;     // X(25)
    // CDEMO-ACCOUNT-INFO
    static final int ACCT_ID = 118;       // 9(11)
    static final int ACCT_STATUS = 129;   // X(01)
    // CDEMO-CARD-INFO
    static final int CARD_NUM = 130;      // 9(16)
    // CDEMO-MORE-INFO
    static final int LAST_MAP = 146;      // X(7)
    static final int LAST_MAPSET = 153;   // X(7)

    public CarddemoCommarea() {
        super(LENGTH);
    }

    protected CarddemoCommarea(int length) {
        super(length);
    }

    public CarddemoCommarea(byte[] buf, int base) {
        super(buf, base, LENGTH);
    }

    public String fromTranid() { return getX(FROM_TRANID, 4); }
    public void setFromTranid(String v) { setX(FROM_TRANID, 4, v); }
    public String fromProgram() { return getX(FROM_PROGRAM, 8); }
    public void setFromProgram(String v) { setX(FROM_PROGRAM, 8, v); }
    public String toTranid() { return getX(TO_TRANID, 4); }
    public void setToTranid(String v) { setX(TO_TRANID, 4, v); }
    public String toProgram() { return getX(TO_PROGRAM, 8); }
    public void setToProgram(String v) { setX(TO_PROGRAM, 8, v); }
    public String userId() { return getX(USER_ID, 8); }
    public void setUserId(String v) { setX(USER_ID, 8, v); }
    public String userType() { return getX(USER_TYPE, 1); }
    public void setUserType(String v) { setX(USER_TYPE, 1, v); }
    public boolean isUsrtypAdmin() { return getByte(USER_TYPE) == UserType.ADMIN.code; }
    public boolean isUsrtypUser() { return getByte(USER_TYPE) == UserType.USER.code; }
    public void setUserType(UserType t) { setByte(USER_TYPE, (byte) t.code); }

    /** Raw CDEMO-PGM-CONTEXT byte as a string (may be non-numeric, e.g. LOW-VALUES). */
    public String pgmContextRaw() { return getX(PGM_CONTEXT, 1); }
    public int pgmContext() { return (int) getNum(PGM_CONTEXT, 1); }
    public void setPgmContext(int v) { setNum(PGM_CONTEXT, 1, v); }
    public boolean isPgmEnter() { return getByte(PGM_CONTEXT) == PgmContext.ENTER.code; }
    public boolean isPgmReenter() { return getByte(PGM_CONTEXT) == PgmContext.REENTER.code; }
    public void setPgmContext(PgmContext c) { setByte(PGM_CONTEXT, (byte) c.code); }

    public long custId() { return getNum(CUST_ID, 9); }
    public void setCustId(long v) { setNum(CUST_ID, 9, v); }
    public String custFname() { return getX(CUST_FNAME, 25); }
    public void setCustFname(String v) { setX(CUST_FNAME, 25, v); }
    public String custMname() { return getX(CUST_MNAME, 25); }
    public void setCustMname(String v) { setX(CUST_MNAME, 25, v); }
    public String custLname() { return getX(CUST_LNAME, 25); }
    public void setCustLname(String v) { setX(CUST_LNAME, 25, v); }
    public long acctId() { return getNum(ACCT_ID, 11); }
    public void setAcctId(long v) { setNum(ACCT_ID, 11, v); }
    public String acctStatus() { return getX(ACCT_STATUS, 1); }
    public void setAcctStatus(String v) { setX(ACCT_STATUS, 1, v); }
    public long cardNum() { return getNum(CARD_NUM, 16); }
    public void setCardNum(long v) { setNum(CARD_NUM, 16, v); }
    public String lastMap() { return getX(LAST_MAP, 7); }
    public void setLastMap(String v) { setX(LAST_MAP, 7, v); }
    public String lastMapset() { return getX(LAST_MAPSET, 7); }
    public void setLastMapset(String v) { setX(LAST_MAPSET, 7, v); }
}

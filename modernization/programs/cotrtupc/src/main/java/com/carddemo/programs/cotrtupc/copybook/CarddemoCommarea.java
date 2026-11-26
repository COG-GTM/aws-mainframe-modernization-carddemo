package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.FixedRecord;

/** COCOM01Y - CARDDEMO-COMMAREA (160 bytes). */
public final class CarddemoCommarea extends FixedRecord {

    public static final int LENGTH = 160;

    // 05 CDEMO-GENERAL-INFO
    static final int FROM_TRANID = 0;      // X(04)
    static final int FROM_PROGRAM = 4;     // X(08)
    static final int TO_TRANID = 12;       // X(04)
    static final int TO_PROGRAM = 16;      // X(08)
    static final int USER_ID = 24;         // X(08)
    static final int USER_TYPE = 32;       // X(01)
    static final int PGM_CONTEXT = 33;     // 9(01)
    // 05 CDEMO-CUSTOMER-INFO
    static final int CUST_ID = 34;         // 9(09)
    static final int CUST_FNAME = 43;      // X(25)
    static final int CUST_MNAME = 68;      // X(25)
    static final int CUST_LNAME = 93;      // X(25)
    // 05 CDEMO-ACCOUNT-INFO
    static final int ACCT_ID = 118;        // 9(11)
    static final int ACCT_STATUS = 129;    // X(01)
    // 05 CDEMO-CARD-INFO
    static final int CARD_NUM = 130;       // 9(16)
    // 05 CDEMO-MORE-INFO
    static final int LAST_MAP = 146;       // X(7)
    static final int LAST_MAPSET = 153;    // X(7)

    public CarddemoCommarea() {
        super(LENGTH);
    }

    /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics to SPACES, numerics to ZERO. */
    public void initialize() {
        fill(0, LENGTH, ' ');
        setNumeric(PGM_CONTEXT, 1, 0);
        setNumeric(CUST_ID, 9, 0);
        setNumeric(ACCT_ID, 11, 0);
        setNumeric(CARD_NUM, 16, 0);
    }

    public String fromTranid() { return get(FROM_TRANID, 4); }
    public void fromTranid(String v) { set(FROM_TRANID, 4, v); }
    public String fromProgram() { return get(FROM_PROGRAM, 8); }
    public void fromProgram(String v) { set(FROM_PROGRAM, 8, v); }
    public String toTranid() { return get(TO_TRANID, 4); }
    public void toTranid(String v) { set(TO_TRANID, 4, v); }
    public String toProgram() { return get(TO_PROGRAM, 8); }
    public void toProgram(String v) { set(TO_PROGRAM, 8, v); }
    public String userId() { return get(USER_ID, 8); }
    public void userId(String v) { set(USER_ID, 8, v); }
    public String userType() { return get(USER_TYPE, 1); }
    public void userType(String v) { set(USER_TYPE, 1, v); }
    public String pgmContextRaw() { return get(PGM_CONTEXT, 1); }

    public boolean isUsrtypAdmin() { return data[USER_TYPE] == 'A'; }
    public boolean isUsrtypUser() { return data[USER_TYPE] == 'U'; }
    public void setUsrtypAdmin() { data[USER_TYPE] = 'A'; }
    public void setUsrtypUser() { data[USER_TYPE] = 'U'; }

    public boolean isPgmEnter() { return data[PGM_CONTEXT] == '0'; }
    public boolean isPgmReenter() { return data[PGM_CONTEXT] == '1'; }
    public void setPgmEnter() { data[PGM_CONTEXT] = '0'; }
    public void setPgmReenter() { data[PGM_CONTEXT] = '1'; }

    public long custId() { return getNumeric(CUST_ID, 9); }
    public void custId(long v) { setNumeric(CUST_ID, 9, v); }
    public String custIdRaw() { return get(CUST_ID, 9); }
    public String custFname() { return get(CUST_FNAME, 25); }
    public void custFname(String v) { set(CUST_FNAME, 25, v); }
    public String custMname() { return get(CUST_MNAME, 25); }
    public void custMname(String v) { set(CUST_MNAME, 25, v); }
    public String custLname() { return get(CUST_LNAME, 25); }
    public void custLname(String v) { set(CUST_LNAME, 25, v); }

    public long acctId() { return getNumeric(ACCT_ID, 11); }
    public void acctId(long v) { setNumeric(ACCT_ID, 11, v); }
    public String acctIdRaw() { return get(ACCT_ID, 11); }
    public String acctStatus() { return get(ACCT_STATUS, 1); }
    public void acctStatus(String v) { set(ACCT_STATUS, 1, v); }
    public void acctStatusLowValues() { fill(ACCT_STATUS, 1, '\u0000'); }

    public long cardNum() { return getNumeric(CARD_NUM, 16); }
    public void cardNum(long v) { setNumeric(CARD_NUM, 16, v); }
    public String cardNumRaw() { return get(CARD_NUM, 16); }

    public String lastMap() { return get(LAST_MAP, 7); }
    public void lastMap(String v) { set(LAST_MAP, 7, v); }
    public String lastMapset() { return get(LAST_MAPSET, 7); }
    public void lastMapset(String v) { set(LAST_MAPSET, 7, v); }
}

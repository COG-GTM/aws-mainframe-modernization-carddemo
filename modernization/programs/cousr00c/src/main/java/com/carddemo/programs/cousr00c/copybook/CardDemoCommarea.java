package com.carddemo.programs.cousr00c.copybook;

import com.carddemo.programs.cousr00c.Cobol;

/**
 * COCOM01Y - CARDDEMO-COMMAREA (160 bytes). Mutable like the COBOL working-storage it replaces.
 */
public class CardDemoCommarea {

    public static final int LENGTH = 160;

    /** CDEMO-USER-TYPE 88-levels. */
    public enum UserType {
        /** 88 CDEMO-USRTYP-ADMIN VALUE 'A'. */
        ADMIN('A'),
        /** 88 CDEMO-USRTYP-USER VALUE 'U'. */
        USER('U');

        private final char code;

        UserType(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }
    }

    /** CDEMO-PGM-CONTEXT 88-levels. */
    public enum PgmContext {
        /** 88 CDEMO-PGM-ENTER VALUE 0. */
        ENTER(0),
        /** 88 CDEMO-PGM-REENTER VALUE 1. */
        REENTER(1);

        private final int code;

        PgmContext(int code) {
            this.code = code;
        }

        public int code() {
            return code;
        }
    }

    // CDEMO-GENERAL-INFO
    private String fromTranid = Cobol.lowValues(4);    // PIC X(04)
    private String fromProgram = Cobol.lowValues(8);   // PIC X(08)
    private String toTranid = Cobol.lowValues(4);      // PIC X(04)
    private String toProgram = Cobol.lowValues(8);     // PIC X(08)
    private String userId = Cobol.lowValues(8);        // PIC X(08)
    private char userType = Cobol.LOW_VALUE;           // PIC X(01)
    private int pgmContext;                            // PIC 9(01)
    // CDEMO-CUSTOMER-INFO
    private long custId;                               // PIC 9(09)
    private String custFname = Cobol.lowValues(25);    // PIC X(25)
    private String custMname = Cobol.lowValues(25);    // PIC X(25)
    private String custLname = Cobol.lowValues(25);    // PIC X(25)
    // CDEMO-ACCOUNT-INFO
    private long acctId;                               // PIC 9(11)
    private char acctStatus = Cobol.LOW_VALUE;         // PIC X(01)
    // CDEMO-CARD-INFO
    private long cardNum;                              // PIC 9(16)
    // CDEMO-MORE-INFO
    private String lastMap = Cobol.lowValues(7);       // PIC X(7)
    private String lastMapset = Cobol.lowValues(7);    // PIC X(7)

    public static CardDemoCommarea fromFixedWidth(String raw) {
        String s = Cobol.alnum(raw, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        int[] p = {0};
        c.fromTranid = take(s, p, 4);
        c.fromProgram = take(s, p, 8);
        c.toTranid = take(s, p, 4);
        c.toProgram = take(s, p, 8);
        c.userId = take(s, p, 8);
        c.userType = take(s, p, 1).charAt(0);
        c.pgmContext = (int) Cobol.parseNumericDisplay(take(s, p, 1));
        c.custId = Cobol.parseNumericDisplay(take(s, p, 9));
        c.custFname = take(s, p, 25);
        c.custMname = take(s, p, 25);
        c.custLname = take(s, p, 25);
        c.acctId = Cobol.parseNumericDisplay(take(s, p, 11));
        c.acctStatus = take(s, p, 1).charAt(0);
        c.cardNum = Cobol.parseNumericDisplay(take(s, p, 16));
        c.lastMap = take(s, p, 7);
        c.lastMapset = take(s, p, 7);
        return c;
    }

    static String take(String s, int[] pos, int len) {
        String v = s.substring(pos[0], pos[0] + len);
        pos[0] += len;
        return v;
    }

    public String toFixedWidth() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType
                + Cobol.numericDisplay(pgmContext, 1)
                + Cobol.numericDisplay(custId, 9) + custFname + custMname + custLname
                + Cobol.numericDisplay(acctId, 11) + acctStatus
                + Cobol.numericDisplay(cardNum, 16)
                + lastMap + lastMapset;
    }

    public boolean isUsrtypAdmin() {
        return userType == UserType.ADMIN.code();
    }

    public boolean isUsrtypUser() {
        return userType == UserType.USER.code();
    }

    public boolean isPgmEnter() {
        return pgmContext == PgmContext.ENTER.code();
    }

    public boolean isPgmReenter() {
        return pgmContext == PgmContext.REENTER.code();
    }

    public void setPgmContext(PgmContext context) {
        this.pgmContext = context.code();
    }

    public String getFromTranid() {
        return fromTranid;
    }

    public void setFromTranid(String v) {
        this.fromTranid = Cobol.alnum(v, 4);
    }

    public String getFromProgram() {
        return fromProgram;
    }

    public void setFromProgram(String v) {
        this.fromProgram = Cobol.alnum(v, 8);
    }

    public String getToTranid() {
        return toTranid;
    }

    public void setToTranid(String v) {
        this.toTranid = Cobol.alnum(v, 4);
    }

    public String getToProgram() {
        return toProgram;
    }

    public void setToProgram(String v) {
        this.toProgram = Cobol.alnum(v, 8);
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String v) {
        this.userId = Cobol.alnum(v, 8);
    }

    public char getUserType() {
        return userType;
    }

    public void setUserType(char v) {
        this.userType = v;
    }

    public void setUserType(UserType v) {
        this.userType = v.code();
    }

    public int getPgmContext() {
        return pgmContext;
    }

    public void setPgmContext(int v) {
        this.pgmContext = (int) Cobol.truncateUnsigned(v, 1);
    }

    public long getCustId() {
        return custId;
    }

    public void setCustId(long v) {
        this.custId = Cobol.truncateUnsigned(v, 9);
    }

    public String getCustFname() {
        return custFname;
    }

    public void setCustFname(String v) {
        this.custFname = Cobol.alnum(v, 25);
    }

    public String getCustMname() {
        return custMname;
    }

    public void setCustMname(String v) {
        this.custMname = Cobol.alnum(v, 25);
    }

    public String getCustLname() {
        return custLname;
    }

    public void setCustLname(String v) {
        this.custLname = Cobol.alnum(v, 25);
    }

    public long getAcctId() {
        return acctId;
    }

    public void setAcctId(long v) {
        this.acctId = Cobol.truncateUnsigned(v, 11);
    }

    public char getAcctStatus() {
        return acctStatus;
    }

    public void setAcctStatus(char v) {
        this.acctStatus = v;
    }

    public long getCardNum() {
        return cardNum;
    }

    public void setCardNum(long v) {
        this.cardNum = Cobol.truncateUnsigned(v, 16);
    }

    public String getLastMap() {
        return lastMap;
    }

    public void setLastMap(String v) {
        this.lastMap = Cobol.alnum(v, 7);
    }

    public String getLastMapset() {
        return lastMapset;
    }

    public void setLastMapset(String v) {
        this.lastMapset = Cobol.alnum(v, 7);
    }
}

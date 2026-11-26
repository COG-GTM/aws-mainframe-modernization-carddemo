package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/**
 * COCOM01Y: {@code CARDDEMO-COMMAREA} (160 bytes). Each field is held as its exact fixed-width text so that a
 * commarea round-trips byte for byte; numeric fields have typed accessors.
 */
public final class CardDemoCommarea {
    public static final int LENGTH = 160;

    /** 88-levels of CDEMO-USER-TYPE PIC X(01). */
    public enum UserType {
        ADMIN('A'), USER('U');
        private final char value;
        UserType(char value) { this.value = value; }
        public char value() { return value; }
    }

    /** 88-levels of CDEMO-PGM-CONTEXT PIC 9(01). */
    public enum PgmContext {
        ENTER('0'), REENTER('1');
        private final char value;
        PgmContext(char value) { this.value = value; }
        public char value() { return value; }
    }

    // CDEMO-GENERAL-INFO
    private String fromTranid;   // X(04)
    private String fromProgram;  // X(08)
    private String toTranid;     // X(04)
    private String toProgram;    // X(08)
    private String userId;       // X(08)
    private String userType;     // X(01)
    private String pgmContext;   // 9(01)
    // CDEMO-CUSTOMER-INFO
    private String custId;       // 9(09)
    private String custFname;    // X(25)
    private String custMname;    // X(25)
    private String custLname;    // X(25)
    // CDEMO-ACCOUNT-INFO
    private String acctId;       // 9(11)
    private String acctStatus;   // X(01)
    // CDEMO-CARD-INFO
    private String cardNum;      // 9(16)
    // CDEMO-MORE-INFO
    private String lastMap;      // X(7)
    private String lastMapset;   // X(7)

    private CardDemoCommarea() {
    }

    /** Fresh CICS working storage with no VALUE clauses: every byte LOW-VALUES. */
    public static CardDemoCommarea lowValues() {
        return fromString(Cobol.lowValues(LENGTH));
    }

    /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics to SPACES, numerics to ZEROS. */
    public static CardDemoCommarea initialized() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initialize();
        return c;
    }

    public void initialize() {
        fromTranid = Cobol.spaces(4);
        fromProgram = Cobol.spaces(8);
        toTranid = Cobol.spaces(4);
        toProgram = Cobol.spaces(8);
        userId = Cobol.spaces(8);
        userType = Cobol.spaces(1);
        pgmContext = "0";
        custId = Cobol.digits(0, 9);
        custFname = Cobol.spaces(25);
        custMname = Cobol.spaces(25);
        custLname = Cobol.spaces(25);
        acctId = Cobol.digits(0, 11);
        acctStatus = Cobol.spaces(1);
        cardNum = Cobol.digits(0, 16);
        lastMap = Cobol.spaces(7);
        lastMapset = Cobol.spaces(7);
    }

    /** Parse the first 160 bytes of a commarea (short input is space padded). */
    public static CardDemoCommarea fromString(String raw) {
        String s = Cobol.alnum(raw == null ? "" : raw, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        int p = 0;
        c.fromTranid = s.substring(p, p += 4);
        c.fromProgram = s.substring(p, p += 8);
        c.toTranid = s.substring(p, p += 4);
        c.toProgram = s.substring(p, p += 8);
        c.userId = s.substring(p, p += 8);
        c.userType = s.substring(p, p += 1);
        c.pgmContext = s.substring(p, p += 1);
        c.custId = s.substring(p, p += 9);
        c.custFname = s.substring(p, p += 25);
        c.custMname = s.substring(p, p += 25);
        c.custLname = s.substring(p, p += 25);
        c.acctId = s.substring(p, p += 11);
        c.acctStatus = s.substring(p, p += 1);
        c.cardNum = s.substring(p, p += 16);
        c.lastMap = s.substring(p, p += 7);
        c.lastMapset = s.substring(p, p += 7);
        return c;
    }

    @Override
    public String toString() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType + pgmContext
                + custId + custFname + custMname + custLname + acctId + acctStatus + cardNum + lastMap + lastMapset;
    }

    public CardDemoCommarea copy() {
        return fromString(toString());
    }

    public String fromTranid() { return fromTranid; }
    public void setFromTranid(String v) { fromTranid = Cobol.alnum(v, 4); }
    public String fromProgram() { return fromProgram; }
    public void setFromProgram(String v) { fromProgram = Cobol.alnum(v, 8); }
    public String toTranid() { return toTranid; }
    public void setToTranid(String v) { toTranid = Cobol.alnum(v, 4); }
    public String toProgram() { return toProgram; }
    public void setToProgram(String v) { toProgram = Cobol.alnum(v, 8); }
    public String userId() { return userId; }
    public void setUserId(String v) { userId = Cobol.alnum(v, 8); }
    public String userType() { return userType; }
    public boolean isUsrtypAdmin() { return userType.charAt(0) == UserType.ADMIN.value(); }
    public boolean isUsrtypUser() { return userType.charAt(0) == UserType.USER.value(); }
    public void setUserType(UserType t) { userType = String.valueOf(t.value()); }
    public String pgmContext() { return pgmContext; }
    public boolean isPgmEnter() { return pgmContext.charAt(0) == PgmContext.ENTER.value(); }
    public boolean isPgmReenter() { return pgmContext.charAt(0) == PgmContext.REENTER.value(); }
    public void setPgmContext(PgmContext c) { pgmContext = String.valueOf(c.value()); }
    public long custId() { return Cobol.toLong(custId); }
    public String custIdText() { return custId; }
    public void setCustId(long v) { custId = Cobol.digits(v, 9); }
    public String custFname() { return custFname; }
    public void setCustFname(String v) { custFname = Cobol.alnum(v, 25); }
    public String custMname() { return custMname; }
    public void setCustMname(String v) { custMname = Cobol.alnum(v, 25); }
    public String custLname() { return custLname; }
    public void setCustLname(String v) { custLname = Cobol.alnum(v, 25); }
    public long acctId() { return Cobol.toLong(acctId); }
    public String acctIdText() { return acctId; }
    public void setAcctId(long v) { acctId = Cobol.digits(v, 11); }
    public String acctStatus() { return acctStatus; }
    public void setAcctStatus(String v) { acctStatus = Cobol.alnum(v, 1); }
    public long cardNum() { return Cobol.toLong(cardNum); }
    public String cardNumText() { return cardNum; }
    public void setCardNum(long v) { cardNum = Cobol.digits(v, 16); }
    public String lastMap() { return lastMap; }
    public void setLastMap(String v) { lastMap = Cobol.alnum(v, 7); }
    public String lastMapset() { return lastMapset; }
    public void setLastMapset(String v) { lastMapset = Cobol.alnum(v, 7); }
}

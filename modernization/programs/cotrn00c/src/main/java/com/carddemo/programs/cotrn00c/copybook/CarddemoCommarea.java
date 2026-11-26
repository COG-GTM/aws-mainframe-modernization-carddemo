package com.carddemo.programs.cotrn00c.copybook;

import java.util.Optional;

/** COCOM01Y - CARDDEMO-COMMAREA (160 bytes), the shared CardDemo COMMAREA. */
public final class CarddemoCommarea {

    public static final int LENGTH = 160;

    /** 88-levels on CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), // CDEMO-USRTYP-ADMIN
        USER('U');  // CDEMO-USRTYP-USER

        private final char code;

        UserType(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }

        public static Optional<UserType> of(String value) {
            for (UserType t : values()) {
                if (value != null && value.length() == 1 && value.charAt(0) == t.code) {
                    return Optional.of(t);
                }
            }
            return Optional.empty();
        }
    }

    /** 88-levels on CDEMO-PGM-CONTEXT. */
    public enum PgmContext {
        ENTER(0),   // CDEMO-PGM-ENTER
        REENTER(1); // CDEMO-PGM-REENTER

        private final int code;

        PgmContext(int code) {
            this.code = code;
        }

        public int code() {
            return code;
        }
    }

    // CDEMO-GENERAL-INFO
    private String fromTranid;     // PIC X(04)
    private String fromProgram;    // PIC X(08)
    private String toTranid;       // PIC X(04)
    private String toProgram;      // PIC X(08)
    private String userId;         // PIC X(08)
    private String userType;       // PIC X(01)
    private int pgmContext;        // PIC 9(01)
    // CDEMO-CUSTOMER-INFO
    private long custId;           // PIC 9(09)
    private String custFname;      // PIC X(25)
    private String custMname;      // PIC X(25)
    private String custLname;      // PIC X(25)
    // CDEMO-ACCOUNT-INFO
    private long acctId;           // PIC 9(11)
    private String acctStatus;     // PIC X(01)
    // CDEMO-CARD-INFO
    private long cardNum;          // PIC 9(16)
    // CDEMO-MORE-INFO
    private String lastMap;        // PIC X(7)
    private String lastMapset;     // PIC X(7)

    /** A COMMAREA whose bytes are all spaces. */
    public static CarddemoCommarea blank() {
        return decode(CobolText.spaces(LENGTH), 0);
    }

    /** Decode from {@code data} starting at {@code offset}; data must cover LENGTH bytes. */
    public static CarddemoCommarea decode(String data, int offset) {
        CarddemoCommarea c = new CarddemoCommarea();
        int p = offset;
        c.fromTranid = data.substring(p, p += 4);
        c.fromProgram = data.substring(p, p += 8);
        c.toTranid = data.substring(p, p += 4);
        c.toProgram = data.substring(p, p += 8);
        c.userId = data.substring(p, p += 8);
        c.userType = data.substring(p, p += 1);
        c.pgmContext = (int) CobolText.parseUnsigned(data.substring(p, p += 1));
        c.custId = CobolText.parseUnsigned(data.substring(p, p += 9));
        c.custFname = data.substring(p, p += 25);
        c.custMname = data.substring(p, p += 25);
        c.custLname = data.substring(p, p += 25);
        c.acctId = CobolText.parseUnsigned(data.substring(p, p += 11));
        c.acctStatus = data.substring(p, p += 1);
        c.cardNum = CobolText.parseUnsigned(data.substring(p, p += 16));
        c.lastMap = data.substring(p, p += 7);
        c.lastMapset = data.substring(p, p + 7);
        return c;
    }

    public String encode() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType
                + CobolText.unsignedDigits(pgmContext, 1)
                + CobolText.unsignedDigits(custId, 9)
                + custFname + custMname + custLname
                + CobolText.unsignedDigits(acctId, 11)
                + acctStatus
                + CobolText.unsignedDigits(cardNum, 16)
                + lastMap + lastMapset;
    }

    public boolean isUsrtypAdmin() {
        return UserType.of(userType).filter(t -> t == UserType.ADMIN).isPresent();
    }

    public boolean isUsrtypUser() {
        return UserType.of(userType).filter(t -> t == UserType.USER).isPresent();
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

    public String fromTranid() {
        return fromTranid;
    }

    public void setFromTranid(String v) {
        fromTranid = CobolText.fit(v, 4);
    }

    public String fromProgram() {
        return fromProgram;
    }

    public void setFromProgram(String v) {
        fromProgram = CobolText.fit(v, 8);
    }

    public String toTranid() {
        return toTranid;
    }

    public void setToTranid(String v) {
        toTranid = CobolText.fit(v, 4);
    }

    public String toProgram() {
        return toProgram;
    }

    public void setToProgram(String v) {
        toProgram = CobolText.fit(v, 8);
    }

    public String userId() {
        return userId;
    }

    public void setUserId(String v) {
        userId = CobolText.fit(v, 8);
    }

    public String userType() {
        return userType;
    }

    public void setUserType(String v) {
        userType = CobolText.fit(v, 1);
    }

    public int pgmContext() {
        return pgmContext;
    }

    public void setPgmContext(int v) {
        pgmContext = (int) Math.floorMod(v, 10);
    }

    public long custId() {
        return custId;
    }

    public void setCustId(long v) {
        custId = v;
    }

    public String custFname() {
        return custFname;
    }

    public void setCustFname(String v) {
        custFname = CobolText.fit(v, 25);
    }

    public String custMname() {
        return custMname;
    }

    public void setCustMname(String v) {
        custMname = CobolText.fit(v, 25);
    }

    public String custLname() {
        return custLname;
    }

    public void setCustLname(String v) {
        custLname = CobolText.fit(v, 25);
    }

    public long acctId() {
        return acctId;
    }

    public void setAcctId(long v) {
        acctId = v;
    }

    public String acctStatus() {
        return acctStatus;
    }

    public void setAcctStatus(String v) {
        acctStatus = CobolText.fit(v, 1);
    }

    public long cardNum() {
        return cardNum;
    }

    public void setCardNum(long v) {
        cardNum = v;
    }

    public String lastMap() {
        return lastMap;
    }

    public void setLastMap(String v) {
        lastMap = CobolText.fit(v, 7);
    }

    public String lastMapset() {
        return lastMapset;
    }

    public void setLastMapset(String v) {
        lastMapset = CobolText.fit(v, 7);
    }
}

package com.carddemo.programs.cotrn01c.copybook;

/**
 * COCOM01Y - 01 CARDDEMO-COMMAREA (160 bytes). Backed by a fixed-width display buffer so that group
 * MOVEs (DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA) and pass-through on XCTL/RETURN are byte-exact.
 */
public class CardDemoCommarea {

    public static final int LENGTH = 160;

    // CDEMO-GENERAL-INFO
    static final int FROM_TRANID = 0;      // X(04)
    static final int FROM_PROGRAM = 4;     // X(08)
    static final int TO_TRANID = 12;       // X(04)
    static final int TO_PROGRAM = 16;      // X(08)
    static final int USER_ID = 24;         // X(08)
    static final int USER_TYPE = 32;       // X(01)
    static final int PGM_CONTEXT = 33;     // 9(01)
    // CDEMO-CUSTOMER-INFO
    static final int CUST_ID = 34;         // 9(09)
    static final int CUST_FNAME = 43;      // X(25)
    static final int CUST_MNAME = 68;      // X(25)
    static final int CUST_LNAME = 93;      // X(25)
    // CDEMO-ACCOUNT-INFO
    static final int ACCT_ID = 118;        // 9(11)
    static final int ACCT_STATUS = 129;    // X(01)
    // CDEMO-CARD-INFO
    static final int CARD_NUM = 130;       // 9(16)
    // CDEMO-MORE-INFO
    static final int LAST_MAP = 146;       // X(7)
    static final int LAST_MAPSET = 153;    // X(7)

    /** 88-level values of CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), USER('U');

        private final char value;

        UserType(char value) {
            this.value = value;
        }

        public char value() {
            return value;
        }
    }

    /** 88-level values of CDEMO-PGM-CONTEXT. */
    public enum PgmContext {
        ENTER('0'), REENTER('1');

        private final char value;

        PgmContext(char value) {
            this.value = value;
        }

        public char value() {
            return value;
        }
    }

    protected final char[] buf;

    protected CardDemoCommarea(int totalLength) {
        buf = new char[totalLength];
        java.util.Arrays.fill(buf, ' ');
        put(PGM_CONTEXT, 1, "0");
        put(CUST_ID, 9, "000000000");
        put(ACCT_ID, 11, "00000000000");
        put(CARD_NUM, 16, "0000000000000000");
    }

    public CardDemoCommarea() {
        this(LENGTH);
    }

    protected final String get(int off, int len) {
        return new String(buf, off, len);
    }

    protected final void put(int off, int len, String value) {
        Cobol.moveAlnum(value, len).getChars(0, len, buf, off);
    }

    /** Group MOVE of a (possibly shorter) source into this area: space-padded on the right. */
    public void moveFrom(String source) {
        Cobol.moveAlnum(source, buf.length).getChars(0, buf.length, buf, 0);
    }

    /** The full fixed-width area, as passed on COMMAREA(...). */
    public String toFixedString() {
        return new String(buf);
    }

    public String fromTranid() { return get(FROM_TRANID, 4); }
    public void fromTranid(String v) { put(FROM_TRANID, 4, v); }
    public String fromProgram() { return get(FROM_PROGRAM, 8); }
    public void fromProgram(String v) { put(FROM_PROGRAM, 8, v); }
    public String toTranid() { return get(TO_TRANID, 4); }
    public void toTranid(String v) { put(TO_TRANID, 4, v); }
    public String toProgram() { return get(TO_PROGRAM, 8); }
    public void toProgram(String v) { put(TO_PROGRAM, 8, v); }
    public String userId() { return get(USER_ID, 8); }
    public void userId(String v) { put(USER_ID, 8, v); }
    public String userType() { return get(USER_TYPE, 1); }
    public void userType(String v) { put(USER_TYPE, 1, v); }
    public boolean isUsrtypAdmin() { return buf[USER_TYPE] == UserType.ADMIN.value(); }
    public boolean isUsrtypUser() { return buf[USER_TYPE] == UserType.USER.value(); }

    /** CDEMO-PGM-CONTEXT PIC 9(01), raw display character. */
    public char pgmContext() { return buf[PGM_CONTEXT]; }
    public void pgmContext(PgmContext ctx) { buf[PGM_CONTEXT] = ctx.value(); }
    /** MOVE ZEROS TO CDEMO-PGM-CONTEXT. */
    public void pgmContextZeros() { buf[PGM_CONTEXT] = '0'; }
    public boolean isPgmEnter() { return buf[PGM_CONTEXT] == PgmContext.ENTER.value(); }
    public boolean isPgmReenter() { return buf[PGM_CONTEXT] == PgmContext.REENTER.value(); }

    public long custId() { return Cobol.parseUnsigned(get(CUST_ID, 9)); }
    public void custId(long v) { put(CUST_ID, 9, Cobol.moveUnsignedNumeric(v, 9)); }
    public String custFname() { return get(CUST_FNAME, 25); }
    public void custFname(String v) { put(CUST_FNAME, 25, v); }
    public String custMname() { return get(CUST_MNAME, 25); }
    public void custMname(String v) { put(CUST_MNAME, 25, v); }
    public String custLname() { return get(CUST_LNAME, 25); }
    public void custLname(String v) { put(CUST_LNAME, 25, v); }
    public long acctId() { return Cobol.parseUnsigned(get(ACCT_ID, 11)); }
    public void acctId(long v) { put(ACCT_ID, 11, Cobol.moveUnsignedNumeric(v, 11)); }
    public String acctStatus() { return get(ACCT_STATUS, 1); }
    public void acctStatus(String v) { put(ACCT_STATUS, 1, v); }
    public long cardNum() { return Cobol.parseUnsigned(get(CARD_NUM, 16)); }
    public void cardNum(long v) { put(CARD_NUM, 16, Cobol.moveUnsignedNumeric(v, 16)); }
    public String lastMap() { return get(LAST_MAP, 7); }
    public void lastMap(String v) { put(LAST_MAP, 7, v); }
    public String lastMapset() { return get(LAST_MAPSET, 7); }
    public void lastMapset(String v) { put(LAST_MAPSET, 7, v); }
}

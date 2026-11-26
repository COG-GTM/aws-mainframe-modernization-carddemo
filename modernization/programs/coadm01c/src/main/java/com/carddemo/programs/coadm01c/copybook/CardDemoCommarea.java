package com.carddemo.programs.coadm01c.copybook;

/**
 * COCOM01Y - 01 CARDDEMO-COMMAREA (160 bytes). Held as the raw fixed-width record so that bytes the program
 * does not touch round-trip unchanged between programs; typed accessors mirror the PIC clauses.
 */
public final class CardDemoCommarea {

    public static final int LENGTH = 160;

    /** Field layout: offset and length within the 160-byte record. */
    public enum Field {
        // 05 CDEMO-GENERAL-INFO
        FROM_TRANID(0, 4),     // CDEMO-FROM-TRANID   PIC X(04)
        FROM_PROGRAM(4, 8),    // CDEMO-FROM-PROGRAM  PIC X(08)
        TO_TRANID(12, 4),      // CDEMO-TO-TRANID     PIC X(04)
        TO_PROGRAM(16, 8),     // CDEMO-TO-PROGRAM    PIC X(08)
        USER_ID(24, 8),        // CDEMO-USER-ID       PIC X(08)
        USER_TYPE(32, 1),      // CDEMO-USER-TYPE     PIC X(01)
        PGM_CONTEXT(33, 1),    // CDEMO-PGM-CONTEXT   PIC 9(01)
        // 05 CDEMO-CUSTOMER-INFO
        CUST_ID(34, 9),        // CDEMO-CUST-ID       PIC 9(09)
        CUST_FNAME(43, 25),    // CDEMO-CUST-FNAME    PIC X(25)
        CUST_MNAME(68, 25),    // CDEMO-CUST-MNAME    PIC X(25)
        CUST_LNAME(93, 25),    // CDEMO-CUST-LNAME    PIC X(25)
        // 05 CDEMO-ACCOUNT-INFO
        ACCT_ID(118, 11),      // CDEMO-ACCT-ID       PIC 9(11)
        ACCT_STATUS(129, 1),   // CDEMO-ACCT-STATUS   PIC X(01)
        // 05 CDEMO-CARD-INFO
        CARD_NUM(130, 16),     // CDEMO-CARD-NUM      PIC 9(16)
        // 05 CDEMO-MORE-INFO
        LAST_MAP(146, 7),      // CDEMO-LAST-MAP      PIC X(7)
        LAST_MAPSET(153, 7);   // CDEMO-LAST-MAPSET   PIC X(7)

        public final int offset;
        public final int length;

        Field(int offset, int length) {
            this.offset = offset;
            this.length = length;
        }
    }

    /** 88-levels on CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), // 88 CDEMO-USRTYP-ADMIN VALUE 'A'
        USER('U');  // 88 CDEMO-USRTYP-USER  VALUE 'U'

        public final char code;

        UserType(char code) {
            this.code = code;
        }
    }

    /** 88-levels on CDEMO-PGM-CONTEXT. */
    public enum PgmContext {
        ENTER('0'),   // 88 CDEMO-PGM-ENTER   VALUE 0
        REENTER('1'); // 88 CDEMO-PGM-REENTER VALUE 1

        public final char code;

        PgmContext(char code) {
            this.code = code;
        }
    }

    private final char[] data;

    /** Uninitialised working storage (treated as LOW-VALUES). */
    public CardDemoCommarea() {
        this.data = CobolFields.lowValues(LENGTH).toCharArray();
    }

    private CardDemoCommarea(char[] data) {
        this.data = data;
    }

    /** {@code MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA}: space padded / truncated to 160 bytes. */
    public static CardDemoCommarea fromRecord(String raw) {
        return new CardDemoCommarea(CobolFields.alnum(raw, LENGTH).toCharArray());
    }

    public String toRecord() {
        return new String(data);
    }

    public CardDemoCommarea copy() {
        return new CardDemoCommarea(data.clone());
    }

    public String get(Field f) {
        return new String(data, f.offset, f.length);
    }

    /** Alphanumeric MOVE into the field. */
    public void set(Field f, String value) {
        CobolFields.alnum(value, f.length).getChars(0, f.length, data, f.offset);
    }

    /** Numeric MOVE into an unsigned PIC 9(n) field. */
    public void setNumeric(Field f, long value) {
        CobolFields.unsigned(value, f.length).getChars(0, f.length, data, f.offset);
    }

    /** Numeric value of a PIC 9(n) field; throws if the bytes are not numeric. */
    public long getNumeric(Field f) {
        String raw = get(f);
        if (!CobolFields.isNumeric(raw)) {
            throw new NumberFormatException(f + " is not numeric: [" + raw + "]");
        }
        return Long.parseLong(raw);
    }

    public String fromTranid() { return get(Field.FROM_TRANID); }
    public void setFromTranid(String v) { set(Field.FROM_TRANID, v); }
    public String fromProgram() { return get(Field.FROM_PROGRAM); }
    public void setFromProgram(String v) { set(Field.FROM_PROGRAM, v); }
    public String toTranid() { return get(Field.TO_TRANID); }
    public void setToTranid(String v) { set(Field.TO_TRANID, v); }
    public String toProgram() { return get(Field.TO_PROGRAM); }
    public void setToProgram(String v) { set(Field.TO_PROGRAM, v); }
    public String userId() { return get(Field.USER_ID); }
    public void setUserId(String v) { set(Field.USER_ID, v); }
    public char userType() { return data[Field.USER_TYPE.offset]; }
    public void setUserType(UserType t) { data[Field.USER_TYPE.offset] = t.code; }
    public boolean isUsrTypAdmin() { return userType() == UserType.ADMIN.code; }
    public boolean isUsrTypUser() { return userType() == UserType.USER.code; }
    public char pgmContext() { return data[Field.PGM_CONTEXT.offset]; }
    public void setPgmContext(PgmContext c) { data[Field.PGM_CONTEXT.offset] = c.code; }
    public boolean isPgmEnter() { return pgmContext() == PgmContext.ENTER.code; }
    public boolean isPgmReenter() { return pgmContext() == PgmContext.REENTER.code; }
    public long custId() { return getNumeric(Field.CUST_ID); }
    public void setCustId(long v) { setNumeric(Field.CUST_ID, v); }
    public String custFname() { return get(Field.CUST_FNAME); }
    public void setCustFname(String v) { set(Field.CUST_FNAME, v); }
    public String custMname() { return get(Field.CUST_MNAME); }
    public void setCustMname(String v) { set(Field.CUST_MNAME, v); }
    public String custLname() { return get(Field.CUST_LNAME); }
    public void setCustLname(String v) { set(Field.CUST_LNAME, v); }
    public long acctId() { return getNumeric(Field.ACCT_ID); }
    public void setAcctId(long v) { setNumeric(Field.ACCT_ID, v); }
    public String acctStatus() { return get(Field.ACCT_STATUS); }
    public void setAcctStatus(String v) { set(Field.ACCT_STATUS, v); }
    public String cardNum() { return get(Field.CARD_NUM); }
    public void setCardNum(long v) { setNumeric(Field.CARD_NUM, v); }
    public String lastMap() { return get(Field.LAST_MAP); }
    public void setLastMap(String v) { set(Field.LAST_MAP, v); }
    public String lastMapset() { return get(Field.LAST_MAPSET); }
    public void setLastMapset(String v) { set(Field.LAST_MAPSET, v); }

    @Override
    public boolean equals(Object o) {
        return o instanceof CardDemoCommarea other && java.util.Arrays.equals(data, other.data);
    }

    @Override
    public int hashCode() {
        return java.util.Arrays.hashCode(data);
    }

    @Override
    public String toString() {
        return "CardDemoCommarea[" + toRecord().replace(CobolFields.LOW_VALUE, '.') + "]";
    }
}

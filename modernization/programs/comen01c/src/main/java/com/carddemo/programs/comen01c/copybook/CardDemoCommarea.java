package com.carddemo.programs.comen01c.copybook;

/**
 * COCOM01Y - {@code 01 CARDDEMO-COMMAREA} (160 bytes).
 *
 * <p>Fields are held in their DISPLAY form (fixed-width strings) so that a COMMAREA received from a
 * caller is passed on byte-for-byte, exactly as the COBOL group MOVE does. Typed accessors are provided
 * for the numeric fields.
 */
public final class CardDemoCommarea {

    public static final int LENGTH = 160;

    // 05 CDEMO-GENERAL-INFO
    private String fromTranid = CobolText.lowValues(4);   // PIC X(04)
    private String fromProgram = CobolText.lowValues(8);  // PIC X(08)
    private String toTranid = CobolText.lowValues(4);     // PIC X(04)
    private String toProgram = CobolText.lowValues(8);    // PIC X(08)
    private String userId = CobolText.lowValues(8);       // PIC X(08)
    private String userType = CobolText.lowValues(1);     // PIC X(01)
    private String pgmContext = CobolText.lowValues(1);   // PIC 9(01)
    // 05 CDEMO-CUSTOMER-INFO
    private String custId = CobolText.lowValues(9);       // PIC 9(09)
    private String custFname = CobolText.lowValues(25);   // PIC X(25)
    private String custMname = CobolText.lowValues(25);   // PIC X(25)
    private String custLname = CobolText.lowValues(25);   // PIC X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private String acctId = CobolText.lowValues(11);      // PIC 9(11)
    private String acctStatus = CobolText.lowValues(1);   // PIC X(01)
    // 05 CDEMO-CARD-INFO
    private String cardNum = CobolText.lowValues(16);     // PIC 9(16)
    // 05 CDEMO-MORE-INFO
    private String lastMap = CobolText.lowValues(7);      // PIC X(7)
    private String lastMapset = CobolText.lowValues(7);   // PIC X(7)

    /** 88-levels of CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), USER('U');

        private final char code;

        UserType(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }
    }

    /** 88-levels of CDEMO-PGM-CONTEXT. */
    public enum PgmContext {
        ENTER('0'), REENTER('1');

        private final char code;

        PgmContext(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }
    }

    /** Uninitialised working-storage copy (all LOW-VALUES). */
    public CardDemoCommarea() {
    }

    /** {@code MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA}: pad with spaces / truncate to 160. */
    public static CardDemoCommarea fromBytes(String commarea) {
        String r = CobolText.move(commarea, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        int p = 0;
        c.fromTranid = r.substring(p, p += 4);
        c.fromProgram = r.substring(p, p += 8);
        c.toTranid = r.substring(p, p += 4);
        c.toProgram = r.substring(p, p += 8);
        c.userId = r.substring(p, p += 8);
        c.userType = r.substring(p, p += 1);
        c.pgmContext = r.substring(p, p += 1);
        c.custId = r.substring(p, p += 9);
        c.custFname = r.substring(p, p += 25);
        c.custMname = r.substring(p, p += 25);
        c.custLname = r.substring(p, p += 25);
        c.acctId = r.substring(p, p += 11);
        c.acctStatus = r.substring(p, p += 1);
        c.cardNum = r.substring(p, p += 16);
        c.lastMap = r.substring(p, p += 7);
        c.lastMapset = r.substring(p, p + 7);
        return c;
    }

    public String toBytes() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType + pgmContext
                + custId + custFname + custMname + custLname + acctId + acctStatus + cardNum
                + lastMap + lastMapset;
    }

    public CardDemoCommarea copy() {
        return fromBytes(toBytes());
    }

    // 88 CDEMO-USRTYP-ADMIN / CDEMO-USRTYP-USER
    public boolean isUsrtypAdmin() {
        return userType.charAt(0) == UserType.ADMIN.code();
    }

    public boolean isUsrtypUser() {
        return userType.charAt(0) == UserType.USER.code();
    }

    // 88 CDEMO-PGM-ENTER / CDEMO-PGM-REENTER
    public boolean isPgmEnter() {
        return pgmContext.charAt(0) == PgmContext.ENTER.code();
    }

    public boolean isPgmReenter() {
        return pgmContext.charAt(0) == PgmContext.REENTER.code();
    }

    public void setPgmContext(PgmContext context) {
        this.pgmContext = String.valueOf(context.code());
    }

    public void setUserType(UserType type) {
        this.userType = String.valueOf(type.code());
    }

    public String fromTranid() { return fromTranid; }
    public void fromTranid(String v) { fromTranid = CobolText.move(v, 4); }
    public String fromProgram() { return fromProgram; }
    public void fromProgram(String v) { fromProgram = CobolText.move(v, 8); }
    public String toTranid() { return toTranid; }
    public void toTranid(String v) { toTranid = CobolText.move(v, 4); }
    public String toProgram() { return toProgram; }
    public void toProgram(String v) { toProgram = CobolText.move(v, 8); }
    public String userId() { return userId; }
    public void userId(String v) { userId = CobolText.move(v, 8); }
    public String userType() { return userType; }
    public void userType(String v) { userType = CobolText.move(v, 1); }
    public String pgmContext() { return pgmContext; }
    public String custFname() { return custFname; }
    public void custFname(String v) { custFname = CobolText.move(v, 25); }
    public String custMname() { return custMname; }
    public void custMname(String v) { custMname = CobolText.move(v, 25); }
    public String custLname() { return custLname; }
    public void custLname(String v) { custLname = CobolText.move(v, 25); }
    public String acctStatus() { return acctStatus; }
    public void acctStatus(String v) { acctStatus = CobolText.move(v, 1); }
    public String lastMap() { return lastMap; }
    public void lastMap(String v) { lastMap = CobolText.move(v, 7); }
    public String lastMapset() { return lastMapset; }
    public void lastMapset(String v) { lastMapset = CobolText.move(v, 7); }

    /** CDEMO-PGM-CONTEXT PIC 9(01). {@code MOVE ZEROS TO CDEMO-PGM-CONTEXT} is {@code pgmContext(0)}. */
    public void pgmContext(int v) { pgmContext = CobolText.pic9(v, 1); }
    public String custIdDisplay() { return custId; }
    public long custId() { return Long.parseLong(custId); }
    public void custId(long v) { custId = CobolText.pic9(v, 9); }
    public String acctIdDisplay() { return acctId; }
    public long acctId() { return Long.parseLong(acctId); }
    public void acctId(long v) { acctId = CobolText.pic9(v, 11); }
    public String cardNumDisplay() { return cardNum; }
    public long cardNum() { return Long.parseLong(cardNum); }
    public void cardNum(long v) { cardNum = CobolText.pic9(v, 16); }
}

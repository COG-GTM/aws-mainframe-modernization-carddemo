package com.carddemo.programs.cousr01c.copybook;

import java.util.Objects;

/**
 * COCOM01Y - CARDDEMO-COMMAREA (160 bytes). Mutable like the COBOL working-storage group it models.
 * PIC 9 DISPLAY fields are kept as their raw characters so non-numeric content (e.g. spaces) is preserved.
 */
public final class CardDemoCommarea {

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
        ENTER('0'),
        /** 88 CDEMO-PGM-REENTER VALUE 1. */
        REENTER('1');

        private final char code;

        PgmContext(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }
    }

    // 05 CDEMO-GENERAL-INFO
    private String fromTranid = Pic.spaces(4);   // PIC X(04)
    private String fromProgram = Pic.spaces(8);  // PIC X(08)
    private String toTranid = Pic.spaces(4);     // PIC X(04)
    private String toProgram = Pic.spaces(8);    // PIC X(08)
    private String userId = Pic.spaces(8);       // PIC X(08)
    private String userType = Pic.spaces(1);     // PIC X(01)
    private String pgmContext = "0";             // PIC 9(01)
    // 05 CDEMO-CUSTOMER-INFO
    private String custId = Pic.nine(0, 9);      // PIC 9(09)
    private String custFname = Pic.spaces(25);   // PIC X(25)
    private String custMname = Pic.spaces(25);   // PIC X(25)
    private String custLname = Pic.spaces(25);   // PIC X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private String acctId = Pic.nine(0, 11);     // PIC 9(11)
    private String acctStatus = Pic.spaces(1);   // PIC X(01)
    // 05 CDEMO-CARD-INFO
    private String cardNum = Pic.nine(0, 16);    // PIC 9(16)
    // 05 CDEMO-MORE-INFO
    private String lastMap = Pic.spaces(7);      // PIC X(7)
    private String lastMapset = Pic.spaces(7);   // PIC X(7)

    /**
     * {@code MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA}: a shorter commarea is space-padded,
     * a longer one truncated.
     */
    public static CardDemoCommarea fromFixedWidth(String raw) {
        String s = Pic.x(raw, LENGTH);
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

    public String toFixedWidth() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType + pgmContext
                + custId + custFname + custMname + custLname + acctId + acctStatus + cardNum
                + lastMap + lastMapset;
    }

    public CardDemoCommarea copy() {
        return fromFixedWidth(toFixedWidth());
    }

    public boolean isUsrtypAdmin() {
        return userType.charAt(0) == UserType.ADMIN.code();
    }

    public boolean isUsrtypUser() {
        return userType.charAt(0) == UserType.USER.code();
    }

    public boolean isPgmEnter() {
        return pgmContext.charAt(0) == PgmContext.ENTER.code();
    }

    public boolean isPgmReenter() {
        return pgmContext.charAt(0) == PgmContext.REENTER.code();
    }

    /** {@code SET CDEMO-PGM-ENTER / CDEMO-PGM-REENTER TO TRUE}. */
    public void setPgmContext(PgmContext ctx) {
        this.pgmContext = String.valueOf(ctx.code());
    }

    /** {@code SET CDEMO-USRTYP-ADMIN / CDEMO-USRTYP-USER TO TRUE}. */
    public void setUserType(UserType type) {
        this.userType = String.valueOf(type.code());
    }

    public String fromTranid() { return fromTranid; }
    public void fromTranid(String v) { fromTranid = Pic.x(v, 4); }
    public String fromProgram() { return fromProgram; }
    public void fromProgram(String v) { fromProgram = Pic.x(v, 8); }
    public String toTranid() { return toTranid; }
    public void toTranid(String v) { toTranid = Pic.x(v, 4); }
    public String toProgram() { return toProgram; }
    public void toProgram(String v) { toProgram = Pic.x(v, 8); }
    public String userId() { return userId; }
    public void userId(String v) { userId = Pic.x(v, 8); }
    public String userType() { return userType; }
    public void userType(String v) { userType = Pic.x(v, 1); }
    public String pgmContext() { return pgmContext; }
    public void pgmContext(long v) { pgmContext = Pic.nine(v, 1); }
    public String custId() { return custId; }
    public void custId(long v) { custId = Pic.nine(v, 9); }
    public String custFname() { return custFname; }
    public void custFname(String v) { custFname = Pic.x(v, 25); }
    public String custMname() { return custMname; }
    public void custMname(String v) { custMname = Pic.x(v, 25); }
    public String custLname() { return custLname; }
    public void custLname(String v) { custLname = Pic.x(v, 25); }
    public String acctId() { return acctId; }
    public void acctId(long v) { acctId = Pic.nine(v, 11); }
    public String acctStatus() { return acctStatus; }
    public void acctStatus(String v) { acctStatus = Pic.x(v, 1); }
    public String cardNum() { return cardNum; }
    public void cardNum(long v) { cardNum = Pic.nine(v, 16); }
    public String lastMap() { return lastMap; }
    public void lastMap(String v) { lastMap = Pic.x(v, 7); }
    public String lastMapset() { return lastMapset; }
    public void lastMapset(String v) { lastMapset = Pic.x(v, 7); }

    @Override
    public boolean equals(Object o) {
        return o instanceof CardDemoCommarea other && toFixedWidth().equals(other.toFixedWidth());
    }

    @Override
    public int hashCode() {
        return Objects.hash(toFixedWidth());
    }

    @Override
    public String toString() {
        return "CardDemoCommarea[" + toFixedWidth() + "]";
    }
}

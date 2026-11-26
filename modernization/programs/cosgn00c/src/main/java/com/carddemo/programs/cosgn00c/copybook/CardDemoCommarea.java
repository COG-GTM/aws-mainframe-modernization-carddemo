package com.carddemo.programs.cosgn00c.copybook;

import static com.carddemo.programs.cosgn00c.copybook.CobolText.parse9;
import static com.carddemo.programs.cosgn00c.copybook.CobolText.pic9;
import static com.carddemo.programs.cosgn00c.copybook.CobolText.picX;

/**
 * COCOM01Y - CARDDEMO-COMMAREA, the communication area passed between CardDemo programs.
 * Mutable to mirror COBOL MOVE semantics; every setter applies the PIC size (truncate / pad).
 * Record length {@value #LENGTH} bytes.
 */
public final class CardDemoCommarea {

    public static final int LENGTH = 160;

    /** 88-levels on CDEMO-USER-TYPE PIC X(01). */
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

    /** 88-levels on CDEMO-PGM-CONTEXT PIC 9(01). */
    public enum PgmContext {
        ENTER(0), REENTER(1);

        private final int value;

        PgmContext(int value) {
            this.value = value;
        }

        public int value() {
            return value;
        }
    }

    // CDEMO-GENERAL-INFO
    private String fromTranid = picX("", 4);      // CDEMO-FROM-TRANID   PIC X(04)
    private String fromProgram = picX("", 8);     // CDEMO-FROM-PROGRAM  PIC X(08)
    private String toTranid = picX("", 4);        // CDEMO-TO-TRANID     PIC X(04)
    private String toProgram = picX("", 8);       // CDEMO-TO-PROGRAM    PIC X(08)
    private String userId = picX("", 8);          // CDEMO-USER-ID       PIC X(08)
    private String userType = picX("", 1);        // CDEMO-USER-TYPE     PIC X(01)
    private int pgmContext;                       // CDEMO-PGM-CONTEXT   PIC 9(01)
    // CDEMO-CUSTOMER-INFO
    private long custId;                          // CDEMO-CUST-ID       PIC 9(09)
    private String custFname = picX("", 25);      // CDEMO-CUST-FNAME    PIC X(25)
    private String custMname = picX("", 25);      // CDEMO-CUST-MNAME    PIC X(25)
    private String custLname = picX("", 25);      // CDEMO-CUST-LNAME    PIC X(25)
    // CDEMO-ACCOUNT-INFO
    private long acctId;                          // CDEMO-ACCT-ID       PIC 9(11)
    private String acctStatus = picX("", 1);      // CDEMO-ACCT-STATUS   PIC X(01)
    // CDEMO-CARD-INFO
    private long cardNum;                         // CDEMO-CARD-NUM      PIC 9(16)
    // CDEMO-MORE-INFO
    private String lastMap = picX("", 7);         // CDEMO-LAST-MAP      PIC X(7)
    private String lastMapset = picX("", 7);      // CDEMO-LAST-MAPSET   PIC X(7)

    public String fromTranid() { return fromTranid; }
    public void fromTranid(String v) { fromTranid = picX(v, 4); }
    public String fromProgram() { return fromProgram; }
    public void fromProgram(String v) { fromProgram = picX(v, 8); }
    public String toTranid() { return toTranid; }
    public void toTranid(String v) { toTranid = picX(v, 4); }
    public String toProgram() { return toProgram; }
    public void toProgram(String v) { toProgram = picX(v, 8); }
    public String userId() { return userId; }
    public void userId(String v) { userId = picX(v, 8); }
    public String userType() { return userType; }
    public void userType(String v) { userType = picX(v, 1); }
    public int pgmContext() { return pgmContext; }
    public void pgmContext(long v) { pgmContext = (int) (Math.abs(v) % 10); }
    public long custId() { return custId; }
    public void custId(long v) { custId = parse9(pic9(v, 9)); }
    public String custFname() { return custFname; }
    public void custFname(String v) { custFname = picX(v, 25); }
    public String custMname() { return custMname; }
    public void custMname(String v) { custMname = picX(v, 25); }
    public String custLname() { return custLname; }
    public void custLname(String v) { custLname = picX(v, 25); }
    public long acctId() { return acctId; }
    public void acctId(long v) { acctId = parse9(pic9(v, 11)); }
    public String acctStatus() { return acctStatus; }
    public void acctStatus(String v) { acctStatus = picX(v, 1); }
    public long cardNum() { return cardNum; }
    public void cardNum(long v) { cardNum = parse9(pic9(v, 16)); }
    public String lastMap() { return lastMap; }
    public void lastMap(String v) { lastMap = picX(v, 7); }
    public String lastMapset() { return lastMapset; }
    public void lastMapset(String v) { lastMapset = picX(v, 7); }

    /** 88 CDEMO-USRTYP-ADMIN VALUE 'A'. */
    public boolean isUserTypeAdmin() {
        return userType.charAt(0) == UserType.ADMIN.code();
    }

    /** 88 CDEMO-USRTYP-USER VALUE 'U'. */
    public boolean isUserTypeUser() {
        return userType.charAt(0) == UserType.USER.code();
    }

    /** 88 CDEMO-PGM-ENTER VALUE 0. */
    public boolean isPgmEnter() {
        return pgmContext == PgmContext.ENTER.value();
    }

    /** 88 CDEMO-PGM-REENTER VALUE 1. */
    public boolean isPgmReenter() {
        return pgmContext == PgmContext.REENTER.value();
    }

    /** Serialises to the 160-byte fixed-width COMMAREA layout. */
    public String toRecord() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType
                + pic9(pgmContext, 1)
                + pic9(custId, 9) + custFname + custMname + custLname
                + pic9(acctId, 11) + acctStatus
                + pic9(cardNum, 16)
                + lastMap + lastMapset;
    }

    /** Parses the 160-byte fixed-width COMMAREA layout (short input is space padded). */
    public static CardDemoCommarea fromRecord(String record) {
        String r = picX(record, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        int p = 0;
        c.fromTranid(r.substring(p, p += 4));
        c.fromProgram(r.substring(p, p += 8));
        c.toTranid(r.substring(p, p += 4));
        c.toProgram(r.substring(p, p += 8));
        c.userId(r.substring(p, p += 8));
        c.userType(r.substring(p, p += 1));
        c.pgmContext(parse9(r.substring(p, p += 1)));
        c.custId(parse9(r.substring(p, p += 9)));
        c.custFname(r.substring(p, p += 25));
        c.custMname(r.substring(p, p += 25));
        c.custLname(r.substring(p, p += 25));
        c.acctId(parse9(r.substring(p, p += 11)));
        c.acctStatus(r.substring(p, p += 1));
        c.cardNum(parse9(r.substring(p, p += 16)));
        c.lastMap(r.substring(p, p += 7));
        c.lastMapset(r.substring(p, p += 7));
        return c;
    }
}

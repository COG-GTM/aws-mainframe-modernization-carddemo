package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;

/** Copybook COCOM01Y: 01 CARDDEMO-COMMAREA (160 bytes, all DISPLAY). */
public final class CardDemoCommarea {

    public static final int LENGTH = 160;

    // 05 CDEMO-GENERAL-INFO
    /** CDEMO-FROM-TRANID PIC X(04). */
    public String fromTranid;
    /** CDEMO-FROM-PROGRAM PIC X(08). */
    public String fromProgram;
    /** CDEMO-TO-TRANID PIC X(04). */
    public String toTranid;
    /** CDEMO-TO-PROGRAM PIC X(08). */
    public String toProgram;
    /** CDEMO-USER-ID PIC X(08). */
    public String userId;
    /** CDEMO-USER-TYPE PIC X(01): 88 ADMIN 'A', 88 USER 'U'. */
    public String userType;
    /** CDEMO-PGM-CONTEXT PIC 9(01): 88 ENTER 0, 88 REENTER 1. */
    public int pgmContext;
    // 05 CDEMO-CUSTOMER-INFO
    /** CDEMO-CUST-ID PIC 9(09). */
    public long custId;
    /** CDEMO-CUST-FNAME PIC X(25). */
    public String custFname;
    /** CDEMO-CUST-MNAME PIC X(25). */
    public String custMname;
    /** CDEMO-CUST-LNAME PIC X(25). */
    public String custLname;
    // 05 CDEMO-ACCOUNT-INFO
    /** CDEMO-ACCT-ID PIC 9(11). */
    public long acctId;
    /** CDEMO-ACCT-STATUS PIC X(01). */
    public String acctStatus;
    // 05 CDEMO-CARD-INFO
    /** CDEMO-CARD-NUM PIC 9(16). */
    public long cardNum;
    // 05 CDEMO-MORE-INFO
    /** CDEMO-LAST-MAP PIC X(7). */
    public String lastMap;
    /** CDEMO-LAST-MAPSET PIC X(7). */
    public String lastMapset;

    public CardDemoCommarea() {
        initialize();
    }

    /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics to SPACES, numerics to ZERO. */
    public void initialize() {
        fromTranid = Cob.spaces(4);
        fromProgram = Cob.spaces(8);
        toTranid = Cob.spaces(4);
        toProgram = Cob.spaces(8);
        userId = Cob.spaces(8);
        userType = Cob.spaces(1);
        pgmContext = 0;
        custId = 0;
        custFname = Cob.spaces(25);
        custMname = Cob.spaces(25);
        custLname = Cob.spaces(25);
        acctId = 0;
        acctStatus = Cob.spaces(1);
        cardNum = 0;
        lastMap = Cob.spaces(7);
        lastMapset = Cob.spaces(7);
    }

    public boolean isUsrtypAdmin() {
        return "A".equals(userType);
    }

    public boolean isUsrtypUser() {
        return "U".equals(userType);
    }

    public void setUsrtypAdmin() {
        userType = "A";
    }

    public void setUsrtypUser() {
        userType = "U";
    }

    public boolean isPgmEnter() {
        return pgmContext == 0;
    }

    public boolean isPgmReenter() {
        return pgmContext == 1;
    }

    public void setPgmEnter() {
        pgmContext = 0;
    }

    public void setPgmReenter() {
        pgmContext = 1;
    }

    public CardDemoCommarea copy() {
        return fromFixed(toFixed());
    }

    /** Serialises to the 160-byte DISPLAY layout. */
    public String toFixed() {
        return Cob.move(fromTranid, 4) + Cob.move(fromProgram, 8) + Cob.move(toTranid, 4)
                + Cob.move(toProgram, 8) + Cob.move(userId, 8) + Cob.move(userType, 1)
                + Cob.zeroPad(pgmContext, 1) + Cob.zeroPad(custId, 9) + Cob.move(custFname, 25)
                + Cob.move(custMname, 25) + Cob.move(custLname, 25) + Cob.zeroPad(acctId, 11)
                + Cob.move(acctStatus, 1) + Cob.zeroPad(cardNum, 16) + Cob.move(lastMap, 7)
                + Cob.move(lastMapset, 7);
    }

    /** Parses the 160-byte DISPLAY layout (DFHCOMMAREA(1:LENGTH OF CARDDEMO-COMMAREA)). */
    public static CardDemoCommarea fromFixed(String s) {
        String r = Cob.move(s, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        int p = 0;
        c.fromTranid = r.substring(p, p += 4);
        c.fromProgram = r.substring(p, p += 8);
        c.toTranid = r.substring(p, p += 4);
        c.toProgram = r.substring(p, p += 8);
        c.userId = r.substring(p, p += 8);
        c.userType = r.substring(p, p += 1);
        c.pgmContext = (int) parseDigits(r.substring(p, p += 1));
        c.custId = parseDigits(r.substring(p, p += 9));
        c.custFname = r.substring(p, p += 25);
        c.custMname = r.substring(p, p += 25);
        c.custLname = r.substring(p, p += 25);
        c.acctId = parseDigits(r.substring(p, p += 11));
        c.acctStatus = r.substring(p, p += 1);
        c.cardNum = parseDigits(r.substring(p, p += 16));
        c.lastMap = r.substring(p, p += 7);
        c.lastMapset = r.substring(p, p + 7);
        return c;
    }

    private static long parseDigits(String s) {
        long v = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            v = v * 10 + (ch >= '0' && ch <= '9' ? ch - '0' : 0);
        }
        return v;
    }
}
